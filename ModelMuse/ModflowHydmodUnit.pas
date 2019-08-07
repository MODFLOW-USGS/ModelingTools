unit ModflowHydmodUnit;

interface

uses
  ModflowBoundaryUnit, ModflowSubsidenceDefUnit, Classes, LayerStructureUnit,
  GoPhastTypes;

type
  TAssignmentMethod = (amCell, amInterpolate);

  THydmodData = class(TModflowScreenObjectProperty)
  private
    FIsUsed: boolean;
    FSubCompaction: boolean;
    FSubLayerGroup: string;
    FSubNoDelayBed: string;
    FAssignmentMethod: TAssignmentMethod;
    FHead: boolean;
    FSubSubsidence: boolean;
    FSfrOutFlow: boolean;
    FHydrographLabel: string;
    FSubUsedLayers: TUseLayersCollection;
    FSfrInFlow: boolean;
    FSfrStage: boolean;
    FDrawdown: boolean;
    FSfrAquiferExchange: boolean;
    FLayerGroup: TLayerGroup;
    FNoDelayItem: TSubNoDelayBedLayerItem;
    FSubPreconsolidationHead: boolean;
    procedure SetBooleanProperty(var AField: boolean; NewValue: boolean);
    procedure SetStringProperty(var AField: string; NewValue: string);
    procedure SetIsUsed(const Value: boolean);
    procedure SetAssignmentMethod(const Value: TAssignmentMethod);
    procedure SetDrawdown(const Value: boolean);
    procedure SetHead(const Value: boolean);
    procedure SetHydrographLabel(const Value: string);
    procedure SetSfrAquiferExchange(const Value: boolean);
    procedure SetSfrInFlow(const Value: boolean);
    procedure SetSfrOutFlow(const Value: boolean);
    procedure SetSfrStage(const Value: boolean);
    procedure SetSubCompaction(const Value: boolean);
    procedure SetSubLayerGroup(const Value: string);
    procedure SetSubNoDelayBed(const Value: string);
    procedure SetSubSubsidence(const Value: boolean);
    procedure SetSubUsedLayers(const Value: TUseLayersCollection);
    function GetSubLayerGroup: string;
    function GetSubNoDelayBed: string;
    procedure SetSubPreconsolidationHead(const Value: boolean);
  protected
    function BoundaryObserverPrefix: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    function Used: boolean; override;
    procedure Loaded;
    procedure SubLayerNumbers(var IntArray: TOneDIntegerArray);
  published
    property IsUsed: boolean read FIsUsed write SetIsUsed;
    property AssignmentMethod: TAssignmentMethod read FAssignmentMethod
      write SetAssignmentMethod;
    property HydrographLabel: string read FHydrographLabel
      write SetHydrographLabel;
    // BAS HD
    property Head: boolean read FHead write SetHead;
    // BAS DD
    property Drawdown: boolean read FDrawdown write SetDrawdown;
    // SFR ST
    property SfrStage: boolean read FSfrStage write SetSfrStage;
    // SFR SI
    property SfrInFlow: boolean read FSfrInFlow write SetSfrInFlow;
    // SFR SO
    property SfrOutFlow: boolean read FSfrOutFlow write SetSfrOutFlow;
    // SFR SA
    property SfrAquiferExchange: boolean read FSfrAquiferExchange
      write SetSfrAquiferExchange;
    // SUB HC
    property SubPreconsolidationHead: boolean read FSubPreconsolidationHead write SetSubPreconsolidationHead;
    // SUB CP
    property SubCompaction: boolean read FSubCompaction write SetSubCompaction;
    // SUB SB
    property SubSubsidence: boolean read FSubSubsidence write SetSubSubsidence;
    property SubLayerGroup: string read GetSubLayerGroup write SetSubLayerGroup;
    property SubNoDelayBed: string read GetSubNoDelayBed write SetSubNoDelayBed;
    property SubUsedLayers: TUseLayersCollection read FSubUsedLayers
      write SetSubUsedLayers;
  end;  

implementation

uses
  PhastModelUnit;

{ THydmodData }

procedure THydmodData.Assign(Source: TPersistent);
var
  HydSource: THydmodData;
begin
  if Source is THydmodData then
  begin
    HydSource := THydmodData(Source);
    IsUsed := HydSource.IsUsed;
    AssignmentMethod := HydSource.AssignmentMethod;
    HydrographLabel := HydSource.HydrographLabel;
    Head := HydSource.Head;
    Drawdown := HydSource.Drawdown;
    SfrStage := HydSource.SfrStage;
    SfrInFlow := HydSource.SfrInFlow;
    SfrOutFlow := HydSource.SfrOutFlow;
    SfrAquiferExchange := HydSource.SfrAquiferExchange;
    SubPreconsolidationHead := HydSource.SubPreconsolidationHead;
    SubCompaction := HydSource.SubCompaction;
    SubSubsidence := HydSource.SubSubsidence;
    SubLayerGroup := HydSource.SubLayerGroup;
    SubNoDelayBed := HydSource.SubNoDelayBed;
    SubUsedLayers := HydSource.SubUsedLayers;
  end
  else
  begin
    inherited;
  end;
end;

function THydmodData.BoundaryObserverPrefix: string;
begin
  result := '';
  Assert(False);
end;

constructor THydmodData.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FSubUsedLayers := TUseLayersCollection.Create(Model);
end;

destructor THydmodData.Destroy;
begin
  FSubUsedLayers.Free;
  inherited;
end;

function THydmodData.GetSubLayerGroup: string;
var
  LocalPhastModel: TPhastModel;
  GroupIndex: Integer;
  Group: TLayerGroup;
begin
  if (ParentModel <> nil) and (FLayerGroup <> nil) then
  begin
    result := '';
    LocalPhastModel := ParentModel as TPhastModel;
    for GroupIndex := 1 to LocalPhastModel.LayerStructure.Count - 1 do
    begin
      Group := LocalPhastModel.LayerStructure[GroupIndex];
      if Group = FLayerGroup then
      begin
        result := Group.AquiferName;
        Exit;
      end;
    end;
  end
  else
  begin
    result := FSubLayerGroup;
  end;
end;

function THydmodData.GetSubNoDelayBed: string;
var
  LocalPhastModel: TPhastModel;
  GroupIndex: Integer;
  Group: TLayerGroup;
  BedIndex: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
begin
  if (ParentModel <> nil) and (FLayerGroup <> nil) and (FNoDelayItem <> nil) then
  begin
    result := '';
    LocalPhastModel := ParentModel as TPhastModel;
    for GroupIndex := 1 to LocalPhastModel.LayerStructure.Count - 1 do
    begin
      Group := LocalPhastModel.LayerStructure[GroupIndex];
      if Group = FLayerGroup then
      begin
        for BedIndex := 0 to FLayerGroup.SubNoDelayBedLayers.Count - 1 do
        begin
          NoDelayItem := FLayerGroup.SubNoDelayBedLayers[BedIndex];
          if FNoDelayItem = NoDelayItem then
          begin
            result := FNoDelayItem.Name;
            Exit;
          end;
        end;
        break;
      end;
    end;
  end
  else
  begin
    result := FSubNoDelayBed;
  end;
end;

procedure THydmodData.Loaded;
begin
  SubLayerGroup := FSubLayerGroup;
  SubNoDelayBed := FSubNoDelayBed;
end;

procedure THydmodData.SetIsUsed(const Value: boolean);
begin
  SetBooleanProperty(FIsUsed, Value);
end;

procedure THydmodData.SetAssignmentMethod(const Value: TAssignmentMethod);
begin
  if FAssignmentMethod <> Value then
  begin
    FAssignmentMethod := Value;
    InvalidateModel;
  end;
end;

procedure THydmodData.SetBooleanProperty(var AField: boolean;
  NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure THydmodData.SetDrawdown(const Value: boolean);
begin
  SetBooleanProperty(FDrawdown, Value);
end;

procedure THydmodData.SetHead(const Value: boolean);
begin
  SetBooleanProperty(FHead, Value);
end;

procedure THydmodData.SetHydrographLabel(const Value: string);
begin
  SetStringProperty(FHydrographLabel, Value);
end;

procedure THydmodData.SetSfrAquiferExchange(const Value: boolean);
begin
  SetBooleanProperty(FSfrAquiferExchange, Value);
end;

procedure THydmodData.SetSfrInFlow(const Value: boolean);
begin
  SetBooleanProperty(FSfrInFlow, Value);
end;

procedure THydmodData.SetSfrOutFlow(const Value: boolean);
begin
  SetBooleanProperty(FSfrOutFlow, Value);
end;

procedure THydmodData.SetSfrStage(const Value: boolean);
begin
  SetBooleanProperty(FSfrStage, Value);
end;

procedure THydmodData.SetStringProperty(var AField: string; NewValue: string);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure THydmodData.SetSubCompaction(const Value: boolean);
begin
  SetBooleanProperty(FSubCompaction, Value);
end;

procedure THydmodData.SetSubLayerGroup(const Value: string);
var
  LocalPhastModel: TPhastModel;
  GroupIndex: Integer;
  Group: TLayerGroup;
begin
  SetStringProperty(FSubLayerGroup, Value);
  FLayerGroup := nil;
  if Value <> '' then
  begin
    if ParentModel <> nil then
    begin
      LocalPhastModel := ParentModel as TPhastModel;
      for GroupIndex := 1 to LocalPhastModel.LayerStructure.Count - 1 do
      begin
        Group := LocalPhastModel.LayerStructure[GroupIndex];
        if Group.AquiferName = Value then
        begin
          FLayerGroup := Group;
          break;
        end;
      end;
    end;
  end;
end;

procedure THydmodData.SetSubNoDelayBed(const Value: string);
var
  LocalPhastModel: TPhastModel;
  GroupIndex: Integer;
  Group: TLayerGroup;
  BedIndex: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
begin
  SetStringProperty(FSubNoDelayBed, Value);
  FNoDelayItem := nil;
  if Value <> '' then
  begin
    if (ParentModel <> nil) and (FSubLayerGroup <> '') then
    begin
      LocalPhastModel := ParentModel as TPhastModel;
      for GroupIndex := 1 to LocalPhastModel.LayerStructure.Count - 1 do
      begin
        Group := LocalPhastModel.LayerStructure[GroupIndex];
        if (Group = FLayerGroup) or (Group.AquiferName = FSubLayerGroup) then
        begin
          FLayerGroup := Group;
          for BedIndex := 0 to FLayerGroup.SubNoDelayBedLayers.Count - 1 do
          begin
            NoDelayItem := FLayerGroup.SubNoDelayBedLayers[BedIndex];
            if NoDelayItem.Name = Value then
            begin
              FNoDelayItem := NoDelayItem;
              Exit;
            end;
          end;
          break;
        end;
      end;
    end;
  end;
end;

procedure THydmodData.SetSubPreconsolidationHead(const Value: boolean);
begin
  SetBooleanProperty(FSubPreconsolidationHead, Value);
end;

procedure THydmodData.SetSubSubsidence(const Value: boolean);
begin
  SetBooleanProperty(FSubSubsidence, Value);
end;

procedure THydmodData.SetSubUsedLayers(const Value: TUseLayersCollection);
begin
  FSubUsedLayers.Assign(Value);
end;

procedure THydmodData.SubLayerNumbers(var IntArray: TOneDIntegerArray);
var
  LocalPhastModel: TPhastModel;
  GroupIndex: Integer;
  Group: TLayerGroup;
  BedIndex: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
  OverlyingCount: integer;
  UseIndex: Integer;
  Index: Integer;
  ArraySize: Integer;
begin
  if (ParentModel <> nil) and (FSubLayerGroup <> '') then
  begin
    OverlyingCount := 0;
    LocalPhastModel := ParentModel as TPhastModel;
    for GroupIndex := 1 to LocalPhastModel.LayerStructure.Count - 1 do
    begin
      Group := LocalPhastModel.LayerStructure[GroupIndex];
      if (Group = FLayerGroup) or (Group.AquiferName = FSubLayerGroup) then
      begin
        FLayerGroup := Group;
        for BedIndex := 0 to FLayerGroup.SubNoDelayBedLayers.Count - 1 do
        begin
          NoDelayItem := FLayerGroup.SubNoDelayBedLayers[BedIndex];
          if NoDelayItem = FNoDelayItem then
          begin
            if FLayerGroup.RunTimeSimulated then
            begin
              if FLayerGroup.LayerCount > 1 then
              begin
                if NoDelayItem.UseInAllLayers then
                begin
                  SetLength(IntArray, FLayerGroup.LayerCount);
                  for Index := 0 to FLayerGroup.LayerCount - 1 do
                  begin
                    IntArray[Index] := OverlyingCount + Index;
                  end;
                end
                else
                begin
                  ArraySize := 0;
                  for UseIndex := 1 to FLayerGroup.LayerCount do
                  begin
                    if (NoDelayItem.UsedLayers.
                      GetItemByLayerNumber(UseIndex) <> nil)
                      and (FSubUsedLayers.
                      GetItemByLayerNumber(UseIndex) <> nil) then
                    begin
                      Inc(ArraySize);
                    end;
                  end;
                  SetLength(IntArray, ArraySize);
                  Index := -1;
                  for UseIndex := 1 to FLayerGroup.LayerCount do
                  begin
                    if (NoDelayItem.UsedLayers.
                      GetItemByLayerNumber(UseIndex) <> nil) then
                    begin
                      Inc(OverlyingCount);
                      if (FSubUsedLayers.
                      GetItemByLayerNumber(UseIndex) <> nil) then
                      begin
                        Inc(Index);
                        Assert(Index < ArraySize);
                        IntArray[Index] := OverlyingCount;
                      end;
                    end;
                  end;
                end;
              end
              else
              begin
                SetLength(IntArray, 1);
                IntArray[0] := OverlyingCount;
              end;
            end
            else
            begin
              SetLength(IntArray, 0);
            end;
            Exit;
          end
          else
          begin
            if FLayerGroup.RunTimeSimulated then
            begin
              if FLayerGroup.LayerCount > 1 then
              begin
                if NoDelayItem.UseInAllLayers then
                begin
                  Inc(OverlyingCount, FLayerGroup.LayerCount);
                end
                else
                begin
                  for UseIndex := 1 to FLayerGroup.LayerCount do
                  begin
                    if NoDelayItem.UsedLayers.
                      GetItemByLayerNumber(UseIndex) <> nil then
                    begin
                      Inc(OverlyingCount);
                    end;
                  end;
                end;
              end
              else
              begin
                Inc(OverlyingCount);
              end;
            end;
          end;
        end;
      end
      else
      begin
        OverlyingCount := OverlyingCount + Group.NoDelayCount;
      end;
    end;
    SetLength(IntArray, 0);
  end
  else
  begin
    SetLength(IntArray, 0);
  end;


end;

function THydmodData.Used: boolean;
begin
  result := IsUsed;
end;

end.
