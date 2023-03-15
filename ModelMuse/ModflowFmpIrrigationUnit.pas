unit ModflowFmpIrrigationUnit;

interface

uses
  System.Classes, OrderedCollectionUnit, GoPhastTypes, ModflowFmpFarmUnit;

type
  TIrrigationItem = class(TOrderedItem)
  private
    FName: string;
//    FStoredEfficiency: TRealStorage;
    procedure SetName(Value: string);
//    procedure SetStoredEfficiency(const Value: TRealStorage);
//    function GetEfficiency: double;
//    procedure SetEfficiency(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
//    property Efficiency: double read GetEfficiency write SetEfficiency;
  published
    property Name: string read FName write SetName;
//    property StoredEfficiency: TRealStorage read FStoredEfficiency
//      write SetStoredEfficiency;
  end;

  TIrrigationCollection = class(TEnhancedOrderedCollection)
  private
    FFarmList: TFarmList;
    function GetItems(Index: Integer): TIrrigationItem;
    procedure SetItems(Index: Integer; const Value: TIrrigationItem);
    function GetFarmList: TFarmList;
  protected
    property FarmList: TFarmList read GetFarmList;
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    property Items[Index: Integer]: TIrrigationItem read GetItems
      write SetItems; default;
  end;

implementation

uses PhastModelUnit, LockedGlobalVariableChangers;

resourcestring
  StrIrrigationVariable = 'Irrigation Variable';

{ TIrrigationItem }

procedure TIrrigationItem.Assign(Source: TPersistent);
var
  SourceItem: TIrrigationItem;
begin
  if Source is TIrrigationItem then
  begin
    SourceItem := TIrrigationItem(Source);
    Name := SourceItem.Name;
//    Efficiency := SourceItem.Efficiency;
  end;
  inherited;
end;

constructor TIrrigationItem.Create(Collection: TCollection);
var
  LocalModel: TPhastModel;
  FarmList: TFarmList;
  FarmIndex: Integer;
  AFarm: TFarm;
  CropEffIndex: Integer;
  AFarmEff: TFarmEfficienciesItem;
begin
  inherited;

  if (Model <> nil) and (Name <> '') then
  begin
    LocalModel := Model as TPhastModel;
    if (not (csDestroying in LocalModel.ComponentState))
      and (not LocalModel.Clearing) then
    begin
      FarmList := (Collection as TIrrigationCollection).FarmList;
      for FarmIndex := 0 to FarmList.Count - 1 do
      begin
        AFarm := FarmList[FarmIndex];
        for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyCollection.Count - 1 do
        begin
          AFarmEff := AFarm.FarmIrrigationEfficiencyCollection[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = Name then
          begin
            AFarmEff.Free;
            break;
          end;
        end;
        for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
        begin
          AFarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = Name then
          begin
            AFarmEff.Free;
            break;
          end;
        end;
      end;
    end;
  end;

end;



destructor TIrrigationItem.Destroy;
begin
//  FStoredEfficiency.Free;
  inherited;
end;

//function TIrrigationItem.GetEfficiency: double;
//begin
//  result := FStoredEfficiency.Value;
//end;

function TIrrigationItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TIrrigationItem;
begin
  result := (AnotherItem is TIrrigationItem);
  if result then
  begin
    OtherItem := TIrrigationItem(AnotherItem);
    result := (Name  = OtherItem.Name)
//      and (Efficiency  = OtherItem.Efficiency)
  end;
end;

//procedure TIrrigationItem.SetEfficiency(const Value: double);
//begin
//  FStoredEfficiency.Value := Value;
//end;

procedure TIrrigationItem.SetName(Value: string);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
  FarmList: TFarmList;
  FarmIndex: Integer;
  AFarm: TFarm;
  AFarmEff: TFarmEfficienciesItem;
  CropEffIndex: Integer;
  FoundMatch: Boolean;
begin
  if (FName <> Value) and (Model <> nil)
    and not (csReading in Model.ComponentState) then
  begin
    Value := GenerateNewName(Value, nil, '_');
  end;
  ChangeGlobals := TDefineGlobalIntegerObject.Create(Model, FName, Value,
    StrIrrigationVariable);
  try
    if FName <> Value then
    begin
      if (Model <> nil) and (Value <> '') then
      begin
        ChangeGlobals.Rename;

        FarmList := (Collection as TIrrigationCollection).FarmList;
        if FarmList <> nil then
        begin
          for FarmIndex := 0 to FarmList.Count - 1 do
          begin
            AFarm := FarmList[FarmIndex];
            if FName = '' then
            begin
              FoundMatch := False;
              for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyCollection.Count - 1 do
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = Value then
                begin
                  FoundMatch := True;
                  break;
                end;
              end;
              if not FoundMatch then
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyCollection.Add;
                AFarmEff.CropEfficiency.CropName := Value;
                AFarmEff.Index := index;
              end;
              FoundMatch := False;
              for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = Value then
                begin
                  FoundMatch := True;
                  break;
                end;
              end;
              if not FoundMatch then
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection.Add;
                AFarmEff.CropEfficiency.CropName := Value;
                AFarmEff.Index := index;
              end;
            end
            else
            begin
              for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyCollection.Count - 1 do
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = FName then
                begin
                  AFarmEff.CropEfficiency.CropName := Value;
                  break;
                end;
              end;
              for CropEffIndex := 0 to AFarm.FarmIrrigationEfficiencyImprovementCollection.Count - 1 do
              begin
                AFarmEff := AFarm.FarmIrrigationEfficiencyImprovementCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = FName then
                begin
                  AFarmEff.CropEfficiency.CropName := Value;
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
      SetCaseSensitiveStringProperty(FName, Value);
    end;

    if (Model <> nil) and (FName <> '') then
    begin
      ChangeGlobals.SetValue(Index+1);
    end;
  finally
    ChangeGlobals.Free;
  end;
end;

//procedure TIrrigationItem.SetStoredEfficiency(const Value: TRealStorage);
//begin
//  FStoredEfficiency.Assign(Value);
//end;

{ TIrrigationCollection }

constructor TIrrigationCollection.Create(Model: TBaseModel);
begin
  inherited Create(TIrrigationItem, Model);
end;

destructor TIrrigationCollection.Destroy;
begin
  FFarmList.Free;
  inherited;
end;

function TIrrigationCollection.GetFarmList: TFarmList;
var
  LocalModel: TPhastModel;
  FarmIndex: integer;
begin
  if FFarmList = nil then
  begin
    FFarmList := TFarmList.Create;
    Assert(Model <> nil);
    LocalModel := Model as TPhastModel;
    for FarmIndex := 0 to LocalModel.Farms.Count - 1 do
    begin
      FFarmList.Add(LocalModel.Farms[FarmIndex]);
    end;
  end;
  result := FFarmList
end;

function TIrrigationCollection.GetItems(Index: Integer): TIrrigationItem;
begin
  result := inherited Items[index] as TIrrigationItem
end;

procedure TIrrigationCollection.SetItems(Index: Integer;
  const Value: TIrrigationItem);
begin
  inherited Items[index] := Value;
end;

end.
