unit ModflowTransientListParameterUnit;

interface

uses SysUtils, Classes, OrderedCollectionUnit, ModflowParameterUnit,
  GoPhastTypes, ModflowTimeSeriesUnit;

type
  TModflowTransientListParameters = class;

  TChildModelValue = class(TOrderedItem)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FChildModel: TBaseModel;
    FChildModelName: string;
    FValue: double;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetChildModel: TBaseModel;
    function GetChildModelName: string;
    procedure SetValue(const Value: double);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure SetChildModel(const Value: TBaseModel);
    procedure SetChildModelName(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property ChildModel: TBaseModel read GetChildModel write SetChildModel;
  published
    property Value: double read FValue write SetValue;
    property ChildModelName: string read GetChildModelName
      write SetChildModelName;
  end;

  TChildModelValues = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: integer): TChildModelValue;
    procedure SetItems(Index: integer; const Value: TChildModelValue);
    procedure Loaded;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TChildModelValue
      read GetItems write SetItems; default;
  end;

  TModflowTransientListParameter = class(TModflowParameter)
  private
    FChildModelValues: TChildModelValues;
    FCellCount: Integer;
    FTimeSeriesList: TTimeSeriesList;
    function Collection: TModflowTransientListParameters;
    procedure SetChildModelValues(const Value: TChildModelValues);
    procedure SetCellCount(const Value: Integer);
  protected
    procedure SetParameterName(const Value: string); override;
    procedure SetParameterType(const Value: TParameterType); override;
    procedure SetValue(Value : double); override;
    procedure Loaded;
  public
    // @name is the number of cells defined by this parameter.
    property CellCount: Integer read FCellCount write SetCellCount;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ChildModelBeingDestroyed(Model: TBaseModel);
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure NewChildModelCreated(Model: TBaseModel);
    property TimeSeriesList: TTimeSeriesList read FTimeSeriesList;
  published
    property ChildModelValues: TChildModelValues read FChildModelValues
      write SetChildModelValues;
  end;

  TModflowTransientListParameters = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: integer): TModflowTransientListParameter;
    procedure SetItems(Index: integer;
      const Value: TModflowTransientListParameter);
  public
    procedure UpdateDisplay(Value: TModflowTransientListParameter);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TModflowTransientListParameter
      read GetItems write SetItems; default;
    function GetParamByName(
      const ParamName: string): TModflowTransientListParameter;
    function CountParam(ParameterType: TParameterType): integer;
    procedure Loaded;
    function ParamNameIndex(const ParamName: string): integer;
    function Add: TModflowTransientListParameter;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit,
  ModflowSfrParamIcalcUnit, ModflowPackageSelectionUnit, frmGoPhastUnit;

{ TModflowTransientListParameter }


procedure TModflowTransientListParameter.Assign(Source: TPersistent);
var
  SourceParam: TModflowTransientListParameter;
  ParmChanged: boolean;
begin
  // if Assign is updated, update IsSame too.
  if Source is TModflowTransientListParameter then
  begin
    SourceParam := TModflowTransientListParameter(Source);
    ParmChanged := not IsSame(SourceParam);
    if ParmChanged and (Collection <> nil) then
    begin
      Collection.UpdateDisplay(self);
    end;
    inherited;
    if ParmChanged and (Collection <> nil) then
    begin
      Collection.UpdateDisplay(SourceParam);
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowTransientListParameter.ChildModelBeingDestroyed(
  Model: TBaseModel);
var
  ChildIndex: Integer;
begin
  if ParameterType in [ptGHB,ptRIV, ptDRN, ptDRT, ptSFR] then
  begin
    for ChildIndex := 0 to ChildModelValues.Count - 1 do
    begin
      if Model = ChildModelValues[ChildIndex].ChildModel then
      begin
        ChildModelValues.Delete(ChildIndex);
        Exit;
      end;
    end;
  end;
end;

function TModflowTransientListParameter.Collection: TModflowTransientListParameters;
begin
  result := inherited Collection as TModflowTransientListParameters;
end;

constructor TModflowTransientListParameter.Create(Collection: TCollection);
begin
  inherited;
  FChildModelValues := TChildModelValues.Create(Model);
  FTimeSeriesList := TTimeSeriesList.Create;
end;

destructor TModflowTransientListParameter.Destroy;
var
  LocalModel: TPhastModel;
  ParameterInstances: TSfrParamInstances;
begin
  FTimeSeriesList.Free;
  if (ParameterType = ptSfr) and (Model <> nil) then
  begin
    LocalModel := Model as TPhastModel;
    ParameterInstances := LocalModel.ModflowPackages.SfrPackage.ParameterInstances;
    ParameterInstances.DeleteInstancesOfParameter(ParameterName);
  end;
  if Collection <> nil then
  begin
    (Collection as TModflowTransientListParameters).UpdateDisplay(self);
  end;
  FChildModelValues.Free;
  inherited;
end;

function TModflowTransientListParameter.IsSame(
  AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TModflowTransientListParameter)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    result := ChildModelValues.IsSame(
      TModflowTransientListParameter(AnotherItem).ChildModelValues)
  end;
end;

procedure TModflowTransientListParameter.Loaded;
begin
  ChildModelValues.Loaded;
end;

procedure TModflowTransientListParameter.NewChildModelCreated(
  Model: TBaseModel);
var
  ChildItem: TChildModelValue;
  ChildIndex: Integer;
begin
  if ParameterType in [ptGHB,ptRIV, ptDRN, ptDRT, ptSFR] then
  begin
    for ChildIndex := 0 to ChildModelValues.Count - 1 do
    begin
      if ChildModelValues[ChildIndex].ChildModelName =
        (Model as TChildModel).ModelName then
      begin
        ChildModelValues[ChildIndex].ChildModel := Model;
        Exit;
      end;
    end;
    ChildItem := ChildModelValues.Add as TChildModelValue;
    ChildItem.ChildModel := Model;
    ChildItem.FValue := Value;
  end;
end;

procedure TModflowTransientListParameter.SetCellCount(const Value: Integer);
begin
  FCellCount := Value;
  if FCellCount = 0 then
  begin
    FTimeSeriesList.Clear;
  end;
end;

procedure TModflowTransientListParameter.SetChildModelValues(
  const Value: TChildModelValues);
begin
  FChildModelValues.Assign(Value);
end;

procedure TModflowTransientListParameter.SetParameterName(const Value: string);
var
  Model: TPhastModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: integer;
  Boundary: TModflowParamBoundary;
  NameIndex: Integer;
  Item: TSfrParamIcalcItem;
  NewName: string;
begin
  NewName := CorrectParamName(Value);
  if FParameterName <> NewName then
  begin
    if ParameterType in [ptRCH,ptEVT,ptETS, ptCHD,ptGHB,ptQ,ptRIV,ptDRN,ptDRT,ptSFR] then
    begin
      if (Collection <> nil) and  (Collection.Model <> nil) then
      begin
        Model := Collection.Model as TPhastModel;
        for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
        begin
          ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
          if ParameterType = ptSFR then
          begin
            if ScreenObject.ModflowSfrBoundary <> nil then
            begin
              for NameIndex := 0 to ScreenObject.ModflowSfrBoundary.
                ParamIcalc.Count - 1 do
              begin
                Item := ScreenObject.ModflowSfrBoundary.
                  ParamIcalc.Items[NameIndex];
                if Item.Param = FParameterName then
                begin
                  Item.Param := NewName
                end;
              end;
            end;
          end
          else
          begin
            Boundary := ScreenObject.GetMfBoundary(ParameterType);
            if Boundary <> nil then
            begin
              ParamIndex := Boundary.Parameters.IndexOfParam(self);
              if ParamIndex >= 0 then
              begin
                Boundary.Parameters[
                  ParamIndex].Param.ParamName := NewName;
              end;
            end;
          end;
        end;
      end;
    end;
    FParameterName := NewName;
    InvalidateModel;
  end;
end;

procedure TModflowTransientListParameter.SetParameterType(
  const Value: TParameterType);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  if Collection <> nil then
  begin
    (Collection as TModflowTransientListParameters).UpdateDisplay(self);
    LocalModel := Model as TPhastModel;
    if LocalModel <> nil then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          NewChildModelCreated(ChildModel);
        end;
      end;
    end;
  end;
end;

procedure TModflowTransientListParameter.SetValue(Value: double);
begin
  inherited;
  if Collection <> nil then
  begin
    (Collection as TModflowTransientListParameters).UpdateDisplay(self);
  end;
end;

{ TModflowTransientListParameters }

function TModflowTransientListParameters.Add: TModflowTransientListParameter;
begin
  result := inherited Add as TModflowTransientListParameter;
end;

function TModflowTransientListParameters.CountParam(
  ParameterType: TParameterType): integer;
var
  Index: Integer;
  Item: TModflowTransientListParameter;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ParameterType = ParameterType then
    begin
      Inc(result);
    end;
  end;
end;

constructor TModflowTransientListParameters.Create(Model: TBaseModel);
begin
  inherited Create(TModflowTransientListParameter, Model);
end;

function TModflowTransientListParameters.GetItems(
  Index: integer): TModflowTransientListParameter;
begin
  result := inherited Items[Index] as TModflowTransientListParameter;
end;

function TModflowTransientListParameters.GetParamByName(
  const ParamName: string): TModflowTransientListParameter;
var
  Index: Integer;
//  Item: TModflowTransientListParameter;
begin
  result := nil;
  Index := ParamNameIndex(ParamName);
  if Index >= 0 then
  begin
    result := Items[Index];
  end;
end;

procedure TModflowTransientListParameters.Loaded;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Items[ItemIndex].Loaded;
  end;
end;

function TModflowTransientListParameters.ParamNameIndex(
  const ParamName: string): integer;
var
  Index: Integer;
  Item: TModflowTransientListParameter;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if SameText(Item.ParameterName, ParamName) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TModflowTransientListParameters.UpdateDisplay(
  Value: TModflowTransientListParameter);
begin
  case Value.ParameterType of
    ptUndefined: ;
    ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB:
      begin
        Assert(False);
      end;
    ptRCH:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfRchRate(self);
        end;
      end;
    ptEVT:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfEvtEvapRate(self);
        end;
      end;
    ptETS:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfEtsEvapRate(self);
        end;
      end;
    ptCHD:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfChdStartingHead(self);
          (Model as TPhastModel).InvalidateMfChdEndingHead(self);
        end;
      end;
    ptGHB:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfGhbConductance(self);
        end;
      end;
    ptQ:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfWellPumpage(self);
        end;
      end;
    ptRIV:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfRivConductance(self);
        end;
      end;
    ptDRN:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfDrnConductance(self);
        end;
      end;
    ptDRT: 
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfDrtConductance(self);
        end;
      end;
    ptSfr:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfSfrData(self);
        end;
      end;
    ptStr:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfStrConductance(self);
        end;
      end;
    ptQMAX:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfFmpMaxPumpingRate(self);
        end;
      end
    else Assert(False);
  end;
end;

procedure TModflowTransientListParameters.SetItems(Index: integer;
  const Value: TModflowTransientListParameter);
var
  ParmChanged: boolean;
begin
  ParmChanged := not Items[Index].IsSame(Value);
  if ParmChanged then
  begin
    UpdateDisplay(Items[Index]);
  end;
  inherited Items[Index] := Value;
  if ParmChanged then
  begin
    UpdateDisplay(Value);
  end;
end;

{ TChildModelValue }

function TChildModelValue.GetChildModel: TBaseModel;
var
  ChildIndex: Integer;
  AChildModel: TChildModel;
begin
  if (FChildModel = nil) and (FChildModelName <> '') then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.ChildModels <> nil) then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if AChildModel <> nil then
        begin
          if AChildModel.ModelName = FChildModelName then
          begin
            FChildModel := AChildModel;
            break;
          end;
        end;
      end;
    end;
  end;
  result := FChildModel;
end;

function TChildModelValue.GetChildModelName: string;
begin
  if FChildModel <> nil then
  begin
    result := (FChildModel as TChildModel).ModelName
  end
  else
  begin
    Result := FChildModelName;
  end;

end;

function TChildModelValue.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherCMValue: TChildModelValue;
begin
  result := AnotherItem is TChildModelValue;
  if result then
  begin
    AnotherCMValue := TChildModelValue(AnotherItem);
    result := (Value = AnotherCMValue.Value)
      and (ChildModelName = AnotherCMValue.ChildModelName);
  end;
end;

procedure TChildModelValue.SetChildModel(const Value: TBaseModel);
begin
  if FChildModel <> Value then
  begin
    FChildModel := Value;
    InvalidateModel;
  end;
  if FChildModel = nil then
  begin
    ChildModelName := '';
  end
  else
  begin
    ChildModelName := (FChildModel as TChildModel).ModelName;
  end;
end;

procedure TChildModelValue.SetChildModelName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FChildModelName, Value);
end;

procedure TChildModelValue.SetValue(const Value: double);
begin
  SetRealProperty(FValue, Value);
end;

{ TChildModelValues }

constructor TChildModelValues.Create(Model: TBaseModel);
begin
  inherited Create(TChildModelValue, Model);
end;

function TChildModelValues.GetItems(Index: integer): TChildModelValue;
begin
  result := inherited Items[Index] as TChildModelValue
end;

procedure TChildModelValues.Loaded;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Items[ItemIndex].ChildModel;
  end;
end;

procedure TChildModelValues.SetItems(Index: integer;
  const Value: TChildModelValue);
begin
  inherited Items[Index] := Value;
end;

end.
