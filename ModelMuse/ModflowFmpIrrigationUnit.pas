unit ModflowFmpIrrigationUnit;

interface

uses
  System.Classes, System.SysUtils, OrderedCollectionUnit, GoPhastTypes, ModflowFmpFarmUnit,
  ModflowFmpBaseClasses, ModflowBoundaryUnit, RealListUnit,
  OrderedCollectionInterfaceUnit, FormulaManagerInterfaceUnit,
  ModflowParameterInterfaceUnit;

type
  TEvapFractionItem = class(TCustomZeroFarmItem)
  private
    const
    EvapIrrigateFractionPosition = 0;
    SWLossFractionIrrigationPosition = 1;
    function GetEvapIrrigateFraction: string;
    procedure SetEvapIrrigateFraction(const Value: string);
    function GetSurfaceWaterLossFractionIrrigation: string;
    procedure SetSurfaceWaterLossFractionIrrigation(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    // EVAPORATION_IRRIGATION_FRACTION
    property EvapIrrigateFraction: string read GetEvapIrrigateFraction
      write SetEvapIrrigateFraction;
    // SURFACEWATER_LOSS_FRACTION_IRRIGATION
    property SurfaceWaterLossFractionIrrigation: string
      read GetSurfaceWaterLossFractionIrrigation
      write SetSurfaceWaterLossFractionIrrigation;
  end;

  // @name represents the choice of irrigation type for one crop.
  TFmp4EvapFractionCollection = class(TCustomFarmCollection)
  private
    FSurfaceWaterLossFractionPestParamMethod: TPestParamMethod;
    FSurfaceWaterLossFractionPestSeriesParameterName: string;
    FSurfaceWaterLossFractionPestSeriesParameter: IModflowParameter;
    function GetItems(Index: Integer): TEvapFractionItem;
    procedure SetItems(Index: Integer; const Value: TEvapFractionItem);
    function GetSurfaceWaterLossFractionPestSeriesParameter: string;
    procedure SetSurfaceWaterLossFractionPestParamMethod(
      const Value: TPestParamMethod);
    procedure SetSurfaceWaterLossFractionPestSeriesParameter(
      const Value: string);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TEvapFractionItem read GetItems
      write SetItems; default;
    function ItemByStartTime(ATime: Double): TEvapFractionItem;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property SurfaceWaterLossFractionPestSeriesParameter: string
      read GetSurfaceWaterLossFractionPestSeriesParameter
      write SetSurfaceWaterLossFractionPestSeriesParameter;
    property SurfaceWaterLossFractionPestParamMethod: TPestParamMethod
      read FSurfaceWaterLossFractionPestParamMethod
      write SetSurfaceWaterLossFractionPestParamMethod;
  end;

  TIrrigationItem = class(TOrderedItem)
  private
    FName: string;
    FEvapFraction: TFmp4EvapFractionCollection;
    procedure SetName(Value: string);
    procedure SetEvapFraction(const Value: TFmp4EvapFractionCollection);
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean);
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean); virtual;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Loaded;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write SetName;
    property EvapFraction: TFmp4EvapFractionCollection read FEvapFraction
      write SetEvapFraction;
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
    procedure Loaded;
    procedure Clear;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean);
    constructor Create(Model: IModelForTOrderedCollection);
    destructor Destroy; override;
    property Items[Index: Integer]: TIrrigationItem read GetItems
      write SetItems; default;
  end;

implementation

uses PhastModelUnit, LockedGlobalVariableChangers;

resourcestring
  StrIrrigationVariable = 'Irrigation Variable';

{ TIrrigationItem }

procedure TIrrigationItem.AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
  Times: TRealList; StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean);
var
  BoundaryIndex: Integer;
  Boundary: TCustomModflowBoundaryItem;
  SP_Epsilon: Double;
  CosestIndex: Integer;
  ExistingTime: Double;
begin
  SP_Epsilon := (Model as TCustomModel).SP_Epsilon;
  for BoundaryIndex := 0 to BoundCol.Count - 1 do
  begin
    Boundary := BoundCol[BoundaryIndex] as TCustomModflowBoundaryItem;
    CosestIndex := Times.IndexOfClosest(Boundary.StartTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.StartTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.StartTime);
      end;
    end;
    CosestIndex := Times.IndexOfClosest(Boundary.EndTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.EndTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.EndTime);
      end;
    end;
//    Times.AddUnique(Boundary.StartTime);
//    Times.AddUnique(Boundary.EndTime);
    if (Boundary.StartTime < StartTestTime-SP_Epsilon) then
    begin
      StartRangeExtended := True;
    end;
    if (Boundary.EndTime > EndTestTime+SP_Epsilon) then
    begin
      EndRangeExtended := True;
    end;
//    if (Boundary.StartTime < StartTestTime) then
//    begin
//      StartRangeExtended := True;
//    end;
//    if (Boundary.EndTime > EndTestTime) then
//    begin
//      EndRangeExtended := True;
//    end;
  end;
end;

procedure TIrrigationItem.Assign(Source: TPersistent);
var
  SourceItem: TIrrigationItem;
begin
  if Source is TIrrigationItem then
  begin
    SourceItem := TIrrigationItem(Source);
    Name := SourceItem.Name;
    EvapFraction := SourceItem.EvapFraction;
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
        for CropEffIndex := 0 to AFarm.AddedDemandRunoffSplitCollection.Count - 1 do
        begin
          AFarmEff := AFarm.AddedDemandRunoffSplitCollection[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = Name then
          begin
            AFarmEff.Free;
            break;
          end;
        end;
        for CropEffIndex := 0 to AFarm.IrrigationUniformity.Count - 1 do
        begin
          AFarmEff := AFarm.IrrigationUniformity[CropEffIndex];
          if AFarmEff.CropEfficiency.CropName = Name then
          begin
            AFarmEff.Free;
            break;
          end;
        end;
      end;
    end;
  end;

  FEvapFraction := TFmp4EvapFractionCollection.Create(Model as TCustomModel);
end;


destructor TIrrigationItem.Destroy;
begin
  inherited;
  FEvapFraction.Free;
end;

function TIrrigationItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TIrrigationItem;
begin
  result := (AnotherItem is TIrrigationItem);
  if result then
  begin
    OtherItem := TIrrigationItem(AnotherItem);
    result := (Name  = OtherItem.Name)
    and EvapFraction.IsSame(OtherItem.EvapFraction)
  end;
end;

procedure TIrrigationItem.Loaded;
begin
  EvapFraction.Loaded;
end;

procedure TIrrigationItem.SetEvapFraction(
  const Value: TFmp4EvapFractionCollection);
begin
  FEvapFraction.Assign(Value);
end;

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
    and not (csReading in (Model as TComponent).ComponentState) then
  begin
    Value := GenerateNewName(Value, nil, '_');
  end;
  ChangeGlobals := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FName, Value,
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

              FoundMatch := False;
              for CropEffIndex := 0 to AFarm.AddedDemandRunoffSplitCollection.Count - 1 do
              begin
                AFarmEff := AFarm.AddedDemandRunoffSplitCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = Value then
                begin
                  FoundMatch := True;
                  break;
                end;
              end;
              if not FoundMatch then
              begin
                AFarmEff := AFarm.AddedDemandRunoffSplitCollection.Add;
                AFarmEff.CropEfficiency.CropName := Value;
                AFarmEff.Index := index;
              end;

              FoundMatch := False;
              for CropEffIndex := 0 to AFarm.IrrigationUniformity.Count - 1 do
              begin
                AFarmEff := AFarm.IrrigationUniformity[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = Value then
                begin
                  FoundMatch := True;
                  break;
                end;
              end;
              if not FoundMatch then
              begin
                AFarmEff := AFarm.IrrigationUniformity.Add;
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

              for CropEffIndex := 0 to AFarm.AddedDemandRunoffSplitCollection.Count - 1 do
              begin
                AFarmEff := AFarm.AddedDemandRunoffSplitCollection[CropEffIndex];
                if AFarmEff.CropEfficiency.CropName = FName then
                begin
                  AFarmEff.CropEfficiency.CropName := Value;
                  break;
                end;
              end;

              for CropEffIndex := 0 to AFarm.IrrigationUniformity.Count - 1 do
              begin
                AFarmEff := AFarm.IrrigationUniformity[CropEffIndex];
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

procedure TIrrigationItem.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean);
begin
  AddBoundaryTimes(EvapFraction, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
end;

procedure TIrrigationCollection.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].UpdateTimes(Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;
end;

{ TIrrigationCollection }

procedure TIrrigationCollection.Clear;
begin
  inherited;
  FreeAndNil(FFarmList);
end;

constructor TIrrigationCollection.Create(Model: IModelForTOrderedCollection);
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

procedure TIrrigationCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded
  end;
end;

procedure TIrrigationCollection.SetItems(Index: Integer;
  const Value: TIrrigationItem);
begin
  inherited Items[index] := Value;
end;

{ TEvapFractionItem }

function TEvapFractionItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TEvapFractionItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    EvapIrrigateFractionPosition:
      result := EvapIrrigateFraction;
    SWLossFractionIrrigationPosition:
      result := SurfaceWaterLossFractionIrrigation;
    else Assert(False);
  end;
end;

function TEvapFractionItem.GetEvapIrrigateFraction: string;
begin
  Result := FFormulaObjects[EvapIrrigateFractionPosition].Formula;
  ResetItemObserver(EvapIrrigateFractionPosition);
end;

function TEvapFractionItem.GetSurfaceWaterLossFractionIrrigation: string;
begin
  Result := FFormulaObjects[SWLossFractionIrrigationPosition].Formula;
  ResetItemObserver(SWLossFractionIrrigationPosition);
end;

procedure TEvapFractionItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    EvapIrrigateFractionPosition:
      EvapIrrigateFraction := Value;
    SWLossFractionIrrigationPosition:
      SurfaceWaterLossFractionIrrigation := Value;
    else Assert(False);
  end;
end;

procedure TEvapFractionItem.SetEvapIrrigateFraction(const Value: string);
begin
  if FFormulaObjects[EvapIrrigateFractionPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, EvapIrrigateFractionPosition,
      FFormulaObjects[EvapIrrigateFractionPosition]);
  end;
end;

procedure TEvapFractionItem.SetSurfaceWaterLossFractionIrrigation(
  const Value: string);
begin
  if FFormulaObjects[SWLossFractionIrrigationPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SWLossFractionIrrigationPosition,
      FFormulaObjects[SWLossFractionIrrigationPosition]);
  end;
end;

{ TFmp4EvapFractionCollection }

procedure TFmp4EvapFractionCollection.Assign(Source: TPersistent);
var
  OtherFarmCollection: TFmp4EvapFractionCollection;
begin
  if Source is TFmp4EvapFractionCollection then
  begin
    OtherFarmCollection := TFmp4EvapFractionCollection(Source);
    SurfaceWaterLossFractionPestSeriesParameter := OtherFarmCollection.SurfaceWaterLossFractionPestSeriesParameter;
    SurfaceWaterLossFractionPestParamMethod := OtherFarmCollection.SurfaceWaterLossFractionPestParamMethod;
  end;
  inherited;

end;

function TFmp4EvapFractionCollection.GetItems(
  Index: Integer): TEvapFractionItem;
begin
  result := inherited Items[Index] as TEvapFractionItem;
end;

function TFmp4EvapFractionCollection.GetSurfaceWaterLossFractionPestSeriesParameter: string;
begin
  if FSurfaceWaterLossFractionPestSeriesParameter <> nil then
  begin
    FSurfaceWaterLossFractionPestSeriesParameterName := FSurfaceWaterLossFractionPestSeriesParameter.ParameterName;
  end;
  result := FSurfaceWaterLossFractionPestSeriesParameterName;
end;

function TFmp4EvapFractionCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  OtherFarmCollection: TFmp4EvapFractionCollection;
begin
  result := (AnOrderedCollection is TFmp4EvapFractionCollection)
    and inherited;
  if result then
  begin
    OtherFarmCollection := TFmp4EvapFractionCollection(AnOrderedCollection);
    result := (SurfaceWaterLossFractionPestSeriesParameter = OtherFarmCollection.SurfaceWaterLossFractionPestSeriesParameter)
      and (SurfaceWaterLossFractionPestParamMethod = OtherFarmCollection.SurfaceWaterLossFractionPestParamMethod)
  end;
end;

function TFmp4EvapFractionCollection.ItemByStartTime(
  ATime: Double): TEvapFractionItem;
var
  TimeIndex: Integer;
  AnItem: TEvapFractionItem;
begin
  result := nil;
  for TimeIndex := 0 to Count - 1 do
  begin
    AnItem := Items[TimeIndex];
    if AnItem.StartTime <= ATime then
    begin
      result := AnItem;

      if AnItem is TCustomModflowBoundaryItem then
      begin
        if TCustomModflowBoundaryItem(AnItem).EndTime > ATime then
        begin
          Break;
        end;
      end;
    end;
  end;
end;

class function TFmp4EvapFractionCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEvapFractionItem;
end;

procedure TFmp4EvapFractionCollection.Loaded;
begin
  if (Model <> nil) then
  begin
    if (FSurfaceWaterLossFractionPestSeriesParameterName <> '') then
    begin
      FSurfaceWaterLossFractionPestSeriesParameter :=
        Model.GetPestParameterByNameI(FSurfaceWaterLossFractionPestSeriesParameterName)
    end;
  end;
  inherited;
end;

procedure TFmp4EvapFractionCollection.SetItems(Index: Integer;
  const Value: TEvapFractionItem);
begin
  inherited Items[Index] := Value;
end;

procedure TFmp4EvapFractionCollection.SetSurfaceWaterLossFractionPestParamMethod(
  const Value: TPestParamMethod);
begin
  if FSurfaceWaterLossFractionPestParamMethod <> Value then
  begin
    FSurfaceWaterLossFractionPestParamMethod := Value;
  end;
end;

procedure TFmp4EvapFractionCollection.SetSurfaceWaterLossFractionPestSeriesParameter(
  const Value: string);
begin
  if FSurfaceWaterLossFractionPestSeriesParameterName <> Value then
  begin
    FSurfaceWaterLossFractionPestSeriesParameterName := Value;
    InvalidateModel;
  end;
  if (Model <> nil) then
  begin
    if Value <> '' then
    begin
      FSurfaceWaterLossFractionPestSeriesParameter := Model.GetPestParameterByNameI(Value);
      if FSurfaceWaterLossFractionPestSeriesParameter = nil then
      begin
        FSurfaceWaterLossFractionPestSeriesParameterName := '';
        InvalidateModel;
      end;
    end
    else
    begin
      FSurfaceWaterLossFractionPestSeriesParameter := nil;
    end;
  end;
end;

end.
