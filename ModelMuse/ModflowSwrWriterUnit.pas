unit ModflowSwrWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, SysUtils,
  Forms, Classes, ModflowBoundaryDisplayUnit, GoPhastTypes,
  PhastModelUnit, Generics.Collections, ModflowSwrReachUnit,
  ScreenObjectUnit, FastGEO, ModflowSwrStructureUnit, SwrReachObjectUnit,
  ModflowSwrObsUnit, IntListUnit;

type
  TStructureList = TList<TStructure>;

  TTransientStructureList = TObjectList<TStructureList>;

  TModflowSwrWriter = class(TCustomTransientWriter)
  private
    FEvapValues: TList;
    FLatInflowValues: TList;
    FStageValues: TList;
    FDirectRunoffValues: TList;
    FTransientReachList: TList;
    FReachList: TObjectList<TReachObject>;
    // @name contains only @link(TScreenObject)s used by SWR.
    FScreenObjectList: TList<TScreenObject>;
    FSwrPackage: TSwrPackage;
//    FNameOfFile: string;
    FStuctureDict: TObjectDictionary<Integer,TStructureList>;
    FDirectRunoffFileName: string;
    NREACHES: integer;
    procedure WriteStressPeriods;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4A;
    procedure WriteDataSet4B;
    procedure WriteDataSet4C;
    procedure WriteDataSet4D;
    procedure WriteDataSet4E;
    procedure WriteDataSet4F;
    procedure WriteGeometry;
    procedure WriteRainArray(CellList: TList);
    procedure WriteRainList(CellList: TList);
    procedure WriteEvapArray(CellList: TList);
    procedure WriteEvapList(CellList: TList);
    procedure WriteLatInflowArray(CellList: TList);
    procedure WriteLatInflowList(CellList: TList);
    procedure WriteStageArray(CellList: TList);
    procedure ClearTimeLists(AModel: TBaseModel);
    // @seealso(TCustomModel.UpdateSwrReachNumber)
    procedure EvaluateSteadyReachData;
    procedure UpdateKRCH;
    procedure EvaluateTransientReachData;
    procedure EvaluateRain;
    procedure EvaluateEvaporation;
    procedure EvaluateLatInflow;
    procedure EvaluateStage;
    procedure EvaluateDirectRunoff;
    procedure EvaluateStructures;
    procedure UpdateADisplayList(TimeLists: TModflowBoundListOfTimeLists;
      ValueLists: TList; Method: TSwrSpecificationMethod;
      UpdateMethod: TUpdateMethod; DataTypeIndex: Integer = 0);
    function IndexOfReach(Reach: Integer): integer;
    procedure EvaluateInterObjectConnections(ReachLists: TStringList);
    procedure AddSearchPoints(ScreenObject: TScreenObject;
      UsedReachs, AllObjectReaches: TList<TReachObject>;
      Points: TList<TPoint2D>);
    procedure AssignFinalReachNumbers;
    procedure WriteDataSet5(TimeIndex: Integer);
    procedure WriteDataSet6(TimeIndex: Integer);
    procedure WriteDataSet7(TimeIndex: Integer);
    procedure WriteDataSet8(TimeIndex: Integer);
    procedure WriteDataSet9(TimeIndex: Integer);
    procedure WriteDataSet10(TimeIndex: Integer);
    procedure WriteDataSet11(TimeIndex: Integer);
    procedure WriteDataSet12(TimeIndex: Integer);
    procedure WriteDataSet13(TimeIndex: Integer);
    procedure WriteDataSet14(TimeIndex: Integer);
    procedure WriteDataSet14A(TimeIndex: Integer);
    procedure WriteDataSet15(TimeIndex: Integer);
    procedure WriteTransientSwrArray(CellList: TList;
      AssignmentMethod: TUpdateMethod; Comment: string;
      const MF6_ArrayName: string);
    procedure WriteSwrTransientList(CellList: TList; Comment: string);
    function GetReach(Index: Integer): TReachObject;
    function GetReachCount: Integer;
    function ObsTypeToString(ObsType: TSwrObsType): string;
    procedure AssignReachObservationLinks;
    procedure CheckThatTransientDataFullyDefined;
    procedure CheckStructuresUsed;
  protected
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateRainDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateEvapDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateLatInflowDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateStageDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateDirectRunoffDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateVerticalOffsetDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateBoundaryTypeDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateGeometryNumberDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateReachNumberDisplay;
    property ReachCount: Integer read GetReachCount;
    property Reaches[Index: Integer]: TReachObject read GetReach;
    function ObsTypeToName(ObsType: TSwrObsType;
      AReachObject: TReachObject; ConnectedReach: TReachObject = nil;
      StructureCount: Integer = 0): string;
  end;

const
  StrSwrReachGroupFlowsA = '.Swr_ReachGroupFlows_A';
  StrSwrReachGroupFlowsB = '.Swr_ReachGroupFlows_B';
  StrSwrReachStageA = '.Swr_ReachStage_A';
  StrSwrReachStageB = '.Swr_ReachStage_B';
  StrSwrReachExchangeA = '.Swr_ReachExchange_A';
  StrSwrReachExchangeB = '.Swr_ReachExchange_B';
  StrSwrLateralFlowA = '.Swr_LateralFlow_A';
  StrSwrLateralFlowB = '.Swr_LateralFlow_B';
  StrSwrStructureFlowA = '.Swr_StructureFlow_A';
  StrSwrStructureFlowB = '.Swr_StructureFlow_B';
  StrSwrTimeStepLengthA = '.Swr_TimeStepLength_A';
  StrSwrTimeStepLengthB = '.Swr_TimeStepLength_B';
  StrSwrConvergence = '.Swr_Convergence';
  StrSwrRIV = '.Swr_RIV';
  StrSwrObsExt_A = '.Swr_Obs_A';
  StrSwrObsExt_B = '.Swr_Obs_B';
  StrSwrDirectRunoffExt = '.Swr_DirectRunoff';

implementation

uses
  ModflowSwrUnit, frmErrorsAndWarningsUnit, frmProgressUnit,
  ModflowCellUnit, ModflowUnitNumbers, RbwParser, DataSetUnit, Contnrs,
  ModflowSwrDirectRunoffUnit, AbstractGridUnit, GIS_Functions,
  ModflowSwrTabfilesUnit, ModflowSwrReachGeometryUnit,
  ModflowTimeUnit;

resourcestring
  StrEvaluatingRainSWRPacka = 'Evaluating rain in the SWR Package';
  StrEvaluatingEvapSWRPacka = 'Evaluating evaporation in the SWR Package';
  StrEvaluatingLatInflSWRPacka = 'Evaluating lateral inflow in the SWR Package';
  StrEvaluatingStageSWRPacka = 'Evaluating stage in the SWR Package';
  StrEvaluatingDirectRunoffSWRPacka = 'Evaluating direct runoff in the SWR Package';
  StrWritingStressP = '    Writing Stress Period %d';
  StrNoRainBoundaryConditio = 'No rain boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrNoEvapBoundaryConditio = 'No evaporation boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrNoLatInfloBoundaryConditio = 'No lateral inflow boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrNoStageBoundaryConditio = 'No stage assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrNoDirectRunoffBoundaryConditio = 'No direct runoff assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  Str0sIn1s = '%0:s in %1:s.';
  StrNoSWRObservations = 'No SWR observations defined.';
  StrAlthoughYouHaveSp = 'Although you have specified that observations in t' +
  'he SWR package should be saved, no observations have been defined in the ' +
  'SWR package.';
  StrInvalidTabFileNam = 'Invalid tab file name';
  StrStructure0sTab = 'Structure: %0:s; Tabfile: %1:s';
  StrInvalidStructureDe = 'Invalid Structure Definition';
  StrInvalidStructureCo = 'Invalid Structure Connection Definition';
  StrStructureReachAbsent = 'A structure was defined for reach %d but that r' +
  'each does not exist';
  StrStructureBadConnection = 'A structure was defined between reach %0:d an' +
  'd %1:d but those reaches are not connected';
  StrSWRReachNumberToo = 'SWR Reach number too large';
  StrTheReachNumber0 = 'The reach number %0:d at [Layer, Row, Col] = [%1:d, ' +
  '%2:d %3:d] defined by object %4:s is too large. Reach numbers must not ex' +
  'ceed the number of reaches. The number of reaches for this model is %5:d';
  StrIllegalDuplicateSW = 'Illegal duplicate SWR reach number';
  StrSWRReachNumber0 = 'SWR Reach number %:0d has been assigned to two or mo' +
  're reaches. Locations [Layer,Row,Col] = [%1:d, %2:d, %3:d] and [%4:d, %5:' +
  'd, %6:d]. Objects = %7:s and %8:s.';
  StrSWRTabfileNameNot = 'SWR Tabfile name not specified';
  StrSWRTabfileDoesNot = 'SWR Tabfile does not exist';
  StrInTheSWRTabfiles = 'In the SWR Tabfiles dialog box, a tab file has been' +
  ' defined but a file name for it has not been specified.';
  StrTheSWRTabfileName = 'The SWR Tabfile named "%s" does not exist.';
  StrTheSWRPackageIsN = 'The SWR package is not supported by MT3DMS.';
  StrMT3DMSVersion53D = 'MT3DMS version 5.3 and MT3D-USGS do not suppport the SWR packag' +
  'e.';
  StrUndefinedTransient = 'Undefined transient SWR data';
  StrInStressPeriodD = 'In stress period %d, some objects that define SWR re' +
  'aches do not define transient';
  StrSomeObjectsThatDe = 'Some objects that define SWR reaches do not define' +
  ' transient data for all stress periods';
  StrSWRTransientDataU = 'SWR Transient Data Undefined';
  StrTheObjectNamedS = 'The object named "%s" does not define any transient ' +
  'SWR data.';
  StrInvalidSWRObservat = 'Invalid SWR Observation Reach';
  StrTheReachSpecified = 'The reach specified for the SWR Observation named ' +
  '%s does not exist.';
  StrInitialSWRStagesN = 'Initial SWR stages not specified';
  StrYouMustSpecifyThe = 'You must specify the stage for all SWR reaches in ' +
  'the first stress period.';
  StrYouMustSpecifyTheFormat = 'You must specify the stage for all SWR reach' +
  'es in the first stress period. Your model has %0:d reaches but the stage ' +
  'was only specified for %1:d';
  StrTheSWRStreamBotto = 'The SWR stream bottom is above the top of the mode' +
  'l';
  StrAtRowColumn = 'At (Row, Column) = (%0:d, %1:d), the bottom of the strea' +
  'm bed is %2:g, and the stream bed vertical elevation offset is %3:g. Toge' +
  'ther, these add up to %4:g which is higher than the top of the model (%5:' +
  'g).';
//  StrUnusedSWRReachGeo = 'Unused SWR reach geometry';
//  StrTheSWRReachGeomet = 'The SWR reach geometry named "%s" has not been use' +
//  'd. This can cause problems for SWR.';

{ TModflowSwrWriter }

procedure TModflowSwrWriter.CheckStructuresUsed;
var
  StructureUsed: Boolean;
  TimeIndex: Integer;
  StructureIndex: Integer;
  AStructure: TStructure;
  StructureTime: TStructureTimeItem;
begin
  for StructureIndex := 0 to Model.SwrStructures.Count - 1 do
  begin
    AStructure := Model.SwrStructures[StructureIndex];
    StructureUsed := False;
    for TimeIndex := 0 to AStructure.Times.Count - 1 do
    begin
      StructureTime := AStructure.Times[TimeIndex];
      StructureUsed := StructureTime.Used;
      if StructureUsed then
      begin
        break;
      end;
    end;
    if not StructureUsed then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingSWRSt, AStructure.Name);
    end;
  end;
end;

procedure TModflowSwrWriter.CheckThatTransientDataFullyDefined;
var
  index: Integer;
//  SwrBoundary: TSwrReachBoundary;
  StartTime: Double;
  EndTime: Double;
  Reaches: TSwrReachCollection;
  AScreenObject: TScreenObject;
  AnItem: TSwrTransientReachItem;
  FirstItem: TSwrTransientReachItem;
  LastItem: TSwrTransientReachItem;
  TimeIndex: Integer;
  PriorItem: TSwrTransientReachItem;
begin
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  EndTime := Model.ModflowFullStressPeriods.Last.EndTime;
  for index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[index];
//    SwrBoundary := FScreenObjectList[index].ModflowSwrReaches;
    Reaches := AScreenObject.ModflowSwrReaches.ReachValues;
    if Reaches.Count > 0 then
    begin
      FirstItem := Reaches.First as TSwrTransientReachItem;
      if FirstItem.StartTime > StartTime then
      begin
        frmErrorsAndWarnings.AddError(Model, StrSWRTransientDataU,
          Format('The object named "%0:s" does not define SWR transient data for the period between %1:g and %2:g',
          [AScreenObject.Name, StartTime, FirstItem.StartTime]), AScreenObject);
      end;
      for TimeIndex := 1 to Reaches.Count - 1 do
      begin
        PriorItem := Reaches[TimeIndex-1] as TSwrTransientReachItem;
        AnItem := Reaches[TimeIndex] as TSwrTransientReachItem;
        if PriorItem.EndTime < AnItem.StartTime then
        begin
          frmErrorsAndWarnings.AddError(Model, StrSWRTransientDataU,
            Format('The object named "%0:s" does not define SWR transient data for the period between %1:g and %2:g',
            [AScreenObject.Name, PriorItem.EndTime, AnItem.StartTime]), AScreenObject);
        end;
      end;
      LastItem := Reaches.Last as TSwrTransientReachItem;
      if LastItem.EndTime < EndTime then
      begin
        frmErrorsAndWarnings.AddError(Model, StrSWRTransientDataU,
          Format('The object named "%0:s" does not define SWR transient data for the period between %1:g and %2:g',
          [AScreenObject.Name, LastItem.EndTime, EndTime]), AScreenObject);
      end;
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, StrSWRTransientDataU,
        Format(StrTheObjectNamedS, [AScreenObject.Name]), AScreenObject);
    end;
//    if SwrBoundary. then
//
//    begin
//
//    end
//    else
//    begin
//
//    end;
  end;
end;

procedure TModflowSwrWriter.ClearTimeLists(AModel: TBaseModel);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  RainBoundary: TSwrRainBoundary;
  EvapBoundary: TSwrEvapBoundary;
  LatInflowBoundary: TSwrLatInflowBoundary;
  StageBoundary: TSwrStageBoundary;
  DirectRunoff: TSwrDirectRunoffBoundary;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    RainBoundary := ScreenObject.ModflowSwrRain;
    if RainBoundary <> nil then
    begin
      RainBoundary.ClearTimeLists(AModel);
    end;
    EvapBoundary := ScreenObject.ModflowSwrEvap;
    if EvapBoundary <> nil then
    begin
      EvapBoundary.ClearTimeLists(AModel);
    end;
    LatInflowBoundary := ScreenObject.ModflowSwrLatInflow;
    if LatInflowBoundary <> nil then
    begin
      LatInflowBoundary.ClearTimeLists(AModel);
    end;
    StageBoundary := ScreenObject.ModflowSwrStage;
    if StageBoundary <> nil then
    begin
      StageBoundary.ClearTimeLists(AModel);
    end;
    DirectRunoff := ScreenObject.ModflowSwrDirectRunoff;
    if DirectRunoff <> nil then
    begin
      DirectRunoff.ClearTimeLists(AModel);
    end;
  end;

end;

constructor TModflowSwrWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FSwrPackage := Model.ModflowPackages.SwrPackage;
  FReachList:= TObjectList<TReachObject>.Create;
  FEvapValues := TObjectList.Create;
  FLatInflowValues := TObjectList.Create;
  FStageValues := TObjectList.Create;
  FDirectRunoffValues := TObjectList.Create;
  FTransientReachList := TObjectList.Create;
  FScreenObjectList := TList<TScreenObject>.Create;
  FStuctureDict := TObjectDictionary<Integer,TStructureList>.Create([doOwnsValues], Model.SwrStructures.Count);
end;

destructor TModflowSwrWriter.Destroy;
begin
  FStuctureDict.Free;
  FScreenObjectList.Free;
  FTransientReachList.Free;
  FDirectRunoffValues.Free;
  FStageValues.Free;
  FLatInflowValues.Free;
  FEvapValues.Free;
  FReachList.Free;
  inherited;
end;

procedure TModflowSwrWriter.Evaluate;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRGeometryNotDef);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoSWRObservations);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidStructureDe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidStructureCo);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRReachNumberToo);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrIllegalDuplicateSW);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRTabfileNameNot);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRTabfileDoesNot);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheSWRPackageIsN);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUndefinedTransient);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSWRObservat);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingSWRSt);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInitialSWRStagesN);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheSWRStreamBotto);

  if Model.ModflowPackages.Mt3dBasic.IsSelected then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTheSWRPackageIsN,
      StrMT3DMSVersion53D);
  end;

  EvaluateSteadyReachData;
  CheckThatTransientDataFullyDefined;
  EvaluateTransientReachData;
  UpdateKRCH;
  EvaluateRain;
  EvaluateEvaporation;
  EvaluateLatInflow;
  EvaluateStage;
  EvaluateDirectRunoff;
  CheckStructuresUsed;
end;

procedure TModflowSwrWriter.EvaluateDirectRunoff;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ErrorRoot: string;
  ABoundary: TSwrDirectRunoffBoundary;
begin
  ErrorRoot := Format(StrNoDirectRunoffBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    frmProgressMM.AddMessage(StrEvaluatingDirectRunoffSWRPacka);
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.ModflowSwrDirectRunoff;
      if ABoundary <> nil then
      begin
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, ErrorRoot, ScreenObject.Name,
            ScreenObject);
        end;
        ABoundary.GetCellValues(FDirectRunoffValues, nil, Model, self);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowSwrWriter.Extension: string;
begin
  result := '.swr';
end;

function TModflowSwrWriter.GetReach(Index: Integer): TReachObject;
begin
  Result := FReachList[Index];
end;

function TModflowSwrWriter.GetReachCount: Integer;
begin
  Result := FReachList.Count;
end;

function TModflowSwrWriter.IndexOfReach(Reach: Integer): integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to FReachList.Count - 1 do
  begin
    if FReachList[index].FReachData.Reach = Reach then
    begin
      result := index;
      exit;
    end;
  end;
end;

function TModflowSwrWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SwrPackage;
end;

procedure TModflowSwrWriter.UpdateEvapDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SwrPackage: TSwrPackage;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the Evaporation data used in the package.
    EvaluateEvaporation;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    SwrPackage := Model.ModflowPackages.SwrPackage;
    UpdateADisplayList(TimeLists, FEvapValues,
      SwrPackage.EvapSpecification, SwrPackage.EvapAssignmentMethod);

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateGeometryNumberDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the vertical offset data used in the package.
    EvaluateTransientReachData;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    UpdateADisplayList(TimeLists, FTransientReachList,
      smObject, umAdd, 2);
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateKRCH;
var
  TimeIndex: Integer;
  ValueCellList: TValueCellList;
  CellIndex: Integer;
  TransientCell: TSwrTransientCell;
  GeoItem: TReachGeometryItem;
begin
  // 1. For reaches that occupy entire cells KRCH must be 1.
  // This is set in UpdateKRCH.
  for TimeIndex := 0 to FTransientReachList.Count -1 do
  begin
    ValueCellList := FTransientReachList[TimeIndex];

    if ValueCellList.Count <> FReachList.Count then
    begin
      frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
        Format(StrInStressPeriodD, [TimeIndex +1]));
      Exit;
    end;

    Assert(ValueCellList.Count = FReachList.Count);
    for CellIndex := 0 to ValueCellList.Count - 1 do
    begin
      TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
      GeoItem := Model.SwrReachGeometry[TransientCell.GeoNumber-1];
      if GeoItem.GeometryType = gtWholeCell then
      begin
        // Layer, row, and column in Cell start at zero, not 1.
        // (Incremented values are exported.)
        FReachList[CellIndex].FReachData.Cell.Layer := 0;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.UpdateLatInflowDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SwrPackage: TSwrPackage;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the lateral infow data used in the package.
    EvaluateLatInflow;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    SwrPackage := Model.ModflowPackages.SwrPackage;
    UpdateADisplayList(TimeLists, FLatInflowValues,
      SwrPackage.LateralInflowSpecification, SwrPackage.LatInflowAssignmentMethod);
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.EvaluateEvaporation;
var
  EvapBoundary: TSwrEvapBoundary;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ErrorRoot: string;
begin
  ErrorRoot := Format(StrNoEvapBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    frmProgressMM.AddMessage(StrEvaluatingEvapSWRPacka);
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      EvapBoundary := ScreenObject.ModflowSwrEvap;
      if EvapBoundary <> nil then
      begin
        if (FSwrPackage.EvapSpecification = smObject)
          and (ScreenObject.ModflowSwrReaches = nil) then
        begin
          Continue;
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, ErrorRoot, ScreenObject.Name,
            ScreenObject);
        end;
        EvapBoundary.GetCellValues(FEvapValues, nil, Model, self);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.EvaluateLatInflow;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ErrorRoot: string;
  ABoundary: TSwrLatInflowBoundary;
begin
  ErrorRoot := Format(StrNoLatInfloBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    frmProgressMM.AddMessage(StrEvaluatingLatInflSWRPacka);
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.ModflowSwrLatInflow;
      if ABoundary <> nil then
      begin
        if (FSwrPackage.LateralInflowSpecification = smObject)
          and (ScreenObject.ModflowSwrReaches = nil) then
        begin
          Continue;
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, ErrorRoot, ScreenObject.Name,
            ScreenObject);
        end;
        ABoundary.GetCellValues(FLatInflowValues, nil, Model, self);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.EvaluateRain;
var
  ScreenObjectIndex: Integer;
  RainBoundary: TSwrRainBoundary;
  ScreenObject: TScreenObject;
  ErrorRoot: string;
begin
  ErrorRoot := Format(StrNoRainBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    frmProgressMM.AddMessage(StrEvaluatingRainSWRPacka);
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      RainBoundary := ScreenObject.ModflowSwrRain;
      if RainBoundary <> nil then
      begin
        if (FSwrPackage.RainSpecification = smObject)
          and (ScreenObject.ModflowSwrReaches = nil) then
        begin
          Continue;
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, ErrorRoot, ScreenObject.Name,
            ScreenObject);
        end;
        RainBoundary.GetCellValues(Values, nil, Model, self);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.EvaluateTransientReachData;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSwrReachBoundary;
  NoAssignmentErrorRoot: string;
begin
  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSwrReaches;
    if Boundary <> nil then
    begin
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
          ScreenObject.Name, ScreenObject);
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingS,
        [ScreenObject.Name]));
      Boundary.GetCellValues(FTransientReachList, nil, Model, self);
    end;
  end;
end;

procedure TModflowSwrWriter.EvaluateStage;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ErrorRoot: string;
  ABoundary: TSwrStageBoundary;
begin
  ErrorRoot := Format(StrNoStageBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    if FSwrPackage.StageSpecification = smArray then
    begin
      frmProgressMM.AddMessage(StrEvaluatingStageSWRPacka);
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        ABoundary := ScreenObject.ModflowSwrStage;
        if ABoundary <> nil then
        begin
          frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
          if not ScreenObject.SetValuesOfEnclosedCells
            and not ScreenObject.SetValuesOfIntersectedCells then
          begin
            frmErrorsAndWarnings.AddError(Model, ErrorRoot, ScreenObject.Name,
              ScreenObject);
          end;
          ABoundary.GetCellValues(FStageValues, nil, Model, self);
        end;
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.EvaluateSteadyReachData;
var
  NoAssignmentErrorRoot: string;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSwrReachBoundary;
  DataToCompile: TModflowDataObject;
  Compiler: TRbwParser;
  DataSetFunction: string;
  ReachLengthComment: string;
  ReachLengthExpression: TExpression;
  ReachNumberExpression: TExpression;
  ReachNumberComment: string;
  Grid: TCustomModelGrid;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ALocation: TCellAssignment;
  AReach: TSwrReachRecord;
  ReachIndex: Integer;
  ObservationExpression: TExpression;
  Variables: TStringList;
  VarIndex: Integer;
  VarName: string;
  DataArrayManager: TDataArrayManager;
  VariablePositions: array of Integer;
  DataSetIndexes: array of Integer;
  VarPosition: Integer;
  DataSetIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LayerToUse: Integer;
  RowToUse: Integer;
  ColToUse: Integer;
  Variable: TCustomValue;
  ADataSet: TDataArray;
  ReachObject: TReachObject;
  NeighborReaches: TList<TReachObject>;
  PriorReach: TReachObject;
  NextReach: TReachObject;
  NeighborReachArray: array[-1..1] of array[-1..1] of TReachObject;
  InnerReachIndex: Integer;
  CenterReach: TReachObject;
  Neighbor: TReachObject;
  OuterReachIndex: Integer;
  ObsFormula: string;
  ReachNumberFormula: string;
  ReachLists: TStringList;
  ReachObsNumberComment: string;
//  UsedFunction: string;
//  UsedExpression: TExpression;
begin
  ObsFormula := 'Round(VertexValue("TabFile", 0.0))';
  ReachNumberFormula := 'Round(VertexValue("' + KReachString + '", 0.0))';
  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);

  Grid := Model.Grid;
  FReachList.Clear;
  CellList:= TCellAssignmentList.Create;
  ReachLists := TStringList.Create;
  Variables := TStringList.Create;
  try
    ReachLists.OwnsObjects := True;
    Variables.Duplicates := dupIgnore;
    Variables.Sorted := True;
    DataArrayManager := Model.DataArrayManager;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowSwrReaches;
      if Boundary <> nil then
      begin
        FScreenObjectList.Add(ScreenObject);
        // Initialize ReachLengthExpression, ReachNumberExpression,
        // and ObservationExpression
        DataToCompile := TModflowDataObject.Create;
        try
          DataToCompile.Compiler := Model.GetCompiler(dsoTop, eaBlocks);
          DataToCompile.DataSetFunction := Boundary.ReachLengthFormula;
          DataToCompile.AlternateName := 'Reach Length';
          DataToCompile.AlternateDataType := rdtDouble;
          (ScreenObject.Delegate as TModflowDelegate).InitializeExpression(
            Compiler, DataSetFunction,
            ReachLengthExpression, nil,
             DataToCompile, Model);
          ReachLengthComment := Format(Str0sIn1s,
            [Boundary.ReachLengthFormula, ScreenObject.Name]);

          DataToCompile.Compiler := Model.GetCompiler(dsoTop, eaBlocks);
          DataToCompile.DataSetFunction := ReachNumberFormula;
          DataToCompile.AlternateName := 'Reach Number';
          DataToCompile.AlternateDataType := rdtInteger;
          (ScreenObject.Delegate as TModflowDelegate).InitializeExpression(
            Compiler, DataSetFunction, ReachNumberExpression, nil,
             DataToCompile, Model);
          ReachNumberComment := Format(Str0sIn1s,
            [ReachNumberFormula, ScreenObject.Name]);


          DataToCompile.Compiler := Model.GetCompiler(dsoTop, eaBlocks);
          DataToCompile.DataSetFunction := ObsFormula;
          DataToCompile.AlternateName := 'Observation Vertex';
          DataToCompile.AlternateDataType := rdtInteger;
          (ScreenObject.Delegate as TModflowDelegate).InitializeExpression(
            Compiler, DataSetFunction,
            ObservationExpression, nil,
             DataToCompile, Model);
          ReachObsNumberComment := Format(Str0sIn1s,
            [ObsFormula, ScreenObject.Name]);

        finally
          DataToCompile.Free;
        end;

        Variables.Assign(ReachLengthExpression.VariablesUsed);
        for VarIndex := 0 to Variables.Count - 1 do
        begin
          VarName := Variables[VarIndex];
          ADataSet := DataArrayManager.GetDataSetByName(VarName);
          if ADataSet <> nil then
          begin
            ADataSet.Initialize;
            DataArrayManager.AddDataSetToCache(ADataSet);
          end;
        end;

        SetLength(VariablePositions, Variables.Count);
        SetLength(DataSetIndexes, Variables.Count);
        for VarIndex := 0 to Variables.Count - 1 do
        begin
          VarName := Variables[VarIndex];
          VarPosition := Compiler.IndexOfVariable(VarName);
          VariablePositions[VarIndex] := VarPosition;
          if VarPosition >= 0 then
          begin
            DataSetIndex := DataArrayManager.IndexOfDataSet(VarName);
            DataSetIndexes[VarIndex] := DataSetIndex;
          end
          else
          begin
            DataSetIndexes[VarIndex] := -1;
          end;
        end;

        NeighborReaches := TList<TReachObject>.Create;
        ReachLists.AddObject(ScreenObject.Name, NeighborReaches);
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Grid,} '0', nil, nil, CellList, alAll, Model);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ALocation := CellList[CellIndex];

          ColIndex := ALocation.Column;
          RowIndex := ALocation.Row;
          LayerIndex := ALocation.Layer;
          UpdateCurrentScreenObject(ScreenObject);
          UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex,
            eaBlocks, Model);
          UpdateCurrentSegment(ALocation.Segment);
          UpdateCurrentSection(ALocation.Section);

          for VarIndex := 0 to Variables.Count - 1 do
          begin
            VarName := Variables[VarIndex];
            VarPosition := VariablePositions[VarIndex];
            if VarPosition >= 0 then
            begin
              Variable := Compiler.Variables[VarPosition];
              DataSetIndex := DataSetIndexes[VarIndex];
              if DataSetIndex >= 0 then
              begin
                ADataSet :=
                  DataArrayManager.DataSets[DataSetIndex];
                Assert(Model = ADataSet.Model);
                Assert(ADataSet.DataType = Variable.ResultType);
                if ADataSet.Orientation = dsoTop then
                begin
                  LayerToUse := 0;
                end
                else
                begin
                  LayerToUse := LayerIndex;
                end;
                if ADataSet.Orientation = dsoFront then
                begin
                  RowToUse := 0;
                end
                else
                begin
                  RowToUse := RowIndex;
                end;
                if ADataSet.Orientation = dsoSide then
                begin
                  ColToUse := 0;
                end
                else
                begin
                  ColToUse := ColIndex;
                end;

                case Variable.ResultType of
                  rdtDouble:
                    begin
                      TRealVariable(Variable).Value :=
                        ADataSet.RealData[LayerToUse, RowToUse,
                        ColToUse];
                    end;
                  rdtInteger:
                    begin
                      TIntegerVariable(Variable).Value :=
                        ADataSet.IntegerData[LayerToUse, RowToUse,
                        ColToUse];
                    end;
                  rdtBoolean:
                    begin
                      TBooleanVariable(Variable).Value :=
                        ADataSet.BooleanData[LayerToUse, RowToUse,
                        ColToUse];
                    end;
                  rdtString:
                    begin
                      TStringVariable(Variable).Value :=
                        ADataSet.StringData[LayerToUse, RowToUse,
                        ColToUse];
                    end;
                else
                  Assert(False);
                end;
              end;
            end;
          end;

          AReach.Cell.Layer := ALocation.Layer;
          AReach.Cell.Row := ALocation.Row;
          AReach.Cell.Column := ALocation.Column;
          AReach.Cell.Section := ALocation.Section;

          UpdateCurrentScreenObject(ScreenObject);
          UpdateCurrentSegment(ALocation.Segment);
          UpdateGlobalLocations(AReach.Cell.Column, AReach.Cell.Row,
            AReach.Cell.Layer, eaBlocks, Model);
          UpdateCurrentSection(AReach.Cell.Section);

          ReachNumberExpression.Evaluate;
          AReach.Reach := ReachNumberExpression.IntegerResult;
          AReach.ReachAnnotation := ReachNumberComment;

          UpdateCurrentScreenObject(ScreenObject);
          UpdateCurrentSegment(ALocation.Segment);
          UpdateGlobalLocations(AReach.Cell.Column, AReach.Cell.Row,
            AReach.Cell.Layer, eaBlocks, Model);
          UpdateCurrentSection(AReach.Cell.Section);

          ReachLengthExpression.Evaluate;
          AReach.ReachLength := ReachLengthExpression.DoubleResult;
          AReach.ReachLengthAnnotation := ReachLengthComment;

          UpdateCurrentScreenObject(ScreenObject);
          UpdateCurrentSegment(ALocation.Segment);
          UpdateGlobalLocations(AReach.Cell.Column, AReach.Cell.Row,
            AReach.Cell.Layer, eaBlocks, Model);
          UpdateCurrentSection(AReach.Cell.Section);

          ObservationExpression.Evaluate;
          AReach.TabLocation := ObservationExpression.IntegerResult;

          AReach.ObjectName := ScreenObject.Name;
          AReach.ScreenObject := ScreenObject;
          AReach.RouteType := Boundary.RouteType;
          AReach.MultiLayer := Boundary.MultiLayer;
          AReach.Grouped := Boundary.Grouped;
          AReach.ReachGroup := Boundary.GroupNumber;
          AReach.ObsTypes := Boundary.ObsTypes;

          ReachObject := TReachObject.Create;
          ReachObject.FReachData := AReach;

          FReachList.Add(ReachObject);
          NeighborReaches.Add(ReachObject);
        end;

        DataArrayManager.CacheDataArrays;

        if NeighborReaches.Count > 1 then
        begin
          if ScreenObject.SetValuesOfEnclosedCells and ScreenObject.Closed then
          begin
            for OuterReachIndex := 0 to NeighborReaches.Count - 1 do
            begin
              CenterReach := NeighborReaches[OuterReachIndex];

              for RowIndex := -1 to 1 do
              begin
                for ColIndex := -1 to 1 do
                begin
                  NeighborReachArray[RowIndex,ColIndex] := nil;
                end;
              end;

              NeighborReachArray[0,0] := CenterReach;

              for InnerReachIndex := 0 to NeighborReaches.Count - 1 do
              begin
                if InnerReachIndex = OuterReachIndex then
                begin
                  Continue;
                end;
                Neighbor := NeighborReaches[InnerReachIndex];
                ColIndex := CenterReach.FReachData.Cell.Column
                  - Neighbor.FReachData.Cell.Column;
                RowIndex := CenterReach.FReachData.Cell.Row
                  - Neighbor.FReachData.Cell.Row;
                if (ColIndex+1 in [0..2]) and (RowIndex+1 in [0..2]) then
                begin
                  NeighborReachArray[ColIndex,RowIndex] := Neighbor;
                end;
              end;

              Neighbor := NeighborReachArray[-1,0];
              if Neighbor <> nil then
              begin
                if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                begin
                  CenterReach.Neighbors.Add(Neighbor);
                  Neighbor.Neighbors.Add(CenterReach);
                end;
                end;

              Neighbor := NeighborReachArray[1,0];
              if Neighbor <> nil then
              begin
                if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                begin
                  CenterReach.Neighbors.Add(Neighbor);
                  Neighbor.Neighbors.Add(CenterReach);
                end;
              end;

              Neighbor := NeighborReachArray[0,-1];
              if Neighbor <> nil then
              begin
                if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                begin
                  CenterReach.Neighbors.Add(Neighbor);
                  Neighbor.Neighbors.Add(CenterReach);
                end;
              end;

              Neighbor := NeighborReachArray[0,1];
              if Neighbor <> nil then
              begin
                if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                begin
                  CenterReach.Neighbors.Add(Neighbor);
                  Neighbor.Neighbors.Add(CenterReach);
                end;
              end;

              if (NeighborReachArray[-1,0] = nil)
                and (NeighborReachArray[0,-1] = nil) then
              begin
                Neighbor := NeighborReachArray[-1,-1];
                if Neighbor <> nil then
                begin
                  if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                  begin
                    CenterReach.Neighbors.Add(Neighbor);
                    Neighbor.Neighbors.Add(CenterReach);
                  end;
                end;
              end;

              if (NeighborReachArray[1,0] = nil)
                and (NeighborReachArray[0,-1] = nil) then
              begin
                Neighbor := NeighborReachArray[1,-1];
                if Neighbor <> nil then
                begin
                  if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                  begin
                    CenterReach.Neighbors.Add(Neighbor);
                    Neighbor.Neighbors.Add(CenterReach);
                  end;
                end;
              end;

              if (NeighborReachArray[1,0] = nil)
                and (NeighborReachArray[0,1] = nil) then
              begin
                Neighbor := NeighborReachArray[1,1];
                if Neighbor <> nil then
                begin
                  if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                  begin
                    CenterReach.Neighbors.Add(Neighbor);
                    Neighbor.Neighbors.Add(CenterReach);
                  end;
                  end;
              end;

              if (NeighborReachArray[-1,0] = nil)
                and (NeighborReachArray[0,1] = nil) then
              begin
                Neighbor := NeighborReachArray[-1,1];
                if Neighbor <> nil then
                begin
                  if CenterReach.Neighbors.IndexOf(Neighbor) < 0 then
                  begin
                    CenterReach.Neighbors.Add(Neighbor);
                    Neighbor.Neighbors.Add(CenterReach);
                  end;
                  end;
              end;
            end;
          end
          else
          begin
            PriorReach := NeighborReaches[0];
            for ReachIndex := 1 to NeighborReaches.Count - 1 do
            begin
              NextReach := NeighborReaches[ReachIndex];
              PriorReach.Neighbors.Add(NextReach);
              NextReach.Neighbors.Add(PriorReach);
              PriorReach := NextReach;
            end;
          end;
        end;
      end;
    end;
    AssignFinalReachNumbers;
    EvaluateInterObjectConnections(ReachLists);
    EvaluateStructures;
    AssignReachObservationLinks;
  finally
    CellList.Free;
    Variables.Free;
    ReachLists.Free;
  end;
end;

procedure TModflowSwrWriter.EvaluateStructures;
var
  StructureIndex: Integer;
  AStructure: TStructure;
  StructureList: TStructureList;
begin
  for StructureIndex := 0 to Model.SwrStructures.Count - 1 do
  begin
    AStructure := Model.SwrStructures[StructureIndex];
    if not FStuctureDict.TryGetValue(AStructure.Reach, StructureList) then
    begin
      StructureList := TStructureList.Create;
      FStuctureDict.Add(AStructure.Reach, StructureList);
    end;
    StructureList.Add(AStructure);
  end;
end;

procedure TModflowSwrWriter.UpdateRainDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SwrPackage: TSwrPackage;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the rain data used in the package.
    EvaluateRain;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    SwrPackage := Model.ModflowPackages.SwrPackage;
    UpdateADisplayList(TimeLists, Values,
      SwrPackage.RainSpecification, SwrPackage.RainAssignmentMethod);

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateReachNumberDisplay;
var
  SwrReachArray: TModflowBoundaryDisplayDataArray;
  SwrReachGroupArray: TModflowBoundaryDisplayDataArray;
  SwrRoutingTypeArray: TModflowBoundaryDisplayDataArray;
  SwrReachLengthArray: TModflowBoundaryDisplayDataArray;
  ReachIndex: integer;
  AReach: TReachObject;
  NewAnnotation, Annotation: string;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  SwrReachArray := Model.DataArrayManager.GetDataSetByName(KSwrReach)
    as TModflowBoundaryDisplayDataArray;
  SwrReachGroupArray := Model.DataArrayManager.GetDataSetByName(KSwrReachGroup)
    as TModflowBoundaryDisplayDataArray;
  SwrRoutingTypeArray := Model.DataArrayManager.GetDataSetByName(KSwrRoutingType)
    as TModflowBoundaryDisplayDataArray;
  SwrReachLengthArray := Model.DataArrayManager.GetDataSetByName(KSwrReachLength)
    as TModflowBoundaryDisplayDataArray;
  if (SwrReachArray <> nil) or (SwrReachGroupArray <> nil)
    or (SwrRoutingTypeArray <> nil) or (SwrReachLengthArray <> nil) then
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRGeometryNotDef);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSWRReachNumberToo);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrIllegalDuplicateSW);
    EvaluateSteadyReachData;
  end;
  if SwrReachArray <> nil then
  begin
    SwrReachArray.Clear;
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReach := FReachList[ReachIndex];
      SwrReachArray.AddDataValue(AReach.FReachData.ReachAnnotation,
        AReach.FReachData.Reach, AReach.FReachData.Cell.Column,
        AReach.FReachData.Cell.Row, AReach.FReachData.Cell.Layer);
    end;
    SwrReachArray.ComputeAverage;
    SwrReachArray.UpToDate := True;
  end;
  if SwrReachGroupArray <> nil then
  begin
    SwrReachGroupArray.Clear;
    Annotation := '';
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReach := FReachList[ReachIndex];
      if AReach.FReachData.Grouped then
      begin
        NewAnnotation := Format('Value assigned by object %s',
          [AReach.FReachData.ObjectName]);
      end
      else
      begin
        NewAnnotation := Format('Automatically assigned: Object = %s',
          [AReach.FReachData.ObjectName]);
      end;

      // reduce memory usage through reference counting.
      if Annotation <> NewAnnotation then
      begin
        Annotation := NewAnnotation;
      end;
      SwrReachGroupArray.AddDataValue(Annotation,
        AReach.FReachData.ReachGroup, AReach.FReachData.Cell.Column,
        AReach.FReachData.Cell.Row, AReach.FReachData.Cell.Layer);
    end;
    SwrReachGroupArray.ComputeAverage;
    SwrReachGroupArray.UpToDate := True;
  end;
  if SwrRoutingTypeArray <> nil then
  begin
    SwrRoutingTypeArray.Clear;
    Annotation := '';
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReach := FReachList[ReachIndex];
      NewAnnotation := Format('Value assigned by object %s',
        [AReach.FReachData.ObjectName]);
      case AReach.FReachData.RouteType of
        rtLevelPool: NewAnnotation := NewAnnotation + ' (level pool)';
        rtTiltedPool: NewAnnotation := NewAnnotation + ' (tilted pool)';
        rtDiffusiveWave: NewAnnotation := NewAnnotation + ' (diffusive wave)';
      end;

      // reduce memory usage through reference counting.
      if Annotation <> NewAnnotation then
      begin
        Annotation := NewAnnotation;
      end;
      SwrRoutingTypeArray.AddDataValue(Annotation,
        Ord(AReach.FReachData.RouteType)+1, AReach.FReachData.Cell.Column,
        AReach.FReachData.Cell.Row, AReach.FReachData.Cell.Layer);
    end;
    SwrRoutingTypeArray.ComputeAverage;
    SwrRoutingTypeArray.UpToDate := True;
  end;
  if SwrReachLengthArray <> nil then
  begin
    SwrReachLengthArray.Clear;
    Annotation := '';
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReach := FReachList[ReachIndex];

      // reduce memory usage through reference counting.
      if Annotation <> AReach.FReachData.ReachLengthAnnotation then
      begin
        Annotation := AReach.FReachData.ReachLengthAnnotation;
      end;
      SwrReachLengthArray.AddDataValue(Annotation,
        AReach.FReachData.ReachLength, AReach.FReachData.Cell.Column,
        AReach.FReachData.Cell.Row, AReach.FReachData.Cell.Layer);
    end;
    SwrReachLengthArray.ComputeAverage;
    SwrReachLengthArray.UpToDate := True;
  end;
  Model.SwrReachConnectionsPlot.UpdateReaches(Self);

end;

procedure TModflowSwrWriter.UpdateStageDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the stage data used in the package.
    if FSwrPackage.StageSpecification = smArray then
    begin
      EvaluateStage;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      UpdateADisplayList(TimeLists, FStageValues,
        FSwrPackage.StageSpecification, FSwrPackage.StageAssignmentMethod);
    end
    else
    begin
      EvaluateTransientReachData;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      UpdateADisplayList(TimeLists, FTransientReachList,
        FSwrPackage.StageSpecification, FSwrPackage.StageAssignmentMethod, 1);
    end;
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateVerticalOffsetDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the vertical offset data used in the package.
    EvaluateTransientReachData;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    UpdateADisplayList(TimeLists, FTransientReachList,
      smObject, umAdd, 0);
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateADisplayList(
  TimeLists: TModflowBoundListOfTimeLists; ValueLists: TList;
  Method: TSwrSpecificationMethod; UpdateMethod: TUpdateMethod;
  DataTypeIndex: Integer = 0);
var
  TimeIndex: Integer;
  CellList: TValueCellList;
  DataArrayList: TList;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  DummyIndex: Integer;
begin
  Assert(TimeLists.Count = 1);
  TimeList := TimeLists[0];
  ClearTimeLists(Model);
  DataArrayList := TList.Create;
  try
    // ValueLists contains lists of cells related to rain for
    // each stress period.
    if (ValueLists.Count <> 0) or (TimeList.Count = 0) then
    begin
      Assert(ValueLists.Count = TimeList.Count);
    end;
    // For each stress period, transfer ValueLists from
    // the cells lists to the data arrays.
    for TimeIndex := 0 to ValueLists.Count - 1 do
    begin
      CellList := ValueLists[TimeIndex];
      if CellList.Count > 0 then
      begin
        DataArrayList.Clear;
        DataArray := TimeList[TimeIndex] as TModflowBoundaryDisplayDataArray;
        case Method of
          smObject:
            begin
              for DummyIndex := 0 to DataTypeIndex - 1 do
              begin
                DataArrayList.Add(nil);
              end;
              DataArrayList.Add(DataArray);
              UpdateCellDisplay(CellList, DataArrayList, [], nil, [DataTypeIndex]);
            end;
          smArray:
            begin
              AssignTransient2DArray(DataArray, DataTypeIndex, CellList, 0,
                rdtdouble, UpdateMethod);
            end;
          else
            Assert(False);
        end;
      end;
    end;
    // Mark all the data arrays and time lists as up to date.
    for TimeIndex := 0 to TimeList.Count - 1 do
    begin
      DataArray := TimeList[TimeIndex] as TModflowBoundaryDisplayDataArray;
      DataArray.UpToDate := True;
    end;
    TimeList.SetUpToDate(True);
  finally
    DataArrayList.Free;
  end;
end;

procedure TModflowSwrWriter.UpdateBoundaryTypeDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the vertical offset data used in the package.
    EvaluateTransientReachData;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    UpdateADisplayList(TimeLists, FTransientReachList,
      smObject, umAdd, 3);
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.UpdateDirectRunoffDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  ListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  CellList: TValueCellList;
  DataArray: TModflowBoundaryDisplayDataArray;
  ReachList: TModflowBoundaryDisplayTimeList;
  ValueList: TModflowBoundaryDisplayTimeList;
  ReachArray: TModflowBoundaryDisplayDataArray;
  ValueArray: TModflowBoundaryDisplayDataArray;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    // evaluate the direct runoff data used in the package.
    EvaluateDirectRunoff;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    ClearTimeLists(Model);

    Assert(TimeLists.Count = 2);

    for ListIndex := 0 to TimeLists.Count - 1 do
    begin
      DisplayTimeList := TimeLists[ListIndex];
      // FDirectRunoffValues contains lists of cells related to evaporation for
      // each stress period.
      if (FDirectRunoffValues.Count <> 0) or (DisplayTimeList.Count = 0) then
      begin
        Assert(FDirectRunoffValues.Count = DisplayTimeList.Count);
      end;
    end;

    ReachList := TimeLists[0];
    ValueList := TimeLists[1];
    // For each stress period, transfer values from
    // the cells lists to the data arrays.
    for TimeIndex := 0 to FDirectRunoffValues.Count - 1 do
    begin
      CellList := FDirectRunoffValues[TimeIndex];
      if CellList.Count > 0 then
      begin
        ReachArray := ReachList[TimeIndex] as TModflowBoundaryDisplayDataArray;
        ValueArray := ValueList[TimeIndex] as TModflowBoundaryDisplayDataArray;
        AssignTransient2DArray(
          ReachArray, 0, CellList, 0, rdtInteger, umAssign);
        AssignTransient2DArray(
          ValueArray, 1, CellList, 0, rdtDouble, umAssign);
      end;
    end;

    // Mark all the data arrays and time lists as up to date.
    for ListIndex := 0 to TimeLists.Count - 1 do
    begin
      DisplayTimeList := TimeLists[ListIndex];
      for TimeIndex := 0 to DisplayTimeList.Count - 1 do
      begin
        DataArray := DisplayTimeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.UpToDate := True;
      end;
      DisplayTimeList.SetUpToDate(True);
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowSwrWriter.WriteSwrTransientList(CellList: TList; Comment: string);
var
  ValueCellList: TValueCellList;
  ACell: TSwrValueCell;
  index: Integer;
  AReach: TReachObject;
begin
  ValueCellList := CellList as TValueCellList;
  if ValueCellList.Count <> FReachList.Count then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
      StrSomeObjectsThatDe);
    Exit;
  end;
  Assert(ValueCellList.Count = FReachList.Count);
  for index := 0 to ValueCellList.Count - 1 do
  begin
    ACell := ValueCellList[index] as TSwrValueCell;
    AReach := FReachList[index];
    // This is being done for all reaches even inactive ones.
    WriteInteger(AReach.FReachData.Reach);
    WriteFloat(ACell.SwrValue);
    WriteString(Comment);
    NewLine;
  end;
end;

procedure TModflowSwrWriter.WriteTransientSwrArray(CellList: TList;
  AssignmentMethod: TUpdateMethod; Comment: string; const MF6_ArrayName: string);
var
  DefaultValue: Double;
  DataTypeIndex: Integer;
  DataType: TRbwDataType;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, AssignmentMethod, True, Dummy, MF6_ArrayName);
end;

procedure TModflowSwrWriter.WriteDataSet5(TimeIndex: Integer);
var
  IPTFLG: Integer;
  IRDAUX: Integer;
  CellIndex: Integer;
  IRDRAI: Integer;
  IRDSTR: Integer;
  IRDLIN: Integer;
  IRDBND: Integer;
  IRDGEO: Integer;
  IRDEVP: Integer;
  TransientCell: TSwrTransientCell;
  ValueCellList: TValueCellList;
  IRDSTG: Integer;
  ITMP: Integer;
  Reach: TReachObject;
  PriorValueCellList: TValueCellList;
  PrioerTransientCell: TSwrTransientCell;
begin
  // Data Set 5
  ITMP := 1;

  if TimeIndex = 0 then
  begin
    IRDBND := FReachList.Count;
  end
  else
  begin
    IRDBND := 0;
    ValueCellList := FTransientReachList[TimeIndex];
    PriorValueCellList := FTransientReachList[TimeIndex-1];
    Assert(ValueCellList.Count = PriorValueCellList.Count);
    for CellIndex := 0 to ValueCellList.Count - 1 do
    begin
      TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
      PrioerTransientCell := PriorValueCellList[CellIndex] as TSwrTransientCell;
      if (TransientCell.ReachType = srtSpecifiedHead)
        or (PrioerTransientCell.ReachType = srtSpecifiedHead)
        or (TransientCell.ReachType <> PrioerTransientCell.ReachType) then
      begin
        Inc(IRDBND);
      end;
    end;
  end;


  IRDRAI := 0;
  if Values.Count > 0 then
  begin
    case FSwrPackage.RainSpecification of
      smObject:
        IRDRAI := FReachList.Count;
      smArray:
        IRDRAI := -1;
    else
      Assert(False);
    end;
  end;
  IRDEVP := 0;
  if FEvapValues.Count > 0 then
  begin
    case FSwrPackage.EvapSpecification of
      smObject:
        IRDEVP := FReachList.Count;
      smArray:
        IRDEVP := -1;
    else
      Assert(False);
    end;
  end;
  IRDLIN := 0;
  if FLatInflowValues.Count > 0 then
  begin
    case FSwrPackage.LateralInflowSpecification of
      smObject:
        IRDLIN := FReachList.Count;
      smArray:
        IRDLIN := -1;
    else
      Assert(False);
    end;
  end;

  IRDGEO := FReachList.Count;

  IRDSTR := FStuctureDict.Count;

  IRDSTG := 0;
  case FSwrPackage.StageSpecification of
    smObject:
      begin
        ValueCellList := FTransientReachList[TimeIndex];
        if ValueCellList.Count <> FReachList.Count then
        begin
          frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
            Format(StrInStressPeriodD, [TimeIndex +1]));
          Exit;
        end;
        Assert(ValueCellList.Count = FReachList.Count);
        for CellIndex := 0 to ValueCellList.Count - 1 do
        begin
          TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
          Reach := FReachList[CellIndex];
          if (TimeIndex = 0)
            or (TransientCell.ReachType = srtSpecifiedHead)
            or (Reach.FReachData.RouteType in [rtLevelPool, rtTiltedPool])
            then
          begin
            Inc(IRDSTG);
          end;
        end;

//        IRDSTG := IRDBND;
//        if TimeIndex = 0 then
//        begin
//          IRDSTG := FReachList.Count;
//        end
//        else
//        begin
//          IRDSTG := 0;
//          ValueCellList := FTransientReachList[TimeIndex];
//          for CellIndex := 0 to ValueCellList.Count - 1 do
//          begin
//            TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
//            if (TransientCell.ReachType = srtSpecifiedHead) then
//            begin
//              Inc(IRDSTG);
//            end;
//          end;
//        end;
        if (TimeIndex = 0) and (IRDSTG <> NREACHES) then
        begin
          frmErrorsAndWarnings.AddError(Model, StrInitialSWRStagesN,
            Format(StrYouMustSpecifyTheFormat, [NREACHES, IRDSTG]));
        end;
      end;
    smArray:
      begin
        if FStageValues.Count > 0 then
        begin
          IRDSTG := -1;
        end
        else
        begin
          IRDSTG := 0;
          if (TimeIndex = 0) then
          begin
          frmErrorsAndWarnings.AddError(Model, StrInitialSWRStagesN,
            StrYouMustSpecifyThe);
          end;
        end;
      end;
  else
    Assert(False);
  end;
  if Model.ModflowOutputControl.PrintInputArrays
    or Model.ModflowOutputControl.PrintInputCellLists then
  begin
    IPTFLG := 1;
  end
  else
  begin
    IPTFLG := 0;
  end;
  if TimeIndex = 0 then
  begin
    IRDAUX := FReachList.Count;
  end
  else
  begin
    IRDAUX := -1;
  end;

  WriteInteger(ITMP);
  WriteInteger(IRDBND);
  WriteInteger(IRDRAI);
  WriteInteger(IRDEVP);
  WriteInteger(IRDLIN);
  WriteInteger(IRDGEO);
  WriteInteger(IRDSTR);
  WriteInteger(IRDSTG);
  WriteInteger(IPTFLG);
  WriteInteger(IRDAUX);
  WriteString(' # Data Set 5, Stress Period '
    + IntToStr(TimeIndex+1)
    + ': ITMP IRDBND IRDRAI IRDEVP IRDLIN IRDGEO IRDSTR IRDSTG IPTFLG IRDAUX');
  NewLine;

end;

procedure TModflowSwrWriter.WriteDataSet14(TimeIndex: Integer);
var
  CellList: TList;
begin
  // Data set 8
  case FSwrPackage.StageSpecification of
    smObject:
      begin
        WriteDataSet14a(TimeIndex);
      end;
    smArray:
      begin
        if FStageValues.Count > 0 then
        begin
          CellList := FStageValues[TimeIndex];
          WriteStageArray(CellList);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TModflowSwrWriter.WriteDataSet14A(TimeIndex: Integer);
var
  CellIndex: Integer;
  Reach: TReachObject;
  ValueCellList: TValueCellList;
  TransientCell: TSwrTransientCell;
begin
  // Data Set 14A
  ValueCellList := FTransientReachList[TimeIndex];
  if ValueCellList.Count <> FReachList.Count then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
      Format(StrInStressPeriodD, [TimeIndex +1]));
    Exit;
  end;
  Assert(ValueCellList.Count = FReachList.Count);
  for CellIndex := 0 to ValueCellList.Count - 1 do
  begin
    TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
    Reach := FReachList[CellIndex];
    if (TimeIndex = 0)
      or (TransientCell.ReachType = srtSpecifiedHead)
      or (Reach.FReachData.RouteType in [rtLevelPool, rtTiltedPool])
      then
    begin
      WriteInteger(Reach.FReachData.Reach);
      WriteValueOrFormula(TransientCell, SwrStagePosition);
//      WriteFloat(TransientCell.Stage);
      WriteString(' # Data Set 14A: IRCHSTG STAGE');
      NewLine;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet15(TimeIndex: Integer);
var
  Reach: TReachObject;
  IRCHAUX: Integer;
  ValueCellList: TValueCellList;
  CellIndex: Integer;
  TransientCell: TSwrTransientCell;
begin
  if TimeIndex = 0 then
  begin
    ValueCellList := FTransientReachList[TimeIndex];
    if ValueCellList.Count <> FReachList.Count then
    begin
      frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
        Format(StrInStressPeriodD, [TimeIndex +1]));
      Exit;
    end;
    Assert(ValueCellList.Count = FReachList.Count);
    for CellIndex := 0 to ValueCellList.Count - 1 do
    begin
      Reach := FReachList[CellIndex];
      IRCHAUX := Reach.FReachData.Reach;
      WriteInteger(IRCHAUX);

      TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
      WriteIface(TransientCell.IFace);

      WriteString(' # Data Set 15: IRCHAUX, IFACE');
      NewLine;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet11(TimeIndex: Integer);
begin
  // Data Set 11;
  WriteGeometry;
end;

procedure TModflowSwrWriter.WriteDataSet12(TimeIndex: Integer);
var
  StuctureArray: TArray<TPair<Integer, TStructureList>>;
  ReachIndex: Integer;
  ISMODRCH: integer;
  NSTRUCT: integer;
  StructPair: TPair<Integer, TStructureList>;
  StructList: TStructureList;
//  StressPeriod: TModflowStressPeriod;
begin
//  StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
  StuctureArray := FStuctureDict.ToArray;
  for ReachIndex := 0 to Length(StuctureArray) - 1 do
  begin
    StructPair := StuctureArray[ReachIndex];
    ISMODRCH := StructPair.Key;
//    NSTRUCT := 0;
    StructList := StructPair.Value;
//    for StructureIndex := 0 to StructList.Count - 1 do
//    begin
//      AStructure := StructList[StructureIndex];
//      if AStructure.UsedInPeriod(StressPeriod.StartTime, StressPeriod.EndTime) then
//      begin
//        Inc(NSTRUCT);
//      end;
//    end;
    NSTRUCT := StructList.Count;
    WriteInteger(ISMODRCH);
    WriteInteger(NSTRUCT);
    WriteString(' # Data Set 12: ISMODRCH, NSTRUCT');
    NewLine;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet13(TimeIndex: Integer);
var
  StressPeriod: TModflowStressPeriod;
  StuctureArray: TArray<TPair<Integer, TStructureList>>;
  ReachIndex: Integer;
  StructPair: TPair<Integer, TStructureList>;
  StructList: TStructureList;
  StructureIndex: Integer;
  AStructure: TStructure;
  // Data Set 13A variables.
  ISTRRCH: integer;
  ISTRNUM: integer;
  ISTRCONN: integer;
  ISTRTYPE: integer;
  NSTRPTS: integer;
  STRCD: double;
  STRCD2: double;
  STRCD3: double;
  STRINV: double;
  STRINV2: double;
  STRWID: double;
  STRWID2: double;
  STRLEN: double;
  STRLEN2: double;
  STRMAN: double;
  STRVAL: double;
  ISTRDIR: integer;
  ISFRSEG: integer;
  ISFRRCH: integer;
  Comment: String;
  // Data set 13B variables
  CSTROTYP: string;
  ISTRORCH: integer;
  ISTROQCON: integer;
  CSTROLO: string;
  CSTRCRIT: string;
  STRCRITC: double;
  STRRT: double;
  STRMAX: double;
  CSTRVAL: string;
  STRCRIT: Double;
  ItemIndex: Integer;
  TableItem: TStructureDischargeItem;
  STRELEV: double;
  STRQ: double;
  TabNumber: integer;
  STRVAL_String: string;
  ReachIndex2: Integer;
  AReachObject: TReachObject;
  IRCH4B: Integer;
  NeighborIndex: Integer;
  ICONN: Integer;
  ConnectedReachIsConnected: Boolean;
  ReachExists: Boolean;
  STRWSMO: Double;
begin
  StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
  StuctureArray := FStuctureDict.ToArray;
  for ReachIndex := 0 to Length(StuctureArray) - 1 do
  begin
    StructPair := StuctureArray[ReachIndex];
    StructList := StructPair.Value;
    ISTRNUM := 0;
    for StructureIndex := 0 to StructList.Count - 1 do
    begin
      AStructure := StructList[StructureIndex];
      // Data Set 13A
      ISTRRCH := AStructure.Reach;

      Inc(ISTRNUM);

      ISTRCONN := AStructure.ConnectedReach;
      if AStructure.StructureType in [sstSfrInflow, sstManning] then
      begin
        ISTRCONN := 0;
      end;

      ISTRTYPE := -3;
      if AStructure.UsedInPeriod(StressPeriod.StartTime, StressPeriod.EndTime) then
      begin
        case AStructure.StructureType of
          sstUncontrolledZeroDepth: ISTRTYPE := -2;
          sstNone: ISTRTYPE := 0;
          sstSpecifiedElevation: ISTRTYPE := 1;
          sstUncontrolledCriticalDepth: ISTRTYPE := 2;
          sstPump: ISTRTYPE := 3;
          sstStageDishargeTable: ISTRTYPE := 4;
          sstCulvert: ISTRTYPE := 5;
          sstFixedWeir: ISTRTYPE := 6;
          sstFixedSpillway: ISTRTYPE := 7;
          sstMoveableWeir: ISTRTYPE := 8;
          sstGatedSpillway: ISTRTYPE := 9;
          sstSpillEquation: ISTRTYPE := 10;
          sstSfrInflow: ISTRTYPE := 11;
          sstManning: ISTRTYPE := 12;
          sstOverbankFlow: ISTRTYPE := 13;
          else Assert(False);
        end;
      end
      else
      begin
        ISTRTYPE := 0;
      end;

      WriteInteger(ISTRRCH);
      WriteInteger(ISTRNUM);
      WriteInteger(ISTRCONN);
      if ISTRCONN <> 0 then
      begin
        ReachExists := False;
        ConnectedReachIsConnected := False;
        AReachObject := nil;
        for ReachIndex2 := 0 to FReachList.Count - 1 do
        begin
          AReachObject := FReachList[ReachIndex2];
          IRCH4B := AReachObject.FReachData.Reach;
          if IRCH4B = ISTRRCH then
          begin
            ReachExists := True;
            for NeighborIndex := 0 to AReachObject.Neighbors.Count - 1 do
            begin
              ICONN := AReachObject.Neighbors[NeighborIndex].FReachData.Reach;
              if ICONN = ISTRCONN then
              begin
                ConnectedReachIsConnected := True;
                break;
              end;
            end;
            break;
          end;
        end;
        if not ReachExists then
        begin
          frmErrorsAndWarnings.AddError(Model, StrInvalidStructureDe,
            Format(StrStructureReachAbsent, [ISTRRCH]));
        end
        else if not ConnectedReachIsConnected then
        begin
          Assert(AReachObject <> nil);
          frmErrorsAndWarnings.AddError(Model, StrInvalidStructureCo,
            Format(StrStructureBadConnection, [ISTRRCH, ISTRCONN]),
            AReachObject.FReachData.ScreenObject);
        end;

      end;
      WriteInteger(ISTRTYPE);
      Comment := ' # Data Set 13: ISTRRCH ISTRNUM ISTRCONN ISTRTYPE';

      if ISTRTYPE = 4 then
      begin
        NSTRPTS := AStructure.Table.Count;
        WriteInteger(NSTRPTS);
        Comment := Comment + ' NSTRPTS';
      end;

      if ISTRTYPE in [5..10, 13] then
      begin
        STRCD := AStructure.WeirDischargeCoefficient;
        WriteFloatCondensed(STRCD);
        Comment := Comment + ' STRCD';
      end;

      if ISTRTYPE in [5,7,9,10] then
      begin
        STRCD2 := AStructure.OrificeDischargeCoefficient;
        WriteFloatCondensed(STRCD2);
        Comment := Comment + ' STRCD2';
      end;

      if ISTRTYPE in [6..10, 13] then
      begin
        STRCD3 := AStructure.SubmergenceExponent;
        WriteFloatCondensed(STRCD3);
        Comment := Comment + ' STRCD3';
      end;

      if (ISTRTYPE = -2) or (ISTRTYPE in [5..10]) then
      begin
        STRINV := AStructure.InvertElevation;
        WriteFloatCondensed(STRINV);
        Comment := Comment + ' STRINV';
      end;

      if ISTRTYPE = 5 then
      begin
        STRINV2 := AStructure.DownstreamInvertElevation;
        WriteFloatCondensed(STRINV2);
        Comment := Comment + ' STRINV2';
      end;

      if ISTRTYPE in [5..10] then
      begin
        STRWID := AStructure.Width;
        if (ISTRTYPE = 5) and (AStructure.CulvertType = ctRectangular) then
        begin
          STRWID := -STRWID;
        end;
        WriteFloatCondensed(STRWID);
        Comment := Comment + ' STRWID';

        if (ISTRTYPE = 5) and (STRWID < 0) then
        begin
          STRWID2 := AStructure.CulvertRise;
          WriteFloatCondensed(STRWID2);
          Comment := Comment + ' STRWID2';
        end;
      end;

      if (ISTRTYPE in [5, 12]) or (AStructure.SpecifyCulvertLengths
        and ((ISTRTYPE = 2) or (ISTRTYPE = -2))) then
      begin
        STRLEN := AStructure.CulvertLength;
        WriteFloatCondensed(STRLEN);
        Comment := Comment + ' STRLEN';
      end;

      if (not AStructure.SpecifyCulvertLengths
        and ((ISTRTYPE = 2) or (ISTRTYPE = -2))) then
      begin
        STRLEN := 0;
        WriteFloatCondensed(STRLEN);
        Comment := Comment + ' STRLEN';
      end;

      if (AStructure.SpecifyCulvertLengths
        and ((ISTRTYPE = 2) or (ISTRTYPE = -2))) then
      begin
        STRLEN2 := AStructure.DownstreamCulvertLength;
        WriteFloatCondensed(STRLEN2);
        Comment := Comment + ' STRLEN2';
      end;

      if (not AStructure.SpecifyCulvertLengths
        and ((ISTRTYPE = 2) or (ISTRTYPE = -2))) then
      begin
        if ISTRCONN > 0 then
        begin
          STRLEN2 := 0;
          WriteFloatCondensed(STRLEN2);
          Comment := Comment + ' STRLEN2';
        end;
      end;


      if (ISTRTYPE = 5) then
      begin
        STRMAN := AStructure.CulvertRoughness;
        WriteFloatCondensed(STRMAN);
        Comment := Comment + ' STRMAN';
      end;

      if ISTRTYPE = 13 then
      begin
        STRWSMO := AStructure.SmoothingValue;
        if AStructure.SmoothingMethod = smLinear then
        begin
          STRWSMO := -STRWSMO;
        end;
        WriteFloatCondensed(STRWSMO);
        Comment := Comment + ' STRWSMO';
      end;

      if ISTRTYPE in [3, 6, 7, 8, 9, 10, 12, 13] then
      begin
        if (ISTRTYPE = 12) and (AStructure.InitialFlowRateMethod = smTabFile) then
        begin
          TabNumber := Model.SwrTabFiles.IndexOfFileName(AStructure.FullInitialFlowRateTabFile);
          if TabNumber >= 0 then
          begin
            Inc(TabNumber);
            STRVAL_String := ' TABDATA' + IntToStr(TabNumber);
            WriteString(STRVAL_String);
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model, StrInvalidTabFileNam,
              Format(StrStructure0sTab, [AStructure.Name,
              AStructure.FullInitialFlowRateTabFile]));
          end;
        end
        else
        begin
          STRVAL := AStructure.InitialFlowRateOrGateOpening;
          WriteFloatCondensed(STRVAL);
        end;
        Comment := Comment + ' STRVAL';
      end;

      if ISTRTYPE in [5..10, 13] then
      begin
        ISTRDIR := Ord(AStructure.StructureRestrictions)-1;
        WriteInteger(ISTRDIR);
        Comment := Comment + ' ISTRDIR';
      end;

      if (ISTRCONN = 0) and Model.ModflowPackages.SfrPackage.IsSelected then
      begin
        ISFRSEG := AStructure.SfrSegment;
        if ISFRSEG > 0 then
        begin
          WriteInteger(ISFRSEG);

          ISFRRCH  := AStructure.SfrReach;
          WriteInteger(ISFRRCH);
          Comment := Comment + ' ISFRSEG ISFRRCH';
        end;
      end;

      WriteString(Comment);
      NewLine;

      // Data Set 13B
      Comment := '';
      if ISTRTYPE in [1, 3, 8, 9, 10] then
      begin
        CSTROTYP := '';
        case AStructure.ControlType of
          ctStage: CSTROTYP := 'STAGE ';
          ctFlow: CSTROTYP := 'FLOW ';
        end;
        WriteString(CSTROTYP);
        Comment := Comment + ' CSTROTYP';
      end;

      if ISTRTYPE in [3, 8, 9, 10] then
      begin
        ISTRORCH := AStructure.ControlReach;
        WriteInteger(ISTRORCH);
        Comment := Comment + ' ISTRORCH';
      end;

      if (ISTRTYPE in [3, 8, 9, 10])
        and (AStructure.ControlType = ctFlow) then
      begin
        ISTROQCON := AStructure.ConnectedControlReach;
        WriteInteger(ISTROQCON);
        Comment := Comment + ' ISTROQCON';
      end;

      if ISTRTYPE in [3, 8, 9, 10] then
      begin
        CSTROLO := '';
        case AStructure.ControlOperated of
          coGreaterEqual: CSTROLO := ' GE ';
          coLessThan: CSTROLO := ' LT ';
        end;
        WriteString(CSTROLO);
        Comment := Comment + ' CSTROLO';
      end;

      if ISTRTYPE in [1, 3, 8, 9, 10] then
      begin
        case AStructure.CriticalMethod of
          smValue:
            begin
              STRCRIT := AStructure.CriticalValue;
              WriteFloatCondensed(STRCRIT);
            end;
          smTabFile:
            begin
              TabNumber := Model.SwrTabFiles.IndexOfFileName(AStructure.FullCriticalTabFileName);
              if TabNumber >= 0 then
              begin
                CSTRCRIT := ' TABDATA' + IntToStr(TabNumber);
                WriteString(CSTRCRIT);
              end
              else
              begin
                frmErrorsAndWarnings.AddError(Model, StrInvalidTabFileNam,
                  Format(StrStructure0sTab, [AStructure.Name,
                  AStructure.FullCriticalTabFileName]));
              end;
            end;
          else Assert(False);
        end;
        Comment := Comment + ' CSTRCRIT';
      end;

      if ISTRTYPE in [3, 8, 9, 10] then
      begin
        STRCRITC := AStructure.ControlOffsetCriterion;
        WriteFloatCondensed(STRCRITC);
        Comment := Comment + ' STRCRITC';
      end;

      if ISTRTYPE in [3, 8, 9, 10] then
      begin
        STRRT := AStructure.StartingControlRate;
        WriteFloatCondensed(STRRT);
        Comment := Comment + ' STRRT';
      end;

      if ISTRTYPE in [1, 3, 8, 9, 10] then
      begin
        STRMAX := AStructure.MaximumControlRate;
        WriteFloatCondensed(STRMAX);
        Comment := Comment + ' STRMAX';
      end;

      if (ISTRTYPE in [1, 3, 8, 9, 10]) and (AStructure.FullDischargeTabFile <> '') then
      begin
        CSTRVAL := ' TABFILE' + ExtractRelativePath(FNameOfFile, AStructure.FullDischargeTabFile);
        WriteString(CSTRVAL);
        Comment := Comment + ' CSTRVAL';
      end;

      if Comment <> '' then
      begin
        Comment := ' # Data Set 13B' + Comment;
        WriteString(Comment);
        NewLine;
      end;

      // Data Set 13C
      if ISTRTYPE = 4 then
      begin
        for ItemIndex := 0 to AStructure.Table.Count - 1 do
        begin
          TableItem := AStructure.Table[ItemIndex];
          STRELEV := TableItem.Elev;
          STRQ := TableItem.Discharge;
          WriteFloatCondensed(STRELEV);
          WriteFloatCondensed(STRQ);
          WriteString(' # Data Set 13C: STRELEV STRQ');
          NewLine;
        end;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet10(TimeIndex: Integer);
var
  CellIndex: Integer;
  ValueCellList: TValueCellList;
  Reach: TReachObject;
  TransientCell: TSwrTransientCell;
  NextGeoNumber: integer;
  GeoIndex: Integer;
  GeoItem: TReachGeometryItem;
  ModelTop: TDataArray;
  ModelTopElev: Double;
  ReachBottom: Double;
begin
  // Data set 10
  for GeoIndex := 0 to Model.SwrReachGeometry.Count - 1 do
  begin
    GeoItem := Model.SwrReachGeometry[GeoIndex];
    GeoItem.GeoNumber := 0;
  end;

  NextGeoNumber := 1;
  ValueCellList := FTransientReachList[TimeIndex];
  if ValueCellList.Count <> FReachList.Count then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
      Format(StrInStressPeriodD, [TimeIndex +1]));
    Exit;
  end;

  ModelTop := Model.DataArrayManager.GetDataSetByName(kModelTop);
  ModelTop.Initialize;
  
  Assert(ValueCellList.Count = FReachList.Count);
  for CellIndex := 0 to ValueCellList.Count - 1 do
  begin
    TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
    Reach := FReachList[CellIndex];
    WriteInteger(Reach.FReachData.Reach);

//    WriteInteger(TransientCell.GeoNumber);

    GeoItem := Model.SwrReachGeometry[TransientCell.GeoNumber-1];
    if GeoItem.GeoNumber = 0 then
    begin
      GeoItem.GeoNumber := NextGeoNumber;
      Inc(NextGeoNumber);
    end;
    WriteInteger(GeoItem.GeoNumber);

    WriteValueOrFormula(TransientCell, SwrVerticalOffsetPosition);
//    WriteFloat(TransientCell.VerticalOffSet);
    WriteString(' # Data Set 10 IGMODRCH IGEONUMR GZSHIFT');
    NewLine;
    
    
    if GeoItem.HasMinimum then
    begin
      ModelTopElev := ModelTop.RealData[0, TransientCell.Row, TransientCell.Column];
      ReachBottom := GeoItem.MinimumElevation + TransientCell.VerticalOffSet;
      if ReachBottom > ModelTopElev then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrTheSWRStreamBotto,
          Format(StrAtRowColumn,
          [TransientCell.Row, TransientCell.Column, GeoItem.MinimumElevation,
          TransientCell.VerticalOffSet,
          GeoItem.MinimumElevation + TransientCell.VerticalOffSet, ModelTopElev]), 
          TransientCell.ScreenObject);
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet7(TimeIndex: Integer);
var
  CellList: TList;
begin
  // Data set 7
  if Values.Count > 0 then
  begin
    CellList := Values[TimeIndex];
    case FSwrPackage.RainSpecification of
      smObject:
        begin
          WriteRainList(CellList);
        end;
      smArray:
        begin
          WriteRainArray(CellList);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet8(TimeIndex: Integer);
var
  CellList: TList;
begin
  // Data set 8
  if FEvapValues.Count > 0 then
  begin
    CellList := FEvapValues[TimeIndex];
    case FSwrPackage.EvapSpecification of
      smObject:
        begin
          WriteEvapList(CellList);
        end;
      smArray:
        begin
          WriteEvapArray(CellList);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet9(TimeIndex: Integer);
var
  CellList: TList;
begin
  // Data set 8
  if FLatInflowValues.Count > 0 then
  begin
    CellList := FLatInflowValues[TimeIndex];
    case FSwrPackage.LateralInflowSpecification of
      smObject:
        begin
          WriteLatInflowList(CellList);
        end;
      smArray:
        begin
          WriteLatInflowArray(CellList);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TModflowSwrWriter.WriteEvapArray(CellList: TList);
begin
  WriteTransientSwrArray(CellList,
    Model.ModflowPackages.SwrPackage.EvapAssignmentMethod,
    '# Data Set 8b EVAP2D', 'EVAP2D');
end;

procedure TModflowSwrWriter.WriteEvapList(CellList: TList);
//var
//  index: Integer;
//  ValueCellList: TValueCellList;
//  ACell: TSwrValueCell;
//  AReach: TReachObject;
begin
  WriteSwrTransientList(CellList, ' # Data Set 8A; IEVPRCH EVAP');
end;

procedure TModflowSwrWriter.WriteDataSet6(TimeIndex: Integer);
var
  Reach: TReachObject;
  TransientCell: TSwrTransientCell;
  CellIndex: Integer;
  ISWRBND: Integer;
  ValueCellList: TValueCellList;
  PriorValueCellList: TValueCellList;
  PriorTransientCell: TSwrTransientCell;
  procedure WriteReach;
  begin
    Reach := FReachList[CellIndex];
    WriteInteger(Reach.FReachData.Reach);
    ISWRBND := MaxInt;
    case TransientCell.ReachType of
      srtActive:
        ISWRBND := 1;
      srtInactive:
        ISWRBND := 0;
      srtSpecifiedHead:
        ISWRBND := -1;
    else
      Assert(False);
    end;
    WriteInteger(ISWRBND);
    WriteString(' # Data Set 6: IBNDRCH ISWRBND');
    NewLine;
  end;
begin
  // Data Set 6
  ValueCellList := FTransientReachList[TimeIndex];
  if TimeIndex > 0 then
  begin
    PriorValueCellList := FTransientReachList[TimeIndex-1];
  end
  else
  begin
    PriorValueCellList := nil;
  end;
  if ValueCellList.Count <> FReachList.Count then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUndefinedTransient,
      Format(StrInStressPeriodD, [TimeIndex +1]));
    Exit;
  end;
  Assert(ValueCellList.Count = FReachList.Count);
  if PriorValueCellList <> nil then
  begin
    Assert(PriorValueCellList.Count = FReachList.Count);
  end;
  for CellIndex := 0 to ValueCellList.Count - 1 do
  begin
    TransientCell := ValueCellList[CellIndex] as TSwrTransientCell;
    if TimeIndex = 0 then
    begin
      WriteReach;
    end
    else
    begin
      PriorTransientCell := PriorValueCellList[CellIndex] as TSwrTransientCell;
      if (TransientCell.ReachType = srtSpecifiedHead)
        or (PriorTransientCell.ReachType = srtSpecifiedHead)
        or (TransientCell.ReachType <> PriorTransientCell.ReachType) then
      begin
        WriteReach;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.AssignFinalReachNumbers;
var
  ANumber: Integer;
  IntList: Generics.Collections.TList<Integer>;
  GroupIndexes: Generics.Collections.TList<Integer>;
  ReachIndex: Integer;
  AReachObject: TReachObject;
  GroupIndex: Integer;
  GroupNumber: Integer;
  NewReachAnnotation, ReachAnnotation: string;
  CheckList: TList<Integer>;
  ErrorReachIndex: Integer;
  OtherReachObject: TReachObject;
  ErrorReachObject: TReachObject;
begin
  ReachAnnotation := '';
  GroupIndexes := TList<Integer>.Create;
  IntList := TList<Integer>.Create;
  CheckList := TList<Integer>.Create;
  try
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReachObject := FReachList[ReachIndex];
      ANumber := AReachObject.FReachData.Reach;
      if ANumber <> 0 then
      begin
        IntList.Add(ANumber);
      end;
      if AReachObject.FReachData.Grouped then
      begin
        ANumber := AReachObject.FReachData.ReachGroup;
        GroupIndexes.Add(ANumber);
      end;
    end;
    IntList.Sort;
    GroupIndexes.Sort;
    for GroupIndex := GroupIndexes.Count - 1 downto 1 do
    begin
      if GroupIndexes[GroupIndex] = GroupIndexes[GroupIndex-1] then
      begin
        GroupIndexes.Delete(GroupIndex);
      end;
    end;
    ANumber := 1;
    GroupNumber := 1;
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReachObject := FReachList[ReachIndex];
      if AReachObject.FReachData.Reach = 0 then
      begin
        while IntList.IndexOf(ANumber) >= 0 do
        begin
          Inc(ANumber);
        end;
        AReachObject.FReachData.Reach := ANumber;
        NewReachAnnotation := Format('Automatically assigned. Object = %s',
          [AReachObject.FReachData.ObjectName]);

        // Use reference counting of strings to reduce memory use.
        if ReachAnnotation <> NewReachAnnotation then
        begin
          ReachAnnotation := NewReachAnnotation;
        end;

        AReachObject.FReachData.ReachAnnotation := ReachAnnotation;
        Inc(ANumber);
      end;
      if CheckList.IndexOf(AReachObject.FReachData.Reach) >= 0 then
      begin
        ErrorReachObject := nil;
        for ErrorReachIndex := 0 to ReachIndex - 1 do
        begin
          OtherReachObject := FReachList[ErrorReachIndex];
          if OtherReachObject.FReachData.Reach = AReachObject.FReachData.Reach then
          begin
            ErrorReachObject := OtherReachObject;
            break;
          end;
        end;
        frmErrorsAndWarnings.AddError(Model, StrIllegalDuplicateSW,
          Format(StrSWRReachNumber0,
          [AReachObject.FReachData.Reach, AReachObject.FReachData.Cell.Layer+1,
          AReachObject.FReachData.Cell.Row+1, AReachObject.FReachData.Cell.Column+1,
          ErrorReachObject.FReachData.Cell.Layer+1, ErrorReachObject.FReachData.Cell.Row+1,
          ErrorReachObject.FReachData.Cell.Column+1,
          (AReachObject.FReachData.ScreenObject as TScreenObject).name,
          (ErrorReachObject.FReachData.ScreenObject as TScreenObject).name]),
          AReachObject.FReachData.ScreenObject);
      end
      else
      begin
        CheckList.Add(AReachObject.FReachData.Reach);
      end;
      if AReachObject.FReachData.Reach > FReachList.Count then
      begin
        frmErrorsAndWarnings.AddError(Model, StrSWRReachNumberToo,
          Format(StrTheReachNumber0,
          [AReachObject.FReachData.Reach, AReachObject.FReachData.Cell.Layer+1,
          AReachObject.FReachData.Cell.Row+1, AReachObject.FReachData.Cell.Column+1,
          (AReachObject.FReachData.ScreenObject as TScreenObject).Name, FReachList.Count]), AReachObject.FReachData.ScreenObject);
      end;
      if not AReachObject.FReachData.Grouped then
      begin
        while GroupIndexes.IndexOf(GroupNumber) >= 0 do
        begin
          Inc(GroupNumber);
        end;
        AReachObject.FReachData.ReachGroup := GroupNumber;
        Inc(GroupNumber);
      end;
    end;
  finally
    IntList.Free;
    CheckList.Free;
    GroupIndexes.Free;
  end;
end;

procedure TModflowSwrWriter.AddSearchPoints(ScreenObject: TScreenObject;
  UsedReachs, AllObjectReaches: TList<TReachObject>; Points: TList<TPoint2D>);
var
  ReachIndex: Integer;
  ReachObject: TReachObject;
  Cell: TCellLocation;
  Grid: TCustomModelGrid;
  ReachPoint: TPoint2D;
begin
  Grid := Model.Grid;
  if ScreenObject.SetValuesOfEnclosedCells and ScreenObject.Closed then
  begin
    UsedReachs.AddRange(AllObjectReaches.ToArray);
    for ReachIndex := 0 to UsedReachs.Count - 1 do
    begin
      ReachObject := UsedReachs[ReachIndex];
      Cell := ReachObject.FReachData.Cell;
      ReachPoint := Grid.TwoDElementCenter(Cell.Column, Cell.Row);
      Points.Add(ReachPoint);
    end;
  end
  else
  begin
    if AllObjectReaches.Count > 0 then
    begin
      UsedReachs.Add(AllObjectReaches.First);
      Points.Add(ScreenObject.Points[0]);
      if ScreenObject.Count > 1 then
      begin
        UsedReachs.Add(AllObjectReaches.Last);
        Points.Add(ScreenObject.Points[ScreenObject.Count - 1]);
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.EvaluateInterObjectConnections(
  ReachLists: TStringList);
var
  NeighborReaches: TList<TReachObject>;
  Boundary: TSwrReachBoundary;
  ReachObject: TReachObject;
  ScreenObject: TScreenObject;
  Neighbor: TReachObject;
  ADistance: TFloat;
  CurrentReaches: TList<TReachObject>;
  ReachPoint: TPoint2D;
  SecondClosest: TReachObject;
  FirstList: TList<TReachObject>;
  SecondPoints: TList<TPoint2D>;
  NeighborObject: TScreenObject;
  Cell: TCellLocation;
  FirstPoints: TList<TPoint2D>;
  AConnection: TSwrConnectionItem;
  FirstClosest: TReachObject;
  ReachPosition: Integer;
  LinkIndex: Integer;
  ConnectionPosition: Integer;
  SecondList: TList<TReachObject>;
  ClosestDistance: TFloat;
  NeighborIndex: Integer;
  NeighborPoint: TPoint2D;
  ClosestReach: TReachObject;
  ScreenObjectIndex: Integer;
  OuterReachIndex: Integer;
  InnerReachIndex: Integer;
  Grid: TCustomModelGrid;
  ReachObjectArray: array of array of array of TList<TReachObject>;
  ReachIndex: Integer;
  AReach: TReachObject;
  ACell: TCellLocation;
  OtherReach: TReachObject;
  ReachListList: TObjectList<TList<TReachObject>>;
  LocalList: TList<TReachObject>;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Grid := Model.Grid;
  for ScreenObjectIndex := 0 to FScreenObjectList.Count - 1 do
  begin
    ScreenObject := FScreenObjectList[ScreenObjectIndex];
    Boundary := ScreenObject.ModflowSwrReaches;
    Assert(Boundary <> nil);
    if Boundary.Connections.Count > 0 then
    begin
      CurrentReaches :=
        ReachLists.Objects[ScreenObjectIndex] as TList<TReachObject>;
      for LinkIndex := 0 to Boundary.Connections.Count - 1 do
      begin
        AConnection := Boundary.Connections[LinkIndex];
        case AConnection.Method of
          scmObject:
            begin
              ConnectionPosition :=
                ReachLists.IndexOf(AConnection.ScreenObjectName);
              if ConnectionPosition >= 0 then
              begin
                NeighborReaches :=
                  ReachLists.Objects[ConnectionPosition] as TList<TReachObject>;
                NeighborObject := FScreenObjectList[ConnectionPosition];
                FirstList := TList<TReachObject>.Create;
                SecondList := TList<TReachObject>.Create;
                FirstPoints := TList<TPoint2D>.Create;
                SecondPoints := TList<TPoint2D>.Create;
                try
                  AddSearchPoints(ScreenObject, FirstList, CurrentReaches,
                    FirstPoints);
                  AddSearchPoints(NeighborObject, SecondList, NeighborReaches,
                    SecondPoints);
                  if (FirstList.Count > 0) and (SecondList.Count > 0) then
                  begin
                    FirstClosest := FirstList.First;
                    SecondClosest := SecondList.First;
                    ReachPoint := FirstPoints.First;
                    NeighborPoint := SecondPoints.First;
                    ClosestDistance := Distance(NeighborPoint, ReachPoint);
                    for OuterReachIndex := 0 to FirstList.Count - 1 do
                    begin
                      ReachPoint := FirstPoints[OuterReachIndex];
                      for InnerReachIndex := 0 to SecondList.Count - 1 do
                      begin
                        NeighborPoint := SecondPoints[InnerReachIndex];
                        ADistance := Distance(NeighborPoint, ReachPoint);
                        if ADistance < ClosestDistance then
                        begin
                          FirstClosest := FirstList[OuterReachIndex];
                          SecondClosest := SecondList[InnerReachIndex];
                          ClosestDistance := ADistance;
                        end;
                      end;
                    end;
                    if (FirstClosest <> SecondClosest)
                      and (SecondClosest.Neighbors.IndexOf(FirstClosest) < 0)
                      then
                    begin
                      SecondClosest.Neighbors.Add(FirstClosest);
                      FirstClosest.Neighbors.Add(SecondClosest);
                    end;
                  end;
                finally
                  FirstList.Free;
                  SecondList.Free;
                  FirstPoints.Free;
                  SecondPoints.Free;
                end;
              end;
            end;
          scmSpecifiedReach:
            begin
              ReachPosition := IndexOfReach(AConnection.Reach);
              if ReachPosition >= 0 then
              begin
                ReachObject := FReachList[ReachPosition];
                Cell := ReachObject.FReachData.Cell;
                ReachPoint := Grid.TwoDElementCenter(Cell.Column, Cell.Row);
                ClosestReach := nil;
                if ScreenObject.SetValuesOfEnclosedCells
                  and ScreenObject.Closed then
                begin
                  ClosestDistance := 0;
                  for NeighborIndex := 0 to CurrentReaches.Count - 1 do
                  begin
                    Neighbor := CurrentReaches[NeighborIndex];
                    Cell := Neighbor.FReachData.Cell;
                    NeighborPoint := Grid.TwoDElementCenter(Cell.Column, Cell.Row);
                    ADistance := Distance(NeighborPoint, ReachPoint);
                    if ClosestReach = nil then
                    begin
                      ClosestReach := Neighbor;
                      ClosestDistance := ADistance;
                    end
                    else
                    begin
                      if ADistance < ClosestDistance then
                      begin
                        ClosestReach := Neighbor;
                        ClosestDistance := ADistance;
                      end;
                    end;
                  end;
                end
                else
                begin
                  if CurrentReaches.Count > 0 then
                  begin
                    Neighbor := CurrentReaches[0];
                    NeighborPoint := ScreenObject.Points[0];
                    ClosestDistance := Distance(NeighborPoint, ReachPoint);
                    ClosestReach := Neighbor;
                    Neighbor := CurrentReaches[CurrentReaches.Count - 1];
                    NeighborPoint := ScreenObject.Points[ScreenObject.Count - 1];
                    if ClosestDistance > Distance(NeighborPoint, ReachPoint) then
                    begin
                      ClosestReach := Neighbor;
                    end;
                  end;
                end;
                if (ClosestReach <> nil) and (ClosestReach <> ReachObject)
                  and (ReachObject.Neighbors.IndexOf(ClosestReach) < 0) then
                begin
                  ReachObject.Neighbors.Add(ClosestReach);
                  ClosestReach.Neighbors.Add(ReachObject);
                end;
              end;
            end;
          scmSameCell:
            begin
              ConnectionPosition :=
                ReachLists.IndexOf(AConnection.ScreenObjectName);
              if ConnectionPosition >= 0 then
              begin
                NeighborReaches :=
                  ReachLists.Objects[ConnectionPosition] as TList<TReachObject>;
                NeighborObject := FScreenObjectList[ConnectionPosition];

                ReachListList := TObjectList<TList<TReachObject>>.Create;
                try
                  SetLength(ReachObjectArray, Grid.LayerCount, Grid.RowCount,
                    Grid.ColumnCount);
                  for LayerIndex := 0 to Grid.LayerCount - 1 do
                  begin
                    for RowIndex := 0 to Grid.RowCount - 1 do
                    begin
                      for ColIndex := 0 to Grid.ColumnCount - 1 do
                      begin
                        ReachObjectArray[LayerIndex,RowIndex,ColIndex] := nil;
                      end;
                    end;
                  end;

                  for ReachIndex := 0 to CurrentReaches.Count - 1 do
                  begin
                    AReach := CurrentReaches[ReachIndex];
                    ACell := AReach.FReachData.Cell;
                    if ReachObjectArray[ACell.Layer, ACell.Row, ACell.Column] = nil then
                    begin
                      LocalList := TList<TReachObject>.Create;
                      ReachObjectArray[ACell.Layer, ACell.Row, ACell.Column] := LocalList;
                      ReachListList.Add(LocalList);
                    end
                    else
                    begin
                      LocalList := ReachObjectArray[ACell.Layer, ACell.Row, ACell.Column];
                    end;
                    LocalList.Add(AReach);
                  end;

                  for ReachIndex := 0 to NeighborReaches.Count - 1 do
                  begin
                    AReach := NeighborReaches[ReachIndex];
                    ACell := AReach.FReachData.Cell;
                    LocalList := ReachObjectArray[ACell.Layer, ACell.Row, ACell.Column];
                    if LocalList <> nil then
                    begin
                      for InnerReachIndex := 0 to LocalList.Count - 1 do
                      begin
                        OtherReach := LocalList[InnerReachIndex];
                        if AReach.Neighbors.IndexOf(OtherReach) < 0 then
                        begin
                          AReach.Neighbors.Add(OtherReach);
                          OtherReach.Neighbors.Add(AReach);
                        end;
                      end;
                    end;
                  end;
                finally
                  ReachListList.Free;
                end;
              end;

            end
        else
          Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteFile(const AFileName: string);
var
  CellList: TList;
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: Integer;
  TimeIndex: Integer;
  Dummy: TDataArray;
begin
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnusedSWRReachGeo);
  { TODO -cFMP : This needs to be finished. }
  if not (Model.ModelSelection in [msModflow, msModflowNWT, msModflowFmp]) then
  begin
    Exit
  end;

  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSWR) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage('Writing SWR Package input.');
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrSWR, Model.UnitNumbers.UnitNumber(StrSWR), FNameOfFile, foInput, Model);
  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2;
    WriteDataSet3;

    WriteDataSet4A;
    WriteDataSet4B;
    WriteDataSet4C;
    WriteDataSet4D;
    WriteDataSet4E;
    WriteDataSet4F;
    frmProgressMM.AddMessage('  Writing Data Sets 5 to 15.');
    WriteStressPeriods;
  finally
    CloseFile;
  end;

  if FDirectRunoffValues.Count > 0 then
  begin
    OpenFile(FDirectRunoffFileName);
    try
      WriteCommentLine('SWR Direct Runoff file created on '
        + DateToStr(Now) + ' by ' + Model.ProgramName + ' version '
        + IModelVersion + '.');

      DefaultValue := 0;
      for TimeIndex := 0 to FDirectRunoffValues.Count - 1 do
      begin
        // Data Set 1
        WriteString('1 1 0 1 # Data Set 1 Stress Period '
          + IntToStr(TimeIndex+1)
          + ': ITMP IRDMAP IRDMULT IRDVAL');
        NewLine;

        CellList := FDirectRunoffValues[TimeIndex];

        // Data Set 2
        DataTypeIndex := 0;
        DataType := rdtInteger;
        WriteTransient2DArray(' # Data Set 2: DROMAP2D' , DataTypeIndex,
          DataType, DefaultValue, CellList, umAssign, True, Dummy, 'DROMAP2D');

        // Skip Data Set 3.

        // Data Set 4
        DataTypeIndex := 1;
        DataType := rdtDouble;
        WriteTransient2DArray(' # Data Set 4: DROVAL2D' , DataTypeIndex,
          DataType, DefaultValue, CellList, umAssign, True, Dummy, 'DROVAL2D');
      end;
    finally
      CloseFile;
    end;
  end;

end;

procedure TModflowSwrWriter.WriteGeometry;
var
  GeoIndex: Integer;
  GeoItem: TReachGeometryItem;
  // data set 11A variables.
  IGEONUM: Integer;
  IGEOTYPE: Integer;
  IGCNDOP: Integer;
  GMANNING: double;
  NGEOPTS: Integer;
  GWIDTH: double;
  GBELEV: double;
  GSSLOPE: double;
  GCND: double;
  GLK: double;
  GCNDLN: double;
  GETEXTD: double;
  Comment: string;
  CrossSectionIndex: Integer;
  SectionItem: TReachCrossSectionItem;
  TableIndex: Integer;
  TableItem: TReachTableItem;
begin
  for GeoIndex := 0 to Model.SwrReachGeometry.Count - 1 do
  begin
    GeoItem := Model.SwrReachGeometry[GeoIndex];

    // Data set 11A
    if GeoItem.GeoNumber = 0 then
    begin
      Continue;
    end;

    IGEONUM := GeoItem.GeoNumber;
    WriteInteger(IGEONUM);

    IGEOTYPE := ord(GeoItem.GeometryType) + 1;
    WriteInteger(IGEOTYPE);

    IGCNDOP := ord(GeoItem.ConductanceMethod);
    WriteInteger(IGCNDOP);

    GMANNING := GeoItem.Roughness;
    WriteFloat(GMANNING);

    Comment := ' # Data Set 11A: IGEONUM IGEOTYPE IGCNDOP GMANNING';

    if IGEOTYPE in [3,4] then
    begin
      if IGEOTYPE = 3 then
      begin
        NGEOPTS := GeoItem.CrossSection.Count;
      end
      else
      begin
        Assert(IGEOTYPE = 4);
        NGEOPTS := GeoItem.Table.Count;
      end;
      WriteInteger(NGEOPTS);
      Comment := Comment + ' NGEOPTS';
    end;

    if IGEOTYPE in [1,2] then
    begin
      GWIDTH := GeoItem.Width;
      WriteFloat(GWIDTH);

      GBELEV := GeoItem.BottomElevation;
      WriteFloat(GBELEV);
      Comment := Comment + ' GWIDTH GBELEV';

      if IGEOTYPE = 2 then
      begin
        GSSLOPE := GeoItem.SideSlope;
        WriteFloat(GSSLOPE);
        Comment := Comment + ' GSSLOPE';
      end;
    end;

    if IGCNDOP = 0 then
    begin
      GCND := GeoItem.Conductance;
      WriteFloat(GCND);
      Comment := Comment + ' GCND';
    end
    else if IGCNDOP in [1,3] then
    begin
      GLK := GeoItem.Leakance;
      WriteFloat(GLK);
      Comment := Comment + ' GLK';
    end;

    if IGCNDOP in [2,3] then
    begin
      GCNDLN := GeoItem.CenterDistance;
      WriteFloat(GCNDLN);
      Comment := Comment + ' GCNDLN';
    end;

    if IGEOTYPE = 5 then
    begin
      GETEXTD := GeoItem.ExtinctionDepth;
      WriteFloat(GETEXTD);
      Comment := Comment + ' GETEXTD';
    end;
    WriteString(Comment);
    NewLine;

    if IGEOTYPE = 3 then
    begin
      for CrossSectionIndex := 0 to GeoItem.CrossSection.Count -1 do
      begin
        SectionItem := GeoItem.CrossSection[CrossSectionIndex];
        WriteFloat(SectionItem.X);
        WriteFloat(SectionItem.Elev);
        WriteString(' # Data Set 11B XB(i) ELEVB(i)');
        NewLine;
      end;
    end
    else if IGEOTYPE = 4 then
    begin
      for TableIndex := 0 to GeoItem.Table.Count - 1 do
      begin
        TableItem := GeoItem.Table[TableIndex];
        WriteFloat(TableItem.Elevation);
        WriteFloat(TableItem.Volume);
        WriteFloat(TableItem.WettedPerimeter);
        WriteFloat(TableItem.SurfaceArea);
        WriteFloat(TableItem.CrossSectionArea);
        WriteString(' # Data Set 11C: ELEV(i) VOL(i) WETPER(i) SAREA(i) XAREA(i)');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteLatInflowArray(CellList: TList);
begin
  WriteTransientSwrArray(CellList,
    Model.ModflowPackages.SwrPackage.LatInflowAssignmentMethod,
    '# Data Set 9b QLATFLOW2D', 'QLATFLOW2D');
end;

procedure TModflowSwrWriter.WriteLatInflowList(CellList: TList);
begin
  WriteSwrTransientList(CellList, ' # Data Set 9A: ILINRCH QLATFLOW');
end;

procedure TModflowSwrWriter.WriteRainArray(CellList: TList);
begin
  WriteTransientSwrArray(CellList,
    Model.ModflowPackages.SwrPackage.RainAssignmentMethod,
    '# Data Set 7b RAIN2D', 'RAIN2D');
end;

procedure TModflowSwrWriter.WriteRainList(CellList: TList);
begin
  WriteSwrTransientList(CellList, ' # Data Set 7A: IRAIRCH RAIN');
end;

procedure TModflowSwrWriter.WriteDataSet1;
var
  ISWRONLY: integer;
  ISWRCBC: integer;
  ISWRPRGF: integer;
  ISWRPSTG: integer;
  ISWRPQAQ: integer;
  ISWRPQM: integer;
  ISWRPSTR: integer;
  ISWRPFRN: integer;
  Option: string;
  OutputFileName: string;
  OutputUnitNumber: integer;
  OptionList: TStringList;
  OptionIndex: Integer;
  CSWROPT: string;
  BaseName: string;
begin
  NREACHES := FReachList.Count;
  if FSwrPackage.OnlyUseSWR then
  begin
    ISWRONLY := 1;
  end
  else
  begin
    ISWRONLY := 0;
  end;
  GetFlowUnitNumber(ISWRCBC);

  ISWRPRGF := 0;
  BaseName := ChangeFileExt(FNameOfFile, '');
  case FSwrPackage.PrintInflowsAndOutflows of
    spoNone: ISWRPRGF := 0;
    spoASCII:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachGroupFlowsA);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachGroupStage);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPRGF := OutputUnitNumber;
      end;
    spoBinary:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachGroupFlowsB);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachGroupStage);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPRGF := -OutputUnitNumber;
      end;
    else
      Assert(False);
  end;

  ISWRPSTG := 0;
  case FSwrPackage.PrintStage of
    spoNone: ISWRPSTG := 0;
    spoASCII:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachStageA);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachStage);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPSTG := OutputUnitNumber;
      end;
    spoBinary:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachStageB);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachStage);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPSTG := -OutputUnitNumber;
      end;
    else
      Assert(False);
  end;

  ISWRPQAQ := 0;
  case FSwrPackage.PrintReachExchangeAndProperties of
    spoNone: ISWRPQAQ := 0;
    spoASCII:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachExchangeA);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachExchange);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPQAQ := OutputUnitNumber;
      end;
    spoBinary:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrReachExchangeB);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrReachExchange);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPQAQ := -OutputUnitNumber;
      end;
    else
      Assert(False);
  end;

  ISWRPQM := 0;
  case FSwrPackage.PrintReachLateralFlow of
    spoNone: ISWRPQM := 0;
    spoASCII:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrLateralFlowA);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrLateral);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPQM := OutputUnitNumber;
      end;
    spoBinary:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrLateralFlowB);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrLateral);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPQM := -OutputUnitNumber;
      end;
    else
      Assert(False);
  end;

  ISWRPSTR := 0;
  case FSwrPackage.PrintStructureFlow of
    spoNone: ISWRPSTR := 0;
    spoASCII:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrStructureFlowA);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrStructure);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPSTR := OutputUnitNumber;
      end;
    spoBinary:
      begin
        OutputFileName := ChangeFileExt(BaseName, StrSwrStructureFlowB);
        OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrStructure);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
        end;
        ISWRPSTR := -OutputUnitNumber;
      end;
    else
      Assert(False);
  end;

  if FSwrPackage.PrintMaxFroude then
  begin
    ISWRPFRN := 1;
  end
  else
  begin
    ISWRPFRN := 0;
  end;

  OptionList := TStringList.Create;
  try
    if FSwrPackage.PrintSwrDataToScreen then
    begin
      OptionList.Add('PRINT_SWR_TO_SCREEN');
    end;

    case FSwrPackage.SaveSwrTimeStepLength of
      spoNone: ; // do nothing
      spoASCII:
        begin
          OutputFileName := ChangeFileExt(BaseName, StrSwrTimeStepLengthA);
          OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrTimeSteps);
          if not WritingTemplate then
          begin
            WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
          end;
          OptionList.Add('SAVE_SWRDT ' + IntToStr(OutputUnitNumber));
        end;
      spoBinary:
        begin
          OutputFileName := ChangeFileExt(BaseName, StrSwrTimeStepLengthB);
          OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrTimeSteps);
          if not WritingTemplate then
          begin
            WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
          end;
          OptionList.Add('SAVE_SWRDT ' + IntToStr(-OutputUnitNumber));
        end;
      else
        Assert(False);
    end;

    if FSwrPackage.SaveAverageSimulatedResults then
    begin
      OptionList.Add('SAVE_AVERAGE_RESULTS');
    end;

    if FSwrPackage.SaveConvergenceHistory then
    begin
      OutputFileName := ChangeFileExt(BaseName, StrSwrConvergence);
      OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrConvergenceHistory);
      if not WritingTemplate then
      begin
        WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
      end;
      OptionList.Add('SAVE_CONVERGENCE_HISTORY ' + IntToStr(OutputUnitNumber));
    end;

    case FSwrPackage.SaveRiver of
      ssrNone: ; // do nothing
      ssrSaveActive:
        begin
          OutputFileName := ChangeFileExt(BaseName, StrSwrRIV);
          OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrRiver);
          if not WritingTemplate then
          begin
            WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
          end;
          OptionList.Add('SAVE_RIVER_PACKAGE ' + IntToStr(OutputUnitNumber));
        end;
      ssrSaveAll:
        begin
          OutputFileName := ChangeFileExt(BaseName, StrSwrRIV);
          OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrRiver);
          if not WritingTemplate then
          begin
            WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
          end;
          OptionList.Add('SAVE_RIVER_PACKAGE_ALL ' + IntToStr(OutputUnitNumber));
        end;
      else
        Assert(False);
    end;

    case FSwrPackage.SaveObs of
      ssoNone: ; // do nothing
      ssoSaveObs, ssoSaveObsAll:
      begin
//        if Model.SwrObservations.Count > 0 then
        begin
          OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrObs);
          if FSwrPackage.SaveObs = ssoSaveObs then
          begin
            CSWROPT := 'SAVE_SWROBSERVATIONS ';
          end
          else
          begin
            CSWROPT := 'SAVE_SWROBSERVATIONS_ALL ';
          end;
          case FSwrPackage.ObsFormat of
            swofAscii:
              begin
                OutputFileName := ChangeFileExt(BaseName, StrSwrObsExt_A);
                if not WritingTemplate then
                begin
                  WriteToNameFile(StrData, OutputUnitNumber, OutputFileName, foOutput, Model);
                end;
                OptionList.Add(CSWROPT + IntToStr(OutputUnitNumber));
              end;
            swofBinary:
              begin
                OutputFileName := ChangeFileExt(BaseName, StrSwrObsExt_B);
                if not WritingTemplate then
                begin
                  WriteToNameFile(StrDataBinary, OutputUnitNumber, OutputFileName, foOutput, Model);
                end;
                OptionList.Add(CSWROPT + IntToStr(-OutputUnitNumber));
              end;
            else Assert(False);
          end;
        end;
      end;
      else
        Assert(False);
    end;

    if Model.SwrTabFiles.Count > 0 then
    begin
      OptionList.Add('USE_TABFILES');
    end;

    if FDirectRunoffValues.Count > 0 then
    begin
      FDirectRunoffFileName := ChangeFileExt(BaseName, StrSwrDirectRunoffExt);
      OutputUnitNumber := Model.UnitNumbers.UnitNumber(StrSwrDirectRunoff);
      if not WritingTemplate then
      begin
        WriteToNameFile(StrData, OutputUnitNumber, FDirectRunoffFileName, foInput, Model);
      end;
      OptionList.Add('USE_DIRECT_RUNOFF ' + IntToStr(OutputUnitNumber));
    end;

    if FSwrPackage.ContinueDespiteNonConvergence then
    begin
      OptionList.Add('USE_NONCONVERGENCE_CONTINUE');
    end;

    if FSwrPackage.UseUpstreamWeightingForDiffusiveWave then
    begin
      OptionList.Add('USE_UPSTREAM_WEIGHTING');
    end;

    if FSwrPackage.UseInexactNewton then
    begin
      OptionList.Add('USE_INEXACT_NEWTON');
    end;

    if FSwrPackage.UseSteadyStateStorage then
    begin
      OptionList.Add('USE_STEADYSTATE_STORAGE');
    end;

    if FSwrPackage.UseLaggedStagesAndFlows then
    begin
      OptionList.Add('USE_LAGGED_OPR_DATA');
    end;

    if FSwrPackage.UseLinearDepthScaling then
    begin
      OptionList.Add('USE_LINEAR_DEPTH_SCALING');
    end;

    case FSwrPackage.Scaling of
      ssNone: ; // do nothing
      ssDiagonal: OptionList.Add('USE_DIAGONAL_SCALING');
      ssL2Norm: OptionList.Add('USE_L2NORM_SCALING');
      else Assert(False);
    end;

    case FSwrPackage.Reordering of
      srNone: ; // do nothing
      srUse: OptionList.Add('USE_RCMREORDERING');
      srUseIfImproved: OptionList.Add('USE_RCMREORDERING_IF_IMPROVEMENT');
      else Assert(False);
    end;

    case FSwrPackage.NewtonCorrection of
      sncNone: ; // do nothing
      sncImplicit: OptionList.Add('USE_IMPLICIT_NEWTON_CORRECTION');
      sncExplicit: OptionList.Add('USE_EXPLICIT_NEWTON_CORRECTION');
      else Assert(False);
    end;

    case FSwrPackage.FlowToleranceOption of
      rtoNone: ; // do nothing
      rtoFractional: OptionList.Add('USE_FRACTIONAL_TOLR');
      rtoL2Norm: OptionList.Add('USE_L2NORM_TOLR');
      else Assert(False);
    end;

    case FSwrPackage.ExchangeToleranceOption of
      etNone: ; // do nothing
      etGlobal: OptionList.Add('USE_GLOBAL_TOLA');
      etAbsolute: OptionList.Add('USE_ABSOLUTE_TOLA');
    end;

    // This option must be last.
    if OptionList.Count > 0 then
    begin
      OptionList.Add('END');
    end;

    if OptionList.Count > 0 then
    begin
      Option := ' AUXILIARY IFACE SWROPTIONS';
//      Option := ' SWROPTIONS';
    end
    else
    begin
      Option := ' AUXILIARY IFACE';
    end;

    // Data Set 1A
    WriteInteger(NREACHES);
    WriteInteger(ISWRONLY);
    WriteInteger(ISWRCBC);
    WriteInteger(ISWRPRGF);
    WriteInteger(ISWRPSTG);
    WriteInteger(ISWRPQAQ);
    WriteInteger(ISWRPQM);
    WriteInteger(ISWRPSTR);
    WriteInteger(ISWRPFRN);
    WriteString(Option);
    WriteString(' # NREACHES ISWRONLY ISWRCBC ISWRPRGF ISWRPSTG ISWRPQAQ ISWRPQM ISWRPSTR ISWRPFRN [Option]');
    NewLine;

    // Data Set 1B
    for OptionIndex := 0 to OptionList.Count - 1 do
    begin
      WriteString(OptionList[OptionIndex]);
      NewLine;
    end;
  finally
    OptionList.Free;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet2;
var
  DLENCONV: double;
  TIMECONV: double;
  RTINI: double;
  RTMIN: double;
  RTMAX: double;
  RTPRN: double;
  RTMULT: double;
  NTMULT: integer;
  DMINGRAD: double;
  DMINDPTH: double;
  DMAXRAI: double;
  DMAXSTG: double;
  DMAXINF: double;
  Comment: string;
begin
  case Model.ModflowOptions.LengthUnit of
    1: {feet} DLENCONV :=  3.28081;
    2: {m} DLENCONV := 1;
    3: {cm} DLENCONV := 100;
    else DLENCONV := 1;
  end;

  case Model.ModflowOptions.TimeUnit of
    1: {seconds} TIMECONV := 1;
    2: {minutes} TIMECONV := 60;
    3: {hours} TIMECONV := 3600;
    4: {days} TIMECONV := 86400;
    5: {years} TIMECONV := 31557600;
    else TIMECONV := 1;
  end;

  RTINI := FSwrPackage.InitialTimeStepLength;
  RTMIN := FSwrPackage.MinTimeStepLength;
  RTMAX := FSwrPackage.MaxTimeStepLength;
  RTPRN := FSwrPackage.SaveFrequency;
  RTMULT := FSwrPackage.TimeStepMultiplier;
  NTMULT := FSwrPackage.TimeStepIncreaseFrequency;
  DMINGRAD := FSwrPackage.MinGradientForDiffusiveFlow;
  DMINDPTH := FSwrPackage.MinDepthForOutflow;

  WriteFloatCondensed(DLENCONV);
  WriteFloatCondensed(TIMECONV);
  WriteFloatCondensed(RTINI);
  WriteFloatCondensed(RTMIN);
  WriteFloatCondensed(RTMAX);
  WriteFloatCondensed(RTPRN);
  WriteFloatCondensed(RTMULT);
  WriteInteger(NTMULT);
  WriteFloatCondensed(DMINGRAD);
  WriteFloatCondensed(DMINDPTH);
  Comment := ' # Data Set 2: DLENCONV TIMECONV RTINI RTMIN RTMAX RTPRN RTMULT NTMULT DMINGRAD DMINDPTH ';

  if (RTMIN < RTMAX) and (RTMULT > 1) then
  begin
    DMAXRAI := FSwrPackage.MaxRainfallForStepAdjustment;
    DMAXSTG := FSwrPackage.MaxStageChangePerStep;
    DMAXINF := FSwrPackage.MaxInflowChange;
    WriteFloat(DMAXRAI);
    WriteFloat(DMAXSTG);
    WriteFloat(DMAXINF);
    Comment := Comment + 'DMAXRAI DMAXSTG DMAXINF';
  end;

  WriteString(Comment);
  NewLine;

end;

procedure TModflowSwrWriter.WriteDataSet3;
var
  ISOLVER: integer;
  NOUTER: integer;
  NINNER: integer;
  IBT: integer;
  TOLS: double;
  TOLR: double;
  TOLA: double;
  DAMPSS: double;
  DAMPTR: double;
  IPRSWR: integer;
  MUTSWR: integer;
  IPC: integer;
  NLEVELS: integer;
  DROPTOL: double;
  IBTPRT: integer;
  PTOLR: double;
  Comment: string;
begin
  ISOLVER := Ord(FSwrPackage.Solver)+1;
  NOUTER := FSwrPackage.MaxOuterIterations;
  NINNER := FSwrPackage.MaxInnerIterations;
  IBT := FSwrPackage.MaxLineSearchIterations;
  TOLS := FSwrPackage.StageTolerance;
  TOLR := FSwrPackage.FlowTolerance;
  TOLA := FSwrPackage.ExchangeTolerance;
  if IBT > 1 then
  begin
    DAMPSS := 1;
    DAMPTR := 1;
  end
  else
  begin
    DAMPSS := FSwrPackage.SteadyStateDampingFactor;
    DAMPTR := FSwrPackage.TransientDampingFactor;
  end;
  IPRSWR := FSwrPackage.ConvergencePrintoutInterval;
  MUTSWR := Ord(FSwrPackage.PrintConvergence);
  IPC := Ord(FSwrPackage.Preconditioner);
  NLEVELS := FSwrPackage.MaxLevels;
  DROPTOL := FSwrPackage.DropThreshold;
  IBTPRT := FSwrPackage.PrintLineSearchInterval;
  PTOLR := FSwrPackage.AlternativeFlowTolerance;

  WriteInteger(ISOLVER);
  WriteInteger(NOUTER);
  WriteInteger(NINNER);
  WriteInteger(IBT);
  WriteFloatCondensed(TOLS);
  WriteFloatCondensed(TOLR);
  WriteFloatCondensed(TOLA);
  WriteFloatCondensed(DAMPSS);
  WriteFloatCondensed(DAMPTR);
  WriteInteger(IPRSWR);
  WriteInteger(MUTSWR);
  Comment := ' # Data Set 3: ISOLVER NOUTER NINNER IBT TOLS TOLR TOLA DAMPSS DAMPTR IPRSWR MUTSWR';
  if ISOLVER > 1 then
  begin
    WriteInteger(IPC);
    Comment := Comment + ' IPC';
    if IPC = 4 then
    begin
      WriteInteger(NLEVELS);
      WriteFloatCondensed(DROPTOL);
    Comment := Comment + ' NLEVELS DROPTOL';
    end;
  end;
  if IBT > 1 then
  begin
    WriteInteger(IBTPRT);
    Comment := Comment + ' IBTPRT';
  end;
  if FSwrPackage.FlowToleranceOption in [rtoFractional, rtoL2Norm] then
  begin
    WriteFloatCondensed(PTOLR);
    Comment := Comment + ' PTOLR';
  end;
  WriteString(Comment);
  NewLine;
end;

procedure TModflowSwrWriter.WriteDataSet4A;
var
  ReachIndex: Integer;
  IRCH4A, IROUTETYPE, IRGNUM, KRCH, IRCH, JRCH: integer;
  RLEN: double;
  AReach: TSwrReachRecord;
begin
  for ReachIndex := 0 to FReachList.Count - 1 do
  begin
    AReach := FReachList[ReachIndex].FReachData;
    IRCH4A := AReach.Reach;
    IROUTETYPE := Ord(AReach.RouteType)+1;
    IRGNUM := AReach.ReachGroup;

    // There are 3 cases for KRCH
    // 1. For reaches that occupy entire cells KRCH must be 1.
    // This is set in UpdateKRCH.
    // 2. For multilayer reaches, KRCH must be negative. This is set here.
    // 3. For other reaches KRCH is the layer number.
    if AReach.MultiLayer then
    begin
      KRCH := -1;
    end
    else
    begin
      KRCH := AReach.Cell.Layer + 1;
    end;

    IRCH := AReach.Cell.Row + 1;
    JRCH := AReach.Cell.Column + 1;
    RLEN := AReach.ReachLength;
    WriteInteger(IRCH4A);
    WriteInteger(IROUTETYPE);
    WriteInteger(IRGNUM);
    WriteInteger(KRCH);
    WriteInteger(IRCH);
    WriteInteger(JRCH);
    WriteFloat(RLEN);
    WriteString(' # Data Set 4A: IRCH4A, IROUTETYPE, IRGNUM, KRCH, IRCH, JRCH, RLEN');
//    WriteString(' ');
//    WriteString((AReach.ScreenObject as TScreenObject).Name);
    NewLine;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet4B;
var
  ReachIndex: Integer;
  AReachObject: TReachObject;
  IRCH4B: integer;
  NCONN: Integer;
  ICONN: array of integer;
  NeighborIndex: Integer;
begin
  for ReachIndex := 0 to FReachList.Count - 1 do
  begin
    AReachObject := FReachList[ReachIndex];
    IRCH4B := AReachObject.FReachData.Reach;
    NCONN := AReachObject.Neighbors.Count;
    SetLength(ICONN, NCONN);
    for NeighborIndex := 0 to AReachObject.Neighbors.Count - 1 do
    begin
      ICONN[NeighborIndex] := AReachObject.Neighbors[NeighborIndex].FReachData.Reach;
    end;
    WriteInteger(IRCH4B);
    WriteInteger(NCONN);
    for NeighborIndex := 0 to Length(ICONN) - 1 do
    begin
      WriteInteger(ICONN[NeighborIndex]);
    end;
    WriteString(' # Data Set 4B: IRCH4B NCONN ICONN(1)...ICONN(NCONN)');
    NewLine;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet4C;
var
  NTABS: Integer;
begin
  if Model.SwrTabFiles.Count > 0 then
  begin
    NTABS := Model.SwrTabFiles.Count;
    WriteInteger(NTABS);
    WriteString(' # Data Set 4C, NTABS');
    NewLine;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet4D;
var
  TabFileIndex: Integer;
  TabItem: TTabFileItem;
  ITAB: integer;
  CTABTYPE: string;
  ITABUNIT: integer;
  CINTP: String;
  CTABRCH: string;
  ITABRCH: TList<Integer>;
  UsedObjects: TStringList;
  RelativeFileName: string;
  ReachIndex: Integer;
  AReachObject: TReachObject;
begin
  if Model.SwrTabFiles.Count > 0 then
  begin
    ITABRCH := TList<Integer>.Create;
    try
      for TabFileIndex := 0 to Model.SwrTabFiles.Count - 1 do
      begin
        ITAB := TabFileIndex + 1;
        TabItem := Model.SwrTabFiles[TabFileIndex];
      {$REGION ''}
        case TabItem.TabType of
          ttRain: CTABTYPE := ' RAIN';
          ttEvap: CTABTYPE := ' EVAP';
          ttLatFlow: CTABTYPE := ' LATFLOW';
          ttStage: CTABTYPE := ' STAGE';
          ttStructure: CTABTYPE := ' STRUCTURE';
          ttTime: CTABTYPE := ' TIME';
          else Assert(False);
        end;
      {$ENDREGION}
        ITABUNIT := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
        if Trim(TabItem.FullTabFileName) = '' then
        begin
          frmErrorsAndWarnings.AddError(Model, StrSWRTabfileNameNot,
            StrInTheSWRTabfiles);
        end
        else
        begin
          if not FileExists(TabItem.FullTabFileName) then
          begin
            frmErrorsAndWarnings.AddError(Model, StrSWRTabfileDoesNot,
              Format(StrTheSWRTabfileName, [TabItem.FullTabFileName]));
          end;
        end;
        RelativeFileName := ExtractRelativePath(FNameOfFile, TabItem.FullTabFileName);
        case TabItem.TabFormat of
          tfText:
            begin
              if not WritingTemplate then
              begin
                WriteToNameFile(StrDATA, ITABUNIT, RelativeFileName, foInput, Model, True);
              end;
            end;
          tfBinary:
            begin
              if not WritingTemplate then
              begin
                WriteToNameFile(StrDATABINARY, ITABUNIT, RelativeFileName, foInput, Model, True);
              end;
              ITABUNIT := -ITABUNIT;
            end;
          else Assert(False);
        end;
      {$REGION ''}
        case TabItem.InterpolationMethod of
          imNone: CINTP := ' NONE';
          imAverage: CINTP := ' AVERAGE';
          imInterpolate: CINTP := ' INTERPOLATE';
          else Assert(False);
        end;
      {$ENDREGION}
        ITABRCH.Clear;
        CTABRCH := '';
        if not (TabItem.TabType in [ttTime, ttStructure]) then
        begin
          {$REGION ''}
            case TabItem.ReachSelectionMethod of
              rsmAll: CTABRCH := ' ALL';
              rsmObjects:
                begin
                  UsedObjects := TStringList.Create;
                  try
                    UsedObjects.CommaText := TabItem.ObjectNames;
                    for ReachIndex := 0 to FReachList.Count - 1 do
                    begin
                      AReachObject := FReachList[ReachIndex];
                      if UsedObjects.IndexOf(AReachObject.FReachData.ObjectName) >= 0 then
                      begin
                        ITABRCH.Add(AReachObject.FReachData.Reach);
                      end;
                    end;
                    CTABRCH := ' ' + IntToStr(ITABRCH.Count);
                  finally
                    UsedObjects.Free;
                  end;
                end;
              rsmValue:
                begin
                  for ReachIndex := 0 to FReachList.Count - 1 do
                  begin
                    AReachObject := FReachList[ReachIndex];
                    if AReachObject.FReachData.TabLocation = TabItem.Value then
                    begin
                      ITABRCH.Add(AReachObject.FReachData.Reach);
                    end;
                  end;
                  CTABRCH := ' ' + IntToStr(ITABRCH.Count);
                end;
              rsmReaches:
                begin
                  ITABRCH.Capacity := TabItem.Reaches.Count;
                  for ReachIndex := 0 to TabItem.Reaches.Count - 1 do
                  begin
                    ITABRCH.Add(TabItem.Reaches[ReachIndex].Value);
                  end;
                  CTABRCH := ' ' + IntToStr(ITABRCH.Count);
                end;
              else Assert(False);
            end;
          {$ENDREGION}
        end;
        WriteInteger(ITAB);
        WriteString(CTABTYPE);
        WriteInteger(ITABUNIT);
        WriteString(CINTP);
        if not (TabItem.TabType in [ttTime, ttStructure]) then
        begin
          WriteString(CTABRCH);
          if TabItem.ReachSelectionMethod <> rsmAll then
          begin
            for ReachIndex := 0 to ITABRCH.Count - 1 do
            begin
              WriteInteger(ITABRCH[ReachIndex]);
            end;
          end;
        end;
        WriteString(' # Data Set 4D: CTABTYPE ITABUNIT CINTP');
        if TabItem.TabType <> ttTime then
        begin
          WriteString(' CTABRCH');
          if TabItem.ReachSelectionMethod <> rsmAll then
          begin
            WriteString(' ITABRCH(1)...ITABRCH(NTABRCH)');
          end;
        end;
        NewLine;
      end;
    finally
      ITABRCH.Free;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteDataSet4E;
var
  NOBS: Integer;
  ReachIndex: Integer;
  AReachObject: TReachObject;
  ObsType: TSwrObsType;
  NeighborIndex: Integer;
  ANeighborReach: TReachObject;
  StructureIndex: Integer;
  AStructure: TStructure;
begin
  if FSwrPackage.SaveObs in [ssoSaveObs, ssoSaveObsAll] then
  begin
    NOBS := Model.SwrObservations.Count;

    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReachObject := FReachList[ReachIndex];
      for ObsType in [sotStage, sotDepth, sotBottom, sotBaseFlow] do
      begin
        if ObsType in AReachObject.FReachData.ObsTypes then
        begin
          Inc(NOBS);
        end;
      end;

      if sotFlow in AReachObject.FReachData.ObsTypes then
      begin
        for NeighborIndex := 0 to AReachObject.Neighbors.Count - 1 do
        begin
          ANeighborReach := AReachObject.Neighbors[NeighborIndex];
          if ANeighborReach.FReachData.Reach > AReachObject.FReachData.Reach then
          begin
            Inc(NOBS);
          end;
        end;
      end;

      if sotStructure in AReachObject.FReachData.ObsTypes then
      begin
        for StructureIndex := 0 to Model.SwrStructures.Count - 1 do
        begin
          AStructure := Model.SwrStructures[StructureIndex];
          if AReachObject.FReachData.Reach = AStructure.Reach then
          begin
            Inc(NOBS);
          end;
        end;
      end;
    end;


    begin
      WriteInteger(NOBS);
      WriteString(' Data Set 4E: NOBS');
      NewLine;
    end;
    if NOBS = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNoSWRObservations,
        StrAlthoughYouHaveSp);
    end;
  end;
end;

function TModflowSwrWriter.ObsTypeToName(ObsType: TSwrObsType;
  AReachObject: TReachObject; ConnectedReach: TReachObject = nil;
  StructureCount: Integer = 0): string;
begin
  result := IntToStr(AReachObject.FReachData.Reach);
  case ObsType of
    sotStage: result := 'Stage_' + result;
    sotDepth: result := 'Depth_' + result;
    sotBottom: result := 'Bottom_' + result;
    SotFlow:
      begin
        Assert(ConnectedReach <> nil);
        Assert(ConnectedReach.FReachData.Reach > AReachObject.FReachData.Reach);
        result := 'Flow_' + result + '_'
          + IntToStr(ConnectedReach.FReachData.Reach);
      end;
    sotStructure:
      begin
        result := 'Struct_' + result + '_' + IntToStr(StructureCount);
      end;
    sotBaseFlow: result := 'Basefl_' + result;
    else Assert(False);
  end;
end;

procedure TModflowSwrWriter.AssignReachObservationLinks;
var
  ObsType: TSwrObsType;
  ObsIndex: Integer;
//  StructCount: Integer;
  Obs: TSwrObsItem;
  StructureIndex: Integer;
  Link: TReachObsLink;
//  AStructure: TStructure;
  NeighborIndex: Integer;
  ANeighborReach: TReachObject;
  ReachIndex: Integer;
  ReachObject: TReachObject;
  StructureList: TStructureList;
begin
  if FSwrPackage.SaveObs in [ssoSaveObs, ssoSaveObsAll] then
  begin
    for ObsIndex := 0 to Model.SwrObservations.Count - 1 do
    begin
      Obs := Model.SwrObservations[ObsIndex];
      if Obs.ObservationReach - 1 < ReachCount then
      begin
        ReachObject := Reaches[Obs.ObservationReach - 1];
        Link := TReachObsLink.Create;
        Link.ObsName := Obs.ObsName;
        Link.ObsType := Obs.ObsType;
        ReachObject.ObsLinks.Add(Link);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidSWRObservat,
          Format(StrTheReachSpecified, [Obs.ObsName]));
      end;
    end;
    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      ReachObject := FReachList[ReachIndex];
      for ObsType in [sotStage, sotDepth, sotBottom, sotBaseFlow] do
      begin
        if ObsType in ReachObject.FReachData.ObsTypes then
        begin
          Link := TReachObsLink.Create;
          Link.ObsName := ObsTypeToName(ObsType, ReachObject);
          Link.ObsType := ObsType;
          ReachObject.ObsLinks.Add(Link);
        end;
      end;
      if sotFlow in ReachObject.FReachData.ObsTypes then
      begin
        for NeighborIndex := 0 to ReachObject.Neighbors.Count - 1 do
        begin
          ANeighborReach := ReachObject.Neighbors[NeighborIndex];
          if ANeighborReach.FReachData.Reach > ReachObject.FReachData.Reach then
          begin
            Link := TReachObsLink.Create;
            Link.ObsName := ObsTypeToName(sotFlow, ReachObject, ANeighborReach);
            Link.ObsType := sotFlow;
            ReachObject.ObsLinks.Add(Link);
          end;
        end;
      end;
      if sotStructure in ReachObject.FReachData.ObsTypes then
      begin
        if not FStuctureDict.TryGetValue(ReachObject.FReachData.Reach, StructureList) then
        begin
          Assert(False);
        end;

        for StructureIndex := 0 to StructureList.Count - 1 do
        begin
//          AStructure := StructureList[StructureIndex];
          Link := TReachObsLink.Create;
          Link.ObsName := ObsTypeToName(sotStructure, ReachObject, nil, StructureIndex+1);
          Link.ObsType := sotStructure;
          ReachObject.ObsLinks.Add(Link);
        end;
      end;
    end;
  end;
end;

function TModflowSwrWriter.ObsTypeToString(ObsType: TSwrObsType): string;
begin
  result := '';
  case ObsType of
    sotStage: result := ' STAGE ';
    sotDepth: result := ' DEPTH ';
    sotBottom: result := ' RBOTTOM ';
    sotFlow: result := ' FLOW ';
    sotStructure: result := ' STRUCTURE ';
    sotBaseFlow: result := ' BASEFLOW ';
    else Assert(False);
  end;
end;

procedure TModflowSwrWriter.WriteDataSet4F;
var
  ObsIndex: Integer;
  Obs: TSwrObsItem;
  COBSNAME: string;
  COBSTYPE: string;
  IOBSLOC: Integer;
  IOBSLOC2: Integer;
  Comment: string;
  IOBSLAY: Integer;
  ReachIndex: Integer;
  AReachObject: TReachObject;
  ObsType: TSwrObsType;
  NeighborIndex: Integer;
  ANeighborReach: TReachObject;
  StructureIndex: Integer;
  AStructure: TStructure;
//  StructCount: Integer;
  StructureList: TStructureList;
begin
  if FSwrPackage.SaveObs in [ssoSaveObs, ssoSaveObsAll] then
  begin
    for ObsIndex := 0 to Model.SwrObservations.Count - 1 do
    begin
      Obs := Model.SwrObservations[ObsIndex];
      COBSNAME := Obs.ObsName + ' ';
      COBSTYPE := ObsTypeToString(Obs.ObsType);
      IOBSLOC := Obs.ObservationReach;
      IOBSLAY := Obs.ObservationLayer;
      if IOBSLAY > 0 then
      begin
        IOBSLAY :=  Model.DataSetLayerToModflowLayer(IOBSLAY);
      end;
      WriteString(COBSNAME);
      WriteString(COBSTYPE);
      WriteInteger(IOBSLOC);
      Comment := ' # Data Set 4f: COBSNAME, COBSTYPE, IOBSLOC';
      if Obs.ObsType in [sotFlow, sotStructure] then
      begin
        if Obs.ObsType = sotFlow then
        begin
          IOBSLOC2 := Obs.ConnectedReachOrStructure;
        end
        else
        begin
          Assert(Obs.ObsType = sotStructure);
          AStructure := Model.SwrStructures.GetStructureByName(Obs.StructureName);
          if not FStuctureDict.TryGetValue(AStructure.Reach, StructureList) then
          begin
            Assert(False);
          end;
          IOBSLOC2 := StructureList.IndexOf(AStructure) + 1;
          Assert(IOBSLOC2 >= 1);
        end;
        WriteInteger(IOBSLOC2);
        Comment := Comment + ' IOBSLOC2';
      end;
      if Obs.ObsType = sotBaseFlow then
      begin
        WriteInteger(IOBSLAY);
        Comment := Comment + ' IOBSLAY';
      end;
      WriteString(Comment);
      NewLine;
    end;

    for ReachIndex := 0 to FReachList.Count - 1 do
    begin
      AReachObject := FReachList[ReachIndex];
      IOBSLOC := AReachObject.FReachData.Reach;
      for ObsType in [sotStage, sotDepth, sotBottom, sotBaseFlow] do
      begin
        if ObsType in AReachObject.FReachData.ObsTypes then
        begin
          COBSNAME := ObsTypeToName(ObsType, AReachObject);
          COBSTYPE := ObsTypeToString(ObsType);
          WriteString(COBSNAME);
          WriteString(COBSTYPE);
          WriteInteger(IOBSLOC);
          Comment := ' # Data Set 4f: COBSNAME, COBSTYPE, IOBSLOC';
          if ObsType = sotBaseFlow then
          begin
            IOBSLAY := 0;
            WriteInteger(IOBSLAY);
            Comment := Comment + ', IOBSLAY';
          end;
          WriteString(Comment);
          NewLine;
        end;
      end;

      if sotFlow in AReachObject.FReachData.ObsTypes then
      begin
        for NeighborIndex := 0 to AReachObject.Neighbors.Count - 1 do
        begin
          ANeighborReach := AReachObject.Neighbors[NeighborIndex];
          if ANeighborReach.FReachData.Reach > AReachObject.FReachData.Reach then
          begin
            COBSNAME := ObsTypeToName(sotFlow, AReachObject, ANeighborReach);
            COBSTYPE := ObsTypeToString(sotFlow);
            IOBSLOC2 := ANeighborReach.FReachData.Reach;
            WriteString(COBSNAME);
            WriteString(COBSTYPE);
            WriteInteger(IOBSLOC);
            WriteInteger(IOBSLOC2);
            Comment := ' # Data Set 4f: COBSNAME, COBSTYPE, IOBSLOC IOBSLOC2';
            WriteString(Comment);
            NewLine;
          end;
        end;
      end;

      if sotStructure in AReachObject.FReachData.ObsTypes then
      begin
        if not FStuctureDict.TryGetValue(AReachObject.FReachData.Reach, StructureList) then
        begin
          Assert(False);
        end;


//        StructCount := 0;
        for StructureIndex := 0 to StructureList.Count - 1 do
        begin
//          AStructure := StructureList[StructureIndex];
//          if AReachObject.FReachData.Reach = AStructure.Reach then
//          begin
//            if AStructure.ConnectedReach > 0 then
//            begin
//              ANeighborReach := FReachList[AStructure.ConnectedReach-1];
//            end
//            else
//            begin
//              ANeighborReach := nil;
//            end;
//            Inc(StructCount);

            COBSNAME := ObsTypeToName(sotStructure, AReachObject,
              nil, StructureIndex+1);
            COBSTYPE := ObsTypeToString(sotStructure);
            IOBSLOC2 := StructureIndex+1;
            WriteString(COBSNAME);
            WriteString(COBSTYPE);
            WriteInteger(IOBSLOC);
            WriteInteger(IOBSLOC2);
            Comment := ' # Data Set 4f: COBSNAME, COBSTYPE, IOBSLOC IOBSLOC2';
            WriteString(Comment);
            NewLine;
//          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowSwrWriter.WriteStageArray(CellList: TList);
begin
  WriteTransientSwrArray(CellList,
    Model.ModflowPackages.SwrPackage.StageAssignmentMethod,
    '# Data Set 14b STAGE2D', 'STAGE2D');
end;

procedure TModflowSwrWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
begin

//  RainSimulated := Values.Count > 0;

  for TimeIndex := 0 to FTransientReachList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(StrWritingStressP, [TimeIndex+1]));

    WriteDataSet5(TimeIndex);

    WriteDataSet6(TimeIndex);
    WriteDataSet7(TimeIndex);
    WriteDataSet8(TimeIndex);
    WriteDataSet9(TimeIndex);
    WriteDataSet10(TimeIndex);
    WriteDataSet11(TimeIndex);
    WriteDataSet12(TimeIndex);
    WriteDataSet13(TimeIndex);
    WriteDataSet14(TimeIndex);
    WriteDataSet15(TimeIndex);

  end;
end;

end.
