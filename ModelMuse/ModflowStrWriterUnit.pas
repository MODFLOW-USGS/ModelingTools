unit ModflowStrWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, Classes, Generics.Collections,CustomModflowWriterUnit,
  Forms, PhastModelUnit, ModflowBoundaryDisplayUnit,
  ModflowPackageSelectionUnit, SysUtils, ModflowCellUnit,
  ScreenObjectUnit, ModflowBoundaryUnit, OrderedCollectionUnit, GoPhastTypes,
  Contnrs, Generics.Defaults, ModflowTransientListParameterUnit,
  RbwParser, FluxObservationUnit, Vcl.Dialogs;

type
  TStrSegment = class(TObject)
  private
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
    FReaches: TList;
    // @name contains a parameter names.  Associated with the
    // parameter name in the Objects property is a TList.  Each TList
    // contains a series of @link(TValueCellList)s; one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for
    // one stress period.
    FParamValues: TStringList;
    FScreenObject: TScreenObject;
    FDownstreamSegments: TGenericIntegerList;
    FDiversionSegments: TGenericIntegerList;
    // @name is used when sorting segments.
    FHasTributariesTemp: boolean;
    FHasTributariesForThisStressPeriod: boolean;
    FParameterName: string;
    FParameterNumber: integer;
    // @name is the segment number exported to MODFLOW.
    FNewSegmentNumber: integer;
    // Each @link(TGenericIntegerList) in @name represents the tributaries for one
    // stress period.
    FTributaries: TListOfTIntegerList;
    // Each member of @name represents the diversion segment number for one
    // stress period.
    FDiversions: TGenericIntegerList;
    // Each member of @name represents the outflow segment number for one
    // stress period.
    FOutFlowSegments: TGenericIntegerList;
    procedure EvaluateLinks(Model: TBaseModel);
    function OriginalSegmentNumber: Integer;
    procedure AssignSegmentAndReachNumbers;
  public
    constructor Create;
    destructor Destroy; override;
    property ScreenObject: TScreenObject read FScreenObject;
  end;

  TStrSegmentComparer = class(TComparer<TStrSegment>)
    function Compare(const Left, Right: TStrSegment): Integer; override;
  end;

  TParameterSegments = class(TObject)
  private
    FParameterName: string;
    FUpStreamParameters: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TStrWriter = class(TFluxObsWriter)
  private
    FSegments: TObjectList<TStrSegment>;
    // Each list in @name contains the segments for the corresponding
    // parameter in @link(FParameters);
    FParameterSegments: TObjectList<TList<TStrSegment>>;
    // @name contains a list of parameter instance names for each parameter.
    FParameterInstanceNames: TStringListObjectList;
//    FNameOfFile: string;
    FCalculateStage: Boolean;
    // @name is a list of @link(TModflowTransientListParameter) that are in
    // use in the STR package.  The corresponding segments are in
    // @link(FParameterSegments).
    FParameters: TList<TModflowTransientListParameter>;
    NTRIB: Integer;
    NDIV: Integer;
    NQST: Integer;
    NQCST: Integer;
    NQTST: Integer;
//    FPestParamUsed: Boolean;
    procedure SortSegments;
    function FindSegment(OriginalSegmentNumber: integer): TStrSegment;
    procedure AssociateParametersWithSegments;
    procedure EvaluateTributariesAndDiversions;
    // @name writes the number of parameters.
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3and4;
    procedure WriteDataSets5to10;
    procedure WriteDataSet5(StressPeriodIndex: Integer);
    procedure WriteDataSet6(StressPeriodIndex: Integer);
    procedure WriteDataSet7(StressPeriodIndex: Integer);
    procedure WriteDataSet8(StressPeriodIndex: Integer);
    procedure WriteDataSet9(StressPeriodIndex: Integer);
    procedure WriteDataSet10(StressPeriodIndex: Integer);
    function GetSegFromObject(AScreenObject: TScreenObject): TStrSegment;
    function FindSegmentWithNewSegmentNumber(
      NewSegmentNumber: integer): TStrSegment;
    procedure CheckReachSeparations;
    procedure CheckStreamTributaries;
    procedure CheckActiveReaches;
    procedure CheckReachProperties;
    function UseFixedFormat: Boolean;
    procedure WriteFileInternal;
  protected
    procedure Evaluate; override;
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    class function ObservationExtension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function ParameterType: TParameterType; override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    function ObsNameWarningString: string; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    procedure WriteObservationCell(ACell: TValueCell; DataSet5: TStringList;
      var Expression: TExpression; DataSets, Variables: TList;
      ObsFactor: TObservationFactor); override;
//    function ObsTypeMF6: string; override;
  public
    class function ComputeStreamConstant(Model: TCustomModel): double;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
    class function ObservationOutputExtension: string; override;
  end;

resourcestring
  StrErrorInStreamPackLength = 'Error in Stream package: Length units for mo' +
  'del are undefined';
  StrErrorInStreamPackTime = 'Error in Stream package: Time units for model ' +
  'are undefined';

implementation

uses
  ModflowUnitNumbers, ModflowStrUnit, frmProgressUnit,
  frmErrorsAndWarningsUnit, GIS_Functions, ModflowTimeUnit, Math,
  frmModflowOutputControlUnit, ModflowOutputControlUnit, DataSetUnit;

resourcestring
  StrWritingSTRPackage = 'Writing STR Package input.';
  StrWritingDataSet0 = '  Writing Data Set 0.';
  StrEvaluatingSTRPacka = 'Evaluating STR Package data.';
  StrEvaluatingS = '    Evaluating %s';
  StrTheFollowingStream = 'The following Stream observation names may be valid' +
  ' for MODFLOW but they are not valid for UCODE.';
  StrTheFollowingStreamPest = 'The following Stream observation names may be valid' +
  ' for MODFLOW but they are not valid for PEST.';
  DupNumbersError = 'The STR segments defined by %0:s and %1:s have '
    + 'the same segment number: %2:d.';
  DupErrorCategory = 'Duplicate STR segment numbers';
  StrInvalidDownstreamS = 'Invalid downstream segment number in STR package.';
  StrTheSTRSegmentDefi = 'The STR segment defined by %0:s lists segment numb' +
  'er %1:d as a downstream segment but no such segment exists.';
  StrStreamsInTheSTRP = 'Streams in the STR package can not be downstream of' +
  ' themselves';
  StrTheSTRStreamSegem = 'The STR stream segment defined by %0:s specifies ' +
  'that the segment downstream of it has a segment number of %1:d. This is i' +
  't''s own segment number.';
  StrSegmentsInTheSTR = 'Segments in the STR package must only have downstre' +
  'am segments that have higher segment numbers.';
  StrTheSTRStreamSegme = 'The STR stream segment defined by %0:s lists %1:d ' +
  'as a downstream segment which is lower than its own segment number: %2:d.';
  StrInvalidDiversionSe = 'Invalid diversion segment number in STR package.';
  StrTheSTRSegmentDiversion = 'The STR segment defined by %0:s lists segment' +
  ' number %1:d as a diversion segment but no such segment exists.';
  StrStreamsInTheSTRPDiversion = 'Streams in the STR package can not divert ' +
  'water from themselves';
  StrTheSTRStreamSegmeDiversion = 'The STR stream segment defined by %0:s sp' +
  'ecifies that the segment from which it diverts water has a segment number' +
  ' of %1:d. This is it''s own segment number.';
  StrSegmentsInTheSTRDiversion = 'Segments in the STR package must only have' +
  ' diversion segments that have lower segment numbers.';
  StrTheSTRStreamSegmeDiver = 'The STR stream segment defined by %0:s lists ' +
  '%1:d as a downstream segment which is higher than its own segment number: ' +
  '%2:d.';
  CircularError = 'Object: %0:s; Segment Number: %1:d.';
  CircularCategory = 'The following STR segments circle back on themselves.';
  StrSTRParameterHasNo = 'STR parameter has no reaches defined.';
  StrNoReachesAreAssoc = 'No reaches are associated with the parameter "%s" ' +
  'in the Stream packages.';
  StrInvalidOutflowSegm = 'Invalid outflow segment number';
  StrTheOutflowSegment = 'The outflow segment (%0:d) specified by %1:s does ' +
  'not correspond to any segment';
  StrTheLengthUnitsFor = 'The length units for the model must be define in t' +
  'he MODFLOW Options dialog box for the stream constant to be calculated.';
  StrTheTimeUnitsForT = 'The time units for the model must be define in the ' +
  'MODFLOW Options dialog box for the stream constant to be calculated.';
  StrMultipliedByTheP = ' multiplied by the parameter value %0:g for paramet' +
  'er %1:s';
  StrFlowSetTo1Becau = 'Flow set to -1 because %s has tributaries.';
  StrInTheStreamSTR = 'In the Stream (STR) package, some reaches are separat' +
  'ed from previous reaches by more than one cell.';
  StrSegment0dReach = 'Segment: %0:d; Reach: %1:d; Stress Period: %2:d; Separation: %3:d';
  StrTributaryWarning = 'In the Stream (STR) package, there is a separation ' +
  'between a tributary segment and outflow segment of more than one cell.';
  StrTributarySegment0 = 'Tributary Segment %0:d, Outflow Segment %1:d; Stre' +
  'ss Period: %2:d, Separation %3:d cells';
  StrOneOrMoreReaches = 'One or more reaches in the stream package are in in' +
  'active cells.';
  StrInactiveReachCells = 'Stress period: %0:d; Segment: %1:d; Reach: %2:d; ' +
  'Layer: %3:d, Row: %4:d; Column: %5:d';
  StrOneOrMoreStreams = 'In one or more streams in STR package the FLOW is ' +
  'set to a negative number indicating that the stream has tributaries even ' +
  'though it doesn''t have tributaries. MODFLOW will treat such segments as ' +
  'having an inflow of zero.';
  StrInObject0sWith = 'Object %0:s with segment number %1:d.';
//  StrObject0sLayer = 'Object: %0:s; Layer: %1:d; Row: %2:d, Column: %3:d; St' +
//  'ress Period: %4:d';
  StrStageTopWarning = 'In the STR package, the stage is less than the st' +
  'ream bed top in one or more reaches.';
  StrStreamTopBottomWarning = 'In the STR package, the stream bed top is les' +
  's than the stream bed bottom in one or more reaches.';
  StrWidthWarning = 'In the STR package, the width is less than or equal to ' +
  'zero in one or more reaches.';
  StrSlopeWarning = 'In the STR package, the slope is less than or equal to ' +
  'zero in one or more reaches.';
  StrHighSlopeWarning = 'In the STR package, the slope is greater than or equal to than ' +
  '0.01 in one or more reaches.';
  StrRoughnessWarning = 'In the STR package, the roughness is less than or equal ' +
  'to than zero in one or more reaches.';
  StrDownhillFlowWarning = 'In the STR package, one or more reaches have a h' +
  'igher stage than in the reach upstream of them.';
  StrTribStageWarning = 'In the STR package, the last reach in some tributar' +
  'ies, have lower stages than the first reach of the segment into which the' +
  'y flow.';
  StrTributaryObject0 = 'Tributary Object: %0:s; Outflow Segment Object: %1:' +
  's; Difference: %2:g';
  StrDownhillObjectWarning = 'Object: %0:s; Reach: %1:d Layer: %2:d; Row: %3' +
  ':d, Column: %4:d; Stress Period: %5:d; Invalid value: %6:g';
  StrHighSTRStreamCond = 'High STR stream conductance compared to the cell-t' +
  'o-cell conductance may cause numerical difficulties';
  StrZeroSTRStreamCond = 'Transmissivity is negative or zero in cell containing a STR reach';
  StrTheFollowingObjectBadTribs = 'The following tributary objects define segments in ' +
  'the STR package that have no reaches.';
  StrInvalidSTRParamete = 'Invalid STR parameter name';
  StrAParameterNamed0 = 'A parameter named %0:s is used to define a stream i' +
  'n the STR package in the %1:s object but no such parameter has been defin' +
  'ed. This stream will be skipped.';
  StrInvalidSTRObservat = 'Invalid STR observation object';
  StrTheObject0sIsL = 'The object %0:s is listed as part of the %1:s observa' +
  'tion in the Stream Observation package but that object does not define a ' +
  'stream segment.';

const
  StrSegmentNumber = 'Segment Number in ';
  StrReachNumber = 'Reach Number in ';
  StrDownstreamSegmentNumber = 'Outflow Segment Number in ';
  StrDiversionSegmentNumber = 'Diversion Segment Number in ';

{ TStrWriter }

function TStrWriter.CellType: TValueCellType;
begin
  result := TStr_Cell;
end;

constructor TStrWriter.Create(Model: TCustomModel;
EvaluationType: TEvaluationType);
  begin
  inherited;
//  FValues := TList.Create;
  FSegments := TObjectList<TStrSegment>.Create;
  FParameterSegments := TObjectList<TList<TStrSegment>>.Create;

//  FParamValues := TStringList.Create;
//  FParamValues.OwnsObjects := True;
  OwnsValueContents := False;
  ParamValues.OwnsObjects := False;

  FParameters := TList<TModflowTransientListParameter>.Create;

  FParameterInstanceNames:= TStringListObjectList.Create;
end;

destructor TStrWriter.Destroy;
begin
  FParameterInstanceNames.Free;
  FParameters.Free;
  FParameterSegments.Free;
//  FParamValues.Free;
  FSegments.Free;
//  FValues.Free;
  inherited;
end;

procedure TStrWriter.Evaluate;
var
//  StartTime: Double;
//  EndTime: Double;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Boundary: TStrBoundary;
  NoAssignmentErrorRoot: string;
//  List: TList;
  NoDefinedErrorRoot: string;
  Segment: TStrSegment;
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  OutflowSegmentNumber: Integer;
  DownstreamSegment: TStrSegment;
  Reaches: TValueCellList;
  StrCell: TStr_Cell;
  AList: TList;
//  ReachList: TList;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, DupErrorCategory);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidDownstreamS);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrStreamsInTheSTRP);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSegmentsInTheSTR);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidDiversionSe);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrStreamsInTheSTRPDiversion);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSegmentsInTheSTRDiversion);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, CircularCategory);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObjectBadTribs);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSTRParamete);

    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSTRParameterHasNo);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidOutflowSegm);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheStreamSTR);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTributaryWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreReaches);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreStreams);

    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrStageTopWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrStreamTopBottomWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrWidthWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSlopeWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighSlopeWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRoughnessWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDownhillFlowWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTribStageWarning);

    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighSTRStreamCond);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrZeroSTRStreamCond);



    if not Package.IsSelected then
    begin
      Exit
    end;
    if Model.PackageGeneratedExternally(StrSTR) then
    begin
      Exit;
    end;
    NoDefinedErrorRoot := Format(StrNoDefinedBoundarie, [Package.PackageIdentifier]);
    NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);

    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoDefinedErrorRoot);

    frmProgressMM.AddMessage(StrEvaluatingSTRPacka);
    FCalculateStage := (Package as TStrPackageSelection).CalculateStage;

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
      Boundary := ScreenObject.ModflowStrBoundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
          ScreenObject.Name, ScreenObject);
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingS,
        [ScreenObject.Name]));
      Boundary.FixItems;
      Boundary.GetCellValues(Values, FParamValues, Model);
      if Values.Count > 0 then
      begin
        Assert(FParamValues.Count = 0);
        Segment := TStrSegment.Create;
        FSegments.Add(Segment);
        Segment.FReaches.Assign(Values);
        Segment.FScreenObject := ScreenObject;
      end
      else if FParamValues.Count > 0 then
      begin
        Segment := TStrSegment.Create;
        FSegments.Add(Segment);
        Segment.FParamValues.Assign(FParamValues);
        Segment.FScreenObject := ScreenObject;
      end;
      Values.Clear;
      FParamValues.Clear;
    end;

    SortSegments;

    AssociateParametersWithSegments;
    EvaluateTributariesAndDiversions;

    StressPeriods := Model.ModflowFullStressPeriods;
    for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
    begin
      for SegmentIndex := 0 to FSegments.Count - 1 do
      begin
        ASegment := FSegments[SegmentIndex];
        OutflowSegmentNumber := ASegment.FOutFlowSegments[StressPeriodIndex];
        if OutflowSegmentNumber <> 0 then
        begin
          DownstreamSegment := FindSegmentWithNewSegmentNumber(OutflowSegmentNumber);
          DownstreamSegment.FHasTributariesForThisStressPeriod := True;
        end;
      end;
      for SegmentIndex := 0 to FSegments.Count - 1 do
      begin
        ASegment := FSegments[SegmentIndex];
        if ASegment.FParamValues.Count > 0 then
        begin
          AList := ASegment.FParamValues.Objects[0] as TList;
          Reaches := AList[StressPeriodIndex];
        end
        else
        begin
          Reaches := ASegment.FReaches[StressPeriodIndex];
        end;
        if (Reaches.Count > 0) then
        begin
          StrCell := Reaches[0] as TStr_Cell;
          if ASegment.FHasTributariesForThisStressPeriod then
          begin
            StrCell.Flow := -1;
            StrCell.FlowAnnotation := Format(StrFlowSetTo1Becau,
              [ASegment.FScreenObject.Name]);
          end
          else if StrCell.Flow < 0 then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrOneOrMoreStreams,
              Format(StrInObject0sWith,
              [ASegment.FScreenObject.Name, ASegment.OriginalSegmentNumber]),
              ASegment.FScreenObject);
          end;
        end;
      end;
    end;

    CheckReachProperties;
    CheckReachSeparations;
    CheckStreamTributaries;
    CheckActiveReaches;


  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TStrWriter.Extension: string;
begin
  result := '.str';
end;

function TStrWriter.GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowStrBoundary;
end;

class function TStrWriter.ObservationExtension: string;
begin
  result := '.ob_stob';
end;

class function TStrWriter.ObservationOutputExtension: string;
begin
  result := '.stob_out';
end;

procedure TStrWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;

    frmProgressMM.AddMessage(StrWritingSTRPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets3and4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets5to10;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    //      Exit;
  finally
    CloseFile;
  end;
end;

function TStrWriter.ObservationPackage: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.StobPackage;
end;

function TStrWriter.ObsNameWarningString: string;
begin
  if Model.PestUsed then
  begin
    result := StrTheFollowingStreamPest;
  end
  else
  begin
    result := StrTheFollowingStream;
  end;
end;

//function TStrWriter.ObsTypeMF6: string;
//begin
//  result := '';
//  Assert(False);
//end;

function TStrWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.StrPackage;
end;

function TStrWriter.ParameterType: TParameterType;
begin
  result := ptSTR;
end;

function TStrWriter.FindSegment(OriginalSegmentNumber: integer): TStrSegment;
var
  Index: Integer;
  Segment: TStrSegment;
begin
  result := nil;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    if (Segment <> nil)
      and (Segment.OriginalSegmentNumber = OriginalSegmentNumber) then
    begin
      result := Segment;
      Exit;
    end;
  end;
end;

function TStrWriter.FindSegmentWithNewSegmentNumber(NewSegmentNumber: integer): TStrSegment;
var
  Index: Integer;
  Segment: TStrSegment;
begin
  result := nil;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    if (Segment <> nil)
      and (Segment.FNewSegmentNumber = NewSegmentNumber) then
    begin
      result := Segment;
      Exit;
    end;
  end;
end;

procedure TStrWriter.SortSegments;
var
  Comparer: TStrSegmentComparer;
  Error: string;
  Index: integer;
  Segment1: TStrSegment;
  Segment2: TStrSegment;
  Segment: TStrSegment;
  DownstreamIndex: Integer;
  SortedSegments: TObjectList<TStrSegment>;
  SegmentsFound: Boolean;
  OutIndex: integer;
  AnotherSegment: TStrSegment;
  OutOfOrder: Boolean;
  PSegments: TParameterSegments;
  PIndex: Integer;
  DownstreamParam: TParameterSegments;
  Changed: Boolean;
  SegIndex: Integer;
  SegmentNumbers: TGenericIntegerList;
  SegPostion: Integer;
  ParamIndex: Integer;
  AnotherPSegments: TParameterSegments;
  ParPosition: Integer;
  FirstSegment: TStrSegment;
  FParameterSegments: TStringList;
begin
  try
    FParameterSegments:= TStringList.Create;
    FParameterSegments.OwnsObjects := True;
    try
      // Create a TParameterSegments for each parameter that is in use.
      for Index := 0 to FSegments.Count - 1 do
      begin
        FSegments[Index].EvaluateLinks(Model);
        if FParameterSegments.IndexOf(FSegments[Index].FParameterName) < 0 then
        begin
          PSegments:= TParameterSegments.Create;
          PSegments.FParameterName := FSegments[Index].FParameterName;
          FParameterSegments.AddObject(PSegments.FParameterName, PSegments);
        end;
      end;

      if FParameterSegments.Count > 1 then
      begin
        // For each TParameterSegments find all TParameterSegments that are
        // tributaries of it.
        for Index := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[Index];
          for OutIndex := 0 to Segment.FDownstreamSegments.Count - 1 do
          begin
            AnotherSegment := FindSegment(Segment.FDownstreamSegments[OutIndex]);
            if AnotherSegment <> nil then
            begin
              if AnotherSegment.FParameterName <> Segment.FParameterName then
              begin
                PIndex := FParameterSegments.IndexOf(AnotherSegment.FParameterName);
                Assert(PIndex >= 0);
                DownstreamParam := FParameterSegments.Objects[PIndex] as TParameterSegments;
                if DownstreamParam.FUpStreamParameters.IndexOf(Segment.FParameterName) < 0 then
                begin
                  DownstreamParam.FUpStreamParameters.Add(Segment.FParameterName);
                end;
              end;
            end;
          end;
        end;

        // For each TParameterSegments file all TParameterSegments from which it
        // diverts flow.
        for Index := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[Index];
          for OutIndex := 0 to Segment.FDiversionSegments.Count - 1 do
          begin
            AnotherSegment := FindSegment(Segment.FDiversionSegments[OutIndex]);
            if AnotherSegment <> nil then
            begin
              if AnotherSegment.FParameterName <> Segment.FParameterName then
              begin
                PIndex := FParameterSegments.IndexOf(Segment.FParameterName);
                Assert(PIndex >= 0);
                DownstreamParam := FParameterSegments.Objects[PIndex] as TParameterSegments;
                if DownstreamParam.FUpStreamParameters.IndexOf(AnotherSegment.FParameterName) < 0 then
                begin
                  DownstreamParam.FUpStreamParameters.Add(AnotherSegment.FParameterName);
                end;
              end;
            end;
          end;
        end;

        // Assign FParameterNumber for each TStrSegment so that the segments
        // with the parameters that are furthest upstream  get the lowest numbers.
        PIndex := 0;
        repeat
          Changed := False;
          for Index := 0 to FParameterSegments.Count - 1 do
          begin
            PSegments := FParameterSegments.Objects[Index] as TParameterSegments;
            if PSegments.FUpStreamParameters.Count = 0 then
            begin
              for SegIndex := 0 to FSegments.Count - 1 do
              begin
                Segment := FSegments[SegIndex];
                if Segment.FParameterName = PSegments.FParameterName then
                begin
                  Segment.FParameterNumber := PIndex;
                end;
              end;
              Changed := True;

              for ParamIndex := 0 to FParameterSegments.Count - 1 do
              begin
                if ParamIndex <> Index then
                begin
                  AnotherPSegments := FParameterSegments.Objects[
                    ParamIndex] as TParameterSegments;
                  ParPosition := AnotherPSegments.FUpStreamParameters.IndexOf(
                    PSegments.FParameterName);
                  if ParPosition >= 0 then
                  begin
                    AnotherPSegments.FUpStreamParameters.Delete(ParPosition);
                  end;
                end;
              end;

              FParameterSegments.Delete(Index);
              Inc(PIndex);
              break;
            end;
          end;
          if not Changed and (FParameterSegments.Count > 0) then
          begin
            for Index := 0 to FParameterSegments.Count - 1 do
            begin
              PSegments := FParameterSegments.Objects[Index] as TParameterSegments;
              for SegIndex := 0 to FSegments.Count - 1 do
              begin
                Segment := FSegments[SegIndex];
                if Segment.FParameterName = PSegments.FParameterName then
                begin
                  Segment.FParameterNumber := PIndex;
                end;
              end;
              Inc(PIndex)
            end;
          end;
        until (not Changed) or (FParameterSegments.Count = 0);
      end;

    finally
      FParameterSegments.Free;
    end;

    // Sort by pararmeter number and then by original segment number.
    Comparer := TStrSegmentComparer.Create;
    try
      FSegments.Sort(Comparer);
    finally
      Comparer.Free;
    end;
    Error := '';

    // Check for duplicate original segment numbers.
    SegmentNumbers := TGenericIntegerList.Create;
    try
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment1 := FSegments[Index];
        SegPostion := SegmentNumbers.IndexOf(Segment1.OriginalSegmentNumber);
        if SegmentNumbers.IndexOf(Segment1.OriginalSegmentNumber) < 0 then
        begin
          SegmentNumbers.Add(SegPostion)
        end
        else
        begin
          Segment2 := FSegments[SegPostion];
          Error := Format(DupNumbersError, [Segment1.FScreenObject.Name,
            Segment2.FScreenObject.Name, Segment1.OriginalSegmentNumber]);
          frmErrorsAndWarnings.AddError(Model, DupErrorCategory, Error,
            Segment1.FScreenObject);
        end;
      end;
    finally
      SegmentNumbers.Free;
    end;

    if (Error = '') and (FSegments.Count > 0) then
    begin
      OutOfOrder := False;
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        if Segment.OriginalSegmentNumber <> Index+1 then
        begin
          OutOfOrder := True;
          break;
        end;
      end;
      if not OutOfOrder then
      begin
        // The segments are in numerical order.
        // Check for errors and leave the order unchanged.
        for Index := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[Index];
          for DownstreamIndex := 0 to Segment.FDownstreamSegments.Count - 1 do
          begin
            if Segment.FDownstreamSegments[DownstreamIndex] > FSegments.Count then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidDownstreamS, Format(StrTheSTRSegmentDefi,
                [Segment.FScreenObject.Name,
                Segment.FDownstreamSegments[DownstreamIndex]]),
                Segment.FScreenObject);
            end;
            if Segment.FDownstreamSegments[DownstreamIndex] =
              Segment.OriginalSegmentNumber then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrStreamsInTheSTRP,
                Format(StrTheSTRStreamSegem,
                [Segment.FScreenObject.Name,
                Segment.FDownstreamSegments[DownstreamIndex]]),
                Segment.FScreenObject);
            end;
            if Segment.FDownstreamSegments[DownstreamIndex] <
              Segment.OriginalSegmentNumber then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrSegmentsInTheSTR, Format(StrTheSTRStreamSegme,
                [Segment.FScreenObject.Name,
                Segment.FDownstreamSegments[DownstreamIndex],
                Segment.OriginalSegmentNumber]),
                Segment.FScreenObject);
            end;
          end;


          for DownstreamIndex := 0 to Segment.FDiversionSegments.Count - 1 do
          begin
            if Segment.FDiversionSegments[DownstreamIndex] > FSegments.Count then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidDiversionSe,
                Format(StrTheSTRSegmentDiversion,
                [Segment.FScreenObject.Name,
                Segment.FDiversionSegments[DownstreamIndex]]),
                Segment.FScreenObject);
            end;
            if Segment.FDiversionSegments[DownstreamIndex] =
              Segment.OriginalSegmentNumber then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrStreamsInTheSTRPDiversion,
                Format(StrTheSTRStreamSegmeDiversion,
                [Segment.FScreenObject.Name,
                Segment.FDiversionSegments[DownstreamIndex]]),
                Segment.FScreenObject);
            end;
            if Segment.FDiversionSegments[DownstreamIndex] >
              Segment.OriginalSegmentNumber then
            begin
              frmErrorsAndWarnings.AddError(Model,
                StrSegmentsInTheSTRDiversion,
                Format(StrTheSTRStreamSegmeDiver,
                [Segment.FScreenObject.Name,
                Segment.FDiversionSegments[DownstreamIndex],
                Segment.OriginalSegmentNumber]),
                Segment.FScreenObject);
            end;
          end;
        end;

        Exit;
      end;
    end;

    FSegments.OwnsObjects := False;

    SortedSegments := TObjectList<TStrSegment>.Create;
    SortedSegments.Capacity := FSegments.Count;

    repeat
      // Mark all segments in FSegments that have tributaries or have water
      // diverted from them by segments that are still in FSegments.

      // First set them all as not having tributaries.
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        Segment.FHasTributariesTemp := False;
      end;

      // Mark the ones that have tributaries.
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        for OutIndex := 0 to Segment.FDownstreamSegments.Count - 1 do
        begin
          AnotherSegment := FindSegment(Segment.FDownstreamSegments[OutIndex]);
          if AnotherSegment <> nil then
          begin
            AnotherSegment.FHasTributariesTemp := True;
          end;
        end;
      end;

      // mark the ones that have water diverted from them.
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        for OutIndex := 0 to Segment.FDiversionSegments.Count - 1 do
        begin
          AnotherSegment := FindSegment(Segment.FDiversionSegments[OutIndex]);
          if AnotherSegment <> nil then
          begin
            Segment.FHasTributariesTemp := True;
            break;
          end;
        end;
      end;

      SegmentsFound := False;
      if FSegments.Count > 0 then
      begin
        FirstSegment := FSegments[0];
        for Index := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[Index];
          if (not Segment.FHasTributariesTemp)
            and (Segment.FParameterNumber = FirstSegment.FParameterNumber) then
          begin
            SortedSegments.Add(Segment);
            FSegments[Index] := nil;
            SegmentsFound := True;
          end;
        end;
      end;

      if SegmentsFound then
      begin
        for Index := FSegments.Count - 1 downto 0 do
        begin
          if FSegments[Index] = nil then
          begin
            FSegments.Delete(Index)
          end;
        end;
      end;

    until (not SegmentsFound) or (FSegments.Count = 0);

    if FSegments.Count > 0 then
    begin
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        Error := Format(CircularError, [Segment.FScreenObject.Name,
          Segment.OriginalSegmentNumber]);
        frmErrorsAndWarnings.AddError(Model, CircularCategory, Error,
          Segment.FScreenObject);
        SortedSegments.Add(Segment)
      end;
    end;
    FSegments.OwnsObjects := False;
    FSegments.Free;

    FSegments := SortedSegments;
  finally
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      Segment.FNewSegmentNumber := Index+1;
      Segment.AssignSegmentAndReachNumbers;
    end;
  end;

end;

procedure TStrWriter.UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
var
  Index: integer;
  MfStrSegmentNumber: TModflowBoundaryDisplayTimeList;
  MfStrReachNumber: TModflowBoundaryDisplayTimeList;
  MfStrFlow: TModflowBoundaryDisplayTimeList;
  MfStrStage: TModflowBoundaryDisplayTimeList;
  MfStrConductance: TModflowBoundaryDisplayTimeList;
  MfStrBedBottomElevation: TModflowBoundaryDisplayTimeList;
  MfStrBedTopElevation: TModflowBoundaryDisplayTimeList;
  MfStrWidth: TModflowBoundaryDisplayTimeList;
  MfStrSlope: TModflowBoundaryDisplayTimeList;
  MfStrRoughness: TModflowBoundaryDisplayTimeList;
  ADisplayList: TModflowBoundaryDisplayTimeList;
  SegmentIndex: Integer;
//  Boundary: TStrBoundary;
  SegmentComment: string;
  ReachComment: string;
  DownstreamComment: string;
  DiversionComment: string;
  ParameterIndex: Integer;
  AParam: TModflowTransientListParameter;
  Segments: TList<TStrSegment>;
  TimeIndex: Integer;
  AList: TList;
  ReachList: TValueCellList;
  StressPeriods: TModflowStressPeriods;
  ASegment: TStrSegment;
  ReachIndex: Integer;
  TimeListIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  MfStrOutflowSegmentNumber: TModflowBoundaryDisplayTimeList;
  MfStrDiversionSegmentNumber: TModflowBoundaryDisplayTimeList;
  SegmentNumberDataArray: TModflowBoundaryDisplayDataArray;
  ReachNumberDataArray: TModflowBoundaryDisplayDataArray;
  OutflowSegmentNumberDataArray: TModflowBoundaryDisplayDataArray;
  DiversionSegmentNumberDataArray: TModflowBoundaryDisplayDataArray;
  FlowDataArray: TModflowBoundaryDisplayDataArray;
  StageDataArray: TModflowBoundaryDisplayDataArray;
  ConductanceDataArray: TModflowBoundaryDisplayDataArray;
  BedBottomElevationDataArray: TModflowBoundaryDisplayDataArray;
  BedTopElevationDataArray: TModflowBoundaryDisplayDataArray;
  WidthDataArray: TModflowBoundaryDisplayDataArray;
  SlopeDataArray: TModflowBoundaryDisplayDataArray;
  RoughnessDataArray: TModflowBoundaryDisplayDataArray;
  procedure AssignSegmentComments;
  begin
    SegmentComment := StrSegmentNumber + ASegment.FScreenObject.Name;
    ReachComment := StrReachNumber + ASegment.FScreenObject.Name;
    DownstreamComment := StrDownstreamSegmentNumber + ASegment.FScreenObject.Name;
    DiversionComment := StrDiversionSegmentNumber + ASegment.FScreenObject.Name;
  end;
  procedure AssignDataSets;
  begin
    SegmentNumberDataArray := MfStrSegmentNumber[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    ReachNumberDataArray := MfStrReachNumber[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    OutflowSegmentNumberDataArray := MfStrOutflowSegmentNumber[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    DiversionSegmentNumberDataArray := MfStrDiversionSegmentNumber[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    FlowDataArray := MfStrFlow[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    StageDataArray := MfStrStage[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    ConductanceDataArray := MfStrConductance[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    BedBottomElevationDataArray := MfStrBedBottomElevation[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    BedTopElevationDataArray := MfStrBedTopElevation[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    WidthDataArray := MfStrWidth[TimeIndex] as TModflowBoundaryDisplayDataArray;
    SlopeDataArray := MfStrSlope[TimeIndex] as TModflowBoundaryDisplayDataArray;
    RoughnessDataArray := MfStrRoughness[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
  end;
  procedure AssignReaches;
  var
    AReach: TStr_Cell;
  begin
    AReach := ReachList[ReachIndex] as TStr_Cell;
    SegmentNumberDataArray.AddDataValue(SegmentComment,
      ASegment.FNewSegmentNumber, AReach.Column, AReach.Row, AReach.Layer);
    ReachNumberDataArray.AddDataValue(ReachComment,
      AReach.ReachNumber, AReach.Column, AReach.Row, AReach.Layer);
    OutflowSegmentNumberDataArray.AddDataValue(DownstreamComment,
      ASegment.FOutFlowSegments[0], AReach.Column, AReach.Row, AReach.Layer);
    DiversionSegmentNumberDataArray.AddDataValue(DiversionComment,
      ASegment.FDiversions[0], AReach.Column, AReach.Row, AReach.Layer);
    FlowDataArray.AddDataValue(AReach.FlowAnnotation,
      AReach.Flow, AReach.Column, AReach.Row, AReach.Layer);
    StageDataArray.AddDataValue(AReach.StageAnnotation,
      AReach.Stage, AReach.Column, AReach.Row, AReach.Layer);
    if AParam <> nil then
    begin
      ConductanceDataArray.AddDataValue(AReach.ConductanceAnnotation
        + Format(StrMultipliedByTheP, [AParam.Value, AParam.ParameterName]),
        AReach.Conductance*AParam.Value, AReach.Column, AReach.Row, AReach.Layer);
    end
    else
    begin
      ConductanceDataArray.AddDataValue(AReach.ConductanceAnnotation,
        AReach.Conductance, AReach.Column, AReach.Row, AReach.Layer);
    end;
    BedBottomElevationDataArray.AddDataValue(AReach.BedBottomAnnotation,
      AReach.BedBottom, AReach.Column, AReach.Row, AReach.Layer);
    BedTopElevationDataArray.AddDataValue(AReach.BedTopAnnotation,
      AReach.BedTop, AReach.Column, AReach.Row, AReach.Layer);
    WidthDataArray.AddDataValue(AReach.WidthAnnotation,
      AReach.Width, AReach.Column, AReach.Row, AReach.Layer);
    SlopeDataArray.AddDataValue(AReach.SlopeAnnotation,
      AReach.Slope, AReach.Column, AReach.Row, AReach.Layer);
    RoughnessDataArray.AddDataValue(AReach.RoughnessAnnotation,
      AReach.Roughness, AReach.Column, AReach.Row, AReach.Layer);
  end;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    Evaluate;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    Assert(TimeLists.Count = 12);
    MfStrSegmentNumber := TimeLists[0];
    MfStrReachNumber := TimeLists[1];
    MfStrOutflowSegmentNumber := TimeLists[2];
    MfStrDiversionSegmentNumber := TimeLists[3];
    MfStrFlow := TimeLists[4];
    MfStrStage := TimeLists[5];
    MfStrConductance := TimeLists[6];
    MfStrBedBottomElevation := TimeLists[7];
    MfStrBedTopElevation := TimeLists[8];
    MfStrWidth := TimeLists[9];
    MfStrSlope := TimeLists[10];
    MfStrRoughness := TimeLists[11];

    // check that all the time lists contain the same number of times
    // as the first one.
    for Index := 1 to TimeLists.Count - 1 do
    begin
      ADisplayList := TimeLists[Index];
      Assert(MfStrSegmentNumber.Count = ADisplayList.Count);
    end;

    StressPeriods := Model.ModflowFullStressPeriods;

    if FParameters.Count > 0 then
    begin
      for ParameterIndex := 0 to FParameters.Count - 1 do
      begin
        AParam := FParameters[ParameterIndex];
        Segments := FParameterSegments[ParameterIndex];
        for TimeIndex := 0 to StressPeriods.Count - 1 do
        begin
          AssignDataSets;

          for SegmentIndex := 0 to Segments.Count - 1 do
          begin
            ASegment := Segments[SegmentIndex];

  //          Boundary := ASegment.FScreenObject.ModflowStrBoundary;

            AssignSegmentComments;

            Assert(ASegment.FParamValues.Count = 1);
            AList := ASegment.FParamValues.Objects[0] as TList;
            ReachList := AList[TimeIndex];
            for ReachIndex := 0 to ReachList.Count - 1 do
            begin
              AssignReaches;
            end;
          end;
        end;
      end;
    end
    else
    begin
      AParam := nil;
      for TimeIndex := 0 to StressPeriods.Count - 1 do
      begin
        AssignDataSets;
        for SegmentIndex := 0 to FSegments.Count - 1 do
        begin
          ASegment := FSegments[SegmentIndex];
  //        Boundary := ASegment.FScreenObject.ModflowStrBoundary;

          AssignSegmentComments;

          ReachList := ASegment.FReaches[TimeIndex];
          for ReachIndex := 0 to ReachList.Count - 1 do
          begin
            AssignReaches;
          end;
        end;
      end;
    end;



    // Mark all the data arrays and time lists as up to date.
    for TimeListIndex := 0 to TimeLists.Count - 1 do
    begin
      DisplayTimeList := TimeLists[TimeListIndex];
      for TimeIndex := 0 to DisplayTimeList.Count - 1 do
      begin
        DataArray := DisplayTimeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.UpToDate := True;
      end;
      DisplayTimeList.SetUpToDate(True);
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

end;

function TStrWriter.UseFixedFormat: Boolean;
begin
  result := Model.ModelSelection in [msModflowLGR, msModflowLGR2,msModflowCfp];
end;

procedure TStrWriter.EvaluateTributariesAndDiversions;
var
  Segment: TStrSegment;
  EndStressPeriod: Integer;
  Param: TModflowParamItem;
  StrItem: TStrItem;
  StressPeriods: TModflowStressPeriods;
  StrBoundary: TStrBoundary;
  StrValues: TStrCollection;
  StartStressPeriod: Integer;
  TribList: TGenericIntegerList;
  ItemIndex: Integer;
  SegmentIndex: Integer;
  DownStreamSegment: TStrSegment;
  SPIndex: Integer;
  UpStreamSegment: TStrSegment;
begin
  StressPeriods := Model.ModflowFullStressPeriods;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    StrBoundary := Segment.FScreenObject.ModflowStrBoundary;
    if StrBoundary.Parameters.Count > 0 then
    begin
      Assert(StrBoundary.Parameters.Count = 1);
      Assert(StrBoundary.Values.Count = 0);
      Param := StrBoundary.Parameters[0];
      StrValues := Param.Param as TStrCollection;
    end
    else
    begin
      StrValues := StrBoundary.Values as TStrCollection;
    end;
    for ItemIndex := 0 to StrValues.Count - 1 do
    begin
      EndStressPeriod := -1;
      StartStressPeriod := -1;
      StrItem := StrValues[ItemIndex] as TStrItem;
      if (StrItem.OutflowSegment > 0) or (StrItem.DiversionSegment > 0) then
      begin
        EndStressPeriod := StressPeriods.FindEndStressPeriod(StrItem.EndTime);
        if EndStressPeriod < 0 then
        begin
          Continue;
        end;
        StartStressPeriod := StressPeriods.FindStressPeriod(StrItem.StartTime);
        if (StartStressPeriod < 0) then
        begin
          StartStressPeriod := 0;
        end;
      end;
      if (StrItem.OutflowSegment > 0) then
      begin
        DownStreamSegment := FindSegment(StrItem.OutflowSegment);
        if DownStreamSegment = nil then
        begin
          frmErrorsAndWarnings.AddError(Model, StrInvalidOutflowSegm,
            Format(StrTheOutflowSegment, [StrItem.OutflowSegment,
            FSegments[SegmentIndex].FScreenObject.Name]),
            FSegments[SegmentIndex].FScreenObject);
        end
        else
        begin
          for SPIndex := StartStressPeriod to EndStressPeriod do
          begin
            TribList := DownStreamSegment.FTributaries[SPIndex];
            TribList.Add(Segment.FNewSegmentNumber);
            Segment.FOutFlowSegments[SPIndex] :=
              DownStreamSegment.FNewSegmentNumber;
          end;
        end;
      end;
      if StrItem.DiversionSegment > 0 then
      begin
        UpStreamSegment := FindSegment(StrItem.DiversionSegment);
        for SPIndex := StartStressPeriod to EndStressPeriod do
        begin
          Segment.FDiversions[SPIndex] := UpStreamSegment.FNewSegmentNumber;
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.AssociateParametersWithSegments;
var
  AList: Generics.Collections.TList<TStrSegment>;
  SegIndex: Integer;
  AParam: TModflowTransientListParameter;
  ParamIndex: Integer;
  Segment: TStrSegment;
begin
  // Associate parameters with segments.
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParamIndex];
    if AParam.ParameterType = ptSTR then
    begin
      AList := TList<TStrSegment>.Create;
      for SegIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegIndex];
        if (Segment.FParamValues.Count > 0) and (AParam.ParameterName = Segment.FParamValues[0]) then
        begin
          AList.Add(Segment);
        end;
      end;
      if AList.Count > 0 then
      begin
        FParameters.Add(AParam);
        FParameterSegments.Add(AList);
        FParameterInstanceNames.Add(TStringList.Create);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrSTRParameterHasNo,
          Format(StrNoReachesAreAssoc, [AParam.ParameterName]));
        AList.Free;
      end;
    end;
  end;
end;

procedure TStrWriter.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  StrReach: TStr_Cell;
  LocalLayer: Integer;
  DataArray: TDataArray;
begin
  StrReach := Cell as TStr_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(StrReach.Layer);

  if (StrReach.ConductancePest <> '')
    or (StrReach.StagePest <> '')
    or (StrReach.BedTopPest <> '')
    or (StrReach.BedBottomPest <> '')
    or (StrReach.FlowPest <> '')
    or (StrReach.WidthPest <> '')
    or (StrReach.SlopePest <> '')
    or (StrReach.RoughnessPest <> '')
    or (StrReach.ConductancePestSeriesName <> '')
    or (StrReach.StagePestSeriesName <> '')
    or (StrReach.BedTopPestSeriesName <> '')
    or (StrReach.BedBottomPestSeriesName <> '')
    or (StrReach.FlowPestSeriesName <> '')
    or (StrReach.WidthPestSeriesName <> '')
    or (StrReach.SlopePestSeriesName <> '')
    or (StrReach.RoughnessPestSeriesName <> '')
    then
  begin
    FPestParamUsed := True;
  end;


  if UseFixedFormat then
  begin
    WriteI5Integer(LocalLayer, Format('Layer Number %d', [LocalLayer]));
    WriteI5Integer(StrReach.Row+1, Format('Row Number %d', [StrReach.Row+1]));
    WriteI5Integer(StrReach.Column+1, Format('Column Number %d', [StrReach.Column+1]));
    WriteI5Integer(StrReach.SegmentNumber, Format('Segment Number %d', [StrReach.SegmentNumber]));
    WriteI5Integer(StrReach.ReachNumber, Format('Reach Number %d', [StrReach.ReachNumber]));
    if StrReach.ReachNumber = 1 then
    begin
      if Model.PestUsed and WritingTemplate and
        ((StrReach.FlowPest <> '') or (StrReach.FlowPestSeriesName <> '')) then
      begin
        WritePestTemplateFormula(StrReach.Flow, StrReach.FlowPest,
          StrReach.FlowPestSeriesName, StrReach.FlowPestSeriesMethod, StrReach);
      end
      else
      begin
        WriteF15Float(StrReach.Flow);
        if StrReach.FlowPest <> '' then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            StrReach.FlowPest);
          if DataArray <> nil then
          begin
            AddUsedPestDataArray(DataArray);
          end;
        end;
        if StrReach.FlowPestSeriesName <> '' then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            StrReach.FlowPestSeriesName);
          if DataArray <> nil then
          begin
            AddUsedPestDataArray(DataArray);
          end;
        end;
      end;

//      WriteF15Float(StrReach.Flow);
    end
    else
    begin
      WriteF15Float(0);
    end;

    if Model.PestUsed and WritingTemplate and
      ((StrReach.StagePest <> '') or (StrReach.StagePestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.Stage, StrReach.StagePest,
        StrReach.StagePestSeriesName, StrReach.StagePestSeriesMethod, StrReach);
    end
    else
    begin
      WriteF10Float(StrReach.Stage);
      if StrReach.StagePest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.StagePest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.StagePestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.StagePestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;

//    WriteF10Float(StrReach.Stage);

    if Model.PestUsed and WritingTemplate and
      ((StrReach.ConductancePest <> '') or (StrReach.ConductancePestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.Conductance, StrReach.ConductancePest,
        StrReach.ConductancePestSeriesName, StrReach.ConductancePestSeriesMethod, StrReach);
    end
    else
    begin
      WriteF10Float(StrReach.Conductance);
      if StrReach.ConductancePest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.ConductancePest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.ConductancePestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.ConductancePestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;

//    WriteF10Float(StrReach.Conductance);

    if Model.PestUsed and WritingTemplate and
      ((StrReach.BedBottomPest <> '') or (StrReach.BedBottomPestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.BedBottom, StrReach.BedBottomPest,
        StrReach.BedBottomPestSeriesName, StrReach.BedBottomPestSeriesMethod, StrReach);
    end
    else
    begin
      WriteF10Float(StrReach.BedBottom);
      if StrReach.BedBottomPest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedBottomPest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.BedBottomPestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedBottomPestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;

//    WriteF10Float(StrReach.BedBottom);

    if Model.PestUsed and WritingTemplate and
      ((StrReach.BedTopPest <> '') or (StrReach.BedTopPestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.BedTop, StrReach.BedTopPest,
        StrReach.BedTopPestSeriesName, StrReach.BedTopPestSeriesMethod, StrReach);
    end
    else
    begin
      WriteF10Float(StrReach.BedTop);
      if StrReach.BedTopPest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedTopPest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.BedTopPestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedTopPestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;

//    WriteF10Float(StrReach.BedTop);

    WriteIface(StrReach.IFace);
  end
  else
  begin
    WriteInteger(LocalLayer);
    WriteInteger(StrReach.Row+1);
    WriteInteger(StrReach.Column+1);
    WriteInteger(StrReach.SegmentNumber);
    WriteInteger(StrReach.ReachNumber);
    if StrReach.ReachNumber = 1 then
    begin
      if Model.PestUsed and WritingTemplate and
        ((StrReach.FlowPest <> '') or (StrReach.FlowPestSeriesName <> '')) then
      begin
        WritePestTemplateFormula(StrReach.Flow, StrReach.FlowPest,
          StrReach.FlowPestSeriesName, StrReach.FlowPestSeriesMethod, StrReach);
      end
      else
      begin
        WriteFloat(StrReach.Flow);
        if StrReach.FlowPest <> '' then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            StrReach.FlowPest);
          if DataArray <> nil then
          begin
            AddUsedPestDataArray(DataArray);
          end;
        end;
        if StrReach.FlowPestSeriesName <> '' then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(
            StrReach.FlowPestSeriesName);
          if DataArray <> nil then
          begin
            AddUsedPestDataArray(DataArray);
          end;
        end;
      end;
//      WriteFloat(StrReach.Flow);
    end
    else
    begin
      WriteFloat(0);
    end;

    if Model.PestUsed and WritingTemplate and
      ((StrReach.StagePest <> '') or (StrReach.StagePestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.Stage, StrReach.StagePest,
        StrReach.StagePestSeriesName, StrReach.StagePestSeriesMethod, StrReach);
    end
    else
    begin
      WriteFloat(StrReach.Stage);
      if StrReach.StagePest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.StagePest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.StagePestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.StagePestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;
//    WriteFloat(StrReach.Stage);
    if Model.PestUsed and WritingTemplate and
      ((StrReach.ConductancePest <> '') or (StrReach.ConductancePestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.Conductance, StrReach.ConductancePest,
        StrReach.ConductancePestSeriesName, StrReach.ConductancePestSeriesMethod, StrReach);
    end
    else
    begin
      WriteFloat(StrReach.Conductance);
      if StrReach.ConductancePest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.ConductancePest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.ConductancePestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.ConductancePestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;
//    WriteFloat(StrReach.Conductance);

    if Model.PestUsed and WritingTemplate and
      ((StrReach.BedBottomPest <> '') or (StrReach.BedBottomPestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.BedBottom, StrReach.BedBottomPest,
        StrReach.BedBottomPestSeriesName, StrReach.BedBottomPestSeriesMethod, StrReach);
    end
    else
    begin
      WriteFloat(StrReach.BedBottom);
      if StrReach.BedBottomPest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedBottomPest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.BedBottomPestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedBottomPestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;
//    WriteFloat(StrReach.BedBottom);

    if Model.PestUsed and WritingTemplate and
      ((StrReach.BedTopPest <> '') or (StrReach.BedTopPestSeriesName <> '')) then
    begin
      WritePestTemplateFormula(StrReach.BedTop, StrReach.BedTopPest,
        StrReach.BedTopPestSeriesName, StrReach.BedTopPestSeriesMethod, StrReach);
    end
    else
    begin
      WriteFloat(StrReach.BedTop);
      if StrReach.BedTopPest <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedTopPest);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if StrReach.BedTopPestSeriesName <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(
          StrReach.BedTopPestSeriesName);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;
//    WriteFloat(StrReach.BedTop);
    WriteIface(StrReach.IFace);
  end;
  WriteString(' # ' + DataSetIdentifier +
    ' Layer Row Col Seg Reach Flow Stage Condfact Sbot Stop, IFACE');

  NewLine;

//  Layer Row Col Seg Reach Flow Stage Condfact Sbot Stop] [xyz]
end;

procedure TStrWriter.WriteDataSet1;
var
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  NPSTR: Integer;
  MXL: Integer;
  AList: TList;
  TimeIndex: Integer;
  ValueCellList: TValueCellList;
  NumReaches: Integer;
begin
  if FParameterSegments.Count > 0 then
  begin
    NPSTR := FParameterSegments.Count;
    MXL := 0;
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      NumReaches := 0;
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        AList := ASegment.FParamValues.Objects[0] as TList;
        for TimeIndex := 0 to AList.Count - 1 do
        begin
          ValueCellList := AList[TimeIndex];
          if NumReaches < ValueCellList.Count then
          begin
            NumReaches := ValueCellList.Count
          end;
        end;
      end;
      MXL := MXL + NumReaches * Model.ModflowFullStressPeriods.Count;
    end;
    WriteString('PARAMETER');
    WriteInteger(NPSTR);
    WriteInteger(MXL);
    WriteString(' # Data Set 1: PARAMETER NPSTR MXL');
    NewLine;
  end;
end;

procedure TStrWriter.WriteDataSet10(StressPeriodIndex: Integer);
var
  SegmentIndex: Integer;
  ASegment: TStrSegment;
begin
  if NDIV > 0 then
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if UseFixedFormat then
      begin
        WriteI10Integer(ASegment.FDiversions[StressPeriodIndex], Format('Diversion number %d', [ASegment.FDiversions[StressPeriodIndex]]));
      end
      else      
      begin      
        WriteInteger(ASegment.FDiversions[StressPeriodIndex]);
      end;            
      WriteString(' # Data Set 10: Iupseg');
      NewLine;
    end;
  end;
end;

class function TStrWriter.ComputeStreamConstant(Model: TCustomModel): double;
begin
  result := 1;
  case Model.ModflowOptions.LengthUnit of
    0:
      begin
        frmErrorsAndWarnings.AddError(Model, StrErrorInStreamPackLength,
          StrTheLengthUnitsFor);
      end;
    1: // feet
      begin
        result := 1/0.3048;
      end;
    2: // m
      begin
      end;
    3: // cm
      begin
        result := 1/100;
      end;
  else Assert(False);
  end;
  if result <> 1 then
  begin
    result := Power(result, 1/3);
  end;
  case Model.ModflowOptions.TimeUnit of
    0: // Undefined
      begin
        frmErrorsAndWarnings.AddError(Model, StrErrorInStreamPackTime,
          StrTheTimeUnitsForT);
      end;
    1: // Seconds
      begin
      end;
    2: // Minutes
      begin
        result := result * 60;
      end;
    3: // Hours
      begin
        result := result * 60 * 60;
      end;
    4: // Days
      begin
        result := result * 60 * 60 * 24;
      end;
    5: // Years
      begin
        result := result * 60 * 60 * 24 * 365.25;
      end;
  else Assert(False);
  end;
end;

procedure TStrWriter.WriteDataSet2;
var
  NSS: Integer;
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  TimeIndex: Integer;
  Tributaries: TGenericIntegerList;
  List: TList;
  ValueCellList: TValueCellList;
  MXACTS: Integer;
  ICALC: Integer;
  StrConst: double;
  ISTCB1: Integer;
  ISTCB2: Integer;
  Option: string;
begin
  MXACTS := 0;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ASegment.FParamValues.Count > 0 then
    begin
      List := ASegment.FParamValues.Objects[0] as TList;
    end
    else
    begin
      List := ASegment.FReaches;
    end;
    ValueCellList := List[0];
    MXACTS := MXACTS + ValueCellList.Count;
  end;

  if FParameterSegments.Count > 0 then
  begin
    NSS := 0;
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        Inc(NSS);
      end;
    end;
  end
  else
  begin
    NSS := FSegments.Count;
  end;

  NTRIB := 0;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    for TimeIndex := 0 to ASegment.FTributaries.Count - 1 do
    begin
      Tributaries := ASegment.FTributaries[TimeIndex];
      if Tributaries.Count > NTRIB then
      begin
        NTRIB := Tributaries.Count;
      end;
    end;
  end;

  NDIV := 0;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ASegment.FDiversionSegments.Count > 0 then
    begin
      NDIV := 1;
      break;
    end;
  end;

  if Model.ModflowPackages.StrPackage.CalculateStage then
  begin
    ICALC := 1;
  end
  else
  begin
    ICALC := 0;
  end;

  StrConst := ComputeStreamConstant(Model);

  GetFlowUnitNumber(ISTCB1);

  ISTCB2 := -1;
  case Model.ModflowOutputControl.SaveCellFlows of
    csfNone, csfListing:
      begin
        ISTCB2 := 0;
      end;
    csfBinary:
      begin
        ISTCB2 := Model.UnitNumbers.UnitNumber(StrSTR_OUT);
        if not WritingTemplate then
        begin
          WriteToNameFile(StrDATABINARY,
                    ISTCB2, ChangeFileExt(FNameOfFile,'.str_flow'), foOutput, Model);
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;

  GetOption(Option);

  if UseFixedFormat then
  begin
    WriteI10Integer(MXACTS, 'MXACTS in data set 2');
    WriteI10Integer(NSS, 'NSS in data set 2');
    WriteI10Integer(NTRIB, 'NTRIB in data set 2');
    WriteI10Integer(NDIV, 'NDIV in data set 2');
    WriteI10Integer(ICALC, 'ICALC in data set 2');
    WriteF10Float(StrConst);
    WriteI10Integer(ISTCB1, 'ISTCB1 in data set 2');
    WriteI10Integer(ISTCB2, 'ISTCB2 in data set 2');
    WriteString(Option);
  end
  else
  begin
    WriteInteger(MXACTS);
    WriteInteger(NSS);
    WriteInteger(NTRIB);
    WriteInteger(NDIV);
    WriteInteger(ICALC);
    WriteFloat(StrConst);
    WriteInteger(ISTCB1);
    WriteInteger(ISTCB2);
    WriteString(Option);
  end;
  WriteString(' # Data Set 2: MXACTS NSS NTRIB NDIV  ICALC CONST ISTCB1 ISTCB2 Option');
  NewLine;
end;

procedure TStrWriter.WriteDataSet5(StressPeriodIndex: Integer);
var
  SegmentIndex: Integer;
  ReachList: TValueCellList;
  ITMP: Integer;
  ASegment: TStrSegment;
  IRDFLG: Integer;
  IPTFLG: Integer;
begin
  if FParameters.Count > 0 then
  begin
    ITMP := FParameters.Count;
  end
  else
  begin
    ITMP := 0;
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      ReachList := ASegment.FReaches[StressPeriodIndex];
      ITMP := ITMP + ReachList.Count;
    end;
  end;

  if Model.ModflowOutputControl.PrintInputCellLists then
  begin
    IRDFLG := 0;
    IPTFLG := 0;
  end
  else
  begin
    IRDFLG := 1;
    IPTFLG := 1;
  end;

  if UseFixedFormat then
  begin  
    WriteI10Integer(ITMP, 'ITMP in data set 5');
    WriteI10Integer(IRDFLG, 'IRDFLG in data set 5');
    WriteI10Integer(IPTFLG, 'IPTFLG in data set 5');
  end    
  else  
  begin  
    WriteInteger(ITMP);
    WriteInteger(IRDFLG);
    WriteInteger(IPTFLG);
  end;    
  WriteString(' # Data Set 5: ITMP IRDFLG IPTFLG');
  NewLine;

end;

procedure TStrWriter.WriteDataSet6(StressPeriodIndex: Integer);
var
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  Reaches: TValueCellList;
  ReachIndex: Integer;
//  StrCell: TStr_Cell;
//  OutflowSegmentNumber: Integer;
//  DownstreamSegment: TStrSegment;
begin
  if FParameters.Count = 0 then
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      Reaches := ASegment.FReaches[StressPeriodIndex];
      for ReachIndex := 0 to Reaches.Count - 1 do
      begin
        WriteCell(Reaches[ReachIndex], 'Data set 6:', '');
      end;
    end;
  end;
end;

procedure TStrWriter.WriteDataSet7(StressPeriodIndex: Integer);
var
  ParameterIndex: Integer;
  AParameter: TModflowTransientListParameter;
  InstanceNames: TStringList;
  Pname: string;
  Iname: string;
begin
  if FParameters.Count > 0 then
  begin
    for ParameterIndex := 0 to FParameters.Count - 1 do
    begin
      AParameter := FParameters[ParameterIndex];
      InstanceNames :=  FParameterInstanceNames[ParameterIndex];

      Pname := AParameter.ParameterName;
      if InstanceNames.Count > 0 then
      begin
        Iname := InstanceNames[StressPeriodIndex];
      end
      else
      begin
        Iname := '';
      end;
      WriteString(Pname);
      if Iname <> '' then
      begin
        WriteString(' ');
        WriteString(Iname);
      end;
      WriteString(' # Data Set 7: Pname');
      if Iname <> '' then
      begin
        WriteString(' Iname');
      end;
      NewLine;
    end;
  end;
end;

procedure TStrWriter.WriteDataSet8(StressPeriodIndex: Integer);
var
  StrPackage: TStrPackageSelection;
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  AList: TList;
  ReachIndex: Integer;
  AReach: TStr_Cell;
  Reaches: TValueCellList;
  DataArray: TDataArray;
begin
  StrPackage := Model.ModflowPackages.StrPackage;
  if StrPackage.CalculateStage then
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        Assert(ASegment.FParamValues.Count = 1);
        AList := ASegment.FParamValues.Objects[0] as TList;
      end
      else
      begin
        AList := ASegment.FReaches;
      end;
      Reaches := AList[StressPeriodIndex];
      for ReachIndex := 0 to Reaches.Count - 1 do
      begin
        AReach := Reaches[ReachIndex] as TStr_Cell;
        if UseFixedFormat then
        begin
          if Model.PestUsed and WritingTemplate and
            ((AReach.WidthPest <> '') or (AReach.WidthPestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Width, AReach.WidthPest,
              AReach.WidthPestSeriesName, AReach.WidthPestSeriesMethod, AReach);
          end
          else
          begin
            WriteF10Float(AReach.Width);
            if AReach.WidthPest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.WidthPest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.WidthPestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.WidthPestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;
//          WriteF10Float(AReach.Width);

          if Model.PestUsed and WritingTemplate and
            ((AReach.SlopePest <> '') or (AReach.SlopePestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Slope, AReach.SlopePest,
              AReach.SlopePestSeriesName, AReach.SlopePestSeriesMethod, AReach);
          end
          else
          begin
            WriteF10Float(AReach.Slope);
            if AReach.SlopePest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.SlopePest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.SlopePestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.SlopePestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;

//          WriteF10Float(AReach.Slope);

          if Model.PestUsed and WritingTemplate and
            ((AReach.RoughnessPest <> '') or (AReach.RoughnessPestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Roughness, AReach.RoughnessPest,
              AReach.RoughnessPestSeriesName, AReach.RoughnessPestSeriesMethod, AReach);
          end
          else
          begin
            WriteF10Float(AReach.Roughness);
            if AReach.RoughnessPest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.RoughnessPest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.RoughnessPestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.RoughnessPestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;

//          WriteF10Float(AReach.Roughness);
        end
        else        
        begin        
          if Model.PestUsed and WritingTemplate and
            ((AReach.WidthPest <> '') or (AReach.WidthPestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Width, AReach.WidthPest,
              AReach.WidthPestSeriesName, AReach.WidthPestSeriesMethod, AReach);
          end
          else
          begin
            WriteFloat(AReach.Width);
            if AReach.WidthPest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.WidthPest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.WidthPestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.WidthPestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;
//          WriteFloat(AReach.Width);

          if Model.PestUsed and WritingTemplate and
            ((AReach.SlopePest <> '') or (AReach.SlopePestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Slope, AReach.SlopePest,
              AReach.SlopePestSeriesName, AReach.SlopePestSeriesMethod, AReach);
          end
          else
          begin
            WriteFloat(AReach.Slope);
            if AReach.SlopePest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.SlopePest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.SlopePestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.SlopePestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;

//          WriteFloat(AReach.Slope);

          if Model.PestUsed and WritingTemplate and
            ((AReach.RoughnessPest <> '') or (AReach.RoughnessPestSeriesName <> '')) then
          begin
            WritePestTemplateFormula(AReach.Roughness, AReach.RoughnessPest,
              AReach.RoughnessPestSeriesName, AReach.RoughnessPestSeriesMethod, AReach);
          end
          else
          begin
            WriteFloat(AReach.Roughness);
            if AReach.RoughnessPest <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.RoughnessPest);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
            if AReach.RoughnessPestSeriesName <> '' then
            begin
              DataArray := Model.DataArrayManager.GetDataSetByName(
                AReach.RoughnessPestSeriesName);
              if DataArray <> nil then
              begin
                AddUsedPestDataArray(DataArray);
              end;
            end;
          end;

//          WriteFloat(AReach.Roughness);
        end;
        WriteString(' # Data Set 8: Width Slope Rough');
        NewLine;
      end;
    end;
  end;
end;

procedure TStrWriter.WriteDataSet9(StressPeriodIndex: Integer);
var
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  TribIndex: Integer;
  Tributaries: TGenericIntegerList;
begin
  if NTRIB > 0 then
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      Tributaries := ASegment.FTributaries[StressPeriodIndex];
      for TribIndex := 0 to Tributaries.Count - 1 do
      begin
        if UseFixedFormat then
        begin        
          WriteI5Integer(Tributaries[TribIndex], Format('Tributary %d', [Tributaries[TribIndex]]));
        end          
        else        
        begin        
          WriteInteger(Tributaries[TribIndex]);
        end;                
      end;
      for TribIndex := Tributaries.Count to NTRIB - 1 do
      begin
        if UseFixedFormat then
        begin        
          WriteI5Integer(0, 'Tributary 0');
        end          
        else        
        begin        
          WriteInteger(0);
        end;                
      end;
      WriteString(' # Data Set 9: Itrib(NTRIB)');
      NewLine;
    end;
  end;
end;

procedure TStrWriter.WriteDataSets3and4;
const
  PARTYP = ' STR';
var
  ParameterIndex: Integer;
  AParam: TModflowTransientListParameter;
  Segments: TList<TStrSegment>;
  PARNAM: string;
  Parval: Double;
  SegmentIndex: Integer;
  ASegment: TStrSegment;
  ReachList: TValueCellList;
  AList: TList;
  NLST: Integer;
  StressPeriods: TModflowStressPeriods;
  TimeIndex: Integer;
  ReachIndex: Integer;
  MaxStressPeriodString: string;
  TerminalString: string;
  InstanceRoot: string;
  InstanceName: string;
  StressPeriodString: string;
  InstanceNames: TStringList;
begin
  StressPeriods := Model.ModflowFullStressPeriods;
  Assert(FParameters.Count = FParameterSegments.Count);
  Assert(FParameters.Count = FParameterInstanceNames.Count);
  for ParameterIndex := 0 to FParameters.Count - 1 do
  begin
    AParam := FParameters[ParameterIndex];
    Segments := FParameterSegments[ParameterIndex];

    // Data set 3
    PARNAM := AParam.ParameterName;
    Parval := AParam.Value;
    NLST := 0;
    for SegmentIndex := 0 to Segments.Count - 1 do
    begin
      ASegment := Segments[SegmentIndex];
      Assert(ASegment.FParamValues.Count = 1);
      AList := ASegment.FParamValues.Objects[0] as TList;
      ReachList := AList[0];
      NLST := NLST + ReachList.Count;
    end;

    WriteString(PARNAM);
    WriteString(PARTYP);
    WriteFloat(Parval);
    WriteInteger(NLST);
    if StressPeriods.Count > 1 then
    begin
      WriteString(' INSTANCES');
      WriteInteger(StressPeriods.Count);
    end;
    WriteString(' # Data Set 3: PARNAM PARTYP Parval NLST');
    if StressPeriods.Count > 1 then
    begin
      WriteString(' INSTANCES NUMINST');
    end;
    NewLine;

    // Data set 4

    MaxStressPeriodString := IntToStr(StressPeriods.Count);
    TerminalString := '_';

    InstanceRoot := PARNAM;
    InstanceName := InstanceRoot + TerminalString + MaxStressPeriodString;
    While(Length(InstanceName)) > 10 do
    begin
      SetLength(InstanceRoot, Length(InstanceRoot)-1);
      InstanceName := InstanceRoot + TerminalString + MaxStressPeriodString;
    end;
    InstanceRoot := InstanceRoot + TerminalString;

    for TimeIndex := 0 to StressPeriods.Count - 1 do
    begin
      // Data set 4a
      if StressPeriods.Count > 1 then
      begin
        StressPeriodString := IntToStr(TimeIndex+1);
        while Length(StressPeriodString) < Length(MaxStressPeriodString) do
        begin
          StressPeriodString := '0' + StressPeriodString;
        end;
        InstanceName := InstanceRoot + StressPeriodString;
        InstanceNames := FParameterInstanceNames[ParameterIndex];
        InstanceNames.Add(InstanceName);

        WriteString(InstanceName);
        WriteString(' # Data Set 4a: INSTNAM');
        NewLine;
      end;

      // Data set 4b
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        Assert(ASegment.FParamValues.Count = 1);
        AList := ASegment.FParamValues.Objects[0] as TList;
        ReachList := AList[TimeIndex];
        for ReachIndex := 0 to ReachList.Count - 1 do
        begin
          WriteCell(ReachList[ReachIndex], 'Data set 4b:', '');
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.WriteDataSets5to10;
var
  StressPeriodIndex: Integer;
  StressPeriods: TModflowStressPeriods;
begin
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    WriteDataSet5(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteDataSet6(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteDataSet7(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteDataSet8(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteDataSet9(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteDataSet10(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  end;

end;

procedure TStrWriter.WriteFile(const AFileName: string);
//  var StartUnitNumber: integer; Lines: TStrings);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSTR) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(StrSTR, Model.UnitNumbers.UnitNumber(StrSTR), FNameOfFile, foInput, Model);
//  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  Evaluate;

  FPestParamUsed := False;
  WriteFileInternal;

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;

end;

function TStrWriter.GetSegFromObject(AScreenObject: TScreenObject): TStrSegment;
var
  SegmentIndex: Integer;
  ASegment: TStrSegment;
begin
  result := nil;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ASegment.FScreenObject = AScreenObject then
    begin
      Result := ASegment;
      Exit;
    end;
  end;
end;

procedure TStrWriter.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
var
  FluxObsCountWarning: string;
  Observations: TFluxObservationGroups;
  ObsIndex: Integer;
  ObservationGroup: TFluxObservationGroup;
  NameOfFile: string;
  IUSTOBSV: Integer;
  OutputName: string;
  AllSegments: TList<TStrSegment>;
  ScreenObjectIndex: Integer;
  ObsFactor: TObservationFactor;
  NQOBST: Integer;
  NQCLST: Integer;
  ASegment: TStrSegment;
  ReachList: TValueCellList;
  AList: TList;
  DataSet4: TStringList;
  DataSet5: TStringList;
  ObsFile: TStringList;
  DataSet1: string;
  PrintObservations: Boolean;
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  // if the package is not selected, quit.
  if not ObservationPackage.IsSelected then
  begin
    Exit;
  end;
  // If the file has been generated externally, quit.
  if Model.PackageGeneratedExternally(StrSTOB) then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, ObsNameWarningString);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSTRObservat);

    FluxObsCountWarning := Format(StrInSNoFlowObser,
      [ObservationPackage.PackageIdentifier]);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, FluxObsCountWarning);

    // count the number of cell groups for which flux observations are listed
    NQST := 0;
    Observations := Model.StreamObservations;
    for ObsIndex := 0 to Observations.Count - 1 do
    begin
      ObservationGroup := Observations[ObsIndex];
      if Purpose = ObservationGroup.Purpose then
      begin
        Inc(NQST);
      end;
    end;

    if (Observations.Count > 0) and (NQST = 0) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, FluxObsCountWarning,
        StrSelectModelObserv);
    end;

    NQCST := 0;
    NQTST := 0;

    NameOfFile := ObservationFileName(AFileName);
    if not WritingTemplate then
    begin
      WriteToNameFile(StrSTOB, Model.UnitNumbers.
            UnitNumber(StrSTOB), NameOfFile, foInput, Model);
    end;

    IUSTOBSV := Model.UnitNumbers.UnitNumber(StrIUSTOBSV);
    OutputName := ObservationOutputFileName(AFileName);
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATA, IUSTOBSV, OutputName, foOutput, Model);
    end;

    if Model.PestUsed then
    begin
      Model.FileNameLines.Add(Format(' STOB %s',
        [ExtractFileName(OutputName)]));
    end;


    AllSegments := TList<TStrSegment>.Create;
    ObsFile := TStringList.Create;
    try
      for ObsIndex := 0 to Observations.Count - 1 do
      begin
        ObservationGroup := Observations[ObsIndex];
        if Purpose = ObservationGroup.Purpose then
        begin
          AllSegments.Clear;
          DataSet4 := TStringList.Create;
          try
            WriteObservationDataSet4(ObservationGroup, DataSet4);
            NQOBST := DataSet4.Count;
            NQTST := NQTST + NQOBST;

            DataSet5 := TStringList.Create;
            try
              for ScreenObjectIndex := 0 to ObservationGroup.ObservationFactors.Count - 1 do
              begin
                ObsFactor := ObservationGroup.ObservationFactors[ScreenObjectIndex];
                Assert(ObsFactor.ScreenObject <> nil);
                ASegment := GetSegFromObject(ObsFactor.ScreenObject as TScreenObject);
                if ASegment = nil then
                begin
                  AScreenObject := ObsFactor.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model, StrInvalidSTRObservat,
                    Format(StrTheObject0sIsL,
                    [AScreenObject.Name, ObservationGroup.ObservationName]),
                    AScreenObject);
                  Continue;
                end;
                Assert(ASegment <> nil);
                AllSegments.Add(ASegment);
                if ASegment.FParamValues.Count > 0 then
                begin
                  AList := ASegment.FParamValues.Objects[0] as TList;
                  ReachList := AList[0];
                end
                else
                begin
                  ReachList := ASegment.FReaches[0];
                end;
                WriteDataSet5ForOneScreenObject(ObsFactor, DataSet5,ReachList);
              end;
              NQCLST := DataSet5.Count;
              NQCST := NQCST + NQCLST;
              ObsFile.Add(IntToStr(NQOBST) + ' ' + IntToStr(NQCLST)
                + ' # Data Set 3: NQOBST NQCLST');
              ObsFile.AddStrings(DataSet4);
              ObsFile.AddStrings(DataSet5);
            finally
              DataSet5.Free;
            end;
          finally
            DataSet4.Free;
          end;
        end;
      end;

      DataSet1 := IntToStr(NQST) + ' ' + IntToStr(NQCST) + ' '
        + IntToStr(NQTST) + ' ' + IntToStr(IUSTOBSV);

      PrintObservations := Model.ModflowOutputControl.PrintObservations;
      if not PrintObservations then
      begin
        DataSet1 := DataSet1 +' NOPRINT';
      end;
      DataSet1 := DataSet1 + ' # Data Set 1: NQST NQCST NQTST IUSTOBSV';
      if not PrintObservations then
      begin
        DataSet1 := DataSet1 +' NOPRINT';
      end;
      ObsFile.Insert(0,DataSet1);
      ObsFile.Insert(1, '1' + ' # Data Set 2: TOMULTST');
      for Index := ObservationPackage.Comments.Count - 1 downto 0 do
      begin
        ObsFile.Insert(0, '# ' + ObservationPackage.Comments[Index]);
      end;
      ObsFile.Insert(0, '# ' + PackageID_Comment(ObservationPackage));
      ObsFile.SaveToFile(NameOfFile);

      SavePestInstructionFile(OutputName);

    finally
      AllSegments.Free;
      ObsFile.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TStrWriter.CheckStreamTributaries;
const
  DownstreamEpsilon = 1E-8;
var
  Tributaries: TGenericIntegerList;
  TribReachCell: TStr_Cell;
  TribIndex: Integer;
  ATributary: TStrSegment;
  OutflowReachCell: TStr_Cell;
  DeltaCell: Integer;
  TribReaches: TValueCellList;
//  OutflowReach: TValueCell;
//  TribReach: TValueCell;
  StressPeriods: TModflowStressPeriods;
  ASegment: TStrSegment;
  Reaches: TValueCellList;
  AList: TList;
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
begin
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        AList := ASegment.FParamValues.Objects[0] as TList;
        Reaches := AList[StressPeriodIndex];
      end
      else
      begin
        Reaches := ASegment.FReaches[StressPeriodIndex];
      end;
      if Reaches.Count = 0 then
      begin
        Continue;
      end;
      OutflowReachCell := Reaches.First as TStr_Cell;
      Tributaries := ASegment.FTributaries[StressPeriodIndex];
      for TribIndex := 0 to Tributaries.Count - 1 do
      begin
        ATributary := FSegments[Tributaries[TribIndex] - 1];
        if ATributary.FParamValues.Count > 0 then
        begin
          AList := ATributary.FParamValues.Objects[0] as TList;
          TribReaches := AList[StressPeriodIndex];
        end
        else
        begin
          TribReaches := ATributary.FReaches[StressPeriodIndex];
        end;
        if TribReaches.Count > 0 then
        begin
          TribReachCell := TribReaches.Last as TStr_Cell;
          DeltaCell := Max(Abs(OutflowReachCell.Row - TribReachCell.Row),
            Abs(OutflowReachCell.Column - TribReachCell.Column));
          if DeltaCell > 1 then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrTributaryWarning,
              Format(StrTributarySegment0, [TribReachCell.SegmentNumber,
              OutflowReachCell.SegmentNumber, StressPeriodIndex + 1, DeltaCell]),
              ATributary.FScreenObject);
          end;
          if OutflowReachCell.Stage > TribReachCell.Stage + DownstreamEpsilon then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrTribStageWarning,
              Format(StrTributaryObject0,
              [ATributary.FScreenObject.Name, ASegment.FScreenObject.Name,
              OutflowReachCell.Stage - TribReachCell.Stage]),
              ATributary.FScreenObject);
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrTheFollowingObjectBadTribs,
            ATributary.ScreenObject.Name,ATributary.ScreenObject);
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.CheckActiveReaches;
var
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
  ReachIndex: Integer;
  AReach: TValueCell;
  ActiveDataSet: TDataArray;
  StrReach: TStr_Cell;
  ASegment: TStrSegment;
  AList: TList;
  Reaches: TValueCellList;
begin
  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataSet <> nil);
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        AList := ASegment.FParamValues.Objects[0] as TList;
        Reaches := AList[StressPeriodIndex];
      end
      else
      begin
        Reaches := ASegment.FReaches[StressPeriodIndex];
      end;
      for ReachIndex := 0 to Reaches.Count - 1 do
      begin
        AReach := Reaches[ReachIndex];
        if not ActiveDataSet.BooleanData[
          AReach.Layer, AReach.Row, AReach.Column] then
        begin
          StrReach := AReach as TStr_Cell;

          frmErrorsAndWarnings.AddWarning(Model, StrOneOrMoreReaches,
            Format(StrInactiveReachCells,
            [StressPeriodIndex+1, StrReach.SegmentNumber, StrReach.ReachNumber,
            StrReach.Layer+1, StrReach.Row+1, StrReach.Column+1]),
            ASegment.FScreenObject);
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.CheckReachProperties;
const
  DownhillEpsilon = 1E-8;
var
  ReachIndex: Integer;
//  DeltaCell: Integer;
  AReach: TStr_Cell;
  PriorReach: TStr_Cell;
//  StrReach: TStr_Cell;
  Reaches: TValueCellList;
  ASegment: TStrSegment;
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
  AList: TList;
  StressPeriods: TModflowStressPeriods;
  CalculateStage: Boolean;
//  Layer: Integer;
  ScreenObjectName: TComponentName;
  ParamValue: Extended;
  ParamIndex: Integer;
  procedure CheckReach(AReach: TStr_Cell);
  const
    HighConductanceContrast = 1e6;
  var
    ScreenObjectName: TComponentName;
    AqCond: Double;
    Ratio: Extended;
    ScreenObject: TScreenObject;
  begin
    if not Model.ModflowPackages.StrPackage.CalculateStage
      and (AReach.Stage < AReach.BedTop) then
    begin
      ScreenObjectName := ASegment.FScreenObject.Name;
      frmErrorsAndWarnings.AddWarning(Model, StrStageTopWarning,
        Format(StrDownhillObjectWarning,
        [ScreenObjectName, AReach.ReachNumber,
        AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
        AReach.Stage - AReach.BedTop]),
        ASegment.FScreenObject);
    end;
    if AReach.BedTop < AReach.BedBottom then
    begin
      ScreenObjectName := ASegment.FScreenObject.Name;
      frmErrorsAndWarnings.AddWarning(Model, StrStreamTopBottomWarning,
        Format(StrDownhillObjectWarning,
        [ScreenObjectName, AReach.ReachNumber,
        AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
        AReach.BedTop - AReach.BedBottom]),
        ASegment.FScreenObject);
    end;
    if CalculateStage then
    begin
      if AReach.Width <= 0 then
      begin
        ScreenObjectName := ASegment.FScreenObject.Name;
        frmErrorsAndWarnings.AddWarning(Model, StrWidthWarning,
          Format(StrDownhillObjectWarning,
          [ScreenObjectName, AReach.ReachNumber,
          AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
          AReach.Width]),
          ASegment.FScreenObject);
      end;
      if AReach.Slope <= 0 then
      begin
        ScreenObjectName := ASegment.FScreenObject.Name;
        frmErrorsAndWarnings.AddWarning(Model, StrSlopeWarning,
          Format(StrDownhillObjectWarning,
          [ScreenObjectName, AReach.ReachNumber,
          AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
          AReach.Slope]),
          ASegment.FScreenObject);
      end;
      if AReach.Slope > HighGradient then
      begin
        ScreenObjectName := ASegment.FScreenObject.Name;
        frmErrorsAndWarnings.AddWarning(Model, StrHighSlopeWarning,
          Format(StrDownhillObjectWarning,
          [ScreenObjectName, AReach.ReachNumber,
          AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
          AReach.Slope]),
          ASegment.FScreenObject);
      end;
      if AReach.Roughness <= 0 then
      begin
        ScreenObjectName := ASegment.FScreenObject.Name;
        frmErrorsAndWarnings.AddWarning(Model, StrRoughnessWarning,
          Format(StrDownhillObjectWarning,
          [ScreenObjectName, AReach.ReachNumber,
          AReach.Layer+1, AReach.Row+1, AReach.Column+1, StressPeriodIndex+1,
          AReach.Roughness]),
          ASegment.FScreenObject);
      end;
    end;

    AqCond := AquiferConductance(AReach.Layer, AReach.Row, AReach.Column);
    if AqCond > 0 then
    begin
      Ratio := AReach.Conductance*ParamValue/AqCond;
      if Ratio > HighConductanceContrast then
      begin
        ScreenObject := AReach.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model,StrHighSTRStreamCond,
          Format(StrLayerRowColObject, [
          AReach.Layer+1, AReach.Row+1, AReach.Column+1, ScreenObject.Name]),
          ScreenObject);
      end;
    end
    else
    begin
      ScreenObject := AReach.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model,StrZeroSTRStreamCond,
        Format(StrLayerRowColObject, [
        AReach.Layer+1, AReach.Row+1, AReach.Column+1, ScreenObject.Name]),
        ScreenObject);
    end;

  end;
begin
  CalculateStage := Model.ModflowPackages.StrPackage.CalculateStage;
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        AList := ASegment.FParamValues.Objects[0] as TList;
        Reaches := AList[StressPeriodIndex];
        ParamValue := 1.;
        for ParamIndex := 0 to FParameters.Count - 1 do
        begin
          if FParameters[ParamIndex].ParameterName = ASegment.FParamValues[0] then
          begin
            ParamValue := FParameters[ParamIndex].Value;
            Break;
          end;
        end;
      end
      else
      begin
        Reaches := ASegment.FReaches[StressPeriodIndex];
        ParamValue := 1.;
      end;
      if Reaches.Count > 0 then
      begin
        PriorReach := Reaches[0] as TStr_Cell;
        CheckReach(PriorReach);
        for ReachIndex := 1 to Reaches.Count - 1 do
        begin
          AReach := Reaches[ReachIndex] as TStr_Cell;
          if PriorReach.Stage + DownhillEpsilon < AReach.Stage then
          begin
            ScreenObjectName := ASegment.FScreenObject.Name;
            frmErrorsAndWarnings.AddWarning(Model, StrDownhillFlowWarning,
              Format(StrDownhillObjectWarning,
              [ScreenObjectName, AReach.ReachNumber,
              AReach.Layer+1, AReach.Row+1, AReach.Column+1,
              StressPeriodIndex+1, PriorReach.Stage - AReach.Stage]),
              ASegment.FScreenObject);
          end;
          CheckReach(AReach);
          PriorReach := AReach;
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.CheckReachSeparations;
var
  ReachIndex: Integer;
  DeltaCell: Integer;
  AReach: TValueCell;
  PriorReach: TValueCell;
  StrReach: TStr_Cell;
  Reaches: TValueCellList;
  ASegment: TStrSegment;
  StressPeriodIndex: Integer;
  SegmentIndex: Integer;
  AList: TList;
  StressPeriods: TModflowStressPeriods;
begin
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      if ASegment.FParamValues.Count > 0 then
      begin
        AList := ASegment.FParamValues.Objects[0] as TList;
        Reaches := AList[StressPeriodIndex];
      end
      else
      begin
        Reaches := ASegment.FReaches[StressPeriodIndex];
      end;
      if Reaches.Count > 1 then
      begin
        PriorReach := Reaches[0];
        for ReachIndex := 1 to Reaches.Count - 1 do
        begin
          AReach := Reaches[ReachIndex];
          DeltaCell := Max(Abs(AReach.Row - PriorReach.Row),
            Abs(AReach.Column - PriorReach.Column));
          if DeltaCell > 1 then
          begin
            StrReach := AReach as TStr_Cell;
            frmErrorsAndWarnings.AddWarning(Model, StrInTheStreamSTR,
              Format(StrSegment0dReach, [StrReach.SegmentNumber,
              StrReach.ReachNumber, StressPeriodIndex + 1, DeltaCell]),
              ASegment.FScreenObject);
          end;
          PriorReach := AReach;
        end;
      end;
    end;
  end;
end;

procedure TStrWriter.WriteObservationCell(ACell: TValueCell;
  DataSet5: TStringList; var Expression: TExpression; DataSets,
  Variables: TList; ObsFactor: TObservationFactor);
var
//  Layer: Integer;
//  Row: Integer;
//  Column: Integer;
  Factor: Extended;
  StrCell: TStr_Cell;
begin
  // Write cell
  StrCell := ACell as TStr_Cell;
//  Layer := Model.DataSetLayerToModflowLayer(ACell.Layer);
//  Row := ACell.Row + 1;
//  Column := ACell.Column + 1;
  EvaluateFactor(Factor, Expression, ACell,
    DataSets, Variables, ObsFactor);
  DataSet5.Add(IntToStr(StrCell.SegmentNumber) + ' '
    + IntToStr(StrCell.ReachNumber) + ' '
    + FreeFormattedReal(Factor)
    + ' # Data Set 5: SEGMENT REACH FACTOR');
end;

procedure TStrWriter.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
end;

{ TStrSegment }

procedure TStrSegment.AssignSegmentAndReachNumbers;
var
  TimeList: TList;
  TimeIndex: Integer;
  ReachList: TValueCellList;
  ReachIndex: Integer;
  AReach: TStr_Cell;
begin
  if FParamValues.Count > 0 then
  begin
    Assert(FParamValues.Count = 1);
    TimeList := FParamValues.Objects[0] as TList;
  end
  else
  begin
    TimeList := FReaches;
  end;

  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    ReachList := TimeList[TimeIndex];
    for ReachIndex := 0 to ReachList.Count - 1 do
    begin
      AReach := ReachList[ReachIndex] as TStr_Cell;
      AReach.SegmentNumber := FNewSegmentNumber;
      AReach.ReachNumber := ReachIndex+1;
    end;
  end;
end;

constructor TStrSegment.Create;
var
  StressPeriods: TModflowStressPeriods;
  index: Integer;
begin
  FReaches := TObjectList.Create;
  FParamValues:= TStringList.Create;
  FParamValues.OwnsObjects := true;
  FDownstreamSegments := TGenericIntegerList.Create;
  FDiversionSegments := TGenericIntegerList.Create;
  FTributaries := TListOfTIntegerList.Create;
  FDiversions := TGenericIntegerList.Create;
  FOutFlowSegments := TGenericIntegerList.Create;
  StressPeriods := (CurrentModel as TCustomModel).ModflowFullStressPeriods;
  for index := 0 to StressPeriods.Count - 1 do
  begin
    FTributaries.Add(TGenericIntegerList.Create);
    FDiversions.Add(0);
    FOutFlowSegments.Add(0);
  end;
end;

destructor TStrSegment.Destroy;
begin
  FDiversions.Free;
  FTributaries.Free;
  FOutFlowSegments.Free;
  FDiversionSegments.Free;
  FDownstreamSegments.Free;
  FParamValues.Free;
  FReaches.Free;
  inherited;
end;

procedure TStrSegment.EvaluateLinks(Model: TBaseModel);
var
  index: Integer;
  StrBoundary: TStrBoundary;
  Item: TStrItem;
  ParamIndex: Integer;
  AParam: TStrParamItem;
  Values: TCustomMF_BoundColl;
begin
  Assert(FScreenObject <> nil);
  if FParamValues.Count > 0 then
  begin
    Assert(FParamValues.Count = 1);
    FParameterName := FParamValues[0];
    FParameterNumber := (Model as TCustomModel).
      ModflowTransientParameters.ParamNameIndex(FParameterName);
    if FParameterNumber < 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidSTRParamete,
        Format(StrAParameterNamed0, [FParameterName, FScreenObject.Name]),
        FScreenObject);
      Exit;
    end;
    Assert(FParameterNumber >= 0);
  end
  else
  begin
    FParameterNumber := -1;
    FParameterName := '';
  end;
  StrBoundary := FScreenObject.ModflowStrBoundary;
  Values := StrBoundary.Values;
  for index := 0 to Values.Count - 1 do
  begin
    Item := Values[index] as TStrItem;
    if (Item.OutflowSegment > 0)
      and (FDownstreamSegments.IndexOf(Item.OutflowSegment) < 0) then
    begin
      FDownstreamSegments.Add(Item.OutflowSegment)
    end;
    if (Item.DiversionSegment > 0)
      and (FDiversionSegments.IndexOf(Item.DiversionSegment) < 0) then
    begin
      FDiversionSegments.Add(Item.DiversionSegment)
    end;
  end;
  for ParamIndex := 0 to StrBoundary.Parameters.Count - 1 do
  begin
    AParam := StrBoundary.Parameters[ParamIndex] as TStrParamItem;
    Values := AParam.Param;
    for index := 0 to Values.Count - 1 do
    begin
      Item := Values[index] as TStrItem;
      if (Item.OutflowSegment > 0)
        and (FDownstreamSegments.IndexOf(Item.OutflowSegment) < 0) then
      begin
        FDownstreamSegments.Add(Item.OutflowSegment)
      end;
      if (Item.DiversionSegment > 0)
        and (FDiversionSegments.IndexOf(Item.DiversionSegment) < 0) then
      begin
        FDiversionSegments.Add(Item.DiversionSegment)
      end;
    end;
  end;
end;

function TStrSegment.OriginalSegmentNumber: Integer;
begin
  result := FScreenObject.ModflowStrBoundary.SegmentNumber
end;

{ TStrSegmentComparer }

function TStrSegmentComparer.Compare(const Left, Right: TStrSegment): Integer;
begin
  result := Left.FParameterNumber - Right.FParameterNumber;
  if result = 0 then
  begin
    result := Left.OriginalSegmentNumber - Right.OriginalSegmentNumber;
  end;
end;

{ TParameterSegments }

constructor TParameterSegments.Create;
begin
  FUpStreamParameters := TStringList.Create;
end;

destructor TParameterSegments.Destroy;
begin
  FUpStreamParameters.Free;
  inherited;
end;

end.
