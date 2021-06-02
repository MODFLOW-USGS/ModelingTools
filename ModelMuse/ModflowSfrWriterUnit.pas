unit ModflowSfrWriterUnit;

interface

uses System.UITypes,Windows, Types, SysUtils, Classes, Contnrs,
  CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, PhastModelUnit, ScreenObjectUnit,
  ModflowSfrParamIcalcUnit, ModflowSfrUnit, ModflowSfrSegment,
  ModflowSfrUnsatSegment, ModflowBoundaryDisplayUnit, ModflowCellUnit,
  Generics.Collections, ModflowSfrReachUnit, GoPhastTypes, ModflowGridUnit,
  Vcl.Dialogs;


type
  TSegment = class;

  TReachList = TList<TSfr_Cell>;

  // @name is used for SFR segments in LGR models. Each segment has
  TSubSegment = class(TObject)
  private
    FReachList: TReachList;
    FSegment: TSegment;
    FModel: TBaseModel;
    // @name is the segment number for MODFLOW.
    FSegmentNumber: integer;
    // @name is the @classname of the other model that
    // is at the same location as this @classname.
    FAssociatedLgrSubSeg: TSubSegment;
    FTotalLength: double;
    FIndex: Integer;
    FUpstreamValues: array of TSfrSegmentRecord;
    FDownstreamValues: array of TSfrSegmentRecord;
    FUpstreamUnsatValues: array of TSfrUnsatSegmentRecord;
    FDownstreamUnsatValues: array of TSfrUnsatSegmentRecord;
    FParentUpstreamValues: array of TSfrSegmentRecord;
    FParentDownstreamValues: array of TSfrSegmentRecord;
    FParentUpstreamUnsatValues: array of TSfrUnsatSegmentRecord;
    FParentDownstreamUnsatValues: array of TSfrUnsatSegmentRecord;
    FUsed: boolean;
    FChildEdge: boolean;
    function GetReach(Index: Integer): TSfr_Cell;
  public
    constructor Create(AModel: TBaseModel; ASegment: TSegment; StressPeriodCount: integer);
    destructor Destroy; override;
    function AddReach(AReach: TSfr_Cell): Integer;
    function ReachCount: integer;
    property Reaches[Index: Integer]: TSfr_Cell read GetReach;
    procedure DeleteReach(Index: Integer);
    property SegmentNumber: integer read FSegmentNumber;
    property Used: boolean read FUsed;
    property ChildEdge: boolean read FChildEdge;
  end;

  TSubSegmentList = TObjectList<TSubSegment>;

  TSegment = class(TObject)
  private
    // @name contains @link(TSfr_Cell)
    FReaches: TValueCellList;
    FScreenObject: TScreenObject;
    // @name is used when sorting segments.
    FHasTributaries: boolean;
    // @name is the segment number exported to MODFLOW.
    FNewSegmentNumber: integer;
    FSubSegmentList: TSubSegmentList;
    FExternalFlow: TExternalFlowProperties;
    function GetReach(Index: integer): TValueCell;
    function GetReachCount: integer;
    procedure SetNewSegmentNumber(const Value: integer);
  public
    constructor Create;
    // @name is the segment number assigned by the user.
    function OriginalSegmentNumber: integer;
    // @name is the segment number exported to MODFLOW.
    property NewSegmentNumber: integer read FNewSegmentNumber
      write SetNewSegmentNumber;
    Destructor Destroy; override;
    function OriginalDownStreamSegmentNumbers: TIntegerDynArray;
    function OriginalDiversionSegmentNumbers: TIntegerDynArray;
    property Reaches[Index: integer]: TValueCell read GetReach;
    property ReachCount: integer read GetReachCount;
    function AddSubSegment(AModel: TBaseModel; StressPeriodCount: integer): TSubSegment;
    property SubSegmentList: TSubSegmentList read FSubSegmentList;
    property ExternalFlow: TExternalFlowProperties read FExternalFlow;
//    procedure DeleteReach(Index: Integer);
  end;

  TStreamDirection = (sdDownstream, sdUpstream);

  TModflowSFR_Writer = class;

  TSfrWriterList = TList<TModflowSFR_Writer>;

  TModflowSFR_Writer = class(TCustomPackageWriter)
  private
//    FNameOfFile: string;
    FValues: TList;
    FSegments: TList;
    FLakes: TList;
    ISFROPT: integer;
    NSFRPAR: integer;
    FLgrUsed: Boolean;
    FIsChildModel: Boolean;
    NUMTAB: Integer;
    MAXVAL: Integer;
    FSegDictionary: TDictionary<Integer, TSegment>;
    FLakDictionary: TDictionary<Integer, TScreenObject>;
    FSfrObservationsUsed: Boolean;
    function NewFormat: boolean;
    procedure CheckParamInstances;
    procedure WriteDataSet1a;
    procedure WriteDataSet1b;
    procedure WriteDataSet1c;
    procedure WriteDataSet2;
    procedure WriteDataSets3and4;
    procedure WriteDataSet4b6a(StartTime: double;
      Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
      SfrBoundary: TSfrBoundary; DataSet4B: boolean; SubSegIndex: integer);
    procedure WriteDataSet4c6b(Parameter: Boolean;
      SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
      StartTime: double; StressPeriodIndex: integer; Segment: TSegment;
      SubSegIndex: integer; ParameterValue: double);
    procedure WriteSegmentValues(StressPeriodIndex: Integer;
      Parameter: Boolean; UpOrDownStreamValues: TSfrSegmentStorage; upstream: Boolean;
      var CommentLine: string; var ValuesWriten: boolean;
      ParamScreenObjectItem: TSfrParamIcalcItem; PSegValue: PSfrSegmentRecord;
      Segment: TSegment; ParameterValue: double);
    procedure WriteUnsatSegmentValues(upstream: Boolean;
      var CommentLine: string; var ValuesWriten: boolean;
      UnsatUpstreamValues: TSfrUnsatSegmentStorage;
      PSegUnsatValue: PSfrUnsatSegmentRecord);
    procedure WriteDataSet4d6c(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem;
      StartTime: double; StressPeriodIndex: integer; Segment: TSegment;
      SubSegIndex: integer; ParameterValue: double);
    procedure WriteDataSet4e6d(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem; StressPeriod: integer;
      Segment: TSegment; SubSegIndex: integer);
    procedure WriteDataSet4f6e(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem; StartTime: double;
      Segment: TSegment; SubSegIndex: integer);
    procedure WriteDataSet8(Segment: TSegment; SubSegIndex,
      TimeIndex: integer);
    procedure WriteDataSets5to7;
    function FindConvertedSegment(OriginalSegmentNumber: integer;
      Direction: TStreamDirection; out Segment: TSegment): integer;
    procedure WriteGages(GageLines: TStrings);
    function GetSegment(Index: integer): TSegment;
    function GetSegmentCount: integer;
    procedure TestBedElevations;
    procedure CreateLgrSubSegments;
    procedure RenumberLgrSubSegments;
    procedure WriteSegment(Segment: TSegment;
      StartTime: double; SubSegIndex: integer; Item: TSfrParamIcalcItem;
      Boundary: TSfrBoundary; TimeIndex: integer;
      IsParameter: boolean; ParameterValue: double);
    procedure LgrAdjustSegmentValues(
      Segment: TSegment; StartTime: double; SubSegIndex: integer;
      StressPeriod: integer);
    procedure AdjustLgrParamValues;
    procedure AdjustLgrNonParamValues;
    procedure AdjustLgrValues;
    procedure InternalUpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure AdjustReachLengths;
    procedure CheckOutflowSegments(StartTime: double;
      Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
      SfrBoundary: TSfrBoundary; SubSegIndex: integer);
    procedure CheckParameterSegments;
    procedure CheckNonParamSegments;
    procedure CheckStreamBottomElevation(ScreenObject: TScreenObject;
      Grid: TModflowGrid; SfrReach: TSfr_Cell);
    procedure WriteObsScript(const AFileName: string);
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    procedure SortSegments;
  public
    procedure Evaluate;
    procedure AssociateLgrSubSegments(SfrWriterList: TSfrWriterList);
    property Segments[Index: integer]: TSegment read GetSegment;
    property SegmentCount: integer read GetSegmentCount;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string; GageLines: TStrings);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

  TExternalFlowFileWriter = class(TCustomModflowWriter)
  private
    FExternalFlow: TExternalFlowProperties;
    FFileName: string;
    FSegNum: Integer;
  protected
    class function Extension: string; override;
    constructor Create(AModel: TCustomModel;
      ExternalFlow: TExternalFlowProperties; Const SegNum: integer); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, OrderedCollectionUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowSfrTable,
  ModflowSfrFlows, ModflowSfrChannelUnit, ModflowSfrEquationUnit,
  ModflowTimeUnit, frmProgressUnit, IntListUnit, Forms,
  ModflowBoundaryUnit, Math, DataSetUnit, ObservationComparisonsUnit,
  PestObsUnit, ModelMuseUtilities;

resourcestring
  StrInvalidStartingTimeStep1 = 'Invalid starting time or missing data for the '
    + 'first time step in the following objects';

  StrInvalidStartingTime = 'Invalid starting time in the following objects';
  StrOneOrMoreSFRStre = 'One or more SFR stream segments have slopes that '
    + 'are zero or negative.';
  StrDownstreamOutOfOrder = 'In the SFR package, the following objects '
    + 'define streams that flow into streams with lower segment numbers.  '
    + 'Because the stream segments are in strict numerical order, ModelMuse '
    + 'will not renumber them. However, you may wish to check the order of the '
    + 'stream segments.';
  StrDiversionOutOfOrder = 'In the SFR package, the following objects define'
    + ' streams that divert flow from streams with higher segment numbers.  ' 
    + 'Because the stream segments are in strict numerical order, ModelMuse '
    + 'will not renumber them. However, you may wish to check the order of the '
    + 'stream segments.';
  StrLakeDownstreamError = 'In the SFR package, the following objects '
    + 'define streams that flow into lakes even though the lake package is ' +
      'not in use.';
  StrLakeDiversionError = 'In the SFR package, the following objects '
    + 'define streams that divert flow from lakes even though the lake ' +
      'package is not in use.';
  StrObjectSTime1 = 'Object: %0:s; Time: %1:g; Upstream elevation: %2:g; Downs' +
  'tream elevation: %3:g';
  StrNoStreamsDefined = 'No streams defined.';
  StrTheSFRPackageHas = 'The SFR package has been activated but no streams h' +
  'ave been defined.';
  StrDupParamInstances = 'The following objects contained duplicate SFR ' +
  'Parameter instances.';
//  StrObject0sLayer = 'Object = %0:s; Layer = %1:d; Row = %2:d; Column = %3:d';
  StrTheFollowingObject = 'The following objects specify external flow files' +
  ' that do not exist';
  StrStreamFlowFileDoe = 'Stream flow file does not exist.';
  StrTheStreamFlowFile2 = 'The stream flow file (%0:s) specified in object ' +
  '%1:s does not exist';
  StrEvaluatingSFRPacka = 'Evaluating SFR Package data.';
  StrEvaluatingS = '    Evaluating %s';
  StrWritingParamete = '    Writing parameter %s';
  StrWritingSFRPackage = 'Writing SFR Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrInterpolatedFromS = 'Interpolated from %s';
  Str0sMultipliedByP = '%0:s multiplied by Parameter: %1:s';
  WarningRootRL_Less_0 = 'SFR reach length (RCHLEN) <= 0';
  StrLargeSeparationBet = 'Large separation between connected segments';
  StrTheDownstreamEndO = 'The downstream end of %0:s is separated from the u' +
  'pstream end of %1:s by %2:d cells.';
  StrLargeDiversionSeparation = 'Large separation between diversion segment and source segment';
  StrWritingStressP = '    Writing stress period %d';
  StrCheckingStress = '    Checking stress period %d';
  StrInactiveReach = 'One or more SFR stream reaches are in inactive cell.';
  StrReachSeparationWarning = 'In the Streamflow Routing (SFR) package, some' +
  ' reaches are separated from previous reaches by more than one cell.';
  StrSegment0dReach = 'Segment: %0:d; Reach: %1:d; Stress Period: %2:d; Separation: %3:d';


  StrSegmentNumber = 'Segment Number in ';
  StrReachNumber = 'Reach Number in ';
  SfrICalcNumber = 'ICALC in ';
  StrDownstreamSegmentNumber = 'Outflow Segment Number in ';
  StrDiversionSegmentNumber = 'Diversion Segment Number in ';
  StrIprior = 'IPRIOR in ';
  StrHighSFRHydraulicC = 'High SFR hydraulic conductivity compared to the aq' +
  'uifer hydraulic conductivity may cause numerical difficulties';
  StrDuplicateSFRStream = 'Duplicate SFR Stream Segments Numbers';
  StrTheSFRSegmentNumb = 'The SFR segment number %0:d in %1:s is the same se' +
  'gment number as in another segment.';
  StrZeroSFRHydraulicC = 'Negative or zero SFR hydraulic conductivity';
  StrOneOrMoreReaches = 'One or more SFR reaches defined by the same object are ' +
  'in the same row and column but different layers.';
  StrAtRowcolumn = 'At (row,column) = (%0:d, %1:d) there are two or more SFR' +
  ' cells defined in different layers by the object %2:s';
  StrWhenUnsaturatedFlo = 'When unsaturated flow beneath SFR streams is simulate' +
  'd, streams must be in unconfined or convertible layers. This requirement ' +
  'is not met at the following locations.';
  StrObject0sLayer = 'Object: %0:s; (Layer, Row, Column) = (%1:d, %2:d, %3:d' +
  ').';
  StrTheMinimumZValue = 'The minimum Z value in a stream cross section must ' +
  'be zero. The zero value must be in positions 1 through 7 of the cross sec' +
  'tion. The following objects don''t meet ths criterion.';
  StrObject0sStartin = 'Object %0:s; Starting time: %1:g; Minimum Z: %2:g';
  StrTheBottomOfTheSt = 'The bottom of the stream bed is below the bottom of' +
  ' the cell at the following locations.';
  StrObject0sLayerThick = 'Object: %0:s; (Layer, Row, Column) = (%1:d, %2:d,' +
  ' %3:d); Streambed top: %4:g; Streambed thickness: %5:g; Cell Bottom: %6:g' +
  '.';
  StrTwoOrMoreLakeObjects = 'Two or more objects define the same lake ID.';
  StrTheLakeObjectNamed = 'The object named %0:s, defines the same Lake ID a' +
  's %1:s';
  StrTheSFRPackageIsN = 'The SFR package is not supported by MT3DMS.';
  StrAlthoughMT3DMSVers = 'Although MT3DMS version 5.3 supports the STR pack' +
  'age, is does not suppport the SFR package.';
  ZeroSlope = 'Object: %0:s; (Layer, Row, Column) = (%1:d, %2:d, %3:d)';
  StrTheSEndOfTheFo = 'The %s end of the following objects may be above or b' +
  'elow the grid';
  StrUpstream = 'upstream';
  StrDownstream = 'downstream';
  StrInvalidSFROption = 'Invalid SFR option';
  StrTheSOptionInNot = 'The %s option in not supported in this version of MO' +
  'DFLOW. It is supported in MODFLOW-NWT 1.1 and later';
  StrTABFILES = 'TABFILES';
  StrLOSSFACTOR = 'LOSSFACTOR';
  StrInvalidSFROutflow = 'Invalid SFR outflow segment';
  StrIn1sTheSFRSeg = 'In %0:s, the SFR segment number is the same as the out' +
  'flow segment number. The outflow segment number should be the number of t' +
  'he stream or lake into which the segment flows. The outflow segment ' +
  'should be zero if the segment does not flow into any other segment in the ' +
  'model.';
  StrSFRChildModelLink = 'SFR child model linkage undefined.';
  StrTheObjectSMayNo = 'The object %s may not set values for both the parent' +
  ' and child models. This may prevent the proper linking of streams between' +
  ' the parent and child models.';

resourcestring
  DupErrorCategory = 'Duplicate SFR segment numbers';
  CircularCategory = 'The following SFR segments circle back on themselves.';
  UnsatError = 'One or more objects do not define unsaturated flow properties '
    + 'in the SFR package.';
  DupNumbersError = 'The SFR segments defined by %0:s and %1:s have '
    + 'the same segment number: %2:d.';
  CircularError = 'Object: %0:s; Segment Number: %1:d.';

{ TModflowSFR_Writer }

function CompareSegments(Item1, Item2: Pointer): Integer;
var
  Segment1, Segment2: TSegment;
begin
  Segment1 := Item1;
  Segment2 := Item2;
  result := Segment1.OriginalSegmentNumber - Segment2.OriginalSegmentNumber;
end;

procedure TModflowSFR_Writer.AdjustReachLengths;
var
  Index: Integer;
  Segment: TSegment;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
  ReachIndex: Integer;
  AReach: TSfr_Cell;
  ParentSubSeg: TSubSegment;
  LocalChildModel: TChildModel;
  ParentCol: integer;
  ParentRow: Integer;
  ParentLayer: Integer;
  ParentReachIndex: Integer;
  ParentReachLengthTotal: double;
  Factor: Extended;
  MoreSubSegs: TList;
  InnerSubSegIndex: Integer;
  AnotherSubSeg: TSubSegment;
  ParentCol1: Integer;
  ParentRow1: Integer;
  ParentLayer1: Integer;
  ChildReachTotal: double;
begin
  if Model is TChildModel then
  begin
    LocalChildModel := TChildModel(Model);
    MoreSubSegs := TList.Create;
    try
      for Index := 0 to SegmentCount - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        Segment := Segments[Index];
        for SubSegIndex := 0 to Segment.SubSegmentList.Count - 1 do
        begin
          SubSeg := Segment.SubSegmentList[SubSegIndex];
          if MoreSubSegs.IndexOf(SubSeg) >= 0 then
          begin
            Continue;
          end;
          if SubSeg.ChildEdge and (SubSeg.FTotalLength > 0) then
          begin
            MoreSubSegs.Clear;
            MoreSubSegs.Add(SubSeg);
            ParentSubSeg := SubSeg.FAssociatedLgrSubSeg;
            if (ParentSubSeg <> nil) and (SubSeg.ReachCount > 0) and (ParentSubSeg.ReachCount > 0) then
            begin
              AReach := SubSeg.Reaches[0];
              ParentCol := LocalChildModel.ChildColToParentCol(AReach.Column);
              ParentRow := LocalChildModel.ChildRowToParentRow(AReach.Row);
              ParentLayer := LocalChildModel.ChildLayerToParentLayer(AReach.Layer);

              for InnerSubSegIndex := SubSegIndex + 1 to Segment.SubSegmentList.Count - 1 do
              begin
                AnotherSubSeg := Segment.SubSegmentList[InnerSubSegIndex];
                if AnotherSubSeg.ReachCount > 0 then
                begin
                  AReach := AnotherSubSeg.Reaches[0];
                  ParentCol1 := LocalChildModel.ChildColToParentCol(AReach.Column);
                  ParentRow1 := LocalChildModel.ChildRowToParentRow(AReach.Row);
                  ParentLayer1 := LocalChildModel.ChildLayerToParentLayer(AReach.Layer);
                  if (ParentCol = ParentCol1)
                    and (ParentRow = ParentRow1)
                    and (ParentLayer = ParentLayer1) then
                  begin
                    MoreSubSegs.Add(AnotherSubSeg);
                  end
                  else
                  begin
                    Break;
                  end;
                end;
              end;

              ParentReachLengthTotal := 0;
              for ParentReachIndex := 0 to ParentSubSeg.ReachCount - 1 do
              begin
                AReach := ParentSubSeg.Reaches[ParentReachIndex];
                if (AReach.Column = ParentCol)
                  and (AReach.Row = ParentRow)
                  and (AReach.Layer = ParentLayer) then
                begin
                  ParentReachLengthTotal := ParentReachLengthTotal + AReach.ReachLength;
                end;
              end;

              if ParentReachLengthTotal > 0 then
              begin
                ChildReachTotal := 0;
                for InnerSubSegIndex := 0 to MoreSubSegs.Count - 1 do
                begin
                  AnotherSubSeg := MoreSubSegs[InnerSubSegIndex];
                  ChildReachTotal := ChildReachTotal + AnotherSubSeg.FTotalLength;
                end;

                Factor := ParentReachLengthTotal/ChildReachTotal;
                for InnerSubSegIndex := 0 to MoreSubSegs.Count - 1 do
                begin
                  AnotherSubSeg := MoreSubSegs[InnerSubSegIndex];
                  AnotherSubSeg.FTotalLength := AnotherSubSeg.FTotalLength * Factor;
                  for ReachIndex := 0 to AnotherSubSeg.ReachCount - 1 do
                  begin
                    AReach := AnotherSubSeg.Reaches[ReachIndex];
                    AReach.Values.AdjustReachLength(Factor);
                  end;
                end;

              end;
            end;
          end;
        end;
      end;
    finally
      MoreSubSegs.Free;
    end;
  end;

end;

procedure TModflowSFR_Writer.AdjustLgrValues;
begin
  AdjustReachLengths;
  AdjustLgrNonParamValues;
  AdjustLgrParamValues;
end;

procedure TModflowSFR_Writer.AssociateLgrSubSegments(
  SfrWriterList: TSfrWriterList);
var
  SegmentIndex: Integer;
  Segment: TSegment;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
  ChildWriter: TModflowSFR_Writer;
  ChildSeg: TSegment;
  ChildSubSeg: TSubSegment;
  ParentReach: TSfr_Cell;
  FirstChildReach: TSfr_Cell;
  LocalChildModel: TChildModel;
  ChildSubSegIndex: Integer;
  ParentCol: Integer;
  ParentRow: Integer;
  ParentLayer: Integer;
  ParentReachIndex: Integer;
  Number: Integer;
  WriterIndex: Integer;
  Writer: TModflowSFR_Writer;
  InnerIndex: Integer;
  AnotherChildSubSeg: TSubSegment;
  function GetChildWriter(AModel: TBaseModel): TModflowSFR_Writer;
  var
    ChildIndex: Integer;
    AWriter: TModflowSFR_Writer;
  begin
    result := nil;
    for ChildIndex := 0 to SfrWriterList.Count - 1 do
    begin
      AWriter := SfrWriterList[ChildIndex];
      if AWriter.Model = AModel then
      begin
        Result := AWriter;
        Exit;
      end;
    end;
  end;
  function GetMatchingChildSeg(Segment: TSegment;
    ChildWriter: TModflowSFR_Writer): TSegment;
  var
    ChildSegIndex: Integer;
    ChildSeg: TSegment;
  begin
    Assert(ChildWriter <> nil);
    Result := nil;
    for ChildSegIndex := 0 to ChildWriter.SegmentCount - 1 do
    begin
      ChildSeg := ChildWriter.Segments[ChildSegIndex];
      if Segment.FScreenObject = ChildSeg.FScreenObject then
      begin
        Result := ChildSeg;
        Exit;
      end;
    end;
  end;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSFR) then
  begin
    Exit;
  end;
  Assert((Model is TPhastModel) and TPhastModel(Model).LgrUsed);
  for SegmentIndex := 0 to SegmentCount - 1 do
  begin
    Segment := Segments[SegmentIndex];
    for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
    begin
      SubSeg :=  Segment.FSubSegmentList[SubSegIndex];
      if SubSeg.FModel <> Model then
      begin
        LocalChildModel := SubSeg.FModel as TChildModel;
        ChildWriter := GetChildWriter(LocalChildModel);
        ChildSeg := GetMatchingChildSeg(Segment, ChildWriter);
        if ChildSeg <> nil then
        begin
          for ChildSubSegIndex := 0 to ChildSeg.FSubSegmentList.Count - 1 do
          begin
            ChildSubSeg := ChildSeg.FSubSegmentList[ChildSubSegIndex];
            if ChildSubSeg.FAssociatedLgrSubSeg <> nil then
            begin
              Continue;
            end;
            FirstChildReach := ChildSubSeg.Reaches[0];

            ParentCol := LocalChildModel.ChildColToParentCol(FirstChildReach.Column);
            ParentRow := LocalChildModel.ChildRowToParentRow(FirstChildReach.Row);
            ParentLayer := LocalChildModel.ChildLayerToParentLayer(FirstChildReach.Layer);
            for ParentReachIndex := 0 to SubSeg.ReachCount - 1 do
            begin
              ParentReach := SubSeg.Reaches[ParentReachIndex];
              if (ParentCol = ParentReach.Column)
                and (ParentRow = ParentReach.Row)
                and (ParentLayer = ParentReach.Layer)
                then
              begin
                if SubSeg.FAssociatedLgrSubSeg = nil then
                begin
                  SubSeg.FAssociatedLgrSubSeg := ChildSubSeg;
                end;
                ChildSubSeg.FAssociatedLgrSubSeg := SubSeg;
                break;
              end;
            end;
  //          Assert(ChildSubSeg.FAssociatedLgrSubSeg <> nil);
          end;

          for ChildSubSegIndex := 0 to ChildSeg.FSubSegmentList.Count - 1 do
          begin
            ChildSubSeg := ChildSeg.FSubSegmentList[ChildSubSegIndex];
            if ChildSubSeg.FAssociatedLgrSubSeg <> nil then
            begin
              Continue;
            end;

            for InnerIndex := ChildSubSegIndex -1 downto 0 do
            begin
              AnotherChildSubSeg := ChildSeg.FSubSegmentList[InnerIndex];
              if AnotherChildSubSeg.FAssociatedLgrSubSeg <> nil then
              begin
                ChildSubSeg.FAssociatedLgrSubSeg := AnotherChildSubSeg.FAssociatedLgrSubSeg;
                break;
              end;
            end;
            if ChildSubSeg.FAssociatedLgrSubSeg <> nil then
            begin
              Continue;
            end;
            for InnerIndex := ChildSubSegIndex +1 to ChildSeg.FSubSegmentList.Count - 1 do
            begin
              AnotherChildSubSeg := ChildSeg.FSubSegmentList[InnerIndex];
              if AnotherChildSubSeg.FAssociatedLgrSubSeg <> nil then
              begin
                ChildSubSeg.FAssociatedLgrSubSeg := AnotherChildSubSeg.FAssociatedLgrSubSeg;
                break;
              end;
            end;
//            Assert(ChildSubSeg.FAssociatedLgrSubSeg <> nil);
          end;
        end;

      end;
    end;
  end;
  if (Model as TPhastModel).LgrV1Used then
  begin
    for SegmentIndex := 0 to SegmentCount - 1 do
    begin
      Segment := Segments[SegmentIndex];
      for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
      begin
        SubSeg :=  Segment.FSubSegmentList[SubSegIndex];
        if SubSeg.FModel <> Model then
        begin
          for ParentReachIndex := SubSeg.ReachCount - 1 downto 0 do
          begin
            ParentReach := SubSeg.Reaches[ParentReachIndex];
            if ParentReach.Values.ReachLength = 0 then
            begin
              SubSeg.DeleteReach(ParentReachIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  Number := 1;
  for SegmentIndex := 0 to SegmentCount - 1 do
  begin
    Segment := Segments[SegmentIndex];
    Segment.NewSegmentNumber := Number;
    if Segment.FSubSegmentList.Count = 0 then
    begin
      Inc(Number);
    end
    else
    begin
      for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
      begin
        SubSeg := Segment.FSubSegmentList[SubSegIndex];
        SubSeg.FSegmentNumber := Number;
        if SubSeg.ReachCount > 0 then
        begin
          Inc(Number);
        end;
      end;
    end;
  end;
  for WriterIndex := 0 to SfrWriterList.Count - 1 do
  begin
    Writer := SfrWriterList[WriterIndex];
    Writer.AdjustLgrValues;
  end;
  for WriterIndex := 0 to SfrWriterList.Count - 1 do
  begin
    Writer := SfrWriterList[WriterIndex];
    Writer.CheckParameterSegments;
    Writer.CheckNonParamSegments;
  end;
end;

procedure TModflowSFR_Writer.AdjustLgrParamValues;
var
  LocalModel: TCustomModel;
  ParamIndex: Integer;
  ParamItem: TModflowTransientListParameter;
  Instances: TList;
  SfrPackage: TSfrPackageSelection;
  Index: Integer;
  InstanceItem: TSfrParamInstance;
  Segment: TSegment;
  ScreenObject: TScreenObject;
  ScreenObjectParamIndex: Integer;
  ParamScreenObjectItem: TSfrParamIcalcItem;
  Segments : TList;
  InstanceIndex: Integer;
  SfrBoundary: TSfrBoundary;
  SubSegIndex: Integer;
//  MaxStressPeriodIndex: Integer;
  StartTime: Double;
  EndTime: Double;
  StressPeriodIndex: Integer;
begin
  LocalModel := Model as TCustomModel;
  SfrPackage := Package as TSfrPackageSelection;
//  MaxStressPeriodIndex :=LocalModel.ModflowFullStressPeriods.Count -1;
  StartTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
  EndTime := LocalModel.ModflowFullStressPeriods[LocalModel.ModflowFullStressPeriods.Count -1].EndTime;
  for ParamIndex := 0 to LocalModel.ModflowTransientParameters.Count - 1 do
  begin
    ParamItem := LocalModel.ModflowTransientParameters.Items[ParamIndex];
    if ParamItem.ParameterType = ptSFR then
    begin
      Instances := TList.Create;
      Segments := TList.Create;
      try
        for Index := 0 to SfrPackage.ParameterInstances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := SfrPackage.ParameterInstances.Items[Index];
          if InstanceItem.ParameterName = ParamItem.ParameterName then
          begin
            if (StartTime <= InstanceItem.StartTime) and (EndTime >= InstanceItem.EndTime) then
            begin
              Instances.Add(InstanceItem);
            end;

          end;
        end;
        for Index := 0 to FSegments.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          Segment := FSegments[Index];
          ScreenObject := Segment.FScreenObject;
          Assert(ScreenObject.ModflowSfrBoundary <> nil);
          for ScreenObjectParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ParamScreenObjectItem := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ScreenObjectParamIndex];
            if ParamScreenObjectItem.Param = ParamItem.ParameterName then
            begin
              Segments.Add(Segment);
              break;
            end;
          end;
        end;
        for Index := 0 to Segments.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          Segment := Segments[Index];
          ScreenObject := Segment.FScreenObject;
          Assert(ScreenObject.ModflowSfrBoundary <> nil);
          SfrBoundary := ScreenObject.ModflowSfrBoundary;

          StressPeriodIndex := -1;
          for InstanceIndex := 0 to Instances.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            InstanceItem := Instances[InstanceIndex];
            for ScreenObjectParamIndex := 0 to SfrBoundary.ParamIcalc.Count - 1 do
            begin
              Application.ProcessMessages;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
              ParamScreenObjectItem := SfrBoundary.
                ParamIcalc.Items[ScreenObjectParamIndex];
              if (ParamScreenObjectItem.Param = ParamItem.ParameterName)
                and (ParamScreenObjectItem.ParamInstance
                = InstanceItem.ParameterInstance) then
              begin
                Inc(StressPeriodIndex);
                if Segment.FSubSegmentList.Count = 0 then
                begin
                  SubSegIndex := -1;
                  LgrAdjustSegmentValues(Segment, InstanceItem.StartTime,
                    SubSegIndex, StressPeriodIndex);
                end
                else
                begin
                  for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
                  begin
                    LgrAdjustSegmentValues(Segment, InstanceItem.StartTime,
                      SubSegIndex, StressPeriodIndex);
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        Instances.Free;
        Segments.Free;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.AdjustLgrNonParamValues;
var
  UsedSegments: TList;
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  SegementIndex: Integer;
  Segment: TSegment;
  Item: TSfrParamIcalcItem;
//  Boundary: TSfrBoundary;
  SubSegIndex: Integer;
begin
  UsedSegments := TList.Create;
  try

    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      // data set 5;
      UsedSegments.Clear;

      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      for SegementIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
//        SfrBoundary := Segment.FScreenObject.ModflowSfrBoundary;
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if (Item = nil) or (Item.Param = '') then
        begin
          UsedSegments.Add(Segment);
        end;
      end;


      for SegementIndex := 0 to UsedSegments.Count - 1 do
      begin
        // Data set 6a
        Segment := UsedSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if Item <> nil then
        begin

//          Boundary := Segment.FScreenObject.ModflowSfrBoundary;
          if Segment.FSubSegmentList.Count = 0 then
          begin
            SubSegIndex := -1;
            LgrAdjustSegmentValues(Segment, StressPeriod.StartTime, SubSegIndex, TimeIndex);
          end
          else
          begin
            for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
            begin
              LgrAdjustSegmentValues(Segment, StressPeriod.StartTime, SubSegIndex, TimeIndex);
            end;
          end;
        end;
      end;
    end;
  finally
    UsedSegments.Free;
  end;

end;

procedure TModflowSFR_Writer.CheckParamInstances;
var
  Index: Integer;
  Segment: TSegment;
  ScreenObject: TScreenObject;
  ScreenObjectParamIndex: Integer;
  ParamScreenObjectItem: TSfrParamIcalcItem;
  ParameterNames: TStringList;
  InstanceNames: TStringList;
  InstancesList: TList;
  Position: Integer;
begin
  ParameterNames := TStringList.Create;
  InstancesList := TObjectList.Create;
  try

    for Index := 0 to FSegments.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ParameterNames.Clear;
      InstancesList.Clear;
      Segment := FSegments[Index];
      ScreenObject := Segment.FScreenObject;
      Assert(ScreenObject.ModflowSfrBoundary <> nil);
      for ScreenObjectParamIndex := 0 to ScreenObject.ModflowSfrBoundary.
        ParamIcalc.Count - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ParamScreenObjectItem := ScreenObject.ModflowSfrBoundary.
          ParamIcalc.Items[ScreenObjectParamIndex];
        if ParamScreenObjectItem.Param = '' then
        begin
          Continue;
        end;
        Position := ParameterNames.IndexOf(ParamScreenObjectItem.Param);
        if Position < 0 then
        begin
          InstanceNames := TStringList.Create;
          InstancesList.Add(InstanceNames);
          ParameterNames.AddObject(ParamScreenObjectItem.Param, InstanceNames)
        end
        else
        begin
          InstanceNames := ParameterNames.Objects[Position] as TStringList;
        end;
        if InstanceNames.IndexOf(ParamScreenObjectItem.ParamInstance) < 0 then
        begin
          InstanceNames.Add(ParamScreenObjectItem.ParamInstance)
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrDupParamInstances,
            ScreenObject.Name, ScreenObject);
        end;
      end;
    end;
  finally
    ParameterNames.Free;
    InstancesList.Free;
  end;
end;

constructor TModflowSFR_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FLgrUsed := (not (Model is TPhastModel) or TPhastModel(Model).LgrUsed);
  if FLgrUsed then
  begin
    FIsChildModel := Model is TChildModel;
  end
  else
  begin
    FIsChildModel := False;
  end;
  FValues := TObjectList.Create;
  FSegments := TObjectList.Create;
end;

procedure TModflowSFR_Writer.CreateLgrSubSegments;
var
  LocalModel: TPhastModel;
  SegmentIndex: Integer;
  ASegment: TSegment;
  ReachIndex: Integer;
  AReach: TSfr_Cell;
  CurrentModel : TCustomModel;
  SubSeg: TSubSegment;
  LocalChildModel: TChildModel;
  OverlapWidth: Integer;
  MaxOverlappedColumn: Integer;
  MaxOverlappedRow: Integer;
  MaxOverlappedLayer: Integer;
  function ReachInModel(AReach: TSfr_Cell; ChildModel: TChildModel): Boolean;
  begin
    Result := (AReach.Column >= ChildModel.FirstCol)
      and (AReach.Column <= ChildModel.LastCol)
      and (AReach.Row >= ChildModel.FirstRow)
      and (AReach.Row <= ChildModel.LastRow)
      and ((LocalModel.Grid.LayerCount -1 = ChildModel.Discretization.BottomLayerIndex)
      or (AReach.Layer < ChildModel.Discretization.BottomLayerIndex))
  end;
  function GetModel(AReach: TSfr_Cell): TCustomModel;
  var
    ChildIndex: Integer;
    ChildModel: TChildModel;
  begin
    result := LocalModel;
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ReachInModel(AReach, ChildModel) then
      begin
        result := ChildModel;
        Exit;
      end;
    end;
  end;
  function ReachInOverlap(AReach: TSfr_Cell): Boolean;
  begin

    if Model.ModelSelection in [msModflowLGR] then
    begin
      result := (AReach.Column < OverlapWidth)
        or (AReach.Row < OverlapWidth)
        or (AReach.Column >= MaxOverlappedColumn)
        or (AReach.Row >= MaxOverlappedRow)
        or (AReach.Layer >= MaxOverlappedLayer)
    end
    else
    begin
      Assert(Model.ModelSelection in [msModflowLGR2, msModflowFmp]);
      result := False;
    end;
  end;
begin
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for SegmentIndex := 0 to SegmentCount - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        CurrentModel := nil;
        SubSeg := nil;
        for ReachIndex := 0 to ASegment.ReachCount - 1 do
        begin
          AReach := ASegment.Reaches[ReachIndex] as TSfr_Cell;
          if (CurrentModel = nil) or (CurrentModel = LocalModel)
            or not ReachInModel(AReach, (CurrentModel as TChildModel)) then
          begin
            CurrentModel := GetModel(AReach);
          end;

          if (SubSeg = nil) or (SubSeg.FModel <> CurrentModel) then
          begin
            SubSeg := ASegment.AddSubSegment(CurrentModel,
              LocalModel.ModflowFullStressPeriods.Count);
          end;
          SubSeg.AddReach(AReach);
//          AReach.Active := True;
        end;
      end;
    end;
  end
  else
  begin
    LocalChildModel := Model as TChildModel;
    if Model.ModelSelection in [msModflowLGR] then
    begin
      OverlapWidth := LocalChildModel.ChildCellsPerParentCell div 2 + 1;
    end
    else
    begin
      Assert(Model.ModelSelection in [msModflowLGR2, msModflowFmp]);
      OverlapWidth := 0;
    end;
    MaxOverlappedColumn := LocalChildModel.Grid.ColumnCount - OverlapWidth;
    MaxOverlappedRow := LocalChildModel.Grid.RowCount - OverlapWidth;
    MaxOverlappedLayer := LocalChildModel.FirstOverlappedLayer;
    for SegmentIndex := 0 to SegmentCount - 1 do
    begin
      ASegment := Segments[SegmentIndex];
      SubSeg := nil;
      for ReachIndex := 0 to ASegment.ReachCount - 1 do
      begin
        AReach := ASegment.Reaches[ReachIndex] as TSfr_Cell;
        if ReachInOverlap(AReach) then
        begin
          SubSeg := ASegment.AddSubSegment(LocalChildModel, LocalChildModel.ModflowFullStressPeriods.Count);
          SubSeg.AddReach(AReach);
          SubSeg.FChildEdge := True;
          SubSeg := nil;
        end
        else
        begin
          if SubSeg = nil then
          begin
            SubSeg := ASegment.AddSubSegment(LocalChildModel, LocalChildModel.ModflowFullStressPeriods.Count);
          end;
          SubSeg.AddReach(AReach);
        end;
      end;
    end;
  end;
end;

destructor TModflowSFR_Writer.Destroy;
begin
  FLakDictionary.Free;
  FSegDictionary.Free;
  FSegments.Free;
  FValues.Free;
  FLakes.Free;
  inherited;
end;

procedure TModflowSFR_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSfrBoundary;
  Dummy: TStringList;
  Segment: TSegment;
  Index: Integer;
  Item: TCustomModflowBoundaryItem;
  StartTime: Double;
  EndTime: Double;
  NUMVAL: Integer;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrSFR) then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSfrInvalid);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrStreamFlowFileDoe);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObject);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidStartingTimeStep1);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, Format(StrTheSEndOfTheFo, [StrUpstream]));
    frmErrorsAndWarnings.RemoveErrorGroup(Model, Format(StrTheSEndOfTheFo, [StrDownstream]));
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrIncompleteSFRData);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, DupErrorCategory);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, CircularCategory);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ChannelRoughnessError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, BankRoughnessError);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, NoSegmentsWarning);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, UnsatError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrOneOrMoreSFRStre);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDownstreamOutOfOrder);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDiversionOutOfOrder);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeDownstreamError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeDiversionError);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoStreamsDefined);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDupParamInstances);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeSeparationBet);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeDiversionSeparation);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInactiveReach);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrReachSeparationWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighSFRHydraulicC);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrZeroSFRHydraulicC);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDuplicateSFRStream);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreReaches);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrWhenUnsaturatedFlo);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheMinimumZValue);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheSFRPackageIsN);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSFRChildModelLink);

    if Model.ModflowPackages.Mt3dBasic.IsSelected
      and (Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheSFRPackageIsN,
        StrAlthoughMT3DMSVers);
    end;

    StartTime := Model.ModflowStressPeriods[0].StartTime;
    EndTime := Model.ModflowStressPeriods[
      Model.ModflowStressPeriods.Count-1].EndTime;

    NUMTAB := 0;
    MAXVAL := 0;
    frmProgressMM.AddMessage(StrEvaluatingSFRPacka);
    ISFROPT := (Package as TSfrPackageSelection).Isfropt;
    Dummy := TStringList.Create;
    try
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
        Boundary := ScreenObject.ModflowSfrBoundary;
        if (Boundary = nil) or not Boundary.Used then
        begin
          Continue;
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        Assert(Boundary.Values.Count = 1);
        Item := Boundary.Values[0] as TCustomModflowBoundaryItem;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        Boundary.GetCellValues(FValues, Dummy, Model, self);

        if (FValues.Count >= 1) then
        begin
          Assert(FValues.Count = 1);
          Segment := TSegment.Create;
          Segment.FReaches := FValues.Extract(FValues[0]);
          if Segment.FReaches.Count > 0 then
          begin
            FSegments.Add(Segment);
            Segment.FScreenObject := ScreenObject;
            Segment.FExternalFlow := Boundary.ExternalFlow;
            NUMVAL := 0;
            case Boundary.ExternalFlow.FlowFileChoice of
              ffcNone:
                begin
                  NUMVAL := 0;
                end;
              ffcFileName:
                begin
                  if FileExists(Boundary.ExternalFlow.FullFlowFileName) then
                  begin
                    Dummy.LoadFromFile(Boundary.ExternalFlow.FullFlowFileName);
                    NUMVAL := Dummy.Count;
                  end
                  else
                  begin
                    NUMVAL := 0;
                    frmErrorsAndWarnings.AddError(Model, StrTheFollowingObject,
                      ScreenObject.Name, ScreenObject);
                  end;
                end;
              ffcSpecify:
                begin
                  NUMVAL := Boundary.ExternalFlow.FlowFileData.Count;
                end;
            end;
            if NUMVAL > 0 then
            begin
              Inc(NUMTAB);
              if NUMVAL > MAXVAL then
              begin
                MAXVAL := NUMVAL;
              end;
            end;
          end
          else
          begin
            frmErrorsAndWarnings.AddWarning(Model,
              NoSegmentsWarning, ScreenObject.Name, ScreenObject);
            Segment.Free;
          end;
        end;
      end;
    finally
      Dummy.Free;
    end;
    SortSegments;
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      Segment.NewSegmentNumber := Index + 1;
    end;
    TestBedElevations;
    CheckParamInstances;

    // LGR
    CreateLgrSubSegments;
    RenumberLgrSubSegments;

    if not FIsChildModel then
    begin
      CheckParameterSegments;
      CheckNonParamSegments;
    end;

    if FSegments.Count = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNoStreamsDefined,
        StrTheSFRPackageHas);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowSFR_Writer.Extension: string;
begin
  result := '.sfr';
end;

function TModflowSFR_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SfrPackage;
end;

procedure TModflowSFR_Writer.RenumberLgrSubSegments;
var
  Index: Integer;
  Segment: TSegment;
  Number: Integer;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
begin
  if (not (Model is TPhastModel)) or TPhastModel(Model).LgrUsed then
  begin
    Number := 1;
    for Index := 0 to SegmentCount - 1 do
    begin
      Segment := Segments[Index];
      Segment.NewSegmentNumber := Number;
      if Segment.FSubSegmentList.Count = 0 then
      begin
        Inc(Number);
      end
      else
      begin
        for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
        begin
          SubSeg := Segment.FSubSegmentList[SubSegIndex];
          SubSeg.FSegmentNumber := Number;
          Inc(Number);
        end;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.SortSegments;
var
  SortedSegments: TList;
  Segment, Segment1, Segment2: TSegment;
  Index: Integer;
  Error: string;
  SegmentsFound: boolean;
  AnotherSegment: TSegment;
  SegmentNumberArray: TIntegerDynArray;
  OutIndex: integer;
  DownStreamOutOfOrder: TList;
  DiversionOutOfOrder: TList;
  WarningIndex: Integer;
  AScreenObject: TScreenObject;
  LakeDownstreamError: TList;
  LakeDiversionError: TList;
  UzfPackage: TUzfPackageSelection;
  function FindSegment(Number: integer): TSegment;
  var
    Index: Integer;
    Segment: TSegment;
  begin
    result := nil;
    for Index := 0 to SegmentCount - 1 do
    begin
      Segment := Segments[Index];
      if (Segment <> nil) and (Segment.OriginalSegmentNumber = Number) then
      begin
        result := Segment;
        Exit;
      end;
    end;
  end;
begin
  // sort the segments in order of their original segment number.
  FSegments.Sort(CompareSegments);

  Error := '';
  for Index := 0 to FSegments.Count - 2 do
  begin
    Segment1 := FSegments[Index];
    Segment2 := FSegments[Index+1];
    if Segment1.OriginalSegmentNumber = Segment2.OriginalSegmentNumber then
    begin
      Error := Format(DupNumbersError, [Segment1.FScreenObject.Name,
        Segment2.FScreenObject.Name, Segment1.OriginalSegmentNumber]);
      frmErrorsAndWarnings.AddError(Model, DupErrorCategory, Error,
      Segment1.FScreenObject);
    end;
  end;

  if not Model.ModflowPackages.LakPackage.IsSelected then
  begin
    LakeDownstreamError := TList.Create;
    LakeDiversionError := TList.Create;
    try
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
        for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
        begin
          if (SegmentNumberArray[OutIndex] < 0) then
          begin
            LakeDownstreamError.Add(Segment.FScreenObject);
            break;
          end;
        end;

        SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
        for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
        begin
          if (SegmentNumberArray[OutIndex] < 0) then
          begin
            LakeDiversionError.Add(Segment.FScreenObject);
            break;
          end;
        end;
      end;
      for WarningIndex := 0 to LakeDownstreamError.Count - 1 do
      begin
        AScreenObject := LakeDownstreamError[WarningIndex];
        frmErrorsAndWarnings.AddError(Model,
          StrLakeDownstreamError, AScreenObject.Name, AScreenObject);
      end;
      for WarningIndex := 0 to LakeDiversionError.Count - 1 do
      begin
        AScreenObject := LakeDiversionError[WarningIndex];
        frmErrorsAndWarnings.AddError(Model,
          StrLakeDiversionError, AScreenObject.Name, AScreenObject);
      end;
    finally
      LakeDiversionError.Free;
      LakeDownstreamError.Free;
    end;
  end;

  // don't renumber the segments if they are already in
  // numerical order.
  if (Error = '') and (FSegments.Count > 0) then
  begin
    Segment1 := FSegments[0];
    if Segment1.OriginalSegmentNumber = 1 then
    begin
      Segment2 := FSegments[FSegments.Count - 1];
      if Segment2.OriginalSegmentNumber = FSegments.Count then
      begin
        // The segments are in numerical order.
        // Check for errors and leave the order unchanged.
        DownStreamOutOfOrder := TList.Create;
        DiversionOutOfOrder := TList.Create;
        try
          for Index := 0 to FSegments.Count - 1 do
          begin
            Segment := FSegments[Index];
            SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
            for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
            begin
              if (SegmentNumberArray[OutIndex] > 0) then
              begin
                if (SegmentNumberArray[OutIndex] > FSegments.Count)
                or (SegmentNumberArray[OutIndex] < Segment.OriginalSegmentNumber) then
                begin
                  if DownStreamOutOfOrder.IndexOf(Segment.FScreenObject) < 0 then
                  begin
                    DownStreamOutOfOrder.Add(Segment.FScreenObject)
                  end;
                end;
              end;
            end;
            SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
            for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
            begin
              if (SegmentNumberArray[OutIndex] > 0) then
              begin
                if (SegmentNumberArray[OutIndex] > FSegments.Count)
                or (SegmentNumberArray[OutIndex] > Segment.OriginalSegmentNumber) then
                begin
                  if DiversionOutOfOrder.IndexOf(Segment.FScreenObject) < 0 then
                  begin
                    DiversionOutOfOrder.Add(Segment.FScreenObject)
                  end;
                end;
              end;
            end;
          end;
          for WarningIndex := 0 to DownStreamOutOfOrder.Count - 1 do
          begin
            AScreenObject := DownStreamOutOfOrder[WarningIndex];
            frmErrorsAndWarnings.AddWarning(Model,
              StrDownstreamOutOfOrder, AScreenObject.Name, AScreenObject);
          end;
          for WarningIndex := 0 to DiversionOutOfOrder.Count - 1 do
          begin
            AScreenObject := DiversionOutOfOrder[WarningIndex];
            frmErrorsAndWarnings.AddWarning(Model,
              StrDiversionOutOfOrder, AScreenObject.Name, AScreenObject);
          end;
        finally
          DownStreamOutOfOrder.Free;
          DiversionOutOfOrder.Free;
        end;
        Exit;
      end;
    end;
  end;

  UzfPackage := Model.ModflowPackages.UzfPackage;
  if UzfPackage.IsSelected and UzfPackage.RouteDischargeToStreams then
  begin
    Exit;
  end;

  (FSegments as TObjectList).OwnsObjects := False;

  SortedSegments := TObjectList.Create;
  SortedSegments.Capacity := FSegments.Count;

  repeat
    SegmentsFound := False;
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      Segment.FHasTributaries := False;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
      for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
      begin
        Segment := FindSegment(SegmentNumberArray[OutIndex]);
        if Segment <> nil then
        begin
          Segment.FHasTributaries := True;
        end;
      end;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
      for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
      begin
        AnotherSegment := FindSegment(SegmentNumberArray[OutIndex]);
        if AnotherSegment <> nil then
        begin
          Segment.FHasTributaries := True;
          break;
        end;
      end;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      if not Segment.FHasTributaries then
      begin
        SortedSegments.Add(Segment);
        FSegments[Index] := nil;
        SegmentsFound := True;
      end;
    end;

    FSegments.Pack;
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
    end;
  end;
  (FSegments as TObjectList).OwnsObjects := True;
  FSegments.Free;

  FSegments := SortedSegments;
end;

procedure TModflowSFR_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  LgrUsed: Boolean;
  ChildIndex: Integer;
  LocalModel: TPhastModel;
  ChildModel: TChildModel;
  DisplayLists: TModflowBoundListOfTimeLists;
  ChildSfrWriter: TModflowSFR_Writer;
  WriterList : TSfrWriterList;
  ChildTimeLists: TList;
  WriterObjectList: TList;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    LgrUsed := (not (Model is TPhastModel)) or TPhastModel(Model).LgrUsed;
    if LgrUsed then
    begin
      if Model is TPhastModel then
      begin
        Evaluate;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        ChildTimeLists := TObjectList.Create;
        WriterList := TSfrWriterList.Create;
        WriterObjectList := TObjectList.Create;
        try
          WriterList.Add(self);
          LocalModel := TPhastModel(Model);
          for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;

            ChildSfrWriter := TModflowSFR_Writer.Create(ChildModel, etDisplay);
            WriterObjectList.Add(ChildSfrWriter);
            WriterList.Add(ChildSfrWriter);
            DisplayLists := TModflowBoundListOfTimeLists.Create;
            ChildTimeLists.Add(DisplayLists);
            ChildModel.ModflowPackages.SfrPackage.GetDisplayLists(DisplayLists);
            ChildSfrWriter.Evaluate;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
          end;

          AssociateLgrSubSegments(WriterList);
          InternalUpdateDisplay(TimeLists);
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;

          Assert(WriterObjectList.Count = ChildTimeLists.Count);
          for ChildIndex := 0 to ChildTimeLists.Count - 1 do
          begin
            DisplayLists := ChildTimeLists[ChildIndex];
            ChildSfrWriter := WriterObjectList[ChildIndex];
            ChildSfrWriter.InternalUpdateDisplay(DisplayLists);
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
            ChildModel.ModflowPackages.SfrPackage.ComputeAverages(DisplayLists);
          end;

        finally
          WriterList.Free;
          ChildTimeLists.Free;
          WriterObjectList.Free;
        end;
      end
      else
      begin
        Exit;
      end;
    end
    else
    begin

      Evaluate;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      InternalUpdateDisplay(TimeLists);
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;

    frmProgressMM.AddMessage(StrWritingSFRPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1a;
    WriteDataSet1b;
    WriteDataSet1c;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3and4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets5to7);
    WriteDataSets5to7;
  finally
    CloseFile;
  end;
end;

procedure TModflowSFR_Writer.CheckStreamBottomElevation(
  ScreenObject: TScreenObject; Grid: TModflowGrid; SfrReach: TSfr_Cell);
var
  CellBottom: Real;
  ShouldCheck: Boolean;
begin
  Isfropt := Model.ModflowPackages.SfrPackage.Isfropt;
  ShouldCheck := Isfropt in [1, 2, 3];
  if ShouldCheck  then
  begin
    CellBottom := Grid.CellElevation[
      SfrReach.Column, SfrReach.Row, SfrReach.Layer + 1];
    if SfrReach.StreambedElevation - SfrReach.StreamBedThickness < CellBottom then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheBottomOfTheSt,
        Format(StrObject0sLayerThick, [ScreenObject.Name, SfrReach.Layer + 1,
        SfrReach.Row + 1, SfrReach.Column + 1, SfrReach.StreambedElevation,
        SfrReach.StreamBedThickness, CellBottom]), ScreenObject);
    end;
  end;
end;

procedure TModflowSFR_Writer.InternalUpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SegmentNumberTimes: TModflowBoundaryDisplayTimeList;
  ReachNumberTimes: TModflowBoundaryDisplayTimeList;
  SegmentIndex: Integer;
  Segment: TSegment;
  ReachIndex: Integer;
  Reach: TSfr_Cell;
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  Boundary: TSfrBoundary;
  Item: TSfrParamIcalcItem;
  ICalcTimeList: TModflowBoundaryDisplayTimeList;
  SegmentComment, ReachComment, ICALC_Comment, DownstreamComment: string;
  ReachLengthList: TModflowBoundaryDisplayTimeList;
  StreamElevationList: TModflowBoundaryDisplayTimeList;
  StreamSlopeList: TModflowBoundaryDisplayTimeList;
  StreamThicknessList: TModflowBoundaryDisplayTimeList;
  StreamKList: TModflowBoundaryDisplayTimeList;
  SatWatContent: TModflowBoundaryDisplayTimeList;
  InitWatContent: TModflowBoundaryDisplayTimeList;
  BrooksCorey: TModflowBoundaryDisplayTimeList;
  UnSatKz: TModflowBoundaryDisplayTimeList;
  OutSeg: TModflowBoundaryDisplayTimeList;
  DiversionSeg: TModflowBoundaryDisplayTimeList;
  ADisplayList: TModflowBoundaryDisplayTimeList;
  Index: Integer;
  DiversionComment, IpriorComment: string;
  IpriorList: TModflowBoundaryDisplayTimeList;
  FlowList: TModflowBoundaryDisplayTimeList;
  FlowRecord: TSfrSegmentFlowRecord;
  RunOffList: TModflowBoundaryDisplayTimeList;
  PrecipitationList: TModflowBoundaryDisplayTimeList;
  EvapotranspirationList: TModflowBoundaryDisplayTimeList;
  ChannelRoughnessList: TModflowBoundaryDisplayTimeList;
  ChannelRecord: TSfrChannelRecord;
  BankRoughnessList: TModflowBoundaryDisplayTimeList;
  DepthCoefficientList: TModflowBoundaryDisplayTimeList;
  DepthExponentList: TModflowBoundaryDisplayTimeList;
  WidthCoefficientList: TModflowBoundaryDisplayTimeList;
  WidthExponentList: TModflowBoundaryDisplayTimeList;
  EquationRecord: TSfrEquationRecord;
  UpstreamHydraulicConductivityList: TModflowBoundaryDisplayTimeList;
  DownstreamHydraulicConductivityList: TModflowBoundaryDisplayTimeList;
  UpstreamValues: TSfrSegmentStorage;
  UpstreamRecord: TSfrSegmentRecord;
  DownstreamValues: TSfrSegmentStorage;
  DownstreamRecord: TSfrSegmentRecord;
  UpstreamWidthList: TModflowBoundaryDisplayTimeList;
  DownstreamWidthList: TModflowBoundaryDisplayTimeList;
  UpstreamThicknessList: TModflowBoundaryDisplayTimeList;
  DownstreamThicknessList: TModflowBoundaryDisplayTimeList;
  UpstreamElevationList: TModflowBoundaryDisplayTimeList;
  DownstreamElevationList: TModflowBoundaryDisplayTimeList;
  UpstreamDepthList: TModflowBoundaryDisplayTimeList;
  DownstreamDepthList: TModflowBoundaryDisplayTimeList;

  UpstreamUnSatWatContList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatWatContList: TModflowBoundaryDisplayTimeList;
  UpstreamUnSatInitWatContList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatInitWatContList: TModflowBoundaryDisplayTimeList;
  UpstreamBrooksCoreyList: TModflowBoundaryDisplayTimeList;
  DownstreamBrooksCoreyList: TModflowBoundaryDisplayTimeList;
  UpstreamUnSatKzList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatKzList: TModflowBoundaryDisplayTimeList;
  UpstreamUnsatValues: TSfrUnsatSegmentStorage;
  UpstreamUnsatRecord: TSfrUnsatSegmentRecord;
  DownstreamUnsatValues: TSfrUnsatSegmentStorage;
  DownstreamUnsatRecord: TSfrUnsatSegmentRecord;
  Param: TModflowTransientListParameter;
  KAnnotation: string;
  ErrorObject: TScreenObject;
  SubSegIndex: integer;
  SubSeg: TSubSegment;
  function WidthValueUsed: boolean;
  begin
    result := True;
    case ISFROPT of
      0,1:
        begin
          result := Item.ICalc <= 1;
        end;
      2,3,4,5:
        begin
          if Item.ICalc <= 0 then
          begin
            result := True;
          end
          else if Item.ICalc = 1 then
          begin
            result := TimeIndex = 0;
          end
          else
          begin
            result := False;
          end;
        end;
      else
        Assert(False);
    end;
  end;
  function ThicknessElevUsed: boolean;
  begin
    result := True;
    case ISFROPT of
      0,1:
        begin
          result := True;
        end;
      2,3:
        begin
          result := False;
        end;
      4,5:
        begin
          if Item.ICalc <= 0 then
          begin
            result := True;
          end
          else if Item.ICalc in [1,2] then
          begin
            result := TimeIndex = 0;
          end
          else
          begin
            result := True;
          end;
        end;
      else
        Assert(False);
    end;
  end;
  procedure AssignReachValues;
  var
    TimeIndex: integer;
    LastReach: Boolean;
  begin
    for TimeIndex := 0 to SegmentNumberTimes.Count - 1 do
    begin
      DataArray := SegmentNumberTimes[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      if SubSeg <> nil then
      begin
        DataArray.AddDataValue(SegmentComment, SubSeg.SegmentNumber,
          Reach.Column, Reach.Row, Reach.Layer);
      end
      else
      begin
        DataArray.AddDataValue(SegmentComment, Segment.OriginalSegmentNumber,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to OutSeg.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        OutSeg.Times[TimeIndex]);
      if Item <> nil then
      begin
        DataArray := OutSeg[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(DownstreamComment,
          Item.OutflowSegment,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to DiversionSeg.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        DiversionSeg.Times[TimeIndex]);
      if Item <> nil then
      begin
        DataArray := DiversionSeg[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(DiversionComment,
          Item.DiversionSegment,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to IpriorList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        IpriorList.Times[TimeIndex]);
      if Item <> nil then
      begin
        if Item.DiversionSegment <> 0 then
        begin
          DataArray := IpriorList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(IpriorComment,
            Item.IPRIOR,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
    end;
    for TimeIndex := 0 to ReachNumberTimes.Count - 1 do
    begin
      DataArray := ReachNumberTimes[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(ReachComment, ReachIndex + 1,
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    for TimeIndex := 0 to ICalcTimeList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        ICalcTimeList.Times[TimeIndex]);
      if Item <> nil then
      begin
        DataArray := ICalcTimeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(ICALC_Comment, Item.ICalc,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to ReachLengthList.Count - 1 do
    begin
      DataArray := ReachLengthList[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.AddDataValue(Reach.ReachLengthAnnotation, Reach.ReachLength,
        Reach.Column, Reach.Row, Reach.Layer);
    end;
    if ISFROPT in [1,2,3] then
    begin
      for TimeIndex := 0 to StreamElevationList.Count - 1 do
      begin
        DataArray := StreamElevationList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.StreambedElevationAnnotation,
          Reach.StreambedElevation,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to StreamSlopeList.Count - 1 do
      begin
        DataArray := StreamSlopeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.StreamSlopeAnnotation,
          Reach.StreamSlope,
          Reach.Column, Reach.Row, Reach.Layer);
        if Reach.StreamSlope <= 0 then
        begin
          frmErrorsAndWarnings.AddError(Model, StrOneOrMoreSFRStre,
            Format(ZeroSlope,
            [Segment.FScreenObject.Name,
            Reach.Layer+1, Reach.Row + 1, Reach.Column + 1]),
            Segment.FScreenObject);
        end;
      end;
      for TimeIndex := 0 to StreamThicknessList.Count - 1 do
      begin
        DataArray := StreamThicknessList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.StreamBedThicknessAnnotation,
          Reach.StreamBedThickness,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to StreamKList.Count - 1 do
      begin
        DataArray := StreamKList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.HydraulicConductivityAnnotation,
          Reach.HydraulicConductivity,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    if ISFROPT in [2,3] then
    begin
      for TimeIndex := 0 to SatWatContent.Count - 1 do
      begin
        DataArray := SatWatContent[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.SaturatedWaterContentAnnotation,
          Reach.SaturatedWaterContent,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to InitWatContent.Count - 1 do
      begin
        DataArray := InitWatContent[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.InitialWaterContentAnnotation,
          Reach.InitialWaterContent,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to BrooksCorey.Count - 1 do
      begin
        DataArray := BrooksCorey[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.BrooksCoreyExponentAnnotation,
          Reach.BrooksCoreyExponent,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    if ISFROPT = 3 then
    begin
      for TimeIndex := 0 to UnSatKz.Count - 1 do
      begin
        DataArray := UnSatKz[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.VerticalKAnnotation,
          Reach.VerticalK,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to FlowList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        FlowList.Times[TimeIndex]);
      if Item <> nil then
      begin
        FlowRecord := Boundary.SegmentFlows.
          GetFlowValuesFromTime(FlowList.Times[TimeIndex]);
        DataArray := FlowList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(FlowRecord.FlowAnnotation,
          FlowRecord.Flow,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to RunOffList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        RunOffList.Times[TimeIndex]);
      if Item <> nil then
      begin
        FlowRecord := Boundary.SegmentFlows.
          GetFlowValuesFromTime(RunOffList.Times[TimeIndex]);
        DataArray := RunOffList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(FlowRecord.RunnoffAnnotation,
          FlowRecord.Runnoff,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to PrecipitationList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        PrecipitationList.Times[TimeIndex]);
      if Item <> nil then
      begin
        FlowRecord := Boundary.SegmentFlows.
          GetFlowValuesFromTime(PrecipitationList.Times[TimeIndex]);
        DataArray := PrecipitationList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(FlowRecord.PrecipitationAnnotation,
          FlowRecord.Precipitation,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to EvapotranspirationList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        EvapotranspirationList.Times[TimeIndex]);
      if Item <> nil then
      begin
        FlowRecord := Boundary.SegmentFlows.
          GetFlowValuesFromTime(EvapotranspirationList.Times[TimeIndex]);
        DataArray := EvapotranspirationList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(FlowRecord.EvapotranspirationAnnotation,
          FlowRecord.Evapotranspiration,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to ChannelRoughnessList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        ChannelRoughnessList.Times[TimeIndex]);
      if (Item <> nil) and (Item.ICalc in [1,2]) then
      begin
        ChannelRecord := Boundary.ChannelValues.
          GetChannelTimeValuesFromTime(Model, ChannelRoughnessList.Times[TimeIndex]);
        DataArray := ChannelRoughnessList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(ChannelRecord.ChannelRoughnessAnnotation,
          ChannelRecord.ChannelRoughness,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to BankRoughnessList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        BankRoughnessList.Times[TimeIndex]);
      if  (Item <> nil) and (Item.ICalc = 2) then
      begin
        ChannelRecord := Boundary.ChannelValues.
          GetChannelTimeValuesFromTime(Model, BankRoughnessList.Times[TimeIndex]);
        DataArray := BankRoughnessList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(ChannelRecord.BankRoughnessAnnotation,
          ChannelRecord.BankRoughness,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to DepthCoefficientList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        DepthCoefficientList.Times[TimeIndex]);
      if  (Item <> nil) and (Item.ICalc = 3) then
      begin
        EquationRecord := Boundary.EquationValues.
          GetEquationTimeValuesFromTime(DepthCoefficientList.Times[TimeIndex]);
        DataArray := DepthCoefficientList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(EquationRecord.DepthCoefficientAnnotation,
          EquationRecord.DepthCoefficient,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to DepthExponentList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        DepthExponentList.Times[TimeIndex]);
      if  (Item <> nil) and (Item.ICalc = 3) then
      begin
        EquationRecord := Boundary.EquationValues.
          GetEquationTimeValuesFromTime(DepthExponentList.Times[TimeIndex]);
        DataArray := DepthExponentList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(EquationRecord.DepthExponentAnnotation,
          EquationRecord.DepthExponent,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to WidthCoefficientList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        WidthCoefficientList.Times[TimeIndex]);
      if  (Item <> nil) and (Item.ICalc = 3) then
      begin
        EquationRecord := Boundary.EquationValues.
          GetEquationTimeValuesFromTime(WidthCoefficientList.Times[TimeIndex]);
        DataArray := WidthCoefficientList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(EquationRecord.WidthCoefficientAnnotation,
          EquationRecord.WidthCoefficient,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    for TimeIndex := 0 to WidthExponentList.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.GetItemByStartTime(
        WidthExponentList.Times[TimeIndex]);
      if  (Item <> nil) and (Item.ICalc = 3) then
      begin
        EquationRecord := Boundary.EquationValues.
          GetEquationTimeValuesFromTime(WidthExponentList.Times[TimeIndex]);
        DataArray := WidthExponentList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(EquationRecord.WidthExponentAnnotation,
          EquationRecord.WidthExponent,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
    end;
    if SubSeg = nil then
    begin
      LastReach := (ReachIndex = Segment.FReaches.Count - 1)
    end
    else
    begin
      LastReach := (ReachIndex = SubSeg.ReachCount - 1)
    end;
    if (ISFROPT in [0,4,5])  then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamHydraulicConductivityList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DepthExponentList.Times[TimeIndex]);
          if (Item <> nil) then
          begin
            Param := nil;
            if Item.Param <> '' then
            begin
              Param := Model.ModflowTransientParameters.GetParamByName(Item.Param);
            end;
            UpstreamValues := Boundary.UpstreamSegmentValues.
              GetBoundaryByStartTime(UpstreamHydraulicConductivityList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if UpstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTime, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(UpstreamValues.SrfSegmentArray) > 0 then
              begin
                UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FUpstreamValues) > 0 then
              begin
                UpstreamRecord := SubSeg.FUpstreamValues[0];
                UpstreamRecord.HydraulicConductivityAnnotation
                  := Format(StrInterpolatedFromS,
                  [UpstreamValues.SrfSegmentArray[0].HydraulicConductivityAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
//              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            DataArray := UpstreamHydraulicConductivityList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            if Param = nil then
            begin
              DataArray.AddDataValue(UpstreamRecord.HydraulicConductivityAnnotation,
                UpstreamRecord.HydraulicConductivity,
                Reach.Column, Reach.Row, Reach.Layer);
            end
            else
            begin
              KAnnotation := Format(Str0sMultipliedByP,
                [UpstreamRecord.HydraulicConductivityAnnotation, Item.Param]);
              DataArray.AddDataValue(
                KAnnotation,
                UpstreamRecord.HydraulicConductivity * Param.Value,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamHydraulicConductivityList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DepthExponentList.Times[TimeIndex]);
          if (Item <> nil) then
          begin
            Param := nil;
            if Item.Param <> '' then
            begin
              Param := Model.ModflowTransientParameters.GetParamByName(Item.Param);
            end;
            DownstreamValues := Boundary.DownstreamSegmentValues.
              GetBoundaryByStartTime(DownstreamHydraulicConductivityList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if DownstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(DownstreamValues.SrfSegmentArray) > 0 then
              begin
                DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FDownstreamValues) > 0 then
              begin
                DownstreamRecord := SubSeg.FDownstreamValues[0];
                DownstreamRecord.HydraulicConductivityAnnotation
                  := Format(StrInterpolatedFromS,
                  [DownstreamValues.SrfSegmentArray[0].HydraulicConductivityAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
            DataArray := DownstreamHydraulicConductivityList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            if Param = nil then
            begin
              DataArray.AddDataValue(DownstreamRecord.HydraulicConductivityAnnotation,
                DownstreamRecord.HydraulicConductivity,
                Reach.Column, Reach.Row, Reach.Layer);
            end
            else
            begin
              KAnnotation := Format(Str0sMultipliedByP,
                [DownstreamRecord.HydraulicConductivityAnnotation, Item.Param]);
              DataArray.AddDataValue(
                KAnnotation,
                DownstreamRecord.HydraulicConductivity * Param.Value,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
    end;

    if (ReachIndex = 0) then
    begin
      for TimeIndex := 0 to UpstreamWidthList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          UpstreamWidthList.Times[TimeIndex]);
        if (Item <> nil) and WidthValueUsed then
        begin
          UpstreamValues := Boundary.UpstreamSegmentValues.
            GetBoundaryByStartTime(UpstreamWidthList.Times[TimeIndex], Model)
            as TSfrSegmentStorage;
          if UpstreamValues = nil then
          begin
            ErrorObject := Boundary.ScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddError(Model,
              StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
            Continue;
          end;
          if SubSeg = nil then
          begin
            if Length(UpstreamValues.SrfSegmentArray) > 0 then
            begin
              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrUpstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end
          else
          begin
            if Length(SubSeg.FUpstreamValues) > 0 then
            begin
              UpstreamRecord := SubSeg.FUpstreamValues[0];
              UpstreamRecord.StreamWidthAnnotation
                := Format(StrInterpolatedFromS,
                [UpstreamValues.SrfSegmentArray[0].StreamWidthAnnotation]);
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end;
          DataArray := UpstreamWidthList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(UpstreamRecord.StreamWidthAnnotation,
            UpstreamRecord.StreamWidth,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
    end;
    if LastReach then
    begin
      for TimeIndex := 0 to DownstreamWidthList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          DownstreamWidthList.Times[TimeIndex]);
        if (Item <> nil) and WidthValueUsed then
        begin
          DownstreamValues := Boundary.DownstreamSegmentValues.
            GetBoundaryByStartTime(DownstreamWidthList.Times[TimeIndex], Model)
            as TSfrSegmentStorage;
          if DownstreamValues = nil then
          begin
            ErrorObject := Boundary.ScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddError(Model,
              StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
            Continue;
          end;
          if SubSeg = nil then
          begin
            if Length(DownstreamValues.SrfSegmentArray) > 0 then
            begin
              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrDownstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end
          else
          begin
            if Length(SubSeg.FDownstreamValues) > 0 then
            begin
              DownstreamRecord := SubSeg.FDownstreamValues[0];
              DownstreamRecord.StreamWidthAnnotation
                := Format(StrInterpolatedFromS,
                  [DownstreamValues.SrfSegmentArray[0].StreamWidthAnnotation]);
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrDownstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end;
//            DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
          DataArray := DownstreamWidthList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(DownstreamRecord.StreamWidthAnnotation,
            DownstreamRecord.StreamWidth,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
    end;
    if ISFROPT in [0,4,5] then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamThicknessList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamThicknessList.Times[TimeIndex]);
          if (Item <> nil) and ThicknessElevUsed then
          begin
            UpstreamValues := Boundary.UpstreamSegmentValues.
              GetBoundaryByStartTime(UpstreamThicknessList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if UpstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(UpstreamValues.SrfSegmentArray) > 0 then
              begin
                UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FUpstreamValues) > 0 then
              begin
                UpstreamRecord := SubSeg.FUpstreamValues[0];
                UpstreamRecord.StreamBedThicknessAnnotation
                  := Format(StrInterpolatedFromS,
                  [UpstreamValues.SrfSegmentArray[0].StreamBedThicknessAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
//              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            DataArray := UpstreamThicknessList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(UpstreamRecord.StreamBedThicknessAnnotation,
              UpstreamRecord.StreamBedThickness,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamThicknessList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamThicknessList.Times[TimeIndex]);
          if (Item <> nil) and ThicknessElevUsed then
          begin
            DownstreamValues := Boundary.DownstreamSegmentValues.
              GetBoundaryByStartTime(DownstreamThicknessList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if DownstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(DownstreamValues.SrfSegmentArray) > 0 then
              begin
                DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FDownstreamValues) > 0 then
              begin
                DownstreamRecord := SubSeg.FDownstreamValues[0];
                DownstreamRecord.StreamBedThicknessAnnotation
                  := Format(StrInterpolatedFromS,
                  [DownstreamValues.SrfSegmentArray[0].StreamBedThicknessAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
//              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            DataArray := DownstreamThicknessList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(DownstreamRecord.StreamBedThicknessAnnotation,
              DownstreamRecord.StreamBedThickness,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
    end;
    if ISFROPT in [0,4,5] then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamElevationList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamElevationList.Times[TimeIndex]);
          if (Item <> nil) and ThicknessElevUsed then
          begin
            UpstreamValues := Boundary.UpstreamSegmentValues.
              GetBoundaryByStartTime(UpstreamElevationList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if UpstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(UpstreamValues.SrfSegmentArray) > 0 then
              begin
                UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FUpstreamValues) > 0 then
              begin
                UpstreamRecord := SubSeg.FUpstreamValues[0];
                UpstreamRecord.StreambedElevationAnnotation
                  := Format(StrInterpolatedFromS,
                  [UpstreamValues.SrfSegmentArray[0].StreambedElevationAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrUpstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
//              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            DataArray := UpstreamElevationList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(UpstreamRecord.StreambedElevationAnnotation,
              UpstreamRecord.StreambedElevation,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamElevationList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamElevationList.Times[TimeIndex]);
          if (Item <> nil) and ThicknessElevUsed then
          begin
            DownstreamValues := Boundary.DownstreamSegmentValues.
              GetBoundaryByStartTime(DownstreamElevationList.Times[TimeIndex], Model)
              as TSfrSegmentStorage;
            if DownstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if SubSeg = nil then
            begin
              if Length(DownstreamValues.SrfSegmentArray) > 0 then
              begin
                DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end
            else
            begin
              if Length(SubSeg.FDownstreamValues) > 0 then
              begin
                DownstreamRecord := SubSeg.FDownstreamValues[0];
                DownstreamRecord.StreambedElevationAnnotation
                  := Format(StrInterpolatedFromS,
                  [DownstreamValues.SrfSegmentArray[0].StreambedElevationAnnotation]);
              end
              else
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  Format(StrTheSEndOfTheFo, [StrDownstream]),
                  ErrorObject.Name, ErrorObject);
                Continue;
              end;
            end;
//              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            DataArray := DownstreamElevationList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(DownstreamRecord.StreambedElevationAnnotation,
              DownstreamRecord.StreambedElevation,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
    end;
    if (ReachIndex = 0) then
    begin
      for TimeIndex := 0 to UpstreamDepthList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          UpstreamDepthList.Times[TimeIndex]);
        if (Item <> nil) and (Item.ICalc <= 0) then
        begin
          UpstreamValues := Boundary.UpstreamSegmentValues.
            GetBoundaryByStartTime(UpstreamDepthList.Times[TimeIndex], Model)
            as TSfrSegmentStorage;
          if UpstreamValues = nil then
          begin
            ErrorObject := Boundary.ScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddError(Model,
              StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
            Continue;
          end;
          if SubSeg = nil then
          begin
            if Length(UpstreamValues.SrfSegmentArray) > 0 then
            begin
              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrUpstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end
          else
          begin
            if Length(SubSeg.FUpstreamValues) > 0 then
            begin
              UpstreamRecord := SubSeg.FUpstreamValues[0];
              UpstreamRecord.StreamDepthAnnotation
                := Format(StrInterpolatedFromS,
                  [UpstreamValues.SrfSegmentArray[0].StreamDepthAnnotation]);
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrUpstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end;
//            UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
          DataArray := UpstreamDepthList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(UpstreamRecord.StreamDepthAnnotation,
            UpstreamRecord.StreamDepth,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
    end;
    if LastReach then
    begin
      for TimeIndex := 0 to DownstreamDepthList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          DownstreamDepthList.Times[TimeIndex]);
        if (Item <> nil) and (Item.ICalc <= 0) then
        begin
          DownstreamValues := Boundary.DownstreamSegmentValues.
            GetBoundaryByStartTime(DownstreamDepthList.Times[TimeIndex], Model)
            as TSfrSegmentStorage;
          if DownstreamValues = nil then
          begin
            ErrorObject := Boundary.ScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddError(Model,
              StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
            Continue;
          end;
          if SubSeg = nil then
          begin
            if Length(DownstreamValues.SrfSegmentArray) > 0 then
            begin
              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrDownstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end
          else
          begin
            if Length(SubSeg.FDownstreamValues) > 0 then
            begin
              DownstreamRecord := SubSeg.FDownstreamValues[0];
              DownstreamRecord.StreamDepthAnnotation
                := Format(StrInterpolatedFromS,
                  [DownstreamValues.SrfSegmentArray[0].StreamDepthAnnotation]);
            end
            else
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrDownstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end;
//            DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
          DataArray := DownstreamDepthList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(DownstreamRecord.StreamDepthAnnotation,
            DownstreamRecord.StreamDepth,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
    end;
    if ISFROPT in [4,5] then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamUnSatWatContList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamUnSatWatContList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.UpstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(UpstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FUpstreamUnsatValues) > 0 then
                begin
                  UpstreamUnsatRecord := SubSeg.FUpstreamUnsatValues[0];
                  UpstreamUnsatRecord.SaturatedWaterContentAnnotation
                    := Format(StrInterpolatedFromS,
                    [UpstreamUnsatValues.SrfUnsatSegmentArray[0].SaturatedWaterContentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
              DataArray := UpstreamUnSatWatContList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamUnsatRecord.SaturatedWaterContentAnnotation,
                UpstreamUnsatRecord.SaturatedWaterContent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamUnSatWatContList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamUnSatWatContList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.DownstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(DownstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  DownstreamUnsatRecord :=
                    DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FDownstreamUnsatValues) > 0 then
                begin
                  DownstreamUnsatRecord := SubSeg.FDownstreamUnsatValues[0];
                  DownstreamUnsatRecord.SaturatedWaterContentAnnotation
                    := Format(StrInterpolatedFromS,
                    [DownstreamUnsatValues.SrfUnsatSegmentArray[0].SaturatedWaterContentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
              DataArray := DownstreamUnSatWatContList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamUnsatRecord.SaturatedWaterContentAnnotation,
                DownstreamUnsatRecord.SaturatedWaterContent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
    end;
    if ISFROPT in [4,5] then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamUnSatInitWatContList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamUnSatInitWatContList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.UpstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(UpstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FUpstreamUnsatValues) > 0 then
                begin
                  UpstreamUnsatRecord := SubSeg.FUpstreamUnsatValues[0];
                  UpstreamUnsatRecord.InitialWaterContentAnnotation
                    := Format(StrInterpolatedFromS,
                    [UpstreamUnsatValues.SrfUnsatSegmentArray[0].InitialWaterContentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := UpstreamUnSatInitWatContList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamUnsatRecord.InitialWaterContentAnnotation,
                UpstreamUnsatRecord.InitialWaterContent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamUnSatInitWatContList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamUnSatInitWatContList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.DownstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(DownstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FDownstreamUnsatValues) > 0 then
                begin
                  DownstreamUnsatRecord := SubSeg.FDownstreamUnsatValues[0];
                  DownstreamUnsatRecord.InitialWaterContentAnnotation
                    := Format(StrInterpolatedFromS,
                    [DownstreamUnsatValues.SrfUnsatSegmentArray[0].InitialWaterContentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := DownstreamUnSatInitWatContList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamUnsatRecord.InitialWaterContentAnnotation,
                DownstreamUnsatRecord.InitialWaterContent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
    end;
    if ISFROPT in [4,5] then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamBrooksCoreyList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamBrooksCoreyList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.UpstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(UpstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FUpstreamUnsatValues) > 0 then
                begin
                  UpstreamUnsatRecord := SubSeg.FUpstreamUnsatValues[0];
                  UpstreamUnsatRecord.BrooksCoreyExponentAnnotation
                    := Format(StrInterpolatedFromS,
                    [UpstreamUnsatValues.SrfUnsatSegmentArray[0].BrooksCoreyExponentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := UpstreamBrooksCoreyList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamUnsatRecord.BrooksCoreyExponentAnnotation,
                UpstreamUnsatRecord.BrooksCoreyExponent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamBrooksCoreyList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamBrooksCoreyList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.DownstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(DownstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FDownstreamUnsatValues) > 0 then
                begin
                  DownstreamUnsatRecord := SubSeg.FDownstreamUnsatValues[0];
                  DownstreamUnsatRecord.BrooksCoreyExponentAnnotation
                    := Format(StrInterpolatedFromS,
                    [DownstreamUnsatValues.SrfUnsatSegmentArray[0].BrooksCoreyExponentAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := DownstreamBrooksCoreyList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamUnsatRecord.BrooksCoreyExponentAnnotation,
                DownstreamUnsatRecord.BrooksCoreyExponent,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
    end;
    if ISFROPT = 5 then
    begin
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamUnSatKzList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamUnSatKzList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.UpstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(UpstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FUpstreamUnsatValues) > 0 then
                begin
                  UpstreamUnsatRecord := SubSeg.FUpstreamUnsatValues[0];
                  UpstreamUnsatRecord.VerticalSaturatedKAnnotation
                    := Format(StrInterpolatedFromS,
                    [UpstreamUnsatValues.SrfUnsatSegmentArray[0].VerticalSaturatedKAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrUpstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := UpstreamUnSatKzList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamUnsatRecord.VerticalSaturatedKAnnotation,
                UpstreamUnsatRecord.VerticalSaturatedK,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if LastReach then
      begin
        for TimeIndex := 0 to DownstreamUnSatKzList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamUnSatKzList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc in [1,2]) then
          begin
            if Boundary.DownstreamUnsatSegmentValues.
              BoundaryCount[Model] = 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, UnsatError,
                Segment.FScreenObject.Name, Segment.FScreenObject);
            end
            else
            begin
              DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                Boundaries[0, Model] as TSfrUnsatSegmentStorage;
              if SubSeg = nil then
              begin
                if Length(DownstreamUnsatValues.SrfUnsatSegmentArray) > 0 then
                begin
                  DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end
              else
              begin
                if Length(SubSeg.FDownstreamUnsatValues) > 0 then
                begin
                  DownstreamUnsatRecord := SubSeg.FDownstreamUnsatValues[0];
                  DownstreamUnsatRecord.VerticalSaturatedKAnnotation
                    := Format(StrInterpolatedFromS,
                    [DownstreamUnsatValues.SrfUnsatSegmentArray[0].VerticalSaturatedKAnnotation]);
                end
                else
                begin
                  ErrorObject := Boundary.ScreenObject as TScreenObject;
                  frmErrorsAndWarnings.AddError(Model,
                    Format(StrTheSEndOfTheFo, [StrDownstream]),
                    ErrorObject.Name, ErrorObject);
                  Continue;
                end;
              end;
//                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
              DataArray := DownstreamUnSatKzList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamUnsatRecord.VerticalSaturatedKAnnotation,
                DownstreamUnsatRecord.VerticalSaturatedK,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
    end;
  end;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;


  SegmentNumberTimes := TimeLists[0];
  ReachNumberTimes := TimeLists[1];
  ICalcTimeList := TimeLists[2];
  ReachLengthList := TimeLists[3];
  StreamElevationList := TimeLists[4];
  StreamSlopeList := TimeLists[5];
  StreamThicknessList := TimeLists[6];
  StreamKList := TimeLists[7];
  SatWatContent := TimeLists[8];
  InitWatContent := TimeLists[9];
  BrooksCorey := TimeLists[10];
  UnSatKz := TimeLists[11];
  OutSeg := TimeLists[12];
  DiversionSeg := TimeLists[13];
  IpriorList := TimeLists[14];
  FlowList := TimeLists[15];
  RunOffList := TimeLists[16];
  PrecipitationList := TimeLists[17];
  EvapotranspirationList := TimeLists[18];
  ChannelRoughnessList := TimeLists[19];
  BankRoughnessList := TimeLists[20];
  DepthCoefficientList := TimeLists[21];
  DepthExponentList := TimeLists[22];
  WidthCoefficientList := TimeLists[23];
  WidthExponentList := TimeLists[24];
  UpstreamHydraulicConductivityList := TimeLists[25];
  DownstreamHydraulicConductivityList := TimeLists[26];
  UpstreamWidthList := TimeLists[27];
  DownstreamWidthList := TimeLists[28];
  UpstreamThicknessList := TimeLists[29];
  DownstreamThicknessList := TimeLists[30];
  UpstreamElevationList := TimeLists[31];
  DownstreamElevationList := TimeLists[32];
  UpstreamDepthList := TimeLists[33];
  DownstreamDepthList := TimeLists[34];
  UpstreamUnSatWatContList := TimeLists[35];
  DownstreamUnSatWatContList := TimeLists[36];
  UpstreamUnSatInitWatContList := TimeLists[37];
  DownstreamUnSatInitWatContList := TimeLists[38];
  UpstreamBrooksCoreyList := TimeLists[39];
  DownstreamBrooksCoreyList := TimeLists[40];
  UpstreamUnSatKzList := TimeLists[41];
  DownstreamUnSatKzList := TimeLists[42];

  // check that all the time lists contain the same number of times
  // as the first one.
  for Index := 1 to TimeLists.Count - 1 do
  begin
    ADisplayList := TimeLists[Index];
    Assert(SegmentNumberTimes.Count = ADisplayList.Count);
  end;

  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;

    SegmentComment := StrSegmentNumber + Segment.FScreenObject.Name;
    ReachComment := StrReachNumber + Segment.FScreenObject.Name;
    ICALC_Comment := SfrICalcNumber + Segment.FScreenObject.Name;
    DownstreamComment := StrDownstreamSegmentNumber + Segment.FScreenObject.Name;
    DiversionComment := StrDiversionSegmentNumber + Segment.FScreenObject.Name;
    IpriorComment := StrIprior + Segment.FScreenObject.Name;


    if Segment.SubSegmentList.Count > 0 then
    begin
      for SubSegIndex := 0 to Segment.SubSegmentList.Count - 1 do
      begin
        SubSeg := Segment.SubSegmentList[SubSegIndex];
        for ReachIndex := 0 to SubSeg.ReachCount - 1 do
        begin
          Reach := SubSeg.Reaches[ReachIndex];
          AssignReachValues;
        end;
      end;
    end
    else
    begin
      SubSeg := nil;
//      SubSegIndex := -1;
      for ReachIndex := 0 to Segment.FReaches.Count - 1 do
      begin
        Reach := Segment.FReaches[ReachIndex] as TSfr_Cell;
        AssignReachValues;
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
end;

procedure TModflowSFR_Writer.TestBedElevations;
var
  DownstreamElev: Double;
  UpstreamElev: Double;
  WriteValue: Boolean;
  DownstreamValues: TSfrSegmentStorage;
  UpstreamValues: TSfrSegmentStorage;
  ParamIcalcItem: TSfrParamIcalcItem;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  PriorDownstreamValues: TSfrSegmentStorage;
  PriorUpstreamValues: TSfrSegmentStorage;
  Index: Integer;
  Segment: TSegment;
  Boundary: TSfrBoundary;
  ErrorObject: TScreenObject;
begin
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;
    PriorUpstreamValues := nil;
    PriorDownstreamValues := nil;
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      ParamIcalcItem := Boundary.ParamIcalc.GetItemByStartTime(StressPeriod.StartTime);
      UpstreamValues := Boundary.UpstreamSegmentValues.
        GetBoundaryByStartTime(StressPeriod.StartTime, Model) as TSfrSegmentStorage;
      if UpstreamValues = nil then
      begin
        ErrorObject := Boundary.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddError(Model,
          StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
        Continue;
      end;
      DownstreamValues := Boundary.DownstreamSegmentValues.
        GetBoundaryByStartTime(StressPeriod.StartTime, Model) as TSfrSegmentStorage;
      if DownstreamValues = nil then
      begin
        ErrorObject := Boundary.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddError(Model,
          StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
        Continue;
      end;
      if (UpstreamValues <> PriorUpstreamValues)
        or (DownstreamValues <> PriorDownstreamValues) then
      begin
        WriteValue := (ISFROPT in [0, 4, 5]) and (ParamIcalcItem.ICalc <> 0);
        if WriteValue and (ISFROPT in [4, 5])
          and (ParamIcalcItem.ICalc in [1, 2]) then
        begin
          WriteValue := TimeIndex = 0;
        end;
        if WriteValue then
        begin
          if (Length(UpstreamValues.SrfSegmentArray) > 0)
            and (Length(DownstreamValues.SrfSegmentArray) > 0) then
          begin
            UpstreamElev := UpstreamValues.SrfSegmentArray[0].StreambedElevation;
            DownstreamElev := DownstreamValues.SrfSegmentArray[0].StreambedElevation;
            if UpstreamElev <= DownstreamElev then
            begin
              frmErrorsAndWarnings.AddError(Model,StrOneOrMoreSFRStre,
                Format(StrObjectSTime1, [Segment.FScreenObject.Name,
                StressPeriod.StartTime, UpstreamElev, DownstreamElev]),
                Segment.FScreenObject);
            end;
          end
          else
          begin
            if (Length(UpstreamValues.SrfSegmentArray) <= 0) then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrUpstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
            if (Length(DownstreamValues.SrfSegmentArray) <= 0) then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                Format(StrTheSEndOfTheFo, [StrDownstream]),
                ErrorObject.Name, ErrorObject);
              Continue;
            end;
          end;
        end;
      end;
      PriorUpstreamValues := UpstreamValues;
      PriorDownstreamValues := DownstreamValues;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet1c;
var
  NSTRM: integer;
  Index: Integer;
  Segment: TSegment;
  NSS: integer;
  SfrPackage: TSfrPackageSelection;
  LocalModel: TCustomModel;
  NPARSEG: integer;
  sfrCONST: double;
  DLEAK: double;
  ISTCB1: integer;
  ISTCB2: integer;
  NSTRAIL: integer;
  ISUZN: integer;
  NSFRSETS: integer;
  IRTFLG: integer;
  NUMTIM: integer;
  FLWTOL: double;
  WEIGHT: double;
  ParameterNames: TStringList;
  NameIndex: Integer;
  Params: TSfrParamIcalcCollection;
  Item: TSfrParamIcalcItem;
  InstanceItem: TSfrParamInstance;
  ParamIndex: Integer;
  FlowFileName: string;
//  LgrUsed: Boolean;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
  ReachIndex: Integer;
  AReach: TSfr_Cell;
  BaseName: string;
//  Used: Boolean;
begin
  NSTRM := 0;
  NSS := 0;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    if Segment.FSubSegmentList.Count > 0 then
    begin
      for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
      begin
        SubSeg := Segment.FSubSegmentList[SubSegIndex];
        if SubSeg.Used then
        begin
          for ReachIndex := 0 to SubSeg.ReachCount - 1 do
          begin
            AReach := SubSeg.Reaches[ReachIndex];
            if AReach.ReachLength > 0 then
            begin
              Inc(NSTRM);
            end;
          end;
          Inc(NSS)
        end;
      end;
    end
    else
    begin
      NSTRM := NSTRM + Segment.ReachCount;
      Inc(NSS)
    end;
  end;

  LocalModel := Model as TCustomModel;
  NSFRPAR := LocalModel.ModflowTransientParameters.CountParam(ptSFR);

  SfrPackage := Package as TSfrPackageSelection;
  NPARSEG := 0;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
    Params := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc;
    ParameterNames := TStringList.Create;
    try
      for NameIndex := 0 to Params.Count - 1 do
      begin
        Item := Params.Items[NameIndex];
        if (Item.Param <> '') and
          (ParameterNames.IndexOf(Item.Param) < 0) then
        begin
          ParameterNames.Add(Item.Param);
        end;
      end;
      for ParamIndex := 0 to SfrPackage.ParameterInstances.Count - 1 do
      begin
        InstanceItem := SfrPackage.ParameterInstances.Items[ParamIndex];
        if ParameterNames.IndexOf(InstanceItem.ParameterName) >= 0 then
        begin
          Inc(NPARSEG, Max(1, Segment.FSubSegmentList.Count));
        end;
      end;
    finally
      ParameterNames.Free;
    end;
  end;

  sfrCONST := SfrPackage.StreamConstant;

  DLEAK := SfrPackage.Dleak;

  GetFlowUnitNumber(ISTCB1);

  // ISTCB2 < 0 is not supported by ModelMuse.
//  if SfrPackage.PrintStreams then
//  begin
//    ISTCB2 := PhastModel.UnitNumbers.UnitNumber(StrLIST);
//  end
//  else
//  begin
//    ISTCB2 := 0;
//  end;
  ISTCB2 := 0;

  BaseName := ChangeFileExt(FNameOfFile, '');
  case SfrPackage.PrintFlows of
    pfNoPrint: ISTCB2 := 0;
    pfListing: ISTCB2 := Model.UnitNumbers.UnitNumber(StrLIST);
    pfText:
      begin
        ISTCB2 := Model.UnitNumbers.UnitNumber(StrISTCB2);
        FlowFileName := ChangeFileExt(BaseName, '.sfr_out');
        if not WritingTemplate then
        begin
          WriteToNameFile('DATA', ISTCB2, FlowFileName, foOutput, Model);
        end;
      end;
    else Assert(False);
  end;

  if SfrPackage.KinematicRouting then
  begin
    IRTFLG := 1;
  end
  else
  begin
    IRTFLG := 0;
  end;

  if (ISFROPT > 0) and not NewFormat then
  begin
    NSTRM := -NSTRM;
  end;

  NSTRAIL := SfrPackage.Nstrail;

  ISUZN := SfrPackage.Isuzn;

  NSFRSETS := SfrPackage.Nsfrsets;

  NUMTIM := SfrPackage.TimeStepsForKinematicRouting;
  WEIGHT := SfrPackage.KinematicRoutingWeight;
  FLWTOL := SfrPackage.KinematicRoutingTolerance;

  WriteInteger(NSTRM);
  WriteInteger(NSS);
  WriteInteger(NSFRPAR);
  WriteInteger(NPARSEG);
  WriteFloat(sfrCONST);
  WriteFloat(DLEAK);
  WriteInteger(ISTCB1);
  WriteInteger(ISTCB2);
  if (NSTRM < 0)  or (NewFormat and (ISFROPT > 0)) then
  begin
    WriteInteger(ISFROPT);
  end;
  if ISFROPT > 1 then
  begin
    WriteInteger(NSTRAIL);
    WriteInteger(ISUZN);
    WriteInteger(NSFRSETS);
  end;

  if (ISFROPT > 0) or (NSTRM < 0) then
  begin
//    if not NewFormat then
//    begin
      WriteInteger(IRTFLG);
//    end;
    if IRTFLG > 0 then
    begin
      WriteInteger(NUMTIM);
      WriteFloat(WEIGHT);
      WriteFloat(FLWTOL);
    end;
  end;

  WriteString(' IFACE');

  if FLgrUsed then
  begin
    WriteString(' AUXILIARY LGRGRID AUXILIARY LGRSEG');
  end;

  WriteString(' # Data Set 1: NSTRM NSS NSFRPAR NPARSEG CONST DLEAK ISTCB1  ISTCB2');
  if (NSTRM < 0)  or (NewFormat and (ISFROPT > 0)) then
  begin
    WriteString(' ISFROPT');
  end;
  if ISFROPT > 0 then
  begin
    WriteString(' NSTRAIL ISUZN NSFRSETS');
  end;
  if (ISFROPT > 0) or (NSTRM < 0) then
  begin
//    if not NewFormat then
//    begin
      WriteString(' IRTFLG');
//    end;
    if IRTFLG > 0 then
    begin
      WriteString(' NUMTIM WEIGHT FLWTOL');
    end;
  end;
  if FLgrUsed then
  begin
    WriteString(' Variables for routing between grids');
  end;
  NewLine;
end;

procedure TModflowSFR_Writer.WriteDataSet1a;
var
  SfrPackage: TSfrPackageSelection;
  NWT_Format: TNwtFormat;
  Factor: Double;
begin
  SfrPackage := Model.ModflowPackages.SfrPackage;
  NWT_Format := Model.NWT_Format;
  if NWT_Format = nf1_1 then
  begin
    WriteString('OPTIONS');
    NewLine;

    if (ISFROPT > 0) then
    begin
      WriteString('REACHINPUT ');
      NewLine;
    end;
    if SfrPackage.KinematicRouting then
    begin
      WriteString('TRANSROUTE ');
      NewLine;
    end;

    if NUMTAB > 0 then
    begin
      WriteString('TABFILES');
      WriteInteger(NUMTAB);
      WriteInteger(MAXVAL);
      NewLine;
    end;

    if SfrPackage.LossFactorOption then
    begin
      Factor := SfrPackage.LossFactor;
      WriteString('LOSSFACTOR');
      WriteFloat(Factor);
      NewLine;
    end;

    WriteString('END');
    NewLine;
  end
  else
  begin
    if NewFormat
      and ((ISFROPT > 0) or SfrPackage.KinematicRouting) then
    begin
      if (ISFROPT > 0) then
      begin
        WriteString('REACHINPUT ');
      end;
      if SfrPackage.KinematicRouting then
      begin
        WriteString('TRANSROUTE ');
      end;
      NewLine;

      begin
        if NUMTAB > 0 then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrInvalidSFROption,
            Format(StrTheSOptionInNot, [StrTABFILES]));
        end;

        if SfrPackage.LossFactorOption then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrInvalidSFROption,
            Format(StrTheSOptionInNot, [StrLOSSFACTOR]));
        end;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet1b;
begin
  if NewFormat and (Model.NWT_Format = nf1_0) then
  begin
    if NUMTAB > 0 then
    begin
      WriteString('TABFILES');
      WriteInteger(NUMTAB);
      WriteInteger(MAXVAL);
      NewLine;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet2;
const
  HighRatio = 1e6;
var
  Index: integer;
  Segment: TSegment;
  Reach: TSfr_Cell;
  ReachIndex: integer;
  LocalLayer: integer;
  ObjectName: string;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
  IFACE: TIface;
  PriorReach: TSfr_Cell;
  LAYTYP: TOneDIntegerArray;
  LayIndex: Integer;
  procedure WriteReach(Reach: TSfr_Cell; SegmentNumber, ReachIndex: integer);
  var
    AqKx: double;
    Ratio: double;
    ScreenObject: TScreenObject;
  begin
    CheckCell(Reach, 'SFR');
    LocalLayer := Model.
      DataSetLayerToModflowLayer(Reach.Layer);
    WriteInteger(LocalLayer);
    WriteInteger(Reach.Row+1);
    WriteInteger(Reach.Column+1);
    WriteInteger(SegmentNumber);
    WriteInteger(ReachIndex+1);
    WriteValueOrFormula(Reach, ReachLengthPosition);
//    WriteFloat(Reach.ReachLength);
    if ISFROPT in [1,2,3] then
    begin
      WriteValueOrFormula(Reach, StreambedElevationPosition);
      WriteValueOrFormula(Reach, StreamSlopePosition);
//      WriteFloat(Reach.StreambedElevation);
//      WriteFloat(Reach.StreamSlope);
      if Reach.StreamSlope <= 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrOneOrMoreSFRStre,
          Format(ZeroSlope,
          [Segment.FScreenObject.Name,
          Reach.Layer+1, Reach.Row + 1, Reach.Column + 1]),
          Segment.FScreenObject);
      end;
      WriteValueOrFormula(Reach, StreamBedThicknessPosition);
      WriteValueOrFormula(Reach, HydraulicConductivityPosition);
//      WriteFloat(Reach.StreamBedThickness);
//      WriteFloat(Reach.HydraulicConductivity);

      AqKx := AquiferKx(Reach.Layer, Reach.Row, Reach.Column);
      if AqKx > 0 then
      begin
        Ratio := Reach.HydraulicConductivity/AqKx;
        if Ratio > HighRatio then
        begin
          ScreenObject := Reach.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddWarning(Model,StrHighSFRHydraulicC,
            Format(StrLayerRowColObject, [
            Reach.Layer+1, Reach.Row+1, Reach.Column+1, ScreenObject.Name]),
            ScreenObject);
        end;
      end
      else
      begin
        ScreenObject := Reach.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model,StrZeroSFRHydraulicC,
          Format(StrLayerRowColObject, [
          Reach.Layer+1, Reach.Row+1, Reach.Column+1, ScreenObject.Name]),
          ScreenObject);
      end;
    end;
    if ISFROPT in [2,3] then
    begin
      WriteValueOrFormula(Reach, SaturatedWaterContentPosition);
      WriteValueOrFormula(Reach, InitialWaterContentPosition);
      WriteValueOrFormula(Reach, BrooksCoreyExponentPosition);
//      WriteFloat(Reach.SaturatedWaterContent);
//      WriteFloat(Reach.InitialWaterContent);
//      WriteFloat(Reach.BrooksCoreyExponent);
    end;
    if ISFROPT = 3 then
    begin
      WriteValueOrFormula(Reach, VerticalKPosition);
//      WriteFloat(Reach.VerticalK);
    end;
    WriteIface(IFACE);
    WriteString(' # Data Set 2: KRCH IRCH JRCH ISEG IREACH RCHLEN');
    if ISFROPT in [1,2,3] then
    begin
      WriteString(' STRTOP SLOPE STRTHICK STRHC1');
    end;
    if ISFROPT in [2,3] then
    begin
      WriteString(' THTS THTI EPS');
    end;
    if ISFROPT = 3 then
    begin
      WriteString(' UHC');
    end;
    WriteString(' IFACE');
    if ReachIndex = 0 then
    begin
      WriteString(' Defined by object: ' + ObjectName);
    end;

    NewLine;

    if Reach.ReachLength <= 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, WarningRootRL_Less_0,
        Format(StrObject0sLayer,
        [ObjectName, Reach.Layer+1, Reach.Row+1, Reach.Column+1]),
        Segment.FScreenObject);
    end;
  end;

  procedure WarnIfVerticallyAlignedReaches;
  begin
    if (PriorReach <> nil) and (PriorReach.Row = Reach.Row)
      and (PriorReach.Column = Reach.Column)
      and (PriorReach.Layer <> Reach.Layer) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrOneOrMoreReaches,
        Format(StrAtRowcolumn, [Reach.Row+1, Reach.Column+1, ObjectName]),
        Segment.FScreenObject);
    end;
  end;
  procedure WarnIfNoSpecificYieldForUnsatFlow;
  begin
    if ISFROPT in [2..5] then
    begin
      if LAYTYP[Model.DataSetLayerToModflowLayer(Reach.Layer)-1] = 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrWhenUnsaturatedFlo,
          Format(StrObject0sLayer, [Segment.FScreenObject.Name, Reach.Layer+1,
          Reach.Row+1, Reach.Column+1]), Segment.FScreenObject);
      end;
    end;
  end;
begin
  LAYTYP := Model.Laytyp;
  for LayIndex := 0 to Length(LAYTYP) - 1 do
  begin
    LAYTYP[LayIndex] := LAYTYP[LayIndex] mod 10;
  end;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    ObjectName := Segment.FScreenObject.Name;
    IFACE := Segment.FScreenObject.IFACE;
    PriorReach := nil;
    if Segment.FSubSegmentList.Count = 0 then
    begin
      for ReachIndex := 0 to Segment.FReaches.Count - 1 do
      begin
        Reach := Segment.FReaches[ReachIndex] as TSfr_Cell;
        WriteReach(Reach, Segment.NewSegmentNumber, ReachIndex);
        WarnIfVerticallyAlignedReaches;
        WarnIfNoSpecificYieldForUnsatFlow;
        PriorReach := Reach;
      end;
    end
    else
    begin
      for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
      begin
        SubSeg := Segment.FSubSegmentList[SubSegIndex];
        for ReachIndex := 0 to SubSeg.ReachCount - 1 do
        begin
          Reach := SubSeg.Reaches[ReachIndex];
          WriteReach(Reach, SubSeg.FSegmentNumber, ReachIndex);
          WarnIfVerticallyAlignedReaches;
          WarnIfNoSpecificYieldForUnsatFlow;
          PriorReach := Reach;
        end;
      end;
    end;
  end;
end;

function Interpolate(Value1, Value2, Fraction: Double): double;
begin
  result := (Value2-Value1)*Fraction + Value1;
end;

procedure TModflowSFR_Writer.CheckParameterSegments;
var
  ParamIndex: integer;
  SfrPackage: TSfrPackageSelection;
//  LocalModel: TCustomModel;
  ParamItem: TModflowTransientListParameter;
  Instances: TList;
  InstanceItem: TSfrParamInstance;
  ScreenObject: TScreenObject;
  Segments: TList;
  ScreenObjectParamIndex: Integer;
  ParamScreenObjectItem: TSfrParamIcalcItem;
  Index: Integer;
  InstanceIndex: Integer;
  SfrBoundary: TSfrBoundary;
  Segment: TSegment;
  SubSegIndex: Integer;
  ActiveDataSet: TDataArray;
  ReachIndex: Integer;
  AReach: TValueCell;
  PriorReach: TValueCell;
  DeltaCell: Integer;
  SfrReach: TSfr_Cell;
  Grid: TModflowGrid;
begin
  Grid := Model.ModflowGrid;
  SfrPackage := Package as TSfrPackageSelection;
  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ParamItem := Model.ModflowTransientParameters.Items[ParamIndex];
    if ParamItem.ParameterType = ptSFR then
    begin
      Instances := TList.Create;
      Segments := TList.Create;
      try
        // Get the segments and instances for the current parameter.
        for Index := 0 to SfrPackage.ParameterInstances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := SfrPackage.ParameterInstances.Items[Index];
          if InstanceItem.ParameterName = ParamItem.ParameterName then
          begin
            Instances.Add(InstanceItem);
          end;
        end;
        for Index := 0 to FSegments.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          Segment := FSegments[Index];
          ScreenObject := Segment.FScreenObject;
          Assert(ScreenObject.ModflowSfrBoundary <> nil);
          for ScreenObjectParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ParamScreenObjectItem := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ScreenObjectParamIndex];
            if ParamScreenObjectItem.Param = ParamItem.ParameterName then
            begin
              Segments.Add(Segment);
              break;
            end;
          end;
          PriorReach := nil;
          for ReachIndex := 0 to Segment.ReachCount - 1 do
          begin
            AReach := Segment.Reaches[ReachIndex] as TSfr_Cell;
            if not ActiveDataSet.BooleanData[
              AReach.Layer, AReach.Row, AReach.Column] then
            begin
//              ScreenObject := Segment.FScreenObject;
              frmErrorsAndWarnings.AddWarning(Model, StrInactiveReach,
                Format(StrObject0sLayer,
                [ScreenObject.Name, AReach.Layer+1, AReach.Row+1, AReach.Column+1]),
                ScreenObject);
            end;

            SfrReach := AReach as TSfr_Cell;
            CheckStreamBottomElevation(ScreenObject, Grid, SfrReach);

            if ReachIndex > 0 then
            begin
              DeltaCell := Max(Abs(AReach.Row - PriorReach.Row),
                Abs(AReach.Column - PriorReach.Column));
              if DeltaCell > 1 then
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrReachSeparationWarning,
                  Format(StrSegment0dReach,
                  [Segment.OriginalSegmentNumber, ReachIndex+1,
                  ScreenObjectParamIndex+ 1, DeltaCell]), Segment.FScreenObject);
              end;
            end;
            PriorReach := AReach;
          end;
        end;


        // Data set 4a
        for InstanceIndex := 0 to Instances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := Instances[InstanceIndex];

          for Index := 0 to Segments.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            Segment := Segments[Index];
            ScreenObject := Segment.FScreenObject;
            Assert(ScreenObject.ModflowSfrBoundary <> nil);
            SfrBoundary := ScreenObject.ModflowSfrBoundary;
            for ScreenObjectParamIndex := 0 to SfrBoundary.ParamIcalc.Count - 1 do
            begin
              Application.ProcessMessages;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
              ParamScreenObjectItem := SfrBoundary.
                ParamIcalc.Items[ScreenObjectParamIndex];
              if (ParamScreenObjectItem.Param = ParamItem.ParameterName)
                and (ParamScreenObjectItem.ParamInstance
                = InstanceItem.ParameterInstance) then
              begin
                if Segment.FSubSegmentList.Count = 0 then
                begin
                  SubSegIndex := -1;
                  CheckOutflowSegments(InstanceItem.StartTime, Segment,
                    ParamScreenObjectItem, SfrBoundary, SubSegIndex);
                end
                else
                begin
                  for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
                  begin
                    CheckOutflowSegments(InstanceItem.StartTime, Segment,
                      ParamScreenObjectItem, SfrBoundary, SubSegIndex)
                  end;
                end;

              end;
            end;
          end;
        end;

      finally
        Instances.Free;
        Segments.Free;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSets3and4;
var
  ParamIndex: integer;
  SfrPackage: TSfrPackageSelection;
  LocalModel: TCustomModel;
  ParamItem: TModflowTransientListParameter;
  Instances: TList;
  InstanceItem: TSfrParamInstance;
  ScreenObject: TScreenObject;
  Segments: TList;
  ScreenObjectParamIndex: Integer;
  ParamScreenObjectItem: TSfrParamIcalcItem;
  Index: Integer;
  InstanceIndex: Integer;
  SfrBoundary: TSfrBoundary;
  Segment: TSegment;
  SegIndex: Integer;
  NLST: Integer;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
begin
  SfrPackage := Package as TSfrPackageSelection;
  LocalModel := Model as TCustomModel;
  for ParamIndex := 0 to LocalModel.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ParamItem := LocalModel.ModflowTransientParameters.Items[ParamIndex];
    if ParamItem.ParameterType = ptSFR then
    begin
      Instances := TList.Create;
      Segments := TList.Create;
      try
        // Get the segments and instances for the current parameter.
        for Index := 0 to SfrPackage.ParameterInstances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := SfrPackage.ParameterInstances.Items[Index];
          if InstanceItem.ParameterName = ParamItem.ParameterName then
          begin
            Instances.Add(InstanceItem);
          end;
        end;
        for Index := 0 to FSegments.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          Segment := FSegments[Index];
          ScreenObject := Segment.FScreenObject;
          Assert(ScreenObject.ModflowSfrBoundary <> nil);
          for ScreenObjectParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ParamScreenObjectItem := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ScreenObjectParamIndex];
            if ParamScreenObjectItem.Param = ParamItem.ParameterName then
            begin
              Segments.Add(Segment);
              break;
            end;
          end;
        end;

        // Data set 3
        frmProgressMM.AddMessage(Format(StrWritingParamete,
          [ParamItem.ParameterName]));
        // PARNAM, PARTYP
        WriteString(ParamItem.ParameterName + ' SFR');
        // Parval
        WriteFloat(ParamItem.Value);
        // NLST
        NLST := 0;
        for SegIndex := 0 to Segments.Count - 1 do
        begin
          Segment := Segments[SegIndex];
          if Segment.FSubSegmentList.Count = 0 then
          begin
            Inc(NLST);
          end
          else
          begin
            for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
            begin
              SubSeg := Segment.FSubSegmentList[SubSegIndex];
              if SubSeg.ReachCount > 0 then
              begin
                Inc(NLST);
              end;
            end;
          end;
        end;
        WriteInteger(NLST);
        // INSTANCES NUMINST
        if Instances.Count > 1 then
        begin
          WriteString(' INSTANCES');
          WriteInteger(Instances.Count);
        end;
        WriteString(' #  Data Set 3: PARNAM PARTYP Parval NLST');
        if Instances.Count > 1 then
        begin
          WriteString(' INSTANCES NUMINST');
        end;
        NewLine;

        Model.WritePValAndTemplate(ParamItem.ParameterName,
          ParamItem.Value, ParamItem);

        // Data set 4a
        for InstanceIndex := 0 to Instances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := Instances[InstanceIndex];
          if Instances.Count > 1 then
          begin
            WriteString(InstanceItem.ParameterInstance);
            WriteString(' # Data Set 4a: INSTNAM');
            NewLine;
          end;

          for Index := 0 to Segments.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            Segment := Segments[Index];
            ScreenObject := Segment.FScreenObject;
            Assert(ScreenObject.ModflowSfrBoundary <> nil);
            SfrBoundary := ScreenObject.ModflowSfrBoundary;
            for ScreenObjectParamIndex := 0 to SfrBoundary.ParamIcalc.Count - 1 do
            begin
              Application.ProcessMessages;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
              ParamScreenObjectItem := SfrBoundary.
                ParamIcalc.Items[ScreenObjectParamIndex];
              if (ParamScreenObjectItem.Param = ParamItem.ParameterName)
                and (ParamScreenObjectItem.ParamInstance
                = InstanceItem.ParameterInstance) then
              begin
                if Segment.FSubSegmentList.Count = 0 then
                begin
                  SubSegIndex := -1;
                  WriteSegment(Segment, InstanceItem.StartTime,
                    SubSegIndex, ParamScreenObjectItem, SfrBoundary,
                    ScreenObjectParamIndex, True, ParamItem.Value);
                end
                else
                begin
                  for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
                  begin
                    WriteSegment(Segment, InstanceItem.StartTime,
                      SubSegIndex, ParamScreenObjectItem, SfrBoundary,
                      ScreenObjectParamIndex, True, ParamItem.Value)
                  end;
                end;

              end;
            end;
          end;
        end;

      finally
        Instances.Free;
        Segments.Free;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.LgrAdjustSegmentValues(
  Segment: TSegment; StartTime: double; SubSegIndex: integer;
  StressPeriod: integer);
var
  SfrBoundary: TSfrBoundary;
  UpstreamValues: TSfrSegmentStorage;
  DownstreamValues: TSfrSegmentStorage;
  UnsatUpstreamValues: TSfrUnsatSegmentStorage;
  UnsatDownstreamValues: TSfrUnsatSegmentStorage;
  UpValues: TSfrSegmentRecord;
  DownValues: TSfrSegmentRecord;
  ASubSeg: TSubSegment;
  ParentSeg: TSegment;
  TotalParentSegLength: double;
  ParentSubSegIndex: Integer;
  ParentSubSeg: TSubSegment;
  CumLength: double;
  UpUnsatValues: TSfrUnsatSegmentRecord;
  DownUnsatValues: TSfrUnsatSegmentRecord;
  Fraction: Extended;
  SubSeg: TSubSegment;
  CumSegLength: double;
  LocalIndex: Integer;
  LocalSubSeg: TSubSegment;
  ErrorObject: TScreenObject;
//  ParentUpstreamValues: TSfrSegmentStorage;
//  ParentDownstreamValues: TSfrSegmentStorage;
//  ParentUnsatUpstreamValues: TSfrUnsatSegmentStorage;
//  ParentUnsatDownstreamValues: TSfrUnsatSegmentStorage;
  procedure InterpolateSegmentValues(var SegValues: TSfrSegmentRecord);
  begin
    SegValues.StreambedElevation :=
      Interpolate(UpValues.StreambedElevation,
      DownValues.StreambedElevation, Fraction);
    SegValues.StreamBedThickness :=
      Interpolate(UpValues.StreamBedThickness,
      DownValues.StreamBedThickness, Fraction);
    SegValues.HydraulicConductivity :=
      Interpolate(UpValues.HydraulicConductivity,
      DownValues.HydraulicConductivity, Fraction);
    SegValues.StreamWidth :=
      Interpolate(UpValues.StreamWidth,
      DownValues.StreamWidth, Fraction);
    SegValues.StreamDepth :=
      Interpolate(UpValues.StreamDepth,
      DownValues.StreamDepth, Fraction);
  end;
  procedure InterpolateUnsatSegmentValues(var SegValues: TSfrUnsatSegmentRecord);
  begin
    SegValues.SaturatedWaterContent :=
      Interpolate(UpUnsatValues.SaturatedWaterContent,
      DownUnsatValues.SaturatedWaterContent, Fraction);
    SegValues.InitialWaterContent :=
      Interpolate(UpUnsatValues.InitialWaterContent,
      DownUnsatValues.InitialWaterContent, Fraction);
    SegValues.BrooksCoreyExponent :=
      Interpolate(UpUnsatValues.BrooksCoreyExponent,
      DownUnsatValues.BrooksCoreyExponent, Fraction);
    SegValues.VerticalSaturatedK :=
      Interpolate(UpUnsatValues.VerticalSaturatedK,
      DownUnsatValues.VerticalSaturatedK, Fraction);
  end;
begin
  if FLgrUsed then
  begin
    SfrBoundary := Segment.FScreenObject.ModflowSfrBoundary;
//    ParentUpstreamValues := SfrBoundary.UpstreamSegmentValues.
//      GetBoundaryByStartTime(StartTime, Model.ParentModel)
//      as TSfrSegmentStorage;
//    ParentDownstreamValues := SfrBoundary.DownstreamSegmentValues.
//      GetBoundaryByStartTime(StartTime, Model.ParentModel)
//      as TSfrSegmentStorage;

    UpstreamValues := SfrBoundary.UpstreamSegmentValues.
      GetBoundaryByStartTime(StartTime, Model)
      as TSfrSegmentStorage;
    DownstreamValues := SfrBoundary.DownstreamSegmentValues.
      GetBoundaryByStartTime(StartTime, Model)
      as TSfrSegmentStorage;
      //      UpstreamValues.SrfSegmentArray[0]

    UnsatUpstreamValues := nil;
    UnsatDownstreamValues := nil;
//    ParentUnsatUpstreamValues := nil;
//    ParentUnsatDownstreamValues := nil;
    if (ISFROPT in [4,5]) {and (StressPeriodIndex = 0)} then
    begin
      if SfrBoundary.UpstreamUnsatSegmentValues.
        BoundaryCount[Model] > 0 then
      begin
        UnsatUpstreamValues := SfrBoundary.UpstreamUnsatSegmentValues.
          Boundaries[0, Model] as TSfrUnsatSegmentStorage;
      end;
//      if SfrBoundary.UpstreamUnsatSegmentValues.
//        BoundaryCount[Model.ParentModel] > 0 then
//      begin
//        ParentUnsatUpstreamValues := SfrBoundary.UpstreamUnsatSegmentValues.
//          Boundaries[0, Model.ParentModel] as TSfrUnsatSegmentStorage;
//      end;
      if SfrBoundary.DownstreamUnsatSegmentValues.
        BoundaryCount[Model] > 0 then
      begin
        UnsatDownstreamValues := SfrBoundary.DownstreamUnsatSegmentValues.
          Boundaries[0, Model] as TSfrUnsatSegmentStorage;
      end;
//      if SfrBoundary.DownstreamUnsatSegmentValues.
//        BoundaryCount[Model.ParentModel] > 0 then
//      begin
//        ParentUnsatDownstreamValues := SfrBoundary.DownstreamUnsatSegmentValues.
//          Boundaries[0, Model.ParentModel] as TSfrUnsatSegmentStorage;
//      end;
    end;

    if (UpstreamValues <> nil) and (DownstreamValues <> nil) then
    begin
      if (Length(UpstreamValues.SrfSegmentArray) > 0)
        and (Length(DownstreamValues.SrfSegmentArray) > 0) then
      begin
        UpValues := UpstreamValues.SrfSegmentArray[0];
        DownValues := DownstreamValues.SrfSegmentArray[0];
        if (UnsatUpstreamValues <> nil) and (UnsatDownstreamValues <> nil) then
        begin
          if (Length(UnsatUpstreamValues.SrfUnsatSegmentArray) > 0)
            and (Length(UnsatDownstreamValues.SrfUnsatSegmentArray) > 0) then
          begin
            UpUnsatValues := UnsatUpstreamValues.SrfUnsatSegmentArray[0];
            DownUnsatValues := UnsatDownstreamValues.SrfUnsatSegmentArray[StressPeriod];
          end;
        end;


        if FIsChildModel then
        begin
          ASubSeg := Segment.FSubSegmentList[0];
          if ASubSeg.FAssociatedLgrSubSeg <> nil then
          begin
            ParentSeg := ASubSeg.FAssociatedLgrSubSeg.FSegment
          end
          else
          begin
            ParentSeg := nil;
          end;
        end
        else
        begin
          ASubSeg := nil;
          ParentSeg := Segment;
        end;
        TotalParentSegLength := 0;
        if (ParentSeg <> nil)  then
        begin
          for ParentSubSegIndex := 0 to ParentSeg.FSubSegmentList.Count - 1 do
          begin
            ParentSubSeg := ParentSeg.FSubSegmentList[ParentSubSegIndex];
            TotalParentSegLength := TotalParentSegLength + ParentSubSeg.FTotalLength;
          end;
          CumLength := 0;
          for ParentSubSegIndex := 0 to ParentSeg.FSubSegmentList.Count - 1 do
          begin
            ParentSubSeg := ParentSeg.FSubSegmentList[ParentSubSegIndex];
            if not FIsChildModel then
            begin
              ParentSubSeg.FUpstreamValues[StressPeriod] := UpstreamValues.SrfSegmentArray[0];
              ParentSubSeg.FDownstreamValues[StressPeriod] := DownstreamValues.SrfSegmentArray[0];
            end;
            ParentSubSeg.FParentUpstreamValues[StressPeriod] := UpstreamValues.SrfSegmentArray[0];
            ParentSubSeg.FParentDownstreamValues[StressPeriod] := DownstreamValues.SrfSegmentArray[0];

            if (UnsatUpstreamValues <> nil) and (UnsatDownstreamValues <> nil) then
            begin
              if not FIsChildModel then
              begin
                ParentSubSeg.FUpstreamUnsatValues[StressPeriod] := UnsatUpstreamValues.SrfUnsatSegmentArray[0];
                ParentSubSeg.FDownstreamUnsatValues[StressPeriod] := UnsatDownstreamValues.SrfUnsatSegmentArray[0];
              end;
              ParentSubSeg.FParentUpstreamUnsatValues[StressPeriod] := UnsatUpstreamValues.SrfUnsatSegmentArray[0];
              ParentSubSeg.FParentDownstreamUnsatValues[StressPeriod] := UnsatDownstreamValues.SrfUnsatSegmentArray[0];
            end;

//            if (ParentUnsatUpstreamValues <> nil) and (ParentUnsatDownstreamValues <> nil) then
//            begin
//              ParentSubSeg.FUpstreamUnsatValues[StressPeriod] := ParentUnsatUpstreamValues.SrfUnsatSegmentArray[0];
//              ParentSubSeg.FDownstreamUnsatValues[StressPeriod] := ParentUnsatDownstreamValues.SrfUnsatSegmentArray[0];
//              UpUnsatValues := ParentUnsatUpstreamValues.SrfUnsatSegmentArray[0];
//              DownUnsatValues := ParentUnsatDownstreamValues.SrfUnsatSegmentArray[0];
//            end;
            if TotalParentSegLength > 0 then
            begin
              if ParentSubSegIndex > 0 then
              begin
                Fraction := CumLength/TotalParentSegLength;
                if FIsChildModel then
                begin
                  InterpolateSegmentValues(ParentSubSeg.FParentUpstreamValues[StressPeriod]);
                end
                else
                begin
                  InterpolateSegmentValues(ParentSubSeg.FUpstreamValues[StressPeriod]);
                end;
                if (UnsatUpstreamValues <> nil) and (UnsatDownstreamValues <> nil) then
                begin
                  if FIsChildModel then
                  begin
                    InterpolateUnsatSegmentValues(ParentSubSeg.FParentUpstreamUnsatValues[StressPeriod]);
                  end
                  else
                  begin
                    InterpolateUnsatSegmentValues(ParentSubSeg.FUpstreamUnsatValues[StressPeriod]);
                  end;
                end;
              end;
              CumLength := CumLength + ParentSubSeg.FTotalLength;
              if ParentSubSegIndex < ParentSeg.FSubSegmentList.Count - 1 then
              begin
                Fraction := CumLength/TotalParentSegLength;
                if FIsChildModel then
                begin
                  InterpolateSegmentValues(ParentSubSeg.FParentDownstreamValues[StressPeriod]);
                end
                else
                begin
                  InterpolateSegmentValues(ParentSubSeg.FDownstreamValues[StressPeriod]);
                end;
                if (UnsatUpstreamValues <> nil) and (UnsatDownstreamValues <> nil) then
                begin
                  if FIsChildModel then
                  begin
                    InterpolateUnsatSegmentValues(ParentSubSeg.FParentDownstreamUnsatValues[StressPeriod]);
                  end
                  else
                  begin
                    InterpolateUnsatSegmentValues(ParentSubSeg.FDownstreamUnsatValues[StressPeriod]);
                  end;
                end;
              end;
            end;
          end;
        end;
        if FIsChildModel then
        begin

          SubSeg := Segment.SubSegmentList[SubSegIndex];
          ParentSubSeg := ASubSeg.FAssociatedLgrSubSeg;
          if ParentSubSeg <> nil then
          begin
            UpValues := ParentSubSeg.FParentUpstreamValues[StressPeriod];
            DownValues := ParentSubSeg.FParentDownstreamValues[StressPeriod];
            if ISFROPT in [4,5] then
            begin
              UpUnsatValues := ParentSubSeg.FParentUpstreamUnsatValues[StressPeriod];
              DownUnsatValues := ParentSubSeg.FParentDownstreamUnsatValues[StressPeriod];
            end;

            CumSegLength := 0;
            for LocalIndex := 0 to Segment.SubSegmentList.Count - 1 do
            begin
              LocalSubSeg := Segment.SubSegmentList[LocalIndex];

              if LocalSubSeg.FAssociatedLgrSubSeg = ASubSeg.FAssociatedLgrSubSeg then
              begin
                if LocalSubSeg = SubSeg then
                begin
                  if ParentSubSeg.FTotalLength = 0 then
                  begin
                    Fraction := 0
                  end
                  else
                  begin
                    Fraction := CumSegLength/ParentSubSeg.FTotalLength;
                  end;
                  InterpolateSegmentValues(LocalSubSeg.FUpstreamValues[StressPeriod]);
                  if ISFROPT in [4,5] then
                  begin
                    InterpolateUnsatSegmentValues(LocalSubSeg.FUpstreamUnsatValues[StressPeriod])
                  end;
                end;

                CumSegLength := CumSegLength + LocalSubSeg.FTotalLength;
                if LocalSubSeg = SubSeg then
                begin
                  if ParentSubSeg.FTotalLength = 0 then
                  begin
                    Fraction := 1
                  end
                  else
                  begin
                    Fraction := CumSegLength/ParentSubSeg.FTotalLength;
                  end;
//                  Fraction := CumSegLength/ParentSubSeg.FTotalLength;
                  InterpolateSegmentValues(LocalSubSeg.FDownstreamValues[StressPeriod]);
                  if ISFROPT in [4,5] then
                  begin
                    InterpolateUnsatSegmentValues(LocalSubSeg.FDownstreamUnsatValues[StressPeriod])
                  end;
                  break;
                end;
              end;
            end;
          end;
        end;
      end
      else
      begin
        if (Length(UpstreamValues.SrfSegmentArray) <=  0) then
        begin
          ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model,
            Format(StrTheSEndOfTheFo, [StrUpstream]),
            ErrorObject.Name, ErrorObject);
        end;
        if (Length(DownstreamValues.SrfSegmentArray) <= 0) then
        begin
          ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model,
            Format(StrTheSEndOfTheFo, [StrDownstream]),
            ErrorObject.Name, ErrorObject);
        end;
      end;
    end;
  end
end;

function TModflowSFR_Writer.NewFormat: boolean;
var
  SfrPackage: TSfrPackageSelection;
begin
  SfrPackage := Package as TSfrPackageSelection;
  Result := (Model.ModelSelection in [msModflow, msModflowNWT, msModflowLGR2,
    msModflowFmp {, msModflowCfp}])
    or SfrPackage.UseGsflowFormat;
end;

procedure TModflowSFR_Writer.WriteSegment(Segment: TSegment; StartTime: double;
  SubSegIndex: integer;
  Item: TSfrParamIcalcItem; Boundary: TSfrBoundary; TimeIndex: integer;
  IsParameter: boolean; ParameterValue: double);
begin
//  LgrAdjustSegmentValues(Segment, StartTime, SubSegIndex, TimeIndex);

  WriteDataSet4b6a(StartTime, Segment, Item,
    Boundary, IsParameter, SubSegIndex);

  // Data set 6b
  WriteDataSet4c6b(IsParameter, Boundary, Item, StartTime,
    TimeIndex, Segment, SubSegIndex, ParameterValue);

  // data set 6c
  WriteDataSet4d6c(IsParameter, Boundary, Item,
    StartTime, TimeIndex, Segment, SubSegIndex, ParameterValue);

  // data set 6d
  WriteDataSet4e6d(IsParameter, Boundary, Item, TimeIndex+1, Segment,
    SubSegIndex);

  // data set 6e
  WriteDataSet4f6e(IsParameter, Boundary, Item, StartTime, Segment,
    SubSegIndex);

  // data set 6f
//  WriteDataSet4g6f(Segment, SubSegIndex, TimeIndex, StartUnitNumber);

  // data set 6g
end;

procedure TModflowSFR_Writer.CheckNonParamSegments;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Segment: TSegment;
  SegementIndex: Integer;
  Item: TSfrParamIcalcItem;
  UsedSegments: TList;
  ParametersUsed: TStringList;
  Boundary: TSfrBoundary;
  PIndex: Integer;
  Instance: TSfrParamInstance;
  Parameters: TStringList;
  Location: integer;
  InstanceList: TList;
  SubSegIndex: Integer;
  ReachIndex: Integer;
  AReach: TValueCell;
  ActiveDataSet: TDataArray;
  ScreenObject: TScreenObject;
  PriorReach: TValueCell;
  DeltaCell: Integer;
  SfrReach: TSfr_Cell;
  Grid: TModflowGrid;
begin
  UsedSegments := TList.Create;
  ParametersUsed := TStringList.Create;
  Parameters := TStringList.Create;
  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  try
    for PIndex := 0 to Model.ModflowPackages.
      SfrPackage.ParameterInstances.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Instance := Model.ModflowPackages.
        SfrPackage.ParameterInstances.Items[PIndex];
      Location := Parameters.IndexOf(Instance.ParameterName);
      if Location < 0 then
      begin
        InstanceList := TList.Create;
        Parameters.AddObject(Instance.ParameterName, InstanceList)
      end
      else
      begin
        InstanceList := Parameters.Objects[Location] as TList;
      end;
      InstanceList.Add(Instance);
    end;


    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      frmProgressMM.AddMessage(Format(StrCheckingStress,
        [TimeIndex + 1]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      // data set 5;
      UsedSegments.Clear;
      ParametersUsed.Clear;
      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      for SegementIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if (Item = nil) or (Item.Param = '') then
        begin
          UsedSegments.Add(Segment);
        end
        else
        begin
          if ParametersUsed.IndexOf(Item.Param) < 0 then
          begin
            ParametersUsed.Add(Item.Param);
          end;
        end;
      end;


      // Data Set 6
      for SegementIndex := 0 to UsedSegments.Count - 1 do
      begin
        // Data set 6a
        Segment := UsedSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if Item <> nil then
        begin

          Boundary := Segment.FScreenObject.ModflowSfrBoundary;
          if Segment.FSubSegmentList.Count = 0 then
          begin
            SubSegIndex := -1;
            CheckOutflowSegments(StressPeriod.StartTime, Segment,
              Item, Boundary, SubSegIndex);
          end
          else
          begin
            for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
            begin
              CheckOutflowSegments(StressPeriod.StartTime, Segment,
                Item, Boundary, SubSegIndex);
            end;
          end;
        end;
        PriorReach := nil;
        for ReachIndex := 0 to Segment.ReachCount - 1 do
        begin
          AReach := Segment.Reaches[ReachIndex];
          if not ActiveDataSet.BooleanData[
            AReach.Layer, AReach.Row, AReach.Column] then
          begin
            ScreenObject := Segment.FScreenObject;
            frmErrorsAndWarnings.AddWarning(Model, StrInactiveReach,
              Format(StrObject0sLayer,
              [ScreenObject.Name, AReach.Layer+1, AReach.Row+1, AReach.Column+1]),
              ScreenObject);
          end;

          if ReachIndex > 0 then
          begin
            DeltaCell := Max(Abs(AReach.Row - PriorReach.Row),
              Abs(AReach.Column - PriorReach.Column));
            if DeltaCell > 1 then
            begin
              ScreenObject := Segment.FScreenObject;
//              SfrReach := AReach as TSfr_Cell;
              frmErrorsAndWarnings.AddWarning(Model, StrReachSeparationWarning,
                Format(StrSegment0dReach,
                [Segment.OriginalSegmentNumber, ReachIndex+1, TimeIndex+1, DeltaCell]),
                ScreenObject);
            end;
          end;
          PriorReach := AReach;
        end;
      end;

    end;

    Grid := Model.ModflowGrid;
    for SegementIndex := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[SegementIndex];
      ScreenObject := Segment.FScreenObject;
      for ReachIndex := 0 to Segment.ReachCount - 1 do
      begin
        AReach := Segment.Reaches[ReachIndex];
        SfrReach := AReach as TSfr_Cell;
        CheckStreamBottomElevation(ScreenObject, Grid, SfrReach);
      end;
    end;

  finally
    UsedSegments.Free;
    ParametersUsed.Free;
    for PIndex := 0 to Parameters.Count - 1 do
    begin
      Parameters.Objects[PIndex].Free;
    end;
    Parameters.Free;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSets5to7;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Segment: TSegment;
  SegementIndex: Integer;
  Item: TSfrParamIcalcItem;
  UsedSegments: TList;
  ITMP: Integer;
  IRDFLG: Integer;
  IPTFLG: Integer;
  ParametersUsed: TStringList;
  NP: Integer;
  Boundary: TSfrBoundary;
  ParamIndex: Integer;
  PIndex: Integer;
  Instance: TSfrParamInstance;
  Parameters: TStringList;
  Location: integer;
  InstanceList: TList;
  InstanceIndex: Integer;
  ParamName: string;
  SubSegIndex: Integer;
//  SfrBoundary: TSfrBoundary;
  SegIndex: Integer;
  ASegment: TSegment;
  SubSeg: TSubSegment;
begin
  UsedSegments := TList.Create;
  ParametersUsed := TStringList.Create;
  Parameters := TStringList.Create;
  try
    for PIndex := 0 to Model.ModflowPackages.
      SfrPackage.ParameterInstances.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Instance := Model.ModflowPackages.
        SfrPackage.ParameterInstances.Items[PIndex];
      Location := Parameters.IndexOf(Instance.ParameterName);
      if Location < 0 then
      begin
        InstanceList := TList.Create;
        Parameters.AddObject(Instance.ParameterName, InstanceList)
      end
      else
      begin
        InstanceList := Parameters.Objects[Location] as TList;
      end;
      InstanceList.Add(Instance);
    end;


    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      frmProgressMM.AddMessage(Format(StrWritingStressP,
        [TimeIndex + 1]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      // data set 5;
      UsedSegments.Clear;
      ParametersUsed.Clear;
      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      for SegementIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
//        SfrBoundary := Segment.FScreenObject.ModflowSfrBoundary;
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if (Item = nil) or (Item.Param = '') then
        begin
          UsedSegments.Add(Segment);
        end
        else
        begin
          if ParametersUsed.IndexOf(Item.Param) < 0 then
          begin
            ParametersUsed.Add(Item.Param);
          end;
        end;
      end;

      ITMP := 0;
      for SegIndex := 0 to UsedSegments.Count - 1 do
      begin
        ASegment := UsedSegments[SegIndex];
        if ASegment.FSubSegmentList.Count = 0 then
        begin
          Inc(ITMP);
        end
        else
        begin
          for SubSegIndex := 0 to ASegment.FSubSegmentList.Count - 1 do
          begin
            SubSeg := ASegment.FSubSegmentList[SubSegIndex];
            if SubSeg.Used then
            begin
              Inc(ITMP);
            end;
          end;
        end;
      end;

      if Model.ModflowOutputControl.PrintInputCellLists then
      begin
        IRDFLG := 0;
      end
      else
      begin
        IRDFLG := 1;
      end;

      IPTFLG := 0;

      NP := ParametersUsed.Count;

      WriteInteger(ITMP);
      WriteInteger(IRDFLG);
      WriteInteger(IPTFLG);
      if NSFRPAR > 0 then
      begin
        WriteInteger(NP);
      end
      else
      begin
        Assert(NP = 0);
      end;
      WriteString(' # Data Set 5, Stress period '
        + IntToStr(TimeIndex+1)
        + ': ITMP IRDFLG IPTFLG');
      if NSFRPAR > 0 then
      begin
        WriteString(' NP');
      end;
      NewLine;

      // Data Set 6
      for SegementIndex := 0 to UsedSegments.Count - 1 do
      begin
        // Data set 6a
        Segment := UsedSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if Item <> nil then
        begin

          Boundary := Segment.FScreenObject.ModflowSfrBoundary;
          if Segment.FSubSegmentList.Count = 0 then
          begin
            SubSegIndex := -1;
            WriteSegment(Segment, StressPeriod.StartTime,
              SubSegIndex, Item, Boundary, TimeIndex, False, 1);
          end
          else
          begin
            for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
            begin
              WriteSegment(Segment, StressPeriod.StartTime,
                SubSegIndex, Item, Boundary, TimeIndex, False, 1);
            end;
          end;
        end;
      end;

      // data set 7
      for ParamIndex := 0 to ParametersUsed.Count - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ParamName := ParametersUsed[ParamIndex];
        Location := Parameters.IndexOf(ParamName);
        Assert(Location >= 0);
        InstanceList := Parameters.Objects[Location] as TList;
        if InstanceList.Count > 1 then
        begin
          for InstanceIndex := 0 to InstanceList.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            Instance := InstanceList[InstanceIndex];
            if (Instance.StartTime >= StressPeriod.StartTime)
              and (Instance.StartTime < StressPeriod.EndTime) then
            begin
              WriteString(ParamName + ' ' + Instance.ParameterInstance);
              break;
            end;
            Assert(InstanceIndex < InstanceList.Count - 1);
          end;
        end
        else
        begin
          WriteString(ParamName);
        end;
        NewLine;
      end;

      // data set 8
      if TimeIndex = 0 then
      begin
        for SegIndex := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[SegIndex];
          if Segment.FSubSegmentList.Count = 0 then
          begin
            SubSegIndex := -1;
            WriteDataSet8(Segment, SubSegIndex, TimeIndex);
          end
          else
          begin
            for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
            begin
              WriteDataSet8(Segment, SubSegIndex, TimeIndex);
            end;
          end;
        end;
      end;

    end;
  finally
    UsedSegments.Free;
    ParametersUsed.Free;
    for PIndex := 0 to Parameters.Count - 1 do
    begin
      Parameters.Objects[PIndex].Free;
    end;
    Parameters.Free;
  end;
end;

procedure TModflowSFR_Writer.WriteFile(const AFileName: string; GageLines: TStrings);
var
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
//    Abbreviation := 'SFR8';
  end
  else
  begin
    Abbreviation := StrSFR;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(Abbreviation, Model.UnitNumbers.UnitNumber(StrSFR), FNameOfFile, foInput, Model);
//  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FPestParamUsed := False;
  WriteFileInternal;

  WriteGages(GageLines);
  WriteObsScript(AFileName);

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

procedure TModflowSFR_Writer.WriteGages(GageLines: TStrings);
var
  SegmentIndex: Integer;
  Segment: TSegment;
  Boundary: TSfrBoundary;
  OutTypes: TByteSet;
  OutIndex: Byte;
  GAGESEG: integer;
  GAGERCH: integer;
  UNIT_Number: integer;
  OUTTYPE: integer;
  ReachIndex: integer;
  SubSeg: TSubSegment;
  SubSegIndex: Integer;
  procedure WriteGage;
  var
    Line: string;
    OutputName: string;
//    ObsIndex: Integer;
//    ObsItem: TSfrObs;
  begin
    UNIT_Number := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
    Line := IntToStr(GAGESEG) + ' '
      + IntToStr(GAGERCH) + ' '
      + IntToStr(UNIT_Number) + ' '
      + IntToStr(OUTTYPE);
    GageLines.Add(Line);
//    Inc(StartUnitNumber);

    OutputName := ChangeFileExt(FNameOfFile, '');
    OutputName := ChangeFileExt(OutputName, '.sfrg');
    OutputName := OutputName + IntToStr(GageLines.Count);
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATA, UNIT_Number, OutputName, foOutput, Model);
    end;

    if OUTTYPE = 4 then
    begin
      Boundary.Observations.GageOutputName := OutputName;
      Boundary.Observations.GageOutputNames.Add(OutputName);
      if Boundary.Observations.Count > 0 then
      begin
        FSfrObservationsUsed := True;
      end;
    end;
  end;
begin
  FSfrObservationsUsed := False;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;
    Boundary.Observations.GageOutputNames.Clear;
    Assert((Boundary <> nil) and Boundary.Used);
    OutTypes := Boundary.OutTypes;
    if (OutTypes <> []) and (Boundary.GageLocation <> glNone) then
    begin
      GAGESEG := Segment.NewSegmentNumber;
      for OutIndex := 0 to 7 do
      begin
        if OutIndex in OutTypes then
        begin
          OUTTYPE := OutIndex;
          case Boundary.GageLocation of
            glFirst:
              begin
                GAGERCH := 1;
                WriteGage;
              end;
            glLast:
              begin
                if Segment.FSubSegmentList.Count = 0 then
                begin
                  GAGERCH := Segment.FReaches.Count;
                end
                else
                begin
                  SubSeg := Segment.FSubSegmentList[Segment.FSubSegmentList.Count-1];
                  GAGESEG := SubSeg.FSegmentNumber;
                  GAGERCH := SubSeg.ReachCount;
                end;
                WriteGage;
              end;
            glAll:
              begin
                if Segment.FSubSegmentList.Count = 0 then
                begin
                  for ReachIndex := 1 to Segment.FReaches.Count  do
                  begin
                    GAGERCH := ReachIndex;
                    WriteGage;
                  end;
                end
                else
                begin
                  for SubSegIndex := 0 to Segment.FSubSegmentList.Count - 1 do
                  begin
                    SubSeg := Segment.FSubSegmentList[SubSegIndex];
                    GAGESEG := SubSeg.FSegmentNumber;
                    for ReachIndex := 1 to SubSeg.ReachCount  do
                    begin
                      GAGERCH := ReachIndex;
                      WriteGage;
                    end;
                  end;
                end;
              end;
            else
              Assert(False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteObsScript(const AFileName: string);
var
  StartTime: Double;
  ScriptFileName: string;
  ComparisonsUsed: Boolean;
  SegmentIndex: Integer;
  ScreenObject: TScreenObject;
//  Observations: TLakeObservations;
  ObsIndex: Integer;
//  Obs: TLakeObs;
  CompIndex: Integer;
  CompItem: TObsCompareItem;
  Segment: TSegment;
  Boundary: TSfrBoundary;
  Observations: TSfrObservations;
  Obs: TSfrObs;
  ReachIndex: Integer;
  function GetObName(ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('SFR', ObjectIndex, Obs);
  end;
  function GetReachObName(ReachIndex, ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('R' + (ReachIndex+1).ToString, ObjectIndex, Obs);
  end;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  if not FSfrObservationsUsed then
  begin
    Exit;
  end;

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  ScriptFileName := ChangeFileExt(AFileName, '.Sfr_script');

  OpenFile(ScriptFileName);
  try
    ComparisonsUsed := False;
    // OBSERVATIONS block
    WriteString('BEGIN OBSERVATIONS');
    NewLine;
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[SegmentIndex];
      ScreenObject := Segment.FScreenObject;
      Boundary := Segment.FScreenObject.ModflowSfrBoundary;
      Observations := Boundary.Observations;
      if Observations.Count > 0 then
      begin
        if Observations.GageOutputNames.Count = 1 then
        begin
          WriteString('  # ');
          WriteString('Observations defined in ');
          WriteString(ScreenObject.Name);
          NewLine;
          WriteString('  FILENAME ');
          WriteString(Observations.GageOutputName);
          NewLine;

          for ObsIndex := 0 to Observations.Count - 1 do
          begin
            Obs := Observations[ObsIndex];
            WriteString('  OBSERVATION ');
            WriteString(GetObName(SegmentIndex, Obs));
            WriteString(' ');
            WriteString(Obs.ObservationType);
            WriteFloat(Obs.Time - StartTime);
            WriteFloat(Obs.ObservedValue);
            WriteFloat(Obs.Weight);
            WriteString(' PRINT');
            NewLine;
          end;

          if Observations.Comparisons.Count > 0 then
          begin
            ComparisonsUsed := True;
          end;
        end
        else
        begin
          WriteString('  # ');
          WriteString('Observations defined in ');
          WriteString(ScreenObject.Name);
          NewLine;
          for ReachIndex := 0 to Observations.GageOutputNames.Count - 1 do
          begin
            WriteString('  FILENAME ');
            WriteString(Observations.GageOutputNames[ReachIndex]);
            NewLine;

            for ObsIndex := 0 to Observations.Count - 1 do
            begin
              Obs := Observations[ObsIndex];
      //          FObsItemDictionary.Add(Obs.GUID, Obs);
              WriteString('  OBSERVATION ');
              WriteString(GetReachObName(ReachIndex, SegmentIndex, Obs));
              WriteString(' ');
              WriteString(Obs.ObservationType);
              WriteFloat(Obs.Time - StartTime);
              WriteFloat(Obs.ObservedValue);
              WriteFloat(0);
              WriteString(' NO_PRINT');
              NewLine;
            end;
          end;
          ComparisonsUsed := True;
        end;
      end;
    end;
    WriteString('END OBSERVATIONS');

    // DERIVED_OBSERVATIONS block
    if ComparisonsUsed then
    begin
      NewLine;
      NewLine;
      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      for SegmentIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegmentIndex];
        Boundary := Segment.FScreenObject.ModflowSfrBoundary;
        ScreenObject := Segment.FScreenObject;
        Observations := Boundary.Observations;
        if (Observations.Comparisons.Count > 0)
          or (Observations.GageOutputNames.Count > 1) then
        begin
          WriteString('  # ');
          WriteString('Observation comparisons defined in ');
          WriteString(ScreenObject.Name);
          NewLine;
        end;

        if Observations.GageOutputNames.Count > 1 then
        begin
          for ObsIndex := 0 to Observations.Count - 1 do
          begin
            Obs := Observations[ObsIndex];
            WriteString('  SUM ');
            WriteString(GetObName(SegmentIndex, Obs));
            WriteString(' ');

            for ReachIndex := 0 to Observations.GageOutputNames.Count - 1 do
            begin
              WriteString(GetReachObName(ReachIndex, SegmentIndex, Obs));
              WriteString(' ');
            end;

            WriteFloat(Obs.ObservedValue);
            WriteFloat(Obs.Weight);
            WriteString(' PRINT');
            NewLine;
          end;

        end;

        for CompIndex := 0 to Observations.Comparisons.Count - 1 do
        begin
          WriteString('  DIFFERENCE ');
          CompItem := Observations.Comparisons[CompIndex];
          WriteString(GetObName(SegmentIndex, CompItem));
          WriteString(' ');
          Obs := Observations[CompItem.Index1];
          WriteString(GetObName(SegmentIndex, Obs));
          WriteString(' ');
          Obs := Observations[CompItem.Index2];
          WriteString(GetObName(SegmentIndex, Obs));
          WriteFloat(CompItem.ObservedValue);
          WriteFloat(CompItem.Weight);
          WriteString(' PRINT');
          NewLine;
        end;
      end;
      WriteString('END DERIVED_OBSERVATIONS');
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet4f6e(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double; Segment: TSegment; SubSegIndex: integer);
var
  TableRow: TSfrFlowTableItemRecord;
  FlowTableIndex: Integer;
  FlowTable: TSfrFlowTableRecord;
  SubSeg: TSubSegment;
begin
  if SubSegIndex >= 0 then
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
  end;

  if ParamScreenObjectItem.ICalc = 4 then
  begin
    FlowTable := SfrBoundary.TableCollection.
      GetRecordForTime(StartTime);
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Flow);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS)');
    end;
    NewLine;
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Depth);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS)');
    end;
    NewLine;
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Width);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS)');
    end;
    NewLine;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet8(Segment: TSegment;
  SubSegIndex, TimeIndex: integer);
var
  SubSeg: TSubSegment;
  ParentSeg: TSegment;
  FirstSegmentInParent: Boolean;
  FlowFileName: string;
  SEGNUM: Integer;
  ExternalWriter: TExternalFlowFileWriter;
  FlowLines: TStringList;
  NUMVAL: Integer;
  IUNIT: Integer;
begin
  IUNIT := -1;
  if (TimeIndex > 0) or (SubSegIndex > 0)
    or (Segment.ExternalFlow.FlowFileChoice = ffcNone) then
  begin
    Exit;
  end;
  SubSeg := nil;
  if SubSegIndex >= 0 then
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
  end;
  if FIsChildModel then
  begin
    Assert(SubSeg <> nil);
    Assert(SubSeg.FAssociatedLgrSubSeg <> nil);
    ParentSeg := SubSeg.FAssociatedLgrSubSeg.FSegment;
    Assert(ParentSeg <> nil);
    FirstSegmentInParent :=
      ParentSeg.FSubSegmentList[0] = SubSeg.FAssociatedLgrSubSeg;
    if not FirstSegmentInParent then
    begin
      Exit;
    end;
  end
  else
  begin
//    ParentSeg := Segment;
//    FirstSegmentInParent := False;
  end;
  if SubSegIndex >= 0 then
  begin
    Assert(SubSeg <> nil);
    SEGNUM := SubSeg.FSegmentNumber;
  end
  else
  begin
    SEGNUM := Segment.NewSegmentNumber;
  end;
  NUMVAL := 0;
  case Segment.ExternalFlow.FlowFileChoice of
    ffcFileName:
      begin
        if FileExists(Segment.ExternalFlow.FullFlowFileName) then
        begin
          FlowLines := TStringList.Create;
          try
            FlowLines.LoadFromFile(Segment.ExternalFlow.FullFlowFileName);
            NUMVAL := FlowLines.Count;
          finally
            FlowLines.Free;
          end;
        end
        else
        begin
          // error
          NUMVAL := 0;
        end;
        FlowFileName := ExtractRelativePath(FNameOfFile,
          Segment.ExternalFlow.FullFlowFileName);
        if not FileExists(FlowFileName) then
        begin
          frmErrorsAndWarnings.AddError(Model,StrStreamFlowFileDoe,
            Format(StrTheStreamFlowFile2,
            [FlowFileName, Segment.FScreenObject.Name]), Segment.FScreenObject);
        end;
        IUNIT := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, IUNIT, FlowFileName,
            foInputAlreadyExists, Model, True);
        end;

      end;
    ffcSpecify:
      begin
        FlowFileName := ChangeFileExt(FNameOfFile, '');
        FlowFileName := TExternalFlowFileWriter.FileName(FlowFileName) + IntToStr(SEGNUM);
        IUNIT := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
        if not WritingTemplate then
        begin
          WriteToNameFile(StrData, IUNIT, FlowFileName, foInput, Model, False);
        end;
        ExternalWriter := TExternalFlowFileWriter.Create(
          Model, Segment.ExternalFlow, SEGNUM);
        try
          ExternalWriter.WriteFile(FlowFileName);
        finally
          ExternalWriter.Free;
        end;
        NUMVAL := Segment.ExternalFlow.FlowFileData.Count;
      end
    else Assert(False);
  end;
  WriteInteger(SEGNUM);
  WriteInteger(NUMVAL);
  WriteInteger(IUNIT);
  WriteString(' # Data set 8: SEGNUM, NUMVAL, IUNIT');
  NewLine;

//  Inc(StartUnitNumber);
end;

procedure TModflowSFR_Writer.WriteDataSet4e6d(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StressPeriod: integer; Segment: TSegment; SubSegIndex: integer);
var
  CrossSectionIndex: Integer;
  CrossSection: TSfrChannelRecord;
  SubSeg: TSubSegment;
  ZMin: Double;
  ScreenObject: TScreenObject;
begin
  if SubSegIndex >= 0 then
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
  end;

  if (ParamScreenObjectItem.ICalc = 2) and
    ((ISFROPT <= 1)  or (StressPeriod = 1) or Parameter) then
  begin
    CrossSection := SfrBoundary.ChannelValues.GetChannelTimeValuesFromTime(
      Model, ParamScreenObjectItem.StartTime);
    for CrossSectionIndex := 0 to 7 do
    begin
      WriteFloat(CrossSection.X[CrossSectionIndex]);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4e: XCPT1 XCPT2 ... XCPT8');
    end
    else
    begin
      WriteString(' # Data set 6d: XCPT1 XCPT2 ... XCPT8');
    end;
    NewLine;

    for CrossSectionIndex := 0 to 7 do
    begin
      WriteFloat(CrossSection.Z[CrossSectionIndex]);
    end;

    ZMin := CrossSection.Z[1];
    for CrossSectionIndex := 2 to 6 do
    begin
      if CrossSection.Z[CrossSectionIndex] < ZMin then
      begin
        ZMin := CrossSection.Z[CrossSectionIndex];
      end;
    end;
    if ZMin <> 0 then
    begin
      ScreenObject := SfrBoundary.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddError(Model, StrTheMinimumZValue,
        Format(StrObject0sStartin,
        [ScreenObject.Name, ParamScreenObjectItem.StartTime, ZMin]));
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4e: ZCPT1 ZCPT2 ... ZCPT8');
    end
    else
    begin
      WriteString(' # Data set 6d: ZCPT1 ZCPT2 ... ZCPT8');
    end;
    NewLine;
  end;
end;

procedure TModflowSFR_Writer.WriteUnsatSegmentValues(upstream: Boolean;
  var CommentLine: string; var ValuesWriten: boolean;
  UnsatUpstreamValues: TSfrUnsatSegmentStorage;
  PSegUnsatValue: PSfrUnsatSegmentRecord);
var
  UnsatSegmentValues: TSfrUnsatSegmentRecord;
begin
  ValuesWriten := True;
  Assert(Length(UnsatUpstreamValues.SrfUnsatSegmentArray) = 1);
  if Assigned(PSegUnsatValue) then
  begin
    UnsatSegmentValues := PSegUnsatValue^;
  end
  else
  begin
    UnsatSegmentValues := UnsatUpstreamValues.SrfUnsatSegmentArray[0];
  end;
  // THTS1, THTS2
  WritePestTemplateFormulaOrValue(UnsatSegmentValues.SaturatedWaterContent,
    UnsatSegmentValues.SaturatedWaterContentPestItem,
    '', ppmMultiply,
    PCellLocation(Addr(UnsatSegmentValues.Cell)), nil);
//  WriteFloat(UnsatSegmentValues.SaturatedWaterContent);
  if upstream then
  begin
    CommentLine := CommentLine + ' THTS1';
  end
  else
  begin
    CommentLine := CommentLine + ' THTS2';
  end;
  // THTI1, THTI2
  WritePestTemplateFormulaOrValue(UnsatSegmentValues.InitialWaterContent,
    UnsatSegmentValues.InitialWaterContentPestItem,
    '', ppmMultiply,
    PCellLocation(Addr(UnsatSegmentValues.Cell)), nil);
//  WriteFloat(UnsatSegmentValues.InitialWaterContent);
  if upstream then
  begin
    CommentLine := CommentLine + ' THTI1';
  end
  else
  begin
    CommentLine := CommentLine + ' THTI2';
  end;
  // EPS1, EPS2
  WritePestTemplateFormulaOrValue(UnsatSegmentValues.BrooksCoreyExponent,
    UnsatSegmentValues.BrooksCoreyExponentPestItem,
    '', ppmMultiply,
    PCellLocation(Addr(UnsatSegmentValues.Cell)), nil);
//  WriteFloat(UnsatSegmentValues.BrooksCoreyExponent);
  if upstream then
  begin
    CommentLine := CommentLine + ' EPS1';
  end
  else
  begin
    CommentLine := CommentLine + ' EPS2';
  end;
  // UHC1, UHC2
  if ISFROPT = 5 then
  begin
    WritePestTemplateFormulaOrValue(UnsatSegmentValues.VerticalSaturatedK,
      UnsatSegmentValues.VerticalSaturatedKPestItem,
      '', ppmMultiply,
      PCellLocation(Addr(UnsatSegmentValues.Cell)), nil);
//    WriteFloat(UnsatSegmentValues.VerticalSaturatedK);
    if upstream then
    begin
      CommentLine := CommentLine + ' UHC1';
    end
    else
    begin
      CommentLine := CommentLine + ' UHC2';
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteSegmentValues(StressPeriodIndex: Integer;
  Parameter: Boolean; UpOrDownStreamValues: TSfrSegmentStorage; upstream: Boolean;
  var CommentLine: string; var ValuesWriten: boolean;
  ParamScreenObjectItem: TSfrParamIcalcItem; PSegValue: PSfrSegmentRecord;
  Segment: TSegment; ParameterValue: double);
const
  HighRatio = 1E6;
var
  SegmentValues: TSfrSegmentRecord;
  WriteValue: Boolean;
  AqKx: Double;
  Ratio: Extended;
  ScreenObject: TScreenObject;
begin
  ValuesWriten := False;
  Assert(Length(UpOrDownStreamValues.SrfSegmentArray) >= 1);
  if PSegValue = nil then
  begin
    SegmentValues := UpOrDownStreamValues.SrfSegmentArray[0];
  end
  else
  begin
    SegmentValues := PSegValue^;
  end;
  // Hc1fact, Hc2fact, HCOND1, HCOND2
  if ISFROPT in [0, 4, 5] then
  begin
    ValuesWriten := True;
//    WriteFloat(SegmentValues.HydraulicConductivity);
    WritePestTemplateFormulaOrValue(SegmentValues.HydraulicConductivity,
      SegmentValues.HydraulicConductivityPestItem,
      SegmentValues.HydraulicConductivityPestSeriesItem,
      SegmentValues.HydraulicConductivityPestSeriesMethod,
      PCellLocation(Addr(SegmentValues.Cell)), nil);
    if Parameter then
    begin
      if upstream then
      begin
        CommentLine := CommentLine + ' Hc1fact';
      end
      else
      begin
        CommentLine := CommentLine + ' Hc2fact';
      end;
    end
    else
    begin
      if upstream then
      begin
        CommentLine := CommentLine + ' HCOND1';
      end
      else
      begin
        CommentLine := CommentLine + ' HCOND2';
      end;
    end;

    if PSegValue = nil then
    begin
      AqKx := AquiferKx(SegmentValues.Cell.Layer, SegmentValues.Cell.Row,
        SegmentValues.Cell.Column);
      if AqKx > 0 then
      begin
        Ratio := SegmentValues.HydraulicConductivity*ParameterValue/AqKx;
        if Ratio > HighRatio then
        begin
          ScreenObject := Segment.FScreenObject;
          frmErrorsAndWarnings.AddWarning(Model,StrHighSFRHydraulicC,
            Format(StrLayerRowColObject, [
            SegmentValues.Cell.Layer+1, SegmentValues.Cell.Row+1,
            SegmentValues.Cell.Column+1, ScreenObject.Name]),
            ScreenObject);
        end;
      end;
    end
    else
    begin
      { TODO : Figure out how to test this for child models. }
    end;

  end;
  // THICKM1, THICKM2
  WriteValue := ISFROPT in [0, 4, 5];
  if (ISFROPT in [4, 5]) and (ParamScreenObjectItem.ICalc in [1, 2]) then
  begin
    WriteValue := StressPeriodIndex = 0;
  end;
  if WriteValue then
  begin
    ValuesWriten := True;
//    WriteFloat(SegmentValues.StreamBedThickness);
    WritePestTemplateFormulaOrValue(SegmentValues.StreamBedThickness,
      SegmentValues.StreamBedThicknessPestItem,
      SegmentValues.StreamBedThicknessPestSeriesItem,
      SegmentValues.StreamBedThicknessPestSeriesMethod,
      PCellLocation(Addr(SegmentValues.Cell)), nil);
    if upstream then
    begin
      CommentLine := CommentLine + ' THICKM1';
    end
    else
    begin
      CommentLine := CommentLine + ' THICKM2';
    end;
  end;
  // ELEVUP, ELEVDN
  if WriteValue then
  begin
    ValuesWriten := True;
//    WriteFloat(SegmentValues.StreambedElevation);
    WritePestTemplateFormulaOrValue(SegmentValues.StreambedElevation,
      SegmentValues.StreambedElevationPestItem,
      SegmentValues.StreambedElevationPestSeriesItem,
      SegmentValues.StreambedElevationPestSeriesMethod,
      PCellLocation(Addr(SegmentValues.Cell)), nil);
    if upstream then
    begin
      CommentLine := CommentLine + ' ELEVUP';
    end
    else
    begin
      CommentLine := CommentLine + ' ELEVDN';
    end;
  end;
  // WIDTH1, WIDTH2
  WriteValue := ParamScreenObjectItem.ICalc <= 1;
  if WriteValue and (ISFROPT > 1) and (ParamScreenObjectItem.ICalc = 1) then
  begin
    WriteValue := StressPeriodIndex = 0;
  end;
  if WriteValue then
  begin
    ValuesWriten := True;
//    WriteFloat(SegmentValues.StreamWidth);
    WritePestTemplateFormulaOrValue(SegmentValues.StreamWidth,
      SegmentValues.StreamWidthPestItem,
      SegmentValues.StreamWidthPestSeriesItem,
      SegmentValues.StreamWidthPestSeriesMethod,
      PCellLocation(Addr(SegmentValues.Cell)), nil);
    if upstream then
    begin
      CommentLine := CommentLine + ' WIDTH1';
    end
    else
    begin
      CommentLine := CommentLine + ' WIDTH2';
    end;
  end;
  // DEPTH1, DEPTH2
  if ParamScreenObjectItem.ICalc = 0 then
  begin
    ValuesWriten := True;
//    WriteFloat(SegmentValues.StreamDepth);
    WritePestTemplateFormulaOrValue(SegmentValues.StreamDepth,
      SegmentValues.StreamDepthPestItem,
      SegmentValues.StreamDepthPestSeriesItem,
      SegmentValues.StreamDepthPestSeriesMethod,
      PCellLocation(Addr(SegmentValues.Cell)), nil);
    if upstream then
    begin
      CommentLine := CommentLine + ' DEPTH1';
    end
    else
    begin
      CommentLine := CommentLine + ' DEPTH2';
    end;
  end;
end;

function TModflowSFR_Writer.FindConvertedSegment(OriginalSegmentNumber: integer;
  Direction: TStreamDirection; out Segment: TSegment): integer;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  SubSeg: TSubSegment;
  DuplicateLakeIDScreenObject: TScreenObject;
begin
  result := 0;
  Segment := nil;
  if OriginalSegmentNumber = 0 then
  begin
    Exit;
  end
  else
  begin
    if OriginalSegmentNumber > 0 then
    begin
      if FSegDictionary = nil then
      begin
        FSegDictionary := TDictionary<integer,TSegment>.Create(FSegments.Count);
        for Index := 0 to FSegments.Count - 1 do
        begin
          Segment := FSegments[Index];
          try
            FSegDictionary.Add(Segment.OriginalSegmentNumber, Segment);
          except on EListError Do
            begin
              frmErrorsAndWarnings.AddError(Model, StrDuplicateSFRStream,
                Format(StrTheSFRSegmentNumb,
                [Segment.OriginalSegmentNumber, Segment.FScreenObject.Name]),
                Segment.FScreenObject);
            end;
          end;
        end;
      end;
      Segment := nil;
      if FSegDictionary.TryGetValue(OriginalSegmentNumber, Segment) then
      begin
        case Direction of
          sdDownstream: result := Segment.NewSegmentNumber;
          sdUpstream:
            begin
              if Segment.FSubSegmentList.Count = 0 then
              begin
                result := Segment.NewSegmentNumber;
              end
              else
              begin
                SubSeg := Segment.FSubSegmentList[
                  Segment.FSubSegmentList.Count-1];
                Result := SubSeg.FSegmentNumber;
              end;
            end
          else Assert(False);
        end;
      end;
//      for Index := 0 to FSegments.Count - 1 do
//      begin
//        Segment := FSegments[Index];
//        if Segment.OriginalSegmentNumber = OriginalSegmentNumber then
//        begin
//          case Direction of
//            sdDownstream: result := Segment.NewSegmentNumber;
//            sdUpstream:
//              begin
//                if Segment.FSubSegmentList.Count = 0 then
//                begin
//                  result := Segment.NewSegmentNumber;
//                end
//                else
//                begin
//                  SubSeg := Segment.FSubSegmentList[
//                    Segment.FSubSegmentList.Count-1];
//                  Result := SubSeg.FSegmentNumber;
//                end;
//              end
//            else Assert(False);
//          end;
//          Exit;
//        end;
//      end;
    end
    else
    begin
      if Model.ModflowPackages.LakPackage.IsSelected then
      begin
        // If TModflowSFR_Writer becomes persistant,
        // there will have to be a better test for determining
        // when the lake list needs to be filled.
        if FLakes = nil then
        begin
          FLakes := TList.Create;
          for Index := 0 to Model.ScreenObjectCount - 1 do
          begin
            AScreenObject := Model.ScreenObjects[Index];
            if (AScreenObject.ModflowLakBoundary <> nil)
              and AScreenObject.ModflowLakBoundary.Used then
            begin
              FLakes.Add(AScreenObject);
            end;
          end;
        end;
        if FLakDictionary = nil then
        begin
          FLakDictionary := TDictionary<Integer, TScreenObject>.Create(FLakes.Count);
          for Index := 0 to FLakes.Count - 1 do
          begin
            AScreenObject := FLakes[Index];
            Assert(AScreenObject.ModflowLakBoundary <> nil);
            if FLakDictionary.TryGetValue(
              AScreenObject.ModflowLakBoundary.LakeID,
              DuplicateLakeIDScreenObject) then
            begin
              frmErrorsAndWarnings.AddError(Model, StrTwoOrMoreLakeObjects,
                Format(StrTheLakeObjectNamed,
                [AScreenObject.Name, DuplicateLakeIDScreenObject.Name]),
                AScreenObject);
            end
            else
            begin
              FLakDictionary.Add(AScreenObject.ModflowLakBoundary.LakeID,
                AScreenObject);
            end;
          end;
        end;
        OriginalSegmentNumber := -OriginalSegmentNumber;
        if FLakDictionary.TryGetValue(OriginalSegmentNumber, AScreenObject) then
        begin
          Assert(AScreenObject.ModflowLakBoundary <> nil);
          result := -AScreenObject.ModflowLakBoundary.TrueLakeID;
        end;
//        for Index := 0 to FLakes.Count - 1 do
//        begin
//          AScreenObject := FLakes[Index];
//          Assert(AScreenObject.ModflowLakBoundary <> nil);
//          if AScreenObject.ModflowLakBoundary.LakeID = OriginalSegmentNumber then
//          begin
//            result := -AScreenObject.ModflowLakBoundary.TrueLakeID;
//            Exit;
//          end;
//        end;
      end;
    end;
  end;
end;

function TModflowSFR_Writer.GetSegment(Index: integer): TSegment;
begin
  result := FSegments[Index];
end;

function TModflowSFR_Writer.GetSegmentCount: integer;
begin
  result := FSegments.Count;
end;

procedure TModflowSFR_Writer.CheckOutflowSegments(StartTime: double;
  Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
  SfrBoundary: TSfrBoundary; SubSegIndex: integer);
var
  IUPSEG: Integer;
  SubSeg: TSubSegment;
  ParentSeg: TSegment;
  FirstSegmentInParent: Boolean;
  NextSubSeg: TSubSegment;
  OUTSEG: Integer;
  OutflowSegement: TSegment;
  TerminalCell: TValueCell;
  ConnectedCell: TValueCell;
  MaxDistance: integer;
  SourceSegment: TSegment;
  UpstreamScreenObject: TScreenObject;
  DownstreamScreenObject: TScreenObject;
  AScreenObject: TScreenObject;
begin
  SubSeg := nil;
  if SubSegIndex >= 0 then
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
  end;
  if FIsChildModel then
  begin
    Assert(SubSeg <> nil);
    if SubSeg.FAssociatedLgrSubSeg <> nil then
    begin
//      Assert(SubSeg.FAssociatedLgrSubSeg <> nil);
      ParentSeg := SubSeg.FAssociatedLgrSubSeg.FSegment;
      Assert(ParentSeg <> nil);
      FirstSegmentInParent :=
        ParentSeg.FSubSegmentList[0] = SubSeg.FAssociatedLgrSubSeg;
    end
    else
    begin
      FirstSegmentInParent := false;
    end;
  end
  else
  begin
//    ParentSeg := Segment;
    FirstSegmentInParent := False;
  end;


  // OUTSEG
  if (SubSegIndex = Segment.FSubSegmentList.Count -1) then
  begin
    // Parent models and last subsegment in child models.
    OUTSEG := FindConvertedSegment(ParamScreenObjectItem.OutflowSegment,
      sdDownstream, OutflowSegement);
    if OUTSEG = Segment.FNewSegmentNumber then
    begin
      AScreenObject := Segment.FScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddError(Model, StrInvalidSFROutflow,
        Format(StrIn1sTheSFRSeg, [AScreenObject.Name]), AScreenObject);
    end;
    if (OUTSEG > 0) and not FIsChildModel then
    begin
      Assert(OutflowSegement <> nil);
      TerminalCell := Segment.FReaches.Last;
      ConnectedCell := OutflowSegement.FReaches.First;
      MaxDistance := Max(Abs(TerminalCell.Column - ConnectedCell.Column),
        Abs(TerminalCell.Row - ConnectedCell.Row));
      if MaxDistance > 1 then
      begin
        UpstreamScreenObject := Segment.FScreenObject as TScreenObject;
        DownstreamScreenObject := OutflowSegement.FScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model, StrLargeSeparationBet,
           Format(StrTheDownstreamEndO,
           [UpstreamScreenObject.Name, DownstreamScreenObject.Name, MaxDistance]),
           UpstreamScreenObject);
      end;
    end;
  end
  else
  begin
    // Subsegments in child model except last sub segment
    Assert(SubSeg <> nil);
    if not FIsChildModel then
    begin
      NextSubSeg := Segment.FSubSegmentList[SubSegIndex+1];
      if not NextSubSeg.Used then
      begin
        OUTSEG := FindConvertedSegment(ParamScreenObjectItem.OutflowSegment,
          sdDownstream, OutflowSegement);
        if OUTSEG = Segment.FNewSegmentNumber then
        begin
          AScreenObject := Segment.FScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model, StrInvalidSFROutflow,
            Format(StrIn1sTheSFRSeg, [AScreenObject.Name]), AScreenObject);
        end;
        if (OUTSEG > 0) and not FIsChildModel then
        begin
          Assert(OutflowSegement <> nil);
          TerminalCell := Segment.FReaches.Last;
          ConnectedCell := OutflowSegement.FReaches.First;
          MaxDistance := Max(Abs(TerminalCell.Column - ConnectedCell.Column),
            Abs(TerminalCell.Row - ConnectedCell.Row));
          if MaxDistance > 1 then
          begin
            UpstreamScreenObject := Segment.FScreenObject as TScreenObject;
            DownstreamScreenObject := OutflowSegement.FScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddWarning(Model, StrLargeSeparationBet,
               Format(StrTheDownstreamEndO,
               [UpstreamScreenObject.Name, DownstreamScreenObject.Name, MaxDistance]),
               UpstreamScreenObject);
          end;
        end;
      end;
    end;
  end;

  // IUPSEG
  if SubSegIndex < 0 then
  begin
    IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
      sdUpstream, SourceSegment);
  end
  else
  begin
    Assert(SubSeg <> nil);
    if FIsChildModel then
    begin
      if FirstSegmentInParent then
      begin
        IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
          sdUpstream, SourceSegment);
      end
      else
      begin
        IUPSEG := 0;
      end;
    end
    else
    begin
      if SubSegIndex = 0 then
      begin
        IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
          sdUpstream, SourceSegment);
      end
      else
      begin
        IUPSEG := 0
      end;
    end;
  end;

  if (IUPSEG > 0) and not FIsChildModel then
  begin
    Assert(SourceSegment <> nil);
    TerminalCell := Segment.FReaches.First;
    ConnectedCell := SourceSegment.FReaches.Last;
    MaxDistance := Max(Abs(TerminalCell.Column - ConnectedCell.Column),
      Abs(TerminalCell.Row - ConnectedCell.Row));
    if MaxDistance > 1 then
    begin
      DownstreamScreenObject := Segment.FScreenObject as TScreenObject;
      UpstreamScreenObject := SourceSegment.FScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model, StrLargeDiversionSeparation,
         Format(StrTheDownstreamEndO,
         [UpstreamScreenObject.Name, DownstreamScreenObject.Name, MaxDistance]),
         UpstreamScreenObject);
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet4b6a(StartTime: double;
  Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
  SfrBoundary: TSfrBoundary; DataSet4B: boolean; SubSegIndex: integer);
var
  ICALC: Integer;
  FlowTable: TSfrFlowTableRecord;
  SegmentFlow: TSfrSegmentFlowRecord;
  ChannelValues: TSfrChannelRecord;
  EqValues: TSfrEquationRecord;
  IUPSEG: Integer;
  SubSeg: TSubSegment;
  ParentSeg: TSegment;
//  IsChildModel: Boolean;
  FirstSegmentInParent: Boolean;
  TotalParentSegLength: double;
  ParentSubSegIndex: Integer;
  ParentSubSeg: TSubSegment;
  RUNOFF: Double;
//  LgrUsed: Boolean;
  PriorSubSeg: TSubSegment;
  LocalPhastModel: TPhastModel;
  ChildModelIndex: Integer;
  LGRGRID: Integer;
  LGRSEG: Integer;
  OtherSubSegIndex: Integer;
  NextSubSeg: TSubSegment;
  ChildSeg: TSegment;
  ChildSubSeg: TSubSegment;
  OUTSEG: Integer;
  OutflowSegement: TSegment;
  TerminalCell: TValueCell;
  ConnectedCell: TValueCell;
  MaxDistance: integer;
  SourceSegment: TSegment;
  UpstreamScreenObject: TScreenObject;
  DownstreamScreenObject: TScreenObject;
  ScreenObject: TScreenObject;
  ExtendedTemplateCharacter: Char;
  Formula: string;
  Fraction: Extended;
begin
  ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
//  IsChildModel := Model is TChildModel;
  SubSeg := nil;
  if SubSegIndex >= 0 then
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
  end;
  if FIsChildModel then
  begin
    Assert(SubSeg <> nil);
    Assert(SubSeg.FAssociatedLgrSubSeg <> nil);
    ParentSeg := SubSeg.FAssociatedLgrSubSeg.FSegment;
    if ParentSeg <> nil then
    begin
      FirstSegmentInParent :=
        ParentSeg.FSubSegmentList[0] = SubSeg.FAssociatedLgrSubSeg;
    end
    else
    begin
      FirstSegmentInParent := False;
      ScreenObject := SfrBoundary.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model, StrSFRChildModelLink,
        Format(StrTheObjectSMayNo, [ScreenObject.Name]), ScreenObject)
    end;
  end
  else
  begin
    ParentSeg := Segment;
    FirstSegmentInParent := False;
  end;

  // data set 4b and 6a
  //  NSEG
  if SubSegIndex >= 0 then
  begin
    Assert(SubSeg <> nil);
    WriteInteger(SubSeg.FSegmentNumber);
  end
  else
  begin
    WriteInteger(Segment.NewSegmentNumber);
  end;

  // ICALC
  ICALC := ParamScreenObjectItem.ICalc;
  WriteInteger(ICALC);

  // OUTSEG
  if (SubSegIndex = Segment.FSubSegmentList.Count -1) then
  begin
    // Parent models and last subsegment in child models.
    OUTSEG := FindConvertedSegment(ParamScreenObjectItem.OutflowSegment,
      sdDownstream, OutflowSegement);
    WriteInteger(OUTSEG);
    if (OUTSEG > 0) and not FIsChildModel then
    begin
      Assert(OutflowSegement <> nil);
      TerminalCell := Segment.FReaches.Last;
      ConnectedCell := OutflowSegement.FReaches.First;
      MaxDistance := Max(Abs(TerminalCell.Column - ConnectedCell.Column),
        Abs(TerminalCell.Row - ConnectedCell.Row));
      if MaxDistance > 1 then
      begin
        UpstreamScreenObject := Segment.FScreenObject as TScreenObject;
        DownstreamScreenObject := OutflowSegement.FScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model, StrLargeSeparationBet,
           Format(StrTheDownstreamEndO,
           [UpstreamScreenObject.Name, DownstreamScreenObject.Name, MaxDistance]),
           UpstreamScreenObject);
      end;
    end;
  end
  else
  begin
    // Subsegments in child model except last sub segment
    Assert(SubSeg <> nil);
    if FIsChildModel then
    begin
      WriteInteger(SubSeg.FSegmentNumber+1);
    end
    else
    begin
      NextSubSeg := Segment.FSubSegmentList[SubSegIndex+1];
      if NextSubSeg.Used then
      begin
        WriteInteger(SubSeg.FSegmentNumber+1);
      end
      else
      begin
        OUTSEG := FindConvertedSegment(ParamScreenObjectItem.OutflowSegment,
          sdDownstream, OutflowSegement);
        WriteInteger(OUTSEG);
        if (OUTSEG > 0) and not FIsChildModel then
        begin
          Assert(OutflowSegement <> nil);
          TerminalCell := Segment.FReaches.Last;
          ConnectedCell := OutflowSegement.FReaches.First;
          MaxDistance := Max(Abs(TerminalCell.Column - ConnectedCell.Column),
            Abs(TerminalCell.Row - ConnectedCell.Row));
          if MaxDistance > 1 then
          begin
            UpstreamScreenObject := Segment.FScreenObject as TScreenObject;
            DownstreamScreenObject := OutflowSegement.FScreenObject as TScreenObject;
            frmErrorsAndWarnings.AddWarning(Model, StrLargeSeparationBet,
               Format(StrTheDownstreamEndO,
               [UpstreamScreenObject.Name, DownstreamScreenObject.Name, MaxDistance]),
               UpstreamScreenObject);
          end;
        end;
      end;
    end;
  end;

  // IUPSEG
  if SubSegIndex < 0 then
  begin
    IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
      sdUpstream, SourceSegment);
  end
  else
  begin
    Assert(SubSeg <> nil);
    if FIsChildModel then
    begin
      if FirstSegmentInParent then
      begin
        IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
          sdUpstream, SourceSegment);
      end
      else
      begin
        IUPSEG := 0;
      end;
    end
    else
    begin
      if SubSegIndex = 0 then
      begin
        IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment,
          sdUpstream, SourceSegment);
      end
      else
      begin
        IUPSEG := 0
      end;
    end;
  end;
  WriteInteger(IUPSEG);

  if IUPSEG > 0 then
  begin
    // IPRIOR
    WriteInteger(ParamScreenObjectItem.IPRIOR);
  end;
  if ICALC = 4 then
  begin
    // NSTRPTS]
    FlowTable := SfrBoundary.TableCollection.GetRecordForTime(
      StartTime);
    WriteInteger(Length(FlowTable.SfrFlowTableArray));
  end;

  SegmentFlow := SfrBoundary.SegmentFlows.GetFlowValuesFromTime(
    StartTime);

  // FLOW
  if SubSegIndex < 0 then
  begin
//    WriteFloat(SegmentFlow.Flow);
    WritePestFormulaOrValue(SegmentFlow.FlowPestItem,
      SegmentFlow.FlowPestSeriesItem, SegmentFlow.FlowPestSeriesMethod,
      SegmentFlow.Flow);
  end
  else
  begin
    if FIsChildModel then
    begin
      if FirstSegmentInParent then
      begin
//        WriteFloat(SegmentFlow.Flow);
        WritePestFormulaOrValue(SegmentFlow.FlowPestItem,
          SegmentFlow.FlowPestSeriesItem, SegmentFlow.FlowPestSeriesMethod,
          SegmentFlow.Flow);
      end
      else
      begin
        WriteFloat(0);
      end;
    end
    else
    begin
      if SubSegIndex = 0 then
      begin
//        WriteFloat(SegmentFlow.Flow);
        WritePestFormulaOrValue(SegmentFlow.FlowPestItem,
          SegmentFlow.FlowPestSeriesItem, SegmentFlow.FlowPestSeriesMethod,
          SegmentFlow.Flow);
      end
      else
      begin
        WriteFloat(0);
      end;
    end;
  end;

  // RUNOFF
  RUNOFF := SegmentFlow.Runnoff;
  Formula := GetPestTemplateFormulaOrValue(SegmentFlow.Runnoff,
    SegmentFlow.RunnoffPestItem, SegmentFlow.RunnoffPestSeriesItem,
    SegmentFlow.RunnoffPestSeriesMethod, nil, nil);
  if (SegmentFlow.RunnoffPestItem <> '') or (SegmentFlow.RunnoffPestSeriesItem <> '') then
  begin
    FPestParamUsed := True;
  end;

  if SubSegIndex >= 0 then
  begin
    // Compute length of parent segment.
    TotalParentSegLength := 0;
    for ParentSubSegIndex := 0 to ParentSeg.FSubSegmentList.Count - 1 do
    begin
      ParentSubSeg := ParentSeg.FSubSegmentList[ParentSubSegIndex];
      TotalParentSegLength := TotalParentSegLength + ParentSubSeg.FTotalLength;
    end;

    Assert(SubSeg <> nil);
    if TotalParentSegLength > 0 then
    begin
      if FIsChildModel then
      begin
        Fraction := SubSeg.FAssociatedLgrSubSeg.FTotalLength/TotalParentSegLength;
      end
      else
      begin
        Fraction := SubSeg.FTotalLength/TotalParentSegLength;
      end;
      RUNOFF := RUNOFF * Fraction;
      Formula := Format('0:s * (%1:s)', [FortranFloatToStr(Fraction), Formula]);
    end;
  end;
  if Model.PestUsed and WritingTemplate and
    ((SegmentFlow.RunnoffPestItem <> '')
    or (SegmentFlow.RunnoffPestSeriesItem <> '')) then
  begin
    Formula := Format(' %0:s                    %1:s%0:s ',
      [ExtendedTemplateCharacter, Formula]);
    WriteString(Formula);
  end
  else
  begin
    WriteFloat(RUNOFF);
  end;

  // ETSW
//  WriteFloat(SegmentFlow.Evapotranspiration);
  WritePestFormulaOrValue(SegmentFlow.EvapotranspirationPestItem,
    SegmentFlow.EvapotranspirationPestSeriesItem,
    SegmentFlow.EvapotranspirationPestSeriesMethod,
    SegmentFlow.Evapotranspiration);
  // PPTSW
//  WriteFloat(SegmentFlow.Precipitation);
  WritePestFormulaOrValue(SegmentFlow.PrecipitationPestItem,
    SegmentFlow.PrecipitationPestSeriesItem,
    SegmentFlow.PrecipitationPestSeriesMethod,
    SegmentFlow.Precipitation);
  // [ROUGHCH] [ROUGHBK] [CDPTH] [FDPTH]
  if ICALC in [1, 2] then
  begin
    // ROUGHCH
    ChannelValues := SfrBoundary.ChannelValues.GetChannelTimeValuesFromTime(
      Model, StartTime);
    WriteFloat(ChannelValues.ChannelRoughness);
    if ICALC = 2 then
    begin
      // ROUGHBK
      WriteFloat(ChannelValues.BankRoughness);
    end;
  end;
  if ICALC = 3 then
  begin
    EqValues := SfrBoundary.EquationValues.
      GetEquationTimeValuesFromTime(StartTime);
    // CDPTH
    WriteFloat(EqValues.DepthCoefficient);
    // FDPTH
    WriteFloat(EqValues.DepthExponent);
    // AWDTH
    WriteFloat(EqValues.WidthCoefficient);
    // BWDTH
    WriteFloat(EqValues.WidthExponent);
  end;

  if FLgrUsed then
  begin
    Assert(SubSeg <> nil);
    if FIsChildModel then
    begin
      if SubSeg.FAssociatedLgrSubSeg.FIndex = 0 then
      begin
        LGRGRID := 0;
        LGRSEG := 0;
      end
      else
      begin
        LGRGRID := 1;
        LGRSEG := SubSeg.FAssociatedLgrSubSeg.FSegmentNumber -1;
        for OtherSubSegIndex := SubSegIndex-1 downto 0 do
        begin
          if SubSeg.FAssociatedLgrSubSeg.FSegmentNumber -1 = LGRSEG then
          begin
            LGRGRID := 0;
            LGRSEG := 0;
            break;
          end;
        end;
      end;
    end
    else
    begin
      if SubSeg.FModel <> Model then
      begin
        LGRGRID := -1;
        LGRSEG := 0;
      end
      else
      begin
        if SubSeg.FIndex > 0 then
        begin
          PriorSubSeg := Segment.FSubSegmentList[SubSeg.FIndex-1];
          LocalPhastModel := TPhastModel(Model);
          LGRGRID := 0;
          for ChildModelIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
          begin
            if LocalPhastModel.ChildModels[ChildModelIndex].ChildModel = PriorSubSeg.FModel then
            begin
              LGRGRID := ChildModelIndex+2;
              Break;
            end;
          end;
          if PriorSubSeg.FAssociatedLgrSubSeg <> nil then
          begin
            ChildSeg := PriorSubSeg.FAssociatedLgrSubSeg.FSegment;
            ChildSubSeg := ChildSeg.SubSegmentList[ChildSeg.SubSegmentList.Count -1];
            LGRSEG := ChildSubSeg.FSegmentNumber;
          end
          else
          begin
            LGRSEG := 0;
          end;
        end
        else
        begin
          LGRGRID := 0;
          LGRSEG := 0;
        end;
      end;
    end;
    WriteInteger(LGRGRID);
    WriteInteger(LGRSEG);
  end;

  if DataSet4B then
  begin
    WriteString(' # Data Set 4b: ');
  end
  else
  begin
    WriteString(' # Data Set 6a: ');
  end;
  WriteString('NSEG ICALC OUTSEG IUPSEG');
  if IUPSEG <> 0 then
  begin
    WriteString(' IPRIOR');
  end;
  if ICALC = 4 then
  begin
    WriteString(' NSTRPTS');
  end;
  WriteString(' FLOW RUNOFF ETSW PPTSW');
  if ICALC in [1, 2] then
  begin
    // ROUGHCH
    WriteString(' ROUGHCH');
    if ICALC = 2 then
    begin
      WriteString(' ROUGHBK');
    end;
  end;
  if ICALC = 3 then
  begin
    WriteString(' CDPTH FDPTH AWDTH BWDTH');
  end;
  if FLgrUsed then
  begin
    WriteString(' LGRGRID LGRSEG');
  end;
  NewLine;
end;

procedure TModflowSFR_Writer.WriteDataSet4c6b(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double; StressPeriodIndex: integer; Segment: TSegment;
  SubSegIndex: integer; ParameterValue: double);
var
  upstream: Boolean;
  CommentLine: string;
  UpstreamValues: TSfrSegmentStorage;
  UnsatUpstreamValues: TSfrUnsatSegmentStorage;
  ValuesWriten: boolean;
  ErrorObject: TScreenObject;
  PSegValue: PSfrSegmentRecord;
  SubSeg: TSubSegment;
  PSegUnsatValue: PSfrUnsatSegmentRecord;
begin
  upstream := True;

  CommentLine :=' #';
  if Parameter then
  begin
    CommentLine := CommentLine + ' Data set 4c:';
  end
  else
  begin
    CommentLine := CommentLine + ' Data set 6b:';
  end;
  ValuesWriten := False;

  UpstreamValues := SfrBoundary.UpstreamSegmentValues.
    GetBoundaryByStartTime(StartTime, Model)
    as TSfrSegmentStorage;
  if UpstreamValues = nil then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
    Exit;
  end
  else if Length(UpstreamValues.SrfSegmentArray) = 0 then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      Format(StrTheSEndOfTheFo, [StrUpstream]), ErrorObject.Name, ErrorObject);
    Exit;
  end;

  if SubSegIndex < 0 then
  begin
    PSegValue := nil;
    PSegUnsatValue := nil;
  end
  else
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
    PSegValue := @(SubSeg.FUpstreamValues[StressPeriodIndex]);
    PSegUnsatValue := @(SubSeg.FUpstreamUnsatValues[StressPeriodIndex]);
  end;
  WriteSegmentValues(StressPeriodIndex, Parameter, UpstreamValues,
    upstream, CommentLine, ValuesWriten, ParamScreenObjectItem, PSegValue,
    Segment, ParameterValue);

  if (ISFROPT in [4,5]) and (StressPeriodIndex = 0) then
  begin
    if SfrBoundary.UpstreamUnsatSegmentValues.
      BoundaryCount[Model] > 0 then
    begin
      UnsatUpstreamValues := SfrBoundary.UpstreamUnsatSegmentValues.
        Boundaries[0, Model] as TSfrUnsatSegmentStorage;
      WriteUnsatSegmentValues(upstream, CommentLine, ValuesWriten,
        UnsatUpstreamValues, PSegUnsatValue);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, UnsatError,
        (SfrBoundary.ScreenObject as TScreenObject).Name,
        SfrBoundary.ScreenObject);
    end;
  end;

  if ValuesWriten then
  begin
    WriteString(CommentLine);
    NewLine;
  end;

end;

procedure TModflowSFR_Writer.WriteDataSet4d6c(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double; StressPeriodIndex: integer; Segment: TSegment;
  SubSegIndex: integer; ParameterValue: double);
var
  upstream: Boolean;
  CommentLine: string;
  DownstreamValues: TSfrSegmentStorage;
  UnsatDownstreamValues: TSfrUnsatSegmentStorage;
  ValuesWriten: boolean;
  ErrorObject: TScreenObject;
  PSegValue: PSfrSegmentRecord;
  SubSeg: TSubSegment;
  PSegUnsatValue: PSfrUnsatSegmentRecord;
begin
  upstream := False;

  CommentLine :=' #';
  if Parameter then
  begin
    CommentLine := CommentLine + ' Data set 4d:';
  end
  else
  begin
    CommentLine := CommentLine + ' Data set 6c:';
  end;
  ValuesWriten:= False;

  DownstreamValues := SfrBoundary.DownstreamSegmentValues.
    GetBoundaryByStartTime(StartTime, Model)
    as TSfrSegmentStorage;
  if DownstreamValues = nil then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      StrInvalidStartingTimeStep1, ErrorObject.Name, ErrorObject);
    Exit;
  end
  else if Length(DownstreamValues.SrfSegmentArray) = 0 then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      Format(StrTheSEndOfTheFo, [StrDownstream]), ErrorObject.Name, ErrorObject);
    Exit;
  end;
  if SubSegIndex < 0 then
  begin
    PSegValue := nil;
    PSegUnsatValue := nil;
  end
  else
  begin
    SubSeg := Segment.FSubSegmentList[SubSegIndex];
    if not SubSeg.Used then
    begin
      Exit;
    end;
    PSegValue := @(SubSeg.FDownstreamValues[StressPeriodIndex]);
    PSegUnsatValue := @(SubSeg.FDownstreamUnsatValues[StressPeriodIndex]);
  end;
  WriteSegmentValues(StressPeriodIndex, Parameter, DownstreamValues,
    upstream, CommentLine, ValuesWriten, ParamScreenObjectItem, PSegValue,
    Segment, ParameterValue);

  if (ISFROPT in [4,5]) and (StressPeriodIndex = 0) then
  begin
    if SfrBoundary.DownstreamUnsatSegmentValues.
      BoundaryCount[Model] > 0 then
    begin
      UnsatDownstreamValues := SfrBoundary.DownstreamUnsatSegmentValues.
        Boundaries[0, Model] as TSfrUnsatSegmentStorage;
      WriteUnsatSegmentValues(upstream, CommentLine, ValuesWriten,
        UnsatDownstreamValues, PSegUnsatValue);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, UnsatError,
        (SfrBoundary.ScreenObject as TScreenObject).Name,
        SfrBoundary.ScreenObject);
    end;
  end;

  if ValuesWriten then
  begin
    WriteString(CommentLine);
    NewLine;
  end;

end;
{ TSegment }

function TSegment.AddSubSegment(AModel: TBaseModel; StressPeriodCount: integer): TSubSegment;
begin
  Result := TSubSegment.Create(AModel, self, StressPeriodCount);
  Result.FIndex := FSubSegmentList.Add(Result);
end;

constructor TSegment.Create;
begin
  FSubSegmentList:= TSubSegmentList.Create;
end;

destructor TSegment.Destroy;
begin
  FSubSegmentList.Free;
  FReaches.Free;
end;

function TSegment.GetReach(Index: integer): TValueCell;
begin
  result := FReaches[Index];
end;

function TSegment.GetReachCount: integer;
begin
  result := FReaches.Count;
end;

function TSegment.OriginalDiversionSegmentNumbers: TIntegerDynArray;
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: Integer;
  IntList: TIntegerList;
  Item: TSfrParamIcalcItem;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
  IntList := TIntegerList.Create;
  try
    IntList.Capacity := ParamIcalc.Count;
    IntList.Sorted := True;
    for Index := 0 to ParamIcalc.Count - 1 do
    begin
      Item := ParamIcalc.Items[Index];
      IntList.AddUnique(Item.DiversionSegment);
    end;
    setLength(result, IntList.Count);
    for Index := 0 to IntList.Count - 1 do
    begin
      result[Index] := IntList[Index];
    end;
  finally
    IntList.Free;
  end;
end;

function TSegment.OriginalDownStreamSegmentNumbers: TIntegerDynArray;
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: Integer;
  IntList: TIntegerList;
  Item: TSfrParamIcalcItem;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
  IntList := TIntegerList.Create;
  try
    IntList.Capacity := ParamIcalc.Count;
    IntList.Sorted := True;
    for Index := 0 to ParamIcalc.Count - 1 do
    begin
      Item := ParamIcalc.Items[Index];
      IntList.AddUnique(Item.OutflowSegment);
    end;
    setLength(result, IntList.Count);
    for Index := 0 to IntList.Count - 1 do
    begin
      result[Index] := IntList[Index];
    end;
  finally
    IntList.Free;
  end;
end;

function TSegment.OriginalSegmentNumber: integer;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  result := FScreenObject.ModflowSfrBoundary.SegmentNumber;
end;

procedure TSegment.SetNewSegmentNumber(const Value: integer);
begin
  FNewSegmentNumber := Value;
  FScreenObject.SfrSegmentNumber := value;
end;

{ TSubSegment }

function TSubSegment.AddReach(AReach: TSfr_Cell): Integer;
begin
  Result := FReachList.Add(AReach);
  FTotalLength := FTotalLength + AReach.LgrReachLength;
  if AReach.ReachLength > 0 then
  begin
    FUsed := True;
  end;
end;

constructor TSubSegment.Create(AModel: TBaseModel; ASegment: TSegment; StressPeriodCount: integer);
begin
  FSegment := ASegment;
  FModel := AModel;
  FReachList := TReachList.Create;
  FUsed := False;
  SetLength(FUpstreamValues, StressPeriodCount);
  SetLength(FDownstreamValues, StressPeriodCount);
  SetLength(FParentUpstreamValues, StressPeriodCount);
  SetLength(FParentDownstreamValues, StressPeriodCount);
  SetLength(FUpstreamUnsatValues, StressPeriodCount);
  SetLength(FDownstreamUnsatValues, StressPeriodCount);
  SetLength(FParentUpstreamUnsatValues, StressPeriodCount);
  SetLength(FParentDownstreamUnsatValues, StressPeriodCount);
end;

procedure TSubSegment.DeleteReach(Index: Integer);
begin
  FReachList.Delete(Index);
end;

destructor TSubSegment.Destroy;
begin
  FReachList.Free;
  inherited;
end;

function TSubSegment.GetReach(Index: Integer): TSfr_Cell;
begin
  Result := FReachList[Index];
end;

function TSubSegment.ReachCount: integer;
begin
  Result := FReachList.Count;
end;

{ TExternalFlowFileWriter }

constructor TExternalFlowFileWriter.Create(AModel: TCustomModel;
  ExternalFlow: TExternalFlowProperties; Const SegNum: integer);
begin
  inherited Create(AModel, etExport);
  FExternalFlow := ExternalFlow;
  FSegNum := SegNum;
end;

class function TExternalFlowFileWriter.Extension: string;
begin
  result := '.tab';
end;

procedure TExternalFlowFileWriter.WriteFile(const AFileName: string);
var
  index: Integer;
  StartTime: Double;
  Item: TFlowFileItem;
  TIME, INFLOW: double;
begin
  FFileName := FileName(AFileName) + IntToStr(FSegNum);
  StartTime := Model.ModflowFullStressPeriods[0].StartTime;
  FInputFileName := FFileName;
  OpenFile(FFileName);
  try
    for index := 0 to FExternalFlow.FlowFileData.Count - 1 do
    begin
      Item := FExternalFlow.FlowFileData[index];
      if FExternalFlow.ReferenceTimeChoice = ffrtModelMuseZero then
      begin
        TIME := Item.Time - StartTime;
      end
      else
      begin
        TIME := Item.Time;
      end;
      INFLOW := Item.Inflow;
      WriteFloat(TIME);
      WriteFloat(INFLOW);
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

end.
