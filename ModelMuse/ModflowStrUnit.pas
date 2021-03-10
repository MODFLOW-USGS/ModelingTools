unit ModflowStrUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes;

type
  TStrRecord = record
    Cell: TCellLocation;
    Conductance: double;
    Stage: double;
    BedTop: double;
    BedBottom: double;
    Flow: double;
    Width: double;
    Slope: Double;
    Roughness: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    StageAnnotation: string;
    BedTopAnnotation: string;
    BedBottomAnnotation: string;
    FlowAnnotation: string;
    WidthAnnotation: string;
    SlopeAnnotation: string;
    RoughnessAnnotation: string;
    SegmentNumber: Integer;
    ReachNumber: Integer;

    ConductancePest: string;
    StagePest: string;
    BedTopPest: string;
    BedBottomPest: string;
    FlowPest: string;
    WidthPest: string;
    SlopePest: string;
    RoughnessPest: string;

    ConductancePestSeriesName: string;
    StagePestSeriesName: string;
    BedTopPestSeriesName: string;
    BedBottomPestSeriesName: string;
    FlowPestSeriesName: string;
    WidthPestSeriesName: string;
    SlopePestSeriesName: string;
    RoughnessPestSeriesName: string;

    ConductancePestSeriesMethod: TPestParamMethod;
    StagePestSeriesMethod: TPestParamMethod;
    BedTopPestSeriesMethod: TPestParamMethod;
    BedBottomPestSeriesMethod: TPestParamMethod;
    FlowPestSeriesMethod: TPestParamMethod;
    WidthPestSeriesMethod: TPestParamMethod;
    SlopePestSeriesMethod: TPestParamMethod;
    RoughnessPestSeriesMethod: TPestParamMethod;

    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TStrArray = array of TStrRecord;

  TStrStorage = class(TCustomBoundaryStorage)
  private
    FStrArray: TStrArray;
    function GetStrArray: TStrArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property StrArray: TStrArray read GetStrArray;
  end;

  // @name represents a MODFLOW STR boundary for one time interval.
  // @name is stored by @link(TStrCollection).
  TStrItem = class(TCustomModflowBoundaryItem)
  private
    FOutflowSegment: integer;
    FDiversionSegment: integer;
    // See @link(Stage).
    procedure SetStage(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetStage: string;
    function GetBedBottom: string;
    function GetBedTop: string;
    function GetFlow: string;
    procedure SetBedBottom(const Value: string);
    procedure SetBedTop(const Value: string);
    procedure SetFlow(const Value: string);
    function GetRoughness: string;
    function GetSlope: string;
    function GetWidth: string;
    procedure SetRoughness(const Value: string);
    procedure SetSlope(const Value: string);
    procedure SetWidth(const Value: string);
    procedure SetDiversionSegment(const Value: integer);
    procedure SetOutflowSegment(const Value: integer);
    function GetSegmentNumber: Integer;
  protected
    // See @link(Stage).
    FStage: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    FBedTop: TFormulaObject;
    FBedBottom: TFormulaObject;
    FFlow: TFormulaObject;
    FWidth: TFormulaObject;
    Fslope: TFormulaObject;
    FRoughness: TFormulaObject;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    property SegmentNumber: Integer read GetSegmentNumber;
  published
    // @name is the formula used to set the stage
    // of this boundary.
    property Stage: string read GetStage write SetStage;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
    property BedTop: string read GetBedTop write SetBedTop;
    property BedBottom: string read GetBedBottom write SetBedBottom;
    property Flow: string read GetFlow write SetFlow;
    property Width: string read GetWidth write SetWidth;
    property Slope: string read GetSlope write SetSlope;
    property Roughness: string read GetRoughness write SetRoughness;
    property OutflowSegment: integer read FOutflowSegment
      write SetOutflowSegment;
    property DiversionSegment: integer read FDiversionSegment
      write SetDiversionSegment;
  end;

  TStrTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to perform notifications of the Stages for a series of
    // Stream Boundaries over a series of time intervals.
    FStageData: TModflowTimeList;
    // @name is used to perform notifications of the Conductances
    // for a series of
    // STR Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    FBedTopData: TModflowTimeList;
    FBedBottomData: TModflowTimeList;
    FFlowData: TModflowTimeList;
    FWidthData: TModflowTimeList;
    FSlopeData: TModflowTimeList;
    FroughnessData: TModflowTimeList;
    FStrSegmentNumberData: TModflowTimeList;
    FStrOutflowSegmentNumberData: TModflowTimeList;
    FStrReachNumberData: TModflowTimeList;
    FStrDiversionSegmentNumberData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW STR boundaries
  // for a series of time intervals.
  TStrCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateBedTopData(Sender: TObject);
    procedure InvalidateBedBottomData(Sender: TObject);
    procedure InvalidateFlowData(Sender: TObject);
    procedure InvalidateWidthData(Sender: TObject);
    procedure InvalidateSlopeData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
    procedure FixItems;
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TStrStorage.StrArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
      override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  // Each @name stores a @link(TStrCollection).
  // @classname is stored by @link(TModflowParameters).
  TStrParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TStr_Cell = class(TValueCell)
  private
    Values: TStrRecord;
    StressPeriod: integer;
    function GetStage: double;
    function GetConductance: double;
    function GetBedTop: double;
    function GetBedBottom: double;
    function GetFlow: double;
    function GetConductanceAnnotation: string;
    function GetStageAnnotation: string;
    function GetBedTopAnnotation: string;
    function GetBedBottomAnnotation: string;
    function GetFlowAnnotation: string;
    function GetRoughness: double;
    function GetRoughnessAnnotation: string;
    function GetSlope: double;
    function GetSlopeAnnotation: string;
    function GetWidth: double;
    function GetWidthAnnotation: string;
    procedure SetFlow(const Value: double);
    procedure SetFlowAnnotation(const Value: string);
    function GetBedBottomPest: string;
    function GetBedBottomPestSeriesMethod: TPestParamMethod;
    function GetBedBottomPestSeriesName: string;
    function GetBedTopPest: string;
    function GetBedTopPestSeriesMethod: TPestParamMethod;
    function GetBedTopPestSeriesName: string;
    function GetConductancePest: string;
    function GetConductancePestSeriesMethod: TPestParamMethod;
    function GetConductancePestSeriesName: string;
    function GetFlowPest: string;
    function GetFlowPestSeriesMethod: TPestParamMethod;
    function GetFlowPestSeriesName: string;
    function GetRoughnessPest: string;
    function GetRoughnessPestSeriesMethod: TPestParamMethod;
    function GetRoughnessPestSeriesName: string;
    function GetSlopePest: string;
    function GetSlopePestSeriesMethod: TPestParamMethod;
    function GetSlopePestSeriesName: string;
    function GetStagePest: string;
    function GetStagePestSeriesMethod: TPestParamMethod;
    function GetStagePestSeriesName: string;
    function GetWidthPest: string;
    function GetWidthPestSeriesMethod: TPestParamMethod;
    function GetWidthPestSeriesName: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property Conductance: double read GetConductance;
    property Stage: double read GetStage;
    property BedTop: double read GetBedTop;
    property BedBottom: double read GetBedBottom;
    property Flow: double read GetFlow write SetFlow;
    property Width: double read GetWidth;
    property Slope: double read GetSlope;
    property Roughness: double read GetRoughness;
    property SegmentNumber: Integer read Values.SegmentNumber
      write Values.SegmentNumber;
    property ReachNumber: Integer read Values.ReachNumber
      write Values.ReachNumber;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property StageAnnotation: string read GetStageAnnotation;
    property BedTopAnnotation: string read GetBedTopAnnotation;
    property BedBottomAnnotation: string read GetBedBottomAnnotation;
    property FlowAnnotation: string read GetFlowAnnotation write SetFlowAnnotation;
    property WidthAnnotation: string read GetWidthAnnotation;
    property SlopeAnnotation: string read GetSlopeAnnotation;
    property RoughnessAnnotation: string read GetRoughnessAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // PEST properties

    property ConductancePest: string read GetConductancePest;
    property StagePest: string read GetStagePest;
    property BedTopPest: string read GetBedTopPest;
    property BedBottomPest: string read GetBedBottomPest;
    property FlowPest: string read GetFlowPest;
    property WidthPest: string read GetWidthPest;
    property SlopePest: string read GetSlopePest;
    property RoughnessPest: string read GetRoughnessPest;

    property ConductancePestSeriesName: string read GetConductancePestSeriesName;
    property StagePestSeriesName: string read GetStagePestSeriesName;
    property BedTopPestSeriesName: string read GetBedTopPestSeriesName;
    property BedBottomPestSeriesName: string read GetBedBottomPestSeriesName;
    property FlowPestSeriesName: string read GetFlowPestSeriesName;
    property WidthPestSeriesName: string read GetWidthPestSeriesName;
    property SlopePestSeriesName: string read GetSlopePestSeriesName;
    property RoughnessPestSeriesName: string read GetRoughnessPestSeriesName;

    property ConductancePestSeriesMethod: TPestParamMethod read GetConductancePestSeriesMethod;
    property StagePestSeriesMethod: TPestParamMethod read GetStagePestSeriesMethod;
    property BedTopPestSeriesMethod: TPestParamMethod read GetBedTopPestSeriesMethod;
    property BedBottomPestSeriesMethod: TPestParamMethod read GetBedBottomPestSeriesMethod;
    property FlowPestSeriesMethod: TPestParamMethod read GetFlowPestSeriesMethod;
    property WidthPestSeriesMethod: TPestParamMethod read GetWidthPestSeriesMethod;
    property SlopePestSeriesMethod: TPestParamMethod read GetSlopePestSeriesMethod;
    property RoughnessPestSeriesMethod: TPestParamMethod read GetRoughnessPestSeriesMethod;

  end;

  // @name represents the MODFLOW STR boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TStrItem.Conductance
  // TStrItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TStrCollection)
  TStrBoundary = class(TSpecificModflowBoundary)
  private
    FSegmentNumber: Integer;
    FConductancePestMethod: TPestParamMethod;
    FRoughnessPestMethod: TPestParamMethod;
    FSlopePestMethod: TPestParamMethod;
    FWidthPestMethod: TPestParamMethod;
    FFlowPestMethod: TPestParamMethod;
    FBedBottomPestMethod: TPestParamMethod;
    FStagePestMethod: TPestParamMethod;
    FBedTopPestMethod: TPestParamMethod;
    FStreamConductanceFormula: TFormulaObject;
    FStreamBedTopFormula: TFormulaObject;
    FStreamBedBottomFormula: TFormulaObject;
    FStreamFlowFormula: TFormulaObject;
    FStreamStageFormula: TFormulaObject;
    FStreamWidthFormula: TFormulaObject;
    FStreamSlopeFormula: TFormulaObject;
    FStreamRoughnessFormula: TFormulaObject;
    FBedBottomPestObserver: TObserver;
    FBedTopPestObserver: TObserver;
    FConductancePestObserver: TObserver;
    FRoughnessPestObserver: TObserver;
    FStagePestObserver: TObserver;
    FWidthPestObserver: TObserver;
    FSlopePestObserver: TObserver;
    FFlowPestObserver: TObserver;
    FUsedObserver: TObserver;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
    procedure SetSegmentNumber(const Value: Integer);
    function GetBedBottomPest: string;
    function GetBedBottomPestObserver: TObserver;
    function GetBedTopPest: string;
    function GetBedTopPestObserver: TObserver;
    function GetConductancePest: string;
    function GetConductancePestObserver: TObserver;
    function GetFlowPest: string;
    function GetFlowPestObserver: TObserver;
    function GetRoughnessPest: string;
    function GetRoughnessPestObserver: TObserver;
    function GetSlopePest: string;
    function GetSlopePestObserver: TObserver;
    function GetStagePest: string;
    function GetStagePestObserver: TObserver;
    function GetWidthPest: string;
    function GetWidthPestObserver: TObserver;
    procedure SetBedBottomPest(const Value: string);
    procedure SetBedTopPest(const Value: string);
    procedure SetConductancePest(const Value: string);
    procedure SetFlowPest(const Value: string);
    procedure SetRoughnessPest(const Value: string);
    procedure SetSlopePest(const Value: string);
    procedure SetStagePest(const Value: string);
    procedure SetWidthPest(const Value: string);
    procedure SetBedBottomPestMethod(const Value: TPestParamMethod);
    procedure SetBedTopPestMethod(const Value: TPestParamMethod);
    procedure SetConductancePestMethod(const Value: TPestParamMethod);
    procedure SetFlowPestMethod(const Value: TPestParamMethod);
    procedure SetRoughnessPestMethod(const Value: TPestParamMethod);
    procedure SetSlopePestMethod(const Value: TPestParamMethod);
    procedure SetStagePestMethod(const Value: TPestParamMethod);
    procedure SetWidthPestMethod(const Value: TPestParamMethod);
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateBedTopData(Sender: TObject);
    procedure InvalidateBedBottomData(Sender: TObject);
    procedure InvalidateFlowData(Sender: TObject);
    procedure InvalidateWidthData(Sender: TObject);
    procedure InvalidateSlopeData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TStr_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod;
      override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property ConductancePestObserver: TObserver read GetConductancePestObserver;
    property StagePestObserver: TObserver read GetStagePestObserver;
    property BedTopPestObserver: TObserver read GetBedTopPestObserver;
    property BedBottomPestObserver: TObserver read GetBedBottomPestObserver;
    property FlowPestObserver: TObserver read GetFlowPestObserver;
    property WidthPestObserver: TObserver read GetWidthPestObserver;
    property SlopePestObserver: TObserver read GetSlopePestObserver;
    property RoughnessPestObserver: TObserver read GetRoughnessPestObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    // @name insures that there are no time gaps in @link(TStrCollection)
    // by filling in any time gaps with inactive TStrItems.
    procedure FixItems;
    // @name copies @link(SegmentNumber) from the Source
    // @classname to this @classname and then calls inherited Assign.
    procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TStrStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW STR parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TStrStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property SegmentNumber: Integer read FSegmentNumber write SetSegmentNumber;
    property ConductancePest: string read GetConductancePest
      write SetConductancePest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property StagePest: string read GetStagePest write SetStagePest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property BedTopPest: string read GetBedTopPest write SetBedTopPest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property BedBottomPest: string read GetBedBottomPest write SetBedBottomPest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property FlowPest: string read GetFlowPest write SetFlowPest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property WidthPest: string read GetWidthPest write SetWidthPest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property SlopePest: string read GetSlopePest write SetSlopePest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property RoughnessPest: string read GetRoughnessPest write SetRoughnessPest
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property ConductancePestMethod: TPestParamMethod read FConductancePestMethod
      write SetConductancePestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property StagePestMethod: TPestParamMethod read FStagePestMethod
      write SetStagePestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property BedTopPestMethod: TPestParamMethod read FBedTopPestMethod
      write SetBedTopPestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property BedBottomPestMethod: TPestParamMethod read FBedBottomPestMethod
      write SetBedBottomPestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property FlowPestMethod: TPestParamMethod read FFlowPestMethod
      write SetFlowPestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property WidthPestMethod: TPestParamMethod read FWidthPestMethod
      write SetWidthPestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property SlopePestMethod: TPestParamMethod read FSlopePestMethod
      write SetSlopePestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property RoughnessPestMethod: TPestParamMethod read FRoughnessPestMethod
      write SetRoughnessPestMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;

  end;

const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, ModflowPackageSelectionUnit;

resourcestring
  StrHead = 'Head';
  StrStreambedTop = 'Streambed Top';
  StrStreambedBottom = 'Streambed Bottom';
  StrFlow = 'Flow';
  StrWidth = 'Width';
  StrSlope = 'Slope';
  StrRoughness = 'Roughness';
  StrSegmentNumber = 'Segment Number';
  StrOutflowSegmentNumber = 'Outflow Segment Number';
  StrDiversionSegmentNu = 'Diversion Segment Number';
  StrReachNumber = 'Reach Number';

{ TStrRecord }

procedure TStrRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, Stage);
  WriteCompReal(Comp, BedTop);
  WriteCompReal(Comp, BedBottom);
  WriteCompReal(Comp, Flow);
  WriteCompReal(Comp, Width);
  WriteCompReal(Comp, Slope);
  WriteCompReal(Comp, Roughness);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, SegmentNumber);
  WriteCompInt(Comp, ReachNumber);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BedTopAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BedBottomAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FlowAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(WidthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SlopeAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(StagePest));
  WriteCompInt(Comp, Strings.IndexOf(BedTopPest));
  WriteCompInt(Comp, Strings.IndexOf(BedBottomPest));
  WriteCompInt(Comp, Strings.IndexOf(FlowPest));
  WriteCompInt(Comp, Strings.IndexOf(WidthPest));
  WriteCompInt(Comp, Strings.IndexOf(SlopePest));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPest));

  WriteCompInt(Comp, Strings.IndexOf(ConductancePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(StagePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(BedTopPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(BedBottomPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(FlowPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(WidthPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(SlopePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPestSeriesName));

  WriteCompInt(Comp, Ord(ConductancePestSeriesMethod));
  WriteCompInt(Comp, Ord(StagePestSeriesMethod));
  WriteCompInt(Comp, Ord(BedTopPestSeriesMethod));
  WriteCompInt(Comp, Ord(BedBottomPestSeriesMethod));
  WriteCompInt(Comp, Ord(FlowPestSeriesMethod));
  WriteCompInt(Comp, Ord(WidthPestSeriesMethod));
  WriteCompInt(Comp, Ord(SlopePestSeriesMethod));
  WriteCompInt(Comp, Ord(RoughnessPestSeriesMethod));
end;

procedure TStrRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(StageAnnotation);
  Strings.Add(BedTopAnnotation);
  Strings.Add(BedBottomAnnotation);
  Strings.Add(FlowAnnotation);
  Strings.Add(WidthAnnotation);
  Strings.Add(SlopeAnnotation);
  Strings.Add(RoughnessAnnotation);

  Strings.Add(ConductancePest);
  Strings.Add(StagePest);
  Strings.Add(BedTopPest);
  Strings.Add(BedBottomPest);
  Strings.Add(FlowPest);
  Strings.Add(WidthPest);
  Strings.Add(SlopePest);
  Strings.Add(RoughnessPest);

  Strings.Add(ConductancePestSeriesName);
  Strings.Add(StagePestSeriesName);
  Strings.Add(BedTopPestSeriesName);
  Strings.Add(BedBottomPestSeriesName);
  Strings.Add(FlowPestSeriesName);
  Strings.Add(WidthPestSeriesName);
  Strings.Add(SlopePestSeriesName);
  Strings.Add(RoughnessPestSeriesName);

end;

procedure TStrRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  Stage := ReadCompReal(Decomp);
  BedTop := ReadCompReal(Decomp);
  BedBottom := ReadCompReal(Decomp);
  Flow := ReadCompReal(Decomp);
  Width := ReadCompReal(Decomp);
  Slope := ReadCompReal(Decomp);
  Roughness := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  SegmentNumber := ReadCompInt(Decomp);
  ReachNumber := ReadCompInt(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  StageAnnotation := Annotations[ReadCompInt(Decomp)];
  BedTopAnnotation := Annotations[ReadCompInt(Decomp)];
  BedBottomAnnotation := Annotations[ReadCompInt(Decomp)];
  FlowAnnotation := Annotations[ReadCompInt(Decomp)];
  WidthAnnotation := Annotations[ReadCompInt(Decomp)];
  SlopeAnnotation := Annotations[ReadCompInt(Decomp)];
  RoughnessAnnotation := Annotations[ReadCompInt(Decomp)];

  ConductancePest := Annotations[ReadCompInt(Decomp)];
  StagePest := Annotations[ReadCompInt(Decomp)];
  BedTopPest := Annotations[ReadCompInt(Decomp)];
  BedBottomPest := Annotations[ReadCompInt(Decomp)];
  FlowPest := Annotations[ReadCompInt(Decomp)];
  WidthPest := Annotations[ReadCompInt(Decomp)];
  SlopePest := Annotations[ReadCompInt(Decomp)];
  RoughnessPest := Annotations[ReadCompInt(Decomp)];

  ConductancePestSeriesName := Annotations[ReadCompInt(Decomp)];
  StagePestSeriesName := Annotations[ReadCompInt(Decomp)];
  BedTopPestSeriesName := Annotations[ReadCompInt(Decomp)];
  BedBottomPestSeriesName := Annotations[ReadCompInt(Decomp)];
  FlowPestSeriesName := Annotations[ReadCompInt(Decomp)];
  WidthPestSeriesName := Annotations[ReadCompInt(Decomp)];
  SlopePestSeriesName := Annotations[ReadCompInt(Decomp)];
  RoughnessPestSeriesName := Annotations[ReadCompInt(Decomp)];

  ConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StagePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  BedTopPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  BedBottomPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  FlowPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  WidthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  SlopePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RoughnessPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TStrStorage }

procedure TStrStorage.Clear;
begin
  SetLength(FStrArray, 0);
  FCleared := True;
end;

function TStrStorage.GetStrArray: TStrArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FStrArray;
end;

procedure TStrStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FStrArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FStrArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TStrStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FStrArray);
    for Index := 0 to Count - 1 do
    begin
      FStrArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FStrArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TStrItem }

procedure TStrItem.Assign(Source: TPersistent);
var
  Str: TStrItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TStrItem then
  begin
    Str := TStrItem(Source);
    Stage := Str.Stage;
    Conductance := Str.Conductance;
    BedTop := Str.BedTop;
    BedBottom := Str.BedBottom;
    Flow := Str.Flow;
    Width := Str.Width;
    Slope := Str.Slope;
    Roughness := Str.Roughness;
    OutflowSegment := Str.OutflowSegment;
    DiversionSegment := Str.DiversionSegment;
  end;
  inherited;
end;

procedure TStrItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TStrCollection;
  StageObserver: TObserver;
  ConductanceObserver: TObserver;
  BedTopObserver: TObserver;
  BedBottomObserver: TObserver;
  FlowObserver: TObserver;
  WidthObserver: TObserver;
  SlopeObserver: TObserver;
  RoughnessObserver: TObserver;
begin
  ParentCollection := Collection as TStrCollection;
  StageObserver := FObserverList[StreamStagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStageData;
  ConductanceObserver := FObserverList[StreamConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  BedTopObserver := FObserverList[StreamBedTopPosition];
  BedTopObserver.OnUpToDateSet := ParentCollection.InvalidateBedTopData;
  BedBottomObserver := FObserverList[StreamBedBottomPosition];
  BedBottomObserver.OnUpToDateSet := ParentCollection.InvalidateBedBottomData;
  FlowObserver := FObserverList[StreamFlowPosition];
  FlowObserver.OnUpToDateSet := ParentCollection.InvalidateFlowData;
  WidthObserver := FObserverList[StreamWidthPosition];
  WidthObserver.OnUpToDateSet := ParentCollection.InvalidateWidthData;
  SlopeObserver := FObserverList[StreamSlopePosition];
  SlopeObserver.OnUpToDateSet := ParentCollection.InvalidateSlopeData;
  RoughnessObserver := FObserverList[StreamRoughnessPosition];
  RoughnessObserver.OnUpToDateSet := ParentCollection.InvalidateRoughnessData;
end;

function TStrItem.BoundaryFormulaCount: integer;
//var
//  StrPackage: TStrPackageSelection;
begin
//  StrPackage := (CurrentModel as TCustomModel).ModflowPackages.StrPackage;
//  if StrPackage.CalculateStage then
//  begin
    result := 8;
//  end
//  else
//  begin
//    result := 5;
//  end;
end;

procedure TStrItem.CreateFormulaObjects;
begin
  FStage := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FBedTop := CreateFormulaObject(dso3D);
  FBedBottom := CreateFormulaObject(dso3D);
  FFlow := CreateFormulaObject(dso3D);
  FWidth := CreateFormulaObject(dso3D);
  FSlope := CreateFormulaObject(dso3D);
  FRoughness := CreateFormulaObject(dso3D);
end;

destructor TStrItem.Destroy;
begin
  Stage := '0';
  Conductance := '0';
  BedTop := '0';
  BedBottom := '0';
  Flow := '0';
  Width := '0';
  Slope := '0';
  Roughness := '0';
  inherited;
end;

function TStrItem.GetBedBottom: string;
begin
  Result := FBedBottom.Formula;
  ResetItemObserver(StreamBedBottomPosition);
end;

function TStrItem.GetBedTop: string;
begin
  Result := FBedTop.Formula;
  ResetItemObserver(StreamBedTopPosition);
end;

function TStrItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StreamConductancePosition: result := Conductance;
    StreamBedTopPosition: result := BedTop;
    StreamBedBottomPosition: result := BedBottom;
    StreamFlowPosition: result := Flow;
    StreamStagePosition: result := Stage;
    StreamWidthPosition: result := Width;
    StreamSlopePosition: result := Slope;
    StreamRoughnessPosition: result := Roughness;
    else Assert(False);
  end;
end;

function TStrItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(StreamConductancePosition);
end;

function TStrItem.GetFlow: string;
begin
  Result := FFlow.Formula;
  ResetItemObserver(StreamFlowPosition);
end;

procedure TStrItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[StreamConductancePosition]);
  end;
  if Sender = FStage then
  begin
    List.Add(FObserverList[StreamStagePosition]);
  end;
  if Sender = FBedTop then
  begin
    List.Add(FObserverList[StreamBedTopPosition]);
  end;
  if Sender = FBedBottom then
  begin
    List.Add(FObserverList[StreamBedBottomPosition]);
  end;
  if Sender = FFlow then
  begin
    List.Add(FObserverList[StreamFlowPosition]);
  end;
  if Sender = FWidth then
  begin
    List.Add(FObserverList[StreamWidthPosition]);
  end;
  if Sender = FSlope then
  begin
    List.Add(FObserverList[StreamSlopePosition]);
  end;
  if Sender = FRoughness then
  begin
    List.Add(FObserverList[StreamRoughnessPosition]);
  end;

end;

function TStrItem.GetRoughness: string;
begin
  Result := FRoughness.Formula;
  ResetItemObserver(StreamRoughnessPosition);
end;

function TStrItem.GetSegmentNumber: Integer;
var
  StrCollection: TStrCollection;
  StrBoundary: TStrBoundary;
begin
  StrCollection := Collection as TStrCollection;
  StrBoundary := StrCollection.BoundaryGroup as TStrBoundary;
  result := StrBoundary.SegmentNumber;
end;

function TStrItem.GetSlope: string;
begin
  Result := FSlope.Formula;
  ResetItemObserver(StreamSlopePosition);
end;

function TStrItem.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(StreamStagePosition);
end;

function TStrItem.GetWidth: string;
begin
  Result := FWidth.Formula;
  ResetItemObserver(StreamWidthPosition);
end;

procedure TStrItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfStrConductance(self);
    PhastModel.InvalidateMfStrStage(self);
    PhastModel.InvalidateMfStrBedTop(self);
    PhastModel.InvalidateMfStrBedBottom(self);
    PhastModel.InvalidateMfStrFlow(self);
    PhastModel.InvalidateMfStrWidth(self);
    PhastModel.InvalidateMfStrSlope(self);
    PhastModel.InvalidateMfStrRoughness(self);
    PhastModel.InvalidateMfStrSegmentNumber(self);
    PhastModel.InvalidateMfStrOutflowSegmentNumber(self);
    PhastModel.InvalidateMfStrDiversionSegmentNumber(self);
    PhastModel.InvalidateMfStrReachNumber(self);
  end;
end;

function TStrItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TStrItem;
begin
  result := (AnotherItem is TStrItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TStrItem(AnotherItem);
    result := (Item.Stage = Stage)
      and (Item.Conductance = Conductance)
      and (Item.BedTop = BedTop)
      and (Item.BedBottom = BedBottom)
      and (Item.Flow = Flow)
      and (Item.Width = Width)
      and (Item.Slope = Slope)
      and (Item.Roughness = Roughness)
      and (Item.OutflowSegment = OutflowSegment)
      and (Item.DiversionSegment = DiversionSegment);
  end;
end;

procedure TStrItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBedTop,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBedBottom,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSlope,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

end;

procedure TStrItem.SetBedBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedBottomPosition, FBedBottom);
end;

procedure TStrItem.SetBedTop(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedTopPosition, FBedTop);
end;

procedure TStrItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    StreamConductancePosition:
      Conductance := Value;
    StreamBedTopPosition:
      BedTop := Value;
    StreamBedBottomPosition:
      BedBottom := Value;
    StreamFlowPosition:
      Flow := Value;
    StreamStagePosition:
      Stage := Value;
    StreamWidthPosition:
      Width := Value;
    StreamSlopePosition:
      Slope := Value;
    StreamRoughnessPosition:
      Roughness := Value;
    else Assert(False);
  end;
end;

procedure TStrItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamConductancePosition, FConductance);
end;

procedure TStrItem.SetDiversionSegment(const Value: integer);
var
  Model: TBaseModel;
begin
  if FDiversionSegment <> Value then
  begin
    InvalidateModel;
    FDiversionSegment := Value;
    Model := (Collection as TStrCollection).Model;
    if Model <> nil then
    begin
      (Model as TCustomModel).ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
    end;
  end;
end;

procedure TStrItem.SetFlow(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamFlowPosition, FFlow);
end;

procedure TStrItem.SetOutflowSegment(const Value: integer);
var
  Model: TBaseModel;
begin
  if FOutflowSegment <> Value then
  begin
    InvalidateModel;
    FOutflowSegment := Value;
    Model := (Collection as TStrCollection).Model;
    if Model <> nil then
    begin
      (Model as TCustomModel).ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
    end;
  end;
end;

procedure TStrItem.SetRoughness(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamRoughnessPosition, FRoughness);
end;

procedure TStrItem.SetSlope(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamSlopePosition, FSlope);
end;

procedure TStrItem.SetStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamStagePosition, FStage);
end;

procedure TStrItem.SetWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamWidthPosition, FWidth);
end;

{ TStrTimeListLink }

procedure TStrTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FStageData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStageData.NonParamDescription := StrHead;
  FStageData.ParamDescription := ' ' + LowerCase(StrHead);

  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;

  FBedTopData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBedTopData.NonParamDescription := StrStreambedTop;
  FBedTopData.ParamDescription := ' ' + LowerCase(StrStreambedTop);

  FBedBottomData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBedBottomData.NonParamDescription := StrStreambedBottom;
  FBedBottomData.ParamDescription := ' ' + LowerCase(StrStreambedBottom);

  FFlowData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFlowData.NonParamDescription := StrFlow;
  FFlowData.ParamDescription := ' ' + LowerCase(StrFlow);

  FWidthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWidthData.NonParamDescription := StrWidth;
  FWidthData.ParamDescription := ' ' + LowerCase(StrWidth);

  FSlopeData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSlopeData.NonParamDescription := StrSlope;
  FSlopeData.ParamDescription := ' ' + LowerCase(StrSlope);

  FRoughnessData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRoughnessData.NonParamDescription := StrRoughness;
  FRoughnessData.ParamDescription := ' ' + LowerCase(StrRoughness);

  FStrSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrSegmentNumberData.NonParamDescription := StrSegmentNumber;
  FStrSegmentNumberData.ParamDescription := ' ' + LowerCase(StrSegmentNumber);

  FStrOutflowSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrOutflowSegmentNumberData.NonParamDescription := StrOutflowSegmentNumber;
  FStrOutflowSegmentNumberData.ParamDescription := ' ' + LowerCase(StrOutflowSegmentNumber);

  FStrDiversionSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrDiversionSegmentNumberData.NonParamDescription := StrDiversionSegmentNu;
  FStrDiversionSegmentNumberData.ParamDescription := ' ' + LowerCase(StrDiversionSegmentNu);

  FStrReachNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrReachNumberData.NonParamDescription := StrReachNumber;
  FStrReachNumberData.ParamDescription := ' ' + LowerCase(StrReachNumber);

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FStageData.OnInvalidate := LocalModel.InvalidateMfStrStage;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfStrConductance;
    FBedTopData.OnInvalidate := LocalModel.InvalidateMfStrBedTop;
    FBedBottomData.OnInvalidate := LocalModel.InvalidateMfStrBedBottom;
    FFlowData.OnInvalidate := LocalModel.InvalidateMfStrFlow;
    FWidthData.OnInvalidate := LocalModel.InvalidateMfStrWidth;
    FSlopeData.OnInvalidate := LocalModel.InvalidateMfStrSlope;
    FRoughnessData.OnInvalidate := LocalModel.InvalidateMfStrRoughness;

    FStrSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrSegmentNumber;
    FStrOutflowSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrOutflowSegmentNumber;
    FStrDiversionSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrDiversionSegmentNumber;
    FStrReachNumberData.OnInvalidate := LocalModel.InvalidateMfStrReachNumber;
  end;

  AddTimeList(FConductanceData);
  AddTimeList(FBedTopData);
  AddTimeList(FBedBottomData);
  AddTimeList(FFlowData);
  AddTimeList(FStageData);
  AddTimeList(FWidthData);
  AddTimeList(FSlopeData);
  AddTimeList(FRoughnessData);
  AddTimeList(FStrSegmentNumberData);
  AddTimeList(FStrOutflowSegmentNumberData);
  AddTimeList(FStrDiversionSegmentNumberData);
  AddTimeList(FStrReachNumberData);
end;

destructor TStrTimeListLink.Destroy;
begin
  FStrReachNumberData.Free;
  FStrDiversionSegmentNumberData.Free;
  FStrOutflowSegmentNumberData.Free;
  FStrSegmentNumberData.Free;
  FRoughnessData.Free;
  FSlopeData.Free;
  FWidthData.Free;
  FStageData.Free;
  FConductanceData.Free;
  FBedTopData.Free;
  FBedBottomData.Free;
  FFlowData.Free;
  inherited;
end;

{ TStrCollection }

procedure TStrCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TStrStorage.Create(AModel));
end;

function TStrCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TStrBoundary;
  ScreenObject: TScreenObject;
  Item: TStrItem;
begin
  Item := Items[ItemIndex] as TStrItem;
  if FormulaIndex = StreamConductancePosition then
  begin
    Boundary := BoundaryGroup as TStrBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.Conductance;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.Conductance
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.Conductance
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    result := Item.BoundaryFormula[FormulaIndex];
  end;
end;

procedure TStrCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
var
  StrStorage: TStrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [StreamStagePosition, StreamConductancePosition,
    StreamBedTopPosition, StreamBedBottomPosition, StreamFlowPosition,
    StreamWidthPosition, StreamSlopePosition, StreamRoughnessPosition]);
  Assert(Expression <> nil);

  StrStorage := BoundaryStorage as TStrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with StrStorage.StrArray[Index] do
    begin
      case BoundaryFunctionIndex of
        StreamConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ConductancePest := PestName;
            ConductancePestSeriesName := PestSeriesName;
            ConductancePestSeriesMethod := PestSeriesMethod;
          end;
        StreamBedTopPosition:
          begin
            BedTop := Expression.DoubleResult;
            BedTopAnnotation := ACell.Annotation;
            BedTopPest := PestName;
            BedTopPestSeriesName := PestSeriesName;
            BedTopPestSeriesMethod := PestSeriesMethod;
          end;
        StreamBedBottomPosition:
          begin
            BedBottom := Expression.DoubleResult;
            BedBottomAnnotation := ACell.Annotation;
            BedBottomPest := PestName;
            BedBottomPestSeriesName := PestSeriesName;
            BedBottomPestSeriesMethod := PestSeriesMethod;
          end;
        StreamFlowPosition:
          begin
            Flow := Expression.DoubleResult;
            FlowAnnotation := ACell.Annotation;
            FlowPest := PestName;
            FlowPestSeriesName := PestSeriesName;
            FlowPestSeriesMethod := PestSeriesMethod;
          end;
        StreamStagePosition:
          begin
            Stage := Expression.DoubleResult;
            StageAnnotation := ACell.Annotation;
            StagePest := PestName;
            StagePestSeriesName := PestSeriesName;
            StagePestSeriesMethod := PestSeriesMethod;
          end;
        StreamWidthPosition:
          begin
            Width := Expression.DoubleResult;
            WidthAnnotation := ACell.Annotation;
            WidthPest := PestName;
            WidthPestSeriesName := PestSeriesName;
            WidthPestSeriesMethod := PestSeriesMethod;
          end;
        StreamSlopePosition:
          begin
            Slope := Expression.DoubleResult;
            SlopeAnnotation := ACell.Annotation;
            SlopePest := PestName;
            SlopePestSeriesName := PestSeriesName;
            SlopePestSeriesMethod := PestSeriesMethod;
          end;
        StreamRoughnessPosition:
          begin
            Roughness := Expression.DoubleResult;
            RoughnessAnnotation := ACell.Annotation;
            RoughnessPest := PestName;
            RoughnessPestSeriesName := PestSeriesName;
            RoughnessPestSeriesMethod := PestSeriesMethod;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TStrCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  StrStorage: TStrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  StrStorage := BoundaryStorage as TStrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with StrStorage.StrArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TStrCollection.Create(Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  SectionDuplicatesAllowed := True;
end;

procedure TStrCollection.FixItems;
var
  ItemIndex: Integer;
  Item1: TCustomModflowBoundaryItem;
  Item2: TCustomModflowBoundaryItem;
  StressPeriods: TModflowStressPeriods;
  StartTime: Double;
  EndTime: Double;
  Item: TStrItem;
  NewItem: TStrItem;
  StrItem2: TStrItem;
  StrItem1: TStrItem;
begin
  if Count > 0 then
  begin
    for ItemIndex := 0 to Count - 2 do
    begin
      Item1 := Items[ItemIndex] as TCustomModflowBoundaryItem;
      Item2 := Items[ItemIndex+1] as TCustomModflowBoundaryItem;
      if Item2.StartTime < Item1.EndTime then
      begin
        Item2.StartTime := Item1.EndTime;
      end;
    end;
    StressPeriods := (Model as TCustomModel).ModflowStressPeriods;
    StartTime := StressPeriods.First.StartTime;
    EndTime := StressPeriods.Last.EndTime;
    Item := Items[0] as TStrItem;
    if Item.StartTime > StartTime then
    begin
      NewItem := Insert(0) as TStrItem;
      NewItem.Assign(Item);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := StartTime;
      NewItem.EndTime := Item.StartTime;
    end;
    Item := Items[Count-1] as TStrItem;
    if Item.EndTime < EndTime then
    begin
      NewItem := Add as TStrItem;
      NewItem.Assign(Item);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := Item.EndTime;
      NewItem.EndTime := EndTime;
    end;
    for ItemIndex := Count - 2 downto 0 do
    begin
      StrItem2 := Items[ItemIndex+1] as TStrItem;
      StrItem1 := Items[ItemIndex] as TStrItem;
      if StrItem1.EndTime < StrItem2.StartTime then
      begin
      NewItem := Insert(ItemIndex+1) as TStrItem;
      NewItem.Assign(StrItem1);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := StartTime;
      NewItem.EndTime := Item.StartTime;
      end;
    end;
  end;
end;

function TStrCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TStrTimeListLink;
end;

procedure TStrCollection.InvalidateBedBottomData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FBedBottomData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FBedBottomData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateBedTopData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FBedTopData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FBedTopData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FConductanceData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateFlowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FFlowData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FFlowData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FRoughnessData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FRoughnessData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateSlopeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FSlopeData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FSlopeData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FStageData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FStageData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FWidthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FWidthData.Invalidate;
    end;
  end;
end;

class function TStrCollection.ItemClass: TBoundaryItemClass;
begin
  result := TStrItem;
end;

procedure TStrCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TStrStorage).FStrArray, BoundaryCount);
  inherited;
end;

procedure TStrCollection.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  Boundary: TStrBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TStrBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

{ TStrParamItem }

class function TStrParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TStrCollection;
end;

{ TStr_Cell }

procedure TStr_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TStr_Cell.GetBedBottom: double;
begin
  result := Values.BedBottom;
end;

function TStr_Cell.GetBedBottomAnnotation: string;
begin
  result := Values.BedBottomAnnotation;
end;

function TStr_Cell.GetBedBottomPest: string;
begin
  result := Values.BedBottomPest;
end;

function TStr_Cell.GetBedBottomPestSeriesMethod: TPestParamMethod;
begin
  result := Values.BedBottomPestSeriesMethod;
end;

function TStr_Cell.GetBedBottomPestSeriesName: string;
begin
  result := Values.BedBottomPestSeriesName;
end;

function TStr_Cell.GetBedTop: double;
begin
  result := Values.BedTop;
end;

function TStr_Cell.GetBedTopAnnotation: string;
begin
  result := Values.BedTopAnnotation;
end;

function TStr_Cell.GetBedTopPest: string;
begin
  result := Values.BedTopPest;
end;

function TStr_Cell.GetBedTopPestSeriesMethod: TPestParamMethod;
begin
  result := Values.BedTopPestSeriesMethod;
end;

function TStr_Cell.GetBedTopPestSeriesName: string;
begin
  result := Values.BedTopPestSeriesName;
end;

function TStr_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TStr_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TStr_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TStr_Cell.GetConductancePest: string;
begin
  result := Values.ConductancePest;
end;

function TStr_Cell.GetConductancePestSeriesMethod: TPestParamMethod;
begin
  result := Values.ConductancePestSeriesMethod;
end;

function TStr_Cell.GetConductancePestSeriesName: string;
begin
  result := Values.ConductancePestSeriesName;
end;

function TStr_Cell.GetFlow: double;
begin
  result := Values.Flow;
end;

function TStr_Cell.GetFlowAnnotation: string;
begin
  result := Values.FlowAnnotation;
end;

function TStr_Cell.GetFlowPest: string;
begin
  result := Values.FlowPest;
end;

function TStr_Cell.GetFlowPestSeriesMethod: TPestParamMethod;
begin
  result := Values.FlowPestSeriesMethod;
end;

function TStr_Cell.GetFlowPestSeriesName: string;
begin
  result := Values.FlowPestSeriesName;
end;

function TStr_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TStr_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TStr_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;


function TStr_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    StreamConductancePosition: result := ConductanceAnnotation;
    StreamBedTopPosition: result := BedTopAnnotation;
    StreamBedBottomPosition: result := BedBottomAnnotation;
    StreamFlowPosition: result := FlowAnnotation;
    StreamStagePosition: result := StageAnnotation;
    StreamWidthPosition: result := WidthAnnotation;
    StreamSlopePosition: result := SlopeAnnotation;
    StreamRoughnessPosition: result := RoughnessAnnotation;
    else Assert(False);
  end;
end;

function TStr_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    StreamConductancePosition: result := Conductance;
    StreamBedTopPosition: result := BedTop;
    StreamBedBottomPosition: result := BedBottom;
    StreamFlowPosition: result := Flow;
    StreamStagePosition: result := Stage;
    StreamWidthPosition: result := Width;
    StreamSlopePosition: result := Slope;
    StreamRoughnessPosition: result := Roughness;
    else Assert(False);
  end;
end;

function TStr_Cell.GetRoughness: double;
begin
  result := Values.Roughness;
end;

function TStr_Cell.GetRoughnessAnnotation: string;
begin
  result := Values.RoughnessAnnotation;
end;

function TStr_Cell.GetRoughnessPest: string;
begin
  result := Values.RoughnessPest;
end;

function TStr_Cell.GetRoughnessPestSeriesMethod: TPestParamMethod;
begin
  result := Values.RoughnessPestSeriesMethod;
end;

function TStr_Cell.GetRoughnessPestSeriesName: string;
begin
  result := Values.RoughnessPestSeriesName;
end;

function TStr_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TStr_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TStr_Cell.GetSlope: double;
begin
  result := Values.Slope;
end;

function TStr_Cell.GetSlopeAnnotation: string;
begin
  result := Values.SlopeAnnotation;
end;

function TStr_Cell.GetSlopePest: string;
begin
  result := Values.SlopePest;
end;

function TStr_Cell.GetSlopePestSeriesMethod: TPestParamMethod;
begin
  result := Values.SlopePestSeriesMethod;
end;

function TStr_Cell.GetSlopePestSeriesName: string;
begin
  result := Values.SlopePestSeriesName;
end;

function TStr_Cell.GetStage: double;
begin
  result := Values.Stage;
end;

function TStr_Cell.GetStageAnnotation: string;
begin
  result := Values.StageAnnotation;
end;

function TStr_Cell.GetStagePest: string;
begin
  result := Values.StagePest;
end;

function TStr_Cell.GetStagePestSeriesMethod: TPestParamMethod;
begin
  result := Values.StagePestSeriesMethod;
end;

function TStr_Cell.GetStagePestSeriesName: string;
begin
  result := Values.StagePestSeriesName;
end;

function TStr_Cell.GetWidth: double;
begin
  result := Values.Width;
end;

function TStr_Cell.GetWidthAnnotation: string;
begin
  result := Values.WidthAnnotation;
end;

function TStr_Cell.GetWidthPest: string;
begin
  result := Values.WidthPest;
end;

function TStr_Cell.GetWidthPestSeriesMethod: TPestParamMethod;
begin
  result := Values.WidthPestSeriesMethod;
end;

function TStr_Cell.GetWidthPestSeriesName: string;
begin
  result := Values.WidthPestSeriesName;
end;

function TStr_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Str_Cell: TStr_Cell;
begin
  result := AnotherCell is TStr_Cell;
  if result then
  begin
    Str_Cell := TStr_Cell(AnotherCell);
    result :=
      (Conductance = Str_Cell.Conductance)
      and (BedTop = Str_Cell.BedTop)
      and (BedBottom = Str_Cell.BedBottom)
      and (Flow = Str_Cell.Flow)
      and (Stage = Str_Cell.Stage)
      and (Width = Str_Cell.Width)
      and (Slope = Str_Cell.Slope)
      and (Roughness = Str_Cell.Roughness)
      and (SegmentNumber = Str_Cell.SegmentNumber)
      and (ReachNumber = Str_Cell.ReachNumber)
      and (IFace = Str_Cell.IFace)
      ;
  end;
end;

procedure TStr_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TStr_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TStr_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TStr_Cell.SetFlow(const Value: double);
begin
  Values.Flow := Value;
end;

procedure TStr_Cell.SetFlowAnnotation(const Value: string);
begin
  Values.FlowAnnotation := Value;
end;

procedure TStr_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TStr_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TStrBoundary }

procedure TStrBoundary.Assign(Source: TPersistent);
var
  StrBoundary: TStrBoundary;
  Index: Integer;
begin
  if Source is TStrBoundary then
  begin
    StrBoundary := TStrBoundary(Source);
    SegmentNumber := StrBoundary.SegmentNumber;

    ConductancePest := StrBoundary.ConductancePest;
    StagePest := StrBoundary.StagePest;
    BedTopPest := StrBoundary.BedTopPest;
    BedBottomPest := StrBoundary.BedBottomPest;
    FlowPest := StrBoundary.FlowPest;
    WidthPest := StrBoundary.WidthPest;
    SlopePest := StrBoundary.SlopePest;
    RoughnessPest := StrBoundary.RoughnessPest;

    for Index := StreamConductancePosition to StreamRoughnessPosition do
    begin
      PestBoundaryMethod[Index] := StrBoundary.PestBoundaryMethod[Index];
    end;
  end;
  inherited;
end;

procedure TStrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TStr_Cell;
  BoundaryValues: TStrRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TStrStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TStrStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TStr_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.StrArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.StrArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.StrArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.StrArray[BoundaryIndex];
        Cell := TStr_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TStrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TStrCollection;
end;

function TStrBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestStr_';
end;

constructor TStrBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  ConductancePest := '';
  StagePest := '';
  BedTopPest := '';
  BedBottomPest := '';
  FlowPest := '';
  WidthPest := '';
  SlopePest := '';
  RoughnessPest := '';
  FConductancePestMethod := DefaultBoundaryMethod(StreamConductancePosition);
  BedTopPestMethod := DefaultBoundaryMethod(StreamBedTopPosition);
  BedBottomPestMethod := DefaultBoundaryMethod(StreamBedBottomPosition);
  FlowPestMethod := DefaultBoundaryMethod(StreamFlowPosition);
  StagePestMethod := DefaultBoundaryMethod(StreamStagePosition);
  WidthPestMethod := DefaultBoundaryMethod(StreamWidthPosition);
  SlopePestMethod := DefaultBoundaryMethod(StreamSlopePosition);
  RoughnessPestMethod := DefaultBoundaryMethod(StreamRoughnessPosition);
end;

procedure TStrBoundary.CreateFormulaObjects;
begin
  FStreamConductanceFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamBedTopFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamBedBottomFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamFlowFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamStageFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamWidthFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamSlopeFormula := CreateFormulaObjectBlocks(dso3D);
  FStreamRoughnessFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TStrBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(ConductancePestObserver);
    FObserverList.Add(StagePestObserver);
    FObserverList.Add(BedTopPestObserver);
    FObserverList.Add(BedBottomPestObserver);
    FObserverList.Add(FlowPestObserver);
    FObserverList.Add(WidthPestObserver);
    FObserverList.Add(SlopePestObserver);
    FObserverList.Add(RoughnessPestObserver);
  end;
end;

class function TStrBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    StreamConductancePosition:
      begin
        result := ppmMultiply;
      end;
    StreamBedTopPosition:
      begin
        result := ppmAdd;
      end;
    StreamBedBottomPosition:
      begin
        result := ppmAdd;
      end;
    StreamFlowPosition:
      begin
        result := ppmMultiply;
      end;
    StreamStagePosition:
      begin
        result := ppmAdd;
      end;
    StreamWidthPosition:
      begin
        result := ppmAdd;
      end;
    StreamSlopePosition:
      begin
        result := ppmMultiply;
      end;
    StreamRoughnessPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TStrBoundary.Destroy;
begin
  ConductancePest := '';
  StagePest := '';
  BedTopPest := '';
  BedBottomPest := '';
  FlowPest := '';
  WidthPest := '';
  SlopePest := '';
  RoughnessPest := '';

  inherited;
end;

procedure TStrBoundary.FixItems;
var
  ParamIndex: Integer;
  Param: TModflowParamItem;
begin
  (Values as TStrCollection).FixItems;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    (Param.Param as TStrCollection).FixItems;
  end;
end;

function TStrBoundary.GetBedBottomPest: string;
begin
  Result := FStreamBedBottomFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamBedBottomPosition);
  end;
end;

function TStrBoundary.GetBedBottomPestObserver: TObserver;
begin
  if FBedBottomPestObserver = nil then
  begin
    CreateObserver('BedBottomPest_', FBedBottomPestObserver, nil);
    FBedBottomPestObserver.OnUpToDateSet := InvalidateBedBottomData;
  end;
  result := FBedBottomPestObserver;
end;

function TStrBoundary.GetBedTopPest: string;
begin
  Result := FStreamBedTopFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamBedTopPosition);
  end;
end;

function TStrBoundary.GetBedTopPestObserver: TObserver;
begin
  if FBedTopPestObserver = nil then
  begin
    CreateObserver('BedTopPest_', FBedTopPestObserver, nil);
    FBedTopPestObserver.OnUpToDateSet := InvalidateBedTopData;
  end;
  result := FBedTopPestObserver;
end;

procedure TStrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TStrStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ValueCount: Integer;
  Item: TCustomModflowBoundaryItem;
begin
  EvaluateListBoundaries(AModel);
  TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
    ObservationsPresent);
  PriorTime := StartOfFirstStressPeriod;
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    Item := Values[ValueIndex] as TCustomModflowBoundaryItem;
    if ObservationsPresent then
    begin
      if PriorTime <= Item.StartTime then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
    if {(ValueIndex = Values.Count - 1) and} ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;
    PriorTime := StartOfFirstStressPeriod;
    ValueCount := 0;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      Item := Param.Param[ValueIndex] as TCustomModflowBoundaryItem;
      if ObservationsPresent then
      begin
        if PriorTime < Item.StartTime then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
        AssignCells(BoundaryStorage, Times, AModel);
        Inc(ValueCount);
      end;
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

function TStrBoundary.GetConductancePest: string;
begin
  Result := FStreamConductanceFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamConductancePosition);
  end;
end;

function TStrBoundary.GetConductancePestObserver: TObserver;
begin
  if FConductancePestObserver = nil then
  begin
    CreateObserver('ConductancePest_', FConductancePestObserver, nil);
    FConductancePestObserver.OnUpToDateSet := InvalidateConductanceData;
  end;
  result := FConductancePestObserver;
end;

function TStrBoundary.GetFlowPest: string;
begin
  Result := FStreamFlowFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamFlowPosition);
  end;
end;

function TStrBoundary.GetFlowPestObserver: TObserver;
begin
  if FFlowPestObserver = nil then
  begin
    CreateObserver('FlowPest_', FFlowPestObserver, nil);
    FFlowPestObserver.OnUpToDateSet := InvalidateFlowData;
  end;
  result := FFlowPestObserver;
end;

function TStrBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    StreamConductancePosition:
      begin
        result := ConductancePest;
      end;
    StreamBedTopPosition:
      begin
        result := BedTopPest;
      end;
    StreamBedBottomPosition:
      begin
        result := BedBottomPest;
      end;
    StreamFlowPosition:
      begin
        result := FlowPest;
      end;
    StreamStagePosition:
      begin
        result := StagePest;
      end;
    StreamWidthPosition:
      begin
        result := WidthPest;
      end;
    StreamSlopePosition:
      begin
        result := SlopePest;
      end;
    StreamRoughnessPosition:
      begin
        result := RoughnessPest;
      end;
    else
      Assert(False);
  end;
end;

function TStrBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    StreamConductancePosition:
      begin
        result := ConductancePestMethod;
      end;
    StreamBedTopPosition:
      begin
        result := BedTopPestMethod;
      end;
    StreamBedBottomPosition:
      begin
        result := BedBottomPestMethod;
      end;
    StreamFlowPosition:
      begin
        result := FlowPestMethod;
      end;
    StreamStagePosition:
      begin
        result := StagePestMethod;
      end;
    StreamWidthPosition:
      begin
        result := WidthPestMethod;
      end;
    StreamSlopePosition:
      begin
        result := SlopePestMethod;
      end;
    StreamRoughnessPosition:
      begin
        result := RoughnessPestMethod;
      end;
    else
      result := inherited;
      Assert(False);
  end;
end;

procedure TStrBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FStreamConductanceFormula then
  begin
    if StreamConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamConductancePosition]);
    end;
  end;

  if Sender = FStreamBedTopFormula then
  begin
    if StreamBedTopPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamBedTopPosition]);
    end;
  end;

  if Sender = FStreamBedBottomFormula then
  begin
    if StreamBedBottomPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamBedBottomPosition]);
    end;
  end;

  if Sender = FStreamFlowFormula then
  begin
    if StreamFlowPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamFlowPosition]);
    end;
  end;

  if Sender = FStreamStageFormula then
  begin
    if StreamStagePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamStagePosition]);
    end;
  end;

  if Sender = FStreamWidthFormula then
  begin
    if StreamWidthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamWidthPosition]);
    end;
  end;

  if Sender = FStreamSlopeFormula then
  begin
    if StreamSlopePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamSlopePosition]);
    end;
  end;

  if Sender = FStreamRoughnessFormula then
  begin
    if StreamRoughnessPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StreamRoughnessPosition]);
    end;
  end;
end;

function TStrBoundary.GetRoughnessPest: string;
begin
  Result := FStreamRoughnessFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamRoughnessPosition);
  end;
end;

function TStrBoundary.GetRoughnessPestObserver: TObserver;
begin
  if FRoughnessPestObserver = nil then
  begin
    CreateObserver('RoughnessPest_', FRoughnessPestObserver, nil);
    FRoughnessPestObserver.OnUpToDateSet := InvalidateRoughnessData;
  end;
  result := FRoughnessPestObserver;
end;

function TStrBoundary.GetSlopePest: string;
begin
  Result := FStreamSlopeFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamSlopePosition);
  end;
end;

function TStrBoundary.GetSlopePestObserver: TObserver;
begin
  if FSlopePestObserver = nil then
  begin
    CreateObserver('SlopePest_', FSlopePestObserver, nil);
    FSlopePestObserver.OnUpToDateSet := InvalidateSlopeData;
  end;
  result := FSlopePestObserver;
end;

function TStrBoundary.GetStagePest: string;
begin
  Result := FStreamStageFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamStagePosition);
  end;
end;

function TStrBoundary.GetStagePestObserver: TObserver;
begin
  if FStagePestObserver = nil then
  begin
    CreateObserver('StagePest_', FStagePestObserver, nil);
    FStagePestObserver.OnUpToDateSet := InvalidateStageData;
  end;
  result := FStagePestObserver;
end;

function TStrBoundary.GetUsedObserver: TObserver;
begin
//  if FUsedObserver = nil then
  begin
    CreateObserver('PestStr_Used_', FUsedObserver, nil);
//    FUsedObserver.OnUpToDateSet := HandleChangedValue;
  end;
  result := FUsedObserver;
end;

function TStrBoundary.GetWidthPest: string;
begin
  Result := FStreamWidthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreamWidthPosition);
  end;
end;

function TStrBoundary.GetWidthPestObserver: TObserver;
begin
  if FWidthPestObserver = nil then
  begin
    CreateObserver('WidthPest_', FWidthPestObserver, nil);
    FWidthPestObserver.OnUpToDateSet := InvalidateWidthData;
  end;
  result := FWidthPestObserver;
end;

procedure TStrBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TStrBoundary.InvalidateBedBottomData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrBedBottom(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrBedBottom(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateBedTopData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrBedTop(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrBedTop(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrConductance(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrConductance(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfStrConductance(self);
    Model.InvalidateMfStrBedTop(self);
    Model.InvalidateMfStrBedBottom(self);
    Model.InvalidateMfStrFlow(self);
    Model.InvalidateMfStrStage(self);
    Model.InvalidateMfStrWidth(self);
    Model.InvalidateMfStrSlope(self);
    Model.InvalidateMfStrRoughness(self);
    Model.InvalidateMfStrSegmentNumber(self);
    Model.InvalidateMfStrOutflowSegmentNumber(self);
    Model.InvalidateMfStrDiversionSegmentNumber(self);
    Model.InvalidateMfStrReachNumber(self);
  end;
end;

procedure TStrBoundary.InvalidateFlowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrFlow(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrFlow(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrRoughness(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrRoughness(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateSlopeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrSlope(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrSlope(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrStage(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrStage(self);
    end;
  end;
end;

procedure TStrBoundary.InvalidateWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfStrWidth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfStrWidth(self);
    end;
  end;
end;

class function TStrBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TStrParamItem;
end;

function TStrBoundary.ParameterType: TParameterType;
begin
  result := ptSTR;
end;

procedure TStrBoundary.SetBedBottomPest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedBottomPosition, FStreamBedBottomFormula);
end;

procedure TStrBoundary.SetBedBottomPestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FBedBottomPestMethod, Value);
end;

procedure TStrBoundary.SetBedTopPest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedTopPosition, FStreamBedTopFormula);
end;

procedure TStrBoundary.SetBedTopPestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FBedTopPestMethod, Value);
end;

procedure TStrBoundary.SetConductancePest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamConductancePosition, FStreamConductanceFormula);
end;

procedure TStrBoundary.SetConductancePestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FConductancePestMethod, Value);
end;

procedure TStrBoundary.SetFlowPest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamFlowPosition, FStreamFlowFormula);
end;

procedure TStrBoundary.SetFlowPestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FFlowPestMethod, Value);
end;

procedure TStrBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    StreamConductancePosition:
      begin
        ConductancePest := Value;
      end;
    StreamBedTopPosition:
      begin
        BedTopPest := Value;
      end;
    StreamBedBottomPosition:
      begin
        BedBottomPest := Value;
      end;
    StreamFlowPosition:
      begin
        FlowPest := Value;
      end;
    StreamStagePosition:
      begin
        StagePest := Value;
      end;
    StreamWidthPosition:
      begin
        WidthPest := Value;
      end;
    StreamSlopePosition:
      begin
        SlopePest := Value;
      end;
    StreamRoughnessPosition:
      begin
        RoughnessPest := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TStrBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    StreamConductancePosition:
      begin
        ConductancePestMethod := Value;
      end;
    StreamBedTopPosition:
      begin
        BedTopPestMethod := Value;
      end;
    StreamBedBottomPosition:
      begin
        BedBottomPestMethod := Value;
      end;
    StreamFlowPosition:
      begin
        FlowPestMethod := Value;
      end;
    StreamStagePosition:
      begin
        StagePestMethod := Value;
      end;
    StreamWidthPosition:
      begin
        WidthPestMethod := Value;
      end;
    StreamSlopePosition:
      begin
        SlopePestMethod := Value;
      end;
    StreamRoughnessPosition:
      begin
        RoughnessPestMethod := Value;
      end;
    else
      inherited;
      Assert(False);
  end;
end;

procedure TStrBoundary.SetRoughnessPest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamRoughnessPosition, FStreamRoughnessFormula);
end;

procedure TStrBoundary.SetRoughnessPestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FRoughnessPestMethod, Value);
end;

procedure TStrBoundary.SetSegmentNumber(const Value: Integer);
var
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  FOldValue: Integer;
  AScreenObject: TScreenObject;
  AnotherStr: TStrBoundary;
  AnItem: TStrItem;
  AParam: TModflowParamItem;
  ItemIndex: Integer;
  ParamIndex: Integer;
begin
  if FSegmentNumber <> Value then
  begin
    InvalidateModel;
    FOldValue := FSegmentNumber;
    FSegmentNumber := Value;
    if ParentModel <> nil then
    begin
      LocalModel :=  ParentModel as TCustomModel;
      LocalModel.ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
      if FOldValue <> 0 then
      begin
        for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
          AnotherStr := AScreenObject.ModflowStrBoundary;
          if AnotherStr <> nil then
          begin
            for ItemIndex := 0 to AnotherStr.Values.Count - 1 do
            begin
              AnItem := AnotherStr.Values[ItemIndex] as TStrItem;
              if AnItem.OutflowSegment = FOldValue then
              begin
                AnItem.OutflowSegment := FSegmentNumber;
              end;
              if AnItem.DiversionSegment = FOldValue then
              begin
                AnItem.DiversionSegment := FSegmentNumber;
              end;
            end;
            for ParamIndex := 0 to AnotherStr.Parameters.Count -1 do
            begin
              AParam := AnotherStr.Parameters[ParamIndex];
              for ItemIndex := 0 to AParam.Param.Count - 1 do
              begin
                AnItem := AParam.Param[ItemIndex] as TStrItem;
                if AnItem.OutflowSegment = FOldValue then
                begin
                  AnItem.OutflowSegment := FSegmentNumber;
                end;
                if AnItem.DiversionSegment = FOldValue then
                begin
                  AnItem.DiversionSegment := FSegmentNumber;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TStrBoundary.SetSlopePest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamSlopePosition, FStreamSlopeFormula);
end;

procedure TStrBoundary.SetSlopePestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FSlopePestMethod, Value);
end;

procedure TStrBoundary.SetStagePest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamStagePosition, FStreamStageFormula);
end;

procedure TStrBoundary.SetStagePestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FStagePestMethod, Value);
end;

procedure TStrBoundary.SetWidthPest(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamWidthPosition, FStreamWidthFormula);
end;

procedure TStrBoundary.SetWidthPestMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FWidthPestMethod, Value);
end;

procedure TStrBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalScreenObject: TScreenObject;
  LocalPhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.StobIsSelected
    and (LocalPhastModel.StreamObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

end.
