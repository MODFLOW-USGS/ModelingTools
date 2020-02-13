unit frmGwMoundUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExExtCtrls, JvNetscapeSplitter, ComCtrls, StdCtrls,
  ArgusDataEntry, JvPageList, JvExControls, Grids, RbwDataGrid4, Mask, JvExMask,
  JvSpin, TeEngine, TeeProcs, Chart, Series, ConvUtils, JvToolEdit, FastGEO,
  JvCreateProcess, JvStringHolder, Types, RealListUnit, OleCtrls, SHDocVw,
  Buttons, AppEvnts, Menus, ImgList, frmAncestorUnit, JvExStdCtrls, JvHtControls,
  JvLabel, VclTee.TeeGDIPlus, System.ImageList;

type
  TBasinShape = (bsSquare, bsRectangle, bsCircle, bsCustom);

  // If any new fields are added to TFileIndexes, AdjustIndices must be changed
  // to include it.
  TFileIndexes = record
    KhPosition: integer;
    PointsEnd: Integer;
    PointsStart: Integer;
    KvPosition: Integer;
    SyPosition: Integer;
    DepthPosition: Integer;
    ParamEndTimePosition1: Integer;
    ParamStartTimePosition2: Integer;
    ParamEndTimePosition2: Integer;
    RCH_PARPosition: Integer;
    EndtimePosition1: Integer;
    PeriodLengthPosition1: Integer;
    FirstTimeStepPosition1: Integer;
    TSMultPosition1: Integer;
    EndtimePosition2: Integer;
    PeriodLengthPosition2: Integer;
    FirstTimeStepPosition2: Integer;
    TSMultPosition2: Integer;
    StartTimePos2: Integer;
    RchPkgPosition: integer;
    UZFPkgPosition: integer;
    UzfRechargePostion: integer;
    UzfEndTimePosition1: Integer;
    UzfStartTimePosition2: Integer;
    UzfEndTimePosition2: Integer;
    BrooksCoreyPosition: Integer;
    InitialUnsatPosition: integer;
    SaturatedPosition: integer;
    MaxUnsatVKPosition: integer;
    ModelTopPosition: Integer;
    Layer1BottomPosition: integer;
    Layer2BottomPosition: Integer;
    Layer3BottomPosition: Integer;
    Layer4BottomPosition: Integer;
    Layer5BottomPosition: integer;
    Layer6BottomPosition: integer;
  end;

  TConstantFileValues = record
    FirstStepLength: double;
    NSTP: Integer;
    ModflowLocation: string;
    ModelMuseLocation: string;
    CenterX: Double;
    CenterY: double;
  end;

  TSimValues = record
    SimTime: Double;
    RechargeRate: Double;
    TotalSimTime: Double;
    FirstStep1: double;
    FirstStep2: double;
    Kx: Double;
    Kz: Double;
    SpecificYield: Double;
    TSMULT: double;
    UnSaturatedFlow: boolean;
    BrooksCoreyEpsilon: Double;
    SaturatedWaterContent: double;
    InitialWaterContent: double;
    DepthToWaterTable: double;
    ModelBottom: double;
  end;

  TResults = record
    Kx: double;
    MaxCumulativePercentDiscrepancy: double;
    MaxTimeStepPercentDiscrepancy: double;
    DrawDowns: TDoubleDynArray;
    Distances: TDoubleDynArray;
    DrawdownTimes: TDoubleDynArray;
    MaxDrawdown: double;
    StressPeriodLength: Extended;
    //  Highest row number with drawdown < -0.05
    RowForDrawdown005: integer;
    //  Maximum distance with drawdown < -0.05
    DistanceForDrawdown005: double;
    //  Highest row number with drawdown < -0.25
    RowForDrawdown025: integer;
    //  Maximum distance with drawdown < -0.25
    DistanceForDrawdown025: double;
    //  Highest row number with drawdown < -1.00
    RowForDrawdown100: integer;
    //  Maximum distance with drawdown < -1.00
    DistanceForDrawdown100: double;
    // Type of termination
    ErrorLine: string;
  end;

  THydCell = Class(TObject)
    Name: string;
    Col: integer;
    Row: integer;
    Layer: integer;
    X: double;
    Y: double;
  end;

  TGridCell = class(TCollectionItem)
  private
    FValue: string;
    FRow: integer;
    FColumn: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Column: integer read FColumn write FColumn;
    property Row: integer read FRow write FRow;
    property Value: string read FValue write FValue;
  end;

  TGridCellCollection = class(TCollection)
  private
    FGrid: TRbwDataGrid4;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetItem(Index: Integer): TGridCell;
    procedure SetItem(Index: Integer; const Value: TGridCell);
  public
    constructor Create;
    property Grid: TRbwDataGrid4 read FGrid write FGrid;
    procedure RestoreValues;
    property Items[Index: Integer]: TGridCell read GetItem write SetItem; default;
    function Add: TGridCell;
    procedure UpdateItems;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateGrid;
  published
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
  end;

  TGwMoundFile = class(TComponent)
  private
    FCustomBasin: TGridCellCollection;
    FAnalyticResults: TGridCellCollection;
    FNumericSummaryTable: TGridCellCollection;
    FNumericProfile: TGridCellCollection;
//    function GetArea: Double;
//    function GetAreaUnits: string;
//    function GetFraction: Double;
//    procedure SetArea(const Value: Double);
//    procedure SetAreaUnits(const Value: string);
//    procedure SetFraction(const Value: Double);
//    function GetDesignStorm: Double;
//    procedure SetDesignStorm(const Value: Double);
//    function GetDesignStormUnits: string;
//    function GetVolumeUnits: string;
//    procedure SetDesignStormUnits(const Value: string);
//    procedure SetVolumeUnits(const Value: string);
//    function GetStormDuration: Double;
//    procedure SetStormDuration(const Value: Double);
//    function GetStormDurationUnits: string;
//    procedure SetStormDurationUnits(const Value: string);
    function GetKv: Double;
    procedure SetKv(const Value: Double);
    function GetKvUnits: string;
    procedure SetKvUnits(const Value: string);
    function GetVertAnisotropy: Double;
    procedure SetVertAnisotropy(const Value: Double);
    function GetSpecificYield: Double;
    procedure SetSpecificYield(const Value: Double);
    function GetDistanceToWaterTable: double;
    function GetDistanceToWaterTableUnits: string;
    function GetSimulationLengthAnalytic: double;
    function GetSimulationLengthUnitsAnalytic: string;
    procedure SetDistanceToWaterTable(const Value: double);
    procedure SetDistanceToWaterTableUnits(const Value: string);
    procedure SetSimulationLengthAnalytic(const Value: double);
    procedure SetSimulationLengthUnitsAnalytic(const Value: string);
//    function GetMaxBasinDepth: double;
//    procedure SetMaxBasinDepth(const Value: double);
    function GetMaxBasinDepthUnits: string;
    procedure SetMaxBasinDepthUnits(const Value: string);
    function GetBasinAreaUnits: string;
    procedure SetBasinAreaUnits(const Value: string);
    function GetBasinShape: TBasinShape;
    procedure SetBasinShape(const Value: TBasinShape);
    function GetBasinDiameter: double;
    function GetBasinDiameterUnits: string;
    function GetBasinRectangleLength: double;
    function GetBasinRectangleUnits: string;
    function GetBasinRectangleWidth: double;
    function GetBasinSquareSide: double;
    function GetBasinSquareSideUnits: string;
    procedure SetBasinDiameter(const Value: double);
    procedure SetBasinDiameterUnits(const Value: string);
    procedure SetBasinRectangleLength(const Value: double);
    procedure SetBasinRectangleUnits(const Value: string);
    procedure SetBasinRectangleWidth(const Value: double);
    procedure SetBasinSquareSide(const Value: double);
    procedure SetBasinSquareSideUnits(const Value: string);
    function GetCustomBasin: TGridCellCollection;
    procedure SetCustomBasin(const Value: TGridCellCollection);
    function GetAnalyticResults: TGridCellCollection;
    function GetCustomBasinUnits: string;
    function GetMaxDistance: double;
    function GetMaxDistanceUnits: string;
    function GetTimeIncrement: double;
    function GetTimeIncrementUnits: string;
    procedure SetAnalyticResults(const Value: TGridCellCollection);
    procedure SetCustomBasinUnits(const Value: string);
    procedure SetMaxDistance(const Value: double);
    procedure SetMaxDistanceUnits(const Value: string);
    procedure SetTimeIncrement(const Value: double);
    procedure SetTimeIncrementUnits(const Value: string);
    function GetBrooksCorey: Double;
    procedure SetBrooksCorey(const Value: Double);
    function GetInitialWaterContent: double;
    function GetSaturatedWaterContent: double;
    function GetUnsaturatedFlow: Boolean;
    procedure SetInitialWaterContent(const Value: double);
    procedure SetSaturatedWaterContent(const Value: double);
    procedure SetUnsaturatedFlow(const Value: Boolean);
    function GetModelMuseFileLocation: string;
    procedure SetModelMuseFileLocation(const Value: string);
    function GetNumericProfile: TGridCellCollection;
    function GetNumericSummaryTable: TGridCellCollection;
    procedure SetNumericProfile(const Value: TGridCellCollection);
    procedure SetNumericSummaryTable(const Value: TGridCellCollection);
    function GetAquiferThickness: double;
    function GetAquiferThicknessUnits: string;
    procedure SetAquiferThickness(const Value: double);
    procedure SetAquiferThicknessUnits(const Value: string);
    procedure SetUnits(Combo: TComboBox; const Value: string);
    function GetMaxDistanceNumeric: double;
    function GetMaxDistanceNumericUnits: string;
    procedure SetMaxDistanceNumeric(const Value: double);
    procedure SetMaxDistanceNumericUnits(const Value: string);
//    function GetVolume: Double;
//    procedure SetVolume(const Value: Double);
    function GetSimulationLengthNumeric: double;
    function GetSimulationLengthUnitsNumeric: string;
    procedure SetSimulationLengthNumeric(const Value: double);
    procedure SetSimulationLengthUnitsNumeric(const Value: string);
    function GetBasinDepth: double;
    procedure SetBasinDepth(const Value: double);
    function GetInfiltrationTime: Double;
    function GetInfiltrationTimeUnits: string;
    procedure SetInfiltrationTime(const Value: Double);
    procedure SetInfiltrationTimeUnits(const Value: string);
  protected
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  published
//    property Area: Double read GetArea write SetArea;
//    property AreaUnits: string read GetAreaUnits write SetAreaUnits;
//    property Fraction: Double read GetFraction write SetFraction;
//    property DesignStorm: Double read GetDesignStorm write SetDesignStorm;
//    property DesignStormUnits: string read GetDesignStormUnits write SetDesignStormUnits;
//    property Volume: Double read GetVolume write SetVolume;
//    property VolumeUnits: string read GetVolumeUnits write SetVolumeUnits;
//    property StormDuration: Double read GetStormDuration write SetStormDuration;
//    property StormDurationUnits: string read GetStormDurationUnits write SetStormDurationUnits;
    property Kv: Double read GetKv write SetKv;
    property KvUnits: string read GetKvUnits write SetKvUnits;
    property Kx: Double read GetVertAnisotropy write SetVertAnisotropy;
    property SpecificYield: Double read GetSpecificYield write SetSpecificYield;
    property DistanceToWaterTable: double read GetDistanceToWaterTable write SetDistanceToWaterTable;
    property DistanceToWaterTableUnits: string read GetDistanceToWaterTableUnits write SetDistanceToWaterTableUnits;
    property AquiferThickness: double read GetAquiferThickness write SetAquiferThickness;
    property AquiferThicknessUnits: string read GetAquiferThicknessUnits write SetAquiferThicknessUnits;
    property SimulationLengthAnalytic: double read GetSimulationLengthAnalytic
      write SetSimulationLengthAnalytic;
    property SimulationLengthUnitsAnalytic: string
      read GetSimulationLengthUnitsAnalytic
      write SetSimulationLengthUnitsAnalytic;
    property SimulationLengthNumeric: double read GetSimulationLengthNumeric
      write SetSimulationLengthNumeric;
    property SimulationLengthUnitsNumeric: string
      read GetSimulationLengthUnitsNumeric
      write SetSimulationLengthUnitsNumeric;
//    property MaxBasinDepth: double read GetMaxBasinDepth write SetMaxBasinDepth;
    property BasinDepth: double read GetBasinDepth write SetBasinDepth;
    property MaxBasinDepthUnits: string read GetMaxBasinDepthUnits write SetMaxBasinDepthUnits;
    property BasinAreaUnits: string read GetBasinAreaUnits write SetBasinAreaUnits;
    property BasinShape: TBasinShape read GetBasinShape write SetBasinShape;
    property BasinDiameter: double read GetBasinDiameter write SetBasinDiameter;
    property BasinDiameterUnits: string read GetBasinDiameterUnits write SetBasinDiameterUnits;
    property BasinSquareSide: double read GetBasinSquareSide write SetBasinSquareSide;
    property BasinSquareSideUnits: string read GetBasinSquareSideUnits write SetBasinSquareSideUnits;
    property BasinRectangleLength: double read GetBasinRectangleLength write SetBasinRectangleLength;
    property BasinRectangleWidth: double read GetBasinRectangleWidth write SetBasinRectangleWidth;
    property BasinRectangleUnits: string read GetBasinRectangleUnits write SetBasinRectangleUnits;
    property CustomBasin: TGridCellCollection read GetCustomBasin write SetCustomBasin;
    property CustomBasinUnits: string read GetCustomBasinUnits write SetCustomBasinUnits;
    property MaxDistance: double read GetMaxDistance write SetMaxDistance;
    property MaxDistanceUnits: string read GetMaxDistanceUnits write SetMaxDistanceUnits;
    property TimeIncrement: double read GetTimeIncrement write SetTimeIncrement;
    property TimeIncrementUnits: string read GetTimeIncrementUnits write SetTimeIncrementUnits;
    property AnalyticResults: TGridCellCollection read GetAnalyticResults write SetAnalyticResults;
    property UnsaturatedFlow: Boolean read GetUnsaturatedFlow write SetUnsaturatedFlow;
    property BrooksCorey: Double read GetBrooksCorey write SetBrooksCorey;
    property SaturatedWaterContent: double read GetSaturatedWaterContent write SetSaturatedWaterContent;
    property InitialWaterContent: double read GetInitialWaterContent write SetInitialWaterContent;
    property ModelMuseFileLocation: string read GetModelMuseFileLocation write SetModelMuseFileLocation;
    property NumericSummaryTable: TGridCellCollection read GetNumericSummaryTable write SetNumericSummaryTable;
    property NumericProfile: TGridCellCollection read GetNumericProfile write SetNumericProfile;
    property MaxDistanceNumeric: double read GetMaxDistanceNumeric write SetMaxDistanceNumeric;
    property MaxDistanceNumericUnits: string read GetMaxDistanceNumericUnits write SetMaxDistanceNumericUnits;
    property InfiltrationTimeUnits: string read GetInfiltrationTimeUnits write SetInfiltrationTimeUnits;
    property InfiltrationTime: Double read GetInfiltrationTime write SetInfiltrationTime;
  end;

  TRunModelThread = class;

  TfrmGwMound = class(TfrmAncestor)
    pnlBottom: TPanel;
    tvNavigation: TTreeView;
    pnlHelp: TPanel;
    splitLeft: TJvNetscapeSplitter;
    splitRight: TJvNetscapeSplitter;
    plMain: TJvPageList;
    jvspAquiferProperties: TJvStandardPage;
    lblKz: TLabel;
    rdeKz: TRbwDataEntry;
    comboKzUnits: TComboBox;
    lblRatio: TLabel;
    rdeKx: TRbwDataEntry;
    lblSpecificYield: TLabel;
    rdeSpecificYield: TRbwDataEntry;
    lblHeightAboveWaterTable: TLabel;
    rdeHeightAboveWaterTable: TRbwDataEntry;
    comboHeightAboveWaterTableUnits: TComboBox;
    plBasin: TJvPageList;
    jvspSquareBasin: TJvStandardPage;
    lblSquareLength: TLabel;
    rdeSquareLength: TRbwDataEntry;
    comboSquareLengthUnits: TComboBox;
    jvspRectangle: TJvStandardPage;
    lblRectBasinLength: TLabel;
    rdeRectBasinLength: TRbwDataEntry;
    comboRectUnits: TComboBox;
    lblRectBasinWidth: TLabel;
    rdeRectBasinWidth: TRbwDataEntry;
    jvspCircle: TJvStandardPage;
    lblBasinCircleDiameter: TLabel;
    rdeBasinCircleDiameter: TRbwDataEntry;
    comboBasinCircleDiameterUnits: TComboBox;
    jvspCustom: TJvStandardPage;
    rdgBasinCoordinates: TRbwDataGrid4;
    lblBasinCoordinates: TLabel;
    seCoordCount: TJvSpinEdit;
    lblCoordCount: TLabel;
    btnCustomBasin: TButton;
    jvspBasinDesign: TJvStandardPage;
    jvspUnsaturatedFlow: TJvStandardPage;
    cbSimulateUnsat: TCheckBox;
    rdeBrooksCoreyEpsilon: TRbwDataEntry;
    lblBrooksCoreyEpsilon: TLabel;
    lblSaturatedWaterFraction: TLabel;
    rdeSaturatedWaterFraction: TRbwDataEntry;
    lblInitialWaterContent: TLabel;
    rdeInitialWaterContent: TRbwDataEntry;
    lblAquiferThickness: TLabel;
    rdeAquiferThickness: TRbwDataEntry;
    comboAquiferThicknessUnits: TComboBox;
    jvspRunAnalyticalModel: TJvStandardPage;
    jvspRunNumeric: TJvStandardPage;
    jvspProgramLocations: TJvStandardPage;
    lblMODFLOW: TLabel;
    feMODFLOW: TJvFilenameEdit;
    lblModelMuseApplication: TLabel;
    feModelMuseApplication: TJvFilenameEdit;
    lblModelMuseFile: TLabel;
    feModelMuseFile: TJvFilenameEdit;
    comboCustomUnits: TComboBox;
    jvstringsMMFile: TJvMultiStringHolder;
    wbHelp: TWebBrowser;
    btnBack: TBitBtn;
    btnNext: TBitBtn;
    aplctnvnts1: TApplicationEvents;
    pgcAnalytic: TPageControl;
    tabAnalyticGraph: TTabSheet;
    chrtAnalytical: TChart;
    seriesAnayltical: TLineSeries;
    tabProfileAnalytic: TTabSheet;
    rdgAnalytic: TRbwDataGrid4;
    pnl1: TPanel;
    btnCopyAnalytic: TButton;
    pgcNumeric: TPageControl;
    tabGraphNumeric: TTabSheet;
    chrtNumeric: TChart;
    seriesNumeric: TLineSeries;
    tabTableNumeric: TTabSheet;
    pnl2: TPanel;
    rdgSummaryNumeric: TRbwDataGrid4;
    tabProfile: TTabSheet;
    rdgProfileNumeric: TRbwDataGrid4;
    btnCopySummary: TButton;
    pnl3: TPanel;
    btnCopyNumericProfile: TButton;
    pnl4: TPanel;
    btnCopyNumericChart: TButton;
    pnl5: TPanel;
    btnCopyAnalyticChart: TButton;
    MenuMain: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    dlgOpenFile: TOpenDialog;
    dlgSaveFile: TSaveDialog;
    SaveAs1: TMenuItem;
    ilStates: TImageList;
    comboMaxDepthUnits: TComboBox;
    comboMinBasinAreaUnits: TComboBox;
    rgBasinShape: TRadioGroup;
    lbledBasinArea: TLabeledEdit;
    htlblModflow: TJvHTLabel;
    htlblModelMuse: TJvHTLabel;
    shpRectangle: TShape;
    jvlblLength: TJvLabel;
    lblWidth: TLabel;
    shpCircle: TShape;
    shpDiameter1: TShape;
    shpDiameter2: TShape;
    shpDiameter3: TShape;
    lblDiameter: TLabel;
    shpSquare: TShape;
    lblSquare: TLabel;
    tabAnalyticControls: TTabSheet;
    lblSimulationLength: TLabel;
    rdeSimulationLengthAnalytic: TRbwDataEntry;
    comboSimulationLengthUnitsAnalytic: TComboBox;
    lblDurationOfInfiltrationAnalytic: TLabel;
    lblMaxDistance: TLabel;
    rdeMaxDistance: TRbwDataEntry;
    comboMaxDistanceUnits: TComboBox;
    lblTimeIncrement: TLabel;
    rdeTimeIncrement: TRbwDataEntry;
    comboTimeIncrementUnits: TComboBox;
    btnRunHantush: TButton;
    btnAbort: TBitBtn;
    tabNumericControls: TTabSheet;
    lblSimulationLengthNumeric: TLabel;
    rdeSimulationLengthNumeric: TRbwDataEntry;
    comboSimulationLengthUnitsNumeric: TComboBox;
    lblDurationOfInfiltrationNumeric: TLabel;
    btnRunNumericModel: TButton;
    btnAbortNumeric: TBitBtn;
    lblMaxDistanceNumeric: TLabel;
    rdeMaxDistanceNumeric: TRbwDataEntry;
    comboMaxNumericDistanceUnits: TComboBox;
    rdeBasinDepth: TRbwDataEntry;
    lblBasinDepth: TLabel;
    lblKxUnits: TLabel;
    btnMetric: TButton;
    btnEnglish: TButton;
    lblDurationOfInfiltration: TLabel;
    rdeDurationOfInfiltration: TRbwDataEntry;
    comboDurationOfInfiltration: TComboBox;
    lblWarning: TLabel;
    procedure rdgBasinCoordinatesEndUpdate(Sender: TObject);
    procedure seCoordCountChange(Sender: TObject);
    procedure cbSimulateUnsatClick(Sender: TObject);
    procedure tvNavigationChange(Sender: TObject; Node: TTreeNode);
    procedure rdeDevelopmentAreaChange(Sender: TObject);
    procedure comboDevelopmentAreaUnitChange(Sender: TObject);
    procedure rdeImperviousFractionChange(Sender: TObject);
    procedure rdeDesignStormChange(Sender: TObject);
    procedure comboDesignStormUnitsChange(Sender: TObject);
    procedure comboVolumeUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rdeMaxDepthChange(Sender: TObject);
    procedure comboMaxDepthUnitsChange(Sender: TObject);
    procedure comboMinBasinAreaUnitsChange(Sender: TObject);
    procedure rdeKzChange(Sender: TObject);
    procedure comboKzUnitsChange(Sender: TObject);
    procedure rgBasinShapeClick(Sender: TObject);
    procedure btnRunHantushClick(Sender: TObject);
    procedure btnRunNumericModelClick(Sender: TObject);
    procedure plMainChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCopyAnalyticClick(Sender: TObject);
    procedure btnCopySummaryClick(Sender: TObject);
    procedure btnCopyNumericProfileClick(Sender: TObject);
    procedure btnCopyNumericChartClick(Sender: TObject);
    procedure btnCopyAnalyticChartClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure comboCustomUnitsChange(Sender: TObject);
    procedure rdeBasinCircleDiameterChange(Sender: TObject);
    procedure comboBasinCircleDiameterUnitsChange(Sender: TObject);
    procedure rdeRectBasinLengthChange(Sender: TObject);
    procedure comboRectUnitsChange(Sender: TObject);
    procedure rdeRectBasinWidthChange(Sender: TObject);
    procedure rdeSquareLengthChange(Sender: TObject);
    procedure comboSquareLengthUnitsChange(Sender: TObject);
    procedure rdeDurationChange(Sender: TObject);
    procedure comboDurationChange(Sender: TObject);
    procedure rdeKxChange(Sender: TObject);
    procedure rdeSpecificYieldChange(Sender: TObject);
    procedure rdeHeightAboveWaterTableChange(Sender: TObject);
    procedure comboHeightAboveWaterTableUnitsChange(Sender: TObject);
    procedure rdeAquiferThicknessChange(Sender: TObject);
    procedure comboAquiferThicknessUnitsChange(Sender: TObject);
    procedure rdeSimulationLengthAnalyticChange(Sender: TObject);
    procedure comboSimulationLengthUnitsAnalyticChange(Sender: TObject);
    procedure rdeMaxDistanceChange(Sender: TObject);
    procedure comboMaxDistanceUnitsChange(Sender: TObject);
    procedure rdeTimeIncrementChange(Sender: TObject);
    procedure comboTimeIncrementUnitsChange(Sender: TObject);
    procedure rdeBrooksCoreyEpsilonClick(Sender: TObject);
    procedure rdeSaturatedWaterFractionClick(Sender: TObject);
    procedure rdeInitialWaterContentClick(Sender: TObject);
    procedure ControlEnter(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure feMODFLOWChange(Sender: TObject);
    procedure feModelMuseApplicationChange(Sender: TObject);
    procedure feModelMuseFileChange(Sender: TObject);
    procedure rdeMaxDistanceNumericChange(Sender: TObject);
    procedure comboMaxNumericDistanceUnitsChange(Sender: TObject);
    procedure btnAbortNumericClick(Sender: TObject);
    procedure btnCustomBasinClick(Sender: TObject);
    procedure rdeSimulationLengthNumericChange(Sender: TObject);
    procedure comboSimulationLengthUnitsNumericChange(Sender: TObject);
    procedure rdeVolumeChange(Sender: TObject);
    procedure rdeExceededBounds(Sender: TObject);
    procedure CheckGreaterThanZero(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SetDefaultUnitsClick(Sender: TObject);
    procedure rdeSpecificYieldExit(Sender: TObject);
    procedure pgcNumericChange(Sender: TObject);
    procedure pgcAnalyticChange(Sender: TObject);
    procedure rdeSimulationLengthNumericExceededBounds(Sender: TObject);
    procedure comboDurationOfInfiltrationEnter(Sender: TObject);
    procedure comboDurationOfInfiltrationChange(Sender: TObject);
    procedure rdeDurationOfInfiltrationChange(Sender: TObject);
  private
    FTimeSeries: TList;
{$IFDEF UNSAT}
    FUnsatNode: TTreeNode;
{$ENDIF}
    FRunAnalytic: TTreeNode;
    FRunNumeric: TTreeNode;
    FAbortAnalytic: Boolean;
    FThread: TRunModelThread;
    FDefaultMetricUnits: TStringList;
    FEnglishMetricUnits: TStringList;
    FIniltrationTimeUnits: Integer;
    function GetTimeUnit(ItemIndex: Integer): TConvType;

    procedure UpdateEdVolume;
//    procedure UpdateEdMinBasinArea;

    function GetRechargeVolume: double;
//    function MinimumBasinArea: double;
    procedure InitializeComboKzUnits;
    function GetBasinArea: double;
    function GetAquiferThickness: Double;
    function GetSquareSideLength: Double;
    function GetRectangleLength: Double;
    function GetRectangleWidth: Double;
    function GetCircleDiameter: Double;
    function GetMaxDistance: Double;
    function GetKz: Double;
//    function GetMaxDepth: Double;
//    function GetMaxDuration: Double;
//    function GetDevelopmentArea: Double;
//    function GetRainfall: Double;
//    function GetFractionImpervious: Double;
    function GetPolygon: TPolygon2D;
//    function GetAnisotropy: Double;
    function GetKx: Double;
    function GetAnalyticSimulationLength: double;
    function GetDistanceToWaterTable: double;

    // @name calculates the radius of a polygon consisting of Count
    // wedge-shaped triangles radiating from the center of the polygon
    // whose combined area would be the same as a
    // circle with the input Radius
    function EquivalentRadius(Radius: double; Count: integer): double;
    function RechargePolygon: TPolygon2D;
    procedure GetFilePositions(ModelMuseFile: TStringList;
      var FileIndexes: TFileIndexes; var FileConstants: TConstantFileValues);
    procedure SetRechargePoints(ModelMuseFile: TStringList;
      var FileIndexes: TFileIndexes; FileContstants: TConstantFileValues);
    procedure AdjustIndices(var FileIndexes: TFileIndexes; NewPosition,
      OldPosition: Integer);
    function GetSpecificYield: double;
    procedure GetRechargeRateAndTime(var RechargeTime,
      RechargeRate: Double);
    procedure GetModelVariables(var SimValues: TSimValues);
    function GetSimulationIncrement: double;
    function IniFileName: string;
    procedure SaveFile;
    procedure UpdateAnalyticGraph;
    procedure UpdateNumericGraph;
    procedure EraseAnalyticResults;
    procedure EraseNumericResults;
    procedure UpdateNodeStateIndex;
    function GetHelpUrl(Keyword: string): string;
    procedure DisplayMoundToHighMesssage;
    procedure DisplayRiseToHigh;
    function GetNumericSimulationLength: double;
    function GetAreaUnitsExtended(ItemIndex: Integer): TConvType;
    procedure UpdateDisplayedBasinArea;
    function GetBasinWidth: double;
    procedure ResetColor(Sender: TObject);
    procedure ShowKxUnits;
    procedure DefineDefaultUnits;
    procedure RemoveSimLengthWarningColor;
    procedure GetDefaultRechargeRateAndTime(var RechargeTime,
      RechargeRate: Double);
    procedure UpdateInfiltrationTime;
    function GetInfiltrationTime: double;
    procedure SetMinForInfiltrationTime;
    { Private declarations }
  public
    { Public declarations }
  end;

  TRunModelThread = class(TThread)
  private
    FDirectory: string;
    FModelMuseFile: TStringList;
    FCreateProcess: TJvCreateProcess;
    FFileIndexes: TFileIndexes;
    FSimValues: TSimValues;
    FConstants: TConstantFileValues;
    FModelMuseFileName: string;
    FResults: TResults;
    FCenterX: double;
    FCenterY: double;
    FSeries: TLineSeries;
    FDistanceUnits: TConvType;
    FTimeUnits: TConvType;
    FSummaryGrid: TRbwDataGrid4;
    FProfileGrid: TRbwDataGrid4;
    procedure ProcessDone(Sender: TObject; ExitCode: DWORD);
    procedure SetModelVariables;
    procedure RunModelMuse;
    procedure RunModflow;
    procedure ExtractResults;
    procedure SaveResults;
    procedure SaveFiles;
    procedure UpdateProgressBar;
    procedure ExtractFromListingFile;
    procedure GetColRowPositions(var ColPositions: TRealList; var RowPositions: TRealList);
    procedure ReadHydmodInput(RowPositions: TRealList; ColPositions: TRealList; var HydInput: TStringList);
    procedure ExtractedHydmodResults(HydInput: TStringList);
    procedure DisableAbort;
  protected
    procedure Execute; override;
  public
    constructor Create(Directory: string; ModelMuseFile: TStringList;
      FileIndexes: TFileIndexes; SimValues: TSimValues;
      FileConstants: TConstantFileValues; Series: TLIneSeries;
      DistanceUnits, TimeUnits: TConvType; SummaryGrid, ProfileGrid: TRbwDataGrid4);
    destructor Destroy; override;
    constructor NilSelf;
    procedure Terminate;
  end;


var
  frmGwMound: TfrmGwMound;

implementation

uses
  StdConvs, Math, HantushUnit, WellFunctionUnit, fmath, Contnrs, RootFinder,
  SyncObjs, BMSearch, IOUtils, ReadModflowArrayUnit, IntListUnit, IniFiles,
  IniFileUtilities, frmCustomBasinUnit, Winapi.ShlObj,
  System.Win.Registry, System.Generics.Collections;

resourcestring
  StrBecauseTheTimeReq = 'Because the time required for drainage is greater ' +
  'than the maximum allowed time for drainage, no results will be calculate' +
  'd.';
  StrTheAnalyticSolutio = 'The analytic solution is only valid for square, r' +
  'ectangular, or circular basin shapes.';
  StrSDoesNotExist = '%s  does not exist.';
  StrProgramLocations = 'Program Locations';
  StrMODFLOW = 'MODFLOW';
  StrModelMuse = 'ModelMuse';
  StrBecauseTheRiseIn = 'Because the rise in the water table is greater than' +
  ' the distance to the water table, the simulation results are unreliable.';
  StrRiseToHigh = 'Because the rise in the water table is greater than 50% o' +
  'f the aquifer thickness, the simulation results are unreliable.';
  StrTheSimulationLengt = 'The simulation length is less than the time requi' +
  'red to drain the basin. Are you sure you want to continue?';

  StrModflowLocation = 'C:\WRDAPP\MF2005.1_12\bin\mf2005dbl.exe';
  StrModelMuseLocation = 'C:\Program Files\USGS\ModelMuse4\bin\ModelMus' +
  'e.exe';
  StrInvalidValueOfHor = 'Invalid value of horizontal hydraulic conductivity';
  StrInvalidValueOfAqu = 'Invalid value of aquifer thickness';
  StrInvalidValueOfSpe = 'Invalid value of specific yield';
  StrInvalidValueOfSiz = 'Invalid value of size of basin';
  StrInvalidValueOfMax = 'Invalid value of maximum distance';
  StrInvalidValueOfVer = 'Invalid value of vertical hydraulic conductivity';
  StrInvalidSimulationT = 'Invalid simulation time';
  StrInvalidValueOfBro = 'Invalid value of Brooks-Corey epsilon';
  StrInvalidValueOfSat = 'Invalid value of saturated water content.';
  StrInvalidValueOfIn  = 'Invalid value of initial water content';
  StrSpecificYieldsGrea = 'Specific yields greater than 0.2 should be suppor' +
  'ted by site measurements.';
  StrInvalidValueOfDisWatTable = 'Invalid value of depth to water table';

const
  siNo = 1;
  siYes = 2;
  seNotAllowed = 3;
  MaxDistances = 37;
  DistanceFractions: array [0..MaxDistances-1] of Double =
    (0, 0.001, 0.003, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04,
    0.045, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2,
    0.22, 0.24, 0.26, 0.28, 0.3, 0.35, 0.4, 0.45, 0.5, 0.6, 0.7, 0.8, 0.9, 1);

{$R *.dfm}

var
  cbVelocity : TConvFamily;

//  vuInchesPerSecond: TConvType;
//  vuInchesPerMinute: TConvType;
  vuInchesPerHour: TConvType;
//  vuInchesPerDay: TConvType;

  vuFeetPerSecond: TConvType;
//  vuFeetPerMinute: TConvType;
//  vuFeetPerHour: TConvType;
  vuFeetPerDay: TConvType;

  vuCentimetersPerSecond: TConvType;
//  vuCentimetersPerMinute: TConvType;
  vuCentimetersPerHour: TConvType;
  vuCentimetersPerDay: TConvType;

  vuMetersPerSecond: TConvType;
//  vuMetersPerMinute: TConvType;
  vuMetersPerHour: TConvType;
  vuMetersPerDay: TConvType;

  VelocityConvTypes: array[0..8] of TConvType;

const
  TSMULT = 1.2;

var
  DirectoryLock: TCriticalSection;

{function GetTSMULT(const PERLEN: double; var FirstStep: double;
  const NSTP: integer): double;
  function GetPerLen(TSMULT: double): double;
  begin
    if TSMULT = 1 then
    begin
      result := FirstStep*NSTP;
    end
    else
    begin
      result := FirstStep * (Power(TSMULT, NSTP)-1)/(TSMULT-1);
    end;
  end;
var
  LowerTestValue: double;
  HigherTestValue: double;
  TestLength: double;
begin
  if PERLEN/NSTP < FirstStep then
  begin
    FirstStep := PERLEN/NSTP;
    result := 1;
    Exit;
  end;
  LowerTestValue := 1;
  HigherTestValue := 1;
  Assert(GetPerLen(LowerTestValue) < PERLEN);
  repeat
    HigherTestValue := HigherTestValue * 2;
  until GetPerLen(HigherTestValue) >= PERLEN;
  repeat
    result := (LowerTestValue+HigherTestValue)/2;
    TestLength := GetPerLen(result);
    if TestLength > PERLEN then
    begin
      HigherTestValue := result;
    end
    else
    begin
      LowerTestValue := result;
    end;
  until Abs(TestLength-PERLEN)/PERLEN < 1e-6;
end;  }

function GetMyDocuments: string;
// from http://delphi.about.com/od/delphitips2007/qt/mydocuments.htm
 var
  r: Bool;
  path: array[0..Max_Path] of Char;
 begin
  r := ShGetSpecialFolderPath(0, path, CSIDL_Personal, False) ;
  if not r then raise Exception.Create('Could not find MyDocuments folder location.') ;
  Result := Path;
 end;

//procedure TfrmGwMound.UpdateEdMinBasinArea;
//var
//  AreaUnits: TConvType;
//begin
//  if csLoading in ComponentState then
//  begin
//    Exit;
//  end;
////  ItemIndex := comboMinBasinAreaUnits.ItemIndex;
//  AreaUnits := GetAreaUnitsExtended(comboMinBasinAreaUnits.ItemIndex);
//  edMinBasinArea.Text := FloatToStr(ConvertTo(MinimumBasinArea, AreaUnits));
//  UpdateDisplayedBasinArea;
//end;

function TfrmGwMound.IniFileName: string;
begin
  result := IniFileUtilities.IniFileName(Handle, ParamStr(0));
end;

procedure TfrmGwMound.SaveFile;
var
  GwMound: TGwMoundFile;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  FileStream := TFileStream.Create(dlgSaveFile.FileName, fmCreate or fmShareDenyWrite);
  try
    MemStream := TMemoryStream.Create;
    try
      GwMound := TGwMoundFile.Create(nil);
      try
        MemStream.WriteComponent(GwMound);
      finally
        GwMound.Free;
      end;
      MemStream.Position := 0;
      ObjectBinaryToText(MemStream, FileStream);
    finally
      MemStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TfrmGwMound.InitializeComboKzUnits;
begin
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuInchesPerSecond));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuInchesPerMinute));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuInchesPerHour));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuInchesPerDay));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuFeetPerSecond));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuFeetPerMinute));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuFeetPerHour));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuFeetPerDay));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuCentimetersPerSecond));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuCentimetersPerMinute));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuCentimetersPerHour));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuCentimetersPerDay));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuMetersPerSecond));
//  comboKzUnits.Items.Add(ConvTypeToDescription(vuMetersPerMinute));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuMetersPerHour));
  comboKzUnits.Items.Add(ConvTypeToDescription(vuMetersPerDay));
  comboKzUnits.ItemIndex := comboKzUnits.Items.IndexOf(ConvTypeToDescription(vuMetersPerSecond));
end;

procedure TfrmGwMound.UpdateEdVolume;
begin
//  if csLoading in ComponentState then
//  begin
//    Exit;
//  end;
//  UpdateEdMinBasinArea;
end;

function TfrmGwMound.GetBasinWidth: double;
var
  SideLength: double;
  RectLength: double;
  RectWidth: double;
  Diameter: double;
  Polygon: TPolygon2D;
begin
  result := 0;
  case TBasinShape(rgBasinShape.ItemIndex) of
    bsSquare: 
      begin 
        SideLength := GetSquareSideLength;
        result := SideLength;
      end;
    bsRectangle: 
      begin 
        RectLength := GetRectangleLength;
        RectWidth := GetRectangleWidth;
        result := Max(RectLength,RectWidth);
      end;
    bsCircle: 
      begin 
        Diameter := GetCircleDiameter;
        result := Diameter;
      end; 
    bsCustom: 
      begin 
        Polygon := GetPolygon;
        result := GeometricSpan(Polygon);
      end;
    else
      Assert(False);
  end;
end;

function TfrmGwMound.GetBasinArea: double;
var
  SideLength: double;
  RectLength: double;
  RectWidth: double;
  Diameter: double;
  Polygon: TPolygon2D;
begin
  result := 0;

  case TBasinShape(rgBasinShape.ItemIndex) of
    bsSquare:
      begin
        SideLength := GetSquareSideLength;
        result := Sqr(SideLength);
        // Square
      end;
    bsRectangle:
      begin
        // Rectangular
        RectLength := GetRectangleLength;
        RectWidth := GetRectangleWidth;
        result := RectLength*RectWidth;
      end;
    bsCircle:
      begin
        // circular
        Diameter := GetCircleDiameter;
        result := Pi*Sqr(Diameter/2);
      end;
    bsCustom:
      begin
        // custom
        Polygon := GetPolygon;
        result := Abs(Area(Polygon));
      end;
    else Assert(False);
  end;
end;

//function TfrmGwMound.GetLengthUnits(ItemIndex: Integer): TConvType;
//begin
//  result := duMeters;
//  case ItemIndex of
//    0:
//      result := duInches;
//    1:
//      result := duFeet;
//    2:
//      result := duCentimeters;
//    3:
//      result := duMeters;
//  else
//    Assert(False);
//  end;
//end;

function TfrmGwMound.GetAquiferThickness: Double;
var
  AquiferThicknessUnits: TConvType;
begin
  result := StrToFloat(rdeAquiferThickness.Text);
  AquiferThicknessUnits := GetStandardLengthUnits(comboAquiferThicknessUnits.ItemIndex);
  result := ConvertFrom(AquiferThicknessUnits, result);
end;

function TfrmGwMound.GetSquareSideLength: Double;
var
  SideUnits: TConvType;
begin
  result := StrToFloat(rdeSquareLength.Text);
  SideUnits := GetLengthUnits(comboSquareLengthUnits.ItemIndex);
  result := ConvertFrom(SideUnits, result);
end;

function TfrmGwMound.GetRectangleLength: Double;
var
  RectUnits: TConvType;
begin
  RectUnits := GetLengthUnits(comboRectUnits.ItemIndex);
  result := StrToFloat(rdeRectBasinLength.Text);
  result := ConvertFrom(RectUnits, result);
end;

function TfrmGwMound.GetRectangleWidth: Double;
var
  RectUnits: TConvType;
begin
  RectUnits := GetLengthUnits(comboRectUnits.ItemIndex);
  result := StrToFloat(rdeRectBasinWidth.Text);
  result := ConvertFrom(RectUnits, result);
end;

function TfrmGwMound.GetCircleDiameter: Double;
var
  DiameterUnits: TConvType;
begin
  result := StrToFloat(rdeBasinCircleDiameter.Text);
  DiameterUnits := GetLengthUnits(comboBasinCircleDiameterUnits.ItemIndex);
  result := ConvertFrom(DiameterUnits, result);
end;

function TfrmGwMound.GetMaxDistance: Double;
var
  MaxDistanceUnits: TConvType;
begin
  result := StrToFloat(rdeMaxDistance.Text);
  MaxDistanceUnits := GetStandardLengthUnits(comboMaxDistanceUnits.ItemIndex);
  result := ConvertFrom(MaxDistanceUnits, result);
end;

function TfrmGwMound.GetKz: Double;
var
  VelUnits: TConvType;
begin
  result := StrToFloatDef(rdeKz.Text, 0);
  if result = 0 then
  begin
    Exit;
  end;
  VelUnits := VelocityConvTypes[comboKzUnits.ItemIndex];
  result := ConvertFrom(VelUnits, result);
end;

//function TfrmGwMound.GetMaxDepth: Double;
//var
//  MaxDepthUnits: TConvType;
//begin
//  result := StrToFloatDef(rdeMaxDepth.Text, 0);
//  MaxDepthUnits := GetLengthUnits(comboMaxDepthUnits.ItemIndex);
//  result := ConvertFrom(MaxDepthUnits, result);
//end;

function TfrmGwMound.GetTimeUnit(ItemIndex: Integer): TConvType;
begin
  result := tuDays;
  case ItemIndex of
    0:
      result := tuSeconds;
    1:
      result := tuMinutes;
    2:
      result := tuHours;
    3:
      result := tuDays;
  else
    Assert(False);
  end;
end;

//function TfrmGwMound.GetMaxDuration: Double;
//var
//  TimeUnit: TConvType;
//begin
//  result := StrToFloatDef(rdeDuration.Text, 0);
//  TimeUnit := GetTimeUnit(comboDuration.ItemIndex);
//  result := ConvertFrom(TimeUnit, result);
//end;

//function TfrmGwMound.GetDevelopmentArea: Double;
//var
//  AreaUnits: TConvType;
//begin
//  result := StrToFloatDef(rdeDevelopmentArea.Text, 0);
//  AreaUnits := GetAreaUnits(comboDevelopmentAreaUnit.ItemIndex);
//  result := ConvertFrom(AreaUnits, result);
//end;

function TfrmGwMound.GetDistanceToWaterTable: double;
var
  DistanceUnits: TConvType;
begin
  result := StrToFloatDef(rdeHeightAboveWaterTable.Text, 0);
  DistanceUnits := GetStandardLengthUnits(comboHeightAboveWaterTableUnits.ItemIndex);
  result := ConvertFrom(DistanceUnits, result);
end;

//function TfrmGwMound.GetRainfall: Double;
//var
//  StormUnits: TConvType;
//begin
//  result := StrToFloatDef(rdeDesignStorm.Text, 0);
//  StormUnits := GetLengthUnits(comboDesignStormUnits.ItemIndex);
//  result := ConvertFrom(StormUnits, result);
//end;

procedure TfrmGwMound.GetFilePositions(ModelMuseFile: TStringList;
  var FileIndexes: TFileIndexes; var FileConstants: TConstantFileValues);
var
  SearchTerm: string;
  PositionIndex: Integer;
  ValueLine: string;
  EqPos: Integer;
  AChar: Char;
begin
  AChar := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';

    SearchTerm := '      ScreenObject.Name = ''Center''';
    PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
    Assert(PositionIndex >= 0);

    Dec(PositionIndex, 3);
    ValueLine := Trim(ModelMuseFile[PositionIndex]);
    EqPos := Pos('=', ValueLine);
    Assert(EqPos > 0);
    ValueLine := Trim(Copy(ValueLine, EqPos+1));
    FileConstants.CenterX := StrToFloat(ValueLine);

    Inc(PositionIndex);
    ValueLine := Trim(ModelMuseFile[PositionIndex]);
    EqPos := Pos('=', ValueLine);
    Assert(EqPos > 0);
    ValueLine := Trim(Copy(ValueLine, EqPos+1));
    FileConstants.CenterY := StrToFloat(ValueLine);
  finally
    FormatSettings.DecimalSeparator := AChar;
  end;

  SearchTerm := '  ModflowPackages.RchPackage.IsSelected = True';
  FileIndexes.RchPkgPosition := ModelMuseFile.IndexOf(SearchTerm);
  if FileIndexes.RchPkgPosition < 0 then
  begin
    SearchTerm := '  ModflowPackages.RchPackage.IsSelected = False';
    FileIndexes.RchPkgPosition := ModelMuseFile.IndexOf(SearchTerm);
  end;
  Assert(FileIndexes.RchPkgPosition >= 0);

  SearchTerm := '  ModflowPackages.UzfPackage.IsSelected = False';
  FileIndexes.UZFPkgPosition := ModelMuseFile.IndexOf(SearchTerm);
  if FileIndexes.UZFPkgPosition < 0 then
  begin
    SearchTerm := '  ModflowPackages.UzfPackage.IsSelected = True';
    FileIndexes.UZFPkgPosition := ModelMuseFile.IndexOf(SearchTerm);
  end;
  Assert(FileIndexes.UZFPkgPosition >= 0);

  SearchTerm := '      Variable.Name = ''UzfRecharge''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.UzfRechargePostion := PositionIndex+2;
  ValueLine := ModelMuseFile[FileIndexes.UzfRechargePostion];
  Assert(Pos('      Variable.RealValue =', ValueLine) > 0);

  SearchTerm := '      ScreenObject.Name = ''Recharge64''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.PointsEnd := PositionIndex-1;
  FileIndexes.PointsStart := PositionIndex-21;
  Assert(ModelMuseFile[FileIndexes.PointsEnd] = '        end>');
  Assert(ModelMuseFile[FileIndexes.PointsStart] = '      Points = <');


  SearchTerm := '      ScreenObject.ModflowRchBoundary.Parameters = <';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.ParamEndTimePosition1 := PositionIndex + 5;
  ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition1];
  Assert(Pos('              EndTime = ', ValueLine) > 0);
  FileIndexes.ParamStartTimePosition2 := FileIndexes.ParamEndTimePosition1+4;
  ValueLine := ModelMuseFile[FileIndexes.ParamStartTimePosition2];
  Assert(Pos('              StartTime =', ValueLine) > 0);
  FileIndexes.ParamEndTimePosition2 := FileIndexes.ParamStartTimePosition2+1;
  ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition2];
  Assert(Pos('              EndTime =', ValueLine) > 0);


  SearchTerm := '      ScreenObject.ModflowUzfBoundary.Values = <';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.UzfEndTimePosition1 := PositionIndex + 2;
  ValueLine := ModelMuseFile[FileIndexes.UzfEndTimePosition1];
  Assert(Pos('          EndTime = ', ValueLine) > 0);
  FileIndexes.UzfStartTimePosition2 := FileIndexes.UzfEndTimePosition1+4;
  ValueLine := ModelMuseFile[FileIndexes.UzfStartTimePosition2];
  Assert(Pos('          StartTime =', ValueLine) > 0);
  FileIndexes.UzfEndTimePosition2 := FileIndexes.UzfStartTimePosition2+1;
  ValueLine := ModelMuseFile[FileIndexes.UzfEndTimePosition2];
  Assert(Pos('          EndTime =', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Brooks_Corey_Epsilon''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.BrooksCoreyPosition := PositionIndex-1;
  ValueLine := ModelMuseFile[FileIndexes.BrooksCoreyPosition];
  Assert(Pos('      DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Initial_Unsaturated_Water_Content''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.InitialUnsatPosition := PositionIndex-1;
  ValueLine := ModelMuseFile[FileIndexes.InitialUnsatPosition];
  Assert(Pos('      DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Maximum_Unsaturated_Vertical_K''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.MaxUnsatVKPosition := PositionIndex-1;
  ValueLine := ModelMuseFile[FileIndexes.MaxUnsatVKPosition];
  Assert(Pos('      DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Saturated_Water_Content''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  FileIndexes.SaturatedPosition := PositionIndex-1;
  ValueLine := ModelMuseFile[FileIndexes.SaturatedPosition];
  Assert(Pos('      DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '  ModflowSteadyParameters = <';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);

  FileIndexes.KhPosition := PositionIndex + 4;
  Assert(ModelMuseFile[FileIndexes.KhPosition-2] = '      ParameterName = ''HK_Par1''');
  ValueLine := ModelMuseFile[FileIndexes.KhPosition];
  Assert(Pos('Value = ', ValueLine) > 0);

  FileIndexes.KvPosition := FileIndexes.KhPosition + 8;
  Assert(ModelMuseFile[FileIndexes.KvPosition-2] = '      ParameterName = ''VK_Par1''');
  ValueLine := ModelMuseFile[FileIndexes.KvPosition];
  Assert(Pos('Value = ', ValueLine) > 0);

  FileIndexes.SyPosition := FileIndexes.KvPosition + 8;
  Assert(ModelMuseFile[FileIndexes.SyPosition-2] = '      ParameterName = ''SY_Par1''');
  ValueLine := ModelMuseFile[FileIndexes.SyPosition];
  Assert(Pos('Value = ', ValueLine) > 0);

  SearchTerm := '      ParameterName = ''RCH_Rate''';
  FileIndexes.RCH_PARPosition := ModelMuseFile.IndexOf(SearchTerm)+2;
  Assert(FileIndexes.RCH_PARPosition >= 2);
  ValueLine := ModelMuseFile[FileIndexes.RCH_PARPosition];
  Assert(Pos('Value = ', ValueLine) > 0);

  SearchTerm := '  ModflowStressPeriods = <';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.EndtimePosition1 := PositionIndex + 3;
  ValueLine := ModelMuseFile[FileIndexes.EndtimePosition1];
  Assert(Pos('EndTime = ', ValueLine) > 0);

  FileIndexes.FirstTimeStepPosition1 := FileIndexes.EndtimePosition1+1;
  ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition1];
  Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);
  EqPos := Pos('=', ValueLine);
  FileConstants.FirstStepLength := StrToFloat(Trim(Copy(ValueLine,EqPos+1,MAXINT)));

  FileIndexes.PeriodLengthPosition1 := FileIndexes.EndtimePosition1 +2;
  ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition1];
  Assert(Pos('PeriodLength = ', ValueLine) > 0);

  FileIndexes.TSMultPosition1 := FileIndexes.PeriodLengthPosition1 +2;
  ValueLine := ModelMuseFile[FileIndexes.TSMultPosition1];
  Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);


  FileIndexes.EndtimePosition2 := FileIndexes.EndtimePosition1 + 8;
  ValueLine := ModelMuseFile[FileIndexes.EndtimePosition2];
  Assert(Pos('EndTime = ', ValueLine) > 0);

  FileIndexes.FirstTimeStepPosition2 := FileIndexes.EndtimePosition2+1;
  ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition2];
  Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);

  FileIndexes.PeriodLengthPosition2 := FileIndexes.EndtimePosition2 +2;
  ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition2];
  Assert(Pos('PeriodLength = ', ValueLine) > 0);

  FileIndexes.TSMultPosition2 := FileIndexes.PeriodLengthPosition2 +3;
  ValueLine := ModelMuseFile[FileIndexes.TSMultPosition2];
  Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);

  FileIndexes.StartTimePos2 := FileIndexes.EndtimePosition2 +3;
  ValueLine := ModelMuseFile[FileIndexes.StartTimePos2];
  Assert(Pos('StartTime = ', ValueLine) > 0);




  SearchTerm := '      Variable.Name = ''BasinDepth''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.DepthPosition := PositionIndex +2;
  ValueLine := ModelMuseFile[FileIndexes.DepthPosition];
  Assert(Pos('Variable.RealValue =', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Model_Top''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.ModelTopPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.ModelTopPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay1Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer1BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer1BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay2Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer2BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer2BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay3Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer3BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer3BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay4Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer4BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer4BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay5Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer5BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer5BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

  SearchTerm := '      DataSet.Name = ''Lay6Bot_Bottom''';
  PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
  Assert(PositionIndex >= 0);
  FileIndexes.Layer6BottomPosition := PositionIndex -1;
  ValueLine := ModelMuseFile[FileIndexes.Layer6BottomPosition];
  Assert(Pos('DataSetFormula = ', ValueLine) > 0);

end;

//function TfrmGwMound.GetFractionImpervious: double;
//begin
//  result := StrToFloatDef(rdeImperviousFraction.Text, 0);
//end;
//
function TfrmGwMound.GetPolygon: TPolygon2D;
var
  Y: Double;
  Count: Integer;
  RowIndex: Integer;
  X: Double;
  LengthUnits: TConvType;
begin
  SetLength(result, rdgBasinCoordinates.RowCount - 1);
  Count := 0;
  LengthUnits := GetLengthUnits(comboCustomUnits.ItemIndex);
  for RowIndex := 1 to rdgBasinCoordinates.RowCount - 1 do
  begin
    if TryStrToFloat(rdgBasinCoordinates.Cells[0, RowIndex], X) and TryStrToFloat(rdgBasinCoordinates.Cells[1, RowIndex], Y) then
    begin
      X := ConvertFrom(LengthUnits, X);
      Y := ConvertFrom(LengthUnits, Y);
      result[Count].X := X;
      result[Count].Y := Y;
      Inc(Count);
    end;
  end;
  SetLength(result, Count);
end;

  function X2Min5(x: Double): double;
  begin
    result := Sqr(x) - 5;
  end;

procedure TfrmGwMound.btnAbortClick(Sender: TObject);
begin
  FAbortAnalytic := True;
end;

procedure TfrmGwMound.btnAbortNumericClick(Sender: TObject);
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
  end;
end;

procedure TfrmGwMound.btnBackClick(Sender: TObject);
begin
  if tvNavigation.Selected <> nil then
  begin
    tvNavigation.Selected := tvNavigation.Items[tvNavigation.Selected.Index-1];
  end;
end;

procedure TfrmGwMound.btnCopyAnalyticClick(Sender: TObject);
begin
  rdgAnalytic.CopyAllCellsToClipboard;
end;

procedure TfrmGwMound.btnCopyAnalyticChartClick(Sender: TObject);
begin
  chrtAnalytical.CopyToClipboardMetafile(True);
end;

procedure TfrmGwMound.btnCopyNumericChartClick(Sender: TObject);
begin
  chrtNumeric.CopyToClipboardMetafile(True);
end;

procedure TfrmGwMound.btnCopyNumericProfileClick(Sender: TObject);
begin
  rdgProfileNumeric.CopyAllCellsToClipboard;
end;

procedure TfrmGwMound.btnCopySummaryClick(Sender: TObject);
begin
  rdgSummaryNumeric.CopyAllCellsToClipboard;
end;

procedure TfrmGwMound.btnNextClick(Sender: TObject);
begin
  if tvNavigation.Selected <> nil then
  begin
    tvNavigation.Selected := tvNavigation.Items[tvNavigation.Selected.Index+1];
  end;
end;

procedure TfrmGwMound.btnRunHantushClick(Sender: TObject);
const
  MaxStep = 100;
  Epsilon = 1e-8;
var
  InitialHead: Double;
  RechargeRate: Double;
  Kx: Double;
  AquiferThickness: double;
  SpecificYield: Extended;
  SideLength: double;
  RectLength: double;
  RectWidth: double;
  HalfLength: double;
  HalfWidth: Double;
  Index: Integer;
  X: double;
  MoundHeight: Double;
  RechargeTime: double;
  MaxDistance: double;
  MaxDistanceUnits: TConvType;
//  MaxHeight: double;
//  PriorMoundHeight: Double;
//  MaxDuration: Double;
  SimulationLength: Double;
  SimulationTime: Double;
  LineSeries: TLineSeries;
  SimIncrement: Double;
  InifiltrationRadius: double;
  BasinShape: TBasinShape;
  CriteriaMet: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  MoundToHigh: boolean;
  DistanceToWaterTable: double;
  RiseToMuch: Boolean;
  Units: string;
  TimeUnit: TConvType;
  Distances: TRealList;
  BasinWidth: Double;
  DistIndex: Integer;
  ADistance: double;
begin
  Screen.Cursor := crHourGlass;
  FAbortAnalytic := False;
  rdgAnalytic.BeginUpdate;
  MoundToHigh := False;
  RiseToMuch := False;
  DistanceToWaterTable := GetDistanceToWaterTable;
  btnAbort.Enabled := True;
  try
    GetRechargeRateAndTime(RechargeTime, RechargeRate);
    SimulationLength := GetAnalyticSimulationLength;
    if SimulationLength < RechargeTime then
    begin
      Beep;
      if not (MessageDlg(StrTheSimulationLengt, mtWarning,
        [mbYes, mbNo], 0, mbNo) = mrYes) then
      begin
        Exit;
      end;

      RechargeTime := SimulationLength;
    end;
//    MaxDuration := GetMaxDuration;
//    if RechargeTime > MaxDuration then
//    begin
//      Beep;
//      MessageDlg(StrBecauseTheTimeReq, mtWarning, [mbOK], 0);
//      Exit;
//    end;

    Kx := GetKx;
    if Kx <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfHor, mtError, [mbOK], 0);
      Exit;
    end;

    AquiferThickness := GetAquiferThickness;
    if AquiferThickness <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfAqu, mtError, [mbOK], 0);
      Exit;
    end;
    InitialHead := AquiferThickness;
    SpecificYield := GetSpecificYield;
    if SpecificYield <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfSpe, mtError, [mbOK], 0);
      Exit;
    end;
    InifiltrationRadius := 0;
    HalfLength := 0;
    HalfWidth := 0;
    BasinShape := TBasinShape(rgBasinShape.ItemIndex);
    case BasinShape of
      bsSquare:
        begin
          SideLength := GetSquareSideLength;
          if SideLength <= 0 then
          begin
            Beep;
            MessageDlg(StrInvalidValueOfSiz, mtError, [mbOK], 0);
            Exit;
          end;
          HalfLength := SideLength/2;
          HalfWidth := HalfLength;
        end;
      bsRectangle:
        begin
          RectLength := GetRectangleLength;
          RectWidth := GetRectangleWidth;
          if (RectLength <= 0) or (RectWidth <= 0) then
          begin
            Beep;
            MessageDlg(StrInvalidValueOfSiz, mtError, [mbOK], 0);
            Exit;
          end;
          HalfLength := RectLength/2;
          HalfWidth := RectWidth/2;
        end;
      bsCircle:
        begin
          InifiltrationRadius := GetCircleDiameter/2;
          if InifiltrationRadius <= 0 then
          begin
            Beep;
            MessageDlg(StrInvalidValueOfSiz, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      bsCustom:
        begin
          Beep;
          MessageDlg(StrTheAnalyticSolutio, mtWarning, [mbOK], 0);
          Exit;
        end
      else Assert(False);
    end;
    MaxDistance := GetMaxDistance;
    if MaxDistance <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfMax, mtError, [mbOK], 0);
      Exit;
    end;

    Units := Format(' (%s)', [comboMaxDistanceUnits.Text]);

    chrtAnalytical.LeftAxis.Title.Caption := 'Change in Water Table' + Units;
    chrtAnalytical.BottomAxis.Title.Caption := 'Position' + Units;
    chrtAnalytical.LeftAxis.Automatic := True;

    Distances := TRealList.Create;
    try

      BasinWidth := GetBasinWidth;
      if BasinWidth > MaxDistance/2 then
      begin
        BasinWidth := MaxDistance/2
      end;
      for DistIndex := 0 to MaxStep do
      begin
        ADistance := DistIndex*BasinWidth*2/MaxStep;
        Distances.Add(ADistance);
      end;
      for DistIndex := 0 to MaxStep do
      begin
        ADistance := DistIndex*MaxDistance/MaxStep;
        if ADistance > BasinWidth*2  then
        begin
          Distances.Add(ADistance);
        end;
      end;

    
      seriesAnayltical.Clear;
      FTimeSeries.Clear;
      Application.ProcessMessages;
      seriesAnayltical.BeginUpdate;
      try
        chrtAnalytical.LeftAxis.Automatic := True;
        chrtAnalytical.BottomAxis.Automatic := True;
        MaxDistanceUnits := GetStandardLengthUnits(comboMaxDistanceUnits.ItemIndex);
//        rdgAnalytic.ColCount := MaxStep+1;
        rdgAnalytic.RowCount := Distances.Count+1;
        rdgAnalytic.ColCount := 2;
        for ColIndex := 0 to rdgAnalytic.ColCount - 1 do
        begin
          rdgAnalytic.Columns[ColIndex].AutoAdjustColWidths := True;
        end;
        rdgAnalytic.Cells[0,0] := 'Distance \ Time';
        rdgAnalytic.Cells[1,0] := FloatToStr(RechargeTime);
//        for Index := 0 to MaxStep do
        for Index := 0 to Distances.Count-1 do
        begin
          if FAbortAnalytic then
          begin
            Exit;
          end;
          X := Distances[Index];
//          X := Index/MaxStep*MaxDistance;
          MoundHeight:= 0;
  //        InifiltrationRadius := 0;
          case BasinShape of
            bsSquare, bsRectangle:
              begin
                MoundHeight := IterateHantushRectangular(InitialHead,
                  RechargeRate, Kx, AquiferThickness, SpecificYield, RechargeTime,
                  RechargeTime, HalfLength, HalfWidth, X, 0);
                CriteriaMet := True;
              end;
            bsCircle:
              begin
                CriteriaMet := True;
                MoundHeight := HantushCircular(InitialHead, RechargeRate, Kx,
                  AquiferThickness, SpecificYield, RechargeTime, RechargeTime,
                  InifiltrationRadius, X, CriteriaMet);
              end;
            else Assert(False);
          end;
          MoundHeight := MoundHeight - InitialHead;
          X := ConvertTo(X, MaxDistanceUnits);
          if MoundHeight >= DistanceToWaterTable then
          begin
            MoundToHigh := True;
          end;
          if MoundHeight > AquiferThickness/2 then
          begin
            RiseToMuch := True;
          end;
          MoundHeight := ConvertTo(MoundHeight, MaxDistanceUnits);
          rdgAnalytic.Cells[0,Index+1] := FloatToStr(X);
          if CriteriaMet then
          begin
            seriesAnayltical.AddXY(X, MoundHeight);
            rdgAnalytic.Cells[1,Index+1] := FloatToStr(MoundHeight);
          end
          else
          begin
//            rdgAnalytic.Cells[0,Index+1] := '';
            rdgAnalytic.Cells[1,Index+1] := '';
          end;
        end;
      finally
        seriesAnayltical.EndUpdate;
        chrtAnalytical.Invalidate;
        Application.ProcessMessages;
      end;
      if SimulationLength = RechargeTime then
      begin
        Exit;
      end;

      SimulationLength := GetAnalyticSimulationLength;
      SimIncrement := GetSimulationIncrement;
      SimulationTime := 0;
      RowIndex := 1;
      while SimulationTime < SimulationLength*(1-Epsilon) do
      begin
        if FAbortAnalytic then
        begin
          Exit;
        end;
        SimulationTime := SimulationTime + SimIncrement;
        if SimulationTime < RechargeTime then
        begin
          Continue;
        end;
        if SimulationTime > SimulationLength then
        begin
          SimulationTime := SimulationLength
        end;
        Inc(RowIndex);
        rdgAnalytic.ColCount := RowIndex+1;
        rdgAnalytic.Columns[RowIndex].AutoAdjustColWidths := True;
        rdgAnalytic.Cells[RowIndex,0] := FloatToStr(SimulationTime);

        LineSeries := TLineSeries.Create(nil);
        FTimeSeries.Add(LineSeries);
        LineSeries.ParentChart := chrtAnalytical;
        chrtAnalytical.AddSeries(LineSeries);
        TimeUnit := GetTimeUnit(comboTimeIncrementUnits.ItemIndex);
        LineSeries.Title := FloatToStr(ConvertTo(SimulationTime, TimeUnit))
          + ' ' + comboTimeIncrementUnits.Text;
        LineSeries.BeginUpdate;
        try
//          for Index := 0 to MaxStep do
          for Index := 0 to Distances.Count-1 do
          begin
          if FAbortAnalytic then
          begin
            Exit;
          end;
            X := Distances[Index];
            MoundHeight := 0;
//            X := Index/MaxStep*MaxDistance;
            case BasinShape of
              bsSquare, bsRectangle:
                begin
                  MoundHeight := IterateHantushRectangular(InitialHead,
                    RechargeRate, Kx, AquiferThickness, SpecificYield,
                    RechargeTime, SimulationTime, HalfLength, HalfWidth, X, 0);
                  CriteriaMet := True;
                end;
              bsCircle:
                begin
                CriteriaMet := True;
                MoundHeight := HantushCircular(InitialHead, RechargeRate, Kx,
                  AquiferThickness, SpecificYield, RechargeTime, SimulationTime,
                  InifiltrationRadius, X, CriteriaMet);
                end;
              else Assert(False);
            end;

            MoundHeight := MoundHeight - InitialHead;
            X := ConvertTo(X, MaxDistanceUnits);
            MoundHeight := ConvertTo(MoundHeight, MaxDistanceUnits);
            if CriteriaMet then
            begin
              LineSeries.AddXY(X, MoundHeight);
              rdgAnalytic.Cells[RowIndex, Index+1] := FloatToStr(MoundHeight);
            end
            else
            begin
              rdgAnalytic.Cells[RowIndex, Index+1] := '';
            end;
          end;
        finally
          LineSeries.EndUpdate;
          chrtAnalytical.Invalidate;
          Application.ProcessMessages;
        end;
      end;

    finally
      Distances.Free;
    end;

  finally
    rdgAnalytic.EndUpdate;
    UpdateNodeStateIndex;
    Screen.Cursor := crDefault;
    if MoundToHigh then
    begin
      DisplayMoundToHighMesssage;
      chrtAnalytical.Color := clRed;
    end
    else
    begin
      chrtAnalytical.Color := clWhite;
      if RiseToMuch then
      begin
        DisplayRiseToHigh;
      end;
    end;
    btnAbort.Enabled := False;
    pgcAnalytic.ActivePage := tabAnalyticGraph;
  end;
end;

procedure TfrmGwMound.btnRunNumericModelClick(Sender: TObject);
  function CheckFile(FileName: string): boolean;
  begin
    result := FileExists(FileName);
    if not result then
    begin
      Beep;
      MessageDlg(Format(StrSDoesNotExist, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
var
  ModelMuseFile: TStringList;
  FileIndexes: TFileIndexes;
  FileConstants: TConstantFileValues;
  SimValues: TSimValues;
//  AThread: TRunModelThread;
  Directory: string;
  Units: string;
  RechargeTime: Double;
  RechargeRate: Double;
//  DistanceToWaterTable: double;
//  MaxDuration: Double;
begin
//  DistanceToWaterTable := GetDistanceToWaterTable;
//  if DistanceToWaterTable <= 0 then
//  begin
//    Beep;
//    MessageDlg(StrInvalidValueOfDisWatTable, mtError, [mbOK], 0);
//    Exit;
//  end;
  GetRechargeRateAndTime(RechargeTime, RechargeRate);
  if RechargeRate <= 0 then
  begin
    Beep;
    MessageDlg(StrInvalidValueOfVer, mtError, [mbOK], 0);
    Exit;
  end;
  if GetBasinArea <= 0 then
  begin
    Beep;
    MessageDlg(StrInvalidValueOfSiz, mtError, [mbOK], 0);
    Exit;
  end;
//  MaxDuration := GetMaxDuration;
//  if RechargeTime > MaxDuration then
//  begin
//    Beep;
//    MessageDlg(StrBecauseTheTimeReq, mtWarning, [mbOK], 0);
//    Exit;
//  end;

  if not CheckFile(feMODFLOW.FileName)
    or not CheckFile(feModelMuseApplication.FileName) then
//    or not CheckFile(feModelMuseFile.FileName) then
  begin
    Exit
  end;
  if feModelMuseFile.FileName <> '' then
  begin
    if not CheckFile(feModelMuseFile.FileName) then
    begin
      Exit;
    end;
  end;
  ModelMuseFile := TStringList.Create;
  try
    if feModelMuseFile.FileName <> '' then
    begin
      ModelMuseFile.LoadFromFile(feModelMuseFile.FileName);
    end
    else if cbSimulateUnsat.Checked then
    begin
      ModelMuseFile.Assign(jvstringsMMFile.MultipleStrings.Items[1].Strings);
    end
    else
    begin
      ModelMuseFile.Assign(jvstringsMMFile.MultipleStrings.Items[0].Strings);
    end;
    GetFilePositions(ModelMuseFile, FileIndexes, FileConstants);

    FileConstants.ModflowLocation := feMODFLOW.FileName;
    FileConstants.ModelMuseLocation := feModelMuseApplication.FileName;

    GetModelVariables(SimValues);
    if SimValues.DepthToWaterTable <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfDisWatTable, mtError, [mbOK], 0);
      Exit;
    end;
    if SimValues.SimTime >= SimValues.TotalSimTime then
    begin
      Beep;
      MessageDlg(StrInvalidSimulationT, mtError, [mbOK], 0);
      Exit;
    end;
    if SimValues.RechargeRate <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfVer, mtError, [mbOK], 0);
      Exit;
    end;
    if SimValues.Kx <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfHor, mtError, [mbOK], 0);
      Exit;
    end;
    if SimValues.SpecificYield <= 0 then
    begin
      Beep;
      MessageDlg(StrInvalidValueOfSpe, mtError, [mbOK], 0);
      Exit;
    end;
    if SimValues.UnSaturatedFlow then
    begin
      if SimValues.BrooksCoreyEpsilon <= 0 then
      begin
        Beep;
        MessageDlg(StrInvalidValueOfBro, mtError, [mbOK], 0);
        Exit;
      end;
      if SimValues.SaturatedWaterContent <= 0 then
      begin
        Beep;
        MessageDlg(StrInvalidValueOfSat, mtError, [mbOK], 0);
        Exit;
      end;
      if SimValues.InitialWaterContent <= 0 then
      begin
        Beep;
        MessageDlg(StrInvalidValueOfIn, mtError, [mbOK], 0);
        Exit;
      end;
    end;


    SetRechargePoints(ModelMuseFile, FileIndexes, FileConstants);

    Directory := IncludeTrailingPathDelimiter(
      GetMyDocuments) + 'GwMounding_Numerical_Models';
    btnAbortNumeric.Enabled := True;

    FThread := TRunModelThread.Create(Directory, ModelMuseFile, FileIndexes,
      SimValues, FileConstants, seriesNumeric,
      GetStandardLengthUnits(comboMaxNumericDistanceUnits.ItemIndex),
      GetTimeUnit(comboSimulationLengthUnitsNumeric.ItemIndex),
      rdgSummaryNumeric, rdgProfileNumeric);

    FThread.Start;

  finally
    ModelMuseFile.Free;
  end;
  UpdateNodeStateIndex;

  Units := Format(' (%s)', [comboMaxNumericDistanceUnits.Text]);

  chrtNumeric.LeftAxis.Title.Caption := 'Maximum Rise in Water Table' + Units;
  chrtNumeric.BottomAxis.Title.Caption := 'Position' + Units;
  pgcNumeric.ActivePage := tabGraphNumeric;

end;

procedure TfrmGwMound.btnCustomBasinClick(Sender: TObject);
var
  MyPoints: TPointList;
  RowIndex: Integer;
  XValue: double;
  YValue: double;
  APoint: TPoint2D;
begin
  MyPoints := TPointList.Create;
  try
    MyPoints.Capacity := rdgBasinCoordinates.RowCount-1;
    for RowIndex := 1 to rdgBasinCoordinates.RowCount - 1 do
    begin
      if TryStrToFloat(rdgBasinCoordinates.Cells[0, RowIndex], XValue)
        and TryStrToFloat(rdgBasinCoordinates.Cells[1, RowIndex], YValue) then
      begin
        APoint.X := XValue;
        APoint.Y := YValue;
        MyPoints.Add(APoint);
      end;
    end;
    frmCustomBasin.GetData(MyPoints);
    frmCustomBasin.ShowModal;
  finally
    MyPoints.Free;
  end;
end;

procedure TfrmGwMound.SetDefaultUnitsClick(Sender: TObject);
var
  DefaultUnits: TStringList;
  List: TList<TComboBox>;
  ComboIndex: Integer;
  ACombo: TComboBox;
  ItemIndex: Integer;
begin
  inherited;
  if Sender = btnMetric then
  begin
    DefaultUnits := FDefaultMetricUnits;
  end
  else
  begin
    Assert(Sender = btnEnglish);
    DefaultUnits := FEnglishMetricUnits;
  end;
  List := TList<TComboBox>.Create;
  try
    List.Add(comboMaxDepthUnits);
    List.Add(comboMinBasinAreaUnits);
    List.Add(comboSquareLengthUnits);
    List.Add(comboBasinCircleDiameterUnits);
    List.Add(comboRectUnits);
    List.Add(comboCustomUnits);
    List.Add(comboKzUnits);
    List.Add(comboHeightAboveWaterTableUnits);
    List.Add(comboAquiferThicknessUnits);
    List.Add(comboMaxDistanceUnits);
    List.Add(comboMaxNumericDistanceUnits);
    for ComboIndex := 0 to List.Count - 1 do
    begin
      ACombo := List[ComboIndex];
      for ItemIndex := 0 to ACombo.Items.Count - 1 do
      begin
        if DefaultUnits.IndexOf(ACombo.Items[ItemIndex])>=0 then
        begin
          if ACombo.ItemIndex <> ItemIndex then
          begin
            ACombo.ItemIndex := ItemIndex;
            if Assigned(ACombo.OnChange) then
            begin
              ACombo.OnChange(ACombo);
            end;
          end;
          break;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmGwMound.cbSimulateUnsatClick(Sender: TObject);
begin
  rdeBrooksCoreyEpsilon.Enabled := cbSimulateUnsat.Checked;
  rdeSaturatedWaterFraction.Enabled := cbSimulateUnsat.Checked;
  rdeInitialWaterContent.Enabled := cbSimulateUnsat.Checked;
  EraseNumericResults;
  UpdateNodeStateIndex;
end;

procedure TfrmGwMound.comboAquiferThicknessUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.comboBasinCircleDiameterUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

procedure TfrmGwMound.comboCustomUnitsChange(Sender: TObject);
begin
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

procedure TfrmGwMound.comboDesignStormUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateEdVolume;
end;

procedure TfrmGwMound.comboDevelopmentAreaUnitChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateEdVolume;
end;

procedure TfrmGwMound.comboDurationChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.comboDurationOfInfiltrationChange(Sender: TObject);
var
  OldTimeUnit: TConvType;
  NewTimeUnit: TConvType;
  InfiltrationTime: double;
begin
  inherited;
  if FIniltrationTimeUnits <> comboDurationOfInfiltration.ItemIndex then
  begin
    if TryStrToFloat(rdeDurationOfInfiltration.Text, InfiltrationTime) then
    begin
      OldTimeUnit := GetTimeUnit(FIniltrationTimeUnits);
      NewTimeUnit := GetTimeUnit(comboDurationOfInfiltration.ItemIndex);
      InfiltrationTime := Convert(InfiltrationTime, OldTimeUnit, NewTimeUnit);
      rdeDurationOfInfiltration.Min := 0;
      rdeDurationOfInfiltration.Text := FloatToStr(InfiltrationTime);
      SetMinForInfiltrationTime;
    end;
  end;
  FIniltrationTimeUnits := comboDurationOfInfiltration.ItemIndex;
  EraseAnalyticResults;
  EraseNumericResults;

end;

procedure TfrmGwMound.comboDurationOfInfiltrationEnter(Sender: TObject);
begin
  inherited;
  ControlEnter(Sender);
  FIniltrationTimeUnits := comboDurationOfInfiltration.ItemIndex;
end;

procedure TfrmGwMound.comboHeightAboveWaterTableUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.comboKzUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateInfiltrationTime;
  ShowKxUnits;
//  UpdateEdMinBasinArea;
end;

procedure TfrmGwMound.comboMaxDepthUnitsChange(Sender: TObject);
begin
//  UpdateEdMinBasinArea;
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.comboMaxDistanceUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
end;

procedure TfrmGwMound.comboMaxNumericDistanceUnitsChange(Sender: TObject);
begin
  EraseNumericResults;
end;

procedure TfrmGwMound.comboMinBasinAreaUnitsChange(Sender: TObject);
begin
  UpdateDisplayedBasinArea;
//  UpdateEdMinBasinArea;
end;

procedure TfrmGwMound.comboRectUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

procedure TfrmGwMound.comboSimulationLengthUnitsAnalyticChange(Sender: TObject);
begin
  EraseAnalyticResults;
  plMainChange(nil);
end;

procedure TfrmGwMound.comboSimulationLengthUnitsNumericChange(Sender: TObject);
begin
  inherited;
  EraseNumericResults;
  plMainChange(nil);
end;

procedure TfrmGwMound.comboSquareLengthUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

procedure TfrmGwMound.comboTimeIncrementUnitsChange(Sender: TObject);
begin
  EraseAnalyticResults;
end;

procedure TfrmGwMound.comboVolumeUnitsChange(Sender: TObject);
begin
  UpdateEdVolume;
  EraseAnalyticResults;
  EraseNumericResults;
end;

function TfrmGwMound.EquivalentRadius(Radius: double; Count: integer): double;
var
  Angle: double;
begin
  Angle := Pi/Count;
  Result := Radius*Sqrt(Pi/(Count*Sin(Angle)*Cos(Angle)));
end;

procedure TfrmGwMound.EraseAnalyticResults;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  seriesAnayltical.Clear;
  FTimeSeries.Clear;
  if rdgAnalytic.Cells[1,1] <> '' then
  begin
    for RowIndex := rdgAnalytic.FixedRows to rdgAnalytic.RowCount - 1 do
    begin
      for ColIndex := rdgAnalytic.FixedCols to rdgAnalytic.ColCount - 1 do
      begin
        rdgAnalytic.Cells[ColIndex,RowIndex] := '';
      end;
    end;
  end;
  UpdateNodeStateIndex;
end;

procedure TfrmGwMound.EraseNumericResults;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  seriesNumeric.Clear;
  if rdgProfileNumeric.Cells[0,1] <> '' then
  begin
    rdgProfileNumeric.BeginUpdate;
    try
      for RowIndex := rdgProfileNumeric.FixedRows to rdgProfileNumeric.RowCount - 1 do
      begin
        for ColIndex := rdgProfileNumeric.FixedCols to rdgProfileNumeric.ColCount - 1 do
        begin
          rdgProfileNumeric.Cells[ColIndex,RowIndex] := '';
        end;
      end;
    finally
      rdgProfileNumeric.EndUpdate;
    end;
  end;

  rdgSummaryNumeric.BeginUpdate;
  try
    for RowIndex := rdgSummaryNumeric.FixedRows to
      rdgSummaryNumeric.RowCount - 1 do
    begin
      rdgProfileNumeric.Cells[1,RowIndex] := '';
    end;
  finally
    rdgSummaryNumeric.EndUpdate;
  end;

  UpdateNodeStateIndex;
end;

procedure TfrmGwMound.UpdateNodeStateIndex;
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;

  if tvNavigation.Selected <> nil then
  begin
    tvNavigation.Selected.StateIndex := siYes;
  end;

{$IFDEF UNSAT}
  if cbSimulateUnsat.Checked then
  begin
    FUnsatNode.StateIndex := siYes;
  end
  else
  begin
    FUnsatNode.StateIndex := siNo;
  end;
{$ENDIF}

  if rdgAnalytic.Cells[1,1] = '' then
  begin
    FRunAnalytic.StateIndex := siNo;
  end
  else
  begin
    FRunAnalytic.StateIndex := siYes;
  end;

  if rdgProfileNumeric.Cells[1,1] = '' then
  begin
    FRunNumeric.StateIndex := siNo;
  end
  else
  begin
    FRunNumeric.StateIndex := siYes;
  end;
end;

function TfrmGwMound.GetHelpUrl(Keyword: string): string;
var
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));

{$IFDEF CHM}
  FileName := FileName + 'Help\GwMounding.chm';
  FileName := FileName + '::/gwmound.htm';
  result := 'mk:@MSITStore:' + FileName;
{$ELSE}
  FileName := FileName + 'Help\';
  FileName := FileName + 'gwmound.htm';
  result := 'file:\\' + FileName;
{$ENDIF}
  if Keyword <> '' then
  begin
    result := result + '#' + Keyword;
  end;
end;

procedure TfrmGwMound.DisplayMoundToHighMesssage;
begin
  Beep;
  MessageDlg(StrBecauseTheRiseIn, mtWarning, [mbOK], 0);
end;

procedure TfrmGwMound.DisplayRiseToHigh;
begin
  Beep;
  MessageDlg(StrRiseToHigh, mtWarning, [mbOK], 0);
end;

procedure TfrmGwMound.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmGwMound.feModelMuseApplicationChange(Sender: TObject);
begin
  if FileExists(feModelMuseApplication.FileName) then
  begin
    feModelMuseApplication.Color := clWindow;
  end
  else
  begin
    feModelMuseApplication.Color := clRed;
  end;
end;

procedure TfrmGwMound.feModelMuseFileChange(Sender: TObject);
begin
  if FileExists(feModelMuseFile.FileName) or (feModelMuseFile.FileName = '') then
  begin
    feModelMuseFile.Color := clWindow;
  end
  else
  begin
    feModelMuseFile.Color := clRed;
  end;

end;

procedure TfrmGwMound.feMODFLOWChange(Sender: TObject);
begin
  if FileExists(feMODFLOW.FileName) then
  begin
    feMODFLOW.Color := clWindow;
  end
  else
  begin
    feMODFLOW.Color := clRed;
  end;
end;

resourcestring
  StrModelMuseRegKey = 'SOFTWARE\Classes\ModelMuseTextFile\shell\open\command';

function GetModelMuseLocation: string;
var
  Reg: TRegistry;
  QuotePos: Integer;
  Value: string;
begin
  result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.KeyExists(StrModelMuseRegKey) then
    begin
      Reg.OpenKey(StrModelMuseRegKey, False);
      try
        Value := Copy(Reg.ReadString(''), 2, MaxInt);
        QuotePos := Pos('"', Value);
        if QuotePos > 0 then
        begin
          result := Copy(Value, 1, QuotePos - 1);
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TfrmGwMound.DefineDefaultUnits;
begin
  FDefaultMetricUnits.Clear;
  FEnglishMetricUnits.Clear;

  FDefaultMetricUnits.Add('m');
  FEnglishMetricUnits.Add('feet');

  FDefaultMetricUnits.Add('square m');
  FEnglishMetricUnits.Add('square feet');

  FDefaultMetricUnits.Add(ConvTypeToDescription(vuMetersPerSecond));
  FEnglishMetricUnits.Add(ConvTypeToDescription(vuFeetPerSecond));

{
ConvTypeToDescription(vuFeetPerSecond)
ConvTypeToDescription(vuMetersPerSecond)

}

end;

procedure TfrmGwMound.RemoveSimLengthWarningColor;
var
  Simlength: Extended;
begin
  Simlength := StrToFloat(rdeSimulationLengthNumeric.Text);
  if Simlength > rdeSimulationLengthNumeric.Min then
  begin
    rdeSimulationLengthNumeric.Color := clWindow;
  end;
end;

procedure TfrmGwMound.FormCreate(Sender: TObject);
var
  IniFile: TMemIniFile;
  MMLocation: string;
begin
  FDefaultMetricUnits := TStringList.Create;
  FEnglishMetricUnits := TStringList.Create;
  DefineDefaultUnits;
  {$IFNDEF CustomShape}
  While rgBasinShape.Items.Count > 3 do
  begin
    rgBasinShape.Items.Delete(rgBasinShape.Items.Count-1);
  end;
  {$ENDIF}

  {$IFNDEF CustomTemplate}
  lblModelMuseFile.Visible := False;
  feModelMuseFile.Visible := False;
  {$ENDIF}

//  tvNavigation.Items.AddChildObject(nil, 'Development', jvspDevelopment);
  tvNavigation.Items.AddChildObject(nil, 'Basin Design', jvspBasinDesign);
  tvNavigation.Items.AddChildObject(nil, 'Aquifer Properties', jvspAquiferProperties);
  FRunAnalytic := tvNavigation.Items.AddChildObject(nil, 'Run Analytical Model', jvspRunAnalyticalModel);
{$IFDEF UNSAT}
  FUnsatNode := tvNavigation.Items.AddChildObject(nil, 'Unsaturated Flow', jvspUnsaturatedFlow);
{$ENDIF}
  tvNavigation.Items.AddChildObject(nil, 'Program Locations', jvspProgramLocations);
  FRunNumeric := tvNavigation.Items.AddChildObject(nil, 'Run Numerical Model', jvspRunNumeric);


  FTimeSeries := TObjectList.Create;
  InitializeComboKzUnits;

  rgBasinShapeClick(nil);
  UpdateEdVolume;

  // Get a default value for the location of ModelMuse.
  MMLocation := GetModelMuseLocation;
  if (MMLocation = '') or not FileExists(MMLocation) then
  begin
    MMLocation := StrModelMuseLocation;
  end;

  IniFile := TMemIniFile.Create(IniFileName);
  try
    feMODFLOW.FileName :=  IniFile.ReadString(StrProgramLocations, StrMODFLOW,
      StrModflowLocation);
    feModelMuseApplication.FileName := IniFile.ReadString(StrProgramLocations,
      StrModelMuse, MMLocation);
  finally
    IniFile.Free;
  end;

  // If retrieved value of the ModelMuse location is wrong, use the default
  // location.
  if not FileExists(feModelMuseApplication.FileName) then
  begin
    feModelMuseApplication.FileName := MMLocation
  end;

  plMain.ActivePageIndex := 0;
  pgcAnalytic.ActivePageIndex := 0;
  pgcNumeric.ActivePageIndex := 0;

  rdgBasinCoordinates.Cells[0,0] := 'X';
  rdgBasinCoordinates.Cells[1,0] := 'Y';
  ShowKxUnits;
  FIniltrationTimeUnits := comboDurationOfInfiltration.ItemIndex;
  UpdateInfiltrationTime;

end;

procedure TfrmGwMound.FormDestroy(Sender: TObject);
var
  IniFile: TMemIniFile;
begin
  FTimeSeries.Free;

  IniFile := TMemIniFile.Create(IniFileName);
  try
    IniFile.WriteString(StrProgramLocations, StrMODFLOW, feMODFLOW.FileName);
    IniFile.WriteString(StrProgramLocations, StrModelMuse, feModelMuseApplication.FileName);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
  FDefaultMetricUnits.Free;
  FEnglishMetricUnits.Free;
end;

procedure TfrmGwMound.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  ANode: TTreeNode;
begin
  inherited;
  ANode := tvNavigation.Selected.GetNext;
  if ANode <> nil then
  begin
    tvNavigation.Selected := ANode;
  end;
end;

procedure TfrmGwMound.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  ANode: TTreeNode;
begin
  inherited;
  ANode := tvNavigation.Selected.GetPrev;
  if ANode <> nil then
  begin
    tvNavigation.Selected := ANode;
  end;
end;

//function TfrmGwMound.MinimumBasinArea: double;
//var
//  Kz: double;
//  RechVol: Double;
//  MaxDepth: Double;
//  Duration: double;
//begin
//  result := 0;
//  if csLoading in ComponentState then
//  begin
//    Exit;
//  end;
//  Kz := GetKz;
//  MaxDepth := GetMaxDepth;
//  Duration := GetMaxDuration;
//  if (Kz = 0) or (MaxDepth = 0) or (Duration = 0) then
//  begin
//    Exit;
//  end;
//
//  RechVol := GetRechargeVolume;
//
//  result := Max(RechVol/(Kz*Duration), RechVol/MaxDepth);
//end;

procedure TfrmGwMound.UpdateAnalyticGraph;
var
  RowIndex: Integer;
  Distances: array of double;
  Value: Extended;
  ColIndex: Integer;
  LineSeries: TLineSeries;
  AValue: Extended;
  SimulationTime: double;
  TimeUnit: TConvType;
begin
  if rdgAnalytic.Cells[1,1] <> '' then
  begin
    SetLength(Distances, rdgAnalytic.RowCount-1);
    FTimeSeries.Clear;
    seriesAnayltical.BeginUpdate;
    try
      seriesAnayltical.Clear;
      for RowIndex := 1 to rdgAnalytic.RowCount - 1 do
      begin
        if TryStrToFloat(rdgAnalytic.Cells[0,RowIndex], AValue) then
        begin
          Distances[RowIndex-1] := AValue;
          if TryStrToFloat(rdgAnalytic.Cells[1,RowIndex], Value) then
          begin
            seriesAnayltical.AddXY(Distances[RowIndex-1], Value)
          end;
        end;
      end;
    finally
      seriesAnayltical.EndUpdate
    end;

    for ColIndex := 2 to rdgAnalytic.ColCount - 1 do
    begin
      LineSeries := TLineSeries.Create(nil);

      SimulationTime := StrToFloat(rdgAnalytic.Cells[ColIndex,0]);

      TimeUnit := GetTimeUnit(comboTimeIncrementUnits.ItemIndex);
      LineSeries.Title := FloatToStr(ConvertTo(SimulationTime, TimeUnit))
        + ' ' + comboTimeIncrementUnits.Text;


      FTimeSeries.Add(LineSeries);
      LineSeries.ParentChart := chrtAnalytical;
      chrtAnalytical.AddSeries(LineSeries);
      LineSeries.BeginUpdate;
      try
        for RowIndex := 1 to rdgAnalytic.RowCount - 1 do
        begin
          if TryStrToFloat(rdgAnalytic.Cells[ColIndex, RowIndex], Value) then
          begin
            LineSeries.AddXY(Distances[RowIndex-1], Value)
          end;
        end;
      finally
        LineSeries.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmGwMound.UpdateNumericGraph;
var
  RowIndex: Integer;
  XValue: Extended;
  YValue: Extended;
begin
  if rdgProfileNumeric.Cells[0,1] <> '' then
  begin
    seriesNumeric.BeginUpdate;
    try
      seriesNumeric.Clear;
      for RowIndex := 1 to rdgProfileNumeric.RowCount - 1 do
      begin
        XValue := StrToFloat(rdgProfileNumeric.Cells[0,RowIndex]);
        YValue := StrToFloat(rdgProfileNumeric.Cells[1,RowIndex]);
        seriesNumeric.AddXY(XValue, YValue);
      end;
      rdeMaxDistanceNumericChange(nil);
    finally
      seriesNumeric.EndUpdate
    end;
  end;
end;

procedure TfrmGwMound.Open1Click(Sender: TObject);
var
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  GwMound: TGwMoundFile;
begin
  if dlgOpenFile.Execute then
  begin
    rdeDurationOfInfiltration.Min := 0;
    FileStream := TFileStream.Create(dlgOpenFile.FileName,
      fmOpenRead or fmShareDenyWrite);
    try
      MemStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(FileStream, MemStream);
        MemStream.Position := 0;
        GwMound := TGwMoundFile.Create(nil);
        try
          MemStream.ReadComponent(GwMound);
        finally
          GwMound.Free;
        end;
      finally
        MemStream.Free;
      end;
    finally
      FileStream.Free;
    end;
    dlgSaveFile.FileName := dlgOpenFile.FileName;
    UpdateEdVolume;
    SetMinForInfiltrationTime;

    UpdateAnalyticGraph;
    UpdateNumericGraph;

  end;
end;

procedure TfrmGwMound.pgcAnalyticChange(Sender: TObject);
var
  Url: string;
begin
  inherited;
  Url := GetHelpUrl(plMain.ActivePage.HelpKeyword);
  wbHelp.Navigate(Url);
end;

procedure TfrmGwMound.pgcNumericChange(Sender: TObject);
var
  Url: string;
begin
  inherited;
  Url := GetHelpUrl(plMain.ActivePage.HelpKeyword);
  wbHelp.Navigate(Url);
end;

procedure TfrmGwMound.plMainChange(Sender: TObject);
var
  RTime: Double;
  RRate: Double;
  TimeUnit: TConvType;
  Simlength: Extended;
begin
  if plMain.ActivePage = jvspRunAnalyticalModel then
  begin
    GetRechargeRateAndTime(RTime, RRate);
    TimeUnit := GetTimeUnit(comboSimulationLengthUnitsAnalytic.ItemIndex);
//    if RTime > GetMaxDuration then
//    begin
//      lblDurationOfInfiltrationAnalytic.Font.Style :=
//        lblDurationOfInfiltrationAnalytic.Font.Style + [fsBold];
//      lblDurationOfInfiltrationAnalytic.Font.Color := clRed;
//    end
//    else
//    begin
//      lblDurationOfInfiltrationAnalytic.Font.Style :=
//        lblDurationOfInfiltrationAnalytic.Font.Style - [fsBold];
//      lblDurationOfInfiltrationAnalytic.Font.Color := clBlack;
//    end;
    RTime := ConvertTo(RTime, TimeUnit);

    lblDurationOfInfiltrationAnalytic.Caption := 'Duration of infiltration = '
      + FloatToStr(RTime) + ' ' + comboSimulationLengthUnitsAnalytic.Text;
  end
  else if plMain.ActivePage = jvspRunNumeric then
  begin
    GetRechargeRateAndTime(RTime, RRate);
    TimeUnit := GetTimeUnit(comboSimulationLengthUnitsNumeric.ItemIndex);
//    if RTime > GetMaxDuration then
//    begin
//      lblDurationOfInfiltrationNumeric.Font.Style :=
//        lblDurationOfInfiltrationNumeric.Font.Style + [fsBold];
//      lblDurationOfInfiltrationNumeric.Font.Color := clRed;
//    end
//    else
//    begin
//      lblDurationOfInfiltrationNumeric.Font.Style :=
//        lblDurationOfInfiltrationNumeric.Font.Style - [fsBold];
//      lblDurationOfInfiltrationNumeric.Font.Color := clBlack;
//    end;
    RTime := ConvertTo(RTime, TimeUnit);
    Simlength := StrToFloat(rdeSimulationLengthNumeric.Text);
    if Simlength <= RTime*(1+ 1e-8) then
    begin
      rdeSimulationLengthNumeric.Text := FloatToStr(RTime);
      rdeSimulationLengthNumeric.Color := clRed;
    end
    else
    begin
      RemoveSimLengthWarningColor;
    end;
    rdeSimulationLengthNumeric.Min := RTime;
    lblDurationOfInfiltrationNumeric.Caption := 'Duration of infiltration = '
      + FloatToStr(RTime) + ' ' + comboSimulationLengthUnitsNumeric.Text;
  end;
end;

procedure TfrmGwMound.rdeAquiferThicknessChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeBasinCircleDiameterChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeBrooksCoreyEpsilonClick(Sender: TObject);
begin
  EraseNumericResults;
end;

procedure TfrmGwMound.rdeDesignStormChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateEdVolume;
end;

procedure TfrmGwMound.rdeDevelopmentAreaChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateEdVolume;
end;

procedure TfrmGwMound.rdeDurationChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.rdeDurationOfInfiltrationChange(Sender: TObject);
begin
  inherited;
  EraseAnalyticResults;
  EraseNumericResults;

end;

procedure TfrmGwMound.rdeHeightAboveWaterTableChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeImperviousFractionChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateEdVolume;
end;

procedure TfrmGwMound.rdeInitialWaterContentClick(Sender: TObject);
begin
  EraseNumericResults;
end;

procedure TfrmGwMound.ResetColor(Sender: TObject);
begin
  (Sender as TRbwDataEntry).Color := clWindow;
end;

procedure TfrmGwMound.ShowKxUnits;
begin
  lblKxUnits.Caption := comboKzUnits.Text;
end;

procedure TfrmGwMound.CheckGreaterThanZero(Sender: TObject);
var
  rde: TRbwDataEntry;
  Value: Extended;
begin
  rde := Sender as TRbwDataEntry;
  if TryStrToFloat(rde.Text, Value) then
  begin
    if Value = 0 then
    begin
      Beep;
      rde.Color := clRed;
    end
    else
    begin
      rde.Color := clWindow;
    end;
  end;
end;

procedure TfrmGwMound.rdeKzChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateInfiltrationTime;
//  UpdateEdMinBasinArea;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeSimulationLengthNumericChange(Sender: TObject);
begin
  inherited;
  RemoveSimLengthWarningColor;
  EraseNumericResults;
end;

procedure TfrmGwMound.rdeSimulationLengthNumericExceededBounds(Sender: TObject);
begin
  inherited;
  Beep;
  rdeSimulationLengthNumeric.Color := clRed;
end;

procedure TfrmGwMound.rdeMaxDepthChange(Sender: TObject);
begin
//  UpdateEdMinBasinArea;
  EraseAnalyticResults;
  EraseNumericResults;
end;

procedure TfrmGwMound.rdeMaxDistanceChange(Sender: TObject);
begin
  EraseAnalyticResults;
end;

procedure TfrmGwMound.rdeMaxDistanceNumericChange(Sender: TObject);
var
  PlotDistance: Extended;
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  PlotDistance := StrToFloat(rdeMaxDistanceNumeric.Output);
  chrtNumeric.BottomAxis.Automatic := False;
  chrtNumeric.BottomAxis.Minimum := -PlotDistance;
  chrtNumeric.BottomAxis.Maximum := PlotDistance;
end;

procedure TfrmGwMound.ControlEnter(Sender: TObject);
var
  Control: TControl;
  Url: string;
begin
  Control := Sender as TControl;
  while (Control <> nil) and (Control.HelpKeyword = '') do
  begin
    Control := Control.Parent;
  end;
  if Control <> nil then
  begin
    Url := GetHelpUrl(Control.HelpKeyword);
    wbHelp.Navigate(Url);
  end;
end;

procedure TfrmGwMound.rdeKxChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeRectBasinLengthChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeRectBasinWidthChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeSaturatedWaterFractionClick(Sender: TObject);
begin
  EraseNumericResults;
end;

procedure TfrmGwMound.rdeSimulationLengthAnalyticChange(Sender: TObject);
begin
  EraseAnalyticResults;
end;

procedure TfrmGwMound.rdeSpecificYieldChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeSpecificYieldExit(Sender: TObject);
var
  Value: Extended;
begin
  inherited;
  if TryStrToFloat(rdeSpecificYield.Text, Value) then
  begin
    if (Value = 0) or (Value = 1) then
    begin
      Beep;
      rdeSpecificYield.Color := clRed;
    end
    else
    begin
      rdeSpecificYield.Color := clWindow;
      if Value > 0.2 then
      begin
        Beep;MessageDlg(StrSpecificYieldsGrea, mtInformation, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TfrmGwMound.rdeSquareLengthChange(Sender: TObject);
begin
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
  ResetColor(Sender);
end;

procedure TfrmGwMound.rdeTimeIncrementChange(Sender: TObject);
begin
  EraseAnalyticResults;
end;

procedure TfrmGwMound.rdeVolumeChange(Sender: TObject);
begin
  inherited;
  EraseAnalyticResults;
  EraseNumericResults;
//  UpdateEdMinBasinArea;
end;

procedure TfrmGwMound.rdeExceededBounds(Sender: TObject);
begin
  inherited;
  Beep;
end;

procedure TfrmGwMound.rdgBasinCoordinatesEndUpdate(Sender: TObject);
begin
  seCoordCount.AsInteger := rdgBasinCoordinates.RowCount-1;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

function TfrmGwMound.RechargePolygon: TPolygon2D;
const
  CircleCount = 100;
var
  HalfLength: Extended;
  HalfWidth: Extended;
  Radius: double;
  Index: Integer;
  Angle: Extended;
  Center: TPoint2D;
begin
  case TBasinShape(rgBasinShape.ItemIndex) of
    bsSquare:
      begin
        HalfLength := GetSquareSideLength/2;
        SetLength(result, 4);
        result[0].x := HalfLength;
        result[0].y := HalfLength;
        result[1].x := HalfLength;
        result[1].y := -HalfLength;
        result[2].x := -HalfLength;
        result[2].y := -HalfLength;
        result[3].x := -HalfLength;
        result[3].y := HalfLength;
      end;
    bsRectangle:
      begin
        HalfLength := GetRectangleLength/2;
        HalfWidth := GetRectangleWidth/2;
        SetLength(result, 4);
        result[0].x := HalfWidth;
        result[0].y := HalfLength;
        result[1].x := HalfWidth;
        result[1].y := -HalfLength;
        result[2].x := -HalfWidth;
        result[2].y := -HalfLength;
        result[3].x := -HalfWidth;
        result[3].y := HalfLength;
      end;
    bsCircle:
      begin
        Radius := GetCircleDiameter/2;
        Radius := EquivalentRadius(Radius, CircleCount);
        SetLength(result, CircleCount);
        for Index := 0 to CircleCount-1 do
        begin
          Angle := 2*Pi*Index/CircleCount;
          result[Index].x := Radius*Cos(Angle);
          result[Index].y := Radius*Sin(Angle);
        end;
      end;
    bsCustom:
      begin
        result := GetPolygon;
        Center := Centroid(result);
        for Index := 0 to Length(result) - 1 do
        begin
          result[Index].x := result[Index].x - Center.x;
          result[Index].y := result[Index].y - Center.y;
        end;
      end;
    else Assert(False);
  end;
end;

function TfrmGwMound.GetRechargeVolume: double;
var
  DepthUnits: TConvType;
  Depth: double;
  Area: double;
begin
  // Get recharge volume from basin depth and area
  result := 0;
  if csLoading in ComponentState then
  begin
    Exit;
  end;

  Depth := StrToFloatDef(rdeBasinDepth.Text, 0);
  DepthUnits := GetLengthUnits(comboMaxDepthUnits.ItemIndex);
  Depth := ConvertFrom(DepthUnits, Depth);

  Area := GetBasinArea;

  Result := Depth*Area

//  result := StrToFloatDef(rdeVolume.Text, 0);
//  VolumeUnits := GetVolumeUnits(comboVolumeUnits.ItemIndex);
//  result := ConvertFrom(VolumeUnits, result);

end;

procedure TfrmGwMound.rgBasinShapeClick(Sender: TObject);
begin
  plBasin.ActivePageIndex := rgBasinShape.ItemIndex;
  EraseAnalyticResults;
  EraseNumericResults;
  UpdateDisplayedBasinArea;
end;

procedure TfrmGwMound.Save1Click(Sender: TObject);
begin
  if dlgSaveFile.FileName = '' then
  begin
    SaveAs1Click(nil);
  end
  else
  begin
    SaveFile;
  end;

end;

procedure TfrmGwMound.SaveAs1Click(Sender: TObject);
begin
  if dlgSaveFile.Execute then
  begin
    SaveFile;
  end;
end;

procedure TfrmGwMound.seCoordCountChange(Sender: TObject);
begin
  rdgBasinCoordinates.RowCount := seCoordCount.AsInteger+1;
end;

procedure TRunModelThread.SaveFiles;
begin

end;

type TGridCrack = class(TRbwDataGrid4);

procedure TRunModelThread.SaveResults;
var
  Index: Integer;
  RowIndex: Integer;
  DistanceToWaterTable: Double;
//  AquiferThickness: Double;
begin
  FSeries.Clear;
  for Index := 0 to Length(Fresults.Distances) - 1 do
  begin
    FSeries.AddXY(ConvertTo(Fresults.Distances[Index], FDistanceUnits),
      ConvertTo(-Fresults.DrawDowns[Index], FDistanceUnits))
  end;
  FSummaryGrid.BeginUpdate;
  try
    FSummaryGrid.RowCount := 7;

    FSummaryGrid.Cells[0,0] := 'Termination';
    if Fresults.ErrorLine = '' then
    begin
      FSummaryGrid.Cells[1,0] := 'normal';
    end
    else
    begin
      FSummaryGrid.Cells[1,0] := Fresults.ErrorLine;
    end;

    if Fresults.ErrorLine <> '' then
    begin
      Beep;
      ShowMessage(Fresults.ErrorLine);
    end;

    FSummaryGrid.Cells[0,1] := 'Maximum cumulative percent discrepancy';
    FSummaryGrid.Cells[1,1] := FloatToStr(Fresults.MaxCumulativePercentDiscrepancy);

    FSummaryGrid.Cells[0,2] := 'Maximum percent discrepancy for a time step';
    FSummaryGrid.Cells[1,2] := FloatToStr(Fresults.MaxTimeStepPercentDiscrepancy);

    FSummaryGrid.Cells[0,3] := 'Maximum water table rise';
    FSummaryGrid.Cells[1,3] := FloatToStr(ConvertTo(-Fresults.MaxDrawdown,FDistanceUnits));

    FSummaryGrid.Cells[0,4] := 'Maximum distance with a water table rise > 0.05 feet';
    FSummaryGrid.Cells[1,4] := FloatToStr(ConvertTo(Fresults.DistanceForDrawdown005,FDistanceUnits));

    FSummaryGrid.Cells[0,5] := 'Maximum distance with a water table rise > 0.25 feet';
    FSummaryGrid.Cells[1,5] := FloatToStr(ConvertTo(Fresults.DistanceForDrawdown025,FDistanceUnits));

    FSummaryGrid.Cells[0,6] := 'Maximum distance with a water table rise > 1 foot';
    FSummaryGrid.Cells[1,6] := FloatToStr(ConvertTo(Fresults.DistanceForDrawdown100,FDistanceUnits));

  finally
    FSummaryGrid.EndUpdate
  end;
  TGridCrack(FSummaryGrid).HideEditor;

  FProfileGrid.BeginUpdate;
  try
    FProfileGrid.RowCount := Length(Fresults.Distances) + 1;
    FProfileGrid.Cells[0,0] := 'Position ('
      + ConvTypeToDescription(FDistanceUnits) + ')';
    FProfileGrid.Cells[1,0] := 'Maximum water table rise ('
      + ConvTypeToDescription(FDistanceUnits) + ')';
    FProfileGrid.Cells[2,0] := 'Time of maximum rise ('
      + ConvTypeToDescription(FTimeUnits) + ')';

    for RowIndex := 1 to Length(Fresults.Distances) do
    begin
      FProfileGrid.Cells[0,RowIndex] := FloatToStr(ConvertTo(Fresults.Distances[RowIndex-1],FDistanceUnits));
      FProfileGrid.Cells[1,RowIndex] := FloatToStr(ConvertTo(-Fresults.DrawDowns[RowIndex-1],FDistanceUnits));
      FProfileGrid.Cells[2,RowIndex] := FloatToStr(ConvertTo(Fresults.DrawdownTimes[RowIndex-1],FTimeUnits));
    end;
  finally
    FProfileGrid.EndUpdate;
  end;
  frmGwMound.UpdateNodeStateIndex;
  frmGwMound.rdeMaxDistanceNumericChange(nil);

  DistanceToWaterTable := frmGwMound.GetDistanceToWaterTable;
  if -Fresults.MaxDrawdown > DistanceToWaterTable then
  begin
    frmGwMound.DisplayMoundToHighMesssage;
    frmGwMound.chrtNumeric.Color := clRed;
  end
  else
  begin
    frmGwMound.chrtNumeric.Color := clWhite;
  end;

//  AquiferThickness := frmGwMound.GetAquiferThickness;
//  if -Fresults.MaxDrawdown > AquiferThickness/2  then
//  begin
//    frmGwMound.DisplayRiseToHigh;
//  end;
end;

procedure TRunModelThread.SetModelVariables;
var
  AChar: Char;
begin
  AChar := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    FModelMuseFile[FFileIndexes.TSMultPosition1] :=
      '      TimeStepMultiplier = ' + FloatToStr(FSimValues.TSMULT);
    FModelMuseFile[FFileIndexes.TSMultPosition2] :=
      '      TimeStepMultiplier = ' + FloatToStr(FSimValues.TSMULT);
    FModelMuseFile[FFileIndexes.FirstTimeStepPosition1] :=
      '      MaxLengthOfFirstTimeStep = ' + FloatToStr(FSimValues.FirstStep1);
    FModelMuseFile[FFileIndexes.FirstTimeStepPosition2] :=
      '      MaxLengthOfFirstTimeStep = ' + FloatToStr(FSimValues.FirstStep2);

    FModelMuseFile[FFileIndexes.KhPosition] :=
      '      Value = ' + FloatToStr(FSimValues.Kx);
    FModelMuseFile[FFileIndexes.KvPosition] :=
      '      Value = ' + FloatToStr(FSimValues.Kz);
    FModelMuseFile[FFileIndexes.RCH_PARPosition] :=
      '      Value = ' + FloatToStr(FSimValues.RechargeRate);
    FModelMuseFile[FFileIndexes.SyPosition] :=
      '      Value = ' + FloatToStr(FSimValues.SpecificYield);

    FModelMuseFile[FFileIndexes.EndtimePosition1] :=
      '      EndTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.StartTimePos2] :=
      '      StartTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.EndtimePosition2] :=
      '      EndTime = ' + FloatToStr(FSimValues.TotalSimTime);

    FModelMuseFile[FFileIndexes.PeriodLengthPosition1] :=
      '      PeriodLength = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.PeriodLengthPosition2] :=
      '      PeriodLength = ' + FloatToStr(FSimValues.TotalSimTime-FSimValues.SimTime);

    FModelMuseFile[FFileIndexes.ParamEndTimePosition1] :=
      '              EndTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.ParamStartTimePosition2] :=
      '              StartTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.ParamEndTimePosition2] :=
      '              EndTime = ' + FloatToStr(FSimValues.TotalSimTime);


    if FSimValues.UnSaturatedFlow then
    begin
      FModelMuseFile[FFileIndexes.RchPkgPosition] :=
        '  ModflowPackages.RchPackage.IsSelected = False';
      FModelMuseFile[FFileIndexes.UZFPkgPosition] :=
        '  ModflowPackages.UzfPackage.IsSelected = True';
    end
    else
    begin
      FModelMuseFile[FFileIndexes.RchPkgPosition] :=
        '  ModflowPackages.RchPackage.IsSelected = True';
      FModelMuseFile[FFileIndexes.UZFPkgPosition] :=
        '  ModflowPackages.UzfPackage.IsSelected = False';
    end;

    FModelMuseFile[FFileIndexes.UzfRechargePostion] :=
      '      Variable.RealValue = ' + FloatToStr(FSimValues.RechargeRate);

    FModelMuseFile[FFileIndexes.UzfEndTimePosition1] :=
      '          EndTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.UzfStartTimePosition2] :=
      '          StartTime = ' + FloatToStr(FSimValues.SimTime);
    FModelMuseFile[FFileIndexes.UzfEndTimePosition2] :=
      '          EndTime = ' + FloatToStr(FSimValues.TotalSimTime);

    FModelMuseFile[FFileIndexes.BrooksCoreyPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.BrooksCoreyEpsilon) + '''';
    FModelMuseFile[FFileIndexes.InitialUnsatPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.InitialWaterContent) + '''';
    FModelMuseFile[FFileIndexes.SaturatedPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.SaturatedWaterContent) + '''';
    FModelMuseFile[FFileIndexes.MaxUnsatVKPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.Kz) + '''';

    FModelMuseFile[FFileIndexes.ModelTopPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.DepthToWaterTable) + '''';

    FModelMuseFile[FFileIndexes.Layer1BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom/6) + '''';
    FModelMuseFile[FFileIndexes.Layer2BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom/3) + '''';
    FModelMuseFile[FFileIndexes.Layer3BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom/2) + '''';
    FModelMuseFile[FFileIndexes.Layer4BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom*2/3) + '''';
    FModelMuseFile[FFileIndexes.Layer5BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom*5/6) + '''';
    FModelMuseFile[FFileIndexes.Layer6BottomPosition] :=
      '      DataSetFormula = ''' + FloatToStr(FSimValues.ModelBottom) + '''';

  finally
    FormatSettings.DecimalSeparator := AChar;
  end;
end;

procedure TRunModelThread.UpdateProgressBar;
begin

end;

function TfrmGwMound.GetAnalyticSimulationLength: double;
var
  TimeUnits: TConvType;
begin
  result := StrToFloat(rdeSimulationLengthAnalytic.Text);
  TimeUnits := GetTimeUnit(comboSimulationLengthUnitsAnalytic.ItemIndex);
  result := ConvertFrom(TimeUnits, result);
end;

function TfrmGwMound.GetNumericSimulationLength: double;
var
  TimeUnits: TConvType;
begin
  result := StrToFloat(rdeSimulationLengthNumeric.Text);
  TimeUnits := GetTimeUnit(comboSimulationLengthUnitsNumeric.ItemIndex);
  result := ConvertFrom(TimeUnits, result);
end;

function TfrmGwMound.GetAreaUnitsExtended(ItemIndex: Integer): TConvType;
begin
  result := auSquareMeters;
  case ItemIndex of
//    0:
//      result := auSquareInches;
    0:
      result := auSquareFeet;
    1:
      result := auAcres;
//    3:
//      result := auSquareCentimeters;
    2:
      result := auSquareMeters;
    3:
      result := auHectares;

  else
    Assert(False);
  end;
end;

procedure TfrmGwMound.UpdateDisplayedBasinArea;
var
  BasinArea: Double;
  AreaUnits: TConvType;
//  RechargeVolume: Double;
//  BasinDepth: Extended;
//  MaxDepthUnits: TConvType;
//  MaxDepth: Double;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  BasinArea := GetBasinArea;
  AreaUnits := GetAreaUnitsExtended(comboMinBasinAreaUnits.ItemIndex);
//  if MinimumBasinArea <= BasinArea then
//  begin
//    lbledBasinArea.Color := clWindow;
//  end
//  else
//  begin
//    lbledBasinArea.Color := clRed;
//  end;
  lbledBasinArea.Text := FloatToStr(ConvertTo(BasinArea, AreaUnits));
  UpdateInfiltrationTime;


//  RechargeVolume := GetRechargeVolume;
//  if BasinArea = 0 then
//  begin
//    BasinDepth := 0
//  end
//  else
//  begin
//    BasinDepth := RechargeVolume/BasinArea;
//  end;
//
//  MaxDepthUnits := GetLengthUnits(comboMaxDepthUnits.ItemIndex);
//
//  MaxDepth := GetMaxDepth;
//  if BasinDepth > MaxDepth then
//  begin
//    lbledBasinDepth.Color := clRed;
//  end
//  else
//  begin
//    lbledBasinDepth.Color := clWindow;
//  end;
//
//  lbledBasinDepth.Text := FloatToStr(ConvertTo(BasinDepth, MaxDepthUnits));
end;

function TfrmGwMound.GetSimulationIncrement: double;
var
  TimeUnits: TConvType;
begin
  result := StrToFloat(rdeTimeIncrement.Text);
  TimeUnits := GetTimeUnit(comboTimeIncrementUnits.ItemIndex);
  result := ConvertFrom(TimeUnits, result);
end;

function TfrmGwMound.GetSpecificYield: double;
begin
  result := StrToFloat(rdeSpecificYield.Text);
end;

procedure TfrmGwMound.UpdateInfiltrationTime;
var
  RechargeTime: Double;
  RechargeRate: Double;
  TimeUnit: TConvType;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  GetDefaultRechargeRateAndTime(RechargeTime, RechargeRate);
  TimeUnit := GetTimeUnit(comboDurationOfInfiltration.ItemIndex);
  RechargeTime := ConvertTo(RechargeTime, TimeUnit);
  rdeDurationOfInfiltration.Min := 0;
  rdeDurationOfInfiltration.Text := FloatToStr(RechargeTime);
  // The double conversion is to avoid round-off error.
  rdeDurationOfInfiltration.Min := StrToFloat(rdeDurationOfInfiltration.Text);
end;

procedure TfrmGwMound.GetDefaultRechargeRateAndTime(var RechargeTime,
  RechargeRate: Double);
var
  RechargeDepth: Double;
  Kz: Double;
  BasinArea: Double;
  Volume: Double;
begin
  Kz := GetKz;
  RechargeRate := Kz;
  BasinArea := GetBasinArea;
  if (BasinArea = 0) or (RechargeRate = 0) then
  begin
    RechargeTime := 0;
  end
  else
  begin
    Volume := GetRechargeVolume;
    RechargeDepth := Volume / BasinArea;
    RechargeTime := RechargeDepth / RechargeRate;
  end;
end;

function TfrmGwMound.GetInfiltrationTime: double;
var
  TimeUnit: TConvType;
begin
  if TryStrToFloat(rdeDurationOfInfiltration.Text, result) then
  begin
    TimeUnit := GetTimeUnit(comboDurationOfInfiltration.ItemIndex);
    result := ConvertFrom(TimeUnit, result)
  end
  else
  begin
    result := 0;
  end;
end;

procedure TfrmGwMound.SetMinForInfiltrationTime;
var
  RechargeTime: Double;
  RechargeRate: Double;
  TimeUnit: TConvType;
begin
  GetDefaultRechargeRateAndTime(RechargeTime, RechargeRate);
  TimeUnit := GetTimeUnit(comboDurationOfInfiltration.ItemIndex);
  RechargeTime := ConvertTo(RechargeTime, TimeUnit);
  // The double conversion is to avoid round-off error.
  rdeDurationOfInfiltration.Min := StrToFloat(FloatToStr(RechargeTime));
end;

procedure TfrmGwMound.GetRechargeRateAndTime(var RechargeTime,
  RechargeRate: Double);
var
//  RechargeDepth: Double;
//  Kz: Double;
//  BasinArea: Double;
//  Volume: Double;
  InfiltrationTime: Double;
begin
  GetDefaultRechargeRateAndTime(RechargeTime, RechargeRate);
//  Kz := GetKz;
//  RechargeRate := Kz;
//  BasinArea := GetBasinArea;
//  if (BasinArea = 0) or (RechargeRate = 0) then
//  begin
//    RechargeTime := 0;
//  end
//  else
//  begin
//    Volume := GetRechargeVolume;
//    RechargeDepth := Volume / BasinArea;
//    RechargeTime := RechargeDepth / RechargeRate;
//  end;

  InfiltrationTime := GetInfiltrationTime;
  if InfiltrationTime <> RechargeTime then
  begin
    if InfiltrationTime = 0 then
    begin
      RechargeTime := 0;
      RechargeRate := 0;
    end
    else
    begin
      RechargeRate := RechargeRate*RechargeTime/InfiltrationTime;
      RechargeTime := InfiltrationTime
    end;
  end;
end;

procedure TfrmGwMound.GetModelVariables(var SimValues: TSimValues);
const
  SaturatedStepCount = 60;
  UZFStepCount = 40;
var
  StepCount: integer;
begin
  SimValues.UnSaturatedFlow := cbSimulateUnsat.Checked;
  if SimValues.UnSaturatedFlow then
  begin
    SimValues.TSMULT := 1;
    StepCount := UZFStepCount;
  end
  else
  begin
    SimValues.TSMULT := TSMULT;
    StepCount := SaturatedStepCount;
  end;
  GetRechargeRateAndTime(SimValues.SimTime, SimValues.RechargeRate);
  SimValues.TotalSimTime := GetNumericSimulationLength;
  if SimValues.TSMULT = 1 then
  begin
    SimValues.FirstStep1 := SimValues.SimTime/StepCount;
    SimValues.FirstStep2 := (SimValues.TotalSimTime - SimValues.SimTime)/StepCount;
  end
  else
  begin
    SimValues.FirstStep1 := SimValues.SimTime * (SimValues.TSMULT - 1)
      / (Power(SimValues.TSMULT, StepCount) - 1);
    SimValues.FirstStep2 := (SimValues.TotalSimTime - SimValues.SimTime)
      * (SimValues.TSMULT - 1) / (Power(SimValues.TSMULT, StepCount) - 1);
  end;
  SimValues.Kx := GetKx;
  SimValues.Kz := GetKz;
  SimValues.SpecificYield := GetSpecificYield;
  SimValues.BrooksCoreyEpsilon := StrToFloat(rdeBrooksCoreyEpsilon.Text);
  SimValues.SaturatedWaterContent := StrToFloat(rdeSaturatedWaterFraction.Text);
  SimValues.InitialWaterContent := StrToFloat(rdeInitialWaterContent.Text);
  SimValues.DepthToWaterTable := GetDistanceToWaterTable;

  SimValues.ModelBottom := -GetAquiferThickness;
end;

//function TfrmGwMound.GetAnisotropy: Double;
//begin
//  result := StrToFloat(rdeRatio.Text);
//end;

function TfrmGwMound.GetKx: Double;
var
  VelUnits: TConvType;
begin
  result := StrToFloatDef(rdeKx.Text, 0);
  if result = 0 then
  begin
    Exit;
  end;
  VelUnits := VelocityConvTypes[comboKzUnits.ItemIndex];
  result := ConvertFrom(VelUnits, result);
end;

procedure TfrmGwMound.SetRechargePoints(ModelMuseFile: TStringList;
  var FileIndexes: TFileIndexes; FileContstants: TConstantFileValues);
var
  TempModelMuseFile: TStringList;

  Index: Integer;
  AChar: Char;
  NewEnd: Integer;
  OldPosition: Integer;
  NewPosition: Integer;
  APolygon: TPolygon2D;
begin
  AChar := FormatSettings.DecimalSeparator;
  TempModelMuseFile := TStringList.Create;
  try
    FormatSettings.DecimalSeparator := '.';
    APolygon := RechargePolygon;
    TempModelMuseFile.Capacity := ModelMuseFile.Count + Length(APolygon)*4;
    for Index := 0 to FileIndexes.PointsStart  do
    begin
      TempModelMuseFile.Add(ModelMuseFile[Index])
    end;
    for Index := 0 to Length(APolygon) - 1 do
    begin
      TempModelMuseFile.Add('        item');
      TempModelMuseFile.Add('          X = ' + FloatToStr(APolygon[Index].x + FileContstants.CenterX));
      TempModelMuseFile.Add('          Y = ' + FloatToStr(APolygon[Index].y + FileContstants.CenterY));
      TempModelMuseFile.Add('        end');
    end;
    TempModelMuseFile.Add('        item');
    TempModelMuseFile.Add('          X = ' + FloatToStr(APolygon[0].x + FileContstants.CenterX));
    TempModelMuseFile.Add('          Y = ' + FloatToStr(APolygon[0].y + FileContstants.CenterY));
    TempModelMuseFile.Add('        end>');
    NewEnd := TempModelMuseFile.Count-1;
    for Index := FileIndexes.PointsEnd+1 to ModelMuseFile.Count -1  do
    begin
      TempModelMuseFile.Add(ModelMuseFile[Index])
    end;
    ModelMuseFile.Assign(TempModelMuseFile);

    OldPosition := FileIndexes.PointsEnd;
    NewPosition := NewEnd;
    AdjustIndices(FileIndexes, NewPosition, OldPosition);
    FileIndexes.PointsEnd := NewEnd;

  finally
    TempModelMuseFile.Free;
    FormatSettings.DecimalSeparator := AChar;
  end;
end;

procedure TfrmGwMound.AdjustIndices(var FileIndexes: TFileIndexes; NewPosition, OldPosition: Integer);
  procedure AdjustIndex(var Index: integer);
  begin
    if Index > OldPosition then
    begin
      Index := Index + NewPosition - OldPosition;
    end;
  end;
begin
  AdjustIndex(FileIndexes.KhPosition);
  AdjustIndex(FileIndexes.PointsStart);
  AdjustIndex(FileIndexes.PointsEnd);
  AdjustIndex(FileIndexes.KvPosition);
  AdjustIndex(FileIndexes.SyPosition);
  AdjustIndex(FileIndexes.DepthPosition);
  AdjustIndex(FileIndexes.ParamEndTimePosition1);
  AdjustIndex(FileIndexes.ParamStartTimePosition2);
  AdjustIndex(FileIndexes.ParamEndTimePosition2);
  AdjustIndex(FileIndexes.RCH_PARPosition);
  AdjustIndex(FileIndexes.EndtimePosition1);
  AdjustIndex(FileIndexes.PeriodLengthPosition1);
  AdjustIndex(FileIndexes.FirstTimeStepPosition1);
  AdjustIndex(FileIndexes.TSMultPosition1);
  AdjustIndex(FileIndexes.EndtimePosition2);
  AdjustIndex(FileIndexes.PeriodLengthPosition2);
  AdjustIndex(FileIndexes.FirstTimeStepPosition2);
  AdjustIndex(FileIndexes.TSMultPosition2);
  AdjustIndex(FileIndexes.StartTimePos2);
  AdjustIndex(FileIndexes.RchPkgPosition);
  AdjustIndex(FileIndexes.UZFPkgPosition);
  AdjustIndex(FileIndexes.UzfRechargePostion);
  AdjustIndex(FileIndexes.UzfEndTimePosition1);
  AdjustIndex(FileIndexes.UzfStartTimePosition2);
  AdjustIndex(FileIndexes.UzfEndTimePosition2);

  AdjustIndex(FileIndexes.BrooksCoreyPosition);
  AdjustIndex(FileIndexes.InitialUnsatPosition);
  AdjustIndex(FileIndexes.SaturatedPosition);
  AdjustIndex(FileIndexes.MaxUnsatVKPosition);
  AdjustIndex(FileIndexes.ModelTopPosition);
  AdjustIndex(FileIndexes.Layer1BottomPosition);
  AdjustIndex(FileIndexes.Layer2BottomPosition);
  AdjustIndex(FileIndexes.Layer3BottomPosition);
  AdjustIndex(FileIndexes.Layer4BottomPosition);
  AdjustIndex(FileIndexes.Layer5BottomPosition);
  AdjustIndex(FileIndexes.Layer6BottomPosition);
end;

procedure TfrmGwMound.tvNavigationChange(Sender: TObject; Node: TTreeNode);
var
  Url: string;
begin
{$IFDEF UNSAT}
  plMain.ActivePageIndex := Node.AbsoluteIndex;
{$ELSE}
  if Node.AbsoluteIndex > FRunAnalytic.AbsoluteIndex then
  begin
    plMain.ActivePageIndex := Node.AbsoluteIndex+1;
  end
  else
  begin
    plMain.ActivePageIndex := Node.AbsoluteIndex;
  end;
{$ENDIF}

  btnBack.Enabled := plMain.ActivePageIndex > 0;
  btnNext.Enabled := plMain.ActivePageIndex < plMain.PageCount-1;

  Url := GetHelpUrl(plMain.ActivePage.HelpKeyword);
  wbHelp.Navigate(Url);
  UpdateNodeStateIndex;
end;

const
  InchesPerFoot = 12;
  FeetPerMeter = 1/0.3048;
  HoursPerDay = 24;
  SecondsPerHour = 3600;
  MinutesPerHour = 60;
  CmPerM = 100;

procedure InitializeVelConversion;
var
  VelIndex: integer;
begin
  cbVelocity := RegisterConversionFamily('Velocity');
  VelIndex := 0;
//  vuInchesPerSecond := RegisterConversionType(cbVelocity, 'inches per second', 1/(InchesPerFoot*FeetPerMeter)*HoursPerDay*SecondsPerHour);
//  VelocityConvTypes[VelIndex] := vuInchesPerSecond;

//  Inc(VelIndex);
//  vuInchesPerMinute := RegisterConversionType(cbVelocity, 'inches per minute', 1/(InchesPerFoot*FeetPerMeter)*HoursPerDay*MinutesPerHour);
//  VelocityConvTypes[VelIndex] := vuInchesPerMinute;

//  Inc(VelIndex);
  vuInchesPerHour := RegisterConversionType(cbVelocity, 'inches per hour', 1/(InchesPerFoot*FeetPerMeter)*HoursPerDay);
  VelocityConvTypes[VelIndex] := vuInchesPerHour;

//  Inc(VelIndex);
//  vuInchesPerDay := RegisterConversionType(cbVelocity, 'inches per day', 1/(InchesPerFoot*FeetPerMeter));
//  VelocityConvTypes[VelIndex] := vuInchesPerDay;

  Inc(VelIndex);
  vuFeetPerSecond := RegisterConversionType(cbVelocity, 'feet per second', 1/FeetPerMeter*HoursPerDay*SecondsPerHour);
  VelocityConvTypes[VelIndex] := vuFeetPerSecond;

//  Inc(VelIndex);
//  vuFeetPerMinute := RegisterConversionType(cbVelocity, 'feet per minute', 1/FeetPerMeter * HoursPerDay*MinutesPerHour);
//  VelocityConvTypes[VelIndex] := vuFeetPerMinute;

//  Inc(VelIndex);
//  vuFeetPerHour := RegisterConversionType(cbVelocity, 'feet per hour', 1/FeetPerMeter * HoursPerDay);
//  VelocityConvTypes[VelIndex] := vuFeetPerHour;

  Inc(VelIndex);
  vuFeetPerDay := RegisterConversionType(cbVelocity, 'feet per day', 1/FeetPerMeter);
  VelocityConvTypes[VelIndex] := vuFeetPerDay;

  Inc(VelIndex);
  vuCentimetersPerSecond := RegisterConversionType(cbVelocity, 'centimeters per second', 1/CmPerM * HoursPerDay*SecondsPerHour);
  VelocityConvTypes[VelIndex] := vuCentimetersPerSecond;

//  Inc(VelIndex);
//  vuCentimetersPerMinute := RegisterConversionType(cbVelocity, 'centimeters per minute', 1/CmPerM * HoursPerDay*MinutesPerHour);
//  VelocityConvTypes[VelIndex] := vuCentimetersPerMinute;

  Inc(VelIndex);
  vuCentimetersPerHour := RegisterConversionType(cbVelocity, 'centimeters per hour', 1/CmPerM * HoursPerDay);
  VelocityConvTypes[VelIndex] := vuCentimetersPerHour;

  Inc(VelIndex);
  vuCentimetersPerDay := RegisterConversionType(cbVelocity, 'centimeters per day', 1/CmPerM);
  VelocityConvTypes[VelIndex] := vuCentimetersPerDay;

  Inc(VelIndex);
  vuMetersPerSecond := RegisterConversionType(cbVelocity, 'meters per second', HoursPerDay*SecondsPerHour);
  VelocityConvTypes[VelIndex] := vuMetersPerSecond;

//  Inc(VelIndex);
//  vuMetersPerMinute := RegisterConversionType(cbVelocity, 'meters per minute', 1/(HoursPerDay*MinutesPerHour));
//  VelocityConvTypes[VelIndex] := vuMetersPerMinute;

  Inc(VelIndex);
  vuMetersPerHour := RegisterConversionType(cbVelocity, 'meters per hour', HoursPerDay);
  VelocityConvTypes[VelIndex] := vuMetersPerHour;

  Inc(VelIndex);
  vuMetersPerDay := RegisterConversionType(cbVelocity, 'meters per day', 1);
  VelocityConvTypes[VelIndex] := vuMetersPerDay;
end;

{ TRunModelThread }

constructor TRunModelThread.Create(Directory: string;
  ModelMuseFile: TStringList; FileIndexes: TFileIndexes; SimValues: TSimValues;
  FileConstants: TConstantFileValues; Series: TLIneSeries;
  DistanceUnits, TimeUnits: TConvType; SummaryGrid, ProfileGrid: TRbwDataGrid4);
var
//  Files: TStringDynArray;
  FileNumber: Integer;
  Directories: TStringDynArray;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  if not DirectoryExists(Directory) then
  begin
    ForceDirectories(Directory)
  end;

  Directories := TDirectory.GetDirectories(Directory, 'Model*');
  FileNumber := Length(Directories)+1;
  FDirectory := IncludeTrailingPathDelimiter(Directory) + 'Model'
    + IntToStr(FileNumber);
  while DirectoryExists(FDirectory) do
  begin
    Inc(FileNumber);
    FDirectory := IncludeTrailingPathDelimiter(Directory) + 'Model'
      + IntToStr(FileNumber);
  end;
  ForceDirectories(FDirectory);

//  Files := TDirectory.GetFiles(FDirectory, 'Model*.gpt');
//  FileNumber := Length(Files)+1;
  FModelMuseFileName := IncludeTrailingPathDelimiter(FDirectory)
    + 'Model' + IntToStr(FileNumber) + '.gpt';
  while FileExists(FModelMuseFileName) do
  begin
    Inc(FileNumber);
    FModelMuseFileName := IncludeTrailingPathDelimiter(FDirectory)
      + 'Model' + IntToStr(FileNumber) + '.gpt';
  end;


  FModelMuseFile := TStringList.Create;
  FModelMuseFile.Assign(ModelMuseFile);
  FCreateProcess := TJvCreateProcess.Create(nil);
  FCreateProcess.OnTerminate := ProcessDone;
  FFileIndexes := FileIndexes;
  FSimValues := SimValues;
  FConstants := FileConstants;
  FSeries := Series;
  FDistanceUnits := DistanceUnits;
  FTimeUnits := TimeUnits;
  FSummaryGrid := SummaryGrid;
  FProfileGrid := ProfileGrid;
end;

constructor TRunModelThread.NilSelf;
begin
  frmGwMound.FThread := nil;
end;

procedure TRunModelThread.Terminate;
begin
  inherited;
  if FCreateProcess.State <> psReady then
  begin
    FCreateProcess.Terminate
  end;
end;

destructor TRunModelThread.Destroy;
begin
  FCreateProcess.Free;
  FModelMuseFile.Free;
  inherited;
end;

procedure TRunModelThread.DisableAbort;
begin
  frmGwMound.btnAbortNumeric.Enabled := False;
end;

procedure TRunModelThread.Execute;
begin
  inherited;
  try
    if Terminated then Exit;
    SetModelVariables;
    FModelMuseFile.SaveToFile(FModelMuseFileName);
    if Terminated then Exit;
    RunModelMuse;
    if Terminated then Exit;
    RunModflow;
    if Terminated then Exit;
    ExtractResults;
    if Terminated then Exit;
    Synchronize(SaveResults);
    if Terminated then Exit;
    Synchronize(UpdateProgressBar);
    if Terminated then Exit;
    SaveFiles;
    if Terminated then Exit;
  finally
    Synchronize(DisableAbort);
  end;
end;

procedure TRunModelThread.ExtractedHydmodResults(HydInput: TStringList);
const
  Point05Feet = -0.05*0.3048;
  Point25Feet = -0.25*0.3048;
  OneFoot = -0.3048;
var
  HydModFile: string;
  AValue: TModflowDouble;
  LabelPos: Integer;
  LabelIndex: Integer;
  Distance: Double;
  TimeIndex: Integer;
  HydCell: THydCell;
//  CellIndex: Integer;
//  TestDistIndex: Integer;
  HydModResults: THydModData;
//  ACell: THydCell;
  LabelPositions: TIntegerList;

begin
  HydModResults := THydModData.Create;
  try
    HydModFile := ChangeFileExt(FModelMuseFileName, '.hyd_out');
    HydModResults.ReadFile(HydModFile);
//    for TestDistIndex := 0 to FFixedDistances.Count - 1 do
//    begin
//      ACell := FixedDistanceCells[TestDistIndex];
//      CellIndex := HydModResults.IndexOfLabel(ACell.Name);
//      Assert(CellIndex >= 0);
//      FResults.FixedDistanceDrawDowns[TestDistIndex] := HydModResults.Values[CellIndex, 0];
//      for TimeIndex := 0 to HydModResults.TimeCount - 1 do
//      begin
//        if HydModResults.Values[CellIndex, TimeIndex] < FResults.FixedDistanceDrawDowns[TestDistIndex] then
//        begin
//          FResults.FixedDistanceDrawDowns[TestDistIndex] := HydModResults.Values[CellIndex, TimeIndex];
//        end;
//      end;
//    end;
    LabelPositions := TIntegerList.Create;
    try
      LabelPositions.Capacity := HydModResults.LabelCount;
      SetLength(FResults.DrawDowns, HydModResults.LabelCount);
      SetLength(FResults.Distances, HydModResults.LabelCount);
      SetLength(FResults.DrawdownTimes, HydModResults.LabelCount);
      for LabelIndex := 0 to HydModResults.LabelCount - 1 do
      begin
        LabelPositions.Add(HydInput.IndexOf(HydModResults.Labels[LabelIndex]));
      end;
      if (HydModResults.LabelCount > 0) and (HydModResults.TimeCount > 0) then
      begin
        FResults.MaxDrawdown := 0;
        FResults.RowForDrawdown005 := 0;
        FResults.DistanceForDrawdown005 := 0;
        FResults.RowForDrawdown025 := 0;
        FResults.DistanceForDrawdown025 := 0;
        FResults.RowForDrawdown100 := 0;
        FResults.DistanceForDrawdown100 := 0;
        for LabelIndex := 0 to LabelPositions.Count - 1 do
        begin
          LabelPos := LabelPositions[LabelIndex];
          if LabelPos >= 0 then
          begin
            HydCell := HydInput.Objects[LabelPos] as THydCell;
            Assert(HydCell.Name = HydModResults.Labels[LabelIndex]);
            Distance := FCenterY - HydCell.Y;
            FResults.Distances[LabelIndex] := Distance;
            FResults.DrawDowns[LabelIndex] := HydModResults.Values[LabelIndex, 0];
            FResults.DrawdownTimes[LabelIndex] := HydModResults.Times[0];
            for TimeIndex := 0 to HydModResults.TimeCount - 1 do
            begin
              AValue := HydModResults.Values[LabelIndex, TimeIndex];
              if FResults.DrawDowns[LabelIndex] > AValue then
              begin
                FResults.DrawDowns[LabelIndex] := AValue;
                FResults.DrawdownTimes[LabelIndex] := HydModResults.Times[TimeIndex];
              end;
              if FResults.MaxDrawdown > AValue then
              begin
                FResults.MaxDrawdown := AValue;
              end;
              if AValue <= Point05Feet then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown005 < Distance then
                begin
                  FResults.DistanceForDrawdown005 := Distance;
                  FResults.RowForDrawdown005 := HydCell.Row;
                end;
              end;
              if AValue <= Point25Feet then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown025 < Distance then
                begin
                  FResults.DistanceForDrawdown025 := Distance;
                  FResults.RowForDrawdown025 := HydCell.Row;
                end;
              end;
              if AValue <= OneFoot then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown100 < Distance then
                begin
                  FResults.DistanceForDrawdown100 := Distance;
                  FResults.RowForDrawdown100 := HydCell.Row;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      LabelPositions.Free;
    end;
  finally
    HydModResults.Free;
  end;
end;

procedure TRunModelThread.ExtractFromListingFile;
const
  BudgetSearch = 'BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  DiscSearch = 'PERCENT DISCREPANCY =';
  ErrorSearch = '****FAILED TO MEET SOLVER CONVERGENCE CRITERIA ';
var
  BudStart: Integer;
  DiscrepStart1: Integer;
  DiscrepEnd: Integer;
  AnAnsiString: AnsiString;
  ListingFileName: string;
  TimeStepString: string;
  StringBuilder: TStringBuilder;
  CumPercent: Double;
  LineIndex: Integer;
  SeachIndex: Integer;
  CumPercentString: string;
  AString: string;
  TimeStepPercent: Double;
  OffSet: Integer;
  DiscrepStart2: Integer;
  ListFile: TStreamReader;
  ErrorStart: integer;
  ErrorEnd: integer;
begin
  ListingFileName := ChangeFileExt(FModelMuseFileName, '.lst');
  ListFile := TFile.OpenText(ListingFileName);
  try
    StringBuilder := TStringBuilder.Create;
    try
      SeachIndex := 0;
      repeat
        StringBuilder.Clear;
        for LineIndex := 1 to 1000 do
        begin
          StringBuilder.Append(ListFile.ReadLine);
          StringBuilder.AppendLine;
          if ListFile.EndOfStream then
          begin
            break;
          end;
        end;
        AString := StringBuilder.ToString;
        AnAnsiString := AnsiString(AString);
        ErrorStart := BMPosSimpleEx(ErrorSearch, AnAnsiString, 1);
        if ErrorStart > 0 then
        begin
          OffSet := ErrorStart + Length(BudgetSearch);
          ErrorEnd := BMPosSimpleEx(#13, AnAnsiString, OffSet);
          FResults.ErrorLine := Trim(Copy(AString, ErrorStart, ErrorEnd-ErrorStart));
        end;

        OffSet := 1;
        repeat
          if SeachIndex = 0 then
          begin
            BudStart := BMPosSimpleEx(BudgetSearch, AnAnsiString, OffSet);
            if BudStart > 0 then
            begin
              OffSet := BudStart + Length(BudgetSearch);
              SeachIndex := 1;
            end;
          end
          else
          begin
            BudStart := 1;
            OffSet := 1;
          end;
          if SeachIndex = 1 then
          begin
            DiscrepStart1 := BMPosSimpleEx(DiscSearch, AnAnsiString, OffSet);
            if DiscrepStart1 > 0 then
            begin
              DiscrepStart1 := DiscrepStart1 + Length(DiscSearch);
              DiscrepStart2 := BMPosSimpleEx(DiscSearch, AnAnsiString, DiscrepStart1);
              Assert(DiscrepStart2 > DiscrepStart1);
              CumPercentString := Trim(Copy(AString, DiscrepStart1, DiscrepStart2 - DiscrepStart1));
              CumPercent := Abs(StrToFloat(CumPercentString));
              DiscrepStart2 := DiscrepStart2 + Length(DiscSearch);
              DiscrepEnd := BMPosSimpleEx(sLineBreak, AnAnsiString, DiscrepStart2);
              Assert(DiscrepEnd > DiscrepStart2);
              TimeStepString := Trim(Copy(AString, DiscrepStart2, DiscrepEnd - DiscrepStart2));
              TimeStepPercent := Abs(StrToFloat(TimeStepString));
              if CumPercent > FResults.MaxCumulativePercentDiscrepancy then
              begin
                FResults.MaxCumulativePercentDiscrepancy := CumPercent;
              end;
              if TimeStepPercent > FResults.MaxTimeStepPercentDiscrepancy then
              begin
                FResults.MaxTimeStepPercentDiscrepancy := TimeStepPercent;
              end;
              SeachIndex := 0;
              OffSet := DiscrepEnd;
            end
            else
            begin
              BudStart := 0;
            end;
          end;
        until (BudStart = 0);
      until (ListFile.EndOfStream);
//      StringBuilder.Clear;
    finally
      StringBuilder.Free;
    end;
  finally
    ListFile.Free;
  end;
end;

procedure TRunModelThread.ExtractResults;
var
  OldDecSeparator : Char;
  ColPositions: TRealList;
  RowPositions: TRealList;
  HydInput: TStringList;
  Index: Integer;
begin
{
   Variables to get:

   Included in TVariableFileValues:
     Shape
     Basin area
     Kv
     Sy
     Basin depth

   Calculated:
     Kx

   Extracted from Listing file:
     Max cummulative percent discrepancy
     Max percent discrepancy for a time step

   Extracted from hydmod output
     Max drawdown

   Determined using a combination of the .dis, hydmod input, and hydmod output
     Highest row number with drawdown < -0.05
     Maximum distance with drawdown < -0.05
     Drawdown at fixed distances
}

  FResults.MaxCumulativePercentDiscrepancy := 0;
  FResults.MaxTimeStepPercentDiscrepancy := 0;
  FResults.ErrorLine := '';
  OldDecSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    ExtractFromListingFile;

    HydInput := TStringList.Create;
    try
      ColPositions := TRealList.Create;
      RowPositions := TRealList.Create;
      try
        GetColRowPositions(ColPositions, RowPositions);
        ReadHydmodInput(RowPositions, ColPositions, HydInput);
      finally
        RowPositions.Free;
        ColPositions.Free;
      end;
      ExtractedHydmodResults(HydInput);

    finally
      for Index := 0 to HydInput.Count - 1 do
      begin
        HydInput.Objects[Index].Free;
      end;
      HydInput.Free;
    end;


  finally
    FormatSettings.DecimalSeparator := OldDecSeparator;
  end;
end;

procedure TRunModelThread.GetColRowPositions(var ColPositions,
  RowPositions: TRealList);
var
  Index: Integer;
  DisFileName: string;
  NCOL: Integer;
  RowPosition: Double;
  ALine: string;
  Splitter: TStringList;
  NROW: Integer;
  DisFile: TStreamReader;
  ColPosition: Double;
begin
  DisFileName := ChangeFileExt(FModelMuseFileName, '.dis');
  DisFile := TFile.OpenText(DisFileName);
  try
    ALine := DisFile.ReadLine;
    while (ALine = '') or (ALine[1] = '#') do
    begin
      ALine := DisFile.ReadLine;
    end;
    Splitter := TStringList.Create;
    try
      // Data set 1.
      Splitter.Delimiter := ' ';
      Splitter.DelimitedText := Trim(ALine);
      NROW := StrToInt(Splitter[1]);
      NCOL := StrToInt(Splitter[2]);
      ColPositions.Capacity := NCOL+1;
      RowPositions.Capacity := NROW+1;
      ColPositions.Add(0);
      RowPositions.Add(0);
      // Skip two lines to get to DELR.
      DisFile.ReadLine;
      DisFile.ReadLine;
      ColPosition := 0;
      while ColPositions.Count < ColPositions.Capacity do
      begin
        Splitter.DelimitedText := Trim(DisFile.ReadLine);
        for Index := 0 to Splitter.Count - 1 do
        begin
          ColPosition := ColPosition + StrToFloat(Splitter[Index]);
          ColPositions.Add(ColPosition);
        end;
      end;
      Assert(ColPositions.Count = NCOL+1);
      // Skip one line to get to DELC
      DisFile.ReadLine;
      RowPosition := 0;
      while RowPositions.Count < RowPositions.Capacity do
      begin
        Splitter.DelimitedText := Trim(DisFile.ReadLine);
        for Index := 0 to Splitter.Count - 1 do
        begin
          RowPosition := RowPosition + StrToFloat(Splitter[Index]);
          RowPositions.Add(RowPosition);
        end;
      end;
      Assert(RowPositions.Count = NROW+1);
      FCenterX := (ColPositions[0] + ColPositions[ColPositions.Count-1])/2;
      FCenterY := (RowPositions[0] + RowPositions[RowPositions.Count-1])/2;
    finally
      Splitter.Free;
    end;
  finally
    DisFile.Free;
  end;
  RowPositions.Sorted := true;
  ColPositions.Sorted := true;
end;

procedure TRunModelThread.ProcessDone(Sender: TObject; ExitCode: DWORD);
begin
  Suspended := False;
end;

procedure TRunModelThread.ReadHydmodInput(RowPositions, ColPositions: TRealList;
  var HydInput: TStringList);
var
  Y: Double;
  Splitter: TStringList;
  HydModInputFile: string;
  ClosestBoundary: Integer;
  ALine: string;
  X: Double;
  HydModFile: TStreamReader;
  HydCell: THydCell;
//  TestDistances: array of double;
//  DistIndex: Integer;
//  FixedDist: Double;
//  CellIndex: Integer;
//  TestDist: double;
begin
  HydModInputFile := ChangeFileExt(FModelMuseFileName, '.hyd');
  HydModFile := TFile.OpenText(HydModInputFile);
  try
    ALine := HydModFile.ReadLine;
    while (ALine = '') or (ALine[1] = '#') do
    begin
      ALine := HydModFile.ReadLine;
    end;
    // Skip a line to get to Data Set 2;
//    HydModFile.ReadLine;
    Splitter := TStringList.Create;
    try
      Splitter.Delimiter := ' ';
      repeat
        Splitter.DelimitedText := Trim(HydModFile.ReadLine);
        if (UpperCase(Splitter[0]) = 'BAS') and (UpperCase(Splitter[1]) = 'DD') then
        begin
          HydCell := THydCell.Create;
          HydCell.Name := UpperCase(Splitter[1] + Splitter[2]);
          HydCell.Layer := StrToInt(Splitter[3]);
          While (Length(Splitter[3]) < 3) do
          begin
            Splitter[3] := '0' + Splitter[3];
          end;
          HydCell.Name := HydCell.Name + Splitter[3] + Splitter[6];
          X := StrToFloat(Splitter[4]);
          Y := StrToFloat(Splitter[5]);
          ClosestBoundary := ColPositions.IndexOfClosest(X);
          if ColPositions[ClosestBoundary] < ClosestBoundary then
          begin
            HydCell.Col := ClosestBoundary + 1;
          end
          else
          begin
            HydCell.Col := ClosestBoundary;
          end;
          ClosestBoundary := RowPositions.IndexOfClosest(y);
          if RowPositions[ClosestBoundary] < ClosestBoundary then
          begin
            HydCell.Row := ClosestBoundary + 1;
          end
          else
          begin
            HydCell.Row := ClosestBoundary;
          end;
          if UpperCase(Splitter[2]) = 'C' then
          begin
            HydCell.X := (ColPositions[HydCell.Col - 1] + ColPositions[HydCell.Col]) / 2;
            HydCell.Y := (RowPositions[HydCell.Row - 1] + RowPositions[HydCell.Row]) / 2;
          end
          else
          begin
            HydCell.X := X;
            HydCell.Y := Y;
          end;
          HydInput.AddObject(HydCell.Name, HydCell);
        end;
      until (HydModFile.EndOfStream);
    finally
      Splitter.Free;
    end;
  finally
    HydModFile.Free;
  end;
  HydInput.Sorted := True;
//  SetLength(TestDistances, FFixedDistances.Count);
//  if HydInput.Count > 0 then
//  begin
//    HydCell := HydInput.Objects[0] as THydCell;
//    for DistIndex := 0 to FFixedDistances.Count - 1 do
//    begin
//      FixedDist := FFixedDistances[DistIndex];
//      TestDistances[DistIndex] := Abs(Abs(FCenterY-HydCell.Y)-FixedDist);
//      FixedDistanceCells[DistIndex] := HydCell;
//    end;
//    for CellIndex := 1 to HydInput.Count - 1 do
//    begin
//      HydCell := HydInput.Objects[CellIndex] as THydCell;
//      for DistIndex := 0 to FFixedDistances.Count - 1 do
//      begin
//        FixedDist := FFixedDistances[DistIndex];
//        TestDist := Abs(Abs(FCenterY-HydCell.Y)-FixedDist);
//        if TestDist < TestDistances[DistIndex] then
//        begin
//          TestDistances[DistIndex] := TestDist;
//          FixedDistanceCells[DistIndex] := HydCell;
//        end
//        else if (TestDist = TestDistances[DistIndex])
//          and (HydCell.Layer < FixedDistanceCells[DistIndex].Layer) then
//        begin
//          FixedDistanceCells[DistIndex] := HydCell;
//        end;
//      end;
//    end;
//  end;
end;

procedure TRunModelThread.RunModelMuse;
begin
  DirectoryLock.Acquire;
  try
    SetCurrentDir(FDirectory);
    FCreateProcess.CommandLine := '"' + FConstants.ModelMuseLocation + '" '
      + ExtractFileName(FModelMuseFileName) + ' -e -c';
    FCreateProcess.Run;
  finally
    DirectoryLock.Release;
  end;
  Suspended := True;
end;

procedure TRunModelThread.RunModflow;
begin
  DirectoryLock.Acquire;
  try
    SetCurrentDir(FDirectory);
    FCreateProcess.CommandLine := '"' + FConstants.ModflowLocation + '" '
      + ExtractFileName(ChangeFileExt(FModelMuseFileName, '.nam'));
    FCreateProcess.Run;
  finally
    DirectoryLock.Release;
  end;
  Suspended := True;
end;

{ TGwMoundFile }

procedure TGwMoundFile.Assign(Source: TPersistent);
var
  GwMoundSource: TGwMoundFile;
begin
  if Source is TGwMoundFile then
  begin
    GwMoundSource := TGwMoundFile(Source);
//    Area := GwMoundSource.Area;
//    AreaUnits := GwMoundSource.AreaUnits;
//    Fraction := GwMoundSource.Fraction;
//    DesignStorm := GwMoundSource.DesignStorm;
//    DesignStormUnits := GwMoundSource.DesignStormUnits;
//    VolumeUnits := GwMoundSource.VolumeUnits;
//    StormDuration := GwMoundSource.StormDuration;
//    StormDurationUnits := GwMoundSource.StormDurationUnits;
    Kv := GwMoundSource.Kv;
    KvUnits := GwMoundSource.KvUnits;
    Kx := GwMoundSource.Kx;
    SpecificYield := GwMoundSource.SpecificYield;
    DistanceToWaterTable := GwMoundSource.DistanceToWaterTable;
    DistanceToWaterTableUnits := GwMoundSource.DistanceToWaterTableUnits;
    AquiferThickness := GwMoundSource.AquiferThickness;
    AquiferThicknessUnits := GwMoundSource.AquiferThicknessUnits;
    SimulationLengthAnalytic := GwMoundSource.SimulationLengthAnalytic;
    SimulationLengthUnitsAnalytic := GwMoundSource.SimulationLengthUnitsAnalytic;
//    MaxBasinDepth := GwMoundSource.MaxBasinDepth;
    BasinDepth := GwMoundSource.BasinDepth;
    MaxBasinDepthUnits := GwMoundSource.MaxBasinDepthUnits;
    BasinAreaUnits := GwMoundSource.BasinAreaUnits;
    BasinShape := GwMoundSource.BasinShape;
    BasinDiameter := GwMoundSource.BasinDiameter;
    BasinDiameterUnits := GwMoundSource.BasinDiameterUnits;
    BasinSquareSide := GwMoundSource.BasinSquareSide;
    BasinSquareSideUnits := GwMoundSource.BasinSquareSideUnits;
    BasinRectangleLength := GwMoundSource.BasinRectangleLength;
    BasinRectangleWidth := GwMoundSource.BasinRectangleWidth;
    BasinRectangleUnits := GwMoundSource.BasinRectangleUnits;
    CustomBasin := GwMoundSource.CustomBasin;
    CustomBasinUnits := GwMoundSource.CustomBasinUnits;
    MaxDistance := GwMoundSource.MaxDistance;
    MaxDistanceUnits := GwMoundSource.MaxDistanceUnits;
    TimeIncrement := GwMoundSource.TimeIncrement;
    TimeIncrementUnits := GwMoundSource.TimeIncrementUnits;
    AnalyticResults := GwMoundSource.AnalyticResults;
    UnsaturatedFlow := GwMoundSource.UnsaturatedFlow;
    BrooksCorey := GwMoundSource.BrooksCorey;
    SaturatedWaterContent := GwMoundSource.SaturatedWaterContent;
    InitialWaterContent := GwMoundSource.InitialWaterContent;
    ModelMuseFileLocation := GwMoundSource.ModelMuseFileLocation;
    NumericSummaryTable := GwMoundSource.NumericSummaryTable;
    NumericProfile := GwMoundSource.NumericProfile;
    MaxDistanceNumeric := GwMoundSource.MaxDistanceNumeric;
    MaxDistanceNumericUnits := GwMoundSource.MaxDistanceNumericUnits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGwMoundFile.Create(AnOwner: TComponent);
begin
  inherited;
  FCustomBasin := TGridCellCollection.Create;
  FCustomBasin.Grid := frmGwMound.rdgBasinCoordinates;
  FAnalyticResults := TGridCellCollection.Create;
  FAnalyticResults.Grid := frmGwMound.rdgAnalytic;
  FNumericSummaryTable := TGridCellCollection.Create;
  FNumericSummaryTable.Grid := frmGwMound.rdgSummaryNumeric;
  FNumericProfile := TGridCellCollection.Create;
  FNumericProfile.Grid := frmGwMound.rdgProfileNumeric;
end;

destructor TGwMoundFile.Destroy;
begin
  FCustomBasin.Free;
  FAnalyticResults.Free;
  FNumericSummaryTable.Free;
  FNumericProfile.Free;
  inherited;
end;

procedure TGwMoundFile.SetUnits(Combo: TComboBox; const Value: string);
begin
  Combo.ItemIndex := Combo.Items.IndexOf(Value);
  Assert(Combo.ItemIndex >= 0);
end;

function TGwMoundFile.GetAnalyticResults: TGridCellCollection;
begin
  FAnalyticResults.UpdateItems;
  result := FAnalyticResults;
end;

function TGwMoundFile.GetAquiferThickness: double;
begin
  result := StrToFloat(frmGwMound.rdeAquiferThickness.Text);
end;

function TGwMoundFile.GetAquiferThicknessUnits: string;
begin
  result := frmGwMound.comboAquiferThicknessUnits.Text;
end;

//function TGwMoundFile.GetArea: Double;
//begin
//  result := StrToFloat(frmCalculateVolume.rdeDevelopmentArea.Text);
//end;

//function TGwMoundFile.GetAreaUnits: string;
//begin
//  result := frmCalculateVolume.comboDevelopmentAreaUnit.Text;
//end;

function TGwMoundFile.GetBasinAreaUnits: string;
begin
  result := frmGwMound.comboMinBasinAreaUnits.Text;
end;

function TGwMoundFile.GetBasinDepth: double;
begin
  result := StrToFloat(frmGwMound.rdeBasinDepth.Text);
end;

function TGwMoundFile.GetBasinDiameter: double;
begin
  result := StrToFloat(frmGwMound.rdeBasinCircleDiameter.Text);
end;

function TGwMoundFile.GetBasinDiameterUnits: string;
begin
  result := frmGwMound.comboBasinCircleDiameterUnits.Text;
end;

function TGwMoundFile.GetBasinRectangleLength: double;
begin
  result := StrToFloat(frmGwMound.rdeRectBasinLength.Text);
end;

function TGwMoundFile.GetBasinRectangleUnits: string;
begin
  result := frmGwMound.comboRectUnits.Text;
end;

function TGwMoundFile.GetBasinRectangleWidth: double;
begin
  result := StrToFloat(frmGwMound.rdeRectBasinWidth.Text);
end;

function TGwMoundFile.GetBasinShape: TBasinShape;
begin
  result := TBasinShape(frmGwMound.rgBasinShape.ItemIndex);
end;

function TGwMoundFile.GetBasinSquareSide: double;
begin
  result := StrToFloat(frmGwMound.rdeSquareLength.Text);
end;

function TGwMoundFile.GetBasinSquareSideUnits: string;
begin
  result := frmGwMound.comboSquareLengthUnits.Text;
end;

function TGwMoundFile.GetBrooksCorey: Double;
begin
  result := StrToFloat(frmGwMound.rdeBrooksCoreyEpsilon.Text);
end;

function TGwMoundFile.GetCustomBasin: TGridCellCollection;
begin
  FCustomBasin.UpdateItems;
  result := FCustomBasin;
end;

function TGwMoundFile.GetCustomBasinUnits: string;
begin
  result := frmGwMound.comboCustomUnits.Text;
end;

//function TGwMoundFile.GetDesignStorm: Double;
//begin
//  result := StrToFloat(frmCalculateVolume.rdeDesignStorm.Text);
//end;
//
//function TGwMoundFile.GetDesignStormUnits: string;
//begin
//  result := frmCalculateVolume.comboDesignStormUnits.Text;
//end;

function TGwMoundFile.GetDistanceToWaterTable: double;
begin
  result := StrToFloat(frmGwMound.rdeHeightAboveWaterTable.Text);
end;

function TGwMoundFile.GetDistanceToWaterTableUnits: string;
begin
  result := frmGwMound.comboHeightAboveWaterTableUnits.Text;
end;

//function TGwMoundFile.GetFraction: Double;
//begin
//  result := StrToFloat(frmCalculateVolume.rdeImperviousFraction.Text);
//end;

function TGwMoundFile.GetInfiltrationTime: Double;
begin
  result := StrToFloat(frmGwMound.rdeDurationOfInfiltration.Text);
end;

function TGwMoundFile.GetInfiltrationTimeUnits: string;
begin
  result := frmGwMound.comboDurationOfInfiltration.Text;
end;

function TGwMoundFile.GetInitialWaterContent: double;
begin
  result := StrToFloat(frmGwMound.rdeInitialWaterContent.Text);
end;

function TGwMoundFile.GetKv: Double;
begin
  result := StrToFloat(frmGwMound.rdeKz.Text);
end;

function TGwMoundFile.GetKvUnits: string;
begin
  result := frmGwMound.comboKzUnits.Text;
end;

//function TGwMoundFile.GetMaxBasinDepth: double;
//begin
//  result := StrToFloat(frmGwMound.rdeMaxDepth.Text);
//end;
//
function TGwMoundFile.GetMaxBasinDepthUnits: string;
begin
  result := frmGwMound.comboMaxDepthUnits.Text;
end;

function TGwMoundFile.GetMaxDistance: double;
begin
  result := StrToFloat(frmGwMound.rdeMaxDistance.Text);
end;

function TGwMoundFile.GetMaxDistanceNumeric: double;
begin
  result := StrToFloat(frmGwMound.rdeMaxDistanceNumeric.Text);
end;

function TGwMoundFile.GetMaxDistanceNumericUnits: string;
begin
  Result := frmGwMound.comboMaxNumericDistanceUnits.Text
end;

function TGwMoundFile.GetMaxDistanceUnits: string;
begin
  result := frmGwMound.comboMaxDistanceUnits.Text;
end;

function TGwMoundFile.GetModelMuseFileLocation: string;
begin
  result := frmGwMound.feModelMuseFile.FileName;
end;

function TGwMoundFile.GetNumericProfile: TGridCellCollection;
begin
  FNumericProfile.UpdateItems;
  result := FNumericProfile;
end;

function TGwMoundFile.GetNumericSummaryTable: TGridCellCollection;
begin
  FNumericSummaryTable.UpdateItems;
  result := FNumericSummaryTable;
end;

function TGwMoundFile.GetSaturatedWaterContent: double;
begin
  result := StrToFloat(frmGwMound.rdeSaturatedWaterFraction.Text);
end;

function TGwMoundFile.GetSimulationLengthAnalytic: double;
begin
  result := StrToFloat(frmGwMound.rdeSimulationLengthAnalytic.Text);
end;

function TGwMoundFile.GetSimulationLengthNumeric: double;
begin
  result := StrToFloat(frmGwMound.rdeSimulationLengthNumeric.Text);
end;

function TGwMoundFile.GetSimulationLengthUnitsAnalytic: string;
begin
  result := frmGwMound.comboSimulationLengthUnitsAnalytic.Text;
end;

function TGwMoundFile.GetSimulationLengthUnitsNumeric: string;
begin
  result := frmGwMound.comboSimulationLengthUnitsNumeric.Text;
end;

function TGwMoundFile.GetSpecificYield: Double;
begin
  result := StrToFloat(frmGwMound.rdeSpecificYield.Text);
end;

//function TGwMoundFile.GetStormDuration: Double;
//begin
//  result := StrToFloat(frmGwMound.rdeDuration.Text);
//end;

//function TGwMoundFile.GetStormDurationUnits: string;
//begin
//  result := frmGwMound.comboDuration.Text;
//end;

function TGwMoundFile.GetTimeIncrement: double;
begin
  result := StrToFloat(frmGwMound.rdeTimeIncrement.Text);
end;

function TGwMoundFile.GetTimeIncrementUnits: string;
begin
  result := frmGwMound.comboTimeIncrementUnits.Text;
end;

function TGwMoundFile.GetUnsaturatedFlow: Boolean;
begin
  result := frmGwMound.cbSimulateUnsat.Checked;
end;

function TGwMoundFile.GetVertAnisotropy: Double;
begin
  result := StrToFloat(frmGwMound.rdeKx.Text);
end;

//function TGwMoundFile.GetVolume: Double;
//begin
//  result := StrToFloat(frmGwMound.rdeVolume.Text)
//end;
//
//function TGwMoundFile.GetVolumeUnits: string;
//begin
//  result := frmGwMound.comboVolumeUnits.Text;
//end;

procedure TGwMoundFile.Loaded;
begin
  inherited;
  FCustomBasin.UpdateGrid;
  FAnalyticResults.UpdateGrid;
  FNumericSummaryTable.UpdateGrid;
  FNumericProfile.UpdateGrid;
end;

procedure TGwMoundFile.SetAnalyticResults(const Value: TGridCellCollection);
begin
  FAnalyticResults.Assign(Value);
  FAnalyticResults.UpdateGrid;
end;

procedure TGwMoundFile.SetAquiferThickness(const Value: double);
begin
  frmGwMound.rdeAquiferThickness.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetAquiferThicknessUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboAquiferThicknessUnits, Value);
end;

//procedure TGwMoundFile.SetArea(const Value: Double);
//begin
//  frmCalculateVolume.rdeDevelopmentArea.Text := FloatToStr(Value);
//end;

//procedure TGwMoundFile.SetAreaUnits(const Value: string);
//begin
//  SetUnits(frmCalculateVolume.comboDevelopmentAreaUnit, Value);
//end;

procedure TGwMoundFile.SetBasinAreaUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboMinBasinAreaUnits, Value);
end;

procedure TGwMoundFile.SetBasinDepth(const Value: double);
begin
  frmGwMound.rdeBasinDepth.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetBasinDiameter(const Value: double);
begin
  frmGwMound.rdeBasinCircleDiameter.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetBasinDiameterUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboBasinCircleDiameterUnits, Value);
end;

procedure TGwMoundFile.SetBasinRectangleLength(const Value: double);
begin
  frmGwMound.rdeRectBasinLength.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetBasinRectangleUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboRectUnits, Value);
end;

procedure TGwMoundFile.SetBasinRectangleWidth(const Value: double);
begin
  frmGwMound.rdeRectBasinWidth.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetBasinShape(const Value: TBasinShape);
begin
  frmGwMound.rgBasinShape.ItemIndex := Ord(Value);
end;

procedure TGwMoundFile.SetBasinSquareSide(const Value: double);
begin
  frmGwMound.rdeSquareLength.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetBasinSquareSideUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboSquareLengthUnits, Value);
end;

procedure TGwMoundFile.SetBrooksCorey(const Value: Double);
begin
  frmGwMound.rdeBrooksCoreyEpsilon.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetCustomBasin(const Value: TGridCellCollection);
begin
  FCustomBasin.Assign(Value);
  FCustomBasin.UpdateGrid;
end;

procedure TGwMoundFile.SetCustomBasinUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboCustomUnits, Value);
end;

//procedure TGwMoundFile.SetDesignStorm(const Value: Double);
//begin
//  frmCalculateVolume.rdeDesignStorm.Text := FloatToStr(Value);
//end;
//
//procedure TGwMoundFile.SetDesignStormUnits(const Value: string);
//begin
//  SetUnits(frmCalculateVolume.comboDesignStormUnits, Value);
//end;

procedure TGwMoundFile.SetDistanceToWaterTable(const Value: double);
begin
  frmGwMound.rdeHeightAboveWaterTable.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetDistanceToWaterTableUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboHeightAboveWaterTableUnits, Value);
end;

//procedure TGwMoundFile.SetFraction(const Value: Double);
//begin
//  frmCalculateVolume.rdeImperviousFraction.Text := FloatToStr(Value);
//end;

procedure TGwMoundFile.SetInfiltrationTime(const Value: Double);
begin
  frmGwMound.rdeDurationOfInfiltration.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetInfiltrationTimeUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboDurationOfInfiltration, Value);
end;

procedure TGwMoundFile.SetInitialWaterContent(const Value: double);
begin
  frmGwMound.rdeInitialWaterContent.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetKv(const Value: Double);
begin
  frmGwMound.rdeKz.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetKvUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboKzUnits, Value);
end;

//procedure TGwMoundFile.SetMaxBasinDepth(const Value: double);
//begin
//  frmGwMound.rdeMaxDepth.Text := FloatToStr(Value);
//end;
//
procedure TGwMoundFile.SetMaxBasinDepthUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboMaxDepthUnits, Value);
end;

procedure TGwMoundFile.SetMaxDistance(const Value: double);
begin
  frmGwMound.rdeMaxDistance.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetMaxDistanceNumeric(const Value: double);
begin
  frmGwMound.rdeMaxDistanceNumeric.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetMaxDistanceNumericUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboMaxNumericDistanceUnits, Value);
end;

procedure TGwMoundFile.SetMaxDistanceUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboMaxDistanceUnits, Value);
end;

procedure TGwMoundFile.SetModelMuseFileLocation(const Value: string);
begin
  frmGwMound.feModelMuseFile.FileName := Value;
end;

procedure TGwMoundFile.SetNumericProfile(const Value: TGridCellCollection);
begin
  FNumericProfile.Assign(Value);
  FNumericProfile.UpdateGrid;
end;

procedure TGwMoundFile.SetNumericSummaryTable(const Value: TGridCellCollection);
begin
  FNumericSummaryTable.Assign(Value);
  FNumericSummaryTable.UpdateGrid;
end;

procedure TGwMoundFile.SetSaturatedWaterContent(const Value: double);
begin
  frmGwMound.rdeSaturatedWaterFraction.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetSimulationLengthAnalytic(const Value: double);
begin
  frmGwMound.rdeSimulationLengthAnalytic.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetSimulationLengthNumeric(const Value: double);
begin
  frmGwMound.rdeSimulationLengthNumeric.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetSimulationLengthUnitsAnalytic(const Value: string);
begin
  SetUnits(frmGwMound.comboSimulationLengthUnitsAnalytic, Value);
end;

procedure TGwMoundFile.SetSimulationLengthUnitsNumeric(const Value: string);
begin
  SetUnits(frmGwMound.comboSimulationLengthUnitsNumeric, Value);
end;

procedure TGwMoundFile.SetSpecificYield(const Value: Double);
begin
  frmGwMound.rdeSpecificYield.Text := FloatToStr(Value);
end;

//procedure TGwMoundFile.SetStormDuration(const Value: Double);
//begin
//  frmGwMound.rdeDuration.Text := FloatToStr(Value);
//end;
//
//procedure TGwMoundFile.SetStormDurationUnits(const Value: string);
//begin
//  SetUnits(frmGwMound.comboDuration, Value);
//end;

procedure TGwMoundFile.SetTimeIncrement(const Value: double);
begin
  frmGwMound.rdeTimeIncrement.Text := FloatToStr(Value);
end;

procedure TGwMoundFile.SetTimeIncrementUnits(const Value: string);
begin
  SetUnits(frmGwMound.comboTimeIncrementUnits, Value);
end;

procedure TGwMoundFile.SetUnsaturatedFlow(const Value: Boolean);
begin
  frmGwMound.cbSimulateUnsat.Checked := Value;
end;

procedure TGwMoundFile.SetVertAnisotropy(const Value: Double);
begin
  frmGwMound.rdeKx.Text := FloatToStr(Value);
end;

//procedure TGwMoundFile.SetVolume(const Value: Double);
//begin
//  frmGwMound.rdeVolume.Text := FloatToStr(Value)
//end;
//
//procedure TGwMoundFile.SetVolumeUnits(const Value: string);
//begin
//  SetUnits(frmGwMound.comboVolumeUnits, Value);
//end;

{ TGridCell }

procedure TGridCell.Assign(Source: TPersistent);
var
  GSSource: TGridCell;
begin
  if Source is TGridCell then
  begin
    GSSource := TGridCell(Source);
    Column := GSSource.Column;
    Row := GSSource.Row;
    Value := GSSource.Value;
  end
  else
  begin
    inherited;
  end;
end;

{ TGridCellCollection }

function TGridCellCollection.Add: TGridCell;
begin
  Result := inherited Add as TGridCell;
end;

procedure TGridCellCollection.Assign(Source: TPersistent);
var
  GCSource: TGridCellCollection;
begin
  if Source is TGridCellCollection then
  begin
    GCSource := TGridCellCollection(Source);
    RowCount := GCSource.RowCount;
    ColCount := GCSource.ColCount;
  end;
  inherited;
end;

constructor TGridCellCollection.Create;
begin
  inherited Create(TGridCell);
end;

function TGridCellCollection.GetColCount: Integer;
begin
  Assert(Assigned(FGrid));
  result := FGrid.ColCount;
end;

function TGridCellCollection.GetItem(Index: Integer): TGridCell;
begin
  Result := inherited Items[Index] as TGridCell;
end;

function TGridCellCollection.GetRowCount: Integer;
begin
  Assert(Assigned(FGrid));
  result := FGrid.RowCount;
end;

procedure TGridCellCollection.RestoreValues;
var
  index: Integer;
  Item: TGridCell;
begin
  Assert(Assigned(FGrid));
  for index := 0 to Count - 1 do
  begin
    Item := Items[index];
    FGrid.Cells[Item.Column, Item.Row] := Item.Value;
  end;
end;

procedure TGridCellCollection.SetColCount(const Value: Integer);
begin
  Assert(Assigned(FGrid));
  FGrid.ColCount := Value;
end;

procedure TGridCellCollection.SetItem(Index: Integer; const Value: TGridCell);
begin
  inherited Items[Index] := Value;
end;

procedure TGridCellCollection.SetRowCount(const Value: Integer);
begin
  Assert(Assigned(FGrid));
  FGrid.RowCount := Value;
end;

procedure TGridCellCollection.UpdateGrid;
var
  index: Integer;
  Item: TGridCell;
begin
  Assert(Assigned(FGrid));
  Grid.BeginUpdate;
  try
    for index := 0 to Grid.ColCount - 1 do
    begin
      Grid.Columns[index].AutoAdjustColWidths := True;
    end;
    for index := 0 to Count - 1 do
    begin
      Item := Items[index];
      Grid.Cells[Item.Column, Item.Row] := Item.Value;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TGridCellCollection.UpdateItems;
var
  RowIndex: Integer;
  ColIndex: Integer;
  Item: TGridCell;
begin
  Assert(Assigned(FGrid));
  Clear;
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Item := Add;
      Item.Column := ColIndex;
      Item.Row := RowIndex;
      Item.Value := Grid.Cells[ColIndex, RowIndex];
    end;
  end;
end;

initialization
  InitializeVelConversion;
  DirectoryLock := TCriticalSection.Create;

finalization
  DirectoryLock.Free;


end.
