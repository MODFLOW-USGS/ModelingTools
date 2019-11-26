unit frmModChartUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TeeProcs, TeEngine, Chart, ExtCtrls, Series, Math, Buttons,
  Menus, IntListUnit, ComCtrls, ToolWin, contnrs, MyFormUnit, TeeEdit,
  TeeComma, TeeScroB, ReaderUnit, ChartRangeUnit, TeeTools, hh_funcs,
  JvPageList, JvExControls, addbtn95, ArgusDataEntry, CheckLst, basecombo,
  treecombo, Mask, JvExMask, JvSpin, BubbleCh, Grids, RbwDataGrid4,
  JvExCheckLst, JvCheckListBox, ObjectStringList, JvxCheckListBox, StatChar,
  RealListUnit;

type
  TFileType = (ftUnknown, ft_os, ft_ww, ft_ws, ft_sc, ft_sd, ft_s1, ft_nm,
    ft_rd, ft_rg, ft_rb, ft_rc, ft_b, ft_pa, ft_pc, ft_ss, ft_rdadv, ft_so,
    ft_ppr, ft_opr, ft_ppr_abschg, ft_ppa, ft_ppa_abschg, ft_opr_abschg,
    ft_opa, ft_opa_abschg, ft_sppp, ft_spsp, ft_sppr, ft_spsr, ft_pcc, ft_linp,
    ft_intconf, ft_intpred, ft_xyztwr, ft_sc_svd, ft_scgrp, ft_mcmc_grr,
    ft_mcmc_pred, ft_mcmc_par, ft_svd);

  TModelSelection = (msModflow, msUcode2005, msModflow2005);
  TPcPlotChoice = (pcMeanCiRange, pcMeanCi, pcCoefVarModel, pcCoefVarPar,
    pcInverse, pcInverseTimesStandError);
  TGroupType = (gtObservation, gtPrediction);

  TString14 = string[14];

  TGroupSort = (gsPlotSymbol, gsName);

  TfrmModChart = class(TMyForm)
    chartModflow: TChart;
    OpenDialogOutputFile: TOpenDialog;
    SerDataPoints: TPointSeries;
    ser1to1: TLineSeries;
    serBarChart: TBarSeries;
    MainMenu1: TMainMenu;
    miFile1: TMenuItem;
    miExit1: TMenuItem;
    miAbout1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    miOpen: TMenuItem;
    miPrintSetup: TMenuItem;
    miPrint: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    miDockToolbar: TMenuItem;
    ToolBar1: TToolBar;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    sbPan: TSpeedButton;
    cbLabelPoints: TCheckBox;
    sbOpenFile: TSpeedButton;
    sbPrint: TSpeedButton;
    sbFormat: TSpeedButton;
    BitBtnClose: TBitBtn;
    miConfigure: TMenuItem;
    miZoomin: TMenuItem;
    miZoomextents: TMenuItem;
    miPan: TMenuItem;
    miSaveChart: TMenuItem;
    miFormatchart: TMenuItem;
    pnlFile: TPanel;
    mainMenuFormChoice: TMainMenu;
    WaterBudgets1: TMenuItem;
    FormType1: TMenuItem;
    Hydrographs1: TMenuItem;
    CalibrationPlots1: TMenuItem;
    LakePlots1: TMenuItem;
    PiperDiagram1: TMenuItem;
    SelectFileType1: TMenuItem;
    sbSaveImage: TSpeedButton;
    Help1: TMenuItem;
    Help2: TMenuItem;
    ChartEditor1: TChartEditor;
    ChartPreviewer1: TChartPreviewer;
    miPrintPreview: TMenuItem;
    sbOldFormatChart: TSpeedButton;
    Formatchartoldstyle1: TMenuItem;
    CellWaterBudgets1: TMenuItem;
    LineAt1: TLineSeries;
    jvplChartControls: TJvPageList;
    jvspChartType: TJvStandardPage;
    rbPlotAverage: TRadioButton95;
    rbPlotSeries: TRadioButton95;
    jvspDataOrder: TJvStandardPage;
    OpenDialog1: TOpenDialog;
    jvsp_b_pa: TJvStandardPage;
    cbDivideParameterValues: TCheckBox95;
    cbShowAllParameters: TCheckBox95;
    jvsp_os: TJvStandardPage;
    cbRecentModflow: TCheckBox95;
    jvsp_pc: TJvStandardPage;
    jvsp_rdrg: TJvStandardPage;
    cbPlot_NmFile: TCheckBox95;
    clbSeriesList: TCheckListBox;
    treecomboFileNames: TksoTreeComboBox;
    jvsp_linp: TJvStandardPage;
    jvsp_intconf: TJvStandardPage;
    sbRefresh: TSpeedButton;
    pcIntConf: TPageControl;
    tabIntconfControls: TTabSheet;
    cbScale_intconf: TCheckBox95;
    rgIntConfWhatToPlot: TRadioGroup;
    tabWarnings: TTabSheet;
    lblRedCI: TLabel;
    lblOrangeCI: TLabel;
    reIntconfFiles: TRichEdit;
    lblAdditionalFiles: TLabel;
    btnIntConfBrowse: TButton;
    odIntConf: TOpenDialog;
    jvsp_xyztwr: TJvStandardPage;
    pcXyzt: TPageControl;
    tabDataControls: TTabSheet;
    rgXAxis: TRadioGroup;
    rgYAxis: TRadioGroup;
    rgDataToPlot: TRadioGroup;
    tabOptions: TTabSheet;
    lblPlotSymbolsToPlot: TLabel;
    clbItemsToPlot: TCheckListBox;
    seRadius: TJvSpinEdit;
    lblRadiusFactor: TLabel;
    cbShowLabels: TCheckBox;
    cbEarliestTimePlotted: TCheckBox95;
    cbLatestTimePlotted: TCheckBox95;
    adeEarliest: TArgusDataEntry;
    adeLatest: TArgusDataEntry;
    Timer1: TTimer;
    lblBlueRedDescripton: TLabel;
    jvspScSo: TJvStandardPage;
    rgScSoSorting: TRadioGroup;
    jvspPcc: TJvStandardPage;
    lblDescription: TLabel;
    treeGroups1: TTreeView;
    jvspPpaAbschg: TJvStandardPage;
    rgItemsToPlot_So: TRadioGroup;
    seN_so: TJvSpinEdit;
    lblN_so: TLabel;
    jvspRc: TJvStandardPage;
    rgItemsToPlot_Rc: TRadioGroup;
    seN_Rc: TJvSpinEdit;
    lblN_Rc: TLabel;
    pcPPR: TPageControl;
    tabControls: TTabSheet;
    tabGroups: TTabSheet;
    rgOrderPPR: TRadioGroup;
    rgItemsToPlotPPR: TRadioGroup;
    seN_PPR: TJvSpinEdit;
    lblN_PPR: TLabel;
    clb_OsPlotsymbols: TCheckListBox;
    Label1: TLabel;
    lblStandardErrorOfRegression: TLabel;
    adeStandardError: TArgusDataEntry;
    btnRead: TButton;
    pc_NM: TPageControl;
    tabDataOrderControls: TTabSheet;
    spN: TJvSpinEdit;
    lblN: TLabel;
    rgItemsToPlot: TRadioGroup;
    rgPointOrder: TRadioGroup;
    rgOrder: TRadioGroup;
    rgWhatToPlot: TRadioGroup;
    lblDataOrder: TLabel;
    tabSeries: TTabSheet;
    rgSeriesToPlot: TRadioGroup;
    seNumberOfSeries: TJvSpinEdit;
    lblNumberOfSeries: TLabel;
    clbSeriesToPlot: TCheckListBox;
    jvsp_nm: TJvStandardPage;
    clbNM: TCheckListBox;
    lblPlotSymbolsNM: TLabel;
    pcLinp: TPageControl;
    tabLinpControls: TTabSheet;
    cbScale_linp: TCheckBox95;
    rgIntervalType: TRadioGroup;
    GroupBox1: TGroupBox;
    rbIndividual: TRadioButton95;
    rbSimulataneous: TRadioButton95;
    rbUndefinedSimulataneous: TRadioButton95;
    tabLinpPlotSymbols: TTabSheet;
    clbLinpPlotSymbol: TCheckListBox;
    serBubbleLegend: TBubbleSeries;
    tabLegend: TTabSheet;
    rdgXyztLegend: TRbwDataGrid4;
    cbXyztLegend: TCheckBox;
    seXyztLegendCount: TJvSpinEdit;
    lblXyztLegendCount: TLabel;
    ConvertCellWaterBudgets1: TMenuItem;
    pgcParameters: TPageControl;
    tsBasic: TTabSheet;
    treeGroups2: TTreeView;
    rgOrder2: TRadioGroup;
    rgItemsToPlotAbsChg: TRadioGroup;
    lblN_AbsChg: TLabel;
    seN_AbsChg: TJvSpinEdit;
    tabParameters: TTabSheet;
    jvchklstParameters: TJvCheckListBox;
    serBarChart2: TBarSeries;
    cbRatio: TCheckBox;
    jvsp_mcmc_par: TJvStandardPage;
    rgMcmcGraphType: TRadioGroup;
    lblParameter: TLabel;
    lst_mcmcParameters: TJvxCheckListBox;
    seMcmcPercent: TJvSpinEdit;
    lblMcmcPercent: TLabel;
    lblMcmcBins: TLabel;
    seMcmcBins: TJvSpinEdit;
    jvsp_mcmc_grr: TJvStandardPage;
    rdeRThreshold: TRbwDataEntry;
    lblRThreshold: TLabel;
    jvsp_mcmc_pred: TJvStandardPage;
    rgMcmc_predGraphType: TRadioGroup;
    jvpl_mcmc_pred: TJvPageList;
    jvsp_mcmc_predHistogram: TJvStandardPage;
    lbl_mcmc_prdBinCount: TLabel;
    se_mcmc_prdBinCount: TJvSpinEdit;
    chcklstbx_Mcmc_pred: TJvxCheckListBox;
    jvsp_mcmc_predProbability: TJvStandardPage;
    rgConfidenceInterval: TRadioGroup;
    rdeConfidenceInterval: TRbwDataEntry;
    pgc_PC: TPageControl;
    tabWhatToPlot: TTabSheet;
    tabInfo: TTabSheet;
    lblPcExpl1: TLabel;
    lblPcExpl2: TLabel;
    gbWhatToPlotPC: TGroupBox;
    rbMeanCiRangeForPC: TRadioButton95;
    rbCoefVarReg: TRadioButton95;
    rbCoerVarPar: TRadioButton95;
    rbMeanCiForPC: TRadioButton95;
    rbInverseCoef: TRadioButton95;
    rb_b_sd_s: TRadioButton95;
    cbScaleValues: TCheckBox95;
    jvsp_svd: TJvStandardPage;
    rgWhatToPlot_svd: TRadioGroup;
    rgRc_PlotOrder: TRadioGroup;
    cbLinpConfidenceIntervals: TCheckBox;
    cbLinpPlotStandardDev: TCheckBox;
    lblIgnoreValueOS: TLabel;
    lblValuesToIgnoreNM: TLabel;
    rdgIgnoreValueOS: TRbwDataGrid4;
    seIgnoreValueOS: TJvSpinEdit;
    seValuesToIgnoreNM: TJvSpinEdit;
    rdgValuesToIgnoreNM: TRbwDataGrid4;
    miFarmBudgets: TMenuItem;
    tmrRedrawDelay: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure SerDataPointsClickPointer(Sender: TCustomSeries; ValueIndex, X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure chartModflowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chartModflowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure chartModflowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbPanClick(Sender: TObject);
    procedure miExit1Click(Sender: TObject);
    procedure miAbout1Click(Sender: TObject);
    procedure miPrintSetupClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure cbLabelPointsClick(Sender: TObject);
    procedure sbFormatClick(Sender: TObject);
    procedure sbSaveImageClick(Sender: TObject);
    function SerDataPointsGetPointerStyle(Sender: TChartSeries;
      ValueIndex: Integer): TSeriesPointerStyle;
    procedure SerDataPointsGetMarkText(Sender: TChartSeries; ValueIndex: Integer;
      var MarkText: String);
    procedure chartModflowGetLegendRect(Sender: TCustomChart;
      var Rect: TRect);
    procedure chartModflowGetLegendPos(Sender: TCustomChart;
      Index: Integer; var X, Y, XColor: Integer);
    procedure chartModflowAfterDraw(Sender: TObject);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure Panel1DockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure miDockToolbarClick(Sender: TObject);
    procedure ToolBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure FormChoiceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolBar1EndDock(Sender, Target: TObject; X, Y: Integer);
    procedure Help2Click(Sender: TObject);
    procedure chartModflowGetAxisLabel(Sender: TChartAxis;
      Series: TChartSeries; ValueIndex: Integer; var LabelText: String);
    procedure miPrintPreviewClick(Sender: TObject);
    procedure sbOldFormatChartClick(Sender: TObject);
    procedure chartModflowDrawAxisLabel(Sender: TChartAxis; X, Y,
      Z: Integer; AxisLabel: String; var DrawLabel: Boolean);
    procedure chartModflowClick(Sender: TObject);
    procedure RedrawChart(Sender: TObject);
    procedure rgWhatToPlotClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure adeStandardErrorChange(Sender: TObject);
    procedure clbSeriesListClickCheck(Sender: TObject);
    procedure treecomboFileNamesChange(Sender: TObject);
    procedure rgItemsToPlotClick(Sender: TObject);
    procedure spNChange(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
    procedure btnIntConfBrowseClick(Sender: TObject);
    procedure rgXAxisClick(Sender: TObject);
    procedure cbEarliestTimePlottedClick(Sender: TObject);
    procedure cbLatestTimePlottedClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DelayRedrawChart(Sender: TObject);
    procedure rgYAxisClick(Sender: TObject);
    procedure rgItemsToPlot_SoClick(Sender: TObject);
    procedure rgItemsToPlot_RcClick(Sender: TObject);
    procedure rgItemsToPlotPPRClick(Sender: TObject);
    procedure rgItemsToPlotAbsChgClick(Sender: TObject);
    procedure rbPlotAverageClick(Sender: TObject);
    procedure rbPlotSeriesClick(Sender: TObject);
    procedure rgSeriesToPlotClick(Sender: TObject);
    procedure clbSeriesToPlotClickCheck(Sender: TObject);
    procedure clbLinpPlotSymbolClickCheck(Sender: TObject);
    procedure chartModflowGetSymbolSize(Sender: TCustomChart;
      const ValueIndex: integer; var SymbolSize: Integer);
    procedure seXyztLegendCountChange(Sender: TObject);
    procedure cbXyztLegendClick(Sender: TObject);
    procedure rdgXyztLegendEndUpdate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure rdgXyztLegendSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure jvchklstParametersClickCheck(Sender: TObject);
    procedure cbRatioClick(Sender: TObject);
    procedure rgMcmcGraphTypeClick(Sender: TObject);
    procedure lst_mcmcParametersClickCheck(Sender: TObject);
    procedure chcklstbx_Mcmc_predClickCheck(Sender: TObject);
    procedure rgMcmc_predGraphTypeClick(Sender: TObject);
    procedure rgConfidenceIntervalClick(Sender: TObject);
    procedure rdeConfidenceIntervalChange(Sender: TObject);
    procedure chartModflowGetNextAxisLabel(Sender: TChartAxis;
      LabelIndex: Integer; var LabelValue: Double; var Stop: Boolean);
    procedure cbLinpConfidenceIntervalsClick(Sender: TObject);
    procedure cbLinpPlotStandardDevClick(Sender: TObject);
    procedure seIgnoreValueOSChange(Sender: TObject);
    procedure rdgIgnoreValueOSEndUpdate(Sender: TObject);
    procedure seValuesToIgnoreNMChange(Sender: TObject);
    procedure rdgValuesToIgnoreNMEndUpdate(Sender: TObject);
    procedure tmrRedrawDelayTimer(Sender: TObject);
    procedure clb_OsPlotsymbolsClickCheck(Sender: TObject);
    procedure clbItemsToPlotClick(Sender: TObject);
  private
    FNameOfFile: string;
    MakeAxesAutomatic: Boolean;
    procedure SetNameOfFile(const Value: string);
    procedure SetMaxAndMinAxisValues(Axis: TChartAxis; NewMax, NewMin: double);
    procedure SetTreeComboDropDownWidth;
    function FileNameToFileType(const FileName: string): TFileType;
    procedure FillTreeCombo;
    procedure EnableControls;
    procedure Read_sc_svd;
    procedure Read_sc_svd_Data(ValueList: TObjectList;
      SortValues: boolean);
    procedure Read_nm(PlotAll: boolean);
    function GetGroupLabels(GroupType: TGroupType; PlotSymbols: TIntegerList;
      GroupNames: TStringList): boolean;
    procedure ReplotXyztIfLegendComplete;
    procedure GetSymbolSize(const Radius: double; var SymbolSize: Integer);
    procedure UpdateXyztLegendAfterDelay;
    procedure Read_scgrp(ValueList: TList);
    procedure FixLabelSize(LabelText: string = '');
    procedure EnableXYZT_DataToPlot;
    procedure Read_mcmc_par(const FileName : string);
    procedure Read_mcmc_grr;
    procedure Read_mcmc_pred(const FileName : string);
    procedure GetFileSeriesNames(FileNames: TStringList; const FileName,
      SearchExtension: string);
    procedure AssignHistogramValues(AValuesList: TRealList;
      BinCount: Integer; AHistogram: THistogramSeries);
    procedure ReadStandardError2(const RegressionFileName: string;
      ShowWarning: boolean; out StandardErrorLine: string);
    procedure DelayRedraw;
  private
    OrangeY: integer;
    RedY: integer;
    DrawingChart : boolean;
    OpeningFile: boolean;
    OpeningFile2: boolean;
    TreeFileNode: TTreeNode;
    TreeFileNodeFileName: string;
    OriginalTreeComboWidth: integer;
    ModelSelection: TModelSelection;
    FFile : TextFile;
    ObservationNames, ParameterNames : TStringList;
    SeriesList : TStringList;
    ExtraSeriesList: TObjectList;
    RdRgStyleList: TList;
    rdRgNameList: TList;
    XStart, YStart, XEnd, YEnd : double;
    Begun : Boolean;
    MarkedPoints : TIntegerList;
    FMarkedPointsList: TList;
    LegendRect : TRect;
    ExplantionVisible : boolean;
    ExplanationTop, ExplanationLeft : integer;
    ExplanationFontHeight : integer;
    MenuItems : TObjectList;
    DivideByInitialValues : boolean;
    ShowAllParameters : boolean;
//    PlotVsTime: boolean;
    IsRecentFile: boolean;
    SdValueList : TObjectList;
    FileType: TFileType;
    RangeSeries: TRangeSeries;
    HorizRangeSeries: TRangeSeries;
    FileToOpen: string;
    UpperDevX: integer;
    UpperDevY: integer;
    UpperDevXColor: integer;
    FParamCount: integer;
    FReadingSD: boolean;
    FPlottingLinp: boolean;
    DeltaLegendX: integer;
    Fmcmc_Variables: TObjectStringList;
    FSvdExponent: integer;
    property NameOfFile: string read FNameOfFile write SetNameOfFile;
    function MultipleSeriesGetPointerStyle(Sender: TChartSeries;
      ValueIndex: Integer): TSeriesPointerStyle;
    Procedure Read_os(FileType: TFileType; Extension : string);
    Procedure Read_sc;
    procedure Read_sd(scValueList : TObjectList);
    procedure Read_rd_rg;
    procedure SetAxisLabels(Extension: string);
    procedure FillFileList(FileRoot: String);
    procedure OpenFile(const FileName: string);
    procedure Read_rc;
    procedure ChooseGraphType(Sender: TObject);
    procedure Read_b;
    procedure Read_scData(ValueList: TObjectList; var WithPrior: boolean);
    procedure Read_pa;
    procedure Read_pc;
    procedure Read_ss;
    procedure Read_rdadv;
    procedure Read_so;
    procedure Read_opr_ppr;
    procedure TitleCase(var AString: string);
    function ReadStandardError(out StanError: string): boolean;
    function ExtractQuotedText(var AString: string): string;
    function ExtractSpaceSeparatedValue(var AString: string): string;
    procedure ClearObservationNames;
    procedure ExtractQuotedStrings(const Line: string; Lines: TStringList);
    procedure ExtractSpaceSeparatedStrings(Line: string;
      Lines: TStringList);
    procedure SetModelSelection(const FileName: string);
    procedure ReadNdNpr(DmFileName: string; out ND, NPR: integer);
    procedure MultipleSeriesClickPointer(Sender: TCustomSeries; ValueIndex,
      X, Y: Integer);
    procedure MultipleSeriesGetMarkText(Sender: TChartSeries;
      ValueIndex: Integer; var MarkText: String);
    procedure Read_ppa_abschg;
    procedure Read_PPR_Files(ChartType: integer; Tree: TTreeView);
    procedure Read_pcc;
    procedure Read_linp;
    procedure Read_intconf_intpred;
    procedure Read_xyztwr;
    procedure chartModflowIntPredGetLegendPos(Sender: TCustomChart;
      Index: Integer; var X, Y, XColor: Integer);
    procedure MakeAxesVariablesDifferent;
    // @name files GroupPlotSymbols with TGroupIndex objects in the
    // Objects properties.
    procedure GetGroupNamesFromGmFile(GroupPlotSymbols: TObjectStringList;
      SortBy: TGroupSort);
    procedure Read_svd;
    { Private declarations }
  public
    { Public declarations }
  end;

  TStyleType = class(TObject)
    Style : TSeriesPointerStyle;
  end;

  TFirstValueLineSeries = class(TLineSeries)
  public
    FirstValue : double;
  end;

type
  TNonLinConfInt = class(TObject)
    Name: string;
    Converged: boolean;
    PredictedValue: double;
    LowerLimit: double;
    UpperLimit: double;
    Simultaneous: boolean;
    UpperPercentDeviation: double;
    LowerPercentDeviation: double;
  end;

resourcestring
  StrExplanation = 'EXPLANATION';
  StrExplanationUnderscore = '_EXPLANATION_';

var
  frmModChart: TfrmModChart;
  mHHelp: THookHelpSystem;

const
//  MaxColor = 16;
//  MyColors : array[0..MaxColor] of TColor = (clBlack, clMaroon, clGreen,
//    clOlive, clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime,
//    clYellow, clBlue, clFuchsia, clAqua, clLtGray, clDkGray);
  ModflowObservationNameMaxLength = 14;
  UCodeObservationNameMaxLength = 20;

function ChartHelpFileName: string;
function HelpFileName: string;
procedure IncrementStyle(var MyStyle: TSeriesPointerStyle);
procedure IncrementLineType(var LineType: TPenStyle);
procedure IncrementColorIndex(var ColorIndex: integer);


Const
    Orange = TColor($00A5FF);
    Plum = TColor($DDA0DD);
    Royalblue = TColor($E16941);

  MaxDefaultColors=19;
  ColorPalette:Array[0..MaxDefaultColors-1] of TColor=
        ( clRed,
          clGreen,
          clBlue,
          clGray,
          clFuchsia,
          clTeal,
          clNavy,
          clMaroon,
          clLime,
          clOlive,
          clPurple,
          Orange, //clSilver,
          clAqua,
          clBlack,
          clMoneyGreen,
          clSkyBlue,
          Plum, //clCream,
          clMedGray,
          Royalblue //clYellow
          );
type
  TFileClassification = record
    Extension: string;
    MenuCaption: string;
    Classification: string;
    FilterDescription: string;
  end;

const
  ModelFit = 'Model Fit';
  OPSensAnalysis = 'O-Par Sens Analysis';
  ParameterValues = 'Parameter Values';
//  SensAnalysisForPredictions = 'Sens Analysis for Predictions';
  ParameterImportanceToPredictions = 'Parameter Importance to Predictions';
  ObservationImportanceToPredictions = 'Observation Importance to Predictions';
  PredictionScaledSensitivities = 'Par-Pred Sens Analysis';
  UncertaintyPar = 'Uncertainty-Par';
  UncertaintyPred = 'Uncertainty-Pred';
  McmcClassification = 'Bayesian Unc Analysis by MCMC';

  MaxExtensions = 40;
  Classifiations: array[0..MaxExtensions] of TFileClassification =
    (
     (Extension: '._xyztwr';
      MenuCaption: '_xyztwr: Spatial-temporal residual distribution';
      Classification: ModelFit;
      FilterDescription: 'Spatial-temporal residual distribution'),

     (Extension: '._os';
      MenuCaption: '_os: Unweighted simulated equivalents and observations';
      Classification: ModelFit;
      FilterDescription: 'Unweighted sim. equivalents and obs.'),

     (Extension: '._ww';
      MenuCaption: '_ww: Weighted simulated equivalents and observations';
      Classification: ModelFit;
      FilterDescription: 'Weighted sim. equivalents and obs.'),

     (Extension: '._ws';
      MenuCaption: '_ws: Weighted simulated equivalents and residuals';
      Classification: ModelFit;
      FilterDescription: 'Weighted sim. equivalents and resid.'),

     (Extension: '._nm';
      MenuCaption: '_nm: Weighted residuals and probability plotting position';
      Classification: ModelFit;
      FilterDescription: 'Weighted res. and prob. plotting pos.'),

     (Extension: '._rd';
      MenuCaption: '_rd: Uncorrelated deviates and probability plotting position';
      Classification: ModelFit;
      FilterDescription: 'Uncorrelated deviates and prob. plot pos.'),

     (Extension: '._rg';
      MenuCaption: '_rg: Correlated deviates and probability plotting position';
      Classification: ModelFit;
      FilterDescription: 'Correlated deviates and prob. plot pos.'),

     (Extension: '._ss';
      MenuCaption: '_ss: Sum of squared weighted residuals per iteration';
      Classification: ModelFit;
      FilterDescription: 'Sum of squared weighted residuals per iter.'),

     (Extension: '._rdadv';
      MenuCaption: '_rdadv: UCODE Residual analysis';
      Classification: ModelFit;
      FilterDescription: 'Residual analysis'),

     (Extension: '._sd';
      MenuCaption: '_sd: Dimensionless scaled sensitivities';
      Classification: OPSensAnalysis;
      FilterDescription: 'Dimensionless scaled sensitivities'),

     (Extension: '._sc';
      MenuCaption: '_sc: Composite scaled sensitivities';
      Classification: OPSensAnalysis;
      FilterDescription: 'Composite scaled sensitivities'),

     (Extension: '._scgrp';
      MenuCaption: '_scgrp: CSS stacked by obs type';
      Classification: OPSensAnalysis;
      FilterDescription: 'CSS stacked by obs type'),

     (Extension: '._sc_svd';
      MenuCaption: '_sc_svd: Composite scaled sensitivities - Singular value decomposition';
      Classification: OPSensAnalysis;
      FilterDescription: 'Composite scaled sensitivities - Singular value decomposition'),

     (Extension: '._svd';
      MenuCaption: '_svd: Singular value decomposition';
      Classification: OPSensAnalysis;
      FilterDescription: 'Singular value decomposition'),

     (Extension: '._pcc';
      MenuCaption: '_pcc: Large parameter correlations';
      Classification: OPSensAnalysis;
      FilterDescription: 'Large parameter correlations'),

     (Extension: '._so';
      MenuCaption: '_so: Leverage';
      Classification: OPSensAnalysis;
      FilterDescription: 'Leverage'),

     (Extension: '._s1';
      MenuCaption: '_s1: One-percent scaled sensitivities';
      Classification: OPSensAnalysis;
      FilterDescription: 'One-percent scaled sensitivities'),

     (Extension: '._rb';
      MenuCaption: '_rb: DFBetas statistics';
      Classification: OPSensAnalysis;
      FilterDescription: 'DFBetas statistics'),

     (Extension: '._rc';
      MenuCaption: '_rc: Cook''s D';
      Classification: OPSensAnalysis;
      FilterDescription: 'Cook''s D'),

     (Extension: '._b';
      MenuCaption: '_b: Parameter values';
      Classification: ParameterValues;
      FilterDescription: 'Parameter values'),

     (Extension: '._pa';
      MenuCaption: '_pa: Values for each iteration';
      Classification: ParameterValues;
      FilterDescription: 'Values per iter'),

     (Extension: '._pc';
      MenuCaption: '_pc: Final values, conf intervals, etc';
      Classification: ParameterValues;
      FilterDescription: 'Final values, conf intervals, etc'),

     (Extension: '._ppr';
      MenuCaption: '_ppr: Percent change in prediction std. dev.';
      Classification: ParameterImportanceToPredictions;
      FilterDescription: 'Parameter-pct change in pred std dev'),

     (Extension: '._ppr_abschg';
      MenuCaption: '_ppr_abschg: absolute change in prediction std. dev.';
      Classification: ParameterImportanceToPredictions;
      FilterDescription: 'Parameter-abs change in pred std dev'),

     (Extension: '._ppa';
      MenuCaption: '_ppa: Percent change in parameter std. dev.';
      Classification: ParameterImportanceToPredictions;
      FilterDescription: 'Parameter-pct change in param std dev'),

     (Extension: '._ppa_abschg';
      MenuCaption: '_ppa_abschg: Absolute change in parameter std. dev.';
      Classification: ParameterImportanceToPredictions;
      FilterDescription: 'Parameter-Abs change in param std dev'),

     (Extension: '._opr';
      MenuCaption: '_opr: Percent change in prediction std. dev.';
      Classification: ObservationImportanceToPredictions;
      FilterDescription: 'Observation-Pct change in pred std dev'),

     (Extension: '._opr_abschg';
      MenuCaption: '_opr_abschg: Absolute change in prediction std. dev.';
      Classification: ObservationImportanceToPredictions;
      FilterDescription: 'Observation-Abs change in pred std dev'),

     (Extension: '._opa';
      MenuCaption: '_opa: Percent change in parameter std. dev.';
      Classification: ObservationImportanceToPredictions;
      FilterDescription: 'Observation-Pct change in param std dev'),

     (Extension: '._opa_abschg';
      MenuCaption: '_opa_abschg: Absolute change in parameter std. dev.';
      Classification: ObservationImportanceToPredictions;
      FilterDescription: 'Observation-Abs change in param std dev'),

     (Extension: '._sppp';
//      MenuCaption: '_sppp: Prediction Scaled Sensitivities-Parameter Value-Predicted Value';
      MenuCaption: '_sppp: Prediction change per 1% parameter value change';
      Classification: PredictionScaledSensitivities;
      FilterDescription: 'Prediction change per 1% parameter value change'),
//      FilterDescription: 'Prediction Scaled Sensitivities-Parameter Value-Predicted Value'),

     (Extension: '._spsp';
//      MenuCaption: '_spsp: Prediction Scaled Sensitivities-Std Dev Parameter Value-Predicted Value';
      MenuCaption: '_spsp: Prediction change per a parameter change equal to 1% of the parameter standard deviation';
      Classification: PredictionScaledSensitivities;
      FilterDescription: 'Prediction change per a parameter change equal to 1% of the parameter standard deviation'),
//      FilterDescription: 'Prediction Scaled Sensitivities-Std Dev Parameter Value-Predicted Value'),

     (Extension: '._sppr';
//      MenuCaption: '_sppr: Prediction Scaled Sensitivities-Parameter Value-Reference Value';
      MenuCaption: '_sppr: Prediction change as % of reference value per 1% parameter value change';
      Classification: PredictionScaledSensitivities;
      FilterDescription: 'Prediction change as % of reference value per 1% parameter value change'),
//      FilterDescription: 'Prediction Scaled Sensitivities-Parameter Value-Reference Value'),

     (Extension: '._spsr';
//      MenuCaption: '_spsr: Prediction Scaled Sensitivities-Std Dev Parameter Value-Reference Value';
      MenuCaption: '_spsr: Prediction change as % of reference value per a parameter change equal to 1% of the parameter standard deviation';
      Classification: PredictionScaledSensitivities;
      FilterDescription: 'Prediction change as % of reference value per a parameter change equal to 1% of the parameter standard deviation'),
//      FilterDescription: 'Prediction Scaled Sensitivities-Std Dev Parameter Value-Reference Value'),

     (Extension: '._linp';
      MenuCaption: '_linp: Linear confidence intervals';
      Classification: UncertaintyPred;
      FilterDescription: 'Linear confidence intervals'),

     (Extension: '._intconf';
      MenuCaption: '_intconf: Nonlinear confidence intervals';
      Classification: UncertaintyPar;
      FilterDescription: 'Nonlinear confidence intervals'),

     (Extension: '._intconf';
      MenuCaption: '_intconf: Nonlinear confidence intervals';
      Classification: UncertaintyPred;
      FilterDescription: 'Nonlinear confidence intervals'),

     (Extension: '._intpred';
      MenuCaption: '_intpred: Nonlinear prediction intervals';
      Classification: UncertaintyPred;
      FilterDescription: 'Nonlinear prediction intervals'),

     (Extension: '._mcmc_grr';
      MenuCaption: '_mcmc_grr: Gelman-Rubin R statistic';
      Classification: McmcClassification;
      FilterDescription: 'MCMC convergence statistic of Gelman-Rubin R'),

     (Extension: '._mcmc_pred*1';
      MenuCaption: '_mcmc_pred: prediction samples';
      Classification: McmcClassification;
      FilterDescription: 'MCMC prediction samples'),

     (Extension: '._mcmc_par*1';
      MenuCaption: '_mcmc: parameter samples';
      Classification: McmcClassification;
      FilterDescription: 'MCMC parameter and prediction samples')
     );

function FortranStrToFloat(AString : string) : double;
function TryFortranStrToFloat(AString : string; var resultValue: double) : Boolean;

implementation


uses StrUtils, frmAboutUnit, frmFormatUnit, TeeShape, frmLakePlotUnit,
  ExtractUnit, PiperGraphUnit,
  frmFlowReader, frmConvertFlowsUnit, TeCanvas, frmFarmUnit;

{$R *.DFM}

type
  TGroupParam = class(TObject)
  public
    ParameterName: string;
    CompositeScaledSensitivity: double;
  end;

  TGroup = class(TObject)
  private
    FParameters: TList;
    function GetParam(Index: Integer): TGroupParam;
    procedure SetParam(Index: Integer; const Value: TGroupParam);
    function GetCount: Integer;
  public
    GroupName: string;
    NumberInGroup: integer;
    PlotSymbol: integer;
    property Parameters[Index: Integer]: TGroupParam read GetParam
      write SetParam; default;
    constructor Create;
    destructor Destroy; override;
    procedure AddParam(Value: TGroupParam);
    property ParamCount: Integer read GetCount;
    procedure SortParameters(ValueList: TList);
  end;

  TGroupIndex = class(TObject)
    PlotSymbol: integer;
    GroupName: string;
  end;

function GetDLLName: string;
var
  FileCheck: array[0..255] of char;
begin
  GetModuleFileName(HInstance, Filecheck, 255);
  result := string(Filecheck)
end;

function GetDllFullPath(FileName: string; var FullPath: string): boolean;
var
  AHandle: HWND;
  ModuleFileName: array[0..255] of char;
begin
  FullPath := '';
  AHandle := GetModuleHandle(PChar(FileName));
  if AHandle = 0 then
  begin
    Result := False;
  end
  else
  begin
    if (GetModuleFileName(AHandle, @ModuleFileName[0],
      SizeOf(ModuleFileName)) > 0) then
    begin
      FullPath := ModuleFileName;
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end;
end;

function ChartHelpFileName: string;
var
  FileName: string;
begin
  FileName := GetDLLName;
  GetDllFullPath(FileName, result);
  result := ExtractFileDir(result) + '\TeeUser6.chm';
end;

function HelpFileName: string;
var
  FileName: string;
begin
  FileName := GetDLLName;
  GetDllFullPath(FileName, result);
  result := ExtractFileDir(result) + '\Gw_chart.chm';
end;


type
  TSensRecord = record
    Name : string[10];
    ISENS : integer;
    LN : integer;
    B : double;
    BL : double;
    BU : double;
    BSCAL : double;
  end;


Type
  TBarValue = class(TObject)
    Value : double;
    ParameterName : string[12];
    Color : TColor;
  end;

  TSdSortLineSeries = class(TObject)
    ParameterName : string[11];
    Series : TLineSeries;
  end;

  TSdValues = class(TObject)
    ObservationName : string;
    PlotSymbol : integer;
    Values : array of double;
  private
    function AbsMax: double;
  end;

function FortranStrToFloat(AString : string) : double;
var
  DPos : integer;
  Sub : string;
begin
  AString := Trim(AString);
  if UpperCase(Copy(AString,1,4)) = '-INF' then
  begin
    result := -MaxDouble;
    Exit;
  end;

  if UpperCase(Copy(AString,1,3)) = 'INF' then
  begin
    result := MaxDouble;
    Exit;
  end;

  DPos := Pos('d', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'e';
  end;
  DPos := Pos('D', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'E';
  end;
  if DecimalSeparator <> '.' then
  begin
    DPos := Pos('.', AString);
    if DPos > 0 then
    begin
      AString[DPos] := DecimalSeparator;
    end;
  end;
  Sub := Copy(AString, 2, Length(AString));
  DPos := Pos('+', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  DPos := Pos('-', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  result := StrToFloat(AString);
end;

function TryFortranStrToFloat(AString : string; var resultValue: double) : Boolean;
var
  DPos : integer;
  Sub : string;
begin
  result := True;
  AString := Trim(AString);
  if UpperCase(Copy(AString,1,4)) = '-INF' then
  begin
    resultValue := -MaxDouble;
    Exit;
  end;

  if UpperCase(Copy(AString,1,3)) = 'INF' then
  begin
    resultValue := MaxDouble;
    Exit;
  end;

  DPos := Pos('d', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'e';
  end;
  DPos := Pos('D', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'E';
  end;
  if DecimalSeparator <> '.' then
  begin
    DPos := Pos('.', AString);
    if DPos > 0 then
    begin
      AString[DPos] := DecimalSeparator;
    end;
  end;
  Sub := Copy(AString, 2, Length(AString));
  DPos := Pos('+', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  DPos := Pos('-', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  result := TryStrToFloat(AString, resultValue);
end;



var
  scValueObjectList : TObjectList = nil;

function IndexOfScParameterName(scObjectList : TObjectList;
  ParameterName : string) : integer;
var
  Index : integer;
  Value : TBarValue;
begin
  result := -1;
  for Index := 0 to scObjectList.Count -1 do
  begin
    Value := scObjectList[Index] as TBarValue;
    if Value.ParameterName =  ParameterName then
    begin
      result := Index;
      Exit;
    end;
  end;
  if result < 0 then
  begin
    Value := TBarValue.Create;
    Value.Value := 0;
    Value.ParameterName := ParameterName;
    Value.Color := clTeeColor;
    result := scObjectList.Add(Value);
  end;

end;

function SortSdValues(Item1, Item2: Pointer): Integer;
var
  Value1, Value2 : TSdValues;
  MaxValue1, MaxValue2 : double;
begin
  Value1 := Item1;
  Value2 := Item2;
  MaxValue1 := Value1.AbsMax;
  MaxValue2 := Value2.AbsMax;
  if MaxValue1 > MaxValue2 then
  begin
    result := -1;
  end
  else if MaxValue2 > MaxValue1 then
  begin
    result := 1;
  end
  else
  begin
    result := 0;
  end;
end;

function SortSeriesList(Item1, Item2: Pointer): Integer;
var
  Value1, Value2 : TSdSortLineSeries;
  Index1, Index2 : integer;
begin
  Value1 := Item1;
  Value2 := Item2;
  assert(scValueObjectList <> nil);
  Index1 := IndexOfScParameterName(scValueObjectList,Value1.ParameterName);
  Index2 := IndexOfScParameterName(scValueObjectList,Value2.ParameterName);
  if Index1 < 0 then
  begin
    ShowMessage(Value1.ParameterName);
  end;
  if Index2 < 0 then
  begin
    ShowMessage(Value2.ParameterName);
  end;
  Assert((Index1 > -1) and (Index2 > -1));
  result := Index1 - Index2;
end;


function SortBarValues(Item1, Item2: Pointer): Integer;
var
  Value1, Value2 : TBarValue;
begin
  Value1 := Item1;
  Value2 := Item2;
  if Abs(Value1.Value) > Abs(Value2.Value) then
  begin
    result := -1;
  end
  else if Abs(Value1.Value) < Abs(Value2.Value) then
  begin
    result := 1;
  end
  else
  begin
    result := 0;
  end;
end;

type
  TFileMenuItem = class(TMenuItem)
  public
    FileName: string;
  end;


procedure TfrmModChart.FillFileList(FileRoot : String);
  function GetCaption(const Extension: string): string;
  var
    Index: integer;
    NumString: string;
    PrefixLength: integer;
    AnInt: Integer;
    I: integer;
  begin
    result := '';

    for Index := 0 to MaxExtensions do
    begin
      I := index;
      if Pos('*', Classifiations[I].Extension) > 0 then
      begin
        PrefixLength := Length(Classifiations[Index].Extension) -2;
        NumString := Copy(Extension, PrefixLength+1, MAXINT);
        if TryStrToInt(NumString, AnInt) then
        begin
          result := Classifiations[Index].MenuCaption;
          break
        end;
      end
      else
      begin
        if Classifiations[Index].Extension = Extension then
        begin
          result := Classifiations[Index].MenuCaption;
          break
        end;
      end;

    end;
    Assert(result <> '');
  end;
var
  ExtensionIndex : integer;
  FileName : String;
  AMenuItem : TFileMenuItem;
//  RequiredWidth, TempWidth: integer;
  Groups: TStringList;
  AGroup: TStringList;
  Position: integer;
  GroupIndex, Index: integer;
  ParentNode: TTreeNode;
//  GroupName: string;
  ParentMenuItem : TMenuItem;
  SearchRec: TSearchRec;
  AFileName: string;
  PrefixLength: integer;
  FileDir: string;
  FileNumber: string;
  AnInt: integer;
begin
  Groups := TStringList.Create;
  try
    Groups.Add(ModelFit);
    Groups.Add(OPSensAnalysis);
    Groups.Add(ParameterValues);
//    Groups.Add(SensAnalysisForPredictions);
    Groups.Add(ParameterImportanceToPredictions);
    Groups.Add(ObservationImportanceToPredictions);
    Groups.Add(PredictionScaledSensitivities);
    Groups.Add(UncertaintyPar);
    Groups.Add(UncertaintyPred);
    Groups.Add(McmcClassification);


    treecomboFileNames.Items.Clear;

    MenuItems.Clear;
    for ExtensionIndex := 0 to MaxExtensions do
    begin
      FileName := FileRoot + Classifiations[ExtensionIndex].Extension;
      if FileExists(FileName) then
      begin
        if Pos('*', FileName) > 0 then
        begin
          PrefixLength := Length(ExtractFileName(FileName))-2;
          FileDir := ExtractFileDir(FileName);
          FileDir := IncludeTrailingPathDelimiter(FileDir);
          try
            if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
            begin
              AFileName := SearchRec.Name;
              FileNumber := Copy(AFileName, PrefixLength+1, MaxInt);
              if TryStrToInt(FileNumber, AnInt) then
              begin
                FileName := FileDir + AFileName;
              end
              else
              begin
                while FindNext(SearchRec) = 0 do
                begin
                  AFileName := SearchRec.Name;
                  FileNumber := Copy(AFileName, PrefixLength+1, MaxInt);
                  if TryStrToInt(FileNumber, AnInt) then
                  begin
                    FileName := FileDir + AFileName;
                    break;
                  end;
                end;
              end;
            end;
          finally
            FindClose(SearchRec);
          end
        end;

        Position := Groups.IndexOf(Classifiations[ExtensionIndex].Classification);
        Assert(Position >= 0);
        AGroup := Groups.Objects[Position] as TStringList;
        if Pos('*', FileName) <= 0  then
        begin
          if AGroup = nil then
          begin
            AGroup:= TStringList.Create;
            Groups.Objects[Position] := AGroup;
          end;
          AGroup.Add(FileName);
        end;
      end;
    end;

    ParentNode := nil;
    for GroupIndex := 0 to Groups.Count -1 do
    begin
      AGroup := Groups.Objects[GroupIndex] as TStringList;
      if AGroup <> nil then
      begin
        ParentNode := treecomboFileNames.Items.Add(ParentNode, Groups[GroupIndex]);

        ParentMenuItem := TMenuItem.Create(self);
        ParentMenuItem.Caption := Groups[GroupIndex];
        SelectFileType1.Add(ParentMenuItem);
        MenuItems.Add(ParentMenuItem);

        for Index := 0 to AGroup.Count -1 do
        begin
          FileName := AGroup[Index];
          treecomboFileNames.Items.AddChild(ParentNode, ExtractFileName(FileName));

          AMenuItem := TFileMenuItem.Create(self);
          AMenuItem.Caption := GetCaption(ExtractFileExt(FileName));
          AMenuItem.FileName := FileName;
//          AMenuItem.Tag := Integer(ANode);
          AMenuItem.OnClick := ChooseGraphType;
          ParentMenuItem.Add(AMenuItem);
          MenuItems.Add(AMenuItem);
        end;
      end;
    end;


    SelectFileType1.Enabled := SelectFileType1.Count > 0;

{    RequiredWidth := 0;
    for GroupIndex := 0 to Groups.Count -1 do
    begin
      if Groups.Objects[GroupIndex] <> nil then
      begin
        GroupName := Groups[GroupIndex];
        TempWidth := Canvas.TextWidth(GroupName) + 25;
        if TempWidth > RequiredWidth then
        begin
          RequiredWidth := TempWidth;
        end;
        AGroup := Groups.Objects[GroupIndex] as TStringList;
        for Index := 0 to AGroup.Count -1 do
        begin
          FileName := AGroup[Index];
          TempWidth := Canvas.TextWidth(FileName) + 50;
          if TempWidth > RequiredWidth then
          begin
            RequiredWidth := TempWidth;
          end;
        end;
      end;
    end;

    if OriginalTreeComboWidth < RequiredWidth + 25 then
    begin
      treecomboFileNames.Width := RequiredWidth + 25;
    end
    else
    begin
      treecomboFileNames.Width := OriginalTreeComboWidth;
    end;                      }
    SetTreeComboDropDownWidth;
  finally
    for Index := 0 to Groups.Count -1 do
    begin
      Groups.Objects[Index].Free;
    end;
    Groups.Free;
    treecomboFileNames.Invalidate;
  end;
end;

{procedure TfrmModChart.SetTreeComboSelectWidth;
var
  RequiredWidth: integer;
begin
  if (treecomboFileNames.TreeView.Selected <> nil) then
  begin
    RequiredWidth := Canvas.TextWidth(treecomboFileNames.
      TreeView.Selected.Text) + 25
      + treecomboFileNames.TreeView.Selected.Level
      * treecomboFileNames.TreeView.Indent;
    if OriginalTreeComboWidth < RequiredWidth + 25 then
    begin
      treecomboFileNames.Width := RequiredWidth + 25;
    end
    else
    begin
      treecomboFileNames.Width := OriginalTreeComboWidth;
    end;
  end;

end; }

procedure TfrmModChart.SetTreeComboDropDownWidth;
var
  Index: integer;
  Item: TTreeNode;
  RequiredWidth: integer;
  TempWidth: integer;
begin
  RequiredWidth := 0;
  for Index := 0 to treecomboFileNames.Items.Count -1 do
  begin
    Item := treecomboFileNames.Items[Index];
    TempWidth := Canvas.TextWidth(Item.Text) + 16
      + (Item.Level + 1) * treecomboFileNames.TreeView.Indent;
    if TempWidth > RequiredWidth then
    begin
      RequiredWidth := TempWidth;
    end;
  end;

  if OriginalTreeComboWidth < RequiredWidth then
  begin
    treecomboFileNames.Width := RequiredWidth;
  end
  else
  begin
    treecomboFileNames.Width := OriginalTreeComboWidth;
  end;
end;

procedure TfrmModChart.SetModelSelection(const FileName: string);
var
  AFile : TextFile;
  ALine: string;
begin
  AssignFile(AFile, FileName);
  reset(AFile);
  try
    Readln(AFile, ALine);
    case FileType of
      ftUnknown:
        begin
           Assert(False)
        end;
      ft_rb, ft_pc, ft_ss, ft_nm, ft_s1, ft_sd, ft_sc, ft_ws, ft_ww, ft_os:
        begin
          if Pos('"', ALine) > 0 then
          begin
            ModelSelection := msUcode2005;
            if FileType = ft_os then
            begin
              if Pos('PLOT SYMBOL', ALine) <= 0 then
              begin
                ModelSelection := msModflow2005;
              end;
            end;
          end
          else
          begin
            ModelSelection := msModflow;
          end;
        end;
      ft_rd:
        begin
          if Pos('RANDOM NUMBER', ALine) > 0 then
          begin
            ModelSelection := msUcode2005;
          end
          else
          begin
            ModelSelection := msModflow;
          end;
        end;
      ft_rg:
        begin
          if Pos('ORDERED CORRELATED NUMBER', ALine) > 0 then
          begin
            ModelSelection := msUcode2005;
          end
          else
          begin
            ModelSelection := msModflow;
          end;
        end;
      ft_rc:
        begin
          if Pos('PRIOR NAME', ALine) > 0 then
          begin
            ModelSelection := msUcode2005;
          end
          else
          begin
            ModelSelection := msModflow;
          end;
        end;
      ft_b:
        begin
          ModelSelection := msModflow;
        end;
      ft_pa:
        begin
          // different
          Readln(AFile, ALine);
          if Pos('"', ALine) > 0 then
          begin
            ModelSelection := msUcode2005;
          end
          else
          begin
            ModelSelection := msModflow;
          end;
        end;
      ft_rdadv, ft_so:
        begin
          ModelSelection := msUcode2005;
        end;
      ft_ppr, ft_opr, ft_ppr_abschg, ft_ppa, ft_ppa_abschg,
        ft_opr_abschg, ft_opa, ft_opa_abschg,
        ft_sppp, ft_spsp, ft_sppr, ft_spsr, ft_pcc, ft_linp,
        ft_intconf, ft_intpred, ft_xyztwr, ft_sc_svd, ft_scgrp, ft_mcmc_grr,
        ft_mcmc_pred, ft_mcmc_par, ft_svd:
        begin
          ModelSelection := msUcode2005;
        end;
    else Assert(False);
    end;
  finally
    CloseFile(AFile);
  end;
end;

Function TfrmModChart.FileNameToFileType(const FileName: string): TFileType;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(FileName));

  if Extension = '._os' then
  begin
    result := ft_os;
  end
  else if Extension = '._ww' then
  begin
    result := ft_ww;
  end
  else if Extension = '._ws' then
  begin
    result := ft_ws;
  end
  else if Extension = '._sc' then
  begin
    result := ft_sc;
  end
  else if Extension = '._sd' then
  begin
    result := ft_sd;
  end
  else if Extension = '._s1' then
  begin
    result := ft_s1;
  end
  else if Extension = '._nm' then
  begin
    result := ft_nm;
  end
  else if Extension = '._rd' then
  begin
    result := ft_rd;
  end
  else if Extension = '._rg' then
  begin
    result := ft_rg;
  end
  else if Extension = '._rb' then
  begin
    result := ft_rb;
  end
  else if Extension = '._rc' then
  begin
    result := ft_rc;
  end
  else if Extension = '._b' then
  begin
    result := ft_b;
  end
  else if Extension = '._pa' then
  begin
    result := ft_pa;
  end
  else if Extension = '._pc' then
  begin
    result := ft_pc;
  end
  else if Extension = '._ss' then
  begin
    result := ft_ss;
  end
  else if Extension = '._rdadv' then
  begin
    result := ft_rdadv;
  end
  else if Extension = '._so' then
  begin
    result := ft_so;
  end
  else if Extension = '._ppr' then
  begin
    result := ft_ppr;
  end
  else if Extension = '._opr' then
  begin
    result := ft_opr;
  end
  else if Extension = '._ppr_abschg' then
  begin
    result := ft_ppr_abschg;
  end
  else if Extension = '._ppa' then
  begin
    result := ft_ppa;
  end
  else if Extension = '._ppa_abschg' then
  begin
    result := ft_ppa_abschg;
  end
  else if Extension = '._opr_abschg' then
  begin
    result := ft_opr_abschg;
  end
  else if Extension = '._opa' then
  begin
    result := ft_opa;
  end
  else if Extension = '._opa_abschg' then
  begin
    result := ft_opa_abschg;
  end
  else if Extension = '._sppp' then
  begin
    result := ft_sppp;
  end
  else if Extension = '._spsp' then
  begin
    result := ft_spsp;
  end
  else if Extension = '._sppr' then
  begin
    result := ft_sppr;
  end
  else if Extension = '._spsr' then
  begin
    result := ft_spsr;
  end
  else if Extension = '._pcc' then
  begin
    result := ft_pcc;
  end
  else if Extension = '._linp' then
  begin
    result := ft_linp;
  end
  else if Extension = '._intconf' then
  begin
    result := ft_intconf;
  end
  else if Extension = '._intpred' then
  begin
    result := ft_intpred;
  end
  else if Extension = '._xyztwr' then
  begin
    result := ft_xyztwr;
  end
  else if Extension = '._sc_svd' then
  begin
    result := ft_sc_svd;
  end
  else if Extension = '._scgrp' then
  begin
    result := ft_scgrp;
  end
  else if Extension = '._mcmc_grr' then
  begin
    result := ft_mcmc_grr;
  end
  else if Extension = '._svd' then
  begin
    result := ft_svd;
  end
  else if Pos('._mcmc_pred', LowerCase(Extension)) = 1 then
  begin
    Result := ft_mcmc_pred;
  end
  else if Pos('._mcmc_par', LowerCase(Extension)) = 1 then
  begin
    Result := ft_mcmc_par;
  end
  else
  begin
    result := ftUnknown;
  end;
end;

procedure TfrmModChart.OpenFile(const FileName : string);
var
  ASeries : TObject;
  Index : integer;
  Extension : string;
  sdFileName : string;
  ValueList : TObjectList;
  AMessage : string;
  Nm_FileName: string;
  ShortFileName: string;
  Node: TTreeNode;
  TempFile : TStringList;
  WithPrior: boolean;
  FileOpened: boolean;
begin
  if OpeningFile2 then
  begin
    Exit;
  end;

  WithPrior := False;
  OpeningFile2 := True;
  try
    chartModflow.Legend.TextStyle := ltsLeftValue;
    chartModflow.Legend.Inverted := False;
    chartModflow.Legend.Symbol.Squared := False;
    chartModflow.Legend.Series := nil;
    chartModflow.Legend.LegendStyle := lsAuto;
    NameOfFile := FileName;
    sbRefresh.Enabled := True;
    ExtraSeriesList.Clear;
    RdRgStyleList.Clear;
    rdRgNameList.Clear;
    SerDataPoints.Active := False;
    ser1to1.Active := False;
    serBarChart.Active := False;
    serBarChart.MultiBar := mbSide;
    serBarChart.ShowInLegend := False;
    serBarChart2.MultiBar := mbSide;
    LineAt1.Active := False;
    LineAt1.Clear;
    RangeSeries.Active := False;
    HorizRangeSeries.Active := False;
    jvplChartControls.Visible := False;
//    serBubble.Active := False;
//    serBubble.Clear;
//    serBubbleLegend.Active := False;
    serBubbleLegend.Clear;
    serBarChart2.Active := False;
    serBarChart2.ShowInLegend := False;
    Try
      StatusBar1.Panels[1].Text := '';
      ValueList := nil;
      chartModflow.OnGetLegendPos := chartModflowGetLegendPos;
      chartModflow.LeftAxis.StartPosition := 0;
      chartModflow.LeftAxis.EndPosition := 100;
      chartModflow.RightAxis.StartPosition := 0;
      chartModflow.RightAxis.EndPosition := 100;
      if MakeAxesAutomatic then
      begin
        chartModflow.BottomAxis.Automatic := True;
        chartModflow.LeftAxis.Automatic := True;
      end;
      chartModflow.BottomAxis.Logarithmic := False;
      chartModflow.LeftAxis.Logarithmic := False;
      chartModflow.Legend.Visible := True;
      chartModflow.Legend.Alignment := laRight;
      chartModflow.BottomAxis.LabelsAngle := 0;
      SerDataPoints.Pointer.HorizSize := 4;
      SerDataPoints.Pointer.VertSize := 4;

      chartModflow.LeftAxis.Increment := 0;
      chartModflow.LeftAxis.AutomaticMinimum := True;
      chartModflow.LeftAxis.AutomaticMaximum := True;
      chartModflow.RightAxis.AxisValuesFormat := '#,##0.###';
      chartModflow.LeftAxis.LabelsSize := 0;

      Extension := LowerCase(ExtractFileExt(FileName));
      Filetype := FileNameToFileType(FileName);

      SetModelSelection(FileName);

      if Filetype in [ft_sd, ft_s1, ft_rb,
        ft_sppp, ft_spsp, ft_sppr, ft_spsr] then
      begin
        sdFileName := FileName;
        SetLength(sdFileName, Length(sdFileName) - Length(Extension));
        sdFileName := sdFileName + '._sc';
        if FileExists(sdFileName) then
        begin
          jvplChartControls.Visible := True;
          jvplChartControls.ActivePage := jvspDataOrder;
          if (rgWhatToPlot.ItemIndex = 0)
            and (rgOrder.ItemIndex = 1) then
          begin
            AssignFile(FFile, sdFileName);
            try
              reset(FFile);
              try
                ValueList := TObjectList.Create;
                Read_scData(ValueList, WithPrior);
              finally
                CloseFile(FFile);
              end;
            except on E: EInOutError do
              begin
                Beep;
                MessageDlg(Format('Error reading the file "%0:s". Error message is "%1:s"',
                  [sdFileName, E.message]), mtError, [mbOK], 0);
                Exit;
              end;
            end
          end;
        end;
      end
      else if FileType = ft_scgrp then
      begin
        sdFileName := ChangeFileExt(FileName, '._sc');
        AssignFile(FFile, sdFileName);
        try
          reset(FFile);
          try
            ValueList := TObjectList.Create;
            Read_scData(ValueList, WithPrior);
          finally
            CloseFile(FFile);
          end;
        except on E: EInOutError do
          begin
            Beep;
            MessageDlg(Format('Error reading the file "%0:s". Error message is "%1:s"',
              [sdFileName, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end
      end
      else if FileType = ft_pc then
      begin
        TempFile := TStringList.Create;
        try
          TempFile.LoadFromFile(FileName);
          FParamCount := TempFile.Count -1;
        finally
          TempFile.Free;
        end;

      end;


      FileOpened := False;
      if FileType <> ft_mcmc_par then
      begin
        AssignFile(FFile, FileName);
        reset(FFile);
        FileOpened := True;
      end;
      try
        ClearObservationNames;
        StatusBar1.Panels[0].Text := '';
        ExplantionVisible := False;
        chartModflow.Title.Text.Clear;
        chartModflow.Title.Text.Add(ExtractFileName(FileName));
        if MakeAxesAutomatic then
        begin
          chartModflow.BottomAxis.Automatic := True;
          chartModflow.LeftAxis.Automatic := True;
        end;
        chartModflow.LeftAxis.AutomaticMinimum := True;
        chartModflow.LeftAxis.AutomaticMaximum := True;
        chartModflow.BottomAxis.LabelStyle := talAuto;
        chartModflow.BottomAxis.LabelsAngle := 0;
        cbLabelPoints.Enabled := False;
        SerDataPoints.Clear;
        ser1to1.Clear;
        serBarChart.Clear;
        serBarChart2.Clear;
        RangeSeries.Clear;
        HorizRangeSeries.Clear;
        SerDataPoints.Pointer.Brush.Color := clScrollBar;
        ser1to1.Pointer.Brush.Color := clScrollBar;
        SetAxisLabels(Extension);
        chartModflow.ShowHint := False;
        for Index := 0 to SeriesList.Count -1 do
        begin
          ASeries := SeriesList.Objects[Index];
          ASeries.Free;
        end;
        SeriesList.Clear;
        MarkedPoints.Clear;
        FMarkedPointsList.Clear;

        case Filetype of
          ft_os, ft_ww, ft_ws:
            begin
              Read_os(Filetype, Extension);
            end;
          ft_nm:
            begin
              Read_nm(False);
            end;
          ft_sc:
            begin
              Read_sc;
            end;
          ft_sd, ft_s1, ft_rb, ft_sppp, ft_spsp, ft_sppr, ft_spsr:
            begin
              Read_sd(ValueList);
            end;
          ft_rd, ft_rg:
            begin
              Read_rd_rg;
            end;
          ft_rc:
            begin
              Read_rc;
            end;
          ft_b:
            begin
              Read_b;
            end;
          ft_pa:
            begin
              Read_pa;
            end;
          ft_pc:
            begin
              Read_pc;
            end;
          ft_ss:
            begin
              Read_ss;
            end;
          ft_rdadv:
            begin
              Read_rdadv;
            end;
          ft_so:
            begin
              Read_so;
            end;
          ft_opr, ft_ppr, ft_ppr_abschg, ft_ppa, ft_opr_abschg, ft_opa:
            begin
              Read_opr_ppr;
            end;
          ft_ppa_abschg, ft_opa_abschg:
            begin
              Read_ppa_abschg;
            end;
          ft_pcc:
            begin
              Read_pcc;
            end;
          ft_linp:
            begin
              Read_linp;
            end;
          ft_intconf, ft_intpred:
            begin
              Read_intconf_intpred;
            end;
          ft_xyztwr:
            begin
              Read_xyztwr;
            end;
          ft_sc_svd:
            begin
              Read_sc_svd;
            end;
          ft_scgrp:
            begin
              Read_scgrp(ValueList);
            end;
          ft_mcmc_grr:
            begin
              Read_mcmc_grr;
            end;
          ft_mcmc_pred:
            begin
              Read_mcmc_pred(FileName);
            end;
          ft_mcmc_par:
            begin
              Read_mcmc_par(FileName);
            end;
          ft_svd:
            begin
              Read_svd;
            end;
        else
          begin
            Assert(False);
          end;;
        end;
      finally
        if FileOpened then
        begin
          CloseFile(FFile);
        end;          
        ValueList.Free;
      end;
    Except on E : EInOutError do
      begin
        case E.ErrorCode of
          2: AMessage := 'File not found';
          3: AMessage := 'Invalid file name';
          4: AMessage := 'Too many open files';
          5: AMessage := 'Access denied';
          100: AMessage := 'End of file';
          101: AMessage := 'Disk Full';
          102: AMessage := 'File not assigned';
          103: AMessage := 'File not Open';
          104: AMessage := 'File not open for input';
          105: AMessage := 'File not open for output';
          106: AMessage := 'Invalid input';
        else raise;
        end;
        MessageDlg(AMessage, mtError, [mbOK], 0);
      end;
    end;
    if (Filetype in [ft_rd, ft_rg]) and cbPlot_NmFile.Checked
      and cbPlot_NmFile.Enabled then
    begin
      Nm_FileName := GetCurrentDir + '\' + treecomboFileNames.Selected.Text;
      Nm_FileName := ChangeFileExt(Nm_FileName, '._nm');
      Assert(FileExists(Nm_FileName));
      AssignFile(FFile, Nm_FileName);
      reset(FFile);
      try
        Read_nm(True);
      finally
        CloseFile(FFile);
      end;
      SerDataPoints.Pointer.HorizSize := 6;
      SerDataPoints.Pointer.VertSize := 6;
    end;
    ShortFileName := ExtractFileName(FileName);
    if (treecomboFileNames.Selected = nil)
      or (treecomboFileNames.Selected.Text <> ShortFileName) then
    begin
      for Index := 0 to treecomboFileNames.TreeView.Items.Count -1 do
      begin
        Node := treecomboFileNames.TreeView.Items[Index];
        if Node.Text = ShortFileName then
        begin
          Node.Selected := True;
          treecomboFileNames.Invalidate;
          break;
        end;
      end;
    end;
  finally
      OpeningFile2 := False;
  end;
  MainMenu1.Merge(mainMenuFormChoice);
end;

procedure TfrmModChart.Read_pc;
const
  MaxModflowParameterNameLength = 16;
  MaxUCodeParameterNameLength = 20;
  // For UCODE-2005, the actual length is 12
  // but there is a blank character at the beginning.
  MaxUCode2005ParameterNameLength = 13;
var
  ParamName: array[1..MaxUCodeParameterNameLength] of Char;
  LN, InReasonableRange, InConfidenceInterval: integer;
  Mean: double;
  BL: double; // lower range
  Bu: double; // upper range
  UpperConfInterval: double;
  LowerConfInterval: double;
  I: integer;
  ALine: string;
  MaxNameLength: integer;
  DivideByMean: boolean;
  OptimalValue: double;
  StandardDeviationRegression: double;
  CoefficientOfVariationRegression: double;
  StandardDeviationNative: double;
  CoefficientOfVariationNative: double;
  Temp: double;
  WhatToPlot: TPcPlotChoice;
  dmFileName: string;
  StandardErrorLine: string;
  StandardError: double;
  procedure LimitValue(var AValue: double);
  const
    Limit = 1.7e307/4;
  begin
    if (AValue > Limit) or (AValue < -Limit) then
    begin
      AValue := AValue/4;
    end;
  end;
begin
  chartModflow.BottomAxis.LabelsAngle := 90;
  case ModelSelection of
    msModflow:
      begin
        if not rbMeanCiRangeForPC.Checked and not rbMeanCiForPC.Checked then
        begin
          rbMeanCiRangeForPC.Checked := True;
        end;
        rbCoefVarReg.Enabled := False;
        rbCoerVarPar.Enabled := False;
        rbInverseCoef.Enabled := False;
      end;
    msUcode2005:
      begin
        rbCoefVarReg.Enabled := True;
        rbCoerVarPar.Enabled := True;
        rbInverseCoef.Enabled := True;
      end;
  else Assert(False);
  end;
  dmFileName := ChangeFileExt(treecomboFileNames.Selected.Text, '._dm');
  rb_b_sd_s.Enabled := rbInverseCoef.Enabled and FileExists(dmFileName);
  if rbMeanCiRangeForPC.Checked then
  begin
    WhatToPlot := pcMeanCiRange;
  end
  else if rbMeanCiForPC.Checked then
  begin
    WhatToPlot := pcMeanCi;
  end
  else if rbCoefVarReg.Checked then
  begin
    WhatToPlot := pcCoefVarModel;
  end
  else if rbCoerVarPar.Checked then
  begin
    WhatToPlot := pcCoefVarPar;
  end
  else if rbInverseCoef.Checked then
  begin
    WhatToPlot := pcInverse;
  end
  else if rb_b_sd_s.Checked then
  begin
    WhatToPlot := pcInverseTimesStandError;
    ReadStandardError2(dmFileName, False, StandardErrorLine);
    StandardError := FortranStrToFloat(StandardErrorLine);
  end
  else
  begin
    Assert(False);
  end;

  case WhatToPlot of
    pcMeanCiRange, pcMeanCi:
      begin
        RangeSeries.Active := True;
        RangeSeries.ShowRange := (WhatToPlot = pcMeanCiRange);
        RangeSeries.ShowConfidenceIntervals := True;
        RangeSeries.Title := 'Parameter Range';
        cbScaleValues.Enabled := True;
        lblPcExpl1.Visible := True;
        lblPcExpl2.Visible := True;
      end;
    pcCoefVarModel, pcCoefVarPar:
      begin
        SerDataPoints.Active := True;
        SerDataPoints.Title := 'Coefficients of variation';
        cbScaleValues.Enabled := False;
        cbScaleValues.Checked := False;
        SetAxisLabels('._pc');
        lblPcExpl1.Visible := False;
        lblPcExpl2.Visible := False;
      end;
    pcInverse:
      begin
        SerDataPoints.Active := True;
        SerDataPoints.Title := 'Parameter value divided by standard deviation (b/SD)';
        cbScaleValues.Enabled := False;
        cbScaleValues.Checked := False;
        SetAxisLabels('._pc');
        lblPcExpl1.Visible := False;
        lblPcExpl2.Visible := False;
      end;
    pcInverseTimesStandError:
      begin
        SerDataPoints.Active := True;
        SerDataPoints.Title := 'Parameter value divided by standard deviation (b/SD*s)';
        cbScaleValues.Enabled := False;
        cbScaleValues.Checked := False;
        SetAxisLabels('._pc');
        lblPcExpl1.Visible := False;
        lblPcExpl2.Visible := False;
      end;
  else Assert(False);
  end;

  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_pc;
  DivideByMean := cbScaleValues.Checked;
  LineAt1.Active := DivideByMean;
  ExplantionVisible := True;
  chartModflow.Legend.Visible := False;

  if not EOF(FFile) then
  begin
    case ModelSelection of
      msModflow: // MODFLOW
        begin
          // do nothing
        end;
{      1: // UCODE
        begin
          Readln(FFile, ALine);
          Readln(FFile, ALine);
        end;  }
      msUcode2005: // UCODE-2005
        begin
          Readln(FFile, ALine);
        end;
    else
      Assert(False);
    end;

  end;
  MaxNameLength := 0;
  case ModelSelection of
    msModflow:
      begin
        // MODFLOW
        MaxNameLength := MaxModflowParameterNameLength;
      end;
{    1:
      begin
        // UCODE
        MaxNameLength := MaxUCodeParameterNameLength;
      end; }
    msUcode2005:
      begin
        // UCODE-2005
        MaxNameLength := MaxUCode2005ParameterNameLength;
      end
  else Assert(False);
  end;

  While not EOF(FFile) do
  begin
    for I := Succ(MaxNameLength) to Length(ParamName) do
    begin
      ParamName[i] := ' ';
      if I = Length(ParamName) then
      begin
        Assert(I = MaxUCodeParameterNameLength);
      end;

    end;

    for I := 1 to MaxNameLength do
    begin
      Read(FFile, ParamName[i]);
    end;
    Mean := 0;
    BL := 0;
    BU := 0;
    UpperConfInterval := 0;
    LowerConfInterval := 0;

    case ModelSelection of
      msModflow:
        begin
          // MODFLOW
          ReadLn(FFile, LN, Mean, BL, Bu, LowerConfInterval, UpperConfInterval);
        end;
{      1:
        begin
          // UCODE
          ReadLn(FFile, Mean, LowerConfInterval, UpperConfInterval, LN,
            InReasonableRange, InConfidenceInterval, BL, Bu);
        end;   }
      msUcode2005:
        begin
          // UCODE-2005
          ReadLn(FFile, ALine);
          Mean := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          LowerConfInterval := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          UpperConfInterval := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          LN := StrToInt(ExtractSpaceSeparatedValue(ALine));
          OptimalValue := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          StandardDeviationRegression := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          CoefficientOfVariationRegression := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          StandardDeviationNative := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          CoefficientOfVariationNative := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          BL := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          Bu := FortranStrToFloat(ExtractSpaceSeparatedValue(ALine));
          InReasonableRange := StrToInt(ExtractSpaceSeparatedValue(ALine));
          InConfidenceInterval := StrToInt(ExtractSpaceSeparatedValue(ALine));
        end;
    else Assert(False);
    end;
    if DivideByMean and (Mean <> 0) then
    begin
      try
        UpperConfInterval := UpperConfInterval/Mean;
      except on EOverflow do
        begin
          if Sign(UpperConfInterval) = Sign(Mean) then
          begin
            UpperConfInterval := MaxDouble;
          end
          else
          begin
            UpperConfInterval := -MaxDouble;
          end;
        end;
      end;

      try
        LowerConfInterval := LowerConfInterval/Mean;
      except on EOverflow do
        begin
          if Sign(LowerConfInterval) = Sign(Mean) then
          begin
            LowerConfInterval := MaxDouble;
          end
          else
          begin
            LowerConfInterval := -MaxDouble;
          end;
        end;
      end;

      if (FParamCount > 0) and (Bu >= 3.4e38) then
      begin
        Bu := 3.4e38;
      end;
      try
        Bu := Bu/Mean;
      except on EOverflow do
        begin
          Bu := 3.4e38/(FParamCount+1)/10000;
//          Bu := 1.7e306;
        end;
      end;
      if (FParamCount > 0) and (BL <= -3.4e38) then
      begin
        BL := -3.4e38;
      end;
      try
        BL := BL/Mean;
      except on EOverflow do
        begin
          BL := -3.4e38/(FParamCount+1)/10000;
//          BL := -1.7e306;
        end;
      end;
      Mean := 1;
      if UpperConfInterval < LowerConfInterval then
      begin
        Temp := UpperConfInterval;
        UpperConfInterval := LowerConfInterval;
        LowerConfInterval := Temp;
      end;
      if Bu < BL then
      begin
        Temp := Bu;
        Bu := BL;
        BL := Temp;
      end;

    end;

    LimitValue(Mean);
    LimitValue(UpperConfInterval);
    LimitValue(LowerConfInterval);
    LimitValue(Bu);
    LimitValue(BL);

    case WhatToPlot of
      pcMeanCiRange:
        begin
          RangeSeries.AddCI_AndRange(Trim(ParamName), Mean, UpperConfInterval,
            LowerConfInterval, Bu, BL);
        end;
      pcMeanCi:
        begin
          RangeSeries.AddConfidenceInterval(Trim(ParamName), Mean, UpperConfInterval,
            LowerConfInterval);
        end;
      pcCoefVarModel:
        begin
          SerDataPoints.AddY(CoefficientOfVariationRegression, Trim(ParamName))
        end;
      pcCoefVarPar:
        begin
          SerDataPoints.AddY(CoefficientOfVariationNative, Trim(ParamName))
        end;
      pcInverse:
        begin
          if CoefficientOfVariationNative <> 0 then
          begin
            SerDataPoints.AddY(1/CoefficientOfVariationNative, Trim(ParamName))
          end;
        end;
      pcInverseTimesStandError:
        begin
          if CoefficientOfVariationNative <> 0 then
          begin
            SerDataPoints.AddY(1/CoefficientOfVariationNative*StandardError, Trim(ParamName))
          end;
        end;
    else Assert(False);
    end;

    if DivideByMean then
    begin
      LineAt1.AddY(1)
    end;
  end
end;

procedure TfrmModChart.Read_rdadv;
var
  ALine: string;
  ORD_WEIGHT_RES: double;
  ORD_SIM_WT_RES: double;
  STDVS_O_S_W_rsadv: double;
  Two_Times_STDV_O_S_W_R: double;
  FREQUENCY_div_PROB: double;
  PROB_PLOT_POSI: double;
  OBSERVATION_PLOT_SYMBOL: string;
  ObservationName: string;
  PlotSymbol: integer;
  Mean: double;
  UpperConfInterval: double;
  LowerConfInterval: double;
  StyleIndex: integer;
  StyleType : TStyleType;
  ColorIndex: integer;
  Color: TColor;
begin
  chartModflow.BottomAxis.LabelStyle := talValue;
  chartModflow.ShowHint := True;
  cbLabelPointsClick(nil);
  cbLabelPoints.Enabled := True;
  chartModflow.Legend.Visible := False;
  // Skip first line
  HorizRangeSeries.Active := True;
  SerDataPoints.Active := True;
  Readln(FFile, ALine);
  while not EOF(FFile) do
  begin
    Readln(FFile, ORD_WEIGHT_RES, ORD_SIM_WT_RES, STDVS_O_S_W_rsadv,
      Two_Times_STDV_O_S_W_R, FREQUENCY_div_PROB, PROB_PLOT_POSI,
      OBSERVATION_PLOT_SYMBOL);
    ObservationName := ExtractSpaceSeparatedValue(OBSERVATION_PLOT_SYMBOL);
    PlotSymbol := StrToInt(OBSERVATION_PLOT_SYMBOL);

    Mean := ORD_WEIGHT_RES;
    UpperConfInterval := ORD_SIM_WT_RES + Two_Times_STDV_O_S_W_R;
    LowerConfInterval := ORD_SIM_WT_RES - Two_Times_STDV_O_S_W_R;
    {Bu := UpperConfInterval;
    BL := LowerConfInterval;}

    StyleIndex := (PlotSymbol -1) mod	Ord(High(TSeriesPointerStyle));

    StyleType := TStyleType.Create;
    StyleType.Style := Low(TSeriesPointerStyle);
    if StyleIndex > 0 then
    begin
      Inc(StyleType.Style,StyleIndex);
    end;
    while StyleType.Style in [psSmallDot, psNothing] do
    begin
      Inc(StyleType.Style);
    end;

    ObservationNames.AddObject(ObservationName, StyleType);

    ColorIndex := PlotSymbol-1;
    ColorIndex := ColorIndex mod (Length(ColorPalette));
    if ColorIndex < 0 then
    begin
      ColorIndex := ColorIndex + Length(ColorPalette);
    end;

    Color := ColorPalette[ColorIndex];

    HorizRangeSeries.AddConfidenceInterval(PROB_PLOT_POSI, Mean, UpperConfInterval,
      LowerConfInterval, ObservationName, Color);

    SerDataPoints.AddXY(ORD_SIM_WT_RES, PROB_PLOT_POSI, '', clBlack)
  end;
end;

procedure IncrementStyle(var MyStyle : TSeriesPointerStyle);
begin
  if MyStyle = High(TSeriesPointerStyle) then
  begin
    MyStyle := Low(TSeriesPointerStyle)
  end
  else
  begin
    Inc(MyStyle);
  end;
  while MyStyle in [psSmallDot, psNothing] do
  begin
    Inc(MyStyle);
  end;
end;

procedure IncrementLineType(var LineType : TPenStyle);
begin
  if LineType = High(TPenStyle) then
  begin
    LineType := Low(TPenStyle)
  end
  else
  begin
    Inc(LineType);
  end;
  if LineType = psClear then
  begin
    Inc(LineType);
  end;
end;

procedure IncrementColorIndex(var ColorIndex : integer);
begin
  if ColorIndex >= MaxDefaultColors-1 then
  begin
    ColorIndex := 0;
  end
  else
  begin
    Inc(ColorIndex);
  end;
end;

procedure TfrmModChart.Read_b;
var
  Sens : TSensRecord;
  ALine : string;
  Index : integer;
  ASeries : TFirstValueLineSeries;
  LineType : TPenStyle;
  Color : TColor;
  ColorIndex : integer;
  MyStyle : TSeriesPointerStyle;
  function ParseLine : boolean;
  begin
    result := not (Pos('PARAMETER-VALUE SET',ALine) >0 )
      and not (pos('FINAL PARAMETER ESTIMATES', ALine) > 0);
    if not result then Exit;
    try
      Sens.Name := Trim(Copy(ALine,1,10));
      Sens.ISENS := StrToInt(Trim(Copy(ALine,13,3)));
      Sens.LN := StrToInt(Trim(Copy(ALine,18,3)));
      Sens.B := InternationalStrToFloat(Trim(Copy(ALine,23,13)));
      Sens.BL := InternationalStrToFloat(Trim(Copy(ALine,38,13)));
      Sens.BU := InternationalStrToFloat(Trim(Copy(ALine,53,13)));
      Sens.BSCAL := InternationalStrToFloat(Trim(Copy(ALine,68,13)));
    except on EConvertError do
      begin
        result := False;
      end;
    end;
  end;
begin
  inherited;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := False;

  ExplantionVisible := True;
  MyStyle := High(TSeriesPointerStyle);
  LineType := High(TPenStyle);
  ColorIndex := MaxDefaultColors;
  While not EOF(FFile) do
  begin
    ReadLn(FFile, ALine);
    if not EOF(FFile) then
    begin
      if ParseLine then
      begin
        Index := SeriesList.IndexOf(Sens.Name);
        if Index < 0 then
        begin
          ASeries := TFirstValueLineSeries.Create(self);
          ASeries.XValues.Order := loNone;
          ASeries.Active := ShowAllParameters or (Sens.ISENS > 0);
          SeriesList.AddObject(Sens.Name,ASeries);
          ASeries.Title := Sens.Name;
          IncrementStyle(MyStyle);
          IncrementLineType(LineType);
          IncrementColorIndex(ColorIndex);
          Color := ColorPalette[ColorIndex];
          ASeries.pointer.Style := MyStyle;
          ASeries.LinePen.Style := LineType;
          ASeries.SeriesColor := Color;
          ASeries.Pointer.Visible := True;
          chartModflow.AddSeries(ASeries);
          if Sens.B = 0 then
          begin
            ASeries.FirstValue := 1;
          end
          else
          begin
            ASeries.FirstValue := Sens.B;
          end;
        end
        else
        begin
          ASeries := SeriesList.Objects[Index] as TFirstValueLineSeries;
        end;
        if DivideByInitialValues then
        begin
          ASeries.AddXY(ASeries.Count,Sens.B/ASeries.FirstValue);
        end
        else
        begin
          ASeries.AddXY(ASeries.Count,Sens.B);
        end;
      end;
    end;
  end;

end;

procedure TfrmModChart.FillTreeCombo;
var
  FileRoot : string;
  Filename : string;
  Index: integer;
  ANode: TTreeNode;
begin
  FileRoot := ChangeFileExt(OpenDialogOutputFile.FileName, '');
  FillFileList(FileRoot);
  Filename := ExtractFileName(OpenDialogOutputFile.FileName);
  for Index := 0 to treecomboFileNames.Items.Count -1 do
  begin
    ANode := treecomboFileNames.Items[Index];
    if LowerCase(ANode.Text) = LowerCase(Filename) then
    begin
      treecomboFileNames.Selected := ANode;
      break;
    end;
  end;
end;

procedure TfrmModChart.EnableControls;
begin
  sbZoomIn.Enabled := True;
  sbZoomOut.Enabled := True;
  sbPan.Enabled := True;
  sbPrint.Enabled := True;
  sbFormat.Enabled := True;
  sbOldFormatChart.Enabled := True;
  sbSaveImage.Enabled := True;

  miOpen.Enabled := True;
  miPrintSetup.Enabled := True;
  miPrint.Enabled := True;
  miPrintPreview.Enabled := True;
  miSaveChart.Enabled := True;
  miZoomin.Enabled := True;
  miZoomextents.Enabled := True;
  miPan.Enabled := True;
  miFormatchart.Enabled := True;
  miConfigure.Enabled := True;

  treecomboFileNames.Enabled := True;
end;

procedure TfrmModChart.Button1Click(Sender: TObject);
begin
  OpenDialogOutputFile.FileName := '';
  if OpenDialogOutputFile.Execute then
  begin
    ChartModflow.Cursor := crHourGlass;
    try
      EnableControls;

      FillTreeCombo;
      clb_OsPlotsymbols.Items.Clear;
      OpenFile(OpenDialogOutputFile.FileName);
    finally
      ChartModflow.Cursor := crDefault;
    end;
  end;
end;

function TfrmModChart.ReadStandardError(out StanError: string): boolean;
const
  StandardError = 'STANDARD ERROR OF THE REGRESSION:';
  StandardError2 ='"STANDARD ERROR OF THE REGRESSION: "';
var
  SSFileName: string;
  SSFile: TextFile;
  ALine: string;
begin
  result := False;
  SSFileName := ChangeFileExt(NameOfFile, '._ss');
  if FileExists(SSFileName) then
  begin
    AssignFile(SSFile, SSFileName);
    reset(SSFile);
    try
      While not EOF(SSFile) do
      begin
        ReadLn(SSFile, ALine);
        if Pos(StandardError, ALine) > 0 then
        begin
          ALine := Trim(Copy(ALine, Pos(StandardError, ALine) +
            Length(StandardError), MAXINT));
          StanError := ALine;
          result := True;
          Exit;
        end
      end;
    finally
      CloseFile(SSFile);
    end;
  end;
  SSFileName := ChangeFileExt(NameOfFile, '._dm');
  if FileExists(SSFileName) then
  begin
    AssignFile(SSFile, SSFileName);
    reset(SSFile);
    try
      While not EOF(SSFile) do
      begin
        ReadLn(SSFile, ALine);
        if Pos(StandardError2, ALine) > 0 then
        begin
          ALine := Trim(Copy(ALine, Pos(StandardError2, ALine) +
            Length(StandardError2), MAXINT));
          StanError := ALine;
          result := True;
          Exit;
        end
      end;
    finally
      CloseFile(SSFile);
    end;
  end;
end;

type
  TBubbleData = class(TObject)
    Coordinates: array[0..3] of double;
    Residuals: array[0..1] of double;
    PlotSymbol: integer;
    Name: string;
    function Value(Index: Integer): double;
  end;

procedure TfrmModChart.Read_nm(PlotAll: boolean);
const
  Epsilon = 1e-8;
var
  Simulated, Observed : double;
  PlotSymbol : integer;
  ObservationName : TString14;
  Color : TColor;
  StyleType : TStyleType;
  StyleIndex : integer;
  ColorIndex : integer;
  ALine: string;
  SplitterList: TStringList;
  PlotSymbols: TIntegerList;
  PlotSymbolIndex, psIndex: integer;
  ShouldPlot: boolean;
  Index: integer;
  Objects: TObjectList;
  InactivePoints : TPointSeries;
  ValuesToIgnore: TRealList;
  ValToIgnore: Double;
  AddPoint: Boolean;
  IgnoreIndex: Integer;
begin
  if PlotAll then
  begin
    InactivePoints := nil;
  end
  else
  begin
    InactivePoints := TPointSeries.Create(nil);
    InactivePoints.Title := 'De-emphasized points';
    SeriesList.AddObject(InactivePoints.Title, InactivePoints);
    ChartModflow.AddSeries(InactivePoints);
    InactivePoints.SeriesColor := clBlack;
    InactivePoints.Pointer.Style  := psSmallDot;
//    InactivePoints.Pointer.Visible := False;
    ChartModflow.Legend.Visible := False;
    ChartModflow.ExchangeSeries(InactivePoints,SerDataPoints);

    jvplChartControls.Visible := True;
    jvplChartControls.ActivePage := jvsp_nm;
  end;

  Objects := TObjectList.Create;
  ValuesToIgnore:= TRealList.Create;
  try
    for IgnoreIndex := 1 to rdgValuesToIgnoreNM.RowCount do
    begin
      if TryFortranStrToFloat(rdgValuesToIgnoreNM.Cells[0,IgnoreIndex], ValToIgnore) then
      begin
        ValuesToIgnore.Add(ValToIgnore)
      end;
    end;
    SerDataPoints.Active := True;
    ser1to1.Active := True;
    serBarChart.Active := False;

    cbLabelPoints.Enabled := True;
    chartModflow.ShowHint := True;
    cbLabelPointsClick(nil);
    chartModflow.BottomAxis.LabelStyle := talValue;
    chartModflow.BottomAxis.LabelsAngle := 0;
    if not EOF(FFile) then
    begin
      if ModelSelection in [msUcode2005,msModflow2005] then
      begin
        // ucode
        ReadLn(FFile, ALine);
      end;
    end;
    PlotSymbols := TIntegerList.Create;
    SplitterList := TStringList.Create;
    try
      PlotSymbols.Sorted := True;

      SplitterList.Delimiter := ' ';
      While not EOF(FFile) do
      begin
        Readln(FFile, ALine);
        SplitterList.DelimitedText := ALine;
        Simulated := FortranStrToFloat(SplitterList[0]);
        Observed := FortranStrToFloat(SplitterList[1]);
        PlotSymbol := StrToInt(SplitterList[2]);
        ObservationName := SplitterList[3];

        PlotSymbolIndex := PlotSymbols.IndexOf(PlotSymbol);
        if PlotSymbolIndex >= 0 then
        begin
          psIndex := clbNM.Items.IndexOf(IntToStr(PlotSymbol));
          ShouldPlot := PlotAll or clbNM.Checked[psIndex];
        end
        else
        begin
          PlotSymbols.Add(PlotSymbol);
          PlotSymbolIndex := PlotSymbols.IndexOf(PlotSymbol);
          Assert(PlotSymbolIndex >= 0);
          psIndex := clbNM.Items.IndexOf(IntToStr(PlotSymbol));
          if psIndex < 0 then
          begin
            ShouldPlot := True;
            if clbNM.Items.Count <= PlotSymbolIndex then
            begin
              clbNM.Items.Add(IntToStr(PlotSymbol))
            end
            else
            begin
              clbNM.Items.Insert(PlotSymbolIndex, IntToStr(PlotSymbol))
            end;
            clbNM.Checked[PlotSymbolIndex] := True;
          end
          else
          begin
            ShouldPlot := PlotAll or clbNM.Checked[psIndex];
          end;
        end;

        ObservationName := Trim(ObservationName);
        if ObservationName = '' then
        begin
          break;
        end;

        Assert(ValuesToIgnore.Count <= 1);
        AddPoint := True;
        if ValuesToIgnore.Count = 1 then
        begin
          AddPoint := False;
          ValToIgnore := ValuesToIgnore[0];
          if ValToIgnore = 0 then
          begin
            if Simulated <> 0 then
            begin
              AddPoint := True;
            end;
          end
          else
          begin
            if Abs((Simulated-ValToIgnore)/ValToIgnore) > Epsilon then
            begin
              AddPoint := True;
//                  ASeries.AddXY(Simulated, Observed, ObservationName, Color);
            end;
          end;
        end
        else
        begin
          AddPoint := True;
        end;

        if ShouldPlot then
        begin
          StyleIndex := (PlotSymbol -1) mod	(Ord(High(TSeriesPointerStyle))-1);

          StyleType := TStyleType.Create;
          StyleType.Style := Low(TSeriesPointerStyle);
          if StyleIndex > 0 then
          begin
            Inc(StyleType.Style,StyleIndex);
          end;
          if StyleType.Style >= psSmallDot then
          begin
            Inc(StyleType.Style,2);
          end;

          ObservationNames.AddObject(ObservationName, StyleType);
          ColorIndex := PlotSymbol-1;
          ColorIndex := ColorIndex mod (Length(ColorPalette));
          if ColorIndex < 0 then
          begin
            ColorIndex := ColorIndex + Length(ColorPalette)
          end;

          Color := ColorPalette[ColorIndex];

          if AddPoint then
          begin
            SerDataPoints.AddXY(Simulated, Observed, ObservationName, Color);
          end;            
        end
        else
        begin
          if AddPoint then
          begin
            InactivePoints.AddXY(Simulated, Observed, ObservationName);
          end;            
        end
      end;

      chartModflow.LeftAxis.Grid.Visible := False;
      chartModflow.LeftAxis.Increment := 0;

      for Index := clbNM.Items.Count -1 downto 0 do
      begin
        if PlotSymbols.IndexOf(StrToInt(clbNM.Items[Index])) < 0 then
        begin
          clbNM.Items.Delete(Index);
        end;
      end;

    finally
      SplitterList.Free;
      PlotSymbols.Free;
    end;
  finally
    Objects.Free;
    ValuesToIgnore.Free;
  end;
end;

procedure TfrmModChart.Read_os(FileType: TFileType; Extension : string);
const
  Epsilon = 1e-8;
var
  Simulated, Observed : double;
  PlotSymbol : integer;
  ObservationName : TString14;
  Color : TColor;
  MinValue, MaxValue : double;
  StyleType : TStyleType;
  StyleIndex : integer;
  ValueIndex : integer;
  ValueIncrement : double;
  Avalue :double;
  StandardError : double;
  YMax, YMin : double;
  ColorIndex : integer;
  Time: double;
  StanError: string;
  ALine: string;
  Plotser1to1 : boolean;
  XMax, XMin : double;
  SplitterList: TStringList;
  PlotSymbols: TIntegerList;
  PlotSymbolIndex, psIndex: integer;
  ShouldPlot: boolean;
  Index: integer;
  XYZT_FileExists: boolean;
  XYZT_FileName: string;
  XYZT_File: TStringList;
  LineIndex: integer;
  Values: TStringList;
  BubbleData: TBubbleData;
  Objects: TObjectList;
  NamedTimes: TStringList;
  BubbleIndex: integer;
  XYTR_FileExists : boolean;
  ASeries: TPointSeries;
  FirstPoint: boolean;
  IntList : TIntegerList;
  GroupPlotSymbols: TIntegerList;
  GroupNames: TStringList;
  GroupFileExists: boolean;
  GroupIndex: integer;
  PlotSeriesName: string;
  ColonPosition: integer;
//  RegressionFileName: string;
  ValuesToIgnore: TRealList;
  ValToIgnore: Double;
  AddPoint: Boolean;
  IgnoreIndex: integer;
begin
//  case ModelSelection of
//    msUcode2005:
//      begin
//        RegressionFileName := ChangeFileExt(treecomboFileNames.Selected.Text, '._dm');
//      end;
//  else Assert(False);
//  end;
//


  GroupPlotSymbols := TIntegerList.Create;
  GroupNames := TStringList.Create;
  ValuesToIgnore:= TRealList.Create;
  try
    for IgnoreIndex := 1 to rdgIgnoreValueOS.RowCount-1 do
    begin
      if TryFortranStrToFloat(rdgIgnoreValueOS.Cells[0,IgnoreIndex], ValToIgnore) then
      begin
        ValuesToIgnore.Add(ValToIgnore)
      end;
    end;
    GroupFileExists :=  GetGroupLabels(gtObservation, GroupPlotSymbols, GroupNames);

    ExplantionVisible := True;
    if (FileType = ft_os) then
    begin
      jvplChartControls.Visible := True;
      jvplChartControls.ActivePage := jvsp_os;
      cbRecentModflow.Enabled := (ModelSelection = msModflow);
      IsRecentFile := (ModelSelection = msModflow) and cbRecentModflow.Checked;
      XYTR_FileExists := (ModelSelection = msUcode2005)
        and FileExists(ChangeFileExt(NameOfFile, '._xyztwr'));

  //    rgPlotObservedVs.Enabled := IsRecentFile or XYTR_FileExists;
    end;

    Objects := TObjectList.Create;
    XYZT_File := TStringList.Create;
    NamedTimes := TStringList.Create;
    try
      Plotser1to1 := Filetype in [ft_os, ft_ww];
  //    SerDataPoints.Active := True;
      SerDataPoints.Active := False;
  //    ser1to1.Active := rgPlotObservedVs.ItemIndex = 0;;
      ser1to1.Active := True;
      serBarChart.Active := False;
      Time := 0;
      XYZT_FileExists := False;

      cbLabelPoints.Enabled := True;
      chartModflow.ShowHint := True;
      chartModflow.BottomAxis.LabelStyle := talValue;
      chartModflow.BottomAxis.LabelsAngle := 0;
      if not EOF(FFile) then
      begin
        if ModelSelection in [msUcode2005,msModflow2005] then
        begin
          // ucode
          ReadLn(FFile, ALine);
        end;
      end;
      PlotSymbols := TIntegerList.Create;
      SplitterList := TStringList.Create;
      try
        PlotSymbols.Sorted := True;

        SplitterList.Delimiter := ' ';
        FirstPoint := True;
        While not EOF(FFile) do
        begin
          if (Extension = '._nm') then
          begin
            // It should no longer be possible to reach
            // this position because _nm files are no
            // longer read by read_os.
            Readln(FFile, ALine);
            SplitterList.DelimitedText := ALine;
            Simulated := FortranStrToFloat(SplitterList[0]);
            Observed := FortranStrToFloat(SplitterList[1]);
            PlotSymbol := StrToInt(SplitterList[2]);
            ObservationName := SplitterList[3]
          end
          else
          begin
            if (Filetype in [ft_os,ft_ww]) and (ModelSelection = msModflow2005) then
            begin
              Readln(FFile, ALine);
              SplitterList.DelimitedText := ALine;
              Simulated := FortranStrToFloat(SplitterList[0]);
              Observed := FortranStrToFloat(SplitterList[1]);
              ObservationName := SplitterList[2];
              PlotSymbol := 1;
            end
            else
            begin
              Readln(FFile, ALine);
              SplitterList.DelimitedText := ALine;
              Simulated := FortranStrToFloat(SplitterList[0]);
              Observed := FortranStrToFloat(SplitterList[1]);
              // there is an ._os file from McNab on July 22, 2015 without
              // a plot symbol but that isn't the normal situation.
              if (ModelSelection = msModflow2005) and (FileType = ft_os) then
              begin
                PlotSymbol := 1;
                ObservationName := SplitterList[2];
              end
              else
              begin
                PlotSymbol := StrToInt(SplitterList[2]);
                ObservationName := SplitterList[3];
              end;
  //            if SplitterList.Count >= 5 then
  //            begin
  //              Time := FortranStrToFloat(SplitterList[4]);
  //            end;
              if Filetype <> ft_ww then
              begin
                if IsRecentFile then
                begin
                  Time := FortranStrToFloat(SplitterList[4]);
                end
                else if XYZT_FileExists then
                begin
                  BubbleIndex := NamedTimes.IndexOf(ObservationName);
                  Assert(BubbleIndex >= 0);
                  BubbleData := NamedTimes.Objects[BubbleIndex] as TBubbleData;
                  Time := BubbleData.Coordinates[3];
                end;
              end;
            end;
          end;
          if GroupFileExists then
          begin
            GroupIndex := GroupPlotSymbols.IndexOf(PlotSymbol);
            if GroupIndex >= 0 then
            begin
              PlotSeriesName := IntToStr(PlotSymbol) + ': ' + GroupNames[GroupIndex]
            end
            else
            begin
              PlotSeriesName := IntToStr(PlotSymbol);
            end;
          end
          else
          begin
            PlotSeriesName := IntToStr(PlotSymbol);
          end;
          PlotSymbolIndex := PlotSymbols.IndexOf(PlotSymbol);
          if PlotSymbolIndex >= 0 then
          begin
            psIndex := clb_OsPlotsymbols.Items.IndexOf(PlotSeriesName);
            ShouldPlot := clb_OsPlotsymbols.Checked[psIndex];
            ASeries := SeriesList.Objects[PlotSymbolIndex] as TPointSeries;

          end
          else
          begin
            PlotSymbolIndex := PlotSymbols.Add(PlotSymbol);
  //          PlotSymbolIndex := PlotSymbols.IndexOf(PlotSymbol);
            Assert(PlotSymbolIndex >= 0);
            psIndex := clb_OsPlotsymbols.Items.IndexOf(PlotSeriesName);
            if psIndex < 0 then
            begin
              ShouldPlot := True;
              if clb_OsPlotsymbols.Items.Count <= PlotSymbolIndex then
              begin
                clb_OsPlotsymbols.Items.Add(PlotSeriesName)
              end
              else
              begin
                clb_OsPlotsymbols.Items.Insert(PlotSymbolIndex, PlotSeriesName)
              end;
              clb_OsPlotsymbols.Checked[PlotSymbolIndex] := True;
            end
            else
            begin
              ShouldPlot := clb_OsPlotsymbols.Checked[psIndex];
            end;
            ASeries := TPointSeries.Create(nil);
            SeriesList.InsertObject(PlotSymbolIndex, IntToStr(PlotSymbol), ASeries);
            ASeries.OnClickPointer := SerDataPointsClickPointer;
            ASeries.OnGetMarkText := SerDataPointsGetMarkText;

            if GroupFileExists then
            begin
              GroupIndex := GroupPlotSymbols.IndexOf(PlotSymbol);
              if GroupIndex >= 0 then
              begin
                ASeries.Title := {IntToStr(PlotSymbol) + ': ' +} GroupNames[GroupIndex]
              end
              else
              begin
                ASeries.Title := IntToStr(PlotSymbol);
              end;
            end
            else
            begin
              ASeries.Title := IntToStr(PlotSymbol);
            end;

        //    SeriesList.AddObject(InactivePoints.Title, InactivePoints);
        //    ChartModflow.AddSeries(InactivePoints);
  //          ASeries.SeriesColor ://= clBlack;
  //          ASeries.Pointer.Style  := psSmallDot;
            ASeries.ShowInLegend := True;

            IntList := TIntegerList.Create;
            IntList.Sorted := True;
            FMarkedPointsList.Insert(PlotSymbolIndex, IntList);
          end;

          ObservationName := Trim(ObservationName);
          if ObservationName = '' then
          begin
            break;
          end;

          if ShouldPlot then
          begin
            StyleIndex := (PlotSymbol -1) mod	(Ord(High(TSeriesPointerStyle))-1);

            StyleType := TStyleType.Create;
            StyleType.Style := Low(TSeriesPointerStyle);
            if StyleIndex > 0 then
            begin
              Inc(StyleType.Style,StyleIndex);
            end;
            if StyleType.Style >= psSmallDot then
            begin
              Inc(StyleType.Style,2);
            end;

            ObservationNames.AddObject(ObservationName, StyleType);
            ColorIndex := PlotSymbol-1;
            ColorIndex := ColorIndex mod (Length(ColorPalette));
            if ColorIndex < 0 then
            begin
              ColorIndex := ColorIndex + Length(ColorPalette)
            end;

            Color := ColorPalette[ColorIndex];
            ASeries.SeriesColor := Color;

            Assert(ValuesToIgnore.Count <= 1);
            AddPoint := True;
            if ValuesToIgnore.Count = 1 then
            begin
              AddPoint := False;
              ValToIgnore := ValuesToIgnore[0];
              if ValToIgnore = 0 then
              begin
                if Simulated <> 0 then
                begin
                  AddPoint := True;
                end;
              end
              else
              begin
                if Abs((Simulated-ValToIgnore)/ValToIgnore) > Epsilon then
                begin
                  AddPoint := True;
                end;
              end;
            end
            else
            begin
              AddPoint := True;
            end;

            if AddPoint then
            begin
              ASeries.AddXY(Simulated, Observed, ObservationName, Color);
            end;

  //          if rgPlotObservedVs.ItemIndex = 0 then
  //          begin
  //          end
  //          else
  //          begin
  //            ASeries.AddXY(Time, Simulated-Observed, ObservationName, Color);
  //          end;

            if AddPoint then
            begin
              if FirstPoint then
              begin
                MaxValue := Max(Simulated, Observed);
                MinValue := Min(Simulated, Observed);
                YMax := Observed;
                YMin := Observed;
                FirstPoint := False;
              end
              else
              begin
                MaxValue := Max(Simulated, MaxValue);
                MinValue := Min(Simulated, MinValue);
                MaxValue := Max(MaxValue, Observed);
                MinValue := Min(MinValue, Observed);
                YMax := Max(YMax, Observed);
                YMin := Min(YMin, Observed);
              end;
            end;              
          end;
        end;
        Assert(SeriesList.Count = clb_OsPlotsymbols.Items.Count);
        for Index := 0 to SeriesList.Count -1 do
        begin
          ASeries := SeriesList.Objects[Index] as TPointSeries;
          chartModflow.AddSeries(ASeries);
          ASeries.Visible := clb_OsPlotsymbols.Checked[Index];
        end;

        if Plotser1to1 then
        begin
          // plot a line using the formulat Y = X;
   //       MaxValue := Max(ASeries.XValues.MaxValue, ASeries.YValues.MaxValue);
  //        MinValue := Min(ASeries.XValues.MinValue, ASeries.YValues.MinValue);
          ValueIncrement := (MaxValue - MinValue)/20;
          for ValueIndex := 0 to 20-1 do
          begin
            Avalue := MinValue + ValueIndex * ValueIncrement;
            ser1to1.AddXY(Avalue, Avalue, '', clTeeColor);
          end;

          ser1to1.AddXY(MaxValue, MaxValue, '', clTeeColor);
        end;

        chartModflow.LeftAxis.Grid.Visible := False;
        chartModflow.LeftAxis.Increment := 0;

        if (Extension = '._ws') then
        begin
          if ReadStandardError(StanError) then
          begin
            StatusBar1.Panels[1].Text := 'Standard Error = ' + StanError
              + ' (The Y grid increment is set to the standard error)';
            StandardError := FortranStrToFloat(StanError);
            chartModflow.LeftAxis.Increment := StandardError;
            chartModflow.LeftAxis.Grid.Visible := True;
            adeStandardError.Text := StanError;
          end
          else
          begin
            adeStandardError.Enabled := True;
            btnRead.Enabled := True;
            lblStandardErrorOfRegression.Enabled := True;

            if (adeStandardError.Text <> '0') and (adeStandardError.Text <> '') then
            begin
              StatusBar1.Panels[1].Text := 'Standard Error = '
                + adeStandardError.Text
                + ' (The Y grid increment is set to the standard error)';
              StandardError := InternationalStrToFloat(adeStandardError.Text);
              chartModflow.LeftAxis.Increment := StandardError;
              chartModflow.LeftAxis.Grid.Visible := True;
            end;
          end;
        end;
        case ModelSelection of
          msModflow, msModflow2005:
            begin
              btnRead.Caption := 'Read from "global" or "list" file';
            end;
          msUcode2005:
            begin
              btnRead.Caption := 'Read from "*._dm" file';
            end;
        else Assert(False);
        end;
        if Extension = '._ws' then
        begin
  //        YMax := Abs(SerDataPoints.MaxYValue);
  //        YMin := Abs(SerDataPoints.MinYValue);
          chartModflow.LeftAxis.Automatic := False;
          chartModflow.LeftAxis.AutomaticMaximum := False;
          chartModflow.LeftAxis.AutomaticMinimum := False;
          SetMaxAndMinAxisValues(chartModflow.LeftAxis, Max(YMax,YMin), -Max(YMax,YMin));
        end
        else if Extension = '._ww' then
        begin
          {
          XMax := Max( SerDataPoints.MaxXValue, SerDataPoints.MaxYValue);
          XMin := Min(SerDataPoints.MinXValue, SerDataPoints.MinYValue);
          chartModflow.BottomAxis.Automatic := False;
          chartModflow.BottomAxis.AutomaticMaximum := False;
          chartModflow.BottomAxis.AutomaticMinimum := False;
          SetMaxAndMinAxisValues(chartModflow.BottomAxis, XMax, XMin);

          chartModflow.LeftAxis.Automatic := False;
          chartModflow.LeftAxis.AutomaticMaximum := False;
          chartModflow.LeftAxis.AutomaticMinimum := False;
          SetMaxAndMinAxisValues(chartModflow.LeftAxis, XMax, XMin);
          }

        end;
        for Index := clb_OsPlotsymbols.Items.Count -1 downto 0 do
        begin
          PlotSeriesName := clb_OsPlotsymbols.Items[Index];
          ColonPosition := Pos(':',PlotSeriesName);
          if ColonPosition > 0 then
          begin
            PlotSeriesName := Copy(PlotSeriesName, 1, ColonPosition-1);
          end;

          if PlotSymbols.IndexOf(StrToInt(PlotSeriesName)) < 0 then
          begin
            clb_OsPlotsymbols.Items.Delete(Index);
          end;
        end;

      finally
        SplitterList.Free;
        PlotSymbols.Free;
      end;
    finally
      XYZT_File.Free;
      Objects.Free;
      NamedTimes.Free;
    end;
    cbLabelPointsClick(nil);
  finally
    GroupPlotSymbols.Free;
    GroupNames.Free;
    ValuesToIgnore.Free;
  end;
end;

procedure TfrmModChart.MultipleSeriesClickPointer(Sender: TCustomSeries;
  ValueIndex, X, Y: Integer);
var
  Location : integer;
  LocalPoints: TIntegerList;
  Position: integer;
begin
  Position := SeriesList.IndexOfObject(Sender);
  if Position >= 0 then
  begin
    LocalPoints := rdRgNameList[Position];

    if cbLabelPoints.Checked then
    begin
      Location := LocalPoints.IndexOf(ValueIndex);
      if Location > -1 then
      begin
        LocalPoints.Delete(Location);
      end
      else
      begin
        LocalPoints.Add(ValueIndex);
      end;
      ChartModflow.Invalidate;
    end;
  end;

//  StatusBar1.Panels[0].Text := 'Observation name: ' + ObservationNames[ValueIndex];
end;


procedure TfrmModChart.SerDataPointsClickPointer(Sender: TCustomSeries;
  ValueIndex, X, Y: Integer);
var
  Location : integer;
  Position: integer;
  MarkList: TIntegerList;
begin

  StatusBar1.Panels[0].Text := 'Observation name: ' + Sender.Labels[ValueIndex];
//  StatusBar1.Panels[0].Text := 'Observation name: ' + ObservationNames[ValueIndex];
  if cbLabelPoints.Checked then
  begin
    Position := SeriesList.IndexOfObject(Sender);
    if (FMarkedPointsList.Count = 0) or (Position < 0) then
    begin
      MarkList := MarkedPoints;
    end
    else
    begin
      MarkList := FMarkedPointsList[Position];
    end;

    Location := MarkList.IndexOf(ValueIndex);
    if Location > -1 then
    begin
      MarkList.Delete(Location);
    end
    else
    begin
      MarkList.Add(ValueIndex);
    end;
    ChartModflow.Invalidate;
  end;
end;

procedure TfrmModChart.FormCreate(Sender: TObject);
var
  FileCheck: array[0..255] of char;
  Filter: string;
  Index: integer;
  FileName: string;
  Option: string;
begin
  rdgIgnoreValueOS.Cells[0,1] := 'Values to ignore';
  rdgValuesToIgnoreNM.Cells[0,1] := 'Values to ignore';

  rdeRThreshold.Text := FloatToStr(1.2);
  rdeConfidenceInterval.Text := FloatToStr(99.9);
  jvpl_mcmc_pred.ActivePageIndex := 0;
  pc_NM.ActivePageIndex := 0;
  pcXyzt.ActivePageIndex := 0;
  pcLinp.ActivePageIndex := 0;
  pgcParameters.ActivePageIndex := 0;
  OrangeY := lblOrangeCI.Top;
  RedY := lblRedCI.Top;
  OriginalTreeComboWidth := treecomboFileNames.Width;
  Filter := 'All supported file types|';
  for Index := 0 to MaxExtensions do
  begin
    if (Index = 0) or (Classifiations[Index].Extension
      <> Classifiations[Index-1].Extension) then
    begin
      Filter := Filter +  '*' + Classifiations[Index].Extension;
      if Index < MaxExtensions then
      begin
        Filter := Filter + ';';
      end;
    end;
  end;
  for Index := 0 to MaxExtensions do
  begin
    Filter := Filter + '|'
      + Classifiations[Index].FilterDescription
      + ' (*' + Classifiations[Index].Extension
      + ')|*' + Classifiations[Index].Extension;
  end;
  OpenDialogOutputFile.Filter := Filter;

  rdgXyztLegend.Cells[0,0] := 'Legend Values';

  ExtraSeriesList := TObjectList.Create;
  RdRgStyleList := TObjectList.Create;
  rdRgNameList := TObjectList.Create;
  SerDataPoints.XValues.Order := loNone;
  ser1to1.XValues.Order := loNone;
  serBarChart.XValues.Order := loNone;

  ObservationNames := TStringList.Create;
  ParameterNames := TStringList.Create;
  SeriesList := TStringList.Create;
  MarkedPoints := TIntegerList.Create;
  MenuItems := TObjectList.Create;
  MarkedPoints.Sort;
  FMarkedPointsList := TObjectList.Create;
  ExplantionVisible := False;
  ExplanationFontHeight := -11;
  GetModuleFileName(HInstance, Filecheck, 255);
  Application.HelpFile := ExtractFileDir(String(Filecheck)) + '\GW_Chart.chm';
  mHHelp := THookHelpSystem.Create(Application.HelpFile, '', htHHAPI);

  RangeSeries:= TRangeSeries.Create(self);
  RangeSeries.ParentChart := chartModflow;
  RangeSeries.Title := 'Parameter Range';
  RangeSeries.Active := False;
  RangeSeries.XValues.Order := loNone;
  RangeSeries.Pointer.Style := psCircle;
  RangeSeries.ColorEachPoint := False;

  HorizRangeSeries:= TRangeSeries.Create(self);
  HorizRangeSeries.ParentChart := chartModflow;
  HorizRangeSeries.Title := 'Parameter Range';
  HorizRangeSeries.Active := False;
  HorizRangeSeries.DrawVertical := False;
  HorizRangeSeries.OnGetPointerStyle := SerDataPointsGetPointerStyle;
  HorizRangeSeries.OnGetMarkText := SerDataPointsGetMarkText;
  HorizRangeSeries.ColorEachPoint := True;
  HorizRangeSeries.OnClickPointer := SerDataPointsClickPointer;
  HorizRangeSeries.Marks.Visible := True;
  HorizRangeSeries.XValues.Order := loNone;
  HorizRangeSeries.Pointer.Style := psCircle;

  if ParamCount > 0 then
  begin
    Option := ParamStr(1);
    if SameText(ConvertOption, Option) then
    begin
    //  do nothing
    end
    else
    begin
      FileName := ParamStr(1);
      if FileExists(FileName) and (FileNameToFileType(FileName) <> ftUnknown) then
      begin
        EnableControls;
        OpenDialogOutputFile.FileName := FileName;
        FillTreeCombo;
        OpenFile(FileName);
      end;
    end;
  end;
end;

procedure TfrmModChart.ClearObservationNames;
var
  Index : integer;
begin
  for Index := 0 to ObservationNames.Count -1 do
  begin
    ObservationNames.Objects[Index].Free;
  end;
  ObservationNames.Clear;
end;

procedure TfrmModChart.FormDestroy(Sender: TObject);
begin
  Fmcmc_Variables.Free;
  ClearObservationNames;
  ObservationNames.Free;
  ParameterNames.Free;
  SeriesList.Free;
  MarkedPoints.Free;
  FMarkedPointsList.Free;
  MenuItems.Free;
  SdValueList.Free;
  RangeSeries.Free;
  HorizRangeSeries.Free;
  ExtraSeriesList.Free;
  RdRgStyleList.Free;
  rdRgNameList.Free;
  //Unhook and free
  mHHelp.Free;
  HHCloseAll;     //Close help before shutdown or big trouble
end;

procedure TfrmModChart.sbZoomInClick(Sender: TObject);
begin
  if Sender <> sbZoomIn then
  begin
    sbZoomIn.Down := True;
  end;
  chartModflow.AllowZoom := sbZoomIn.Down;

end;

procedure TfrmModChart.sbZoomOutClick(Sender: TObject);
begin
  chartModflow.BottomAxis.Automatic := True;
  chartModflow.LeftAxis.Automatic := True;
  chartModflow.UndoZoom;
  sbZoomIn.Down := False;
  sbPan.Down := False;
end;

procedure TfrmModChart.chartModflowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if sbPan.Down then
  begin
    Begun := True;
    XStart := chartModflow.BottomAxis.CalcPosPoint(X);
    YStart := chartModflow.LeftAxis.CalcPosPoint(Y);
  end;
end;

procedure TfrmModChart.chartModflowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if sbPan.Down then
  begin
    Screen.Cursor := crHandPoint;
  end
  else if sbZoomIn.Down then
  begin
    Screen.Cursor := crCross;
  end
  else
  begin
    Screen.Cursor := crDefault;
  end;
  if sbPan.Down and Begun then
  begin
    XEnd := chartModflow.BottomAxis.CalcPosPoint(X);
    YEnd := chartModflow.LeftAxis.CalcPosPoint(Y);
    chartModflow.BottomAxis.Scroll(XStart - XEnd, False);
    chartModflow.LeftAxis.Scroll(YStart - YEnd, False);
  end;

end;

procedure TfrmModChart.chartModflowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if sbPan.Down and Begun then
  begin
    XEnd := chartModflow.BottomAxis.CalcPosPoint(X);
    YEnd := chartModflow.LeftAxis.CalcPosPoint(Y);
    chartModflow.BottomAxis.Scroll(XStart - XEnd, False);
    chartModflow.LeftAxis.Scroll(YStart - YEnd, False);
    Begun := False;
  end;

end;

procedure TfrmModChart.sbPanClick(Sender: TObject);
begin
  if Sender <> sbPan then
  begin
    sbPan.Down := not sbPan.Down;
  end;
  if sbPan.Down then
  begin
    miPan.Caption := 'Stop Panning';
  end
  else
  begin
    Begun := False;
    Screen.Cursor := crDefault;
    miPan.Caption := 'Pan';
  end;
  chartModflow.AllowZoom := sbZoomIn.Down;

end;

{procedure TfrmModChart.SortBarChart(ASeries : TChartSeries);
var
  InnerIndex, OuterIndex : integer;
  OuterY : double;
begin
  for OuterIndex := 0 to ASeries.Count -2 do
  begin
    OuterY := ASeries.YValue[OuterIndex];
    for InnerIndex := OuterIndex+1 to ASeries.Count-1 do
    begin
      if ASeries.YValue[InnerIndex] > OuterY then
      begin
        ASeries.SwapValueIndex(InnerIndex,OuterIndex);
        OuterY := ASeries.YValue[OuterIndex];
      end;
    end;

  end;

end;  }

procedure TfrmModChart.Read_scData(ValueList : TObjectList;
  var WithPrior: boolean);
const
  MaxModflowParameterNameLength = 11;
  MaxUCodeParameterNameLength = 12;
var
  ALine : string;
  AValue : TBarValue;
  MaxParamNameLength: integer;
  SpacePosition: integer;
begin
  WithPrior := False;
  if not EOF(FFile) then
  begin
    if ModelSelection = msUcode2005 then
    begin
      // ucode, skip first line.
      Readln(FFile, ALine);
    end;
  end;
  While not EOF(FFile) do
  begin
    Readln(FFile, ALine);
    if (ModelSelection = msUcode2005) and (Trim(ALine) = '"WITH PRIOR:"') then
    begin
      WithPrior := True;
      break;
    end;

    if Length(ALine)> 0 then
    begin
      AValue := TBarValue.Create;
      MaxParamNameLength := 0;
      case ModelSelection of
        msModflow: // MODFLOW
          begin
            MaxParamNameLength := MaxModflowParameterNameLength;
          end;
        msUcode2005: // UCODE
          begin
            // UCODE prints a blank character before the beginning of the parameter name.
            MaxParamNameLength := MaxUCodeParameterNameLength+1;
            ALine := Trim(ALine);
          end;
      else Assert(False);
      end;
      AValue.ParameterName := UpperCase(Trim(Copy(
        ALine,1,MaxParamNameLength)));
      ALine := Trim(Copy(ALine,Succ(MaxParamNameLength), MAXINT));
      SpacePosition := Pos(' ', ALine);
      if SpacePosition <= 0 then
      begin
        SpacePosition := MAXINT;
      end;

      AValue.Value := InternationalStrToFloat(
        Trim(Copy(ALine,1,SpacePosition)));
      AValue.Color := clTeeColor;
      ValueList.Add(AValue);
    end;
  end;
end;

procedure TfrmModChart.GetGroupNamesFromGmFile(
  GroupPlotSymbols: TObjectStringList; SortBy: TGroupSort);
var
  GM_FileName: string;
  GMFile: TStringList;
  LineIndex: integer;
  Splitter: TStringList;
  GroupName: string;
  PS: string;
  PlotSymbol: integer;
  GroupI: TGroupIndex;
begin
  Splitter := TStringList.Create;
  try
    Splitter.QuoteChar := '"';
    Splitter.Delimiter := ' ';

    GroupPlotSymbols.CaseSensitive := False;
    GM_FileName := ChangeFileExt(NameOfFile, '._gm');
    if FileExists(GM_FileName) then
    begin
      GMFile := TStringList.Create;
      try
        GMFile.LoadFromFile(GM_FileName);
        for LineIndex := 1 to GMFile.Count -1 do
        begin
          Splitter.DelimitedText := GMFile[LineIndex];
          if Splitter.Count = 3 then
          begin
            GroupName := Splitter[0];
            PlotSymbol := StrToInt(Splitter[2]);
            case SortBy of
              gsPlotSymbol:
                begin
                  PS := Splitter[2];
                  if GroupPlotSymbols.IndexOf(PS) < 0 then
                  begin
                    GroupI:= TGroupIndex.Create;
                    GroupI.PlotSymbol := PlotSymbol;
                    GroupI.GroupName := GroupName;
                    GroupPlotSymbols.AddObject(PS, GroupI)
                  end;
                end;
              gsName:
                begin
                  if GroupPlotSymbols.IndexOf(GroupName) < 0 then
                  begin
                    GroupI:= TGroupIndex.Create;
                    GroupI.PlotSymbol := PlotSymbol;
                    GroupI.GroupName := GroupName;
                    GroupPlotSymbols.AddObject(GroupName, GroupI)
                  end;
                end;
            else Assert(False);
            end;

          end;
        end;
      finally
        GMFile.Free;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TfrmModChart.Read_scgrp(ValueList: TList);
var
  ALine: string;
  AGroup: TGroup;
  GroupList: TList;
  Param: TGroupParam;
  GroupIndex: integer;
  ParamIndex: Integer;
  Splitter: TStringList;
  AFactor: Double;
  BarValue: TBarValue;
  SumValues: array of double;
  Bar: TBarSeries;
//  GM_FileName: string;
//  GMFile: TStringList;
  GroupPlotSymbols: TObjectStringList;
//  LineIndex: integer;
//  GroupName: string;
  GroupI: TGroupIndex;
//  PlotSymbol: integer;
  PSIndex: integer;
  FirstIndex, LastIndex: integer;
  N: integer;
  ScalingFactor: double;
  CIndex: integer;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspScSo;
  chartModflow.Legend.Inverted := True;
  Splitter := TStringList.Create;
  GroupList := TObjectList.Create;
  GroupPlotSymbols := TObjectStringList.Create;
  chartModflow.BottomAxis.LabelsAngle := 90;
  try
    Splitter.QuoteChar := '"';
    Splitter.Delimiter := ' ';

    // Step 1. Get plot symbols from ._gm file.
    GetGroupNamesFromGmFile(GroupPlotSymbols, gsName);

    // step 2. read _scgrp
    AGroup := nil;
    while not EOF(FFile) do
    begin
      ReadLn(FFile, ALine);
      ALine := Trim(ALine);
      if ALine <> '' then
      begin
        Splitter.DelimitedText := ALine;
        if Splitter[0] = 'GROUP NAME' then
        begin
          AGroup := TGroup.Create;
          GroupList.Add(AGroup);
          AGroup.GroupName := Splitter[1];
          AGroup.NumberInGroup := StrToInt(Splitter[3]);
          PSIndex := GroupPlotSymbols.IndexOf(AGroup.GroupName);
          if PSIndex >= 0 then
          begin
            GroupI := GroupPlotSymbols.Objects[PSIndex] as TGroupIndex;
            AGroup.PlotSymbol := GroupI.PlotSymbol;
          end
          else
          begin
            AGroup.PlotSymbol := GroupList.Count - 1;
          end;
        end
        else if Splitter[0] = 'PARAMETER NAME' then
        begin
          Continue;
        end
        else
        begin
          Param:= TGroupParam.Create;
          AGroup.AddParam(Param);
          Param.ParameterName := Splitter[0];
          Param.CompositeScaledSensitivity := FortranStrToFloat(Splitter[1]);
        end;
      end;
    end;

    // Step 3 check groups agains sc data
    for GroupIndex := 0 to GroupList.count -1 do
    begin
      AGroup := GroupList[GroupIndex];
      Assert(AGroup.ParamCount = ValueList.Count);
      for ParamIndex := 0 to AGroup.ParamCount -1 do
      begin
        Param := AGroup[ParamIndex];
        BarValue := ValueList[ParamIndex];
        Assert(UpperCase(Param.ParameterName) = UpperCase(BarValue.ParameterName));
      end;
    end;

    SetLength(SumValues, ValueList.Count);
    for ParamIndex := 0 to Length(SumValues) -1 do
    begin
      SumValues[ParamIndex] := 0;
    end;

    // Step 4, multiply param values by Sqrt of NumberInGroup.
    // and calculate sum
    for GroupIndex := 0 to GroupList.count -1 do
    begin
      AGroup := GroupList[GroupIndex];
      AFactor := Sqrt(AGroup.NumberInGroup);
      for ParamIndex := 0 to AGroup.ParamCount -1 do
      begin
        Param := AGroup[ParamIndex];
        Param.CompositeScaledSensitivity :=
          Param.CompositeScaledSensitivity * AFactor;
        SumValues[ParamIndex] := SumValues[ParamIndex]
          + Param.CompositeScaledSensitivity;
      end;
    end;

    // step 5. scale by sum and css
    for GroupIndex := 0 to GroupList.count -1 do
    begin
      AGroup := GroupList[GroupIndex];
      for ParamIndex := 0 to AGroup.ParamCount -1 do
      begin
        Param := AGroup[ParamIndex];
        BarValue := ValueList[ParamIndex];

        if SumValues[ParamIndex] = 0 then
        begin
          Param.CompositeScaledSensitivity := 0;
        end
        else
        begin
          Param.CompositeScaledSensitivity := Param.CompositeScaledSensitivity
            * BarValue.Value / SumValues[ParamIndex];
        end;

      end;
    end;

    ScalingFactor := 1;
    if (rgScSoSorting.ItemIndex = 1) or cbRatio.Checked then
    begin
      // Step 6. Sort the data.
      ValueList.Sort(SortBarValues);
      if (rgScSoSorting.ItemIndex = 1) then
      begin
        for GroupIndex := 0 to GroupList.count -1 do
        begin
          AGroup := GroupList[GroupIndex];
          AGroup.SortParameters(ValueList);
        end;
      end;
      // set the scaling factor and the maximum value
      if cbRatio.Checked then
      begin
        BarValue := ValueList[0];
        ScalingFactor := BarValue.Value;

        chartModflow.LeftAxis.AutomaticMaximum := False;
        chartModflow.LeftAxis.Maximum := 1;

        FixLabelSize;
      end;
    end;

    // Step 7. Select the data to plot.
    FirstIndex := 0;
    LastIndex := ValueList.Count -1;
    case rgItemsToPlot_So.ItemIndex of
      1:
        begin
          // First N Items;
          N := seN_so.AsInteger;
          LastIndex := N-1;
        end;
      2:
        begin
          // Last N Items;
          N := seN_so.AsInteger;
          FirstIndex := LastIndex-N+1;
        end;
    end;

    // Step 8. plot data.
    ExplantionVisible := True;

    for GroupIndex := 0 to GroupList.count -1 do
    begin
      AGroup := GroupList[GroupIndex];
      Bar:= TBarSeries.Create(self);
      SeriesList.AddObject(AGroup.GroupName, Bar);
      Bar.Title := AGroup.GroupName;
      Bar.Marks.Visible := False;
      CIndex := AGroup.PlotSymbol-1 mod MaxDefaultColors;
      if CIndex < 0 then
      begin
        CIndex := CIndex + MaxDefaultColors;
      end;

      Bar.Color := ColorPalette[CIndex];
      chartModflow.AddSeries(Bar);
      Bar.MultiBar := mbStacked;
      Bar.StackGroup := 0;

      for ParamIndex := FirstIndex to LastIndex do
      begin
        Param := AGroup[ParamIndex];
        Bar.Add(Param.CompositeScaledSensitivity/ScalingFactor,
          Param.ParameterName, clTeeColor);

      end;
    end;
  finally
//    for GroupIndex := 0 to GroupPlotSymbols.Count -1 do
//    begin
//      GroupPlotSymbols.Objects[GroupIndex].Free;
//    end;
    GroupPlotSymbols.Free;

    GroupList.Free;
    Splitter.Free;
  end;

end;

procedure TfrmModChart.FixLabelSize(LabelText: string = '');
var
  TempFont: TTeeFont;
begin
    // This is a work-around for a bug in TChart.
    // Without it, the left axis labels overlap the left axis title.
  if chartModflow.LeftAxis.LabelsAngle = 0 then
  begin
    TempFont := TTeeFont.Create(nil);
    try
      TempFont.Assign(chartModflow.Canvas.Font) ;
      chartModflow.Canvas.Font.Assign(chartModflow.LeftAxis.LabelsFont);
      if Text = '' then
      begin
        chartModflow.LeftAxis.LabelsSize := chartModflow.Canvas.TextWidth('1.0');
      end
      else
      begin
        chartModflow.LeftAxis.LabelsSize := chartModflow.Canvas.TextWidth('LabelText');
      end;
      chartModflow.Canvas.Font.Assign(TempFont);
    finally
      TempFont.Free;
    end;
  end;
end;


procedure TfrmModChart.Read_sc;
var
  AValue, OtherValue : TBarValue;
  ValueList,SecondValueList : TObjectList;
  Index : integer;
  ShouldPlot: boolean;
  WithPrior: boolean;
  OtherIndex: integer;
  Largest: double;
  LargestWithPrior: double;
begin
  WithPrior := False;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspScSo;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := True;
  ValueList := TObjectList.Create;
  SecondValueList := TObjectList.Create;
  FixLabelSize;
  try
    chartModflow.BottomAxis.LabelsAngle := 90;
    Read_scData(ValueList, WithPrior);
    if WithPrior then
    begin
      Read_scData(SecondValueList, WithPrior);
      if SecondValueList.Count > 0 then
      begin
        serBarChart2.Active := True;

        serBarChart.SeriesColor := clRed;
        serBarChart2.SeriesColor := clBlue;
        serBarChart.Title := 'css - obs';
        serBarChart2.Title := 'css - obs+prior';
        serBarChart.ShowInLegend := True;
        serBarChart2.ShowInLegend := True;
        ExplantionVisible := True;
      end;
    end
    else
    begin
      serBarChart.SeriesColor := clYellow;
      ExplantionVisible := False;
    end;

    Largest := 1;
    LargestWithPrior := 1;
    if cbRatio.Checked then
    begin
      // This is a work-around for a bug in TChart.
      // Without it, the left axis labels overlap the left axis title.

      FixLabelSize;

      if ValueList.Count > 0 then
      begin
        AValue := ValueList[0] as TBarValue;
        Largest := AValue.Value;
        for Index := 1 to ValueList.Count -1 do
        begin
          AValue := ValueList[Index] as TBarValue;
          if AValue.Value > Largest then
          begin
            Largest := AValue.Value
          end;
        end;
      end;

      if SecondValueList.Count > 0 then
      begin
        AValue := SecondValueList[0] as TBarValue;
        LargestWithPrior := AValue.Value;
        for Index := 1 to SecondValueList.Count -1 do
        begin
          AValue := SecondValueList[Index] as TBarValue;
          if AValue.Value > LargestWithPrior then
          begin
            LargestWithPrior := AValue.Value
          end;
        end;
      end;

    end;


    if rgScSoSorting.ItemIndex = 1 then
    begin
      ValueList.Sort(SortBarValues);
    end;

    for Index := 0 to ValueList.Count -1 do
    begin
      case rgItemsToPlot_So.ItemIndex of
        0:
          begin
            ShouldPlot := True;
          end;
        1:
          begin
            ShouldPlot := Index < seN_so.AsInteger;
          end;
        2:
          begin
            ShouldPlot := ValueList.Count - Index -1 < seN_so.AsInteger;
          end;
      else Assert(False);
      end;
      if ShouldPlot then
      begin
        AValue := ValueList[Index] as TBarValue;
        if cbRatio.Checked and (Largest <> 0) then
        begin
          serBarChart.Add(AValue.Value/Largest, AValue.ParameterName, AValue.Color);
        end
        else
        begin
          serBarChart.Add(AValue.Value, AValue.ParameterName, AValue.Color);
        end;

        if serBarChart2.Active then
        begin
          for OtherIndex := 0 to SecondValueList.Count -1 do
          begin
            OtherValue := SecondValueList[OtherIndex] as TBarValue;
            if AValue.ParameterName = OtherValue.ParameterName then
            begin
              if cbRatio.Checked and (LargestWithPrior <> 0) then
              begin
                serBarChart2.Add(OtherValue.Value/LargestWithPrior, OtherValue.ParameterName,
                  OtherValue.Color);
              end
              else
              begin
                serBarChart2.Add(OtherValue.Value, OtherValue.ParameterName,
                  OtherValue.Color);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    ValueList.Free;
    SecondValueList.Free;
  end;
end;

type
  TSvdData = class(TObject)
    ParameterName: string;
    Total: double;
    RatioToLargest: double;
    Values: array of double;
  end;

function CompareSvdData(Item1, Item2: Pointer): Integer;
var
  SvdData1, SvdData2 : TSvdData;
begin
  SvdData1 := Item1;
  SvdData2 := Item2;
  result := Sign(SvdData2.Total - SvdData1.Total);
end;

procedure TfrmModChart.Read_sc_svd_Data(ValueList : TObjectList;
  SortValues: boolean);
var
  Line: string;
  List: TStringList;
  ValueCount: integer;
  SvdData: TSvdData;
  Index: integer;
begin
  List := TStringList.Create;
  try
    // skip first two lines.
    ReadLn(FFile);
    ReadLn(FFile, Line);
    List.Delimiter := ' ';
    List.QuoteChar := '"';
    List.DelimitedText := Line;
    ValueCount := List.Count - 3;
    while not EOF(FFile) do
    begin
      ReadLn(FFile, Line);
      if (Line <> '') and (Line <> ' "NO PRIOR INFORMATION WAS USED IN THE REGRESSION"') then
      begin
        List.DelimitedText := Line;
        Assert(List.Count = ValueCount + 3);
        SvdData := TSvdData.Create;
        ValueList.Add(SvdData);
        SetLength(SvdData.Values, ValueCount);
        SvdData.ParameterName := List[0];
        SvdData.Total := FortranStrToFloat(List[1]);
        SvdData.RatioToLargest := FortranStrToFloat(List[2]);
        for Index := 0 to ValueCount-1 do
        begin
          if List[Index+3] = 'NaN' then
          begin
            SvdData.Values[Index] := 0;
          end
          else
          begin
            SvdData.Values[Index] := FortranStrToFloat(List[Index+3]);
          end;
        end;
      end;
    end;
    if SortValues then
    begin
      ValueList.Sort(CompareSvdData);
    end;

  finally
    List.Free;
  end;

end;

procedure TfrmModChart.Read_sc_svd;
var
  ValueList : TObjectList;
  DataIndex : integer;
  ParamIndex: integer;
  ShouldPlot: boolean;
  Bar: TBarSeries;
  SvdData: TSvdData;
  RatioUsed: boolean;
begin
  ExplantionVisible := True;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspScSo;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := False;
  serBarChart.MultiBar := mbStacked;
  ValueList := TObjectList.Create;
  RatioUsed := cbRatio.Checked;
  try
    chartModflow.BottomAxis.LabelsAngle := 90;
    FixLabelSize;
    Read_sc_svd_Data(ValueList, rgScSoSorting.ItemIndex = 1);
    if ValueList.Count > 0 then
    begin
      SvdData := ValueList[0] as TSvdData;
      for DataIndex := 0 to Length(SvdData.Values)-1 do
      begin
        Bar:= TBarSeries.Create(self);
        SeriesList.AddObject(IntToStr(DataIndex+1), Bar);
        Bar.Title := 'Vector ' + IntToStr(DataIndex+1);
        Bar.MultiBar := mbStacked;
        Bar.Marks.Visible := False;
        Bar.Color := ColorPalette[DataIndex mod MaxDefaultColors];
        chartModflow.AddSeries(Bar);
      end;

      for ParamIndex := 0 to ValueList.Count -1 do
      begin
        SvdData := ValueList[ParamIndex] as TSvdData;
        case rgItemsToPlot_So.ItemIndex of
          0:
            begin
              ShouldPlot := True;
            end;
          1:
            begin
              ShouldPlot := ParamIndex < seN_so.AsInteger;
            end;
          2:
            begin
              ShouldPlot := ValueList.Count - ParamIndex -1 < seN_so.AsInteger;
            end;
        else Assert(False);
        end;
        if ShouldPlot then
        begin
          for DataIndex := 0 to Length(SvdData.Values)-1 do
          begin
            Bar:= SeriesList.Objects[DataIndex] as TBarSeries;
            if RatioUsed then
            begin
              if SvdData.Total = 0 then
              begin
                Bar.AddBar(0, SvdData.ParameterName,
                  clTeeColor);
              end
              else
              begin
                Bar.AddBar(SvdData.Values[DataIndex]/SvdData.Total, SvdData.ParameterName,
                  clTeeColor);
              end;
            end
            else
            begin
              Bar.AddBar(SvdData.Values[DataIndex], SvdData.ParameterName,
                clTeeColor);
            end;

          end;
        end;
      end;
    end;
  finally
    ValueList.Free;
  end;
end;

procedure TfrmModChart.ReadNdNpr(DmFileName: string; out ND, NPR: integer);
const
  ObsCount = '"NUMBER OBSERVATIONS INCLUDED: "';
  PriorCount = '"NUMBER PRIOR: "';
var
  DmFile: TStringList;
  Index: integer;
  Line: string;
  Position: integer;
begin
  DmFile := TStringList.Create;
  try
    DmFile.LoadFromFile(DmFileName);
    ND := -1;
    for Index := 0 to DmFile.Count -1 do
    begin
      Line := DmFile[Index];
      Position := Pos(ObsCount, Line);
      if Position > 0 then
      begin
        Delete(Line, 1, Position + Length(ObsCount)-1);
        if not TryStrToInt(Trim(Line), ND) then
        begin
          ND := -1;
        end;
        break;
      end;
    end;
    NPR := -1;
    for Index := 0 to DmFile.Count -1 do
    begin
      Line := DmFile[Index];
      Position := Pos(PriorCount, Line);
      if Position > 0 then
      begin
        Delete(Line, 1, Position + Length(PriorCount)-1);
        if not TryStrToInt(Trim(Line), NPR) then
        begin
          NPR := -1;
        end;
        break;
      end;
    end;
  finally
    DmFile.Free;
  end;
end;

procedure TfrmModChart.Read_rc;
var
  ALine : string;
  AValue : TBarValue;
  ValueList : TObjectList;
  Index : integer;
  PlotSymbol : integer;
  DmFileName: string;
  ND, NPR: integer;
  CriticalValue: double;
  ShouldPlot: boolean;
  ColorIndex: integer;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspRc;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := True;
  ValueList := TObjectList.Create;
  try
//    chartModflow.BottomAxis.LabelStyle := talMark;
    chartModflow.BottomAxis.LabelsAngle := 90;
    Readln(FFile, ALine);
    While not EOF(FFile) do
    begin
      Readln(FFile, ALine);
      if Length(ALine)> 0 then
      begin
        PlotSymbol := -1;
        AValue := TBarValue.Create;
        case ModelSelection of
          msModflow:
            begin
              // MODFLOW
              AValue.ParameterName := (Trim(Copy(ALine,18,12)));
              AValue.Value := InternationalStrToFloat(Trim(Copy(ALine,1,15)));
              PlotSymbol := StrToInt(Trim(Copy(ALine,32,9)));
            end;
          msUcode2005:
            begin
              // UCODE-2005
              AValue.ParameterName := (Trim(Copy(ALine,18,27)));
              AValue.Value := InternationalStrToFloat(Trim(Copy(ALine,1,15)));
              PlotSymbol := StrToInt(Trim(Copy(ALine,47,13)));
            end;
        else
          Assert(False);
        end;

        ColorIndex := PlotSymbol-1;
        if ColorIndex < 0 then
        begin
          ColorIndex := ColorIndex + Length(ColorPalette)
        end;
        ColorIndex := ColorIndex mod (Length(ColorPalette));
        if ColorIndex < 0 then
        begin
          ColorIndex := ColorIndex + Length(ColorPalette);
        end;

//        if (PlotSymbol > 0) and (PlotSymbol <= MaxDefaultColors) then
//        begin
          AValue.Color := ColorPalette[ColorIndex];
//        end
//        else
//        begin
//          AValue.Color := clTeeColor;
//        end;

        ValueList.Add(AValue);
      end;
    end;
    if rgRc_PlotOrder.ItemIndex = 0 then
    begin
      ValueList.Sort(SortBarValues);
    end;      
    for Index := 0 to ValueList.Count -1 do
    begin
      case rgItemsToPlot_Rc.ItemIndex of
        0:
          begin
            ShouldPlot := True;
          end;
        1:
          begin
            ShouldPlot := Index < seN_Rc.AsInteger;
          end;
        2:
          begin
            ShouldPlot := ValueList.Count -1 - Index < seN_Rc.AsInteger;
          end;
      else Assert(False);
      end;
      if ShouldPlot then
      begin
        AValue := ValueList[Index] as TBarValue;
        with AValue do
        begin
          serBarChart.Add(Value, ParameterName, Color);
        end;
      end;
    end;
  finally
    ValueList.Free;
  end;
  DmFileName := treecomboFileNames.Selected.Text;
  DmFileName := ChangeFileExt(DmFileName, '._dm');
  if FileExists(DmFileName) then
  begin
    ReadNdNpr(DmFileName, ND, NPR);
    if (NPR >= 0) and (ND >= 0) then
    begin
      ser1to1.Active := True;
      CriticalValue := 4/(ND+NPR);
      ser1to1.AddXY(0, CriticalValue);
      ser1to1.AddXY(serBarChart.Count-1, CriticalValue);
    end;
  end;

end;

function TfrmModChart.ExtractSpaceSeparatedValue(var AString: string): string;
var
  SpacePosition: integer;
begin
  AString := Trim(AString);
  SpacePosition := Pos(' ', AString);
  if SpacePosition > 0 then
  begin
    result := copy(AString, 1, Pred(SpacePosition));
    AString := Copy(AString, Succ(SpacePosition), MAXINT);
  end
  else
  begin
    result := AString;
    AString := '';
  end;

end;

function TfrmModChart.ExtractQuotedText(var AString: string): string;
var
  Quote1, Quote2: integer;
begin
  result := '';
  Quote1 := Pos('"', AString);
  If Quote1 >= 1 then
  begin
    Quote2 := PosEx('"', AString, Succ(Quote1));
    Assert(Quote2 > Quote1);
    result := Copy(AString, Succ(Quote1), Quote2-Quote1-1);
    AString := Copy(AString, Succ(Quote2), MAXINT);
  end;
end;

function CompareSeriesByMaxY(Item1, Item2: Pointer): Integer;
var
  Series1: TLineSeries;
  Series2: TLineSeries;
  Max1, Max2: double;
begin
  Series1 := Item1;
  Series2 := Item2;
  Max1 := Max(Abs(Series1.MaxYValue), Abs(Series1.MinYValue));
  Max2 := Max(Abs(Series2.MaxYValue), Abs(Series2.MinYValue));
  result := Sign(Max2 - Max1);
end;

procedure TfrmModChart.Read_sd(scValueList : TObjectList);
const
  MaxSeriesDisplayed = 15;
var
  ALine : string;
  Position : integer;
  AName, ParameterName : string;
  ASeries, TempSeries : TLineSeries;
  Index: integer;
  ACharacter : Char;
  ObservationName : string[21];
  PlotSymbol : integer;
  AValue : double;
  MyStyle : TSeriesPointerStyle;
  LineType : TPenStyle;
  Color : TColor;
  ColorIndex : integer;
  AList : TObjectList;
  SeriesIndex : integer;
  SortLineSeries : TSdSortLineSeries;
  ValueSet : TSdValues;
  ObservationIndex, ParameterIndex : integer;
  MaxObsNameLength: integer;
  DmFileName: string;
  ND, NPR: integer;
  Count: integer;
  CriticalValue: double;
  StartIndex: integer;
  EndIndex: Integer;
  procedure CreateSeries;
  var
    SIndex: integer;
  begin
    ParameterNames.Add(AName);
    if rgWhatToPlot.ItemIndex = 0 then
    begin
      ASeries := TLineSeries.Create(self);
      ASeries.XValues.Order := loNone;
      ASeries.Title := AName;
      if MyStyle = High(TSeriesPointerStyle) then
      begin
        MyStyle := Low(TSeriesPointerStyle)
      end
      else
      begin
        Inc(MyStyle);
      end;
      while MyStyle in [psSmallDot, psNothing] do
      begin
        Inc(MyStyle);
      end;
      if LineType = High(TPenStyle) then
      begin
        LineType := Low(TPenStyle)
      end
      else
      begin
        Inc(LineType);
      end;
      if LineType = psClear then
      begin
        Inc(LineType);
      end;
      IncrementColorIndex(ColorIndex);
//      if ColorIndex >= Length(ColorPalette) then
//      begin
//        ColorIndex := 0;
//      end
//      else
//      begin
//        Inc(ColorIndex);
//      end;
      Color := ColorPalette[ColorIndex];
      ASeries.pointer.Style := MyStyle;
      ASeries.LinePen.Style := LineType;
      ASeries.SeriesColor := Color;
      ASeries.Pointer.Visible := True;
      if scValueList = nil then
      begin
        chartModflow.AddSeries(ASeries);
      end
      else if IndexOfScParameterName(scValueList,AName) < 0 then
      begin
        for SIndex := 0 to SeriesList.Count -1 do
        begin
          TempSeries := SeriesList.Objects[SIndex] as TLineSeries;
          chartModflow.AddSeries(TempSeries);
        end;
        scValueList := nil;
        chartModflow.AddSeries(ASeries);
        Beep;
        MessageDlg('The .sc file does not contain a parameter named ' + AName
          + ' so the parameters will be plotted in their original order.',
          mtInformation, [mbOK], 0);
      end;
      SeriesList.AddObject(AName, ASeries);
    end;
  end;
  procedure SortSeriesByMaxValue;
  var
    LocalSeries: TList;
    Index: integer;
  begin
    LocalSeries := TList.Create;
    try
      for Index := 0 to SeriesList.Count -1 do
      begin
        LocalSeries.Add(SeriesList.Objects[Index]);
      end;
      LocalSeries.Sort(CompareSeriesByMaxY);
      SeriesList.Clear;
      SeriesList.Capacity := LocalSeries.Count;
      for Index := 0 to LocalSeries.Count -1 do
      begin
        ASeries := LocalSeries[Index];
        SeriesList.AddObject(ASeries.Title, ASeries)
      end;
    finally
      LocalSeries.Free;
    end;
  end;
begin
  FReadingSD := True;
  try
  if FileType in [ft_sppp, ft_spsp, ft_sppr, ft_spsr] then
  begin
    rgWhatToPlot.Items[0] := 'Predictions';
  end
  else
  begin
    Assert(FileType in [ft_sd, ft_s1, ft_rb]);
    rgWhatToPlot.Items[0] := 'Observations';
  end;

  Count := 0;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspDataOrder;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := False;

  // SdValueList stores TSdValues
  if SdValueList = nil then
  begin
    SdValueList := TObjectList.Create;
  end
  else
  begin
    SdValueList.Clear;
  end;

  ExplantionVisible := True;
  chartModflow.BottomAxis.LabelStyle := talText;
  chartModflow.BottomAxis.LabelsAngle := 90;
  ParameterNames.Clear;
  MyStyle := High(TSeriesPointerStyle);
  LineType := High(TPenStyle);
  ColorIndex := MaxDefaultColors;
  if not EOF(FFile) then
  begin
    // Read first line and create series.
    Readln(FFile, ALine);
    Case ModelSelection of
      msModflow:
      begin
        // MODFLOW-2000
        // Skip first;
        Position := 22;
        ALine := Copy(ALine, Position, MAXINT);

        Position := 15;
        While Length(ALine) > 0 do
        begin
          AName := UpperCase(Trim(copy(ALine,1,Position)));
          CreateSeries;
          ALine := Trim(Copy(ALine, Position+1, Length(ALine)));
        end;
      end;
      msUcode2005:
      begin
        // UCODE-2005
        // Skip "OBSERVATION NAME"
        AName := ExtractQuotedText(ALine);
        // Skip "PLOT SYMBOL"
        AName := ExtractQuotedText(ALine);

        AName := UpperCase(Trim(ExtractQuotedText(ALine)));
        while AName <> '' do
        begin
          CreateSeries;
          AName := UpperCase(Trim(ExtractQuotedText(ALine)));
        end;
      end;
    else
      begin
        Assert(False);
      end;
    end;
  end;

  While not EOF(FFile) do
  begin
    MaxObsNameLength := 0;
    case ModelSelection of
      msModflow:
        begin
          MaxObsNameLength := ModflowObservationNameMaxLength;
        end;
      msUcode2005:
        begin
          if FileType = ft_rb then
          begin
            MaxObsNameLength := 17;
          end
          else
          begin
            MaxObsNameLength := UCodeObservationNameMaxLength;
          end;
        end;
    else Assert(False);
    end;

    ObservationName := '                    ';
    for Index := 1 to MaxObsNameLength do
    begin
      if not EOF(FFile) then
      begin
        Read(FFile, ACharacter);
        ObservationName[Index] := ACharacter;
      end;
    end;
    if not EOF(FFile) then
    begin
      ObservationName := Trim(ObservationName) + ' ';
      if rgWhatToPlot.ItemIndex <> 0 then
      begin
        ASeries := TLineSeries.Create(self);
        ASeries.XValues.Order := loNone;
        ASeries.Title := ObservationName;
        if MyStyle = High(TSeriesPointerStyle) then
        begin
          MyStyle := Low(TSeriesPointerStyle)
        end
        else
        begin
          Inc(MyStyle);
        end;
        while MyStyle in [psSmallDot, psNothing] do
        begin
          Inc(MyStyle);
        end;
        if LineType = High(TPenStyle) then
        begin
          LineType := Low(TPenStyle)
        end
        else
        begin
          Inc(LineType);
        end;
        if LineType = psClear then
        begin
          Inc(LineType);
        end;
        IncrementColorIndex(ColorIndex);
//        if ColorIndex >= Length(ColorPalette) then
//        begin
//          ColorIndex := 0;
//        end
//        else
//        begin
//          Inc(ColorIndex);
//        end;
        Color := ColorPalette[ColorIndex];
        ASeries.pointer.Style := MyStyle;
        ASeries.LinePen.Style := LineType;
        ASeries.SeriesColor := Color;
        ASeries.Pointer.Visible := True;
        ASeries.Active := SeriesList.Count < 20;
        SeriesList.AddObject(ObservationName, ASeries);
      end;

      ObservationNames.Add(ObservationName);
      Read(FFile, PlotSymbol);

      if not EOF(FFile) then
      begin
        ValueSet := TSdValues.Create;
        SetLength(ValueSet.Values, ParameterNames.Count);
        ValueSet.ObservationName := ObservationName;
        ValueSet.PlotSymbol := PlotSymbol;
        SdValueList.Add(ValueSet);
        for Index := 0 to ParameterNames.Count -1 do
        begin
          Read(FFile, AValue);
          if not EOF(FFile) then
          begin
            ValueSet.Values[Index] := AValue;
          end;
        end;
        ReadLn(FFile);
      end;
    end;
  end;

  if rgWhatToPlot.ItemIndex = 0 then
  begin
    // plot observations
    if rgPointOrder.itemIndex = 1 then
    begin
      SdValueList.Sort(SortSdValues);
    end;

    case rgItemsToPlot.ItemIndex of
      0:
        begin
          StartIndex := 0;
          EndIndex := SdValueList.Count -1;
        end;
      1:
        begin
          StartIndex := 0;
          EndIndex := spN.AsInteger-1;
        end;
      2:
        begin
          StartIndex := spN.AsInteger-1;
          EndIndex := SdValueList.Count -1;
        end;
    else
      Assert(False)
    end;


    for ObservationIndex := StartIndex to EndIndex do
    begin
      ValueSet := SdValueList[ObservationIndex] as TSdValues;
      ObservationName := ValueSet.ObservationName;
      for ParameterIndex := 0 to ParameterNames.Count -1 do
      begin
        ASeries := SeriesList.Objects[ParameterIndex] as TLineSeries;
        ASeries.AddY(ValueSet.Values[ParameterIndex], ObservationName, clTeeColor);
        if Count < ASeries.Count then
        begin
          Count := ASeries.Count;
        end;

      end;
    end;
  end
  else
  begin
    // plot parameters
    for ObservationIndex := 0 to SdValueList.Count -1 do
    begin
      ValueSet := SdValueList[ObservationIndex] as TSdValues;
      ASeries := SeriesList.Objects[ObservationIndex] as TLineSeries;
      for ParameterIndex := 0 to ParameterNames.Count -1 do
      begin
        ParameterName := ParameterNames[ParameterIndex];
        ASeries.AddY(ValueSet.Values[ParameterIndex], ParameterName, clTeeColor);
        if Count < ASeries.Count then
        begin
          Count := ASeries.Count;
        end;
      end;
    end;
    SeriesList.Sort;
    for ObservationIndex := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[ObservationIndex] as TLineSeries;
      ASeries.Active := ObservationIndex < 20;
      chartModflow.AddSeries(ASeries);
    end;
  end;


  if scValueList <> nil then
  begin
    AList := TObjectList.Create;
    try
      for SeriesIndex := 0 to SeriesList.Count -1 do
      begin
        SortLineSeries := TSdSortLineSeries.Create;
        SortLineSeries.ParameterName := SeriesList[SeriesIndex];
        SortLineSeries.Series := SeriesList.Objects[SeriesIndex] as TLineSeries;
        AList.Add(SortLineSeries);
      end;
      scValueObjectList := scValueList;
      AList.Sort(SortSeriesList);
      SeriesList.Clear;
      MyStyle := High(TSeriesPointerStyle);
      LineType := High(TPenStyle);
      ColorIndex := MaxDefaultColors;
      for SeriesIndex := 0 to AList.Count -1 do
      begin
        SortLineSeries := AList[SeriesIndex] as TSdSortLineSeries;
        SeriesList.AddObject(SortLineSeries.ParameterName, SortLineSeries.Series);
        SortLineSeries.Series.Active := SeriesIndex < MaxSeriesDisplayed;

        if MyStyle = High(TSeriesPointerStyle) then
        begin
          MyStyle := Low(TSeriesPointerStyle)
        end
        else
        begin
          Inc(MyStyle);
        end;
        while MyStyle in [psSmallDot, psNothing] do
        begin
          Inc(MyStyle);
        end;
        if LineType = High(TPenStyle) then
        begin
          LineType := Low(TPenStyle)
        end
        else
        begin
          Inc(LineType);
        end;
        if LineType = psClear then
        begin
          Inc(LineType);
        end;
        IncrementColorIndex(ColorIndex);
//        if ColorIndex >= Length(ColorPalette) then
//        begin
//          ColorIndex := 0;
//        end
//        else
//        begin
//          Inc(ColorIndex);
//        end;
        Color := ColorPalette[ColorIndex];
        SortLineSeries.Series.pointer.Style := MyStyle;
        SortLineSeries.Series.LinePen.Style := LineType;
        SortLineSeries.Series.SeriesColor := Color;

        chartModflow.AddSeries(SortLineSeries.Series);
      end;
//      lblSeriesMax.Visible := AList.Count >= MaxSeriesDisplayed;

    finally
      AList.Free;
    end;
  end;
  case rgItemsToPlot.ItemIndex of
    0:
      begin
        chartModflow.BottomAxis.Automatic := True;
      end;
    1:
      begin
        chartModflow.BottomAxis.AutomaticMinimum := True;
        chartModflow.BottomAxis.AutomaticMaximum := False;
        chartModflow.BottomAxis.Maximum := spN.AsInteger -1;
      end;
    2:
      begin
        chartModflow.BottomAxis.AutomaticMinimum := False;
        chartModflow.BottomAxis.AutomaticMaximum := True;
        chartModflow.BottomAxis.Minimum :=
          chartModflow.BottomAxis.Maximum - spN.AsInteger +1;
      end;
  else Assert(False);
  end;

  if LowerCase(ExtractFileExt(treecomboFileNames.Selected.Text)) = '._rb' then
  begin
    DmFileName := treecomboFileNames.Selected.Text;
    DmFileName := ChangeFileExt(DmFileName, '._dm');
    if FileExists(DmFileName) then
    begin
      ReadNdNpr(DmFileName, ND, NPR);
      if (NPR >= 0) and (ND >= 0) then
      begin
        CriticalValue := 2/Sqrt(ND+NPR);

        ASeries := TLineSeries.Create(self);
        ExtraSeriesList.Add(ASeries);
        ASeries.XValues.Order := loNone;
        ASeries.LinePen.Style := psSolid;
//        ASeries.Color := clBlack;
        ASeries.Active := True;
        ASeries.AddXY(0, CriticalValue);
        ASeries.AddXY(Count-1, CriticalValue);
        ASeries.Name := 'UpperCriticalValue';
        ASeries.Title := 'Critical Value';
        chartModflow.AddSeries(ASeries);

        ASeries := TLineSeries.Create(self);
        ExtraSeriesList.Add(ASeries);
        ASeries.XValues.Order := loNone;
        ASeries.LinePen.Style := psSolid;
//        ASeries.Color := clBlack;
        ASeries.Active := True;
        ASeries.AddXY(0, -CriticalValue);
        ASeries.AddXY(Count-1, -CriticalValue);
        ASeries.Name := 'LowerCriticalValue';
        ASeries.Title := 'Critical Value';
        chartModflow.AddSeries(ASeries);
      end;
    end;
  end;

  // Determine which plots to make visible.
  if SeriesList.Count <> clbSeriesToPlot.Items.Count then
  begin
    clbSeriesToPlot.Items.Clear;
    rgSeriesToPlot.ItemIndex := 1;
  end
  else
  begin
    for Index := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[Index] as TLineSeries;
      if clbSeriesToPlot.Items[Index] <> ASeries.Title then
      begin
        clbSeriesToPlot.Items.Clear;
        rgSeriesToPlot.ItemIndex := 1;
        break;
      end
      else
      begin
        clbSeriesToPlot.Items.Objects[Index] := ASeries;
      end;
    end;
  end;
  if clbSeriesToPlot.Items.Count = 0 then
  begin
    clbSeriesToPlot.Items.Assign(SeriesList);
  end;

  case rgSeriesToPlot.ItemIndex of
    0:
      begin
        for Index := 0 to SeriesList.Count -1 do
        begin
          ASeries := SeriesList.Objects[Index] as TLineSeries;
          ASeries.Active := True;
          clbSeriesToPlot.State[Index] := cbChecked;
        end;
      end;
    1:
      begin
        SortSeriesByMaxValue;
        for Index := 0 to SeriesList.Count -1 do
        begin
          ASeries := SeriesList.Objects[Index] as TLineSeries;
          ASeries.Active := Index < seNumberOfSeries.AsInteger;
          Position := clbSeriesToPlot.Items.IndexOfObject(ASeries);
          clbSeriesToPlot.Checked[Position] := ASeries.Active;
        end;
      end;
    2:
      begin
        for Index := 0 to clbSeriesToPlot.Items.Count -1 do
        begin
          ASeries := clbSeriesToPlot.Items.Objects[Index] as TLineSeries;
          ASeries.Active := clbSeriesToPlot.Checked[Index];
        end;
      end;
  else Assert(False);
  end;

  finally
    FReadingSD := False;
  end;

end;

procedure TfrmModChart.Read_rd_rg;
var
  PlotPostion, Value : double;
  Color : TColor;
  ALine : string;
  ASeries : TPointSeries;
  MyStyle : TSeriesPointerStyle;
  Lines : TStringList;
  Nm_FileName: string;
  SeriesIndex: integer;
  PlotSymbol: integer;
  ObsName: string;
  StyleList: TList;
  StyleIndex: integer;
  StyleType: TStyleType;
  NameList: TIntegerList;
  PlotSymbolIndex: integer;
  ObsNameIndex: integer;
  ValueIndex: integer;
  PlotPostionIndex: integer;
begin
  PlotSymbolIndex := -1;
  ObsNameIndex := -1;
  ValueIndex := -1;
  PlotPostionIndex := -1;
  Nm_FileName := GetCurrentDir + '\' + treecomboFileNames.Selected.Text;
  Nm_FileName := ChangeFileExt(Nm_FileName, '._nm');
  cbPlot_NmFile.Enabled := FileExists(Nm_FileName);
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_rdrg;
  cbLabelPoints.Enabled := True;

  clbSeriesList.Items.Clear;
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := False;
  chartModflow.BottomAxis.LabelStyle := talValue;
  chartModflow.BottomAxis.LabelsAngle := 0;
  ASeries := TPointSeries.Create(self);
  ASeries.XValues.Order := loNone;
  ASeries.ShowInLegend := False;
  chartModflow.AddSeries(ASeries);
  SeriesList.AddObject('',ASeries);
  clbSeriesList.Items.AddObject('Series 1', ASeries);
  clbSeriesList.Checked[0] := True;

  StyleList := TObjectList.Create;
  RdRgStyleList.Add(StyleList);

  NameList := TIntegerList.Create;
  rdRgNameList.Add(NameList);

  ASeries.OnGetPointerStyle := MultipleSeriesGetPointerStyle;
  ASeries.OnClickPointer := MultipleSeriesClickPointer;
  ASeries.OnGetMarkText := MultipleSeriesGetMarkText;
  ASeries.Marks.Visible := True;
  ASeries.Marks.ArrowLength := 8;

  Color := clTeeColor;
  MyStyle := High(TSeriesPointerStyle);

  SeriesIndex := 1;
  While not EOF(FFile) do
  begin
    Readln(FFile, ALine);
    If Pos('ORDERED',ALine) < 1 then
    begin
      if Trim(ALine) = '' then
      begin
        if not EOF(FFile) then
        begin
          ASeries := TPointSeries.Create(self);
          ASeries.XValues.Order := loNone;
          ASeries.ShowInLegend := False;
          if MyStyle = High(TSeriesPointerStyle) then
          begin
            MyStyle := Low(TSeriesPointerStyle)
          end
          else
          begin
            Inc(MyStyle);
          end;
          while MyStyle in [psSmallDot, psNothing] do
          begin
            Inc(MyStyle);
          end;
          ASeries.pointer.Style := MyStyle;
          chartModflow.AddSeries(ASeries);
          SeriesList.AddObject('',ASeries);
          Inc(SeriesIndex);
          clbSeriesList.Items.AddObject('Series ' + IntToStr(SeriesIndex), ASeries);
          clbSeriesList.Checked[SeriesIndex-1] := True;

          StyleList := TObjectList.Create;
          RdRgStyleList.Add(StyleList);
          ASeries.OnGetPointerStyle := MultipleSeriesGetPointerStyle;
          ASeries.OnClickPointer := MultipleSeriesClickPointer;
          ASeries.OnGetMarkText := MultipleSeriesGetMarkText;

          NameList := TIntegerList.Create;
          rdRgNameList.Add(NameList);
          ASeries.Marks.Visible := True;
          ASeries.Marks.ArrowLength := 8;
        end;
      end
      else
      begin
        Lines := TStringList.Create;
        try
          ExtractSpaceSeparatedStrings(ALine, Lines);
          Assert(Lines.Count >= 4);
          Value := InternationalStrToFloat(Lines[ValueIndex]);
          PlotPostion := InternationalStrToFloat(Lines[PlotPostionIndex]);

          if ObsNameIndex >= 0 then
          begin
            ObsName := Lines[ObsNameIndex];
          end
          else
          begin
            ObsName := '';
          end;

          if PlotSymbolIndex >= 0 then
          begin
            PlotSymbol := StrToInt(Lines[PlotSymbolIndex]);
          end
          else
          begin
            PlotSymbol := 1;
          end;

          StyleIndex := (PlotSymbol -1) mod	(Ord(High(TSeriesPointerStyle))-3);

          StyleType := TStyleType.Create;
          StyleList.Add(StyleType);
          StyleType.Style := Low(TSeriesPointerStyle);
          if StyleIndex > 0 then
          begin
            Inc(StyleType.Style,StyleIndex);
          end;
          if StyleType.Style >= psSmallDot then
          begin
            Inc(StyleType.Style,2);
          end;

        finally
          Lines.Free;
        end;

        if not EOF(FFile) then
        begin
          ASeries.AddXY(Value, PlotPostion, ObsName, Color);
        end;
      end;
    end
    else
    begin
      Lines := TStringList.Create;
      try
        ExtractQuotedStrings(ALine, Lines);
        Assert(Lines.Count >= 2);
        chartModflow.BottomAxis.Title.Caption := UpperCase(Copy(Lines[0], 2, Length(Lines[0])-2));
        chartModflow.LeftAxis.Title.Caption := UpperCase(Copy(Lines[1], 2, Length(Lines[1])-2));
        PlotSymbolIndex := Lines.IndexOf('"PLOT SYMBOL"');
        if PlotSymbolIndex < 0 then
        begin
        PlotSymbolIndex := Lines.IndexOf('"PLOT-SYMBOL"');
        end;
        ObsNameIndex := Lines.IndexOf('"OBSERVATION or PRIOR NAME"');
        if ObsNameIndex < 0 then
        begin
          ObsNameIndex := Lines.IndexOf('"OBSERVATION"');
        end;

        ValueIndex := Lines.IndexOf('"ORDERED CORRELATED DEVIATE"');
        if ValueIndex < 0 then
        begin
          ValueIndex := Lines.IndexOf('"ORDERED CORRELATED NUMBER"');
        end;
        if ValueIndex < 0 then
        begin
          ValueIndex := Lines.IndexOf('"ORDERED INDEPENDENT DEVIATE"');
        end;
        if ValueIndex < 0 then
        begin
          ValueIndex := Lines.IndexOf('"ORDERED INDEPENDENT RANDOM NUMBER"');
        end;
        Assert(ValueIndex >= 0);

        PlotPostionIndex := Lines.IndexOf('"STANDARD NORMAL STATISTIC"');
        if PlotPostionIndex < 0 then
        begin
          PlotPostionIndex := Lines.IndexOf('"PROBABILITY PLOTTING POSITION"');
        end;
        Assert(PlotPostionIndex >= 0);

      finally
        Lines.Free;
      end;

    end;
  end;
end;

procedure TfrmModChart.SetAxisLabels(Extension : string);
var
  XYTR_FileExists: boolean;
  PercentString: string;
begin
  IsRecentFile := False;
//  PlotVsTime := False;
  chartModflow.RightAxis.Title.Caption := '';
  if (Extension = '._os') then
  begin
    IsRecentFile := (ModelSelection = msModflow) and cbRecentModflow.Checked;
    XYTR_FileExists := (ModelSelection = msUcode2005)
      and FileExists(ChangeFileExt(NameOfFile, '._xyztwr'));

//    PlotVsTime := (IsRecentFile and (rgPlotObservedVs.ItemIndex = 1))
//      or (XYTR_FileExists and (rgPlotObservedVs.ItemIndex = 1));
//
//    if PlotVsTime then
//    begin
//      chartModflow.BottomAxis.Title.Caption := UpperCase('Time');
//      chartModflow.LeftAxis.Title.Caption := UpperCase('SIM. EQUIV. minus Obs.');
//    end
//    else
//    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('SIMULATED EQUIVALENT');
      chartModflow.LeftAxis.Title.Caption := UpperCase('OBSERVATION');
//    end;
  end
  else if (Extension = '._ww') then
  begin
    jvplChartControls.Visible := True;
    jvplChartControls.ActivePage := jvsp_os;
    cbRecentModflow.Enabled := (ModelSelection = msModflow);
    IsRecentFile := (ModelSelection = msModflow) and cbRecentModflow.Checked;
    XYTR_FileExists := (ModelSelection = msUcode2005)
      and FileExists(ChangeFileExt(NameOfFile, '._xyztwr'));

//    rgPlotObservedVs.Enabled := False;
//    PlotVsTime := False;
    chartModflow.BottomAxis.Title.Caption := UpperCase('WEIGHTED SIMULATED EQUIVALENT');
    chartModflow.LeftAxis.Title.Caption := UpperCase('WEIGHTED OBSERVATION');
  end
  else if (Extension = '._ws') then
  begin
    jvplChartControls.Visible := True;
    jvplChartControls.ActivePage := jvsp_os;
    chartModflow.LeftAxis.Title.Caption := UpperCase('Weighted residual');
    XYTR_FileExists := (ModelSelection = msUcode2005)
      and FileExists(ChangeFileExt(NameOfFile, '._xyztwr'));
//    PlotVsTime := XYTR_FileExists and (rgPlotObservedVs.ItemIndex = 1);
//    if PlotVsTime then
//    begin
//      chartModflow.BottomAxis.Title.Caption := UpperCase('Time');
//    end
//    else
//    begin
      case ModelSelection of
        msModflow: chartModflow.BottomAxis.Title.Caption := UpperCase('WEIGHTED SIMULATED EQUIVALENT');
        msUcode2005: chartModflow.BottomAxis.Title.Caption := UpperCase('Unweighted simulated values');
      else Assert(False);
      end;
//    end;
  end
  else if (Extension = '._nm') then
  begin
//    chartModflow.LeftAxis.Title.Caption := UpperCase('Standard normally distributed number');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Standard normal number');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Weighted residual');
  end
  else if (Extension = '._rd') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Independent normal number');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Weighted residual');
  end
  else if (Extension = '._rg') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Correlated normal number');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Weighted residual');
  end
  else if (Extension = '._sc') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('PARAMETER NAME');
    if cbRatio.Checked then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Ratio to largest css');
    end
    else
    begin
//      chartModflow.LeftAxis.Title.Caption := UpperCase('Composite scaled sensitivity');
      chartModflow.LeftAxis.Title.Caption := 'PARAMETER IMPORTANCE TO OBSERVATIONS,'#13'BASED ON CSS';
    end;

  end
  else if (Extension = '._sd') then
  begin
    if rgWhatToPlot.ItemIndex = 0 then
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Observation name');
    end
    else
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter name');
    end;
//    chartModflow.LeftAxis.Title.Caption := UpperCase('Dimensionless scaled sensitivity');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Observation Importance,'#13#10'Based on DSS');
  end
  else if (Extension = '._s1') then
  begin
    if rgWhatToPlot.ItemIndex = 0 then
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Observation name');
    end
    else
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter name');
    end;
    chartModflow.LeftAxis.Title.Caption := UpperCase('One-percent scaled sensitivity');
  end
  else if (Extension = '._rb') then
  begin
    if rgWhatToPlot.ItemIndex = 0 then
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Observation name');
    end
    else
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter name');
    end;
    chartModflow.LeftAxis.Title.Caption := UpperCase('DFBetas statistics');
  end
  else if (Extension = '._rc') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('COOK''S D');
  end
  else if (Extension = '._b') or (Extension = '._pa') then
  begin
    jvplChartControls.Visible := True;
    jvplChartControls.ActivePage := jvsp_b_pa;
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Estimation Iteration');
    DivideByInitialValues := cbDivideParameterValues.Checked;
    if DivideByInitialValues then
    begin
//      chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter Value/Initial Parameter Value');
      chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter (Estimate/Starting)');
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter Value');
    end;
    if (Extension = '._b') then
    begin
      cbShowAllParameters.Enabled := True;
      ShowAllParameters := cbShowAllParameters.Checked;
    end
    else
    begin
      cbShowAllParameters.Enabled := False;
    end;
  end
  else if (Extension = '._pc') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Name');
    if rb_b_sd_s.Checked then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Fit independant parameter importance,'#13'based on') + ' (b/SD)*s';
    end
    else if rbInverseCoef.Checked then
    begin
//      chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter value divided by '#13'standard deviation') + ' (b/SD)';
      chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter importance, based on') + ' b/SD';
    end
    else if cbScaleValues.Checked then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Value / Estimated Parameter Value');
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Value');
    end;

  end
  else if (Extension = '._ss') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Iteration');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Sum of Squared Weighted Residuals');
    chartModflow.RightAxis.Title.Caption := UpperCase('Number of Observations');
  end
  else if (Extension = '._rdadv') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Weighted Residual');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Probability Plot Position');
  end
  else if (Extension = '._so') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation or Prior Name');
//    chartModflow.LeftAxis.Title.Caption := UpperCase('Leverage');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Observation Dominance,'#13#10'Based on Leverage');
  end
  else if (Extension = '._ppr') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('PPR Statistic');
  end
  else if (Extension = '._opr') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('OPR Statistic');
  end
  else if (Extension = '._ppr_abschg') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Change in Pred Std Dev');
  end
  else if (Extension = '._ppa') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('PPA Statistic');
  end
  else if (Extension = '._ppa_abschg') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Change in Param Std Dev');
  end
  else if (Extension = '._opr_abschg') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Change in Pred Std Dev');
  end
  else if (Extension = '._opa') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('OPA Statistic');
  end
  else if (Extension = '._opa_abschg') then
  begin
    chartModflow.BottomAxis.Title.Caption := UpperCase('Observation Name');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Change in Param Std Dev');
  end
  else if (Extension = '._sppp') or (Extension = '._spsp')
    or (Extension = '._sppr') or (Extension = '._spsr') then
  begin
    if rgWhatToPlot.ItemIndex = 0 then
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction name');
    end
    else
    begin
      chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter name');
    end;
    chartModflow.LeftAxis.Title.Caption := UpperCase('Scaled sensitivity');
  end
  else if (Extension = '._pcc') then
  begin
//    chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter Correlation Coefficient');
    chartModflow.LeftAxis.Title.Caption := UpperCase('Parameter Interdependence,'#13#10' Based on PCC');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Pair');
  end
  else if (Extension = '._linp') then
  begin
    if cbScale_linp.Checked then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('95% Intervals');
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('95% Intervals Scaled by Predicted Values');
    end;

    chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction');
  end
  else if (Extension = '._intconf') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Nonlinear Confidence Intervals');
    case rgIntConfWhatToPlot.ItemIndex of
      0:
        begin
          chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter');
        end;
      1:
        begin
          chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction');
        end;
      2:
        begin
          chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction or Parameter');
        end;
    else Assert(False);
    end;

    chartModflow.RightAxis.Title.Caption := UpperCase('Deviation from Goal');
  end
  else if (Extension = '._linpred') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Nonlinear Prediction Intervals');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction or Parameter');
    chartModflow.RightAxis.Title.Caption := UpperCase('Deviation from Goal');
  end
  else if (Extension = '._intpred') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Nonlinear Confidence Intervals');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction or Parameter');
    chartModflow.RightAxis.Title.Caption := UpperCase('Deviation from Goal');
  end
  else if (Extension = '._xyztwr') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase(rgYAxis.Items[rgYAxis.ItemIndex]);
    chartModflow.BottomAxis.Title.Caption := UpperCase(rgXAxis.Items[rgXAxis.ItemIndex]);
  end
  else if (Extension = '._sc_svd') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Composite Scaled Sensitivity'#13
      + 'Singular Value Decomposition');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Names');
  end
  else if (Extension = '._scgrp') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('PARAMETER IMPORTANCE TO'#13
      + 'OBSERVATIONS, BASED ON CSS');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Parameter Names');
  end
  else if (Extension = '._mcmc_grr') then
  begin
    chartModflow.LeftAxis.Title.Caption := UpperCase('Gelman-Rubin R');
    chartModflow.BottomAxis.Title.Caption := UpperCase('Iteration');
  end
  else if (Extension = '._svd') then
  begin
    if rgWhatToPlot_svd.ItemIndex = 0 then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Value Relative to Largest');
      chartModflow.BottomAxis.Title.Caption := UpperCase('Singular Value');
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Process model parameter contribution'#13'to each SVD parameter');
      chartModflow.BottomAxis.Title.Caption := UpperCase('Singular vector');
    end;

  end
  else if Pos('._mcmc_pred', LowerCase(Extension)) = 1 then
  begin
    if rgMcmc_predGraphType.ItemIndex = 0 then
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Posterior Density');
      chartModflow.BottomAxis.Title.Caption := '';
    end
    else
    begin
      if rgConfidenceInterval.ItemIndex <> 3 then
      begin
        PercentString :=
          rgConfidenceInterval.Items[rgConfidenceInterval.ItemIndex];
      end
      else
      begin
        PercentString := rdeConfidenceInterval.Text + '%';
      end;

      chartModflow.LeftAxis.Title.Caption :=
        UpperCase(Format('%s Bayesian intervals', [PercentString]));
      chartModflow.BottomAxis.Title.Caption := UpperCase('Prediction');
    end;
  end
  else if Pos('._mcmc', LowerCase(Extension)) = 1 then
  begin
    if rgMcmcGraphType.ItemIndex = 0 then
    begin
      chartModflow.LeftAxis.Title.Caption := '';
      chartModflow.BottomAxis.Title.Caption := UpperCase('Iteration');
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := UpperCase('Posterior Density');
      chartModflow.BottomAxis.Title.Caption := '';
    end;
  end
  else
  begin
    Assert(False)
  end;
end;

procedure TfrmModChart.miExit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmModChart.miAbout1Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmModChart.miPrintSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfrmModChart.miPrintClick(Sender: TObject);
var
  Colors : array of TColor;
  Index : integer;
  ASeries : TChartSeries;
  PrintInBW : boolean;
begin
  PrintInBW := False;
  if SeriesList.Count > 0 then
  begin
    If MessageDlg('Do you wish to print in Black and White?', mtInformation,
      [mbYes, mbNo], 0) = mrYes then
      begin
        PrintInBW := True;
        SetLength(Colors, SeriesList.Count);
        for Index := 0 to SeriesList.Count -1 do
        begin
          ASeries := SeriesList.Objects[Index] as TLineSeries;
          Colors[Index] := ASeries.SeriesColor;
          ASeries.SeriesColor := clBlack;
        end;
      end;
  end;
  ChartModflow.print;
  if PrintInBW then
  begin
    for Index := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[Index] as TLineSeries;
      ASeries.SeriesColor := Colors[Index];
    end;
  end;
end;

procedure TfrmModChart.cbLabelPointsClick(Sender: TObject);
var
  SeriesIndex: integer;
  ASeries: TPointSeries;
begin
  if Filetype in [ft_os, ft_ww, ft_ws] then
  begin
    for SeriesIndex := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[SeriesIndex] as TPointSeries;
      ASeries.Marks.Visible := cbLabelPoints.Checked;
    end;

  end
  else
  begin
    SerDataPoints.Marks.Visible := cbLabelPoints.Checked;
    HorizRangeSeries.Marks.Visible := cbLabelPoints.Checked;
  end;
  if cbLabelPoints.Checked then
  begin
    if chartModflow.ShowHint then
    begin
      chartModflow.Hint := 'Click on a data point to see its name';
    end
    else
    begin
      chartModflow.Hint := '';
    end;
  end
  else
  begin
    if chartModflow.ShowHint then
    begin
      chartModflow.Hint :=
        'Click on a data point to see its name on the status bar';
    end
    else
    begin
      chartModflow.Hint := '';
    end;
    MarkedPoints.Clear;
    FMarkedPointsList.Clear;
    StatusBar1.Panels[0].Text := '';
  end;
end;

procedure TfrmModChart.sbFormatClick(Sender: TObject);
begin
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
{  frmFormat.GetData(chartModflow, 20);
  frmFormat.ShowModal;  }
end;

procedure TfrmModChart.sbSaveImageClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.bmp' then
    begin
      chartModflow.SaveToBitmapFile(SaveDialog1.FileName);
    end
    else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.emf' then
    begin
      chartModflow.SaveToMetafileEnh(SaveDialog1.FileName);
    end
    else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.wmf' then
    begin
      chartModflow.SaveToMetafile(SaveDialog1.FileName);
    end;
  end;

end;

function TfrmModChart.SerDataPointsGetPointerStyle(Sender: TChartSeries;
  ValueIndex: Integer): TSeriesPointerStyle;
var
  StyleType : TStyleType;
begin
  if HorizRangeSeries.Active and (Sender = SerDataPoints) then
  begin
    result := psDiamond;
  end
  else
  begin
    if ValueIndex > ObservationNames.Count -1 then
    begin
      result := Low(TSeriesPointerStyle);
    end
    else
    begin
      StyleType := ObservationNames.Objects[ValueIndex] as TStyleType;
      result := StyleType.Style;
    end;
  end;
end;

procedure TfrmModChart.MultipleSeriesGetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
var
  //Location : integer;
  LocalPoints: TIntegerList;
  Position: integer;
begin
  Position := SeriesList.IndexOfObject(Sender);
  if Position >= 0 then
  begin
    LocalPoints := rdRgNameList[Position];
    if (LocalPoints.IndexOf(ValueIndex) < 0)
      or not cbLabelPoints.Checked
      or (HorizRangeSeries.Active and (Sender = SerDataPoints)) then
    begin
      MarkText := '';
    end;
  end;
end;


procedure TfrmModChart.SerDataPointsGetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
var
  MarkList : TIntegerList;
  Position : integer;
begin
  Position := SeriesList.IndexOfObject(Sender);
  if (FMarkedPointsList.Count = 0) or (Position < 0) then
  begin
    MarkList := MarkedPoints;
  end
  else
  begin
    MarkList := FMarkedPointsList[Position];
  end;

  if (MarkList.IndexOf(ValueIndex) < 0)
    or not cbLabelPoints.Checked
    or (HorizRangeSeries.Active and (Sender = SerDataPoints)) then
  begin
    MarkText := '';
  end;
end;

procedure TfrmModChart.chartModflowGetLegendRect(Sender: TCustomChart;
  var Rect: TRect);
var
  ExplanationWidth : integer;
  ExplanationHeight : integer;
  TempFont : TFont;
begin
  LegendRect := Rect;
  ExplanationTop := Rect.Top + 5;
  ExplanationLeft := Rect.Left + 5;
  TempFont := TFont.Create;
  try
    TempFont.Assign(chartModflow.Canvas.Font);
    try
      chartModflow.Canvas.Font.Assign(chartModflow.Legend.Font);
      ExplanationWidth := chartModflow.Canvas.TextWidth(StrExplanationUnderscore);
      ExplanationHeight := chartModflow.Canvas.TextHeight(StrExplanation);
    finally
      chartModflow.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
//  FXOffset := Rect.Left;
  if Rect.Right - Rect.Left < ExplanationWidth then
  begin
    Rect.Right := Rect.Left + ExplanationWidth;
  end;
//  if Rect.Right > chartModflow.Width - 20 then
//  begin
//    Rect.Right := chartModflow.Width - 20;
//    if Rect.Right - Rect.Left < ExplanationWidth then
//    begin
//      Rect.Left := Rect.Right - ExplanationWidth;
//    end;
//  end;
//  FXOffset := Rect.Left - FXOffset ;

  Rect.Bottom := Rect.Bottom + ExplanationHeight;
  if Rect.Right + 20 > ChartModflow.Width then
  begin
    DeltaLegendX := Rect.Right - ChartModflow.Width + 20;
    Rect.Left := Rect.Left - DeltaLegendX;
    Rect.Right := Rect.Right - DeltaLegendX;
    ExplanationLeft := ExplanationLeft - DeltaLegendX;
  end
  else
  begin
    DeltaLegendX := 0
  end;
end;

procedure TfrmModChart.chartModflowGetLegendPos(Sender: TCustomChart;
  Index: Integer; var X, Y, XColor: Integer);
var
  ExplanationHeight : integer;
  TempFont : TFont;
  ASize: TSize;
  SymbolSize: integer;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(chartModflow.Canvas.Font);
    try
      chartModflow.Canvas.Font.Assign(chartModflow.Legend.Font);
      ASize.cy := chartModflow.Canvas.TextHeight(StrExplanation);
      ASize.cx := chartModflow.Canvas.TextWidth(StrExplanation);
      ExplanationHeight := ASize.cy;
    finally
      chartModflow.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  Y := Y + ExplanationHeight;
  if (FileType = ft_xyztwr) and rgDataToPlot.Enabled then
  begin
    chartModflowGetSymbolSize(Sender, Index, SymbolSize);
    Y := Y + SymbolSize;
    X := X - DeltaLegendX;
    XColor := XColor - DeltaLegendX;
  end;
//  X := X + FXOffset;
//  if X + ASize.cx + 20 > chartModflow.Width then
//  begin
//    X := chartModflow.Width - ASize.cx - 20
//  end;

end;

procedure TfrmModChart.chartModflowIntPredGetLegendPos(Sender: TCustomChart;
  Index: Integer; var X, Y, XColor: Integer);
begin
  chartModflowGetLegendPos(Sender, Index, X, Y, XColor);
  if Index = 1 then
  begin
    UpperDevX := X;
    UpperDevY := Y;
    UpperDevXColor := XColor;
  end
  else if Index = 2 then
  begin
    if UpperDevY <> Y then
    begin
      X := UpperDevX;
      XColor := UpperDevXColor;
    end;
  end;
end;


procedure TfrmModChart.chartModflowAfterDraw(Sender: TObject);
var
  TempFont : TFont;
begin
  if ExplantionVisible and chartModflow.Legend.Visible then
  begin

    TempFont := TFont.Create;
    try
      TempFont.Assign(chartModflow.Canvas.Font);
      try
        chartModflow.Canvas.Font.Assign(chartModflow.Legend.Font);
        chartModflow.Canvas.TextOut(ExplanationLeft,ExplanationTop,
          StrExplanation);
        
      finally
        chartModflow.Canvas.Font.Assign(TempFont);
      end;

    finally
      TempFont.Free;
    end;

  end;
end;

procedure TfrmModChart.FormDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  ToolBar1.Align := alTop;
  miDockToolbar.Caption := '&Undock Toolbar';
end;

procedure TfrmModChart.Panel1DockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := (Source.Control is TToolBar);
  if Accept then
  begin
    //Modify the DockRect to preview dock area (Coolbar client area)
    ARect.TopLeft := ClientToScreen(ClientRect.TopLeft);
    ARect.BottomRight := ClientToScreen(ClientRect.BottomRight);
    ARect.Bottom := ARect.Top + ToolBar1.Height;
    Source.DockRect := ARect;
  end;
end;

procedure TfrmModChart.miDockToolbarClick(Sender: TObject);
var
  ARect: TRect;
begin
  ToolBar1.Visible := True;
  if miDockToolbar.Caption = '&Undock Toolbar' then
  begin
    ARect.TopLeft := ClientToScreen(ClientRect.TopLeft);
    ARect.BottomRight := ClientToScreen(ClientRect.BottomRight);
    ARect.Bottom := ARect.Top + ToolBar1.Height;
    ToolBar1.ManualFloat(ARect);
    miDockToolbar.Caption := '&Dock Toolbar';
  end
  else
  begin
    ToolBar1.ManualDock(frmModChart, nil, alTop);
    miDockToolbar.Caption := '&Undock Toolbar';
  end;
  MainMenu1.Merge(mainMenuFormChoice);
end;

procedure TfrmModChart.ToolBar1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TfrmModChart.BitBtnCloseClick(Sender: TObject);
begin
  if miDockToolbar.Caption <> '&Undock Toolbar' then
  begin
    miDockToolbarClick(Sender);
  end
  else
  begin
    Close;
  end;
end;

procedure TfrmModChart.FormChoiceClick(Sender: TObject);
var
  AList : TList;
  AForm : TForm;
  Index : integer;
begin
  if miDockToolbar.Caption <> '&Undock Toolbar' then
  begin
    miDockToolbarClick(Sender);
  end;
  AList := TList.Create;
  try
    AList.Add(frmModChart);
    AList.Add(frmZoneBdgtReader);
    AList.Add(frmLakePlot);
    AList.Add(frmExtract);
    AList.Add(frmPiperGraph);
    AList.Add(frmCellFlows);
    AList.Add(frmConvertFlows);
    AList.Add(frmFarm);
    AForm := nil;
    if Sender = WaterBudgets1 then
    begin
      AForm := frmZoneBdgtReader;
    end
    else if Sender = Hydrographs1 then
    begin
      AForm := frmExtract;
    end
    else if Sender = CalibrationPlots1 then
    begin
      AForm := frmModChart;
    end
    else if Sender = LakePlots1 then
    begin
      AForm := frmLakePlot;
    end
    else if Sender = PiperDiagram1 then
    begin
      AForm := frmPiperGraph;
    end
    else if Sender = CellWaterBudgets1 then
    begin
      AForm := frmCellFlows;
    end
    else if Sender = ConvertCellWaterBudgets1 then
    begin
      AForm := frmConvertFlows;
    end
    else if Sender = miFarmBudgets then
    begin
      AForm := frmFarm;
    end
    else
    begin
      Assert(False);
    end;
    AList.Remove(AForm);
    AForm.Show;
    for Index := 0 to AList.Count -1 do
    begin
      AForm := AList[Index];
      AForm.Hide;
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmModChart.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(mainMenuFormChoice);
end;

procedure TfrmModChart.ToolBar1EndDock(Sender, Target: TObject; X,
  Y: Integer);
begin
  if Target <> frmModChart then
  begin
    miDockToolbar.Caption := '&Dock Toolbar'
  end
  else
  begin
    miDockToolbar.Caption := '&Undock Toolbar';
  end;
end;

procedure TfrmModChart.ChooseGraphType(Sender: TObject);
var
  AMenuItem : TFileMenuItem;
  ParentItem: TMenuItem;
  Caption: string;
begin
  AMenuItem := Sender as TFileMenuItem;
  FileToOpen := AMenuItem.FileName;
  if FileExists(FileToOpen) then
  begin
    if ExtractFileExt(FileToOpen) = '._intconf' then
    begin
      DrawingChart := True;
      try
        ParentItem := AMenuItem.Parent;
        Caption := StringReplace(ParentItem.Caption, '&', '',
          [rfReplaceAll, rfIgnoreCase]);

        if Caption = UncertaintyPar then
        begin
          rgIntConfWhatToPlot.ItemIndex := 0;
        end
        else if Caption = UncertaintyPred then
        begin
          rgIntConfWhatToPlot.ItemIndex := 1;
        end
        else
        begin
          Assert(False);
        end;
      finally
        DrawingChart := False;
      end;
    end;
    OpenFile(FileToOpen);
  end;
end;

procedure TfrmModChart.Help2Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmModChart.chartModflowGetAxisLabel(Sender: TChartAxis;
  Series: TChartSeries; ValueIndex: Integer; var LabelText: String);
var
  Value : double;
  Temp : string;
  CommaPos : integer;
  Comma : Char;
begin
  if Sender = chartModflow.LeftAxis then
  begin
    Try
      Comma := ',';
      Temp := LabelText;
      CommaPos := Pos(Comma,Temp);
      while CommaPos > 0 do
      begin
        Temp := copy(Temp,1,CommaPos-1) + copy(Temp,CommaPos+1,Length(Temp));
        CommaPos := Pos(Comma,Temp);
      end;

      Value := InternationalStrToFloat(Temp);
      if (Abs(Value) < 1e-3) and (Value <> 0) then
      begin
        LabelText := Format('%8.2g', [Value]);
      end;
    Except on EConvertError do
      begin
      end;
    end;
  end;
end;

{ TsdValues }

function TsdValues.AbsMax: double;
var
  temp : double;
  Index : Integer;
begin
  result := 0;
  for Index := 0 to Length(Values) -1 do
  begin
    temp := Abs(Values[Index]);
    if Index = 0 then
    begin
      result := temp;
    end
    else
    begin
      if temp > result then
      begin
        result := Temp;
      end;
    end;
  end;
end;

procedure TfrmModChart.Read_pa;
const
  SearchTerm = 'PARAMETER:';
var
  ALine : string;
  LineSeries : TLineSeries;
  MyStyle : TSeriesPointerStyle;
  LineType : TPenStyle;
  ColorIndex : integer;
  Color : TColor;
  Iteration : integer;
  Estimate, FirstEstimate : double;
  FirstValue : boolean;
begin
  SerDataPoints.Active := False;
  ser1to1.Active := False;
  serBarChart.Active := False;
  ExplantionVisible := True;
  FirstValue := True;
  LineSeries := nil;
  MyStyle := High(TSeriesPointerStyle);
  LineType := High(TPenStyle);
  ColorIndex := MaxDefaultColors;
  FirstEstimate := 0;
  While not EOF(FFile) do
  begin
    ReadLn(FFile, ALine);
    if Trim(ALine) <> '' then
    begin
      if Pos(SearchTerm, ALine) > 0 then
      begin
        LineSeries := TLineSeries.Create(self);
        LineSeries.XValues.Order := loNone;
        LineSeries.Title := Trim(Copy(ALine,Length(SearchTerm)+2,Length(ALine)));
        SeriesList.AddObject(LineSeries.Title,LineSeries);
        IncrementStyle(MyStyle);
        IncrementLineType(LineType);
        IncrementColorIndex(ColorIndex);
        Color := ColorPalette[ColorIndex];
        LineSeries.pointer.Style := MyStyle;
        LineSeries.LinePen.Style := LineType;
        LineSeries.SeriesColor := Color;
        LineSeries.Pointer.Visible := True;
        chartModflow.AddSeries(LineSeries);
        FirstValue := True;
        // skip next line
        ReadLn(FFile, ALine);
      end
      else
      begin
        Iteration := StrToInt(Trim(Copy(ALine,2,5)));
        Estimate := FortranStrToFloat(Trim(Copy(ALine,13,MAXINT)));
        if FirstValue and DivideByInitialValues then
        begin
          FirstEstimate := Estimate;
          if FirstEstimate = 0 then
          begin
            FirstEstimate := 1;
          end;
          FirstValue := False;
        end;
        if DivideByInitialValues then
        begin
          Estimate := Estimate / FirstEstimate;
        end;
        LineSeries.AddXY(Iteration,Estimate);
      end;
    end;
  end;
end;

procedure TfrmModChart.miPrintPreviewClick(Sender: TObject);
begin
  inherited;
  ChartPreviewer1.Execute;
end;

procedure TfrmModChart.sbOldFormatChartClick(Sender: TObject);
begin
  inherited;
  frmFormat.GetData(chartModflow, 20);
  frmFormat.ShowModal;
end;

procedure TfrmModChart.chartModflowDrawAxisLabel(Sender: TChartAxis; X, Y,
  Z: Integer; AxisLabel: String; var DrawLabel: Boolean);
var
  Index: integer;
  ValueSet: TSdValues;
  ColorIndex : integer;
begin
  inherited;
  if (Sender = chartModflow.BottomAxis) and (Filetype in [ft_sd, ft_rb])
    and (rgWhatToPlot.ItemIndex = 0) and (SdValueList <> nil) then
  begin
    for Index := 0 to SdValueList.Count -1 do
    begin
      ValueSet := SdValueList[Index] as TSdValues;
      if ValueSet.ObservationName = AxisLabel then
      begin
        ColorIndex := ValueSet.PlotSymbol-1;
        ColorIndex := ColorIndex mod (Length(ColorPalette));
        if ColorIndex < 0 then
        begin
          ColorIndex := ColorIndex + Length(ColorPalette);
        end;
        chartModflow.Canvas.Font.Color := ColorPalette[ColorIndex];
        Exit;
      end;

    end;
  end;
end;

procedure TfrmModChart.TitleCase(var AString: string);
var
  AChar: String;
  Index: integer;
begin
  AString := LowerCase(AString);
  if Length(AString) > 0 then
  begin
    AChar := UpperCase(AString[1]);
    AString[1] := AChar[1];
    for Index := 2 to Length(AString) do
    begin
      if AString[Index-1] in  [' ', '-'] then
      begin
        AChar := UpperCase(AString[Index]);
        AString[Index] := AChar[1];
      end;
    end;
  end;
end;

procedure TfrmModChart.Read_ss;
const
  SSWR = 'SSWR-(';
  StandardError = 'STANDARD ERROR OF THE REGRESSION:';
var
  ALine: string;
  SeriesDataList: TObjectList;
  SeriesData: TRealList;
  StartTitle, EndTitle: integer;
  Title: string;
  ASeries : TLineSeries;
  AValue: double;
  SpacePos: integer;
  SeriesIndex, DataIndex: integer;
  LineType : TPenStyle;
  Color : TColor;
  ColorIndex : integer;
  MyStyle : TSeriesPointerStyle;
  AName: string;
  procedure CreateSeries;
  begin
    TitleCase(Title);
    ASeries := TLineSeries.Create(self);
    ASeries.XValues.Order := loNone;
    SeriesList.AddObject(Title, ASeries);
    ASeries.ParentChart := chartModflow;
    ASeries.Title := Title;

    SeriesData := TRealList.Create;
    SeriesDataList.Add(SeriesData);
  end;
begin
  chartModflow.RightAxis.AutomaticMaximum := True;
  chartModflow.RightAxis.AutomaticMinimum := True;
  chartModflow.Legend.Alignment := laBottom;
  MyStyle := High(TSeriesPointerStyle);
  LineType := High(TPenStyle);
  ColorIndex := MaxDefaultColors;
  ExplantionVisible := True;
  SeriesData := nil;
  SeriesDataList := TObjectList.Create;
  try
    While not EOF(FFile) do
    begin
      ReadLn(FFile, ALine);
      case ModelSelection of
        msModflow: // MODFLOW
          begin
            if Pos('ITERATION', ALine) > 0 then
            begin
              StartTitle := Pos(SSWR, ALine);
              Assert(StartTitle > 0);
              Inc(StartTitle, Length(SSWR));
              Title := Copy(ALine, StartTitle, MAXINT);
              SetLength(Title, Length(Title) -1);

              CreateSeries;
            end
            else if Pos(StandardError, ALine) > 0 then
            begin
              ALine := Trim(Copy(ALine, Pos(StandardError, ALine) +
                Length(StandardError), MAXINT));
              StatusBar1.Panels[1].Text := 'Standard Error = ' + ALine;
            end
            else
            begin
              ALine := Trim(ALine);
              if Length(ALine) > 0 then
              begin
                Assert(SeriesData <> nil);
                SpacePos := Pos(' ', ALine);
                Assert(SpacePos > 0);
                ALine := Trim(Copy(ALine, SpacePos, MAXINT));
                AValue := FortranStrToFloat(ALine);
                SeriesData.Add(AValue);
              end;
            end;
          end;
        msUcode2005: // UCODE
          begin
            if Pos('DONE', ALine) > 0 then
            begin
              break;
            end;
            if Pos('ITERATION', ALine) > 0 then
            begin
              // skip "ITERATION"
              AName := ExtractQuotedText(ALine);

              AName := ExtractQuotedText(ALine);
              while AName <> '' do
              begin
                StartTitle := Pos(SSWR, AName);
                if (StartTitle > 0) then
                begin
                  Inc(StartTitle, Length(SSWR));
                  EndTitle := PosEx(')', AName, Succ(StartTitle));

                  Title := Copy(AName, StartTitle, EndTitle-StartTitle);

                end
                else
                begin
                  Title := AName;
                end;
                CreateSeries;

                if UpperCase(AName) = '# OBSERVATIONS INCLUDED' then
                begin
                  ASeries.VertAxis := aRightAxis;
                end;

                AName := ExtractQuotedText(ALine);
              end;

            end
            else
            begin
              ALine := Trim(ALine);
              if Length(ALine) > 0 then
              begin
                // skip iteration number
                Assert(SeriesData <> nil);
                SpacePos := Pos(' ', ALine);
                Assert(SpacePos > 0);
                ALine := Trim(Copy(ALine, SpacePos, MAXINT));

                for SeriesIndex := 0 to SeriesDataList.Count -1 do
                begin
                  SeriesData := SeriesDataList[SeriesIndex] as TRealList;
                  AValue := FortranStrToFloat(
                    ExtractSpaceSeparatedValue(ALine));
                  SeriesData.Add(AValue);
                end;
              end;
            end;
          end;
      else Assert(False);
      end;

    end;
    Assert(SeriesList.Count = SeriesDataList.Count);
    for SeriesIndex := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[SeriesIndex] as TLineSeries;
      IncrementStyle(MyStyle);
      IncrementLineType(LineType);
      IncrementColorIndex(ColorIndex);
      Color := ColorPalette[ColorIndex];
      ASeries.pointer.Style := MyStyle;
      ASeries.LinePen.Style := LineType;
      ASeries.SeriesColor := Color;
      ASeries.Pointer.Visible := True;
      SeriesData := SeriesDataList[SeriesIndex] as TRealList;
      for DataIndex := 0 to SeriesData.Count -1 do
      begin
        ASeries.AddY(SeriesData[DataIndex]);
      end;
    end;

  finally
    SeriesDataList.Free;
  end;

end;

procedure TfrmModChart.chartModflowClick(Sender: TObject);
begin
  inherited;
  cbLabelPointsClick(nil);
end;

procedure TfrmModChart.ExtractQuotedStrings(const Line: string; Lines: TStringList);
var
  FirstQuotePos: integer;
  SecondQuotePos: integer;
begin
  FirstQuotePos := Pos('"', Line);
  SecondQuotePos := PosEx('"', Line, Succ(FirstQuotePos));
  while (FirstQuotePos > 0) and (SecondQuotePos > 0) do
  begin
    Lines.Add(Copy(Line, FirstQuotePos, SecondQuotePos-FirstQuotePos + 1));
    FirstQuotePos := PosEx('"', Line, Succ(SecondQuotePos));
    SecondQuotePos := PosEx('"', Line, Succ(FirstQuotePos));
  end;
end;

procedure TfrmModChart.ExtractSpaceSeparatedStrings(Line: string; Lines: TStringList);
var
  SpacePos: integer;
begin
  Lines.Clear;
  Line := Trim(Line);
  SpacePos := Pos(' ', Line);
  while (SpacePos > 0) do
  begin
    Lines.Add(Copy(Line, 1, SpacePos-1));
    Line := Trim(Copy(Line, SpacePos+1, MAXINT));
    SpacePos := Pos(' ', Line);
  end;
  Lines.Add(Line);
end;

type
  TSoData = class(TObject)
    Leverage: double;
    Name: string;
    Color: TColor;
  end;

function CompareSoData(Item1, Item2: pointer): integer;
var
  SoData1, SoData2: TSoData;
begin
  SoData1 := Item1;
  SoData2 := Item2;
  result := Sign(SoData2.Leverage - SoData1.Leverage);
end;

procedure TfrmModChart.Read_so;
  procedure ShowWarning(const AString: string);
  begin
    MessageDlg('Quitting: ' + AString
      + ' was not found in the first line of the file.',
      MtWarning, [mbOK], 0);
  end;
var
  Line: string;
  Words: TStringList;
  ObsIndex, PlotIndex, LeverageIndex: integer;
  Obs: string;
  PS: integer;
  Lev: double;
  ColorIndex: integer;
  Color: TColor;
  SoDataList: TList;
  SoData: TSoData;
  Index: integer;
  ShouldPlot: boolean;
  GroupPlotSymbols: TObjectStringList;
  PlotSymbolString: string;
  PlotPosition: Integer;
  GroupI: TGroupIndex;
  GroupName: string;
  Bar: TBarSeries;
const
  ObsName = '"OBSERVATION or PRIOR NAME"';
  PlotSymbol = '"PLOT SYMBOL"';
  Leverage = '"LEVERAGE"';
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspScSo;
  chartModflow.LeftAxis.LabelsSize := 24;
  chartModflow.BottomAxis.LabelStyle := talText;
  chartModflow.BottomAxis.LabelsAngle := 90;
  chartModflow.LeftAxis.Automatic := False;
  chartModflow.LeftAxis.AutomaticMinimum := True;
  chartModflow.LeftAxis.AutomaticMaximum := False;
  chartModflow.LeftAxis.Maximum := 1;
  chartModflow.ShowHint := False;
  cbLabelPoints.Enabled := False;
  chartModflow.Legend.Visible := True;
  ExplantionVisible := True;

  serBarChart.Active := True;
  serBarChart.MultiBar := mbStacked;
  ReadLn(FFile, Line);
  SoDataList := TObjectList.Create;
  Words := TStringList.Create;
  GroupPlotSymbols := TObjectStringList.Create;
  try
    GetGroupNamesFromGmFile(GroupPlotSymbols, gsPlotSymbol);

    ExtractQuotedStrings(Line, Words);
    ObsIndex := Words.IndexOf(ObsName);
    if ObsIndex < 0 then
    begin
      ShowWarning(ObsName);
      Exit;
    end;

    PlotIndex := Words.IndexOf(PlotSymbol);
    if PlotIndex < 0 then
    begin
      ShowWarning(PlotSymbol);
      Exit;
    end;

    LeverageIndex := Words.IndexOf(Leverage);
    if LeverageIndex < 0 then
    begin
      ShowWarning(Leverage);
      Exit;
    end;

    while not EOF(FFile) do
    begin
      Readln(FFile, Line);
      if Line = '' then break;
      ExtractSpaceSeparatedStrings(Line, Words);

      Obs := Words[ObsIndex];
      PlotSymbolString := Words[PlotIndex];
      PS := StrToInt(PlotSymbolString);
      Lev := FortranStrToFloat(Words[LeverageIndex]);

      ColorIndex := PS-1;
      ColorIndex := ColorIndex mod (Length(ColorPalette));
      if ColorIndex < 0 then
      begin
        ColorIndex := ColorIndex + Length(ColorPalette);
      end;

      Color := ColorPalette[ColorIndex];

      SoData:= TSoData.Create;
      SoDataList.Add(SoData);
      SoData.Leverage := Lev;
      SoData.Name := Obs;
      SoData.Color := Color;

      PlotPosition := GroupPlotSymbols.IndexOf(PlotSymbolString);
      if PlotPosition >= 0 then
      begin
        GroupI := GroupPlotSymbols.Objects[PlotPosition] as TGroupIndex;
        GroupName := GroupI.GroupName;
      end
      else
      begin
        GroupName := PlotSymbolString;
      end;
      if SeriesList.IndexOf(GroupName) < 0 then
      begin
        Bar:= TBarSeries.Create(self);
        SeriesList.AddObject(GroupName, Bar);
        Bar.Title := GroupName;
        Bar.MultiBar := mbStacked;
        Bar.Marks.Visible := False;
        Bar.Color := Color;
        chartModflow.AddSeries(Bar);
        Bar.ShowInLegend := True;
//        Bar.Visible := False;
      end;

    end;

    if rgScSoSorting.ItemIndex = 1 then
    begin
      SoDataList.Sort(CompareSoData);
    end;

    for Index := 0 to SoDataList.Count -1 do
    begin
      case rgItemsToPlot_So.ItemIndex of
        0:
          begin
            ShouldPlot := True;
          end;
        1:
          begin
            ShouldPlot := Index < seN_so.AsInteger;
          end;
        2:
          begin
            ShouldPlot := SoDataList.Count -1 - Index < seN_so.AsInteger;
          end;
      else Assert(False);
      end;

      if ShouldPlot then
      begin
        SoData := SoDataList[Index];
        serBarChart.AddY(SoData.Leverage, SoData.Name, SoData.Color);
      end;
    end;
  finally
    Words.Free;
    SoDataList.Free;
    GroupPlotSymbols.Free;
  end;

end;

procedure TfrmModChart.Read_opr_ppr;
var
  ChartType: integer;
begin

  case FileType of
    ft_opr:
      begin
        rbPlotAverage.Caption := 'Plot average value for each observation';
        rbPlotSeries.Caption := 'Plot a separate series for each prediction';
      end;
    ft_ppr:
      begin
        rbPlotAverage.Caption := 'Plot average value for each parameter';
        rbPlotSeries.Caption := 'Plot a separate series for each prediction';
      end;
    ft_ppr_abschg:
      begin
        rbPlotAverage.Caption := 'Plot average value for each parameter (only meaningful if all predictions have the same units)';
        rbPlotSeries.Caption := 'Plot a separate series for each prediction';
      end;
    ft_ppa:
      begin
        rbPlotAverage.Caption := 'Plot average value for each parameter';
        rbPlotSeries.Caption := 'Plot a separate series for each parameter';
      end;
    ft_opr_abschg:
      begin
        rbPlotAverage.Caption := 'Plot average value for each observation (only meaningful if all predictions have the same units)';
        rbPlotSeries.Caption := 'Plot a separate series for each prediction';
      end;
    ft_opa:
      begin
        rbPlotAverage.Caption := 'Plot average value for each observation';
        rbPlotSeries.Caption := 'Plot a separate series for each parameter';
      end;
    else Assert(False);
  end;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspChartType;
  if rbPlotAverage.Checked then
  begin
    ChartType := 0;
  end
  else
  begin
    ChartType := 1;
  end;
  Read_PPR_Files(ChartType, treeGroups1);
end;

procedure TfrmModChart.Read_PPR_Files(ChartType: integer; Tree: TTreeView);
var
  Line: string;
  ObsPredNames: TStringList;
  Values: TStringList;
  SeriesDataList: TObjectList;
  Index: integer;
  Title: string;
  ASeries : TChartSeries;
  StringValue: string;
  Value: double;
  ParameterName: string;
  Sum: double;
  GroupFileName: string;
  GroupFile: TextFile;
  ALine: string;
  Groups: TStringList;
  AGroup: TStringList;
  NewCount: integer;
  GroupIndex: integer;
  FoundGroups: boolean;
  GroupNode: TTreeNode;
//  LineIndex: integer;
  ShouldPlot: boolean;
  MaxItems: integer;
  PlotChoice: integer;
  ShouldSort: boolean;
  BarValues: TList;
  BarValuesList: TList;
  SeriesBarValues: TList;
  BarValue: TBarValue;
  ValueIndex: integer;
  SIndex: integer;
  ClickCheckEvent : TNotifyEvent;
  procedure CreateSeries;
  var
    PositionIndex: integer;
  begin
    TitleCase(Title);
    ASeries := TBarSeries.Create(self);
    ASeries.XValues.Order := loNone;
    SeriesList.AddObject(Title, ASeries);
    ASeries.ParentChart := chartModflow;
    ASeries.Title := Title;
    ASeries.Marks.Visible := False;

    SeriesBarValues := TObjectList.Create;
    BarValuesList.Add(SeriesBarValues);

    if FileType in [ft_ppa_abschg, ft_opa_abschg] then
    begin
      PositionIndex := jvchklstParameters.Items.IndexOf(ASeries.Title);
      if PositionIndex < 0 then
      begin
        PositionIndex := jvchklstParameters.Items.AddObject(ASeries.Title, ASeries);
        jvchklstParameters.Checked[PositionIndex] := True;
      end
      else
      begin
        jvchklstParameters.Items.Objects[PositionIndex] := ASeries;
        ASeries.Visible := jvchklstParameters.Checked[PositionIndex];
      end;
    end;
  end;
begin
  ClickCheckEvent := jvchklstParameters.OnClickCheck;
  jvchklstParameters.OnClickCheck := nil;
  try
    chartModflow.BottomAxis.LabelsAngle := 90;
    if FileType in [ft_ppa_abschg, ft_opa_abschg] then
    begin
      ShouldSort := rgOrder2.ItemIndex = 1;;
      PlotChoice := rgItemsToPlotAbsChg.ItemIndex;
      MaxItems := seN_AbsChg.AsInteger;
    end
    else
    begin
      ShouldSort := rgOrderPPR.ItemIndex = 1;
      PlotChoice := rgItemsToPlotPPR.ItemIndex;
      MaxItems := seN_PPR.AsInteger;
    end;

    Tree.Items.Clear;

    BarValuesList := TObjectList.Create;
    BarValues := TObjectList.Create;
    Groups:= TStringList.Create;
    try
      GroupFileName := ChangeFileExt(NameOfFile, '.#OPR-PPR');
      FoundGroups := False;
      if FileExists(GroupFileName) then
      begin
        AssignFile(GroupFile, GroupFileName);
        reset(GroupFile);
        try
          GroupNode := nil;
          while not EOF(GroupFile) and not FoundGroups do
          begin
            ReadLn(GroupFile, ALine);
            ALine := Trim(ALine);
            if ALine = 'PARAMETER GROUPS FOR PPR STATISTIC:' then
            begin
              FoundGroups := True;
              // skip a line;
              ReadLn(GroupFile, ALine);
              ReadLn(GroupFile, ALine);
              while Trim(ALine) <> '' do
              begin
                AGroup := TStringList.Create;
                AGroup.Delimiter := ' ';
                AGroup.DelimitedText := Trim(ALine);
                if AGroup.Count > 2 then
                begin
                  AGroup.Delete(0);
                  Groups.AddObject(AGroup[0], AGroup);
                  GroupNode := Tree.Items.Add(GroupNode, AGroup[0]);

                  AGroup.Delete(0);
                  NewCount := AGroup.Count div 2;
                  while AGroup.Count > NewCount do
                  begin
                    AGroup.Delete(AGroup.Count-1);
                  end;
                  for Index := 0 to AGroup.Count -1 do
                  begin
                    Tree.Items.AddChild(GroupNode, AGroup[Index])
                  end;
                end
                else
                begin
                  AGroup.Free;
                end;
                ReadLn(GroupFile, ALine);
              end;
            end;

          end;
        finally
          CloseFile(GroupFile);
        end;
      end;
  //    if not FoundGroups and (jvplChartControls.ActivePage = jvspPpaAbschg) then
  //    begin
  //      jvplChartControls.Visible := False;
  //    end;

      SeriesDataList:= TObjectList.Create;
      ObsPredNames := TStringList.Create;
      Values := TStringList.Create;
      try
        ReadLn(FFile, Line);
        ObsPredNames.Delimiter := ' ';
        ObsPredNames.DelimitedText := Trim(Line);

        case ChartType of
          0:
            begin
              ExplantionVisible := False;
              // Average Values, do nothing
            end;
          1:
            begin
              ExplantionVisible := True;
              for Index := 1 to ObsPredNames.Count -1 do
              begin
                // Separate Series
                Title := Trim(ObsPredNames[Index]);
                CreateSeries;
              end;
            end;
        else Assert(False);
        end;

        Values.Delimiter := ' ';
        while not EOF(FFile) do
        begin
          Readln(FFile, Line);
          if Line = '' then break;
          Values.DelimitedText := Trim(Line);

          Assert(Values.Count = ObsPredNames.Count);
          ParameterName := Trim(Values[0]);
          Sum := 0;
          for Index := 1 to Values.Count -1 do
          begin
            StringValue := Values[Index];
            Value := FortranStrToFloat(StringValue);

            case ChartType of
              0:
                begin
                  // Average Values
                  Sum := Sum + Value;
                end;
              1:
                begin
                  SeriesBarValues := BarValuesList[Index-1];
                  // Separate Series
  //                ASeries := SeriesList.Objects[Index-1] as TChartSeries;
                  if chartModflow.LeftAxis.Logarithmic then
                  begin
                    Value := Abs(Value);
                  end;

                  BarValue := TBarValue.Create;
                  SeriesBarValues.Add(BarValue);
                  BarValue.ParameterName := ParameterName;
                  BarValue.Value := Value;

  //                  ASeries.AddY(Value, ParameterName, clTeeColor);
                end;
            else Assert(False);
            end;
          end;

          if ChartType = 0 then
          begin
            BarValue := TBarValue.Create;
            BarValues.Add(BarValue);
            BarValue.ParameterName := ParameterName;
            BarValue.Value := Sum /(Values.Count -1);

  //            serBarChart.AddBar(Sum /(Values.Count -1), ParameterName, clTeeColor);
          end;
        end;

        if FileType in [ft_ppa_abschg, ft_opa_abschg] then
        begin
          for SIndex := jvchklstParameters.Items.Count -1 downto 0 do
          begin
            if SeriesList.IndexOfObject(jvchklstParameters.Items.Objects[SIndex]) < 0 then
            begin
              jvchklstParameters.Items.Delete(SIndex);
            end;
          end;
        end;


        if ShouldSort then
        begin
          for Index := 0 to BarValuesList.Count -1 do
          begin
            SeriesBarValues := BarValuesList[Index];
            SeriesBarValues.Sort(SortBarValues);
          end;
          BarValues.Sort(SortBarValues);
        end;

        for Index := 0 to BarValuesList.Count -1 do
        begin
          SeriesBarValues := BarValuesList[Index];
          ASeries := SeriesList.Objects[Index] as TChartSeries;
          for ValueIndex := 0 to SeriesBarValues.Count-1 do
          begin
            case PlotChoice of
              0:
                begin
                  ShouldPlot := True;
                end;
              1:
                begin
                  ShouldPlot := ValueIndex < MaxItems;
                end;
              2:
                begin
                  ShouldPlot := SeriesBarValues.Count-1 - ValueIndex < MaxItems;
                end;
            else Assert(False);
            end;

            if ShouldPlot then
            begin
              BarValue := SeriesBarValues[ValueIndex];
              ASeries.AddY(BarValue.Value, BarValue.ParameterName, clTeeColor);
            end;

          end;
        end;

        for ValueIndex := 0 to BarValues.Count-1 do
        begin
          case PlotChoice of
            0:
              begin
                ShouldPlot := True;
              end;
            1:
              begin
                ShouldPlot := ValueIndex < MaxItems;
              end;
            2:
              begin
                ShouldPlot := BarValues.Count-1 - ValueIndex < MaxItems;
              end;
          else Assert(False);
          end;
          if ShouldPlot then
          begin
            BarValue := BarValues[ValueIndex];
            serBarChart.AddBar(BarValue.Value, BarValue.ParameterName, clTeeColor);
          end;
        end;

        if ChartType = 0 then
        begin
          serBarChart.Visible := True;
        end;
      finally
        ObsPredNames.Free;
        Values.Free;
        SeriesDataList.Free;
      end;
    finally
      for GroupIndex := 0 to Groups.Count -1 do
      begin
        Groups.Objects[GroupIndex].Free;
      end;
      Groups.Free;
      BarValues.Free;
      BarValuesList.Free;
    end;
  finally
    jvchklstParameters.OnClickCheck := ClickCheckEvent;
  end;
end;

procedure TfrmModChart.Read_pcc;
var
  Values: TStringList;
  Line: string;
  Parameter1: string;
  Parameter2: string;
  StrValue: string;
  Value: double;
  Color: TColor;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspPcc;
  chartModflow.BottomAxis.LabelsAngle := 90;

  serBarChart2.Active := True;
  serBarChart.SeriesColor := clRed;
  serBarChart2.SeriesColor := clBlue;
  serBarChart.Title := 'Positive Values';
  serBarChart2.Title := 'Negative Values';
  serBarChart.ShowInLegend := True;
  serBarChart2.ShowInLegend := True;

  serBarChart.MultiBar := mbStacked;
  serBarChart2.MultiBar := mbStacked;


  ExplantionVisible := True;
  Values := TStringList.Create;
  try
    Values.Delimiter := ' ';

    ReadLn(FFile, Line);
    While not Eof(FFile) do
    begin
      ReadLn(FFile, Line);
      if Line = '' then
      begin
        break;
      end;
      Values.DelimitedText := Line;
      Parameter1 := Values[0];
      Parameter2 := Values[1];
      StrValue := Values[2];
      Value := FortranStrToFloat(StrValue);
      if Value > 0 then
      begin
        Color := clRed;
      end
      else
      begin
        Color := clBlue;
      end;

      serBarChart.AddBar(Abs(Value), Parameter1 + #13 + Parameter2, Color);
    end;

  finally
    Values.Free;
  end;
  SetMaxAndMinAxisValues(chartModflow.LeftAxis, 1, 0.85);
//  chartModflow.LeftAxis.Maximum := 1;
//  chartModflow.LeftAxis.Minimum := 0.85;
  chartModflow.LeftAxis.Increment := 0.05;
  chartModflow.LeftAxis.AutomaticMinimum := False;
  chartModflow.LeftAxis.AutomaticMaximum := False;

  serBarChart.Visible := True;
end;

procedure TfrmModChart.SetMaxAndMinAxisValues(Axis: TChartAxis;
  NewMax, NewMin: double);
begin
  if Axis.Maximum < NewMax then
  begin
    Axis.Maximum := NewMax;
  end;
  if Axis.Minimum > NewMin then
  begin
    Axis.Minimum := NewMin;
  end;
  Axis.Maximum := NewMax;
  Axis.Minimum := NewMin;
end;

procedure TfrmModChart.Read_ppa_abschg;
var
  ChartType: integer;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvspPpaAbschg;
  ChartType := 1;
//  chartModflow.LeftAxis.Logarithmic := True;
  Read_PPR_Files(ChartType, treeGroups2);
end;

procedure TfrmModChart.RedrawChart(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  if DrawingChart then
  begin
    Exit
  end;

  DrawingChart := True;
  try
    if (OpenDialogOutputFile <> nil) and (treecomboFileNames <> nil) then
    begin
      if treecomboFileNames.Selected <> nil then
      begin
        FileName := GetCurrentDir + '\' + treecomboFileNames.Selected.Text;
        if FileExists(FileName) then
        begin
          OpenFile(FileName);
        end;
      end;
    end;
  finally
    DrawingChart := False;
  end;
end;

procedure TfrmModChart.rgWhatToPlotClick(Sender: TObject);
begin
  inherited;
  rgOrder.Enabled := rgWhatToPlot.ItemIndex = 0;
  rgPointOrder.Enabled := rgOrder.Enabled;
  RedrawChart(Sender);
end;

procedure TfrmModChart.ReadStandardError2(const RegressionFileName: string;
  ShowWarning: boolean; out StandardErrorLine: string);
var
  AStringList : TStringList;
  Index : integer;
  AString : string;
  EqualPos : integer;
begin
  StandardErrorLine := '';
  AStringList := TStringList.Create;
  try
    AStringList.LoadFromFile(RegressionFileName);
    for Index := 0 to AStringList.Count -1 do
    begin
      AString := AStringList[Index];
      if Pos('STANDARD ERROR OF THE REGRESSION',AString) > 0 then
      begin
        EqualPos := -1;
        case ModelSelection of
          msModflow:
            begin
              EqualPos := Pos('=',AString);
            end;
          msUcode2005:
            begin
              EqualPos := PosEx('"',AString, 3);
            end;
        else Assert(False);
        end;

        if EqualPos > 0 then
        begin
          Delete(AString,1,EqualPos);
          StandardErrorLine := trim(AString);
          Exit;
        end;
      end;
    end;
    if ShowWarning then
    begin
      Beep;
      MessageDlg('Standard error of regression not found in '
        + RegressionFileName, mtWarning, [mbOK], 0);
    end;        
  finally
    AStringList.Free;
  end;
end;

procedure TfrmModChart.btnReadClick(Sender: TObject);
var
//  AStringList : TStringList;
//  Index : integer;
//  AString : string;
//  EqualPos : integer;
//  RegressionFileName: string;
  StandardErrorLine: string;
begin
  case ModelSelection of
    msModflow:
      begin
        OpenDialog1.Filter := 'global or list files|*.list;*.lst;*.glo;*.lsg|All Files|*.*';
      end;
    msUcode2005:
      begin
        OpenDialog1.Filter := '_dm files|*._dm';
        OpenDialog1.FileName := ChangeFileExt(treecomboFileNames.Selected.Text, '._dm');
      end;
  else Assert(False);
  end;

  if OpenDialog1.Execute then
  begin
//    RegressionFileName := OpenDialog1.FileName;
    ReadStandardError2(OpenDialog1.FileName, True, StandardErrorLine);
    if StandardErrorLine <> '' then
    begin
      adeStandardError.Text := StandardErrorLine;
      RedrawChart(nil);
    end;      

//    AStringList := TStringList.Create;
//    try
//      AStringList.LoadFromFile(RegressionFileName);
//      for Index := 0 to AStringList.Count -1 do
//      begin
//        AString := AStringList[Index];
//        if Pos('STANDARD ERROR OF THE REGRESSION',AString) > 0 then
//        begin
//          EqualPos := -1;
//          case ModelSelection of
//            msModflow:
//              begin
//                EqualPos := Pos('=',AString);
//              end;
//            msUcode2005:
//              begin
//                EqualPos := PosEx('"',AString, 3);
//              end;
//          else Assert(False);
//          end;
//
//          if EqualPos > 0 then
//          begin
//            Delete(AString,1,EqualPos);
//            adeStandardError.Text := trim(AString);
//            RedrawChart(Sender);
//            Exit;
//          end;
//        end;
//      end;
//      Beep;
//      MessageDlg('Standard error of regression not found in '
//        + RegressionFileName, mtWarning, [mbOK], 0);
//    finally
//      AStringList.Free;
//    end;
  end;
end;

procedure TfrmModChart.adeStandardErrorChange(Sender: TObject);
begin
  inherited;
  RedrawChart(Sender);
end;

procedure TfrmModChart.clbSeriesListClickCheck(Sender: TObject);
var
  Index: integer;
  ASeries: TPointSeries;
begin
  inherited;
  for Index := 0 to clbSeriesList.Items.Count -1 do
  begin
    ASeries := clbSeriesList.Items.Objects[Index] as TPointSeries;
    ASeries.Visible := clbSeriesList.Checked[Index];
  end;

end;

procedure TfrmModChart.treecomboFileNamesChange(Sender: TObject);
var
  FileName: string;
  ParentNode: TTreeNode;
begin
  inherited;
  if OpeningFile then Exit;
  OpeningFile := True;
  try
    if treecomboFileNames.Selected <> nil then
    begin
      FileName := GetCurrentDir + '\' + treecomboFileNames.Selected.Text;
      if FileExists(FileName) and (FileName <> TreeFileNodeFileName) then
      begin
        if ExtractFileExt(FileName) = '._intconf' then
        begin
          ParentNode := treecomboFileNames.Selected.Parent;
          DrawingChart := True;
          try
            if ParentNode.Text = UncertaintyPar then
            begin
              rgIntConfWhatToPlot.ItemIndex := 0;
            end
            else if ParentNode.Text = UncertaintyPred then
            begin
              rgIntConfWhatToPlot.ItemIndex := 1;
            end
            else
            begin
              Assert(False);
            end;
          finally
            DrawingChart := False;
          end;

        end;
        TreeFileNodeFileName := FileName;
        TreeFileNode := treecomboFileNames.Selected;
        OpenFile(FileName);
        // because _intconf files are classified two ways,
        // treecomboFileNames.Selected can get reassigned during
        // OpenFile.
        treecomboFileNames.Selected := TreeFileNode;
      end;
    end;
  finally
    OpeningFile := False;
  end;
end;

procedure TfrmModChart.rgItemsToPlotClick(Sender: TObject);
begin
  inherited;
  spN.Enabled := rgItemsToPlot.ItemIndex > 0;
  RedrawChart(Sender);
end;

procedure TfrmModChart.spNChange(Sender: TObject);
begin
  inherited;
  DelayRedrawChart(Sender);
end;

function TfrmModChart.MultipleSeriesGetPointerStyle(Sender: TChartSeries;
  ValueIndex: Integer): TSeriesPointerStyle;
var
  ChartPosition: integer;
  StyleList: TList;
  Style: TStyleType;
begin
  ChartPosition := SeriesList.IndexOfObject(Sender);
  Assert(ChartPosition >= 0);
  StyleList := RdRgStyleList[ChartPosition];
  if ValueIndex >= StyleList.Count then
  begin
    result := Low(TSeriesPointerStyle);
  end
  else
  begin
    Style := StyleList[ValueIndex];
    result := Style.Style;
  end;
end;

function TfrmModChart.GetGroupLabels(GroupType: TGroupType;
  PlotSymbols: TIntegerList; GroupNames: TStringList): boolean;
var
  GroupFileName: string;
  GroupFile: TStringList;
  Index: integer;
  GroupLineSplitter: TStringList;
  GroupName: string;
  PlotSymbol: integer;
  PlotSymbolPosition: integer;
begin
  GroupFileName := '';
  case GroupType of
    gtObservation:
      begin
        GroupFileName := ChangeFileExt(NameOfFile, '._gm');
      end;
    gtPrediction:
      begin
        GroupFileName := ChangeFileExt(NameOfFile, '._gmp');
      end;
  else Assert(False);
  end;
  result := FileExists(GroupFileName);
  if result then
  begin
    PlotSymbols.Clear;
    PlotSymbols.Sorted := True;
    GroupNames.Clear;
    GroupFile := TStringList.Create;
    GroupLineSplitter := TStringList.Create;
    try
      GroupLineSplitter.Delimiter := ' ';
      GroupFile.LoadFromFile(GroupFileName);
      // skip first line.
      for Index := 1 to GroupFile.Count -1 do
      begin
        GroupLineSplitter.DelimitedText := GroupFile[Index];
        GroupName := GroupLineSplitter[0];
        PlotSymbol := StrToInt(GroupLineSplitter[2]);
        PlotSymbolPosition := PlotSymbols.IndexOf(PlotSymbol);
        if PlotSymbolPosition < 0 then
        begin
          PlotSymbolPosition := PlotSymbols.Add(PlotSymbol);
          GroupNames.Insert(PlotSymbolPosition, GroupName);
        end
        else
        begin
          if UpperCase(GroupNames[PlotSymbolPosition]) <>
            UpperCase(GroupName) then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
    finally
      GroupFile.Free;
      GroupLineSplitter.Free;
    end;
  end;
end;

const
  Captions_linp: array[0..5] of string =
    ('"INDIVIDUAL 95% CONFIDENCE INTERVALS"',
     '"SIMULTANEOUS 95% CONFIDENCE INTERVALS"',
     '"UNDEFINED NUMBER OF SIMULTANEOUS 95% CONFIDENCE INTERVALS"',
     '"INDIVIDUAL 95% PREDICTION INTERVALS"',
     '"SIMULTANEOUS 95% PREDICTION INTERVALS"',
     '"UNDEFINED NUMBER OF SIMULTANEOUS 95% PREDICTION INTERVALS"'
     );

procedure TfrmModChart.Read_linp;
var
  PredictedValue: double;
  BL: double; // lower range
  Bu: double; // upper range
  UpperConfInterval: double;
  LowerConfInterval: double;
  ALine: string;
  DivideByMean: boolean;
  Temp: double;
  LineList, Captions : TStringList;
  DesiredList: integer;
  SelectedIndex: integer;
  CorrectSeries: boolean;
  PredictionName: string;
  StandardDeviation: double;
  Index: integer;
  PlotSymbol: integer;
  PlotSymbols: TIntegerList;
  SymbolIndex: integer;
  ConvertPlotSymbolToGroupName: boolean;
  GroupPlotSymbols: TIntegerList;
  GroupNames: TStringList;
  Line: string;
  SpaceIndex: integer;
  PlotSymbolCaption: string;
begin
  FPlottingLinp := True;
  try
    chartModflow.BottomAxis.LabelsAngle := 90;
    jvplChartControls.Visible := True;
    jvplChartControls.ActivePage := jvsp_linp;
    DivideByMean := cbScale_linp.Checked;
    LineAt1.Active := DivideByMean;
    ExplantionVisible := True;
    chartModflow.Legend.Visible := False;
    RangeSeries.Active := True;
    RangeSeries.Title := 'Parameter Range';

    DesiredList := 0;
    if rbIndividual.Checked then
    begin
      DesiredList := 0;
    end
    else if rbSimulataneous.Checked then
    begin
      DesiredList := 1;
    end
    else if rbUndefinedSimulataneous.Checked then
    begin
      DesiredList := 2;
    end
    else
    begin
      Assert(False);
    end;

    DesiredList := DesiredList + rgIntervalType.ItemIndex*3;

    GroupNames := TStringList.Create;
    GroupPlotSymbols := TIntegerList.Create;
    PlotSymbols := TIntegerList.Create;
    LineList := TStringList.Create;
    Captions := TStringList.Create;
    try
      ConvertPlotSymbolToGroupName := GetGroupLabels(gtPrediction,
        GroupPlotSymbols, GroupNames);

      for Index := 0 to clbLinpPlotSymbol.Items.Count -1 do
      begin
        Line := clbLinpPlotSymbol.Items[Index];
        SpaceIndex := Pos(' ', Line);
        if SpaceIndex > 0 then
        begin
          Line := Copy(Line, 1, SpaceIndex-1);
        end;
        PlotSymbols.Add(StrToInt(Line));
      end;
      PlotSymbols.Sorted := True;
      if ConvertPlotSymbolToGroupName and (PlotSymbols.Count > 0) then
      begin
        ConvertPlotSymbolToGroupName :=
          PlotSymbols.Count = GroupPlotSymbols.Count;
        if ConvertPlotSymbolToGroupName then
        begin
          for Index := 0 to PlotSymbols.Count-1 do
          begin
            if PlotSymbols[Index] <> GroupPlotSymbols[Index] then
            begin
              ConvertPlotSymbolToGroupName := False;
              break;
            end;
          end;
        end;
      end;

      LineList.Delimiter := ' ';
      for Index := 0 to Length(Captions_linp) -1 do
      begin
        Captions.Add(Captions_linp[Index]);
      end;

      CorrectSeries := False;
      While not EOF(FFile) do
      begin
        ReadLn(FFile, ALine);
        if Trim(ALine) = '' then
        begin
          Continue;
        end;

        SelectedIndex := Captions.IndexOf(Trim(ALine));
        if SelectedIndex >= 0 then
        begin
          CorrectSeries := SelectedIndex = DesiredList;
          // skip a line.
          ReadLn(FFile, ALine);
          if SelectedIndex <= DesiredList then
          begin
            Continue;
          end
          else
          begin
            break;
          end;
        end;
        if not CorrectSeries then
        begin
          Continue;
        end;

        LineList.DelimitedText := ALine;
        Assert(LineList.Count = 6);
        PredictionName := LineList[0];
        PredictedValue := FortranStrToFloat(LineList[1]);
        LowerConfInterval := FortranStrToFloat(LineList[2]);
        UpperConfInterval := FortranStrToFloat(LineList[3]);
        StandardDeviation := FortranStrToFloat(LineList[4]);
        PlotSymbol := StrToInt(LineList[5]);

        SymbolIndex := PlotSymbols.IndexOf(PlotSymbol);
        if SymbolIndex < 0 then
        begin

          if ConvertPlotSymbolToGroupName then
          begin
            SymbolIndex := GroupPlotSymbols.IndexOf(PlotSymbol);
            if SymbolIndex < 0 then
            begin
              PlotSymbolCaption := IntToStr(PlotSymbol);
            end
            else
            begin
              PlotSymbolCaption := IntToStr(PlotSymbol)
                + ' ' + GroupNames[SymbolIndex];
            end;
          end
          else
          begin
            PlotSymbolCaption := IntToStr(PlotSymbol);
          end;

          SymbolIndex := PlotSymbols.Add(PlotSymbol);
          clbLinpPlotSymbol.Items.Insert(SymbolIndex, PlotSymbolCaption);
          clbLinpPlotSymbol.Checked[SymbolIndex] := True;
        end;
        if not clbLinpPlotSymbol.Checked[SymbolIndex] then
        begin
          Continue;
        end;

        BL := PredictedValue - StandardDeviation;
        BU := PredictedValue + StandardDeviation;

        if DivideByMean and (PredictedValue <> 0) then
        begin
          UpperConfInterval := UpperConfInterval/PredictedValue;
          LowerConfInterval := LowerConfInterval/PredictedValue;
          Bu := Bu/PredictedValue;
          BL := BL/PredictedValue;
          PredictedValue := 1;
          if UpperConfInterval < LowerConfInterval then
          begin
            Temp := UpperConfInterval;
            UpperConfInterval := LowerConfInterval;
            LowerConfInterval := Temp;
          end;
          if Bu < BL then
          begin
            Temp := Bu;
            Bu := BL;
            BL := Temp;
          end;
        end;

        RangeSeries.AddCI_AndRange(Trim(PredictionName), PredictedValue,
          UpperConfInterval, LowerConfInterval, Bu, BL);
        RangeSeries.ShowRange := cbLinpPlotStandardDev.Checked;
        RangeSeries.ShowConfidenceIntervals := cbLinpConfidenceIntervals.Checked;

        if DivideByMean then
        begin
          LineAt1.AddY(1)
        end;
      end
    finally
      LineList.Free;
      Captions.Free;
      PlotSymbols.Free;
      GroupPlotSymbols.Free;
      GroupNames.Free;
    end;
  finally
    FPlottingLinp := False;
  end;
end;

procedure TfrmModChart.Read_intconf_intpred;
var
  Intervals: TList;
  ALine: string;
  Values: TStringList;
  IntervalName: string;
  Identifier: integer;
  Converged: boolean;
  PredictedValue: double;
  Limit: double;
  Interval: TNonLinConfInt;
  Index: integer;
  BarColor: TColor;
  DivideByMean: boolean;
  Simultaneous: boolean;
  PercentDeviation: double;
  UpperDeviationSeries: TLineSeries;
  LowerDeviationSeries: TLineSeries;
  MaxDeviation: double;
  ParameterNames: TStringList;
  PredictionNames: TStringList;
  TempFile: TStringList;
  DelimitedLine: TStringList;
  AlternateFileName: string;
  ShouldPlot: boolean;
  FileIndex: integer;
  ExtraFile: TStringList;
  LineIndex: integer;
  PointColor: TColor;
  WarningMessage: string;
  procedure CreateDeviationSeries(var DevSeries: TLineSeries; Const Title: string);
  begin
    DevSeries := TLineSeries.Create(self);
    DevSeries.XValues.Order := loNone;
    SeriesList.AddObject(Title, DevSeries);
    DevSeries.ParentChart := chartModflow;
    DevSeries.Title := Title;
    DevSeries.VertAxis := aRightAxis;
    DevSeries.Pointer.Visible := True;
    DevSeries.Pointer.HorizSize := 3;
    DevSeries.Pointer.VertSize := 3;
  end;
  procedure HandleLine(ALine: string);
  begin
    Values.DelimitedText := ALine;

    IntervalName := Values[0];
    Identifier := StrToInt(Values[1]);
    Converged := Values[2] = 'YES';
    PredictedValue := FortranStrToFloat(Values[3]);
    Limit := FortranStrToFloat(Values[4]);
    Simultaneous := Values[8] = 'SIMULTANEOUS';
    PercentDeviation := FortranStrToFloat(Values[7]);

    if Abs(Identifier)-1 >= Intervals.Count then
    begin
      Interval := TNonLinConfInt.Create;
      Intervals.Add(Interval);
      Interval.Name := IntervalName;
      Interval.Converged := Converged;
      Interval.PredictedValue := PredictedValue;
      Interval.Simultaneous := Simultaneous;
      if Identifier > 0 then
      begin
        Interval.UpperLimit := Limit;
        Interval.LowerLimit := PredictedValue;
        Interval.UpperPercentDeviation := PercentDeviation;
      end
      else if Identifier < 0 then
      begin
        Interval.LowerLimit := Limit;
        Interval.UpperLimit := PredictedValue;
        Interval.LowerPercentDeviation := PercentDeviation;
      end
      else
      begin
        assert(False);
      end;
    end
    else
    begin
      Interval := Intervals[Abs(Identifier)-1];
      Assert(Interval.Name = IntervalName);
      Interval.Converged := Interval.Converged and Converged;
      Assert(Interval.PredictedValue = PredictedValue);
      if Identifier > 0 then
      begin
        Interval.UpperLimit := Limit;
        Interval.UpperPercentDeviation := PercentDeviation;
      end
      else if Identifier < 0 then
      begin
        Interval.LowerLimit := Limit;
        Interval.LowerPercentDeviation := PercentDeviation;
      end
      else
      begin
        assert(False);
      end;
    end;
  end;
begin
  WarningMessage := '';
  RangeSeries.ColorEachPoint := True;
  lblOrangeCI.Visible := False;
  lblRedCI.Visible := False;
  chartModflow.OnGetLegendPos := chartModflowIntPredGetLegendPos;
  chartModflow.BottomAxis.LabelsAngle := 90;
  chartModflow.RightAxis.AxisValuesFormat := '#,##0.###%  ';
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_intconf;
  DivideByMean := cbScale_intconf.Checked;
  ExplantionVisible := True;
  chartModflow.Legend.Visible := True;
  chartModflow.Legend.Alignment := laBottom;
  RangeSeries.Active := True;
  RangeSeries.Title := 'Predicted Value';
  chartModflow.LeftAxis.StartPosition := 30;
  chartModflow.LeftAxis.EndPosition := 100;
  chartModflow.RightAxis.StartPosition := 0;
  chartModflow.RightAxis.EndPosition := 30;

  CreateDeviationSeries(UpperDeviationSeries, 'Deviation Upper Interval');
  CreateDeviationSeries(LowerDeviationSeries, 'Deviation Lower Interval');
  UpperDeviationSeries.Color := clFuchsia;
  LowerDeviationSeries.Color := clGreen;
  UpperDeviationSeries.Pointer.Style := psTriangle;
  LowerDeviationSeries.Pointer.Style := psDownTriangle;

  ParameterNames:= TStringList.Create;
  PredictionNames:= TStringList.Create;
  Intervals := TObjectList.Create;
  Values := TStringList.Create;
  try
    if FileType = ft_intconf then
    begin
      rgIntConfWhatToPlot.Enabled := True;
      TempFile := TStringList.Create;
      DelimitedLine := TStringList.Create;
      try
        DelimitedLine.Delimiter := ' ';
        AlternateFileName := ChangeFileExt(NameOfFile, '._p');
        if FileExists(AlternateFileName) then
        begin
          TempFile.LoadFromFile(AlternateFileName);
          for Index := 1 to TempFile.Count -1 do
          begin
            DelimitedLine.DelimitedText := TempFile[Index];
            Assert(DelimitedLine.Count >= 3);
            PredictionNames.Add(DelimitedLine[2])
          end;
        end;
        rgIntConfWhatToPlot.Controls[1].Enabled := PredictionNames.Count > 0;

        AlternateFileName := ChangeFileExt(NameOfFile, '._paopt');
        if FileExists(AlternateFileName) then
        begin
          TempFile.LoadFromFile(AlternateFileName);
          for Index := 1 to TempFile.Count -1 do
          begin
            DelimitedLine.DelimitedText := TempFile[Index];
            Assert(DelimitedLine.Count >= 4);
            ParameterNames.Add(DelimitedLine[0])
          end;
        end;
        rgIntConfWhatToPlot.Controls[0].Enabled := ParameterNames.Count > 0;
        if not rgIntConfWhatToPlot.Controls[rgIntConfWhatToPlot.ItemIndex].Enabled then
        begin
          rgIntConfWhatToPlot.ItemIndex := 2;
        end;
      finally
        TempFile.Free;
        DelimitedLine.Free;
      end;
    end
    else
    begin
      rgIntConfWhatToPlot.Enabled := False;
    end;

    Values.Delimiter := ' ';
    // skip three lines
    Readln(FFile, ALine);
    Readln(FFile, ALine);
    Readln(FFile, ALine);
    while not EOF(FFile) do
    begin
      Readln(FFile, ALine);
      if Trim(ALine) = '' then
      begin
        Continue;
      end;
      HandleLine(ALine);
    end;

    WarningMessage := '';
    for FileIndex := 0 to reIntconfFiles.Lines.Count -1 do
    begin
      if Trim(reIntconfFiles.Lines[FileIndex]) <> '' then
      begin
        if FileExists(reIntconfFiles.Lines[FileIndex]) then
        begin
          ExtraFile:= TStringList.Create;
          try
            ExtraFile.LoadFromFile(reIntconfFiles.Lines[FileIndex]);
            for LineIndex := 3 to ExtraFile.Count -1 do
            begin
              if Trim(ExtraFile[LineIndex]) = '' then
              begin
                Continue;
              end;
              HandleLine(ExtraFile[LineIndex]);
            end;
          finally
            ExtraFile.Free
          end;
        end
        else
        begin
          WarningMessage := WarningMessage
            + reIntconfFiles.Lines[FileIndex] + #13#10;
        end;
      end;
    end;

    MaxDeviation := 0;
    for Index := 0 to Intervals.Count -1 do
    begin
      Interval := Intervals[Index];
      if (Interval.PredictedValue < Interval.LowerLimit)
        or (Interval.PredictedValue > Interval.UpperLimit) then
      begin
        BarColor := clRed;
        PointColor := BarColor;
        lblRedCI.Visible := True;
      end
      else if not Interval.Converged then
      begin
        // orange
        BarColor := TColor($00C0FF);;
        PointColor := BarColor;
        lblOrangeCI.Visible := True;
      end
      else
      begin
        BarColor := clBlack;
        PointColor := clBlue;
      end;

      if DivideByMean and (Interval.PredictedValue <> 0) then
      begin
        Interval.LowerLimit := Interval.LowerLimit/Interval.PredictedValue;
        Interval.UpperLimit := Interval.UpperLimit/Interval.PredictedValue;
        Interval.PredictedValue := 1;
      end;

      if MaxDeviation < Abs(Interval.UpperPercentDeviation) then
      begin
        MaxDeviation := Abs(Interval.UpperPercentDeviation)
      end;
      if MaxDeviation < Abs(Interval.LowerPercentDeviation) then
      begin
        MaxDeviation := Abs(Interval.LowerPercentDeviation)
      end;

      ShouldPlot := True;
      if FileType = ft_intconf then
      begin
        case rgIntConfWhatToPlot.ItemIndex of
          0: // parameters
            begin
              ShouldPlot := ParameterNames.IndexOf(Interval.Name) >= 0;
            end;
          1: // predictions
            begin
              ShouldPlot := PredictionNames.IndexOf(Interval.Name) >= 0;
            end;
          2: // both
            begin
              // do nothing;
            end;
        else Assert(False);
        end;

      end;

      if ShouldPlot then
      begin
        RangeSeries.AddConfidenceInterval(Interval.Name, Interval.PredictedValue,
          Interval.UpperLimit, Interval.LowerLimit, PointColor, BarColor, BarColor);
        RangeSeries.ShowConfidenceIntervals := True;

        UpperDeviationSeries.AddY(Interval.UpperPercentDeviation, Interval.Name);
        LowerDeviationSeries.AddY(Interval.LowerPercentDeviation, Interval.Name);
      end;

{      if DivideByMean then
      begin
        LineAt1.AddY(1)
      end;   }
    end;

    if lblOrangeCI.Visible then
    begin
      lblRedCI.Top := RedY;
    end
    else
    begin
      lblRedCI.Top := OrangeY;
    end;

    tabWarnings.TabVisible := (lblRedCI.Visible or lblRedCI.Visible);
    tabIntconfControls.TabVisible := tabWarnings.TabVisible;
    if tabWarnings.TabVisible then
    begin
      pcIntConf.ActivePage := tabWarnings;
    end
    else
    begin
      pcIntConf.ActivePage := tabIntconfControls;
    end;


    if Intervals.Count > 0 then
    begin
      MaxDeviation := MaxDeviation * 1.1;
      chartModflow.RightAxis.AutomaticMaximum := False;
      chartModflow.RightAxis.AutomaticMinimum := False;
      SetMaxAndMinAxisValues(chartModflow.RightAxis, MaxDeviation, -MaxDeviation);

{      if chartModflow.RightAxis.Maximum < MaxDeviation then
      begin
        chartModflow.RightAxis.Maximum := MaxDeviation;
      end;
      if chartModflow.RightAxis.Minimum > -MaxDeviation then
      begin
        chartModflow.RightAxis.Minimum := -MaxDeviation;
      end;

      chartModflow.RightAxis.Maximum := MaxDeviation;
      chartModflow.RightAxis.Minimum := -MaxDeviation;}

      case FileType of
        ft_intconf:
          begin
            if Simultaneous then
            begin
              chartModflow.LeftAxis.Title.Caption
                := UpperCase('Nonlinear Simultaneous'#13'Confidence Intervals');
            end
            else
            begin
              chartModflow.LeftAxis.Title.Caption
                := UpperCase('Nonlinear Individual'#13'Confidence Intervals');
            end;
          end;
        ft_intpred:
            if Simultaneous then
            begin
              chartModflow.LeftAxis.Title.Caption
                := UpperCase('Nonlinear Simultaneous'#13'Prediction Intervals');
            end
            else
            begin
              chartModflow.LeftAxis.Title.Caption
                := UpperCase('Nonlinear Individual'#13'Prediction Intervals');
            end;
      else Assert(False);
      end;
    end;
  finally
    Intervals.Free;
    Values.Free;
    ParameterNames.Free;
    PredictionNames.Free;
  end;
  if WarningMessage <> '' then
  begin
    Beep;
    MessageDlg('The following files do not exist: '#13#10 + WarningMessage,
      mtWarning, [mbOK], 0);
  end;

end;

procedure TfrmModChart.sbRefreshClick(Sender: TObject);
begin
  inherited;
  OpenFile(NameOfFile);
end;

procedure TfrmModChart.btnIntConfBrowseClick(Sender: TObject);
begin
  inherited;
  if LowerCase(ExtractFileExt(NameOfFile)) = '._intconf' then
  begin
    odIntConf.Filter := '_intconf files (*._intconf)|*._intconf'
  end
  else
  begin
    odIntConf.Filter := '_intpred files (*._intpred)|*._intpred'
  end;
  if odIntConf.Execute then
  begin
    reIntconfFiles.Lines := odIntConf.Files;
    RedrawChart(nil);
  end;
end;

procedure TfrmModChart.MakeAxesVariablesDifferent;
var
  Index: integer;
begin
  for Index := 0 to rgXAxis.Items.Count -1 do
  begin
    rgYAxis.Controls[Index].Enabled := Index <> rgXAxis.ItemIndex;
  end;
  if rgXAxis.ItemIndex = rgYAxis.ItemIndex then
  begin
    if rgXAxis.ItemIndex = rgXAxis.Items.Count -1 then
    begin
      rgYAxis.ItemIndex := 0;
    end
    else
    begin
      rgYAxis.ItemIndex := rgXAxis.ItemIndex+1;
    end;
  end;
end;

procedure TfrmModChart.EnableXYZT_DataToPlot;
begin
  rgDataToPlot.Enabled := (rgXAxis.ItemIndex <= 3) and (rgYAxis.ItemIndex <= 3);
  seRadius.Enabled := rgDataToPlot.Enabled;
  seXyztLegendCount.Enabled := cbXyztLegend.Checked and rgDataToPlot.Enabled;
  rdgXyztLegend.Enabled := cbXyztLegend.Checked and rgDataToPlot.Enabled;
  lblBlueRedDescripton.Visible := rgDataToPlot.Enabled;
end;

procedure TfrmModChart.rgXAxisClick(Sender: TObject);
begin
  inherited;
  MakeAxesAutomatic := True;
  MakeAxesVariablesDifferent;
  EnableXYZT_DataToPlot;
  DelayRedraw;
//  RedrawChart(Sender);
end;

procedure TfrmModChart.Read_xyztwr;
var
  ALine: string;
  Values : TStringList;
  PlotSymbols: TIntegerList;
  Objects: TList;
  BubbleData: TBubbleData;
  Index: integer;
  ExistingPlotSymbols: TIntegerList;
  PlotSymbol, PlotIndex: integer;
  Color: TColor;
  FoundFirst: boolean;
  MaxValue: double;
  Mult: double;
  MinX, MaxX: double;
  MinY, MaxY: double;
  YRange: double;
  TimeLimit: double;
  MaxIndex: integer;
  Value: double;
  RowIndex: integer;
  Radius: double;
  MaxSymbolSize: integer;
  SymbolSize: integer;
  Residual: double;
  MaxResidual, MinResidual: double;
  PlotSymbolIndex: integer;
  ASeries: TPointSeries;
  ABubbleSeries: TBubbleSeries;
  ColorIndex: integer;
  IntList: TIntegerList;
  LastX: double;
  LastY: double;
  Function ShouldDraw(const BubbleData: TBubbleData): boolean;
  begin
    if rgDataToPlot.Enabled then
    begin
      PlotIndex := ExistingPlotSymbols.IndexOf(BubbleData.PlotSymbol);
      result := clbItemsToPlot.Checked[PlotIndex];
    end
    else
    begin
      result := True;
    end;
    if result then
    begin
      if cbEarliestTimePlotted.Checked then
      begin
        if TryStrToFloat(adeEarliest.Text, TimeLimit) then
        begin
          result := TimeLimit <= BubbleData.Coordinates[3];
        end;
      end;
    end;
    if result then
    begin
      if cbLatestTimePlotted.Checked then
      begin
        if TryStrToFloat(adeLatest.Text, TimeLimit) then
        begin
          result := TimeLimit >= BubbleData.Coordinates[3];
        end;
      end;
    end;
  end;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_xyztwr;
  MakeAxesVariablesDifferent;
  chartModflow.BottomAxis.LabelStyle := talValue;
  if rgDataToPlot.Enabled then
  begin
    chartModflow.Legend.TextStyle := ltsPlain;
    chartModflow.Legend.Series := serBubbleLegend;
  end;

  ExplantionVisible := True;
//  serBubble.Active := rgDataToPlot.Enabled;
//  serBubble.Marks.Transparent := False;
//  serBubble.Marks.Visible := cbShowLabels.Checked;
//  serBubble.ShowInLegend := cbXyztLegend.Checked;


  Values := TStringList.Create;
  PlotSymbols := TIntegerList.Create;
  ExistingPlotSymbols := TIntegerList.Create;
  Objects := TObjectList.Create;
  try
    PlotSymbols.Sorted := True;
    Values.Delimiter := ' ';
    // Skip first line
    ReadLn(FFile, ALine);
    while not EOF(FFIle) do
    begin
      ReadLn(FFile, ALine);
      if Trim(ALine) <> '' then
      begin
        Values.DelimitedText := ALine;
        BubbleData:= TBubbleData.Create;
        Objects.Add(BubbleData);
        BubbleData.Coordinates[0] := FortranStrToFloat(Values[0]);
        BubbleData.Coordinates[1] := FortranStrToFloat(Values[1]);
        BubbleData.Coordinates[2] := FortranStrToFloat(Values[2]);
        BubbleData.Coordinates[3] := FortranStrToFloat(Values[3]);
        BubbleData.Residuals[0] := FortranStrToFloat(Values[4]);
        BubbleData.Residuals[1] := FortranStrToFloat(Values[5]);
        BubbleData.PlotSymbol := StrToInt(Values[6]);
        BubbleData.Name := Values[7];
        if PlotSymbols.IndexOf(BubbleData.PlotSymbol) < 0 then
        begin
          PlotSymbols.Add(BubbleData.PlotSymbol);
        end;
      end;
    end;

    for Index := clbItemsToPlot.Items.Count -1 downto 0 do
    begin
      PlotSymbol := StrToInt(clbItemsToPlot.Items[Index]);
      if PlotSymbols.IndexOf(PlotSymbol) < 0 then
      begin
        clbItemsToPlot.Items.Delete(Index);
      end
      else
      begin
        ExistingPlotSymbols.Add(PlotSymbol);
      end;
    end;
    ExistingPlotSymbols.Sorted := True;

    for Index := 0 to PlotSymbols.Count -1 do
    begin
      PlotSymbol := PlotSymbols[Index];
      if ExistingPlotSymbols.IndexOf(PlotSymbol) < 0 then
      begin
        ExistingPlotSymbols.Add(PlotSymbol);
        PlotIndex := ExistingPlotSymbols.IndexOf(PlotSymbol);
        clbItemsToPlot.Items.Insert(PlotIndex, IntToStr(PlotSymbol));
        clbItemsToPlot.Checked[PlotIndex] := True;
      end;
    end;

    FoundFirst := False;

    MaxIndex := 0;
    for Index := 0 to Objects.Count -1 do
    begin
      BubbleData := Objects[Index];
      if ShouldDraw(BubbleData) then
      begin
        Residual := BubbleData.Residuals[rgDataToPlot.ItemIndex];
        Value := Abs(Residual);
        if FoundFirst then
        begin
          MaxValue := Max(MaxValue, Value);
          MinX := Min(MinX, BubbleData.Value(rgXAxis.ItemIndex));
          MaxX := Max(MaxX, BubbleData.Value(rgXAxis.ItemIndex));
          MinY := Min(MinY, BubbleData.Value(rgYAxis.ItemIndex));
          MaxY := Max(MaxY, BubbleData.Value(rgYAxis.ItemIndex));
          MinResidual := Min(MinResidual, Residual);
          MaxResidual := Max(MaxResidual, Residual);
        end
        else
        begin
          MaxValue := Value;
          MinX := BubbleData.Value(rgXAxis.ItemIndex);
          MaxX := MinX;
          MinY := BubbleData.Value(rgYAxis.ItemIndex);
          MaxY := MinY;
          MaxResidual := Residual;
          MinResidual := Residual;
          FoundFirst := True;
        end;
      end;
    end;

    if MaxValue <> 0 then
    begin
//      YRange := Max((MaxY-MinY), Sqrt(MaxValue));
      YRange := (MaxY-MinY);
      if YRange = 0 then
      begin
        YRange := Sqrt(MaxValue);
      end;

      Mult := YRange*seRadius.Value/Sqrt(MaxValue);
    end
    else
    begin
      Mult := 1;
    end;

    if rgDataToPlot.Enabled then
    begin
      chartModflow.Legend.LegendStyle := lsValues;
      for PlotSymbolIndex := 0 to clbItemsToPlot.Items.Count -1 do
      begin
        PlotSymbol := StrToInt(clbItemsToPlot.Items[PlotSymbolIndex]);
        ABubbleSeries := TBubbleSeries.Create(nil);
        ABubbleSeries.ParentChart := chartModflow;
        SeriesList.InsertObject(PlotSymbolIndex, IntToStr(PlotSymbol), ABubbleSeries);
        ABubbleSeries.Active := True;
        ABubbleSeries.Visible := clbItemsToPlot.Checked[PlotSymbolIndex];
        ABubbleSeries.Title := IntToStr(PlotSymbol);
        ABubbleSeries.ShowInLegend := False;
        ABubbleSeries.Marks.Transparent := False;
        ABubbleSeries.Marks.Visible := cbShowLabels.Checked;

        ColorIndex := PlotSymbol-1;
        ColorIndex := ColorIndex mod (Length(ColorPalette));
        if ColorIndex < 0 then
        begin
          ColorIndex := ColorIndex + Length(ColorPalette)
        end;

        Color := ColorPalette[ColorIndex];
        ABubbleSeries.SeriesColor := Color;

        ABubbleSeries.OnClickPointer := SerDataPointsClickPointer;
        ABubbleSeries.OnGetMarkText := SerDataPointsGetMarkText;

        IntList := TIntegerList.Create;
        IntList.Sorted := True;
        FMarkedPointsList.Insert(PlotSymbolIndex, IntList);

      end;
    // bubble plot
      for Index := 0 to Objects.Count -1 do
      begin
        BubbleData := Objects[Index];
        if ShouldDraw(BubbleData) then
        begin
          Value := BubbleData.Residuals[rgDataToPlot.ItemIndex];
        end
        else
        begin
          Value := 0;
        end;
          if Value > 0 then
          begin
            Color := clBlue;
          end
          else
          begin
            Color := clRed;
          end;
          PlotIndex := ExistingPlotSymbols.IndexOf(BubbleData.PlotSymbol);
          ABubbleSeries := SeriesList.Objects[PlotIndex] as TBubbleSeries;
          Assert(StrToInt(ABubbleSeries.Title) = BubbleData.PlotSymbol);
//
          ABubbleSeries.AddBubble(BubbleData.Coordinates[rgXAxis.ItemIndex],
            BubbleData.Coordinates[rgYAxis.ItemIndex],
            Sqrt(Abs(Value))*Mult,
            BubbleData.Name, Color);

//          serBubbleLegend.AddBubble(BubbleData.Coordinates[rgXAxis.ItemIndex],
//            BubbleData.Coordinates[rgYAxis.ItemIndex],
//            0,
//            BubbleData.Name, Color);

          if ShouldDraw(BubbleData) then
          begin
            LastX := BubbleData.Coordinates[rgXAxis.ItemIndex];
            LastY := BubbleData.Coordinates[rgYAxis.ItemIndex];
          end;

  //      end;
      end;
      if cbXyztLegend.Checked then
      begin
        serBubbleLegend.Clear;
        MaxSymbolSize := 0;
        for RowIndex := 1 to rdgXyztLegend.RowCount -1 do
        begin
          if TryStrToFloat(rdgXyztLegend.Cells[0, RowIndex], Value) then
          begin
            if Value > 0 then
            begin
              Color := clBlue;
            end
            else
            begin
              Color := clRed;
            end;
            Radius := Sqrt(Abs(Value))*Mult;
            serBubbleLegend.AddBubble(LastX, LastY,
              Radius,
              rdgXyztLegend.Cells[0, RowIndex], Color);
            GetSymbolSize(Radius, SymbolSize);
            if MaxSymbolSize < SymbolSize then
            begin
              MaxSymbolSize := SymbolSize;
            end;
          end;
        end;
        chartModflow.Legend.Visible :=
          serBubbleLegend.RadiusValues.Count > 0;
        if chartModflow.Legend.Visible then
        begin
          chartModflow.Legend.Symbol.Squared := True;
          serBubbleLegend.ShowInLegend := True;
          serBubbleLegend.Active := True;
          serBubbleLegend.Visible := False;
        end;
      end
      else
      begin
        chartModflow.Legend.Visible := False;
        rdgXyztLegend.Cells[0,1] := FloatToStr(MaxResidual);
        rdgXyztLegend.Cells[0,2] := FloatToStr(MinResidual);

      end;
    end
    else
    begin
      chartModflow.Legend.Visible := cbXyztLegend.Checked;

      cbLabelPoints.Checked := True;
      for PlotSymbolIndex := 0 to clbItemsToPlot.Items.Count -1 do
      begin
        PlotSymbol := StrToInt(clbItemsToPlot.Items[PlotSymbolIndex]);
        ASeries := TPointSeries.Create(nil);
        ASeries.ParentChart := chartModflow;
        SeriesList.InsertObject(PlotSymbolIndex, IntToStr(PlotSymbol), ASeries);
        ASeries.Visible := clbItemsToPlot.Checked[PlotSymbolIndex];
        ASeries.Title := IntToStr(PlotSymbol);
        ASeries.ShowInLegend := cbXyztLegend.Checked;
        ASeries.Marks.Visible := cbShowLabels.Checked;

        ColorIndex := PlotSymbol-1;
        ColorIndex := ColorIndex mod (Length(ColorPalette));
        if ColorIndex < 0 then
        begin
          ColorIndex := ColorIndex + Length(ColorPalette)
        end;

        Color := ColorPalette[ColorIndex];
        ASeries.SeriesColor := Color;

        ASeries.OnClickPointer := SerDataPointsClickPointer;
        ASeries.OnGetMarkText := SerDataPointsGetMarkText;

        IntList := TIntegerList.Create;
        IntList.Sorted := True;
        FMarkedPointsList.Insert(PlotSymbolIndex, IntList);

      end;
      for Index := 0 to Objects.Count -1 do
      begin
        BubbleData := Objects[Index];
        if ShouldDraw(BubbleData) then
        begin
          PlotIndex := ExistingPlotSymbols.IndexOf(BubbleData.PlotSymbol);
          ASeries := SeriesList.Objects[PlotIndex] as TPointSeries;
          Assert(StrToInt(ASeries.Title) = BubbleData.PlotSymbol);
          ASeries.AddXY(BubbleData.Value(rgXAxis.ItemIndex),
            BubbleData.Value(rgYAxis.ItemIndex), BubbleData.Name);

        end;

      end;


    end;                      
  finally
    Values.Free;
    PlotSymbols.Free;
    Objects.Free;
    ExistingPlotSymbols.Free;
  end;
end;

{ TBubbleData }

function TBubbleData.Value(Index: Integer): double;
begin
  if Index <= 3 then
  begin
    result := Coordinates[Index];
  end
  else
  begin
    result := Residuals[Index-4];
  end;
end;


procedure TfrmModChart.cbEarliestTimePlottedClick(Sender: TObject);
begin
  inherited;
  adeEarliest.Enabled := cbEarliestTimePlotted.Checked;
  RedrawChart(Sender);
end;

procedure TfrmModChart.cbLatestTimePlottedClick(Sender: TObject);
begin
  inherited;
  adeLatest.Enabled := cbLatestTimePlotted.Checked;
  RedrawChart(Sender);
end;

procedure TfrmModChart.Timer1Timer(Sender: TObject);
begin
  inherited;
  Timer1.Enabled := False;
  RedrawChart(Sender);
end;

procedure TfrmModChart.DelayRedrawChart(Sender: TObject);
begin
  inherited;
  if Timer1 <> nil then
  begin
    Timer1.Enabled := False;
    Timer1.Enabled := True;
  end;
end;

procedure TfrmModChart.SetNameOfFile(const Value: string);
begin
  if FNameOfFile = Value then
  begin
    MakeAxesAutomatic := False;
  end
  else
  begin
    MakeAxesAutomatic := True;
    FNameOfFile := Value;
    rgItemsToPlot_So.ItemIndex := 0;
  end;
end;

procedure TfrmModChart.rgYAxisClick(Sender: TObject);
begin
  inherited;
  MakeAxesAutomatic := True;
  EnableXYZT_DataToPlot;
  DelayRedraw;
//  RedrawChart(Sender);
end;

procedure TfrmModChart.rgItemsToPlot_SoClick(Sender: TObject);
begin
  inherited;
  seN_so.Enabled := rgItemsToPlot_So.ItemIndex > 0;
  RedrawChart(Sender);
end;

procedure TfrmModChart.rgItemsToPlot_RcClick(Sender: TObject);
begin
  inherited;
  seN_Rc.Enabled := rgItemsToPlot_Rc.ItemIndex > 0;
  RedrawChart(Sender);
end;

procedure TfrmModChart.rgItemsToPlotPPRClick(Sender: TObject);
begin
  inherited;
  seN_PPR.Enabled := rgItemsToPlotPPR.ItemIndex > 0;
  RedrawChart(Sender);
end;

procedure TfrmModChart.rgItemsToPlotAbsChgClick(Sender: TObject);
begin
  inherited;
  seN_AbsChg.Enabled := rgItemsToPlotAbsChg.ItemIndex > 0;
  RedrawChart(Sender);
end;

procedure TfrmModChart.rbPlotAverageClick(Sender: TObject);
begin
  inherited;
  rgOrderPPR.Enabled := True;
  RedrawChart(Sender);
end;

procedure TfrmModChart.rbPlotSeriesClick(Sender: TObject);
begin
  inherited;
  rgOrderPPR.ItemIndex := 0;
  rgOrderPPR.Enabled := False;
  RedrawChart(Sender);

end;

procedure TfrmModChart.rgSeriesToPlotClick(Sender: TObject);
begin
  inherited;
  if FReadingSD then Exit;

  seNumberOfSeries.Enabled := rgSeriesToPlot.ItemIndex = 1;
  clbSeriesToPlot.Enabled  := rgSeriesToPlot.ItemIndex = 2;
  RedrawChart(Sender);
end;

procedure TfrmModChart.clbSeriesToPlotClickCheck(Sender: TObject);
begin
  inherited;
  if FReadingSD then Exit;
  RedrawChart(Sender);
end;

procedure TfrmModChart.clbLinpPlotSymbolClickCheck(Sender: TObject);
begin
  inherited;
  if FPlottingLinp then
  begin
    Exit;
  end;
  self.RedrawChart(Sender);
end;

procedure TfrmModChart.GetSymbolSize(Const Radius: double; var SymbolSize: Integer);
begin
  SymbolSize:=serBubbleLegend.GetVertAxis.CalcSizeValue(Radius);
end;

procedure TfrmModChart.chartModflowGetSymbolSize(Sender: TCustomChart;
  const ValueIndex: integer; var SymbolSize: Integer);
var
  Radius: double;
begin
  inherited;
  if (FileType = ft_xyztwr) then
  begin
    if ValueIndex < serBubbleLegend.RadiusValues.Count then
    begin
      Radius := serBubbleLegend.RadiusValues.Value[ValueIndex];
      GetSymbolSize(Radius, SymbolSize);
    end;
  end;
end;


procedure TfrmModChart.seXyztLegendCountChange(Sender: TObject);
begin
  inherited;
  rdgXyztLegend.RowCount := seXyztLegendCount.AsInteger + 1;
  ReplotXyztIfLegendComplete;
end;

procedure TfrmModChart.ReplotXyztIfLegendComplete;
var
  RowIndex: integer;
begin
  if cbXyztLegend.Checked then
  begin
    for RowIndex := 1 to rdgXyztLegend.RowCount -1 do
    begin
      if rdgXyztLegend.Cells[0,RowIndex] = '' then
      begin
        Exit;
      end;
    end;
  end;
  UpdateXyztLegendAfterDelay;
end;

procedure TfrmModChart.cbXyztLegendClick(Sender: TObject);
begin
  inherited;
  seXyztLegendCount.Enabled := cbXyztLegend.Checked and rgDataToPlot.Enabled;
  rdgXyztLegend.Enabled := cbXyztLegend.Checked and rgDataToPlot.Enabled;
  if rgDataToPlot.Enabled then
  begin
    if rdgXyztLegend.Enabled then
    begin
      rdgXyztLegend.Color := clWindow;
    end
    else
    begin
      rdgXyztLegend.Color := clBtnFace;
    end;

    ReplotXyztIfLegendComplete;
  end
  else
  begin
    RedrawChart(nil);
  end;
end;

procedure TfrmModChart.rdgXyztLegendEndUpdate(Sender: TObject);
begin
  inherited;
  if seXyztLegendCount.AsInteger <> rdgXyztLegend.RowCount -1 then
  begin
    seXyztLegendCount.AsInteger := rdgXyztLegend.RowCount -1;
    ReplotXyztIfLegendComplete;
  end;
end;

procedure TfrmModChart.UpdateXyztLegendAfterDelay;
begin
//  if cbXyztLegend.Checked then
  begin
    Timer1.Enabled := False;
    Timer1.Enabled := True;
  end;
end;

procedure TfrmModChart.FormResize(Sender: TObject);
begin
  inherited;
  if FileType = ft_xyztwr then
  begin
    UpdateXyztLegendAfterDelay;
  end;
end;

procedure TfrmModChart.rdgXyztLegendSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  inherited;
  ReplotXyztIfLegendComplete;
end;

procedure TfrmModChart.jvchklstParametersClickCheck(Sender: TObject);
var
  ASeries: TBarSeries;
  Index: integer;
begin
  inherited;
  for Index := 0 to jvchklstParameters.Items.Count -1 do
  begin
    ASeries := jvchklstParameters.Items.Objects[Index] as TBarSeries;
    ASeries.Visible := jvchklstParameters.Checked[Index]
  end;

end;

procedure TfrmModChart.cbRatioClick(Sender: TObject);
begin
  inherited;
  OpenFile(NameOfFile);
end;

{ TGroup }

procedure TGroup.AddParam(Value: TGroupParam);
begin
  FParameters.Add(Value);
end;

constructor TGroup.Create;
begin
  FParameters:= TObjectList.Create;
end;

destructor TGroup.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TGroup.GetCount: Integer;
begin
  result := FParameters.Count;
end;

function TGroup.GetParam(Index: Integer): TGroupParam;
begin
  result := FParameters[index];
end;

procedure TGroup.SetParam(Index: Integer; const Value: TGroupParam);
begin
  FParameters[index] := Value;
end;

procedure TGroup.SortParameters(ValueList: TList);
var
  NameList: TStringList;
  BarValue: TBarValue;
  NameIndex: integer;
  NewParameters: TList;
  AParam: TGroupParam;
  ParamIndex: integer;
begin
  // ValueList is already sorted and contains instances of TBarValue.
  NameList := TStringList.Create;
  try
    NameList.Capacity := ValueList.Count;
    NameList.CaseSensitive := False;
    for NameIndex := 0 to ValueList.Count -1 do
    begin
      BarValue := ValueList[NameIndex];
      NameList.Add(BarValue.ParameterName)
    end;
    
    NewParameters := TObjectList.Create;
    (FParameters as TObjectList).OwnsObjects := False;
    NewParameters.Count := FParameters.Count;
    for ParamIndex := 0 to ParamCount -1 do
    begin
      AParam := Parameters[ParamIndex];
      NameIndex := NameList.IndexOf(AParam.ParameterName);
      Assert(NameIndex >= 0);
      Assert(NameIndex < NewParameters.Count);
      NewParameters[NameIndex] := AParam;
    end;
    FParameters.Free;
    FParameters := NewParameters;
  finally
    NameList.Free;
  end;

end;

procedure TfrmModChart.GetFileSeriesNames(FileNames: TStringList;
  const FileName, SearchExtension : string);
var
  FileSearch: string;
  FileDir: string;
  PrefixLength: integer;
  AFileName: string;
  FileNumber: string;
  AnInt: integer;
  SearchRec: TSearchRec;
begin
  FileSearch := ChangeFileExt(FileName, SearchExtension);
  FileDir := ExtractFileDir(FileName);
  FileDir := IncludeTrailingPathDelimiter(FileDir);
  PrefixLength := Length(ExtractFileName(FileSearch));
  FileSearch := FileSearch + '*';
  try
    if FindFirst(FileSearch, faAnyFile, SearchRec) = 0 then
    begin
      AFileName := SearchRec.Name;
      FileNumber := Copy(AFileName, PrefixLength+1, MaxInt);
      if TryStrToInt(FileNumber, AnInt) then
      begin
        FileNames.Add(FileDir + AFileName);
      end;
      while FindNext(SearchRec) = 0 do
      begin
        AFileName := SearchRec.Name;
        FileNumber := Copy(AFileName, PrefixLength+1, MaxInt);
        if TryStrToInt(FileNumber, AnInt) then
        begin
          FileNames.Add(FileDir + AFileName);
        end;
      end;
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TfrmModChart.AssignHistogramValues(AValuesList: TRealList;
  BinCount: Integer; AHistogram: THistogramSeries);
var
  MinValue: double;
  MaxValue: double;
  PriorPositionIndex: Integer;
  BinIndex: integer;
  BreakValue: double;
  PositionIndex: integer;
  TotalCount: Integer;
  CountInBin: Integer;
begin
  TotalCount := 0;
  AValuesList.Sort;
  MinValue := AValuesList.First;
  MaxValue := AValuesList.Last;
  PriorPositionIndex := -1;
  for BinIndex := 1 to BinCount do
  begin
    if BinIndex < BinCount then
    begin
      BreakValue := (MaxValue-MinValue)*BinIndex/BinCount + MinValue;
      PositionIndex := AValuesList.IndexOfClosest(BreakValue);
      While AValuesList[PositionIndex] > BreakValue do
      begin
        Dec(PositionIndex);
      end;
    end
    else
    begin
      PositionIndex := AValuesList.Count -1;
    end;
    CountInBin := PositionIndex - PriorPositionIndex;
    PriorPositionIndex := PositionIndex;
    AHistogram.AddXY(BreakValue, CountInBin/AValuesList.Count);
    TotalCount := TotalCount + CountInBin;
  end;
  Assert(TotalCount = AValuesList.Count);
end;

procedure TfrmModChart.Read_mcmc_par(const FileName : string);
var
//  FileSearch: string;
//  PrefixLength: integer;
  FileNames: TStringList;
//  SearchRec: TSearchRec;
  AFileName: string;
//  FileNumber: string;
//  AnInt: integer;
  LineSplitter: TStringList;
//  FileDir: string;
  McmcFile: TStringList;
  FileIndex: Integer;
  VariableIndex: integer;
  AVariableName: string;
  VarPosition: integer;
  LineIndex: integer;
//  ALineSeries: TLineSeries;
  AList: TList;
  ASeries: TLineSeries;
  ATitle: string;
  LocalSeriesList: TList;
  IterationNumber: integer;
  AValue: Double;
  AHistogram: THistogramSeries;
  ValueLists: TList;
  AValuesList: TRealList;
  AFraction: double;
//  MinValue: double;
//  MaxValue: double;
  BinCount: Integer;
//  BinIndex: integer;
//  BreakValue: double;
//  PositionIndex: Integer;
//  PriorPositionIndex: integer;
  SeriesIndex: Integer;
//  TotalCount: Integer;
//  CountInBin: integer;
  StartLine: integer;
  SelectedParameter: string;
begin
  chartModflow.LeftAxis.Automatic := True;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_mcmc_par;
  if Fmcmc_Variables = nil then
  begin
    Fmcmc_Variables := TObjectStringList.Create;
  end
  else
  begin
    Fmcmc_Variables.Clear;
  end;
  if lst_mcmcParameters.ItemIndex >= 0 then
  begin
    SelectedParameter :=
      lst_mcmcParameters.Items[lst_mcmcParameters.ItemIndex];
  end
  else
  begin
    SelectedParameter := '';
  end;


  lst_mcmcParameters.Items.Clear;
  FileNames := TStringList.Create;
  try
    GetFileSeriesNames(FileNames, FileName, '._mcmc_par');


    LocalSeriesList := TList.Create;
    LineSplitter := TStringList.Create;
    McmcFile := TStringList.Create;
    ValueLists := TObjectList.Create;
    try
      LineSplitter.Delimiter := ' ';
      LineSplitter.QuoteChar := '"';

      for FileIndex := 0 to FileNames.Count -1 do
      begin
        AFileName := FileNames[FileIndex];
        McmcFile.LoadFromFile(AFileName);
        Assert(Pos('MONITORED PARAMETER SAMPLE VALUES', McmcFile[0]) > 0);
        if rgMcmcGraphType.ItemIndex = 0 then
        begin
            // Line series

          LineSplitter.DelimitedText := McmcFile[1];

          LocalSeriesList.Clear;
          for VariableIndex := 2 to LineSplitter.Count -1 do
          begin
            AVariableName := Trim(LineSplitter[VariableIndex]);
            VarPosition := Fmcmc_Variables.IndexOf(AVariableName);
            if VarPosition < 0 then
            begin
              Assert(FileIndex = 0);
              VarPosition := Fmcmc_Variables.AddObject(AVariableName, TList.Create);
              lst_mcmcParameters.Items.Add(AVariableName);
            end
            else
            begin
              Assert(FileIndex > 0);
            end;
            Assert(VarPosition = VariableIndex-2);

            AList := Fmcmc_Variables.Objects[VarPosition] as TList;

            ATitle := ExtractFileName(FileNames[FileIndex]);
            ASeries := TLineSeries.Create(self);
            ASeries.XValues.Order := loNone;
            SeriesList.AddObject(ATitle, ASeries);
            ASeries.ParentChart := chartModflow;
            ASeries.Title := ATitle;
            ASeries.Visible := False;
            AList.Add(ASeries);
            LocalSeriesList.Add(ASeries);
          end;

          for LineIndex := 2 to McmcFile.Count-1 do
          begin
            LineSplitter.DelimitedText := McmcFile[LineIndex];
            if LineSplitter.Count > 0 then
            begin
              Assert(LineSplitter.Count = Fmcmc_Variables.Count +2);
              IterationNumber := StrToInt(Trim(LineSplitter[0]));
              for VariableIndex := 2 to LineSplitter.Count -1 do
              begin
                VarPosition := VariableIndex-2;
                ASeries := LocalSeriesList[VarPosition];
                AValue := FortranStrToFloat(LineSplitter[VariableIndex]);
                ASeries.AddXY(IterationNumber, AValue);
              end;
            end;
          end;
        end
        else
        begin
          // HistogramSeries
          AFraction := seMcmcPercent.Value/100;
          LineSplitter.DelimitedText := McmcFile[1];

//            LocalSeriesList.Clear;
          for VariableIndex := 2 to LineSplitter.Count -1 do
          begin
            AVariableName := Trim(LineSplitter[VariableIndex]);
            VarPosition := Fmcmc_Variables.IndexOf(AVariableName);
            if VarPosition < 0 then
            begin
              Assert(FileIndex = 0);
              VarPosition := Fmcmc_Variables.AddObject(AVariableName, TList.Create);
              lst_mcmcParameters.Items.Add(AVariableName);

              AList := Fmcmc_Variables.Objects[VarPosition] as TList;

              ATitle := AVariableName;
              AHistogram := THistogramSeries.Create(self);
              AHistogram.XValues.Order := loNone;
              SeriesList.AddObject(ATitle, AHistogram);
              AHistogram.ParentChart := chartModflow;
              AHistogram.Title := ATitle;
              AHistogram.Visible := False;
              AList.Add(AHistogram);
              LocalSeriesList.Add(AHistogram);

              AValuesList := TRealList.Create;
              ValueLists.Add(AValuesList);
              AValuesList.Capacity :=
                Round(AFraction * (McmcFile.Count * FileNames.Count));

            end
            else
            begin
              Assert(FileIndex > 0);
            end;
            Assert(VarPosition = VariableIndex-2);
            AValuesList := ValueLists[VarPosition];
          end;

          StartLine := McmcFile.Count - Round((McmcFile.Count-2)*AFraction);
          StartLine := Max(2, StartLine);
          for LineIndex := StartLine to McmcFile.Count-1 do
          begin
            LineSplitter.DelimitedText := McmcFile[LineIndex];
            if LineSplitter.Count > 0 then
            begin
              Assert(LineSplitter.Count = Fmcmc_Variables.Count +2);
//                  IterationNumber := StrToInt(Trim(LineSplitter[0]));
              for VariableIndex := 2 to LineSplitter.Count -1 do
              begin
                VarPosition := VariableIndex-2;
                AValuesList := ValueLists[VarPosition];
//                    ASeries := LocalSeriesList[VarPosition];
                AValue := FortranStrToFloat(LineSplitter[VariableIndex]);
                AValuesList.Add(AValue);
              end;
            end;
          end
        end;
      end;
      if rgMcmcGraphType.ItemIndex = 1 then
      begin
        Assert(LocalSeriesList.Count = ValueLists.Count);
        for SeriesIndex := 0 to LocalSeriesList.Count -1 do
        begin
          AHistogram := LocalSeriesList[SeriesIndex];
          AValuesList := ValueLists[SeriesIndex];
          BinCount := seMcmcBins.AsInteger;

          AssignHistogramValues(AValuesList, BinCount, AHistogram);
//          TotalCount := 0;
//          AValuesList.Sort;
//          MinValue := AValuesList.First;
//          MaxValue := AValuesList.Last;
//          PriorPositionIndex := -1;
//          for BinIndex := 1 to BinCount do
//          begin
//            if BinIndex < BinCount then
//            begin
//              BreakValue := (MaxValue-MinValue)*BinIndex/BinCount + MinValue;
//              PositionIndex := AValuesList.IndexOfClosest(BreakValue);
//              While AValuesList[PositionIndex] > BreakValue do
//              begin
//                Dec(PositionIndex);
//              end;
//            end
//            else
//            begin
//              PositionIndex := AValuesList.Count -1;
//            end;
//            CountInBin := PositionIndex - PriorPositionIndex;
//            PriorPositionIndex := PositionIndex;
//            AHistogram.AddXY(BreakValue, CountInBin/AValuesList.Count);
//            TotalCount := TotalCount + CountInBin;
//          end;
//          Assert(TotalCount = AValuesList.Count);
        end;

      end;

    finally
      LineSplitter.Free;
      McmcFile.Free;
      LocalSeriesList.Free;
      ValueLists.Free;
    end;

  finally
    FileNames.Free;
  end;

  if SelectedParameter <> '' then
  begin
    lst_mcmcParameters.ItemIndex :=
      lst_mcmcParameters.Items.IndexOf(SelectedParameter);
  end;

  if lst_mcmcParameters.ItemIndex < 0 then
  begin
    lst_mcmcParameters.ItemIndex := 0;
  end;
  lst_mcmcParameters.Checked[lst_mcmcParameters.ItemIndex] := True;
  lst_mcmcParametersClickCheck(nil);
end;

procedure TfrmModChart.rgMcmcGraphTypeClick(Sender: TObject);
begin
  inherited;
  seMcmcPercent.Enabled := rgMcmcGraphType.ItemIndex = 1;
  seMcmcBins.Enabled := rgMcmcGraphType.ItemIndex = 1;
  RedrawChart(nil);
end;

procedure TfrmModChart.lst_mcmcParametersClickCheck(Sender: TObject);
var
  ListIndex: integer;
  AList: TList;
  ASeries: TChartSeries;
  SeriesIndex: integer;
  MaxFound: boolean;
  PointIndex: integer;
  AValue: double;
  MaxValue: double;
  TrueMax: double;
  TrueMaxFound: boolean;
  TwentyPercentPoints: integer;
begin
  inherited;
  chartModflow.LeftAxis.Automatic := True;
  if Fmcmc_Variables <> nil then
  begin
    MaxFound := False;
    TrueMaxFound := False;
    for ListIndex := 0 to Fmcmc_Variables.count -1 do
    begin
      AList := Fmcmc_Variables.Objects[ListIndex] as TList;
      for SeriesIndex := 0 to AList.Count -1 do
      begin
        ASeries := AList[SeriesIndex];

        if rgMcmcGraphType.ItemIndex = 1 then
        begin
          // This prevents a range check error when you click on the chart.
          // The range check error is due to a bug in THistogramSeries.
          // THistogramSeries doesn't calculate the visible points until
          // it has been displayed but tries to use an unitialized value
          // of THistogramSeries.FFirstVisibleIndex.
          // Thus, this is a work-around and not a fix.
          ASeries.Visible := True;
          Application.ProcessMessages;
        end;          
        ASeries.Visible := ListIndex = lst_mcmcParameters.ItemIndex;
        if ASeries.Visible and (ASeries is TLineSeries) then
        begin
          TwentyPercentPoints := ASeries.YValues.Count div 5;
          for PointIndex := 0 to ASeries.Count -1 do
          begin
            AValue := ASeries.YValues[PointIndex];
            if TrueMaxFound then
            begin
              if AValue > TrueMax then
              begin
                TrueMax := AValue;
              end;
            end
            else
            begin
              TrueMax := AValue;
              TrueMaxFound := True;
            end;
            if PointIndex >= TwentyPercentPoints then
            begin
              if MaxFound then
              begin
                if AValue > MaxValue then
                begin
                  MaxValue := AValue;
                end;
              end
              else
              begin
                MaxValue := AValue;
                MaxFound := True;
              end;
            end;
          end;
        end;
      end;
    end;
    if lst_mcmcParameters.ItemIndex >= 0 then
    begin
      if rgMcmcGraphType.ItemIndex = 0 then
      begin
        chartModflow.Legend.Visible := True;
        chartModflow.LeftAxis.Title.Caption :=
          lst_mcmcParameters.Items[lst_mcmcParameters.ItemIndex];
      end
      else
      begin
        chartModflow.Legend.Visible := False;
        chartModflow.BottomAxis.Title.Caption :=
          lst_mcmcParameters.Items[lst_mcmcParameters.ItemIndex];
      end;
      if MaxFound and (MaxValue > 0) {and (MaxValue > chartModflow.LeftAxis.Minimum)} then
      begin
        chartModflow.LeftAxis.AutomaticMaximum := False;
        chartModflow.LeftAxis.Minimum := 0;
        chartModflow.LeftAxis.Maximum :=
          Min(TrueMax, (MaxValue * 1.5));
      end;
    end
    else
    begin
      chartModflow.LeftAxis.Title.Caption := '';
    end;
  end;
end;

procedure TfrmModChart.Read_mcmc_grr;
var
  ALine: string;
  Splitter: TStringList;
  ASeries: TLineSeries;
  VarIndex: Integer;
  ATitle: string;
  AList: TList;
  AValue: double;
  Iteration: integer;
  ThresholdSeries: TLineSeries;
  Threshold: Double;
begin
  chartModflow.LeftAxis.AutomaticMaximum := False;
  chartModflow.LeftAxis.Maximum := 2;
  chartModflow.LeftAxis.AutomaticMinimum := False;
  chartModflow.LeftAxis.Minimum := 1;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_mcmc_grr;
  Readln(FFile, ALine);
  Assert(Pos('MONITORED PARAMETER INTER-CHAINS GELMAN RUBIN R STATISTIC', ALine) > 0);
  Splitter := TStringList.Create;
  AList := TList.Create;
  try
    Splitter.QuoteChar := '"';
    Splitter.Delimiter := ' ';
    Readln(FFile, ALine);

    ATitle := 'RThreshold';
    ThresholdSeries := TLineSeries.Create(self);
    ThresholdSeries.XValues.Order := loNone;
    SeriesList.AddObject(ATitle, ThresholdSeries);
    ThresholdSeries.ParentChart := chartModflow;
    ThresholdSeries.Title := ATitle;
    ThresholdSeries.Visible := True;
    ThresholdSeries.LinePen.Style := psDash;
    ThresholdSeries.ShowInLegend := False;
    ThresholdSeries.Color := clBlack;


    Splitter.DelimitedText := ALine;
    for VarIndex := 1 to Splitter.Count -1 do
    begin
      ATitle := Trim(Splitter[VarIndex]);
      ASeries := TLineSeries.Create(self);
      ASeries.XValues.Order := loNone;
      SeriesList.AddObject(ATitle, ASeries);
      ASeries.ParentChart := chartModflow;
      ASeries.Title := ATitle;
      ASeries.Visible := True;
      AList.Add(ASeries);
    end;

    While not EOF(FFile) do
    begin
      Readln(FFile, ALine);
      Splitter.DelimitedText := ALine;
      Iteration := StrToInt(Splitter[0]);
      for VarIndex := 0 to AList.Count -1 do
      begin
        ASeries := AList[VarIndex];
        AValue := FortranStrToFloat(Splitter[VarIndex+1]);
        ASeries.AddXY(Iteration, AValue);
      end;
    end;

    ASeries := AList[0];
    Threshold := StrToFloat(rdeRThreshold.Text);
    ThresholdSeries.AddXY(ASeries.XValues.First, Threshold);
    ThresholdSeries.AddXY(ASeries.XValues.Last, Threshold);
  finally
    Splitter.Free;
    AList.Free;
  end;          
end;

procedure TfrmModChart.Read_mcmc_pred(const FileName: string);
var
  FileNames: TStringList;
  FileIndex: integer;
  AFile: TStringList;
  Splitter: TStringList;
  VarIndex: Integer;
  VarName: string;
  ValueLists: TList;
  AValuesList: TRealList;
  ATitle: string;
  AHistogram: THistogramSeries;
  BinCount: integer;
  LineIndex: integer;
  AValue: double;
  AName: string;
  ConfidenceInterval: double;
  Fraction: double;
  Mean: double;
  HiRange: Double;
  LowRange: Double;
  Low_CI: Double;
  High_CI: Double;
  ValueIndex: integer;
  SelectedPrediction: string;
begin
  chartModflow.Legend.Visible := False;
  if chcklstbx_Mcmc_pred.ItemIndex >= 0 then
  begin
    SelectedPrediction :=
      chcklstbx_Mcmc_pred.Items[chcklstbx_Mcmc_pred.ItemIndex];
  end
  else
  begin
    SelectedPrediction := '';
  end;

  chcklstbx_Mcmc_pred.Items.Clear;
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_mcmc_pred;
  RangeSeries.Visible := rgMcmc_predGraphType.ItemIndex = 1;
//  RangeSeries.RangeStyle := rsBarGraph;
  if Fmcmc_Variables = nil then
  begin
    Fmcmc_Variables := TObjectStringList.Create;
  end
  else
  begin
    Fmcmc_Variables.Clear;
  end;

  FileNames := TStringList.Create;
  AFile := TStringList.Create;
  Splitter := TStringList.Create;
  ValueLists := TObjectList.Create;
  try
    GetFileSeriesNames(FileNames, FileName, '._mcmc_pred');

    Splitter.QuoteChar := '"';
    Splitter.Delimiter := ' ';
    for FileIndex := 0 to FileNames.Count -1 do
    begin
      AFile.LoadFromFile(FileNames[FileIndex]);
      Assert(Pos('PREDICTION VALUES CALCULATED FROM PARAMETER SAMPLES', AFile[0]) > 0);

      Splitter.DelimitedText := AFile[1];

      for VarIndex := 1 to Splitter.Count -1 do
      begin
        VarName := Trim(Splitter[VarIndex]);
        if chcklstbx_Mcmc_pred.Items.IndexOf(VarName) < 0 then
        begin
          AValuesList := TRealList.Create;
          ValueLists.Add(AValuesList);
          chcklstbx_Mcmc_pred.Items.Add(VarName);

          AValuesList.Capacity := AFile.Count * FileNames.Count;
        end;
      end;

      for LineIndex := 2 to AFile.Count -1 do
      begin
        Splitter.DelimitedText := AFile[LineIndex];
        for VarIndex := 1 to Splitter.Count -1 do
        begin
          AValuesList := ValueLists[VarIndex-1];
          AValue := FortranStrToFloat(Splitter[VarIndex]);
          AValuesList.Add(AValue);

        end;
      end;
    end;

    BinCount := se_mcmc_prdBinCount.AsInteger;
    case rgConfidenceInterval.ItemIndex of
      0: ConfidenceInterval := 0.9;
      1: ConfidenceInterval := 0.95;
      2: ConfidenceInterval := 0.99;
      3: ConfidenceInterval := StrToFloat(rdeConfidenceInterval.Text)/100;
    else Assert(False);
    end;

    Fraction := (1-ConfidenceInterval)/2;

    for VarIndex := 0 to ValueLists.Count -1 do
    begin
      AValuesList := ValueLists[VarIndex];

      if rgMcmc_predGraphType.ItemIndex = 0 then
      begin
        ATitle := chcklstbx_Mcmc_pred.Items[VarIndex];
        AHistogram := THistogramSeries.Create(self);
        AHistogram.XValues.Order := loNone;
        SeriesList.AddObject(ATitle, AHistogram);
        AHistogram.ParentChart := chartModflow;
        AHistogram.Title := ATitle;
        AHistogram.Visible := False;
//        SeriesList.AddObject(ATitle, AHistogram);
//        Fmcmc_Variables.AddObject(ATitle, AHistogram);

        AssignHistogramValues(AValuesList, BinCount, AHistogram);
      end
      else
      begin
        AValuesList.Sort;
        AName := chcklstbx_Mcmc_pred.Items[VarIndex];
        Mean := 0;
        for ValueIndex := 0 to AValuesList.Count -1 do
        begin
          Mean := Mean + AValuesList[ValueIndex];
        end;
        Mean := Mean/AValuesList.Count;
        LowRange := AValuesList.First;
        HiRange := AValuesList.Last;
        Low_CI := AValuesList[Round(AValuesList.Count*Fraction)];
        High_CI := AValuesList[Round(AValuesList.Count*(1-Fraction))];
        RangeSeries.AddCI_AndRange(AName, Mean, High_CI, Low_CI, HiRange, LowRange);
        RangeSeries.ShowConfidenceIntervals := True;
        RangeSeries.ShowRange := True;
      end;
    end;

    if SelectedPrediction <> '' then
    begin
      chcklstbx_Mcmc_pred.ItemIndex :=
        chcklstbx_Mcmc_pred.Items.IndexOf(SelectedPrediction);
    end;

    if chcklstbx_Mcmc_pred.ItemIndex < 0 then
    begin
      chcklstbx_Mcmc_pred.ItemIndex := 0;
    end;
    chcklstbx_Mcmc_pred.Checked[chcklstbx_Mcmc_pred.ItemIndex] := True;
    chcklstbx_Mcmc_predClickCheck(nil);
  finally
    FileNames.Free;
    AFile.Free;
    Splitter.Free;
    ValueLists.Free;
  end;

end;

procedure TfrmModChart.chcklstbx_Mcmc_predClickCheck(Sender: TObject);
var
  SeriesIndex: integer;
  ASeries: TChartSeries;
begin
  inherited;
  if rgMcmc_predGraphType.ItemIndex = 0 then
  begin
    for SeriesIndex := 0 to SeriesList.Count -1 do
    begin
      ASeries := SeriesList.Objects[SeriesIndex] as TChartSeries;

      // This prevents a range check error when you click on the chart.
      // The range check error is due to a bug in THistogramSeries.
      // THistogramSeries doesn't calculate the visible points until
      // it has been displayed but tries to use an unitialized value
      // of THistogramSeries.FFirstVisibleIndex.
      // Thus, this is a work-around and not a fix.
      ASeries.Visible := True;
      Application.ProcessMessages;
      
      ASeries.Visible := SeriesIndex = chcklstbx_Mcmc_pred.ItemIndex;
    end;
    if chcklstbx_Mcmc_pred.ItemIndex >= 0 then
    begin
      ASeries := SeriesList.Objects[chcklstbx_Mcmc_pred.ItemIndex] as TChartSeries;
      ASeries.Visible := True;
      chartModflow.BottomAxis.Title.Caption :=
        chcklstbx_Mcmc_pred.Items[chcklstbx_Mcmc_pred.ItemIndex];
    end;

  end;    
end;

procedure TfrmModChart.rgMcmc_predGraphTypeClick(Sender: TObject);
begin
  inherited;
  jvpl_mcmc_pred.ActivePageIndex := rgMcmc_predGraphType.ItemIndex;
  RedrawChart(nil);
end;

procedure TfrmModChart.rgConfidenceIntervalClick(Sender: TObject);
begin
  inherited;
  rdeConfidenceInterval.Enabled := rgConfidenceInterval.ItemIndex = 3;
  RedrawChart(nil);
end;

procedure TfrmModChart.rdeConfidenceIntervalChange(Sender: TObject);
var
  AValue: double;
begin
  inherited;
  if TryStrToFloat(rdeConfidenceInterval.Text, AValue) then
  begin
    RedrawChart(nil);
  end;

end;

procedure TfrmModChart.Read_svd;
const
  RatioString = '"RATIO TO LARGEST SINGULARVALUE "';
  ParameterString = '"PARAMETER FOR EACH VECTOR ELEMENT "';
var
  ALine: string;
  Splitter: TStringList;
  ValueCount: integer;
  RatioPos: integer;
  RatioSeries: TLineSeries;
  ATitle: string;
  AValue: double;
  ValueIndex: Integer;
  Smallest: double;
  ParameterPosition: integer;
  VectorCount: integer;
  ABarSeries: TBarSeries;
  VectorIndex: integer;
begin
  jvplChartControls.Visible := True;
  jvplChartControls.ActivePage := jvsp_svd;
//  FixLabelSize;
  // Skip first line
  ReadLn(FFile, ALine);
  // number of values
  ReadLn(FFile, ALine);
  FSvdExponent := 1;
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    Splitter.DelimitedText := Trim(ALine);
    ValueCount := Splitter.Count;

    ReadLn(FFile, ALine);
    Assert(Pos('"SINGULARVALUE FOR EACH VECTOR  "', ALine) > 0);

    ReadLn(FFile, ALine);
    RatioPos := Pos(RatioString, ALine);
    Assert(RatioPos > 0);
    if rgWhatToPlot_svd.ItemIndex = 0 then
    begin
      chartModflow.Legend.Visible := False;
      chartModflow.LeftAxis.Logarithmic := True;

      ATitle := 'Singular value relative to largest';
      RatioSeries := TLineSeries.Create(self);
//      RatioSeries.XValues.Order := loNone;
      SeriesList.AddObject(ATitle, RatioSeries);
      RatioSeries.ParentChart := chartModflow;
      RatioSeries.Title := ATitle;
      RatioSeries.Visible := True;

      Smallest := 2;
      ALine := Copy(ALine, RatioPos + Length(RatioString), MaxInt);
      Splitter.DelimitedText := Trim(ALine);
      for ValueIndex := 0 to Splitter.Count -1 do
      begin
        AValue := FortranStrToFloat(Splitter[ValueIndex]);
        if (AValue > 0) and (AValue < Smallest) then
        begin
          Smallest := AValue;
        end;

        RatioSeries.AddXY(ValueIndex+1, AValue);
      end;
      if Smallest < 2 then
      begin
        FSvdExponent := Floor(Log10(Smallest));
        FixLabelSize(FloatToStr(Power(10, FSvdExponent)));
        chartModflow.LeftAxis.AutomaticMaximum := False;
        chartModflow.LeftAxis.Maximum := 1;
        chartModflow.LeftAxis.AutomaticMinimum := False;
        chartModflow.LeftAxis.Minimum := Power(10,FSvdExponent);
      end;
    end
    else
    begin
      ExplantionVisible := True;
      ReadLn(FFile, ALine);
      ParameterPosition := Pos(ParameterString, ALine);
      Assert(ParameterPosition > 0);
      ALine := Copy(ALine, ParameterPosition + Length(ParameterString), MaxInt);
      Splitter.DelimitedText := ALine;
      VectorCount := Splitter.Count;
//      for VectorIndex := 0 to VectorCount -1 do
//      begin
//        ATitle := 'Vector ' + IntToStr(VectorIndex+1);
//        ABarSeries := TBarSeries.Create(self);
//  //      ABarSeries.XValues.Order := loNone;
//        SeriesList.AddObject(ATitle, ABarSeries);
//        ABarSeries.ParentChart := chartModflow;
//        ABarSeries.Title := ATitle;
//        ABarSeries.Visible := True;
//        ABarSeries.Stacked := True;
//      end;



      While not EOF(FFile) do
      begin
        ReadLn(FFile, ALine);
        if ALine <> '' then
        begin
          Splitter.DelimitedText := Trim(ALine);
          ATitle := Splitter[0];
          ABarSeries := TBarSeries.Create(self);
          SeriesList.AddObject(ATitle, ABarSeries);
          ABarSeries.ParentChart := chartModflow;
          ABarSeries.Title := ATitle;
          ABarSeries.Visible := True;
          ABarSeries.MultiBar := mbStacked;
          ABarSeries.Marks.Visible := False;
          for VectorIndex := 1 to Splitter.Count -1 do
          begin
            ABarSeries.AddXY(VectorIndex, FortranStrToFloat(Splitter[VectorIndex]));
          end;

        end;
      end;

    end;


  finally
    Splitter.Free;
  end;


end;

procedure TfrmModChart.chartModflowGetNextAxisLabel(Sender: TChartAxis;
  LabelIndex: Integer; var LabelValue: Double; var Stop: Boolean);
begin
  inherited;
  if (Sender = chartModflow.LeftAxis) and (FileType = ft_svd) and (rgWhatToPlot_svd.ItemIndex = 0) then
  begin
    if FSvdExponent < 1 then
    begin
      LabelValue := Power(10,FSvdExponent+LabelIndex);
      if LabelValue < 0.999 then
      begin
        Stop := False;
      end;

    end;
  end;    
end;

procedure TfrmModChart.cbLinpConfidenceIntervalsClick(Sender: TObject);
begin
  inherited;
  RangeSeries.ShowConfidenceIntervals := cbLinpConfidenceIntervals.Checked;
end;

procedure TfrmModChart.cbLinpPlotStandardDevClick(Sender: TObject);
begin
  inherited;
  RangeSeries.ShowRange := cbLinpPlotStandardDev.Checked;
end;

procedure TfrmModChart.seIgnoreValueOSChange(Sender: TObject);
begin
  inherited;
  rdgIgnoreValueOS.RowCount := Min(2, seIgnoreValueOS.AsInteger+1);
  if seIgnoreValueOS.AsInteger = 0 then
  begin
    rdgIgnoreValueOS.Cells[0,1] := '';
  end;

end;

procedure TfrmModChart.rdgIgnoreValueOSEndUpdate(Sender: TObject);
begin
  inherited;
  seIgnoreValueOS.AsInteger := rdgIgnoreValueOS.RowCount -1;
end;

procedure TfrmModChart.seValuesToIgnoreNMChange(Sender: TObject);
begin
  inherited;
  rdgValuesToIgnoreNM.RowCount := Min(2, seValuesToIgnoreNM.AsInteger+1);
  if seValuesToIgnoreNM.AsInteger = 0 then
  begin
    rdgValuesToIgnoreNM.Cells[0,1] := '';
  end;
end;

procedure TfrmModChart.rdgValuesToIgnoreNMEndUpdate(Sender: TObject);
begin
  inherited;
  seValuesToIgnoreNM.AsInteger := rdgValuesToIgnoreNM.RowCount -1;
end;

procedure TfrmModChart.DelayRedraw;
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;


  tmrRedrawDelay.Enabled := False;
  tmrRedrawDelay.Enabled := True;
end;

procedure TfrmModChart.tmrRedrawDelayTimer(Sender: TObject);
begin
  inherited;
  tmrRedrawDelay.Enabled := False;
  RedrawChart(Sender);
end;

procedure TfrmModChart.clb_OsPlotsymbolsClickCheck(Sender: TObject);
var
  ASeries: TPointSeries;
  Index: integer;
begin
  inherited;
  Assert(SeriesList.Count = clb_OsPlotsymbols.Items.Count);
  for Index := 0 to SeriesList.Count -1 do
  begin
    ASeries := SeriesList.Objects[Index] as TPointSeries;
    ASeries.Visible := clb_OsPlotsymbols.Checked[Index];
  end;
end;

procedure TfrmModChart.clbItemsToPlotClick(Sender: TObject);
var
  ASeries: TPointSeries;
  Index: integer;
begin
  inherited;
  Assert(SeriesList.Count = clbItemsToPlot.Items.Count);
  for Index := 0 to SeriesList.Count -1 do
  begin
    ASeries := SeriesList.Objects[Index] as TPointSeries;
    ASeries.Visible := clbItemsToPlot.Checked[Index];
  end;
end;

end.
