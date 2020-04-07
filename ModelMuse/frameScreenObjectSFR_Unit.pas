unit frameScreenObjectSFR_Unit;

interface

uses System.UITypes,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ArgusDataEntry, Grids, RbwDataGrid4,
  Mask, JvExMask, JvSpin, frameFlowTableUnit, JvPageList, JvExControls,
  frameCrossSectionUnit, Buttons, ZoomBox2, JvExStdCtrls, JvCombobox,
  JvListComb, RbwParser, UndoItemsScreenObjects, ModflowSfrUnit, JvToolEdit,
  GoPhastTypes, JvPageListTreeView, frameScreenObjectUnit, frameGridUnit,
  GrayTabs, framePestObsUnit;

type
  TFrameClass = class of TFrame;

  TGetParserEvent = Function (Sender: TObject): TRbwParser of object;

  TframeScreenObjectSFR = class(TframeScreenObject)
    pcSFR: TPageControl;
    tabSegment: TTabSheet;
    tabTable: TTabSheet;
    tabChannel: TTabSheet;
    tabEquation: TTabSheet;
    dgSfrEquation: TRbwDataGrid4;
    pnlSegmentUpstream: TPanel;
    dgUp: TRbwDataGrid4;
    pnlSegmentDownstream: TPanel;
    dgDown: TRbwDataGrid4;
    Splitter1: TSplitter;
    Panel4: TPanel;
    dgTableTime: TRbwDataGrid4;
    Splitter2: TSplitter;
    Panel5: TPanel;
    dgSfrRough: TRbwDataGrid4;
    Splitter3: TSplitter;
    rdeSegmentNumber: TRbwDataEntry;
    Label1: TLabel;
    tabBasic: TTabSheet;
    gReachProperties: TGroupBox;
    lblStreamTop: TLabel;
    lblSlope: TLabel;
    lblStreambedThickness: TLabel;
    lblStreambedK: TLabel;
    lblSaturatedVolumetricWater: TLabel;
    lblInitialVolumetricWater: TLabel;
    lblBrooksCoreyExponent: TLabel;
    lblMaxUnsaturatedKz: TLabel;
    tabUnsaturatedProperties: TTabSheet;
    zbChannel: TQRbwZoomBox2;
    gpFlowTable: TGridPanel;
    zbFlowDepthTable: TQRbwZoomBox2;
    zbFlowWidthTable: TQRbwZoomBox2;
    pnlCaption: TPanel;
    gbUnsatUpstream: TGroupBox;
    Label6: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    gbUnsatDownstream: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    tabFlows: TTabSheet;
    dgFlowTimes: TRbwDataGrid4;
    pnlDownstream: TPanel;
    pnlUpstream: TPanel;
    jceStreamTop: TJvComboEdit;
    jceSlope: TJvComboEdit;
    jceStreambedThickness: TJvComboEdit;
    jceStreambedK: TJvComboEdit;
    jceSaturatedVolumetricWater: TJvComboEdit;
    jceInitialVolumetricWater: TJvComboEdit;
    jceBrooksCoreyExponent: TJvComboEdit;
    jceMaxUnsaturatedKz: TJvComboEdit;
    jceSaturatedVolumetricWaterUpstream: TJvComboEdit;
    jceInitialVolumetricWaterUpstream: TJvComboEdit;
    jceBrooksCoreyExponentUpstream: TJvComboEdit;
    jceMaxUnsaturatedKzUpstream: TJvComboEdit;
    jceSaturatedVolumetricWaterDownstream: TJvComboEdit;
    jceInitialVolumetricWaterDownstream: TJvComboEdit;
    jceBrooksCoreyExponentDownstream: TJvComboEdit;
    jceMaxUnsaturatedKzDownstream: TJvComboEdit;
    lblReachLength: TLabel;
    jvcReachLength: TJvComboEdit;
    pnlFlowTop: TPanel;
    rdeFlowFormula: TRbwDataEntry;
    lblFlowFormula: TLabel;
    lblUpstreamFormula: TLabel;
    rdeUpstreamFormula: TRbwDataEntry;
    lblDownstreamFormula: TLabel;
    rdeDownstreamFormula: TRbwDataEntry;
    pnlChannelTop: TPanel;
    lblChannelFormula: TLabel;
    rdeChannelFormula: TRbwDataEntry;
    pnlEquationTop: TPanel;
    lblEquationFormula: TLabel;
    rdeEquationFormula: TRbwDataEntry;
    tabTime: TTabSheet;
    pnlParamTop: TPanel;
    lblParameterChoices: TLabel;
    rdgParameters: TRbwDataGrid4;
    pnlParamBottom: TPanel;
    lblParametersCount: TLabel;
    seParametersCount: TJvSpinEdit;
    btnInserParameters: TBitBtn;
    btnDeleteParameters: TBitBtn;
    comboParameterChoices: TJvImageComboBox;
    lblIcalcChoice: TLabel;
    comboIcalcChoice: TJvImageComboBox;
    tabGage: TTabSheet;
    rgGages: TRadioGroup;
    gbObservationTypes: TGroupBox;
    cbGagStandard: TCheckBox;
    cbGag1: TCheckBox;
    cbGag2: TCheckBox;
    cbGag3: TCheckBox;
    cbGag5: TCheckBox;
    cbGag6: TCheckBox;
    cbGag7: TCheckBox;
    tabNetwork: TTabSheet;
    pnlNetwork: TPanel;
    lblSegment: TLabel;
    rdeNetwork: TRbwDataEntry;
    comboMultiIprior: TJvImageComboBox;
    rdgNetwork: TRbwDataGrid4;
    tabExternalFlowFile: TTabSheet;
    frameExternalFileValues: TframeGrid;
    pnlFlowFile: TPanel;
    rgExternalFlowChoice: TRadioGroup;
    rgReferenceTimeChoice: TRadioGroup;
    fedExternalFileName: TJvFilenameEdit;
    lblExternalFileName: TLabel;
    frameCrossSection1: TframeCrossSection;
    frameFlowTable1: TframeFlowTable;
    tabObservations: TTabSheet;
    frameSfrPestObs: TframePestObs;
    procedure dgTableTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgSfrRoughSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgUpSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgFlowTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgFlowTimesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdeFlowFormulaChange(Sender: TObject);
    procedure dgFlowTimesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgFlowTimesColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgFlowTimesHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure dgFlowTimesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdeUpstreamFormulaChange(Sender: TObject);
    procedure rdeDownstreamFormulaChange(Sender: TObject);
    procedure dgUpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgDownMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgUpColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgUpHorizontalScroll(Sender: TObject);
    procedure dgUpSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgDownSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgDownColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgDownHorizontalScroll(Sender: TObject);
    procedure rdeChannelFormulaChange(Sender: TObject);
    procedure dgSfrRoughMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgSfrRoughSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgSfrRoughColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgSfrRoughHorizontalScroll(Sender: TObject);
    procedure rdeEquationFormulaChange(Sender: TObject);
    procedure dgSfrEquationMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgSfrEquationSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgSfrEquationColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgSfrEquationHorizontalScroll(Sender: TObject);
    procedure dgTableTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seParametersCountChange(Sender: TObject);
    procedure btnInserParametersClick(Sender: TObject);
    procedure btnDeleteParametersClick(Sender: TObject);
    procedure rdgParametersHorizontalScroll(Sender: TObject);
    procedure rdgParametersColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgParametersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgParametersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgParametersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure comboParameterChoicesChange(Sender: TObject);
    procedure comboIcalcChoiceChange(Sender: TObject);
    procedure dgSfrEquationSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgParametersBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdeSegmentNumberChange(Sender: TObject);
    procedure cbSfrGagClick(Sender: TObject);
    procedure rgGagesClick(Sender: TObject);
    procedure rdgNetworkHorizontalScroll(Sender: TObject);
    procedure rdgNetworkColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgNetworkMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgNetworkSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgNetworkSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure comboMultiIpriorChange(Sender: TObject);
    procedure rdeNetworkChange(Sender: TObject);
    procedure rgExternalFlowChoiceClick(Sender: TObject);
    procedure frameCrossSection1dg8PointSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameCrossSection1dg8PointSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameCrossSection1dg8PointButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure seParametersCountEnter(Sender: TObject);
    procedure frameFlowTable1dgSfrTableSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFlowTable1dgSfrTableButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure dgSfrTableSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameFlowTable1seTableCountChange(Sender: TObject);
    procedure frameFlowTable1btnInsertFlowTableRowClick(Sender: TObject);
    procedure frameFlowTable1btnDeleteFlowTableRowClick(Sender: TObject);
  private
    FGettingData: boolean;
    FISFROPT: integer;
    FGetParser: TGetParserEvent;
    FOnButtonClick: TGridButtonEvent;
    FDeletingRow: Boolean;
    FOnEdited: TNotifyEvent;
    FUpdatingICalc: Boolean;
    FDeletingTime: Boolean;
    FTimeCount: integer;
    FInitializing: Boolean;
    FListCount: Integer;
    procedure PaintCrossSection(Sender: TObject; Buffer: TBitmap32);
    procedure DrawCrossSection(ABitMap: TBitmap32);
    procedure dgCrossSectionSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure PaintFlowDepth(Sender: TObject; Buffer: TBitmap32);
    procedure PaintFlowWidth(Sender: TObject; Buffer: TBitmap32);
    procedure DrawFlowDepth(ABitMap: TBitmap32);
    procedure DrawFlowWidth(ABitMap: TBitmap32);
    procedure DrawFlowTable(ABitMap: TBitmap32; YColumn: Integer; ZoomBox: TQRbwZoomBox2);
    procedure SetISFROPT(const Value: integer);
    procedure EnableUnsatControls;
    procedure GetSfrValues(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetSfrFlows(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetSfrSegments(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetSfrChannel(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetSfrEquation(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetSfrFlowTable(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure GetUnsaturatedValues(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure SetSfrValues(Boundary: TSfrBoundary);
    procedure SetSfrFlows(Boundary: TSfrBoundary);
    procedure SetSfrSegments(Boundary: TSfrBoundary);
    procedure SetSfrChannel(Boundary: TSfrBoundary);
    procedure SetSfrEquation(Boundary: TSfrBoundary);
    procedure SetSfrFlowTable(Boundary: TSfrBoundary);
    procedure SetUnsaturatedValues(Boundary: TSfrBoundary);
    procedure ClearTable(Grid: TStringGrid);
    procedure SetOnButtonClick(const Value: TGridButtonEvent);
    procedure GetStartTimes(Grid: TRbwDataGrid4; Col: integer);
    procedure GetEndTimes(Grid: TRbwDataGrid4; Col: integer);
    procedure AssignSelectedCellsInGrid(DataGrid: TRbwDataGrid4;
      const NewText: string);
    procedure EnableMultiEditControl(EdControl: TControl; DataGrid: TRbwDataGrid4);
    procedure LayoutMultiRowFlowEditControls;
    procedure UpdateSpinEditValue(DataGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure LayoutMultiRowUpstreamEditControls;
    procedure LayoutMultiRowDownstreamEditControls;
    procedure LayoutMultiRowChannelEditControls;
    procedure LayoutMultiRowEquationEditControls;
    procedure LayoutMultiRowParamIcalcControls;
    procedure AssignParamIcalcInGrid(ColIndex: Integer; NewText: string);
    procedure GetParamIcalcValues(Boundary: TSfrBoundary; FoundFirst: Boolean);
    procedure SetSfrParamIcalc(Boundary: TSfrBoundary);
    function IcalcSet: TByteSet;
    procedure EnableTabs;
    procedure UpdatedTimesInSfrGrids(const Value: string; ACol: Integer; ARow: Integer);
    function LocateRowFromStartAndEndTimes(StartTime, EndTime: double): integer;
    function IcalcRowSet(Row: integer): TByteSet;
    procedure dg8PointSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure UpdateICalc;
    procedure UpdateKCaption;
    procedure LayoutMultiRowNetworkControls;
    procedure AssignMultipleIPrior(NewText: string; ColIndex: Integer);
    procedure GetExternalFlows(Boundary: TSfrBoundary);
    procedure SetExternalFlowFile(Boundary: TSfrBoundary);
  protected
    procedure Loaded; override;
  private
    FTimesChanged: boolean;
    procedure Edited;
    { Private declarations }
  public
    procedure Initialize;
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property ISFROPT: integer read FISFROPT write SetISFROPT;
    constructor Create(AOwner: TComponent); override;
    property GetParser: TGetParserEvent read FGetParser write FGetParser;
    property OnButtonClick: TGridButtonEvent read FOnButtonClick write SetOnButtonClick;
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

  TSfrNetworkColumns = (sncStartTime, sncEndtime, sncOutflowSegment,
    sncDiversionSegment, sncIprior);

implementation

uses Math, BigCanvasMethods, ModflowSfrFlows,
  ModflowSfrSegment, ModflowSfrChannelUnit, ModflowSfrEquationUnit,
  ModflowSfrTable, ModflowSfrUnsatSegment, ModflowSfrReachUnit, frmGoPhastUnit,
  ModflowTimeUnit, ModflowTransientListParameterUnit, OrderedCollectionUnit,
  frmCustomGoPhastUnit, ModflowSfrParamIcalcUnit, ModflowPackageSelectionUnit;

resourcestring
  StrStartingTime = 'Starting time';
  StrEndingTime = 'Ending time';
  StrHydraulicConductivi = 'Hydraulic conductivity';
  StrHydraulicCondMult = 'Hydraulic conductivity multiplier';
  StrTime = 'Time';
  StrInflow = 'Inflow';
  StrSorryThatChoiceI = 'Sorry, that choice is not allowed.';
  StrStreambedThickness = 'Streambed thickness';
  StrStreambedElevation = 'Streambed elevation';
  StrStreamWidth = 'Stream width';
  StrStreamDepth = 'Stream depth';
  StrChannelRoughness = 'Channel roughness';
  StrBankRoughness = 'Bank roughness';
  StrOutflowSegmentOUT = 'Outflow segment or lake (OUTSEG)';
  StrDiversionSegmentI = 'Diversion segment or lake (IUPSEG)';
  StrDiversionPriority = 'Diversion priority (IPRIOR)';
  StrDepthCoefficient = 'Depth coefficient';
  StrDepthExponent = 'Depth exponent';
  StrWidthCoefficient = 'Width coefficient';
  StrWidthExponent = 'Width exponent';
  StrFlowIntoUpstreamE = 'Flow into upstream end (FLOW)';
  StrPrecipitationVolume = 'Precipitation rate (PTSW)';
  StrEvapotranspirationV = 'Evapotranspiration rate (ETSW)';
  StrRunoffVolumeRUNOF = 'Runoff volume (RUNOFF)';
  StrParameter = 'Parameter';
  StrParameterInstance = 'Parameter instance';
  StrStageCalculationI = 'Stage calculation (ICALC)';
  StrSorryICALCMustBe = 'Sorry, ICALC must be the same for all stress period' +
  's when unsaturated flow beneath a stream is simulated.';
  StrSorryICALCCanNot = 'Sorry, ICALC can not be changed to 1 or 2 after the' +
  ' first stress period when unsaturated flow beneath streams is simulated.';
  StrNone = 'none';

{$R *.dfm}

Type
  TSfrColumn = (scStartTime, scEndTime, scK, scBedThickness, scBedElevation,
    scStreamWidth, scStreamDepth);

//  TUnsatSfrColumn = (uscSatWatCont,
//    uscInitWaterCont, uscBrooksCorey, uscVK);

  TsfrRoughColumn = (srStartTime, srEndTime, srChannelRough, srBankRough);

  TSfrEquationColumns = (seStartTime, seEndTime, seDepthCoeff, seDepthExp,
    seWidthCoeff, seWidthExp);

  TSfrFlowColumns = (sfcStartTime, sfcEndTime, sfcFlow, sfcPrecip,
    sfcEvap, sfcRunoff);

  TSfrTableTime = (sttStartTime, sttEndTime);

  TSfrParamIcalcColumns = (spicStartTime, spicEndTime,
    spicParameter, spicParamInstance, spicIcalc);

{ TframeScreenObjectSFR }

procedure TframeScreenObjectSFR.btnDeleteParametersClick(Sender: TObject);
var
  RowToDelete: integer;
begin
  RowToDelete := rdgParameters.SelectedRow;
  if RowToDelete > 0 then
  begin
    rdgParameters.DeleteRow(RowToDelete);
    rdgNetwork.DeleteRow(RowToDelete);

    dgFlowTimes.DeleteRow(RowToDelete);
    dgUp.DeleteRow(RowToDelete);
    dgDown.DeleteRow(RowToDelete);
    dgSfrEquation.DeleteRow(RowToDelete);

    frameCrossSection1.DeleteItem(RowToDelete-1);
    frameFlowTable1.DeleteItem(RowToDelete-1);
    Dec(FTimeCount);
    if dgSfrRough.SelectedRow >= 1 then
    begin
      frameCrossSection1.DisplayData(dgSfrRough.SelectedRow-1);
    end;
    if dgTableTime.SelectedRow >= 1 then
    begin
      frameCrossSection1.DisplayData(dgTableTime.SelectedRow-1);
    end;

    seParametersCount.AsInteger := seParametersCount.AsInteger -1;
    seParametersCount.OnChange(nil);

    EnableTabs;
  end;
end;

procedure TframeScreenObjectSFR.Loaded;
var
  Index: integer;
begin
  inherited;
  pcSFR.ActivePageIndex := 0;
  for Index := 0 to rdgParameters.ColCount - 1 do
  begin
    rdgParameters.Columns[Index].ComboUsed := True;
  end;
  frameExternalFileValues.Grid.Cells[0,0] := StrTime;
  frameExternalFileValues.Grid.Cells[1,0] := StrInflow;
end;

procedure TframeScreenObjectSFR.rdeChannelFormulaChange(Sender: TObject);
begin
  AssignSelectedCellsInGrid(dgSfrRough, rdeChannelFormula.Text);
end;

procedure TframeScreenObjectSFR.rdeDownstreamFormulaChange(Sender: TObject);
begin
  AssignSelectedCellsInGrid(dgDown, rdeDownstreamFormula.Text);
end;

procedure TframeScreenObjectSFR.rdeEquationFormulaChange(Sender: TObject);
begin
  AssignSelectedCellsInGrid(dgSfrEquation, rdeEquationFormula.Text);
end;

procedure TframeScreenObjectSFR.rdeFlowFormulaChange(Sender: TObject);
begin
  AssignSelectedCellsInGrid(dgFlowTimes, rdeFlowFormula.Text);
end;

procedure TframeScreenObjectSFR.rdeNetworkChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  NewText: string;
begin
  NewText := rdeNetwork.Text;
  for RowIndex := rdgNetwork.FixedRows to rdgNetwork.RowCount - 1 do
  begin
    for ColIndex := Ord(sncOutflowSegment) to Ord(sncDiversionSegment) do
    begin
      if rdgNetwork.IsSelectedCell(ColIndex, RowIndex) then
      begin
        rdgNetwork.Cells[ColIndex, RowIndex] := NewText;
        if Assigned(rdgNetwork.OnSetEditText) then
        begin
          rdgNetwork.OnSetEditText(rdgNetwork, ColIndex, RowIndex, NewText);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.rdeSegmentNumberChange(Sender: TObject);
begin
  Edited;
end;

procedure TframeScreenObjectSFR.rdeUpstreamFormulaChange(Sender: TObject);
begin
  AssignSelectedCellsInGrid(dgUp, rdeUpstreamFormula.Text);
end;

procedure TframeScreenObjectSFR.rdgNetworkColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowNetworkControls;
end;

procedure TframeScreenObjectSFR.rdgNetworkHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowNetworkControls;
end;

procedure TframeScreenObjectSFR.rdgNetworkMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  ShouldEnable := False;
  for ColIndex := Ord(sncOutflowSegment) to Ord(sncDiversionSegment) do
  begin
    for RowIndex := rdgNetwork.FixedRows to rdgNetwork.RowCount - 1 do
    begin
      ShouldEnable := rdgNetwork.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeNetwork.Enabled := ShouldEnable;

  ShouldEnable := False;
  ColIndex := Ord(sncIprior);
  for RowIndex := rdgNetwork.FixedRows to rdgNetwork.RowCount - 1 do
  begin
    ShouldEnable := rdgNetwork.IsSelectedCell(ColIndex, RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboMultiIprior.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectSFR.rdgNetworkSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ItemCount: integer;
  AnInt: Integer;
begin
  ItemCount := seParametersCount.AsInteger;
  CanSelect := (ItemCount > 0);
  if not CanSelect then
  begin
    Exit;
  end;

  if TSfrNetworkColumns(ACol) in [sncStartTime, sncEndtime] then
  begin
    CanSelect := False;
  end;

  if (ACol = Ord(sncIprior)) and (ARow >= 1) then
  begin
    if TryStrToInt(rdgNetwork.Cells[Ord(sncDiversionSegment), ARow], AnInt) then
    begin
      // IPRIOR can only be specified when diverting from streams not when
      // diverting from lakes.
      CanSelect := AnInt >= 1;
    end
    else
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeScreenObjectSFR.rdgNetworkSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if FDeletingTime then
  begin
    Exit;
  end;
  Edited;
//  EnableTabs;
  UpdateSpinEditValue(rdgNetwork, seParametersCount);
end;

procedure TframeScreenObjectSFR.rdgParametersBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  Parameter: string;
  Instance: string;
  Index: Integer;
begin
  if TSfrParamIcalcColumns(ACol) in [spicParameter, spicParamInstance] then
  begin
    Parameter := rdgParameters.Cells[Ord(spicParameter), ARow];
    if (Parameter <> '')
      and (Parameter <> rdgParameters.Columns[Ord(spicParameter)].PickList[0]) then
    begin
      Instance := rdgParameters.Cells[Ord(spicParamInstance), ARow];
      if Instance <> '' then
      begin
        for Index := 1 to rdgParameters.RowCount - 1 do
        begin
          if Index = ARow then
          begin
            Continue;
          end;
          if (Parameter = rdgParameters.Cells[Ord(spicParameter), Index])
            and (Instance = rdgParameters.Cells[Ord(spicParamInstance), Index]) then
          begin
            rdgParameters.Canvas.Brush.Color := clRed;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.rdgParametersColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowParamIcalcControls;
end;

procedure TframeScreenObjectSFR.rdgParametersHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowParamIcalcControls;
end;

procedure TframeScreenObjectSFR.rdgParametersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  ShouldEnable := False;
  ColIndex := Ord(spicParameter);
  for RowIndex := rdgParameters.FixedRows to rdgParameters.RowCount - 1 do
  begin
    ShouldEnable := rdgParameters.IsSelectedCell(ColIndex, RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboParameterChoices.Enabled := ShouldEnable;

  ShouldEnable := False;
  ColIndex := Ord(spicIcalc);
  for RowIndex := rdgParameters.FixedRows to rdgParameters.RowCount - 1 do
  begin
    ShouldEnable := rdgParameters.IsSelectedCell(ColIndex, RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboIcalcChoice.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectSFR.rdgParametersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ItemCount: integer;
  PickList: TStringList;
  Index: Integer;
  ParameterInstances: TSfrParamInstances;
  Item: TSfrParamInstance;
  ParamName: string;
begin
  ItemCount := seParametersCount.AsInteger;
  CanSelect := (ItemCount > 0);
  if not CanSelect then
  begin
    Exit;
  end;

  if (ACol = Ord(spicParamInstance)) and (ARow >= 1) then
  begin
    ParamName := rdgParameters.Cells[Ord(spicParameter), ARow];
    if ParamName =
      rdgParameters.Columns[Ord(spicParameter)].PickList[0] then
    begin
      CanSelect := False;
      rdgParameters.Columns[Ord(spicParamInstance)].PickList.Clear;
    end
    else
    begin
      PickList := TStringList.Create;
      try
        ParameterInstances := frmGoPhast.PhastModel.
          ModflowPackages.SfrPackage.ParameterInstances;
        for Index := 0 to ParameterInstances.Count - 1 do
        begin
          Item := ParameterInstances.Items[Index];
          if Item.ParameterName = ParamName then
          begin
            PickList.Add(Item.ParameterInstance);
          end;
        end;
        rdgParameters.Columns[Ord(spicParamInstance)].PickList := PickList;
      finally
        PickList.Free;
      end;
    end;
  end;

  if (ACol in [Ord(spicStartTime), Ord(spicEndTime)])
    and (ARow >= 1)
    and (rdgParameters.Columns[Ord(spicParameter)].PickList.Count > 0) then
  begin
    ParamName := rdgParameters.Cells[Ord(spicParameter), ARow];
    CanSelect := ParamName = rdgParameters.Columns[Ord(spicParameter)].PickList[0];
  end;
  if (ACol = Ord(spicIcalc)) and (ARow > 1) and (ISFROPT > 1) then
  begin
    if (rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
      rdgParameters.Cells[Ord(spicIcalc), 1]) in [1,2]) then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeScreenObjectSFR.rdgParametersSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ParamName: string;
  InstanceName: string;
  Index: Integer;
  ParameterInstances: TSfrParamInstances;
  Item: TSfrParamInstance;
  NewValue: string;
begin
  if FDeletingTime then
  begin
    Exit;
  end;
  Edited;
  EnableTabs;
  UpdateSpinEditValue(rdgParameters, seParametersCount);
  if (ACol in [Ord(spicParameter), Ord(spicParamInstance)]) and (ARow >= 1) then
  begin
    UpdateKCaption;
    ParamName := rdgParameters.Cells[Ord(spicParameter), ARow];
    if (ParamName <> '') and
      (ParamName <> rdgParameters.Columns[Ord(spicParameter)].PickList[0]) then
    begin
      InstanceName := rdgParameters.Cells[Ord(spicParamInstance), ARow];
      if InstanceName <> '' then
      begin
        ParameterInstances := frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances;
        for Index := 0 to ParameterInstances.Count - 1 do
        begin
          Item := ParameterInstances.Items[Index];
          if (Item.ParameterName = ParamName)
            and (Item.ParameterInstance = InstanceName) then
          begin
            NewValue := FloatToStr(Item.StartTime);
            rdgParameters.Cells[Ord(spicStartTime), ARow] := NewValue;
            UpdatedTimesInSfrGrids(NewValue, Ord(spicStartTime), ARow);

            NewValue := FloatToStr(Item.EndTime);
            rdgParameters.Cells[Ord(spicEndTime), ARow] := NewValue;
            UpdatedTimesInSfrGrids(NewValue, Ord(spicEndTime), ARow);
          end;
        end;
      end;
    end;
  end;
  UpdateNextTimeCell(rdgParameters, ACol, ARow);
  if (ACol in [Ord(spicStartTime), Ord(spicEndTime)]) and (ARow >= 1) then
  begin
    UpdatedTimesInSfrGrids(Value, ACol, ARow);
    if ACol = Ord(spicStartTime) then
    begin
      UpdatedTimesInSfrGrids(rdgParameters.Cells[Ord(spicEndTime), ARow],
        Ord(spicEndTime), ARow);
    end
    else if ARow +1 < rdgParameters.RowCount then
    begin
      UpdatedTimesInSfrGrids(rdgParameters.Cells[Ord(spicStartTime), ARow+1],
        Ord(spicStartTime), ARow+1);
    end;
  end;
  if (ACol = Ord(spicIcalc)) and (ARow > 1) then
  begin
    UpdateICalc;
    if (ISFROPT > 1) then
    begin
      if (rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
        rdgParameters.Cells[Ord(spicIcalc), ARow]) in [1,2])
        or (rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
        rdgParameters.Cells[Ord(spicIcalc), 1]) in [1,2]) then
      begin
        if rdgParameters.Cells[Ord(spicIcalc), ARow] <>
          rdgParameters.Cells[Ord(spicIcalc), 1] then
        begin
          rdgParameters.Cells[Ord(spicIcalc), ARow] :=
            rdgParameters.Cells[Ord(spicIcalc), 1];
          rdgParameters.OnSetEditText(Sender,Ord(spicIcalc), ARow,
            rdgParameters.Cells[Ord(spicIcalc), ARow]);
          Beep;
          MessageDlg(StrSorryThatChoiceI, mtInformation, [mbOK], 0);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.rgExternalFlowChoiceClick(Sender: TObject);
begin
  inherited;
  fedExternalFileName.Enabled := rgExternalFlowChoice.ItemIndex = 1;
  frameExternalFileValues.Enabled := rgExternalFlowChoice.ItemIndex = 2;
  rgReferenceTimeChoice.Enabled := frameExternalFileValues.Enabled;
  if fedExternalFileName.Enabled then
  begin
    rgReferenceTimeChoice.Enabled := False;
    rgReferenceTimeChoice.ItemIndex := 1;
  end;
  if frameExternalFileValues.Enabled then
  begin
    frameExternalFileValues.Grid.Color := clWindow;
    rgReferenceTimeChoice.ItemIndex := 0;
  end
  else
  begin
    frameExternalFileValues.Grid.Color := clBtnFace;
  end;
end;

procedure TframeScreenObjectSFR.rgGagesClick(Sender: TObject);
var
  FlowTypes: TStringList;
begin
  cbGagStandard.Enabled := rgGages.ItemIndex <> 0;
  cbGag1.Enabled := rgGages.ItemIndex <> 0;
  cbGag2.Enabled := rgGages.ItemIndex <> 0;
  cbGag3.Enabled := rgGages.ItemIndex <> 0;
  cbGag5.Enabled := rgGages.ItemIndex <> 0;
  cbGag6.Enabled := rgGages.ItemIndex <> 0;
  cbGag7.Enabled := rgGages.ItemIndex <> 0;

{$IFDEF PEST}
  tabObservations.TabVisible := (rgGages.ItemIndex in [1,2,3])
    and (FListCount = 1);
  if rgGages.ItemIndex = 3 then
  begin
    FlowTypes := TStringList.Create;
    try
      FlowTypes.Add(StreamGageOutputTypes[StreamGageOutputTypes.Count-1]);
      frameSfrPestObs.SpecifyObservationTypes(FlowTypes);
    finally
      FlowTypes.Free;
    end;
  end
  else
  begin
    frameSfrPestObs.SpecifyObservationTypes(StreamGageOutputTypes);
  end;
{$ENDIF}

end;

procedure TframeScreenObjectSFR.btnInserParametersClick(Sender: TObject);
var
  ARow: integer;
begin
  if rdgParameters.SelectedRow > 0 then
  begin
    ARow := rdgParameters.SelectedRow;
    rdgParameters.InsertRow(ARow);
    dgFlowTimes.InsertRow(ARow);
    dgUp.InsertRow(ARow);
    dgDown.InsertRow(ARow);
    dgSfrEquation.InsertRow(ARow);
    frameCrossSection1.InsertItem(ARow-1);
    frameFlowTable1.InsertIntem(ARow-1);
    Inc(FTimeCount);
    if dgSfrRough.SelectedRow >= 1 then
    begin
      frameCrossSection1.DisplayData(dgSfrRough.SelectedRow-1);
    end;
    if dgTableTime.SelectedRow >= 1 then
    begin
      frameFlowTable1.DisplayData(dgTableTime.SelectedRow-1);
    end;

    seParametersCount.AsInteger := seParametersCount.AsInteger +1;
    seParametersCount.OnChange(nil);
  end;
end;

procedure TframeScreenObjectSFR.cbSfrGagClick(Sender: TObject);
begin
  with Sender as TCheckBox do
  begin
    AllowGrayed := State = cbGrayed;
  end;
end;

Function TframeScreenObjectSFR.IcalcRowSet(Row: integer): TByteSet;
var
  Icalc: integer;
begin
  result := [];
  if Row <= 0 then
  begin
    Exit;
  end;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    result := [0,1,2,3,4];
    Exit;
  end;
  if rdgParameters.Cells[Ord(spicIcalc), Row] = '' then
  begin
    result := [0,1,2,3,4];
    Exit;
  end
  else
  begin
    Icalc := rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
      rdgParameters.Cells[Ord(spicIcalc), Row]);
    Assert(Icalc >= 0);
    Assert(Icalc <= 4);
    Include(result, Icalc);
  end;
end;

Function TframeScreenObjectSFR.IcalcSet: TByteSet;
var
  RowIndex: integer;
  Icalc: integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    result := [0,1,2,3,4];
    Exit;
  end;
  result := [];
  for RowIndex := 1 to rdgParameters.RowCount - 1 do
  begin
    if rdgParameters.Cells[Ord(spicIcalc), RowIndex] = '' then
    begin
      result := [0,1,2,3,4];
      Exit;
    end
    else
    begin
      Icalc := rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
        rdgParameters.Cells[Ord(spicIcalc), RowIndex]);
      Assert(Icalc >= 0);
      Assert(Icalc <= 4);
      Include(result, Icalc);
    end;
  end;
end;

procedure TframeScreenObjectSFR.EnableTabs;
var
  Icalc: TByteSet;
begin
  tabChannel.TabVisible := False;
  tabTable.TabVisible := False;
  tabEquation.TabVisible := False;
  TabUnsaturatedProperties.TabVisible := ISFROPT in [4,5];
  Icalc := IcalcSet;
  if 1 in Icalc then
  begin
    tabChannel.TabVisible := True;
  end;
  if 2 in Icalc then
  begin
    tabChannel.TabVisible := True;
  end;
  if 3 in Icalc then
  begin
    tabEquation.TabVisible := True;
  end;
  if 4 in Icalc then
  begin
    tabTable.TabVisible := True;
  end;
  EnableUnsatControls;
end;

procedure TframeScreenObjectSFR.comboIcalcChoiceChange(Sender: TObject);
begin
  AssignParamIcalcInGrid(Ord(spicIcalc), comboIcalcChoice.Text);
end;

procedure TframeScreenObjectSFR.comboMultiIpriorChange(Sender: TObject);
begin
  AssignMultipleIPrior(comboMultiIprior.Text, Ord(sncIprior));
end;

procedure TframeScreenObjectSFR.comboParameterChoicesChange(Sender: TObject);
begin
  AssignParamIcalcInGrid(Ord(spicParameter), comboParameterChoices.Text);
end;

procedure TframeScreenObjectSFR.UpdateKCaption;
var
  RowIndex: Integer;
  ParamUsed: Boolean;
begin
  ParamUsed := False;
  for RowIndex := 1 to rdgParameters.RowCount - 1 do
  begin
    if (rdgParameters.Cells[Ord(spicParameter), RowIndex] <> '')
      and (rdgParameters.Cells[Ord(spicParameter), RowIndex] <>
        rdgParameters.Columns[Ord(spicParameter)].PickList[0])
      and (rdgParameters.Cells[Ord(spicParamInstance), RowIndex] <> '') then
    begin
      ParamUsed := True;
      break;
    end;
  end;
  if ParamUsed then
  begin
    dgUp.Cells[Ord(scK),0]   := StrHydraulicCondMult;
    dgDown.Cells[Ord(scK),0] := StrHydraulicCondMult;
  end
  else
  begin
    dgUp.Cells[Ord(scK),0]   := StrHydraulicConductivi;
    dgDown.Cells[Ord(scK),0] := StrHydraulicConductivi;
  end;
end;

constructor TframeScreenObjectSFR.Create(AOwner: TComponent);
var
  TempLayer: TPositionedLayer;
  procedure AssignSegmentHeadings(dg: TStringGrid);
  begin
    dg.Cells[Ord(scStartTime),0]     := StrStartingTime;
    dg.Cells[Ord(scEndTime),0]       := StrEndingTime;
    dg.Cells[Ord(scK),0]             := StrHydraulicConductivi;
    dg.Cells[Ord(scBedThickness),0]  := StrStreambedThickness;
    dg.Cells[Ord(scBedElevation),0]  := StrStreambedElevation;
    dg.Cells[Ord(scStreamWidth),0]   := StrStreamWidth;
    dg.Cells[Ord(scStreamDepth),0]   := StrStreamDepth;
  end;
begin
  inherited;
  FUpdatingICalc := False;
  FGettingData := False;
  seParametersCountChange(nil);
  AssignSegmentHeadings(dgUp);
  AssignSegmentHeadings(dgDown);

  dgSfrRough.Cells[Ord(srStartTime),0]    := StrStartingTime;
  dgSfrRough.Cells[Ord(srEndTime),0]      := StrEndingTime;
  dgSfrRough.Cells[Ord(srChannelRough),0] := StrChannelRoughness;
  dgSfrRough.Cells[Ord(srBankRough),0]    := StrBankRoughness;

  rdgNetwork.Cells[Ord(sncStartTime),0]    := StrStartingTime;
  rdgNetwork.Cells[Ord(sncEndtime),0]      := StrEndingTime;
  rdgNetwork.Cells[Ord(sncOutflowSegment),0] := StrOutflowSegmentOUT;
  rdgNetwork.Cells[Ord(sncDiversionSegment),0]    := StrDiversionSegmentI;
  rdgNetwork.Cells[Ord(sncIprior),0]    := StrDiversionPriority;

  dgSfrEquation.Cells[Ord(seStartTime),0]  := StrStartingTime;
  dgSfrEquation.Cells[Ord(seEndTime),0]    := StrEndingTime;
  dgSfrEquation.Cells[Ord(seDepthCoeff),0] := StrDepthCoefficient;
  dgSfrEquation.Cells[Ord(seDepthExp),0]   := StrDepthExponent;
  dgSfrEquation.Cells[Ord(seWidthCoeff),0] := StrWidthCoefficient;
  dgSfrEquation.Cells[Ord(seWidthExp),0]   := StrWidthExponent;

  dgTableTime.Cells[Ord(sttStartTime),0]    := StrStartingTime;
  dgTableTime.Cells[Ord(sttEndTime),0]      := StrEndingTime;

  dgFlowTimes.ColWidths[Ord(sfcFlow)] := 100;
  dgFlowTimes.ColWidths[Ord(sfcPrecip)] := 100;
  dgFlowTimes.ColWidths[Ord(sfcEvap)] := 100;
  dgFlowTimes.ColWidths[Ord(sfcRunoff)] := 100;
  dgFlowTimes.Cells[Ord(sfcStartTime),0] := StrStartingTime;
  dgFlowTimes.Cells[Ord(sfcEndTime),0] := StrEndingTime;
  dgFlowTimes.Cells[Ord(sfcFlow),0] := StrFlowIntoUpstreamE;
  dgFlowTimes.Cells[Ord(sfcPrecip),0] := StrPrecipitationVolume;
  dgFlowTimes.Cells[Ord(sfcEvap),0] := StrEvapotranspirationV;
  dgFlowTimes.Cells[Ord(sfcRunoff),0] := StrRunoffVolumeRUNOF;

  rdgParameters.Cells[Ord(spicStartTime),0]  := StrStartingTime;
  rdgParameters.Cells[Ord(spicEndTime),0]    := StrEndingTime;
  rdgParameters.Cells[Ord(spicParameter),0] := StrParameter;
  rdgParameters.Cells[Ord(spicParamInstance),0] := StrParameterInstance;
  rdgParameters.Cells[Ord(spicIcalc),0]   := StrStageCalculationI;

  // TempLayer is used for painting the selected and old screen objects.
  TempLayer := zbChannel.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  TempLayer.OnPaint := PaintCrossSection;

  TempLayer := zbFlowDepthTable.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  TempLayer.OnPaint := PaintFlowDepth;

  TempLayer := zbFlowWidthTable.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  TempLayer.OnPaint := PaintFlowWidth;
end;

procedure TframeScreenObjectSFR.SetExternalFlowFile(Boundary: TSfrBoundary);
var
  FlowItem: TFlowFileItem;
  Inflow: Double;
  FlowTime: Double;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
  ExternalFlow: TExternalFlowProperties;
begin
  if tabExternalFlowFile.TabVisible then
  begin
    ExternalFlow := Boundary.ExternalFlow;
    ExternalFlow.FlowFileChoice := TFlowFileChoice(rgExternalFlowChoice.ItemIndex);
    case ExternalFlow.FlowFileChoice of
      ffcNone:
        begin
          ExternalFlow.Clear;
        end;
      ffcFileName:
        begin
          ExternalFlow.ReferenceTimeChoice := ffrtStartOfModel;
          ExternalFlow.FlowFileData.Clear;
          ExternalFlow.FullFlowFileName := fedExternalFileName.FileName;
        end;
      ffcSpecify:
        begin
          ExternalFlow.ReferenceTimeChoice := TFlowFileReferenceTime(rgReferenceTimeChoice.ItemIndex);
          ExternalFlow.FullFlowFileName := '';
          Grid := frameExternalFileValues.Grid;
          ItemIndex := 0;
          for RowIndex := 1 to frameExternalFileValues.Grid.RowCount - 1 do
          begin
            if TryStrToFloat(Grid.Cells[0, RowIndex], FlowTime) and TryStrToFloat(Grid.Cells[1, RowIndex], Inflow) then
            begin
              if ExternalFlow.FlowFileData.Count > ItemIndex then
              begin
                FlowItem := ExternalFlow.FlowFileData[ItemIndex];
              end
              else
              begin
                FlowItem := ExternalFlow.FlowFileData.Add;
              end;
              FlowItem.Time := FlowTime;
              FlowItem.Inflow := Inflow;
              Inc(ItemIndex);
            end;
          end;
          while ExternalFlow.FlowFileData.Count > ItemIndex do
          begin
            ExternalFlow.FlowFileData.Delete(ExternalFlow.FlowFileData.Count - 1);
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetExternalFlows(Boundary: TSfrBoundary);
var
  FlowItem: TFlowFileItem;
  ExternalFlow: TExternalFlowProperties;
  FlowIndex: Integer;
begin
  if tabExternalFlowFile.TabVisible then
  begin
    ExternalFlow := Boundary.ExternalFlow;
    rgExternalFlowChoice.ItemIndex := Ord(ExternalFlow.FlowFileChoice);
    rgExternalFlowChoiceClick(nil);
    rgReferenceTimeChoice.ItemIndex := Ord(ExternalFlow.ReferenceTimeChoice);
    try
      fedExternalFileName.FileName := ExternalFlow.FullFlowFileName;
    except on EComboEditError do
      begin
        // do nothing.
      end;
    end;
    frameExternalFileValues.seNumber.AsInteger := ExternalFlow.FlowFileData.Count;
    for FlowIndex := 0 to ExternalFlow.FlowFileData.Count - 1 do
    begin
      FlowItem := ExternalFlow.FlowFileData[FlowIndex];
      frameExternalFileValues.Grid.Cells[0, FlowIndex + 1] := FloatToStr(FlowItem.Time);
      frameExternalFileValues.Grid.Cells[1, FlowIndex + 1] := FloatToStr(FlowItem.Inflow);
    end;
  end;
end;

procedure TframeScreenObjectSFR.AssignMultipleIPrior(NewText: string; ColIndex: Integer);
var
  RowIndex: Integer;
begin
  for RowIndex := rdgNetwork.FixedRows to rdgNetwork.RowCount - 1 do
  begin
    if rdgNetwork.IsSelectedCell(ColIndex, RowIndex) then
    begin
      rdgNetwork.Cells[ColIndex, RowIndex] := NewText;
      if Assigned(rdgNetwork.OnSetEditText) then
      begin
        rdgNetwork.OnSetEditText(rdgNetwork, ColIndex, RowIndex, NewText);
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.UpdateICalc;
var
  Index: Integer;
  FirstChangeMade, SecondChangeMade: boolean;
begin
  if FGettingData or FInitializing then
  begin
    Exit;
  end;
  if FUpdatingICalc then
  begin
    Exit;
  end;
  FUpdatingICalc := True;
  try

  FirstChangeMade := False;
  SecondChangeMade := False;
  if ISFROPT > 1 then
  begin
    if (rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
      rdgParameters.Cells[Ord(spicIcalc), 1]) in [1, 2]) then
    begin
      for Index := 2 to rdgParameters.RowCount - 1 do
      begin
        if rdgParameters.Cells[Ord(spicIcalc), Index]
          <> rdgParameters.Cells[Ord(spicIcalc), 1] then
        begin
          FirstChangeMade := True;
        end;
        rdgParameters.Cells[Ord(spicIcalc), Index] :=
          rdgParameters.Cells[Ord(spicIcalc), 1];
        rdgParameters.OnSetEditText(rdgParameters, Ord(spicIcalc), Index,
          rdgParameters.Cells[Ord(spicIcalc), Index]);
      end;
    end
    else
    begin
      for Index := 2 to rdgParameters.RowCount - 1 do
      begin
        if (rdgParameters.Columns[Ord(spicIcalc)].PickList.IndexOf(
          rdgParameters.Cells[Ord(spicIcalc), Index]) in [1, 2]) then
        begin
          if rdgParameters.Cells[Ord(spicIcalc), Index] <>
            rdgParameters.Cells[Ord(spicIcalc), 1] then
          begin
            SecondChangeMade := True;
          end;
          rdgParameters.Cells[Ord(spicIcalc), Index] :=
            rdgParameters.Cells[Ord(spicIcalc), 1];
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicIcalc), Index,
            rdgParameters.Cells[Ord(spicIcalc), Index]);
        end;
      end;
    end;
  end;
  if (FirstChangeMade or SecondChangeMade) and not FGettingData then
  begin
    Beep;
    if FirstChangeMade then
    begin
      MessageDlg(StrSorryICALCMustBe, mtInformation, [mbOK], 0);
    end;
    if SecondChangeMade then
    begin
      MessageDlg(StrSorryICALCCanNot, mtInformation, [mbOK], 0);
    end;
  end;
  finally
    FUpdatingICalc := False;
  end;
end;

procedure TframeScreenObjectSFR.UpdatedTimesInSfrGrids(const Value: string; ACol: Integer; ARow: Integer);
begin
  dgFlowTimes.Cells[ACol, ARow] := Value;
  dgUp.Cells[ACol, ARow] := Value;
  dgDown.Cells[ACol, ARow] := Value;
  dgSfrRough.Cells[ACol, ARow] := Value;
  dgSfrEquation.Cells[ACol, ARow] := Value;
  dgTableTime.Cells[ACol, ARow] := Value;
  rdgNetwork.Cells[ACol, ARow] := Value;
end;

procedure TframeScreenObjectSFR.AssignParamIcalcInGrid(ColIndex: Integer; NewText: string);
var
  RowIndex: Integer;
begin
  for RowIndex := rdgParameters.FixedRows to rdgParameters.RowCount - 1 do
  begin
    if rdgParameters.IsSelectedCell(ColIndex, RowIndex) then
    begin
      rdgParameters.Cells[ColIndex, RowIndex] := NewText;
      if Assigned(rdgParameters.OnSetEditText) then
      begin
        rdgParameters.OnSetEditText(rdgParameters, ColIndex, RowIndex, NewText);
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.UpdateSpinEditValue(DataGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
begin
  if SpinEdit.AsInteger < DataGrid.RowCount - 1 then
  begin
    SpinEdit.AsInteger := DataGrid.RowCount - 1;
    SpinEdit.OnChange(SpinEdit);
  end;
end;

procedure TframeScreenObjectSFR.Edited;
begin
  if Assigned(FOnEdited) then
  begin
    FOnEdited(self);
  end;
end;

procedure TframeScreenObjectSFR.EnableMultiEditControl(EdControl: TControl; DataGrid: TRbwDataGrid4);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  ShouldEnable := False;
  for RowIndex := DataGrid.FixedRows to DataGrid.RowCount - 1 do
  begin
    for ColIndex := 2 to DataGrid.ColCount - 1 do
    begin
      ShouldEnable := DataGrid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  EdControl.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectSFR.AssignSelectedCellsInGrid(
  DataGrid: TRbwDataGrid4; const NewText: string);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  for RowIndex := DataGrid.FixedRows to DataGrid.RowCount - 1 do
  begin
    for ColIndex := 2 to DataGrid.ColCount - 1 do
    begin
      if DataGrid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        DataGrid.Cells[ColIndex, RowIndex] := NewText;
        if Assigned(DataGrid.OnSetEditText) then
        begin
          DataGrid.OnSetEditText(DataGrid, ColIndex, RowIndex, NewText);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.Initialize;
var
  Index: Integer;
  Param: TModflowTransientListParameter;
  ARect: TGridRect;
begin
  FInitializing := True;
  try
    GetStartTimes(dgFlowTimes, Ord(sfcStartTime));
    GetEndTimes(dgFlowTimes, Ord(sfcEndTime));
    GetStartTimes(dgUp, Ord(scStartTime));
    GetEndTimes(dgUp, Ord(scEndTime));
    GetStartTimes(dgDown, Ord(scStartTime));
    GetEndTimes(dgDown, Ord(scEndTime));
    GetStartTimes(dgSfrRough, Ord(srStartTime));
    GetEndTimes(dgSfrRough, Ord(srEndTime));
    GetStartTimes(dgSfrEquation, Ord(seStartTime));
    GetEndTimes(dgSfrEquation, Ord(seEndTime));
    GetStartTimes(dgTableTime, Ord(sttStartTime));
    GetEndTimes(dgTableTime, Ord(sttEndTime));
    GetStartTimes(rdgNetwork, Ord(sncStartTime));
    GetEndTimes(rdgNetwork, Ord(sncEndtime));

    GetStartTimes(rdgParameters, Ord(spicStartTime));
    GetEndTimes(rdgParameters, Ord(spicEndTime));

    rdgParameters.Columns[Ord(spicParameter)].PickList.Clear;
    rdgParameters.Columns[Ord(spicParameter)].PickList.Add(StrNone);
    comboParameterChoices.Items.Clear;
    comboParameterChoices.Items.Add.Text := StrNone;
    for Index := 0 to frmGoPhast.PhastModel.ModflowTransientParameters.Count - 1 do
    begin
      Param := frmGoPhast.PhastModel.ModflowTransientParameters[Index];
      if Param.ParameterType = ptSFR then
      begin
        rdgParameters.Columns[Ord(spicParameter)].PickList.Add(Param.ParameterName);
        comboParameterChoices.Items.Add.Text := Param.ParameterName;
      end;
    end;

    seParametersCount.AsInteger := 1;
    seParametersCount.OnChange(seParametersCount);

    comboParameterChoices.ItemIndex := -1;
    EnableTabs;
    pcSFR.ActivePageIndex := 0;

    ARect.Left := 2;
    ARect.Right := 2;
    ARect.Top := 1;
    ARect.Bottom := 1;
    rdgNetwork.Selection := ARect;
    dgFlowTimes.Selection := ARect;
    dgUp.Selection := ARect;
    dgDown.Selection := ARect;
    dgSfrRough.Selection := ARect;
    dgSfrEquation.Selection := ARect;

    rgExternalFlowChoice.ItemIndex := 0;
    rgReferenceTimeChoice.ItemIndex := 0;
    fedExternalFileName.FileName := '';
    ClearTable(frameExternalFileValues.Grid);
  finally
    FInitializing := False;
  end;

end;

procedure TframeScreenObjectSFR.ClearTable(Grid: TStringGrid);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if Grid is TRbwDataGrid4 then
  begin
    ClearGrid(TRbwDataGrid4(Grid));
  end
  else
  begin
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
      end;
    end
  end;
end;

procedure TframeScreenObjectSFR.GetSfrFlows(Boundary: TSfrBoundary; FoundFirst: Boolean);
var
  TableIndex: Integer;
  Row: integer;
  FlowItem: TSfrSegmentFlowItem;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger = Boundary.SegmentFlows.Count then
    begin
      for TableIndex := 0 to Boundary.SegmentFlows.Count - 1 do
      begin
        FlowItem := Boundary.SegmentFlows.Items[TableIndex] as TSfrSegmentFlowItem;
        Row := LocateRowFromStartAndEndTimes(FlowItem.StartTime, FlowItem.EndTime);
        if Row >= 1 then
        begin
  //        Row := TableIndex + 1;
          if dgFlowTimes.Cells[Ord(sfcStartTime), Row] <> FloatToStr(FlowItem.StartTime) then
          begin
            dgFlowTimes.Cells[Ord(sfcStartTime), Row] := '';
          end;
          if dgFlowTimes.Cells[Ord(sfcEndTime), Row] <> FloatToStr(FlowItem.EndTime) then
          begin
            dgFlowTimes.Cells[Ord(sfcEndTime), Row] := '';
          end;
          if dgFlowTimes.Cells[Ord(sfcFlow), Row] <> FlowItem.Flow then
          begin
            dgFlowTimes.Cells[Ord(sfcFlow), Row] := '';
          end;
          if dgFlowTimes.Cells[Ord(sfcPrecip), Row] <> FlowItem.Precipitation then
          begin
            dgFlowTimes.Cells[Ord(sfcPrecip), Row] := '';
          end;
          if dgFlowTimes.Cells[Ord(sfcEvap), Row] <> FlowItem.Evapotranspiration then
          begin
            dgFlowTimes.Cells[Ord(sfcEvap), Row] := '';
          end;
          if dgFlowTimes.Cells[Ord(sfcRunoff), Row] <> FlowItem.Runnoff then
          begin
            dgFlowTimes.Cells[Ord(sfcRunoff), Row] := '';
          end;
        end
        else
        begin
          ClearTable(dgFlowTimes);
          break;
        end;
      end;
    end
    else
    begin
      ClearTable(dgFlowTimes);
    end;
  end
  else
  begin
    Assert(seParametersCount.AsInteger >= Boundary.SegmentFlows.Count);
    for TableIndex := 0 to Boundary.SegmentFlows.Count - 1 do
    begin
      FlowItem := Boundary.SegmentFlows.Items[TableIndex] as TSfrSegmentFlowItem;
      Row := LocateRowFromStartAndEndTimes(FlowItem.StartTime, FlowItem.EndTime);
      if Row >= 1 then
      begin
        dgFlowTimes.Cells[Ord(sfcStartTime), Row] := FloatToStr(FlowItem.StartTime);
        dgFlowTimes.Cells[Ord(sfcEndTime), Row] := FloatToStr(FlowItem.EndTime);
        dgFlowTimes.Cells[Ord(sfcFlow), Row] := FlowItem.Flow;
        dgFlowTimes.Cells[Ord(sfcPrecip), Row] := FlowItem.Precipitation;
        dgFlowTimes.Cells[Ord(sfcEvap), Row] := FlowItem.Evapotranspiration;
        dgFlowTimes.Cells[Ord(sfcRunoff), Row] := FlowItem.Runnoff;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetSfrFlowTable(Boundary: TSfrBoundary;
  FoundFirst: Boolean);
var
  TablelItem: TSfrTablelItem;
  TableIndex: integer;
  Row: integer;
  procedure ClearAll;
  begin
    ClearTable(dgTableTime);
    frameFlowTable1.ClearTable;
  end;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger = Boundary.TableCollection.Count then
    begin
      for TableIndex := 0 to Boundary.TableCollection.Count - 1 do
      begin
        TablelItem := Boundary.TableCollection.Items[TableIndex] as TSfrTablelItem;
        Row := self.LocateRowFromStartAndEndTimes(TablelItem.StartTime, TablelItem.EndTime);
        if Row >= 1 then
        begin
          if dgTableTime.Cells[Ord(sttStartTime), Row] <> FloatToStr(TablelItem.StartTime) then
          begin
            dgTableTime.Cells[Ord(sttStartTime), Row] := '';
          end;
          if dgTableTime.Cells[Ord(sttEndTime), Row] <> FloatToStr(TablelItem.EndTime) then
          begin
            dgTableTime.Cells[Ord(sttEndTime), Row] := '';
          end;

        end
        else
        begin
          ClearAll;
          break;
        end;
      end;
    end
    else
    begin
      ClearAll;
    end;
  end
  else
  begin
    while Boundary.TableCollection.Count > seParametersCount.AsInteger do
    begin
      Boundary.TableCollection.Last.Free;
    end;
    Assert(seParametersCount.AsInteger >= Boundary.TableCollection.Count);

    for TableIndex := 0 to Boundary.TableCollection.Count - 1 do
    begin
      TablelItem := Boundary.TableCollection.Items[TableIndex] as TSfrTablelItem;
      Row := self.LocateRowFromStartAndEndTimes(TablelItem.StartTime, TablelItem.EndTime);
      if Row >= 1 then
      begin
        dgTableTime.Cells[Ord(sttStartTime), Row] := FloatToStr(TablelItem.StartTime);
        dgTableTime.Cells[Ord(sttEndTime), Row] := FloatToStr(TablelItem.EndTime);

      end;
    end;
  end;
  FrameFlowTable1.TableCountChanged := False;
end;

procedure TframeScreenObjectSFR.GetSfrSegments(Boundary: TSfrBoundary; FoundFirst: Boolean);
  procedure AssignSegmentTable(SegmentCollection: TSfrSegmentCollection;
    DataGrid: TStringGrid);
  var
    TableIndex: Integer;
    SegmentItem : TSfrSegmentItem;
    Row: integer;
  begin
    for TableIndex := 0 to SegmentCollection.Count - 1 do
    begin
      SegmentItem := SegmentCollection.Items[TableIndex] as TSfrSegmentItem;
      Row := LocateRowFromStartAndEndTimes(SegmentItem.StartTime, SegmentItem.EndTime);
      if Row >= 1 then
      begin
        DataGrid.Cells[Ord(scStartTime), Row] := FloatToStr(SegmentItem.StartTime);
        DataGrid.Cells[Ord(scEndTime), Row] := FloatToStr(SegmentItem.EndTime);
        DataGrid.Cells[Ord(scK), Row] := SegmentItem.HydraulicConductivity;
        DataGrid.Cells[Ord(scBedThickness), Row] := SegmentItem.StreamBedThickness;
        DataGrid.Cells[Ord(scBedElevation), Row] := SegmentItem.StreambedElevation;
        DataGrid.Cells[Ord(scStreamWidth), Row] := SegmentItem.StreamWidth;
        DataGrid.Cells[Ord(scStreamDepth), Row] := SegmentItem.StreamDepth;
      end;
    end;
  end;
  procedure CheckSegmentTable(SegmentCollection: TSfrSegmentCollection;
    DataGrid: TStringGrid);
  var
    TableIndex: Integer;
    SegmentItem : TSfrSegmentItem;
    Row: integer;
  begin
    for TableIndex := 0 to SegmentCollection.Count - 1 do
    begin
      SegmentItem := SegmentCollection.Items[TableIndex] as TSfrSegmentItem;
      Row := LocateRowFromStartAndEndTimes(SegmentItem.StartTime, SegmentItem.EndTime);
      if Row >= 1 then
      begin
        if DataGrid.Cells[Ord(scStartTime), Row] <> FloatToStr(SegmentItem.StartTime) then
        begin
          DataGrid.Cells[Ord(scStartTime), Row] := '';
        end;
        if DataGrid.Cells[Ord(scEndTime), Row] <> FloatToStr(SegmentItem.EndTime) then
        begin
          DataGrid.Cells[Ord(scEndTime), Row] := '';
        end;
        if DataGrid.Cells[Ord(scK), Row] <> SegmentItem.HydraulicConductivity then
        begin
          DataGrid.Cells[Ord(scK), Row] := '';
        end;
        if DataGrid.Cells[Ord(scBedThickness), Row] <> SegmentItem.StreamBedThickness then
        begin
          DataGrid.Cells[Ord(scBedThickness), Row] := '';
        end;
        if DataGrid.Cells[Ord(scBedElevation), Row] <> SegmentItem.StreambedElevation then
        begin
          DataGrid.Cells[Ord(scBedElevation), Row] := '';
        end;
        if DataGrid.Cells[Ord(scStreamWidth), Row] <> SegmentItem.StreamWidth then
        begin
          DataGrid.Cells[Ord(scStreamWidth), Row] := '';
        end;
        if DataGrid.Cells[Ord(scStreamDepth), Row] <> SegmentItem.StreamDepth then
        begin
          DataGrid.Cells[Ord(scStreamDepth), Row] := '';
        end;
      end
      else
      begin
        ClearTable(DataGrid);
        break;
      end
    end;
  end;
var
  Row: Integer;
  Column: Integer;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger = Boundary.UpstreamSegmentValues.Count then
    begin
      CheckSegmentTable(Boundary.UpstreamSegmentValues, dgUp);
      CheckSegmentTable(Boundary.DownstreamSegmentValues, dgDown);
    end
    else
    begin
      for Row := 1 to dgUp.RowCount - 1 do
      begin
        for Column := Ord(scK) to dgUp.ColCount - 1 do
        begin
          dgUp.Cells[Column, Row] := '';
        end;
      end;
      for Row := 1 to dgDown.RowCount - 1 do
      begin
        for Column := Ord(scK) to dgDown.ColCount - 1 do
        begin
          dgDown.Cells[Column, Row] := '';
        end;
      end;
    end;
  end
  else
  begin
    Assert(seParametersCount.AsInteger >= Boundary.UpstreamSegmentValues.Count);
    Assert(Boundary.UpstreamSegmentValues.Count = Boundary.DownstreamSegmentValues.Count);

    AssignSegmentTable(Boundary.UpstreamSegmentValues, dgUp);
    AssignSegmentTable(Boundary.DownstreamSegmentValues, dgDown);
  end;
end;

function TframeScreenObjectSFR.LocateRowFromStartAndEndTimes(
  StartTime, EndTime: double): integer;
var
  Index: Integer;
  STime, ETime: string;
begin
  result := -1;
  STime := FloatToStr(StartTime);
  ETime := FloatToStr(EndTime);
  for Index := 1 to rdgParameters.RowCount - 1 do
  begin
    if (rdgParameters.Cells[Ord(spicStartTime), Index] = STime)
      and (rdgParameters.Cells[Ord(spicEndTime), Index] = ETime) then
    begin
      result := Index;
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetParamIcalcValues(Boundary: TSfrBoundary;
  FoundFirst: Boolean);
var
  TableIndex: Integer;
  Item: TSfrParamIcalcItem;
  Row: Integer;
  Parname: string;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger <> Boundary.ParamIcalc.Count then
    begin
      ClearTable(rdgParameters);
      ClearTable(rdgNetwork);
    end
    else
    begin
      for TableIndex := 0 to Boundary.ParamIcalc.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.Items[TableIndex];
        Row := TableIndex + 1;

        if rdgParameters.Cells[Ord(spicStartTime), Row] <> FloatToStr(Item.StartTime) then
        begin
          rdgParameters.Cells[Ord(spicStartTime), Row] := '';
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicStartTime), Row, '');
          rdgNetwork.Cells[Ord(sncStartTime), Row] := '';
          rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncStartTime), Row, '');
        end;
        if rdgParameters.Cells[Ord(spicEndTime), Row] <> FloatToStr(Item.EndTime) then
        begin
          rdgParameters.Cells[Ord(spicEndTime), Row] := '';
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicEndTime), Row, '');
          rdgNetwork.Cells[Ord(sncEndtime), Row] := '';
          rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncEndtime), Row, '');
        end;
        if Item.Param = '' then
        begin
          Parname :=
            rdgParameters.Columns[Ord(spicParameter)].PickList[0];
        end
        else
        begin
          Parname := Item.Param;
        end;
        if rdgParameters.Cells[Ord(spicParameter), Row] <> Parname then
        begin
          rdgParameters.Cells[Ord(spicParameter), Row] := '';
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicParameter), Row, '');
        end;
        if rdgParameters.Cells[Ord(spicParamInstance), Row] <> Item.ParamInstance then
        begin
          rdgParameters.Cells[Ord(spicParamInstance), Row] := '';
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicParamInstance), Row, '');
        end;
        if rdgParameters.Cells[Ord(spicIcalc), Row] <>
          rdgParameters.Columns[Ord(spicIcalc)].PickList[Item.Icalc] then
        begin
          rdgParameters.Cells[Ord(spicIcalc), Row] := '';
          rdgParameters.OnSetEditText(rdgParameters, Ord(spicIcalc), Row, '');
        end;
        if rdgNetwork.Cells[Ord(sncOutflowSegment), Row] <>
          IntToStr(Item.OutflowSegment) then
        begin
          rdgNetwork.Cells[Ord(sncOutflowSegment), Row] := '';
          rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncOutflowSegment), Row, '');
        end;
        if rdgNetwork.Cells[Ord(sncDiversionSegment), Row] <>
          IntToStr(Item.DiversionSegment) then
        begin
          rdgNetwork.Cells[Ord(sncDiversionSegment), Row] := '';
          rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncDiversionSegment), Row, '');
        end;
        if rdgNetwork.Cells[Ord(sncIprior), Row] <>
          rdgNetwork.Columns[Ord(sncIprior)].PickList[-Item.IPRIOR] then
        begin
          rdgNetwork.Cells[Ord(sncIprior), Row] := '';
          rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncIprior), Row, '');
        end;
      end
    end;
  end
  else
  begin
    seParametersCount.AsInteger := Boundary.ParamIcalc.Count;
    seParametersCount.OnChange(seParametersCount);
    for TableIndex := 0 to Boundary.ParamIcalc.Count - 1 do
    begin
      Item := Boundary.ParamIcalc.Items[TableIndex];
      Row := TableIndex + 1;

      rdgParameters.Cells[Ord(spicStartTime), Row] := FloatToStr(Item.StartTime);
      rdgParameters.OnSetEditText(rdgParameters, Ord(spicStartTime), Row,
        rdgParameters.Cells[Ord(spicStartTime), Row]);
      rdgParameters.Cells[Ord(spicEndTime), Row] := FloatToStr(Item.EndTime);
      rdgParameters.OnSetEditText(rdgParameters, Ord(spicEndTime), Row,
        rdgParameters.Cells[Ord(spicEndTime), Row]);

      rdgNetwork.Cells[Ord(sncStartTime), Row] := FloatToStr(Item.StartTime);
      rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncStartTime), Row,
        rdgNetwork.Cells[Ord(sncStartTime), Row]);
      rdgNetwork.Cells[Ord(sncEndtime), Row] := FloatToStr(Item.EndTime);
      rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncEndtime), Row,
        rdgNetwork.Cells[Ord(sncEndtime), Row]);

      if Item.Param = '' then
      begin
        rdgParameters.Cells[Ord(spicParameter), Row] :=
          rdgParameters.Columns[Ord(spicParameter)].PickList[0];
      end
      else
      begin
        rdgParameters.Cells[Ord(spicParameter), Row] := Item.Param;
      end;
      rdgParameters.OnSetEditText(rdgParameters, Ord(spicParameter), Row,
        rdgParameters.Cells[Ord(spicParameter), Row]);

      if Item.Param = '' then
      begin
        rdgParameters.Cells[Ord(spicParamInstance), Row] := '';
      end
      else
      begin
        rdgParameters.Cells[Ord(spicParamInstance), Row] := Item.ParamInstance;
      end;
      rdgParameters.OnSetEditText(rdgParameters, Ord(spicParamInstance), Row,
        rdgParameters.Cells[Ord(spicParamInstance), Row]);

      rdgParameters.Cells[Ord(spicIcalc), Row] :=
        rdgParameters.Columns[Ord(spicIcalc)].PickList[Item.Icalc];
      rdgParameters.OnSetEditText(rdgParameters, Ord(spicIcalc), Row,
        rdgParameters.Cells[Ord(spicIcalc), Row]);

      rdgNetwork.Cells[Ord(sncOutflowSegment), Row] := IntToStr(Item.OutflowSegment);
      rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncOutflowSegment), Row,
        rdgNetwork.Cells[Ord(sncOutflowSegment), Row]);
      rdgNetwork.Cells[Ord(sncDiversionSegment), Row] := IntToStr(Item.DiversionSegment);
      rdgNetwork.OnSetEditText(rdgNetwork, Ord(sncDiversionSegment), Row,
        rdgNetwork.Cells[Ord(sncDiversionSegment), Row]);

      rdgNetwork.Cells[Ord(spicIcalc), Row] :=
        rdgNetwork.Columns[Ord(spicIcalc)].PickList[-Item.IPRIOR];
      rdgNetwork.OnSetEditText(rdgNetwork, Ord(spicIcalc), Row,
        rdgNetwork.Cells[Ord(spicIcalc), Row]);
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetSfrValues(Boundary: TSfrBoundary;
  FoundFirst: Boolean);
var
  Item: TSfrItem;
begin
  if FoundFirst then
  begin
    if Boundary.Values.Count >= 1 then
    begin
      Assert(Boundary.Values.Count = 1);
      Item := Boundary.Values.Items[0] as TSfrItem;
      if jvcReachLength.Text <> Item.ReachLength then
      begin
        jvcReachLength.Text := '';
      end;
      if jceStreamTop.Text <> Item.StreambedElevation then
      begin
        jceStreamTop.Text := '';
      end;
      if jceSlope.Text <> Item.StreamSlope then
      begin
        jceSlope.Text := '';
      end;
      if jceStreambedThickness.Text <> Item.StreamBedThickness then
      begin
        jceStreambedThickness.Text := '';
      end;
      if jceStreambedK.Text <> Item.HydraulicConductivity then
      begin
        jceStreambedK.Text := '';
      end;
      if jceSaturatedVolumetricWater.Text <> Item.SaturatedWaterContent then
      begin
        jceSaturatedVolumetricWater.Text := '';
      end;
      if jceInitialVolumetricWater.Text <> Item.InitialWaterContent then
      begin
        jceInitialVolumetricWater.Text := '';
      end;
      if jceBrooksCoreyExponent.Text <> Item.BrooksCoreyExponent then
      begin
        jceBrooksCoreyExponent.Text := '';
      end;
      if jceMaxUnsaturatedKz.Text <> Item.VerticalK then
      begin
        jceMaxUnsaturatedKz.Text := '';
      end;
    end;
  end
  else
  begin
    if Boundary.Values.Count >= 1 then
    begin
      Assert(Boundary.Values.Count = 1);
      Item := Boundary.Values.Items[0] as TSfrItem;

      jvcReachLength.Text := Item.ReachLength;
      jceStreamTop.Text := Item.StreambedElevation;
      jceSlope.Text := Item.StreamSlope;
      jceStreambedThickness.Text := Item.StreamBedThickness;
      jceStreambedK.Text := Item.HydraulicConductivity;
      jceSaturatedVolumetricWater.Text := Item.SaturatedWaterContent;
      jceInitialVolumetricWater.Text := Item.InitialWaterContent;
      jceBrooksCoreyExponent.Text := Item.BrooksCoreyExponent;
      jceMaxUnsaturatedKz.Text := Item.VerticalK;
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetStartTimes(Grid: TRbwDataGrid4; Col: integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(Grid, Col);
end;

procedure TframeScreenObjectSFR.GetUnsaturatedValues(Boundary: TSfrBoundary;
  FoundFirst: Boolean);
var
  UnsatItem : TSfrUnsatSegmentItem;
begin
  if FoundFirst then
  begin
    if Boundary.UpstreamUnsatSegmentValues.Count >= 1 then
    begin
      Assert(Boundary.UpstreamUnsatSegmentValues.Count = 1);
      UnsatItem := Boundary.UpstreamUnsatSegmentValues.Items[0] as TSfrUnsatSegmentItem;

      if jceSaturatedVolumetricWaterUpstream.Text <> UnsatItem.SaturatedWaterContent then
      begin
        jceSaturatedVolumetricWaterUpstream.Text := '';
      end;
      if jceInitialVolumetricWaterUpstream.Text <> UnsatItem.InitialWaterContent then
      begin
        jceInitialVolumetricWaterUpstream.Text := '';
      end;
      if jceBrooksCoreyExponentUpstream.Text <> UnsatItem.BrooksCoreyExponent then
      begin
        jceBrooksCoreyExponentUpstream.Text := '';
      end;
      if jceMaxUnsaturatedKzUpstream.Text <> UnsatItem.VerticalSaturatedK then
      begin
        jceMaxUnsaturatedKzUpstream.Text := '';
      end;

      Assert(Boundary.DownstreamUnsatSegmentValues.Count = 1);
      UnsatItem := Boundary.DownstreamUnsatSegmentValues.Items[0] as TSfrUnsatSegmentItem;


      if jceSaturatedVolumetricWaterDownstream.Text <> UnsatItem.SaturatedWaterContent then
      begin
        jceSaturatedVolumetricWaterDownstream.Text := '';
      end;
      if jceInitialVolumetricWaterDownstream.Text <> UnsatItem.InitialWaterContent then
      begin
        jceInitialVolumetricWaterDownstream.Text := '';
      end;
      if jceBrooksCoreyExponentDownstream.Text <> UnsatItem.BrooksCoreyExponent then
      begin
        jceBrooksCoreyExponentDownstream.Text := '';
      end;
      if jceMaxUnsaturatedKzDownstream.Text <> UnsatItem.VerticalSaturatedK then
      begin
        jceMaxUnsaturatedKzDownstream.Text := '';
      end;
    end
    else
    begin
      Assert(Boundary.DownstreamUnsatSegmentValues.Count = 0);
    end;
  end
  else
  begin
    if Boundary.UpstreamUnsatSegmentValues.Count >= 1 then
    begin
      Assert(Boundary.UpstreamUnsatSegmentValues.Count = 1);
      UnsatItem := Boundary.UpstreamUnsatSegmentValues.Items[0] as TSfrUnsatSegmentItem;

      jceSaturatedVolumetricWaterUpstream.Text := UnsatItem.SaturatedWaterContent;
      jceInitialVolumetricWaterUpstream.Text := UnsatItem.InitialWaterContent;
      jceBrooksCoreyExponentUpstream.Text := UnsatItem.BrooksCoreyExponent;
      jceMaxUnsaturatedKzUpstream.Text := UnsatItem.VerticalSaturatedK;

      Assert(Boundary.DownstreamUnsatSegmentValues.Count = 1);
      UnsatItem := Boundary.DownstreamUnsatSegmentValues.Items[0] as TSfrUnsatSegmentItem;

      jceSaturatedVolumetricWaterDownstream.Text := UnsatItem.SaturatedWaterContent;
      jceInitialVolumetricWaterDownstream.Text := UnsatItem.InitialWaterContent;
      jceBrooksCoreyExponentDownstream.Text := UnsatItem.BrooksCoreyExponent;
      jceMaxUnsaturatedKzDownstream.Text := UnsatItem.VerticalSaturatedK;
    end
    else
    begin
      Assert(Boundary.DownstreamUnsatSegmentValues.Count = 0);
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetSfrChannel(Boundary: TSfrBoundary; FoundFirst: Boolean);
var
  TableIndex: Integer;
  Item: TSfrChannelItem;
  Row: integer;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger <> Boundary.ChannelValues.Count then
    begin
      if Boundary.ChannelValues.Count > 0 then
      begin
        ClearTable(dgSfrRough);
        frameCrossSection1.ClearTable;
      end;
    end
    else
    begin
      for TableIndex := 0 to Boundary.ChannelValues.Count - 1 do
      begin
        Item := Boundary.ChannelValues.Items[TableIndex];
        Row := LocateRowFromStartAndEndTimes(Item.StartTime, Item.EndTime);
        if Row >= 1 then
        begin
          if dgSfrRough.Cells[Ord(srStartTime), Row] <> FloatToStr(Item.StartTime) then
          begin
            dgSfrRough.Cells[Ord(srStartTime), Row] := '';
          end;
          if dgSfrRough.Cells[Ord(srEndTime), Row] <> FloatToStr(Item.EndTime) then
          begin
            dgSfrRough.Cells[Ord(srEndTime), Row] := '';
          end;
          if dgSfrRough.Cells[Ord(srChannelRough), Row] <> Item.ChannelRoughness then
          begin
            dgSfrRough.Cells[Ord(srChannelRough), Row] := '';
          end;
          if dgSfrRough.Cells[Ord(srBankRough), Row] <> Item.BankRoughness then
          begin
            dgSfrRough.Cells[Ord(srBankRough), Row] := '';
          end;
        end
        else
        begin
          ClearTable(dgSfrRough);
          frameCrossSection1.ClearTable;
          break;
        end;
      end;
    end;
  end
  else
  begin
    while Boundary.ChannelValues.Count > seParametersCount.AsInteger do
    begin
      Boundary.ChannelValues.Delete(Boundary.ChannelValues.Count-1);
    end;

    Assert(seParametersCount.AsInteger >= Boundary.ChannelValues.Count);
    for TableIndex := 0 to Boundary.ChannelValues.Count - 1 do
    begin
      Item := Boundary.ChannelValues.Items[TableIndex];
      Row := self.LocateRowFromStartAndEndTimes(Item.StartTime, Item.EndTime);
      if Row >= 1 then
      begin
        dgSfrRough.Cells[Ord(srStartTime), Row] := FloatToStr(Item.StartTime);
        dgSfrRough.Cells[Ord(srEndTime), Row] := FloatToStr(Item.EndTime);
        dgSfrRough.Cells[Ord(srChannelRough), Row] := Item.ChannelRoughness;
        dgSfrRough.Cells[Ord(srBankRough), Row] := Item.BankRoughness;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.GetSfrEquation(Boundary: TSfrBoundary;
  FoundFirst: Boolean);
var
  TableIndex: Integer;
  Item: TSfrEquationItem;
  Row: integer;
begin
  if FoundFirst then
  begin
    if seParametersCount.AsInteger = Boundary.EquationValues.Count then
    begin
      for TableIndex := 0 to Boundary.EquationValues.Count - 1 do
      begin
        Item := Boundary.EquationValues.Items[TableIndex] as TSfrEquationItem;
        Row := LocateRowFromStartAndEndTimes(Item.StartTime, Item.EndTime);
        if Row >= 1 then
        begin
          if dgSfrEquation.Cells[Ord(seStartTime), Row] <> FloatToStr(Item.StartTime) then
          begin
            dgSfrEquation.Cells[Ord(seStartTime), Row] := '';
          end;
          if dgSfrEquation.Cells[Ord(seEndTime), Row] <> FloatToStr(Item.EndTime) then
          begin
            dgSfrEquation.Cells[Ord(seEndTime), Row] := '';
          end;
          if dgSfrEquation.Cells[Ord(seDepthCoeff), Row] <> Item.DepthCoefficient then
          begin
            dgSfrEquation.Cells[Ord(seDepthCoeff), Row] := '';
          end;
          if dgSfrEquation.Cells[Ord(seDepthExp), Row] <> Item.DepthExponent then
          begin
            dgSfrEquation.Cells[Ord(seDepthExp), Row] := '';
          end;
          if dgSfrEquation.Cells[Ord(seWidthCoeff), Row] <> Item.WidthCoefficient then
          begin
            dgSfrEquation.Cells[Ord(seWidthCoeff), Row] := '';
          end;
          if dgSfrEquation.Cells[Ord(seWidthExp), Row] <> Item.WidthExponent then
          begin
            dgSfrEquation.Cells[Ord(seWidthExp), Row] := '';
          end;
        end
        else
        begin
          ClearTable(dgSfrEquation);
          break;
        end;
      end;
    end
    else
    begin
      ClearTable(dgSfrEquation);
    end;
  end
  else
  begin
    // The following While loop is to compensate for a bug that has
    // since been fixed.
    While seParametersCount.AsInteger < Boundary.EquationValues.Count do
    begin
      Boundary.EquationValues.Delete(Boundary.EquationValues.Count-1);
    end;

    Assert(seParametersCount.AsInteger >= Boundary.EquationValues.Count);

    for TableIndex := 0 to Boundary.EquationValues.Count - 1 do
    begin
      Item := Boundary.EquationValues.Items[TableIndex] as TSfrEquationItem;
      Row := LocateRowFromStartAndEndTimes(Item.StartTime, Item.EndTime);
      if Row >= 1 then
      begin

        dgSfrEquation.Cells[Ord(seStartTime), Row] := FloatToStr(Item.StartTime);
        dgSfrEquation.Cells[Ord(seEndTime), Row] := FloatToStr(Item.EndTime);
        dgSfrEquation.Cells[Ord(seDepthCoeff), Row] := Item.DepthCoefficient;
        dgSfrEquation.Cells[Ord(seDepthExp), Row] := Item.DepthExponent;
        dgSfrEquation.Cells[Ord(seWidthCoeff), Row] := Item.WidthCoefficient;
        dgSfrEquation.Cells[Ord(seWidthExp), Row] := Item.WidthExponent;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.DrawFlowTable(ABitMap: TBitmap32; YColumn: Integer; ZoomBox: TQRbwZoomBox2);
var
  MinY: Double;
  MaxY: Double;
  SectionPoints: TPointArray;
  PointIndex: Integer;
  MagX: Double;
  MagY: Double;
  Grid: TStringGrid;
  PointFound: Boolean;
  Index: Integer;
  X: Double;
  Y: Double;
  XArray: array of Double;
  YArray:  array  of Double;
  PointOK:  array  of Boolean;
  MinX: Double;
  MaxX: Double;
  XFormula: string;
  YFormula: string;
  Parser: TRbwParser;
begin
  if Assigned(GetParser) then
  begin
    Grid := frameFlowTable1.dgSfrTable;
    Parser := GetParser(self);
    PointFound := False;
    MinX := 0;
    MaxX := 0;
    MinY := 0;
    MaxY := 0;
    SetLength(XArray, Grid.RowCount - 1);
    SetLength(YArray, Grid.RowCount - 1);
    SetLength(PointOK, Grid.RowCount - 1);
    for Index := 1 to Grid.RowCount - 1 do
    begin
      PointOK[Index - 1] := False;
      XFormula := Grid.Cells[0, Index];
      YFormula := Grid.Cells[YColumn, Index];
      if (XFormula <>'') and (YFormula <> '') then
      begin
        X := 0;
        Y := 0;
        try
          Parser.Compile(XFormula);
          Parser.CurrentExpression.Evaluate;
          X := Parser.CurrentExpression.DoubleResult;

          Parser.Compile(YFormula);
          Parser.CurrentExpression.Evaluate;
          Y := Parser.CurrentExpression.DoubleResult;
        except on E: ERbwParserError do
          Continue;
        end;
        XArray[Index - 1] := X;
        YArray[Index - 1] := Y;
        PointOK[Index - 1] := True;
        if PointFound then
        begin
          if MinX > X then
          begin
            MinX := X;
          end;
          if MaxX < X then
          begin
            MaxX := X;
          end;
          if MinY > Y then
          begin
            MinY := Y;
          end;
          if MaxY < Y then
          begin
            MaxY := Y;
          end;
        end
        else
        begin
          MinX := X;
          MaxX := X;
          MinY := Y;
          MaxY := Y;
        end;
        PointFound := True;
      end;
    end;
    if PointFound and (MinX <> MaxX) and (MinY <> MaxY) then
    begin
      SetLength(SectionPoints, Grid.RowCount - 1);
      PointIndex := 0;
      ZoomBox.OriginX := MinX - (MaxX - MinX) / 10;
      ZoomBox.OriginY := MinY - (MaxY - MinY) / 10;
      MagX := ZoomBox.Width / ((MaxX - MinX) * 1.2);
      MagY := ZoomBox.Height / ((MaxY - MinY) * 1.2);
      ZoomBox.Magnification := MagX;
      ZoomBox.Exaggeration := MagY / MagX;
      for Index := 0 to Length(SectionPoints)-1 do
      begin
        if PointOK[Index] then
        begin
          SectionPoints[PointIndex].X := ZoomBox.XCoord(XArray[Index]);
          SectionPoints[PointIndex].Y := ZoomBox.YCoord(YArray[Index]);
          Inc(PointIndex);
        end
        else
        begin
          if PointIndex > 0 then
          begin
            SetLength(SectionPoints, PointIndex);
            DrawBigPolyline32(ABitMap, clBlack32, 1, SectionPoints, True);
            SetLength(SectionPoints, Grid.RowCount - 1);
            PointIndex := 0;
          end;
        end;
      end;
      if PointIndex > 0 then
      begin
        SetLength(SectionPoints, PointIndex);
        DrawBigPolyline32(ABitMap, clBlack32, 1, SectionPoints, True);
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.dgSfrTableSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ICalc: TByteSet;
begin
  ICalc := IcalcRowSet(dgTableTime.SelectedRow);
  CanSelect := (4 in ICalc);
end;

procedure TframeScreenObjectSFR.dg8PointSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ICalc: TByteSet;
begin
  ICalc := IcalcRowSet(dgSfrRough.SelectedRow);
  CanSelect := (2 in ICalc);
end;

procedure TframeScreenObjectSFR.dgSfrEquationColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowEquationEditControls;
end;

procedure TframeScreenObjectSFR.dgSfrEquationHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowEquationEditControls;
end;

procedure TframeScreenObjectSFR.dgSfrEquationMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(rdeEquationFormula, dgSfrEquation);
end;

procedure TframeScreenObjectSFR.dgSfrEquationSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var  
  ItemCount: integer;
begin
  ItemCount := seParametersCount.AsInteger;
  CanSelect := ItemCount > 0;
  if not CanSelect then
  begin
    Exit;
  end;
  CanSelect := not (ACol in [Ord(seStartTime), Ord(seEndTime)]);
  if CanSelect then
  begin
    CanSelect := 3 in IcalcRowSet(ARow);
  end;
end;

procedure TframeScreenObjectSFR.dgSfrEquationSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgSfrEquation, seParametersCount);
end;

procedure TframeScreenObjectSFR.dgSfrRoughColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowChannelEditControls;
end;

procedure TframeScreenObjectSFR.dgSfrRoughHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowChannelEditControls;
end;

procedure TframeScreenObjectSFR.dgSfrRoughMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(rdeChannelFormula, dgSfrRough);
end;

procedure TframeScreenObjectSFR.dgSfrRoughSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  TimePeriod: integer;
  Column: TsfrRoughColumn;
  ICalc: TByteSet;
  ItemCount: integer;
begin
  if FDeletingRow then
  begin
    CanSelect := True;
  end
  else
  begin
    ItemCount := seParametersCount.AsInteger;
    CanSelect := ItemCount > 0;
    if not CanSelect then
    begin
      Exit;
    end;
    ICalc := IcalcRowSet(ARow);
    if (ACol >= 0) and (ACol <= Ord(High(TsfrRoughColumn))) then
    begin
      Column := TsfrRoughColumn(ACol);
      case Column of
        srStartTime:
          begin
            CanSelect := False;
          end;
        srEndTime:
          begin
            CanSelect := False;
          end;
        srChannelRough:
          begin
            CanSelect := (ICalc * [1,2] <> []);
          end;
        srBankRough:
          begin
            CanSelect := (2 in ICalc);
          end;
        else Assert(False);
      end;
    end
    else
    begin
      CanSelect := False;
    end;
  end;

  if (not dgSfrRough.Drawing) and (ARow >= 1) then
  begin
    TimePeriod := ARow -1;
    // The cross section is only read for the first stress period
    // if unsaturated flow is simulated.
    if ISFROPT <= 1 then
    begin
      // saturated flow
      frameCrossSection1.DisplayData(TimePeriod);
    end
    else
    begin
      // unsaturated flow
      frameCrossSection1.DisplayData(0);
    end;
    frameCrossSection1.dg8Point.Invalidate;

    zbChannel.InvalidateImage32;
  end;
end;

procedure TframeScreenObjectSFR.dgSfrRoughSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgSfrRough, seParametersCount);
end;

procedure TframeScreenObjectSFR.dgCrossSectionSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  frameCrossSection1.dg8PointSetEditText(Sender, ACol, ARow, Value);
  Edited;
  zbChannel.InvalidateImage32;
end;

procedure TframeScreenObjectSFR.dgDownColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowDownstreamEditControls;
end;

procedure TframeScreenObjectSFR.dgDownHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowDownstreamEditControls;
end;

procedure TframeScreenObjectSFR.dgDownMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(rdeDownstreamFormula, dgDown);
end;

procedure TframeScreenObjectSFR.dgDownSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgDown, seParametersCount);
end;

procedure TframeScreenObjectSFR.dgTableTimeSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  TimePeriod: integer;
  ItemCount: integer;
begin
  if FDeletingRow then
  begin
    CanSelect := True;
  end
  else
  begin
    ItemCount := seParametersCount.AsInteger;
    CanSelect := ItemCount > 0;
    if not CanSelect then
    begin
      Exit;
    end;
    CanSelect := 4 in IcalcRowSet(ARow);
    if (not dgTableTime.Drawing) and (ARow >= 1) then
    begin
      TimePeriod := ARow -1;
      frameFlowTable1.DisplayData(TimePeriod);

      zbFlowDepthTable.InvalidateImage32;
      zbFlowWidthTable.InvalidateImage32;

      FrameFlowTable1.dgSfrTable.Invalidate;

      FrameFlowTable1.seTableCount.Enabled := CanSelect;
      FrameFlowTable1.btnInsertFlowTableRow.Enabled := CanSelect;
      FrameFlowTable1.seTableCount.Enabled := CanSelect;
      FrameFlowTable1.EnableDelete;
    end;
  end;
end;

procedure TframeScreenObjectSFR.dgTableTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgTableTime, seParametersCount);
end;

procedure TframeScreenObjectSFR.dgFlowTableSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  zbFlowDepthTable.InvalidateImage32;
  zbFlowWidthTable.InvalidateImage32;
  if not (Sender is TRbwDataGrid4) then Exit;
  frameFlowTable1.dgSfrTableSetEditText(Sender, ACol, ARow, Value);
end;

procedure TframeScreenObjectSFR.dgFlowTimesColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowFlowEditControls;
end;

procedure TframeScreenObjectSFR.dgFlowTimesHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowFlowEditControls;
end;

procedure TframeScreenObjectSFR.dgFlowTimesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(rdeFlowFormula, dgFlowTimes);
end;

procedure TframeScreenObjectSFR.LayoutMultiRowFlowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(dgFlowTimes, rdeFlowFormula, lblFlowFormula,
    Max(2,dgFlowTimes.LeftCol));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowParamIcalcControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgParameters, comboParameterChoices, lblParameterChoices,
    Ord(spicParameter));
  LayoutControls(rdgParameters, comboIcalcChoice, lblIcalcChoice,
    Ord(spicIcalc));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowNetworkControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgNetwork, rdeNetwork, lblSegment,
    Ord(sncOutflowSegment));
  LayoutControls(rdgNetwork, comboMultiIprior, nil,
    Ord(sncIprior));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowChannelEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(dgSfrRough, rdeChannelFormula, lblChannelFormula,
    Max(2,dgSfrRough.LeftCol));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowEquationEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(dgSfrEquation, rdeEquationFormula, lblEquationFormula,
    Max(2,dgSfrEquation.LeftCol));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowUpstreamEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(dgUp, rdeUpstreamFormula, lblUpstreamFormula,
    Max(2,dgUp.LeftCol));
end;

procedure TframeScreenObjectSFR.LayoutMultiRowDownstreamEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(dgDown, rdeDownstreamFormula, lblDownstreamFormula,
    Max(2,dgDown.LeftCol));
end;

procedure TframeScreenObjectSFR.dgFlowTimesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ItemCount: integer;
begin
  ItemCount := seParametersCount.AsInteger;
  CanSelect := (ItemCount > 0) and not (ACol in [Ord(sfcStartTime), Ord(sfcEndTime)]);
end;

procedure TframeScreenObjectSFR.dgFlowTimesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgFlowTimes, seParametersCount);
end;

procedure TframeScreenObjectSFR.dgUpColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiRowUpstreamEditControls;
end;

procedure TframeScreenObjectSFR.dgUpHorizontalScroll(Sender: TObject);
begin
  LayoutMultiRowUpstreamEditControls;
end;

procedure TframeScreenObjectSFR.dgUpMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(rdeUpstreamFormula, dgUp);
end;

procedure TframeScreenObjectSFR.dgUpSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Column: TSfrColumn;
  ICalc: TByteSet;
  ItemCount: integer;
begin
  ItemCount := seParametersCount.AsInteger;
  CanSelect := ItemCount > 0;
  if not CanSelect then
  begin
    Exit;
  end;

  if ACol >= 0 then
  begin

    Column := TSfrColumn(ACol);
    ICalc := IcalcRowSet(ARow);
    case Column of
      scStartTime:
        begin
          CanSelect := False;
        end;
      scEndTime:
        begin
          CanSelect := False;
        end;
      scK:
        begin
          CanSelect := ISFROPT in [0,4,5];
        end;
      scBedThickness, scBedElevation:
        begin
          CanSelect := ISFROPT in [0,4,5];
          if CanSelect then
          begin
            if ISFROPT in [4,5] then
            begin
              if [0,3,4] * ICalc <> [] then
              begin
                CanSelect := True;
              end
              else
              begin
                CanSelect := ARow = 1
              end;
            end;
          end;
        end;
      scStreamWidth:
        begin
          CanSelect := [0,1] * ICalc <> [];
          if CanSelect then
          begin
            if (ISFROPT >= 2) and not (0 in ICalc) then
            begin
              CanSelect := ARow = 1
            end;
          end;
        end;
      scStreamDepth:
        begin
          CanSelect := 0 in ICalc;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TframeScreenObjectSFR.dgUpSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  Edited;
  UpdateSpinEditValue(dgUp, seParametersCount);
end;

procedure TframeScreenObjectSFR.SetSfrParamIcalc(Boundary: TSfrBoundary);
var
  ParamIcalcValues: TSfrParamIcalcCollection;
  RowIndex: Integer;
  StartTime, EndTime: double;
  Item: TSfrParamIcalcItem;
  ICalc: integer;
  Valid: Boolean;
  IntValue: integer;
  NewItem: Boolean;
  SfrItem: TSfrItem;
  LastItem: TSfrItem;
begin
  ParamIcalcValues := TSfrParamIcalcCollection.Create(nil, nil, nil);
  try
    ParamIcalcValues.Assign(Boundary.ParamIcalc);
    if FTimesChanged then
    begin
      While ParamIcalcValues.Count > rdgParameters.RowCount - 1 do
      begin
        ParamIcalcValues.Delete(ParamIcalcValues.Count-1);
      end;
    end;
    for RowIndex := 1 to rdgParameters.RowCount - 1 do
    begin
      if TryStrToFloat(rdgParameters.Cells[Ord(spicStartTime), RowIndex], StartTime)
        and TryStrToFloat(rdgParameters.Cells[Ord(spicEndTime), RowIndex], EndTime)
        then
      begin
        Valid := True;
        if (rdgParameters.Cells[Ord(spicParameter), RowIndex] <> '')
          and (rdgParameters.ItemIndex[Ord(spicParameter), RowIndex] <> 0)
          then
        begin
          Valid := rdgParameters.Cells[Ord(spicParamInstance), RowIndex] <> ''
        end;
        if Valid then
        begin
          NewItem := False;
          if RowIndex-1 < ParamIcalcValues.Count then
          begin
            Item := ParamIcalcValues.Items[RowIndex-1];
          end
          else
          begin
            Item := ParamIcalcValues.Add as TSfrParamIcalcItem;
            NewItem := True;
          end;
          Item.StartTime := StartTime;
          Item.EndTime := EndTime;
          if rdgParameters.ItemIndex[Ord(spicParameter), RowIndex] = 0
            then
          begin
            Item.Param := '';
            Item.ParamInstance := ''
          end
          else
          begin
            Item.Param := rdgParameters.Cells[Ord(spicParameter), RowIndex];
            if Item.Param  <> '' then
            begin
              Item.ParamInstance := rdgParameters.Cells[Ord(spicParamInstance), RowIndex];
            end
            else
            begin
              Item.ParamInstance := '';
            end;
          end;

          ICalc := rdgParameters.ItemIndex[Ord(spicIcalc), RowIndex];
          if (ICalc < 0) and NewItem then
          begin
            if Item.Index > 0 then
            begin
              ICalc := ParamIcalcValues.Items[Item.Index-1].ICalc;
            end
            else
            begin
              ICalc := 0;
            end;
          end;
          if (ICalc >= 0) then
          begin
            Item.ICalc := ICalc;
          end;

          if TryStrToInt(rdgNetwork.Cells[Ord(sncOutflowSegment), RowIndex], IntValue) then
          begin
            Item.OutflowSegment := IntValue;
          end;
          if TryStrToInt(rdgNetwork.Cells[Ord(sncDiversionSegment), RowIndex], IntValue) then
          begin
            Item.DiversionSegment := IntValue;
          end;
          IntValue := rdgNetwork.Columns[Ord(sncIprior)].PickList.IndexOf(
            rdgNetwork.Cells[Ord(sncIprior), RowIndex]);
          if IntValue >= 0 then
          begin
            Item.IPRIOR := -IntValue;
          end;
        end;
      end;
    end;
    if ParamIcalcValues.Count > 0 then
    begin
      Boundary.ParamIcalc := ParamIcalcValues;
      SfrItem := Boundary.Values.Items[0] as TSfrItem;
      SfrItem.StartTime := ParamIcalcValues[0].StartTime;
      LastItem := Boundary.Values.Items[Boundary.Values.Count-1] as TSfrItem;
      SfrItem.EndTime := LastItem.EndTime;
    end
    else if FTimesChanged then
    begin
      Boundary.ParamIcalc.Clear;
    end;
  finally
    ParamIcalcValues.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrChannel(Boundary: TSfrBoundary);
var
  ChannelValues: TSfrChannelCollection;
  StartTime, EndTime: double;
  Item: TSfrChannelItem;
  RowIndex: integer;
  Formula: string;
begin
  ChannelValues := TSfrChannelCollection.Create(nil, nil, nil);
  try
    ChannelValues.Assign(Boundary.ChannelValues);
    if FTimesChanged then
    begin
      While ChannelValues.Count > dgSfrRough.RowCount - 1 do
      begin
        ChannelValues.Delete(ChannelValues.Count-1);
      end;
    end;
    for RowIndex := 1 to dgSfrRough.RowCount - 1 do
    begin
      if TryStrToFloat(dgSfrRough.Cells[Ord(srStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgSfrRough.Cells[Ord(srEndTime), RowIndex], EndTime)
        then
      begin
        if RowIndex - 1 < ChannelValues.Count then
        begin
          Item := ChannelValues.Items[RowIndex - 1];
        end
        else
        begin
          Item := ChannelValues.Add as TSfrChannelItem;
        end;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;
        Formula := dgSfrRough.Cells[Ord(srChannelRough), RowIndex];
        if Formula <> '' then
        begin
          Item.ChannelRoughness := Formula;
        end;


        Formula := dgSfrRough.Cells[Ord(srBankRough), RowIndex];
        if Formula <> '' then
        begin
          Item.BankRoughness := Formula;
        end;
      end;
    end;
    if ChannelValues.Count > 0 then
    begin
      Boundary.ChannelValues := ChannelValues;
    end
    else if FTimesChanged then
    begin
      Boundary.ChannelValues.Clear;
    end;
  finally
    ChannelValues.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrEquation(Boundary: TSfrBoundary);
var
  EquationValues: TSfrEquationCollection;
  RowIndex: Integer;
  StartTime, EndTime: double;
  Item: TSfrEquationItem;
  Formula: string;
begin
  EquationValues := TSfrEquationCollection.Create(nil, nil, nil);
  try
    EquationValues.Assign(Boundary.EquationValues);
    if FTimesChanged then
    begin
      While EquationValues.Count > dgSfrEquation.RowCount - 1 do
      begin
        EquationValues.Delete(EquationValues.Count-1);
      end;
    end;
    for RowIndex := 0 to dgSfrEquation.RowCount - 1 do
    begin
      if TryStrToFloat(dgSfrEquation.Cells[Ord(seStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgSfrEquation.Cells[Ord(seEndTime), RowIndex], EndTime)
        then
      begin
        if RowIndex -1 < EquationValues.Count then
        begin
          Item := EquationValues.Items[RowIndex -1] as TSfrEquationItem;
        end
        else
        begin
          Item := EquationValues.Add as TSfrEquationItem;
        end;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        Formula := dgSfrEquation.Cells[Ord(seDepthCoeff), RowIndex];
        if Formula <> '' then
        begin
          Item.DepthCoefficient := Formula;
        end;

        Formula := dgSfrEquation.Cells[Ord(seDepthExp), RowIndex];
        if Formula <> '' then
        begin
          Item.DepthExponent := Formula;
        end;

        Formula := dgSfrEquation.Cells[Ord(seWidthCoeff), RowIndex];
        if Formula <> '' then
        begin
          Item.WidthCoefficient := Formula;
        end;

        Formula := dgSfrEquation.Cells[Ord(seWidthExp), RowIndex];
        if Formula <> '' then
        begin
          Item.WidthExponent := Formula;
        end;
      end;
    end;
    if EquationValues.Count > 0 then
    begin
      Boundary.EquationValues := EquationValues;
    end
    else if FTimesChanged then
    begin
      Boundary.EquationValues.Clear;
    end;
  finally
    EquationValues.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrFlows(Boundary: TSfrBoundary);
var
  SegmentFlows: TSfrSegmentFlowCollection;
  RowIndex: Integer;
  StartTime, EndTime: double;
  Item: TSfrSegmentFlowItem;
  Formula: string;
begin
  SegmentFlows := TSfrSegmentFlowCollection.Create(nil, nil, nil);
  try
    SegmentFlows.Assign(Boundary.SegmentFlows);
    if FTimesChanged then
    begin
      While SegmentFlows.Count > dgFlowTimes.RowCount - 1 do
      begin
        SegmentFlows.Delete(SegmentFlows.Count-1);
      end;
    end;
    for RowIndex := 1 to dgFlowTimes.RowCount - 1 do
    begin
      if TryStrToFloat(dgFlowTimes.Cells[Ord(sfcStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgFlowTimes.Cells[Ord(sfcEndTime), RowIndex], EndTime)
        then
      begin
        if RowIndex-1 < SegmentFlows.Count then
        begin
          Item := SegmentFlows.Items[RowIndex-1] as TSfrSegmentFlowItem;
        end
        else
        begin
          Item := SegmentFlows.Add as TSfrSegmentFlowItem;
        end;

        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        Formula := dgFlowTimes.Cells[Ord(sfcFlow), RowIndex];
        if Formula <> '' then
        begin
          Item.Flow := Formula;
        end;

        Formula := dgFlowTimes.Cells[Ord(sfcPrecip), RowIndex];
        if Formula <> '' then
        begin
          Item.Precipitation := Formula;
        end;

        Formula := dgFlowTimes.Cells[Ord(sfcEvap), RowIndex];
        if Formula <> '' then
        begin
          Item.Evapotranspiration := Formula;
        end;

        Formula := dgFlowTimes.Cells[Ord(sfcRunoff), RowIndex];
        if Formula <> '' then
        begin
          Item.Runnoff := Formula;
        end;
      end;
    end;
    if SegmentFlows.Count > 0 then
    begin
      Boundary.SegmentFlows := SegmentFlows;
    end
    else if FTimesChanged then
    begin
      Boundary.SegmentFlows.Clear;
    end;
  finally
    SegmentFlows.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrFlowTable(Boundary: TSfrBoundary);
var
  TableCollection: TSfrTableCollection;
  RowIndex: Integer;
  StartTime, EndTime: double;
  Item: TSfrTablelItem;
begin
  TableCollection := TSfrTableCollection.Create(nil, nil, nil);
  try
    TableCollection.Assign(Boundary.TableCollection);
    if FTimesChanged then
    begin
      While TableCollection.Count > dgTableTime.RowCount - 1 do
      begin
        TableCollection.Delete(TableCollection.Count-1);
      end;
    end;
    for RowIndex := 1 to dgTableTime.RowCount - 1 do
    begin
      if TryStrToFloat(dgTableTime.Cells[Ord(sttStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgTableTime.Cells[Ord(sttEndTime), RowIndex], EndTime)
        then
      begin
        if RowIndex - 1 < TableCollection.Count then
        begin
          Item := TableCollection.Items[RowIndex - 1] as TSfrTablelItem;
        end
        else
        begin
          Item := TableCollection.Add as TSfrTablelItem;
        end;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;
      end;
    end;
    if TableCollection.Count > 0 then
    begin
      Boundary.TableCollection := TableCollection;
    end;
  finally
    TableCollection.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrSegments(Boundary: TSfrBoundary);

var
  UpSegments, DownSegments: TSfrSegmentCollection;
  procedure CreateSegments(Segments, ExistingSegments: TSfrSegmentCollection;
   Grid: TStringGrid);
  var
    RowIndex: integer;
    StartTime, EndTime: double;
    Item: TSfrSegmentItem;
    Formula: string;
  begin
    Segments.Assign(ExistingSegments);
    if FTimesChanged then
    begin
      While Segments.Count > Grid.RowCount - 1 do
      begin
        Segments.Delete(Segments.Count-1);
      end;
    end;

    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      if TryStrToFloat(Grid.Cells[Ord(scStartTime), RowIndex], StartTime)
        and TryStrToFloat(Grid.Cells[Ord(scEndTime), RowIndex], EndTime)
        then
      begin
        if RowIndex - 1 < Segments.Count then
        begin
          Item := Segments.Items[RowIndex - 1] as TSfrSegmentItem;
        end
        else
        begin
          Item := Segments.Add as TSfrSegmentItem;
        end;

        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        Formula := Grid.Cells[Ord(scK), RowIndex];
        if Formula <> '' then
        begin
          Item.HydraulicConductivity := Formula;
        end;

        Formula := Grid.Cells[Ord(scBedThickness), RowIndex];
        if Formula <> '' then
        begin
          Item.StreamBedThickness := Formula;
        end;

        Formula := Grid.Cells[Ord(scBedElevation), RowIndex];
        if Formula <> '' then
        begin
          Item.StreambedElevation := Formula;
        end;

        Formula := Grid.Cells[Ord(scStreamWidth), RowIndex];
        if Formula <> '' then
        begin
          Item.StreamWidth := Formula;
        end;

        Formula := Grid.Cells[Ord(scStreamDepth), RowIndex];
        if Formula <> '' then
        begin
          Item.StreamDepth := Formula;
        end;
      end;
    end;
  end;
begin
  UpSegments := TSfrSegmentCollection.Create(nil, nil, nil);
  DownSegments := TSfrSegmentCollection.Create(nil, nil, nil);
  try
    begin
      CreateSegments(UpSegments, Boundary.UpstreamSegmentValues, dgUp);
      CreateSegments(DownSegments, Boundary.DownstreamSegmentValues, dgDown);

      if (UpSegments.Count > 0) then
      begin
        Boundary.UpstreamSegmentValues := UpSegments;
      end
      else if FTimesChanged then
      begin
        Boundary.UpstreamSegmentValues.Clear;
      end;

      if (DownSegments.Count > 0) then
      begin
        Boundary.DownstreamSegmentValues := DownSegments;
      end
      else if FTimesChanged then
      begin
        Boundary.DownstreamSegmentValues.Clear;
      end;
    end;
  finally
    UpSegments.Free;
    DownSegments.Free;
  end;
end;

procedure TframeScreenObjectSFR.SetUnsaturatedValues(Boundary: TSfrBoundary);
  procedure SetAValue(Control: TJvComboEdit; var AValue: string);
  begin
    AValue := Control.Text;
    if not Control.Enabled and (AValue = '') then
    begin
      AValue := '0';
    end;
  end;
var
  THTS1, THTI1, EPS1, UHC1: string;
  THTS2, THTI2, EPS2, UHC2: string;
  UpUnsat: TSfrUnsatSegmentCollection;
  Item: TSfrUnsatSegmentItem;
begin
  SetAValue(jceSaturatedVolumetricWaterUpstream, THTS1);
  SetAValue(jceInitialVolumetricWaterUpstream, THTI1);
  SetAValue(jceBrooksCoreyExponentUpstream, EPS1);
  SetAValue(jceMaxUnsaturatedKzUpstream, UHC1);

  SetAValue(jceSaturatedVolumetricWaterDownstream, THTS2);
  SetAValue(jceInitialVolumetricWaterDownstream, THTI2);
  SetAValue(jceBrooksCoreyExponentDownstream, EPS2);
  SetAValue(jceMaxUnsaturatedKzDownstream, UHC2);
  if (THTS1 <> '')
    and (THTI1 <> '')
    and (EPS1 <> '')
    and (UHC1 <> '')
    and (THTS2 <> '')
    and (THTI2 <> '')
    and (EPS2 <> '')
    and (UHC2 <> '')
    then
  begin
    UpUnsat := TSfrUnsatSegmentCollection.Create(nil, nil, nil);
    try
      UpUnsat.Assign(Boundary.UpstreamUnsatSegmentValues);
      if UpUnsat.Count = 0 then
      begin
        Item := UpUnsat.Add as TSfrUnsatSegmentItem;
      end
      else
      begin
        Item := UpUnsat[0] as TSfrUnsatSegmentItem;
      end;

      Item.StartTime := 0;
      Item.EndTime := 0;
      Item.SaturatedWaterContent := THTS1;
      Item.InitialWaterContent := THTI1;
      Item.BrooksCoreyExponent := EPS1;
      Item.VerticalSaturatedK := UHC1;

      Boundary.UpstreamUnsatSegmentValues := UpUnsat;
    finally
      UpUnsat.Free;
    end;

    UpUnsat := TSfrUnsatSegmentCollection.Create(nil, nil, nil);
    try
      UpUnsat.Assign(Boundary.DownstreamUnsatSegmentValues);
      if UpUnsat.Count = 0 then
      begin
        Item := UpUnsat.Add as TSfrUnsatSegmentItem;
      end
      else
      begin
        Item := UpUnsat[0] as TSfrUnsatSegmentItem;
      end;
//      Item := UpUnsat.Add as TSfrUnsatSegmentItem;
      Item.StartTime := 0;
      Item.EndTime := 0;
      Item.SaturatedWaterContent := THTS2;
      Item.InitialWaterContent := THTI2;
      Item.BrooksCoreyExponent := EPS2;
      Item.VerticalSaturatedK := UHC2;

      Boundary.DownstreamUnsatSegmentValues := UpUnsat;
    finally
      UpUnsat.Free;
    end;
  end;
end;

procedure TframeScreenObjectSFR.SetSfrValues(Boundary: TSfrBoundary);
var
  Item: TSfrItem;
begin
  if
    (jvcReachLength.Text <> '') or
    (jceStreamTop.Text <> '') or
    (jceSlope.Text <> '') or
    (jceStreambedThickness.Text <> '') or
    (jceStreambedK.Text <> '') or
    (jceSaturatedVolumetricWater.Text <> '') or
    (jceInitialVolumetricWater.Text <> '') or
    (jceBrooksCoreyExponent.Text <> '') or
    (jceMaxUnsaturatedKz.Text <> '') then
  begin
    if Boundary.Values.Count >= 1 then
    begin
      Assert(Boundary.Values.Count = 1);
      Item := Boundary.Values.Items[0] as TSfrItem;
    end
    else
    begin
      Item := Boundary.Values.Add as TSfrItem;
    end;

    if jvcReachLength.Text <> '' then
    begin
      Item.ReachLength := jvcReachLength.Text;
    end;
    if jceStreamTop.Text <> '' then
    begin
      Item.StreambedElevation := jceStreamTop.Text;
    end;
    if jceSlope.Text <> '' then
    begin
      Item.StreamSlope := jceSlope.Text;
    end;
    if jceStreambedThickness.Text <> '' then
    begin
      Item.StreamBedThickness := jceStreambedThickness.Text;
    end;
    if jceStreambedK.Text <> '' then
    begin
      Item.HydraulicConductivity := jceStreambedK.Text;
    end;
    if jceSaturatedVolumetricWater.Text <> '' then
    begin
      Item.SaturatedWaterContent := jceSaturatedVolumetricWater.Text;
    end;
    if jceInitialVolumetricWater.Text <> '' then
    begin
      Item.InitialWaterContent := jceInitialVolumetricWater.Text;
    end;
    if jceBrooksCoreyExponent.Text <> '' then
    begin
      Item.BrooksCoreyExponent := jceBrooksCoreyExponent.Text;
    end;
    if jceMaxUnsaturatedKz.Text <> '' then
    begin
      Item.VerticalK := jceMaxUnsaturatedKz.Text;
    end;
  end;
end;

procedure TframeScreenObjectSFR.SetData(List: TScreenObjectEditCollection;
  SetAll: boolean; ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TSfrBoundary;
  IntValue: integer;
  BoundaryUsed: boolean;
begin
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowSfrBoundary;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;

    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.Clear;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        Item.ScreenObject.CreateSfrBoundary;
        Boundary := Item.ScreenObject.ModflowSfrBoundary;
      end;
      if TryStrToInt(rdeSegmentNumber.Text, IntValue) then
      begin
        Boundary.SegmentNumber := IntValue;
      end;

      SetSfrValues(Boundary);
      SetSfrParamIcalc(Boundary);
      SetSfrFlows(Boundary);
      SetSfrSegments(Boundary);
      SetSfrChannel(Boundary);
      SetSfrEquation(Boundary);
      SetSfrFlowTable(Boundary);
      SetUnsaturatedValues(Boundary);
      SetExternalFlowFile(Boundary);

      if rgGages.ItemIndex >= 0 then
      begin
        Boundary.GageLocation := TGageLocation(rgGages.ItemIndex);
      end;
      if cbGagStandard.State <> cbGrayed then
      begin
        Boundary.Gage0 := cbGagStandard.Checked;
      end;
      if cbGag1.State <> cbGrayed then
      begin
        Boundary.Gage1 := cbGag1.Checked;
      end;
      if cbGag2.State <> cbGrayed then
      begin
        Boundary.Gage2 := cbGag2.Checked;
      end;
      if cbGag3.State <> cbGrayed then
      begin
        Boundary.Gage3 := cbGag3.Checked;
      end;
      if cbGag5.State <> cbGrayed then
      begin
        Boundary.Gage5 := cbGag5.Checked;
      end;
      if cbGag6.State <> cbGrayed then
      begin
        Boundary.Gage6 := cbGag6.Checked;
      end;
      if cbGag7.State <> cbGrayed then
      begin
        Boundary.Gage7 := cbGag7.Checked;
      end;
      Boundary.FixCollections;

      if List.Count = 1 then
      begin
        frameSfrPestObs.SetData(Boundary.Observations);
      end;
    end;
  end;
  frameCrossSection1.SetData(List, SetAll, ClearAll);
  frameFlowTable1.SetData(List, SetAll, ClearAll);
end;

procedure TframeScreenObjectSFR.SetISFROPT(const Value: integer);
begin
  FISFROPT := Value;
  EnableTabs;
end;

procedure TframeScreenObjectSFR.SetOnButtonClick(const Value: TGridButtonEvent);
begin
  FOnButtonClick := Value;
  FrameFlowTable1.dgSfrTable.OnButtonClick := Value;
  FrameCrossSection1.dg8Point.OnButtonClick := Value;
end;

procedure TframeScreenObjectSFR.seParametersCountChange(Sender: TObject);
var
  ItemCount: integer;
  Index: integer;
  OldRowcount: integer;
  RowIndex: Integer;
begin
  FDeletingTime := True;
  try
    FTimesChanged := True;
    ItemCount := seParametersCount.AsInteger;
    Assert(ItemCount >= 0);
    if ItemCount = 0 then
    begin
      rdgParameters.RowCount := 2;
    end
    else
    begin
      OldRowcount := rdgParameters.RowCount;
      rdgParameters.RowCount := ItemCount + 1;
      for RowIndex := OldRowcount to rdgParameters.RowCount - 1 do
      begin
        if (RowIndex > 1)
          and (rdgParameters.Cells[Ord(spicIcalc), RowIndex] = '')
          then
        begin
          if (rdgParameters.Cells[Ord(spicIcalc), RowIndex-1] <> '')
          then
          begin
            rdgParameters.Cells[Ord(spicIcalc), RowIndex] :=
              rdgParameters.Cells[Ord(spicIcalc), RowIndex-1];
          end
          else
          begin
            rdgParameters.ItemIndex[Ord(spicIcalc), RowIndex] := 0;
          end;
        end;
      end;
    end;
    if not (csLoading in ComponentState) then
    begin
      for Index := 1 to ItemCount do
      begin
        if rdgParameters.Cells[Ord(spicParameter), Index] = '' then
        begin
          rdgParameters.Cells[Ord(spicParameter), Index] :=
            rdgParameters.Columns[Ord(spicParameter)].PickList[0];
        end;
      end;
    end;

    if ItemCount = 0 then
    begin
      dgFlowTimes.RowCount := 2;
      dgUp.RowCount := 2;
      dgDown.RowCount := 2;
      dgSfrEquation.RowCount := 2;
      rdgNetwork.RowCount := 2;
    end
    else
    begin
      dgFlowTimes.RowCount := ItemCount + 1;
      dgUp.RowCount := ItemCount + 1;
      dgDown.RowCount := ItemCount + 1;
      dgSfrEquation.RowCount := ItemCount + 1;
      rdgNetwork.RowCount := ItemCount + 1;
    end;

    while FTimeCount < ItemCount do
    begin
      if not FInitializing and not FGettingData then
      begin
        frameCrossSection1.AddItem;
        frameFlowTable1.AddItem;
      end;
      Inc(FTimeCount)
    end;
    while FTimeCount > ItemCount do
    begin
      if not FInitializing and not FGettingData then
      begin
        frameCrossSection1.DeleteItem(FTimeCount);
        frameFlowTable1.DeleteItem(FTimeCount);
      end;
      Dec(FTimeCount);
    end;
    if not FInitializing and not FGettingData then
    begin
      if dgSfrRough.SelectedRow >= 1 then
      begin
        frameCrossSection1.DisplayData(dgSfrRough.SelectedRow-1);
      end;
      if dgTableTime.SelectedRow >= 1 then
      begin
        frameFlowTable1.DisplayData(dgTableTime.SelectedRow-1);
      end;
    end;

    if ItemCount = 0 then
    begin
      dgSfrRough.RowCount := 2;
      dgTableTime.RowCount := 2;
    end
    else
    begin
      dgSfrRough.RowCount := ItemCount + 1;
      dgTableTime.RowCount := ItemCount + 1;
    end;

    btnDeleteParameters.Enabled := (ItemCount > 0);
    UpdateICalc;
    EnableTabs;
    rdgParameters.Invalidate;
  finally
    FDeletingTime := False;
  end;
end;

procedure TframeScreenObjectSFR.seParametersCountEnter(Sender: TObject);
begin
  inherited;
  FTimeCount := seParametersCount.AsInteger
end;

procedure TframeScreenObjectSFR.DrawCrossSection(ABitMap: TBitmap32);
var
  XArray: array[0..7] of double;
  YArray: array[0..7] of double;
  PointOK: array[0..7] of boolean;
  X, Y: double;
  Index: integer;
  Frame: TframeCrossSection;
  Grid: TStringGrid;
  PointFound: boolean;
  MinX, MaxX, MinY, MaxY: double;
  MagX, MagY: double;
  SectionPoints: TPointArray;
  PointIndex: integer;
  FormulaX, FormulaZ: string;
  Parser: TRbwParser;
begin
  if Assigned(GetParser) then
  begin
    Frame := frameCrossSection1;
    Parser := GetParser(self);
    Grid := Frame.dg8Point;
    PointFound := False;
    MinX := 0;
    MaxX := 0;
    MinY := 0;
    MaxY := 0;
    for Index := 1 to 8 do
    begin
      FormulaX := Grid.Cells[Ord(s8pX),Index];
      FormulaZ := Grid.Cells[Ord(s8pZ),Index];
      PointOK[Index-1] := False;
      if (FormulaX <> '') and (FormulaZ <> '') then
      begin
        X := 0;
        Y := 0;
        try
          Parser.Compile(FormulaX);
          Parser.CurrentExpression.Evaluate;
          X := Parser.CurrentExpression.DoubleResult;

          Parser.Compile(FormulaZ);
          Parser.CurrentExpression.Evaluate;
          Y := Parser.CurrentExpression.DoubleResult;
        except on E: ERbwParserError do
          begin
            Continue
          end;
        end;
        XArray[Index-1] := X;
        YArray[Index-1] := Y;
        PointOK[Index-1] := True;
        if PointFound then
        begin
          if MinX > X then
          begin
            MinX := X;
          end;
          if MaxX < X then
          begin
            MaxX := X;
          end;
          if MinY > Y then
          begin
            MinY := Y;
          end;
          if MaxY < Y then
          begin
            MaxY := Y;
          end;
        end
        else
        begin
          MinX := X;
          MaxX := X;
          MinY := Y;
          MaxY := Y;
        end;
        PointFound := True;
      end;
    end;
    if PointFound and (MinX <> MaxX) and (MinY <> MaxY) then
    begin
      SetLength(SectionPoints, 8);
      PointIndex := 0;

      zbChannel.OriginX := MinX - (MaxX-MinX)/10;
      zbChannel.OriginY := MinY - (MaxY-MinY)/10;
      MagX := zbChannel.Width/((MaxX-MinX)*1.2);
      MagY := zbChannel.Height/((MaxY-MinY)*1.2);
      zbChannel.Magnification := MagX;
      zbChannel.Exaggeration := MagY/MagX;
      for Index := 0 to 7 do
      begin
        if PointOK[Index] then
        begin
          SectionPoints[PointIndex].X := zbChannel.XCoord(XArray[Index]);
          SectionPoints[PointIndex].Y := zbChannel.YCoord(YArray[Index]);
          Inc(PointIndex);
        end
        else
        begin
          if PointIndex > 0 then
          begin
            SetLength(SectionPoints,PointIndex);
            DrawBigPolyline32(ABitMap, clBlack32, 1, SectionPoints, True);
            SetLength(SectionPoints,8);
            PointIndex := 0;
          end;
        end;
      end;
      if PointIndex > 0 then
      begin
        SetLength(SectionPoints,PointIndex);
        DrawBigPolyline32(ABitMap, clBlack32, 1, SectionPoints, True);
      end;
    end;
  end;
end;

procedure TframeScreenObjectSFR.DrawFlowDepth(ABitMap: TBitmap32);
begin
  DrawFlowTable(ABitMap, 1, zbFlowDepthTable);
end;

procedure TframeScreenObjectSFR.DrawFlowWidth(ABitMap: TBitmap32);
begin
  DrawFlowTable(ABitMap, 2, zbFlowWidthTable);
end;

procedure TframeScreenObjectSFR.EnableUnsatControls;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := ISFROPT in [4,5];
  jceSaturatedVolumetricWaterUpstream.Enabled := ShouldEnable;
  jceSaturatedVolumetricWaterDownstream.Enabled := ShouldEnable;
  jceInitialVolumetricWaterUpstream.Enabled := ShouldEnable;
  jceInitialVolumetricWaterDownstream.Enabled := ShouldEnable;
  jceBrooksCoreyExponentUpstream.Enabled := ShouldEnable;
  jceBrooksCoreyExponentDownstream.Enabled := ShouldEnable;
  ShouldEnable := ISFROPT  = 5;

  jceMaxUnsaturatedKzUpstream.Enabled := ShouldEnable;
  jceMaxUnsaturatedKzDownstream.Enabled := ShouldEnable;

  ShouldEnable := ISFROPT in [2,3];
  jceSaturatedVolumetricWater.Enabled := ShouldEnable;
  jceInitialVolumetricWater.Enabled := ShouldEnable;
  jceBrooksCoreyExponent.Enabled := ShouldEnable;
  ShouldEnable := ISFROPT = 3;
  jceMaxUnsaturatedKz.Enabled := ShouldEnable;

  ShouldEnable := ISFROPT in [1,2,3];
  jceStreamTop.Enabled := ShouldEnable;
  jceSlope.Enabled := ShouldEnable;
  jceStreambedThickness.Enabled := ShouldEnable;
  jceStreambedK.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectSFR.frameCrossSection1dg8PointButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  OnButtonClick(Sender, ACol, ARow);
end;

procedure TframeScreenObjectSFR.frameCrossSection1dg8PointSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  dg8PointSelectCell(Sender, ACol, ARow, CanSelect);
end;

procedure TframeScreenObjectSFR.frameCrossSection1dg8PointSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  dgCrossSectionSetEditText(Sender, ACol, ARow, Value);
end;

procedure TframeScreenObjectSFR.frameFlowTable1btnDeleteFlowTableRowClick(
  Sender: TObject);
begin
  inherited;
  frameFlowTable1.btnDeleteFlowTableRowClick(Sender);

end;

procedure TframeScreenObjectSFR.frameFlowTable1btnInsertFlowTableRowClick(
  Sender: TObject);
begin
  inherited;
  frameFlowTable1.btnInsertFlowTableRowClick(Sender);

end;

procedure TframeScreenObjectSFR.frameFlowTable1dgSfrTableButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if Assigned(OnButtonClick) then
  begin
    OnButtonClick(Sender, ACol, ARow);
  end;
end;

procedure TframeScreenObjectSFR.frameFlowTable1dgSfrTableSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  dgFlowTableSetEditText(Sender, ACol, ARow, Value);
end;

procedure TframeScreenObjectSFR.frameFlowTable1seTableCountChange(
  Sender: TObject);
begin
  inherited;
  frameFlowTable1.seTableCountChange(Sender);

end;

procedure TframeScreenObjectSFR.FrameResize(Sender: TObject);
begin
  LayoutMultiRowFlowEditControls;
  LayoutMultiRowUpstreamEditControls;
  LayoutMultiRowDownstreamEditControls;
  LayoutMultiRowChannelEditControls;
  LayoutMultiRowEquationEditControls;
  LayoutMultiRowParamIcalcControls;
end;

procedure TframeScreenObjectSFR.GetData(List: TScreenObjectEditCollection);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  FoundFirst: boolean;
  Boundary: TSfrBoundary;
  First: boolean;
  Gage0: TCheckBoxState;
  Gage1: TCheckBoxState;
  Gage2: TCheckBoxState;
  Gage3: TCheckBoxState;
  Gage5: TCheckBoxState;
  Gage6: TCheckBoxState;
  Gage7: TCheckBoxState;
  NewState: TCheckBoxState;
  NewLocation : integer;
begin
{$IFNDEF PEST}
  tabObservations.TabVisible := False;
{$ENDIF}
  rdgParameters.BeginUpdate;
  dgFlowTimes.BeginUpdate;
  dgUp.BeginUpdate;
  dgDown.BeginUpdate;
  dgSfrRough.BeginUpdate;
  dgSfrEquation.BeginUpdate;
  dgTableTime.BeginUpdate;
  rdgNetwork.BeginUpdate;
  frameExternalFileValues.Grid.BeginUpdate;
  FGettingData := True;
  try
    frameSfrPestObs.InitializeControls;
    frameSfrPestObs.SpecifyObservationTypes(StreamGageOutputTypes);
    FoundFirst := False;

    rdeSegmentNumber.Text := '';

    jceStreamTop.Text := '';
    jceSlope.Text := '';
    jceStreambedThickness.Text := '';
    jceStreambedK.Text := '';
    jceSaturatedVolumetricWater.Text := '';
    jceInitialVolumetricWater.Text := '';
    jceBrooksCoreyExponent.Text := '';
    jceMaxUnsaturatedKz.Text := '';
    jvcReachLength.Text := StrObjectIntersectLength;

    jceSaturatedVolumetricWaterUpstream.Text := '';
    jceInitialVolumetricWaterUpstream.Text := '';
    jceBrooksCoreyExponentUpstream.Text := '';
    jceMaxUnsaturatedKzUpstream.Text := '';
    jceSaturatedVolumetricWaterDownstream.Text := '';
    jceInitialVolumetricWaterDownstream.Text := '';
    jceBrooksCoreyExponentDownstream.Text := '';
    jceMaxUnsaturatedKzDownstream.Text := '';

    ClearTable(rdgParameters);
    ClearTable(dgFlowTimes);
    ClearTable(dgUp);
    ClearTable(dgDown);
    ClearTable(dgSfrRough);
    ClearTable(dgSfrEquation);
    ClearTable(dgTableTime);
    ClearTable(rdgNetwork);

    rdgParameters.Cells[Ord(spicParameter), 1] :=
      rdgParameters.Columns[Ord(spicParameter)].PickList[0];
    rdgParameters.Cells[Ord(spicIcalc), 1] :=
      rdgParameters.Columns[Ord(spicIcalc)].PickList[0];

    EnableTabs;
    FrameCrossSection1.ClearTable;

    FrameFlowTable1.ClearTable;

    tabExternalFlowFile.TabVisible := List.Count = 1;

    rdeSegmentNumber.Enabled := True;

    FListCount := List.Count;

    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Boundary := Item.ScreenObject.ModflowSfrBoundary;
      if (Boundary <> nil) and Boundary.Used then
      begin
        if FoundFirst then
        begin
          rdeSegmentNumber.Text := '';
          rdeSegmentNumber.Enabled := False;
        end
        else
        begin
          rdeSegmentNumber.Text := IntToStr(Boundary.SegmentNumber);
        end;

        GetSfrValues(Boundary, FoundFirst);
        GetParamIcalcValues(Boundary, FoundFirst);
        GetSfrFlows(Boundary, FoundFirst);
        GetSfrSegments(Boundary, FoundFirst);
        GetSfrChannel(Boundary, FoundFirst);
        GetSfrEquation(Boundary, FoundFirst);
        GetSfrFlowTable(Boundary, FoundFirst);
        GetUnsaturatedValues(Boundary, FoundFirst);
        GetExternalFlows(Boundary);

        FoundFirst := True;
      end;
    end;
    First := True;
    Gage0 := cbUnChecked;
    Gage1 := cbUnChecked;
    Gage2 := cbUnChecked;
    Gage3 := cbUnChecked;
    Gage5 := cbUnChecked;
    Gage6 := cbUnChecked;
    Gage7 := cbUnChecked;
    NewLocation := -2;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Boundary := Item.ScreenObject.ModflowSfrBoundary;
      if (Boundary <> nil) and Boundary.Used then
      begin
        if First then
        begin
          NewLocation := Ord(Boundary.GageLocation);
          if Boundary.Gage0 then
          begin
            Gage0 := cbChecked;
          end
          else
          begin
            Gage0 := cbUnChecked;
          end;

          if Boundary.Gage1 then
          begin
            Gage1 := cbChecked;
          end
          else
          begin
            Gage1 := cbUnChecked;
          end;

          if Boundary.Gage2 then
          begin
            Gage2 := cbChecked;
          end
          else
          begin
            Gage2 := cbUnChecked;
          end;

          if Boundary.Gage3 then
          begin
            Gage3 := cbChecked;
          end
          else
          begin
            Gage3 := cbUnChecked;
          end;

          if Boundary.Gage5 then
          begin
            Gage5 := cbChecked;
          end
          else
          begin
            Gage5 := cbUnChecked;
          end;

          if Boundary.Gage6 then
          begin
            Gage6 := cbChecked;
          end
          else
          begin
            Gage6 := cbUnChecked;
          end;

          if Boundary.Gage7 then
          begin
            Gage7 := cbChecked;
          end
          else
          begin
            Gage7 := cbUnChecked;
          end;
          First := False;
        end
        else
        begin
          if NewLocation <> Ord(Boundary.GageLocation) then
          begin
            NewLocation := -1;
          end;

          if Boundary.Gage0 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage0 <> NewState then
          begin
            Gage0 := cbGrayed;
          end;

          if Boundary.Gage1 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage1 <> NewState then
          begin
            Gage1 := cbGrayed;
          end;

          if Boundary.Gage2 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage2 <> NewState then
          begin
            Gage2 := cbGrayed;
          end;

          if Boundary.Gage3 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage3 <> NewState then
          begin
            Gage3 := cbGrayed;
          end;

          if Boundary.Gage5 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage5 <> NewState then
          begin
            Gage5 := cbGrayed;
          end;

          if Boundary.Gage6 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage6 <> NewState then
          begin
            Gage6 := cbGrayed;
          end;

          if Boundary.Gage7 then
          begin
            NewState := cbChecked;
          end
          else
          begin
            NewState := cbUnChecked;
          end;
          if Gage7 <> NewState then
          begin
            Gage7 := cbGrayed;
          end;
        end;
      end;
    end;
    rgGages.ItemIndex := NewLocation;
    rgGagesClick(nil);

    if List.Count = 1 then
    begin
      if (Boundary <> nil) and Boundary.Used then
      begin
        frameSfrPestObs.GetData(Boundary.Observations);
      end;
    end;

    cbGagStandard.State := Gage0;
    cbGag1.State := Gage1;
    cbGag2.State := Gage2;
    cbGag3.State := Gage3;
    cbGag5.State := Gage5;
    cbGag6.State := Gage6;
    cbGag7.State := Gage7;
    UpdateKCaption;
    FrameCrossSection1.GetData(List);
    FrameCrossSection1.DisplayData(0);
    frameFlowTable1.GetData(List);
    frameFlowTable1.DisplayData(0);
  finally
    FGettingData := False;
    UpdateICalc;
    frameExternalFileValues.Grid.EndUpdate;
    rdgNetwork.EndUpdate;
    rdgParameters.EndUpdate;
    dgFlowTimes.EndUpdate;
    dgUp.EndUpdate;
    dgDown.EndUpdate;
    dgSfrRough.EndUpdate;
    dgSfrEquation.EndUpdate;
    dgTableTime.EndUpdate;
  end;
  FTimesChanged := False;
  LayoutMultiRowFlowEditControls;
  LayoutMultiRowParamIcalcControls;
  LayoutMultiRowNetworkControls;
  LayoutMultiRowChannelEditControls;
  LayoutMultiRowEquationEditControls;
  LayoutMultiRowUpstreamEditControls;
  LayoutMultiRowDownstreamEditControls;

end;

procedure TframeScreenObjectSFR.GetEndTimes(Grid: TRbwDataGrid4; Col: integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithEndTimes(Grid, Col);
end;

procedure TframeScreenObjectSFR.PaintFlowWidth(Sender: TObject; Buffer:
  TBitmap32);
var
  ABitMap: TBitmap32;
begin
  Buffer.BeginUpdate;
  try
    ABitMap := TBitmap32.Create;
    try
      ABitMap.Height := Buffer.Height;
      ABitMap.Width := Buffer.Width;
      ABitMap.DrawMode := dmBlend;
      DrawFlowWidth(ABitMap);
      Buffer.Draw(0, 0, ABitMap);
    finally
      ABitMap.Free;
    end;
  finally
    Buffer.EndUpdate;
  end;
end;

procedure TframeScreenObjectSFR.PaintFlowDepth(Sender: TObject; Buffer:
  TBitmap32);
var
  ABitMap: TBitmap32;
begin
  Buffer.BeginUpdate;
  try
    ABitMap := TBitmap32.Create;
    try
      ABitMap.Height := Buffer.Height;
      ABitMap.Width := Buffer.Width;
      ABitMap.DrawMode := dmBlend;
      DrawFlowDepth(ABitMap);
      Buffer.Draw(0, 0, ABitMap);
    finally
      ABitMap.Free;
    end;
  finally
    Buffer.EndUpdate;
  end;
end;

procedure TframeScreenObjectSFR.PaintCrossSection(Sender: TObject; Buffer:
  TBitmap32);
var
  ABitMap: TBitmap32;
begin
  Buffer.BeginUpdate;
  try
    ABitMap := TBitmap32.Create;
    try
      ABitMap.Height := Buffer.Height;
      ABitMap.Width := Buffer.Width;
      ABitMap.DrawMode := dmBlend;
      DrawCrossSection(ABitMap);
      Buffer.Draw(0, 0, ABitMap);
    finally
      ABitMap.Free;
    end;
  finally
    Buffer.EndUpdate;
  end;
end;

end.
