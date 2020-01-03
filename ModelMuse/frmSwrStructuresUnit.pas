unit frmSwrStructuresUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls, JvPageList, JvExControls, JvExExtCtrls, JvNetscapeSplitter,
  frameGridUnit, Grids, RbwDataGrid4, ModflowSwrStructureUnit, UndoItems,
  ComCtrls, frameFormulaGridUnit, frameStructureGridUnit, JvExStdCtrls,
  JvCombobox, JvListComb, ArgusDataEntry, framePlotGridUnit, GrayTabs;

type
  {
    ccDownstreamInvertElevation: STRINV2
    ccCulvertType: STRWID
    ccCulvertRise: STRWID2
    ccSpecifyLengths: STRLEN, STRLEN2
    ccCulvertLength: STRLEN
    ccDownstreamCulvertLength: STRLEN2
    ccRoughness: STRMAN

  }
  TCulvertColumns = (ccName, ccDownstreamInvertElevation, ccCulvertType,
    ccCulvertRise, ccSpecifyLengths, ccCulvertLength, ccDownstreamCulvertLength,
    ccRoughness);

{
  drcElevation: STRELEV
  drcDischarge: STRQ
}

  TDischargeRatingColumns = (drcElevation, drcDischarge);

{
  pwcControlType: CSTROTYP
  pwcControlReach: ISTROTCH
  pwcConnectedReach: ISTROQCON
  pwcControlOperated: CSTROLO
  pwcCriticalMethod: CSTRCRIT
  pwcCriticalTabFileName: CSTRCRIT
  pwcCriticalValue: STRCRIT
  pwcControlOffsetCriterion: STRCRITC
  pwcStartingControlRate: STRRT
  pwcMaximumControlRate: STRMAX
  pwcDischargeTabFile: CSTRVAL
}

  TPumpWeirColumns = (pwcName, pwcControlType, pwcControlReach,
    pwcConnectedReach, pwcControlOperated, pwcCriticalMethod,
    pwcCriticalTabFileName, pwcCriticalValue, pwcControlOffsetCriterion,
    pwcStartingControlRate, pwcMaximumControlRate, pwcDischargeTabFile);
  TPumpWeirColumnSet = set of TPumpWeirColumns;

  {
    scSmoothingMethod: STRWSMO
    scSmoothing: STRWSMO
  }

  TSmoothingColumns = (scName, scSmoothingMethod, scSmoothing);

  TUndoSwrStructures = class(TCustomUndo)
  private
    FOldStructures: TStructureCollection;
    FNewStructures: TStructureCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var Structures: TStructureCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSwrStructures = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    splttrMain: TJvNetscapeSplitter;
    jvplOptions: TJvPageList;
    jvspStageDischarge: TJvStandardPage;
    frameStageDischarge: TframePlotGrid;
    jvspCulvert: TJvStandardPage;
    rdgCulvert: TRbwDataGrid4;
    jvspPumpWeir: TJvStandardPage;
    rdgPumpWeir: TRbwDataGrid4;
    pcStructures: TPageControl;
    tabStructureDefinition: TTabSheet;
    tabTiming: TTabSheet;
    frameTiming: TframeFormulaGrid;
    jvspBlank: TJvStandardPage;
    frameMain: TframeStructureGrid;
    pnlCulvert: TPanel;
    comboCulvertType: TJvImageComboBox;
    rdeCulvertValue: TRbwDataEntry;
    cbCulvert: TCheckBox;
    pnlPumpWeir: TPanel;
    comboPumpWeirControlType: TJvImageComboBox;
    comboPumpWeirControlOperated: TJvImageComboBox;
    comboPumpWeirCriticalMethod: TJvImageComboBox;
    comboPumpWeirCriticalTabFileName: TJvImageComboBox;
    comboPumpWeirDischargeTabFile: TJvImageComboBox;
    rdePumpWeirValue: TRbwDataEntry;
    jvspSmoothing: TJvStandardPage;
    rdgSmoothing: TRbwDataGrid4;
    procedure frameMainGridseNumberChange(Sender: TObject);
    procedure frameMainGridGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameMainGridGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameMainGridGridTopLeftChanged(Sender: TObject);
    procedure rdgCulvertSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure frameMainsbAddClick(Sender: TObject);
    procedure frameMainsbInsertClick(Sender: TObject);
    procedure frameMainsbDeleteClick(Sender: TObject);
    procedure frameStageDischargeseNumberChange(Sender: TObject);
    procedure frameStageDischargesbAddClick(Sender: TObject);
    procedure frameStageDischargesbInsertClick(Sender: TObject);
    procedure frameStageDischargesbDeleteClick(Sender: TObject);
    procedure frameStageDischargeGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgCulvertSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgCulvertStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure FormShow(Sender: TObject);
    procedure rdgPumpWeirSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgPumpWeirSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameTimingGridStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure frameTimingGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameTimingseNumberChange(Sender: TObject);
    procedure frameTimingsbInsertClick(Sender: TObject);
    procedure frameTimingsbDeleteClick(Sender: TObject);
    procedure frameMainseNumberChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure rdgCulvertHorizontalScroll(Sender: TObject);
    procedure rdgCulvertColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgCulvertMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboCulvertTypeChange(Sender: TObject);
    procedure rdeCulvertValueChange(Sender: TObject);
    procedure cbCulvertClick(Sender: TObject);
    procedure rdgPumpWeirHorizontalScroll(Sender: TObject);
    procedure rdgPumpWeirColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgPumpWeirMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboPumpWeirControlTypeChange(Sender: TObject);
    procedure comboPumpWeirControlOperatedChange(Sender: TObject);
    procedure comboPumpWeirCriticalMethodChange(Sender: TObject);
    procedure comboPumpWeirCriticalTabFileNameChange(Sender: TObject);
    procedure comboPumpWeirDischargeTabFileChange(Sender: TObject);
    procedure rdgPumpWeirMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdePumpWeirValueChange(Sender: TObject);
    procedure frameStageDischargepbPlotPaint(Sender: TObject);
    procedure frameStageDischargeGridEndUpdate(Sender: TObject);
    procedure rdgSmoothingSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgSmoothingSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FStructures: TStructureCollection;
    FSelectedRow: Integer;
    FStructure: TStructure;
    FDisplayingTable: Boolean;
    FGettingData: Boolean;
    FDeletingStructure: Boolean;
    FShowWarnings: Boolean;
    procedure GetData;
    procedure SetData;
    procedure DisplayStageDischargeTable(Structure: TStructure);
    procedure ClearGridRow(Grid: TRbwDataGrid4; Row: Integer);
    procedure UpdateNextTimeCell(ACol, ARow: Integer);
    procedure LayoutCulvertControls;
    procedure LayoutPumpWeirControls;
    procedure SetSelectedPumpWeirValues(const AValue: string; Columns: TPumpWeirColumnSet);
    procedure EnablePumpWeirMultiControl(Columns: TPumpWeirColumnSet; AControl: TControl);
    procedure CheckStructuresUsed;
    { Private declarations }
  public
    class var
    SelectedStructureIndex: integer;
    AddNewStructure: Boolean;
    Reach: Integer;
    { Public declarations }
  end;

var
  frmSwrStructures: TfrmSwrStructures;


implementation

uses
  frmGoPhastUnit, ModflowSwrTabfilesUnit, GoPhastTypes, Math,
  Generics.Collections, xygraph, frmErrorsAndWarningsUnit, PhastModelUnit;

resourcestring
  StrNameISTRNUM = 'Name';
  StrReachISTRNUM = 'Reach (ISMODRCH, ISTRRCH)';
  StrConnectedReachIST = 'Connected Reach (ISTRCONN)';
  StrStructureTypeISTR = 'Structure Type (ISTRTYPE)';
  StrWeirDischargeCoeff = 'Weir Discharge Coefficient (STRCD)';
  StrOrificeDischargeCo = 'Orifice Discharge Coefficient (STRCD2)';
  StrSubmergenceExponent = 'Submergence Exponent (STRCD3)';
  StrStructureInvertEle = 'Structure Invert Elevation (STRINV)';
  StrStructureWidthSTR = 'Structure Width (STRWID)';
  StrInitialValueSTRVA = 'Initial Value (STRVAL)';
  StrDownstreamInvertEl = 'Downstream Invert Elevation (STRINV2)';
  StrCulvertTypeSTRWID = 'Culvert Type (STRWID)';
  StrCulvertRiseSTRWID = 'Culvert Rise (STRWID2)';
  StrSpecifyLengthsSTR = 'Specify Lengths (STRLEN and STRLEN2)';
  StrCulvertLengthSTRL = 'Culvert Length (STRLEN)';
  StrDownstreamCulvertL = 'Downstream Culvert Length (STRLEN2)';
  StrManningsRoughness = 'Manning''s Roughness (STRMAN)';
  StrElevationSTRELEV = 'Elevation (STRELEV)';
  StrDischargeSTRQ = 'Discharge (STRQ)';
  StrControlTypeCSTROT = 'Control Type (CSTROTYP)';
  StrControlReachISTRO = 'Control Reach (ISTRORCH)';
  StrConnectedControlRe = 'Connected Control Reach (ISTROQCON)';
  StrWhenIsStructureOp = 'When is Structure Operated (CSTROLO)';
  StrMethodForSpecifyin = 'Method for Specifying Critical Value (CSTRCRIT)';
  StrCriticalValueTabfi = 'Critical Value Tabfile Name (CSTRCRIT)';
  StrCriticalValueCSTR = 'Critical Value (STRCRIT)';
  StrControlOffsetCrite = 'Control Offset Criterion (STRCRITC)';
  StrInitialControlRate = 'Initial Control Rate (STRRT)';
  StrMaximumControlRate = 'Maximum Control Rate (STRMAX)';
  StrOptionalDischargeT = '(Optional) Discharge Tabfile Name (CSTRVAL)';
  StrFlowRestrictionsI = 'Flow Restrictions (ISTRDIR)';
  StrSFRSegmentISFRSEG = 'SFR Segment (ISFRSEG)';
  StrSFRReachISFRRCH = 'SFR Reach (ISFRRCH)';
  StrMethodForInitial = 'Method for Specifying Initial Value (STRVAL)';
  StrInitialValueTabfil = 'Initial Value Tabfile Name (STRVAL)';
  StrSTRVAL = 'STRVAL';
  StrSmoothingMethodST = 'Smoothing method (STRWSMO)';
  StrSmoothingValueSTR = 'Smoothing value (STRWSMO)';

{$R *.dfm}

type
  TGridCrack = class(TStringGrid);

procedure TfrmSwrStructures.CheckStructuresUsed;
var
  ColIndex: Integer;
  RowIndex: Integer;
  StructureUsed: Boolean;
begin
  FShowWarnings := False;
  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel, StrTheFollowingSWRSt);
  for ColIndex := 2 to frameTiming.Grid.ColCount - 1 do
  begin
    StructureUsed := False;
    for RowIndex := 1 to frameTiming.Grid.RowCount - 1 do
    begin
      StructureUsed := frameTiming.Grid.Checked[ColIndex,RowIndex];
      if StructureUsed then
      begin
        break;
      end;
    end;
    if not StructureUsed then
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrTheFollowingSWRSt,
        frameTiming.Grid.Cells[ColIndex,0]);
      FShowWarnings := True;
    end;
  end;
end;

procedure TfrmSwrStructures.btnOKClick(Sender: TObject);
begin
  inherited;
  CheckStructuresUsed;
  SetData;
  if FShowWarnings then
  begin
    frmErrorsAndWarnings.ShowAfterDelay;
  end;
end;

procedure TfrmSwrStructures.FormCreate(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  DummyEvent: TNotifyEvent;
  TabFileIndex: Integer;
  SwrTabFiles: TTabFileCollection;
  TabFileItem: TTabFileItem;
  TabFiles: TStringList;
begin
  inherited;
  pcStructures.ActivePageIndex := 0;
  jvplOptions.ActivePageIndex := 0;

  Grid := frameMain.Grid;
  Grid.Cells[Ord(mcName), 0] := StrNameISTRNUM;
  Grid.Cells[Ord(mcReach), 0] := StrReachISTRNUM;
  Grid.Cells[Ord(mcConnectedReach), 0] := StrConnectedReachIST;
  Grid.Cells[Ord(mcRestrictions), 0] := StrFlowRestrictionsI;
  Grid.Cells[Ord(mcStructureType), 0] := StrStructureTypeISTR;
  Grid.Cells[Ord(mcWeirDischarge), 0] := StrWeirDischargeCoeff;
  Grid.Cells[Ord(mcOrificeDischarge), 0] := StrOrificeDischargeCo;
  Grid.Cells[Ord(mcSubmergenceExponent), 0] := StrSubmergenceExponent;
  Grid.Cells[Ord(mcInvertElevation), 0] := StrStructureInvertEle;
  Grid.Cells[Ord(mcStructureWidth), 0] := StrStructureWidthSTR;
  Grid.Cells[Ord(mcInitialValueMethod), 0] := StrMethodForInitial;
  Grid.Cells[Ord(mcInitialValue), 0] := StrInitialValueSTRVA;
  Grid.Cells[Ord(mcInitialValueTabFile), 0] := StrInitialValueTabfil;
  Grid.Cells[Ord(mcSfrSegment), 0] := StrSFRSegmentISFRSEG;
  Grid.Cells[Ord(mcSfrReach), 0] := StrSFRReachISFRRCH;

  frameMain.lblType.Caption := StrStructureTypeISTR;
  frameMain.lblRestrictions.Caption := StrFlowRestrictionsI;
  frameMain.lblInitialValueMethod.Caption := StrSTRVAL;
  frameMain.lblInitialValueTabFile.Caption := StrInitialValueTabfil;

  comboCulvertType.Items.Assign(rdgCulvert.Columns[Ord(ccCulvertType)].PickList);
//  cbCulvert.Caption := StrSpecifyLengthsSTR

  rdgCulvert.Cells[Ord(ccName), 0] := StrNameISTRNUM;
  rdgCulvert.Cells[Ord(ccDownstreamInvertElevation), 0] := StrDownstreamInvertEl;
  rdgCulvert.Cells[Ord(ccCulvertType), 0] := StrCulvertTypeSTRWID;
  rdgCulvert.Cells[Ord(ccCulvertRise), 0] := StrCulvertRiseSTRWID;
  rdgCulvert.Cells[Ord(ccSpecifyLengths), 0] := StrSpecifyLengthsSTR;
  rdgCulvert.Cells[Ord(ccCulvertLength), 0] := StrCulvertLengthSTRL;
  rdgCulvert.Cells[Ord(ccDownstreamCulvertLength), 0] := StrDownstreamCulvertL;
  rdgCulvert.Cells[Ord(ccRoughness), 0] := StrManningsRoughness;

  frameStageDischarge.Grid.Cells[Ord(drcElevation), 0] := StrElevationSTRELEV;
  frameStageDischarge.Grid.Cells[Ord(drcDischarge), 0] := StrDischargeSTRQ;

  rdgPumpWeir.Cells[Ord(pwcName), 0] :=  StrNameISTRNUM;
  rdgPumpWeir.Cells[Ord(pwcControlType), 0] :=  StrControlTypeCSTROT;
  rdgPumpWeir.Cells[Ord(pwcControlReach), 0] :=  StrControlReachISTRO;
  rdgPumpWeir.Cells[Ord(pwcConnectedReach), 0] :=  StrConnectedControlRe;
  rdgPumpWeir.Cells[Ord(pwcControlOperated), 0] :=  StrWhenIsStructureOp;
  rdgPumpWeir.Cells[Ord(pwcCriticalMethod), 0] :=  StrMethodForSpecifyin;
  rdgPumpWeir.Cells[Ord(pwcCriticalTabFileName), 0] :=  StrCriticalValueTabfi;
  rdgPumpWeir.Cells[Ord(pwcCriticalValue), 0] :=  StrCriticalValueCSTR;
  rdgPumpWeir.Cells[Ord(pwcControlOffsetCriterion), 0] :=  StrControlOffsetCrite;
  rdgPumpWeir.Cells[Ord(pwcStartingControlRate), 0] :=  StrInitialControlRate;
  rdgPumpWeir.Cells[Ord(pwcMaximumControlRate), 0] :=  StrMaximumControlRate;
  rdgPumpWeir.Cells[Ord(pwcDischargeTabFile), 0] :=  StrOptionalDischargeT;

  rdgSmoothing.Cells[Ord(scName), 0] := StrNameISTRNUM;
  rdgSmoothing.Cells[Ord(scSmoothingMethod), 0] := StrSmoothingMethodST;
  rdgSmoothing.Cells[Ord(scSmoothing), 0] := StrSmoothingValueSTR;

  frameTiming.Grid.Cells[0,0] := StrStartingTime;
  frameTiming.Grid.Cells[1,0] := StrEndingTime;

  FSelectedRow := -1;

  TabFiles := TStringList.Create;
  try
    SwrTabFiles := frmGoPhast.PhastModel.SwrTabFiles;
    for TabFileIndex := 0 to SwrTabFiles.Count - 1 do
    begin
      TabFileItem := SwrTabFiles[TabFileIndex];
      if TabFileItem.TabType = ttStructure then
      begin
        TabFiles.Add(TabFileItem.FullTabFileName);
      end;
    end;
    rdgPumpWeir.Columns[Ord(pwcCriticalTabFileName)].PickList := TabFiles;
    rdgPumpWeir.Columns[Ord(pwcDischargeTabFile)].PickList := TabFiles;
    Grid.Columns[Ord(mcInitialValueTabFile)].PickList := TabFiles;
  finally
    TabFiles.Free;
  end;

  comboPumpWeirControlType.Items.Assign(rdgPumpWeir.Columns[Ord(pwcControlType)].PickList);
  comboPumpWeirControlOperated.Items.Assign(rdgPumpWeir.Columns[Ord(pwcControlOperated)].PickList);
  comboPumpWeirCriticalMethod.Items.Assign(rdgPumpWeir.Columns[Ord(pwcCriticalMethod)].PickList);
  comboPumpWeirCriticalTabFileName.Items.Assign(rdgPumpWeir.Columns[Ord(pwcCriticalTabFileName)].PickList);
  comboPumpWeirDischargeTabFile.Items.Assign(rdgPumpWeir.Columns[Ord(pwcDischargeTabFile)].PickList);

  DummyEvent := nil;
  FStructures := TStructureCollection.Create(nil);

  GetData;
end;

procedure TfrmSwrStructures.FormDestroy(Sender: TObject);
begin
  inherited;
  FStructures.Free;
end;

procedure TfrmSwrStructures.FormResize(Sender: TObject);
begin
  inherited;
  LayoutCulvertControls;
  LayoutPumpWeirControls;
end;

procedure TfrmSwrStructures.FormShow(Sender: TObject);
begin
  inherited;
//  TGridCrack(rdgCulvert).HideEditor;

end;

procedure TfrmSwrStructures.DisplayStageDischargeTable(Structure: TStructure);
var
  ItemIndex: Integer;
  AnItem: TStructureDischargeItem;
  Grid: TRbwDataGrid4;
begin
  FDisplayingTable := True;
  try
    FStructure := Structure;
    frameStageDischarge.seNumber.AsInteger := Structure.Table.Count;
    frameStageDischarge.seNumber.OnChange(nil);
    Grid := frameStageDischarge.Grid;
    for ItemIndex := 0 to Structure.Table.Count - 1 do
    begin
      AnItem := Structure.Table[ItemIndex];
      Grid.RealValue[Ord(drcElevation),ItemIndex+1] := AnItem.Elev;
      Grid.RealValue[Ord(drcDischarge),ItemIndex+1] := AnItem.Discharge;
    end;
  finally
    FDisplayingTable := False;
  end;
  frameStageDischarge.pbPlot.Invalidate;
end;

procedure TfrmSwrStructures.EnablePumpWeirMultiControl(
  Columns: TPumpWeirColumnSet; AControl: TControl);
var
  ColIndex: Integer;
  AColumnItem: TPumpWeirColumns;
  RowIndex: integer;
  ShouldEnable: boolean;
begin
  ShouldEnable := False;
  for AColumnItem in Columns do
  begin
    ColIndex := Ord(AColumnItem);
    for RowIndex := rdgPumpWeir.FixedRows to rdgPumpWeir.RowCount - 1 do
    begin
      ShouldEnable := rdgPumpWeir.IsSelectedCell(ColIndex,RowIndex);
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
  AControl.Enabled := ShouldEnable;
end;

procedure TfrmSwrStructures.frameMainGridGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  StructureItemIndex: Integer;
  MainCol: TMainColumns;
begin
  inherited;
  if (ARow >= 1) and (ACol >= 0) then
  begin
    StructureItemIndex := frameMain.Grid.ItemIndex[Ord(mcStructureType),ARow];
    MainCol := TMainColumns(ACol);
    case MainCol of
      mcName, mcStructureType, mcReach: ; // do nothing
      mcConnectedReach:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and not (TSwrStructureType(StructureItemIndex) in
            [sstSfrInflow, sstManning, sstOverbankFlow]);
        end;
      mcRestrictions:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstCulvert, sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
            sstGatedSpillway, sstSpillEquation, sstOverbankFlow]);
        end;
       mcWeirDischarge, mcStructureWidth:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstCulvert, sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
            sstGatedSpillway, sstSpillEquation]);
        end;
      mcOrificeDischarge:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstCulvert, sstFixedSpillway,
            sstGatedSpillway, sstSpillEquation]);
        end;
      mcSubmergenceExponent:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
            sstGatedSpillway, sstSpillEquation, sstOverbankFlow]);
        end;
      mcInvertElevation:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstUncontrolledZeroDepth, sstCulvert, sstFixedWeir, sstFixedSpillway,
            sstMoveableWeir, sstGatedSpillway, sstSpillEquation]);
        end;
      mcInitialValueMethod:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstManning, sstOverbankFlow]);
        end;
      mcInitialValue:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in
            [sstPump, sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
            sstGatedSpillway, sstSpillEquation, sstManning, sstOverbankFlow]);
          if CanSelect and (TSwrStructureType(StructureItemIndex) in
            [sstManning]) then
          begin
            CanSelect := frameMain.Grid.ItemIndex[Ord(mcInitialValueMethod),ARow] = 0;
          end;
        end;
      mcInitialValueTabFile:
        begin
          CanSelect := (StructureItemIndex >= 0)
            and (TSwrStructureType(StructureItemIndex) in [sstManning, sstOverbankFlow])
            and (frameMain.Grid.ItemIndex[Ord(mcInitialValueMethod),ARow] = 1);
        end;
      mcSfrSegment, mcSfrReach:
        begin
          CanSelect := frmGoPhast.PhastModel.SfrIsSelected;
        end
      else Assert(False);
    end;

    if not frameMain.Grid.Drawing then
    begin
      if frameMain.Grid.SelectedRow >= frameMain.Grid.FixedRows then
      begin
        rdgCulvert.Row := frameMain.Grid.SelectedRow;
        rdgPumpWeir.Row := frameMain.Grid.SelectedRow;
      end;
      if StructureItemIndex < 0 then
      begin
        jvplOptions.ActivePage := jvspCulvert;
      end
      else
      begin
        case TSwrStructureType(StructureItemIndex) of
          sstSpecifiedElevation, sstPump, sstMoveableWeir, sstGatedSpillway,
            sstSpillEquation:
            begin
              jvplOptions.ActivePage := jvspPumpWeir;
              if (rdgPumpWeir.Row < rdgPumpWeir.TopRow) then
              begin
                rdgPumpWeir.TopRow := rdgPumpWeir.Row
              end
              else if (rdgPumpWeir.Row > rdgPumpWeir.TopRow + rdgPumpWeir.VisibleRowCount-1) then
              begin
                rdgPumpWeir.TopRow := rdgPumpWeir.Row - rdgPumpWeir.VisibleRowCount+1
              end;
            end;
          sstStageDishargeTable:
            begin
              jvplOptions.ActivePage := jvspStageDischarge;
              if FSelectedRow <> ARow then
              begin
                DisplayStageDischargeTable(FStructures[ARow-1]);
                FSelectedRow := ARow;
              end;
            end;
          sstUncontrolledZeroDepth, sstUncontrolledCriticalDepth,
            sstCulvert:
            begin
              jvplOptions.ActivePage := jvspCulvert;
              if (rdgCulvert.Row < rdgCulvert.TopRow) then
              begin
                rdgCulvert.TopRow := rdgCulvert.Row
              end
              else if (rdgCulvert.Row > rdgCulvert.TopRow + rdgCulvert.VisibleRowCount-1) then
              begin
                rdgCulvert.TopRow := rdgCulvert.Row - rdgCulvert.VisibleRowCount+1
              end;
            end;
          sstOverbankFlow:
            begin
              jvplOptions.ActivePage := jvspSmoothing;
            end;
          sstNone, sstFixedWeir, sstFixedSpillway,
            sstSfrInflow, sstManning:
            begin
              jvplOptions.ActivePage := jvspBlank;
            end

          else
            Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TfrmSwrStructures.frameMainGridGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  MainCol: TMainColumns;
  AStructure: TStructure;
  ItemIndex: Integer;
  Dummy: Boolean;
begin
  inherited;
  if (ACol >= 0) and (ARow >= 1) and not FGettingData and (FStructures <> nil)
    and not FDeletingStructure then
  begin
    MainCol := TMainColumns(ACol);
    while ARow > FStructures.Count do
    begin
      FStructures.Add;
    end;
    frameMain.seNumber.AsInteger := FStructures.Count;

    AStructure := FStructures[ARow-1];
    case MainCol of
      mcName:
        begin
          AStructure.Name := Value;
          rdgCulvert.Cells[Ord(ccName), ARow] := Value;
          rdgPumpWeir.Cells[Ord(pwcName), ARow] := Value;
          rdgSmoothing.Cells[Ord(scName), ARow] := Value;
          frameTiming.Grid.Cells[ARow+1, 0] := Value;
        end;
      mcReach:
        begin
          AStructure.Reach := frameMain.Grid.IntegerValueDefault[ACol, ARow, 1];
        end;
      mcConnectedReach:
        begin
          AStructure.ConnectedReach := frameMain.Grid.IntegerValueDefault[ACol, ARow, 1];
        end;
      mcRestrictions:
        begin
          ItemIndex := frameMain.Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.StructureRestrictions := TStructureRestrictions(ItemIndex);
          end
          else
          begin
            AStructure.StructureRestrictions := srBidirectional;
          end;
        end;
      mcStructureType:
        begin
          ItemIndex := frameMain.Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.StructureType := TSwrStructureType(ItemIndex);
            Dummy := True;
            frameMainGridGridSelectCell(Sender, ACol, ARow, Dummy);
          end;
          rdgCulvert.Options := rdgCulvert.Options + [goAlwaysShowEditor];
          frameMain.Grid.Invalidate;
          rdgCulvert.Invalidate;
          TGridCrack(rdgCulvert).HideEditor;
        end;
      mcWeirDischarge:
        begin
          AStructure.WeirDischargeCoefficient := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcOrificeDischarge:
        begin
          AStructure.OrificeDischargeCoefficient := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcSubmergenceExponent:
        begin
          AStructure.SubmergenceExponent := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcInvertElevation:
        begin
          AStructure.InvertElevation := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcStructureWidth:
        begin
          AStructure.Width := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcInitialValueMethod:
        begin
          ItemIndex := frameMain.Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.InitialFlowRateMethod := TSpecificationMethod(ItemIndex);
            frameMainGridGridSelectCell(Sender, ACol, ARow, Dummy);
          end;
          frameMain.Grid.Invalidate;
        end;
      mcInitialValue:
        begin
          AStructure.InitialFlowRateOrGateOpening := frameMain.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      mcInitialValueTabFile:
        begin
          AStructure.FullInitialFlowRateTabFile := frameMain.Grid.Cells[ACol, ARow];
        end;
      mcSfrSegment:
        begin
          AStructure.SfrSegment := frameMain.Grid.IntegerValueDefault[ACol, ARow, 0];
        end;
      mcSfrReach:
        begin
          AStructure.SfrReach := frameMain.Grid.IntegerValueDefault[ACol, ARow, 0];
        end;
      else Assert(False);
    end;

  end;
end;

procedure TfrmSwrStructures.frameMainGridGridTopLeftChanged(Sender: TObject);
begin
  inherited;
  rdgCulvert.TopRow := frameMain.Grid.TopRow;
end;

procedure TfrmSwrStructures.frameMainGridseNumberChange(Sender: TObject);
var
  OldCount: Integer;
  FirstStructure: TStructure;
  AColumn: TRbwColumn4;
  AStructure: TStructure;
  TimeIndex: Integer;
  ColIndex: Integer;
  StructureIndex: Integer;
begin
  inherited;
  frameMain.seNumberChange(Sender);

  if FDeletingStructure then
  begin
    Exit;
  end;
//  if FGettingData then
//  begin
//    Exit;
//  end;
  OldCount := FStructures.Count;
  if OldCount > 0 then
  begin
    FirstStructure := FStructures.First as TStructure;
  end
  else
  begin
    FirstStructure := nil;
  end;
  FStructures.Count := frameMain.seNumber.AsInteger;
  rdgCulvert.RowCount := frameMain.Grid.RowCount;
  rdgPumpWeir.RowCount := frameMain.Grid.RowCount;
  rdgSmoothing.RowCount := frameMain.Grid.RowCount;
  frameTiming.Grid.ColCount := FStructures.Count + 2;
  if FStructures.Count > OldCount then
  begin
    for ColIndex := OldCount + 2 to frameTiming.Grid.ColCount - 1 do
    begin
      AColumn := frameTiming.Grid.Columns[ColIndex];
      AColumn.Format := rcf4Boolean;
      AColumn.WordWrapCaptions := True;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
    end;
    if FirstStructure <> nil then
    begin
      for StructureIndex := OldCount to FStructures.Count - 1 do
      begin
        AStructure := FStructures[StructureIndex];
        AStructure.Times := FirstStructure.Times;
        for TimeIndex := 0 to AStructure.Times.Count - 1 do
        begin
          AStructure.Times[TimeIndex].Used := False;
        end;
      end;
    end;
  end;
end;

procedure TfrmSwrStructures.frameMainsbAddClick(Sender: TObject);
var
  FirstStructure: TStructure;
  NewStructure: TStructure;
  TimeIndex: Integer;
  AColumn: TRbwColumn4;
begin
  inherited;
  FirstStructure := FStructures.First as TStructure;
  FStructures.Add;
  frameMain.sbAddClick(Sender);
  if FirstStructure <> nil then
  begin
    NewStructure := FStructures.Last as TStructure;
    NewStructure.Times := FirstStructure.Times;
    for TimeIndex := 0 to NewStructure.Times.Count - 1 do
    begin
      NewStructure.Times[TimeIndex].Used := False;
    end;
  end;
  AColumn := frameTiming.Grid.Columns[frameTiming.Grid.ColCount-1];
  AColumn.Format := rcf4Boolean;
  AColumn.WordWrapCaptions := True;
  AColumn.AutoAdjustColWidths := True;
  AColumn.AutoAdjustRowHeights := True;
end;

procedure TfrmSwrStructures.frameMainsbDeleteClick(Sender: TObject);
begin
  inherited;
  try
    if frameMain.Grid.SelectedRow >= 1 then
    begin
      FDeletingStructure := True;
      rdgPumpWeir.DeleteRow(frameMain.Grid.SelectedRow);
      FStructures.Delete(frameMain.Grid.SelectedRow-1);
      rdgCulvert.DeleteRow(frameMain.Grid.SelectedRow);
      frameTiming.Grid.DeleteColumn(frameMain.Grid.SelectedRow+1);
    end;
    frameMain.sbDeleteClick(Sender);
  finally
    FDeletingStructure := False;
  end;
//  frameMain.Grid.FixedRows := 1;
end;

procedure TfrmSwrStructures.cbCulvertClick(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  CanSelect: Boolean;
begin
  inherited;
  rdgCulvert.BeginUpdate;
  try
    ColIndex := Ord(ccSpecifyLengths);
    for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount - 1 do
    begin
      if rdgCulvert.IsSelectedCell(ColIndex, RowIndex) then
      begin
        CanSelect := True;
        if Assigned(rdgCulvert.OnSelectCell) then
        begin
          rdgCulvert.OnSelectCell(rdgCulvert, ColIndex, RowIndex, CanSelect);
        end;
        if CanSelect then
        begin
          rdgCulvert.Checked[ColIndex, RowIndex] := cbCulvert.Checked;
        end;
      end;
    end;
  finally
    rdgCulvert.EndUpdate;
  end
end;

procedure TfrmSwrStructures.ClearGridRow(Grid: TRbwDataGrid4; Row: Integer);
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Cells[ColIndex, Grid.SelectedRow] := '';
    Grid.Checked[ColIndex, Grid.SelectedRow] := False;
  end;
end;

procedure TfrmSwrStructures.comboCulvertTypeChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  CanSelect: Boolean;
begin
  inherited;
  rdgCulvert.BeginUpdate;
  try
    ColIndex := Ord(ccCulvertType);
    for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount - 1 do
    begin
      if rdgCulvert.IsSelectedCell(ColIndex, RowIndex) then
      begin
        CanSelect := True;
        if Assigned(rdgCulvert.OnSelectCell) then
        begin
          rdgCulvert.OnSelectCell(rdgCulvert, ColIndex, RowIndex, CanSelect);
        end;
        if CanSelect then
        begin
          rdgCulvert.Cells[ColIndex, RowIndex] := comboCulvertType.Text;
        end;
      end;
    end;
  finally
    rdgCulvert.EndUpdate;
  end
end;

procedure TfrmSwrStructures.comboPumpWeirControlOperatedChange(Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(comboPumpWeirControlOperated.Text, [pwcControlOperated]);
end;

procedure TfrmSwrStructures.comboPumpWeirControlTypeChange(Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(comboPumpWeirControlType.Text, [pwcControlType]);
end;

procedure TfrmSwrStructures.comboPumpWeirCriticalMethodChange(Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(comboPumpWeirCriticalMethod.Text, [pwcCriticalMethod]);
end;

procedure TfrmSwrStructures.comboPumpWeirCriticalTabFileNameChange(
  Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(comboPumpWeirCriticalTabFileName.Text, [pwcCriticalTabFileName]);
end;

procedure TfrmSwrStructures.comboPumpWeirDischargeTabFileChange(
  Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(comboPumpWeirDischargeTabFile.Text, [pwcDischargeTabFile]);
end;

procedure TfrmSwrStructures.frameMainsbInsertClick(Sender: TObject);
var
  AColumn: TRbwColumn4;
  FirstStructure: TStructure;
  NewStructure: TStructure;
  timeIndex: Integer;
begin
  inherited;
  if frameMain.Grid.SelectedRow >= 1 then
  begin
    FirstStructure := FStructures.First as TStructure;
    FStructures.Insert(frameMain.Grid.SelectedRow-1);
    if FirstStructure <> nil then
    begin
      NewStructure := FStructures[frameMain.Grid.SelectedRow-1];
      NewStructure.Times := FirstStructure.Times;
      for timeIndex := 0 to NewStructure.Times.Count - 1 do
      begin
        NewStructure.Times[timeIndex].Used := False;
      end;
    end;

    rdgCulvert.InsertRow(frameMain.Grid.SelectedRow);
    ClearGridRow(rdgCulvert,frameMain.Grid.SelectedRow);

    rdgPumpWeir.InsertRow(frameMain.Grid.SelectedRow);
    ClearGridRow(rdgPumpWeir,frameMain.Grid.SelectedRow);

    rdgSmoothing.InsertRow(frameMain.Grid.SelectedRow);
    ClearGridRow(rdgSmoothing,frameMain.Grid.SelectedRow);

    frameTiming.Grid.InsertColumn(frameMain.Grid.SelectedRow+1);
    AColumn := frameTiming.Grid.Columns[frameMain.Grid.SelectedRow+1];
    AColumn.Format := rcf4Boolean;
    AColumn.AutoAdjustColWidths := true;
    AColumn.AutoAdjustRowHeights := true;
    AColumn.WordWrapCaptions := true;
  end;
  frameMain.sbInsertClick(Sender);
end;

procedure TfrmSwrStructures.frameMainseNumberChange(Sender: TObject);
begin
  inherited;
  frameMain.seNumberChange(Sender);

end;

procedure TfrmSwrStructures.frameStageDischargeGridEndUpdate(Sender: TObject);
begin
  inherited;
  frameStageDischarge.GridEndUpdate(Sender);
  frameStageDischarge.pbPlot.Invalidate;
end;

procedure TfrmSwrStructures.frameStageDischargeGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  DisCol: TDischargeRatingColumns;
  AnItem: TStructureDischargeItem;
begin
  inherited;
  if (ARow >= 1) and (ACol >= 0) and not FDisplayingTable and (FStructures <> nil) then
  begin
    while ARow > FStructure.Table.Count do
    begin
      FStructure.Table.Add;
    end;
    frameStageDischarge.seNumber.AsInteger := FStructure.Table.Count;

    AnItem := FStructure.Table[ARow-1];
    DisCol := TDischargeRatingColumns(ACol);
    case DisCol of
      drcElevation:
        begin
          AnItem.Elev := frameStageDischarge.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      drcDischarge:
        begin
          AnItem.Discharge := frameStageDischarge.Grid.RealValueDefault[ACol, ARow, 0];
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TfrmSwrStructures.frameStageDischargepbPlotPaint(Sender: TObject);
var
  XList, YList: TList<Double>;
  Grid: TRbwDataGrid4;
  X: double;
  Y: double;
  RowIndex: Integer;
  Data: Tdatatype;
  MinX: double;
  MaxX: double;
  MinY: double;
  MaxY: double;
  index: integer;
begin
  inherited;

  MinX := 0;
  MaxX := 0;
  MinY := 0;
  MaxY := 0;
  XList := TList<Double>.Create;
  YList := TList<Double>.Create;
  try
    Grid := frameStageDischarge.Grid;
    XList.Capacity := Grid.RowCount - 1;
    YList.Capacity := Grid.RowCount - 1;
    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      if TryStrToFloat(Grid.Cells[Ord(drcElevation), RowIndex], X)
        and TryStrToFloat(Grid.Cells[Ord(drcDischarge), RowIndex], Y) then
      begin
        if XList.Count = 0 then
        begin
          MinX := X;
          MaxX := X;
          MinY := Y;
          MaxY := Y;
        end
        else
        begin
          if X < MinX then
          begin
            MinX := X;
          end
          else if X > MaxX then
          begin
            MaxX := X;
          end;
          if Y < MinY then
          begin
            MinY := Y;
          end
          else if Y > MaxY then
          begin
            MaxY := Y;
          end;
        end;
        XList.Add(X);
        YList.Add(Y);
      end;
    end;
    if XList.Count > 0 then
    begin
      xysetdataarray(Data, XList.Count, 1);
      try
        xycleargraph(frameStageDischarge.pbPlot,clWhite,clBlack,1);

        xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

        xyxaxis(clBlack,MinX,MaxX,
          (MaxX-MinX)/10,0,'Elevation',1,False,False,True, 2);

        xyyaxis(clBlack,MinY,MaxY,
          (MaxY-MinY)/10,0,'Discharge',5,False,False,True, 2);

        for index := 0 to XList.Count - 1 do
        begin
          Data[index+1, 0] := XList[index];
          Data[index+1, 1] := YList[index];
        end;

        xysymbol(1,0,0);
        xyplotarray(data,0,2);

        xyfinish;
      except on E: exception do
        begin
          ShowMessage(e.message);
          Exit;
        end;
      end;
    end;
  finally
    XList.Free;
    YList.Free;
  end;
end;

procedure TfrmSwrStructures.frameStageDischargesbAddClick(Sender: TObject);
begin
  inherited;
  frameStageDischarge.sbAddClick(Sender);
//  FStructure.Table.Add;
end;

procedure TfrmSwrStructures.frameStageDischargesbDeleteClick(Sender: TObject);
begin
  inherited;
  if frameStageDischarge.Grid.SelectedRow >= 1 then
  begin
    FStructure.Table.Delete(frameStageDischarge.Grid.SelectedRow-1)
  end;
  frameStageDischarge.sbDeleteClick(Sender);

end;

procedure TfrmSwrStructures.frameStageDischargesbInsertClick(Sender: TObject);
begin
  inherited;
  if frameStageDischarge.Grid.SelectedRow >= 1 then
  begin
    FStructure.Table.Insert(frameStageDischarge.Grid.SelectedRow-1)
  end;
  frameStageDischarge.sbInsertClick(Sender);

end;

procedure TfrmSwrStructures.frameStageDischargeseNumberChange(Sender: TObject);
begin
  inherited;
  frameStageDischarge.seNumberChange(Sender);
  Assert(FStructure <> nil);
  FStructure.Table.Count := frameStageDischarge.seNumber.AsInteger;
end;

procedure TfrmSwrStructures.frameTimingGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ATime: double;
  StructureIndex: Integer;
  AStucture: TStructure;
  TimeItem: TStructureTimeItem;
  ColIndex: Integer;
begin
  inherited;
  UpdateNextTimeCell(ACol, ARow);
  if not FGettingData and (ACol in [0,1]) and (ARow >=1) then
  begin
    begin
      for ColIndex := 0 to 1 do
      begin
        if TryStrToFloat(frameTiming.Grid.Cells[ColIndex, ARow], ATime) then
        begin
          for StructureIndex := 0 to FStructures.Count - 1 do
          begin
            AStucture := FStructures[StructureIndex];
            while AStucture.Times.Count <= ARow-1 do
            begin
              AStucture.Times.Add;
            end;
            TimeItem := AStucture.Times[ARow-1];
            case ColIndex of
              0:
                begin
                  TimeItem.StartTime := ATime;
                end;
              1:
                begin
                  TimeItem.EndTime := ATime;
                end;
              else
                Assert(False);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmSwrStructures.frameTimingGridStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  AStructure: TStructure;
  ATimeItem: TStructureTimeItem;
begin
  inherited;
  if (ARow >= 1) and (ACol >= 2) and not FGettingData and (FStructures <> nil) then
  begin
    AStructure := FStructures[ACol-2];
    ATimeItem := AStructure.Times[ARow-1];
    ATimeItem.Used := Value = cbChecked;
  end;

end;

procedure TfrmSwrStructures.frameTimingsbDeleteClick(Sender: TObject);
var
  StructureIndex: Integer;
begin
  inherited;
  frameTiming.sbDeleteClick(Sender);
  if frameTiming.Grid.SelectedRow >= 1 then
  begin
    for StructureIndex := 0 to FStructures.Count - 1 do
    begin
      FStructures[StructureIndex].Times.Delete(frameTiming.Grid.SelectedRow-1)
    end;
  end;

end;

procedure TfrmSwrStructures.frameTimingsbInsertClick(Sender: TObject);
var
  StructureIndex: Integer;
begin
  inherited;
  frameTiming.sbInsertClick(Sender);
  if frameTiming.Grid.SelectedRow >= 1 then
  begin
    for StructureIndex := 0 to FStructures.Count - 1 do
    begin
      FStructures[StructureIndex].Times.Insert(frameTiming.Grid.SelectedRow-1)
    end;
  end;

end;

procedure TfrmSwrStructures.frameTimingseNumberChange(Sender: TObject);
var
  StructureIndex: Integer;
  TimeCount: integer;
begin
  inherited;
  frameTiming.seNumberChange(Sender);
  TimeCount := frameTiming.seNumber.asInteger;
  for StructureIndex := 0 to FStructures.Count - 1 do
  begin
    FStructures[StructureIndex].Times.Count := TimeCount;
  end;
end;

procedure TfrmSwrStructures.GetData;
var
  StructureIndex: Integer;
  AStructure: TStructure;
  Grid: TRbwDataGrid4;
  Dummy: Boolean;
  ATimeItem: TStructureTimeItem;
  TimeIndex: Integer;
  AColumn: TRbwColumn4;
begin
  frameTiming.FirstCheckColumn := 2;
  TGridCrack(rdgCulvert).HideEdit;
  frameTiming.FirstCheckColumn := 2;

  frameMain.comboType.Items.Assign(
    frameMain.Grid.Columns[Ord(mcStructureType)].PickList);
  frameMain.comboRestrictions.Items.Assign(
    frameMain.Grid.Columns[Ord(mcRestrictions)].PickList);
  frameMain.comboInitialValueMethod.Items.Assign(
    frameMain.Grid.Columns[Ord(mcInitialValueMethod)].PickList);
  frameMain.comboInitialValueTabFile.Items.Assign(
    frameMain.Grid.Columns[Ord(mcInitialValueTabFile)].PickList);
  if frameMain.comboInitialValueTabFile.Items.Count = 0 then
  begin
    frameMain.lblInitialValueTabFile.Caption := StrSTRVAL;
  end;

  frameMain.Grid.BeginUpdate;
  rdgCulvert.BeginUpdate;
  rdgPumpWeir.BeginUpdate;
  rdgSmoothing.BeginUpdate;

  FGettingData := True;
  try
    FStructures.Assign(frmGoPhast.PhastModel.SwrStructures);
    if AddNewStructure then
    begin
      AStructure := FStructures.Add;
      AStructure.Reach := Reach;
      AStructure.Name := 'Struct' + IntToStr(FStructures.Count);
      AStructure.StructureType := sstNone;
    end;
    frameMain.seNumber.AsInteger := FStructures.Count;
    frameMain.seNumber.OnChange(nil);

    frameTiming.Grid.ColCount := FStructures.Count + 2;
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(
      frameTiming.Grid, 0);
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(
      frameTiming.Grid, 1);

    Grid := frameMain.Grid;
    for StructureIndex := 0 to FStructures.Count - 1 do
    begin
      AStructure := FStructures[StructureIndex];

      Grid.Cells[Ord(mcName), StructureIndex+1] := AStructure.Name;
      Grid.IntegerValue[Ord(mcReach), StructureIndex+1] := AStructure.Reach;
      Grid.IntegerValue[Ord(mcConnectedReach), StructureIndex+1] := AStructure.ConnectedReach;
      Grid.ItemIndex[Ord(mcRestrictions), StructureIndex+1] := Ord(AStructure.StructureRestrictions);
      Grid.ItemIndex[Ord(mcStructureType), StructureIndex+1] := Ord(AStructure.StructureType);
      Grid.RealValue[Ord(mcWeirDischarge), StructureIndex+1] := AStructure.WeirDischargeCoefficient;
      Grid.RealValue[Ord(mcOrificeDischarge), StructureIndex+1] := AStructure.OrificeDischargeCoefficient;
      Grid.RealValue[Ord(mcSubmergenceExponent), StructureIndex+1] := AStructure.SubmergenceExponent;
      Grid.RealValue[Ord(mcInvertElevation), StructureIndex+1] := AStructure.InvertElevation;
      Grid.RealValue[Ord(mcStructureWidth), StructureIndex+1] := AStructure.Width;
      Grid.ItemIndex[Ord(mcInitialValueMethod), StructureIndex+1] := Ord(AStructure.InitialFlowRateMethod);
      Grid.RealValue[Ord(mcInitialValue), StructureIndex+1] := AStructure.InitialFlowRateOrGateOpening;
      Grid.Cells[Ord(mcInitialValueTabFile), StructureIndex+1] := AStructure.FullInitialFlowRateTabFile;
      Grid.IntegerValue[Ord(mcSfrSegment), StructureIndex+1] := AStructure.SfrSegment;
      Grid.IntegerValue[Ord(mcSfrReach), StructureIndex+1] := AStructure.SfrReach;

      rdgCulvert.Cells[Ord(ccName), StructureIndex+1] := AStructure.Name;
      rdgCulvert.RealValue[Ord(ccDownstreamInvertElevation), StructureIndex+1] := AStructure.DownstreamInvertElevation;
      rdgCulvert.ItemIndex[Ord(ccCulvertType), StructureIndex+1] := Ord(AStructure.CulvertType);
      rdgCulvert.RealValue[Ord(ccCulvertRise), StructureIndex+1] := AStructure.CulvertRise;
      rdgCulvert.Checked[Ord(ccSpecifyLengths), StructureIndex+1] := AStructure.SpecifyCulvertLengths;
      rdgCulvert.RealValue[Ord(ccCulvertLength), StructureIndex+1] := AStructure.CulvertLength;
      rdgCulvert.RealValue[Ord(ccDownstreamCulvertLength), StructureIndex+1] := AStructure.DownstreamCulvertLength;
      rdgCulvert.RealValue[Ord(ccRoughness), StructureIndex+1] := AStructure.CulvertRoughness;

      rdgPumpWeir.Cells[Ord(pwcName), StructureIndex+1] := AStructure.Name;
      rdgPumpWeir.ItemIndex[Ord(pwcControlType), StructureIndex+1] := Ord(AStructure.ControlType);
      rdgPumpWeir.IntegerValue[Ord(pwcControlReach), StructureIndex+1] := AStructure.ControlReach;
      rdgPumpWeir.IntegerValue[Ord(pwcConnectedReach), StructureIndex+1] := AStructure.ConnectedControlReach;
      rdgPumpWeir.ItemIndex[Ord(pwcControlOperated), StructureIndex+1] := Ord(AStructure.ControlOperated);
      rdgPumpWeir.ItemIndex[Ord(pwcCriticalMethod), StructureIndex+1] := Ord(AStructure.CriticalMethod);
      rdgPumpWeir.Cells[Ord(pwcCriticalTabFileName), StructureIndex+1] := AStructure.FullCriticalTabFileName;
      rdgPumpWeir.RealValue[Ord(pwcCriticalValue), StructureIndex+1] := AStructure.CriticalValue;
      rdgPumpWeir.RealValue[Ord(pwcControlOffsetCriterion), StructureIndex+1] := AStructure.ControlOffsetCriterion;
      rdgPumpWeir.RealValue[Ord(pwcStartingControlRate), StructureIndex+1] := AStructure.StartingControlRate;
      rdgPumpWeir.RealValue[Ord(pwcMaximumControlRate), StructureIndex+1] := AStructure.MaximumControlRate;
      rdgPumpWeir.Cells[Ord(pwcDischargeTabFile), StructureIndex+1] := AStructure.FullDischargeTabFile;

      rdgSmoothing.Cells[Ord(scName), StructureIndex+1] := AStructure.Name;
      rdgSmoothing.ItemIndex[Ord(scSmoothingMethod), StructureIndex+1] := Ord(AStructure.SmoothingMethod);
      rdgSmoothing.RealValue[Ord(scSmoothing), StructureIndex+1] := AStructure.SmoothingValue;

      AColumn := frameTiming.Grid.Columns[StructureIndex+2];
      AColumn.Format := rcf4Boolean;
      AColumn.WordWrapCaptions := True;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      frameTiming.Grid.Cells[StructureIndex+2, 0] := AStructure.Name;
      if StructureIndex = 0 then
      begin
        frameTiming.seNumber.AsInteger := AStructure.Times.Count;
      end
      else
      begin
        Assert(frameTiming.seNumber.AsInteger = AStructure.Times.Count);
      end;
      for TimeIndex := 0 to AStructure.Times.Count - 1 do
      begin
        ATimeItem := AStructure.Times[TimeIndex];
        if StructureIndex = 0 then
        begin
          frameTiming.Grid.RealValue[0,TimeIndex+1] := ATimeItem.StartTime;
          frameTiming.Grid.RealValue[1,TimeIndex+1] := ATimeItem.EndTime;
        end;
        frameTiming.Grid.Checked[StructureIndex+2,TimeIndex+1] := ATimeItem.Used;
      end;
    end;
  finally
    FGettingData := False;
    frameMain.Grid.EndUpdate;
    rdgCulvert.EndUpdate;
    rdgPumpWeir.EndUpdate;
    rdgSmoothing.EndUpdate;
  end;
  frameMainGridGridSelectCell(nil, 0, 1, Dummy);
  frameMain.Grid.TopRow := SelectedStructureIndex+1;
  frameMain.Grid.Row := SelectedStructureIndex+1;
  frameMainGridGridSelectCell(nil, 0, frameMain.Grid.Row, Dummy);
  frameTiming.LayoutMultiRowEditControls;
  frameMain.LayoutMultiRowEditControls
end;

procedure TfrmSwrStructures.LayoutCulvertControls;
const
  CheckBoxHorizontalOffset = 5;
var
  ACol: Integer;
begin
  LayoutControls(rdgCulvert, comboCulvertType, nil, Ord(ccCulvertType));
  LayoutControls(rdgCulvert, cbCulvert, nil, Ord(ccSpecifyLengths),
    CheckBoxHorizontalOffset);
  ACol := Max(rdgCulvert.LeftCol, Ord(ccDownstreamInvertElevation));
  if ACol = Ord(ccCulvertType) then
  begin
    ACol := Ord(ccCulvertRise)
  end
  else if ACol = Ord(ccSpecifyLengths) then
  begin
    ACol := Ord(ccCulvertLength)
  end;
  LayoutControls(rdgCulvert, rdeCulvertValue, nil, ACol);
end;

procedure TfrmSwrStructures.LayoutPumpWeirControls;
var
  ACol: integer;
begin
  LayoutControls(rdgPumpWeir, comboPumpWeirControlType, nil, Ord(pwcControlType));
  LayoutControls(rdgPumpWeir, comboPumpWeirControlOperated, nil, Ord(pwcControlOperated));
  LayoutControls(rdgPumpWeir, comboPumpWeirCriticalMethod, nil, Ord(pwcCriticalMethod));
  LayoutControls(rdgPumpWeir, comboPumpWeirCriticalTabFileName, nil, Ord(pwcCriticalTabFileName));
  LayoutControls(rdgPumpWeir, comboPumpWeirDischargeTabFile, nil, Ord(pwcDischargeTabFile));
  ACol := Max(rdgPumpWeir.LeftCol, Ord(pwcCriticalValue));
  ACol := Min(ACol, Ord(pwcMaximumControlRate));
  LayoutControls(rdgPumpWeir, rdePumpWeirValue, nil, ACol);
end;

procedure TfrmSwrStructures.rdeCulvertValueChange(Sender: TObject);
var
  AColumnItem: TCulvertColumns;
  ColIndex: Integer;
  RowIndex: Integer;
  CanSelect: Boolean;
begin
  inherited;
  rdgCulvert.BeginUpdate;
  try
    for AColumnItem in [ccDownstreamInvertElevation, ccCulvertRise,
      ccCulvertLength, ccDownstreamCulvertLength, ccRoughness] do
    begin
      ColIndex := Ord(AColumnItem);
      for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount - 1 do
      begin
        if rdgCulvert.IsSelectedCell(ColIndex, RowIndex) then
        begin
          CanSelect := True;
          if Assigned(rdgCulvert.OnSelectCell) then
          begin
            rdgCulvert.OnSelectCell(rdgCulvert, ColIndex, RowIndex, CanSelect);
          end;
          if CanSelect then
          begin
            rdgCulvert.Cells[ColIndex, RowIndex] := rdeCulvertValue.Text;
          end;
        end;
      end;
    end;
  finally
    rdgCulvert.EndUpdate;
  end;

end;

procedure TfrmSwrStructures.rdePumpWeirValueChange(Sender: TObject);
begin
  inherited;
  SetSelectedPumpWeirValues(rdePumpWeirValue.Text, [pwcCriticalValue,
    pwcControlOffsetCriterion, pwcStartingControlRate, pwcMaximumControlRate]);
end;

procedure TfrmSwrStructures.rdgCulvertColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutCulvertControls;
end;

procedure TfrmSwrStructures.rdgCulvertHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutCulvertControls;
end;

procedure TfrmSwrStructures.rdgCulvertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  AColumnItem: TCulvertColumns;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount -1 do
  begin
    ShouldEnable := rdgCulvert.IsSelectedCell(Ord(ccCulvertType),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboCulvertType.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount -1 do
  begin
    ShouldEnable := rdgCulvert.IsSelectedCell(Ord(ccSpecifyLengths),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbCulvert.Enabled := ShouldEnable;

  ShouldEnable := False;
  for AColumnItem in [ccDownstreamInvertElevation, ccCulvertRise,
    ccCulvertLength, ccDownstreamCulvertLength, ccRoughness] do
  begin
    ColIndex := Ord(AColumnItem);
    for RowIndex := rdgCulvert.FixedRows to rdgCulvert.RowCount - 1 do
    begin
      ShouldEnable := rdgCulvert.IsSelectedCell(ColIndex,RowIndex);
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
  rdeCulvertValue.Enabled := ShouldEnable;
end;

procedure TfrmSwrStructures.rdgCulvertSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  StructureIndex: Integer;
  CulvertCol: TCulvertColumns;
  StructureType: TSwrStructureType;
  CulvertTypeIndex: Integer;
begin
  inherited;
  if (ACol >= frameMain.Grid.FixedCols) and (ARow >= frameMain.Grid.FixedCols) then
  begin
    StructureIndex := frameMain.Grid.ItemIndex[Ord(mcStructureType), ARow];
    CulvertCol := TCulvertColumns(ACol);
    case CulvertCol of
      ccName: ;
      ccDownstreamInvertElevation, ccCulvertType, ccRoughness:
        begin
          CanSelect := (StructureIndex >= 0) and
            (TSwrStructureType(StructureIndex) = sstCulvert);
        end;
      ccCulvertRise:
        begin
          CulvertTypeIndex := rdgCulvert.ItemIndex[Ord(ccCulvertType), ARow];
          CanSelect := (StructureIndex >= 0) and
            (TSwrStructureType(StructureIndex) = sstCulvert)
            and (CulvertTypeIndex >= 0)
            and (TCulvertType(CulvertTypeIndex) = ctRectangular);
        end;
      ccSpecifyLengths:
        begin
          CanSelect := (StructureIndex >= 0) and
            (TSwrStructureType(StructureIndex)
            in [sstUncontrolledZeroDepth, sstUncontrolledCriticalDepth]);
        end;
      ccCulvertLength:
        begin
          CanSelect := (StructureIndex >= 0);
          if CanSelect then
          begin
            StructureType := TSwrStructureType(StructureIndex);
            case StructureType of
              sstUncontrolledZeroDepth, sstUncontrolledCriticalDepth:
                begin
                  CanSelect := rdgCulvert.Checked[Ord(ccSpecifyLengths), ARow];
                end;
              sstCulvert, sstManning:
                begin
                  CanSelect := True;
                end;
              sstNone, sstSpecifiedElevation, sstPump, sstStageDishargeTable,
                sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
                sstGatedSpillway, sstSpillEquation, sstSfrInflow, sstOverbankFlow:
                begin
                  CanSelect := False;
                end;
              else
                Assert(False);
            end;
          end;
        end;
      ccDownstreamCulvertLength:
        begin
          CanSelect := (StructureIndex >= 0) and
            (TSwrStructureType(StructureIndex)
            in [sstUncontrolledZeroDepth, sstUncontrolledCriticalDepth])
            and rdgCulvert.Checked[Ord(ccSpecifyLengths), ARow];
        end;
      else
        Assert(False);
    end;

    if not rdgCulvert.Drawing then
    begin
      frameMain.Grid.Row := rdgCulvert.SelectedRow;
    end;
  end;
end;

procedure TfrmSwrStructures.rdgCulvertSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AStructure: TStructure;
  CulverCol: TCulvertColumns;
  ItemIndex: Integer;
begin
  inherited;
  if (ARow >= 1) and (ACol >= 0) and not FGettingData and (FStructures <> nil) then
  begin
    while ARow > FStructures.Count do
    begin
      FStructures.Add;
    end;
    frameMain.seNumber.AsInteger := FStructures.Count;
    AStructure := FStructures[ARow-1];
    CulverCol := TCulvertColumns(ACol);
    case CulverCol of
      ccName, ccSpecifyLengths: ;
      ccDownstreamInvertElevation:
        begin
          AStructure.DownstreamInvertElevation := rdgCulvert.RealValueDefault[ACol,ARow,0];
        end;
      ccCulvertType:
        begin
          ItemIndex := rdgCulvert.ItemIndex[ACol,ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.CulvertType := TCulvertType(ItemIndex);
          end;
          rdgCulvert.Invalidate;
        end;
      ccCulvertRise:
        begin
          AStructure.CulvertRise := rdgCulvert.RealValueDefault[ACol,ARow,0];
        end;
      ccCulvertLength:
        begin
          AStructure.CulvertLength := rdgCulvert.RealValueDefault[ACol,ARow,0];
        end;
      ccDownstreamCulvertLength:
        begin
          AStructure.DownstreamCulvertLength := rdgCulvert.RealValueDefault[ACol,ARow,0];
        end;
      ccRoughness:
        begin
          AStructure.CulvertRoughness := rdgCulvert.RealValueDefault[ACol,ARow,0];
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TfrmSwrStructures.rdgCulvertStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  AStructure: TStructure;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(ccSpecifyLengths)) and not FGettingData then
  begin
    AStructure := FStructures[ARow-1];
    AStructure.SpecifyCulvertLengths := rdgCulvert.Checked[ACol,ARow];
  end;
  rdgCulvert.Invalidate;
end;

procedure TfrmSwrStructures.rdgPumpWeirColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutPumpWeirControls
end;

procedure TfrmSwrStructures.rdgPumpWeirHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutPumpWeirControls
end;

procedure TfrmSwrStructures.rdgPumpWeirMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  LayoutPumpWeirControls;
end;

procedure TfrmSwrStructures.rdgPumpWeirMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnablePumpWeirMultiControl([pwcControlType], comboPumpWeirControlType);
  EnablePumpWeirMultiControl([pwcControlOperated], comboPumpWeirControlOperated);
  EnablePumpWeirMultiControl([pwcCriticalMethod], comboPumpWeirCriticalMethod);
  EnablePumpWeirMultiControl([pwcCriticalTabFileName], comboPumpWeirCriticalTabFileName);
  EnablePumpWeirMultiControl([pwcDischargeTabFile], comboPumpWeirDischargeTabFile);
  EnablePumpWeirMultiControl([pwcCriticalValue, pwcControlOffsetCriterion,
    pwcStartingControlRate, pwcMaximumControlRate], rdePumpWeirValue);
end;

procedure TfrmSwrStructures.rdgPumpWeirSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  AStructure: TStructure;
  PumpWeirCol: TPumpWeirColumns;
begin
  inherited;
  if (ACol >= 0) and (ARow >= 1) and not FGettingData and (FStructures <> nil) then
  begin
    while ARow > FStructures.Count do
    begin
      FStructures.Add;
    end;
    frameMain.seNumber.AsInteger := FStructures.Count;
    AStructure := FStructures[ARow-1];
    PumpWeirCol := TPumpWeirColumns(ACol);
    case PumpWeirCol of
      pwcName:
        begin
          CanSelect := False;
        end;
      pwcControlType, pwcCriticalMethod, pwcMaximumControlRate:
        begin
          CanSelect := AStructure.StructureType in [sstSpecifiedElevation,
            sstPump, sstMoveableWeir, sstGatedSpillway, sstSpillEquation];
        end;
      pwcControlReach, pwcControlOperated,
        pwcControlOffsetCriterion, pwcStartingControlRate:
        begin
          CanSelect := AStructure.StructureType in
            [sstPump, sstMoveableWeir, sstGatedSpillway, sstSpillEquation];
        end;
      pwcConnectedReach:
        begin
          CanSelect := (AStructure.StructureType in
            [sstPump, sstMoveableWeir, sstGatedSpillway, sstSpillEquation])
            and (rdgPumpWeir.ItemIndex[Ord(pwcControlType), ARow] = 1);
        end;
      pwcCriticalTabFileName:
        begin
          CanSelect := (AStructure.StructureType in [sstSpecifiedElevation,
            sstPump, sstMoveableWeir, sstGatedSpillway, sstSpillEquation])
            and (rdgPumpWeir.ItemIndex[Ord(pwcCriticalMethod), ARow] = 1);
        end;
      pwcCriticalValue, pwcDischargeTabFile:
        begin
          CanSelect := (AStructure.StructureType in [sstSpecifiedElevation,
            sstPump, sstMoveableWeir, sstGatedSpillway, sstSpillEquation])
            and (rdgPumpWeir.ItemIndex[Ord(pwcCriticalMethod), ARow] = 0)
        end;
    end;
  end;
end;

procedure TfrmSwrStructures.rdgPumpWeirSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AStructure: TStructure;
  PumpWeirCol: TPumpWeirColumns;
  ItemIndex: Integer;
begin
  inherited;

  if (ARow >= 1) and (ACol >= 0) and not FGettingData and (FStructures <> nil) then
  begin
    while ARow > FStructures.Count do
    begin
      FStructures.Add;
    end;
    frameMain.seNumber.AsInteger := FStructures.Count;
    AStructure := FStructures[ARow-1];
    PumpWeirCol := TPumpWeirColumns(ACol);
    case PumpWeirCol of
      pwcName: ;
      pwcControlType:
        begin
          ItemIndex := rdgPumpWeir.ItemIndex[Ord(pwcControlType), ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.ControlType := TControlType(ItemIndex);
          end;
        end;
      pwcControlReach:
        begin
          AStructure.ControlReach := rdgPumpWeir.IntegerValueDefault[Ord(pwcControlReach), ARow, 1];
        end;
      pwcConnectedReach:
        begin
          AStructure.ConnectedControlReach := rdgPumpWeir.IntegerValueDefault[Ord(pwcConnectedReach), ARow, 1];
        end;
      pwcControlOperated:
        begin
          ItemIndex := rdgPumpWeir.ItemIndex[Ord(pwcControlOperated), ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.ControlOperated := TControlOperated(rdgPumpWeir.ItemIndex[Ord(pwcControlOperated), ARow]);
          end;
        end;
      pwcCriticalMethod:
        begin
          ItemIndex := rdgPumpWeir.ItemIndex[Ord(pwcCriticalMethod), ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.CriticalMethod := TSpecificationMethod(rdgPumpWeir.ItemIndex[Ord(pwcCriticalMethod), ARow]);
          end;
        end;
      pwcCriticalTabFileName:
        begin
          AStructure.FullCriticalTabFileName := rdgPumpWeir.Cells[Ord(pwcCriticalTabFileName), ARow];
        end;
      pwcCriticalValue:
        begin
          AStructure.CriticalValue := rdgPumpWeir.RealValueDefault[Ord(pwcCriticalValue), ARow, 0];
        end;
      pwcControlOffsetCriterion:
        begin
          AStructure.ControlOffsetCriterion := rdgPumpWeir.RealValueDefault[Ord(pwcControlOffsetCriterion), ARow, 0];
        end;
      pwcStartingControlRate:
        begin
          AStructure.StartingControlRate := rdgPumpWeir.RealValueDefault[Ord(pwcStartingControlRate), ARow, 0];
        end;
      pwcMaximumControlRate:
        begin
          AStructure.MaximumControlRate := rdgPumpWeir.RealValueDefault[Ord(pwcMaximumControlRate), ARow, 0];
        end;
      pwcDischargeTabFile:
        begin
          AStructure.FullDischargeTabFile := rdgPumpWeir.Cells[Ord(pwcDischargeTabFile), ARow];
        end;
      else
        Assert(False);
    end;
    rdgPumpWeir.Invalidate;
  end;
end;

procedure TfrmSwrStructures.rdgSmoothingSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  StructureItemIndex: integer;
begin
  inherited;
  CanSelect := False;
  StructureItemIndex := frameMain.Grid.ItemIndex[Ord(mcStructureType),ARow];
  if (StructureItemIndex >= 0)
    and (TSwrStructureType(StructureItemIndex) = sstOverbankFlow) then
  begin
    CanSelect := True;
  end;
end;

procedure TfrmSwrStructures.rdgSmoothingSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AStructure: TStructure;
  AValue: double;
  ItemIndex: Integer;
begin
  inherited;
  if (ACol > 0) and (ARow >= 1) and not FGettingData and (FStructures <> nil)
    and not FDeletingStructure then
  begin
    AStructure := FStructures[ARow-1];
    case TSmoothingColumns(ACol) of
      scName: Assert(False);
      scSmoothingMethod:
        begin
          ItemIndex := rdgSmoothing.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AStructure.SmoothingMethod := TSmoothingMethod(ItemIndex);
          end;
        end;
      scSmoothing:
        begin
          if TryStrToFloat(rdgSmoothing.Cells[ACol, ARow], AValue) then
          begin
            AStructure.SmoothingValue := AValue;
          end;
        end;
    end;
  end;
end;

procedure TfrmSwrStructures.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoSwrStructures.Create(FStructures));
end;

procedure TfrmSwrStructures.SetSelectedPumpWeirValues(const AValue: string;
  Columns: TPumpWeirColumnSet);
var
  AColumnItem: TPumpWeirColumns;
  RowIndex: Integer;
  ColIndex: Integer;
  CanSelect: Boolean;
begin
  rdgPumpWeir.BeginUpdate;
  try
    for AColumnItem in Columns do
    begin
      ColIndex := Ord(AColumnItem);
      for RowIndex := rdgPumpWeir.FixedRows to rdgPumpWeir.RowCount - 1 do
      begin
        if rdgPumpWeir.IsSelectedCell(ColIndex, RowIndex) then
        begin
          CanSelect := True;
          if Assigned(rdgPumpWeir.OnSelectCell) then
          begin
            rdgPumpWeir.OnSelectCell(rdgPumpWeir, ColIndex, RowIndex, CanSelect);
          end;
          if CanSelect then
          begin
            rdgPumpWeir.Cells[ColIndex, RowIndex] := AValue;
            if Assigned(rdgPumpWeir.OnSetEditText) then
            begin
              rdgPumpWeir.OnSetEditText(rdgPumpWeir, ColIndex, RowIndex, AValue);
            end;
          end;
        end;
      end;
    end;
  finally
    rdgPumpWeir.EndUpdate;
  end;
end;

procedure TfrmSwrStructures.UpdateNextTimeCell(ACol, ARow: Integer);
//var
//  SelectIndex: Integer;
begin
  if not FGettingData then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(frameTiming.Grid, ACol, ARow);
  end;
//  if not FGettingData and (ARow >= frameTiming.Grid.FixedRows)
//    and (ACol in [0, 1])then
//  begin
//    SelectIndex := frameTiming.Grid.ItemIndex[ACol, ARow];
//    if SelectIndex >= 0 then
//    begin
//      if (ACol = 0) then
//      begin
//        if frameTiming.Grid.Cells[1, ARow] = '' then
//        begin
//          frameTiming.Grid.ItemIndex[1, ARow] := SelectIndex;
//        end;
//      end
//      else if (ACol = 1) then
//      begin
//        if (ARow + 1 < frameTiming.Grid.RowCount) and
//          (frameTiming.Grid.Cells[0, ARow + 1] = '') then
//        begin
//          if SelectIndex + 1 < frameTiming.Grid.Columns[0].PickList.Count then
//          begin
//            frameTiming.Grid.ItemIndex[0, ARow + 1] := SelectIndex + 1;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

{ TUndoSwrStructures }

constructor TUndoSwrStructures.Create(var Structures: TStructureCollection);
begin
  FOldStructures := TStructureCollection.Create(nil);
  FOldStructures.Assign(frmGoPhast.PhastModel.SwrStructures);

  FNewStructures := Structures;
  Structures := nil;
end;

function TUndoSwrStructures.Description: string;
begin
  result := 'change SWR structures';
end;

destructor TUndoSwrStructures.Destroy;
begin
  FOldStructures.Free;
  FNewStructures.Free;
  inherited;
end;

procedure TUndoSwrStructures.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SwrStructures := FNewStructures;
end;

procedure TUndoSwrStructures.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SwrStructures := FOldStructures;
end;

end.
