unit frameScreenObjectMNW2Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, RbwEdit, ArgusDataEntry, JvExStdCtrls,
  JvCombobox, JvListComb, ComCtrls, Mask, JvExMask, JvSpin, Grids, RbwDataGrid4,
  ModflowMnw2Unit, UndoItemsScreenObjects, JvToolEdit, JvExComCtrls, JvComCtrls,
  frameLocationMethodUnit, frameScreenObjectUnit, GrayTabs, frameGridUnit,
  System.Generics.Collections, PestObsUnit, framePestObsUnit;

type
  // This type is used in this unit and in frmImportShapefileUnit.
  TMnwTimeColumns = (mtcStartTime, mtcEndTime, mtcPumpingRate, mtcMultiplier,
    mtcLimitingWaterLevel, mtcLimitMethod, mtcMinRate, mtcMaxRate);

  TMnwLiftTableColumns = (mltcLift, mltcQ);

  TVerticalScreenColumns = (vsZTop, vsZBot, vsRw, vsRSkin, vsKSkin, vsB, vsC, vsP, vsCWC);

  TMnwiObsColumns = (mocName, mocType, mocTime, mocValue, mocWeight, mocComment);
  TMnwiObsCompColumns = (moccName, moccObs1, moccObs2, moccValue, moccWeight, moccComment);

  TframeScreenObjectMNW2 = class(TframeScreenObject)
    pnlCaption: TPanel;
    pcMnw2: TPageControl;
    tabBasic: TTabSheet;
    lblWellId: TLabel;
    edWellId: TRbwEdit;
    lblLossType: TLabel;
    comboLossType: TJvImageComboBox;
    cbConstrainPumping: TCheckBox;
    cbPartialPenetrationFlag: TCheckBox;
    cbPumpCap: TCheckBox;
    tabLossControls: TTabSheet;
    lblWellRadius: TLabel;
    lblSkinRadius: TLabel;
    lblBCoefficient: TLabel;
    lblCCoefficient: TLabel;
    lblPCoefficient: TLabel;
    lblCellToWellConductance: TLabel;
    cbSpecifyPump: TCheckBox;
    tabDischargeAdjustment: TTabSheet;
    rdeReferenceHead: TRbwDataEntry;
    lblReferenceHead: TLabel;
    lblLiftQ0: TLabel;
    rdeLiftQ0: TRbwDataEntry;
    lblLiftQMax: TLabel;
    rdeLiftQMax: TRbwDataEntry;
    lblWellTolerance: TLabel;
    rdeWellTolerance: TRbwDataEntry;
    rdgLiftTable: TRbwDataGrid4;
    seLiftTableRows: TJvSpinEdit;
    btnInsertLift: TButton;
    btnDeleteLift: TButton;
    lflLiftTableRows: TLabel;
    lblLiftTable: TLabel;
    tabPumpingRate: TTabSheet;
    rdgTimeTable: TRbwDataGrid4;
    Panel1: TPanel;
    seTimeTableRows: TJvSpinEdit;
    lblTimeTableRows: TLabel;
    btnInsertTime: TButton;
    btnDeleteTime: TButton;
    Panel2: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    lblKSkin: TLabel;
    lblPartialPenetration: TLabel;
    comboQCUT: TJvImageComboBox;
    lblQCUT: TLabel;
    edPartialPenetration: TJvComboEdit;
    edWellRadius: TJvComboEdit;
    edSkinRadius: TJvComboEdit;
    edKSkin: TJvComboEdit;
    edBCoefficient: TJvComboEdit;
    edCCoefficient: TJvComboEdit;
    edPCoefficient: TJvComboEdit;
    edCellToWellConductance: TJvComboEdit;
    framePumpLocationMethod: TframeLocationMethod;
    lblZPump: TLabel;
    rdeZPump: TRbwDataEntry;
    gbMNWI: TGroupBox;
    cbSaveExternal: TCheckBox;
    cbSaveInternal: TCheckBox;
    tabWellScreens: TTabSheet;
    Panel3: TPanel;
    Label1: TLabel;
    seVerticalScreens: TJvSpinEdit;
    btnInsertVertialScreen: TButton;
    btnDeleteVertialScreen: TButton;
    rdgVerticalScreens: TRbwDataGrid4;
    Panel4: TPanel;
    Label2: TLabel;
    rdeWellScreenFormula: TRbwDataEntry;
    lblWellScreenFormula: TLabel;
    cbSaveMnwiBasic: TCheckBox;
    tabObservations: TTabSheet;
    framePestObsMnw2: TframePestObs;
    procedure edWellIdChange(Sender: TObject);
    procedure seLiftTableRowsChange(Sender: TObject);
    procedure rdgTimeTableEndUpdate(Sender: TObject);
    procedure seTimeTableRowsChange(Sender: TObject);
    procedure cbSpecifyPumpClick(Sender: TObject);
    procedure comboLossTypeChange(Sender: TObject);
    procedure cbPartialPenetrationFlagClick(Sender: TObject);
    procedure rdgTimeTableSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cbPumpCapClick(Sender: TObject);
    procedure rdgLiftTableEndUpdate(Sender: TObject);
    procedure btnInsertLiftClick(Sender: TObject);
    procedure btnDeleteLiftClick(Sender: TObject);
    procedure btnInsertTimeClick(Sender: TObject);
    procedure btnDeleteTimeClick(Sender: TObject);
    procedure cbConstrainPumpingClick(Sender: TObject);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgTimeTableColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgTimeTableHorizontalScroll(Sender: TObject);
    procedure rdgTimeTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgTimeTableMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboQCUTChange(Sender: TObject);
    procedure cbSaveExternalClick(Sender: TObject);
    procedure cbSaveInternalClick(Sender: TObject);
    procedure framePumpLocationMethodcomboLocationChoiceChange(Sender: TObject);
    procedure edPartialPenetrationChange(Sender: TObject);
    procedure edWellRadiusChange(Sender: TObject);
    procedure rdgVerticalScreensSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seVerticalScreensChange(Sender: TObject);
    procedure btnInsertVertialScreenClick(Sender: TObject);
    procedure btnDeleteVertialScreenClick(Sender: TObject);
    procedure rdgVerticalScreensEndUpdate(Sender: TObject);
    procedure rdeWellScreenFormulaChange(Sender: TObject);
    procedure rdgVerticalScreensColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgVerticalScreensHorizontalScroll(Sender: TObject);
    procedure rdgVerticalScreensMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbSaveMnwiBasicClick(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FVerticalWell: TCheckBoxState;
    FFirstBoundary: TMnw2Boundary;
    FOnCheckPestCell: TSelectCellEvent;
    procedure Changed;
    procedure SetVerticalWell(const Value: TCheckBoxState);
    procedure EnablePartialPenetration;
    procedure InitializeControls;
    procedure EnableDeleteLiftButton;
    procedure EnableDeleteTimeButton;
    procedure AssignFirstItem(LocalList: TList);
    procedure UpdateCheckBox(NewValue: Boolean; CheckBox: TCheckBox);
    procedure UpdateFormulaEdit(NewFormula: string; Control: TJvComboEdit;
      ShouldCheck: Boolean); overload;
    procedure UpdateFormulaEdit(NewFormula: string; Control: TRbwDataEntry;
      ShouldCheck: Boolean); overload;
    procedure SetEdForFirstItem(NewFormula: string; Control: TJvComboEdit;
      ShouldSet: boolean); overload;
    procedure SetEdForFirstItem(NewFormula: string; Control: TRbwDataEntry;
      ShouldSet: boolean); overload;
    procedure SetTimeGridCellForFirstItem(Column: TMnwTimeColumns;
      SetValue: Boolean; TimeIndex: Integer; TimeItem: TMnw2TimeItem;
      Value: string);
    procedure UpdateTimeGridCell(Value: string; Column: TMnwTimeColumns;
      TimeIndex: Integer; ShouldSet: boolean; AnObject: TObject);
    procedure UpdateSpatialValueControls(Boundary: TMnw2Boundary);
    procedure UpdateLiftTable(Boundary: TMnw2Boundary);
    procedure UpdateTimeTable(Boundary: TMnw2Boundary);
    procedure LayoutMultiCellEditControlsForStressPeriods;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetChanging(const Value: Boolean);
    procedure EnableVerticalScreenButton;
    procedure UpdateVerticalScreenTable(Boundary: TMnw2Boundary);
    procedure LayoutMultiCellEditControlsForWellScreens;
    property VerticalWell: TCheckBoxState read FVerticalWell
      write SetVerticalWell;
    property Changing: Boolean read FChanging write SetChanging;
    procedure UpdateVerticalScreenGridCell(ScreenIndex: Integer; VerticalScreen: TVerticalScreen; AValue: string; Column: TVerticalScreenColumns);
//    procedure UpdatedSelectedCell;
    { Private declarations }
  public
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    constructor Create(AOwner: TComponent); override;
    property OnCheckPestCell: TSelectCellEvent read FOnCheckPestCell
      write FOnCheckPestCell;
    { Public declarations }
  end;

resourcestring
  StrDesiredPumpingRate = 'Desired pumping rate (Qdes)';
  StrHeadCapacityMultip = 'Head capacity multiplier (CapMult)';
  StrLimitingWaterLevel = 'Limiting water level (Hlim)';
  StrPumpingLimitMethod = 'Pumping limit method (QCUT)';
  StrDeactivationPumping = 'Deactivation pumping rate (Qfrcmn)';
  StrReactivationPumping = 'Reactivation pumping rate (Qfrcmx)';

implementation

uses
  GoPhastTypes, ScreenObjectUnit, frmGoPhastUnit, ModflowTimeUnit,
  frmCustomGoPhastUnit, ModflowMNW2_WriterUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrWellScreenTopZTo = 'Well screen top (ZTop)';
  StrWellScreenBottom = 'Well screen bottom (ZBottom)';
  StrNoMNW2WELLIDWasD = 'No MNW2 WELLID was defined for the following object' +
  's.';
  StrObservationName = 'Observation Name';
  StrObservationType = 'Observation Type';
  StrObservationTime = 'Observation Time';
  StrObservationValue = 'Observation Value';
  StrObservationWeight = 'Observation Weight';
  StrComment = 'Comment';
  StrFirstObservation = 'First Observation';
  StrSecondObservation = 'Second Observation';
  StrLift = 'Lift';
  StrQ = 'Q';

{$R *.dfm}

{ TframeScreenObjectMNW2 }

procedure TframeScreenObjectMNW2.btnDeleteLiftClick(Sender: TObject);
begin
  if rdgLiftTable.Row < 1 then
  begin
    Exit;
  end;
  rdgLiftTable.DeleteRow(rdgLiftTable.Row);
  seLiftTableRows.AsInteger := rdgLiftTable.RowCount - 1;
  EnableDeleteLiftButton;
  Changed;
end;

procedure TframeScreenObjectMNW2.btnDeleteTimeClick(Sender: TObject);
begin
  if rdgTimeTable.Row < 1 + PestRowOffset then
  begin
    Exit;
  end;
  rdgTimeTable.DeleteRow(rdgTimeTable.Row);
  seTimeTableRows.AsInteger := rdgTimeTable.RowCount - 1- PestRowOffset;
  EnableDeleteTimeButton;
  Changed;
end;

procedure TframeScreenObjectMNW2.btnDeleteVertialScreenClick(Sender: TObject);
begin
  if rdgVerticalScreens.Row < 1 then
  begin
    Exit;
  end;
  if rdgVerticalScreens.RowCount > 2 then
  begin
    rdgVerticalScreens.DeleteRow(rdgVerticalScreens.Row);
  end;
  seVerticalScreens.AsInteger := seVerticalScreens.AsInteger -1;
  EnableVerticalScreenButton;
  Changed;
end;

procedure TframeScreenObjectMNW2.btnInsertLiftClick(Sender: TObject);
begin
  if rdgLiftTable.Row < 1 then
  begin
    Exit;
  end;
  rdgLiftTable.InsertRow(rdgLiftTable.Row);
  seLiftTableRows.AsInteger := rdgLiftTable.RowCount - 1;
  Changed;
end;

procedure TframeScreenObjectMNW2.btnInsertTimeClick(Sender: TObject);
begin
  if rdgTimeTable.Row < 1 + PestRowOffset then
  begin
    Exit;
  end;
  rdgTimeTable.InsertRow(rdgTimeTable.Row);
  seTimeTableRows.AsInteger := rdgTimeTable.RowCount - 1 - PestRowOffset;
  Changed;
end;

procedure TframeScreenObjectMNW2.btnInsertVertialScreenClick(Sender: TObject);
begin
  if rdgVerticalScreens.Row < 1 then
  begin
    Exit;
  end;
  rdgVerticalScreens.InsertRow(rdgVerticalScreens.Row);
  seVerticalScreens.AsInteger := rdgVerticalScreens.RowCount - 1;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbConstrainPumpingClick(Sender: TObject);
begin
  cbConstrainPumping.AllowGrayed := cbConstrainPumping.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbPartialPenetrationFlagClick(Sender: TObject);
begin
  EnablePartialPenetration;
  cbPartialPenetrationFlag.AllowGrayed :=
    cbPartialPenetrationFlag.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbPumpCapClick(Sender: TObject);
begin
  tabDischargeAdjustment.TabVisible := (cbPumpCap.State <> cbUnchecked);
  cbPumpCap.AllowGrayed := cbPumpCap.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbSaveExternalClick(Sender: TObject);
begin
  cbSaveExternal.AllowGrayed := cbSaveExternal.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbSaveInternalClick(Sender: TObject);
begin
  cbSaveInternal.AllowGrayed := cbSaveInternal.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbSaveMnwiBasicClick(Sender: TObject);
begin
  cbSaveMnwiBasic.AllowGrayed := cbSaveMnwiBasic.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.cbSpecifyPumpClick(Sender: TObject);
begin
  rdeZPump.Enabled := (cbSpecifyPump.State <> cbUnchecked)
    and (VerticalWell <> cbUnchecked);
  framePumpLocationMethod.Enabled := (cbSpecifyPump.State <> cbUnchecked)
    and (VerticalWell <> cbChecked);
  cbSpecifyPump.AllowGrayed := cbSpecifyPump.State = cbGrayed;
  Changed;
end;

procedure TframeScreenObjectMNW2.Changed;
begin
  if Assigned(OnChange) then
  begin
    if Changing then
      Exit;
    Changing := True;
    try
      OnChange(self);
    finally
      Changing := False;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.comboLossTypeChange(Sender: TObject);
var
  LossTypes: Set of TMnwLossType;
begin
  if comboLossType.ItemIndex < 0 then
  begin
    LossTypes := [mltNone, mltThiem, mltSkin, mltEquation, mtlSpecify]
  end
  else
  begin
    LossTypes := [TMnwLossType(comboLossType.ItemIndex)];
  end;
  tabLossControls.TabVisible :=
    LossTypes * [mltThiem, mltSkin, mltEquation, mtlSpecify] <> [];
  edWellRadius.Enabled :=
    LossTypes * [mltThiem, mltSkin, mltEquation] <> [];

  edSkinRadius.Enabled := mltSkin in LossTypes;

  edKSkin.Enabled := mltSkin in LossTypes;

  edBCoefficient.Enabled := mltEquation in LossTypes;

  edCCoefficient.Enabled := mltEquation in LossTypes;

  edPCoefficient.Enabled := mltEquation in LossTypes;

  edCellToWellConductance.Enabled := mtlSpecify in LossTypes;

  Changed;
end;

procedure TframeScreenObjectMNW2.comboQCUTChange(Sender: TObject);
var
  RowIndex: Integer;
  NewText: string;
begin
  if comboQCUT.ItemIndex < 0 then
  begin
    Exit;
  end;
  NewText := comboQCUT.Text;
  for RowIndex := rdgTimeTable.FixedRows + PestRowOffset to rdgTimeTable.RowCount - 1 do
  begin
    if rdgTimeTable.IsSelectedCell(Ord(mtcLimitMethod), RowIndex) then
    begin
      rdgTimeTable.Cells[Ord(mtcLimitMethod), RowIndex] := NewText;
      if Assigned(rdgTimeTable.OnSetEditText) then
      begin
        rdgTimeTable.OnSetEditText(rdgTimeTable,
          Ord(mtcLimitMethod), RowIndex, NewText);
      end;
    end;
  end;
end;

constructor TframeScreenObjectMNW2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  framePumpLocationMethod.OnChange := edWellIdChange;
end;


procedure TframeScreenObjectMNW2.UpdateVerticalScreenGridCell(ScreenIndex: Integer; VerticalScreen: TVerticalScreen; AValue: string; Column: TVerticalScreenColumns);
begin
  if rdgVerticalScreens.Objects[Ord(Column), ScreenIndex + 1] = nil then
  begin
    rdgVerticalScreens.Cells[Ord(Column), ScreenIndex + 1] := AValue;
    rdgVerticalScreens.Objects[Ord(Column), ScreenIndex + 1] := VerticalScreen;
  end
  else
  begin
    if rdgVerticalScreens.Cells[Ord(Column), ScreenIndex + 1] <> AValue then
    begin
      rdgVerticalScreens.Cells[Ord(Column), ScreenIndex + 1] := '';
    end;
  end;
end;

procedure TframeScreenObjectMNW2.edPartialPenetrationChange(Sender: TObject);
begin
  Changed;
end;

procedure TframeScreenObjectMNW2.edWellIdChange(Sender: TObject);
var
  NewValue: AnsiString;
begin
  NewValue := AnsiString(edWellId.Text);
  if edWellId.Text <> string(NewValue) then
  begin
    edWellId.Text := string(NewValue);
  end;
  Changed;
end;

procedure TframeScreenObjectMNW2.edWellRadiusChange(Sender: TObject);
begin
  Changed;
end;

procedure TframeScreenObjectMNW2.EnablePartialPenetration;
begin
  edPartialPenetration.Enabled := ((VerticalWell <> cbChecked)
    or (comboLossType.ItemIndex = 0))
    and (cbPartialPenetrationFlag.State <> cbUnchecked);
end;

procedure TframeScreenObjectMNW2.framePumpLocationMethodcomboLocationChoiceChange(
  Sender: TObject);
begin
  framePumpLocationMethod.comboLocationChoiceChange(Sender);
  Changed;
end;

procedure TframeScreenObjectMNW2.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  AScreenObject: TScreenObject;
  LocalList: TList;
  Index: Integer;
  BoundaryIndex: Integer;
  Boundary: TMnw2Boundary;
  Item: TScreenObjectEditItem;
  VerticalState: TCheckBoxState;
begin
{$IFNDEF PEST}
  tabObservations.TabVisible := False;
{$ENDIF}  
  Changing := True;
  try
    InitializeControls;

    Assert(ScreenObjectList.Count >= 1);
    VerticalState := cbGrayed;
    for Index := 0 to ScreenObjectList.Count - 1 do
    begin
      Item := ScreenObjectList[Index];
      AScreenObject := Item.ScreenObject;
      if Index = 0 then
      begin
        if TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
        begin
          VerticalState := cbChecked;
        end
        else
        begin
          VerticalState := cbUnChecked;
        end;
      end
      else
      begin
        if TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
        begin
          if VerticalState <> cbChecked then
          begin
            VerticalState := cbGrayed;
            Break;
          end;
        end
        else
        begin
          if VerticalState <> cbUnChecked then
          begin
            VerticalState := cbGrayed;
            Break;
          end;
        end;
      end;
    end;
    VerticalWell := VerticalState;
    tabWellScreens.TabVisible := VerticalWell in [cbChecked, cbGrayed];

    LocalList := TList.Create;
    try
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        if (AScreenObject.ModflowMnw2Boundary <> nil)
          and (AScreenObject.ModflowMnw2Boundary.Used) then
        begin
          LocalList.Add(AScreenObject.ModflowMnw2Boundary)
        end;
      end;
      // PEST observations
      if (LocalList.Count = 1) and (ScreenObjectList.Count = 1) then
      begin
        Boundary := LocalList[0];
//        Mnw2Observations := Boundary.Observations;
        framePestObsMnw2.GetData(Boundary.Observations);
      end
      else
      begin
        tabObservations.TabVisible := (ScreenObjectList.Count = 1);
      end;
      
      edWellId.Enabled := LocalList.Count <= 1;
      if LocalList.Count > 0 then
      begin
        AssignFirstItem(LocalList);
        for BoundaryIndex := 1 to LocalList.Count - 1 do
        begin
          Boundary := LocalList[BoundaryIndex];

          {$IFDEF PEST}
          if FFirstBoundary.PestPumpingRateFormula <> Boundary.PestPumpingRateFormula then
          begin
            PestModifierAssigned[rdgTimeTable, Ord(mtcPumpingRate)] := False
          end;
          if FFirstBoundary.PestPumpingRateMethod <> Boundary.PestPumpingRateMethod then
          begin
            PestMethodAssigned[rdgTimeTable, Ord(mtcPumpingRate)] := False
          end;

          if FFirstBoundary.PestHeadCapacityMultiplierFormula <> Boundary.PestHeadCapacityMultiplierFormula then
          begin
            PestModifierAssigned[rdgTimeTable, Ord(mtcMultiplier)] := False
          end;
          if FFirstBoundary.PestHeadCapacityMultiplierMethod <> Boundary.PestHeadCapacityMultiplierMethod then
          begin
            PestMethodAssigned[rdgTimeTable, Ord(mtcMultiplier)] := False
          end;

          if FFirstBoundary.PestLimitingWaterLevelFormula <> Boundary.PestLimitingWaterLevelFormula then
          begin
            PestModifierAssigned[rdgTimeTable, Ord(mtcLimitingWaterLevel)] := False
          end;
          if FFirstBoundary.PestLimitingWaterLevelMethod <> Boundary.PestLimitingWaterLevelMethod then
          begin
            PestMethodAssigned[rdgTimeTable, Ord(mtcLimitingWaterLevel)] := False
          end;

          if FFirstBoundary.PestInactivationPumpingRateFormula <> Boundary.PestInactivationPumpingRateFormula then
          begin
            PestModifierAssigned[rdgTimeTable, Ord(mtcMinRate)] := False
          end;
          if FFirstBoundary.PestInactivationPumpingRateMethod <> Boundary.PestInactivationPumpingRateMethod then
          begin
            PestMethodAssigned[rdgTimeTable, Ord(mtcMinRate)] := False
          end;

          if FFirstBoundary.PestReactivationPumpingRateFormula <> Boundary.PestReactivationPumpingRateFormula then
          begin
            PestModifierAssigned[rdgTimeTable, Ord(mtcMaxRate)] := False
          end;
          if FFirstBoundary.PestReactivationPumpingRateMethod <> Boundary.PestReactivationPumpingRateMethod then
          begin
            PestMethodAssigned[rdgTimeTable, Ord(mtcMaxRate)] := False
          end;
          {$ENDIF}

          if comboLossType.ItemIndex <> Ord(Boundary.LossType) then
          begin
            comboLossType.ItemIndex := -1;
            comboLossTypeChange(nil);
          end;
          UpdateCheckBox(Boundary.SpecifyPump, cbSpecifyPump);
          UpdateCheckBox(Boundary.SaveMnwiInfo, cbSaveMnwiBasic);
          UpdateCheckBox(Boundary.SaveExternalFlows, cbSaveExternal);
          UpdateCheckBox(Boundary.SaveInternalFlows, cbSaveInternal);
          UpdateFormulaEdit(FloatToStr(Boundary.PumpElevation),
            rdeZPump, Boundary.SpecifyPump);
          UpdateCheckBox(Boundary.ConstrainPumping, cbConstrainPumping);
          UpdateCheckBox(Boundary.PartialPenetrationCorrection,
            cbPartialPenetrationFlag);
          UpdateCheckBox(Boundary.AdjustPumping, cbPumpCap);

          UpdateSpatialValueControls(Boundary);

          UpdateFormulaEdit(FloatToStr(Boundary.ReferenceHead),
            rdeReferenceHead, Boundary.AdjustPumping);
          UpdateFormulaEdit(FloatToStr(Boundary.MaximumLift),
            rdeLiftQ0, Boundary.AdjustPumping);
          UpdateFormulaEdit(FloatToStr(Boundary.LiftAtMaxRate),
            rdeLiftQMax, Boundary.AdjustPumping);
          UpdateFormulaEdit(FloatToStr(Boundary.WellTolerance),
            rdeWellTolerance, Boundary.AdjustPumping);

          UpdateLiftTable(Boundary);
          UpdateTimeTable(Boundary);
          UpdateVerticalScreenTable(Boundary);
        end;
      end;
    finally
      LocalList.Free;
    end;

    framePumpLocationMethod.GetData(ScreenObjectList);
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateVerticalScreenTable(Boundary: TMnw2Boundary);
var
  ColIndex: Integer;
  RowIndex: Integer;
  VerticalScreen: TVerticalScreen;
  ScreenIndex: Integer;
  AScreenObject: TScreenObject;
begin
  AScreenObject := Boundary.ScreenObject as TScreenObject;
  if TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
  begin
    if seVerticalScreens.AsInteger = Boundary.VerticalScreens.Count then
    begin
      for ScreenIndex := 0 to Boundary.VerticalScreens.Count - 1 do
      begin
        VerticalScreen := Boundary.VerticalScreens.Items[ScreenIndex] as TVerticalScreen;

        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          FloatToStr(VerticalScreen.ZTop), vsZTop);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          FloatToStr(VerticalScreen.ZBottom), vsZBot);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.WellRadius, vsRw);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.SkinRadius, vsRSkin);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.SkinK, vsKSkin);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.B, vsB);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.C, vsC);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.P, vsP);
        UpdateVerticalScreenGridCell(ScreenIndex, VerticalScreen,
          VerticalScreen.CellToWellConductance, vsCWC);
      end;
    end
    else
    begin
      for RowIndex := 1 to rdgTimeTable.RowCount - 1 do
      begin
        for ColIndex := 0 to rdgTimeTable.ColCount - 1 do
        begin
          rdgTimeTable.Cells[ColIndex, RowIndex] := '';
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateTimeTable(Boundary: TMnw2Boundary);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TimeItem: TMnw2TimeItem;
  TimeIndex: Integer;
begin
  if seTimeTableRows.AsInteger = Boundary.TimeValues.Count then
  begin
    for TimeIndex := 0 to Boundary.TimeValues.Count - 1 do
    begin
      TimeItem := Boundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
      UpdateTimeGridCell(FloatToStr(TimeItem.StartTime), mtcStartTime,
        TimeIndex, True, TimeItem);
      UpdateTimeGridCell(FloatToStr(TimeItem.EndTime), mtcEndTime,
        TimeIndex, True, TimeItem);
      UpdateTimeGridCell(TimeItem.PumpingRate, mtcPumpingRate,
        TimeIndex, True, TimeItem);
      UpdateTimeGridCell(TimeItem.HeadCapacityMultiplier, mtcMultiplier,
        TimeIndex, Boundary.AdjustPumping, TimeItem);
      UpdateTimeGridCell(TimeItem.LimitingWaterLevel, mtcLimitingWaterLevel,
        TimeIndex, Boundary.ConstrainPumping, TimeItem);
      if Boundary.ConstrainPumping then
      begin
        if rdgTimeTable.Objects[Ord(mtcLimitMethod), TimeIndex + 1] = nil then
        begin
          rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), TimeIndex + 1] :=
            Ord(TimeItem.LimitMethod);
          rdgTimeTable.Objects[Ord(mtcLimitMethod), TimeIndex + 1] := TimeItem;
        end
        else
        begin
          if rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), TimeIndex + 1] <>
            Ord(TimeItem.LimitMethod) then
          begin
            rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), TimeIndex + 1] := -1;
          end;
        end;
      end;
      UpdateTimeGridCell(TimeItem.InactivationPumpingRate, mtcMinRate,
        TimeIndex, Boundary.ConstrainPumping and
        (TimeItem.LimitMethod <> mlmNoMinimum), TimeItem);
      UpdateTimeGridCell(TimeItem.ReactivationPumpingRate, mtcMaxRate,
        TimeIndex, Boundary.ConstrainPumping and
        (TimeItem.LimitMethod <> mlmNoMinimum), TimeItem);
    end;
  end
  else
  begin
    for RowIndex := 1 to rdgTimeTable.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgTimeTable.ColCount - 1 do
      begin
        rdgTimeTable.Cells[ColIndex, RowIndex] := '';
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateLiftTable(Boundary: TMnw2Boundary);
var
  Lift: TLiftItem;
  LiftIndex: Integer;
  procedure ClearLiftTable;
  var
    ColIndex: Integer;
    RowIndex: Integer;
  begin
    for RowIndex := 1 to rdgLiftTable.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgLiftTable.ColCount - 1 do
      begin
        rdgLiftTable.Cells[ColIndex, RowIndex] := '';
      end;
    end;
  end;
begin
  if (Boundary.LiftValues.Count > 0)
    and (seLiftTableRows.AsInteger = Boundary.LiftValues.Count) then
  begin
    for LiftIndex := 0 to Boundary.LiftValues.Count - 1 do
    begin
      Lift := Boundary.LiftValues.Items[LiftIndex] as TLiftItem;
      if rdgLiftTable.Cells[Ord(mltcLift), LiftIndex + 1] <>
        FloatToStr(Lift.Lift) then
      begin
        ClearLiftTable;
        Exit;
      end;
      if rdgLiftTable.Cells[Ord(mltcQ), LiftIndex + 1] <>
        FloatToStr(Lift.Q) then
      begin
        ClearLiftTable;
        Exit;
      end;
    end;
  end
  else
  begin
    ClearLiftTable;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateSpatialValueControls(
  Boundary: TMnw2Boundary);
var
  SpatialItem: TMnw2SpatialItem;
begin
  if Boundary.Values.Count > 0 then
  begin
    SpatialItem := Boundary.Values[0] as TMnw2SpatialItem;
    UpdateFormulaEdit(SpatialItem.PartialPenetration, edPartialPenetration,
      Boundary.PartialPenetrationCorrection);
    UpdateFormulaEdit(SpatialItem.WellRadius, edWellRadius,
      Boundary.LossType in [mltThiem, mltSkin, mltEquation]);
    UpdateFormulaEdit(SpatialItem.SkinRadius, edSkinRadius,
      Boundary.LossType = mltSkin);
    UpdateFormulaEdit(SpatialItem.SkinK, edKSkin, Boundary.LossType = mltSkin);
    UpdateFormulaEdit(SpatialItem.B, edBCoefficient,
      Boundary.LossType = mltEquation);
    UpdateFormulaEdit(SpatialItem.C, edCCoefficient,
      Boundary.LossType = mltEquation);
    UpdateFormulaEdit(SpatialItem.P, edPCoefficient,
      Boundary.LossType = mltEquation);
    UpdateFormulaEdit(SpatialItem.CellToWellConductance,
      edCellToWellConductance, Boundary.LossType = mtlSpecify);
  end;
end;

procedure TframeScreenObjectMNW2.UpdateTimeGridCell(Value: string;
  Column: TMnwTimeColumns; TimeIndex: Integer; ShouldSet: boolean;
  AnObject: TObject);
begin
  if ShouldSet then
  begin
    if rdgTimeTable.Objects[Ord(Column), TimeIndex + 1] = nil then
    begin
      rdgTimeTable.Cells[Ord(Column), TimeIndex + 1] := Value;
      rdgTimeTable.Objects[Ord(Column), TimeIndex + 1] := AnObject;
    end
    else
    begin
      if rdgTimeTable.Cells[Ord(Column), TimeIndex + 1] <> Value then
      begin
        rdgTimeTable.Cells[Ord(Column), TimeIndex + 1] := '';
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  if framePumpLocationMethod <> nil then
  begin
    framePumpLocationMethod.OnChange := Value;
  end;
end;

procedure TframeScreenObjectMNW2.SetTimeGridCellForFirstItem(
  Column: TMnwTimeColumns; SetValue: Boolean; TimeIndex: Integer;
  TimeItem: TMnw2TimeItem; Value: string);
begin
  if SetValue then
  begin
    rdgTimeTable.Cells[Ord(Column), TimeIndex + 1 + PestRowOffset] := Value;
    rdgTimeTable.Objects[Ord(Column), TimeIndex + 1 + PestRowOffset] := TimeItem;
  end
  else
  begin
    rdgTimeTable.Cells[Ord(Column), TimeIndex + 1 + PestRowOffset] := '';
    rdgTimeTable.Objects[Ord(Column), TimeIndex + 1 + PestRowOffset] := nil;
  end;
end;

procedure TframeScreenObjectMNW2.SetEdForFirstItem(NewFormula: string;
  Control: TJvComboEdit; ShouldSet: boolean);
begin
  if ShouldSet then
  begin
    Control.Text := NewFormula;
    Control.Tag := 1;
  end
  else
  begin
    Control.Tag := 0;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateFormulaEdit(NewFormula: string;
  Control: TRbwDataEntry; ShouldCheck: Boolean);
begin
  if ShouldCheck then
  begin
    if Control.Tag = 0 then
    begin
      Control.Text := NewFormula;
      Control.Tag := 1;
    end
    else
    begin
      if Control.Text <> NewFormula then
      begin
        Control.Text := '';
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateFormulaEdit(NewFormula: string;
  Control: TJvComboEdit; ShouldCheck: Boolean);
begin
  if ShouldCheck then
  begin
    if Control.Tag = 0 then
    begin
      Control.Text := NewFormula;
      Control.Tag := 1;
    end
    else
    begin
      if Control.Text <> NewFormula then
      begin
        Control.Text := '';
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.UpdateCheckBox(NewValue: Boolean;
  CheckBox: TCheckBox);
begin
  if (CheckBox.State <> cbGrayed) and (CheckBox.Checked <> NewValue) then
  begin
    CheckBox.AllowGrayed := True;
    CheckBox.State := cbGrayed;
  end;
end;

procedure TframeScreenObjectMNW2.AssignFirstItem(LocalList: TList);
var
  Boundary: TMnw2Boundary;
  SpatialItem: TMnw2SpatialItem;
  LiftIndex: Integer;
  Lift: TLiftItem;
  TimeIndex: Integer;
  TimeItem: TMnw2TimeItem;
  VerticalScreens: TVerticalScreenCollection;
  Index: Integer;
  VerticalScreen: TVerticalScreen;
  AScreenObject: TScreenObject;
  ColIndex: Integer;
begin
  Boundary := LocalList[0];
  FFirstBoundary := Boundary;
  if LocalList.Count = 1 then
  begin
    edWellId.Text := Boundary.WellID;
  end;
  comboLossType.ItemIndex := Ord(Boundary.LossType);
  comboLossTypeChange(nil);
  cbSpecifyPump.Checked := Boundary.SpecifyPump;
  cbSaveMnwiBasic.Checked := Boundary.SaveMnwiInfo;
  cbSaveExternal.Checked := Boundary.SaveExternalFlows;
  cbSaveInternal.Checked := Boundary.SaveInternalFlows;
  SetEdForFirstItem(FloatToStr(Boundary.PumpElevation), rdeZPump,
    Boundary.SpecifyPump);
  cbConstrainPumping.Checked := Boundary.ConstrainPumping;
  cbPartialPenetrationFlag.Checked := Boundary.PartialPenetrationCorrection;
  cbPumpCap.Checked := Boundary.AdjustPumping;
  if Boundary.Values.Count > 0 then
  begin
    SpatialItem := Boundary.Values[0] as TMnw2SpatialItem;
    SetEdForFirstItem(SpatialItem.PartialPenetration, edPartialPenetration,
      Boundary.PartialPenetrationCorrection);
    SetEdForFirstItem(SpatialItem.WellRadius, edWellRadius,
      Boundary.LossType in [mltThiem, mltSkin, mltEquation]);
    SetEdForFirstItem(SpatialItem.SkinRadius, edSkinRadius,
      Boundary.LossType = mltSkin);
    SetEdForFirstItem(SpatialItem.SkinK, edKSkin, Boundary.LossType = mltSkin);
    SetEdForFirstItem(SpatialItem.B, edBCoefficient,
      Boundary.LossType = mltEquation);
    SetEdForFirstItem(SpatialItem.C, edCCoefficient,
      Boundary.LossType = mltEquation);
    SetEdForFirstItem(SpatialItem.P, edPCoefficient,
      Boundary.LossType = mltEquation);
    SetEdForFirstItem(SpatialItem.CellToWellConductance,
      edCellToWellConductance, Boundary.LossType = mtlSpecify);
  end;

  SetEdForFirstItem(FloatToStr(Boundary.ReferenceHead),
    rdeReferenceHead, Boundary.AdjustPumping);
  SetEdForFirstItem(FloatToStr(Boundary.MaximumLift),
    rdeLiftQ0, Boundary.AdjustPumping);
  SetEdForFirstItem(FloatToStr(Boundary.LiftAtMaxRate),
    rdeLiftQMax, Boundary.AdjustPumping);
  SetEdForFirstItem(FloatToStr(Boundary.WellTolerance),
    rdeWellTolerance, Boundary.AdjustPumping);

  if Boundary.AdjustPumping then
  begin
    if Boundary.LiftValues.Count > 0 then
    begin
      seLiftTableRows.AsInteger := Boundary.LiftValues.Count;
      seLiftTableRowsChange(nil);
      rdgLiftTable.BeginUpdate;
      try
        for LiftIndex := 0 to Boundary.LiftValues.Count - 1 do
        begin
          Lift := Boundary.LiftValues.Items[LiftIndex] as TLiftItem;
          rdgLiftTable.Cells[Ord(mltcLift), LiftIndex + 1] :=
            FloatToStr(Lift.Lift);
          rdgLiftTable.Cells[Ord(mltcQ), LiftIndex + 1] :=
            FloatToStr(Lift.Q);
        end;
      finally
        rdgLiftTable.EndUpdate;
      end;
    end;
  end;
  seTimeTableRows.AsInteger := Boundary.TimeValues.Count;
  seTimeTableRowsChange(nil);
  rdgTimeTable.BeginUpdate;
  try
    {$IFDEF PEST}
    PestModifier[rdgTimeTable, Ord(mtcPumpingRate)] := Boundary.PestPumpingRateFormula;
    PestMethod[rdgTimeTable, Ord(mtcPumpingRate)] := Boundary.PestPumpingRateMethod;

    PestModifier[rdgTimeTable, Ord(mtcMultiplier)] := Boundary.PestHeadCapacityMultiplierFormula;
    PestMethod[rdgTimeTable, Ord(mtcMultiplier)] := Boundary.PestHeadCapacityMultiplierMethod;

    PestModifier[rdgTimeTable, Ord(mtcLimitingWaterLevel)] := Boundary.PestLimitingWaterLevelFormula;
    PestMethod[rdgTimeTable, Ord(mtcLimitingWaterLevel)] := Boundary.PestLimitingWaterLevelMethod;

    PestModifier[rdgTimeTable, Ord(mtcMinRate)] := Boundary.PestInactivationPumpingRateFormula;
    PestMethod[rdgTimeTable, Ord(mtcMinRate)] := Boundary.PestInactivationPumpingRateMethod;

    PestModifier[rdgTimeTable, Ord(mtcMaxRate)] := Boundary.PestReactivationPumpingRateFormula;
    PestMethod[rdgTimeTable, Ord(mtcMaxRate)] := Boundary.PestReactivationPumpingRateMethod;

    {$ENDIF}

    for TimeIndex := 0 to Boundary.TimeValues.Count - 1 do
    begin
      TimeItem := Boundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
      rdgTimeTable.Cells[Ord(mtcStartTime), TimeIndex + 1 + PestRowOffset] :=
        FloatToStr(TimeItem.StartTime);
      rdgTimeTable.Objects[Ord(mtcStartTime), TimeIndex + 1 + PestRowOffset] := TimeItem;

      rdgTimeTable.Cells[Ord(mtcEndTime), TimeIndex + 1 + PestRowOffset] :=
        FloatToStr(TimeItem.EndTime);
      rdgTimeTable.Objects[Ord(mtcEndTime), TimeIndex + 1 + PestRowOffset] := TimeItem;

      rdgTimeTable.Cells[Ord(mtcPumpingRate), TimeIndex + 1 + PestRowOffset] :=
        TimeItem.PumpingRate;
      rdgTimeTable.Objects[Ord(mtcPumpingRate), TimeIndex + 1 + PestRowOffset] := TimeItem;

      SetTimeGridCellForFirstItem(mtcMultiplier, Boundary.AdjustPumping,
        TimeIndex, TimeItem, TimeItem.HeadCapacityMultiplier);

      SetTimeGridCellForFirstItem(mtcLimitingWaterLevel,
        Boundary.ConstrainPumping,
        TimeIndex, TimeItem, TimeItem.LimitingWaterLevel);

      if Boundary.ConstrainPumping then
      begin
        rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), TimeIndex + 1 + PestRowOffset] :=
          Ord(TimeItem.LimitMethod);
        rdgTimeTable.Objects[Ord(mtcLimitMethod), TimeIndex + 1 + PestRowOffset] :=
          TimeItem;
      end
      else
      begin
        rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), TimeIndex + 1 + PestRowOffset] := -1;
        rdgTimeTable.Objects[Ord(mtcLimitMethod), TimeIndex + 1 + PestRowOffset] := nil;
      end;

      SetTimeGridCellForFirstItem(mtcMinRate,
        Boundary.ConstrainPumping and (TimeItem.LimitMethod <> mlmNoMinimum),
        TimeIndex, TimeItem, TimeItem.InactivationPumpingRate);

      SetTimeGridCellForFirstItem(mtcMaxRate,
        Boundary.ConstrainPumping and (TimeItem.LimitMethod <> mlmNoMinimum),
        TimeIndex, TimeItem, TimeItem.ReactivationPumpingRate);
    end;
  finally
    rdgTimeTable.EndUpdate;
  end;

  AScreenObject := Boundary.ScreenObject as TScreenObject;
  if TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
  begin
    rdgVerticalScreens.BeginUpdate;
    try
      VerticalScreens := Boundary.VerticalScreens;
      seVerticalScreens.AsInteger := VerticalScreens.Count;
      seVerticalScreensChange(nil);
      for Index := 0 to VerticalScreens.Count - 1 do
      begin
        VerticalScreen := VerticalScreens.Items[Index] as TVerticalScreen;
        rdgVerticalScreens.Cells[Ord(vsZTop), Index+1] := FloatToStr(VerticalScreen.ZTop);
        rdgVerticalScreens.Cells[Ord(vsZBot), Index+1] := FloatToStr(VerticalScreen.ZBottom);
        rdgVerticalScreens.Cells[Ord(vsRw), Index+1] := VerticalScreen.WellRadius;
        rdgVerticalScreens.Cells[Ord(vsRSkin), Index+1] := VerticalScreen.SkinRadius;
        rdgVerticalScreens.Cells[Ord(vsKSkin), Index+1] := VerticalScreen.SkinK;
        rdgVerticalScreens.Cells[Ord(vsB), Index+1] := VerticalScreen.B;
        rdgVerticalScreens.Cells[Ord(vsC), Index+1] := VerticalScreen.C;
        rdgVerticalScreens.Cells[Ord(vsP), Index+1] := VerticalScreen.P;
        rdgVerticalScreens.Cells[Ord(vsCWC), Index+1] := VerticalScreen.CellToWellConductance;
        for ColIndex := 0 to rdgVerticalScreens.ColCount - 1 do
        begin
          rdgVerticalScreens.Objects[ColIndex, Index+1] := VerticalScreen;
        end;
      end;
    finally
      rdgVerticalScreens.EndUpdate;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.EnableDeleteTimeButton;
begin
  btnDeleteTime.Enabled := (seTimeTableRows.AsInteger > 1);
end;

procedure TframeScreenObjectMNW2.EnableVerticalScreenButton;
begin
  btnDeleteVertialScreen.Enabled := (seVerticalScreens.AsInteger > 0);
end;

procedure TframeScreenObjectMNW2.EnableDeleteLiftButton;
begin
  btnDeleteLift.Enabled := seLiftTableRows.AsInteger > 1;
end;

procedure TframeScreenObjectMNW2.InitializeControls;
var
  ColIndex: Integer;
  ItemIndex: Integer;
  RowIndex: Integer;
begin
  rdgVerticalScreens.BeginUpdate;
  try
    for ColIndex := 0 to rdgVerticalScreens.ColCount - 1 do
    begin
      for RowIndex := 1 to rdgVerticalScreens.RowCount - 1 do
      begin
        rdgVerticalScreens.Cells[ColIndex, RowIndex] := '';
        rdgVerticalScreens.Objects[ColIndex, RowIndex] := nil;
      end;
//      rdgVerticalScreens.Columns[ColIndex].AutoAdjustColWidths := True;
    end;
    rdgVerticalScreens.Cells[Ord(vsZTop), 0] := StrWellScreenTopZTo;
    rdgVerticalScreens.Cells[Ord(vsZBot), 0] := StrWellScreenBottom;
    rdgVerticalScreens.Cells[Ord(vsRw), 0] := lblWellRadius.Caption;
    rdgVerticalScreens.Cells[Ord(vsRSkin), 0] := lblSkinRadius.Caption;
    rdgVerticalScreens.Cells[Ord(vsKSkin), 0] := lblKSkin.Caption;
    rdgVerticalScreens.Cells[Ord(vsB), 0] := lblBCoefficient.Caption;
    rdgVerticalScreens.Cells[Ord(vsC), 0] := lblCCoefficient.Caption;
    rdgVerticalScreens.Cells[Ord(vsP), 0] := lblPCoefficient.Caption;
    rdgVerticalScreens.Cells[Ord(vsCWC), 0] := lblCellToWellConductance.Caption;
  finally
    rdgVerticalScreens.EndUpdate;
  end;
  for ColIndex := 0 to rdgVerticalScreens.ColCount - 1 do
  begin
    rdgVerticalScreens.ColWidths[ColIndex] :=
      rdgVerticalScreens.WidthNeededToFitText(ColIndex,0);
    rdgVerticalScreens.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  rdgLiftTable.Cells[Ord(mltcLift), 0] := StrLift;
  rdgLiftTable.Cells[Ord(mltcQ), 0] := StrQ;

  rdgTimeTable.BeginUpdate;
  try
    {$IFDEF PEST}
    seTimeTableRowsChange(nil);
    rdgTimeTable.Cells[0, PestModifierRow] := StrPestModifier;
    rdgTimeTable.Cells[0, PestMethodRow] := StrModificationMethod;

    PestModifier[rdgTimeTable, Ord(mtcPumpingRate)] := '';
    PestMethod[rdgTimeTable, Ord(mtcPumpingRate)] :=
      TMnw2Boundary.DefaultBoundaryMethod(PumpingRatePosition);

    PestModifier[rdgTimeTable, Ord(mtcMultiplier)] := '';
    PestMethod[rdgTimeTable, Ord(mtcMultiplier)] :=
      TMnw2Boundary.DefaultBoundaryMethod(HeadCapacityMultiplierPosition);

    PestModifier[rdgTimeTable, Ord(mtcLimitingWaterLevel)] := '';
    PestMethod[rdgTimeTable, Ord(mtcLimitingWaterLevel)] :=
      TMnw2Boundary.DefaultBoundaryMethod(LimitingWaterLevelPosition);

    PestModifier[rdgTimeTable, Ord(mtcMinRate)] := '';
    PestMethod[rdgTimeTable, Ord(mtcMinRate)] :=
      TMnw2Boundary.DefaultBoundaryMethod(InactivationPumpingRatePosition);

    PestModifier[rdgTimeTable, Ord(mtcMaxRate)] := '';
    PestMethod[rdgTimeTable, Ord(mtcMaxRate)] :=
      TMnw2Boundary.DefaultBoundaryMethod(ReactivationPumpingRatePosition);

    {$ENDIF}

    rdgTimeTable.Cells[Ord(mtcStartTime), 0] := StrStartingTime;
    rdgTimeTable.Cells[Ord(mtcEndTime), 0] := StrEndingTime;
    rdgTimeTable.Cells[Ord(mtcPumpingRate), 0] :=
      StrDesiredPumpingRate;
    rdgTimeTable.Cells[Ord(mtcMultiplier), 0] :=
      StrHeadCapacityMultip;
    rdgTimeTable.Cells[Ord(mtcLimitingWaterLevel), 0] :=
      StrLimitingWaterLevel;
    rdgTimeTable.Cells[Ord(mtcLimitMethod), 0] :=
      StrPumpingLimitMethod;
    rdgTimeTable.Cells[Ord(mtcMinRate), 0] :=
      StrDeactivationPumping;
    rdgTimeTable.Cells[Ord(mtcMaxRate), 0] :=
      StrReactivationPumping;
  finally
    rdgTimeTable.EndUpdate;
  end;

  for ColIndex := 0 to rdgTimeTable.ColCount - 1 do
  begin
    rdgTimeTable.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  cbSpecifyPumpClick(nil);
  comboLossType.ItemIndex := Ord(mltThiem);
  comboLossTypeChange(nil);
  cbPartialPenetrationFlagClick(nil);
  cbPumpCapClick(nil);

  pcMnw2.ActivePageIndex := 0;
  cbSpecifyPump.AllowGrayed := False;
  cbConstrainPumping.AllowGrayed := False;
  cbPartialPenetrationFlag.AllowGrayed := False;
  cbPumpCap.AllowGrayed := False;
  edWellId.Text := '';
  cbSpecifyPump.Checked := False;
  rdeZPump.Text := '0';
  cbConstrainPumping.Checked := False;
  cbPartialPenetrationFlag.Checked := False;
  edPartialPenetration.Text := '1';
  cbPumpCap.Checked := False;
  edWellRadius.Text := '0';
  edSkinRadius.Text := '0';
  edKSkin.Text := '0';
  edBCoefficient.Text := '0';
  edCCoefficient.Text := '0';
  edPCoefficient.Text := '0';
  edCellToWellConductance.Text := '0';
  rdeReferenceHead.Text := '0';
  rdeLiftQ0.Text := '0';
  rdeLiftQMax.Text := '0';
  rdeWellTolerance.Text := '0';
  seLiftTableRows.AsInteger := 1;
  for ColIndex := 0 to rdgLiftTable.ColCount - 1 do
  begin
    rdgLiftTable.Cells[ColIndex, 1] := '';
  end;
  seTimeTableRows.AsInteger := 1;
  for ColIndex := 0 to rdgTimeTable.ColCount - 1 do
  begin
    rdgTimeTable.Cells[ColIndex, 1] := '';
  end;

  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithStartTimes(rdgTimeTable, Ord(mtcStartTime));
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithEndTimes(rdgTimeTable, Ord(mtcEndTime));

  comboQCUT.Items.Clear;
  for ItemIndex := 0 to rdgTimeTable.Columns[
    Ord(mtcLimitMethod)].PickList.Count - 1 do
  begin
    comboQCUT.Items.Add.Text := rdgTimeTable.Columns[
    Ord(mtcLimitMethod)].PickList[ItemIndex];
  end;
  comboQCUT.ItemIndex := -1;

  cbSaveMnwiBasic.AllowGrayed := False;
  cbSaveExternal.AllowGrayed := False;
  cbSaveInternal.AllowGrayed := False;
  cbSaveMnwiBasic.Checked := False;
  cbSaveExternal.Checked := False;
  cbSaveInternal.Checked := False;

  framePestObsMnw2.InitializeControls;
end;


procedure TframeScreenObjectMNW2.rdeFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  NewText: string;
begin
  NewText := rdeFormula.Text;
  for RowIndex := rdgTimeTable.FixedRows + PestRowOffset to rdgTimeTable.RowCount - 1 do
  begin
    for ColIndex := Ord(mtcPumpingRate) to Ord(mtcMaxRate) do
    begin
      if ColIndex = Ord(mtcLimitMethod) then
      begin
        Continue;
      end;
      if rdgTimeTable.IsSelectedCell(ColIndex, RowIndex) then
      begin
        rdgTimeTable.Cells[ColIndex, RowIndex] := NewText;
        if Assigned(rdgTimeTable.OnSetEditText) then
        begin
          rdgTimeTable.OnSetEditText(rdgTimeTable, ColIndex, RowIndex, NewText);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.rdeWellScreenFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  NewText: string;
begin
  NewText := rdeWellScreenFormula.Text;
  for RowIndex := rdgVerticalScreens.FixedRows to rdgVerticalScreens.RowCount - 1 do
  begin
    for ColIndex := Ord(vsRw) to Ord(vsCWC) do
    begin
      if rdgVerticalScreens.IsSelectedCell(ColIndex, RowIndex) then
      begin
        rdgVerticalScreens.Cells[ColIndex, RowIndex] := NewText;
        if Assigned(rdgVerticalScreens.OnSetEditText) then
        begin
          rdgVerticalScreens.OnSetEditText(rdgVerticalScreens, ColIndex, RowIndex, NewText);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.rdgLiftTableEndUpdate(Sender: TObject);
begin
  if seLiftTableRows <> nil then
  begin
    seLiftTableRows.AsInteger := rdgLiftTable.RowCount - 1;
    Changed;
  end;
end;

procedure TframeScreenObjectMNW2.rdgTimeTableColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiCellEditControlsForStressPeriods;
end;

procedure TframeScreenObjectMNW2.rdgTimeTableEndUpdate(Sender: TObject);
begin
  if seTimeTableRows <> nil then
  begin
    seTimeTableRows.AsInteger := rdgTimeTable.RowCount - 1- PestRowOffset;
    Changed;
  end;
end;

procedure TframeScreenObjectMNW2.rdgTimeTableHorizontalScroll(Sender: TObject);
begin
  LayoutMultiCellEditControlsForStressPeriods;
end;

procedure TframeScreenObjectMNW2.rdgTimeTableMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  ShouldEnable := False;
  for ColIndex := Ord(mtcPumpingRate) to Ord(mtcMaxRate) do
  begin
    if ColIndex = Ord(mtcLimitMethod) then
    begin
      Continue;
    end;
    for RowIndex := rdgTimeTable.FixedRows + PestRowOffset to rdgTimeTable.RowCount - 1 do
    begin
      ShouldEnable := rdgTimeTable.IsSelectedCell(ColIndex, RowIndex);
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
  rdeFormula.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgTimeTable.FixedRows + PestRowOffset to rdgTimeTable.RowCount - 1 do
  begin
    ShouldEnable := rdgTimeTable.IsSelectedCell(Ord(mtcLimitMethod), RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboQCUT.Enabled := ShouldEnable;

end;

procedure TframeScreenObjectMNW2.rdgTimeTableSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  MnwColumn : TMnwTimeColumns;
begin
  if ARow >= 1 + PestRowOffset then
  begin
    if (ACol >= 0 ) and (ACol < rdgTimeTable.ColCount) then
    begin
      MnwColumn := TMnwTimeColumns(ACol);
      case MnwColumn of
        mtcStartTime, mtcEndTime, mtcPumpingRate:
          begin
            // do nothing;
          end;
        mtcMultiplier:
          begin
            CanSelect := cbPumpCap.State <> cbUnchecked
          end;
        mtcLimitingWaterLevel, mtcLimitMethod:
          begin
            CanSelect := cbConstrainPumping.State <> cbUnchecked
          end;
        mtcMinRate, mtcMaxRate:
          begin
            CanSelect := (cbConstrainPumping.State <> cbUnchecked)
              and (rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), ARow] > 0);
          end
        else Assert(False);
      end;
    end;
  end;

  if Assigned(OnCheckPestCell) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end
  else
  begin
    if ARow <= PestRowOffset then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.rdgTimeTableSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if (ARow >= 1 + PestRowOffset) and (ACol = Ord(mtcLimitMethod)) then
  begin
    rdgTimeTable.Invalidate;
  end;
  UpdateNextTimeCell(rdgTimeTable, ACol, ARow);
end;

procedure TframeScreenObjectMNW2.rdgVerticalScreensColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  LayoutMultiCellEditControlsForWellScreens;
end;

procedure TframeScreenObjectMNW2.rdgVerticalScreensEndUpdate(Sender: TObject);
var
  NewCount: integer;
begin
  if seVerticalScreens <> nil then
  begin
    NewCount := rdgVerticalScreens.RowCount - 1;
    if NewCount = 1 then
    begin
      if (rdgVerticalScreens.Cells[Ord(vsZTop),1] = '')
        and (rdgVerticalScreens.Cells[Ord(vsZBot),1] = '') then
      begin
        NewCount := 0;
      end;
    end;
    seVerticalScreens.AsInteger := NewCount;
    Changed;
  end;
end;

procedure TframeScreenObjectMNW2.rdgVerticalScreensHorizontalScroll(
  Sender: TObject);
begin
  LayoutMultiCellEditControlsForWellScreens;
end;

procedure TframeScreenObjectMNW2.rdgVerticalScreensMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  ShouldEnable := False;
  for ColIndex := Ord(vsRw) to Ord(vsCWC) do
  begin
    for RowIndex := rdgVerticalScreens.FixedRows to rdgVerticalScreens.RowCount - 1 do
    begin
      ShouldEnable := rdgVerticalScreens.IsSelectedCell(ColIndex, RowIndex);
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
  rdeWellScreenFormula.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectMNW2.rdgVerticalScreensSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  LossTypes : set of TMnwLossType;
begin
  if ACol > Ord(vsZBot) then
  begin
    if comboLossType.ItemIndex < 0 then
    begin
      LossTypes := [mltNone, mltThiem, mltSkin, mltEquation, mtlSpecify]
    end
    else
    begin
      LossTypes := [TMnwLossType(comboLossType.ItemIndex)];
    end;
    case TVerticalScreenColumns(ACol) of
      vsRw:
        begin
          CanSelect := LossTypes * [mltThiem, mltSkin, mltEquation] <> [];
        end;
      vsRSkin, vsKSkin:
        begin
          CanSelect := mltSkin in LossTypes;
        end;
      vsB, vsC, vsP:
        begin
          CanSelect := mltEquation in LossTypes;
        end;
      vsCWC:
        begin
          CanSelect := mtlSpecify in LossTypes;
        end;
    end;
  end;
end;

procedure TframeScreenObjectMNW2.seLiftTableRowsChange(Sender: TObject);
var
  RowCount: Integer;
begin
  RowCount := seLiftTableRows.AsInteger;
  if RowCount < 1 then
  begin
    RowCount := 1;
  end;
  rdgLiftTable.RowCount := RowCount + 1;
  EnableDeleteLiftButton;
  Changed;
end;

procedure TframeScreenObjectMNW2.SetChanging(const Value: Boolean);
begin
  FChanging := Value;
  if framePumpLocationMethod <> nil then
  begin
    framePumpLocationMethod.Changing := Value;
  end;
end;

procedure TframeScreenObjectMNW2.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TMnw2Boundary;
  BoundaryUsed: boolean;
  SpatialItem: TMnw2SpatialItem;
  RowIndex: Integer;
  LiftCount: Integer;
  LiftIndex: Integer;
  LiftItem: TLiftItem;
  TimeCount: Integer;
  TimeIndex: Integer;
  TimeItem: TMnw2TimeItem;
  LimitMethodInt: Integer;
  ScreenObject: TScreenObject;
  ScreenCount: Integer;
  ScreenIndex: Integer;
  VerticalScreen: TVerticalScreen;
  AValue: string;
  LiftError1: Boolean;
  LiftError2: Boolean;
  ShowError: boolean;
  ObsCount: Integer;
begin
  ShowError := False;
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    ScreenObject := Item.ScreenObject;
    Boundary := Item.ScreenObject.ModflowMnw2Boundary;
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
        Item.ScreenObject.CreateMnw2Boundary;
        Boundary := Item.ScreenObject.ModflowMnw2Boundary;
      end;


      ObsCount := 0;
      if List.Count = 1 then
      begin
        // Observations
        framePestObsMnw2.SetData(Boundary.Observations);
      end;

      if Trim(edWellId.Text) <> '' then
      begin
        Boundary.WellID := Trim(edWellId.Text);
      end;
      if Trim(Boundary.WellID) = '' then
      begin
        // Because this is a copy of the TScreenObject and not the original,
        // it should not be included in the call to
        // frmErrorsAndWarnings.AddError.
        frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrNoMNW2WELLIDWasD,
          ScreenObject.Name);
        ShowError := True;
      end;
      if comboLossType.ItemIndex >= 0 then
      begin
        Boundary.LossType := TMnwLossType(comboLossType.ItemIndex);
      end;
      if cbSpecifyPump.State <> cbGrayed then
      begin
        Boundary.SpecifyPump := cbSpecifyPump.Checked;
      end;
      if Boundary.SpecifyPump and (rdeZPump.Text <> '') then
      begin
        Boundary.PumpElevation := StrToFloat(rdeZPump.Text);
      end;
      if cbConstrainPumping.State <> cbGrayed then
      begin
        Boundary.ConstrainPumping := cbConstrainPumping.Checked;
      end;
      if cbPartialPenetrationFlag.State <> cbGrayed then
      begin
        Boundary.PartialPenetrationCorrection := cbPartialPenetrationFlag.Checked;
      end;
      if cbSaveMnwiBasic.State <> cbGrayed then
      begin
        Boundary.SaveMnwiInfo := cbSaveMnwiBasic.Checked or (ObsCount > 0);
      end;
      if cbSaveExternal.State <> cbGrayed then
      begin
        Boundary.SaveExternalFlows := cbSaveExternal.Checked;
      end;
      if cbSaveInternal.State <> cbGrayed then
      begin
        Boundary.SaveInternalFlows := cbSaveInternal.Checked;
      end;
      if Boundary.Values.Count = 0 then
      begin
        Boundary.Values.Add;
      end;
      SpatialItem := Boundary.Values[0] as TMnw2SpatialItem;
      if Boundary.PartialPenetrationCorrection and (edPartialPenetration.Text <> '') then
      begin
        SpatialItem.PartialPenetration := edPartialPenetration.Text;
      end;
      if cbPumpCap.State <> cbGrayed then
      begin
        Boundary.AdjustPumping := cbPumpCap.Checked;
      end;
      case Boundary.LossType of
        mltNone:
          begin
            // do nothing
          end;
        mltThiem:
          begin
            if edWellRadius.Text <> '' then
            begin
              SpatialItem.WellRadius := edWellRadius.Text;
            end;
          end;
        mltSkin:
          begin
            if edWellRadius.Text <> '' then
            begin
              SpatialItem.WellRadius := edWellRadius.Text;
            end;
            if edSkinRadius.Text <> '' then
            begin
              SpatialItem.SkinRadius := edSkinRadius.Text;
            end;
            if edKSkin.Text <> '' then
            begin
              SpatialItem.SkinK := edKSkin.Text;
            end;
          end;
        mltEquation:
          begin
            if edWellRadius.Text <> '' then
            begin
              SpatialItem.WellRadius := edWellRadius.Text;
            end;
            if edBCoefficient.Text <> '' then
            begin
              SpatialItem.B := edBCoefficient.Text;
            end;
            if edCCoefficient.Text <> '' then
            begin
              SpatialItem.C := edCCoefficient.Text;
            end;
            if edPCoefficient.Text <> '' then
            begin
              SpatialItem.P := edPCoefficient.Text;
            end;
          end;
        mtlSpecify:
          begin
            if edCellToWellConductance.Text <> '' then
            begin
              SpatialItem.CellToWellConductance := edCellToWellConductance.Text;
            end;
          end;
        else Assert(False);
      end;
      if Boundary.AdjustPumping then
      begin
        if rdeReferenceHead.Text <> '' then
        begin
          Boundary.ReferenceHead := StrToFloat(rdeReferenceHead.Text);
        end;
        if rdeLiftQ0.Text <> '' then
        begin
          Boundary.MaximumLift := StrToFloat(rdeLiftQ0.Text);
        end;
        if rdeLiftQMax.Text <> '' then
        begin
          Boundary.LiftAtMaxRate := StrToFloat(rdeLiftQMax.Text);
        end;
        if rdeWellTolerance.Text <> '' then
        begin
          Boundary.WellTolerance := StrToFloat(rdeWellTolerance.Text);
        end;
        LiftCount := 0;
        for RowIndex := 1 to rdgLiftTable.RowCount - 1 do
        begin
          if (rdgLiftTable.Cells[Ord(mltcLift), RowIndex] <> '')
            and (rdgLiftTable.Cells[Ord(mltcQ), RowIndex] <> '') then
          begin
            Inc(LiftCount);
          end;
        end;
        LiftError1 := False;
        LiftError2 := False;
        if LiftCount > 0 then
        begin
          While Boundary.LiftValues.Count < LiftCount do
          begin
            Boundary.LiftValues.Add;
          end;
          While Boundary.LiftValues.Count > LiftCount do
          begin
            Boundary.LiftValues.Delete(Boundary.LiftValues.Count-1);
          end;
          LiftIndex := -1;
          for RowIndex := 1 to rdgLiftTable.RowCount - 1 do
          begin
            if (rdgLiftTable.Cells[Ord(mltcLift), RowIndex] <> '')
              and (rdgLiftTable.Cells[Ord(mltcQ), RowIndex] <> '') then
            begin
              Inc(LiftIndex);
              LiftItem := Boundary.LiftValues.Items[LiftIndex] as TLiftItem;
              LiftItem.Lift := StrToFloat(rdgLiftTable.Cells[Ord(mltcLift), RowIndex]);
              LiftItem.Q := StrToFloat(rdgLiftTable.Cells[Ord(mltcQ), RowIndex]);
              if Boundary.MaximumLift < LiftItem.Lift then
              begin
                LiftError1 := True;
              end;
              if Boundary.LiftAtMaxRate > LiftItem.Lift then
              begin
                LiftError2 := True;
              end;
            end;
          end;
        end;
        if LiftError1 then
        begin
          ShowError := True;
          // Because this is a copy of the TScreenObject and not the original,
          // it should not be included in the call to
          // frmErrorsAndWarnings.AddError.
          frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrInvalidMnwTable,
            ScreenObject.Name);
        end;
        if LiftError2 then
        begin
          ShowError := True;
          // Because this is a copy of the TScreenObject and not the original,
          // it should not be included in the call to
          // frmErrorsAndWarnings.AddError.
          frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrInvalidMnwTable2,
            ScreenObject.Name);
        end;
      end;

      {$IFDEF PEST}
      if PestModifierAssigned[rdgTimeTable, Ord(mtcPumpingRate)]  then
      begin
        Boundary.PestPumpingRateFormula := PestModifier[rdgTimeTable, Ord(mtcPumpingRate)];
      end;
      if PestMethodAssigned[rdgTimeTable, Ord(mtcPumpingRate)] then
      begin
        Boundary.PestPumpingRateMethod := PestMethod[rdgTimeTable, Ord(mtcPumpingRate)];
      end;

      if PestModifierAssigned[rdgTimeTable, Ord(mtcMultiplier)]  then
      begin
        Boundary.PestHeadCapacityMultiplierFormula := PestModifier[rdgTimeTable, Ord(mtcMultiplier)];
      end;
      if PestMethodAssigned[rdgTimeTable, Ord(mtcMultiplier)] then
      begin
        Boundary.PestHeadCapacityMultiplierMethod := PestMethod[rdgTimeTable, Ord(mtcMultiplier)];
      end;

      if PestModifierAssigned[rdgTimeTable, Ord(mtcLimitingWaterLevel)]  then
      begin
        Boundary.PestLimitingWaterLevelFormula := PestModifier[rdgTimeTable, Ord(mtcLimitingWaterLevel)];
      end;
      if PestMethodAssigned[rdgTimeTable, Ord(mtcLimitingWaterLevel)] then
      begin
        Boundary.PestLimitingWaterLevelMethod := PestMethod[rdgTimeTable, Ord(mtcLimitingWaterLevel)];
      end;

      if PestModifierAssigned[rdgTimeTable, Ord(mtcMinRate)]  then
      begin
        Boundary.PestInactivationPumpingRateFormula := PestModifier[rdgTimeTable, Ord(mtcMinRate)];
      end;
      if PestMethodAssigned[rdgTimeTable, Ord(mtcMinRate)] then
      begin
        Boundary.PestInactivationPumpingRateMethod := PestMethod[rdgTimeTable, Ord(mtcMinRate)];
      end;

      if PestModifierAssigned[rdgTimeTable, Ord(mtcMaxRate)]  then
      begin
        Boundary.PestReactivationPumpingRateFormula := PestModifier[rdgTimeTable, Ord(mtcMaxRate)];
      end;
      if PestMethodAssigned[rdgTimeTable, Ord(mtcMaxRate)] then
      begin
        Boundary.PestReactivationPumpingRateMethod := PestMethod[rdgTimeTable, Ord(mtcMaxRate)];
      end;
      {$ENDIF}

      TimeCount := 0;
      for RowIndex := 1 + PestRowOffset to rdgTimeTable.RowCount - 1 do
      begin
        if (rdgTimeTable.Cells[Ord(mtcStartTime), RowIndex] <> '')
          and (rdgTimeTable.Cells[Ord(mtcEndTime), RowIndex] <> '')
          and (rdgTimeTable.Cells[Ord(mtcPumpingRate), RowIndex] <> '') then
        begin
          Inc(TimeCount);
        end;
      end;
      if TimeCount > 0 then
      begin
        While Boundary.TimeValues.Count < TimeCount do
        begin
          Boundary.TimeValues.Add;
        end;
        While Boundary.TimeValues.Count > TimeCount do
        begin
          Boundary.TimeValues.Delete(Boundary.TimeValues.Count-1);
        end;
        TimeIndex := -1;
        for RowIndex := 1 + PestRowOffset to rdgTimeTable.RowCount - 1 do
        begin
          if (rdgTimeTable.Cells[Ord(mtcStartTime), RowIndex] <> '')
            and (rdgTimeTable.Cells[Ord(mtcEndTime), RowIndex] <> '')
            and (rdgTimeTable.Cells[Ord(mtcPumpingRate), RowIndex] <> '') then
          begin
            Inc(TimeIndex);
            TimeItem := Boundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
            TimeItem.StartTime := StrToFloat(
              rdgTimeTable.Cells[Ord(mtcStartTime), RowIndex]);
            TimeItem.EndTime := StrToFloat(
              rdgTimeTable.Cells[Ord(mtcEndTime), RowIndex]);
            TimeItem.PumpingRate :=
              rdgTimeTable.Cells[Ord(mtcPumpingRate), RowIndex];
            if Boundary.AdjustPumping then
            begin
              TimeItem.HeadCapacityMultiplier :=
                rdgTimeTable.Cells[Ord(mtcMultiplier), RowIndex];
            end;
            if Boundary.ConstrainPumping then
            begin
              TimeItem.LimitingWaterLevel :=
                rdgTimeTable.Cells[Ord(mtcLimitingWaterLevel), RowIndex];
              LimitMethodInt :=
                rdgTimeTable.ItemIndex[Ord(mtcLimitMethod), RowIndex];
              if LimitMethodInt >= 0 then
              begin
                TimeItem.LimitMethod := TMnwLimitMethod(LimitMethodInt);
              end;
              if TimeItem.LimitMethod <> mlmNoMinimum then
              begin
                TimeItem.InactivationPumpingRate :=
                  rdgTimeTable.Cells[Ord(mtcMinRate), RowIndex];
                TimeItem.ReactivationPumpingRate :=
                  rdgTimeTable.Cells[Ord(mtcMaxRate), RowIndex];
              end;
            end;
          end;
        end;
      end;

      if TMultinodeWell.IsScreenObjectVertical(ScreenObject) then
      begin
        ScreenCount := 0;
        for RowIndex := 1 to seVerticalScreens.AsInteger do
        begin
          if (rdgVerticalScreens.Cells[Ord(vsZTop), RowIndex] <> '')
            and (rdgVerticalScreens.Cells[Ord(vsZBot), RowIndex] <> '') then
          begin
            Inc(ScreenCount);
          end;
        end;
        if ScreenCount > 0 then
        begin
          While Boundary.VerticalScreens.Count < ScreenCount do
          begin
            Boundary.VerticalScreens.Add;
          end;
          While Boundary.VerticalScreens.Count > ScreenCount do
          begin
            Boundary.VerticalScreens.Delete(Boundary.VerticalScreens.Count-1);
          end;
          ScreenIndex := -1;
          for RowIndex := 1 to rdgVerticalScreens.RowCount - 1 do
          begin
            if (rdgVerticalScreens.Cells[Ord(vsZTop), RowIndex] <> '')
              and (rdgVerticalScreens.Cells[Ord(vsZBot), RowIndex] <> '') then
            begin
              Inc(ScreenIndex);
              VerticalScreen := Boundary.VerticalScreens.Items[ScreenIndex]
                as TVerticalScreen;
              // The start and end times need to be different or the
              // vertical screen will be deleted.
              VerticalScreen.StartTime := ScreenIndex;
              VerticalScreen.EndTime := ScreenIndex+1;
              VerticalScreen.ZTop := StrToFloat(
                rdgVerticalScreens.Cells[Ord(vsZTop), RowIndex]);
              VerticalScreen.ZBottom := StrToFloat(
                rdgVerticalScreens.Cells[Ord(vsZBot), RowIndex]);

              AValue := rdgVerticalScreens.Cells[Ord(vsRw), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.WellRadius := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsRSkin), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.SkinRadius := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsKSkin), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.SkinK := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsB), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.B := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsC), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.C := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsP), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.P := AValue;
              end;

              AValue := rdgVerticalScreens.Cells[Ord(vsCWC), RowIndex];
              if AValue <> '' then
              begin
                VerticalScreen.CellToWellConductance := AValue;
              end;
            end;
          end
        end;
      end;
    end;
  end;
  framePumpLocationMethod.SetData(List, SetAll, ClearAll);
  if ShowError then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TframeScreenObjectMNW2.LayoutMultiCellEditControlsForStressPeriods;
var
  ColIndex: Integer;
  Rect: TRect;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(rdgTimeTable, comboQCUT, lblQCUT, Ord(mtcLimitMethod));

  for ColIndex := Ord(mtcPumpingRate) to Ord(mtcMaxRate) do
  begin
    if ColIndex = Ord(mtcLimitMethod) then
    begin
      Continue;
    end;
    Rect := rdgTimeTable.CellRect(ColIndex, 0);
    if (Rect.Left <> 0) or (Rect.Right <> 0) then
    begin
      LayoutControls(rdgTimeTable, rdeFormula, lblFormula, ColIndex);
      Exit;
    end;
  end;
  LayoutControls(rdgTimeTable, rdeFormula, lblFormula, Ord(mtcPumpingRate));
end;

procedure TframeScreenObjectMNW2.LayoutMultiCellEditControlsForWellScreens;
var
  ColIndex: Integer;
  Rect: TRect;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  for ColIndex := Ord(vsRw) to Ord(vsCWC) do
  begin
    Rect := rdgVerticalScreens.CellRect(ColIndex, 0);
    if (Rect.Left <> 0) or (Rect.Right <> 0) then
    begin
      LayoutControls(rdgVerticalScreens, rdeWellScreenFormula,
        lblWellScreenFormula, ColIndex);
      Exit;
    end;
  end;
  LayoutControls(rdgVerticalScreens, rdeWellScreenFormula,
    lblWellScreenFormula, Ord(vsRw));

end;

procedure TframeScreenObjectMNW2.SetEdForFirstItem(NewFormula: string;
  Control: TRbwDataEntry; ShouldSet: boolean);
begin
  if ShouldSet then
  begin
    Control.Text := NewFormula;
    Control.Tag := 1;
  end
  else
  begin
    Control.Tag := 0;
  end;
end;

procedure TframeScreenObjectMNW2.seTimeTableRowsChange(Sender: TObject);
var
  RowCount: Integer;
begin
  RowCount := seTimeTableRows.AsInteger + PestRowOffset;
  if RowCount < 1 + PestRowOffset then
  begin
    RowCount := 1 + PestRowOffset;
  end;
  rdgTimeTable.RowCount := RowCount + 1;
  EnableDeleteTimeButton;
  Changed;
end;

procedure TframeScreenObjectMNW2.SetVerticalWell(const Value: TCheckBoxState);
begin
  FVerticalWell := Value;
  EnablePartialPenetration;
  cbSpecifyPumpClick(nil);
end;

procedure TframeScreenObjectMNW2.seVerticalScreensChange(Sender: TObject);
var
  RowCount: Integer;
  ColIndex: Integer;
begin
  RowCount := seVerticalScreens.AsInteger;
  if RowCount < 1 then
  begin
    RowCount := 1;
  end;
  rdgVerticalScreens.RowCount := RowCount + 1;
  if seVerticalScreens.AsInteger = 0 then
  begin
    for ColIndex := 0 to rdgVerticalScreens.ColCount - 1 do
    begin
      rdgVerticalScreens.Cells[ColIndex, 1] := '';
    end;
  end;
  EnableVerticalScreenButton;
  Changed;
end;

end.
