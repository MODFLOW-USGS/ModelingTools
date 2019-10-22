unit frmModflowTimeUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Grids,
  RbwDataGrid4, ComCtrls, Mask, JvExMask, JvSpin, JvExStdCtrls, JvCombobox,
  JvListComb, ArgusDataEntry, ModflowTimeUnit, UndoItems,
  RequiredDataSetsUndoUnit, Mt3dmsTimesUnit, frameGridUnit,
  Generics.Collections, ScreenObjectUnit, GrayTabs;

type
  TMt3dmsTimeColumns = (mtStressPeriod, mtcStartTime, mtcEndTime, mtcStepSize,
    mtcMaxSteps, mtcMultiplier, mtcMaxStepSize, mtcSteadyState);

  TfrmModflowTime = class(TfrmCustomGoPhast)
    pnlTop: TPanel;
    dgTime: TRbwDataGrid4;
    rdePeriodLength: TRbwDataEntry;
    rdeMaxFirstStepLength: TRbwDataEntry;
    rdeMultiplier: TRbwDataEntry;
    comboSteadyTransient: TJvImageComboBox;
    lblPeriodLength: TLabel;
    lblMaxFirstTimeStepLength: TLabel;
    lblMultiplier: TLabel;
    lblSteadyTransient: TLabel;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    pgcMain: TPageControl;
    tabModflow: TTabSheet;
    btnHelp: TBitBtn;
    tabMt3dms: TTabSheet;
    pnlModflowBottom: TPanel;
    seNumPeriods: TJvSpinEdit;
    lblNumPeriods: TLabel;
    comboTimeUnit: TJvComboBox;
    lblTimeUnit: TLabel;
    btnDelete: TButton;
    btnInsert: TButton;
    frameGrid: TframeGrid;
    btnConvertTimeUnits: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure dgTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seNumPeriodsChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgTimeColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdePeriodLengthChange(Sender: TObject);
    procedure rdeMaxFirstStepLengthChange(Sender: TObject);
    procedure rdeMultiplierChange(Sender: TObject);
    procedure comboSteadyTransientChange(Sender: TObject);
    procedure dgTimeBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure dgTimeHorizontalScroll(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure dgTimeButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure frameGridGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure pgcMainChange(Sender: TObject);
    procedure frameGridGridBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure comboTimeUnitChange(Sender: TObject);
    procedure btnConvertTimeUnitsClick(Sender: TObject);
  private
    FModflowStressPeriods: TModflowStressPeriods;
    FDeleting: Boolean;
    FGettingData: Boolean;
    procedure GetData;
    procedure SetData;
    procedure FillEmptyCells;
    procedure LayoutMultiRowEditControls;
    procedure GetTimePeriodValues(ARow: Integer; var PerLength: Double;
      var MaxTimeStepLength: Double; var TimeStepMultiplier: Double);
    procedure SetDeleteButtonEnabled;
    procedure UpdateNumberOfTimeSteps(const ARow: integer);
  { Private declarations }
  public
    { Public declarations }
  end;

  TUndoModflowStressPeriods = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewStressPeriods: TModflowStressPeriods;
    FOldStressPeriods: TModflowStressPeriods;
    FNewTimeUnit: integer;
    FOldTimeUnit: integer;
    FOldMt3dmsTimes: TMt3dmsTimeCollection;
    FNewMt3dmsTimes: TMt3dmsTimeCollection;
    FOldTimes: TList<Double>;
    FNewTimes: TList<Double>;
    FOldScreenObjects: TObjectList<TScreenObject>;
    FExistingScreenObjects: TList<TScreenObject>;
  protected
    function Description: string; override;
  public
    constructor Create(var NewStressPeriods: TModflowStressPeriods;
      NewTimeUnit: integer; var NewMt3dmsTimes: TMt3dmsTimeCollection);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

{$R *.dfm}

uses Math, frmGoPhastUnit, frmTimeStepLengthCalculatorUnit, GoPhastTypes,
  ModflowPackageSelectionUnit, frmErrorsAndWarningsUnit, PhastModelUnit,
  frmTimeUnitsConverterUnit, ModflowOutputControlUnit;

type
  TTimeColumn = (tcStressPeriod, tcStartTime, tcEndTime, tcLength,
    tcTimeFirstStep, tcMultiplier, tcSteady, tcDrawDownReference, tcSteps);

resourcestring
  StrStressPeriod = 'Stress period';
  StrLength = 'Length';
  StrMaxFirstTimeStep = 'Max first time step length';
  StrMultiplier = 'Multiplier';
  StrSSTR = 'Steady State/ Transient';
  StrNumberOfSteps = 'Number of steps (calculated)';
  StrDrawdownReference = 'Drawdown reference';
  StrChangeStressPeriod = 'change stress periods';
  StrYouMustSpecifyThe = 'You must specify the length of the stress period a' +
  'nd multiplier to calculate the length of the first time step.';
  StrPreferredStepSize = 'Preferred step size (DT0)';
  StrInitialStepSize = 'Initial step size (DT0)';
  StrMaximumnTransportS = 'Maximumn transport steps per flow step (MXSTRN)';
  StrTimeStepMultiplier = 'Time step multiplier (TTSMULT)';
  StrMaximumStepSize = 'Maximum step size (TTSMAX)';
  StrChangeMT3DMSTimeD = 'change MT3DMS time data';
  StrNoStressPeriodsHa = 'No stress periods have been properly defined. Plea' +
  'se check again.';
  StrSteadyStateTranspo = 'Steady state transport (SSFlag)';
  StrMT3DMSAllowsAMaxi = 'MT3DMS allows a maximun of 1000 time steps in a mo' +
  'del. You model has more than that. You will need to fix this before you c' +
  'an run MT3DMS.';
  StrAtLeastOneStress = 'At least one stress period has %d time steps. If th' +
  'is is not what you intend, you should fix the problem before trying to ru' +
  'n the model.';
  StrTheFirstAndOnlyS = 'The first and only stress period is a also a refere' +
  'nce stress period for calculating drawdown.  This might be a mistake beca' +
  'use drawdown will always be calculated as zero under these conditions.';
  StrTheFirstStressPer = 'The first stress period is a transient stress peri' +
  'od but it is also a reference stress period for calculating drawdown.  Th' +
  'is might be a mistake.';
  StrYouNeedToSelectA = 'You need to select a row in the grid before clickin' +
  'g the Insert button.';
  StrTheValuesYouSpeci = 'The values you specified for the lenth of a stress' +
  ' period, maximum time step length and time step multiplier in stress peri' +
  'od %d result in too large a number of time steps.';
  StrYouMustSetTheLen = 'You must set the length of the first time step in e' +
  'very stress period to a value greater than zero. Check for an error in st' +
  'ress period %d';
  StrTimeUnitsOfSWil = 'Time units of %s will be used throughout the model. '
    + 'Be sure that hydraulic conductivity and any other affected variables '
    + 'use the correct units.';
  StrYouWillNeedToAct = 'You will need to activate the Storage package to us' +
  'e transient stress periods.';
  StrThereAreTooManyS = 'There are too many stress periods to save heads or ' +
  'drawdowns to text files. Save them in binary files instead using the "Mod' +
  'el|MODFLOW Output Control" dialog box.';
  StrTheMaximumNumberO = 'The maximum number of time steps in a stress perio' +
  'd compatible with a formatted head or  drawdown file is 999,999. Save the' +
  'm in binary files instead using the "Model|MODFLOW Output Control" dialog' +
  ' box.';

var
  MaxSteps: integer = 100;

procedure TfrmModflowTime.btnOKClick(Sender: TObject);
var
  Index: Integer;
  StressPeriods: TModflowStressPeriods;
  StressPeriod: TModflowStressPeriod;
  Steps: integer;
  TotalSteps: Int64;
  ShowUzfLakWarning: boolean;
  UzfLakUsed: boolean;
  LengthFirstTimeStep: Extended;
  OutputControl: TModflowOutputControl;
  FormatedOutputUsed: Boolean;
  Warning: string;
begin
  inherited;
  for Index := 1 to dgTime.RowCount - 1 do
  begin
    LengthFirstTimeStep := dgTime.RealValueDefault[Ord(tcTimeFirstStep), Index, 0];
    if LengthFirstTimeStep <= 0 then
    begin
      Beep;
      MessageDlg(Format(StrYouMustSetTheLen, [Index]), mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    end;
  end;

  SetData;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;

  Warning := StressPeriods.FmpTimeStepSizeWarning(frmGoPhast.PhastModel);
  if Warning <> '' then
  begin
    Beep;
    MessageDlg(Warning, mtWarning, [mbOK], 0);
  end;

  OutputControl := frmGoPhast.PhastModel.ModflowOutputControl;
  FormatedOutputUsed := OutputControl.FormattedOutputUsed;
  if StressPeriods.Count >= 10000 then
  begin
    if FormatedOutputUsed then
    begin
      Beep;
      MessageDlg(StrThereAreTooManyS, mtWarning, [mbOK], 0);
    end;
  end;

  if (frmGoPhast.ModelSelection = msModflow2015) and StressPeriods.TransientModel
    and not frmGoPhast.PhastModel.ModflowPackages.StoPackage.IsSelected then
  begin
    Beep;
    MessageDlg(StrYouWillNeedToAct, mtWarning, [mbOK], 0);
  end;

  Steps := 0;
  TotalSteps := 0;
  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel,
    StrMultiplierWarning);
  UzfLakUsed := frmGoPhast.PhastModel.UzfIsSelected
    or frmGoPhast.PhastModel.LakIsSelected;

  ShowUzfLakWarning := False;
  for Index := 0 to StressPeriods.Count - 1 do
  begin
    StressPeriod := StressPeriods[Index];
    if StressPeriod.NumberOfSteps > Steps then
    begin
      Steps := StressPeriod.NumberOfSteps;
    end;
    TotalSteps := TotalSteps + StressPeriod.NumberOfSteps;
    if UzfLakUsed and (StressPeriod.TimeStepMultiplier <> 1) then
    begin
      ShowUzfLakWarning := True;
    end;
  end;

  if ShowUzfLakWarning then
  begin
    frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrMultiplierWarning,
      StrMultiplierFullWarning);
  end;

  if dgTime.Checked[Ord(tcDrawDownReference), 1] then
  begin
    if frmGoPhast.ModelSelection <> msModflow2015 then
    begin
      if dgTime.Cells[Ord(tcSteady), 1] =
        dgTime.Columns[Ord(tcSteady)].PickList[1] then
      begin
        // Transient model
        Beep;
        MessageDlg(StrTheFirstStressPer, mtWarning, [mbOK], 0);
      end
      else if dgTime.RowCount = 2 then
      begin
        Beep;
        MessageDlg(StrTheFirstAndOnlyS, mtWarning, [mbOK], 0);
      end;
    end;
  end;
  if Steps > MaxSteps then
  begin
    Beep;
    MessageDlg(Format(StrAtLeastOneStress, [Steps]),
      mtWarning, [mbOK], 0);
  end;
  if Steps >= 100000 then
  begin
    if FormatedOutputUsed then
    begin
      Beep;
      MessageDlg(StrTheMaximumNumberO, mtWarning, [mbOK], 0);
    end;
  end;
  if (TotalSteps > 1000) and frmGoPhast.PhastModel.Mt3dmsIsSelected then
  begin
    Beep;
    MessageDlg(StrMT3DMSAllowsAMaxi, mtWarning, [mbOK], 0);
  end;

end;

procedure TfrmModflowTime.SetDeleteButtonEnabled;
begin
  btnDelete.Enabled := dgTime.RowCount > 2;
end;

procedure TfrmModflowTime.btnConvertTimeUnitsClick(Sender: TObject);
var
  frmTimeUnitsConverter: TfrmTimeUnitsConverter;
begin
  inherited;
  frmTimeUnitsConverter := TfrmTimeUnitsConverter.Create(nil);
  frmTimeUnitsConverter.Show;
end;

procedure TfrmModflowTime.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if dgTime.SelectedRow >= dgTime.FixedRows then
  begin
    dgTime.DeleteRow(dgTime.SelectedRow);
    seNumPeriods.AsInteger := seNumPeriods.AsInteger - 1;
  end;
  SetDeleteButtonEnabled;
end;

procedure TfrmModflowTime.btnInsertClick(Sender: TObject);
begin
  inherited;
  if (dgTime.SelectedRow <= 0)
    or (dgTime.SelectedRow >= dgTime.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  dgTime.InsertRow(dgTime.SelectedRow);
  seNumPeriods.AsInteger := seNumPeriods.AsInteger + 1;
  SetDeleteButtonEnabled;
end;

procedure TfrmModflowTime.dgTimeBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  PerLength: double;
begin
  inherited;
  if (ARow >= dgTime.FixedRows) then
  begin
    case TTimeColumn(ACol) of
      tcLength:
        begin
          if tryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PerLength) then
          begin
            if PerLength <= 0 then
            begin
              dgTime.Canvas.Brush.Color := clRed;
            end;
          end;
        end;
      tcStartTime:
        begin
          if (ARow > 1) and
            (dgTime.Cells[Ord(tcStartTime), ARow] <>
              dgTime.Cells[Ord(tcEndTime), ARow-1]) then
          begin
            dgTime.Canvas.Brush.Color := clRed;
          end;
        end;
      tcEndTime:
        begin
          if (ARow < dgTime.RowCount-1) and
            (dgTime.Cells[Ord(tcStartTime), ARow+1] <>
              dgTime.Cells[Ord(tcEndTime), ARow]) then
          begin
            dgTime.Canvas.Brush.Color := clRed;
          end;
        end;
      tcDrawDownReference:
        begin
          if dgTime.Checked[Ord(tcDrawDownReference),ARow]
            and (dgTime.Cells[Ord(tcSteady),ARow] =
            dgTime.Columns[Ord(tcSteady)].PickList[1])  then
          begin
            dgTime.Canvas.Brush.Color := clYellow;
          end;
        end;
    end;
  end;

end;

procedure TfrmModflowTime.dgTimeButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  NumSteps: integer;
  PeriodLength, Multiplier: double;
  TimeStepLength: double;
begin
  inherited;
  if TryStrToInt(dgTime.Cells[Ord(tcSteps),ARow], NumSteps)
    and TryStrToFloat(dgTime.Cells[Ord(tcLength),ARow], PeriodLength)
    and TryStrToFloat(dgTime.Cells[Ord(tcMultiplier),ARow], Multiplier)
    then
  begin
    if CalculateTimeStepLength(NumSteps, PeriodLength, Multiplier,
      TimeStepLength) then
    begin
      dgTime.Cells[Ord(tcMultiplier),ARow] := FloatToStr(Multiplier);
      dgTimeSetEditText(dgTime, Ord(tcMultiplier),ARow,
        dgTime.Cells[Ord(tcMultiplier),ARow]);

      dgTime.Cells[Ord(tcLength),ARow] := FloatToStr(PeriodLength);
      dgTimeSetEditText(dgTime, Ord(tcLength),ARow,
        dgTime.Cells[Ord(tcLength),ARow]);

      dgTime.Cells[Ord(tcTimeFirstStep),ARow] := FloatToStr(TimeStepLength);
      dgTimeSetEditText(dgTime, Ord(tcTimeFirstStep),ARow,
        dgTime.Cells[Ord(tcTimeFirstStep),ARow]);
    end;
  end
  else
  begin
    Beep;
    MessageDlg(StrYouMustSpecifyThe, mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmModflowTime.dgTimeColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmModflowTime.dgTimeHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmModflowTime.dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgTime, rdePeriodLength, Ord(tcLength));
  EnableMultiEditControl(dgTime, rdeMaxFirstStepLength, Ord(tcTimeFirstStep));
  EnableMultiEditControl(dgTime, rdeMultiplier, Ord(tcMultiplier));
  EnableMultiEditControl(dgTime, comboSteadyTransient, Ord(tcSteady));
end;

procedure TfrmModflowTime.dgTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  inherited;
  if ACol= Ord(tcSteps) then
  begin
    CanSelect := False;
  end;

  if (ACol = Ord(tcDrawDownReference))
    and (frmGoPhast.ModelSelection = msModflow2015) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmModflowTime.comboSteadyTransientChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcSteady), comboSteadyTransient.Text);
end;

procedure TfrmModflowTime.comboTimeUnitChange(Sender: TObject);
begin
  inherited;
  if not FGettingData then
  begin
    MessageDlg(Format(StrTimeUnitsOfSWil, [comboTimeUnit.Text]), mtInformation, [mbOK], 0);
  end;
end;

procedure TfrmModflowTime.rdeMultiplierChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcMultiplier), rdeMultiplier.Text);
end;

procedure TfrmModflowTime.rdeMaxFirstStepLengthChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcTimeFirstStep), rdeMaxFirstStepLength.Text);
end;

procedure TfrmModflowTime.rdePeriodLengthChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcLength), rdePeriodLength.Text);
end;

procedure TfrmModflowTime.FillEmptyCells;
var
  RowIndex: integer;
  StartTime: Double;
  EndTime: Double;
  PeriodLength: double;
begin
  for RowIndex := 1 to dgTime.RowCount - 1 do
  begin
    if dgTime.Cells[Ord(tcStressPeriod), RowIndex] = '' then
    begin
      dgTime.Cells[Ord(tcStressPeriod), RowIndex] := IntToStr(RowIndex);
    end;

    if dgTime.Cells[Ord(tcStartTime), RowIndex] = '' then
    begin
      if RowIndex = 1 then
      begin
        dgTime.Cells[Ord(tcStartTime), RowIndex] := '0';
      end
      else
      begin
        dgTime.Cells[Ord(tcStartTime), RowIndex] :=
          dgTime.Cells[Ord(tcEndTime), RowIndex-1];
      end;

    end;

    if dgTime.Cells[Ord(tcEndTime), RowIndex] = '' then
    begin
      if TryStrToFloat(dgTime.Cells[Ord(tcStartTime), RowIndex], StartTime) then
      begin
        if (RowIndex = 1) or
          not TryStrToFloat(dgTime.Cells[Ord(tcLength), RowIndex-1],
          PeriodLength) then
        begin
          PeriodLength := 1
        end;
        dgTime.Cells[Ord(tcEndTime), RowIndex] :=
          FloatToStr(StartTime + PeriodLength);
      end
      else
      begin
        dgTime.Cells[Ord(tcEndTime), RowIndex] := '1';
      end;

    end;

    if dgTime.Cells[Ord(tcLength), RowIndex] = '' then
    begin
      if TryStrToFloat(dgTime.Cells[Ord(tcStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgTime.Cells[Ord(tcEndTime), RowIndex], EndTime) then
      begin
        dgTime.Cells[Ord(tcLength), RowIndex]
          := FloatToStr(EndTime - StartTime);
      end
      else
      begin
        dgTime.Cells[Ord(tcLength), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] :=
          dgTime.Cells[Ord(tcTimeFirstStep), RowIndex-1];
      end
      else
      begin
        dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcMultiplier), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcMultiplier), RowIndex] :=
          dgTime.Cells[Ord(tcMultiplier), RowIndex-1];
      end
      else
      begin
        dgTime.Cells[Ord(tcMultiplier), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcSteady), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcSteady), RowIndex] :=
          dgTime.Columns[Ord(tcSteady)].PickList[1];
      end
      else
      begin
        dgTime.Cells[Ord(tcSteady), RowIndex] :=
          dgTime.Columns[Ord(tcSteady)].PickList[0];
      end;
    end;

    UpdateNumberOfTimeSteps(RowIndex);
  end;
end;

procedure TfrmModflowTime.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(dgTime, rdePeriodLength, lblPeriodLength, Ord(tcLength));
  LayoutControls(dgTime, rdeMaxFirstStepLength, lblMaxFirstTimeStepLength, Ord(tcTimeFirstStep));
  LayoutControls(dgTime, rdeMultiplier, lblMultiplier, Ord(tcMultiplier));
  LayoutControls(dgTime, comboSteadyTransient, lblSteadyTransient, Ord(tcSteady));
end;

type TGridCrack = class(TRbwDataGrid4);

procedure TfrmModflowTime.pgcMainChange(Sender: TObject);
var
  AList: TStringList;
begin
  inherited;
  if pgcMain.ActivePage = tabMt3dms then
  begin
    AList := TStringList.Create;
    try
      AList.Assign(dgTime.Cols[Ord(tcStartTime)]);
      AList.Delete(0);
      frameGrid.Grid.Columns[Ord(mtcStartTime)].PickList := AList;
      AList.Assign(dgTime.Cols[Ord(tcEndTime)]);
      AList.Delete(0);
      frameGrid.Grid.Columns[Ord(mtcEndTime)].PickList := AList;
      TGridCrack(frameGrid.Grid).HideEditor;
    finally
      AList.Free;
    end;
  end;
end;

procedure TfrmModflowTime.seNumPeriodsChange(Sender: TObject);
begin
  inherited;
  dgTime.BeginUpdate;
  FDeleting := True;
  try
    // If the first and only stress period is a steady-state
    // stress period, then make it the reference period for
    // calculating drawdown when adding more stress periods.
    If (dgTime.RowCount = 2) then
    begin
      if dgTime.Cells[Ord(tcSteady),1] =
        dgTime.Columns[Ord(tcSteady)].PickList[0] then
      begin
        dgTime.Checked[Ord(tcDrawDownReference),1] := True; 
      end;
    end;

    dgTime.RowCount := seNumPeriods.AsInteger + 1;
    FillEmptyCells;
    SetDeleteButtonEnabled;
  finally
    FDeleting := False;
    dgTime.EndUpdate;
  end;
end;

procedure TfrmModflowTime.GetData;
var
  RowIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Mt3dmsTimes: TMt3dmsTimeCollection;
  Index: Integer;
  TimeItem: TMt3dmsTimeItem;
begin
  FGettingData := True;
  try
    comboTimeUnit.ItemIndex := frmGoPhast.PhastModel.ModflowOptions.TimeUnit;
    seNumPeriods.AsInteger := frmGoPhast.PhastModel.ModflowStressPeriods.Count;
    FillEmptyCells;
    dgTime.BeginUpdate;
    try
      for RowIndex := 1 to dgTime.RowCount - 1 do
      begin
        StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[RowIndex-1];
        dgTime.Cells[ord(tcStartTime), RowIndex]
          := FloatToStr(StressPeriod.StartTime);
        dgTime.Cells[ord(tcEndTime), RowIndex]
          := FloatToStr(StressPeriod.EndTime);
        dgTime.Cells[ord(tcLength), RowIndex]
          := FloatToStr(StressPeriod.PeriodLength);
        dgTime.Cells[ord(tcTimeFirstStep), RowIndex]
          := FloatToStr(StressPeriod.MaxLengthOfFirstTimeStep);
        dgTime.Cells[ord(tcMultiplier), RowIndex]
          := FloatToStr(StressPeriod.TimeStepMultiplier);
        dgTime.Cells[ord(tcSteady), RowIndex]
          := dgTime.Columns[Ord(tcSteady)].
          PickList[Ord(StressPeriod.StressPeriodType)];
        dgTime.Checked[ord(tcDrawDownReference), RowIndex]
          := StressPeriod.DrawDownReference;
        UpdateNumberOfTimeSteps(RowIndex);
      end;
    finally
      dgTime.EndUpdate
    end;

    if frmGoPhast.PhastModel.Mt3dmsIsSelected then
    begin
      frameGrid.Grid.BeginUpdate;
      try
        Mt3dmsTimes := frmGoPhast.PhastModel.Mt3dmsTimes;
        frameGrid.seNumber.AsInteger := Mt3dmsTimes.Count;
        for Index := 0 to Mt3dmsTimes.Count - 1 do
        begin
          TimeItem := Mt3dmsTimes[Index];
          frameGrid.Grid.Cells[Ord(mtStressPeriod), Index+1] := IntToStr(Index+1);
          frameGrid.Grid.Cells[Ord(mtcStartTime), Index+1] := FloatToStr(TimeItem.StartTime);
          frameGrid.Grid.Cells[Ord(mtcEndTime), Index+1] := FloatToStr(TimeItem.EndTime);
          frameGrid.Grid.Cells[Ord(mtcStepSize), Index+1] := FloatToStr(TimeItem.StepSize);
          frameGrid.Grid.Cells[Ord(mtcMaxSteps), Index+1] := IntToStr(TimeItem.MaxSteps);
          frameGrid.Grid.Cells[Ord(mtcMultiplier), Index+1] := FloatToStr(TimeItem.TimeStepMultiplier);
          frameGrid.Grid.Cells[Ord(mtcMaxStepSize), Index+1] := FloatToStr(TimeItem.MaxStepSize);
          frameGrid.Grid.Checked[Ord(mtcSteadyState), Index+1] := TimeItem.SteadyState;
        end;
      finally
        frameGrid.Grid.EndUpdate;
      end;
      frameGrid.seNumber.MinValue := 1;
    end
    else
    begin
      tabModflow.TabVisible := False;
      tabMt3dms.TabVisible := False;
    end;
    pgcMain.ActivePage := tabModflow;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmModflowTime.SetData;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
  Value: double;
  IntValue: integer;
  Undo: TUndoModflowStressPeriods;
  Mt3dmsTimes: TMt3dmsTimeCollection;
  StartTime, EndTime, StepSize, Multiplier, MaxStepSize: double;
  MaxSteps: integer;
  TimeItem: TMt3dmsTimeItem;
begin
  FModflowStressPeriods.Clear;

  for Index := 1 to dgTime.RowCount - 1 do
  begin
    StressPeriod := FModflowStressPeriods.Add as TModflowStressPeriod;

    if TryStrToFloat(dgTime.Cells[ord(tcStartTime), Index], Value) then
    begin
      StressPeriod.StartTime := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcEndTime), Index], Value) then
    begin
      StressPeriod.EndTime := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcLength), Index], Value) then
    begin
      StressPeriod.PeriodLength := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcTimeFirstStep), Index], Value) then
    begin
      StressPeriod.MaxLengthOfFirstTimeStep := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcMultiplier), Index], Value) then
    begin
      StressPeriod.TimeStepMultiplier := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    IntValue := dgTime.Columns[Ord(tcSteady)].PickList.IndexOf(
      dgTime.Cells[ord(tcSteady), Index]);
    Assert(IntValue in
      [Ord(Low(TStressPeriodType))..Ord(High(TStressPeriodType))]);
    StressPeriod.StressPeriodType := TStressPeriodType(IntValue);
    StressPeriod.DrawDownReference :=
      dgTime.Checked[Ord(tcDrawDownReference), Index]
  end;


  Mt3dmsTimes := TMt3dmsTimeCollection.Create(nil);
  try
    for Index := 0 to frameGrid.seNumber.AsInteger - 1 do
    begin
      if TryStrToFloat(frameGrid.Grid.Cells[Ord(mtcStartTime), Index+1], StartTime)
        and TryStrToFloat(frameGrid.Grid.Cells[Ord(mtcEndTime), Index+1], EndTime)
        and TryStrToFloat(frameGrid.Grid.Cells[Ord(mtcStepSize), Index+1], StepSize)
        and TryStrToInt(frameGrid.Grid.Cells[Ord(mtcMaxSteps), Index+1], MaxSteps)
        and TryStrToFloat(frameGrid.Grid.Cells[Ord(mtcMultiplier), Index+1], Multiplier)
        and TryStrToFloat(frameGrid.Grid.Cells[Ord(mtcMaxStepSize), Index+1], MaxStepSize) then
      begin
        TimeItem := Mt3dmsTimes.Add;
        TimeItem.StartTime := StartTime;
        TimeItem.EndTime := EndTime;
        TimeItem.StepSize := StepSize;
        TimeItem.MaxSteps := MaxSteps;
        TimeItem.TimeStepMultiplier := Multiplier;
        TimeItem.MaxStepSize := MaxStepSize;
        TimeItem.SteadyState := frameGrid.Grid.Checked[Ord(mtcSteadyState), Index+1];
      end;
    end;

    if FModflowStressPeriods.Count > 0 then
    begin
      Undo:= TUndoModflowStressPeriods.Create(FModflowStressPeriods,
        comboTimeUnit.ItemIndex, Mt3dmsTimes);
      frmGoPhast.UndoStack.Submit(Undo);
    end
    else
    begin
      Beep;
      MessageDlg(StrNoStressPeriodsHa, mtError, [mbOK], 0);
      ModalResult := mrNone;
    end;
  finally
    Mt3dmsTimes.Free;
  end;

end;

procedure TfrmModflowTime.GetTimePeriodValues(ARow: Integer;
  var PerLength, MaxTimeStepLength, TimeStepMultiplier: Double);
begin
  if not TryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PerLength) then
  begin
    PerLength := 0;
  end;
  if not TryStrToFloat(dgTime.Cells[Ord(tcTimeFirstStep), ARow], MaxTimeStepLength) then
  begin
    MaxTimeStepLength := 1;
  end;
  if not TryStrToFloat(dgTime.Cells[Ord(tcMultiplier), ARow],
    TimeStepMultiplier) then
  begin
    TimeStepMultiplier := 1;
  end;
end;

procedure TfrmModflowTime.UpdateNumberOfTimeSteps(const ARow: integer);
var
  PerLength: double;
  MaxTimeStepLength: double;
  TimeStepMultiplier: double;
begin
  GetTimePeriodValues(ARow, PerLength, MaxTimeStepLength, TimeStepMultiplier);
  try
    dgTime.Cells[Ord(tcSteps),ARow] := IntToStr(
      GetNumberOfTimeSteps(PerLength, MaxTimeStepLength, TimeStepMultiplier));
  except on E: EIntOverflow do
    begin
      dgTime.Cells[Ord(tcSteps),ARow] := IntToStr(MAXINT);
      Beep;
      MessageDlg(Format(StrTheValuesYouSpeci, [ARow]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmModflowTime.dgTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  StartTime: double;
  EndTime: double;
  PeriodLength: double;
  procedure UpdatePeriodLength(Const ARow: integer);
  begin
    if not TryStrToFloat(dgTime.Cells[Ord(tcStartTime), ARow], StartTime) then
    begin
      StartTime := 0;
    end;
    if not TryStrToFloat(dgTime.Cells[Ord(tcEndTime), ARow], EndTime) then
    begin
      EndTime := 0;
    end;
    dgTime.Cells[Ord(tcLength), ARow] := FloatToStr(EndTime-StartTime);
    UpdateNumberOfTimeSteps(ARow);
  end;
begin
  inherited;
  if FDeleting then
  begin
    Exit;
  end;
  if dgTime.RowCount <> seNumPeriods.AsInteger + 1 then
  begin
    seNumPeriods.AsInteger := dgTime.RowCount -1;
  end;

  if Ord(tcStartTime) = ACol then
  begin
    UpdatePeriodLength(ARow);
    if ARow > 1 then
    begin
      dgTime.Cells[Ord(tcEndTime), ARow -1] := Value;
      UpdatePeriodLength(ARow-1);
    end;
  end;
  if Ord(tcEndTime) = ACol then
  begin
    UpdatePeriodLength(ARow);
    if ARow < dgTime.RowCount -1 then
    begin
      dgTime.Cells[Ord(tcStartTime), ARow +1] := Value;
      UpdatePeriodLength(ARow+1);
    end;
  end;
  if Ord(tcLength) = ACol then
  begin
    if not TryStrToFloat(dgTime.Cells[Ord(tcStartTime), ARow], StartTime) then
    begin
      StartTime := 0;
    end;
    if not TryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PeriodLength) then
    begin
      PeriodLength := 0;
    end;
    EndTime := StartTime + PeriodLength;
    dgTime.Cells[Ord(tcEndTime),ARow] := FloatToStr(EndTime);
    if ARow < dgTime.RowCount -1 then
    begin
      dgTime.Cells[Ord(tcStartTime), ARow +1] :=
        dgTime.Cells[Ord(tcEndTime),ARow];
      UpdatePeriodLength(ARow+1);
    end;
  end;
  UpdateNumberOfTimeSteps(ARow);
end;

procedure TfrmModflowTime.FormCreate(Sender: TObject);
var
  ColIndex: Integer;
  NewWidth: Integer;
begin
  inherited;
  FModflowStressPeriods:= TModflowStressPeriods.Create(nil);

  dgTime.ColWidths[Ord(tcTimeFirstStep)] := Round(dgTime.DefaultColWidth*1.6);

  dgTime.Cells[Ord(tcStressPeriod), 0] := StrStressPeriod;
  dgTime.Cells[Ord(tcStartTime), 0] := StrStartingTime;
  dgTime.Cells[Ord(tcEndTime), 0] := StrEndingTime;
  dgTime.Cells[Ord(tcLength), 0] := StrLength;
  dgTime.Cells[Ord(tcTimeFirstStep), 0] := StrMaxFirstTimeStep;
  dgTime.Cells[Ord(tcMultiplier), 0] := StrMultiplier;
  dgTime.Cells[Ord(tcSteady), 0] := StrSSTR;
  dgTime.Cells[Ord(tcDrawDownReference), 0] := StrDrawdownReference;
  dgTime.Cells[Ord(tcSteps), 0] := StrNumberOfSteps;

  lblPeriodLength.Caption := StrLength;
  lblMaxFirstTimeStepLength.Caption := StrMaxFirstTimeStep;
  lblMultiplier.Caption := StrMultiplier;
  lblSteadyTransient.Caption := StrSSTR;

  LayoutMultiRowEditControls;

  FillEmptyCells;

  frameGrid.Grid.Cells[Ord(mtcStartTime), 0] := StrStartingTime;
  frameGrid.Grid.Cells[Ord(mtcEndTime), 0] := StrEndingTime;
  if frmGoPhast.PhastModel.ModflowPackages.Mt3dmsGCGSolver.IsSelected then
  begin
    // Implicit
    frameGrid.Grid.Cells[Ord(mtcStepSize), 0] := StrInitialStepSize;
  end
  else
  begin
    // Explicit
    frameGrid.Grid.Cells[Ord(mtcStepSize), 0] := StrPreferredStepSize;
  end;
  frameGrid.Grid.Cells[Ord(mtcMaxSteps), 0] := StrMaximumnTransportS;
  frameGrid.Grid.Cells[Ord(mtcMultiplier), 0] := StrTimeStepMultiplier;
  frameGrid.Grid.Cells[Ord(mtcMaxStepSize), 0] := StrMaximumStepSize;
  frameGrid.Grid.Cells[Ord(mtcSteadyState), 0] := StrSteadyStateTranspo;

  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithStartTimes(frameGrid.Grid, Ord(mtcStartTime));
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillPickListWithEndTimes(frameGrid.Grid, Ord(mtcEndTime));

  GetData;
  SetDeleteButtonEnabled;
  NewWidth := 24;
  for ColIndex := 0 to dgTime.ColCount - 1 do
  begin
    NewWidth := NewWidth + dgTime.ColWidths[ColIndex];
  end;
  if NewWidth > Screen.Width then
  begin
    NewWidth := Screen.Width
  end;
  if NewWidth > ClientWidth then
  begin
    ClientWidth := NewWidth;
  end;


end;

procedure TfrmModflowTime.FormDestroy(Sender: TObject);
begin
  inherited;
  FModflowStressPeriods.Free;
end;

procedure TfrmModflowTime.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmModflowTime.frameGridGridBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  StepSize: double;
  MaxStepSize: double;
  Grid: TRbwDataGrid4;
begin
  inherited;
  if (ARow >= 1) and (TMt3dmsTimeColumns(ACol) in [mtcStepSize, mtcMaxStepSize]) then
  begin
    Grid := frameGrid.Grid;
    if TryStrToFloat(Grid.Cells[Ord(mtcStepSize), ARow], StepSize) and
      TryStrToFloat(Grid.Cells[Ord(mtcMaxStepSize), ARow], MaxStepSize) then
    begin
      if (StepSize > MaxStepSize) and (MaxStepSize <> 0) then
      begin
        Grid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TfrmModflowTime.frameGridGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Mt3dmsAdvection: TMt3dmsAdvection;
begin
  inherited;
  if (ARow >= frameGrid.Grid.FixedRows) and (ACol = Ord(mtcSteadyState)) then
  begin
    Mt3dmsAdvection := frmGoPhast.PhastModel.ModflowPackages.Mt3dmsAdvection;
    CanSelect := Mt3dmsAdvection.IsSelected
      and (Mt3dmsAdvection.AdvectionSolution = asStandard);
  end;
end;

{ TUndoModflowStressPeriods }

constructor TUndoModflowStressPeriods.Create(
  var NewStressPeriods: TModflowStressPeriods; NewTimeUnit: integer;
  var NewMt3dmsTimes: TMt3dmsTimeCollection);
var
  TimeIndex: Integer;
  OldSP: TModflowStressPeriod;
  NewSP: TModflowStressPeriod;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  NewScreenObject: TScreenObject;
begin
  inherited Create;
  FNewStressPeriods := NewStressPeriods;
  NewStressPeriods := nil;
  FOldStressPeriods:= TModflowStressPeriods.Create(nil);
  FOldStressPeriods.Assign(frmGoPhast.PhastModel.ModflowStressPeriods);

  FNewTimeUnit := NewTimeUnit;
  FOldTimeUnit := frmGoPhast.PhastModel.ModflowOptions.TimeUnit;

  FOldMt3dmsTimes := TMt3dmsTimeCollection.Create(nil);
  FOldMt3dmsTimes.Assign(frmGoPhast.PhastModel.Mt3dmsTimes);

  FNewMt3dmsTimes := NewMt3dmsTimes;
  NewMt3dmsTimes := nil;

  FOldTimes := TList<Double>.Create;
  FNewTimes := TList<Double>.Create;

  for TimeIndex := 0 to
    Min(FNewStressPeriods.Count, FOldStressPeriods.Count) -1 do
  begin
    OldSP := FOldStressPeriods[TimeIndex];
    NewSP := FNewStressPeriods[TimeIndex];
    if (OldSP.StartTime <> NewSP.StartTime)
      and (FOldTimes.IndexOf(OldSP.StartTime) < 0) then
    begin
      FOldTimes.Add(OldSP.StartTime);
      FNewTimes.Add(NewSP.StartTime);
    end;
    if (OldSP.EndTime <> NewSP.EndTime)
      and (FOldTimes.IndexOf(OldSP.EndTime) < 0) then
    begin
      FOldTimes.Add(OldSP.EndTime);
      FNewTimes.Add(NewSP.EndTime);
    end;
  end;

  FOldScreenObjects := TObjectList<TScreenObject>.Create;
  FExistingScreenObjects := TList<TScreenObject>.Create;

  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    for TimeIndex := 0 to FOldTimes.Count - 1 do
    begin
      if AScreenObject.UsesATime(FOldTimes[TimeIndex]) then
      begin
        FExistingScreenObjects.Add(AScreenObject);
        NewScreenObject := TScreenObjectClass(AScreenObject.ClassType).Create(nil);
        NewScreenObject.Assign(AScreenObject);
        FOldScreenObjects.Add(NewScreenObject);
        break;
      end;
    end;
  end;

end;

function TUndoModflowStressPeriods.Description: string;
begin
  result := StrChangeStressPeriod;
end;

destructor TUndoModflowStressPeriods.Destroy;
begin
  FExistingScreenObjects.Free;
  FOldScreenObjects.Free;
  FOldTimes.Free;
  FNewTimes.Free;
  FNewStressPeriods.Free;
  FOldStressPeriods.Free;
  FOldMt3dmsTimes.Free;
  FNewMt3dmsTimes.Free;
  inherited;
end;

procedure TUndoModflowStressPeriods.DoCommand;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  TimeIndex: Integer;
  OldCount: integer;
begin
  inherited;
  OldCount := frmGoPhast.PhastModel.ModflowStressPeriods.Count;
  frmGoPhast.PhastModel.ModflowStressPeriods := FNewStressPeriods;
  frmGoPhast.PhastModel.ModflowOptions.TimeUnit := FNewTimeUnit;
  frmGoPhast.PhastModel.Mt3dmsTimes := FNewMt3dmsTimes;
  UpdatedRequiredDataSets;

  if OldCount = frmGoPhast.PhastModel.ModflowStressPeriods.Count then
  begin
    for ScreenObjectIndex := 0 to FExistingScreenObjects.Count - 1 do
    begin
      AScreenObject := FExistingScreenObjects[ScreenObjectIndex];
      for TimeIndex := FOldTimes.Count - 1 downto 0 do
      begin
        if FOldTimes[TimeIndex] < FNewTimes[TimeIndex] then
        begin
          AScreenObject.ReplaceATime(FOldTimes[TimeIndex], FNewTimes[TimeIndex]);
        end;
      end;
      for TimeIndex := 0 to FOldTimes.Count - 1 do
      begin
        if FOldTimes[TimeIndex] > FNewTimes[TimeIndex] then
        begin
          AScreenObject.ReplaceATime(FOldTimes[TimeIndex], FNewTimes[TimeIndex]);
        end;
      end;
    end;
  end;
end;

procedure TUndoModflowStressPeriods.Undo;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowStressPeriods.Assign(FOldStressPeriods);
  frmGoPhast.PhastModel.ModflowOptions.TimeUnit := FOldTimeUnit;
  frmGoPhast.PhastModel.Mt3dmsTimes := FOldMt3dmsTimes;
  UpdatedRequiredDataSets;
  for ScreenObjectIndex := 0 to FExistingScreenObjects.Count - 1 do
  begin
    AScreenObject := FExistingScreenObjects[ScreenObjectIndex];
    AScreenObject.Assign(FOldScreenObjects[ScreenObjectIndex]);
  end;
end;

end.
