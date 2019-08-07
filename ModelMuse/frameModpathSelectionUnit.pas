unit frameModpathSelectionUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ArgusDataEntry, ModflowPackageSelectionUnit, ExtCtrls, Buttons,
  Grids, RbwDataGrid4, Mask, JvExMask, JvSpin, JvGroupBox, ComCtrls, GrayTabs;

type
  TframeModpathSelection = class(TframePackage)
    pcModpath: TPageControl;
    tabResponse: TTabSheet;
    comboTrackingDirection: TJvImageComboBox;
    lblTrackingDirection: TLabel;
    lblWeakSinkTreatment: TLabel;
    comboWeakSinkTreatment: TJvImageComboBox;
    rdeWeakSinkThreshold: TRbwDataEntry;
    lblWeakSinkThreshold: TLabel;
    cbStopInZone: TCheckBox;
    rdeStopZone: TRbwDataEntry;
    lblStopZone: TLabel;
    comboWhichEndpoints: TJvImageComboBox;
    lblWhichEndpoints: TLabel;
    rgOutputMode: TRadioGroup;
    lblReferenceTime: TLabel;
    rdeReferenceTime: TRbwDataEntry;
    tabOutputTimes: TTabSheet;
    gbTime: TJvGroupBox;
    lblTimeCount: TLabel;
    sbAddRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbDeleteRow: TSpeedButton;
    rdgTimes: TRbwDataGrid4;
    seTimeCount: TJvSpinEdit;
    comboTimeMethod: TJvImageComboBox;
    lblTimeMethod: TLabel;
    rdeParticleInterval: TRbwDataEntry;
    lblParticleInterval: TLabel;
    lblMaxTimes: TLabel;
    rdeMaxTimes: TRbwDataEntry;
    tsVersion6Options: TTabSheet;
    lblWeakSource: TLabel;
    comboWeakSource: TJvImageComboBox;
    lblStopOption: TLabel;
    comboStopOption: TJvImageComboBox;
    lblStopTime: TLabel;
    rdeStopTime: TRbwDataEntry;
    lblBudget: TLabel;
    comboBudget: TJvImageComboBox;
    lblTraceID: TLabel;
    rdeTraceID: TRbwDataEntry;
    chkRetardation: TCheckBox;
    lblAdvObs: TLabel;
    comboAdvObs: TJvImageComboBox;
    tabVersion5Options: TTabSheet;
    cbCompact: TCheckBox;
    cbBinary: TCheckBox;
    rdeBeginningTime: TRbwDataEntry;
    rdeEndingTime: TRbwDataEntry;
    rdeMaxSize: TRbwDataEntry;
    lblBeginningTime: TLabel;
    lblEndingTime: TLabel;
    lblMaxSize: TLabel;
    lblErrorTolerance: TLabel;
    rdeErrorTolerance: TRbwDataEntry;
    cbComputeBudget: TCheckBox;
    cbSummarize: TCheckBox;
    cbBigBudget: TCheckBox;
    cbStopAfterMaxTime: TCheckBox;
    lblMaxTime: TLabel;
    rdeMaxTime: TRbwDataEntry;
    lblReleaseTime: TLabel;
    rdeReleaseTime: TRbwDataEntry;
    rgModpathVersion: TRadioGroup;
    comboEvtSink: TJvImageComboBox;
    lblEvtSink: TLabel;
    comboRchSource: TJvImageComboBox;
    lblRchSource: TLabel;
    comboUzfIface: TJvImageComboBox;
    lblUzfIface: TLabel;
    lblMnw2Iface: TLabel;
    comboMnw2Iface: TJvImageComboBox;
    lblResIface: TLabel;
    comboResIface: TJvImageComboBox;
    lblSfrIface: TLabel;
    comboSfrIface: TJvImageComboBox;
    lblEtsIface: TLabel;
    comboEtsIface: TJvImageComboBox;
    lblLakIface: TLabel;
    comboLakIface: TJvImageComboBox;
    procedure seTimeCountChange(Sender: TObject);
    procedure rdgTimesEndUpdate(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure rgOutputModeClick(Sender: TObject);
    procedure rdgTimesBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure cbStopAfterMaxTimeClick(Sender: TObject);
    procedure comboWeakSinkTreatmentChange(Sender: TObject);
    procedure cbStopInZoneClick(Sender: TObject);
    procedure cbComputeBudgetClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboTimeMethodChange(Sender: TObject);
    procedure comboTrackingDirectionChange(Sender: TObject);
    procedure rgModpathVersionClick(Sender: TObject);
    procedure comboStopOptionChange(Sender: TObject);
    procedure comboBudgetChange(Sender: TObject);
  private
    FGettingData: Boolean;
    FActiveMPath: Boolean;
    procedure UpdateTimes;
    procedure SetTimeControlsEnabled;
    procedure EnableStopZone;
    procedure EnableMaxTime;
    procedure EnableSinkThreshold;
    procedure EnableErrorTolerance;
    procedure UpdateMaxTimes;
    procedure EnableWhichEndPointsRecorded;
    procedure EnableTimeControls;
    procedure EnableBeginAndEndTime;
    procedure EnablePathPlusTimeStep;
    procedure SetActiveMPath(const Value: Boolean);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    Property ActiveMPath: Boolean read FActiveMPath write SetActiveMPath;
    { Public declarations }
  end;

var
  frameModpathSelection: TframeModpathSelection;

implementation

uses
  ModpathParticleUnit, frmGoPhastUnit, ModflowTimeUnit, GoPhastTypes;

resourcestring
  StrN = 'N';
  StrTime = 'Time';
  StrStopComputingPathsMax = 'Stop computing paths after a specified maximum' +
  ' time';
  StrMaximumTime = 'Maximum time';
  StrStopComputingPathsSpec = 'Stop computing paths after a specified tracki' +
  'ng time';
  StrMaximumTrackingTim = 'Maximum tracking time';
  StrResponseFileOption = 'Response file options';
  StrSimulationFileOpti = 'Simulation file options';
  StrStopIfExitingFlow = 'Stop if exiting flow fraction is above threshold';
  StrYouHaveSpecifiedA = 'You have specified a stopping zone for particles t' +
  'hat is less than 2. The stopping zone is required to be greater than or e' +
  'qual to 2.';

{$R *.dfm}

{ TframeModpathSelection }

procedure TframeModpathSelection.cbComputeBudgetClick(Sender: TObject);
begin
  inherited;
  EnableErrorTolerance;
end;

procedure TframeModpathSelection.cbStopAfterMaxTimeClick(Sender: TObject);
begin
  inherited;
  EnableMaxTime;
end;

procedure TframeModpathSelection.cbStopInZoneClick(Sender: TObject);
begin
  inherited;
  EnableStopZone;
  EnableWhichEndPointsRecorded;
end;

procedure TframeModpathSelection.comboBudgetChange(Sender: TObject);
begin
  inherited;
  rdeTraceID.Enabled := (comboBudget.ItemIndex = 3)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.comboStopOptionChange(Sender: TObject);
begin
  inherited;
  rdeStopTime.Enabled := (comboStopOption.ItemIndex = 2)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.comboTimeMethodChange(Sender: TObject);
begin
  inherited;
  SetTimeControlsEnabled;
  UpdateMaxTimes;
end;

procedure TframeModpathSelection.comboTrackingDirectionChange(Sender: TObject);
begin
  inherited;
  EnableTimeControls;
end;

procedure TframeModpathSelection.comboWeakSinkTreatmentChange(Sender: TObject);
begin
  inherited;
  EnableSinkThreshold;
end;

constructor TframeModpathSelection.Create(AOwner: TComponent);
begin
  inherited;
  rdgTimes.Cells[0,0] := StrN;
  rdgTimes.Cells[1,0] := StrTime;
end;

procedure TframeModpathSelection.GetData(Package: TModflowPackageSelection);
var
  ModpathSource: TModpathSelection;
  Index: Integer;
  Item: TModpathTimeItem;
  StressPeriod: TModflowStressPeriod;
begin
//  rgOutputMode.Buttons[Ord(mopPathAndTime)].Enabled := False;
  FGettingData := True;
  try
    inherited;
    ModpathSource := Package as TModpathSelection;
    pcModpath.ActivePageIndex := 0;
    if not frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
    begin
      cbStopAfterMaxTime.Caption :=
        StrStopComputingPathsMax;
      lblMaxTime.Caption := StrMaximumTime;
    end
    else
    begin
      cbStopAfterMaxTime.Caption :=
        StrStopComputingPathsSpec;
      lblMaxTime.Caption := StrMaximumTrackingTim;
    end;
    rgModpathVersion.ItemIndex := Ord(ModpathSource.MpathVersion);
    rgModpathVersionClick(nil);

    comboTrackingDirection.ItemIndex := Ord(ModpathSource.TrackingDirection);
    EnableTimeControls;
    EnableBeginAndEndTime;

    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[0];
    if rdeBeginningTime.RealValue < StressPeriod.StartTime then
    begin
      rdeBeginningTime.RealValue := StressPeriod.StartTime;
    end;
    if rdeEndingTime.RealValue < StressPeriod.StartTime then
    begin
      rdeEndingTime.RealValue := StressPeriod.StartTime;
    end;
    rdeBeginningTime.Min := StressPeriod.StartTime;
    rdeEndingTime.Min := StressPeriod.StartTime;
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[
      frmGoPhast.PhastModel.ModflowStressPeriods.Count-1];
    if rdeBeginningTime.RealValue > StressPeriod.EndTime then
    begin
      rdeBeginningTime.RealValue := StressPeriod.EndTime;
    end;
    if rdeEndingTime.RealValue > StressPeriod.EndTime then
    begin
      rdeEndingTime.RealValue := StressPeriod.EndTime;
    end;
    rdeBeginningTime.Max := StressPeriod.EndTime;
    rdeEndingTime.Max := StressPeriod.EndTime;

    rdgTimes.FixedCols := 1;

    rdeMaxSize.Text := IntToStr(ModpathSource.MaximumSize);
    comboEvtSink.ItemIndex := Ord(ModpathSource.EVT_Sink);
    comboRchSource.ItemIndex := Ord(ModpathSource.RCH_Source);
    cbCompact.Checked := ModpathSource.Compact;
    cbBinary.Checked := ModpathSource.Binary;
    rdeBeginningTime.Text := FloatToStr(ModpathSource.BeginningTime);
    rdeEndingTime.Text := FloatToStr(ModpathSource.EndingTime);
    rdeReferenceTime.Text := FloatToStr(ModpathSource.ReferenceTime);
    rgOutputMode.ItemIndex := Ord(ModpathSource.OutputMode);
    SetTimeControlsEnabled;
    seTimeCount.AsInteger := ModpathSource.OutputTimes.Count;
    UpdateTimes;
    for Index := 0 to ModpathSource.OutputTimes.Count - 1 do
    begin
      Item := ModpathSource.OutputTimes.Items[Index] as TModpathTimeItem;
      rdgTimes.Cells[1,Index+1] := FloatToStr(Item.Time);
    end;

    cbStopAfterMaxTime.Checked := ModpathSource.StopAfterMaxTime;
    rdeMaxTime.Text := FloatToStr(ModpathSource.MaxTime);
    comboWeakSinkTreatment.ItemIndex := Ord(ModpathSource.WeakSink);
    rdeWeakSinkThreshold.Text := FloatToStr(ModpathSource.WeakSinkThreshold);
    cbStopInZone.Checked := ModpathSource.StopInZone;
    rdeStopZone.Text := IntToStr(ModpathSource.StopZoneNumber);
    comboWhichEndpoints.ItemIndex := Ord(ModpathSource.EndpointWrite);
    cbComputeBudget.Checked := ModpathSource.ComputeBudgetInAllCells;
    rdeErrorTolerance.Text := FloatToStr(ModpathSource.ErrorTolerance);
    cbSummarize.Checked := ModpathSource.Summarize;
    cbBigBudget.Checked := ModpathSource.MakeBigBudgetFile;

    comboTimeMethod.ItemIndex := Ord(ModpathSource.TimeSeriesMethod);
    rdeParticleInterval.Text := FloatToStr(ModpathSource.TimeSeriesInterval);
    rdeMaxTimes.Text := IntToStr(ModpathSource.TimeSeriesMaxCount);

    rdeReleaseTime.Text := FloatToStr(ModpathSource.BackwardsTrackingReleaseTime);

    // version 6
    comboWeakSource.ItemIndex := Ord(ModpathSource.WeakSource);
    comboStopOption.ItemIndex := Ord(ModpathSource.StopOption);
    rdeStopTime.Text := FloatToStr(ModpathSource.StopTime);
    comboBudget.ItemIndex := Ord(ModpathSource.BudgetChecking);
    rdeTraceID.Text := IntToStr(ModpathSource.TraceID);
    chkRetardation.Checked := ModpathSource.RetardationOption = roUsed;
    comboAdvObs.ItemIndex := Ord(ModpathSource.AdvectiveObservations);

    comboEtsIface.ItemIndex := Ord(ModpathSource.Ets_Sink);
    comboUzfIface.ItemIndex := Ord(ModpathSource.Uzf_Source);
    comboMnw2Iface.ItemIndex := Ord(ModpathSource.Mnw2_Source);
    comboResIface.ItemIndex := Ord(ModpathSource.Res_Source);
    comboSfrIface.ItemIndex := Ord(ModpathSource.Sfr_Source);
    comboLakIface.ItemIndex := Ord(ModpathSource.Lak_Source);

    comboStopOptionChange(nil);

    EnableErrorTolerance;
    EnableMaxTime;
    EnableSinkThreshold;
    EnableStopZone;
    SetTimeControlsEnabled;
    EnableWhichEndPointsRecorded;
  finally
    FGettingData := False;
  end;
  EnablePathPlusTimeStep;
end;

procedure TframeModpathSelection.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableBeginAndEndTime;
//  rdeBeginningTime.Enabled := rcSelectionController.Enabled
//    and frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;
//  rdeEndingTime.Enabled := rdeBeginningTime.Enabled;

  EnableTimeControls;
  SetTimeControlsEnabled;
  EnableStopZone;
  EnableMaxTime;
  EnableSinkThreshold;
  EnableErrorTolerance;
  EnableWhichEndPointsRecorded;
  comboStopOptionChange(nil);
  comboBudgetChange(nil);
  rgOutputModeClick(nil);
  rgModpathVersionClick(nil);
end;

procedure TframeModpathSelection.rdgTimesBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  if not rdgTimes.Enabled then
  begin
    rdgTimes.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeModpathSelection.rdgTimesEndUpdate(Sender: TObject);
begin
  inherited;
  if seTimeCount <> nil then
  begin
    seTimeCount.AsInteger := rdgTimes.RowCount-1;
    UpdateTimes;
  end;
end;

procedure TframeModpathSelection.rgModpathVersionClick(Sender: TObject);
begin
  inherited;
  case rgModpathVersion.ItemIndex of
    0:
      begin
        tabResponse.Caption := StrResponseFileOption;
        tsVersion6Options.TabVisible := False;
        tabVersion5Options.TabVisible := True;
        while comboWeakSinkTreatment.Items.Count < 3 do
        begin
          comboWeakSinkTreatment.Items.Add;
        end;

        comboWeakSinkTreatment.Items[2].Text := StrStopIfExitingFlow;

        cbStopInZone.Caption := 'Stop particles entering a particular zone';
        end;
    1,2:
      begin
        tabResponse.Caption := StrSimulationFileOpti;
        tsVersion6Options.TabVisible := True;
        tabVersion5Options.TabVisible := False;
        while comboWeakSinkTreatment.Items.Count > 2 do
        begin
          comboWeakSinkTreatment.Items.Delete(
            comboWeakSinkTreatment.Items.Count-1);
        end;
        if comboWeakSinkTreatment.ItemIndex < 0 then
        begin
          comboWeakSinkTreatment.ItemIndex := 1;
        end;
        cbStopInZone.Caption := 'Use zone arrays (ZoneArrayOption)';
      end;
    else
      Assert(False);
  end;
  cbCompact.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  cbBinary.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  rdeMaxSize.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  cbComputeBudget.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  cbSummarize.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  cbBigBudget.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  cbStopAfterMaxTime.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
  EnableBeginAndEndTime;
  EnableWhichEndPointsRecorded;
  EnablePathPlusTimeStep;


end;

procedure TframeModpathSelection.rgOutputModeClick(Sender: TObject);
begin
  inherited;
  SetTimeControlsEnabled;
  EnableWhichEndPointsRecorded;
  comboAdvObs.Enabled := (rgOutputMode.ItemIndex in [2, 3])
    and rcSelectionController.Enabled;
//  EnableStopZone;
  UpdateMaxTimes;
end;

procedure TframeModpathSelection.sbAddRowClick(Sender: TObject);
begin
  inherited;
  seTimeCount.AsInteger := seTimeCount.AsInteger + 1;
  UpdateTimes;
end;

procedure TframeModpathSelection.sbDeleteRowClick(Sender: TObject);
begin
  inherited;
  if rdgTimes.Row >= 1 then
  begin
    if seTimeCount.AsInteger > 1 then
    begin
      rdgTimes.DeleteRow(rdgTimes.Row);
    end
    else
    begin
      rdgTimes.Cells[1,1] := '';
    end;
    seTimeCount.AsInteger := seTimeCount.AsInteger - 1;
    UpdateTimes;
  end;
end;

procedure TframeModpathSelection.sbInsertRowClick(Sender: TObject);
begin
  inherited;
  if rdgTimes.Row >= 1 then
  begin
    rdgTimes.InsertRow(rdgTimes.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger + 1;
    UpdateTimes;
  end;
end;

procedure TframeModpathSelection.SetActiveMPath(const Value: Boolean);
begin
  FActiveMPath := Value;
  if FActiveMPath and (frmGoPhast.ModelSelection = msModflow2015) then
  begin
    rgModpathVersion.ItemIndex := Ord(mp7);
  end;
end;

procedure TframeModpathSelection.SetData(Package: TModflowPackageSelection);
var
  ModpathSource: TModpathSelection;
  Index: Integer;
  Item: TModpathTimeItem;
  ItemIndex: Integer;
  ATime: double;
begin
  inherited;
  ModpathSource := Package as TModpathSelection;
  if ModpathSource.IsSelected
    and (rgModpathVersion.ItemIndex = 0)
    and cbStopInZone.Checked
    and (rdeStopZone.IntegerValue <= 1) then
  begin
    Beep;
    MessageDlg(StrYouHaveSpecifiedA, mtWarning, [mbOK], 0);
  end;

  ModpathSource.MpathVersion := TMpathVersion(rgModpathVersion.ItemIndex);

  ModpathSource.MaximumSize := StrToInt(rdeMaxSize.Text);
  ModpathSource.EVT_Sink :=
    TSurfaceApplicationPosition(comboEvtSink.ItemIndex);
  ModpathSource.RCH_Source :=
    TSurfaceApplicationPosition(comboRchSource.ItemIndex);
  ModpathSource.Compact := cbCompact.Checked;
  ModpathSource.Binary := cbBinary.Checked;
  ModpathSource.BeginningTime := StrToFloat(rdeBeginningTime.Text);
  ModpathSource.EndingTime := StrToFloat(rdeEndingTime.Text);
  ModpathSource.ReferenceTime := StrToFloat(rdeReferenceTime.Text);
  ModpathSource.OutputMode := TModpathOutputMode(rgOutputMode.ItemIndex);
  ItemIndex := 0;
  for Index := 0 to seTimeCount.AsInteger - 1 do
  begin
    if TryStrToFloat(rdgTimes.Cells[1,Index+1], ATime) then
    begin
      while ItemIndex >= ModpathSource.OutputTimes.Count do
      begin
        ModpathSource.OutputTimes.Add;
      end;
      Item := ModpathSource.OutputTimes.Items[ItemIndex] as TModpathTimeItem;
      Item.Time := ATime;
      Inc(ItemIndex);
    end;
  end;
  While ModpathSource.OutputTimes.Count > ItemIndex do
  begin
    ModpathSource.OutputTimes.Delete(ModpathSource.OutputTimes.Count-1);
  end;

  ModpathSource.StopAfterMaxTime := cbStopAfterMaxTime.Checked;
  ModpathSource.MaxTime := StrToFloat(rdeMaxTime.Text);
  ModpathSource.TrackingDirection := TTrackingDirection(comboTrackingDirection.ItemIndex);
  ModpathSource.WeakSink := TWeakSink(comboWeakSinkTreatment.ItemIndex);
  ModpathSource.WeakSinkThreshold := StrToFloat(rdeWeakSinkThreshold.Text);
  ModpathSource.StopInZone := cbStopInZone.Checked;
  ModpathSource.StopZoneNumber := StrToInt(rdeStopZone.Text);
  ModpathSource.EndpointWrite := TEndpointWrite(comboWhichEndpoints.ItemIndex);
  ModpathSource.ComputeBudgetInAllCells := cbComputeBudget.Checked;
  ModpathSource.ErrorTolerance := StrToFloat(rdeErrorTolerance.Text);
  ModpathSource.Summarize := cbSummarize.Checked;
  ModpathSource.MakeBigBudgetFile := cbBigBudget.Checked;

  ModpathSource.TimeSeriesMethod := TTimeSeriesMethod(comboTimeMethod.ItemIndex);
  ModpathSource.TimeSeriesInterval := StrToFloat(rdeParticleInterval.Text);
  ModpathSource.TimeSeriesMaxCount := StrToInt(rdeMaxTimes.Text);
  ModpathSource.BackwardsTrackingReleaseTime := StrToFloat(rdeReleaseTime.Text);
  // version 6
  ModpathSource.WeakSource := TWeakSink(comboWeakSource.ItemIndex);
  ModpathSource.StopOption := TStopOption(comboStopOption.ItemIndex);
  ModpathSource.StopTime := StrTofloat(rdeStopTime.Text);
  ModpathSource.BudgetChecking := TBudgetChecking(comboBudget.ItemIndex);
  ModpathSource.TraceID := StrToInt(rdeTraceID.Text);
  ModpathSource.RetardationOption := TRetardationOption(chkRetardation.Checked);
  ModpathSource.AdvectiveObservations := TAdvectiveObservations(comboAdvObs.ItemIndex);
  ModpathSource.Ets_Sink := TSurfaceApplicationPosition(comboEtsIface.ItemIndex);
  ModpathSource.Uzf_Source := TSurfaceApplicationPosition(comboUzfIface.ItemIndex);
  ModpathSource.Mnw2_Source := TSurfaceApplicationPosition(comboMnw2Iface.ItemIndex);
  ModpathSource.Res_Source := TSurfaceApplicationPosition(comboResIface.ItemIndex);
  ModpathSource.Sfr_Source := TSurfaceApplicationPosition(comboSfrIface.ItemIndex);
  ModpathSource.Lak_Source := TSurfaceApplicationPosition(comboLakIface.ItemIndex);
end;

procedure TframeModpathSelection.EnableBeginAndEndTime;
begin
  rdeBeginningTime.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0)
    and frmGoPhast.PhastModel.ModflowStressPeriods.CompletelyTransient;
  rdeEndingTime.Enabled := rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0)
    and frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;
end;

procedure TframeModpathSelection.EnableTimeControls;
begin

  rdeReferenceTime.Enabled := rcSelectionController.Enabled
    and ((rgModpathVersion.ItemIndex in [1,2])
    or (frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel
    and (comboTrackingDirection.ItemIndex = 0)));
  rdeReleaseTime.Enabled := rcSelectionController.Enabled
    and frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel
    and (comboTrackingDirection.ItemIndex in [1,2]);
end;

procedure TframeModpathSelection.EnableWhichEndPointsRecorded;
begin
  comboWhichEndpoints.Enabled := cbStopInZone.Checked
    and (rgOutputMode.ItemIndex = 0) and rcSelectionController.Enabled
    and (rgModpathVersion.ItemIndex = 0);
end;

procedure TframeModpathSelection.UpdateMaxTimes;
begin
  if not FGettingData
    and (rgOutputMode.ItemIndex in [2,3]) // time series
    and (comboTimeMethod.ItemIndex = 0) // individual times
    and (StrToInt(rdeMaxTimes.Text) = 0) then
  begin
    rdeMaxTimes.Text := '1000';
  end;
end;

procedure TframeModpathSelection.EnableErrorTolerance;
begin
  rdeErrorTolerance.Enabled := cbComputeBudget.Checked
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnableSinkThreshold;
begin
  rdeWeakSinkThreshold.Enabled :=
    (comboWeakSinkTreatment.ItemIndex = 2)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnableMaxTime;
begin
  rdeMaxTime.Enabled := cbStopAfterMaxTime.Checked
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnablePathPlusTimeStep;
begin
  if not FGettingData then
  begin
    rgOutputMode.Buttons[Ord(mopPathAndTime)].Enabled :=
      rgModpathVersion.ItemIndex = 2;
    if not rgOutputMode.Buttons[Ord(mopPathAndTime)].Enabled
      and (rgOutputMode.ItemIndex = Ord(mopPathAndTime)) then
    begin
      rgOutputMode.ItemIndex := Ord(mopPathline)
    end;
  end;
end;

procedure TframeModpathSelection.EnableStopZone;
begin
  rdeStopZone.Enabled := cbStopInZone.Checked
//    and (rgOutputMode.ItemIndex = 0)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.SetTimeControlsEnabled;
begin
  seTimeCount.Enabled := (rgOutputMode.ItemIndex in [1, 2, 3])
    and (comboTimeMethod.ItemIndex = 1)
    and rcSelectionController.Enabled;
  rdgTimes.Enabled := seTimeCount.Enabled;
  if seTimeCount.Enabled then
  begin
    seTimeCount.Color := clWindow;
  end
  else
  begin
    seTimeCount.Color := clBtnFace;
  end;
  rdgTimes.Color := seTimeCount.Color;
  sbAddRow.Enabled := seTimeCount.Enabled;
  sbInsertRow.Enabled := seTimeCount.Enabled;
  UpdateTimes;

  comboTimeMethod.Enabled := (rgOutputMode.ItemIndex in [1, 2, 3])
    and rcSelectionController.Enabled;
  rdeParticleInterval.Enabled := (rgOutputMode.ItemIndex in [1, 2, 3])
    and (comboTimeMethod.ItemIndex = 0)
    and rcSelectionController.Enabled;
  rdeMaxTimes.Enabled := rdeParticleInterval.Enabled;
end;

procedure TframeModpathSelection.UpdateTimes;
var
  Index: Integer;
begin
  if (seTimeCount.AsInteger = 0) then
  begin
    rdgTimes.Enabled := False;
    rdgTimes.RowCount := 2;
  end
  else
  begin
    rdgTimes.Enabled :=  seTimeCount.Enabled;
    rdgTimes.RowCount := seTimeCount.AsInteger + 1;
  end;
  sbDeleteRow.Enabled := seTimeCount.Enabled
    and (seTimeCount.AsInteger > 0);
  for Index := 1 to rdgTimes.RowCount - 1 do
  begin
    rdgTimes.Cells[0,Index] := IntToStr(Index);
  end;
end;

procedure TframeModpathSelection.seTimeCountChange(Sender: TObject);
begin
  inherited;
  UpdateTimes;
end;

end.
