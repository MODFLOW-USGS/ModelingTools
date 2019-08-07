unit framePackageSwrUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController,
  StdCtrls, ComCtrls, JvExComCtrls, JvPageListTreeView, JvExControls,
  JvPageList, ExtCtrls, JvExExtCtrls, JvNetscapeSplitter, JvExStdCtrls,
  JvCombobox, JvListComb, ArgusDataEntry, Mask, JvExMask, JvSpin,
  ModflowPackageSelectionUnit;

type
  TframePackageSwr = class(TframePackage)
    jvplSwr: TJvPageList;
    tvpglstSwr: TJvPageListTreeView;
    jvspSolutionOptions: TJvStandardPage;
    jvspComments: TJvStandardPage;
    splttrSwr: TJvNetscapeSplitter;
    cbSwrOnly: TCheckBox;
    cbContinueNonConverge: TCheckBox;
    cbUpstreamWeighting: TCheckBox;
    cbInexactNewton: TCheckBox;
    cbUseSteadyStateStorage: TCheckBox;
    cbUseLaggedStagesAndFlows: TCheckBox;
    cbUseLinearDepthScaling: TCheckBox;
    comboScaling: TJvImageComboBox;
    lblScaling: TLabel;
    lblReordering: TLabel;
    comboReordering: TJvImageComboBox;
    comboNewton: TJvImageComboBox;
    lblNewton: TLabel;
    jvspTimeStepOptions: TJvStandardPage;
    rdeInitialTimeStepLength: TRbwDataEntry;
    lblInitialTimeStepLength: TLabel;
    rdeMinTimeStepLength: TRbwDataEntry;
    lblMinTimeStepLength: TLabel;
    rdeMaxTimeStepLength: TRbwDataEntry;
    lblMaxTimeStepLength: TLabel;
    rdeTimeStepMultiplier: TRbwDataEntry;
    lblTimeStepMultiplier: TLabel;
    seTimeStepIncreaseFrequency: TJvSpinEdit;
    lblTimeStepIncreaseFrequency: TLabel;
    rdeMinGradientForDiffusiveFlow: TRbwDataEntry;
    lblMinGradientForDiffusiveFlow: TLabel;
    rdeMinDepthForOutflow: TRbwDataEntry;
    lblMinDepthForOutflow: TLabel;
    rdeMaxRainfallForStepAdjustment: TRbwDataEntry;
    lblMaxRainfallForStepAdjustment: TLabel;
    rdeMaxStageChangePerStep: TRbwDataEntry;
    lblMaxStageChangePerStep: TLabel;
    rdeMaxInflowChange: TRbwDataEntry;
    lblMaxInflowChange: TLabel;
    jvspSpecificationMethod: TJvStandardPage;
    rgRainfallSpecification: TRadioGroup;
    rgEvapSpecification: TRadioGroup;
    rgStageSpecification: TRadioGroup;
    rgLateralInflowSpecification: TRadioGroup;
    jvspPrintOptions: TJvStandardPage;
    comboPrintInflowsAndOutflows: TJvImageComboBox;
    lblPrintInflowsAndOutflows: TLabel;
    comboPrintStage: TJvImageComboBox;
    lblPrintStage: TLabel;
    comboPrintReachExchangeAndProperties: TJvImageComboBox;
    lblPrintReachExchangeAndProperties: TLabel;
    comboPrintReachLateralFlow: TJvImageComboBox;
    lblPrintReachLateralFlow: TLabel;
    comboPrintStructureFlow: TJvImageComboBox;
    lblPrintStructureFlow: TLabel;
    cbPrintMaxFroude: TCheckBox;
    cbPrintSwrDataToScreen: TCheckBox;
    comboSaveSwrTimeStepLength: TJvImageComboBox;
    lblSaveSwrTimeStepLength: TLabel;
    cbSaveConvergenceHistory: TCheckBox;
    comboSaveRiver: TJvImageComboBox;
    lblSaveRiver: TLabel;
    lblSaveObs: TLabel;
    comboSaveObs: TJvImageComboBox;
    rdeSaveFrequency: TRbwDataEntry;
    lblSaveFrequency: TLabel;
    jvspSolverMandatory: TJvStandardPage;
    comboSolver: TJvImageComboBox;
    lblSolver: TLabel;
    seMaxOuterIterations: TJvSpinEdit;
    lblMaxOuterIterations: TLabel;
    seMaxInnerIterations: TJvSpinEdit;
    lblMaxInnerIterations: TLabel;
    seMaxLineSearchIterations: TJvSpinEdit;
    lblMaxLineSearchIterations: TLabel;
    rdeStageTolerance: TRbwDataEntry;
    lblStageTolerance: TLabel;
    comboFlowToleranceOption: TJvImageComboBox;
    lblFlowToleranceOption: TLabel;
    rdeFlowTolerance: TRbwDataEntry;
    lblFlowTolerance: TLabel;
    comboExchangeToleranceOption: TJvImageComboBox;
    lblExchangeToleranceOption: TLabel;
    rdeExchangeTolerance: TRbwDataEntry;
    lblExchangeTolerance: TLabel;
    rdeSteadyStateDampingFactor: TRbwDataEntry;
    lblSteadyStateDampingFactor: TLabel;
    rdeTransientDampingFactor: TRbwDataEntry;
    lblTransientDampingFactor: TLabel;
    seConvergencePrintoutInterval: TJvSpinEdit;
    lblConvergencePrintoutInterval: TLabel;
    comboPrintConvergence: TJvImageComboBox;
    lblPrintConvergence: TLabel;
    jvspSolverOptional: TJvStandardPage;
    comboPreconditioner: TJvImageComboBox;
    lblPreconditioner: TLabel;
    seMaxLevels: TJvSpinEdit;
    lblMaxLevels: TLabel;
    rdeDropThreshold: TRbwDataEntry;
    lblDropThreshold: TLabel;
    sePrintLineSearchInterval: TJvSpinEdit;
    lblPrintLineSearchInterval: TLabel;
    rdeAlternativeFlowTolerance: TRbwDataEntry;
    lblAlternativeFlowTolerance: TLabel;
    cbSaveAverageSimulatedResults: TCheckBox;
    comboObsFormat: TJvImageComboBox;
    lblObsFormat: TLabel;
    rgRainAssignmentMethod: TRadioGroup;
    rgEvapAssignmentMethod: TRadioGroup;
    rgLateralInflowAssignmentMethod: TRadioGroup;
    rgStageAssignmentMethod: TRadioGroup;
    grpSpecificationMethod: TGroupBox;
    grpAssignmentMethod: TGroupBox;
    procedure rdeMinTimeStepLengthChange(Sender: TObject);
    procedure rdeMaxTimeStepLengthChange(Sender: TObject);
    procedure rdeTimeStepMultiplierChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboSolverChange(Sender: TObject);
    procedure comboPreconditionerChange(Sender: TObject);
    procedure seMaxLineSearchIterationsChange(Sender: TObject);
    procedure comboFlowToleranceOptionChange(Sender: TObject);
    procedure comboSaveObsChange(Sender: TObject);
    procedure rgRainfallSpecificationClick(Sender: TObject);
    procedure rgEvapSpecificationClick(Sender: TObject);
    procedure rgLateralInflowSpecificationClick(Sender: TObject);
    procedure rgStageSpecificationClick(Sender: TObject);
  private
    FSwrPackage: TSwrPackage;
    { Private declarations }
    procedure MoveInheritedControls;
    procedure CreatePageLinks;
    procedure EnableSpecificationControls;
    procedure EnablePreconditioner;
    procedure EnableNlevelsAndDroptol;
    procedure EnableIbtprt;
    procedure EnablePtolr;
    procedure EnableDamping;
    procedure EnableObsFormat;
    procedure FillControlList;

  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure Loaded; override;
    { Public declarations }
  end;

var
  framePackageSwr: TframePackageSwr;

implementation

uses
  Generics.Collections, GoPhastTypes;

resourcestring
  StrComments = 'Comments';
  StrSolutionOptions = 'Solution options';
  StrTimeStepOptions = 'Time step options';
  StrSpecificationMethod = 'Specification method';
  StrPrintFlags = 'Print flags';
  StrSolverOptions1 = 'Solver Options 1';
  StrSolverOptions2 = 'Solver Options 2';

{$R *.dfm}

{ TframePackage1 }

procedure TframePackageSwr.comboFlowToleranceOptionChange(Sender: TObject);
begin
  inherited;
  EnablePtolr;
end;

procedure TframePackageSwr.comboPreconditionerChange(Sender: TObject);
begin
  inherited;
  EnableNlevelsAndDroptol;
end;

procedure TframePackageSwr.comboSaveObsChange(Sender: TObject);
begin
  inherited;
  EnableObsFormat;
end;

procedure TframePackageSwr.comboSolverChange(Sender: TObject);
begin
  inherited;
  EnablePreconditioner;
end;

procedure TframePackageSwr.CreatePageLinks;
var
  Node: TJvPageIndexNode;
begin
  tvpglstSwr.Items.Clear;
  Node := tvpglstSwr.Items.Add(nil, StrComments) as TJvPageIndexNode;
  Node.PageIndex := jvspComments.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrSolutionOptions) as TJvPageIndexNode;
  Node.PageIndex := jvspSolutionOptions.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrTimeStepOptions) as TJvPageIndexNode;
  Node.PageIndex := jvspTimeStepOptions.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrSpecificationMethod) as TJvPageIndexNode;
  Node.PageIndex := jvspSpecificationMethod.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrPrintFlags) as TJvPageIndexNode;
  Node.PageIndex := jvspPrintOptions.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrSolverOptions1) as TJvPageIndexNode;
  Node.PageIndex := jvspSolverMandatory.PageIndex;
  Node := tvpglstSwr.Items.Add(nil, StrSolverOptions2) as TJvPageIndexNode;
  Node.PageIndex := jvspSolverOptional.PageIndex;
end;

procedure TframePackageSwr.EnableDamping;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  rdeSteadyStateDampingFactor.Enabled := rcSelectionController.Enabled
    and (seMaxLineSearchIterations.AsInteger <= 1);
  rdeTransientDampingFactor.Enabled := rdeSteadyStateDampingFactor.Enabled;
end;

procedure TframePackageSwr.EnableIbtprt;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  sePrintLineSearchInterval.Enabled := rcSelectionController.Enabled
    and (seMaxLineSearchIterations.AsInteger > 1)
end;

procedure TframePackageSwr.EnableNlevelsAndDroptol;
var
  ShouldEnable: Boolean;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  ShouldEnable := comboPreconditioner.Enabled
    and (comboPreconditioner.ItemIndex = 4);
  seMaxLevels.Enabled := ShouldEnable;
  rdeDropThreshold.Enabled := ShouldEnable;
end;

procedure TframePackageSwr.EnableObsFormat;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  comboObsFormat.Enabled := rcSelectionController.Enabled
    and (comboSaveObs.ItemIndex > 0);
end;

procedure TframePackageSwr.EnablePreconditioner;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  comboPreconditioner.Enabled := rcSelectionController.Enabled
    and (comboSolver.ItemIndex > 0);
  EnableNlevelsAndDroptol;
end;

procedure TframePackageSwr.EnablePtolr;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  rdeAlternativeFlowTolerance.Enabled := rcSelectionController.Enabled
    and (comboFlowToleranceOption.ItemIndex > 0)
end;

procedure TframePackageSwr.EnableSpecificationControls;
var
  ShouldEnable: Boolean;
  RTMIN: double;
  RTMAX: double;
  RTMULT: double;
begin
  if csReading in ComponentState then
  begin
    Exit;
  end;
  ShouldEnable := False;
  if TryStrToFloat(rdeMinTimeStepLength.Text, RTMIN)
    and TryStrToFloat(rdeMaxTimeStepLength.Text, RTMAX)
    and TryStrToFloat(rdeTimeStepMultiplier.Text, RTMULT)
    then
  begin
    ShouldEnable := (RTMIN < RTMAX) and (RTMULT > 1);
  end;

  rdeMaxRainfallForStepAdjustment.Enabled := ShouldEnable;
  rdeMaxStageChangePerStep.Enabled := ShouldEnable;
  rdeMaxInflowChange.Enabled := ShouldEnable;

  rgRainfallSpecification.OnClick(rgRainfallSpecification);
  rgEvapSpecification.OnClick(rgEvapSpecification);
  rgLateralInflowSpecification.OnClick(rgLateralInflowSpecification);
  rgStageSpecification.OnClick(rgStageSpecification);

end;

procedure TframePackageSwr.FillControlList;
var
  List: TList<TControl>;
  index: Integer;
  AComponent: TComponent;
  AControl: TControl;
begin
  List := TList<TControl>.Create;
  try
    for Index := 0 to rcSelectionController.ControlList.Count - 1 do
    begin
      List.Add(rcSelectionController.ControlList[Index].Control);
    end;
    List.Add(rgEvapSpecification);
    List.Add(rgLateralInflowSpecification);
    List.Add(rgStageSpecification);
    List.Add(comboPreconditioner);
    List.Add(seMaxLevels);
    List.Add(rdeDropThreshold);
    List.Add(sePrintLineSearchInterval);
    List.Add(rdeAlternativeFlowTolerance);
    List.Add(rdeSteadyStateDampingFactor);
    List.Add(rdeTransientDampingFactor);
    List.Add(tvpglstSwr);

    for index := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[index];
      if AComponent is TControl then
      begin
        AControl := TControl(AComponent);
        if List.IndexOf(AControl) < 0 then
        begin
          (rcSelectionController.ControlList.Add
            as RbwController.TControlItem).Control := AControl;
        end;
      end;
    end;

  finally
    List.Free;
  end;
end;

procedure TframePackageSwr.GetData(Package: TModflowPackageSelection);
var
  CheckMinList: TList<TRbwDataEntry>;
  CheckMaxList: TList<TRbwDataEntry>;
  ComponentIndex: integer;
  AComponent: TComponent;
  RbwDataEntry: TRbwDataEntry;
begin
  inherited;
  CheckMinList := TList<TRbwDataEntry>.Create;
  CheckMaxList := TList<TRbwDataEntry>.Create;
  try
    for ComponentIndex := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[ComponentIndex];
      if AComponent is TRbwDataEntry then
      begin
        RbwDataEntry := TRbwDataEntry(AComponent);
        if RbwDataEntry.CheckMin then
        begin
          RbwDataEntry.CheckMin := False;
          CheckMinList.Add(RbwDataEntry);
        end;
        if RbwDataEntry.CheckMax then
        begin
          RbwDataEntry.CheckMax := False;
          CheckMaxList.Add(RbwDataEntry);
        end;
      end;
    end;
    FSwrPackage := Package as TSwrPackage;

    // Other
    rgRainAssignmentMethod.ItemIndex := Ord(FSwrPackage.RainAssignmentMethod);
    rgEvapAssignmentMethod.ItemIndex := Ord(FSwrPackage.EvapAssignmentMethod);
    rgLateralInflowAssignmentMethod.ItemIndex := Ord(FSwrPackage.LatInflowAssignmentMethod);
    rgStageAssignmentMethod.ItemIndex := Ord(FSwrPackage.StageAssignmentMethod);

    // Group 1: solution options
    cbSwrOnly.Checked := FSwrPackage.OnlyUseSWR;
    cbContinueNonConverge.Checked := FSwrPackage.ContinueDespiteNonConvergence;
    cbUpstreamWeighting.Checked := FSwrPackage.UseUpstreamWeightingForDiffusiveWave;
    cbInexactNewton.Checked := FSwrPackage.UseInexactNewton;
    cbUseSteadyStateStorage.Checked := FSwrPackage.UseSteadyStateStorage;
    cbUseLaggedStagesAndFlows.Checked := FSwrPackage.UseLaggedStagesAndFlows;
    cbUseLinearDepthScaling.Checked := FSwrPackage.UseLinearDepthScaling;
    comboScaling.ItemIndex := Ord(FSwrPackage.Scaling);
    comboReordering.ItemIndex := Ord(FSwrPackage.Reordering);
    comboNewton.ItemIndex := Ord(FSwrPackage.NewtonCorrection);

    // Group 2: Time step options
    rdeInitialTimeStepLength.RealValue := FSwrPackage.InitialTimeStepLength;
    rdeMinTimeStepLength.RealValue := FSwrPackage.MinTimeStepLength;
    rdeMaxTimeStepLength.RealValue := FSwrPackage.MaxTimeStepLength;
    rdeTimeStepMultiplier.RealValue := FSwrPackage.TimeStepMultiplier;
    seTimeStepIncreaseFrequency.AsInteger:= FSwrPackage.TimeStepIncreaseFrequency;
    rdeMinGradientForDiffusiveFlow.RealValue := FSwrPackage.MinGradientForDiffusiveFlow;
    rdeMinDepthForOutflow.RealValue := FSwrPackage.MinDepthForOutflow;
    rdeMaxRainfallForStepAdjustment.RealValue := FSwrPackage.MaxRainfallForStepAdjustment;
    rdeMaxStageChangePerStep.RealValue := FSwrPackage.MaxStageChangePerStep;
    rdeMaxInflowChange.RealValue := FSwrPackage.MaxInflowChange;

    // Group 3 Methods for specifying data, Data Set 5

    rgRainfallSpecification.ItemIndex := Ord(FSwrPackage.RainSpecification);
    rgRainfallSpecificationClick(nil);
    rgEvapSpecification.ItemIndex := Ord(FSwrPackage.EvapSpecification);
    rgEvapSpecificationClick(nil);
    rgLateralInflowSpecification.ItemIndex := Ord(FSwrPackage.LateralInflowSpecification);
    rgLateralInflowSpecificationClick(nil);
    rgStageSpecification.ItemIndex := Ord(FSwrPackage.StageSpecification);
    rgStageSpecificationClick(nil);

    // Group 4:   Print flags
    comboPrintInflowsAndOutflows.ItemIndex := Ord(FSwrPackage.PrintInflowsAndOutflows);
    comboPrintStage.ItemIndex := Ord(FSwrPackage.PrintStage);
    comboPrintReachExchangeAndProperties.ItemIndex := Ord(FSwrPackage.PrintReachExchangeAndProperties);
    comboPrintReachLateralFlow.ItemIndex := Ord(FSwrPackage.PrintReachLateralFlow);
    comboPrintStructureFlow.ItemIndex := Ord(FSwrPackage.PrintStructureFlow);
    cbPrintMaxFroude.Checked := FSwrPackage.PrintMaxFroude;
    cbPrintSwrDataToScreen.Checked := FSwrPackage.PrintSwrDataToScreen;
    comboSaveSwrTimeStepLength.ItemIndex := Ord(FSwrPackage.SaveSwrTimeStepLength);
    cbSaveAverageSimulatedResults.Checked := FSwrPackage.SaveAverageSimulatedResults;
    cbSaveConvergenceHistory.Checked := FSwrPackage.SaveConvergenceHistory;
    comboSaveRiver.ItemIndex := Ord(FSwrPackage.SaveRiver);
    comboSaveObs.ItemIndex := Ord(FSwrPackage.SaveObs);
    comboObsFormat.ItemIndex := Ord(FSwrPackage.ObsFormat);
    rdeSaveFrequency.RealValue := FSwrPackage.SaveFrequency;

    // Group 5: Solver

    comboSolver.ItemIndex := Ord(FSwrPackage.Solver);
    seMaxOuterIterations.AsInteger := FSwrPackage.MaxOuterIterations;
    seMaxInnerIterations.AsInteger := FSwrPackage.MaxInnerIterations;
    seMaxLineSearchIterations.AsInteger := FSwrPackage.MaxLineSearchIterations;
    rdeStageTolerance.RealValue := FSwrPackage.StageTolerance;
    comboFlowToleranceOption.ItemIndex := Ord(FSwrPackage.FlowToleranceOption);
    rdeFlowTolerance.RealValue := FSwrPackage.FlowTolerance;
    comboExchangeToleranceOption.ItemIndex := Ord(FSwrPackage.ExchangeToleranceOption);
    rdeExchangeTolerance.RealValue := FSwrPackage.ExchangeTolerance;
    rdeSteadyStateDampingFactor.RealValue := FSwrPackage.SteadyStateDampingFactor;
    rdeTransientDampingFactor.RealValue := FSwrPackage.TransientDampingFactor;
    seConvergencePrintoutInterval.AsInteger := FSwrPackage.ConvergencePrintoutInterval;
    comboPrintConvergence.ItemIndex := Ord(FSwrPackage.PrintConvergence);
    comboPreconditioner.ItemIndex := Ord(FSwrPackage.Preconditioner);
    seMaxLevels.AsInteger := FSwrPackage.MaxLevels;
    rdeDropThreshold.RealValue := FSwrPackage.DropThreshold;
    sePrintLineSearchInterval.AsInteger := FSwrPackage.PrintLineSearchInterval;
    rdeAlternativeFlowTolerance.RealValue := FSwrPackage.AlternativeFlowTolerance;

    rcSelectionControllerEnabledChange(nil);

    for ComponentIndex := 0 to CheckMinList.Count - 1 do
    begin
      CheckMinList[ComponentIndex].CheckMin := True;
    end;
    for ComponentIndex := 0 to CheckMaxList.Count - 1 do
    begin
      CheckMaxList[ComponentIndex].CheckMax := True;
    end;

  finally
    CheckMinList.Free;
    CheckMaxList.Free;
  end;

  rgRainfallSpecification.OnClick(rgRainfallSpecification);
  rgEvapSpecification.OnClick(rgEvapSpecification);
  rgLateralInflowSpecification.OnClick(rgLateralInflowSpecification);
  rgStageSpecification.OnClick(rgStageSpecification);
end;

procedure TframePackageSwr.Loaded;
begin
  inherited;
  MoveInheritedControls;
  CreatePageLinks;
  jvplSwr.ActivePageIndex := 0;
  FillControlList;
end;

procedure TframePackageSwr.MoveInheritedControls;
begin
//  lblPackage.Parent := jvspBasic;
//  memoComments.Parent := jvspBasic;
  memoComments.Width := jvspComments.ClientWidth - memoComments.Left -8;
  memoComments.Height := jvspComments.ClientHeight - memoComments.Top -8;
end;

procedure TframePackageSwr.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableSpecificationControls;
  EnablePreconditioner;
  EnableIbtprt;
  EnablePtolr;
  EnableDamping;
  EnableObsFormat;
end;

procedure TframePackageSwr.rdeMaxTimeStepLengthChange(Sender: TObject);
var
  RTMAX: double;
begin
  inherited;
  EnableSpecificationControls;
  if TryStrToFloat(rdeMaxTimeStepLength.Text, RTMAX) then
  begin
    rdeInitialTimeStepLength.Max := RTMAX;
  end;
end;

procedure TframePackageSwr.rdeMinTimeStepLengthChange(Sender: TObject);
var
  RTMIN: double;
begin
  inherited;
  if csReading in ComponentState then
  begin
    Exit;
  end;
  EnableSpecificationControls;
  if TryStrToFloat(rdeMinTimeStepLength.Text, RTMIN) then
  begin
    rdeInitialTimeStepLength.Min := RTMIN;
  end;
end;

procedure TframePackageSwr.rdeTimeStepMultiplierChange(Sender: TObject);
begin
  inherited;
  EnableSpecificationControls;
end;

procedure TframePackageSwr.rgEvapSpecificationClick(Sender: TObject);
begin
  inherited;
  rgEvapAssignmentMethod.Enabled := rcSelectionController.Enabled and
    (rgEvapSpecification.ItemIndex = 1);
end;

procedure TframePackageSwr.rgLateralInflowSpecificationClick(Sender: TObject);
begin
  inherited;
  rgLateralInflowAssignmentMethod.Enabled := rcSelectionController.Enabled and
    (rgLateralInflowSpecification.ItemIndex = 1);
end;

procedure TframePackageSwr.rgRainfallSpecificationClick(Sender: TObject);
begin
  inherited;
  rgRainAssignmentMethod.Enabled := rcSelectionController.Enabled
    and (rgRainfallSpecification.ItemIndex = 1);
end;

procedure TframePackageSwr.rgStageSpecificationClick(Sender: TObject);
begin
  inherited;
  rgStageAssignmentMethod.Enabled := rcSelectionController.Enabled and
    (rgStageSpecification.ItemIndex = 1);
end;

procedure TframePackageSwr.seMaxLineSearchIterationsChange(Sender: TObject);
begin
  inherited;
  EnableIbtprt;
  EnableDamping;
end;

procedure TframePackageSwr.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  FSwrPackage := Package as TSwrPackage;
    // Other
  FSwrPackage.EvapAssignmentMethod := TUpdateMethod(rgEvapAssignmentMethod.ItemIndex);
  FSwrPackage.RainAssignmentMethod := TUpdateMethod(rgRainAssignmentMethod.ItemIndex);
  FSwrPackage.LatInflowAssignmentMethod := TUpdateMethod(rgLateralInflowAssignmentMethod.ItemIndex);
  FSwrPackage.StageAssignmentMethod := TUpdateMethod(rgStageAssignmentMethod.ItemIndex);
  // Group 1: solution options
  FSwrPackage.OnlyUseSWR := cbSwrOnly.Checked;
  FSwrPackage.ContinueDespiteNonConvergence := cbContinueNonConverge.Checked;
  FSwrPackage.UseUpstreamWeightingForDiffusiveWave := cbUpstreamWeighting.Checked;
  FSwrPackage.UseInexactNewton := cbInexactNewton.Checked;
  FSwrPackage.UseSteadyStateStorage := cbUseSteadyStateStorage.Checked;
  FSwrPackage.UseLaggedStagesAndFlows := cbUseLaggedStagesAndFlows.Checked;
  FSwrPackage.UseLinearDepthScaling := cbUseLinearDepthScaling.Checked;
  FSwrPackage.Scaling := TSwrScaling(comboScaling.ItemIndex);
  FSwrPackage.Reordering := TSwrReordering(comboReordering.ItemIndex);
  FSwrPackage.NewtonCorrection := TSwrNewtonCorrection(comboNewton.ItemIndex);

  // Group 2: Time step options
  FSwrPackage.InitialTimeStepLength := rdeInitialTimeStepLength.RealValue;
  FSwrPackage.MinTimeStepLength := rdeMinTimeStepLength.RealValue;
  FSwrPackage.MaxTimeStepLength := rdeMaxTimeStepLength.RealValue;
  FSwrPackage.TimeStepMultiplier := rdeTimeStepMultiplier.RealValue;
  FSwrPackage.TimeStepIncreaseFrequency :=seTimeStepIncreaseFrequency.AsInteger;
  FSwrPackage.MinGradientForDiffusiveFlow := rdeMinGradientForDiffusiveFlow.RealValue;
  FSwrPackage.MinDepthForOutflow := rdeMinDepthForOutflow.RealValue;
  FSwrPackage.MaxRainfallForStepAdjustment := rdeMaxRainfallForStepAdjustment.RealValue;
  FSwrPackage.MaxStageChangePerStep := rdeMaxStageChangePerStep.RealValue;
  FSwrPackage.MaxInflowChange := rdeMaxInflowChange.RealValue;

  // Group 3 Methods for specifying data, Data Set 5

  FSwrPackage.RainSpecification := TSwrSpecificationMethod(rgRainfallSpecification.ItemIndex);
  FSwrPackage.EvapSpecification := TSwrSpecificationMethod(rgEvapSpecification.ItemIndex);
  FSwrPackage.LateralInflowSpecification := TSwrSpecificationMethod(rgLateralInflowSpecification.ItemIndex);
  FSwrPackage.StageSpecification := TSwrSpecificationMethod(rgStageSpecification.ItemIndex);

  // Group 4:   Print flags
  FSwrPackage.PrintInflowsAndOutflows := TSwrPrintOption(comboPrintInflowsAndOutflows.ItemIndex);
  FSwrPackage.PrintStage := TSwrPrintOption(comboPrintStage.ItemIndex);
  FSwrPackage.PrintReachExchangeAndProperties := TSwrPrintOption(comboPrintReachExchangeAndProperties.ItemIndex);
  FSwrPackage.PrintReachLateralFlow := TSwrPrintOption(comboPrintReachLateralFlow.ItemIndex);
  FSwrPackage.PrintStructureFlow := TSwrPrintOption(comboPrintStructureFlow.ItemIndex);
  FSwrPackage.PrintMaxFroude := cbPrintMaxFroude.Checked;
  FSwrPackage.PrintSwrDataToScreen := cbPrintSwrDataToScreen.Checked;
  FSwrPackage.SaveSwrTimeStepLength := TSwrPrintOption(comboSaveSwrTimeStepLength.ItemIndex);
  FSwrPackage.SaveAverageSimulatedResults := cbSaveAverageSimulatedResults.Checked;
  FSwrPackage.SaveConvergenceHistory := cbSaveConvergenceHistory.Checked;
  FSwrPackage.SaveRiver := TSwrSaveRiver(comboSaveRiver.ItemIndex);
  FSwrPackage.SaveObs := TSwrSaveObservations(comboSaveObs.ItemIndex);
  FSwrPackage.ObsFormat := TSwrObsFormat(comboObsFormat.ItemIndex);
  FSwrPackage.SaveFrequency := rdeSaveFrequency.RealValue;

  // Group 5: Solver

  FSwrPackage.Solver := TSwrSolver(comboSolver.ItemIndex);
  FSwrPackage.MaxOuterIterations := seMaxOuterIterations.AsInteger;
  FSwrPackage.MaxInnerIterations := seMaxInnerIterations.AsInteger;
  FSwrPackage.MaxLineSearchIterations := seMaxLineSearchIterations.AsInteger;
  FSwrPackage.StageTolerance := rdeStageTolerance.RealValue;
  FSwrPackage.FlowToleranceOption := TSwrFlowToleranceOption(comboFlowToleranceOption.ItemIndex);
  FSwrPackage.FlowTolerance := rdeFlowTolerance.RealValue;
  FSwrPackage.ExchangeToleranceOption := TSwrExchangeTolerance(comboExchangeToleranceOption.ItemIndex);
  FSwrPackage.ExchangeTolerance := rdeExchangeTolerance.RealValue;
  FSwrPackage.SteadyStateDampingFactor := rdeSteadyStateDampingFactor.RealValue;
  FSwrPackage.TransientDampingFactor := rdeTransientDampingFactor.RealValue;
  FSwrPackage.ConvergencePrintoutInterval := seConvergencePrintoutInterval.AsInteger;
  FSwrPackage.PrintConvergence := TSwrPrintConvergence(comboPrintConvergence.ItemIndex);
  FSwrPackage.Preconditioner := TSwrPreconditioner(comboPreconditioner.ItemIndex);
  FSwrPackage.MaxLevels := seMaxLevels.AsInteger;
  FSwrPackage.DropThreshold := rdeDropThreshold.RealValue;
  FSwrPackage.PrintLineSearchInterval := sePrintLineSearchInterval.AsInteger;
  FSwrPackage.AlternativeFlowTolerance := rdeAlternativeFlowTolerance.RealValue;
end;

end.
