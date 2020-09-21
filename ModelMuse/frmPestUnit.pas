unit frmPestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  JvPageList, JvExControls, Vcl.ComCtrls, JvExComCtrls, JvPageListTreeView,
  ArgusDataEntry, PestPropertiesUnit, Vcl.Buttons, Vcl.ExtCtrls, UndoItems;

type
  TUndoPestOptions = class(TCustomUndo)
  private
    FOldPestProperties: TPestProperties;
    FNewPestProperties: TPestProperties;
  protected
    function Description: string; override;
    procedure UpdateProperties(PestProperties: TPestProperties);
  public
    constructor Create(var NewPestProperties: TPestProperties);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

  TfrmPEST = class(TfrmCustomGoPhast)
    tvPEST: TJvPageListTreeView;
    pgMain: TJvPageList;
    jvspBasic: TJvStandardPage;
    cbPEST: TCheckBox;
    rdePilotPointSpacing: TRbwDataEntry;
    lblPilotPointSpacing: TLabel;
    cbShowPilotPoints: TCheckBox;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    comboTemplateCharacter: TComboBox;
    lblTemplateCharacter: TLabel;
    comboFormulaMarker: TComboBox;
    lblFormulaMarker: TLabel;
    jvspControlDataMode: TJvStandardPage;
    cbSaveRestart: TCheckBox;
    lblPestMode: TLabel;
    comboPestMode: TComboBox;
    jvspDimensions: TJvStandardPage;
    rdeMaxCompDim: TRbwDataEntry;
    lblMaxCompDim: TLabel;
    rdeZeroLimit: TRbwDataEntry;
    lblZeroLimit: TLabel;
    jvspInversionControls: TJvStandardPage;
    rdeInitialLambda: TRbwDataEntry;
    lblInitialLambda: TLabel;
    rdeLambdaAdj: TRbwDataEntry;
    comboLambdaAdj: TLabel;
    rdeIterationClosure: TRbwDataEntry;
    lblIterationClosure: TLabel;
    rdeLambdaTermination: TRbwDataEntry;
    lblLambdaTermination: TLabel;
    rdeMaxLambdas: TRbwDataEntry;
    lblMaxLambdas: TLabel;
    rdeJacobianUpdate: TRbwDataEntry;
    lblJacobianUpdate: TLabel;
    cbLamForgive: TCheckBox;
    cbDerForgive: TCheckBox;
    jvspParameterAdjustmentControls: TJvStandardPage;
    rdeMaxRelParamChange: TRbwDataEntry;
    lblMaxRelParamChange: TLabel;
    rdeMaxFacParamChange: TRbwDataEntry;
    lblMaxFacParamChange: TLabel;
    rdeFactorOriginal: TRbwDataEntry;
    lblFactorOriginal: TLabel;
    rdeBoundStick: TRbwDataEntry;
    lblBoundStick: TLabel;
    cbParameterBending: TCheckBox;
    jvspInversionControls2: TJvStandardPage;
    rdeSwitchCriterion: TRbwDataEntry;
    lblSwitchCriterion: TLabel;
    rdeSwitchCount: TRbwDataEntry;
    lblSwitchCount: TLabel;
    rdeSplitSlopeCriterion: TRbwDataEntry;
    lblSplitSlopeCriterion: TLabel;
    comboAutomaticUserIntervation: TComboBox;
    lblAutomaticUserIntervation: TLabel;
    cbSensitivityReuse: TCheckBox;
    cbBoundsScaling: TCheckBox;
    jvspIterationControls: TJvStandardPage;
    procedure FormCreate(Sender: TObject); override;
    procedure MarkerChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
//    procedure btnOK1Click(Sender: TObject);

    { Public declarations }
  end;

var
  frmPEST: TfrmPEST;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

//procedure TfrmPEST.btnOK1Click(Sender: TObject);
//begin
//  inherited;
//  SetData;
//end;

procedure TfrmPEST.MarkerChange(Sender: TObject);
begin
  inherited;
  if comboTemplateCharacter.Text = comboFormulaMarker.Text then
  begin
    comboTemplateCharacter.Color := clRed;
    comboFormulaMarker.Color := clRed;
    Beep;
  end
  else
  begin
    comboTemplateCharacter.Color := clWindow;
    comboFormulaMarker.Color := clWindow;
  end;
end;

procedure TfrmPEST.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;

end;

procedure TfrmPEST.FormCreate(Sender: TObject);
var
  NewNode: TJvPageIndexNode;
  ControlDataNode: TJvPageIndexNode;
begin
  inherited;
  NewNode := tvPEST.Items.AddChild(
    nil, 'Basic') as TJvPageIndexNode;
  NewNode.PageIndex := jvspBasic.PageIndex;

  ControlDataNode := tvPEST.Items.AddChild(
    nil, 'Control Data') as TJvPageIndexNode;
  ControlDataNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Mode') as TJvPageIndexNode;
  NewNode.PageIndex := jvspControlDataMode.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Dimensions') as TJvPageIndexNode;
  NewNode.PageIndex := jvspDimensions.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Inversion Controls 1') as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Parameter Adjustment Controls') as TJvPageIndexNode;
  NewNode.PageIndex := jvspParameterAdjustmentControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Inversion Controls 2') as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls2.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Iteration Controls') as TJvPageIndexNode;
  NewNode.PageIndex := jvspIterationControls.PageIndex;

  pgMain.ActivePageIndex := 0;

  GetData
end;

procedure TfrmPEST.GetData;
var
  PestProperties: TPestProperties;
  PestControlData: TPestControlData;
begin
  PestProperties := frmGoPhast.PhastModel.PestProperties;

  cbPEST.Checked := PestProperties.PestUsed;
  comboTemplateCharacter.ItemIndex :=
    comboTemplateCharacter.Items.IndexOf(PestProperties.TemplateCharacter);

  comboFormulaMarker.ItemIndex :=
    comboFormulaMarker.Items.IndexOf(PestProperties.ExtendedTemplateCharacter);
  cbShowPilotPoints.Checked := PestProperties.ShowPilotPoints;
  rdePilotPointSpacing.RealValue := PestProperties.PilotPointSpacing;

  PestControlData := PestProperties.PestControlData;
  cbSaveRestart.Checked := Boolean(PestControlData.PestRestart);
  comboPestMode.ItemIndex := Ord(PestControlData.PestMode);

  rdeMaxCompDim.IntegerValue := PestControlData.MaxCompressionDimension;
  rdeZeroLimit.RealValue  := PestControlData.ZeroLimit;

  rdeInitialLambda.RealValue  := PestControlData.InitalLambda;
  rdeLambdaAdj.RealValue  := PestControlData.LambdaAdjustmentFactor;
  rdeIterationClosure.RealValue  := PestControlData.PhiRatioSufficient;
  rdeLambdaTermination.RealValue  := PestControlData.PhiReductionLambda;
  rdeMaxLambdas.IntegerValue := PestControlData.NumberOfLambdas;
  rdeJacobianUpdate.IntegerValue := PestControlData.JacobianUpdate;
  cbLamForgive.Checked := Boolean(PestControlData.LambdaForgive);
  cbDerForgive.Checked := Boolean(PestControlData.DerivedForgive);

  rdeMaxRelParamChange.RealValue  := PestControlData.RelativeMaxParamChange;
  rdeMaxFacParamChange.RealValue  := PestControlData.FactorMaxParamChange;
  rdeFactorOriginal.RealValue  := PestControlData.FactorOriginal;
  rdeBoundStick.IntegerValue  := PestControlData.BoundStick;
  cbParameterBending.Checked := Boolean(PestControlData.UpgradeParamVectorBending);

  rdeSwitchCriterion.RealValue := PestControlData.SwitchCriterion;
  rdeSwitchCount.IntegerValue := PestControlData.OptSwitchCount;
  rdeSwitchCriterion.RealValue := PestControlData.SplitSlopeCriterion;
  comboAutomaticUserIntervation.ItemIndex := Ord(PestControlData.AutomaticUserIntervation);
  cbSensitivityReuse.Checked := Boolean(PestControlData.SensitivityReuse);
  cbBoundsScaling.Checked := Boolean(PestControlData.Boundscaling);
end;

procedure TfrmPEST.SetData;
var
  PestProperties: TPestProperties;
  InvalidateModelEvent: TNotifyEvent;
  PestControlData: TPestControlData;
begin
  InvalidateModelEvent := nil;
  PestProperties := TPestProperties.Create(InvalidateModelEvent);
  try
    PestProperties.PestUsed := cbPEST.Checked;
    if comboTemplateCharacter.Text <> '' then
    begin
      PestProperties.TemplateCharacter := comboTemplateCharacter.Text[1];
    end;
    if comboFormulaMarker.Text <> '' then
    begin
      PestProperties.ExtendedTemplateCharacter := comboFormulaMarker.Text[1];
    end;
    PestProperties.ShowPilotPoints := cbShowPilotPoints.Checked;
    PestProperties.PilotPointSpacing := rdePilotPointSpacing.RealValue;

    PestControlData := PestProperties.PestControlData;
    PestControlData.PestRestart := TPestRestart(cbSaveRestart.Checked);
    PestControlData.PestMode := TPestMode(comboPestMode.ItemIndex);

    if rdeMaxCompDim.Text <> '' then
    begin
      PestControlData.MaxCompressionDimension := rdeMaxCompDim.IntegerValue;
    end;
    if rdeZeroLimit.Text <> '' then
    begin
      PestControlData.ZeroLimit := rdeZeroLimit.RealValue;
    end;


    if rdeInitialLambda.Text <> '' then
    begin
      PestControlData.InitalLambda := rdeInitialLambda.RealValue;
    end;
    if rdeLambdaAdj.Text <> '' then
    begin
      PestControlData.LambdaAdjustmentFactor := rdeLambdaAdj.RealValue;
    end;
    if rdeIterationClosure.Text <> '' then
    begin
      PestControlData.PhiRatioSufficient := rdeIterationClosure.RealValue;
    end;
    if rdeLambdaTermination.Text <> '' then
    begin
      PestControlData.PhiReductionLambda := rdeLambdaTermination.RealValue;
    end;
    if rdeMaxLambdas.Text <> '' then
    begin
      PestControlData.NumberOfLambdas := rdeMaxLambdas.IntegerValue;
    end;
    if rdeJacobianUpdate.Text <> '' then
    begin
      PestControlData.JacobianUpdate := rdeJacobianUpdate.IntegerValue;
    end;
   PestControlData.LambdaForgive := TLambdaForgive(cbLamForgive.Checked);
   PestControlData.DerivedForgive := TDerivedForgive(cbDerForgive.Checked);


    if rdeMaxRelParamChange.Text <> '' then
    begin
      PestControlData.RelativeMaxParamChange :=  rdeMaxRelParamChange.RealValue;
    end;
    if rdeMaxFacParamChange.Text <> '' then
    begin
      PestControlData.FactorMaxParamChange :=  rdeMaxFacParamChange.RealValue;
    end;
    if rdeFactorOriginal.Text <> '' then
    begin
      PestControlData.FactorOriginal :=  rdeFactorOriginal.RealValue;
    end;
    if rdeBoundStick.Text <> '' then
    begin
      PestControlData.BoundStick :=  rdeBoundStick.IntegerValue;
    end;
   PestControlData.UpgradeParamVectorBending :=
     TUpgradeParamVectorBending(cbLamForgive.Checked);

    if rdeSwitchCriterion.Text <> '' then
    begin
      PestControlData.SwitchCriterion := rdeSwitchCriterion.RealValue;
    end;
    if rdeSwitchCount.Text <> '' then
    begin
      PestControlData.OptSwitchCount := rdeSwitchCount.IntegerValue;
    end;
    if rdeSwitchCriterion.Text <> '' then
    begin
      PestControlData.SplitSlopeCriterion := rdeSwitchCriterion.RealValue;
    end;
    PestControlData.AutomaticUserIntervation := TAutomaticUserIntervation(comboAutomaticUserIntervation.ItemIndex);
    PestControlData.SensitivityReuse := TSensitivityReuse(cbSensitivityReuse.Checked);
    PestControlData.Boundscaling := TBoundsScaling(cbBoundsScaling.Checked);


    frmGoPhast.UndoStack.Submit(TUndoPestOptions.Create(PestProperties));
  finally
    PestProperties.Free
  end;

end;

{ TUndoPestOptions }

constructor TUndoPestOptions.Create(var NewPestProperties: TPestProperties);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  FOldPestProperties := TPestProperties.Create(InvalidateModelEvent);
  FOldPestProperties.Assign(frmGoPhast.PhastModel.PestProperties);
  FNewPestProperties := NewPestProperties;
  NewPestProperties := nil;
end;

function TUndoPestOptions.Description: string;
begin
  result := 'change PEST properties';
end;

destructor TUndoPestOptions.Destroy;
begin
  FOldPestProperties.Free;
  FNewPestProperties.Free;
  inherited;
end;

procedure TUndoPestOptions.DoCommand;
begin
  inherited;
//  frmGoPhast.PhastModel.PestProperties := FNewPestProperties;
  UpdateProperties(FNewPestProperties)
end;

procedure TUndoPestOptions.Undo;
begin
  inherited;
  UpdateProperties(FOldPestProperties)
end;

procedure TUndoPestOptions.UpdateProperties(PestProperties: TPestProperties);
var
  ShouldUpdateView: Boolean;
begin
  ShouldUpdateView := frmGoPhast.PhastModel.PestProperties.ShouldDrawPilotPoints
    <> PestProperties.ShouldDrawPilotPoints;
  frmGoPhast.PhastModel.PestProperties := PestProperties;
  if ShouldUpdateView then
  begin
    frmGoPhast.SynchronizeViews(vdTop);
//    frmGoPhast.PhastModel.Bitmaps.InvalidateView(vdTop);
  end;
end;

end.
