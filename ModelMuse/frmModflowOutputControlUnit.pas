unit frmModflowOutputControlUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, JvPageList,
  JvExControls, ComCtrls, JvExComCtrls, JvPageListTreeView,
  JvExStdCtrls, JvCheckBox, JvExExtCtrls, JvRadioGroup, frameOutputControlUnit,
  ArgusDataEntry, Mask, JvExMask, JvSpin, JvCombobox, JvListComb,
  JvNetscapeSplitter, RbwDataGrid4, frameGridUnit, UndoItems,
  ModflowOutputControlUnit;

type
  TfrmModflowOutputControl = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pltrPageNavigator: TJvPageListTreeView;
    jvPages: TJvPageList;
    jvspGeneral: TJvStandardPage;
    jvspHeads: TJvStandardPage;
    cbPrintInputArrays: TJvCheckBox;
    rgSaveCellFlows: TJvRadioGroup;
    cbPrintInputCellLists: TJvCheckBox;
    jvspDrawdown: TJvStandardPage;
    frameDrawdown: TframeOutputControl;
    frameHead: TframeOutputControl;
    cbCompact: TJvCheckBox;
    memoComments: TMemo;
    Comments: TLabel;
    jvspBudget: TJvStandardPage;
    comboFrequency: TJvImageComboBox;
    lblN: TLabel;
    spN: TJvSpinEdit;
    lblFrequency: TLabel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    lblBudget: TLabel;
    jvspMT3DMS: TJvStandardPage;
    cbMt3dSaveConc: TCheckBox;
    frameMt3dmsTimes: TframeGrid;
    comboSaveMt3msResults: TComboBox;
    spinMt3dmsPrintN: TJvSpinEdit;
    lblMt3dmsPrintN: TLabel;
    lblMt3dmsPrintWhen: TLabel;
    spinMt3dmsPrintObs: TJvSpinEdit;
    lblMt3dmsPrintConc: TLabel;
    lblMt3dMsPrintMassBalance: TLabel;
    spinMt3dMsPrintMassBalance: TJvSpinEdit;
    cbSummarizeMassBalance: TCheckBox;
    cbPrintObservations: TCheckBox;
    comboOutputSuppression: TJvImageComboBox;
    lblOutputSuppression: TLabel;
    cbSaveBudgetSummary: TCheckBox;
    jvspGwt: TJvStandardPage;
    frameGWT: TframeOutputControl;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure jvPagesChange(Sender: TObject);
    procedure comboSaveMt3msResultsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure pltrPageNavigatorCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FOutputControl: TModflowOutputControl;
    FMt3dmsOutputControl: TMt3dmsOutputControl;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoOutputControl = class(TCustomUndo)
  private
    FOldModflowOutputControl: TModflowOutputControl;
    FNewModflowOutputControl: TModflowOutputControl;
    FOldMt3dmsOutputControl: TMt3dmsOutputControl;
    FNewMt3dmsOutputControl: TMt3dmsOutputControl;
  protected
    function Description: string; override;
  public
    constructor Create(var NewModflowOutputControl: TModflowOutputControl;
      var NewMt3dmsOutputControl: TMt3dmsOutputControl);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModflowOutputControl: TfrmModflowOutputControl;

implementation

uses frmGoPhastUnit, Mt3dmsTimesUnit, frmErrorsAndWarningsUnit, GoPhastTypes;

resourcestring
  StrChangeOutputContro = 'change output control';
  StrOutputTimesTIMPRS = 'Output times (TIMPRS)';
  StrNotAvailableInM = ' (not available in MODFLOW 6)';

{$R *.dfm}

{ TfrmModflowOutputControl }

procedure TfrmModflowOutputControl.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;

  frmGoPhast.PhastModel.ModpathHeadWarning;
  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmModflowOutputControl.comboSaveMt3msResultsChange(Sender: TObject);
begin
  inherited;
  frameMt3dmsTimes.Enabled := comboSaveMt3msResults.ItemIndex = 0;
  spinMt3dmsPrintN.Enabled := comboSaveMt3msResults.ItemIndex = 2;
end;

procedure TfrmModflowOutputControl.FormCreate(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  frameDrawdown.ParentFont := True;
  frameHead.ParentFont := True;
  // Create the links between nodes and pages in code because
  // they are too easily disrupted by editing the nodes in the form designer.
  for Index := 0 to pltrPageNavigator.Items.Count -1 do
  begin
    (pltrPageNavigator.Items[Index] as TJvPageIndexNode).PageIndex := Index;
  end;
  jvPages.ActivePage := jvspGeneral;

  frameMt3dmsTimes.Grid.Cells[0,0] := StrOutputTimesTIMPRS;

  GetData;
end;

procedure TfrmModflowOutputControl.FormDestroy(Sender: TObject);
begin
  inherited;
  FOutputControl.Free;
  FMt3dmsOutputControl.Free;
end;

procedure TfrmModflowOutputControl.GetData;
var
  OutputControl: TModflowOutputControl;
  index: Integer;
  Mt3dmsOutputControl: TMt3dmsOutputControl;
  TimeItem: TOuptputTimeItem;
begin
  // MODFLOW
  OutputControl := frmGoPhast.PhastModel.ModflowOutputControl;
  cbPrintInputArrays.Checked := OutputControl.PrintInputArrays;
  rgSaveCellFlows.ItemIndex := Ord(OutputControl.SaveCellFlows);
  cbPrintInputCellLists.Checked := OutputControl.PrintInputCellLists;
  cbPrintObservations.Checked := OutputControl.PrintObservations;
  cbCompact.Checked := OutputControl.Compact;
  frameHead.GetData(OutputControl.HeadOC);
  frameDrawdown.GetData(OutputControl.DrawdownOC);
  frameGWT.GetData(OutputControl.ConcentrationOC);
  MemoComments.Lines.Assign(OutputControl.Comments);
  cbSaveBudgetSummary.Checked := OutputControl.SaveBudgetSummary;
  cbSaveBudgetSummary.Enabled := frmGoPhast.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ];

  comboFrequency.ItemIndex := Ord(OutputControl.BudgetFrequencyChoice);
  spN.AsInteger := OutputControl.BudgetFrequency;

  comboOutputSuppression.ItemIndex := Ord(OutputControl.OutputSuppression);
  comboOutputSuppression.Enabled := frmGoPhast.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
          ];

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    frameDrawdown.lblOutputType.Caption := frameDrawdown.lblOutputType.Caption
      + StrNotAvailableInM;
  end;

  // MT3DMS
  Mt3dmsOutputControl := frmGoPhast.PhastModel.Mt3dmsOutputControl;
  cbMt3dSaveConc.Checked := Mt3dmsOutputControl.SaveConcentrations;
  comboSaveMt3msResults.ItemIndex := Ord(Mt3dmsOutputControl.OutputFreqChoice);
  spinMt3dmsPrintN.AsInteger := Mt3dmsOutputControl.PeriodicOutputCount;
  spinMt3dmsPrintObs.AsInteger := Mt3dmsOutputControl.ObservationFrequency;
  spinMt3dMsPrintMassBalance.AsInteger := Mt3dmsOutputControl.MassBalanceFrequency;
  cbSummarizeMassBalance.Checked := Mt3dmsOutputControl.SummarizeMassBalance;

  frameMt3dmsTimes.seNumber.AsInteger := Mt3dmsOutputControl.OutputTimes.Count;
  for index := 0 to Mt3dmsOutputControl.OutputTimes.Count-1 do
  begin
    TimeItem := Mt3dmsOutputControl.OutputTimes[index];
    frameMt3dmsTimes.Grid.Cells[0, index+1] := FloatToStr(TimeItem.ObservationTime);
  end;
  comboSaveMt3msResultsChange(nil);
end;

procedure TfrmModflowOutputControl.jvPagesChange(Sender: TObject);
begin
  inherited;
  HelpKeyWord := jvPages.ActivePage.HelpKeyword;
end;

procedure TfrmModflowOutputControl.pltrPageNavigatorCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TfrmModflowOutputControl.SetData;
var
  Index: integer;
  TimeItem: TOuptputTimeItem;
  AValue: Extended;
  Undo: TUndoOutputControl;
begin
  FOutputControl := TModflowOutputControl.Create(nil);

  FOutputControl.PrintInputArrays := cbPrintInputArrays.Checked;
  FOutputControl.SaveCellFlows := TCellSaveFormat(rgSaveCellFlows.ItemIndex);
  FOutputControl.PrintInputCellLists := cbPrintInputCellLists.Checked;
  FOutputControl.PrintObservations := cbPrintObservations.Checked;
  FOutputControl.Compact := cbCompact.Checked;
  frameHead.SetData(FOutputControl.HeadOC);
  frameDrawdown.SetData(FOutputControl.DrawdownOC);
  frameGWT.SetData(FOutputControl.ConcentrationOC);
  FOutputControl.Comments.Assign(MemoComments.Lines);
  FOutputControl.BudgetFrequencyChoice :=
    TFrequencyChoice(comboFrequency.ItemIndex);
  FOutputControl.BudgetFrequency := spN.AsInteger;
  FOutputControl.OutputSuppression :=
    TOutputSuppression(comboOutputSuppression.ItemIndex);
  FOutputControl.SaveBudgetSummary := cbSaveBudgetSummary.Checked;

  FMt3dmsOutputControl:= TMt3dmsOutputControl.Create(nil);
  FMt3dmsOutputControl.SaveConcentrations := cbMt3dSaveConc.Checked;
  FMt3dmsOutputControl.OutputFreqChoice := TMt3dmsOutputFreq(comboSaveMt3msResults.ItemIndex);
  FMt3dmsOutputControl.PeriodicOutputCount := spinMt3dmsPrintN.AsInteger;
  FMt3dmsOutputControl.ObservationFrequency := spinMt3dmsPrintObs.AsInteger;
  FMt3dmsOutputControl.MassBalanceFrequency := spinMt3dMsPrintMassBalance.AsInteger;
  FMt3dmsOutputControl.SummarizeMassBalance := cbSummarizeMassBalance.Checked;

  for index := 0 to frameMt3dmsTimes.seNumber.AsInteger-1 do
  begin
    if TryStrToFloat(frameMt3dmsTimes.Grid.Cells[0, index+1], AValue) then
    begin
      TimeItem := FMt3dmsOutputControl.OutputTimes.Add;
      TimeItem.ObservationTime := AValue
    end;
  end;

  Undo := TUndoOutputControl.Create(FOutputControl, FMt3dmsOutputControl);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoOutputControl }

constructor TUndoOutputControl.Create(
  var NewModflowOutputControl: TModflowOutputControl;
  var NewMt3dmsOutputControl: TMt3dmsOutputControl);
begin
  FOldModflowOutputControl := TModflowOutputControl.Create(nil);
  FOldModflowOutputControl.Assign(frmGoPhast.PhastModel.ModflowOutputControl);
  FNewModflowOutputControl := NewModflowOutputControl;
  NewModflowOutputControl := nil;

  FOldMt3dmsOutputControl := TMt3dmsOutputControl.Create(nil);
  FOldMt3dmsOutputControl.Assign(frmGoPhast.PhastModel.Mt3dmsOutputControl);
  FNewMt3dmsOutputControl := NewMt3dmsOutputControl;
  NewMt3dmsOutputControl := nil;
end;

function TUndoOutputControl.Description: string;
begin
  Result := StrChangeOutputContro;
end;

destructor TUndoOutputControl.Destroy;
begin
  FNewMt3dmsOutputControl.Free;
  FOldMt3dmsOutputControl.Free;
  FNewModflowOutputControl.Free;
  FOldModflowOutputControl.Free;
  inherited;
end;

procedure TUndoOutputControl.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowOutputControl := FNewModflowOutputControl;
  frmGoPhast.PhastModel.Mt3dmsOutputControl := FNewMt3dmsOutputControl;
end;

procedure TUndoOutputControl.Undo;
begin
  frmGoPhast.PhastModel.ModflowOutputControl := FOldModflowOutputControl;
  frmGoPhast.PhastModel.Mt3dmsOutputControl := FOldMt3dmsOutputControl;
end;

end.
