unit frmSutraOutputControlUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, ComCtrls,
  JvExComCtrls, JvPageListTreeView, JvExControls, JvPageList, StdCtrls,
  Mask, JvExMask, JvSpin, CheckLst,
  Buttons, ExtCtrls, UndoItems, SutraOutputControlUnit;

type
  TfrmSutraOutputControl = class(TfrmCustomGoPhast)
    jvplMain: TJvPageList;
    jvpltvNavigator: TJvPageListTreeView;
    jvspListing: TJvStandardPage;
    seNprint: TJvSpinEdit;
    lblNprint: TLabel;
    jvspNodEle: TJvStandardPage;
    seNE_PrintFrequency: TJvSpinEdit;
    lblNE_PrintFrequency: TLabel;
    clbNcol: TCheckListBox;
    lblNcol: TLabel;
    jvspObsBound: TJvStandardPage;
    seNoblin: TJvSpinEdit;
    lblNoblin: TLabel;
    seNbcfpr: TJvSpinEdit;
    lblNbcfpr: TLabel;
    seNbcspr: TJvSpinEdit;
    lblNbcspr: TLabel;
    lblNbcppr: TLabel;
    seNbcppr: TJvSpinEdit;
    seNbcupr: TJvSpinEdit;
    lblNbcupr: TLabel;
    cbCinact: TCheckBox;
    chklstOptions: TCheckListBox;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    spl1: TSplitter;
    seGenFlowFrequency: TJvSpinEdit;
    lblGenFlowFrequency: TLabel;
    seGenTransportFrequency: TJvSpinEdit;
    lblGenTransportFrequency: TLabel;
    seLakeOutputCycle: TJvSpinEdit;
    lblLakeOutputCycle: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure jvpltvNavigatorCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoSutraOutputControl = class(TCustomUndo)
  private
    FOldOutputControl: TSutraOutputControl;
    FNewOutputControl: TSutraOutputControl;
  protected
    function Description: string; override;
  public
    constructor Create(var OC: TSutraOutputControl);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmSutraOutputControl: TfrmSutraOutputControl;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrListingFile = 'Listing File';
  StrNodAndEleFiles = 'Nod and Ele Files';
  StrObsAndBoundaryCon = 'Obs and Boundary Condition Files';

{$R *.dfm}

procedure TfrmSutraOutputControl.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSutraOutputControl.FormCreate(Sender: TObject);
var
  Node: TJvPageIndexNode;
begin
  inherited;
  Handle;
  jvpltvNavigator.Handle;
  jvpltvNavigator.Items.Clear;
  Node := jvpltvNavigator.Items.Add(nil, StrListingFile) as TJvPageIndexNode;
  Node.PageIndex := jvspListing.PageIndex;
  Node := jvpltvNavigator.Items.Add(Node, StrNodAndEleFiles) as TJvPageIndexNode;
  Node.PageIndex := jvspNodEle.PageIndex;
  Node := jvpltvNavigator.Items.Add(Node, StrObsAndBoundaryCon) as TJvPageIndexNode;
  Node.PageIndex := jvspObsBound.PageIndex;

  jvplMain.ActivePageIndex := 0;

  GetData;
end;

procedure TfrmSutraOutputControl.GetData;
var
  SutraOutputControl: TSutraOutputControl;
  OptIndex: TSutraListingOption;
  NodeElOptIndex: TSutraNodeEleOption;
begin
  clbNcol.ItemEnabled[Ord(neoLiquidSaturation)] := frmGoPhast.ModelSelection = msSutra40;
  clbNcol.ItemEnabled[Ord(neoIceSaturation)] := frmGoPhast.ModelSelection = msSutra40;
  clbNcol.ItemEnabled[Ord(neoDarcyVelocities)] := frmGoPhast.ModelSelection = msSutra40;

  SutraOutputControl := frmGoPhast.PhastModel.SutraOutputControl;
  seNprint.AsInteger := SutraOutputControl.ListingPrintFrequency;
  for OptIndex in SutraOutputControl.ListingOptions do
  begin
    chklstOptions.Checked[ord(OptIndex)] :=
      OptIndex in SutraOutputControl.ListingOptions;
  end;

  seNE_PrintFrequency.AsInteger := SutraOutputControl.NE_PrintFrequency;
  for NodeElOptIndex in SutraOutputControl.NodeElementOptions do
  begin
    clbNcol.Checked[ord(NodeElOptIndex)] :=
      NodeElOptIndex in SutraOutputControl.NodeElementOptions
  end;

  seNoblin.AsInteger := SutraOutputControl.MaxObsPerLine;
  seNbcfpr.AsInteger := SutraOutputControl.FluidSourcePrintFrequency;
  seNbcspr.AsInteger := SutraOutputControl.SoluteEnergySourcePrintFrequency;
  seNbcppr.AsInteger := SutraOutputControl.SpecifiedPressurePrintFrequency;
  seNbcupr.AsInteger := SutraOutputControl.SpecifiedConcTempPrintFrequency;
  seGenFlowFrequency.AsInteger := SutraOutputControl.GeneralizedFlowPrintFrequency;
  seGenTransportFrequency.AsInteger := SutraOutputControl.GeneralizedTransportPrintFrequency;
  seLakeOutputCycle.AsInteger := SutraOutputControl.LakePrintFrequency;

  seGenFlowFrequency.Enabled := frmGoPhast.ModelSelection <> msSutra22;
  seGenTransportFrequency.Enabled := frmGoPhast.ModelSelection <> msSutra22;
  seLakeOutputCycle.Enabled := frmGoPhast.ModelSelection <> msSutra22;

  cbCinact.Checked := SutraOutputControl.ListAll;
end;

procedure TfrmSutraOutputControl.jvpltvNavigatorCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TfrmSutraOutputControl.SetData;
var
  SutraOutputControl: TSutraOutputControl;
  OptIndex: TSutraListingOption;
  NodeElOptIndex: TSutraNodeEleOption;
  ListingOptions: TSutraListingOptions;
  NodeEleOptions: TSutraNodeEleOptions;
  Undo: TUndoSutraOutputControl;
begin
  SutraOutputControl := TSutraOutputControl.Create(nil);
  try
    SutraOutputControl.ListingPrintFrequency := seNprint.AsInteger;
    ListingOptions := [];
    for OptIndex := Low(TSutraListingOption) to High(TSutraListingOption) do
    begin
      if chklstOptions.Checked[Ord(OptIndex)] then
      begin
        Include(ListingOptions, OptIndex);
      end;
    end;
    SutraOutputControl.ListingOptions := ListingOptions;

    SutraOutputControl.NE_PrintFrequency := seNE_PrintFrequency.AsInteger;


    NodeEleOptions := [];
    for NodeElOptIndex := Low(TSutraNodeEleOption) to High(TSutraNodeEleOption) do
    begin
      if clbNcol.Checked[Ord(NodeElOptIndex)] then
      begin
        Include(NodeEleOptions, NodeElOptIndex);
      end;
    end;
    SutraOutputControl.NodeElementOptions := NodeEleOptions;

    SutraOutputControl.MaxObsPerLine := seNoblin.AsInteger;
    SutraOutputControl.FluidSourcePrintFrequency := seNbcfpr.AsInteger;
    SutraOutputControl.SoluteEnergySourcePrintFrequency := seNbcspr.AsInteger;
    SutraOutputControl.SpecifiedPressurePrintFrequency := seNbcppr.AsInteger;
    SutraOutputControl.SpecifiedConcTempPrintFrequency := seNbcupr.AsInteger;
    SutraOutputControl.GeneralizedFlowPrintFrequency := seGenFlowFrequency.AsInteger;
    SutraOutputControl.GeneralizedTransportPrintFrequency := seGenTransportFrequency.AsInteger;
    SutraOutputControl.LakePrintFrequency := seLakeOutputCycle.AsInteger;

    SutraOutputControl.ListAll := cbCinact.Checked;

    Undo := TUndoSutraOutputControl.Create(SutraOutputControl);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    SutraOutputControl.Free;
  end;
end;

{ TUndoSutraOutputControl }

constructor TUndoSutraOutputControl.Create(var OC: TSutraOutputControl);
begin
  FOldOutputControl := TSutraOutputControl.Create(nil);
  FOldOutputControl.Assign(frmGoPhast.PhastModel.SutraOutputControl);

  FNewOutputControl := OC;
  OC := nil;
end;

function TUndoSutraOutputControl.Description: string;
begin
  result := 'edit SUTRA output control options';
end;

destructor TUndoSutraOutputControl.Destroy;
begin
  FOldOutputControl.Free;
  FNewOutputControl.Free;
  inherited;
end;

procedure TUndoSutraOutputControl.DoCommand;
begin
  frmGoPhast.PhastModel.SutraOutputControl := FNewOutputControl;
  inherited;
end;

procedure TUndoSutraOutputControl.Undo;
begin
  frmGoPhast.PhastModel.SutraOutputControl := FOldOutputControl;
  inherited;
end;

end.
