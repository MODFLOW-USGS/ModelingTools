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
    jvspPilotPoints: TJvStandardPage;
    rdePilotPointSpacing: TRbwDataEntry;
    lblPilotPointSpacing: TLabel;
    cbShowPilotPoints: TCheckBox;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edTemplateCharacter: TLabeledEdit;
    edFormulaMarker: TLabeledEdit;
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
var
  Ed: TLabeledEdit;
  AChar: Char;
  OK: Boolean;
begin
  inherited;
  OK := False;
  Ed := Sender as TLabeledEdit;
  if Ed.Text <> '' then
  begin
    AChar := Ed.Text[1];
    OK :=  CharInSet(AChar, ['@', '#', '$', '%', '?'])
      and (edTemplateCharacter.Text <> edFormulaMarker.Text);
  end;
  if OK then
  begin
    Ed.Brush.Color := clWindow;
  end
  else
  begin
    Ed.Brush.Color := clRed;
  end;
  Ed.Invalidate;
end;

procedure TfrmPEST.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;

end;

procedure TfrmPEST.FormCreate(Sender: TObject);
var
  NewNode: TJvPageIndexNode;
begin
  inherited;
  NewNode := tvPEST.Items.AddChild(
    nil, 'Basic') as TJvPageIndexNode;
  NewNode.PageIndex := jvspBasic.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, 'Pilot Points') as TJvPageIndexNode;
  NewNode.PageIndex := jvspPilotPoints.PageIndex;

  pgMain.ActivePageIndex := 0;

  GetData
end;

procedure TfrmPEST.GetData;
var
  PestProperties: TPestProperties;
begin
  PestProperties := frmGoPhast.PhastModel.PestProperties;

  cbPEST.Checked := PestProperties.PestUsed;
  edTemplateCharacter.Text := PestProperties.TemplateCharacter;
  edFormulaMarker.Text := PestProperties.ExtendedTemplateCharacter;
  cbShowPilotPoints.Checked := PestProperties.ShowPilotPoints;
  rdePilotPointSpacing.RealValue := PestProperties.PilotPointSpacing;
end;

procedure TfrmPEST.SetData;
var
  PestProperties: TPestProperties;
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  PestProperties := TPestProperties.Create(InvalidateModelEvent);
  try
    PestProperties.PestUsed := cbPEST.Checked;
    if edTemplateCharacter.Text <> '' then
    begin
      PestProperties.TemplateCharacter := edTemplateCharacter.Text[1];
    end;
    if edFormulaMarker.Text <> '' then
    begin
      PestProperties.ExtendedTemplateCharacter := edFormulaMarker.Text[1];
    end;
    PestProperties.ShowPilotPoints := cbShowPilotPoints.Checked;
    PestProperties.PilotPointSpacing := rdePilotPointSpacing.RealValue;

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
