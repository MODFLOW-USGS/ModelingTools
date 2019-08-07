unit frmCustomizeMeshUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Buttons,
  StdCtrls, DisplaySettingsUnit, UndoItems;

type
  TUndoSutraMeshDisplay = class(TCustomUndo)
  private
    FOldSutraSettings: TSutraSettings;
    FNewSutraSettings: TSutraSettings;
    procedure ForceRedraw;
  protected
    function Description: string; override;
  public
    constructor Create(var NewSettings: TSutraSettings);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmCustomizeMesh = class(TfrmCustomGoPhast)
    cbShowNodeNumbers: TCheckBox;
    cbShowElementNumbers: TCheckBox;
    btnEditNodeFont: TButton;
    btnEditElementFont: TButton;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgFont: TFontDialog;
    cbNodeCellOutline: TCheckBox;
    cbShowElements: TCheckBox;
    cbShowElementCenters: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnEditNodeFontClick(Sender: TObject);
    procedure btnEditElementFontClick(Sender: TObject);
  private
    FSutraSettings: TSutraSettings;
    procedure SetData;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

var
  frmCustomizeMesh: TfrmCustomizeMesh = nil;

implementation

uses
  frmGoPhastUnit, SutraMeshUnit, GoPhastTypes;

{$R *.dfm}

procedure TfrmCustomizeMesh.btnEditElementFontClick(Sender: TObject);
begin
  inherited;
  dlgFont.Font := FSutraSettings.ElementFont;
  if dlgFont.Execute then
  begin
    FSutraSettings.ElementFont := dlgFont.Font;
  end;
end;

procedure TfrmCustomizeMesh.btnEditNodeFontClick(Sender: TObject);
begin
  inherited;
  dlgFont.Font := FSutraSettings.NodeFont;
  if dlgFont.Execute then
  begin
    FSutraSettings.NodeFont := dlgFont.Font;
  end;
end;

procedure TfrmCustomizeMesh.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  GetData;
end;

procedure TfrmCustomizeMesh.FormDestroy(Sender: TObject);
begin
  inherited;
  FSutraSettings.Free;
end;

procedure TfrmCustomizeMesh.GetData;
var
  Mesh: TSutraMesh3D;
begin
  FSutraSettings.Free;
  FSutraSettings := TSutraSettings.Create(nil);
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  cbShowNodeNumbers.Checked := Mesh.DrawNodeNumbers;
  cbShowElementNumbers.Checked := Mesh.DrawElementNumbers;
  cbNodeCellOutline.Checked := Mesh.NodeDrawingChoice = dcAll;
  cbShowElements.Checked := Mesh.ElementDrawingChoice = dcAll;
  cbShowElementCenters.Checked := Mesh.DrawElementCenters;
  FSutraSettings.Assign(Mesh);
end;

procedure TfrmCustomizeMesh.SetData;
var
  Undo: TUndoSutraMeshDisplay;
begin
  FSutraSettings.ShowNodeNumbers := cbShowNodeNumbers.Checked;
  FSutraSettings.ShowElementNumbers := cbShowElementNumbers.Checked;
  FSutraSettings.DrawElementCenters := cbShowElementCenters.Checked;
  if cbNodeCellOutline.Checked then
  begin
    FSutraSettings.NodeDrawingChoice := dcAll;
  end
  else
  begin
    FSutraSettings.NodeDrawingChoice := dcEdge;
  end;
  if cbShowElements.Checked then
  begin
    FSutraSettings.ElementDrawingChoice := dcAll;
  end
  else
  begin
    FSutraSettings.ElementDrawingChoice := dcEdge;
  end;
  Undo := TUndoSutraMeshDisplay.Create(FSutraSettings);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoSutraMeshDisplay }

constructor TUndoSutraMeshDisplay.Create(var NewSettings: TSutraSettings);
begin
  FOldSutraSettings := TSutraSettings.Create(nil);
  FOldSutraSettings.Assign(frmGoPhast.PhastModel.SutraMesh);
  FNewSutraSettings := NewSettings;
  NewSettings := nil;
end;

function TUndoSutraMeshDisplay.Description: string;
begin
  result := 'customize SUTRA mesh';
end;

destructor TUndoSutraMeshDisplay.Destroy;
begin
  FOldSutraSettings.Free;
  FNewSutraSettings.Free;
  inherited;
end;

procedure TUndoSutraMeshDisplay.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SutraSettings := FNewSutraSettings;
//  frmGoPhast.PhastModel.SutraMesh.Assign(FNewSutraSettings);
  ForceRedraw;
end;

procedure TUndoSutraMeshDisplay.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SutraSettings := FOldSutraSettings;
//  frmGoPhast.PhastModel.SutraMesh.Assign(FOldSutraSettings);
  ForceRedraw;
end;

procedure TUndoSutraMeshDisplay.ForceRedraw;
begin
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameFrontView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
end;

end.
