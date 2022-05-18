unit frmSutraProgramLocationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Mask, JvExMask, JvToolEdit, JvExStdCtrls, JvHtControls, Buttons,
  ExtCtrls;

type
  TfrmSutraProgramLocations = class(TfrmCustomGoPhast)
    htlblSutra22: TJvHTLabel;
    fedSutra22: TJvFilenameEdit;
    lblSutra22: TLabel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblTextEditor: TLabel;
    fedTextEditor: TJvFilenameEdit;
    htlblSutra3: TJvHTLabel;
    fedSutra30: TJvFilenameEdit;
    lblSutra3: TLabel;
    lblSutra40: TLabel;
    htlblSutra4: TJvHTLabel;
    fedSutra40: TJvFilenameEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure fedSutra22Change(Sender: TObject);
    procedure fedTextEditorChange(Sender: TObject);
    procedure fedSutra30Change(Sender: TObject);
    procedure fedSutra40Change(Sender: TObject);
  private
    procedure SetData;
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSutraProgramLocations: TfrmSutraProgramLocations;

implementation

uses
  frmProgramLocationsUnit, PhastModelUnit, frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

procedure TfrmSutraProgramLocations.fedSutra22Change(Sender: TObject);
begin
  inherited;
  if not FileExists(fedSutra22.FileName)
    and (frmGoPhast.ModelSelection = msSutra22) then
  begin
    fedSutra22.Color := clRed;
  end
  else
  begin
    fedSutra22.Color := clWindow;
  end;
end;

procedure TfrmSutraProgramLocations.fedTextEditorChange(Sender: TObject);
begin
  inherited;
  if (fedTextEditor.FileName = '')
    or (LowerCase(fedTextEditor.FileName) = 'notepad.exe')
    or FileExists(fedTextEditor.FileName) then
  begin
    fedTextEditor.Color := clWindow;
  end
  else
  begin
    fedTextEditor.Color := clRed;
  end;
end;

procedure TfrmSutraProgramLocations.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSutraProgramLocations.GetData;
begin

  try
    fedSutra22.FileName := frmGoPhast.PhastModel.ProgramLocations.Sutra22Location;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedSutra22Change(nil);

  try
    fedSutra30.FileName := frmGoPhast.PhastModel.ProgramLocations.Sutra30Location;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedSutra30Change(nil);

  try
    fedSutra40.FileName := frmGoPhast.PhastModel.ProgramLocations.Sutra40Location;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedSutra30Change(nil);

  try
    fedTextEditor.FileName := frmGoPhast.PhastModel.ProgramLocations.TextEditorLocation;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedTextEditorChange(nil);
end;

procedure TfrmSutraProgramLocations.fedSutra30Change(Sender: TObject);
begin
  inherited;
  if not FileExists(fedSutra30.FileName)
    and (frmGoPhast.ModelSelection = msSutra30) then
  begin
    fedSutra30.Color := clRed;
  end
  else
  begin
    fedSutra30.Color := clWindow;
  end;

end;

procedure TfrmSutraProgramLocations.fedSutra40Change(Sender: TObject);
begin
  inherited;
  if not FileExists(fedSutra40.FileName)
    and (frmGoPhast.ModelSelection = msSutra40) then
  begin
    fedSutra40.Color := clRed;
  end
  else
  begin
    fedSutra40.Color := clWindow;
  end;

end;

procedure TfrmSutraProgramLocations.SetData;
var
  NewLocations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  inherited;
  NewLocations := TProgramLocations.Create;
  NewLocations.Assign(frmGoPhast.PhastModel.ProgramLocations);
  NewLocations.Sutra22Location := fedSutra22.FileName;
  NewLocations.Sutra30Location := fedSutra30.FileName;
  NewLocations.Sutra40Location := fedSutra40.FileName;
  NewLocations.TextEditorLocation := fedTextEditor.FileName;
  Undo := TUndoChangeProgramLocations.Create(NewLocations);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmSutraProgramLocations.btnOKClick(Sender: TObject);
begin
  SetData;
end;

end.
