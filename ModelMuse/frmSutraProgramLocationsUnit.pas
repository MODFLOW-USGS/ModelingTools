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
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure fedSutra22Change(Sender: TObject);
    procedure fedTextEditorChange(Sender: TObject);
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
  frmProgramLocationsUnit, PhastModelUnit, frmGoPhastUnit;

{$R *.dfm}

procedure TfrmSutraProgramLocations.fedSutra22Change(Sender: TObject);
begin
  inherited;
  if not FileExists(fedSutra22.FileName) then
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
    fedTextEditor.FileName := frmGoPhast.PhastModel.ProgramLocations.TextEditorLocation;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedTextEditorChange(nil);
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
  NewLocations.TextEditorLocation := fedTextEditor.FileName;
  Undo := TUndoChangeProgramLocations.Create(NewLocations);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmSutraProgramLocations.btnOKClick(Sender: TObject);
begin
  SetData;
end;

end.
