unit frmFootprintLocationUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Mask, JvExMask, JvToolEdit, JvExStdCtrls,
  JvHtControls;

type
  TfrmFootprintLocation = class(TfrmCustomGoPhast)
    htlblSutra22: TJvHTLabel;
    lblFootprint: TLabel;
    lblTextEditor: TLabel;
    fedFootprint: TJvFilenameEdit;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    fedTextEditor: TJvFilenameEdit;
    procedure fedFootprintChange(Sender: TObject);
    procedure fedTextEditorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure SetData;
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFootprintLocation: TfrmFootprintLocation;

implementation

{$R *.dfm}

uses frmGoPhastUnit, UndoItems, frmProgramLocationsUnit, PhastModelUnit;

procedure TfrmFootprintLocation.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFootprintLocation.fedFootprintChange(Sender: TObject);
begin
  inherited;
  if not FileExists(fedFootprint.FileName) then
  begin
    fedFootprint.Color := clRed;
  end
  else
  begin
    fedFootprint.Color := clWindow;
  end;
end;

procedure TfrmFootprintLocation.fedTextEditorChange(Sender: TObject);
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

procedure TfrmFootprintLocation.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmFootprintLocation.GetData;
begin
  try
    fedFootprint.FileName := frmGoPhast.PhastModel.ProgramLocations.FootprintLocation;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedFootprintChange(nil);
  try
    fedTextEditor.FileName := frmGoPhast.PhastModel.ProgramLocations.TextEditorLocation;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedTextEditorChange(nil);
end;

procedure TfrmFootprintLocation.SetData;
var
  NewLocations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  inherited;
  NewLocations := TProgramLocations.Create;
  NewLocations.Assign(frmGoPhast.PhastModel.ProgramLocations);
  NewLocations.FootprintLocation := fedFootprint.FileName;
  NewLocations.TextEditorLocation := fedTextEditor.FileName;
  Undo := TUndoChangeProgramLocations.Create(NewLocations);
  frmGoPhast.UndoStack.Submit(Undo);
end;

end.
