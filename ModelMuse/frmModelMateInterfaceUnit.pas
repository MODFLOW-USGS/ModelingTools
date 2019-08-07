unit frmModelMateInterfaceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, Mask, JvExMask, JvToolEdit,
  UndoItems;

type
  TfrmModelMateInterface = class(TfrmCustomGoPhast)
    feModelMateFile: TJvFilenameEdit;
    lblModelMateFileName: TLabel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure feModelMateFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoModelMateData = class(TCustomUndo)
  private
    FOldModelMateFileName: string;
    FNewModelMateFileName: string;
  public
    Constructor Create(NewFileName: string);
    function Description: string; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

var
  frmModelMateInterface: TfrmModelMateInterface;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmModelMateInterface }

procedure TfrmModelMateInterface.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModelMateInterface.feModelMateFileBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  inherited;
  if AName = '' then
  begin
    AName := ChangeFileExt(frmGoPhast.sdSaveDialog.FileName, '.mtc');
  end;
end;

procedure TfrmModelMateInterface.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmModelMateInterface.GetData;
begin
  feModelMateFile.FileName := frmGoPhast.PhastModel.ModelMateProjectFileName;
end;

procedure TfrmModelMateInterface.SetData;
var
  Undo: TUndoModelMateData;
begin
  Undo := TUndoModelMateData.Create(ChangeFileExt(
    feModelMateFile.FileName, feModelMateFile.DefaultExt));
  try
    frmGoPhast.UndoStack.Submit(Undo)
  except
    Undo.Free;
  end;
end;

{ TUndoModelMateData }

constructor TUndoModelMateData.Create(NewFileName: string);
begin
  FOldModelMateFileName := frmGoPhast.PhastModel.ModelMateProjectFileName;
  FNewModelMateFileName := NewFileName;
end;

function TUndoModelMateData.Description: string;
begin
  result := 'change ModelMate file';
end;

procedure TUndoModelMateData.DoCommand;
begin
  frmGoPhast.PhastModel.ModelMateProjectFileName := FNewModelMateFileName;
end;

procedure TUndoModelMateData.Undo;
begin
  frmGoPhast.PhastModel.ModelMateProjectFileName := FOldModelMateFileName;
end;

end.
