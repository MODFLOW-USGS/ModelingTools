unit frmEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvRichEdit, ComCtrls, ActnList, Menus,
  ToolWin, ImgList,
  Utilities;

type
  TFormEditor = class(TForm)
    OpenDialog1: TOpenDialog;
    JvRichEd: TJvRichEdit;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    ActionList1: TActionList;
    actFileOpen: TAction;
    actExit: TAction;
    tbarEditor: TToolBar;
    tbtnOpen: TToolButton;
    ImageList1: TImageList;
    actReload: TAction;
    miReload: TMenuItem;
    ToolButton1: TToolButton;
    actTop: TAction;
    actBottom: TAction;
    ToolButton2: TToolButton;
    actFind: TAction;
    ToolButton4: TToolButton;
    Edit1: TMenuItem;
    miFind: TMenuItem;
    op2: TMenuItem;
    Bottom2: TMenuItem;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    actToggleWordWrap: TAction;
    View1: TMenuItem;
    WordWrap1: TMenuItem;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton3: TToolButton;
    actSave: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure ShowViewCurrentEditorFile;
    procedure actReloadExecute(Sender: TObject);
    procedure actTopExecute(Sender: TObject);
    procedure actBottomExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure JvRichEdTextNotFound(Sender: TObject; const FindText: string);
    procedure actToggleWordWrapExecute(Sender: TObject);
  private
    FName: TFileName;
    SList: TStringList;
    procedure SetCaption;
    { Private declarations }
  public
    { Public declarations }
    CurrentEditorFile: TFileName;
  end;

var
  FormEditorAppInput: TFormEditor;
  FormEditorAppOutput: TFormEditor;

implementation

{$R *.dfm}

procedure TFormEditor.actBottomExecute(Sender: TObject);
var
  I: integer;
  Point: TPoint;
begin
  I := JvRichEd.Lines.Count;
  JvRichEd.SetSelection(I,I,False);
  JvRichEd.CaretPos := Point;
end;

procedure TFormEditor.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormEditor.actFileOpenExecute(Sender: TObject);
begin
  OpenDialog1.FileName := CurrentEditorFile;
  if OpenDialog1.Execute then
    begin
      FName := OpenDialog1.FileName;
      SList.LoadFromFile(FName);
      JvRichEd.Clear;
      JvRichEd.Lines.AddStrings(SList);
      CurrentEditorFile := FName;
      JvRichEd.SetSelection(0,0,True);
      SetCaption;
    end;
end;

procedure TFormEditor.actFindExecute(Sender: TObject);
begin
  JvRichEd.FindDialog('');
end;

procedure TFormEditor.actReloadExecute(Sender: TObject);
begin
  SList.LoadFromFile(CurrentEditorFile);
  JvRichEd.Clear;
  JvRichEd.Lines.AddStrings(SList);
  JvRichEd.SetSelection(0,0,True);
  SList.Clear;
  SetCaption;
end;

procedure TFormEditor.actToggleWordWrapExecute(Sender: TObject);
begin
  JvRichEd.WordWrap := not JvRichEd.WordWrap;
  actToggleWordWrap.Checked := JvRichEd.WordWrap;
end;

procedure TFormEditor.actTopExecute(Sender: TObject);
begin
  JvRichEd.SetSelection(0,0,True);
end;

procedure TFormEditor.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  SList := TStringList.Create;
  CurrentEditorFile := '';
end;

procedure TFormEditor.FormDestroy(Sender: TObject);
begin
  SList.Free;
  JvRichEd.Free;
end;

procedure TFormEditor.FormShow(Sender: TObject);
begin
  JvRichEd.Clear;
  JvRichEd.Font.Name := 'Courier';
  JvRichEd.Font.Size := 12;
end;

procedure TFormEditor.JvRichEdTextNotFound(Sender: TObject;
  const FindText: string);
var
  Messg: string;
begin
  Messg := 'Text not found: ' + FindText;
  ShowMessage(Messg);
end;

procedure TFormEditor.ShowViewCurrentEditorFile;
var
  str: string;
begin
  if FileExists(CurrentEditorFile) then
    begin
      self.Show;
      SList.LoadFromFile(CurrentEditorFile);
      JvRichEd.Clear;
      JvRichEd.Lines.AddStrings(SList);
      JvRichEd.SetSelection(0,0,True);
      SetCaption;
      SList.Clear;
    end
  else
    begin
      str := 'File "' + CurrentEditorFile + '" does not exist.';
      ShowMessage(str);
    end;
end;

procedure TFormEditor.SetCaption;
var
  AbsPath: TFileName;
begin
  AbsPath := ExpandFileName(CurrentEditorFile);
  Caption := 'ModelMate File Viewer: [' + AbsPath + ']';
end;

end.
