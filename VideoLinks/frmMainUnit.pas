unit frmMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls,
  Xml.VerySimple, Vcl.WinXPickers, JvToolEdit, Vcl.Mask, JvExMask, JvMaskEdit,
  JvCheckedMaskEdit, JvDatePickerEdit;

type
  TfrmMain = class(TForm)
    tvVideos: TTreeView;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    edText: TEdit;
    btnAddChild: TButton;
    btnDeleteNode: TButton;
    btnAddNode: TButton;
    edTitle: TEdit;
    lblURL: TLabel;
    lblTitle: TLabel;
    lblDate: TLabel;
    sbStatus: TStatusBar;
    jvdDate: TJvDateEdit;
    edTopic: TEdit;
    lblTopic: TLabel;
    procedure tvVideosChange(Sender: TObject; Node: TTreeNode);
    procedure edTextChange(Sender: TObject);
    procedure btnAddChildClick(Sender: TObject);
    procedure btnDeleteNodeClick(Sender: TObject);
    procedure btnAddNodeClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure tvVideosMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure jvdDateChange(Sender: TObject);
    procedure edTopicChange(Sender: TObject);
  private
    FVideoList: TXmlVerySimple;
    FChanged: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.DateUtils;

resourcestring
  StrYear = 'Year';
  StrMonth = 'Month';
  StrDay = 'Day';
  StrTitle = 'Title';
  StrTopic = 'Topic';
  StrDoYouWantToSave = 'Do you want to save the file?';

{$R *.dfm}

procedure TfrmMain.btnAddChildClick(Sender: TObject);
var
  ANode: TTreeNode;
  AnXmlNode: TXmlNode;
  NewXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    FChanged := True;
    ANode := tvVideos.Items.AddChild(tvVideos.Selected, 'NewNode');
    if tvVideos.Selected.Data <> nil then
    begin
      AnXmlNode := tvVideos.Selected.Data;
      NewXmlNode := AnXmlNode.AddChild('URL');
      NewXmlNode.Text := 'NewNode';
      ANode.Data := NewXmlNode;
    end;
    tvVideos.Selected := ANode;
    jvdDate.Text := '';

  end;
end;

procedure TfrmMain.btnAddNodeClick(Sender: TObject);
var
  ANode: TTreeNode;
  AnXmlNode: TXmlNode;
begin
  ANode := tvVideos.Items.Add(nil, 'NewNode');
  AnXmlNode := FVideoList.AddChild('URL');
  AnXmlNode.Text := 'NewNode';
  ANode.Data := AnXmlNode;
  tvVideos.Selected := ANode;
  jvdDate.Text := '';
  FChanged := True;
end;

procedure TfrmMain.btnDeleteNodeClick(Sender: TObject);
var
  AnXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    AnXmlNode.Free;
    tvVideos.Selected.Free;
    FChanged := True;
  end;
end;

procedure TfrmMain.edTextChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    tvVideos.Selected.Text := EdText.Text;
    if tvVideos.Selected.Data <> nil then
    begin
      AnXmlNode := tvVideos.Selected.Data;
      AnXmlNode.Text := EdText.Text;
    end;
    FChanged := True;
  end;
end;

procedure TfrmMain.edTitleChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    Assert(AnXmlNode <> nil);
    AnXmlNode.SetAttribute(StrTitle, edTitle.Text);
    FChanged := True;
  end;
end;

procedure TfrmMain.edTopicChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    Assert(AnXmlNode <> nil);
    AnXmlNode.SetAttribute(StrTopic, edTopic.Text);
    FChanged := True;
  end;
end;

procedure TfrmMain.miOpenClick(Sender: TObject);
var
  NodeIndex: Integer;
  AnXmlNode: TXmlNode;
  ATreeNode: TTreeNode;
  InnerNodeIndex: Integer;
  ChildXmlNode: TXmlNode;
  ChildTreeNode: TTreeNode;
begin
  if FChanged then
  begin
    if (MessageDlg(StrDoYouWantToSave, mtInformation, [mbYes, mbNo], 0) = mrYes) then
    begin
      miSaveClick(nil);
    end;
  end;
  if dlgOpen.Execute then
  begin
    FChanged := False;
    FVideoList.LoadFromFile(dlgOpen.FileName);
    tvVideos.Items.Clear;
    for NodeIndex := 1 to FVideoList.ChildNodes.Count - 1 do
    begin
      AnXmlNode := FVideoList.ChildNodes[NodeIndex];
      ATreeNode := tvVideos.Items.Add(nil, AnXmlNode.Text);
      ATreeNode.Data := AnXmlNode;
      for InnerNodeIndex := 0 to AnXmlNode.ChildNodes.Count - 1 do
      begin
        ChildXmlNode := AnXmlNode.ChildNodes[InnerNodeIndex];
        ChildTreeNode := tvVideos.Items.AddChild(ATreeNode, ChildXmlNode.Text);
        ChildTreeNode.Data := ChildXmlNode;
      end;
    end;

    dlgSave.FileName := dlgOpen.FileName;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FVideoList := TXmlVerySimple.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if FChanged then
  begin
    if (MessageDlg(StrDoYouWantToSave, mtInformation, [mbYes, mbNo], 0) = mrYes) then
    begin
      miSaveClick(nil);
    end;
  end;
  FVideoList.Free;
end;

procedure TfrmMain.jvdDateChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
  AYear: Word;
  AMonth: Word;
  ADay: Word;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    Assert(AnXmlNode <> nil);
    DecodeDate(jvdDate.Date, AYear, AMonth, ADay);
    AnXmlNode.SetAttribute(StrYear, AYear.ToString);
    AnXmlNode.SetAttribute(StrMonth, AMonth.ToString);
    AnXmlNode.SetAttribute(StrDay, ADay.ToString);
    FChanged := True;
  end;

end;

procedure TfrmMain.miSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    FVideoList.SaveToFile(dlgSave.FileName);
    FChanged := False;
  end;
end;

procedure TfrmMain.tvVideosChange(Sender: TObject; Node: TTreeNode);
var
  AnXmlNode: TXmlNode;
  Year: Word;
  Month: Word;
  Day: Word;
begin
  if Node <> nil then
  begin
    EdText.Text := Node.Text;
    AnXmlNode := Node.Data;
    if AnXmlNode.HasAttribute(StrTitle) then
    begin
      edTitle.Text := AnXmlNode.Attributes[StrTitle];
    end
    else
    begin
      edTitle.Text := ''
    end;
    if AnXmlNode.HasAttribute(StrTopic) then
    begin
      edTopic.Text := AnXmlNode.Attributes[StrTopic];
    end
    else
    begin
      edTopic.Text := ''
    end;
    if AnXmlNode.HasAttribute(StrYear) then
    begin
      Year := AnXmlNode.Attributes[StrYear].ToInteger;
      Month := AnXmlNode.Attributes[StrMonth].ToInteger;
      Day := AnXmlNode.Attributes[StrDay].ToInteger;
      jvdDate.Date := EncodeDate(Year, Month, Day);
    end
    else
    begin
      jvdDate.Text := '';
    end;
  end;
end;

procedure TfrmMain.tvVideosMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ANode: TTreeNode;
  AnXmlNode: TXmlNode;
begin
  ANode := tvVideos.GetNodeAt(X, Y);
  if ANode = nil then
  begin
    Exit
  end;
  AnXmlNode := ANode.Data;
  if AnXmlNode.HasAttribute(StrTitle) then
  begin
    sbStatus.SimpleText := AnXmlNode.Attributes[StrTitle]
     + '; ' + ANode.Text;
  end
  else
  begin
    sbStatus.SimpleText := ANode.Text;
  end;
end;

end.
