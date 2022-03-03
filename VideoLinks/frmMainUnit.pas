unit frmMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls,
  Xml.VerySimple, Vcl.WinXPickers, JvToolEdit, Vcl.Mask, JvExMask, JvMaskEdit,
  JvCheckedMaskEdit, JvDatePickerEdit, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    tvVideos: TTreeView;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    sbStatus: TStatusBar;
    Panel1: TPanel;
    btnAddChild: TButton;
    btnAddNode: TButton;
    btnDeleteNode: TButton;
    edTitle: TEdit;
    edURL: TEdit;
    jvdDate: TJvDateEdit;
    lblDate: TLabel;
    lblTitle: TLabel;
    lblTopic: TLabel;
    lblURL: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    Splitter1: TSplitter;
    comtoTitleGroup: TComboBox;
    btnSortByTitle: TButton;
    btnSortByGroup: TButton;
    procedure tvVideosChange(Sender: TObject; Node: TTreeNode);
    procedure edURLChange(Sender: TObject);
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
    procedure comtoTitleGroupChange(Sender: TObject);
    procedure comtoTitleGroupExit(Sender: TObject);
    procedure btnSortByTitleClick(Sender: TObject);
    procedure tvVideosCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure btnSortByGroupClick(Sender: TObject);
  private
    FVideoList: TXmlVerySimple;
    FChanged: Boolean;
    FTitleGroups: TStringList;
    FSortByGroup: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.DateUtils, System.StrUtils;

resourcestring
  StrYear = 'Year';
  StrMonth = 'Month';
  StrDay = 'Day';
//  StrTitle = 'Title';
  StrTopic = 'Topic';
  StrDoYouWantToSave = 'Do you want to save the file?';

{$R *.dfm}

procedure TfrmMain.btnAddChildClick(Sender: TObject);
var
  ANode: TTreeNode;
  AnXmlNode: TXmlNode;
  NewXmlNode: TXmlNode;
begin
  if (tvVideos.Selected <> nil) and not tvVideos.Selected.HasChildren then
  begin
    FChanged := True;
    ANode := tvVideos.Items.AddChild(tvVideos.Selected, 'NewNode');
    if tvVideos.Selected.Data <> nil then
    begin
      AnXmlNode := tvVideos.Selected.Data;
      NewXmlNode := AnXmlNode.AddChild('URL');
      NewXmlNode.Text := '';
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
  AnXmlNode.Text := '';
  ANode.Data := AnXmlNode;
  tvVideos.Selected := ANode;
  jvdDate.Text := '';
  FChanged := True;
end;

procedure TfrmMain.btnDeleteNodeClick(Sender: TObject);
var
  AnXmlNode: TXmlNode;
  ParentTreeNode: TTreeNode;
  ParentXmlNode: TXmlNode;
begin
  if tvVideos.Selected <> nil then
  begin
    ParentTreeNode := tvVideos.Selected.Parent;
    AnXmlNode := tvVideos.Selected.Data;
    if ParentTreeNode = nil then
    begin
      FVideoList.ChildNodes.Remove(AnXmlNode);
    end
    else
    begin
      ParentXmlNode := ParentTreeNode.Data;
      ParentXmlNode.ChildNodes.Remove(AnXmlNode);
    end;
    tvVideos.Selected.Free;
    FChanged := True;
  end;
end;

procedure TfrmMain.btnSortByGroupClick(Sender: TObject);
begin
  FSortByGroup := True;
  tvVideos.AlphaSort;
end;

procedure TfrmMain.btnSortByTitleClick(Sender: TObject);
begin
  FSortByGroup := False;
  tvVideos.AlphaSort;
end;

procedure TfrmMain.comtoTitleGroupChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
  TopicName: string;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    Assert(AnXmlNode <> nil);
    if AnXmlNode.HasAttribute(StrTopic) then
    begin
      TopicName := AnXmlNode.Attributes[StrTopic];
    end
    else
    begin
      TopicName := ''
    end;
    if TopicName <> comtoTitleGroup.Text then
    begin
      AnXmlNode.SetAttribute(StrTopic, comtoTitleGroup.Text);
      FChanged := True;
    end;
  end;
end;

procedure TfrmMain.comtoTitleGroupExit(Sender: TObject);
begin
  if comtoTitleGroup.Text <> '' then
  begin
    FTitleGroups.Add(comtoTitleGroup.Text);
    comtoTitleGroup.Items := FTitleGroups;
  end;
end;

procedure TfrmMain.edURLChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
  Url: string;
  NewText: string;
begin
  if tvVideos.Selected <> nil then
  begin
    if tvVideos.Selected.Data <> nil then
    begin
      Url := ReplaceStr(edURL.Text, 'http:', 'https:');
      if Url <> edURL.Text then
      begin
        edURL.Text := Url;
      end;

      AnXmlNode := tvVideos.Selected.Data;
      AnXmlNode.Name := Url;

      if (AnXmlNode.text <> '') then
      begin
        NewText := AnXmlNode.text + '; ' + Url;
      end
      else
      begin
        NewText := Url;
      end;
    end;
    if tvVideos.Selected.Text <> NewText then
    begin
      tvVideos.Selected.Text := NewText;
      FChanged := True;
    end;
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
    if AnXmlNode.Text <> edTitle.Text then
    begin
      AnXmlNode.Text := edTitle.Text;
      FChanged := True;
    end;
    if AnXmlNode.text <> '' then
    begin
      tvVideos.Selected.Text :=
        AnXmlNode.text + '; ' + edURL.Text;
    end
    else
    begin
      tvVideos.Selected.Text := edURL.Text;
    end;
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
  TopicName: string;
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
//      AnXmlNode.
      if AnXmlNode.HasAttribute(StrTopic) then
      begin
        TopicName := AnXmlNode.Attributes[StrTopic];
        FTitleGroups.Add(TopicName);
      end;

      ATreeNode := tvVideos.Items.Add(nil, AnXmlNode.Text);
      ATreeNode.Data := AnXmlNode;
      if AnXmlNode.text <> '' then
      begin
        ATreeNode.Text :=
          AnXmlNode.text + '; ' + AnXmlNode.Name;
      end
      else
      begin
        ATreeNode.Text := AnXmlNode.Name;
      end;

      for InnerNodeIndex := 0 to AnXmlNode.ChildNodes.Count - 1 do
      begin
        ChildXmlNode := AnXmlNode.ChildNodes[InnerNodeIndex];
        ChildTreeNode := tvVideos.Items.AddChild(ATreeNode, ChildXmlNode.Text);
        ChildTreeNode.Data := ChildXmlNode;
        if ChildXmlNode.text <> '' then
        begin
          ChildTreeNode.Text :=
            ChildXmlNode.text + '; ' + ChildXmlNode.Name;
        end
        else
        begin
          ChildTreeNode.Text := ChildXmlNode.Name;
        end;
      end;
    end;
    comtoTitleGroup.Items := FTitleGroups;

    dlgSave.FileName := dlgOpen.FileName;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FVideoList := TXmlVerySimple.Create;
  FTitleGroups := TStringList.Create;
  FTitleGroups.CaseSensitive := False;
  FTitleGroups.Duplicates := dupIgnore;
  FTitleGroups.Sorted := True;
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
  FTitleGroups.Free;
  FVideoList.Free;
end;

procedure TfrmMain.jvdDateChange(Sender: TObject);
var
  AnXmlNode: TXmlNode;
  AYear: Word;
  AMonth: Word;
  ADay: Word;
  YearString: string;
  MonthString: string;
  DayString: string;
begin
  if tvVideos.Selected <> nil then
  begin
    AnXmlNode := tvVideos.Selected.Data;
    Assert(AnXmlNode <> nil);
    DecodeDate(jvdDate.Date, AYear, AMonth, ADay);
    if AnXmlNode.HasAttribute(StrYear) then
    begin
      YearString := AnXmlNode.Attributes[StrYear];
    end
    else
    begin
      YearString := ''
    end;

    if AnXmlNode.HasAttribute(StrMonth) then
    begin
      MonthString := AnXmlNode.Attributes[StrMonth];
    end
    else
    begin
      MonthString := ''
    end;

    if AnXmlNode.HasAttribute(StrDay) then
    begin
      DayString := AnXmlNode.Attributes[StrDay];
    end
    else
    begin
      DayString := ''
    end;

    if YearString <> AYear.ToString then
    begin
      AnXmlNode.SetAttribute(StrYear, AYear.ToString);
      FChanged := True;
    end;
    if MonthString <> AMonth.ToString then
    begin
      AnXmlNode.SetAttribute(StrMonth, AMonth.ToString);
      FChanged := True;
    end;
    if DayString <> ADay.ToString then
    begin
      AnXmlNode.SetAttribute(StrDay, ADay.ToString);
      FChanged := True;
    end;
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
    AnXmlNode := Node.Data;
    edURL.Text := AnXmlNode.Name;
    edTitle.Text := AnXmlNode.Text;
    if AnXmlNode.HasAttribute(StrTopic) then
    begin
      comtoTitleGroup.Text := AnXmlNode.Attributes[StrTopic];
    end
    else
    begin
      comtoTitleGroup.Text := '';
//      edTopic.Text := ''
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

procedure TfrmMain.tvVideosCompare(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
var
  AnXmlNode: TXmlNode;
  Group1: string;
  Group2: string;
begin
  if FSortByGroup then
  begin
    AnXmlNode := Node1.Data;
    if AnXmlNode.HasAttribute(StrTopic) then
    begin
      Group1 := AnXmlNode.Attributes[StrTopic];
    end
    else
    begin
      Group1 := '';
    end;
    AnXmlNode := Node2.Data;
    if AnXmlNode.HasAttribute(StrTopic) then
    begin
      Group2 := AnXmlNode.Attributes[StrTopic];
    end
    else
    begin
      Group2 := '';
    end;
    Compare := CompareText(Group1, Group2);
    if Compare = 0 then
    begin
      Compare := CompareText(Node1.Text, Node2.Text);
    end;
  end
  else
  begin
    Compare := CompareText(Node1.Text, Node2.Text);
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
  if AnXmlNode.Text <> '' then
  begin
    sbStatus.SimpleText := AnXmlNode.Text
     + '; ' + AnXmlNode.Name;
  end
  else
  begin
    sbStatus.SimpleText := AnXmlNode.Name;
  end;
end;

end.
