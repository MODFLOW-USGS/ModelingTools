unit frmArrangeModelsUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, ArchiveNodeInterface,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmArrangeModels = class(TForm)
    pnl5: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    lbModelNames: TListBox;
    btnAlphabetize: TButton;
    MemoDescription: TMemo;
    Splitter1: TSplitter;
    btnHelp: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnAlphabetizeClick(Sender: TObject);
    procedure lbModelNamesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure lbModelNamesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure lbModelNamesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FItem: TListBoxItem;
    procedure SetModelData;
    { Private declarations }
  public
    procedure GetModelData(ModelRoot: IArchiveNodeInterface);
    { Public declarations }
  end;

var
  frmArrangeModels: TfrmArrangeModels;

implementation

{$R *.fmx}

uses frmModelArchiverUnit;

{ TfrmArrangeModels }

procedure TfrmArrangeModels.btnAlphabetizeClick(Sender: TObject);
var
  ASorter: TStringList;
begin
  lbModelNames.BeginUpdate;
  try
    ASorter := TStringList.Create;
    try
      ASorter.Assign(lbModelNames.Items);
      ASorter.Sorted := True;
      lbModelNames.Items.Assign(ASorter);
    finally
      ASorter.Free;
    end;
  finally
    lbModelNames.EndUpdate;
  end;
end;

procedure TfrmArrangeModels.btnHelpClick(Sender: TObject);
begin
  OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmArrangeModels.btnOkClick(Sender: TObject);
begin
  SetModelData;
end;

procedure TfrmArrangeModels.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(btnHelp.HelpKeyword);
  end;
end;

procedure TfrmArrangeModels.GetModelData(ModelRoot: IArchiveNodeInterface);
var
  ModelIndex: Integer;
  AModelNode: TArchiveObject;
begin
  lbModelNames.BeginUpdate;
  try
    lbModelNames.Clear;
    for ModelIndex := 0 to ModelRoot.Count - 1 do
    begin
      AModelNode := ModelRoot.Children[ModelIndex] as TArchiveObject;
      lbModelNames.Items.AddObject(AModelNode.TreeViewItem.Text, AModelNode)
    end;
  finally
    lbModelNames.EndUpdate;
  end;
end;

procedure TfrmArrangeModels.lbModelNamesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  AModelNode: TArchiveObject;
begin
  AModelNode := lbModelNames.Items.Objects[Item.Index] as TArchiveObject;
  MemoDescription.Lines.Text := AModelNode.Description;
end;

procedure TfrmArrangeModels.lbModelNamesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FItem := lbModelNames.ItemByPoint(X, Y)
end;

procedure TfrmArrangeModels.lbModelNamesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  NewItem: TListBoxItem;
begin
  NewItem := lbModelNames.ItemByPoint(X, Y);
  if (FItem <> nil) and (NewItem <> nil) then
  begin
    FItem.Index := NewItem.Index;
  end;
end;

procedure TfrmArrangeModels.SetModelData;
begin
  frmModelArchiver.SortModels(lbModelNames.Items);
end;

end.
