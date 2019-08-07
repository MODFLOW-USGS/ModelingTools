unit frmExtensionsUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.ListBox, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.StdCtrls,
  ExtensionTypeUnit;

type
  TfrmExtensions = class(TForm)
    sgExtensions: TStringGrid;
    scolExt: TStringColumn;
    comboExtensions: TComboBox;
    scolFileType: TStringColumn;
    Panel1: TPanel;
    sbExtensionCount: TSpinBox;
    btnOK: TButton;
    btnCancel: TButton;
    scolDescription: TStringColumn;
    Label1: TLabel;
    btnHelp: TButton;
    procedure sgExtensionsSelectCell(Sender: TObject; const ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure comboExtensionsChange(Sender: TObject);
    procedure sbExtensionCountChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure sgExtensionsHeaderClick(Column: TColumn);
  private
    FSelectedRow: Integer;
    FSettingCombo: Boolean;
    FPriorColumn: TColumn;
    procedure HideCombo(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF;
    const ContentSizeChanged: Boolean);
    procedure AssignExtensionsToStringGrid(Extensions: TExtensionList);
    procedure AssignExtensionsFromStringGrid(Extensions: TExtensionList);
    { Private declarations }
  public
    procedure GetExtensionData;
    procedure SetExtensionData;
    { Public declarations }
  end;

var
  frmExtensions: TfrmExtensions;

implementation

uses frmModelArchiverUnit;

{$R *.fmx}

procedure TfrmExtensions.btnHelpClick(Sender: TObject);
begin
  OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmExtensions.btnOKClick(Sender: TObject);
begin
  SetExtensionData;
end;

procedure TfrmExtensions.comboExtensionsChange(Sender: TObject);
begin
  if not FSettingCombo and (FSelectedRow >= 0) and (comboExtensions.ItemIndex >= 0) then
  begin
    sgExtensions.Cells[1,FSelectedRow] := comboExtensions.Items[comboExtensions.ItemIndex];
  end;
end;

procedure TfrmExtensions.FormCreate(Sender: TObject);
begin
  comboExtensions.Visible := False;
  comboExtensions.Items := ExtensionTypeNames;
  sbExtensionCount.Max := Maxint;
  scolDescription.Width := 600;
  sgExtensions.OnViewportPositionChange := HideCombo;
  FPriorColumn := scolExt;
end;

procedure TfrmExtensions.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(btnHelp.HelpKeyword);
  end;
end;

procedure TfrmExtensions.GetExtensionData;
var
  Extensions: TExtensionList;
begin
  Extensions := frmModelArchiver.Extensions;
  AssignExtensionsToStringGrid(Extensions);
end;

procedure TfrmExtensions.HideCombo(Sender: TObject; const OldViewportPosition,
  NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  comboExtensions.Visible := False;
end;

procedure TfrmExtensions.sbExtensionCountChange(Sender: TObject);
begin
  sgExtensions.RowCount := Round(sbExtensionCount.Value)
end;

procedure TfrmExtensions.SetExtensionData;
var
  Extensions: TExtensionList;
begin
  Extensions := frmModelArchiver.Extensions;
  AssignExtensionsFromStringGrid(Extensions);
end;

procedure TfrmExtensions.AssignExtensionsFromStringGrid(Extensions: TExtensionList);
var
  Index: Integer;
  AnExt: TExtensionObject;
  ItemIndex: Integer;
begin
  Extensions.Clear;
  for Index := 0 to sgExtensions.RowCount - 1 do
  begin
    AnExt := TExtensionObject.Create;
    AnExt.Extension := sgExtensions.Cells[0, Index];
    ItemIndex := comboExtensions.Items.IndexOf(sgExtensions.Cells[1, Index]);
    if (ItemIndex >= 0) and (AnExt.Extension <> '') then
    begin
      AnExt.ExtensionType := TExtensionType(ItemIndex);
      Extensions.Add(AnExt);
    end;
    AnExt.Description := sgExtensions.Cells[2, Index];
  end;
end;

procedure TfrmExtensions.AssignExtensionsToStringGrid(Extensions: TExtensionList);
var
  Index: Integer;
  AnExt: TExtensionObject;
begin
  sbExtensionCount.Value := Extensions.Count;
  sbExtensionCountChange(nil);
  for Index := 0 to Extensions.Count - 1 do
  begin
    AnExt := Extensions[Index];
    sgExtensions.Cells[0, Index] := AnExt.Extension;
    sgExtensions.Cells[1, Index] := comboExtensions.Items[Ord(AnExt.ExtensionType)];
    sgExtensions.Cells[2, Index] := AnExt.Description;
  end;
end;

procedure TfrmExtensions.sgExtensionsHeaderClick(Column: TColumn);
var
  LocalExtensions: TExtensionList;
begin
  if Column <> FPriorColumn then
  begin
    FPriorColumn := Column;
    LocalExtensions := TExtensionList.Create;
    try
      AssignExtensionsFromStringGrid(LocalExtensions);
      if Column = scolExt then
      begin
        LocalExtensions.SortRecords;
      end
      else if Column = scolFileType then
      begin
        LocalExtensions.SortFunction;
      end
      else if Column = scolDescription then
      begin
        LocalExtensions.SortDescription;
      end;
      AssignExtensionsToStringGrid(LocalExtensions);
    finally
      LocalExtensions.Free;
    end;
  end;
end;

procedure TfrmExtensions.sgExtensionsSelectCell(Sender: TObject; const ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ARect: TRect;
  APosition: TPoint;
begin
  if (ACol >= 0) and (ARow >= 0) then
  begin
    ARect := sgExtensions.CellRect(1, ARow);
    APosition := ARect.BottomRight;
    APosition.X := APosition.X - ARect.Width - Round(sgExtensions.ViewportPosition.X);
    APosition.Y := APosition.Y - Round(sgExtensions.ViewportPosition.Y);
    comboExtensions.Position.Point := sgExtensions.LocalToAbsolute(APosition);
    FSettingCombo := True;
    try
      comboExtensions.ItemIndex := comboExtensions.Items.IndexOf(sgExtensions.Cells[1,ARow]);
    finally
      FSettingCombo := False;
    end;
    FSelectedRow := ARow;
    comboExtensions.Width := scolFileType.Width;
    comboExtensions.Visible := True;
  end
  else if comboExtensions <> nil then
  begin
    comboExtensions.Visible := False;
  end;
end;

end.
