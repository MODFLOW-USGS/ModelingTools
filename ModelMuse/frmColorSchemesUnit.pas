unit frmColorSchemesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, frameGridUnit, ColorSchemes,
  UndoItems, Vcl.Mask;

type
  TUndoColorSchemes = class(TCustomUndo)
  private
    FNewColorSchemes: TUserDefinedColorSchemeCollection;
    FOldColorSchemes: TUserDefinedColorSchemeCollection;
    procedure UpdateViews;
  protected
    function Description: string; override;
  public
    constructor Create(var NewColorSchemes: TUserDefinedColorSchemeCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

  TfrmColorSchemes = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlSelect: TPanel;
    grdpnl1: TGridPanel;
    btnAdd: TSpeedButton;
    btnInsertUnit: TSpeedButton;
    btnDeleteUnit: TSpeedButton;
    tvColorSchemes: TTreeView;
    spl1: TSplitter;
    pnlColorScheme: TPanel;
    edName: TLabeledEdit;
    frameColorScheme: TframeGrid;
    dlgColor: TColorDialog;
    procedure frameColorSchemeGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameColorSchemeGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure tvColorSchemesChange(Sender: TObject; Node: TTreeNode);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteUnitClick(Sender: TObject);
    procedure btnInsertUnitClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
  private
    FColorSchemes: TUserDefinedColorSchemeCollection;
    FSelectedColorScheme: TUserDefinedColorSchemeItem;
    procedure SetSelectedColorScheme(const Value: TUserDefinedColorSchemeItem);
    property SelectedColorScheme: TUserDefinedColorSchemeItem
      read FSelectedColorScheme write SetSelectedColorScheme;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmColorSchemes: TfrmColorSchemes;

implementation

uses
  frmGoPhastUnit, frmDisplayDataUnit;

{$R *.dfm}

procedure TfrmColorSchemes.btnAddClick(Sender: TObject);
var
  NewItem: TUserDefinedColorSchemeItem;
  ANode: TTreeNode;
begin
  inherited;
  NewItem := FColorSchemes.Add;
  NewItem.Name := 'New Custom Color Scheme';
  ANode := tvColorSchemes.Items.AddObject(nil, NewItem.Name,
    NewItem);
  ANode.Selected := True;
end;

procedure TfrmColorSchemes.btnDeleteUnitClick(Sender: TObject);
var
  AnItem: TUserDefinedColorSchemeItem;
begin
  inherited;
  if tvColorSchemes.Selected <> nil then
  begin
    AnItem := tvColorSchemes.Selected.Data;
    SelectedColorScheme := nil;
    AnItem.Free;
    tvColorSchemes.Selected.Delete;
  end;
end;

procedure TfrmColorSchemes.btnInsertUnitClick(Sender: TObject);
var
  NewItem: TUserDefinedColorSchemeItem;
  CurrentItem: TUserDefinedColorSchemeItem;
  ANode: TTreeNode;
begin
  inherited;
  if tvColorSchemes.Selected = nil then
  begin
    btnAddClick(Sender);
  end
  else
  begin
    CurrentItem := tvColorSchemes.Selected.Data;
    NewItem := FColorSchemes.Add;
    NewItem.Name := 'New Custom Color Scheme';
    NewItem.Index := CurrentItem.Index + 1;
    ANode := tvColorSchemes.Items.AddObject(nil, NewItem.Name,
      NewItem);
    ANode.Selected := True;
  end;
end;

procedure TfrmColorSchemes.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmColorSchemes.FormCreate(Sender: TObject);
begin
  inherited;

  frameColorScheme.Grid.Cells[0,0] := 'Fraction';
  frameColorScheme.Grid.Cells[1,0] := 'Color';

  GetData;
end;

procedure TfrmColorSchemes.FormDestroy(Sender: TObject);
begin
  inherited;
  FColorSchemes.Free;
end;

procedure TfrmColorSchemes.frameColorSchemeGridBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  AValue: integer;
begin
  inherited;

  if (ACol = 1) and (ARow > 0) then
  begin
    if TryStrToInt(frameColorScheme.Grid.Cells[ACol, ARow], AValue) then
    begin
      frameColorScheme.Grid.Canvas.Brush.Color := AValue;
    end;
  end;
end;

procedure TfrmColorSchemes.frameColorSchemeGridButtonClick(Sender: TObject;
  ACol, ARow: Integer);
//var
//  AnInt: Integer;
begin
  inherited;
  dlgColor.Color := StrToIntDef(frameColorScheme.Grid.Cells[ACol, ARow], clBlack);
  if dlgColor.Execute then
  begin
//    AnInt :=  dlgColor.Color;
    frameColorScheme.Grid.Cells[ACol, ARow] := '$' + IntToHex(dlgColor.Color, 6);
  end;
end;

procedure TfrmColorSchemes.GetData;
var
  SchemeIndex: Integer;
  AColorScheme: TUserDefinedColorSchemeItem;
  ANode: TTreeNode;
begin
  FColorSchemes := TUserDefinedColorSchemeCollection.Create(nil);
  FColorSchemes.Assign(frmGoPhast.PhastModel.ColorSchemes);
  ANode := nil;
  for SchemeIndex := 0 to FColorSchemes.Count - 1 do
  begin
    AColorScheme := FColorSchemes[SchemeIndex];
    ANode := tvColorSchemes.Items.AddObject(ANode, AColorScheme.Name,
      AColorScheme);
  end;
  if tvColorSchemes.Items.Count > 0 then
  begin
    ANode := tvColorSchemes.Items[0];
    ANode.Selected := True;
  end
  else
  begin
    SelectedColorScheme := nil;
  end;
end;

procedure TfrmColorSchemes.edNameChange(Sender: TObject);
begin
  inherited;
  if tvColorSchemes.Selected <> nil then
  begin
    tvColorSchemes.Selected.Text := edName.Text;
  end;
end;

procedure TfrmColorSchemes.SetData;
var
  Undo: TUndoColorSchemes;
begin
  SelectedColorScheme := nil;
  Undo := TUndoColorSchemes.Create(FColorSchemes);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmColorSchemes.SetSelectedColorScheme(
  const Value: TUserDefinedColorSchemeItem);
var
  RowIndex: Integer;
  AFraction: double;
  AnInt: integer;
  Item: TColorItem;
  ShouldEnable: boolean;
begin
  if FSelectedColorScheme <> Value then
  begin
    if FSelectedColorScheme <> nil then
    begin
      FSelectedColorScheme.Name := edName.Text;
      FSelectedColorScheme.Colors.Clear;
      for RowIndex := 1 to frameColorScheme.Grid.RowCount - 1 do
      begin
        if not TryStrToFloat(frameColorScheme.Grid.Cells[0, RowIndex], AFraction) then
        begin
          Continue;
        end;
        if not TryStrToInt(frameColorScheme.Grid.Cells[1, RowIndex], AnInt) then
        begin
          Continue;
        end;
        Item := FSelectedColorScheme.Colors.Add;
        Item.Fraction := AFraction;
        Item.Color := AnInt;
      end;
    end;
    FSelectedColorScheme := Value;
    if FSelectedColorScheme <> nil then
    begin
      edName.Text := FSelectedColorScheme.Name;
      frameColorScheme.seNumber.AsInteger := FSelectedColorScheme.Colors.Count;

      if FSelectedColorScheme.Colors.Count > 0 then
      begin
        frameColorScheme.Grid.RowCount := FSelectedColorScheme.Colors.Count+1;
        for RowIndex := 1 to frameColorScheme.Grid.RowCount - 1 do
        begin
          Item := FSelectedColorScheme.Colors[RowIndex-1];
          frameColorScheme.Grid.Cells[0, RowIndex] := FloatToStr(Item.Fraction);
          frameColorScheme.Grid.Cells[1, RowIndex] := '$' + IntToHex(Item.Color, 6);
        end;
      end
      else
      begin
        frameColorScheme.Grid.RowCount := 2;
        frameColorScheme.Grid.Cells[0, 1] := '';
        frameColorScheme.Grid.Cells[1, 1] := '';
      end;
    end;
  end;
  ShouldEnable := FSelectedColorScheme <> nil;
  edName.Enabled := ShouldEnable;
  frameColorScheme.Enabled := ShouldEnable;
  if ShouldEnable then
  begin
    frameColorScheme.Grid.Color := clWindow;
    frameColorScheme.Grid.ColorSelectedRow := True;
  end
  else
  begin
    frameColorScheme.Grid.Color := clBtnFace;
    frameColorScheme.Grid.ColorSelectedRow := False;
  end;
end;

procedure TfrmColorSchemes.tvColorSchemesChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  if Node = nil then
  begin
    SelectedColorScheme := nil;
  end
  else
  begin
    SelectedColorScheme := Node.Data;
  end;

end;

{ TUndoColorSchemes }

constructor TUndoColorSchemes.Create(
  var NewColorSchemes: TUserDefinedColorSchemeCollection);
begin
  FNewColorSchemes := NewColorSchemes;
  NewColorSchemes := nil;
  FOldColorSchemes := TUserDefinedColorSchemeCollection.Create(nil);
  FOldColorSchemes.Assign(frmGoPhast.PhastModel.ColorSchemes);
end;

function TUndoColorSchemes.Description: string;
begin
  result := 'change custom color schemes';
end;

destructor TUndoColorSchemes.Destroy;
begin
  FNewColorSchemes.Free;
  FOldColorSchemes.Free;
  inherited;
end;

procedure TUndoColorSchemes.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ColorSchemes := FNewColorSchemes;
  if frmDisplayData <> nil then
  begin
    frmDisplayData.UpdateColorSchemes;
  end;
  UpdateViews;

end;

procedure TUndoColorSchemes.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ColorSchemes := FOldColorSchemes;
  if frmDisplayData <> nil then
  begin
    frmDisplayData.UpdateColorSchemes;
  end;
  UpdateViews
end;

procedure TUndoColorSchemes.UpdateViews;
var
  ColorParameters: TColorParameters;
begin
  ColorParameters := frmGoPhast.PhastModel.GridColorParameters;
  if ColorParameters.ColorScheme > MaxColorScheme then
  begin
    frmGoPhast.InvalidateAllViews;
  end
  else
  begin
    ColorParameters := frmGoPhast.PhastModel.ContourColorParameters;
    if ColorParameters.ColorScheme > MaxColorScheme then
    begin
      frmGoPhast.InvalidateAllViews;
    end;
  end;
end;

end.
