{@abstract(The main purpose of @name is to define @link(TfrmGridSpacing)
  which is used to specify the positions of grid lines in
  @link(TPhastGrid).)}
unit frmGridSpacingUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, ComCtrls, Grids, RbwDataGrid4,  
  Buttons, ExtCtrls, Clipbrd, GoPhastTypes, ArgusDataEntry, Spin, Mask,
  JvExMask, JvSpin, PhastModelUnit, GrayTabs;

type
  {@abstract(@name is used to specify the positions of grid lines in
    @link(TPhastGrid).)
    See TfrmGoPhast.@link(TfrmGoPhast.acEditGridLinesExecute).}
  TfrmGridSpacing = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays "Default spacing".
    lblColDefaultSpacing: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in the X (column) direction: ".
    lblColNumNodes: TLabel;
    // @name: TLabel;
    // The text in @name describes how to use @classname.
    lblDescribe: TLabel;
    // @name: TLabel;
    // @name displays "Default spacing".
    lblLayDefaultSpacing: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in the Z (layer) direction: ".
    lblLayNumNodes: TLabel;
    // @name: TLabel;
    // @name displays "Default spacing".
    lblRowDefaultSpacing: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in the Y (row) direction: ".
    lblRowNumNodes: TLabel;
    // @name: TPageControl;
    // @name holds tabs for columns, rows and layers.
    // See @link(tabColumns), @link(tabRows), and @link(tabLayers).
    pcSubdivide: TPageControl;
    // @name: TPanel;
    // @name is the panel at the button that has the buttons.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name holds controls relating to columns.
    pnlColumns: TPanel;
    // @name: TPanel;
    // @name holds @link(lblDescribe).
    pnlDescribe: TPanel;
    // @name: TPanel;
    // @name holds controls relating to layers.
    pnlLayers: TPanel;
    // @name: TPanel;
    // @name holds controls relating to rows.
    pnlRows: TPanel;
    // @name: TRbwDataEntry;
    // @name sets the default spacing for columns.
    rdeSpacingColumns: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name sets the default spacing for layers.
    rdeSpacingLayers: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name sets the default spacing for rows.
    rdeSpacingRows: TRbwDataEntry;
    // @name: TTabSheet;
    // @name holds the data relating to columns.
    tabColumns: TTabSheet;
    // @name: TTabSheet;
    // @name holds the data relating to layers.
    tabLayers: TTabSheet;
    // @name: TTabSheet;
    // @name holds the data relating to rows.
    tabRows: TTabSheet;
    dgColumns: TRbwDataGrid4;
    dgRows: TRbwDataGrid4;
    dgLayers: TRbwDataGrid4;
    seColumns: TJvSpinEdit;
    seRows: TJvSpinEdit;
    seLayers: TJvSpinEdit;
    comboModel: TComboBox;
    lblModel: TLabel;
    // @name sets the column settings specified in @classname.
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name is the OnKeyUp event handler for @link(dgColumns),
    // @link(dgRows), and @link(dgLayers).
    // @name checks whether an end-or-line occurs in the pasted text.
    // If so, it calls @link(PasteMultipleCells).
    procedure dgKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // @name is the OnMouseDown event handler for @link(dgColumns),
    // @link(dgRows), and @link(dgLayers).
    // @name sets the cursor to give a visual indication that a row is being
    // dragged to a new position.
    procedure dgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name is the OnMouseMove event handler for @link(dgColumns),
    // @link(dgRows), and @link(dgLayers).
    // @name sets the cursor to give a visual indication that a row is being
    // dragged to a new position.
    procedure dgMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // @name is the OnMouseUp event handler for @link(dgColumns),
    // @link(dgRows), and @link(dgLayers).
    // @name sets the cursor to give a visual indication that a row has been
    // dragged to a new position.
    procedure dgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name prevents the user from selecting row 0.
    procedure dgSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    // @name tests whether a tab, end-of-line, empty space, or comma
    // If so, it calls @link(PasteMultipleCells).
    procedure dgSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    // @name calls @link(UpdateGrid) with the proper parameters for
    // @link(dgColumns).
    procedure seColumnsChange(Sender: TObject);
    // @name calls @link(UpdateGrid) with the proper parameters for
    // @link(dgLayers).
    procedure seLayersChange(Sender: TObject);
    // @name calls @link(UpdateGrid) with the proper parameters for
    // @link(dgRows).
    procedure seRowsChange(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
  private
    // @name is the column heading for column 1 in
    // @link(dgColumns).
    FColHeading: string;
    // @name is set to true when the user is dragging a row to a new position.
    FDraggingRows: boolean;
    // @name is the column heading for column 1 in
    // @link(dgLayers).
    FLayerHeading: string;
    // @name is the column heading for column 1 in
    // @link(dgRows).
    FRowHeading: string;
    FLabelText: string;
    // @name retrieves the locations of the grid lines in @link(TPhastGrid).
    procedure GetData(Model: TCustomModel);
    // @name splits up Value into multiple values and pastes the values
    // into ADataGrid.
    procedure PasteMultipleCells(const ADataGrid: TRbwDataGrid4;
      const ARow: Integer; const Value: String);
    // @name sets the cursor to indicate whether or not the user is dragging
    // a row in one of the TRbwDataGrids.
    procedure SetCursor(const ACol, ARow: integer);
    // @name sets the positions of the grid lines in @link(TPhastGrid)
    // based on the data in @classname.
    procedure SetData;
    // @name places the values from AnArray in Grid.
    procedure SetGrid(const AnArray: TOneDRealArray; const Grid: TRbwDataGrid4;
      const SpinEdit: TJvSpinEdit);
    procedure SetHeadings;
    // @name updates Grid to show NewCount number of data values.
    // If this increase the number of rows in the grid, the new grid
    // lines will have a spacing determined by the value in rdeSpacing.
    procedure UpdateGrid(const NewCount: integer;
      const rdeSpacing: TRbwDataEntry; const Grid: TRbwDataGrid4);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, AbstractGridUnit, UndoItems, CursorsFoiledAgain,
  System.Generics.Collections;

resourcestring
  StrChildGridsCanNot = ' Child grids can not be edited directly. Instead you'
    + ' must edit the parent grid to change the child grid.';
  StrColumnPositions = 'Column Positions';
  StrRowPositions = 'Row Positions';
  StrLayerElevations = 'Layer Elevations';

{$R *.dfm}

{ TfrmGridSpacing }

procedure TfrmGridSpacing.SetGrid(const AnArray: TOneDRealArray;
  const Grid: TRbwDataGrid4; const SpinEdit: TJvSpinEdit);
var
  Index: integer;
  Value: integer;
begin
  Value := Length(AnArray);
  if Value > 0 then
  begin
    SpinEdit.Value := Value;
  end
  else
  begin
    SpinEdit.Value := 1;
  end;

  Grid.RowCount := SpinEdit.AsInteger + 1;
  if Value = 0 then
  begin
    Grid.Cells[0, 1] := '1';
    Grid.Cells[1, 1] := '0';
  end
  else
  begin
    for Index := 1 to Value do
    begin
      Grid.Cells[0, Index] := IntToStr(Index);
      Grid.Cells[1, Index] := FloatToStr(AnArray[Index - 1]);
    end;
  end;
end;

procedure TfrmGridSpacing.GetData(Model: TCustomModel);
begin
  SetGrid(Model.Grid.ColumnPositions, dgColumns, seColumns);
  FColHeading := StrColumnPositions;
  dgColumns.Cells[1, 0] := FColHeading;

  SetGrid(Model.Grid.RowPositions, dgRows, seRows);
  FRowHeading := StrRowPositions;
  dgRows.Cells[1, 0] := FRowHeading;

  if frmGoPhast.ModelSelection = msPhast then
  begin
    tabLayers.TabVisible := True;
    SetGrid(Model.PhastGrid.LayerElevations, dgLayers, seLayers);
    FLayerHeading := StrLayerElevations;
    dgLayers.Cells[1, 0] := FLayerHeading;
  end
  else
  begin
    tabLayers.TabVisible := False;
  end;
end;

procedure TfrmGridSpacing.SetData;
var
//  Index: integer;
  UndoEditGridLines: TUndoEditGridLines;
  procedure GridToArray(var AnArray: TOneDRealArray; Grid: TRbwDataGrid4);
  var
    Index: integer;
    NumberList: TList<Double>;
    Value: double;
  begin
    NumberList := TList<Double>.Create;
    try
      NumberList.Capacity := Grid.RowCount -1;
      for Index := 1 to Grid.RowCount - 1 do
      begin
        if TryStrToFloat(Grid.Cells[1, Index], Value) then
        begin
          NumberList.Add(Value);
        end;
      end;
      if NumberList.Count = 0 then
      begin
        NumberList.Add(0);
      end;
      SetLength(AnArray, NumberList.Count);
      for Index := 0 to NumberList.Count - 1 do
      begin
        AnArray[Index] := NumberList[Index];
      end;
    finally
      NumberList.Free;
    end;
  end;
begin
  UndoEditGridLines := TUndoEditGridLines.Create;
  try

    GridToArray(UndoEditGridLines.FNewColumns, dgColumns);
//    SetLength(UndoEditGridLines.FNewColumns, dgColumns.RowCount - 1);
//    for Index := 1 to dgColumns.RowCount - 1 do
//    begin
//      try
//         UndoEditGridLines.FNewColumns[Index - 1] :=
//          StrToFloat(dgColumns.Cells[1, Index]);
//      except on EConvertError do
//          UndoEditGridLines.FNewColumns[Index - 1] := 0;
//      end;
//    end;

    GridToArray(UndoEditGridLines.FNewRows, dgRows);
//    SetLength(UndoEditGridLines.FNewRows, dgRows.RowCount - 1);
//    for Index := 1 to dgRows.RowCount - 1 do
//    begin
//      try
//        UndoEditGridLines.FNewRows[Index - 1] :=
//          StrToFloat(dgRows.Cells[1, Index]);
//      except on EConvertError do
//          UndoEditGridLines.FNewRows[Index - 1] := 0;
//      end;
//    end;

    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      GridToArray(UndoEditGridLines.FNewLayerElevations, dgLayers);
//      SetLength(UndoEditGridLines.FNewLayerElevations, dgLayers.RowCount - 1);
//      for Index := 1 to dgLayers.RowCount - 1 do
//      begin
//        try
//          UndoEditGridLines.FNewLayerElevations[Index - 1] :=
//            StrToFloat(dgLayers.Cells[1, Index]);
//        except on EConvertError do
//            UndoEditGridLines.FNewLayerElevations[Index - 1] := 0;
//        end;
//      end;
    end
    else
    begin
      SetLength(UndoEditGridLines.FNewLayerElevations, 0);
    end;

  except
    UndoEditGridLines.Free;
    raise;
  end;

  frmGoPhast.UndoStack.Submit(UndoEditGridLines);
end;

procedure TfrmGridSpacing.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGridSpacing.UpdateGrid(const NewCount: integer;
  const rdeSpacing: TRbwDataEntry; const Grid: TRbwDataGrid4);
var
  PreviousCell: string;
  PreviousDouble: double;
  OldCount: integer;
  Index: integer;
  Spacing: double;
  Count: integer;
begin
  if rdeSpacing.Text = '' then
  begin
    Exit;
  end;
  Spacing := StrToFloat(rdeSpacing.Text);

  PreviousCell := Grid.Cells[1, Grid.RowCount - 1];
  if Trim(PreviousCell) = '' then
  begin
    PreviousDouble := 0
  end
  else
  begin
    try
      PreviousDouble := StrToFloat(PreviousCell);
    except on EConvertError do
      begin
        PreviousDouble := 0;
      end;
    end;
  end;

  OldCount := Grid.RowCount;
  Grid.RowCount := NewCount + 1;
  Count := 0;
  for Index := OldCount to Grid.RowCount - 1 do
  begin
    Inc(Count);
    Grid.Cells[0, Index] := IntToStr(Index);
    //    if Grid.Cells[1, Index] = '' then
    begin
      Grid.Cells[1, Index] := FloatToStr(PreviousDouble + Count * Spacing);
    end;
  end;
  if Grid.RowCount > 1 then
  begin
    Grid.FixedRows := 1;
    SetHeadings;
  end;
end;

procedure TfrmGridSpacing.seColumnsChange(Sender: TObject);
begin
  inherited;
  UpdateGrid(seColumns.AsInteger, rdeSpacingColumns, dgColumns);
end;

procedure TfrmGridSpacing.seRowsChange(Sender: TObject);
begin
  inherited;
  UpdateGrid(seRows.AsInteger, rdeSpacingRows, dgRows);
end;

procedure TfrmGridSpacing.seLayersChange(Sender: TObject);
begin
  inherited;
  UpdateGrid(seLayers.AsInteger, rdeSpacingLayers, dgLayers);
end;

procedure TfrmGridSpacing.FormCreate(Sender: TObject);
begin
  inherited;
  pnlBottom.ParentColor := True;
  pcSubdivide.ActivePageIndex := 0;
  pcSubdivide.TabHeight := Abs(pcSubdivide.Font.Height) + 8;
  lblDescribe.Width := pnlDescribe.Width - 16;
  lblDescribe.Height := pnlDescribe.Height - 16;
  dgColumns.ColWidths[0] := 60;
  dgRows.ColWidths[0] := 60;
  dgLayers.ColWidths[0] := 60;
  case frmGoPhast.PhastModel.ModelSelection of
    msPhast:
      begin
        // do nothing
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        rdeSpacingRows.Text := '-100';
      end;
    else
      Assert(False);
  end;

  FillComboWithModelNames(comboModel);
  comboModel.Visible := frmGoPhast.PhastModel.LgrUsed;
  lblModel.Visible := comboModel.Visible;
  FLabelText := lblDescribe.Caption;

  GetData(frmGoPhast.PhastModel);
end;

procedure TfrmGridSpacing.PasteMultipleCells(const ADataGrid: TRbwDataGrid4;
  const ARow: Integer; const Value: String);
var
  Lines: TStringList;
  Index: integer;
  NewLines: TStringList;
  ALine: string;
  TabPos, CommaPos, SpacePos: integer;
  MinPos: integer;
  NewLine: string;
  ASpinEdit: TJvSpinEdit;
begin
  if ADataGrid = dgColumns then
  begin
    ASpinEdit := seColumns;
  end
  else if ADataGrid = dgRows then
  begin
    ASpinEdit := self.seRows;
  end
  else if ADataGrid = dgLayers then
  begin
    ASpinEdit := seLayers;
  end
  else
  begin
    ASpinEdit := nil;
    Assert(False)
  end;

  Lines := TStringList.Create;
  NewLines := TStringList.Create;
  try
    Lines.Text := Value;
    for Index := 0 to Lines.Count - 1 do
    begin
      ALine := Trim(Lines[Index]);
      TabPos := Pos(#9, ALine);
      if FormatSettings.DecimalSeparator = ',' then
      begin
        CommaPos := -1;
      end
      else
      begin
        CommaPos := Pos(',', ALine);
      end;
      SpacePos := Pos(' ', ALine);
      while (TabPos > 0) or (CommaPos > 0) or (SpacePos > 0) do
      begin
        MinPos := 0;
        if (TabPos > 0) then
        begin
          MinPos := TabPos;
        end;
        if CommaPos > 0 then
        begin
          if (MinPos = 0) or (CommaPos < MinPos) then
          begin
            MinPos := CommaPos;
          end;
        end;
        if SpacePos > 0 then
        begin
          if (MinPos = 0) or (SpacePos < MinPos) then
          begin
            MinPos := SpacePos;
          end;
        end;
        NewLine := Trim(Copy(ALine, 1, MinPos - 1));
        if NewLine <> '' then
        begin
          NewLines.Add(NewLine);
        end;
        ALine := Trim(Copy(ALine, MinPos + 1, MAXINT));

        TabPos := Pos(#9, ALine);
        if FormatSettings.DecimalSeparator = ',' then
        begin
          CommaPos := -1;
        end
        else
        begin
          CommaPos := Pos(',', ALine);
        end;
//        CommaPos := Pos(',', ALine);
        SpacePos := Pos(' ', ALine);
      end;
      if ALine <> '' then
      begin
        NewLines.Add(ALine);
      end;
    end;

    if NewLines.Count + ARow > ADataGrid.RowCount then
    begin
      ASpinEdit.Value := NewLines.Count + ARow - 1;
      ASpinEdit.OnChange(ASpinEdit);
    end;
    for Index := 0 to NewLines.Count - 1 do
    begin
      ADataGrid.Cells[1, Index + ARow] := NewLines[Index];
    end;
  finally
    Lines.Free;
    NewLines.Free;
  end;

end;

procedure TfrmGridSpacing.dgSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  DataGrid: TRbwDataGrid4;
  ShouldSplit: Boolean;
begin
  inherited;
  DataGrid := Sender as TRbwDataGrid4;
  ShouldSplit := (Pos(#10, Value) > 0) or (Pos(#13, Value) > 0) or (Pos(' ', Value) > 0)
    or (Pos(#9, Value) > 0);
  if FormatSettings.DecimalSeparator <>',' then
  begin
    ShouldSplit := ShouldSplit or (Pos(',', Value) > 0);
  end;
  if ShouldSplit then
  begin
    PasteMultipleCells(DataGrid, ARow, Value);
  end;
end;

procedure TfrmGridSpacing.comboModelChange(Sender: TObject);
var
  Model: TCustomModel;
begin
  inherited;
  Model := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  GetData(Model);
  if Model is TPhastModel then
  begin
    lblDescribe.Caption := FLabelText;
    btnOK.Enabled := True;
  end
  else
  begin
    lblDescribe.Caption := FLabelText + StrChildGridsCanNot;
    btnOK.Enabled := False;
  end;
end;

procedure TfrmGridSpacing.dgKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Value: string;
  DataGrid: TRbwDataGrid4;
begin
  inherited;
    // @name is the OnKeyUp event handler for @link(dgColumns).
    // @name checks whether an end-or-line occurs in the pasted text.
    // If so, it calls @link(PasteMultipleCells).
  if (Char(Key) = 'V') and (Shift = [ssCtrl]) then
  begin
    DataGrid := Sender as TRbwDataGrid4;
    Value := ClipBoard.AsText;
    if (Pos(#10, Value) > 0) or (Pos(#13, Value) > 0) then
    begin
      PasteMultipleCells(DataGrid, DataGrid.Row, Value);
    end;
  end;
end;

procedure TfrmGridSpacing.dgSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if ARow = 0 then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmGridSpacing.SetHeadings;
begin
  dgColumns.Cells[1, 0] := FColHeading;
  dgRows.Cells[1, 0] := FRowHeading;
  dgLayers.Cells[1, 0] := FLayerHeading;
end;

procedure TfrmGridSpacing.FormKeyPress(Sender: TObject; var Key: Char);
var
  Grid: TRbwDataGrid4;
begin
  // Move to next row when the user depresses the return key.

  // It isn't clear that this works.  The ActiveControl may never be
  // a TInplaceEdit.
  if Key = #13 then
  begin
    if ActiveControl is TInplaceEdit then
    begin
      Grid := ActiveControl.Owner as TRbwDataGrid4;
      if Grid.Row < Grid.RowCount - 1 then
      begin
        Grid.Row := Grid.Row + 1;
      end;
    end;
  end;
  inherited;
end;

procedure TfrmGridSpacing.SetCursor(const ACol, ARow: integer);
begin
  // If the cursor is over the left hand column, the user can
  // drag the rows to rearrange the objects. Use crHandGrab
  // to indicate that the rows are being rearranged or crHandFlat
  // to indicate that they can be moved.  Otherwise, just use
  // the arrow cursor.
  if (ARow > 0) and (ACol = 0) then
  begin
    if FDraggingRows then
    begin
      dgColumns.Cursor := crHandGrab;
    end
    else
    begin
      dgColumns.Cursor := crHandFlat;
    end;
  end
  else
  begin
    dgColumns.Cursor := crArrow;
  end;
  dgRows.Cursor := dgColumns.Cursor;
  dgLayers.Cursor := dgColumns.Cursor;
end;

procedure TfrmGridSpacing.dgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  DataGrid: TRbwDataGrid4;

begin
  inherited;
  DataGrid := Sender as TRbwDataGrid4;
//  if ([ssShift, ssCtrl] * Shift) = [] then
//  begin
//    DataGrid.Options := DataGrid.Options + [goEditing];
//  end
//  else
//  begin
//    DataGrid.Options := DataGrid.Options - [goEditing];
//  end;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  DataGrid.MouseToCell(X, Y, ACol, ARow);
  FDraggingRows := (ARow > 0) and (ACol = 0);
  SetCursor(ACol, ARow);
end;

procedure TfrmGridSpacing.dgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  (Sender as TRbwDataGrid4).MouseToCell(X, Y, ACol, ARow);
  SetCursor(ACol, ARow);
end;

procedure TfrmGridSpacing.dgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited;
  // rearranging the cells is handled by the control so it
  // doesn't need to be done here.  However, set the cursor
  // to give a visual indication of what is happening.
  (Sender as TRbwDataGrid4).MouseToCell(X, Y, ACol, ARow);
  FDraggingRows := False;
  SetCursor(ACol, ARow);
end;

end.

