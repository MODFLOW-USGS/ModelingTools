{
May 24, 2006: implemented setting hint of the top front and side view
@link(TQRbwZoomBox2 zoomboxes) with the current tool.
}

{@abstract(The main purpose of @name is to define @link(TfrmSetSpacing)
  and @link(TSpacingGridTool) which are used to
  change the size of the columns, rows,
  and/or layers of the @link(TPhastGrid).)}
unit frmSetSpacingUnit;

interface

uses
  // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit,
  Buttons, InteractiveTools, AbstractGridUnit, GoPhastTypes,
  ArgusDataEntry, Mask, JvExMask, JvSpin;

type
  {@abstract(@name is used to change the size of the columns, rows,
    and/or layers of the @link(TPhastGrid).)}
  TfrmSetSpacing = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    gbColumns: TGroupBox;
    cbColumns: TCheckBox;
    lblFromCol: TLabel;
    seCol1: TJvSpinEdit;
    lblThroughCol: TLabel;
    seCol2: TJvSpinEdit;
    lblSetCol: TLabel;
    rdeCol: TRbwDataEntry;
    gbRows: TGroupBox;
    cbRows: TCheckBox;
    lblFromRow: TLabel;
    seRow1: TJvSpinEdit;
    lblThroughRow: TLabel;
    seRow2: TJvSpinEdit;
    lblSetRow: TLabel;
    rdeRow: TRbwDataEntry;
    gbLayers: TGroupBox;
    cbLayers: TCheckBox;
    lblFromLayer: TLabel;
    seLayer1: TJvSpinEdit;
    lblThroughLayer: TLabel;
    seLayer2: TJvSpinEdit;
    lblSetLayer: TLabel;
    rdeLayer: TRbwDataEntry;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name is used to enable or disable @link(seCol1), @link(seCol2),
    // and @link(rdeCol),
    procedure cbColumnsClick(Sender: TObject);
    // @name is used to enable or disable @link(seLayer1), @link(seLayer2),
    // and @link(rdeLayer),
    procedure cbLayersClick(Sender: TObject);
    // @name is used to enable or disable @link(seRow1), @link(seRow2),
    // and @link(rdeRow),
    procedure cbRowsClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name causes the grid to be redrawn.
    procedure FormDestroy(Sender: TObject); override;
    // @name calls @link(EnableOK).
    procedure rdeChange(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingColumn)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingColumn)
    // and causes the grid to be redrawn.
    procedure seCol1Change(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingColumn)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingColumn)
    // and causes the grid to be redrawn.
    procedure seCol2Change(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingRow)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingRow)
    // and causes the grid to be redrawn.
    procedure seRow1Change(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingRow)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingRow)
    // and causes the grid to be redrawn.
    procedure seRow2Change(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingLayer)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingLayer)
    // and causes the grid to be redrawn.
    procedure seLayer1Change(Sender: TObject);
    // @name updates SpacingGridTool.@link(TSpacingGridTool.FFirstSpacingLayer)
    // or SpacingGridTool.@link(TSpacingGridTool.FLastSpacingLayer)
    // and causes the grid to be redrawn.
    procedure seLayer2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    // @name is used to indicate that the user has specified a value
    // for in @link(seCol1) that is greater than the one in @link(seCol2).
    FReversedCols: boolean;
    // @name is used to indicate that the user has specified a value
    // for in @link(seLayer1) that is greater than the one in @link(seLayer2).
    FReversedLayers: boolean;
    // @name is used to indicate that the user has specified a value
    // for in @link(seRow1) that is greater than the one in @link(seRow2).
    FReversedRows: boolean;
    FGettingData: Boolean;
    // @name makes sure that the OK button can only be clicked if there
    // is valid data specified in @classname.
    procedure EnableOK;
    // @name displays data from @link(SpacingGridTool) in @classname.
    procedure GetData;
    // @name changes the @link(TPhastGrid) using the values specified
    // in @classname using a @link(TUndoEditGridLines).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to set the size of selected columns, rows,
    or layers in @link(TPhastGrid).)}
  TSpacingGridTool = class(TCustomCellSelectionTool)
  private
    // @name: boolean;
    // @name is used in @link(DrawSetSpacing), @link(MouseMove), and @link(MouseUp)
    // to respond differently if the user is in the middle of selecting
    // cells in order to set their spacing.
    FSettingSpacing: boolean;
    procedure DrawSetSpacing(const Direction: TViewDirection;
      const BitMap: TBitmap32);
  protected
    // @name: integer;
    // See @link(FirstSpacingColumn).
    FFirstSpacingColumn: integer;
    // @name: integer;
    // See @link(FirstSpacingLayer).
    FFirstSpacingLayer: integer;
    // @name: integer;
    // See @link(FirstSpacingRow).
    FFirstSpacingRow: integer;
    // @name: integer;
    // See @link(LastSpacingColumn).
    FLastSpacingColumn: integer;
    // @name: integer;
    // See @link(LastSpacingLayer).
    FLastSpacingLayer: integer;
    // @name: integer;
    // See @link(LastSpacingRow).
    FLastSpacingRow: integer;
    // @name sets @link(FSettingSpacing) to @true
    //  and sets @link(FirstSpacingColumn), @link(FirstSpacingLayer),
    // @link(FirstSpacingRow) based on X,Y
    // @param(X X is the X-coordinate of the mouse).
    // @param(Y Y is the Y-coordinate of the mouse).
    procedure BeginSetSpacing(X, Y: Integer);
    // @name sets @link(LastSpacingColumn), @link(LastSpacingLayer),
    // @link(LastSpacingRow) based on X,Y
    // @param(X X is the X-coordinate of the mouse).
    // @param(Y Y is the Y-coordinate of the mouse).
    procedure ContinueSetSpacing(X, Y: integer);
    // Used to define @link(TCustomInteractiveTool.Hint).
    function GetHint: string; override;
    // @name shows an instance of @link(TfrmSetSpacing).
    procedure SetGridSpacing;
    // @name sets @link(LastSpacingColumn), @link(LastSpacingLayer),
    // @link(LastSpacingRow) based on X,Y on the front view of the model
    // and calls @link(SetGridSpacing).
    // @param(X X is the X-coordinate of the mouse).
    // @param(Y Y is the Y-coordinate of the mouse).
    procedure SetSpacingFront(X, Y: Integer);
    // @name sets @link(LastSpacingColumn), @link(LastSpacingLayer),
    // @link(LastSpacingRow) based on X,Y on the side view of the model
    // and calls @link(SetGridSpacing).
    // @param(X X is the X-coordinate of the mouse).
    // @param(Y Y is the Y-coordinate of the mouse).
    procedure SetSpacingSide(X, Y: Integer);
    // @name sets @link(LastSpacingColumn), @link(LastSpacingLayer),
    // @link(LastSpacingRow) based on X,Y on the top view of the model
    // and calls @link(SetGridSpacing).
    // @param(X X is the X-coordinate of the mouse).
    // @param(Y Y is the Y-coordinate of the mouse).
    procedure SetSpacingTop(X, Y: Integer);
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    // @name draws the selected cells in @link(TPhastGrid).
//    procedure Draw(const Sender: TObject; const Direction: TViewDirection);
//      override;
    // @name is the first column in the range of columns that have been
    // selected in order to set their size.
    // See @link(LastSpacingColumn).
    property FirstSpacingColumn: integer read FFirstSpacingColumn;
    // @name is the first layer in the range of layers that have been
    // selected in order to set their size.
    // See @link(LastSpacingRow).
    property FirstSpacingLayer: integer read FFirstSpacingLayer;
    // @name is the first row in the range of rows that have been
    // selected in order to set their size.
    // See @link(LastSpacingRow).
    property FirstSpacingRow: integer read FFirstSpacingRow;
    // @name is the last column in the range of columns that have been
    // selected in order to set their size.
    // See @link(FirstSpacingColumn).
    property LastSpacingColumn: integer read FLastSpacingColumn;
    // @name is the last layer in the range of layers that have been
    // selected in order to set their size.
    // See @link(FirstSpacingLayer).
    property LastSpacingLayer: integer read FLastSpacingLayer;
    // @name is the last row in the range of rows that have been
    // selected in order to set their size.
    // See @link(FirstSpacingRow).
    property LastSpacingRow: integer read FLastSpacingRow;
    // @name calls @link(BeginSetSpacing).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name calls @link(ContinueSetSpacing).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name calls @link(SetSpacingTop),
    // @link(SetSpacingFront), or @link(SetSpacingSide).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

var
  // @name is the instance of @link(TSpacingGridTool) used in GoPhast.
  SpacingGridTool: TSpacingGridTool;

implementation

uses frmGoPhastUnit, UndoItems, FastGEO;

resourcestring
  StrClickOnGridAndDr = 'Click on grid and drag to set grid spacing';

{$R *.dfm}

{ TfrmSetSpacing }

procedure TfrmSetSpacing.EnableOK;
begin
  if
    (cbColumns = nil)
    or (cbRows = nil)
    or (cbLayers = nil)
    or (btnOK = nil) then
  begin
    Exit;
  end;
  btnOK.Enabled := cbColumns.Checked or cbRows.Checked or cbLayers.Checked;
  if not btnOK.Enabled then
    Exit;
  if cbColumns.Checked then
  begin
    if rdeCol.Text = '' then
    begin
      btnOK.Enabled := False;
    end;
  end;
  if cbRows.Checked then
  begin
    if rdeRow.Text = '' then
    begin
      btnOK.Enabled := False;
    end;
  end;
  if cbLayers.Checked then
  begin
    if rdeLayer.Text = '' then
    begin
      btnOK.Enabled := False;
    end;
  end;
end;

procedure TfrmSetSpacing.GetData;
var
  temp: integer;
  Spacing: real;
begin
  FGettingData := True;
  try
    FReversedCols := False;
    FReversedRows := False;
    FReversedLayers := False;
    seCol1.MinValue := 1;
    seCol2.MinValue := 1;
    seRow1.MinValue := 1;
    seRow2.MinValue := 1;
    seLayer1.MinValue := 1;
    seLayer2.MinValue := 1;

    seCol1.MaxValue := frmGoPhast.Grid.ColumnCount;
    seCol2.MaxValue := frmGoPhast.Grid.ColumnCount;
    seRow1.MaxValue := frmGoPhast.Grid.RowCount;
    seRow2.MaxValue := frmGoPhast.Grid.RowCount;
    seLayer1.MaxValue := frmGoPhast.Grid.LayerCount;
    seLayer2.MaxValue := frmGoPhast.Grid.LayerCount;

    seLayer1.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;
    seLayer2.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;

    if (SpacingGridTool.FirstSpacingColumn >= 0)
      and (SpacingGridTool.LastSpacingColumn >= 0) then
    begin
      seCol1.Value := SpacingGridTool.FirstSpacingColumn + 1;
      seCol2.Value := SpacingGridTool.LastSpacingColumn + 1;
      if seCol1.Value > seCol2.Value then
      begin
        FReversedCols := True;
        Temp := seCol1.AsInteger;
        seCol1.Value := seCol2.Value;
        seCol2.Value := Temp;
      end;
      with frmGoPhast.Grid do
      begin
        Spacing := (ColumnPosition[seCol2.AsInteger]
          - ColumnPosition[seCol1.AsInteger - 1]) / (seCol2.AsInteger - seCol1.Value +
          1);
        if frmGoPhast.Grid.ColumnDirection = cdEastToWest then
        begin
          Spacing := -Spacing;
        end;
        rdeCol.Text := FloatToStr(Spacing);
      end;
    end
    else
    begin
      cbColumns.Enabled := False;
    end;
    if (SpacingGridTool.FirstSpacingRow >= 0)
      and (SpacingGridTool.LastSpacingRow >= 0) then
    begin
      seRow1.Value := SpacingGridTool.FirstSpacingRow + 1;
      seRow2.Value := SpacingGridTool.LastSpacingRow + 1;
      if seRow1.Value > seRow2.Value then
      begin
        FReversedRows := True;
        Temp := seRow1.AsInteger;
        seRow1.Value := seRow2.Value;
        seRow2.Value := Temp;
      end;
      with frmGoPhast.Grid do
      begin
        Spacing := (RowPosition[seRow2.AsInteger]
          - RowPosition[seRow1.AsInteger - 1]) / (seRow2.AsInteger - seRow1.Value + 1);
        if frmGoPhast.Grid.RowDirection = rdNorthToSouth then
        begin
          Spacing := -Spacing;
        end;
        rdeRow.Text := FloatToStr(Spacing);
      end;
    end
    else
    begin
      cbRows.Enabled := False;
    end;
    if (SpacingGridTool.FirstSpacingLayer >= 0)
      and (SpacingGridTool.LastSpacingLayer >= 0) then
    begin
      seLayer1.Value := SpacingGridTool.FirstSpacingLayer + 1;
      seLayer2.Value := SpacingGridTool.LastSpacingLayer + 1;
      if seLayer1.Value > seLayer2.Value then
      begin
        FReversedLayers := True;
        Temp := seLayer1.AsInteger;
        seLayer1.Value := seLayer2.Value;
        seLayer2.Value := Temp;
      end;
      if frmGoPhast.PhastModel.ModelSelection = msPhast then
      begin
        with frmGoPhast.PhastGrid do
        begin
          Spacing := (LayerElevation[seLayer2.AsInteger]
              - LayerElevation[seLayer1.AsInteger - 1]) / (seLayer2.Value - seLayer1.Value
            + 1);
        if frmGoPhast.Grid.LayerDirection = ldTopToBottom then
        begin
          Spacing := -Spacing;
        end;
          rdeLayer.Text := FloatToStr(Spacing);
        end;
      end;
    end
    else
    begin
      cbLayers.Enabled := False;
    end;
    seCol1.MaxValue := frmGoPhast.Grid.ColumnCount;
    seCol2.MaxValue := frmGoPhast.Grid.ColumnCount;
    seRow1.MaxValue := frmGoPhast.Grid.RowCount;
    seRow2.MaxValue := frmGoPhast.Grid.RowCount;
    seLayer1.MaxValue := frmGoPhast.Grid.LayerCount;
    seLayer2.MaxValue := frmGoPhast.Grid.LayerCount;

    if frmGoPhast.PhastModel.ModelSelection <> msPhast then
    begin
      seLayer1.Enabled := False;
      seLayer2.Enabled := False;
      rdeLayer.Enabled := False;
    end;
  finally
    FGettingData := False;
  end;

//  SetSpinColor;
end;

procedure TfrmSetSpacing.SetData;
var
  Index: integer;
  UndoEditGridLines: TUndoEditGridLines;
  First, Last, Temp: integer;
  NewSpacing: real;
  Delta, PrevDelta: real;
begin
  UndoEditGridLines := TUndoEditGridLines.Create;
  try
    SetLength(UndoEditGridLines.FNewColumns,
      frmGoPhast.Grid.ColumnCount + 1);
    if cbColumns.Checked then
    begin
      First := seCol1.AsInteger;
      Last := seCol2.AsInteger;
      if First > Last then
      begin
        Temp := First;
        First := Last;
        Last := Temp;
      end;
      NewSpacing := StrToFloat(rdeCol.Text);
      if frmGoPhast.Grid.ColumnDirection = cdEastToWest then
      begin
        NewSpacing := -NewSpacing;
      end;
      PrevDelta := frmGoPhast.Grid.ColumnPosition[Last]
        - frmGoPhast.Grid.ColumnPosition[First - 1];
      for Index := 0 to frmGoPhast.Grid.ColumnCount do
      begin
        if Index < First then
        begin
          UndoEditGridLines.FNewColumns[Index] :=
            frmGoPhast.Grid.ColumnPosition[Index];
        end
        else if Index <= Last then
        begin
          Delta := (Index - First + 1) * NewSpacing;
          UndoEditGridLines.FNewColumns[Index] :=
            frmGoPhast.Grid.ColumnPosition[First - 1] + Delta;
        end
        else
        begin
          Delta := (Last - First + 1) * NewSpacing;
          UndoEditGridLines.FNewColumns[Index] :=
            frmGoPhast.Grid.ColumnPosition[Index] + Delta - PrevDelta;
        end;
      end;
    end
    else
    begin
      Move(frmGoPhast.Grid.ColumnPositions[0],
        UndoEditGridLines.FNewColumns[0],
        (frmGoPhast.Grid.ColumnCount + 1) * SizeOf(real));
    end;

    SetLength(UndoEditGridLines.FNewRows, frmGoPhast.Grid.RowCount + 1);
    if cbRows.Checked then
    begin
      First := seRow1.AsInteger;
      Last := seRow2.AsInteger;
      if First > Last then
      begin
        Temp := First;
        First := Last;
        Last := Temp;
      end;
      NewSpacing := StrToFloat(rdeRow.Text);
      if frmGoPhast.Grid.RowDirection = rdNorthToSouth then
      begin
        NewSpacing := -NewSpacing;
      end;
      PrevDelta := frmGoPhast.Grid.RowPosition[Last]
        - frmGoPhast.Grid.RowPosition[First - 1];
      for Index := 0 to frmGoPhast.Grid.RowCount do
      begin
        if Index < First then
        begin
          UndoEditGridLines.FNewRows[Index] :=
            frmGoPhast.Grid.RowPosition[Index];
        end
        else if Index <= Last then
        begin
          Delta := (Index - First + 1) * NewSpacing;
          UndoEditGridLines.FNewRows[Index] :=
            frmGoPhast.Grid.RowPosition[First - 1] + Delta;
        end
        else
        begin
          Delta := (Last - First + 1) * NewSpacing;
          UndoEditGridLines.FNewRows[Index] :=
            frmGoPhast.Grid.RowPosition[Index] + Delta - PrevDelta;
        end;
      end;
    end
    else
    begin
      Move(frmGoPhast.Grid.RowPositions[0],
        UndoEditGridLines.FNewRows[0],
        (frmGoPhast.Grid.RowCount + 1) * SizeOf(real));
    end;


    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      SetLength(UndoEditGridLines.FNewLayerElevations,
        frmGoPhast.PhastGrid.LayerCount + 1);
      if cbLayers.Checked then
      begin
        First := seLayer1.AsInteger;
        Last := seLayer2.AsInteger;
        if First > Last then
        begin
          Temp := First;
          First := Last;
          Last := Temp;
        end;
        NewSpacing := StrToFloat(rdeLayer.Text);
        if frmGoPhast.Grid.LayerDirection = ldTopToBottom then
        begin
          NewSpacing := -NewSpacing;
        end;
        PrevDelta := frmGoPhast.PhastGrid.LayerElevation[Last]
          - frmGoPhast.PhastGrid.LayerElevation[First - 1];
        for Index := 0 to frmGoPhast.PhastGrid.LayerCount do
        begin
          if Index < First then
          begin
            UndoEditGridLines.FNewLayerElevations[Index] :=
              frmGoPhast.PhastGrid.LayerElevation[Index];
          end
          else if Index <= Last then
          begin
            Delta := (Index - First + 1) * NewSpacing;
            UndoEditGridLines.FNewLayerElevations[Index] :=
              frmGoPhast.PhastGrid.LayerElevation[First - 1] + Delta;
          end
          else
          begin
            Delta := (Last - First + 1) * NewSpacing;
            UndoEditGridLines.FNewLayerElevations[Index] :=
              frmGoPhast.PhastGrid.LayerElevation[Index] + Delta - PrevDelta;
          end;
        end;
      end
      else
      begin
        Move(frmGoPhast.PhastGrid.LayerElevations[0],
          UndoEditGridLines.FNewLayerElevations[0],
          (frmGoPhast.PhastGrid.LayerCount + 1) * SizeOf(real));
      end;
    end;
  except
    UndoEditGridLines.Free;
    raise;
  end;

  frmGoPhast.UndoStack.Submit(UndoEditGridLines);
end;

procedure TfrmSetSpacing.cbColumnsClick(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  EnableOK;
  seCol1.Enabled := cbColumns.Checked;
  seCol2.Enabled := cbColumns.Checked;
  rdeCol.Enabled := cbColumns.Checked;
//  SetSpinColor;
end;

procedure TfrmSetSpacing.cbRowsClick(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  EnableOK;
  seRow1.Enabled := cbRows.Checked;
  seRow2.Enabled := cbRows.Checked;
  rdeRow.Enabled := cbRows.Checked;
//  SetSpinColor;
end;

procedure TfrmSetSpacing.cbLayersClick(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  EnableOK;
  seLayer1.Enabled := cbLayers.Checked;
  seLayer2.Enabled := cbLayers.Checked;
  rdeLayer.Enabled := cbLayers.Checked;
//  SetSpinColor;
end;

procedure TfrmSetSpacing.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  SpacingGridTool.FSettingSpacing := False;
  SpacingGridTool.Layer32.Changed;
  SpacingGridTool.View.ZoomBox.InvalidateImage32;

end;

procedure TfrmSetSpacing.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSetSpacing.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSetSpacing.FormDestroy(Sender: TObject);
begin
  inherited;
  SpacingGridTool.FSettingSpacing := False;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TfrmSetSpacing.rdeChange(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  EnableOK;
end;

procedure TfrmSetSpacing.seCol1Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedCols then
    begin
      SpacingGridTool.FLastSpacingColumn := seCol1.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FFirstSpacingColumn := seCol1.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSetSpacing.seCol2Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedCols then
    begin
      SpacingGridTool.FFirstSpacingColumn := seCol2.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FLastSpacingColumn := seCol2.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSetSpacing.seRow1Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedRows then
    begin
      SpacingGridTool.FLastSpacingRow := seRow1.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FFirstSpacingRow := seRow1.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSetSpacing.seRow2Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedRows then
    begin
      SpacingGridTool.FFirstSpacingRow := seRow2.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FLastSpacingRow := seRow2.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSetSpacing.seLayer1Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedLayers then
    begin
      SpacingGridTool.FLastSpacingLayer := seLayer1.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FFirstSpacingLayer := seLayer1.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSetSpacing.seLayer2Change(Sender: TObject);
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
  if not (csCreating in ControlState) then
  begin
    if FReversedLayers then
    begin
      SpacingGridTool.FFirstSpacingLayer := seLayer2.AsInteger - 1;
    end
    else
    begin
      SpacingGridTool.FLastSpacingLayer := seLayer2.AsInteger - 1;
    end;
    SpacingGridTool.Layer32.Changed;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

{ TSpacingGridTool }

procedure TSpacingGridTool.BeginSetSpacing(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to determine and store the
  // first column, row, and layer that will be subdivided.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FFirstSpacingColumn := Column;
          FFirstSpacingRow := Row;
          FFirstSpacingLayer := -1;
          FSettingSpacing := True;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FFirstSpacingColumn := Column;
          FFirstSpacingRow := -1;
          FFirstSpacingLayer := Layer;
          FSettingSpacing := True;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FFirstSpacingColumn := -1;
          FFirstSpacingRow := Row;
          FFirstSpacingLayer := Layer;
          FSettingSpacing := True;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TSpacingGridTool.ContinueSetSpacing(X, Y: integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to update the view on the screen when
  // setting the spacing of rows, columns or layers.

  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FLastSpacingColumn := Column;
          FLastSpacingRow := Row;
          ZoomBox.InvalidateImage32;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FLastSpacingLayer := Layer;
          FLastSpacingColumn := Column;
          ZoomBox.InvalidateImage32;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FLastSpacingLayer := Layer;
          FLastSpacingRow := Row;
          ZoomBox.InvalidateImage32;
        end;
      end;
  else
    Assert(False);
  end;
end;

//procedure TSpacingGridTool.Draw(const Sender: TObject;
//  const Direction: TViewDirection);
//begin
//  DrawSetSpacing(Direction);
//end;

procedure TSpacingGridTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  if FButton <> mbMiddle then
  begin
    if (frmGoPhast.CurrentTool = self)
      and (Sender = Layer32)
      and (frmGoPhast.CursorGrid = View.CursorGrid) then
    begin
      DrawSetSpacing(View.ViewDirection, Buffer);
    end;
  end;
end;

function TSpacingGridTool.GetHint: string;
begin
  result := StrClickOnGridAndDr;
end;

procedure TSpacingGridTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    BeginSetSpacing(X, Y);
  end;
end;

procedure TSpacingGridTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FSettingSpacing then
  begin
    ContinueSetSpacing(X, Y);
  end;
end;

procedure TSpacingGridTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FSettingSpacing then
    begin
      // Set the spacing of the selected cells.
      case ViewDirection of
        vdTop: SetSpacingTop(X, Y);
        vdFront: SetSpacingFront(X, Y);
        vdSide: SetSpacingSide(X, Y);
      else
        Assert(False);
      end;
    end;
  end;
  inherited;
end;

procedure TSpacingGridTool.DrawSetSpacing(const Direction: TViewDirection;
  const BitMap: TBitmap32);
begin
  if FSettingSpacing then
  begin
    case Direction of
      vdTop:
        begin
          DrawSelectedTopCells(FirstSpacingColumn,
            LastSpacingColumn,
            FirstSpacingRow,
            LastSpacingRow,
            BitMap, Direction);
        end;
      vdFront:
        begin
          DrawSelectedFrontCells(FirstSpacingColumn,
            LastSpacingColumn,
            FirstSpacingLayer,
            LastSpacingLayer,
            BitMap, Direction);
        end;
      vdSide:
        begin
          DrawSelectedSideCells(FirstSpacingRow,
            LastSpacingRow,
            FirstSpacingLayer,
            LastSpacingLayer,
            BitMap, Direction);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TSpacingGridTool.SetGridSpacing;
begin
  with TfrmSetSpacing.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TSpacingGridTool.SetSpacingFront(X, Y: Integer);
var
  APoint: TPoint2D;
  Layer, Col: integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetColLayer(APoint, Col, Layer);
  if (Col >= 0) and (Layer >= 0) then
  begin
    FLastSpacingColumn := Col;
    FLastSpacingRow := -1;
    FLastSpacingLayer := Layer;
  end;
  // set the spacing for the column and layer.
  SetGridSpacing;
end;

procedure TSpacingGridTool.SetSpacingSide(X, Y: Integer);
var
  APoint: TPoint2D;
  Layer, Row: integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    FLastSpacingColumn := -1;
    FLastSpacingRow := Row;
    FLastSpacingLayer := Layer;
  end;
  // set the spacing for the row and layer.
  SetGridSpacing;
end;

procedure TSpacingGridTool.SetSpacingTop(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  // get the column and row containing the mouse position.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowCol(APoint, Row, Column);
  if (Row >= 0) and (Column >= 0) then
  begin
    FLastSpacingColumn := Column;
    FLastSpacingRow := Row;
    FLastSpacingLayer := -1;
  end;
  // set the spacing for the column and row.
  SetGridSpacing;
end;

initialization
  SpacingGridTool := TSpacingGridTool.Create(nil);

finalization
  SpacingGridTool.Free;

end.


