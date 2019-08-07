{
May 24, 2006: implemented setting hint of the top front and side view
@link(TQRbwZoomBox2 zoomboxes) with the current tool.
}

{@abstract(The main purpose of @name is to define @link(TfrmSubdivide)
  and @link(TSubdivideGridTool) which are used
  to subdivide elements in @link(TPhastGrid) into multiple
  columns, rows, and layers.)}
unit frmSubdivideUnit;

interface

uses
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, AbstractGridUnit,
  ComCtrls, InteractiveTools, GoPhastTypes, Spin, Mask, JvExMask, JvSpin;

type
  {@abstract(@name is used in conjunction with @link(TSubdivideGridTool)
    to subdivide elements in @link(TPhastGrid) into multiple
    columns, rows, and layers.)}
  TfrmSubdivide = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name displays "From &row".
    lblFromRow: TLabel;
    // @name displays "rows".
    lblRows: TLabel;
    // @name displays "subdivide each row into".
    lblSubdivideRow: TLabel;
    // @name displays "through row".
    lblThroughRow: TLabel;
    // @name specifies the first row to subdivide.
    seRow1: TJvSpinEdit;
    // @name specifies the first row to subdivide.
    seRow2: TJvSpinEdit;
    // @name specifies how many rows each selected row will be
    // divided into.
    seRowCount: TJvSpinEdit;
    GroupBox1: TGroupBox;
    lblFromCol: TLabel;
    seCol1: TJvSpinEdit;
    lblThroughCol: TLabel;
    seCol2: TJvSpinEdit;
    lblSubdivideCol: TLabel;
    seColCount: TJvSpinEdit;
    lblColumns: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    lblFromLayer: TLabel;
    seLayer1: TJvSpinEdit;
    lblThroughLayer: TLabel;
    seLayer2: TJvSpinEdit;
    lblSubdivideLayer: TLabel;
    seLayerCount: TJvSpinEdit;
    lblLayers: TLabel;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name causes the view of the grid to be redrawn.
    procedure FormDestroy(Sender: TObject); override;
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seCol1Changed(Sender: TObject);
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seCol2Changed(Sender: TObject);
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seLayer1Changed(Sender: TObject);
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seLayer2Changed(Sender: TObject);
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seRow1Changed(Sender: TObject);
    // @name updates @link(SubdivideGridTool) and causes
    // the grid to be redrawn.
    procedure seRow2Changed(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure seColCountChange(Sender: TObject);
    procedure seRowCountChange(Sender: TObject);

  private
    FGettingData: Boolean;
    // @name subdivides the selected grid cells using a @link(TUndoSubdivide).
    procedure SetData;

  { Private declarations }
  public
    // @name gets the selected columns, rows, and layers to subdivide.
    procedure GetData(const ACol, ARow, ALayer: integer);
    { Public declarations }
  end;

  {@abstract(@name is used to subdivide elements in @link(TPhastGrid)
    into multiple columns, rows, and layers.)}
  TSubdivideGridTool = class(TCustomCellSelectionTool)
  protected
    // @name: integer;
    // See @link(FirstSubdivideColumn).
    FFirstSubdivideColumn: integer;
    // @name: integer;
    // See @link(FirstSubdivideLayer).
    FFirstSubdivideLayer: integer;
    // @name: integer;
    // See @link(FirstSubdivideRow).
    FFirstSubdivideRow: integer;
    // @name: integer;
    // See @link(LastSubdivideColumn).
    FLastSubdivideColumn: integer;
    // @name: integer;
    // See @link(LastSubdivideLayer).
    FLastSubdivideLayer: integer;
    // @name: integer;
    // See @link(LastSubdivideRow).
    FLastSubdivideRow: integer;
    // @name: boolean;
    // See @link(Subdividing).
    FSubdividing: boolean;
    // @name is sets @link(FFirstSubdivideColumn), @link(FFirstSubdivideLayer),
    // and @link(FFirstSubdivideRow) based on X,Y and
    // @link(TCustomInteractiveTool.ViewDirection).
    // @param(X X is the X-coordinate of the mouse.)
    // @param(Y Y is the Y-coordinate of the mouse.)
    procedure BeginSubdivide(X, Y: Integer);
    // @name is sets @link(FLastSubdivideColumn), @link(FLastSubdivideLayer),
    // and @link(FLastSubdivideRow) based on X,Y and
    // @link(TCustomInteractiveTool.ViewDirection).
    // @param(X X is the X-coordinate of the mouse.)
    // @param(Y Y is the Y-coordinate of the mouse.)
    procedure ContinueSubdivide(X, Y: integer);
    procedure DrawSubdividing(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    // Used to define @link(TCustomInteractiveTool.Hint).
    function GetHint: string; override;
    // @name subdivides the selected columns and layers on the front view of
    // the model.
    // @param(X X is the X-coordinate of the mouse.)
    // @param(Y Y is the Y-coordinate of the mouse.)
    procedure SubdivideFront(X, Y: Integer);
    // @name subdivides the selected rows and layers on the side view of
    // the model.
    // @param(X X is the X-coordinate of the mouse.)
    // @param(Y Y is the Y-coordinate of the mouse.)
    procedure SubdivideSide(X, Y: Integer);
    // @name subdivides the selected columns and rows on the top view of
    // the model.
    // @param(X X is the X-coordinate of the mouse.)
    // @param(Y Y is the Y-coordinate of the mouse.)
    procedure SubdivideTop(X, Y: Integer);
    // @name displays a @link(TfrmSubdivide).
    procedure Subdivide(const LastCol, LastRow, LastLayer: integer);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    // @name draws the selected elements.
//    procedure Draw(const Sender: TObject; const Direction: TViewDirection);
//      override;
    // @name is the first selected column to be subdivided.
    property FirstSubdivideColumn: integer read FFirstSubdivideColumn;
    // @name is the first selected layer to be subdivided.
    property FirstSubdivideLayer: integer read FFirstSubdivideLayer;
    // @name is the first selected row to be subdivided.
    property FirstSubdivideRow: integer read FFirstSubdivideRow;
    // @name is the last selected column to be subdivided.
    property LastSubdivideColumn: integer read FLastSubdivideColumn;
    // @name is the last selected layer to be subdivided.
    property LastSubdivideLayer: integer read FLastSubdivideLayer;
    // @name is the last selected row to be subdivided.
    property LastSubdivideRow: integer read FLastSubdivideRow;
    // @name calls @link(BeginSubdivide).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name calls @link(ContinueSubdivide).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name calls @link(SubdivideTop), @link(SubdivideFront), or
    // @link(SubdivideSide) depending on
    // @link(TCustomInteractiveTool.ViewDirection).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name indicates whether the user is in the process of subdividing
    // grid elements.
    property Subdividing: boolean read FSubdividing write FSubdividing;
  end;

var
  // @name is the instance of @link(TSubdivideGridTool) used in GoPhast.
  SubdivideGridTool: TSubdivideGridTool;

implementation

uses frmGoPhastUnit, UndoItems, FastGEO;

resourcestring
  StrClickOnGridAndDr = 'Click on grid and drag to subdivide elements';
  StrClickOnGridAndDrCell = 'Click on grid and drag to subdivide cells';

{$R *.dfm}

{ TfrmSubdivide }

procedure TfrmSubdivide.GetData(const ACol, ARow, ALayer: integer);
begin
  FGettingData := True;
  try
    if ACol < 0 then
    begin
      seCol1.Enabled := False;
      seCol2.Enabled := False;
      seColCount.Enabled := False;
      seCol1.MinValue := 0;
      seCol2.MinValue := 0;
      seCol1.Value := 0;
      seCol2.Value := 0;
    end
    else
    begin
      seCol1.MaxValue := frmGoPhast.Grid.ColumnCount;
      seCol2.MaxValue := seCol1.MaxValue;
      seCol1.Value := SubdivideGridTool.FirstSubdivideColumn + 1;
      seCol2.Value := ACol + 1;
    end;

    if ARow < 0 then
    begin
      seRow1.Enabled := False;
      seRow2.Enabled := False;
      seRowCount.Enabled := False;
      seRow1.MinValue := 0;
      seRow2.MinValue := 0;
      seRow1.Value := 0;
      seRow2.Value := 0;
    end
    else
    begin
      seRow1.MaxValue := frmGoPhast.Grid.RowCount;
      seRow2.MaxValue := seRow1.MaxValue;
      seRow1.Value := SubdivideGridTool.FirstSubdivideRow + 1;
      seRow2.Value := ARow + 1;
    end;

    if ALayer < 0 then
    begin
      seLayer1.Enabled := False;
      seLayer2.Enabled := False;
      seLayerCount.Enabled := False;
      seLayer1.MinValue := 0;
      seLayer2.MinValue := 0;
      seLayer1.Value := 0;
      seLayer2.Value := 0;
    end
    else
    begin
      seLayer1.MaxValue := frmGoPhast.Grid.LayerCount;
      seLayer2.MaxValue := seLayer1.MaxValue;
      seLayer1.Value := SubdivideGridTool.FirstSubdivideLayer + 1;
      seLayer2.Value := ALayer + 1;
    end;

    if frmGoPhast.ModelSelection <> msPhast then
    begin
      seLayer1.Enabled := False;
      seLayer2.Enabled := False;
      seLayerCount.Enabled := False;
    end;

    if frmGoPhast.ModelSelection = msFootPrint then
    begin
      seCol1.MaxValue := 1;
      seRow1.MaxValue := 1;
      seCol2.MinValue := seCol2.MaxValue;
      seRow2.MinValue := seRow2.MaxValue;
    end;
  finally
    FGettingData := False;
  end;

//  SetSpinColor;
end;

procedure TfrmSubdivide.SetData;
var
  UndoSubdivide: TUndoSubdivide;
  TempValue: integer;
begin
  UndoSubdivide := TUndoSubdivide.Create;

  UndoSubdivide.FirstColumn := seCol1.AsInteger - 1;
  UndoSubdivide.LastColumn := seCol2.AsInteger - 1;
  if UndoSubdivide.LastColumn < UndoSubdivide.FirstColumn then
  begin
    TempValue := UndoSubdivide.FirstColumn;
    UndoSubdivide.FirstColumn := UndoSubdivide.LastColumn;
    UndoSubdivide.LastColumn := TempValue;
  end;
  if UndoSubdivide.FirstColumn >= 0 then
  begin
    UndoSubdivide.ColumnCount := seColCount.AsInteger;
  end
  else
  begin
    UndoSubdivide.ColumnCount := 0;
  end;

  UndoSubdivide.FirstRow := seRow1.AsInteger - 1;
  UndoSubdivide.LastRow := seRow2.AsInteger - 1;
  if UndoSubdivide.LastRow < UndoSubdivide.FirstRow then
  begin
    TempValue := UndoSubdivide.FirstRow;
    UndoSubdivide.FirstRow := UndoSubdivide.LastRow;
    UndoSubdivide.LastRow := TempValue;
  end;
  if UndoSubdivide.FirstRow >= 0 then
  begin
    UndoSubdivide.RowCount := seRowCount.AsInteger;
  end
  else
  begin
    UndoSubdivide.RowCount := 0;
  end;

  UndoSubdivide.FirstLayer := seLayer1.AsInteger - 1;
  UndoSubdivide.LastLayer := seLayer2.AsInteger - 1;
  if UndoSubdivide.LastLayer < UndoSubdivide.FirstLayer then
  begin
    TempValue := UndoSubdivide.FirstLayer;
    UndoSubdivide.FirstLayer := UndoSubdivide.LastLayer;
    UndoSubdivide.LastLayer := TempValue;
  end;
  if UndoSubdivide.FirstLayer >= 0 then
  begin
    UndoSubdivide.LayerCount := seLayerCount.AsInteger;
  end
  else
  begin
    UndoSubdivide.LayerCount := 0;
  end;

  if (UndoSubdivide.LayerCount <= 1) and (UndoSubdivide.RowCount <= 1)
    and (UndoSubdivide.ColumnCount <= 1) then
  begin
    UndoSubdivide.Free;
  end
  else
  begin
    frmGoPhast.UndoStack.Submit(UndoSubdivide);
  end;
end;

procedure TfrmSubdivide.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSubdivide.FormDestroy(Sender: TObject);
begin
  inherited;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TfrmSubdivide.FormShow(Sender: TObject);
begin
  inherited;
  if seCol1.Enabled then
  begin
    seCol1.SetFocus;
  end
  else if seRow1.Enabled then
  begin
    seRow1.SetFocus;
  end
  else if seLayer1.Enabled then
  begin
    seLayer1.SetFocus;
  end

end;

procedure TfrmSubdivide.seCol1Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FFirstSubdivideColumn := seCol1.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSubdivide.seCol2Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FLastSubdivideColumn := seCol2.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSubdivide.seColCountChange(Sender: TObject);
begin
  inherited;
  if frmGoPhast.ModelSelection = msFootPrint then
  begin
    if seRowCount.AsInteger <> seColCount.AsInteger then
    begin
      seRowCount.AsInteger := seColCount.AsInteger;
    end;
  end;
end;

procedure TfrmSubdivide.seLayer2Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FLastSubdivideLayer := seLayer2.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSubdivide.seRow1Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FFirstSubdivideRow := seRow1.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSubdivide.seRow2Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FLastSubdivideRow := seRow2.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmSubdivide.seRowCountChange(Sender: TObject);
begin
  inherited;
  if frmGoPhast.ModelSelection = msFootPrint then
  begin
    if seRowCount.AsInteger <> seColCount.AsInteger then
    begin
      seColCount.AsInteger := seRowCount.AsInteger;
    end;
  end;
end;

procedure TfrmSubdivide.seLayer1Changed(Sender: TObject);
begin
  inherited;
  if not FGettingData and not (csCreating in ControlState) then
  begin
    SubdivideGridTool.FFirstSubdivideLayer := seLayer1.AsInteger - 1;
    SubdivideGridTool.Layer32.Changed;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

{ TSubdivideGridTool }
procedure TSubdivideGridTool.BeginSubdivide(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to determine and store
  // the first column and row that will be subdivided.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FFirstSubdivideColumn := Column;
          FFirstSubdivideRow := Row;
          FFirstSubdivideLayer := -1;
          FSubdividing := True;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FFirstSubdivideColumn := Column;
          FFirstSubdivideRow := -1;
          FFirstSubdivideLayer := Layer;
          FSubdividing := True;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FFirstSubdivideColumn := -1;
          FFirstSubdivideRow := Row;
          FFirstSubdivideLayer := Layer;
          FSubdividing := True;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TSubdivideGridTool.ContinueSubdivide(X, Y: integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to update the view of the grid when
  // subdividing rows, columns or layers.

  // Get the row, column, or layer number of the cell underneath
  // the cursor.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  case ViewDirection of
    vdTop:
      begin
        // Get row and column numbers.
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FLastSubdivideColumn := Column;
          FLastSubdivideRow := Row;
          ZoomBox.InvalidateImage32;
        end;
      end;
    vdFront:
      begin
        // Get layer and column numbers.
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FLastSubdivideLayer := Layer;
          FLastSubdivideColumn := Column;
          ZoomBox.InvalidateImage32;
        end;
      end;
    vdSide:
      begin
        // Get layer and row numbers.
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FLastSubdivideLayer := Layer;
          FLastSubdivideRow := Row;
          ZoomBox.InvalidateImage32;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TSubdivideGridTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  if FButton <> mbMiddle then
  begin
    if (frmGoPhast.CurrentTool = self)
      and (Sender = Layer32)
      and (frmGoPhast.CursorGrid = View.CursorGrid) then
    begin
      DrawSubdividing(View.ViewDirection, Buffer);
    end;
  end;
end;

function TSubdivideGridTool.GetHint: string;
begin
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined: Assert(False);
    msPhast:
      begin
        result := StrClickOnGridAndDr;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        result := StrClickOnGridAndDrCell;
      end;
    else Assert(False);
  end;
  result := frmGoPhast.acSubdivide.Hint;
end;

procedure TSubdivideGridTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  BeginSubdivide(X, Y);
end;

procedure TSubdivideGridTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Subdividing then
  begin
    ContinueSubdivide(X, Y);
  end;
end;

procedure TSubdivideGridTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Subdividing then
    begin
      case ViewDirection of
        vdTop: SubdivideTop(X, Y);
        vdFront: SubdivideFront(X, Y);
        vdSide: SubdivideSide(X, Y);
      else
        Assert(False);
      end;
    end;
  end;
  inherited;
end;

procedure TSubdivideGridTool.DrawSubdividing(const Direction: TViewDirection;
  const BitMap: TBitmap32);
begin
  if Subdividing then
  begin
    if BitMap <> nil then
    begin
      Layer32.BringToFront;
    end;

    case Direction of
      vdTop:
        begin
          DrawSelectedTopCells(SubdivideGridTool.FirstSubdivideColumn,
            SubdivideGridTool.LastSubdivideColumn,
            SubdivideGridTool.FirstSubdivideRow,
            SubdivideGridTool.LastSubdivideRow,
            BitMap, Direction);
        end;
      vdFront:
        begin
          DrawSelectedFrontCells(SubdivideGridTool.FirstSubdivideColumn,
            SubdivideGridTool.LastSubdivideColumn,
            SubdivideGridTool.FirstSubdivideLayer,
            SubdivideGridTool.LastSubdivideLayer,
            BitMap, Direction);
        end;
      vdSide:
        begin
          DrawSelectedSideCells(SubdivideGridTool.FirstSubdivideRow,
            SubdivideGridTool.LastSubdivideRow,
            SubdivideGridTool.FirstSubdivideLayer,
            SubdivideGridTool.LastSubdivideLayer,
            BitMap, Direction);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TSubdivideGridTool.Subdivide(const LastCol, LastRow,
  LastLayer: integer);
var
  frmSubdivide: TfrmSubdivide;
begin
  // Subdivide rows columns or layers.
  Application.CreateForm(TfrmSubdivide, frmSubdivide);
  try
    frmSubdivide.GetData(LastCol, LastRow, LastLayer);
    frmSubdivide.ShowModal;
  finally
    frmSubdivide.Free;
  end;
end;

procedure TSubdivideGridTool.SubdivideFront(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Layer: integer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetColLayer(APoint, Column, Layer);
  if (Column >= 0) and (Layer >= 0) then
  begin
    Subdivide(Column, -1, Layer);
  end
  else
  begin
    Subdivide(LastSubdivideColumn, -1, LastSubdivideLayer)
  end;
end;

procedure TSubdivideGridTool.SubdivideSide(X, Y: Integer);
var
  APoint: TPoint2D;
  Row, Layer: integer;
begin
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  GetRowLayer(APoint, Row, Layer);
  if (Row >= 0) and (Layer >= 0) then
  begin
    Subdivide(-1, Row, Layer);
  end
  else
  begin
    Subdivide(-1, LastSubdivideRow, LastSubdivideLayer)
  end;
end;

procedure TSubdivideGridTool.SubdivideTop(X, Y: Integer);
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
    // subdivide the column and row
    Subdivide(Column, Row, -1);
  end
  else
  begin
    Subdivide(LastSubdivideColumn, LastSubdivideRow, -1);
  end;
end;

procedure TfrmSubdivide.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  SubdivideGridTool.FSubdividing := False;
  SubdivideGridTool.Layer32.Changed;
  SubdivideGridTool.View.ZoomBox.InvalidateImage32;

end;

procedure TfrmSubdivide.FormCreate(Sender: TObject);
begin
  inherited;
  FGettingData := True;
  try
    seColCount.MaxValue := MAXINT;
    seRowCount.MaxValue := MAXINT;
    seLayerCount.MaxValue := MAXINT;
    seCol1.MaxValue := MAXINT;
    seCol2.MaxValue := MAXINT;
    seRow1.MaxValue := MAXINT;
    seRow2.MaxValue := MAXINT;
    seLayer1.MaxValue := MAXINT;
    seLayer2.MaxValue := MAXINT;
  finally
    FGettingData := False;
  end;
end;

initialization
  SubdivideGridTool := TSubdivideGridTool.Create(nil);

finalization
  SubdivideGridTool.Free;

end.
