{@abstract(The main purpose of @name is to define @link(TfrmSmoothGrid)
  which is used to perform "grid-smoothing".)}
unit frmSmoothGridUnit;


interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons,
  AbstractGridUnit, GoPhastTypes, ArgusDataEntry;

type
  {@abstract(@name is used to perform "grid-smoothing".)

  When "grid-smoothing" is performed, the widths of columns, rows, or layers
  in @link(TPhastGrid) are changed to keep the ratio between the widths
  of adjacent rows, columns, or layers below the "grid-smothing
  criterion".  The "grid-smothing criterion" is generally less
  than or equal to 1.5.}
  TfrmSmoothGrid = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name is used to specify whether the columns should be "smoothed".
    // See @link(cbClick).
    cbColumns: TCheckBox;
    // @name: TCheckBox;
    // @name is used to specify whether the layers should be "smoothed".
    // See @link(cbClick).
    cbLayers: TCheckBox;
    // @name: TCheckBox;
    // @name is used to specify whether the rows should be "smoothed".
    // See @link(cbClick).
    cbRows: TCheckBox;
    // @name: TLabel;
    // @name displays "Grid smoothing criterion".
    lblCriterion: TLabel;
    // @name: TRbwDataEntry;
    // @name is used to set the grid smoothing criterion.
    // The grid smoothing criterion is the largest ratio between
    // adjacent column, row, or layer widths that is allowed.
    // The grid smoothing criterion should usually be less than 1.5.
    rdeCriterion: TRbwDataEntry;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name is the OnClick event-handler for @link(cbColumns),
    // @link(cbLayers), and @link(cbRows).
    // It enables @link(btnOK) if at least one of them is checked.
    procedure cbClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    // @name uses a @link(TUndoSmoothGrid) to smooth the grid.
    // @link(SmoothArray) is called to do the smoothing.
    procedure SetData;
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

// @name alters AnArray so that the widths of the spacing between
// adjacent items in AnArray is less than or equal to Criterion.
// @param(AnArray AnArray represents the column, row, or layer positions
//   in a @link(TPhastGrid).)
// @param(Criterion Criterion is the maximum allowed ratio between adjacent
//    column, row, or layer widths.)
procedure SmoothArray(var AnArray: TOneDRealArray; const Criterion: real);

implementation

uses frmGoPhastUnit, UndoItems;

{$R *.dfm}

procedure SmoothArray(var AnArray: TOneDRealArray;
  const Criterion: real);
const
  Epsilon = 1E-8;
  Max = 100;
var
  Index: integer;
  Width1, Width2: real;
  Changed: boolean;
  Factor1, Factor2: real;
  Count: integer;
  ArrayLength: integer;
  procedure AdjustArray;
  begin
    Width1 := AnArray[Index] - AnArray[Index - 1];
    Width2 := AnArray[Index + 1] - AnArray[Index];
    if (Width1 = 0) or (Width2 = 0) then
    begin
      AnArray[Index] := AnArray[Index - 1]
        + (Width1 + Width2) / 2;
      Changed := True;
    end
    else if Width1 / Width2 > Criterion then
    begin
      AnArray[Index] := AnArray[Index - 1]
        + Factor1 * (Width1 + Width2);
      Changed := True;
    end
    else if Width2 / Width1 > Criterion then
    begin
      AnArray[Index] := AnArray[Index - 1]
        + Factor2 * (Width1 + Width2);
      Changed := True;
    end;
  end;
begin
  Factor2 := 1 / (1 + Criterion * (1 - Epsilon)) * (1 + Epsilon);
  Factor1 := 1 - Factor2;
  ArrayLength := Length(AnArray);
  Count := 0;
  repeat
    Inc(Count);
    Changed := False;
    for index := 1 to ArrayLength - 2 do
    begin
      AdjustArray;
    end;
    for index := ArrayLength - 2 downto 1 do
    begin
      AdjustArray;
    end;
  until not Changed or (Count >= Max);
end;

procedure TfrmSmoothGrid.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSmoothGrid.SetData;
var
  Criterion: real;
  UndoSmoothGrid: TUndoSmoothGrid;
begin
  UndoSmoothGrid := TUndoSmoothGrid.Create;
  Criterion := StrToFloat(rdeCriterion.Text);
  UndoSmoothGrid.FNewColumns := frmGoPhast.Grid.ColumnPositions;
  SetLength(UndoSmoothGrid.FNewColumns, Length(UndoSmoothGrid.FNewColumns));
  if cbColumns.Checked then
  begin
    SmoothArray(UndoSmoothGrid.FNewColumns, Criterion);
  end;
  UndoSmoothGrid.FNewRows := frmGoPhast.Grid.RowPositions;
  SetLength(UndoSmoothGrid.FNewRows, Length(UndoSmoothGrid.FNewRows));
  if cbRows.Checked then
  begin
    SmoothArray(UndoSmoothGrid.FNewRows, Criterion);
  end;
  if frmGoPhast.ModelSelection = msPhast then
  begin
    UndoSmoothGrid.FNewLayerElevations := frmGoPhast.PhastGrid.LayerElevations;
    SetLength(UndoSmoothGrid.FNewLayerElevations,
      Length(UndoSmoothGrid.FNewLayerElevations));
    if cbLayers.Checked then
    begin
      SmoothArray(UndoSmoothGrid.FNewLayerElevations, Criterion);
    end;
  end;
  frmGoPhast.UndoStack.Submit(UndoSmoothGrid);
end;

procedure TfrmSmoothGrid.cbClick(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := cbColumns.Checked or cbRows.Checked or cbLayers.Checked;
end;

procedure TfrmSmoothGrid.GetData;
begin
  cbLayers.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;
end;

procedure TfrmSmoothGrid.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

end.


