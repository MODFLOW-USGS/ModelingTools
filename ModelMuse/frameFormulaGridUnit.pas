unit frameFormulaGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, Buttons, ExtCtrls;

type
  TValidCellEvent = procedure (Sender: TObject; ACol, ARow: Integer; var ValidCell: Boolean) of object;

  TframeFormulaGrid = class(TframeGrid)
    pnlTop: TPanel;
    edFormula: TLabeledEdit;
    cbMultiCheck: TCheckBox;
    procedure edFormulaChange(Sender: TObject);
    procedure GridColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure GridHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbMultiCheckClick(Sender: TObject);
  private
    FFirstFormulaColumn: Integer;
    FOnValidCell: TValidCellEvent;
    FFirstCheckColumn: Integer;
    FOnValidCheckCell: TValidCellEvent;
    { Private declarations }
  public
    procedure LayoutMultiRowEditControls; virtual;
    property FirstFormulaColumn: Integer read FFirstFormulaColumn
      write FFirstFormulaColumn;
    property FirstCheckColumn: Integer read FFirstCheckColumn
      write FFirstCheckColumn;
    property OnValidCell: TValidCellEvent read FOnValidCell write FOnValidCell;
    property OnValidCheckCell: TValidCellEvent read FOnValidCheckCell
      write FOnValidCheckCell;
    { Public declarations }
  end;

var
  frameFormulaGrid: TframeFormulaGrid;

implementation

uses
  frmCustomGoPhastUnit, Math;

{$R *.dfm}

procedure TframeFormulaGrid.cbMultiCheckClick(Sender: TObject);
var
  ColIndex: integer;
  RowIndex: Integer;
//  TempOptions: TGridOptions;
  ValidCell: Boolean;
begin
  inherited;
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to
      Grid.RowCount - 1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          ValidCell := True;
          if Assigned (OnValidCell) then
          begin
            OnValidCell(Grid, ColIndex, RowIndex, ValidCell);
          end;
          if ValidCell then
          begin
            Grid.Checked[ColIndex, RowIndex] := cbMultiCheck.Checked;
            if Assigned(Grid.OnStateChange) then
            begin
              Grid.OnStateChange(
                Grid,ColIndex,RowIndex, cbMultiCheck.State);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate
  end
end;

procedure TframeFormulaGrid.edFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
  ValidCell: Boolean;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to
      Grid.RowCount - 1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          ValidCell := True;
          if Assigned (OnValidCell) then
          begin
            OnValidCell(Grid, ColIndex, RowIndex, ValidCell);
          end;
          if ValidCell then
          begin
            Grid.Cells[ColIndex, RowIndex] := edFormula.Text;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(
                Grid,ColIndex,RowIndex, edFormula.Text);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
end;

procedure TframeFormulaGrid.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  inherited;
  if edFormula.Visible then
  begin
    ShouldEnable := False;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
        if ShouldEnable then
        begin
          if Assigned(OnValidCell) then
          begin
            OnValidCell(self, ColIndex, RowIndex, ShouldEnable);
            if ShouldEnable then
            begin
              Break;
            end;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ShouldEnable then
      begin
        break;
      end;
    end;
    edFormula.Enabled := ShouldEnable;
  end;
  if cbMultiCheck.Visible then
  begin
    ShouldEnable := False;
    for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
    begin
      for ColIndex := FirstCheckColumn to Grid.ColCount - 1 do
      begin
        ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
        if ShouldEnable then
        begin
          if Assigned(OnValidCheckCell) then
          begin
            OnValidCheckCell(self, ColIndex, RowIndex, ShouldEnable);
            if ShouldEnable then
            begin
              Break;
            end;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ShouldEnable then
      begin
        break;
      end;
    end;
    cbMultiCheck.Enabled := ShouldEnable;
  end
end;

procedure TframeFormulaGrid.LayoutMultiRowEditControls;
var
  Column: integer;
  Row: Integer;
  ColIndex: Integer;
  ValidCell: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  if edFormula.Visible then
  begin
    Column := Max(FirstFormulaColumn,Grid.LeftCol);
    if Assigned(OnValidCell) then
    begin
      Row := 1;
      for ColIndex := Column to Grid.ColCount - 1 do
      begin
        ValidCell := True;
        OnValidCell(self, ColIndex,Row,ValidCell);
        if ValidCell then
        begin
          Column := ColIndex;
          break;
        end;
      end;
    end;
    LayoutControls(Grid, edFormula, nil,
      Column);
  end;
  if cbMultiCheck.Visible then
  begin
    Column := Max(FirstCheckColumn,Grid.LeftCol);
    if Assigned(OnValidCheckCell) then
    begin
      Row := 1;
      for ColIndex := Column to Grid.ColCount - 1 do
      begin
        ValidCell := True;
        OnValidCheckCell(self, ColIndex,Row,ValidCell);
        if ValidCell then
        begin
          Column := ColIndex;
          break;
        end;
      end;
    end;
    LayoutControls(Grid, cbMultiCheck, nil,
      Column);
  end;
end;

end.
