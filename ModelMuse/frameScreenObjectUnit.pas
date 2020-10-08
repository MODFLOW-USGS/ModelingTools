unit frameScreenObjectUnit;

interface

uses
  Grids, RbwDataGrid4, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs;

type
  TframeScreenObject = class(TFrame)
  private { Private declarations }
    // See @link(FrameLoaded).
    FFrameLoaded: boolean;
    // See @link(FrameLoaded).
    procedure SetFrameLoaded(const Value: boolean);
  protected
    FLastTimeColumn: integer;
    // When the user sets the starting time for a boundary that matches the
    // starting time of a stress period, automatically
    // set the ending time to be the ending time to be the ending time of the
    // same stress period.
    procedure UpdateNextTimeCell(DataGrid: TRbwDataGrid4; ACol, ARow: Integer);
    procedure ClearGrid(Grid: TRbwDataGrid4); virtual;

  public
    // @name is used in @link(TframeScreenObjectParam.clbParametersStateChange
    // TframeScreenObjectParam.clbParametersStateChange) to prevent grayed
    // check boxes from being converted to normal ones incorrectly.
    // It is also used in @link(UpdateNextTimeCell) to prevent cells from
    // being set improperly when @link(TfrmScreenObjectProperties) is
    // reading data.
    property FrameLoaded: boolean read FFrameLoaded write SetFrameLoaded;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  frmCustomGoPhastUnit;

{$R *.dfm}

procedure TframeScreenObject.ClearGrid(Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
        Grid.Checked[ColIndex,RowIndex] := False;
        Grid.Objects[ColIndex,RowIndex] := nil;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

constructor TframeScreenObject.Create(AOwner: TComponent);
begin
  inherited;
  FLastTimeColumn := 1;
end;

procedure TframeScreenObject.SetFrameLoaded(const Value: boolean);
begin
  FFrameLoaded := Value;
end;

procedure TframeScreenObject.UpdateNextTimeCell(DataGrid: TRbwDataGrid4;
  ACol, ARow: Integer);
//var
//  SelectIndex: Integer;
begin
  if FrameLoaded and (FLastTimeColumn = 1) then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(DataGrid, ACol, ARow);
  end;
//  if FrameLoaded and (ARow >= DataGrid.FixedRows) and (ACol in [0, 1])
//    and (FLastTimeColumn = 1) then
//  begin
//    SelectIndex := DataGrid.ItemIndex[ACol, ARow];
//    if SelectIndex >= 0 then
//    begin
//      if (ACol = 0) then
//      begin
//        if DataGrid.Cells[1, ARow] = '' then
//        begin
//          DataGrid.ItemIndex[1, ARow] := SelectIndex;
//        end;
//      end
//      else if (ACol = 1) then
//      begin
//        if (ARow + 1 < DataGrid.RowCount) and
//          (DataGrid.Cells[0, ARow + 1] = '') then
//        begin
//          if SelectIndex + 1 < DataGrid.Columns[0].PickList.Count then
//          begin
//            DataGrid.ItemIndex[0, ARow + 1] := SelectIndex + 1;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

end.
