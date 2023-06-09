unit frameGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask,
  JvSpin;

type
  TframeGrid = class(TFrame)
    Panel: TPanel;
    Grid: TRbwDataGrid4;
    seNumber: TJvSpinEdit;
    lbNumber: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    procedure GridEndUpdate(Sender: TObject);
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
  private
    FIncludePestAdjustment: Boolean;
    procedure SetIncludePestAdjustment(const Value: Boolean);
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    procedure ClearSelectedRow;
    procedure ClearGrid;
    property IncludePestAdjustment: Boolean read FIncludePestAdjustment
       write SetIncludePestAdjustment;
    { Public declarations }
  end;

implementation

uses
  Math,
  GoPhastTypes
  ;

{$R *.dfm}

procedure TframeGrid.GridEndUpdate(Sender: TObject);
begin
  if seNumber <> nil then
  begin
    if IncludePestAdjustment then
    begin
      seNumber.AsInteger := Grid.RowCount -1 - PestRowOffset;
    end
    else
    begin
      seNumber.AsInteger := Grid.RowCount -1;
    end;
  end;
end;

procedure TframeGrid.sbAddClick(Sender: TObject);
begin
  seNumber.AsInteger := seNumber.AsInteger +1;
end;

procedure TframeGrid.sbDeleteClick(Sender: TObject);
var
  FirstValidRow: integer;
begin
  if IncludePestAdjustment then
  begin
    FirstValidRow := Grid.FixedRows + PestRowOffset;
  end
  else
  begin
    FirstValidRow := Grid.FixedRows;
  end;
  if Grid.SelectedRow >= FirstValidRow  then
  begin
    if Grid.RowCount > FirstValidRow + 1 then
    begin
      ClearSelectedRow;
      Grid.DeleteRow(Grid.SelectedRow);
      GridEndUpdate(nil);
    end
    else
    begin
      ClearSelectedRow;
      seNumber.AsInteger := seNumber.AsInteger -1;
    end;
  end;
end;

procedure TframeGrid.sbInsertClick(Sender: TObject);
var
  FirstValidRow: Integer;
begin
  if IncludePestAdjustment then
  begin
    FirstValidRow := Grid.FixedRows + PestRowOffset;
  end
  else
  begin
    FirstValidRow := Grid.FixedRows;
  end;
  if Grid.SelectedRow >= FirstValidRow then
  begin
    Grid.InsertRow(Grid.SelectedRow);
    ClearSelectedRow;
    GridEndUpdate(nil);
  end;
end;

type
  TRbwDataGrid4Crack = class(TRbwDataGrid4);

procedure TframeGrid.SetEnabled(Value: boolean);
begin
  inherited;
  sbAdd.Enabled := Value;
  sbInsert.Enabled := Value;
  Grid.Enabled := Value;
  seNumber.Enabled := Value;
  sbDelete.Enabled := Value and (seNumber.AsInteger >= 1);
  if Grid.Enabled then
  begin
    Grid.Color := clWindow;
    Grid.ColorSelectedRow := True;
  end
  else
  begin
    Grid.Color := clBtnFace;
    TRbwDataGrid4Crack(Grid).HideEditor;
    Grid.ColorSelectedRow := False;
//    Grid.Invalidate;
  end;
end;

procedure TframeGrid.SetIncludePestAdjustment(const Value: Boolean);
begin
  FIncludePestAdjustment := Value;
end;

procedure TframeGrid.ClearSelectedRow;
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Cells[ColIndex, Grid.SelectedRow] := '';
    Grid.Checked[ColIndex, Grid.SelectedRow] := False;
    Grid.Objects[ColIndex, Grid.SelectedRow] := nil;
  end;
end;

procedure TframeGrid.seNumberChange(Sender: TObject);
var
  NewRowCount: integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewCount: Integer;
begin
  NewCount := seNumber.AsInteger;
  NewRowCount := Max(2, NewCount+1);
  if IncludePestAdjustment then
  begin
    NewRowCount := NewRowCount + PestRowOffset;
  end;
  if (NewRowCount < Grid.RowCount) then
  begin
    Grid.BeginUpdate;
    try
      for RowIndex := NewRowCount to Grid.RowCount - 1 do
      begin
        for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
        begin
          Grid.Cells[ColIndex, RowIndex] := '';
          Grid.Checked[ColIndex, RowIndex] := False;
          Grid.Objects[ColIndex, RowIndex] := nil;
        end;
      end;
    finally
      Grid.EndUpdate;
    end;
  end;
  Grid.RowCount := NewRowCount;
  seNumber.AsInteger := NewCount;
  sbDelete.Enabled := seNumber.AsInteger > 0;
end;

procedure TframeGrid.ClearGrid;
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
        Grid.Cells[ColIndex, RowIndex] := '';
        Grid.Checked[ColIndex, RowIndex] := False;
      end;
    end;
    if IncludePestAdjustment then
    begin
      Grid.Cells[0, PestModifierRow] := StrPestModifier;
      Grid.Cells[0, PestMethodRow] := StrModificationMethod;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

initialization
  RegisterClass(TframeGrid);

end.
