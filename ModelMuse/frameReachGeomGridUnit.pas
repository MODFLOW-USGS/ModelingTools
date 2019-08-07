unit frameReachGeomGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, StdCtrls,
  ArgusDataEntry, JvExStdCtrls, JvCombobox, JvListComb, Grids, RbwDataGrid4,
  Mask, JvExMask, JvSpin, Buttons, ExtCtrls;

type
  TGeometryColumns = (gcNumber, gcName, gcType, gcMethod, gcRoughness, gcWidth,
    gcBottomElevation, gcSideSlope, gcConductance, gcLeakance, gcCenterDistance,
    gcExtinctionDepth);

  TframeReachGeomGrid = class(TframeGrid)
    comboType: TJvImageComboBox;
    comboMethod: TJvImageComboBox;
    rdeValue: TRbwDataEntry;
    procedure FrameResize(Sender: TObject);
    procedure GridColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure GridHorizontalScroll(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeValueChange(Sender: TObject);
    procedure comboTypeChange(Sender: TObject);
    procedure comboMethodChange(Sender: TObject);
  private
    procedure LayoutMultiRowEditControls;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameReachGeomGrid: TframeReachGeomGrid;

implementation

uses
  frmCustomGoPhastUnit, Math;

{$R *.dfm}

{ TframeReachGeomGrid }

procedure TframeReachGeomGrid.comboMethodChange(Sender: TObject);
var
  RowIndex: Integer;
  TempOptions: TGridOptions;
begin
  inherited;
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(Ord(gcMethod), RowIndex) then
      begin
        Grid.Cells[Ord(gcMethod), RowIndex] := comboMethod.Text;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(
            Grid,Ord(gcMethod),RowIndex, comboMethod.Text);
        end;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;

end;

procedure TframeReachGeomGrid.comboTypeChange(Sender: TObject);
var
  RowIndex: Integer;
  TempOptions: TGridOptions;
begin
  inherited;
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(Ord(gcType), RowIndex) then
      begin
        Grid.Cells[Ord(gcType), RowIndex] := comboType.Text;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(
            Grid,Ord(gcType),RowIndex, comboType.Text);
        end;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;

end;

procedure TframeReachGeomGrid.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls
end;

procedure TframeReachGeomGrid.GridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeReachGeomGrid.GridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls
end;

procedure TframeReachGeomGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(gcType),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboType.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(gcMethod),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;

  comboMethod.Enabled := ShouldEnable;
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := Ord(gcRoughness) to Grid.ColCount - 1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeValue.Enabled := ShouldEnable;
end;

procedure TframeReachGeomGrid.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(Grid, comboType, nil, Ord(gcType));
  LayoutControls(Grid, comboMethod, nil, Ord(gcMethod));
  LayoutControls(Grid, rdeValue, nil, Max(Ord(gcRoughness), Grid.LeftCol));

end;

procedure TframeReachGeomGrid.rdeValueChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
  TempOptions: TGridOptions;
  CanSelect: Boolean;
begin
  inherited;
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Ord(gcRoughness) to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          CanSelect := True;
          if Assigned(Grid.OnSelectCell) then
          begin
            Grid.OnSelectCell(Grid, ColIndex, RowIndex, CanSelect);
          end;
          if CanSelect then
          begin
            Grid.Cells[ColIndex, RowIndex] := rdeValue.Text;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(
                Grid,ColIndex,RowIndex, rdeValue.Text);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;

end;

end.
