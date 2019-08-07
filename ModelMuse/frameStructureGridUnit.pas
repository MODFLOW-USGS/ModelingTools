unit frameStructureGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, Buttons, ExtCtrls,
  JvExStdCtrls, JvCombobox, JvListComb, ArgusDataEntry;

type
{
   mcReach: ISMODRCH,   ISSTRRCH
   mcStructureType: ISTRTYPE
   mcConnectedReach: ISTRCONN
*   mcRestrictions: ISTRDIR
   mcWeirDischarge: STRCD
   mcOrificeDischarge: STRCD2
*   mcSubmergenceExponent: STRCD3
   mcInvertElevation: STRINV
   mcStructureWidth: STRWID
*   mcInitialValueMethod: STRVAL
*   mcInitialValue: STRVAL
*   mcInitialValueTabFile: STRVAL
   mcSfrSegment: ISFRSEG
   mcSfrReach: ISFRRCH
}

  TMainColumns = (mcName, mcReach, mcStructureType, mcConnectedReach,
    mcRestrictions, mcWeirDischarge, mcOrificeDischarge,
    mcSubmergenceExponent, mcInvertElevation, mcStructureWidth,
    mcInitialValueMethod, mcInitialValue, mcInitialValueTabFile,
    mcSfrSegment, mcSfrReach);

  TMainColumnSet = set of TMainColumns;

  TframeStructureGrid = class(TframeGrid)
    comboType: TJvImageComboBox;
    lblType: TLabel;
    lblRestrictions: TLabel;
    comboRestrictions: TJvImageComboBox;
    lblInitialValueMethod: TLabel;
    comboInitialValueMethod: TJvImageComboBox;
    lblInitialValueTabFile: TLabel;
    comboInitialValueTabFile: TJvImageComboBox;
    rdeValue: TRbwDataEntry;
    procedure FrameResize(Sender: TObject);
    procedure GridColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure GridHorizontalScroll(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboTypeChange(Sender: TObject);
    procedure comboRestrictionsChange(Sender: TObject);
    procedure comboInitialValueMethodChange(Sender: TObject);
    procedure comboInitialValueTabFileChange(Sender: TObject);
    procedure rdeValueChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LayoutMultiRowEditControls;
    procedure SetSelectedValues(const AValue: string; Columns: TMainColumnSet);
    { Public declarations }
  end;

var
  frameStructureGrid: TframeStructureGrid;

implementation

uses
  frmCustomGoPhastUnit, Math;

{$R *.dfm}

{ TframeStructureGrid }

procedure TframeStructureGrid.comboInitialValueMethodChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboInitialValueMethod.Text, [mcInitialValueMethod]);
end;

procedure TframeStructureGrid.comboInitialValueTabFileChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboInitialValueTabFile.Text, [mcInitialValueTabFile]);
end;

procedure TframeStructureGrid.comboRestrictionsChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboRestrictions.Text, [mcRestrictions]);
end;

procedure TframeStructureGrid.comboTypeChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(comboType.Text, [mcStructureType]);
end;

procedure TframeStructureGrid.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeStructureGrid.GridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeStructureGrid.GridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeStructureGrid.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeStructureGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColItem: TMainColumns;
  ColIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColItem in [mcWeirDischarge, mcOrificeDischarge, mcSubmergenceExponent,
      mcInvertElevation, mcStructureWidth, mcInitialValue] do
    begin
      ColIndex := Ord(ColItem);
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

  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(mcStructureType),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboType.Enabled := ShouldEnable;
  lblType.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(mcRestrictions),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboRestrictions.Enabled := ShouldEnable;
  lblRestrictions.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(mcInitialValueMethod),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboInitialValueMethod.Enabled := ShouldEnable;
  lblInitialValueMethod.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Ord(mcInitialValueTabFile),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboInitialValueTabFile.Enabled := ShouldEnable;
  lblInitialValueTabFile.Enabled := ShouldEnable;
end;

procedure TframeStructureGrid.LayoutMultiRowEditControls;
var
  ACol: integer;
begin
  LayoutControls(Grid, comboType, lblType, Ord(mcStructureType));
  LayoutControls(Grid, comboRestrictions, lblRestrictions, Ord(mcRestrictions));
  LayoutControls(Grid, comboInitialValueMethod, lblInitialValueMethod,
    Ord(mcInitialValueMethod));
  LayoutControls(Grid, comboInitialValueTabFile, lblInitialValueTabFile,
    Ord(mcInitialValueTabFile));
  ACol := Max(Grid.LeftCol, Ord(mcWeirDischarge));
  if (ACol = Ord(mcInitialValueMethod)) or (ACol > Ord(mcInitialValue)) then
  begin
    ACol := Ord(mcInitialValue);
  end;
  LayoutControls(Grid, rdeValue, nil, ACol);
end;

procedure TframeStructureGrid.rdeValueChange(Sender: TObject);
begin
  inherited;
  SetSelectedValues(rdeValue.Text, [mcWeirDischarge, mcOrificeDischarge,
    mcSubmergenceExponent, mcInvertElevation, mcStructureWidth, mcInitialValue]);
end;

procedure TframeStructureGrid.SetSelectedValues(const AValue: string;
  Columns: TMainColumnSet);
var
  AColumnItem: TMainColumns;
  RowIndex: Integer;
  ColIndex: Integer;
  CanSelect: Boolean;
begin
  Grid.BeginUpdate;
  try
    for AColumnItem in Columns do
    begin
      ColIndex := Ord(AColumnItem);
      for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
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
            Grid.Cells[ColIndex, RowIndex] := AValue;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(Grid, ColIndex, RowIndex, AValue);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

end.
