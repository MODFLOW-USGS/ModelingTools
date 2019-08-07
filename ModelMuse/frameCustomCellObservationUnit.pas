unit frameCustomCellObservationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, Grids, RbwDataGrid4, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvCombobox, JvListComb, ArgusDataEntry, ComCtrls, JvExComCtrls,
  JvComCtrls;

type
  TframeCustomCellObservation = class(TFrame)
    pcData: TJvPageControl;
    tabTimes: TTabSheet;
    Panel5: TPanel;
    rdeMultiValueEdit: TRbwDataEntry;
    Panel2: TPanel;
    lblNumberOfTimes: TLabel;
    seTimes: TJvSpinEdit;
    btnDeleteValue: TButton;
    btnInsertValue: TButton;
    rdgObservations: TRbwDataGrid4;
    tabLayers: TTabSheet;
    Panel4: TPanel;
    lblNumberOfLayers: TLabel;
    seLayers: TJvSpinEdit;
    btnDeleteLayer: TButton;
    btnInsertLayer: TButton;
    Panel6: TPanel;
    rdeMultiLayerEdit: TRbwDataEntry;
    rdgLayers: TRbwDataGrid4;
    pnlCaption: TPanel;
    pnlName: TPanel;
    lblTreatment: TLabel;
    edObsName: TLabeledEdit;
    comboTreatment: TComboBox;
    procedure edObsNameChange(Sender: TObject);
    procedure edObsNameExit(Sender: TObject);
    procedure comboTreatmentChange(Sender: TObject); virtual;
    procedure seTimesChange(Sender: TObject); virtual;
    procedure btnDeleteValueClick(Sender: TObject);
    procedure btnInsertValueClick(Sender: TObject);
    procedure rdgObservationsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgObservationsExit(Sender: TObject);
    procedure rdgObservationsHorizontalScroll(Sender: TObject);
    procedure rdgObservationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean); virtual;
    procedure rdgObservationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdeMultiLayerEditChange(Sender: TObject);
    procedure seLayersChange(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure btnInsertLayerClick(Sender: TObject);
    procedure rdgLayersExit(Sender: TObject);
    procedure rdgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLayersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure FrameResize(Sender: TObject);
  private
    FDeletingTime: Boolean;
    FDeletingLayer: Boolean;
    procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      const StartCol, EndCol: integer);
    procedure DeleteSelectedRow(rdgGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit; DeleteButton: TButton);
    procedure UpdateSpinEdit(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure InsertRow(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit; DeleteButton: TButton);
    procedure EnableDeleteButton(DeleteButton: TButton; SpinEdit: TJvSpinEdit);
    procedure SetSpinCount(SpinEdit: TJvSpinEdit; Grid: TRbwDataGrid4);
    { Private declarations }
  protected
    FChanged: Boolean;
    FLayerCountChanged: Boolean;
    FTimesCountChanged: Boolean;
    procedure AssignValuesToSelectedGridCells(const NewText: string;
      Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
    procedure LayoutMultiCellEditControls; virtual; abstract;
  public
    { Public declarations }
    procedure InitializeControls; virtual;
  end;

  THeadLayers = (hlLayer, hlFraction);

resourcestring
  StrTime = 'Time';
  StrComment = 'Comment';

implementation

uses
  GoPhastTypes, Math;

resourcestring
  StrLayer = 'Layer';
  StrWeight = 'Weight';

{$R *.dfm}

{ TframeCustomCellObservation }

procedure TframeCustomCellObservation.AssignValuesToSelectedGridCells(
  const NewText: string; Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempText: string;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  FChanged := True;
  for ColIndex := StartCol to EndCol do
  begin
    if Grid.Columns[ColIndex].Format = rcf4Integer then
    begin
      TempText := IntToStr(Round(StrToFloat(NewText)));
    end
    else
    begin
      TempText := NewText;
    end;
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        Grid.Cells[ColIndex, RowIndex] := TempText;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(Grid, ColIndex, RowIndex, TempText);
        end;
      end;
    end;
  end;
  if Assigned(Grid.OnExit) then
  begin
    Grid.OnExit(Grid);
  end;
end;

procedure TframeCustomCellObservation.btnDeleteLayerClick(Sender: TObject);
begin
  DeleteSelectedRow(rdgLayers, seLayers, btnDeleteLayer);
end;

procedure TframeCustomCellObservation.btnDeleteValueClick(Sender: TObject);
begin
  DeleteSelectedRow(rdgObservations, seTimes, btnDeleteValue);
end;

procedure TframeCustomCellObservation.btnInsertLayerClick(Sender: TObject);
begin
  InsertRow(rdgLayers, seLayers, btnDeleteLayer);
end;

procedure TframeCustomCellObservation.btnInsertValueClick(Sender: TObject);
begin
  InsertRow(rdgObservations, seTimes, btnDeleteValue);
end;

type TRbwDataGrid4Crack = class(TRbwDataGrid4);

procedure TframeCustomCellObservation.comboTreatmentChange(Sender: TObject);
begin
  FChanged := True;
  TRbwDataGrid4Crack(rdgObservations).HideEditor;
end;

procedure TframeCustomCellObservation.DeleteSelectedRow(rdgGrid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; DeleteButton: TButton);
var
  ColIndex: Integer;
begin
  FChanged := True;
  if (rdgGrid.SelectedRow > 0) and (rdgGrid.SelectedRow < rdgGrid.RowCount)
    and (rdgGrid.RowCount > 2) then
  begin
    rdgGrid.DeleteRow(rdgGrid.SelectedRow);
    UpdateSpinEdit(rdgGrid, SpinEdit);
  end
  else if (rdgGrid.SelectedRow = 1) and (rdgGrid.RowCount = 2) then
  begin
    for ColIndex := 0 to rdgGrid.ColCount - 1 do
    begin
      rdgGrid.Cells[ColIndex,1] := '';
    end;
    SpinEdit.AsInteger := 0;
  end;
  EnableDeleteButton(DeleteButton, SpinEdit);
end;

procedure TframeCustomCellObservation.edObsNameChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TframeCustomCellObservation.edObsNameExit(Sender: TObject);
begin
  edObsName.Text := StringReplace(edObsName.Text, ' ', '_', [rfReplaceAll]);
end;

procedure TframeCustomCellObservation.EnableDeleteButton(DeleteButton: TButton;
  SpinEdit: TJvSpinEdit);
begin
  DeleteButton.Enabled := (SpinEdit.AsInteger > 0);
end;

procedure TframeCustomCellObservation.EnableMultiEditControl(
  Grid: TRbwDataGrid4; AControl: TControl; const StartCol, EndCol: integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
  EnableCount: Integer;
begin
  EnableCount := 0;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := StartCol to EndCol do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        Inc(EnableCount);
        if EnableCount >= 2 then
        begin
          break;
        end;
      end;
    end;
  end;
  ShouldEnable := EnableCount >= 2;
  AControl.Enabled := ShouldEnable;
end;

procedure TframeCustomCellObservation.FrameResize(Sender: TObject);
begin
  LayoutMultiCellEditControls;
end;

procedure TframeCustomCellObservation.InitializeControls;
begin
  pcData.ActivePageIndex := 0;

  rdgLayers.Cells[Ord(hlLayer),0] := StrLayer;
  rdgLayers.Cells[Ord(hlFraction),0] := StrWeight;

  comboTreatment.ItemIndex := 0;
  lblTreatment.Top := comboTreatment.Top - lblTreatment.Height - 2;

  LayoutMultiCellEditControls;

end;

procedure TframeCustomCellObservation.InsertRow(Grid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; DeleteButton: TButton);
begin
  FChanged := True;
  if SpinEdit.AsInteger = 0 then
  begin
    SpinEdit.AsInteger := 1;
  end
  else
  begin
    if (Grid.SelectedRow > 0) and (Grid.SelectedRow < Grid.RowCount) then
    begin
      Grid.InsertRow(Grid.SelectedRow);
      UpdateSpinEdit(Grid, SpinEdit);
      EnableDeleteButton(DeleteButton, SpinEdit);
    end;
  end;
end;

procedure TframeCustomCellObservation.rdeMultiLayerEditChange(Sender: TObject);
begin
  AssignValuesToSelectedGridCells(rdeMultiLayerEdit.Text, rdgLayers,
    Ord(hlLayer), Ord(hlFraction));
end;

procedure TframeCustomCellObservation.rdgObservationsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiCellEditControls;
end;

procedure TframeCustomCellObservation.rdgObservationsExit(Sender: TObject);
begin
  SetSpinCount(seTimes, rdgObservations);
end;

procedure TframeCustomCellObservation.rdgObservationsHorizontalScroll(Sender: TObject);
begin
  LayoutMultiCellEditControls;
end;

procedure TframeCustomCellObservation.rdgObservationsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seTimes.AsInteger > 0;
end;

procedure TframeCustomCellObservation.rdgObservationsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if FDeletingTime then
  begin
    Exit;
  end;
  UpdateSpinEdit(rdgObservations, seTimes);
end;

procedure TframeCustomCellObservation.rdgLayersExit(Sender: TObject);
begin
  SetSpinCount(seLayers, rdgLayers);
end;

procedure TframeCustomCellObservation.rdgLayersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seLayers.AsInteger > 0;
  EnableMultiEditControl(rdgLayers, rdeMultiLayerEdit,
    Ord(hlLayer), Ord(hlFraction));
end;

procedure TframeCustomCellObservation.rdgLayersSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if FDeletingLayer then
  begin
    Exit;
  end;
  UpdateSpinEdit(rdgLayers, seLayers);
end;

procedure TframeCustomCellObservation.seLayersChange(Sender: TObject);
begin
  FLayerCountChanged := True;
  FDeletingLayer := True;
  try
    if seLayers.AsInteger = 0 then
    begin
      rdgLayers.RowCount := 2;
      rdgLayers.Options := rdgLayers.Options - [goAlwaysShowEditor];
    end
    else
    begin
      rdgLayers.RowCount := seLayers.AsInteger + 1;
      rdgLayers.Options := rdgLayers.Options + [goAlwaysShowEditor];
    end;
    rdgLayers.Invalidate;
    EnableDeleteButton(btnDeleteLayer, seLayers);
  finally
    FDeletingLayer := False;
  end;
end;

procedure TframeCustomCellObservation.seTimesChange(Sender: TObject);
var
  CharNumber: integer;
begin
  FTimesCountChanged := True;
  FChanged := True;
  FDeletingTime := True;
  try
    if seTimes.AsInteger = 0 then
    begin
      rdgObservations.RowCount := 2;
      rdgObservations.Options := rdgObservations.Options - [goAlwaysShowEditor];
    end
    else
    begin
      rdgObservations.RowCount := seTimes.AsInteger + 1;
      rdgObservations.Options := rdgObservations.Options + [goAlwaysShowEditor];

      if seTimes.AsInteger = 1 then
      begin
        edObsName.MaxLength := 12;
      end
      else
      begin
        CharNumber := Trunc(Log10(seTimes.AsInteger))+1;
        edObsName.MaxLength := 12-CharNumber;
      end;

      if Length(edObsName.Text) > edObsName.MaxLength then
      begin
        edObsName.Text := Copy(edObsName.Text, 1, edObsName.MaxLength);
      end;
    end;
    EnableDeleteButton(btnDeleteValue, seTimes);
    rdgObservations.Invalidate;
  finally
    FDeletingTime := False;
  end;
end;

procedure TframeCustomCellObservation.SetSpinCount(SpinEdit: TJvSpinEdit;
  Grid: TRbwDataGrid4);
begin
  if Grid.RowCount = 2 then
  begin
    if (Grid.Cells[0, 1] = '')
      and (Grid.Cells[1, 1] = '')
      and (Grid.Cells[2, 1] = '')
      and (Grid.Cells[3, 1] = '') then
    begin
      SpinEdit.AsInteger := 0;
    end
    else
    begin
      SpinEdit.AsInteger := 1;
    end;
  end;
end;

procedure TframeCustomCellObservation.UpdateSpinEdit(Grid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit);
begin
  FChanged := True;
  if Grid.RowCount >= 2 then
  begin
    SpinEdit.AsInteger := Grid.RowCount - 1;
  end;
end;

end.
