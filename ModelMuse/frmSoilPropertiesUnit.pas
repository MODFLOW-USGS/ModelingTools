unit frmSoilPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  frameFormulaGridUnit, StdCtrls, Buttons, ExtCtrls,
  ModflowFmpSoilUnit, UndoItems, Grids, RbwParser, RbwDataGrid4,
  GoPhastTypes;

type
  TUndoSoils = class(TCustomUndo)
  private
    FOldSoils: TSoilCollection;
    FNewSoils: TSoilCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var NewSoils: TSoilCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  TfrmSoilProperties = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    frameSoils: TframeFormulaGrid;
    comboSoilType: TComboBox;
    rbwprsrGlobal: TRbwParser;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameSoilsGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameSoilsGridHorizontalScroll(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure comboSoilTypeChange(Sender: TObject);
    procedure frameSoilsGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frameSoilsGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameSoilsseNumberChange(Sender: TObject);
    procedure frameSoilsGridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure frameSoilsGridRowMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
  private
    FSoils: TSoilCollection;
    procedure SetGridCaptions;
    procedure GetData;
    procedure SetData;
    procedure IsValidCell(Sender: TObject; ACol, ARow: Integer;
      var ValidCell: Boolean);
    procedure LayoutMultiRowEditControls;
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure GetGlobalVariables;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSoilProperties: TfrmSoilProperties;

implementation

uses
  frmGoPhastUnit, frmConvertChoiceUnit, frmFormulaUnit;

resourcestring
  StrErrorInFormulaS = 'Error in formula: %s';
  StrSoilID = 'Soil ID';
  StrSoilName = 'Soil Name';
  StrCapillaryFringeCa = 'Capillary Fringe (CapFringe)';
  StrSoilType = 'Soil-Type';
  StrACoeff = 'A-Coeff';
  StrBCoeff = 'B-Coeff';
  StrCCoeff = 'C-Coeff';
  StrDCoeff = 'D-Coeff';
  StrECoeff = 'E-Coeff';

{$R *.dfm}

type
  TSoilColumns = (scID, scName, scCapFringe, scSoiltype, scACoeff, scBCoeff,
    scCCoeff, scDCoeff, scECoeff);

{ TfrmSoilProperties }

procedure TfrmSoilProperties.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSoilProperties.comboSoilTypeChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
begin
  ColIndex := Ord(scSoiltype);
  for RowIndex := frameSoils.Grid.FixedRows to
    frameSoils.Grid.RowCount - 1 do
  begin
    if frameSoils.Grid.IsSelectedCell(ColIndex, RowIndex) then
    begin
        frameSoils.Grid.Cells[ColIndex, RowIndex] := comboSoilType.Text;
        if Assigned(frameSoils.Grid.OnSetEditText) then
        begin
          frameSoils.Grid.OnSetEditText(
            frameSoils.Grid,ColIndex,RowIndex, comboSoilType.Text);
        end;
    end;
  end;
  TempOptions := frameSoils.Grid.Options;
  try
    frameSoils.Grid.Options := [goEditing, goAlwaysShowEditor];
    frameSoils.Grid.UpdateEditor;
  finally
    frameSoils.Grid.Options := TempOptions;
  end;
end;

procedure TfrmSoilProperties.CreateBoundaryFormula(
  const DataGrid: TRbwDataGrid4; const ACol, ARow: integer; Formula: string;
  const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
begin
  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := rbwprsrGlobal;
  try
    TempCompiler.Compile(Formula);

  except on E: ERbwParserError do
    begin
      Beep;
      raise ERbwParserError.Create(Format(StrErrorInFormulaS,
        [E.Message]));
      Exit;
    end
  end;
  CompiledFormula := TempCompiler.CurrentExpression;

  ResultType := rdtDouble;

  if (ResultType = CompiledFormula.ResultType) or
    ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
      then
  begin
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end
  else
  begin
    Formula := AdjustFormula(Formula, CompiledFormula.ResultType, ResultType);
    TempCompiler.Compile(Formula);
    CompiledFormula := TempCompiler.CurrentExpression;
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end;
  if Assigned(DataGrid.OnSetEditText) then
  begin
    DataGrid.OnSetEditText(DataGrid, ACol, ARow, DataGrid.Cells[ACol, ARow]);
  end;
end;

procedure TfrmSoilProperties.FormCreate(Sender: TObject);
begin
  inherited;
  SetGridCaptions;
  comboSoilType.Items := frameSoils.Grid.Columns[Ord(scSoiltype)].PickList;
  frameSoils.FirstFormulaColumn := Ord(scCapFringe);
  LayoutMultiRowEditControls;
  frameSoils.OnValidCell := IsValidCell;
  GetData;
end;

procedure TfrmSoilProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  FSoils.Free;
end;

procedure TfrmSoilProperties.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmSoilProperties.frameSoilsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  Orientation: TDataSetOrientation;
  DataGrid: TRbwDataGrid4;
  EvaluatedAt: TEvaluatedAt;
  NewValue: string;
begin
  inherited;
  DataGrid := Sender as TRbwDataGrid4;
  // Lakes and reservoirs can only be specified from the top.
  Orientation := dsoTop;
  // All the MODFLOW boundary conditions are evaluated at blocks.
  EvaluatedAt := eaBlocks;

  NewValue := DataGrid.Cells[ACol, ARow];
  if (NewValue = '') then
  begin
    NewValue := '0';
  end;

//  with TfrmFormula.Create(self) do
  with frmFormula do
  begin
    try
      Initialize;
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.

      PopupParent := self;

      // Show the functions and global variables.
      IncludeTimeSeries := False;
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CreateBoundaryFormula(DataGrid, ACol, ARow, Formula, Orientation,
          EvaluatedAt);
      end;
    finally
      Initialize;
//      Free;
    end;
  end;
end;

procedure TfrmSoilProperties.frameSoilsGridHorizontalScroll(Sender: TObject);
begin
  inherited;
  frameSoils.GridHorizontalScroll(Sender);
  LayoutMultiRowEditControls;
end;

procedure TfrmSoilProperties.frameSoilsGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  inherited;
  frameSoils.GridMouseUp(Sender, Button, Shift, X, Y);
  inherited;
  ShouldEnable := False;
  ColIndex := Ord(scSoiltype);
  for RowIndex := frameSoils.Grid.FixedRows to frameSoils.Grid.RowCount -1 do
  begin
    ShouldEnable := frameSoils.Grid.IsSelectedCell(ColIndex,RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboSoilType.Enabled := ShouldEnable;
end;

procedure TfrmSoilProperties.frameSoilsGridRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
var
  index: Integer;
begin
  inherited;
  for index := 1 to frameSoils.Grid.RowCount - 1 do
  begin
    frameSoils.Grid.Cells[ord(scID), index] := IntToStr(index);

  end;
end;

procedure TfrmSoilProperties.frameSoilsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= 1) and (ACol >= Ord(scACoeff)) then
  begin
    CanSelect := frameSoils.Grid.ItemIndex[ord(scSoiltype), ARow] = Ord(stOther);
  end;
end;

procedure TfrmSoilProperties.frameSoilsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ACol = Ord(scName)) and (Value <> '') then
  begin
    frameSoils.seNumber.asInteger := frameSoils.Grid.RowCount-1;
  end;
  if (ACol = Ord(scSoiltype)) and (ARow >= frameSoils.Grid.FixedRows) then
  begin
    frameSoils.Grid.Invalidate;
  end;
end;

procedure TfrmSoilProperties.frameSoilsseNumberChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameSoils.seNumberChange(Sender);
  for RowIndex := 1 to frameSoils.Grid.RowCount - 1 do
  begin
    frameSoils.Grid.Cells[Ord(scID), RowIndex] := IntToStr(RowIndex);
  end;
end;

procedure TfrmSoilProperties.GetData;
var
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
  ASoil: TSoilItem;
begin
  GetGlobalVariables;
  FSoils := TSoilCollection.Create(nil);
  FSoils.Assign(frmGoPhast.PhastModel.FmpSoils);

  frameSoils.seNumber.AsInteger := FSoils.Count;
  frameSoils.seNumber.OnChange(frameSoils.seNumber);
  Grid := frameSoils.Grid;
  if FSoils.Count = 0 then
  begin
    Grid.Row := 1;
    frameSoils.ClearSelectedRow;
  end;
  Grid.BeginUpdate;
  try
    for ItemIndex := 0 to FSoils.Count - 1 do
    begin
      ASoil := FSoils[ItemIndex];
      Grid.Cells[ord(scID), ItemIndex+1] := IntToStr(ItemIndex+1);
      Grid.Cells[ord(scName), ItemIndex+1] := ASoil.SoilName;
      Grid.Cells[ord(scCapFringe), ItemIndex+1] := ASoil.CapillaryFringe;
      Grid.ItemIndex[ord(scSoiltype), ItemIndex+1] := Ord(ASoil.SoilType);
      Grid.Cells[ord(scACoeff), ItemIndex+1] := ASoil.ACoeff;
      Grid.Cells[ord(scBCoeff), ItemIndex+1] := ASoil.BCoeff;
      Grid.Cells[ord(scCCoeff), ItemIndex+1] := ASoil.CCoeff;
      Grid.Cells[ord(scDCoeff), ItemIndex+1] := ASoil.DCoeff;
      Grid.Cells[ord(scECoeff), ItemIndex+1] := ASoil.ECoeff;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmSoilProperties.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    CompilerList.Add(rbwprsrGlobal);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmSoilProperties.IsValidCell(Sender: TObject; ACol, ARow: Integer;
  var ValidCell: Boolean);
begin
  ValidCell := (ARow >= 1)
    and ((ACol = Ord(scCapFringe)) or (ACol >= Ord(scACoeff)));
end;

procedure TfrmSoilProperties.LayoutMultiRowEditControls;
begin
  frameSoils.LayoutMultiRowEditControls;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(frameSoils.Grid, comboSoilType, nil, Ord(scSoiltype));
end;

procedure TfrmSoilProperties.SetData;
var
  Count: Integer;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  ASoil: TSoilItem;
  Index: Integer;
begin
  Count := 0;
  Grid := frameSoils.Grid;
  for RowIndex := 1 to frameSoils.seNumber.AsInteger do
  begin
    if (Grid.Cells[ord(scName), RowIndex] <> '')
      and (Grid.ItemIndex[ord(scSoiltype), RowIndex] >= 0) then
    begin
      if Count >= FSoils.Count then
      begin
        FSoils.Add;
      end;
      ASoil := FSoils[Count];
      ASoil.SoilName := Grid.Cells[ord(scName), RowIndex];
      ASoil.CapillaryFringe := Grid.Cells[ord(scCapFringe), RowIndex];
      ASoil.SoilType := TSoilType(Grid.ItemIndex[ord(scSoiltype), RowIndex]);
      ASoil.ACoeff := Grid.Cells[ord(scACoeff), RowIndex];
      ASoil.BCoeff := Grid.Cells[ord(scBCoeff), RowIndex];
      ASoil.CCoeff := Grid.Cells[ord(scCCoeff), RowIndex];
      ASoil.DCoeff := Grid.Cells[ord(scDCoeff), RowIndex];
      ASoil.ECoeff := Grid.Cells[ord(scECoeff), RowIndex];

      Inc(Count);
    end;
  end;
  while Count < FSoils.Count do
  begin
    FSoils.Last.Free;
  end;

  // Ensure that the soils are sorted consistently.
  for Index := 0 to FSoils.Count - 1 do
  begin
    ASoil := FSoils[Index];
    ASoil.StartTime := Index;
    ASoil.EndTime := Index + 1;
  end;

  frmGoPhast.UndoStack.Submit(TUndoSoils.Create(FSoils));
end;

procedure TfrmSoilProperties.SetGridCaptions;
var
  ColIndex: Integer;
begin
  frameSoils.Grid.BeginUpdate;
  try
    for ColIndex := 0 to frameSoils.Grid.ColCount - 1 do
    begin
      frameSoils.Grid.Columns[ColIndex].AutoAdjustColWidths := true;
    end;
    frameSoils.Grid.Cells[Ord(scID), 0] := StrSoilID;
    frameSoils.Grid.Cells[Ord(scName), 0] := StrSoilName;
    frameSoils.Grid.Cells[Ord(scCapFringe), 0] := StrCapillaryFringeCa;
    frameSoils.Grid.Cells[Ord(scSoiltype), 0] := StrSoilType;
    frameSoils.Grid.Cells[Ord(scACoeff), 0] := StrACoeff;
    frameSoils.Grid.Cells[Ord(scBCoeff), 0] := StrBCoeff;
    frameSoils.Grid.Cells[Ord(scCCoeff), 0] := StrCCoeff;
    frameSoils.Grid.Cells[Ord(scDCoeff), 0] := StrDCoeff;
    frameSoils.Grid.Cells[Ord(scECoeff), 0] := StrECoeff;
  finally
    frameSoils.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameSoils.Grid.ColCount - 1 do
  begin
    if ColIndex in [Ord(scName)] then
    begin
      Continue;
    end;
    frameSoils.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

{ TUndoSoils }

constructor TUndoSoils.Create(var NewSoils: TSoilCollection);
begin
  FNewSoils := NewSoils;
  NewSoils := nil;
  FOldSoils := TSoilCollection.Create(nil);
  FOldSoils.Assign(frmGoPhast.PhastModel.FmpSoils);
end;

function TUndoSoils.Description: string;
begin
  result := 'change farm soils';
end;

destructor TUndoSoils.Destroy;
begin
  FNewSoils.Free;
  FOldSoils.Free;
  inherited;
end;

procedure TUndoSoils.DoCommand;
begin
  frmGoPhast.PhastModel.FmpSoils := FNewSoils;
end;

procedure TUndoSoils.Undo;
begin
  frmGoPhast.PhastModel.FmpSoils := FOldSoils;
end;

end.
