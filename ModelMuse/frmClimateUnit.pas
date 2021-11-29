unit frmClimateUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, RbwParser,
  StdCtrls, Buttons, ExtCtrls, frameGridUnit, frameFormulaGridUnit,
  ModflowFmpClimateUnit, GoPhastTypes, RbwDataGrid4, UndoItems;

type
  TUndoClimate = class(TCustomUndo)
  private
    FOldClimate: TClimateCollection;
    FNewClimate: TClimateCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var NewClimate: TClimateCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  TfrmClimate = class(TfrmCustomGoPhast)
    frameClimate: TframeFormulaGrid;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    rbwprsrGlobal: TRbwParser;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure frameClimateGridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure frameClimateGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FClimate: TClimateCollection;
    procedure InitializeGridCaptions;
    procedure GetStartingAndEndingTimes;
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure GetGlobalVariables;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmClimate: TfrmClimate;

implementation

uses
  frmGoPhastUnit, ModflowTimeUnit, frmConvertChoiceUnit, frmFormulaUnit;

resourcestring
  StrMaximumTemperature = 'Maximum temperature (MaxT)';
  StrMinimumTemperature = 'Minimum temperature (MinT)';
  StrPrecipitationFlux = 'Precipitation flux (Precip)';
  StrReferenceEvapotrans = 'Reference evapotranspiration flux (ETref)';
  StrErrorInFormulaS = 'Error in formula: %s';

type
  TClimateColumns = (ccStartingTime, ccEndingTime, ccMaxT, ccMinT, ccPrecip,
    ccETref);

{$R *.dfm}

{ TfrmClimate }

procedure TfrmClimate.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmClimate.CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
  const ACol, ARow: integer; Formula: string;
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

procedure TfrmClimate.FormCreate(Sender: TObject);
begin
  inherited;

  InitializeGridCaptions;
  frameClimate.FirstFormulaColumn := Ord(ccMaxT);
  frameClimate.LayoutMultiRowEditControls;
  GetStartingAndEndingTimes;
  GetGlobalVariables;
  GetData;
end;

procedure TfrmClimate.InitializeGridCaptions;
var
  ColIndex: Integer;
begin
  frameClimate.Grid.BeginUpdate;
  try
    for ColIndex := 0 to frameClimate.Grid.ColCount - 1 do
    begin
      frameClimate.Grid.Columns[ColIndex].AutoAdjustColWidths := true;
    end;
    frameClimate.Grid.Cells[Ord(ccStartingTime), 0] := StrStartingTime;
    frameClimate.Grid.Cells[Ord(ccEndingTime), 0] := StrEndingTime;
    frameClimate.Grid.Cells[Ord(ccMaxT), 0] := StrMaximumTemperature;
    frameClimate.Grid.Cells[Ord(ccMinT), 0] := StrMinimumTemperature;
    frameClimate.Grid.Cells[Ord(ccPrecip), 0] := StrPrecipitationFlux;
    frameClimate.Grid.Cells[Ord(ccETref), 0] := StrReferenceEvapotrans;
  finally
    frameClimate.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameClimate.Grid.ColCount - 1 do
  begin
    frameClimate.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

procedure TfrmClimate.SetData;
var
  Count: Integer;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  AClimate: TClimateItem;
  StartingTime: double;
  EndingTime: double;
begin
  Count := 0;
  Grid := frameClimate.Grid;
  for RowIndex := 1 to frameClimate.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(ccStartingTime), RowIndex], StartingTime)
      and TryStrToFloat(Grid.Cells[Ord(ccEndingTime), RowIndex], EndingTime) then
    begin
      if Count >= FClimate.Count then
      begin
        FClimate.Add;
      end;
      AClimate := FClimate[Count];
      AClimate.StartTime := StartingTime;
      AClimate.EndTime := EndingTime;
      AClimate.MaxT := Grid.Cells[ord(ccMaxT), RowIndex];
      AClimate.MinT := Grid.Cells[ord(ccMinT), RowIndex];
      AClimate.Precip := Grid.Cells[ord(ccPrecip), RowIndex];
      AClimate.ETref := Grid.Cells[ord(ccETref), RowIndex];

      Inc(Count);
    end;
  end;
  while Count < FClimate.Count do
  begin
    FClimate.Last.Free;
  end;
  frmGoPhast.UndoStack.Submit(TUndoClimate.Create(FClimate));
end;

procedure TfrmClimate.FormDestroy(Sender: TObject);
begin
  FClimate.Free;
  inherited;
end;

procedure TfrmClimate.frameClimateGridButtonClick(Sender: TObject; ACol,
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

procedure TfrmClimate.frameClimateGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
begin
  inherited;
  if (ACol = 0) and (ARow >= 1) then
  begin
    Grid := Sender as TRbwDataGrid4;
    if (Grid.Cells[ACol+1, ARow] = '') then
    begin
      ItemIndex := Grid.ItemIndex[ACol, ARow];
      if ItemIndex >= 0 then
      begin
        Grid.ItemIndex[ACol+1, ARow] := ItemIndex;
      end;
    end;
  end;
end;

procedure TfrmClimate.GetData;
var
  ItemIndex: Integer;
  AnItem: TClimateItem;
begin
  FClimate := TClimateCollection.Create(nil);
  FClimate.Assign(frmGoPhast.PhastModel.FmpClimate);

  frameClimate.seNumber.AsInteger := FClimate.Count;
  frameClimate.seNumber.OnChange(frameClimate.seNumber);
  if frameClimate.seNumber.AsInteger = 0 then
  begin
    frameClimate.Grid.Row := 1;
    frameClimate.ClearSelectedRow;
  end;
  frameClimate.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to FClimate.Count - 1 do
    begin
      AnItem := FClimate[ItemIndex];
      frameClimate.Grid.Cells[Ord(ccStartingTime), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameClimate.Grid.Cells[Ord(ccEndingTime), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameClimate.Grid.Cells[Ord(ccMaxT), ItemIndex+1] := AnItem.MaxT;
      frameClimate.Grid.Cells[Ord(ccMinT), ItemIndex+1] := AnItem.MinT;
      frameClimate.Grid.Cells[Ord(ccPrecip), ItemIndex+1] := AnItem.Precip;
      frameClimate.Grid.Cells[Ord(ccETref), ItemIndex+1] := AnItem.ETref;
    end;
  finally
    frameClimate.Grid.EndUpdate;
  end;
end;

procedure TfrmClimate.GetGlobalVariables;
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

procedure TfrmClimate.GetStartingAndEndingTimes;
//var
//  EndTimes: TStringList;
//  StartTimes: TStringList;
//  StressPeriods: TModflowStressPeriods;
//  TimeIndex: Integer;
//  AStressPeriod: TModflowStressPeriod;
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
    (frameClimate.Grid, Ord(ccStartingTime));
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
    (frameClimate.Grid, Ord(ccEndingTime));

//  StartTimes := TStringList.Create;
//  EndTimes := TStringList.Create;
//  try
//    StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
//    for TimeIndex := 0 to StressPeriods.Count - 1 do
//    begin
//      AStressPeriod := StressPeriods[TimeIndex];
//      StartTimes.Add(FloatToStr(AStressPeriod.StartTime));
//      EndTimes.Add(FloatToStr(AStressPeriod.EndTime));
//    end;
//    frameClimate.Grid.Columns[Ord(ccStartingTime)].PickList := StartTimes;
    frameClimate.Grid.Columns[Ord(ccStartingTime)].ComboUsed := True;
//    frameClimate.Grid.Columns[Ord(ccEndingTime)].PickList := EndTimes;
    frameClimate.Grid.Columns[Ord(ccEndingTime)].ComboUsed := True;
//  finally
//    EndTimes.Free;
//    StartTimes.Free;
//  end;
end;

{ TUndoClimate }

constructor TUndoClimate.Create(var NewClimate: TClimateCollection);
begin
  FOldClimate := TClimateCollection.Create(nil);
  FOldClimate.Assign(frmGoPhast.PhastModel.FmpClimate);
  FNewClimate := NewClimate;
  NewClimate := nil;
end;

function TUndoClimate.Description: string;
begin
  result := 'change climate';
end;

destructor TUndoClimate.Destroy;
begin
  FOldClimate.Free;
  FNewClimate.Free;
  inherited;
end;

procedure TUndoClimate.DoCommand;
begin
  frmGoPhast.PhastModel.FmpClimate := FNewClimate;
end;

procedure TUndoClimate.Undo;
begin
  frmGoPhast.PhastModel.FmpClimate := FOldClimate;
end;

end.
