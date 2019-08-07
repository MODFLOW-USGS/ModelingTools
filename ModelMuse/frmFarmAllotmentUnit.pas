unit frmFarmAllotmentUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, RbwParser,
  StdCtrls, Buttons, ExtCtrls, frameGridUnit, frameFormulaGridUnit,
  RbwDataGrid4, GoPhastTypes, ModflowFmpAllotmentUnit, UndoItems;

type
  TUndoAllotment = class(TCustomUndo)
  private
    FOldAllotment: TAllotmentCollection;
    FNewAllotment: TAllotmentCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var NewAllotment: TAllotmentCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  TfrmFarmAllotment = class(TfrmCustomGoPhast)
    frameAllotment: TframeFormulaGrid;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    rbwprsrGlobal: TRbwParser;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure frameAllotmentGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure frameAllotmentGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    FAllotment: TAllotmentCollection;
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
  frmFarmAllotment: TfrmFarmAllotment;

implementation

uses
  frmGoPhastUnit, frmConvertChoiceUnit, frmFormulaUnit;

resourcestring
  StrSurfacewaterAllotm = 'Surface-water allotment height (ALLOT)';

{$R *.dfm}

type
  TAllotmentColumns = (acStartingTime, acEndingTime, acAllotment);

{ TfrmFarmAllotment }

procedure TfrmFarmAllotment.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFarmAllotment.CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
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
      raise ERbwParserError.Create(Format('Error in formula: %s',
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

procedure TfrmFarmAllotment.FormCreate(Sender: TObject);
begin
  inherited;
  InitializeGridCaptions;
  frameAllotment.FirstFormulaColumn := Ord(acAllotment);
  frameAllotment.LayoutMultiRowEditControls;
  GetStartingAndEndingTimes;
  GetGlobalVariables;
  GetData;
end;

procedure TfrmFarmAllotment.FormDestroy(Sender: TObject);
begin
  FAllotment.Free;
  inherited;

end;

procedure TfrmFarmAllotment.frameAllotmentGridButtonClick(Sender: TObject; ACol,
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

procedure TfrmFarmAllotment.frameAllotmentGridSetEditText(Sender: TObject; ACol,
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

procedure TfrmFarmAllotment.GetData;
var
  ItemIndex: Integer;
  AnItem: TAllotmentItem;
begin
  FAllotment := TAllotmentCollection.Create(nil);
  FAllotment.Assign(frmGoPhast.PhastModel.FmpAllotment);

  frameAllotment.seNumber.AsInteger := FAllotment.Count;
  frameAllotment.seNumber.OnChange(frameAllotment.seNumber);
  if frameAllotment.seNumber.AsInteger = 0 then
  begin
    frameAllotment.Grid.Row := 1;
    frameAllotment.ClearSelectedRow;
  end;
  frameAllotment.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to FAllotment.Count - 1 do
    begin
      AnItem := FAllotment[ItemIndex];
      frameAllotment.Grid.Cells[Ord(acStartingTime), ItemIndex+1] := FloatToStr(AnItem.StartTime);
      frameAllotment.Grid.Cells[Ord(acEndingTime), ItemIndex+1] := FloatToStr(AnItem.EndTime);
      frameAllotment.Grid.Cells[Ord(acAllotment), ItemIndex+1] := AnItem.Allotment;
    end;
  finally
    frameAllotment.Grid.EndUpdate;
  end;
end;

procedure TfrmFarmAllotment.GetGlobalVariables;
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

procedure TfrmFarmAllotment.GetStartingAndEndingTimes;
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
    (frameAllotment.Grid, Ord(acStartingTime));
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
    (frameAllotment.Grid, Ord(acEndingTime));
  frameAllotment.Grid.Columns[Ord(acStartingTime)].ComboUsed := True;
  frameAllotment.Grid.Columns[Ord(acEndingTime)].ComboUsed := True;
end;

procedure TfrmFarmAllotment.InitializeGridCaptions;
var
  ColIndex: Integer;
begin
  frameAllotment.Grid.BeginUpdate;
  try
    for ColIndex := 0 to frameAllotment.Grid.ColCount - 1 do
    begin
      frameAllotment.Grid.Columns[ColIndex].AutoAdjustColWidths := true;
    end;
    frameAllotment.Grid.Cells[Ord(acStartingTime), 0] := StrStartingTime;
    frameAllotment.Grid.Cells[Ord(acEndingTime), 0] := StrEndingTime;
    frameAllotment.Grid.Cells[Ord(acAllotment), 0] := StrSurfacewaterAllotm;
  finally
    frameAllotment.Grid.EndUpdate;
  end;
  for ColIndex := 0 to frameAllotment.Grid.ColCount - 1 do
  begin
    frameAllotment.Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
end;

procedure TfrmFarmAllotment.SetData;
var
  Count: Integer;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  AnAllotment: TAllotmentItem;
  StartingTime: double;
  EndingTime: double;
begin
  Count := 0;
  Grid := frameAllotment.Grid;
  for RowIndex := 1 to frameAllotment.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(acStartingTime), RowIndex], StartingTime)
      and TryStrToFloat(Grid.Cells[Ord(acEndingTime), RowIndex], EndingTime) then
    begin
      if Count >= FAllotment.Count then
      begin
        FAllotment.Add;
      end;
      AnAllotment := FAllotment[Count];
      AnAllotment.StartTime := StartingTime;
      AnAllotment.EndTime := EndingTime;
      AnAllotment.Allotment := Grid.Cells[ord(acAllotment), RowIndex];

      Inc(Count);
    end;
  end;
  while Count < FAllotment.Count do
  begin
    FAllotment.Last.Free;
  end;
  frmGoPhast.UndoStack.Submit(TUndoAllotment.Create(FAllotment));
end;

{ TUndoAllotment }

constructor TUndoAllotment.Create(var NewAllotment: TAllotmentCollection);
begin
  FOldAllotment := TAllotmentCollection.Create(nil);
  FOldAllotment.Assign(frmGoPhast.PhastModel.FmpAllotment);
  FNewAllotment := NewAllotment;
  NewAllotment := nil;
end;

function TUndoAllotment.Description: string;
begin
  result := 'change farm allotment';
end;

destructor TUndoAllotment.Destroy;
begin
  FOldAllotment.Free;
  FNewAllotment.Free;
  inherited;
end;

procedure TUndoAllotment.DoCommand;
begin
  frmGoPhast.PhastModel.FmpAllotment := FNewAllotment;
end;

procedure TUndoAllotment.Undo;
begin
  frmGoPhast.PhastModel.FmpAllotment := FOldAllotment;
end;

end.
