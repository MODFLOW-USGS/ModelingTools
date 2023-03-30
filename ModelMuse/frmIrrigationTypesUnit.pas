unit frmIrrigationTypesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ExtCtrls,
  frameGridUnit, Vcl.StdCtrls, Vcl.Buttons, UndoItems, ModflowFmpIrrigationUnit,
  JvPageList, JvExControls, JvExExtCtrls, JvNetscapeSplitter, Vcl.ComCtrls,
  JvExComCtrls, JvPageListTreeView, frameFormulaGridUnit, RbwDataGrid4,
  GoPhastTypes, RbwParser, PhastModelUnit, ModflowPackageSelectionUnit;

type
  TIrrigationColumn = (icNumber, icName {, icEfficiency});

  TEvapFractionColumns = (efcStart, efcEnd, efcEvaporationFraction,
    efcSurfaceWaterLossFractionIrrigate);

  TfrmIrrigationTypes = class(TfrmCustomGoPhast)
    frameIrrigationTypes: TframeGrid;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    jvpltvMain: TJvPageListTreeView;
    splitterMain: TJvNetscapeSplitter;
    jvplMain: TJvPageList;
    jvspIrrigationTypes: TJvStandardPage;
    jvspEvapFraction: TJvStandardPage;
    frameEvaporationFractions: TframeFormulaGrid;
    rbwprsrGlobal: TRbwParser;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameIrrigationTypesGridEndUpdate(Sender: TObject);
    procedure frameIrrigationTypesseNumberChange(Sender: TObject);
    procedure jvpltvMainChange(Sender: TObject; Node: TTreeNode);
    procedure frameEvaporationFractionsGridEndUpdate(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure GridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure jvpltvMainCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FIrrigationTypes: TIrrigationCollection;
    FGettingData: Boolean;
    FIrrigationTypesNode: TJvPageIndexNode;
    FFarmProcess4: TFarmProcess4;
    FFarmLandUse: TFarmProcess4LandUse;
    FEvapFraction: TFmp4EvapFractionCollection;
    procedure GetData;
    procedure SetData;
    procedure InitializeGrid;
    procedure SetGridColumnProperties(Grid: TRbwDataGrid4);
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure GetGlobalVariables;
    procedure SetUseButton(Grid: TRbwDataGrid4; StartCol: Integer);
    procedure SetStartAndEndTimeLists(StartTimes, EndTimes: TStringList;
      Grid: TRbwDataGrid4);
    procedure EvapFractionTable(Model: TCustomModel);
    procedure GeEvapFraction(EvapFraction: TFmp4EvapFractionCollection);
    procedure CreateChildNodes(IrrigationItem: TIrrigationItem;
      IrrigationNode: TJvPageIndexNode);
    procedure GetIrrigationTypeNames;
    { Private declarations }
  public
    destructor Destroy; override;
    { Public declarations }
  end;

  TUndoSetIrrigationTypes = class(TCustomUndo)
  private
    FNewIrrigationTypes: TIrrigationCollection;
    FOldIrrigationTypes: TIrrigationCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var IrrigationTypes: TIrrigationCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmIrrigationTypes: TfrmIrrigationTypes;

implementation

uses
  frmGoPhastUnit, frmConvertChoiceUnit, ModflowTimeUnit, frmFormulaUnit;

resourcestring
  StrErrorInFormulaS = 'Error in formula: %s';
  StrEvaporationIrrigati = 'Evaporation Irrigation Fraction';
  StrSurfaceWaterLossF = 'Surface Water Loss Fraction Irrigation';
  StrIrrigationFractions = 'Irrigation Fractions';
  StrIrrigationTypes = 'Irrigation Types';

{$R *.dfm}

{ TfrmModflowIrrigationTypes }

procedure TfrmIrrigationTypes.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmIrrigationTypes.CreateBoundaryFormula(
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
//  if (DataGrid = frameCropWaterUse.Grid)then
//  begin
//    if ACol = Ord(cwuIrrigated) then
//    begin
//      ResultType := rdtBoolean;
//    end
//  end
//  else if (DataGrid = frameIrrigation.Grid)then
//  begin
//    if ACol = Ord(IcIrrigation) then
//    begin
//      ResultType := rdtInteger;
//    end
//  end
//  else if (DataGrid = frameCropName.Grid)then
//  begin
//    if (FFallowStart > 0) and (ACol = FFallowStart + Ord(fcFallow)) then
//    begin
//      ResultType := rdtBoolean;
//    end;
//    if (FCropPropStart > 0) and (ACol = FCropPropStart + Ord(cpIrrigated)) then
//    begin
//      ResultType := rdtBoolean;
//    end;
//  end;

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

procedure TfrmIrrigationTypes.CreateChildNodes(IrrigationItem: TIrrigationItem;
  IrrigationNode: TJvPageIndexNode);
var
  ANode: TJvPageIndexNode;
begin
{$IFDEF OWHMV2}
  if (frmGoPhast.ModelSelection = msModflowOwhm2)
    and FFarmProcess4.IsSelected and FFarmLandUse.IsSelected
    and (FFarmLandUse.IrrigationListUsed
      or FFarmLandUse.EvapIrrigateFractionListByIrrigationUsed
      or FFarmLandUse.SwLossFracIrrigListByIrrigationUsed) then
  begin
    ANode := jvpltvMain.Items.AddChild(
      IrrigationNode, StrIrrigationFractions) as TJvPageIndexNode;
    ANode.PageIndex := jvspEvapFraction.PageIndex;
    ANode.Data := IrrigationItem.EvapFraction;
  end;
{$ENDIF}
end;

destructor TfrmIrrigationTypes.Destroy;
begin
  FIrrigationTypes.Free;
  inherited;
end;

procedure TfrmIrrigationTypes.GetIrrigationTypeNames;
var
  Index: Integer;
begin
  frameIrrigationTypes.Grid.BeginUpdate;
  try
    InitializeGrid;
    frameIrrigationTypes.seNumber.AsInteger := FIrrigationTypes.Count;
    for Index := 0 to FIrrigationTypes.Count - 1 do
    begin
      frameIrrigationTypes.Grid.Cells[Ord(icNumber), Index + 1] :=
        IntToStr(Index + 1);
      frameIrrigationTypes.Grid.Cells[Ord(icName), Index + 1] :=
        FIrrigationTypes[Index].Name;
      frameIrrigationTypes.Grid.Objects[Ord(icName), Index + 1] :=
        FIrrigationTypes[Index];
    end;
  finally
    frameIrrigationTypes.Grid.EndUpdate;
  end;
end;

procedure TfrmIrrigationTypes.GridButtonClick(Sender: TObject; ACol,
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
        if Assigned(DataGrid.OnEndUpdate) then
        begin
          DataGrid.OnEndUpdate(nil);
        end;
      end;
    finally
      Initialize;
//      Free;
    end;
  end;
end;

procedure TfrmIrrigationTypes.GridSetEditText(Sender: TObject; ACol,
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

procedure TfrmIrrigationTypes.FormCreate(Sender: TObject);
begin
  inherited;
  jvplMain.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmIrrigationTypes.frameEvaporationFractionsGridEndUpdate(
  Sender: TObject);
var
  RowIndex: Integer;
  ItemCount: Integer;
  StartTime: double;
  EndTime: double;
  AnItem: TEvapFractionItem;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;

  frameEvaporationFractions.GridEndUpdate(Sender);
  if FEvapFraction <> nil then
  begin
//  TEvapFractionColumns = (efcStart, efcEnd, efcEvaporationFraction);
    ItemCount := 0;
    for RowIndex := 1 to frameEvaporationFractions.seNumber.AsInteger do
    begin
      if TryStrToFloat(frameEvaporationFractions.Grid.Cells[
        Ord(efcStart), RowIndex], StartTime)
        and TryStrToFloat(frameEvaporationFractions.Grid.Cells[
        Ord(efcEnd), RowIndex], EndTime) then
      begin
        if ItemCount >= FEvapFraction.Count then
        begin
          FEvapFraction.Add;
        end;
        AnItem := FEvapFraction[ItemCount];
        AnItem.StartTime := StartTime;
        AnItem.EndTime := EndTime;
        AnItem.EvapIrrigateFraction :=
          frameEvaporationFractions.Grid.Cells[Ord(efcEvaporationFraction), RowIndex];
        AnItem.SurfaceWaterLossFractionIrrigation :=
          frameEvaporationFractions.Grid.Cells[Ord(efcSurfaceWaterLossFractionIrrigate), RowIndex];
        Inc(ItemCount);
      end;
    end;
    while FEvapFraction.Count > ItemCount do
    begin
      FEvapFraction.Last.Free;
    end;
  end
end;

procedure TfrmIrrigationTypes.frameIrrigationTypesGridEndUpdate(
  Sender: TObject);
begin
  inherited;
  if not FGettingData then
  begin
    frameIrrigationTypes.GridEndUpdate(Sender);
  end;
end;

procedure TfrmIrrigationTypes.frameIrrigationTypesseNumberChange(
  Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameIrrigationTypes.seNumberChange(Sender);
  for RowIndex := 1 to frameIrrigationTypes.Grid.RowCount - 1 do
  begin
    frameIrrigationTypes.Grid.Cells[Ord(icNumber), RowIndex] :=
      IntToStr(RowIndex);
  end;
end;

procedure TfrmIrrigationTypes.InitializeGrid;
begin
  frameIrrigationTypes.Grid.Cells[Ord(icName), 0] := 'Irrigation Type';
//  frameIrrigationTypes.Grid.Cells[Ord(icEfficiency), 0] := 'Efficiency';
  frameIrrigationTypes.Grid.Cells[Ord(icNumber), 1] := '1';
end;

procedure TfrmIrrigationTypes.jvpltvMainChange(Sender: TObject;
  Node: TTreeNode);
var
  AnObject: TObject;
begin
  inherited;
  if Node.Data <> nil then
  begin
    FGettingData := True;
    try
      AnObject := Node.Data;
      Assert(AnObject <> nil);
      if AnObject is TFmp4EvapFractionCollection then
      begin
        GeEvapFraction(TFmp4EvapFractionCollection(AnObject));
      end;

    finally
      FGettingData := False;
    end;
  end;

end;

procedure TfrmIrrigationTypes.jvpltvMainCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TfrmIrrigationTypes.GeEvapFraction(
  EvapFraction: TFmp4EvapFractionCollection);
var
  ItemIndex: Integer;
  AnItem: TEvapFractionItem;
begin
  Assert(EvapFraction <> nil);
  FEvapFraction := EvapFraction;
  frameEvaporationFractions.ClearGrid;
  frameEvaporationFractions.seNumber.AsInteger := EvapFraction.Count;
  frameEvaporationFractions.seNumber.OnChange(frameEvaporationFractions.seNumber);
  if frameEvaporationFractions.seNumber.AsInteger = 0 then
  begin
    frameEvaporationFractions.Grid.Row := 1;
    frameEvaporationFractions.ClearSelectedRow;
  end;
  frameEvaporationFractions.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to EvapFraction.Count - 1 do
    begin
      AnItem := EvapFraction[ItemIndex];
      frameEvaporationFractions.Grid.Cells[Ord(efcStart), ItemIndex+1] :=
        FloatToStr(AnItem.StartTime);
      frameEvaporationFractions.Grid.Cells[Ord(efcEnd), ItemIndex+1] :=
        FloatToStr(AnItem.EndTime);
      frameEvaporationFractions.Grid.Cells[Ord(efcEvaporationFraction), ItemIndex+1] :=
        AnItem.EvapIrrigateFraction;
      frameEvaporationFractions.Grid.Cells[Ord(efcSurfaceWaterLossFractionIrrigate), ItemIndex+1] :=
        AnItem.SurfaceWaterLossFractionIrrigation;
    end;
  finally
    frameEvaporationFractions.Grid.EndUpdate;
  end;
end;

procedure TfrmIrrigationTypes.GetData;
var
  StartTimes: TStringList;
  EndTimes: TStringList;
  StressPeriods: TModflowStressPeriods;
  TimeIndex: Integer;
  AStressPeriod: TModflowStressPeriod;
  IrrIndex: Integer;
  IrrigationType: TIrrigationItem;
  IrrigationNode: TJvPageIndexNode;
begin
  FGettingData := True;
  try
    GetGlobalVariables;
    EvapFractionTable(frmGoPhast.PhastModel);

    StartTimes := TStringList.Create;
    EndTimes := TStringList.Create;
    try
      StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
      for TimeIndex := 0 to StressPeriods.Count - 1 do
      begin
        AStressPeriod := StressPeriods[TimeIndex];
        StartTimes.Add(FloatToStr(AStressPeriod.StartTime));
        EndTimes.Add(FloatToStr(AStressPeriod.EndTime));
      end;
      SetStartAndEndTimeLists(StartTimes, EndTimes,
        frameEvaporationFractions.Grid);
    finally
      EndTimes.Free;
      StartTimes.Free;
    end;


    FIrrigationTypes := TIrrigationCollection.Create(nil);
    FIrrigationTypes.Assign(frmGoPhast.PhastModel.IrrigationTypes);
    GetIrrigationTypeNames;

    FIrrigationTypesNode := jvpltvMain.Items.Add(nil, StrIrrigationTypes) as TJvPageIndexNode;
    FIrrigationTypesNode.PageIndex := jvspIrrigationTypes.PageIndex;
    FIrrigationTypesNode.Data := FIrrigationTypes;

    FFarmProcess4 := frmGoPhast.PhastModel.ModflowPackages.FarmProcess4;
    FFarmLandUse := frmGoPhast.PhastModel.ModflowPackages.FarmLandUse;

    for IrrIndex := 0 to FIrrigationTypes.Count - 1 do
    begin
      IrrigationType := FIrrigationTypes[IrrIndex];
      IrrigationNode := jvpltvMain.Items.Add(nil, IrrigationType.Name) as TJvPageIndexNode;
      CreateChildNodes(IrrigationType, IrrigationNode);
    end;

  finally
    FGettingData := False;
  end;
end;

procedure TfrmIrrigationTypes.GetGlobalVariables;
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

procedure TfrmIrrigationTypes.SetData;
var
  ItemList: TList;
  AnObject: TObject;
  IrrigationItem: TIrrigationItem;
  RowIndex: Integer;
  ItemIndex: Integer;
begin
  ItemList := TList.Create;
  try
    for RowIndex := 1 to frameIrrigationTypes.seNumber.AsInteger do
    begin
      AnObject := frameIrrigationTypes.Grid.Objects[Ord(icName), RowIndex];
      if AnObject <> nil then
      begin
        ItemList.Add(AnObject);
      end;
    end;
    for ItemIndex := 0 to FIrrigationTypes.Count - 1 do
    begin
      IrrigationItem := FIrrigationTypes[ItemIndex];
      if ItemList.IndexOf(IrrigationItem) < 0 then
      begin
        IrrigationItem.Free;
      end;
    end;
  finally
    ItemList.Free;
  end;

  for RowIndex := 1 to frameIrrigationTypes.seNumber.AsInteger do
  begin
    AnObject := frameIrrigationTypes.Grid.Objects[Ord(icName), RowIndex];
    if AnObject = nil then
    begin
      IrrigationItem := FIrrigationTypes.Add as TIrrigationItem;
    end
    else
    begin
      IrrigationItem := AnObject as TIrrigationItem;
    end;
    IrrigationItem.Index := RowIndex -1;
    IrrigationItem.Name := frameIrrigationTypes.Grid.Cells[Ord(icName), RowIndex];
  end;

  if FIrrigationTypes.IsSame(frmGoPhast.PhastModel.IrrigationTypes) then
  begin
    FreeAndNil(FIrrigationTypes);
  end
  else
  begin
    frmGoPhast.UndoStack.Submit(TUndoSetIrrigationTypes.Create(FIrrigationTypes));
  end;
end;

procedure TfrmIrrigationTypes.SetGridColumnProperties(Grid: TRbwDataGrid4);
var
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].WordWrapCaptions := True;
    Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
    Grid.Columns[ColIndex].AutoAdjustColWidths := True;
  end;
  Grid.EndUpdate;
  Grid.BeginUpdate;
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  Grid.EndUpdate;
end;

procedure TfrmIrrigationTypes.SetStartAndEndTimeLists(StartTimes,
  EndTimes: TStringList; Grid: TRbwDataGrid4);
begin
  Grid.Columns[0].PickList := StartTimes;
  Grid.Columns[0].ComboUsed := True;
  Grid.Columns[1].PickList := EndTimes;
  Grid.Columns[1].ComboUsed := True;
end;

procedure TfrmIrrigationTypes.EvapFractionTable(Model: TCustomModel);
begin
  frameEvaporationFractions.Grid.ColCount := 4;
  frameEvaporationFractions.Grid.FixedCols := 0;
  frameEvaporationFractions.Grid.Columns[Ord(efcStart)].Format := rcf4Real;
  frameEvaporationFractions.Grid.Columns[Ord(efcEnd)].Format := rcf4Real;
  frameEvaporationFractions.Grid.Cells[Ord(efcStart), 0] := StrStartingTime;
  frameEvaporationFractions.Grid.Cells[Ord(efcEnd), 0] := StrEndingTime;
  frameEvaporationFractions.Grid.Cells[Ord(efcEvaporationFraction), 0] :=
    StrEvaporationIrrigati;
  frameEvaporationFractions.Grid.Cells[Ord(efcSurfaceWaterLossFractionIrrigate), 0] :=
    StrSurfaceWaterLossF;
  SetGridColumnProperties(frameEvaporationFractions.Grid);
  SetUseButton(frameEvaporationFractions.Grid, Ord(efcEvaporationFraction));
  frameEvaporationFractions.FirstFormulaColumn := 2;
  frameEvaporationFractions.LayoutMultiRowEditControls;
end;

procedure TfrmIrrigationTypes.SetUseButton(Grid: TRbwDataGrid4;
  StartCol: Integer);
var
  ColIndex: Integer;
begin
  for ColIndex := StartCol to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].UseButton := True;
    Grid.Columns[ColIndex].ButtonCaption := StrFormulaButtonCaption;
    Grid.Columns[ColIndex].ButtonWidth := 35;
  end;
end;

{ TUndoSetIrrigationTypes }

constructor TUndoSetIrrigationTypes.Create(
  var IrrigationTypes: TIrrigationCollection);
begin
  FNewIrrigationTypes := IrrigationTypes;
  IrrigationTypes := nil;
  FOldIrrigationTypes := TIrrigationCollection.Create(nil);
  FOldIrrigationTypes.Assign(frmGoPhast.PhastModel.IrrigationTypes);
end;

function TUndoSetIrrigationTypes.Description: string;
begin
  result := 'change irrigation types';
end;

destructor TUndoSetIrrigationTypes.Destroy;
begin
  FNewIrrigationTypes.Free;
  FOldIrrigationTypes.Free;
  inherited;
end;

procedure TUndoSetIrrigationTypes.DoCommand;
begin
  frmGoPhast.PhastModel.IrrigationTypes := FNewIrrigationTypes
end;

procedure TUndoSetIrrigationTypes.Undo;
begin
  frmGoPhast.PhastModel.IrrigationTypes := FOldIrrigationTypes
end;

end.
