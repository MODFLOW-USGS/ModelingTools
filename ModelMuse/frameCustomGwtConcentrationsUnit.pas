unit frameCustomGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  GoPhastTypes, Vcl.ExtCtrls, Vcl.StdCtrls, SsButtonEd, RbwParser;

type
  TframeCustomGwtConcentrations = class(TFrame)
    rdgConcentrations: TRbwDataGrid4;
    pnl1: TPanel;
    btnedInitialConcentration: TssButtonEdit;
    lblInitialConcentration: TLabel;
    rparserThreeDFormulaElements: TRbwParser;
    procedure btnedInitialConcentrationChange(Sender: TObject);
    procedure rdgConcentrationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgConcentrationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgConcentrationsButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnedInitialConcentrationButtonClick(Sender: TObject);
  private
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifier(ACol: Integer): string;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifier(ACol: Integer; const Value: string);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure CreateInitialHeadFormula(Formula: string;
      const Orientation: TDataSetOrientation);
    { Private declarations }
  protected
    FDataAssigned: Boolean;
    FPestBlockParametersAndDataSets: TStringList;
    FPestParameters: TStringList;
    function GetPestModifiers: TStringList; virtual;
    procedure InitializeControls;
    property PestModifiers: TStringList read GetPestModifiers;
    function EvaluateFormulasAtLocation: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property PestMethod[ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
    { Public declarations }
  end;

implementation

uses
  frmCustomGoPhastUnit, frmGoPhastUnit, PhastModelUnit, ModflowParameterUnit,
  DataSetUnit, OrderedCollectionUnit, frmFormulaUnit,
  Modflow6TimeSeriesCollectionsUnit, Modflow6TimeSeriesUnit,
  frmConvertChoiceUnit, System.UITypes;

{$R *.dfm}

var
  FPestMethods: TStringList;

resourcestring
  StrErrorIn0sRow = 'Error in %0:s Row: %1:d Column: %2:d. %3:s';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrErrorInInitialCon = 'Error in initial concentration in the Lake transpo' +
  'rt package. %s';

{ TframeCustomGwtConcentrations }

procedure TframeCustomGwtConcentrations.InitializeControls;
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataArrayManager: TDataArrayManager;
  DataSetIndex: Integer;
  ADataArray: TDataArray;
  ModflowSteadyParameters: TModflowSteadyParameters;
  ParameterIndex: Integer;
  AParameter: TModflowSteadyParameter;
  CompilerList: TList;
  Index: Integer;
begin
  FxButton.Canvas.Font := Font;
  btnedInitialConcentration.Glyph := FxButton;
  btnedInitialConcentration.Text := '';

  rdgConcentrations.FixedCols := 2;
  rdgConcentrations.BeginUpdate;
  try
    for RowIndex := 1 to rdgConcentrations.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgConcentrations.ColCount - 1 do
      begin
        rdgConcentrations.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    rdgConcentrations.Cells[0,0] := StrStartingTime;
    rdgConcentrations.Cells[1,0] := StrEndingTime;
    rdgConcentrations.Cells[2,0] := StrStatus;

    rdgConcentrations.UseSpecialFormat[0, PestModifierRow] := True;
    rdgConcentrations.UseSpecialFormat[0, PestMethodRow] := True;
    rdgConcentrations.SpecialFormat[0, PestModifierRow] := rcf4String;
    rdgConcentrations.SpecialFormat[0, PestMethodRow] := rcf4String;
    rdgConcentrations.Cells[0, PestModifierRow] := StrPestModifier;
    rdgConcentrations.Cells[0, PestMethodRow] := StrModificationMethod;
    for ColIndex := 3 to rdgConcentrations.ColCount - 1 do
    begin
      PestMethod[ColIndex] := ppmMultiply;
    end;
  finally
    rdgConcentrations.EndUpdate;
  end;

  FPestParameters.Clear;
  FPestBlockParametersAndDataSets.Clear;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for DataSetIndex := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    ADataArray := DataArrayManager.DataSets[DataSetIndex];
    if ADataArray.PestParametersUsed then
    begin
      if ADataArray.EvaluatedAt = eaBlocks then
      begin
        FPestBlockParametersAndDataSets.AddObject(ADataArray.Name, ADataArray);
      end;
    end;
  end;
  ModflowSteadyParameters := frmGoPhast.PhastModel.ModflowSteadyParameters;
  for ParameterIndex := 0 to ModflowSteadyParameters.Count - 1 do
  begin
    AParameter := ModflowSteadyParameters[ParameterIndex];
    if AParameter.ParameterType = ptPEST then
    begin
      FPestBlockParametersAndDataSets.AddObject(AParameter.ParameterName, AParameter);
      FPestParameters.AddObject(AParameter.ParameterName, AParameter);
    end;
  end;
  FPestBlockParametersAndDataSets.Sorted := True;
  FPestBlockParametersAndDataSets.Sorted := False;
  FPestParameters.Sorted := True;
  FPestParameters.Sorted := False;
  FPestBlockParametersAndDataSets.Insert(0, strNone);
  FPestParameters.Insert(0, strNone);

  rparserThreeDFormulaElements.ClearExpressions;
  rparserThreeDFormulaElements.ClearVariables;
  CompilerList := TList.Create;
  try
    CompilerList.Add(rparserThreeDFormulaElements);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;

  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    ADataArray := DataArrayManager.DataSets[Index];
    if not ADataArray.Visible then
    begin
      Continue;
    end;
    if ADataArray is TCustomSparseDataSet then
    begin
      Continue;
    end;
    if (eaBlocks = ADataArray.EvaluatedAt) then
    begin
      case ADataArray.DataType of
        rdtDouble:
          begin
            rparserThreeDFormulaElements.CreateVariable(ADataArray.Name,
              ADataArray.FullClassification, 0.0, ADataArray.DisplayName);
          end;
        rdtInteger:
          begin
            rparserThreeDFormulaElements.CreateVariable(ADataArray.Name,
              ADataArray.FullClassification, 0, ADataArray.DisplayName);
          end;
        rdtBoolean:
          begin
            rparserThreeDFormulaElements.CreateVariable(ADataArray.Name,
              ADataArray.FullClassification, False, ADataArray.DisplayName);
          end;
        rdtString:
          begin
            rparserThreeDFormulaElements.CreateVariable(ADataArray.Name,
              ADataArray.FullClassification, '', ADataArray.DisplayName);
          end;
        else
          Assert(False);
      end;
    end;
  end;

  FDataAssigned := False;
end;

procedure TframeCustomGwtConcentrations.rdgConcentrationsButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  NewValue: string;
  Index: Integer;
  Variable: TCustomValue;
begin
  NewValue := rdgConcentrations.Cells[ACol, ARow];
  if (NewValue = '') then
  begin
    NewValue := '0';
  end;

  with frmFormula do
  begin
    try
      Initialize;
      // For lakes,
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.
      if EvaluateFormulasAtLocation then
      begin
        IncludeGIS_Functions(eaBlocks);
        RemoveGetVCont;
        RemoveHufFunctions;
        for Index := 0 to rparserThreeDFormulaElements.VariableCount - 1 do
        begin
          Variable := rparserThreeDFormulaElements.Variables[Index];
          if rbFormulaParser.IndexOfVariable(Variable.Name) < 0 then
          begin
            rbFormulaParser.RegisterVariable(Variable);
          end;
        end;
      end;

      PopupParent := GetParentForm(self);

      // Show the functions and global variables.
      IncludeTimeSeries := True;
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        try
          CreateBoundaryFormula(rdgConcentrations, ACol, ARow, Formula, dso3D,
            eaBlocks);
        except on E: Exception do
          begin
            Beep;
            MessageDlg(Format(StrErrorIn0sRow,
              ['Lake transport package',
              ARow + 1, ACol+1, E.Message]), mtError,[mbOK], 0);
            Exit;
          end;
        end;
      end;
    finally
      Initialize;
    end;
  end
end;

procedure TframeCustomGwtConcentrations.rdgConcentrationsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
begin
  if (ACol = 2)  and  (ARow <= PestRowOffset) then
  begin
    CanSelect := False;
  end;
  if rdgConcentrations.Drawing then
  begin
    Exit;
  end;
  if (ARow >= 1) and (ACol >= 3) then
  begin
    Column := rdgConcentrations.Columns[ACol];
    if (ARow <= PestRowOffset)  then
    begin
      Column.ComboUsed := True;
      Column.LimitToList := True;
      if ARow = PestMethodRow then
      begin
        Column.PickList := FPestMethods
      end
      else
      begin
        Column.PickList := PestModifiers;
      end;
    end
    else
    begin
      Column.ButtonUsed := True;
      Column.LimitToList := False;
    end;
  end;
end;

procedure TframeCustomGwtConcentrations.rdgConcentrationsSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  FDataAssigned := True;
end;

procedure TframeCustomGwtConcentrations.btnedInitialConcentrationButtonClick(
  Sender: TObject);
var
  NewValue: string;
  Index: Integer;
  Variable: TCustomValue;
begin
  NewValue := btnedInitialConcentration.Text;
  if (NewValue = '') then
  begin
    NewValue := '0';
  end;

  with frmFormula do
  begin
    try
      Initialize;
      // For lakes,
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.
      if EvaluateFormulasAtLocation then
      begin
        IncludeGIS_Functions(eaBlocks);
        RemoveGetVCont;
        RemoveHufFunctions;
        for Index := 0 to rparserThreeDFormulaElements.VariableCount - 1 do
        begin
          Variable := rparserThreeDFormulaElements.Variables[Index];
          rbFormulaParser.RegisterVariable(Variable);
        end;
      end;

      PopupParent := GetParentForm(self);

      // Show the functions and global variables.
      IncludeTimeSeries := True;
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        try
          CreateInitialHeadFormula(Formula, dso3D);
        except on E: Exception do
          begin
            Beep;
            MessageDlg(Format(StrErrorInInitialCon,
              [E.Message]), mtError,[mbOK], 0);
            Exit;
          end;
        end;
      end;
    finally
      Initialize;
    end;
  end
end;

procedure TframeCustomGwtConcentrations.btnedInitialConcentrationChange(
  Sender: TObject);
begin
  FDataAssigned := True;
end;

constructor TframeCustomGwtConcentrations.Create(AOwner: TComponent);
begin
  inherited;
  FPestBlockParametersAndDataSets := TStringList.Create;
  FPestParameters := TStringList.Create;
end;

procedure TframeCustomGwtConcentrations.CreateInitialHeadFormula(
  Formula: string; const Orientation: TDataSetOrientation);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
  PestParamAllowed: Boolean;
  TimeSeriesAllowed: Boolean;
  Mf6TimesSeries: TTimesSeriesCollections;
  TimeSeries: TMf6TimeSeries;
begin
  PestParamAllowed := True;
  if Formula = '' then
  begin
    Formula := '0';
  end;

  TimeSeries := nil;
  TimeSeriesAllowed := False;
  if TimeSeriesAllowed then
  begin
    Mf6TimesSeries := frmGoPhast.PhastModel.Mf6TimesSeries;
    TimeSeries := Mf6TimesSeries.GetTimeSeriesByName(Formula);
    if TimeSeries <> nil then
    begin
      Formula := '1';
    end;
  end;

  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := rparserThreeDFormulaElements;
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

  if PestParamAllowed then
  begin
    PestParamAllowed :=
      frmGoPhast.PhastModel.GetPestParameterByName(
      CompiledFormula.DecompileDisplay) <> nil;
  end;

  if (ResultType = CompiledFormula.ResultType) or
    ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
      then
  begin
    if TimeSeries <> nil then
    begin
      btnedInitialConcentration.Text := string(TimeSeries.SeriesName);
    end
    else
    begin
      btnedInitialConcentration.Text := CompiledFormula.DecompileDisplay;
    end;
  end
  else if PestParamAllowed then
  begin
    if TimeSeries <> nil then
    begin
      btnedInitialConcentration.Text := string(TimeSeries.SeriesName);
    end
    else
    begin
      btnedInitialConcentration.Text := CompiledFormula.DecompileDisplay;
    end;
  end
  else
  begin
    if TimeSeries <> nil then
    begin
      btnedInitialConcentration.Text := string(TimeSeries.SeriesName);
    end
    else
    begin
      Formula := AdjustFormula(Formula, CompiledFormula.ResultType, ResultType);
      TempCompiler.Compile(Formula);
      CompiledFormula := TempCompiler.CurrentExpression;
      btnedInitialConcentration.Text := CompiledFormula.DecompileDisplay;
    end;
  end;
  if Assigned(btnedInitialConcentration.OnChange) then
  begin
    btnedInitialConcentration.OnChange(btnedInitialConcentration);
  end;
end;


procedure TframeCustomGwtConcentrations.CreateBoundaryFormula(
  const DataGrid: TRbwDataGrid4; const ACol, ARow: integer; Formula: string;
  const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
  PestParamAllowed: Boolean;
  TimeSeriesAllowed: Boolean;
  Mf6TimesSeries: TTimesSeriesCollections;
  TimeSeries: TMf6TimeSeries;
begin
  PestParamAllowed := True;
  if Formula = '' then
  begin
    Formula := '0';
  end;

  TimeSeries := nil;
  TimeSeriesAllowed := True;
  if TimeSeriesAllowed then
  begin
    Mf6TimesSeries := frmGoPhast.PhastModel.Mf6TimesSeries;
    TimeSeries := Mf6TimesSeries.GetTimeSeriesByName(Formula);
    if TimeSeries <> nil then
    begin
      Formula := '1';
    end;
  end;

  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := rparserThreeDFormulaElements;
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

  if PestParamAllowed then
  begin
    PestParamAllowed :=
      frmGoPhast.PhastModel.GetPestParameterByName(
      CompiledFormula.DecompileDisplay) <> nil;
  end;

  if (ResultType = CompiledFormula.ResultType) or
    ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
      then
  begin
    if TimeSeries <> nil then
    begin
      DataGrid.Cells[ACol, ARow] := string(TimeSeries.SeriesName);
    end
    else
    begin
      DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
    end;
  end
  else if PestParamAllowed then
  begin
    if TimeSeries <> nil then
    begin
      DataGrid.Cells[ACol, ARow] := string(TimeSeries.SeriesName);
    end
    else
    begin
      DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
    end;
  end
  else
  begin
    if TimeSeries <> nil then
    begin
      DataGrid.Cells[ACol, ARow] := string(TimeSeries.SeriesName);
    end
    else
    begin
      Formula := AdjustFormula(Formula, CompiledFormula.ResultType, ResultType);
      TempCompiler.Compile(Formula);
      CompiledFormula := TempCompiler.CurrentExpression;
      DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
    end;
  end;
  if Assigned(DataGrid.OnSetEditText) then
  begin
    DataGrid.OnSetEditText(DataGrid, ACol, ARow, DataGrid.Cells[ACol, ARow]);
  end;
end;

destructor TframeCustomGwtConcentrations.Destroy;
begin
  FPestBlockParametersAndDataSets.Free;
  FPestParameters.Free;
  inherited;
end;

function TframeCustomGwtConcentrations.EvaluateFormulasAtLocation: Boolean;
begin
  result := True;
end;

function TframeCustomGwtConcentrations.GetPestMethod(
  ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
  ItemIndex := FPestMethods.IndexOf(
    rdgConcentrations.Cells[ACol,PestMethodRow]);
  if ItemIndex >= 0 then
  begin
    result := TPestParamMethod(ItemIndex);
  end
  else
  begin
    result := ppmMultiply;
  end;
end;

function TframeCustomGwtConcentrations.GetPestMethodAssigned(
  ACol: Integer): Boolean;
begin
  result := FPestMethods.IndexOf(rdgConcentrations.Cells[ACol,PestMethodRow]) >= 0;
end;

function TframeCustomGwtConcentrations.GetPestModifier(ACol: Integer): string;
begin
  result := rdgConcentrations.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeCustomGwtConcentrations.GetPestModifierAssigned(
  ACol: Integer): Boolean;
begin
  result := rdgConcentrations.Cells[ACol, PestModifierRow] <> '';
end;

function TframeCustomGwtConcentrations.GetPestModifiers: TStringList;
begin
  result := FPestBlockParametersAndDataSets;
end;

procedure TframeCustomGwtConcentrations.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  rdgConcentrations.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeCustomGwtConcentrations.SetPestMethodAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := '';
  end;
end;

procedure TframeCustomGwtConcentrations.SetPestModifier(ACol: Integer;
  const Value: string);
begin
  if Value = '' then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeCustomGwtConcentrations.SetPestModifierAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := '';
  end;
end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
