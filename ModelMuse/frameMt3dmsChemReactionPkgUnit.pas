unit frameMt3dmsChemReactionPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4,
  GoPhastTypes;

type
  TframeMt3dmsChemReactionPkg = class(TframePackage)
    comboSorptionChoice: TJvImageComboBox;
    comboKineticChoice: TJvImageComboBox;
    cbInitialConcChoice: TCheckBox;
    lblSorptionChoice: TLabel;
    lblKineticChoice: TLabel;
    rdgYieldCoefficient: TRbwDataGrid4;
    procedure rdgYieldCoefficientButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure comboKineticChoiceChange(Sender: TObject);
  private
    FNumberOfSpecies: Integer;
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    { Private declarations }
  public
    procedure SetSpeciesNames(Names: TStringList);
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    property NumberOfSpecies: Integer read FNumberOfSpecies;
    { Public declarations }
  end;

var
  frameMt3dmsChemReactionPkg: TframeMt3dmsChemReactionPkg;

implementation

uses
  System.Math, RbwParser, frmGoPhastUnit, frmFormulaUnit,
    frmConvertChoiceUnit;

resourcestring
  StrYieldCoefficients = 'Yield Coefficients';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrErrorIn0sRow = 'Error in %0:s Row: %1:d Column: %2:d. %3:s';

{$R *.dfm}

{ TframeMt3dmsChemReactionPkg }

procedure TframeMt3dmsChemReactionPkg.comboKineticChoiceChange(Sender: TObject);
var
  KineticChoice: TKineticChoice;
begin
  inherited;
  if comboKineticChoice.ItemIndex >= 0 then
  begin
    KineticChoice := TKineticChoice(comboKineticChoice.ItemIndex);
    rdgYieldCoefficient.Enabled := KineticChoice = kcFirstOrderChain;
  end
  else
  begin
    rdgYieldCoefficient.Enabled := False;
  end;
end;

procedure TframeMt3dmsChemReactionPkg.CreateBoundaryFormula(
  const DataGrid: TRbwDataGrid4; const ACol, ARow: integer; Formula: string;
  const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
  PestParamAllowed: Boolean;
begin
//  PestParamAllowed := GetPestParameterAllowed(DataGrid, ACol);
  PestParamAllowed := False;
  if Formula = '' then
  begin
    Formula := '0';
  end;
  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := frmGoPhast.PhastModel.GetCompiler(Orientation, EvaluatedAt);
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
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end
  else if PestParamAllowed then
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

procedure TframeMt3dmsChemReactionPkg.GetData(
  Package: TModflowPackageSelection);
var
  RctPkg: TMt3dmsChemReaction;
  ItemIndex: Integer;
begin
  inherited;
  RctPkg := Package as TMt3dmsChemReaction;
  comboSorptionChoice.ItemIndex := Ord(RctPkg.SorptionChoice);
  comboKineticChoice.ItemIndex := Ord(RctPkg.KineticChoice);
  cbInitialConcChoice.Checked := RctPkg.OtherInitialConcChoice = oicUse;

  rdgYieldCoefficient.Cells[1,0] := StrYieldCoefficients;
  for ItemIndex := 0 to RctPkg.YieldCoefficients.Count - 1 do
  begin
    rdgYieldCoefficient.Cells[1,ItemIndex+1] := RctPkg.YieldCoefficients[ItemIndex];
  end;
end;

procedure TframeMt3dmsChemReactionPkg.rdgYieldCoefficientButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  Orientation: TDataSetOrientation;
  DataGrid: TRbwDataGrid4;
  EvaluatedAt: TEvaluatedAt;
  NewValue: string;
  AControl: TControl;
begin
  inherited;
  DataGrid := rdgYieldCoefficient;
  // Lakes and reservoirs can only be specified from the top.
  Orientation := dso3D;
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

      AControl := self;
      While AControl <> nil do
      begin
        if AControl is TCustomForm then
        begin
          PopupParent := TCustomForm(AControl);
          break;
        end;
        AControl := AControl.Parent
      end;



      // Show the functions and global variables.
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        try
          CreateBoundaryFormula(DataGrid, ACol, ARow, Formula, Orientation,
            EvaluatedAt);
        except on E: Exception do
          begin
            Beep;
            MessageDlg(Format(StrErrorIn0sRow,
              ['MT3D-USGS Reaction package yield coefficients',
              ARow + 1, ACol+1, E.Message]), mtError,[mbOK], 0);
            Exit;
          end;
        end;
      end;
    finally
      Initialize;
//      Free;
    end;
  end;
end;

procedure TframeMt3dmsChemReactionPkg.SetData(
  Package: TModflowPackageSelection);
var
  RctPkg: TMt3dmsChemReaction;
  RowIndex: Integer;
  YieldCoefficients: TStringList;
begin
  inherited;
  RctPkg := Package as TMt3dmsChemReaction;
  RctPkg.SorptionChoice := TSorptionChoice(comboSorptionChoice.ItemIndex);
  RctPkg.KineticChoice := TKineticChoice(comboKineticChoice.ItemIndex);
  if cbInitialConcChoice.Checked then
  begin
    RctPkg.OtherInitialConcChoice := oicUse;
  end
  else
  begin
    RctPkg.OtherInitialConcChoice := oicDontUse;
  end;

  YieldCoefficients := TStringList.Create;
  try
    YieldCoefficients.Capacity := rdgYieldCoefficient.RowCount - 1;
    for RowIndex := 1 to rdgYieldCoefficient.RowCount - 1 do
    begin
      YieldCoefficients.Add(rdgYieldCoefficient.Cells[1,RowIndex]);
    end;
    RctPkg.YieldCoefficients := YieldCoefficients;
  finally
    YieldCoefficients.Free;
  end;
end;

procedure TframeMt3dmsChemReactionPkg.SetSpeciesNames(Names: TStringList);
var
  Index: Integer;
begin
  // The number of yield coefficients is one less than the number of species.
  FNumberOfSpecies := Names.Count;
  rdgYieldCoefficient.RowCount := Max(Names.Count, 2);
  for Index := 1 to Names.Count - 1 do
  begin
    rdgYieldCoefficient.Cells[0, Index] :=
      Format('%0:s -> %1:s', [Names[Index-1], Names[Index]]);
  end;


end;

end.
