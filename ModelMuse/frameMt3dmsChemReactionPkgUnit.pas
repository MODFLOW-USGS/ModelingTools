unit frameMt3dmsChemReactionPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4,
  GoPhastTypes, ArgusDataEntry, Vcl.ComCtrls, Vcl.CheckLst, JvExCheckLst,
  JvCheckListBox, Vcl.Mask, JvExMask, JvSpin;

type
  TframeMt3dmsChemReactionPkg = class(TframePackage)
    comboSorptionChoice: TJvImageComboBox;
    comboKineticChoice: TJvImageComboBox;
    cbInitialConcChoice: TCheckBox;
    lblSorptionChoice: TLabel;
    lblKineticChoice: TLabel;
    rdgYieldCoefficient: TRbwDataGrid4;
    comboReactionChoice: TJvImageComboBox;
    lblReactionChoice: TLabel;
    comboElectronDonor: TComboBox;
    lblElectronDonor: TLabel;
    lblElectronAcceptor: TLabel;
    comboElectronAcceptor: TComboBox;
    rdeStochiometricRatio: TRbwDataEntry;
    lblStochiometricRatio: TLabel;
    PageControl1: TPageControl;
    tabMain: TTabSheet;
    tabKinetic1: TTabSheet;
    seElectronDonors: TJvSpinEdit;
    lblElectronDonors: TLabel;
    lblElectronAcceptors: TLabel;
    seElectronAcceptors: TJvSpinEdit;
    clbSpecialCases: TJvCheckListBox;
    lblSpecialCases: TLabel;
    cbSolidFe: TCheckBox;
    memoDonors: TMemo;
    memoAcceptors: TMemo;
    tabSpecialCases: TTabSheet;
    rdgSpecialCases: TRbwDataGrid4;
    tabElectronAcceptors: TTabSheet;
    rdgAcceptors: TRbwDataGrid4;
    procedure rdgYieldCoefficientButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure comboKineticChoiceChange(Sender: TObject);
    procedure comboReactionChoiceChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure seElectronDonorsChange(Sender: TObject);
    procedure seElectronAcceptorsChange(Sender: TObject);
    procedure clbSpecialCasesClickCheck(Sender: TObject);
    procedure clbSpecialCasesEnter(Sender: TObject);
    procedure clbSpecialCasesExit(Sender: TObject);
  private
    FNumberOfSpecies: Integer;
    FSpeciesNames: TStringList;
    FSpecialChanged: Boolean;
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    procedure EnableReactionItems;
    procedure EnableYieldCoefficients;
    procedure AssignDonorsAndAcceptors;
    procedure UpdateSpecialCaseGrid;
    { Private declarations }
  public
    procedure SetSpeciesNames(Names: TStringList);
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    property NumberOfSpecies: Integer read FNumberOfSpecies;
    Constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frameMt3dmsChemReactionPkg: TframeMt3dmsChemReactionPkg;

implementation

uses
  System.Math, RbwParser, frmGoPhastUnit, frmFormulaUnit,
    frmConvertChoiceUnit, frmCustomGoPhastUnit;

resourcestring
  StrYieldCoefficients = 'Yield Coefficients';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrErrorIn0sRow = 'Error in %0:s Row: %1:d Column: %2:d. %3:s';

{$R *.dfm}

{ TframeMt3dmsChemReactionPkg }

procedure TframeMt3dmsChemReactionPkg.clbSpecialCasesClickCheck(
  Sender: TObject);
begin
  inherited;
  FSpecialChanged := True
end;

procedure TframeMt3dmsChemReactionPkg.clbSpecialCasesEnter(Sender: TObject);
begin
  inherited;
  FSpecialChanged := False;
end;

procedure TframeMt3dmsChemReactionPkg.clbSpecialCasesExit(Sender: TObject);
begin
  inherited;
  UpdateSpecialCaseGrid;
end;

procedure TframeMt3dmsChemReactionPkg.comboKineticChoiceChange(Sender: TObject);
begin
  inherited;
  EnableYieldCoefficients;
end;

procedure TframeMt3dmsChemReactionPkg.comboReactionChoiceChange(
  Sender: TObject);
begin
  inherited;
  EnableReactionItems;
end;

constructor TframeMt3dmsChemReactionPkg.Create(Owner: TComponent);
begin
  inherited;
  FSpeciesNames := TStringList.Create;
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

destructor TframeMt3dmsChemReactionPkg.Destroy;
begin
  FSpeciesNames.Free;
  inherited;
end;

procedure TframeMt3dmsChemReactionPkg.UpdateSpecialCaseGrid;
var
  SpecialCases: TStringList;
  Index: Integer;
  ExistingSpecialCases: TStringList;
begin
  if FSpecialChanged then
  begin
    SpecialCases := TStringList.Create;
    try
      for Index := 0 to clbSpecialCases.Items.Count - 1 do
      begin
        if clbSpecialCases.Checked[Index] then
        begin
          SpecialCases.Add(clbSpecialCases.Items[Index]);
        end;
      end;
      SpecialCases.CaseSensitive := False;
      ExistingSpecialCases := TStringList.Create;
      try
        ExistingSpecialCases.Assign(rdgSpecialCases.Cols[0]);
        //        ExistingSpecialCases.Delete(0);
        for Index := ExistingSpecialCases.Count - 1 downto 1 do
        begin
          if SpecialCases.IndexOf(ExistingSpecialCases[Index]) < 0 then
          begin
            rdgSpecialCases.DeleteRow(Index);
          end;
        end;
        ExistingSpecialCases.Assign(rdgSpecialCases.Cols[0]);
        ExistingSpecialCases.CaseSensitive := False;
        ExistingSpecialCases.Delete(0);
        if SpecialCases.Count > 0 then
        begin
          rdgSpecialCases.RowCount := SpecialCases.Count + 1;
        end
        else
        begin
          rdgSpecialCases.RowCount := 2;
          ClearGrid(rdgSpecialCases);
          rdgSpecialCases.Cells[0, 1] := '';
        end;
        for Index := 0 to SpecialCases.Count - 1 do
        begin
          if ExistingSpecialCases.IndexOf(SpecialCases[Index]) < 0 then
          begin
            ExistingSpecialCases.Add(SpecialCases[Index]);
          end;
        end;
        if ExistingSpecialCases.Count > 0 then
        begin
          rdgSpecialCases.RowCount := ExistingSpecialCases.Count + 1;
        end
        else
        begin
          rdgSpecialCases.RowCount := 2;
          ClearGrid(rdgSpecialCases);
          rdgSpecialCases.Cells[0, 1] := '';
        end;
        rdgSpecialCases.FixedRows := 1;
        for Index := 0 to ExistingSpecialCases.Count - 1 do
        begin
          rdgSpecialCases.Cells[0, Index + 1] := ExistingSpecialCases[Index];
        end;
      finally
        ExistingSpecialCases.Free;
      end;
      rdgSpecialCases.Invalidate;
    finally
      SpecialCases.Free;
      FSpecialChanged := False;
    end;
  end;
end;

procedure TframeMt3dmsChemReactionPkg.AssignDonorsAndAcceptors;
var
  Index: Integer;
  SpeciesIndex: Integer;
  Acceptors: TStringList;
  ExistingAcceptors: TStringList;
begin
  memoDonors.Clear;
  memoAcceptors.Clear;
  for Index := 0 to seElectronDonors.AsInteger - 1 do
  begin
    if Index < FSpeciesNames.Count then
    begin
      memoDonors.Lines.Add(FSpeciesNames[Index]);
    end;
  end;
  for Index := 0 to seElectronAcceptors.AsInteger - 1 do
  begin
    SpeciesIndex := seElectronDonors.AsInteger + Index;
    if SpeciesIndex < FSpeciesNames.Count then
    begin
      memoAcceptors.Lines.Add(FSpeciesNames[SpeciesIndex]);
    end;
  end;

  Acceptors := TStringList.Create;
  ExistingAcceptors := TStringList.Create;
  try
    Acceptors.Assign(memoAcceptors.Lines);
    ExistingAcceptors.Assign(rdgAcceptors.Cols[0]);
    for Index := ExistingAcceptors.Count - 1 downto 1 do
    begin
      if Acceptors.IndexOf(ExistingAcceptors[Index]) < 0 then
      begin
        rdgAcceptors.DeleteRow(Index);
      end;
    end;
    if Acceptors.Count > 0 then
    begin
      rdgAcceptors.RowCount := Acceptors.Count + 1;
    end
    else
    begin
      rdgAcceptors.RowCount := 2;
    end;
    rdgAcceptors.FixedRows := 1;
    for Index := 0 to Acceptors.Count - 1 do
    begin
      rdgAcceptors.Cells[0,Index+1] := Acceptors[Index];
    end;

  finally
    Acceptors.Free;
    ExistingAcceptors.Free;
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
  comboReactionChoice.ItemIndex := Ord(RctPkg.ReactionChoice);
  cbInitialConcChoice.Checked := RctPkg.OtherInitialConcChoice = oicUse;

  rdgYieldCoefficient.Cells[1,0] := StrYieldCoefficients;
  for ItemIndex := 0 to RctPkg.YieldCoefficients.Count - 1 do
  begin
    rdgYieldCoefficient.Cells[1,ItemIndex+1] := RctPkg.YieldCoefficients[ItemIndex];
  end;
  if RctPkg.ElectronDonor < comboElectronDonor.Items.Count then
  begin
    comboElectronDonor.ItemIndex := RctPkg.ElectronDonor;
  end
  else
  begin
    comboElectronDonor.ItemIndex := -1;
  end;
  if RctPkg.ElectronAcceptor < comboElectronAcceptor.Items.Count then
  begin
    comboElectronAcceptor.ItemIndex := RctPkg.ElectronAcceptor;
  end
  else
  begin
    comboElectronAcceptor.ItemIndex := -1;
  end;
  rdeStochiometricRatio.RealValue := RctPkg.StochiometricRatio;
end;

procedure TframeMt3dmsChemReactionPkg.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableReactionItems;
  EnableYieldCoefficients;
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

procedure TframeMt3dmsChemReactionPkg.seElectronAcceptorsChange(
  Sender: TObject);
begin
  inherited;
  AssignDonorsAndAcceptors;
end;

procedure TframeMt3dmsChemReactionPkg.seElectronDonorsChange(Sender: TObject);
begin
  inherited;
  AssignDonorsAndAcceptors;
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
  RctPkg.ReactionChoice := TReactionChoice(comboReactionChoice.ItemIndex);
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

  RctPkg.ElectronDonor := comboElectronDonor.ItemIndex;
  RctPkg.ElectronAcceptor := comboElectronAcceptor.ItemIndex;
  RctPkg.StochiometricRatio := rdeStochiometricRatio.RealValue;

end;

procedure TframeMt3dmsChemReactionPkg.EnableYieldCoefficients;
var
  KineticChoice: TKineticChoice;
begin
  if rcSelectionController.Enabled and (comboKineticChoice.ItemIndex >= 0) then
  begin
    KineticChoice := TKineticChoice(comboKineticChoice.ItemIndex);
    rdgYieldCoefficient.Enabled := KineticChoice = kcFirstOrderChain;
  end
  else
  begin
    rdgYieldCoefficient.Enabled := False;
  end;
end;

procedure TframeMt3dmsChemReactionPkg.EnableReactionItems;
begin
  comboElectronDonor.Enabled := rcSelectionController.Enabled and (comboReactionChoice.ItemIndex = 1);
  comboElectronAcceptor.Enabled := comboElectronDonor.Enabled;
  rdeStochiometricRatio.Enabled := comboElectronDonor.Enabled;
end;

procedure TframeMt3dmsChemReactionPkg.SetSpeciesNames(Names: TStringList);
var
  Index: Integer;
  SpecialCaseNames: TStringList;
  ItemIndex: Integer;
  procedure SetSpecies(Combo: TComboBox);
  var
    SpeciesIndex: Integer;
    Species: string;
  begin
    SpeciesIndex := Combo.ItemIndex;
    Species := Combo.Text;
    Combo.Items := Names;
    if SpeciesIndex >= 0 then
    begin
      if Names.IndexOf(Species) >= 0 then
      begin
        Combo.ItemIndex := Names.IndexOf(Species);
      end
      else if SpeciesIndex < Names.Count then
      begin
        Combo.ItemIndex := SpeciesIndex;
      end
      else
      begin
        Combo.ItemIndex := -1;
      end;
    end
    else
    begin
      Combo.ItemIndex := SpeciesIndex;
    end;
  end;
begin
  // The number of yield coefficients is one less than the number of species.
  FSpeciesNames.Assign(Names);
  FNumberOfSpecies := Names.Count;
  rdgYieldCoefficient.RowCount := Max(Names.Count, 2);
  for Index := 1 to Names.Count - 1 do
  begin
    rdgYieldCoefficient.Cells[0, Index] :=
      Format('%0:s -> %1:s', [Names[Index-1], Names[Index]]);
  end;

  Names.CaseSensitive := False;

  SetSpecies(comboElectronDonor);
  SetSpecies(comboElectronAcceptor);
  AssignDonorsAndAcceptors;

  SpecialCaseNames := TStringList.Create;
  try
    for Index := 0 to clbSpecialCases.Items.Count - 1 do
    begin
      if clbSpecialCases.Checked[Index] then
      begin
        SpecialCaseNames.Add(clbSpecialCases.Items[Index]);
      end;
    end;
    clbSpecialCases.Items := Names;
    for Index := 0 to SpecialCaseNames.Count - 1 do
    begin
      ItemIndex := Names.IndexOf(SpecialCaseNames[Index]);
      if ItemIndex >= 0 then
      begin
        clbSpecialCases.Checked[ItemIndex] := True;
      end;
    end;
  finally
    SpecialCaseNames.Free
  end;

  FSpecialChanged := True;
  UpdateSpecialCaseGrid
end;

end.
