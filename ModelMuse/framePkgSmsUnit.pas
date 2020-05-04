unit framePkgSmsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, Vcl.Mask, JvExMask, JvSpin, Vcl.ComCtrls;

type
  TSmsColumns = (scName, scOverride, scValue);

  TframePkgSms = class(TframePackage)
    pgcControls: TPageControl;
    tabBasic: TTabSheet;
    tabNonLinear: TTabSheet;
    tabLinear: TTabSheet;
    lblPrintOption: TLabel;
    comboPrintOption: TJvImageComboBox;
    lblComplexity: TLabel;
    comboComplexity: TJvImageComboBox;
    lblSolutionGroupMaxIter: TLabel;
    cbContinue: TCheckBox;
    cbCsvOutput: TCheckBox;
    seSolutionGroupMaxIter: TJvSpinEdit;
    rdgNonlinearOptions: TRbwDataGrid4;
    rdgLinearOptions: TRbwDataGrid4;
    comboUsePTC: TJvImageComboBox;
    lblUsePTC: TLabel;
    lblMaxErrors: TLabel;
    seMaxErrors: TJvSpinEdit;
    cbCheckInput: TCheckBox;
    lblMemoryPrint: TLabel;
    comboMemoryPrint: TJvImageComboBox;
    cbNewton: TCheckBox;
    cbUnderRelaxation: TCheckBox;
    procedure rdgNonlinearOptionsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgNonlinearOptionsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgNonlinearOptionsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rdgLinearOptionsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLinearOptionsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLinearOptionsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure cbNewtonClick(Sender: TObject);
  private
    FInitializedGrid: boolean;
    FUnderRelaxPickList: TStringList;
    FLinearSolverPickList: TStringList;
    FRCloseOptionPickList: TStringList;
    FLinLinearAccPickList: TStringList;
    FReorderingPickList: TStringList;
    FScalingMethodPickList: TStringList;
//    FXmdLinearAccPickList: TStringList;
    procedure InitializeGrids;
    function SmsOrdToRow(SmsOrdinal: TSmsOverride): Integer;
    function LinearRowToToSmsOrd(Row: Integer): TSmsOverride;

    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    function LineAccel: TSmsLinLinearAcceleration;
    { Public declarations }
  end;

var
  framePkgSms: TframePkgSms;

implementation

{$R *.dfm}

resourcestring
  StrOption = 'Option';
  StrOverride = 'Override';
  StrValue = 'Value';
  StrOuterHClose = 'Outer HClose';
  StrOuterMaximumIterat = 'Outer maximum iterations';
  StrUnderrelaxationSch = 'Under-relaxation scheme';
  StrUnderRelaxationThe = 'Under relaxation theta';
  StrUnderRelaxationKap = 'Under relaxation kappa';
  StrUnderRelaxationGam = 'Under relaxation gamma';
  StrUnderRelaxationMom = 'Under relaxation momentum';
  StrBacktrackingNumber = 'Backtracking number';
  StrBacktrackingToleran = 'Backtracking tolerance';
  StrBacktrackingReducti = 'Backtracking reduction factor';
  StrBacktrackingResidua = 'Backtracking residual limit';
  StrInnerMaximumIterat = 'Inner maximum iterations';
  StrInnerHClose = 'Inner HClose';
  StrInnerRClose = 'Inner RClose';
  StrLinearAcceleration = 'Linear acceleration';
  StrPreconditionerLevel = 'Preconditioner levels';
  StrNumberOfOrthogonal = 'Number of orthogonalizations';
  StrReorderingMethod = 'Reordering method';
  StrPreconditionerDrop = 'Preconditioner drop tolerance';
  StrRcloseOption = 'Rclose option';
  StrRelaxationFactor = 'Relaxation factor';
  StrScalingMethod = 'Scaling method';
  StrOuterRCloseBND = 'Outer RClose BND';

{ TframePkgSms }

procedure TframePkgSms.cbNewtonClick(Sender: TObject);
begin
  inherited;
  rcSelectionControllerEnabledChange(nil);
end;

constructor TframePkgSms.Create(AOwner: TComponent);
begin
  inherited;
  FUnderRelaxPickList := TStringList.Create;
  FUnderRelaxPickList.Add('None');
  FUnderRelaxPickList.Add('Simple');
  FUnderRelaxPickList.Add('DBD');
  FUnderRelaxPickList.Add('Cooley');

  FLinearSolverPickList := TStringList.Create;
  FLinearSolverPickList.Add('Linear');
  FLinearSolverPickList.Add('XMD');

  FRCloseOptionPickList := TStringList.Create;
  FRCloseOptionPickList.Add('Absolute');
  FRCloseOptionPickList.Add('Strict');
  FRCloseOptionPickList.Add('L2Norm');
  FRCloseOptionPickList.Add('Relative');

  FLinLinearAccPickList := TStringList.Create;
  FLinLinearAccPickList.Add('CG (Conjugate Gradient)');
  FLinLinearAccPickList.Add('BICGSTAB');

  FReorderingPickList := TStringList.Create;
  FReorderingPickList.Add('None');
  FReorderingPickList.Add('reverse Cuthill McKee');
  FReorderingPickList.Add('minimum degree');

  FScalingMethodPickList := TStringList.Create;
  FScalingMethodPickList.Add('None');
  FScalingMethodPickList.Add('Diagonal');
  FScalingMethodPickList.Add('L2Norm');

//  FXmdLinearAccPickList := TStringList.Create;
//  FXmdLinearAccPickList.Add('CG');
//  FXmdLinearAccPickList.Add('ORTHOMIN');
//  FXmdLinearAccPickList.Add('BICGSTAB');
end;

destructor TframePkgSms.Destroy;
begin
//  FXmdLinearAccPickList.Free;
  FScalingMethodPickList.Free;
  FReorderingPickList.Free;
  FLinLinearAccPickList.Free;
  FRCloseOptionPickList.Free;
  FLinearSolverPickList.Free;
  FUnderRelaxPickList.Free;
  inherited;
end;

procedure TframePkgSms.GetData(Package: TModflowPackageSelection);
var
  SmsPackage: TSmsPackageSelection;
  SmsOveride: TSmsOverride;
begin
  inherited;
  pgcControls.ActivePageIndex := 0;
  rdgNonlinearOptions.BeginUpdate;
  try
    if not FInitializedGrid then
    begin
      InitializeGrids;
      FInitializedGrid := True;
    end;

    SmsPackage := Package as TSmsPackageSelection;

    seSolutionGroupMaxIter.AsInteger := SmsPackage.SolutionGroupMaxIteration;
    comboPrintOption.ItemIndex := Ord(SmsPackage.Print);
    comboComplexity.ItemIndex := Ord(SmsPackage.Complexity);
    cbContinue.Checked := SmsPackage.ContinueModel;
    cbCsvOutput.Checked := Boolean(Ord(SmsPackage.CsvOutput));
    comboUsePTC.ItemIndex := Ord(SmsPackage.UsePTC);
    seMaxErrors.AsInteger := SmsPackage.MaxErrors;
    cbCheckInput.Checked := SmsPackage.CheckInput = ciCheckAll;
    comboMemoryPrint.ItemIndex := Ord(SmsPackage.MemoryPrint);

    cbNewton.Checked := SmsPackage.NewtonMF6;
    cbUnderRelaxation.Checked := SmsPackage.UnderRelaxationMF6;
    rcSelectionControllerEnabledChange(nil);

    for SmsOveride := Low(TSmsOverride) to High(TSmsOverride) do
    begin
      if SmsOveride in [soLinearSolver, soXmdLinearAcceleration, soRedBlackOrder] then
      begin
        Continue;
      end;
      if SmsOveride < soInnerMaxIterations then
      begin
        rdgNonlinearOptions.Checked[Ord(scOverride), Ord(SmsOveride)+1] :=
          SmsOveride in SmsPackage.SmsOverrides;
      end
      else
      begin
        rdgLinearOptions.Checked[Ord(scOverride), SmsOrdToRow(SmsOveride)] :=
          SmsOveride in SmsPackage.SmsOverrides;
      end;
    end;

    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soOuterHclose)+1] :=
      SmsPackage.OuterHclose;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soOuterRClose)+1] :=
      SmsPackage.OuterRClose;
    rdgNonlinearOptions.IntegerValue[Ord(scValue), Ord(soOuterMaxIt)+1] :=
      SmsPackage.MaxOuterIterations;
    rdgNonlinearOptions.Cells[Ord(scValue), Ord(soUnderRelax)+1] :=
      FUnderRelaxPickList[Ord(SmsPackage.UnderRelaxation)];
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soUnderRelaxTheta)+1] :=
      SmsPackage.UnderRelaxTheta;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soUnderRelaxKappa)+1] :=
      SmsPackage.UnderRelaxKappa;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soUnderRelaxGamma)+1] :=
      SmsPackage.UnderRelaxGamma;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soUnderRelaxMomentum)+1] :=
      SmsPackage.UnderRelaxMomentum;
    rdgNonlinearOptions.IntegerValue[Ord(scValue), Ord(soBacktrackingNumber)+1] :=
      SmsPackage.BacktrackingNumber;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soBacktrackingTolerance)+1] :=
      SmsPackage.BacktrackingTolerance;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soBacktrackingReductionFactor)+1] :=
      SmsPackage.BacktrackingReductionFactor;
    rdgNonlinearOptions.RealValue[Ord(scValue), Ord(soBacktrackingResidualLimit)+1] :=
      SmsPackage.BacktrackingResidualLimit;
  finally
    rdgNonlinearOptions.EndUpdate;
  end;

  rdgLinearOptions.BeginUpdate;
  try
//    rdgOptions.Cells[Ord(scValue), Ord(soLinearSolver)+1] :=
//      FLinearSolverPickList[Ord(SmsPackage.LinearSolver)];
    rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soInnerMaxIterations)] :=
      SmsPackage.InnerMaxIterations;
    rdgLinearOptions.RealValue[Ord(scValue), SmsOrdToRow(soInnerHclose)] :=
      SmsPackage.InnerHclose;
    rdgLinearOptions.RealValue[Ord(scValue), SmsOrdToRow(soInnerRclose)] :=
      SmsPackage.InnerRclose;
    rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soLinLinearAcceleration)] :=
      FLinLinearAccPickList[Ord(SmsPackage.LinLinearAcceleration)];
    rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soPreconditionerLevel)] :=
      SmsPackage.PreconditionerLevel;
    rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soNumberOfOrthoganalizations)] :=
      SmsPackage.NumberOfOrthoganalizations;
    rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soReorderingMethod)] :=
      FReorderingPickList[Ord(SmsPackage.ReorderingMethod)];
    rdgLinearOptions.RealValue[Ord(scValue), SmsOrdToRow(soPreconditionerDropTolerance)] :=
      SmsPackage.PreconditionerDropTolerance;
    rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soRcloseOption)] :=
      FRCloseOptionPickList[Ord(SmsPackage.RcloseOption)];
    rdgLinearOptions.RealValue[Ord(scValue), SmsOrdToRow(soRelaxationFactor)] :=
      SmsPackage.RelaxationFactor;
    rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soScalingMethod)] :=
      FScalingMethodPickList[Ord(SmsPackage.ScalingMethod)];
//    rdgOptions.Cells[Ord(scValue), Ord(soXmdLinearAcceleration)+1] :=
//      FXmdLinearAccPickList[Ord(SmsPackage.XmdLinearAcceleration)];
//    rdgOptions.Checked[Ord(scValue), Ord(soRedBlackOrder)+1] :=
//      SmsPackage.RedBlackOrder;
  finally
    rdgLinearOptions.EndUpdate;
  end;

end;

procedure TframePkgSms.InitializeGrids;
begin
  rdgNonlinearOptions.RowCount := Ord(soBacktrackingResidualLimit) + 2;
  rdgNonlinearOptions.BeginUpdate;
  try
    rdgNonlinearOptions.FixedCols := 1;
    rdgNonlinearOptions.Columns[Ord(scValue)].PickList := FReorderingPickList;

    rdgNonlinearOptions.Cells[Ord(scName), 0] := StrOption;
    rdgNonlinearOptions.Cells[Ord(scOverride), 0] := StrOverride;
    rdgNonlinearOptions.Cells[Ord(scValue), 0] := StrValue;

    rdgNonlinearOptions.Cells[Ord(scName), Ord(soOuterHclose)+1] := StrOuterHClose;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soOuterRClose)+1] := StrOuterRCloseBND;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soOuterMaxIt)+1] := StrOuterMaximumIterat;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soUnderRelax)+1] := StrUnderrelaxationSch;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soUnderRelaxTheta)+1] := StrUnderRelaxationThe;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soUnderRelaxKappa)+1] := StrUnderRelaxationKap;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soUnderRelaxGamma)+1] := StrUnderRelaxationGam;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soUnderRelaxMomentum)+1] := StrUnderRelaxationMom;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soBacktrackingNumber)+1] := StrBacktrackingNumber;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soBacktrackingTolerance)+1] := StrBacktrackingToleran;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soBacktrackingReductionFactor)+1] := StrBacktrackingReducti;
    rdgNonlinearOptions.Cells[Ord(scName), Ord(soBacktrackingResidualLimit)+1] := StrBacktrackingResidua;
//    rdgOptions.Cells[Ord(scName), Ord(soLinearSolver)+1] := 'Linear solver';

    rdgNonlinearOptions.SpecialFormat[Ord(scValue), Ord(soOuterMaxIt)+1] := rcf4Integer;
    rdgNonlinearOptions.UseSpecialFormat[Ord(scValue), Ord(soOuterMaxIt)+1] := True;

    rdgNonlinearOptions.SpecialFormat[Ord(scValue), Ord(soUnderRelax)+1] := rcf4String;
    rdgNonlinearOptions.UseSpecialFormat[Ord(scValue), Ord(soUnderRelax)+1] := True;

    rdgNonlinearOptions.SpecialFormat[Ord(scValue), Ord(soBacktrackingNumber)+1] := rcf4Integer;
    rdgNonlinearOptions.UseSpecialFormat[Ord(scValue), Ord(soBacktrackingNumber)+1] := True;

  finally
    rdgNonlinearOptions.EndUpdate;
  end;

  rdgLinearOptions.RowCount := SmsOrdToRow(soReorderingMethod) + 1;
  rdgLinearOptions.BeginUpdate;
  try
    rdgLinearOptions.FixedCols := 1;
    rdgLinearOptions.Columns[Ord(scValue)].PickList := FReorderingPickList;

    rdgLinearOptions.Cells[Ord(scName), 0] := StrOption;
    rdgLinearOptions.Cells[Ord(scOverride), 0] := StrOverride;
    rdgLinearOptions.Cells[Ord(scValue), 0] := StrValue;

    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soInnerMaxIterations)] := StrInnerMaximumIterat;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soInnerHclose)] := StrInnerHClose;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soInnerRclose)] := StrInnerRClose;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soLinLinearAcceleration)] := StrLinearAcceleration;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soPreconditionerLevel)] := StrPreconditionerLevel;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soNumberOfOrthoganalizations)] := StrNumberOfOrthogonal;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soReorderingMethod)] := StrReorderingMethod;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soPreconditionerDropTolerance)] := StrPreconditionerDrop;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soRcloseOption)] := StrRcloseOption;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soRelaxationFactor)] := StrRelaxationFactor;
    rdgLinearOptions.Cells[Ord(scName), SmsOrdToRow(soScalingMethod)] := StrScalingMethod;
//    rdgOptions.Cells[Ord(scName), Ord(soXmdLinearAcceleration)+1] := 'Linear acceleration (XMD block)';
//    rdgOptions.Cells[Ord(scName), Ord(soRedBlackOrder)+1] := 'Use red-black ordering scheme';



//    rdgOptions.SpecialFormat[Ord(scValue), Ord(soLinearSolver)+1] := rcf4String;
//    rdgOptions.UseSpecialFormat[Ord(scValue), Ord(soLinearSolver)+1] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soInnerMaxIterations)] := rcf4Integer;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soInnerMaxIterations)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soLinLinearAcceleration)] := rcf4String;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soLinLinearAcceleration)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soPreconditionerLevel)] := rcf4Integer;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soPreconditionerLevel)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soNumberOfOrthoganalizations)] := rcf4Integer;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soNumberOfOrthoganalizations)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soReorderingMethod)] := rcf4String;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soReorderingMethod)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soRcloseOption)] := rcf4String;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soRcloseOption)] := True;

    rdgLinearOptions.SpecialFormat[Ord(scValue), SmsOrdToRow(soScalingMethod)] := rcf4String;
    rdgLinearOptions.UseSpecialFormat[Ord(scValue), SmsOrdToRow(soScalingMethod)] := True;
  finally
    rdgLinearOptions.EndUpdate;
  end;

end;

function TframePkgSms.LineAccel: TSmsLinLinearAcceleration;
var
  ARow: Integer;
begin
  ARow := SmsOrdToRow(soLinLinearAcceleration);
  if rdgLinearOptions.Checked[Ord(scOverride), ARow] then
  begin
    result := TSmsLinLinearAcceleration(FLinLinearAccPickList.IndexOf(rdgLinearOptions.Cells[Ord(scValue), ARow]));
  end
  else
  begin
    result := sllaCg;
  end;
end;

function TframePkgSms.LinearRowToToSmsOrd(Row: Integer): TSmsOverride;
begin
  Result := TSmsOverride(Row - 1 + Ord(soInnerMaxIterations));
  Assert(Result in [soInnerMaxIterations..soReorderingMethod]);
end;

function TframePkgSms.SmsOrdToRow(SmsOrdinal: TSmsOverride): Integer;
begin
  result := Ord(SmsOrdinal) - Ord(soInnerMaxIterations) + 1;
  Assert(result >= 1);
end;

procedure TframePkgSms.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  cbUnderRelaxation.Enabled := rcSelectionController.Enabled and cbNewton.Checked;
end;

procedure TframePkgSms.rdgLinearOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  SmsOverride: TSmsOverride;
  AColumn: TRbwColumn4;
  SmsColumn: TSmsColumns;
  function Complexity: TSmsComplexityOption;
  begin
    result := TSmsComplexityOption(comboComplexity.ItemIndex);
  end;
begin
  inherited;

  if (ARow >= 1) and (ACol >= 1) then
  begin
    SmsOverride := LinearRowToToSmsOrd(ARow);
    SmsColumn := TSmsColumns(ACol);
    case SmsColumn of
      scOverride:
        begin
          case SmsOverride of
            soInnerMaxIterations, soInnerHclose:
              begin
                CanSelect := True;
              end;
            soInnerRclose:
              begin
                CanSelect := True;
              end;
            soRcloseOption:
              begin
                // RcloseOption is only used with RClose in the default
                // linear block
                CanSelect := True; {(Solver = slsDefault)
//                  and rdgOptions.Checked[Ord(scOverride), Ord(soInnerRclose)+1];}
              end;
            soLinLinearAcceleration:
              begin
                CanSelect := True
              end;
            soPreconditionerLevel, soNumberOfOrthoganalizations:
              begin
                // When the Complexity is specified,
                // PRECONDITIONER_LEVELS and NUMBER_ORTHOGONALIZATIONS
                // are required in the XMD solver but is optional in the
                // default linear solver.
                CanSelect := True;// (Solver <> slsXMD);
              end;
            soReorderingMethod:
              begin
                // REORDERING_METHOD is optional with either solver.
                CanSelect := True;
              end;
            soPreconditionerDropTolerance:
              begin
                // When the Complexity is specified,
                // PRECONDITIONER_DROP_TOLERANCE
                // is required in the XMD solver but is optional in the
                // default linear solver.
                CanSelect := True; //(Solver <> slsXMD);
              end;
            soRelaxationFactor, soScalingMethod:
              begin
                // RELAXATION_FACTOR and [SCALING_METHOD are only used with
                // the default linear solver.
                CanSelect := True;// Solver = slsDefault
              end;
//            soXmdLinearAcceleration:
//              begin
//                // The user must specify LINEAR_ACCELERATION
//                // for the XMD solver
//                // if the Complexity is specified
//                // LINEAR_ACCELERATION is specified by a different variable
//                // for the linear solver.
//                CanSelect := True; // (Solver = slsXMD);
//              end;
//            soRedBlackOrder:
//              begin
//                // RED_BLACK_ORDERING is only used with the XMD solver.
//                CanSelect := True; //(Solver = slsXMD)
//              end;
            else
              Assert(False);
          end;
        end;
      scValue:
        begin

          if CanSelect then
          begin
            CanSelect := rdgLinearOptions.Checked[ACol-1, ARow];
          end;
        end;
      else Assert(False);
    end;

    if (SmsColumn = scValue) and not rdgLinearOptions.Drawing  then
    begin
      AColumn := rdgLinearOptions.Columns[ACol];
      case SmsOverride of
{
        soOuterHclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soOuterMaxIt:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelax:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FUnderRelaxPickList;
          end;
        soUnderRelaxTheta:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxKappa:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxGamma:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxMomentum:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingNumber:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingTolerance:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingReductionFactor:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingResidualLimit:
          begin
            AColumn.ComboUsed := False;
          end;
//        soLinearSolver:
//          begin
//            AColumn.ComboUsed := True;
//            AColumn.PickList := FLinearSolverPickList;
//          end;
}
        soInnerMaxIterations:
          begin
            AColumn.ComboUsed := False;
          end;
        soInnerHclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soInnerRclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soRcloseOption:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FRCloseOptionPickList;
          end;
        soLinLinearAcceleration:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FLinLinearAccPickList;
          end;
        soPreconditionerLevel:
          begin
            AColumn.ComboUsed := False;
          end;
        soNumberOfOrthoganalizations:
          begin
            AColumn.ComboUsed := False;
          end;
        soReorderingMethod:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FReorderingPickList;
          end;
        soPreconditionerDropTolerance:
          begin
            AColumn.ComboUsed := False;
          end;
        soRelaxationFactor:
          begin
            AColumn.ComboUsed := False;
          end;
        soScalingMethod:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FScalingMethodPickList;
          end;
//        soXmdLinearAcceleration:
//          begin
//            AColumn.ComboUsed := True;
//            AColumn.PickList := FXmdLinearAccPickList;
//          end;
//        soRedBlackOrder:
//          begin
//            AColumn.ComboUsed := False;
//          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TframePkgSms.rdgLinearOptionsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  FloatValue: Extended;
  SmsOverride: TSmsOverride;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(scValue)) then
  begin
    SmsOverride := LinearRowToToSmsOrd(ARow);
    if SmsOverride in [{soUnderRelaxTheta, soUnderRelaxKappa, soUnderRelaxGamma,
        soUnderRelaxMomentum, soBacktrackingTolerance,
        soBacktrackingReductionFactor,} soRelaxationFactor] then
    begin
      if TryStrToFloat(Value, FloatValue) then
      begin
        if FloatValue > 1 then
        begin
          Beep;
          rdgLinearOptions.Cells[ACol,ARow] := '1';
        end;
      end;
    end;
//    if SmsOverride = soLinearSolver then
//    begin
//      comboComplexityChange(Sender);
//    end;
    rdgLinearOptions.Invalidate;
  end
end;

procedure TframePkgSms.rdgLinearOptionsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  rdgLinearOptions.Invalidate;
end;

procedure TframePkgSms.rdgNonlinearOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  SmsOverride: TSmsOverride;
  AColumn: TRbwColumn4;
  SmsColumn: TSmsColumns;
  function Complexity: TSmsComplexityOption;
  begin
    result := TSmsComplexityOption(comboComplexity.ItemIndex);
  end;
  function BackTrackingNumber: integer;
  begin
    if rdgNonlinearOptions.Checked[Ord(scOverride), Ord(soBacktrackingNumber)+1] then
    begin
      result := rdgNonlinearOptions.IntegerValueDefault[Ord(scValue), Ord(soBacktrackingNumber)+1, 0];
    end
    else
    begin
      result := 10;
    end;
  end;
  function UnderRelaxation: TSmsUnderRelaxation;
  begin
    if rdgNonlinearOptions.Checked[Ord(scOverride), Ord(soUnderRelax)+1] then
    begin
      result := TSmsUnderRelaxation(FUnderRelaxPickList.IndexOf(
        rdgNonlinearOptions.Cells[Ord(scValue), Ord(soUnderRelax)+1]));
    end
    else
    begin
      result := surNone;
    end;
  end;
begin
  inherited;

  if (ARow >= 1) and (ACol >= 1) then
  begin
    SmsOverride := TSmsOverride(ARow-1);
    SmsColumn := TSmsColumns(ACol);
    case SmsColumn of
      scOverride:
        begin
          case SmsOverride of
            soOuterHclose, soOuterRClose, soOuterMaxIt, soUnderRelax:
              begin
                CanSelect := True;
              end;
            soUnderRelaxTheta:
              begin
                CanSelect := (UnderRelaxation = surDbd);
              end;
            soUnderRelaxKappa:
              begin
                CanSelect := (UnderRelaxation = surDbd);
              end;
            soUnderRelaxGamma:
              begin
                CanSelect := (UnderRelaxation <> surNone);
              end;
            soUnderRelaxMomentum:
              begin
                CanSelect := (UnderRelaxation = surDbd);
              end;
            soBacktrackingNumber:
              begin
                CanSelect := True;
              end;
            soBacktrackingTolerance:
              begin
                CanSelect := BackTrackingNumber > 0;
              end;
            soBacktrackingReductionFactor:
              begin
                CanSelect := BackTrackingNumber > 0;
              end;
            soBacktrackingResidualLimit:
              begin
                CanSelect := BackTrackingNumber > 0;
              end;
            soInnerMaxIterations, soInnerHclose:
              begin
                CanSelect := True;
              end;
            soInnerRclose:
              begin
                CanSelect := True;
              end;
            soRcloseOption:
              begin
                // RcloseOption is only used with RClose in the default
                // linear block
                CanSelect := True; {(Solver = slsDefault)
//                  and rdgOptions.Checked[Ord(scOverride), Ord(soInnerRclose)+1];}
              end;
            soLinLinearAcceleration:
              begin
                CanSelect := True
              end;
            soPreconditionerLevel, soNumberOfOrthoganalizations:
              begin
                // When the Complexity is specified,
                // PRECONDITIONER_LEVELS and NUMBER_ORTHOGONALIZATIONS
                // are required in the XMD solver but is optional in the
                // default linear solver.
                CanSelect := True;// (Solver <> slsXMD);
              end;
            soReorderingMethod:
              begin
                // REORDERING_METHOD is optional with either solver.
                CanSelect := True;
              end;
            soPreconditionerDropTolerance:
              begin
                // When the Complexity is specified,
                // PRECONDITIONER_DROP_TOLERANCE
                // is required in the XMD solver but is optional in the
                // default linear solver.
                CanSelect := True; //(Solver <> slsXMD);
              end;
            soRelaxationFactor, soScalingMethod:
              begin
                // RELAXATION_FACTOR and [SCALING_METHOD are only used with
                // the default linear solver.
                CanSelect := True;// Solver = slsDefault
              end;
            else
              Assert(False);
          end;
        end;
      scValue:
        begin
          rdgNonlinearOptionsSelectCell(Sender, Ord(scOverride), ARow, CanSelect);

          if CanSelect then
          begin
            CanSelect := rdgNonlinearOptions.Checked[ACol-1, ARow];
          end;
        end;
      else Assert(False);
    end;

    if (SmsColumn = scValue) and not rdgNonlinearOptions.Drawing  then
    begin
      AColumn := rdgNonlinearOptions.Columns[ACol];
      case SmsOverride of
        soOuterHclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soOuterRClose:
          begin
            AColumn.ComboUsed := False;
          end;
        soOuterMaxIt:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelax:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FUnderRelaxPickList;
          end;
        soUnderRelaxTheta:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxKappa:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxGamma:
          begin
            AColumn.ComboUsed := False;
          end;
        soUnderRelaxMomentum:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingNumber:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingTolerance:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingReductionFactor:
          begin
            AColumn.ComboUsed := False;
          end;
        soBacktrackingResidualLimit:
          begin
            AColumn.ComboUsed := False;
          end;
        soInnerMaxIterations:
          begin
            AColumn.ComboUsed := False;
          end;
        soInnerHclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soInnerRclose:
          begin
            AColumn.ComboUsed := False;
          end;
        soRcloseOption:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FRCloseOptionPickList;
          end;
        soLinLinearAcceleration:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FLinLinearAccPickList;
          end;
        soPreconditionerLevel:
          begin
            AColumn.ComboUsed := False;
          end;
        soNumberOfOrthoganalizations:
          begin
            AColumn.ComboUsed := False;
          end;
        soReorderingMethod:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FReorderingPickList;
          end;
        soPreconditionerDropTolerance:
          begin
            AColumn.ComboUsed := False;
          end;
        soRelaxationFactor:
          begin
            AColumn.ComboUsed := False;
          end;
        soScalingMethod:
          begin
            AColumn.ComboUsed := True;
            AColumn.PickList := FScalingMethodPickList;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TframePkgSms.rdgNonlinearOptionsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  FloatValue: Extended;
  SmsOverride: TSmsOverride;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(scValue)) then
  begin
    SmsOverride := TSmsOverride(ARow - 1);
    if SmsOverride in [soUnderRelaxTheta, soUnderRelaxKappa, soUnderRelaxGamma,
        soUnderRelaxMomentum,
        soBacktrackingReductionFactor, soRelaxationFactor] then
    begin
      if TryStrToFloat(Value, FloatValue) then
      begin
        if FloatValue > 1 then
        begin
          Beep;
          rdgNonlinearOptions.Cells[ACol,ARow] := '1';
        end;
      end;
    end;
    if SmsOverride = soBacktrackingTolerance then
    begin
      if TryStrToFloat(Value, FloatValue) then
      begin
        if FloatValue < 1 then
        begin
          Beep;
          rdgNonlinearOptions.Cells[ACol,ARow] := '1';
        end;
      end;
    end;

//    if SmsOverride = soLinearSolver then
//    begin
//      comboComplexityChange(Sender);
//    end;
    rdgNonlinearOptions.Invalidate;
  end
end;

procedure TframePkgSms.rdgNonlinearOptionsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  rdgNonlinearOptions.Invalidate;
end;

procedure TframePkgSms.SetData(Package: TModflowPackageSelection);
var
  SmsPackage: TSmsPackageSelection;
  NewOverRides: TSmsOverrides;
  SmsOveride: TSmsOverride;
begin
  inherited;
  SmsPackage := Package as TSmsPackageSelection;

  SmsPackage.SolutionGroupMaxIteration := seSolutionGroupMaxIter.AsInteger;
  SmsPackage.Print := TSmsPrint(comboPrintOption.ItemIndex);
  SmsPackage.Complexity := TSmsComplexityOption(comboComplexity.ItemIndex);
  SmsPackage.ContinueModel := cbContinue.Checked;
  SmsPackage.CsvOutput := TSmsSolutionPrint(Ord(cbCsvOutput.Checked));
  SmsPackage.UsePTC := TUsePTC(comboUsePTC.ItemIndex);
  SmsPackage.MaxErrors := seMaxErrors.AsInteger;
  SmsPackage.CheckInput := TCheckInput(not cbCheckInput.Checked);
  SmsPackage.MemoryPrint := TMemoryPrint(comboMemoryPrint.ItemIndex);

  SmsPackage.NewtonMF6 := cbNewton.Checked;
  SmsPackage.UnderRelaxationMF6 := cbUnderRelaxation.Checked;

  NewOverRides := [];
  for SmsOveride := Low(TSmsOverride) to High(TSmsOverride) do
  begin
    if SmsOveride in [soLinearSolver, soXmdLinearAcceleration, soRedBlackOrder] then
    begin
      Continue;
    end;
    if SmsOveride < soInnerMaxIterations then
    begin
      if rdgNonlinearOptions.Checked[Ord(scOverride), Ord(SmsOveride)+1] then
      begin
        Include(NewOverRides, SmsOveride);
      end;
    end
    else
    begin
      if rdgLinearOptions.Checked[Ord(scOverride), SmsOrdToRow(SmsOveride)] then
      begin
        Include(NewOverRides, SmsOveride);
      end;
    end;
  end;
  SmsPackage.SmsOverrides := NewOverRides;

  SmsPackage.OuterHclose := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soOuterHclose)+1, SmsPackage.OuterHclose];
  SmsPackage.OuterRClose := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soOuterRClose)+1, SmsPackage.OuterRClose];
  SmsPackage.MaxOuterIterations := rdgNonlinearOptions.IntegerValue[Ord(scValue), Ord(soOuterMaxIt)+1];
  SmsPackage.UnderRelaxation := TSmsUnderRelaxation(FUnderRelaxPickList.IndexOf(rdgNonlinearOptions.Cells[Ord(scValue), Ord(soUnderRelax)+1]));

  SmsPackage.UnderRelaxTheta := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soUnderRelaxTheta)+1, SmsPackage.UnderRelaxTheta ];
  SmsPackage.UnderRelaxKappa := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soUnderRelaxKappa)+1, SmsPackage.UnderRelaxKappa];
  SmsPackage.UnderRelaxGamma := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soUnderRelaxGamma)+1, SmsPackage.UnderRelaxGamma];
  SmsPackage.UnderRelaxMomentum := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soUnderRelaxMomentum)+1, SmsPackage.UnderRelaxMomentum];
  SmsPackage.BacktrackingNumber := rdgNonlinearOptions.IntegerValue[Ord(scValue), Ord(soBacktrackingNumber)+1];
  SmsPackage.BacktrackingTolerance := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soBacktrackingTolerance)+1, SmsPackage.BacktrackingTolerance];
  SmsPackage.BacktrackingReductionFactor := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soBacktrackingReductionFactor)+1, SmsPackage.BacktrackingReductionFactor];
  SmsPackage.BacktrackingResidualLimit := rdgNonlinearOptions.RealValueDefault[Ord(scValue), Ord(soBacktrackingResidualLimit)+1, SmsPackage.BacktrackingResidualLimit];


  //  SmsPackage.LinearSolver := TSmsLinearSolver(FLinearSolverPickList.IndexOf(rdgOptions.Cells[Ord(scValue), Ord(soLinearSolver)+1]));
  SmsPackage.InnerMaxIterations := rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soInnerMaxIterations)];
  SmsPackage.InnerHclose := rdgLinearOptions.RealValueDefault[Ord(scValue), SmsOrdToRow(soInnerHclose), SmsPackage.InnerHclose];
  SmsPackage.InnerRclose := rdgLinearOptions.RealValueDefault[Ord(scValue), SmsOrdToRow(soInnerRclose), SmsPackage.InnerRclose];
  SmsPackage.LinLinearAcceleration := TSmsLinLinearAcceleration(FLinLinearAccPickList.IndexOf(rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soLinLinearAcceleration)]));
  SmsPackage.PreconditionerLevel := rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soPreconditionerLevel)];
  SmsPackage.NumberOfOrthoganalizations := rdgLinearOptions.IntegerValue[Ord(scValue), SmsOrdToRow(soNumberOfOrthoganalizations)];
  SmsPackage.ReorderingMethod := TSmsReorderingMethod(FReorderingPickList.IndexOf(rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soReorderingMethod)]));
  SmsPackage.PreconditionerDropTolerance := rdgLinearOptions.RealValueDefault[Ord(scValue), SmsOrdToRow(soPreconditionerDropTolerance), SmsPackage.PreconditionerDropTolerance];
  SmsPackage.RcloseOption := TSmsRcloseOption(FRCloseOptionPickList.IndexOf(rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soRcloseOption)]));
  SmsPackage.RelaxationFactor := rdgLinearOptions.RealValueDefault[Ord(scValue), SmsOrdToRow(soRelaxationFactor), SmsPackage.RelaxationFactor];
  SmsPackage.ScalingMethod := TSmsScalingMethod(FScalingMethodPickList.IndexOf(rdgLinearOptions.Cells[Ord(scValue), SmsOrdToRow(soScalingMethod)]));
//  SmsPackage.XmdLinearAcceleration := TSmsXmdLinearAcceleration(FXmdLinearAccPickList.IndexOf(rdgOptions.Cells[Ord(scValue), Ord(soXmdLinearAcceleration)+1]));
//  SmsPackage.RedBlackOrder := rdgOptions.Checked[Ord(scValue), Ord(soRedBlackOrder)+1];

end;

end.
