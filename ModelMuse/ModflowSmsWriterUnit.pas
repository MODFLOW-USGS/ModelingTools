unit ModflowSmsWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TImsWriter = class(TCustomSolverWriter)
  private
    FNameOfFile: string;
    procedure WriteInnerMaximum;
    procedure WriteInnerHClose;
    procedure WritePreconditionerLevels;
    procedure WritePreconditionerDropTolerances;
    procedure WriteNumberOfOrthogonalizations;
    procedure WriteReorderingMethod;
  protected
    FImsPackage: TSmsPackageSelection;
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    procedure WriteOptions;
    procedure WriteNonLinearBlock;
//    function LinearSolver: TSmsLinearSolver;
    procedure WriteLinearBlock;
//    procedure WriteXmdBlock;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, PhastModelUnit, frmProgressUnit;

{ TSmsWriter }

class function TImsWriter.Extension: string;
begin
  result := '.ims';
end;

//function TImsWriter.LinearSolver: TSmsLinearSolver;
//begin
//  result := slsDefault;
//  if soLinearSolver in FSmsPackage.SmsOverrides then
//  begin
//    result := FSmsPackage.LinearSolver;
//  end;
//end;

function TImsWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SmsPackage;
end;

procedure TImsWriter.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  // write to simulation name file
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing IMS Package input');
    WriteDataSet0;
    FImsPackage := Model.ModflowPackages.SmsPackage;
    WriteOptions;
    WriteNonLinearBlock;
//    case LinearSolver of
//      slsDefault:
        WriteLinearBlock;
//      slsXMD:
//        WriteXmdBlock;
//      else Assert(False);
//    end;
  finally
    CloseFile;
  end;

end;

procedure TImsWriter.WriteReorderingMethod;
begin
  if soReorderingMethod in FImsPackage.SmsOverrides then
  begin
    WriteString('  REORDERING_METHOD ');
    case FImsPackage.ReorderingMethod of
      srmNone:
        WriteString('NONE');
      srmReverseCuthillMcKee:
        WriteString('RKM');
      srmMinimumDegreeOrdering:
        WriteString('MD');
    else
      Assert(False);
    end;
    NewLine;
  end;
end;

procedure TImsWriter.WriteNumberOfOrthogonalizations;
begin
  if soNumberOfOrthoganalizations in FImsPackage.SmsOverrides then
  begin
    WriteString('  NUMBER_ORTHOGONALIZATIONS');
    WriteInteger(FImsPackage.NumberOfOrthoganalizations);
    NewLine;
  end;
end;

procedure TImsWriter.WritePreconditionerDropTolerances;
begin
  if soPreconditionerDropTolerance in FImsPackage.SmsOverrides then
  begin
    WriteString('  PRECONDITIONER_DROP_TOLERANCE');
    WriteFloat(FImsPackage.PreconditionerDropTolerance);
    NewLine;
  end;
end;

procedure TImsWriter.WritePreconditionerLevels;
begin
  if soPreconditionerLevel in FImsPackage.SmsOverrides then
  begin
    WriteString('  PRECONDITIONER_LEVELS');
    WriteInteger(FImsPackage.PreconditionerLevel);
    NewLine;
  end;
end;

procedure TImsWriter.WriteInnerHClose;
begin
  WriteString('  INNER_HCLOSE');
  if soInnerHclose in FImsPackage.SmsOverrides then
  begin
    WriteFloat(FImsPackage.InnerHclose);
  end
  else
  begin
    WriteFloat(0.0001);
  end;
  NewLine;
end;

procedure TImsWriter.WriteInnerMaximum;
begin
  WriteString('  INNER_MAXIMUM');
  if soInnerMaxIterations in FImsPackage.SmsOverrides then
  begin
    WriteInteger(FImsPackage.InnerMaxIterations);
  end
  else
  begin
    WriteInteger(100);
  end;
  NewLine;
end;

procedure TImsWriter.WriteLinearBlock;
//var
//  UseNonLinear: Boolean;
begin
//  UseNonLinear := [soInnerMaxIterations, soInnerHclose, soInnerRclose,
//    soLinLinearAcceleration, soRelaxationFactor, soPreconditionerLevel,
//    soPreconditionerDropTolerance, soNumberOfOrthoganalizations,
//    soScalingMethod, soReorderingMethod] * FSmsPackage.SmsOverrides <> [];
//  if not UseNonLinear then
//  begin
//    Exit;
//  end;

  WriteString('BEGIN LINEAR');
  NewLine;

  WriteInnerMaximum;
  WriteInnerHClose;

  WriteString('  INNER_RCLOSE');
  if soInnerRclose in FImsPackage.SmsOverrides then
  begin
    WriteFloat(FImsPackage.InnerRclose);

    if soRcloseOption in FImsPackage.SmsOverrides then
    begin
      case FImsPackage.RcloseOption of
        sroAbsolute: {do nothing};
        sroStrict: WriteString(' STRICT');
        sroL2Norm: WriteString(' L2NORM_RCLOSE');
        sroRelative: WriteString(' RELATIVE_RCLOSE');
        else Assert(False);
      end;
    end;
  end
  else
  begin
    WriteFloat(0.1);
  end;
  NewLine;

  WriteString('  LINEAR_ACCELERATION ');
  if soLinLinearAcceleration in FImsPackage.SmsOverrides then
  begin
    case FImsPackage.LinLinearAcceleration of
      sllaCg: WriteString('CG');
      sllaBiCgStab: WriteString('BICGSTAB');
      else Assert(False);
    end;
  end
  else
  begin
    WriteString('CG');
  end;
  NewLine;

  if soRelaxationFactor in FImsPackage.SmsOverrides then
  begin
    WriteString('  RELAXATION_FACTOR');
    WriteFloat(FImsPackage.RelaxationFactor);
    NewLine;
  end;

  WritePreconditionerLevels;
  WritePreconditionerDropTolerances;
  WriteNumberOfOrthogonalizations;

  if soScalingMethod in FImsPackage.SmsOverrides then
  begin
    WriteString('  SCALING_METHOD ');
    case FImsPackage.ScalingMethod of
      ssmNone: WriteString('NONE');
      ssmDiagonal: WriteString('DIAGONAL');
      ssmL2Norm: WriteString('L2NORM');
      else Assert(False);
    end;
    NewLine;
  end;
  WriteReorderingMethod;

  WriteString('END LINEAR');
  NewLine;
  NewLine;
end;

procedure TImsWriter.WriteNonLinearBlock;
var
  BacktrackingNumber: integer;
//var
//  UseNonLinear: Boolean;
begin
//  UseNonLinear := False;
//  if [soOuterHclose, soOuterMaxIt, soUnderRelax, soBacktrackingNumber]
//    * FSmsPackage.SmsOverrides <> []  then
//  begin
//    UseNonLinear := True;
//  end
//  else if (soLinearSolver in FSmsPackage.SmsOverrides)
//    and (FSmsPackage.LinearSolver = slsXMD) then
//  begin
//    UseNonLinear := True;
//  end;
//
//  if not UseNonLinear then
//  begin
//    Exit;
//  end;


  WriteString('BEGIN NONLINEAR');
  NewLine;

  WriteString('  OUTER_HCLOSE ');
  if soOuterHclose in FImsPackage.SmsOverrides then
  begin
    WriteFloat(FImsPackage.OuterHclose);
  end
  else
  begin
    WriteFloat(0.01);
  end;
  NewLine;

  WriteString('  OUTER_MAXIMUM ');
  if soOuterMaxIt in FImsPackage.SmsOverrides then
  begin
    WriteInteger(FImsPackage.MaxOuterIterations);
  end
  else
  begin
    WriteInteger(100);
  end;
  NewLine;

  WriteString('  UNDER_RELAXATION ');
  if soUnderRelax in FImsPackage.SmsOverrides then
  begin
    case FImsPackage.UnderRelaxation of
      surNone: WriteString('NONE');
      surSimple: WriteString('SIMPLE');
      surDbd: WriteString('DTD');
      surCooley: WriteString('COOLEY');
      else Assert(False);
    end;
    NewLine;

    if FImsPackage.UnderRelaxation = surDbd then
    begin
      if soUnderRelaxTheta in FImsPackage.SmsOverrides then
      begin
        WriteString('  UNDER_RELAXATION_THETA ');
        WriteFloat(FImsPackage.UnderRelaxTheta);
        NewLine;
      end;
    end;

    if FImsPackage.UnderRelaxation = surDbd then
    begin
      if soUnderRelaxKappa in FImsPackage.SmsOverrides then
      begin
        WriteString('  UNDER_RELAXATION_KAPPA');
        WriteFloat(FImsPackage.UnderRelaxKappa);
        NewLine;
      end;
    end;

    if FImsPackage.UnderRelaxation <> surNone then
    begin
      if soUnderRelaxGamma in FImsPackage.SmsOverrides then
      begin
        WriteString('  UNDER_RELAXATION_GAMMA');
        WriteFloat(FImsPackage.UnderRelaxGamma);
        NewLine;
      end;
    end;

    if FImsPackage.UnderRelaxation = surDbd then
    begin
      if soUnderRelaxMomentum in FImsPackage.SmsOverrides then
      begin
        WriteString('  UNDER_RELAXATION_MOMENTUM');
        WriteFloat(FImsPackage.UnderRelaxMomentum);
        NewLine;
      end;
    end;
  end
  else
  begin
    WriteString('NONE');
    NewLine;
  end;

  if soBacktrackingNumber in FImsPackage.SmsOverrides then
  begin
    BacktrackingNumber := FImsPackage.BacktrackingNumber;
  end
  else
  begin
    BacktrackingNumber := 10;
  end;
  WriteString('  BACKTRACKING_NUMBER');
  WriteInteger(BacktrackingNumber);
  NewLine;

  if BacktrackingNumber > 0 then
  begin
    if soBacktrackingTolerance in FImsPackage.SmsOverrides then
    begin
      WriteString('  BACKTRACKING_TOLERANCE');
      WriteFloat(FImsPackage.BacktrackingTolerance);
      NewLine;
    end;

    if soBacktrackingReductionFactor in FImsPackage.SmsOverrides then
    begin
      WriteString('  BACKTRACKING_REDUCTION_FACTOR');
      WriteFloat(FImsPackage.BacktrackingReductionFactor);
      NewLine;
    end;

    if soBacktrackingResidualLimit in FImsPackage.SmsOverrides then
    begin
      WriteString('  BACKTRACKING_RESIDUAL_LIMIT');
      WriteFloat(FImsPackage.BacktrackingResidualLimit);
      NewLine;
    end;
  end;

//  if (soLinearSolver in FSmsPackage.SmsOverrides)
//    and (FSmsPackage.LinearSolver = slsXMD) then
//  begin
//    WriteString('  LINEAR_SOLVER XMD');
//    NewLine;
//  end;

  WriteString('END NONLINEAR');
  NewLine;
  NewLine;
end;

procedure TImsWriter.WriteOptions;
var
  CsvFile: string;
begin
  WriteBeginOptions;

  WriteString('  PRINT_OPTION ');
  case FImsPackage.Print of
    spPrintNone: WriteString('NONE');
    spSummary: WriteString('SUMMARY');
    spFull: WriteString('ALL');
  end;
  NewLine;

  WriteString('  COMPLEXITY ');
  case FImsPackage.Complexity of
    scoSimple: WriteString('SIMPLE');
    scoModerate: WriteString('MODERATE');
    scoComplex: WriteString('COMPLEX');
//    scoSpecified: WriteString('SPECIFIED');
    else Assert(False);
  end;
  NewLine;

  if FImsPackage.CsvOutput <> sspNone then
  begin
    WriteString('CSV_OUTPUT FILEOUT ');
    CsvFile := ChangeFileExt(FNameOfFile, '.Solution.CSV');
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
    NewLine;
  end;

  WriteEndOptions;
end;

//procedure TImsWriter.WriteXmdBlock;
//var
//  UseXmd: Boolean;
//begin
//  UseXmd := False;
//  if [soInnerMaxIterations, soInnerHclose, soInnerRclose,
//    soXmdLinearAcceleration, soPreconditionerLevel,
//    soPreconditionerDropTolerance, soNumberOfOrthoganalizations,
//    soReorderingMethod]
//    * FSmsPackage.SmsOverrides <> [] then
//  begin
//    UseXmd := True;
//  end
//  else if (soRedBlackOrder in FSmsPackage.SmsOverrides)
//    and FSmsPackage.RedBlackOrder then
//  begin
//    UseXmd := True;
//  end;
//
//  if not UseXmd then
//  begin
//    Exit;
//  end;
//
//  WriteString('BEGIN XMD');
//  NewLine;
//
//  WriteInnerMaximum;
//  WriteInnerHClose;
//
//  WriteString('  INNER_RCLOSE');
//  if soInnerRclose in FSmsPackage.SmsOverrides then
//  begin
//    WriteFloat(FSmsPackage.InnerRclose);
//  end
//  else
//  begin
//    WriteFloat(0.1);
//  end;
//  NewLine;
//
//  WriteString('  LINEAR_ACCELERATION ');
//  if soXmdLinearAcceleration in FSmsPackage.SmsOverrides then
//  begin
//    case FSmsPackage.XmdLinearAcceleration of
//      sxlaCg: WriteString('CG');
//      sxlaOrthomin: WriteString('ORTHOMIN');
//      sxlaBiCgStab: WriteString('BICGSTAB');
//      else Assert(False);
//    end;
//  end
//  else
//  begin
//    WriteString('CG');
//  end;
//  NewLine;
//
//  WritePreconditionerLevels;
//  WritePreconditionerDropTolerances;
//  WriteNumberOfOrthogonalizations;
//
//  if (soRedBlackOrder in FSmsPackage.SmsOverrides)
//    and FSmsPackage.RedBlackOrder then
//  begin
//    WriteString('  RED_BLACK_ORDERING');
//    NewLine;
//  end;
//
//  WriteReorderingMethod;
//
//  WriteString('END XMD');
//  NewLine;
//end;

end.
