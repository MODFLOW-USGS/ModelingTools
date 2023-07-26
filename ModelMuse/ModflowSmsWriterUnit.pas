unit ModflowSmsWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit,
  PhastModelUnit;

type
  TImsWriter = class(TCustomSolverWriter)
  private
    FDVClose: Double;
    FSpeciesName: string;
    // A negative value means that this is for the flow model.
    FSpeciesIndex: Integer;
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
    procedure WriteLinearBlock;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType;
      SpeciesIndex: Integer); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses frmProgressUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrIMSSolverProblem = 'IMS solver problem: INNER_DVCLOSE >= OUTER_DVCLOSE';
  StrINNERDVCLOSEIsTyp = 'INNER_DVCLOSE is typically and order of magnitude ' +
  'less than OUTER_DVCLOSE. In this model INNER_DVCLOSE is %0:g and OUTER_DV' +
  'CLOSE is %1:g.';

{ TSmsWriter }

constructor TImsWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType; SpeciesIndex: Integer);
begin
  inherited Create(AModel, EvaluationType);
  FSpeciesIndex := SpeciesIndex;
end;

class function TImsWriter.Extension: string;
begin
  result := '.ims';
end;

function TImsWriter.Package: TModflowPackageSelection;
begin
  if FSpeciesIndex < 0 then
  begin
    result := Model.ModflowPackages.SmsPackage;
  end
  else
  begin
    result := Model.ModflowPackages.GwtPackages[FSpeciesIndex].GwtIms;
  end;
end;

procedure TImsWriter.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally(Model) then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrIMSSolverProblem);
  if FSpeciesIndex < 0 then
  begin
    FNameOfFile := FileName(AFileName);
    FSpeciesName := '';
  end
  else
  begin
    if not Model.MobileComponents[FSpeciesIndex].UsedForGWT then
    begin
      Exit;
    end;
    FNameOfFile := GwtFileName(AFileName, FSpeciesIndex);
    FSpeciesName := Model.MobileComponents[FSpeciesIndex].Name;
  end;
  // write to simulation name file
  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing IMS Package input');
    WriteDataSet0;
    FImsPackage := Package as TSmsPackageSelection;
    WriteOptions;
    WriteNonLinearBlock;
    WriteLinearBlock;
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
var
  InnerDvClose: Double;
begin
  WriteString('  INNER_DVCLOSE');
  InnerDvClose := 0.0001;
  if soInnerHclose in FImsPackage.SmsOverrides then
  begin
    InnerDvClose := FImsPackage.InnerHclose;
//    WriteFloat(FImsPackage.InnerHclose);
  end
  else
  begin
    case FImsPackage.Complexity of
      scoSimple: InnerDvClose := 0.0001;
      scoModerate: InnerDvClose := 0.001;
      scoComplex: InnerDvClose := 0.01;
      else Assert(False);
    end;
  end;
  WriteFloat(InnerDvClose);
  if InnerDvClose >= FDVClose then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrIMSSolverProblem,
	    Format(StrINNERDVCLOSEIsTyp, [InnerDvClose, FDVClose]));
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
    case FImsPackage.Complexity of
      scoSimple: WriteInteger(50);
      scoModerate: WriteInteger(100); 
      scoComplex: WriteInteger(500);
      else Assert(False);
    end;
  end;
  NewLine;
end;

procedure TImsWriter.WriteLinearBlock;
begin
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
  case FImsPackage.UsedLinAccel of
    sllaCg: WriteString('CG');
    sllaBiCgStab: WriteString('BICGSTAB');
    else Assert(False);
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
begin

  WriteString('BEGIN NONLINEAR');
  NewLine;

  WriteString('  OUTER_DVCLOSE ');
  if soOuterHclose in FImsPackage.SmsOverrides then
  begin
    FDVClose := FImsPackage.OuterHclose;
  end
  else
  begin
    case FImsPackage.Complexity of
      scoSimple: FDVClose :=  0.001;
      scoModerate: FDVClose := 0.01;
      scoComplex: FDVClose := 0.1;
      else Assert(False);
    end;
  end;
  WriteFloat(FDVClose);
  NewLine;

  WriteString('  OUTER_MAXIMUM ');
  if soOuterMaxIt in FImsPackage.SmsOverrides then
  begin
    WriteInteger(FImsPackage.MaxOuterIterations);
  end
  else
  begin
    case FImsPackage.Complexity of
      scoSimple: WriteInteger(25);
      scoModerate: WriteInteger(50);
      scoComplex: WriteInteger(100);
      else Assert(False);
    end;
  end;
  NewLine;

  WriteString('  UNDER_RELAXATION ');
  if soUnderRelax in FImsPackage.SmsOverrides then
  begin
    case FImsPackage.UnderRelaxation of
      surNone: WriteString('NONE');
      surSimple: WriteString('SIMPLE');
      surDbd: WriteString('DBD');
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
      // You need to specify gamma even if it is not overridden.
//      if (soUnderRelaxGamma in FImsPackage.SmsOverrides)
//        or (FImsPackage.UnderRelaxation = surSimple) then
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

  BacktrackingNumber := -1;
  if soBacktrackingNumber in FImsPackage.SmsOverrides then
  begin
    BacktrackingNumber := FImsPackage.BacktrackingNumber;
  end
  else
  begin
    case FImsPackage.Complexity of
      scoSimple: BacktrackingNumber := 0;
      scoModerate: BacktrackingNumber := 0;
      scoComplex: BacktrackingNumber := 20;
      else Assert(False);
    end;
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

  WriteString('END NONLINEAR');
  NewLine;
  NewLine;
end;

procedure TImsWriter.WriteOptions;
var
  CsvFile: string;
  BaseFile: string;
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
    else Assert(False);
  end;
  NewLine;

  BaseFile := ChangeFileExt(FNameOfFile, '');
  if FImsPackage.CsvOutput <> sspNone then
  begin
    WriteString('  CSV_OUTER_OUTPUT FILEOUT ');
    if FSpeciesName = '' then
    begin
      CsvFile := ChangeFileExt(BaseFile, '.OuterSolution.CSV');
    end
    else
    begin
      CsvFile := ChangeFileExt(BaseFile, '.' + FSpeciesName + '.OuterSolution.CSV');
    end;
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
    NewLine;
  end;

  if FImsPackage.CsvOutput <> sspNone then
  begin
    WriteString('  CSV_INNER_OUTPUT FILEOUT ');
    if FSpeciesName = '' then
    begin
      CsvFile := ChangeFileExt(BaseFile, '.InnerSolution.CSV');
    end
    else
    begin
      CsvFile := ChangeFileExt(BaseFile, '.' + FSpeciesName + '.InnerSolution.CSV');
    end;
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
    NewLine;
  end;

  case FImsPackage.UsePTC  of
    upUse:
      begin
        // do nothing
      end;
    upDontUseForAll:
      begin
        WriteString('  NO_PTC ALL');
        NewLine;
      end;
    upDontUseForFirst:
      begin
        WriteString('  NO_PTC FIRST');
        NewLine;
      end;
    else
      Assert(False);
  end;

  WriteString('  ATS_OUTER_MAXIMUM_FRACTION');
  WriteFloat(FImsPackage.AtsOuterMaxFraction);
  NewLine;

  WriteEndOptions;
end;

end.
