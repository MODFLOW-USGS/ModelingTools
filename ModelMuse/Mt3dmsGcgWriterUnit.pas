unit Mt3dmsGcgWriterUnit;

interface
uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms, PhastModelUnit;

type
  TMt3dmsGcgWriter = class(TCustomModflowWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, GoPhastTypes;

resourcestring
  StrWritingMT3DMSGcgP = 'Writing MT3DMS or MT3D-USGS gcg Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';

{ TMt3dmsGcgWriter }

class function TMt3dmsGcgWriter.Extension: string;
begin
  result := '.gcg';
end;

procedure TMt3dmsGcgWriter.WriteDataSet1;
var
  MXITER, ITER1, ISOLVE, NCRS: integer;
  GcgPkg: TMt3dmsGCGSolverPackage;
begin
  GcgPkg := Model.ModflowPackages.Mt3dmsGCGSolver;
  MXITER := GcgPkg.MaxOuterIterations;
  ITER1 := GcgPkg.MaxInnerIterations;
  ISOLVE := Ord(GcgPkg.PreconditionerChoice)+1;
  NCRS := Ord(GcgPkg.DispersionTensorChoice);
  WriteInteger(MXITER);
  WriteInteger(ITER1);
  WriteInteger(ISOLVE);
  WriteInteger(NCRS);
  WriteString(' # Data Set 1: MXITER, ITER1, ISOLVE, NCRS');
  NewLine;
end;

procedure TMt3dmsGcgWriter.WriteDataSet2;
var
  ACCL, CCLOSE: double;
  IPRGCG: integer;
  GcgPkg: TMt3dmsGCGSolverPackage;
begin
  GcgPkg := Model.ModflowPackages.Mt3dmsGCGSolver;
  ACCL := GcgPkg.RelaxationFactor;
  CCLOSE := GcgPkg.ConvergenceCriterion;
  IPRGCG := GcgPkg.PrintoutInterval;
  WriteFloat(ACCL);
  WriteFloat(CCLOSE);
  WriteInteger(IPRGCG);
  WriteString(' # Data Set 2: ACCL, CCLOSE, IPRGCG');
  NewLine;
end;

procedure TMt3dmsGcgWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Model.ModflowPackages.Mt3dmsGCGSolver.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);

  NameOfFile := FileName(AFileName);
  // PackageGeneratedExternally needs to be updated for MT3DMS
  if Model.PackageGeneratedExternally(StrGCG) then
  begin
    Exit;
  end;

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrGCG, Mt3dGcg, NameOfFile, foInput, Model);

  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMT3DMSGcgP);

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

end.
