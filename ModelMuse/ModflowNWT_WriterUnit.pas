unit ModflowNWT_WriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TNwtWriter = class(TCustomSolverWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2a;
    procedure WriteDataSet2b;
    procedure WriteFloat(const Value: double);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

resourcestring
  StrTheΧMDInTheNWTS = 'The χMD in the NWT solver can not be used with local' +
  ' grid refinement in MODFLOW-OWHM. Try using the GMRES solver instead.';

implementation

uses ModflowUnitNumbers, PhastModelUnit, frmProgressUnit,
  frmErrorsAndWarningsUnit, frmGoPhastUnit;

resourcestring
  StrWritingNWTPackage = 'Writing NWT Package input.';
  StrIllegalSolverChoic = 'Illegal solver choice';

{ TNwtWriter }

class function TNwtWriter.Extension: string;
begin
  result := '.nwt';
end;

function TNwtWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.NwtPackage;
end;

procedure TNwtWriter.WriteFloat(const Value: double);
begin
  WriteString(' ');
  WriteF10Float(Value);
end;

procedure TNwtWriter.WriteDataSet1;
var
  NWT: TNwtPackageSelection;
  HEADTOL: double;
  FLUXTOL: double;
  MAXITEROUT: integer;
  THICKFACT: double;
  LINMETH: integer;
  IPRNWT: integer;
  IBOTAV: integer;
  OPTIONS: string;
  DBDTHETA: double;
  DBDKAPPA: double;
  DBDGAMMA: double;
  MOMFACT: double;
  BACKFLAG: integer;
  MAXBACKITER: integer;
  BACKTOL: double;
  BACKREDUCE: double;
begin
  NWT := Model.ModflowPackages.NwtPackage;
  HEADTOL := NWT.HeadTolerance.Value;
  FLUXTOL := NWT.FluxTolerance.Value;
  MAXITEROUT := NWT.MaxOuterIterations;
  THICKFACT := NWT.ThicknessFactor.Value;
  LINMETH := Ord(NWT.SolverMethod) + 1;
  if (LINMETH = 2) and frmGoPhast.PhastModel.LgrUsed then
  begin
    frmErrorsAndWarnings.AddError(Model,StrIllegalSolverChoic, StrTheΧMDInTheNWTS);
  end;
  IPRNWT := NWT.PrintFlag;
  IBOTAV := NWT.CorrectForCellBottom;
  case NWT.Option of
    noSimple: OPTIONS := ' SIMPLE';
    noModerate: OPTIONS := ' MODERATE';
    noComplex: OPTIONS := ' COMPLEX';
    noSpecified: OPTIONS := ' SPECIFIED';
    else Assert(False);
  end;
  if NWT.ContinueNWT then
  begin
    OPTIONS := OPTIONS + ' CONTINUE';
  end;
  DBDTHETA := NWT.DBDTheta.Value;
  DBDKAPPA := NWT.DBDKappa.Value;
  DBDGAMMA := NWT.DBDGamma.Value;
  MOMFACT := NWT.MomementumCoefficient.Value;
  BACKFLAG := NWT.BackFlag;
  MAXBACKITER := NWT.MaxBackIterations;
  BACKTOL := NWT.BackTol.Value;
  BACKREDUCE := NWT.BackReduce.Value;

  WriteFloat(HEADTOL);
  WriteFloat(FLUXTOL);
  WriteInteger(MAXITEROUT);
  WriteFloat(THICKFACT);
  WriteInteger(LINMETH);
  WriteInteger(IPRNWT);
  WriteInteger(IBOTAV);
  WriteString(OPTIONS);
  if NWT.Option = noSpecified then
  begin
    WriteFloat(DBDTHETA);
    WriteFloat(DBDKAPPA);
    WriteFloat(DBDGAMMA);
    WriteFloat(MOMFACT);
    WriteInteger(BACKFLAG);
    WriteInteger(MAXBACKITER);
    WriteFloat(BACKTOL);
    WriteFloat(BACKREDUCE);
  end;

  WriteString(' # Data Set 1, HEADTOL FLUXTOL MAXITEROUT THICKFACT LINMETH IPRNWT IBOTAV OPTIONS');
  if NWT.Option = noSpecified then
  begin
    WriteString(' DBDTHETA DBDKAPPA DBDGAMMA MOMFACT BACKFLAG MAXBACKITER BACKTOL BACKREDUCE');
  end;

  NewLine;

end;

procedure TNwtWriter.WriteDataSet2a;
var
  NWT: TNwtPackageSelection;
  MAXITINNER: integer;
  ILUMETHOD: integer;
  LEVFILL: integer;
  STOPTOL: double;
  MSDR: integer;
begin
  NWT := Model.ModflowPackages.NwtPackage;
  if (NWT.Option = noSpecified) and (NWT.SolverMethod = nsmGmres) then
  begin
    MAXITINNER := NWT.MaxIterInner;
    ILUMETHOD := Ord(NWT.IluMethod)+1;
    LEVFILL := -1;
    case NWT.IluMethod of
      nimDropTol: LEVFILL := NWT.FillLimit;
      nimKOrder: LEVFILL := NWT.FillLevel;
      else Assert(False);
    end;
    STOPTOL := NWT.StopTolerance.Value;
    MSDR := NWT.MaxGmresRestarts;

    WriteInteger(MAXITINNER);
    WriteInteger(ILUMETHOD);
    WriteInteger(LEVFILL);
    WriteFloat(STOPTOL);
    WriteInteger(MSDR);

    WriteString(' # Data set 2a, MAXITINNER ILUMETHOD LEVFILL STOPTOL MSDR');

    NewLine;
  end;
end;

procedure TNwtWriter.WriteDataSet2b;
var
  NWT: TNwtPackageSelection;
  IACL: integer;
  NORDER: integer;
  LEVEL: integer;
  NORTH: integer;
  IREDSYS: integer;
  RRCTOLS:double;
  IDROPTOL: integer;
  EPSRN:double;
  HCLOSEXMD:double;
  MXITERXMD: integer;
begin
  NWT := Model.ModflowPackages.NwtPackage;
  if (NWT.Option = noSpecified) and (NWT.SolverMethod = nsmChiMD) then
  begin
    IACL := Ord(NWT.AccelMethod);
    NORDER := Ord(NWT.OrderingMethod);
    LEVEL := NWT.Level;
    NORTH := NWT.NumberOfOrthogonalizations;
    IREDSYS := Ord(NWT.ApplyReducedPrecondition);
    RRCTOLS := NWT.ResidReducConv.Value;
    IDROPTOL := Ord(NWT.UseDropTolerance);
    EPSRN := NWT.DropTolerancePreconditioning.Value;
    HCLOSEXMD := NWT.InnerHeadClosureCriterion.Value;
    MXITERXMD := NWT.MaxInnerIterations;

    WriteInteger(IACL);
    WriteInteger(NORDER);
    WriteInteger(LEVEL);
    WriteInteger(NORTH);
    WriteInteger(IREDSYS);
    WriteFloat(RRCTOLS);
    WriteInteger(IDROPTOL);
    WriteFloat(EPSRN);
    WriteFloat(HCLOSEXMD);
    WriteInteger(MXITERXMD);

    WriteString(' # Data set 2b, IACL NORDER LEVEL NORTH IREDSYS RRCTOLS IDROPTOL EPSRN HCLOSEXMD MXITERXMD');
    NewLine;
  end;
end;

procedure TNwtWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model,StrIllegalSolverChoic);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile('NWT', Model.UnitNumbers.UnitNumber(StrNWT),
    NameOfFile, foInput, Model);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingNWTPackage);
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2a;
    WriteDataSet2b;
  finally
    CloseFile;
  end;
end;

end.
