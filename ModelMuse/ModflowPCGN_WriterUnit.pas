unit ModflowPCGN_WriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

Type
  TPcgnWriter = class(TCustomSolverWriter)
  private
    FNameOfFile: string;
    FPcgn: TPcgnSelection;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, PhastModelUnit, frmProgressUnit;

resourcestring
  StrWritingPCGNPackage = 'Writing PCGN Package input.';


{ TPcgnWriter }

class function TPcgnWriter.Extension: string;
begin
  result := '.pcgn';
end;

function TPcgnWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.PcgnPackage;
end;

procedure TPcgnWriter.WriteDataSet1;
begin
  WriteInteger(FPcgn.ITER_MO);
  WriteInteger(FPcgn.ITER_MI);
  WriteFloat(FPcgn.CLOSE_R.Value);
  WriteFloat(FPcgn.CLOSE_H.Value);
  WriteString(' # ITER_MO, ITER_MI, CLOSE_R, CLOSE_H');
  NewLine;
end;

procedure TPcgnWriter.WriteDataSet2;
var
  UNIT_PC: integer;
  UNIT_TS: integer;
  NameOfFile: string;
begin
  WriteFloat(FPcgn.RELAX.Value);
  WriteInteger(FPcgn.IFILL);

  if FPcgn.UNIT_PC then
  begin
    UNIT_PC := PCGN_UNIT_PC;
    NameOfFile := ChangeFileExt(FNameOfFile, '.pcgn_pc');
    WriteToNameFile(StrData, UNIT_PC, NameOfFile, foOutput, Model);
  end
  else
  begin
    UNIT_PC := 0;
  end;
  WriteInteger(UNIT_PC);

  if FPcgn.UNIT_TS then
  begin
    UNIT_TS := PCGN_UNIT_TS;
    NameOfFile := ChangeFileExt(FNameOfFile, '.pcgn_ts');
    WriteToNameFile(StrData, UNIT_TS, NameOfFile, foOutput, Model);
  end
  else
  begin
    UNIT_TS := 0;
  end;
  WriteInteger(UNIT_TS);
  WriteString(' # RELAX, IFILL, UNIT_PC, UNIT_TS');
  NewLine;
end;

procedure TPcgnWriter.WriteDataSet3;
var
  ADAMP: integer;
begin
  if FPcgn.ITER_MO > 1 then
  begin
    ADAMP := Ord(FPcgn.ADAMP);
    WriteInteger(ADAMP);

    WriteFloat(FPcgn.DAMP.Value);
    WriteFloat(FPcgn.DAMP_LB.Value);
    WriteFloat(FPcgn.RATE_D.Value);
    WriteFloat(FPcgn.CHGLIMIT.Value);

    WriteString(' # ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT');
    NewLine;
  end;
end;

procedure TPcgnWriter.WriteDataSet4;
var
  ACNVG: integer;
  IPUNIT: integer;
  NameOfFile: string;
begin
  if FPcgn.ITER_MO > 1 then
  begin
    ACNVG := Ord(FPcgn.ACNVG);
    WriteInteger(ACNVG);
    WriteFloat(FPcgn.CNVG_LB.Value);
    WriteInteger(FPcgn.MCNVG);
    WriteFloat(FPcgn.RATE_C.Value);
    IPUNIT := Ord(FPcgn.IPUNIT)-1;
    if IPUNIT > 0 then
    begin
      IPUNIT := PCGN_IPUNIT;
      NameOfFile := ChangeFileExt(FNameOfFile, '.pcgn_ip');
      WriteToNameFile(StrData, IPUNIT, NameOfFile, foOutput, Model);
    end;
    WriteInteger(IPUNIT);
    WriteString(' # ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT');
    NewLine;
  end;
end;

procedure TPcgnWriter.WriteFile(const AFileName: string);
begin
  FPcgn := Package as TPcgnSelection;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile('PCGN', Model.UnitNumbers.UnitNumber(StrPCGN),
    FNameOfFile, foInput, Model);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingPCGNPackage);
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2;
    WriteDataSet3;
    WriteDataSet4;
  finally
    CloseFile;
  end;
end;

end.
