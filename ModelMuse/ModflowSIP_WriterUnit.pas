unit ModflowSIP_WriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

Type
  TSipWriter = class(TCustomSolverWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, PhastModelUnit, frmProgressUnit, GoPhastTypes;

resourcestring
  StrWritingSIPPackage = 'Writing SIP Package input.';

{ TSipWriter }

class function TSipWriter.Extension: string;
begin
  result := '.sip';
end;

function TSipWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SipPackage;
end;

procedure TSipWriter.WriteDataSet1;
var
  SIP: TSIPPackageSelection;
begin
  SIP := Model.ModflowPackages.SipPackage;
  WriteInteger(SIP.MXITER);
  WriteInteger(SIP.NPARM);
  WriteString(' # Data Set 1: MXITER NPARM');
  NewLine;
end;

procedure TSipWriter.WriteDataSet2;
var
  SIP: TSIPPackageSelection;
begin
  SIP := Model.ModflowPackages.SipPackage;
  WriteFloat(SIP.ACCL.Value);
  WriteFloat(SIP.HCLOSE.Value);
  WriteInteger(SIP.IPCALC);
  WriteFloat(SIP.WSEED.Value);
  WriteInteger(SIP.IPRSIP);
  WriteString(' # Data Set 2: ACCL HCLOSE IPCALC WSEED IPRSIP');
  NewLine;
end;

procedure TSipWriter.WriteFile(const AFileName: string);
//var
//  NameOfFile: string;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally(Model) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile('SIP', Model.UnitNumbers.UnitNumber(StrSIP), FNameOfFile, foInput, Model);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingSIPPackage);
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

end.
