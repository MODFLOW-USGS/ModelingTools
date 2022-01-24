unit ModflowDE4_WriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

Type
  TDe4Writer = class(TCustomSolverWriter)
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
  StrWritingDE4Package = 'Writing DE4 Package input.';

{ TDe4Writer }

class function TDe4Writer.Extension: string;
begin
  result := '.de4';
end;

function TDe4Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.De4Package;
end;

procedure TDe4Writer.WriteDataSet1;
var
  DE4: TDE4PackageSelection;
begin
  DE4 := Model.ModflowPackages.De4Package;
  WriteInteger(DE4.ITMX);
  WriteInteger(DE4.MXUP);
  WriteInteger(DE4.MXLOW);
  WriteInteger(DE4.MXBW);
  WriteString(' # MXUP MXLOW MXBW');
  NewLine;
end;

procedure TDe4Writer.WriteDataSet2;
var
  DE4: TDE4PackageSelection;
begin
  DE4 := Model.ModflowPackages.De4Package;
  WriteInteger(DE4.IFREQ);
  WriteInteger(DE4.MUTD4);
  WriteFloat(DE4.ACCL.Value);
  WriteFloat(DE4.HCLOSE.Value);
  WriteInteger(DE4.IPRD4);
  WriteString(' # IFREQ MUTD4 ACCL HCLOSE IPRD4');
  NewLine;
end;

procedure TDe4Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally(Model) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  WriteToNameFile('DE4', Model.UnitNumbers.UnitNumber(StrDE4), NameOfFile, foInput, Model);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDE4Package);
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

end.
