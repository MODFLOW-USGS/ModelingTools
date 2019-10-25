unit ModflowMt3dmsLinkWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowMt3dmsLinkWriter = class(TCustomPackageWriter)
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, SysUtils, GoPhastTypes,
  ModflowPackagesUnit;

resourcestring
  StrWritingLMT6Package = 'Writing LMT6 Package input.';
  StrWritingLMT7Package = 'Writing LMT7 Package input.';

{ TModflowMt3dmsLinkWriter }

class function TModflowMt3dmsLinkWriter.Extension: string;
begin
  result := '.lmt';
end;

function TModflowMt3dmsLinkWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mt3dBasic;
end;

procedure TModflowMt3dmsLinkWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  FtlFileName: string;
  Packages: TModflowPackages;
begin
  if not Package.IsSelected then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrLMT6) then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrLMT7) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  FtlFileName := ChangeFileExt(NameOfFile, '.ftl');
  if Model.ModelSelection = msModflowLGR2 then
  begin
    WriteToNameFile(StrLMT7, Model.UnitNumbers.UnitNumber(StrLMT7),
      NameOfFile, foInput, Model);
  end
  else
  begin
    WriteToNameFile(StrLMT6, Model.UnitNumbers.UnitNumber(StrLMT6),
      NameOfFile, foInput, Model);
  end;

  Model.AddModelOutputFile(FtlFileName);
  {
  if muoTextFtl in Model.ModflowPackages.Mt3dBasic.Mt3dUsgsOptions then
  begin
    WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrFTL),
      FtlFileName, foOutput, Model);
  end
  else
  begin
    WriteToNameFile(StrDATABINARY, Model.UnitNumbers.UnitNumber(StrFTL),
      FtlFileName, foOutput, Model);
  end;
  }

  OpenFile(NameOfFile);
  try
    if Model.ModelSelection = msModflowLGR2 then
    begin
      frmProgressMM.AddMessage(StrWritingLMT7Package);

    end
    else
    begin
      frmProgressMM.AddMessage(StrWritingLMT6Package);
    end;

    WriteString('OUTPUT_FILE_NAME ');
    WriteString(ExtractFileName(FtlFileName));
    NewLine;

    WriteString('OUTPUT_FILE_UNIT ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFTL));
    NewLine;

    WriteString('OUTPUT_FIlE_HEADER Extended');
    NewLine;

    if muoTextFtl in Model.ModflowPackages.Mt3dBasic.Mt3dUsgsOptions then
    begin
      WriteString('OUTPUT_FILE_FORMAT formatted');
    end
    else
    begin
      WriteString('OUTPUT_FILE_FORMAT Unformatted');
    end;
    NewLine;

    {$IFDEF Mt3dUSGS}
    Packages := Model.ModflowPackages;
    if Packages.Mt3dUnsatTransport.IsSelected
      or Packages.Mt3dLkt.IsSelected
      or Packages.Mt3dSft.IsSelected then
    begin
      WriteString('PACKAGE_FLOWS');
      if Packages.Mt3dUnsatTransport.IsSelected then
      begin
        WriteString(' UZF');
      end;
      if Packages.Mt3dLkt.IsSelected then
      begin
        WriteString(' LAK');
      end;
      if Packages.Mt3dSft.IsSelected then
      begin
        WriteString(' SFR');
      end;
      NewLine;
    end;
    {$ENDIF}
  finally
    CloseFile;
  end;
end;

end.
