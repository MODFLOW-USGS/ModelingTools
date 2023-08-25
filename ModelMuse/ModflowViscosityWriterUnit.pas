unit ModflowViscosityWriterUnit;

interface

uses
  CustomModflowWriterUnit, Vcl.Forms, ModflowPackageSelectionUnit,
  System.SysUtils, System.Classes;

type
  TViscosityWriter = class(TCustomPackageWriter)
  private
    FViscosityPackage: TViscosityPackage;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
  protected
    function Package: TModflowPackageSelection; override;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses
  ModflowUnitNumbers, frmProgressUnit, GoPhastTypes, Mt3dmsChemSpeciesUnit;

resourcestring
  StrEvaluatingVSC6Pack = 'Evaluating VSC6 Package data.';

{ TViscosityWriter }

class function TViscosityWriter.Extension: string;
begin
  result := '.vsc'
end;

function TViscosityWriter.Package: TModflowPackageSelection;
begin
  Result := Model.ModflowPackages.ViscosityPackage;
end;

procedure TViscosityWriter.WriteDimensions;
var
  IgnoredNames: TStringList;
begin
  WriteBeginDimensions;
  IgnoredNames := TStringList.Create;
  try
    Model.GetIgnoredSpeciesNames(IgnoredNames);
    WriteString('  NVISCSPECIES');
    WriteInteger(Model.MobileComponents.Count - IgnoredNames.Count);
    NewLine;
  finally
    IgnoredNames.Free;
    WriteEndDimensions;
  end;
end;

procedure TViscosityWriter.WriteFile(const AFileName: string);
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrVsc) then
  begin
    Exit;
  end;

  FViscosityPackage := Package as TViscosityPackage;

  frmProgressMM.AddMessage(StrEvaluatingVSC6Pack);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrVsc, 0, FNameOfFile, foInput, Model, False, StrVsc);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FInputFileName := FNameOfFile;

  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing Buoyancy Package');
    Application.ProcessMessages;

    WriteTemplateHeader;

    WriteDataSet0;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDimensions);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('Writing Package Data');
    WritePackageData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TViscosityWriter.WriteOptions;
var
  FileOut: string;
begin
  WriteBeginOptions;
  try
    WriteString('  VISCREF');
    WriteFloat(FViscosityPackage.RefViscosity);
    NewLine;

    if FViscosityPackage.ThermalSpecies <> '' then
    begin
      WriteString('  TEMPERATURE_SPECIES_NAME ');
      WriteString(FViscosityPackage.ThermalSpecies);
      NewLine;
    end;

    WriteString('  THERMAL_FORMULATION ');
    case FViscosityPackage.ThermalFormulation of
      tfLinear: WriteString('LINEAR');
      tfNonLinear: WriteString('NONLINEAR');
    end;
    NewLine;

    if FViscosityPackage.ThermalFormulation = tfNonLinear then
    begin
      WriteString('  THERMAL_A2');
      WriteFloat(FViscosityPackage.ThermalA2);
      NewLine;
      WriteString('  THERMAL_A3');
      WriteFloat(FViscosityPackage.ThermalA3);
      NewLine;
      WriteString('  THERMAL_A4');
      WriteFloat(FViscosityPackage.ThermalA4);
      NewLine;
    end;

    if FViscosityPackage.WriteViscosity then
    begin
      FileOut := ChangeFileExt(FNameOfFile, '.viscosity');
      Model.AddModelOutputFile(FileOut);

      FileOut := ExtractFileName(FileOut);
      WriteString('  VISCOSITY FILEOUT ');
      WriteString(FileOut);
      NewLine;
    end;
  finally
    WriteEndOptions
  end;
end;

procedure TViscosityWriter.WritePackageData;
var
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
  IgnoredNames: TStringList;
  SIndex: Integer;
begin
  WriteBeginPackageData;
  IgnoredNames := TStringList.Create;
  try
    IgnoredNames.CaseSensitive := False;
    Model.GetIgnoredSpeciesNames(IgnoredNames);
    SIndex := 1;
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ASpecies := Model.MobileComponents[SpeciesIndex];
      if IgnoredNames.IndexOf(ASpecies.Name) >= 0 then
      begin
        Continue;
      end;
      WriteInteger(SIndex);
      WriteFloat(ASpecies.ViscositySlope);
      WriteFloat(ASpecies.RefViscosity);
      WriteString(' ' + ASpecies.Name);
      WriteString(' ' + ASpecies.Name);
      NewLine;
      Inc(SIndex);
    end;
  finally
    WriteEndPackageData;
    IgnoredNames.Free;
  end;
end;

end.
