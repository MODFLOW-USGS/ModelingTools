unit ModflowBuoyancyWriterUnit;

interface

uses
  CustomModflowWriterUnit, Vcl.Forms, ModflowPackageSelectionUnit,
  System.SysUtils;

type
  TBuoyancyWriter = class(TCustomPackageWriter)
  private
    FBuoyancyPackage: TBuoyancyPackage;
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
  StrEvaluatingBUY6Pack = 'Evaluating BUY6 Package data.';

{ TBuoyancyWriter }

class function TBuoyancyWriter.Extension: string;
begin
  result := '.buy';
end;

function TBuoyancyWriter.Package: TModflowPackageSelection;
begin
  Result := Model.ModflowPackages.BuoyancyPackage;
end;

procedure TBuoyancyWriter.WriteDimensions;
begin
  WriteBeginDimensions;
  try
    WriteString('  NRHOSPECIES');
    WriteInteger(Model.MobileComponents.Count);
    NewLine;
  finally
    WriteEndDimensions;
  end;
end;

procedure TBuoyancyWriter.WriteFile(const AFileName: string);
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrBuy) then
  begin
    Exit;
  end;

  FBuoyancyPackage := Package as TBuoyancyPackage;

  frmProgressMM.AddMessage(StrEvaluatingBUY6Pack);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrBuy, 0, FNameOfFile, foInput, Model, False, StrBuy);
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

procedure TBuoyancyWriter.WriteOptions;
var
  FileOut: string;
begin
  WriteBeginOptions;
  try
    if FBuoyancyPackage.RightHandSide then
    begin
      WriteString('  HHFORMULATION_RHS');
      NewLine;
    end;

    WriteString('  DENSEREF');
    WriteFloat(FBuoyancyPackage.RefDensity);
    NewLine;

    if FBuoyancyPackage.WriteDensity then
    begin
      FileOut := ChangeFileExt(FNameOfFile, '.density');
      Model.AddModelOutputFile(FileOut);

      FileOut := ExtractFileName(FileOut);
      WriteString('  DENSITY FILEOUT ');
      WriteString(FileOut);
      NewLine;
    end;
  finally
    WriteEndOptions
  end;
end;

procedure TBuoyancyWriter.WritePackageData;
var
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
begin
  WriteBeginPackageData;
  try
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ASpecies := Model.MobileComponents[SpeciesIndex];
      WriteInteger(SpeciesIndex+1);
      WriteFloat(ASpecies.DensitySlope);
      WriteFloat(ASpecies.RefConcentration);
      WriteString(' ' + ASpecies.Name);
      WriteString(' ' + ASpecies.Name);
      NewLine;
    end;
  finally
    WriteEndPackageData;
  end;
end;

end.
