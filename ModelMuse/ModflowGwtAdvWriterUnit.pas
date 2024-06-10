unit ModflowGwtAdvWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowGwtAdvWriter = class(TCustomPackageWriter)
  private
    FAdvPackage: TGwtAdvectionPackage;
    procedure WriteOptions;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string);
  end;

implementation

uses
  System.IOUtils;

{ TModflowGwtAdvWriter }

class function TModflowGwtAdvWriter.Extension: string;
begin
  result := '.adv';
end;

function TModflowGwtAdvWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtAdvectionPackage;
end;

procedure TModflowGwtAdvWriter.WriteFile(AFileName: string);
var
  Abbreviation: string;
  GwtFile: string;
  SpeciesIndex: Integer;
  SpeciesGwtFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FAdvPackage := Model.ModflowPackages.GwtAdvectionPackage;

  Abbreviation := 'ADV6';
  GwtFile := GwtFileName(AFileName, 0);
  FNameOfFile := GwtFile;
  FInputFileName := GwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, 0);

  FPestParamUsed := False;
  WritingTemplate := False;

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
  finally
    CloseFile;
  end;

  for SpeciesIndex := 1 to Model.MobileComponents.Count - 1 do
  begin
    SpeciesGwtFile := GwtFileName(AFileName, SpeciesIndex);
    WriteToGwtNameFile(Abbreviation, SpeciesGwtFile, SpeciesIndex);
    TFile.Copy(GwtFile, SpeciesGwtFile, True);
  end;
end;

procedure TModflowGwtAdvWriter.WriteOptions;
begin
  WriteBeginOptions;

  WriteString('  SCHEME ');
  case FAdvPackage.Scheme of
    gsUpstream:
      begin
        WriteString('upstream');
      end;
    gsCentral:
      begin
        WriteString('central');
      end;
    gsTVD:
      begin
        WriteString('tvd');
      end;
    else
      Assert(False);
  end;
  NewLine;

  WriteEndOptions;
end;

end.
