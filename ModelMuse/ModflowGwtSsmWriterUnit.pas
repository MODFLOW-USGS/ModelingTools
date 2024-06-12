unit ModflowGwtSsmWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowGwtSsmWriter = class(TCustomPackageWriter)
  private
    FSpeciesIndex: Integer;
    procedure WriteOptions;
    procedure WriteSources;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; SpeciesIndex: Integer);
  end;


implementation



{ TModflowGwtSsmWriter }

class function TModflowGwtSsmWriter.Extension: string;
begin
  result := '.ssm';
end;

function TModflowGwtSsmWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtSsmPackage;
end;

procedure TModflowGwtSsmWriter.WriteFile(AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FSpeciesIndex := SpeciesIndex;
//  FAdvPackage := Model.ModflowPackages.GwtSsmPackage;

  Abbreviation := 'SSM6';
  GwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := GwtFile;
  FInputFileName := GwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    writeSources;
  finally
    CloseFile;
  end;

end;

procedure TModflowGwtSsmWriter.WriteOptions;
begin
  WriteBeginOptions;

//  WritePrintFlowsOption;
  WriteSaveFlowsOption;

  WriteEndOptions;
end;

procedure TModflowGwtSsmWriter.WriteSources;
var
  SpeciesName: string;
begin
  SpeciesName := Model.MobileComponents[FSPeciesIndex].Name;

  WriteString('BEGIN SOURCES');
  NewLine;

  if Model.ModflowPackages.ChdBoundary.IsSelected then
  begin
    WriteString('  CHD-1 AUX ');
    WriteString(SpeciesName);
    NewLine;
  end;

  if Model.ModflowPackages.GhbBoundary.IsSelected then
  begin
    WriteString('  GHB-1 AUX ');
    WriteString(SpeciesName);
    NewLine;
  end;

  if Model.ModflowPackages.WelPackage.IsSelected then
  begin
    WriteString('  WEL-1 AUX ');
    WriteString(SpeciesName);
    NewLine;
  end;

  if Model.ModflowPackages.RivPackage.IsSelected then
  begin
    WriteString('  RIV-1 AUX ');
    WriteString(SpeciesName);
    NewLine;
  end;

  // is this needed or not?
//  if Model.ModflowPackages.DrnPackage.IsSelected then
//  begin
//    WriteString('  DRN-1 AUX ');
//    WriteString(SpeciesName);
//    NewLine;
//  end;

  if Model.ModflowPackages.RchPackage.IsSelected then
  begin
    WriteString('  RCH-1 AUXMIXED ');
    WriteString(SpeciesName);
    NewLine;
  end;

  if Model.ModflowPackages.EtsPackage.IsSelected then
  begin
    WriteString('  EVT-1 AUXMIXED ');
    WriteString(SpeciesName);
    NewLine;
  end;

  WriteString('END SOURCES');
  NewLine;
end;

end.
