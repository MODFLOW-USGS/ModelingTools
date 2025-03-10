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
    procedure WritePrintFlowsOption;
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; SpeciesIndex: Integer);
  end;


implementation

uses
  ModflowOutputControlUnit;



{ TModflowGwtSsmWriter }

class function TModflowGwtSsmWriter.Extension: string;
begin
  result := '.ssm';
end;

function TModflowGwtSsmWriter.Package: TModflowPackageSelection;
begin
  if Model.GweUsed and (Model.MobileComponents[FSpeciesIndex].UsedForGWE) then
  begin
    result := Model.ModflowPackages.GweSsmPackage;
  end
  else
  begin
    result := Model.ModflowPackages.GwtSsmPackage;
  end;
end;

procedure TModflowGwtSsmWriter.WriteFile(AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
begin
  FSpeciesIndex := SpeciesIndex;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not (Model.GwtUsed or Model.GweUsed) then
  begin
    Exit
  end;
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

  WritePrintFlowsOption;
  WriteSaveFlowsOption;

  WriteEndOptions;
end;

procedure TModflowGwtSsmWriter.WritePrintFlowsOption;
var
  OC: TModflowOutputControl;
begin
  OC := Model.ModflowOutputControl;
  if OC.SaveCellFlows in [csfListing, csfBoth] then
  begin
    WriteString('  PRINT_FLOWS');
    NewLine;
  end;
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
