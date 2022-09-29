unit ModflowFMI_WriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils;

type
  TModflowFmiWriter = class(TCustomModflowWriter)
  private
    FFlowFile: string;
    procedure WriteOptions;
    procedure WritePackageData;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowPackageSelectionUnit, ModflowLakMf6WriterUnit, ModflowMawWriterUnit,
  ModflowSfr6WriterUnit, ModflowUzfMf6WriterUnit, ModflowMvrWriterUnit;

{ TModflowFmiWriter }

class function TModflowFmiWriter.Extension: string;
begin
  result := '.fmi';
end;

procedure TModflowFmiWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  SpeciesIndex: Integer;
  SpeciesName: string;
begin
  if not Model.SeparateGwtUsed then
  begin
    Exit;
  end;

//  NameOfFile := FileName(AFileName);
  FFlowFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  FNameOfFile := NameOfFile;
  for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
  begin
    SpeciesName := '.' + Model.MobileComponents[SpeciesIndex].Name + Extension;
    NameOfFile := ChangeFileExt(AFileName, SpeciesName);
    FInputFileName := NameOfFile;
    FNameOfFile := NameOfFile;
    WriteToGwtNameFile('FMI6', FNameOfFile, SpeciesIndex);

    OpenFile(FNameOfFile);
    try
      WriteCommentLine(File_Comment('FMI6'));
      WriteOptions;
      WritePackageData;
    finally
      CloseFile;
    end;
  end;

end;

procedure TModflowFmiWriter.WriteOptions;
var
  GwtProcess: TGwtProcess;
begin
  WriteBeginOptions;
  try
    WriteSaveFlowsOption;
    GwtProcess := Model.ModflowPackages.GwtProcess;
    if GwtProcess.FLOW_IMBALANCE_CORRECTION then
    begin
      WriteString('  FLOW_IMBALANCE_CORRECTION');
      NewLine;
    end;
  finally
    WriteEndOptions;
  end;
end;

procedure TModflowFmiWriter.WritePackageData;
var
  GWFlowFileName: string;
begin
  WriteBeginPackageData;
  try
    WriteString('  GWFBUDGET');
    WriteString(' FILEIN ');
    GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.cbc'));
    WriteString(GWFlowFileName);
    NewLine;

    WriteString('  GWFHEAD');
    WriteString(' FILEIN ');
    GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.bhd'));
    WriteString(GWFlowFileName);
    NewLine;

    if Model.ModflowPackages.MvrPackage.IsSelected then
    begin
      WriteString('  GWFMOVER');
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrMvrbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.LakMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrLakeFlowPackageName);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrLkbud));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrSfrFlowPackageName);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrSfrbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.MawPackage.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrMAW1);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrMawbud));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.UzfMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(KUZF1);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrUzfbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;
  finally
    WriteEndPackageData;
  end;
end;

end.
