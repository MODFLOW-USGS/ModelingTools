unit ModflowFMI_WriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils;

type
  TModflowFmiWriter = class(TCustomModflowWriter)
  private
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
begin
  if not Model.SeparateGwtUsed then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  FNameOfFile := NameOfFile;
  for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
  begin
    WriteToGwtNameFile('FMI6', FNameOfFile, SpeciesIndex);
  end;

  OpenFile(FNameOfFile);
  try
    WriteCommentLine(File_Comment('FMI6'));
    WriteOptions;
    WritePackageData;
  finally
    CloseFile;
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
    GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, '.cbc'));
    WriteString(GWFlowFileName);
    NewLine;

    WriteString('  GWFHEAD');
    WriteString(' FILEIN ');
    GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, '.bhd'));
    WriteString(GWFlowFileName);
    NewLine;

    if Model.ModflowPackages.MvrPackage.IsSelected then
    begin
      WriteString('  GWFMOVER');
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrMvrbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.LakMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrLakeFlowPackageName);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrLkbud));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrSfrFlowPackageName);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrSfrbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.MawPackage.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrMAW1);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrMawbud));
      WriteString(GWFlowFileName);
      NewLine;
    end;

    if Model.ModflowPackages.UzfMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(KUZF1);
      WriteString(' FILEIN ');
      GWFlowFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrUzfbudget));
      WriteString(GWFlowFileName);
      NewLine;
    end;
  finally
    WriteEndPackageData;
  end;
end;

end.
