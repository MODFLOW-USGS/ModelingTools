unit ModflowFMI_WriterUnit;

interface

uses
  ModflowOptionsUnit, CustomModflowWriterUnit, System.SysUtils;

type
  TCustomFmiWriter = class(TCustomModflowWriter)
  protected
    FFlowFile: string;
    procedure WriteGwfBudget;
    procedure WriteGwfHead;
    procedure WriteGwfGrid;
    class function Extension: string; override;
  end;

  TModflowFmiWriterGwtGwe = class(TCustomFmiWriter)
  private
    procedure WriteOptions;
    procedure WritePackageData;
  public
    procedure WriteFile(const AFileName: string);
  end;

  TModflowFmiWriterPrt = class(TCustomFmiWriter)
  private
    procedure WritePackageData;
  public
    procedure WriteFile(const AFileName: string);

  end;

implementation

uses
  ModflowPackageSelectionUnit, ModflowLakMf6WriterUnit, ModflowMawWriterUnit,
  ModflowSfr6WriterUnit, ModflowUzfMf6WriterUnit, ModflowMvrWriterUnit,
  PhastModelUnit;

{ TModflowFmiWriterGwtGwe }

class function TCustomFmiWriter.Extension: string;
begin
  result := '.fmi';
end;

procedure TModflowFmiWriterGwtGwe.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  SpeciesIndex: Integer;
  SpeciesName: string;
begin
  if not Model.SeparateGwtUsed and not Model.SeparateGweUsed then
  begin
    Exit;
  end;

  FFlowFile := FileName(AFileName);
  FInputFileName := FFlowFile;
  FNameOfFile := FFlowFile;
  for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
  begin
    if not (Model.MobileComponents[SpeciesIndex].UsedForGWT
     or Model.MobileComponents[SpeciesIndex].UsedForGWE) then
    begin
      Continue;
    end;
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

procedure TModflowFmiWriterGwtGwe.WriteOptions;
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

procedure TModflowFmiWriterGwtGwe.WritePackageData;
var
  AFileName: string;
begin
  WriteBeginPackageData;
  try
    WriteGwfBudget;
    WriteGwfHead;
    WriteGwfGrid;

    if Model.ModflowPackages.MvrPackage.IsSelected then
    begin
      WriteString('  GWFMOVER');
      WriteString(' FILEIN ');
      AFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrMvrbudget));
      WriteString(AFileName);
      NewLine;
    end;

    if Model.ModflowPackages.LakMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrLakeFlowPackageName);
      WriteString(' FILEIN ');
      AFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrLkbud));
      WriteString(AFileName);
      NewLine;
    end;

    if Model.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrSfrFlowPackageName);
      WriteString(' FILEIN ');
      AFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrSfrbudget));
      WriteString(AFileName);
      NewLine;
    end;

    if Model.ModflowPackages.MawPackage.IsSelected then
    begin
      WriteString('  ');
      WriteString(StrMAW1);
      WriteString(' FILEIN ');
      AFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrMawbud));
      WriteString(AFileName);
      NewLine;
    end;

    if Model.ModflowPackages.UzfMf6Package.IsSelected then
    begin
      WriteString('  ');
      WriteString(KUZF1);
      WriteString(' FILEIN ');
      AFileName := ExtractFileName(ChangeFileExt(FFlowFile, StrUzfbudget));
      WriteString(AFileName);
      NewLine;
    end;
  finally
    WriteEndPackageData;
  end;
end;

procedure TCustomFmiWriter.WriteGwfBudget;
var
  GWFlowFileName: string;
begin
  WriteString('  GWFBUDGET');
  WriteString(' FILEIN ');
  GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.cbc'));
  WriteString(GWFlowFileName);
  NewLine;
end;

procedure TCustomFmiWriter.WriteGwfHead;
var
  GWFlowFileName: string;
begin
  WriteString('  GWFHEAD');
  WriteString(' FILEIN ');
  GWFlowFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.bhd'));
  WriteString(GWFlowFileName);
  NewLine;
end;

procedure TCustomFmiWriter.WriteGwfGrid;
var
  GrbFileName: string;
begin
  if Model.ModflowOptions.WriteBinaryGridFile then
  begin
    { TODO : Allow printing GWFGRID FILEIN this in MODFLOW 6.9 }
    WriteString('# In MODFLOW vesion 6.8 and 6.8.1 the GRB file isn''t read correctly');
    NewLine;
    WriteString('# in GWT or GWE models. It can be read correctly in PRT models.');
    NewLine;
    WriteString('#');

    WriteString('  GWFGRID');
    WriteString(' FILEIN ');
    if Model.DisvUsed then
    begin
      GrbFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.disv.grb'));
    end
    else
    begin
      GrbFileName := ExtractFileName(ChangeFileExt(FFlowFile, '.dis.grb'));
    end;
    WriteString(GrbFileName);
    NewLine;
  end;
end;

{ TModflowFmiWriterPrt }

procedure TModflowFmiWriterPrt.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  PrtModel: TPrtModel;
  ModelName: String;
begin
//  if not Model.SeparatePrtUsed then
//  begin
//    Exit;
//  end;

//  NameOfFile := FileName(AFileName);
  FFlowFile := FileName(AFileName);
  FInputFileName := FFlowFile;
  FNameOfFile := FFlowFile;
  for var PrtIndex := 0 to Model.ModflowPackages.PrtModels.Count - 1 do
  begin
    PrtModel := Model.ModflowPackages.PrtModels[PrtIndex].PrtModel;
    if PrtModel.IsSelected and PrtModel.RunAsSeparateSimulation then
    begin

      ModelName := '.' + PrtModel.ModelName + Extension;
      NameOfFile := ChangeFileExt(AFileName, ModelName);
      FInputFileName := NameOfFile;
      FNameOfFile := NameOfFile;
      WriteToPrtNameFile('FMI6', FNameOfFile, PrtModel, PrtModel.ModelName);

      OpenFile(FNameOfFile);
      try
        WriteCommentLine(File_Comment('FMI6'));
        WritePackageData;
      finally
        CloseFile;
      end;
    end;
  end;

end;

procedure TModflowFmiWriterPrt.WritePackageData;
begin
  WriteBeginPackageData;
  try
    WriteGwfBudget;
    WriteGwfHead;
    WriteGwfGrid;
  finally
    WriteEndPackageData;
  end;
end;

end.
