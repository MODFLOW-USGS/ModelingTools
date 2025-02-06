unit ModflowInitialConcentrationWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit;

type
  TGwtInitialConcWriter = class(TCustomModflowWriter)
  private
    FSpeciesIndex: Integer;
    FPestScriptFileName: string;
  protected
    class function Extension: string; override;
    procedure WriteOptions;
    procedure WriteInitialConcentrations;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
  end;


implementation

uses
  DataSetUnit, Mt3dmsChemSpeciesUnit, GoPhastTypes, DataSetNamesUnit;

{ TGwtInitialConcWriter }

class function TGwtInitialConcWriter.Extension: string;
begin
  result := '.ic';
end;

procedure TGwtInitialConcWriter.WriteFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  AGwtFileName: string;
  SpeciesName: string;
begin
  if not Model.GwtUsed then
  begin
    Exit;
  end;
  FPestScriptFileName := AFileName;
  FSpeciesIndex := SpeciesIndex;
  SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  AGwtFileName := GwtFileName(AFileName, SpeciesIndex);

  WriteToGwtNameFile('IC6', AGwtFileName, SpeciesIndex);

  FNameOfFile := AGwtFileName;
  FInputFileName := AGwtFileName;
  OpenFile(FNameOfFile);
  try
    WriteCommentLine(File_Comment('Initial concentration file for ' + SpeciesName));
    WriteInitialConcentrations;
  finally
    CloseFile;
  end;
end;

procedure TGwtInitialConcWriter.WriteInitialConcentrations;
var
  Item: TMobileChemSpeciesItem;
  DataArray: TDataArray;
  DataSetName: string;
begin
  Item := Model.MobileComponents[FSpeciesIndex];
  DataArray := Model.DataArrayManager.GetDataSetByName(
    Item.InitialConcDataArrayName);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  DataSetName := 'STRT';

  WriteBeginGridData;

  WriteMf6_DataSet(DataArray, 'STRT');
  WritePestZones(DataArray, FPestScriptFileName, 'STRT', '.' + Item.Name, 'STRT');

  WriteEndGridData;
end;

procedure TGwtInitialConcWriter.WriteOptions;
begin
  WriteBeginOptions;
  try
    WriteExportAsciiArray;
  finally
    WriteEndOptions;
  end;
end;

end.
