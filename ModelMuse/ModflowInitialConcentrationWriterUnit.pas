unit ModflowInitialConcentrationWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit;

type
  TGwtInitialConcWriter = class(TCustomModflowWriter)
  private
    FSpeciesIndex: Integer;
  protected
    class function Extension: string; override;
    procedure WriteInitialConcentrations;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
  end;


implementation

uses
  DataSetUnit, Mt3dmsChemSpeciesUnit, GoPhastTypes;

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
  FSpeciesIndex := SpeciesIndex;
  SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  AGwtFileName := GwtFileName(AFileName, SpeciesIndex);

  WriteToGwtNameFile('IC6', AGwtFileName, SpeciesIndex);

  FNameOfFile := AGwtFileName;
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
  LayerIndex: Integer;
  DataSetName: string;
begin
  Item := Model.MobileComponents[FSpeciesIndex];
  DataArray := Model.DataArrayManager.GetDataSetByName(
    Item.InitialConcDataArrayName);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  DataSetName := 'STRT';

  WriteBeginGridData;

  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    if LayerIndex = 0 then
    begin
      WriteArray(DataArray, LayerIndex,
        DataSetName + ' ' + Model.ModflowLayerBottomDescription(LayerIndex),
        StrNoValueAssigned, DataSetName);
    end
    else
    begin
      WriteArray(DataArray, LayerIndex,
        DataSetName + ' ' + Model.ModflowLayerBottomDescription(LayerIndex),
        StrNoValueAssigned, '');
    end;
  end;

  WriteEndGridData;
end;

end.
