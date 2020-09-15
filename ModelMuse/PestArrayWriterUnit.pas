unit PestArrayWriterUnit;

interface

uses
  DataSetUnit, System.SysUtils, RbwParser, GoPhastTypes,
  CustomModflowWriterUnit, OrderedCollectionUnit, System.Classes,
  System.Generics.Collections;

type
  TPestDataArrayWriter = class(TCustomFileWriter)
  private
    FDataSetList: TList<TDataArray>;
    function ListName(DataArray: TDataArray; Layer: Integer): string;
    procedure WriteCoordinateList(FileName: string;
      EvalAt: TEvaluatedAt; out CListName, CListFileName: string);
    procedure WritePestSelectionArray(DataArray: TDataArray; FileName: string;
      out SListFileName: string);
    procedure WritePestMultiplierArray(DataArray: TDataArray; FileName: string;
      out PListFileName: string);
  protected
    class function Extension: string; override;
  public
    procedure WriteParamTypeArrays(ParameterTypes: TParameterTypes;
      FileName: string);
  end;

implementation

uses
  PhastModelUnit, AbstractGridUnit, MeshRenumberingTypes, FastGEO,
  ModflowParameterUnit, System.IOUtils;

const
  StrArrays = 'arrays';

procedure TPestDataArrayWriter.WriteParamTypeArrays(
  ParameterTypes: TParameterTypes; FileName: string);
const
  CoordListName = 'cl1';
var
  ParamList: TList<TModflowSteadyParameter>;
  ParamTypeIndex: Integer;
  AParam: TModflowSteadyParameter;
  AParamType: TParameterType;
  CListName: string;
  CListFileName: string;
  ZoneDataArray: TDataArray;
  SListFileName: string;
  MultDataArray: TDataArray;
  PListFileName: string;
  Script: TStringList;
  Equations: TStringList;
  ScalarFile: TStringList;
  SListFileNames: TStringList;
  PListFileNames: TStringList;
  LayerIndex: Integer;
  DataName: string;
  ZoneDataSets: TList<TDataArray>;
  MultiplierDataSets: TList<TDataArray>;
  LayerCount: Integer;
  Equation: string;
  PIndex: Integer;
  Directory: string;
  ExportDataSets: TStringList;
  DataToExportName: string;
  BaseName: string;
  FinalScript: TStringList;
  ScriptFileName: string;
  ArrayDir: string;
  CreateList: string;
  ScalarFileName: string;
  ScalarLine: string;
  ScalarFileTemplate: TStringList;
  TemplateChar: Char;
  function ReplacementString(AParmName: string): string;
  begin
    Result := TemplateChar + AParmName;
    while Length(Result) < 14 do
    begin
      Result := Result + ' ';
    end;
    result := Result + TemplateChar;
  end;
  procedure ReadCoordinates;
  begin
    Script.Add('# Read coordinates of grid or mesh.');
    Script.Add(CoordListName + ' = read_list_file(skiplines=1,dimensions=2,  &');
    Script.Add('  id_type=''indexed'',  &');
    Script.Add('  file=''' + CListFileName + ''')');
  end;
begin
  TemplateChar := Model.PestProperties.TemplateCharacter;
  Directory := ExtractFileDir(FileName);
  Assert(TDirectory.Exists(Directory));
  ArrayDir := IncludeTrailingPathDelimiter(Directory) + StrArrays;
  if not TDirectory.Exists(ArrayDir) then
  begin
    TDirectory.CreateDirectory(ArrayDir);
  end;

  BaseName := ChangeFileExt(ExtractFileName(FileName), '');
  CListName := '';
  CListFileName := '';
  Script := TStringList.Create;
  ScalarFile := TStringList.Create;
  ScalarFileTemplate := TStringList.Create;
  Equations := TStringList.Create;
  SListFileNames := TStringList.Create;
  PListFileNames := TStringList.Create;
  ZoneDataSets := TList<TDataArray>.Create;
  MultiplierDataSets := TList<TDataArray>.Create;
  ParamList := TList<TModflowSteadyParameter>.Create;
  ExportDataSets := TStringList.Create;
  FDataSetList := TList<TDataArray>.Create;
  try
    ScalarFileTemplate.Add('ptf '+ TemplateChar);
    LayerCount := 0;
    for AParamType in ParameterTypes do
    begin
      ParamList.Clear;
      ZoneDataSets.Clear;
      MultiplierDataSets.Clear;
      for ParamTypeIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
      begin
        AParam := Model.ModflowSteadyParameters[ParamTypeIndex];
        if AParam.ParameterType = AParamType then
        begin
          ParamList.Add(AParam);
        end;
      end;
      for ParamTypeIndex := 0 to ParamList.Count - 1 do
      begin
        AParam := ParamList[ParamTypeIndex];
        ScalarFile.Add(AParam.ParameterName + ' ' + AParam.Value.ToString);
        ScalarFileTemplate.Add(AParam.ParameterName + ' '
          + ReplacementString(AParam.ParameterName));

        if AParam.UseZone then
        begin
          ZoneDataArray := Model.DataArrayManager.
            GetDataSetByName(AParam.ZoneName);
          FDataSetList.Add(ZoneDataArray);
          if CListName = '' then
          begin
            LayerCount := ZoneDataArray.LayerCount;
            WriteCoordinateList(FileName, ZoneDataArray.EvaluatedAt,
              CListName, CListFileName);
            ReadCoordinates;
          end;
          WritePestSelectionArray(ZoneDataArray, FileName, SListFileName);

          Script.Add('');
          Script.Add('# Read Zone arrays for ' + AParam.ParameterName);
          Script.Add('read_list_file('
            + 'reference_clist=' + CoordListName + ',skiplines=1,dimensions=2,  &');
          for LayerIndex := 0 to ZoneDataArray.LayerCount - 1 do
          begin
            Script.Add('  slist=' + ListName(ZoneDataArray, LayerIndex)
              + ';column=' + (LayerIndex+2).ToString + ',  &');
          end;
          Script.Add('  id_type=''indexed'',  &');
          Script.Add('  file=''' + SListFileName + ''')');
        end
        else
        begin
          ZoneDataArray := nil;
          SListFileName := '';
        end;
        SListFileNames.Add(SListFileName);
        ZoneDataSets.Add(ZoneDataArray);

        if AParam.UseMultiplier then
        begin
          MultDataArray := Model.DataArrayManager.
            GetDataSetByName(AParam.MultiplierName);
          FDataSetList.Add(MultDataArray);
          if CListName = '' then
          begin
            LayerCount := MultDataArray.LayerCount;
            WriteCoordinateList(FileName, MultDataArray.EvaluatedAt,
              CListName, CListFileName);
            ReadCoordinates;
          end;
          WritePestMultiplierArray(MultDataArray, FileName, PListFileName);

          Script.Add('');
          Script.Add('# Read Multiplier arrays for ' + AParam.ParameterName);
          Script.Add('read_list_file('
            + 'reference_clist=' + CoordListName + ',skiplines=1,dimensions=2,  &');
          for LayerIndex := 0 to MultDataArray.LayerCount - 1 do
          begin
            Script.Add('  plist=' + ListName(MultDataArray, LayerIndex)
              + ';column=' + (LayerIndex+2).ToString + ',  &');
          end;
          Script.Add('  id_type=''indexed'',  &');
          Script.Add('  file=''' + PListFileName + ''')');
        end
        else
        begin
          PListFileName := '';
          MultDataArray := nil;
        end;
        PListFileNames.Add(PListFileName);
        MultiplierDataSets.Add(MultDataArray);
      end;

      case AParamType of
        ptUndefined: ;
        ptLPF_HK: DataName := 'HK';
        ptLPF_HANI: DataName := 'HANI';
        ptLPF_VK: DataName := 'VK';
        ptLPF_VANI: DataName := 'VANI';
        ptLPF_SS: DataName := 'SS';
        ptLPF_SY: DataName := 'SY';
        ptLPF_VKCB: DataName := 'VKCB';
        ptRCH: ;
        ptEVT: ;
        ptETS: ;
        ptCHD: ;
        ptGHB: ;
        ptQ: ;
        ptRIV: ;
        ptDRN: ;
        ptDRT: ;
        ptSFR: ;
        ptHFB: ;
        ptHUF_HK: ;
        ptHUF_HANI: ;
        ptHUF_VK: ;
        ptHUF_VANI: ;
        ptHUF_SS: ;
        ptHUF_SY: ;
        ptHUF_SYTP: ;
        ptHUF_KDEP: ;
        ptHUF_LVDA: ;
        ptSTR: ;
        ptQMAX: ;
      end;

      for LayerIndex := 0 to LayerCount - 1 do
      begin
        if ParamList.Count > 0 then
        begin
          DataToExportName := DataName+ '_' + (LayerIndex+1).ToString;
          CreateList := DataToExportName + ' = new_plist(reference_clist=' + CoordListName +  ',value=0)';

          ExportDataSets.Add(Format('write_column_data_file(file=''%0:s.%1:s'', plist=%2:s)',
            [BaseName, DataToExportName, DataToExportName]));

          Equation := DataToExportName + ' =';
          PIndex := 0;
          for ParamTypeIndex := 0 to ParamList.Count - 1 do
          begin
            AParam := ParamList[ParamTypeIndex];
            if ParamTypeIndex > 0 then
            begin
              Equation := Equation + ' + ';
            end;
            Equation := Equation + ' ' + AParam.ParameterName;
            if AParam.UseMultiplier then
            begin
              MultDataArray := MultiplierDataSets[PIndex];
              Assert(MultDataArray <> nil);
              Equation := Equation + '*' + ListName(MultDataArray,LayerIndex);
            end;
            if AParam.UseZone then
            begin
              ZoneDataArray := ZoneDataSets[PIndex];
              Assert(ZoneDataArray <> nil);
              Equation := Equation + '*' + ListName(ZoneDataArray,LayerIndex);
            end;
            Inc(PIndex);
          end;

          Equations.Add(CreateList);
          Equations.Add(Equation);
        end;
      end;
    end;

    ScalarFileName := IncludeTrailingPathDelimiter(ArrayDir) + BaseName + '.ParamValues';
    ScalarFile.SaveToFile(ScalarFileName);
    ScalarFileTemplate.SaveToFile(ScalarFileName + '.tpl');
    ScalarLine := Format('read_scalar_file(file="%s", valuecolumn=2, namecolumn=1)', [ScalarFileName]);

    FinalScript := TStringList.Create;
    try
      FinalScript.Add(ScalarLine);
      FinalScript.AddStrings(Script);
      FinalScript.AddStrings(Equations);
      FinalScript.AddStrings(ExportDataSets);

      ScriptFileName := IncludeTrailingPathDelimiter(ArrayDir)
        + BaseName + '.script';
      FinalScript.SaveToFile(ScriptFileName);
    finally
      FinalScript.Free;
    end;
  finally
    FDataSetList.Free;
    ExportDataSets.Free;
    ParamList.Free;
    Script.Free;
    Equations.Free;
    ScalarFile.Free;
    ScalarFileTemplate.Free;
    SListFileNames.Free;
    PListFileNames.Free;
    ZoneDataSets.Free;
    MultiplierDataSets.Free;
  end;
end;

procedure TPestDataArrayWriter.WritePestMultiplierArray(DataArray: TDataArray;
  FileName: string; out PListFileName: string);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ListIndex: Integer;
  Directory: string;
  BaseName: string;
begin
  Directory := ExtractFileDir(FileName);
  BaseName := ExtractFileName(FileName);
  BaseName := ChangeFileExt(BaseName, '');
  Assert(DataArray.DataType = rdtDouble);
  PListFileName := IncludeTrailingPathDelimiter(Directory) + StrArrays;
  PListFileName := IncludeTrailingPathDelimiter(PListFileName)
    + BaseName + '.' + DataArray.Name + '.plist';
  OpenFile(PListFileName);
  try
    WriteString('pp');
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      WriteString(' ');
      WriteString(ListName(DataArray,LayerIndex));
    end;
    NewLine;

    ListIndex := 1;
    for RowIndex := 0 to DataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to DataArray.ColumnCount - 1 do
      begin
        WriteInteger(ListIndex);
        Inc(ListIndex);
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          WriteFloat(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
        end;
        NewLine;
      end;
    end;
  finally
    CloseFile;
  end;
end;

procedure TPestDataArrayWriter.WritePestSelectionArray(DataArray: TDataArray;
  FileName: string; out SListFileName: string);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ListIndex: Integer;
  Directory: string;
  BaseName: string;
begin
  Directory := ExtractFileDir(FileName);
  BaseName := ExtractFileName(FileName);
  BaseName := ChangeFileExt(BaseName, '');
  Assert(DataArray.DataType = rdtBoolean);
  SListFileName := IncludeTrailingPathDelimiter(Directory) + StrArrays;
  SListFileName := IncludeTrailingPathDelimiter(SListFileName)
    + BaseName + '.' + DataArray.Name + '.slist';
  OpenFile(SListFileName);
  try
    WriteString('pp');
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      WriteString(' ');
      WriteString(ListName(DataArray,LayerIndex));
    end;
    NewLine;

    ListIndex := 1;
    for RowIndex := 0 to DataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to DataArray.ColumnCount - 1 do
      begin
        WriteInteger(ListIndex);
        Inc(ListIndex);
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          WriteInteger(Ord(DataArray.BooleanData[LayerIndex, RowIndex, ColIndex]));
        end;
        NewLine;
      end;
    end;
  finally
    CloseFile;
  end;
end;

class function TPestDataArrayWriter.Extension: string;
begin
  result := '.tpl';
end;

function TPestDataArrayWriter.ListName(DataArray: TDataArray;
  Layer: Integer): string;
var
  ListIndex: Integer;
begin
  ListIndex := FDataSetList.IndexOf(DataArray);
  Assert(ListIndex >=0);
  if DataArray.LayerCount > 1 then
  begin
    result := Format('List%0:d_L_%1:d', [ListIndex+1, Layer+1]);
  end
  else
  begin
    result := Format('List%0:d', [ListIndex+1]);
  end;
end;

procedure TPestDataArrayWriter.WriteCoordinateList(
  FileName: string; EvalAt: TEvaluatedAt; out CListName, CListFileName: string);
var
  LocalModel: TCustomModel;
  Grid: TCustomModelGrid;
  Mesh: IMesh2D;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint: TPoint2D;
  ListIndex: Integer;
  ElementIndex: Integer;
  NodeIndex: Integer;
  Directory: String;
  BaseName: string;
  procedure WriteAPoint;
  begin
    WriteInteger(ListIndex);
    WriteFloat(APoint.x);
    WriteFloat(APoint.y);
    NewLine;
    Inc(ListIndex);
  end;
begin
  Directory := ExtractFileDir(FileName);
  BaseName := ExtractFileName(FileName);
  BaseName := ChangeFileExt(BaseName, '');
  CListName := BaseName;
  case EvalAt of
    eaBlocks: CListName := CListName + '.Blocks';
    eaNodes: CListName := CListName + '_.Nodes';
    else Assert(False);
  end;

  CListFileName := IncludeTrailingPathDelimiter(Directory) + StrArrays;
  CListFileName := IncludeTrailingPathDelimiter(CListFileName) + CListName + '.clist';

  OpenFile(CListFileName);
  try
    ListIndex := 1;

    WriteString('pp    x    y');
    NewLine;

    LocalModel := Model;
    Grid := LocalModel.Grid;
    if Grid <> nil then
    begin
      Mesh := nil;
      case EvalAt of
        eaBlocks:
          for RowIndex := 0 to Grid.RowCount - 1 do
          begin
            for ColIndex := 0 to Grid.ColumnCount - 1 do
            begin
              APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
              WriteAPoint;
            end;
          end;
        eaNodes:
          for RowIndex := 0 to Grid.RowCount do
          begin
            for ColIndex := 0 to Grid.ColumnCount do
            begin
              APoint := Grid.TwoDElementCorner(ColIndex, RowIndex);
              WriteAPoint;
            end;
          end;
        else Assert(False)
      end;
    end
    else
    begin
      Mesh := LocalModel.Mesh3D.Mesh2DI;
      case EvalAt of
        eaBlocks:
          begin
            for ElementIndex := 0 to Mesh.ElementCount- 1 do
            begin
              APoint := Mesh.ElementsI2D[ElementIndex].Center;
              WriteAPoint;
            end;
          end;
        eaNodes:
          begin
            for NodeIndex := 0 to Mesh.NodeCount - 1 do
            begin
              APoint := Mesh.NodesI2D[NodeIndex].Location;
              WriteAPoint;
            end;
          end;
        else Assert(False)
      end;
    end;
  finally
    CloseFile;
  end;
end;

end.
