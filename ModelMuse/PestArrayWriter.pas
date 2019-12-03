unit PestArrayWriter;

interface

uses
  DataSetUnit, System.SysUtils, RbwParser, GoPhastTypes,
  CustomModflowWriterUnit, OrderedCollectionUnit, System.Classes;

type
  TPestDataArrayWriter = class(TCustomFileWriter)
  private
    function ListName(DataArray: TDataArray; Layer: Integer): string;
  public
    procedure WriteCoordinateList(Directory: string;
      EvalAt: TEvaluatedAt; out CListName, CListFileName: string);
    procedure WritePestSelectionArray(DataArray: TDataArray; Directory: string;
      out SListFileName: string);
    procedure WritePestMultiplierArray(DataArray: TDataArray; Directory: string;
      out PListFileName: string);
    procedure WriteParamTypeArrays(ParameterTypes: TParameterTypes;
      Directory: string);
  end;

implementation

uses
  PhastModelUnit, AbstractGridUnit, MeshRenumberingTypes, FastGEO,
  System.Generics.Collections, ModflowParameterUnit;

procedure TPestDataArrayWriter.WriteParamTypeArrays(
  ParameterTypes: TParameterTypes; Directory: string);
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
  EvalAt: TEvaluatedAt;
  ZoneDataSets: TList<TDataArray>;
  MultiplierDataSets: TList<TDataArray>;
  LayerCount: Integer;
  Equation: string;
  PIndex: Integer;
  procedure ReadCoordinates;
  begin
    Script.Add(CoordListName + ' = read_list_file(skiplines=1,dimensions=2,  &');
    Script.Add('  id_type=''indexed''  &');
    Script.Add('  file=' + CListFileName + ')');
  end;
begin
  CListName := '';
  CListFileName := '';
  Script := TStringList.Create;
  ScalarFile := TStringList.Create;
  Equations := TStringList.Create;
  SListFileNames := TStringList.Create;
  PListFileNames := TStringList.Create;
  ZoneDataSets := TList<TDataArray>.Create;
  MultiplierDataSets := TList<TDataArray>.Create;
  ParamList := TList<TModflowSteadyParameter>.Create;
  try
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

        if AParam.UseZone then
        begin
          ZoneDataArray := Model.DataArrayManager.
            GetDataSetByName(AParam.ZoneName);
          if CListName = '' then
          begin
            LayerCount := ZoneDataArray.LayerCount;
            WriteCoordinateList(Directory, ZoneDataArray.EvaluatedAt,
              CListName, CListFileName);
            ReadCoordinates;
          end;
          WritePestSelectionArray(ZoneDataArray, Directory, SListFileName);

          Script.Add('read_list_file('
            + 'reference_clist=' + CoordListName + ',skiplines=1,dimensions=2,  &');
          for LayerIndex := 0 to ZoneDataArray.LayerCount - 1 do
          begin
            Script.Add('  slist=' + ListName(ZoneDataArray, LayerIndex)
              + ';column=' + (7+1).ToString + ',  &');
          end;
          Script.Add('  id_type=''indexed''  &');
          Script.Add('  file=' + SListFileName + ')');
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
          if CListName = '' then
          begin
            LayerCount := MultDataArray.LayerCount;
            WriteCoordinateList(Directory, MultDataArray.EvaluatedAt,
              CListName, CListFileName);
            ReadCoordinates;
          end;
          WritePestMultiplierArray(MultDataArray, Directory, PListFileName);

          Script.Add('read_list_file('
            + 'reference_clist=' + CoordListName + ',skiplines=1,dimensions=2,  &');
          for LayerIndex := 0 to MultDataArray.LayerCount - 1 do
          begin
            Script.Add('  plist=' + ListName(MultDataArray, LayerIndex)
              + ';column=' + (7+1).ToString + ',  &');
          end;
          Script.Add('  id_type=''indexed''  &');
          Script.Add('  file=' + PListFileName + ')');
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
        ptLPF_HANI: ;
        ptLPF_VK: ;
        ptLPF_VANI: ;
        ptLPF_SS: ;
        ptLPF_SY: ;
        ptLPF_VKCB: ;
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

      PIndex := 0;
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        Equation := DataName+ '_' + (LayerIndex+1).ToString + ' =';
        for ParamTypeIndex := 0 to ParamList.Count - 1 do
        begin
          AParam := ParamList[ParamTypeIndex];
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
        end;
        Equations.Add(Equation);
      end;
    end;
  finally
    ParamList.Free;
    Script.Free;
    Equations.Free;
    ScalarFile.Free;
    SListFileNames.Free;
    PListFileNames.Free;
    ZoneDataSets.Free;
    MultiplierDataSets.Free;
  end;
end;

procedure TPestDataArrayWriter.WritePestMultiplierArray(DataArray: TDataArray;
  Directory: string; out PListFileName: string);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ListIndex: Integer;
begin
  Assert(DataArray.DataType = rdtDouble);
  PListFileName := IncludeTrailingPathDelimiter(Directory) + 'arrays';
  PListFileName := IncludeTrailingPathDelimiter(PListFileName)
    + DataArray.Name + '.plist';
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
  Directory: string; out SListFileName: string);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ListIndex: Integer;
begin
  Assert(DataArray.DataType = rdtBoolean);
  SListFileName := IncludeTrailingPathDelimiter(Directory) + 'arrays';
  SListFileName := IncludeTrailingPathDelimiter(SListFileName)
    + DataArray.Name + '.slist';
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

function TPestDataArrayWriter.ListName(DataArray: TDataArray;
  Layer: Integer): string;
begin
  result := DataArray.Name + '_L_' + IntToStr(Layer+1)
end;

procedure TPestDataArrayWriter.WriteCoordinateList(
  Directory: string; EvalAt: TEvaluatedAt; out CListName, CListFileName: string);
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
  procedure WriteAPoint;
  begin
    WriteInteger(ListIndex);
    WriteFloat(APoint.x);
    WriteFloat(APoint.y);
    NewLine;
    Inc(ListIndex);
  end;
begin
  CListName := Model.DisplayName;
  case EvalAt of
    eaBlocks: CListName := CListName + '_Blocks';
    eaNodes: CListName := CListName + '_Nodes';
    else Assert(False);
  end;

  CListFileName := IncludeTrailingPathDelimiter(Directory) + 'arrays';
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
