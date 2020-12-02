unit PestPilotPointFileWriterUnit;

interface

uses
  System.Classes, System.SysUtils, PhastModelUnit, CustomModflowWriterUnit,
  DataSetUnit, System.Generics.Collections, ModflowParameterUnit, GoPhastTypes;

type
  TPilotPointFileObject = class(TObject)
  private
    FFileName: string;
    FDataArray: TDataArray;
    FParameter: TModflowSteadyParameter;
    FLayer: Integer;
    FParameterIndex: Integer;
    procedure SetDataArray(const Value: TDataArray);
    procedure SetFileName(const Value: string);
    procedure SetParameter(const Value: TModflowSteadyParameter);
    procedure SetLayer(const Value: Integer);
    procedure SetParameterIndex(const Value: Integer);
  public
   property DataArray: TDataArray read FDataArray write SetDataArray;
   property Parameter: TModflowSteadyParameter read FParameter write SetParameter;
   property ParameterIndex: Integer read FParameterIndex write SetParameterIndex;
   property FileName: string read FFileName write SetFileName;
   property Layer: Integer read FLayer write SetLayer;
  end;

  TPilotPointFiles = TObjectList<TPilotPointFileObject>;

  TPilotPointWriter = class(TCustomFileWriter)
  private
    FFileName: string;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; DataArray: TDataArray;
      PilotPointFiles: TPilotPointFiles);
  end;


implementation

uses
  QuadTreeClass, FastGEO, PestPropertiesUnit;

{ TPilotPointWriter }

class function TPilotPointWriter.Extension: string;
begin
  result := '.pp';
end;

procedure TPilotPointWriter.WriteFile(AFileName: string; DataArray: TDataArray;
  PilotPointFiles: TPilotPointFiles);
var
  ParamList: TList<TModflowSteadyParameter>;
  ParamIndex: Integer;
  AParam: TModflowSteadyParameter;
  QuadTreeList: TList<TRbwQuadTree>;
  DisLimits: TGridLimit;
  AQuadTree: TRbwQuadTree;
  ParamQuadDictionary: TDictionary<TModflowSteadyParameter, TRbwQuadTree>;
  ParamNameDictionary: TDictionary<string, TModflowSteadyParameter>;
  LayerIndex: Integer;
  ParamNameDataArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  ParamName: String;
  APoint: TPoint2D;
  Values: TOneDRealArray;
  ValueIndex: Integer;
  PestProperties: TPestProperties;
  PilotPointIndex: Integer;
  APilotPoint: TPoint2D;
  ACell: T2DTopCell;
  ValuePointer: PDouble;
  QuadTreeIndex: Integer;
  Value: Double;
  FileProperties: TPilotPointFileObject;
begin

  Assert(DataArray <> nil);
  Assert(DataArray.PestParametersUsed);
  PestProperties := Model.PestProperties;
  FFileName := ChangeFileExt(AFileName, DataArray.Name);// + Extension;
  DisLimits := Model.DiscretizationLimits(vdTop);
  ParamList := TList<TModflowSteadyParameter>.Create;
  QuadTreeList := TList<TRbwQuadTree>.Create;
  ParamQuadDictionary := TDictionary<TModflowSteadyParameter, TRbwQuadTree>.Create;
  ParamNameDictionary := TDictionary<string, TModflowSteadyParameter>.Create;
  try                                                                      
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[ParamIndex];
      if AParam.UsePilotPoints then
      begin
        ParamList.Add(AParam);
        ParamNameDictionary.Add(UpperCase(AParam.ParameterName), AParam);
        AQuadTree := TRbwQuadTree.Create(nil);
        QuadTreeList.Add(AQuadTree);
        AQuadTree.XMax := DisLimits.MaxX;
        AQuadTree.YMax := DisLimits.MaxY;
        AQuadTree.XMin := DisLimits.MinX;
        AQuadTree.YMin := DisLimits.MinY;
        ParamQuadDictionary.Add(AParam, AQuadTree);
      end;
    end;
    ParamNameDataArray := Model.DataArrayManager.GetDataSetByName(
      DataArray.ParamDataSetName);
    SetLength(Values, DataArray.RowCount * DataArray.ColumnCount);
     
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      for QuadTreeIndex := 0 to QuadTreeList.Count - 1 do
      begin
        AQuadTree := QuadTreeList[QuadTreeIndex];
        AQuadTree.Clear;
      end;

      ValueIndex := 0;
      for RowIndex := 0 to DataArray.RowCount - 1 do
      begin
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          ParamName := UpperCase(
            ParamNameDataArray.StringData[LayerIndex, RowIndex, ColIndex]);
          if ParamNameDictionary.TryGetValue(ParamName, AParam) then
          begin
            APoint := Model.ItemTopLocation[DataArray.EvaluatedAt,RowIndex, ColIndex];
            Values[ValueIndex] := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
            Assert(ParamQuadDictionary.TryGetValue(AParam, AQuadTree));     
            AQuadTree.AddPoint(APoint.x, APoint.y, Addr(Values[ValueIndex])); 
          end;

          Inc(ValueIndex);
        end;                                         
      end;

      for ParamIndex := 0 to ParamList.Count - 1 do
      begin
        AParam := ParamList[ParamIndex];
        AQuadTree := QuadTreeList[ParamIndex];
        if AQuadTree.Count > 0 then
        begin
//          AFileName := FFileName + '.' + AParam.ParameterName + '.' + IntToStr(LayerIndex+1) + Extension;
          AFileName := Format('%0:s.%1:s.%2:d%3:s',
            [FFileName,AParam.ParameterName,LayerIndex+1,Extension]);

          FileProperties := TPilotPointFileObject.Create;
          PilotPointFiles.Add(FileProperties);
          FileProperties.DataArray := DataArray;
          FileProperties.Parameter := AParam;
          FileProperties.ParameterIndex := ParamIndex+1;
          FileProperties.FileName := AFileName;
          FileProperties.Layer := LayerIndex;

          OpenFile(AFileName);
          try
            ParamNameDataArray := Model.DataArrayManager.GetDataSetByName(
              DataArray.ParamDataSetName);
            for PilotPointIndex := 0 to PestProperties.PilotPointCount - 1 do
            begin
              APilotPoint := PestProperties.PilotPoints[PilotPointIndex];
              ACell := Model.PointToCell(DataArray.EvaluatedAt, APilotPoint);
              if (ACell.Col >= 0) and (ACell.Row >= 0) then
              begin
                ParamName := UpperCase(ParamNameDataArray.StringData[
                  LayerIndex, ACell.Row, ACell.Col]);
                if UpperCase(AParam.ParameterName) = ParamName then
                begin
                  Value := DataArray.RealData[LayerIndex, ACell.Row, ACell.Col]
                end
                else
                begin
                  ValuePointer := AQuadTree.NearestPointsFirstData(
                    APilotPoint.x, APilotPoint.y);
                  Value := ValuePointer^;
                end;
              end
              else
              begin
                ValuePointer := AQuadTree.NearestPointsFirstData(
                  APilotPoint.x, APilotPoint.y);
                Value := ValuePointer^;
              end;

              WriteInteger(PilotPointIndex + 1);
              WriteFloat(APilotPoint.x);
              WriteFloat(APilotPoint.y);
              WriteInteger(ParamIndex + 1);
              WriteFloat(Value);
              NewLine;

            end;
          finally
            CloseFile;
          end;

        end;
      end;                             
    end;
  finally    
    ParamList.Free;
    QuadTreeList.Free;
    ParamNameDictionary.Free;
    ParamQuadDictionary.Free;
  end;      
end;

{ TPilotPointFileObject }

procedure TPilotPointFileObject.SetDataArray(const Value: TDataArray);
begin
  FDataArray := Value;
end;

procedure TPilotPointFileObject.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TPilotPointFileObject.SetLayer(const Value: Integer);
begin
  FLayer := Value;
end;

procedure TPilotPointFileObject.SetParameter(
  const Value: TModflowSteadyParameter);
begin
  FParameter := Value;
end;

procedure TPilotPointFileObject.SetParameterIndex(const Value: Integer);
begin
  FParameterIndex := Value;
end;

end.
