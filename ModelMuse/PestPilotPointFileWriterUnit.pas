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
    procedure SetDataArray(const Value: TDataArray);
    procedure SetFileName(const Value: string);
    procedure SetParameter(const Value: TModflowSteadyParameter);
  public
   property DataArray: TDataArray read FDataArray write SetDataArray;
   property Parameter: TModflowSteadyParameter read FParameter write SetParameter;
   property FileName: string read FFileName write SetFileName;
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
  QuadTreeClass, FastGEO;

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
begin
  Assert(DataArray <> nil);
  Assert(DataArray.PestParametersUsed);
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
//          Model.PointToCell
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

procedure TPilotPointFileObject.SetParameter(
  const Value: TModflowSteadyParameter);
begin
  FParameter := Value;
end;

end.
