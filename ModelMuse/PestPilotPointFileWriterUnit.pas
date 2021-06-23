unit PestPilotPointFileWriterUnit;

interface

uses
  System.Classes, System.SysUtils, PhastModelUnit, CustomModflowWriterUnit,
  DataSetUnit, System.Generics.Collections, ModflowParameterUnit, GoPhastTypes,
  PilotPointDataUnit;

type
  TPilotPointWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    FTemplateFileStream: TFileStream;
    procedure OpenTemplateFile(const FileName: string);
    procedure CloseTemplateFile;
    procedure SwitchToTemplate;
    procedure SwitchToMain;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(AFileName: string; DataArray: TDataArray;
      PilotPointFiles: TPilotPointFiles; const DataArrayID: string);
  end;

implementation

uses
  QuadTreeClass, FastGEO, PestPropertiesUnit, frmErrorsAndWarningsUnit,
  SutraMeshUnit, System.IOUtils;

resourcestring
  StrNoPilotPointsDefi = 'No pilot points defined';
  StrPilotPointsWillNo = 'Pilot points will not be used with %0:s because no' +
  ' pilot points have been defined in the "Model|Pest Properties" dialog box' +
  '.';

{ TPilotPointWriter }

procedure TPilotPointWriter.CloseTemplateFile;
begin
  SwitchToMain;
  FreeAndNil(FTemplateFileStream);
end;

class function TPilotPointWriter.Extension: string;
begin
  result := '.pp';
end;

procedure TPilotPointWriter.OpenTemplateFile(const FileName: string);
begin
  Assert(FMainFileStream = nil);
  Assert(FFileStream <> nil);
  FMainFileStream := FFileStream;
  FTemplateFileStream:= TFileStream.Create(FileName + '.tpl', fmCreate or fmShareDenyWrite);
end;

procedure TPilotPointWriter.SwitchToMain;
begin
  FFileStream := FMainFileStream;
end;

procedure TPilotPointWriter.SwitchToTemplate;
begin
  FFileStream := FTemplateFileStream;
end;

procedure TPilotPointWriter.WriteFile(AFileName: string; DataArray: TDataArray;
  PilotPointFiles: TPilotPointFiles; const DataArrayID: string);
var
  ParamList: TList<TModflowSteadyParameter>;
  ParamIndex: Integer;
  AParam: TModflowSteadyParameter;
  QuadTreeList: TObjectList<TRbwQuadTree>;
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
  PointToTest: TPoint2D;
  resultPointer: Pointer;
  CriticalDistance: Double;
  PIndex: Integer;
  ActiveDataSet: TDataArray;
  function IsActive(LayerIndex, RowIndex, ColIndex: Integer): boolean;
  var
    ANode3D: TSutraNode3D;
    AnElement3D: TSutraElement3D;
  begin
    if Model.ModelSelection = msModflow2015 then
    begin
      result := ActiveDataSet.IntegerData[LayerIndex, RowIndex, ColIndex] > 0;
    end
    else if Model.ModelSelection in SutraSelection then
    begin
      if Model.SutraMesh.MeshType <> mt3D then
      begin
        result := True;
      end
      else if DataArray.EvaluatedAt = eaNodes then
      begin
        ANode3D := Model.SutraMesh.NodeArray[LayerIndex, ColIndex];
        result := ANode3D.Active;
      end
      else
      begin
        Assert(DataArray.EvaluatedAt = eaBlocks);
        AnElement3D := Model.SutraMesh.ElementArray[LayerIndex, ColIndex];
        result := AnElement3D.Active;
      end;
    end
    else
    begin
      result := ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
    end;
  end;
begin
  Assert(DataArray <> nil);
  Assert(DataArray.PestParametersUsed);
  PestProperties := Model.PestProperties;
  if Model.PilotPointCount = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrNoPilotPointsDefi,
      Format(StrPilotPointsWillNo, [DataArray.Name]));
    Exit;
  end;
  CriticalDistance := Model.PilotPointBuffer;
  FFileName := ChangeFileExt(AFileName, '.' + DataArray.Name);// + Extension;
  DisLimits := Model.DiscretizationLimits(vdTop);

  ParamList := TList<TModflowSteadyParameter>.Create;
  QuadTreeList := TObjectList<TRbwQuadTree>.Create;
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

    if Model.ModelSelection = msModflow2015 then
    begin
      ActiveDataSet := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    end
    else if Model.ModelSelection in SutraSelection then
    begin
      ActiveDataSet := nil;
    end
    else
    begin
      ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
    end;
//    Assert(ActiveDataSet <> nil);

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
          if IsActive(LayerIndex, RowIndex, ColIndex) then
          begin
            ParamName := UpperCase(
              ParamNameDataArray.StringData[LayerIndex, RowIndex, ColIndex]);
            if ParamNameDictionary.TryGetValue(ParamName, AParam) then
            begin
              APoint := Model.ItemTopLocation[DataArray.EvaluatedAt,ColIndex, RowIndex];
              Values[ValueIndex] := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
              Assert(ParamQuadDictionary.TryGetValue(AParam, AQuadTree));
              AQuadTree.AddPoint(APoint.x, APoint.y, Addr(Values[ValueIndex]));
            end;

            Inc(ValueIndex);
          end;
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
          FileProperties.DataArray := DataArray;
          FileProperties.Parameter := AParam;
          FileProperties.ParameterIndex := ParamIndex+1;
          FileProperties.FileName := AFileName;
          FileProperties.Layer := LayerIndex;
          FileProperties.ParamFamily := Format('Fam%d_',
            [PilotPointFiles.Count+1 + + Model.PilotPointData.Count]);
//          FileProperties.ParamFamily := Format('%0:s_%1:d_%2:d_',
//            [DataArrayID, ParamIndex+1, LayerIndex+1]);

          PIndex := 1;
          OpenFile(AFileName);
          OpenTemplateFile(AFileName);
          try
            SwitchToTemplate;
            WriteString('ptf ');
            WriteString(PestProperties.TemplateCharacter);
            NewLine;

            ParamNameDataArray := Model.DataArrayManager.GetDataSetByName(
              DataArray.ParamDataSetName);
            for PilotPointIndex := 0 to Model.PilotPointCount - 1 do
            begin
              APilotPoint := Model.PilotPoints[PilotPointIndex];
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
                  PointToTest := APilotPoint;
                  AQuadTree.FirstNearestPoint(PointToTest.x, PointToTest.y,
                    resultPointer);
                  if Distance(PointToTest, APilotPoint) <= CriticalDistance then
                  begin
                    ValuePointer := resultPointer;
                    Value := ValuePointer^;
                  end
                  else
                  begin
                    // This point is not on the active area for this parameter
                    // and there is at least one other point that is closer.
                    Continue;
                  end;
                end;
              end
              else
              begin
                PointToTest := APilotPoint;
                AQuadTree.FirstNearestPoint(PointToTest.x, PointToTest.y,
                  resultPointer);
                if Distance(PointToTest, APilotPoint) <= CriticalDistance then
                begin
                  ValuePointer := resultPointer;
                  Value := ValuePointer^;
                end
                else
                begin
                  // This point is not on the active area for this parameter
                  // and there is at least one other point that is closer.
                  Continue;
                end;
              end;

              FileProperties.Count := PIndex;
              FileProperties.AddValue(Value);
              FileProperties.Points.AddPoint2D(APilotPoint);

              SwitchToMain;
              WriteString(Format('%0:s%1:d', [FileProperties.ParamFamily, PIndex]));
//              WriteInteger(PIndex);
              WriteFloat(APilotPoint.x);
              WriteFloat(APilotPoint.y);
              WriteInteger(ParamIndex + 1);
              WriteFloat(Value);
              NewLine;

              SwitchToTemplate;
              WriteString(Format('%0:s%1:d', [FileProperties.ParamFamily, PIndex]));
//              WriteInteger(PIndex);
              WriteFloat(APilotPoint.x);
              WriteFloat(APilotPoint.y);
              WriteInteger(ParamIndex + 1);
              WriteString(' ');
              WriteString(PestProperties.TemplateCharacter);
              WriteString('           ');
              WriteString(FileProperties.ParameterName(PIndex));
              WriteString(PestProperties.TemplateCharacter);
              NewLine;

              Inc(PIndex);
            end;
          finally
            CloseTemplateFile;
            CloseFile;

            if PIndex = 1 then
            begin
              TFile.Delete(AFileName);
              TFile.Delete(AFileName + '.tpl');
              FileProperties.Free;
              // no pilot points.
            end
            else
            begin
              PilotPointFiles.Add(FileProperties);
            end;
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

end.
