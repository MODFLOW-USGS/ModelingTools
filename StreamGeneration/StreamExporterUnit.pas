unit StreamExporterUnit;

interface

uses
  GenericRasterUnit, ShapefileUnit, XBase1, RasterValuesAlongSegmentsUnit,
  System.Types, System.SysUtils, System.Classes;

procedure ExportStreams(const Streams: TStreamObjectList;
  PitlessRaster, AccumulatedFlowArea: IRaster; FileName: string);

implementation

uses
  FastGEO;

procedure ExportStreams(const Streams: TStreamObjectList;
  PitlessRaster, AccumulatedFlowArea: IRaster; FileName: string);
var
  LowerLeftCenter: TPoint2D;
  ShapeGeometryWriter: TShapefileGeometryWriter;
  Stream: TGeoStream;
  APoint: TPoint;
  ShapeObject: TShapeObject;
  PointIndex: Integer;
  AShapePoint: TShapePoint;
  IndexName: string;
  XBase: TXBase;
  Fields: TStringList;
  DataBaseFileName: string;
  StartZ: Extended;
  StopZ: Extended;
  ShapeLength: Double;
  AnotherShape: TShapeObject;
  Slope: double;
  ShapeIndex: Integer;
begin
  Streams.CutOffCorners;
  LowerLeftCenter := PitlessRaster.LowerLeft;
  LowerLeftCenter.x := LowerLeftCenter.x + PitlessRaster.XSpacing/2;
  LowerLeftCenter.y := LowerLeftCenter.y + PitlessRaster.YSpacing/2;
  ShapeGeometryWriter := TShapefileGeometryWriter.Create(stPolyLineZ, True);
  XBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add('ID=N');
      Fields.Add('DOWNSTREAM=N');
      Fields.Add('UP_ELEV=N18,10');
      Fields.Add('DOWN_ELEV=N18,10');
      Fields.Add('UP_ACCUM=N');
      Fields.Add('DOWN_ACCUM=N');
      Fields.Add('SLOPE=N18,10');

      DataBaseFileName := ChangeFileExt(FileName, '.dbf');
      if FileExists(DataBaseFileName) then
      begin
        DeleteFile(DataBaseFileName);
      end;
      XBase.DBFCreate(DataBaseFileName, Fields);
      XBase.FileName := DataBaseFileName;
      XBase.Active := True;
      XBase.GotoBOF;

    finally
      Fields.Free;
    end;
//    XBase.
    ShapeGeometryWriter.Capacity := Streams.Count;
    for Stream in Streams do
    begin
      ShapeObject := TShapeObject.Create;
      ShapeObject.FShapeType := stPolyLineZ;
      ShapeObject.FNumPoints := Stream.Count;
      ShapeObject.FNumParts := 1;
      SetLength(ShapeObject.FParts, 1);
      ShapeObject.FParts[0] := 0;
      SetLength(ShapeObject.FMArray, Stream.Count);
      SetLength(ShapeObject.FPoints, Stream.Count);
      SetLength(ShapeObject.FZArray, Stream.Count);
      for PointIndex := 0 to Stream.Count -1 do
      begin
        APoint := Stream[PointIndex];
        AShapePoint.X := LowerLeftCenter.x + PitlessRaster.XSpacing * APoint.X;
        AShapePoint.Y := LowerLeftCenter.y + PitlessRaster.YSpacing * APoint.Y;
        ShapeObject.FPoints[PointIndex] := AShapePoint;
        ShapeObject.FMArray[PointIndex] := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
        ShapeObject.FZArray[PointIndex] := PitlessRaster.Z[APoint.X, APoint.Y];
      end;
      ShapeGeometryWriter.AddShape(ShapeObject);

      XBase.AppendBlank;
      XBase.UpdFieldInt('ID', Stream.StreamNumber);
      XBase.UpdFieldInt('DOWNSTREAM', Stream.DownstreamNumber);

      APoint := Stream[0];
      XBase.UpdFieldNum('UP_ELEV', PitlessRaster.Z[APoint.X, APoint.Y]);
      XBase.UpdFieldInt('UP_ACCUM', Round(AccumulatedFlowArea.Z[APoint.X, APoint.Y]));

      APoint := Stream[Stream.Count -1];
      XBase.UpdFieldNum('DOWN_ELEV', PitlessRaster.Z[APoint.X, APoint.Y]);
      XBase.UpdFieldInt('DOWN_ACCUM', Round(AccumulatedFlowArea.Z[APoint.X, APoint.Y]));

      XBase.PostChanges;

      Stream.Clear;
    end;

    XBase.GotoBOF;
    for ShapeIndex := 0 to ShapeGeometryWriter.Count -1 do
    Begin
      ShapeObject := ShapeGeometryWriter.Items[ShapeIndex];
      ShapeLength := ShapeObject.ShapeLength;
      StartZ := ShapeObject.FZArray[0];
      StopZ := ShapeObject.FZArray[ShapeObject.FNumPoints-1];
      Stream := Streams[ShapeIndex];
      while (StartZ = StopZ) or (ShapeLength = 0) do
      begin
        if Stream.DownstreamNumber = 0 then
        begin
          Break;
        end;
        AnotherShape := ShapeGeometryWriter[Stream.DownstreamNumber-1];
        Stream  := Streams[Stream.DownstreamNumber-1];
        ShapeLength := ShapeLength + AnotherShape.ShapeLength;
        StopZ := AnotherShape.FZArray[AnotherShape.FNumPoints-1];
      end;
      if ShapeLength = 0 then
      begin
        Slope := 0;
      end
      else
      begin
        Slope := (StartZ-StopZ)/ShapeLength;
      end;
      XBase.UpdFieldNum('SLOPE', Slope);
      XBase.PostChanges;
      XBase.GotoNext;
    End;

    IndexName := ChangeFileExt(FileName, '.shx');
    ShapeGeometryWriter.WriteToFile(FileName, IndexName);
  finally
    ShapeGeometryWriter.Free;
    XBase.Free
  end;
end;

end.
