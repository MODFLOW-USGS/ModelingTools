unit ExtractStreamUnit;

interface

uses
  RasterValuesAlongSegmentsUnit, System.Types, GenericRasterUnit,
  System.Generics.Collections, Vcl.Dialogs, PitRemovalUnit,
  SurferGridFileReaderUnit, System.IOUtils;

procedure ComputeAccumulation(const PitlessRaster: IRaster;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRaster;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRaster = nil
  );

procedure ComputeAccumulationModified(const PitlessRaster: IRaster;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRaster;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRaster = nil
  );

procedure ComputeAccumulationSurf(const PitlessRaster: IRasterFile;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRasterFile;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  MinAreaRaster: IRasterFile = nil
  );

procedure ComputeAccumulationSurfModified(const PitlessRaster: IRasterFile;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRasterFile;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRasterFile = nil
  );

procedure ExtractRunoffLocations(Streams: TStreamObjectList;
  FlowDirections: TFlowDirections;
  const PitlessRaster: IRaster; out RunoffRaster: IRaster;
  OnProgress: TProgressEvent);

procedure ExtractRunoffLocationsSurf(Streams: TStreamObjectList;
  FlowDirections: TFlowDirections;
  const PitlessRaster: IRasterFile; out RunoffRaster: IRasterFile;
  OnProgress: TProgressEvent);

type
  TTempIntSurfRaster = class(TSurferRaster7File2)
  private
    FNewFileName: string;
    function GetIntZ(XIndex, YIndex: Integer): Integer;
    procedure SetIntZ(XIndex, YIndex: Integer; const Value: Integer);
  public
    property IntZ[XIndex, YIndex: Integer]: Integer read GetIntZ write SetIntZ; default;
    constructor Create(ARaster: IRasterFile); overload;
    constructor Create(AFileName: string); overload;
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, System.SysUtils, FileQueueUnit, Unit6, TempFiles;

procedure PackStreams(Streams: TStreamObjectList);
var
  StreamIndex: Integer;
  AStream: TGeoStream;
  Offset: Integer;
begin
  Assert(not Streams.OwnsObjects);
  Offset := 0;
  for StreamIndex := 0 to Streams.Count - 1 do
  begin
    AStream := Streams[StreamIndex];
    if AStream <> nil then
    begin
      if AStream.Count <= 1 then
      begin
        AStream.Free;
        Streams[StreamIndex] := nil;
      end
      else
      begin
        if StreamIndex > Offset then
        begin
          Streams[Offset] := AStream;
          Streams[StreamIndex] := nil;
          AStream.StreamNumber := Offset + 1;
        end;
        Inc(Offset);
      end;
    end;
  end;
  Streams.Count := Offset;
end;

procedure ComputeAccumulationSurf(const PitlessRaster: IRasterFile;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRasterFile;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  MinAreaRaster: IRasterFile = nil
  );
var
  XIndex: Integer;
  YIndex: Integer;
  FlowDir: TFlowDirection;
  GRaster: TTempSurferRaster7File;
  Inputs: T2DIntArray;
  APoint: TPoint;
  DownstreamPoint: TPoint;
  StreamNumbers: T2DIntArray;
  StreamCounts: T2DIntArray;
  StreamArray: array of array of TGeoStream;
  AStream: TGeoStream;
  StreamIndex: Integer;
  FlowArea: Double;
  UsePoint: Boolean;
  AnotherStream: TGeoStream;
  PointIndex: Integer;
  StreamPosition: integer;
  FirstPoint: TPoint;
  AFlowOrd: TFlowOrdinal;
  PointQueue: TFileQueue<TPoint>;
  NumberOfInflowingStreams: T2DIntArray;
  procedure AccumulateAPoint(APoint: TPoint);
  var
    AStream: TGeoStream;
  begin
    AStream := TGeoStream.Create;
    AStream.StreamNumber := Streams.Add(AStream) + 1;
    while Inputs[APoint.X, APoint.Y] = 0 do
    begin
      GRaster.Z[APoint.X,APoint.Y] := GRaster.Z[APoint.X,APoint.Y] + 1;
      FlowDir := FlowDirections[APoint.X,APoint.Y];
      AStream.Add(APoint);

      if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
      begin
        DownstreamPoint.X := APoint.X + FlowDir.DeltaX;
        DownstreamPoint.Y := APoint.Y + FlowDir.DeltaY;
        GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y] :=
          GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y]
          + GRaster.Z[APoint.X,APoint.Y];
        Dec(Inputs[DownstreamPoint.X,DownstreamPoint.Y]);
        APoint := DownstreamPoint;
        if (NumberOfInflowingStreams[APoint.X,APoint.Y] > 1)
          and (Inputs[APoint.X, APoint.Y] = 0)
          and (FlowDirections.Ordinals[APoint.X, APoint.Y] <> foMiddle)
          and (StreamArray[APoint.X, APoint.Y] <> nil) then
        begin
          PointQueue.Enqueue(APoint);
          break;
        end;
      end
      else
      begin
        break;
      end;
    end;
    if (AStream.Last.X <> APoint.X) or  (AStream.Last.Y <> APoint.Y) then
    begin
      AStream.Add(APoint);
    end;
  end;
begin
  GRaster := TTempSurferRaster7File.Create(PitlessRaster);
  Streams := TStreamObjectList.Create;
  AccumulatedFlowArea := GRaster;
  SetLength(Inputs, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamNumbers, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamCounts, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamArray, PitlessRaster.XCount, PitlessRaster.YCount);
  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      GRaster.Z[XIndex,YIndex] := 0;
      Inputs[XIndex,YIndex] := 0;
      StreamNumbers[XIndex,YIndex] := 0;
      StreamCounts[XIndex,YIndex] := 0;
      StreamArray[XIndex,YIndex] := nil;
    end;
  end;

  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      if not GRaster.Ignore[XIndex,YIndex] then
      begin
        FlowDir := FlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
        begin
          Inc(Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY])
        end;
      end;
    end;
  end;
  NumberOfInflowingStreams := Inputs;
  SetLength(NumberOfInflowingStreams, PitlessRaster.XCount, PitlessRaster.YCount);

  PointQueue := TFileQueue<TPoint>.Create;
  try
    for YIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for XIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        if (not GRaster.Ignore[XIndex,YIndex])
          and (NumberOfInflowingStreams[XIndex,YIndex] = 0) then
        begin
          APoint.X := XIndex;
          APoint.Y := YIndex;
          AccumulateAPoint(APoint);
        end;
      end;
    end;

    while PointQueue.Count > 0 do
    begin
      APoint := PointQueue.Dequeue;
      if Inputs[APoint.X, APoint.Y] = 0 then
      begin
        AccumulateAPoint(APoint);
      end
      else
      begin
        PointQueue.Enqueue(APoint);
      end;
    end;

    // Delete stream sections with cumulative areas that are too small.

    for StreamIndex := 0 to Streams.Count -1 do
    begin
      AStream := Streams[StreamIndex];
      APoint := AStream.First;
      if StreamArray[APoint.X,APoint.Y] <> nil then
      begin
  //      StreamArray[APoint.X,APoint.Y,0] := AStream;
        APoint := AStream.Last;
        StreamArray[APoint.X,APoint.Y] := AStream;
        Continue;
      end;
      while AStream.Count > 0 do
      begin
        APoint := AStream.First;
        if StreamArray[APoint.X,APoint.Y] <> nil then
        begin
          Break
        end;
        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
        if MinAreaRaster = nil then
        begin
          UsePoint := FlowArea >= MinArea;
        end
        else
        begin
          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
        end;
        if UsePoint then
        begin
          Break;
        end
        else
        begin
          AStream.Delete(0);
        end;
      end;
      if AStream.Count > 1 then
      begin
        APoint := AStream.Last;
        StreamArray[APoint.X,APoint.Y] := AStream;
      end;
    end;
    Streams.OwnsObjects := False;
    PackStreams(Streams);

    // merge streams when appropriate
    for StreamIndex := 0 to Streams.Count -1 do
    begin
      AStream := Streams[StreamIndex];
      APoint := AStream.First;
      StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
      APoint := AStream.Last;
      Inc(StreamCounts[APoint.X, APoint.Y]);
    end;
    for StreamIndex := 0 to Streams.Count - 1 do
    begin
      AStream := Streams[StreamIndex];
      if AStream <> nil then
      begin
        repeat
          APoint := AStream.Last;
          AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
          if (StreamCounts[APoint.X, APoint.Y] = 1) then
          begin
            StreamPosition := StreamNumbers[APoint.X, APoint.Y] - 1;
            if StreamPosition >= 0 then
            begin
              AnotherStream := Streams[StreamPosition];
              FirstPoint := AnotherStream.First;
              Assert(APoint.X = FirstPoint.X);
              Assert(APoint.Y = FirstPoint.Y);
              for PointIndex := 1 to AnotherStream.Count - 1 do
              begin
                AStream.Add(AnotherStream[PointIndex]);
              end;
              AnotherStream.Free;
              Streams[StreamPosition] := nil;
            end
            else
            begin
              break;
            end;

            APoint := AStream.Last;
            AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
            if (AFlowOrd = foMiddle) then
            begin
              break;
            end;
          end
          else
          begin
            Break;
          end;
        until False;
      end;
    end;

    PackStreams(Streams);
    Streams.OwnsObjects := True;

    for AStream in Streams do
    begin
      APoint := AStream.First;
      StreamNumbers[APoint.X,APoint.Y] := AStream.StreamNumber;
    end;
    for AStream in Streams do
    begin
      APoint := AStream.Last;
      AStream.DownstreamNumber := StreamNumbers[APoint.X,APoint.Y];
    end;


  finally
    PointQueue.Free;
  end;
end;

procedure ComputeAccumulation(const PitlessRaster: IRaster;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRaster;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRaster = nil
  );
var
  XIndex: Integer;
  YIndex: Integer;
  FlowDir: TFlowDirection;
  GRaster: TGenericRaster;
  Inputs: T2DIntArray;
  APoint: TPoint;
  DownstreamPoint: TPoint;
  StreamNumbers: T2DIntArray;
  StreamCounts: T2DIntArray;
  StreamArray: array of array of TGeoStream;
  AStream: TGeoStream;
  StreamIndex: Integer;
  FlowArea: Double;
  UsePoint: Boolean;
  AnotherStream: TGeoStream;
  PointIndex: Integer;
  StreamPosition: integer;
  FirstPoint: TPoint;
  AFlowOrd: TFlowOrdinal;
  PointQueue: TQueue<TPoint>;
  CellUpdate: Integer;
  MaxQueue: Integer;
  Count: Integer;
  NumberOfInflowingStreams: T2DIntArray;
  procedure AccumulateAPoint(APoint: TPoint);
  var
    AStream: TGeoStream;
  begin
    AStream := TGeoStream.Create;
    AStream.StreamNumber := Streams.Add(AStream) + 1;
    while Inputs[APoint.X, APoint.Y] = 0 do
    begin
      GRaster.Z[APoint.X,APoint.Y] := GRaster.Z[APoint.X,APoint.Y] + 1;
      FlowDir := FlowDirections[APoint.X,APoint.Y];
      AStream.Add(APoint);

      if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
      begin
        DownstreamPoint.X := APoint.X + FlowDir.DeltaX;
        DownstreamPoint.Y := APoint.Y + FlowDir.DeltaY;
        GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y] :=
          GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y]
          + GRaster.Z[APoint.X,APoint.Y];
        Dec(Inputs[DownstreamPoint.X,DownstreamPoint.Y]);
        if {not FoundError and} (PitlessRaster.Z[DownstreamPoint.X, DownstreamPoint.y]
          > PitlessRaster.Z[APoint.X, APoint.y]) then
        begin
          Form6.memo2.Lines.Add(Format('(%d, %d), (%d, %d)',
            [APoint.X, APoint.y, DownstreamPoint.X,DownstreamPoint.Y]));
//            FoundError := True;
        end;
        APoint := DownstreamPoint;
        if (NumberOfInflowingStreams[APoint.X,APoint.Y] > 1)
          and (Inputs[APoint.X, APoint.Y] = 0)
          and (FlowDirections.Ordinals[APoint.X, APoint.Y] <> foMiddle) then
        begin
          PointQueue.Enqueue(APoint);
          break;
        end;
      end
      else
      begin
        break;
      end;
    end;
    if (AStream.Last.X <> APoint.X) or  (AStream.Last.Y <> APoint.Y) then
    begin
      AStream.Add(APoint);
    end;
  end;
begin
  GRaster := TGenericRaster.Create(PitlessRaster);
  Streams := TStreamObjectList.Create;
  AccumulatedFlowArea := GRaster;
  SetLength(Inputs, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamNumbers, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamCounts, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamArray, PitlessRaster.XCount, PitlessRaster.YCount);
  Count := 0;
  CellUpdate := PitlessRaster.YCount*PitlessRaster.XCount div 1000;
  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      GRaster.Z[XIndex,YIndex] := 0;
      Inputs[XIndex,YIndex] := 0;
      StreamNumbers[XIndex,YIndex] := 0;
      StreamCounts[XIndex,YIndex] := 0;
      StreamArray[XIndex,YIndex] := nil;
      if Assigned(OnProgress) then
      begin
        Inc(Count);
        if (Count mod CellUpdate) = 0 then
        begin
          OnProgress(nil, Count div CellUpdate, 1000);
        end;
      end;
    end;
  end;

  Count := 0;
  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      if not GRaster.Ignore[XIndex,YIndex] then
      begin
        FlowDir := FlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
        begin
          Inc(Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY])
        end;
      end;
      if Assigned(OnProgress) then
      begin
        Inc(Count);
        if (Count mod CellUpdate) = 0 then
        begin
          OnProgress(nil, Count div CellUpdate, 1000);
        end;
      end;
    end;
  end;
  NumberOfInflowingStreams := Inputs;
  SetLength(NumberOfInflowingStreams, PitlessRaster.XCount, PitlessRaster.YCount);

  PointQueue := TQueue<TPoint>.Create;
  try
    Count := 0;
    for YIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for XIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        if (not GRaster.Ignore[XIndex,YIndex])
          and (NumberOfInflowingStreams[XIndex,YIndex] = 0) then
        begin
          APoint.X := XIndex;
          APoint.Y := YIndex;
          AccumulateAPoint(APoint);
        end;
        if Assigned(OnProgress) then
        begin
          Inc(Count);
          if (Count mod CellUpdate) = 0 then
          begin
            OnProgress(nil, Count div CellUpdate, 1000);
          end;
        end;
      end;
    end;

    MaxQueue := PointQueue.Count;
    while PointQueue.Count > 0 do
    begin
      APoint := PointQueue.Dequeue;
      if Inputs[APoint.X, APoint.Y] = 0 then
      begin
        AccumulateAPoint(APoint);
      end
      else
      begin
        PointQueue.Enqueue(APoint);
      end;
        if Assigned(OnProgress) then
        begin
//          Inc(Count);
//          if (Count mod CellUpdate) = 0 then
          begin
            OnProgress(nil, MaxQueue - PointQueue.Count, MaxQueue);
          end;
        end;

    end;

    // Delete stream sections with cumulative areas that are too small.

    for StreamIndex := 0 to Streams.Count -1 do
    begin
      AStream := Streams[StreamIndex];
      APoint := AStream.First;
      if StreamArray[APoint.X,APoint.Y] <> nil then
      begin
  //      StreamArray[APoint.X,APoint.Y,0] := AStream;
        APoint := AStream.Last;
        StreamArray[APoint.X,APoint.Y] := AStream;
        Continue;
      end;
      while AStream.Count > 0 do
      begin
        APoint := AStream.First;
        if StreamArray[APoint.X,APoint.Y] <> nil then
        begin
          Break
        end;
        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
        if MinAreaRaster = nil then
        begin
          UsePoint := FlowArea >= MinArea;
        end
        else
        begin
          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
        end;
        if UsePoint then
        begin
          Break;
        end
        else
        begin
          AStream.Delete(0);
        end;
      end;
      if AStream.Count > 1 then
      begin
        APoint := AStream.Last;
        StreamArray[APoint.X,APoint.Y] := AStream;
      end;
    end;
    Streams.OwnsObjects := False;
    PackStreams(Streams);

    // merge streams when appropriate
    for StreamIndex := 0 to Streams.Count -1 do
    begin
      AStream := Streams[StreamIndex];
      APoint := AStream.First;
      StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
      APoint := AStream.Last;
      Inc(StreamCounts[APoint.X, APoint.Y]);
    end;
    for StreamIndex := 0 to Streams.Count - 1 do
    begin
      AStream := Streams[StreamIndex];
      if AStream <> nil then
      begin
        repeat
          APoint := AStream.Last;
          AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
          if (StreamCounts[APoint.X, APoint.Y] = 1) then
          begin
            StreamPosition := StreamNumbers[APoint.X, APoint.Y] - 1;
            if StreamPosition >= 0 then
            begin
              AnotherStream := Streams[StreamPosition];
              FirstPoint := AnotherStream.First;
              Assert(APoint.X = FirstPoint.X);
              Assert(APoint.Y = FirstPoint.Y);
              for PointIndex := 1 to AnotherStream.Count - 1 do
              begin
                AStream.Add(AnotherStream[PointIndex]);
              end;
              AnotherStream.Free;
              Streams[StreamPosition] := nil;
            end
            else
            begin
              break;
            end;

            APoint := AStream.Last;
            AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
            if (AFlowOrd = foMiddle) then
            begin
              break;
            end;
          end
          else
          begin
            Break;
          end;
        until False;
      end;
    end;

    PackStreams(Streams);
    Streams.OwnsObjects := True;

  for AStream in Streams do
  begin
    APoint := AStream.First;
    StreamNumbers[APoint.X,APoint.Y] := AStream.StreamNumber;
  end;
  for AStream in Streams do
  begin
    APoint := AStream.Last;
    AStream.DownstreamNumber := StreamNumbers[APoint.X,APoint.Y];
  end;


  finally
    PointQueue.Free;
  end;
end;
procedure ExtractRunoffLocationsSurf(Streams: TStreamObjectList;
  FlowDirections: TFlowDirections;
  const PitlessRaster: IRasterFile; out RunoffRaster: IRasterFile;
  OnProgress: TProgressEvent);
var
  StreamNumbers: T2DIntArray;
  XIndex: integer;
  YIndex: Integer;
  StreamIndex: Integer;
  AStream: TGeoStream;
  PointIndex: Integer;
  APoint: TPoint;
  InnerXStart: integer;
  InnerXEnd: integer;
  InnerYStart: integer;
  InnerYEnd: integer;
  InnerXIndex: Integer;
  InnerYIndex: Integer;
  FlowDir: TFlowDirection;
  PointQueue: TFileQueue<TPoint>;
  UpstreamPoint: TPoint;
  GRaster: TTempSurferRaster7File;
  InQueue: array of array of Boolean;
  UpdateCount: Int64;
  MaxCount: Int64;
  CurrentCount: Int64;
begin
  GRaster := TTempSurferRaster7File.Create(PitlessRaster);
  RunoffRaster := GRaster;
  MaxCount := RunoffRaster.XCount * RunoffRaster.YCount;
  UpdateCount := MaxCount div 1000;
  if UpdateCount = 0 then
  begin
    UpdateCount := 1;
  end;
  SetLength(StreamNumbers, RunoffRaster.XCount, RunoffRaster.YCount);
  SetLength(InQueue, RunoffRaster.XCount, RunoffRaster.YCount);
  for YIndex := 0 to RunoffRaster.YCount - 1 do
  begin
    for XIndex := 0 to RunoffRaster.XCount - 1 do
    begin
      StreamNumbers[XIndex, YIndex] := 0;
      InQueue[XIndex, YIndex] := False;
    end;
  end;
  PointQueue := TFileQueue<TPoint>.Create;
  try
    for StreamIndex := 0 to Streams.Count - 1 do
    begin
      AStream := Streams[StreamIndex];
      for PointIndex := 0 to AStream.Count - 1 do
      begin
        APoint := AStream[PointIndex];
        StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
        if not InQueue[APoint.X, APoint.Y] then
        begin
          PointQueue.EnQueue(APoint);
          InQueue[APoint.X, APoint.Y] := True;
        end;
      end;
    end;
//    for YIndex := 0 to RunoffRaster.YCount - 1 do
//    begin
//      for XIndex := 0 to RunoffRaster.XCount - 1 do
//      begin
//        if StreamNumbers[XIndex, YIndex] <> 0 then
//        begin
//          InnerXStart := Max(0,XIndex-1);
//          InnerXEnd := Min(RunoffRaster.XCount - 1, XIndex+1);
//          InnerYStart := Max(0,YIndex-1);
//          InnerYEnd := Min(RunoffRaster.YCount - 1, YIndex+1);
//          for InnerXIndex := InnerXStart to InnerXEnd do
//          begin
//            for InnerYIndex := InnerYStart to InnerYEnd do
//            begin
//              if StreamNumbers[InnerXIndex, InnerYIndex] = 0 then
//              begin
//                FlowDir := FlowDirections[InnerXIndex,InnerYIndex];
//                if (InnerXIndex + FlowDir.DeltaX = XIndex)
//                  and (InnerYIndex + FlowDir.DeltaY = YIndex) then
//                begin
//                  StreamNumbers[InnerXIndex,InnerYIndex] :=
//                    StreamNumbers[XIndex, YIndex];
//                  if (InnerXIndex < XIndex) or
//                    ((InnerXIndex = XIndex) and (InnerYIndex < YIndex)) then
//                  begin
//                    APoint.X := InnerXIndex;
//                    APoint.Y := InnerYIndex;
//                    PointQueue.Enqueue(APoint);
//                  end;
//                end;
//              end;
//            end;
//          end;
//        end;
//      end;
//    end;

    CurrentCount := MaxCount;
    while PointQueue.Count > 0 do
    begin
      if Assigned(OnProgress) then
      begin
        Dec(CurrentCount);
        if (CurrentCount mod UpdateCount) = 0 then
        begin
          OnProgress(nil, (MaxCount-CurrentCount) div UpdateCount, 1000);
        end;
      end;
      APoint := PointQueue.Dequeue;
      InnerXStart := Max(0,APoint.X-1);
      InnerXEnd := Min(RunoffRaster.XCount - 1, APoint.X+1);
      InnerYStart := Max(0,APoint.Y-1);
      InnerYEnd := Min(RunoffRaster.YCount - 1, APoint.Y+1);
      for InnerXIndex := InnerXStart to InnerXEnd do
      begin
        for InnerYIndex := InnerYStart to InnerYEnd do
        begin
          if StreamNumbers[InnerXIndex, InnerYIndex] = 0 then
          begin
            FlowDir := FlowDirections[InnerXIndex,InnerYIndex];
            if (InnerXIndex + FlowDir.DeltaX = APoint.X)
              and (InnerYIndex + FlowDir.DeltaY = APoint.Y) then
            begin
              StreamNumbers[InnerXIndex,InnerYIndex] :=
                StreamNumbers[APoint.X, APoint.Y];
              UpstreamPoint.X := InnerXIndex;
              UpstreamPoint.Y := InnerYIndex;
//              if not InQueue[UpstreamPoint.X, UpstreamPoint.Y] then
              begin
                PointQueue.Enqueue(UpstreamPoint);
                InQueue[UpstreamPoint.X, UpstreamPoint.Y] := True;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    PointQueue.Free;
  end;

  for YIndex := 0 to RunoffRaster.YCount - 1 do
  begin
    for XIndex := 0 to RunoffRaster.XCount - 1 do
    begin
      GRaster.Z[XIndex, YIndex] := StreamNumbers[XIndex, YIndex];
    end;
  end;

end;

procedure ExtractRunoffLocations(Streams: TStreamObjectList;
  FlowDirections: TFlowDirections;
  const PitlessRaster: IRaster; out RunoffRaster: IRaster;
  OnProgress: TProgressEvent);
var
  StreamNumbers: T2DIntArray;
  XIndex: integer;
  YIndex: Integer;
  StreamIndex: Integer;
  AStream: TGeoStream;
  PointIndex: Integer;
  APoint: TPoint;
  InnerXStart: integer;
  InnerXEnd: integer;
  InnerYStart: integer;
  InnerYEnd: integer;
  InnerXIndex: Integer;
  InnerYIndex: Integer;
  FlowDir: TFlowDirection;
  PointQueue: TQueue<TPoint>;
  UpstreamPoint: TPoint;
  GRaster: TGenericRaster;
  InQueue: array of array of Boolean;
  MaxCount: Int64;
  UpdateCount: Int64;
  CurrentCount: Int64;
begin
  GRaster := TGenericRaster.Create(PitlessRaster);
  RunoffRaster := GRaster;
  MaxCount := RunoffRaster.XCount * RunoffRaster.YCount;
  UpdateCount := MaxCount div 1000;
  if UpdateCount = 0 then
  begin
    UpdateCount := 1;
  end;
  SetLength(StreamNumbers, RunoffRaster.XCount, RunoffRaster.YCount);
  SetLength(InQueue, RunoffRaster.XCount, RunoffRaster.YCount);
  for YIndex := 0 to RunoffRaster.YCount - 1 do
  begin
    for XIndex := 0 to RunoffRaster.XCount - 1 do
    begin
      StreamNumbers[XIndex, YIndex] := 0;
      InQueue[XIndex, YIndex] := False;
    end;
  end;
  PointQueue := TQueue<TPoint>.Create;
  try
    for StreamIndex := 0 to Streams.Count - 1 do
    begin
      AStream := Streams[StreamIndex];
      for PointIndex := 0 to AStream.Count - 1 do
      begin
        APoint := AStream[PointIndex];
        StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
        if not InQueue[APoint.X, APoint.Y] then
        begin
          PointQueue.EnQueue(APoint);
        end;
      end;
    end;
//    for YIndex := 0 to RunoffRaster.YCount - 1 do
//    begin
//      for XIndex := 0 to RunoffRaster.XCount - 1 do
//      begin
//        if StreamNumbers[XIndex, YIndex] <> 0 then
//        begin
//          InnerXStart := Max(0,XIndex-1);
//          InnerXEnd := Min(RunoffRaster.XCount - 1, XIndex+1);
//          InnerYStart := Max(0,YIndex-1);
//          InnerYEnd := Min(RunoffRaster.YCount - 1, YIndex+1);
//          for InnerXIndex := InnerXStart to InnerXEnd do
//          begin
//            for InnerYIndex := InnerYStart to InnerYEnd do
//            begin
//              if StreamNumbers[InnerXIndex, InnerYIndex] = 0 then
//              begin
//                FlowDir := FlowDirections[InnerXIndex,InnerYIndex];
//                if (InnerXIndex + FlowDir.DeltaX = XIndex)
//                  and (InnerYIndex + FlowDir.DeltaY = YIndex) then
//                begin
//                  StreamNumbers[InnerXIndex,InnerYIndex] :=
//                    StreamNumbers[XIndex, YIndex];
//                  if (InnerXIndex < XIndex) or
//                    ((InnerXIndex = XIndex) and (InnerYIndex < YIndex)) then
//                  begin
//                    APoint.X := InnerXIndex;
//                    APoint.Y := InnerYIndex;
//                    PointQueue.Enqueue(APoint);
//                  end;
//                end;
//              end;
//            end;
//          end;
//        end;
//      end;
//    end;

    CurrentCount := MaxCount;
    while PointQueue.Count > 0 do
    begin
      Dec(CurrentCount);
      if (CurrentCount mod UpdateCount) = 0 then
      begin
        OnProgress(nil, (MaxCount-CurrentCount) div UpdateCount, 1000);
      end;
      APoint := PointQueue.Dequeue;
      InnerXStart := Max(0,APoint.X-1);
      InnerXEnd := Min(RunoffRaster.XCount - 1, APoint.X+1);
      InnerYStart := Max(0,APoint.Y-1);
      InnerYEnd := Min(RunoffRaster.YCount - 1, APoint.Y+1);
      for InnerXIndex := InnerXStart to InnerXEnd do
      begin
        for InnerYIndex := InnerYStart to InnerYEnd do
        begin
          if StreamNumbers[InnerXIndex, InnerYIndex] = 0 then
          begin
            FlowDir := FlowDirections[InnerXIndex,InnerYIndex];
            if (InnerXIndex + FlowDir.DeltaX = APoint.X)
              and (InnerYIndex + FlowDir.DeltaY = APoint.Y) then
            begin
              StreamNumbers[InnerXIndex,InnerYIndex] :=
              StreamNumbers[APoint.X, APoint.Y];
              UpstreamPoint.X := InnerXIndex;
              UpstreamPoint.Y := InnerYIndex;
              PointQueue.Enqueue(UpstreamPoint);
            end;
          end;
        end;
      end;
    end;
  finally
    PointQueue.Free;
  end;

  for YIndex := 0 to RunoffRaster.YCount - 1 do
  begin
    for XIndex := 0 to RunoffRaster.XCount - 1 do
    begin
      GRaster.Z[XIndex, YIndex] := StreamNumbers[XIndex, YIndex];
    end;
  end;

end;

procedure ComputeAccumulationModified(const PitlessRaster: IRaster;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRaster;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRaster = nil
  );
var
  XIndex: Integer;
  YIndex: Integer;
  FlowDir: TFlowDirection;
  GRaster: TGenericRaster;
  Inputs: T2DIntArray;
  APoint: TPoint;
  DownstreamPoint: TPoint;
  StreamNumbers: T2DIntArray;
  StreamCounts: T2DIntArray;
  StreamArray: array of array of TGeoStream;
  AStream: TGeoStream;
  StreamIndex: Integer;
  FlowArea: Double;
  UsePoint: Boolean;
  AnotherStream: TGeoStream;
  PointIndex: Integer;
  StreamPosition: integer;
  FirstPoint: TPoint;
  AFlowOrd: TFlowOrdinal;
  PointQueue: TQueue<TPoint>;
  UpdateCount: Int64;
  Count: Int64;
  MaxQueue: Int64;
  FoundError: Boolean;
  PriorCount: Int64;
  NumberOfInflowingStreams: T2DIntArray;
  procedure AccumulateAPoint(APoint: TPoint);
  var
    AStream: TGeoStream;
  begin
    AStream := TGeoStream.Create;
//    AStream.StreamNumber := Streams.Add(AStream) + 1;
    while Inputs[APoint.X, APoint.Y] = 0 do
    begin
      GRaster.Z[APoint.X,APoint.Y] := GRaster.Z[APoint.X,APoint.Y] + 1;
      FlowDir := FlowDirections[APoint.X,APoint.Y];
      AStream.Add(APoint);

      if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
      begin
        DownstreamPoint.X := APoint.X + FlowDir.DeltaX;
        DownstreamPoint.Y := APoint.Y + FlowDir.DeltaY;
        GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y] :=
          GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y]
          + GRaster.Z[APoint.X,APoint.Y];
        Dec(Inputs[DownstreamPoint.X,DownstreamPoint.Y]);
        if {not FoundError and} (PitlessRaster.Z[DownstreamPoint.X, DownstreamPoint.y]
          > PitlessRaster.Z[APoint.X, APoint.y]) then
        begin
          Form6.memo2.Lines.Add(Format('(%d, %d), (%d, %d)',
            [APoint.X, APoint.y, DownstreamPoint.X,DownstreamPoint.Y]));
            FoundError := True;
        end;

        APoint := DownstreamPoint;
        if (NumberOfInflowingStreams[APoint.X,APoint.Y] > 1)
          and (Inputs[APoint.X, APoint.Y] = 0)
          and (FlowDirections.Ordinals[APoint.X, APoint.Y] <> foMiddle)
          and (StreamArray[APoint.X,APoint.Y] <> nil) then
        begin
          PointQueue.Enqueue(APoint);
          break;
        end;
      end
      else
      begin
        break;
      end;
    end;
    if (AStream.Last.X <> APoint.X) or  (AStream.Last.Y <> APoint.Y) then
    begin
      AStream.Add(APoint);
    end;

    APoint := AStream.First;
    if StreamArray[APoint.X,APoint.Y] <> nil then
    begin
//      StreamArray[APoint.X,APoint.Y,0] := AStream;
      APoint := AStream.Last;
//      StreamArray[APoint.X,APoint.Y] := AStream;
//      Continue;
    end
    else
    begin
      while AStream.Count > 0 do
      begin
        APoint := AStream.First;
        if StreamArray[APoint.X,APoint.Y] <> nil then
        begin
          Break
        end;
        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
        if MinAreaRaster = nil then
        begin
          UsePoint := FlowArea >= MinArea;
        end
        else
        begin
          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
        end;
        if UsePoint then
        begin
          Break;
        end
        else
        begin
          AStream.Delete(0);
        end;
      end;
    end;
    if AStream.Count > 1 then
    begin
      APoint := AStream.Last;
      StreamArray[APoint.X,APoint.Y] := AStream;
      AStream.StreamNumber := Streams.Add(AStream) + 1;
    end
    else
    begin
      AStream.Free;
    end;


  end;
begin
  FoundError := False;
  Form6.memo2.Lines.BeginUpdate;
  GRaster := TGenericRaster.Create(PitlessRaster);
  Streams := TStreamObjectList.Create;
  AccumulatedFlowArea := GRaster;
  SetLength(Inputs, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamNumbers, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamCounts, PitlessRaster.XCount, PitlessRaster.YCount);
  SetLength(StreamArray, PitlessRaster.XCount, PitlessRaster.YCount);
  UpdateCount := PitlessRaster.YCount * PitlessRaster.XCount div 1000;
  if UpdateCount = 0 then
  begin
    UpdateCount := 1;
  end;
  Count := 0;
  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      GRaster.Z[XIndex,YIndex] := 0;
      Inputs[XIndex,YIndex] := 0;
      StreamNumbers[XIndex,YIndex] := 0;
      StreamCounts[XIndex,YIndex] := 0;
      StreamArray[XIndex,YIndex] := nil;
      if Assigned(OnProgress) then
      begin
        Inc(Count);
        if (Count mod UpdateCount) = 0 then
        begin
          OnProgress(nil, Count div UpdateCount, 1000);
        end;
      end;
    end;
  end;

  Count := 0;
  for YIndex := 0 to PitlessRaster.YCount - 1 do
  begin
    for XIndex := 0 to PitlessRaster.XCount - 1 do
    begin
      if not GRaster.Ignore[XIndex,YIndex] then
      begin
        FlowDir := FlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
        begin
          Inc(Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY])
        end;
      end;
      if Assigned(OnProgress) then
      begin
        Inc(Count);
        if (Count mod UpdateCount) = 0 then
        begin
          OnProgress(nil, Count div UpdateCount, 1000);
        end;
      end;
    end;
  end;
  NumberOfInflowingStreams := Inputs;
  SetLength(NumberOfInflowingStreams, PitlessRaster.XCount, PitlessRaster.YCount);

  PointQueue := TQueue<TPoint>.Create;
  try
    Count := 0;
    for YIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for XIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        if (not GRaster.Ignore[XIndex,YIndex])
          and (NumberOfInflowingStreams[XIndex,YIndex] = 0) then
        begin
          APoint.X := XIndex;
          APoint.Y := YIndex;
          AccumulateAPoint(APoint);
        end;
        if Assigned(OnProgress) then
        begin
          Inc(Count);
          if (Count mod UpdateCount) = 0 then
          begin
            OnProgress(nil, Count div UpdateCount, 1000);
          end;
        end;
      end;
    end;

    MaxQueue := PointQueue.Count;
    UpdateCount := MaxQueue div 1000;
    if UpdateCount = 0 then
    begin
      UpdateCount := 1;
    end;
    PriorCount := MaxQueue;
    while PointQueue.Count > 0 do
    begin
      APoint := PointQueue.Dequeue;
      if Inputs[APoint.X, APoint.Y] = 0 then
      begin
        AccumulateAPoint(APoint);
      end
      else
      begin
        PointQueue.Enqueue(APoint);
      end;
      if Assigned(OnProgress) and (PointQueue.Count <> PriorCount)
        and (UpdateCount <> 0) then
      begin
        PriorCount := PointQueue.Count;
          if ((MaxQueue - PointQueue.Count) mod UpdateCount) = 0 then
          begin
            OnProgress(nil, (MaxQueue - PointQueue.Count) div  UpdateCount, 1000);
          end;
      end;
    end;

    // Delete stream sections with cumulative areas that are too small.

//    for StreamIndex := 0 to Streams.Count -1 do
//    begin
//      AStream := Streams[StreamIndex];
//      APoint := AStream.First;
//      if StreamArray[APoint.X,APoint.Y] <> nil then
//      begin
//  //      StreamArray[APoint.X,APoint.Y,0] := AStream;
//        APoint := AStream.Last;
//        StreamArray[APoint.X,APoint.Y] := AStream;
//        Continue;
//      end;
//      while AStream.Count > 0 do
//      begin
//        APoint := AStream.First;
//        if StreamArray[APoint.X,APoint.Y] <> nil then
//        begin
//          Break
//        end;
//        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
//        if MinAreaRaster = nil then
//        begin
//          UsePoint := FlowArea >= MinArea;
//        end
//        else
//        begin
//          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
//        end;
//        if UsePoint then
//        begin
//          Break;
//        end
//        else
//        begin
//          AStream.Delete(0);
//        end;
//      end;
//      if AStream.Count > 1 then
//      begin
//        APoint := AStream.Last;
//        StreamArray[APoint.X,APoint.Y] := AStream;
//      end;
//    end;
//    Streams.OwnsObjects := False;
//    PackStreams(Streams);

    // merge streams when appropriate
    for StreamIndex := 0 to Streams.Count -1 do
    begin
      AStream := Streams[StreamIndex];
      APoint := AStream.First;
      StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
      APoint := AStream.Last;
      Inc(StreamCounts[APoint.X, APoint.Y]);
    end;
    UpdateCount := Streams.Count div 1000;
    if UpdateCount = 0 then
    begin
      UpdateCount := 1;
    end;
    for StreamIndex := 0 to Streams.Count - 1 do
    begin
      AStream := Streams[StreamIndex];
      try
        if AStream <> nil then
        begin
          repeat
            APoint := AStream.Last;
            AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
            if (StreamCounts[APoint.X, APoint.Y] = 1) then
            begin
              StreamPosition := StreamNumbers[APoint.X, APoint.Y] - 1;
              if StreamPosition >= 0 then
              begin
                AnotherStream := Streams[StreamPosition];
                FirstPoint := AnotherStream.First;
                Assert(APoint.X = FirstPoint.X);
                Assert(APoint.Y = FirstPoint.Y);
                for PointIndex := 1 to AnotherStream.Count - 1 do
                begin
                  AStream.Add(AnotherStream[PointIndex]);
                end;
  //              AnotherStream.Free;
                Streams[StreamPosition] := nil;
              end
              else
              begin
                break;
              end;

              APoint := AStream.Last;
              AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
              if (AFlowOrd = foMiddle) then
              begin
                break;
              end;
            end
            else
            begin
              Break;
            end;
          until False;
        end;
        if Assigned(OnProgress) then
        begin
          if (StreamIndex mod UpdateCount) = 0 then
          begin
            OnProgress(nil, StreamIndex div  UpdateCount, 1000);
          end;
        end;
      except
        ShowMessage(IntToStr(StreamIndex));
        raise
      end;
    end;

    Streams.OwnsObjects := False;
    PackStreams(Streams);
    Streams.OwnsObjects := True;

    UpdateCount := Streams.Count div 1000;
    if UpdateCount = 0 then
    begin
      UpdateCount := 1;
    end;
    StreamIndex := 0;
    for AStream in Streams do
    begin
      if Assigned(OnProgress) then
      begin
        if (StreamIndex mod UpdateCount) = 0 then
        begin
          OnProgress(nil, StreamIndex div  UpdateCount, 1000);
        end;
      end;
      Inc(StreamIndex);
      APoint := AStream.First;
      StreamNumbers[APoint.X,APoint.Y] := AStream.StreamNumber;

    end;
    for AStream in Streams do
    begin
      if Assigned(OnProgress) then
      begin
        if (StreamIndex mod UpdateCount) = 0 then
        begin
          OnProgress(nil, StreamIndex div  UpdateCount, 1000);
        end;
      end;
      Inc(StreamIndex);
      APoint := AStream.Last;
      AStream.DownstreamNumber := StreamNumbers[APoint.X,APoint.Y];
    end;


  finally
    PointQueue.Free;
  end;
  Form6.memo2.Lines.EndUpdate;
end;

procedure ComputeAccumulationSurfModified(const PitlessRaster: IRasterFile;
  const FlowDirections: TFlowDirections; out AccumulatedFlowArea: IRasterFile;
  MinArea: Integer;
  out Streams: TStreamObjectList;
  OnProgress: TProgressEvent;
  MinAreaRaster: IRasterFile = nil
  );
var
  XIndex: Integer;
  YIndex: Integer;
  FlowDir: TFlowDirection;
  GRaster: TTempSurferRaster7File;
  Inputs: TTempIntSurfRaster;
//  IInputs: IRasterFile;
  APoint: TPoint;
  DownstreamPoint: TPoint;
  StreamNumbers: TTempIntSurfRaster;
  StreamCounts: TTempIntSurfRaster;
  StreamArray: array of array of TGeoStream;
  AStream: TGeoStream;
  StreamIndex: Integer;
  FlowArea: Double;
  UsePoint: Boolean;
  AnotherStream: TGeoStream;
  PointIndex: Integer;
  StreamPosition: integer;
  FirstPoint: TPoint;
  AFlowOrd: TFlowOrdinal;
  PointQueue: TFileQueue<TPoint>;
  UpdateCount: Int64;
  Count: Int64;
  MaxQueue: Int64;
  FoundError: Boolean;
  PriorCount: Int64;
  NumberOfInflowingStreams: TTempIntSurfRaster;
//  INumberOfInflowingStreams: IRasterFile;
  procedure AccumulateAPoint(APoint: TPoint);
  var
    AStream: TGeoStream;
  begin
    AStream := TGeoStream.Create;
//    AStream.StreamNumber := Streams.Add(AStream) + 1;
    while Inputs[APoint.X, APoint.Y] = 0 do
    begin
      GRaster.Z[APoint.X,APoint.Y] := GRaster.Z[APoint.X,APoint.Y] + 1;
      FlowDir := FlowDirections[APoint.X,APoint.Y];
      AStream.Add(APoint);

      if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
      begin
        DownstreamPoint.X := APoint.X + FlowDir.DeltaX;
        DownstreamPoint.Y := APoint.Y + FlowDir.DeltaY;
        GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y] :=
          GRaster.Z[DownstreamPoint.X,DownstreamPoint.Y]
          + GRaster.Z[APoint.X,APoint.Y];
//        Dec(Inputs[DownstreamPoint.X,DownstreamPoint.Y]);
        Inputs[DownstreamPoint.X,DownstreamPoint.Y] := Inputs[DownstreamPoint.X,DownstreamPoint.Y] -1;

        if {not FoundError and} (PitlessRaster.Z[DownstreamPoint.X, DownstreamPoint.y]
          > PitlessRaster.Z[APoint.X, APoint.y]) then
        begin
          Form6.memo2.Lines.Add(Format('(%d, %d), (%d, %d)',
            [APoint.X, APoint.y, DownstreamPoint.X,DownstreamPoint.Y]));
            FoundError := True;
        end;

        APoint := DownstreamPoint;
        if (NumberOfInflowingStreams[APoint.X,APoint.Y] > 1)
          and (Inputs[APoint.X, APoint.Y] = 0)
          and (FlowDirections.Ordinals[APoint.X, APoint.Y] <> foMiddle)
          and (StreamArray[APoint.X,APoint.Y] <> nil) then
        begin
          PointQueue.Enqueue(APoint);
          break;
        end;
      end
      else
      begin
        break;
      end;
    end;
    if (AStream.Last.X <> APoint.X) or  (AStream.Last.Y <> APoint.Y) then
    begin
      AStream.Add(APoint);
    end;

    APoint := AStream.First;
    if StreamArray[APoint.X,APoint.Y] <> nil then
    begin
//      StreamArray[APoint.X,APoint.Y,0] := AStream;
      APoint := AStream.Last;
//      StreamArray[APoint.X,APoint.Y] := AStream;
//      Continue;
    end
    else
    begin
      while AStream.Count > 0 do
      begin
        APoint := AStream.First;
        if StreamArray[APoint.X,APoint.Y] <> nil then
        begin
          Break
        end;
        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
        if MinAreaRaster = nil then
        begin
          UsePoint := FlowArea >= MinArea;
        end
        else
        begin
          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
        end;
        if UsePoint then
        begin
          Break;
        end
        else
        begin
          AStream.Delete(0);
        end;
      end;
    end;
    if AStream.Count > 1 then
    begin
      APoint := AStream.Last;
      StreamArray[APoint.X,APoint.Y] := AStream;
      AStream.StreamNumber := Streams.Add(AStream) + 1;
    end
    else
    begin
      AStream.Free;
    end;
  end;
begin
  Inputs := TTempIntSurfRaster.Create(PitlessRaster);
//  IInputs := Inputs;
  StreamNumbers := TTempIntSurfRaster.Create(PitlessRaster);
  StreamCounts := TTempIntSurfRaster.Create(PitlessRaster);
  try
    FoundError := False;
    Form6.memo2.Lines.BeginUpdate;
    Streams := TStreamObjectList.Create;
    GRaster := TTempSurferRaster7File.Create(PitlessRaster);
    AccumulatedFlowArea := GRaster;

//    SetLength(Inputs, PitlessRaster.XCount, PitlessRaster.YCount);
//    SetLength(StreamNumbers, PitlessRaster.XCount, PitlessRaster.YCount);
//    SetLength(StreamCounts, PitlessRaster.XCount, PitlessRaster.YCount);
    SetLength(StreamArray, PitlessRaster.XCount, PitlessRaster.YCount);
    UpdateCount := PitlessRaster.YCount * PitlessRaster.XCount div 1000;
    if UpdateCount = 0 then
    begin
      UpdateCount := 1;
    end;
    Count := 0;
    for YIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for XIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        GRaster.Z[XIndex,YIndex] := 0;
        Inputs[XIndex,YIndex] := 0;
        StreamNumbers[XIndex,YIndex] := 0;
        StreamCounts[XIndex,YIndex] := 0;
        StreamArray[XIndex,YIndex] := nil;
        if Assigned(OnProgress) then
        begin
          Inc(Count);
          if (Count mod UpdateCount) = 0 then
          begin
            OnProgress(nil, Count div UpdateCount, 1000);
          end;
        end;
      end;
    end;

    Count := 0;
    for YIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for XIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        if not GRaster.Ignore[XIndex,YIndex] then
        begin
          FlowDir := FlowDirections[XIndex,YIndex];
          if (FlowDir.DeltaX <> 0) or (FlowDir.DeltaY <> 0) then
          begin
//            Inc(Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY])
            Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY] :=
              Inputs[XIndex+FlowDir.DeltaX,YIndex+FlowDir.DeltaY] + 1;
          end;
        end;
        if Assigned(OnProgress) then
        begin
          Inc(Count);
          if (Count mod UpdateCount) = 0 then
          begin
            OnProgress(nil, Count div UpdateCount, 1000);
          end;
        end;
      end;
    end;
//    NumberOfInflowingStreams := Inputs;
    Inputs.Flush;
    NumberOfInflowingStreams := TTempIntSurfRaster.Create(Inputs.FileName);
//    INumberOfInflowingStreams := NumberOfInflowingStreams;
//    SetLength(NumberOfInflowingStreams, PitlessRaster.XCount, PitlessRaster.YCount);

    PointQueue := TFileQueue<TPoint>.Create;
    try
      Count := 0;
      for YIndex := 0 to PitlessRaster.YCount - 1 do
      begin
        for XIndex := 0 to PitlessRaster.XCount - 1 do
        begin
          if (not GRaster.Ignore[XIndex,YIndex])
            and (NumberOfInflowingStreams[XIndex,YIndex] = 0) then
          begin
            APoint.X := XIndex;
            APoint.Y := YIndex;
            AccumulateAPoint(APoint);
          end;
          if Assigned(OnProgress) then
          begin
            Inc(Count);
            if (Count mod UpdateCount) = 0 then
            begin
              OnProgress(nil, Count div UpdateCount, 1000);
            end;
          end;
        end;
      end;

      MaxQueue := PointQueue.Count;
      UpdateCount := MaxQueue div 1000;
      if UpdateCount = 0 then
      begin
        UpdateCount := 1;
      end;
      PriorCount := MaxQueue;
      while PointQueue.Count > 0 do
      begin
        APoint := PointQueue.Dequeue;
        if Inputs[APoint.X, APoint.Y] = 0 then
        begin
          AccumulateAPoint(APoint);
        end
        else
        begin
          PointQueue.Enqueue(APoint);
        end;
        if Assigned(OnProgress) and (PointQueue.Count <> PriorCount)
          and (UpdateCount <> 0) then
        begin
          PriorCount := PointQueue.Count;
            if ((MaxQueue - PointQueue.Count) mod UpdateCount) = 0 then
            begin
              OnProgress(nil, (MaxQueue - PointQueue.Count) div  UpdateCount, 1000);
            end;
        end;
      end;

      // Delete stream sections with cumulative areas that are too small.

  //    for StreamIndex := 0 to Streams.Count -1 do
  //    begin
  //      AStream := Streams[StreamIndex];
  //      APoint := AStream.First;
  //      if StreamArray[APoint.X,APoint.Y] <> nil then
  //      begin
  //  //      StreamArray[APoint.X,APoint.Y,0] := AStream;
  //        APoint := AStream.Last;
  //        StreamArray[APoint.X,APoint.Y] := AStream;
  //        Continue;
  //      end;
  //      while AStream.Count > 0 do
  //      begin
  //        APoint := AStream.First;
  //        if StreamArray[APoint.X,APoint.Y] <> nil then
  //        begin
  //          Break
  //        end;
  //        FlowArea := AccumulatedFlowArea.Z[APoint.X, APoint.Y];
  //        if MinAreaRaster = nil then
  //        begin
  //          UsePoint := FlowArea >= MinArea;
  //        end
  //        else
  //        begin
  //          UsePoint := FlowArea > MinAreaRaster.Z[APoint.X, APoint.Y];
  //        end;
  //        if UsePoint then
  //        begin
  //          Break;
  //        end
  //        else
  //        begin
  //          AStream.Delete(0);
  //        end;
  //      end;
  //      if AStream.Count > 1 then
  //      begin
  //        APoint := AStream.Last;
  //        StreamArray[APoint.X,APoint.Y] := AStream;
  //      end;
  //    end;
  //    Streams.OwnsObjects := False;
  //    PackStreams(Streams);

      // merge streams when appropriate
      for StreamIndex := 0 to Streams.Count -1 do
      begin
        AStream := Streams[StreamIndex];
        APoint := AStream.First;
        StreamNumbers[APoint.X, APoint.Y] := AStream.StreamNumber;
        APoint := AStream.Last;
//        Inc(StreamCounts[APoint.X, APoint.Y]);
        StreamCounts[APoint.X, APoint.Y] := StreamCounts[APoint.X, APoint.Y] + 1;
      end;
      UpdateCount := Streams.Count div 1000;
      if UpdateCount = 0 then
      begin
        UpdateCount := 1;
      end;
      for StreamIndex := 0 to Streams.Count - 1 do
      begin
        AStream := Streams[StreamIndex];
        try
        if AStream <> nil then
        begin
          repeat
            APoint := AStream.Last;
            AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
            if (StreamCounts[APoint.X, APoint.Y] = 1) then
            begin
              StreamPosition := StreamNumbers[APoint.X, APoint.Y] - 1;
              if StreamPosition >= 0 then
              begin
                AnotherStream := Streams[StreamPosition];
                FirstPoint := AnotherStream.First;
                Assert(APoint.X = FirstPoint.X);
                Assert(APoint.Y = FirstPoint.Y);
                for PointIndex := 1 to AnotherStream.Count - 1 do
                begin
                  AStream.Add(AnotherStream[PointIndex]);
                end;
  //              AnotherStream.Free;
                Streams[StreamPosition] := nil;
              end
              else
              begin
                break;
              end;

              APoint := AStream.Last;
              AFlowOrd := FlowDirections.Ordinals[APoint.X, APoint.Y];
              if (AFlowOrd = foMiddle) then
              begin
                break;
              end;
            end
            else
            begin
              Break;
            end;
          until False;
        end;
        if Assigned(OnProgress) then
        begin
          if (StreamIndex mod UpdateCount) = 0 then
          begin
            OnProgress(nil, StreamIndex div  UpdateCount, 1000);
          end;
        end;
        except
          ShowMessage(IntToStr(StreamIndex));
          raise
        end;
      end;

      Streams.OwnsObjects := False;
      PackStreams(Streams);
      Streams.OwnsObjects := True;

      UpdateCount := Streams.Count div 1000;
      if UpdateCount = 0 then
      begin
        UpdateCount := 1;
      end;
      StreamIndex := 0;
      for AStream in Streams do
      begin
        if Assigned(OnProgress) then
        begin
          if (StreamIndex mod UpdateCount) = 0 then
          begin
            OnProgress(nil, StreamIndex div  UpdateCount, 1000);
          end;
        end;
        Inc(StreamIndex);
        APoint := AStream.First;
        StreamNumbers[APoint.X,APoint.Y] := AStream.StreamNumber;

      end;
      for AStream in Streams do
      begin
        if Assigned(OnProgress) then
        begin
          if (StreamIndex mod UpdateCount) = 0 then
          begin
            OnProgress(nil, StreamIndex div  UpdateCount, 1000);
          end;
        end;
        Inc(StreamIndex);
        APoint := AStream.Last;
        AStream.DownstreamNumber := StreamNumbers[APoint.X,APoint.Y];
      end;


    finally
      PointQueue.Free;
      NumberOfInflowingStreams.Free;
    end;
    Form6.memo2.Lines.EndUpdate;
  finally
    Inputs.Free;
    StreamNumbers.Free;
    StreamCounts.Free;
  end;
end;

{ TIntSurfRaster }

constructor TTempIntSurfRaster.Create(ARaster: IRasterFile);
begin
  Create(ARaster.FileName);
end;

constructor TTempIntSurfRaster.Create(AFileName: string);
begin
  FNewFileName := TempFileName;
  TFile.Copy(AFileName, FNewFileName, True);
  inherited Create(FNewFileName);
end;

destructor TTempIntSurfRaster.Destroy;
begin
  inherited;
  if TFile.Exists(FNewFileName) then
  begin
    TFile.Delete(FNewFileName)
  end;
end;

function TTempIntSurfRaster.GetIntZ(XIndex, YIndex: Integer): Integer;
begin
  result := Round(Z[XIndex, YIndex]);
end;

procedure TTempIntSurfRaster.SetIntZ(XIndex, YIndex: Integer; const Value: Integer);
begin
  Z[XIndex, YIndex] := Value;
end;

end.
