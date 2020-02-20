unit AlternateAlgorithm;

interface

uses
  RasterValuesAlongSegmentsUnit, System.IOUtils, GenericRasterUnit, System.Math,
  System.Types, System.SysUtils, System.Classes, System.Generics.Collections,
  TempFiles, System.Generics.Defaults, SurferGridFileReaderUnit;

type
  TPitTranslation = record
    ID: Integer;
    PitLocationID: Int64;
  end;

  TPitTranslationList = TList<TPitTranslation>;

  TEdgeType = (etNotEdge, etEdgeOfActive, etBoundary);


  TSpillPoint = record
    PitID: Integer;
    Location: Int64;
    Z: double;
    EdgeType: TEdgeType;
  end;

  TPit = record
    Location: Int64;
    Z: double;
  end;

  TPitList = TList<TPit>;

  TSpillPointList = TList<TSpillPoint>;

  TNeighbors = record
  private
    function GetNeighbor(Index: Integer): Integer;
    procedure SetNeighbor(Index: Integer; const Value: Integer);
  public
    Count: Integer;
    Neighor0: Integer;
    Neighor1: Integer;
    Neighor2: Integer;
    Neighor3: Integer;
    Neighor4: Integer;
    Neighor5: Integer;
    Neighor6: Integer;
    Neighor7: Integer;
    property Neighbors[Index: Integer]: Integer read GetNeighbor write SetNeighbor;
  end;

  TPointZ = record
    X: Integer;
    Y: Integer;
    Z: double;
  end;

  TPointZList = TList<TPointZ>;

function IdToLocation(ID: Int64; Raster: TSurferRaster7File2): TPoint;

procedure AssignFlowDirections(const ElevationRasterFileName,
  FlowDirectionsFileName: string; PitList: TPitList);

procedure AssignNeighbors(const FlowDirectionsFileName, NeighborFileName: string);

procedure AssignEndPoints(const FlowDirectionsFileName, NeighborFileName,
  EndPointFileName: string; PitTranslationList: TPitTranslationList);

procedure GetEdgePoints(const EndPointFileName, EdgePointBooleanFileName: string;
  EdgePointListFileNames: TStrings);

procedure GetSpillPoints(const ElevationRasterFileName, EndPointFileName,
  EdgePointBooleanFileName: string; EdgePointListFileNames: TStrings;
  SpillPoints: TSpillPointList);

procedure MergePits(SpillPoints: TSpillPointList; const ElevationFileName,
  FlowDirectionsFileName, EndPointFileName: string;
  EdgePointListFileNames: TStrings; PitTranslationList: TPitTranslationList;
  var SpillPits: TArray<TSpillPoint>; PitList: TPitList);

procedure HandlePits(const ElevationFileName, FlowDirectionsFileName,
  EndPointFileName, PitlessElevationFileName: string;
  SpillPits: TArray<TSpillPoint>; PitList: TPitList);

implementation

type
  TFlowDirection = record
  strict private
    FDeltaX: integer;
    FDeltaY: Integer;
    procedure SetDeltaX(const Value: integer);
    procedure SetDeltaY(const Value: Integer);
  public
    property DeltaX: integer read FDeltaX write SetDeltaX;
    property DeltaY: Integer read FDeltaY write SetDeltaY;
  end;

function LocationToID(APoint: TPoint; Raster: TSurferRaster7File2): Int64;
begin
  Assert(APoint.X >= 0);
  Assert(APoint.Y >= 0);
  Assert(APoint.X < Raster.XCount);
  Assert(APoint.Y < Raster.YCount);
  Result := APoint.Y * Raster.XCount + APoint.X;
end;

function IdToLocation(ID: Int64; Raster: TSurferRaster7File2): TPoint;
begin
  Assert(ID >= 0);
  Assert(ID < Raster.XCount*Raster.YCount);
  Result.Y := ID div Raster.XCount;
  Result.X := ID mod Raster.XCount;
end;

procedure AssignFlowDirections(const ElevationRasterFileName,
  FlowDirectionsFileName: string; PitList: TPitList);
const
  FlowOrdinals:  array[-1..1, -1..1] of TFlowOrdinal
//    = ((foUpperLeft, foUpper,  foUpperRight),
//       (foLeft,      foMiddle, foRight),
//       (foLowerLeft, foLower,  foLowerRight));
//    = ((foUpperLeft, foLeft,  foLowerLeft),
//       (foUpper,      foMiddle, foLower),
//       (foUpperRight, foRight,  foLowerRight));
    = ((foLowerLeft, foLeft,  foUpperLeft),
       (foLower,      foMiddle, foUpper),
       (foLowerRight, foRight,  foUpperRight));
var
  ElevationRaster: TSurferRaster7File2;
  FlowDirections: TSurferRaster7File2;
  YIndex: Integer;
  XIndex: Integer;
  StartY: Integer;
  EndY: Integer;
  StartX: Integer;
  EndX: Integer;
  LowPoint: TPoint;
  FoundFirst: Boolean;
  LowZ: Double;
  InnerYIndex: Integer;
  InnerXIndex: Integer;
  TestZ: Double;
  Header: TGrid7Header;
  APit: TPit;
  procedure AssignOrdinal;
  begin
    LowPoint.X := LowPoint.X-XIndex;
    LowPoint.Y := LowPoint.Y-YIndex;
    FlowDirections.Z[XIndex,YIndex] :=
      Ord(FlowOrdinals[LowPoint.X, LowPoint.Y]);
  end;
  procedure AddPit;
  begin
    APit.Location := LocationToID(Point(XIndex,YIndex), ElevationRaster);
    APit.Z := ElevationRaster.Z[XIndex,YIndex];
    PitList.Add(APit);
  end;
begin
  Assert(TFile.Exists(ElevationRasterFileName));
  TFile.Copy(ElevationRasterFileName,  FlowDirectionsFileName, True);
  ElevationRaster := TSurferRaster7File2.Create(ElevationRasterFileName);
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  try
    for YIndex := 0 to ElevationRaster.YCount - 1 do
    begin
      StartY := Max(0, YIndex-1);
      EndY := Min(ElevationRaster.YCount - 1, YIndex+1);
      for XIndex := 0 to ElevationRaster.XCount - 1 do
      begin
        if not ElevationRaster.Ignore[XIndex,YIndex] then
        begin
          StartX := Max(0, XIndex-1);
          EndX := Min(ElevationRaster.XCount - 1, XIndex+1);
          LowPoint.X := XIndex;
          LowPoint.Y := YIndex;
          FoundFirst := False;
          LowZ := ElevationRaster.Z[XIndex,YIndex];
          for InnerYIndex := StartY to EndY do
          begin
            for InnerXIndex := StartX to EndX do
            begin
              if (InnerXIndex = XIndex) and (InnerYIndex = YIndex) then
              begin
                Continue;
              end;
              TestZ := ElevationRaster.Z[InnerXIndex,InnerYIndex];
//              if FoundFirst then
              begin
                if TestZ <= LowZ then
                begin
                  LowZ := TestZ;
                  LowPoint.X := InnerXIndex;
                  LowPoint.Y := InnerYIndex;
                  FoundFirst := True;
                end;
//              end
//              else
//              begin
//                LowZ := TestZ;
//                LowPoint.X := InnerXIndex;
//                LowPoint.Y := InnerYIndex;
//                FoundFirst := True;
              end;
            end;
          end;
          if FoundFirst then
          begin
            if LowZ = ElevationRaster.Z[XIndex,YIndex] then
            begin
              AddPit;
              if LowPoint.Y > YIndex then
              begin
                AssignOrdinal;
              end
              else if (LowPoint.X > XIndex) and (LowPoint.Y <= YIndex) then
              begin
                if LowPoint.Y < YIndex then
                begin
                  FlowDirections.Z[XIndex,YIndex] := Ord(foMiddle);
                end
                else
                begin
                  AssignOrdinal;
                end;
              end
              else
              begin
                FlowDirections.Z[XIndex,YIndex] := Ord(foMiddle);
              end;
            end
            else
            begin
              AssignOrdinal;
              if (TFlowOrdinal(Round(FlowDirections.Z[XIndex,YIndex])) = foMiddle) then
              begin
                AddPit;
              end;
            end;
          end
          else
          begin
            FlowDirections.Z[XIndex,YIndex] := Ord(foMiddle);
            AddPit;
          end;
        end;
      end;
    end;
    Header := FlowDirections.Header;
    Header.zMin := Ord(Low(TFlowOrdinal));
    Header.zMax := Ord(High(TFlowOrdinal));
    FlowDirections.Header := Header;
  finally
    ElevationRaster.Free;
    FlowDirections.Free;
  end;
end;

{ TNeighbors }

function TNeighbors.GetNeighbor(Index: Integer): Integer;
begin
  result := -1;
  Assert(index in [0..7]);
  case Index of
    0: Result := Neighor0;
    1: Result := Neighor1;
    2: Result := Neighor2;
    3: Result := Neighor3;
    4: Result := Neighor4;
    5: Result := Neighor5;
    6: Result := Neighor6;
    7: Result := Neighor7;
  end;
end;

procedure TNeighbors.SetNeighbor(Index: Integer; const Value: Integer);
begin
  Assert(index in [0..7]);
  case Index of
    0:
      Neighor0 := Value;
    1:
      Neighor1 := Value;
    2:
      Neighor2 := Value;
    3:
      Neighor3 := Value;
    4:
      Neighor4 := Value;
    5:
      Neighor5 := Value;
    6:
      Neighor6 := Value;
    7:
      Neighor7 := Value;
  end;
end;

procedure AssignNeighbors(const FlowDirectionsFileName, NeighborFileName: string);
const
  FlowValues: array[TFlowOrdinal] of TFlowDirection
//    = ((FDeltaX : -1; FDeltaY : -1), (FDeltaX : 0; FDeltaY : -1), (FDeltaX : 1; FDeltaY : -1),
//       (FDeltaX : -1; FDeltaY :  0), (FDeltaX : 0; FDeltaY :  0), (FDeltaX : 1; FDeltaY :  0),
//       (FDeltaX : -1; FDeltaY :  1), (FDeltaX : 0; FDeltaY :  1), (FDeltaX : 1; FDeltaY :  1));
//    = ((FDeltaX : -1; FDeltaY : -1), (FDeltaX : -1; FDeltaY : 0), (FDeltaX : -1; FDeltaY : 1),
//       (FDeltaX :  0; FDeltaY : -1), (FDeltaX :  0; FDeltaY : 0), (FDeltaX :  0; FDeltaY : 1),
//       (FDeltaX :  1; FDeltaY : -1), (FDeltaX :  1; FDeltaY : 0), (FDeltaX :  1; FDeltaY : 1));
    = (
       (FDeltaX : -1; FDeltaY : +1), (FDeltaX :  0; FDeltaY : +1), (FDeltaX : +1; FDeltaY : +1),
       (FDeltaX : -1; FDeltaY :  0), (FDeltaX :  0; FDeltaY :  0), (FDeltaX : +1; FDeltaY :  0),
       (FDeltaX : -1; FDeltaY : -1), (FDeltaX :  0; FDeltaY : -1), (FDeltaX : +1; FDeltaY : -1)
      );
var
  FlowDirections: TSurferRaster7File2;
  YIndex: Integer;
  StartY: Integer;
  EndY: Integer;
  XIndex: Integer;
  StartX: Integer;
  EndX: Integer;
  AFlowOrdinal: TFlowOrdinal;
  InnerYIndex: Integer;
  InnerXIndex: Integer;
  AFlowDirection: TFlowDirection;
  NeighborFile: TFileStream;
  Neighbors: TNeighbors;
  APoint: TPoint;
  ID: Int64;
begin
  Assert(TFile.Exists(FlowDirectionsFileName));
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  NeighborFile := TFile.Create(NeighborFileName);
  try
    for YIndex := 0 to FlowDirections.YCount - 1 do
    begin
      StartY := Max(0, YIndex-1);
      EndY := Min(FlowDirections.YCount - 1, YIndex+1);
      for XIndex := 0 to FlowDirections.XCount - 1 do
      begin
        Neighbors.Count := 0;
        if not FlowDirections.Ignore[XIndex,YIndex] then
        begin
          StartX := Max(0, XIndex-1);
          EndX := Min(FlowDirections.XCount - 1, XIndex+1);
          for InnerYIndex := StartY to EndY do
          begin
            for InnerXIndex := StartX to EndX do
            begin
              if (InnerYIndex = YIndex) and (InnerXIndex = XIndex) then
              begin
                Continue;
              end;
              if not FlowDirections.Ignore[InnerXIndex,InnerYIndex] then
              begin
                AFlowOrdinal := TFlowOrdinal(Round(
                  FlowDirections.Z[InnerXIndex,InnerYIndex]));
                AFlowDirection := FlowValues[AFlowOrdinal];
                if (AFlowDirection.DeltaX + InnerXIndex = XIndex)
                  and (AFlowDirection.DeltaY + InnerYIndex = YIndex) then
                begin
                  APoint.X := InnerXIndex;
                  APoint.Y := InnerYIndex;
                  ID := LocationToID(APoint, FlowDirections);
                  Neighbors.Neighbors[Neighbors.Count] := ID;
                  Inc(Neighbors.Count);
                end;
              end;
            end;
          end;
        end;
        NeighborFile.Write(Neighbors, SizeOf(Neighbors));
      end;
    end;
  finally
    FlowDirections.Free;
    NeighborFile.Free;
  end;

end;

{ TFlowDirection }

procedure TFlowDirection.SetDeltaX(const Value: integer);
begin
  Assert((Value >= -1) and (Value <= 1));
  FDeltaX := Value;
end;

procedure TFlowDirection.SetDeltaY(const Value: Integer);
begin
  Assert((Value >= -1) and (Value <= 1));
  FDeltaY := Value;
end;

procedure AssignEndPoints(const FlowDirectionsFileName, NeighborFileName,
  EndPointFileName: string; PitTranslationList: TPitTranslationList);
var
  FlowDirections: TSurferRaster7File2;
  EndPoints: TSurferRaster7File2;
  NeighborFile: TFileStream;
  YIndex: Integer;
  XIndex: Integer;
  FlowOrdinal: TFlowOrdinal;
  APoint: TPoint;
  ID: Int64;
  NeighborQueue: TQueue<Int64>;
  ANeighbor: TNeighbors;
  PitID: Int64;
  NeighborIndex: Integer;
  MaxPit: Int64;
  Header: TGrid7Header;
  PitTranslation: TPitTranslation;
begin
  Assert(TFile.Exists(FlowDirectionsFileName));
  Assert(TFile.Exists(NeighborFileName));

  TFile.Copy(FlowDirectionsFileName,EndPointFileName, True);
  PitTranslationList.Clear;

  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  EndPoints := TSurferRaster7File2.Create(EndPointFileName);
  NeighborFile := TFile.Open(NeighborFileName, TFileMode.fmOpen);
  try
    for YIndex := FlowDirections.YCount - 1 downto 0 do
    begin
      for XIndex := 0 to FlowDirections.XCount - 1 do
      begin
        if not FlowDirections.Ignore[XIndex,YIndex] then
        begin
          EndPoints.Z[XIndex,YIndex] := -1;
        end;
      end;
    end;

    MaxPit := 0;
    NeighborQueue := TQueue<Int64>.Create;
    try
      for YIndex := 0 to FlowDirections.YCount - 1 do
      begin
        for XIndex := 0 to FlowDirections.XCount - 1 do
        begin
          if not FlowDirections.Ignore[XIndex,YIndex] then
          begin
            FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[XIndex,YIndex]));
            if FlowOrdinal = foMiddle then
            begin
              APoint.X := XIndex;
              APoint.Y := YIndex;
              PitID := LocationToID(APoint, FlowDirections);
              PitTranslation.ID := MaxPit;
              PitTranslation.PitLocationID := PitID;
              PitTranslationList.Add(PitTranslation);
              NeighborQueue.Enqueue(PitID);
              while NeighborQueue.Count > 0 do
              begin
                ID := NeighborQueue.Dequeue;
                APoint := IdToLocation(ID, FlowDirections);
                EndPoints.Z[APoint.X,APoint.Y] := MaxPit;
                NeighborFile.Seek(ID * SizeOf(TNeighbors), TSeekOrigin.soBeginning);
                NeighborFile.Read(ANeighbor, SizeOf(TNeighbors));
                for NeighborIndex := 0 to ANeighbor.Count - 1 do
                begin
                  NeighborQueue.Enqueue(ANeighbor.Neighbors[NeighborIndex]);
                end;
              end;
              Inc(MaxPit);
            end;
          end;
        end;
      end;
      Header := EndPoints.Header;
      Header.zMin := -1;
      Header.zMax := MaxPit-1;
      EndPoints.Header := Header;
    finally
      NeighborQueue.Free;
    end;
  finally
    FlowDirections.Free;
    EndPoints.Free;
    NeighborFile.Free;
  end;
end;

procedure GetEdgePoints(const EndPointFileName, EdgePointBooleanFileName: string;
  EdgePointListFileNames: TStrings);
var
  EndPoints: TSurferRaster7File2;
  PitIndex: Integer;
  FileStreams: TObjectList<TFileStream>;
  AFileName: string;
  AFileStream: TFileStream;
  YIndex: Integer;
  StartY: Integer;
  EndY: Integer;
  XIndex: Integer;
  StartX: Integer;
  EndX: Integer;
  ID: Integer;
  InnerYIndex: Integer;
  InnerXIndex: Integer;
  EdgeID: Int64;
  IsEdge: Boolean;
  TestID: Integer;
  APoint: TPoint;
  IntArray: TArray<Int64>;
  Count: Integer;
  IntList: TList<Int64>;
  EdgePointBoolean: TSurferRaster7File2;
  Header: TGrid7Header;
  EdgeType: TEdgeType;
//  IntArray2: TArray<Int64>;
begin
  Assert(TFile.Exists(EndPointFileName));
  TFile.Copy(EndPointFileName, EdgePointBooleanFileName, True);
  EndPoints := TSurferRaster7File2.Create(EndPointFileName);
  EdgePointBoolean := TSurferRaster7File2.Create(EdgePointBooleanFileName);
  FileStreams := TObjectList<TFileStream>.Create;
  try
    for PitIndex := 0 to Round(EndPoints.Header.zMax) do
    begin
      AFileName := TempFileName;
      EdgePointListFileNames.Add(AFileName);
      AFileStream := TFile.Open(AFileName, TFileMode.fmOpenOrCreate);
      FileStreams.Add(AFileStream);
    end;

    for YIndex := 0 to EndPoints.YCount - 1 do
    begin
      StartY := Max(0, YIndex-1);
      EndY := Min(EndPoints.YCount - 1, YIndex+1);
      for XIndex := 0 to EndPoints.XCount - 1 do
      begin
        if not EndPoints.Ignore[XIndex,YIndex] then
        begin
          ID := Round(EndPoints.Z[XIndex,YIndex]);
          StartX := Max(0, XIndex-1);
          EndX := Min(EndPoints.XCount - 1, XIndex+1);

          IsEdge := (XIndex = 0) or (XIndex = EndPoints.XCount - 1)
            or (YIndex = 0) or (YIndex = EndPoints.YCount - 1);
          if IsEdge then
          begin
            EdgeType := etEdgeOfActive;
          end
          else
          begin
            EdgeType := etNotEdge;
            for InnerYIndex := StartY to EndY do
            begin
              for InnerXIndex := StartX to EndX do
              begin
                if EndPoints.Ignore[InnerXIndex,InnerYIndex] then
                begin
//                  IsEdge := True;
                  EdgeType := etEdgeOfActive;
                  break;
                end
                else
                begin
                  TestID := Round(EndPoints.Z[InnerXIndex,InnerYIndex]);
                  if TestID <> ID then
                  begin
//                    IsEdge := True;
                    if EdgeType <> etEdgeOfActive then
                    begin
                      EdgeType := etBoundary;
                    end;
                  end;
                end;
//                if IsEdge then
//                begin
//                  break;
//                end;
              end;
              if EdgeType = etEdgeOfActive then
              begin
                break;
              end;
//              if IsEdge then
//              begin
//                break;
//              end;
            end;
          end;
          if EdgeType in [etEdgeOfActive, etBoundary] then
          begin
            AFileStream := FileStreams[ID];
            APoint.X := XIndex;
            APoint.Y := YIndex;
            EdgeID := LocationToID(APoint, EndPoints);
            AFileStream.Write(EdgeID, SizeOf(EdgeID));
          end;
//          else
//          begin
            EdgePointBoolean.Z[XIndex,YIndex] := Ord(EdgeType);
//          end;
        end;
      end;
    end;

    IntList := TList<Int64>.Create;
    try
      for PitIndex := 0 to FileStreams.Count -1 do
      begin
        AFileStream := FileStreams[PitIndex];
        Count := AFileStream.Size div SizeOf(Int64);
        if Count > 0 then
        begin
          SetLength(IntArray, Count);
          AFileStream.Seek(0, TSeekOrigin.soBeginning);
          AFileStream.Read(IntArray[0], AFileStream.Size);
          IntList.Clear;
          IntList.AddRange(IntArray);
          IntList.Sort;
          IntArray := IntList.ToArray;
          AFileStream.Seek(0, TSeekOrigin.soBeginning);
          AFileStream.Write(IntArray[0], Count*SizeOf(Int64));
        end;
      end;
    finally
      IntList.Free;
    end;
    Header := EdgePointBoolean.Header;
    Header.zMin := Ord(False);
    Header.zMax := Ord(True);
    EdgePointBoolean.Header := Header;
  finally
    EdgePointBoolean.Free;
    EndPoints.Free;
    FileStreams.Free;
  end;
end;

procedure GetSpillPoints(const ElevationRasterFileName, EndPointFileName,
  EdgePointBooleanFileName: string; EdgePointListFileNames: TStrings;
  SpillPoints: TSpillPointList);
var
  ElevationRaster: TSurferRaster7File2;
  EndPoints: TSurferRaster7File2;
  EdgePointBoolean: TSurferRaster7File2;
  PitIndex: Integer;
  FileStreams: TObjectList<TFileStream>;
//  AFileName: string;
//  AFileStream: TFileStream;
  ASpillPoint: TSpillPoint;
  XIndex: Integer;
  YIndex: Integer;
  Edgetype: TEdgeType;
  PitID: Integer;
  Elevation: Double;
  APoint: TPoint;
begin
  Assert(TFile.Exists(ElevationRasterFileName));
  Assert(TFile.Exists(EndPointFileName));
  Assert(TFile.Exists(EdgePointBooleanFileName));
  ElevationRaster := TSurferRaster7File2.Create(ElevationRasterFileName);
  EndPoints := TSurferRaster7File2.Create(EndPointFileName);
  EdgePointBoolean := TSurferRaster7File2.Create(EdgePointBooleanFileName);
  FileStreams := TObjectList<TFileStream>.Create;
  try
    ASpillPoint.PitID := -1;
    ASpillPoint.Location := -1;
    ASpillPoint.Z := 0;
    SpillPoints.Clear;
    SpillPoints.Capacity := EdgePointListFileNames.Count;

    for PitIndex := 0 to EdgePointListFileNames.Count -1 do
    begin
      SpillPoints.Add(ASpillPoint);
    end;
    for YIndex := 0 to EndPoints.YCount - 1 do
    begin
      for XIndex := 0 to EndPoints.XCount - 1 do
      begin
        if not EdgePointBoolean.Ignore[XIndex, YIndex] then
        begin
          Edgetype := TEdgeType(Round(EdgePointBoolean.Z[XIndex, YIndex]));
          if Edgetype in [etEdgeOfActive, etBoundary] then
          begin
            PitID := Round(EndPoints.Z[XIndex, YIndex]);
            ASpillPoint := SpillPoints[PitID];
            Elevation := ElevationRaster.Z[XIndex, YIndex];
            if ASpillPoint.PitID < 0 then
            begin
              ASpillPoint.PitID := PitID;
              ASpillPoint.Z := Elevation;
              APoint.X := XIndex;
              APoint.Y := YIndex;
              ASpillPoint.Location := LocationToID(APoint, ElevationRaster);
              ASpillPoint.EdgeType := Edgetype;
              SpillPoints[PitID] := ASpillPoint;
            end
            else if Elevation < ASpillPoint.Z then
            begin
              ASpillPoint.Z := Elevation;
              APoint.X := XIndex;
              APoint.Y := YIndex;
              ASpillPoint.Location := LocationToID(APoint, ElevationRaster);
              ASpillPoint.EdgeType := Edgetype;
              SpillPoints[PitID] := ASpillPoint;
            end
            else if (Elevation = ASpillPoint.Z)
              and (ASpillPoint.EdgeType = etBoundary)
              and (Edgetype = etEdgeOfActive) then
            begin
              ASpillPoint.Z := Elevation;
              APoint.X := XIndex;
              APoint.Y := YIndex;
              ASpillPoint.Location := LocationToID(APoint, ElevationRaster);
              ASpillPoint.EdgeType := Edgetype;
              SpillPoints[PitID] := ASpillPoint;
            end;
          end;
        end;
      end;
    end;
//    SpillPoints.Sort;
  finally
    ElevationRaster.Free;
    EdgePointBoolean.Free;
    EndPoints.Free;
    FileStreams.Free;
  end;
end;

procedure MergePits(SpillPoints: TSpillPointList; const ElevationFileName,
  FlowDirectionsFileName, EndPointFileName: string;
  EdgePointListFileNames: TStrings; PitTranslationList: TPitTranslationList;
  var SpillPits: TArray<TSpillPoint>; PitList: TPitList);
const
  FlowOrdinals:  array[-1..1, -1..1] of TFlowOrdinal
    = ((foLowerLeft, foLeft,  foUpperLeft),
       (foLower,      foMiddle, foUpper),
       (foLowerRight, foRight,  foUpperRight));
  FlowValues: array[TFlowOrdinal] of TFlowDirection
    = (
       (FDeltaX : -1; FDeltaY : +1), (FDeltaX :  0; FDeltaY : +1), (FDeltaX : +1; FDeltaY : +1),
       (FDeltaX : -1; FDeltaY :  0), (FDeltaX :  0; FDeltaY :  0), (FDeltaX : +1; FDeltaY :  0),
       (FDeltaX : -1; FDeltaY : -1), (FDeltaX :  0; FDeltaY : -1), (FDeltaX : +1; FDeltaY : -1)
      );
var
  Eliminated: array of Boolean;
  index: Integer;
  ASpillPoint: TSpillPoint;
  EndPoints: TSurferRaster7File2;
  FlowDirections: TSurferRaster7File2;
  ID: Int64;
  APoint: TPoint;
  PitEdgePoint: TPoint;
  IsEdge: Boolean;
  StartY: Integer;
  EndY: Integer;
  YIndex: Integer;
  XIndex: Integer;
  PitID: Int64;
  Elevations: TSurferRaster7File2;
  Elevation: Double;
  StartX: Integer;
  EndX: Integer;
  TestPitID: Int64;
  FoundFirst: Boolean;
  MergePoint: TPoint;
  TestElevation: Double;
  AFlowOrdinal: TFlowOrdinal;
  NewFlowOrdinal: TFlowOrdinal;
  AFlowDirection: TFlowDirection;
  NextPoint: TPoint;
  OtherPit: Int64;
  PitIndex: Integer;
  AFileName: string;
  AFileStream: TFileStream;
  FileStreams: TObjectList<TFileStream>;
  Count: Int64;
  OtherPitEdgePoints: TArray<Int64>;
  PointIndex: Integer;
  Point1: Int64;
  Point2: Int64;
  Location1: TPoint;
  Location2: TPoint;
  ExistingPitEdgePoints: TArray<Int64>;
  NewEdgePoints: TList<Int64>;
  CurrentCount: Integer;
  EdgePointIndex: Integer;
  AnId: Int64;
  SpillPointArray: TArray<TSpillPoint>;
  Comparer: IComparer<TSpillPoint>;
  FoundIndex: Integer;

  RealOtherPit: Int64;
  PitZone: Int64;
  Found: Boolean;
  NextFlowOrdinal: TFlowOrdinal;
  APit: TPit;
  function IsEdgePoint(APoint: TPoint): Boolean;
  var
    StartY: Integer;
    EndY: Integer;
    StartX: Integer;
    EndX: Integer;
    YIndex: Integer;
    XIndex: Integer;
    PitID: Int64;
  begin
    result := (APoint.X = 0) or (APoint.X = EndPoints.XCount-1)
      or (APoint.Y = 0) or (APoint.Y = EndPoints.YCount-1);
    if not result then
    begin
      StartY := Max(0, APoint.Y-1);
      EndY := Min(EndPoints.YCount - 1, APoint.Y+1);
      StartX := Max(0, APoint.X-1);
      EndX := Min(EndPoints.XCount - 1, APoint.X+1);
      PitID := Round(EndPoints.Z[APoint.X, APoint.Y]);
      for YIndex := StartY to EndY do
      begin
        for XIndex := StartX to EndX do
        begin
          if (APoint.Y = YIndex) and (APoint.X = XIndex) then
          begin
            Continue;
          end;
          Result := EndPoints.Ignore[XIndex,YIndex]
            or (PitID <> Round(EndPoints.Z[XIndex,YIndex]));
          if Result then
          begin
            Exit;
          end;
        end;
      end;
    end;

  end;
begin
  Comparer := TComparer<TSpillPoint>.Construct(
    function (const Left, Right: TSpillPoint): Integer
    begin
      result := Sign(Right.Z - Left.Z);
    end);
  SetLength(Eliminated, SpillPoints.Count);
  for index := 0 to Length(Eliminated) - 1 do
  begin
    Eliminated[index] := False;
  end;
  SpillPits := SpillPoints.ToArray;
  SpillPoints.Sort(Comparer);

  Assert(TFile.Exists(ElevationFileName));
  Assert(TFile.Exists(EndPointFileName));
  Assert(TFile.Exists(FlowDirectionsFileName));
  Elevations := TSurferRaster7File2.Create(ElevationFileName);
  EndPoints := TSurferRaster7File2.Create(EndPointFileName);
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  FileStreams := TObjectList<TFileStream>.Create;
  try
    for PitIndex := 0 to EdgePointListFileNames.Count -1 do
    begin
      AFileName := EdgePointListFileNames[PitIndex];
      AFileStream := TFile.Open(AFileName, TFileMode.fmOpen);
      FileStreams.Add(AFileStream);
    end;

    index := 0;
    while index < SpillPoints.Count do
    begin
      ASpillPoint := SpillPoints[index];
      try
        if Eliminated[ASpillPoint.PitID] then
        begin
          Continue;
        end;
        ID := SpillPits[ASpillPoint.PitID].Location;
        APoint := IdToLocation(ID, EndPoints);
        PitEdgePoint := APoint;
        IsEdge := (APoint.X = 0) or (APoint.X = EndPoints.XCount-1)
          or (APoint.Y = 0) or (APoint.Y = EndPoints.YCount-1);
        if IsEdge then
        begin
          MergePoint := APoint;
        end
        else
        begin
          StartY := Max(0, APoint.Y-1);
          EndY := Min(EndPoints.YCount - 1, APoint.Y+1);
          StartX := Max(0, APoint.X-1);
          EndX := Min(EndPoints.XCount - 1, APoint.X+1);
          PitID := Round(EndPoints.Z[APoint.X, APoint.Y]);
          FoundFirst := False;
          for YIndex := StartY to EndY do
          begin
            if IsEdge then
            begin
              Break;
            end;
            for XIndex := StartX to EndX do
            begin
              if EndPoints.Ignore[XIndex,YIndex] then
              begin
                IsEdge := True;
                Break;
              end
              else
              begin
                TestPitID := Round(EndPoints.Z[XIndex, YIndex]);
                if TestPitID <> PitID then
                begin
                  TestElevation := Elevations.Z[XIndex, YIndex];
                  if FoundFirst then
                  begin
                    if TestElevation < Elevation then
                    begin
                      MergePoint.X := XIndex;
                      MergePoint.Y := YIndex;
                      Elevation := TestElevation;
                    end;
                  end
                  else
                  begin
                    MergePoint.X := XIndex;
                    MergePoint.Y := YIndex;
                    Elevation := TestElevation;
                    FoundFirst := True;
                  end;
                end;
              end;
            end;
          end;
        end;

        if not IsEdge then
        begin
          APoint := PitEdgePoint;
          AFlowDirection.DeltaX := MergePoint.X - PitEdgePoint.X;
          AFlowDirection.DeltaY := MergePoint.Y - PitEdgePoint.Y;
          NewFlowOrdinal := FlowOrdinals[AFlowDirection.DeltaX, AFlowDirection.DeltaY];
          AFlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APoint.X, APoint.Y]));
          FlowDirections.Z[APoint.X, APoint.Y] := Ord(NewFlowOrdinal);
          APit.Location := LocationToID(Point(APoint.X, APoint.Y), FlowDirections);
          APit.Z := Elevations.Z[APoint.X, APoint.Y];
          PitList.Add(APit);
          while AFlowOrdinal <> foMiddle do
          begin
            AFlowDirection := FlowValues[AFlowOrdinal];
            NextPoint.X := APoint.X + AFlowDirection.DeltaX;
            NextPoint.Y := APoint.Y + AFlowDirection.DeltaY;
            AFlowDirection.DeltaX := -AFlowDirection.DeltaX;
            AFlowDirection.DeltaY := -AFlowDirection.DeltaY;
            AFlowOrdinal := FlowOrdinals[AFlowDirection.DeltaX, AFlowDirection.DeltaY];
            NextFlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[NextPoint.X, NextPoint.Y]));
            FlowDirections.Z[NextPoint.X, NextPoint.Y] := Ord(AFlowOrdinal);
            AFlowOrdinal := NextFlowOrdinal;
//            if AFlowOrdinal = foMiddle then
//            begin
//              AFlowDirection.DeltaX := APoint.X - NextPoint.X;
//              AFlowDirection.DeltaY := APoint.Y - NextPoint.Y;
//              NewFlowOrdinal := FlowOrdinals[AFlowDirection.DeltaX, AFlowDirection.DeltaY];
//              FlowDirections.Z[NextPoint.X, NextPoint.Y] := Ord(NewFlowOrdinal);
//            end;
            APoint := NextPoint;
            APit.Location := LocationToID(Point(APoint.X, APoint.Y), FlowDirections);
            APit.Z := Elevations.Z[APoint.X, APoint.Y];
            PitList.Add(APit);
          end;
        end;

        RealOtherPit := Round(EndPoints.Z[MergePoint.X, MergePoint.Y]);
        if not IsEdge then
        begin
          OtherPit := Round(EndPoints.Z[PitEdgePoint.X, PitEdgePoint.Y]);
          Eliminated[OtherPit] := True;
          AFileStream := FileStreams[OtherPit];
          Count := AFileStream.Size div SizeOf(Int64);
//          Assert(not Odd(Count));
          SetLength(OtherPitEdgePoints, Count);
          AFileStream.Seek(0, TSeekOrigin.soBeginning);
          AFileStream.Read(OtherPitEdgePoints[0], Count*SizeOf(Int64));
          if Count = 1 then
          begin
            Point1 := OtherPitEdgePoints[0];
            Location1 := IdToLocation(Point1, FlowDirections);
            EndPoints.Z[Location1.X, Location1.Y] := RealOtherPit;
          end;
          for PointIndex := 1 to Length(OtherPitEdgePoints) - 1 do
          begin
//            if Odd(PointIndex) then
            begin
              Point1 := OtherPitEdgePoints[PointIndex-1];
              Point2 := OtherPitEdgePoints[PointIndex];
              Location1 := IdToLocation(Point1, FlowDirections);
              Location2 := IdToLocation(Point2, FlowDirections);
              if (Location1.Y = Location2.Y)
                and (OtherPit = Round(EndPoints.Z[Location2.X, Location2.Y])) then
              begin
                Assert(Location1.X < Location2.X);
                for XIndex := Location1.X to Location2.X do
                begin
                  PitZone := Round(EndPoints.Z[XIndex, Location1.Y]);
                  if (PitZone <> OtherPit) and (PitZone <> RealOtherPit) then
                  begin
                    PitZone := Round(EndPoints.Z[Location2.X, Location2.Y]);
                    Assert((PitZone = OtherPit) or (PitZone = RealOtherPit));
                    EndPoints.Z[Location2.X, Location2.Y] := RealOtherPit;
                    Break;
                  end;
//                  Assert(OtherPit = Round(EndPoints.Z[XIndex, Location1.Y]));
                  EndPoints.Z[XIndex, Location1.Y] := RealOtherPit;
                end;
              end
              else
              begin
                PitZone := Round(EndPoints.Z[Location1.X, Location1.Y]);
                Assert((PitZone = OtherPit) or (PitZone = RealOtherPit));
                EndPoints.Z[Location1.X, Location1.Y] := RealOtherPit;

                PitZone := Round(EndPoints.Z[Location2.X, Location2.Y]);
                Assert((PitZone = OtherPit) or (PitZone = RealOtherPit));
                EndPoints.Z[Location2.X, Location2.Y] := RealOtherPit;
              end;
            end;
          end;

          AFileStream := FileStreams[RealOtherPit];
          Count := AFileStream.Size div SizeOf(Int64);
//          Assert(not Odd(Count));
          SetLength(ExistingPitEdgePoints, Count);
          AFileStream.Seek(0, TSeekOrigin.soBeginning);
          AFileStream.Read(ExistingPitEdgePoints[0], Count*SizeOf(Int64));

          NewEdgePoints := TList<Int64>.Create;
          try
            NewEdgePoints.AddRange(OtherPitEdgePoints);
            NewEdgePoints.AddRange(ExistingPitEdgePoints);
            NewEdgePoints.Sort;

            CurrentCount := 0;
            for EdgePointIndex := 0 to NewEdgePoints.Count - 1 do
            begin
              AnId := NewEdgePoints[EdgePointIndex];
              APoint := IdToLocation(AnId, EndPoints);
              if IsEdgePoint(APoint) then
              begin
                if CurrentCount <> EdgePointIndex then
                begin
                  NewEdgePoints[CurrentCount] := AnId;
                end;
                Inc(CurrentCount);
              end;
            end;
            ExistingPitEdgePoints := NewEdgePoints.ToArray;
            AFileStream.Size := 0;
            AFileStream.Seek(0, TSeekOrigin.soBeginning);
            AFileStream.Write(ExistingPitEdgePoints[0], CurrentCount*SizeOf(Int64));

            FoundFirst := False;
            for EdgePointIndex := 0 to CurrentCount - 1 do
            begin
              AnId := ExistingPitEdgePoints[EdgePointIndex];
              APoint := IdToLocation(AnId, EndPoints);
              Elevation := Elevations.Z[APoint.X, APoint.Y];
              if FoundFirst then
              begin
                if Elevation < ASpillPoint.Z then
                begin
                  ASpillPoint.Location := AnId;
                  ASpillPoint.Z := Elevation;
                end;
              end
              else
              begin
                ASpillPoint.Location := AnId;
                ASpillPoint.Z := Elevation;
                ASpillPoint.PitID := RealOtherPit;
                FoundFirst := True;
              end;
            end;
            SpillPits[RealOtherPit] := ASpillPoint;
            SpillPointArray := SpillPoints.ToArray;
            Found := TArray.BinarySearch<TSpillPoint>(SpillPointArray, ASpillPoint,
              FoundIndex, Comparer, index, SpillPoints.Count - Index);
            if (FoundIndex > Index) then
            begin
              if Found then
              begin
                if (SpillPoints[FoundIndex].Location <> ASpillPoint.Location)
                  or (SpillPoints[FoundIndex].PitID <> ASpillPoint.PitID) then
                begin
                  SpillPoints.Insert(FoundIndex, ASpillPoint);
                end;
              end
              else
              begin
                SpillPoints.Insert(FoundIndex, ASpillPoint);
              end;
            end;
          finally
            NewEdgePoints.Free;
          end;

        end;
      finally
        Inc(index);
      end;
    end;
  finally
    Elevations.Free;
    EndPoints.Free;
    FlowDirections.Free;
    FileStreams.Free;
  end;
end;

procedure HandlePits(const ElevationFileName, FlowDirectionsFileName,
  EndPointFileName, PitlessElevationFileName: string;
  SpillPits: TArray<TSpillPoint>; PitList: TPitList);
const
  FlowOrdinals:  array[-1..1, -1..1] of TFlowOrdinal
    = ((foLowerLeft, foLeft,  foUpperLeft),
       (foLower,      foMiddle, foUpper),
       (foLowerRight, foRight,  foUpperRight));
  FlowValues: array[TFlowOrdinal] of TFlowDirection
    = (
       (FDeltaX : -1; FDeltaY : +1), (FDeltaX :  0; FDeltaY : +1), (FDeltaX : +1; FDeltaY : +1),
       (FDeltaX : -1; FDeltaY :  0), (FDeltaX :  0; FDeltaY :  0), (FDeltaX : +1; FDeltaY :  0),
       (FDeltaX : -1; FDeltaY : -1), (FDeltaX :  0; FDeltaY : -1), (FDeltaX : +1; FDeltaY : -1)
      );
var
  Elevations: TSurferRaster7File2;
  EndPoints: TSurferRaster7File2;
  FlowDirections: TSurferRaster7File2;
  PitIndex: Integer;
  APit: TPit;
  APoint: TPoint;
  FlowOrdinal: TFlowOrdinal;
  EndPoint: Int64;
  ASpillPoint: TSpillPoint;
  SpillPoint: TPoint;
  CarvingPath: TPointZList;
  APointZ: TPointZ;
  AFlowDirection: TFlowDirection;
  TestZ: Double;
  PointIndex: Integer;
  FillList: TPointZList;
  CarvingPathArray: TArray<TPointZ>;
  TestIndex: Integer;
  FillArray: TArray<TPointZ>;
  PriorCost: Double;
  NewCost: Double;
  PriorZ: Double;
  Comparer: IComparer<TPointZ>;
  ReverseDirectionArray: TArray<TPointZ>;
  ReverseIndex: Integer;
  AFlowOrdinal: TFlowOrdinal;
  MaxIndex: Integer;
  MinIndex: Integer;
//  XIndex: Integer;
//  YIndex: Integer;
//  StartY: Integer;
//  EndY: Integer;
//  StartX: Integer;
//  EndX: Integer;
//  Z: Double;
//  IsPit: Boolean;
//  InnerYIndex: Integer;
//  InnerXIndex: Integer;
  function CarvingCost(Z: Double): double;
  var
    index: Integer;
    APointZ: TPointZ;
    FoundFirst: Boolean;
  begin
    result := 0;
    FoundFirst := False;
    for index := 0 to CarvingPath.Count - 1 do
    begin
      APointZ := CarvingPath[index];
      if APointZ.Z > Z then
      begin
        FoundFirst := True;
        result := result + APointZ.Z - Z;
      end
      else if FoundFirst then
      begin
        break;
      end;
    end;
  end;
  function FillCost(Z: Double): double;
  var
    index: Integer;
    APointZ: TPointZ;
    FoundFirst: Boolean;
  begin
    result := 0;
    FoundFirst := False;
    for index := 0 to FillList.Count - 1 do
    begin
      APointZ := FillList[index];
      result := result + Z - APointZ.Z;
    end;
  end;
  procedure Carve(Z: Double);
  var
    index: Integer;
    APointZ: TPointZ;
    FoundFirst: Boolean;
  begin
    FoundFirst := False;
    for index := 0 to CarvingPath.Count - 1 do
    begin
      APointZ := CarvingPath[index];
      if APointZ.Z > Z then
      begin
        FoundFirst := True;
        Elevations.Z[APointZ.X, APointZ.Y] := Z;
      end
      else if FoundFirst then
      begin
        break;
      end;
    end;
  end;
  procedure Fill(Z: Double);
  var
    index: Integer;
    APointZ: TPointZ;
  begin
    for index := 0 to Length(FillArray) - 1 do
    begin
      APointZ := FillArray[index];
      Elevations.Z[APointZ.X, APointZ.Y] := Z;
    end;
  end;
  procedure FindFillPoints(Z: Double);
  var
    APointZ: TPointZ;
    PointZQueue: TQueue<TPointZ>;
    StartY: Integer;
    EndY: Integer;
    StartX: Integer;
    EndX: Integer;
    FlowOrdinal: TFlowOrdinal;
    AFlowDirection: TFlowDirection;
    NewPointZ: TPointZ;
    YIndex: Integer;
    XIndex: Integer;
  begin
    FillList.Clear;
    APointZ.X := APoint.X;
    APointZ.Y := APoint.Y;
    APointZ.Z := Elevations.Z[APointZ.X, APointZ.Y];
    PointZQueue := TQueue<TPointZ>.Create;
    try
      PointZQueue.Enqueue(APointZ);
      while PointZQueue.Count > 0 do
      begin
        APointZ := PointZQueue.Dequeue;
        FillList.Add(APointZ);
        StartY := Max(0, APoint.Y-1);
        EndY := Min(Elevations.YCount - 1, APoint.Y+1);
        StartX := Max(0, APoint.X-1);
        EndX := Min(Elevations.XCount - 1, APoint.X+1);
        for YIndex := StartY to EndY do
        begin
          for XIndex := StartX to EndX do
          begin
            if (XIndex = APointZ.X) and (YIndex = APointZ.Y) then
            begin
              Continue;
            end;
            if not Elevations.Ignore[XIndex,YIndex] then
            begin
              FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[XIndex, YIndex]));
              AFlowDirection := FlowValues[FlowOrdinal];
              NewPointZ.X := XIndex;
              NewPointZ.Y := YIndex;
              if (NewPointZ.X + AFlowDirection.DeltaX = APointZ.X)
                and (NewPointZ.Y + AFlowDirection.DeltaY = APointZ.Y) then
              begin
                NewPointZ.Z := Elevations.Z[NewPointZ.X, NewPointZ.Y];
                if NewPointZ.Z <= Z then
                begin
                  PointZQueue.Enqueue(NewPointZ);
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      PointZQueue.Free;
    end;
  end;
  procedure FillCarvingPath(APoint: TPointZ);
  var
    PointIndex: Integer;
  begin
    CarvingPath.Clear;
    CarvingPath.Add(APointZ);
    FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
    while (FlowOrdinal <> foMiddle) do
    begin
      AFlowDirection := FlowValues[FlowOrdinal];
      APointZ.X := APointZ.X + AFlowDirection.DeltaX;
      APointZ.Y := APointZ.Y + AFlowDirection.DeltaY;
      APointZ.Z := Elevations.Z[APointZ.X, APointZ.Y];
      CarvingPath.Add(APointZ);
      FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
    end;
    Assert((APointZ.X = APoint.X) and (APointZ.Y = APoint.Y));
    ReverseDirectionArray := CarvingPath.ToArray;
    CarvingPath.Reverse;
    TestZ := CarvingPath[0].Z;
    for PointIndex := 1 to CarvingPath.Count - 1 do
    begin
      APointZ := CarvingPath[PointIndex];
      if APointZ.Z <= TestZ then
      begin
        CarvingPath.Count := PointIndex-1;
        Break;
      end;
    end;

  end;
begin

  Comparer := TComparer<TPointZ>.Construct(
      function (const Left, Right: TPointZ): Integer
      begin
        result := Sign(Left.Z - Right.Z);
      end);

  TFile.Copy(ElevationFileName, PitlessElevationFileName, True);
  Elevations := TSurferRaster7File2.Create(PitlessElevationFileName);
  EndPoints := TSurferRaster7File2.Create(EndPointFileName);
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
//  PitList := TPitList.Create;
  try
//    for YIndex := 0 to Elevations.YCount - 1 do
//    begin
//      StartY := Max(0, YIndex-1);
//      EndY := Min(Elevations.YCount - 1, YIndex+1);
//      for XIndex := 0 to Elevations.XCount - 1 do
//      begin
//        StartX := Max(0, XIndex-1);
//        EndX := Min(Elevations.XCount - 1, XIndex+1);
//        if not Elevations.Ignore[XIndex,YIndex] then
//        begin
//          Z := Elevations.Z[XIndex,YIndex];
//          IsPit := True;
//          for InnerYIndex := StartY to EndY do
//          begin
//            for InnerXIndex := StartX to EndX do
//            begin
//              if not EndPoints.Ignore[InnerXIndex,InnerYIndex] then
//              begin
//                if Z > Elevations.Z[InnerXIndex,InnerYIndex] then
//                begin
//                  IsPit := False;
//                  Break;
//                end;
//              end;
//            end;
//            if not IsPit then
//            begin
//              Break;
//            end;
//          end;
//          if IsPit then
//          begin
//            APit.Location := LocationToID(Point(XIndex,YIndex),Elevations);
//            APit.Z := Z;
//            PitList.Add(APit);
//          end;
//        end;
//      end;
//    end;
    PitList.Sort(TComparer<TPit>.Construct(
      function (const Left, Right: TPit): Integer
      begin
        result := Sign(Left.Z - Right.Z);
      end));

    for PitIndex := 0 to PitList.Count - 1 do
    begin
      CarvingPath := TPointZList.Create;
      FillList := TPointZList.Create;
      try
        APit := PitList[PitIndex];
        APoint := IdToLocation(APit.Location, Elevations);
        FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APoint.X, APoint.Y]));
        ReverseDirectionArray := nil;

        if FlowOrdinal = foMiddle then
        begin
          EndPoint := Round(EndPoints.Z[APoint.X, APoint.Y]);
          ASpillPoint := SpillPits[EndPoint];
          Assert(EndPoint = ASpillPoint.PitID);
          SpillPoint := IdToLocation(ASpillPoint.Location, Elevations);
          if (SpillPoint.X <> APoint.X) or (SpillPoint.Y <> APoint.Y) then
          begin
            APointZ.X := SpillPoint.X;
            APointZ.Y := SpillPoint.Y;
            APointZ.Z := Elevations.Z[SpillPoint.X, SpillPoint.Y];
            CarvingPath.Add(APointZ);
            FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
            while (FlowOrdinal <> foMiddle) do
            begin
              AFlowDirection := FlowValues[FlowOrdinal];
              APointZ.X := APointZ.X + AFlowDirection.DeltaX;
              APointZ.Y := APointZ.Y + AFlowDirection.DeltaY;
              APointZ.Z := Elevations.Z[APointZ.X, APointZ.Y];
              CarvingPath.Add(APointZ);
              FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
            end;
            Assert((APointZ.X = APoint.X) and (APointZ.Y = APoint.Y));
            ReverseDirectionArray := CarvingPath.ToArray;
            CarvingPath.Reverse;
            TestZ := CarvingPath[0].Z;
            for PointIndex := 1 to CarvingPath.Count - 1 do
            begin
              APointZ := CarvingPath[PointIndex];
              if APointZ.Z <= TestZ then
              begin
                CarvingPath.Count := PointIndex-1;
                Break;
              end;
            end;
          end;
        end
        else
        begin
          APointZ.X := APoint.X;
          APointZ.Y := APoint.Y;
          APointZ.Z := Elevations.Z[APoint.X, APoint.Y];
          TestZ := APointZ.Z;
          CarvingPath.Add(APointZ);
          FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
          while FlowOrdinal <> foMiddle do
          begin
            AFlowDirection := FlowValues[FlowOrdinal];
            APointZ.X := APointZ.X + AFlowDirection.DeltaX;
            APointZ.Y := APointZ.Y + AFlowDirection.DeltaY;
            APointZ.Z := Elevations.Z[APointZ.X, APointZ.Y];
            if APointZ.Z < TestZ then
            begin
              Break;
            end;
            CarvingPath.Add(APointZ);
            FlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
          end;
        end;
        if CarvingPath.Count > 0 then
        begin
          CarvingPathArray := CarvingPath.ToArray;
          PriorCost := CarvingCost(CarvingPath[0].Z);
          if PriorCost > 0 then
          begin
            FillArray := nil;

//            TArray.Sort<TPointZ>(CarvingPathArray, Comparer);
            TestZ := CarvingPath[0].Z;
            for MaxIndex := 1 to CarvingPath.Count - 1 do
            begin
              if CarvingPath[MaxIndex].Z > TestZ then
              begin
                TestZ := CarvingPath[MaxIndex].Z;
              end;
            end;

            FindFillPoints(TestZ);
            TestZ := CarvingPath[0].Z;
            if FillList.Count > 0 then
            begin
              for MinIndex := 0 to FillList.Count - 1 do
              begin
                if FillList[MinIndex].Z > TestZ then
                begin
                  TestZ := CarvingPath[MinIndex].Z;
                end;
              end;
            end;

            FindFillPoints(TestZ);
            TestZ := CarvingPath[0].Z;
            if FillList.Count > 0 then
            begin
              for MinIndex := 0 to FillList.Count - 1 do
              begin
                if FillList[MinIndex].Z > TestZ then
                begin
                  TestZ := FillList[MinIndex].Z;
                end;
              end;
            end;


            FillList.AddRange(CarvingPathArray);
            CarvingPathArray := FillList.ToArray;
            TArray.Sort<TPointZ>(CarvingPathArray, Comparer);

            TestZ := CarvingPath[0].Z;
            for TestIndex := 0 to Length(CarvingPathArray) - 1 do
            begin
              TestZ := CarvingPathArray[TestIndex].Z;
              if (TestIndex > 0) and (PriorZ = TestZ) then
              begin
                Continue;
              end;
              FindFillPoints(TestZ);
              NewCost := CarvingCost(TestZ) + FillCost(TestZ);
              if NewCost <= PriorCost then
              begin
                PriorCost := NewCost;
                FillArray := FillList.ToArray;
              end
              else
              begin
                if TestIndex > 0 then
                begin
                  TestZ := CarvingPathArray[TestIndex-1].Z;
                end;
                Break;
              end;
              PriorZ := TestZ;
            end;
            Carve(TestZ);
            Fill(TestZ);
            if Length(ReverseDirectionArray) > 0 then
            begin
              for ReverseIndex := Length(ReverseDirectionArray) - 1 downto 1 do
              begin
                APointZ := ReverseDirectionArray[ReverseIndex-1];
                AFlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[APointZ.X, APointZ.Y]));
                AFlowDirection := FlowValues[AFlowOrdinal];
                AFlowDirection.DeltaX := -AFlowDirection.DeltaX;
                AFlowDirection.DeltaY := -AFlowDirection.DeltaY;
                AFlowOrdinal := FlowOrdinals[AFlowDirection.DeltaX, AFlowDirection.DeltaY];
                APointZ := ReverseDirectionArray[ReverseIndex];
                FlowDirections.Z[APointZ.X, APointZ.Y] := Ord(AFlowOrdinal);
              end;
              APointZ := ReverseDirectionArray[0];
              FlowDirections.Z[APointZ.X, APointZ.Y] := Ord(foMiddle);
            end;
          end;
        end;
      finally
        CarvingPath.Free;
        FillList.Free;
      end;
    end;
  finally
    Elevations.Free;
    EndPoints.Free;
    FlowDirections.Free;
//    PitList.Free;
  end;
end;

end.
