unit PitRemovalUnit;

interface

uses Types, RasterValuesAlongSegmentsUnit, System.Generics.Collections, FastGEO,
  PriorityQueueUnit, System.Generics.Defaults, RealListUnit, GenericRasterUnit,
  SurferGridFileReaderUnit, System.SysUtils, SubPolygonUnit;

type
  TPriorityPoint = record
    X: integer;
    Y: Integer;
    Z: Double;
  end;

  TCutCriteria = record
    LimitLength: Boolean;
    MaxLength: double;
    LimitCutDepth: Boolean;
    MaxCutDepth: double;
  end;

  TPriorityPointList = class(TList<TPriorityPoint>)
    function CarvingLength(ReferenceZ: Double): double;
  end;

  TPointQueue = class(TRbwPriorityQueue<TPriorityPoint>)
    constructor Create;
  end;

  TTestElevation = class(TObject)
    Z: double;
    Cost: double;
    PositionIndex: Integer;
    Computed: boolean;
  end;

  TTestElevationObjectList = TObjectList<TTestElevation>;

  TTestElevationQueue = class(TRbwPriorityQueue<TTestElevation>)
    constructor Create;
  end;

type
  TBoolArray2D = array of array of Boolean;

  TProgressEvent = procedure(Sender: TObject; Position, Max: integer) of object;

  TCustomPitRemover = class(TObject)
  private
    FPointQueue: TPointQueue;
    FCarvingPath: TPriorityPointList;
    FPitList: TPriorityPointList;
    FElevations: TRealList;
    FTestElevations: TTestElevationObjectList;
    FElevationQueue: TTestElevationQueue;
    FPitLocations: TBoolArray2D;
    FUsed: TBoolArray2D;
    FInPit: TBoolArray2D;
    FFlowDirections: TFlowDirections;
    FOnProgress: TProgressEvent;
    FMaxPoints: Int64;
    FChanged: Boolean;
    function GetRasterXCount: Integer; virtual; abstract;
    function GetRasterYCount: Integer; virtual; abstract;
    function GetRasterIgnore(XIndex, YIndex: integer): boolean;
      virtual; abstract;
    function GetRasterZ(XIndex, YIndex: Integer): Double; virtual; abstract;
    procedure SetRasterZ(XIndex, YIndex: Integer; const Value: Double); virtual;
      abstract;
    procedure InitializeArrays(const Pits, StartPoints: TPointList);
    procedure InitializePointQueue(const StartPoints: TPointList);
    procedure GetCarvingPath(PriorityPoint: TPriorityPoint; ReferenceZ: Double);
    procedure AddPitNeighbors(ReferenceZ: Double);
    function ElevationPosition(const PriorityPoint: TPriorityPoint): integer;
    procedure CreateNewTestElevation(PositionIndex: integer;
      const PriorityPoint: TPriorityPoint; var NewTestElev: TTestElevation);
    procedure TestNewElevations(const PriorityPoint: TPriorityPoint);
    procedure AddNeighbors(APoint: TPoint);
    procedure ComputeCost(TestElev: TTestElevation);
    procedure InitializeElevationObjects;
    function CountPits: integer;
    procedure UpdateRaster(ReferenceZ: Double);
    function GetMaxZInCarvingPath: Double;
    procedure FillNewPitList(ReferenceZ: Double; PriorityPoint: TPriorityPoint);
//    procedure AdjustFlowDirections;
  protected
    procedure InternalRemovePits(var FlowDirections: TFlowDirections;
      const Pits: TList<TPoint>; const StartPoints: TList<TPoint>;
      const CutCriteria: TCutCriteria; var Changed: boolean);
    property RasterXCount: Integer read GetRasterXCount;
    property RasterYCount: Integer read GetRasterYCount;
    property RasterIgnore[XIndex, YIndex: Integer]: Boolean read GetRasterIgnore;
    property RasterZ[XIndex, YIndex: Integer]: Double read GetRasterZ write SetRasterZ;
  public
    constructor Create;
    destructor Destroy; override;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TPitRemover = class(TCustomPitRemover)
  private
    FNewRaster: TGenericRaster;
    function GetRasterXCount: Integer; override;
    function GetRasterYCount: Integer; override;
    function GetRasterIgnore(XIndex, YIndex: Integer): Boolean; override;
    function GetRasterZ(XIndex, YIndex: Integer): Double; override;
    procedure SetRasterZ(XIndex, YIndex: Integer; const Value: Double); override;
  public
    procedure RemovePits(const ARaster: IRaster;
      const StartPoints, Pits: TPointList; out NewRaster: IRaster;
      out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
      Out Changed: boolean);
  end;

  TPitRemoverSurfGrid7 = class(TCustomPitRemover)
  private
    FNewRaster: TTempSurferRaster7File;
    function GetRasterXCount: Integer; override;
    function GetRasterYCount: Integer; override;
    function GetRasterIgnore(XIndex, YIndex: Integer): Boolean; override;
    function GetRasterZ(XIndex, YIndex: Integer): Double; override;
    procedure SetRasterZ(XIndex, YIndex: Integer; const Value: Double);
      override;
    procedure RemovePitsWithFiles(const ARaster: IRasterFile;
      const StartPoints, Pits: TPointList; out NewRaster: IRasterFile;
      out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
      Out Changed: boolean);
  public
    procedure RemovePits(const ARaster: IRaster;
      const StartPoints, Pits: TPointList; out NewRaster: IRaster;
      out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
      Out Changed: boolean);
  end;

{ @name identifies points in ARaster that are lower than all surrounding points
  in ARaster and are are on the active part of ARaster. Such points are added
  to either EdgeStartPoints or Pits depending on whether they are next to an
  edge or not. In addition if StartPointsOutline is defined and an identified
  point is inside it, the point will be added to EdgeStartPoints rather than
  Pits.

  Finally, if a point is a pit and it is next to but not on the edge of the
  active part of ARaster, The neighboring edge point with the lowest elevation
  will be added to EdgeStartPoints.
}
procedure IdentifyStartPointsAndPits(ARaster: IRaster;
  out EdgeStartPoints, Pits: TPointList; StartPointsOutline: TOutline;
  OnProgress: TProgressEvent);

implementation

uses
  System.Math;

procedure IdentifyStartPointsAndPits(ARaster: IRaster;
  out EdgeStartPoints, Pits: TPointList; StartPointsOutline: TOutline;
  OnProgress: TProgressEvent);
var
  XIndex: Integer;
  YIndex: Integer;
  List: TPointList;
  Z: Double;
  InnerXStart: integer;
  InnerXEnd: Integer;
  InnerYStart: integer;
  InnerYEnd: Integer;
  APoint: TPoint;
  IsPit: Boolean;
  InnerXIndex: Integer;
  InnerYIndex: Integer;
  Point2D: TPoint2D;
  MaxValue: Int64;
  UpdateDiv: Int64;
  CurrentCount: Int64;
  EdgePoints: array of array of boolean;
  IgnorePoints: array of array of boolean;
  FoundNeighbor: Boolean;
  NeighborPoint: TPoint;
  NeighborPoints: array of array of boolean;
  TestZ: Double;
  NewZ: Double;
begin
  SetLength(EdgePoints, ARaster.XCount, ARaster.YCount);
  SetLength(IgnorePoints, ARaster.XCount, ARaster.YCount);
  SetLength(NeighborPoints, ARaster.XCount, ARaster.YCount);
  MaxValue := ARaster.YCount * ARaster.XCount;
  UpdateDiv := MaxValue div 1000;
  if UpdateDiv = 0 then
  begin
    UpdateDiv := 1;
  end;
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  CurrentCount := 0;


  for YIndex := 0 to ARaster.YCount - 1 do
  begin
    for XIndex := 0 to ARaster.XCount - 1 do
    begin
      EdgePoints[XIndex, YIndex] := False;
      NeighborPoints[XIndex, YIndex] := False;
      IgnorePoints[XIndex, YIndex] := ARaster.Ignore[XIndex, YIndex];
      if Assigned(OnProgress) then
      begin
        Inc(CurrentCount);
        if (CurrentCount mod UpdateDiv) = 0 then
        begin
          OnProgress(nil, CurrentCount div UpdateDiv, 1000);
        end;
      end;
    end;
  end;

  CurrentCount := 0;
  for YIndex := 0 to ARaster.YCount - 1 do
  begin
    EdgePoints[0, YIndex] := True;
    EdgePoints[ARaster.XCount - 1, YIndex] := True;
  end;
  for XIndex := 0 to ARaster.XCount - 1 do
  begin
    EdgePoints[XIndex, 0] := True;
    EdgePoints[XIndex, ARaster.YCount - 1] := True;
  end;

  for YIndex := 0 to ARaster.YCount - 1 do
  begin
    for XIndex := 0 to ARaster.XCount - 1 do
    begin
      if EdgePoints[XIndex, YIndex] then
      begin
        Continue;
      end;
      InnerXStart := Max(0,XIndex-1);
      InnerXEnd := Min(ARaster.XCount - 1, XIndex+1);
      InnerYStart := Max(0,YIndex-1);
      InnerYEnd := Min(ARaster.YCount - 1, YIndex+1);
      for InnerYIndex := InnerYStart to InnerYEnd do
      begin
        if EdgePoints[XIndex, YIndex] then
        begin
          Continue;
        end;
        for InnerXIndex := InnerXStart to InnerXEnd do
        begin
          if IgnorePoints[InnerXIndex, InnerYIndex]  then
          begin
            EdgePoints[XIndex, YIndex] := True;
            Break;
          end;
        end;
      end;
    end;
  end;
  for YIndex := 0 to ARaster.YCount - 1 do
  begin
    for XIndex := 0 to ARaster.XCount - 1 do
    begin
      if Assigned(OnProgress) then
      begin
        Inc(CurrentCount);
        if (CurrentCount mod UpdateDiv) = 0 then
        begin
          OnProgress(nil, CurrentCount div UpdateDiv, 1000);
        end;
      end;
      if IgnorePoints[XIndex, YIndex] then
      begin
        Continue;
      end;

      List := Pits;
      if (XIndex = 0) or (XIndex = ARaster.XCount - 1) then
      begin
        List := EdgeStartPoints;
      end
      else if (YIndex = 0) or (YIndex = ARaster.YCount - 1) then
      begin
        List := EdgeStartPoints;
      end;
      Z := ARaster.Z[XIndex, YIndex];

      InnerXStart := Max(0,XIndex-1);
      InnerXEnd := Min(ARaster.XCount - 1, XIndex+1);
      InnerYStart := Max(0,YIndex-1);
      InnerYEnd := Min(ARaster.YCount - 1, YIndex+1);
      IsPit := True;
      for InnerYIndex := InnerYStart to InnerYEnd do
      begin
        if not IsPit then
        begin
          Break;
        end;
        for InnerXIndex := InnerXStart to InnerXEnd do
        begin
          if (InnerXIndex = XIndex) and (InnerYIndex = YIndex) then
          begin
            Continue;
          end;
          if IgnorePoints[InnerXIndex, InnerYIndex] then
          begin
            List := EdgeStartPoints;
            Continue;
          end;
          if ARaster.Z[InnerXIndex, InnerYIndex] < Z then
          begin
            if EdgePoints[XIndex, YIndex] then
            begin
              if EdgePoints[InnerXIndex, InnerYIndex] then
              begin
                IsPit := False;
                break;
              end;
            end
            else
            begin
              IsPit := False;
              break;
            end;
          end;
        end;
      end;
      if IsPit then
      begin
        APoint.X := XIndex;
        APoint.Y := YIndex;
        if (StartPointsOutline <> nil) and (List = Pits) then
        begin
          Point2D := ARaster.LowerLeft;
          Point2D.x := Point2D.x + ARaster.XSpacing * (APoint.X + 0.5);
          Point2D.y := Point2D.y + ARaster.YSpacing * (APoint.Y + 0.5);
          if not StartPointsOutline.PointInside(Point2D) then
          begin
            List := EdgeStartPoints;
          end;
        end;
        List.Add(APoint);

        // If the pit is next to an edge point,
        // make the edge point a starting point.
        if List = Pits then
        begin
          FoundNeighbor := False;
          TestZ := 0;
          for InnerYIndex := InnerYStart to InnerYEnd do
          begin
            for InnerXIndex := InnerXStart to InnerXEnd do
            begin
              if (InnerXIndex = XIndex) and (InnerYIndex = YIndex) then
              begin
                Continue;
              end;
              if IgnorePoints[InnerXIndex, InnerYIndex] then
              begin
                Continue;
              end;
              if EdgePoints[InnerXIndex, InnerYIndex]
                and not IgnorePoints[InnerXIndex, InnerYIndex] then
              begin
                if FoundNeighbor then
                begin
                  NewZ := ARaster.Z[InnerXIndex, InnerYIndex];
                  if NewZ < TestZ then
                  begin
                    NeighborPoint.X := InnerXIndex;
                    NeighborPoint.Y := InnerYIndex;
                    TestZ := NewZ
                  end;
                end
                else
                begin
                  NeighborPoint.X := InnerXIndex;
                  NeighborPoint.Y := InnerYIndex;
                  FoundNeighbor := True;
                  TestZ := ARaster.Z[InnerXIndex, InnerYIndex];
                end;
              end;
            end;
          end;
          if FoundNeighbor then
          begin
            // don't add the same point twice.
            if not NeighborPoints[NeighborPoint.X, NeighborPoint.Y] then
            begin
              EdgeStartPoints.Add(NeighborPoint);
              NeighborPoints[NeighborPoint.X, NeighborPoint.Y] := True;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TGenericRaster }
{ TPointQueue }
{ TTestElevationQueue }

{ TPitRemover }


procedure TPitRemover.RemovePits(const ARaster: IRaster; const StartPoints,
  Pits: TPointList; out NewRaster: IRaster;
  out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
  Out Changed: boolean);
begin
  FNewRaster := TGenericRaster.Create(ARaster);
  NewRaster := FNewRaster;
  InternalRemovePits(FlowDirections, Pits, StartPoints, CutCriteria, Changed);

end;

procedure TPitRemover.SetRasterZ(XIndex, YIndex: Integer; const Value: Double);
begin
  FNewRaster.z[XIndex, YIndex] := Value;
end;

function TPitRemover.GetRasterIgnore(XIndex, YIndex: Integer): Boolean;
begin
  Result := FNewRaster.Ignore[XIndex, YIndex];
end;

function TPitRemover.GetRasterXCount: Integer;
begin
  result := FNewRaster.XCount;
end;

function TPitRemover.GetRasterYCount: Integer;
begin
  result := FNewRaster.YCount;
end;

function TPitRemover.GetRasterZ(XIndex, YIndex: Integer): Double;
begin
  result := FNewRaster.z[XIndex, YIndex];
end;

{ TPitRemoverSurfGrid7 }


constructor TCustomPitRemover.Create;
begin
  FPointQueue := TPointQueue.Create;
  FCarvingPath := TPriorityPointList.Create;
  FPitList := TPriorityPointList.Create;
  FElevations := TRealList.Create;
  FTestElevations := TTestElevationObjectList.Create;
  FElevationQueue := TTestElevationQueue.Create;
end;

destructor TCustomPitRemover.Destroy;
begin
  FElevationQueue.Free;
  FTestElevations.Free;
  FPitList.Free;
  FPointQueue.Free;
  FCarvingPath.Free;
  FElevations.Free;
  inherited;
end;

//procedure TCustomPitRemover.AdjustFlowDirections;
//var
//  XIndex: Integer;
//  MinX: Integer;
//  DownstreamZ: Double;
//  InnerYIndex: Integer;
//  YIndex: Integer;
//  MaxX: Integer;
//  MinY: Integer;
//  InnerXIndex: Integer;
//  TestZ: Double;
//  AFlowDir: TFlowDirection;
//  MaxY: Integer;
//  UpstreamZ: Double;
//begin
//  for YIndex := 0 to RasterYCount - 1 do
//  begin
//    for XIndex := 0 to RasterXCount - 1 do
//    begin
//      if not RasterIgnore[XIndex, YIndex] then
//      begin
//        AFlowDir := FFlowDirections[XIndex, YIndex];
//        if (AFlowDir.DeltaX <> 0) or (AFlowDir.DeltaY <> 0) then
//        begin
//          UpstreamZ := RasterZ[XIndex, YIndex];
//          InnerXIndex := XIndex + AFlowDir.DeltaX;
//          InnerYIndex := YIndex + AFlowDir.DeltaY;
//          Assert(not RasterIgnore[InnerXIndex, InnerYIndex]);
//          DownstreamZ := RasterZ[InnerXIndex, InnerYIndex];
//          MinX := Max(0, XIndex - 1);
//          MaxX := Min(RasterXCount - 1, XIndex + 1);
//          MinY := Max(0, YIndex - 1);
//          MaxY := Min(RasterYCount - 1, YIndex + 1);
//          for InnerYIndex := MinY to MaxY do
//          begin
//            for InnerXIndex := MinX to MaxX do
//            begin
//              if (InnerXIndex = XIndex) and (InnerYIndex = YIndex) then
//              begin
//                Continue;
//              end;
//              if RasterIgnore[InnerXIndex, InnerYIndex] then
//              begin
//                Continue;
//              end;
//              TestZ := RasterZ[InnerXIndex, InnerYIndex];
//              if TestZ < DownstreamZ then
//              begin
//                AFlowDir.DeltaX := InnerXIndex - XIndex;
//                AFlowDir.DeltaY := InnerYIndex - YIndex;
//                FFlowDirections[XIndex, XIndex] := AFlowDir;
//                DownstreamZ := TestZ;
//              end;
//            end;
//          end;
//        end;
//      end;
//    end;
//  end;
//end;

function TPitRemoverSurfGrid7.GetRasterIgnore(XIndex, YIndex: Integer): Boolean;
begin
  Result := FNewRaster.Ignore[XIndex, YIndex];
end;

function TPitRemoverSurfGrid7.GetRasterXCount: Integer;
begin
  result := FNewRaster.XCount;
end;

function TPitRemoverSurfGrid7.GetRasterYCount: Integer;
begin
  result := FNewRaster.YCount;
end;
  function TPitRemoverSurfGrid7.GetRasterZ(XIndex, YIndex: Integer): Double;
begin
  result := FNewRaster.z[XIndex, YIndex];
end;

procedure TPitRemoverSurfGrid7.RemovePitsWithFiles(const ARaster: IRasterFile;
  const StartPoints, Pits: TPointList; out NewRaster: IRasterFile;
  out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
  Out Changed: boolean);
begin
  FNewRaster := TTempSurferRaster7File.Create(ARaster);

  NewRaster := FNewRaster;
  InternalRemovePits(FlowDirections, Pits, StartPoints, CutCriteria, Changed);
end;

procedure TPitRemoverSurfGrid7.RemovePits(const ARaster: IRaster;
  const StartPoints, Pits: TPointList; out NewRaster: IRaster;
  out FlowDirections: TFlowDirections; const CutCriteria: TCutCriteria;
  out Changed: boolean);
var
  NewRaster2: IRasterFile;
begin
  RemovePitsWithFiles(ARaster as IRasterFile, StartPoints, Pits,
    NewRaster2, FlowDirections, CutCriteria, Changed);
  NewRaster := NewRaster2 as IRasterFile
end;

procedure TPitRemoverSurfGrid7.SetRasterZ(XIndex, YIndex: Integer;
  const Value: Double);
begin
  FNewRaster.z[XIndex, YIndex] := Value;
end;

{ TPointQueue }

constructor TPointQueue.Create;
begin
  inherited Create(TComparer<TPriorityPoint>.Construct(
    function (const Left, Right: TPriorityPoint): Integer
    begin
      result := Sign(Right.Z - Left.Z);
    end));

end;

{ TTestElevationQueue }

constructor TTestElevationQueue.Create;
begin
  inherited Create(TComparer<TTestElevation>.Construct(
    function (const Left, Right: TTestElevation): Integer
    begin
      result := Sign(Right.Z - Left.Z);
    end));
end;

procedure TCustomPitRemover.InitializeArrays(const Pits,
    StartPoints: TPointList);
var
  XIndex: integer;
  APoint: TPoint;
  AFlowDirection: TFlowDirection;
  PointIndex: integer;
  YIndex: integer;
  ProgressDiv: integer;
  ProgressCount: integer;
begin
  FFlowDirections := TFlowDirections.Create(RasterXCount, RasterYCount);
  SetLength(FPitLocations, RasterXCount, RasterYCount);
  SetLength(FUsed, RasterXCount, RasterYCount);
  SetLength(FInPit, RasterXCount, RasterYCount);
  ProgressDiv := (RasterYCount * RasterXCount) div 1000;
  ProgressCount := 0;
  AFlowDirection.DeltaX := 0;
  AFlowDirection.DeltaY := 0;
  FMaxPoints := 0;
  for YIndex := 0 to RasterYCount - 1 do
  begin
    for XIndex := 0 to RasterXCount - 1 do
    begin
      FFlowDirections[XIndex, YIndex] := AFlowDirection;
      FUsed[XIndex, YIndex] := RasterIgnore[XIndex, YIndex];
      FInPit[XIndex, YIndex] := FUsed[XIndex, YIndex];
      FPitLocations[XIndex, YIndex] := False;
      if not FUsed[XIndex, YIndex] then
      begin
        Inc(FMaxPoints);
      end;
      if Assigned(OnProgress) then
      begin
        Inc(ProgressCount);
        if (ProgressCount mod ProgressDiv) = 0 then
        begin OnProgress(self,
          ProgressCount div ProgressDiv, 1000);
        end;
      end;
    end;
  end;
  for PointIndex := 0 to StartPoints.Count - 1 do
  begin
    APoint := StartPoints[PointIndex];
    FUsed[APoint.X, APoint.Y] := True;
  end;
  for PointIndex := 0 to Pits.Count - 1 do
  begin
    APoint := Pits[PointIndex];
    FPitLocations[APoint.X, APoint.Y] := True;
  end;
end;

procedure TCustomPitRemover.InitializePointQueue(const StartPoints: TPointList);
var
  APoint: TPoint;
  PointIndex: integer;
  PriorityPoint: TPriorityPoint;
begin
  for PointIndex := 0 to StartPoints.Count - 1 do
  begin
    APoint := StartPoints[PointIndex];
    PriorityPoint.X := APoint.X;
    PriorityPoint.Y := APoint.Y;
    PriorityPoint.Z := RasterZ[APoint.X, APoint.Y];
    FUsed[APoint.X, APoint.Y] := True;
    FPointQueue.Enqueue(PriorityPoint);
  end;
end;

procedure TCustomPitRemover.GetCarvingPath(PriorityPoint: TPriorityPoint;
ReferenceZ: Double);
var
  DownStreamPoint: TPriorityPoint;
  AFlowDirection: TFlowDirection;
begin
  FCarvingPath.Clear;
  DownStreamPoint := PriorityPoint;
  repeat
    AFlowDirection := FFlowDirections[DownStreamPoint.X, DownStreamPoint.Y];
    if (AFlowDirection.DeltaX = 0) and (AFlowDirection.DeltaY = 0) then
    begin
      Break;
    end;
    DownStreamPoint.X := DownStreamPoint.X + AFlowDirection.DeltaX;
    DownStreamPoint.Y := DownStreamPoint.Y + AFlowDirection.DeltaY;
    DownStreamPoint.Z := RasterZ[DownStreamPoint.X, DownStreamPoint.Y];
    FCarvingPath.Add(DownStreamPoint);
    if DownStreamPoint.Z <= ReferenceZ then
    begin
      Break;
    end;
  until False;
end;

procedure TCustomPitRemover.AddPitNeighbors(ReferenceZ: Double);
var
  InnerXStart: integer;
  InnerXEnd: integer;
  InnerYStart: integer;
  InnerYEnd: integer;
  XIndex: integer;
  YIndex: integer;
  PPoint: TPriorityPoint;
  APoint: TPriorityPoint;
  PitIndex: integer;
begin
  PitIndex := 0;
  while PitIndex < FPitList.Count do
  begin
    APoint := FPitList[PitIndex];
    InnerXStart := Max(0, APoint.X - 1);
    InnerXEnd := Min(RasterXCount - 1, APoint.X + 1);
    InnerYStart := Max(0, APoint.Y - 1);
    InnerYEnd := Min(RasterYCount - 1, APoint.Y + 1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if FInPit[XIndex, YIndex] then
        begin
          Continue;
        end;
        PPoint.Z := RasterZ[XIndex, YIndex];
        if PPoint.Z < ReferenceZ then
        begin
          PPoint.X := XIndex;
          PPoint.Y := YIndex;
          FPitList.Add(PPoint);
          FInPit[XIndex, YIndex] := True;
        end;
      end;
    end;
    Inc(PitIndex);
  end;
end;

function TCustomPitRemover.ElevationPosition(const PriorityPoint
  : TPriorityPoint): integer;
begin
  result := FElevations.IndexOf(PriorityPoint.Z);
end;

procedure TCustomPitRemover.CreateNewTestElevation(PositionIndex: integer; const PriorityPoint: TPriorityPoint; var NewTestElev: TTestElevation);
begin
  NewTestElev := TTestElevation.Create;
  NewTestElev.Z := PriorityPoint.Z;
  NewTestElev.Cost := 0;
  NewTestElev.PositionIndex := PositionIndex;
  NewTestElev.Computed := False;
  FTestElevations.Add(NewTestElev);
  FElevationQueue.Enqueue(NewTestElev);
end;

procedure TCustomPitRemover.TestNewElevations(const PriorityPoint
  : TPriorityPoint);
var
  PositionIndex: integer;
  NewTestElev: TTestElevation;
begin
  PositionIndex := ElevationPosition(PriorityPoint);
  if PositionIndex < 0 then
  begin
    FElevations.Add(PriorityPoint.Z);
    CreateNewTestElevation(PositionIndex, PriorityPoint, NewTestElev);
  end
end;

procedure TCustomPitRemover.AddNeighbors(APoint: TPoint);
var
  InnerXStart: integer;
  InnerXEnd: integer;
  InnerYStart: integer;
  InnerYEnd: integer;
  XIndex: integer;
  YIndex: integer;
  AFlowDirection: TFlowDirection;
  PriorityPoint: TPriorityPoint;
//  TestZ: Double;
//  FoundPoint: Boolean;
//  DeltaZDiagonal: Double;
//  DeltaZNonDialognal: Double;
//  TestPoint: TPoint;
//  PriorZ: Double;
begin
  InnerXStart := Max(0, APoint.X - 1);
  InnerXEnd := Min(RasterXCount - 1, APoint.X + 1);
  InnerYStart := Max(0, APoint.Y - 1);
  InnerYEnd := Min(RasterYCount - 1, APoint.Y + 1);

  {
  for XIndex := InnerXStart to InnerXEnd do
  begin
    if FUsed[XIndex, APoint.Y] then
    begin
      Continue;
    end;
    FUsed[XIndex, APoint.Y] := True;
    AFlowDirection.DeltaX := APoint.X - XIndex;
    AFlowDirection.DeltaY := 0;

    FFlowDirections[XIndex, APoint.Y] := AFlowDirection;

    PriorityPoint.X := XIndex;
    PriorityPoint.Y := APoint.Y;
    PriorityPoint.Z := RasterZ[XIndex, APoint.Y];
    FPointQueue.Enqueue(PriorityPoint);
  end;

  for YIndex := InnerYStart to InnerYEnd do
  begin
    if FUsed[APoint.X, YIndex] then
    begin
      Continue;
    end;
    FUsed[APoint.X, YIndex] := True;
    AFlowDirection.DeltaX := 0;
    AFlowDirection.DeltaY := APoint.Y - YIndex;

    FFlowDirections[APoint.X, YIndex] := AFlowDirection;

    PriorityPoint.X := APoint.X;
    PriorityPoint.Y := YIndex;
    PriorityPoint.Z := RasterZ[APoint.X, YIndex];
    FPointQueue.Enqueue(PriorityPoint);
  end;
  }


  for YIndex := InnerYStart to InnerYEnd do
  begin
    for XIndex := InnerXStart to InnerXEnd do
    begin
      if FUsed[XIndex, YIndex] then
      begin
        Continue;
      end;
      FUsed[XIndex, YIndex] := True;
      AFlowDirection.DeltaX := APoint.X - XIndex;
      AFlowDirection.DeltaY := APoint.Y - YIndex;

      // Check if there is another point
      // to which it would be better to connect.
      {
      if (AFlowDirection.DeltaX <> 0) and (AFlowDirection.DeltaY <> 0) then
      begin
        PriorZ := RasterZ[APoint.X, APoint.Y];
        TestZ := RasterZ[XIndex, YIndex];
        if TestZ > PriorZ then        
        begin        
          FoundPoint := False;
          if (InnerXStart < XIndex) and FUsed[XIndex-1, YIndex]
            and (RasterZ[XIndex-1, YIndex] < TestZ)
            and (RasterZ[XIndex-1, YIndex] < PriorZ)
            then
          begin
            TestZ := RasterZ[XIndex-1, YIndex];
            TestPoint.X := XIndex-1;
            TestPoint.Y := YIndex;
            FoundPoint := True;
          end;
          if (InnerXEnd > XIndex) and FUsed[XIndex+1, YIndex]
            and (RasterZ[XIndex+1, YIndex] < TestZ) 
            and (RasterZ[XIndex+1, YIndex] < PriorZ)
            then
          begin
            TestZ := RasterZ[XIndex+1, YIndex];
            TestPoint.X := XIndex+1;
            TestPoint.Y := YIndex;
            FoundPoint := True;
          end;
          if (InnerYStart < YIndex) and FUsed[XIndex, YIndex-1]
            and (RasterZ[XIndex, YIndex-1] < TestZ) 
            and (RasterZ[XIndex, YIndex-1] < PriorZ)
            then
          begin
            TestZ := RasterZ[XIndex, YIndex-1];
            TestPoint.X := XIndex;
            TestPoint.Y := YIndex-1;
            FoundPoint := True;
          end;
          if (InnerYEnd > YIndex) and FUsed[XIndex, YIndex+1]
            and (RasterZ[XIndex, YIndex+1] < TestZ)
            and (RasterZ[XIndex, YIndex+1] < PriorZ)
            then
          begin
            TestZ := RasterZ[XIndex, YIndex+1];
            TestPoint.X := XIndex;
            TestPoint.Y := YIndex+1;
            FoundPoint := True;
          end;
          if FoundPoint then
          begin
            Assert(RasterZ[XIndex, YIndex] > TestZ);
  //          Assert(TestZ >= PriorZ);
            DeltaZDiagonal := RasterZ[XIndex, YIndex]
              - RasterZ[APoint.X, APoint.Y];
  //          Assert(DeltaZDiagonal >= 0);
            DeltaZNonDialognal := RasterZ[XIndex, YIndex]
              - RasterZ[TestPoint.X, TestPoint.Y];
  //          Assert(DeltaZNonDialognal >= 0);
            if (DeltaZDiagonal > 0)
              and (DeltaZNonDialognal > 0)
              and (DeltaZNonDialognal*Sqrt2 > DeltaZDiagonal) then
            begin
  //            AFlowDirection.DeltaX := TestPoint.X - XIndex;
  //            AFlowDirection.DeltaY := TestPoint.Y - YIndex;
              AFlowDirection.DeltaX := APoint.X - TestPoint.X;
              AFlowDirection.DeltaY := APoint.Y - TestPoint.Y;
            end;
          end;
        end;          
      end;
      }

      FFlowDirections[XIndex, YIndex] := AFlowDirection;

      PriorityPoint.X := XIndex;
      PriorityPoint.Y := YIndex;
      PriorityPoint.Z := RasterZ[XIndex, YIndex];
      FPointQueue.Enqueue(PriorityPoint);
    end;
  end

end;

procedure TCustomPitRemover.ComputeCost(TestElev: TTestElevation);
var
  Cost: Double;
  Item: TPriorityPoint;
  AboveBarrier: boolean;
  Index: integer;
  NextCarvingPathPoint: TPriorityPoint;
  CarvingNeighborsToFill: TPriorityPointList;
  StartFilling: boolean;
  PointIndex: integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: integer;
    InnerYStart: integer;
    InnerYEnd: integer;
    YIndex: integer;
    XIndex: integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0, APriorityPoint.X - 1);
    InnerXEnd := Min(RasterXCount - 1, APriorityPoint.X + 1);
    InnerYStart := Max(0, APriorityPoint.Y - 1);
    InnerYEnd := Min(RasterYCount - 1, APriorityPoint.Y + 1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and
          (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;
        FlowDir := FFlowDirections[XIndex, YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X) and
          (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := RasterZ[XIndex, YIndex];
          if NeighborPoint.Z < TestElev.Z then
          begin
            CarvingNeighborsToFill.Add(NeighborPoint);
            FillCarvingPathNeighbors(NeighborPoint);
          end;
        end;
      end;
    end;
  end;

begin
  CarvingNeighborsToFill := TPriorityPointList.Create;
  try
    Cost := 0;
    AboveBarrier := False;
    StartFilling := False;
    for Index := FCarvingPath.Count - 1 downto 0 do
    begin
      Item := FCarvingPath[Index];
      if Item.Z > TestElev.Z then
      begin
        Cost := Cost + Item.Z - TestElev.Z;
        StartFilling := True;
      end;
      if Item.Z >= TestElev.Z then
      begin
        AboveBarrier := True;
      end
      else if not AboveBarrier then
      begin
        Cost := Cost + TestElev.Z - Item.Z;
      end;
      if Index > 0 then
      begin
        NextCarvingPathPoint := FCarvingPath[Index - 1];
      end
      else
      begin
        NextCarvingPathPoint.X := -1;
        NextCarvingPathPoint.Y := -1;
      end;
      if StartFilling then
      begin
        FillCarvingPathNeighbors(Item);
      end;
    end;
    for Item in FPitList do
    begin
      if Item.Z < TestElev.Z then
      begin
        Cost := Cost + TestElev.Z - Item.Z;
      end;
    end;
    for PointIndex := 0 to CarvingNeighborsToFill.Count - 1 do
    begin
      APoint := CarvingNeighborsToFill[PointIndex];
      Cost := Cost + TestElev.Z - APoint.Z;
    end;
    TestElev.Cost := Cost;
    TestElev.Computed := True;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;

procedure TCustomPitRemover.InitializeElevationObjects;
begin
  FElevations.Clear;
  FElevations.Sorted := True;
  FElevationQueue.Clear;
  FTestElevations.Clear;
end;

function TCustomPitRemover.CountPits: integer;
var
  PitIndex: integer;
  APit: TPriorityPoint;
begin
  result := 0;
  for PitIndex := 0 to FPitList.Count - 1 do
  begin
    APit := FPitList[PitIndex];
    if FPitLocations[APit.X, APit.Y] then
    begin
      Inc(result);
    end;
  end;
end;

procedure TCustomPitRemover.UpdateRaster(ReferenceZ: Double);
var
  APitPoint: TPriorityPoint;
  ACarvingPathPoint: TPriorityPoint;
  PathIndex: integer;
  PitIndex: integer;
  StartFilling: boolean;
  CarvingNeighborsToFill: TPriorityPointList;
  NextCarvingPathPoint: TPriorityPoint;
  PointIndex: integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: integer;
    InnerYStart: integer;
    InnerYEnd: integer;
    YIndex: integer;
    XIndex: integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0, APriorityPoint.X - 1);
    InnerXEnd := Min(RasterXCount - 1, APriorityPoint.X + 1);
    InnerYStart := Max(0, APriorityPoint.Y - 1);
    InnerYEnd := Min(RasterYCount - 1, APriorityPoint.Y + 1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and
          (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;
        FlowDir := FFlowDirections[XIndex, YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X) and
          (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := RasterZ[XIndex, YIndex];
          if NeighborPoint.Z < ReferenceZ then
          begin
            CarvingNeighborsToFill.Add(NeighborPoint);
            FillCarvingPathNeighbors(NeighborPoint);
          end;
        end;
      end;
    end;
  end;

begin
  CarvingNeighborsToFill := TPriorityPointList.Create;
  try
    StartFilling := False;
    for PathIndex := FCarvingPath.Count - 1 downto 0 do
    begin
      ACarvingPathPoint := FCarvingPath[PathIndex];
      if ACarvingPathPoint.Z >= ReferenceZ then
      begin
        StartFilling := True;
      end;
      if StartFilling then
      begin
        if ACarvingPathPoint.Z <> ReferenceZ then
        begin
          RasterZ[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
          FChanged := True;
        end;
        if PathIndex > 0 then
        begin
          NextCarvingPathPoint := FCarvingPath[PathIndex - 1];
        end
        else
        begin
          NextCarvingPathPoint.X := -1;
          NextCarvingPathPoint.Y := -1;
        end;
        FillCarvingPathNeighbors(ACarvingPathPoint);
      end
      else
      begin
        if ACarvingPathPoint.Z > ReferenceZ then
        begin
          RasterZ[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
          FChanged := True;
        end;
      end;
    end;
    for PointIndex := 0 to CarvingNeighborsToFill.Count - 1 do
    begin
      APoint := CarvingNeighborsToFill[PointIndex];
      // Assert(APoint.Z < ReferenceZ);
      // Assert(FNewRaster.Z[APoint.X, APoint.Y] = APoint.Z);
      if RasterZ[APoint.X, APoint.Y] < ReferenceZ then
      begin
        RasterZ[APoint.X, APoint.Y] := ReferenceZ;
        FChanged := True;
      end;
    end;
    for PitIndex := 0 to FPitList.Count - 1 do
    begin
      APitPoint := FPitList[PitIndex];
      if APitPoint.Z < ReferenceZ then
      begin
        RasterZ[APitPoint.X, APitPoint.Y] := ReferenceZ;
        FChanged := True;
      end;
      FInPit[APitPoint.X, APitPoint.Y] := False;
      FPitLocations[APitPoint.X, APitPoint.Y] := False;
    end;
    FPitList.Clear;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;

function TCustomPitRemover.GetMaxZInCarvingPath: Double;
var
  DownStreamPoint: TPriorityPoint;
begin
  result := FCarvingPath[0].Z;
  for DownStreamPoint in FCarvingPath do
  begin
    if DownStreamPoint.Z > result then
    begin
      result := DownStreamPoint.Z;
    end;
  end;
end;

procedure TCustomPitRemover.FillNewPitList(ReferenceZ: Double; PriorityPoint: TPriorityPoint);
var
  PitIndex: integer;
  APit: TPriorityPoint;
begin
  for PitIndex := 0 to FPitList.Count - 1 do
  begin
    APit := FPitList[PitIndex];
    FInPit[APit.X, APit.Y] := False;
  end;
  for PitIndex := 0 to FCarvingPath.Count - 1 do
  begin
    APit := FCarvingPath[PitIndex];
    FInPit[APit.X, APit.Y] := True;
  end;
  FPitList.Clear;
  FPitList.Add(PriorityPoint);
  FInPit[PriorityPoint.X, PriorityPoint.Y] := True;
  AddPitNeighbors(ReferenceZ);
  for PitIndex := 0 to FCarvingPath.Count - 1 do
  begin
    APit := FCarvingPath[PitIndex];
    FInPit[APit.X, APit.Y] := False;
  end;
end;

procedure TCustomPitRemover.InternalRemovePits(
  var FlowDirections: TFlowDirections; const Pits: TList<TPoint>;
  const StartPoints: TList<TPoint>; const CutCriteria: TCutCriteria;
  var Changed: boolean);
var
  DownStreamPoint: TPriorityPoint;
  APit: TPriorityPoint;
  ProgressDiv: integer;
  BestElev: TTestElevation;
  APoint: TPoint;
  PriorPitCount: integer;
  ProgressCount: integer;
  TestElev: TTestElevation;
  MaxZ: Double;
  PriorityPoint: TPriorityPoint;
  Overflowed: boolean;
  ReferenceZ: Double;
  TotalCost: Double;
  PitIndex: integer;
  PitCount: integer;
begin
  FChanged := False;
  if Assigned(OnProgress) then
  begin
    OnProgress(self, 0, 1000);
  end;
  InitializeArrays(Pits, StartPoints);
  FPointQueue.Clear;
  FCarvingPath.Clear;
  FPitList.Clear;
  FElevations.Clear;
  FTestElevations.Clear;
  FElevationQueue.Clear;
  // StoreElevations;
  InitializePointQueue(StartPoints);
  ProgressDiv := FMaxPoints div 1000;
  ProgressCount := 0;
  while FPointQueue.Count > 0 do
  begin
    Inc(ProgressCount);
    if Assigned(OnProgress) then
    begin
      if (ProgressCount mod ProgressDiv) = 0 then
      begin
        OnProgress(self, ProgressCount div ProgressDiv, 1000);
      end;
    end;
    PriorityPoint := FPointQueue.Dequeue;
    if FPitLocations[PriorityPoint.X, PriorityPoint.Y] then
    begin
      // Handle Pit
      InitializeElevationObjects;
      TestNewElevations(PriorityPoint);
      ReferenceZ := PriorityPoint.Z;
      try
        GetCarvingPath(PriorityPoint, ReferenceZ);
      except
        // Beep;
        raise;
      end;
      if FCarvingPath.Count > 0 then
      begin
        for DownStreamPoint in FCarvingPath do
        begin
          if DownStreamPoint.Z > PriorityPoint.Z then
          begin
            TestNewElevations(DownStreamPoint);
          end;
        end;
        MaxZ := GetMaxZInCarvingPath;
        if MaxZ > ReferenceZ then
        begin
          ReferenceZ := MaxZ;
          FillNewPitList(ReferenceZ, PriorityPoint);
          for PitIndex := 1 to FPitList.Count - 1 do
          begin
            APit := FPitList[PitIndex];
            // TestElev := nil;
            if APit.Z > PriorityPoint.Z then
            begin
              TestNewElevations(APit);
            end;
          end;
          ReferenceZ := PriorityPoint.Z;
          TotalCost := MaxInt;

          FillNewPitList(ReferenceZ, PriorityPoint);
          PriorPitCount := CountPits;
          BestElev := nil;
          while FElevationQueue.Count > 0 do
          begin
            TestElev := FElevationQueue.Dequeue;
            if CutCriteria.LimitCutDepth then
            begin
              if (MaxZ - TestElev.Z) > CutCriteria.MaxCutDepth then
              begin
                Continue;
              end;
            end;
            if CutCriteria.LimitLength then
            begin
              if FCarvingPath.CarvingLength(TestElev.Z) > CutCriteria.MaxLength then
              begin
                Continue;
              end;
            end;
            ReferenceZ := TestElev.Z;
            AddPitNeighbors(ReferenceZ);
            PitCount := CountPits;
            Overflowed := PitCount <> PriorPitCount;
            PriorPitCount := PitCount;
            ComputeCost(TestElev);
            if TestElev.Cost < TotalCost then
            begin
              TotalCost := TestElev.Cost;
              BestElev := TestElev;
            end
            else if TestElev.Cost > TotalCost then
            begin
              if not Overflowed then
              begin
                Break;
              end;
            end;
          end;
          if BestElev = nil then
          begin
            ReferenceZ := PriorityPoint.Z;
          end
          else if ReferenceZ <> BestElev.Z then
          begin
            ReferenceZ := BestElev.Z;
          end;
          FillNewPitList(ReferenceZ, PriorityPoint);
          UpdateRaster(ReferenceZ);
        end;
      end;
    end;
    APoint.X := PriorityPoint.X;
    APoint.Y := PriorityPoint.Y;
    AddNeighbors(APoint);
  end;

//  AdjustFlowDirections;

  FlowDirections := FFlowDirections;
  Changed := FChanged;
end;

{ TPriorityPointList }

function TPriorityPointList.CarvingLength(ReferenceZ: Double): double;
var
  index: Integer;
  Point1: TPriorityPoint;
  Point2: TPriorityPoint;
begin
  result := 0;
  for index := 1 to Count - 1 do
  begin
    Point1 := Items[Index-1];
    Point2 := Items[Index];
    if (Point1.Z > ReferenceZ) or (Point2.Z > ReferenceZ) then
    begin
      result := Result + Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y));
    end;
  end;
end;

end.
