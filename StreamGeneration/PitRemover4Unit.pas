unit PitRemover4Unit;

interface

uses Types, RasterValuesAlongSegmentsUnit, System.Generics.Collections, FastGEO,
  PriorityQueueUnit, System.Generics.Defaults, RealListUnit, GenericRasterUnit,
  SurferGridFileReaderUnit, System.SysUtils, SubPolygonUnit, RbwLadderQueue;

type
  TPriorityPoint = record
    X: integer;
    Y: Integer;
    Z: Double;
  end;

  TPriorityPointList = TList<TPriorityPoint>;

  TPointQueue = class(TRbwLadderQueue<TPriorityPoint>)
    // @name is used to arrange the points so that the point with
    // the lowest elevation has the highest priority.
//    function Compare (Left, Right: TPriorityPoint): Integer; Override;
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
//    function Compare (Left, Right: TTestElevation): Integer; Override;
  end;

type
  TBoolArray2D = array of array of Boolean;

  TProgressEvent = procedure(Sender: TObject; Position, Max: integer) of object;

  TPitRemover4 = class(TObject)
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
    FNewRaster: TGenericRaster;
    FFlowDirections: TFlowDirections;
    FOnProgress: TProgressEvent;
    FMaxPoints: Integer;
    procedure InitializeArrays(const Pits, StartPoints: TPointList);
    procedure InitializePointQueue(const StartPoints: TPointList);
    procedure GetCarvingPath(PriorityPoint: TPriorityPoint; ReferenceZ: double);
    procedure AddPitNeighbors(ReferenceZ: double);
    procedure TestNewElevations(const PriorityPoint: TPriorityPoint);
    procedure AddNeighbors(APoint: TPoint);
    procedure ComputeCost(TestElev: TTestElevation);
    procedure InitializeElevationObjects;
    function ElevationPosition(const PriorityPoint: TPriorityPoint): Integer;
    procedure CreateNewTestElevation(PositionIndex: Integer;
      const PriorityPoint: TPriorityPoint; var NewTestElev: TTestElevation);
    function CountPits: Integer;
    procedure UpdateRaster(ReferenceZ: Double);
    function GetMaxZInCarvingPath: Double;
    procedure FillNewPitList(ReferenceZ: double; PriorityPoint: TPriorityPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemovePits(const ARaster: IRaster;
      const StartPoints, Pits: TPointList; out NewRaster: IRaster;
      out FlowDirections: TFlowDirections);
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TPitRemover4SurfGrid7 = class(TObject)
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
    FNewRaster: TTempSurferRaster7File;
    FFlowDirections: TFlowDirections;
    FOnProgress: TProgressEvent;
    FMaxPoints: Int64;
    FChanged: Boolean;
    procedure InitializeArrays(const Pits, StartPoints: TPointList);
    procedure InitializePointQueue(const StartPoints: TPointList);
    procedure GetCarvingPath(PriorityPoint: TPriorityPoint; ReferenceZ: double);
    procedure AddPitNeighbors(ReferenceZ: double);
    procedure TestNewElevations(const PriorityPoint: TPriorityPoint);
    procedure AddNeighbors(APoint: TPoint);
    procedure ComputeCost(TestElev: TTestElevation);
    procedure InitializeElevationObjects;
    function ElevationPosition(const PriorityPoint: TPriorityPoint): Integer;
    procedure CreateNewTestElevation(PositionIndex: Integer;
      const PriorityPoint: TPriorityPoint; var NewTestElev: TTestElevation);
    function CountPits: Integer;
    procedure UpdateRaster(ReferenceZ: Double);
    function GetMaxZInCarvingPath: Double;
    procedure FillNewPitList(ReferenceZ: double; PriorityPoint: TPriorityPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemovePits(const ARaster: IRasterFile;
      const StartPoints, Pits: TPointList; out NewRaster: IRasterFile;
      out FlowDirections: TFlowDirections; Out Changed: boolean);
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

procedure IdentifyStartPointsAndPits(ARaster: IRaster;
  out EdgeStartPoints, Pits: TPointList; StartPointsOutline: TOutline;
  OnProgress: TProgressEvent);

implementation

uses
  System.Math, Vcl.Dialogs;

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
begin
  SetLength(EdgePoints, ARaster.XCount, ARaster.YCount);
  SetLength(IgnorePoints, ARaster.XCount, ARaster.YCount);
  MaxValue := ARaster.YCount * ARaster.XCount;
  UpdateDiv := MaxValue div 1000;
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  CurrentCount := 0;


  for YIndex := 0 to ARaster.YCount - 1 do
  begin
    for XIndex := 0 to ARaster.XCount - 1 do
    begin
      EdgePoints[XIndex, YIndex] := False;
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
//      IsPit := True;
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
          if StartPointsOutline.PointInside(Point2D) then
          begin
            List := EdgeStartPoints;
          end;
        end;
        List.Add(APoint);
      end;
    end;
  end;
end;

{ TGenericRaster }
{ TPointQueue }

//function TPointQueue.Compare(Left, Right: TPriorityPoint): Integer;
//begin
//  Result := Sign(Right.Z- Left.Z);
//end;
{ TTestElevationQueue }

//function TTestElevationQueue.Compare(Left, Right: TTestElevation): Integer;
//begin
//  Result := Sign(Right.Z- Left.Z);
//end;

constructor TTestElevationQueue.Create;
begin
  inherited Create(TComparer<TTestElevation>.Construct(
    function (const Left, Right: TTestElevation): Integer
    begin
      Result := Sign(Right.Z- Left.Z);
    end));
end;

{ TPitRemover4 }

procedure TPitRemover4.AddNeighbors(APoint: TPoint);
var
  InnerXStart: integer;
  InnerXEnd: Integer;
  InnerYStart: integer;
  InnerYEnd: Integer;
  XIndex: Integer;
  YIndex: Integer;
  AFlowDirection: TFlowDirection;
  PriorityPoint: TPriorityPoint;
begin
  InnerXStart := Max(0,APoint.X-1);
  InnerXEnd := Min(FNewRaster.XCount - 1, APoint.X+1);
  InnerYStart := Max(0,APoint.Y-1);
  InnerYEnd := Min(FNewRaster.YCount - 1, APoint.Y+1);
  for YIndex := InnerYStart to InnerYEnd do
  begin
    for XIndex := InnerXStart to InnerXEnd do
    begin
      if FUsed[XIndex,YIndex] then
      begin
        Continue;
      end;
      FUsed[XIndex,YIndex] := True;
      AFlowDirection.DeltaX := APoint.X - XIndex;
      AFlowDirection.DeltaY := APoint.Y - YIndex;
      FFlowDirections[XIndex, YIndex] := AFlowDirection;

      PriorityPoint.X := XIndex;
      PriorityPoint.Y := YIndex;
      PriorityPoint.Z := FNewRaster.Z[XIndex, YIndex];
      FPointQueue.Enqueue(PriorityPoint, PriorityPoint.Z);
    end;
  end
end;

procedure TPitRemover4.FillNewPitList(ReferenceZ: double;
  PriorityPoint: TPriorityPoint);
var
  PitIndex: Integer;
  APit: TPriorityPoint;
begin
  for PitIndex := 0 to FPitList.Count - 1 do
  begin
    APit := FPitList[PitIndex];
    FInPit[APit.X,APit.Y] := False;
  end;
  for PitIndex := 0 to FCarvingPath.Count - 1 do
  begin
    APit := FCarvingPath[PitIndex];
    FInPit[APit.x, APit.Y] := True;
  end;
  FPitList.Clear;
  FPitList.Add(PriorityPoint);
  AddPitNeighbors(ReferenceZ);
end;

procedure TPitRemover4.AddPitNeighbors(ReferenceZ: double);
var
  InnerXStart: integer;
  InnerXEnd: Integer;
  InnerYStart: integer;
  InnerYEnd: Integer;
  XIndex: Integer;
  YIndex: Integer;
  PPoint: TPriorityPoint;
  APoint: TPriorityPoint;
  PitIndex: Integer;
begin
  PitIndex := 0;
  while PitIndex < FPitList.Count do
  begin
    APoint := FPitList[PitIndex];
    InnerXStart := Max(0,APoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APoint.X+1);
    InnerYStart := Max(0,APoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if FInPit[XIndex,YIndex] then
        begin
          Continue;
        end;

        PPoint.Z := FNewRaster.Z[XIndex, YIndex];
        if PPoint.Z < ReferenceZ then
        begin
          PPoint.X := XIndex;
          PPoint.Y := YIndex;
          FPitList.Add(PPoint);
          FInPit[XIndex,YIndex] := True;
        end;
      end;
    end;
    Inc(PitIndex);
  end;
end;

procedure TPitRemover4.ComputeCost(TestElev: TTestElevation);
var
  Cost: double;
  Item: TPriorityPoint;
  AboveBarrier: Boolean;
  Index: Integer;
  NextCarvingPathPoint: TPriorityPoint;
  CarvingNeighborsToFill: TPriorityPointList;
  StartFilling: boolean;
  PointIndex: Integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: Integer;
    InnerYStart: Integer;
    InnerYEnd: Integer;
    YIndex: Integer;
    XIndex: Integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0,APriorityPoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APriorityPoint.X+1);
    InnerYStart := Max(0,APriorityPoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APriorityPoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;

        FlowDir := FFlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X)
          and (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := FNewRaster.Z[XIndex,YIndex];
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
        NextCarvingPathPoint := FCarvingPath[Index-1];
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
    TestElev.Computed := true;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;

constructor TPitRemover4.Create;
begin
  FPointQueue := TPointQueue.Create;
  FCarvingPath := TPriorityPointList.Create;
  FPitList := TPriorityPointList.Create;
  FElevations := TRealList.Create;
  FTestElevations := TTestElevationObjectList.Create;
  FElevationQueue := TTestElevationQueue.Create;
end;

destructor TPitRemover4.Destroy;
begin
  FElevationQueue.Free;
  FTestElevations.Free;
  FPitList.Free;
  FPointQueue.Free;
  FCarvingPath.Free;
  FElevations.Free;
//  FFlowDirections.Free;
  inherited;
end;

procedure TPitRemover4.GetCarvingPath(PriorityPoint: TPriorityPoint;
  ReferenceZ: double);
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
    DownStreamPoint.Z := FNewRaster.Z[DownStreamPoint.X, DownStreamPoint.y];
    FCarvingPath.Add(DownStreamPoint);
    if DownStreamPoint.Z <= ReferenceZ then
    begin
      Break;
    end;
  until False;

end;

procedure TPitRemover4.RemovePits(const ARaster: IRaster; const StartPoints,
  Pits: TPointList; out NewRaster: IRaster;
  out FlowDirections: TFlowDirections);
var
  PriorityPoint: TPriorityPoint;
  TotalCost: Double;
  APoint: TPoint;
  DownStreamPoint: TPriorityPoint;
  ReferenceZ: Double;
  PitIndex: Integer;
  APit: TPriorityPoint;
  PriorPitCount: Integer;
  PitCount: Integer;
  TestElev: TTestElevation;
  BestElev: TTestElevation;
  Overflowed: Boolean;
  MaxZ: double;
  ProgressDiv: integer;
  ProgressCount: integer;
begin
  FNewRaster := TGenericRaster.Create(ARaster);
  if Assigned(OnProgress) then
  begin
    OnProgress(self, 0, 1000);
  end;

  NewRaster := FNewRaster;
  InitializeArrays(Pits, StartPoints);

  FPointQueue.Free;
  FPointQueue := TPointQueue.Create;
  FCarvingPath.Clear;
  FPitList.Clear;
  FElevations.Clear;
  FTestElevations.Clear;
  FElevationQueue.Clear;

//  StoreElevations;
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
      GetCarvingPath(PriorityPoint, ReferenceZ);

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
    //        TestElev := nil;
            if APit.Z > PriorityPoint.Z then
            begin
              TestNewElevations(APit);
            end;
          end;

  //        if PriorityPoint.Z > FElevationQueue.Peek.Z then
  //        begin
  //          ReferenceZ := FElevationQueue.Peek.Z;
  //          GetCarvingPath(PriorityPoint, ReferenceZ);
  //
  //  //        TestElev := nil;
  //          TestNewElevations(PriorityPoint);
  //        end
  //        else
  //        begin
  //          ReferenceZ := PriorityPoint.Z;
  //        end;


          ReferenceZ := PriorityPoint.Z;
          TotalCost := MaxInt;
  //        TotalCost := 0;
  //        for DownStreamPoint in FCarvingPath do
  //        begin
  //          if DownStreamPoint.Z > ReferenceZ then
  //          begin
  //            TotalCost := TotalCost + DownStreamPoint.Z - ReferenceZ;
  //          end;
  //        end;

          FillNewPitList(ReferenceZ, PriorityPoint);
          PriorPitCount := CountPits;

          BestElev := nil;
          while FElevationQueue.Count > 0 do
          begin
            TestElev := FElevationQueue.Dequeue;
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
//      end
//      else
//      begin
//        beep;
      end;
    end;
    APoint.X := PriorityPoint.X;
    APoint.Y := PriorityPoint.Y;
    AddNeighbors(APoint);
  end;

  FlowDirections := FFlowDirections;
end;

function TPitRemover4.GetMaxZInCarvingPath: Double;
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

procedure TPitRemover4.UpdateRaster(ReferenceZ: Double);
var
  APitPoint: TPriorityPoint;
  ACarvingPathPoint: TPriorityPoint;
  PathIndex: Integer;
  PitIndex: Integer;
  StartFilling: Boolean;
  CarvingNeighborsToFill: TPriorityPointList;
  NextCarvingPathPoint: TPriorityPoint;
  PointIndex: Integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: Integer;
    InnerYStart: Integer;
    InnerYEnd: Integer;
    YIndex: Integer;
    XIndex: Integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0,APriorityPoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APriorityPoint.X+1);
    InnerYStart := Max(0,APriorityPoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APriorityPoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;

        FlowDir := FFlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X)
          and (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := FNewRaster.Z[XIndex,YIndex];
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
      if ACarvingPathPoint.Z >= ReferenceZ  then
      begin
        StartFilling := True;
      end;
      if StartFilling then
      begin
        if ACarvingPathPoint.Z <> ReferenceZ then
        begin
          FNewRaster.Z[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
        end;
        if PathIndex > 0 then
        begin
          NextCarvingPathPoint := FCarvingPath[PathIndex-1];
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
          FNewRaster.Z[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
        end;
      end;
    end;
    for PointIndex := 0 to CarvingNeighborsToFill.Count - 1 do
    begin
      APoint := CarvingNeighborsToFill[PointIndex];
//      Assert(APoint.Z < ReferenceZ);
//      Assert(FNewRaster.Z[APoint.X, APoint.Y] = APoint.Z);
      if FNewRaster.Z[APoint.X, APoint.Y] < ReferenceZ then
      begin
        FNewRaster.Z[APoint.X, APoint.Y] := ReferenceZ;
      end;
    end;
    for PitIndex := 0 to FPitList.Count - 1 do
    begin
      APitPoint := FPitList[PitIndex];
      if APitPoint.Z < ReferenceZ then
      begin
        FNewRaster.Z[APitPoint.X, APitPoint.Y] := ReferenceZ;
      end;
//      if PitIndex <> 0 then
      begin
        FInPit[APitPoint.X, APitPoint.Y] := False;
        FPitLocations[APitPoint.X, APitPoint.Y] := False;
      end;
    end;
  FPitList.Clear;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;

function TPitRemover4.CountPits: Integer;
var
  PitIndex: Integer;
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

procedure TPitRemover4.CreateNewTestElevation(PositionIndex: Integer;
  const PriorityPoint: TPriorityPoint;
  var NewTestElev: TTestElevation);
begin
  NewTestElev := TTestElevation.Create;
  NewTestElev.Z := PriorityPoint.Z;
  NewTestElev.Cost := 0;
  NewTestElev.PositionIndex := PositionIndex;
  NewTestElev.Computed := False;
  FTestElevations.Add(NewTestElev);
  FElevationQueue.Enqueue(NewTestElev);
end;

procedure TPitRemover4.InitializeElevationObjects;
begin
  FElevations.Clear;
  FElevations.Sorted := True;
  FElevationQueue.Clear;
  FTestElevations.Clear;
end;

procedure TPitRemover4.InitializePointQueue(const StartPoints: TPointList);
var
  APoint: TPoint;
  PointIndex: Integer;
begin
  for PointIndex := 0 to StartPoints.Count - 1 do
  begin
    APoint := StartPoints[PointIndex];
    AddNeighbors(APoint);
  end;
end;

function TPitRemover4.ElevationPosition(const PriorityPoint: TPriorityPoint): Integer;
begin
  result := FElevations.IndexOf(PriorityPoint.Z);
end;

procedure TPitRemover4.TestNewElevations(const PriorityPoint: TPriorityPoint);
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

procedure TPitRemover4.InitializeArrays(const Pits, StartPoints: TPointList);
var
  XIndex: Integer;
  APoint: TPoint;
  AFlowDirection: TFlowDirection;
  PointIndex: Integer;
  YIndex: Integer;
begin
  FFlowDirections := TFlowDirections.Create(FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FPitLocations, FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FUsed, FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FInPit, FNewRaster.XCount, FNewRaster.YCount);

  AFlowDirection.DeltaX := 0;
  AFlowDirection.DeltaY := 0;
  FMaxPoints := 0;
  for YIndex := 0 to FNewRaster.YCount - 1 do
  begin
    for XIndex := 0 to FNewRaster.XCount - 1 do
    begin
      FFlowDirections[XIndex, YIndex] := AFlowDirection;
      FUsed[XIndex, YIndex] := FNewRaster.Ignore[XIndex, YIndex];
      FInPit[XIndex, YIndex] := FUsed[XIndex, YIndex];
      FPitLocations[XIndex, YIndex] := False;
      if not FUsed[XIndex, YIndex] then
      begin
        Inc(FMaxPoints);
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

{ TPitRemover4SurfGrid7 }

procedure TPitRemover4SurfGrid7.AddNeighbors(APoint: TPoint);
var
  InnerXStart: integer;
  InnerXEnd: Integer;
  InnerYStart: integer;
  InnerYEnd: Integer;
  XIndex: Integer;
  YIndex: Integer;
  AFlowDirection: TFlowDirection;
  PriorityPoint: TPriorityPoint;
begin
  InnerXStart := Max(0,APoint.X-1);
  InnerXEnd := Min(FNewRaster.XCount - 1, APoint.X+1);
  InnerYStart := Max(0,APoint.Y-1);
  InnerYEnd := Min(FNewRaster.YCount - 1, APoint.Y+1);
  for YIndex := InnerYStart to InnerYEnd do
  begin
    for XIndex := InnerXStart to InnerXEnd do
    begin
      if FUsed[XIndex,YIndex] then
      begin
        Continue;
      end;
      FUsed[XIndex,YIndex] := True;
      AFlowDirection.DeltaX := APoint.X - XIndex;
      AFlowDirection.DeltaY := APoint.Y - YIndex;
      FFlowDirections[XIndex, YIndex] := AFlowDirection;
//      if (XIndex = 3414) and (YIndex = 1437) then
//      begin
//        Beep;
//      end;
//      if (XIndex = 3415) and (YIndex = 1438) then
//      begin
//        Beep;
//      end;

      PriorityPoint.X := XIndex;
      PriorityPoint.Y := YIndex;
      PriorityPoint.Z := FNewRaster.Z[XIndex, YIndex];
      FPointQueue.Enqueue(PriorityPoint, PriorityPoint.Z);
    end;
  end
end;

procedure TPitRemover4SurfGrid7.AddPitNeighbors(ReferenceZ: double);
var
  InnerXStart: integer;
  InnerXEnd: Integer;
  InnerYStart: integer;
  InnerYEnd: Integer;
  XIndex: Integer;
  YIndex: Integer;
  PPoint: TPriorityPoint;
  APoint: TPriorityPoint;
  PitIndex: Integer;
begin
  PitIndex := 0;
  while PitIndex < FPitList.Count do
  begin
    APoint := FPitList[PitIndex];
    InnerXStart := Max(0,APoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APoint.X+1);
    InnerYStart := Max(0,APoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if FInPit[XIndex,YIndex] then
        begin
          Continue;
        end;

        PPoint.Z := FNewRaster.Z[XIndex, YIndex];
        if PPoint.Z < ReferenceZ then
        begin
          PPoint.X := XIndex;
          PPoint.Y := YIndex;
          FPitList.Add(PPoint);
          FInPit[XIndex,YIndex] := True;
        end;
      end;
    end;
    Inc(PitIndex);
  end;
end;

procedure TPitRemover4SurfGrid7.ComputeCost(TestElev: TTestElevation);
var
  Cost: double;
  Item: TPriorityPoint;
  AboveBarrier: Boolean;
  Index: Integer;
  NextCarvingPathPoint: TPriorityPoint;
  CarvingNeighborsToFill: TPriorityPointList;
  StartFilling: boolean;
  PointIndex: Integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: Integer;
    InnerYStart: Integer;
    InnerYEnd: Integer;
    YIndex: Integer;
    XIndex: Integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0,APriorityPoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APriorityPoint.X+1);
    InnerYStart := Max(0,APriorityPoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APriorityPoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;

        FlowDir := FFlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X)
          and (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := FNewRaster.Z[XIndex,YIndex];
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
        NextCarvingPathPoint := FCarvingPath[Index-1];
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
    TestElev.Computed := true;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;
//var
//  Cost: double;
//  Item: TPriorityPoint;
//  AboveBarrier: Boolean;
//  Index: Integer;
//  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
//  var
//    InnerXStart: integer;
//    InnerXEnd: Integer;
//    InnerYStart: Integer;
//    InnerYEnd: Integer;
//    YIndex: Integer;
//    XIndex: Integer;
//    FlowDir: TFlowDirection;
//    NeighborPoint: TPriorityPoint;
//  begin
//    InnerXStart := Max(0,APriorityPoint.X-1);
//    InnerXEnd := Min(FNewRaster.XCount - 1, APriorityPoint.X+1);
//    InnerYStart := Max(0,APriorityPoint.Y-1);
//    InnerYEnd := Min(FNewRaster.YCount - 1, APriorityPoint.Y+1);
//    for YIndex := InnerYStart to InnerYEnd do
//    begin
//      for XIndex := InnerXStart to InnerXEnd do
//      begin
//        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
//        begin
//          Continue;
//        end;
//        if (YIndex = NextCarvingPathPoint.Y) and (XIndex = NextCarvingPathPoint.X) then
//        begin
//          Continue;
//        end;
//
//        FlowDir := FFlowDirections[XIndex,YIndex];
//        if (FlowDir.DeltaX + XIndex = APriorityPoint.X)
//          and (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
//        begin
//          NeighborPoint.X := XIndex;
//          NeighborPoint.Y := YIndex;
//          NeighborPoint.Z := FNewRaster.Z[XIndex,YIndex];
//          if NeighborPoint.Z < ReferenceZ then
//          begin
//            CarvingNeighborsToFill.Add(NeighborPoint);
//            FillCarvingPathNeighbors(NeighborPoint);
//          end;
//        end;
//      end;
//    end;
//  end;
//begin
//  Cost := 0;
//  AboveBarrier := False;
//  for Index := FCarvingPath.Count - 1 downto 0 do
//  begin
//    Item := FCarvingPath[Index];
//    if Item.Z > TestElev.Z then
//    begin
//      Cost := Cost + Item.Z - TestElev.Z;
//    end;
//    if Item.Z >= TestElev.Z then
//    begin
//      AboveBarrier := True;
//    end
//    else if not AboveBarrier then
//    begin
//      Cost := Cost + TestElev.Z - Item.Z;
//    end;
//  end;
//  for Item in FPitList do
//  begin
//    if Item.Z < TestElev.Z then
//    begin
//      Cost := Cost + TestElev.Z - Item.Z;
//    end;
//  end;
//  TestElev.Cost := Cost;
//  TestElev.Computed := true;
//end;

function TPitRemover4SurfGrid7.CountPits: Integer;
var
  PitIndex: Integer;
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

constructor TPitRemover4SurfGrid7.Create;
begin
  FPointQueue := TPointQueue.Create;
  FCarvingPath := TPriorityPointList.Create;
  FPitList := TPriorityPointList.Create;
  FElevations := TRealList.Create;
  FTestElevations := TTestElevationObjectList.Create;
  FElevationQueue := TTestElevationQueue.Create;
end;

procedure TPitRemover4SurfGrid7.CreateNewTestElevation(PositionIndex: Integer;
  const PriorityPoint: TPriorityPoint; var NewTestElev: TTestElevation);
begin
  NewTestElev := TTestElevation.Create;
  NewTestElev.Z := PriorityPoint.Z;
  NewTestElev.Cost := 0;
  NewTestElev.PositionIndex := PositionIndex;
  NewTestElev.Computed := False;
  FTestElevations.Add(NewTestElev);
  FElevationQueue.Enqueue(NewTestElev);
end;

destructor TPitRemover4SurfGrid7.Destroy;
begin
  FElevationQueue.Free;
  FTestElevations.Free;
  FPitList.Free;
  FPointQueue.Free;
  FCarvingPath.Free;
  FElevations.Free;
  inherited;
end;

function TPitRemover4SurfGrid7.ElevationPosition(
  const PriorityPoint: TPriorityPoint): Integer;
begin
  result := FElevations.IndexOf(PriorityPoint.Z);
end;

procedure TPitRemover4SurfGrid7.FillNewPitList(ReferenceZ: double;
  PriorityPoint: TPriorityPoint);
var
  PitIndex: Integer;
  APit: TPriorityPoint;
begin
  for PitIndex := 0 to FPitList.Count - 1 do
  begin
    APit := FPitList[PitIndex];
    FInPit[APit.X,APit.Y] := False;
  end;
  for PitIndex := 0 to FCarvingPath.Count - 1 do
  begin
    APit := FCarvingPath[PitIndex];
    FInPit[APit.x, APit.Y] := True;
  end;
  FPitList.Clear;
  FPitList.Add(PriorityPoint);
  AddPitNeighbors(ReferenceZ);
  for PitIndex := 0 to FCarvingPath.Count - 1 do
  begin
    APit := FCarvingPath[PitIndex];
    FInPit[APit.x, APit.Y] := False;
  end;
end;

procedure TPitRemover4SurfGrid7.GetCarvingPath(PriorityPoint: TPriorityPoint;
  ReferenceZ: double);
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
    DownStreamPoint.Z := FNewRaster.Z[DownStreamPoint.X, DownStreamPoint.y];
    FCarvingPath.Add(DownStreamPoint);
    if DownStreamPoint.Z <= ReferenceZ then
    begin
      Break;
    end;
//    if FCarvingPath.Count > 100 then
//    begin
//      Beep;
//    end;
  until False;

end;

function TPitRemover4SurfGrid7.GetMaxZInCarvingPath: Double;
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

procedure TPitRemover4SurfGrid7.InitializeArrays(const Pits,
  StartPoints: TPointList);
var
  XIndex: Integer;
  APoint: TPoint;
  AFlowDirection: TFlowDirection;
  PointIndex: Integer;
  YIndex: Integer;
  ProgressDiv: Integer;
  ProgressCount: Integer;
begin
  FFlowDirections := TFlowDirections.Create(FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FPitLocations, FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FUsed, FNewRaster.XCount, FNewRaster.YCount);
  SetLength(FInPit, FNewRaster.XCount, FNewRaster.YCount);

  ProgressDiv := (FNewRaster.YCount*FNewRaster.XCount) div 1000;
  ProgressCount := 0;


  AFlowDirection.DeltaX := 0;
  AFlowDirection.DeltaY := 0;
  FMaxPoints := 0;
  for YIndex := 0 to FNewRaster.YCount - 1 do
  begin
    for XIndex := 0 to FNewRaster.XCount - 1 do
    begin
      FFlowDirections[XIndex, YIndex] := AFlowDirection;
      FUsed[XIndex, YIndex] := FNewRaster.Ignore[XIndex, YIndex];
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
        begin
          OnProgress(self, ProgressCount div ProgressDiv, 1000);
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

procedure TPitRemover4SurfGrid7.InitializeElevationObjects;
begin
  FElevations.Clear;
  FElevations.Sorted := True;
  FElevationQueue.Clear;
  FTestElevations.Clear;
end;

procedure TPitRemover4SurfGrid7.InitializePointQueue(
  const StartPoints: TPointList);
var
  APoint: TPoint;
  PointIndex: Integer;
begin
  for PointIndex := 0 to StartPoints.Count - 1 do
  begin
    APoint := StartPoints[PointIndex];
    AddNeighbors(APoint);
  end;
end;

procedure TPitRemover4SurfGrid7.RemovePits(const ARaster: IRasterFile;
  const StartPoints, Pits: TPointList; out NewRaster: IRasterFile;
  out FlowDirections: TFlowDirections; Out Changed: boolean);
var
  PriorityPoint: TPriorityPoint;
  TotalCost: Double;
  APoint: TPoint;
  DownStreamPoint: TPriorityPoint;
  ReferenceZ: Double;
  PitIndex: Integer;
  APit: TPriorityPoint;
  PriorPitCount: Integer;
  PitCount: Integer;
  TestElev: TTestElevation;
  BestElev: TTestElevation;
  Overflowed: Boolean;
  MaxZ: double;
  ProgressDiv: integer;
  ProgressCount: integer;
//  RasterFile: IRasterFile;
begin
  FChanged := False;
  if Assigned(OnProgress) then
  begin
    OnProgress(self, 0, 1000);
  end;

//  if Supports(ARaster, IRasterFile, RasterFile) then
//  begin
    FNewRaster := TTempSurferRaster7File.Create(ARaster);
//  end
//  else
//  begin
//    Assert(False);
//  end;

  NewRaster := FNewRaster;
  InitializeArrays(Pits, StartPoints);

  FPointQueue.Free;
  FPointQueue := TPointQueue.Create;
  FCarvingPath.Clear;
  FPitList.Clear;
  FElevations.Clear;
  FTestElevations.Clear;
  FElevationQueue.Clear;

//  StoreElevations;
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
    try
      PriorityPoint := FPointQueue.Dequeue;
    except on EAssertionFailed do
      begin
      ShowMessage(IntToStr(ProgressCount));
      raise;
      end;
    end;
    if FPitLocations[PriorityPoint.X, PriorityPoint.Y] then
    begin
      // Handle Pit
      InitializeElevationObjects;

      TestNewElevations(PriorityPoint);
      ReferenceZ := PriorityPoint.Z;
      try
        GetCarvingPath(PriorityPoint, ReferenceZ);
      except
//        Beep;
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
    //        TestElev := nil;
            if APit.Z > PriorityPoint.Z then
            begin
              TestNewElevations(APit);
            end;
          end;

  //        if PriorityPoint.Z > FElevationQueue.Peek.Z then
  //        begin
  //          ReferenceZ := FElevationQueue.Peek.Z;
  //          GetCarvingPath(PriorityPoint, ReferenceZ);
  //
  //  //        TestElev := nil;
  //          TestNewElevations(PriorityPoint);
  //        end
  //        else
  //        begin
  //          ReferenceZ := PriorityPoint.Z;
  //        end;


          ReferenceZ := PriorityPoint.Z;
          TotalCost := MAXINT;
  //        TotalCost := 0;
  //        for DownStreamPoint in FCarvingPath do
  //        begin
  //          if DownStreamPoint.Z > ReferenceZ then
  //          begin
  //            TotalCost := TotalCost + DownStreamPoint.Z - ReferenceZ;
  //          end;
  //        end;

          FillNewPitList(ReferenceZ, PriorityPoint);
          PriorPitCount := CountPits;

          BestElev := nil;
          while FElevationQueue.Count > 0 do
          begin
            TestElev := FElevationQueue.Dequeue;
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
    try
    AddNeighbors(APoint);
    except on EAssertionFailed do
      begin
      ShowMessage(IntToStr(ProgressCount));
      raise;
      end;
    end;
  end;

  FlowDirections := FFlowDirections;
  Changed := FChanged;
end;

procedure TPitRemover4SurfGrid7.TestNewElevations(
  const PriorityPoint: TPriorityPoint);
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

procedure TPitRemover4SurfGrid7.UpdateRaster(ReferenceZ: Double);
var
  APitPoint: TPriorityPoint;
  ACarvingPathPoint: TPriorityPoint;
  PathIndex: Integer;
  PitIndex: Integer;
  StartFilling: Boolean;
  CarvingNeighborsToFill: TPriorityPointList;
  NextCarvingPathPoint: TPriorityPoint;
  PointIndex: Integer;
  APoint: TPriorityPoint;
  procedure FillCarvingPathNeighbors(APriorityPoint: TPriorityPoint);
  var
    InnerXStart: integer;
    InnerXEnd: Integer;
    InnerYStart: Integer;
    InnerYEnd: Integer;
    YIndex: Integer;
    XIndex: Integer;
    FlowDir: TFlowDirection;
    NeighborPoint: TPriorityPoint;
  begin
    InnerXStart := Max(0,APriorityPoint.X-1);
    InnerXEnd := Min(FNewRaster.XCount - 1, APriorityPoint.X+1);
    InnerYStart := Max(0,APriorityPoint.Y-1);
    InnerYEnd := Min(FNewRaster.YCount - 1, APriorityPoint.Y+1);
    for YIndex := InnerYStart to InnerYEnd do
    begin
      for XIndex := InnerXStart to InnerXEnd do
      begin
        if (YIndex = APriorityPoint.Y) and (XIndex = APriorityPoint.X) then
        begin
          Continue;
        end;
        if (YIndex = NextCarvingPathPoint.Y) and (XIndex = NextCarvingPathPoint.X) then
        begin
          Continue;
        end;

        FlowDir := FFlowDirections[XIndex,YIndex];
        if (FlowDir.DeltaX + XIndex = APriorityPoint.X)
          and (FlowDir.DeltaY + YIndex = APriorityPoint.Y) then
        begin
          NeighborPoint.X := XIndex;
          NeighborPoint.Y := YIndex;
          NeighborPoint.Z := FNewRaster.Z[XIndex,YIndex];
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
      if ACarvingPathPoint.Z >= ReferenceZ  then
      begin
        StartFilling := True;
      end;
      if StartFilling then
      begin
        if ACarvingPathPoint.Z <> ReferenceZ then
        begin
          FNewRaster.Z[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
          FChanged := True;
        end;
        if PathIndex > 0 then
        begin
          NextCarvingPathPoint := FCarvingPath[PathIndex-1];
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
          FNewRaster.Z[ACarvingPathPoint.X, ACarvingPathPoint.Y] := ReferenceZ;
          FChanged := True;
        end;
      end;
    end;
    for PointIndex := 0 to CarvingNeighborsToFill.Count - 1 do
    begin
      APoint := CarvingNeighborsToFill[PointIndex];
//      Assert(APoint.Z < ReferenceZ);
//      Assert(FNewRaster.Z[APoint.X, APoint.Y] = APoint.Z);
      if FNewRaster.Z[APoint.X, APoint.Y] < ReferenceZ then
      begin
        FNewRaster.Z[APoint.X, APoint.Y] := ReferenceZ;
        FChanged := True;
      end;
    end;
    for PitIndex := 0 to FPitList.Count - 1 do
    begin
      APitPoint := FPitList[PitIndex];
      if APitPoint.Z < ReferenceZ then
      begin
        FNewRaster.Z[APitPoint.X, APitPoint.Y] := ReferenceZ;
        FChanged := True;
      end;
//      if PitIndex <> 0 then
      begin
        FInPit[APitPoint.X, APitPoint.Y] := False;
        FPitLocations[APitPoint.X, APitPoint.Y] := False;
      end;
    end;
    FPitList.Clear;
  finally
    CarvingNeighborsToFill.Free;
  end;
end;

end.
