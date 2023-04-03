{@abstract(The purpose of @name is to supply the @link(GenerateGrid)
  function which is used to create the grid based on
  @link(ScreenObjectUnit.TScreenObject)s.)}
unit GridGeneration;

interface

uses System.UITypes, Windows, Classes, Dialogs, Forms, Controls;

{@name is used to create the grid based on
  @link(ScreenObjectUnit.TScreenObject)s.

  @returns(@name returns true if the grid was generated.
    If a grid was not created, @name returns false and ErrorMessage
    gives the reason why grid generation failed.)

  @param(ErrorMessage specifies the reason why grid generation failed.
  Ignore ErrorMessage if @name returns @True.)

  @param(SpecifyGridAngle specifies whether SpecifiedGridAngle
  should be used as the grid angle or whether SpecifiedGridAngle should be
  ignored.  Set SpecifyGridAngle to @true to use SpecifiedGridAngle.)

  @param(SpecifiedGridAngle is the grid angle that will be used in generating
  the grid. SpecifiedGridAngle is only used when SpecifyGridAngle is @true.)

  @param(SmoothColumns says whether the grid should be modified so that
  the ratio between the widths of adjacent columns is less than
  SmoothingCriterion.  If SmoothColumns is @true,
  the grid will be modified in this way.)

  @param(SmoothRows says whether the grid should be modified so that
  the ratio between the widths of adjacent rows is less than
  SmoothingCriterion.  If SmoothRows is @true,
  the grid will be modified in this way.)

  @param(SmoothLayers says whether the grid should be modified so that
  the ratio between the widths of adjacent layers is less than
  SmoothingCriterion.  If SmoothLayers is @true,
  the grid will be modified in this way.)

  @param(SmoothingCriterion is the desired ratio between the
  widths of adjacent columns, rows, or layers. SmoothingCriterion
  is ignored if SmoothColumns, SmoothRows, and SmoothLayers are all false.)
  }
function GenerateGrid(out ErrorMessage, WarningMessage: string;
  const SpecifyGridAngle: boolean; const SpecifiedGridAngle: real;
  const SmoothColumns, SmoothRows, SmoothLayers: boolean;
  const SmoothingCriterion: double): boolean;

function GenGridErrorMessage: string;
//resourcestring
//  StrToGenerateAGrid = 'To generate a grid, you need at least one ' +
//    'grid-definition polygon on the top view of the model';

implementation

uses frmGoPhastUnit, ScreenObjectUnit, AbstractGridUnit, GoPhastTypes, Math,
  Contnrs, RealListUnit, frmGoToUnit, frmSmoothGridUnit, UndoItems, FastGEO,
  SysUtils, System.Generics.Collections;

resourcestring
  StrFailedToGenerateG = 'Failed to generate grid';
  StrToGenerateAGridPhast = 'To generate a grid, you need at least one grid-' +
  'definition polygon on the top view of the model and one grid-definition o' +
  'bject on the side view.';
  StrToGenerateAGridModflow = 'To generate a grid, you need at least one gri' +
  'd-definition polygon on the top view of the model.';
  StrGridGenerationCancColumn = 'Grid generation canceled because only one ' +
  'column boundary would have been created.';
  StrGridGenerationCancRow = 'Grid generation canceled because only one row ' +
  'boundary would have been created.';
  StrGridGenerationCancLayer = 'Grid generation canceled because only one la' +
  'yer boundary would have been created.  This probably means that you need ' +
  'to create an object on the front or side view of the model in which you s' +
  'pecify the cell size.';
  StrUnableToGenerateG = 'Unable to generate grid. The grid would have had ' +
  'more than %d cells. Check that the objects you are using to define the grid' +
  ' have reasonable values for the grid cell size.';
  StrTheNewGridWillHa = 'The new grid will have %.0n cells. Do you want to con' +
  'tinue?';

type
  TZone = class(TObject)
  private
    FDesiredCellSize: real;
    LowerLimit: real;
    UpperLimit: real;
    procedure SetDesiredCellSize(const Value: real);
  public
    property DesiredCellSize: real read FDesiredCellSize write
      SetDesiredCellSize;
  end;

function SortZones(Item1, Item2: Pointer): Integer;
var
  Zone1, Zone2: TZone;
begin
  Zone1 := Item1;
  Zone2 := Item2;
  if Zone1.LowerLimit < Zone2.LowerLimit then
  begin
    result := -1
  end
  else if Zone1.LowerLimit > Zone2.LowerLimit then
  begin
    result := 1
  end
  else if Zone1.UpperLimit < Zone2.UpperLimit then
  begin
    result := -1
  end
  else if Zone1.UpperLimit > Zone2.UpperLimit then
  begin
    result := 1
  end
  else if Zone1.DesiredCellSize < Zone2.DesiredCellSize then
  begin
    result := -1
  end
  else if Zone1.DesiredCellSize > Zone2.DesiredCellSize then
  begin
    result := 1
  end
  else
  begin
    result := 0;
  end;
end;

function SortZonesBySize(Item1, Item2: Pointer): Integer;
var
  Zone1, Zone2: TZone;
begin
  Zone1 := Item1;
  Zone2 := Item2;
  if Zone1.DesiredCellSize < Zone2.DesiredCellSize then
  begin
    result := -1
  end
  else if Zone1.DesiredCellSize > Zone2.DesiredCellSize then
  begin
    result := 1
  end
  else
  begin
    result := 0;
  end;
end;

procedure SortZoneList(const ZoneList: TObjectList);
var
  ZoneIndex, InnerZoneIndex: integer;
  Subdivided: boolean;
  AZone, Zone1, Zone2: TZone;
begin
  repeat
    ZoneList.Sort(SortZones);
    Subdivided := False;
    for ZoneIndex := 0 to ZoneList.Count - 2 do
    begin
      Zone1 := ZoneList[ZoneIndex] as TZone;
      for InnerZoneIndex := ZoneIndex + 1 to ZoneList.Count - 1 do
      begin
        Zone2 := ZoneList[InnerZoneIndex] as TZone;

        if Zone2.LowerLimit >= Zone1.UpperLimit then
        begin
          break;
        end
        else if (Zone1.UpperLimit >= Zone2.UpperLimit)
          and (Zone1.DesiredCellSize <= Zone2.DesiredCellSize) then
        begin
          ZoneList.Delete(InnerZoneIndex);
          Subdivided := True;
          break;
        end
        else
        begin
          AZone := TZone.Create;
          ZoneList.Add(AZone);
          AZone.LowerLimit := Zone1.LowerLimit;
          AZone.UpperLimit := Zone2.LowerLimit;
          AZone.DesiredCellSize := Zone1.DesiredCellSize;
          if Zone1.DesiredCellSize < Zone2.DesiredCellSize then
          begin
            AZone.UpperLimit := Zone1.UpperLimit;
            Zone2.LowerLimit := AZone.UpperLimit;
          end;
          Subdivided := true;
          if Zone1.UpperLimit > Zone2.UpperLimit then
          begin
            AZone := TZone.Create;
            ZoneList.Add(AZone);
            AZone.LowerLimit := Zone2.UpperLimit;
            AZone.UpperLimit := Zone1.UpperLimit;
            AZone.DesiredCellSize := Zone1.DesiredCellSize;
          end;
          ZoneList.Delete(ZoneIndex);
          break;
        end;
      end;
      if Subdivided then
        break;
    end;
  until not Subdivided;
  for ZoneIndex := ZoneList.Count - 2 downto 0 do
  begin
    Zone1 := ZoneList[ZoneIndex] as TZone;
    Zone2 := ZoneList[ZoneIndex + 1] as TZone;
    if Zone1.DesiredCellSize = Zone2.DesiredCellSize then
    begin
      Zone1.UpperLimit := Zone2.UpperLimit;
      ZoneList.Delete(ZoneIndex + 1);
    end;
  end;
end;

procedure CreateCells(const ZoneList: TObjectList;
  const Positions: TRealList);
var
  Index, InnerIndex: integer;
  Capacity: integer;
  AZone, NeighborBelow, NeighborAbove: TZone;
  Limit: integer;
  ZoneSizeList: TList;
  NumberOfCells: integer;
  ZoneSize: Real;
  ZoneMiddle: Real;
  ZoneIndex: integer;
  NewZoneLimit: Real;
  UpperLimit, LowerLimit: Real;
  function GetLimit: integer;
  var
    DoubleLimit: double;
  begin
    DoubleLimit := (AZone.UpperLimit - AZone.LowerLimit)
      / AZone.DesiredCellSize;
    result := Round(DoubleLimit);
  end;
  function GetPosition: double;
  begin
    if InnerIndex = Limit then
    begin
      result := AZone.UpperLimit;
    end
    else
    begin
      result := InnerIndex / Limit * (AZone.UpperLimit - AZone.LowerLimit)
        + AZone.LowerLimit;
    end;
  end;
begin
  ZoneSizeList := TList.Create;
  try
    ZoneSizeList.Assign(ZoneList);
    ZoneSizeList.Sort(SortZonesBySize);
    for Index := 0 to ZoneSizeList.Count - 1 do
    begin
      AZone := ZoneSizeList[Index];
      ZoneIndex := ZoneList.IndexOf(AZone);

      if ZoneIndex > 0 then
      begin
        NeighborBelow := ZoneList[ZoneIndex-1] as TZone;
        if NeighborBelow.DesiredCellSize < AZone.DesiredCellSize then
        begin
          NeighborBelow := nil;
        end
        else if NeighborBelow.DesiredCellSize = AZone.DesiredCellSize then
        begin
          if ZoneSizeList.IndexOf(AZone) >
            ZoneSizeList.IndexOf(NeighborBelow) then
          begin
            NeighborBelow := nil;
          end;
        end;
      end
      else
      begin
        NeighborBelow := nil;
      end;

      if ZoneIndex < ZoneList.Count -1 then
      begin
        NeighborAbove := ZoneList[ZoneIndex+1] as TZone;
        if NeighborAbove.DesiredCellSize < AZone.DesiredCellSize then
        begin
          NeighborAbove := nil;
        end
        else if NeighborAbove.DesiredCellSize = AZone.DesiredCellSize then
        begin
          if ZoneSizeList.IndexOf(AZone) >
            ZoneSizeList.IndexOf(NeighborAbove) then
          begin
            NeighborAbove := nil;
          end;
        end;
      end
      else
      begin
        NeighborAbove := nil;
      end;

      if NeighborBelow = nil then
      begin
        LowerLimit := AZone.LowerLimit;
      end
      else
      begin
        LowerLimit := NeighborBelow.UpperLimit;
      end;

      if NeighborAbove = nil then
      begin
        UpperLimit := AZone.UpperLimit;
      end
      else
      begin
        UpperLimit := NeighborAbove.LowerLimit;
      end;

      NumberOfCells := Round((UpperLimit - LowerLimit)
        / AZone.DesiredCellSize);
      ZoneSize := NumberOfCells * AZone.DesiredCellSize;
      ZoneMiddle := (UpperLimit + LowerLimit)/2;

      if ZoneIndex > 0 then
      begin
        if NeighborBelow <> nil then
        begin
          if NeighborAbove = nil then
          begin
            NewZoneLimit := UpperLimit - ZoneSize;
          end
          else
          begin
            NewZoneLimit := ZoneMiddle - ZoneSize/2;
          end;
          NeighborBelow.UpperLimit := NewZoneLimit;
          AZone.LowerLimit := NewZoneLimit;
        end;
      end
      else
      begin
        if (NeighborAbove = nil) and (ZoneList.Count > 1) then
        begin
          NewZoneLimit := UpperLimit - ZoneSize;
        end
        else
        begin
          NewZoneLimit := ZoneMiddle - ZoneSize/2;
        end;
        AZone.LowerLimit := NewZoneLimit;
      end;

      if ZoneIndex < ZoneList.Count -1 then
      begin
        if NeighborABove <> nil then
        begin
          if NeighborBelow = nil then
          begin
            NewZoneLimit := LowerLimit + ZoneSize;
          end
          else
          begin
            NewZoneLimit := ZoneMiddle + ZoneSize/2;
          end;
          NeighborABove.LowerLimit := NewZoneLimit;
          AZone.UpperLimit := NewZoneLimit;
        end;
      end
      else
      begin
        if (NeighborBelow = nil) and (ZoneList.Count > 1) then
        begin
          NewZoneLimit := LowerLimit + ZoneSize;
        end
        else
        begin
          NewZoneLimit := ZoneMiddle + ZoneSize/2;
        end;
        AZone.UpperLimit := NewZoneLimit;
      end;

    end;
  finally
    ZoneSizeList.Free;
  end;

  Capacity := 1;
  for Index := 0 to ZoneList.Count - 1 do
  begin
    AZone := ZoneList[Index] as TZone;
    Capacity := Capacity + GetLimit;
  end;
  Positions.Clear;
  Positions.Capacity := Capacity;
  for Index := 0 to ZoneList.Count - 1 do
  begin
    AZone := ZoneList[Index] as TZone;
    if Index = 0 then
    begin
      Positions.Add(AZone.LowerLimit);
    end;
    Limit := GetLimit;
    for InnerIndex := 1 to Limit do
    begin
      Positions.Add(GetPosition);
    end;
  end;
end;

function GetLocalGrid: TCustomModelGrid;
begin
  if frmGoPhast.DisvUsed then
  begin
    result := frmGoPhast.PhastModel.ModflowGrid;
  end
  else
  begin
    result := frmGoPhast.Grid;
  end;
end;

procedure MoveToGridCenter;
var
  XCoordinate, YCoordinate, ZCoordinate: double;
  TopPoint: TPoint2D;
  FrontPoint, SidePoint: TPoint3D;
  LocalGrid: TCustomModelGrid;
begin
  LocalGrid := GetLocalGrid;

  TopPoint := LocalGrid.TwoDElementCenter(
    LocalGrid.ColumnCount div 2,
    LocalGrid.RowCount div 2);
  XCoordinate := TopPoint.X;
  YCoordinate := TopPoint.Y;
  SetTopPosition(XCoordinate, YCoordinate);

  FrontPoint := LocalGrid.RotatedThreeDElementCenter(
    LocalGrid.ColumnCount div 2, 0,
    LocalGrid.LayerCount div 2);
  XCoordinate := FrontPoint.X;
  ZCoordinate := FrontPoint.Z;
  SetFrontPosition(XCoordinate, ZCoordinate);

  SidePoint := LocalGrid.RotatedThreeDElementCenter(0,
    LocalGrid.RowCount div 2,
    LocalGrid.LayerCount div 2);
  YCoordinate := SidePoint.Y;
  ZCoordinate := SidePoint.Z;
  SetSidePosition(YCoordinate, ZCoordinate);
end;

function GenGridErrorMessage: string;
begin
  //      ErrorMessage := StrToGenerateAGrid;
  if frmGoPhast.ModelSelection = msPhast then
  begin
    result := StrToGenerateAGridPhast;
  end
  else
  begin
    result := StrToGenerateAGridModflow;
  end;
end;

function GenerateGrid(out ErrorMessage, WarningMessage: string;
  const SpecifyGridAngle: boolean; const SpecifiedGridAngle: real;
  const SmoothColumns, SmoothRows, SmoothLayers: boolean;
  const SmoothingCriterion: double): boolean;
var
  DomainList: TList<TScreenObject>;
  OutlineList: TList<TScreenObject>;
  ScreenObjectIndex, PointIndex: integer;
  AScreenObject: TScreenObject;
  Points, RotatatedPoints: TRealPointArray;
  ArraySize, Position: integer;
  DomainScreenObject: TScreenObject;
  Count: integer;
  AngleIndex: integer;
  Angle: real;
  APoint: TPoint2D;
  Area, TempArea: real;
  FinalAngle: real;
  MinX, MaxX, MinY, MaxY: real;
  ColumnZones, RowZones, LayerZones: TObjectList;
  XZone, YZone: TZone;
  DomainMinX, DomainMaxX, DomainMinY, DomainMaxY: real;
  ColumnPositions, RowPositions, LayerPositions: TRealList;
  Positions: TOneDRealArray;
  PositionIndex: integer;
  UndoCreateGrid: TUndoCreateGrid;
  DomainCount: integer;
  NewNumElements: integer;
  AMessage: string;
  DeletedObject: TScreenObject;
  procedure GetMinMax(const PointArray: TRealPointArray;
    out MinX, MaxX, MinY, MaxY: real);
  var
    PointIndex: integer;
  begin
    Assert(ArraySize > 0);
    APoint := PointArray[0];
    MinX := APoint.X;
    MaxX := APoint.X;
    MinY := APoint.Y;
    MaxY := APoint.Y;
    for PointIndex := 1 to ArraySize - 1 do
    begin
      APoint := PointArray[PointIndex];
      if APoint.X < MinX then
      begin
        MinX := APoint.X
      end
      else if APoint.X > MaxX then
      begin
        MaxX := APoint.X
      end;
      if APoint.Y < MinY then
      begin
        MinY := APoint.Y
      end
      else if APoint.Y > MaxY then
      begin
        MaxY := APoint.Y
      end;
    end;
  end;
  function ArrayArea(const PointArray: TRealPointArray;
    out MinX, MaxX, MinY, MaxY: real): real;
  begin
    GetMinMax(PointArray, MinX, MaxX, MinY, MaxY);
    result := (MaxX - MinX) * (MaxY - MinY);
  end;
  function RotatePoint(const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
  begin
    result := APoint;
    if Angle <> 0 then
    begin
      temp.X := Cos(-Angle) * result.X - Sin(-Angle) * result.Y;
      temp.Y := Sin(-Angle) * result.X + Cos(-Angle) * result.Y;
      result := temp;
    end;
  end;
begin
  ErrorMessage := StrFailedToGenerateG;
  WarningMessage := '';
  result := False;
  DomainList := TList<TScreenObject>.Create;
  OutlineList := TList<TScreenObject>.Create;
  try
    // Get a list of the domain screen objects.
    ArraySize := 0;
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.CellSizeUsed
        and not AScreenObject.Deleted then
      begin
        if frmGoPhast.ModelSelection <> msFootPrint then
        begin
          DomainList.Add(AScreenObject);
        end
        else if AScreenObject.ScreenObjectArea > 0 then
        begin
          DomainList.Add(AScreenObject);
        end;
        if (frmGoPhast.ModelSelection = msFootPrint) and (DomainList.Count > 1) then
        begin
          WarningMessage := 'Multiple objects were used to define the domain outline. Only the largest one will be used.';
          if DomainList[0].ScreenObjectArea > DomainList[1].ScreenObjectArea then
          begin
            DeletedObject := DomainList[1];
            DomainList.Delete(1);
          end
          else
          begin
            DeletedObject := DomainList[0];
            DomainList.Delete(0);
          end;
        end;
        if (AScreenObject.ViewDirection = vdTop) and AScreenObject.Closed
          and (frmGoPhast.ModelSelection <> msFootPrint) then
        begin
          ArraySize := ArraySize + AScreenObject.Count;
          OutlineList.Add(AScreenObject);

        end;
      end;
    end;
    if (frmGoPhast.ModelSelection = msFootPrint) and
      (DomainList.Count > 0) then
    begin
      OutlineList.Add(DomainList[0]);
      ArraySize := DomainList[0].Count;
    end;

    // test if it is possible to generate the grid.
    if OutlineList.Count = 0 then
    begin
      ErrorMessage := GenGridErrorMessage;
      Exit;
    end;

    SetLength(Points, ArraySize);
    SetLength(RotatatedPoints, ArraySize);

    Position := 0;
    for ScreenObjectIndex := 0 to OutlineList.Count - 1 do
    begin
      DomainScreenObject := OutlineList[ScreenObjectIndex];
      Count := DomainScreenObject.Count;
      DomainScreenObject.CopyPoints(Points, Position, 0, Count);
      Inc(Position, Count);
    end;
    Assert(Position = ArraySize);

    if SpecifyGridAngle then
    begin
      if SpecifiedGridAngle = 0 then
      begin
        GetMinMax(Points, DomainMinX, DomainMaxX, DomainMinY, DomainMaxY);
      end
      else
      begin
        Angle := SpecifiedGridAngle;
        for PointIndex := 0 to ArraySize - 1 do
        begin
          RotatatedPoints[PointIndex] := RotatePoint(Points[PointIndex]);
        end;
        GetMinMax(RotatatedPoints, DomainMinX, DomainMaxX,
          DomainMinY, DomainMaxY);
      end;
      FinalAngle := SpecifiedGridAngle;
    end
    else
    begin
      Area := ArrayArea(Points, DomainMinX, DomainMaxX, DomainMinY, DomainMaxY);
      FinalAngle := 0;

      // Test potential angles for grid in 1 degree increments.
      // Use the angle that makes the grid have the smallest area.
      for AngleIndex := -44 to 45 do
      begin
        // Skip AngleIndex because that was already determined prior to entry
        // to the loop.
        if AngleIndex = 0 then
        begin
          Continue;
        end;
        Angle := AngleIndex * Pi / 180;
        for PointIndex := 0 to ArraySize - 1 do
        begin
          RotatatedPoints[PointIndex] := RotatePoint(Points[PointIndex]);
        end;
        TempArea := ArrayArea(RotatatedPoints, MinX, MaxX, MinY, MaxY);
        if TempArea < Area then
        begin
          FinalAngle := Angle;
          DomainMinX := MinX;
          DomainMaxX := MaxX;
          DomainMinY := MinY;
          DomainMaxY := MaxY;
          Area := TempArea;
        end;
      end;
    end;
    Angle := FinalAngle;

    // determine zones
    ColumnZones := TObjectList.Create;
    RowZones := TObjectList.Create;
    LayerZones := TObjectList.Create;
    try
      for ScreenObjectIndex := 0 to DomainList.Count - 1 do
      begin
        DomainScreenObject := DomainList[ScreenObjectIndex];
        if DomainScreenObject.Count > Length(Points) then
        begin
          SetLength(Points, DomainScreenObject.Count);
        end;
        ArraySize := DomainScreenObject.Count;
        DomainCount := DomainScreenObject.Count;
        DomainScreenObject.CopyPoints(Points, 0, 0, DomainCount);
        Assert(DomainCount = ArraySize);
        if (DomainScreenObject.ViewDirection = vdTop) and (Angle <> 0) then
        begin
          for PointIndex := 0 to ArraySize - 1 do
          begin
            Points[PointIndex] := RotatePoint(Points[PointIndex]);
          end;
        end;
        GetMinMax(Points, MinX, MaxX, MinY, MaxY);

        XZone := TZone.Create;
        XZone.LowerLimit := MinX;
        XZone.UpperLimit := MaxX;
        XZone.DesiredCellSize := DomainScreenObject.CellSize;

        YZone := TZone.Create;
        YZone.LowerLimit := MinY;
        YZone.UpperLimit := MaxY;
        YZone.DesiredCellSize := DomainScreenObject.CellSize;
        case DomainScreenObject.ViewDirection of
          vdTop:
            begin
              if (XZone.UpperLimit <= DomainMinX) or
                (XZone.LowerLimit >= DomainMaxX) then
              begin
                XZone.Free;
              end
              else
              begin
                if XZone.LowerLimit < DomainMinX then
                begin
                  XZone.LowerLimit := DomainMinX;
                end;
                if XZone.UpperLimit > DomainMaxX then
                begin
                  XZone.UpperLimit := DomainMaxX;
                end;
                if XZone.LowerLimit = XZone.UpperLimit then
                begin
                  XZone.Free;
                end
                else
                begin
                  ColumnZones.Add(XZone);
                end;
              end;

              if (YZone.UpperLimit <= DomainMinY) or
                (YZone.LowerLimit >= DomainMaxY) then
              begin
                YZone.Free;
              end
              else
              begin
                if YZone.LowerLimit < DomainMinY then
                begin
                  YZone.LowerLimit := DomainMinY;
                end;
                if YZone.UpperLimit > DomainMaxY then
                begin
                  YZone.UpperLimit := DomainMaxY;
                end;
                if YZone.LowerLimit = YZone.UpperLimit then
                begin
                  YZone.Free;
                end
                else
                begin
                  RowZones.Add(YZone);
                end;
              end;
            end;
          vdFront:
            begin
              XZone.Free;
              LayerZones.Add(YZone);
            end;
          vdSide:
            begin
              YZone.Free;
              LayerZones.Add(XZone);
            end;
        else
          Assert(False);
        end;
      end;

      // zones complete
      SortZoneList(ColumnZones);
      SortZoneList(RowZones);
      SortZoneList(LayerZones);

      ColumnPositions := TRealList.Create;
      RowPositions := TRealList.Create;
      LayerPositions := TRealList.Create;
      try
        CreateCells(ColumnZones, ColumnPositions);
        CreateCells(RowZones, RowPositions);
        if frmGoPhast.ModelSelection = msPhast then
        begin
          CreateCells(LayerZones, LayerPositions);
        end;


        if ColumnPositions.Count <= 1 then
        begin
          ErrorMessage := StrGridGenerationCancColumn;
          Exit;
        end;
        if RowPositions.Count <= 1 then
        begin
          ErrorMessage := StrGridGenerationCancRow;
          Exit;
        end;
        if frmGoPhast.ModelSelection = msPhast then
        begin
          if LayerPositions.Count <= 1 then
          begin
            ErrorMessage := StrGridGenerationCancLayer;
            Exit;
          end;
        end;

        UndoCreateGrid := TUndoCreateGrid.Create;
        UndoCreateGrid.NewAngle := FinalAngle;

        SetLength(Positions, ColumnPositions.Count);
        for PositionIndex := 0 to ColumnPositions.Count - 1 do
        begin
          Positions[PositionIndex] := ColumnPositions[PositionIndex];
        end;
        if SmoothColumns then
        begin
          SmoothArray(Positions, SmoothingCriterion);
        end;
        UndoCreateGrid.FNewColumns := Positions;
        SetLength(UndoCreateGrid.FNewColumns, Length(UndoCreateGrid.FNewColumns));

        SetLength(Positions, RowPositions.Count);
        for PositionIndex := 0 to RowPositions.Count - 1 do
        begin
          Positions[PositionIndex] := RowPositions[PositionIndex];
        end;
        if SmoothRows then
        begin
          SmoothArray(Positions, SmoothingCriterion);
        end;
        UndoCreateGrid.FNewRows := Positions;
        SetLength(UndoCreateGrid.FNewRows, Length(UndoCreateGrid.FNewRows));

        if frmGoPhast.ModelSelection = msPhast then
        begin
          SetLength(Positions, LayerPositions.Count);
          for PositionIndex := 0 to LayerPositions.Count - 1 do
          begin
            Positions[PositionIndex] := LayerPositions[PositionIndex];
          end;
          if SmoothLayers then
          begin
            SmoothArray(Positions, SmoothingCriterion);
          end;
          UndoCreateGrid.FNewLayerElevations := Positions;
          SetLength(UndoCreateGrid.FNewLayerElevations,
            Length(UndoCreateGrid.FNewLayerElevations));
        end;

        try
          if frmGoPhast.ModelSelection = msPhast then
          begin
            NewNumElements := Length(UndoCreateGrid.FNewLayerElevations) *
              Length(UndoCreateGrid.FNewRows)
              * Length(UndoCreateGrid.FNewColumns);
          end
          else
          begin
            NewNumElements := frmGoPhast.PhastModel.LayerStructure.LayerCount *
              Length(UndoCreateGrid.FNewRows)
              * Length(UndoCreateGrid.FNewColumns);
          end;
          if NewNumElements >= 1000000 then
          begin
            Beep;
            AMessage := Format(StrTheNewGridWillHa, [NewNumElements + 0.0]);
            if MessageDlg(AMessage, mtWarning, [mbYes, mbNo], 0, mbNo) = mrNo then
            begin
              UndoCreateGrid.Free;
              // This prevents a wanning message from being displayed.
              ErrorMessage := '';
              Exit;
            end;
          end;
        except
          on EIntOverflow do
          begin
            Beep;
            ErrorMessage := Format(StrUnableToGenerateG, [MAXINT]);
            UndoCreateGrid.Free;
            Exit;
          end;
          on ERangeError do
          begin
            Beep;
            ErrorMessage := Format(StrUnableToGenerateG, [MAXINT]);
            UndoCreateGrid.Free;
            Exit;
          end;

        end;
        frmGoPhast.UndoStack.Submit(UndoCreateGrid);

        MoveToGridCenter;

        result := True;
      finally
        ColumnPositions.Free;
        RowPositions.Free;
        LayerPositions.Free;
      end;
    finally
      ColumnZones.Free;
      RowZones.Free;
      LayerZones.Free;
    end;
  finally
    DomainList.Free;
    OutlineList.Free;
  end;
end;

{ TZone }

procedure TZone.SetDesiredCellSize(const Value: real);
begin
  if Value <= 0 then
  begin
    FDesiredCellSize := 1;
  end
  else
  begin
    FDesiredCellSize := Value;
  end;
end;

end.

