unit PestHeadObsWeightsUnit;

interface

uses
  ModflowIrregularMeshUnit, FastGEO;

type
  TObsWeights = array of TModflowIrregularCell2D;

procedure GetObsWeights(Cell: TModflowIrregularCell2D; ObsLocation: TPoint2D;
  out ObsCells: TObsWeights; Epsilon: double);

implementation

uses
  System.Math, GoPhastTypes, SubPolygonUnit;

procedure GetObsWeights(Cell: TModflowIrregularCell2D; ObsLocation: TPoint2D;
  out ObsCells: TObsWeights; Epsilon: double);
var
  CellList: TMFIrregularCell2D_List;
  ObsAngle: Double;
  CellIndex: Integer;
  NeighborCell: TModflowIrregularCell2D;
  NeighborAngle: Double;
  HigherNeighbor: TModflowIrregularCell2D;
  LowerNeighbor: TModflowIrregularCell2D;
  ClosestPoint: TPoint2D;
  NeighborIndex: Integer;
  LastNeighbor: TModflowIrregularCell2D;
  Points: TRealPointArray;
  Polygon: TSimplePolygon;
  SharedNode: TModflowNode;
  NodeIndex: Integer;
  ACell: TModflowIrregularCell2D;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
    end;
  end;
  function PointsNearlyTheSame(Point1, Point2: TPoint2D): boolean;
  begin
    result := NearlyTheSame(Point1.X, Point2.X) and
      NearlyTheSame(Point1.Y, Point2.Y);
  end;
begin
  if PointsNearlyTheSame(ObsLocation, Cell.Location) then
  begin
    SetLength(ObsCells, 1);
    ObsCells[0] := Cell;
    Exit;
  end;

  CellList := TMFIrregularCell2D_List.Create;
  try
    ObsAngle := ArcTan2(ObsLocation.y - Cell.Location.y,
      ObsLocation.x - Cell.Location.x);
    Cell.GetNeighborsInOrder(CellList);
    if CellList.Count > 1 then
    begin
      HigherNeighbor := nil;
      LowerNeighbor := nil;
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        NeighborCell := CellList[CellIndex];
        NeighborAngle := ArcTan2(NeighborCell.Location.y - Cell.Location.y,
          NeighborCell.Location.x - Cell.Location.x);
        if NeighborAngle > ObsAngle then
        begin
          HigherNeighbor := NeighborCell;
          if CellIndex > 0 then
          begin
            LowerNeighbor := CellList[CellIndex-1];
          end
          else
          begin
            LowerNeighbor := CellList[CellList.Count - 1];
          end;
          break;
        end;
      end;
      if HigherNeighbor = nil then
      begin
        HigherNeighbor := CellList[0];
        LowerNeighbor := CellList[CellList.Count - 1];
      end;
      if HigherNeighbor.IsNeighbor(LowerNeighbor) then
      begin
        ClosestPoint := ClosestPointOnSegmentFromPoint(
          EquateSegment(HigherNeighbor.Location, LowerNeighbor.Location),
          ObsLocation);

        if HigherNeighbor.PointInside(ClosestPoint) then
        begin
          NeighborIndex := CellList.IndexOf(HigherNeighbor) + 1;
          if NeighborIndex < CellList.Count then
          begin
            LastNeighbor := CellList[NeighborIndex];
          end
          else
          begin
            LastNeighbor := CellList[0];
          end;
          if (LastNeighbor <> HigherNeighbor) and (LastNeighbor <> LowerNeighbor) then
          begin
            if HigherNeighbor.ShareANode(LastNeighbor) then
            begin
              // construct weights using Cell, LowerNeighbor, HigherNeighbor,
              // and LastNeighbor in that order
              SetLength(ObsCells, 4);
              ObsCells[0] := Cell;
              ObsCells[1] := LowerNeighbor;
              ObsCells[2] := HigherNeighbor;
              ObsCells[3] := LastNeighbor;
              Exit;
            end
            else
            begin
              // construct weights using Cell, LowerNeighbor, and HigherNeighbor,
              // in that order
              SetLength(ObsCells, 3);
              ObsCells[0] := Cell;
              ObsCells[1] := LowerNeighbor;
              ObsCells[2] := HigherNeighbor;
              Exit;
            end;
          end
          else
          begin
            SetLength(Points, 4);
            Points[0] := Cell.Location;
            Points[1] := LowerNeighbor.Location;
            Points[2] := HigherNeighbor.Location;
            Points[3] := Points[0];
            Polygon := TSimplePolygon.Create(Points);
            try
              if Polygon.PointInside(ObsLocation) then
              begin
                // If the observation point is inside the triangle made up
                // of Cell, LowerNeighbor, and HigherNeighbor,
                // construct weights using Cell, LowerNeighbor, and HigherNeighbor,
                // in that order
                SetLength(ObsCells, 3);
                ObsCells[0] := Cell;
                ObsCells[1] := LowerNeighbor;
                ObsCells[2] := HigherNeighbor;
                Exit;
              end
              else
              begin
                // Otherwise, do linear interpolation depending on whether the
                // ClosestPoint is inside LowerNeighbor or HigherNeighbor.
                // In this case HigherNeighbor
                SetLength(ObsCells, 2);
                ObsCells[0] := Cell;
                ObsCells[1] := HigherNeighbor;
                Exit;
              end;
            finally
              Polygon.Free;
            end;
          end;
        end
        else
        begin
          Assert(LowerNeighbor.PointInside(ClosestPoint));
          NeighborIndex := CellList.IndexOf(LowerNeighbor) - 1;
          if NeighborIndex >= 0 then
          begin
            LastNeighbor := CellList[NeighborIndex];
          end
          else
          begin
            LastNeighbor := CellList[CellList.Count-1];
          end;
          // Set LastNeighbor to the neighbor before LowerNeighbor
          // and then


          if (LastNeighbor <> HigherNeighbor) and (LastNeighbor <> LowerNeighbor) then
          begin
            if LowerNeighbor.ShareANode(LastNeighbor) then
            begin
            // construct weights using Cell, LastNeighbor, LowerNeighbor, ,
            // and HigherNeighbor in that order (or without LastNeighbor if it
            // does not share a node with LowerNeighbor).
              SetLength(ObsCells, 4);
              ObsCells[0] := Cell;
              ObsCells[1] := LastNeighbor;
              ObsCells[2] := LowerNeighbor;
              ObsCells[3] := HigherNeighbor;
              Exit;
            end
            else
            begin
              // construct weights using Cell, LowerNeighbor, and HigherNeighbor,
              // in that order
              SetLength(ObsCells, 3);
              ObsCells[0] := Cell;
              ObsCells[1] := LowerNeighbor;
              ObsCells[2] := HigherNeighbor;
              Exit;
            end;
          end
          else
          begin
            SetLength(Points, 4);
            Points[0] := Cell.Location;
            Points[1] := LowerNeighbor.Location;
            Points[2] := HigherNeighbor.Location;
            Points[3] := Points[0];
            Polygon := TSimplePolygon.Create(Points);
            try
              if Polygon.PointInside(ObsLocation) then
              begin
                // If the observation point is inside the triangle made up
                // of Cell, LowerNeighbor, and HigherNeighbor,
                // construct weights using Cell, LowerNeighbor, and HigherNeighbor,
                // in that order
                SetLength(ObsCells, 3);
                ObsCells[0] := Cell;
                ObsCells[1] := LowerNeighbor;
                ObsCells[2] := HigherNeighbor;
                Exit;
              end
              else
              begin
                // Otherwise, do linear interpolation depending on whether the
                // ClosestPoint is inside LowerNeighbor or HigherNeighbor.
                // In this case LowerNeighbor
                SetLength(ObsCells, 2);
                ObsCells[0] := Cell;
                ObsCells[1] := LowerNeighbor;
                Exit;
              end;
            finally
              Polygon.Free;
            end;
          end;
        end;
      end
      else
      begin
        // Find cell that shares a node with LowerNeighbor and HigherNeighbor
        // but is not Cell if one exists.
        SharedNode := nil;
        for NodeIndex := 0 to LowerNeighbor.ElementCorners.Count - 1 do
        begin
          if HigherNeighbor.ElementCorners.IndexOf(
            LowerNeighbor.ElementCorners[NodeIndex]) >= 0 then
          begin
            SharedNode := LowerNeighbor.ElementCorners[NodeIndex];
            break;
          end;
        end;
        if SharedNode = nil then
        begin
          // Use linear interpolation to which ever neighbor is nearest.
          if Distance(ObsLocation, LowerNeighbor.Location)
            < Distance(ObsLocation, HigherNeighbor.Location) then
          begin
            ClosestPoint := ClosestPointOnSegmentFromPoint(
              EquateSegment(LowerNeighbor.Location, Cell.Location),
              ObsLocation);

            if PointsNearlyTheSame(ClosestPoint, Cell.Location) then
            begin
              SetLength(ObsCells, 1);
              ObsCells[0] := Cell;
              Exit;
            end
            else
            begin
              SetLength(ObsCells, 2);
              ObsCells[0] := Cell;
              ObsCells[1] := LowerNeighbor;
              Exit;
            end;

          end
          else
          begin
            ClosestPoint := ClosestPointOnSegmentFromPoint(
              EquateSegment(HigherNeighbor.Location, Cell.Location),
              ObsLocation);

            if PointsNearlyTheSame(ClosestPoint, Cell.Location) then
            begin
              SetLength(ObsCells, 1);
              ObsCells[0] := Cell;
              Exit;
            end
            else
            begin
              SetLength(ObsCells, 2);
              ObsCells[0] := Cell;
              ObsCells[1] := HigherNeighbor;
              Exit;
            end;
          end;
        end
        else
        begin
          // Construct weights using Cell, LowerNeighbor, LastNeighbor,
          // and HigherNeighbor in that order (or without LastNeighbor
          LastNeighbor := nil;
          Assert(SharedNode.ActiveElementCount = 4);
          for CellIndex := 0 to SharedNode.ActiveElementCount - 1 do
          begin
            ACell := SharedNode.Cells[CellIndex];
            if (ACell <> Cell) and (ACell <> LowerNeighbor)
               and (ACell <> HigherNeighbor) then
            begin
              LastNeighbor := ACell;
              break;
            end;
          end;
          Assert(LastNeighbor <> nil);
          SetLength(ObsCells, 4);
          ObsCells[0] := Cell;
          ObsCells[1] := LowerNeighbor;
          ObsCells[2] := LastNeighbor;
          ObsCells[3] := HigherNeighbor;
          Exit;
        end;
      end;
    end
    else
    begin
      if CellList.Count = 1 then
      begin
      // Do linear interpolation between the two nodes if the closest point
      // on the line between the cell centers to the point is in the middle of
      // the line.
        ACell := CellList[0];

        ClosestPoint := ClosestPointOnSegmentFromPoint(
          EquateSegment(Cell.Location, ACell.Location),
          ObsLocation);
        if PointsNearlyTheSame(ClosestPoint, Cell.Location) then
        begin
          SetLength(ObsCells, 1);
          ObsCells[0] := Cell;
          Exit;
        end
        else
        begin
          SetLength(ObsCells, 2);
          ObsCells[0] := Cell;
          ObsCells[1] := CellList[0];
          Exit;
        end;
      end
      else
      begin
        Assert(CellList.Count = 0);
        SetLength(ObsCells, 1);
        ObsCells[0] := Cell;
        Exit;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

end.
