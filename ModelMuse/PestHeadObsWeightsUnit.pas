unit PestHeadObsWeightsUnit;

interface

uses
  ModflowIrregularMeshUnit, FastGEO;

type
  TObsWeight = record
    Cell: TModflowIrregularCell2D;
    Weight: double;
  end;

  TObsWeights = array of TObsWeight;

procedure GetObsWeights(Cell: TModflowIrregularCell2D; ObsLocation: TPoint2D;
  out Weights: TObsWeights; Epsilon: double);

implementation

uses
  BasisFunctionUnit, System.Math;

procedure GetObsWeights(Cell: TModflowIrregularCell2D; ObsLocation: TPoint2D;
  out Weights: TObsWeights; Epsilon: double);
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
    SetLength(Weights, 1);
    Weights[0].Cell := Cell;
    Weights[0].Weight := 1;
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
          if HigherNeighbor.ShareANode(LastNeighbor) then
          begin
            // construct weights using Cell, LowerNeighbor, HigherNeigbor,
            // and LastNeighbor in that order
          end
          else
          begin
            // construct weights using Cell, LowerNeighbor, and HigherNeigbor,
            // in that order
          end;
        end
        else
        begin
          Assert(LowerNeighbor.PointInside(ClosestPoint));
          NeighborIndex := CellList.IndexOf(LowerNeighbor) - 1;
          // Set LastNeighbor to the neighbor before LowerNeighbor
          // and then
          // construct weights using Cell, LastNeighbor, LowerNeighbor, ,
          // and HigherNeigbor in that order (or without LastNeighbor if it
          // does not share a node with LowerNeighbor).
        end;

      end
      else
      begin
        // Find cell that shares a node with LowerNeighbor and HigherNeigbor
        // but is not Cell if one exists.
        // Construct weights using Cell, LastNeighbor, LowerNeighbor, ,
        // and HigherNeigbor in that order (or without LastNeighbor if it
        // does not share a node with LowerNeighbor).

        // figure out what to do if there is not another cell between
        // LowerNeighbor and HigherNeigbor
      end;
    end
    else
    begin
      // Do linear interpolation between the two nodes if the closest point
      // on the line between the cell centers to the point is in the middle of
      // the line.
    end;

  finally
    CellList.Free;
  end;
end;

end.
