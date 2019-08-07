unit RegularTriMeshUnit;

interface

uses
  System.Generics.Collections, System.Types, GoPhastTypes, TriPackRoutines;

//type
//  TTwoDIntegerArray = array of array of Integer;
//  T2DBoolArray = array of array of Boolean;
//  TIntArray = array of Integer;

procedure GetTriangulation(const NodeNumbers: TTwoDIntegerArray;
  const Active: T2DBoolArray; var IADJ, IEND: TIntArray);

implementation

var
  Directions: TList<TPoint>;
  AltDirections: TList<TPoint>;
  NulPoint: TPoint;

procedure GetTriangulation(const NodeNumbers: TTwoDIntegerArray;
  const Active: T2DBoolArray; var IADJ, IEND: TIntArray);
var
  RowCount: Integer;
  ColCount: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  SkipFound: Boolean;
  APoint: TPoint;
  StartIndex: Integer;
  PointIndex: Integer;
  Neighbors: TList<Integer>;
  ADirection: TPoint;
  TestPoint: TPoint;
  IadjIndex: Integer;
  IEndIndex: Integer;
  SkipNext: Boolean;
  function OkPoint(APoint: TPoint): Boolean;
  begin
    result := (APoint.x >= 0) and (APoint.y >= 0)
      and (APoint.x < RowCount) and (APoint.y < ColCount);
    if result then
    begin
      result := Active[APoint.x, APoint.y];
    end;
  end;
  procedure MarkABreak;
  begin
    if (Neighbors.Count > 0) and (Neighbors.Last <> 0) then
    begin
      Neighbors.Add(0);
    end;
  end;
begin
// the two input arrays should be the same size and shape;
  Assert(Length(NodeNumbers) = Length(Active));
  RowCount := Length(NodeNumbers);
  Assert(RowCount > 0);

  Assert(Length(NodeNumbers[0]) = Length(Active[0]));
  ColCount := Length(NodeNumbers[0]);
  Assert(ColCount > 0);

  SetLength(IADJ, RowCount*ColCount*6);
  SetLength(IEnd, RowCount*ColCount);

  IadjIndex := 0;
  IEndIndex := 0;

  Neighbors := TList<Integer>.Create;
  try
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if not Active[RowIndex, ColIndex] then
        begin
          Continue;
        end;

        Neighbors.Clear;

        SkipFound := False;
        APoint.Create(RowIndex, ColIndex);

        StartIndex := 0;
        for PointIndex := 0 to 5 do
        begin
          ADirection := Directions[PointIndex];
          if not OkPoint(APoint + ADirection)
            and ((AltDirections[PointIndex] = NulPoint)
            or not OkPoint(APoint + AltDirections[PointIndex])) then
          begin
            SkipFound := True;
          end
          else
          begin
            if SkipFound then
            begin
              StartIndex := PointIndex;
              break;
            end;
          end;
        end;

        SkipNext := False;
//        ALineBuilder.Clear;
//        ALineBuilder.Append(Sg1.cells[ColIndex,RowIndex] + ' ');
        for PointIndex := StartIndex to StartIndex + 5 do
        begin
          TestPoint := APoint + Directions[PointIndex];
          if OkPoint(TestPoint) then
          begin
            Neighbors.Add(NodeNumbers[TestPoint.x,TestPoint.y]);
//            ALineBuilder.Append(Sg1.cells[TestPoint.x,TestPoint.y] + ' ');
            SkipNext := False;
          end
          else
          begin
            if AltDirections[PointIndex] <> NulPoint then
            begin
              TestPoint := APoint + AltDirections[PointIndex];
              if OkPoint(TestPoint) then
              begin
                Neighbors.Add(NodeNumbers[TestPoint.x,TestPoint.y]);
//                ALineBuilder.Append(Sg1.cells[TestPoint.x,TestPoint.y] + ' ');
                SkipNext := True;
              end
              else
              begin
//                Neighbors.Add(0);
                MarkABreak;
//                ALineBuilder.Append('0 ');
              end;
            end
            else if SkipNext then
            begin
//              Neighbors.Add(0);
              MarkABreak;
//              ALineBuilder.Append('0 ');
              SkipNext := False;
            end;
          end;
        end;

        TestPoint := APoint + Directions[StartIndex];
        if not OkPoint(TestPoint) then
        begin
  //          TestPoint := APoint + AltDirections[StartIndex];
          if AltDirections[StartIndex] <> NulPoint then
          begin
            MarkABreak;
//            Neighbors.Add(0);
          end;
        end;

        for PointIndex := 0 to Neighbors.Count - 1 do
        begin
          IADJ[IadjIndex] := Neighbors[PointIndex];
          Inc(IadjIndex);
        end;
        IEND[IEndIndex] := IadjIndex;
        Inc(IEndIndex);
//        memo1.Lines.Add(ALineBuilder.ToString);

      end;
    end;
  finally
    Neighbors.Free;
  end;

  SetLength(IADJ, IadjIndex);
  SetLength(IEND, IEndIndex);

end;

procedure InitializeDirections;
var
  APoint: TPoint;
  PointIndex: Integer;
begin
  APoint.x := 0;
  APoint.y := 1;
  Directions.Add(APoint);
  APoint.x := -1;
  Directions.Add(APoint);
  APoint.y := 0;
  Directions.Add(APoint);
  APoint.x := 0;
  APoint.y := -1;
  Directions.Add(APoint);
  APoint.x := 1;
  Directions.Add(APoint);
  APoint.y := 0;
  Directions.Add(APoint);

  for PointIndex := 0 to Directions.Count - 1 do
  begin
    Directions.Add(Directions[PointIndex]);
  end;

  APoint.x := 1;
  APoint.y := 1;
  AltDirections.Add(APoint);
  APoint.x := 0;
  APoint.y :=0;
  AltDirections.Add(APoint);
  APoint.x := -1;
  APoint.y := -1;
  AltDirections.Add(APoint);
  AltDirections.Add(APoint);
  APoint.x := 0;
  APoint.y :=0;
  AltDirections.Add(APoint);
  APoint.x := 1;
  APoint.y := 1;
  AltDirections.Add(APoint);

  for PointIndex := 0 to AltDirections.Count - 1 do
  begin
    AltDirections.Add(AltDirections[PointIndex]);
  end;

end;

initialization
  NulPoint := TPoint.Create(0,0);
  Directions := TList<TPoint>.Create;
  AltDirections := TList<TPoint>.Create;
  InitializeDirections;

finalization
  Directions.Free;
  AltDirections.Free;

end.
