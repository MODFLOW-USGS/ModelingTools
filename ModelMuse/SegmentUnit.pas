unit SegmentUnit;

interface

uses sysutils, Classes, Math, VertexUnit;

type
  TSegment = class(TObject)
  private
    FFirstVertex: TVertex;
    FSecondVertex: TVertex;
    function Intercept: double;
    procedure SimpleIntersection(ASegment: TSegment; ResultVertex: TVertex);
    function OverLap(ASegment: TSegment; ResultVertex: TVertex): boolean;
  public
    constructor Create(FirstVertex, SecondVertex: TVertex);
    destructor Destroy; override;
    function Intersection(ASegment: TSegment; ResultVertex: TVertex): boolean;
    property FirstVertex: TVertex read FFirstVertex;
    property SecondVertex: TVertex read FSecondVertex;
    function IsHorizontal: boolean;
    function IsVertical: boolean;
    function VertexWithinExtents(AVertex: TVertex): boolean;
    function Slope: double;
  end;

implementation

resourcestring
  StrErrorEvaluatingSlo = 'Error evaluating slope. Contact  developer.';

  { TSegment }

constructor TSegment.Create(FirstVertex, SecondVertex: TVertex);
begin
  inherited Create;
  FFirstVertex := FirstVertex.Copy;
  FSecondVertex := SecondVertex.Copy;
end;

destructor TSegment.Destroy;
begin
  FFirstVertex.Free;
  FSecondVertex.Free;
  inherited;
end;

function TSegment.Intercept: double;
begin
  result := FFirstVertex.Y - Slope * FFirstVertex.X;
end;

function TSegment.Intersection(ASegment: TSegment;
  ResultVertex: TVertex): boolean;
{ var
  ThisSlope, OtherSlope : double; }
begin
  if IsVertical then
  begin
    if ASegment.IsVertical then
    begin
      result := (FFirstVertex.X = ASegment.FFirstVertex.X) and
        OverLap(ASegment, ResultVertex);
    end
    else
    begin
      // OtherSlope := ASegment.Slope;
      ResultVertex.X := FFirstVertex.X;
      ResultVertex.Y := ASegment.Slope * ResultVertex.X + ASegment.Intercept;
      result := ASegment.VertexWithinExtents(ResultVertex) and
        VertexWithinExtents(ResultVertex);
    end;
  end
  else if ASegment.IsVertical then
  begin
    // ThisSlope := Slope;
    ResultVertex.X := ASegment.FFirstVertex.X;
    ResultVertex.Y := Slope * ResultVertex.X + Intercept;
    result := ASegment.VertexWithinExtents(ResultVertex) and
      VertexWithinExtents(ResultVertex);
  end
  else
  begin
    // ThisSlope := Slope;
    // OtherSlope := ASegment.Slope;
    if Slope = ASegment.Slope then
    begin
      result := (ASegment.Intercept = Intercept) and
        OverLap(ASegment, ResultVertex);
    end
    else
    begin
      SimpleIntersection(ASegment, ResultVertex);
      result := VertexWithinExtents(ResultVertex) and
        ASegment.VertexWithinExtents(ResultVertex);
    end;
  end;

end;

function TSegment.IsHorizontal: boolean;
begin
  result := FSecondVertex.Y = FFirstVertex.Y
end;

function TSegment.IsVertical: boolean;
begin
  result := FSecondVertex.X = FFirstVertex.X
end;

function TSegment.OverLap(ASegment: TSegment; ResultVertex: TVertex): boolean;
begin
  result := True;
  if VertexWithinExtents(ASegment.FFirstVertex) then
  begin
    ResultVertex.X := ASegment.FFirstVertex.X;
    ResultVertex.Y := ASegment.FFirstVertex.Y;
  end
  else if VertexWithinExtents(ASegment.FSecondVertex) then
  begin
    ResultVertex.X := ASegment.FSecondVertex.X;
    ResultVertex.Y := ASegment.FSecondVertex.Y;
  end
  else if ASegment.VertexWithinExtents(FFirstVertex) then
  begin
    ResultVertex.X := FFirstVertex.X;
    ResultVertex.Y := FFirstVertex.Y;
  end
  else if ASegment.VertexWithinExtents(FSecondVertex) then
  begin
    ResultVertex.X := FSecondVertex.X;
    ResultVertex.Y := FSecondVertex.Y;
  end
  else
  begin
    result := False;
  end;

end;

procedure TSegment.SimpleIntersection(ASegment: TSegment;
  ResultVertex: TVertex);
var
  X, Y: double;
  ThisSlope, OtherSlope: double;
  ThisIntercept, OtherIntercept: double;
begin
  ThisSlope := Slope;
  OtherSlope := ASegment.Slope;
  ThisIntercept := Intercept;
  OtherIntercept := ASegment.Intercept;

  X := (Intercept - OtherIntercept) / (OtherSlope - ThisSlope);
  ResultVertex.X := X;
  if OtherSlope = 0 then
  begin
    ResultVertex.Y := ASegment.FFirstVertex.Y;
  end
  else
  begin
    Y := ThisSlope * X + ThisIntercept;
    ResultVertex.Y := Y;
  end;
end;

function TSegment.Slope: double;
begin
  if IsVertical then
    raise EZeroDivide.Create(StrErrorEvaluatingSlo);
  result := (FSecondVertex.Y - FFirstVertex.Y) /
    (FSecondVertex.X - FFirstVertex.X);
end;

function TSegment.VertexWithinExtents(AVertex: TVertex): boolean;
Var
  Higher, Lower: double;
begin
  Higher := Max(FFirstVertex.X, FSecondVertex.X);
  Lower := Min(FFirstVertex.X, FSecondVertex.X);
  result := (Higher >= AVertex.X) and (AVertex.X >= Lower);
  if result then
  begin
    Higher := Max(FFirstVertex.Y, FSecondVertex.Y);
    Lower := Min(FFirstVertex.Y, FSecondVertex.Y);
    result := (Higher >= AVertex.Y) and (AVertex.Y >= Lower);
  end;
end;

end.
