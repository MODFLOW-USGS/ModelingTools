unit VertexUnit;

interface

uses Classes;

type
  TVertex = Class(TObject)
  private
    FX: extended;
    FY: extended;
    procedure SetX(const Value: extended);
    procedure SetY(const Value: extended);
  public
    Moved: boolean;
    Property X: extended read FX write SetX;
    Property Y: extended read FY write SetY;
    function Copy: TVertex;
    Constructor Create;
    function DistanceToVertex(AVertex: TVertex): extended;
  end;

implementation

{ TVertex }

function TVertex.Copy: TVertex;
begin
  result := TVertex.Create;
  result.FX := FX;
  result.FY := FY;
end;

constructor TVertex.Create;
begin
  inherited;
  Moved := False;
end;

function TVertex.DistanceToVertex(AVertex: TVertex): extended;
begin
  result := Sqrt(Sqr(AVertex.X - X) + Sqr(AVertex.Y - Y));
end;

procedure TVertex.SetX(const Value: extended);
begin
  FX := Value;
end;

procedure TVertex.SetY(const Value: extended);
begin
  FY := Value;
end;

end.
