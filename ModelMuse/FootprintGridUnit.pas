unit FootprintGridUnit;

interface

uses
  AbstractGridUnit, GoPhastTypes, ZoomBox2, System.Classes;

type
  TFootprintGrid = class(TCustomModelGrid)
  private
    FLayerElevations: TOneDRealArray;
  protected
    procedure DrawFront(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    procedure DrawSide(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); override;
    function GetCellElevation(const CellID: TZeroBasedID): real;
      override;
    function GetCellThickness(const Column, Row, Layer: integer): real;
      override;
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray;
      override;
    procedure SetCellElevation(const CellID: TZeroBasedID;
      const Value: real); override;
    // See @link(CellThickness).
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); override;
    procedure SetLayerCount(const Value: integer); override;
  public
    constructor Create(Model: TBaseModel);
    function GetContainingLayer(ACol, ARow: integer;
      const AZPosition: real): integer; override;
    function HighestElevation: real; override;
    function LowestElevation: real; override;

  end;

implementation

{ TFootprintGrid }

constructor TFootprintGrid.Create(Model: TBaseModel);
begin
  inherited Create(Model);
  inherited SetLayerCount(1);
  SetLength(FLayerElevations, 2);
  FLayerElevations[0] := 0;
  FLayerElevations[1] := 0;
  RowDirection := rdNorthToSouth;
end;

procedure TFootprintGrid.DrawFront(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
begin
// do nothing

end;

procedure TFootprintGrid.DrawSide(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
begin
// do nothing

end;

procedure TFootprintGrid.GetCellCornerElevations(const EvalAt: TEvaluatedAt;
  out Elevations: TThreeDRealArray);
var
  CCount, RCount, LCount: integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
begin
//  inherited;
  CCount := ColumnCount + 1;
  RCount := RowCount + 1;
  LCount := LayerCount + 1;
  SetLength(Elevations, CCount, RCount, LCount);

  for ColIndex := 0 to CCount - 1 do
  begin
    for RowIndex := 0 to RCount - 1 do
    begin
      for LayerIndex := 0 to LCount - 1 do
      begin
        Elevations[ColIndex, RowIndex, LayerIndex] := 0;
      end;
    end;
  end;

end;

function TFootprintGrid.GetCellElevation(const CellID: TZeroBasedID): real;
begin
  result := 0;
end;

function TFootprintGrid.GetCellThickness(const Column, Row,
  Layer: integer): real;
begin
  result := 0;
end;

function TFootprintGrid.GetContainingLayer(ACol, ARow: integer;
  const AZPosition: real): integer;
begin
  result := 0;
end;

function TFootprintGrid.GetTwoDCellElevations(const Col,
  Row: integer): TOneDRealArray;
begin
  result := FLayerElevations;
end;

function TFootprintGrid.HighestElevation: real;
begin
  Result := 0.;
end;

function TFootprintGrid.LowestElevation: real;
begin
  Result := 0.;
end;

procedure TFootprintGrid.SetCellElevation(const CellID: TZeroBasedID;
  const Value: real);
begin
  // do nothing

end;

procedure TFootprintGrid.SetCellThickness(const Column, Row, Layer: integer;
  const Value: real);
begin
  // do nothing

end;

procedure TFootprintGrid.SetLayerCount(const Value: integer);
begin
  // do nothing

end;

end.
