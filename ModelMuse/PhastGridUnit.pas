{@abstract(@name defines @link(TPhastGrid) which is the grid used in PHAST.)}
unit PhastGridUnit;

interface

uses Types, Classes, SysUtils, Graphics, Forms,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  FastGEO, GoPhastTypes, AbstractGridUnit,
  DataSetUnit, ZoomBox2;

type
  {@abstract(@name defines the grid used with PHAST.)}
  TPhastGrid = class(TCustomModelGrid)
  private
    // @name: @link(TOneDRealArray);
    // See @link(LayerElevations).
    FLayerElevations: TOneDRealArray;
    // @name defines how to read layer elevations from a stream.
    // See @link(LayerElevations) and @link(DefineProperties).
    procedure ReadLayerElevations(Reader: TReader);
    // @name defines how to write layer elevations to a stream.
    // See @link(LayerElevations) and @link(DefineProperties).
    procedure WriteLayerElevations(Writer: TWriter);
    procedure DrawOrdinaryFrontLayers(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure DrawOrdinaryFrontColumns(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure DrawOrdinarySideLayers(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
    procedure DrawOrdinarySideRows(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TPersistent);
  protected
    // @name is used to define how layer elevations should be saved
    // and read from a file. See @link(ReadLayerElevations) and
    // @link(WriteLayerElevations).
    procedure DefineProperties(Filer: TFiler); override;
    // @name defines how to draw the front view of the grid.
    procedure DrawFront(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    // @name defines how to draw the side view of the grid.
    procedure DrawSide(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    // @name returns the elevations of the corners of cells in the grid.
    // See @link(AbstractGridUnit.TCustomModelGrid.GetCellCornerElevations).
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); override;
    // @name gets the elevation of a cell boundary.
    function GetCellElevation(const CellID: TZeroBasedID): real;
      override;
    // @name gets the vertical thickness of a cells.
    function GetCellThickness(const Column, Row, Layer: integer): real;
      override;
    // @name gets the elevation of a layer boundary.
    function GetLayerElevation(const Layer: integer): real;
    // @name sets the elevation of a cell boundary.
    procedure SetCellElevation(const CellID: TZeroBasedID;
      const Value: real); override;
    // @name gets the thickness of a layer.
    function GetLayerThickness(const Layer: integer): real;
    // @name sets the thickness of a cell.
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); override;
    // @name sets the number of layers in the grid.  There are
    // @link(AbstractGridUnit.TCustomModelGrid.LayerCount) + 1 layer boundaries
    // in the grid.
    // See @link(AbstractGridUnit.TCustomModelGrid.LayerCount).
    procedure SetLayerCount(const Value: integer); override;
    // @name specifies whether the layers are numbered from top to
    // bottom or from bottom to top.
    procedure SetLayerDirection(const Value: TLayerDirection); override;
    // @name sets the elevation of a layer.
    procedure SetLayerElevation(const Layer: integer;
      const Value: real);
    // @name sets all the elevations in the grid.
    procedure SetLayerElevations(const Value: TOneDRealArray);
    // @name sets the thickness of a layer.
    procedure SetLayerThickness(const Layer: integer;
      const Value: real);
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray; override;
  public
    // Add a new layer boundary with an elevation of Value
    procedure AddLayer(const Value: real);
    // If Source is a @classname, @name copies the layer elevations from
    // Source before calling inherited.
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    constructor Create(Model: TBaseModel);
    // @name deletes the layer boundary with the number ALayer.
    procedure DeleteLayer(const ALayer: integer);
    // @name returns the cell (viewed from the front) containing APoint.
    // Specifying BelowCol, AboveCol, BelowLayer and/or AboveLayer
    // can speed things up.
    function FrontContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt;
      const BelowCol: integer = -1;
      const AboveCol: integer = -1;
      const BelowLayer: integer = -1;
      const AboveLayer: integer = -1): T2DFrontCell;
    // @name gets a T3DCell containing Location.
    function GetCell(const Location: TPoint2D;
      const ViewDirection: TViewDirection;
      const EvaluatedAt: TEvaluatedAt): T3DCell;
    {
     @name gets the layer (not layer boundary) that contains APosition.
     If APosition < LayerElevations[0] then result := -1.
     If APosition > LayerElevations[LayerCount] then
       result := LayerCount-1. }
    function GetContainingLayer(ACol, ARow: integer; const AZPosition: real): integer; override;
    {@name returns the position of the center of a layer}
    function LayerCenter(const Layer: integer): real;
    // read or write the elevation of the layer boundary with the
    // number ALayer.
    property LayerElevation[const Layer: integer]: real
      read GetLayerElevation write SetLayerElevation;
    // LayerElevations is the array of boundaries between adjacent layers.
    // After editing LayerElevations, call UpdateLayerElevations to update the
    // number of layers and to make sure the layers are sorted in ascending
    // order.
    property LayerElevations: TOneDRealArray read FLayerElevations
      write SetLayerElevations;
    // read or write the thickness of a layer.
    property LayerThickness[const Layer: integer]: real
      read GetLayerThickness write SetLayerThickness;
    { @name returns the index of the layer center closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestLayerCenter(const APosition: real;
      First: integer = -1; Last: integer = -1): integer;
    // Find the layer boundary closest to APosition. First and Last can
    // be specified to increase efficiency.
    function NearestLayerPosition(const APosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    // @name returns the cell (viewed from the side) containing APoint.
    // Specifying BelowRow, AboveRow, BelowLayer and/or AboveLayer
    // can speed things up.
    function SideContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt;
      const BelowRow: integer = -1;
      const AboveRow: integer = -1;
      const BelowLayer: integer = -1;
      const AboveLayer: integer = -1): T2DSideCell;
    // After editing LayerElevations, call UpdateLayerElevations to update the
    // number of layers and to make sure the layer are sorted in ascending
    // order.
    procedure UpdateLayerElevations;  // update this.
    function HighestElevation: real; override;
    function LowestElevation: real; override;
    procedure BlockExtents(ViewDirection: TViewDirection;
      EvaluatedAt: TEvaluatedAt; Index: integer;
      out LowerBound, UpperBound: double);
    procedure DrawBlockExtents(ViewDirection: TViewDirection;
      EvaluatedAt: TEvaluatedAt; Index: integer;
      out LowerBound, UpperBound: double);
  end;

implementation

uses GR32_Polygons, RealListUnit, frmGoPhastUnit, BigCanvasMethods;

resourcestring
  StrLayerThicknessesMu = 'Layer thicknesses must be greater than or equal t' +
  'o 0.';

{ TPhastGrid }

procedure TPhastGrid.AddLayer(const Value: real);
begin
  SetLength(FLayerElevations, Length(FLayerElevations) + 1);
  FLayerElevations[Length(FLayerElevations) - 1] := Value;
  UpdateLayerElevations;
  GridChanged;
  LayersChanged;
end;

constructor TPhastGrid.Create(Model: TBaseModel);
begin
  inherited;
  SetLength(FLayerElevations, 0);
  //  FLayerElevations[0] := 0;
end;

procedure TPhastGrid.DeleteLayer(const ALayer: integer);
var
  Index: integer;
begin
  if (ALayer < 0) or (ALayer >= Length(FLayerElevations)) then
  begin
    Exit;
  end;
  for Index := ALayer + 1 to Length(FLayerElevations) - 1 do
  begin
    FLayerElevations[Index - 1] := FLayerElevations[Index];
  end;
  LayerCount := LayerCount - 1;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  GridChanged;
  LayersChanged;
end;

function TPhastGrid.GetCellElevation(const CellID: TZeroBasedID): real;
begin
  result := GetLayerElevation(CellID.Layer);
end;

function TPhastGrid.GetCellThickness(const Column, Row,
  Layer: integer): real;
begin
  result := GetLayerThickness(Layer);
end;

function TPhastGrid.GetLayerElevation(const Layer: integer): real;
begin
  if (Layer < 0) or (Layer > LayerCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  result := FLayerElevations[Layer];
end;

function TPhastGrid.GetLayerThickness(const Layer: integer): real;
begin
  result := LayerElevation[Layer + 1] - LayerElevation[Layer];
  if LayerDirection = ldTopToBottom then
  begin
    result := -result;
  end;
end;

function TPhastGrid.GetTwoDCellElevations(const Col,
  Row: integer): TOneDRealArray;
begin
  result := LayerElevations;
end;

function TPhastGrid.HighestElevation: real;
begin
  result := LayerElevations[LayerCount];
end;

function TPhastGrid.NearestLayerPosition(const APosition: real;
  const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(FLayerElevations, APosition, First, Last);
end;

procedure TPhastGrid.SetCellElevation(const CellID: TZeroBasedID;
  const Value: real);
begin
  if LayerElevation[CellID.Layer] <> Value then
  begin
    LayerElevation[CellID.Layer] := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    GridChanged;
    LayersChanged;
  end;
end;

procedure TPhastGrid.SetCellThickness(const Column, Row, Layer: integer;
  const Value: real);
begin
  if LayerThickness[Layer] <> Value then
  begin
    LayerThickness[Layer] := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    GridChanged;
    LayersChanged;
  end;
end;

procedure TPhastGrid.SetLayerCount(const Value: integer);
var
  Index: integer;
  LastValue: real;
begin
  if LayerCount <> Value then
  begin
    SetLength(FLayerElevations, Value + 1);
    if Value > LayerCount then
    begin
      if LayerCount >= 0 then
      begin
        LastValue := FLayerElevations[LayerCount];
      end
      else
      begin
        LastValue := 0;
      end;

      for Index := LayerCount + 1 to Value do
      begin
        FLayerElevations[Index] := LastValue;
      end;
      frmGoPhast.PhastModel.InvalidateSegments;
      frmGoPhast.PhastModel.InvalidateScreenObjects;
      frmGoPhast.InvalidateModel;
    end;
    LayersChanged;
    inherited;
  end;
end;

procedure TPhastGrid.SetLayerDirection(const Value: TLayerDirection);
var
  Index: Integer;
  Temp: real;
begin
  if LayerDirection <> Value then
  begin
    inherited;
    for Index := 0 to LayerCount div 2 do
    begin
      Temp := FLayerElevations[Index];
      FLayerElevations[Index] := FLayerElevations[LayerCount - Index];
      FLayerElevations[LayerCount - Index] := Temp;
      frmGoPhast.PhastModel.InvalidateSegments;
      frmGoPhast.PhastModel.InvalidateScreenObjects;
      frmGoPhast.InvalidateModel;
    end;
    LayersChanged;
  end;
end;

procedure TPhastGrid.SetLayerElevation(const Layer: integer;
  const Value: real);
begin
  if (Layer < 0) or (Layer > LayerCount) then
  begin
    raise EInvalidGrid.Create(Format(StrInvalidLayerNumber, [Layer]));
  end;
  if FLayerElevations[Layer] <> Value then
  begin
    FLayerElevations[Layer] := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    LayersChanged;
  end;

  if (LayerDirection = ldBottomToTop) then
  begin
    if ((Layer > 0) and (Value < FLayerElevations[Layer - 1]))
      or ((Layer < LayerCount) and (Value > FLayerElevations[Layer + 1])) then
    begin
      UpdateLayerElevations;
    end;
  end
  else
  begin
    if ((Layer > 0) and (Value > FLayerElevations[Layer - 1]))
      or ((Layer < LayerCount) and (Value < FLayerElevations[Layer + 1])) then
    begin
      UpdateLayerElevations;
    end;
  end;
end;

procedure TPhastGrid.SetLayerElevations(const Value: TOneDRealArray);
var
  Index: integer;
begin
  if SelectedLayer >  Length(Value) - 2 then
  begin
    SelectedLayer := Length(Value) - 2;
  end;

  SetLength(FLayerElevations, Length(Value));
  for Index := 0 to Length(Value) - 1 do
  begin
    FLayerElevations[Index] := Value[Index];
  end;
  //  FLayerElevations := Value;
  UpdateLayerElevations;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  GridChanged;
  LayersChanged;
end;

procedure TPhastGrid.SetLayerThickness(const Layer: integer;
  const Value: real);
var
  Delta: real;
  Index: integer;
begin
  if LayerThickness[Layer] <> Value then
  begin
    FNeedToRecalculateSideCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    if Value < 0 then
    begin
      raise EInvalidGrid.Create(StrLayerThicknessesMu);
    end;
    Delta := LayerElevation[Layer] - Value;
    for Index := Layer + 1 to LayerCount do
    begin
      if LayerDirection = ldBottomToTop then
      begin
        FLayerElevations[Index] := FLayerElevations[Index] - Delta;
      end
      else
      begin
        FLayerElevations[Index] := FLayerElevations[Index] + Delta;
      end;
    end;
    GridChanged;
    LayersChanged;
  end;
end;

procedure TPhastGrid.UpdateLayerElevations;
var
  ARealList: TRealList;
  Index: integer;
begin
  inherited SetLayerCount(Length(FLayerElevations) - 1);
  FNeedToRecalculateFrontCellColors := True;
  FNeedToRecalculateSideCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.UpdateDataSetDimensions;
  frmGoPhast.InvalidateModel;
  ARealList := TRealList.Create;
  try
    ARealList.Capacity := LayerCount + 1;
    if LayerDirection = ldBottomToTop then
    begin
      for Index := 0 to LayerCount do
      begin
        ARealList.Add(FLayerElevations[Index]);
      end;
    end
    else
    begin
      for Index := LayerCount downto 0 do
      begin
        ARealList.Add(FLayerElevations[Index]);
      end;
    end;

    ARealList.Sort;

    for Index := ARealList.Count - 1 downto 1 do
    begin
      if ARealList[Index] = ARealList[Index - 1] then
      begin
        ARealList.Delete(Index);
      end;
    end;

    LayerCount := ARealList.Count - 1;

    if LayerDirection = ldBottomToTop then
    begin
      for Index := 0 to LayerCount do
      begin
        FLayerElevations[Index] := ARealList[Index];
      end;
    end
    else
    begin
      for Index := LayerCount downto 0 do
      begin
        FLayerElevations[LayerCount-Index] := ARealList[Index];
      end;
    end;
    if SelectedLayer < 0 then
    begin
      SelectedLayer := LayerCount - 1;
    end;

  finally
    ARealList.Free;
  end;
  LayersChanged;
end;

function TPhastGrid.FrontContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt; const BelowCol, AboveCol,
  BelowLayer, AboveLayer: integer): T2DFrontCell;
begin
  result.Col := NearestColumnPosition(APoint.X);
  result.Lay := NearestLayerPosition(APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        Exit;
      end;
  else
    Assert(False);
  end;

  if result.Col < 0 then
  begin
    result.Col := 0;
  end;
  if result.Lay < 0 then
  begin
    result.Lay := 0;
  end;

  if result.Col >= ColumnCount then
  begin
    if EvaluatedAt = eaBlocks then
    begin
      result.Col := ColumnCount -1;
    end
    else
    begin
      Dec(result.Col);
    end;
  end;
  if result.Lay >= LayerCount then
  begin
    if EvaluatedAt = eaBlocks then
    begin
      result.Lay := LayerCount -1;
    end
    else
    begin
      Dec(result.Lay);
    end;
  end;
  if ColumnDirection = cdWestToEast then
  begin
    if (result.Col > 0) and (ColumnPosition[result.Col] > APoint.X) then
    begin
      Dec(result.Col);
    end;
  end
  else
  begin
    if (result.Col > 0) and (ColumnPosition[result.Col] < APoint.X) then
    begin
      Dec(result.Col);
    end;
  end;
  if LayerDirection = ldBottomToTop then
  begin
    if (result.Lay > 0) and (LayerElevation[result.Lay] > APoint.Y) then
    begin
      Dec(result.Lay);
    end;
  end
  else
  begin
    if (result.Lay > 0) and (LayerElevation[result.Lay] < APoint.Y) then
    begin
      Dec(result.Lay);
    end;
  end;
end;

procedure TPhastGrid.DrawFront(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
var
  LayerIndex, ColumnIndex: integer;
  Point1, Point2, Point3, Point4: TPoint3D;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  BoxCoordinates: TPointArray;
  APoint: TPoint;
  ColumnLimit, LayerLimit: integer;
  LineWidth: single;
  P: TPolygon32;
  MultiplePolygons: boolean;
  function GetNodeBasedCorner(Col, Row, Lay: integer): TPoint3D;
  begin
    if (Col = 0) or (Col > ColumnCount) then
    begin
      if (Col > ColumnCount) then
      begin
        Dec(Col);
      end;
      if (Lay = 0) or (Lay > LayerCount) then
      begin
        if (Lay > LayerCount) then
        begin
          Dec(Lay);
        end;
        result := ThreeDElementCorner(Col, Row, Lay);
      end
      else
      begin
        result := ThreeDLayerEdgeCenter(Col, Row - 1, Lay - 1);
      end;
    end
    else
    begin
      if (Lay = 0) or (Lay > LayerCount) then
      begin
        if Lay > LayerCount then
        begin
          Dec(Lay);
        end;
        result := ThreeDRowEdgeCenter(Col - 1, Row - 1, Lay);
      end
      else
      begin
        result := ThreeDElementCenter(Col - 1, Row - 1, Lay - 1);
      end;
    end;
  end;
begin
  P := nil;
  MultiplePolygons := False;
  SetLength(BoxCoordinates, 4);
//  with ZoomBox do
  begin

    if (ColumnCount <= 0)
      or (RowCount <= 0)
      or (LayerCount <= 0) then
    begin
      LineWidth := OrdinaryGridLineThickness;
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (GridLineDrawingChoice = gldcExterior) and (ColumnIndex <> 0)
          and (ColumnIndex <> ColumnCount) then
        begin
          Continue;
        end;
        APoint.X := ZoomBox.XCoord(ColumnPosition[ColumnIndex]);
        APoint.Y := 0;
        BoxCoordinates[0] := APoint;
        APoint.Y := ZoomBox.Image32.Height;
        BoxCoordinates[1] := APoint;
        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;

      for LayerIndex := 0 to LayerCount do
      begin
        if (GridLineDrawingChoice = gldcExterior) and (LayerIndex <> 0)
          and (LayerIndex <> LayerCount) then
        begin
          Continue;
        end;
        APoint.X := 0;
        APoint.Y := ZoomBox.YCoord(LayerElevation[LayerIndex]);
        BoxCoordinates[0] := APoint;
        APoint.X := ZoomBox.Image32.Width;
        BoxCoordinates[1] := APoint;
        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;
    end
    else
    begin
      if FrontDataSet <> nil then
      begin
        case FrontDataSet.EvaluatedAt of
          eaBlocks:
            begin
              ColumnLimit := ColumnCount - 1;
              LayerLimit := LayerCount - 1;
            end;
          eaNodes:
            begin
              ColumnLimit := ColumnCount;
              LayerLimit := LayerCount;
            end;
        else
          begin
            Assert(False);
            ColumnLimit := -1;
            LayerLimit := -1;
          end;
        end;

        for ColumnIndex := 0 to ColumnLimit do
        begin
          if LayerCount > 0 then
          begin
            PriorLayer := 0;
            AColor := FrontCellColors[ColumnIndex, 0];
            NewColor := AColor;
            for LayerIndex := 1 to LayerLimit do
            begin
              NewColor := FrontCellColors[ColumnIndex, LayerIndex];
              if (NewColor <> AColor) then
              begin
                if AColor <> clWhite then
                begin
                  case FrontDataSet.EvaluatedAt of
                    eaBlocks:
                      begin
                        Point1 := ThreeDElementCorner(ColumnIndex, 0, PriorLayer);
                        Point2 := ThreeDElementCorner(ColumnIndex + 1, 0,
                          PriorLayer);
                        Point3 := ThreeDElementCorner(ColumnIndex + 1, 0,
                          LayerIndex);
                        Point4 := ThreeDElementCorner(ColumnIndex, 0, LayerIndex);
                      end;
                    eaNodes:
                      begin
                        Point1 := GetNodeBasedCorner(ColumnIndex, 1,
                          PriorLayer);
                        Point2 := GetNodeBasedCorner(ColumnIndex + 1, 1,
                          PriorLayer);
                        Point3 := GetNodeBasedCorner(ColumnIndex + 1, 1,
                          LayerIndex);
                        Point4 := GetNodeBasedCorner(ColumnIndex, 1,
                          LayerIndex);
                      end;
                  else
                    begin
                      Assert(False);
                    end;
                  end;
                  BoxCoordinates[0] := Point(ZoomBox.XCoord(Point1.X),
                    ZoomBox.YCoord(Point1.Z));
                  BoxCoordinates[1] := Point(ZoomBox.XCoord(Point2.X),
                    ZoomBox.YCoord(Point2.Z));
                  BoxCoordinates[2] := Point(ZoomBox.XCoord(Point3.X),
                    ZoomBox.YCoord(Point3.Z));
                  BoxCoordinates[3] := Point(ZoomBox.XCoord(Point4.X),
                    ZoomBox.YCoord(Point4.Z));
                  DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                    0, BoxCoordinates, P, MultiplePolygons, True);
                end;
                AColor := NewColor;
                PriorLayer := LayerIndex;

              end;
//              Application.ProcessMessages;
            end;

            if NewColor <> clWhite then
            begin
              case FrontDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    Point1 := ThreeDElementCorner(ColumnIndex, 0, PriorLayer);
                    Point2 := ThreeDElementCorner(ColumnIndex + 1, 0, PriorLayer);
                    Point3 := ThreeDElementCorner(ColumnIndex + 1, 0, LayerCount);
                    Point4 := ThreeDElementCorner(ColumnIndex, 0, LayerCount);
                  end;
                eaNodes:
                  begin
                    Point1 := GetNodeBasedCorner(ColumnIndex, 1, PriorLayer);
                    Point2 := GetNodeBasedCorner(ColumnIndex + 1, 1,
                      PriorLayer);
                    Point3 := GetNodeBasedCorner(ColumnIndex + 1, 1, LayerCount
                      + 1);
                    Point4 := GetNodeBasedCorner(ColumnIndex, 1, LayerCount +
                      1);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              BoxCoordinates[0] := Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z));
              BoxCoordinates[1] := Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z));
              BoxCoordinates[2] := Point(ZoomBox.XCoord(Point3.X), ZoomBox.YCoord(Point3.Z));
              BoxCoordinates[3] := Point(ZoomBox.XCoord(Point4.X), ZoomBox.YCoord(Point4.Z));

              DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
                0, BoxCoordinates, P, MultiplePolygons, True);
            end;
          end;
        end;
      end;
      DrawOrdinaryFrontColumns(BitMap, ZoomBox);
      DrawOrdinaryFrontLayers(BitMap, ZoomBox);

    end;
  end;
  DrawFrontContours(ZoomBox, BitMap);
end;

procedure TPhastGrid.DrawSide(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2);
var
  LayerIndex, RowIndex: integer;
  Point1, Point2, Point3, Point4: TPoint3D;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  BoxCoordinates: TPointArray;
  APoint: TPoint;
  RowLimit, LayerLimit: integer;
  LineWidth: single;
  P: TPolygon32;
  MultiplePolygons: boolean;
  function GetNodeBasedCorner(Col, Row, Lay: integer): TPoint3D;
  begin
    if (Row = 0) or (Row > RowCount) then
    begin
      if (Row > RowCount) then
      begin
        Dec(Row);
      end;
      if (Lay = 0) or (Lay > LayerCount) then
      begin
        if (Lay > LayerCount) then
        begin
          Dec(Lay);
        end;
        result := ThreeDElementCorner(Col, Row, Lay);
      end
      else
      begin
        result := ThreeDLayerEdgeCenter(Col, Row, Lay - 1);
      end;
    end
    else
    begin
      if (Lay = 0) or (Lay > LayerCount) then
      begin
        if Lay > LayerCount then
        begin
          Dec(Lay);
        end;
        result := ThreeDColumnEdgeCenter(Col, Row - 1, Lay);
      end
      else
      begin
        result := ThreeDElementCenter(Col, Row - 1, Lay - 1);
      end;
    end;
  end;
begin
  P := nil;
  MultiplePolygons := False;
  SetLength(BoxCoordinates, 4);
//  with ZoomBox do
  begin
    LineWidth := OrdinaryGridLineThickness;
    if (ColumnCount <= 0)
      or (RowCount <= 0)
      or (LayerCount <= 0) then
    begin
      for RowIndex := 0 to RowCount do
      begin
        APoint.Y := ZoomBox.YCoord(RowPosition[RowIndex]);
        APoint.X := 0;
        BoxCoordinates[0] := APoint;
        APoint.X := ZoomBox.Image32.Width;
        BoxCoordinates[1] := APoint;
        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;

      for LayerIndex := 0 to LayerCount do
      begin
        APoint.X := ZoomBox.XCoord(LayerElevations[LayerIndex]);
        APoint.Y := 0;
        BoxCoordinates[0] := APoint;
        APoint.Y := ZoomBox.Image32.Height;
        BoxCoordinates[1] := APoint;
        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;
    end
    else
    begin
      if SideDataSet <> nil then
      begin
        case SideDataSet.EvaluatedAt of
          eaBlocks:
            begin
              RowLimit := RowCount - 1;
              LayerLimit := LayerCount - 1;
            end;
          eaNodes:
            begin
              RowLimit := RowCount;
              LayerLimit := LayerCount;
            end;
        else
          begin
            Assert(False);
            RowLimit := -1;
            LayerLimit := -1;
          end;
        end;

        for RowIndex := 0 to RowLimit do
        begin
          if LayerCount > 0 then
          begin
            PriorLayer := 0;
            AColor := SideCellColors[RowIndex, 0];
            NewColor := AColor;
            for LayerIndex := 1 to LayerLimit do
            begin
              NewColor := SideCellColors[RowIndex, LayerIndex];
              if (NewColor <> AColor) { or (RowIndex = RowCount -1)} then
              begin
                if AColor <> clWhite then
                begin
                  case SideDataSet.EvaluatedAt of
                    eaBlocks:
                      begin
                        Point1 := ThreeDElementCorner(0, RowIndex, PriorLayer);
                        Point2 := ThreeDElementCorner(0, RowIndex + 1, PriorLayer);
                        Point3 := ThreeDElementCorner(0, RowIndex + 1, LayerIndex);
                        Point4 := ThreeDElementCorner(0, RowIndex, LayerIndex);
                      end;
                    eaNodes:
                      begin
                        Point1 := GetNodeBasedCorner(1, RowIndex, PriorLayer);
                        Point2 := GetNodeBasedCorner(1, RowIndex + 1,
                          PriorLayer);
                        Point3 := GetNodeBasedCorner(1, RowIndex + 1,
                          LayerIndex);
                        Point4 := GetNodeBasedCorner(1, RowIndex, LayerIndex);
                      end;
                  else
                    begin
                      Assert(False);
                    end;
                  end;
                  BoxCoordinates[0] := Point(ZoomBox.XCoord(Point1.Z),
                    ZoomBox.YCoord(Point1.Y));
                  BoxCoordinates[1] := Point(ZoomBox.XCoord(Point2.Z),
                    ZoomBox.YCoord(Point2.Y));
                  BoxCoordinates[2] := Point(ZoomBox.XCoord(Point3.Z),
                    ZoomBox.YCoord(Point3.Y));
                  BoxCoordinates[3] := Point(ZoomBox.XCoord(Point4.Z),
                    ZoomBox.YCoord(Point4.Y));

                  DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                    0, BoxCoordinates, P, MultiplePolygons, True);
                end;
                AColor := NewColor;
                PriorLayer := LayerIndex;
              end;
            end;

            if NewColor <> clWhite then
            begin
              case SideDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    Point1 := ThreeDElementCorner(0, RowIndex, PriorLayer);
                    Point2 := ThreeDElementCorner(0, RowIndex + 1, PriorLayer);
                    Point3 := ThreeDElementCorner(0, RowIndex + 1, LayerCount);
                    Point4 := ThreeDElementCorner(0, RowIndex, LayerCount);
                  end;
                eaNodes:
                  begin
                    Point1 := GetNodeBasedCorner(0, RowIndex, PriorLayer);
                    Point2 := GetNodeBasedCorner(0, RowIndex + 1, PriorLayer);
                    Point3 := GetNodeBasedCorner(0, RowIndex + 1, LayerCount +
                      1);
                    Point4 := GetNodeBasedCorner(0, RowIndex, LayerCount + 1);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              BoxCoordinates[0] := Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y));
              BoxCoordinates[1] := Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y));
              BoxCoordinates[2] := Point(ZoomBox.XCoord(Point3.Z), ZoomBox.YCoord(Point3.Y));
              BoxCoordinates[3] := Point(ZoomBox.XCoord(Point4.Z), ZoomBox.YCoord(Point4.Y));
              DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
                0, BoxCoordinates, P, MultiplePolygons, True);
            end;
          end;
        end;
      end;
      DrawOrdinarySideRows(ZoomBox, BitMap);
      DrawOrdinarySideLayers(BitMap, ZoomBox);
    end;
  end;
  DrawSideContours(ZoomBox, BitMap);
end;

function TPhastGrid.SideContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt;
  const BelowRow, AboveRow, BelowLayer, AboveLayer: integer): T2DSideCell;
begin
  result.Row := NearestRowPosition(APoint.X);
  result.Lay := NearestLayerPosition(APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        Exit;
      end;
  else
    Assert(False);
  end;

  if result.Row < 0 then
  begin
    result.Row := 0
  end;
  if result.Lay < 0 then
  begin
    result.Lay := 0;
  end;

//  if (result.Row > -1) and (result.Lay > -1) then
//  begin
    if (result.Row > 0) and (result.Row >= RowCount) then
    begin
      if EvaluatedAt = eaBlocks then
      begin
        result.Row := RowCount -1;
      end
      else
      begin
        Dec(result.Row);
      end;
    end;
    if (result.Lay > 0) and (result.Lay >= LayerCount) then
    begin
      if  EvaluatedAt = eaBlocks then
      begin
        result.Lay := LayerCount -1;
      end
      else
      begin
        Dec(result.Lay);
      end;
    end;
    if RowDirection = rdSouthToNorth then
    begin
      if (result.Row > 0) and (RowPosition[result.Row] > APoint.X) then
      begin
        Dec(result.Row);
      end;
    end
    else
    begin
      if (result.Row > 0) and (RowPosition[result.Row] < APoint.X) then
      begin
        Dec(result.Row);
      end;
    end;
    if LayerDirection = ldBottomToTop then
    begin
      if (result.Lay > 0) and (LayerElevation[result.Lay] > APoint.Y) then
      begin
        Dec(result.Lay);
      end;
    end
    else
    begin
      if (result.Lay > 0) and (LayerElevation[result.Lay] < APoint.Y) then
      begin
        Dec(result.Lay);
      end;
    end;
//
//  end;
end;

function TPhastGrid.GetContainingLayer(ACol, ARow: integer; const AZPosition: real): integer;
begin
  result := GetContainingColumnOrRow(FLayerElevations, AZPosition);
end;

function TPhastGrid.GetCell(const Location: TPoint2D;
  const ViewDirection: TViewDirection;
  const EvaluatedAt: TEvaluatedAt): T3DCell;
var
  TopCell: T2DTopCell;
  FrontCell: T2DFrontCell;
  SideCell: T2DSideCell;
begin
  case ViewDirection of
    vdTop:
      begin
        TopCell := TopContainingCell(Location, EvaluatedAt);
        result.Col := TopCell.Col;
        result.Row := TopCell.Row;
        result.Lay := 0
      end;
    vdFront:
      begin
        FrontCell := FrontContainingCell(Location, EvaluatedAt);
        result.Col := FrontCell.Col;
        result.Row := 0;
        result.Lay := FrontCell.Lay;
      end;
    vdSide:
      begin
        SideCell := SideContainingCell(Location, EvaluatedAt);
        result.Col := 0;
        result.Row := SideCell.Row;
        result.Lay := SideCell.Lay;
      end;
  else
    Assert(False);
  end;
  case EvaluatedAt of
    eaBlocks:
      begin
        if result.Col >= ColumnCount then
        begin
          Dec(result.Col);
        end;
        if result.Row >= RowCount then
        begin
          Dec(result.Row);
        end;
        if result.Lay >= LayerCount then
        begin
          Dec(result.Lay);
        end;

      end;
    eaNodes:
      begin
        if result.Col > ColumnCount then
        begin
          Dec(result.Col);
        end;
        if result.Row > RowCount then
        begin
          Dec(result.Row);
        end;
        if result.Lay > LayerCount then
        begin
          Dec(result.Lay);
        end;
      end;
    else
      Assert(False);
  end;
  if result.Col < 0 then
  begin
    Inc(result.Col);
  end;
  if result.Row < 0 then
  begin
    Inc(result.Row);
  end;
  if result.Lay < 0 then
  begin
    Inc(result.Lay);
  end;

end;

procedure TPhastGrid.Assign(Source: TPersistent);
var
  SourceGrid: TPhastGrid;
begin
  if Source is TPhastGrid then
  begin
    SourceGrid := TPhastGrid(Source);
    LayerElevations := SourceGrid.LayerElevations;
  end;
  inherited Assign(Source);
end;

procedure TPhastGrid.DrawBlockExtents(ViewDirection: TViewDirection;
  EvaluatedAt: TEvaluatedAt; Index: integer; out LowerBound, UpperBound: double);
begin
  case EvaluatedAt of
    eaBlocks: BlockExtents(ViewDirection,EvaluatedAt, Index,
      LowerBound, UpperBound);
    eaNodes:
      begin
        case ViewDirection of
          vdTop:
            begin
              if Index = 0 then
              begin
                LowerBound := LayerElevation[0] -
                  (LayerElevation[1] - LayerElevation[0])/2;
              end
              else
              begin
                LowerBound := (LayerElevation[Index-1]
                  + LayerElevation[Index])/2
              end;
              if Index = LayerCount then
              begin
                UpperBound := LayerElevation[Index]
                  + (LayerElevation[Index] - LayerElevation[Index-1])/2;
              end
              else
              begin
                UpperBound := (LayerElevation[Index]
                  + LayerElevation[Index+1])/2
              end;
            end;
          vdFront:
            begin
              if Index = 0 then
              begin
                LowerBound := RowPosition[0] -
                  (RowPosition[1]- RowPosition[0])/2;
              end
              else
              begin
                LowerBound := (RowPosition[Index-1]
                  + RowPosition[Index])/2
              end;
              if Index = RowCount then
              begin
                UpperBound := RowPosition[Index]
                  + (RowPosition[Index]-RowPosition[Index-1])/2;
              end
              else
              begin
                UpperBound := (RowPosition[Index]
                  + RowPosition[Index+1])/2
              end;
            end;
          vdSide:
            begin
              if Index = 0 then
              begin
                LowerBound := ColumnPosition[0]
                  - (ColumnPosition[1] - ColumnPosition[0])/2;
              end
              else
              begin
                LowerBound := (ColumnPosition[Index-1]
                  + ColumnPosition[Index])/2
              end;
              if Index = ColumnCount then
              begin
                UpperBound := ColumnPosition[Index]
                  + (ColumnPosition[Index] - ColumnPosition[Index-1])/2;
              end
              else
              begin
                UpperBound := (ColumnPosition[Index]
                  + ColumnPosition[Index+1])/2
              end;
            end;
          else Assert(False);
        end
     end;
  end;
end;

procedure TPhastGrid.DrawOrdinarySideRows(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TPersistent);
var
  RowIndex: Integer;
  LineWidth: Single;
  Point1: TPoint3D;
  Point2: TPoint3D;
  LocalEvalAt: TEvaluatedAt;
  LineColor: TColor32;
  Column: Integer;
  LayerIndex: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      or ((RowIndex > 0)
      and IsElementActive(LayerIndex,RowIndex-1, Column))
  end;
  function IsEdge: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      <> ((RowIndex > 0)
      and IsElementActive(LayerIndex,RowIndex-1, Column));
  end;
begin
  SetLocalEvalAt(vdSide, LocalEvalAt);
  for RowIndex := 0 to RowCount do
  begin
    if (RowIndex mod 10 = 0) or (RowIndex = RowCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetRowLineColor(RowIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll: 
        begin
          Point1 := ThreeDElementCorner(0, RowIndex, 0);
          Point2 := ThreeDElementCorner(0, RowIndex, LayerCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
            Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
        end;
      gldcExterior:
        begin
          if (RowIndex <> 0) and (RowIndex <> RowCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := ThreeDElementCorner(0, RowIndex, 0);
          Point2 := ThreeDElementCorner(0, RowIndex, LayerCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
            Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
        end;
      gldcActive:
        begin
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := ThreeDElementCorner(Column, RowIndex, LayerIndex);
              Point2 := ThreeDElementCorner(Column, RowIndex, LayerIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
                Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := ThreeDElementCorner(Column, RowIndex, LayerIndex);
              Point2 := ThreeDElementCorner(Column, RowIndex, LayerIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
                Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TPhastGrid.DrawOrdinarySideLayers(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  LayerIndex: Integer;
  LineWidth: Single;
  LineColor: TColor32;
  LocalEvalAt: TEvaluatedAt;
  Point1: TPoint3D;
  Point2: TPoint3D;
  Column: Integer;
  RowIndex: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      or ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,RowIndex, Column))
  end;
  function IsEdge: boolean;
  begin
    result :=  ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      <> ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,RowIndex, Column));
  end;
begin
  SetLocalEvalAt(vdSide, LocalEvalAt);
  for LayerIndex := 0 to LayerCount do
  begin
    if (LayerIndex mod 10 = 0) or (LayerIndex = LayerCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetLayerLineColor(LayerIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll: 
        begin
          Point1 := ThreeDElementCorner(0, 0, LayerIndex);
          Point2 := ThreeDElementCorner(0, RowCount, LayerIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
            Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
        end;
      gldcExterior:
        begin
          if (LayerIndex <> 0) and (LayerIndex <> LayerCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := ThreeDElementCorner(0, 0, LayerIndex);
          Point2 := ThreeDElementCorner(0, RowCount, LayerIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
            Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
        end;
      gldcActive:
        begin
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for RowIndex := 0 to RowCount-1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := ThreeDElementCorner(Column, RowIndex, LayerIndex);
              Point2 := ThreeDElementCorner(Column, RowIndex+1, LayerIndex);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
                Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for RowIndex := 0 to RowCount-1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := ThreeDElementCorner(Column, RowIndex, LayerIndex);
              Point2 := ThreeDElementCorner(Column, RowIndex+1, LayerIndex);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [Point(ZoomBox.XCoord(Point1.Z), ZoomBox.YCoord(Point1.Y)),
                Point(ZoomBox.XCoord(Point2.Z), ZoomBox.YCoord(Point2.Y))], True);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TPhastGrid.DrawOrdinaryFrontColumns(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  ColumnIndex: Integer;
  LineColor: TColor32;
  LocalEvalAt: TEvaluatedAt;
  Point1: TPoint3D;
  Point2: TPoint3D;
  LineWidth: Single;
  Row: Integer;
  LayerIndex: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((ColumnIndex < ColumnCount) and
      IsElementActive(LayerIndex,Row, ColumnIndex))
      or ((ColumnIndex > 0)
      and IsElementActive(LayerIndex,Row, ColumnIndex-1))
  end;
  function IsEdge: boolean;
  begin
    result := ((ColumnIndex < ColumnCount) and
      IsElementActive(LayerIndex,Row, ColumnIndex))
      <> ((ColumnIndex > 0)
      and IsElementActive(LayerIndex,Row, ColumnIndex-1));
  end;
begin
  SetLocalEvalAt(vdFront, LocalEvalAt);
  for ColumnIndex := 0 to ColumnCount do
  begin
    if (ColumnIndex mod 10 = 0) or (ColumnIndex = ColumnCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetColumnLineColor(ColumnIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll: 
        begin
          Point1 := ThreeDElementCorner(ColumnIndex, 0, 0);
          Point2 := ThreeDElementCorner(ColumnIndex, 0, LayerCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
            Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
        end;
      gldcExterior:
        begin
          if (GridLineDrawingChoice = gldcExterior)
            and (ColumnIndex <> 0) and (ColumnIndex <> ColumnCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := ThreeDElementCorner(ColumnIndex, 0, 0);
          Point2 := ThreeDElementCorner(ColumnIndex, 0, LayerCount);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
            Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
        end;
      gldcActive:
        begin
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := ThreeDElementCorner(ColumnIndex, 0, LayerIndex);
              Point2 := ThreeDElementCorner(ColumnIndex, 0, LayerIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
                Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := ThreeDElementCorner(ColumnIndex, 0, LayerIndex);
              Point2 := ThreeDElementCorner(ColumnIndex, 0, LayerIndex+1);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
                Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
            end;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TPhastGrid.DrawOrdinaryFrontLayers(const BitMap: TPersistent; const ZoomBox: TQRbwZoomBox2);
var
  LayerIndex: Integer;
  LocalEvalAt: TEvaluatedAt;
  LineWidth: Single;
  LineColor: TColor32;
  Point1: TPoint3D;
  Point2: TPoint3D;
  Row: Integer;
  ColIndex: Integer;
  LocalLineWidth: Extended;
  function IsActive: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      or ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,Row, ColIndex))
  end;
  function IsEdge: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      <> ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,Row, ColIndex))
  end;
begin
  SetLocalEvalAt(vdFront, LocalEvalAt);
  for LayerIndex := 0 to LayerCount do
  begin
    if (LayerIndex mod 10 = 0) or (LayerIndex = LayerCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetLayerLineColor(LayerIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          Point1 := ThreeDElementCorner(0, 0, LayerIndex);
          Point2 := ThreeDElementCorner(ColumnCount, 0, LayerIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
            Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
        end;
      gldcExterior:
        begin
          if (LayerIndex <> 0) and (LayerIndex <> LayerCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          Point1 := ThreeDElementCorner(0, 0, LayerIndex);
          Point2 := ThreeDElementCorner(ColumnCount, 0, LayerIndex);
          DrawBigPolyline32(BitMap, LineColor, LineWidth,
            [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
            Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
        end;
      gldcActive:
        begin
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for ColIndex := 0 to ColumnCount-1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              Point1 := ThreeDElementCorner(ColIndex, 0, LayerIndex);
              Point2 := ThreeDElementCorner(ColIndex+1, 0, LayerIndex);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth,
                [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
                Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for ColIndex := 0 to ColumnCount-1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              Point1 := ThreeDElementCorner(ColIndex, 0, LayerIndex);
              Point2 := ThreeDElementCorner(ColIndex+1, 0, LayerIndex);
              DrawBigPolyline32(BitMap, LineColor, LineWidth,
                [Point(ZoomBox.XCoord(Point1.X), ZoomBox.YCoord(Point1.Z)),
                Point(ZoomBox.XCoord(Point2.X), ZoomBox.YCoord(Point2.Z))], True);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TPhastGrid.BlockExtents(ViewDirection: TViewDirection;
  EvaluatedAt: TEvaluatedAt; Index: integer; out LowerBound, UpperBound: double);
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        case ViewDirection of
          vdTop:
            begin
              LowerBound := LayerElevation[Index];
              UpperBound := LayerElevation[Index+1];
            end;
          vdFront:
            begin
              LowerBound := RowPosition[Index];
              UpperBound := RowPosition[Index+1];
            end;
          vdSide:
            begin
              LowerBound := ColumnPosition[Index];
              UpperBound := ColumnPosition[Index+1];
            end;
          else Assert(False);
        end;
      end;
    eaNodes:
      begin
        case ViewDirection of
          vdTop:
            begin
              if Index = 0 then
              begin
                LowerBound := LayerElevation[Index];
              end
              else
              begin
                LowerBound := (LayerElevation[Index-1]
                  + LayerElevation[Index])/2
              end;
              if Index = LayerCount then
              begin
                UpperBound := LayerElevation[Index];
              end
              else
              begin
                UpperBound := (LayerElevation[Index]
                  + LayerElevation[Index+1])/2
              end;
            end;
          vdFront:
            begin
              if Index = 0 then
              begin
                LowerBound := RowPosition[Index];
              end
              else
              begin
                LowerBound := (RowPosition[Index-1]
                  + RowPosition[Index])/2
              end;
              if Index = RowCount then
              begin
                UpperBound := RowPosition[Index];
              end
              else
              begin
                UpperBound := (RowPosition[Index]
                  + RowPosition[Index+1])/2
              end;
            end;
          vdSide:
            begin
              if Index = 0 then
              begin
                LowerBound := ColumnPosition[Index];
              end
              else
              begin
                LowerBound := (ColumnPosition[Index-1]
                  + ColumnPosition[Index])/2
              end;
              if Index = ColumnCount then
              begin
                UpperBound := ColumnPosition[Index];
              end
              else
              begin
                UpperBound := (ColumnPosition[Index]
                  + ColumnPosition[Index+1])/2
              end;
            end;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
end;

procedure TPhastGrid.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('LayerElevations', ReadLayerElevations,
    WriteLayerElevations, True);
end;

procedure TPhastGrid.ReadLayerElevations(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions, LayerCount + 1);
  LayerElevations := Positions;
end;

procedure TPhastGrid.WriteLayerElevations(Writer: TWriter);
begin
  WriteRealArray(Writer, LayerElevations);
end;

procedure TPhastGrid.GetCellCornerElevations(const EvalAt: TEvaluatedAt;
  out Elevations: TThreeDRealArray);
var
  Elevations1D: TOneDRealArray;
  CCount, RCount, LCount: integer;
  ColIndex, RowIndex, LayerIndex: integer;
  Index: integer;
begin
  CCount := 0;
  RCount := 0;
  LCount := 0;
  case EvalAt of
    eaBlocks:
      begin
        CCount := ColumnCount + 1;
        RCount := RowCount + 1;
        LCount := LayerCount + 1;
        Elevations1D := FLayerElevations;
      end;
    eaNodes:
      begin
        CCount := ColumnCount + 2;
        RCount := RowCount + 2;
        LCount := LayerCount + 2;

        SetLength(Elevations1D, Length(FLayerElevations) + 1);
        Elevations1D[0] := FLayerElevations[0];
        for Index := 1 to Length(FLayerElevations) - 1 do
        begin
          Elevations1D[Index] := (FLayerElevations[Index]
            + FLayerElevations[Index - 1]) / 2;
        end;
        Elevations1D[Length(FLayerElevations)]
          := FLayerElevations[Length(FLayerElevations) - 1];
      end;
  else
    Assert(False);
  end;

  SetLength(Elevations, CCount, RCount, LCount);

  for ColIndex := 0 to CCount - 1 do
  begin
    for RowIndex := 0 to RCount - 1 do
    begin
      for LayerIndex := 0 to LCount - 1 do
      begin
        Elevations[ColIndex, RowIndex, LayerIndex] := Elevations1D[LayerIndex];
      end;
    end;
  end;

end;

function TPhastGrid.NearestLayerCenter(const APosition: real;
  First: integer; Last: integer): integer;
begin
  if First <> -1 then
  begin
    Dec(First);
  end;
  if Last <> -1 then
  begin
    Inc(Last);
  end;
  result := NearestLayerPosition(APosition, First, Last);
  if result > 0 then
  begin
    if (result > LayerCount) or (LayerElevation[result] > APosition) then
    begin
      Dec(result);
    end;
    if (result >= LayerCount) then
    begin
      result := LayerCount -1;
    end;
  end;
end;

function TPhastGrid.LayerCenter(const Layer: integer): real;
begin
  result := (LayerElevation[Layer] + LayerElevation[Layer + 1])/2
end;

function TPhastGrid.LowestElevation: real;
begin
  result := LayerElevations[0];
end;

end.
