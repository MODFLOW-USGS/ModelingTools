{@name is used to create a series of contour lines
based on a grid of data values or data values at arbitrary locations.
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit ContourUnit;

interface

uses Classes, Graphics, FastGeo, GR32, ZoomBox2, GoPhastTypes, DataSetUnit,
  ColorSchemes, QuadTreeClass, Generics.Collections, Types, TriPackRoutines,
  SparseDataSets, SparseArrayUnit, DrawMeshTypesUnit, MeshRenumberingTypes;

const
  DefaultLineThickness = 2;
  DefaultMajorLineThickness = 4;

type
  T2DGridPoint = record
    P: TPoint2D;
    Value: TFloat;
    Active: boolean;
  end;

  P2DGridPoint = ^T2DGridPoint;
  TGridSquare = record
    // Points to 4 grid points (square)
    GridPoint : Array [0..3] of P2DGridPoint;
  end;

  T2DGrid = array of array of T2DGridPoint;
  TSquares = array of array of TGridSquare;

  TTriangulationData = class(TObject)
  private
    FVertexNumbers: T3DSparseIntegerArray;
    function GetVertexNumber(Layer, Row, Col: Integer): Integer;
    procedure SetVertexNumber(Layer, Row, Col: Integer; const Value: Integer);
    procedure EliminateUniformTriangles;
  public
    Triangulation: TIntArray;
    X: TRealArray;
    Y: TRealArray;
    Values: TRealArray;
    IADJ: TIntArray;
    IEND: TIntArray;
    property VertexNumbers[Layer, Row, Col: Integer]: Integer
      read GetVertexNumber write SetVertexNumber;
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    destructor Destroy; override;
  end;

  TExtractSegmentEvent = procedure (Sender: TObject;
    const Segments: TLine2DArray) of Object;

  TLabel = class(TObject)
    X: integer;
    Y: integer;
    Value: string;
  end;

  TLabelObjectList = TObjectList<TLabel>;

  TContourCreator = class(TObject)
  private
    FBitMap: TPersistent;
    FColor: TColor32;
    FGrid: T2DGrid;
    FLineThickness: single;
    FValue: TFloat;
    FZoomBox: TQRbwZoomBox2;
    // See @link(OnExtractSegments).
    // During execution of DrawContour, @name is temporarily set to
    // @link(ConvertAndDrawSegments).
    FOnExtractSegments: TExtractSegmentEvent;
    FEvaluatedAt: TEvaluatedAt;
    FLabelLocations: TRbwQuadTree;
    FLabels: TLabelObjectList;
    FContourLabel: string;
    FLabelSpacing: Integer;
    FSegmentArrays: TList<TLine2DArray>;
    procedure ExtractSegments(const GridSquare: TGridSquare);
    procedure ConvertAndDrawSegments(Sender: TObject;
      const SegmentArray: TLine2DArray);
  public
    property BitMap: TPersistent read FBitMap write FBitMap;
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write FEvaluatedAt;
    property Color: TColor32 read FColor write FColor;
    property Grid: T2DGrid read FGrid write FGrid;
    property LineThickness: single read FLineThickness write FLineThickness;
    property Value: TFloat read FValue write FValue;
    property ContourLabel: string read FContourLabel write FContourLabel;
    property ZoomBox: TQRbwZoomBox2 read FZoomBox write FZoomBox;
    property LabelLocations: TRbwQuadTree read FLabelLocations write FLabelLocations;
    property Labels: TLabelObjectList read FLabels write FLabels;
    // While @link(ExtractContour) is being executed, @name is called
    // multiple times.  The contents of Segments will be one or more
    // segments in the contour being extracted. For SUTRA profile meshes,
    // the @name must convert from transformed coordinates to real coordinates
    // by dividing the Y coordinate of each point by the vertical exaggeration.
    // @name is set to @link(TTransientArrayImporter.ImportSegments) in
    // @link(Modflow2005ImporterUnit).
    // @name is set to @link(TContourCreator.ConvertAndDrawSegments) in
    // @link(TContourCreator.DrawContour).
    // @name is set to @link(TContourExtractor.ImportSegments) in
    // @link(TContourExtractor.CreateShapes).
    property OnExtractSegments: TExtractSegmentEvent read FOnExtractSegments
      write FOnExtractSegments;
    procedure DrawContour;
    procedure ExtractContour;
    constructor Create(ContourLabelSpacing: integer);
    destructor Destroy; override;
  end;

  TCustomContourCreator = class(TObject)
  private
    FActiveDataSet: TDataArray;
    FDataSet: TDataArray;
    FViewDirection: TViewDirection;
    FGrid: T2DGrid;
    FMesh: IDrawMesh;
    FTriangulationData: TTriangulationData;
    FMeshOutline: TObject;
    FSelectedLayer: Integer;
    Active: T3DBooleanDataSet;
    procedure EvaluateMinMaxLgr(out MaxValue, MinValue, MinPositive: Double;
      DSValues: TStringList;
      ViewDirection: TViewDirection);
    procedure EvaluateMinMaxMesh(out MaxValue, MinValue, MinPositive: Double;
      DSValues: TStringList;
      ViewDirection: TViewDirection);
    procedure SetMesh(const Value: IDrawMesh);
    procedure SetActiveDataSet(const Value: TDataArray);
    procedure SetDataSet(const Value: TDataArray);
    procedure SetGrid(const Value: T2DGrid);
    procedure SetViewDirection(const Value: TViewDirection);
    procedure IsElementActive (Sender: TObject; Item: ITriangulatable;
      var IsActive: Boolean);
    procedure IsNodeActive (Sender: TObject; Item: ITriangulatable;
      var IsActive: Boolean);
  protected
    // @name set Active for the selected column, row, or layer based
    // on @link(ActiveDataSet)
    procedure EvaluateActive(var Active: T3DBooleanDataSet;
      AnActiveDataSet: TDataArray);
    procedure EvaluateActiveMesh(var Active: T3DBooleanDataSet;
      ADataSet: TDataArray);
    {If @link(DataSet).DataType is rdtString, DSValues will contain a sorted
    list of the unigue values in @link(DataSet).  Otherwise,
    MaxValue and MinValue will be set to the maximum and minimum values in
    @link(DataSet) where Active is true.}
    procedure EvaluateMinMax(out MaxValue, MinValue, MinPositive: Double;
      DSValues: TStringList; Active: T3DBooleanDataSet; ADataArray: TDataArray;
      SelectedColRowLayer: Integer);
      // @name calls @link(EvaluateActive) and @link(EvaluateMinMax)
      // and then assigns values to @link(FGrid).
    procedure AssignGridValues(out MinValue, MaxValue, MinPositive: double;
      SelectedColRowLayer: integer; DSValues: TStringList;
      ViewDirection: TViewDirection);
    procedure AssignTriangulationValuesFromGrid(out MinValue, MaxValue, MinPositive: double;
      SelectedColRowLayer: integer; DSValues: TStringList;
      ViewDirection: TViewDirection);
    procedure AssignTriangulationValuesFromMesh(out MinValue, MaxValue, MinPositive: double;
      SelectedColRowLayer: integer; DSValues: TStringList;
      ViewDirection: TViewDirection);
    // C = CONTOUR LEVELS
    procedure PerformAlg626(C: TRealArray);
    procedure CreateSimpleContoursFromMesh(const ContourValues: TOneDRealArray);
    procedure Clear; virtual;
  public
    destructor Destroy; override;
    property ActiveDataSet: TDataArray read FActiveDataSet write SetActiveDataSet;
    property DataSet: TDataArray read FDataSet write SetDataSet;
    property ViewDirection: TViewDirection read FViewDirection
      write SetViewDirection;
    property Grid: T2DGrid read FGrid write SetGrid;
    property Mesh: IDrawMesh read FMesh write SetMesh;
  end;


  {@name is used to create contours for drawing. If @name is changed,
  consider changing @link(TContourExtractor) in @link(ContourExport)
  and @link(TfrmExportImage) too.}
  TMultipleContourCreator = class(TCustomContourCreator)
  private
    FBitMap: TPersistent;
    FZoomBox: TQRbwZoomBox2;
    FLabelLocations: TRbwQuadTree;
    FLabels: TLabelObjectList;
    FAlgorithm: TContourAlg;
    FLabelSpacing: Integer;
    // @name calls @link(TContourCreator.DrawContour) for each memeber of
    // ContourValues.
    procedure CreateAndDrawContours(const ContourValues,
      LineThicknesses: TOneDRealArray; const ContourColors: TArrayOfColor32;
      ContourLabels: TStringList);
    // @name initializes the @link(TDataArray)s and then calls
    // @link(AssignGridValues) and @link(CreateAndDrawContours).
    // @name is called if @link(TContours.SpecifyContours
    // DataSet.Contours.SpecifyContours) is true;
    procedure DrawContours(const ContourValues, LineThicknesses: TOneDRealArray;
      const ContourColors: TArrayOfColor32; SelectedColRowLayer: integer;
        ViewDirection: TViewDirection; ContourLabels: TStringList); overload;
    // @name updates MinValue and MaxValue base on limits
    // in @link(DataSet).ContourLimits.
    procedure GetSpecifiedMinMax(var MinValue, MaxValue: Double;
      DSValues: TStringList);
    // @name returns the values need to define the contour values.
    procedure GetContouringParameters(var RequiredSize: Integer;
      MinValue: Double; MaxValue: Double; var DesiredSpacing: Double;
      var SmallestContour: Double; var LargestContour: Double);
    // @name initializes ContourValues.
    procedure GetContourValues(LargestContour, SmallestContour: Double;
      RequiredSize: Integer; var ContourValues: TOneDRealArray);
    // @name initializes LineThicknesses and ContourColors.
    procedure GetContourColorsAndThicknesses(DesiredSpacing: Double;
      RequiredSize: Integer; var LineThicknesses: TOneDRealArray;
      var ContourColors: TArrayOfColor32; ContourValues: TOneDRealArray;
      ColorParameters: TColorParameters);
    procedure PlotContourLines(const ContourValues,
      LineThicknesses: TOneDRealArray; const ContourColors: TArrayOfColor32;
      ContourLabels: TStringList);
    procedure DrawContourLabels;
    procedure InitializeLabelData;
  protected
    procedure Clear; override;
  public
    Constructor Create;
    Destructor Destroy; override;
    property BitMap: TPersistent read FBitMap write FBitMap;
    property ZoomBox: TQRbwZoomBox2 read FZoomBox write FZoomBox;
    procedure DrawContours(SelectedColRowLayer: integer;
      ColorParameters: TColorParameters; ViewDirection: TViewDirection;
      LabelSpacing: integer; UpToDate: Boolean); overload;
  end;


implementation

uses Math, RbwParser, BigCanvasMethods, PhastModelUnit,
  SysUtils, frmGoPhastUnit, frmDisplayDataUnit,
  LineStorage, TriCP_Routines, RealListUnit,
  CalCompRoutines, SubPolygonUnit, SutraMeshUnit,
  RegularTriMeshUnit, TriangulateMeshUnit;

resourcestring
  StrErrorAttemptingTo = 'Error attempting to log transform %g';
  StrInvalidContourAlgo = 'Invalid contour algorithm';

function Interpolate(const C1, C2 : TPoint2D; Val1, Val2 : TFloat;
  ContourValue: TFloat) : TPoint2D;
var mu : TFloat;
begin
  if (Val1 = ContourValue) then
    Result := C1
  else
  if (Val2 = ContourValue) then
    Result := C2
  else
  begin
    mu := (ContourValue - Val1) / (Val2 - Val1);
    Result.x := C1.x + mu * (C2.x - C1.x);
    Result.y := C1.y + mu * (C2.y - C1.y);
  end;
end;

type TPointArray4 = array[0..3] of TPoint2D;

{ TContourCreator }

procedure PlotLabel(const CenterX, CenterY: Integer; const LabelLocations:
  TRbwQuadTree; const AContourLabel: string; const Labels: TLabelObjectList;
  BitMap: TPersistent; LabelSpacing: Integer);
var
  PointX: double;
  PointY: double;
  LabelObject: TLabel;
  Data: Pointer;
  ASize: TSize;
begin
  if (CenterX > 0) and (CenterY > 0)
    and (CenterX < LabelLocations.XMax)
    and (CenterY < LabelLocations.YMax) then
  begin
    PointX := CenterX;
    PointY := CenterY;

    if LabelLocations.Count > 0 then
    begin
      LabelLocations.FirstNearestPoint(PointX, PointY, Data);
    end;
    if (LabelLocations.Count = 0) or (Data <> nil) then
    begin
      if (LabelLocations.Count = 0)
        or (Distance(CenterX, CenterY, PointX, PointY) > LabelSpacing) then
      begin
        LabelLocations.AddPoint(CenterX, CenterY, Pointer(1));

        LabelObject := TLabel.Create;
        LabelObject.Value := AContourLabel;
        if Bitmap is TBitmap32 then
        begin
          ASize := TBitmap32(Bitmap).TextExtent(LabelObject.Value);
        end
        else
        begin
          (Bitmap as TCanvas).TextExtent(LabelObject.Value);
        end;
        LabelObject.X := CenterX - ASize.cx div 2;
        LabelObject.Y := CenterY - ASize.cy div 2;
        Labels.Add(LabelObject);
      end;
    end;
  end;
end;



procedure TContourCreator.ConvertAndDrawSegments(Sender: TObject;
  const SegmentArray: TLine2DArray);
const
  MaxLengthOfSegMentArray = 4;
var
  SegmentI: array[0..MaxLengthOfSegMentArray-1] of TPoint;
  Index: Integer;
  CenterX: Int64;
  CenterY: Int64;
  PositionIndex: Int64;
  Exaggeration: double;
begin
  Assert(Length(SegmentArray) <= MaxLengthOfSegMentArray);
  Exaggeration := 1.0;
  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    if frmGoPhast.SutraMesh.MeshType = mtProfile then
    begin
      Exaggeration := frmGoPhast.PhastModel.Exaggeration;
      if Exaggeration = 0 then
      begin
        Exaggeration := 1;
      end;
    end;
  end;
  Assert(Length(SegmentArray) <= MaxLengthOfSegMentArray);
  for Index := 0 to Length(SegmentArray) - 1 do
  begin
    PositionIndex := Index;
    SegmentI[PositionIndex * 2].X := ZoomBox.XCoord(SegmentArray[PositionIndex, 1].x);
    SegmentI[PositionIndex * 2].y := ZoomBox.YCoord(SegmentArray[PositionIndex, 1].y/Exaggeration);
    SegmentI[PositionIndex * 2 + 1].X := ZoomBox.XCoord(SegmentArray[PositionIndex, 2].x);
    SegmentI[PositionIndex * 2 + 1].y := ZoomBox.YCoord(SegmentArray[PositionIndex, 2].y/Exaggeration);
  end;
  for Index := 0 to Length(SegmentArray) - 1 do
  begin
    PositionIndex := Index;
    DrawBigPolyline32(Bitmap, Color, LineThickness,
      SegmentI, True, False, PositionIndex * 2, 2);
    if LabelLocations <> nil then
    begin
      Assert(Labels <> nil);
      CenterX := (SegmentI[PositionIndex * 2].X + SegmentI[PositionIndex * 2 + 1].X) div 2;
      CenterY := (SegmentI[PositionIndex * 2].Y + SegmentI[PositionIndex * 2 + 1].Y) div 2;
      if (CenterX > 0) and (CenterY > 0)
        and (CenterX < LabelLocations.XMax) and (CenterY < LabelLocations.YMax) then
      begin
        PlotLabel(CenterX, CenterY, LabelLocations, ContourLabel,
          Labels, BitMap, FLabelSpacing);
      end;
    end;
  end;
end;

constructor TContourCreator.Create(ContourLabelSpacing: integer);
begin
  FLabelSpacing := ContourLabelSpacing;
  FSegmentArrays := TList<TLine2DArray>.Create;
end;

destructor TContourCreator.Destroy;
begin
  FSegmentArrays.Free;
  inherited;
end;

procedure TContourCreator.DrawContour;
var
  Temp: TExtractSegmentEvent;
begin
  Assert(Assigned(BitMap));
  Assert(Assigned(ZoomBox));
  Temp := OnExtractSegments;
  try
    OnExtractSegments := ConvertAndDrawSegments;
    ExtractContour;
  finally
    OnExtractSegments := Temp;
  end;
end;

procedure TContourCreator.ExtractContour;
var
  cy: Integer;
  cx: Integer;
  Squares: TSquares;
  GridSize2: Integer;
  GridSize1: Integer;
begin
  Assert(Assigned(Grid));
  Assert(Assigned(OnExtractSegments));

  FSegmentArrays.Clear;
  GridSize1 := Length(Grid);
  if GridSize1 > 0 then
  begin
    GridSize2 := Length(Grid[0]);
    if GridSize2 > 0 then
    begin
      SetLength(Squares, GridSize1 - 1, GridSize2 - 1);
      for cx := 0 to GridSize1 - 2 do
      begin
        for cy := 0 to GridSize2 - 2 do
        begin
          Squares[cx, cy].GridPoint[0] := @Grid[cx, cy];
          Squares[cx, cy].GridPoint[1] := @Grid[cx + 1, cy];
          Squares[cx, cy].GridPoint[2] := @Grid[cx, cy + 1];
          Squares[cx, cy].GridPoint[3] := @Grid[cx + 1, cy + 1];
        end;
      end;
      case EvaluatedAt of
        eaBlocks:
          begin
            for cx := 0 to GridSize1 - 2 do
            begin
              for cy := 0 to GridSize2 - 2 do
              begin
                ExtractSegments(Squares[cx, cy]);
              end;
            end;
          end;
        eaNodes:
          begin
            for cx := 1 to GridSize1 - 3 do
            begin
              for cy := 1 to GridSize2 - 3 do
              begin
                ExtractSegments(Squares[cx, cy]);
              end;
            end;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TContourCreator.ExtractSegments(const GridSquare: TGridSquare);
var
  TemDist: Extended;
  Index: Integer;
  MinDistance: Extended;
  MinIndex: Integer;
  LineIndex: Integer;
  Segments: TPointArray4;
  Count: Integer;
  SegmentArray: TLine2DArray;
begin
  // Determine the index into the edge table which tells
  // us which vertices are inside/outside the metaballs
  LineIndex := 0;
  if GridSquare.GridPoint[0]^.Active
    and GridSquare.GridPoint[1]^.Active
    and GridSquare.GridPoint[2]^.Active
    and GridSquare.GridPoint[3]^.Active
    then
  begin
    if GridSquare.GridPoint[0]^.Value < Value then
      LineIndex := LineIndex or 1;
    if GridSquare.GridPoint[1]^.Value < Value then
      LineIndex := LineIndex or 2;
    if GridSquare.GridPoint[2]^.Value < Value then
      LineIndex := LineIndex or 4;
    if GridSquare.GridPoint[3]^.Value < Value then
      LineIndex := LineIndex or 8;
  end;
  Count := 0;
  case LineIndex of
    0, 15:
      begin
        Count := 0;
      end;
    1, 14:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[1].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[2].Value,
          value);
      end;
    2, 13:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[0].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    3, 12:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[2].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    4, 11:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].Value,
          GridSquare.GridPoint[0].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    5, 10:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[1].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[2].Value,
          value);
      end;
    6, 9:
      begin
        Count := 4;
        MinIndex := 0;
        MinDistance := Abs(Value - GridSquare.GridPoint[0].Value);
        for Index := 1 to 3 do
        begin
          TemDist := Abs(Value - GridSquare.GridPoint[Index].Value);
          if TemDist < MinDistance then
          begin
            MinDistance := TemDist;
            MinIndex := Index;
          end;
        end;
        case MinIndex of
          0, 3:
            begin
              Segments[0] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[0].Value,
                value);
              Segments[1] := Interpolate(
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[0].Value,
                GridSquare.GridPoint[1].Value,
                value);
              Segments[2] := Interpolate(
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[1].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[3] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[3].Value,
                value);
            end;
          1, 2:
            begin
              Segments[0] := Interpolate(
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[0].Value,
                GridSquare.GridPoint[1].Value,
                value);
              Segments[1] := Interpolate(
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[1].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[2] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[3] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[0].Value,
                value);
            end;
        else
          Assert(False);
        end;
      end;
    7, 8:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[2].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[1].Value,
          value);
      end;
  else
    begin
      Assert(False);
    end;
  end;
  if Count > 0 then
  begin
    SetLength(SegmentArray, Count div 2);
    for Index := 0 to (Count div 2) - 1 do
    begin
      SegmentArray[Index,1] := Segments[Index*2];
      SegmentArray[Index,2] := Segments[Index*2+1];
    end;
	  FSegmentArrays.Add(SegmentArray);
    OnExtractSegments(self, SegmentArray);
  end;
end;

{ TMultipleContourCreator }

procedure TMultipleContourCreator.DrawContours(const ContourValues,
  LineThicknesses: TOneDRealArray; const ContourColors: TArrayOfColor32;
  SelectedColRowLayer: integer; ViewDirection: TViewDirection;
  ContourLabels: TStringList);
var
  DSValues: TStringList;
  MinValue, MaxValue: double;
  MinPositive: Double;
begin

  case FAlgorithm of
    caSimple:
      begin
        if Assigned(Grid) then
        begin
          Assert(Assigned(ActiveDataSet));
        end;
      end;
    caACM626:
      begin
        if Assigned(Grid) then
        begin
          Assert(Assigned(ActiveDataSet));
        end
        else
        begin
          Assert(Assigned(Mesh));
        end;
      end
    else Assert(False, StrInvalidContourAlgo);
  end;


//  Assert(Assigned(ActiveDataSet));
  Assert(Assigned(DataSet));
  Assert(Assigned(BitMap));
//  Assert(Assigned(Grid));
  Assert(Assigned(ZoomBox));
  Assert(Length(ContourValues) = Length(ContourColors));
  Assert(Length(ContourValues) = Length(LineThicknesses));

  DataSet.Initialize;
  if Assigned(ActiveDataSet) then
  begin
    ActiveDataSet.Initialize;
  end;

  DSValues := TStringList.Create;
  try
    case FAlgorithm of
      caSimple:
        begin
          if Assigned(Grid) then
          begin
            AssignGridValues(MinValue, MaxValue, MinPositive, SelectedColRowLayer,
              DSValues, ViewDirection);
          end
          else
          begin
            AssignTriangulationValuesFromMesh(MinValue, MinPositive, MaxValue,
              SelectedColRowLayer, DSValues, ViewDirection);
          end;
        end;
      caACM626:
        begin
          if Assigned(Grid) then
          begin
            AssignTriangulationValuesFromGrid(MinValue, MaxValue, MinPositive,
              SelectedColRowLayer, DSValues, ViewDirection);
          end
          else
          begin
            AssignTriangulationValuesFromMesh(MinValue, MaxValue, MinPositive,
              SelectedColRowLayer, DSValues, ViewDirection);
          end;
        end;
      else Assert(False, StrInvalidContourAlgo);
    end;
  finally
    DSValues.Free;
  end;
  Assert(Length(ContourValues) = Length(LineThicknesses));
  Assert(Length(ContourValues) = Length(ContourColors));
  CreateAndDrawContours(ContourValues, LineThicknesses, ContourColors,
    ContourLabels);
end;

procedure TCustomContourCreator.AssignGridValues(out MinValue, MaxValue, MinPositive: double;
  SelectedColRowLayer: integer; DSValues: TStringList; ViewDirection: TViewDirection);
var
  Active: T3DBooleanDataSet;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  DSCol: Integer;
  DSRow: Integer;
  DSLayer: Integer;
  Value: Double;
  Position: Integer;
  Index: Integer;
  ColumnLimit, RowLimit, LayerLimit: integer;
begin
  EvaluateMinMaxLgr(MaxValue, MinValue, MinPositive, DSValues, ViewDirection);
  EvaluateActive(Active, ActiveDataSet);

  LayerLimit := -1;
  RowLimit := -1;
  ColumnLimit := -1;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        ColumnLimit := DataSet.ColumnCount;
        RowLimit := DataSet.RowCount;
        LayerLimit := DataSet.LayerCount;
      end;
    eaNodes:
      begin
        ColumnLimit := DataSet.ColumnCount+1;
        RowLimit := DataSet.RowCount+1;
        LayerLimit := DataSet.LayerCount+1;
      end;
    else Assert(False);
  end;
  case DataSet.Orientation of
    dsoTop: LayerLimit := 1;
    dsoFront: RowLimit := 1;
    dsoSide: ColumnLimit := 1;
    dso3D: ;  // do nothing
    else Assert(False);
  end;

  for ColIndex := 0 to ColumnLimit - 1 do
  begin
    if (ViewDirection = vdSide) and (ColIndex <> SelectedColRowLayer)
      and (DataSet.Orientation <> dsoSide) then
    begin
      Continue;
    end;
    for RowIndex := 0 to RowLimit - 1 do
    begin
      if (ViewDirection = vdFront) and (RowIndex <> SelectedColRowLayer)
        and (DataSet.Orientation <> dsoFront) then
      begin
        Continue;
      end;
      for LayerIndex := 0 to LayerLimit - 1 do
      begin
        if (ViewDirection = vdTop) and (LayerIndex <> SelectedColRowLayer)
          and (DataSet.Orientation <> dsoTop) then
        begin
          Continue;
        end;
        Column := -1;
        Row := -1;
        Layer := -1;
        case ViewDirection of
          vdTop:
            begin
              Column := ColIndex;
              Row := RowIndex;
              Layer := SelectedColRowLayer;
            end;
          vdFront:
            begin
              Column := ColIndex;
              Row := SelectedColRowLayer;
              Layer := LayerIndex;
            end;
          vdSide:
            begin
              Column := SelectedColRowLayer;
              Row := RowIndex;
              Layer := LayerIndex;
            end;
          else Assert(False);
        end;
        if Active[Column,Row,Layer]
          and DataSet.ContourGridValueOK(LayerIndex, RowIndex, ColIndex) then
        begin
          DSCol := Column;
          DSRow := Row;
          DSLayer := Layer;

          case DataSet.Orientation of
            dsoTop:
              begin
                DSLayer := 0;
              end;
            dsoFront:
              begin
                DSRow := 0;
              end;
            dsoSide:
              begin
                DSCol := 0;
              end;
            dso3D: ; // do nothing
            else Assert(False);
          end;

          Value := 0;
          case DataSet.DataType of
            rdtDouble:
              begin
                Value := DataSet.RealData[DSLayer,DSRow,DSCol];
                if DataSet.ContourLimits.LogTransform then
                begin
                  if Value > 0 then
                  begin
                    Value := Log10(Value);
                  end
                  else
                  begin
                    Assert(MinPositive > 0, Format(StrErrorAttemptingTo, [Value]));
                    Value := Log10(MinPositive/2);
                  end;
                end;
              end;
            rdtInteger:
              begin
                Value := DataSet.IntegerData[DSLayer,DSRow,DSCol];
              end;
            rdtBoolean:
              begin
                Value := Ord(DataSet.BooleanData[DSLayer,DSRow,DSCol]);
              end;
            rdtString:
              begin
                Value := DSValues.IndexOf(DataSet.StringData[DSLayer,DSRow,DSCol]);
              end;
            else Assert(False);
          end;

          case ViewDirection of
            vdTop:
              begin
                Grid[ColIndex+1,RowIndex+1].Value := Value;
                Grid[ColIndex+1,RowIndex+1].Active := True;
              end;
            vdFront:
              begin
                Grid[ColIndex+1,LayerIndex+1].Value := Value;
                Grid[ColIndex+1,LayerIndex+1].Active := True;
              end;
            vdSide:
              begin
                Grid[RowIndex+1,LayerIndex+1].Value := Value;
                Grid[RowIndex+1,LayerIndex+1].Active := True;
              end;
          end;

        end
        else
        begin
          case ViewDirection of
            vdTop:
              begin
                Grid[ColIndex+1,RowIndex+1].Active := False;
              end;
            vdFront:
              begin
                Grid[ColIndex+1,LayerIndex+1].Active := False;
              end;
            vdSide:
              begin
                Grid[RowIndex+1,LayerIndex+1].Active := False;
              end;
          end;
        end;
      end;
    end;
  end;

  Position := Length(Grid[0])-1;
  for Index := 1 to Length(Grid) - 2 do
  begin
    Grid[Index,0].Value := Grid[Index,1].Value;
    Grid[Index,0].Active := Grid[Index,1].Active;

    Grid[Index,Position].Value := Grid[Index,Position-1].Value;
    Grid[Index,Position].Active := Grid[Index,Position-1].Active;
  end;

  Position := Length(Grid)-1;
  for Index := 0 to Length(Grid[0]) - 1 do
  begin
    Grid[0,Index].Value := Grid[1,Index].Value;
    Grid[0,Index].Active := Grid[1,Index].Active;

    Grid[Position,Index].Value := Grid[Position-1,Index].Value;
    Grid[Position,Index].Active := Grid[Position-1,Index].Active;
  end;
end;

destructor TMultipleContourCreator.Destroy;
begin
  inherited;
  FLabels.Free;
  FLabelLocations.Free;
end;

procedure TMultipleContourCreator.DrawContours(SelectedColRowLayer: integer;
  ColorParameters: TColorParameters; ViewDirection: TViewDirection;
  LabelSpacing: integer; UpToDate: Boolean);
var
  DSValues: TStringList;
  ContourValues, NewContourValues: TOneDRealArray;
  LineThicknesses, NewLineThicknesses: TOneDRealArray;
  ContourColors, NewContourColors: TArrayOfColor32;
  MaxValue: double;
  MinValue: double;
  Index: Integer;
  Contours: TContours;
  DesiredSpacing: Double;
  SmallestContour: double;
  LargestContour: double;
  RequiredSize: integer;
  ContourLabels: TStringList;
  ContourIndex: Integer;
  DupLabels: TStringList;
  ALabel, NextLabel: string;
  LabelStart: Integer;
  LabelEnd: Integer;
  MidLabelPostion: Integer;
  MinPositive: Double;
//  LocalMesh: TSutraMesh3D;
begin
  Assert(Assigned(DataSet));
  FLabelSpacing := LabelSpacing;
  FAlgorithm := DataSet.ContourAlg;
  if not DataSet.UpToDate then
  begin
    UpToDate := False;
  end;
  DataSet.Initialize;

  Assert(Assigned(BitMap));

  if Assigned(Grid) then
  begin
    Assert(Assigned(ActiveDataSet));
    FMeshOutline := nil;
    CalCompRoutines.MeshOutline := nil;
  end
  else
  begin
    Assert(Assigned(Mesh));
    case ViewDirection of
      vdTop: FMeshOutline := Mesh.TopOutline(SelectedColRowLayer);
      vdFront: FMeshOutline := Mesh.FrontOutline;
      else Assert(False);
    end;
  end;

  Assert(Assigned(ZoomBox));

  if frmGoPhast.PhastModel.ShowContourLabels then
  begin
    if BitMap is TBitmap32 then
    begin
      TBitmap32(BitMap).Font := frmGoPhast.PhastModel.ContourFont;
    end
    else
    begin
      (BitMap as TCanvas).Font := frmGoPhast.PhastModel.ContourFont;
    end;
  end;

  try
    Contours :=  DataSet.Contours;
    if (Contours <> nil) and Contours.SpecifyContours then
    begin
      if Contours.AutomaticColors and (Length(Contours.ContourValues) > 0) then
      begin
        MinValue := Contours.ContourValues[0];
        MaxValue := Contours.ContourValues[Length(Contours.ContourValues) -1];
        if MaxValue > MinValue then
        begin
          for Index := 0 to Length(Contours.ContourValues) - 2 do
          begin
            Contours.ContourColors[Index] :=
              Color32(ColorParameters.FracToColor(
              (MaxValue - Contours.ContourValues[Index])/(MaxValue - MinValue)));
          end;
          Contours.ContourColors[Length(Contours.ContourValues) - 1] :=
            Color32(ColorParameters.FracToColor(0));
        end
        else
        begin
          Contours.Count := 0;
        end;
      end;
      // Make copies of arrays so they don't get
      // altered in DrawContours.
      // Even though they are passed as const variables,
      // ContourColors can be altered in DataSet.Initialize.
      ContourValues := Contours.ContourValues;
      LineThicknesses := Contours.LineThicknesses;
      ContourColors := Contours.ContourColors;
      ContourLabels := Contours.ContourStringValues;

      if UpToDate then
      begin
        InitializeLabelData;
        PlotContourLines(ContourValues, LineThicknesses, ContourColors,
          ContourLabels);
        DrawContourLabels;
        Exit;
      end;

      DrawContours(ContourValues, LineThicknesses,
        ContourColors, SelectedColRowLayer, ViewDirection, ContourLabels);
    end
    else
    begin
      DataSet.Initialize;
      if Assigned(ActiveDataSet) then
      begin
        ActiveDataSet.Initialize;
      end;

      DSValues := TStringList.Create;
      ContourLabels := TStringList.Create;
      try

        case FAlgorithm of
          caSimple:
            begin

              if Assigned(Grid) then
              begin
                AssignGridValues(MinValue, MaxValue, MinPositive, SelectedColRowLayer,
                  DSValues, ViewDirection);
              end
              else
              begin
                if UpToDate then
                begin
                  EvaluateMinMaxMesh(MaxValue, MinValue, MinPositive, DSValues, ViewDirection);
//                  AssignTriangulationValuesFromMesh(MinValue, MaxValue,
//                    SelectedColRowLayer, DSValues, ViewDirection);
                end
                else
                begin
                  AssignTriangulationValuesFromMesh(MinValue, MaxValue, MinPositive,
                    SelectedColRowLayer, DSValues, ViewDirection);
                end;
              end;
            end;
          caACM626:
            begin
              if Assigned(Grid) then
              begin
                AssignTriangulationValuesFromGrid(MinValue, MaxValue, MinPositive,
                  SelectedColRowLayer, DSValues, ViewDirection);
              end
              else
              begin
                if UpToDate then
                begin
                  EvaluateMinMaxMesh(MaxValue, MinValue, MinPositive, DSValues, ViewDirection);
//                  AssignTriangulationValuesFromMesh(MinValue, MaxValue,
//                    SelectedColRowLayer, DSValues, ViewDirection);
                end
                else
                begin
                  AssignTriangulationValuesFromMesh(MinValue, MaxValue, MinPositive,
                    SelectedColRowLayer, DSValues, ViewDirection);
                end;
              end;
            end;
          else Assert(False, StrInvalidContourAlgo);
        end;

        GetSpecifiedMinMax(MinValue, MaxValue, DSValues);
//        if Assigned(Grid) then
//        begin
//          Grid.app
//        end
//        else
//        begin
//        end;


        if MaxValue > MinValue then
        begin
          if DataSet.DataType = rdtBoolean then
          begin
            SetLength(ContourValues, 1);
            SetLength(ContourColors, 1);
            SetLength(LineThicknesses, 1);
            ContourValues[0] := 0.5;
            LineThicknesses[0] := DefaultLineThickness;
            ContourColors[0] := Color32(ColorParameters.FracToColor(0.5));
          end
          else
          begin
            GetContouringParameters(RequiredSize, MinValue, MaxValue,
              DesiredSpacing, SmallestContour, LargestContour);
            GetContourValues(LargestContour, SmallestContour, RequiredSize,
              ContourValues);
            GetContourColorsAndThicknesses(DesiredSpacing, RequiredSize,
              LineThicknesses, ContourColors, ContourValues, ColorParameters);
            if DataSet.DataType = rdtString then
            begin
              DupLabels := TStringList.Create;
              try
                DupLabels.Duplicates := dupIgnore;
                DupLabels.Sorted := True;
                for ContourIndex := 0 to Length(ContourValues) - 1 do
                begin
                  ALabel := DSValues[Round(ContourValues[ContourIndex])];
                  ContourLabels.Add(ALabel);
                  DupLabels.Add(ALabel);
                end;
                if DupLabels.Count <> ContourLabels.Count then
                begin
                  SetLength(NewLineThicknesses, DupLabels.Count);
                  SetLength(NewContourColors, DupLabels.Count);
                  SetLength(NewContourValues, DupLabels.Count);
                  for Index := 0 to DupLabels.Count - 1 do
                  begin
                    ALabel := DupLabels[Index];
                    LabelStart := ContourLabels.IndexOf(ALabel);
                    if Index < DupLabels.Count - 1 then
                    begin
                      NextLabel := DupLabels[Index+1];
                      LabelEnd := ContourLabels.IndexOf(NextLabel)-1;
                    end
                    else
                    begin
                      LabelEnd := ContourLabels.Count - 1;
                    end;
                    MidLabelPostion := (LabelStart + LabelEnd) div 2;
                    NewLineThicknesses[Index] := LineThicknesses[MidLabelPostion];
                    NewContourColors[Index] := ContourColors[MidLabelPostion];
                    NewContourValues[Index] := ContourValues[MidLabelPostion];
                  end;
                  LineThicknesses := NewLineThicknesses;
                  ContourValues := NewContourValues;
                  ContourColors := NewContourColors;
                  ContourLabels.Assign(DupLabels);
                end;
              finally
                DupLabels.Free;
              end;

            end;
          end;
        end
        else
        begin
          SetLength(LineThicknesses,0);
          SetLength(ContourColors,0);
          SetLength(ContourValues,0);
        end;

        if UpToDate then
        begin
          InitializeLabelData;
          PlotContourLines(ContourValues, LineThicknesses, ContourColors,
            ContourLabels);
          DrawContourLabels;
          Exit;
        end;

        CreateAndDrawContours(ContourValues, LineThicknesses, ContourColors,
          ContourLabels);
        Contours := TContours.Create;
        try
          Assert(Length(ContourValues) = Length(LineThicknesses));
          Assert(Length(ContourValues) = Length(ContourColors));
          Contours.ContourValues := ContourValues;
          Contours.LineThicknesses := LineThicknesses;
          Contours.ContourColors := ContourColors;
          Contours.ContourStringValues := DSValues;
          DataSet.Contours := Contours;
        finally
          Contours.Free;
        end;
      finally
        DSValues.Free;
        ContourLabels.Free;
      end;
    end;
  finally
    if frmDisplayData <> nil then
    begin
      frmDisplayData.frameContourData.UpdateContours;
    end;

  end;
end;

procedure TMultipleContourCreator.DrawContourLabels;
var
  LabelIndex: Integer;
  ALabel: TLabel;
  ASize: TSize;
  ACanvas: TCanvas;
begin
  for LabelIndex := 0 to FLabels.Count - 1 do
  begin
    ALabel := FLabels[LabelIndex];
    if (FBitMap is TBitmap32) then
    begin
      ASize := TBitmap32(FBitMap).TextExtent(ALabel.Value);
      TBitmap32(FBitMap).FillRectS(ALabel.X, ALabel.Y, ALabel.X + ASize.cx, ALabel.Y + ASize.cy, clWhite32);
      TBitmap32(FBitMap).Textout(ALabel.X, ALabel.Y, ALabel.Value);
    end
    else
    begin
      ACanvas := FBitMap as TCanvas;
      ASize := ACanvas.TextExtent(ALabel.Value);
      ACanvas.Brush.Color := clWhite;
      ACanvas.FillRect(Rect(ALabel.X, ALabel.Y, ALabel.X + ASize.cx, ALabel.Y + ASize.cy));
      ACanvas.Textout(ALabel.X, ALabel.Y, ALabel.Value);
    end;
  end;
end;

procedure TMultipleContourCreator.GetContourColorsAndThicknesses(
  DesiredSpacing: Double; RequiredSize: Integer;
  var LineThicknesses: TOneDRealArray; var ContourColors: TArrayOfColor32;
  ContourValues: TOneDRealArray; ColorParameters: TColorParameters);
var
  ContourIndicator: Double;
  Index: Integer;
begin
  SetLength(ContourColors, RequiredSize);
  SetLength(LineThicknesses, RequiredSize);
  if RequiredSize = 0 then
  begin
    Exit;
  end;
  for Index := 0 to Length(ContourValues) - 2 do
  begin
    ContourIndicator := ContourValues[Index] / DesiredSpacing / 5;
    if Abs(Round(ContourIndicator) - ContourIndicator) < 0.01 then
    begin
      LineThicknesses[Index] := DefaultMajorLineThickness;
    end
    else
    begin
      LineThicknesses[Index] := DefaultLineThickness;
    end;
    ContourColors[Index] := Color32(ColorParameters.FracToColor(1 - (Index / (Length(ContourValues) - 1))));
  end;
  ContourIndicator := ContourValues[Length(ContourValues) - 1] / DesiredSpacing / 5;
  if Abs(Round(ContourIndicator) - ContourIndicator) < 0.01 then
  begin
    LineThicknesses[Length(ContourValues) - 1] := DefaultMajorLineThickness;
  end
  else
  begin
    LineThicknesses[Length(ContourValues) - 1] := DefaultLineThickness;
  end;
  ContourColors[Length(ContourValues) - 1] := Color32(ColorParameters.FracToColor(0));
end;

procedure TMultipleContourCreator.GetContourValues(LargestContour,
  SmallestContour: Double; RequiredSize: Integer;
  var ContourValues: TOneDRealArray);
var
  Index: Integer;
begin
  SetLength(ContourValues, RequiredSize);
  for Index := 0 to Length(ContourValues) - 2 do
  begin
    ContourValues[Index] := SmallestContour
      + Index * (LargestContour - SmallestContour)
      / (Length(ContourValues) - 1);
  end;
  if RequiredSize > 0 then
  begin
    ContourValues[Length(ContourValues) - 1] := LargestContour;
  end;
end;

procedure TMultipleContourCreator.GetContouringParameters(
  var RequiredSize: Integer; MinValue: Double; MaxValue: Double;
  var DesiredSpacing: Double; var SmallestContour: Double;
  var LargestContour: Double);
var
  UsedMin: Double;
  UsedMax: Double;
  ErrorEncountered: Boolean;
begin
  if DataSet.ContourLimits.LogTransform then
  begin
    UsedMin := Log10(MinValue);
    UsedMax := Log10(MaxValue);
  end
  else
  begin
    UsedMin := MinValue;
    UsedMax := MaxValue;
  end;
  if DataSet.ContourInterval.Value = 0 then
  begin
    DesiredSpacing := (UsedMax - UsedMin) / 20;
    DesiredSpacing := Power(10, Trunc(Log10(DesiredSpacing)));
  end
  else
  begin
    DesiredSpacing := Abs(DataSet.ContourInterval.Value);
  end;
  repeat
    ErrorEncountered := False;
    SmallestContour := Round(UsedMin / DesiredSpacing) * DesiredSpacing;
    while (SmallestContour > UsedMin) do
    begin
      SmallestContour := SmallestContour - DesiredSpacing;
    end;
    while (SmallestContour < UsedMin) do
    begin
      SmallestContour := SmallestContour + DesiredSpacing;
    end;
    LargestContour := Round(UsedMax / DesiredSpacing) * DesiredSpacing;
    while (LargestContour < UsedMax) do
    begin
      LargestContour := LargestContour + DesiredSpacing;
    end;
    while (LargestContour > UsedMax) do
    begin
      LargestContour := LargestContour - DesiredSpacing;
    end;
    try
      RequiredSize := Round((LargestContour - SmallestContour) / DesiredSpacing) + 1;
    except on ERangeError do
      begin
        ErrorEncountered := True;
        DesiredSpacing := DesiredSpacing *10;
      end;
    end;
  until not ErrorEncountered;
end;

procedure TMultipleContourCreator.GetSpecifiedMinMax(var MinValue,
  MaxValue: Double; DSValues: TStringList);
var
  StringValue: string;
  StringPos: integer;
begin
  case DataSet.DataType of
    rdtDouble:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          MaxValue := DataSet.ContourLimits.UpperLimit.RealLimitValue;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          MinValue := DataSet.ContourLimits.LowerLimit.RealLimitValue;
        end;
      end;
    rdtInteger:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          MaxValue := DataSet.ContourLimits.UpperLimit.IntegerLimitValue;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          MinValue := DataSet.ContourLimits.LowerLimit.IntegerLimitValue;
        end;
      end;
    rdtString:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          StringValue := DataSet.ContourLimits.UpperLimit.StringLimitValue;
          if not DSValues.Find(StringValue, StringPos) then
          begin
            Dec(StringPos)
          end;
          MaxValue := StringPos
//          MaxValue := DSValues.IndexOf(StringValue);
//          if MaxValue < 0 then
//          begin
//            Position := DSValues.Add(StringValue);
//            MaxValue := Position - 0.5;
//            DSValues.Delete(Position);
//          end;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          StringValue := DataSet.ContourLimits.LowerLimit.StringLimitValue;
          DSValues.Find(StringValue, StringPos);
          MinValue := StringPos
//          MinValue := DSValues.IndexOf(StringValue);
//          if MinValue < 0 then
//          begin
//            Position := DSValues.Add(StringValue);
//            MinValue := Position + 0.5;
//            DSValues.Delete(Position);
//          end;
        end;
      end;
    rdtBoolean:
      begin
        MinValue := 0;
        MaxValue := 1;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomContourCreator.EvaluateMinMaxLgr(out MaxValue, MinValue, MinPositive: Double;
  DSValues: TStringList;
  ViewDirection: TViewDirection);
var
  LocalPhastModel: TPhastModel;
  Active: T3DBooleanDataSet;
  AnActiveDataSet: TDataArray;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildMaxValue: Double;
  ChildMinValue: Double;
  ChildMinPositive: Double;
begin
  if DataSet.Model is TPhastModel then
  begin
    LocalPhastModel := TPhastModel(DataSet.Model);
  end
  else
  begin
    LocalPhastModel := (DataSet.Model as TChildModel).ParentModel as TPhastModel;
  end;

  AnActiveDataSet := LocalPhastModel.DataArrayManager.GetDataSetByName(rsActive);
  EvaluateActive(Active, AnActiveDataSet);
  case ViewDirection of
    vdTop:
      begin
        EvaluateMinMax(MaxValue, MinValue, MinPositive, DSValues, Active,
          LocalPhastModel.Grid.TopContourDataSet,
          LocalPhastModel.Grid.SelectedLayer);
      end;
    vdFront:
      begin
        EvaluateMinMax(MaxValue, MinValue, MinPositive, DSValues, Active,
          LocalPhastModel.Grid.FrontContourDataSet,
          LocalPhastModel.Grid.SelectedRow);
      end;
    vdSide:
      begin
        EvaluateMinMax(MaxValue, MinValue, MinPositive, DSValues, Active,
          LocalPhastModel.Grid.SideContourDataSet,
          LocalPhastModel.Grid.SelectedColumn);
      end;
    else
      Assert(False);
  end;

  if LocalPhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        AnActiveDataSet := ChildModel.DataArrayManager.GetDataSetByName(rsActive);
        EvaluateActive(Active, AnActiveDataSet);
        case ViewDirection of
          vdTop:
            begin
              if ChildModel.Grid.TopContourDataSet = nil then
              begin
                Continue;
              end;
              EvaluateMinMax(ChildMaxValue, ChildMinValue, ChildMinPositive, DSValues, Active,
                ChildModel.Grid.TopContourDataSet,
                ChildModel.Grid.SelectedLayer);
            end;
          vdFront:
            begin
              if ChildModel.Grid.FrontContourDataSet = nil then
              begin
                Continue;
              end;
              EvaluateMinMax(ChildMaxValue, ChildMinValue, ChildMinPositive, DSValues, Active,
                ChildModel.Grid.FrontContourDataSet,
                ChildModel.Grid.SelectedRow);
            end;
          vdSide:
            begin
              if ChildModel.Grid.SideContourDataSet = nil then
              begin
                Continue;
              end;
              EvaluateMinMax(ChildMaxValue, ChildMinValue, ChildMinPositive, DSValues, Active,
                ChildModel.Grid.SideContourDataSet,
                ChildModel.Grid.SelectedColumn);
            end;
          else
            Assert(False);
        end;
        if MinValue > ChildMinValue then
        begin
          MinValue := ChildMinValue;
        end;
        if MaxValue < ChildMaxValue then
        begin
          MaxValue := ChildMaxValue;
        end;
        if MinPositive = 0 then
        begin
          MinPositive := ChildMinPositive;
        end
        else if ChildMinPositive < MinPositive then
        begin
          MinPositive := ChildMinPositive;
        end;
      end;
    end;
  end;
end;

procedure TCustomContourCreator.EvaluateMinMaxMesh(out MaxValue,
  MinValue, MinPositive: Double; DSValues: TStringList; ViewDirection: TViewDirection);
var
  Active: T3DBooleanDataSet;
begin
  EvaluateActiveMesh(Active, DataSet);
  EvaluateMinMax(MaxValue, MinValue, MinPositive, DSValues, Active,
    DataSet, Mesh.SelectedLayer);
end;

procedure TCustomContourCreator.IsElementActive(Sender: TObject;
  Item: ITriangulatable; var IsActive: Boolean);
var
  Element2D: IElement2D;
  ColIndex: Integer;
begin
  Element2D := Item as IElement2D;
  ColIndex := Element2D.ElementNumber;
  IsActive := Active[ColIndex,0,FSelectedLayer]
end;

procedure TCustomContourCreator.IsNodeActive(Sender: TObject;
  Item: ITriangulatable; var IsActive: Boolean);
var
  Node2D: INode2D;
  ColIndex: Integer;
begin
  Node2D := Item as INode2D;
  ColIndex := Node2D.NodeNumber;
  IsActive := Active[ColIndex,0,FSelectedLayer]
end;

procedure TCustomContourCreator.PerformAlg626(C: TRealArray);
var
  ND: Integer;
  WK: TRealArray;
  NC: Integer;
  IPL: TIntArray;
  NewLength: Integer;
  NT: Integer;
//  LocalMesh: TSutraMesh3D;
begin
  Assert(FTriangulationData <> nil);
  PlotList.Clear;
  CurrentLineList := nil;
  CurrentLine := nil;
  if Length(FTriangulationData.Triangulation) = 0 then
  begin
    Exit;
  end;
  ND := Length(FTriangulationData.X);
  SetLength(WK, ND*5);
  NC := Length(C);
  SetLength(IPL, 6*ND);
  NT:= Length(FTriangulationData.Triangulation) div 3;
  NewLength := ND*6-15;
  Assert(NewLength >= Length(FTriangulationData.Triangulation));
  SetLength(FTriangulationData.Triangulation, NewLength);
  if Assigned(Mesh) then
  begin
    if FMeshOutline = nil then
    begin
//      LocalMesh := Mesh as TSutraMesh3D;
      case ViewDirection of
        vdTop: FMeshOutline := Mesh.TopOutline(Mesh.SelectedLayer);
        vdFront: FMeshOutline := Mesh.FrontOutline;
        else Assert(False);
      end;
    end;
    CalCompRoutines.MeshOutline := FMeshOutline as TOutline;
  end
  else
  begin
    CalCompRoutines.MeshOutline := nil;
  end;
  if (Mesh <> nil)
    and Mesh.IsFishnetMesh then
  begin
    // TRMESH doesn't get a good triangulation for fishnet meshes.
    TRICP_Pascal(FTriangulationData.X, FTriangulationData.Y,
      FTriangulationData.Values, C, WK, ND, NCP, NC,
      0, // NORMAL MODE
      NT, FTriangulationData.Triangulation, IPL);
  end
  else
  begin
    TRICP_Pascal(FTriangulationData.X, FTriangulationData.Y,
      FTriangulationData.Values, C, WK, ND, NCP, NC,
      1, // NO TRIANGULATION REQUESTED.
      NT, FTriangulationData.Triangulation, IPL);
  end;
end;

procedure TCustomContourCreator.SetActiveDataSet(const Value: TDataArray);
begin
  if Value <> FActiveDataSet then
  begin
    FActiveDataSet := Value;
	  Clear;
  end;
end;

procedure TCustomContourCreator.SetDataSet(const Value: TDataArray);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    Clear;
  end;
end;

procedure TCustomContourCreator.SetGrid(const Value: T2DGrid);
begin
  if FGrid <> Value then
  begin
    FGrid := Value;
    Clear;
  end;
end;

procedure TCustomContourCreator.SetMesh(const Value: IDrawMesh);
begin
//  if Value <> nil then
//  begin
//    Assert(Value is TSutraMesh3D);
//  end;
  if FMesh <> Value then
  begin
    FMesh := Value;
    Clear;
  end;
end;

procedure TCustomContourCreator.SetViewDirection(const Value: TViewDirection);
begin
  FViewDirection := Value;
end;

procedure TCustomContourCreator.EvaluateMinMax(out MaxValue, MinValue, MinPositive: Double;
  DSValues: TStringList; Active: T3DBooleanDataSet; ADataArray: TDataArray;
  SelectedColRowLayer: Integer);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FoundFirst: Boolean;
  ActiveColumn, ActiveRow, ActiveLayer: integer;
  AValue: Double;
begin
  ADataArray.Initialize;
  DSValues.Sorted := True;
  DSValues.Duplicates := dupIgnore;
  MinValue := 0;
  MaxValue := 0;
  MinPositive := 0;
  FoundFirst := false;
  for ColIndex := 0 to ADataArray.ColumnCount - 1 do
  begin
    if ADataArray.Orientation = dsoSide then
    begin
      ActiveColumn := SelectedColRowLayer;
    end
    else
    begin
      ActiveColumn := ColIndex;
    end;
    for RowIndex := 0 to ADataArray.RowCount - 1 do
    begin
      if ADataArray.Orientation = dsoFront then
      begin
        ActiveRow := SelectedColRowLayer;
      end
      else
      begin
        ActiveRow := RowIndex;
      end;
      for LayerIndex := 0 to ADataArray.LayerCount - 1 do
      begin
        if ADataArray.Orientation = dsoTop then
        begin
//          ActiveLayer := SelectedColRowLayer;
          ActiveLayer := 0;
        end
        else
        begin
          ActiveLayer := LayerIndex;
        end;
        if Active[ActiveColumn, ActiveRow, ActiveLayer]
          and ADataArray.ContourGridValueOK(LayerIndex, RowIndex, ColIndex) then
        begin
          if ADataArray.DataType = rdtString then
          begin
            FoundFirst := True;
            DSValues.Add(ADataArray.StringData[LayerIndex, RowIndex, ColIndex]);
          end
          else if not FoundFirst then
          begin
            case ADataArray.DataType of
              rdtDouble:
                begin
                  MinValue := ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                  MaxValue := MinValue;
                  FoundFirst := True;
                  if MinValue > 0 then
                  begin
                    MinPositive := MinValue;
                  end;
                end;
              rdtInteger:
                begin
                  MinValue := ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
                  MaxValue := MinValue;
                  FoundFirst := True;
                  if MinValue > 0 then
                  begin
                    MinPositive := MinValue;
                  end;
                end;
              rdtBoolean:
                begin
                  MinValue := Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]);
                  MaxValue := MinValue;
                  FoundFirst := True;
                  if MinValue > 0 then
                  begin
                    MinPositive := MinValue;
                  end;
                end;
            else
              Assert(False);
            end;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                begin
                  AValue := ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                  if MinValue > AValue then
                  begin
                    MinValue := AValue;
                  end
                  else if MaxValue < AValue then
                  begin
                    MaxValue := AValue;
                  end;
                  if AValue > 0 then
                  begin
                    if MinPositive = 0 then
                    begin
                      MinPositive := AValue;
                    end
                    else if AValue < MinPositive then
                    begin
                      MinPositive := AValue;
                    end;
                  end;
                end;
              rdtInteger:
                begin
                  AValue := ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
                  if MinValue > AValue then
                  begin
                    MinValue := AValue;
                  end
                  else if MaxValue < AValue then
                  begin
                    MaxValue := AValue;
                  end;
                  if AValue > 0 then
                  begin
                    if MinPositive = 0 then
                    begin
                      MinPositive := AValue;
                    end
                    else if AValue < MinPositive then
                    begin
                      MinPositive := AValue;
                    end;
                  end;
                end;
              rdtBoolean:
                begin
                  AValue := Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]);
                  if MinValue > AValue then
                  begin
                    MinValue := AValue;
                  end
                  else if MaxValue < AValue then
                  begin
                    MaxValue := AValue;
                  end;
                  if AValue > 0 then
                  begin
                    if MinPositive = 0 then
                    begin
                      MinPositive := AValue;
                    end
                    else if AValue < MinPositive then
                    begin
                      MinPositive := AValue;
                    end;
                  end;
                end;
            else
              Assert(False);
            end;
          end;
        end;
      end;
    end;
  end;
  if ADataArray.DataType = rdtString then
  begin
    MinValue := 0;
    MaxValue := DSValues.Count - 1;
    if DSValues.Count > 1 then
    begin
      MinPositive := 1;
    end;
  end;

  if ADataArray.DataType = rdtDouble then
  begin
    if ADataArray.ContourLimits.LowerLimit.UseLimit then
    begin
      MinValue := ADataArray.ContourLimits.LowerLimit.RealLimitValue;
      if ADataArray.ContourLimits.LogTransform
        and (ADataArray.ContourLimits.LowerLimit.RealLimitValue > 0) then
      begin
        MinPositive := ADataArray.ContourLimits.LowerLimit.RealLimitValue;
      end;
    end;
    if ADataArray.ContourLimits.UpperLimit.UseLimit then
    begin
      MaxValue := ADataArray.ContourLimits.UpperLimit.RealLimitValue;
    end;
  end;
end;

procedure TCustomContourCreator.AssignTriangulationValuesFromGrid(out MinValue,
  MaxValue, MinPositive: double; SelectedColRowLayer: integer; DSValues: TStringList;
  ViewDirection: TViewDirection);
var
  Active: T3DBooleanDataSet;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  DSCol: Integer;
  DSRow: Integer;
  DSLayer: Integer;
  Value: Double;
  ColumnLimit, RowLimit, LayerLimit: integer;
  MaxLength: Integer;
  PointIndex: Integer;
  APoint: TPoint2D;
  TriangleIndex: Integer;
  NodeUL: Integer;
  NodeLL: Integer;
  NodeUR: Integer;
  NodeLR: Integer;
  Model: TCustomModel;
begin
  EvaluateMinMaxLgr(MaxValue, MinValue, MinPositive, DSValues, ViewDirection);
  EvaluateActive(Active, ActiveDataSet);

  LayerLimit := -1;
  RowLimit := -1;
  ColumnLimit := -1;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        ColumnLimit := DataSet.ColumnCount;
        RowLimit := DataSet.RowCount;
        LayerLimit := DataSet.LayerCount;
      end;
    eaNodes:
      begin
        ColumnLimit := DataSet.ColumnCount+1;
        RowLimit := DataSet.RowCount+1;
        LayerLimit := DataSet.LayerCount+1;
      end;
    else Assert(False);
  end;
  case DataSet.Orientation of
    dsoTop: LayerLimit := 1;
    dsoFront: RowLimit := 1;
    dsoSide: ColumnLimit := 1;
    dso3D: ;  // do nothing
    else Assert(False);
  end;

  MaxLength := 0;
  case ViewDirection of
    vdTop:
      begin
        MaxLength := ColumnLimit*RowLimit;
      end;
    vdFront:
      begin
        MaxLength := ColumnLimit*LayerLimit;
      end;
    vdSide:
      begin
        MaxLength := RowLimit*LayerLimit;
      end;
    else Assert(False);
  end;

  FTriangulationData.Free;
  FTriangulationData := TTriangulationData.Create(GetQuantum(LayerLimit),
    GetQuantum(RowLimit), GetQuantum(ColumnLimit));
  SetLength(FTriangulationData.Triangulation, MaxLength*6);
  SetLength(FTriangulationData.X, MaxLength);
  SetLength(FTriangulationData.Y, MaxLength);
  SetLength(FTriangulationData.Values, MaxLength);

  PointIndex := 0;
  for ColIndex := 0 to ColumnLimit - 1 do
  begin
    if (ViewDirection = vdSide) and (ColIndex <> SelectedColRowLayer)
      and (DataSet.Orientation <> dsoSide) then
    begin
      Continue;
    end;
    for RowIndex := 0 to RowLimit - 1 do
    begin
      if (ViewDirection = vdFront) and (RowIndex <> SelectedColRowLayer)
        and (DataSet.Orientation <> dsoFront) then
      begin
        Continue;
      end;
      for LayerIndex := 0 to LayerLimit - 1 do
      begin
        if (ViewDirection = vdTop) and (LayerIndex <> SelectedColRowLayer)
          and (DataSet.Orientation <> dsoTop) then
        begin
          Continue;
        end;
        Column := -1;
        Row := -1;
        Layer := -1;
        case ViewDirection of
          vdTop:
            begin
              Column := ColIndex;
              Row := RowIndex;
              Layer := SelectedColRowLayer;
            end;
          vdFront:
            begin
              Column := ColIndex;
              Row := SelectedColRowLayer;
              Layer := LayerIndex;
            end;
          vdSide:
            begin
              Column := SelectedColRowLayer;
              Row := RowIndex;
              Layer := LayerIndex;
            end;
          else Assert(False);
        end;
        if Active[Column,Row,Layer]
          and DataSet.ContourGridValueOK(LayerIndex, RowIndex, ColIndex) then
        begin
          DSCol := Column;
          DSRow := Row;
          DSLayer := Layer;

          case DataSet.Orientation of
            dsoTop:
              begin
                DSLayer := 0;
              end;
            dsoFront:
              begin
                DSRow := 0;
              end;
            dsoSide:
              begin
                DSCol := 0;
              end;
            dso3D: ; // do nothing
            else Assert(False);
          end;

          Value := 0;
          case DataSet.DataType of
            rdtDouble:
              begin
                Value := DataSet.RealData[DSLayer,DSRow,DSCol];
                if DataSet.ContourLimits.LogTransform then
                begin
                  if Value > 0 then
                  begin
                    Value := Log10(Value);
                  end
                  else
                  begin
                    Assert(MinPositive > 0, Format(StrErrorAttemptingTo, [Value]));
                    Value := Log10(MinPositive/2);
                  end;
                end;
              end;
            rdtInteger:
              begin
                Value := DataSet.IntegerData[DSLayer,DSRow,DSCol];
              end;
            rdtBoolean:
              begin
                Value := Ord(DataSet.BooleanData[DSLayer,DSRow,DSCol]);
              end;
            rdtString:
              begin
                Value := DSValues.IndexOf(DataSet.StringData[DSLayer,DSRow,DSCol]);
              end;
            else Assert(False);
          end;


          case ViewDirection of
            vdTop:
              begin
                APoint := Grid[ColIndex+1,RowIndex+1].P;
              end;
            vdFront:
              begin
                APoint := Grid[ColIndex+1,LayerIndex+1].P;
              end;
            vdSide:
              begin
                APoint := Grid[RowIndex+1,LayerIndex+1].P;
              end;
            else
              Assert(False);
          end;

          FTriangulationData.X[PointIndex] := APoint.x;
          FTriangulationData.Y[PointIndex] := APoint.y;
          FTriangulationData.Values[PointIndex] := Value;
          FTriangulationData.VertexNumbers[DSLayer,DSRow,DSCol] := PointIndex;

          Inc(PointIndex);
        end;
      end;
    end;
  end;
  SetLength(FTriangulationData.X, PointIndex);
  SetLength(FTriangulationData.Y, PointIndex);
  SetLength(FTriangulationData.Values, PointIndex);
  if PointIndex = 0 then
  begin
    SetLength(FTriangulationData.Triangulation, 0);
    Exit;
  end;

  TriangleIndex := 0;
  Model := FDataSet.Model as TCustomModel;
  case ViewDirection of
    vdTop:
      begin
        if DataSet.Orientation = dsoTop then
        begin
          DSLayer := 0
        end
        else
        begin
          DSLayer := SelectedColRowLayer;
        end;
        for ColIndex := 0 to ColumnLimit - 2 do
        begin
          for RowIndex := 0 to RowLimit - 2 do
          begin
            if Active[ColIndex,RowIndex,SelectedColRowLayer]
              and Active[ColIndex+1,RowIndex,SelectedColRowLayer]
              and Active[ColIndex,RowIndex+1,SelectedColRowLayer]
              and Active[ColIndex+1,RowIndex+1,SelectedColRowLayer]
              and DataSet.ContourGridValueOK(DSLayer, RowIndex, ColIndex)
              and DataSet.ContourGridValueOK(DSLayer, RowIndex, ColIndex+1)
              and DataSet.ContourGridValueOK(DSLayer, RowIndex+1, ColIndex)
              and DataSet.ContourGridValueOK(DSLayer, RowIndex+1, ColIndex+1)
              then
            begin
              NodeUL := FTriangulationData.VertexNumbers[DSLayer,RowIndex,ColIndex];
              NodeLL := FTriangulationData.VertexNumbers[DSLayer,RowIndex+1,ColIndex];
              NodeUR := FTriangulationData.VertexNumbers[DSLayer,RowIndex,ColIndex+1];
              NodeLR := FTriangulationData.VertexNumbers[DSLayer,RowIndex+1,ColIndex+1];
              Assert(NodeUL >= 0);
              Assert(NodeLL >= 0);
              Assert(NodeUR >= 0);
              Assert(NodeLR >= 0);

              if Model.ModelSelection <> msPhast then
              begin
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
              end
              else
              begin
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
              end;
            end;
          end;
        end;
      end;
    vdFront:
      begin
        if DataSet.Orientation = dsoFront then
        begin
          DSRow := 0
        end
        else
        begin
          DSRow := SelectedColRowLayer;
        end;
        for ColIndex := 0 to ColumnLimit - 2 do
        begin
          for LayerIndex := 0 to LayerLimit - 2 do
          begin
            if Active[ColIndex,SelectedColRowLayer,LayerIndex]
              and Active[ColIndex+1,SelectedColRowLayer,LayerIndex]
              and Active[ColIndex,SelectedColRowLayer,LayerIndex+1]
              and Active[ColIndex+1,SelectedColRowLayer,LayerIndex+1]
              and DataSet.ContourGridValueOK(LayerIndex, DSRow, ColIndex)
              and DataSet.ContourGridValueOK(LayerIndex, DSRow, ColIndex+1)
              and DataSet.ContourGridValueOK(LayerIndex+1, DSRow, ColIndex)
              and DataSet.ContourGridValueOK(LayerIndex+1, DSRow, ColIndex+1)
              then
            begin
              NodeUL := FTriangulationData.VertexNumbers[LayerIndex,DSRow,ColIndex];
              NodeLL := FTriangulationData.VertexNumbers[LayerIndex+1,DSRow,ColIndex];
              NodeUR := FTriangulationData.VertexNumbers[LayerIndex,DSRow,ColIndex+1];
              NodeLR := FTriangulationData.VertexNumbers[LayerIndex+1,DSRow,ColIndex+1];
              Assert(NodeUL >= 0);
              Assert(NodeLL >= 0);
              Assert(NodeUR >= 0);
              Assert(NodeLR >= 0);

              if Model.ModelSelection <> msPhast then
              begin
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
              end
              else
              begin
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
              end;
            end;
          end;
        end;
      end;
    vdSide:
      begin
        if DataSet.Orientation = dsoSide then
        begin
          DSCol := 0;
        end
        else
        begin
          DSCol := SelectedColRowLayer;
        end;
        for RowIndex := 0 to RowLimit - 2 do
        begin
          for LayerIndex := 0 to LayerLimit - 2 do
          begin
            if Active[SelectedColRowLayer,RowIndex,LayerIndex]
              and Active[SelectedColRowLayer,RowIndex+1,LayerIndex]
              and Active[SelectedColRowLayer,RowIndex,LayerIndex+1]
              and Active[SelectedColRowLayer,RowIndex+1,LayerIndex+1]
              and DataSet.ContourGridValueOK(LayerIndex, RowIndex, DSCol)
              and DataSet.ContourGridValueOK(LayerIndex, RowIndex+1, DSCol)
              and DataSet.ContourGridValueOK(LayerIndex+1, RowIndex, DSCol)
              and DataSet.ContourGridValueOK(LayerIndex+1, RowIndex+1, DSCol)
              then
            begin
              NodeUR := FTriangulationData.VertexNumbers[LayerIndex,RowIndex,DSCol];
              NodeUL := FTriangulationData.VertexNumbers[LayerIndex+1,RowIndex,DSCol];
              NodeLR := FTriangulationData.VertexNumbers[LayerIndex,RowIndex+1,DSCol];
              NodeLL := FTriangulationData.VertexNumbers[LayerIndex+1,RowIndex+1,DSCol];
              Assert(NodeUL >= 0);
              Assert(NodeLL >= 0);
              Assert(NodeUR >= 0);
              Assert(NodeLR >= 0);

              if Model.ModelSelection <> msPhast then
              begin
                FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                Inc(TriangleIndex);
                FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                Inc(TriangleIndex);
              end
              else
              begin
                if ViewDirection = vdSide then
                begin
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                  Inc(TriangleIndex);
                end
                else
                begin
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUL;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeUR;
                  Inc(TriangleIndex);
                  FTriangulationData.Triangulation[TriangleIndex] := NodeLR;
                  Inc(TriangleIndex);
                end;
              end;
            end;
          end;
        end;
      end;
    else Assert(False);
  end;
  SetLength(FTriangulationData.Triangulation, TriangleIndex);
  FTriangulationData.EliminateUniformTriangles;
end;

procedure TCustomContourCreator.AssignTriangulationValuesFromMesh(out MinValue,
  MaxValue, MinPositive: double; SelectedColRowLayer: integer; DSValues: TStringList;
  ViewDirection: TViewDirection);
var
  ColIndex: Integer;
  LayerIndex: Integer;
  DSCol: Integer;
  DSRow: Integer;
  DSLayer: Integer;
  Value: Double;
  MaxLength: Integer;
  PointIndex: Integer;
  APoint: TPoint2D;
  AnNode2D: INode2D;
  N: integer;
  IADJ: TIntArray;
  IEND: TIntArray;
//  IER: Integer;
  NT: Integer;
  IPL: TIntArray;
  IPT: TIntArray;
  NodeList: TINode2DList;
  ElementList: TIElement2DList;
  ALine: TLine2D;
  ClosestPoint: TPoint2D;
  Node2D: INode2D;
  StartPoint: TPoint2D;
  Angle: double;
  X_Float: TFloat;
  Node3D: INode3D;
  Node2D_Index: Integer;
  SegmentAngle: Double;
  Element3D: IElement3D;
  Element2D_Index: Integer;
  AnElement2D: IElement2D;
  LayerCount: Integer;
  Exaggeration: Double;
//  Temp: Real;
//  TestIndex: Integer;
//  MaxRand: Integer;
//  PIndex: Integer;
  NodeNumbers: TTwoDIntegerArray;
  ActiveNodes: T2DBoolArray;
  NodeNumber: Integer;
  Triangulator: TMeshTriangulator;
  OldLength: Integer;
  PIndex: Integer;
  procedure AssignValue;
  begin
    Value := 0;
    case DataSet.DataType of
      rdtDouble:
        begin
          Value := DataSet.RealData[DSLayer,DSRow,DSCol];
          if DataSet.ContourLimits.LogTransform then
          begin
            if Value > 0 then
            begin
              Value := Log10(Value);
            end
            else
            begin
              Assert(MinPositive > 0, Format(StrErrorAttemptingTo, [Value]));
              Value := Log10(MinPositive/2);
            end;
          end;
        end;
      rdtInteger:
        begin
          Value := DataSet.IntegerData[DSLayer,DSRow,DSCol];
        end;
      rdtBoolean:
        begin
          Value := Ord(DataSet.BooleanData[DSLayer,DSRow,DSCol]);
        end;
      rdtString:
        begin
          Value := DSValues.IndexOf(DataSet.StringData[DSLayer,DSRow,DSCol]);
        end;
      else Assert(False);
    end
  end;
  function ConvertProfileY(Y: Double): double;
  begin
    if (frmGoPhast.ModelSelection in SutraSelection) 
      and (frmGoPhast.SutraMesh.MeshType = mtProfile) then
    begin
      result := Y * Exaggeration;
    end
    else
    begin
      result := Y;
    end;
  end;
begin
  EvaluateMinMaxMesh(MaxValue, MinValue, MinPositive, DSValues, ViewDirection);
  EvaluateActiveMesh(Active, DataSet);

  Exaggeration := frmGoPhast.PhastModel.Exaggeration;
  MaxLength := 0;

  if not Mesh.Is3DMesh then
  begin
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          MaxLength := Mesh.Mesh2DI.ElementCount;
        end;
      eaNodes:
        begin
          MaxLength := Mesh.Mesh2DI.NodeCount;
        end;
      else Assert(False);
    end;
    LayerCount := 1;
  end
  else
  begin
    LayerCount := Mesh.LayerCount;
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          MaxLength := Mesh.ElementCount;
        end;
      eaNodes:
        begin
          MaxLength := Mesh.NodeCount;
          Inc(LayerCount);
        end;
      else Assert(False);
    end;
  end;

  FTriangulationData.Free;
  FTriangulationData := TTriangulationData.Create(GetQuantum(LayerCount),
    GetQuantum(1), GetQuantum(MaxLength));
  SetLength(FTriangulationData.Triangulation, MaxLength*6);
  SetLength(FTriangulationData.X, MaxLength);
  SetLength(FTriangulationData.Y, MaxLength);
  SetLength(FTriangulationData.Values, MaxLength);

  PointIndex := 0;
  case ViewDirection of
    vdTop:
      begin
        if SelectedColRowLayer >= DataSet.LayerCount then
        begin
          SelectedColRowLayer := DataSet.LayerCount-1;
        end;
        DSLayer := SelectedColRowLayer;
        FSelectedLayer := SelectedColRowLayer;
        DSRow := 0;
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              for ColIndex := 0 to Mesh.Mesh2DI.ElementCount - 1 do
              begin
                if Active[ColIndex,0,SelectedColRowLayer] then
                begin
                  APoint := Mesh.Mesh2DI.ElementsI2D[ColIndex].Center;
                  FTriangulationData.X[PointIndex] := APoint.X;
                  FTriangulationData.Y[PointIndex] := ConvertProfileY(APoint.Y);
                  DSCol := ColIndex;
                  AssignValue;
                  FTriangulationData.Values[PointIndex] := Value;
                  Inc(PointIndex);
                end;
              end;
            end;
          eaNodes:
            begin
              for ColIndex := 0 to Mesh.Mesh2DI.NodeCount - 1 do
              begin
                if Active[ColIndex,0,SelectedColRowLayer] then
                begin
                  AnNode2D := Mesh.Mesh2DI.NodesI2D[ColIndex];
                  FTriangulationData.X[PointIndex] := AnNode2D.Location.X;
                  FTriangulationData.Y[PointIndex] := ConvertProfileY(AnNode2D.Location.Y);
                  DSCol := ColIndex;
                  AssignValue;
                  FTriangulationData.Values[PointIndex] := Value;
                  Inc(PointIndex);
                end;
              end;
            end;
        end;

//        RandSeed := PointIndex;
//        MaxRand := PointIndex;
//        for TestIndex := 0 to MaxRand -1 do
//        begin
//          PIndex := Random(MaxRand);
//          Temp := FTriangulationData.X[TestIndex];
//          FTriangulationData.X[TestIndex] := FTriangulationData.X[PIndex];
//          FTriangulationData.X[PIndex] := Temp;
//
//          Temp := FTriangulationData.Y[TestIndex];
//          FTriangulationData.Y[TestIndex] := FTriangulationData.Y[PIndex];
//          FTriangulationData.Y[PIndex] := Temp;
//
//          Temp := FTriangulationData.Values[TestIndex];
//          FTriangulationData.Values[TestIndex] := FTriangulationData.Values[PIndex];
//          FTriangulationData.Values[PIndex] := Temp;
//        end;
      end;
    vdFront:
      begin
        ALine := EquateLine(Mesh.CrossSection.StartPoint, Mesh.CrossSection.EndPoint);
        SegmentAngle := Mesh.CrossSection.Angle;

        ClosestPoint := ClosestPointOnLineFromPoint(
          ALine, EquatePoint(0.0, 0.0));

        DSRow := 0;
        StartPoint := EquatePoint(0.0, 0.0);
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              ElementList := TIElement2DList.Create;
              try
                Mesh.GetElementsIntfOnCrossSection(ElementList);

                SetLength(NodeNumbers, Mesh.LayerCount, ElementList.Count);
                SetLength(ActiveNodes, Mesh.LayerCount, ElementList.Count);
                NodeNumber := 0;

                for Element2D_Index := 0 to ElementList.Count - 1 do
                begin
                  AnElement2D := ElementList[Element2D_Index];
                  DSCol := AnElement2D.ElementNumber;
                  APoint := AnElement2D.Center;
                  Angle := ArcTan2(APoint.y - StartPoint.y,
                    APoint.x - StartPoint.x) - SegmentAngle;
                  X_Float := Distance(StartPoint, APoint)*Cos(Angle)
                    + StartPoint.x;
                  for LayerIndex := 0 to Mesh.LayerCount - 1 do
                  begin

                    Element3D := Mesh.ElementArrayI[LayerIndex,
                      AnElement2D.ElementNumber];
                    if Element3D.Active then
                    begin
                      DSLayer := LayerIndex;
                      FTriangulationData.X[PointIndex] := X_Float;
                      FTriangulationData.Y[PointIndex] := Element3D.CenterElevation;
                      AssignValue;
                      FTriangulationData.Values[PointIndex] := Value;
                      Inc(PointIndex);
                      Inc(NodeNumber);
                      NodeNumbers[LayerIndex,Element2D_Index] := NodeNumber;
                      ActiveNodes[LayerIndex,Element2D_Index] := True;
                    end
                    else
                    begin
                      NodeNumbers[LayerIndex,Element2D_Index] := 0;
                      ActiveNodes[LayerIndex,Element2D_Index] := False;
                    end;
                  end;
                end;
                // Exchange first and last points
                // so the first three points are not colinear.
{
                if ElementList.Count > 0 then
                begin
                  Temp := FTriangulationData.X[0];
                  FTriangulationData.X[0] := FTriangulationData.X[PointIndex-1];
                  FTriangulationData.X[PointIndex-1] := Temp;

                  Temp := FTriangulationData.Y[0];
                  FTriangulationData.Y[0] := FTriangulationData.Y[PointIndex-1];
                  FTriangulationData.Y[PointIndex-1] := Temp;

                  Temp := FTriangulationData.Values[0];
                  FTriangulationData.Values[0] := FTriangulationData.Values[PointIndex-1];
                  FTriangulationData.Values[PointIndex-1] := Temp;
                end;
}
              finally
                ElementList.Free;
              end;
            end;
          eaNodes:
            begin
              NodeList := TINode2DList.Create;
              try
                Mesh.GetNodesIntfOnCrossSection(NodeList);

                SetLength(NodeNumbers, Mesh.LayerCount+1, NodeList.Count);
                SetLength(ActiveNodes, Mesh.LayerCount+1, NodeList.Count);
                NodeNumber := 0;

                for Node2D_Index := 0 to NodeList.Count - 1 do
                begin
                  Node2D := NodeList[Node2D_Index];
                  DSCol := Node2D.NodeNumber;
                  Angle := ArcTan2(Node2D.Location.y - StartPoint.y,
                    Node2D.Location.x - StartPoint.x) - SegmentAngle;
                  X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
                    + StartPoint.x;
                  for LayerIndex := 0 to Mesh.LayerCount do
                  begin
                    Node3D := Mesh.NodeArrayI[LayerIndex, Node2D.NodeNumber];
                    if Node3D.Active then
                    begin
                      DSLayer := LayerIndex;
                      FTriangulationData.X[PointIndex] := X_Float;
                      FTriangulationData.Y[PointIndex] := Node3D.Z;
                      AssignValue;
                      FTriangulationData.Values[PointIndex] := Value;
                      Inc(PointIndex);
                      Inc(NodeNumber);
                      NodeNumbers[LayerIndex,Node2D_Index] := NodeNumber;
                      ActiveNodes[LayerIndex,Node2D_Index] := True;
                    end
                    else
                    begin
                      NodeNumbers[LayerIndex,Node2D_Index] := 0;
                      ActiveNodes[LayerIndex,Node2D_Index] := False;
                    end;
                  end;
                end;
                // Exchange first and last points
                // so the first three points are not colinearl.
{
                if NodeList.Count > 0 then
                begin
                  Temp := FTriangulationData.X[0];
                  FTriangulationData.X[0] := FTriangulationData.X[PointIndex-1];
                  FTriangulationData.X[PointIndex-1] := Temp;

                  Temp := FTriangulationData.Y[0];
                  FTriangulationData.Y[0] := FTriangulationData.Y[PointIndex-1];
                  FTriangulationData.Y[PointIndex-1] := Temp;

                  Temp := FTriangulationData.Values[0];
                  FTriangulationData.Values[0] := FTriangulationData.Values[PointIndex-1];
                  FTriangulationData.Values[PointIndex-1] := Temp;
                end;
}
              finally
                NodeList.Free;
              end;
            end;
          else Assert(False);
        end;
      end;
    else
      Assert(False);
  end;

  N := PointIndex;
  SetLength(FTriangulationData.X, N);
  SetLength(FTriangulationData.Y, N);
  SetLength(FTriangulationData.Values, N);
  SetLength(IEND, N);
  if N <= 1 then
  begin
    Exit;
  end;
  SetLength(IADJ, 6*N-9);

  case ViewDirection of
    vdTop:
      begin
        Triangulator := TMeshTriangulator.Create((Mesh as IMesh3D).Mesh2DI);
        try
          case DataSet.EvaluatedAt of
            eaBlocks:
              begin
                Triangulator.OnIsItemActive := IsElementActive;
                Triangulator.TriangulateElements(IADJ, IEND);
              end;
            eaNodes:
              begin
                Triangulator.OnIsItemActive := IsNodeActive;
                Triangulator.TriangulateNodes(IADJ, IEND);
              end;
            else
              Assert(False);
          end;
          if Length(IADJ) < 6*N-9 then
          begin
            OldLength := Length(IADJ);
            SetLength(IADJ, 6*N-9);
            for PIndex := OldLength to 6*N-9 - 1 do
            begin
              IADJ[PIndex] := 0;
            end;
          end;
        finally
          Triangulator.Free;
        end;

//        TRMESH (N, FTriangulationData.X, FTriangulationData.Y, IADJ, IEND, IER);
      end;
    vdFront:
      begin
        GetTriangulation(NodeNumbers, ActiveNodes, IADJ, IEND);
      end;
    else
      begin
        Assert(False);
      end;
  end;

//  if IER <> 0 then
//  begin
//    SetLength(FTriangulationData.Triangulation, 0);
//    Exit;
//  end;
  FTriangulationData.IADJ := IADJ;
  FTriangulationData.IEND := IEND;

  SetLength(IPL, 6*N);
  SetLength(IPT, 6*N-15);

  Trmesh_2_Idtang(NT, IEND, IADJ, IPL, IPT, N);
  FTriangulationData.Triangulation := IPT;
  SetLength(FTriangulationData.Triangulation, NT*3);
end;

destructor TCustomContourCreator.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTriangulationData.EliminateUniformTriangles;
var
  Index: Integer;
  NT: integer;
  N1: Integer;
  N2: Integer;
  N3: Integer;
  NewIndex: Integer;
begin
  NT := Length(Triangulation) div 3;
  NewIndex := 0;
  for Index := 0 to NT - 1 do
  begin
    N1 := Triangulation[Index * 3];
    N2 := Triangulation[Index * 3 + 1];
    N3 := Triangulation[Index * 3 + 2];
    if Index <> NewIndex then
    begin
      Triangulation[NewIndex * 3] := N1;
      Triangulation[NewIndex * 3 + 1] := N2;
      Triangulation[NewIndex * 3 + 2] := N3;
    end;
    if (Values[N1] <> Values[N2])
      or (Values[N1] <> Values[N3]) then
    begin
      Inc(NewIndex);
    end;
  end;
  if NewIndex <> NT then
  begin
    SetLength(Triangulation, NewIndex * 3);
  end;
end;

procedure TCustomContourCreator.EvaluateActive(var Active: T3DBooleanDataSet;
  AnActiveDataSet: TDataArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  AnActiveDataSet.Initialize;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        SetLength(Active, AnActiveDataSet.ColumnCount,
          AnActiveDataSet.RowCount, AnActiveDataSet.LayerCount);
      end;
    eaNodes:
      begin
        SetLength(Active, AnActiveDataSet.ColumnCount + 1,
          AnActiveDataSet.RowCount + 1, AnActiveDataSet.LayerCount + 1);
        for ColIndex := 0 to AnActiveDataSet.ColumnCount do
        begin
          for RowIndex := 0 to AnActiveDataSet.RowCount do
          begin
            for LayerIndex := 0 to AnActiveDataSet.LayerCount do
            begin
              Active[ColIndex, RowIndex, LayerIndex] := False;
            end;
          end;
        end;
      end;
  else
    Assert(False);
  end;
  for ColIndex := 0 to AnActiveDataSet.ColumnCount - 1 do
  begin
    for RowIndex := 0 to AnActiveDataSet.RowCount - 1 do
    begin
      for LayerIndex := 0 to AnActiveDataSet.LayerCount - 1 do
      begin
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              if AnActiveDataSet.DataType = rdtBoolean then
              begin
                Active[ColIndex, RowIndex, LayerIndex] :=
                  AnActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
              end
              else
              begin
                Active[ColIndex, RowIndex, LayerIndex] :=
                  AnActiveDataSet.IntegerData[LayerIndex, RowIndex, ColIndex] > 0;
              end;
            end;
          eaNodes:
            begin
              if AnActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
              begin
                Active[ColIndex, RowIndex, LayerIndex] := True;
                Active[ColIndex + 1, RowIndex, LayerIndex] := True;
                Active[ColIndex, RowIndex + 1, LayerIndex] := True;
                Active[ColIndex + 1, RowIndex + 1, LayerIndex] := True;
                Active[ColIndex, RowIndex, LayerIndex + 1] := True;
                Active[ColIndex + 1, RowIndex, LayerIndex + 1] := True;
                Active[ColIndex, RowIndex + 1, LayerIndex + 1] := True;
                Active[ColIndex + 1, RowIndex + 1, LayerIndex + 1] := True;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TCustomContourCreator.EvaluateActiveMesh(
  var Active: T3DBooleanDataSet; ADataSet: TDataArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  AnElement: IElement3D;
  ANode: INode3D;
begin
  ADataSet.Initialize;
  SetLength(Active, ADataSet.ColumnCount,
    ADataSet.RowCount, ADataSet.LayerCount);
  if not Mesh.Is3DMesh then
  begin
    for ColIndex := 0 to ADataSet.ColumnCount-1 do
    begin
      for RowIndex := 0 to ADataSet.RowCount-1 do
      begin
        for LayerIndex := 0 to ADataSet.LayerCount-1 do
        begin
          Active[ColIndex, RowIndex, LayerIndex] := True;
        end;
      end;
    end;
  end
  else
  begin
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          for ColIndex := 0 to ADataSet.ColumnCount - 1 do
          begin
            if ADataSet.Orientation = dsoTop then
            begin
              for LayerIndex := 0 to Mesh.LayerCount - 1 do
              begin
                AnElement := Mesh.ElementArrayI[LayerIndex, ColIndex];
                Active[ColIndex, 0, 0] :=
                  Active[ColIndex, 0, 0] or AnElement.Active;
              end;
            end
            else
            begin
              for LayerIndex := 0 to ADataSet.LayerCount - 1 do
              begin
                AnElement := Mesh.ElementArrayI[LayerIndex, ColIndex];
                Active[ColIndex, 0, LayerIndex] := AnElement.Active;
              end;
            end;
          end;
        end;
      eaNodes:
        begin
          for ColIndex := 0 to ADataSet.ColumnCount - 1 do
          begin
            if ADataSet.Orientation = dsoTop then
            begin
              for LayerIndex := 0 to Mesh.LayerCount do
              begin
                ANode := Mesh.NodeArrayI[LayerIndex, ColIndex];
                Active[ColIndex, 0, 0] := Active[ColIndex, 0, 0]
                  or ANode.Active;
              end;
            end
            else
            begin
              for LayerIndex := 0 to ADataSet.LayerCount -1 do
              begin
                ANode := Mesh.NodeArrayI[LayerIndex, ColIndex];
                Active[ColIndex, 0, LayerIndex] := ANode.Active;
              end;
            end;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TMultipleContourCreator.Clear;
begin
  FLabelLocations.Clear;
  FLabels.Clear;
  Inherited;
end;

constructor TMultipleContourCreator.Create;
begin
  inherited;
  FAlgorithm := caSimple;
  FLabelLocations := TRbwQuadTree.Create(nil);
  FLabels := TLabelObjectList.Create;
end;

procedure TCustomContourCreator.Clear;
begin
  FreeAndNil(FTriangulationData);
end;


procedure TCustomContourCreator.CreateSimpleContoursFromMesh
  (const ContourValues: TOneDRealArray);
var
  NeighborStart: Integer;
  ALineList: TLineList;
  PointIndex: Integer;
  NeighborStop: Integer;
  NeighborNode2: Integer;
  NeighborIndex: Integer;
  NeighborNode1: Integer;
  PointValue: Real;
  Neighbor1Value: Real;
  Neighbor2Value: Real;
  PointX: Real;
  Neighbor1X: Real;
  Neighbor2X: Real;
  PointY: Real;
  Neighbor1Y: Real;
  Neighbor2Y: Real;
  ContourIndex: Integer;
  Frac1: Extended;
  Frac2: Extended;
  CrossX1: Real;
  CrossY1: Real;
  CrossX2: Real;
  CrossY2: Real;
  ALine: TLine;
  MeshOutline: TOutline;
  AValue: Double;
begin
  if Length(FTriangulationData.X) <= 1 then
  begin
    Exit;
  end;

  Assert(Assigned(Mesh));

  if FMeshOutline = nil then
  begin
//      LocalMesh := Mesh as TSutraMesh3D;
    case ViewDirection of
      vdTop: FMeshOutline := Mesh.TopOutline(Mesh.SelectedLayer);
      vdFront: FMeshOutline := Mesh.FrontOutline;
      else Assert(False);
    end;
  end;

//  Assert(Assigned(FMeshOutline));
  Assert(Assigned(FTriangulationData));
  NeighborStart := 0;
  PlotList.Clear;
  CurrentLineList := nil;
  CurrentLine := nil;

  if Assigned(FMeshOutline) then
  begin
    MeshOutline := FMeshOutline as TOutline;

  end
  else
  begin
    MeshOutline := nil;
  end;

  ALineList := TLineList.Create;
  PlotList.Add(ALineList);
  for PointIndex := 0 to Length(FTriangulationData.X) - 1 do
  begin
    // Values in FTriangulationData.IEND and FTriangulationData.IADJ
    // are offset by 1.
    NeighborStop := FTriangulationData.IEND[PointIndex] -1;
    NeighborNode2 := FTriangulationData.IADJ[NeighborStop]-1;
    for NeighborIndex := NeighborStart to NeighborStop do
    begin
      NeighborNode1 := FTriangulationData.IADJ[NeighborIndex]-1;

      if (NeighborNode1 >= 0) and (NeighborNode2 >= 0) then
      begin
        PointValue := FTriangulationData.Values[PointIndex];
        Neighbor1Value := FTriangulationData.Values[NeighborNode1];
        Neighbor2Value := FTriangulationData.Values[NeighborNode2];
        if (PointValue <> Neighbor1Value)
          or (PointValue <> Neighbor2Value)  then
        begin
          PointX := FTriangulationData.X[PointIndex];
          Neighbor1X := FTriangulationData.X[NeighborNode1];
          Neighbor2X := FTriangulationData.X[NeighborNode2];
          PointY := FTriangulationData.Y[PointIndex];
          Neighbor1Y := FTriangulationData.Y[NeighborNode1];
          Neighbor2Y := FTriangulationData.Y[NeighborNode2];
          for ContourIndex := 0 to Length(ContourValues) - 1 do
          begin
            AValue := ContourValues[ContourIndex];
            if ((PointValue >= AValue) <> (Neighbor1Value >= AValue))
              and ((PointValue >= AValue) <> (Neighbor2Value >= AValue)) then
            begin
              Frac1 := (PointValue-AValue)/(PointValue-Neighbor1Value);
              Frac2 := (PointValue-AValue)/(PointValue-Neighbor2Value);
              if PointX = Neighbor1X then
              begin
                CrossX1 := PointX;
              end
              else
              begin
                CrossX1 := PointX + Frac1*(Neighbor1X-PointX);
              end;
              if PointY = Neighbor1Y then
              begin
                CrossY1 := PointY;
              end
              else
              begin
                CrossY1 := PointY + Frac1*(Neighbor1Y-PointY);
              end;
              if PointX = Neighbor2X then
              begin
                CrossX2 := PointX;
              end
              else
              begin
                CrossX2 := PointX + Frac2*(Neighbor2X-PointX);
              end;
              if PointY = Neighbor2Y then
              begin
                CrossY2 := PointY;
              end
              else
              begin
                CrossY2 := PointY + Frac2*(Neighbor2Y-PointY);
              end;

              if (MeshOutline = nil) or MeshOutline.PointInside(
                (CrossX1+CrossX2)/2, (CrossY1+CrossY2)/2) then
              begin
                ALine := TLine.Create;
                ALineList.Add(ALine);
                ALine.ContourLevel := AValue;
                ALine.Add(CrossX1, CrossY1);
                ALine.Add(CrossX2, CrossY2);
              end;
            end;
          end;
        end;
      end;
      NeighborNode2 := NeighborNode1;
    end;
    NeighborStart := NeighborStop+1
  end
end;

procedure TMultipleContourCreator.PlotContourLines(const ContourValues, LineThicknesses: TOneDRealArray;
  const ContourColors: TArrayOfColor32; ContourLabels: TStringList);
var
  ContourIndex: integer;
  PointIndex: integer;
  Exaggeration: double;
  APlot: TLineList;
  LabelLocations: TRbwQuadTree;
  Labels: TLabelObjectList;
  AContourLine: TLine;
  ValueIndex: Integer;
  AColor: TColor32;
  LineThickness: Double;
  Points: array of TPoint;
  ALocation: TLocation;
  CenterX: Integer;
  CenterY: Integer;
  CenterIndex: Integer;
  AContourLabel: string;
  AValue: Double;
  ContVals: TRealList;
  index: Integer;
begin
  if PlotList.Count = 0 then
  begin
    Exit;
  end;
  Exaggeration := 1;
  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    if (FMesh as TSutraMesh3D).MeshType = mtProfile then
    begin
      Exaggeration := frmGoPhast.PhastModel.Exaggeration;
    end;
  end;
  ContVals := TRealList.Create;
  try
    ContVals.Capacity := Length(ContourValues);
    for index := 0 to Length(ContourValues) - 1 do
    begin
      ContVals.Add(ContourValues[index]);
    end;

    Assert(PlotList.Count = 1);
    APlot := PlotList[0];
    if frmGoPhast.PhastModel.ShowContourLabels then
    begin
      LabelLocations := FLabelLocations;
      Labels := FLabels;
    end
    else
    begin
      LabelLocations := nil;
      Labels := nil;
    end;
    for ContourIndex := 0 to APlot.Count - 1 do
    begin
      AContourLine := APlot[ContourIndex];
      if AContourLine.Count = 0 then
      begin
        Continue;
      end;
      ValueIndex := ContVals.IndexOfClosest(AContourLine.ContourLevel);
      if ValueIndex < 0 then
      begin
        Continue;
      end;
      AColor := ContourColors[ValueIndex];
      LineThickness := LineThicknesses[ValueIndex];
      SetLength(Points, AContourLine.Count);
      for PointIndex := 0 to AContourLine.Count - 1 do
      begin
        ALocation := AContourLine.Items[PointIndex];
        Points[PointIndex].X := ZoomBox.XCoord(ALocation.x);
        Points[PointIndex].Y := ZoomBox.YCoord(ALocation.y/Exaggeration);
      end;
      DrawBigPolyline32(Bitmap, AColor, LineThickness,
        Points, True);


      if LabelLocations <> nil then
      begin
        Assert(Labels <> nil);
        if AContourLine.Count = 2 then
        begin
          CenterX := (Points[0].X + Points[1].X) div 2;
          CenterY := (Points[0].Y + Points[1].Y) div 2;
        end
        else
        begin
          CenterIndex := AContourLine.Count div 2;
          CenterX := Points[CenterIndex].X;
          CenterY := Points[CenterIndex].Y;
        end;
        if ContourLabels.Count = 0 then
        begin
          AValue := AContourLine.ContourLevel;
          if DataSet.ContourLimits.LogTransform then
          begin
            AValue := Power(10, AValue);
          end;

          AContourLabel := FloatToStrF(AValue,
            ffGeneral, 7, 0);
        end
        else
        begin
          AContourLabel := ContourLabels[ValueIndex];
        end;
        if (CenterX > 0) and (CenterY > 0)
          and (CenterX < LabelLocations.XMax)
          and (CenterY < LabelLocations.YMax) then
        begin

          PlotLabel(CenterX, CenterY, LabelLocations, AContourLabel,
            Labels, BitMap, FLabelSpacing);
        end;
      end;
    end
  finally
    ContVals.Free;
  end;
end;

procedure TMultipleContourCreator.InitializeLabelData;
begin
  FLabelLocations.Clear;
  FLabels.Clear;
  FLabelLocations.XMin := 0;
  FLabelLocations.YMin := 0;
  if (BitMap is TBitmap32) then
  begin
    FLabelLocations.XMax := TBitmap32(BitMap).Width;
    FLabelLocations.YMax := TBitmap32(BitMap).Height;
  end
  else
  begin
//    FLabelLocations.XMax := (BitMap as TCanvas).Width;
//    FLabelLocations.YMax := (BitMap as TCanvas).Height;
  end;
end;

procedure TMultipleContourCreator.CreateAndDrawContours(
  const ContourValues, LineThicknesses: TOneDRealArray;
  const ContourColors: TArrayOfColor32; ContourLabels: TStringList);
var
  ContourIndex: Integer;
  ContourCreator: TContourCreator;
  AValue: Double;
  C: TRealArray;
  index: Integer;
begin
  Assert(Length(ContourValues) = Length(LineThicknesses));
  Assert(Length(ContourValues) = Length(ContourColors));

  InitializeLabelData;

  case FAlgorithm of
    caSimple:
      begin
        if Assigned(Grid) then
        begin
          ContourCreator := TContourCreator.Create(FLabelSpacing);
          try
            ContourCreator.BitMap := BitMap;
            ContourCreator.Grid := Grid;
            ContourCreator.ZoomBox := ZoomBox;
            ContourCreator.EvaluatedAt := DataSet.EvaluatedAt;
            for ContourIndex := 0 to Length(ContourValues) - 1 do
            begin
              AValue := ContourValues[ContourIndex];
              ContourCreator.Value := AValue;

              if DataSet.ContourLimits.LogTransform then
              begin
                AValue := Power(10, AValue);
              end;

              if ContourLabels.Count = 0 then
              begin
                ContourCreator.ContourLabel := FloatToStrF(AValue, ffGeneral, 7, 0);
              end
              else
              begin
                ContourCreator.ContourLabel := ContourLabels[ContourIndex];
              end;

              ContourCreator.Color := ContourColors[ContourIndex];
              ContourCreator.LineThickness := LineThicknesses[ContourIndex];
              if frmGoPhast.PhastModel.ShowContourLabels then
              begin
                ContourCreator.LabelLocations := FLabelLocations;
                ContourCreator.Labels := FLabels;
              end
              else
              begin
                ContourCreator.LabelLocations := nil;
                ContourCreator.Labels := nil;
              end;
              ContourCreator.DrawContour;
            end;
          finally
            ContourCreator.Free;
          end;
        end
        else
        begin
          CreateSimpleContoursFromMesh(ContourValues);

//          ContVals := TRealList.Create;
//          try
//            ContVals.Capacity := Length(ContourValues);
//            for index := 0 to Length(ContourValues) - 1 do
//            begin
//              ContVals.Add(ContourValues[index]);
//            end;
            PlotContourLines(ContourValues, LineThicknesses, ContourColors,
              ContourLabels);
//          finally
//            ContVals.Free;
//          end;
        end;
      end;
    caACM626:
      begin
//        ContVals := TRealList.Create;
//        try
//          ContVals.Capacity := Length(ContourValues);
          SetLength(C, Length(ContourValues));
          for index := 0 to Length(ContourValues) - 1 do
          begin
            C[index] := ContourValues[index];
//            ContVals.Add(ContourValues[index]);
          end;
          PerformAlg626(C);

          PlotContourLines(ContourValues, LineThicknesses, ContourColors,
              ContourLabels);
//        finally
//          ContVals.Free;
//        end;
      end;
    else
      Assert(False, StrInvalidContourAlgo);
  end;

  DrawContourLabels;

end;

{ TTriangulationData }

constructor TTriangulationData.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  FVertexNumbers:= T3DSparseIntegerArray.Create(Quantum1, Quantum2, Quantum3);
end;

destructor TTriangulationData.Destroy;
begin
  FVertexNumbers.Free;
  inherited;
end;

function TTriangulationData.GetVertexNumber(Layer, Row, Col: Integer): Integer;
begin
  if FVertexNumbers.IsValue[Layer, Row, Col] then
  begin
    result := FVertexNumbers.Items[Layer, Row, Col]
  end
  else
  begin
    result := -1;
  end;
end;

procedure TTriangulationData.SetVertexNumber(Layer, Row, Col: Integer;
  const Value: Integer);
begin
  FVertexNumbers.Items[Layer, Row, Col] := Value;
end;

end.
