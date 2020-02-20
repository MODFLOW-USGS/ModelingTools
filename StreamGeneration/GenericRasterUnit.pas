unit GenericRasterUnit;

interface

uses
  Winapi.Windows, RasterValuesAlongSegmentsUnit, FastGEO, System.Types,
  System.Generics.Collections, SurferGridFileReaderUnit, System.SysUtils,
  ClippedSurferRasterUnit;

type
  T2DIntArray = array of array of integer;

  TFlowDirection = record
  strict private
    FDeltaX: integer;
    FDeltaY: Integer;
    procedure SetDeltaX(const Value: integer);
    procedure SetDeltaY(const Value: Integer);
  public
    property DeltaX: integer read FDeltaX write SetDeltaX;
    property DeltaY: Integer read FDeltaY write SetDeltaY;
  end;

  TFlowOrdinal = (foUpperLeft, foUpper,  foUpperRight,
                  foLeft,      foMiddle, foRight,
                  foLowerLeft, foLower,  foLowerRight);

  TFlowDirections = class(TObject)
  private
  const
    FlowOrdinals:  array[-1..1, -1..1] of TFlowOrdinal
      = ((foUpperLeft, foUpper,  foUpperRight),
         (foLeft,      foMiddle, foRight),
         (foLowerLeft, foLower,  foLowerRight));
    FlowValues: array[TFlowOrdinal] of TFlowDirection
      = ((FDeltaX : -1; FDeltaY : -1), (FDeltaX : 0; FDeltaY : -1), (FDeltaX : 1; FDeltaY : -1),
         (FDeltaX : -1; FDeltaY :  0), (FDeltaX : 0; FDeltaY :  0), (FDeltaX : 1; FDeltaY :  0),
         (FDeltaX : -1; FDeltaY :  1), (FDeltaX : 0; FDeltaY :  1), (FDeltaX : 1; FDeltaY :  1));
    FModified: Boolean;
    procedure ReadOrdinalsFromRaster;
    procedure WriteOrdinalsToRaster;
    procedure SetOrdinalArraySize;
    function GetItems(XIndex, YIndex: Integer): TFlowDirection;
    procedure SetItems(XIndex, YIndex: Integer; const Value: TFlowDirection);
    function GetOrdinals(XIndex, YIndex: Integer): TFlowOrdinal;
    procedure SetOrdinals(XIndex, YIndex: Integer; const Value: TFlowOrdinal);
  var
    FOrdinals: array of array of TFlowOrdinal;
    FXCount: Integer;
    FYCount: Integer;
    FRasterInterface: IRasterFlush;
  public
    constructor Create(XCount, YCount: Integer); overload;
    constructor Create(RasterInterface: IRasterFlush); overload;
    property XCount: Integer read FXCount;
    property YCount: Integer read FYCount;
    property Items[XIndex, YIndex: Integer]: TFlowDirection read GetItems
      write SetItems; default;
    property Ordinals[XIndex, YIndex: Integer]: TFlowOrdinal read GetOrdinals write SetOrdinals;
  end;

//  TFlowDirections = array of array of TFlowDirection;

  TPointList = TList<TPoint>;

  TGeoStream = class(TPointList)
    StreamNumber: integer;
    DownstreamNumber: integer;
    procedure CutOffCorners;
  end;

  TStreamObjectList = class(TObjectList<TGeoStream>)
    procedure CutOffCorners;
  end;

  TCustomGenericRaster = class abstract(TInterfacedObject, IRaster)
    FLowerLeft: TPoint2D;
    FXCount: integer;
    FYCount: integer;
    FXSpacing: Double;
    FYSpacing: Double;
  protected
    FZ: array of array of Double;
    FIgnore: array of array of boolean;
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: integer): Double;
    function GetIgnore(XIndex, YIndex: integer): boolean;
  public
    constructor Create(AnXCount, AYCount: integer);
    destructor Destroy; override;
  end;

  TGenericRaster = class(TCustomGenericRaster)
  private
    procedure SetZ(XIndex, YIndex: integer; const Value: Double);
  public
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: integer read GetXCount;
    property YCount: integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Z[XIndex, YIndex: integer]: Double read GetZ write SetZ;
    property Ignore[XIndex, YIndex: integer]: boolean read GetIgnore;
    constructor Create(ARaster: IRaster);
  end;

  TTempSurferRaster7File = class(TClippedSurferRaster)
  private
    FNewFileName: string;
  public
    constructor Create(ARaster: IRasterFile);
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, TempFiles;

{ TFlowDirection }

procedure TFlowDirection.SetDeltaX(const Value: integer);
begin
  Assert((Value >= -1) and (Value <= 1));
  FDeltaX := Value;
end;

procedure TFlowDirection.SetDeltaY(const Value: Integer);
begin
  Assert((Value >= -1) and (Value <= 1));
  FDeltaY := Value;
end;


destructor TCustomGenericRaster.Destroy;
begin
//  Beep;
  inherited;
end;

function TCustomGenericRaster.GetIgnore(XIndex, YIndex: integer): boolean;
begin
  result := FIgnore[XIndex, YIndex]
end;

function TCustomGenericRaster.GetLowerLeft: TPoint2D;
begin
  result := FLowerLeft;
end;

function TCustomGenericRaster.GetXCount: integer;
begin
  result := FXCount;
end;

function TCustomGenericRaster.GetXSpacing: Double;
begin
  result := FXSpacing;
end;

function TCustomGenericRaster.GetYCount: integer;
begin
  result := FYCount;
end;

function TCustomGenericRaster.GetYSpacing: Double;
begin
  result := FYSpacing;
end;

function TCustomGenericRaster.GetZ(XIndex, YIndex: integer): Double;
begin
  result := FZ[XIndex, YIndex];
end;

constructor TCustomGenericRaster.Create(AnXCount, AYCount: integer);
begin
  FXCount := AnXCount;
  FYCount := AYCount;
  SetLength(FZ, FXCount, FYCount);
  SetLength(FIgnore, FXCount, FYCount);
end;

constructor TGenericRaster.Create(ARaster: IRaster);
var
  XIndex: integer;
  YIndex: integer;
begin
  inherited Create(ARaster.XCount, ARaster.YCount);
  FLowerLeft := ARaster.LowerLeft;
  FXSpacing := ARaster.XSpacing;
  FYSpacing := ARaster.YSpacing;
  for XIndex := 0 to FXCount - 1 do
  begin
    for YIndex := 0 to FYCount - 1 do
    begin
      FZ[XIndex, YIndex] := ARaster.Z[XIndex, YIndex];
      FIgnore[XIndex, YIndex] := ARaster.Ignore[XIndex, YIndex];
    end;
  end;
end;

procedure TGenericRaster.SetZ(XIndex, YIndex: integer; const Value: Double);
begin
  FZ[XIndex, YIndex] := Value;
end;

{ TFlowDirections }

constructor TFlowDirections.Create(XCount, YCount: Integer);
begin
  FXCount := XCount;
  FYCount := YCount;
  SetOrdinalArraySize;
end;

constructor TFlowDirections.Create(RasterInterface: IRasterFlush);
begin
  FRasterInterface := RasterInterface;
  Create(FRasterInterface.XCount, FRasterInterface.YCount);

end;

procedure TFlowDirections.ReadOrdinalsFromRaster;
var
  YIndex: Integer;
  XIndex: Integer;
begin
  if (Length(FOrdinals) = 0) then
  begin
    Assert(FRasterInterface <> nil);
    SetOrdinalArraySize;
    for YIndex := 0 to FRasterInterface.YCount - 1 do
    begin
      for XIndex := 0 to FRasterInterface.XCount - 1 do
      begin
        if FRasterInterface.Ignore[XIndex, YIndex] then
        begin
          Ordinals[XIndex, YIndex] := foMiddle;
        end
        else
        begin
          Ordinals[XIndex, YIndex] := TFlowOrdinal(Round(FRasterInterface.Z[XIndex, YIndex]));
        end;
      end;
    end;
    FModified := False;
  end;
end;

procedure TFlowDirections.SetOrdinalArraySize;
begin
  SetLength(FOrdinals, FXCount, FYCount);
end;

function TFlowDirections.GetItems(XIndex, YIndex: Integer): TFlowDirection;
begin
  ReadOrdinalsFromRaster;
  Result := FlowValues[FOrdinals[XIndex, YIndex]];
end;

function TFlowDirections.GetOrdinals(XIndex, YIndex: Integer): TFlowOrdinal;
begin
  ReadOrdinalsFromRaster;
  result := FOrdinals[XIndex, YIndex];
end;

procedure TFlowDirections.SetItems(XIndex, YIndex: Integer;
  const Value: TFlowDirection);
begin
  FOrdinals[XIndex, YIndex] := FlowOrdinals[Value.DeltaY, Value.DeltaX];
  FModified := True;
end;

procedure TFlowDirections.SetOrdinals(XIndex, YIndex: Integer;
  const Value: TFlowOrdinal);
begin
  FOrdinals[XIndex, YIndex] := Value;
  FModified := True;
end;

procedure TFlowDirections.WriteOrdinalsToRaster;
var
  YIndex: Integer;
  XIndex: Integer;
  Z: Integer;
begin
  if FModified then
  begin
    for YIndex := 0 to FRasterInterface.YCount - 1 do
    begin
      for XIndex := 0 to FRasterInterface.XCount - 1 do
      begin
        if not FRasterInterface.Ignore[XIndex, YIndex] then
        begin
          Z := Ord(Ordinals[XIndex, YIndex]);
          FRasterInterface.Z[XIndex, YIndex] := Z;
        end;
      end;
    end;
    FRasterInterface.Flush;
    FModified := False;
  end;
end;

{ TTempSurferRaster7File }

constructor TTempSurferRaster7File.Create(ARaster: IRasterFile);
var
  ClippedRaster: TClippedSurferRaster;
begin
  FNewFileName := TempFileName;
  TFile.Delete(FNewFileName);
  TFile.Copy(ARaster.FileName, FNewFileName);
  inherited Create(FNewFileName);
  if ARaster is TClippedSurferRaster then
  begin
    ClippedRaster := TClippedSurferRaster(ARaster);
    IncludeShapeFileName := ClippedRaster.IncludeShapeFileName;
    ExcludeShapeFileNames := ClippedRaster.ExcludeShapeFileNames;
//    if ClippedRaster.ExcludeShapeFileNames.Count > 0 then
//    begin
//      ExcludeShapeFiles(ClippedRaster.ExcludeShapeFileNames);
//    end;
  end;
end;

destructor TTempSurferRaster7File.Destroy;
begin
  inherited;
  if TFile.Exists(FNewFileName) then
  begin
    TFile.Delete(FNewFileName)
  end;
end;

{ TGeoStream }

procedure TGeoStream.CutOffCorners;
var
  PointIndex: Integer;
  Point1: TPoint;
  Point2: TPoint;
begin
  for PointIndex := Count - 2 downto 1 do
  begin
    Point1 := Items[PointIndex-1];
    Point2 := Items[PointIndex+1];
    if (Abs(Point1.X - Point2.X) <=1)
      and (Abs(Point1.Y - Point2.Y) <=1) then
    begin
      Delete(PointIndex);
    end;
  end;
end;

{ TStreamObjectList }

procedure TStreamObjectList.CutOffCorners;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].CutOffCorners;
  end;
end;

end.
