unit RasterSorter;

interface

uses
  System.Types, System.Classes, RasterValuesAlongSegmentsUnit, StreamSorterUnit,
  System.SysUtils, System.Generics.Defaults, System.Math;

type
  TRasterSorter = class(TObject)
  private
    FRaster: IRaster;
    FTempFileName: string;
    FPointStream: TFileStream;
    FSortStream: TSortStream<TPoint>;
    function GetCount: Int64;
    function GetSortedLocation(Index: Int64): TPoint;
  public
    constructor Create(ARaster: IRaster);
    destructor Destroy; override;
    property SortedLocations[Index: Int64]: TPoint read GetSortedLocation;
    property Count: Int64 read GetCount;
  end;

implementation

uses
  System.IOUtils;

{ TRasterSorter }

constructor TRasterSorter.Create(ARaster: IRaster);
var
  XIndex: Integer;
  APoint: TPoint;
  YIndex: Integer;
  Comparer: IComparer<TPoint>;
begin
  FRaster := ARaster;
  FTempFileName := TPath.GetTempFileName;
  FPointStream := TFileStream.Create(FTempFileName, fmOpenReadWrite);
  for YIndex := 0 to FRaster.YCount - 1 do
  begin
    APoint.Y := YIndex;
    for XIndex := 0 to FRaster.XCount - 1 do
    begin
      APoint.X := XIndex;
      if not FRaster.Ignore[XIndex,YIndex] then
      begin
        FPointStream.Write(APoint, SizeOf(APoint))
      end;
    end;
  end;
  FSortStream := TSortStream<TPoint>.Create(FPointStream);
  Comparer := TComparer<TPoint>.Construct(
    function (const Left, Right: TPoint): Integer
    begin
      result := Sign(FRaster.Z[Left.X, Left.Y] - FRaster.Z[Right.X, Right.Y]);
    end);
  FSortStream.Sort(Comparer);
end;

destructor TRasterSorter.Destroy;
begin
  FSortStream.Free;
  FPointStream.Free;
  if TFile.Exists(FTempFileName) then
  begin
    TFile.Delete(FTempFileName);
  end;
  inherited;
end;

function TRasterSorter.GetCount: Int64;
begin
  result := FSortStream.Count;
end;

function TRasterSorter.GetSortedLocation(Index: Int64): TPoint;
begin
  result := FSortStream.Values[Index];
end;

end.
