unit PointCollectionUnit;

interface

uses
  System.Classes, ZLib, FastGEO;

type
  {@abstract(@name is used to store a TPoint2D.)}
  TPointItem = class(TCollectionItem)
  private
    // See @link(X).
    FX: double;
    // See @link(Y).
    FY: double;
    function GetPoint2D: TPoint2D;
    procedure SetPoint2D(const Value: TPoint2D);
  public
    property Point2D: TPoint2D read GetPoint2D write SetPoint2D;
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the X coordinate of the point.
    property X: double read FX write FX;
    // @name is the Y coordinate of the point.
    property Y: double read FY write FY;
  end;

  TSimplePointCollection = class(TCollection)
  private
    function GetPoint(Index: Integer): TPointItem;
    procedure SetPoint(Index: Integer; const Value: TPointItem);
    function GetPoint2D(Index: Integer): TPoint2D;
  public
    constructor Create;
    function Add: TPointItem;
    property Items[Index: Integer]: TPointItem read GetPoint write SetPoint; default;
    function IsSame(OtherCollection: TSimplePointCollection): Boolean;
    function Last: TPointItem;
    procedure AddPoint2D(APoint2d: TPoint2D);
    property Points[Index: Integer]: TPoint2D read GetPoint2D;
  end;

  {@abstract(@name is used to store a series of TPoint2Ds.)}
  TPointCollection = class(TCollection)
  private
    FTempFileName: string;
    FCaching: Boolean;
//    FCanCache: Boolean;
    procedure StoreData(Stream: TStream);
    procedure ReadData(DecompressionStream: TDecompressionStream); virtual;
    function GetCount: Integer;
  protected
    procedure Cache;
    procedure Restore;
  public
    property Count: Integer read GetCount;
    // @name creates an instance of @classname.
    // See @link(TPointItem).
    constructor Create;
    procedure EndUpdate; override;
  end;


implementation

uses
  TempFiles;

{ TPointItem }

procedure TPointItem.Assign(Source: TPersistent);
begin
  if Source is TPointItem then
  begin
    Point2D := TPointItem(Source).Point2D
  end
  else
  begin
    inherited;
  end;
end;

function TPointItem.GetPoint2D: TPoint2D;
begin
  result := EquatePoint(X, Y);
end;

procedure TPointItem.SetPoint2D(const Value: TPoint2D);
begin
  X := Value.x;
  Y := Value.y;
end;

{ TPointCollection }

procedure TPointCollection.Cache;
var
  MemStream: TMemoryStream;
  Compressor: TZCompressionStream;
  TempStream: TMemoryStream;
begin
  if FCaching then
  begin
    Exit;
  end;
  FCaching := True;
  try
    if FTempFileName = '' then
    begin
      FTempFileName := TempFileName;
    end;
    MemStream := TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(ZLib.clDefault, MemStream);
      TempStream := TMemoryStream.Create;
      try
        MemStream.Position := 0;
        StoreData(TempStream);
        TempStream.SaveToStream(Compressor);
      finally
        Compressor.Free;
        TempStream.Free;
      end;
      MemStream.Position := 0;
      ZipAFile(FTempFileName, MemStream);
    finally
      MemStream.Free;
    end;
    Clear;
  finally
    FCaching := False;
  end;
end;

constructor TPointCollection.Create;
begin
  inherited Create(TPointItem);
end;

procedure TPointCollection.EndUpdate;
begin
  inherited;
  if UpdateCount = 0 then
  begin
    Cache;
  end;
end;

function TPointCollection.GetCount: Integer;
begin
  if FTempFileName <> '' then
  begin
    Restore;
  end;
  result := inherited Count;
end;

procedure TPointCollection.ReadData(DecompressionStream: TDecompressionStream);
var
  LocalCount: Integer;
  index: Integer;
  Item: TPointItem;
  Value: double;
begin
  Clear;
  DecompressionStream.Read(LocalCount, SizeOf(LocalCount));
  Capacity := LocalCount;
  BeginUpdate;
  try
    for index := 0 to LocalCount - 1 do
    begin
      Item := Add as TPointItem;
      DecompressionStream.Read(Value, SizeOf(Value));
      Item.X := Value;
      DecompressionStream.Read(Value, SizeOf(Value));
      Item.Y := Value;
    end;
  finally
    inherited EndUpdate;
  end;
end;

procedure TPointCollection.Restore;
var
  MemStream: TMemoryStream;
  DecompressionStream: TZDecompressionStream;
begin
  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    DecompressionStream := TDecompressionStream.Create(MemStream);
    try
      ReadData(DecompressionStream);
    finally
      DecompressionStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TPointCollection.StoreData(Stream: TStream);
var
  LocalCount: Integer;
  index: Integer;
  Item: TPointItem;
begin
  LocalCount := inherited Count;
  Stream.Write(LocalCount, SizeOf(LocalCount));
  for Index := 0 to inherited Count - 1 do
  begin
    Item := Items[Index] as TPointItem;
    Stream.Write(Item.X, SizeOf(Item.X));
    Stream.Write(Item.Y, SizeOf(Item.Y));
  end;
end;

{ TSimplePointCollection }

function TSimplePointCollection.Add: TPointItem;
begin
  result := inherited Add as TPointItem;
end;

procedure TSimplePointCollection.AddPoint2D(APoint2d: TPoint2D);
begin
  Add.Point2d := APoint2d;
end;

constructor TSimplePointCollection.Create;
begin
  inherited Create(TPointItem);
end;

function TSimplePointCollection.GetPoint(Index: Integer): TPointItem;
begin
  result := inherited Items[Index] as TPointItem
end;

function TSimplePointCollection.GetPoint2D(Index: Integer): TPoint2D;
begin
  result := Items[Index].Point2D;
end;

function TSimplePointCollection.IsSame(
  OtherCollection: TSimplePointCollection): Boolean;
var
  ItemIndex: Integer;
  APoint: TPoint2D;
  OtherPoint: TPoint2D;
begin
  result := Count = OtherCollection.Count;
  if result then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      APoint := Items[ItemIndex].Point2D;
      OtherPoint := OtherCollection[ItemIndex].Point2D;
      result := (APoint.x = OtherPoint.x) and (APoint.y = OtherPoint.y);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TSimplePointCollection.Last: TPointItem;
begin
  result := Items[Count-1];
end;

procedure TSimplePointCollection.SetPoint(Index: Integer;
  const Value: TPointItem);
begin
  inherited Items[Index] := Value;
end;

end.
