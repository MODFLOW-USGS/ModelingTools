unit CacheableSparseDataSets;

interface

uses Classes, SparseDataSets, SysUtils;

type
  T3DSparseCacheableRealArray = class(T3DSparseRealArray)
  private
    FCached: Boolean;
    FTempFileName: string;
    FCleared: Boolean;
    procedure StoreData(Compressor: TStream);
    procedure Restore;
    procedure ReadData(DecompressionStream: TStream);
  protected
    procedure SetItems(const Layer, Row, Col: NativeInt;
      const Value: double); override;
  public
    procedure Cache;
    procedure CheckRestore;
    destructor Destroy; override;
  end;

implementation

uses
  TempFiles, ZLib;

{ T3DSparseCacheableRealArray }

procedure T3DSparseCacheableRealArray.Cache;
var
  Compressor: TCompressionStream;
  MemStream: TMemoryStream;
  TempStream: TMemoryStream;
begin
  if not FCached then
  begin
    if FTempFileName = '' then
    begin
      FTempFileName := TempFileName;
    end;
    MemStream := TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(clDefault, MemStream);
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
      MemStream.Free
    end;
    FCached := True;
  end;
  Clear;
  FCleared := True;
end;

procedure T3DSparseCacheableRealArray.CheckRestore;
begin
  if FCached and FCleared then
  begin
    Restore;
  end;
end;

destructor T3DSparseCacheableRealArray.Destroy;
begin
  if FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
  end;
  inherited;
end;

procedure T3DSparseCacheableRealArray.ReadData(DecompressionStream: TStream);
var
  ColIndex: NativeInt;
  RowIndex: NativeInt;
  LayerIndex: NativeInt;
  Index: NativeInt;
  AValue: double;
  ACount: NativeInt;
begin
  DecompressionStream.Read(ACount, SizeOf(ACount));
  if ACount > 0 then
  begin
    for Index := 0 to ACount - 1 do
    begin
      DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
      DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
      DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(AValue, SizeOf(AValue));
      Items[LayerIndex, RowIndex, ColIndex] := AValue;
    end;
  end;
end;

procedure T3DSparseCacheableRealArray.Restore;
var
  MemStream: TMemoryStream;
  DecompressionStream: TDecompressionStream;
begin
  Assert(FCached);
  Assert(FCleared);
  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    DecompressionStream := TDecompressionStream.Create(MemStream);
  try
    ReadData(DecompressionStream);
    FCached := True;
  finally
    DecompressionStream.Free;
  end;
  finally
    MemStream.Free;
  end;
  FCleared := False;
end;

procedure T3DSparseCacheableRealArray.SetItems(const Layer, Row, Col: NativeInt;
  const Value: double);
begin
  FCached := False;
  inherited;
end;

procedure T3DSparseCacheableRealArray.StoreData(Compressor: TStream);
var
  LayerIndex: NativeInt;
  RowIndex: NativeInt;
  ColIndex: NativeInt;
  AValue: Double;
begin
  Compressor.Write(FCount, SizeOf(FCount));
  if FCount > 0 then
  begin
    for LayerIndex := FMinLayer to FMaxLayer do
    begin
      for RowIndex := FMinRow to FMaxRow do
      begin
        for ColIndex := FMinCol to FMaxCol do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            AValue := Items[LayerIndex, RowIndex, ColIndex];
            Compressor.Write(LayerIndex, SizeOf(LayerIndex));
            Compressor.Write(RowIndex, SizeOf(RowIndex));
            Compressor.Write(ColIndex, SizeOf(ColIndex));
            Compressor.Write(AValue, SizeOf(AValue));
          end;
        end;
      end;
    end;
  end;
end;

end.
