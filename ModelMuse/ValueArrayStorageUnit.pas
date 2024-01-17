unit ValueArrayStorageUnit;

interface

uses Classes, RbwParser, System.Generics.Collections;

type
  TValueArrayStorage = class(TPersistent)
  private
    FIntValues: array of integer;
    FRealValues: array of double;
    FBooleanValues: array of boolean;
    FStringValues: TStringList;
    FDataType: TRbwDataType;
    FCached: boolean;
    FCleared: boolean;
    FTempFileName: string;
    FErrorRestoringData: Boolean;
    function GetBooleanValues(Index: integer): boolean;
    function GetCount: integer;
    function GetIntValues(Index: integer): integer;
    function GetRealValues(Index: integer): double;
    function GetStringValues(Index: integer): string;
    procedure SetBooleanValues(Index: integer; const Value: boolean);
    procedure SetCount(const Value: integer);
    procedure SetDataType(const Value: TRbwDataType);
    procedure SetIntValues(Index: integer; const Value: integer);
    procedure SetRealValues(Index: integer; const Value: double);
    procedure SetStringValues(Index: integer; Value: string);
    procedure ReadValues(Reader: TReader);
    procedure WriteValues(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property IntValues[Index: integer]: integer read GetIntValues
      write SetIntValues;
    property RealValues[Index: integer]: double read GetRealValues
      write SetRealValues;
    property BooleanValues[Index: integer]: boolean read GetBooleanValues
      write SetBooleanValues;
    property StringValues[Index: integer]: string read GetStringValues
      write SetStringValues;
    procedure Add; overload;
    procedure Add(Value: double); overload;
    procedure Add(Value: integer); overload;
    procedure Add(Value: boolean); overload;
    procedure Add(const Value: string); overload;
    procedure Delete(Index: integer);
    procedure Insert(Index: integer);
    procedure Clear;
    procedure CacheData;
    procedure RestoreData;
    procedure Reverse;
    function UniformValues: boolean;
    function IsSame(OtherStorage: TValueArrayStorage): Boolean;
  published
    property DataType: TRbwDataType read FDataType write SetDataType;
    property Count: integer read GetCount write SetCount;
    property ErrorRestoringData: Boolean read FErrorRestoringData;
  end;

  TValueArrayItem = class(TCollectionItem)
  private
    FName: string;
    FValues: TValueArrayStorage;
    procedure SetName(const Value: string);
    procedure SetValues(const Value: TValueArrayStorage);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure CacheData;
  published
    property Name: string read FName write SetName;
    property Values: TValueArrayStorage read FValues write SetValues;
  end;

  TValueArrayItemList = TList<TValueArrayItem>;

  TValueCollection = class(TCollection)
  private
    FCachedName: string;
    FCachedIndex: integer;
    FErrorRestoringData: Boolean;
    function GetItem(Index: Integer): TValueArrayItem;
    procedure SetItem(Index: Integer; const Value: TValueArrayItem);
  public
    procedure CacheData;
    procedure RestoreData;
    constructor Create;
    property Items[Index: Integer]: TValueArrayItem read GetItem write SetItem; default;
    function ValuesByName(AName: string): TValueArrayStorage;
    function ValueItemByName(AName: string): TValueArrayItem;
    property ErrorRestoringData: Boolean read FErrorRestoringData;
    function Add: TValueArrayItem;
  end;

implementation

uses SysUtils, ZLib, TempFiles;

{ TValueArrayStorage }

procedure TValueArrayStorage.ReadValues(Reader: TReader);
var
  Index: Integer;
begin
  Reader.ReadListBegin;
  Count := Reader.ReadInteger;
  for Index := 0 to Count - 1 do
  begin
    case DataType of
      rdtDouble: RealValues[Index] := Reader.ReadFloat;
      rdtInteger: IntValues[Index] := Reader.ReadInteger;
      rdtBoolean: BooleanValues[Index] := Reader.ReadBoolean;
      rdtString: StringValues[Index] := Reader.ReadString;
      else Assert(False);
    end;
  end;
  Reader.ReadListEnd;
  CacheData;
end;

procedure TValueArrayStorage.RestoreData;
var
  DecompressionStream: TDecompressionStream;
  LocalCount: integer;
  ValueLength: Integer;
  Index: Integer;
  StringValue: string;
  CachedData: TMemoryStream;
  SizeToRestore: Int64;
  MaxItemsToRestore: Integer;
  MaxSizeToRestore: Integer;
  PositionToRestore: Integer;
begin
  FErrorRestoringData := False;
  if not FCached or not FCleared then
  begin
    Exit;
  end;
  CachedData := TMemoryStream.Create;
  try
    CachedData.Position := 0;
    ExtractAFile(FTempFileName, CachedData);
    DecompressionStream := TDecompressionStream.Create(CachedData);
    try
      DecompressionStream.Read(LocalCount, SizeOf(LocalCount));
      if LocalCount <= 0 then
      begin
        FErrorRestoringData := True;
        Exit;
      end;
      Count := LocalCount;
      if LocalCount > 0 then
      begin
        case DataType of
          rdtDouble:
            begin
              SizeToRestore := LocalCount*SizeOf(double);
              MaxItemsToRestore := High(LongInt) div SizeOf(double);
              MaxSizeToRestore := MaxItemsToRestore*SizeOf(double);
              PositionToRestore := 0;
              while SizeToRestore > MaxSizeToRestore do
              begin
                DecompressionStream.Read(FRealValues[PositionToRestore], MaxSizeToRestore);
                Inc(PositionToRestore, MaxItemsToRestore);
                SizeToRestore := SizeToRestore - MaxSizeToRestore
              end;
              if SizeToRestore > 0 then
              begin
                DecompressionStream.Read(FRealValues[PositionToRestore], SizeToRestore);
              end;
            end;
          rdtInteger:
            begin
              DecompressionStream.Read(FIntValues[0], LocalCount*SizeOf(integer));
            end;
          rdtBoolean:
            begin
              DecompressionStream.Read(FBooleanValues[0], LocalCount*SizeOf(boolean));
            end;
          rdtString:
            begin
              for Index := 0 to LocalCount - 1 do
              begin
                DecompressionStream.Read(ValueLength, SizeOf(ValueLength));
                SetString(StringValue, nil, ValueLength);
                DecompressionStream.Read(Pointer(StringValue)^, ValueLength * SizeOf(Char));
                FStringValues[Index] := StringValue;
              end;
            end;
        end;
      end;
    finally
      DecompressionStream.Free;
    end;
  finally
    CachedData.Free;
  end;
  FCleared := False;
end;

procedure TValueArrayStorage.Reverse;
var
  Index: Integer;
  TempR: Double;
  TempI: Integer;
  TempB: Boolean;
  TempS: string;
begin
  for Index := 0 to Count div 2 - 1 do
  begin
    case DataType of
      rdtDouble:
        begin
          TempR := RealValues[Index];
          RealValues[Index] := RealValues[Count - 1 - Index];
          RealValues[Count - 1 - Index] := TempR;
        end;
      rdtInteger:
        begin
          TempI := IntValues[Index];
          IntValues[Index] := IntValues[Count - 1 - Index];
          IntValues[Count - 1 - Index] := TempI;
        end;
      rdtBoolean:
        begin
          TempB := BooleanValues[Index];
          BooleanValues[Index] := BooleanValues[Count - 1 - Index];
          BooleanValues[Count - 1 - Index] := TempB;
        end;
      rdtString:
        begin
          TempS := StringValues[Index];
          StringValues[Index] := StringValues[Count - 1 - Index];
          StringValues[Count - 1 - Index] := TempS;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TValueArrayStorage.WriteValues(Writer: TWriter);
var
  Index: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteInteger(Count);
  for Index := 0 to Count - 1 do
  begin
    case DataType of
      rdtDouble: Writer.WriteFloat(RealValues[Index]);
      rdtInteger: Writer.WriteInteger(IntValues[Index]);
      rdtBoolean: Writer.WriteBoolean(BooleanValues[Index]);
      rdtString: Writer.WriteString(StringValues[Index]);
      else Assert(False);
    end;
  end;
  Writer.WriteListEnd;
  CacheData;
end;

procedure TValueArrayStorage.Add;
begin
  Count := Count + 1;
end;

procedure TValueArrayStorage.Add(Value: integer);
begin
  Assert(DataType = rdtInteger);
  Add;
  IntValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(Value: double);
begin
  Assert(DataType = rdtDouble);
  Add;
  RealValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(const Value: string);
begin
  Assert(DataType = rdtString);
  Add;
  StringValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(Value: boolean);
begin
  Assert(DataType = rdtBoolean);
  Add;
  BooleanValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Assign(Source: TPersistent);
var
  SourceStorage: TValueArrayStorage;
begin
  if Source = self then Exit;

  if Source is TValueArrayStorage then
  begin
    SourceStorage := TValueArrayStorage(Source);
    Count := 0;
    DataType := SourceStorage.DataType;
    if SourceStorage.FCached and SourceStorage.FCleared
      and (SourceStorage.FTempFileName <> '') then
    begin
      FCached := True;
      FCleared := True;
      FTempFileName := SourceStorage.FTempFileName;
    end
    else
    begin
      Count := SourceStorage.Count;
      case DataType of
        rdtDouble: FRealValues := SourceStorage.FRealValues;
        rdtInteger: FIntValues := SourceStorage.FIntValues;
        rdtBoolean: FBooleanValues := SourceStorage.FBooleanValues;
        rdtString: FStringValues.Assign(SourceStorage.FStringValues);
        else Assert(False);
      end;
      Count := SourceStorage.Count;
      CacheData;
      SourceStorage.CacheData;
    end;
  end
  else
  begin
    inherited
  end;
end;

procedure TValueArrayStorage.CacheData;
var
  Compressor: TCompressionStream;
  LocalCount: Integer;
  Index: Integer;
  StringValue: string;
  ValueLength: Integer;
  CachedData: TMemoryStream;
  SizeToSave: Int64;
  MaxItemsToSave: Integer;
  MaxSizeToSave: Integer;
  PositionToSave: Integer;
begin
  if FCached then
  begin
    Clear;
    FCached := True;
    Exit;
  end;
  if (Count = 0) then
  begin
    Exit;
  end;
  CachedData := TMemoryStream.Create;
  try
    if FTempFileName = '' then
    begin
      FTempFileName := TempFileName;
    end;
    Compressor := TCompressionStream.Create(clDefault, CachedData);
    try
      CachedData.Position := 0;
      LocalCount := Count;
      Compressor.Write(LocalCount, SizeOf(LocalCount));
      case DataType of
        rdtDouble:
          begin
            SizeToSave := Int64(LocalCount)*SizeOf(double);
            MaxItemsToSave := High(LongInt) div SizeOf(double);
            MaxSizeToSave := MaxItemsToSave*SizeOf(double);
            PositionToSave := 0;
            While SizeToSave > MaxSizeToSave do
            begin
              Compressor.Write(FRealValues[PositionToSave], MaxSizeToSave);
              Inc(PositionToSave, MaxItemsToSave);
              SizeToSave := SizeToSave - MaxSizeToSave;
            end;
            if SizeToSave > 0 then
            begin
              Compressor.Write(FRealValues[PositionToSave], SizeToSave);
            end;
          end;
        rdtInteger:
          begin
//            SizeToSave := LocalCount*SizeOf(integer);
            Compressor.Write(FIntValues[0], LocalCount*SizeOf(integer));
          end;
        rdtBoolean:
          begin
//            SizeToSave := LocalCount*SizeOf(boolean);
            Compressor.Write(FBooleanValues[0], LocalCount*SizeOf(boolean));
          end;
        rdtString:
          begin
            for Index := 0 to FStringValues.Count - 1 do
            begin
              StringValue := FStringValues[Index];
              ValueLength := Length(StringValue);
              Compressor.Write(ValueLength, SizeOf(ValueLength));
              Compressor.WriteBuffer(Pointer(StringValue)^,
                ByteLength(StringValue));
            end;
          end;
      end;

    finally
      Compressor.Free;
    end;
    CachedData.Position := 0;
    ZipAFile(FTempFileName, CachedData);
  finally
    CachedData.Free;
  end;
  Clear;
  FCached := True;
end;

procedure TValueArrayStorage.Clear;
begin
  Count := 0;
  FCleared := True;
end;

constructor TValueArrayStorage.Create;
begin
  FStringValues := TStringList.Create;
  FCached := False;
end;

procedure TValueArrayStorage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Values', ReadValues,
    WriteValues, True);
end;

procedure TValueArrayStorage.Delete(Index: integer);
var
  LocalCount: integer;
begin
  Assert(Index >= 0);
  LocalCount := Count;
  Assert(Index < LocalCount);
  if Index < LocalCount -1 then
  begin
    case DataType of
      rdtDouble:
        begin
          Move(FRealValues[Index+1], FRealValues[Index],
            (LocalCount-Index-1)*SizeOf(Double));
        end;
      rdtInteger:
        begin
          Move(FIntValues[Index+1], FIntValues[Index],
            (LocalCount-Index-1)*SizeOf(integer));
        end;
      rdtBoolean:
        begin
          Move(FBooleanValues[Index+1], FBooleanValues[Index],
            (LocalCount-Index-1)*SizeOf(boolean));
        end;
      rdtString:
        begin
          FStringValues.Delete(Index);
        end;
      else Assert(False);
    end;
  end;
  Count := LocalCount -1;
end;

destructor TValueArrayStorage.Destroy;
begin
  FStringValues.Free;
  inherited;
end;

function TValueArrayStorage.GetBooleanValues(Index: integer): boolean;
begin
  RestoreData;
  result := FBooleanValues[Index];
end;

function TValueArrayStorage.GetCount: integer;
begin
  RestoreData;
  result := 0;
  case DataType of
    rdtDouble: result := length(FRealValues);
    rdtInteger: result := length(FIntValues);
    rdtBoolean: result := length(FBooleanValues);
    rdtString: result := FStringValues.Count;
    else Assert(False);
  end;
end;

function TValueArrayStorage.GetIntValues(Index: integer): integer;
begin
  RestoreData;
  result := FIntValues[Index];
end;

function TValueArrayStorage.GetRealValues(Index: integer): double;
begin
  RestoreData;
  result := FRealValues[Index];
end;

function TValueArrayStorage.GetStringValues(Index: integer): string;
begin
  RestoreData;
  result := FStringValues[Index];
end;

procedure TValueArrayStorage.Insert(Index: integer);
var
  LocalCount: integer;
begin
  Assert(Index >= 0);
  LocalCount := Count;
  Assert(Index <= LocalCount);
  Count := Count +1;
  if Index <= LocalCount -1 then
  begin
    case DataType of
      rdtDouble:
        begin
          Move(FRealValues[Index], FRealValues[Index+1],
            (LocalCount-Index)*SizeOf(Double));
        end;
      rdtInteger:
        begin
          Move(FIntValues[Index], FIntValues[Index+1],
            (LocalCount-Index)*SizeOf(integer));
        end;
      rdtBoolean:
        begin
          Move(FBooleanValues[Index], FBooleanValues[Index+1],
            (LocalCount-Index)*SizeOf(boolean));
        end;
      rdtString:
        begin
          FStringValues.Insert(Index, '');
        end;
      else Assert(False);
    end;
  end;
end;

function TValueArrayStorage.IsSame(OtherStorage: TValueArrayStorage): Boolean;
var
  index: Integer;
begin
  result := (DataType = OtherStorage.DataType) and (Count = OtherStorage.Count);
  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      case DataType of
        rdtDouble: result := RealValues[index] = OtherStorage.RealValues[index];
        rdtInteger: result := IntValues[index] = OtherStorage.IntValues[index];
        rdtBoolean: result := BooleanValues[index] = OtherStorage.BooleanValues[index];
        rdtString: result := StringValues[index] = OtherStorage.StringValues[index];
      end;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TValueArrayStorage.SetBooleanValues(Index: integer;
  const Value: boolean);
begin
  RestoreData;
  FBooleanValues[Index] := Value;
  FCached := False;
end;

procedure TValueArrayStorage.SetCount(const Value: integer);
begin
  Assert(Value >= 0);
  case DataType of
    rdtDouble: SetLength(FRealValues, Value);
    rdtInteger: SetLength(FIntValues, Value);
    rdtBoolean: SetLength(FBooleanValues, Value);
    rdtString:
      begin
        While (FStringValues.Count > Value) do
        begin
          FStringValues.Delete(FStringValues.Count-1);
        end;
        While (FStringValues.Count < Value) do
        begin
          FStringValues.Add('');
        end;
      end
    else Assert(False);
  end;
  FCached := False;
end;

procedure TValueArrayStorage.SetDataType(const Value: TRbwDataType);
var
  TempCount: integer;
begin
  if FDataType <> Value then
  begin
    TempCount := Count;
    Count := 0;
    FDataType := Value;
    Count := TempCount;
    FCached := False;
  end;
end;

procedure TValueArrayStorage.SetIntValues(Index: integer; const Value: integer);
begin
  RestoreData;
  FIntValues[Index] := Value;
  FCached := False;
end;

procedure TValueArrayStorage.SetRealValues(Index: integer;
  const Value: double);
begin
  RestoreData;
  FRealValues[Index] := Value;
  FCached := False;
end;

procedure TValueArrayStorage.SetStringValues(Index: integer;
  Value: string);
begin
  if Length(Value) > 0 then
  begin
    if Value[1] = '"' then
    begin
      Value := Copy(Value, 2, MAXINT);
    end;
  end;
  if Length(Value) > 0 then
  begin
    if Value[Length(Value)] = '"' then
    begin
      Value := Copy(Value, 1, Length(Value)-1);
    end;
  end;
  RestoreData;
  FStringValues[Index] := Value;
  FCached := False;
end;

function TValueArrayStorage.UniformValues: boolean;
var
  FirstReal: Double;
  FirstInteger: Integer;
  FirstBool: Boolean;
  FirstString: string;
  index: Integer;
begin
  if Count = 0 then
  begin
    result := False;
  end
  else
  begin
    result := True;

    case DataType of
      rdtDouble:
        begin
          FirstReal := RealValues[0];
          for index := 0 to Count - 1 do
          begin
            if FirstReal <> RealValues[index] then
            begin
              result := False;
              Exit;
            end;
          end;
        end;
      rdtInteger:
        begin
          FirstInteger := IntValues[0];
          for index := 0 to Count - 1 do
          begin
            if FirstInteger <> IntValues[index] then
            begin
              result := False;
              Exit;
            end;
          end;
        end;
      rdtBoolean:
        begin
          FirstBool := BooleanValues[0];
          for index := 0 to Count - 1 do
          begin
            if FirstBool <> BooleanValues[index] then
            begin
              result := False;
              Exit;
            end;
          end;
        end;
      rdtString:
        begin
          FirstString := StringValues[0];
          for index := 0 to Count - 1 do
          begin
            if FirstString <> StringValues[index] then
            begin
              result := False;
              Exit;
            end;
          end;
        end;
    end;
  end;
end;

{ TValueArrayItem }

procedure TValueArrayItem.Assign(Source: TPersistent);
var
  SourceValues: TValueArrayItem;
begin
  if Source is TValueArrayItem then
  begin
    SourceValues := TValueArrayItem(Source);
    Name := SourceValues.Name;
    Values := SourceValues.Values;
  end
  else
  begin
    inherited;
  end;
end;

procedure TValueArrayItem.CacheData;
begin
  FValues.CacheData;
end;

constructor TValueArrayItem.Create(Collection: TCollection);
begin
  inherited;
  FValues := TValueArrayStorage.Create;
end;

destructor TValueArrayItem.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TValueArrayItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TValueArrayItem.SetValues(const Value: TValueArrayStorage);
begin
  FValues.Assign(Value);
end;

{ TValueCollection }

function TValueCollection.Add: TValueArrayItem;
begin
  result := inherited Add as TValueArrayItem;
end;

procedure TValueCollection.CacheData;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].CacheData;
  end;
end;

constructor TValueCollection.Create;
begin
  inherited Create(TValueArrayItem);
  FCachedIndex := -1;
end;

function TValueCollection.GetItem(Index: Integer): TValueArrayItem;
begin
  result := inherited Items[Index] as TValueArrayItem;
end;

procedure TValueCollection.RestoreData;
var
  Index: Integer;
begin
  FErrorRestoringData := False;
  for Index := 0 to Count - 1 do
  begin
    Items[Index].FValues.RestoreData;
    FErrorRestoringData :=
      FErrorRestoringData or Items[Index].FValues.ErrorRestoringData;
  end;
end;

procedure TValueCollection.SetItem(Index: Integer;
  const Value: TValueArrayItem);
begin
  inherited Items[Index] := Value;
end;

function TValueCollection.ValueItemByName(AName: string): TValueArrayItem;
var
  Index: Integer;
begin
  result := nil;

  if (FCachedName <> '') and SameText(FCachedName, AName) then
  begin
    result := Items[FCachedIndex];
    Exit;
  end;

  for Index := 0 to Count - 1 do
  begin
    if SameText(Items[Index].Name, AName) then
    begin
      result := Items[Index];
      FCachedName := AName;
      FCachedIndex := Index;
      break;
    end;
  end;
end;

function TValueCollection.ValuesByName(AName: string): TValueArrayStorage;
var
  Index: Integer;
begin
  result := nil;

  if (FCachedName <> '') and SameText(FCachedName, AName) then
  begin
    result := Items[FCachedIndex].Values;
    Exit;
  end;

  for Index := 0 to Count - 1 do
  begin
    if SameText(Items[Index].Name, AName) then
    begin
      result := Items[Index].Values;
      FCachedName := AName;
      FCachedIndex := Index;
      break;
    end;
  end;
end;

initialization
  RegisterClass(TValueArrayStorage);

end.
