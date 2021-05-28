unit ModflowCellUnit;

interface

uses Windows, SysUtils, Classes, Contnrs, ZLib, GoPhastTypes,
  System.Generics.Collections;

type
  // @name defines a cell location.
  //  @longcode(
  //  TCellLocation = record
  //    Layer: integer;
  //    Row: integer;
  //    Column: integer;
  //  end;
  //  )
  // @member(Layer Layer is the layer in the grid to for this boundary (Zero based).)
  // @member(Row Row is the row in the grid to for this boundary (Zero based).)
  // @member(Column Column is the column in the grid to for this boundary (Zero based).)
  TCellLocation = record
    Layer: integer;
    Row: integer;
    Column: integer;
    Section: integer;
    class operator Equal(ACell: TCellLocation; BCell: TCellLocation): boolean;
    class operator NotEqual(ACell: TCellLocation; BCell: TCellLocation): boolean;
  end;

  PCellLocation = ^TCellLocation;

  TCellLocationArray = array of TCellLocation;
  TCellLocationList = TList<TCellLocation>;

  TValueCell = class(TObject)
  private
    FIFace: TIface;
    FScreenObject: TObject;
    FBoundaryIndex: integer;
    FMf6ObsName: string;
    procedure SetScreenObject(const Value: TObject);
  protected
    function GetColumn: integer; virtual; abstract;
    function GetLayer: integer; virtual; abstract;
    function GetRow: integer; virtual; abstract;
    procedure SetColumn(const Value: integer); virtual; abstract;
    procedure SetLayer(const Value: integer); virtual; abstract;
    procedure SetRow(const Value: integer); virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetIntegerValue(Index: integer;
      AModel: TBaseModel): integer; virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetRealValue(Index: integer;
      AModel: TBaseModel): double; virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetBooleanValue(Index: integer;
      AModel: TBaseModel): boolean; virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetRealAnnotation(Index: integer;
      AModel: TBaseModel): string; virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetIntegerAnnotation(Index: integer;
      AModel: TBaseModel): string; virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetBooleanAnnotation(Index: integer;
      AModel: TBaseModel): string; virtual;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); virtual;
    procedure Restore(Decomp: TDecompressionStream;
      Annotations: TStringList); virtual;
    function GetSection: integer; virtual; abstract;
    procedure RecordStrings(Strings: TStringList); virtual;
    function GetPestName(Index: Integer): string; virtual;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; virtual;
    function GetPestSeriesName(Index: Integer): string; virtual;
    function GetCellLocation: TCellLocation; virtual;
  public
    Constructor Create; virtual;
    // @name is used for MODFLOW 6 PEST observations.
    property BoundaryIndex: integer read FBoundaryIndex write FBoundaryIndex;
    // @name is the layer number for this cell. Valid values range from 0 to
    // the number of layers in the grid minus 1.
    property Layer: integer read GetLayer write SetLayer;
    // @name is the row number for this cell. Valid values range from 0 to
    // the number of rows in the grid minus 1.
    property Row: integer read GetRow write SetRow;
    // @name is the column number for this cell. Valid values range from 0 to
    // the number of columns in the grid minus 1.
    property Column: integer read GetColumn write SetColumn;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property IntegerValue[Index: integer; AModel: TBaseModel]: integer
      read GetIntegerValue;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property RealValue[Index: integer; AModel: TBaseModel]: double
      read GetRealValue;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property BooleanValue[Index: integer; AModel: TBaseModel]: boolean
      read GetBooleanValue;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property RealAnnotation[Index: integer; AModel: TBaseModel]: string
      read GetRealAnnotation;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property IntegerAnnotation[Index: integer; AModel: TBaseModel]: string
      read GetIntegerAnnotation;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property BooleanAnnotation[Index: integer; AModel: TBaseModel]: string
      read GetBooleanAnnotation;
    property IFace: TIface read FIFace write FIFace;
    // @name is the @link(TScreenObject) used to assign this
    // @classname.  @name is assigned in @link(TModflowBoundary.AssignCells).
    // @name is only assigned for boundary conditions that have
    // observations associated with them. (Drains, General head boundaries,
    // Rivers, and constant head cells.).
    property ScreenObject: TObject read FScreenObject write SetScreenObject;
    property Section: integer read GetSection;
    function IsIdentical(AnotherCell: TValueCell): boolean; virtual;
    function AreRealValuesIdentical(AnotherCell: TValueCell;
      DataIndex: integer): boolean;
    function AreIntegerValuesIdentical(AnotherCell: TValueCell;
      DataIndex: integer): boolean;
    function AreBooleanValuesIdentical(AnotherCell: TValueCell;
      DataIndex: integer): boolean;
    property Mf6ObsName: string read FMf6ObsName write FMf6ObsName;
    Property PestName[Index: Integer]: string read GetPestName;
    Property PestSeriesName[Index: Integer]: string read GetPestSeriesName;
    Property PestSeriesMethod[Index: Integer]: TPestParamMethod read GetPestSeriesMethod;
    // The section may not be correct in @name.
    // override GetCellLocation if the section needs to be correct.
    property CellLocation: TCellLocation read GetCellLocation;
  end;

  TValueCellType = class of TValueCell;

  TValueCellList = class(TObjectList)
  private
    FCachedCount: integer;
    FCleared: Boolean;
    FCached: Boolean;
    FValueCellType: TValueCellType;
    FTempFileNames: TStringList;
    FOldCachedCount: Integer;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetItem(Index: integer): TValueCell;
    procedure SetItem(Index: integer; const Value: TValueCell);
    procedure Restore;
    procedure ClearFileNames;
    procedure CheckRestore;
  public
    function Add(Item: TValueCell): integer;
    procedure Cache;
    Constructor Create(ValueCellType: TValueCellType);
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: TValueCell read GetItem
      write SetItem; default;
    Destructor Destroy; override;
    function AreRealValuesIdentical(AnotherList: TValueCellList;
      DataIndex: integer): boolean;
    function AreIntegerValuesIdentical(AnotherList: TValueCellList;
      DataIndex: integer): boolean;
    procedure Clear; override;
    function Last: TValueCell;
    function First: TValueCell;
    procedure InvalidateCache;
  end;

procedure WriteCompInt(Stream: TStream; Value: integer);
procedure WriteCompReal(Stream: TStream; Value: double);
procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
procedure WriteCompString(Stream: TStream; Value: string);
procedure WriteCompCell(Stream: TStream; Cell: TCellLocation);

function ReadCompInt(Stream: TStream): integer;
function ReadCompReal(Stream: TStream): double;
function ReadCompBoolean(Stream: TStream): Boolean;
function ReadCompStringSimple(Stream: TStream): string;
function ReadCompString(Stream: TStream; Annotations: TStringList): string;
function ReadCompCell(Stream: TStream): TCellLocation;

implementation

uses TempFiles, ScreenObjectUnit, frmGoPhastUnit;

const
  MaxCondensed = 100;

procedure WriteCompInt(Stream: TStream; Value: integer);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompInt(Stream: TStream): integer;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompReal(Stream: TStream; Value: double);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompReal(Stream: TStream): double;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompBoolean(Stream: TStream): Boolean;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompString(Stream: TStream; Value: string);
var
  StringLength: integer;
begin
  StringLength := Length(Value);
  WriteCompInt(Stream, StringLength);
  if StringLength > 0 then
  begin
    Stream.WriteBuffer(Pointer(Value)^, ByteLength(Value));
  end;
end;

function ReadCompStringSimple(Stream: TStream): string;
var
  StringLength: Integer;
begin
  Stream.Read(StringLength, SizeOf(StringLength));
  if StringLength > 0 then
  begin
    SetString(result, nil, StringLength);
    Stream.Read(Pointer(result)^, StringLength * SizeOf(Char));
  end
  else
  begin
    result := ''
  end;
end;

function ReadCompString(Stream: TStream; Annotations: TStringList): string;
var
  StringPostion: integer;
begin
  result := ReadCompStringSimple(Stream);
  StringPostion := Annotations.IndexOf(result);
  if StringPostion < 0 then
  begin
    Annotations.Add(result);
  end
  else
  begin
    result := Annotations[StringPostion]
  end;
end;

procedure WriteCompCell(Stream: TStream; Cell: TCellLocation);
begin
  Stream.Write(Cell, SizeOf(Cell));
end;

function ReadCompCell(Stream: TStream): TCellLocation;
begin
  Stream.Read(result, SizeOf(result));
end;


{ TValueCellList }

function TValueCellList.Add(Item: TValueCell): integer;
begin
//  CheckRestore;
  result := inherited Add(Item);
  FCached := False;
  FCleared := False;
end;

function TValueCellList.AreIntegerValuesIdentical(AnotherList: TValueCellList;
  DataIndex: integer): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherList.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].AreIntegerValuesIdentical(
        AnotherList.Items[Index], DataIndex);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TValueCellList.Clear;
begin
  ClearFileNames;
  FCached := False;
  FCleared := False;
  FCachedCount := 0;
  inherited;
end;

procedure TValueCellList.ClearFileNames;
var
  NameIndex: Integer;
begin
  for NameIndex := 0 to FTempFileNames.Count - 1 do
  begin
    FreeMemory(FTempFileNames[NameIndex]);
  end;
  FTempFileNames.Clear;
end;

function TValueCellList.AreRealValuesIdentical(AnotherList: TValueCellList;
  DataIndex: integer): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherList.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].AreRealValuesIdentical(
        AnotherList.Items[Index], DataIndex);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TValueCellList.Cache;
var
  Compressor: TCompressionStream;
  Index: Integer;
  Cell: TValueCell;
  LocalCount: integer;
  Strings: TStringList;
  StringIndex: Integer;
  TempFile: TMemoryStream;
  ATempFileName: string;
begin
  LocalCount := inherited Count;
  if (LocalCount > 0) and not FCached then
  begin
    ATempFileName := TempFileName;
    FTempFileNames.Add(ATempFileName);
    TempFile := TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(clDefault, TempFile);
      try
        FCachedCount := FCachedCount + LocalCount;

        Strings := TStringList.Create;
        try
          Strings.Sorted := True;
          Strings.Duplicates := dupIgnore;
          for Index := 0 to LocalCount - 1 do
          begin
            Cell := inherited Items[Index] as TValueCell;
            Cell.RecordStrings(Strings);
          end;

          WriteCompInt(Compressor, Strings.Count);
          for StringIndex := 0 to Strings.Count - 1 do
          begin
            WriteCompString(Compressor, Strings[StringIndex])
          end;

          WriteCompInt(Compressor, LocalCount);
          for Index := 0 to LocalCount - 1 do
          begin
            Cell := inherited Items[Index] as TValueCell;
            Cell.Cache(Compressor, Strings);
          end;
        finally
          Strings.Free;
        end;
      finally
        Compressor.Free;
      end;
      ZipAFile(ATempFileName, TempFile);
    finally
      TempFile.Free;
    end;
    FCached := True;
    FOldCachedCount := FCachedCount;
  end
  else if FCached then
  begin
    FCachedCount := FOldCachedCount;
  end;
  inherited Clear;
  FCleared := True;
end;

procedure TValueCellList.CheckRestore;
begin
  if FCached and FCleared and (FTempFileNames.Count > 0) then
  begin
    Restore;
  end;
end;

constructor TValueCellList.Create(ValueCellType: TValueCellType);
begin
  inherited Create;
  FValueCellType := ValueCellType;
  FTempFileNames := TStringList.Create;
  FCached := False;
end;

destructor TValueCellList.Destroy;
begin
  ClearFileNames;
  inherited;
  FTempFileNames.Free;
end;

function TValueCellList.First: TValueCell;
begin
  CheckRestore;
  result := inherited First as TValueCell
end;

function TValueCellList.GetCount: integer;
begin
  result := FCachedCount + inherited Count;
end;

function TValueCellList.GetItem(Index: integer): TValueCell;
begin
  CheckRestore;
  result := inherited Items[Index] as TValueCell;
end;

procedure TValueCellList.InvalidateCache;
begin
  CheckRestore;
  FCached := False;
  ClearFileNames;
end;

function TValueCellList.Last: TValueCell;
begin
  CheckRestore;
  result := inherited Last as TValueCell
end;

procedure TValueCellList.Restore;
var
  DecompressionStream: TDecompressionStream;
  ValueCell: TValueCell;
  LocalCount : integer;
  CellIndex: Integer;
  Annotations: TStringList;
  StringIndex: Integer;
  StringCount: Integer;
  TempFile: TMemoryStream;
  NameIndex: Integer;
  ATempFileName: string;
begin
  if not FCleared then
  begin
    Cache;
  end;
  Annotations := TStringList.Create;
  try
    if Capacity < FCachedCount then
    begin
      Capacity := FCachedCount;
    end;

    for NameIndex := 0 to FTempFileNames.Count - 1 do
    begin
      ATempFileName := FTempFileNames[NameIndex];
      TempFile := TMemoryStream.Create;
      ExtractAFile(ATempFileName, TempFile);
      DecompressionStream := TDecompressionStream.Create(TempFile);
      try
        StringCount := ReadCompInt(DecompressionStream);
        Annotations.Clear;
        Annotations.Capacity := StringCount;
        for StringIndex := 0 to StringCount - 1 do
        begin
          Annotations.Add(ReadCompStringSimple(DecompressionStream));
        end;

        LocalCount := ReadCompInt(DecompressionStream);
        for CellIndex := 0 to LocalCount - 1 do
        begin
          ValueCell := FValueCellType.Create;
          inherited Add(ValueCell);
          ValueCell.Restore(DecompressionStream, Annotations);
        end;
      finally
        DecompressionStream.Free;
        TempFile.Free;
      end;
    end;

//    ClearFileNames;
    FCleared := False;
    FOldCachedCount := FCachedCount;
    FCachedCount := 0;

  finally
    Annotations.Free;
  end;
end;

procedure TValueCellList.SetCount(const Value: integer);
begin
  if FCleared then
  begin
    Restore;
  end;
  inherited Count := Value;
end;

procedure TValueCellList.SetItem(Index: integer; const Value: TValueCell);
begin
  inherited Items[Index] := Value;
end;

{ TValueCell }

function TValueCell.AreBooleanValuesIdentical(AnotherCell: TValueCell;
  DataIndex: integer): boolean;
begin
  result := BooleanValue[DataIndex, nil] =
    AnotherCell.BooleanValue[DataIndex, nil];
end;

function TValueCell.AreIntegerValuesIdentical(AnotherCell: TValueCell;
  DataIndex: integer): boolean;
begin
  result := IntegerValue[DataIndex, nil] =
    AnotherCell.IntegerValue[DataIndex, nil];
end;

function TValueCell.AreRealValuesIdentical(AnotherCell: TValueCell;
  DataIndex: integer): boolean;
begin
  result := RealValue[DataIndex, nil] = AnotherCell.RealValue[DataIndex, nil];
end;

procedure TValueCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompInt(Comp, FBoundaryIndex);

  Comp.Write(FIface, SizeOf(FIface));
  if ScreenObject = nil then
  begin
    WriteCompString(Comp, '');
  end
  else
  begin
    WriteCompString(Comp, TScreenObject(ScreenObject).Name);
  end;
  WriteCompString(Comp, Mf6ObsName);
end;

constructor TValueCell.Create;
begin
  FMf6ObsName := '';
  inherited;
end;

function TValueCell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TValueCell.GetBooleanValue(Index: integer;
  AModel: TBaseModel): boolean;
begin
  result := False;
  Assert(False);
end;

function TValueCell.GetCellLocation: TCellLocation;
begin
  result.Layer := Layer;
  result.Row := Row;
  result.Column := Column;
end;

function TValueCell.GetPestName(Index: Integer): string;
begin
  result := ''
end;

function TValueCell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

function TValueCell.GetPestSeriesName(Index: Integer): string;
begin
  result := ''
end;

function TValueCell.IsIdentical(AnotherCell: TValueCell): boolean;
begin
  result := False;
end;

procedure TValueCell.RecordStrings(Strings: TStringList);
begin
  Strings.Add(Mf6ObsName);
end;

procedure TValueCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  ScreenObjectName: string;
begin
  FBoundaryIndex := ReadCompInt(Decomp);
  Decomp.Read(FIFace, SizeOf(FIFace));
  ScreenObjectName := ReadCompStringSimple(Decomp);
  if ScreenObjectName = '' then
  begin
    ScreenObject := nil;
  end
  else
  begin
    ScreenObject := frmGoPhast.PhastModel.
      GetScreenObjectByName(ScreenObjectName);
    Assert(ScreenObject <> nil);
  end;
  Mf6ObsName := ReadCompStringSimple(Decomp);
end;

procedure TValueCell.SetScreenObject(const Value: TObject);
begin
  if Value <> nil then
  begin
    Assert(Value is TScreenObject)
  end;
  FScreenObject := Value;
end;

{ TCellLocation }

class operator TCellLocation.Equal(ACell: TCellLocation;
  BCell: TCellLocation): boolean;
begin
  result :=
    (ACell.Layer = BCell.Layer)
    and (ACell.Row = BCell.Row)
    and (ACell.Column = BCell.Column)
end;

class operator TCellLocation.NotEqual(ACell, BCell: TCellLocation): boolean;
begin
  result := not (ACell = BCell);
end;

end.
