
{
@abstract(The classes in this unit are meant to act as sparse arrays to
store boundary condition information
in PHAST.)  In PHAST, the locations of boundary conditions of boundaries
are defined in the initial time step and subsequent time steps can redefine
the associated values but not the locations of the boundaries.

GoPhast allows users to define changes in the boundaries at any
time they like and then synthesizes the stress periods from the times that
were defined.  That is the reason why the Items property of each of the
T2D<type>TimeData and T3D<type>TimeData classes does a search backwards in time
to find the last previously defined data value for a particular location.

Before reading the Items property, the program should first call the IsValue
method for the first time step.  Only if it returns True should the Items
property be read.
}
unit SparseDataSets;

interface

uses Windows, Classes, SparseArrayUnit, SysUtils;

type
  {@abstract(@name acts like a 2D array of pointers.  It provides
  constant time access to its elements through the its
  @link(T2DSparsePointerArray.Items) property.  However, when many of the
  elements are nil, it can use much less memory than an array. )
  }
  T2DSparsePointerArray = class(TObject)
  private
    // @name is used to hold pointers to @link(TSparsePointerArray)s.
    FData: TSparsePointerArray;
    // @name defines the section size. Once allocated, a section can
    // store a number of pointers.
    FQuantum1: TSPAQuantum;
    FQuantum2: TSPAQuantum;
    // See @link(IsValue).
    function GetIsValue(const Index1, Index2: NativeInt): boolean;
    // See @link(Items).
    function GetItems(const Index1, Index2: NativeInt): Pointer;
    // See @link(IsValue).
    procedure SetIsValue(const Index1, Index2: NativeInt;
      const Value: boolean);
    // See @link(Items).
    procedure SetItems(const Index1, Index2: NativeInt;
      const Value: Pointer);
  public
    // @name removes all items from @classname.
    procedure Clear; virtual;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2: TSPAQuantum);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name can be read to see if the item at (Index1, Index2) is non-nil.
    // Setting @name at (Index1, Index2) to @False sets Items[Index1, Index2]
    // to nil. Setting @name at (Index1, Index2) to @true has no effect.
    property IsValue[const Index1, Index2: NativeInt]: boolean
      read GetIsValue write SetIsValue;
    // @name provides access to the pointer stored at location
    // Index1, Index2.
    property Items[const Index1, Index2: NativeInt]: Pointer read GetItems
      write SetItems; default;
  end;

  {@abstract(@name acts like a 3D array of pointers.  It provides
  constant time access to its elements through the its
  @link(T3DSparsePointerArray.Items) property.  However, when many of the
  elements are nil, it can use much less memory than an array. )
  }
  T3DSparsePointerArray = class(TObject)
  private
    // @name is used to hold pointers to @link(T2DSparsePointerArray)s.
    FData: TSparsePointerArray;
    // @name defines the section size. Once allocated, a section can
    // store a number of pointers.
    FQuantum1: TSPAQuantum;
    FQuantum2: TSPAQuantum;
    FQuantum3: TSPAQuantum;
    // See @link(IsValue).
    function GetIsValue(const Layer, Row, Col: NativeInt): boolean;
    // See @link(Items).
    function GetItems(const Layer, Row, Col: NativeInt): Pointer;
    // See @link(IsValue).
    procedure SetIsValue(const Layer, Row, Col: NativeInt;
      const Value: boolean);
    // See @link(Items).
    procedure SetItems(const Layer, Row, Col: NativeInt;
      const Value: Pointer);
  protected
    FMinRow: NativeInt;
    FMaxLayer: NativeInt;
    FMinLayer: NativeInt;
    FMaxCol: NativeInt;
    FMaxRow: NativeInt;
    FMinCol: NativeInt;
    procedure UpdateMinMaxPositions(const Col, Row, Layer: NativeInt);
  public
    // @name removes all items from @classname.
    procedure Clear; virtual;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name can be read to see if the item at (Layer, Row, Col) is non-nil.
    // Setting @name at (Layer, Row, Col) to @False sets Items[Layer, Row, Col]
    // to nil. Setting @name at (Layer, Row, Col) to @true has no effect.
    property IsValue[const Layer, Row, Col: NativeInt]: boolean
      read GetIsValue write SetIsValue;
    // @name provides access to the pointer stored at location
    // Index1, Index2.
    property Items[const Layer, Row, Col: NativeInt]: Pointer read GetItems
      write SetItems; default;
    property MinLayer: NativeInt read FMinLayer;
    property MaxLayer: NativeInt read FMaxLayer;
    property MinRow: NativeInt read FMinRow;
    property MaxRow: NativeInt read FMaxRow;
    property MinCol: NativeInt read FMinCol;
    property MaxCol: NativeInt read FMaxCol;
  end;

  {@abstract(@name acts like a 2D array of real numbers.  It provides
  constant time access to its elements through the its
  @link(T2DSparseRealArray.Items) property.  However, when many of the
  elements are unassigned, it can use much less memory than an array.)}
  T2DSparseRealArray = class(T2DSparsePointerArray)
  private
    // @name is the number of real numbers stored in @classname.
    FCount: NativeInt;
    // @name is an array of real-numbers stored in @classname
    FValues: array of double;
    // See @link(Items).
    function GetItems(const Index1, Index2: NativeInt): double;
    // See @link(Items).
    procedure SetItems(const Index1, Index2: NativeInt;
      const Value: double);
  public
    // @name removes all the members of @classname.
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2: TSPAQuantum);
    // @name provides access to the real number stored at location
    // Index1, Index2. Before reading @name, read
    // @link(T2DSparsePointerArray.IsValue) to
    // be sure that a number is stored at Index1, Index2.
    property Items[const Index1, Index2: NativeInt]: double read GetItems
      write SetItems; default;
  end;

  T2DSparseBooleanArray = class(T2DSparsePointerArray)
    // The address of @name is stored to indicate a value that is @False.
    FFalse: boolean;
    // The address of @name is stored to indicate a value that is @True.
    FTrue: boolean;
    // See @link(Items).
    function GetItems(const Row, Col: NativeInt): boolean;
    // See @link(Items).
    procedure SetItems(const Row, Col: NativeInt; const Value: boolean);
  public
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2: TSPAQuantum);
    // @name provides access to the boolean stored at location
    // Layer, Row, Col. Before reading @name, read
    // @link(T3DSparsePointerArray.IsValue) to
    // be sure that a boolean is stored at Layer, Row, Col.
    property Items[const Row, Col: NativeInt]: boolean
      read GetItems write SetItems; default;
  end;

  {@abstract(@name acts like a 3D array of booleans.  It provides
  constant time access to its elements through the its
  @link(T3DSparseBooleanArray.Items) property.  However, when many of the
  elements are unassigned, it can use much less memory than an array.)}
  T3DSparseBooleanArray = class(T3DSparsePointerArray)
  private
    // The address of @name is stored to indicate a value that is @False.
    FFalse: boolean;
    // The address of @name is stored to indicate a value that is @True.
    FTrue: boolean;
    // See @link(Items).
    function GetItems(const Layer, Row, Col: NativeInt): boolean;
    // See @link(Items).
    procedure SetItems(const Layer, Row, Col: NativeInt; const Value: boolean);
  public
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name provides access to the boolean stored at location
    // Layer, Row, Col. Before reading @name, read
    // @link(T3DSparsePointerArray.IsValue) to
    // be sure that a boolean is stored at Layer, Row, Col.
    property Items[const Layer, Row, Col: NativeInt]: boolean
      read GetItems write SetItems; default;
  end;

  {@abstract(@name acts like a 3D array of integers.  It provides
  constant time access to its elements through the its
  @link(T3DSparseIntegerArray.Items) property.  However, when many of the
  elements are unassigned, it can use much less memory than an array.)}
  T3DSparseIntegerArray = class(T3DSparsePointerArray)
  private
    // @name is the number of integers stored in @classname.
    FCount: NativeInt;
    // @name is an array of integers stored in @classname
    FValues: array of NativeInt;
    // See @link(Items).
    function GetItems(const Layer, Row, Col: NativeInt): NativeInt;
    // See @link(Items).
    procedure SetItems(const Layer, Row, Col: NativeInt; const Value: NativeInt);
  public
    procedure RemoveValue(const Layer, Row, Col: NativeInt);
    // @name removes all the members of @classname.
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name provides access to the NativeInt stored at location
    // Layer, Row, Col. Before reading @name, read
    // @link(T3DSparsePointerArray.IsValue) to
    // be sure that a number is stored at Layer, Row, Col.
    property Items[const Layer, Row, Col: NativeInt]: NativeInt read GetItems
      write SetItems; default;
  end;

  {@abstract(@name acts like a 3D array of real-numbers.  It provides
  constant time access to its elements through the its
  @link(T3DSparseRealArray.Items) property.  However, when many of the
  elements are unassigned, it can use much less memory than an array.)
  @seealso(T3DSparseCacheableRealArray)}
  T3DSparseRealArray = class(T3DSparsePointerArray)
  private
    // @name is an array of real numbers stored in @classname
    FValues: array of double;
    FCleared: Boolean;
    // See @link(Items).
    function GetItems(const Layer, Row, Col: NativeInt): double;
  protected
    // @name is the number of real numbers stored in @classname.
    FCount: NativeInt;
    // See @link(Items).
    procedure SetItems(const Layer, Row, Col: NativeInt;
      const Value: double); virtual;
  public
    procedure RemoveValue(const Layer, Row, Col: NativeInt);
    // @name removes all the members of @classname.
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name provides access to the real number stored at location
    // Layer, Row, Col. Before reading @name, read
    // @link(T3DSparsePointerArray.IsValue) to
    // be sure that a number is stored at Layer, Row, Col.
    property Items[const Layer, Row, Col: NativeInt]: double read GetItems
      write SetItems; default;
  end;

  {@abstract(@name acts like a 3D array of strings.  It provides
  constant time access to its elements through the its
  @link(T3DSparseRealArray.Items) property.  However, when many of the
  elements are unassigned, it can use much less memory than an array.)}
  T3DSparseStringArray = class(T3DSparsePointerArray)
  private
    // @name is the number of strings stored in @classname.
    FCount: NativeInt;
    // @name is an array of strings stored in @classname
    FValues: array of string;
    // See @link(Items).
    function GetItems(const Layer, Row, Col: NativeInt): string;
    // See @link(Items).
    procedure SetItems(const Layer, Row, Col: NativeInt;
      const Value: string);
  public
    procedure RemoveValue(const Layer, Row, Col: NativeInt);
    // @name removes all the members of @classname.
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name provides access to the string stored at location
    // Layer, Row, Col. Before reading @name, read
    // @link(T3DSparsePointerArray.IsValue) to
    // be sure that a string is stored at Layer, Row, Col.
    property Items[const Layer, Row, Col: NativeInt]: string read GetItems
      write SetItems; default;
  end;

implementation



function FreeSparsePointerItem(TheIndex: NativeInt; TheItem: Pointer): NativeInt;
var
  SparsePointerArray: TSparsePointerArray;
begin
  SparsePointerArray := TheItem;
  SparsePointerArray.Free;
  result := 0;
end;

function Free2DSparsePointerItem(TheIndex: NativeInt; TheItem: Pointer): NativeInt;
var
  SparsePointerArray: T2DSparsePointerArray;
begin
  SparsePointerArray := TheItem;
  SparsePointerArray.Free;
  result := 0;
end;

{ T2DSparsePointerArray }

constructor T2DSparsePointerArray.Create(Quantum1, Quantum2: TSPAQuantum);
begin
  inherited Create;
  FQuantum1 := Quantum1;
  FQuantum2 := Quantum2;
  FData := TSparsePointerArray.Create(Quantum1);
end;

procedure T2DSparsePointerArray.Clear;
var
  Index: NativeInt;
  SparsePointerArray: TSparsePointerArray;
begin
  for Index := 0 to FData.HighBound do
  begin
    SparsePointerArray := FData.Items[Index];
    SparsePointerArray.Free;
    FData.Items[Index] := nil;
  end;
  FData.ResetHighBound;
end;

destructor T2DSparsePointerArray.Destroy;
begin
  FData.ForAll(FreeSparsePointerItem);
  FData.Free;
  inherited;
end;

function T2DSparsePointerArray.GetItems(const Index1,
  Index2: NativeInt): Pointer;
var
  InnerData: TSparsePointerArray;
begin
  InnerData := FData[Index1];
  if InnerData = nil then
  begin
    result := nil;
  end
  else
  begin
    result := InnerData[Index2];
  end;
end;

procedure T2DSparsePointerArray.SetItems(const Index1, Index2: NativeInt;
  const Value: Pointer);
var
  InnerData: TSparsePointerArray;
begin
  InnerData := FData[Index1];
  if InnerData = nil then
  begin
    if Value = nil then
      Exit;
    InnerData := TSparsePointerArray.Create(FQuantum2);
    FData[Index1] := InnerData;
  end;
  InnerData[Index2] := Value;
end;

function T2DSparsePointerArray.GetIsValue(const Index1,
  Index2: NativeInt): boolean;
begin
  result := Items[Index1, Index2] <> nil;
end;

procedure T2DSparsePointerArray.SetIsValue(const Index1, Index2: NativeInt;
  const Value: boolean);
begin
  if not Value then
  begin
    Items[Index1, Index2] := nil;
  end;
end;

{ T3DSparsePointerArray }

procedure T3DSparsePointerArray.Clear;
var
  Index: NativeInt;
  TwoDSparsePointerArray: T2DSparsePointerArray;
begin
  FMinLayer := -1;
  FMaxLayer := -1;
  FMinRow := -1;
  FMaxRow := -1;
  FMinCol := -1;
  FMaxCol := -1;

  for Index := 0 to FData.HighBound do
  begin
    TwoDSparsePointerArray := FData.Items[Index];
    TwoDSparsePointerArray.Free;
    FData.Items[Index] := nil;
  end;
  FData.ResetHighBound;
end;

constructor T3DSparsePointerArray.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited Create;

  // Descendants of T3DSparsePointerArray assume that a pointer is the
  // same size as a NativeInt in their GetItems and SetItems methods.
  // If a change in the size of pointer occurs in the future, that code
  // will need to be updated.
{$IF SizeOf(NativeInt) <> SizeOf(Pointer)}
  Assert(False);
{$IFEND}

  FQuantum1 := Quantum1;
  FQuantum2 := Quantum2;
  FQuantum3 := Quantum3;
  FData := TSparsePointerArray.Create(Quantum1);
  FMinLayer := -1;
  FMaxLayer := -1;
  FMinRow := -1;
  FMaxRow := -1;
  FMinCol := -1;
  FMaxCol := -1;
end;

destructor T3DSparsePointerArray.Destroy;
begin
  FData.ForAll(Free2DSparsePointerItem);
  FData.Free;
  inherited;
end;

function T3DSparsePointerArray.GetIsValue(const Layer, Row, Col: NativeInt): boolean;
begin
  result := Items[Layer, Row, Col] <> nil;
end;

function T3DSparsePointerArray.GetItems(const Layer, Row, Col: NativeInt): Pointer;
var
  InnerData: T2DSparsePointerArray;
begin
  InnerData := FData[Layer];
  if InnerData = nil then
  begin
    result := nil;
  end
  else
  begin
    result := InnerData[Row, Col];
  end;
end;

procedure T3DSparsePointerArray.SetIsValue(const Layer, Row, Col: NativeInt; const Value: boolean);
begin
  if not Value then
  begin
    Items[Layer, Row, Col] := nil;
  end;
end;

procedure T3DSparsePointerArray.SetItems(const Layer, Row, Col: NativeInt; const Value: Pointer);
var
  InnerData: T2DSparsePointerArray;
begin
  InnerData := FData[Layer];
  if InnerData = nil then
  begin
    if Value = nil then
      Exit;
    InnerData := T2DSparsePointerArray.Create(FQuantum2, FQuantum3);
    FData[Layer] := InnerData;
  end;
  InnerData[Row, Col] := Value;
  UpdateMinMaxPositions(Col, Row, Layer);
end;

{ T3DSparseBooleanArray }

constructor T3DSparseBooleanArray.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited;
  FTrue := True;
end;

function T3DSparseBooleanArray.GetItems(const Layer, Row,
  Col: NativeInt): boolean;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Layer, Row, Col];
  Assert(resultPtr <> nil);
  result := PBoolean(resultPtr)^
end;

procedure T3DSparseBooleanArray.SetItems(const Layer, Row, Col: NativeInt;
  const Value: boolean);
var
  DataPtr: Pointer;
begin
  if Value then
  begin
    DataPtr := @FTrue;
  end
  else
  begin
    DataPtr := @FFalse;
  end;
  inherited Items[Layer, Row, Col] := DataPtr;
end;

{ T3DSparseIntegerArray }

procedure T3DSparseIntegerArray.Clear;
begin
  inherited;
  FCount := 0;
  SetLength(FValues,0);
end;

constructor T3DSparseIntegerArray.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited;
  SetLength(FValues, 4);
end;

procedure T3DSparsePointerArray.UpdateMinMaxPositions(const Col, Row, Layer: NativeInt);
begin
  Assert(Col >= 0);
  Assert(Row >= 0);
  Assert(Layer >= 0);
  if FMinLayer = -1 then
  begin
    FMinLayer := Layer;
    FMaxLayer := Layer;
    FMinRow := Row;
    FMaxRow := Row;
    FMinCol := Col;
    FMaxCol := Col;
  end
  else
  begin
    if FMinLayer > Layer then
    begin
      FMinLayer := Layer;
    end;
    if FMaxLayer < Layer then
    begin
      FMaxLayer := Layer;
    end;
    if FMinRow > Row then
    begin
      FMinRow := Row;
    end;
    if FMaxRow < Row then
    begin
      FMaxRow := Row;
    end;
    if FMinCol > Col then
    begin
      FMinCol := Col;
    end;
    if FMaxCol < Col then
    begin
      FMaxCol := Col;
    end;
  end;
end;

function T3DSparseIntegerArray.GetItems(const Layer, Row,
  Col: NativeInt): NativeInt;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Layer, Row, Col];
  Assert(resultPtr <> nil);
  result := FValues[Pred(longint(resultPtr))];
end;

procedure T3DSparseIntegerArray.RemoveValue(const Layer, Row, Col: NativeInt);
begin
  inherited Items[Layer, Row, Col] := nil;
end;

procedure T3DSparseIntegerArray.SetItems(const Layer, Row, Col,
  Value: NativeInt);
var
  DataPtr: Pointer;
begin
  DataPtr := inherited Items[Layer, Row, Col];
  if DataPtr = nil then
  begin
    if Length(FValues) = FCount then
    begin
      if FCount = 0 then
      begin
        SetLength(FValues, 4);
      end
      else
      begin
        SetLength(FValues, FCount + FCount div 4);
      end;
    end;
    FValues[FCount] := Value;
    Inc(FCount);
    DataPtr := Pointer(FCount);
    inherited Items[Layer, Row, Col] := DataPtr;
  end
  else
  begin
    FValues[Pred(longint(DataPtr))] := Value
  end;
end;

{ T3DSparseRealArray }

procedure T3DSparseRealArray.Clear;
begin
  inherited;
  FCount := 0;
  SetLength(FValues, 0);
end;

constructor T3DSparseRealArray.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited;
  SetLength(FValues, 4);
end;

function T3DSparseRealArray.GetItems(const Layer, Row,
  Col: NativeInt): double;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Layer, Row, Col];
  Assert(resultPtr <> nil);
  result := FValues[Pred(longint(resultPtr))];
end;

procedure T3DSparseRealArray.RemoveValue(const Layer, Row, Col: NativeInt);
begin
  inherited Items[Layer, Row, Col] := nil;
end;

procedure T3DSparseRealArray.SetItems(const Layer, Row,
  Col: NativeInt; const Value: double);
var
  DataPtr: Pointer;
begin
  FCleared := False;
  DataPtr := inherited Items[Layer, Row, Col];
  if DataPtr = nil then
  begin
    if Length(FValues) = FCount then
    begin
      if FCount = 0 then
      begin
        SetLength(FValues,4);
      end
      else
      begin
        SetLength(FValues, FCount + FCount div 4);
      end;
    end;
    FValues[FCount] := Value;
    Inc(FCount);
    DataPtr := Pointer(FCount);
    inherited Items[Layer, Row, Col] := DataPtr;
  end
  else
  begin
    FValues[Pred(longint(DataPtr))] := Value
  end;
end;

{ T3DSparseStringArray }

procedure T3DSparseStringArray.Clear;
begin
  inherited;
  FCount := 0;
  SetLength(FValues,0);
end;

constructor T3DSparseStringArray.Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited;
  SetLength(FValues, 4);
end;

function T3DSparseStringArray.GetItems(const Layer, Row,
  Col: NativeInt): string;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Layer, Row, Col];
  Assert(resultPtr <> nil);
  result := FValues[Pred(longint(resultPtr))];
end;

procedure T3DSparseStringArray.RemoveValue(const Layer, Row, Col: NativeInt);
begin
  inherited Items[Layer, Row, Col] := nil;
end;

procedure T3DSparseStringArray.SetItems(const Layer, Row,
  Col: NativeInt; const Value: string);
var
  DataPtr: Pointer;
begin
  DataPtr := inherited Items[Layer, Row, Col];
  if DataPtr = nil then
  begin
    if Length(FValues) = FCount then
    begin
      if FCount = 0 then
      begin
        SetLength(FValues, 4);
      end
      else
      begin
        SetLength(FValues, FCount + FCount div 4);
      end;
    end;
    FValues[FCount] := Value;
    Inc(FCount);
    DataPtr := Pointer(FCount);
    inherited Items[Layer, Row, Col] := DataPtr;
  end
  else
  begin
    FValues[Pred(longint(DataPtr))] := Value
  end;
end;

{ T2DSparseRealArray }

procedure T2DSparseRealArray.Clear;
begin
  inherited;
  FCount := 0;
end;

constructor T2DSparseRealArray.Create(Quantum1, Quantum2: TSPAQuantum);
begin
  inherited;
  SetLength(FValues, 4);
end;

function T2DSparseRealArray.GetItems(const Index1,
  Index2: NativeInt): double;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Index1, Index2];
  Assert(resultPtr <> nil);
  result := FValues[Pred(longint(resultPtr))];
end;

procedure T2DSparseRealArray.SetItems(const Index1, Index2: NativeInt;
  const Value: double);
var
  DataPtr: Pointer;
begin
  DataPtr := inherited Items[Index1, Index2];
  if DataPtr = nil then
  begin
    if Length(FValues) = FCount then
    begin
      SetLength(FValues, FCount + FCount div 4);
    end;
    FValues[FCount] := Value;
    Inc(FCount);
    DataPtr := Pointer(FCount);
    inherited Items[Index1, Index2] := DataPtr;
  end
  else
  begin
    FValues[Pred(longint(DataPtr))] := Value
  end;
end;

{ T2DSparseBooleanArray }

constructor T2DSparseBooleanArray.Create(Quantum1, Quantum2: TSPAQuantum);
begin
  inherited;
  FTrue := True;
end;

function T2DSparseBooleanArray.GetItems(const Row, Col: NativeInt): boolean;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Row, Col];
  Assert(resultPtr <> nil);
  result := PBoolean(resultPtr)^
end;

procedure T2DSparseBooleanArray.SetItems(const Row, Col: NativeInt;
  const Value: boolean);
var
  DataPtr: Pointer;
begin
  if Value then
  begin
    DataPtr := @FTrue;
  end
  else
  begin
    DataPtr := @FFalse;
  end;
  inherited Items[Row, Col] := DataPtr;
end;

initialization
  Assert(SizeOf(NativeInt) = SizeOf(Pointer));

end.

