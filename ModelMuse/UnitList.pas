{ This unit contains several classes for fast indexing of objects on ID, Name or both. The indexing
  is done via hash tables for which you can adapt the hash function on a per class basis. The items
  added to the class are contained in a double linked list allowing for a O(1) insertion and
  deletion if you create a large enough minimum/initial hash table size. Else O(log(N)) insertion
  and deletion time is found due to resizing the hash table.

  The hash table may be resized when becoming too full or too empty. The table size is doubled /
  halved. Therefore insertion/deletion may trigger a hash table resize with log(N) chance. This
  violates the O(1) insertion/deletion efficiency. You can set the initial hash table size to
  something "big" so this resizing does not occur in your application and O(1) behaviour is kept. }
unit UnitList;

{ ContinuIT Free Library License (CFLL)

  This source code is owned and only owned by ContinuIT BV.
  (C) 2004 ContinuIT BV, All rights reserved.

  Permission is granted to anyone to use this software for any kind of application, and to
  alter it and redistribute it freely, subject to the following restrictions:
  - The origin of this software must not be misrepresented, you must not claim that you wrote
    the original software.
  - Altered source versions must be plainly marked as such, and must not be misrepresented as
    being the original software.
  - You may not create a library that uses this library as a main part and sell that library.
  - You must have a visible line in your programs about box and/or documentation stating that this
    ContinuIT library is used, with a link where this library can be found.
  - This notice may not be removed or altered from any source distribution.
  - This software is provided 'as-is', without any expressed or implied warranty. In no event will
    the author(s) or ContinuIT BV be held liable for any damages arising from the use of this
    software.

  This license is subject to Dutch Law. }

{ History:
  2002-11-20  ritsaert@continuit.nl       Initial version.
  2002-12-09  ritsaert@continuit.nl       All lists optimized for speed.
                                          ('and' instead of 'mod',
                                           splitted out raises to avoid SEH frames,
                                           Made all string params const).
  2002-12-17  ritsaert@continuit.nl        Created base classes TBaseList and TBaseItem
                                           and adapted the other classes to inherid from them.
  2002-12-29  ritsaert@continuit.nl        Initial Sorter implemented.
  2003-01-01  ritsaert@continuit.nl        Extended TBaseItem with Add, Remove and Clear,
                                           For simple double liked list Items with no ID or Name.
  2003-01-01  ritsaert@continuit.nl        New sorter implemented (Mergesort that is depth first).
  2003-01-03  ritsaert@continuit.nl        Usage of natural order implemented in sorter.
  2003-01-07  ritsaert@continuit.nl        Optimized the sorter, documentation updated.
  2003-01-07  ritsaert@continuit.nl        Implemented the MinumumHashTableSize property.
  2003-07-01  ritsaert@continuit.nl        Added TBaseList.MoveToFront for MRU caches. }

// When testing; comment this away

{$ASSERTIONS OFF}
{$BOOLEVAL OFF}
{$RANGECHECKS OFF}
{$STACKFRAMES OFF}
{$OVERFLOWCHECKS OFF}
{$OPTIMIZATION ON}

interface

uses
  SysUtils;

type
  { All exceptions raised by the list classes in this unit are of this type. }
  EListException = class(Exception);

  { The behavior of the list classes @link(TNamedIDList) can be changed with the following flags.
    The flags themsalves are a set of this type: @link(TListOptions). }
  TListOption = (
    { When accessing the items ByID or ByName and no item is found, the property returns nil if
      this option is not in the set or raises an @link(EListException) otherwise. }
    loRaiseExceptionIfNotFound,
    { When adding an item into a list that is already owned by another list will raise an
      @link(EListException) if this option is not in the set. If it is the item will be removed
      from the other list and added. This option will only work if the base classes of the two
      lists are the same and the Remove methods are not overridden or those overrides have
      no side effects. }
    loAllowStealingFromOtherList,
    { If you try to add into a list an item that has an ID or Name that is already in the list
      and this option is not in the Options set this will raise an @link(EListException). If this
      option is set the items that we're in the list are freed. Note that with the
      @link(TNameIDList) this may trigger two freeing actions on a single addition. }
    loReplaceOnCollision
  );

  { The behavior of the list classes @link(TNamedIDList) can be changed with the following flags.
    See @link(TListOption) for more details. }
  TListOptions = set of TListOption;


  TBaseList = class;

  { All item classes use this class as their base class. The properties of this base class
    allow the derivants to be inserted in a double linked list fashion. The derivatives of
    @link(TBaseList) i this unit can access the private parts of objects this base class to
    implement the Add and Remove procedures. The reason why this is not solved here is for
    performance reasons only. }
  TBaseItem = class
  private
    { The list this item belongs to. If this item belongs to no list this is nil.
      This field is made public by the List property. }
    FList: TBaseList;
    { Double linked list next reference for use with @link(Next) and @link(Prev),
      Also see @link(TBaseList.First) and @link(TBaseList.Last) }
    FNext: TBaseItem;
    { Double linked list previous refrence for use with @link(Next) and @link(Prev),
      Also see @link(TBaseList.First) and @link(TBaseList.Last) }
    FPrev: TBaseItem;
  public
    { The @link(TBaseList) this item belongs to. If this item belongs to no list, this is nil }
    property List: TBaseList read FList;
    { Get the next item the list. You can get the first item in a list by calling
      @link(TBaseList.First). }
    property Next: TBaseItem read FNext;
    { Get the previous item the list. You can get the last item in a list by calling
      @link(TBaseList.Last). }
    property Previous: TBaseItem read FPrev;
  end;

  { <p>A Comparator is a function that compares two items and returns. The procedure
    @link(TBaseList.Sort) uses a comparator to sort the items in the list. Here only the
    information if ItemA is less or equal ItemB is needed:</p>

    True   ItemA is smaller or equal than ItemB<br>
    False  Otherwise (ItemB is smaller) }
  TBaseItemComparator = function(ItemA, ItemB: TBaseItem): Boolean;

  { <p>A ComparatorEvent is an event capable of doing the same thing as a @link(TBaseItemComparator).
    An event carries an additional Self pointer around and is hence more flexible (you can access
    the local object) but also a bit slower (one extra parameter in the method call).</p>

    <p>A Comparator is a function that compares two items and returns. The procedure
    @link(TBaseList.Sort) uses a comparator to sort the items in the list. Here only the
    information if ItemA is less or equal ItemB is needed:</p>

    True   ItemA is smaller or equal than ItemB<br>
    False  Otherwise (ItemB is smaller). }
  TBaseItemComparatorEvent = function(ItemA, ItemB: TBaseItem): Boolean of object;

  { This is the base class of all other List classes in this unit. The properties of this base
    class allow the derivants to be inserted in a double linked list fashion. The derivatives of
    @link(TBaseList) i this unit can access the private parts of objects this base class to
    implement the Add and Remove procedures. The reason why this is not solved here is for
    performance reasons only. }
  TBaseList = class
  private
    { Total number of @link(TNamedIDItem) items in this list }
    FCount: Integer;
    { The items that are owned are kept into a double linked list.
      This reference points to the first item in the linked list. }
    FFirst: TBaseItem;
    { The items that are owned are kept into a double linked list.
      This reference points to the last item in the linked list. }
    FLast: TBaseItem;
    { Holder for the options property }
    FOptions: TListOptions;
  protected
    { This procedure is called when an item should be freed }
    procedure FreeItem(Item: TBaseItem); virtual;
  public
    destructor Destroy; override;
    { Sort the items in the list given the comparator.
      The sorting algorithm used is a merge sort for linked lists that accounts for the
      natural order before sorting. This way the sorting operation is stable and at worst
      O(N.log(N)). At best it is O(N) if the list was already sorted and UseNaturalOrder is True. }
    procedure Sort(LessOrEqual: TBaseItemComparator; UseNaturalOrder: Boolean = True); overload;
    { Sort the items in the list given the comparator.
      The sorting algorithm used is a merge sort for linked lists that accounts for the
      natural order before sorting. This way the sorting operation is stable and at worst
      O(N.log(N)). At best it is O(N) if the list was already sorted and UseNaturalOrder is True.
      This one is slightly slower. }
    procedure Sort(LessOrEqual: TBaseItemComparatorEvent; UseNaturalOrder: Boolean = True); overload;

    { Free all items this list. }
    procedure Clear;
    { Add an item to the list. Adding items is always done on the tail of the linked list. }
    procedure Add(Item: TBaseItem);
    { Remove an item from this list. If the item is not in this list, raise an exception }
    procedure Remove(Item: TBaseItem);
    { Move the selected item to the front. This allows for easy building MRU caches}
    procedure MoveToFront(Item: TBaseItem);
    { The the total number of items that this list contains. }
    property Count: Integer read FCount;
    { Get the first item that this list contains.<br>
      To walk all items in the list:

      <pre>
      I := TMyListType(List.First);<br>
      while Assigned(I) do begin<br>
        // Do something with I<br>
        I := TMyListType(I.Next);<br>
      end;</pre> }
    property First: TBaseItem read FFirst;
    { Get the last item that this list contains.<br>
      To walk all items in the list in reverse:

      <pre>
      I := TMyListType(List.Last);<br>
      while Assigned(I) do begin<br>
        // Do something with I<br>
        I := TMyListType(I.Prev);<br>
      end;</pre> }
    property Last: TBaseItem read FLast;
    { Options to control actions taken in exceptional cases }
    property Options: TListOptions read FOptions write FOptions;
  end;

  { Class type of the @link(TBaseList) class. }
  TBaseListClass = class of TBaseList;

  { Base class for Items that must be referenced efficiently by Name and ID.
    They can be contained by @link(TNameIDList). }
  TNameIDItem = class(TBaseItem)
  private
    { Single linked list next reference for use within the ID hash table }
    FIDNext: TNameIDItem;
    { Single linked list next reference for use within the Name hash table }
    FNameNext: TNameIDItem;
    { ID of this Item. Can be read/changed via the @link(ID) property }
    FID: Integer;
    { Name of this Item. Can be read/changed via the @link(Name) property }
    FName: AnsiString;
    { Hash value of the @link(ID) property. Is calculated via the @link(HashID) virtual function }
    FIDHash: Longword;
    { Hash value of the @link(Name) property.
      Is calculated via the @link(HashName) virtual function }
    FNameHash: Longword;
    { Accessor for the @link(ID) property; Will notify the owning list of the change. }
    procedure SetID(const Value: Integer);
    { Accessor for the @link(Name) property; Will notify the owning list of the change. }
    procedure SetName(const Value: AnsiString);
  public
    { ID of this item. This ID should be unique within the containing list. }
    property ID: Integer read FID write SetID;
    { Name of this item. This Name should be unique within the containing list. }
    property Name: AnsiString read FName write SetName;
  end;

  { A list of @link(TNameIDItem) descendants. It allows for walking all items in the list via
    @link(First) and @link(Last) and finding items @link(ByID) or @link(ByName) very efficiently }
  TNameIDList = class(TBaseList)
  private
    { Andmask used to find the position of a hash item in the hash table }
    FAndMask: Longword;
    { If @link(FCount) >= FGrowSize in @link(Add) we know we need to @link(Resize). }
    FGrowSize: Integer;
    { If @link(FCount) <= FShrinkSize in @link(Remove) we know we need to @link(Resize). }
    FShrinkSize: Integer;
    { Minimum hash table size }
    FMinimumSize: Integer;
    { Array that holds the ID hash table for fast lookup of Items on ID }
    FIDArray: array of TNameIDItem;
    { Array that holds the Name hash table for fast lookup of Items on Name }
    FNameArray: array of TNameIDItem;
    { Internal function for resizing the hash tables }
    procedure Resize(NewLength: Integer);
    { Internal function for @link(ByID) property }
    function GetByID(ID: Integer): TNameIDItem;
    { Internal function for @link(ByName) property }
    function GetByName(const Name: AnsiString): TNameIDItem;
    { Internal procedure for the @link(MinimumHashTableSize) property }
    procedure SetMinimumSize(const Value: Integer);
  protected
    { virtual function to calculate the hash value from the @link(ID).
      It defaults to the ID value itself. }
    function HashID(ID: Integer): Longword; virtual;
    { virtual function to calculate the hash value from the @link(Name).
      It defaults to the CRC of the Name string and is hence case sensitive. }
    function HashName(const Name: AnsiString): Longword; virtual;
  public
    { Create a list for @link(TNameIDItem) items. Within one list these items have a unique ID
      and a unique Name. Naturally also descendants of @link(TNameIDItem) can be used. }
    constructor Create;
    { Cleanup the list. All Items in this list are freed as well. }
    destructor Destroy; override;
    { Free all items this list }
    procedure Clear;
    { Add an item to the list. Adding items is always done on the tail of the linked list. }
    procedure Add(Item: TNameIDItem); 
    { Remove an item from this list. If the item is not in this list, raise an exception }
    procedure Remove(Item: TNameIDItem); 
    { Find an itme by it's ID. }
    property ByID[ID: Integer]: TNameIDItem read GetByID;
    { Find an item by it's Name. }
    property ByName[const Name: AnsiString]: TNameIDItem read GetByName;
    { The minimun hash table size can be set in order to minimize the number of hash table resizes
      while the number of items in the list varies greatly. The size supplied here should be a
      power of two and at least 16 (the initial value). If the value given here is at least two
      times the maximum number of items in the list, all adds and removes are O(1) operations. }
    property MinimumHashTableSize: Integer read FMinimumSize write SetMinimumSize;
  end;

  { Class type of the @link(TNameIDList) type. }
  TNameIDListClass = class of TNameIDList;

  { Base class for Items that must be referenced efficiently by Name.
    They can be contained by @link(TNameList). }
  TNameItem = class(TBaseItem)
  private
    { Single linked list next reference for use within the Name hash table }
    FNameNext: TNameItem;
    { Name of this Item. Can be read/changed via the @link(Name) property }
    FName: AnsiString;
    { Hash value of the @link(Name) property.
      Is calculated via the @link(HashName) virtual function }
    FNameHash: Longword;
    { Accessor for the @link(Name) property; Will notify the owning list of the change }
    procedure SetName(const Value: AnsiString);
  public
    { Name of this item. This Name should be unique within the containing list }
    property Name: AnsiString read FName write SetName;
  end;

  { A list of @link(TNameItem) descendants. It allows for walking all items in the list via
    @link(First) and @link(Last) and finding items @link(ByID) or @link(ByName) very efficiently }
  TNameList = class(TBaseList)
  private
    { Andmask used to find the posiution of a hash item in the hash table }
    FAndMask: Longword;
    { If @link(FCount) >= FGrowSize in @link(Add) we know we need to @link(Resize). }
    FGrowSize: Integer;
    { If @link(FCount) <= FShrinkSize in @link(Remove) we know we need to @link(Resize). }
    FShrinkSize: Integer;
    { Minimum hash table size }
    FMinimumSize: Integer;
    { Array that holds the Name hash table for fast lookup of Items on Name }
    FNameArray: array of TNameItem;
    { Internal function for resizing the hash tables }
    procedure Resize(NewLength: Integer);
    { Internal function for @link(ByName) property }
    function GetByName(const Name: AnsiString): TNameItem;
    { Internal procedure for the @link(MinimumHashTableSize) property }
    procedure SetMinimumSize(const Value: Integer);
  protected
    { virtual function to calculate the hash value from the @link(Name).
      It defaults to the CRC of the Name string and is hence case sensitive. }
    function HashName(const Name: AnsiString): Longword; virtual;
  public
    { Create a list for @link(TNameItem) items. Within one list these items have a unique ID
      and a unique Name. Naturally also descendants of @link(TNameItem) can be used. }
    constructor Create;
    { Cleanup the list. All Items in this list are freed as well. }
    destructor Destroy; override;
    { Free all items this list }
    procedure Clear;
    { Add an item to the list. Adding items is always done on the tail of the linked list. }
    procedure Add(Item: TNameItem);
    { Remove an item from this list. If the item is not in this list, raise an exception }
    procedure Remove(Item: TNameItem); 
    { Find an item by it's Name. }
    property ByName[const Name: AnsiString]: TNameItem read GetByName;
    { The minimun hash table size can be set in order to minimize the number of hash table resizes
      while the number of items in the list varies greatly. The size supplied here should be a
      power of two and at least 16 (the initial value). If the value given here is at least two
      times the maximum number of items in the list, all adds and removes are O(1) operations. }
    property MinimumHashTableSize: Integer read FMinimumSize write SetMinimumSize;
  end;

  { Class type of te @link(TNameList) type. }
  TNameListClass = class of TNameList;

  { Base class for Items that must be referenced efficiently by ID.
    They can be contained by @link(TIDList). }
  TIDItem = class(TBaseItem)
  private
    { Single linked list next reference for use within the ID hash table }
    FIDNext: TIDItem;
    { ID of this Item. Can be read/changed via the @link(ID) property }
    FID: Integer;
    { Hash value of the @link(ID) property. Is calculated via the @link(HashID) virtual function }
    FIDHash: Longword;
    { Accessor for the @link(ID) property; Will notify the owning list of the change }
    procedure SetID(const Value: Integer);
  public
    { ID of this item. This ID should be unique within the containing list }
    property ID: Integer read FID write SetID;
  end;

  { A list of @link(TIDItem) descendants. It allows for walking all items in the list via
    @link(First) and @link(Last) and finding items @link(ByID) very efficiently }
  TIDList = class(TBaseList)
  private
    { Andmask used to find the posiution of a hash item in the hash table }
    FAndMask: Longword;
    { If @link(FCount) >= FGrowSize in @link(Add) we know we need to @link(Resize). }
    FGrowSize: Integer;
    { If @link(FCount) <= FShrinkSize in @link(Remove) we know we need to @link(Resize). }
    FShrinkSize: Integer;
    { Minimum hash table size }
    FMinimumSize: Integer;
    { Array that holds the ID hash table for fast lookup of Items on ID }
    FIDArray: array of TIDItem;
    { Internal function for resizing the hash tables }
    procedure Resize(NewLength: Integer);
    { Internal function for @link(ByID) property }
    function GetByID(ID: Integer): TIDItem;
    { Internal procedure for the @link(MinimumHashTableSize) property }
    procedure SetMinimumSize(const Value: Integer);
  protected
    { virtual function to calculate the hash value from the @link(ID).
      It defaults to the ID value itself. }
    function HashID(ID: Integer): Longword; virtual;
  public
    { Create a list for @link(TIDItem) items. Within one list these items have a unique ID
      and a unique Name. Naturally also descendants of @link(TIDItem) can be used. }
    constructor Create;
    { Cleanup the list. All Items in this list are freed as well. }
    destructor Destroy; override;
    { Free all items this list }
    procedure Clear;
    { Add an item to the list. Adding items is always done on the tail of the linked list. }
    procedure Add(Item: TIDItem);
    { Remove an item from this list. If the item is not in this list, raise an exception }
    procedure Remove(Item: TIDItem);
    { Find an itme by it's ID. }
    property ByID[ID: Integer]: TIDItem read GetByID;
    { The minimun hash table size can be set in order to minimize the number of hash table resizes
      while the number of items in the list varies greatly. The size supplied here should be a
      power of two and at least 16 (the initial value). If the value given here is at least two
      times the maximum number of items in the list, all adds and removes are O(1) operations. }
    property MinimumHashTableSize: Integer read FMinimumSize write SetMinimumSize;
  end;

  { class type of the @link(TIDList) type. }
  TIDListClass = class of TIDList;

  function Crc32(const AString: AnsiString): Cardinal; overload;
  function Crc32(const AString: string): Cardinal; overload;

implementation

uses
  JclMath;



resourcestring
  strNoItem        = 'T{Name}{ID}List: Item not assigned.';
  strAlreadyInList = 'T{Name}{ID}List: Item already owned by another list.';
  strDuplicateID   = 'T{Name}{ID}List.Add: Item with same ID already in list.';
  strDuplicateName = 'T{Name}{ID}List.Add: Item with same Name already in list.';
  strNotOwned      = 'T{Name}{ID}List: Cannot TNameIDItem I do not own.';
  strItemNotFound  = 'T{Name}{ID}List: Item not found.';
  strInvalidSize   = 'T{Name}{ID}List: Invalid minimum cache size (must be power of two and >= 16)';
  strListInUse     = 'T{Name}{ID}List: Cannot change minimum cache size when list contains items';

const
  { The minimum hash table size for the hash tables of IDs and Names
    This minimum must be a power of two }
  CMinimumTableSize = 16;

{ TBaseList }

function Crc32(const AString: AnsiString): Cardinal;
var
  ByteArray: array of Byte;
begin
  SetLength(ByteArray, Length(AString)*SizeOf(AnsiChar));
  Move(AString[1], ByteArray[0], Length(ByteArray));
  result := Crc32(ByteArray, Length(ByteArray));
end;

function Crc32(const AString: string): Cardinal;
var
  ByteArray: array of Byte;
begin
  SetLength(ByteArray, ByteLength(AString));
  Move(AString[1], ByteArray[0], Length(ByteArray));
  result := Crc32(ByteArray, Length(ByteArray));
end;


procedure TBaseList.Add(Item: TBaseItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseAlreadyInList;
  begin // SEH frame avoider
    raise EListException.Create(strAlreadyInList);
  end;
begin
  // Sanity check
  if not Assigned(Item) then RaiseNoItem;
  // Do we or another HashList own this Item?
  if Assigned(Item.FList) then begin
    // Self already owns it... nothing to do...
    if Self = Item.FList then Exit;
    // Another TNameIDList owns this Item; remove it.
    if loAllowStealingFromOtherList in FOptions then TBaseList(Item.FList).Remove(Item)
    else RaiseAlreadyInList;
  end;
  // Item should not be connected to anything
  Assert((Item.FNext = nil) and (Item.FPrev = nil));
  // Add to double linked list
  if Assigned(FFirst) then begin
    Item.FPrev := FLast;
    FLast.FNext := Item;
  end else FFirst := Item;
  FLast := Item;
  Inc(FCount);
  Item.FList := Self;
end;

procedure TBaseList.Clear;
var
  Item, Next: TBaseItem;
begin
  // Free all items in the double linked list
  Item := First;
  while Assigned(Item) do begin
    Next := Item.Next;
    FreeItem(Item);
    Item := Next;
  end;
  FFirst := nil;
  FLast := nil;
  // We contain no items...
  FCount := 0;
end;

destructor TBaseList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBaseList.FreeItem(Item: TBaseItem);
begin
  Item.Free;
end;

procedure TBaseList.MoveToFront(Item: TBaseItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseNotOwned;
  begin // SEH frame avoider
    raise EListException.Create(strNotOwned);
  end;
begin
  // Sanity check: I can only delete a TNameIDItem that I own.
  if not Assigned(Item) then RaiseNoItem;
  if Item.FList <> Self then RaiseNotOwned;
  // Are we the first? If so, nothing to do...
  if FFirst = Item then Exit;
  // Unlink
  Item.FPrev.FNext := Item.FNext;
  if FLast = Item then FLast := Item.FPrev else Item.FNext.FPrev := Item.FPrev;
  // relink
  Item.FNext := FFirst;
  Item.FPrev := nil;
  FFirst.FPrev := Item;
  FFirst := Item;
end;

procedure TBaseList.Remove(Item: TBaseItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseNotOwned;
  begin // SEH frame avoider
    raise EListException.Create(strNotOwned);
  end;
begin
  // Sanity check: I can only delete a TNameIDItem that I own.
  if not Assigned(Item) then RaiseNoItem;
  if Item.FList <> Self then RaiseNotOwned;
  // Remove from the double linked list
  if Assigned(Item.FPrev) then Item.FPrev.FNext := Item.FNext else FFirst := Item.FNext;
  if Assigned(Item.FNext) then Item.FNext.FPrev := Item.FPrev else FLast := Item.FPrev;
  Item.FNext := nil;
  Item.FPrev := nil;
  Dec(FCount);
  Item.FList := nil;
end;

procedure TBaseList.Sort(LessOrEqual: TBaseItemComparator; UseNaturalOrder: Boolean);
var
  i: Integer;
  Item, Next, LastItem, StackItem, Sentinel: TBaseItem;
  Stack: array [0..31] of TBaseItem; // A Maximum of 2 billion items.
begin
  // Sanity check
  Assert(Assigned(LessOrEqual));
  // Is there anything to sort? since it only contains 0 or 1 item
  if FCount <= 1 then Exit;

  { Explanation of the algorithm used:

    Merge sort on linked list accounting for the natural (initial) order of the items.

    This sorting routine has the following properties:

     - Can be used for any single linked list structure
     - Uses only a fixed amount of extra space (37 pointers, an integer and a sentinel) while
       sorting
     - Is a stable sorter (eg items that are equal appear in the same order in the sorted list
       as in the unsorted list
     - It's worst case complexity is O(N.log(N)), the best case complexity is O(N) when
       UseNaturalOrder is true, otherwise the best case complexity is O(N.log(N)).
     - It's speed is quite comparable to TList's Quicksort although that sorting routine has a
       worst case complexity of O(N.N) and best case complexity of O(N.log(N)).

    Classical merge sort:
    Merge sort sorts a list by merging smaller sorted lists to larger ones. If you do not account
    for natural order you start with N sorted linked 'lists' of length 1. After one pass you have
    N/2 sorted linked lists of length 2. The next pass you have N/4 sorted linked lists each having
    linked lists of length 4 etc. After Log(N) passes you end up with a single sorted linked list.
    The nice property of merging sorted lists into a single list is that you only head to compare
    the heads of the to be sorted lists, pop off the smallest and keep that one in storage. At most
    N+M-1 compares need to be done where N and M are the initial sizes of the to be merged lists.
    Each scan through the currently sorted linked lists uses less than N compares and halves the
    number of linked lists rounded upwards. Since the number of rounds is Log(N) the complexity
    of this algorithm is O(N.log(N)), worst, best and expected.

    Improvement: the stack.
    In this implementation we use a stack of already merged sorted linked lists. On level 0 we
    store the found sorted lists from the original list (eg single item lists if no natural order
    is used). On level 1 we keep the linked lists that are merged from the level 0 lists, on level
    2 we keep the linked lists that are merged from the level 1 lists, etc. etc. We only need to
    keep a single linked list on every level and can hence use a single stack. If we need to
    insert a linked list on an already occupied level, we can perform an addition merge, clear that
    level and insert the newly merged list and level higher.
    This stack approach has two main advantages. First the bookkeeping cost to keep track of all the
    merged linked lists becomes very small (log(Nmax) stack pointers). Second, the merges occur in
    a cache frienly pattern compared to the classical merge sort, greatly improving the execution
    speed.

    Improvement: using natural order
    The 'initial' linked lists that are fed into the merge sorter contain a single item. An
    initial scan of the linked list fed into the sorter can result in larger lists that are fed
    into the sorter. If the first compare is negative, we can still feed a linked list of length
    2 to the merge sorter; the first merging step is already done. As long as the compares are
    positive can we go on and extent the linked list we need to feed to the merge sorter.
    This is especially advanteous if you try to sort an already sorted linked list. The natural
    order scanner will find a single linked list and no merging is done. Only the compares to
    validate the sorting are performed. Even on randomly sorted linked lists does this change
    give an improvement although very small. The worst case complexity keeps on being O(N.Log(N))
    but the best case complexity becomes O(N).

    Within the merge sorter we use a sentinel that only needs to be allocated once. This way we can
    keep the inner loop of the merge sorter is simple as possible, eg the head of the linked list
    is also an item and no special 'first' case need to be made in the inner loop.
  }
  Sentinel := TBaseItem.Create;
  try
    // Cleanup the stack
    for i := Low(Stack) to High(Stack) do Stack[i] := nil;

    // Feed all items to the stack
    Item := FFirst;
    if UseNaturalOrder then begin
      repeat
        // Get a string of items that is already sorted
        Next := Item.FNext;
        if Assigned(Next) then begin
          // We have another item... check if it belongs to the string...
          if LessOrEqual(Item,Next) then begin
            // Yep, we have a string... keep on going until we reach the end of it
            repeat
              StackItem := Next;
              Next := Next.FNext;
            until (not Assigned(Next)) or (not LessOrEqual(StackItem,Next));
          end else begin
            // Nope, we do not have a string but we know that we have two items sorted in reverse.
            // if we exchange Item with StackItem we have a small sorted string of length 2
            StackItem := Item;
            Item := Next;
            Next := Next.FNext;
            Item.FNext := StackItem;
          end;
          // Close the string found...
          StackItem.FNext := nil;
        end else begin
          // There was only one item left... add a single item...
          Item.FNext := nil;
        end;
        // Merge the found string pointed by Item into the stack
        i := 0;
        while Assigned(Stack[i]) do begin
          // Merge the item we have now with the item on the stack position [i]
          LastItem := Sentinel;
          StackItem := Stack[i];
          Stack[i] := nil;
          repeat
            if LessOrEqual(StackItem,Item) then begin
              LastItem.FNext := StackItem;
              LastItem := StackItem;
              StackItem := StackItem.FNext;
              if Assigned(StackItem) then Continue;
              LastItem.FNext := Item;
              Break;
            end else begin
              LastItem.FNext := Item;
              LastItem := Item;
              Item := Item.FNext;
              if Assigned(Item) then Continue;
              LastItem.FNext := StackItem;
              Break;
            end;
          until False;
          // The full merged Item can be found on the next reference of the sentinel
          Item := Sentinel.FNext;
          // And look one merge level above
          Inc(i);
        end;
        // Store the merged Item on the stack
        Stack[i] := Item;
        // And try the next item
        Item := Next;
      until not Assigned(Item);
    end else begin
      repeat
        // Get a single item as a sorted 'linked list'
        Next := Item.FNext;
        Item.FNext := nil;
        // Merge the found item into the stack
        i := 0;
        while Assigned(Stack[i]) do begin
          // Merge the item we have now with the item on the stack position [i]
          LastItem := Sentinel;
          StackItem := Stack[i];
          Stack[i] := nil;
          repeat
            if LessOrEqual(StackItem,Item) then begin
              LastItem.FNext := StackItem;
              LastItem := StackItem;
              StackItem := StackItem.FNext;
              if Assigned(StackItem) then Continue;
              LastItem.FNext := Item;
              Break;
            end else begin
              LastItem.FNext := Item;
              LastItem := Item;
              Item := Item.FNext;
              if Assigned(Item) then Continue;
              LastItem.FNext := StackItem;
              Break;
            end;
          until False;
          // The full merged Item can be found on the next reference of the sentinel
          Item := Sentinel.FNext;
          // And look one merge level above
          Inc(i);
        end;
        // Store the merged Item on the stack
        Stack[i] := Item;
        // And try the next item
        Item := Next;
      until not Assigned(Item);
    end;
    // Now all Items are on the stack but multiple stack linked lists may exist...
    // For Stable sort: the Lists on lower stack positions are added after the higher positions

    // Propagate all items to one item on top of the stack...
    i := 0;
    while not Assigned(Stack[i]) do Inc(i);
    Item := Stack[i];
    Inc(i);
    while i < Length(Stack) do begin
      if Assigned(Stack[i]) then begin
        // Merge...
        // Merge the item we have now with the item on the stack position [i]
        LastItem := Sentinel;
        StackItem := Stack[i];
        Stack[i] := nil;
        repeat
          if LessOrEqual(StackItem,Item) then begin
            LastItem.FNext := StackItem;
            LastItem := StackItem;
            StackItem := StackItem.FNext;
            if Assigned(StackItem) then Continue;
            LastItem.FNext := Item;
            Break;
          end else begin
            LastItem.FNext := Item;
            LastItem := Item;
            Item := Item.FNext;
            if Assigned(Item) then Continue;
            LastItem.FNext := StackItem;
            Break;
          end;
        until False;
        // The full merged Item can be found on the next reference of the sentinel
        Item := Sentinel.FNext;
      end;
      Inc(i);
    end;
    // Re-hook the sorted list to FFirst and rehook all the FPrev items
    FFirst := Item;
    LastItem := nil;
    repeat
      Item.FPrev := LastItem;
      LastItem := Item;
      Item := Item.FNext;
    until not Assigned(Item);
    FLast := LastItem;
    // The double linked list is now in correct state...
  finally
    // Release the sentinel...
    Sentinel.Free;
  end;
end;

procedure TBaseList.Sort(LessOrEqual: TBaseItemComparatorEvent; UseNaturalOrder: Boolean);
var
  i: Integer;
  Item, Next, LastItem, StackItem, Sentinel: TBaseItem;
  Stack: array [0..31] of TBaseItem; // A Maximum of 2 billion items.
begin
  // Sanity check
  Assert(Assigned(LessOrEqual));
  // Is there anything to sort? since it only contains 0 or 1 item
  if FCount <= 1 then Exit;
  // Create the sentinel... explanation: see above.
  Sentinel := TBaseItem.Create;
  try
    // Cleanup the stack
    for i := Low(Stack) to High(Stack) do Stack[i] := nil;

    // Feed all items to the stack
    Item := FFirst;
    if UseNaturalOrder then begin
      repeat
        // Get a string of items that is already sorted
        Next := Item.FNext;
        if Assigned(Next) then begin
          // We have another item... check if it belongs to the string...
          if LessOrEqual(Item,Next) then begin
            // Yep, we have a string... keep on going until we reach the end of it
            repeat
              StackItem := Next;
              Next := Next.FNext;
            until (not Assigned(Next)) or (not LessOrEqual(StackItem,Next));
          end else begin
            // Nope, we do not have a string but we know that we have two items sorted in reverse.
            // if we exchange Item with StackItem we have a small sorted string of length 2
            StackItem := Item;
            Item := Next;
            Next := Next.FNext;
            Item.FNext := StackItem;
          end;
          // Close the string found...
          StackItem.FNext := nil;
        end else begin
          // There was only one item left... add a single item...
          Item.FNext := nil;
        end;
        // Merge the found string pointed by Item into the stack
        i := 0;
        while Assigned(Stack[i]) do begin
          // Merge the item we have now with the item on the stack position [i]
          LastItem := Sentinel;
          StackItem := Stack[i];
          Stack[i] := nil;
          repeat
            if LessOrEqual(StackItem,Item) then begin
              LastItem.FNext := StackItem;
              LastItem := StackItem;
              StackItem := StackItem.FNext;
              if Assigned(StackItem) then Continue;
              LastItem.FNext := Item;
              Break;
            end else begin
              LastItem.FNext := Item;
              LastItem := Item;
              Item := Item.FNext;
              if Assigned(Item) then Continue;
              LastItem.FNext := StackItem;
              Break;
            end;
          until False;
          // The full merged Item can be found on the next reference of the sentinel
          Item := Sentinel.FNext;
          // And look one merge level above
          Inc(i);
        end;
        // Store the merged Item on the stack
        Stack[i] := Item;
        // And try the next item
        Item := Next;
      until not Assigned(Item);
    end else begin
      repeat
        // Get a single item as a sorted 'linked list'
        Next := Item.FNext;
        Item.FNext := nil;
        // Merge the found item into the stack
        i := 0;
        while Assigned(Stack[i]) do begin
          // Merge the item we have now with the item on the stack position [i]
          LastItem := Sentinel;
          StackItem := Stack[i];
          Stack[i] := nil;
          repeat
            if LessOrEqual(StackItem,Item) then begin
              LastItem.FNext := StackItem;
              LastItem := StackItem;
              StackItem := StackItem.FNext;
              if Assigned(StackItem) then Continue;
              LastItem.FNext := Item;
              Break;
            end else begin
              LastItem.FNext := Item;
              LastItem := Item;
              Item := Item.FNext;
              if Assigned(Item) then Continue;
              LastItem.FNext := StackItem;
              Break;
            end;
          until False;
          // The full merged Item can be found on the next reference of the sentinel
          Item := Sentinel.FNext;
          // And look one merge level above
          Inc(i);
        end;
        // Store the merged Item on the stack
        Stack[i] := Item;
        // And try the next item
        Item := Next;
      until not Assigned(Item);
    end;
    // Now all Items are on the stack but multiple stack linked lists may exist...
    // For Stable sort: the Lists on lower stack positions are added after the higher positions

    // Propagate all items to one item on top of the stack...
    i := 0;
    while not Assigned(Stack[i]) do Inc(i);
    Item := Stack[i];
    Inc(i);
    while i < Length(Stack) do begin
      if Assigned(Stack[i]) then begin
        // Merge...
        // Merge the item we have now with the item on the stack position [i]
        LastItem := Sentinel;
        StackItem := Stack[i];
        Stack[i] := nil;
        repeat
          if LessOrEqual(StackItem,Item) then begin
            LastItem.FNext := StackItem;
            LastItem := StackItem;
            StackItem := StackItem.FNext;
            if Assigned(StackItem) then Continue;
            LastItem.FNext := Item;
            Break;
          end else begin
            LastItem.FNext := Item;
            LastItem := Item;
            Item := Item.FNext;
            if Assigned(Item) then Continue;
            LastItem.FNext := StackItem;
            Break;
          end;
        until False;
        // The full merged Item can be found on the next reference of the sentinel
        Item := Sentinel.FNext;
      end;
      Inc(i);
    end;
    // Re-hook the sorted list to FFirst and rehook all the FPrev items
    FFirst := Item;
    LastItem := nil;
    repeat
      Item.FPrev := LastItem;
      LastItem := Item;
      Item := Item.FNext;
    until not Assigned(Item);
    FLast := LastItem;
    // The double linked list is now in correct state...
  finally
    // Release the sentinel...
    Sentinel.Free;
  end;
end;

{ TNameIDItem }

procedure TNameIDItem.SetID(const Value: Integer);
var
  List: TNameIDList;
begin
  if Assigned(FList) then begin
    List := TNameIDList(FList);
    List.Remove(Self);
    FID := Value;
    List.Add(Self);
  end else FID := Value;
end;

procedure TNameIDItem.SetName(const Value: AnsiString);
var
  List: TNameIDList;
begin
  if Assigned(FList) then begin
    List := TNameIDList(FList);
    List.Remove(Self);
    FName := Value;
    List.Add(Self);
  end else FName := Value;
end;

{ TNameIDList }

constructor TNameIDList.Create;
begin
  inherited Create;
  FMinimumSize := CMinimumTableSize;
  Clear;
end;

destructor TNameIDList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TNameIDList.Clear;
var
  Item, Next: TBaseItem;
begin
  // Free all items in the double linked list
  Item := First;
  while Assigned(Item) do begin
    Next := Item.Next;
    FreeItem(Item);
    Item := Next;
  end;
  FFirst := nil;
  FLast := nil;
  // We contain no items...
  FCount := 0;
  // Reset table sizes to default values
  Resize(FMinimumSize);
end;

procedure TNameIDList.Resize(NewLength: Integer);
var
  Item: TNameIDItem;
  P: Longword;
begin
  // Sanity check NewLength >= # Items, Minimumsize and should be a power of two
  Assert((NewLength > FCount) and (NewLength >= FMinimumSize) and ((NewLength and (NewLength - 1)) = 0));
  // Recreate & clean the hash arrays
  SetLength(FIDArray,0);
  SetLength(FNameArray,0);
  SetLength(FIDArray,NewLength);
  SetLength(FNameArray,NewLength);
  // Note the new and mask, growsize and schrinksize
  FAndMask := Longword(NewLength) - 1;
  FGrowSize := NewLength shr 1;
  FShrinkSize := NewLength shr 3;
  if FShrinkSize <= (FMinimumSize shr 1) then FShrinkSize := -1;
  // Rebuild the hashtables
  Item := TNameIDItem(First);
  while Assigned(Item) do begin
    // Add to ID hashtable
    P := Item.FIDHash and FAndMask; 
    Item.FIDNext := FIDArray[P];
    FIDArray[P] := Item;
    // Add to Name hashtable
    P := Item.FNameHash and FAndMask;
    Item.FNameNext := FNameArray[P];
    FNameArray[P] := Item;
    // And add the next item if it exists
    Item := TNameIDItem(Item.Next);
  end;
end;

procedure TNameIDList.Add(Item: TNameIDItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseAlreadyInList;
  begin // SEH frame avoider
    raise EListException.Create(strAlreadyInList);
  end;
  procedure RaiseDupicateName;
  begin // SEH frame avoider
    raise EListException.Create(strDuplicateName);
  end;
  procedure RaiseDupicateID;
  begin // SEH frame avoider
    raise EListException.Create(strDuplicateID);
  end;
var
  PID, PName: Longword;
  ItemID, ItemName: TNameIDItem;
begin
  // Sanity check
  if not Assigned(Item) then RaiseNoItem;
  // Do we or another HashList own this Item?
  if Assigned(Item.FList) then begin
    // Self already owns it... nothing to do...
    if Self = Item.FList then Exit;
    // Another TNameIDList owns this Item; remove it.
    if loAllowStealingFromOtherList in FOptions then TNameIDList(Item.FList).Remove(Item)
    else RaiseAlreadyInList;
  end;
  // Item should not be connected to anything
  Assert((Item.FNext = nil) and (Item.FPrev = nil));
  // Do we need to resize? If so use array doubling to keep this routine fast.
  if FCount >= FGrowSize then Resize(FGrowSize shl 2);
  // ID hash table
  Item.FIDHash := HashID(Item.ID);
  PID := Item.FIDHash and FAndMask;
  ItemID := FIDArray[PID];
  while Assigned(ItemID) do begin
    if ItemID.ID = Item.ID then begin
      if loReplaceOnCollision in FOptions then begin
        Remove(ItemID);
        FreeItem(ItemID);
        Add(Item);
        Exit;
      end;
      RaiseDupicateID;
    end;
    ItemID := ItemID.FIDNext;
  end;
  // Name hash table
  Item.FNameHash := HashName(Item.Name);
  PName := Item.FNameHash and FAndMask;
  ItemName := FNameArray[PName];
  while Assigned(ItemName) do begin
    if ItemName.Name = Item.Name then begin
      if loReplaceOnCollision in FOptions then begin
        Remove(ItemName);
        FreeItem(ItemName);
        Add(Item);
        Exit;
      end;
      RaiseDupicateName;
    end;
    ItemName := ItemName.FNameNext;
  end;
  // Add to ID hash table
  Item.FIDNext := FIDArray[PID];
  FIDArray[PID] := Item;
  // Add to Name hash table
  Item.FNameNext := FNameArray[PName];
  FNameArray[PName] := Item;
  // Add to double linked list
  if Assigned(FFirst) then begin
    Item.FPrev := FLast;
    FLast.FNext := Item;
  end else FFirst := Item;
  FLast := Item;
  Inc(FCount);
  Item.FList := Self;
end;

procedure TNameIDList.Remove(Item: TNameIDItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseNotOwned;
  begin // SEH frame avoider
    raise EListException.Create(strNotOwned);
  end;
var
  P: Longword;
  Prev,Iter: TNameIDItem;
begin
  // Sanity check: I can only delete a TNameIDItem that I own.
  if not Assigned(Item) then RaiseNoItem;
  if Item.FList <> Self then RaiseNotOwned;
  // Remove from the double linked list
  if Assigned(Item.FPrev) then Item.FPrev.FNext := Item.FNext else FFirst := Item.FNext;
  if Assigned(Item.FNext) then Item.FNext.FPrev := Item.FPrev else FLast := Item.FPrev;
  Item.FNext := nil;
  Item.FPrev := nil;
  Dec(FCount);
  Item.FList := nil;
  // Remove from the ID hash table
  P := Item.FIDHash and FAndMask;
  Iter := FIDArray[P];
  if Iter = Item then FIDArray[P] := Item.FIDNext else begin
//    Prev := nil;
    repeat
      Assert(Assigned(Iter));
      Prev := Iter;
      Iter := Iter.FIDNext;
    until Iter = Item;
    Prev.FIDNext := Iter.FIDNext;
  end;
  // Remove from the Name hash table
  P := Item.FNameHash and FAndMask;
  Iter := FNameArray[P];
  if Iter = Item then FNameArray[P] := Item.FNameNext else begin
//    Prev := nil;
    repeat
      Assert(Assigned(Iter));
      Prev := Iter;
      Iter := Iter.FNameNext;
    until Iter = Item;
    Prev.FNameNext := Iter.FNameNext;
  end;
  // Check if we need to resize the hash tables
  if FCount < FShrinkSize then Resize(FShrinksize shl 1);
end;

function TNameIDList.GetByID(ID: Integer): TNameIDItem;
  procedure RaiseItemNotFound;
  begin
    raise EListException.Create(strItemNotFound);
  end;
begin
  // Get the correct linked list in the ID hash table...
  Result := FIDArray[HashID(ID) and FAndMask];
  while Assigned(Result) do begin
    // Did we find it?
    if Result.ID = ID then Exit;
    Result := Result.FIDNext;
  end;
  if loRaiseExceptionIfNotFound in FOptions then RaiseItemNotFound;
end;

function TNameIDList.GetByName(const Name: AnsiString): TNameIDItem;
  procedure RaiseItemNotFound;
  begin
    raise EListException.Create(strItemNotFound);
  end;
begin
  // Get the correct linked list in the Name hash table...
  Result := FNameArray[HashName(Name) and FAndMask];
  while Assigned(Result) do begin
    // Did we find it?
    if Result.Name = Name then Exit;
    Result := Result.FNameNext;
  end;
  if loRaiseExceptionIfNotFound in FOptions then RaiseItemNotFound;
end;

function TNameIDList.HashID(ID: Integer): Longword;
begin
  Result := Longword(ID) xor (Longword(ID) shr 4) xor (Longword(ID) shr 8);
end;

function TNameIDList.HashName(const Name: AnsiString): Longword;
begin
  Result := CRC32(Name);
end;

procedure TNameIDList.SetMinimumSize(const Value: Integer);
  procedure RaiseInvalidSize;
  begin
    raise EListException.Create(strInvalidSize);
  end;
  procedure RaiseListInUse;
  begin
    raise EListException.Create(strListInUse);
  end;
begin
  // Sanity check
  if Value = Length(FIDArray) then Exit;
  // Sanity check Value should be a power of two and at least as large as the Minimum size
  if (Value < FMinimumSize) and ((Value and (Value - 1)) <> 0) then RaiseInvalidSize;
  // Sanity Check; no Items are allowed in the list (this is a bit exxegarated)
  if FCount <> 0 then RaiseListInUse;
  // And resize
  FMinimumSize := Value;
  Resize(FMinimumSize);
end;

{ TNameItem }

procedure TNameItem.SetName(const Value: AnsiString);
var
  List: TNameList;
begin
  if Assigned(FList) then begin
    List := TNameList(FList);
    List.Remove(Self);
    FName := Value;
    List.Add(Self);
  end else FName := Value;
end;

{ TNameList }

constructor TNameList.Create;
begin
  inherited Create;
  FMinimumSize := CMinimumTableSize;
  Clear;
end;

destructor TNameList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TNameList.Clear;
var
  Item, Next: TNameItem;
begin
  // Free all items in the double linked list
  Item := TNameItem(First);
  while Assigned(Item) do begin
    Next := TNameItem(Item.Next);
    FreeItem(Item);
    Item := Next;
  end;
  FFirst := nil;
  FLast := nil;
  // We contain no items...
  FCount := 0;
  // Reset table sizes to default values
  Resize(FMinimumSize);
end;

procedure TNameList.Resize(NewLength: Integer);
var
  Item: TNameItem;
  P: Longword;
begin
  // Sanity check
  Assert((NewLength > FCount) and (NewLength >= FMinimumSize) and ((NewLength and (NewLength - 1)) = 0));
  // Recreate & clean the hash array
  SetLength(FNameArray,0);
  SetLength(FNameArray,NewLength);
  // Note the new and mask, growsize and schrinksize
  FAndMask := Longword(NewLength) - 1;
  FGrowSize := NewLength shr 1;
  FShrinkSize := NewLength shr 3;
  if FShrinkSize <= (FMinimumSize shr 1) then FShrinkSize := -1;
  // Rebuild the hashtables
  Item := TNameItem(First);
  while Assigned(Item) do begin
    // Add to Name hashtable
    P := Item.FNameHash and FAndMask;
    Item.FNameNext := FNameArray[P];
    FNameArray[P] := Item;
    // And add the next item if it exists
    Item := TNameItem(Item.Next);
  end;
end;

procedure TNameList.Add(Item: TNameItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseAlreadyInList;
  begin // SEH frame avoider
    raise EListException.Create(strAlreadyInList);
  end;
  procedure RaiseDupicateName;
  begin // SEH frame avoider
    raise EListException.Create(strDuplicateName);
  end;
var
  PName: Longword;
  ItemName: TNameItem;
begin
  // Sanity check
  if not Assigned(Item) then RaiseNoItem;
  // Do we or another HashList own this Item?
  if Assigned(Item.FList) then begin
    // Self already owns it... nothing to do...
    if Self = Item.FList then Exit;
    // Another TNameList owns this Item; remove it.
    if loAllowStealingFromOtherList in FOptions then TNameList(Item.FList).Remove(Item)
    else RaiseAlreadyInList;
  end;
  // Item should not be connected to anything
  Assert((Item.FNext = nil) and (Item.FPrev = nil));
  // Do we need to resize? If so use array doubling to keep this routine fast.
  if FCount >= FGrowSize then Resize(FGrowSize shl 2);
  // Name hash table
  Item.FNameHash := HashName(Item.Name);
  PName := Item.FNameHash and FAndMask;
  ItemName := FNameArray[PName];
  while Assigned(ItemName) do begin
    if ItemName.Name = Item.Name then begin
      if loReplaceOnCollision in FOptions then begin
        Remove(ItemName);
        FreeItem(ItemName);
        Add(Item);
        Exit;
      end;
      RaiseDupicateName;
    end;
    ItemName := ItemName.FNameNext;
  end;
  // Add to double linked list
  if Assigned(FFirst) then begin
    Item.FPrev := FLast;
    FLast.FNext := Item;
  end else FFirst := Item;
  FLast := Item;
  // Add to Name hash table
  Item.FNameNext := FNameArray[PName];
  FNameArray[PName] := Item;
  // And... we're done!
  Inc(FCount);
  Item.FList := Self;
end;

procedure TNameList.Remove(Item: TNameItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseNotOwned;
  begin // SEH frame avoider
    raise EListException.Create(strNotOwned);
  end;
var
  P: Longword;
  Prev,Iter: TNameItem;
begin
  // Sanity check: I can only delete a TNameItem that I own.
  if not Assigned(Item) then RaiseNoItem;
  if Item.FList <> Self then RaiseNotOwned;
  // Remove from the double linked list
  if Assigned(Item.FPrev) then Item.FPrev.FNext := Item.FNext else FFirst := Item.FNext;
  if Assigned(Item.FNext) then Item.FNext.FPrev := Item.FPrev else FLast := Item.FPrev;
  Item.FNext := nil;
  Item.FPrev := nil;
  // Remove from the Name hash table
  P := Item.FNameHash and FAndMask;
  Iter := FNameArray[P];
  if Iter = Item then FNameArray[P] := Item.FNameNext else begin
//    Prev := nil;
    repeat
      Assert(Assigned(Iter));
      Prev := Iter;
      Iter := Iter.FNameNext;
    until Iter = Item;
    Prev.FNameNext := Iter.FNameNext;
  end;
  // Decrement FCount
  Dec(FCount);
  if FCount < FShrinkSize then Resize(FShrinksize shl 1);
  // And we're done...
  Item.FList := nil;
end;

function TNameList.GetByName(const Name: AnsiString): TNameItem;
  procedure RaiseItemNotFound;
  begin
    raise EListException.Create(strItemNotFound);
  end;
begin
  // Get the correct linked list in the Name hash table...
  Result := FNameArray[HashName(Name) and FAndMask];
  while Assigned(Result) do begin
    // Dit we find it?
    if Result.Name = Name then Exit;
    Result := Result.FNameNext;
  end;
  if loRaiseExceptionIfNotFound in FOptions then RaiseItemNotFound;
end;

function TNameList.HashName(const Name: AnsiString): Longword;
begin
  Result := CRC32(Name);
end;


procedure TNameList.SetMinimumSize(const Value: Integer);
  procedure RaiseInvalidSize;
  begin
    raise EListException.Create(strInvalidSize);
  end;
  procedure RaiseListInUse;
  begin
    raise EListException.Create(strListInUse);
  end;
begin
  // Sanity check
  if Value = Length(FNameArray) then Exit;
  // Sanity check Value should be a power of two and at least as large as the Minimum size
  if (Value < FMinimumSize) and ((Value and (Value - 1)) <> 0) then RaiseInvalidSize;
  // Sanity Check; no Items are allowed in the list (this is a bit exxegarated)
  if FCount <> 0 then RaiseListInUse;
  // And resize
  FMinimumSize := Value;
  Resize(FMinimumSize);
end;

{ TIDItem }

procedure TIDItem.SetID(const Value: Integer);
var
  List: TIDList;
begin
  if Assigned(FList) then begin
    List := TIDList(FList);
    List.Remove(Self);
    FID := Value;
    List.Add(Self);
  end else FID := Value;
end;

{ TIDList }

constructor TIDList.Create;
begin
  inherited Create;
  FMinimumSize := CMinimumTableSize; 
  Clear;
end;

destructor TIDList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIDList.Clear;
var
  Item, Next: TIDItem;
begin
  // Free all items in the double linked list
  Item := TIDItem(First);
  while Assigned(Item) do begin
    Next := TIDItem(Item.Next);
    FreeItem(Item);
    Item := Next;
  end;
  FFirst := nil;
  FLast := nil;
  // We contain no items...
  FCount := 0;
  // Reset table sizes to default values
  Resize(FMinimumSize);
end;

procedure TIDList.Resize(NewLength: Integer);
var
  Item: TIDItem;
  P: Longword;
begin
  // Sanity check
  Assert((NewLength > FCount) and (NewLength >= FMinimumSize) and ((NewLength and (NewLength - 1)) = 0));
  // Recreate & clean the hash array; this is the fastest way
  SetLength(FIDArray,0);
  SetLength(FIDArray,NewLength);
  // Note the new and mask, growsize and schrinksize
  FAndMask := Longword(NewLength) - 1;
  FGrowSize := NewLength shr 1;
  FShrinkSize := NewLength shr 3;
  if FShrinkSize <= (FMinimumSize shr 1) then FShrinkSize := -1;
  // Rebuild the hashtables
  Item := TIDItem(First);
  while Assigned(Item) do begin
    // Add to ID hashtable
    P := Item.FIDHash and FAndMask;
    Item.FIDNext := FIDArray[P];
    FIDArray[P] := Item;
    // And add the next item if it exists
    Item := TIDItem(Item.Next);
  end;
end;

procedure TIDList.Add(Item: TIDItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseAlreadyInList;
  begin // SEH frame avoider
    raise EListException.Create(strAlreadyInList);
  end;
  procedure RaiseDupicateID;
  begin // SEH frame avoider
    raise EListException.Create(strDuplicateID);
  end;
var
  PID: Longword;
  ItemID: TIDItem;
begin
  // Sanity check
  if not Assigned(Item) then RaiseNoItem;
  // Do we or another HashList own this Item?
  if Assigned(Item.FList) then begin
    // Self already owns it... nothing to do...
    if Self = Item.FList then Exit;
    // Another TIDList owns this Item; remove it.
    if loAllowStealingFromOtherList in FOptions then TIDList(Item.FList).Remove(Item) else RaiseAlreadyInList;
  end;
  // Item should not be connected to anything
  Assert((Item.FNext = nil) and (Item.FPrev = nil));
  // Do we need to resize? If so use array doubling to keep this routine fast.
  if FCount >= FGrowSize then Resize(FGrowSize shl 2);
  // ID hash table
  Item.FIDHash := HashID(Item.ID);
  PID := Item.FIDHash and FAndMask;
  ItemID := FIDArray[PID];
  while Assigned(ItemID) do begin
    if ItemID.ID = Item.ID then begin
      if loReplaceOnCollision in FOptions then begin
        Remove(ItemID);
        FreeItem(ItemID);
        Add(Item);
        Exit;
      end;
      RaiseDupicateID;
    end;
    ItemID := ItemID.FIDNext;
  end;
  // Add to double linked list
  if Assigned(FFirst) then begin
    Item.FPrev := FLast;
    FLast.FNext := Item;
  end else FFirst := Item;
  FLast := Item;
  // Add to ID hash table
  Item.FIDNext := FIDArray[PID];
  FIDArray[PID] := Item;
  // And... we're done!
  Inc(FCount);
  Item.FList := Self;
end;

procedure TIDList.Remove(Item: TIDItem);
  procedure RaiseNoItem;
  begin // SEH frame avoider
    raise EListException.Create(strNoItem);
  end;
  procedure RaiseNotOwned;
  begin // SEH frame avoider
    raise EListException.Create(strNotOwned);
  end;
var
  P: Longword;
  Prev,Iter: TIDItem;
begin
  // Sanity check: I can only delete a TIDItem that I own.
  if not Assigned(Item) then RaiseNoItem;
  if Item.FList <> Self then RaiseNotOwned;
  // Remove from the double linked list
  if Assigned(Item.FPrev) then Item.FPrev.FNext := Item.FNext else FFirst := Item.FNext;
  if Assigned(Item.FNext) then Item.FNext.FPrev := Item.FPrev else FLast := Item.FPrev;
  Item.FNext := nil;
  Item.FPrev := nil;
  // Remove from the ID hash table
  P := Item.FIDHash and FAndMask;
  Iter := FIDArray[P];
  if Iter = Item then FIDArray[P] := Item.FIDNext else begin
//    Prev := nil;
    repeat
      Assert(Assigned(Iter));
      Prev := Iter;
      Iter := Iter.FIDNext;
    until Iter = Item;
    Prev.FIDNext := Iter.FIDNext;
  end;
  // Decrement FCount
  Dec(FCount);
  if FCount < FShrinkSize then Resize(FShrinksize shl 1);
  // And we're done...
  Item.FList := nil;
end;

function TIDList.GetByID(ID: Integer): TIDItem;
  procedure RaiseItemNotFound;
  begin // SEH frame avoider
    raise EListException.Create(strItemNotFound);
  end;
begin
  // Get the correct linked list in the ID hash table...
  Result := FIDArray[HashID(ID) and FAndMask];
  // Walk the linked list until we find the item we are looking for; if so exit procedure
  while Assigned(Result) do begin
    if Result.ID = ID then Exit;
    Result := Result.FIDNext;
  end;
  // If we did not find it, check if we need to raise the ItemNotFound exception
  if loRaiseExceptionIfNotFound in FOptions then RaiseItemNotFound;
end;

function TIDList.HashID(ID: Integer): Longword;
begin
  Result := Longword(ID) xor (Longword(ID) shr 4) xor (Longword(ID) shr 8);
end;

procedure TIDList.SetMinimumSize(const Value: Integer);
  procedure RaiseInvalidSize;
  begin
    raise EListException.Create(strInvalidSize);
  end;
  procedure RaiseListInUse;
  begin
    raise EListException.Create(strListInUse);
  end;
begin
  // Sanity check
  if Value = Length(FIDArray) then Exit;
  // Sanity check Value should be a power of two and at least as large as the Minimum size
  if (Value < FMinimumSize) and ((Value and (Value - 1)) <> 0) then RaiseInvalidSize;
  // Sanity Check; no Items are allowed in the list (this is a bit exxegarated)
  if FCount <> 0 then RaiseListInUse;
  // And resize
  FMinimumSize := Value;
  Resize(FMinimumSize);
end;

end.
