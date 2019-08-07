{== unit ObserverProxyU ==============================================}
{: This unit defines a helper class for storing observer interfaces,
  and a list class for this class.
@author GenTypedLists Version 1.0
@desc   Version 1.0 created  2004-01-20<BR>
        Last modified       2004-01-20<P>
   }
{======================================================================}
unit ObserverProxyU;
interface

uses Classes, AbstractTypedList, ObserverIntfU ;

type
  TObserverProxy = class(TObject)
  private
    FObserver: IObjectObserver;
    FKindSet: TChangeKindSet;
  public
    constructor Create(aObserver: IObjectObserver; aKindSet:
        TChangeKindSet);
    function Clone: TObserverProxy;    
    procedure NotifyOnAdd(Obj: TObject);
    procedure NotifyOnChange(Obj: TObject; aChangeKind: TChangeKind = 0);
    procedure NotifyOnRemove(Obj: TObject);
    property Observer: IObjectObserver read FObserver;
    property KindSet: TChangeKindSet read FKindSet write FKindSet;
  end;

  TObserverProxyList = class( TAbstractTypedList )
  private
    procedure Put( index: Integer; const Item: TObserverProxy );
    function  Get( index: Integer ): TObserverProxy;
    procedure FreeItem( index: Integer );

  public
    function  Add(const Item: TObserverProxy): Integer;
    procedure Insert(Index: Integer; const Item: TObserverProxy);
    procedure Assign(Source: TPersistent); override;

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function  Detach(Index: Integer): TObserverProxy;
    function IndexOf( item: TObserverProxy ): Integer; overload;
    function IndexOf( const aObserver: IObjectObserver ): Integer; overload;
    property Items[Index: Integer]: TObserverProxy read Get write Put; default;
  end; { TObserverProxyList }

implementation

uses Sysutils;

{== TObserverProxyList ===============================================}

procedure TObserverProxyList.Assign(Source: TPersistent);
var
  Sourcelist: TObserverProxyList;
  i: Integer;
begin
  if Source Is TObserverProxyList then begin
    Sourcelist:= TObserverProxyList(source);
    Clear;
    // begin change, rbw
    Capacity := Sourcelist.LastIndex +1;
    // end change rbw
    for i:= 0 to Sourcelist.LastIndex do
      Add( Sourcelist[i].Clone );
  end
  else
    inherited;
end;

{-- TObserverProxyList.Add -------------------------------------------}
{: Adds the passed item to the list
@Param Item is the item to add
@Returns the index of the item
@Raises EOutOfMemoryError if we run out of memory (rare).
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
function TObserverProxyList.Add(const Item: TObserverProxy): Integer;
begin
  Result := Count;
  Insert( Result, Item );
end; { TObserverProxyList.Add }

{-- TObserverProxyList.Insert ----------------------------------------}
{: Inserts an item at a specified position in the list
@Param Index of the item before which to insert the new item.
  if Index >= Count the item will be appended to the list.
@Param Item is the item to instert
@Desc Inserts the passed item into the list. if the data type is
  larger than 4 bytes memory for the Item is allocated on the
  heap, the item is copied into it and the pointer is stored
  in the list, otherwise the data is stored directly, typecast
  to a pointer.
@Raises EOutOfMemoryError if we run out of memory (rare).
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
procedure TObserverProxyList.Insert(Index: Integer; const Item: TObserverProxy);
begin
  if Index > Count then
    Index := Count;
  Storage.Insert( index, Nil );
  try
    Put( index, Item );
  except
    Storage.Delete( index );
    raise
  end;
end; { TObserverProxyList.Insert }

{-- TObserverProxyList.Clear -----------------------------------------}
{: Clears the list, freeing memory for the items if necessary.
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
procedure TObserverProxyList.Clear;
var
  i: Integer;
begin
  for i := LastIndex downto 0 do
    FreeItem( i );
  inherited Clear;
end; { TObserverProxyList.Clear }

{-- TObserverProxyList.FreeItem --------------------------------------}
{: Frees the memory for the item at index, if required.
@Param index is the index of the item to free, 0 <= index < count
@Raises EListError if the index is invalid
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
procedure TObserverProxyList.FreeItem( index: Integer );
begin
  Items[ Index ].Free;
end; { TObserverProxyList.FreeItem }

{-- TObserverProxyList.IndexOf ---------------------------------------}
{: Searches for an object in the list
@Param item object to search for
@Returns the items index, or -1 if not found
@Desc This method only makes sense if the list contains object.
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
function TObserverProxyList.IndexOf( item: TObserverProxy ): Integer;
begin { IndexOf }
  Result := Storage.IndexOf( item );
end; { TObserverProxyList.IndexOf }

function TObserverProxyList.IndexOf( const aObserver: IObjectObserver ): Integer;
begin { IndexOf }
  for Result := 0 to LastIndex do
    if Items[Result].Observer = aObserver then
      Exit;
  Result := -1;
end; { TObserverProxyList.IndexOf }

{-- TObserverProxyList.Delete ----------------------------------------}
{: Deletes an item from the list, freeing its memory if necessary.
@Param Index is the index of item to delete, 0 <= index < count
@Raises EListError if the index is invalid
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
procedure TObserverProxyList.Delete(Index: Integer);
begin
  FreeItem( Index );
  inherited Delete( Index );
end; { TObserverProxyList.Delete }

{-- TObserverProxyList.Detach ----------------------------------------}
{: Returns the item at index and then deletes the entry from the list.
@Param Index is the index of item to remove from the list,
   0 <= index < count
@Returns the removed item
@Desc if the list stores objects the object will of course <b>not</b> be
  freed, the list relegates ownership of the data to the caller.
@Raises EListError if the index is invalid
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
function  TObserverProxyList.Detach(Index: Integer): TObserverProxy;
begin
  Result := Items[ Index ];
  inherited Delete( index );
end; { TObserverProxyList.Detach }

{-- TObserverProxyList.Put -------------------------------------------}
{: Accessor for the Items property (write)
@Param index gives the slot to store the item into, 0 <= index < count
@Param Item is the item to store
@Desc  Frees memory for the item at index, if required, and stores
   the passed item in that slot of the list. if the data type is
   larger than 4 bytes memory for the Item is allocated on the
   heap, the item is copied into it and the pointer is stored
   in the list, otherwise the data is stored directly, typecast
   to a pointer.
@Raises EListError if the index is invalid
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
procedure TObserverProxyList.Put( index: Integer; const Item: TObserverProxy );
begin
  FreeItem( index );
  Storage.Items[ Index ]:= Pointer( Item );
end; { TObserverProxyList.Put }

{-- TObserverProxyList.Get -------------------------------------------}
{: Accessor for the Items property (read)
@Param index is the index of the item to return, 0 <= index < count
@Returns the item at index
@Raises EListError if the index is invalid
}{ Created 2004-01-20 by GenTypedLists Version 1.0
-----------------------------------------------------------------------}
function  TObserverProxyList.Get( index: Integer ): TObserverProxy;
begin
  Result := TObserverProxy( Storage.Items[ index ] );
end; { TObserverProxyList.Get }

{== TObserverProxy ====================================================}

function TObserverProxy.Clone: TObserverProxy;
begin
  Result := TObserverProxy.Create( FObserver, FKindSet );
end;

constructor TObserverProxy.Create(aObserver: IObjectObserver; aKindSet:
    TChangeKindSet);
begin
  inherited Create;
  FObserver := aObserver;
  FKindSet  := aKindSet;
end;

procedure TObserverProxy.NotifyOnAdd(Obj: TObject);
var
  intf: IObjectListObserver;
begin
  if Supports( Observer, IObjectListObserver, intf ) then
    intf.ObjectAdded(Obj);
end;

procedure TObserverProxy.NotifyOnChange(Obj: TObject;
  aChangeKind: TChangeKind);
begin
  if (aChangeKind = ck_Unspecified) or (Kindset = []) or
     (aChangeKind In KindSet)
  then
    Observer.DataChanged(Obj, aChangeKind);
end;

procedure TObserverProxy.NotifyOnRemove(Obj: TObject);
begin
  Observer.ObjectRemoved(Obj);
end;

initialization
end { ObserverProxyU }.

