{== ObserverListU =====================================================}
{: This unit provides a default implementation of the IObjectObserverlist
  and ISubject interfaces, which objects can use to handle the subject
  side of the Observer pattern.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-01-20<BR>
        Last modified       2004-01-20<P>
The IObjectObserverlist implementation used here is thread-safe. However,
the observers will be called in the context of the thread that called
the IObjectObserverlist notification methods, not in the context of the
thread that registered the observer! In a multithreaded environment the
observer methods themselves need also to be thread-safe. }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit ObserverListU;

interface

uses Classes, ObserverIntfU;

{: Returns an interface for a new object that implements both the
  IObjectObserverlist and ISubject interfaces. The returned interface
  is the only one for the created object around, it controls the objects
  lifetime. }
function NewObjectObserverlist: IObjectObserverlist;

implementation

uses Sysutils, SyncObjs, ObserverProxyU;

type
  TListOpKind = (lokAdd, lokChange, lokRemove);

  TObserverlist = class;

  IBackdoor = interface(IInterface)
  ['{740A95D9-4290-4318-95BB-4A4D6DF1FB94}']
    function GetObject: TObserverlist;
  end;

  TObserverlist = class( TInterfacedObject, ISubject, IObjectObserverlist,
    IBackdoor )
  private
    FList: TObserverProxyList;
    FGuardian: TCriticalSection;
  protected
    function Lock: TObserverProxyList;
    procedure Unlock;
    procedure DoListOperation(Kind: TListOpKind; Obj: TObject;
      aChangeKind: TChangeKind = 0);

    { Methods of the IObjectObserverlist interface }
    procedure AddOrModify(const aObserver: IObjectObserver;
      const ChangesOfInterest: TChangeKindSet = []);
    function Contains(const aObserver: IObjectObserver): Boolean;
    procedure NotifyOnAdd(Obj: TObject);
    procedure NotifyOnChange(Obj: TObject; aChangeKind: TChangeKind = 0);
    procedure NotifyOnRemove(Obj: TObject);
    procedure Remove(const aObserver: IObjectObserver);
    procedure Assign( Source: IObjectObserverList );

    { Methods of the ISubject interface }
    procedure ISubject.Subscribe = AddOrModify;
    procedure ISubject.Unsubscribe = Remove;

    { Methods of the IBackdoor interface }
    function GetObject: TObserverlist;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function NewObjectObserverlist: IObjectObserverlist;
begin
  Result := TObserverlist.Create;
end;

{ TObserverlist }

procedure TObserverlist.AddOrModify(const aObserver: IObjectObserver;
    const ChangesOfInterest: TChangeKindSet = []);
var
  i: Integer;
  obj: TObserverProxy;
begin
  Assert( Assigned( aObserver ));
  Lock;
  try
    i:= FList.IndexOf(aObserver);
    if i >= 0 then
      FList[i].KindSet := ChangesOfInterest
    else begin
      obj := TObserverProxy.Create(aObserver, ChangesOfInterest);
      try
        FList.Add(obj);
      except
        obj.Free;
        raise;
      end; { try except }
    end; { else }
  finally
    Unlock;
  end; { try finally }
end;

procedure TObserverlist.Assign(Source: IObjectObserverList);
var
  SourceList: TObserverProxyList;
  SourceObj : TObserverlist;
begin
  SourceObj := (Source as IBackdoor).GetObject;
  Lock;
  try
    Sourcelist := SourceObj.Lock;
    try
      Flist.Assign(SourceList);
    finally
      SourceObj.Unlock;
    end; { try finally }
  finally
    Unlock;
  end; { try finally }
end;

function TObserverlist.Contains(const aObserver: IObjectObserver):
    Boolean;
begin
  Lock;
  try
    Result := FList.IndexOf(aObserver) >= 0;
  finally
    Unlock;
  end; { try finally }
end;

constructor TObserverlist.Create;
begin
  inherited;
  FList:= TObserverProxyList.Create;
  FGuardian:= TCriticalSection.Create;
end;

destructor TObserverlist.Destroy;
begin
  Lock;
  FList.Free;
  Unlock;
  FreeAndNil(FGuardian);
  inherited;
end;

procedure TObserverlist.DoListOperation(Kind: TListOpKind; Obj: TObject;
  aChangeKind: TChangeKind);
var
  i: Integer;
begin
  Lock;
  try
    for i:= FLIst.LastIndex downto 0 do
      case Kind of
        lokAdd: FList[i].NotifyOnAdd( Obj );
        lokChange: FList[i].NotifyOnChange( Obj, aChangeKind );
        lokRemove: FList[i].NotifyOnRemove( Obj );
      else
        Assert(false, 'Unknown list operation kind');
      end;
  finally
    Unlock;
  end; { try finally }
end;

function TObserverlist.GetObject: TObserverlist;
begin
  Result := self;
end;

function TObserverlist.Lock: TObserverProxyList;
begin
  if Assigned( FGuardian ) then
    FGuardian.Acquire;
  Result := FList;
end;

procedure TObserverlist.NotifyOnAdd(Obj: TObject);
begin
  Assert( Assigned( Obj ));
  DoListOperation(lokAdd, Obj);
end;

procedure TObserverlist.NotifyOnChange(Obj: TObject;
  aChangeKind: TChangeKind);
begin
  Assert( Assigned( Obj ));
  DoListOperation(lokChange, Obj, aChangeKind);
end;

procedure TObserverlist.NotifyOnRemove(Obj: TObject);
begin
  Assert( Assigned( Obj ));
  DoListOperation(lokRemove, Obj);
end;

procedure TObserverlist.Remove(const aObserver: IObjectObserver);
var
  i: Integer;
begin
  Assert( Assigned( aObserver ));
  Lock;
  try
    i:= FList.IndexOf(aObserver);
    if i >= 0 then
      FList.Delete(i);
  finally
    Unlock;
  end; { try finally }
end;

procedure TObserverlist.Unlock;
begin
  if Assigned( FGuardian ) then
    FGuardian.Release;
end;

end.
