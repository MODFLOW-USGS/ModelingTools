{== ObserverIntfU =====================================================}
{: This unit defines a number of interfaces used in implementing the
  Observer pattern for objects.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-01-20<BR>
        Last modified       2004-01-20<P>   }
{Some comments modified to make more compatible with PasDoc.
  Richard B. Winston, May 16, 2005.}
{======================================================================}
unit ObserverIntfU;

interface

const
  {: Subjects may identify the kind of change that caused the DataChange
    event to fire by passing a byte constant > 0. A value of 0 is
    reserved for indicating an unspecified change. Observers can be
    registered for specific changes.  }
  ck_Unspecified = 0;
type
  {: Enumeration for the three kinds of notifications we support in
    the Observer pattern }
  TNotificationKind= ( nkAdd, nkChange, nkRemove);

  {: Type identifying a type of change for an object }
  TChangeKind    = type byte;
  {: Type defining a set of changes an observer may be interested in }
  TChangeKindSet = set of TChangeKind;

  {: Event type used by the standard observer delegate implemented by
     TObjectObserver <see unit="ObserverU">. }
  TObserverEvent = procedure (Kind: TNotificationKInd; Obj: TObject;
    ChangeKind: TChangeKind ) of Object;

  {: Basic observer for objects }
  IObjectObserver = interface(IInterface)
  ['{2C03E445-A5F7-4964-9833-DF690C309165}']
    {: Called by the observed object when its data changed.
      @param( Obj is the observed object, guaranteed to be <> nil)
      @param(aChangeKind is an optional tag that identifies the kind of change) }
    procedure DataChanged(Obj: TObject; aChangeKind: TchangeKind = ck_Unspecified);

    {: Called by the observed object when it is about to be freed
     @param Obj is the observed object, guaranteed to be <> nil }
    procedure ObjectRemoved(Obj: TObject);
  end;

  {: Basic observer for object lists. The list informs the observer
     when objects are added or deleted from it, the object parameter
     passed to the observer methods will be the object added or
     removed from the list. For the DataChanged method, it depends on
     the particular implementation of the list whether this method
     will be called or not. If it is called the passed object will be
     an object held in the list that did change, not the list itself. }
  IObjectListObserver = interface(IObjectObserver)
  ['{A22CF43C-8FD8-46F3-9F07-67A7913780B5}']
    {: Called by the observed list if an object is added to the list.
       @param Obj is the added object, guaranteed to be <> nil. }
    procedure ObjectAdded(Obj: TObject);
  end;

  {: This is the interface an object supporting observers will offer
    the outside world to add and remove observers. }
  ISubject = interface(IInterface)
  ['{58BC2589-8C48-4048-B8E9-FE53CEB3781F}']
    {: Add an observer to the object, or modify the change set for an
      existing observer.
     @param( aObserver is the observer to add or modify)
     @param(ChangesOfInterest is a set describing the changes the observer
       wants to be informed of using the DataChange method. If this
       set is empty the method will be called for all changes.)
     @precondition aObserver <> nil  }
    procedure Subscribe(const aObserver: IObjectObserver;
      const ChangesOfInterest: TChangeKindSet = [] );

    {: Remove an observer from an object.
     @param( aObserver is the observer to remove)
     @precondition aObserver <> nil
     @desc Trying to remove an observer that is not in the objects list
       is not considered an error. }
    procedure Unsubscribe(const aObserver: IObjectObserver);
  end;

  {: This is a manager interface an object supporting observers
    will typically use internally to manage its observers. }
  IObjectObserverlist = interface(IInterface)
  ['{2CA6FFE1-4D7C-4333-A470-4003128E9414}']
    {: Add an observer to the list, or modify the change set for an
      existing observer.
     @param( aObserver is the observer to add or modify)
     @param(ChangesOfInterest is a set describing the changes the observer
       wants to be informed of using the DataChange method. If this
       set is empty the method will be called for all changes.)
     @precondition aObserver <> nil  }
    procedure AddOrModify(const aObserver: IObjectObserver;
      const ChangesOfInterest: TChangeKindSet = [] );

    {: Remove an observer from the list.
     @param aObserver is the observer to remove
     @precondition aObserver <> nil
     @desc Trying to remove an observer that is not in the list
       is not considered an error. }
    procedure Remove(const aObserver: IObjectObserver);

    {: Test if the passed observer is in the list }
    function Contains(const aObserver: IObjectObserver): Boolean;

    {: Call the DataChanged method of all observers in the list that
      are interested in the kind of change.
      @param Obj is the observed object, guaranteed to be <> nil }
    procedure NotifyOnChange(Obj: TObject;
      aChangeKind: TchangeKind = ck_Unspecified);

    {: Call the ObjectRemoved method of all observers, passing the Obj
      parameter.
      @param Obj is the removed object, guaranteed to be <> nil }
    procedure NotifyOnRemove(Obj: TObject);

    {: Call the ObectAdded method of any observers in the list that
      support the IObjectlistObserver interface.
      @param Obj is the object added, guaranteed to be <> nil }
    procedure NotifyOnAdd(Obj: TObject);

    {: Copy all Observers in the Source list to this list. }
    procedure Assign( Source: IObjectObserverList );
  end;

implementation

end.
