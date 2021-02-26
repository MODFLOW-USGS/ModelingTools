{@abstract(@name is used to define @link(TObserver) which is used to
  coordinate when @link(DataSetUnit.TDataArray)s need to be updated.)}
unit SubscriptionUnit;

interface

uses System.Types, Classes, SysUtils, ObserverIntfU, ObserverListU, ObserverU,
  Generics.Collections;

const
  // @name is used in TObserver.@link(TObserver.IsRecursive) to
  // see if a @link(TObserver) is directly or indirectly observing itself.
  // See TObserver.@link(TObserver.ObserverNotification) and TChangeKind
  // (defined in ObserverIntfU).
  ckCheckDependance = 1;
  // @name is used in TObserver.@link(TObserver.IsRecursive) to
  // see if a @link(TObserver) is directly or indirectly observing itself.
  // See TObserver.@link(TObserver.ObserverNotification) and TChangeKind
  // (defined in ObserverIntfU).
  ckResetObserved = 2;

type

  {@abstract(@name is used to
  coordinate when @link(DataSetUnit.TDataArray)s need to be updated.)
  }
  TObserver = class(TComponent)
  private
    // @name: boolean;
    // See @link(Observed).
    FObserved: boolean;
    // @name: IObjectObserver;
    // IObjectObserver is defined in ObserverIntfU.
    // See @link(Observer).
    FObserver: IObjectObserver;
    // @name: IObjectObserverList;
    // IObjectObserverList is defined in ObserverIntfU.
    // See @link(ObserverList).
    FObserverList: IObjectObserverList;
    // @name: TNotifyEvent;
    // See @link(OnNotify).
    FOnNotify: TNotifyEvent;
    // @name: TNotifyEvent;
    // See @link(OnUpToDateSet).
    FOnUpToDateSet: TNotifyEvent;
    { TODO : Could FSubscribers be deleted and its
      functionality be transferred to FObserverList? }
    { @name: TList;
      @name is a list of (@classname)s that should be notified
      when there is a change in the current object.}
    FSubscribers: TList;
    // @name: boolean;
    // See @link(UpToDate).
    FUpToDate: boolean;
    FOnNameChange: TNotifyEvent;
    // See @link(Name).
    function GetName: TComponentName;
    // See @link(Subject).
    function GetSubject: ISubject;
    function GetObserverList: IObjectObserverList;
    // @name is an interface implemented as a TObjectObserver
    // which is used in handling subscriptions.  See @link(TalksTo).
    // TObjectObserver is defined in ObserverU.
    property Observer: IObjectObserver read FObserver;
    // @name is an interface implemented as a
    // ObserverListU.TObserverlist
    // which is used in handling subscriptions.  See @link(TalksTo).
    property Subject: ISubject read GetSubject;
  protected
    // See @link(Name).
    procedure SetName(const Value: TComponentName); override;
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean); virtual;
  public
    // @name is used in @link(IsRecursive) to see if a @classname is
    // observing itself.
    property Observed: boolean read FObserved write FObserved;
   // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name checks whether an instance of @name is directly or indirectly
    // observing itself.  @name should always return false because
    // if it is observing itself, there has been an error.
    function IsRecursive: boolean;
    // @name is an interface to a list of the objects which
    // observe the current object.
    property ObserverList: IObjectObserverList read GetObserverList;
    // @name responds to ChangeKind = @link(ckResetObserved) be setting
    // @link(Observed) to false for itself and everything that observes it.
    // @name responds to ChangeKind = @link(ckCheckDependance) by
    // setting @link(Observed) to true
    // for itself and everything that observes it.
    // For any other value of ChangeKind, it sets @link(UpToDate)
    // to false.  If @link(UpToDate) was @true, it calls
    // @link(ObserverList).NotifyOnChange.
    // @name also call @link(OnNotify).
    procedure ObserverNotification(Kind: TNotificationKInd; Obj: TObject;
      ChangeKind: TChangeKind); virtual;
    // @name removes all subscribers to the current object.
    // See @link(StopsTalkingTo). @name is called in @link(Destroy).
    procedure StopTalkingToAnyone; virtual;
    {@name causes Observer to be notified when there is a change
     in this object.
     @name calls
     @longcode(#
     Subject.Subscribe(Observer.Observer);
     #)
     See @link(StopsTalkingTo), @link(Subject), and @link(Observer).}
    procedure TalksTo(const Observer: TObserver);
    // @name removes Observer from the list of items
    // to be notified when there is a change
    // in this object. See @link(TalksTo) and @link(StopTalkingToAnyone).
    procedure StopsTalkingTo(const Observer: TObserver);
    // UpdateWithName assigns Name := AName.
    procedure UpdateWithName(const AName: string); virtual;
    // @name indicates whether the current @classname is up-to-date.
    // Changing @name from @true to @false will call all the things that
    // observe the current @classname to also have their @name set to false.
    property UpToDate: boolean read FUpToDate write SetUpToDate;
    function IsListeningTo(const AnotherObserver: TObserver): boolean;
    property OnNameChange: TNotifyEvent read FOnNameChange write FOnNameChange;
  published
    // @name is the name of the @classname.
    property Name: TComponentName read GetName write SetName;
    // @name is called in @link(ObserverNotification);
    property OnNotify: TNotifyEvent read FOnNotify write FOnNotify;
    // @name is called whenever @link(UpToDate) is set even if @link(UpToDate)
    // is not changed.
    property OnUpToDateSet: TNotifyEvent read FOnUpToDateSet write
      FOnUpToDateSet;
  end;

  TObserverList = TList<TObserver>;
  TObserverObjectList = TObjectList<TObserver>;

implementation

uses Contnrs;

constructor TObserver.Create(AnOwner: TComponent);
begin
  inherited Create(nil);
  FObserver := TObjectObserver.Create(ObserverNotification);
end;

procedure TObserver.UpdateWithName(const AName: string);
begin
  Name := AName;
end;

destructor TObserver.Destroy;
begin
  StopTalkingToAnyone;
  FSubscribers.Free;
  FObserver := nil;
  inherited;
end;

function TObserver.GetName: TComponentName;
begin
  result := inherited Name;
end;

function TObserver.GetObserverList: IObjectObserverList;
begin
  if not Assigned(FObserverlist) then
  begin
    FObserverList := NewObjectObserverlist;
  end;
  result := FObserverList;
end;

function TObserver.IsListeningTo(const AnotherObserver: TObserver): boolean;
begin
  Observed := False;
  AnotherObserver.ObserverList.NotifyOnChange(AnotherObserver, ckResetObserved);
  AnotherObserver.ObserverList.NotifyOnChange(AnotherObserver, ckCheckDependance);
  result := Observed;
end;

function TObserver.IsRecursive: boolean;
begin
  Observed := False;
  ObserverList.NotifyOnChange(self, ckResetObserved);
  ObserverList.NotifyOnChange(self, ckCheckDependance);
  result := Observed;
end;

procedure TObserver.SetName(const Value: TComponentName);
var
  OldName: TComponentName;
begin
  if Name <> Value then
  begin
    OldName := Name;
    inherited SetName(Value);
    if (OldName <> '') and Assigned(OnNameChange) then
    begin
      OnNameChange(self);
    end;
  end;
end;

procedure TObserver.SetUpToDate(const Value: boolean);
var
  Changed: boolean;
  AnotherObserver: TObserver;
  ListenerIndex: Integer;
begin
  Changed := False;
  if FUpToDate <> Value then
  begin
    FUpToDate := Value;
    Changed := True;
  end;
  if Assigned(FOnUpToDateSet) then
  begin
    FOnUpToDateSet(self);
  end;
  if not FUpToDate and Changed then
  begin
    ObserverList.NotifyOnChange(self);

//    try
      if FSubscribers <> nil then
      begin
        for ListenerIndex := 0 to FSubscribers.Count - 1 do
        begin
          AnotherObserver := FSubscribers[ListenerIndex];

//          AnotherObserver.UpToDate := True
//          causes an access violation if a data set is colored
//          while changing the model selection.
//          if not AnotherObserver.UpToDate then
//          begin
//            AnotherObserver.UpToDate := True;
//          end;

          AnotherObserver.UpToDate := False;
        end;
      end;
//
//      ShowMessage(Name);
//

  end;
end;

function TObserver.GetSubject: ISubject;
begin
  if not Assigned(FObserverlist) then
  begin
    FObserverList := NewObjectObserverlist;
  end;
  Result := FObserverlist as ISubject;
end;

procedure TObserver.ObserverNotification(Kind: TNotificationKInd;
  Obj: TObject; ChangeKind: TChangeKind);
begin
  if ChangeKind = ckResetObserved then
  begin
    if Observed then
    begin
      Observed := False;
      if Assigned(FObserverList) then
      begin
        FObserverList.NotifyOnChange(Obj, ChangeKind);
      end;
    end;
  end
  else if ChangeKind = ckCheckDependance then
  begin
    if not Observed then
    begin
      Observed := True;
      if Assigned(FObserverList) then
      begin
        FObserverList.NotifyOnChange(Obj, ChangeKind);
      end;
    end;
  end
  else
  begin
    if UpToDate then
    begin
      UpToDate := False;
      if Assigned(FObserverList) then
      begin
        FObserverList.NotifyOnChange(self, ChangeKind);
      end;
    end;
    if Assigned(FOnNotify) then
    begin
      FOnNotify(self);
    end;
  end;
end;

procedure TObserver.TalksTo(const Observer: TObserver);
begin
  Assert(Observer <> nil);
  Subject.Subscribe(Observer.Observer);
  if FSubscribers = nil then
  begin
    FSubscribers := TList.Create;
  end;
  if FSubscribers.IndexOf(Observer) < 0 then
  begin
    FSubscribers.Add(Observer);
    // is the following OK?
    // The following may be needed when adding or removing
    // a new HUF multiplier or zone array.
    Observer.UpToDate := False;
  end;
end;

procedure TObserver.StopsTalkingTo(const Observer: TObserver);
begin
  try
    if (Observer <> nil) and Assigned(Observer.Observer)
      and Assigned(Subject) then
    begin
      Subject.Unsubscribe(Observer.Observer);
    end;
    if (FSubscribers <> nil) and (FSubscribers.IndexOf(Observer) >= 0) then
    begin
      FSubscribers.Remove(Observer);
      // is the following OK?
      // The following may be needed when adding
      // or removing a new HUF multiplier or zone array.
      if Observer <> nil then
      begin
        Observer.UpToDate := False;
      end;
    end;
  except on E: Exception do
    begin
      E.Message := E.Message + '; in observer "' + Name + '."';
      raise;
    end;
  end;
end;

procedure TObserver.StopTalkingToAnyone;
var
  Index: integer;
  Observer: TObserver;
begin
  if FSubscribers = nil then
  begin
    Exit;
  end;
  for Index := FSubscribers.Count - 1 downto 0 do
  begin
    Observer := FSubscribers[Index];
    if Observer <> nil then
    begin
      StopsTalkingTo(Observer);
    end;
  end;
  FreeAndNil(FSubscribers);
end;

end.

