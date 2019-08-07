{== ObserverU =========================================================}
{: This unit provides a default implementation of the IObjectObserver
  interface.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-01-20<BR>
        Last modified       2004-01-21<P>
   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit ObserverU;

interface

uses Classes, ObserverIntfU;

type
  IObserverManager= interface
  ['{C48E374A-470D-4843-92E0-A330A9987BCA}']
    procedure Disable;
    procedure Enable;
    procedure SubscribeTo( Subject: ISubject; ChangesOfInterest: TchangeKindSet );
    procedure UnsubscribeFrom( Subject: ISubject);
    function GetOnChange: TObserverEvent;
    procedure SetOnChange(Value: TObserverEvent);
    property OnChange: TObserverEvent read GetOnChange write SetOnChange;
  end;

  TObjectObserver = class( TInterfacedObject,
    IObjectObserver, IObjectListObserver, IObserverManager )
  private
    FObserverEvent: TObserverEvent;
    FDisabledCount: Integer;
  protected
    constructor InternalCreate(aHandler: TObserverEvent);
    procedure DoEvent(Kind: TNotificationKInd; Obj: TObject;
      ChangeKind: TChangeKind = ck_Unspecified);

    { Methods of the IObjectObserver and IObjectListObserver interface }
    procedure DataChanged(Obj: TObject; aChangeKind: TchangeKind = ck_Unspecified);
    procedure ObjectRemoved(Obj: TObject);
    procedure ObjectAdded(Obj: TObject);

    { Methods of the IObserverManager interface }
    procedure Disable;
    procedure Enable;
    procedure SubscribeTo( Subject: ISubject; ChangesOfInterest: TchangeKindSet );
    procedure UnsubscribeFrom( Subject: ISubject);
    function GetOnChange: TObserverEvent;
    procedure SetOnChange(Value: TObserverEvent);
  public
    class function Create(aHandler: TObserverEvent): IObjectObserver;
  end;

implementation

uses
// rbw begin change
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
// rbw end change
  SysUtils;

{ TObjectObserver }

constructor TObjectObserver.InternalCreate(aHandler: TObserverEvent);
begin
  inherited Create;
  FObserverEvent := aHandler;
end;

class function TObjectObserver.Create(
  aHandler: TObserverEvent): IObjectObserver;
begin
  Result := InternalCreate(aHandler) as IObjectObserver;
end;

procedure TObjectObserver.DataChanged(Obj: TObject;
  aChangeKind: TchangeKind);
begin
  DoEvent( nkChange, Obj, aChangeKind );
end;

procedure TObjectObserver.Disable;
begin
  InterlockedIncrement( FDisabledCount );
end;

procedure TObjectObserver.DoEvent(Kind: TNotificationKInd; Obj: TObject;
  ChangeKind: TChangeKind);
begin
  if (FDisabledCount = 0) and Assigned( FObserverEvent ) then
    FObserverEvent( Kind, Obj, ChangeKind );
end;

procedure TObjectObserver.Enable;
begin
  InterlockedDecrement( FDisabledCount );
end;

function TObjectObserver.GetOnChange: TObserverEvent;
begin
  Result := FObserverEvent;
end;

procedure TObjectObserver.ObjectAdded(Obj: TObject);
begin
  DoEvent( nkAdd, Obj );
end;

procedure TObjectObserver.ObjectRemoved(Obj: TObject);
begin
  DoEvent( nkRemove, Obj );
end;

procedure TObjectObserver.SetOnChange(Value: TObserverEvent);
begin
  FObserverEvent := Value;
end;

procedure TObjectObserver.SubscribeTo(Subject: ISubject; 
    ChangesOfInterest: TchangeKindSet );
begin
  Assert( Assigned( Subject ));
  Subject.Subscribe( Self As IObjectObserver, ChangesOfInterest );
end;

procedure TObjectObserver.UnsubscribeFrom(Subject: ISubject);
begin
  Assert( Assigned( Subject ));
  Subject.Unsubscribe( Self as IObjectObserver );
end;

end.
