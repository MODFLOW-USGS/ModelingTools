unit RbwController;

interface

uses
  SysUtils, Classes, Controls;

type
  TRbwController = class;

  TControlItem = class(TCollectionItem)
  private
    FControl: TControl;
    procedure SetControl(const Value: TControl);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Control: TControl read FControl write SetControl;
  end;

  TControlCollection = class(TCollection)
  private
    FOwner: TRbwController;
    function GetItems(Index: integer): TControlItem;
    procedure SetItems(Index: integer; const Value: TControlItem);
  public
    Constructor Create(Owner: TRbwController);
    property Items[Index: integer]: TControlItem read GetItems
      write SetItems; default;
  end;

  // @name has a collection of @link(TControlItem)s. Setting @link(Enabled)
  // of @name sets the enabled property of each of the
  // @link(TControlItem.Control TControlItem.Control).
  TRbwController = class(TComponent)
  private
    FControlList: TControlCollection;
    FEnabled: boolean;
    FOnEnabledChange: TNotifyEvent;
    procedure SetControlList(const Value: TControlCollection);
    procedure SetEnabled(const Value: boolean);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    property ControlList: TControlCollection read FControlList
      write SetControlList;
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property OnEnabledChange: TNotifyEvent read FOnEnabledChange
      write FOnEnabledChange;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwController]);
end;

{ TControlItem }

procedure TControlItem.Assign(Source: TPersistent);
begin
  if Source is TControlItem then
  begin
    Control := TControlItem(Source).Control;
  end;
  
end;

procedure TControlItem.SetControl(const Value: TControl);
begin
  FControl := Value;
  if FControl <> nil then
  begin
    FControl.Enabled := (Collection as TControlCollection).FOwner.Enabled;
  end;
end;

{ TControlCollection }

constructor TControlCollection.Create(Owner: TRbwController);
begin
  inherited Create(TControlItem);
  FOwner := Owner;
end;

function TControlCollection.GetItems(Index: integer): TControlItem;
begin
  result := inherited Items[Index] as TControlItem;
end;

procedure TControlCollection.SetItems(Index: integer;
  const Value: TControlItem);
begin
  inherited Items[Index].Assign(Value);
end;

{ TRbwController }

constructor TRbwController.Create(AnOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  FControlList := TControlCollection.Create(self);
end;

destructor TRbwController.Destroy;
begin
  FControlList.Free;
  inherited;
end;

procedure TRbwController.SetControlList(const Value: TControlCollection);
begin
  FControlList.Assign(Value);
end;

procedure TRbwController.SetEnabled(const Value: boolean);
var
  Index: Integer;
  Changed: Boolean;
begin
  Changed := False;
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed := True;
  end;
  for Index := 0 to ControlList.Count - 1 do
  begin
    if Assigned(ControlList.Items[Index].Control) then
    begin
      ControlList.Items[Index].Control.Enabled := FEnabled;
    end;
  end;
  if Changed and Assigned(FOnEnabledChange) then
  begin
    FOnEnabledChange(self);
  end;
end;

end.
