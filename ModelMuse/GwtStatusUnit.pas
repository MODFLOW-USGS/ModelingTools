unit GwtStatusUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TGwtBoundaryStatus = (gbsActive, gbsInactive, gbsConstant);

  TGwtBoundaryStatusArray = array of TGwtBoundaryStatus;

  TGwtBoundaryStatusItem = class(TCollectionItem)
  private
    FGwtBoundaryStatus: TGwtBoundaryStatus;
    procedure SetGwtBoundaryStatus(const Value: TGwtBoundaryStatus);
  public
    procedure Assign(Source: TPersistent); override;
  published
    Property GwtBoundaryStatus: TGwtBoundaryStatus read FGwtBoundaryStatus
      write SetGwtBoundaryStatus;
  end;

  TGwtBoundaryStatusCollection = class(TCollection)
  private
    FModel: TBaseModel;
    function GetItems(Index: Integer): TGwtBoundaryStatusItem;
    procedure SetItems(Index: Integer; const Value: TGwtBoundaryStatusItem);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  protected
    property Model: TBaseModel read FModel;
  public
    constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TGwtBoundaryStatusItem read GetItems
      write SetItems; default;
    function IsSame(OtherCollection: TGwtBoundaryStatusCollection): Boolean;
    property Count: Integer read GetCount write SetCount;
    function Add: TGwtBoundaryStatusItem;
  end;



implementation

uses
  PhastModelUnit;

{ TGwtBoundaryStatusItem }

procedure TGwtBoundaryStatusItem.Assign(Source: TPersistent);
begin
  if Source is TGwtBoundaryStatusItem then
  begin
    GwtBoundaryStatus := TGwtBoundaryStatusItem(Source).GwtBoundaryStatus;
  end
  else
  begin
    inherited;
  end;
end;

procedure TGwtBoundaryStatusItem.SetGwtBoundaryStatus(
  const Value: TGwtBoundaryStatus);
begin
  FGwtBoundaryStatus := Value;
end;

{ TGwtBoundaryStatusCollection }

function TGwtBoundaryStatusCollection.Add: TGwtBoundaryStatusItem;
begin
  result := inherited Add as TGwtBoundaryStatusItem;
end;

procedure TGwtBoundaryStatusCollection.Assign(Source: TPersistent);
begin
  if Source is TGwtBoundaryStatusCollection then
  begin
    TGwtBoundaryStatusCollection(Source).Count;
  end;
  inherited;
end;

constructor TGwtBoundaryStatusCollection.Create(Model: TBaseModel);
begin
  FModel := Model;
  inherited Create(TGwtBoundaryStatusItem);
end;

function TGwtBoundaryStatusCollection.GetCount: Integer;
var
  LocalModel: TCustomModel;
begin
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    if inherited Count < LocalModel.MobileComponents.Count then
    begin
      Count := LocalModel.MobileComponents.Count;
    end;
  end;
  result := inherited Count
end;

function TGwtBoundaryStatusCollection.GetItems(
  Index: Integer): TGwtBoundaryStatusItem;
var
  LocalModel: TCustomModel;
begin
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    if inherited Count < LocalModel.MobileComponents.Count then
    begin
      Count := LocalModel.MobileComponents.Count;
    end;
  end
  else
  begin
    while Index >= Count do
    begin
      Add;
    end;
  end;
  result := inherited Items[Index] as TGwtBoundaryStatusItem
end;

function TGwtBoundaryStatusCollection.IsSame(
  OtherCollection: TGwtBoundaryStatusCollection): Boolean;
var
  Index: Integer;
begin
  result := Count = OtherCollection.Count;
  if Result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].GwtBoundaryStatus = OtherCollection[Index].GwtBoundaryStatus;
      if not Result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TGwtBoundaryStatusCollection.SetCount(const Value: Integer);
begin
  while inherited Count < Value do
  begin
    Add;
  end;
  while inherited Count > Value do
  begin
    Delete(inherited Count -1);
  end;
end;

procedure TGwtBoundaryStatusCollection.SetItems(Index: Integer;
  const Value: TGwtBoundaryStatusItem);
begin
  inherited Items[Index] := Value;
end;

end.
