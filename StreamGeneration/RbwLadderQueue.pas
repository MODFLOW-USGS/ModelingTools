unit RbwLadderQueue;

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults,
  PriorityQueueUnit;

type
//  IValue = interface(IUnknown)
//    function GetSortValue: Double;
//    property SortValue: Double read GetSortValue;
//  end;

  TLinkItem<T> = class(TObject)
  private
    FNext: TLinkItem<T>;
    FSortValue: double;
    FValue: T;
    function GetSortValue: Double;
  public
    property SortValue: Double read GetSortValue;
    constructor Create(Value: T; SortValue: double);
    property Next: TLinkItem<T> read FNext write FNext;
//    Destructor Destroy; override;
  end;

  TBucket<T>  = class(TObject)
  private
    FNBucket: Integer;
    FLinkedList: TLinkItem<T>;
    FUniform: boolean;
  public
    procedure Push(Item: TLinkItem<T>);
    function Peek: TLinkItem<T>;
    function Pop: TLinkItem<T>;
    property Count: Integer read FNBucket;
    destructor Destroy; override;
  end;

  TBucketList<T> = class(TObjectList<TBucket<T>>)
  private
    FBucketWidth: double;
    FUsedBucketCount: integer;
    FRCur: double;
    FRStart: double;
    FCurrentBucketIndex: Integer;
    procedure SetUsedBucketCount(const Value: integer);
  public
    property BucketWidth: double read FBucketWidth write FBucketWidth;
    property UsedBucketCount: integer read FUsedBucketCount write SetUsedBucketCount;
    property RCur: double read FRCur write FRCur;
    property RStart: double read FRStart write FRStart;
    property CurrentBucketIndex: Integer read FCurrentBucketIndex
      write FCurrentBucketIndex;
  end;

  TRungList<T> = class(TObjectList<TBucketList<T>>);

  TLinkBinaryQueue<T> = class(TRbwPriorityQueue<TLinkItem<T>>)
  public
    constructor Create;
//  protected
//    function Compare (Left, Right: TLinkItem<T>): Integer; override;
  end;

  TRbwLadderQueue<T> = class(TObject)
  private
    const
    THRES = 50;
    procedure SetNRung(const Value: Integer);
    var
    // top variables
    FMaxTS: Double;
    FMinTS: Double;
    FNTop: Integer;
    FTopStart: double;
    FTop: TLinkItem<T>;
    // Ladder variables
    FNRung: Integer;
    FRungList: TRungList<T>;
    // Bottom variables;
    FNBot: Integer;
    FBottom: TLinkItem<T>;
    // other
    FUnusedLinks: TLinkItem<T>;
    FBelowBottom: TLinkBinaryQueue<T>;
    FCount: Integer;
    FUniformBottom: Boolean;
    function GetLink(Item: T; Value: double): TLinkItem<T>;
    procedure ReleaseLink(Link: TLinkItem<T>);
    procedure Spawn(ABucket: TBucket<T>);
    property NRung: Integer read FNRung write SetNRung;
    procedure TransferToBottom(ABucket: TBucket<T>);
    // transfer from top to ladder
    procedure TransferToLadder;
    procedure InsertInBottom(Item: TLinkItem<T>);
    procedure TransferBottomToBucket;
    procedure AddToLadder(Item: TLinkItem<T>);
    procedure TransferNextLadderBucketToBottom;
    procedure AddToTop(Item: TLinkItem<T>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(Item: T; Value: double);
    property Count: Integer read FCount;
    function Dequeue: T;
//    function Peek: T;
  end;

implementation

uses
  System.Math, System.SysUtils;

{ TLinkItem<T> }

constructor TLinkItem<T>.Create(Value: T; SortValue: double);
begin
  FSortValue := SortValue;
  FValue := Value;
end;

//destructor TLinkItem<T>.Destroy;
//begin
////  Beep;
//  inherited;
//end;

function TLinkItem<T>.GetSortValue: Double;
begin
  Result := FSortValue;
end;

{ TBucket<T> }

function TBucket<T>.Peek: TLinkItem<T>;
begin
  if FLinkedList = nil then
  begin
    result := nil;
  end
  else
  begin
    result := FLinkedList;
  end;
end;

function TBucket<T>.Pop: TLinkItem<T>;
var
  Item: TLinkItem<T>;
begin
  result := FLinkedList;
  if FLinkedList <> nil then
  begin
    FLinkedList := FLinkedList.Next;
    Dec(FNBucket);
  end;
  if result <> nil then
  begin
    result.Next := nil;
  end;
end;

procedure TBucket<T>.Push(Item: TLinkItem<T>);
var
  NewItem: TLinkItem<T>;
  OtherItem: TLinkItem<T>;
  PreviousItem: TLinkItem<T>;
begin
  if FLinkedList = nil then
  begin
    FUniform := True;
  end
  else if FLinkedList.SortValue <> Item.SortValue then
  begin
    FUniform := False;
  end;
  Assert(Item <> nil);
  Item.Next := FLinkedList;
  FLinkedList := Item;
  Inc(FNBucket);
end;

destructor TBucket<T>.Destroy;
var
  Item: TLinkItem<T>;
begin
  while FLinkedList <> nil do
  begin
    Item := FLinkedList;
    FLinkedList := Item.Next;
    Item.Free;
  end;
  inherited;
end;

{ TRbwLadderQueue<T> }

procedure TRbwLadderQueue<T>.AddToLadder(Item: TLinkItem<T>);
var
  RungIndex: Integer;
  ARung: TBucketList<T>;
  BucketIndex: Integer;
  Bucket: TBucket<T>;
begin
  RungIndex := 0;
  if NRung = 0 then
  begin
    NRung := 1;
  end;
  ARung := FRungList[RungIndex];
  if Item.SortValue < ARung.FRStart then
  begin
    FBelowBottom.Enqueue(Item);
    Exit;
  end;

  BucketIndex := Floor((Item.SortValue-ARung.FRStart)/ARung.BucketWidth);
  if BucketIndex < ARung.FCurrentBucketIndex then
  begin
    InsertInBottom(Item);
    Exit;
  end;
  while (BucketIndex = ARung.FCurrentBucketIndex) and (RungIndex < NRung-1) do
  begin
    Inc(RungIndex);
    ARung := FRungList[RungIndex];
    BucketIndex := Floor((Item.SortValue-ARung.FRStart)/ARung.BucketWidth);
    if BucketIndex < ARung.FCurrentBucketIndex then
    begin
      InsertInBottom(Item);
      Exit;
    end;
  end;

  if (BucketIndex = ARung.FCurrentBucketIndex) and (RungIndex = NRung-1) then
  begin
    InsertInBottom(Item);
  end
  else
  begin
    Bucket := ARung[BucketIndex];
    Bucket.Push(Item);
  end;
end;

procedure TRbwLadderQueue<T>.AddToTop(Item: TLinkItem<T>);
begin
  if FTop = nil then
  begin
    FMaxTS := Item.SortValue;
    FMinTS := Item.SortValue;
  end
  else
  begin
    if Item.SortValue > FMaxTS then
    begin
      FMaxTS := Item.SortValue;
    end;
    if Item.SortValue < FMinTS then
    begin
      FMinTS := Item.SortValue;
    end;
  end;
  Item.Next := FTop;
  FTop := Item;
  Inc(FNTop);
end;

constructor TRbwLadderQueue<T>.Create;
begin
  inherited;
  FUnusedLinks := nil;
  FRungList := TRungList<T>.Create;
  FNRung := 0;
  FBelowBottom := TLinkBinaryQueue<T>.Create;
end;

function TRbwLadderQueue<T>.Dequeue: T;
var
  Item: TLinkItem<T>;
  CurrentRung: TBucketList<T>;
begin
  Assert(FCount > 0);
  if FBelowBottom.Count > 0 then
  begin
    Item := FBelowBottom.Dequeue;
  end
  else
  begin
    if FBottom = nil then
    begin
      if NRung = 0 then
      begin
        TransferToLadder;
      end;
      if FBottom = nil then
      begin
        Assert(NRung > 0);
        TransferNextLadderBucketToBottom;
      end;
      Assert(FBottom <> nil);
    end;
    Item := FBottom;
    Assert(Item <> nil);
    FBottom := Item.Next;
    Dec(FNBot);
    if FNBot = 1 then
    begin
      FUniformBottom := True;
    end;
  end;
  Result := Item.FValue;
  ReleaseLink(Item);
  Dec(FCount);
end;

destructor TRbwLadderQueue<T>.Destroy;
var
  Item: TLinkItem<T>;
begin
  while FUnusedLinks <> nil do
  begin
    Item := FUnusedLinks;
    FUnusedLinks := Item.Next;
    Item.Free;
  end;
  FRungList.Free;

  while FBelowBottom.Count > 0 do
  begin
    FBelowBottom.Dequeue.Free;
  end;
  FBelowBottom.Free;

  inherited;
end;

procedure TRbwLadderQueue<T>.Enqueue(Item: T; Value: double);
var
  LinkItem: TLinkItem<T>;
begin
//  if (Value > 2036.62133789063 -0.0001) and
//    (Value < 2036.62133789063 +0.0001) then
//  begin
//    Beep;
//  end;
  LinkItem := GetLink(Item, Value);
  if (NRung = 0) or (LinkItem.SortValue >= FTopStart) then
  begin
    AddToTop(LinkItem)
  end
  else
  begin
    AddToLadder(LinkItem);
  end;
  Inc(FCount);
end;

function TRbwLadderQueue<T>.GetLink(Item: T; Value: double): TLinkItem<T>;
begin
  if FUnusedLinks = nil then
  begin
    result := TLinkItem<T>.Create(Item, Value);
  end
  else
  begin
    result := FUnusedLinks;
    FUnusedLinks := FUnusedLinks.Next;
    result.FValue := Item;
    result.FSortValue := Value;
    result.Next := nil;
  end;
end;

procedure TRbwLadderQueue<T>.InsertInBottom(Item: TLinkItem<T>);
var
  OtherItem: TLinkItem<T>;
  PreviousItem: TLinkItem<T>;
begin
  OtherItem := FBottom;
  PreviousItem := nil;
  while OtherItem <> nil do
  begin
    if Item.SortValue > OtherItem.SortValue then
    begin
      PreviousItem := OtherItem;
      OtherItem := PreviousItem.Next;
      FUniformBottom := False;
    end
    else
    begin
      break
    end;
  end;
  if PreviousItem = nil then
  begin
    if FBottom <> nil then
    begin
      if Item.SortValue <> FBottom.SortValue then
      begin
        FUniformBottom := False;
      end;
    end
    else
    begin
      FUniformBottom := True;
    end;
    Item.Next := FBottom;
    FBottom := Item;
  end
  else
  begin
    PreviousItem.Next := Item;
    Item.Next := OtherItem;
  end;
  Inc(FNBot);
  if FNBot > THRES then
  begin
    TransferBottomToBucket;
  end;
end;

//function TRbwLadderQueue<T>.Peek: T;
//begin
//
//end;

procedure TRbwLadderQueue<T>.ReleaseLink(Link: TLinkItem<T>);
begin
  Link.Next := FUnusedLinks;
  FUnusedLinks := Link;
end;

procedure TRbwLadderQueue<T>.SetNRung(const Value: Integer);
begin
  FNRung := Value;
  while FRungList.Count < FNRung do
  begin
    FRungList.Add(TBucketList<T>.Create);
  end;
end;

procedure TRbwLadderQueue<T>.Spawn(ABucket: TBucket<T>);
var
  CurrentRung: TBucketList<T>;
  NewRung: TBucketList<T>;
  AnItem: TLinkItem<T>;
  NewIndex: Integer;
  NewBucket: TBucket<T>;
  Index: Integer;
begin
  Assert(NRung > 0);
  CurrentRung := FRungList[NRung-1];
  Assert(CurrentRung.CurrentBucketIndex < CurrentRung.UsedBucketCount);
  Assert(CurrentRung[CurrentRung.CurrentBucketIndex] = ABucket);

  NRung := NRung + 1;
  NewRung := FRungList[NRung-1];
  NewRung.UsedBucketCount := THRES;
  NewRung.BucketWidth := CurrentRung.BucketWidth/THRES;
  NewRung.RStart := CurrentRung.RStart
    + CurrentRung.BucketWidth * CurrentRung.CurrentBucketIndex;
  NewRung.CurrentBucketIndex := -1;
  for Index := 0 to NewRung.UsedBucketCount - 1 do
  begin
    Assert(NewRung[index].Count = 0);
  end;

  while ABucket.Count > 0 do
  begin
    AnItem := ABucket.Pop;
    NewIndex := Floor((AnItem.SortValue - NewRung.RStart)/NewRung.BucketWidth);
    if NewIndex < 0 then
    begin
      Assert(NewIndex >= 0);
    end;
    if NewIndex = NewRung.UsedBucketCount then
    begin
      Dec(NewIndex);
    end;
    Assert(NewIndex < NewRung.UsedBucketCount);
    NewBucket := NewRung[NewIndex];

    NewBucket.Push(AnItem);
  end;
end;

procedure TRbwLadderQueue<T>.TransferBottomToBucket;
var
  CurrentRung: TBucketList<T>;
  CurrentBucket: TBucket<T>;
  MinValue: Double;
  TempBottom: TLinkItem<T>;
  BucketMin: Double;
  Item: TLinkItem<T>;
  Index: Integer;
begin
  if FUniformBottom then
  begin
    Exit;
  end;
  Assert(NRung > 0);
  Assert(FNBot >= THRES);
  CurrentRung := FRungList[NRung-1];
  Assert(CurrentRung.CurrentBucketIndex >= 0);
  CurrentBucket :=  CurrentRung[CurrentRung.CurrentBucketIndex];
  Assert(CurrentBucket.Count = 0);
  Assert(CurrentBucket.FLinkedList = nil);
  MinValue := FBottom.SortValue;

  BucketMin := CurrentRung.FRStart
    + CurrentRung.BucketWidth * CurrentRung.CurrentBucketIndex;
  if BucketMin > MinValue then
  begin
    for Index := 0 to NRung - 1 do
    begin
      FRungList[index].CurrentBucketIndex := -1;
    end;
    CurrentRung.CurrentBucketIndex := -1;
//    NRung := NRung -1;
//    if NRung = 0 then
//    begin
      TempBottom := FBottom;
      FBottom := nil;
      FNBot := 0;
      while TempBottom <> nil do
      begin
        Item := TempBottom;
        TempBottom := Item.Next;
        Item.Next := nil;
        AddToLadder(Item);
      end;
//    end
//    else
//    begin
//      TransferBottomToBucket;
//    end;
    Exit;
  end;


  CurrentRung.CurrentBucketIndex := CurrentRung.CurrentBucketIndex -1;
  CurrentBucket.FLinkedList := FBottom;
  FBottom := nil;
  CurrentBucket.FNBucket := FNBot;
  FNBot := 0;
end;

procedure TRbwLadderQueue<T>.TransferNextLadderBucketToBottom;
var
  RungIndex: Integer;
  ARung: TBucketList<T>;
  ABucket: TBucket<T>;
  Index: Integer;
begin
  Assert(FNRung > 0);
  ARung := FRungList[FNRung -1];
  while (ARung.CurrentBucketIndex = ARung.UsedBucketCount -1) and (NRung > 0) do
  begin
    NRung := NRung -1;
    if NRung = 0 then
    begin
      Break;
    end;
    ARung := FRungList[FNRung -1];
  end;
  if NRung = 0 then
  begin
    TransferToLadder;
    if FBottom = nil then
    begin
      TransferNextLadderBucketToBottom;
    end;
    Exit;
  end;
  ARung.CurrentBucketIndex := ARung.CurrentBucketIndex + 1;
  Assert(ARung.CurrentBucketIndex < ARung.UsedBucketCount);
  for Index := 0 to ARung.CurrentBucketIndex - 1 do
  begin
    Assert(ARung[Index].count = 0);
  end;
  ABucket := ARung[ARung.CurrentBucketIndex];
  while (ABucket.Count = 0) and (ARung.CurrentBucketIndex < ARung.UsedBucketCount -1) do
  begin
    ARung.CurrentBucketIndex := ARung.CurrentBucketIndex + 1;
    ABucket := ARung[ARung.CurrentBucketIndex];
  end;
  if ABucket.Count = 0 then
  begin
    NRung := NRung -1;
    if NRung = 0 then
    begin
      TransferToLadder;
    end;
    if FBottom = nil then
    begin
      TransferNextLadderBucketToBottom;
    end;
  end
  else
  begin
    if (ABucket.Count >= THRES) and not ABucket.FUniform then
    begin
      Spawn(ABucket);
      TransferNextLadderBucketToBottom;
    end
    else
    begin
      TransferToBottom(ABucket);
    end;
  end;
end;

procedure TRbwLadderQueue<T>.TransferToBottom(ABucket: TBucket<T>);
var
  AList: TList<TLinkItem<T>>;
  Item: TLinkItem<T>;
  Index: Integer;
begin
  Assert(FBottom = nil);
  if ABucket.FUniform then
  begin
    FBottom := ABucket.FLinkedList;
    FNBot := ABucket.Count;
    ABucket.FLinkedList := nil;
    ABucket.FNBucket := 0;
    FUniformBottom := True;
  end
  else
  begin
    AList := TList<TLinkItem<T>>.Create;
    try
      Assert(ABucket.Count <= THRES);
      AList.Capacity := ABucket.Count;
      Item := ABucket.Pop;
      while Item <> nil do
      begin
        AList.Add(Item);
        Item := ABucket.Pop;
      end;
      Assert(ABucket.Count = 0);
      AList.Sort(TComparer<TLinkItem<T>>.Construct(
        function (const Left, Right: TLinkItem<T>): integer
        begin
          result := Sign(Left.SortValue - Right.SortValue);
        end));
      FUniformBottom := AList.First.SortValue = AList.Last.SortValue;
      FBottom := nil;
      FNBot := AList.Count;
      for Index := AList.Count - 1 downto 0 do
      begin
        Item := AList[Index];
        Item.Next := FBottom;
        FBottom := Item;
      end;
    finally
      AList.Free;
    end;
  end;
end;

procedure TRbwLadderQueue<T>.TransferToLadder;
var
  NewRung: TBucketList<T>;
  AnItem: TLinkItem<T>;
  NewIndex: Integer;
  NewBucket: TBucket<T>;
begin
  Assert(FBottom = nil);
  Assert(FBelowBottom.Count = 0);
  Assert(FTop <> nil);
  Assert(FNRung = 0);
  if FMaxTS = FMinTS then
  begin
    FBottom := FTop;
    FNBot := FNTop;
    FNTop := 0;
    FTop := nil;
    Exit;
  end;
  NRung := 1;

  NewRung := FRungList[0];
  NewRung.UsedBucketCount := FNTop+1;
  NewRung.BucketWidth := (FMaxTS-FMinTS)/FNTop;
  NewRung.RStart := FMinTS;
  NewRung.CurrentBucketIndex := -1;

  while FTop <> nil do
  begin
    AnItem := FTop;
    FTop := AnItem.Next;
    NewIndex := Floor((AnItem.SortValue - NewRung.RStart)/NewRung.BucketWidth);
//    if NewIndex = NewRung.UsedBucketCount then
//    begin
//      Dec(NewIndex);
//    end;
    Assert(NewIndex < NewRung.UsedBucketCount);
    NewBucket := NewRung[NewIndex];
    NewBucket.Push(AnItem);
  end;

  FTopStart := NewRung.RStart + NewRung.BucketWidth*(FNTop+1);
  FNTop := 0

end;

{ TBucketList<T> }

procedure TBucketList<T>.SetUsedBucketCount(const Value: integer);
var
  ABucket: TBucket<T>;
begin
  FUsedBucketCount := Value;
  while Count < FUsedBucketCount do
  begin
    ABucket := TBucket<T>.Create;
    Add(ABucket);
  end;
end;

{ TLinkBinaryQueue<T> }

//function TLinkBinaryQueue<T>.Compare(Left, Right: TLinkItem<T>): Integer;
//begin
//  Result := Sign(Left.SortValue - Right.SortValue);
//end;

constructor TLinkBinaryQueue<T>.Create;
begin
  inherited Create(TComparer<TLinkItem<T>>.Construct(
    function (const Left, Right: TLinkItem<T>): Integer
    begin
      Result := Sign(Left.SortValue - Right.SortValue);
    end));
end;

end.
