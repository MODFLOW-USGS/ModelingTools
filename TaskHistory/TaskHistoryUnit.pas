unit TaskHistoryUnit;

interface

uses
  Classes;

Type
  TDateHistoryItem = class(TCollectionItem)
  private
    FDate: TDate;
    FCategories: TStrings;
    procedure SetCategories(const Value: TStrings);
    procedure SetDate(const Value: TDate);
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Date: TDate read FDate write SetDate;
    property Categories: TStrings read FCategories write SetCategories;
  end;

  TDateHistoryCollection = class(TCollection)
  public
    Constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Sort;
  end;

  TClassificationItem = class(TCollectionItem)
  private
    FDisplay: boolean;
    FName: string;
    FColumn: integer;
    procedure SetDisplay(const Value: boolean);
    procedure SetName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property Column: integer read FColumn write FColumn;
  published
    property Name: string read FName write SetName;
    property Display: boolean read FDisplay write SetDisplay;
  end;

  TClassificationCollection = class(TCollection)
  public
    Constructor Create;
    function GetItemByName(const AClassifiation: string): TClassificationItem;
  end;

  TTaskHistory = class(TComponent)
  private
    FHistory: TDateHistoryCollection;
    FClassification: TClassificationCollection;
    procedure SetClassification(const Value: TClassificationCollection);
    procedure SetHistory(const Value: TDateHistoryCollection);
  public
    Constructor Create(AnOwner: TComponent); override;
    Destructor Destroy; override;
  published
    property Classification: TClassificationCollection read FClassification
      write SetClassification;
    property History: TDateHistoryCollection read FHistory write SetHistory;
  end;

implementation

uses
  Math;

function CompareHistoryItems(Item1, Item2: Pointer): integer;
var
  Hist1: TDateHistoryItem;
  Hist2: TDateHistoryItem;
begin
  Hist1 := Item1;
  Hist2 := Item2;
  result := Sign(Hist1.Date - Hist2.Date);
end;


{ TDateHistory }

procedure TDateHistoryItem.Assign(Source: TPersistent);
var
  LocalSource: TDateHistoryItem;
begin
  if Source is TDateHistoryItem then
  begin
    LocalSource := TDateHistoryItem(Source);
    Date := LocalSource.Date;
    Categories := LocalSource.Categories;
  end
  else
  begin
    inherited;
  end;
end;

constructor TDateHistoryItem.Create(Collection: TCollection);
begin
  inherited;
  FCategories := TStringList.Create;
//  FCategories.
end;

destructor TDateHistoryItem.Destroy;
begin
  FCategories.Free;
  inherited;
end;

procedure TDateHistoryItem.SetCategories(const Value: TStrings);
begin
  FCategories.Assign(Value);
end;

procedure TDateHistoryItem.SetDate(const Value: TDate);
begin
  FDate := Value;
end;

{ TDateHistoryCollection }

procedure TDateHistoryCollection.Assign(Source: TPersistent);
begin
  inherited;
  Sort;
end;

constructor TDateHistoryCollection.Create;
begin
  inherited Create(TDateHistoryItem);
end;

procedure TDateHistoryCollection.Sort;
var
  AList: TList;
  Index: Integer;
  Item: TDateHistoryItem;
begin
  AList := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      AList.Add(Items[Index])
    end;
    AList.Sort(CompareHistoryItems);
    for Index := 0 to AList.Count - 1 do
    begin
      Item := AList[Index];
      Item.Index := Index;
    end;
  finally
    AList.Free;
  end;
end;

{ TClassificationItem }

procedure TClassificationItem.Assign(Source: TPersistent);
var
  LocalSource: TClassificationItem;
begin
  if Source is TClassificationItem then
  begin
    LocalSource := TClassificationItem(Source);
    Name := LocalSource.Name;
    Display := LocalSource.Display;
  end
  else
  begin
    inherited;
  end;
end;

procedure TClassificationItem.SetDisplay(const Value: boolean);
begin
  FDisplay := Value;
end;

procedure TClassificationItem.SetName(const Value: string);
begin
  FName := Value;
end;

{ TClassificationCollection }

constructor TClassificationCollection.Create;
begin
  inherited Create(TClassificationItem);
end;

function TClassificationCollection.GetItemByName(
  const AClassifiation: string): TClassificationItem;
var
  Index: Integer;
  AnItem: TClassificationItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TClassificationItem;
    if AnItem.Name = AClassifiation then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

{ TTaskHistory }

constructor TTaskHistory.Create(AnOwner: TComponent);
begin
  inherited;
  FClassification := TClassificationCollection.Create;
  FHistory := TDateHistoryCollection.Create;
end;

destructor TTaskHistory.Destroy;
begin
  FHistory.Free;
  FClassification.Free;
  inherited;
end;

procedure TTaskHistory.SetClassification(
  const Value: TClassificationCollection);
begin
  FClassification.Assign(Value);
end;

procedure TTaskHistory.SetHistory(const Value: TDateHistoryCollection);
begin
  FHistory.Assign(Value);
end;

end.
