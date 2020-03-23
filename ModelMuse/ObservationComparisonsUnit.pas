unit ObservationComparisonsUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  // Compare two @link(TCustomObservationItem)s in different objects.
  TGlobalComparisonItem = class(TCustomObservationItem)
  private
    FGuid2: string;
    FGuid1: string;
    procedure SetGuid1(const Value: string);
    procedure SetGuid2(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Guid1: string read FGuid1 write SetGuid1;
    property Guid2: string read FGuid2 write SetGuid2;
  end;

  TGlobalComparisons = class(TPhastCollection)
  private
    function GetItem(Index: Integer): TGlobalComparisonItem;
    procedure SetItem(Index: Integer; const Value: TGlobalComparisonItem);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TGlobalComparisonItem read GetItem
      write SetItem; default;
    function Add: TGlobalComparisonItem;
   end;


implementation

{ TGlobalComparisonItem }

procedure TGlobalComparisonItem.Assign(Source: TPersistent);
var
  CompareItem: TGlobalComparisonItem;
begin
  if Source is TGlobalComparisonItem then
  begin
    CompareItem := TGlobalComparisonItem(Source);
    Guid1 := CompareItem.Guid1;
    Guid2 := CompareItem.Guid2;
  end;
  inherited;

end;

procedure TGlobalComparisonItem.SetGuid1(const Value: string);
begin
  SetStringProperty(FGuid1, Value);
end;

procedure TGlobalComparisonItem.SetGuid2(const Value: string);
begin
  SetStringProperty(FGuid2, Value);
end;

{ TGlobalComparisons }

function TGlobalComparisons.Add: TGlobalComparisonItem;
begin
  result := inherited Add as TGlobalComparisonItem;
end;

procedure TGlobalComparisons.Assign(Source: TPersistent);
var
  ComparisonsSource: TGlobalComparisons;
  ItemIndex: Integer;
begin
  if Source is TGlobalComparisons then
  begin
    ComparisonsSource := TGlobalComparisons(Source);
    Count :=  ComparisonsSource.Count;
    for ItemIndex := 0 to ComparisonsSource.Count - 1 do
    begin
      Items[ItemIndex].Assign(ComparisonsSource.Items[ItemIndex]);
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGlobalComparisons.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TGlobalComparisonItem, InvalidateModelEvent);
end;

function TGlobalComparisons.GetItem(Index: Integer): TGlobalComparisonItem;
begin
  result := inherited Items[Index] as TGlobalComparisonItem;
end;

procedure TGlobalComparisons.SetItem(Index: Integer;
  const Value: TGlobalComparisonItem);
begin
  inherited Items[Index] := Value;
end;

end.
