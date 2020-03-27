unit ObservationComparisonsUnit;

interface

uses
  System.Classes, GoPhastTypes, PestObsUnit;

type
  // Compare two @link(TCustomObservationItem)s in different objects.
  TGlobalObsComparisonItem = class(TCustomObservationItem)
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

  TGlobalObservationComparisons = class(TScreenObjectOwnerCollection)
  private
    function GetItem(Index: Integer): TGlobalObsComparisonItem;
    procedure SetItem(Index: Integer; const Value: TGlobalObsComparisonItem);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TGlobalObsComparisonItem read GetItem
      write SetItem; default;
    function Add: TGlobalObsComparisonItem;
   end;

implementation

{ TGlobalObsComparisonItem }

procedure TGlobalObsComparisonItem.Assign(Source: TPersistent);
var
  CompareItem: TGlobalObsComparisonItem;
begin
  if Source is TGlobalObsComparisonItem then
  begin
    CompareItem := TGlobalObsComparisonItem(Source);
    Guid1 := CompareItem.Guid1;
    Guid2 := CompareItem.Guid2;
  end;
  inherited;

end;

procedure TGlobalObsComparisonItem.SetGuid1(const Value: string);
begin
  SetStringProperty(FGuid1, Value);
end;

procedure TGlobalObsComparisonItem.SetGuid2(const Value: string);
begin
  SetStringProperty(FGuid2, Value);
end;

{ TGlobalObservationComparisons }

function TGlobalObservationComparisons.Add: TGlobalObsComparisonItem;
begin
  result := inherited Add as TGlobalObsComparisonItem;
end;

procedure TGlobalObservationComparisons.Assign(Source: TPersistent);
var
  ComparisonsSource: TGlobalObservationComparisons;
  ItemIndex: Integer;
begin
  if Source is TGlobalObservationComparisons then
  begin
    ComparisonsSource := TGlobalObservationComparisons(Source);
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

constructor TGlobalObservationComparisons.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TGlobalObsComparisonItem, InvalidateModelEvent, nil);
end;

function TGlobalObservationComparisons.GetItem(Index: Integer): TGlobalObsComparisonItem;
begin
  result := inherited Items[Index] as TGlobalObsComparisonItem;
end;

procedure TGlobalObservationComparisons.SetItem(Index: Integer;
  const Value: TGlobalObsComparisonItem);
begin
  inherited Items[Index] := Value;
end;

end.
