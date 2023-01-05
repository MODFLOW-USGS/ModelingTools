unit ModflowFmp4EfficiencyUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit;

type
  TFmp4EfficiencyTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4EfficiencyCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4EfficiencyBoundary = class(TFmp4Boundary)
  private
    FUsedObserver: TObserver;
    FPestValueObserver: TObserver;
    procedure InvalidateEfficiency(Sender: TObject);
  protected
    function GetUsedObserver: TObserver; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    function BoundaryObserverPrefix: string; override;
    function GetPestValueObserver: TObserver; override;
  public
    function ValueDescription: string; override;
  end;

implementation

resourcestring
  StrFmp4Efficiency = 'Fmp4 Efficiency';

{ TFmp4EfficiencyTimeListLink }

procedure TFmp4EfficiencyTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4EfficiencyTimeListLink.GetDescription: string;
begin
  result := StrFmp4Efficiency;
end;

{ TFmp4EfficiencyCollection }

class function TFmp4EfficiencyCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4EfficiencyTimeListLink;
end;

{ TFmp4EfficiencyBoundary }

class function TFmp4EfficiencyBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4EfficiencyCollection;
end;

function TFmp4EfficiencyBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestFmp4Efficiency_';
end;

function TFmp4EfficiencyBoundary.GetPestValueObserver: TObserver;
begin
//  if FPestValueObserver = nil then
  begin
    CreateObserver('PestEfficiency_', FPestValueObserver, nil);
    FPestValueObserver.OnUpToDateSet := InvalidateEfficiency;
  end;
  result := FPestValueObserver;
end;

function TFmp4EfficiencyBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestFmpEfficieny_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFmp4EfficiencyBoundary.InvalidateEfficiency(Sender: TObject);
begin

end;

function TFmp4EfficiencyBoundary.ValueDescription: string;
begin
  result := StrFmp4Efficiency;
end;

end.
