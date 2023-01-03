unit ModflowFmp4EfficiencyUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4EfficiencyTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4EfficiencyCollection = class(TFmp4Collection)
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4EfficiencyBoundary = class(TFmp4Boundary)
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  end;

implementation

{ TFmp4EfficiencyTimeListLink }

procedure TFmp4EfficiencyTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4EfficiencyTimeListLink.GetDescription: string;
begin
  result := 'Fmp4 Efficiency';
end;

{ TFmp4EfficiencyCollection }

function TFmp4EfficiencyCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4EfficiencyTimeListLink;
end;

{ TFmp4EfficiencyBoundary }

class function TFmp4EfficiencyBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4EfficiencyCollection;
end;

end.
