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
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4EfficiencyBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

implementation

uses
  PhastModelUnit, GoPhastTypes;

resourcestring
  StrFmp4Efficiency = 'Fmp4_Efficiency';

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

procedure TFmp4EfficiencyBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4EfficiencyBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4Efficiency(self);
    end
    ;
  end;
end;

class function TFmp4EfficiencyBoundary.ValueDescription: string;
begin
  result := StrFmp4Efficiency;
end;

end.
