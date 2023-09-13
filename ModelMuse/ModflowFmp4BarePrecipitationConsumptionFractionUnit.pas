unit ModflowFmp4BarePrecipitationConsumptionFractionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4BarePrecipitationConsumptionFractionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4BarePrecipitationConsumptionFractionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4BarePrecipitationConsumptionFractionBoundary = class(TFmp4Boundary)
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
  StrFmp4BarePrecipitationConsumptionFraction = 'Fmp4_Bare_Precipitation_Consumption_Fraction';

{ TFmp4BarePrecipitationConsumptionFractionTimeListLink }

procedure TFmp4BarePrecipitationConsumptionFractionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4BarePrecipitationConsumptionFractionTimeListLink.GetDescription: string;
begin
  result := StrFmp4BarePrecipitationConsumptionFraction;
end;

{ TFmp4BarePrecipitationConsumptionFractionCollection }

class function TFmp4BarePrecipitationConsumptionFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4BarePrecipitationConsumptionFractionTimeListLink;
end;

{ TFmp4BarePrecipitationConsumptionFractionBoundary }

class function TFmp4BarePrecipitationConsumptionFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4BarePrecipitationConsumptionFractionCollection;
end;

procedure TFmp4BarePrecipitationConsumptionFractionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4BarePrecipitationConsumptionFractionBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4BarePrecipitationConsumptionFraction(self);
    end;
  end;
end;

class function TFmp4BarePrecipitationConsumptionFractionBoundary.ValueDescription: string;
begin
  result := StrFmp4BarePrecipitationConsumptionFraction;
end;

end.
