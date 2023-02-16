unit ModflowFmp4LandUseAreaFractionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit;

type
  TFmp4LandUseAreaFractionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4LandUseAreaFractionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4LandUseAreaFractionBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

resourcestring
  StrFmp4LandUseAreaFraction = 'Fmp4_Land_Use_Area_Fraction';

implementation

uses
  PhastModelUnit, GoPhastTypes;

{ TFmp4LandUseAreaFractionTimeListLink }

procedure TFmp4LandUseAreaFractionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4LandUseAreaFractionTimeListLink.GetDescription: string;
begin
  result := StrFmp4LandUseAreaFraction;
end;

{ TFmp4LandUseAreaFractionCollection }

class function TFmp4LandUseAreaFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4LandUseAreaFractionTimeListLink;
end;

{ TFmp4LandUseAreaFractionBoundary }

class function TFmp4LandUseAreaFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4LandUseAreaFractionCollection;
end;

procedure TFmp4LandUseAreaFractionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4LandUseAreaFractionBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
  {$IFDEF OWHMV2}
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4LandUseAreaFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4LandUseAreaFractionBoundary.ValueDescription: string;
begin
  result := StrFmp4LandUseAreaFraction;
end;

end.
