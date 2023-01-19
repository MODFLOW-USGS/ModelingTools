unit ModflowFmp4BareRunoffFractionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit;

type
  TFmp4BareRunoffFractionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4BareRunoffFractionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4BareRunoffFractionBoundary = class(TFmp4Boundary)
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
  StrFmp4BareRunoffFraction = 'Fmp4_Bare_Runoff_Fraction';

{ TFmp4BareRunoffFractionTimeListLink }

procedure TFmp4BareRunoffFractionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4BareRunoffFractionTimeListLink.GetDescription: string;
begin
  result := StrFmp4BareRunoffFraction;
end;

{ TFmp4BareRunoffFractionCollection }

class function TFmp4BareRunoffFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4BareRunoffFractionTimeListLink;
end;

{ TFmp4BareRunoffFractionBoundary }

class function TFmp4BareRunoffFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4BareRunoffFractionCollection;
end;

procedure TFmp4BareRunoffFractionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4BareRunoffFractionBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4BareRunoffFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4BareRunoffFractionBoundary.ValueDescription: string;
begin
  result := StrFmp4BareRunoffFraction;
end;

end.
