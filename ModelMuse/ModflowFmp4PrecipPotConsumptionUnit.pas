unit ModflowFmp4PrecipPotConsumptionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4PrecipPotConsumptionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4PrecipPotConsumptionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4PrecipPotConsumptionBoundary = class(TFmp4Boundary)
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
  StrFmp4PrecipPotConsumption = 'Fmp4_Precip_Pot_Consumption';

{ TFmp4PrecipPotConsumptionTimeListLink }

procedure TFmp4PrecipPotConsumptionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4PrecipPotConsumptionTimeListLink.GetDescription: string;
begin
  result := StrFmp4PrecipPotConsumption;
end;

{ TFmp4PrecipPotConsumptionCollection }

class function TFmp4PrecipPotConsumptionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4PrecipPotConsumptionTimeListLink;
end;

{ TFmp4PrecipPotConsumptionBoundary }

class function TFmp4PrecipPotConsumptionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4PrecipPotConsumptionCollection;
end;

procedure TFmp4PrecipPotConsumptionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4PrecipPotConsumptionBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4PrecipPotConsumption(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4PrecipPotConsumptionBoundary.ValueDescription: string;
begin
  result := StrFmp4PrecipPotConsumption;
end;

end.
