unit ModflowFmp4PotentialEvapBareUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4BareEvapTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4BareEvapCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4BareEvapBoundary = class(TFmp4Boundary)
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
  StrFmp4BareEvap = 'Fmp4_Potential_Bare_Evaporation';

{ TFmp4BareEvapTimeListLink }

procedure TFmp4BareEvapTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4BareEvapTimeListLink.GetDescription: string;
begin
  result := StrFmp4BareEvap;
end;

{ TFmp4BareEvapCollection }

class function TFmp4BareEvapCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4BareEvapTimeListLink;
end;

{ TFmp4BareEvapBoundary }

class function TFmp4BareEvapBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4BareEvapCollection;
end;

procedure TFmp4BareEvapBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4BareEvapBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4BareEvap(self);
    end;
  end;
end;

class function TFmp4BareEvapBoundary.ValueDescription: string;
begin
  result := StrFmp4BareEvap;
end;

end.
