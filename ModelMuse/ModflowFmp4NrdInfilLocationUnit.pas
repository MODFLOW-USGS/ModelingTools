unit ModflowFmp4NrdInfilLocationUnit;

interface

uses ModflowFmp4IntBoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4NrdInfilLocationTimeListLink  = class(TFmp4IntTimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4NrdInfilLocationCollection = class(TFmp4IntCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4NrdInfilLocationBoundary = class(TFmp4IntBoundary)
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
  StrFmp4NrdInfilLocation = 'Fmp4_NRD_Infiltration_Location';

{ TFmp4NrdInfilLocationTimeListLink }

procedure TFmp4NrdInfilLocationTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4NrdInfilLocationTimeListLink.GetDescription: string;
begin
  result := StrFmp4NrdInfilLocation;
end;

{ TFmp4NrdInfilLocationCollection }

class function TFmp4NrdInfilLocationCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4NrdInfilLocationTimeListLink;
end;

{ TFmp4NrdInfilLocationBoundary }

class function TFmp4NrdInfilLocationBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4NrdInfilLocationCollection;
end;

procedure TFmp4NrdInfilLocationBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4NrdInfilLocationBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4NrdInfilLocation(self);
    end;
  end;
end;

class function TFmp4NrdInfilLocationBoundary.ValueDescription: string;
begin
  result := StrFmp4NrdInfilLocation;
end;

end.
