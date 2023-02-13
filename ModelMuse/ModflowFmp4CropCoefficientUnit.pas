unit ModflowFmp4CropCoefficientUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit;

type
  TFmp4CropCoefficientTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4CropCoefficientCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4CropCoefficientBoundary = class(TFmp4Boundary)
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
  StrFmp4CropCoefficient = 'Fmp4_Crop_Coefficient';

{ TFmp4CropCoefficientTimeListLink }

procedure TFmp4CropCoefficientTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4CropCoefficientTimeListLink.GetDescription: string;
begin
  result := StrFmp4CropCoefficient;
end;

{ TFmp4CropCoefficientCollection }

class function TFmp4CropCoefficientCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4CropCoefficientTimeListLink;
end;

{ TFmp4CropCoefficientBoundary }

class function TFmp4CropCoefficientBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4CropCoefficientCollection;
end;

procedure TFmp4CropCoefficientBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4CropCoefficientBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4CropCoefficient(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4CropCoefficientBoundary.ValueDescription: string;
begin
  result := StrFmp4CropCoefficient;
end;

end.
