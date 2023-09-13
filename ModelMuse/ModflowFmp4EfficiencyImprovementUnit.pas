unit ModflowFmp4EfficiencyImprovementUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, RbwParser;

type
  TFmp4EfficiencyImprovementTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
    function GetDefaultDataType: TRbwDataType; override;
  end;

  TFmp4EfficiencyImprovementCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4EfficiencyImprovementBoundary = class(TFmp4Boundary)
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
  StrFmp4EfficiencyImprovement = 'Fmp4_Efficiency_Improvement';

{ TFmp4EfficiencyImprovementTimeListLink }

procedure TFmp4EfficiencyImprovementTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

function TFmp4EfficiencyImprovementTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtBoolean;
end;

class function TFmp4EfficiencyImprovementTimeListLink.GetDescription: string;
begin
  result := StrFmp4EfficiencyImprovement;
end;

{ TFmp4EfficiencyImprovementCollection }

class function TFmp4EfficiencyImprovementCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4EfficiencyImprovementTimeListLink;
end;

{ TFmp4EfficiencyImprovementBoundary }

class function TFmp4EfficiencyImprovementBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4EfficiencyImprovementCollection;
end;

procedure TFmp4EfficiencyImprovementBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4EfficiencyImprovementBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4EfficiencyImprovement(self);
    end
    ;
  end;
end;

class function TFmp4EfficiencyImprovementBoundary.ValueDescription: string;
begin
  result := StrFmp4EfficiencyImprovement;
end;

end.
