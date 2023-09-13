unit ModflowFmp4AddedDemandRunoffSplitUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4AddedDemandRunoffSplitTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4AddedDemandRunoffSplitCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4AddedDemandRunoffSplitBoundary = class(TFmp4Boundary)
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
  StrFmp4AddedDemandRunoffSplit = 'Fmp4_Added_Demand_Runoff_Split';

{ TFmp4AddedDemandRunoffSplitTimeListLink }

procedure TFmp4AddedDemandRunoffSplitTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4AddedDemandRunoffSplitTimeListLink.GetDescription: string;
begin
  result := StrFmp4AddedDemandRunoffSplit;
end;

{ TFmp4AddedDemandRunoffSplitCollection }

class function TFmp4AddedDemandRunoffSplitCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4AddedDemandRunoffSplitTimeListLink;
end;

{ TFmp4AddedDemandRunoffSplitBoundary }

class function TFmp4AddedDemandRunoffSplitBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4AddedDemandRunoffSplitCollection;
end;

procedure TFmp4AddedDemandRunoffSplitBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4AddedDemandRunoffSplitBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4AddedDemandRunoffSplit(self);
    end;
  end;
end;

class function TFmp4AddedDemandRunoffSplitBoundary.ValueDescription: string;
begin
  result := StrFmp4AddedDemandRunoffSplit;
end;

end.
