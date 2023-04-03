unit ModflowFmp4DirectRechargeUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit;

type
  TFmp4DirectRechargeTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4DirectRechargeCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4DirectRechargeBoundary = class(TFmp4Boundary)
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
  StrFmp4DirectRecharge = 'Fmp4_Direct_Recharge';

{ TFmp4DirectRechargeTimeListLink }

procedure TFmp4DirectRechargeTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4DirectRechargeTimeListLink.GetDescription: string;
begin
  result := StrFmp4DirectRecharge;
end;

{ TFmp4DirectRechargeCollection }

class function TFmp4DirectRechargeCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4DirectRechargeTimeListLink;
end;

{ TFmp4DirectRechargeBoundary }

class function TFmp4DirectRechargeBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4DirectRechargeCollection;
end;

procedure TFmp4DirectRechargeBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4DirectRechargeBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4DirectRecharge(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4DirectRechargeBoundary.ValueDescription: string;
begin
  result := StrFmp4DirectRecharge;
end;

end.
