unit ModflowFmp4TranspirationFractionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4TranspirationFractionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4TranspirationFractionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4TranspirationFractionBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultTranspirationFractionTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultTranspirationFractionCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultTranspirationFractionBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4TranspirationFraction = 'Fmp4_Transpiration_Fraction';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4TranspirationFractionTimeListLink }

procedure TFmp4TranspirationFractionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4TranspirationFractionTimeListLink.GetDescription: string;
begin
  result := StrFmp4TranspirationFraction;
end;

{ TFmp4TranspirationFractionCollection }

class function TFmp4TranspirationFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4TranspirationFractionTimeListLink;
end;

{ TFmp4TranspirationFractionBoundary }

class function TFmp4TranspirationFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4TranspirationFractionCollection;
end;

procedure TFmp4TranspirationFractionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4TranspirationFractionBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4TranspirationFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4TranspirationFractionBoundary.ValueDescription: string;
begin
  result := StrFmp4TranspirationFraction;
end;

{ TFmp4MultTranspirationFractionTimeListLink }

constructor TFmp4MultTranspirationFractionTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4TranspirationFraction;
  end;
end;

class function TFmp4MultTranspirationFractionTimeListLink.GetDescription: string;
begin
  result := 'Transpiration_Fraction';
end;

function TFmp4MultTranspirationFractionTimeListLink.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
{$IFDEF OWHMV2}
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.TranspirationFraction.FarmOption = foTransient)
      and (LandUse.TranspirationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultTranspirationFractionCollection }

class function TFmp4MultTranspirationFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultTranspirationFractionTimeListLink;
end;

function TFmp4MultTranspirationFractionCollection.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
{$IFDEF OWHMV2}
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.TranspirationFraction.FarmOption = foTransient)
      and (LandUse.TranspirationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultTranspirationFractionBoundary }

class function TFmp4MultTranspirationFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultTranspirationFractionCollection;
end;

procedure TFmp4MultTranspirationFractionBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultTranspirationFractionBoundary.InvalidateLandUseData(
  Sender: TObject);
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
      Model.InvalidateMfFmp4TranspirationFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

function TFmp4MultTranspirationFractionBoundary.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
{$IFDEF OWHMV2}
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.TranspirationFraction.FarmOption = foTransient)
      and (LandUse.TranspirationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

class function TFmp4MultTranspirationFractionBoundary.ValueDescription: string;
begin
  result := 'Transpiration_Fraction';
end;

end.
