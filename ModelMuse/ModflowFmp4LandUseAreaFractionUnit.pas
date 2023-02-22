unit ModflowFmp4LandUseAreaFractionUnit;

interface

uses
  ModflowFmp4BoundaryUnit, ModflowBoundaryUnit, SubscriptionUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

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
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultLandUseFractionTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultLandUseAreaFractionCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultLandUseAreaFractionBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4LandUseAreaFraction = 'Fmp4_Land_Use_Area_Fraction';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

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

{ TLandUseFractionTimeListLink }

constructor TFmp4MultLandUseFractionTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4LandUseAreaFraction;
  end;
end;

{ TFmp4MultLandUseAreaFractionCollection }

class function TFmp4MultLandUseAreaFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultLandUseFractionTimeListLink;
end;

function TFmp4MultLandUseAreaFractionCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.LandUseFraction.FarmOption = foTransient)
      and (LandUse.LandUseFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultLandUseAreaFractionBoundary }

class function TFmp4MultLandUseAreaFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultLandUseAreaFractionCollection;
end;

procedure TFmp4MultLandUseAreaFractionBoundary.InvalidateDisplay;
begin
  inherited;
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultLandUseAreaFractionBoundary.InvalidateLandUseData(
  Sender: TObject);
begin
//  inherited;
  if ParentModel <>  nil then
  begin
    (ParentModel as TCustomModel).InvalidateMfFmp4LandUseAreaFraction(nil);
  end;
end;

function TFmp4MultLandUseAreaFractionBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.LandUseFraction.FarmOption = foTransient)
      and (LandUse.LandUseFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

class function TFmp4MultLandUseAreaFractionBoundary.ValueDescription: string;
begin
  result := 'Land_Use_Area_Fractions';
end;

class function TFmp4MultLandUseFractionTimeListLink.GetDescription: string;
begin
  result := 'Land_Use_Area_Fractions';
end;

function TFmp4MultLandUseFractionTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.LandUseFraction.FarmOption = foTransient)
      and (LandUse.LandUseFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

end.
