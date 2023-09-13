unit ModflowFmp4FractionOfPrecipToSurfaceWaterUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4FractionOfPrecipToSurfaceWaterTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4FractionOfPrecipToSurfaceWaterCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4FractionOfPrecipToSurfaceWaterBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultFractionOfPrecipToSurfaceWaterCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultFractionOfPrecipToSurfaceWaterBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4FractionOfPrecipToSurfaceWater = 'Fraction_of_Excess_Precip_to_SW';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4FractionOfPrecipToSurfaceWaterTimeListLink }

procedure TFmp4FractionOfPrecipToSurfaceWaterTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4FractionOfPrecipToSurfaceWaterTimeListLink.GetDescription: string;
begin
  result := StrFmp4FractionOfPrecipToSurfaceWater;
end;

{ TFmp4FractionOfPrecipToSurfaceWaterCollection }

class function TFmp4FractionOfPrecipToSurfaceWaterCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4FractionOfPrecipToSurfaceWaterTimeListLink;
end;

{ TFmp4FractionOfPrecipToSurfaceWaterBoundary }

class function TFmp4FractionOfPrecipToSurfaceWaterBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4FractionOfPrecipToSurfaceWaterCollection;
end;

procedure TFmp4FractionOfPrecipToSurfaceWaterBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4FractionOfPrecipToSurfaceWaterBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4FractionOfPrecipToSurfaceWater(self);
    end;
  end;
end;

class function TFmp4FractionOfPrecipToSurfaceWaterBoundary.ValueDescription: string;
begin
  result := StrFmp4FractionOfPrecipToSurfaceWater;
end;

{ TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink }

constructor TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4FractionOfPrecipToSurfaceWater;
  end;
end;

class function TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink.GetDescription: string;
begin
  result := 'Fraction_of_Excess_Precip_to_SW';
end;

function TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.FractionOfPrecipToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfPrecipToSurfaceWater.ArrayList = alArray)
  end;
end;

{ TFmp4MultFractionOfPrecipToSurfaceWaterCollection }

class function TFmp4MultFractionOfPrecipToSurfaceWaterCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultFractionOfPrecipToSurfaceWaterTimeListLink;
end;

function TFmp4MultFractionOfPrecipToSurfaceWaterCollection.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.FractionOfPrecipToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfPrecipToSurfaceWater.ArrayList = alArray)
  end;
end;

{ TFmp4MultFractionOfPrecipToSurfaceWaterBoundary }

class function TFmp4MultFractionOfPrecipToSurfaceWaterBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultFractionOfPrecipToSurfaceWaterCollection;
end;

procedure TFmp4MultFractionOfPrecipToSurfaceWaterBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultFractionOfPrecipToSurfaceWaterBoundary.InvalidateLandUseData(
  Sender: TObject);
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4FractionOfPrecipToSurfaceWater(self);
    end;
  end;
end;

function TFmp4MultFractionOfPrecipToSurfaceWaterBoundary.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
begin
  result := False;
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (LandUse.FractionOfPrecipToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfPrecipToSurfaceWater.ArrayList = alArray)
  end;
end;

class function TFmp4MultFractionOfPrecipToSurfaceWaterBoundary.ValueDescription: string;
begin
  result := 'Fraction_of_Excess_Precip_to_SW';
end;

end.
