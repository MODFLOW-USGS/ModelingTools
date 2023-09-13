unit ModflowFmp4CropCoefficientUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

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

  // Multiple land use per cell

  TFmp4MultCropCoefficientTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultCropCoefficientCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultCropCoefficientBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;


resourcestring
  StrFmp4CropCoefficient = 'Fmp4_Crop_Coefficient';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

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
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4CropCoefficient(self);
    end;
  end;
end;

class function TFmp4CropCoefficientBoundary.ValueDescription: string;
begin
  result := StrFmp4CropCoefficient;
end;

{ TFmp4MultCropCoefficientTimeListLink }

constructor TFmp4MultCropCoefficientTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4CropCoefficient;
  end;
end;

class function TFmp4MultCropCoefficientTimeListLink.GetDescription: string;
begin
  result := 'Crop_Coefficients';
end;

function TFmp4MultCropCoefficientTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.CropCoeff.FarmOption = foTransient)
      and (LandUse.CropCoeff.ArrayList = alArray)
  end;
end;

{ TFmp4MultCropCoefficientCollection }

class function TFmp4MultCropCoefficientCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultCropCoefficientTimeListLink;
end;

function TFmp4MultCropCoefficientCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.CropCoeff.FarmOption = foTransient)
      and (LandUse.CropCoeff.ArrayList = alArray)
  end;
end;

{ TFmp4MultCropCoefficientBoundary }

class function TFmp4MultCropCoefficientBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultCropCoefficientCollection;
end;

procedure TFmp4MultCropCoefficientBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultCropCoefficientBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4CropCoefficient(self);
    end;
  end;
end;

function TFmp4MultCropCoefficientBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.CropCoeff.FarmOption = foTransient)
      and (LandUse.CropCoeff.ArrayList = alArray)
  end;
end;

class function TFmp4MultCropCoefficientBoundary.ValueDescription: string;
begin
  result := 'Crop_Coefficients';
end;

end.
