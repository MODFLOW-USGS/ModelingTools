unit ModflowFmp4CropHasSalinityDemandUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes, RbwParser;

type
  // Single land use per cell

  TFmp4CropHasSalinityDemandTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
    function GetDefaultDataType: TRbwDataType; override;
  end;

  TFmp4CropHasSalinityDemandCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4CropHasSalinityDemandBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultCropHasSalinityDemandTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
    function GetDefaultDataType: TRbwDataType; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultCropHasSalinityDemandCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultCropHasSalinityDemandBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4CropHasSalinityDemand = 'Fmp4_Crop_Has_Salinity_Demand';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4CropHasSalinityDemandTimeListLink }

procedure TFmp4CropHasSalinityDemandTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

function TFmp4CropHasSalinityDemandTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtBoolean;
end;

class function TFmp4CropHasSalinityDemandTimeListLink.GetDescription: string;
begin
  result := StrFmp4CropHasSalinityDemand;
end;

{ TFmp4CropHasSalinityDemandCollection }

class function TFmp4CropHasSalinityDemandCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4CropHasSalinityDemandTimeListLink;
end;

{ TFmp4CropHasSalinityDemandBoundary }

class function TFmp4CropHasSalinityDemandBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4CropHasSalinityDemandCollection;
end;

procedure TFmp4CropHasSalinityDemandBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4CropHasSalinityDemandBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4CropHasSalinityDemand(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4CropHasSalinityDemandBoundary.ValueDescription: string;
begin
  result := StrFmp4CropHasSalinityDemand;
end;

{ TFmp4MultCropHasSalinityDemandTimeListLink }

constructor TFmp4MultCropHasSalinityDemandTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4CropHasSalinityDemand;
  end;
end;

function TFmp4MultCropHasSalinityDemandTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtBoolean;
end;

class function TFmp4MultCropHasSalinityDemandTimeListLink.GetDescription: string;
begin
  result := 'Crop_Has_Salinity_Demand';
end;

function TFmp4MultCropHasSalinityDemandTimeListLink.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
  FarmSalinityFlush: TFarmProcess4SalinityFlush;
begin
  result := False;
{$IFDEF OWHMV2}
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    FarmSalinityFlush := LocalModel.ModflowPackages.FarmSalinityFlush;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and FarmSalinityFlush.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (FarmSalinityFlush.CropSalinityDemandChoice.FarmOption = foTransient)
      and (FarmSalinityFlush.CropSalinityDemandChoice.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultCropHasSalinityDemandCollection }

class function TFmp4MultCropHasSalinityDemandCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultCropHasSalinityDemandTimeListLink;
end;

function TFmp4MultCropHasSalinityDemandCollection.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
  FarmSalinityFlush: TFarmProcess4SalinityFlush;
begin
  result := False;
{$IFDEF OWHMV2}
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    FarmSalinityFlush := LocalModel.ModflowPackages.FarmSalinityFlush;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and FarmSalinityFlush.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (FarmSalinityFlush.CropSalinityDemandChoice.FarmOption = foTransient)
      and (FarmSalinityFlush.CropSalinityDemandChoice.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultCropHasSalinityDemandBoundary }

class function TFmp4MultCropHasSalinityDemandBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultCropHasSalinityDemandCollection;
end;

procedure TFmp4MultCropHasSalinityDemandBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultCropHasSalinityDemandBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4CropHasSalinityDemand(self);
    end
  {$ENDIF}
    ;
  end;
end;

function TFmp4MultCropHasSalinityDemandBoundary.MultipleCropsPerCellUsed: Boolean;
var
  LocalModel: TCustomModel;
  LandUse: TFarmProcess4LandUse;
  FarmSalinityFlush: TFarmProcess4SalinityFlush;
begin
  result := False;
{$IFDEF OWHMV2}
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    LandUse := LocalModel.ModflowPackages.FarmLandUse;
    FarmSalinityFlush := LocalModel.ModflowPackages.FarmSalinityFlush;
    result := (LocalModel.ModelSelection = msModflowOwhm2)
      and LandUse.IsSelected
      and FarmSalinityFlush.IsSelected
      and (LandUse.LandUseOption = luoMultiple)
      and (FarmSalinityFlush.CropSalinityDemandChoice.FarmOption = foTransient)
      and (FarmSalinityFlush.CropSalinityDemandChoice.ArrayList = alArray)
  end;
{$ENDIF}
end;

class function TFmp4MultCropHasSalinityDemandBoundary.ValueDescription: string;
begin
  result := 'Crop_Has_Salinity_Demand';
end;

end.
