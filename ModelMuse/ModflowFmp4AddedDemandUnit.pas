unit ModflowFmp4AddedDemandUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4AddedDemandTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4AddedDemandCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4AddedDemandBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultAddedDemandTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultAddedDemandCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultAddedDemandBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4AddedDemand = 'Fmp4_Added_Demand';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4AddedDemandTimeListLink }

procedure TFmp4AddedDemandTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4AddedDemandTimeListLink.GetDescription: string;
begin
  result := StrFmp4AddedDemand;
end;

{ TFmp4AddedDemandCollection }

class function TFmp4AddedDemandCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4AddedDemandTimeListLink;
end;

{ TFmp4AddedDemandBoundary }

class function TFmp4AddedDemandBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4AddedDemandCollection;
end;

procedure TFmp4AddedDemandBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4AddedDemandBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4AddedDemand(self);
    end;
  end;
end;

class function TFmp4AddedDemandBoundary.ValueDescription: string;
begin
  result := StrFmp4AddedDemand;
end;

{ TFmp4MultAddedDemandTimeListLink }

constructor TFmp4MultAddedDemandTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4AddedDemand;
  end;
end;

class function TFmp4MultAddedDemandTimeListLink.GetDescription: string;
begin
  result := 'Added_Demand';
end;

function TFmp4MultAddedDemandTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.AddedDemand.FarmOption = foTransient)
      and (LandUse.AddedDemand.ArrayList = alArray)
  end;
end;

{ TFmp4MultAddedDemandCollection }

class function TFmp4MultAddedDemandCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultAddedDemandTimeListLink;
end;

function TFmp4MultAddedDemandCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.AddedDemand.FarmOption = foTransient)
      and (LandUse.AddedDemand.ArrayList = alArray)
  end;
end;

{ TFmp4MultAddedDemandBoundary }

class function TFmp4MultAddedDemandBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultAddedDemandCollection;
end;

procedure TFmp4MultAddedDemandBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultAddedDemandBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4AddedDemand(self);
    end;
  end;
end;

function TFmp4MultAddedDemandBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.AddedDemand.FarmOption = foTransient)
      and (LandUse.AddedDemand.ArrayList = alArray)
  end;
end;

class function TFmp4MultAddedDemandBoundary.ValueDescription: string;
begin
  result := 'Added_Demand';
end;

end.
