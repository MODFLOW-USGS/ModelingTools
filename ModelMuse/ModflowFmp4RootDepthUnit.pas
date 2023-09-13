unit ModflowFmp4RootDepthUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4RootDepthTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4RootDepthCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4RootDepthBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultRootDepthTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultRootDepthCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultRootDepthBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;


resourcestring
  StrFmp4RootDepth = 'Fmp4_Root_Depth';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4RootDepthTimeListLink }

procedure TFmp4RootDepthTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4RootDepthTimeListLink.GetDescription: string;
begin
  result := StrFmp4RootDepth;
end;

{ TFmp4RootDepthCollection }

class function TFmp4RootDepthCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4RootDepthTimeListLink;
end;

{ TFmp4RootDepthBoundary }

class function TFmp4RootDepthBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4RootDepthCollection;
end;

procedure TFmp4RootDepthBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4RootDepthBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4RootDepth(self);
    end;
  end;
end;

class function TFmp4RootDepthBoundary.ValueDescription: string;
begin
  result := StrFmp4RootDepth;
end;

{ TFmp4MultRootDepthTimeListLink }

constructor TFmp4MultRootDepthTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4RootDepth;
  end;
end;

class function TFmp4MultRootDepthTimeListLink.GetDescription: string;
begin
  result := 'Root_Depth';
end;

function TFmp4MultRootDepthTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.RootDepth.FarmOption = foTransient)
      and (LandUse.RootDepth.ArrayList = alArray)
  end;
end;

{ TFmp4MultRootDepthCollection }

class function TFmp4MultRootDepthCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultRootDepthTimeListLink;
end;

function TFmp4MultRootDepthCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.RootDepth.FarmOption = foTransient)
      and (LandUse.RootDepth.ArrayList = alArray)
  end;
end;

{ TFmp4MultRootDepthBoundary }

class function TFmp4MultRootDepthBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultRootDepthCollection;
end;

procedure TFmp4MultRootDepthBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultRootDepthBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4RootDepth(self);
    end;
  end;
end;

function TFmp4MultRootDepthBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.RootDepth.FarmOption = foTransient)
      and (LandUse.RootDepth.ArrayList = alArray)
  end;
end;

class function TFmp4MultRootDepthBoundary.ValueDescription: string;
begin
  result := 'Root_Depth';
end;

end.
