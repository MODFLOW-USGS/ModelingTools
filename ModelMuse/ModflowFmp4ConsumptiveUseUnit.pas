unit ModflowFmp4ConsumptiveUseUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4ConsumptiveUseTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4ConsumptiveUseCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4ConsumptiveUseBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultConsumptiveUseTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultConsumptiveUseCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultConsumptiveUseBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;


resourcestring
  StrFmp4ConsumptiveUse = 'Fmp4_Consumptive_Use';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4ConsumptiveUseTimeListLink }

procedure TFmp4ConsumptiveUseTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4ConsumptiveUseTimeListLink.GetDescription: string;
begin
  result := StrFmp4ConsumptiveUse;
end;

{ TFmp4ConsumptiveUseCollection }

class function TFmp4ConsumptiveUseCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4ConsumptiveUseTimeListLink;
end;

{ TFmp4ConsumptiveUseBoundary }

class function TFmp4ConsumptiveUseBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4ConsumptiveUseCollection;
end;

procedure TFmp4ConsumptiveUseBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4ConsumptiveUseBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4ConsumptiveUse(self);
    end;
  end;
end;

class function TFmp4ConsumptiveUseBoundary.ValueDescription: string;
begin
  result := StrFmp4ConsumptiveUse;
end;

{ TFmp4MultConsumptiveUseTimeListLink }

constructor TFmp4MultConsumptiveUseTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4ConsumptiveUse;
  end;
end;

class function TFmp4MultConsumptiveUseTimeListLink.GetDescription: string;
begin
  result := 'Consumptive_Use';
end;

function TFmp4MultConsumptiveUseTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.ConsumptiveUse.FarmOption = foTransient)
      and (LandUse.ConsumptiveUse.ArrayList = alArray)
  end;
end;

{ TFmp4MultConsumptiveUseCollection }

class function TFmp4MultConsumptiveUseCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultConsumptiveUseTimeListLink;
end;

function TFmp4MultConsumptiveUseCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.ConsumptiveUse.FarmOption = foTransient)
      and (LandUse.ConsumptiveUse.ArrayList = alArray)
  end;
end;

{ TFmp4MultConsumptiveUseBoundary }

class function TFmp4MultConsumptiveUseBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultConsumptiveUseCollection;
end;

procedure TFmp4MultConsumptiveUseBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultConsumptiveUseBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4ConsumptiveUse(self);
    end;
  end;
end;

function TFmp4MultConsumptiveUseBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.ConsumptiveUse.FarmOption = foTransient)
      and (LandUse.ConsumptiveUse.ArrayList = alArray)
  end;
end;

class function TFmp4MultConsumptiveUseBoundary.ValueDescription: string;
begin
  result := 'Consumptive_Use';
end;

end.
