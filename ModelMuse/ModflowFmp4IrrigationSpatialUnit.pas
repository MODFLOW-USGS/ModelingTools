unit ModflowFmp4IrrigationSpatialUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes, RbwParser;

type
  // Single land use per cell

  TFmp4IrrigationTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
    function GetDefaultDataType: TRbwDataType; override;
  end;

  TFmp4IrrigationCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4IrrigationBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultIrrigationTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
    function GetDefaultDataType: TRbwDataType; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultIrrigationCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultIrrigationBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;


resourcestring
  StrFmp4Irrigation = 'Fmp4_Irrigation';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4IrrigationTimeListLink }

procedure TFmp4IrrigationTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

function TFmp4IrrigationTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtInteger;
end;

class function TFmp4IrrigationTimeListLink.GetDescription: string;
begin
  result := StrFmp4Irrigation;
end;

{ TFmp4IrrigationCollection }

class function TFmp4IrrigationCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4IrrigationTimeListLink;
end;

{ TFmp4IrrigationBoundary }

class function TFmp4IrrigationBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4IrrigationCollection;
end;

procedure TFmp4IrrigationBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4IrrigationBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4Irrigation(self);
    end;
  end;
end;

class function TFmp4IrrigationBoundary.ValueDescription: string;
begin
  result := StrFmp4Irrigation;
end;

{ TFmp4MultIrrigationTimeListLink }

constructor TFmp4MultIrrigationTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4Irrigation;
  end;
end;

function TFmp4MultIrrigationTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtInteger;
end;

class function TFmp4MultIrrigationTimeListLink.GetDescription: string;
begin
  result := 'Irrigation';
end;

function TFmp4MultIrrigationTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.Irrigation.FarmOption = foTransient)
      and (LandUse.Irrigation.ArrayList = alArray)
      and (LocalModel.IrrigationTypes.Count > 0)
  end;
end;

{ TFmp4MultIrrigationCollection }

class function TFmp4MultIrrigationCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultIrrigationTimeListLink;
end;

function TFmp4MultIrrigationCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.Irrigation.FarmOption = foTransient)
      and (LandUse.Irrigation.ArrayList = alArray)
      and (LocalModel.IrrigationTypes.Count > 0)
  end;
end;

{ TFmp4MultIrrigationBoundary }

class function TFmp4MultIrrigationBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultIrrigationCollection;
end;

procedure TFmp4MultIrrigationBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultIrrigationBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4Irrigation(self);
    end;
  end;
end;

function TFmp4MultIrrigationBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.Irrigation.FarmOption = foTransient)
      and (LandUse.Irrigation.ArrayList = alArray)
      and (LocalModel.IrrigationTypes.Count > 0)
  end;
end;

class function TFmp4MultIrrigationBoundary.ValueDescription: string;
begin
  result := 'Irrigation';
end;

end.
