unit ModflowFmp4FractionOfIrrigToSurfaceWaterUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4FractionOfIrrigToSurfaceWaterTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4FractionOfIrrigToSurfaceWaterCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4FractionOfIrrigToSurfaceWaterBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultFractionOfIrrigToSurfaceWaterCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultFractionOfIrrigToSurfaceWaterBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4FractionOfIrrigToSurfaceWater = 'Fraction_of_Excess_Irrig_to_SW';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4FractionOfIrrigToSurfaceWaterTimeListLink }

procedure TFmp4FractionOfIrrigToSurfaceWaterTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4FractionOfIrrigToSurfaceWaterTimeListLink.GetDescription: string;
begin
  result := StrFmp4FractionOfIrrigToSurfaceWater;
end;

{ TFmp4FractionOfIrrigToSurfaceWaterCollection }

class function TFmp4FractionOfIrrigToSurfaceWaterCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4FractionOfIrrigToSurfaceWaterTimeListLink;
end;

{ TFmp4FractionOfIrrigToSurfaceWaterBoundary }

class function TFmp4FractionOfIrrigToSurfaceWaterBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4FractionOfIrrigToSurfaceWaterCollection;
end;

procedure TFmp4FractionOfIrrigToSurfaceWaterBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4FractionOfIrrigToSurfaceWaterBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4FractionOfIrrigToSurfaceWater(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4FractionOfIrrigToSurfaceWaterBoundary.ValueDescription: string;
begin
  result := StrFmp4FractionOfIrrigToSurfaceWater;
end;

{ TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink }

constructor TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4FractionOfIrrigToSurfaceWater;
  end;
end;

class function TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink.GetDescription: string;
begin
  result := 'Fraction_of_Excess_Irrig_to_SW';
end;

function TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.FractionOfIrrigationToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfIrrigationToSurfaceWater.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultFractionOfIrrigToSurfaceWaterCollection }

class function TFmp4MultFractionOfIrrigToSurfaceWaterCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultFractionOfIrrigToSurfaceWaterTimeListLink;
end;

function TFmp4MultFractionOfIrrigToSurfaceWaterCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.FractionOfIrrigationToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfIrrigationToSurfaceWater.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultFractionOfIrrigToSurfaceWaterBoundary }

class function TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultFractionOfIrrigToSurfaceWaterCollection;
end;

procedure TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4FractionOfIrrigToSurfaceWater(self);
    end
  {$ENDIF}
    ;
  end;
end;

function TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.FractionOfIrrigationToSurfaceWater.FarmOption = foTransient)
      and (LandUse.FractionOfIrrigationToSurfaceWater.ArrayList = alArray)
  end;
{$ENDIF}
end;

class function TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.ValueDescription: string;
begin
  result := 'Fraction_of_Excess_Irrig_to_SW';
end;

end.
