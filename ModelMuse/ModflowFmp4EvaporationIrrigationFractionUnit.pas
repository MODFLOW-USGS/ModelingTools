unit ModflowFmp4EvaporationIrrigationFractionUnit;

interface

uses ModflowFmp4BoundaryUnit, ModflowBoundaryUnit,
  ModflowFmp4LandUseBoundaryUnit, GoPhastTypes;

type
  // Single land use per cell

  TFmp4EvaporationIrrigationFractionTimeListLink  = class(TFmp4TimeListLink)
  protected
    class function GetDescription: string; override;
    procedure AssignInvalidateEvent; override;
  end;

  TFmp4EvaporationIrrigationFractionCollection = class(TFmp4Collection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4EvaporationIrrigationFractionBoundary = class(TFmp4Boundary)
  private
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
    class function ValueDescription: string; override;
  end;

  // Multiple land use per cell

  TFmp4MultEvaporationIrrigationFractionTimeListLink = class(TFmp4LandUseTimeListLink)
  protected
    class function GetDescription: string; override;
    function MultipleCropsPerCellUsed: Boolean; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
  end;

  TFmp4MultEvaporationIrrigationFractionCollection = class(TFmp4LandUseCollection)
    function MultipleCropsPerCellUsed: Boolean; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFmp4MultEvaporationIrrigationFractionBoundary = class(TFmp4LandUseBoundary)
  protected
    function MultipleCropsPerCellUsed: Boolean; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure InvalidateLandUseData(Sender: TObject); override;
  public
    class function ValueDescription: string; override;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrFmp4EvaporationIrrigationFraction = 'Fmp4_Evaporation_Irrigation_Fraction';

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit;

{ TFmp4EvaporationIrrigationFractionTimeListLink }

procedure TFmp4EvaporationIrrigationFractionTimeListLink.AssignInvalidateEvent;
begin
//  inherited;

end;

class function TFmp4EvaporationIrrigationFractionTimeListLink.GetDescription: string;
begin
  result := StrFmp4EvaporationIrrigationFraction;
end;

{ TFmp4EvaporationIrrigationFractionCollection }

class function TFmp4EvaporationIrrigationFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4EvaporationIrrigationFractionTimeListLink;
end;

{ TFmp4EvaporationIrrigationFractionBoundary }

class function TFmp4EvaporationIrrigationFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4EvaporationIrrigationFractionCollection;
end;

procedure TFmp4EvaporationIrrigationFractionBoundary.InvalidateData(Sender: TObject);
begin
  InvalidateDisplay;
end;

procedure TFmp4EvaporationIrrigationFractionBoundary.InvalidateDisplay;
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
      Model.InvalidateMfFmp4EvaporationIrrigationFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

class function TFmp4EvaporationIrrigationFractionBoundary.ValueDescription: string;
begin
  result := StrFmp4EvaporationIrrigationFraction;
end;

{ TFmp4MultEvaporationIrrigationFractionTimeListLink }

constructor TFmp4MultEvaporationIrrigationFractionTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  if AModel <> nil then
  begin
    FOnInvalidateLanduse := (AModel as TCustomModel).InvalidateMfFmp4EvaporationIrrigationFraction;
  end;
end;

class function TFmp4MultEvaporationIrrigationFractionTimeListLink.GetDescription: string;
begin
  result := 'Evaporation_Irrigation_Fraction';
end;

function TFmp4MultEvaporationIrrigationFractionTimeListLink.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.EvapIrrigationFraction.FarmOption = foTransient)
      and (LandUse.EvapIrrigationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultEvaporationIrrigationFractionCollection }

class function TFmp4MultEvaporationIrrigationFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmp4MultEvaporationIrrigationFractionTimeListLink;
end;

function TFmp4MultEvaporationIrrigationFractionCollection.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.EvapIrrigationFraction.FarmOption = foTransient)
      and (LandUse.EvapIrrigationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

{ TFmp4MultEvaporationIrrigationFractionBoundary }

class function TFmp4MultEvaporationIrrigationFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmp4MultEvaporationIrrigationFractionCollection;
end;

procedure TFmp4MultEvaporationIrrigationFractionBoundary.InvalidateDisplay;
begin
  InvalidateLandUseData(nil);
end;

procedure TFmp4MultEvaporationIrrigationFractionBoundary.InvalidateLandUseData(
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
      Model.InvalidateMfFmp4EvaporationIrrigationFraction(self);
    end
  {$ENDIF}
    ;
  end;
end;

function TFmp4MultEvaporationIrrigationFractionBoundary.MultipleCropsPerCellUsed: Boolean;
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
      and (LandUse.EvapIrrigationFraction.FarmOption = foTransient)
      and (LandUse.EvapIrrigationFraction.ArrayList = alArray)
  end;
{$ENDIF}
end;

class function TFmp4MultEvaporationIrrigationFractionBoundary.ValueDescription: string;
begin
  result := 'Evaporation_Irrigation_Fraction';
end;

end.
