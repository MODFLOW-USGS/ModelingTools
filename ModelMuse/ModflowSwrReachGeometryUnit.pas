unit ModflowSwrReachGeometryUnit;

interface

uses
  Classes, GoPhastTypes, OrderedCollectionUnit, SysUtils,
  OrderedCollectionInterfaceUnit;

type
  TGeometryType = (gtRectangular, gtTrapezoidal, gtIrregular, gtTable,
    gtWholeCell);
  TConductanceMethod = (cmFixed, cmReachLeakance, cmK, cmLeakanceAndK);

  TReachCrossSectionItem = class(TOrderedItem)
  private
    FStoredElevation: TRealStorage;
    FStoredX: TRealStorage;
    function GetElev: Double;
    function GetX: Double;
    procedure SetElev(const Value: Double);
    procedure SetStoredElevation(const Value: TRealStorage);
    procedure SetStoredX(const Value: TRealStorage);
    procedure SetX(const Value: Double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // XB
    property X: Double read GetX write SetX;
    // ELEVB
    property Elev: Double read GetElev write SetElev;
  published
    property StoredX: TRealStorage read FStoredX write SetStoredX;
    property StoredElevation: TRealStorage read FStoredElevation
      write SetStoredElevation;
  end;

  TReachCrossSection = class(TOrderedCollection)
  private
    function GetItems(Index: Integer): TReachCrossSectionItem;
    procedure SetItems(Index: Integer; const Value: TReachCrossSectionItem);
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TReachCrossSectionItem read GetItems
      write SetItems; default;
    function Add: TReachCrossSectionItem;
  end;

  TReachTableItem = class(TOrderedItem)
  private
    FStoredWettedPerimeter: TRealStorage;
    FStoredVolume: TRealStorage;
    FStoredCrossSectionArea: TRealStorage;
    FStoredElevation: TRealStorage;
    FStoredSurfaceArea: TRealStorage;
    function GetCrossSectionArea: double;
    function GetElevation: double;
    function GetSurfaceArea: double;
    function GetVolume: double;
    function GetWettedPerimeter: double;
    procedure SetCrossSectionArea(const Value: double);
    procedure SetElevation(const Value: double);
    procedure SetStoredCrossSectionArea(const Value: TRealStorage);
    procedure SetStoredElevation(const Value: TRealStorage);
    procedure SetStoredSurfaceArea(const Value: TRealStorage);
    procedure SetStoredVolume(const Value: TRealStorage);
    procedure SetStoredWettedPerimeter(const Value: TRealStorage);
    procedure SetSurfaceArea(const Value: double);
    procedure SetVolume(const Value: double);
    procedure SetWettedPerimeter(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // ELEV
    property Elevation: double read GetElevation write SetElevation;
    // VOL
    property Volume: double read GetVolume write SetVolume;
    // WETPER
    property WettedPerimeter: double read GetWettedPerimeter
      write SetWettedPerimeter;
    // SAREA
    property SurfaceArea: double read GetSurfaceArea write SetSurfaceArea;
    // XAREA
    property CrossSectionArea: double read GetCrossSectionArea
      write SetCrossSectionArea;
  published
    property StoredElevation: TRealStorage read FStoredElevation
      write SetStoredElevation;
    property StoredVolume: TRealStorage read FStoredVolume
      write SetStoredVolume;
    property StoredWettedPerimeter: TRealStorage read FStoredWettedPerimeter
      write SetStoredWettedPerimeter;
    property StoredSurfaceArea: TRealStorage read FStoredSurfaceArea
      write SetStoredSurfaceArea;
    property StoredCrossSectionArea: TRealStorage read FStoredCrossSectionArea
      write SetStoredCrossSectionArea;
  end;

  TReachTable = class(TOrderedCollection)
  private
    function GetItems(Index: Integer): TReachTableItem;
    procedure SetItems(Index: Integer; const Value: TReachTableItem);
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TReachTableItem read GetItems
      write SetItems; default;
    function Add: TReachTableItem;
  end;

  TReachGeometryItem = class(TOrderedItem)
  private
    FTable: TReachTable;
    FName: string;
    FConductanceMethod: TConductanceMethod;
    FStoredBottomElevation: TRealStorage;
    FCrossSection: TReachCrossSection;
    FStoredSideSlope: TRealStorage;
    FStoredLeakance: TRealStorage;
    FStoredExtinctionDepth: TRealStorage;
    FStoredCenterDistance: TRealStorage;
    FStoredConductance: TRealStorage;
    FGeometryType: TGeometryType;
    FStoredRoughness: TRealStorage;
    FStoredWidth: TRealStorage;
    FGeoNumber: integer;
    FminCalc: Boolean;
    FMinElev: Double;
    function GetBottomElevation: Double;
    function GetCenterDistance: Double;
    function GetConductance: Double;
    function GetExtinctionDepth: Double;
    function GetLeakance: Double;
    function GetRoughness: Double;
    function GetSideSlope: Double;
    function GetWidth: Double;
    procedure SetBottomElevation(const Value: Double);
    procedure SetCenterDistance(const Value: Double);
    procedure SetConductance(const Value: Double);
    procedure SetConductanceMethod(const Value: TConductanceMethod);
    procedure SetCrossSection(const Value: TReachCrossSection);
    procedure SetExtinctionDepth(const Value: Double);
    procedure SetGeometryType(const Value: TGeometryType);
    procedure SetLeakance(const Value: Double);
    procedure SetName(const Value: string);
    procedure SetRoughness(const Value: Double);
    procedure SetSideSlope(const Value: Double);
    procedure SetStoredBottomElevation(const Value: TRealStorage);
    procedure SetStoredCenterDistance(const Value: TRealStorage);
    procedure SetStoredConductance(const Value: TRealStorage);
    procedure SetStoredExtinctionDepth(const Value: TRealStorage);
    procedure SetStoredLeakance(const Value: TRealStorage);
    procedure SetStoredRoughness(const Value: TRealStorage);
    procedure SetStoredSideSlope(const Value: TRealStorage);
    procedure SetStoredWidth(const Value: TRealStorage);
    procedure SetTable(const Value: TReachTable);
    procedure SetWidth(const Value: Double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    function HasMinimum: Boolean;
    function MinimumElevation: double;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // GMANNING
    property Roughness: Double read GetRoughness write SetRoughness;
    // GWIDTH
    property Width: Double  read GetWidth write SetWidth;
    // GBELEV
    property BottomElevation: Double  read GetBottomElevation
      write SetBottomElevation;
    // GSSLOPE
    property SideSlope: Double  read GetSideSlope write SetSideSlope;
    // GCND
    property Conductance: Double  read GetConductance write SetConductance;
    // GLK
    property Leakance: Double  read GetLeakance write SetLeakance;
    // GCNDLN
    property CenterDistance: Double  read GetCenterDistance
      write SetCenterDistance;
    // GETEXTD
    property ExtinctionDepth: Double  read GetExtinctionDepth
      write SetExtinctionDepth;
    // IGEONUM
    property GeoNumber: integer read FGeoNumber write FGeoNumber;
  published
    // IGEONUM
    property Name: string read FName write SetName;
    // IGEOTYPE
    property GeometryType: TGeometryType read FGeometryType
      write SetGeometryType stored True;
    // IGCNDOP
    property ConductanceMethod: TConductanceMethod read FConductanceMethod
      write SetConductanceMethod stored True;
    // GMANNING
    property StoredRoughness: TRealStorage read FStoredRoughness
      write SetStoredRoughness;
    // GWIDTH
    property StoredWidth: TRealStorage read FStoredWidth write SetStoredWidth;
    // GBELEV
    property StoredBottomElevation: TRealStorage read FStoredBottomElevation
      write SetStoredBottomElevation;
    // GSSLOPE
    property StoredSideSlope: TRealStorage read FStoredSideSlope
      write SetStoredSideSlope;
    // GCND
    property StoredConductance: TRealStorage read FStoredConductance
      write SetStoredConductance;
    // GLK
    property StoredLeakance: TRealStorage read FStoredLeakance
      write SetStoredLeakance;
    // GCNDLN
    property StoredCenterDistance: TRealStorage read FStoredCenterDistance
      write SetStoredCenterDistance;
    // GETEXTD
    property StoredExtinctionDepth: TRealStorage read FStoredExtinctionDepth
      write SetStoredExtinctionDepth;
    // Data set 11b
    property CrossSection: TReachCrossSection read FCrossSection
      write SetCrossSection;
    // Data set 11c
    property Table: TReachTable read FTable write SetTable;
  end;

  TReachGeometryCollection = class(TOrderedCollection)
  private
    function GetItems(Index: Integer): TReachGeometryItem;
    procedure SetItems(Index: Integer; const Value: TReachGeometryItem);
  protected
    function SortItems: Boolean; override;
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TReachGeometryItem read GetItems
      write SetItems; default;
    function GetItemByName(AName: string): TReachGeometryItem;
    function Add: TReachGeometryItem;
  end;

implementation

uses
  PhastModelUnit, ScreenObjectUnit, ModflowSwrReachUnit;



{ TReachCrossSectionItem }

procedure TReachCrossSectionItem.Assign(Source: TPersistent);
var
  SourceItem: TReachCrossSectionItem;
begin
  if Source is TReachCrossSectionItem then
  begin
    SourceItem := TReachCrossSectionItem(Source);
    X := SourceItem.X;
    Elev := SourceItem.Elev;
  end
  else
  begin
    inherited;
  end;
end;

constructor TReachCrossSectionItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredElevation := TRealStorage.Create;
  FStoredX := TRealStorage.Create;
  FStoredElevation.OnChange := OnInvalidateModelEvent;
  FStoredX.OnChange := OnInvalidateModelEvent;
end;

destructor TReachCrossSectionItem.Destroy;
begin
  FStoredX.Free;
  FStoredElevation.Free;
  inherited;
end;

function TReachCrossSectionItem.GetElev: Double;
begin
  result := FStoredElevation.Value;
end;

function TReachCrossSectionItem.GetX: Double;
begin
  result := FStoredX.Value;
end;

function TReachCrossSectionItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  CrossSectItem: TReachCrossSectionItem;
begin
  result := (AnotherItem is TReachCrossSectionItem);
  if result then
  begin
    CrossSectItem := TReachCrossSectionItem(AnotherItem);
    result := (X = CrossSectItem.X)
      and (Elev = CrossSectItem.Elev);
  end;
end;

procedure TReachCrossSectionItem.SetElev(const Value: Double);
begin
  FStoredElevation.Value := Value;
end;

procedure TReachCrossSectionItem.SetStoredElevation(const Value: TRealStorage);
begin
  FStoredElevation.Assign(Value);
end;

procedure TReachCrossSectionItem.SetStoredX(const Value: TRealStorage);
begin
  FStoredX.Assign(Value);
end;

procedure TReachCrossSectionItem.SetX(const Value: Double);
begin
  FStoredX.Value := Value;
end;

{ TReachCrossSection }

function TReachCrossSection.Add: TReachCrossSectionItem;
begin
  Result := inherited Add as TReachCrossSectionItem;
end;

constructor TReachCrossSection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TReachCrossSectionItem, Model);
end;

function TReachCrossSection.GetItems(Index: Integer): TReachCrossSectionItem;
begin
  result := inherited Items[Index] as TReachCrossSectionItem;
end;

procedure TReachCrossSection.SetItems(Index: Integer;
  const Value: TReachCrossSectionItem);
begin
  inherited Items[Index]:= Value;
end;

{ TReachTableItem }

procedure TReachTableItem.Assign(Source: TPersistent);
var
  SourceItem: TReachTableItem;
begin
  if Source is TReachTableItem then
  begin
    SourceItem := TReachTableItem(Source);
    Elevation := SourceItem.Elevation;
    Volume := SourceItem.Volume;
    WettedPerimeter := SourceItem.WettedPerimeter;
    SurfaceArea := SourceItem.SurfaceArea;
    CrossSectionArea := SourceItem.CrossSectionArea;
  end
  else
  begin
    inherited;
  end;
end;

constructor TReachTableItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredWettedPerimeter := TRealStorage.Create;
  FStoredVolume := TRealStorage.Create;
  FStoredCrossSectionArea := TRealStorage.Create;
  FStoredElevation := TRealStorage.Create;
  FStoredSurfaceArea := TRealStorage.Create;

  FStoredWettedPerimeter.OnChange := OnInvalidateModelEvent;
  FStoredVolume.OnChange := OnInvalidateModelEvent;
  FStoredCrossSectionArea.OnChange := OnInvalidateModelEvent;
  FStoredElevation.OnChange := OnInvalidateModelEvent;
  FStoredSurfaceArea.OnChange := OnInvalidateModelEvent;
end;

destructor TReachTableItem.Destroy;
begin
  FStoredWettedPerimeter.Free;
  FStoredVolume.Free;
  FStoredCrossSectionArea.Free;
  FStoredElevation.Free;
  FStoredSurfaceArea.Free;
  inherited;
end;

function TReachTableItem.GetCrossSectionArea: double;
begin
  result := FStoredCrossSectionArea.Value;
end;

function TReachTableItem.GetElevation: double;
begin
  result := FStoredElevation.Value;
end;

function TReachTableItem.GetSurfaceArea: double;
begin
  result := FStoredSurfaceArea.Value;
end;

function TReachTableItem.GetVolume: double;
begin
  result := FStoredVolume.Value;
end;

function TReachTableItem.GetWettedPerimeter: double;
begin
  result := FStoredWettedPerimeter.Value;
end;

function TReachTableItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  ReachTable: TReachTableItem;
begin
  result := (AnotherItem is TReachTableItem);
  if result then
  begin
    ReachTable := TReachTableItem(AnotherItem);
    result := (Elevation = ReachTable.Elevation)
      and (Volume = ReachTable.Volume)
      and (WettedPerimeter = ReachTable.WettedPerimeter)
      and (SurfaceArea = ReachTable.SurfaceArea)
      and (CrossSectionArea = ReachTable.CrossSectionArea);
  end;
end;

procedure TReachTableItem.SetCrossSectionArea(const Value: double);
begin
  FStoredCrossSectionArea.Value := Value;
end;

procedure TReachTableItem.SetElevation(const Value: double);
begin
  FStoredElevation.Value := Value;
end;

procedure TReachTableItem.SetStoredCrossSectionArea(const Value: TRealStorage);
begin
  FStoredCrossSectionArea.Assign(Value);
end;

procedure TReachTableItem.SetStoredElevation(const Value: TRealStorage);
begin
  FStoredElevation.Assign(Value);
end;

procedure TReachTableItem.SetStoredSurfaceArea(const Value: TRealStorage);
begin
  FStoredSurfaceArea.Assign(Value);
end;

procedure TReachTableItem.SetStoredVolume(const Value: TRealStorage);
begin
  FStoredVolume.Assign(Value);
end;

procedure TReachTableItem.SetStoredWettedPerimeter(const Value: TRealStorage);
begin
  FStoredWettedPerimeter.Assign(Value);
end;

procedure TReachTableItem.SetSurfaceArea(const Value: double);
begin
  FStoredSurfaceArea.Value := Value;
end;

procedure TReachTableItem.SetVolume(const Value: double);
begin
  FStoredVolume.Value := Value;
end;

procedure TReachTableItem.SetWettedPerimeter(const Value: double);
begin
  FStoredWettedPerimeter.Value := Value;
end;

{ TReachTable }

function TReachTable.Add: TReachTableItem;
begin
  Result := inherited Add as TReachTableItem;
end;

constructor TReachTable.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TReachTableItem, Model);
end;

function TReachTable.GetItems(Index: Integer): TReachTableItem;
begin
  result := inherited Items[Index] as TReachTableItem;
end;

procedure TReachTable.SetItems(Index: Integer; const Value: TReachTableItem);
begin
  inherited Items[Index] := Value;
end;

{ TReachGeometryItem }

procedure TReachGeometryItem.Assign(Source: TPersistent);
var
  SourceItem: TReachGeometryItem;
begin
  if Source is TReachGeometryItem then
  begin
    SourceItem := TReachGeometryItem(Source);
    Name := SourceItem.Name;
    GeometryType := SourceItem.GeometryType;
    Roughness := SourceItem.Roughness;
    ConductanceMethod := SourceItem.ConductanceMethod;
    Width := SourceItem.Width;
    BottomElevation := SourceItem.BottomElevation;
    SideSlope := SourceItem.SideSlope;
    Conductance := SourceItem.Conductance;
    Leakance := SourceItem.Leakance;
    CenterDistance := SourceItem.CenterDistance;
    ExtinctionDepth := SourceItem.ExtinctionDepth;
    CrossSection := SourceItem.CrossSection;
    Table := SourceItem.Table;
    FminCalc := False;
  end;
  inherited;
end;

constructor TReachGeometryItem.Create(Collection: TCollection);
begin
  inherited;
  FTable := TReachTable.Create(Model as TCustomModel);
  FCrossSection := TReachCrossSection.Create(Model as TCustomModel);
  FStoredBottomElevation := TRealStorage.Create;
  FStoredSideSlope := TRealStorage.Create;
  FStoredLeakance := TRealStorage.Create;
  FStoredExtinctionDepth := TRealStorage.Create;
  FStoredCenterDistance := TRealStorage.Create;
  FStoredConductance := TRealStorage.Create;
  FStoredRoughness := TRealStorage.Create;
  FStoredWidth := TRealStorage.Create;

  FStoredBottomElevation.OnChange := OnInvalidateModelEvent;
  FStoredSideSlope.OnChange := OnInvalidateModelEvent;
  FStoredLeakance.OnChange := OnInvalidateModelEvent;
  FStoredExtinctionDepth.OnChange := OnInvalidateModelEvent;
  FStoredCenterDistance.OnChange := OnInvalidateModelEvent;
  FStoredConductance.OnChange := OnInvalidateModelEvent;
  FStoredRoughness.OnChange := OnInvalidateModelEvent;
  FStoredWidth.OnChange := OnInvalidateModelEvent;

end;

destructor TReachGeometryItem.Destroy;
var
  LocalModel: TCustomModel;
  Index: Integer;
  AScreenObject: TScreenObject;
  Reaches: TSwrReachBoundary;
begin
  FTable.Free;
  FCrossSection.Free;
  FStoredBottomElevation.Free;
  FStoredSideSlope.Free;
  FStoredLeakance.Free;
  FStoredExtinctionDepth.Free;
  FStoredCenterDistance.Free;
  FStoredConductance.Free;
  FStoredRoughness.Free;
  FStoredWidth.Free;
  LocalModel := Model as TCustomModel;
  if (LocalModel <> nil) and not (csDestroying in LocalModel.ComponentState)
    and not LocalModel.Clearing then
  begin
    for Index := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[Index];
      Reaches := AScreenObject.ModflowSwrReaches;
      if Reaches <> nil then
      begin
        Reaches.RemoveGeom(self);
      end;
    end;
  end;
  inherited;
end;

function TReachGeometryItem.GetBottomElevation: Double;
begin
  Result := FStoredBottomElevation.Value
end;

function TReachGeometryItem.GetCenterDistance: Double;
begin
  Result := FStoredCenterDistance.Value
end;

function TReachGeometryItem.GetConductance: Double;
begin
  Result := FStoredConductance.Value
end;

function TReachGeometryItem.GetExtinctionDepth: Double;
begin
  Result := FStoredExtinctionDepth.Value
end;

function TReachGeometryItem.GetLeakance: Double;
begin
  Result := FStoredLeakance.Value
end;

function TReachGeometryItem.GetRoughness: Double;
begin
  Result := FStoredRoughness.Value
end;

function TReachGeometryItem.GetSideSlope: Double;
begin
  Result := FStoredSideSlope.Value
end;

function TReachGeometryItem.GetWidth: Double;
begin
  Result := FStoredWidth.Value
end;

function TReachGeometryItem.HasMinimum: Boolean;
begin
  result := GeometryType in [gtRectangular, gtTrapezoidal, gtIrregular, gtTable]
end;

function TReachGeometryItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  GeomItem: TReachGeometryItem;
begin
  result := (AnotherItem is TReachGeometryItem);
  if result then
  begin
    GeomItem := TReachGeometryItem(AnotherItem);
    result := (Name = GeomItem.Name)
      and (GeometryType = GeomItem.GeometryType)
      and (ConductanceMethod = GeomItem.ConductanceMethod)
      and (Roughness = GeomItem.Roughness)
      and (Width = GeomItem.Width)
      and (BottomElevation = GeomItem.BottomElevation)
      and (SideSlope = GeomItem.SideSlope)
      and (Conductance = GeomItem.Conductance)
      and (Leakance = GeomItem.Leakance)
      and (CenterDistance = GeomItem.CenterDistance)
      and (ExtinctionDepth = GeomItem.ExtinctionDepth)
      and CrossSection.IsSame(GeomItem.CrossSection)
      and Table.IsSame(GeomItem.Table);
  end;
end;

function TReachGeometryItem.MinimumElevation: double;
var
  CrossSectionIndex: Integer;
begin
  if not FminCalc then
  begin
    case GeometryType of
      gtRectangular, gtTrapezoidal:
        begin
          FMinElev := BottomElevation;
        end;
      gtIrregular:
        begin
          FMinElev := CrossSection[0].Elev;
          for CrossSectionIndex := 1 to CrossSection.Count - 1 do
          begin
            if CrossSection[CrossSectionIndex].Elev < FMinElev then
            begin
              FMinElev := CrossSection[CrossSectionIndex].Elev;
            end;
          end;
        end;
      gtTable:
        begin
          FMinElev := Table[0].Elevation;
        end;
      gtWholeCell:
        begin
          Assert(False);
        end;
    end;
    FminCalc := True;
  end;
  result := FMinElev;
end;

procedure TReachGeometryItem.SetBottomElevation(const Value: Double);
begin
  FStoredBottomElevation.Value := Value;
end;

procedure TReachGeometryItem.SetCenterDistance(const Value: Double);
begin
  FStoredCenterDistance.Value := Value;
end;

procedure TReachGeometryItem.SetConductance(const Value: Double);
begin
  FStoredConductance.Value := Value;
end;

procedure TReachGeometryItem.SetConductanceMethod(
  const Value: TConductanceMethod);
begin
  if FConductanceMethod <> Value then
  begin
    FConductanceMethod := Value;
    InvalidateModel;
  end;
end;

procedure TReachGeometryItem.SetCrossSection(const Value: TReachCrossSection);
begin
  FCrossSection.Assign(Value);
end;

procedure TReachGeometryItem.SetExtinctionDepth(const Value: Double);
begin
  FStoredExtinctionDepth.Value := Value;
end;

procedure TReachGeometryItem.SetGeometryType(const Value: TGeometryType);
begin
  if FGeometryType <> Value then
  begin
    FGeometryType := Value;
    InvalidateModel;
  end;
end;

procedure TReachGeometryItem.SetLeakance(const Value: Double);
begin
  FStoredLeakance.Value := Value;
end;

procedure TReachGeometryItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TReachGeometryItem.SetRoughness(const Value: Double);
begin
  FStoredRoughness.Value := Value;
end;

procedure TReachGeometryItem.SetSideSlope(const Value: Double);
begin
  FStoredSideSlope.Value := Value;
end;

procedure TReachGeometryItem.SetStoredBottomElevation(
  const Value: TRealStorage);
begin
  FStoredBottomElevation.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredCenterDistance(const Value: TRealStorage);
begin
  FStoredCenterDistance.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredConductance(const Value: TRealStorage);
begin
  FStoredConductance.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredExtinctionDepth(
  const Value: TRealStorage);
begin
  FStoredExtinctionDepth.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredLeakance(const Value: TRealStorage);
begin
  FStoredLeakance.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredRoughness(const Value: TRealStorage);
begin
  FStoredRoughness.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredSideSlope(const Value: TRealStorage);
begin
  FStoredSideSlope.Assign(Value);
end;

procedure TReachGeometryItem.SetStoredWidth(const Value: TRealStorage);
begin
  FStoredWidth.Assign(Value);
end;

procedure TReachGeometryItem.SetTable(const Value: TReachTable);
begin
  FTable.Assign(Value);
end;

procedure TReachGeometryItem.SetWidth(const Value: Double);
begin
  FStoredWidth.Value := Value;
end;

{ TReachGeometryCollection }

function TReachGeometryCollection.Add: TReachGeometryItem;
begin
  result := inherited Add as TReachGeometryItem;
end;

constructor TReachGeometryCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TReachGeometryItem, Model);
end;

function TReachGeometryCollection.GetItemByName(
  AName: string): TReachGeometryItem;
var
  AnItem: TReachGeometryItem;
  index: Integer;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    AnItem := Items[index];
    if AnsiCompareText(AName, AnItem.Name) = 0 then
    begin
      result := AnItem;
      Break;
    end;
  end;
end;

function TReachGeometryCollection.GetItems(Index: Integer): TReachGeometryItem;
begin
  result := inherited Items[Index] as TReachGeometryItem;
end;

procedure TReachGeometryCollection.SetItems(Index: Integer;
  const Value: TReachGeometryItem);
begin
  inherited Items[Index] := Value;
end;

function TReachGeometryCollection.SortItems: Boolean;
begin
  result := True;
end;

end.
