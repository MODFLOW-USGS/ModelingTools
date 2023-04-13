unit ModpathParticleUnit;

interface

uses Classes, SysUtils, GLScene, GLObjects, GoPhastTypes, GLColor,
  GLCoordinates, GLVectorTypes;

type
  // @name is a collection of @link(TParticleLocation)s.
  TParticles = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FMaxParticleSize: double;
  public
    Constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    procedure UpdateMaxParticleSize(ParticleSize: Double);
    procedure ClearSpheres;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TParticleLocation = class(TCollectionItem)
  private
    FSphere: TGLSphere;
    FZ: double;
    FX: double;
    FY: double;
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
    procedure SetZ(const Value: double);
    procedure SetSphereLocation;
    procedure InvalidateModel;
  public
    Destructor Destroy; override;
    procedure CreateSphere(aParentOwner : TGLBaseSceneObject);
    procedure Assign(Source: TPersistent); override;
    property Sphere: TGLSphere read FSphere;
  published
    property X: double read FX write SetX;
    property Y: double read FY write SetY;
    property Z: double read FZ write SetZ;
  end;

  TParticleDistribution = (pdGrid, pdCylinder, pdSphere, pdIndividual, pdObjectLocation);
  TParticleGroupOrientation = (pgoVertical, pgoEastWest, pgoNorthSouth);

  TGridDistribution = class(TGoPhastPersistent)
  private
    FBackFace: boolean;
    FTopFace: boolean;
    FLeftFace: boolean;
    FRightFace: boolean;
    FFrontFace: boolean;
    FZCount: integer;
    FInternal: boolean;
    FBottomFace: boolean;
    FXCount: integer;
    FYCount: integer;
    FParticles: TParticles;
    FMaxParticleSize: double;
    procedure SetBackFace(const Value: boolean);
    procedure SetBottomFace(const Value: boolean);
    procedure SetFrontFace(const Value: boolean);
    procedure SetInternal(const Value: boolean);
    procedure SetLeftFace(const Value: boolean);
    procedure SetRightFace(const Value: boolean);
    procedure SetTopFace(const Value: boolean);
    procedure SetXCount(const Value: integer);
    procedure SetYCount(const Value: integer);
    procedure SetZCount(const Value: integer);
    function GetParticles: TParticles;
    procedure CreateParticlesOnXFace(X: Double);
    procedure CreateParticlesOnYFace(Y: Double);
    procedure CreateParticlesOnZFace(Z: Double);
    procedure UpdateMaxParticleSize(ParticleSize: Double);
  protected
    procedure InvalidateModel; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Particles: TParticles read GetParticles;
    Destructor Destroy; override;
  published
    property LeftFace: boolean read FLeftFace write SetLeftFace;
    property RightFace: boolean read FRightFace write SetRightFace;
    property BackFace: boolean read FBackFace write SetBackFace;
    property FrontFace: boolean read FFrontFace write SetFrontFace;
    property BottomFace: boolean read FBottomFace write SetBottomFace;
    property TopFace: boolean read FTopFace write SetTopFace;
    property Internal: boolean read FInternal write SetInternal default True;
    property XCount: integer read FXCount write SetXCount default 1;
    property YCount: integer read FYCount write SetYCount default 1;
    property ZCount: integer read FZCount write SetZCount default 1;
  end;

  TCylSphereDistribution = class(TGoPhastPersistent)
  private
    FRadius: double;
    FLayerCount: integer;
    FCircleParticleCount: integer;
    FOrientation: TParticleGroupOrientation;
    FParticles: TParticles;
    FDistributionChoice: TParticleDistribution;
    FMaxParticleSize: double;
    procedure SetCircleParticleCount(const Value: integer);
    procedure SetLayerCount(const Value: integer);
    procedure SetOrientation(const Value: TParticleGroupOrientation);
    procedure SetRadius(const Value: double);
    procedure SetDistributionChoice(const Value: TParticleDistribution);
    function GetParticles: TParticles;
    procedure CreateCylinderParticles;
    procedure UpdateMaxParticleSize(ParticleSize: Double);
    procedure CreateVerticalCylinderParticles;
    procedure CreateEastWestCylinderParticles;
    procedure CreateNorthSouthCylinderParticles;
    procedure CreateSphereParticles;
    procedure CreateVerticalSphereParticles;
    procedure CreateEastWestSphereParticles;
    procedure CreateNorthSouthSphereParticles;
  protected
    procedure InvalidateModel; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Particles: TParticles read GetParticles;
    Destructor Destroy; override;
  published
    property DistributionChoice: TParticleDistribution read FDistributionChoice
      write SetDistributionChoice;
    property Orientation: TParticleGroupOrientation read FOrientation
      write SetOrientation;
    property CircleParticleCount: integer read FCircleParticleCount
      write SetCircleParticleCount default 8;
    property LayerCount: integer read FLayerCount write SetLayerCount
      default 1;
    property Radius: double read FRadius write SetRadius stored True;
  end;

  // @name is used in both @link(TParticleStorage) and
  // @link(TModpathSelection).
  TModpathTimeItem = class(TCollectionItem)
  private
    FTime: double;
    procedure InvalidateModel;
    procedure SetTime(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Time: double read FTime write SetTime;
  end;

  // @name is used in both @link(TParticleStorage) and
  // @link(TModpathSelection).
  TModpathTimes = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetItem(Index: Integer): TModpathTimeItem;
    procedure SetItem(Index: Integer; const Value: TModpathTimeItem);
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TModpathTimeItem read GetItem write SetItem; default;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TParticleStorage = class(TGoPhastPersistent)
  private
    FParticleDistribution: TParticleDistribution;
    FCylinderParticles: TCylSphereDistribution;
    FCustomParticles: TParticles;
    FSphereParticles: TCylSphereDistribution;
    FGridParticles: TGridDistribution;
    FUsed: boolean;
    FReleaseTimes: TModpathTimes;
    procedure SetParticleDistribution(const Value: TParticleDistribution);
    procedure SetCustomParticles(const Value: TParticles);
    procedure SetCylinderParticles(const Value: TCylSphereDistribution);
    procedure SetGridParticles(const Value: TGridDistribution);
    procedure SetSphereParticles(const Value: TCylSphereDistribution);
    function GetParticles: TParticles;
    function GetMaxSphereSize: double;
    function StoreGridParticles: boolean;
    function StoreCylinderParticles: boolean;
    function StoreSphereParticles: boolean;
    function StoreCustomParticles: boolean;
    procedure SetUsed(const Value: boolean);
    procedure SetReleaseTimes(const Value: TModpathTimes);
  public
    procedure ClearSpheres;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent }
    //
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    property Particles: TParticles read GetParticles;
    property MaxSphereSize: double read GetMaxSphereSize;
    procedure Assign(Source: TPersistent); override;
  published
    property ParticleDistribution: TParticleDistribution
      read FParticleDistribution write SetParticleDistribution;
    property GridParticles: TGridDistribution read FGridParticles
      write SetGridParticles stored StoreGridParticles;
    property CylinderParticles: TCylSphereDistribution read FCylinderParticles
      write SetCylinderParticles stored StoreCylinderParticles;
    property SphereParticles: TCylSphereDistribution read FSphereParticles
      write SetSphereParticles stored StoreSphereParticles;
    property CustomParticles: TParticles read FCustomParticles
      write SetCustomParticles stored StoreCustomParticles;
    property Used: boolean read FUsed write SetUsed;
    property ReleaseTimes: TModpathTimes read FReleaseTimes write SetReleaseTimes;
  end;

implementation

uses OctTreeClass;
{ TParticles }

procedure TParticles.Assign(Source: TPersistent);
begin
  if Source is TParticles then
  begin
    if Count <> TParticles(Source).Count then
    begin
      InvalidateModel;
    end;
  end;
  inherited;
end;

procedure TParticles.ClearSpheres;
var
  Index: Integer;
  Item: TParticleLocation;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TParticleLocation;
    FreeAndNil(Item.FSphere);
  end;
end;

constructor TParticles.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TParticleLocation, InvalidateModelEvent);
  FMaxParticleSize := 1;
end;

procedure TParticles.UpdateMaxParticleSize(ParticleSize: Double);
begin
  if FMaxParticleSize > ParticleSize then
  begin
    FMaxParticleSize := ParticleSize;
  end;
end;

{ TParticleLocation }

procedure TParticleLocation.Assign(Source: TPersistent);
var
  SourceItem: TParticleLocation;
begin
  if Source is TParticleLocation then
  begin
    SourceItem := TParticleLocation(Source);
    X := SourceItem.X;
    Y := SourceItem.Y;
    Z := SourceItem.Z;
  end
  else
  begin
    inherited;
  end;
end;

procedure TParticleLocation.CreateSphere(aParentOwner: TGLBaseSceneObject);
begin
  FSphere := TGLSphere.CreateAsChild(aParentOwner);
  FSphere.Scale.X := 0.2;
  FSphere.Scale.Y := 0.2;
  FSphere.Scale.Z := 0.2;
  FSphere.Material.FrontProperties.Diffuse.Color := clrOrange;
  SetSphereLocation;
end;

procedure TParticleLocation.SetSphereLocation;
begin
  if FSphere <> nil then
  begin
    FSphere.Position.X := FX - 0.5;
    FSphere.Position.Y := Y - 0.5;
    FSphere.Position.Z := -Z + 0.5;
  end;
end;

destructor TParticleLocation.Destroy;
begin
  FSphere.Free;
  inherited;
end;

procedure TParticleLocation.InvalidateModel;
begin
  (Collection as TParticles).InvalidateModel;
end;

procedure TParticleLocation.SetX(const Value: double);
begin
  if FX <> Value then
  begin
    FX := Value;
    InvalidateModel;
  end;
  SetSphereLocation;
end;

procedure TParticleLocation.SetY(const Value: double);
begin
  if FY <> Value then
  begin
    FY := Value;
    InvalidateModel;
  end;
  SetSphereLocation;
end;

procedure TParticleLocation.SetZ(const Value: double);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    InvalidateModel;
  end;
  SetSphereLocation;
end;


{ TParticleStorage }

procedure TParticleStorage.Assign(Source: TPersistent);
var
  StorageSource: TParticleStorage;
begin
  if Source is TParticleStorage then
  begin
    StorageSource := TParticleStorage(Source);
    Used := StorageSource.Used;
    if Used then
    begin
      ParticleDistribution := StorageSource.ParticleDistribution;
      case ParticleDistribution of
        pdGrid: GridParticles := StorageSource.GridParticles;
        pdCylinder: CylinderParticles := StorageSource.CylinderParticles;
        pdSphere: SphereParticles := StorageSource.SphereParticles;
        pdIndividual: CustomParticles := StorageSource.CustomParticles;
        pdObjectLocation: ; // do nothing.
      end;
      ReleaseTimes := StorageSource.ReleaseTimes;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TParticleStorage.ClearSpheres;
begin
  GridParticles.Particles.ClearSpheres;
  CylinderParticles.Particles.ClearSpheres;
  SphereParticles.Particles.ClearSpheres;
  CustomParticles.ClearSpheres;
end;

constructor TParticleStorage.Create(Model: TBaseModel);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(OnInvalidateModelEvent);
//  inherited;
  FGridParticles := TGridDistribution.Create(OnInvalidateModelEvent);
  FCylinderParticles := TCylSphereDistribution.Create(OnInvalidateModelEvent);
  FCylinderParticles.FDistributionChoice := pdCylinder;
  FSphereParticles := TCylSphereDistribution.Create(OnInvalidateModelEvent);
  FSphereParticles.FDistributionChoice := pdSphere;
  FSphereParticles.FLayerCount := 5;
  FCustomParticles := TParticles.Create(Model);
  FReleaseTimes :=  TModpathTimes.Create(Model);
  FReleaseTimes.Add;
end;

destructor TParticleStorage.Destroy;
begin
  FReleaseTimes.Free;
  FCustomParticles.Free;
  FSphereParticles.Free;
  FCylinderParticles.Free;
  FGridParticles.Free;
  inherited;
end;

function TParticleStorage.GetMaxSphereSize: double;
var
  OctTree: TRbwOctTree;
  Index: Integer;
  Item: TParticleLocation;
  OtherX: Double;
  OtherY: Double;
  OtherZ: Double;
  Data: Pointer;
begin
  result := 0;
  try
  case ParticleDistribution of
    pdGrid:
      begin
        Particles;
        result := FGridParticles.FMaxParticleSize;
      end;
    pdCylinder:
      begin
        Particles;
        result := FCylinderParticles.FMaxParticleSize;
      end;
    pdSphere:
      begin
        Particles;
        result := FSphereParticles.FMaxParticleSize;
      end;
    pdIndividual:
      begin
        CustomParticles.FMaxParticleSize := 1;
        OctTree := TRbwOctTree.Create(nil);
        try
          OctTree.XMax := 1;
          OctTree.YMax := 1;
          OctTree.ZMax := 1;
          for Index := 0 to CustomParticles.Count -1 do
          begin
            begin
              Item := CustomParticles.Items[Index] as TParticleLocation;
              if OctTree.Count > 0 then
              begin
                OtherX := Item.X;
                OtherY := Item.Y;
                OtherZ := Item.Z;
                OctTree.FirstNearestPoint(OtherX, OtherY, OtherZ, Data);
                if (OtherX = Item.X)
                  and (OtherY = Item.Y)
                  and (OtherZ = Item.Z) then
                begin
                  Continue;
                end
                else
                begin
                  CustomParticles.UpdateMaxParticleSize(Sqrt(
                    Sqr(OtherX - Item.X)
                    + Sqr(OtherY - Item.Y)
                    + Sqr(OtherZ - Item.Z)));
                end;
              end;
              OctTree.AddPoint(Item.X, Item.Y, Item.Z, Item);
            end;
          end;
        finally
          OctTree.Free;
        end;
        result := CustomParticles.FMaxParticleSize;
      end;
    pdObjectLocation:
      begin
        result := 0;
      end
    else
      Assert(False);
  end;
  finally
    result := result * 0.8;
    if result > 0.2 then
    begin
      result := 0.2
    end;
  end;
end;

function TParticleStorage.GetParticles: TParticles;
begin
  result := nil;
  case ParticleDistribution of
    pdGrid: result := GridParticles.Particles;
    pdCylinder: result := CylinderParticles.Particles;
    pdSphere: result := SphereParticles.Particles;
    pdIndividual: result := CustomParticles;
    pdObjectLocation: result := nil;
    else Assert(False);
  end;
end;

procedure TParticleStorage.SetCustomParticles(const Value: TParticles);
begin
  FCustomParticles.Assign(Value);
end;

procedure TParticleStorage.SetCylinderParticles(
  const Value: TCylSphereDistribution);
begin
  FCylinderParticles.Assign(Value);
end;

procedure TParticleStorage.SetGridParticles(const Value: TGridDistribution);
begin
  FGridParticles.Assign(Value);
end;

procedure TParticleStorage.SetParticleDistribution(
  const Value: TParticleDistribution);
begin
  if FParticleDistribution <> Value then
  begin
    FParticleDistribution := Value;
    InvalidateModel;
  end;
end;

procedure TParticleStorage.SetReleaseTimes(const Value: TModpathTimes);
begin
  FReleaseTimes.Assign(Value);
end;

procedure TParticleStorage.SetSphereParticles(
  const Value: TCylSphereDistribution);
begin
  FSphereParticles.Assign(Value);
end;

procedure TParticleStorage.SetUsed(const Value: boolean);
begin
  if FUsed <> Value then
  begin
    InvalidateModel;
    FUsed := Value;
  end;
end;

function TParticleStorage.StoreCustomParticles: boolean;
begin
  result := ParticleDistribution = pdIndividual;
end;

function TParticleStorage.StoreCylinderParticles: boolean;
begin
  result := ParticleDistribution = pdCylinder;
end;

function TParticleStorage.StoreGridParticles: boolean;
begin
  result := ParticleDistribution = pdGrid;
end;

function TParticleStorage.StoreSphereParticles: boolean;
begin
  result := ParticleDistribution = pdSphere;
end;

{ TGridDistribution }

procedure TGridDistribution.Assign(Source: TPersistent);
var
  SourceGridDist: TGridDistribution;
begin
  if Source is TGridDistribution then
  begin
    SourceGridDist := TGridDistribution(Source);
    LeftFace := SourceGridDist.LeftFace;
    RightFace := SourceGridDist.RightFace;
    BackFace := SourceGridDist.BackFace;
    FrontFace := SourceGridDist.FrontFace;
    BottomFace := SourceGridDist.BottomFace;
    TopFace := SourceGridDist.TopFace;
    Internal := SourceGridDist.Internal;
    XCount := SourceGridDist.XCount;
    YCount := SourceGridDist.YCount;
    ZCount := SourceGridDist.ZCount;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGridDistribution.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;
  FInternal := True;
  FXCount := 1;
  FYCount := 1;
  FZCount := 1;
end;

procedure TGridDistribution.CreateParticlesOnXFace(X: Double);
var
  Item: TParticleLocation;
  Z: Double;
  ZIndex: Integer;
  Y: Double;
  YIndex: Integer;
  ZOffset: Double;
  YOffset: Double;
  ZMax: Integer;
  YMax: Integer;
begin
  YMax := YCount;
  ZMax := ZCount;
  YOffset := 1 / YMax / 2;
  ZOffset := 1 / ZMax / 2;
  UpdateMaxParticleSize(1/YMax);
  UpdateMaxParticleSize(1/ZMax);
  for YIndex := 1 to YMax do
  begin
    Y := YIndex / YMax - YOffset;
    for ZIndex := 1 to ZMax do
    begin
      Z := ZIndex / ZMax - ZOffset;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TGridDistribution.CreateParticlesOnYFace(Y: Double);
var
  XMax: Integer;
  ZMax: Integer;
  XOffset: Double;
  ZOffset: Double;
  XIndex: Integer;
  X: Double;
  ZIndex: Integer;
  Z: Double;
  Item: TParticleLocation;
begin
  XMax := XCount;
  ZMax := ZCount;
  XOffset := 1 / XMax / 2;
  ZOffset := 1 / ZMax / 2;
  UpdateMaxParticleSize(1/XMax);
  UpdateMaxParticleSize(1/ZMax);
  for XIndex := 1 to XMax do
  begin
    X := XIndex / XMax - XOffset;
    for ZIndex := 1 to ZMax do
    begin
      Z := ZIndex / ZMax - ZOffset;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TGridDistribution.CreateParticlesOnZFace(Z: Double);
var
  XMax: Integer;
  YMax: Integer;
  XOffset: Double;
  YOffset: Double;
  XIndex: Integer;
  X: Double;
  YIndex: Integer;
  Y: Double;
  Item: TParticleLocation;
begin
  XMax := XCount;
  YMax := YCount;
  XOffset := 1 / XMax / 2;
  YOffset := 1 / YMax / 2;
  UpdateMaxParticleSize(1/XMax);
  UpdateMaxParticleSize(1/YMax);
  for XIndex := 1 to XMax do
  begin
    X := XIndex / XMax - XOffset;
    for YIndex := 1 to YMax do
    begin
      Y := YIndex / YMax - YOffset;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

destructor TGridDistribution.Destroy;
begin
  FParticles.Free;
  inherited;
end;

function TGridDistribution.GetParticles: TParticles;
var
  ZIndex: Integer;
  ZOffset: Double;
  ZMax: Integer;
  Z: Double;
begin
  if FParticles = nil then
  begin
    FMaxParticleSize := 1;
    FParticles := TParticles.Create(nil);
    if LeftFace then
    begin
      CreateParticlesOnXFace(0);
    end;
    if RightFace then
    begin
      CreateParticlesOnXFace(1);
    end;
    if BackFace then
    begin
      CreateParticlesOnYFace(0);
    end;
    if FrontFace then
    begin
      CreateParticlesOnYFace(1);
    end;
    if BottomFace then
    begin
      CreateParticlesOnZFace(0);
    end;
    if TopFace then
    begin
      CreateParticlesOnZFace(1);
    end;
    if Internal then
    begin
      ZMax := ZCount;
      ZOffset := 1 / ZMax / 2;
      UpdateMaxParticleSize(1 / ZMax);
      for ZIndex := 1 to ZMax do
      begin
        Z := ZIndex / ZMax - ZOffset;
        CreateParticlesOnZFace(Z);
      end;
    end;
  end;
  result := FParticles;
end;

procedure TGridDistribution.InvalidateModel;
begin
  inherited;
  FreeAndNil(FParticles);
end;

procedure TGridDistribution.SetBackFace(const Value: boolean);
begin
  if FBackFace <> Value then
  begin
    FBackFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetBottomFace(const Value: boolean);
begin
  if FBottomFace <> Value then
  begin
    FBottomFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetFrontFace(const Value: boolean);
begin
  if FFrontFace <> Value then
  begin
    FFrontFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetInternal(const Value: boolean);
begin
  if FInternal <> Value then
  begin
    FInternal := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetLeftFace(const Value: boolean);
begin
  if FLeftFace <> Value then
  begin
    FLeftFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetRightFace(const Value: boolean);
begin
  if FRightFace <> Value then
  begin
    FRightFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetTopFace(const Value: boolean);
begin
  if FTopFace <> Value then
  begin
    FTopFace := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetXCount(const Value: integer);
begin
  if FXCount <> Value then
  begin
    FXCount := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetYCount(const Value: integer);
begin
  if FYCount <> Value then
  begin
    FYCount := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.SetZCount(const Value: integer);
begin
  if FZCount <> Value then
  begin
    FZCount := Value;
    InvalidateModel;
  end;
end;

procedure TGridDistribution.UpdateMaxParticleSize(ParticleSize: Double);
begin
  if FMaxParticleSize > ParticleSize then
  begin
    FMaxParticleSize := ParticleSize;
  end;
end;

{ TCylSphereDistribution }

procedure TCylSphereDistribution.Assign(Source: TPersistent);
var
  SourceDist: TCylSphereDistribution;
begin
  if Source is TCylSphereDistribution then
  begin
    SourceDist := TCylSphereDistribution(Source);
    DistributionChoice := SourceDist.DistributionChoice;
    Orientation := SourceDist.Orientation;
    CircleParticleCount := SourceDist.CircleParticleCount;
    LayerCount := SourceDist.LayerCount;
    Radius := SourceDist.Radius;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCylSphereDistribution.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;
  FCircleParticleCount := 8;
  FLayerCount := 1;
  FRadius := 0.4;
end;

procedure TCylSphereDistribution.CreateCylinderParticles;
begin
  UpdateMaxParticleSize(1 / LayerCount);
  if CircleParticleCount > 1 then
  begin
    UpdateMaxParticleSize(Radius * 2 * Pi / CircleParticleCount);
  end;
  case Orientation of
    pgoVertical:
      begin
        CreateVerticalCylinderParticles;
      end;
    pgoEastWest:
      begin
        CreateEastWestCylinderParticles;
      end;
    pgoNorthSouth:
      begin
        CreateNorthSouthCylinderParticles;
      end;
  else
    Assert(False);
  end;
end;

procedure TCylSphereDistribution.CreateEastWestCylinderParticles;
var
  XIndex: Integer;
  XOffset: Double;
  XMax: Integer;
  Item: TParticleLocation;
  Z: Double;
  Y: Double;
  Angle: Double;
  CircumIndex: Integer;
  X: Double;
begin
  // East-West
  XMax := LayerCount;
  XOffset := 1 / XMax / 2;
  for XIndex := 1 to XMax do
  begin
    X := XIndex / XMax - XOffset;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      Y := Cos(Angle) * Radius + 0.5;
      Z := Sin(Angle) * Radius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TCylSphereDistribution.CreateNorthSouthCylinderParticles;
var
  YOffset: Double;
  YIndex: Integer;
  Y: Double;
  CircumIndex: Integer;
  Angle: Double;
  X: Double;
  Z: Double;
  Item: TParticleLocation;
  YMax: Integer;
begin
  // North-South
  YMax := LayerCount;
  YOffset := 1 / YMax / 2;
  for YIndex := 1 to YMax do
  begin
    Y := YIndex / YMax - YOffset;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      X := Cos(Angle) * Radius + 0.5;
      Z := Sin(Angle) * Radius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TCylSphereDistribution.CreateSphereParticles;
begin
  UpdateMaxParticleSize(Radius * 2 / (LayerCount - 1));
  case Orientation of
    pgoVertical:
      begin
        CreateVerticalSphereParticles;
      end;
    pgoEastWest:
      begin
        CreateEastWestSphereParticles;
      end;
    pgoNorthSouth:
      begin
        CreateNorthSouthSphereParticles;
      end;
  else
    Assert(False);
  end;
end;

procedure TCylSphereDistribution.CreateVerticalCylinderParticles;
var
  Angle: Double;
  CircumIndex: Integer;
  Z: Double;
  ZIndex: Integer;
  ZOffset: Double;
  ZMax: Integer;
  Item: TParticleLocation;
  Y: Double;
  X: Double;
begin
  // Vertical
  ZMax := LayerCount;
  ZOffset := 1 / ZMax / 2;
  for ZIndex := 1 to ZMax do
  begin
    Z := ZIndex / ZMax - ZOffset;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      X := Cos(Angle) * Radius + 0.5;
      Y := Sin(Angle) * Radius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TCylSphereDistribution.CreateEastWestSphereParticles;
var
  XMax: Integer;
  XOffset: Double;
  X: Double;
  Y: Double;
  Z: Double;
  Item: TParticleLocation;
  XIndex: Integer;
  CircleRadius: Double;
  CircumIndex: Integer;
  Angle: Double;
begin
  XMax := LayerCount;
  XOffset := Radius * 2 / (XMax - 1);
  X := 0.5 - Radius;
  Y := 0.5;
  Z := 0.5;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;

  X := 0.5 + Radius;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;
  
  for XIndex := 2 to XMax - 1 do
  begin
    X := 0.5 - Radius + (XIndex - 1) * XOffset;
    CircleRadius := Sqrt(Sqr(Radius) - Sqr(X - 0.5));
    if CircleParticleCount > 1 then
    begin
      UpdateMaxParticleSize(CircleRadius * 2 * Pi / CircleParticleCount);
    end;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      Y := Cos(Angle) * CircleRadius + 0.5;
      Z := Sin(Angle) * CircleRadius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TCylSphereDistribution.CreateNorthSouthSphereParticles;
var
  CircumIndex: Integer;
  Angle: Double;
  Z: Double;
  YMax: Integer;
  YOffset: Double;
  X: Double;
  Y: Double;
  Item: TParticleLocation;
  YIndex: Integer;
  CircleRadius: Double;
begin
  YMax := LayerCount;
  YOffset := Radius * 2 / (YMax - 1);
  X := 0.5;
  Y := 0.5 - Radius;
  Z := 0.5;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;

  Y := 0.5 + Radius;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;
  for YIndex := 2 to YMax - 1 do
  begin
    Y := 0.5 - Radius + (YIndex - 1) * YOffset;
    CircleRadius := Sqrt(Sqr(Radius) - Sqr(Y - 0.5));
    if CircleParticleCount > 1 then
    begin
      UpdateMaxParticleSize(CircleRadius * 2 * Pi / CircleParticleCount);
    end;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      X := Cos(Angle) * CircleRadius + 0.5;
      Z := Sin(Angle) * CircleRadius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

procedure TCylSphereDistribution.CreateVerticalSphereParticles;
var
  Angle: Double;
  CircumIndex: Integer;
  CircleRadius: Double;
  ZIndex: Integer;
  Item: TParticleLocation;
  Z: Double;
  Y: Double;
  X: Double;
  ZOffset: Double;
  ZMax: Integer;
begin
  ZMax := LayerCount;
  ZOffset := Radius * 2 / (ZMax - 1);

  X := 0.5;
  Y := 0.5;
  Z := 0.5 - Radius;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;

  Z := 0.5 + Radius;
  Item := FParticles.Add as TParticleLocation;
  Item.X := X;
  Item.Y := Y;
  Item.Z := Z;
  for ZIndex := 2 to ZMax - 1 do
  begin
    Z := 0.5 - Radius + (ZIndex - 1) * ZOffset;
    CircleRadius := Sqrt(Sqr(Radius) - Sqr(Z - 0.5));
    if CircleParticleCount > 1 then
    begin
      UpdateMaxParticleSize(CircleRadius * 2 * Pi / CircleParticleCount);
    end;
    for CircumIndex := 0 to CircleParticleCount - 1 do
    begin
      Angle := CircumIndex / CircleParticleCount * 2 * Pi;
      X := Cos(Angle) * CircleRadius + 0.5;
      Y := Sin(Angle) * CircleRadius + 0.5;
      Item := FParticles.Add as TParticleLocation;
      Item.X := X;
      Item.Y := Y;
      Item.Z := Z;
    end;
  end;
end;

destructor TCylSphereDistribution.Destroy;
begin
  FParticles.Free;
  inherited;
end;

function TCylSphereDistribution.GetParticles: TParticles;
begin
  if FParticles = nil then
  begin
    FMaxParticleSize := 1;
    FParticles := TParticles.Create(nil);
    case DistributionChoice of
      pdCylinder:
        begin
          CreateCylinderParticles;
        end;
      pdSphere:
        begin
          CreateSphereParticles;
        end;
      else
        Assert(False);
    end;
  end;
  result := FParticles;
end;

procedure TCylSphereDistribution.InvalidateModel;
begin
  inherited;
  FreeAndNil(FParticles);
end;

procedure TCylSphereDistribution.SetCircleParticleCount(const Value: integer);
begin
  if FCircleParticleCount <> Value then
  begin
    FCircleParticleCount := Value;
    InvalidateModel;
  end;
end;

procedure TCylSphereDistribution.SetDistributionChoice(
  const Value: TParticleDistribution);
begin
  if FDistributionChoice <> Value then
  begin
    Assert(Value in [pdCylinder, pdSphere]);
    FDistributionChoice := Value;
    InvalidateModel;
  end;
end;

procedure TCylSphereDistribution.SetLayerCount(const Value: integer);
begin
  if FLayerCount <> Value then
  begin
    FLayerCount := Value;
    InvalidateModel;
  end;
end;

procedure TCylSphereDistribution.SetOrientation(
  const Value: TParticleGroupOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    InvalidateModel;
  end;
end;

procedure TCylSphereDistribution.SetRadius(const Value: double);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    InvalidateModel;
  end;
end;

procedure TCylSphereDistribution.UpdateMaxParticleSize(ParticleSize: Double);
begin
  if FMaxParticleSize > ParticleSize then
  begin
    FMaxParticleSize := ParticleSize;
  end;
end;

{ TModpathTimeItem }

procedure TModpathTimeItem.Assign(Source: TPersistent);
begin
  if Source is TModpathTimeItem then
  begin
    Time := TModpathTimeItem(Source).Time;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModpathTimeItem.InvalidateModel;
begin
  (Collection as TModpathTimes).InvalidateModel;
end;

procedure TModpathTimeItem.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    InvalidateModel;
    FTime := Value;
  end;
end;

{ TModpathReleaseTimes }

procedure TModpathTimes.Assign(Source: TPersistent);
begin
  if Source is TModpathTimes then
  begin
    if Count <> TModpathTimes(Source).Count then
    begin
      InvalidateModel;
    end;
  end;
  inherited;
end;

constructor TModpathTimes.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TModpathTimeItem, InvalidateModelEvent);
end;

function TModpathTimes.GetItem(Index: Integer): TModpathTimeItem;
begin
  result := inherited Items[Index] as TModpathTimeItem;
end;

procedure TModpathTimes.SetItem(Index: Integer; const Value: TModpathTimeItem);
begin
  inherited Items[Index] := Value;
end;

end.
