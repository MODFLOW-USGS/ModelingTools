unit frameModpathParticlesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, StdCtrls, GLGeomObjects,
  OctTreeClass, Grids, RbwDataGrid4, ExtCtrls, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvGroupBox, ComCtrls, Buttons, JvPageList, JvExControls,
  ModpathParticleUnit, ModflowPackageSelectionUnit, GLCoordinates,
  GLCrossPlatform,
{$IF CompilerVersion < 23}
  BaseClasses;
{$ELSE}
  GLBaseClasses;
{$IFEND}


type
  TframeModpathParticles = class(TFrame)
    OctTree: TRbwOctTree;
    GLScene1: TGLScene;
    GLDummyCube: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    BottomPlane: TGLPlane;
    LeftPlane: TGLPlane;
    BackPlane: TGLPlane;
    GLCylinder1: TGLCylinder;
    GLCylinder2: TGLCylinder;
    GLCylinder3: TGLCylinder;
    GLLightSource2: TGLLightSource;
    GLCamera: TGLCamera;
    plParticlePlacement: TJvPageList;
    jvspGrid: TJvStandardPage;
    cbLeftFace: TCheckBox;
    cbRightFace: TCheckBox;
    cbFrontFace: TCheckBox;
    cbBackFace: TCheckBox;
    cbBottomFace: TCheckBox;
    cbTopFace: TCheckBox;
    cbInternal: TCheckBox;
    seX: TJvSpinEdit;
    seY: TJvSpinEdit;
    seZ: TJvSpinEdit;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    jvspCylinder: TJvStandardPage;
    rgCylinderOrientation: TRadioGroup;
    seCylParticleCount: TJvSpinEdit;
    seCylLayerCount: TJvSpinEdit;
    seCylRadius: TJvSpinEdit;
    lblCylParticleCount: TLabel;
    lblClylLayerCount: TLabel;
    lblCylRadius: TLabel;
    jvspSphere: TJvStandardPage;
    rgSphereOrientation: TRadioGroup;
    seSphereParticleCount: TJvSpinEdit;
    seSphereLayerCount: TJvSpinEdit;
    seSphereRadius: TJvSpinEdit;
    lblSpherParticleCount: TLabel;
    lblSpherelLayerCount: TLabel;
    lblSphereRadius: TLabel;
    jvspIndividual: TJvStandardPage;
    rdgSpecific: TRbwDataGrid4;
    pnlBottom: TPanel;
    seSpecificParticleCount: TJvSpinEdit;
    lblCount: TLabel;
    gbParticles: TJvGroupBox;
    rgChoice: TRadioGroup;
    GLSceneViewer1: TGLSceneViewer;
    jvspBlank: TJvStandardPage;
    seTimeCount: TJvSpinEdit;
    lblTimeCount: TLabel;
    rdgReleaseTimes: TRbwDataGrid4;
    sbDeleteRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbAddRow: TSpeedButton;
    sbAddTime: TSpeedButton;
    sbInsertTime: TSpeedButton;
    sbDeleteTime: TSpeedButton;
    lblMessage: TLabel;
    procedure rdgSpecificEndUpdate(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure sbAddTimeClick(Sender: TObject);
    procedure sbInsertTimeClick(Sender: TObject);
    procedure sbDeleteTimeClick(Sender: TObject);
    procedure rdgReleaseTimesEndUpdate(Sender: TObject);
    procedure gbParticlesCheckBoxClick(Sender: TObject);
  private
    FParticleStorage: TParticleStorage;
    FCanCreateParticles: Boolean;
    FTrackingDirection: TTrackingDirection;
    FMPathVersion: TMpathVersion;
    FObjectChoice: Boolean;
    procedure CreateGridParticles;
    procedure CreateCylinderParticles;
    procedure CreateSphereParticles;
    procedure CreateSpecificParticles;
    procedure ScaleParticles;
    procedure NumberSpecificGridRows;
    procedure NumberTimeGridRows;
    procedure SetTrackingDirection(const Value: TTrackingDirection);
    procedure EnableTrackingTimeGrid;
    procedure EnableDeleteTimeButton;
    procedure SetMPathVersion(const Value: TMpathVersion);
    procedure EnableTimeControls;
    { Private declarations }
  public
    procedure UpdateRowCount;
    procedure UpdateTimeRowCount;
    procedure CreateParticles;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure InitializeFrame;
    property TrackingDirection: TTrackingDirection read FTrackingDirection
      write SetTrackingDirection;
    property MPathVersion : TMpathVersion read FMPathVersion write SetMPathVersion;
    procedure SetObjectChoiceEnabled(Value: Boolean);
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Contnrs, Math;

resourcestring
  StrN = 'N';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrReleaseTrackingT = 'Release (tracking) time';

{ TframeModpathParticles }

constructor TframeModpathParticles.Create(AOwner: TComponent);
begin
  inherited;
  FParticleStorage := TParticleStorage.Create(nil);
  rdgSpecific.Cells[0,0] := StrN;
  rdgSpecific.Cells[1,0] := StrX;
  rdgSpecific.Cells[2,0] := StrY;
  rdgSpecific.Cells[3,0] := StrZ;
  rdgReleaseTimes.BeginUpdate;
  try
    rdgReleaseTimes.Cells[0,0] := StrN;
    rdgReleaseTimes.Cells[1,0] := StrReleaseTrackingT;
    NumberSpecificGridRows;
  finally
    rdgReleaseTimes.EndUpdate;
  end;
  NumberTimeGridRows;
  CreateParticles;
end;

procedure TframeModpathParticles.CreateCylinderParticles;
var
  CylinderParticles: TCylSphereDistribution;
begin
  if (rgCylinderOrientation.ItemIndex <> -1)
    and (seCylParticleCount.AsInteger <> 0)
    and (seCylLayerCount.AsInteger <> 0)
    and (seCylRadius.Value <> 0) then
  begin
    FCanCreateParticles := True;
    CylinderParticles := FParticleStorage.CylinderParticles;
    CylinderParticles.Orientation := TParticleGroupOrientation(
      rgCylinderOrientation.ItemIndex);
    CylinderParticles.CircleParticleCount := seCylParticleCount.AsInteger;
    CylinderParticles.LayerCount := seCylLayerCount.AsInteger;
    CylinderParticles.Radius := seCylRadius.Value;
  end;
end;

procedure TframeModpathParticles.CreateGridParticles;
var
  GridParticles: TGridDistribution;
//  FCanCreateParticles: Boolean;
begin
  if not cbLeftFace.AllowGrayed
    and not cbRightFace.AllowGrayed
    and not cbBackFace.AllowGrayed
    and not cbFrontFace.AllowGrayed
    and not cbBottomFace.AllowGrayed
    and not cbTopFace.AllowGrayed
    and not cbInternal.AllowGrayed
    and (seX.AsInteger > 0)
    and (seY.AsInteger > 0)
    and (seZ.AsInteger > 0) then
  begin
    FCanCreateParticles := True;
    GridParticles := FParticleStorage.GridParticles;
    GridParticles.LeftFace := cbLeftFace.Checked;
    GridParticles.RightFace := cbRightFace.Checked;
    GridParticles.BackFace := cbBackFace.Checked;
    GridParticles.FrontFace := cbFrontFace.Checked;
    GridParticles.BottomFace := cbBottomFace.Checked;
    GridParticles.TopFace := cbTopFace.Checked;
    GridParticles.Internal := cbInternal.Checked;
    GridParticles.XCount := seX.AsInteger;
    GridParticles.YCount := seY.AsInteger;
    GridParticles.ZCount := seZ.AsInteger;
  end;
end;

procedure TframeModpathParticles.CreateParticles;
begin
  GLScene1.BeginUpdate;
  try
    FParticleStorage.ClearSpheres;
    FCanCreateParticles := False;
    if rgChoice.ItemIndex >= 0 then
    begin
      FParticleStorage.ParticleDistribution
        := TParticleDistribution(rgChoice.ItemIndex);
      case FParticleStorage.ParticleDistribution of
        pdGrid:
          begin
            CreateGridParticles;
          end;
        pdCylinder:
          begin
            CreateCylinderParticles;
          end;
        pdSphere:
          begin
            CreateSphereParticles;
          end;
        pdIndividual:
          begin
            CreateSpecificParticles;
          end;
        pdObjectLocation:
          begin

          end
        else
          Assert(False);
      end;
      ScaleParticles;
    end;
  finally
    GLScene1.EndUpdate;
  end;
end;

procedure TframeModpathParticles.CreateSpecificParticles;
var
  Z: Double;
  Y: Double;
  X: Double;
  Index: Integer;
  Item: TParticleLocation;
  CustomParticles: TParticles;
begin
  if seSpecificParticleCount.AsInteger > 0 then
  begin
    FCanCreateParticles := True;
    CustomParticles := FParticleStorage.CustomParticles;
    CustomParticles.Clear;
    for Index := 1 to seSpecificParticleCount.AsInteger do
    begin
      if TryStrToFloat(rdgSpecific.Cells[1, Index], X)
        and TryStrToFloat(rdgSpecific.Cells[2, Index], Y)
        and TryStrToFloat(rdgSpecific.Cells[3, Index], Z) then
      begin
        Item := CustomParticles.Add as TParticleLocation;
        Item.X := X;
        Item.Y := Y;
        Item.Z := Z;
      end;
    end;
  end;
end;

destructor TframeModpathParticles.Destroy;
begin
  FParticleStorage.Free;
  inherited;
end;

procedure TframeModpathParticles.InitializeFrame;
begin
  gbParticles.Checked := False;

  rgChoice.ItemIndex := 0;
  plParticlePlacement.ActivePageIndex := 0;

  cbLeftFace.Checked := False;
  cbFrontFace.Checked := False;
  cbBottomFace.Checked := False;
  cbRightFace.Checked := False;
  cbBackFace.Checked := False;
  cbTopFace.Checked := False;
  cbInternal.Checked := True;
  seX.AsInteger := 1;
  seY.AsInteger := 1;
  seZ.AsInteger := 1;

  rgCylinderOrientation.ItemIndex := 0;
  seCylParticleCount.AsInteger := 8;
  seCylLayerCount.AsInteger := 1;
  seCylRadius.Value := 0.4;

  rgSphereOrientation.ItemIndex := 0;
  seSphereParticleCount.AsInteger := 8;
  seSphereLayerCount.AsInteger := 5;
  seSphereRadius.Value := 0.4;

  seSpecificParticleCount.AsInteger := 0;
  rdgSpecific.FixedCols := 1;
  rdgSpecific.RowCount := 2;
  rdgSpecific.Cells[1,1] := '';
  rdgSpecific.Cells[2,1] := '';
  rdgSpecific.Cells[3,1] := '';

  seTimeCount.AsInteger := 1;
  rdgReleaseTimes.FixedCols := 1;
  rdgReleaseTimes.RowCount := 2;
  rdgReleaseTimes.BeginUpdate;
  try
    rdgReleaseTimes.Cells[1,1] := '0';
  finally
    rdgReleaseTimes.EndUpdate;
  end;
end;

procedure TframeModpathParticles.EnableTimeControls;
var
  ShouldEnable: Boolean;
begin
  EnableTrackingTimeGrid;
  ShouldEnable := ((TrackingDirection = tdForward) or (MPathVersion = mp6));
  seTimeCount.Enabled := ShouldEnable;
  sbAddTime.Enabled := ShouldEnable;
  sbInsertTime.Enabled := ShouldEnable;
  EnableDeleteTimeButton;
end;

procedure TframeModpathParticles.EnableDeleteTimeButton;
begin
  sbDeleteTime.Enabled :=
    ((TrackingDirection = tdForward) or (MPathVersion= mp6))
    and (seTimeCount.AsInteger > 1);
end;

procedure TframeModpathParticles.EnableTrackingTimeGrid;
begin
  rdgReleaseTimes.Enabled := (seTimeCount.AsInteger > 0)
    and ((TrackingDirection = tdForward) or (MPathVersion= mp6));
end;

procedure TframeModpathParticles.gbParticlesCheckBoxClick(Sender: TObject);
begin
  if gbParticles.Checked then
  begin
    SetObjectChoiceEnabled(FObjectChoice);
  end;
end;

procedure TframeModpathParticles.CreateSphereParticles;
var
  SphereParticles: TCylSphereDistribution;
begin
    if (rgSphereOrientation.ItemIndex <> -1)
      and (seSphereParticleCount.AsInteger <> 0)
      and (seSphereLayerCount.AsInteger <> 0)
      and (seSphereRadius.Value <> 0) then
  begin
    FCanCreateParticles := True;
    SphereParticles := FParticleStorage.SphereParticles;
    SphereParticles.Orientation := TParticleGroupOrientation(
      rgSphereOrientation.ItemIndex);
    SphereParticles.CircleParticleCount := seSphereParticleCount.AsInteger;
    SphereParticles.LayerCount := seSphereLayerCount.AsInteger;
    SphereParticles.Radius := seSphereRadius.Value;
  end;
end;

procedure TframeModpathParticles.NumberSpecificGridRows;
var
  Index: Integer;
begin
  rdgReleaseTimes.BeginUpdate;
  try
    for Index := 1 to rdgSpecific.RowCount - 1 do
    begin
      rdgSpecific.Cells[0, Index] := IntToStr(Index);
    end;
  finally
    rdgReleaseTimes.EndUpdate;
  end;
end;

procedure TframeModpathParticles.NumberTimeGridRows;
var
  Index: Integer;
begin
  for Index := 1 to rdgReleaseTimes.RowCount - 1 do
  begin
    rdgReleaseTimes.Cells[0, Index] := IntToStr(Index);
  end;
end;

procedure TframeModpathParticles.rdgReleaseTimesEndUpdate(Sender: TObject);
begin
  if seTimeCount <> nil then
  begin
    seTimeCount.AsInteger := rdgReleaseTimes.RowCount -1;
    if Assigned(seTimeCount.OnChange) then
    begin
      seTimeCount.OnChange(seTimeCount);
    end;
  end;

end;

procedure TframeModpathParticles.rdgSpecificEndUpdate(Sender: TObject);
begin
  if seSpecificParticleCount <> nil then
  begin
    seSpecificParticleCount.AsInteger := rdgSpecific.RowCount -1;
    Assert(Assigned(seSpecificParticleCount.OnChange));
    seSpecificParticleCount.OnChange(seSpecificParticleCount);
  end;
end;

procedure TframeModpathParticles.sbAddRowClick(Sender: TObject);
begin
  seSpecificParticleCount.AsInteger := seSpecificParticleCount.AsInteger + 1;
  UpdateRowCount;
end;

procedure TframeModpathParticles.sbAddTimeClick(Sender: TObject);
begin
  seTimeCount.AsInteger := seTimeCount.AsInteger + 1;
  UpdateTimeRowCount;
end;

procedure TframeModpathParticles.sbDeleteRowClick(Sender: TObject);
begin
  if rdgSpecific.RowCount = 2 then
  begin
    seSpecificParticleCount.AsInteger := 0;
  end
  else
  begin
    if (rdgSpecific.Row >= rdgSpecific.FixedRows)
      and (rdgSpecific.Row < rdgSpecific.RowCount) then
    begin
      rdgSpecific.DeleteRow(rdgSpecific.Row);
      seSpecificParticleCount.AsInteger := rdgSpecific.RowCount -1;
    end;
  end;
  UpdateRowCount;
end;

procedure TframeModpathParticles.sbDeleteTimeClick(Sender: TObject);
begin
  if rdgReleaseTimes.RowCount = 2 then
  begin
    Exit
  end
  else
  begin
    if (rdgReleaseTimes.Row >= rdgReleaseTimes.FixedRows)
      and (rdgReleaseTimes.Row < rdgReleaseTimes.RowCount) then
    begin
      rdgReleaseTimes.DeleteRow(rdgReleaseTimes.Row);
      seTimeCount.AsInteger := rdgReleaseTimes.RowCount -1;
    end;
  end;
  UpdateTimeRowCount;
end;

procedure TframeModpathParticles.sbInsertRowClick(Sender: TObject);
begin
  if (rdgSpecific.Row >= rdgSpecific.FixedRows)
    and (rdgSpecific.Row < rdgSpecific.RowCount) then
  begin
    rdgSpecific.InsertRow(rdgSpecific.Row);
    seSpecificParticleCount.AsInteger := rdgSpecific.RowCount -1;
    UpdateRowCount;
  end;
end;

procedure TframeModpathParticles.sbInsertTimeClick(Sender: TObject);
begin
  if (rdgReleaseTimes.Row >= rdgReleaseTimes.FixedRows)
    and (rdgReleaseTimes.Row < rdgReleaseTimes.RowCount) then
  begin
    rdgReleaseTimes.InsertRow(rdgReleaseTimes.Row);
    seTimeCount.AsInteger := rdgReleaseTimes.RowCount -1;
    UpdateTimeRowCount;
  end;
end;

procedure TframeModpathParticles.ScaleParticles;
var
  Index: Integer;
  Item: TParticleLocation;
  Particles: TParticles;
  MaxParticleSize: double;
begin
  Particles := FParticleStorage.Particles;
  if Particles <> nil then
  begin
    MaxParticleSize := FParticleStorage.MaxSphereSize;
    for Index := 0 to Particles.Count - 1 do
    begin
      Item := Particles.Items[Index] as TParticleLocation;
      Item.CreateSphere(GLDummyCube);
      Item.Sphere.Scale.X := MaxParticleSize;
      Item.Sphere.Scale.Y := MaxParticleSize;
      Item.Sphere.Scale.Z := MaxParticleSize;
    end;
  end;
end;

procedure TframeModpathParticles.SetMPathVersion(const Value: TMpathVersion);
begin
  FMPathVersion := Value;
  EnableTimeControls;
end;

procedure TframeModpathParticles.SetObjectChoiceEnabled(Value: Boolean);
begin
  FObjectChoice := Value;
  rgChoice.Controls[Ord(pdObjectLocation)].Enabled := Value;
end;

procedure TframeModpathParticles.SetTrackingDirection(
  const Value: TTrackingDirection);
begin
  FTrackingDirection := Value;
  EnableTimeControls;
end;

procedure TframeModpathParticles.UpdateRowCount;
begin
  if seSpecificParticleCount.AsInteger <= 0 then
  begin
    rdgSpecific.RowCount := 2;
    rdgSpecific.Enabled := False;
    sbDeleteRow.Enabled := False;
  end
  else
  begin
    sbDeleteRow.Enabled := True;
    rdgSpecific.Enabled := True;
    rdgSpecific.RowCount := seSpecificParticleCount.AsInteger + 1;
  end;
  NumberSpecificGridRows;
  CreateParticles;
end;

procedure TframeModpathParticles.UpdateTimeRowCount;
begin
  if seTimeCount.AsInteger <= 1 then
  begin
    rdgReleaseTimes.RowCount := 2;
  end
  else
  begin
    rdgReleaseTimes.RowCount := seTimeCount.AsInteger + 1;
  end;
  EnableDeleteTimeButton;
  EnableTrackingTimeGrid;
  NumberTimeGridRows;
end;

end.
