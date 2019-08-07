unit frameMt3dmsAdvPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, ArgusDataEntry, StdCtrls, ComCtrls, RbwController,
  ModflowPackageSelectionUnit, Mask, JvExMask, JvSpin, GrayTabs;

type
  TframeMt3dmsAdvPkg = class(TframePackage)
    pcAdvection: TPageControl;
    tabAdvection1: TTabSheet;
    tabAdvection2: TTabSheet;
    lbllMethod: TLabel;
    comboAdvSolScheme: TComboBox;
    lblParticleTracking: TLabel;
    comboParticleTrackingAlg: TComboBox;
    lbNumCellsParticle: TLabel;
    lblMaxParticlesCount: TLabel;
    lblConcWeight: TLabel;
    lblNegConcGrad: TLabel;
    lblInitParticlesSmall: TLabel;
    lblInitParticlesLarge: TLabel;
    Label12: TLabel;
    comboAdvWeightingScheme: TComboBox;
    adeMaxParticleMovement: TRbwDataEntry;
    adeConcWeight: TRbwDataEntry;
    adeNeglSize: TRbwDataEntry;
    lblInitParticlePlacement: TLabel;
    lblInitParticlePlanes: TLabel;
    lblMinParticles: TLabel;
    lblMaxParticles: TLabel;
    lblSinkParticlePlacement: TLabel;
    lblSinkParticlePlanes: TLabel;
    lblSinkParticleN: TLabel;
    lblCritConcGrad: TLabel;
    comboInitPartPlace: TComboBox;
    comboInitPartSinkChoice: TComboBox;
    adeCritRelConcGrad: TRbwDataEntry;
    spinMaxParticlesCount: TJvSpinEdit;
    spinInitParticlesSmall: TJvSpinEdit;
    spinInitParticlesLarge: TJvSpinEdit;
    spinInitParticlePlanes: TJvSpinEdit;
    spinMinParticles: TJvSpinEdit;
    spinMaxParticles: TJvSpinEdit;
    spinSinkParticlePlanes: TJvSpinEdit;
    spinSinkParticleN: TJvSpinEdit;
    procedure comboInitPartPlaceChange(Sender: TObject);
    procedure comboInitPartSinkChoiceChange(Sender: TObject);
    procedure comboAdvSolSchemeChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure Loaded; override;
    { Public declarations }
  end;

var
  frameMt3dmsAdvPkg: TframeMt3dmsAdvPkg;

implementation

{$R *.dfm}

{ TframeMt3dmsAdvPkg }

procedure TframeMt3dmsAdvPkg.comboAdvSolSchemeChange(Sender: TObject);
var
  Scheme: TAdvectionSolution;
  UseRecordB2: Boolean;
  UseRecordB3: Boolean;
  UseRecordB4: Boolean;
  UseRecordB5: Boolean;
begin
  inherited;
  Scheme := TAdvectionSolution(comboAdvSolScheme.ItemIndex);


  UseRecordB2 := rcSelectionController.Enabled and (Scheme in [asMoc, asMmoc, asHmoc]);
  comboParticleTrackingAlg.Enabled := UseRecordB2;
  adeConcWeight.Enabled := UseRecordB2;

  UseRecordB3 := rcSelectionController.Enabled and (Scheme in [asMoc, asHmoc]);
  spinMaxParticlesCount.Enabled := UseRecordB3;
  adeNeglSize.Enabled := UseRecordB3;
  spinInitParticlesSmall.Enabled := UseRecordB3;
  spinInitParticlesLarge.Enabled := UseRecordB3;
  comboInitPartPlace.Enabled := UseRecordB3;
  comboInitPartPlaceChange(nil);
  spinMinParticles.Enabled := UseRecordB3;
  spinMaxParticles.Enabled := UseRecordB3;

  UseRecordB4 := rcSelectionController.Enabled and (Scheme in [asMmoc, asHmoc]);
  comboInitPartSinkChoice.Enabled := UseRecordB4;
  comboInitPartSinkChoiceChange(nil);
  spinSinkParticleN.Enabled := UseRecordB4;

  UseRecordB5 := rcSelectionController.Enabled and (Scheme = asHmoc);
  adeCritRelConcGrad.Enabled := UseRecordB5;
end;

procedure TframeMt3dmsAdvPkg.comboInitPartPlaceChange(Sender: TObject);
var
  Scheme: TAdvectionSolution;
  UseRecordB3: Boolean;
begin
  inherited;
  Scheme := TAdvectionSolution(comboAdvSolScheme.ItemIndex);
  UseRecordB3 := rcSelectionController.Enabled and (Scheme in [asMoc, asHmoc]);
  spinInitParticlePlanes.Enabled := UseRecordB3
    and (comboInitPartPlace.ItemIndex = 1)
end;

procedure TframeMt3dmsAdvPkg.comboInitPartSinkChoiceChange(Sender: TObject);
var
  Scheme: TAdvectionSolution;
  UseRecordB4: Boolean;
begin
  inherited;
  Scheme := TAdvectionSolution(comboAdvSolScheme.ItemIndex);
  UseRecordB4 := rcSelectionController.Enabled and (Scheme in [asMmoc, asHmoc]);
  spinSinkParticlePlanes.Enabled := UseRecordB4
    and (comboInitPartSinkChoice.ItemIndex = 1);
end;

procedure TframeMt3dmsAdvPkg.GetData(Package: TModflowPackageSelection);
var
  AdvPkg: TMt3dmsAdvection;
begin
  inherited;
  AdvPkg := Package as TMt3dmsAdvection;

  comboAdvSolScheme.ItemIndex := Ord(AdvPkg.AdvectionSolution);
  comboParticleTrackingAlg.ItemIndex := Ord(AdvPkg.ParticleTrackMethod);
  adeMaxParticleMovement.Text := FloatToStr(AdvPkg.Courant);
  spinMaxParticlesCount.AsInteger := AdvPkg.MaximumParticles;
  adeConcWeight.Text := FloatToStr(AdvPkg.ConcWeight);
  adeNeglSize.Text := FloatToStr(AdvPkg.RelCelConcGrad);
  spinInitParticlesSmall.AsInteger := AdvPkg.LowGradientParticleCount;
  spinInitParticlesLarge.AsInteger := AdvPkg.HighGradientParticleCount;
  comboAdvWeightingScheme.ItemIndex := Ord(AdvPkg.WeightingScheme);

  comboInitPartPlace.ItemIndex := Ord(AdvPkg.ParticlePlacementMethod);
  spinInitParticlePlanes.AsInteger := AdvPkg.NumberOfParticlePlanes;
  spinMinParticles.AsInteger := AdvPkg.MinParticlePerCell;
  spinMaxParticles.AsInteger := AdvPkg.MaxParticlesPerCell;
  comboInitPartSinkChoice.ItemIndex := Ord(AdvPkg.SinkParticlePlacementMethod);
  spinSinkParticlePlanes.AsInteger := AdvPkg.SinkNumberOfParticlePlanes;
  spinSinkParticleN.AsInteger := AdvPkg.SinkParticleCount;
  adeCritRelConcGrad.Text := FloatToStr(AdvPkg.CriticalConcGradient);

  comboAdvSolSchemeChange(nil);

end;

procedure TframeMt3dmsAdvPkg.Loaded;
begin
  inherited;
  pcAdvection.ActivePageIndex := 0;
end;

procedure TframeMt3dmsAdvPkg.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  comboAdvSolSchemeChange(nil);
end;

procedure TframeMt3dmsAdvPkg.SetData(Package: TModflowPackageSelection);
var
  AdvPkg: TMt3dmsAdvection;
begin
  inherited;
  AdvPkg := Package as TMt3dmsAdvection;

  AdvPkg.AdvectionSolution := TAdvectionSolution(comboAdvSolScheme.ItemIndex);
  AdvPkg.ParticleTrackMethod := TParticleTrackMethod(comboParticleTrackingAlg.ItemIndex);
  AdvPkg.Courant := StrToFloat(adeMaxParticleMovement.Text);
  AdvPkg.MaximumParticles := spinMaxParticlesCount.AsInteger;
  AdvPkg.ConcWeight := StrToFloat(adeConcWeight.Text);
  AdvPkg.RelCelConcGrad := StrToFloat(adeNeglSize.Text);
  AdvPkg.LowGradientParticleCount := spinInitParticlesSmall.AsInteger;
  AdvPkg.HighGradientParticleCount := spinInitParticlesLarge.AsInteger;
  AdvPkg.WeightingScheme := TWeightingScheme(comboAdvWeightingScheme.ItemIndex);

  AdvPkg.ParticlePlacementMethod := TParticlePlacementMethod(comboInitPartPlace.ItemIndex);
  AdvPkg.NumberOfParticlePlanes := spinInitParticlePlanes.AsInteger;
  AdvPkg.MinParticlePerCell := spinMinParticles.AsInteger;
  AdvPkg.MaxParticlesPerCell := spinMaxParticles.AsInteger;
  AdvPkg.SinkParticlePlacementMethod := TParticlePlacementMethod(comboInitPartSinkChoice.ItemIndex);
  AdvPkg.SinkNumberOfParticlePlanes := spinSinkParticlePlanes.AsInteger;
  AdvPkg.SinkParticleCount := spinSinkParticleN.AsInteger;
  AdvPkg.CriticalConcGradient := StrToFloat(adeCritRelConcGrad.Text);
end;

end.
