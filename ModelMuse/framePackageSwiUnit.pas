unit framePackageSwiUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController,
  StdCtrls, ComCtrls, Mask, JvExMask, JvSpin, JvExStdCtrls,
  JvCombobox, JvListComb, ArgusDataEntry, Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit, GrayTabs;

type
  TDensityColumns = (dcNumber, dcDensity);

  TframePackageSWI = class(TframePackage)
    pcSWI: TPageControl;
    tabBasic: TTabSheet;
    tabSolver: TTabSheet;
    tabDensity: TTabSheet;
    seNumberOfSurfaces: TJvSpinEdit;
    lblNumberOfSurfaces: TLabel;
    comboDensityChoice: TJvImageComboBox;
    lblDensityChoice: TLabel;
    cbSaveZeta: TCheckBox;
    comboObservations: TJvImageComboBox;
    cbAdaptive: TCheckBox;
    comboSolver: TJvImageComboBox;
    lblSolver: TLabel;
    lblObservations: TLabel;
    sePrintoutInterval: TJvSpinEdit;
    lblPrintoutInterval: TLabel;
    comboPCGPrint: TJvImageComboBox;
    lblPCGPrintControl: TLabel;
    seMaxIterOuter: TJvSpinEdit;
    lblMaxIterOuter: TLabel;
    seMaxIterInner: TJvSpinEdit;
    lblMaxIterInner: TLabel;
    comboPCGPrecondMeth: TJvImageComboBox;
    lblPCGMethod: TLabel;
    lblMaxZetaChange: TLabel;
    lblMaxRes: TLabel;
    rdeMaxZetaChange: TRbwDataEntry;
    rdeMaxRes: TRbwDataEntry;
    lblRelax: TLabel;
    rdeRelax: TRbwDataEntry;
    comboEigenValue: TJvImageComboBox;
    lblEigenValue: TLabel;
    lblDamp: TLabel;
    lblDampT: TLabel;
    rdeDamp: TRbwDataEntry;
    rdeDampT: TRbwDataEntry;
    rdeToeslope: TRbwDataEntry;
    lblToeslope: TLabel;
    rdeTipSlope: TRbwDataEntry;
    lblTipSlope: TLabel;
    rdeAlpha: TRbwDataEntry;
    rdeBeta: TRbwDataEntry;
    lblAlpha: TLabel;
    lblBeta: TLabel;
    seMaxAdaptiveSteps: TJvSpinEdit;
    lblMaxAdaptiveSteps: TLabel;
    seMinAdaptiveSteps: TJvSpinEdit;
    lblMinAdaptiveSteps: TLabel;
    rdeAdaptiveFactor: TRbwDataEntry;
    lblAdaptiveFactor: TLabel;
    rdgDensity: TRbwDataGrid4;
    comboModflowPrecision: TJvImageComboBox;
    lblModflowPrecision: TLabel;
    procedure seNumberOfSurfacesChange(Sender: TObject);
    procedure comboDensityChoiceChange(Sender: TObject);
    procedure cbAdaptiveClick(Sender: TObject);
    procedure comboSolverChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboPCGPrecondMethChange(Sender: TObject);
    procedure comboObservationsChange(Sender: TObject);
  private
    procedure InitializeGrid;
    procedure EnableAdaptiveControls;
    procedure EnableSolverControls;
    procedure EnableEigenValue;
    procedure EnableRelax;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageSWI: TframePackageSWI;

implementation

resourcestring
  StrDimensionlessDensit = 'Dimensionless Density';
  StrSurface = 'Surface';
  StrLayerTop = 'Layer Top';
  StrLayerBottom = 'Layer Bottom';
  StrSurface1 = 'Surface %d';
  StrZone = 'Zone';
  StrZone1 = 'Zone %d';

{$R *.dfm}

{ TframePackage3 }

procedure TframePackageSWI.cbAdaptiveClick(Sender: TObject);
begin
  inherited;
  EnableAdaptiveControls;
end;

procedure TframePackageSWI.comboDensityChoiceChange(Sender: TObject);
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageSWI.comboObservationsChange(Sender: TObject);
begin
  inherited;
  comboModflowPrecision.Enabled := rcSelectionController.Enabled and
    (comboObservations.ItemIndex = 2);
end;

procedure TframePackageSWI.comboPCGPrecondMethChange(Sender: TObject);
begin
  inherited;
  EnableEigenValue;
  EnableRelax
end;

procedure TframePackageSWI.comboSolverChange(Sender: TObject);
begin
  inherited;
  EnableSolverControls;
end;

procedure TframePackageSWI.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TSwiPackage;
  ItemIndex: Integer;
begin
  inherited GetData(Package);
  pcSWI.ActivePageIndex := 0;
  SourcePkg := Package as TSwiPackage;
  seNumberOfSurfaces.AsInteger := SourcePkg.NumberOfSurfaces;
  comboDensityChoice.ItemIndex := Ord(SourcePkg.DensityChoice);
  cbSaveZeta.Checked := SourcePkg.SaveZeta;
  comboObservations.ItemIndex := Ord(SourcePkg.ObsChoice);
  rdeToeslope.RealValue := SourcePkg.ToeSlope.Value;
  rdeTipSlope.RealValue := SourcePkg.TipSlope.Value;
  rdeAlpha.RealValue := SourcePkg.Alpha.Value;
  rdeBeta.RealValue := SourcePkg.Beta.Value;
  cbAdaptive.Checked := SourcePkg.Adaptive;
  seMaxAdaptiveSteps.AsInteger := SourcePkg.MaxAdaptiveTimeSteps;
  seMinAdaptiveSteps.AsInteger := SourcePkg.MinAdaptiveTimeSteps;
  rdeAdaptiveFactor.RealValue := SourcePkg.AdaptiveFactor.Value;

  comboSolver.ItemIndex := Ord(SourcePkg.Solver);
  sePrintoutInterval.AsInteger := SourcePkg.SolverPrintoutInterval;
  comboPCGPrint.ItemIndex := Ord(SourcePkg.SolverPrintChoice);
  seMaxIterOuter.AsInteger := SourcePkg.MXITER;
  seMaxIterInner.AsInteger := SourcePkg.ITER1;
  comboPCGPrecondMeth.ItemIndex := Ord(SourcePkg.NPCOND);
  rdeMaxZetaChange.RealValue := SourcePkg.ZCLOSE.Value;
  rdeMaxRes.RealValue := SourcePkg.RCLOSE.Value;
  rdeRelax.RealValue := SourcePkg.RELAX.Value;
  comboEigenValue.ItemIndex := Ord(SourcePkg.NBPOL);
  rdeDamp.RealValue := SourcePkg.DAMP.Value;
  rdeDampT.RealValue := SourcePkg.DAMPT.Value;
  comboModflowPrecision.ItemIndex := Ord(SourcePkg.ModflowPrecision);

  rdgDensity.BeginUpdate;
  try
    InitializeGrid;
    for ItemIndex := 0 to SourcePkg.ZoneDimensionlessDensities.Count - 1 do
    begin
      rdgDensity.Cells[Ord(dcDensity), ItemIndex+1] :=
        FloatToStr(SourcePkg.ZoneDimensionlessDensities[ItemIndex].Value);
    end;
  finally
    rdgDensity.EndUpdate
  end;
  InitializeGrid;
  rcSelectionControllerEnabledChange(nil);
end;

procedure TframePackageSWI.InitializeGrid;
var
  NSURF: Integer;
  RowIndex: Integer;
begin
  rdgDensity.BeginUpdate;
  try
    rdgDensity.Cells[Ord(dcDensity), 0] := StrDimensionlessDensit;
    NSURF := seNumberOfSurfaces.AsInteger;
    case TDensityChoice(comboDensityChoice.ItemIndex) of
      dcLinear:
        begin
          rdgDensity.Cells[Ord(dcNumber), 0] := StrSurface;
          rdgDensity.RowCount := NSURF + 3;
          rdgDensity.Cells[Ord(dcNumber), 1] := StrLayerTop;
          rdgDensity.Cells[Ord(dcNumber), rdgDensity.RowCount - 1] := StrLayerBottom;
          for RowIndex := 2 to rdgDensity.RowCount - 2 do
          begin
            rdgDensity.Cells[Ord(dcNumber), RowIndex] := Format(StrSurface1,[RowIndex-1]);
          end;
        end;
      dcZoned:
        begin
          rdgDensity.Cells[Ord(dcNumber), 0] := StrZone;
          rdgDensity.RowCount := NSURF + 2;
          for RowIndex := 1 to rdgDensity.RowCount - 1 do
          begin
            rdgDensity.Cells[Ord(dcNumber), RowIndex] := Format(StrZone1, [RowIndex]);
          end;
        end;
      else
        Assert(False);
    end;
  finally
    rdgDensity.EndUpdate;
  end;
end;

procedure TframePackageSWI.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
   EnableAdaptiveControls;
   EnableSolverControls;
   comboObservationsChange(nil);
end;

procedure TframePackageSWI.seNumberOfSurfacesChange(Sender: TObject);
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageSWI.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TSwiPackage;
  ItemIndex: Integer;
begin
  inherited SetData(Package);
  SourcePkg := Package as TSwiPackage;
  SourcePkg.NumberOfSurfaces := seNumberOfSurfaces.AsInteger;
  SourcePkg.DensityChoice := TDensityChoice(comboDensityChoice.ItemIndex);
  SourcePkg.SaveZeta := cbSaveZeta.Checked;
  SourcePkg.ObsChoice := TSwiObsChoice(comboObservations.ItemIndex);
  SourcePkg.ToeSlope.Value := rdeToeslope.RealValue;
  SourcePkg.TipSlope.Value := rdeTipSlope.RealValue;
  SourcePkg.Alpha.Value := rdeAlpha.RealValue;
  SourcePkg.Beta.Value := rdeBeta.RealValue;
  SourcePkg.Adaptive := cbAdaptive.Checked;
  SourcePkg.MaxAdaptiveTimeSteps := seMaxAdaptiveSteps.AsInteger;
  SourcePkg.MinAdaptiveTimeSteps := seMinAdaptiveSteps.AsInteger;
  SourcePkg.AdaptiveFactor.Value := rdeAdaptiveFactor.RealValue;

  SourcePkg.Solver := TSwiSolver(comboSolver.ItemIndex);
  SourcePkg.SolverPrintoutInterval := sePrintoutInterval.AsInteger;
  SourcePkg.SolverPrintChoice := TSwiSolverPrintChoice(comboPCGPrint.ItemIndex);
  SourcePkg.MXITER := seMaxIterOuter.AsInteger;
  SourcePkg.ITER1 := seMaxIterInner.AsInteger;
  SourcePkg.NPCOND := TPcgMethod(comboPCGPrecondMeth.ItemIndex);
  SourcePkg.ZCLOSE.Value := rdeMaxZetaChange.RealValue;
  SourcePkg.RCLOSE.Value := rdeMaxRes.RealValue;
  SourcePkg.RELAX.Value := rdeRelax.RealValue;
  SourcePkg.NBPOL := TPcgEstimateMaxEigenvalue(comboEigenValue.ItemIndex);
  SourcePkg.DAMP.Value := rdeDamp.RealValue;
  SourcePkg.DAMPT.Value := rdeDampT.RealValue;
  SourcePkg.ModflowPrecision := TModflowPrecision(comboModflowPrecision.ItemIndex);

  for ItemIndex := 0 to rdgDensity.RowCount - 1 do
  begin
    if SourcePkg.ZoneDimensionlessDensities.Count <= ItemIndex then
    begin
      SourcePkg.ZoneDimensionlessDensities.Add;
    end;
    SourcePkg.ZoneDimensionlessDensities[ItemIndex].Value :=
      StrToFloatDef(rdgDensity.Cells[Ord(dcDensity), ItemIndex+1], 0)
  end;
  while SourcePkg.ZoneDimensionlessDensities.Count + 1 > rdgDensity.RowCount do
  begin
    SourcePkg.ZoneDimensionlessDensities.Last.Free;
  end;
end;

procedure TframePackageSWI.EnableRelax;
begin
  rdeRelax.Enabled := rcSelectionController.Enabled
    and (comboSolver.ItemIndex = 1) and (comboPCGPrecondMeth.ItemIndex = 0);
end;

procedure TframePackageSWI.EnableEigenValue;
begin
  comboEigenValue.Enabled := rcSelectionController.Enabled
    and (comboSolver.ItemIndex = 1) and (comboPCGPrecondMeth.ItemIndex = 1);
end;

procedure TframePackageSWI.EnableSolverControls;
begin
  seMaxIterOuter.Enabled := rcSelectionController.Enabled and (comboSolver.ItemIndex = 1);
  seMaxIterInner.Enabled := seMaxIterOuter.Enabled;
  comboPCGPrecondMeth.Enabled := seMaxIterOuter.Enabled;
  rdeMaxZetaChange.Enabled := seMaxIterOuter.Enabled;
  rdeMaxRes.Enabled := seMaxIterOuter.Enabled;
  rdeDamp.Enabled := seMaxIterOuter.Enabled;
  rdeDampT.Enabled := seMaxIterOuter.Enabled;
  EnableRelax;
  EnableEigenValue;
end;

procedure TframePackageSWI.EnableAdaptiveControls;
begin
  seMaxAdaptiveSteps.Enabled := rcSelectionController.Enabled and cbAdaptive.Checked;
  seMinAdaptiveSteps.Enabled := seMaxAdaptiveSteps.Enabled;
  rdeAdaptiveFactor.Enabled := seMaxAdaptiveSteps.Enabled;
end;

end.
