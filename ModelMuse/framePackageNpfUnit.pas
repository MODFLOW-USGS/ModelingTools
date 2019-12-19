unit framePackageNpfUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, Vcl.Grids, RbwDataGrid4,
  ModflowPackageSelectionUnit;

type
  TNprOptions = (noUseXT3D, noXt3dOnRightHandSide, noThickStrt, noVaryingVerticalConductance, noDewatered,
    noPerched, {noNewton, noDampening,} 
    noSaveSpecificDischarge, noHorizontalAnis, noVerticalAnis);

  TframePackageNpf = class(TframePackage)
    rdgOptions: TRbwDataGrid4;
    lblInterblockMethod: TLabel;
    comboInterblockMethod: TJvImageComboBox;
    procedure FrameResize(Sender: TObject);
    procedure rdgOptionsVerticalScroll(Sender: TObject);
    procedure rdgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    procedure Enable_comboInterblockMethod;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageNpf: TframePackageNpf;

implementation

uses frmGoPhastUnit, ModflowOptionsUnit;

resourcestring
  StrCellsHavingANegat = 'Cells having a negative CellType are confined. How' +
  'ever, their cell thickness for conductance calculations will be based on ' +
  'the initial head minus the cell bottom rather than the cell height (THICKSTRT)';
  StrVerticalConductance = 'Vertical conductance will be recalculated each t' +
  'ime step based on saturated thickness (VARIABLECV).';
  StrDewatered = 'When a cell is less than fully saturated, the vertical con' +
  'ductance of the overlying cell will depend only on the properties of the ' +
  'overlying cell (DEWATERED).';
  StrPerched = 'When a cell is less than fully saturated, the vertical head ' +
  'gradient between it and the overlying cell will be the difference between' +
  ' the head in the overlying cell and the bottom of the overlying cell (PER' +
  'CHED)';
  StrActivateTheNewton = 'Activate the Newton-Raphson formulation for ground' +
  'water flow between connected, convertible groundwater cells (NEWTON).';
  StrGroundwaterHeadIn = 'Groundwater head in a cell will be dampened when w' +
  'ater levels fall below the bottom of a cell (NEWTON HEAD DAMPENING).';
  StrXT3DOption = 'XT3D Option: the XT3D option is used to specify anisotropy' +
  ' in three dimensions and to help calculate ' +
  'fluxes between cells more accuratedly when cell centers are not aligned.';
  StrXT3DRHSOption = 'XT3D RHS Option: Some terms of the XT3D option are ' +
  'included on the Right Hand Side of the matrix equation';
  StrSaveXYAndZCom = 'Save X, Y, and Z components of specific discharge. (SA' +
  'VE_SPECIFIC_DISCHARGE)';
  StrUseHorizontalAniso = 'Use horizontal anisotropy (K22OVERK)';
  StrUseVerticalAnisotr = 'Use vertical anisotropy (K33OVERK)';

{$R *.dfm}

procedure TframePackageNpf.Enable_comboInterblockMethod;
begin
  comboInterblockMethod.Enabled := rcSelectionController.Enabled
    and not rdgOptions.Checked[0, Ord(noUseXT3D)];
end;

procedure TframePackageNpf.FrameResize(Sender: TObject);
begin
  inherited;
  rdgOptions.BeginUpdate;
  rdgOptions.ColWidths[0] := rdgOptions.Width-4;
  rdgOptions.EndUpdate;
end;

procedure TframePackageNpf.GetData(Package: TModflowPackageSelection);
var
  NpfPackage: TNpfPackage;
  GridSelection: TGridRect;
begin
  inherited;

  GridSelection.Left := 0;
  GridSelection.Right := 0;
  GridSelection.Top := 0;
  GridSelection.Bottom := 0;
  rdgOptions.Selection := GridSelection;

  NpfPackage := Package as TNpfPackage;
  comboInterblockMethod.ItemIndex := Ord(NpfPackage.CellAveraging);
  rdgOptions.Checked[0, Ord(noThickStrt)] := NpfPackage.UseSaturatedThickness;
  rdgOptions.Checked[0, Ord(noVaryingVerticalConductance)] := NpfPackage.TimeVaryingVerticalConductance;
  rdgOptions.Checked[0, Ord(noDewatered)] := NpfPackage.Dewatered;
  rdgOptions.Checked[0, Ord(noPerched)] := NpfPackage.Perched;
//  rdgOptions.Checked[0, Ord(noNewton)] := NpfPackage.UseNewtonRaphson;
//  rdgOptions.Checked[0, Ord(noDampening)] := NpfPackage.ApplyHeadDampening;
  rdgOptions.Checked[0, Ord(noUseXT3D)] := NpfPackage.UseXT3D;
  rdgOptions.Checked[0, Ord(noXt3dOnRightHandSide)] := NpfPackage.Xt3dOnRightHandSide;
  rdgOptions.Checked[0, Ord(noSaveSpecificDischarge)] := NpfPackage.SaveSpecificDischarge;
  rdgOptions.Checked[0, Ord(noHorizontalAnis)] := NpfPackage.UseHorizontalAnisotropy;
  rdgOptions.Checked[0, Ord(noVerticalAnis)] := NpfPackage.UseVerticalAnisotropy;
end;

procedure TframePackageNpf.Loaded;
var
  GridSelection: TGridRect;
begin
  inherited;
  rdgOptions.BeginUpdate;
  try
    FrameResize(self);
    rdgOptions.Cells[0, Ord(noThickStrt)] := StrCellsHavingANegat;
    rdgOptions.Cells[0, Ord(noVaryingVerticalConductance)] := StrVerticalConductance;
    rdgOptions.Cells[0, Ord(noDewatered)] := StrDewatered;
    rdgOptions.Cells[0, Ord(noPerched)] := StrPerched;
//    rdgOptions.Cells[0, Ord(noNewton)] := StrActivateTheNewton;
//    rdgOptions.Cells[0, Ord(noDampening)] := StrGroundwaterHeadIn;
    rdgOptions.Cells[0, Ord(noUseXT3D)] := StrXT3DOption;
    rdgOptions.Cells[0, Ord(noXt3dOnRightHandSide)] := StrXT3DRHSOption;
    rdgOptions.Cells[0, Ord(noSaveSpecificDischarge)] := StrSaveXYAndZCom;
    rdgOptions.Cells[0, Ord(noHorizontalAnis)] := StrUseHorizontalAniso;
    rdgOptions.Cells[0, Ord(noVerticalAnis)] := StrUseVerticalAnisotr;

  finally
    rdgOptions.EndUpdate;

  end;

  GridSelection.Left := -1;
  GridSelection.Right := -1;
  GridSelection.Top := -1;
  GridSelection.Bottom := -1;
  rdgOptions.Selection := GridSelection;
end;

procedure TframePackageNpf.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  Enable_comboInterblockMethod;
end;

procedure TframePackageNpf.rdgOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ModflowOptions: TModflowOptions;
begin
  inherited;
  ModflowOptions := frmGoPhast.PhastModel.ModflowOptions;
  if (ACol >= 0)  then
  begin
    if ARow = Ord(noDewatered) then
    begin
      if (not rdgOptions.Checked[0, Ord(noVaryingVerticalConductance)])
        or rdgOptions.Checked[0, Ord(noUseXT3D)]
        or ModflowOptions.NewtonMF6 then
      begin
        CanSelect := False;
      end;
    end
    else  if ARow = Ord(noXt3dOnRightHandSide) then
    begin
      if not rdgOptions.Checked[0, Ord(noUseXT3D)] then
      begin
        CanSelect := False;
      end;
    end
    else  if ARow = Ord(noThickStrt) then
    begin
      if rdgOptions.Checked[0, Ord(noUseXT3D)] then
      begin
        CanSelect := False;
      end;
    end
    else  if ARow = Ord(noPerched) then
    begin
      if rdgOptions.Checked[0, Ord(noUseXT3D)] or ModflowOptions.NewtonMF6 then
      begin
        CanSelect := False;
      end;
    end
    else  if ARow = Ord(noVaryingVerticalConductance) then
    begin
      if rdgOptions.Checked[0, Ord(noUseXT3D)] or ModflowOptions.NewtonMF6 then
      begin
        CanSelect := False;
      end;
    end;
  end;
end;

procedure TframePackageNpf.rdgOptionsVerticalScroll(Sender: TObject);
begin
  inherited;
  // this ensures that the cells are redrawn properly.
  rdgOptions.EditorMode := False;
end;

procedure TframePackageNpf.SetData(Package: TModflowPackageSelection);
var
  NpfPackage: TNpfPackage;
begin
  inherited;
  NpfPackage := Package as TNpfPackage;
  NpfPackage.CellAveraging := TCellAveraging(comboInterblockMethod.ItemIndex);
  NpfPackage.UseSaturatedThickness := rdgOptions.Checked[0, Ord(noThickStrt)];
  NpfPackage.TimeVaryingVerticalConductance := rdgOptions.Checked[0, Ord(noVaryingVerticalConductance)];
  NpfPackage.Dewatered := rdgOptions.Checked[0, Ord(noDewatered)];
  NpfPackage.Perched := rdgOptions.Checked[0, Ord(noPerched)];
//  NpfPackage.UseNewtonRaphson := rdgOptions.Checked[0, Ord(noNewton)];
//  NpfPackage.ApplyHeadDampening := rdgOptions.Checked[0, Ord(noDampening)];
  NpfPackage.UseXT3D := rdgOptions.Checked[0, Ord(noUseXT3D)];
  NpfPackage.Xt3dOnRightHandSide := rdgOptions.Checked[0, Ord(noXt3dOnRightHandSide)];
  NpfPackage.SaveSpecificDischarge := rdgOptions.Checked[0, Ord(noSaveSpecificDischarge)];
  NpfPackage.UseHorizontalAnisotropy := rdgOptions.Checked[0, Ord(noHorizontalAnis)];
  NpfPackage.UseVerticalAnisotropy := rdgOptions.Checked[0, Ord(noVerticalAnis)];
end;

end.
