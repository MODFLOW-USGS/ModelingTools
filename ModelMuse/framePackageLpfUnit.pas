unit framePackageLpfUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, RbwController,
  ModflowPackageSelectionUnit, Grids, RbwDataGrid4;

type
  TframePackageLpf = class(TframePackage)
    rdgOptions: TRbwDataGrid4;
    procedure FrameResize(Sender: TObject);
    procedure rdgOptionsVerticalScroll(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageLpf: TframePackageLpf;

resourcestring
  StrSkipCheckingThatA = 'Skip checking that a value is defined for all cell' +
  's when parameters are used to define layer data (NOPARCHECK)';

implementation

resourcestring
  StrInterpretVariableS = 'Interpret variable Ss and SS parameters as storag' +
  'e coefficient rather than specific storage (STORAGECOEFFICIENT)';
  StrUseCellThicknessT = 'Use cell thickness to compute vertical conductance' +
  ' in unconfined cells (CONSTANTCV)';
  StrInDesignatedConfin = 'In designated confined layers; starting heads wil' +
  'l be used to compute cell thickness (THICKSTRT)';
  StrUseVerticalConduct = 'Use vertical conductance correction (inverse of N' +
  'OCVCORRECTION)';
  StrUseVerticalFlowCo = 'Use vertical flow correction under dewatered condi' +
  'tions (inverse of NOVFC)';

{$R *.dfm}

type
  TLpfOptionRows = (lorStorageCoefficient, lorThikStrt, lorConstantCV, 
    lorNoCvCorrection, lorNoVFC, lorNoParCheck);


{ TframePackageLpf }

procedure TframePackageLpf.FrameResize(Sender: TObject);
begin
  inherited;
  rdgOptions.BeginUpdate;
  rdgOptions.ColWidths[0] := rdgOptions.Width-45;
  rdgOptions.EndUpdate;
end;

procedure TframePackageLpf.GetData(Package: TModflowPackageSelection);
var
  LpfPackage: TLpfSelection;
begin
  inherited;
  LpfPackage := Package as TLpfSelection;

  rdgOptions.Checked[0, Ord(lorStorageCoefficient)] :=
    LpfPackage.UseStorageCoefficient;
  rdgOptions.Checked[0, Ord(lorConstantCV)] := LpfPackage.UseConstantCV;
  rdgOptions.Checked[0, Ord(lorThikStrt)] := LpfPackage.UseSaturatedThickness;
  rdgOptions.Checked[0, Ord(lorNoCvCorrection)] := LpfPackage.UseCvCorrection;
  rdgOptions.Checked[0, Ord(lorNoVFC)] := LpfPackage.UseVerticalFlowCorrection;
  rdgOptions.Checked[0, Ord(lorNoParCheck)] := LpfPackage.NoParCheck;

  FrameResize(self);
end;

procedure TframePackageLpf.Loaded;
begin
  inherited;
  rdgOptions.BeginUpdate;
  try
    FrameResize(self);
    rdgOptions.Cells[0, Ord(lorStorageCoefficient)] :=
      StrInterpretVariableS;
    rdgOptions.Cells[0, Ord(lorConstantCV)] :=
      StrUseCellThicknessT;

  // ModelMuse determines the designated layers by those in which LAYTYP < 0
    rdgOptions.Cells[0, Ord(lorThikStrt)] :=
      StrInDesignatedConfin;
    rdgOptions.Cells[0, Ord(lorNoCvCorrection)] :=
      StrUseVerticalConduct;
    rdgOptions.Cells[0, Ord(lorNoVFC)] :=
      StrUseVerticalFlowCo;

    rdgOptions.Cells[0, Ord(lorNoParCheck)] :=
      StrSkipCheckingThatA;
    finally
    rdgOptions.EndUpdate;
  end;

end;

procedure TframePackageLpf.rdgOptionsVerticalScroll(Sender: TObject);
begin
  inherited;
  // this ensures that the cells are redrawn properly.
  rdgOptions.EditorMode := False;
end;

procedure TframePackageLpf.SetData(Package: TModflowPackageSelection);
var
  LpfPackage: TLpfSelection;
begin
  inherited;
  LpfPackage := Package as TLpfSelection;

  LpfPackage.UseStorageCoefficient := rdgOptions.Checked[0, Ord(lorStorageCoefficient)];
  LpfPackage.UseConstantCV := rdgOptions.Checked[0, Ord(lorConstantCV)];
  LpfPackage.UseSaturatedThickness := rdgOptions.Checked[0, Ord(lorThikStrt)];
  LpfPackage.UseCvCorrection := rdgOptions.Checked[0, Ord(lorNoCvCorrection)];
  LpfPackage.UseVerticalFlowCorrection := rdgOptions.Checked[0, Ord(lorNoVFC)];
  LpfPackage.NoParCheck := rdgOptions.Checked[0, Ord(lorNoParCheck)];
end;

end.
