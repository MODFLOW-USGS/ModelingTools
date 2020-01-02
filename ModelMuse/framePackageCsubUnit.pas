unit framePackageCsubUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Mask, JvExMask, JvSpin, JvExStdCtrls,
  JvCombobox, JvListComb, Vcl.ComCtrls, frameGridUnit, Vcl.CheckLst,
  ModflowPackageSelectionUnit;

type
  TframePackageCsub = class(TframePackage)
    pcCsub: TPageControl;
    tabOptions: TTabSheet;
    rdeGamma: TRbwDataEntry;
    lblGamma: TLabel;
    lblBeta: TLabel;
    rdeBeta: TRbwDataEntry;
    cbHeadBased: TCheckBox;
    cbPreconsolidationHeadUsed: TCheckBox;
    seNDelayCells: TJvSpinEdit;
    cbUseCompressionIndicies: TCheckBox;
    cbUpdateMaterialProperties: TCheckBox;
    comboInterbedThicknessMethod: TJvImageComboBox;
    lblInterbedThicknessMethod: TLabel;
    cbEffectiveStressLag: TCheckBox;
    cbSpecifyInitialDelayHead: TCheckBox;
    cbSpecifyInitialPreconsolidationStress: TCheckBox;
    tabOutputTypes: TTabSheet;
    chklstOutput: TCheckListBox;
    tabInterbeds: TTabSheet;
    frameInterbeds: TframeGrid;
    lblseNDelayCells: TLabel;
  private
    procedure InitializeGrid;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageCsub: TframePackageCsub;

implementation

resourcestring
  StrInterbedName = 'Interbed name';
  StrInterbedType = 'Interbed type';

{$R *.dfm}

type
  TInterbedColumns = (icName, icType);
{ TframePackageCsub }

procedure TframePackageCsub.GetData(Package: TModflowPackageSelection);
var
  CSubPackage: TCSubPackageSelection;
begin
  inherited;
  CSubPackage := Package as TCSubPackageSelection;
  rdeGamma.RealValue := CSubPackage.Gamma;
  rdeBeta.RealValue := CSubPackage.Beta;
  cbHeadBased.Checked := CSubPackage.HeadBased;
  cbPreconsolidationHeadUsed.Checked := CSubPackage.PreconsolidationHeadUsed;
  seNDelayCells.asInteger := CSubPackage.NumberOfDelayCells;
end;

procedure TframePackageCsub.InitializeGrid;
begin
  frameInterbeds.Grid.Cells[Ord(icName), 0] := StrInterbedName;
  frameInterbeds.Grid.Cells[Ord(icType), 0] := StrInterbedType;
end;

procedure TframePackageCsub.Loaded;
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageCsub.SetData(Package: TModflowPackageSelection);
var
  CSubPackage: TCSubPackageSelection;
begin
  inherited;
  CSubPackage := Package as TCSubPackageSelection;

end;

end.
