unit framePcgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, ArgusDataEntry, JvExStdCtrls,
  ModflowPackageSelectionUnit, JvCombobox, JvListComb, RbwController;

type
  TframePCG = class(TframePackage)
    lblPCGMaxOuter: TLabel;
    rdePCGMaxOuter: TRbwDataEntry;
    lblPCGMaxInner: TLabel;
    rdePCGMaxInner: TRbwDataEntry;
    lblPCGMethod: TLabel;
    comboPCGPrecondMeth: TJvImageComboBox;
    lblPCGMaxChangeHead: TLabel;
    rdePCGMaxHeadChange: TRbwDataEntry;
    lblPCGMaxResidual: TLabel;
    rdePCGMaxResChange: TRbwDataEntry;
    lblPCGRelaxation: TLabel;
    rdePCGRelax: TRbwDataEntry;
    lblPCGMaxEigen: TLabel;
    comboPCGEigenValue: TJvImageComboBox;
    lblPCGPrintInterval: TLabel;
    rdePCGPrintInt: TRbwDataEntry;
    lblPCGPrintControl: TLabel;
    comboPCGPrint: TJvImageComboBox;
    lblPCGDampingFactor: TLabel;
    rdePCGDamp: TRbwDataEntry;
    lblPCGDampPcgT: TLabel;
    rdePCGDampPcgT: TRbwDataEntry;
    gbIHCOFADD: TGroupBox;
    rbIHCOFADD_0: TRadioButton;
    rbIHCOFADD_1: TRadioButton;
    procedure comboPCGPrecondMethChange(Sender: TObject);
  private
    procedure EnableNpbol;
    procedure EnableRelax;
    { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  framePCG: TframePCG;

implementation

{$R *.dfm}

{ TframePCG }

procedure TframePCG.comboPCGPrecondMethChange(Sender: TObject);
begin
  inherited;
  EnableNpbol;
  EnableRelax;
end;

procedure TframePCG.EnableNpbol;
begin
  comboPCGEigenValue.Enabled := Selected
    and (comboPCGPrecondMeth.ItemIndex = 1)
end;

procedure TframePCG.EnableRelax;
begin
  rdePCGRelax.Enabled := Selected
    and (comboPCGPrecondMeth.ItemIndex = 0)
end;

procedure TframePCG.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TPcgSelection;
begin
  inherited GetData(Package);
  SourcePkg := Package as TPcgSelection;
  rdePCGMaxOuter.Text := IntToStr(SourcePkg.MXITER);
  rdePCGMaxInner.Text := IntToStr(SourcePkg.ITER1);
  comboPCGPrecondMeth.ItemIndex :=  Ord(SourcePkg.NPCOND);
  rdePCGMaxHeadChange.Text := FloatToStr(SourcePkg.HCLOSE.Value);
  rdePCGMaxResChange.Text := FloatToStr(SourcePkg.RCLOSE.Value);
  rdePCGRelax.Text := FloatToStr(SourcePkg.RELAX.Value);
  comboPCGEigenValue.ItemIndex := Ord(SourcePkg.NBPOL);
  rdePCGPrintInt.Text := IntToStr(SourcePkg.IPRPCG);
  comboPCGPrint.ItemIndex := Ord(SourcePkg.MUTPCG);
  rdePCGDamp.Text := FloatToStr(SourcePkg.DAMPPCG.Value);
  rdePCGDampPcgT.Text := FloatToStr(SourcePkg.DAMPPCGT.Value);
  case SourcePkg.IHCOFADD of
    dcoConvertWhenSurrounded: rbIHCOFADD_0.Checked := True;
    dcoConvertWhenNoFlow: rbIHCOFADD_1.Checked := True;
    else Assert(False);
  end;
  EnableNpbol;
  EnableRelax;
end;

procedure TframePCG.SetData(Package: TModflowPackageSelection);
var
  IntValue: integer;
  FloatValue: double;
  SourcePkg: TPcgSelection;
begin
  inherited SetData(Package);
  SourcePkg := Package as TPcgSelection;
  if TryStrToInt(rdePCGMaxOuter.Text, IntValue) then
  begin
    SourcePkg.MXITER := IntValue;
  end;
  if TryStrToInt(rdePCGMaxInner.Text, IntValue) then
  begin
    SourcePkg.ITER1 := IntValue;
  end;
  SourcePkg.NPCOND := TPcgMethod(comboPCGPrecondMeth.ItemIndex);
  if TryStrToFloat(rdePCGMaxHeadChange.Text, FloatValue) then
  begin
     SourcePkg.HCLOSE.Value := FloatValue;
  end;
  if TryStrToFloat(rdePCGMaxResChange.Text, FloatValue) then
  begin
     SourcePkg.RCLOSE.Value := FloatValue;
  end;
  if TryStrToFloat(rdePCGRelax.Text, FloatValue) then
  begin
     SourcePkg.RELAX.Value := FloatValue;
  end;
  SourcePkg.NBPOL := TPcgEstimateMaxEigenvalue(comboPCGEigenValue.ItemIndex);
  if TryStrToInt(rdePCGPrintInt.Text, IntValue) then
  begin
    SourcePkg.IPRPCG := IntValue;
  end;
  SourcePkg.MUTPCG := TPcgPrintSelection(comboPCGPrint.ItemIndex);
  if TryStrToFloat(rdePCGDamp.Text, FloatValue) then
  begin
     SourcePkg.DAMPPCG.Value := FloatValue;
  end;
  if TryStrToFloat(rdePCGDampPcgT.Text, FloatValue) then
  begin
     SourcePkg.DAMPPCGT.Value := FloatValue;
  end;
  if rbIHCOFADD_0.Checked then
  begin
    SourcePkg.IHCOFADD := dcoConvertWhenSurrounded;
    Assert(not rbIHCOFADD_1.Checked);
  end
  else
  begin
    SourcePkg.IHCOFADD := dcoConvertWhenNoFlow;
    Assert(rbIHCOFADD_1.Checked);
  end;
end;

procedure TframePCG.SetSelected(const Value: boolean);
begin
  inherited;
  EnableNpbol;
  EnableRelax;
end;

end.
