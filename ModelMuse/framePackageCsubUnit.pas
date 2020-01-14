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
    comboCompressionMethod: TJvImageComboBox;
    lblCompressionMethod: TLabel;
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
  CsubOutputType: TCsubOutputType;
  ItemIndex: Integer;
  Interbed: TInterbed;
begin
  inherited;
  CSubPackage := Package as TCSubPackageSelection;
  rdeGamma.RealValue := CSubPackage.Gamma;
  rdeBeta.RealValue := CSubPackage.Beta;
  cbHeadBased.Checked := CSubPackage.HeadBased;
  cbPreconsolidationHeadUsed.Checked := CSubPackage.PreconsolidationHeadUsed;
  seNDelayCells.asInteger := CSubPackage.NumberOfDelayCells;
  comboCompressionMethod.ItemIndex := Ord(CSubPackage.CompressionMethod);
  cbUpdateMaterialProperties.Checked := CSubPackage.UpdateMaterialProperties;
  comboInterbedThicknessMethod.ItemIndex := Ord(CSubPackage.InterbedThicknessMethod);
  cbSpecifyInitialPreconsolidationStress.Checked := CSubPackage.SpecifyInitialPreconsolidationStress;
  cbSpecifyInitialDelayHead.Checked := CSubPackage.SpecifyInitialDelayHead;
  cbEffectiveStressLag.Checked := CSubPackage.EffectiveStressLag;

  for CsubOutputType := coInterbedStrain to coZDisplacement do
  begin
    chklstOutput.Checked[Ord(CsubOutputType)] := CsubOutputType in CSubPackage.OutputTypes;
  end;

  frameInterbeds.Grid.BeginUpdate;
  try
    frameInterbeds.SeNumber.AsInteger := CSubPackage.Interbeds.Count;
    for ItemIndex := 0 to CSubPackage.Interbeds.Count - 1 do
    begin
      Interbed := CSubPackage.Interbeds[ItemIndex];
      frameInterbeds.Grid.Cells[Ord(icName), ItemIndex+1] := Interbed.Name;
      frameInterbeds.Grid.ItemIndex[Ord(icType), ItemIndex+1] := Ord(Interbed.InterbedType);
    end;
  finally
    frameInterbeds.Grid.EndUpdate;
  end;
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
  CsubOutputType: TCsubOutputType;
  ItemIndex: Integer;
  Interbed: TInterbed;
  OutputTypes: TCsubOutputTypes;
  CSubCount: Integer;
  RowIndex: Integer;
begin
  inherited;
  CSubPackage := Package as TCSubPackageSelection;
  if rdeGamma.Text <> '' then
  begin
    CSubPackage.Gamma := rdeGamma.RealValue;
  end;
  if rdeBeta.Text <> '' then
  begin
    CSubPackage.Beta := rdeBeta.RealValue;
  end;
  CSubPackage.HeadBased := cbHeadBased.Checked;
  CSubPackage.PreconsolidationHeadUsed := cbPreconsolidationHeadUsed.Checked;
  CSubPackage.NumberOfDelayCells := seNDelayCells.asInteger;
  CSubPackage.CompressionMethod := TCompressionMethod(comboCompressionMethod.ItemIndex);
  CSubPackage.UpdateMaterialProperties := cbUpdateMaterialProperties.Checked;
  CSubPackage.InterbedThicknessMethod := TInterbedThicknessMethod(comboInterbedThicknessMethod.ItemIndex);
  CSubPackage.SpecifyInitialPreconsolidationStress := cbSpecifyInitialPreconsolidationStress.Checked;
  CSubPackage.SpecifyInitialDelayHead := cbSpecifyInitialDelayHead.Checked;
  CSubPackage.EffectiveStressLag := cbEffectiveStressLag.Checked;

  OutputTypes := [];
  for CsubOutputType := coInterbedStrain to coZDisplacement do
  begin
    if chklstOutput.Checked[Ord(CsubOutputType)] then
    begin
      Include(OutputTypes, CsubOutputType);
    end;
  end;
  CSubPackage.OutputTypes := OutputTypes;

  CSubCount := 0;
  for ItemIndex := 0 to frameInterbeds.SeNumber.AsInteger - 1 do
  begin
    if (frameInterbeds.Grid.Cells[Ord(icName), ItemIndex+1] <> '')
      and (frameInterbeds.Grid.ItemIndex[Ord(icType), ItemIndex+1] >= 0)  then
    begin
      Inc(CSubCount);
    end;
  end;

  ItemIndex := -1;
  CSubPackage.Interbeds.Count := CSubCount;
  for RowIndex := 1 to frameInterbeds.SeNumber.AsInteger do
  begin
    if (frameInterbeds.Grid.Cells[Ord(icName), RowIndex] <> '')
      and (frameInterbeds.Grid.ItemIndex[Ord(icType), RowIndex] >= 0)  then
    begin
      Inc(ItemIndex);

      Interbed := CSubPackage.Interbeds[ItemIndex];
      Interbed.Name := frameInterbeds.Grid.Cells[Ord(icName), ItemIndex+1];
      if frameInterbeds.Grid.ItemIndex[Ord(icType), ItemIndex+1] >= 0 then
      begin
        Interbed.InterbedType := TInterbedType(frameInterbeds.Grid.ItemIndex[Ord(icType), ItemIndex+1]);
      end;
    end;
  end;

end;

end.
