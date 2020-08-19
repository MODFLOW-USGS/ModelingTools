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
    cbPackageConvergence: TCheckBox;
    procedure cbHeadBasedClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
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

uses ModflowCSubInterbed;

resourcestring
  StrInterbedName = 'Interbed name';
  StrInterbedType = 'Interbed type';

{$R *.dfm}

type
  TInterbedColumns = (icName, icType);
{ TframePackageCsub }

procedure TframePackageCsub.cbHeadBasedClick(Sender: TObject);
begin
  inherited;
  cbPreconsolidationHeadUsed.Enabled := rcSelectionController.Enabled
    and not cbHeadBased.Checked;
  if cbHeadBased.Checked and rcSelectionController.Enabled then
  begin
    cbPreconsolidationHeadUsed.Checked := True;
  end;
end;

procedure TframePackageCsub.GetData(Package: TModflowPackageSelection);
var
  CSubPackage: TCSubPackageSelection;
  CsubOutputType: TCsubOutputType;
  ItemIndex: Integer;
  Interbed: TCSubInterbed;
begin
  inherited;
  pcCsub.ActivePageIndex := 0;
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
  cbPackageConvergence.Checked := CSubPackage.WriteConvergenceData;

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
      frameInterbeds.Grid.Objects[Ord(icName), ItemIndex+1] := Interbed;
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

procedure TframePackageCsub.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  cbHeadBasedClick(Sender);
end;

procedure TframePackageCsub.SetData(Package: TModflowPackageSelection);
var
  CSubPackage: TCSubPackageSelection;
  CsubOutputType: TCsubOutputType;
  ItemIndex: Integer;
  Interbed: TCSubInterbed;
  OutputTypes: TCsubOutputTypes;
  RowIndex: Integer;
  InterbedIndex: Integer;
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
  CSubPackage.WriteConvergenceData := cbPackageConvergence.Checked;

  OutputTypes := [];
  for CsubOutputType := coInterbedStrain to coZDisplacement do
  begin
    if chklstOutput.Checked[Ord(CsubOutputType)] then
    begin
      Include(OutputTypes, CsubOutputType);
    end;
  end;
  CSubPackage.OutputTypes := OutputTypes;

  for InterbedIndex := CSubPackage.Interbeds.Count -1 downto 0 do
  begin
    Interbed := CSubPackage.Interbeds[InterbedIndex];
    if frameInterbeds.Grid.Cols[Ord(icName)].IndexOfObject(Interbed) < 0 then
    begin
      CSubPackage.Interbeds.Delete(InterbedIndex);
    end;
  end;

  ItemIndex := -1;
  for RowIndex := 1 to frameInterbeds.seNumber.AsInteger do
  begin
    if (frameInterbeds.Grid.Cells[Ord(icName), RowIndex] <> '')
      and (frameInterbeds.Grid.ItemIndex[Ord(icType), RowIndex] >= 0)  then
    begin
      Inc(ItemIndex);

      Interbed := frameInterbeds.Grid.Objects[Ord(icName), RowIndex] as TCSubInterbed;
      if Interbed = nil then
      begin
        Interbed := CSubPackage.Interbeds.Add;
        Interbed.Index := ItemIndex;
      end;
      Interbed.Name := frameInterbeds.Grid.Cells[Ord(icName), ItemIndex+1];
      Interbed.InterbedType := TCSubInterbedType(frameInterbeds.Grid.ItemIndex[Ord(icType), RowIndex]);
    end
    else
    begin
      frameInterbeds.Grid.Objects[Ord(icName), RowIndex].Free;
    end;
  end;

end;

end.
