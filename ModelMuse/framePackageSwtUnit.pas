unit framePackageSwtUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ExtCtrls, ComCtrls, Grids,
  RbwDataGrid4, JvExStdCtrls, JvCombobox, JvListComb, Buttons, Mask, JvExMask,
  JvSpin, ModflowPackageSelectionUnit, GrayTabs;

type
  TframePackageSwt = class(TframePackage)
    pcSWT: TPageControl;
    tabControls: TTabSheet;
    gbIthk: TGroupBox;
    rgIthkConstant: TRadioButton;
    rbIthkVariable: TRadioButton;
    lblIvoid: TLabel;
    lblIstpcs: TLabel;
    lblIcrcc: TLabel;
    tabPrintSave: TTabSheet;
    rdgInitialPrintChoices: TRbwDataGrid4;
    lblOutputChoice: TLabel;
    comboOutputChoice: TJvImageComboBox;
    rdgOutput: TRbwDataGrid4;
    seNumExportPeriods: TJvSpinEdit;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    lblNumExportPeriods: TLabel;
    comboMultiFomat: TJvImageComboBox;
    cbMultiPrintSave: TCheckBox;
    comboIvoid: TJvImageComboBox;
    comboIstpcs: TJvImageComboBox;
    comboIcrcc: TJvImageComboBox;
    procedure seNumExportPeriodsChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure comboMultiFomatChange(Sender: TObject);
    procedure cbMultiPrintSaveClick(Sender: TObject);
    procedure rdgOutputColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgOutputEndUpdate(Sender: TObject);
    procedure rdgOutputHorizontalScroll(Sender: TObject);
    procedure rdgOutputMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgOutputSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FrameResize(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    FChangingRowCount: Boolean;
    procedure EnableDeleteButton;
    procedure LayoutPrintSaveControls;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

  TInitialPrintRows = (iprLabel, iprLayerCenterElev, iprGeostaticStress,
    iprEffectiveStress, iprPreconsolidationStress, iprStorage);
  TInitialPrintColumn = (ipcLabel, ipcPrint, ipcFormat);

  TSwtOuputColumns = (swocStartTime, swocEndTime,
    swocPrintSubsidence, swocSaveSubsidence,
    swocPrintCompactionByLayer, swocSaveCompactionByLayer,
    swocPrintCompactionBySystem, swocSaveCompactionBySystem,
    swocPrintVerticalDisplacement, swocSaveVerticalDisplacement,
    swocPrintPreconsolidationStress, swocSavePreconsolidationStress,
    swocPrintDeltaPreconsolidationStress, swocSaveDeltaPreconsolidationStress,
    swocPrintGeostaticStress, swocSaveGeostaticStress,
    swocPrintDeltaGeostaticStress, swocSaveDeltaGeostaticStress,
    swocPrintEffectiveStress, swocSaveEffectiveStress,
    swocPrintDeltaEffectiveStress, swocSaveDeltaEffectiveStress,
    swocPrintVoidRatio, swocSaveVoidRatio,
    swocPrintCompressibleBedThickness, swocSaveCompressibleBedThickness,
    swocPrintLayerCenterElevation, swocSaveLayerCenterElevation);

var
  framePackageSwt: TframePackageSwt;

implementation

uses
  Math, frmCustomGoPhastUnit, frmGoPhastUnit, ModflowTimeUnit, GoPhastTypes;

resourcestring
  StrPrint = 'Print';
  StrFormat = 'Format';
  StrLayercenterElevati = 'Layer-center elevation (IZCFL IZCFM)';
  StrGeostaticStressIG = 'Geostatic stress (IGLFL IGLFM)';
  StrEffectiveStressIE = 'Effective stress (IESTFL IESTFM)';
  StrPreconsolidationStr = 'Preconsolidation stress (IPCSFL IPCSFM)';
  StrInitialEquivalentS = 'Initial equivalent storage properties (ISTFL ISTF' +
  'M)';
  StrPrintSubsidenceIf = 'Print subsidence (Ifm1, Ifl1)';
  StrSaveSubsidenceIfl = 'Save subsidence (Ifl2)';
  StrPrintCompactionBy = 'Print compaction by model layer (Ifm2, Ifl3)';
  StrSaveCompactionByM = 'Save compaction by model layer (Ifl4)';
  StrPrintCompactionBySystem = 'Print compaction by interbed system (Ifm3, I' +
  'fl5)';
  StrSaveCompactionByISystem = 'Save compaction by interbed system (Ifl6)';
  StrPrintVerticalDispl = 'Print vertical displacement (Ifm4, Ifl7)';
  StrSaveVerticalDispla = 'Save vertical displacement (Ifl8)';
  StrPrintPreconsolidati = 'Print preconsolidation stress (Ifm5, Ifl9)';
  StrSavePreconsolidatio = 'Save preconsolidation stress (Ifl10)';
  StrPrintChangeInPrec = 'Print change in preconsolidation stress (Ifm6, Ifl' +
  '11)';
  StrSaveChangeInPreco = 'Save change in preconsolidation stress (Ifl12)';
  StrPrintGeostaticStre = 'Print geostatic stress (Ifm7, Ifl13)';
  StrSaveGeostaticStres = 'Save geostatic stress (Ifl14)';
  StrPrintChangeInGeos = 'Print change in geostatic stress (Ifm8, Ifl15)';
  StrSaveChangeInGeost = 'Save change in geostatic stress (Ifl16)';
  StrPrintEffectiveStre = 'Print effective stress (Ifm9, Ifl17)';
  StrSaveEffectiveStres = 'Save effective stress (Ifl18)';
  StrPrintChangeInEffe = 'Print change in effective stress (Ifm10, Ifl19)';
  StrSaveChangeInEffec = 'Save change in effective stress (Ifl20)';
  StrPrintVoidRatioIf = 'Print void ratio (Ifm11, Ifl21)';
  StrSaveVoidRatioIfl = 'Save void ratio (Ifl22)';
  StrPrintThicknessOfC = 'Print thickness of compressible sediments (Ifm12, ' +
  'Ifl23)';
  StrSaveThicknessOfCo = 'Save thickness of compressible sediments (Ifl24)';
  StrPrintLayercenterE = 'Print layer-center elevation (Ifm13, Ifl25)';
  StrSaveLayercenterEl = 'Save layer-center elevation (Ifl26)';

{$R *.dfm}

const
  FormatRow = 1;
  FirstStateRow = FormatRow+1;

{ TframePackageSwt }

procedure TframePackageSwt.Loaded;
var
  ColIndex: Integer;
begin
  inherited;

  seNumExportPeriods.Top := tabPrintSave.Height
    - seNumExportPeriods.Height -8;
  lblNumExportPeriods.Top := seNumExportPeriods.Top + 3;
  sbAdd.Top := tabPrintSave.Height
    - sbAdd.Height -8;
  sbInsert.Top := sbAdd.Top;
  sbDelete.Top := sbAdd.Top;

  rdgOutput.Height := seNumExportPeriods.Top - rdgOutput.Top - 8;


  rdgInitialPrintChoices.BeginUpdate;
  try
    rdgInitialPrintChoices.FixedCols := 1;

    rdgInitialPrintChoices.Cells[Ord(ipcPrint), Ord(iprLabel)] := StrPrint;
    rdgInitialPrintChoices.Cells[Ord(ipcFormat), Ord(iprLabel)] := StrFormat;

    rdgInitialPrintChoices.Cells[Ord(iprLabel), Ord(iprLayerCenterElev)] := StrLayercenterElevati;
    rdgInitialPrintChoices.Cells[Ord(iprLabel), Ord(iprGeostaticStress)] := StrGeostaticStressIG;
    rdgInitialPrintChoices.Cells[Ord(iprLabel), Ord(iprEffectiveStress)] := StrEffectiveStressIE;
    rdgInitialPrintChoices.Cells[Ord(iprLabel), Ord(iprPreconsolidationStress)] := StrPreconsolidationStr;
    rdgInitialPrintChoices.Cells[Ord(iprLabel), Ord(iprStorage)] := StrInitialEquivalentS;
  finally
    rdgInitialPrintChoices.EndUpdate;
  end;

  rdgOutput.BeginUpdate;
  try
    rdgOutput.Cells[Ord(swocStartTime), 0] := StrStartingTime;
    rdgOutput.Cells[Ord(swocEndTime), 0] := StrEndingTime;
    rdgOutput.Cells[Ord(swocPrintSubsidence), 0] := StrPrintSubsidenceIf;
    rdgOutput.Cells[Ord(swocSaveSubsidence), 0] := StrSaveSubsidenceIfl;
    rdgOutput.Cells[Ord(swocPrintCompactionByLayer), 0] := StrPrintCompactionBy;
    rdgOutput.Cells[Ord(swocSaveCompactionByLayer), 0] := StrSaveCompactionByM;
    rdgOutput.Cells[Ord(swocPrintCompactionBySystem), 0] := StrPrintCompactionBySystem;
    rdgOutput.Cells[Ord(swocSaveCompactionBySystem), 0] := StrSaveCompactionByISystem;
    rdgOutput.Cells[Ord(swocPrintVerticalDisplacement), 0] := StrPrintVerticalDispl;
    rdgOutput.Cells[Ord(swocSaveVerticalDisplacement), 0] := StrSaveVerticalDispla;
    rdgOutput.Cells[Ord(swocPrintPreconsolidationStress), 0] := StrPrintPreconsolidati;
    rdgOutput.Cells[Ord(swocSavePreconsolidationStress), 0] := StrSavePreconsolidatio;
    rdgOutput.Cells[Ord(swocPrintDeltaPreconsolidationStress), 0] := StrPrintChangeInPrec;
    rdgOutput.Cells[Ord(swocSaveDeltaPreconsolidationStress), 0] := StrSaveChangeInPreco;
    rdgOutput.Cells[Ord(swocPrintGeostaticStress), 0] := StrPrintGeostaticStre;
    rdgOutput.Cells[Ord(swocSaveGeostaticStress), 0] := StrSaveGeostaticStres;
    rdgOutput.Cells[Ord(swocPrintDeltaGeostaticStress), 0] := StrPrintChangeInGeos;
    rdgOutput.Cells[Ord(swocSaveDeltaGeostaticStress), 0] := StrSaveChangeInGeost;
    rdgOutput.Cells[Ord(swocPrintEffectiveStress), 0] := StrPrintEffectiveStre;
    rdgOutput.Cells[Ord(swocSaveEffectiveStress), 0] := StrSaveEffectiveStres;
    rdgOutput.Cells[Ord(swocPrintDeltaEffectiveStress), 0] := StrPrintChangeInEffe;
    rdgOutput.Cells[Ord(swocSaveDeltaEffectiveStress), 0] := StrSaveChangeInEffec;
    rdgOutput.Cells[Ord(swocPrintVoidRatio), 0] := StrPrintVoidRatioIf;
    rdgOutput.Cells[Ord(swocSaveVoidRatio), 0] := StrSaveVoidRatioIfl;
    rdgOutput.Cells[Ord(swocPrintCompressibleBedThickness), 0] := StrPrintThicknessOfC;
    rdgOutput.Cells[Ord(swocSaveCompressibleBedThickness), 0] := StrSaveThicknessOfCo;
    rdgOutput.Cells[Ord(swocPrintLayerCenterElevation), 0] := StrPrintLayercenterE;
    rdgOutput.Cells[Ord(swocSaveLayerCenterElevation), 0] := StrSaveLayercenterEl;

    rdgOutput.Cells[Ord(swocStartTime), FormatRow] := StrFormat;


    for ColIndex := 0 to rdgOutput.ColCount - 1 do
    begin
      rdgOutput.SpecialFormat[ColIndex,FormatRow] := rcf4String;
      rdgOutput.UseSpecialFormat[ColIndex,FormatRow] := True;
    end;
    for ColIndex := Ord(swocPrintSubsidence) to rdgOutput.ColCount - 1 do
    begin
      rdgOutput.Columns[ColIndex].ComboUsed := True;
      rdgOutput.Columns[ColIndex].PickList :=
        rdgOutput.Columns[Ord(swocPrintSubsidence)].PickList;
    end;
  finally
    rdgOutput.EndUpdate;
  end;
end;

procedure TframePackageSwt.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableDeleteButton;
end;

procedure TframePackageSwt.rdgOutputColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSwt.rdgOutputEndUpdate(Sender: TObject);
var
  NumRows: Integer;
begin
  inherited;
  if (not FChangingRowCount) and (seNumExportPeriods <> nil) then
  begin
    NumRows := rdgOutput.RowCount - FirstStateRow;
    if (NumRows = FirstStateRow-1)
      and (rdgOutput.Cells[Ord(swocStartTime),FirstStateRow] = '')
      and (rdgOutput.Cells[Ord(swocEndTime),FirstStateRow] = '') then
    begin
      NumRows := 0;
    end;
    seNumExportPeriods.AsInteger := NumRows;
  end;
end;

procedure TframePackageSwt.rdgOutputHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSwt.rdgOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(comboMultiFomat, rdgOutput);
  EnableMultiEditControl(cbMultiPrintSave, rdgOutput);
end;

procedure TframePackageSwt.rdgOutputSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ColType: TSwtOuputColumns;
begin
  inherited;
  if ARow = FormatRow then
  begin
    Assert((ACol >= 0) and (ACol <= Ord(High(TSwtOuputColumns))));
    ColType := TSwtOuputColumns(ACol);
    CanSelect := ColType in [swocPrintSubsidence, swocPrintCompactionByLayer,
      swocPrintCompactionBySystem, swocPrintVerticalDisplacement,
      swocPrintPreconsolidationStress, swocPrintDeltaPreconsolidationStress,
      swocPrintGeostaticStress, swocPrintDeltaGeostaticStress,
      swocPrintEffectiveStress, swocPrintDeltaEffectiveStress,
      swocPrintVoidRatio, swocPrintCompressibleBedThickness,
      swocPrintLayerCenterElevation];
  end;
end;

procedure TframePackageSwt.LayoutPrintSaveControls;
var
  Column: integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Column := Max(Ord(swocPrintSubsidence), rdgOutput.LeftCol);
  LayoutControls(rdgOutput, comboMultiFomat, nil,
    Column, rdgOutput.Left);
  comboMultiFomat.Width := rdgOutput.ColWidths[Column];
  Inc(Column);
  LayoutControls(rdgOutput, cbMultiPrintSave, nil,
    Column, rdgOutput.Left + 3);
  cbMultiPrintSave.Width := 200;
end;

procedure TframePackageSwt.sbAddClick(Sender: TObject);
begin
  inherited;
  seNumExportPeriods.AsInteger := seNumExportPeriods.AsInteger +1;
  seNumExportPeriodsChange(nil);
end;

procedure TframePackageSwt.sbDeleteClick(Sender: TObject);
begin
  inherited;
  Assert(seNumExportPeriods.AsInteger > 0);
  if seNumExportPeriods.AsInteger = 1 then
  begin
    if (rdgOutput.Row >= FirstStateRow) then
    begin
      seNumExportPeriods.AsInteger := 0;
      seNumExportPeriodsChange(Sender);
    end;
  end
  else
  begin
    if (rdgOutput.Row >= FirstStateRow)
      and (rdgOutput.Row < rdgOutput.RowCount) then
    begin
      rdgOutput.BeginUpdate;
      try
        rdgOutput.DeleteRow(rdgOutput.Row);
      finally
        rdgOutput.EndUpdate;
      end;
    end;
  end;
end;

procedure TframePackageSwt.sbInsertClick(Sender: TObject);
begin
  inherited;
  if seNumExportPeriods.AsInteger = 0 then
  begin
    sbAddClick(nil);
  end
  else
  begin
    if (rdgOutput.Row >= FirstStateRow)
      and (rdgOutput.Row < rdgOutput.RowCount) then
    begin
      rdgOutput.BeginUpdate;
      try
        rdgOutput.InsertRow(rdgOutput.Row);
      finally
        rdgOutput.EndUpdate;
      end;
    end;
  end;
end;

procedure TframePackageSwt.seNumExportPeriodsChange(Sender: TObject);
var
  NumRows: Integer;
  ColIndex: Integer;
begin
  inherited;
  FChangingRowCount := True;
  try
    NumRows := seNumExportPeriods.AsInteger + FirstStateRow;
    if NumRows = FirstStateRow then
    begin
      NumRows := FirstStateRow+1;
    end;
    rdgOutput.BeginUpdate;
    try
      rdgOutput.RowCount := NumRows;
      if seNumExportPeriods.AsInteger = 0 then
      begin
        for ColIndex := Ord(swocStartTime) to Ord(swocEndTime) do
        begin
          rdgOutput.Cells[ColIndex,FirstStateRow] := '';
        end;
        for ColIndex := Ord(swocPrintSubsidence) to Ord(High(TSwtOuputColumns)) do
        begin
          rdgOutput.Checked[ColIndex,FirstStateRow] := False;
        end;
      end;
    finally
      rdgOutput.EndUpdate;
    end;
    EnableDeleteButton;
  finally
    FChangingRowCount := False;
  end
end;

procedure TframePackageSwt.SetData(Package: TModflowPackageSelection);
var
  SwtPackage: TSwtPackageSelection;
  Index: Integer;
  Item: TSwtPrintItem;
  PrintFormats: TSwtPrintFormats;
  PrintChoices: TSwtPrintCollection;
  StartTime: double;
  EndTime: double;
  ItemCount: Integer;
begin
  inherited;
  SwtPackage := Package as TSwtPackageSelection;
  if rgIthkConstant.Checked then
  begin
    SwtPackage.ThickResponse := trConstant;
  end
  else
  begin
    Assert(rbIthkVariable.Checked);
    SwtPackage.ThickResponse := trVariable;
  end;
  SwtPackage.VoidRatioResponse := TVoidRatioResponse(comboIvoid.ItemIndex);
  SwtPackage.PreconsolidationSource := TPreconsolidationSource(comboIstpcs.ItemIndex);
  SwtPackage.CompressionSource := TCompressionSource(comboIcrcc.ItemIndex);
  SwtPackage.BinaryOutputChoice := TSubBinaryOutputChoice(comboOutputChoice.ItemIndex);

  SwtPackage.InitialPrint.PrintInitialLayerCenterElevations :=
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprLayerCenterElev)];
  SwtPackage.InitialPrint.InitialLayerCenterElevationFormat :=
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprLayerCenterElev)];

  SwtPackage.InitialPrint.PrintInitialGeostaticStress :=
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprGeostaticStress)];
  SwtPackage.InitialPrint.InitialGeostaticStressFormat :=
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprGeostaticStress)];

  SwtPackage.InitialPrint.PrintInitialEffectiveStress :=
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprEffectiveStress)];
  SwtPackage.InitialPrint.InitialEffectiveStressFormat :=
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprEffectiveStress)];

  SwtPackage.InitialPrint.PrintInitialPreconsolidationStress :=
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprPreconsolidationStress)];
  SwtPackage.InitialPrint.InitialPreconsolidationStressFormat :=
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprPreconsolidationStress)];

  SwtPackage.InitialPrint.PrintInitialEquivalentStorageProperties :=
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprStorage)];
  SwtPackage.InitialPrint.InitialEquivalentStoragePropertiesFormat :=
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprStorage)];


  PrintFormats := SwtPackage.PrintFormats;
  PrintFormats.SubsidenceFormat := rdgOutput.ItemIndex[Ord(swocPrintSubsidence), FormatRow];
  PrintFormats.CompactionByModelLayerFormat := rdgOutput.ItemIndex[Ord(swocPrintCompactionByLayer), FormatRow];
  PrintFormats.CompactionByInterbedSystemFormat := rdgOutput.ItemIndex[Ord(swocPrintCompactionBySystem), FormatRow];
  PrintFormats.VerticalDisplacementFormat := rdgOutput.ItemIndex[Ord(swocPrintVerticalDisplacement), FormatRow];
  PrintFormats.PreconsolidationStress := rdgOutput.ItemIndex[Ord(swocPrintPreconsolidationStress), FormatRow];
  PrintFormats.DeltaPreconsolidationStress := rdgOutput.ItemIndex[Ord(swocPrintDeltaPreconsolidationStress), FormatRow];
  PrintFormats.GeostaticStress := rdgOutput.ItemIndex[Ord(swocPrintGeostaticStress), FormatRow];
  PrintFormats.DeltaGeostaticStress := rdgOutput.ItemIndex[Ord(swocPrintDeltaGeostaticStress), FormatRow];
  PrintFormats.EffectiveStress := rdgOutput.ItemIndex[Ord(swocPrintEffectiveStress), FormatRow];
  PrintFormats.DeltaEffectiveStress := rdgOutput.ItemIndex[Ord(swocPrintDeltaEffectiveStress), FormatRow];
  PrintFormats.VoidRatio := rdgOutput.ItemIndex[Ord(swocPrintVoidRatio), FormatRow];
  PrintFormats.ThicknessCompressibleSediments := rdgOutput.ItemIndex[Ord(swocPrintCompressibleBedThickness), FormatRow];
  PrintFormats.LayerCenterElevation := rdgOutput.ItemIndex[Ord(swocPrintLayerCenterElevation), FormatRow];

  PrintChoices := SwtPackage.PrintChoices;
  ItemCount := 0;
  for Index := 0 to seNumExportPeriods.AsInteger - 1 do
  begin
    if TryStrToFloat(rdgOutput.Cells[Ord(swocStartTime),Index+FirstStateRow], StartTime)
      and TryStrToFloat(rdgOutput.Cells[Ord(swocEndTime),Index+FirstStateRow], EndTime) then
    begin
      if ItemCount < SwtPackage.PrintChoices.Count then
      begin
        Item := SwtPackage.PrintChoices[ItemCount];
      end
      else
      begin
        Item := SwtPackage.PrintChoices.Add as TSwtPrintItem;
      end;
      Inc(ItemCount);
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;
      Item.PrintSubsidence :=
        rdgOutput.Checked[Ord(swocPrintSubsidence),Index+FirstStateRow];
      Item.SaveSubsidence :=
        rdgOutput.Checked[Ord(swocSaveSubsidence),Index+FirstStateRow];
      Item.PrintCompactionByModelLayer :=
        rdgOutput.Checked[Ord(swocPrintCompactionByLayer),Index+FirstStateRow];
      Item.SaveCompactionByModelLayer :=
        rdgOutput.Checked[Ord(swocSaveCompactionByLayer),Index+FirstStateRow];
      Item.PrintCompactionByInterbedSystem :=
        rdgOutput.Checked[Ord(swocPrintCompactionBySystem),Index+FirstStateRow];
      Item.SaveCompactionByInterbedSystem :=
        rdgOutput.Checked[Ord(swocSaveCompactionBySystem),Index+FirstStateRow];
      Item.PrintVerticalDisplacement :=
        rdgOutput.Checked[Ord(swocPrintVerticalDisplacement),Index+FirstStateRow];
      Item.SaveVerticalDisplacement :=
        rdgOutput.Checked[Ord(swocSaveVerticalDisplacement),Index+FirstStateRow];
      Item.PrintPreconsolidationStress :=
        rdgOutput.Checked[Ord(swocPrintPreconsolidationStress),Index+FirstStateRow];
      Item.SavePreconsolidationStress :=
        rdgOutput.Checked[Ord(swocSavePreconsolidationStress),Index+FirstStateRow];
      Item.PrintDeltaPreconsolidationStress :=
        rdgOutput.Checked[Ord(swocPrintDeltaPreconsolidationStress),Index+FirstStateRow];
      Item.SaveDeltaPreconsolidationStress :=
        rdgOutput.Checked[Ord(swocSaveDeltaPreconsolidationStress),Index+FirstStateRow];
      Item.PrintGeostaticStress :=
        rdgOutput.Checked[Ord(swocPrintGeostaticStress),Index+FirstStateRow];
      Item.SaveGeostaticStress :=
        rdgOutput.Checked[Ord(swocSaveGeostaticStress),Index+FirstStateRow];
      Item.PrintDeltaGeostaticStress :=
        rdgOutput.Checked[Ord(swocPrintDeltaGeostaticStress),Index+FirstStateRow];
      Item.SaveDeltaGeostaticStress :=
        rdgOutput.Checked[Ord(swocSaveDeltaGeostaticStress),Index+FirstStateRow];
      Item.PrintEffectiveStress :=
        rdgOutput.Checked[Ord(swocPrintEffectiveStress),Index+FirstStateRow];
      Item.SaveEffectiveStress :=
        rdgOutput.Checked[Ord(swocSaveEffectiveStress),Index+FirstStateRow];
      Item.PrintDeltaEffectiveStress :=
        rdgOutput.Checked[Ord(swocPrintDeltaEffectiveStress),Index+FirstStateRow];
      Item.SaveDeltaEffectiveStress :=
        rdgOutput.Checked[Ord(swocSaveDeltaEffectiveStress),Index+FirstStateRow];
      Item.PrintVoidRatio :=
        rdgOutput.Checked[Ord(swocPrintVoidRatio),Index+FirstStateRow];
      Item.SaveVoidRatio :=
        rdgOutput.Checked[Ord(swocSaveVoidRatio),Index+FirstStateRow];
      Item.PrintThicknessCompressibleSediments :=
        rdgOutput.Checked[Ord(swocPrintCompressibleBedThickness),Index+FirstStateRow];
      Item.SaveThicknessCompressibleSediments :=
        rdgOutput.Checked[Ord(swocSaveCompressibleBedThickness),Index+FirstStateRow];
      Item.PrintLayerCenterElevation :=
        rdgOutput.Checked[Ord(swocPrintLayerCenterElevation),Index+FirstStateRow];
      Item.SaveLayerCenterElevation :=
        rdgOutput.Checked[Ord(swocSaveLayerCenterElevation),Index+FirstStateRow];
    end;
  end;
  while PrintChoices.Count > ItemCount do
  begin
    PrintChoices.Delete(PrintChoices.Count-1);
  end;
end;

procedure TframePackageSwt.cbMultiPrintSaveClick(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  rdgOutput.BeginUpdate;
  try
    for RowIndex := FirstStateRow to rdgOutput.RowCount - 1 do
    begin
      for ColIndex := Ord(swocPrintSubsidence) to rdgOutput.ColCount - 1 do
      begin
        if rdgOutput.IsSelectedCell(ColIndex, RowIndex) then
        begin
          rdgOutput.Checked[ColIndex, RowIndex] := cbMultiPrintSave.Checked;
          if Assigned(rdgOutput.OnStateChange) then
          begin
            rdgOutput.OnStateChange(rdgOutput, ColIndex, RowIndex, cbMultiPrintSave.State);
          end;
        end;
      end;
    end;
  finally
    rdgOutput.EndUpdate;
  end;
end;

procedure TframePackageSwt.comboMultiFomatChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  inherited;
  rdgOutput.BeginUpdate;
  try
    for ColIndex := 0 to rdgOutput.ColCount - 1 do
    begin
      if rdgOutput.IsSelectedCell(ColIndex, FormatRow) then
      begin
        rdgOutput.Cells[ColIndex, FormatRow] := comboMultiFomat.Text;
        if Assigned(rdgOutput.OnSetEditText) then
        begin
          rdgOutput.OnSetEditText(rdgOutput, ColIndex, FormatRow, comboMultiFomat.Text);
        end;
      end;
    end;
  finally
    rdgOutput.EndUpdate;
  end;end;

procedure TframePackageSwt.EnableDeleteButton;
begin
  sbDelete.Enabled := (seNumExportPeriods.AsInteger >= 1)
    and rcSelectionController.Enabled;
end;


procedure TframePackageSwt.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutPrintSaveControls;

end;

procedure TframePackageSwt.GetData(Package: TModflowPackageSelection);
var
  StartCell: TGridRect;
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  SwtPackage: TSwtPackageSelection;
  Index: Integer;
  Item: TSwtPrintItem;
  InitialPrint: TSwtInitialPrint;
  PrintFormats: TSwtPrintFormats;
begin
  inherited;
  lblPackage.Width := lblPackage.Parent.Width - lblPackage.Left -8;

  pcSWT.ActivePageIndex := 0;

  StartCell.Left := 0;
  StartCell.Right := 0;
  StartCell.Top := 2;
  StartCell.Bottom := 2;

  rdgOutput.Selection := StartCell;

  rdgOutput.Columns[Ord(swocStartTime)].PickList.Clear;
  rdgOutput.Columns[Ord(swocEndTime)].PickList.Clear;
  rdgOutput.Columns[Ord(swocStartTime)].PickList.Capacity :=
    frmGoPhast.PhastModel.ModflowStressPeriods.Count;
  rdgOutput.Columns[Ord(swocEndTime)].PickList.Capacity :=
    frmGoPhast.PhastModel.ModflowStressPeriods.Count;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[TimeIndex];
    rdgOutput.Columns[Ord(swocStartTime)].PickList.
      Add(FloatToStr(StressPeriod.StartTime));
    rdgOutput.Columns[Ord(swocEndTime)].PickList.
      Add(FloatToStr(StressPeriod.EndTime));
  end;

  SwtPackage := Package as TSwtPackageSelection;
  case SwtPackage.ThickResponse of
    trConstant: rgIthkConstant.Checked := True;
    trVariable: rbIthkVariable.Checked := True;
    else Assert(False);
  end;
  comboIvoid.ItemIndex := Ord(SwtPackage.VoidRatioResponse);
  comboIstpcs.ItemIndex := Ord(SwtPackage.PreconsolidationSource);
  comboIcrcc.ItemIndex := Ord(SwtPackage.CompressionSource);
  comboOutputChoice.ItemIndex := Ord(SwtPackage.BinaryOutputChoice);

  rdgInitialPrintChoices.BeginUpdate;
  try
    InitialPrint := SwtPackage.InitialPrint;
    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprLayerCenterElev)]
      := InitialPrint.PrintInitialLayerCenterElevations;
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprLayerCenterElev)]
      := InitialPrint.InitialLayerCenterElevationFormat;

    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprGeostaticStress)]
      := InitialPrint.PrintInitialGeostaticStress;
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprGeostaticStress)]
      := InitialPrint.InitialGeostaticStressFormat;

    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprEffectiveStress)]
      := InitialPrint.PrintInitialEffectiveStress;
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprEffectiveStress)]
      := InitialPrint.InitialEffectiveStressFormat;

    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprPreconsolidationStress)]
      := InitialPrint.PrintInitialPreconsolidationStress;
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprPreconsolidationStress)]
      := InitialPrint.InitialPreconsolidationStressFormat;

    rdgInitialPrintChoices.Checked[Ord(ipcPrint), Ord(iprStorage)]
      := InitialPrint.PrintInitialEquivalentStorageProperties;
    rdgInitialPrintChoices.ItemIndex[Ord(ipcFormat), Ord(iprStorage)]
      := InitialPrint.InitialEquivalentStoragePropertiesFormat;
  finally
    rdgInitialPrintChoices.EndUpdate;
  end;

  rdgOutput.BeginUpdate;
  try
    seNumExportPeriods.AsInteger := SwtPackage.PrintChoices.Count;
    seNumExportPeriodsChange(nil);

    PrintFormats := SwtPackage.PrintFormats;
    rdgOutput.ItemIndex[Ord(swocPrintSubsidence), FormatRow] :=
      PrintFormats.SubsidenceFormat;
    rdgOutput.ItemIndex[Ord(swocPrintCompactionByLayer), FormatRow] :=
      PrintFormats.CompactionByModelLayerFormat;
    rdgOutput.ItemIndex[Ord(swocPrintCompactionBySystem), FormatRow] :=
      PrintFormats.CompactionByInterbedSystemFormat;
    rdgOutput.ItemIndex[Ord(swocPrintVerticalDisplacement), FormatRow] :=
      PrintFormats.VerticalDisplacementFormat;
    rdgOutput.ItemIndex[Ord(swocPrintPreconsolidationStress), FormatRow] :=
      PrintFormats.PreconsolidationStress;
    rdgOutput.ItemIndex[Ord(swocPrintDeltaPreconsolidationStress), FormatRow] :=
      PrintFormats.DeltaPreconsolidationStress;
    rdgOutput.ItemIndex[Ord(swocPrintGeostaticStress), FormatRow] :=
      PrintFormats.GeostaticStress;
    rdgOutput.ItemIndex[Ord(swocPrintDeltaGeostaticStress), FormatRow] :=
      PrintFormats.DeltaGeostaticStress;
    rdgOutput.ItemIndex[Ord(swocPrintEffectiveStress), FormatRow] :=
      PrintFormats.EffectiveStress;
    rdgOutput.ItemIndex[Ord(swocPrintDeltaEffectiveStress), FormatRow] :=
      PrintFormats.DeltaEffectiveStress;
    rdgOutput.ItemIndex[Ord(swocPrintVoidRatio), FormatRow] :=
      PrintFormats.VoidRatio;
    rdgOutput.ItemIndex[Ord(swocPrintCompressibleBedThickness), FormatRow] :=
      PrintFormats.ThicknessCompressibleSediments;
    rdgOutput.ItemIndex[Ord(swocPrintLayerCenterElevation), FormatRow] :=
      PrintFormats.LayerCenterElevation;

    for Index := 0 to SwtPackage.PrintChoices.Count - 1 do
    begin
      Item := SwtPackage.PrintChoices[Index];
      rdgOutput.Cells[Ord(swocStartTime),Index+FirstStateRow]
        := FloatToStr(Item.StartTime);
      rdgOutput.Cells[Ord(swocEndTime),Index+FirstStateRow]
        := FloatToStr(Item.EndTime);
      rdgOutput.Checked[Ord(swocPrintSubsidence),Index+FirstStateRow]
        := Item.PrintSubsidence;
      rdgOutput.Checked[Ord(swocSaveSubsidence),Index+FirstStateRow]
        := Item.SaveSubsidence;
      rdgOutput.Checked[Ord(swocPrintCompactionByLayer),Index+FirstStateRow]
        := Item.PrintCompactionByModelLayer;
      rdgOutput.Checked[Ord(swocSaveCompactionByLayer),Index+FirstStateRow]
        := Item.SaveCompactionByModelLayer;
      rdgOutput.Checked[Ord(swocPrintCompactionBySystem),Index+FirstStateRow]
        := Item.PrintCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(swocSaveCompactionBySystem),Index+FirstStateRow]
        := Item.SaveCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(swocPrintVerticalDisplacement),Index+FirstStateRow]
        := Item.PrintVerticalDisplacement;
      rdgOutput.Checked[Ord(swocSaveVerticalDisplacement),Index+FirstStateRow]
        := Item.SaveVerticalDisplacement;
      rdgOutput.Checked[Ord(swocPrintPreconsolidationStress),Index+FirstStateRow]
        := Item.PrintPreconsolidationStress;
      rdgOutput.Checked[Ord(swocSavePreconsolidationStress),Index+FirstStateRow]
        := Item.SavePreconsolidationStress;
      rdgOutput.Checked[Ord(swocPrintDeltaPreconsolidationStress),Index+FirstStateRow]
        := Item.PrintDeltaPreconsolidationStress;
      rdgOutput.Checked[Ord(swocSaveDeltaPreconsolidationStress),Index+FirstStateRow]
        := Item.SaveDeltaPreconsolidationStress;
      rdgOutput.Checked[Ord(swocPrintGeostaticStress),Index+FirstStateRow]
        := Item.PrintGeostaticStress;
      rdgOutput.Checked[Ord(swocSaveGeostaticStress),Index+FirstStateRow]
        := Item.SaveGeostaticStress;
      rdgOutput.Checked[Ord(swocPrintDeltaGeostaticStress),Index+FirstStateRow]
        := Item.PrintDeltaGeostaticStress;
      rdgOutput.Checked[Ord(swocSaveDeltaGeostaticStress),Index+FirstStateRow]
        := Item.SaveDeltaGeostaticStress;
      rdgOutput.Checked[Ord(swocPrintEffectiveStress),Index+FirstStateRow]
        := Item.PrintEffectiveStress;
      rdgOutput.Checked[Ord(swocSaveEffectiveStress),Index+FirstStateRow]
        := Item.SaveEffectiveStress;
      rdgOutput.Checked[Ord(swocPrintDeltaEffectiveStress),Index+FirstStateRow]
        := Item.PrintDeltaEffectiveStress;
      rdgOutput.Checked[Ord(swocSaveDeltaEffectiveStress),Index+FirstStateRow]
        := Item.SaveDeltaEffectiveStress;
      rdgOutput.Checked[Ord(swocPrintVoidRatio),Index+FirstStateRow]
        := Item.PrintVoidRatio;
      rdgOutput.Checked[Ord(swocSaveVoidRatio),Index+FirstStateRow]
        := Item.SaveVoidRatio;
      rdgOutput.Checked[Ord(swocPrintCompressibleBedThickness),Index+FirstStateRow]
        := Item.PrintThicknessCompressibleSediments;
      rdgOutput.Checked[Ord(swocSaveCompressibleBedThickness),Index+FirstStateRow]
        := Item.SaveThicknessCompressibleSediments;
      rdgOutput.Checked[Ord(swocPrintLayerCenterElevation),Index+FirstStateRow]
        := Item.PrintLayerCenterElevation;
      rdgOutput.Checked[Ord(swocSaveLayerCenterElevation),Index+FirstStateRow]
        := Item.SaveLayerCenterElevation;
    end;

  finally
    rdgOutput.EndUpdate;
  end;
end;

end.
