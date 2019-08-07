unit framePackageSubUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, Mask, JvExMask, JvSpin,
  ArgusDataEntry, JvToolEdit, Grids, RbwDataGrid4, ComCtrls, JvExStdCtrls,
  JvCombobox, JvListComb, Math, ModflowPackageSelectionUnit, Buttons, GrayTabs;

type
  TSubOutputColumns = (socStartTime, socEndTime,
    socPrintSubsidence, socSaveSubsidence,
    socPrintCompactModelLayer, socSaveCompactModelLayer,
    socPrintCompactByInterbedSystem, socSaveCompactByInterbedSystem,
    socPrintVerticalDisplacement, socSaveVerticalDisplacement,
    socPrintNoDelayPreconsolidationHead, socSaveNoDelayPreconsolidationHead,
    socPrintDelayPreconsolidationHead, socSaveDelayPreconsolidationHead,
    socPrintDelayInterbedBudget, socPrintElastCompML, socSaveElastCompML,
    socPrintInelastCompML, socSaveInelastCompML, socPrintElastCompIB,
    socSaveElastCompIB, socPrintInelastCompIB, socSaveInelastCompIB);

  TframePackageSub = class(TframePackage)
    pcSub: TPageControl;
    tabControls: TTabSheet;
    seNumberOfNodes: TJvSpinEdit;
    lblNumberOfNodes: TLabel;
    rdeAccel1: TRbwDataEntry;
    lblAccel1: TLabel;
    rdeAccel2: TRbwDataEntry;
    lblAccel2: TLabel;
    rdeMinIterations: TRbwDataEntry;
    lblMinIterations: TLabel;
    cbSaveRestart: TCheckBox;
    lbReadRestart: TLabel;
    feReadRestart: TJvFilenameEdit;
    tabPrintSave: TTabSheet;
    cbMultiPrintSave: TCheckBox;
    rdgOutput: TRbwDataGrid4;
    seNumExportPeriods: TJvSpinEdit;
    lblNumExportPeriods: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    comboOutputChoice: TJvImageComboBox;
    lblOutputChoice: TLabel;
    comboMultiFomat: TJvImageComboBox;
    cbLinkSubsidence: TCheckBox;
    procedure comboMultiFomatChange(Sender: TObject);
    procedure cbMultiPrintSaveClick(Sender: TObject);
    procedure rdgOutputColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgOutputHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rdgOutputMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seNumExportPeriodsChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure rdgOutputEndUpdate(Sender: TObject);
    procedure rdgOutputSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FChangingRowCount: Boolean;
    procedure LayoutPrintSaveControls;
    procedure EnableDeleteButton;
    procedure EnableLinkSubsidence;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure Loaded; override;
    { Public declarations }
  end;

var
  framePackageSub: TframePackageSub;

implementation

uses
  GoPhastTypes, frmCustomGoPhastUnit, frmGoPhastUnit, ModflowTimeUnit,
  ModelMuseUtilities;

resourcestring
  StrPrintSubsidenceIf = 'Print subsidence (Ifm1, Ifl1)';
  StrSaveSubsidenceIfl = 'Save subsidence (Ifl2)';
  StrPrintCompactionBy = 'Print compaction by model layer (Ifm2, Ifl3)';
  StrSaveCompactionByM = 'Save compaction by model layer (Ifl4)';
  StrPrintCompactionByIB = 'Print compaction by interbed system (Ifm3, Ifl5)';
  StrSaveCompactionByIB = 'Save compaction by interbed system (Ifl6)';
  StrPrintVerticalDispl = 'Print vertical displacement (Ifm4, Ifl7)';
  StrSaveVerticalDispla = 'Save vertical displacement (Ifl8)';
  StrPrintCriticalHead = 'Print critical head for no-delay interbeds (Ifm5, ' +
  'Ifl9)';
  StrSaveCriticalHeadF = 'Save critical head for no-delay interbeds (Ifl10)';
  StrPrintCriticalHeadDelay = 'Print critical head for delay interbeds (Ifm6' +
  ', Ifl11)';
  StrSaveCriticalHeadFDelay = 'Save critical head for delay interbeds (Ifl12' +
  ')';
  StrPrintVolumetricBud = 'Print volumetric budget for delay interbeds (Ifl1' +
  '3)';
  StrFormat = 'Format';
  StrPrintElasticCompacML = 'Print elastic compaction by model layer (Ifm7, ' +
  'Ifl14)';
  StrSaveElasticCompactMl = 'Save elastic compaction by model layer (Ifl15)';
  StrPrintInelasticByML = 'Print inelastic compaction by model layer (Ifm8, Ifl16)';
  StrSaveInelasticCompaML = 'Save inelastic compaction by model layer (Ifl17)';
  StrPrintElasticCompacIB = 'Print elastic compaction by interbed system (If' +
  'm9, Ifl18)';
  StrSaveElasticCompactIB = 'Save elastic compaction by interbed system (Ifl19)';
  StrPrintInelasticCompIB = 'Print inelastic compaction by interbed system (' +
  'Ifm10, Ifl20)';
  StrSaveInelasticCompaIB = 'Save inelastic compaction by interbed system (Ifl21)';

{$R *.dfm}

const
  FormatRow = 1;
  FirstStateRow = FormatRow+1;

{ TframePackageSub }

procedure TframePackageSub.cbMultiPrintSaveClick(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  rdgOutput.BeginUpdate;
  try
    for RowIndex := FirstStateRow to rdgOutput.RowCount - 1 do
    begin
      for ColIndex := Ord(socPrintSubsidence) to rdgOutput.ColCount - 1 do
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

procedure TframePackageSub.comboMultiFomatChange(Sender: TObject);
var
  ColIndex: Integer;
begin
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
  end;
end;

procedure TframePackageSub.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.GetData(Package: TModflowPackageSelection);
var
  SubPackage: TSubPackageSelection;
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Index: Integer;
  Item: TSubPrintItem;
  StartCell: TGridRect;
begin
  inherited;
  pcSub.ActivePageIndex := 0;

  StartCell.Left := 0;
  StartCell.Right := 0;
  StartCell.Top := 2;
  StartCell.Bottom := 2;

  rdgOutput.Selection := StartCell;

  rdgOutput.Columns[Ord(socStartTime)].PickList.Clear;
  rdgOutput.Columns[Ord(socEndTime)].PickList.Clear;
  rdgOutput.Columns[Ord(socStartTime)].PickList.Capacity :=
    frmGoPhast.PhastModel.ModflowStressPeriods.Count;
  rdgOutput.Columns[Ord(socEndTime)].PickList.Capacity :=
    frmGoPhast.PhastModel.ModflowStressPeriods.Count;

  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[TimeIndex];
    rdgOutput.Columns[Ord(socStartTime)].PickList.
      Add(FloatToStr(StressPeriod.StartTime));
    rdgOutput.Columns[Ord(socEndTime)].PickList.
      Add(FloatToStr(StressPeriod.EndTime));
  end;

  SubPackage := Package as TSubPackageSelection;
  seNumberOfNodes.AsInteger := SubPackage.NumberOfNodes;
  rdeAccel1.Text := FloatToStr(SubPackage.AccelerationParameter1);
  rdeAccel2.Text := FloatToStr(SubPackage.AccelerationParameter2);
  rdeMinIterations.Text := IntToStr(SubPackage.MinIterations);
  cbSaveRestart.Checked := SubPackage.SaveDelayRestart;
  cbLinkSubsidence.Checked := SubPackage.LinkSubsidence;
  try
    feReadRestart.FileName := SubPackage.ReadDelayRestartFileName;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  comboOutputChoice.ItemIndex := Ord(SubPackage.BinaryOutputChoice);

  rdgOutput.BeginUpdate;
  try
    seNumExportPeriods.AsInteger := SubPackage.PrintChoices.Count;
    seNumExportPeriodsChange(nil);

    rdgOutput.ItemIndex[Ord(socPrintSubsidence), FormatRow] :=
      SubPackage.PrintFormats.SubsidenceFormat;

    rdgOutput.ItemIndex[Ord(socPrintCompactModelLayer), FormatRow] :=
      SubPackage.PrintFormats.CompactionByModelLayerFormat;

    rdgOutput.ItemIndex[Ord(socPrintCompactByInterbedSystem), FormatRow] :=
      SubPackage.PrintFormats.CompactionByInterbedSystemFormat;

    rdgOutput.ItemIndex[Ord(socPrintVerticalDisplacement), FormatRow] :=
      SubPackage.PrintFormats.VerticalDisplacementFormat;

    rdgOutput.ItemIndex[Ord(socPrintNoDelayPreconsolidationHead), FormatRow] :=
      SubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat;

    rdgOutput.ItemIndex[Ord(socPrintDelayPreconsolidationHead), FormatRow] :=
      SubPackage.PrintFormats.DelayPreconsolidationHeadFormat;

    rdgOutput.ItemIndex[Ord(socPrintElastCompML), FormatRow] :=
      SubPackage.PrintFormats.ElasticCompactionByModelLayerFormat;

    rdgOutput.ItemIndex[Ord(socPrintInelastCompML), FormatRow] :=
      SubPackage.PrintFormats.InelasticCompactionByModelLayerFormat;

    rdgOutput.ItemIndex[Ord(socPrintElastCompIB), FormatRow] :=
      SubPackage.PrintFormats.ElasticCompactionByInterbedSystemFormat;

    rdgOutput.ItemIndex[Ord(socPrintInelastCompIB), FormatRow] :=
      SubPackage.PrintFormats.InelasticCompactionByInterbedSystemFormat;

    for Index := 0 to SubPackage.PrintChoices.Count - 1 do
    begin
      Item := SubPackage.PrintChoices[Index];
      rdgOutput.Cells[Ord(socStartTime),Index+FirstStateRow]
        := FloatToStr(Item.StartTime);
      rdgOutput.Cells[Ord(socEndTime),Index+FirstStateRow]
        := FloatToStr(Item.EndTime);
      rdgOutput.Checked[Ord(socPrintSubsidence),Index+FirstStateRow]
        := Item.PrintSubsidence;
      rdgOutput.Checked[Ord(socSaveSubsidence),Index+FirstStateRow]
        := Item.SaveSubsidence;
      rdgOutput.Checked[Ord(socPrintCompactModelLayer),Index+FirstStateRow]
        := Item.PrintCompactionByModelLayer;
      rdgOutput.Checked[Ord(socSaveCompactModelLayer),Index+FirstStateRow]
        := Item.SaveCompactionByModelLayer;
      rdgOutput.Checked[Ord(socPrintCompactByInterbedSystem),Index+FirstStateRow]
        := Item.PrintCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(socSaveCompactByInterbedSystem),Index+FirstStateRow]
        := Item.SaveCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(socPrintVerticalDisplacement),Index+FirstStateRow]
        := Item.PrintVerticalDisplacement;
      rdgOutput.Checked[Ord(socSaveVerticalDisplacement),Index+FirstStateRow]
        := Item.SaveVerticalDisplacement;
      rdgOutput.Checked[Ord(socPrintNoDelayPreconsolidationHead),Index+FirstStateRow]
        := Item.PrintCriticalHeadNoDelay;
      rdgOutput.Checked[Ord(socSaveNoDelayPreconsolidationHead),Index+FirstStateRow]
        := Item.SaveCriticalHeadNoDelay;
      rdgOutput.Checked[Ord(socPrintDelayPreconsolidationHead),Index+FirstStateRow]
        := Item.PrintCriticalHeadDelay;
      rdgOutput.Checked[Ord(socSaveDelayPreconsolidationHead),Index+FirstStateRow]
        := Item.SaveCriticalHeadDelay;
      rdgOutput.Checked[Ord(socPrintDelayInterbedBudget),Index+FirstStateRow]
        := Item.PrintDelayBudgets;

      rdgOutput.Checked[Ord(socPrintElastCompML),Index+FirstStateRow]
        := Item.PrintElasticCompactionByModelLayer;
      rdgOutput.Checked[Ord(socSaveElastCompML),Index+FirstStateRow]
        := Item.SaveElasticCompactionByModelLayer;
      rdgOutput.Checked[Ord(socPrintInelastCompML),Index+FirstStateRow]
        := Item.PrintInelasticCompactionByModelLayer;
      rdgOutput.Checked[Ord(socSaveInelastCompML),Index+FirstStateRow]
        := Item.SaveInelasticCompactionByModelLayer;
      rdgOutput.Checked[Ord(socPrintElastCompIB),Index+FirstStateRow]
        := Item.PrintElasticCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(socSaveElastCompIB),Index+FirstStateRow]
        := Item.SaveElasticCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(socPrintInelastCompIB),Index+FirstStateRow]
        := Item.PrintInelasticCompactionByInterbedSystem;
      rdgOutput.Checked[Ord(socSaveInelastCompIB),Index+FirstStateRow]
        := Item.SaveInelasticCompactionByInterbedSystem;

    end;
  finally
    rdgOutput.EndUpdate;
  end;
end;

procedure TframePackageSub.LayoutPrintSaveControls;
var
  Column: integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Column := Max(Ord(socPrintSubsidence), rdgOutput.LeftCol);
  LayoutControls(rdgOutput, comboMultiFomat, nil,
    Column, rdgOutput.Left);
  comboMultiFomat.Width := rdgOutput.ColWidths[Column];
  Inc(Column);
  LayoutControls(rdgOutput, cbMultiPrintSave, nil,
    Column, rdgOutput.Left + 3);
  cbMultiPrintSave.Width := 200;
end;

procedure TframePackageSub.Loaded;
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
  rdgOutput.Height := tabPrintSave.Height - rdgOutput.Top -
    (tabPrintSave.Height - seNumExportPeriods.Top + 8);

  for ColIndex := 0 to rdgOutput.ColCount - 1 do
  begin
    rdgOutput.SpecialFormat[ColIndex,FormatRow] := rcf4String;
    rdgOutput.UseSpecialFormat[ColIndex,FormatRow] := True;
  end;
  for ColIndex := Ord(socPrintSubsidence) to rdgOutput.ColCount - 1 do
  begin
    rdgOutput.Columns[ColIndex].ComboUsed := True;
    rdgOutput.Columns[ColIndex].PickList :=
      rdgOutput.Columns[Ord(socPrintSubsidence)].PickList;
  end;

  rdgOutput.Cells[Ord(socStartTime), 0] := StrStartingTime;
  rdgOutput.Cells[Ord(socEndTime), 0] := StrEndingTime;
  rdgOutput.Cells[Ord(socPrintSubsidence), 0] := StrPrintSubsidenceIf;
  rdgOutput.Cells[Ord(socSaveSubsidence), 0] := StrSaveSubsidenceIfl;
  rdgOutput.Cells[Ord(socPrintCompactModelLayer), 0] := StrPrintCompactionBy;
  rdgOutput.Cells[Ord(socSaveCompactModelLayer), 0] := StrSaveCompactionByM;
  rdgOutput.Cells[Ord(socPrintCompactByInterbedSystem), 0] := StrPrintCompactionByIB;
  rdgOutput.Cells[Ord(socSaveCompactByInterbedSystem), 0] := StrSaveCompactionByIB;
  rdgOutput.Cells[Ord(socPrintVerticalDisplacement), 0] := StrPrintVerticalDispl;
  rdgOutput.Cells[Ord(socSaveVerticalDisplacement), 0] := StrSaveVerticalDispla;
  rdgOutput.Cells[Ord(socPrintNoDelayPreconsolidationHead), 0] := StrPrintCriticalHead;
  rdgOutput.Cells[Ord(socSaveNoDelayPreconsolidationHead), 0] := StrSaveCriticalHeadF;
  rdgOutput.Cells[Ord(socPrintDelayPreconsolidationHead), 0] := StrPrintCriticalHeadDelay;
  rdgOutput.Cells[Ord(socSaveDelayPreconsolidationHead), 0] := StrSaveCriticalHeadFDelay;
  rdgOutput.Cells[Ord(socPrintDelayInterbedBudget), 0] := StrPrintVolumetricBud;

  rdgOutput.Cells[Ord(socPrintElastCompML), 0] := StrPrintElasticCompacML;
  rdgOutput.Cells[Ord(socSaveElastCompML), 0] := StrSaveElasticCompactMl;
  rdgOutput.Cells[Ord(socPrintInelastCompML), 0] := StrPrintInelasticByML;
  rdgOutput.Cells[Ord(socSaveInelastCompML), 0] := StrSaveInelasticCompaML;
  rdgOutput.Cells[Ord(socPrintElastCompIB), 0] := StrPrintElasticCompacIB;
  rdgOutput.Cells[Ord(socSaveElastCompIB), 0] := StrSaveElasticCompactIB;
  rdgOutput.Cells[Ord(socPrintInelastCompIB), 0] := StrPrintInelasticCompIB;
  rdgOutput.Cells[Ord(socSaveInelastCompIB), 0] := StrSaveInelasticCompaIB;

  rdgOutput.Cells[Ord(socStartTime), FormatRow] := StrFormat;

  pcSub.ActivePageIndex := 0;

end;

procedure TframePackageSub.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableDeleteButton;
  EnableLinkSubsidence;
end;

procedure TframePackageSub.rdgOutputColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.rdgOutputEndUpdate(Sender: TObject);
var
  NumRows: Integer;
begin
  inherited;
  if (not FChangingRowCount) and (seNumExportPeriods <> nil) then
  begin
    NumRows := rdgOutput.RowCount - FirstStateRow;
    if (NumRows = FirstStateRow-1)
      and (rdgOutput.Cells[Ord(socStartTime),FirstStateRow] = '')
      and (rdgOutput.Cells[Ord(socEndTime),FirstStateRow] = '') then
    begin
      NumRows := 0;
    end;
    seNumExportPeriods.AsInteger := NumRows;
  end;
end;

procedure TframePackageSub.rdgOutputHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.rdgOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(comboMultiFomat, rdgOutput);
  EnableMultiEditControl(cbMultiPrintSave, rdgOutput);
end;

procedure TframePackageSub.rdgOutputSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ColType: TSubOutputColumns;
begin
  inherited;
  if ARow = FormatRow then
  begin
    Assert((ACol >= 0) and (ACol <= Ord(High(TSubOutputColumns))));
    ColType := TSubOutputColumns(ACol);
    CanSelect := ColType in [socPrintSubsidence, socPrintCompactModelLayer,
      socPrintCompactByInterbedSystem, socPrintVerticalDisplacement,
      socPrintNoDelayPreconsolidationHead, socPrintDelayPreconsolidationHead,
      socPrintElastCompML, socPrintInelastCompML, socPrintElastCompIB,
      socPrintInelastCompIB];
  end;
  if frmGoPhast.ModelSelection <> msModflowFmp then
  begin
    if ACol >= Ord(socPrintElastCompML) then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframePackageSub.sbAddClick(Sender: TObject);
begin
  inherited;
  seNumExportPeriods.AsInteger := seNumExportPeriods.AsInteger +1;
  seNumExportPeriodsChange(nil);
end;

procedure TframePackageSub.sbDeleteClick(Sender: TObject);
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

procedure TframePackageSub.sbInsertClick(Sender: TObject);
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

procedure TframePackageSub.seNumExportPeriodsChange(Sender: TObject);
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
        for ColIndex := Ord(socStartTime) to Ord(socEndTime) do
        begin
          rdgOutput.Cells[ColIndex,FirstStateRow] := '';
        end;
        for ColIndex := Ord(socPrintSubsidence) to Ord(High(TSubOutputColumns)) do
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
  end;
end;

procedure TframePackageSub.SetData(Package: TModflowPackageSelection);
var
  SubPackage: TSubPackageSelection;
  PrintChoiceCount: Integer;
  Index: Integer;
  StartTime: double;
  EndTime: double;
  Item: TSubPrintItem;
begin
  inherited;
  SubPackage := Package as TSubPackageSelection;
  SubPackage.NumberOfNodes := seNumberOfNodes.AsInteger;
  SubPackage.AccelerationParameter1 := FortranStrToFloat(rdeAccel1.Text);
  SubPackage.AccelerationParameter2 := FortranStrToFloat(rdeAccel2.Text);
  SubPackage.MinIterations := StrToInt(rdeMinIterations.Text);
  SubPackage.SaveDelayRestart := cbSaveRestart.Checked;
  SubPackage.ReadDelayRestartFileName := feReadRestart.FileName;
  SubPackage.LinkSubsidence := cbLinkSubsidence.Checked;
  SubPackage.BinaryOutputChoice :=
    TSubBinaryOutputChoice(comboOutputChoice.ItemIndex);

  SubPackage.PrintFormats.SubsidenceFormat :=
    rdgOutput.ItemIndex[Ord(socPrintSubsidence), FormatRow];

  SubPackage.PrintFormats.CompactionByModelLayerFormat :=
    rdgOutput.ItemIndex[Ord(socPrintCompactModelLayer), FormatRow];

  SubPackage.PrintFormats.CompactionByInterbedSystemFormat :=
    rdgOutput.ItemIndex[Ord(socPrintCompactByInterbedSystem), FormatRow];

  SubPackage.PrintFormats.VerticalDisplacementFormat :=
    rdgOutput.ItemIndex[Ord(socPrintVerticalDisplacement), FormatRow];

  SubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat :=
    rdgOutput.ItemIndex[Ord(socPrintNoDelayPreconsolidationHead), FormatRow];

  SubPackage.PrintFormats.DelayPreconsolidationHeadFormat :=
    rdgOutput.ItemIndex[Ord(socPrintDelayPreconsolidationHead), FormatRow];

  SubPackage.PrintFormats.ElasticCompactionByModelLayerFormat :=
    rdgOutput.ItemIndex[Ord(socPrintElastCompML), FormatRow];

  SubPackage.PrintFormats.InelasticCompactionByModelLayerFormat :=
    rdgOutput.ItemIndex[Ord(socPrintInelastCompML), FormatRow];

  SubPackage.PrintFormats.ElasticCompactionByInterbedSystemFormat :=
    rdgOutput.ItemIndex[Ord(socPrintElastCompIB), FormatRow];

  SubPackage.PrintFormats.InelasticCompactionByInterbedSystemFormat :=
    rdgOutput.ItemIndex[Ord(socPrintInelastCompIB), FormatRow];


  PrintChoiceCount := 0;
  if seNumExportPeriods.AsInteger = 0 then
  begin
    SubPackage.PrintChoices.Clear;
  end
  else
  begin
    for Index := FirstStateRow to rdgOutput.RowCount - 1 do
    begin
      if TryStrToFloat(rdgOutput.Cells[Ord(socStartTime),Index], StartTime)
        and TryStrToFloat(rdgOutput.Cells[Ord(socEndTime),Index], EndTime) then
      begin
        Inc(PrintChoiceCount);
        if SubPackage.PrintChoices.Count < PrintChoiceCount then
        begin
          Item := SubPackage.PrintChoices.Add as TSubPrintItem;
        end
        else
        begin
          Item := SubPackage.PrintChoices[PrintChoiceCount-1];
        end;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;
        Item.PrintSubsidence :=
          rdgOutput.Checked[Ord(socPrintSubsidence),Index];
        Item.SaveSubsidence :=
          rdgOutput.Checked[Ord(socSaveSubsidence),Index];
        Item.PrintCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socPrintCompactModelLayer),Index];
        Item.SaveCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socSaveCompactModelLayer),Index];
        Item.PrintCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socPrintCompactByInterbedSystem),Index];
        Item.SaveCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socSaveCompactByInterbedSystem),Index];
        Item.PrintVerticalDisplacement :=
          rdgOutput.Checked[Ord(socPrintVerticalDisplacement),Index];
        Item.SaveVerticalDisplacement :=
          rdgOutput.Checked[Ord(socSaveVerticalDisplacement),Index];
        Item.PrintCriticalHeadNoDelay :=
          rdgOutput.Checked[Ord(socPrintNoDelayPreconsolidationHead),Index];
        Item.SaveCriticalHeadNoDelay :=
          rdgOutput.Checked[Ord(socSaveNoDelayPreconsolidationHead),Index];
        Item.PrintCriticalHeadDelay :=
          rdgOutput.Checked[Ord(socPrintDelayPreconsolidationHead),Index];
        Item.SaveCriticalHeadDelay :=
          rdgOutput.Checked[Ord(socSaveDelayPreconsolidationHead),Index];
        Item.PrintDelayBudgets :=
          rdgOutput.Checked[Ord(socPrintDelayInterbedBudget),Index];

        Item.PrintElasticCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socPrintElastCompML),Index];
        Item.SaveElasticCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socSaveElastCompML),Index];
        Item.PrintInelasticCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socPrintInelastCompML),Index];
        Item.SaveInelasticCompactionByModelLayer :=
          rdgOutput.Checked[Ord(socSaveInelastCompML),Index];
        Item.PrintElasticCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socPrintElastCompIB),Index];
        Item.SaveElasticCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socSaveElastCompIB),Index];
        Item.PrintInelasticCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socPrintInelastCompIB),Index];
        Item.SaveInelasticCompactionByInterbedSystem :=
          rdgOutput.Checked[Ord(socSaveInelastCompIB),Index];

      end;
    end;
    while SubPackage.PrintChoices.Count > PrintChoiceCount do
    begin
      SubPackage.PrintChoices.Delete(SubPackage.PrintChoices.Count-1);
    end;
  end;
end;

procedure TframePackageSub.EnableDeleteButton;
begin
  sbDelete.Enabled := (seNumExportPeriods.AsInteger >= 1)
    and rcSelectionController.Enabled;
end;

procedure TframePackageSub.EnableLinkSubsidence;
begin
  cbLinkSubsidence.Enabled := rcSelectionController.Enabled
    and (frmGoPhast.ModelSelection = msModflowFmp);
end;

end.
