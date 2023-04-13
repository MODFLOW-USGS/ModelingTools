unit frameScreenObjectCSubUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectTabbedUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects, System.UITypes;

type
  TInterbedColumns = (icName, icUsed, icInitialOffset, icThickness,
    icEquivInterbedNumber, icInitialInelasticSpecificStorage,
    icInitialElasticSpecificStorage, icInitialPorosity, icDelayKv,
    icInitialDelayHeadOffset);

  TStressColumns = (scStartingTime, scEndingTime, scStressOffset);

  TframeScreenObjectCSub = class(TframeScreenObjectTabbed)
    tabInterbedSystems: TTabSheet;
    rdgSubGroups: TRbwDataGrid4;
    pnlEditPkgProp: TPanel;
    lblFormulaPkgProp: TLabel;
    rdeFormulaPkgProp: TRbwDataEntry;
    cbUsedPkgProp: TCheckBox;
    procedure rdgSubGroupsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgSubGroupsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rdgSubGroupsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgSubGroupsHorizontalScroll(Sender: TObject);
    procedure rdgSubGroupsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbUsedPkgPropClick(Sender: TObject);
    procedure rdeFormulaPkgPropChange(Sender: TObject);
    procedure rdgSubGroupsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure tabInterbedSystemsShow(Sender: TObject);
  private
//    FPackageDataCleared: Boolean;
    FTimeDataCleared: Boolean;
    FInterbedsDefined: Boolean;
    procedure InitializeControls;
    procedure LayoutMultiRowEditControlsPkgProp;
    { Private declarations }
  public
    procedure GetData(const List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectCSub: TframeScreenObjectCSub;

implementation

uses
  ModflowPackageSelectionUnit, frmGoPhastUnit, ScreenObjectUnit,
  ModflowCsubUnit, GoPhastTypes, System.Math, frmCustomGoPhastUnit,
  ModflowCSubInterbed, PhastModelUnit, DataSetUnit, DataArrayManagerUnit,
  DataSetNamesUnit;

resourcestring
  StrInterbed = 'Interbed system';
  StrUsed = 'Used';
  StrInitialOffset = 'Initial Stress Offset (pcs0)';
  StrThickness = 'Interbed Thickness (thick_frac)';
  StrEquivalentInterbed = 'Equivalent Interbed Number (rnb)';
  StrInitialInelasticS = ' Initial Inelastic Specific Storage (ssv_cc)';
  StrInitialElasticSpec = 'Initial Elastic Specific Storage (sse_cr)';
  StrInitialPorosity = 'Initial Porosity (theta)';
  StrDelayKv = 'Delay Kv (kv)';
  StrInitialDelayHeadO = 'Initial Delay Head Offset (h0)';
  StrStressOffset = 'Stress Offset (sig0)';
  StrInitialPreconsolida = 'Initial Preconsolidation Stress (pcs0)';
  StrInterbedCellFracti = 'Interbed Cell Fraction (thick_frac)';
  StrInitialCompression = 'Initial Inelastic Compression Index (ssv_cc)';
  StrInitialElasticComp = 'Initial Elastic Recompression Index (sse_cr)';
  StrInitialDelayHead = 'Initial Delay Head (h0)';
  StrNoInterbedsHaveBe = 'No interbeds have been defined for the CSUB packag' +
  'e in the MODFLOW Packages and Programs dialog box.';

{$R *.dfm}

{ TframeScreenObjectCSub }

procedure TframeScreenObjectCSub.cbUsedPkgPropClick(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    if rdgSubGroups.IsSelectedCell(Ord(icUsed),RowIndex) then
    begin
      rdgSubGroups.Checked[Ord(icUsed),RowIndex] := cbUsedPkgProp.Checked;
    end;
  end;
end;

procedure TframeScreenObjectCSub.GetData(
  const List: TScreenObjectEditCollection);
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  ModflowCSub: TCSubBoundary;
  FoundFirst: Boolean;
  InterbedIndex: Integer;
  Interbed: TCSubPackageData;
  RowIndex: Integer;
  FirstCSub: TCSubBoundary;
  Item: TCSubItem;
begin
  FGettingData := True;
  try
    InitializeControls;

    rdgSubGroups.BeginUpdate;
    try
      FTimeDataCleared := False;
      FoundFirst := False;
      FirstCSub := nil;
      for ScreenObjectIndex := 0 to List.Count - 1 do
      begin
        ScreenObject := List[ScreenObjectIndex].ScreenObject;
        ModflowCSub := ScreenObject.ModflowCSub;
        if (ModflowCSub <> nil) and ModflowCSub.Used then
        begin
          if not FoundFirst then
          begin
            FoundFirst := True;
            FirstCSub := ModflowCSub;

            PestModifier[CsubStressOffsetPosition + PestRowOffset] :=
              ModflowCSub.PestStressOffsetFormula;
            PestMethod[CsubStressOffsetPosition + PestRowOffset] :=
              ModflowCSub.PestStressOffsetMethod;

            for InterbedIndex := 0 to ModflowCSub.CSubPackageData.Count -1 do
            begin
              Interbed := ModflowCSub.CSubPackageData[InterbedIndex];
              RowIndex := rdgSubGroups.Cols[Ord(icName)].IndexOf(Interbed.InterbedSystemName);
              if RowIndex >= 1 then
              begin
                rdgSubGroups.Checked[Ord(icUsed), RowIndex] := Interbed.Used;
                rdgSubGroups.Cells[Ord(icInitialOffset), RowIndex] :=
                  Interbed.InitialOffset;
                rdgSubGroups.Cells[Ord(icThickness), RowIndex] :=
                  Interbed.Thickness;
                rdgSubGroups.Cells[Ord(icEquivInterbedNumber), RowIndex] :=
                  Interbed.EquivInterbedNumber;
                rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), RowIndex] :=
                  Interbed.InitialInelasticSpecificStorage;
                rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), RowIndex] :=
                  Interbed.InitialElasticSpecificStorage;
                rdgSubGroups.Cells[Ord(icInitialPorosity), RowIndex] :=
                  Interbed.InitialPorosity;
                rdgSubGroups.Cells[Ord(icDelayKv), RowIndex] := Interbed.DelayKv;
                rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), RowIndex] :=
                  Interbed.InitialDelayHeadOffset;
              end;
            end;

            rdgModflowBoundary.BeginUpdate;
            try
              seNumberOfTimes.AsInteger := ModflowCSub.Values.Count;
              for RowIndex := 1 to ModflowCSub.Values.Count do
              begin
                Item := ModflowCSub.Values[RowIndex-1] as TCSubItem;
                rdgModflowBoundary.Cells[Ord(scStartingTime), RowIndex+PestRowOffset] :=
                  FloatToStr(Item.StartTime);
                rdgModflowBoundary.Cells[Ord(scEndingTime), RowIndex+PestRowOffset] :=
                  FloatToStr(Item.EndTime);
                rdgModflowBoundary.Cells[Ord(scStressOffset), RowIndex+PestRowOffset] :=
                  Item.StressOffset;
              end;
            finally
              rdgModflowBoundary.EndUpdate;
            end;
          end
          else
          begin
            if ModflowCSub.PestStressOffsetFormula <> FirstCSub.PestStressOffsetFormula then
            begin
              PestModifierAssigned[CsubStressOffsetPosition + PestRowOffset] := False;
            end;
            if ModflowCSub.PestStressOffsetMethod <> FirstCSub.PestStressOffsetMethod then
            begin
              PestMethodAssigned[CsubStressOffsetPosition + PestRowOffset] := False;
            end;

            if not ModflowCSub.CSubPackageData.IsSame(FirstCSub.CSubPackageData) then
            begin
              for InterbedIndex := 0 to ModflowCSub.CSubPackageData.Count -1 do
              begin
                Interbed := ModflowCSub.CSubPackageData[InterbedIndex];
                RowIndex := rdgSubGroups.Cols[Ord(icName)].IndexOf(Interbed.InterbedSystemName);
                if RowIndex >= 1 then
                begin
                  if rdgSubGroups.Checked[Ord(icUsed), RowIndex] <> Interbed.Used then
                  begin
                    rdgSubGroups.CheckState[Ord(icUsed), RowIndex] := cbGrayed;
                  end;
                  if rdgSubGroups.Cells[Ord(icInitialOffset), RowIndex] <> Interbed.InitialOffset then
                  begin
                    rdgSubGroups.Cells[Ord(icInitialOffset), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icThickness), RowIndex] <> Interbed.Thickness then
                  begin
                    rdgSubGroups.Cells[Ord(icThickness), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icEquivInterbedNumber), RowIndex] <> Interbed.EquivInterbedNumber then
                  begin
                    rdgSubGroups.Cells[Ord(icEquivInterbedNumber), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), RowIndex] <> Interbed.InitialInelasticSpecificStorage then
                  begin
                    rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), RowIndex] <> Interbed.InitialElasticSpecificStorage then
                  begin
                    rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icInitialPorosity), RowIndex] <> Interbed.InitialPorosity then
                  begin
                    rdgSubGroups.Cells[Ord(icInitialPorosity), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icDelayKv), RowIndex] <> Interbed.DelayKv then
                  begin
                    rdgSubGroups.Cells[Ord(icDelayKv), RowIndex] := '';
                  end;
                  if rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), RowIndex] <> Interbed.InitialDelayHeadOffset then
                  begin
                    rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), RowIndex] := '';
                  end;
                end;
              end;
            end;
            if not ModflowCSub.Values.IsSame(FirstCSub.Values) then
            begin
              ClearGrid(rdgModflowBoundary);
              FTimeDataCleared := True;
            end;
          end;
        end;
      end;
    finally
      rdgSubGroups.EndUpdate;
    end;
  finally
    FGettingData := False;
  end;

end;

procedure TframeScreenObjectCSub.InitializeControls;
var
  Interbeds: TCSubInterbeds;
  ColIndex: Integer;
  RowIndex: Integer;
  CSubPackage: TCSubPackageSelection;
begin
  FInterbedsDefined := True;
  pcMain.ActivePageIndex := 0;
  rdgSubGroups.FixedCols := 1;
  CSubPackage := frmGoPhast.PhastModel.ModflowPackages.CSubPackage;
  Interbeds := CSubPackage.Interbeds;
  if Interbeds.Count = 0 then
  begin
    FInterbedsDefined := False;
    Beep;
    MessageDlg(StrNoInterbedsHaveBe, mtWarning, [mbOK], 0);
  end;
  ClearGrid(rdgSubGroups);
  rdgSubGroups.BeginUpdate;
  try
    for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
    begin
      rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := True;
    end;
    rdgSubGroups.RowCount := Max(Interbeds.Count + 1, 2);
    for RowIndex := 1 to Interbeds.Count do
    begin
      rdgSubGroups.Cells[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1].Name;
      rdgSubGroups.Objects[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1];
    end;

    rdgSubGroups.Cells[Ord(icName), 0] := StrInterbed;
    rdgSubGroups.Cells[Ord(icUsed), 0] := StrUsed;

    if CSubPackage.SpecifyInitialPreconsolidationStress then
    begin
      rdgSubGroups.Cells[Ord(icInitialOffset), 0] := StrInitialPreconsolida;
    end
    else
    begin
      rdgSubGroups.Cells[Ord(icInitialOffset), 0] := StrInitialOffset;
    end;
    
    if CSubPackage.InterbedThicknessMethod = itmThickness then
    begin
      rdgSubGroups.Cells[Ord(icThickness), 0] := StrThickness;
    end
    else
    begin
      rdgSubGroups.Cells[Ord(icThickness), 0] := StrInterbedCellFracti;
    end;
    
    rdgSubGroups.Cells[Ord(icEquivInterbedNumber), 0] := StrEquivalentInterbed;
    
    if CSubPackage.CompressionMethod = coRecompression then
    begin
      rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), 0] := StrInitialCompression;
    end
    else
    begin
      rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), 0] := StrInitialInelasticS;
    end;

    if CSubPackage.CompressionMethod = coRecompression then
    begin
      rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), 0] := StrInitialElasticComp;
    end
    else
    begin
      rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), 0] := StrInitialElasticSpec;
    end;

    rdgSubGroups.Cells[Ord(icInitialPorosity), 0] := StrInitialPorosity;
    rdgSubGroups.Cells[Ord(icDelayKv), 0] := StrDelayKv;

    if CSubPackage.SpecifyInitialDelayHead then
    begin
      rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), 0] := StrInitialDelayHead;
    end
    else
    begin
      rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), 0] := StrInitialDelayHeadO;
    end;
  finally
    rdgSubGroups.EndUpdate;
  end;

  for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
  begin
    rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  ClearGrid(rdgModflowBoundary);

  seNumberOfTimes.AsInteger := 0;
  seNumberOfTimes.OnChange(seNumberOfTimes);

  rdgModflowBoundary.BeginUpdate;
  try
    rdgModflowBoundary.Cells[Ord(scStartingTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(scEndingTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(scStressOffset), 0] := StrStressOffset;
    FillPicklistsWithStartTimes;

    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
    PestMethod[CsubStressOffsetPosition + PestRowOffset] :=
      TCSubBoundary.DefaultBoundaryMethod(CsubStressOffsetPosition);
  finally
    rdgModflowBoundary.EndUpdate;
  end;

  LayoutMultiRowEditControls
end;

procedure TframeScreenObjectCSub.LayoutMultiRowEditControlsPkgProp;
var
  FormulaColumn: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FormulaColumn := Max(Ord(icInitialOffset),rdgModflowBoundary.LeftCol);
  LayoutControls(rdgSubGroups, rdeFormulaPkgProp, lblFormulaPkgProp, FormulaColumn);

  FormulaColumn := Ord(icUsed);
  LayoutControls(rdgSubGroups, cbUsedPkgProp, nil, FormulaColumn);
end;

procedure TframeScreenObjectCSub.rdeFormulaPkgPropChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    for ColIndex := Ord(icInitialOffset) to rdgSubGroups.ColCount do
    begin
      if rdgSubGroups.IsSelectedCell(ColIndex,RowIndex) then
      begin
        rdgSubGroups.Cells[ColIndex,RowIndex] := rdeFormulaPkgProp.Text;
      end;
    end;
  end;

end;

procedure TframeScreenObjectCSub.rdgSubGroupsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControlsPkgProp;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControlsPkgProp;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    for ColIndex := Ord(icInitialOffset) to rdgSubGroups.ColCount do
    begin
      ShouldEnable := rdgSubGroups.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeFormulaPkgProp.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    ShouldEnable := rdgSubGroups.IsSelectedCell(Ord(icUsed),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbUsedPkgProp.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Interbed: TCSubInterbed;
begin
  inherited;
  if not FInterbedsDefined then
  begin
    CanSelect := False;
    Exit;
  end;
  if (ARow >= 1) and (ACol >= Ord(icInitialOffset)) then
  begin
    CanSelect := rdgSubGroups.CheckState[Ord(icUsed), ARow] <> cbUnchecked;
    if CanSelect
      and (TInterbedColumns(ACol) in [icEquivInterbedNumber, icDelayKv, icInitialDelayHeadOffset]) then
    begin
      Interbed := rdgSubGroups.Objects[Ord(icName), ARow] as TCSubInterbed;
      CanSelect := (Interbed <> nil) and (Interbed.InterbedType = itDelay);
    end;
//
  end;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCSub.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TCSubBoundary;
  BoundaryUsed: Boolean;
  Interbeds: TCSubInterbeds;
  CSubPackageData: TCSubPackageDataCollection;
  CSubValues: TCSubCollection;
  AnInterBed: TCSubInterbed;
  InterBedSystem: TCSubPackageData;
  Formula: string;
  TimeIndex: Integer;
  StartTime: double;
  EndTime: double;
  CSubItem: TCSubItem;
  SubDataSets: TStringList;
  DsIndex: Integer;
  DataArrayManager: TDataArrayManager;
  ADataArray: TDataArray;
  PackageDataIndex: Integer;
  DataSetIndex: Integer;
  IbIndex: Integer;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  Interbeds := frmGoPhast.PhastModel.ModflowPackages.CSubPackage.Interbeds;
  CSubPackageData := TCSubPackageDataCollection.Create(nil, nil);
  CSubValues := TCSubCollection.Create(nil, nil, nil);
  try
  for Index := 0 to Interbeds.Count -1 do
  begin
    AnInterBed := Interbeds[Index];
    InterBedSystem := CSubPackageData.Add;
    InterBedSystem.Interbed := AnInterBed;
    InterBedSystem.Used := rdgSubGroups.Checked[Ord(icUsed), Index+1];
//    if InterBedSystem.Used then
//    begin
//      FPackageDataCleared := False;
//    end;

    Formula := rdgSubGroups.Cells[Ord(icInitialOffset), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.InitialOffset := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icThickness), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.Thickness := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icEquivInterbedNumber), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.EquivInterbedNumber := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.InitialInelasticSpecificStorage := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.InitialElasticSpecificStorage := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialPorosity), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.InitialPorosity := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icDelayKv), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.DelayKv := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), Index+1];
//    if Formula <> '' then
    begin
      InterBedSystem.InitialDelayHeadOffset := Formula;
    end;
  end;

  for TimeIndex := 0 to seNumberOfTimes.AsInteger -1 do
  begin
    if TryStrToFloat(rdgModflowBoundary.Cells[Ord(scStartingTime), TimeIndex+1+PestRowOffset], StartTime)
      and TryStrToFloat(rdgModflowBoundary.Cells[Ord(scEndingTime), TimeIndex+1+PestRowOffset], EndTime)
      and (rdgModflowBoundary.Cells[Ord(scStressOffset), TimeIndex+1+PestRowOffset] <> '') then
    begin
      CSubItem := CSubValues.Add;
      CSubItem.StartTime := StartTime;
      CSubItem.EndTime := EndTime;
      CSubItem.StressOffset := rdgModflowBoundary.Cells[Ord(scStressOffset), TimeIndex+1+PestRowOffset];
    end;
  end;

//  TStressColumns = (scStartingTime, scEndingTime, scStressOffset);
  
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowCSub;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;
    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.Clear;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if SetAll and (Boundary = nil) then
      begin
        Item.ScreenObject.CreateCSubBoundary;
        Boundary := Item.ScreenObject.ModflowCSub;
        Boundary.CSubPackageData := CSubPackageData;
      end;
      
      if (Boundary <> nil) then
      begin
        if PestModifierAssigned[CsubStressOffsetPosition + PestRowOffset] then
        begin
          Boundary.PestStressOffsetFormula := PestModifier[CsubStressOffsetPosition + PestRowOffset]
        end;
        if PestMethodAssigned[CsubStressOffsetPosition + PestRowOffset] then
        begin
          Boundary.PestStressOffsetMethod := PestMethod[CsubStressOffsetPosition + PestRowOffset]
        end;

        Boundary.CSubPackageData := CSubPackageData;
        for PackageDataIndex := 0 to Boundary.CSubPackageData.Count - 1 do
        begin
          InterBedSystem := Boundary.CSubPackageData[PackageDataIndex];
          AnInterBed := Interbeds.GetInterbedByName(InterBedSystem.InterbedSystemName);
          Assert(AnInterBed <> nil);
          if InterBedSystem.Used then
          begin
            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.DelayKvName);
            if AnInterBed.InterbedType = itDelay then
            begin
              Assert(ADataArray <> nil);
              DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
              Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.DelayKv;
            end
            else
            begin
              if ADataArray <> nil then
              begin
                Item.ScreenObject.RemoveDataSet(ADataArray)
              end;
            end;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.EquivInterbedNumberName);
            if AnInterBed.InterbedType = itDelay then
            begin
              Assert(ADataArray <> nil);
              DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
              Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.EquivInterbedNumber;
            end
            else
            begin
              if ADataArray <> nil then
              begin
                Item.ScreenObject.RemoveDataSet(ADataArray)
              end;
            end;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialDelayHeadOffset);
            if AnInterBed.InterbedType = itDelay then
            begin
              Assert(ADataArray <> nil);
              DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
              Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.InitialDelayHeadOffset;
            end
            else
            begin
              if ADataArray <> nil then
              begin
                Item.ScreenObject.RemoveDataSet(ADataArray)
              end;
            end;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialElasticSpecificStorage);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.InitialElasticSpecificStorage;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialInelasticSpecificStorage);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.InitialInelasticSpecificStorage;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialOffset);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.InitialOffset;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialPorosity);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.InitialPorosity;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.Thickness);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := InterBedSystem.Thickness;

            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.CSubBoundName);
            Assert(ADataArray <> nil);
            DataSetIndex := Item.ScreenObject.AddDataSet(ADataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := Format('"%s"', [Item.ScreenObject.Name]);
          end
          else
          begin
            SubDataSets := AnInterBed.DataSetNames;
            for DsIndex := 0 to SubDataSets.Count - 1 do
            begin
              ADataArray := DataArrayManager.GetDataSetByName(SubDataSets[DsIndex]);
              if ADataArray <> nil then
              begin
                Item.ScreenObject.RemoveDataSet(ADataArray)
              end;
            end;
          end;
        end;

        if (not FTimeDataCleared) or (CSubValues.Count > 0) then
        begin
          Boundary.Values := CSubValues;
        end;
      end
      else
      begin
        for IbIndex := 0 to Interbeds.Count -1 do
        begin
          AnInterBed := Interbeds[IbIndex];
          SubDataSets := AnInterBed.DataSetNames;
          for DsIndex := 0 to SubDataSets.Count - 1 do
          begin
            ADataArray := DataArrayManager.GetDataSetByName(SubDataSets[DsIndex]);
            if ADataArray <> nil then
            begin
              Item.ScreenObject.RemoveDataSet(ADataArray)
            end;
          end;
        end;

      end;
    end;
  end;
  finally
    CSubPackageData.Free;
    CSubValues.Free;
  end;
end;

procedure TframeScreenObjectCSub.tabInterbedSystemsShow(Sender: TObject);
begin
  inherited;
  rdgSubGroups.HideEditor;
end;

end.
