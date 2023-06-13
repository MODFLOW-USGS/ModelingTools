unit frameDeliveryGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameFormulaGridUnit, ExtCtrls,
  Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  Math, ModflowFmpFarmUnit;

type
  // @name is used for editing data set 33 in the Farm Process of MODFLOW-OWHM
  TframeDeliveryGrid = class(TframeFormulaGrid)
    lblNumberOfDeliveryTypes: TLabel;
    seNumberOfDeliveryTypes: TJvSpinEdit;
    comboHowUsed: TComboBox;
    lblHowUsed: TLabel;
    procedure seNumberOfDeliveryTypesChange(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seNumberChange(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure edFormulaChange(Sender: TObject);
    procedure comboHowUsedChange(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sbAddClick(Sender: TObject);
  private
    FChanged: boolean;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure CheckValidCell(Sender: TObject; ACol, ARow: Integer; var ValidCell: Boolean);
    procedure GetValidHowUsed(ColIndex, RowIndex: Integer; var ValidCell: Boolean);
    procedure DoChange;
    { Private declarations }
  public
    property DataChanged: Boolean read FChanged;
    procedure InitializeControls;
    // ScreenObjectList contains only objects that define farms.
    procedure GetData_OwhmV1(FarmList: TFarmList);
    procedure SetData_OwhmV1(FarmList: TFarmList);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure LayoutMultiRowEditControls; override;
    { Public declarations }
  end;

type
  TDeliveryTimeColumns = (dtcStart, dtcEnd);
  TDeliveryColumns_Owhmv1 = (dcVolume, dcRank, dcHowUsed, dcVirtualFarm);

var
  frameDeliveryGrid: TframeDeliveryGrid;

implementation

uses
  GoPhastTypes, ModflowTimeUnit, frmGoPhastUnit,
  Generics.Collections, frmCustomGoPhastUnit, ModflowPackagesUnit,
  ModflowPackageSelectionUnit;

resourcestring
  StrOnlyTheAmountRequ = 'Take required amount (0)';
  StrSurplusDischargedT = 'Surplus discharged (1)';
  StrSurplusStoredInGr = 'Surplus stored (2)';
  StrVolumeNRDV = 'Volume (NRDV) (L^3)';
  StrRateNRDV = 'Rate (NRDV) (L^3/T)';
  StrRankNRDR = 'Rank (NRDR)';
  StrHowUsedNRDU = 'How used (NRDU)';
  StrVirtualFarm0 = 'Virtual Farm (<0)';
  StrVirtualFarmNumber = 'Virtual farm number (NRDU)';

const
  DeliveryColumns = Ord(High(TDeliveryColumns_Owhmv1))+1;
var
  PickListOwhmV1: TStringList;
  PickListOwhmV2: TStringList;

{$R *.dfm}

procedure TframeDeliveryGrid.CheckValidCell(Sender: TObject; ACol,
  ARow: Integer; var ValidCell: Boolean);
begin
  ValidCell := (ARow >= 1) and (ACol > Ord(dtcEnd))
    and (((ACol-2) mod DeliveryColumns) <> Ord(dcHowUsed));
end;

procedure TframeDeliveryGrid.comboHowUsedChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
  ValidCell: Boolean;
begin
  for RowIndex := Grid.FixedRows to
    Grid.RowCount - 1 do
  begin
    for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        GetValidHowUsed(ColIndex, RowIndex, ValidCell);
        if ValidCell then
        begin
          Grid.Cells[ColIndex, RowIndex] := comboHowUsed.Text;
          if Assigned(Grid.OnSetEditText) then
          begin
            Grid.OnSetEditText(
              Grid,ColIndex,RowIndex, comboHowUsed.Text);
          end;
        end;
      end;
    end;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
end;

procedure TframeDeliveryGrid.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
  FChanged := True;
end;

procedure TframeDeliveryGrid.edFormulaChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;


procedure TframeDeliveryGrid.GetData_OwhmV1(FarmList: TFarmList);
var
  ObjectIndex: Integer;
  AFarm: TFarm;
  MaxCount: Integer;
  FirstFarm: TFarm;
  DelivItem: TDeliveryParamItem;
  OuterIndex: Integer;
  TimeIndex: Integer;
  TimeItem: TNonRoutedDeliveryParameterItem;
  ModelSelection: TModelSelection;
  DeliveryParam: TNonRoutedDeliveryParameterCollection;
begin
  ModelSelection := frmGoPhast.ModelSelection;
  if ModelSelection = msModflowFmp then
  begin
    comboHowUsed.Items := PickListOwhmV1;
  end
  else
  begin
    comboHowUsed.Items := PickListOwhmV2;
  end;
  Changing := True;
  try
    Assert(FarmList.Count > 0);
    MaxCount := 0;
    FirstFarm := FarmList[0];
    for ObjectIndex := 1 to FarmList.Count - 1 do
    begin
      AFarm := FarmList[ObjectIndex];
      if not FirstFarm.DeliveryParamCollection.IsSame(
        AFarm.DeliveryParamCollection) then
      begin
        ClearGrid;
        seNumberOfDeliveryTypes.AsInteger := 0;
        seNumberOfDeliveryTypes.OnChange(seNumberOfDeliveryTypes);
        Grid.RowCount := 2;
        seNumber.AsInteger := 0;
        seNumber.OnChange(seNumber);
        Exit;
      end;
    end;
    MaxCount := Max(MaxCount, FirstFarm.DeliveryParamCollection.Count);
    if MaxCount = 0 then
    begin
      ClearGrid;
      seNumberOfDeliveryTypes.AsInteger := 0;
      seNumberOfDeliveryTypes.OnChange(seNumberOfDeliveryTypes);
      Grid.RowCount := 2;
      seNumber.AsInteger := 0;
      seNumber.OnChange(seNumber);
      Exit;
    end;

    PestUsedOnCol[Ord(dtcStart)] := False;
    PestUsedOnCol[Ord(dtcEnd)] := False;

    seNumberOfDeliveryTypes.AsInteger := MaxCount;
    Grid.BeginUpdate;
    try
      ClearGrid;
      DelivItem := FirstFarm.DeliveryParamCollection[0];

      Grid.RowCount := Max(2, DelivItem.DeliveryParam.Count);
      seNumber.AsInteger := DelivItem.DeliveryParam.Count;
      seNumber.OnChange(seNumber);
      for OuterIndex := 0 to FirstFarm.DeliveryParamCollection.Count - 1 do
      begin
        DelivItem := FirstFarm.DeliveryParamCollection[OuterIndex];
        PestUsedOnCol[Ord(dcVolume) + OuterIndex*DeliveryColumns + 2] := True;
        PestUsedOnCol[Ord(dcRank) + OuterIndex*DeliveryColumns + 2] := False;
        PestUsedOnCol[Ord(dcHowUsed) + OuterIndex*DeliveryColumns + 2] := False;
        PestUsedOnCol[Ord(dcVirtualFarm) + OuterIndex*DeliveryColumns + 2] := False;

        DeliveryParam := DelivItem.DeliveryParam;
        PestMethod[Ord(dcVolume) + OuterIndex*DeliveryColumns + 2] := DeliveryParam.PestParamMethod;
        PestModifier[Ord(dcVolume) + OuterIndex*DeliveryColumns + 2] := DeliveryParam.PestSeriesParameter;


        for TimeIndex := 0 to DeliveryParam.Count - 1 do
        begin
          TimeItem := DeliveryParam[TimeIndex];
          Grid.Cells[Ord(dtcStart), TimeIndex+1+PestRowOffset] := FloatToStr(TimeItem.StartTime);
          Grid.Cells[Ord(dtcEnd), TimeIndex+1+PestRowOffset] := FloatToStr(TimeItem.EndTime);
          Grid.Cells[Ord(dcVolume) + OuterIndex*DeliveryColumns + 2, TimeIndex+1+PestRowOffset] := TimeItem.Volume;
          Grid.Cells[Ord(dcRank)   + OuterIndex*DeliveryColumns + 2, TimeIndex+1+PestRowOffset] := TimeItem.Rank;
          if ModelSelection = msModflowFmp then
          begin
            Grid.ItemIndex[Ord(dcHowUsed) + OuterIndex*DeliveryColumns + 2,
              TimeIndex+1+PestRowOffset] := Ord(TimeItem.NonRoutedDeliveryType);
          end
          else
          begin
            Grid.ItemIndex[Ord(dcHowUsed) + OuterIndex*DeliveryColumns + 2,
              TimeIndex+1+PestRowOffset] := Ord(TimeItem.NonRoutedDeliveryTypeOwhm2);
          end;
          Grid.Cells[Ord(dcVirtualFarm) + OuterIndex*DeliveryColumns + 2,
            TimeIndex+1+PestRowOffset] := TimeItem.VirtualFarm;
        end;
      end;
    finally
      Grid.EndUpdate;
    end;
  finally
    FChanged := False;
    Changing := False;
  end;
end;

procedure TframeDeliveryGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        GetValidHowUsed(ColIndex, RowIndex, ShouldEnable);
        if ShouldEnable then
        begin
          Break;
        end;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboHowUsed.Enabled := ShouldEnable;
  lblHowUsed.Enabled := ShouldEnable;
end;

procedure TframeDeliveryGrid.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ColumnType: TDeliveryColumns_Owhmv1;
  DelivType: TNonRoutedDeliveryType;
begin
  inherited;
  if ACol >= 2 then
  begin
    ColumnType := TDeliveryColumns_Owhmv1((ACol-2) mod DeliveryColumns);
    if ColumnType = dcVirtualFarm then
    begin
      DelivType := TNonRoutedDeliveryType(Grid.ItemIndex[ACol-1,ARow]);
      CanSelect  := DelivType = nrdtVirtualFarm;
    end;
  end;
end;

procedure TframeDeliveryGrid.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
//  UpdateNextTimeCell(Grid, ACol, ARow);
  DoChange;
end;

procedure TframeDeliveryGrid.sbAddClick(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeDeliveryGrid.sbDeleteClick(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.sbInsertClick(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.seNumberChange(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.seNumberOfDeliveryTypesChange(Sender: TObject);

var
  PickList: TStringList;
  PriorColCount: Integer;
  ColIndex: Integer;
  ColumnType: TDeliveryColumns_Owhmv1;
  Packages: TModflowPackages;
  UseVolume: Boolean;
begin
  inherited;
  PriorColCount := Grid.ColCount;
  Assert(PriorColCount >= 2);
  Grid.ColCount := seNumberOfDeliveryTypes.AsInteger * DeliveryColumns + 2;

  UseVolume := True;
  if frmGoPhast.ModelSelection = msModflowFmp then
  begin
    PickList := PickListOwhmV1;
  end
  else
  begin
    PickList := PickListOwhmV2;
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    if Packages.FarmSurfaceWater4.NRDOption = nrdoRate then
    begin
      UseVolume := False;
    end;
  end;
  comboHowUsed.Items := PickList;

  Grid.BeginUpdate;
  try
    for ColIndex := PriorColCount to Grid.ColCount - 1 do
    begin
      ColumnType := TDeliveryColumns_Owhmv1((ColIndex-2) mod DeliveryColumns);
      case ColumnType of
        dcVolume:
          begin
            if UseVolume then
            begin
              Grid.Cells[ColIndex,0] := StrVolumeNRDV;
            end
            else
            begin
              Grid.Cells[ColIndex,0] := StrRateNRDV;
            end;
            Grid.Columns[ColIndex].ButtonUsed := True;
            Grid.Columns[ColIndex].ButtonCaption := StrF;
            Grid.Columns[ColIndex].ButtonWidth := 35;
            PestUsedOnCol[ColIndex] := True;
          end;
        dcRank:
          begin
            Grid.Cells[ColIndex,0] := StrRankNRDR;
            Grid.Columns[ColIndex].ButtonUsed := True;
            Grid.Columns[ColIndex].ButtonCaption := StrF;
            Grid.Columns[ColIndex].ButtonWidth := 35;
            PestUsedOnCol[ColIndex] := False;
          end;
        dcHowUsed:
          begin
            Grid.Cells[ColIndex,0] := StrHowUsedNRDU;
            Grid.Columns[ColIndex].ComboUsed := True;
            Grid.Columns[ColIndex].LimitToList := True;
            Grid.Columns[ColIndex].PickList := PickList;
            PestUsedOnCol[ColIndex] := False;
          end;
        dcVirtualFarm:
          begin
            if  frmGoPhast.ModelSelection = msModflowFmp then
            begin
              Grid.Cells[ColIndex,0] := StrVirtualFarmNumber;
            end
            else
            begin
             Grid.Cells[ColIndex,0] := 'Infiltration Location';
            end;
            Grid.Columns[ColIndex].ButtonUsed := True;
            Grid.Columns[ColIndex].ButtonCaption := StrF;
            Grid.Columns[ColIndex].ButtonWidth := 35;
            PestUsedOnCol[ColIndex] := False;
          end;
        else
          Assert(False);
      end;
      Grid.Columns[ColIndex].AutoAdjustColWidths := True;
      Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
      Grid.Columns[ColIndex].WordWrapCaptions := True;
      Grid.Columns[ColIndex].ButtonFont := Font;
    end;
  finally
    Grid.EndUpdate;
  end;

  for ColIndex := PriorColCount to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  LayoutMultiRowEditControls;

  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.SetData_OwhmV1(FarmList: TFarmList);
var
  index: Integer;
  Farm: TFarm;
  Delivery: TDeliveryParamCollection;
  StartTimes: TList<Double>;
  EndTimes: TList<Double>;
  Rows: TGenericIntegerList;
  StartTime: double;
  EndTime: double;
  RowIndex: Integer;
  DeliveryIndex: Integer;
  DeliveryItem: TDeliveryParamItem;
  ARow: Integer;
  ColStart: Integer;
  DeliveryTimeItem: TNonRoutedDeliveryParameterItem;
  ModelSelection: TModelSelection;
  DeliveryParam: TNonRoutedDeliveryParameterCollection;
begin
  ModelSelection := frmGoPhast.ModelSelection;
  for index := 0 to FarmList.Count - 1 do
  begin
    Farm := FarmList[index];
    if Farm <> nil then
    begin
      Delivery := Farm.DeliveryParamCollection;
      while Delivery.Count < seNumberOfDeliveryTypes.AsInteger do
      begin
        Delivery.Add;
      end;
      while Delivery.Count > seNumberOfDeliveryTypes.AsInteger do
      begin
        Delivery.Last.Free;
      end;
      StartTimes := TList<Double>.Create;
      EndTimes := TList<Double>.Create;
      Rows := TGenericIntegerList.Create;
      try
        for RowIndex := 1 to seNumber.AsInteger do
        begin
          if TryStrToFloat(Grid.Cells[Ord(dtcStart), RowIndex+PestRowOffset], StartTime)
            and TryStrToFloat(Grid.Cells[Ord(dtcEnd), RowIndex+PestRowOffset], EndTime) then
          begin
            Rows.Add(RowIndex+PestRowOffset);
            StartTimes.Add(StartTime);
            EndTimes.Add(EndTime);
          end;
        end;
        for DeliveryIndex := 0 to seNumberOfDeliveryTypes.AsInteger - 1 do
        begin
          DeliveryItem := Delivery[DeliveryIndex];
          ColStart := DeliveryIndex*DeliveryColumns+2;
          for RowIndex := 0 to Rows.Count-1 do
          begin
            ARow := Rows[RowIndex];
            if RowIndex < DeliveryItem.DeliveryParam.Count then
            begin
              DeliveryTimeItem := DeliveryItem.DeliveryParam[RowIndex];
            end
            else
            begin
              DeliveryTimeItem := DeliveryItem.DeliveryParam.Add;
            end;
            DeliveryTimeItem.StartTime := StartTimes[RowIndex];
            DeliveryTimeItem.EndTime := EndTimes[RowIndex];
            DeliveryTimeItem.Volume := Grid.Cells[ColStart + Ord(dcVolume),ARow];
            DeliveryTimeItem.Rank := Grid.Cells[ColStart + Ord(dcRank),ARow];
            if ModelSelection = msModflowFmp then
            begin
              DeliveryTimeItem.NonRoutedDeliveryType :=
                TNonRoutedDeliveryType(Max(0, Grid.ItemIndex[ColStart + Ord(dcHowUsed),ARow]));
              if DeliveryTimeItem.NonRoutedDeliveryType = nrdtVirtualFarm then
              begin
                DeliveryTimeItem.VirtualFarm := Grid.Cells[ColStart + Ord(dcVirtualFarm),ARow];
              end;
            end
            else
            begin
              DeliveryTimeItem.NonRoutedDeliveryTypeOwhm2 :=
                TNonRoutedDeliveryTypeOwhm2(Max(0, Grid.ItemIndex[ColStart + Ord(dcHowUsed),ARow]));
              if DeliveryTimeItem.NonRoutedDeliveryTypeOwhm2 = nrdt2Infiltrate then
              begin
                DeliveryTimeItem.VirtualFarm := Grid.Cells[ColStart + Ord(dcVirtualFarm),ARow];
              end;
            end;
          end;
          while DeliveryItem.DeliveryParam.Count > Rows.Count do
          begin
            DeliveryItem.DeliveryParam.Last.Free;
          end;
        end;
      finally
        StartTimes.Free;
        EndTimes.Free;
        Rows.Free;
      end;
      for DeliveryIndex := 0 to seNumberOfDeliveryTypes.AsInteger - 1 do
      begin
        DeliveryItem := Delivery[DeliveryIndex];
        ColStart := DeliveryIndex*DeliveryColumns+2;
        DeliveryParam := DeliveryItem.DeliveryParam;
        if PestMethodAssigned[ColStart+Ord(dcVolume)] then
        begin
          DeliveryParam.PestParamMethod := PestMethod[ColStart+Ord(dcVolume)];
        end;
        if PestModifierAssigned[ColStart+Ord(dcVolume)] then
        begin
          DeliveryParam.PestSeriesParameter := PestModifier[ColStart+Ord(dcVolume)];
        end;
      end;
    end;
  end;
end;

procedure TframeDeliveryGrid.GetValidHowUsed(ColIndex, RowIndex: Integer;
  var ValidCell: Boolean);
begin
  ValidCell := (RowIndex >= 1) and (ColIndex > Ord(dtcEnd))
    and (((ColIndex - 2) mod DeliveryColumns) = Ord(dcHowUsed));
end;

procedure TframeDeliveryGrid.InitializeControls;
var
  StressPeriods: TModflowStressPeriods;
begin
  FirstFormulaColumn := Succ(Ord(dtcEnd));
  IncludePestAdjustment := True;
  InitializePestParameters;
  ClearGrid;
  OnValidCell := CheckValidCell;
  Grid.Cells[Ord(dtcStart), 0] := StrStartingTime;
  Grid.Cells[Ord(dtcEnd), 0] := StrEndingTime;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
  StressPeriods.FillPickListWithStartTimes(Grid, Ord(dtcStart));
  StressPeriods.FillPickListWithEndTimes(Grid, Ord(dtcEnd));
  seNumberOfDeliveryTypes.AsInteger := 0;
  seNumber.AsInteger := 0;
  seNumberChange(nil);
  LayoutMultiRowEditControls;
end;

procedure TframeDeliveryGrid.LayoutMultiRowEditControls;
var
  Column: integer;
  ColIndex: Integer;
  ValidCell: Boolean;
begin
  inherited;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Column := Max(FirstFormulaColumn,Grid.LeftCol);
  for ColIndex := Column to Grid.ColCount - 1 do
  begin
    GetValidHowUsed(ColIndex,1,ValidCell);
    if ValidCell then
    begin
      Column := ColIndex;
      break;
    end;
  end;
  LayoutControls(Grid, comboHowUsed, lblHowUsed,
    Column);
end;

initialization

  PickListOwhmV1 := TStringList.Create;
  PickListOwhmV1.Add(StrOnlyTheAmountRequ);
  PickListOwhmV1.Add(StrSurplusDischargedT);
  PickListOwhmV1.Add(StrSurplusStoredInGr);
  PickListOwhmV1.Add(StrVirtualFarm0);

  PickListOwhmV2 := TStringList.Create;
  PickListOwhmV2.Add(StrOnlyTheAmountRequ);
  PickListOwhmV2.Add(StrSurplusDischargedT);
  PickListOwhmV2.Add(StrSurplusStoredInGr);

  // The infiltration location option has not been implemented as of
  // MODFLOW-OWHM version 4.3. uncomment the next line if it is activated.
  // A change would also need to be made in
  // TframePackageFmp4SurfaceWater.rdgSurfaceWaterSelectCell.

//  PickListOwhmV2.Add('Infiltration Location');

finalization

  PickListOwhmV1.Free;
  PickListOwhmV2.Free;

end.

