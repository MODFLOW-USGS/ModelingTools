unit frameSutraBoundaryUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameCustomSutraFeatureUnit,
  ArgusDataEntry, StdCtrls, Grids, RbwDataGrid4, Buttons, Mask,
  JvExMask, JvSpin, ExtCtrls, UndoItemsScreenObjects, SutraBoundariesUnit,
  GoPhastTypes, SutraTimeScheduleUnit, SutraBoundaryUnit;

type
  TSutraBoundaryGridColumns = (sbgtTime, sbgtUsed, sbgtVariable1, sbgtVariable2);

  TframeSutraBoundary = class(TframeCustomSutraTimeVaryingFeature)
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    lblFluidSourceInLakesPresent: TLabel;
    comboFluidSourceInLakesPresent: TComboBox;
    procedure edNameChange(Sender: TObject);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure comboScheduleChange(Sender: TObject);
    procedure edNameExit(Sender: TObject);
    procedure rdgSutraFeatureColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgSutraFeatureHorizontalScroll(Sender: TObject);
    procedure rdgSutraFeatureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgSutraFeatureSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgSutraFeatureBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgSutraFeatureEndUpdate(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
  private
    FInitialTime: Double;
    FBoundaryType: TSutraBoundaryType;
    FBoundariesTheSame: Boolean;
    procedure GetScheduleName(BoundaryList: TSutraBoundaryList);
    procedure GetLakeEffect(BoundaryList: TSutraBoundaryList);
    procedure DisplayBoundaries(BoundColl: TCustomSutraBoundaryCollection);
    procedure SetBoundaryType(const Value: TSutraBoundaryType);
    procedure InitializeColumnHeadings;
    procedure GetBoundaryValues(BoundaryList: TSutraBoundaryList);
    procedure SetBoundaryValues(BoundValues: TCustomSutraBoundaryCollection);
    procedure LayoutMultiEditControls;
    function GetValidTime(ACol, ARow: integer): Boolean;
    procedure UpdateColWidths;
    { Private declarations }
  protected
//    procedure ClearBoundaries;
    function UsedColumn: Integer; override;
  public
    property BoundaryType: TSutraBoundaryType read FBoundaryType write SetBoundaryType;
    procedure GetData(ScreenObjects: TScreenObjectEditCollection); override;
    procedure SetData(ScreenObjects: TScreenObjectEditCollection; SetAll,
      ClearAll: boolean); override;
    { Public declarations }
  end;

var
  frameSutraBoundary: TframeSutraBoundary;

implementation

uses
  ModflowBoundaryUnit, Generics.Collections, ScreenObjectUnit,
  frmGoPhastUnit,
  RealListUnit, frmCustomGoPhastUnit,
  SutraOptionsUnit, frmErrorsAndWarningsUnit, AdjustSutraBoundaryValuesUnit;

resourcestring
  StrFluidSource = 'Fluid source';
  StrAssociatedConcentra = 'Associated concentration';
  StrAssociatedTemp = 'Associated temperature';
  StrSoluteSource = 'Solute source';
  StrEnergySouce = 'Energy source';
  StrSpecifiedPressure = 'Specified pressure';
  StrSutraSpecifiedHead = 'Specified head';
  StrSpecifiedTemperatur = 'Specified temperature';
  StrSpecifiedConcentration = 'Specified concentration';
  StrTime = 'Time';
  StrUsed = 'Used';
  StrYouMustSpecifyAT = 'You must specify a time that is greater than or equ' +
  'al to the initial time.';
  StrYouMustSpecifyG = 'You must specify a time that is greater than ' +
  'the initial time.';

{$R *.dfm}

{ TframeCustomSutraBoundary }


procedure TframeSutraBoundary.GetData(
  ScreenObjects: TScreenObjectEditCollection);
var
  index: Integer;
  BoundaryList: TSutraBoundaryList;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraBoundary;
begin
  rdgSutraFeature.Columns[Ord(sbgtUsed)].Format := rcf4String;
  rdgSutraFeature.Columns[Ord(sbgtUsed)].ButtonUsed := True;
  rdgSutraFeature.Columns[Ord(sbgtUsed)].ButtonCaption := 'F()';
  rdgSutraFeature.Columns[Ord(sbgtUsed)].ButtonWidth := 35;
  comboFluidSourceInLakesPresent.ItemIndex := Ord(lbiNoChange);

  rdgSutraFeature.BeginUpdate;
  try
    inherited;
    ClearData;

    FInitialTime := frmGoPhast.PhastModel.SutraTimeOptions.InitialTime;
    FGettingData := True;
    BoundaryList := TSutraBoundaryList.Create;
    try
      for index := 0 to ScreenObjects.Count - 1 do
      begin
        ABoundary := nil;
        SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
        case BoundaryType of
          sbtFluidSource:
            begin
              ABoundary := SutraBoundaries.FluidSource;
            end;
          sbtMassEnergySource:
            begin
              ABoundary := SutraBoundaries.MassEnergySource;
            end;
          sbtSpecPress:
            begin
              ABoundary := SutraBoundaries.SpecifiedPressure;
            end;
          sbtSpecConcTemp:
            begin
              ABoundary := SutraBoundaries.SpecifiedConcTemp;
            end;
          else
            Assert(False);
        end;
        if ABoundary.Used then
        begin
          BoundaryList.Add(ABoundary);
        end;
      end;

      if BoundaryList.Count = 0 then
      begin
        FCheckState := cbUnchecked;
      end
      else if ScreenObjects.Count = BoundaryList.Count then
      begin
        FCheckState := cbChecked;
      end
      else
      begin
        FCheckState := cbGrayed;
      end;
      if Assigned(OnActivate) then
      begin
        OnActivate(self, FCheckState);
      end;

      if BoundaryList.Count = 0 then
      begin
        Exit;
      end;
      GetScheduleName(BoundaryList);
      GetLakeEffect(BoundaryList);
      GetBoundaryValues(BoundaryList);
  //    comboScheduleChange(nil);
  //    CheckSchedule(BoundaryList);


    finally
      BoundaryList.Free;
      FGettingData := False;
    end;
    comboScheduleChange(nil);
  finally
    rdgSutraFeature.EndUpdate;
  end;
end;

procedure TframeSutraBoundary.GetLakeEffect(BoundaryList: TSutraBoundaryList);
var
//  ScheduleName: AnsiString;
  Same: Boolean;
  FirstBoundary: TSutraBoundary;
  ABoundColl: TCustomSutraBoundaryCollection;
  BoundColl: TCustomSutraBoundaryCollection;
  Index: Integer;
  ABoundary: TSutraBoundary;
  LakeInteraction: TLakeBoundaryInteraction;
begin
  FirstBoundary := BoundaryList[0];
//  BoundColl := FirstBoundary.Values as TCustomSutraBoundaryCollection;
  LakeInteraction := FirstBoundary.LakeInteraction;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
//    ABoundColl := ABoundary.Values as TCustomSutraBoundaryCollection;
    Same := LakeInteraction = ABoundary.LakeInteraction;
    if not Same then
    begin
      Break;
    end;
  end;
  if Same then
  begin
    comboFluidSourceInLakesPresent.ItemIndex := Ord(LakeInteraction);
  end
  else
  begin
    comboFluidSourceInLakesPresent.ItemIndex := -1;
  end;
end;

procedure TframeSutraBoundary.SetBoundaryType(
  const Value: TSutraBoundaryType);
begin
  FBoundaryType := Value;
  case FBoundaryType of
    sbtFluidSource:
      begin
        rdgSutraFeature.ColCount := 4;
      end;
    sbtMassEnergySource:
      begin
        rdgSutraFeature.ColCount := 3;
      end;
    sbtSpecPress:
      begin
        rdgSutraFeature.ColCount := 4;
      end;
    sbtSpecConcTemp:
      begin
        rdgSutraFeature.ColCount := 3;
      end;
    else Assert(False);
  end;
  InitializeColumnHeadings;

end;

procedure TframeSutraBoundary.SetData(
  ScreenObjects: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  BoundaryList: TSutraBoundaryList;
  index: integer;
  SutraBoundaries: TSutraBoundaries;
  ABoundary: TSutraBoundary;
  LocalScreenObjects: TList<TScreenObject>;
//  BoundaryName: string;
  BoundValues: TCustomSutraBoundaryCollection;
begin
  inherited;
  LocalScreenObjects := TList<TScreenObject>.Create;
  BoundaryList := TSutraBoundaryList.Create;
  try
    for index := 0 to ScreenObjects.Count - 1 do
    begin
      ABoundary := nil;
      SutraBoundaries := ScreenObjects[index].ScreenObject.SutraBoundaries;
      case BoundaryType of
        sbtFluidSource:
          begin
            ABoundary := SutraBoundaries.FluidSource;
          end;
        sbtMassEnergySource:
          begin
            ABoundary := SutraBoundaries.MassEnergySource;
          end;
        sbtSpecPress:
          begin
            ABoundary := SutraBoundaries.SpecifiedPressure;
          end;
        sbtSpecConcTemp:
          begin
            ABoundary := SutraBoundaries.SpecifiedConcTemp;
          end;
        else
          Assert(False);
      end;
      if ClearAll then
      begin
        ABoundary.Values.Clear;
      end
      else if SetAll or ABoundary.Used then
      begin
        BoundaryList.Add(ABoundary);
        LocalScreenObjects.Add(ScreenObjects[index].ScreenObject);
      end;
    end;

    for index := 0 to BoundaryList.Count - 1 do
    begin
      ABoundary := BoundaryList[index];
      BoundValues := ABoundary.Values as TCustomSutraBoundaryCollection;

      if comboSchedule.ItemIndex > 0 then
      begin
        BoundValues.ScheduleName := AnsiString(comboSchedule.Text);
      end
      else
      begin
        BoundValues.ScheduleName := '';
      end;

      if comboFluidSourceInLakesPresent.ItemIndex >= 0 then
      begin
        ABoundary.LakeInteraction :=
          TLakeBoundaryInteraction(comboFluidSourceInLakesPresent.ItemIndex)
      end;

      SetBoundaryValues(BoundValues);
    end;

  finally
    BoundaryList.Free;
    LocalScreenObjects.Free;
  end;
end;

procedure TframeSutraBoundary.UpdateColWidths;
var
  NewColWidth: Integer;
  ColIndex: Integer;
begin
  NewColWidth := rdgSutraFeature.ClientWidth - (rdgSutraFeature.ColWidths[0] + rdgSutraFeature.ColWidths[1]);
  if rdgSutraFeature.ColCount = 4 then
  begin
    NewColWidth := (NewColWidth div 2) - 20;
  end
  else
  begin
    NewColWidth := NewColWidth - 20;
  end;
  for ColIndex := 2 to rdgSutraFeature.ColCount - 1 do
  begin
    rdgSutraFeature.ColWidths[ColIndex] := NewColWidth;
  end;
end;

function TframeSutraBoundary.UsedColumn: Integer;
begin
  result := Ord(sbgtUsed);
end;

function TframeSutraBoundary.GetValidTime(ACol, ARow: integer): Boolean;
var
  TestValue: Double;
  TestText: string;
begin
  result := True;
  if (ACol = Ord(sbgtTime)) and (ARow > 0) then
  begin
    TestText := rdgSutraFeature.Cells[ACol, ARow];
    if TryStrToFloat(TestText, TestValue) then
    begin
      case FBoundaryType of
        sbtFluidSource, sbtSpecPress:
          begin
            if TestValue < FInitialTime then
            begin
              result := False;
            end;
          end;
        sbtMassEnergySource, sbtSpecConcTemp:
          begin
            if TestValue <= FInitialTime then
            begin
              result := False;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TframeSutraBoundary.SetBoundaryValues(
  BoundValues: TCustomSutraBoundaryCollection);
var
  ColIndex: Integer;
  AssocItem: TCustomSutraAssociatedBoundaryItem;
  ItemIndex: Integer;
  RowIndex: Integer;
  BoundItem: TCustomSutraBoundaryItem;
  ATime: Extended;
  OK: Boolean;
  Initialtime: Double;
  BoundaryTypeString: string;
  StartIndex: Integer;
begin
  if seNumberOfTimes.AsInteger > 0 then
  begin
    Initialtime := frmGoPhast.PhastModel.SutraTimeOptions.InitialTime;
    ItemIndex := 0;
    for RowIndex := 1 to seNumberOfTimes.AsInteger do
    begin
      if TryStrToFloat(rdgSutraFeature.Cells[Ord(sbgtTime), RowIndex], ATime) then
      begin
        OK := False;
        StartIndex := Ord(sbgtUsed);

        for ColIndex := StartIndex to rdgSutraFeature.ColCount - 1 do
        begin
          OK := rdgSutraFeature.Cells[ColIndex, RowIndex] <> '';
          if not OK then
          begin
            Break;
          end;
        end;

        if OK then
        begin
          if ItemIndex < BoundValues.Count then
          begin
            BoundItem := BoundValues.Items[ItemIndex] as TCustomSutraBoundaryItem;
          end
          else
          begin
            BoundItem := BoundValues.Add as TCustomSutraBoundaryItem;
          end;
          if (BoundaryType in [sbtFluidSource, sbtMassEnergySource] )
            and (ATime <= Initialtime)
            and (frmGoPhast.PhastModel.SutraOptions.SimulationType
            <> stSteadyFlowSteadyTransport) then
          begin
            case BoundaryType of
              sbtFluidSource:
                begin
                  BoundaryTypeString := StrFluidSourcesAndSi;
                end;
              sbtMassEnergySource:
                begin
                  BoundaryTypeString := StrMassOrEnergySourc;
                end;
            end;
            frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
              StrInvalidBoundaryTim,
              Format(StrInSTheFirstSpe, [BoundaryTypeString])
              );
            frmErrorsAndWarnings.Show;
          end;
          BoundItem.StartTime := ATime;
          BoundItem.UsedFormula := rdgSutraFeature.Cells[Ord(sbgtUsed), RowIndex];

          if BoundItem is TCustomSutraAssociatedBoundaryItem then
          begin
            AssocItem := TCustomSutraAssociatedBoundaryItem(BoundItem);
            AssocItem.PQFormula := rdgSutraFeature.Cells[2, RowIndex];
            AssocItem.UFormula := rdgSutraFeature.Cells[3, RowIndex];
          end
          else
          begin
            BoundItem.UFormula := rdgSutraFeature.Cells[2, RowIndex];
          end;
          Inc(ItemIndex);
        end;
      end;
    end;
    while BoundValues.Count > ItemIndex do
    begin
      BoundValues.Delete(BoundValues.Count - 1);
    end;
  end;
end;


procedure TframeSutraBoundary.GetBoundaryValues(
  BoundaryList: TSutraBoundaryList);
var
  FirstBoundary: TSutraBoundary;
  Same: Boolean;
  BoundColl: TCustomSutraBoundaryCollection;
  Index: Integer;
  ABoundary: TSutraBoundary;
  ASchedule: TSutraTimeSchedule;
begin
  FirstBoundary := BoundaryList[0];
  BoundColl := FirstBoundary.Values as TCustomSutraBoundaryCollection;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    Same := BoundColl.isSame(ABoundary.Values);
    if not Same then
    begin
      Break;
    end;
  end;
  FBoundariesTheSame := Same;
  if Same then
  begin
    if comboSchedule.ItemIndex >= 1 then
    begin
      ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
        as TSutraTimeSchedule;

      AdjustBoundaryValues(ASchedule, BoundColl);
    end;
//    CheckSchedule(BoundaryList);
    DisplayBoundaries(BoundColl);
  end
  else
  begin
    ClearBoundaries;
  end;
end;

procedure TframeSutraBoundary.btnDeleteClick(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraBoundary.btnInsertClick(Sender: TObject);
begin
  inherited;
//
end;


procedure TframeSutraBoundary.comboScheduleChange(Sender: TObject);
var
  SutraTimeOptions: TSutraTimeOptions;
  ASchedule: TSutraTimeSchedule;
  TimeValues: TOneDRealArray;
  TimeIndex: Integer;
  PickList: TStrings;
  TimeList: TRealList;
  RowIndex: Integer;
  AValue: double;
  TimePos: Integer;
  Initialtime: Double;
begin
  inherited;
  if (comboSchedule.ItemIndex > 0) then
  begin
    SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
    ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
      as TSutraTimeSchedule;
    Initialtime := SutraTimeOptions.InitialTime;
    TimeValues := ASchedule.TimeValues(InitialTime,
       SutraTimeOptions.Schedules);

    PickList := rdgSutraFeature.Columns[0].PickList;
    PickList.Clear;
//    FSettingTimes := True;
    rdgSutraFeature.BeginUpdate;
    TimeList := TRealList.Create;
    try
      for TimeIndex := 0 to Length(TimeValues) - 1 do
      begin
        case FBoundaryType of
          sbtSpecPress, sbtSpecConcTemp:
            begin
              TimeList.Add(TimeValues[TimeIndex]);
            end;
          sbtFluidSource, sbtMassEnergySource:
            begin
              if TimeValues[TimeIndex] <> Initialtime then
              begin
                TimeList.Add(TimeValues[TimeIndex]);
              end;
            end;
        else
          Assert(False)
        end;

      end;
      TimeList.Sort;
      for TimeIndex := 0 to TimeList.Count - 1 do
      begin
        PickList.Add(FloatToStr(TimeList[TimeIndex]));
      end;

      for RowIndex := 1 to rdgSutraFeature.RowCount - 1 do
      begin
        if TryStrToFloat(rdgSutraFeature.Cells[0,RowIndex], AValue) then
        begin
          TimePos := TimeList.IndexOfClosest(AValue);
          if TimeList[TimePos] <> AValue then
          begin
            rdgSutraFeature.Cells[0,RowIndex] := FloatToStr(TimeList[TimePos]);
          end;
        end;
      end;
      for RowIndex := rdgSutraFeature.RowCount - 2 downto 1 do
      begin
        if rdgSutraFeature.Cells[0,RowIndex+1] = rdgSutraFeature.Cells[0,RowIndex] then
        begin
          rdgSutraFeature.DeleteRow(RowIndex+1);
        end;
      end;
      if comboSchedule.ItemIndex <> 1 then
      begin
        RowIndex := 1;
        While RowIndex < rdgSutraFeature.RowCount do
        begin
          if TryStrToFloat(rdgSutraFeature.Cells[0,RowIndex], AValue) then
          begin
            TimePos := TimeList.IndexOfClosest(AValue)+1;
            if TimePos > RowIndex then
            begin
              if RowIndex-1 >= TimeList.Count then
              begin
                Break;
              end;
              rdgSutraFeature.InsertRow(RowIndex);
//              Continue;
              rdgSutraFeature.Cells[0,RowIndex] := FloatToStr(TimeList[RowIndex-1]);
              rdgSutraFeature.Checked[1,RowIndex] := False;
            end;
          end
          else
          begin
            if RowIndex-1 >= TimeList.Count then
            begin
              Break;
            end;
//            rdgSutraFeature.InsertRow(RowIndex);
            rdgSutraFeature.Cells[0,RowIndex] := FloatToStr(TimeList[RowIndex-1]);
            rdgSutraFeature.Checked[1,RowIndex] := False;
          end;
          Inc(RowIndex);
        end;
        while RowIndex-1 < TimeList.Count do
        begin
          rdgSutraFeature.RowCount := rdgSutraFeature.RowCount+1;
          seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger + 1;
          rdgSutraFeature.Cells[0,RowIndex] := FloatToStr(TimeList[RowIndex-1]);
          rdgSutraFeature.Checked[1,RowIndex] := False;
          Inc(RowIndex);
        end;
      end;
    finally
      TimeList.Free;
      rdgSutraFeature.EndUpdate;
//      FSettingTimes := False;
    end;
    rdgSutraFeature.Columns[0].LimitToList := True;

//    if (seNumberOfTimes.AsInteger > 0) then
//    begin
//      BoundValues := nil;
//      try
//        case BoundaryType of
//          sbtFluidSource: BoundValues := TSutraFluidBoundaryCollection.Create(nil, nil, nil);
//          sbtMassEnergySource: BoundValues := TSutraMassEnergySourceSinkCollection.Create(nil, nil, nil);
//          sbtSpecPress: BoundValues := TSutraSpecifiedPressureCollection.Create(nil, nil, nil);
//          sbtSpecConcTemp: BoundValues := TSutraSpecifiedConcTempCollection.Create(nil, nil, nil);
//        end;
//        SetBoundaryValues(BoundValues);
//
//        if Length(TimeValues) = BoundValues.Count then
//        begin
//          for TimeIndex := 0 to BoundValues.Count - 1 do
//          begin
//            BoundValues[TimeIndex].StartTime := TimeValues[TimeIndex];
//          end;
//        end
//        else
//        begin
//          AdjustBoundaryTimes(TimeValues, BoundValues);
//        end;
//
//        DisplayBoundaries(BoundValues);
//      finally
//        BoundValues.Free;
//      end;
//  //    BoundValues: TCustomSutraBoundaryCollection
//    end
//    else
//    begin
//      FSettingTimes := True;
//      rdgSutraFeature.BeginUpdate;
//      try
//        seNumberOfTimes.AsInteger := Length(TimeValues);
//        for TimeIndex := 0 to Length(TimeValues) - 1 do
//        begin
//          rdgSutraFeature.Cells[0,TimeIndex+1] := FloatToStr(TimeValues[TimeIndex]);
//          rdgSutraFeature.Checked[1,TimeIndex+1] := True;
//        end;
//      finally
//        rdgSutraFeature.EndUpdate;
//        FSettingTimes := False;
//      end;
//    end;
  end
  else
  begin
    SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;

    ASchedule := SutraTimeOptions.Schedules[0].Schedule;
    TimeValues := ASchedule.TimeValues(SutraTimeOptions.InitialTime,
       SutraTimeOptions.Schedules);

    PickList := rdgSutraFeature.Columns[0].PickList;
    PickList.Clear;
//    FSettingTimes := True;
    for TimeIndex := 0 to Length(TimeValues) - 1 do
    begin
      case FBoundaryType of
        sbtFluidSource, sbtSpecPress:
          begin
            PickList.Add(FloatToStr(TimeValues[TimeIndex]));
          end;
        sbtMassEnergySource, sbtSpecConcTemp:
          begin
            if TimeIndex <> 0 then
            begin
              PickList.Add(FloatToStr(TimeValues[TimeIndex]));
            end;
          end;
      else
        Assert(False)
      end;

    end;
    rdgSutraFeature.Columns[0].LimitToList := False;
  end;
  UpdateColWidths;
end;

procedure TframeSutraBoundary.DisplayBoundaries(
  BoundColl: TCustomSutraBoundaryCollection);
var
  ItemIndex: Integer;
  Item: TCustomSutraBoundaryItem;
  AssocItem: TCustomSutraAssociatedBoundaryItem;
begin
//  FDisplayingData := True;
  rdgSutraFeature.BeginUpdate;
  try
    seNumberOfTimes.AsInteger := BoundColl.Count;
    rdgSutraFeature.RowCount := BoundColl.Count+1;
    for ItemIndex := 0 to BoundColl.Count - 1 do
    begin
      Item := BoundColl[ItemIndex] as TCustomSutraBoundaryItem;
      rdgSutraFeature.Cells[0,ItemIndex+1] := FloatToStr(Item.StartTime);
      rdgSutraFeature.Cells[1,ItemIndex+1] := Item.UsedFormula;
      if Item is TCustomSutraAssociatedBoundaryItem then
      begin
        AssocItem := TCustomSutraAssociatedBoundaryItem(Item);
        rdgSutraFeature.Cells[2,ItemIndex+1] := AssocItem.PQFormula;
        rdgSutraFeature.Cells[3,ItemIndex+1] := AssocItem.UFormula;
      end
      else
      begin
        rdgSutraFeature.Cells[2,ItemIndex+1] := Item.UFormula;
      end;
    end;
  finally
    rdgSutraFeature.EndUpdate;
//    FDisplayingData := False;
  end;
end;

procedure TframeSutraBoundary.edNameChange(Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraBoundary.edNameExit(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraBoundary.GetScheduleName(BoundaryList: TSutraBoundaryList);
var
  ScheduleName: AnsiString;
  Same: Boolean;
  FirstBoundary: TSutraBoundary;
  ABoundColl: TCustomSutraBoundaryCollection;
  BoundColl: TCustomSutraBoundaryCollection;
  Index: Integer;
  ABoundary: TSutraBoundary;
begin
  FirstBoundary := BoundaryList[0];
  BoundColl := FirstBoundary.Values as TCustomSutraBoundaryCollection;
  ScheduleName := BoundColl.ScheduleName;
  Same := True;
  for Index := 1 to BoundaryList.Count - 1 do
  begin
    ABoundary := BoundaryList[Index];
    ABoundColl := ABoundary.Values as TCustomSutraBoundaryCollection;
    Same := ScheduleName = ABoundColl.ScheduleName;
    if not Same then
    begin
      Break;
    end;
  end;
  SetScheduleIndex(ScheduleName, Same);
end;

procedure TframeSutraBoundary.InitializeColumnHeadings;
var
  ColIndex: Integer;
  TransportChoice: TTransportChoice;
begin
  rdgSutraFeature.Cells[0,0] := StrTime;
  rdgSutraFeature.Cells[1,0] := StrUsed;
  UpdateColWidths;

  TransportChoice := frmGoPhast.PhastModel.SutraOptions.TransportChoice;

  case FBoundaryType of
    sbtFluidSource:
      begin
        rdgSutraFeature.Cells[2,0] := StrFluidSource;
        case TransportChoice of
          tcSolute, tcSoluteHead: rdgSutraFeature.Cells[3,0] := StrAssociatedConcentra;
          tcEnergy: rdgSutraFeature.Cells[3,0] := StrAssociatedTemp;
          else Assert(False);
        end;
      end;
    sbtMassEnergySource:
      begin
        case TransportChoice of
          tcSolute, tcSoluteHead: rdgSutraFeature.Cells[2,0] := StrSoluteSource;
          tcEnergy: rdgSutraFeature.Cells[2,0] := StrEnergySouce;
          else Assert(False);
        end;
      end;
    sbtSpecPress:
      begin
        case TransportChoice of
          tcSolute, tcEnergy:
            begin
              rdgSutraFeature.Cells[2,0] := StrSpecifiedPressure;
            end;
          tcSoluteHead:
            begin
              rdgSutraFeature.Cells[2,0] := StrSutraSpecifiedHead;
            end;
        end;
        case TransportChoice of
          tcSolute, tcSoluteHead: rdgSutraFeature.Cells[3,0] := StrAssociatedConcentra;
          tcEnergy: rdgSutraFeature.Cells[3,0] := StrAssociatedTemp;
          else Assert(False);
        end;
      end;
    sbtSpecConcTemp:
      begin
        case TransportChoice of
          tcSolute, tcSoluteHead: rdgSutraFeature.Cells[2,0] := StrSpecifiedConcentration;
          tcEnergy: rdgSutraFeature.Cells[2,0] := StrSpecifiedTemperatur;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;

  for ColIndex := 2 to rdgSutraFeature.ColCount - 1 do
  begin
    rdgSutraFeature.Columns[ColIndex].AutoAdjustColWidths := false;
  end;

end;

procedure TframeSutraBoundary.LayoutMultiEditControls;
var
  Col: Integer;
  Index: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Col := 1;
  for Index := Col to rdgSutraFeature.ColCount - 1 do
  begin
    if rdgSutraFeature.ColVisible[Index] then
    begin
      Col := Index;
      break;
    end;
  end;
  LayoutControls(rdgSutraFeature, rdeFormula, lblFormula, Col);

end;

procedure TframeSutraBoundary.rdeFormulaChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgSutraFeature, 1, rdeFormula.Text);
  ChangeSelectedCellsInColumn(rdgSutraFeature, 2, rdeFormula.Text);
  if rdgSutraFeature.ColCount >= 4 then
  begin
    ChangeSelectedCellsInColumn(rdgSutraFeature, 3, rdeFormula.Text);
  end;
end;

procedure TframeSutraBoundary.rdgSutraFeatureBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if not GetValidTime(ACol, ARow) then
  begin
    rdgSutraFeature.Canvas.Brush.Color := clRed;
  end;
end;

procedure TframeSutraBoundary.rdgSutraFeatureColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TframeSutraBoundary.rdgSutraFeatureEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := 1 to rdgSutraFeature.RowCount - 1 do
  begin
    if not GetValidTime(Ord(sbgtTime), RowIndex) then
    begin
      case FBoundaryType of
        sbtFluidSource, sbtSpecPress:
          begin
            Beep;
            MessageDlg(StrYouMustSpecifyAT, mtError, [mbOK], 0);
            break;
          end;
        sbtMassEnergySource, sbtSpecConcTemp:
          begin
            Beep;
            MessageDlg(StrYouMustSpecifyG, mtError, [mbOK], 0);
            break;
          end;
        else Assert(False);
      end;
    end;
  end;
  UpdateColWidths;
end;

procedure TframeSutraBoundary.rdgSutraFeatureHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiEditControls;
end;

procedure TframeSutraBoundary.rdgSutraFeatureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if rdgSutraFeature.ColCount = 3 then
  begin
    EnableMultiEditControl(rdgSutraFeature, rdeFormula, 2);
  end
  else
  begin
    EnableMultiEditControl(rdgSutraFeature, rdeFormula, [2,3]);
  end;
end;

procedure TframeSutraBoundary.rdgSutraFeatureSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if Value <> '' then
  begin
    seNumberOfTimes.AsInteger := rdgSutraFeature.RowCount -1;
  end;
end;

procedure TframeSutraBoundary.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

end.
