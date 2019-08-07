unit frmSutraTimesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, ExtCtrls, Grids,
  RbwDataGrid4, ArgusDataEntry, JvPageList, JvExControls, StdCtrls,
  VirtualTrees, frameGridUnit, Buttons, SutraTimeScheduleUnit, Mask,
  JvExMask, JvSpin, UndoItems, ComCtrls, JvGroupHeader, GrayTabs;

type
  PSutraTimeScheduleNodeData = ^TSutraTimeScheduleNodeData;

  TSutraTimeScheduleNodeData = record
    SutraTimeScheduleItem: TSutraTimeScheduleItem;
  end;

  TUndoChangeSutraTimes = class(TCustomUndo)
  private
    FOldSutraTimeOptions: TSutraTimeOptions;
    FNewSutraTimeOptions: TSutraTimeOptions;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
    constructor Create(var SutraTimeOptions: TSutraTimeOptions);
    destructor Destroy; override;
  end;

  TfrmSutraTimes = class(TfrmCustomGoPhast)
    vstScedules: TVirtualStringTree;
    grp1: TGroupBox;
    lblScheduleType: TLabel;
    comboScheduleType: TComboBox;
    jvpglstTemporal: TJvPageList;
    jvspTimeCycle: TJvStandardPage;
    grp3: TGroupBox;
    lblNTMAX: TLabel;
    lblTimei: TLabel;
    lblTimel: TLabel;
    rdeTimei: TRbwDataEntry;
    rdeTimel: TRbwDataEntry;
    grp4: TGroupBox;
    lblNTCYC: TLabel;
    lblTcmult: TLabel;
    lblDTMAX: TLabel;
    lblTCMIN: TLabel;
    rdeTcmult: TRbwDataEntry;
    rdeDTMAX: TRbwDataEntry;
    rdeTCMIN: TRbwDataEntry;
    jvspTimeList: TJvStandardPage;
    grp5: TGroupBox;
    rgMannerOfTimeSpecification: TRadioGroup;
    lbledName: TLabeledEdit;
    frameTimes: TframeGrid;
    jvspStepCyle: TJvStandardPage;
    jvspStepList: TJvStandardPage;
    grp2: TGroupBox;
    frameSteps: TframeGrid;
    rdeTimec: TRbwDataEntry;
    lblTimec: TLabel;
    grp6: TGroupBox;
    lblMaxSteps: TLabel;
    lblInitialStep: TLabel;
    lblLimitingStep: TLabel;
    lblTimeStepIncrement: TLabel;
    pnl1: TPanel;
    sbAddUnit: TSpeedButton;
    sbDeleteUnit: TSpeedButton;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rdeScaleFactor: TRbwDataEntry;
    lblScaleFactor: TLabel;
    seNtmax: TJvSpinEdit;
    seMaxSteps: TJvSpinEdit;
    seInitialStep: TJvSpinEdit;
    seLimitingStep: TJvSpinEdit;
    seTimeStepIncrement: TJvSpinEdit;
    seNTCYC: TJvSpinEdit;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    pcMain: TPageControl;
    tabSchedules: TTabSheet;
    tabOptions: TTabSheet;
    rdeInitialTime: TRbwDataEntry;
    lblInitialtime: TLabel;
    sePressureCycles: TJvSpinEdit;
    lblPressureCycles: TLabel;
    seTransportCycles: TJvSpinEdit;
    lblTransportCycles: TLabel;
    jvgrphdrICS: TJvGroupHeader;
    jvgrphdrInput6: TJvGroupHeader;
    btnConvertTimeUnits: TButton;
    procedure comboScheduleTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure sbAddUnitClick(Sender: TObject);
    procedure sbDeleteUnitClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure vstScedulesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstScedulesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstScedulesNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure lbledNameChange(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure sePressureCyclesChange(Sender: TObject);
    procedure seTransportCyclesChange(Sender: TObject);
    procedure rdeInitialTimeChange(Sender: TObject);
    procedure frameTimesGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rgMannerOfTimeSpecificationClick(Sender: TObject);
    procedure btnConvertTimeUnitsClick(Sender: TObject);
  private
    FSutraTimeOptions: TSutraTimeOptions;
    FSelectedSchedule: TSutraTimeScheduleItem;
    FGettingData: Boolean;
    FUpdatingSchedule: Boolean;
    FGettingItem: Boolean;
    procedure SetSelectedSchedule(const Value: TSutraTimeScheduleItem);
    procedure GetItem;
    procedure SetItem;
    procedure GetData;
    procedure SetData;
    procedure ExchangeScheduleNodes(Selected: PVirtualNode;
      AnotherNode: PVirtualNode);
    procedure EnableUpDownButtons(const Value: TSutraTimeScheduleItem);
    procedure UpdateTimeStepsSchedule;
    { Private declarations }
  public
    property SelectedSchedule: TSutraTimeScheduleItem read FSelectedSchedule
      write SetSelectedSchedule;
    { Public declarations }
  end;

var
  frmSutraTimes: TfrmSutraTimes;

implementation

uses
  frmGoPhastUnit, Undo, GoPhastTypes, frmTimeUnitsConverterUnit;

resourcestring
  StrStepList = 'Step List';
  StrStepCycle = 'Step Cycle';
  StrTimes = 'Times';
  StrSteps = 'Steps';
  StrNone = 'none';
  StrChangeSUTRATimeOp = 'Change SUTRA time options';

{$R *.dfm}

procedure TfrmSutraTimes.btnConvertTimeUnitsClick(Sender: TObject);
var
  frmTimeUnitsConverter: TfrmTimeUnitsConverter;
begin
  inherited;
  frmTimeUnitsConverter := TfrmTimeUnitsConverter.Create(nil);
  frmTimeUnitsConverter.Show;
end;

procedure TfrmSutraTimes.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSutraTimes.comboScheduleTypeChange(Sender: TObject);
begin
  inherited;
  jvpglstTemporal.ActivePageIndex := comboScheduleType.ItemIndex;
  rgMannerOfTimeSpecification.Enabled :=
    TScheduleType(comboScheduleType.ItemIndex) in [stTimeList, stTimeCycle];
  rdeScaleFactor.Enabled := rgMannerOfTimeSpecification.Enabled;
  UpdateTimeStepsSchedule;
end;

procedure TfrmSutraTimes.FormCreate(Sender: TObject);
begin
  inherited;
  pcMain.ActivePageIndex := 0;

  frameTimes.Grid.Cells[0, 0] := StrTimes;
  frameSteps.Grid.Cells[0, 0] := StrSteps;

  FSutraTimeOptions := TSutraTimeOptions.Create(nil);
  GetData;
end;

procedure TfrmSutraTimes.FormDestroy(Sender: TObject);
begin
  inherited;
  FSutraTimeOptions.Free;
end;

procedure TfrmSutraTimes.frameTimesGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (FSelectedSchedule.Index = 0) and (ARow = 1) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmSutraTimes.sbAddUnitClick(Sender: TObject);
var
  Node: PVirtualNode;
  SutraTimeScheduleNodeData: PSutraTimeScheduleNodeData;
  ASchedule: TSutraTimeScheduleItem;
begin
  inherited;
  ASchedule := FSutraTimeOptions.Schedules.Add;
  Node := vstScedules.AddChild(nil);
  SutraTimeScheduleNodeData := vstScedules.GetNodeData(Node);
  SutraTimeScheduleNodeData.SutraTimeScheduleItem := ASchedule;
  SelectedSchedule := ASchedule;
  vstScedules.Selected[Node] := True;
end;

procedure TfrmSutraTimes.sbDeleteUnitClick(Sender: TObject);
var
  NewIndex: Integer;
  OldSchedule: TSutraTimeScheduleItem;
  Selected: PVirtualNode;
begin
  inherited;
  if (SelectedSchedule <> nil) and (SelectedSchedule.Index > 0) then
  begin
    OldSchedule := SelectedSchedule;
    NewIndex := SelectedSchedule.Index + 1;
    if NewIndex >= FSutraTimeOptions.Schedules.Count then
    begin
      NewIndex := NewIndex - 2;
    end;
    if NewIndex >= 0 then
    begin
      SelectedSchedule := FSutraTimeOptions.Schedules[NewIndex]
    end
    else
    begin
      SelectedSchedule := nil;
    end;
    Selected := vstScedules.GetFirstSelected;
    vstScedules.DeleteNode(Selected);

    OldSchedule.Free;
  end;
end;

procedure TfrmSutraTimes.sbDownClick(Sender: TObject);
var
  Selected: PVirtualNode;
  AnotherNode: PVirtualNode;
begin
  inherited;
  Selected := vstScedules.GetFirstSelected;
  if Selected <> nil then
  begin
    AnotherNode := vstScedules.GetNextSibling(Selected);
    if AnotherNode <> nil then
    begin
      ExchangeScheduleNodes(Selected, AnotherNode);
    end;
  end;
end;

procedure TfrmSutraTimes.sbUpClick(Sender: TObject);
var
  Selected: PVirtualNode;
  AnotherNode: PVirtualNode;
begin
  inherited;
  Selected := vstScedules.GetFirstSelected;
  if Selected <> nil then
  begin
    AnotherNode := vstScedules.GetPreviousSibling(Selected);
    if AnotherNode <> nil then
    begin
      ExchangeScheduleNodes(Selected, AnotherNode);
    end;
  end;
end;

procedure TfrmSutraTimes.sePressureCyclesChange(Sender: TObject);
begin
  inherited;
  if sePressureCycles.AsInteger <> 1 then
  begin
    seTransportCycles.AsInteger := 1;
  end;
end;

procedure TfrmSutraTimes.SetData;
var
  Undo: TUndoChangeSutraTimes;
begin
  SelectedSchedule := nil;

  FSutraTimeOptions.InitialTime := StrToFloat(rdeInitialTime.Text);
  FSutraTimeOptions.HydraulicSolutionCycleSteps := sePressureCycles.AsInteger;
  FSutraTimeOptions.TransportSolutionCycleSteps := seTransportCycles.AsInteger;

  Undo := TUndoChangeSutraTimes.Create(FSutraTimeOptions);
  try
    frmGoPhast.UndoStack.Submit(Undo);
  except
    Undo.Free;
    raise;
  end;
end;

procedure TfrmSutraTimes.ExchangeScheduleNodes(Selected: PVirtualNode;
  AnotherNode: PVirtualNode);
var
  AnotherData: PSutraTimeScheduleNodeData;
  SelectedData: PSutraTimeScheduleNodeData;
  Temp: TSutraTimeScheduleItem;
begin
  SelectedData := vstScedules.GetNodeData(Selected);
  AnotherData := vstScedules.GetNodeData(AnotherNode);
  Temp := SelectedData.SutraTimeScheduleItem;
  AnotherData.SutraTimeScheduleItem.Index := Temp.Index;
  SelectedData.SutraTimeScheduleItem := AnotherData.SutraTimeScheduleItem;
  AnotherData.SutraTimeScheduleItem := Temp;
  vstScedules.Selected[AnotherNode] := True;
  vstScedules.InvalidateNode(Selected);
  vstScedules.InvalidateNode(AnotherNode);
  EnableUpDownButtons(SelectedSchedule);
end;

procedure TfrmSutraTimes.EnableUpDownButtons(const Value
  : TSutraTimeScheduleItem);
begin
  sbUp.Enabled := (Value <> nil) and (Value.Index > 1);
  sbDown.Enabled := (Value <> nil) and (Value.Index > 0) and
    (Value.Index < Value.Collection.Count - 1);
end;

type
  TGridCrack = class(TStringGrid);

procedure TfrmSutraTimes.UpdateTimeStepsSchedule;
var
  TICS: Extended;
  TimeSteps: TSutraTimeScheduleItem;
begin
  if not FGettingData and not FGettingItem and (FSutraTimeOptions <> nil) and
    TryStrToFloat(rdeInitialTime.Text, TICS) then
  begin
    if FUpdatingSchedule then
    begin
      Exit;
    end;
    FUpdatingSchedule := True;
    try
      TimeSteps := FSutraTimeOptions.Schedules[0];
      if (FSelectedSchedule = TimeSteps) then
      begin
        SetItem;
      end;
      if TimeSteps.Schedule.ScheduleType = stTimeList then
      begin
        if TimeSteps.Schedule.SutraTimeChoice = stcElapsed then
        begin
          TICS := 0;
        end;
        if TimeSteps.Schedule.Times.Count = 0 then
        begin
          TimeSteps.Schedule.Times.Add.Value := TICS;
        end
        else
        begin
          TimeSteps.Schedule.Times[0].Value := TICS;
        end;
      end;
      if (FSelectedSchedule = TimeSteps) then
      begin
        GetItem;
      end;
      if (TimeSteps.Schedule.ScheduleType = stTimeList) and
        (FSelectedSchedule = TimeSteps) then
      begin
        TGridCrack(frameTimes.Grid).HideEditor;
      end;
    finally
      FUpdatingSchedule := False;
    end;
  end;
end;

procedure TfrmSutraTimes.SetItem;
var
  Schedule: TSutraTimeSchedule;
  RowIndex: Integer;
  ItemIndex: Integer;
  AFloat: double;
  AnInt: Integer;
begin
  if SelectedSchedule <> nil then
  begin
    Schedule := SelectedSchedule.Schedule;
    Schedule.Name := AnsiString(lbledName.Text);
    Schedule.ScheduleType := TScheduleType(comboScheduleType.ItemIndex);
    Schedule.ScaleFactor := StrToFloat(rdeScaleFactor.Text);
    Schedule.SutraTimeChoice :=
      TSutraTimeChoice(rgMannerOfTimeSpecification.ItemIndex);

    ItemIndex := 0;
    for RowIndex := 1 to frameTimes.Grid.RowCount - 1 do
    begin
      if TryStrToFloat(frameTimes.Grid.Cells[0, RowIndex], AFloat) then
      begin
        if Schedule.Times.Count <= ItemIndex then
        begin
          Schedule.Times.Add.Value := AFloat;
        end
        else
        begin
          Schedule.Times[ItemIndex].Value := AFloat;
        end;
        Inc(ItemIndex);
      end;
    end;
    while Schedule.Times.Count > ItemIndex do
    begin
      Schedule.Times.Delete(Schedule.Times.Count - 1);
    end;
    Schedule.Times.Sort;

    Schedule.MaxTimeCycle := seNtmax.AsInteger;
    Schedule.InitialTime := StrToFloatDef(rdeTimei.Text, 0);
    Schedule.LimitingTime := StrToFloatDef(rdeTimel.Text, 1E99);
    Schedule.InitialTimeIncrement := StrToFloatDef(rdeTimec.Text, 1);
    Schedule.IncrementUpdateCount := seNTCYC.AsInteger;
    Schedule.TimeMultiplier := StrToFloatDef(rdeTcmult.Text, 1);
    Schedule.MinIncrement := StrToFloatDef(rdeTCMIN.Text, 1E-20);
    Schedule.MaxIncrement := StrToFloatDef(rdeDTMAX.Text, 1E99);

    ItemIndex := 0;
    for RowIndex := 1 to frameSteps.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameSteps.Grid.Cells[0, RowIndex], AnInt) then
      begin
        if Schedule.Steps.Count <= ItemIndex then
        begin
          Schedule.Steps.Add.Value := AnInt;
        end
        else
        begin
          Schedule.Steps[ItemIndex].Value := AnInt;
        end;
        Inc(ItemIndex);
      end;
    end;
    while Schedule.Steps.Count > ItemIndex do
    begin
      Schedule.Steps.Delete(Schedule.Steps.Count - 1);
    end;
    Schedule.Steps.Sort;

    Schedule.MaxSteps := seMaxSteps.AsInteger;
    Schedule.InitialTimeStep := seInitialStep.AsInteger;
    Schedule.LimitingTimeStep := seLimitingStep.AsInteger;
    Schedule.TimeStepIncrement := seTimeStepIncrement.AsInteger;

  end;
end;

procedure TfrmSutraTimes.seTransportCyclesChange(Sender: TObject);
begin
  inherited;
  if seTransportCycles.AsInteger <> 1 then
  begin
    sePressureCycles.AsInteger := 1;
  end;
end;

procedure TfrmSutraTimes.GetData;
var
  ItemIndex: Integer;
  ANode: PVirtualNode;
  SutraTimeScheduleNodeData: PSutraTimeScheduleNodeData;
  Item: TSutraTimeScheduleItem;
  FirstNode: PVirtualNode;
  FirstSchedule: TSutraTimeScheduleItem;
begin
  inherited;
  FGettingData := True;
  try
    FSelectedSchedule := nil;
    FSutraTimeOptions.Assign(frmGoPhast.PhastModel.SutraTimeOptions);

    rdeInitialTime.Text := FloatToStr(FSutraTimeOptions.InitialTime);
    sePressureCycles.AsInteger := FSutraTimeOptions.HydraulicSolutionCycleSteps;
    seTransportCycles.AsInteger :=
      FSutraTimeOptions.TransportSolutionCycleSteps;

    FirstNode := nil;
    FirstSchedule := nil;
    for ItemIndex := 0 to FSutraTimeOptions.Schedules.Count - 1 do
    begin
      Item := FSutraTimeOptions.Schedules[ItemIndex];
      ANode := vstScedules.AddChild(nil);
      SutraTimeScheduleNodeData := vstScedules.GetNodeData(ANode);
      SutraTimeScheduleNodeData.SutraTimeScheduleItem := Item;
      if FirstNode = nil then
      begin
        FirstNode := ANode;
        FirstSchedule := Item;
      end;
    end;
    if FirstNode <> nil then
    begin
      vstScedules.Selected[FirstNode] := True;
    end;
    SelectedSchedule := FirstSchedule;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmSutraTimes.GetItem;
var
  Schedule: TSutraTimeSchedule;
  RowIndex: Integer;
  TICS: Extended;
  OldSutraTimeChoice: TSutraTimeChoice;
  TimeSteps: TSutraTimeSchedule;
  TimeValues: TOneDRealArray;
  GridColumn: TRbwColumn4;
  TimeIndex: Integer;
begin
  FGettingItem := True;
  try
    if SelectedSchedule = nil then
    begin
      lbledName.Enabled := False;
      comboScheduleType.Enabled := False;
      rdeScaleFactor.Enabled := False;
      rgMannerOfTimeSpecification.Enabled := False;
      frameTimes.Enabled := False;
      seNtmax.Enabled := False;
      rdeTimei.Enabled := False;
      rdeTimel.Enabled := False;
      rdeTimec.Enabled := False;
      seNTCYC.Enabled := False;
      rdeTcmult.Enabled := False;
      rdeTCMIN.Enabled := False;
      rdeDTMAX.Enabled := False;
      frameSteps.Enabled := False;
      seMaxSteps.Enabled := False;
      seInitialStep.Enabled := False;
      seLimitingStep.Enabled := False;
      seTimeStepIncrement.Enabled := False;
    end
    else
    begin
      lbledName.Enabled := SelectedSchedule.Index > 0;
      comboScheduleType.Enabled := True;
      rdeScaleFactor.Enabled := True;
      rgMannerOfTimeSpecification.Enabled := True;
      frameTimes.Enabled := True;
      seNtmax.Enabled := True;
      rdeTimei.Enabled := True;
      rdeTimel.Enabled := True;
      rdeTimec.Enabled := True;
      seNTCYC.Enabled := True;
      rdeTcmult.Enabled := True;
      rdeTCMIN.Enabled := True;
      rdeDTMAX.Enabled := True;
      frameSteps.Enabled := True;
      seMaxSteps.Enabled := True;
      seInitialStep.Enabled := True;
      seLimitingStep.Enabled := True;
      seTimeStepIncrement.Enabled := True;

      if lbledName.Enabled then
      begin
        if comboScheduleType.Items.Count < 4 then
        begin
          Assert(comboScheduleType.Items.Count = 2);
          comboScheduleType.Items.Add(StrStepList);
          comboScheduleType.Items.Add(StrStepCycle);
        end;
      end
      else
      begin
        while comboScheduleType.Items.Count > 2 do
        begin
          comboScheduleType.Items.Delete(comboScheduleType.Items.Count - 1);
        end;
      end;

      Schedule := SelectedSchedule.Schedule;
      lbledName.Text := string(Schedule.Name);
      comboScheduleType.ItemIndex := Ord(Schedule.ScheduleType);
      comboScheduleTypeChange(nil);
      rdeScaleFactor.Text := FloatToStr(Schedule.ScaleFactor);
      rgMannerOfTimeSpecification.ItemIndex := Ord(Schedule.SutraTimeChoice);
      rgMannerOfTimeSpecificationClick(nil);

      frameTimes.seNumber.AsInteger := Schedule.Times.Count;
      frameTimes.seNumberChange(nil);

      frameTimes.Grid.BeginUpdate;
      try
        GridColumn := frameTimes.Grid.Columns[0];
        if Schedule.SutraTimeChoice = stcElapsed then
        begin
          TICS := 0;
        end
        else if not TryStrToFloat(rdeInitialTime.Text, TICS) then
        begin
          TICS := 0;
        end;
        if SelectedSchedule.Index = 0 then
        begin
          GridColumn.CheckMin := True;
          GridColumn.Min := TICS;
          GridColumn.ComboUsed := False;
          GridColumn.LimitToList := False;
        end
        else
        begin
          TimeSteps := FSutraTimeOptions.Schedules[0].Schedule;
          OldSutraTimeChoice := TimeSteps.SutraTimeChoice;
          try
            TimeSteps.SutraTimeChoice := Schedule.SutraTimeChoice;
            TimeValues := TimeSteps.TimeValues(TICS, FSutraTimeOptions.Schedules);
            GridColumn.CheckMin := False;
            GridColumn.ComboUsed := True;
            GridColumn.PickList.Clear;
            GridColumn.PickList.Capacity := Length(TimeValues);
            for TimeIndex := 0 to Length(TimeValues) - 1 do
            begin
              GridColumn.PickList.Add(FloatToStr(TimeValues[TimeIndex]));
            end;
            GridColumn.LimitToList := True;
          finally
            TimeSteps.SutraTimeChoice := OldSutraTimeChoice;
          end;
        end;

        if Schedule.Times.Count = 0 then
        begin
          for RowIndex := 1 to frameTimes.Grid.RowCount - 1 do
          begin
            frameTimes.Grid.Cells[0, RowIndex] := '';
          end;
        end
        else
        begin
          for RowIndex := 0 to Schedule.Times.Count - 1 do
          begin
            frameTimes.Grid.Cells[0, RowIndex + 1] :=
              FloatToStr(Schedule.Times[RowIndex].Value);
          end;
        end;
      finally
        frameTimes.Grid.EndUpdate
      end;

      seNtmax.AsInteger := Schedule.MaxTimeCycle;
      rdeTimei.Text := FloatToStr(Schedule.InitialTime);
      rdeTimel.Text := FloatToStr(Schedule.LimitingTime);
      rdeTimec.Text := FloatToStr(Schedule.InitialTimeIncrement);
      seNTCYC.AsInteger := Schedule.IncrementUpdateCount;
      rdeTcmult.Text := FloatToStr(Schedule.TimeMultiplier);
      rdeTCMIN.Text := FloatToStr(Schedule.MinIncrement);
      rdeDTMAX.Text := FloatToStr(Schedule.MaxIncrement);

      frameSteps.seNumber.AsInteger := Schedule.Steps.Count;
      frameSteps.seNumberChange(nil);

      frameSteps.Grid.BeginUpdate;
      try
        if Schedule.Steps.Count = 0 then
        begin
          for RowIndex := 1 to frameSteps.Grid.RowCount - 1 do
          begin
            frameSteps.Grid.Cells[0, RowIndex] := '';
          end;
        end
        else
        begin
          for RowIndex := 0 to Schedule.Steps.Count - 1 do
          begin
            frameSteps.Grid.Cells[0, RowIndex + 1] :=
              IntToStr(Schedule.Steps[RowIndex].Value);
          end;
        end;
      finally
        frameSteps.Grid.EndUpdate;
      end;

      seMaxSteps.AsInteger := Schedule.MaxSteps;
      seInitialStep.AsInteger := Schedule.InitialTimeStep;
      seLimitingStep.AsInteger := Schedule.LimitingTimeStep;
      seTimeStepIncrement.AsInteger := Schedule.TimeStepIncrement;
    end;
  finally
    FGettingItem := False;
  end;
end;

procedure TfrmSutraTimes.lbledNameChange(Sender: TObject);
begin
  inherited;
  if SelectedSchedule <> nil then
  begin
    SelectedSchedule.Schedule.Name := AnsiString(lbledName.Text);
    vstScedules.Invalidate;
  end;
end;

procedure TfrmSutraTimes.rdeInitialTimeChange(Sender: TObject);
begin
  inherited;
  UpdateTimeStepsSchedule;
end;

procedure TfrmSutraTimes.rgMannerOfTimeSpecificationClick(Sender: TObject);
begin
  inherited;
  if (FSelectedSchedule <> nil) then
  begin
    if (FSelectedSchedule.Index = 0)
      and (rgMannerOfTimeSpecification.ItemIndex = 1) then
    begin
      rdeTimei.Text := '0';
      rdeTimei.Enabled := False;
    end
    else
    begin
      rdeTimei.Enabled := True;
    end;
    end;
end;

procedure TfrmSutraTimes.SetSelectedSchedule(const Value
  : TSutraTimeScheduleItem);
begin
  if (FSelectedSchedule <> Value) or (Value = nil) then
  begin
    SetItem;
    FSelectedSchedule := Value;
    GetItem;
    EnableUpDownButtons(FSelectedSchedule);
  end;
end;

procedure TfrmSutraTimes.vstScedulesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TSutraTimeScheduleNodeData)
end;

procedure TfrmSutraTimes.vstScedulesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  SutraTimeScheduleNodeData: PSutraTimeScheduleNodeData;
begin
  inherited;
  SutraTimeScheduleNodeData := Sender.GetNodeData(Node);
  if not Assigned(SutraTimeScheduleNodeData) or
    not Assigned(SutraTimeScheduleNodeData.SutraTimeScheduleItem) then
  begin
    CellText := StrNone;
  end
  else
  begin
    CellText := string(SutraTimeScheduleNodeData.
      SutraTimeScheduleItem.Schedule.Name);
  end;
end;

procedure TfrmSutraTimes.vstScedulesNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  SutraTimeScheduleNodeData: PSutraTimeScheduleNodeData;
begin
  inherited;
  SutraTimeScheduleNodeData := Sender.GetNodeData(HitInfo.HitNode);
  if Assigned(SutraTimeScheduleNodeData) then
  begin
    SelectedSchedule := SutraTimeScheduleNodeData.SutraTimeScheduleItem;
  end;
end;

{ TUndoChangeSutraTimeSchedules }

constructor TUndoChangeSutraTimes.Create(var SutraTimeOptions
  : TSutraTimeOptions);
begin
  FOldSutraTimeOptions := TSutraTimeOptions.Create(nil);
  FOldSutraTimeOptions.Assign(frmGoPhast.PhastModel.SutraTimeOptions);
  FNewSutraTimeOptions := SutraTimeOptions;
  SutraTimeOptions := nil;
end;

function TUndoChangeSutraTimes.Description: string;
begin
  result := StrChangeSUTRATimeOp;
end;

destructor TUndoChangeSutraTimes.Destroy;
begin
  FOldSutraTimeOptions.Free;
  FNewSutraTimeOptions.Free;
  inherited;
end;

procedure TUndoChangeSutraTimes.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SutraTimeOptions := FNewSutraTimeOptions;
end;

procedure TUndoChangeSutraTimes.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SutraTimeOptions := FOldSutraTimeOptions;
end;

end.
