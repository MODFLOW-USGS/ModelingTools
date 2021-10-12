unit frmManageHeadObservationsUnit;

interface

uses System.UITypes, System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Grids,
  RbwDataGrid4, ComCtrls, ScreenObjectUnit, ModflowHobUnit, GoPhastTypes,
  UndoItemsScreenObjects, ArgusDataEntry, Mask, JvExMask, JvToolEdit, GrayTabs;

type
  TFilterRow = (frLabel, frName, frObsPred, frITT, frObjName, frValue, frTime,
    frStatistic, frStatFlag, frObsName);
  TFilterCol = (fcLabel, fcLow, fcHigh);

  TObservationCol = (ocObsGroupName, ocObsPred, ocITT, ocObjName, ocValue,
    ocTime, ocStatistic, ocStatFlag, ocObsName);

  TObsEdit = class(TObject)
    ScreenObject: TScreenObject;
    ObsItem: THobItem;
    ObsGroupName: string;
    Purpose: TObservationPurpose;
    Value: double;
    MultiObsMethod: TMultiObsMethod;
    ObjectName: string;
    Time: double;
    Statistic: double;
    StatFlag: TStatFlag;
    // @name is the position of @link(ObsItem) in
    // @link(TScreenObject.ModflowHeadObservations).
    TimeIndex: integer;
    function ObsName: string;
    function ChangedValues: boolean;
  end;

  TfrmManageHeadObservations = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    btnHighlightObject: TButton;
    pcMain: TPageControl;
    tabObservations: TTabSheet;
    rdgObservations: TRbwDataGrid4;
    tabFilters: TTabSheet;
    rdgRowFilter: TRbwRowDataGrid;
    Panel1: TPanel;
    edObsGroupName: TEdit;
    comboObsPred: TComboBox;
    comboITT: TComboBox;
    rdeValue: TRbwDataEntry;
    rdeTime: TRbwDataEntry;
    rdeStatistic: TRbwDataEntry;
    comboStatFlag: TComboBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgObservationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgRowFilterSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
    procedure btnHighlightObjectClick(Sender: TObject);
    procedure rdgObservationsExit(Sender: TObject);
    procedure rdgObservationsColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure FormResize(Sender: TObject);
    procedure rdgObservationsHorizontalScroll(Sender: TObject);
    procedure rdgObservationsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edObsGroupNameChange(Sender: TObject);
    procedure comboObsPredChange(Sender: TObject);
    procedure comboITTChange(Sender: TObject);
    procedure rdeValueChange(Sender: TObject);
    procedure rdeTimeChange(Sender: TObject);
    procedure rdeStatisticChange(Sender: TObject);
    procedure comboStatFlagChange(Sender: TObject);
    procedure rdgObservationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FObsEdits: TList;
    FDisplayingList: Boolean;
    FLoaded: Boolean;
    FGettingData: Boolean;
    procedure DisplayFilteredList;
    procedure GetData;
    procedure SetData;
    procedure LayoutMultiRowEditControls;
    procedure EnableMultiEditControls;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoSetHeadObs = class(TUndoSetScreenObjectProperties)
  protected
    function Description: string; override;
  end;

var
  frmManageHeadObservations: TfrmManageHeadObservations;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, Contnrs, frmGoToUnit, Math;

resourcestring
  StrObservationGroupName = 'Observation group name';
  StrObsPred = 'Obs/Pred';
  StrValue = 'Observed head';
  StrITT = 'ITT';
  StrObjectName = 'Object name';
  StrTime = 'Time';
  StrStatistic = 'Statistic';
  StrStatFlag = 'Stat Flag';
  StrObservationName = 'Observation name';
  StrLow = 'Low';
  StrHigh = 'High';
  StrHead = 'Head';
  StrDrawdown = 'Drawdown';
  StrSimulatedValue = 'Simulated value';
  StrDifference = 'Difference';
  StrNoneOfTheObservat = 'None of the observation names in the file matched ' +
  'the observation names for this model.';
  StrThereWasNotAPerf = 'There was not a perfect correspondance between the ' +
  'observations in the file and the observations in this model. Do you want ' +
  'to import those that do match?';
  StrListingFile = 'Listing file';
  StrEditHeadObservatio = 'edit head observations';
  StrTheManageHeadObse = 'The Manage Head Observations dialog box can not be' +
  ' displayed until some head observations have been defined in one or more ' +
  'objects.';
  StrYouMustSelectOne = 'You must select one or more objects in the table in' +
  ' order to highlight them.';

{$R *.dfm}

Type
  TDataGridCrack = class(TRbwRowDataGrid);
  TControlCrack = class(TControl);

Type
  TCompareMethod = class(TObject)
    Method: TObservationCol;
  end;

var
  SortOrder: TList = nil;

function CompareGroupName(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := AnsiCompareText(P1.ObsGroupName, P2.ObsGroupName);
end;

function CompareObsPred(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Ord(P1.Purpose) - Ord(P2.Purpose);
end;

function CompareITT(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Ord(P1.MultiObsMethod) - Ord(P2.MultiObsMethod);
end;

function CompareObjectName(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := AnsiCompareText(P1.ObjectName, P2.ObjectName);
end;

function CompareValue(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Value - P2.Value);
end;

function CompareTime(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Time - P2.Time);
end;

function CompareStatistic(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Sign(P1.Statistic - P2.Statistic);
end;

function CompareStatFlag(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := Ord(P1.StatFlag) - Ord(P2.StatFlag);
end;

function CompareObsName(Item1, Item2: Pointer): Integer;
var
  P1, P2: TObsEdit;
begin
  P1 := Item1;
  P2 := Item2;
  result := AnsiCompareText(P1.ObsName, P2.ObsName);
end;

function CompareObservations(Item1, Item2: Pointer): Integer;
var
  Index: Integer;
  CM: TCompareMethod;
begin
  result := 0;
  for Index := 0 to SortOrder.Count - 1 do
  begin
    CM := SortOrder[Index];
    case CM.Method of
      ocObsGroupName: result := CompareGroupName(Item1, Item2);
      ocObsPred: result := CompareObsPred(Item1, Item2);
      ocITT: result := CompareITT(Item1, Item2);
      ocObjName: result := CompareObjectName(Item1, Item2);
      ocValue: result := CompareValue(Item1, Item2);
      ocTime: result := CompareTime(Item1, Item2);
      ocStatistic: result := CompareStatistic(Item1, Item2);
      ocStatFlag: result := CompareStatFlag(Item1, Item2);
      ocObsName: result := CompareObsName(Item1, Item2);
      else Assert(False);
    end;
    if result <> 0 then
    begin
      Exit;
    end;
  end;
end;

procedure TfrmManageHeadObservations.btnHighlightObjectClick(Sender: TObject);
var
  Undo: TUndoChangeSelection;
  ObsEdit: TObsEdit;
  XCoordinate: double;
  YCoordinate: double;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  if rdgObservations.SelectedRow <= 0 then
  begin
    Beep;
    MessageDlg(StrYouMustSelectOne, mtInformation, [mbOK], 0);
    Exit;
  end;
  
  Undo := TUndoChangeSelection.Create;

  frmGoPhast.ResetSelectedScreenObjects;

  ObsEdit := nil;
  for RowIndex := 1 to rdgObservations.RowCount - 1 do
  begin
    for ColIndex := 0 to rdgObservations.ColCount - 1 do
    begin
      if rdgObservations.IsSelectedCell(ColIndex, RowIndex) then
      begin
        ObsEdit := rdgObservations.Objects[0,RowIndex] as TObsEdit;
        ObsEdit.ScreenObject.Selected := True;
        break;
      end;
    end;
  end;
  if rdgObservations.SelectedRow >= rdgObservations.FixedRows then
  begin
    ObsEdit := rdgObservations.Objects[
      0,rdgObservations.SelectedRow] as TObsEdit;
    ObsEdit.ScreenObject.Selected := True;
  end;
  Assert(ObsEdit <> nil);

  Undo.SetPostSelection;

  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;

  XCoordinate := ObsEdit.ScreenObject.Points[0].X;
  YCoordinate := ObsEdit.ScreenObject.Points[0].Y;
  case ObsEdit.ScreenObject.ViewDirection of
    vdTop:
      begin
        SetTopPosition(XCoordinate, YCoordinate);
      end;
    vdFront:
      begin
        SetFrontPosition(XCoordinate, YCoordinate);
      end;
    vdSide:
      begin
        SetSidePosition(YCoordinate, XCoordinate);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmManageHeadObservations.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmManageHeadObservations.comboITTChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocITT),
    comboITT.Text);
end;

procedure TfrmManageHeadObservations.comboObsPredChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocObsPred),
    comboObsPred.Text);
end;

procedure TfrmManageHeadObservations.comboStatFlagChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocStatFlag),
    comboStatFlag.Text);
end;

procedure TfrmManageHeadObservations.DisplayFilteredList;
var
  EditIndex: Integer;
  FilteredEditList: TList;
  ObsEdit: TObsEdit;
  AValue: double;
  ColIndex: Integer;
  ObjectIndex: Integer;
  RowIndex: Integer;
begin
  if FDisplayingList then
  begin
    Exit;
  end;
  rdgObservations.BeginUpdate;
  FDisplayingList := True;
  FilteredEditList := TList.Create;
  try
    for EditIndex := 0 to FObsEdits.Count - 1 do
    begin
      ObsEdit := FObsEdits[EditIndex];
      if rdgRowFilter.Cells[Ord(fcLow), Ord(frName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObsGroupName,
          rdgRowFilter.Cells[Ord(fcLow), Ord(frName)]) < 0 then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObsGroupName,
          rdgRowFilter.Cells[Ord(fcHigh), Ord(frName)]) > 0 then
        begin
          Continue;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frObsPred)] <> '' then
      begin
        if Ord(ObsEdit.Purpose) <
          rdgRowFilter.ItemIndex[Ord(fcLow), Ord(frObsPred)] then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frObsPred)] <> '' then
      begin
        if Ord(ObsEdit.Purpose) >
          rdgRowFilter.ItemIndex[Ord(fcHigh), Ord(frObsPred)] then
        begin
          Continue;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frValue)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcLow),
          Ord(frValue)], AValue) then
        begin
          if ObsEdit.Value < AValue then
          begin
            Continue;
          end;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frValue)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcHigh),
          Ord(frValue)], AValue) then
        begin
          if ObsEdit.Value > AValue then
          begin
            Continue;
          end;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frITT)] <> '' then
      begin
        if Ord(ObsEdit.MultiObsMethod) <
          rdgRowFilter.ItemIndex[Ord(fcLow), Ord(frITT)] then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frITT)] <> '' then
      begin
        if Ord(ObsEdit.MultiObsMethod) >
          rdgRowFilter.ItemIndex[Ord(fcHigh), Ord(frITT)] then
        begin
          Continue;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frObjName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObjectName,
          rdgRowFilter.Cells[Ord(fcLow), Ord(frObjName)]) < 0 then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frObjName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObjectName,
          rdgRowFilter.Cells[Ord(fcHigh), Ord(frObjName)]) > 0 then
        begin
          Continue;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frTime)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcLow),
          Ord(frTime)], AValue) then
        begin
          if AValue > ObsEdit.Time then
          begin
            Continue;
          end;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frTime)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcHigh),
          Ord(frTime)], AValue) then
        begin
          if AValue < ObsEdit.Time then
          begin
            Continue;
          end;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frStatistic)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcLow),
          Ord(frStatistic)], AValue) then
        begin
          if AValue > ObsEdit.Statistic then
          begin
            Continue;
          end;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frStatistic)] <> '' then
      begin
        if TryStrToFloat(rdgRowFilter.Cells[Ord(fcHigh),
          Ord(frStatistic)], AValue) then
        begin
          if AValue < ObsEdit.Statistic then
          begin
            Continue;
          end;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frStatFlag)] <> '' then
      begin
        if Ord(ObsEdit.StatFlag) <
          rdgRowFilter.ItemIndex[Ord(fcLow), Ord(frStatFlag)] then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frStatFlag)] <> '' then
      begin
        if Ord(ObsEdit.StatFlag) >
          rdgRowFilter.ItemIndex[Ord(fcHigh), Ord(frStatFlag)] then
        begin
          Continue;
        end;
      end;

      if rdgRowFilter.Cells[Ord(fcLow), Ord(frObsName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObsName,
          rdgRowFilter.Cells[Ord(fcLow), Ord(frObsName)]) < 0 then
        begin
          Continue;
        end;
      end;
      if rdgRowFilter.Cells[Ord(fcHigh), Ord(frObsName)] <> '' then
      begin
        if AnsiCompareText(ObsEdit.ObsName,
          rdgRowFilter.Cells[Ord(fcHigh), Ord(frObsName)]) > 0 then
        begin
          Continue;
        end;
      end;

      FilteredEditList.Add(ObsEdit);
    end;

    rdgObservations.BeginUpdate;
    try
      if FilteredEditList.Count = 0 then
      begin
        rdgObservations.RowCount := 2;
        for ColIndex := 0 to rdgObservations.ColCount - 1 do
        begin
          rdgObservations.Cells[ColIndex, 1] := '';
        end;
      end
      else
      begin
        FilteredEditList.Sort(CompareObservations);
        rdgObservations.RowCount := FilteredEditList.Count + 1;
        for ObjectIndex := 0 to FilteredEditList.Count - 1 do
        begin
          ObsEdit := FilteredEditList[ObjectIndex];
          RowIndex := ObjectIndex+1;
          rdgObservations.Objects[0, RowIndex] := ObsEdit;
          rdgObservations.Cells[Ord(ocObsGroupName), RowIndex] :=
            ObsEdit.ObsGroupName;
          rdgObservations.ItemIndex[Ord(ocObsPred), RowIndex] :=
            Ord(ObsEdit.Purpose);
          rdgObservations.Cells[Ord(ocValue), RowIndex] :=
            FloatToStr(ObsEdit.Value);
          rdgObservations.ItemIndex[Ord(ocITT), RowIndex] :=
            Ord(ObsEdit.MultiObsMethod);
          rdgObservations.Cells[Ord(ocObjName), RowIndex] :=
            ObsEdit.ObjectName;
          rdgObservations.Cells[Ord(ocTime), RowIndex] :=
            FloatToStr(ObsEdit.Time);
          rdgObservations.Cells[Ord(ocStatistic), RowIndex] :=
            FloatToStr(ObsEdit.Statistic);
          rdgObservations.ItemIndex[Ord(ocStatFlag), RowIndex] :=
            Ord(ObsEdit.StatFlag);
          rdgObservations.Cells[Ord(ocObsName), RowIndex] :=
            ObsEdit.ObsName;
        end;
      end;
    finally
      rdgObservations.EndUpdate;
    end;
  finally
    FilteredEditList.Free;
    FDisplayingList := False;
    rdgObservations.EndUpdate;
  end;
  LayoutMultiRowEditControls;
end;

procedure TfrmManageHeadObservations.edObsGroupNameChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocObsGroupName),
    string(AnsiString(edObsGroupName.Text)));
end;

procedure TfrmManageHeadObservations.FormCreate(Sender: TObject);
var
  TotalWidth: Integer;
  ColIndex: Integer;
begin
  inherited;
  pcMain.ActivePageIndex := 0;

  rdgRowFilter.Cells[Ord(fcLow), Ord(frLabel)] := StrLow;
  rdgRowFilter.Cells[Ord(fcHigh), Ord(frLabel)] := StrHigh;

  rdgRowFilter.Cells[Ord(fcLabel), Ord(frName)] := StrObservationGroupName;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frObsPred)] := StrObsPred;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frITT)] := StrITT;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frObjName)] := StrObjectName;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frValue)] := StrValue;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frTime)] := StrTime;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frStatistic)] := StrStatistic;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frStatFlag)] := StrStatFlag;
  rdgRowFilter.Rows[Ord(frStatFlag)].PickList :=
    rdgObservations.Columns[Ord(ocStatFlag)].PickList;
  rdgRowFilter.Cells[Ord(fcLabel), Ord(frObsName)] := StrObservationName;

  rdgObservations.Cells[Ord(ocObsGroupName), 0] := StrObservationGroupName;
  rdgObservations.Cells[Ord(ocObsPred), 0] := StrObsPred;
  rdgObservations.Cells[Ord(ocITT), 0] := StrITT;
  rdgObservations.Cells[Ord(ocObjName), 0] := StrObjectName;
  rdgObservations.Cells[Ord(ocValue), 0] := StrValue;
  rdgObservations.Cells[Ord(ocTime), 0] := StrTime;
  rdgObservations.Cells[Ord(ocStatistic), 0] := StrStatistic;
  rdgObservations.Cells[Ord(ocStatFlag), 0] := StrStatFlag;
  rdgObservations.Cells[Ord(ocObsName), 0] := StrObservationName;

  comboObsPred.Items := rdgObservations.Columns[Ord(ocObsPred)].PickList;
  comboITT.Items := rdgObservations.Columns[Ord(ocITT)].PickList;
  comboStatFlag.Items := rdgObservations.Columns[Ord(ocStatFlag)].PickList;

  EnableMultiEditControls;
  FObsEdits:= TObjectList.Create;

  GetData;

  TotalWidth := 20;
  for ColIndex := 0 to rdgObservations.ColCount - 1 do
  begin
    TotalWidth := TotalWidth + rdgObservations.ColWidths[ColIndex];
  end;
  if TotalWidth > Screen.Width then
  begin
    TotalWidth := Screen.Width
  end;
  if ClientWidth < TotalWidth then
  begin
    ClientWidth := TotalWidth
  end;

end;

procedure TfrmManageHeadObservations.FormDestroy(Sender: TObject);
begin
  inherited;
  FObsEdits.Free;
end;

procedure TfrmManageHeadObservations.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmManageHeadObservations.GetData;
var
  Index: Integer;
  PhastModel: TPhastModel;
  ScreenObject: TScreenObject;
  TimeIndex: Integer;
  ObsItem: THobItem;
  ObsEdit: TObsEdit;
begin
  FGettingData := True;
  try

    rdeStatistic.Visible := True;
    comboStatFlag.Visible := True;
    rdgObservations.Columns[Ord(ocStatistic)].AutoAdjustColWidths := True;
    rdgObservations.Columns[Ord(ocStatFlag)].AutoAdjustColWidths := True;
    rdgObservations.Columns[Ord(ocObsName)].AutoAdjustColWidths := True;
    rdgRowFilter.RowHeights[Ord(frStatistic)] := rdgRowFilter.DefaultRowHeight;
    rdgRowFilter.RowHeights[Ord(frStatFlag)] := rdgRowFilter.DefaultRowHeight;

    PhastModel := frmGoPhast.PhastModel;
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := PhastModel.ScreenObjects[Index];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if ScreenObject.ModflowHeadObservations <> nil then
      begin
        for TimeIndex := 0 to ScreenObject.
          ModflowHeadObservations.Values.Count - 1 do
        begin
          ObsItem := ScreenObject.
            ModflowHeadObservations.Values.HobItems[TimeIndex];
          ObsEdit := TObsEdit.Create;
          FObsEdits.Add(ObsEdit);
          ObsEdit.ScreenObject := ScreenObject;
          ObsEdit.ObsItem := ObsItem;
          ObsEdit.ObsGroupName :=
            ScreenObject.ModflowHeadObservations.ObservationName;
          ObsEdit.Purpose := ScreenObject.ModflowHeadObservations.Purpose;
          ObsEdit.Value := ObsItem.Head;
          ObsEdit.MultiObsMethod :=
            ScreenObject.ModflowHeadObservations.MultiObsMethod;
          ObsEdit.ObjectName := ScreenObject.Name;
          ObsEdit.Time := ObsItem.Time;
          ObsEdit.Statistic := ObsItem.Statistic;
          ObsEdit.StatFlag := ObsItem.StatFlag;
          ObsEdit.TimeIndex := TimeIndex;
        end;
      end;
    end;
    DisplayFilteredList;
    FLoaded := True;
    if FObsEdits.Count = 0 then
    begin
      Beep;
      MessageDlg(StrTheManageHeadObse, mtWarning, [mbOK], 0);
      ModalResult := mrCancel;
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmManageHeadObservations.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgObservations, edObsGroupName, nil, Ord(ocObsGroupName));
  LayoutControls(rdgObservations, comboObsPred, nil, Ord(ocObsPred));
  LayoutControls(rdgObservations, comboITT, nil, Ord(ocITT));
  LayoutControls(rdgObservations, rdeValue, nil, Ord(ocValue));
  LayoutControls(rdgObservations, rdeTime, nil, Ord(ocTime));
  LayoutControls(rdgObservations, rdeStatistic, nil, Ord(ocStatistic));
  LayoutControls(rdgObservations, comboStatFlag, nil, Ord(ocStatFlag));

end;

procedure TfrmManageHeadObservations.rdeStatisticChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocStatistic),
    rdeStatistic.Text);
end;

procedure TfrmManageHeadObservations.rdeTimeChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocTime),
    rdeTime.Text);
end;

procedure TfrmManageHeadObservations.rdeValueChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgObservations, Ord(ocValue),
    rdeValue.Text);
end;

procedure TfrmManageHeadObservations.rdgObservationsColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmManageHeadObservations.rdgObservationsExit(Sender: TObject);
begin
  inherited;
  TDataGridCrack(rdgObservations).HideEditor;
end;

procedure TfrmManageHeadObservations.rdgObservationsHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmManageHeadObservations.EnableMultiEditControls;
  procedure UpdateColor(Cntl: TControl);
  begin
    if Cntl.Enabled then
    begin
      TControlCrack(Cntl).Color := clWindow;
    end
    else
    begin
      TControlCrack(Cntl).Color := clBtnFace;
    end;
  end;
begin
  EnableMultiEditControl(rdgObservations, edObsGroupName, Ord(ocObsGroupName));
  EnableMultiEditControl(rdgObservations, comboObsPred, Ord(ocObsPred));
  EnableMultiEditControl(rdgObservations, comboITT, Ord(ocITT));
  EnableMultiEditControl(rdgObservations, rdeValue, Ord(ocValue));
  EnableMultiEditControl(rdgObservations, rdeTime, Ord(ocTime));
  EnableMultiEditControl(rdgObservations, rdeStatistic, Ord(ocStatistic));
  EnableMultiEditControl(rdgObservations, comboStatFlag, Ord(ocStatFlag));

  UpdateColor(edObsGroupName);
  UpdateColor(comboObsPred);
  UpdateColor(comboITT);
  UpdateColor(comboStatFlag);
end;

type TGridCrack = class(TRbwDataGrid4);

procedure TfrmManageHeadObservations.rdgObservationsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
  ObsCol: TObservationCol;
  Index: Integer;
  CM: TCompareMethod;
begin
  inherited;
  EnableMultiEditControls;

  rdgObservations.MouseToCell(X, Y, ACol, ARow);
  if (ARow = 0) and (ACol >= 0) and (ACol < rdgObservations.ColCount) then
  begin
    TGridCrack(rdgObservations).HideEditor;
    ObsCol := TObservationCol(ACol);
    for Index := 0 to SortOrder.Count - 1 do
    begin
      CM := SortOrder[Index];
      if CM.Method = ObsCol then
      begin
        SortOrder.Extract(CM);
        SortOrder.Insert(0, CM);
        DisplayFilteredList;
        break;
      end;
    end;
  end;
end;

procedure TfrmManageHeadObservations.rdgObservationsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  ObsEdit: TObsEdit;
begin
  inherited;
  if (ARow > 0) and (ACol = Ord(ocStatFlag))
    and (ARow < rdgObservations.RowCount)
    and not rdgObservations.Drawing
    then
  begin
    ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
    case ObsEdit.Purpose of
      ofObserved, ofInacative:
        begin
          rdgObservations.Columns[Ord(ocStatFlag)].PickList :=
            ObservationStatFlagLabels;
        end;
      ofPredicted:
        begin
          rdgObservations.Columns[Ord(ocStatFlag)].PickList :=
            PredictionStatFlagLabels;
        end;
      else
        begin
          Assert(false);
        end;
    end;
  end;
  CanSelect := ACol <> Ord(ocObsName);
end;

procedure TfrmManageHeadObservations.rdgObservationsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  ObsCol: TObservationCol;
  ObsEdit: TObsEdit;
  RowIndex: Integer;
  AnotherObsEdit: TObsEdit;
  AValue: double;
  AStatFlag: TStatFlag;
begin
  inherited;
  if not FLoaded then
  begin
    Exit;
  end;
  LayoutMultiRowEditControls;
  if (ACol >= 0) and (ARow > 0) and (ACol < rdgObservations.ColCount)
    and (ARow < rdgObservations.RowCount) then
  begin
    ObsCol := TObservationCol(ACol);
    case ObsCol of
      ocObsGroupName:
        begin
          ObsEdit := rdgObservations.Objects[
            0, ARow] as TObsEdit;
          ObsEdit.ObsGroupName
            := string(AnsiString(rdgObservations.Cells[Ord(ocObsGroupName), ARow]));
          if rdgObservations.Cells[Ord(ocObsGroupName), ARow]
            <> ObsEdit.ObsGroupName then
          begin
            rdgObservations.Cells[Ord(ocObsGroupName), ARow]
              := ObsEdit.ObsGroupName;
          end;

          FLoaded := False;
          try
            for RowIndex := 1 to rdgObservations.RowCount - 1 do
            begin
              AnotherObsEdit := rdgObservations.Objects[
                0, RowIndex] as TObsEdit;
              if ObsEdit.ScreenObject = AnotherObsEdit.ScreenObject then
              begin
                AnotherObsEdit.ObsGroupName := ObsEdit.ObsGroupName;
                if rdgObservations.Cells[Ord(ocObsGroupName), RowIndex]
                  <> string(AnsiString(ObsEdit.ObsGroupName)) then
                begin
                  rdgObservations.Cells[Ord(ocObsGroupName), RowIndex] :=
                    string(AnsiString(ObsEdit.ObsGroupName));
                end;
              end;
            end;
          finally
            FLoaded := True;
          end;
        end;
      ocObsPred:
        begin
          ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
          ObsEdit.Purpose := TObservationPurpose(
            rdgObservations.ItemIndex[Ord(ocObsPred), ARow]);
          FLoaded := False;
          try
            for RowIndex := 1 to rdgObservations.RowCount - 1 do
            begin
              AnotherObsEdit := rdgObservations.Objects[
                0, RowIndex] as TObsEdit;
              if ObsEdit.ScreenObject = AnotherObsEdit.ScreenObject then
              begin
                AnotherObsEdit.Purpose := ObsEdit.Purpose;
                rdgObservations.ItemIndex[Ord(ocObsPred), RowIndex] :=
                  Ord(ObsEdit.Purpose);
                if (AnotherObsEdit.Purpose = ofPredicted)
                  and not (AnotherObsEdit.StatFlag in
                  [stVariance, stStandardDev]) then
                begin
                  AnotherObsEdit.StatFlag := stVariance;
                  rdgObservations.ItemIndex[Ord(ocStatFlag), RowIndex] :=
                    Ord(stVariance);
                end;
              end;
            end;
          finally
            FLoaded := True;
          end;
        end;
      ocValue:
        begin
          if TryStrToFloat(rdgObservations.Cells[
            Ord(ocValue), ARow], AValue) then
          begin
            ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
            ObsEdit.Value := AValue;
          end;
        end;
      ocITT:
        begin
          ObsEdit := rdgObservations.Objects[
            0, ARow] as TObsEdit;
          ObsEdit.MultiObsMethod
            := TMultiObsMethod(rdgObservations.ItemIndex[Ord(ocITT), ARow]);
          FLoaded := False;
          try
            for RowIndex := 1 to rdgObservations.RowCount - 1 do
            begin
              AnotherObsEdit := rdgObservations.Objects[
                0, RowIndex] as TObsEdit;
              if ObsEdit.ScreenObject = AnotherObsEdit.ScreenObject then
              begin
                AnotherObsEdit.MultiObsMethod := ObsEdit.MultiObsMethod;
                rdgObservations.ItemIndex[Ord(ocITT), RowIndex] :=
                  Ord(ObsEdit.MultiObsMethod)
              end;
            end;
          finally
            FLoaded := True;
          end;
        end;
      ocObjName:
        begin
          ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
          ObsEdit.ObjectName
            := TScreenObject.ValidName(rdgObservations.Cells[Ord(ocObjName), ARow]);
          FLoaded := False;
          try
            for RowIndex := 1 to rdgObservations.RowCount - 1 do
            begin
              AnotherObsEdit := rdgObservations.Objects[
                0, RowIndex] as TObsEdit;
              if ObsEdit.ScreenObject = AnotherObsEdit.ScreenObject then
              begin
                AnotherObsEdit.ObjectName := ObsEdit.ObjectName;
                if RowIndex <> ARow then
                begin
                  rdgObservations.Cells[Ord(ocObjName), RowIndex] :=
                    ObsEdit.ObjectName;
                end;
              end;
            end;
          finally
            FLoaded := True;
          end;
        end;
      ocTime:
        begin
          if TryStrToFloat(rdgObservations.Cells[
            Ord(ocTime), ARow], AValue) then
          begin
            ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
            ObsEdit.Time := AValue;
          end;
        end;
      ocStatistic:
        begin
          if TryStrToFloat(rdgObservations.Cells[
            Ord(ocStatistic), ARow], AValue) then
          begin
            ObsEdit := rdgObservations.Objects[0, ARow] as TObsEdit;
            ObsEdit.Statistic := AValue;
          end;
        end;
      ocStatFlag:
        begin
          ObsEdit := rdgObservations.Objects[
            0, ARow] as TObsEdit;
          AStatFlag := TStatFlag(rdgObservations.ItemIndex[Ord(ocStatFlag), ARow]);
          if (ObsEdit.Purpose = ofPredicted)
            and not (AStatFlag in [stVariance, stStandardDev]) then
          begin
            AStatFlag := ObsEdit.StatFlag
          end;
          ObsEdit.StatFlag := AStatFlag;
          if TStatFlag(rdgObservations.ItemIndex[Ord(ocStatFlag), ARow])
            <> AStatFlag then
          begin
            rdgObservations.ItemIndex[Ord(ocStatFlag), ARow] := Ord(AStatFlag);
          end;
        end;
      ocObsName: ; // do nothing
      else Assert(False);
    end;
  end;
end;

procedure TfrmManageHeadObservations.rdgRowFilterSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if not FLoaded then
  begin
    Exit;
  end;
  DisplayFilteredList;
end;

procedure TfrmManageHeadObservations.SetData;
var
  Index: Integer;
  ObsEdit: TObsEdit;
  ScreenObjects: TList;
  NewScreenObjects: TScreenObjectEditCollection;
  OldScreenObjects: TScreenObjectEditCollection;
  ObjectPosition: Integer;
  NewItem: TScreenObjectEditItem;
  OldItem: TScreenObjectEditItem;
  ScreenObjectClass: TScreenObjectClass;
  NewHobItem: THobItem;
  Undo: TUndoSetHeadObs;
  OldChildModels: TList;
begin
  for Index := FObsEdits.Count - 1 downto 0 do
  begin
    ObsEdit := FObsEdits[Index];
    if not ObsEdit.ChangedValues then
    begin
      FObsEdits.Delete(Index);
    end;
  end;
  if FObsEdits.Count > 0 then
  begin
    NewScreenObjects := TScreenObjectEditCollection.Create;
    OldScreenObjects := TScreenObjectEditCollection.Create;
    ScreenObjects := TList.Create;
    try
      NewScreenObjects.OwnScreenObject := True;
      OldScreenObjects.OwnScreenObject := True;
      for Index := 0 to FObsEdits.Count - 1 do
      begin
        ObsEdit := FObsEdits[Index];
        ObjectPosition := ScreenObjects.IndexOf(ObsEdit.ScreenObject);
        if ObjectPosition < 0 then
        begin
          ScreenObjects.Add(ObsEdit.ScreenObject);
          ScreenObjectClass := TScreenObjectClass(
            ObsEdit.ScreenObject.ClassType);

          OldItem := OldScreenObjects.Add;
          OldItem.ScreenObject := ScreenObjectClass.Create(nil);
          OldItem.ScreenObject.Assign(ObsEdit.ScreenObject);

          NewItem := NewScreenObjects.Add;
          NewItem.ScreenObject := ScreenObjectClass.Create(nil);
          NewItem.ScreenObject.Assign(ObsEdit.ScreenObject);

          NewItem.ScreenObject.ModflowHeadObservations.ObservationName :=
            ObsEdit.ObsGroupName;
          NewItem.ScreenObject.Name := ObsEdit.ObjectName;
          NewItem.ScreenObject.ModflowHeadObservations.Purpose :=
            ObsEdit.Purpose;
          NewItem.ScreenObject.ModflowHeadObservations.MultiObsMethod :=
            ObsEdit.MultiObsMethod;
        end
        else
        begin
          NewItem := NewScreenObjects.Items[ObjectPosition];
        end;

        NewHobItem := NewItem.ScreenObject.ModflowHeadObservations.
          Values.HobItems[ObsEdit.TimeIndex];
        NewHobItem.Head := ObsEdit.Value;
        NewHobItem.Time := ObsEdit.Time;
        NewHobItem.Statistic := ObsEdit.Statistic;
        NewHobItem.StatFlag := ObsEdit.StatFlag;
      end;
      OldChildModels := nil;
      Undo := TUndoSetHeadObs.Create(ScreenObjects, NewScreenObjects,
        OldScreenObjects, OldChildModels);
      Undo.UpdateObservations;
      frmGoPhast.UndoStack.Submit(Undo);
    finally
      ScreenObjects.Free;
      NewScreenObjects.Free;
      OldScreenObjects.Free;
    end;
  end;
end;

{ TObsEdit }

function TObsEdit.ChangedValues: boolean;
begin
  result := (ScreenObject.Name <> ObjectName)
    or (ScreenObject.ModflowHeadObservations.ObservationName <> ObsGroupName)
    or (ScreenObject.ModflowHeadObservations.Purpose <> Purpose)
    or (ScreenObject.ModflowHeadObservations.MultiObsMethod <> MultiObsMethod)
    or (ObsItem.Head <> Value)
    or (ObsItem.Time <> Time)
    or (ObsItem.Statistic <> Statistic)
    or (ObsItem.StatFlag <> StatFlag)
end;

function TObsEdit.ObsName: string;
begin
  result := ScreenObject.ModflowBoundaries.ModflowHeadObservations.
    GetItemObsName(ObsItem)
end;

{ TUndoSetHeadObs }

function TUndoSetHeadObs.Description: string;
begin
  result := StrEditHeadObservatio;
end;

procedure InitializeSortOrder;
var
  Index: TObservationCol;
  CM: TCompareMethod;
begin
  SortOrder.Free;
  SortOrder := TObjectList.Create;
  for Index := Low(TObservationCol) to High(TObservationCol) do
  begin
    CM := TCompareMethod.Create;
    CM.Method := Index;
    SortOrder.Add(CM)
  end;
end;

initialization
  InitializeSortOrder;

finalization
  SortOrder.Free;

end.
