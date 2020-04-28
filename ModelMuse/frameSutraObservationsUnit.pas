unit frameSutraObservationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameCustomSutraFeatureUnit,
  StdCtrls, Grids, RbwDataGrid4, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, UndoItemsScreenObjects, SutraBoundariesUnit,
  Generics.Collections, GoPhastTypes, SutraBoundaryUnit;

type
  TframeSutraObservations = class(TframeCustomSutraTimeVaryingFeature)
    lblObservationFormat: TLabel;
    comboObservationFormat: TComboBox;
    lblName: TLabel;
    edName: TEdit;
    procedure comboScheduleChange(Sender: TObject);
    procedure rdgSutraFeatureEndUpdate(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure comboObservationFormatChange(Sender: TObject);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure edNameExit(Sender: TObject);
    procedure rdgSutraFeatureSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    // @name is @true if all the features being displayed use the same set
    // of times.
    FTimesIdentical: boolean;
    FDisplayingTime: Boolean;
    procedure DisplayTimes(Times: TRealCollection); overload;
    procedure DisplayTimes(TimeValues: TOneDRealArray); overload;
    procedure ClearTimes;
    procedure GetScheduleName(ObsList: TSutraObsList);
    procedure GetObservationFormat(ObsList: TSutraObsList);
    procedure GetObservationTimes(ObsList: TSutraObsList);
    procedure CheckStoredTimes(ObsList: TSutraObsList);
    procedure GetObservationName(ObsList: TSutraObsList);
    { Private declarations }
  public
    procedure GetData(ScreenObjects: TScreenObjectEditCollection); override;
    procedure SetData(ScreenObjects: TScreenObjectEditCollection; SetAll,
      ClearAll: boolean); override;
    { Public declarations }
  end;

var
  frameSutraObservations: TframeSutraObservations;

resourcestring
  StrObservationTimes = 'Observation times';

implementation

uses
  ScreenObjectUnit, SutraTimeScheduleUnit, frmGoPhastUnit,
  frmSutraTimeAdjustChoiceUnit, Math;

{$R *.dfm}

{ TframeSutraObservations }

procedure TframeSutraObservations.ClearTimes;
begin
  seNumberOfTimes.AsInteger := 0;
  rdgSutraFeature.RowCount := 2;
  rdgSutraFeature.Cells[0,1] := '';
end;

procedure TframeSutraObservations.comboObservationFormatChange(Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraObservations.comboScheduleChange(Sender: TObject);
var
  SutraTimeOptions: TSutraTimeOptions;
  ASchedule: TSutraTimeSchedule;
  TimeValues: TOneDRealArray;
begin
  inherited;
  if not FGettingData and (comboSchedule.ItemIndex > 0) then
  begin
    SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
    ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
      as TSutraTimeSchedule;
    TimeValues := ASchedule.TimeValues(SutraTimeOptions.InitialTime,
      SutraTimeOptions.Schedules);
    DisplayTimes(TimeValues);
    UpdateCheckState;
  end;
end;

procedure TframeSutraObservations.DisplayTimes(TimeValues: TOneDRealArray);
var
  Index: integer;
begin
  FDisplayingTime := True;
  try
    seNumberOfTimes.AsInteger := Length(TimeValues);
    rdgSutraFeature.BeginUpdate;
    try
      rdgSutraFeature.RowCount := Length(TimeValues) + 1;
      for Index := 0 to Length(TimeValues) - 1 do
      begin
        rdgSutraFeature.Cells[0, Index+1] := FloatToStr(TimeValues[Index])
      end;
    finally
      rdgSutraFeature.EndUpdate;
    end;
  finally
    FDisplayingTime := False;
  end;
end;

procedure TframeSutraObservations.DisplayTimes(Times: TRealCollection);
var
  Index: integer;
begin
  seNumberOfTimes.AsInteger := Times.Count;
  rdgSutraFeature.BeginUpdate;
  try
    rdgSutraFeature.RowCount := Times.Count + 1;
    for Index := 0 to Times.Count - 1 do
    begin
      rdgSutraFeature.Cells[0, Index+1] := FloatToStr(Times[Index].Value)
    end;
  finally
    rdgSutraFeature.EndUpdate;
  end;
end;

procedure TframeSutraObservations.edNameChange(Sender: TObject);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraObservations.edNameExit(Sender: TObject);
begin
  inherited;
//
end;

procedure TframeSutraObservations.GetData(
  ScreenObjects: TScreenObjectEditCollection);
var
  index: Integer;
  Obs: TSutraObservations;
  ObsList: TSutraObsList;
begin
  rdgSutraFeature.BeginUpdate;
  try
    inherited;
    FGettingData := True;
    rdgSutraFeature.Cells[0,0] := StrObservationTimes;
    ObsList := TSutraObsList.Create;
    try
      for index := 0 to ScreenObjects.Count - 1 do
      begin
        Obs := ScreenObjects[index].ScreenObject.SutraBoundaries.Observations;
        if Obs.Used then
        begin
          ObsList.Add(Obs);
        end;
      end;

      if ObsList.Count = 0 then
      begin
        FCheckState := cbUnchecked;
      end
      else if ScreenObjects.Count = ObsList.Count then
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

      if ObsList.Count = 0 then
      begin
        comboSchedule.ItemIndex := 0;
        comboObservationFormat.ItemIndex := 0;
        seNumberOfTimes.AsInteger := 0;
        seNumberOfTimes.OnChange(seNumberOfTimes);
        edName.Text := '';
        Exit;
      end;

      GetObservationName(ObsList);
      GetScheduleName(ObsList);
      GetObservationFormat(ObsList);
      GetObservationTimes(ObsList);
      CheckStoredTimes(ObsList);
    finally
      ObsList.Free;
    end;
  finally
    rdgSutraFeature.EndUpdate;
    FGettingData := False;
  end;
end;

procedure TframeSutraObservations.SetData(
  ScreenObjects: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
var
  ObsList: TSutraObsList;
  index: Integer;
  Obs: TSutraObservations;
  ObsName: TCaption;
  LocalScreenObjects: TList<TScreenObject>;
  SutraTimeOptions: TSutraTimeOptions;
  ASchedule: TSutraTimeSchedule;
  TimeValues: TOneDRealArray;
  NewTimes: TRealCollection;
  RowIndex: Integer;
  AValue: Extended;
  InvalidateModelEvent: TNotifyEvent;
begin
  inherited;
  Assert(ScreenObjects.Count > 0);

  NewTimes := nil;
  LocalScreenObjects := TList<TScreenObject>.Create;
  ObsList := TSutraObsList.Create;
  try
    for index := 0 to ScreenObjects.Count - 1 do
    begin
      Obs := ScreenObjects[index].ScreenObject.SutraBoundaries.Observations;
      if ClearAll then
      begin
        Obs.Times.Clear;
      end
      else if SetAll or Obs.Used then
      begin
        ObsList.Add(Obs);
        LocalScreenObjects.Add(ScreenObjects[index].ScreenObject);
      end;
    end;

    if (ObsList.Count > 0) then
    begin
      if comboSchedule.ItemIndex > 0 then
      begin
        SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
        ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
          as TSutraTimeSchedule;
        TimeValues := ASchedule.TimeValues(SutraTimeOptions.InitialTime,
          SutraTimeOptions.Schedules);
        InvalidateModelEvent := nil;
        NewTimes := TRealCollection.Create(InvalidateModelEvent, TimeValues);
      end
      else
      begin
        InvalidateModelEvent := nil;
        NewTimes := TRealCollection.Create(InvalidateModelEvent);
        for RowIndex := 1 to seNumberOfTimes.AsInteger do
        begin
          if TryStrToFloat(rdgSutraFeature.Cells[0,RowIndex], AValue) then
          begin
            NewTimes.Add.Value := AValue;
          end;
        end;
        NewTimes.Sort;
      end;
    end;

    for index := 0 to ObsList.Count - 1 do
    begin
      Obs := ObsList[index];
      if ObsList.Count = 1 then
      begin
        ObsName := Trim(edName.Text);
        if ObsName = '' then
        begin
          ObsName := LocalScreenObjects.Items[index].Name;
        end;
        Obs.ObservationName := AnsiString(ObsName);
      end;

      if comboObservationFormat.ItemIndex >= 0 then
      begin
        Obs.ObservationFormat := TObservationFormat(comboObservationFormat.ItemIndex);
      end;

      if comboSchedule.ItemIndex > 0 then
      begin
        Obs.ScheduleName := AnsiString(comboSchedule.Text);
      end
      else
      begin
        Obs.ScheduleName := '';
      end;

      if NewTimes.Count > 0 then
      begin
        Obs.Times := NewTimes;
      end;

    end;
  finally
    ObsList.Free;
    LocalScreenObjects.Free;
    NewTimes.Free;
  end;

end;

procedure TframeSutraObservations.GetObservationName(ObsList: TSutraObsList);
var
  FirstObservations: TSutraObservations;
begin
  if ObsList.Count = 1 then
  begin
    FirstObservations := ObsList[0];
    edName.Text := string(FirstObservations.ObservationName);
  end
  else
  begin
    edName.Text := '';
  end;
end;

procedure TframeSutraObservations.btnDeleteClick(Sender: TObject);
begin
  inherited;
  comboSchedule.ItemIndex := 0;
end;

procedure TframeSutraObservations.btnInsertClick(Sender: TObject);
begin
  inherited;
  comboSchedule.ItemIndex := 0;
end;

procedure TframeSutraObservations.CheckStoredTimes(ObsList: TSutraObsList);
var
  Times: TRealCollection;
  SameValues: Boolean;
  TimeValues: TOneDRealArray;
  TimeIndex: Integer;
  FirstObservations: TSutraObservations;
  SutraTimeOptions: TSutraTimeOptions;
  ASchedule: TSutraTimeSchedule;
  AdjustChoice: TAdjustChoice;
  Form: TfrmSutraTimeAdjustChoice;
  LocalEpsilon: double;
  function NearlyTheSame(const A, B: double): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < LocalEpsilon;
    end;
  end;

//  ScheduleValues: TRealCollection;
begin
  if FTimesIdentical and (comboSchedule.ItemIndex >= 1) then
  begin
    FirstObservations := ObsList[0];
    Times := FirstObservations.Times;

    SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
    ASchedule := comboSchedule.Items.Objects[comboSchedule.ItemIndex]
      as TSutraTimeSchedule;
    TimeValues := ASchedule.TimeValues(SutraTimeOptions.InitialTime,
      SutraTimeOptions.Schedules);

    SameValues := Length(TimeValues) = Times.Count;
    if SameValues then
    begin
      LocalEpsilon := Max(Abs(TimeValues[Times.Count - 1]), Abs(Times[Times.Count - 1].Value))/1e9;
      for TimeIndex := 0 to Times.Count - 1 do
      begin
        SameValues := NearlyTheSame(TimeValues[TimeIndex], Times[TimeIndex].Value);
        if not SameValues then
        begin
          break;
        end;
      end;
    end;

    if not SameValues then
    begin
      Beep;
      Form := TfrmSutraTimeAdjustChoice.Create(nil);
      try
        Form.ShowModal;
        AdjustChoice := Form.AdjustChoice;
      finally
        Form.Free;
      end;
      case AdjustChoice of
        acUseSchedule:
          begin
            DisplayTimes(TimeValues);
          end;
        acConvert:
          begin
            comboSchedule.ItemIndex := 0;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TframeSutraObservations.GetObservationTimes(ObsList: TSutraObsList);
var
  Same: Boolean;
  Index: Integer;
  FirstObservations: TSutraObservations;
  Times: TRealCollection;
begin
  FirstObservations := ObsList[0];
  Times := FirstObservations.Times;
  Same := True;
  for Index := 1 to ObsList.Count - 1 do
  begin
    Same := Times.IsSame(ObsList[Index].Times);
    if not Same then
    begin
      Break;
    end;
  end;
  FTimesIdentical := Same;
  if Same then
  begin
    DisplayTimes(Times);
  end
  else
  begin
    ClearTimes;
  end;

end;

procedure TframeSutraObservations.GetObservationFormat(ObsList: TSutraObsList);
var
  Same: Boolean;
  Index: Integer;
  FirstObservations: TSutraObservations;
  ObservationFormat: TObservationFormat;
begin
  FirstObservations := ObsList[0];
  ObservationFormat := FirstObservations.ObservationFormat;
  Same := True;
  for Index := 1 to ObsList.Count - 1 do
  begin
    Same := ObservationFormat = ObsList[Index].ObservationFormat;
    if not Same then
    begin
      Break;
    end;
  end;
  if Same then
  begin
    comboObservationFormat.ItemIndex := Ord(ObservationFormat);
  end
  else
  begin
    comboObservationFormat.ItemIndex := -1;
  end;

end;

procedure TframeSutraObservations.GetScheduleName(ObsList: TSutraObsList);
var
  Same: Boolean;
  ScheduleName: AnsiString;
  Index: Integer;
  FirstObservations: TSutraObservations;
begin
  FirstObservations := ObsList[0];
  ScheduleName := FirstObservations.ScheduleName;
  Same := True;
  for Index := 1 to ObsList.Count - 1 do
  begin
    Same := ScheduleName = ObsList[Index].ScheduleName;
    if not Same then
    begin
      Break;
    end;
  end;
  SetScheduleIndex(ScheduleName, Same);
end;

procedure TframeSutraObservations.rdgSutraFeatureEndUpdate(Sender: TObject);
begin
  inherited;
  if (seNumberOfTimes <> nil) and (comboSchedule <> nil) then
  begin
    seNumberOfTimes.AsInteger := rdgSutraFeature.RowCount -1;
    if not FGettingData and not FDisplayingTime then
    begin
      comboSchedule.ItemIndex := 0;
    end;
  end;
end;

procedure TframeSutraObservations.rdgSutraFeatureSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateCheckState;
end;

procedure TframeSutraObservations.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  UpdateCheckState;
  if not FGettingData and not FDisplayingTime then
  begin
    comboSchedule.ItemIndex := 0;
  end;
end;

end.
