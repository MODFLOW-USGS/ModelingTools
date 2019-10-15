unit frameCustomSutraFeatureUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Grids,
  RbwDataGrid4, Buttons, Mask, JvExMask, JvSpin, ExtCtrls,
  UndoItemsScreenObjects, GoPhastTypes, frameActivatibleFeatureUnit;

type
  TframeCustomSutraTimeVaryingFeature = class(TframeActivatibleFeature)
    pnlBottom: TPanel;
    lblNumTimes: TLabel;
    seNumberOfTimes: TJvSpinEdit;
    btnDelete: TBitBtn;
    btnInsert: TBitBtn;
    pnlGrid: TPanel;
    rdgSutraFeature: TRbwDataGrid4;
    pnlTop: TPanel;
    lblSchedule: TLabel;
    pnlCaption: TPanel;
    comboSchedule: TComboBox;
    procedure comboScheduleChange(Sender: TObject);
  public
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure seNumberOfTimesChange(Sender: TObject);
  private
    FDeleting: Boolean;
//    FOnActivate: TActivateEvent;
    { Private declarations }
  protected
    FGettingData: Boolean;
    procedure UpdateCheckState;
    procedure SetScheduleIndex(const ScheduleName: AnsiString; Same: Boolean);
    procedure ClearData;
    function UsedColumn: Integer; virtual; abstract;
    procedure ClearBoundaries;
  public
    procedure GetData(ScreenObjects: TScreenObjectEditCollection); virtual;
    procedure SetData(ScreenObjects: TScreenObjectEditCollection;
      SetAll, ClearAll: Boolean); virtual; abstract;
//    property OnActivate: TActivateEvent read FOnActivate write FOnActivate;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, SutraTimeScheduleUnit;

resourcestring
  StrCustom = 'Custom';

{$R *.dfm}

{ TframeCustomSutraFeature }

procedure TframeCustomSutraTimeVaryingFeature.btnDeleteClick(Sender: TObject);
begin
  if (rdgSutraFeature.RowCount > 2)
    and (rdgSutraFeature.Row> 0) then
  begin
    rdgSutraFeature.DeleteRow(rdgSutraFeature.Row);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;
end;

procedure TframeCustomSutraTimeVaryingFeature.btnInsertClick(Sender: TObject);
begin
  if (rdgSutraFeature.SelectedRow <= 0)
    or (rdgSutraFeature.SelectedRow >= rdgSutraFeature.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  if (seNumberOfTimes.AsInteger > 0) then
  begin
    rdgSutraFeature.InsertRow(rdgSutraFeature.SelectedRow);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger +1;
end;

procedure TframeCustomSutraTimeVaryingFeature.comboScheduleChange(Sender: TObject);
begin
  UpdateCheckState;
end;

procedure TframeCustomSutraTimeVaryingFeature.GetData(
  ScreenObjects: TScreenObjectEditCollection);
var
  Schedules: TSutraTimeSchedules;
  index: Integer;
  ASchedule: TSutraTimeSchedule;
  Times: TOneDRealArray;
  PickList: TStrings;
begin
  comboSchedule.Items.Clear;
  comboSchedule.Items.Add(StrCustom);
  Schedules := frmGoPhast.PhastModel.SutraTimeOptions.Schedules;
  for index := 0 to Schedules.Count - 1 do
  begin
    ASchedule := Schedules[index].Schedule;
    comboSchedule.Items.AddObject(string(ASchedule.Name), ASchedule);
  end;

  ASchedule := Schedules[0].Schedule;
  Times := ASchedule.TimeValues(
    frmGoPhast.PhastModel.SutraTimeOptions.InitialTime, Schedules);
  PickList := rdgSutraFeature.Columns[0].PickList;
  PickList.Clear;
  PickList.Capacity := Length(Times);
  for index := 0 to Length(Times) - 1 do
  begin
    PickList.Add(FloatToStr(Times[index]));
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.seNumberOfTimesChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  FDeleting := True;
  try
    if seNumberOfTimes.AsInteger = 0 then
    begin
      rdgSutraFeature.RowCount := 2;
      for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
      begin
        rdgSutraFeature.Cells[ColIndex,1] := '';
      end;
    end
    else
    begin
      rdgSutraFeature.RowCount := seNumberOfTimes.AsInteger + 1;
    end;
    btnDelete.Enabled := seNumberOfTimes.AsInteger >= 1;
    rdgSutraFeature.Invalidate;
  finally
    FDeleting := False;
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.UpdateCheckState;
begin
  if not FGettingData and (FCheckState = cbUnchecked) and
    (seNumberOfTimes.AsInteger > 0) then
  begin
    FCheckState := cbChecked;
    if Assigned(OnActivate) then
    begin
      OnActivate(self, FCheckState);
    end;
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.SetScheduleIndex(const ScheduleName
  : AnsiString; Same: Boolean);
var
  ScheduleIndex: Integer;
begin
  if Same then
  begin
    ScheduleIndex := comboSchedule.Items.IndexOf(string(ScheduleName));
    if ScheduleIndex >= 0 then
    begin
      comboSchedule.ItemIndex := ScheduleIndex;
    end
    else
    begin
      comboSchedule.ItemIndex := 0;
    end;
  end
  else
  begin
    comboSchedule.ItemIndex := -1;
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.ClearBoundaries;
var
  ColIndex: Integer;
begin
  seNumberOfTimes.AsInteger := 0;
  rdgSutraFeature.RowCount := 2;
  for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
  begin
    rdgSutraFeature.Cells[ColIndex,1] := '';
  end;
  rdgSutraFeature.Checked[UsedColumn,1] := False;
end;

procedure TframeCustomSutraTimeVaryingFeature.ClearData;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  rdgSutraFeature.BeginUpdate;
  try
    for RowIndex := rdgSutraFeature.FixedRows to rdgSutraFeature.RowCount - 1 do
    begin
      rdgSutraFeature.Checked[UsedColumn, RowIndex] := False;
      for ColIndex := rdgSutraFeature.FixedCols to rdgSutraFeature.
        ColCount - 1 do
      begin
        rdgSutraFeature.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    rdgSutraFeature.RowCount := 2;
    seNumberOfTimes.AsInteger := 0;
    comboSchedule.ItemIndex := 0;
  finally
    rdgSutraFeature.EndUpdate;
  end;

end;

end.
