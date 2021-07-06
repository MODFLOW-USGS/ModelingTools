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
    procedure rdgSutraFeatureSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  public
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure seNumberOfTimesChange(Sender: TObject);
  private
    FOnCheckPestCell: TSelectCellEvent;
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifier(ACol: Integer): string;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifier(ACol: Integer; const Value: string);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    { Private declarations }
  protected
    FDeleting: Boolean;
    FGettingData: Boolean;
    procedure UpdateCheckState;
    procedure SetScheduleIndex(const ScheduleName: AnsiString; Same: Boolean);
    procedure ClearData;
    function UsedColumn: Integer; virtual; abstract;
    procedure ClearBoundaries;
    Property PestMethod[ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
  public
    procedure GetData(ScreenObjects: TScreenObjectEditCollection); virtual;
    procedure SetData(ScreenObjects: TScreenObjectEditCollection;
      SetAll, ClearAll: Boolean); virtual; abstract;
    property OnCheckPestCell: TSelectCellEvent read FOnCheckPestCell
      write FOnCheckPestCell;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, SutraTimeScheduleUnit;

resourcestring
  StrCustom = 'Custom';
  StrNone = 'none';

{$R *.dfm}

var
  FPestMethods: TStringList;

{ TframeCustomSutraFeature }

procedure TframeCustomSutraTimeVaryingFeature.btnDeleteClick(Sender: TObject);
begin
  if (rdgSutraFeature.RowCount > 2+PestRowOffset)
    and (rdgSutraFeature.Row > 0+PestRowOffset) then
  begin
    rdgSutraFeature.DeleteRow(rdgSutraFeature.Row);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;
end;

procedure TframeCustomSutraTimeVaryingFeature.btnInsertClick(Sender: TObject);
begin
  if (rdgSutraFeature.SelectedRow <= 0+ PestRowOffset)
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

function TframeCustomSutraTimeVaryingFeature.GetPestMethod(
  ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
  if PestRowOffset = 0 then
  begin
    result := ppmMultiply;
    Assert(False);
    Exit;
  end;
  ItemIndex := FPestMethods.IndexOf(
    rdgSutraFeature.Cells[ACol,PestMethodRow]);
  if ItemIndex >= 0 then
  begin
    result := TPestParamMethod(ItemIndex);
  end
  else
  begin
    result := ppmMultiply;
  end;

end;

function TframeCustomSutraTimeVaryingFeature.GetPestMethodAssigned(
  ACol: Integer): Boolean;
begin
  if PestRowOffset = 0 then
  begin
    result := False;
    Assert(False);
    Exit;
  end;
  result := FPestMethods.IndexOf(rdgSutraFeature.Cells[ACol,PestMethodRow]) >= 0;
end;

function TframeCustomSutraTimeVaryingFeature.GetPestModifier(
  ACol: Integer): string;
begin
  if PestRowOffset = 0 then
  begin
    result := '';
    Assert(False);
    Exit;
  end;
  result := rdgSutraFeature.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeCustomSutraTimeVaryingFeature.GetPestModifierAssigned(
  ACol: Integer): Boolean;
begin
  if PestRowOffset = 0 then
  begin
    result := False;
    Assert(False);
    Exit;
  end;
  result := rdgSutraFeature.Cells[ACol, PestModifierRow] <> '';
end;

procedure TframeCustomSutraTimeVaryingFeature.rdgSutraFeatureSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if Assigned(OnCheckPestCell) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end
  else
  begin
    if ARow <= PestRowOffset then
    begin
      CanSelect := False;
    end;
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
      rdgSutraFeature.RowCount := 2 + PestRowOffset;
      for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
      begin
        rdgSutraFeature.Cells[ColIndex,1+PestRowOffset] := '';
      end;
    end
    else
    begin
      rdgSutraFeature.RowCount := seNumberOfTimes.AsInteger + 1+PestRowOffset;
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

procedure TframeCustomSutraTimeVaryingFeature.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  if PestMethodRow = 0 then
  begin
    Exit;
  end;
  rdgSutraFeature.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeCustomSutraTimeVaryingFeature.SetPestMethodAssigned(
  ACol: Integer; const Value: Boolean);
begin
  if PestMethodRow = 0 then
  begin
    Exit;
  end;
  if not Value then
  begin
    rdgSutraFeature.Cells[ACol,PestMethodRow] := '';
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.SetPestModifier(ACol: Integer;
  const Value: string);
begin
  if PestRowOffset = 0 then
  begin
    Assert(False);
    Exit;
  end;
  if Value = '' then
  begin
    rdgSutraFeature.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    rdgSutraFeature.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeCustomSutraTimeVaryingFeature.SetPestModifierAssigned(
  ACol: Integer; const Value: Boolean);
begin
  if PestRowOffset = 0 then
  begin
    Assert(False);
    Exit;
  end;
  if not Value then
  begin
    rdgSutraFeature.Cells[ACol, PestModifierRow] := '';
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
  rdgSutraFeature.RowCount := 2+PestRowOffset;
  for ColIndex := 0 to rdgSutraFeature.ColCount - 1 do
  begin
    rdgSutraFeature.Cells[ColIndex,1+PestRowOffset] := '';
  end;
  rdgSutraFeature.Checked[UsedColumn,1+PestRowOffset] := False;
end;

procedure TframeCustomSutraTimeVaryingFeature.ClearData;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  rdgSutraFeature.BeginUpdate;
  try
    for RowIndex := rdgSutraFeature.FixedRows+PestRowOffset to rdgSutraFeature.RowCount - 1 do
    begin
      rdgSutraFeature.Checked[UsedColumn, RowIndex] := False;
      for ColIndex := rdgSutraFeature.FixedCols to rdgSutraFeature.
        ColCount - 1 do
      begin
        rdgSutraFeature.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    rdgSutraFeature.RowCount := 2+PestRowOffset;
    seNumberOfTimes.AsInteger := 0;
    comboSchedule.ItemIndex := 0;
  finally
    rdgSutraFeature.EndUpdate;
  end;

end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
