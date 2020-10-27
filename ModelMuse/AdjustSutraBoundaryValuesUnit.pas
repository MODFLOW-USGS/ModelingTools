unit AdjustSutraBoundaryValuesUnit;

interface

uses
  SutraBoundariesUnit, GoPhastTypes, SutraTimeScheduleUnit, SutraBoundaryUnit,
  ModflowBoundaryUnit;

procedure AdjustBoundaryValues(ASchedule: TSutraTimeSchedule;
  SutraValues: TCustomMF_ListBoundColl);

implementation

uses
  RealListUnit, frmGoPhastUnit;

procedure AdjustBoundaryValues(ASchedule: TSutraTimeSchedule;
  SutraValues: TCustomMF_ListBoundColl);
var
  TimeIndex: Integer;
  SutraTimeOptions: TSutraTimeOptions;
  TimeValues: TOneDRealArray;
  TimeList: TRealList;
  Item: TCustomBoundaryItem;
  TimePos: Integer;
begin
  SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
  TimeValues := ASchedule.TimeValues(SutraTimeOptions.InitialTime,
     SutraTimeOptions.Schedules);

  if Length(TimeValues) = 0 then
  begin
    Exit;
  end;

  TimeList := TRealList.Create;
  try
    for TimeIndex := 0 to Length(TimeValues) - 1 do
    begin
      TimeList.Add(TimeValues[TimeIndex]);
    end;
    TimeList.Sort;

    for TimeIndex := SutraValues.Count - 1 downto 0 do
    begin
      Item := SutraValues[TimeIndex];
      TimePos := TimeList.IndexOfClosest(Item.StartTime);
      Item.StartTime := TimeList[TimePos];
    end;

    for TimeIndex := SutraValues.Count - 1 downto 1 do
    begin
      if SutraValues[TimeIndex].StartTime = SutraValues[TimeIndex-1].StartTime then
      begin
        SutraValues.Delete(TimeIndex-1);
      end;
    end;
  finally
    TimeList.Free;
  end;
end;

end.
