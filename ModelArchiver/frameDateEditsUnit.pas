unit frameDateEditsUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Calendar, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, FMX.TabControl, MetaDataInterfacesUnit;

type
  TframeDateEdits = class(TFrame)
    numbxYear: TNumberBox;
    calDate: TCalendar;
    Year: TLabel;
    lblMonth: TLabel;
    lblDay: TLabel;
    edMonth: TEdit;
    edDay: TEdit;
    procedure numbxYearChange(Sender: TObject);
    procedure calDateChange(Sender: TObject);
    procedure edMonthPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure edDayPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure edMonthChange(Sender: TObject);
    procedure edDayChange(Sender: TObject);
  private
    FData: IDate;
    FOnDateChanged: TNotifyEvent;
    FGettingData: Boolean;
    FAssigningDate: Boolean;
    procedure AssignDate;
    procedure AssignDateFormat;
    { Private declarations }
  public
    procedure GetDate(Data: IDate);
    property OnDateChanged: TNotifyEvent read FOnDateChanged write FOnDateChanged;
    { Public declarations }
  end;

implementation

uses
  System.DateUtils;

{$R *.fmx}

{ TframeDateEdits }

procedure TframeDateEdits.AssignDate;
begin
  if FAssigningDate then
  begin
    Exit;
  end;
  FAssigningDate := True;
  try
    numbxYear.Value := FData.Year;
    if FData.DateFormat in [dfYearMonth, dfYearMonthDay] then
    begin
      edMonth.Text := FData.Month.ToString;
      if FData.DateFormat = dfYearMonthDay then
      begin
        edDay.Text := FData.Day.ToString;
      end
      else
      begin
        edDay.Text := '';
      end;
    end
    else
    begin
      edMonth.Text := '';
      edDay.Text := '';
    end;
    if FData.Year >= 1 then
    begin
      try
        calDate.Date := EncodeDate(FData.Year, FData.Month, FData.Day);
      except on E: EConvertError do
        begin

        end;
      end;
    end;
    if (not FGettingData) and Assigned(OnDateChanged) then
    begin
      OnDateChanged(Self);
    end;
  finally
    FAssigningDate := False;
  end;
end;

procedure TframeDateEdits.calDateChange(Sender: TObject);
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  if FGettingData then
  begin
    Exit;
  end;
  DecodeDate(calDate.Date, Year, Month, Day);
  FData.Year := Year;
  FData.Month := Month;
  FData.Day := Day;
  FData.DateFormat := dfYearMonthDay;
  AssignDate;
end;

procedure TframeDateEdits.edDayChange(Sender: TObject);
var
  Day: Integer;
begin
  if FGettingData then
  begin
    Exit;
  end;
  if TryStrToInt(edDay.Text, Day) then
  begin
    FData.Day := Day;
    AssignDateFormat;
    AssignDate;
  end;
end;

procedure TframeDateEdits.edDayPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  Month: Integer;
  Day: Integer;
  Year: integer;
  MaxDays: integer;
begin
  if (edMonth.Text <> '') and (edDay.Text <> '') then
  begin
    if TryStrToInt(edMonth.Text, Day) then
    begin
      if TryStrToInt(edMonth.Text, Month) then
      begin
        if (Month >= 1) or (Month <= 12) then
        begin
          Year := Round(numbxYear.Value);
          if Year >= -9999 then
          begin
            if Year >= 1 then
            begin
              MaxDays := DaysInMonth(EncodeDate(Year, Month, 1));
            end
            else
            begin
              MaxDays := 31;
            end;

//            MaxDays := DaysInMonth(EncodeDate(Year, Month, 1));
            if (Day < 1) or (Day > MaxDays) then
            begin
              Canvas.Fill.Color := TAlphaColorRec.Red;
              Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
            end;
          end;
        end;
      end;
    end
    else
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end;
  end;
end;

procedure TframeDateEdits.edMonthChange(Sender: TObject);
var
  Month: Integer;
begin
  if FGettingData then
  begin
    Exit;
  end;
  if TryStrToInt(edMonth.Text, Month) then
  begin
    FData.Month := Month;
    AssignDateFormat;
    AssignDate;
  end;
end;

procedure TframeDateEdits.edMonthPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  Month: Integer;
begin
  if edMonth.Text <> '' then
  begin
    if TryStrToInt(edMonth.Text, Month) then
    begin
      if (Month < 1) or (Month > 12) then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
      end;
    end
    else
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end;
  end;
end;

procedure TframeDateEdits.GetDate(Data: IDate);
begin
  FData := Data;
  FGettingData := True;
  try
    AssignDate;
  finally
    FGettingData := False;
  end;
end;

procedure TframeDateEdits.AssignDateFormat;
var
  Month: Integer;
  Day: Integer;
  MaxDays: Word;
begin
  if FGettingData then
  begin
    Exit
  end;
  if FData.Year < -9999 then
  begin
    FData.DateFormat := dfYear;
  end
  else
  begin
    if TryStrToInt(edMonth.Text, Month) then
    begin
      if (Month >= 1) and (Month <= 12) then
      begin
        FData.DateFormat := dfYearMonth;
        if TryStrToInt(edDay.Text, Day) then
        begin
          if FData.Year >= 1 then
          begin
            MaxDays := DaysInMonth(EncodeDate(FData.Year, Month, 1));
          end
          else
          begin
            MaxDays := 31;
          end;
          if (Day >= 1) and (Day <= MaxDays) then
          begin
            FData.DateFormat := dfYearMonthDay;
          end;
        end;
      end
      else
      begin
        FData.DateFormat := dfYear;
      end;
    end
    else
    begin
      FData.DateFormat := dfYear;
    end;
  end;
end;

procedure TframeDateEdits.numbxYearChange(Sender: TObject);
begin
  if FGettingData then
  begin
    Exit;
  end;
  FData.Year := Round(numbxYear.Value);
  AssignDateFormat;
  AssignDate;
end;

end.
