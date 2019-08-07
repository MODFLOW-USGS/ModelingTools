unit frmTimeStepLengthCalculatorUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, JvExControls, JvgHoleShape, StdCtrls,
  ArgusDataEntry, Mask, JvExMask, JvSpin, ExtCtrls, Buttons;

type
  TfrmTimeStepLengthCalculator = class(TfrmCustomGoPhast)
    lblNumSteps: TLabel;
    seNumSteps: TJvSpinEdit;
    rdeMultiplier: TRbwDataEntry;
    lblMultiplier: TLabel;
    lblPeriodLength: TLabel;
    rdePeriodLength: TRbwDataEntry;
    pbArrow: TPaintBox;
    lblMaxLengthFirstTimeStep: TLabel;
    rdeMaxLengthFirstTimeStep: TRbwDataEntry;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    procedure pbArrowPaint(Sender: TObject);
    procedure seNumStepsChange(Sender: TObject);
  private
    procedure CalculateLength;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTimeStepLengthCalculator: TfrmTimeStepLengthCalculator;

function CalculateTimeStepLength(var NumSteps: integer;
  var PeriodLength, Multiplier: double; out TimeStepLength: double): boolean;

implementation

uses Math;

{$R *.dfm}

function CalculateTimeStepLength(var NumSteps: integer;
  var PeriodLength, Multiplier: double; out TimeStepLength: double): boolean;
begin
  with TfrmTimeStepLengthCalculator.Create(nil) do
  begin
    try
      rdePeriodLength.Text := FloatToStr(PeriodLength);
      seNumSteps.AsInteger := NumSteps;
      rdeMultiplier.Text := FloatToStr(Multiplier);
      CalculateLength;
      result := ShowModal = mrOK;
      if result then
      begin
        PeriodLength := StrToFloat(rdePeriodLength.Text);
        NumSteps := seNumSteps.AsInteger;
        Multiplier := StrToFloat(rdeMultiplier.Text);
        TimeStepLength := StrToFloat(rdeMaxLengthFirstTimeStep.Text);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmTimeStepLengthCalculator.CalculateLength;
var
  PERLEN: double;
  NSTP: integer;
  TSMULT: double;
  MaxLength: double;
begin
  if csLoading in ComponentState then Exit;
  try
    PERLEN := StrToFloat(rdePeriodLength.Text);
    NSTP := seNumSteps.AsInteger;
    TSMULT := StrToFloat(rdeMultiplier.Text);
  except on EConvertError do
    begin
      Exit;
    end;
  end;
  if TSMULT = 1 then
  begin
    MaxLength := PERLEN/NSTP;
  end
  else
  begin
    try
      MaxLength := PERLEN * (TSMULT-1)/(Power(TSMULT,NSTP)-1);
    except on E: EMathError do
      begin
        MaxLength := 0;
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
      end;
    end;
  end;
  rdeMaxLengthFirstTimeStep.Text := FloatToStr(MaxLength);
end;

procedure TfrmTimeStepLengthCalculator.pbArrowPaint(Sender: TObject);
var
  ArrowHead: array[0..2] of TPoint;
begin
  inherited;
  ArrowHead[0].X := pbArrow.Width - 20;
  ArrowHead[0].y := 0;
  ArrowHead[1].Y := pbArrow.Height div 2;
  ArrowHead[1].X := ArrowHead[0].X + ArrowHead[1].Y;
  ArrowHead[2].Y := ArrowHead[1].Y * 2;
  ArrowHead[2].X := ArrowHead[0].X;
  pbArrow.Canvas.Pen.Width := 4;
  pbArrow.Canvas.MoveTo(0,ArrowHead[1].Y);
  pbArrow.Canvas.LineTo(ArrowHead[1].X-2,ArrowHead[1].Y);
  pbArrow.Canvas.Polyline(ArrowHead);
end;

procedure TfrmTimeStepLengthCalculator.seNumStepsChange(Sender: TObject);
begin
  inherited;
  CalculateLength;
end;

end.
