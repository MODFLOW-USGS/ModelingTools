{
  By: Richard B. Winston
  email: rbwinst@usgs.gov
  This file is in the public domain.  It may be used for any purpose.

  The main purpose of this file is to define TRangeSeries which is used to
  display values along with associated confidence intervals and/or data ranges.
}
unit ChartRangeUnit;

interface

uses Classes, TeEngine, Series, Graphics;

type
  TRangeItem = record
    Mean: Integer;
    HighCI: Integer;
    LowCI: Integer;
    HighRange: Integer;
    LowRange: Integer;
    PixelPosition: Integer;
    tmpLeftWidth: Integer;
    tmpRightWidth: Integer;
  end;

  TRangeStyle = (rsLineGraph, rsBarGraph);

  TColorList = class(TObject)
  private
    FColors: array of TColor;
    FCount: integer;
    procedure Grow;
    function GetColors(Index: integer): TColor;
    procedure SetCapacity(const Value: integer);
    procedure SetColors(Index: integer; const Value: TColor);
    function GetCapacity: integer;
  public
    Constructor Create;
    procedure Add(AColor: TColor);
    property Count: integer read FCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Colors[Index: integer]: TColor read GetColors write SetColors; default;
    procedure Clear;
  end;

  TRangeSeries = class(TCustomSeries)
  private
    FShowMean: boolean;
    FShowRange: boolean;
    FShowConfidenceIntervals: boolean;
    FLowConfidenceIntervalValues: TChartValueList;
    FUpperConfidenceIntervalValues: TChartValueList;
    FHighRangeValues: TChartValueList;
    FLowRangeValues: TChartValueList;
    FBarWidth: integer;
    FRangeStyle: TRangeStyle;
    FDrawVertical: boolean;
    FRangeColors: TColorList;
    FCiColors: TColorList;
    procedure CalcItem(ValueIndex: Integer; out AItem: TRangeItem);
    procedure SetHighRangeValues(const Value: TChartValueList);
    procedure SetLowConfidenceIntervalValues(const Value: TChartValueList);
    procedure SetLowRangeValues(const Value: TChartValueList);
    procedure SetMeanValues(const Value: TChartValueList);
    procedure SetShowConfidenceIntervals(const Value: boolean);
    procedure SetShowMean(const Value: boolean);
    procedure SetShowRange(const Value: boolean);
    procedure SetUpperConfidenceIntervalValues(
      const Value: TChartValueList);
    function GetMeanValues: TChartValueList;
    procedure SetBarWidth(const Value: integer);
    function GetDark3D: Boolean;
    function GetDraw3D: Boolean;
    procedure SetDark3D(const Value: Boolean);
    procedure SetDraw3D(const Value: Boolean);
    procedure SetRangeStyle(const Value: TRangeStyle);
    procedure SetDrawVertical(const Value: boolean);
    function MaxValue: Double;
    function MinValue: Double;
  protected
    procedure AddSampleValues(NumValues: Integer); override;
    procedure DrawValue(ValueIndex: Integer); override;
  public
    Procedure Assign(Source:TPersistent); override;
    Procedure Clear; override;
    function IsValidSourceOf(Value: TChartSeries): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddMean(const PlotPosition, Mean: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor): Integer; overload;
    function AddMean(const ALabel: string; const Mean: Double;
      AColor:TColor=clTeeColor): Integer;
      overload;
    function AddConfidenceInterval(const PlotPosition, Mean, UpperConfidenceInterval,
      LowerConfidenceInterval: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack; RangeColor: TColor = clLtGray): Integer; overload;
    function AddConfidenceInterval(const ALabel: string; const Mean,
      UpperConfidenceInterval,
      LowerConfidenceInterval: Double;
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack; RangeColor: TColor = clLtGray): Integer; overload;
    function AddRange(const PlotPosition, Mean, HighRange, LowRange: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack; RangeColor: TColor = clLtGray): Integer;
      overload;
    function AddRange(const ALabel: string; const Mean, HighRange, LowRange:
      Double; AColor:TColor=clTeeColor; CiColor: TColor = clBlack; RangeColor: TColor = clLtGray): Integer; overload;
    function AddCI_AndRange(const PlotPosition, Mean, UpperConfidenceInterval,
      LowerConfidenceInterval, HighRange, LowRange: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
      RangeColor: TColor = clLtGray): Integer; overload;
    function AddCI_AndRange(const ALabel: string; const Mean,
      UpperConfidenceInterval, LowerConfidenceInterval, HighRange,
      LowRange: Double;
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
      RangeColor: TColor = clLtGray): Integer; overload;
    function MaxXValue: Double; override;
    function MinXValue: Double; override;
    function MaxYValue: Double; override;
    function MinYValue: Double; override;
  published
    property BarWidth: integer read FBarWidth write SetBarWidth;
    property LowConfidenceIntervalValues: TChartValueList read
      FLowConfidenceIntervalValues write SetLowConfidenceIntervalValues;
    property HighRangeValues: TChartValueList read FHighRangeValues write
      SetHighRangeValues;
    property LowRangeValues: TChartValueList read FLowRangeValues write
      SetLowRangeValues;
    property MeanValues: TChartValueList read GetMeanValues write SetMeanValues;
    property RangeStyle: TRangeStyle read FRangeStyle write SetRangeStyle;
    property ShowConfidenceIntervals: boolean read FShowConfidenceIntervals write
      SetShowConfidenceIntervals;
    property ShowMean: boolean read FShowMean write SetShowMean;
    property ShowRange: boolean read FShowRange write SetShowRange;
    property UpperConfidenceIntervalValues: TChartValueList read
      FUpperConfidenceIntervalValues write SetUpperConfidenceIntervalValues;
    property Active;
    property ColorEachPoint;
    property ColorSource;
    property Cursor;
    property Depth;
    property HorizAxis;
    property Marks;
    property ParentChart;
    property DataSource;
    property PercentFormat;
    property SeriesColor;
    property ShowInLegend;
    property Title;
    property ValueFormat;
    property VertAxis;
    property XLabelsSource;
    property AfterDrawValues;
    property BeforeDrawValues;
    property OnAfterAdd;
    property OnBeforeAdd;
    property OnClearValues;
    property OnClick;
    property OnDblClick;
    property OnGetMarkText;
    property OnMouseEnter;
    property OnMouseLeave;
    property Draw3D: Boolean read GetDraw3D write SetDraw3D default False;
    property Dark3D: Boolean read GetDark3D write SetDark3D default True;
    property DrawVertical: boolean read FDrawVertical write SetDrawVertical;
  end;

procedure GetRandomRange(const Mean: Double; out HighRange, LowRange, HighCI,
  LowCI: Double);

var
  crMeanMessage,
    crUpperConfidenceIntervalMessage,
    crLowerConfidenceIntervalMessage,
    crHighRangeMessage,
    crLowRangeMessage:
  string;

implementation

uses TeCanvas;

procedure GetRandomRange(const Mean: Double; out HighRange, LowRange, HighCI,
  LowCI: Double);
var
  CI: double;
  UpperRangeInterval: double;
  LowerRangeInterval: double;
begin
  if Mean = 0 then
  begin
    CI := 0.5;
    UpperRangeInterval := 0.7;
    LowerRangeInterval := -0.7;
  end
  else
  begin
    CI := Abs(Random * Mean);
    UpperRangeInterval := CI + Abs(Random * Mean) * 0.2;
    LowerRangeInterval := CI + Abs(Random * Mean) * 0.2;
  end;
  HighRange := Mean + UpperRangeInterval;
  LowRange := Mean - LowerRangeInterval;
  HighCI := Mean + CI;
  LowCI := Mean - CI;
end;

{ TRangeSeries }

function TRangeSeries.AddCI_AndRange(const PlotPosition, Mean,
  UpperConfidenceInterval, LowerConfidenceInterval, HighRange,
  LowRange: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
      RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := UpperConfidenceInterval;
  LowConfidenceIntervalValues.TempValue := LowerConfidenceInterval;
  HighRangeValues.TempValue := HighRange;
  LowRangeValues.TempValue := LowRange;
  if DrawVertical then
  begin
    result := AddXY(PlotPosition, Mean, ALabel, AColor);
  end
  else
  begin
    result := AddXY(Mean, PlotPosition, ALabel, AColor);
  end;

end;

function TRangeSeries.AddConfidenceInterval(const PlotPosition, Mean,
  UpperConfidenceInterval, LowerConfidenceInterval: Double; Const ALabel:String='';
  AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
  RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := UpperConfidenceInterval;
  LowConfidenceIntervalValues.TempValue := LowerConfidenceInterval;
  HighRangeValues.TempValue := UpperConfidenceInterval;
  LowRangeValues.TempValue := LowerConfidenceInterval;
  if DrawVertical then
  begin
    result := AddXY(PlotPosition, Mean, ALabel, AColor);
  end
  else
  begin
    result := AddXY(Mean, PlotPosition, ALabel, AColor);
  end;

  ShowRange := False;
end;

function TRangeSeries.AddMean(const PlotPosition, Mean: Double; Const ALabel:String='';
      AColor:TColor=clTeeColor): Integer;
begin
  FRangeColors.Add(clLtGray);
  FCiColors.Add(clBlack);
  UpperConfidenceIntervalValues.TempValue := Mean;
  LowConfidenceIntervalValues.TempValue := Mean;
  HighRangeValues.TempValue := Mean;
  LowRangeValues.TempValue := Mean;
  if DrawVertical then
  begin
    result := AddXY(PlotPosition, Mean, ALabel, AColor);
  end
  else
  begin
    result := AddXY(Mean, PlotPosition, ALabel, AColor);
  end;

  ShowRange := False;
  ShowConfidenceIntervals := False;
end;

function TRangeSeries.AddRange(const PlotPosition, Mean, HighRange,
  LowRange: Double; Const ALabel:String='';
  AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
  RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := HighRange;
  LowConfidenceIntervalValues.TempValue := LowRange;
  HighRangeValues.TempValue := HighRange;
  LowRangeValues.TempValue := LowRange;
  if DrawVertical then
  begin
    result := AddXY(PlotPosition, Mean, ALabel, AColor);
  end
  else
  begin
    result := AddXY(Mean, PlotPosition, ALabel, AColor);
  end;

  ShowConfidenceIntervals := False;
end;

function TRangeSeries.AddRange(const ALabel: string; const Mean, HighRange,
  LowRange: Double; AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
  RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := HighRange;
  LowConfidenceIntervalValues.TempValue := LowRange;
  HighRangeValues.TempValue := HighRange;
  LowRangeValues.TempValue := LowRange;
  if DrawVertical then
  begin
    result := AddY(Mean, ALabel, AColor);
  end
  else
  begin
    result := AddX(Mean, ALabel, AColor);
  end;
  ShowConfidenceIntervals := False;
end;

procedure TRangeSeries.AddSampleValues(NumValues: Integer);
var
  Mean: Double;
  HighRange: Double;
  LowRange: Double;
  HighCI: Double;
  LowCI: Double;
  t: Integer;
begin
  with RandomBounds(NumValues) do
  begin
    for t := 1 to NumValues do
    begin
      Mean := MinY + Random(Round(DifY)); { starting open price }
      { Generate random figures }
      GetRandomRange(Mean, HighRange, LowRange, HighCI, LowCI);
      { call the standard add method }
      AddCI_AndRange(tmpX, Mean, HighCI, LowCI, HighRange, LowRange);
      tmpX := tmpX + StepX; { <-- next point X value }
    end;
  end;
end;

procedure TRangeSeries.CalcItem(ValueIndex: Integer;
  out AItem: TRangeItem);
begin
  if DrawVertical then
  begin
    with AItem do
    begin
      PixelPosition := CalcXPosValue(XValues.Value[ValueIndex]); { The horizontal position }

      { Vertical positions of Open, High, Low & Close values for this point }
      Mean := CalcYPosValue(MeanValues.Value[ValueIndex]);
      HighCI := CalcYPosValue(UpperConfidenceIntervalValues.Value[ValueIndex]);
      LowCI := CalcYPosValue(LowConfidenceIntervalValues.Value[ValueIndex]);
      HighRange := CalcYPosValue(HighRangeValues.Value[ValueIndex]);
      LowRange := CalcYPosValue(LowRangeValues.Value[ValueIndex]);

      tmpLeftWidth := FBarWidth div 2; { calc half bar Width }
      tmpRightWidth := FBarWidth - tmpLeftWidth;
    end;
  end
  else
  begin
    with AItem do
    begin
      PixelPosition := CalcYPosValue(YValues.Value[ValueIndex]); { The vertical position }

      { horizontal positions of Open, High, Low & Close values for this point }
      Mean := CalcXPosValue(MeanValues.Value[ValueIndex]);
      HighCI := CalcXPosValue(UpperConfidenceIntervalValues.Value[ValueIndex]);
      LowCI := CalcXPosValue(LowConfidenceIntervalValues.Value[ValueIndex]);
      HighRange := CalcXPosValue(HighRangeValues.Value[ValueIndex]);
      LowRange := CalcXPosValue(LowRangeValues.Value[ValueIndex]);

      tmpLeftWidth := FBarWidth div 2; { calc half bar Width }
      tmpRightWidth := FBarWidth - tmpLeftWidth;
    end;
  end;

end;

constructor TRangeSeries.Create(AOwner: TComponent);
begin
  inherited;
  FRangeColors := TColorList.Create;
  FCiColors := TColorList.Create;
  FDrawVertical := True;
  FShowMean := True;
  FShowRange := True;
  FShowConfidenceIntervals := True;
  FBarWidth := 10;

  YValues.Name := crMeanMessage;

  FHighRangeValues := TChartValueList.Create(Self, crHighRangeMessage);
  FLowRangeValues := TChartValueList.Create(Self, crLowRangeMessage);
  FLowConfidenceIntervalValues := TChartValueList.Create(Self,
    crLowerConfidenceIntervalMessage);
  FUpperConfidenceIntervalValues := TChartValueList.Create(Self,
    crUpperConfidenceIntervalMessage);

  Pointer.Style := psStar;
end;

type
  TPointerAccess = class(TSeriesPointer);

procedure TRangeSeries.DrawValue(ValueIndex: Integer);
var
  tmpStyle: TSeriesPointerStyle;
  tmpItem: TRangeItem;
  AColor: TColor;
  OldBrushColor, OldPenColor, RangeColor, CiColor: TColor;

begin
// Don't call inherited.
//  inherited;
  RangeColor := FRangeColors[ValueIndex];
  CiColor := FCiColors[ValueIndex];

  if ColorEachPoint then
  begin
    AColor := ValueColor[ValueIndex];
  end
  else
  begin
    AColor := SeriesColor;
  end;


  if Assigned(OnGetPointerStyle) then
    tmpStyle := OnGetPointerStyle(Self, ValueIndex)
  else
    tmpStyle := Pointer.Style;

  { Prepare Pointer Pen and Brush styles }
  TPointerAccess(Pointer).PrepareCanvas(ParentChart.Canvas, AColor);

  CalcItem(ValueIndex, tmpItem);

  with tmpItem, ParentChart {, Canvas} do
  begin
    case RangeStyle of
      rsLineGraph:
        begin
          if View3D and Pointer.Draw3D then
          begin
            if ShowRange then
            begin
              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Brush.Color := RangeColor;
                if DrawVertical then
                begin
                  Canvas.Cube(
                    PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                    HighRange, LowRange,
                    StartZ, EndZ, True);
                end
                else
                begin
                  Canvas.Cube(
                    HighRange, LowRange,
                    PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                    StartZ, EndZ, True);
                end;
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;

            if ShowConfidenceIntervals then
            begin
              Brush.Color := AColor;
              if DrawVertical then
              begin
                //here
                Canvas.RectangleY(PixelPosition - tmpLeftWidth,
                  LowCI, PixelPosition + tmpRightWidth,
                  StartZ, EndZ);

                Canvas.RectangleZ(PixelPosition, HighCI, LowCI, StartZ, EndZ);

                Canvas.RectangleY(PixelPosition - tmpLeftWidth,
                  HighCI, PixelPosition + tmpRightWidth,
                  StartZ, EndZ);
              end
              else
              begin
                Canvas.RectangleZ(LowCI, PixelPosition - tmpLeftWidth,
                  PixelPosition + tmpRightWidth,
                  StartZ, EndZ);

                Canvas.RectangleY(LowCI, PixelPosition, HighCI, StartZ, EndZ);

                Canvas.RectangleZ(HighCI, PixelPosition - tmpLeftWidth,
                  PixelPosition + tmpRightWidth,
                  StartZ, EndZ);
              end;
            end;
          end
          else
          begin
            if ShowRange then
            begin
              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Brush.Color := RangeColor;
                if View3D then
                begin
                  if DrawVertical then
                  begin
                    Canvas.Cube(
                      PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                      HighRange, LowRange, StartZ, EndZ, True);
                  end
                  else
                  begin
                    Canvas.RectangleZ(HighRange, PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                      StartZ, EndZ);
                    Canvas.RectangleZ(LowRange, PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                      StartZ, EndZ);
                  end;
                end
                else
                begin
                  if DrawVertical then
                  begin
                    Canvas.Rectangle(PixelPosition - tmpLeftWidth, HighRange,
                      PixelPosition + tmpRightWidth, LowRange);
                  end
                  else
                  begin
                    Canvas.Rectangle(HighRange, PixelPosition - tmpLeftWidth,
                      LowRange, PixelPosition + tmpRightWidth);
                  end;
                end;
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;

            if ShowConfidenceIntervals {or ShowRange} then
            begin
              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Pen.Color := CiColor;
                if View3D then
                begin
                  if DrawVertical then
                  begin
                    Canvas.RectangleZ(PixelPosition, HighCI, LowCI, StartZ, EndZ);
                  end
                  else
                  begin
                    Canvas.RectangleZ(HighCI, LowCI, PixelPosition, StartZ, EndZ);
                  end;
                end
                else
                begin
                  if DrawVertical then
                  begin
                    Canvas.DoVertLine(PixelPosition, HighCI, LowCI);
                  end
                  else
                  begin
                    Canvas.DoHorizLine(HighCI, LowCI, PixelPosition);
                  end;
                end
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;

            if ShowConfidenceIntervals then
            begin
              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Pen.Color := CiColor;
                Brush.Color := AColor;
                if DrawVertical then
                begin
                  Canvas.DoHorizLine(PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                    HighCI);
                  Canvas.DoHorizLine(PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth,
                    LowCI);
                end
                else
                begin
                  Canvas.DoVertLine(HighCI, PixelPosition - tmpLeftWidth,
                    PixelPosition + tmpRightWidth);
                  Canvas.DoVertLine(LowCI, PixelPosition - tmpLeftWidth,
                    PixelPosition + tmpRightWidth);
                end;
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;
          end;
        end;
      rsBarGraph:
        begin
          if View3D and Pointer.Draw3D then
          begin
            if ShowRange then
            begin
              if DrawVertical then
              begin
                Canvas.VertLine3D(PixelPosition, HighRange, LowRange, MiddleZ);
              end
              else
              begin
                Canvas.HorizLine3D(HighRange, LowRange, PixelPosition, MiddleZ);
              end;
            end;

            if ShowConfidenceIntervals then
            begin
              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Pen.Color := CiColor;
                Brush.Color := AColor;
                if HighCI = LowCI then
                  Pen.Color := AColor;

                Canvas.Cube(PixelPosition - tmpLeftWidth, PixelPosition + tmpRightWidth, HighCI, LowCI,
                  StartZ, EndZ, Pointer.Dark3D);
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;
          end
          else
          begin
            if ShowRange then
            begin
              if View3D then
              begin
                if DrawVertical then
                begin
                  Canvas.VertLine3D(PixelPosition, HighRange, LowRange, MiddleZ);
                end
                else
                begin
                  Canvas.HorizLine3D(HighRange, LowRange, PixelPosition, MiddleZ);
                end;
              end
              else
              begin
                if DrawVertical then
                begin
                  Canvas.DoVertLine(PixelPosition, HighRange, LowRange);
                end
                else
                begin
                  Canvas.DoHorizLine(HighRange, LowRange, MiddleZ);
                end;
              end;
            end;

            if ShowConfidenceIntervals then
            begin
              // prevent zero height rectangles
              if HighCI = LowCI then
                Dec(LowCI);

              OldBrushColor := Canvas.Brush.Color;
              OldPenColor := Canvas.Pen.Color;
              try
                Canvas.Pen.Color := CiColor;
                // draw the candle
                Brush.Color := AColor;

                if View3D then
                  Canvas.RectangleWithZ(TeeRect(PixelPosition - tmpLeftWidth, HighCI, PixelPosition +
                    tmpRightWidth, LowCI),
                    MiddleZ)
                else
                begin
                  if not Self.Pen.Visible then
                    if HighCI < LowCI then
                      Dec(HighCI)
                    else
                      Dec(LowCI);
                  Canvas.Rectangle(PixelPosition - tmpLeftWidth, HighCI, PixelPosition + tmpRightWidth + 1,
                    LowCI);
                end;
              finally
                Canvas.Brush.Color := OldBrushColor;
                Canvas.Pen.Color := OldPenColor;
              end;
            end;
          end;
        end;
    else
      Assert(False);
    end;

    if ShowMean then
    begin
      if DrawVertical then
      begin
        Pointer.DrawPointer(ParentChart.Canvas,
          ParentChart.View3D,
          PixelPosition,
          Mean,
          Pointer.HorizSize,
          Pointer.VertSize,
          AColor, tmpStyle);
      end
      else
      begin
        Pointer.DrawPointer(ParentChart.Canvas,
          ParentChart.View3D,
          Mean,
          PixelPosition,
          Pointer.HorizSize,
          Pointer.VertSize,
          AColor, tmpStyle);
      end;
    end;
  end;
end;

function TRangeSeries.GetDark3D: Boolean;
begin
  result := Pointer.Dark3D;
end;

function TRangeSeries.GetDraw3D: Boolean;
begin
  result := Pointer.Draw3D;
end;

function TRangeSeries.GetMeanValues: TChartValueList;
begin
  if DrawVertical then
  begin
    result := YValues; { overrides default YValues }
  end
  else
  begin
    result := XValues; { overrides default XValues }
  end;
end;

function TRangeSeries.IsValidSourceOf(Value: TChartSeries): Boolean;
begin
  result := Value is TRangeSeries;
end;

function TRangeSeries.MaxYValue: Double;
begin
  if DrawVertical then
  begin
    result := MaxValue;
  end
  else
  begin
    result := inherited MaxYValue;
  end;
end;

function TRangeSeries.MinYValue: Double;
begin
  if DrawVertical then
  begin
    result := MinValue;
  end
  else
  begin
    result := inherited MinYValue;
  end;
end;

procedure TRangeSeries.SetBarWidth(const Value: integer);
begin
  if FBarWidth <> Value then
  begin
    FBarWidth := Value;
    Repaint;
  end;
end;

procedure TRangeSeries.SetDark3D(const Value: Boolean);
begin
  Pointer.Dark3D := Value;
end;

procedure TRangeSeries.SetDraw3D(const Value: Boolean);
begin
  Pointer.Draw3D := Value;
end;

procedure TRangeSeries.SetHighRangeValues(const Value: TChartValueList);
begin
  SetChartValueList(FHighRangeValues, Value);
end;

procedure TRangeSeries.SetLowConfidenceIntervalValues(
  const Value: TChartValueList);
begin
  SetChartValueList(FLowConfidenceIntervalValues, Value);
end;

procedure TRangeSeries.SetLowRangeValues(const Value: TChartValueList);
begin
  SetChartValueList(FLowRangeValues, Value);
end;

procedure TRangeSeries.SetMeanValues(const Value: TChartValueList);
begin
  SetYValues(Value); { overrides default YValues }
end;

procedure TRangeSeries.SetRangeStyle(const Value: TRangeStyle);
begin
  if FRangeStyle <> Value then
  begin
    FRangeStyle := Value;
    Repaint;
  end;
end;

procedure TRangeSeries.SetShowConfidenceIntervals(const Value: boolean);
begin
  if FShowConfidenceIntervals <> Value then
  begin
    FShowConfidenceIntervals := Value;
    Repaint;
  end;
end;

procedure TRangeSeries.SetShowMean(const Value: boolean);
begin
  if FShowMean <> Value then
  begin
    FShowMean := Value;
    Repaint;
  end;
end;

procedure TRangeSeries.SetShowRange(const Value: boolean);
begin
  if FShowRange <> Value then
  begin
    FShowRange := Value;
    Repaint;
  end;
end;

procedure TRangeSeries.SetUpperConfidenceIntervalValues(
  const Value: TChartValueList);
begin
  SetChartValueList(FUpperConfidenceIntervalValues, Value);
end;

procedure SetChartRangeMessages;
begin
  crMeanMessage := 'Mean';
  crUpperConfidenceIntervalMessage := 'Upper Confidence Interval';
  crLowerConfidenceIntervalMessage := 'Lower Confidence Interval';
  crHighRangeMessage := 'High Range';
  crLowRangeMessage := 'Low Range';
end;

function TRangeSeries.AddCI_AndRange(const ALabel: string; const Mean,
  UpperConfidenceInterval, LowerConfidenceInterval, HighRange,
  LowRange: Double; AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
  RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := UpperConfidenceInterval;
  LowConfidenceIntervalValues.TempValue := LowerConfidenceInterval;
  HighRangeValues.TempValue := HighRange;
  LowRangeValues.TempValue := LowRange;
  if DrawVertical then
  begin
    result := AddY(Mean, ALabel, AColor);
  end
  else
  begin
    result := AddX(Mean, ALabel, AColor);
  end;

end;

function TRangeSeries.AddConfidenceInterval(const ALabel: string; const Mean,
  UpperConfidenceInterval, LowerConfidenceInterval: Double;
      AColor:TColor=clTeeColor; CiColor: TColor = clBlack;
      RangeColor: TColor = clLtGray): Integer;
begin
  FRangeColors.Add(RangeColor);
  FCiColors.Add(CiColor);
  UpperConfidenceIntervalValues.TempValue := UpperConfidenceInterval;
  LowConfidenceIntervalValues.TempValue := LowerConfidenceInterval;
  HighRangeValues.TempValue := UpperConfidenceInterval;
  LowRangeValues.TempValue := LowerConfidenceInterval;
  if DrawVertical then
  begin
    result := AddY(Mean, ALabel, AColor);
  end
  else
  begin
    result := AddX(Mean, ALabel, AColor);
  end;

  ShowRange := False;
end;

function TRangeSeries.AddMean(const ALabel: string;
  const Mean: Double;
      AColor:TColor=clTeeColor): Integer;
begin
  FRangeColors.Add(clLtGray);
  FCiColors.Add(clBlack);
  UpperConfidenceIntervalValues.TempValue := Mean;
  LowConfidenceIntervalValues.TempValue := Mean;
  HighRangeValues.TempValue := Mean;
  LowRangeValues.TempValue := Mean;
  if DrawVertical then
  begin
    result := AddY(Mean, ALabel, AColor);
  end
  else
  begin
    result := AddX(Mean, ALabel, AColor);
  end;

  ShowRange := False;
  ShowConfidenceIntervals := False;
end;

procedure TRangeSeries.Assign(Source: TPersistent);
begin
  if Source is TRangeSeries then
  With TRangeSeries(Source) do
  begin
    Self.BarWidth                      :=BarWidth;
    Self.ShowMean                      :=ShowMean;
    Self.ShowConfidenceIntervals       :=ShowConfidenceIntervals;
    Self.ShowRange                     :=ShowRange;
    Self.MeanValues                    :=MeanValues;
    Self.UpperConfidenceIntervalValues := UpperConfidenceIntervalValues;
    Self.LowConfidenceIntervalValues   :=LowConfidenceIntervalValues;
    Self.HighRangeValues               :=HighRangeValues;
    Self.LowRangeValues                :=LowRangeValues;
    Self.RangeStyle                    :=RangeStyle;
  end;
  inherited;

end;

procedure TRangeSeries.SetDrawVertical(const Value: boolean);
begin
  if FDrawVertical <> Value then
  begin
    FDrawVertical := Value;
    Repaint;
  end;
end;

function TRangeSeries.MaxValue: Double;
var
  temp1, temp2: double;
  resultSet: boolean;
begin
  result := 0;
  resultSet := False;
  if ShowMean then
  begin
    result := MeanValues.MaxValue;
    resultSet := True;
  end;
  if ShowConfidenceIntervals then
  begin
    temp1 := LowConfidenceIntervalValues.MaxValue;
    temp2 := UpperConfidenceIntervalValues.MaxValue;
    if temp2 > temp1 then
    begin
      temp1 := temp2;
    end;
    if resultSet then
    begin
      if temp1 > result then
      begin
        result := temp1;
      end;
    end
    else
    begin
      result := temp1;
    end;
    resultSet := True;
  end;
  if ShowRange then
  begin
    temp1 := HighRangeValues.MaxValue;
    temp2 := LowRangeValues.MaxValue;
    if temp2 > temp1 then
    begin
      temp1 := temp2;
    end;
    if resultSet then
    begin
      if temp1 > result then
      begin
        result := temp1;
      end;
    end
    else
    begin
      result := temp1;
    end;
  end;
end;
function TRangeSeries.MaxXValue: Double;
begin
  if not DrawVertical then
  begin
    result := MaxValue;
  end
  else
  begin
    result := inherited MaxXValue;
  end;
end;

function TRangeSeries.MinValue: Double;
var
  temp1, temp2: double;
  resultSet: boolean;
begin
  result := 0;
  resultSet := False;
  if ShowMean then
  begin
    result := MeanValues.MinValue;
    resultSet := True;
  end;
  if ShowConfidenceIntervals then
  begin
    temp1 := LowConfidenceIntervalValues.MinValue;
    temp2 := UpperConfidenceIntervalValues.MinValue;
    if temp2 < temp1 then
    begin
      temp1 := temp2;
    end;
    if resultSet then
    begin
      if temp1 < result then
      begin
        result := temp1;
      end;
    end
    else
    begin
      result := temp1;
    end;
    resultSet := True;
  end;
  if ShowRange then
  begin
    temp1 := HighRangeValues.MinValue;
    temp2 := LowRangeValues.MinValue;
    if temp2 < temp1 then
    begin
      temp1 := temp2;
    end;
    if resultSet then
    begin
      if temp1 < result then
      begin
        result := temp1;
      end;
    end
    else
    begin
      result := temp1;
    end;
  end;
end;


function TRangeSeries.MinXValue: Double;
begin
  if not DrawVertical then
  begin
    result := MinValue;
  end
  else
  begin
    result := inherited MinXValue;
  end;
end;

procedure TRangeSeries.Clear;
begin
  inherited;
  FRangeColors.Clear;
  FCiColors.Clear;
end;

destructor TRangeSeries.Destroy;
begin
  inherited;
  FCiColors.Free;
  FRangeColors.Free;
end;

{ TColorList }

procedure TColorList.Add(AColor: TColor);
begin
  if Capacity = FCount then
  begin
    Grow;
  end;
  FColors[FCount] := AColor;
  Inc(FCount);
end;

procedure TColorList.Clear;
begin
  SetLength(FColors, 0);
  FCount := 0;
end;

constructor TColorList.Create;
begin
  FCount := 0;
end;

function TColorList.GetCapacity: integer;
begin
  result := Length(FColors);
end;

function TColorList.GetColors(Index: integer): TColor;
begin
  result := FColors[Index];
end;

procedure TColorList.Grow;
var
  Delta: integer;
  LocalCapacity : integer;
begin
  LocalCapacity := Capacity;
  if LocalCapacity <= 16 then
  begin
    Delta := 4
  end
  else
  begin
    Delta := LocalCapacity div 4;
  end;
  Inc(LocalCapacity, Delta);
  SetLength(FColors, LocalCapacity);
end;

procedure TColorList.SetCapacity(const Value: integer);
begin
  SetLength(FColors, Value);
end;

procedure TColorList.SetColors(Index: integer; const Value: TColor);
begin
  FColors[Index] := Value;
end;

initialization
  SetChartRangeMessages;

end.

