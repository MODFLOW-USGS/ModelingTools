// @abstract(@name registers @link(TRbwRuler) which draws a ruler.)
unit RbwRuler;

interface

uses
  Types, Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics;

type
  // See TRbwRuler.@link(TRbwRuler.RulerPosition).
  TRulerPosition = (rpLeft, rpBottom, rpRight, rpTop);
  // See TRbwRuler.@link(TRbwRuler.RulerStart).
  TStart = (sTopLeft, sBottomRight);
  // See TRbwRuler.@link(TRbwRuler.RulerTextPosition).
  TTextPosition = (tpOutside, tpInside);
  TOrientation = (orVertical, orHorizontal);

  TRbwRuler = class;
  TRulerPainter = class;

  ERulerException = class(Exception);

  {@abstract(See TRbwRuler.@link(TRbwRuler.RulerEnds).)}
  TRulerPositions = class(TPersistent)
  private
    // See @link(Upper).
    FHigh: integer;
    // See @link(Lower).
    FLow: integer;
    // @name is the @link(TRulerPainter) that owns and uses the @classname.
    FOwner: TRulerPainter;
    // See @link(Upper).
    procedure SetHigh(const Value: integer);
    // See @link(Lower).
    procedure SetLow(const Value: integer);
  public
    // If Source is a @classname, @name copies its
    // values of @link(Lower) and @link(Upper).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Owner: TRulerPainter);
  published
    // @name is the position in pixels of the lower end or the ruler.
    property Lower: integer read FLow write SetLow;
    // @name is the position in pixels of the upper end or the ruler.
    property Upper: integer read FHigh write SetHigh;
  end;

  {@abstract(See TRbwRuler.@link(TRbwRuler.RulerValues).)}
  TRulerValues = class(TPersistent)
  private
    // See @link(Upper).
    FHigh: double;
    // See @link(Lower).
    FLow: double;
    // @name is the @link(TRulerPainter) that owns and uses the @classname.
    FOwner: TRulerPainter;
    // See @link(Upper).
    procedure SetHigh(const Value: double);
    // See @link(Lower).
    procedure SetLow(const Value: double);
  public
    // If Source is a @classname, @name copies its
    // values of @link(Lower) and @link(Upper).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Owner: TRulerPainter);
  published
    // @name represents the value at the lower end of the ruler.
    property Lower: double read FLow write SetLow;
    // @name represents the value at the upper end of the ruler.
    property Upper: double read FHigh write SetHigh;
  end;

  // @name is a class used to draw the ruler on a TCanvas.
  // It does the actual drawing of the ruler in @link(TRbwRuler) but
  // it can also be used to draw a ruler on any TCanvas.
  TRulerPainter = class(TObject)
  private
    FPrecision: integer;
    FMinorTickLength: integer;
    FValues: TRulerValues;
    FMajorTickLength: integer;
    FDigits: integer;
    FStart: TStart;
    FLinePosition: integer;
    FTextPosition: TTextPosition;
    FTextOffset: integer;
    FPositions: TRulerPositions;
    FDesiredSpacing: integer;
    FRulerPosition: TRulerPosition;
    FOnInvalidate: TNotifyEvent;
    FOwner: TComponent;
    FAvailableArea: TRect;
    procedure SetDesiredSpacing(const Value: integer);
    procedure SetDigits(Value: integer);
    procedure SetLinePosition(const Value: integer);
    procedure SetMajorTickLength(const Value: integer);
    procedure SetMinorTickLength(const Value: integer);
    procedure SetPositions(const Value: TRulerPositions);
    procedure SetPrecision(Value: integer);
    procedure SetRulerPosition(const Value: TRulerPosition);
    procedure SetStart(const Value: TStart);
    procedure SetTextOffset(const Value: integer);
    procedure SetTextPosition(const Value: TTextPosition);
    procedure SetValues(const Value: TRulerValues);
    procedure Invalidate;
    function GetComponentState: TComponentState;
    // @name draws the main line of the ruler.
    procedure DrawMainLine(ACanvas: TCanvas; out OutputRect: TRect);
    // @name get the spacing between tick marks and Factor.
    procedure GetTickSpacingAndFactor(
      out Spacing: integer; out Factor: double);
    // @name gets the X or Y coordinate where the main line of the
    // ruler should be drawn.
    function GetLineDrawingPosition: integer;
    function GetOrientation: TOrientation;
    // Get the pixel equivalent of the current number.
    function GetCurrentPosition(const CurPositionR: double): integer;
    // @name draws the numbers on the ruler.
    procedure DrawLabel(ACanvas: TCanvas; const CurPositionR, Increment: double;
      const CurPositionI: integer; var FirstLabel: boolean;
      var LastLabelLT, LastLabelRB: integer; out LabelDrawn: boolean;
      out LabelRect: TRect);
    // @name draws a tick line
    procedure DrawTick(ACanvas: TCanvas; const CurPositionI: integer;
      const IsMajorTick: boolean; out OutputRect: TRect);
    // @name draws minor tick lines on the ruler
    procedure DrawMinorTicks(ACanvas: TCanvas;
      const LowCoord, Increment: double;
      const Index, Spacing: integer; DrawIntermediates: boolean;
      out OutputRect: TRect; Out TicksDrawn: boolean);
    // @name determines where the labels are drawn with respect
    // to the main line of the ruler.
    function GetTextDrawingPosition: TStart;
    // @name converts Value to a string using @link(RulerPrecision) and
    // @Link(RulerDigits).  However, if Value is
    // within Increment/RoundFactor of 0, @name returns '0'.
    function RoundNumber(Value, Increment: double): string;
    // @name is the ratio between the length that the ruler represents
    // and its length in pixels.
    function Multiplier: double;
    // @name gets the length (which may be negative) of the
    // major ticks during drawing.
    function GetDrawingMajorTickLength: integer;
    // @name gets the length (which may be negative) of the
    // minor ticks during drawing.
    function GetDrawingMinorTickLength: integer;
    function GetHeight: integer;
    function GetWidth: integer;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
    procedure GenerateRect(out OutputRect: TRect; X1, Y1, X2, Y2: Integer);
  protected
    // @name controls whether the ruler is drawn in a horizontal
    // or vertical orientation. @name is determined by @link(RulerPosition).
    property RulerOrientation: TOrientation read GetOrientation;
  public
    procedure SetDefaults;
    procedure Assign(Source: TRulerPainter);
    property ComponentState: TComponentState read GetComponentState;
    // @name creates and instance of @classname
    constructor Create(Owner: TComponent);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    { @name draws Text at the position X,Y at the angle specified by Angle.
      X and Y are in pixels (despite being real numbers).  Angle is measured
      in degrees in a clockwise direction.  The font must be a TrueType
      Font for @name to work properly.}
    class procedure PaintRotated(ACanvas: TCanvas; const X, Y, Angle: double;
      const Text: string; out LabelRect: TRect); virtual;
    // @name is the desired spacing between major ticks in
    // pixels.
    // The actual spacing will, in most cases, be slightly different from
    // @name.
    property RulerDesiredSpacing: integer read FDesiredSpacing
      write SetDesiredSpacing;
    // @name specifies how many digits will
    // appear in the exponent portion of numbers on the ruler.
    property RulerDigits: integer read FDigits write SetDigits;
    // @name specifies the positions, in pixels,
    // of the starting and ending
    // ends of the main line of the ruler.
    property RulerEnds: TRulerPositions read FPositions write SetPositions;
    // @name specifies the distance of the main line of the ruler
    // from the left, top, right, or bottom edge in pixels.
    // Which one is determined by RulerPosition.
    property RulerLinePosition: integer read FLinePosition
      write SetLinePosition;
    // @name is the length of the major ticks in pixels.
    property RulerMajorTickLength: integer read FMajorTickLength
      write SetMajorTickLength;
    // @name is the length of the minor ticks in pixels.
    property RulerMinorTickLength: integer read FMinorTickLength
      write SetMinorTickLength;
    // @name specifies the orientation of the main line on the
    // ruler. @name controls @link(RulerOrientation).
    property RulerPosition: TRulerPosition read FRulerPosition
      write SetRulerPosition;
    // @name specifies how many digits appear in the
    // number in the ruler.
    property RulerPrecision: integer read FPrecision write SetPrecision;
    // @name specifies the end of the ruler
    // that has the lower value of the ruler.
    property RulerStart: TStart read FStart write SetStart;
    // @name specifies the distance from the main line of the
    // ruler to the labels on the ruler.
    property RulerTextOffset: integer read FTextOffset write SetTextOffset;
    // @name specifies whether the labels are inside or outside
    // the ruler.
    property RulerTextPosition: TTextPosition read FTextPosition
      write SetTextPosition;
    // @name specifies the values of the lower and upper end of the
    // ruler line. Thus the ruler represents a length equal to
    // TRulerValues.@link(TRulerValues.Upper) minus
    // TRulerValues.@link(TRulerValues.Lower)
    property RulerValues: TRulerValues read FValues write SetValues;
    procedure DrawRuler(ACanvas: TCanvas; InputRect: TRect; var OutputRect: TRect);
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
  end;

  // @abstract(@name draws a ruler.)  The orientation,
  // scaling, number format, and position of the ruler
  // can be controlled through properties.  Rotated text will only
  // work if a TrueType font is used.
  TRbwRuler = class(TPaintBox)
  private
    FPainter: TRulerPainter;
    // See @link(RulerDesiredSpacing).
    procedure SetRulerDesiredSpacing(const Value: integer);
    // See @link(RulerDigits).
    procedure SetRulerDigits(Value: integer);
    // See @link(RulerLinePosition).
    procedure SetRulerLinePosition(const Value: integer);
    // See @link(RulerMajorTickLength).
    procedure SetRulerMajorTickLength(const Value: integer);
    // See @link(RulerMinorTickLength).
    procedure SetRulerMinorTickLength(const Value: integer);
    // See @link(RulerEnds).
    procedure SetRulerEnds(const Value: TRulerPositions);
    // See @link(RulerPrecision).
    procedure SetRulerPrecision(Value: integer);
    // See @link(RulerPosition).
    procedure SetRulerPosition(const Value: TRulerPosition);
    // See @link(RulerStart).
    procedure SetRulerStart(const Value: TStart);
    // See @link(RulerTextOffset).
    procedure SetRulerTextOffset(const Value: integer);
    // See @link(RulerTextPosition).
    procedure SetRulerTextPosition(const Value: TTextPosition);
    // See @link(RulerValues).
    procedure SetRulerValues(const Value: TRulerValues);
    // See @link(RulerDesiredSpacing).
    function GetRulerDesiredSpacing: integer;
    // @name is the event handler for @link(TRulerPainter.OnInvalidate).
    // It calls Invalidate.
    procedure PainterInvalidate(Sender: TObject);
    // See @link(RulerDigits).
    function GetRulerDigits: integer;
    // See @link(RulerEnds).
    function GetRulerEnds: TRulerPositions;
    // See @link(RulerLinePosition).
    function GetRulerLinePosition: integer;
    // See @link(RulerMajorTickLength).
    function GetRulerMajorTickLength: integer;
    // See @link(RulerMinorTickLength).
    function GetRulerMinorTickLength: integer;
    // See @link(RulerPosition).
    function GetRulerPosition: TRulerPosition;
    // See @link(RulerPrecision).
    function GetRulerPrecision: integer;
    // See @link(RulerStart).
    function GetRulerStart: TStart;
    // See @link(RulerTextOffset).
    function GetRulerTextOffset: integer;
    // See @link(RulerTextPosition).
    function GetRulerTextPosition: TTextPosition;
    // See @link(RulerValues).
    function GetRulerValues: TRulerValues;
    { Private declarations }
  protected
    // @name calls inherited @name and then draws the ruler.
    procedure Paint; override;
    { Protected declarations }
  public
    // @name creates and instance of @classname
    constructor Create(AOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    { @name draws Text at the position X,Y at the angle specified by Angle.
      X and Y are in pixels (despite being real numbers).  Angle is measured
      in degrees in a clockwise direction.  The font must be a TrueType
      Font for @name to work properly.}
    procedure PaintRotated(ACanvas: TCanvas; const X, Y, Angle: double;
      const Text: string); virtual;
    property Painter: TRulerPainter read FPainter;
    { Public declarations }
  published
    // @name is the desired spacing between major ticks in
    // pixels.
    // The actual spacing will, in most cases, be slightly different from
    // @name.
    property RulerDesiredSpacing: integer read GetRulerDesiredSpacing
      write SetRulerDesiredSpacing default 60;
    // @name specifies how many digits will
    // appear in the exponent portion of numbers on the ruler.
    property RulerDigits: integer read GetRulerDigits write SetRulerDigits;
    // @name specifies the positions, in pixels,
    // of the starting and ending
    // ends of the main line of the ruler.
    property RulerEnds: TRulerPositions read GetRulerEnds write SetRulerEnds;
    // @name specifies the distance of the main line of the ruler
    // from the left, top, right, or bottom edge in pixels.
    // Which one is determined by RulerPosition.
    property RulerLinePosition: integer read GetRulerLinePosition
      write SetRulerLinePosition;
    // @name is the length of the major ticks in pixels.
    property RulerMajorTickLength: integer read GetRulerMajorTickLength
      write SetRulerMajorTickLength;
    // @name is the length of the minor ticks in pixels.
    property RulerMinorTickLength: integer read GetRulerMinorTickLength
      write SetRulerMinorTickLength;
    // See @link(TRulerPainter.RulerPosition).
    property RulerPosition: TRulerPosition read GetRulerPosition
      write SetRulerPosition;
    // @name specifies how many digits appear in the
    // number in the ruler.
    property RulerPrecision: integer read GetRulerPrecision write SetRulerPrecision;
    // @name specifies the end of the ruler
    // that has the lower value of the ruler.
    property RulerStart: TStart read GetRulerStart write SetRulerStart;
    // @name specifies the distance from the main line of the
    // ruler to the labels on the ruler.
    property RulerTextOffset: integer read GetRulerTextOffset
      write SetRulerTextOffset;
    // @name specifies whether the labels are inside or outside
    // the ruler.
    property RulerTextPosition: TTextPosition read GetRulerTextPosition
      write SetRulerTextPosition;
    // @name specifies the values of the lower and upper end of the
    // ruler line. Thus the ruler represents a length equal to
    // TRulerValues.@link(TRulerValues.Upper) minus
    // TRulerValues.@link(TRulerValues.Lower)
    property RulerValues: TRulerValues read GetRulerValues write SetRulerValues;
    { Published declarations }
  end;

  // @name registers @link(TRbwRuler).
procedure Register;

implementation

uses Math;

const
  Epsilon = 1E-8;
  RoundFactor = 200;
  Ten = 10;

procedure RbwUnionRect(out ResultRect: TRect; const R1, R2: TRect);
begin
  ResultRect.Left := Min(R1.Left, R2.Left);
  ResultRect.Right := Max(R1.Right, R2.Right);
  ResultRect.Top := Min(R1.Top, R2.Top);
  ResultRect.Bottom := Max(R1.Bottom, R2.Bottom);
end;

function Power10(const Value: double): double;
begin
  result := Power(Ten, Value); //exp(ln10*Value);
end;

procedure Register;
begin
  RegisterComponents('RBW', [TRbwRuler]);
end;

procedure GetRoundMinMax(var LowRange, HighRange: double; out Increment: double;
  const factor: double);
var
  Range: double;
  Multiplier: double;
begin
  if LowRange = HighRange then
    Exit;
  Range := HighRange - LowRange;
  Increment := Range / factor;
  if Range < 0 then
  begin
    Increment := Range;
    Exit;
  end;
  Increment := Power10(Int(Log10(Increment)));
  Multiplier := (LowRange / Increment);
  if (Multiplier <> Int(Multiplier)) then
  begin
    if LowRange > 0 then
    begin
      Multiplier := Int(Multiplier) + 1;
    end
    else
    begin
      Multiplier := Int(Multiplier);
    end;

  end;
  LowRange := Increment * Multiplier;
  Multiplier := Int(HighRange / Increment + Increment / 1E8);
  if HighRange < 0 then
  begin
    Multiplier := Multiplier - 1;
  end;
  HighRange := Increment * Multiplier;
end;

{ TRbwRuler }

constructor TRbwRuler.Create(AOwner: TComponent);
begin
  inherited;
  FPainter := TRulerPainter.Create(self);
  FPainter.OnInvalidate := PainterInvalidate;
end;

destructor TRbwRuler.Destroy;
begin
  FPainter.Free;
  inherited;
end;

procedure TRbwRuler.Paint;
var
  AvailableArea: TRect;
  Dummy: TRect;
begin
  inherited;
  AvailableArea.Left := 0;
  AvailableArea.Top := 0;
  AvailableArea.Right := Width;
  AvailableArea.Bottom := Height;
  FPainter.DrawRuler(Canvas, AvailableArea, Dummy);
end;

procedure TRbwRuler.PainterInvalidate(Sender: TObject);
begin
  Invalidate;
end;

function TRbwRuler.GetRulerDesiredSpacing: integer;
begin
  result := FPainter.RulerDesiredSpacing;
end;

function TRbwRuler.GetRulerDigits: integer;
begin
  result := FPainter.RulerDigits;
end;

function TRbwRuler.GetRulerEnds: TRulerPositions;
begin
  result := FPainter.RulerEnds;
end;

function TRbwRuler.GetRulerLinePosition: integer;
begin
  result := FPainter.RulerLinePosition;
end;

function TRbwRuler.GetRulerMajorTickLength: integer;
begin
  result := FPainter.RulerMajorTickLength;
end;

function TRbwRuler.GetRulerMinorTickLength: integer;
begin
  result := FPainter.RulerMinorTickLength;
end;

function TRbwRuler.GetRulerPosition: TRulerPosition;
begin
  result := FPainter.RulerPosition;
end;

function TRbwRuler.GetRulerPrecision: integer;
begin
  result := FPainter.RulerPrecision;
end;

function TRbwRuler.GetRulerStart: TStart;
begin
  result := FPainter.RulerStart;
end;

function TRbwRuler.GetRulerTextOffset: integer;
begin
  result := FPainter.RulerTextOffset;
end;

function TRbwRuler.GetRulerTextPosition: TTextPosition;
begin
  result := FPainter.RulerTextPosition;
end;

function TRbwRuler.GetRulerValues: TRulerValues;
begin
  result := FPainter.RulerValues;
end;

procedure TRbwRuler.PaintRotated(ACanvas: TCanvas; const X, Y, Angle: double;
  const Text: string);
var
  Dummy: TRect;
begin
  FPainter.PaintRotated(ACanvas, X, Y, Angle, Text, Dummy);
end;

procedure TRbwRuler.SetRulerDesiredSpacing(const Value: integer);
begin
  FPainter.RulerDesiredSpacing := Value;
end;

procedure TRbwRuler.SetRulerLinePosition(const Value: integer);
begin
  FPainter.RulerLinePosition := Value;
end;

procedure TRbwRuler.SetRulerMajorTickLength(const Value: integer);
begin
  FPainter.RulerMajorTickLength := Value;
end;

procedure TRbwRuler.SetRulerMinorTickLength(const Value: integer);
begin
  FPainter.RulerMinorTickLength := Value;
end;

procedure TRbwRuler.SetRulerEnds(const Value: TRulerPositions);
begin
  FPainter.RulerEnds := Value;
end;

procedure TRbwRuler.SetRulerPosition(const Value: TRulerPosition);
begin
  FPainter.RulerPosition := Value;
end;

procedure TRbwRuler.SetRulerStart(const Value: TStart);
begin
  FPainter.RulerStart := Value;
end;

procedure TRbwRuler.SetRulerTextOffset(const Value: integer);
begin
  FPainter.RulerTextOffset := Value;
end;

procedure TRbwRuler.SetRulerTextPosition(const Value: TTextPosition);
begin
  FPainter.RulerTextPosition := Value;
end;

procedure TRbwRuler.SetRulerValues(const Value: TRulerValues);
begin
  FPainter.RulerValues := Value;
end;

procedure TRbwRuler.SetRulerDigits(Value: integer);
begin
  FPainter.RulerDigits := Value;
end;

procedure TRbwRuler.SetRulerPrecision(Value: integer);
begin
  FPainter.RulerPrecision := Value;
end;

{ TRulerPosition }

procedure TRulerPositions.Assign(Source: TPersistent);
begin
  if Source is TRulerPositions then
  begin
    Lower := TRulerPositions(Source).Lower;
    Upper := TRulerPositions(Source).Upper;
  end
  else
  begin
    inherited;
  end;
end;

{ TRulerValues }

procedure TRulerValues.Assign(Source: TPersistent);
begin
  if Source is TRulerValues then
  begin
    Lower := TRulerValues(Source).Lower;
    Upper := TRulerValues(Source).Upper;
  end
  else
  begin
    inherited;
  end;
end;

constructor TRulerValues.Create(Owner: TRulerPainter);
begin
  inherited Create;
  FOwner := Owner;
  FHigh := 100;
end;

procedure TRulerValues.SetHigh(const Value: double);
begin
  if FHigh <> Value then
  begin
    FHigh := Value;
    FOwner.Invalidate;
  end;
end;

procedure TRulerValues.SetLow(const Value: double);
begin
  if FLow <> Value then
  begin
    FLow := Value;
    FOwner.Invalidate;
  end;
end;

constructor TRulerPositions.Create(Owner: TRulerPainter);
begin
  inherited Create;
  FOwner := Owner;
  FHigh := 100;
end;

procedure TRulerPositions.SetHigh(const Value: integer);
begin
  if FHigh <> Value then
  begin
    FHigh := Value;
    if (FOwner <> nil) and (csDesigning in FOwner.ComponentState) and (FLow >
      FHigh) then
    begin
      FLow := FHigh - 1;
    end;
    FOwner.Invalidate;
  end;
end;

procedure TRulerPositions.SetLow(const Value: integer);
begin
  if FLow <> Value then
  begin
    FLow := Value;
    if (FOwner <> nil) and (csDesigning in FOwner.ComponentState)
      and (FLow > FHigh) then
    begin
      FHigh := FLow + 1;
    end;
    FOwner.Invalidate;
  end;
end;

{ TRulerPainter }

procedure TRulerPainter.Assign(Source: TRulerPainter);
begin
  RulerDesiredSpacing := Source.RulerDesiredSpacing;
  RulerDigits := Source.RulerDigits;
  RulerEnds := Source.RulerEnds;
  RulerLinePosition := Source.RulerLinePosition;
  RulerMajorTickLength := Source.RulerMajorTickLength;
  RulerMinorTickLength := Source.RulerMinorTickLength;
  RulerPosition := Source.RulerPosition;
  RulerPrecision := Source.RulerPrecision;
  RulerStart := Source.RulerStart;
  RulerTextOffset := Source.RulerTextOffset;
  RulerTextPosition := Source.RulerTextPosition;
  RulerValues := Source.RulerValues;
end;

constructor TRulerPainter.Create(Owner: TComponent);
begin
  inherited Create;
  FOwner := Owner;
  SetDefaults;
  FPositions := TRulerPositions.Create(self);
  FValues := TRulerValues.Create(self);
end;

destructor TRulerPainter.Destroy;
begin
  FPositions.Free;
  FValues.Free;
  inherited;
end;

procedure TRulerPainter.DrawLabel(ACanvas: TCanvas; const CurPositionR,
  Increment: double; const CurPositionI: integer; var FirstLabel: boolean;
  var LastLabelLT, LastLabelRB: integer; out LabelDrawn: boolean;
  out LabelRect: TRect);
var
  LabelString: string;
  TextSize: TSize;
  LabelWH, Other: integer;
  LabelPosition: integer;
  ThisLabelLT, ThisLabelRB: integer;
  LocalTextPosition: TStart;
  localLinePosition: integer;
  BufferSize: integer;
  X1: Integer;
  Y1: Integer;
  Extent: TSize;
begin
  LabelDrawn := False;
  LocalTextPosition := GetTextDrawingPosition;
  localLinePosition := GetLineDrawingPosition;
  LabelString := RoundNumber(CurPositionR, Increment);
  BufferSize := ACanvas.TextWidth('0') div 2;

  TextSize := ACanvas.TextExtent(LabelString);

  LabelWH := TextSize.cx;
  Other := TextSize.cy;

  if LocalTextPosition = sBottomRight then
  begin
    LabelPosition := Max(localLinePosition + RulerTextOffset,
      localLinePosition);
  end
  else
  begin
    LabelPosition := Min(localLinePosition - RulerTextOffset - Other,
      localLinePosition);
  end;

  ThisLabelLT := CurPositionI - LabelWH div 2;
  ThisLabelRB := ThisLabelLT + LabelWH;

  if FirstLabel or (ThisLabelRB + BufferSize < LastLabelLT)
    or (ThisLabelLT - BufferSize > LastLabelRB) then
  begin
    if RulerOrientation = orHorizontal then
    begin
      if (ThisLabelLT >= 0) and (ThisLabelRB < Width) then
      begin
        X1 := FAvailableArea.Left + CurPositionI - LabelWH div 2;
        Y1 := FAvailableArea.Top + LabelPosition;
        ACanvas.TextOut(X1, Y1, LabelString);
        Extent := ACanvas.TextExtent(LabelString);
        LabelRect.Left := X1;
        LabelRect.Top := Y1;
        LabelRect.Right := X1 + Extent.cx;
        LabelRect.Bottom := Y1 + Extent.cy;
        FirstLabel := False;
        LastLabelLT := ThisLabelLT;
        LastLabelRB := ThisLabelRB;
        LabelDrawn := True;
      end;
    end
    else
    begin
      if (ThisLabelLT >= 0) and (ThisLabelRB < Height) then
      begin
        PaintRotated(ACanvas, FAvailableArea.Left + LabelPosition,
          FAvailableArea.Top + CurPositionI + LabelWH / 2, -90, LabelString,
          LabelRect);
        FirstLabel := False;
        LastLabelLT := ThisLabelLT;
        LastLabelRB := ThisLabelRB;
        LabelDrawn := True;
      end;
    end;
  end;
end;

procedure TRulerPainter.DrawMainLine(ACanvas: TCanvas; out OutputRect: TRect);
var
  localLinePosition: integer;
  X1: Integer;
  Y1: Integer;
  X2: Integer;
  Y2: Integer;
begin
  localLinePosition := GetLineDrawingPosition;
  if RulerOrientation = orHorizontal then
  begin
    X1 := FAvailableArea.Left + RulerEnds.Lower;
    Y1 := FAvailableArea.Top + localLinePosition;
    X2 := FAvailableArea.Left + RulerEnds.Upper;
    Y2 := FAvailableArea.Top + localLinePosition;
  end
  else
  begin
    X1 := FAvailableArea.Left + localLinePosition;
    Y1 := FAvailableArea.Top + RulerEnds.Lower;
    X2 := FAvailableArea.Left + localLinePosition;
    Y2 := FAvailableArea.Top + RulerEnds.Upper;
  end;
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
  GenerateRect(OutputRect, X1, Y1, X2, Y2);
end;

procedure TRulerPainter.DrawMinorTicks(ACanvas: TCanvas; const LowCoord,
  Increment: double; const Index, Spacing: integer; DrawIntermediates: boolean;
  out OutputRect: TRect; Out TicksDrawn: boolean);
var
  MinorTickIndex: integer;
  CurPositionR: double;
  CurPositionI: integer;
  ARect: TRect;

begin
  TicksDrawn := False;
  if DrawIntermediates and (Spacing * Ten >= RulerDesiredSpacing) then
  begin
    for MinorTickIndex := 1 to 9 do
    begin
      CurPositionR := LowCoord + Increment * Index
        + Increment / Ten * MinorTickIndex;
      if (CurPositionR > RulerValues.Lower - Increment / 20)
        and (CurPositionR < RulerValues.Upper + Increment / 20) then
      begin
        if RulerStart = sTopLeft then
        begin
          CurPositionI := RulerEnds.Lower + Round((CurPositionR -
            RulerValues.Lower) / Multiplier);
        end
        else
        begin
          CurPositionI := RulerEnds.Upper - Round((CurPositionR -
            RulerValues.Lower) / Multiplier);
        end;
        if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <=
          RulerEnds.Upper) then
        begin
          DrawTick(ACanvas, CurPositionI, (MinorTickIndex = 5), ARect);
          if TicksDrawn then
          begin
            RbwUnionRect(OutputRect, OutputRect, ARect);
          end
          else
          begin
            OutputRect := ARect;
            TicksDrawn := True;
          end;
        end;
      end;
    end;
  end
  else if Spacing * Ten >= RulerDesiredSpacing then
  begin
    CurPositionR := LowCoord + Increment * Index
      + Increment / 2;
    if (CurPositionR > RulerValues.Lower - Increment / 20)
      and (CurPositionR < RulerValues.Upper + Increment / 20) then
    begin
      if RulerStart = sTopLeft then
      begin
        CurPositionI := RulerEnds.Lower + Round((CurPositionR -
          RulerValues.Lower) / Multiplier);
      end
      else
      begin
        CurPositionI := RulerEnds.Upper - Round((CurPositionR -
          RulerValues.Lower) / Multiplier);
      end;
      if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <=
        RulerEnds.Upper) then
      begin
        DrawTick(ACanvas, CurPositionI, DrawIntermediates, ARect);
        if TicksDrawn then
        begin
          RbwUnionRect(OutputRect, OutputRect, ARect);
        end
        else
        begin
          OutputRect := ARect;
          TicksDrawn := True;
        end;
      end;
    end;
  end;
end;

procedure TRulerPainter.DrawRuler(ACanvas: TCanvas; InputRect: TRect; var OutputRect: TRect);
var
  LowCoord, HighCoord: double;
  Increment: double;
  Index: integer;
  CurPositionR: double;
  CurPositionI: integer;
  factor: double;
  Spacing: integer;
  LastLabelLT, LastLabelRB: integer;
  FirstLabel: boolean;
  LabelDrawn: boolean;
  DrawIntermediates: boolean;
  AStyle: TPenStyle;
  ARect: TRect;
  TicksDrawn: Boolean;
begin
  if (RulerEnds.Lower = RulerEnds.Upper)
    or (RulerValues.Lower = RulerValues.Upper) then
  begin
    Exit;
  end;

  FAvailableArea := InputRect;

  AStyle := ACanvas.Pen.Style;
  try
    ACanvas.Pen.Style := psSolid;

    DrawMainLine(ACanvas, OutputRect);

    //Determine Tick Spacing
    GetTickSpacingAndFactor(Spacing, Factor);

    // Get the Increment.
    factor := factor / Ten;
    LowCoord := RulerValues.Lower;
    HighCoord := RulerValues.Upper;
    GetRoundMinMax(LowCoord, HighCoord, Increment, factor);

    // initialize variables.
    Index := 0;
    CurPositionR := LowCoord;
    FirstLabel := True;
    LastLabelLT := 0;
    LastLabelRB := 0;
    DrawIntermediates := True;
    while CurPositionR < RulerValues.Upper + Increment / 2 do
    begin
      CurPositionI := GetCurrentPosition(CurPositionR);
      // If the major tick is inside the ends of the ruler, draw and label it.
      if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <= RulerEnds.Upper)
        then
      begin
        DrawLabel(ACanvas, CurPositionR, Increment, CurPositionI, FirstLabel,
          LastLabelLT, LastLabelRB, LabelDrawn, ARect);
        if LabelDrawn then
        begin
          RbwUnionRect(OutputRect, OutputRect, ARect);
        end;

        DrawIntermediates := DrawIntermediates and LabelDrawn;

        DrawTick(ACanvas, CurPositionI, LabelDrawn, ARect);
        if LabelDrawn then
        begin
          RbwUnionRect(OutputRect, OutputRect, ARect);
        end;
      end;

      // update variables
      Inc(Index);
      CurPositionR := LowCoord + Increment * Index;
    end;

    // initialize variables.
    Index := -1;
    CurPositionR := LowCoord;
    FirstLabel := True;
    LastLabelLT := 0;
    LastLabelRB := 0;
    while CurPositionR < RulerValues.Upper + Increment / 2 do
    begin
      DrawMinorTicks(ACanvas, LowCoord, Increment, Index, Spacing,
      DrawIntermediates, ARect, TicksDrawn);
      if TicksDrawn then
      begin
        RbwUnionRect(OutputRect, OutputRect, ARect);
      end;

      // update variables
      Inc(Index);
      CurPositionR := LowCoord + Increment * Index;
    end;
  finally
    ACanvas.Pen.Style := AStyle
  end;  
end;

procedure TRulerPainter.SetDefaults;
begin
  FDesiredSpacing := 60;
  RulerMinorTickLength := 10;
  RulerMajorTickLength := 20;
  FLinePosition := 30;
  FTextOffset := 5;
  FPrecision := 5;
  FDigits := 1;
  FTextPosition := tpOutside;
end;

procedure TRulerPainter.GenerateRect(out OutputRect: TRect; X1, Y1, X2, Y2: Integer);
begin
  OutputRect.Left := Min(X1, X2);
  OutputRect.Right := Max(X1, X2);
  OutputRect.Top := Min(Y1, Y2);
  OutputRect.Bottom := Max(Y1, Y2);
end;

procedure TRulerPainter.DrawTick(ACanvas: TCanvas; const CurPositionI: integer;
  const IsMajorTick: boolean; out OutputRect: TRect);
var
  localLinePosition: integer;
  X1: Integer;
  Y1: Integer;
  X2: Integer;
  Y2: Integer;
begin
  localLinePosition := GetLineDrawingPosition;
  if RulerOrientation = orHorizontal then
  begin
    X1 := FAvailableArea.Left + CurPositionI;
    Y1 := FAvailableArea.Top + localLinePosition;
    ACanvas.MoveTo(X1, Y1);
    if IsMajorTick then
    begin
      X2 := FAvailableArea.Left + CurPositionI;
      Y2 := FAvailableArea.Top + localLinePosition + GetDrawingMajorTickLength;
      ACanvas.LineTo(X2, Y2);
    end
    else
    begin
      X2 := FAvailableArea.Left + CurPositionI;
      Y2 := FAvailableArea.Top + localLinePosition + GetDrawingMinorTickLength;
      ACanvas.LineTo(X2, Y2);
    end;
  end
  else
  begin
    X1 := FAvailableArea.Left + localLinePosition;
    Y1 := FAvailableArea.Top + CurPositionI;
    ACanvas.MoveTo(X1, Y1);
    if IsMajorTick then
    begin
      X2 := FAvailableArea.Left + localLinePosition + GetDrawingMajorTickLength;
      Y2 := FAvailableArea.Top + CurPositionI;
      ACanvas.LineTo(X2, Y2);
    end
    else
    begin
      X2 := FAvailableArea.Left + localLinePosition + GetDrawingMinorTickLength;
      Y2 := FAvailableArea.Top + CurPositionI;
      ACanvas.LineTo(X2, Y2);
    end;
  end;
  GenerateRect(OutputRect, X1, Y1, X2, Y2);
end;

function TRulerPainter.GetComponentState: TComponentState;
begin
  if FOwner = nil then
  begin
    result := [];
  end
  else
  begin
    result := FOwner.ComponentState;
  end;
end;

function TRulerPainter.GetCurrentPosition(const CurPositionR: double): integer;
begin
  if RulerStart = sTopLeft then
  begin
    result := RulerEnds.Lower + Round((CurPositionR - RulerValues.Lower) /
      Multiplier);
  end
  else
  begin
    result := RulerEnds.Upper - Round((CurPositionR - RulerValues.Lower) /
      Multiplier);
  end;
end;

function TRulerPainter.GetDrawingMajorTickLength: integer;
begin
  result := RulerMajorTickLength;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    result := -result;
  end;
end;

function TRulerPainter.GetDrawingMinorTickLength: integer;
begin
  result := RulerMinorTickLength;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    result := -result;
  end;
end;

function TRulerPainter.GetHeight: integer;
begin
  result := FAvailableArea.Bottom - FAvailableArea.Top;
end;

function TRulerPainter.GetLineDrawingPosition: integer;
begin
  result := RulerLinePosition;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    if RulerPosition = rpBottom then
    begin
      result := Height - result;
    end
    else
    begin
      result := Width - result;
    end;
  end;
end;

function TRulerPainter.GetOrientation: TOrientation;
begin
  if (FRulerPosition = rpLeft) or (FRulerPosition = rpRight) then
  begin
    result := orVertical;
  end
  else
  begin
    result := orHorizontal;
  end;
end;

function TRulerPainter.GetTextDrawingPosition: TStart;
begin
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    if RulerTextPosition = tpOutside then
    begin
      result := sBottomRight;
    end
    else
    begin
      result := sTopLeft;
    end;
  end
  else
  begin
    if RulerTextPosition = tpOutside then
    begin
      result := sTopLeft;
    end
    else
    begin
      result := sBottomRight;
    end;
  end;
end;

procedure TRulerPainter.GetTickSpacingAndFactor(out Spacing: integer;
  out Factor: double);
var
  LowCoord, HighCoord: double;
  Increment: double;
  PriorSpacing: Integer;
begin
  Factor := Ten;
  LowCoord := RulerValues.Lower;
  HighCoord := RulerValues.Upper;
  try
    GetRoundMinMax(LowCoord, HighCoord, Increment, factor);
    Spacing := Round(Increment / Multiplier);
  except on E: ERangeError do
    begin
      raise ERulerException.Create(
        'RulerValues.Upper = ' + RulerValues.Upper.ToString
        + '; RulerValues.Lower = ' + RulerValues.Lower.ToString
        + '; RulerEnds.Upper = ' + RulerEnds.Upper.ToString
        + '; RulerEnds.Lower = ' + RulerEnds.Lower.ToString
        + Format(' The error message was %s', [E.Message]));
    end;
//  Multiplier := (RulerValues.Upper - RulerValues.Lower)
//    / (RulerEnds.Upper - RulerEnds.Lower);

  end;
  PriorSpacing := Spacing;
  while Spacing > RulerDesiredSpacing do
  begin
    Factor := Factor * Ten;
    LowCoord := RulerValues.Lower;
    HighCoord := RulerValues.Upper;
    GetRoundMinMax(LowCoord, HighCoord, Increment, factor);
    Spacing := Round(Increment / Multiplier);
    if Spacing = PriorSpacing then
    begin
      Exit;
    end;
    PriorSpacing := Spacing;
  end;
end;

function TRulerPainter.GetWidth: integer;
begin
  result := FAvailableArea.Right - FAvailableArea.Left;
end;

procedure TRulerPainter.Invalidate;
begin
  if Assigned(OnInvalidate) then
  begin
    OnInvalidate(self);
  end;
end;

function TRulerPainter.Multiplier: double;
begin
  Multiplier := (RulerValues.Upper - RulerValues.Lower)
    / (RulerEnds.Upper - RulerEnds.Lower);
end;

class procedure TRulerPainter.PaintRotated(ACanvas: TCanvas; const X, Y,
  Angle: double; const Text: string; out LabelRect: TRect);
var
  LogFont: TLogFont;
  AFont: TFont;
  X1: integer;
  Y1: integer;
  AngleRadians: double;
  Extent: TSize;
  X2: Integer;
  Y2: Integer;
begin
  AFont := TFont.Create;
  try
    AFont.Assign(ACanvas.Font);

    GetObject(AFont.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfEscapement := Round(-Angle * 10);
    LogFont.lfOrientation := Round(-Angle * 10);
    AFont.Handle := CreateFontIndirect(LogFont);
    ACanvas.Font.Assign(AFont);
    X1 := Round(X);
    Y1 := Round(Y);

    LabelRect.Left := X1;
    LabelRect.Right := X1;
    LabelRect.Top := Y1;
    LabelRect.Bottom := Y1;

    ACanvas.TextOut(X1, Y1, Text);
    Extent := ACanvas.TextExtent(Text);
    AngleRadians := Angle * Pi / 180;

    X2 := X1 + Round(Cos(AngleRadians)*Extent.cx);
    Y2 := Y1 + Round(Sin(AngleRadians)*Extent.cx);

    LabelRect.Left := Min(LabelRect.Left,X2);
    LabelRect.Right := Max(LabelRect.Right,X2);
    LabelRect.Top := Min(LabelRect.Top,Y2);
    LabelRect.Bottom := Max(LabelRect.Bottom,Y2);

    X2 := X1 + Round(Cos(AngleRadians)*Extent.cx - Sin(AngleRadians)*Extent.cy);
    Y2 := Y1 + Round(Sin(AngleRadians)*Extent.cx + Cos(AngleRadians)*Extent.cy);

    LabelRect.Left := Min(LabelRect.Left,X2);
    LabelRect.Right := Max(LabelRect.Right,X2);
    LabelRect.Top := Min(LabelRect.Top,Y2);
    LabelRect.Bottom := Max(LabelRect.Bottom,Y2);

    X2 := X1 + Round(-Sin(AngleRadians)*Extent.cy);
    Y2 := Y1 + Round(+ Cos(AngleRadians)*Extent.cy);

    LabelRect.Left := Min(LabelRect.Left,X2);
    LabelRect.Right := Max(LabelRect.Right,X2);
    LabelRect.Top := Min(LabelRect.Top,Y2);
    LabelRect.Bottom := Max(LabelRect.Bottom,Y2);

    DeleteObject(ACanvas.Font.Handle);
    ACanvas.Font := AFont;
  finally
    AFont.Free;
  end;
end;

function TRulerPainter.RoundNumber(Value, Increment: double): string;
begin
  if (Value + Increment / RoundFactor > 0)
    and (Value - Increment / RoundFactor < 0) then
  begin
    result := '0';
  end
  else
  begin
    result := FloatToStrF(Value, ffNumber, RulerPrecision, RulerDigits);
  end;
end;

procedure TRulerPainter.SetDesiredSpacing(const Value: integer);
begin
  if FDesiredSpacing <> Value then
  begin
    FDesiredSpacing := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetDigits(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if Value > 4 then
  begin
    Value := 4;
  end;
  if Value <> FDigits then
  begin
    FDigits := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetLinePosition(const Value: integer);
begin
  if FLinePosition <> Value then
  begin
    FLinePosition := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetMajorTickLength(const Value: integer);
begin
  if FMajorTickLength <> Value then
  begin
    FMajorTickLength := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetMinorTickLength(const Value: integer);
begin
  if FMinorTickLength <> Value then
  begin
    FMinorTickLength := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetPositions(const Value: TRulerPositions);
begin
  FPositions.Assign(Value);
  Invalidate;
end;

procedure TRulerPainter.SetPrecision(Value: integer);
begin
  if Value < 1 then
  begin
    Value := 1;
  end
  else if Value > 15 then
  begin
    Value := 15;
  end;
  if Value <> FPrecision then
  begin
    FPrecision := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetRulerPosition(const Value: TRulerPosition);
begin
  if FRulerPosition <> Value then
  begin
    FRulerPosition := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetStart(const Value: TStart);
begin
  if FStart <> Value then
  begin
    FStart := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetTextOffset(const Value: integer);
begin
  if FTextOffset <> Value then
  begin
    FTextOffset := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetTextPosition(const Value: TTextPosition);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TRulerPainter.SetValues(const Value: TRulerValues);
begin
  FValues.Assign(Value);
  Invalidate;
end;

end.

