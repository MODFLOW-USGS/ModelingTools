// @abstract(@name provides method for converting
// from a fraction in the range of 0 to 1
// to a color.)
// The data for most of these color schemes comes from
// http://geography.uoregon.edu/datagraphics/color_scales.htm

unit ColorSchemes;

interface

uses Classes, Graphics, GoPhastTypes, Generics.Collections,
  Generics.Defaults, ColorSchemesInterface;

type
  // @name is used to index @link(TColorArray).
  TColorComponents = (ccRed, ccGreen, ccBlue);

  {@name is used to specify a color.  Each member of @name
   is a fraction in the range 0 to 1 inclusive.  Each fraction
   represents the degree of saturation with a particular color component
   For example, the following specifies a color in which the red component
   is 25% saturated, the green component is 50% saturated and the blue
   component is 75% saturated.

   @longcode(#
   var
     ColorArray: TColorArray;
   begin
     ColorArray[ccRed]   := 0.25;
     ColorArray[ccGreen] := 0.50;
     ColorArray[ccBlue]  := 0.75;
   end;
   #)
  }
  TColorArray = array[TColorComponents] of double;

// @name converts Fraction to a TColor using a user-supplied
// look-up table.  The color that is returned will be interpolated
// from value in the table.
function FracLookup(Fraction: real;
  const LookUpTable: array of TColorArray): TColor;

// @name converts Fraction to a color ranging from blue to dark orange.
function FracToBlueDarkOrange(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from blue to gray.
function FracToBlueGray(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from blue to green.
function FracToBlueGreen(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from blue to orange.
function FracToBlueOrange(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from blue to red.
function FracToBlueRed(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from brown to blue.
function FracToBrownBlue(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from green to magenta.
function FracToGreenMagenta(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from red to green to blue.
function FracToSpectrum(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from blue to orangish red.
function FracToBlue_OrangeRed(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color ranging from light blue to dark blue.
function FracToLightBlue_DarkBlue(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a spectral color but avoiding green.
function ModifiedSpectralScheme(Fraction, ColorAdjustmentFactor: real): TColor;

// @name converts Fraction to a color using a scheme that
// varies the base color between one of five choices and within
// each of those choices varies the chroma and intensity.
function SteppedSequential(Fraction, ColorAdjustmentFactor: real): TColor;

function FracAndSchemeToColor(ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;


type
  TColorItem = class(TInterfacedPhastCollectionItem, IColorItem)
  private
    FColor: TColor;
    FStoredFraction: TRealStorage;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetStoredFraction(const Value: TRealStorage);
    procedure Changed(Sender: TObject);
    function GetFraction: double;
    procedure SetFraction(const Value: double);
//    function GetIndex: Integer;
//    procedure SetIndex(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Fraction: double read GetFraction write SetFraction;
    procedure Assign(Source: TPersistent); override;
//    property Index: Integer read GetIndex write SetIndex;
  published
    property Color: TColor read GetColor write SetColor;
    property StoredFraction: TRealStorage read FStoredFraction
      write SetStoredFraction;
  end;
  TColorComparer = TComparer<IColorItem>;
  TColorList = TList<IColorItem>;

  TColorCollection = class(TPhastCollection, IColorCollection)
  private
    FSorted: Boolean;
    function GetItem(Index: Integer): IColorItem;
    procedure SetItem(Index: Integer; const Value: IColorItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    function Add: IColorItem;
    property Items[Index: Integer]: IColorItem read GetItem write SetItem; default;
    procedure SortColors;
  end;

  TUserDefinedColorSchemeItem = class(TInterfacedPhastCollectionItem, IUserDefinedColorSchemeItem)
  private
    FColors: TColorCollection;
    FName: string;
    function GetColors: TColorCollection;
    procedure SetColors(const Value: TColorCollection);
    function GetColorI: IColorCollection;
    procedure SetColorI(const Value: IColorCollection);
    procedure SetName(const Value: string);
    function GetName: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ColorsI: IColorCollection read GetColorI write SetColorI;
  published
    property Colors: TColorCollection read GetColors write SetColors;
    property Name: string read GetName write SetName;
  end;

  TUserDefinedColorSchemeCollection = class(TPhastCollection, IUserDefinedColorSchemeCollection)
  private
    function GetItem(Index: Integer): IUserDefinedColorSchemeItem;
    procedure SetItem(Index: Integer; const Value: IUserDefinedColorSchemeItem);
  public
    function Add: IUserDefinedColorSchemeItem;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: IUserDefinedColorSchemeItem
      read GetItem write SetItem; default;
  end;

  TColorParameters = class(TPersistent)
  private
    procedure Changed;
  strict private
    FColorScheme: integer;
    FColorExponent: real;
    FColorCycles: integer;
    FOnChange: TNotifyEvent;
    procedure SetColorCycles(Value: integer);
    procedure SetColorExponent(const Value: real);
    procedure SetColorScheme(Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    procedure Clear;
    {
     @name converts a fraction between 0 and 1 to
     a color using the selected
     TModel.@link(ColorScheme), @link(ColorCycles),
     and TModel.@link(ColorExponent).
    }
    function FracToColor(Fraction: real): TColor; overload;
    function FracToColor(Fraction: real;
      ColorScheme: IUserDefinedColorSchemeItem): TColor; overload;
    function IsSame(OtherParamaters: TColorParameters): Boolean;
  published
    // @name is used to specify how many times the range of colors specified
    // by @link(ColorScheme) is repeated when assigning colors to grid cells
    // or contours.
    property ColorCycles: integer read FColorCycles
      write SetColorCycles default 1;
    // @name is used to specify which color scheme is used to color grid cells
    // or contours.
    // See @link(FracToColor).
    property ColorScheme: integer read FColorScheme write SetColorScheme;
    // @name is used to adjust the colors.
    property ColorExponent: real read FColorExponent write SetColorExponent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const MaxColorScheme = 11;

implementation

uses Math, PhastModelInterfaceUnit;

var
  ColorParameters: TColorParameters;

procedure AdjustColorFactor(var ColorAdjustmentFactor: Real);
begin
  if ColorAdjustmentFactor = 2 then
  begin
    ColorAdjustmentFactor := MAXINT;
  end
  else if ColorAdjustmentFactor > 1 then
  begin
    ColorAdjustmentFactor := 1 / (2 - ColorAdjustmentFactor);
  end;
end;

function FracToSpectrum(Fraction, ColorAdjustmentFactor: real): TColor;
// The value of ColorAdjustmentFactor was determined emperically.
//const
//  ColorAdjustmentFactor = 0.7; // 0.7 works well on laptop.
//  ColorAdjustmentFactor = 0.4; // 0.4 works well on desktop but is horrible on laptop.
var
  Choice: integer;
begin
  AdjustColorFactor(ColorAdjustmentFactor);
  Assert((Fraction >= 0) and (Fraction <= 1));
  fraction := Fraction * 4;
  Choice := Trunc(fraction);
  fraction := Frac(fraction);

  // Linear interpolation results in too wide a green band and
  // yellow and aqua bands that are too narrow.
  // The following transformation evens things out.
  // The value of ColorAdjustmentFactor was determined empirically.
  if Odd(Choice) then
  begin
    fraction := 1- Power((1-fraction), ColorAdjustmentFactor);
  end
  else
  begin
    fraction := Power(fraction, ColorAdjustmentFactor);
  end;

  case Choice of
    0:
      begin
        // R -> R+G
        // Colors ranging from Red to Yellow
        result := Round(Fraction * $FF) * $100 + $FF;
      end;
    1:
      begin
        // R+G -> G
        // Colors ranging from Yellow to Green
        result := $FF00 + Round((1 - Fraction) * $FF);
      end;
    2:
      begin
        // G -> G+B
        // Colors ranging from Green to Aqua
        result := Round(Fraction * $FF) * $10000 + $FF00;
      end;
    3:
      begin
        // G+B -> B
        // Colors ranging from Aqua to Blue
        result := $FF0000 + Round((1 - Fraction) * $FF) * $100;
      end;
    4:
      begin
        // Blue
        result := $FF0000; // + Round(Fraction * $FF); // B -> B+R
      end;
    {    5:
          begin
            result := $FF00FF; // B
          end;    }
  else
    begin
      result := 0;
      Assert(False);
    end;
  end;
end;

procedure AdjustFractionAndColorFactor(ColorAdjustmentFactor: Real;
  var Fraction: Real);
begin
  AdjustColorFactor(ColorAdjustmentFactor);
  if fraction < 0.5 then
  begin
    fraction := (1-Power(-(fraction-0.5)*2,ColorAdjustmentFactor))/2;
  end
  else
  begin
    fraction := (1+Power((fraction-0.5)*2,ColorAdjustmentFactor))/2;
  end;
end;

function FracToGreenMagenta(Fraction, ColorAdjustmentFactor: real): TColor;
var
  Choice: integer;
begin
  Assert((Fraction >= 0) and (Fraction <= 1));
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  fraction := Fraction * 3 + 0.5;
  Choice := Trunc(fraction);
  fraction := Frac(fraction);
  case Choice of
    0:
      begin
        result := Round(Fraction * $FF) * $100;
      end;
    1:
      begin
        result := $FF00 + Round(Fraction * $FF) + Round(Fraction * $FF) *
          $10000;
      end;
    2:
      begin
        result := $FF + $FF0000 + Round((1 - Fraction) * $FF) * $100;
      end;
    3:
      begin
        result := Round((1 - Fraction) * $FF) * $10000 + Round((1 - Fraction) *
          $FF);
      end;
  else
    begin
      result := 0;
      Assert(False);
    end;
  end;
end;

function FracLookup(Fraction: real; const LookUpTable: array of TColorArray):
  TColor;
var
  Choice: integer;
  RedFraction: double;
  GreenFraction: double;
  BlueFraction: double;
  LookUpLength: integer;
begin
  Assert((Fraction >= 0) and (Fraction <= 1));
  LookUpLength := length(LookUpTable) - 1;
  fraction := Fraction * LookUpLength;
  Choice := Trunc(fraction);
  fraction := Frac(fraction);
  if Choice = LookUpLength then
  begin
    RedFraction := LookUpTable[LookUpLength, ccRed];
    GreenFraction := LookUpTable[LookUpLength, ccGreen];
    BlueFraction := LookUpTable[LookUpLength, ccBlue];
  end
  else
  begin
    RedFraction := (1 - fraction) * LookUpTable[Choice, ccRed]
      + fraction * LookUpTable[Choice + 1, ccRed];
    GreenFraction := (1 - fraction) * LookUpTable[Choice, ccGreen]
      + fraction * LookUpTable[Choice + 1, ccGreen];
    BlueFraction := (1 - fraction) * LookUpTable[Choice, ccBlue]
      + fraction * LookUpTable[Choice + 1, ccBlue];
  end;

  Assert((RedFraction >= 0) and (RedFraction <= 1));
  Assert((GreenFraction >= 0) and (GreenFraction <= 1));
  Assert((BlueFraction >= 0) and (BlueFraction <= 1));

  result := Round(RedFraction * $FF)
    + Round(GreenFraction * $FF) * $100
    + Round(BlueFraction * $FF) * $10000;
end;

function FracToBlueRed(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..17] of TColorArray =
  (
    (0.142, 0, 0.85),
    (0.097, 0.112, 0.97),
    (0.16, 0.342, 1),
    (0.24, 0.531, 1),
    (0.34, 0.692, 1),
    (0.46, 0.829, 1),
    (0.6, 0.92, 1),
    (0.74, 0.978, 1),
    (0.92, 1, 1),
    (1, 1, 0.92),
    (1, 0.948, 0.74),
    (1, 0.84, 0.6),
    (1, 0.676, 0.46),
    (1, 0.472, 0.34),
    (1, 0.24, 0.24),
    (0.97, 0.155, 0.21),
    (0.85, 0.085, 0.187),
    (0.65, 0, 0.13)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBlueDarkOrange(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..17] of TColorArray =
  (
    (0, 0.4, 0.4),
    (0, 0.6, 0.6),
    (0, 0.8, 0.8),
    (0, 1, 1),
    (0.2, 1, 1),
    (0.4, 1, 1),
    (0.6, 1, 1),
    (0.7, 1, 1),
    (0.8, 1, 1),
    (0.9, 1, 1),
    (1, 0.9, 0.8),
    (1, 0.793, 0.6),
    (1, 0.68, 0.4),
    (1, 0.56, 0.2),
    (1, 0.433, 0),
    (0.8, 0.333, 0),
    (0.6, 0.24, 0),
    (0.4, 0.153, 0)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBlueGreen(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..13] of TColorArray =
  (
    (0, 0, 1),
    (0.2, 0.2, 1),
    (0.4, 0.4, 1),
    (0.6, 0.6, 1),
    (0.7, 0.7, 1),
    (0.8, 0.8, 1),
    (0.9, 0.9, 1),
    (0.9, 1, 0.9),
    (0.8, 1, 0.8),
    (0.7, 1, 0.7),
    (0.6, 1, 0.6),
    (0.4, 1, 0.4),
    (0.2, 1, 0.2),
    (0, 1, 0)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBrownBlue(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..11] of TColorArray =
  (
    (0.2, 0.1, 0),
    (0.4, 0.187, 0),
    (0.6, 0.379, 0.21),
    (0.8, 0.608, 0.48),
    (0.85, 0.688, 0.595),
    (0.95, 0.855, 0.808),
    (0.8, 0.993, 1),
    (0.6, 0.973, 1),
    (0.4, 0.94, 1),
    (0.2, 0.893, 1),
    (0, 0.667, 0.8),
    (0, 0.48, 0.6)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBlueGray(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..7] of TColorArray =
  (
    (0, 0.6, 0.8),
    (0.4, 0.9, 1),
    (0.6, 1, 1),
    (0.8, 1, 1),
    (0.9, 0.9, 0.9),
    (0.6, 0.6, 0.6),
    (0.4, 0.4, 0.4),
    (0.2, 0.2, 0.2)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBlueOrange(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..11] of TColorArray =
  (
    (0, 0.167, 1),
    (0.1, 0.4, 1),
    (0.2, 0.6, 1),
    (0.4, 0.8, 1),
    (0.6, 0.933, 1),
    (0.8, 1, 1),
    (1, 1, 0.8),
    (1, 0.933, 0.6),
    (1, 0.8, 0.4),
    (1, 0.6, 0.2),
    (1, 0.4, 0.1),
    (1, 0.167, 0)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToBlue_OrangeRed(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..13] of TColorArray =
  (
    (0.03, 0.353, 1),
    (0.2, 0.467, 1),
    (0.35, 0.567, 1),
    (0.55, 0.7, 1),
    (0.75, 0.833, 1),
    (0.9, 0.933, 1),
    (0.97, 0.98, 1),
    (1, 1, 0.8),
    (1, 1, 0.6),
    (1, 1, 0),
    (1, 0.8, 0),
    (1, 0.6, 0),
    (1, 0.4, 0),
    (1, 0, 0)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function FracToLightBlue_DarkBlue(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..9] of TColorArray =
  (
    (0.9, 1, 1),
    (0.8, 0.983, 1),
    (0.7, 0.95, 1),
    (0.6, 0.9, 1),
    (0.5, 0.833, 1),
    (0.4, 0.75, 1),
    (0.3, 0.65, 1),
    (0.2, 0.533, 1),
    (0.1, 0.4, 1),
    (0, 0.25, 1)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function ModifiedSpectralScheme(Fraction, ColorAdjustmentFactor: real): TColor;
const
  LookUpTable: array[0..10] of TColorArray =
  (
   (0.650,   0.000,   0.130),
   (0.850,   0.150,   0.196),
   (0.970,   0.430,   0.370),
   (1.000,   0.680,   0.450),
   (1.000,   0.880,   0.600),
   (1.000,   1.000,   0.750),
   (0.880,   1.000,   1.000),
   (0.670,   0.970,   1.000),
   (0.450,   0.850,   1.000),
   (0.250,   0.630,   1.000),
   (0.150,   0.300,   1.000)
    );
begin
  AdjustFractionAndColorFactor(ColorAdjustmentFactor, Fraction);
  result := FracLookup(Fraction, LookUpTable);
end;

function SteppedSequential(Fraction, ColorAdjustmentFactor: real): TColor;
var
  TableChoice: integer;
const
  LookUpTable4: array[0..4] of TColorArray =
  (
   (0.6, 0.058824, 0.058824     ),
   (0.701961, 0.176471, 0.176471),
   (0.8, 0.321569, 0.321569     ),
   (0.901961, 0.494118, 0.494118),
   (1, 0.698039, 0.698039       )
    );

  LookUpTable3: array[0..4] of TColorArray =
  (
   (0.6, 0.329412, 0.058824     ),
   (0.701961, 0.439216, 0.176471),
   (0.8, 0.560784, 0.321569     ),
   (0.901961, 0.698039, 0.494118),
   (1, 0.85098, 0.698039        )
    );

  LookUpTable2: array[0..4] of TColorArray =
  (
   (0.419608, 0.6, 0.058824     ),
   (0.52549, 0.701961, 0.176471 ),
   (0.639216, 0.8, 0.321569     ),
   (0.764706, 0.901961, 0.494118),
   (0.898039, 1, 0.698039       )
    );

  LookUpTable1: array[0..4] of TColorArray =
  (
   (0.058824, 0.419608, 0.6     ),
   (0.176471, 0.52549, 0.701961 ),
   (0.321569, 0.639216, 0.8     ),
   (0.494118, 0.764706, 0.901961),
   (0.698039, 0.898039, 1       )
    );

  LookUpTable0: array[0..4] of TColorArray =
  (
   (0.14902, 0.058824, 0.6      ),
   (0.262745, 0.176471, 0.701961),
   (0.4, 0.321569, 0.8          ),
   (0.564706, 0.494118, 0.901961),
   (0.74902, 0.698039, 1        )
    );
begin
  if ColorAdjustmentFactor = 2 then
  begin
    ColorAdjustmentFactor := MAXINT;
  end
  else if ColorAdjustmentFactor > 1 then
  begin
    ColorAdjustmentFactor := 1/(2-ColorAdjustmentFactor);
  end;
  fraction := fraction * 5;
  TableChoice := Trunc(fraction);
  fraction := Frac(fraction);
  if TableChoice = 5 then
  begin
    TableChoice := 4;
    fraction := 1;
  end;
  fraction := 1- Power((1-fraction), ColorAdjustmentFactor);

  case TableChoice of
    0:
      begin
        result := FracLookup(Fraction, LookUpTable0);
      end;
    1:
      begin
        result := FracLookup(Fraction, LookUpTable1);
      end;
    2:
      begin
        result := FracLookup(Fraction, LookUpTable2);
      end;
    3:
      begin
        result := FracLookup(Fraction, LookUpTable3);
      end;
    4:
      begin
        result := FracLookup(Fraction, LookUpTable4);
      end;
  else
    Assert(False);
    result := clBlack;
  end;
end;

{
function FracToDarkRed_Blue(Fraction: real): TColor;
const
  LookUpTable: array[0..17] of TColorArray =
  (
   (0.142,   0.000,   0.850),
   (0.097,   0.112,   0.970),
   (0.160,   0.342,   1.000),
   (0.240,   0.531,   1.000),
   (0.340,   0.692,   1.000),
   (0.460,   0.829,   1.000),
   (0.600,   0.920,   1.000),
   (0.740,   0.978,   1.000),
   (0.920,   1.000,   1.000),
   (1.000,   1.000,   0.920),
   (1.000,   0.948,   0.740),
   (1.000,   0.840,   0.600),
   (1.000,   0.676,   0.460),
   (1.000,   0.472,   0.340),
   (1.000,   0.240,   0.240),
   (0.970,   0.155,   0.210),
   (0.850,   0.085,   0.187),
   (0.650,   0.000,   0.130)
    );
begin
  result := FracLookup(Fraction, LookUpTable);
end;
}

{ TColorParameters }

procedure TColorParameters.Assign(Source: TPersistent);
var
  SourceColorParameters: TColorParameters;
begin
  if Source is TColorParameters then
  begin
    SourceColorParameters := TColorParameters(Source);
    ColorCycles := SourceColorParameters.ColorCycles;
    ColorScheme := SourceColorParameters.ColorScheme;
    ColorExponent := SourceColorParameters.ColorExponent;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColorParameters.Clear;
begin
  FColorCycles := 1;
  FColorExponent := 0.60;
  FColorScheme := 0;
end;

constructor TColorParameters.Create;
begin
  Clear;
end;

function TColorParameters.FracToColor(Fraction: real;
  ColorScheme: IUserDefinedColorSchemeItem): TColor;
var
  ColorAdjustmentFactor: Real;
  ColorIndex: Integer;
  Frac1: Double;
  Frac2: Double;
  Color1: TColor;
  Color2: TColor;
  Comp1: Integer;
  Comp2: Integer;
begin
  Result := clBlack;
  Fraction := 1-Fraction;
  Assert(ColorScheme <> nil);
  if ColorScheme.ColorsI.Count = 0 then
  begin
    Exit;
  end;
  ColorScheme.ColorsI.SortColors;
  if Fraction <> 1 then
  begin
    Fraction := Frac(Fraction*ColorCycles);
  end;
  Assert((Fraction >= 0) and (Fraction <= 1));
  ColorAdjustmentFactor := ColorExponent;
  AdjustColorFactor(ColorAdjustmentFactor);
//  fraction := 1- Power((1-fraction), ColorAdjustmentFactor);
  if fraction <= ColorScheme.ColorsI[0].Fraction then
  begin
    result := ColorScheme.ColorsI[0].Color;
  end
  else if fraction >= ColorScheme.ColorsI[ColorScheme.ColorsI.Count-1].Fraction then
  begin
    result := ColorScheme.ColorsI[ColorScheme.ColorsI.Count-1].Color;
  end
  else
  begin
    for ColorIndex := 1 to ColorScheme.ColorsI.Count - 1 do
    begin
      Frac1 := ColorScheme.ColorsI[ColorIndex-1].Fraction;
      Frac2 := ColorScheme.ColorsI[ColorIndex].Fraction;
      if fraction = Frac2 then
      begin
        result := ColorScheme.ColorsI[ColorIndex].Color;
        Break;
      end
      else if (fraction > Frac1) and (fraction <= Frac2) then
      begin
        fraction := (fraction-Frac1)/(Frac2-Frac1);
        if Odd(ColorIndex) then
        begin
          if ColorAdjustmentFactor = 0 then
          begin
            ColorAdjustmentFactor := MAXINT;
          end
          else
          begin
            ColorAdjustmentFactor := 1/ColorAdjustmentFactor;
          end;
        end;
        fraction := 1- Power((1-fraction), ColorAdjustmentFactor);
        Color1 := ColorScheme.ColorsI[ColorIndex-1].Color;
        Color2 := ColorScheme.ColorsI[ColorIndex].Color;
        Comp1 := (Color1 and $FF0000) shr 16;
        Comp2 := (Color2 and $FF0000) shr 16;
        result := (Round(Comp1*(1-fraction) + Comp2*fraction) and $FF) shl 16;
        Comp1 := (Color1 and $FF00) shr 8;
        Comp2 := (Color2 and $FF00) shr 8;
        result := result +
          (Round(Comp1*(1-fraction) + Comp2*fraction) and $FF) shl 8;
        Comp1 := (Color1 and $FF);
        Comp2 := (Color2 and $FF);
        result := result +
          (Round(Comp1*(1-fraction) + Comp2*fraction) and $FF);
        Break;
      end;
    end;
  end;
end;

function TColorParameters.IsSame(OtherParamaters: TColorParameters): Boolean;
begin
  result := (ColorCycles = OtherParamaters.ColorCycles)
    and (ColorScheme = OtherParamaters.ColorScheme)
    and (ColorExponent = OtherParamaters.ColorExponent);
end;

function TColorParameters.FracToColor(Fraction: real): TColor;
var
  CustomIndex: Integer;
  CustomColorScheme: IUserDefinedColorSchemeItem;
  LocalModel: IModelForTUserDefinedColorSchemeCollection;
begin
  Assert(ColorScheme >= 0);
  if ColorScheme <= MaxColorScheme then
  begin
    if Fraction <> 1 then
    begin
      Fraction := Frac(Fraction*ColorCycles);
    end;
    case ColorScheme of
      0: result := FracToSpectrum(Fraction, ColorExponent);
      1: result := FracToGreenMagenta(1 - Fraction, ColorExponent);
      2: result := FracToBlueRed(1 - Fraction, ColorExponent);
      3: result := FracToBlueDarkOrange(1 - Fraction, ColorExponent);
      4: result := FracToBlueGreen(1 - Fraction, ColorExponent);
      5: result := FracToBrownBlue(1 - Fraction, ColorExponent);
      6: result := FracToBlueGray(1 - Fraction, ColorExponent);
      7: result := FracToBlueOrange(1 - Fraction, ColorExponent);
      8: result := FracToBlue_OrangeRed(1 - Fraction, ColorExponent);
      9: result := FracToLightBlue_DarkBlue(1 - Fraction, ColorExponent);
      10: result := ModifiedSpectralScheme(1 - Fraction, ColorExponent);
      11: result := SteppedSequential(1 - Fraction, ColorExponent);
    else
      result := clWhite;
      Assert(False);
    end;
  end
  else
  begin
    CustomIndex := ColorScheme-MaxColorScheme-1;
    LocalModel := IGlobalModel as IModelForTUserDefinedColorSchemeCollection;
    if CustomIndex < LocalModel.ColorSchemesI.Count then
    begin
      CustomColorScheme := LocalModel.ColorSchemesI[CustomIndex];
      result := FracToColor(Fraction, CustomColorScheme);
    end
    else
    begin
      result := clBlack;
    end;
  end;
end;

procedure TColorParameters.Changed;
begin
  if Assigned(OnChange) then
  begin
    OnChange(self);
  end;
end;

procedure TColorParameters.SetColorCycles(Value: integer);
begin
  if Value < 1 then
  begin
    Value := 1;
  end;
  if FColorCycles <> Value then
  begin
    FColorCycles := Value;
    Changed;
  end;
end;

procedure TColorParameters.SetColorExponent(const Value: real);
begin
  if FColorExponent <> Value then
  begin
    FColorExponent := Value;
    Changed;
  end;
end;

procedure TColorParameters.SetColorScheme(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end;
  if FColorScheme <> Value then
  begin
    FColorScheme := Value;
    Changed;
  end;
end;



{ TColorItem }

procedure TColorItem.Assign(Source: TPersistent);
var
  SourceItem: TColorItem;
begin
  if Source is TColorItem then
  begin
    SourceItem := TColorItem(Source);
    Color := SourceItem.Color;
    Fraction := SourceItem.Fraction;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColorItem.Changed(Sender: TObject);
begin
  InvalidateModel;
  (Collection as TColorCollection).FSorted := False;
end;

constructor TColorItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredFraction := TRealStorage.Create;
  FStoredFraction.OnChange := Changed;
end;

destructor TColorItem.Destroy;
begin
  FStoredFraction.Free;
  inherited;
end;

function TColorItem.GetColor: TColor;
begin
  result := FColor;
end;

function TColorItem.GetFraction: double;
begin
  result := FStoredFraction.Value;
end;

//function TColorItem.GetIndex: Integer;
//begin
//  result := inherited Index;
//end;

procedure TColorItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    InvalidateModel;
  end;
end;

procedure TColorItem.SetFraction(const Value: double);
begin
  FStoredFraction.Value := Value;
end;

//procedure TColorItem.SetIndex(const Value: Integer);
//begin
//  inherited Index := Value;
//end;

procedure TColorItem.SetStoredFraction(const Value: TRealStorage);
begin
  FStoredFraction.Assign(Value);
end;

{ TColorCollection }

function TColorCollection.Add: IColorItem;
begin
  Result := inherited Add as TColorItem;
end;

constructor TColorCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TColorItem, InvalidateModelEvent);
end;

function TColorCollection.GetItem(Index: Integer): IColorItem;
begin
  result := inherited Items[Index] as TColorItem;
end;

procedure TColorCollection.SetItem(Index: Integer; const Value: IColorItem);
begin
  inherited Items[Index] := (Value as TColorItem);
end;

procedure TColorCollection.SortColors;
var
  SortList: TColorList;
  Index: Integer;
begin
  if FSorted then
  begin
    Exit;
  end;
  SortList := TColorList.Create;
  try
    SortList.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      SortList.Add(Items[Index]);
    end;
    SortList.Sort(TColorComparer.Construct(
      function (const L, R: IColorItem): integer
      begin
        result := Sign(L.Fraction - R.Fraction);
        if result = 0 then
        begin
          result := L.Index - R.Index;
        end;
      end
      )) ;
    for Index := 0 to SortList.Count - 1 do
    begin
      SortList[Index].Index := Index;
    end;
  finally
    SortList.Free;
  end;
  FSorted := True;
end;

{ TUserDefinedColorSchemeItem }

procedure TUserDefinedColorSchemeItem.Assign(Source: TPersistent);
var
  SourceScheme: TUserDefinedColorSchemeItem;
begin
  if Source is (TUserDefinedColorSchemeItem) then
  begin
    SourceScheme := TUserDefinedColorSchemeItem(Source);
    Colors := SourceScheme.Colors;
    Name := SourceScheme.Name;
  end
  else
  begin
    inherited;
  end;
end;

constructor TUserDefinedColorSchemeItem.Create(Collection: TCollection);
begin
  inherited;
  FColors:= TColorCollection.Create(OnInvalidateModel);
end;

destructor TUserDefinedColorSchemeItem.Destroy;
begin
  FColors.Free;
  inherited;
end;

function TUserDefinedColorSchemeItem.GetColorI: IColorCollection;
begin
  result := Colors;
end;

function TUserDefinedColorSchemeItem.GetColors: TColorCollection;
begin
  result := FColors;
end;

function TUserDefinedColorSchemeItem.GetName: string;
begin
  result := FName;
end;

procedure TUserDefinedColorSchemeItem.SetColorI(const Value: IColorCollection);
begin
  Colors := Value as TColorCollection;
end;

procedure TUserDefinedColorSchemeItem.SetColors(const Value: TColorCollection);
begin
  FColors.Assign(Value);
end;

procedure TUserDefinedColorSchemeItem.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

{ TUserDefinedColorSchemeCollection }

function TUserDefinedColorSchemeCollection.Add: IUserDefinedColorSchemeItem;
begin
  result := inherited Add as TUserDefinedColorSchemeItem;
end;

constructor TUserDefinedColorSchemeCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TUserDefinedColorSchemeItem, InvalidateModelEvent);
end;

function TUserDefinedColorSchemeCollection.GetItem(
  Index: Integer): IUserDefinedColorSchemeItem;
begin
  result := inherited Items[Index] as TUserDefinedColorSchemeItem;
end;

procedure TUserDefinedColorSchemeCollection.SetItem(Index: Integer;
  const Value: IUserDefinedColorSchemeItem);
begin
  inherited Items[Index] := (Value as TUserDefinedColorSchemeItem);
end;

function FracAndSchemeToColor(ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;
var
  ColorScheme: IUserDefinedColorSchemeItem;
  LocalModel: IModelForTUserDefinedColorSchemeCollection;
begin
  if ColorSchemeIndex <= MaxColorScheme then
  begin
    if ColorSchemeIndex < 0 then
    begin
      result := clWhite;
      Exit;
    end;
    if Fraction <> 1 then
    begin
      Fraction := Frac(Fraction*Cycles);
    end;

    case ColorSchemeIndex of
      0: result := FracToSpectrum(Fraction, ColorAdjustmentFactor);
      1: result := FracToGreenMagenta(1 - Fraction, ColorAdjustmentFactor);
      2: result := FracToBlueRed(1 - Fraction, ColorAdjustmentFactor);
      3: result := FracToBlueDarkOrange(1 - Fraction, ColorAdjustmentFactor);
      4: result := FracToBlueGreen(1 - Fraction, ColorAdjustmentFactor);
      5: result := FracToBrownBlue(1 - Fraction, ColorAdjustmentFactor);
      6: result := FracToBlueGray(1 - Fraction, ColorAdjustmentFactor);
      7: result := FracToBlueOrange(1 - Fraction, ColorAdjustmentFactor);
      8: result := FracToBlue_OrangeRed(1 - Fraction, ColorAdjustmentFactor);
      9: result := FracToLightBlue_DarkBlue(1 - Fraction, ColorAdjustmentFactor);
      10: result := ModifiedSpectralScheme(1 - Fraction, ColorAdjustmentFactor);
      11: result := SteppedSequential(1 - Fraction, ColorAdjustmentFactor);
    else
      result := clWhite;
      Assert(False);
    end;
  end
  else
  begin
    ColorSchemeIndex := ColorSchemeIndex-MaxColorScheme-1;
    LocalModel := IGlobalModel as IModelForTUserDefinedColorSchemeCollection;
    if ColorSchemeIndex <= LocalModel.ColorSchemesI.Count then
    begin
      ColorParameters.ColorCycles := Cycles;
      ColorParameters.ColorExponent := ColorAdjustmentFactor;
      ColorScheme:= LocalModel.ColorSchemesI[ColorSchemeIndex];
      result := ColorParameters.FracToColor(Fraction, ColorScheme)
    end
    else
    begin
      result := clWhite;
    end;
  end;
end;

initialization
  ColorParameters := TColorParameters.Create;

finalization
  ColorParameters.Free;

end.

