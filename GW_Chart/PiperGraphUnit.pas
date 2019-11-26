unit PiperGraphUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Grids, ExtCtrls, ComCtrls, DataGrid, Buttons, printers, Math,
  Clipbrd, Menus, ArgusDataEntry, MyFormUnit, ReaderUnit;

type
  TSpecies = (Ca, Mg, Na, K, CO3, HCO3, Cl, SO4, TDS);
const
  SymbolIndex = Ord(High(TSpecies)) + 1;
  ColorIndex = SymbolIndex + 1;
  wtCa = 40.08;
  wtMg = 24.31;
  wtNa = 22.99;
  wtK = 39.10;
  wtS = 32.06;
  wtO = 16.00;
  wtC = 12.01;
  wtH = 1.008;
  wtCl = 35.45;
  wtSO4 = wtS + 4 * wtO;
  wtCO3 = wtC + 3 * wtO;
  wtHCO3 = wtH + wtCO3;
  eqCa = wtCa / 2;
  eqMg = wtMg / 2;
  eqNa = wtNa;
  eqK = wtK;
  eqSO4 = wtSO4 / 2;
  eqCO3 = wtCO3 / 2;
  eqHCO3 = wtHCO3;
  eqCl = wtCl;
  eqTDS = 0;
type
  TOrientation = (oBelowLeft, oBelowCenter, oBelowRight,
    oAboveLeft, oAboveCenter, oAboveRight, oLeft, ORight);
type
  TGraphSymbol = (gsCircle, gsCircleOpen, gsSquare, gsSquareOpen,
    gsTriangle, gsTriangleOpen, gsInvertedTriangle, gsInvertedTriangleOpen,
    gsStar, gsStarOpen, gsCross, gsX);
type
  TColorSelection = (csAllBlack, csLinked, csIndividual);

type
  TLegendObject = class(TObject)
  private
    procedure Draw(Text: string; Canvas: TCanvas; RadiusUsed: integer;
      AColor: TColor; HorizontalSeparation, VerticalSeparation, X: integer;
      var Y: integer);
    function Height(Text: string; Canvas: TCanvas; RadiusUsed,
      VerticalSeparation: integer; var TextHeight: integer): integer;
    procedure DrawSymbol(Canvas: TCanvas; RadiusUsed: integer;
      APoint: TPoint; AColor: TColor);
  public
    Color: TColor;
    Symbol: TGraphSymbol;
    Rect: TRect;
  end;

type
  TfrmPiperGraph = class(TMyForm)
    MainMenu1: TMainMenu;
    sbSaveImage: TSpeedButton;
    sbFormat: TSpeedButton;
    shpLeft: TShape;
    shpRight: TShape;
    rgOrientation: TRadioGroup;
    sbFormat2: TSpeedButton;
    Help1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    File1: TMenuItem;
    Opendatafile1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    Saveasimage1: TMenuItem;
    Print1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbPrintPreviewClick(Sender: TObject);
    procedure tabPlotResize(Sender: TObject);
    procedure rgOrientationClick(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    LegendSelected: boolean;
    function GetRectBorder(Printing: boolean): integer;
    procedure SelectLegend(X, Y: integer);
    function GetIsDigit(AChar: Char): boolean;
    function GetIsValance(AChar: Char): Integer;
    procedure DrawCircle(Canvas: TCanvas; RadiusUsed: integer; pt1: TPoint;
      Color: TColor; Open: boolean);
    procedure DrawSquare(Canvas: TCanvas; RadiusUsed: integer; pt1: TPoint;
      Color: TColor; Open: boolean);
    function GetSymbolBrushColor(Open: boolean; Color: TColor): TColor;
    procedure InvertedTriangle(Canvas: TCanvas; radius: integer;
      APoint: TPoint; Color: TColor; Open: boolean);
    procedure Triangle(Canvas: TCanvas; radius: integer; APoint: TPoint;
      Color: TColor; Open: boolean);
    procedure Star(Canvas: TCanvas; radius: integer; APoint: TPoint;
      Color: TColor; Open: boolean);
    procedure CalculateVisible;
  published
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    tabData: TTabSheet;
    tabPlot: TTabSheet;
    Panel1: TPanel;
    seNumPoints: TSpinEdit;
    DataGrid1: TDataGrid;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    Panel3: TPanel;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ColorDialog1: TColorDialog;
    sbOpenFile: TSpeedButton;
    sbSaveFile: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    SaveDialog2: TSaveDialog;
    rgDataType: TRadioGroup;
    procedure seNumPointsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure DataGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure FormDestroy(Sender: TObject);
    procedure DataGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cbPercentClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure sbFormatClick(Sender: TObject);
    procedure DataGrid1EditButtonClick(Sender: TObject);
    procedure DataGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sbSaveFileClick(Sender: TObject);
    procedure sbOpenFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure sbSaveImageClick(Sender: TObject);
    procedure rgDataTypeClick(Sender: TObject);
  private
    AFile: TextFile;
    ptCa, ptNaK, ptMg, ptCl, ptSO4, ptCO3, ptDTop, ptDBottom,
      ptDLeft, ptDRight: TPoint; // these represent the locations
    // of the corners of the triangles and diamond
    values: array[Low(TSpecies)..High(TSpecies)] of array of double;
    OkArray: array of Boolean;
    TextFont: TFont;
    PrintFont: TFont;
    TitleFont: TFont;
    PrintTitleFont: TFont;
    VertSize: integer;
    Side: integer;
    Multiplier: double;
    Wait: boolean;
    Margin: integer;
    InteriorMargin: integer;
    LineThickness: integer;
    GraphTitle: string;
    SymbolRadius: integer;
    TitleBottom: integer;
    FGridLines: boolean;
    FColorSelection: TColorSelection;
    ShowNumbers: boolean;
    LabelTextSize: Integer;
    LinkRadius: boolean;
    MinTDS, MaxTDS: double;
    MinSymbolSize, MaxSymbolSize: integer;
    LegendRect: TRect;
    Modified: boolean;
    procedure SetColorSelection(AValue: TColorSelection);
    procedure SetGridLines(AValue: boolean);
    procedure PaintGraph(Width, Height: integer; Canvas: TCanvas;
      Printing, Saving: boolean);
    procedure GetDimensions(CanvasWidth, CanvasHeight: integer;
      Canvas: TCanvas; Printing: boolean);
    procedure PaintAxes(Canvas: TCanvas; Printing: boolean);
    procedure SetValues;
    procedure SetArraySizes;
    procedure DrawValues(Canvas: TCanvas; Printing: boolean);
    procedure DrawText(Canvas: TCanvas; Printing: boolean);
    procedure DataGrid1DrawCaptions(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawAText(Canvas: TCanvas; TextString: string;
      Position: TPoint; AnOrientation: TOrientation; IsNumber, Printing,
      RotateRight: boolean);
    function GetRect(Canvas: TCanvas; TextString: string; Position: TPoint;
      AnOrientation: TOrientation; IsNumber, UseOffset, Printing, RotateRight:
      boolean): TRect;
    procedure DrawGridLines(Canvas: TCanvas);
    procedure DrawTriangeGridLines(Canvas: TCanvas; Pt1, Pt2, Pt3: TPoint);
    procedure DrawDiamondGridLines(Canvas: TCanvas);
    function DrawTitle(CanvasWidth, CanvasHeight: integer; Canvas: TCanvas;
      Printing: boolean): integer;
    procedure DrawAValue(Canvas: TCanvas; Printing: boolean; pt1, pt2,
      pt3: TPoint; AGraphSymbol: TGraphSymbol; Color: TColor;
      SymbolRadius: double);
    procedure Cross(Canvas: TCanvas; radius: integer; APoint: TPoint; Color:
      TColor);
    procedure XCross(Canvas: TCanvas; radius: integer; APoint: TPoint; Color:
      TColor);
    procedure SaveFont(AStringList: TStringlist; AFont: TFont);
    procedure LoadFont(AFont: TFont);
    procedure RotatePrint(CV: TCanvas; TextString: string; const X, Y,
      Angle: integer);
    procedure MyRotatePrint(CV: TCanvas; TextString: string; X, Y,
      Angle: integer; AnOrientation: TOrientation; Printing, RotateRight:
      boolean);
    procedure DrawArrows(Canvas: TCanvas; Printing: boolean);
    procedure DrawAnArrow(Canvas: TCanvas; Pt1, Pt2: TPoint;
      Printing: boolean);
    procedure DrawLegend(Canvas: TCanvas; Printing, Saving: boolean);
    procedure ClearLegend;
    procedure GetTDSRAnge;
    { Private declarations }
  public
    LegendX, LegendY: integer;
    LegendOffsetX, LegendOffSetY: integer;
    Legend: TStringList;
    Colors: array[Low(TGraphSymbol)..High(TGraphSymbol)] of TColor;
    GridLineCount: integer;
    property GridLines: boolean read FGridlines write SetGridLines;
    property ColorSelection: TColorSelection read FColorSelection write
      SetColorSelection;
    { Public declarations }
  end;

var
  frmPiperGraph: TfrmPiperGraph;
  Digits: array[0..9] of Char = ('0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9');
  Valance: array[0..3] of Char = ('!', '@', '#', '$'); // = (2-, -, +, 2+)
  ValanceStrings: array[0..3] of string = ('2-', '-', '+', '2+');

  {
  Constants for converting mg/l to meq/l
  multiply
  concentration
  in mg/l                by         to get meq/l

  Ca                      0.04990
  Mg                      0.08229
  Na                      0.04350
  K                       0.02558
  Cl                      0.02821
  Fl                      0.05264
  SO4                     0.02082
  HCO3                    0.01639
  NO3, as nitrogen        0.07139
  PO4 as phosphorous      0.09685
  Alkalinity as calcium   0.02000  Bicarbonate concentration
    carbonante in mg/l               in meq/l

  }

implementation

uses PiperFormatUnit, frmModChartUnit, frmAboutUnit, frmEditCO3Unit,
  frmFormatUnit;

{$R *.DFM}

procedure TfrmPiperGraph.RotatePrint(CV: TCanvas; TextString: string;
  const X, Y, Angle: integer);
var
  LogFont: TLogFont;
begin
  GetObject(CV.Font.Handle, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement := Angle * 10;
  LogFont.lfOrientation := Angle * 10;
  CV.Font.Handle := CreateFontIndirect(LogFont);
  CV.TextOut(X, Y, TextString);
  DeleteObject(CV.Font.Handle);
end;

procedure TfrmPiperGraph.MyRotatePrint(CV: TCanvas; TextString: string;
  X, Y, Angle: integer; AnOrientation: TOrientation; Printing, RotateRight:
  boolean);
const
  BorderConst = 15;
var
  AChar: Char;
  AString: string;
  CharIndex: integer;
  IsDigit, IsValance: boolean;
  OldFontSize: integer;
  TotalX: integer;
  ThisX, ThisY: integer;
  DigitFont: TFont;
  Rect: TRect;
  APoint: TPoint;
  distance: double;
  StartAngle: double;
  Border: integer;
  ValanceIndex: integer;
begin
  Border := BorderConst;
  if Printing then
  begin
    Border := Round(Multiplier * Border);
  end;
  DigitFont := TFont.Create;
  try

    if Printing then
    begin
      DigitFont.Assign(PrintFont);
    end
    else
    begin

      DigitFont.Assign(TextFont);
    end;
    APoint.X := X;
    APoint.Y := Y;
    Rect := GetRect(CV, TextString, APoint, AnOrientation, False, False,
      Printing, RotateRight);
    case AnOrientation of
      oBelowLeft, oBelowCenter, oBelowRight:
        begin
          Rect.Top := Rect.Top + Border;
          Rect.Bottom := Rect.Bottom + Border;
        end;
      oAboveLeft, oAboveCenter, oAboveRight:
        begin
          Rect.Top := Rect.Top - Border;
          Rect.Bottom := Rect.Bottom - Border;
        end;
    else
      begin
        Assert(False);
      end;
    end;

    distance := Sqrt(Sqr(X - Rect.Left) + Sqr(Y - Rect.Top));
    StartAngle := ArcTan2(Y - Rect.Top, Rect.Left - x) + Angle / 180 * Pi;
    X := X + Round(distance * Cos(StartAngle));
    Y := Y - Round(distance * Sin(StartAngle));

    DigitFont.Height := DigitFont.Height * 3 div 4;
    TotalX := 0;

    with CV do
    begin
      for CharIndex := 1 to Length(TextString) do
      begin
        AChar := TextString[CharIndex];
        AString := AChar;
        IsDigit := GetIsDigit(AChar);
        ValanceIndex := GetIsValance(AChar);
        IsValance := ValanceIndex > -1;
        if IsValance then
        begin
          AString := ValanceStrings[ValanceIndex];
        end;

        ThisX := X + Round(TotalX * Cos(Angle / 180 * Pi));
        ThisY := Y - Round(TotalX * Sin(Angle / 180 * Pi));

        if Printing then
        begin
          CV.Font.Assign(PrintFont);
        end
        else
        begin
          CV.Font.Assign(TextFont);
        end;

        if IsDigit then
        begin
          OldFontSize := CV.Font.Height;
          CV.Font.Assign(DigitFont);
          ThisX := ThisX + Round(Abs(OldFontSize) / 2 * Sin(Angle / 180 * Pi));
          ThisY := ThisY + Round(Abs(OldFontSize) / 2 * Cos(Angle / 180 * Pi));
          CV.Font.Height := OldFontSize * 3 div 4;
        end
        else if IsValance then
        begin
          OldFontSize := CV.Font.Height;
          CV.Font.Assign(DigitFont);
          ThisX := ThisX - Round(Abs(OldFontSize) / 2 * Sin(Angle / 180 * Pi));
          ThisY := ThisY - Round(Abs(OldFontSize) / 2 * Cos(Angle / 180 * Pi));
          CV.Font.Height := OldFontSize * 3 div 4;
        end;

        RotatePrint(CV, AString, ThisX, ThisY, Angle);
        TotalX := TotalX + TextWidth(AString); //ASize.cx ;

      end;
    end;

  finally
    DigitFont.Free;
  end;

end;

procedure TfrmPiperGraph.SetArraySizes;
var
  ASpecies: TSpecies;
begin
  for ASpecies := Low(TSpecies) to High(TSpecies) do
  begin
    SetLength(values[ASpecies], seNumPoints.Value);
  end;
  SetLength(OkArray, seNumPoints.Value);
end;

procedure TfrmPiperGraph.seNumPointsChange(Sender: TObject);
var
  LineIndex: integer;
begin
  DataGrid1.RowCount := seNumPoints.Value + 1;
  SetArraySizes;
  SetValues;
  for LineIndex := 1 to DataGrid1.RowCount - 1 do
  begin
    if DataGrid1.Cells[SymbolIndex, LineIndex] = '' then
    begin
      DataGrid1.Cells[SymbolIndex, LineIndex] :=
        DataGrid1.Columns[SymbolIndex].PickList[0];
      DataGrid1.Cells[ColorIndex, LineIndex] := IntToStr(Colors[gsCircle]);
    end;
  end;
  GetTDSRAnge;
  Modified := True;
end;

procedure TfrmPiperGraph.FormCreate(Sender: TObject);
begin
  TextFont := TFont.Create;
  PrintFont := TFont.Create;
  TitleFont := TFont.Create;
  PrintTitleFont := TFont.Create;
  Legend := TStringList.Create;
  GridLineCount := 1;
  rgOrientation.ItemIndex := Ord(Printer.Orientation);
  Margin := 20;
  SymbolRadius := 4;
  MinSymbolSize := SymbolRadius;
  MaxSymbolSize := MinSymbolSize * 3 div 2;
  LinkRadius := False;
  LegendSelected := False;
  seNumPoints.MaxValue := High(Longint) - 1;
  seNumPoints.MinValue := 1;
  SetArraySizes;
  TextFont.Assign(Font);
  PrintFont.Assign(Font);
  TitleFont.Assign(Font);
  TitleFont.Size := TitleFont.Size + 4;
  PrintTitleFont.Assign(TitleFont);
  Wait := False;
  LineThickness := PaintBox1.Canvas.Pen.Width;
  DataGrid1.Cells[SymbolIndex, 1] := DataGrid1.Columns[SymbolIndex].PickList[0];
  DataGrid1.Cells[ColorIndex, 1] := IntToStr(clBlack);
  LegendX := 20;
  LegendY := 20;
  GridLines := False;
  ColorSelection := csAllBlack;
  PageControl1.ActivePageIndex := 0;
  InteriorMargin := 80;
  ShowNumbers := False;
  cbPrintPreviewClick(Sender);
  Modified := False;
end;

function TfrmPiperGraph.DrawTitle(CanvasWidth, CanvasHeight: integer;
  Canvas: TCanvas; Printing: boolean): integer;
var
  TextSize: TSize;
  TitleHeight: integer;
  MarginUsed: integer;
  PixelsPerInch: integer;
begin
  if Printing then
  begin
    PixelsPerInch := Canvas.Font.PixelsPerInch;
    Multiplier := Min(CanvasHeight / PaintBox1.ClientHeight,
      CanvasWidth / PaintBox1.ClientWidth);
    MarginUsed := Round(Margin * Multiplier);
    PrintTitleFont.PixelsPerInch := PixelsPerInch;
    PrintTitleFont.Assign(TitleFont);
    PrintTitleFont.Height := Round(PrintTitleFont.Height * Multiplier /
      (PixelsPerInch / TitleFont.PixelsPerInch));
    Canvas.Font.Assign(PrintTitleFont);
  end
  else
  begin
    Canvas.Font.Assign(TitleFont);
    MarginUsed := Margin;
  end;
  TextSize := Canvas.TextExtent(GraphTitle);
  TitleHeight := TextSize.cy;
  Canvas.TextOut((CanvasWidth - TextSize.cx) div 2, MarginUsed,
    GraphTitle);
  result := TitleHeight;
end;

procedure TfrmPiperGraph.GetDimensions(CanvasWidth, CanvasHeight: integer;
  Canvas: TCanvas; Printing: boolean);
var
  AvailableHeight, AvailableWidth: integer;
  Base: integer;
  VertOffset: integer;
  VertSide: integer;
  SinAngle: double;
  MarginUsed: integer;
  InteriorMarginUsed: integer;
  TitleHeight: integer;
  PixelsPerInch: Integer;
begin
  TitleHeight := DrawTitle(CanvasWidth, CanvasHeight, Canvas, Printing);

  SinAngle := Sin(Pi / 3);

  if Printing then
  begin
    PixelsPerInch := Canvas.Font.PixelsPerInch;
    Multiplier := Min(CanvasHeight / PaintBox1.ClientHeight,
      CanvasWidth / PaintBox1.ClientWidth);
    MarginUsed := Round(Margin * Multiplier);
    InteriorMarginUsed := Round(InteriorMargin * Multiplier);
    PrintFont.PixelsPerInch := PixelsPerInch;
    PrintFont.Assign(TextFont);
    PrintFont.Height := Round(PrintFont.Height * Multiplier /
      (PixelsPerInch / TextFont.PixelsPerInch));
    Canvas.Font.Assign(PrintFont);
  end
  else
  begin
    Canvas.Font.Assign(TextFont);
    MarginUsed := Margin;
    InteriorMarginUsed := InteriorMargin;
  end;
  TitleBottom := TitleHeight + MarginUsed;
  TitleHeight := TitleBottom + MarginUsed;

  LabelTextSize := Canvas.TextHeight('Ca');

  AvailableHeight := CanvasHeight - MarginUsed
    - (LabelTextSize * 4) - TitleHeight;
  if ShowNumbers then
  begin
    AvailableHeight := AvailableHeight - ((LabelTextSize * 2))
  end;
  AvailableWidth := CanvasWidth - 2 * MarginUsed;
  if ShowNumbers then
  begin
    AvailableWidth := AvailableWidth - Canvas.TextWidth('0') -
      Canvas.TextWidth('100') - 2 * GetRectBorder(Printing);
  end;
  Base := (AvailableWidth - InteriorMarginUsed) div 2;
  VertOffset := Round(InteriorMarginUsed * SinAngle);
  VertSide := Round(((AvailableHeight - VertOffset) / 2) / SinAngle);
  if Base > VertSide then
  begin
    Side := VertSide;
  end
  else
  begin
    Side := Base;
  end;
  VertSize := Round(Side * SinAngle);
  ptNaK.x := (CanvasWidth - InteriorMarginUsed) div 2;
  ptNaK.y := TitleHeight + 2 * VertSize + VertOffset;
  if ShowNumbers then
  begin
    ptNaK.y := ptNaK.y + LabelTextSize;
  end;

  ptCa.x := ptNaK.x - Side;
  ptCa.y := ptNaK.y;

  ptMg.x := ptNaK.x - Side div 2;
  ptMg.y := ptNaK.y - VertSize;

  ptCO3.x := (CanvasWidth + InteriorMarginUsed) div 2;
  ptCO3.y := ptNaK.y;

  ptCL.x := ptCO3.x + Side;
  ptCL.y := ptNaK.y;

  ptSO4.x := ptCO3.x + Side div 2;
  ptSO4.y := ptMg.y;

  ptDBottom.x := CanvasWidth div 2;
  ptDBottom.y := ptNaK.y - VertOffset;

  ptDLeft.x := ptDBottom.x - Side div 2;
  ptDLeft.y := ptDBottom.y - VertSize;

  ptDRight.x := ptDBottom.x + Side div 2;
  ptDRight.y := ptDLeft.y;

  ptDTop.x := ptDBottom.x;
  ptDTop.y := ptDBottom.y - 2 * VertSize;

end;

procedure TfrmPiperGraph.PaintAxes(Canvas: TCanvas; Printing: boolean);
var
  LocalThickness: integer;
begin
  with Canvas do
  begin
    LocalThickness := Canvas.Pen.Width;
    try
      if Printing then
      begin
        Canvas.Pen.Width := Round(LineThickness * Multiplier);
      end
      else
      begin
        Canvas.Pen.Width := LineThickness;
      end;

      MoveTo(ptCa.X, ptCa.Y);
      LineTo(ptNaK.X, ptNaK.Y);
      LineTo(ptMg.X, ptMg.Y);
      LineTo(ptCa.X, ptCa.Y);

      MoveTo(ptCl.X, ptCl.Y);
      LineTo(ptSO4.X, ptSO4.Y);
      LineTo(ptCO3.X, ptCO3.Y);
      LineTo(ptCl.X, ptCl.Y);

      MoveTo(ptDBottom.X, ptDBottom.Y);
      LineTo(ptDLeft.X, ptDLeft.Y);
      LineTo(ptDTop.X, ptDTop.Y);
      LineTo(ptDRight.X, ptDRight.Y);
      LineTo(ptDBottom.X, ptDBottom.Y);
    finally
      Canvas.Pen.Width := LocalThickness;
    end;
  end;
end;

procedure TfrmPiperGraph.SetValues;
var
  RowIndex: integer;
  ColIndex: TSpecies;
begin
  with DataGrid1 do
  begin
    for RowIndex := 1 to RowCount - 1 do
    begin
      for ColIndex := Low(TSpecies) to High(TSpecies) do
      begin
        DataGrid1SetEditText(DataGrid1, Ord(ColIndex), RowIndex,
          Cells[Ord(ColIndex), RowIndex]);
      end;
    end;
  end;
end;

procedure TfrmPiperGraph.Triangle(Canvas: TCanvas; radius: integer;
  APoint: TPoint; Color: TColor; Open: boolean);
var
  Points: array[0..2] of TPoint;
  HOffSet, VOffSet: integer;
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  VOffSet := Round(radius * Sin(Pi / 6));
  HOffSet := Round(radius * Cos(Pi / 6));

  Points[0].X := APoint.x;
  Points[0].Y := APoint.y - radius;

  Points[1].X := APoint.x + HOffSet;
  Points[1].Y := APoint.y + VOffSet;

  Points[2].X := APoint.x - HOffSet;
  Points[2].Y := APoint.y + VOffSet;

  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := GetSymbolBrushColor(Open, Color);
      Polygon(Points);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.InvertedTriangle(Canvas: TCanvas; radius: integer;
  APoint: TPoint; Color: TColor; Open: boolean);
var
  Points: array[0..2] of TPoint;
  HOffSet, VOffSet: integer;
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  VOffSet := Round(radius * Sin(Pi / 6));
  HOffSet := Round(radius * Cos(Pi / 6));

  Points[0].X := APoint.x;
  Points[0].Y := APoint.y + radius;

  Points[1].X := APoint.x + HOffSet;
  Points[1].Y := APoint.y - VOffSet;

  Points[2].X := APoint.x - HOffSet;
  Points[2].Y := APoint.y - VOffSet;

  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := GetSymbolBrushColor(Open, Color);
      Polygon(Points);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.Star(Canvas: TCanvas; radius: integer;
  APoint: TPoint; Color: TColor; Open: boolean);
var
  Points: array[0..9] of TPoint;
  HOffSet1, VOffSet1: integer;
  HOffSet2, VOffSet2: integer;
  HOffSet3, VOffSet3: integer;
  HOffSet4, VOffSet4: integer;
  CurrentPenColor, CurrentBrushColor: TColor;
  Radius2: double;
begin
  VOffSet1 := Round(radius * Sin(Pi / 10));
  HOffSet1 := Round(radius * Cos(Pi / 10));

  VOffSet2 := Round(radius * Sin(3 * Pi / 10));
  HOffSet2 := Round(radius * Cos(3 * Pi / 10));

  Points[0].X := APoint.x;
  Points[0].Y := APoint.y - radius;

  Points[2].X := APoint.x + HOffSet1;
  Points[2].Y := APoint.y - VOffSet1;

  Points[4].X := APoint.x + HOffSet2;
  Points[4].Y := APoint.y + VOffSet2;

  Points[6].X := APoint.x - HOffSet2;
  Points[6].Y := APoint.y + VOffSet2;

  Points[8].X := APoint.x - HOffSet1;
  Points[8].Y := APoint.y - VOffSet1;

  Radius2 := radius * Sin(Pi / 10) / Cos(Pi / 5);

  VOffSet3 := VOffSet1;
  HOffSet3 := Round(Radius2 * Sin(Pi / 5));

  VOffSet4 := Round(Radius2 * Sin(Pi / 10));
  HOffSet4 := Round(Radius2 * Cos(Pi / 10));

  Points[1].X := APoint.x + HOffSet3;
  Points[1].Y := APoint.y - VOffSet3;

  Points[3].X := APoint.x + HOffSet4;
  Points[3].Y := APoint.y + VOffSet4;

  Points[5].X := APoint.x;
  Points[5].Y := APoint.y + Round(Radius2);

  Points[7].X := APoint.x - HOffSet4;
  Points[7].Y := APoint.y + VOffSet4;

  Points[9].X := APoint.x - HOffSet3;
  Points[9].Y := APoint.y - VOffSet3;

  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := GetSymbolBrushColor(Open, Color);
      Polygon(Points);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.XCross(Canvas: TCanvas; radius: integer; APoint:
  TPoint; Color: TColor);
var
  LocalWidth: integer;
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    LocalWidth := Pen.Width;
    try
      Pen.Color := Color;
      Brush.Color := Color;
      Pen.Width := 2;
      MoveTo(APoint.x - radius, APoint.y - radius);
      LineTo(APoint.x + radius, APoint.y + radius);
      MoveTo(APoint.x - radius, APoint.y + radius);
      LineTo(APoint.x + radius, APoint.y - radius);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
      Pen.Width := LocalWidth;
    end;
  end;
end;

function TfrmPiperGraph.GetSymbolBrushColor(Open: boolean; Color: TColor):
  TColor;
begin
  if Open then
  begin
    result := PaintBox1.Color;
  end
  else
  begin
    result := Color;
  end;
end;

procedure TfrmPiperGraph.DrawCircle(Canvas: TCanvas; RadiusUsed: integer; pt1:
  TPoint;
  Color: TColor; Open: boolean);
var
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := GetSymbolBrushColor(Open, Color);
      Ellipse(pt1.x - RadiusUsed, pt1.Y - RadiusUsed,
        pt1.x + RadiusUsed, pt1.Y + RadiusUsed);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.DrawSquare(Canvas: TCanvas; RadiusUsed: integer; pt1:
  TPoint;
  Color: TColor; Open: boolean);
var
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := GetSymbolBrushColor(Open, Color);
      Rectangle(pt1.x - RadiusUsed, pt1.Y - RadiusUsed,
        pt1.x + RadiusUsed, pt1.Y + RadiusUsed);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.DrawAValue(Canvas: TCanvas; Printing: boolean;
  pt1, pt2, pt3: TPoint; AGraphSymbol: TGraphSymbol; Color: TColor;
  SymbolRadius: double);
var
  RadiusUsed: integer;
begin
  if Printing then
  begin
    RadiusUsed := Round(Multiplier * SymbolRadius);
  end
  else
  begin
    RadiusUsed := Round(SymbolRadius);
  end;
  with Canvas do
  begin
    case AGraphSymbol of
      gsCircle:
        begin
          DrawCircle(Canvas, RadiusUsed, pt1, Color, False);
          DrawCircle(Canvas, RadiusUsed, pt2, Color, False);
          DrawCircle(Canvas, RadiusUsed, pt3, Color, False);
        end;
      gsCircleOpen:
        begin
          DrawCircle(Canvas, RadiusUsed, pt1, Color, True);
          DrawCircle(Canvas, RadiusUsed, pt2, Color, True);
          DrawCircle(Canvas, RadiusUsed, pt3, Color, True);
        end;
      gsSquare:
        begin
          DrawSquare(Canvas, RadiusUsed, pt1, Color, False);
          DrawSquare(Canvas, RadiusUsed, pt2, Color, False);
          DrawSquare(Canvas, RadiusUsed, pt3, Color, False);
        end;
      gsSquareOpen:
        begin
          DrawSquare(Canvas, RadiusUsed, pt1, Color, True);
          DrawSquare(Canvas, RadiusUsed, pt2, Color, True);
          DrawSquare(Canvas, RadiusUsed, pt3, Color, True);
        end;
      gsTriangle:
        begin
          Triangle(Canvas, RadiusUsed, pt1, Color, False);
          Triangle(Canvas, RadiusUsed, pt2, Color, False);
          Triangle(Canvas, RadiusUsed, pt3, Color, False);
        end;
      gsTriangleOpen:
        begin
          Triangle(Canvas, RadiusUsed, pt1, Color, True);
          Triangle(Canvas, RadiusUsed, pt2, Color, True);
          Triangle(Canvas, RadiusUsed, pt3, Color, True);
        end;
      gsInvertedTriangle:
        begin
          InvertedTriangle(Canvas, RadiusUsed, pt1, Color, False);
          InvertedTriangle(Canvas, RadiusUsed, pt2, Color, False);
          InvertedTriangle(Canvas, RadiusUsed, pt3, Color, False);
        end;
      gsInvertedTriangleOpen:
        begin
          InvertedTriangle(Canvas, RadiusUsed, pt1, Color, True);
          InvertedTriangle(Canvas, RadiusUsed, pt2, Color, True);
          InvertedTriangle(Canvas, RadiusUsed, pt3, Color, True);
        end;
      gsStar:
        begin
          Star(Canvas, RadiusUsed, pt1, Color, False);
          Star(Canvas, RadiusUsed, pt2, Color, False);
          Star(Canvas, RadiusUsed, pt3, Color, False);
        end;
      gsStarOpen:
        begin
          Star(Canvas, RadiusUsed, pt1, Color, True);
          Star(Canvas, RadiusUsed, pt2, Color, True);
          Star(Canvas, RadiusUsed, pt3, Color, True);
        end;
      gsCross:
        begin
          Cross(Canvas, RadiusUsed, pt1, Color);
          Cross(Canvas, RadiusUsed, pt2, Color);
          Cross(Canvas, RadiusUsed, pt3, Color);
        end;
      gsX:
        begin
          XCross(Canvas, RadiusUsed, pt1, Color);
          XCross(Canvas, RadiusUsed, pt2, Color);
          XCross(Canvas, RadiusUsed, pt3, Color);
        end;
    else
      begin
        Assert(False);
      end;
    end;
  end;

end;

procedure TfrmPiperGraph.DrawValues(Canvas: TCanvas; Printing: boolean);
var
  ValueIndex: integer;
  LeftPoint, RightPoint, DiamondPoint: TPoint;
  Value: double;
  CO3Value, NaKValue: double;
  CO3Distance, NaKDistance: double;
  AColor: TColor;
  CosPi3, SinPi3: double;
  AGraphSymbol: TGraphSymbol;
  GSIndex: integer;
  SymbolString: string;
  OldPenColor, OldBrushColor: TColor;
  LocalSymbolRadius: double;
  AnInt: integer;
  function GetSymbolRadius: double;
  begin
    if LinkRadius then
    begin
      if MaxTDS <> MinTDS then
      begin
        result := (Values[TDS, ValueIndex] - MinTDS) / (MaxTDS - MinTDS)
          * (MaxSymbolSize - MinSymbolSize) + MinSymbolSize;
      end
      else
      begin
        result := MinSymbolSize;
      end;
    end
    else
    begin
      result := SymbolRadius;
    end;
  end;
begin
  OldPenColor := Canvas.Pen.Color;
  OldBrushColor := Canvas.Brush.Color;
  try
    CosPi3 := Cos(Pi / 3);
    SinPi3 := Sin(Pi / 3);
    AColor := Canvas.Brush.Color;
    try
      Canvas.Brush.Color := clBlack;
      for ValueIndex := 0 to seNumPoints.Value - 1 do
      begin
        if OKArray[ValueIndex] then
        begin
          // left point
          // X
          Value := Values[Ca, ValueIndex];
          Value := 1 - Value;
          Assert(Value <= 1);
          Assert(Value >= 0);
          LeftPoint.x := Round(Value * Side * CosPi3) + ptCa.x;

          Value := Values[Na, ValueIndex] + Values[K, ValueIndex];
          Assert(Value <= 1);
          Assert(Value >= 0);
          LeftPoint.x := Round(Value * Side * CosPi3) + LeftPoint.x;

          // Y
          Value := Values[Mg, ValueIndex];
          Assert(Value <= 1);
          Assert(Value >= 0);
          LeftPoint.y := ptCa.y - Round(Value * VertSize);

          //right point
          // X
          Value := Values[CO3, ValueIndex] + Values[HCO3, ValueIndex];
          Value := 1 - Value;
          Assert(Value <= 1);
          Assert(Value >= 0);
          RightPoint.x := Round(Value * Side * CosPi3) + ptCO3.x;

          Value := Values[Cl, ValueIndex];
          Assert(Value <= 1);
          Assert(Value >= 0);
          RightPoint.x := Round(Value * Side * CosPi3) + RightPoint.x;

          // Y
          Value := Values[SO4, ValueIndex];
          Assert(Value <= 1);
          Assert(Value >= 0);
          RightPoint.y := ptCO3.y - Round(Value * VertSize);

          // Diamond Point
          NaKValue := Values[Na, ValueIndex] + Values[K, ValueIndex];
          NaKValue := 1 - NaKValue;
          Assert(NaKValue <= 1);
          Assert(NaKValue >= 0);

          CO3Value := Values[CO3, ValueIndex] + Values[HCO3, ValueIndex];
          CO3Value := 1 - CO3Value;
          Assert(CO3Value <= 1);
          Assert(CO3Value >= 0);

          CO3Distance := CO3Value * Side;
          NaKDistance := NaKValue * Side;

          DiamondPoint.x := ptDBottom.x + Round(CO3Distance * CosPi3)
            - Round(NaKDistance * CosPi3);

          DiamondPoint.y := ptDBottom.y - Round(CO3Distance * SinPi3)
            - Round(NaKDistance * SinPi3);

          SymbolString := DataGrid1.Cells[SymbolIndex, ValueIndex + 1];

          GSIndex :=
            DataGrid1.Columns[SymbolIndex].PickList.IndexOf(SymbolString)
            {as TGraphSymbol};
          Assert((GSIndex >= 0) and (GSIndex <= Ord(High(TGraphSymbol))));
          AGraphSymbol := TGraphSymbol(GSIndex);
          if ColorSelection = csAllBlack then
          begin
            AColor := clBlack;
          end
          else
          begin
            if TryStrToInt(DataGrid1.Cells[ColorIndex, ValueIndex + 1], AnInt) then
            begin
              AColor := AnInt;
            end
            else
            begin
              AColor := clBlack;
            end;
          end;

          LocalSymbolRadius := GetSymbolRadius;
          DrawAValue(Canvas, Printing,
            LeftPoint, RightPoint, DiamondPoint, AGraphSymbol, AColor,
            LocalSymbolRadius)
        end;
      end;
    finally
      Canvas.Brush.Color := AColor;
    end;
  finally
    Canvas.Pen.Color := OldPenColor;
    Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure TfrmPiperGraph.DrawTriangeGridLines(Canvas: TCanvas; Pt1, Pt2, Pt3:
  TPoint);
var
  LineIndex: integer;
  X1, Y1: integer;
  X2, Y2: integer;
  X3, Y3: integer;
begin
  with Canvas do
  begin
    for LineIndex := 1 to GridLineCount do
    begin
      X1 := Pt1.x + (Pt2.x - Pt1.x) * LineIndex div (GridLineCount + 1);
      Y1 := Pt1.Y + (Pt2.Y - Pt1.Y) * LineIndex div (GridLineCount + 1);
      MoveTo(X1, Y1);

      X2 := Pt3.x + (Pt2.x - Pt3.x) * LineIndex div (GridLineCount + 1);
      Y2 := Pt3.Y + (Pt2.Y - Pt3.Y) * LineIndex div (GridLineCount + 1);
      LineTo(X2, Y2);

      Canvas.MoveTo(X1, Y1);
      X3 := Pt1.x + (Pt3.x - Pt1.x) * LineIndex div (GridLineCount + 1);
      Y3 := Pt1.Y + (Pt3.Y - Pt1.Y) * LineIndex div (GridLineCount + 1);
      LineTo(X3, Y3);

      X1 := Pt3.x + (Pt2.x - Pt3.x) * LineIndex div (GridLineCount + 1);
      Y1 := Pt3.Y + (Pt2.Y - Pt3.Y) * LineIndex div (GridLineCount + 1);
      MoveTo(X1, Y1);

      X2 := Pt3.x + (Pt1.x - Pt3.x) * LineIndex div (GridLineCount + 1);
      Y2 := Pt3.Y + (Pt1.Y - Pt3.Y) * LineIndex div (GridLineCount + 1);
      LineTo(X2, Y2);
    end;
  end;
end;

procedure TfrmPiperGraph.DrawDiamondGridLines(Canvas: TCanvas);
var
  LineIndex: integer;
  X1, Y1: integer;
  X2, Y2: integer;
begin
  with Canvas do
  begin
    for LineIndex := 1 to GridLineCount do
    begin
      X1 := ptDBottom.x + (ptDLeft.x - ptDBottom.x) * LineIndex div
        (GridLineCount + 1);
      Y1 := ptDBottom.Y + (ptDLeft.Y - ptDBottom.Y) * LineIndex div
        (GridLineCount + 1);
      MoveTo(X1, Y1);

      X2 := ptDRight.x + (ptDTop.x - ptDRight.x) * LineIndex div (GridLineCount
        + 1);
      Y2 := ptDRight.Y + (ptDTop.Y - ptDRight.Y) * LineIndex div (GridLineCount
        + 1);
      LineTo(X2, Y2);

      X1 := ptDBottom.x + (ptDRight.x - ptDBottom.x) * LineIndex div
        (GridLineCount + 1);
      Y1 := ptDBottom.Y + (ptDRight.Y - ptDBottom.Y) * LineIndex div
        (GridLineCount + 1);
      MoveTo(X1, Y1);

      X2 := ptDLeft.x + (ptDTop.x - ptDLeft.x) * LineIndex div (GridLineCount +
        1);
      Y2 := ptDLeft.Y + (ptDTop.Y - ptDLeft.Y) * LineIndex div (GridLineCount +
        1);
      LineTo(X2, Y2);
    end;
  end;
end;

procedure TfrmPiperGraph.DrawAnArrow(Canvas: TCanvas; Pt1, Pt2: TPoint;
  Printing: boolean);
const
  ArrowheadLengthConst = 5;
  ArrowheadThickness = 1;
var
  Angle: double;
  UpAngle, DownAngle: double;
  ArrowHead: array[0..2] of TPoint;
  AColor: TColor;
  ArrowheadLength: integer;
  LocalThickness: integer;
begin
  LocalThickness := Canvas.Pen.Width;
  try
    if Printing then
    begin
      Canvas.Pen.Width := Round(ArrowheadThickness * Multiplier);
    end
    else
    begin
      Canvas.Pen.Width := ArrowheadThickness;
    end;
    ArrowheadLength := ArrowheadLengthConst;
    if Printing then
    begin
      ArrowheadLength := Round(Multiplier * ArrowheadLength);
    end;
    Angle := ArcTan2(Pt2.Y - Pt1.y, Pt2.x - Pt1.x);
    UpAngle := -Angle + Pi / 6;
    DownAngle := -Angle - Pi / 6;

    ArrowHead[0] := Pt2;
    ArrowHead[1].X := Pt2.x - Round(ArrowheadLength * Cos(UpAngle));
    ArrowHead[1].Y := Pt2.y + Round(ArrowheadLength * Sin(UpAngle));
    ArrowHead[2].X := Pt2.x - Round(ArrowheadLength * Cos(DownAngle));
    ArrowHead[2].Y := Pt2.y + Round(ArrowheadLength * Sin(DownAngle));

    with Canvas do
    begin
      AColor := Brush.Color;
      try
        Brush.Color := clBlack;
        MoveTo(Pt1.X, Pt1.y);
        LineTo(Pt2.x, Pt2.y);
        Polygon(Arrowhead);
      finally
        Brush.Color := AColor
      end;
    end;
  finally
    Canvas.Pen.Width := LocalThickness;
  end;

end;

procedure TfrmPiperGraph.DrawArrows(Canvas: TCanvas; Printing: boolean);
const
  Border = 10;
var
  Pt1, Pt2: TPoint;
  Length: double;
  ThisBorder: integer;
begin
  Length := 0.2 * Sqrt(Sqr(ptDTop.x - ptDLeft.x) + Sqr(ptDTop.y - ptDLeft.y));
  ThisBorder := Border;
  if Printing then
  begin
    ThisBorder := Round(Multiplier * ThisBorder);
  end;

  Pt1.x := (ptDTop.x + ptDLeft.x) div 2 - Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptDTop.Y + ptDLeft.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt2.x := (ptDTop.x + ptDLeft.x) div 2 + Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptDTop.Y + ptDLeft.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt1.x := (ptDTop.x + ptDRight.x) div 2 + Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptDTop.Y + ptDRight.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt2.x := (ptDTop.x + ptDRight.x) div 2 - Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptDTop.Y + ptDRight.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt1.x := (ptMg.x + ptCa.x) div 2 - Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptMg.Y + ptCa.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt2.x := (ptMg.x + ptCa.x) div 2 + Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptMg.Y + ptCa.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt2.x := (ptMg.x + ptNaK.x) div 2 + Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptMg.Y + ptNaK.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt1.x := (ptMg.x + ptNaK.x) div 2 - Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptMg.Y + ptNaK.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt2.x := (ptCO3.x + ptSO4.x) div 2 - Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptCO3.Y + ptSO4.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt1.x := (ptCO3.x + ptSO4.x) div 2 + Round(Length * Cos(Pi / 3)) -
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptCO3.Y + ptSO4.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt1.x := (ptCl.x + ptSO4.x) div 2 + Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt1.Y := (ptCl.Y + ptSO4.Y) div 2 + Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  Pt2.x := (ptCl.x + ptSO4.x) div 2 - Round(Length * Cos(Pi / 3)) +
    Round(ThisBorder * Sin(Pi / 3));
  Pt2.Y := (ptCl.Y + ptSO4.Y) div 2 - Round(Length * Sin(Pi / 3)) -
    Round(ThisBorder * Cos(Pi / 3));
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt2.X := (ptCa.x + ptNaK.x) div 2 - Round(Length);
  Pt2.Y :=  ptCa.Y + ThisBorder;
  Pt1.X := (ptCa.x + ptNaK.x) div 2 + Round(Length);
  Pt1.Y :=  ptCa.Y + ThisBorder;
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);

  Pt1.X := (ptCl.x + ptCO3.x) div 2 - Round(Length);
  Pt1.Y :=  ptCl.Y + ThisBorder;
  Pt2.X := (ptCl.x + ptCO3.x) div 2 + Round(Length);
  Pt2.Y :=  ptCl.Y + ThisBorder;
  DrawAnArrow(Canvas, Pt1, Pt2, Printing);
end;

procedure TfrmPiperGraph.DrawGridLines(Canvas: TCanvas);
var
  APenStyle: TPenStyle;
begin
  if GridLines then
  begin
    APenStyle := Canvas.Pen.Style;
    try
      Canvas.Pen.Style := psDash;
      DrawTriangeGridLines(Canvas, ptCa, ptMg, ptNaK);
      DrawTriangeGridLines(Canvas, ptCO3, ptSO4, ptCl);
      DrawDiamondGridLines(Canvas);
    finally
      Canvas.Pen.Style := APenStyle;
    end;
  end;

end;

procedure TfrmPiperGraph.DrawLegend(Canvas: TCanvas; Printing, Saving: boolean);
const
  VerticalSeparationConst = 5;
  HorizontalSeparationConst = 15;
var
  LineIndex: integer;
  TextHeight: integer;
  X, Y: integer;
  LegendObject: TLegendObject;
  RadiusUsed: integer;
  AColor: TColor;
  VerticalSeparation: integer;
  HorizontalSeparation: integer;
  FirstItem: boolean;
  LegendTop, LegendExplWidth: integer;
  procedure GetColor;
  begin
    case ColorSelection of
      csAllBlack:
        begin
          AColor := clBlack;
        end;
      csLinked:
        begin
          AColor := Colors[LegendObject.Symbol];
        end;
      csIndividual:
        begin
          AColor := LegendObject.Color;
        end;
    else
      ;
      begin
        Assert(False);
      end;
    end;
  end;
  procedure UpdateRect;
  begin
    if FirstItem then
    begin
      LegendRect := LegendObject.Rect;
      LegendRect.Left := LegendRect.Left;
      LegendRect.Right := LegendRect.Right;
    end
    else
    begin
      LegendRect.Right := Max(LegendRect.Right, LegendObject.Rect.Right + 2);
      LegendRect.Bottom := Max(LegendRect.Bottom, LegendObject.Rect.Bottom);
    end;
  end;
begin
  LegendExplWidth := 0;
  LegendTop := 0;
  if LinkRadius then
  begin
    RadiusUsed := MinSymbolSize;
  end
  else
  begin
    RadiusUsed := SymbolRadius;
  end;
  if Printing then
  begin
    VerticalSeparation := Round(Multiplier * VerticalSeparationConst);
    HorizontalSeparation := Round(Multiplier * HorizontalSeparationConst);
    RadiusUsed := Round(Multiplier * RadiusUsed);
    Canvas.Font.Assign(PrintFont);
    X := Round(LegendX * Multiplier);
    Y := Round((LegendY) * Multiplier + TitleBottom);
  end
  else
  begin
    VerticalSeparation := VerticalSeparationConst;
    HorizontalSeparation := HorizontalSeparationConst;
    RadiusUsed := RadiusUsed;
    Canvas.Font.Assign(TextFont);
    X := LegendX;
    Y := LegendY + TitleBottom;
  end;

  FirstItem := True;
  if LinkRadius or (Legend.Count > 0) then
  begin
    LegendTop := Y;
    LegendExplWidth := Canvas.TextWidth('_EXPLANATION');
    Canvas.TextOut(X, Y, 'EXPLANATION');
    LegendObject := TLegendObject.Create;
    try
      LegendObject.Color := clBlack;
      LegendObject.Symbol := gsCircle;
      Y := Y + LegendObject.Height(FloatToStr(MinTDS), Canvas, RadiusUsed,
        VerticalSeparation, TextHeight);
    finally
      LegendObject.Free;
    end;
  end;
  for LineIndex := 0 to Legend.Count - 1 do
  begin
    LegendObject := Legend.Objects[LineIndex] as TLegendObject;
    GetColor;
    LegendObject.Draw(Legend[LineIndex], Canvas, RadiusUsed, AColor,
      HorizontalSeparation, VerticalSeparation, X, Y);
    UpdateRect;
    FirstItem := False;
  end;
  if LinkRadius then
  begin
    LegendObject := TLegendObject.Create;
    try
      LegendObject.Color := clBlack;
      LegendObject.Symbol := gsCircle;
      GetColor;
      Y := Y + LegendObject.Height(FloatToStr(MinTDS), Canvas, RadiusUsed,
        VerticalSeparation, TextHeight);

      LegendObject.Draw(FloatToStr(MinTDS), Canvas, RadiusUsed, AColor,
        HorizontalSeparation, VerticalSeparation, X, Y);
      UpdateRect;
      FirstItem := False;

      RadiusUsed := MaxSymbolSize;
      if Printing then
      begin
        RadiusUsed := Round(Multiplier * RadiusUsed);
      end;
      LegendObject.Draw(FloatToStr(MaxTDS), Canvas, RadiusUsed, AColor,
        HorizontalSeparation, VerticalSeparation, X, Y);
      UpdateRect;
    finally
      LegendObject.Free;
    end;

  end;

  LegendRect.Top := LegendTop;
  if LegendRect.Right - LegendRect.Left < LegendExplWidth then
  begin
    LegendRect.Right := LegendRect.Left + LegendExplWidth;
  end;

  if LegendSelected and not Printing and not Saving then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(LegendRect);

  end;
end;

procedure TfrmPiperGraph.SelectLegend(X, Y: integer);
begin
  LegendSelected := ((Legend.Count > 0) or LinkRadius) and (X >= LegendRect.Left)
    and (X <= LegendRect.Right) and (Y >= LegendRect.Top)
    and (Y <= LegendRect.Bottom);
end;

procedure TfrmPiperGraph.PaintGraph(Width, Height: integer; Canvas: TCanvas;
  Printing, Saving: boolean);
var
  AFont, TempPrintFont, TempPrintTitleFont, TempTextFont, TempTitleFont: TFont;

begin
  AFont := TFont.Create;
  TempPrintFont := TFont.Create;
  TempPrintTitleFont := TFont.Create;
  TempTextFont := TFont.Create;
  TempTitleFont := TFont.Create;
  try
    AFont.Assign(Canvas.Font);
    TempPrintFont.Assign(PrintFont);
    TempPrintTitleFont.Assign(PrintTitleFont);
    TempTextFont.Assign(TextFont);
    TempTitleFont.Assign(TitleFont);
    try
      GetDimensions(Width, Height, Canvas, Printing);
      PaintAxes(Canvas, Printing);
      DrawGridLines(Canvas);
      DrawArrows(Canvas, Printing);
      DrawValues(Canvas, Printing);
      DrawText(Canvas, Printing);
      DrawLegend(Canvas, Printing, Saving);
    finally
      Canvas.Font.Assign(AFont);
      PrintFont.Assign(TempPrintFont);
      PrintTitleFont.Assign(TempPrintTitleFont);
      TextFont.Assign(TempTextFont);
      TitleFont.Assign(TempTitleFont);
    end;

  finally
    AFont.Free;
    TempPrintFont.Free;
    TempPrintTitleFont.Free;
    TempTextFont.Free;
    TempTitleFont.Free;
  end;

end;

procedure TfrmPiperGraph.PaintBox1Paint(Sender: TObject);
begin
  if not Wait then
  begin
    PaintGraph(PaintBox1.ClientWidth, PaintBox1.ClientHeight,
      PaintBox1.Canvas, False, False);
  end;
end;

procedure TfrmPiperGraph.DataGrid1SetEditText(Sender: TObject; ACol, ARow:
  Integer;
  const Value: string);
const
  Epsilon = 1E-8;
  eqWeights: array[Low(TSpecies)..High(TSpecies)] of double
  = (eqCa, eqMg, eqNa, eqK, eqCO3, eqHCO3, eqCl, eqSO4, eqTDS);
var
  OK: boolean;
  ColIndex: TSpecies;
  AValue: double;
  LocalColorIndex: integer;
  ColorSymbolIndex: TGraphSymbol;
  AColor: TColor;
  Sum: double;
begin
  OK := True;
  with DataGrid1 do
  begin
    if (ACol = SymbolIndex) and (ColorSelection <> csIndividual) then
    begin
      LocalColorIndex := Columns[SymbolIndex].Picklist.IndexOf(Cells[ACol,
        ARow]);
      Assert((LocalColorIndex > -1)
        and (LocalColorIndex <= Ord(High(TGraphSymbol))));
      ColorSymbolIndex := TGraphSymbol(LocalColorIndex);
      AColor := Colors[ColorSymbolIndex];
      Cells[ColorIndex, ARow] := IntToStr(AColor);
    end;
    for ColIndex := Low(TSpecies) to High(TSpecies) do
    begin
      try
        if Cells[Ord(ColIndex), ARow] = '' then
        begin
          OK := False;
          AValue := 0;
        end
        else
        begin
          AValue := StrToFloat(Cells[Ord(ColIndex), ARow]);
        end;
      except on EConvertError do
        begin
          OK := False;
          AValue := 0;
        end;
      end;
      Values[ColIndex, ARow - 1] := AValue
    end;
    if OK then
    begin
      if rgDataType.ItemIndex = 1 then
      begin
        for ColIndex := Ca to K do
        begin
          Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] / 100;
        end;
      end
      else if rgDataType.ItemIndex = 2 then
      begin
        Sum := 0;
        for ColIndex := Ca to K do
        begin
          Sum := Sum + Values[ColIndex, ARow - 1];
        end;
        for ColIndex := Ca to K do
        begin
          Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] / Sum;
        end;
      end
      else if rgDataType.ItemIndex = 3 then
      begin
        Sum := 0;
        for ColIndex := Ca to K do
        begin
          Sum := Sum + Values[ColIndex, ARow - 1] / eqWeights[ColIndex];
        end;
        for ColIndex := Ca to K do
        begin
          Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] /
            eqWeights[ColIndex] / Sum;
        end;
      end;
      if rgDataType.ItemIndex < 2 then
      begin
        AValue := 0;
        for ColIndex := Ca to K do
        begin
          AValue := AValue + Values[ColIndex, ARow - 1];
        end;
        if (AValue < 1 - Epsilon) or (AValue > 1 + Epsilon) then
        begin
          OK := False;
        end;
      end;
      if OK then
      begin
        if rgDataType.ItemIndex = 1 then
        begin
          for ColIndex := CO3 to SO4 do
          begin
            Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] / 100;
          end;
        end
        else if rgDataType.ItemIndex = 2 then
        begin
          Sum := 0;
          for ColIndex := CO3 to SO4 do
          begin
            Sum := Sum + Values[ColIndex, ARow - 1];
          end;
          for ColIndex := CO3 to SO4 do
          begin
            Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] / Sum;
          end;
        end
        else if rgDataType.ItemIndex = 3 then
        begin
          Sum := 0;
          for ColIndex := CO3 to SO4 do
          begin
            Sum := Sum + Values[ColIndex, ARow - 1] / eqWeights[ColIndex];
          end;
          for ColIndex := CO3 to SO4 do
          begin
            Values[ColIndex, ARow - 1] := Values[ColIndex, ARow - 1] /
              eqWeights[ColIndex] / Sum;
          end;
        end;
        if rgDataType.ItemIndex < 2 then
        begin
          AValue := 0;
          for ColIndex := CO3 to SO4 do
          begin
            AValue := AValue + Values[ColIndex, ARow - 1];
          end;
          if (AValue < 1 - Epsilon) or (AValue > 1 + Epsilon) then
          begin
            OK := False;
          end;
        end;

      end;
    end;
    OKArray[ARow - 1] := OK;
    Invalidate;
  end;
  if (ACol = ord(TDS)) and (ARow > 0) then
  begin
    GetTDSRAnge;
  end;
  Modified := True;
end;

procedure TfrmPiperGraph.FormDestroy(Sender: TObject);
begin
  TextFont.Free;
  PrintFont.Free;
  TitleFont.Free;
  PrintTitleFont.Free;
  ClearLegend;
  Legend.Free;
end;

procedure TfrmPiperGraph.DataGrid1DrawCaptions(Sender: TObject; ACol, ARow:
  Integer;
  Rect: TRect; State: TGridDrawState);
var
  AText: string;
  AChar: Char;
  ASize: TSize;
  CharIndex: integer;
  CharLeft, CharTop: integer;
  IsDigit: boolean;
  OldFontSize: integer;
  TextTop: integer;
begin
  with DataGrid1 do
  begin
    with Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect);
      TextTop := Rect.Top + 2;
      CharLeft := Rect.Left + 2;
      AText := Columns[ACol].Title.Caption;
      for CharIndex := 1 to Length(AText) do
      begin
        AChar := AText[CharIndex];
        IsDigit := GetIsDigit(AChar);
        OldFontSize := 0;
        if IsDigit then
        begin
          OldFontSize := Font.Height;
          CharTop := TextTop - OldFontSize div 2;
          Canvas.Font.Height := OldFontSize * 3 div 4;
        end
        else
        begin
          CharTop := TextTop;
        end;
        ASize := TextExtent(AChar);

        Rect.Top := CharTop;
        Rect.Left := CharLeft;

        TextRect(Rect, Rect.Left, Rect.Top, AChar);

        CharLeft := CharLeft + ASize.cx;
        if IsDigit then
        begin
          Font.Height := OldFontSize;
        end;
      end;
    end;
  end;
end;

procedure TfrmPiperGraph.DataGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  Epsilon = 1E-8;
var
  StartCol: integer;
  ColIndex: integer;
  Sum: double;
  AColor: TColor;
  InvalidCell: boolean;
  AnInt: integer;
  procedure CheckCell;
  begin
    with DataGrid1 do
    begin
      InvalidCell := False;
      try
        if Cells[ACol, ARow] = '' then
        begin
          InvalidCell := True;
        end
        else
        begin
          InternationalStrToFloat(Cells[ACol, ARow]);
        end;

      except on EConvertError do
        begin
          InvalidCell := True;
        end;

      end;
      if InvalidCell then
      begin
        with Canvas do
        begin
          Brush.Color := clRed;
          FillRect(Rect);
          TextRect(Rect, Rect.Left, Rect.Top, Cells[ACol, ARow]);
        end;
      end;
    end;
  end;
begin
  with DataGrid1 do
  begin
    if (ARow <> Row) or (ACol <> Col) then
    begin
      if ARow = 0 then
      begin
        DataGrid1DrawCaptions(Sender, ACol, ARow, Rect, State);
      end
      else
      begin
        if (ACol <= Ord(High(TSpecies)) - 1) then
        begin
          if (rgDataType.ItemIndex < 2) then
          begin
            if ACol < Ord(CO3) then
            begin
              StartCol := 0;
            end
            else
            begin
              StartCol := Ord(CO3);
            end;
            try
              Sum := 0;
              for ColIndex := StartCol to StartCol + 3 do
              begin
                if Cells[ColIndex, ARow] = '' then
                begin
                  Sum := 0;
                  break;
                end
                else
                begin
                  Sum := Sum + InternationalStrToFloat(Cells[ColIndex, ARow]);
                end;
              end;
            except on EConvertError do
              begin
                Sum := 0;
              end;
            end;
            if rgDataType.ItemIndex = 1 then
            begin
              Sum := Sum / 100;
            end;

            if (Sum > 1 + Epsilon) or (Sum < 1 - Epsilon) then
            begin
              with Canvas do
              begin
                Brush.Color := clRed;
                FillRect(Rect);
                TextRect(Rect, Rect.Left, Rect.Top, Cells[ACol, ARow]);
              end;
            end;
          end
          else
          begin
            CheckCell
          end;
        end
        else if ACol = ColorIndex then
        begin
          with Canvas do
          begin
            if ColorSelection = csAllBlack then
            begin
              AColor := clBlack;
            end
            else
            begin
              if TryStrToInt(Cells[ACol, ARow], AnInt) then
              begin
                AColor := AnInt;
              end
              else
              begin
                AColor := clBlack;
              end;
            end;
            Brush.Color := AColor;
            FillRect(Rect);
          end;
        end
        else if ACol = Ord(High(TSpecies)) then
        begin
          CheckCell;
        end;
      end;
    end;
  end;
end;

procedure TfrmPiperGraph.cbPercentClick(Sender: TObject);
begin
  DataGrid1.Invalidate;
  SetValues;
end;

function TfrmPiperGraph.GetRectBorder(Printing: boolean): integer;
const
  BorderConst = 6;
begin
  if Printing then
  begin
    result := Round(BorderConst * Multiplier);
  end
  else
  begin
    result := BorderConst;
  end;
end;

function TfrmPiperGraph.GetIsDigit(AChar: Char): boolean;
var
  DigitIndex: integer;
begin
  result := False;
  for DigitIndex := 0 to 9 do
  begin
    if AChar = Digits[DigitIndex] then
    begin
      result := True;
      Break;
    end;
  end;
end;

function TfrmPiperGraph.GetIsValance(AChar: Char): Integer;
var
  ValanceIndex: integer;
begin
  result := -1;
  for ValanceIndex := 0 to 3 do
  begin
    if AChar = Valance[ValanceIndex] then
    begin
      result := ValanceIndex;
      Break;
    end;
  end;
end;

function TfrmPiperGraph.GetRect(Canvas: TCanvas; TextString: string;
  Position: TPoint; AnOrientation: TOrientation; IsNumber, UseOffset,
  Printing, RotateRight: boolean): TRect;
var
  StringLength, StringHeight: integer;
  AChar: Char;
  AString: string;
  CharIndex: integer;
  IsDigit, IsValance: boolean;
  OldFontSize: integer;
  Border: integer;
  ValenceIndex: integer;
  ValanceUsed: boolean;
begin
  Border := GetRectBorder(Printing);
  with Canvas do
  begin
    StringHeight := Abs(Font.Height * 3 div 2);
    if ((AnOrientation = oBelowLeft)
      or (AnOrientation = oBelowCenter)
      or (AnOrientation = oBelowRight)) then
    begin
      Border := Border + StringHeight div 3;
      if ShowNumbers and not IsNumber then
      begin
        Border := Border + StringHeight div 3
      end;
    end;
    StringLength := 0;
    ValanceUsed := False;
    for CharIndex := 1 to Length(TextString) do
    begin
      AChar := TextString[CharIndex];
      AString := AChar;
      IsDigit := False;
      IsValance := False;
      if not IsNumber then
      begin
        IsDigit := GetIsDigit(AChar);
        ValenceIndex := GetIsValance(AChar);
        IsValance := ValenceIndex > -1;
        if IsValance then
        begin
          AString := ValanceStrings[ValenceIndex];
          ValanceUsed := True;
        end;
      end;
      OldFontSize := 0;
      if IsDigit or IsValance then
      begin
        OldFontSize := Font.Height;
        Font.Height := OldFontSize * 3 div 4;
      end;
      StringLength := StringLength + TextWidth(AString);

      if IsDigit or IsValance then
      begin
        Font.Height := OldFontSize;
      end;
    end;
    with Result do
    begin
      with Position do
      begin
        case AnOrientation of
          oBelowLeft:
            begin
              Left := x;
              Top := y + Border;
              if ShowNumbers and not IsNumber then
              begin
                Top := Top + LabelTextSize
              end;
            end;
          oBelowCenter:
            begin
              Left := x - StringLength div 2;
              Top := y + Border;
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top + LabelTextSize
              end;
            end;
          oBelowRight:
            begin
              Left := x - StringLength;
              Top := y + Border;
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top + LabelTextSize
              end;
            end;
          oAboveLeft:
            begin
              Left := x;
              Top := y - StringHeight - Border div 2;
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top - LabelTextSize
              end;
            end;
          oAboveCenter:
            begin
              Left := x - StringLength div 2;
              Top := y - StringHeight - Border div 2;
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top - LabelTextSize;
              end;
              if ValanceUsed and RotateRight then
              begin
                Top := Top - LabelTextSize div 2;
              end;
            end;
          oAboveRight:
            begin
              Left := x - StringLength;
              Top := y - StringHeight - Border div 2;
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top - LabelTextSize
              end;
            end;
          oLeft:
            begin
              Left := x + Border;
              Top := y - StringHeight div 3 {+ Border div 2};
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top - LabelTextSize
              end;
            end;
          oRight:
            begin
              Left := x - StringLength - Border;
              Top := y - StringHeight div 3 {+ Border div 2};
              if ShowNumbers and not IsNumber and UseOffset then
              begin
                Top := Top - LabelTextSize
              end;
            end;
        else
          ;
          begin
            Assert(False);
          end;
        end;
      end;
      Right := Left + StringLength;
      Bottom := Top + StringHeight;
    end;
  end;
end;

procedure TfrmPiperGraph.DrawAText(Canvas: TCanvas; TextString: string;
  Position: TPoint; AnOrientation: TOrientation; IsNumber, Printing,
  RotateRight: boolean);
var
  AChar: Char;
  ASize: TSize;
  CharIndex: integer;
  CharLeft, CharTop: integer;
  IsDigit, IsValance: boolean;
  OldFontSize: integer;
  TextTop: integer;
  Rect: TRect;
  AString: string;
  ValanceIndex: integer;
begin
  Rect := GetRect(Canvas, TextString, Position, AnOrientation, IsNumber, True,
    Printing, RotateRight);
  TextTop := Rect.Top;
  CharLeft := Rect.Left;
  with Canvas do
  begin
    if Printing then
    begin
      Font.Assign(PrintFont);
    end
    else
    begin
      Font.Assign(TextFont);
    end;
    for CharIndex := 1 to Length(TextString) do
    begin
      AChar := TextString[CharIndex];
      AString := AChar;
      IsDigit := False;
      IsValance := False;
      OldFontSize := 0;
      ValanceIndex := -1;
      if not IsNumber then
      begin
        IsDigit := GetIsDigit(AChar);
        ValanceIndex := GetIsValance(AChar);
        IsValance := ValanceIndex > -1;
      end;
      if IsDigit then
      begin
        OldFontSize := Font.Height;
        CharTop := TextTop - OldFontSize div 2;
        Font.Height := OldFontSize * 3 div 4;
      end
      else if IsValance then
      begin
        OldFontSize := Font.Height;
        CharTop := TextTop + OldFontSize div 2;
        Font.Height := OldFontSize * 3 div 4;
        AString := ValanceStrings[ValanceIndex];
      end
      else
      begin
        CharTop := TextTop;
      end;
      ASize := TextExtent(AString);

      with Rect do
      begin
        Top := CharTop;
        Left := CharLeft;

        TextOut(Left, Top, AString);
      end;

      CharLeft := CharLeft + ASize.cx;
      if IsDigit or IsValance then
      begin
        Font.Height := OldFontSize;
      end;
    end;
  end;
end;

procedure TfrmPiperGraph.DrawText(Canvas: TCanvas; Printing: boolean);

var
  TempPoint: TPoint;
  OffSet: integer;
  TextHeight: integer;
const
  BottomOffset = 8;

begin
  //('!', '@', '#', '$') represent (2-, -, +, 2+)
//  DrawAText(Canvas, 'Ca$', ptCa, oBelowCenter, False, Printing, False);
  TempPoint.X := (ptCa.x + ptNaK.x) div 2;
  TempPoint.Y := (ptCa.y + ptNaK.y) div 2 + BottomOffset;
  DrawAText(Canvas, 'Ca$', TempPoint, oBelowCenter, False, Printing, False);

  TextHeight := Canvas.TextHeight('CATIONS');
  TempPoint.Y := TempPoint.Y + TextHeight;// * 3 div 2;
  DrawAText(Canvas, 'CATIONS', TempPoint, oBelowCenter, False, Printing, False);

//  DrawAText(Canvas, 'Mg$', ptMg, oAboveCenter, False, Printing, False);
  TempPoint.X := (ptCa.x + ptMg.x) div 2;
  TempPoint.Y := (ptCa.y + ptMg.y) div 2;
  MyRotatePrint(Canvas, 'Mg$', TempPoint.X,
    TempPoint.Y, 60, oAboveCenter, Printing, False);

//  DrawAText(Canvas, 'Na# + K#', ptNaK, oBelowRight, False, Printing, False);
  TempPoint.X := (ptNaK.x + ptMg.x) div 2;
  TempPoint.Y := (ptNaK.y + ptMg.y) div 2;
  MyRotatePrint(Canvas, 'Na# + K#', TempPoint.X,
    TempPoint.Y, -60, oAboveCenter, Printing, False);

//  DrawAText(Canvas, 'CO3! + HCO3@', ptCO3, oBelowLeft, False, Printing, False);
  TempPoint.X := (ptCO3.x + ptSO4.x) div 2;
  TempPoint.Y := (ptCO3.y + ptSO4.y) div 2;
  MyRotatePrint(Canvas, 'CO3! + HCO3@', TempPoint.X,
    TempPoint.Y, 60, oAboveCenter, Printing, False);

//  DrawAText(Canvas, 'SO4!', ptSO4, oAboveCenter, False, Printing, False);
  TempPoint.X := (ptCl.x + ptSO4.x) div 2;
  TempPoint.Y := (ptCl.y + ptSO4.y) div 2;
  MyRotatePrint(Canvas, 'SO4!', TempPoint.X,
    TempPoint.Y, -60, oAboveCenter, Printing, False);

//  DrawAText(Canvas, 'Cl@', ptCl, oBelowCenter, False, Printing, False);
  TempPoint.X := (ptCl.x + ptCO3.x) div 2;
  TempPoint.Y := (ptCl.y + ptCO3.y) div 2 + BottomOffset;
  DrawAText(Canvas, 'Cl@', TempPoint, oBelowCenter, False, Printing, False);

  TextHeight := Canvas.TextHeight('ANIONS');
  TempPoint.Y := TempPoint.Y + TextHeight;// * 3 div 2;
  DrawAText(Canvas, 'ANIONS', TempPoint, oBelowCenter, False, Printing, False);

  MyRotatePrint(Canvas, 'SO4! + Cl@', (ptDLeft.x + ptDTop.x) div 2,
    (ptDLeft.y + ptDTop.y) div 2, 60, oAboveCenter, Printing, False);

  MyRotatePrint(Canvas, 'Ca$ + Mg$', (ptDRight.x + ptDTop.x) div 2,
    (ptDRight.y + ptDTop.y) div 2, -60, oAboveCenter, Printing, True);

  if ShowNumbers then
  begin
    if Printing then
    begin
      Canvas.Font := PrintFont;
    end
    else
    begin
      Canvas.Font := TextFont;
    end;
    OffSet := LabelTextSize div 4;

    DrawAText(Canvas, '100', ptCa, oBelowCenter, True, Printing, False);
    DrawAText(Canvas, '0', ptCa, oRight, True, Printing, False);
    DrawAText(Canvas, '0', ptNaK, oBelowRight, True, Printing, False);
    DrawAText(Canvas, '100', ptNaK, oLeft, True, Printing, False);

    TempPoint := ptMg;
    if ShowNumbers then
    begin
      TempPoint.x := TempPoint.x - OffSet;
    end;
    DrawAText(Canvas, '100', TempPoint, oAboveRight, True, Printing, False);

    TempPoint := ptMg;
    if ShowNumbers then
    begin
      TempPoint.x := TempPoint.x + OffSet;
    end;
    DrawAText(Canvas, '0', TempPoint, oAboveLeft, True, Printing, False);

    DrawAText(Canvas, '0', ptCO3, oBelowCenter, True, Printing, False);
    DrawAText(Canvas, '100', ptCO3, oRight, True, Printing, False);
    DrawAText(Canvas, '100', ptCl, oBelowRight, True, Printing, False);
    DrawAText(Canvas, '0', ptCl, oLeft, True, Printing, False);

    TempPoint := ptSO4;
    if ShowNumbers then
    begin
      TempPoint.x := TempPoint.x - OffSet;
    end;
    DrawAText(Canvas, '0', TempPoint, oAboveRight, True, Printing, False);

    TempPoint := ptSO4;
    if ShowNumbers then
    begin
      TempPoint.x := TempPoint.x + OffSet;
    end;
    DrawAText(Canvas, '100', TempPoint, oAboveLeft, True, Printing, False);

    DrawAText(Canvas, '100', ptDTop, oAboveCenter, True, Printing, False);
    DrawAText(Canvas, '100', ptDBottom, oBelowCenter, True, Printing, False);
    DrawAText(Canvas, '0', ptDLeft, ORight, True, Printing, False);
    DrawAText(Canvas, '0', ptDRight, oLeft, True, Printing, False);

  end;

end;

procedure TfrmPiperGraph.BitBtn2Click(Sender: TObject);
var
  APrinter: TPrinter;
begin
  if PrinterSetupDialog1.Execute then
  begin
    rgOrientation.ItemIndex := Ord(Printer.Orientation);
    cbPrintPreviewClick(Sender);
    Wait := True;
    try
      PaintBox1.Invalidate;
      APrinter := Printer;
      with APrinter do
      begin
        BeginDoc;
        PaintGraph(PageWidth, PageHeight, Canvas, True, False);
        EndDoc;
      end;
    finally
      Wait := False;
      PaintBox1.Invalidate;
    end;
  end;
end;

procedure TfrmPiperGraph.sbFormatClick(Sender: TObject);
var
  LineIndex: integer;
  ALegendObject: TLegendObject;
begin
  case GridLineCount of
    1:
      begin
        frmPiperFormat.rgGridLines.ItemIndex := 4;
      end;
    3:
      begin
        frmPiperFormat.rgGridLines.ItemIndex := 3;
      end;
    4:
      begin
        frmPiperFormat.rgGridLines.ItemIndex := 2;
      end;
    9:
      begin
        frmPiperFormat.rgGridLines.ItemIndex := 1;
      end;
    19:
      begin
        frmPiperFormat.rgGridLines.ItemIndex := 0;
      end;
  else
    Assert(False);
  end;

  frmPiperFormat.seMargin.Value := Margin;
  frmPiperFormat.seInteriorMargin.Value := InteriorMargin;
  frmPiperFormat.TextFont.Assign(TextFont);
  frmPiperFormat.PrintFont.Assign(PrintFont);
  frmPiperFormat.TitleFont.Assign(TitleFont);
  frmPiperFormat.PrintTitleFont.Assign(PrintTitleFont);
  frmPiperFormat.seLineThickness.Value := LineThickness;
  frmPiperFormat.edTitle.Text := GraphTitle;
  frmPiperFormat.seSymbolRadius.Value := SymbolRadius;

  frmPiperFormat.seMinSymb.Value := MinSymbolSize;
  frmPiperFormat.seMaxSymb.Value := MaxSymbolSize;
  frmPiperFormat.cbSymbolSize.Checked := LinkRadius;
  frmPiperFormat.cbSymbolSizeClick(frmPiperFormat.cbSymbolSize);

  frmPiperFormat.shpCircle.Brush.Color := Colors[gsCircle];
  frmPiperFormat.shpOpenCircle.Brush.Color := Colors[gsCircleOpen];
  frmPiperFormat.shpSquare.Brush.Color := Colors[gsSquare];
  frmPiperFormat.shpOpenSquare.Brush.Color := Colors[gsSquareOpen];
  frmPiperFormat.shpTriangle.Brush.Color := Colors[gsTriangle];
  frmPiperFormat.shpOpenTriangle.Brush.Color := Colors[gsTriangleOpen];
  frmPiperFormat.shpInvertedTriangle.Brush.Color := Colors[gsInvertedTriangle];
  frmPiperFormat.shpOpenInvertedTriangle.Brush.Color :=
    Colors[gsInvertedTriangleOpen];
  frmPiperFormat.shpStar.Brush.Color := Colors[gsStar];
  frmPiperFormat.shpOpenStar.Brush.Color := Colors[gsStarOpen];
  frmPiperFormat.shpCross.Brush.Color := Colors[gsCross];
  frmPiperFormat.shpX.Brush.Color := Colors[gsX];

  frmPiperFormat.Colors[gsCircle] := Colors[gsCircle];
  frmPiperFormat.Colors[gsCircleOpen] := Colors[gsCircleOpen];
  frmPiperFormat.Colors[gsSquare] := Colors[gsSquare];
  frmPiperFormat.Colors[gsSquareOpen] := Colors[gsSquareOpen];
  frmPiperFormat.Colors[gsTriangle] := Colors[gsTriangle];
  frmPiperFormat.Colors[gsTriangleOpen] := Colors[gsTriangleOpen];
  frmPiperFormat.Colors[gsInvertedTriangle] := Colors[gsInvertedTriangle];
  frmPiperFormat.Colors[gsInvertedTriangleOpen] :=
    Colors[gsInvertedTriangleOpen];
  frmPiperFormat.Colors[gsStar] := Colors[gsStar];
  frmPiperFormat.Colors[gsStarOpen] := Colors[gsStarOpen];
  frmPiperFormat.Colors[gsCross] := Colors[gsCross];
  frmPiperFormat.Colors[gsX] := Colors[gsX];
  frmPiperFormat.dgLegend.Columns[1].PickList.Assign(DataGrid1.Columns[SymbolIndex].Picklist);
  frmPiperFormat.seLegendCount.Value := Legend.Count;
  frmPiperFormat.seLegendCountChange(frmPiperFormat.seLegendCount);
  frmPiperFormat.cbGridLines.Checked := Gridlines;
  frmPiperFormat.cbShowNumbers.Checked := ShowNumbers;
  frmPiperFormat.rgColor.ItemIndex := Ord(ColorSelection);

  for LineIndex := 1 to frmPiperFormat.seLegendCount.Value do
  begin
    ALegendObject := Legend.Objects[LineIndex - 1] as TLegendObject;
    frmPiperFormat.dgLegend.Cells[0, LineIndex] :=
      IntToStr(ALegendObject.Color);
    frmPiperFormat.dgLegend.Cells[1, LineIndex] :=
      frmPiperFormat.dgLegend.Columns[1].PickList[Ord(ALegendObject.Symbol)];
    frmPiperFormat.dgLegend.Cells[2, LineIndex] := Legend[LineIndex - 1];
  end;

  if frmPiperFormat.ShowModal = mrOK then
  begin
    Margin := frmPiperFormat.seMargin.Value;
    InteriorMargin := frmPiperFormat.seInteriorMargin.Value;
    TextFont.Assign(frmPiperFormat.TextFont);
    PrintFont.Assign(frmPiperFormat.PrintFont);
    TitleFont.Assign(frmPiperFormat.TitleFont);
    PrintTitleFont.Assign(frmPiperFormat.PrintTitleFont);
    LineThickness := frmPiperFormat.seLineThickness.Value;
    GraphTitle := frmPiperFormat.edTitle.Text;
    SymbolRadius := frmPiperFormat.seSymbolRadius.Value;

    MinSymbolSize := frmPiperFormat.seMinSymb.Value;
    MaxSymbolSize := frmPiperFormat.seMaxSymb.Value;
    LinkRadius := frmPiperFormat.cbSymbolSize.Checked;

    Colors[gsCircle] := frmPiperFormat.shpCircle.Brush.Color;
    Colors[gsCircleOpen] := frmPiperFormat.shpOpenCircle.Brush.Color;
    Colors[gsSquare] := frmPiperFormat.shpSquare.Brush.Color;
    Colors[gsSquareOpen] := frmPiperFormat.shpOpenSquare.Brush.Color;
    Colors[gsTriangle] := frmPiperFormat.shpTriangle.Brush.Color;
    Colors[gsTriangleOpen] := frmPiperFormat.shpOpenTriangle.Brush.Color;
    Colors[gsInvertedTriangle] :=
      frmPiperFormat.shpInvertedTriangle.Brush.Color;
    Colors[gsInvertedTriangleOpen] :=
      frmPiperFormat.shpOpenInvertedTriangle.Brush.Color;
    Colors[gsStar] := frmPiperFormat.shpStar.Brush.Color;
    Colors[gsStarOpen] := frmPiperFormat.shpOpenStar.Brush.Color;
    Colors[gsCross] := frmPiperFormat.shpCross.Brush.Color;
    Colors[gsX] := frmPiperFormat.shpX.Brush.Color;

    ColorSelection := TColorSelection(frmPiperFormat.rgColor.ItemIndex);
    Gridlines := frmPiperFormat.cbGridLines.Checked;
    ShowNumbers := frmPiperFormat.cbShowNumbers.Checked;
    ClearLegend;
    for LineIndex := 1 to frmPiperFormat.seLegendCount.Value do
    begin
      ALegendObject := TLegendObject.Create;
      ALegendObject.Color := StrToInt(frmPiperFormat.dgLegend.Cells[0,
        LineIndex]);
      ALegendObject.Symbol := TGraphSymbol(frmPiperFormat.dgLegend.Columns[1].
        PickList.IndexOf(frmPiperFormat.dgLegend.Cells[1, LineIndex]));
      Legend.AddObject(frmPiperFormat.dgLegend.Cells[2, LineIndex],
        ALegendObject);

    end;

    case frmPiperFormat.rgGridLines.ItemIndex of
      0:
        begin
          GridLineCount := 19;
        end;
      1:
        begin
          GridLineCount := 9;
        end;
      2:
        begin
          GridLineCount := 4;
        end;
      3:
        begin
          GridLineCount := 3;
        end;
      4:
        begin
          GridLineCount := 1;
        end;
    else
      Assert(False);
    end;

    Modified := True;
    PaintBox1.Invalidate;
  end;
end;

procedure TfrmPiperGraph.Cross(Canvas: TCanvas; radius: integer; APoint: TPoint;
  Color: TColor);
var
  LocalRadius: integer;
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  LocalRadius := radius div 3;
  if LocalRadius < 1 then
  begin
    LocalRadius := 1;
  end;
  with Canvas do
  begin
    CurrentPenColor := Pen.Color;
    CurrentBrushColor := Brush.Color;
    try
      Pen.Color := Color;
      Brush.Color := Color;
      Rectangle(APoint.x - radius, APoint.y - LocalRadius,
        APoint.x + radius, APoint.y + LocalRadius);
      Rectangle(APoint.x - LocalRadius, APoint.y - radius,
        APoint.x + LocalRadius, APoint.y + radius);
    finally
      Pen.Color := CurrentPenColor;
      Brush.Color := CurrentBrushColor;
    end;
  end
end;

procedure TfrmPiperGraph.DataGrid1EditButtonClick(Sender: TObject);
var
  AnInt: integer;
begin
  if DataGrid1.Col = ColorIndex then
  begin
    if TryStrToInt(DataGrid1.Cells[DataGrid1.Col,
      DataGrid1.Row], AnInt) then
    begin
      ColorDialog1.Color := AnInt;
    end
    else
    begin
      ColorDialog1.Color := clBlack;
    end;

    if ColorDialog1.Execute then
    begin
      DataGrid1.Cells[DataGrid1.Col, DataGrid1.Row] :=
        IntToStr(ColorDialog1.Color);
    end;
  end
  else if DataGrid1.Col = Ord(CO3) then
  begin
    frmEditCO3.GetData;
    frmEditCO3.ShowModal;
  end;
end;

procedure TfrmPiperGraph.DataGrid1SelectCell(Sender: TObject; ACol, ARow:
  Integer;
  var CanSelect: Boolean);
begin
  if (ACol = ColorIndex) and (ColorSelection <> csIndividual) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmPiperGraph.SaveFont(AStringList: TStringlist; AFont: TFont);
var
  StyleIndex: TFontStyle;
begin
  AStringList.Add(IntToStr(AFont.Charset));
  AStringList.Add(IntToStr(AFont.Color));
  AStringList.Add(IntToStr(AFont.Height));
  AStringList.Add(AFont.Name);
  AStringList.Add(IntToStr(Ord(AFont.Pitch)));
  AStringList.Add(IntToStr(AFont.Size));
  for StyleIndex := Low(TFontStyle) to High(TFontStyle) do
  begin
    if StyleIndex in AFont.Style then
    begin
      AStringList.Add('1');
    end
    else
    begin
      AStringList.Add('0');
    end;
  end;
end;

procedure TfrmPiperGraph.LoadFont(AFont: TFont);
var
  AnInteger: integer;
  AString: string;
  StyleIndex: TFontStyle;
begin
  Read(AFile, AnInteger);
  AFont.Charset := AnInteger;

  Read(AFile, AnInteger);
  AFont.Color := AnInteger;

  Read(AFile, AnInteger);
  AFont.Height := AnInteger;

  Readln(AFile);
  Readln(AFile, AString);
  AFont.Name := AString;

  Read(AFile, AnInteger);
  AFont.Pitch := TFontPitch(AnInteger);

  Read(AFile, AnInteger);
  AFont.Size := AnInteger;

  for StyleIndex := Low(TFontStyle) to High(TFontStyle) do
  begin
    Read(AFile, AnInteger);
    if AnInteger = 1 then
    begin
      AFont.Style := AFont.Style + [StyleIndex];
    end
    else
    begin
      AFont.Style := AFont.Style - [StyleIndex];
    end;
  end;
end;

procedure TfrmPiperGraph.sbSaveFileClick(Sender: TObject);
const
  Version = '3';
var
  AStringList: TStringList;
  RowIndex, ColIndex: integer;
  ColorIndex: TGraphSymbol;
  LegendIndex: integer;
  LegendObject: TLegendObject;
begin
  if SaveDialog1.Execute then
  begin
    AStringList := TStringList.Create;
    try
      AStringList.Add(Version);
      AStringList.Add(IntToStr(Width));
      AStringList.Add(IntToStr(Height));
      AStringList.Add(IntToStr(seNumPoints.Value));
      AStringList.Add(IntToStr(rgDataType.ItemIndex));

      AStringList.Add(IntToStr(Ord(ColorSelection)));

      if GridLines then
      begin
        AStringList.Add('1');
      end
      else
      begin
        AStringList.Add('0');
      end;
      AStringList.Add(IntToStr(GridLineCount));

      AStringList.Add(IntToStr(Margin));
      AStringList.Add(IntToStr(InteriorMargin));
      AStringList.Add(IntToStr(LineThickness));
      AStringList.Add(GraphTitle);
      AStringList.Add(IntToStr(SymbolRadius));

      if ShowNumbers then
      begin
        AStringList.Add('1');
      end
      else
      begin
        AStringList.Add('0');
      end;
      if LinkRadius then
      begin
        AStringList.Add('1');
      end
      else
      begin
        AStringList.Add('0');
      end;

      AStringList.Add(IntToStr(LabelTextSize));
      AStringList.Add(IntToStr(LegendX));
      AStringList.Add(IntToStr(LegendY));
      AStringList.Add(IntToStr(MinSymbolSize));
      AStringList.Add(IntToStr(MaxSymbolSize));
      AStringList.Add(IntToStr(rgOrientation.ItemIndex));

      for RowIndex := 1 to DataGrid1.RowCount - 1 do
      begin
        for ColIndex := 0 to DataGrid1.ColCount - 1 do
        begin
          AStringList.Add(DataGrid1.Cells[ColIndex, RowIndex]);
        end;
      end;

      for ColorIndex := Low(TGraphSymbol) to High(TGraphSymbol) do
      begin
        AStringList.Add(IntToStr(Colors[ColorIndex]));
      end;

      SaveFont(AStringList, TextFont);
      SaveFont(AStringList, PrintFont);
      SaveFont(AStringList, TitleFont);
      SaveFont(AStringList, PrintTitleFont);

      // version 3
      AStringList.Add(IntToStr(Legend.Count));
      for LegendIndex := 0 to Legend.Count - 1 do
      begin
        AStringList.Add(Legend[LegendIndex]);
        LegendObject := Legend.Objects[LegendIndex] as TLegendObject;
        AStringList.Add(IntToStr(LegendObject.Color));
        AStringList.Add(IntToStr(Ord(LegendObject.Symbol)));
      end;

      AStringList.SaveToFile(SaveDialog1.FileName);
    finally
      AStringList.Free;
      Modified := False;
    end;

  end;
end;

procedure TfrmPiperGraph.sbOpenFileClick(Sender: TObject);
var
  AnInteger: integer;
  Version: string;
  RowIndex, ColIndex: integer;
  AString: string;
  ColorIndex: TGraphSymbol;
  LegendCount: integer;
  LegendIndex: integer;
  LegendObject: TLegendObject;
  LegendString: string;
begin
  if OpenDialog1.Execute then
  begin
    AssignFile(AFile, OpenDialog1.FileName);
    Reset(AFile);
    try
      ReadLn(AFile, Version);

      Read(AFile, AnInteger);
      Width := AnInteger;
      Read(AFile, AnInteger);
      Height := AnInteger;

      Read(AFile, AnInteger);
      seNumPoints.Value := AnInteger;
      seNumPointsChange(seNumPoints);

      Read(AFile, AnInteger);
      rgDataType.ItemIndex := AnInteger;

      Read(AFile, AnInteger);
      ColorSelection := TColorSelection(AnInteger);

      Read(AFile, AnInteger);
      if AnInteger = 1 then
      begin
        GridLines := True;
      end
      else
      begin
        GridLines := False;
      end;

      if StrToInt(Version) > 1 then
      begin
        Read(AFile, GridLineCount);
      end;

      Read(AFile, Margin);
      if Version <> '0' then
      begin
        Read(AFile, InteriorMargin);
      end;
      Read(AFile, LineThickness);
      ReadLn(AFile);
      Read(AFile, GraphTitle);
      Read(AFile, SymbolRadius);

      if Version <> '0' then
      begin
        Read(AFile, AnInteger);
        if AnInteger = 1 then
        begin
          ShowNumbers := True;
        end
        else
        begin
          ShowNumbers := False;
        end;
        Read(AFile, AnInteger);
        if AnInteger = 1 then
        begin
          LinkRadius := True;
        end
        else
        begin
          LinkRadius := False;
        end;
        Read(AFile, LabelTextSize);
        Read(AFile, LegendX);
        Read(AFile, LegendY);
        Read(AFile, MinSymbolSize);
        Read(AFile, MaxSymbolSize);
        Read(AFile, AnInteger);
        rgOrientation.ItemIndex := AnInteger
      end;

      ReadLn(AFile);
      for RowIndex := 1 to DataGrid1.RowCount - 1 do
      begin
        for ColIndex := 0 to DataGrid1.ColCount - 1 do
        begin
          if (Version <> '0') or ((ColIndex <> Ord(CO3)) and (ColIndex <>
            Ord(K))) then
          begin
            ReadLn(AFile, AString);
          end
          else
          begin
            AString := '0'
          end;
          DataGrid1.Cells[ColIndex, RowIndex] := AString;
        end;
      end;

      for ColorIndex := Low(TGraphSymbol) to High(TGraphSymbol) do
      begin
        Read(AFile, AnInteger);
        Colors[ColorIndex] := AnInteger;
      end;

      LoadFont(TextFont);
      LoadFont(PrintFont);
      LoadFont(TitleFont);
      LoadFont(PrintTitleFont);

      if StrToInt(Version) >= 3 then
      begin
        Read(AFile, LegendCount);
        for LegendIndex := 0 to LegendCount-1 do
        begin
          ReadLn(AFile);
          Read(AFile, LegendString);
          LegendObject := TLegendObject.Create;
          Legend.AddObject(LegendString, LegendObject);
          Read(AFile, AnInteger);
          LegendObject.Color := AnInteger;
          Read(AFile, AnInteger);
          LegendObject.Symbol := TGraphSymbol(AnInteger);
        end;
      end;

      SetValues;
    finally
      CloseFile(AFile);
      Modified := False;
    end;

  end;
end;

procedure TfrmPiperGraph.Button2Click(Sender: TObject);
var
  AStringList: TStringList;
  LineIndex: integer;
  AString: string;
  TabPos: integer;
  ColIndex: integer;
  CellString: string;
  procedure SetSymbol(CellString: string);
  var
    PickList: TStringList;
    PickIndex: integer;
  begin
    PickList := TStringList.Create;
    try
      PickList.Assign(DataGrid1.Columns[SymbolIndex].PickList);
      PickList.Text := UpperCase(PickList.Text);
      PickIndex := PickList.IndexOf(UpperCase(CellString));
      if PickIndex > -1 then
      begin
        CellString := DataGrid1.Columns[SymbolIndex].PickList[PickIndex];
        DataGrid1.Cells[ColIndex, LineIndex + 1] := CellString;
        DataGrid1SetEditText(DataGrid1, ColIndex, LineIndex + 1, CellString);
      end;
    finally
      PickList.Free;
    end;
  end;
  procedure SetColor(CellString: string);
  begin
    if ColorSelection <> csLinked then
    begin
      try
        StrToInt(CellString);
        DataGrid1.Cells[ColIndex, LineIndex + 1] := CellString;
        DataGrid1SetEditText(DataGrid1, ColIndex, LineIndex + 1, CellString);
      except on EConvertError do
        begin
        end;
      end;
    end;
  end;
  procedure SetValue(CellString: string);
  begin
    try
      InternationalStrToFloat(CellString);
      DataGrid1.Cells[ColIndex, LineIndex + 1] := CellString;
      DataGrid1SetEditText(DataGrid1, ColIndex, LineIndex + 1, CellString);
    except on EConvertError do
      begin
      end;
    end;
  end;
  procedure SetCell(CellString: string);
  begin
    case ColIndex of
      0..(SymbolIndex - 1):
        begin
          SetValue(CellString);
        end;
      SymbolIndex:
        begin
          SetSymbol(CellString);
        end;
      ColorIndex:
        begin
          SetColor(CellString);
        end;
    end;
  end;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := ClipBoard.AsText;
    seNumPoints.Value := AStringList.Count;
    seNumPointsChange(seNumPoints);
    for LineIndex := 0 to AStringList.Count - 1 do
    begin
      AString := AStringList[LineIndex];
      TabPos := Pos(#9, AString);
      ColIndex := 0;
      while (TabPos > 0) and (ColIndex < DataGrid1.ColCount) do
      begin
        CellString := Copy(AString, 1, TabPos - 1);
        SetCell(CellString);

        AString := Copy(AString, TabPos + 1, Length(AString));
        Inc(ColIndex);
        TabPos := Pos(#9, AString);

        if (TabPos = 0) and (AString <> '') and (ColIndex < DataGrid1.ColCount)
          then
        begin
          CellString := AString;
          SetCell(CellString);
        end;
      end;
    end;

  finally
    AStringList.Free;
    Modified := True;
  end;

end;

procedure TfrmPiperGraph.sbSaveImageClick(Sender: TObject);
var
  Wmf: TMetafile;
  WmfCanvas: TMetafileCanvas;
  Bmp: TBitMap;
begin
  if SaveDialog2.Execute then
  begin
    if LowerCase(ExtractFileExt(SaveDialog2.FileName)) = '.bmp' then
    begin
      Bmp := TBitMap.Create;
      try
        Bmp.PixelFormat := pf32bit;
        Bmp.Height := PaintBox1.ClientHeight;
        Bmp.Width := PaintBox1.ClientWidth;
        PaintGraph(Bmp.Width, Bmp.Height, Bmp.Canvas, False, True);
        Bmp.SaveToFile(SaveDialog2.FileName);
      finally
        Bmp.Free;
      end;
    end
    else
    begin
      Wmf := TMetafile.Create;
      try
        WmfCanvas := TMetafileCanvas.CreateWithComment(Wmf, 0, 'PiperGraph',
          'Piper plot of chemical data');
        try
          if LowerCase(ExtractFileExt(SaveDialog2.FileName)) = '.wmf' then
          begin
            Wmf.Enhanced := False;
          end
          else
          begin
            Wmf.Enhanced := True;
          end;
          Wait := True;
          try
            PaintGraph(PaintBox1.ClientWidth, PaintBox1.ClientHeight, WmfCanvas,
              False, True);
          finally
            Wait := False;
          end;
        finally
          WmfCanvas.Free;
        end;
        Wmf.SaveToFile(SaveDialog2.FileName);
      finally
        Wmf.Free;
      end;
    end;
    PaintBox1.Invalidate;
  end;

end;

procedure TfrmPiperGraph.rgDataTypeClick(Sender: TObject);
begin
  DataGrid1.Invalidate;
  SetValues;
  GetTDSRAnge;
  Modified := True;
  if rgDataType.ItemIndex < 2 then
  begin
    DataGrid1.Columns[Ord(CO3)].ButtonStyle := cbsAuto
  end
  else
  begin
    DataGrid1.Columns[Ord(CO3)].ButtonStyle := cbsEllipsis
  end;
end;

procedure TfrmPiperGraph.ClearLegend;
var
  Index: integer;
begin
  for Index := 0 to Legend.Count - 1 do
  begin
    Legend.Objects[Index].Free;
  end;
  Legend.Clear;
end;

procedure TfrmPiperGraph.SetGridLines(AValue: boolean);
begin
  FGridLines := AValue;
  PaintBox1.Invalidate;
end;

procedure TfrmPiperGraph.SetColorSelection(AValue: TColorSelection);
var
  LineIndex: integer;
  LocalColorIndex: integer;
  ColorSymbolIndex: TGraphSymbol;
  AColor: TColor;
  ALegendObject: TLegendObject;
begin
  FColorSelection := AValue;
  if FColorSelection = csLinked then
  begin
    with DataGrid1 do
    begin
      for LineIndex := 1 to RowCount - 1 do
      begin
        LocalColorIndex :=
          Columns[SymbolIndex].Picklist.IndexOf(Cells[SymbolIndex, LineIndex]);
        ColorSymbolIndex := TGraphSymbol(LocalColorIndex);
        AColor := Colors[ColorSymbolIndex];
        Cells[ColorIndex, LineIndex] := IntToStr(AColor);
      end;
      Invalidate;
    end;
    with frmPiperFormat.dgLegend do
    begin
      for LineIndex := 1 to RowCount - 1 do
      begin
        LocalColorIndex := Columns[1].Picklist.IndexOf(Cells[1, LineIndex]);
        ColorSymbolIndex := TGraphSymbol(LocalColorIndex);
        AColor := Colors[ColorSymbolIndex];
        Cells[0, LineIndex] := IntToStr(AColor);
      end;
      for LineIndex := 0 to Legend.Count - 1 do
      begin
        ALegendObject := Legend.Objects[LineIndex] as TLegendObject;
        ColorSymbolIndex := ALegendObject.Symbol;
        AColor := Colors[ColorSymbolIndex];
        ALegendObject.Color := AColor;
      end;
    end;
  end;
end;

procedure TfrmPiperGraph.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);

end;

procedure TfrmPiperGraph.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  UserResponse: word;
begin
  if Visible then
  begin
    UserResponse := mrNo;
    if Modified then
    begin
      UserResponse :=
        MessageDlg('Do you want to save your data before closing?',
        mtInformation, [mbYes, mbNo, mbCancel], 0);
    end;
    if UserResponse = mrYes then
    begin
      sbSaveFileClick(Sender);
    end
    else if UserResponse = mrCancel then
    begin
      Action := caNone;
    end
    else
    begin
      frmModChart.Close
    end;
  end;
end;

procedure TfrmPiperGraph.DataGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (DataGrid1.Row < DataGrid1.RowCount - 1) then
  begin
    DataGrid1.Row := DataGrid1.Row + 1;
  end;
end;

procedure TfrmPiperGraph.GetTDSRAnge;
var
  Index: Integer;
  start: integer;
begin
  start := -1;
  for Index := 0 to seNumPoints.Value - 1 do
  begin
    if OkArray[Index] then
    begin
      start := Index;
      break;
    end;
  end;

  if (start > -1) then
  begin
    MinTDS := Values[TDS, start];
    MaxTDS := Values[TDS, start];
    for Index := start + 1 to seNumPoints.Value - 1 do
    begin
      if OkArray[Index] then
      begin
        MinTDS := Min(MinTDS, Values[TDS, Index]);
        MaxTDS := Max(MaxTDS, Values[TDS, Index]);
      end;
    end;
  end
  else
  begin
    MinTDS := 0;
    MaxTDS := 1;
  end;
end;

procedure TfrmPiperGraph.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelectLegend(X, Y);
  if LegendSelected then
  begin
    LegendOffsetX := X - LegendX;
    LegendOffSetY := Y - LegendY;
  end;
  PaintBox1.Invalidate;
end;

procedure TfrmPiperGraph.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if LegendSelected then
  begin
    LegendX := X - LegendOffsetX;
    LegendY := Y - LegendOffSetY;
    PaintBox1.Invalidate;
  end;
end;

procedure TfrmPiperGraph.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LegendSelected := False;
end;

procedure TfrmPiperGraph.CalculateVisible;
var
  PrintWidth, PrintHeight: integer;
  PrintRatio, ScreenRatio: double;
  Width: integer;
begin
  GetDimensions(PaintBox1.ClientWidth, PaintBox1.ClientHeight,
    PaintBox1.Canvas, False);
  PrintWidth := Printer.PageWidth;
  PrintHeight := Printer.PageHeight;
  if (PrintWidth = 0) or (PaintBox1.ClientWidth = 0)
    or (PaintBox1.ClientHeight = 0) then
    Exit;
  PrintRatio := PrintHeight / PrintWidth;
  ScreenRatio := PaintBox1.ClientHeight / PaintBox1.ClientWidth;
  if PrintRatio < ScreenRatio then
  begin
    shpLeft.Align := alTop;
    shpRight.Align := alBottom;
    Width := Round(PaintBox1.ClientHeight - PaintBox1.ClientWidth * PrintRatio);
    shpLeft.Height := 0;
    shpRight.Height := Width;
  end
  else
  begin
    shpLeft.Align := alLeft;
    shpRight.Align := alRight;
    Width := Round((PaintBox1.ClientWidth - PaintBox1.ClientHeight / PrintRatio)
      / 2);
    shpLeft.Width := Width;
    shpRight.Width := Width;
  end;

end;

procedure TfrmPiperGraph.cbPrintPreviewClick(Sender: TObject);
begin
  shpLeft.Visible := False;
  shpRight.Visible := False;
  CalculateVisible;
  shpLeft.Visible := (shpLeft.Align = alLeft);
  shpRight.Visible := True;
end;

procedure TfrmPiperGraph.tabPlotResize(Sender: TObject);
begin
  if not Wait then
  begin
    cbPrintPreviewClick(Sender);
  end;
end;

procedure TfrmPiperGraph.rgOrientationClick(Sender: TObject);
begin
  Printer.Orientation := TPrinterOrientation(rgOrientation.ItemIndex);
  tabPlotResize(Sender);
  Modified := True;
end;

{ TLegendObject }

function TLegendObject.Height(Text: string; Canvas: TCanvas;
  RadiusUsed, VerticalSeparation: integer; var
  TextHeight: integer): integer;
begin
  TextHeight := Canvas.TextHeight(Text);
  result := Max(RadiusUsed * 2, TextHeight - 2) + VerticalSeparation;
end;

procedure TLegendObject.Draw(Text: string; Canvas: TCanvas;
  RadiusUsed: integer; AColor: TColor; HorizontalSeparation,
  VerticalSeparation, X: integer; var Y: integer);
var
  TextHeight, ItemHeight, TextX, TextY: integer;
  APoint: TPoint;
begin
  Rect.Top := Y;
  Rect.Left := X - 4;
  ItemHeight := Height(Text, Canvas, RadiusUsed, VerticalSeparation,
    TextHeight);
  APoint.x := X + RadiusUsed;
  APoint.y := Y + ItemHeight div 2;
  DrawSymbol(Canvas, RadiusUsed, APoint, AColor);
  TextX := X + RadiusUsed * 2 + HorizontalSeparation;
  TextY := APoint.y - (TextHeight div 2);
  Canvas.TextOut(TextX, TextY, Text);
  Rect.Right := TextX + Canvas.TextWidth(Text) + 4;
  Y := Y + ItemHeight;
  Rect.Bottom := Y;
end;

procedure TLegendObject.DrawSymbol(Canvas: TCanvas; RadiusUsed: integer; APoint:
  TPoint; AColor: TColor);
begin
  case Symbol of
    gsCircle:
      begin
        frmPiperGraph.DrawCircle(Canvas, RadiusUsed, APoint, AColor, False);
      end;
    gsCircleOpen:
      begin
        frmPiperGraph.DrawCircle(Canvas, RadiusUsed, APoint, AColor, True);
      end;
    gsSquare:
      begin
        frmPiperGraph.DrawSquare(Canvas, RadiusUsed, APoint, AColor, False);
      end;
    gsSquareOpen:
      begin
        frmPiperGraph.DrawSquare(Canvas, RadiusUsed, APoint, AColor, True);
      end;
    gsTriangle:
      begin
        frmPiperGraph.Triangle(Canvas, RadiusUsed, APoint, AColor, False);
      end;
    gsTriangleOpen:
      begin
        frmPiperGraph.Triangle(Canvas, RadiusUsed, APoint, AColor, True);
      end;
    gsInvertedTriangle:
      begin
        frmPiperGraph.InvertedTriangle(Canvas, RadiusUsed, APoint, AColor,
          False);
      end;
    gsInvertedTriangleOpen:
      begin
        frmPiperGraph.InvertedTriangle(Canvas, RadiusUsed, APoint, AColor,
          True);
      end;
    gsStar:
      begin
        frmPiperGraph.Star(Canvas, RadiusUsed, APoint, AColor, False);
      end;
    gsStarOpen:
      begin
        frmPiperGraph.Star(Canvas, RadiusUsed, APoint, AColor, True);
      end;
    gsCross:
      begin
        frmPiperGraph.Cross(Canvas, RadiusUsed, APoint, AColor);
      end;
    gsX:
      begin
        frmPiperGraph.XCross(Canvas, RadiusUsed, APoint, AColor);
      end;
  else
    ;
    Assert(False);
  end;

end;

procedure TfrmPiperGraph.Help2Click(Sender: TObject);
begin
  Application.HelpContext(PageControl1.ActivePage.HelpContext);
end;

procedure TfrmPiperGraph.About1Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmPiperGraph.Exit1Click(Sender: TObject);
begin
  inherited;
  Close;
end;

end.

