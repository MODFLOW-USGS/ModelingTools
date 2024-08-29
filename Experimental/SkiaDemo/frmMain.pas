unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Skia.Canvas, FMX.StdCtrls, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.Edit, FMX.Objects,
  Mf6.SimulationNameFileReaderUnit, System.Generics.Collections, FMX.Menus;

type
  // @name indicates whether exaggeration is applied in
  // a vertical or horizontal direction.
  // See TQRbwZoomBox2.@link(TQRbwZoomBox2.ExaggerationDirection).
  TExaggerationDirection = (edVertical, edHorizontal);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.VerticalDirection).}
  TVerticalDirection = (vdDown, vdUp);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.HorizontalDirection).}
  THorizontalDirection = (hdRight, hdLeft);

  TGridLimit = record
    MinX: double;
    MaxX: double;
    MinY: double;
    MaxY: double;
    MinZ: double;
    MaxZ: double;
  end;

  TCellList = TList<ISkPath>;

  TMyThread = class(TThread)
  private
    FCellList: TCellList;
  public
    procedure Execute; override;
    constructor Create(CellList: TCellList);
  end;

  TForm2 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPath: ISkPath;
    FPath2: ISkPath;
    FGrid: ISkPath;
    FMf6Simulation: TMf6Simulation;
    FOriginX: double;
    FOriginY: Double;
    FMagnification: Double;
    FHorizontalDirection: THorizontalDirection;
    FVerticalDirection: TVerticalDirection;
    FExaggerationDirection: TExaggerationDirection;
    FExaggeration: Double;
    FCellList: TCellList;
    MinK: double;
    MaxK: double;
    KxValues: TDoubleDynArray;
    FThreadCount: Integer;
    FRotationAngle: Double;
    procedure SetMagnification(const Value: double);
    procedure SetOriginX(const Value: double);
    procedure SetOriginY(const Value: double);
    procedure SetExaggerationDirection(const Value: TExaggerationDirection);
    procedure SetHorizontalDirection(const Value: THorizontalDirection);
    procedure SetVerticalDirection(const Value: TVerticalDirection);
    procedure SetExaggeration(const Value: Double);
    procedure SetTopPosition(const XCoordinate, YCoordinate: Double);
    function Color2AlphaColor(AColor: TColor): TAlphaColor;
    procedure DecrementThreadCount(Sender: TObject);
    { Private declarations }
  public
    {X converts a screen coordinate into a real-number X coordinate.}
    function X(XCoord: integer): extended;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    function XCoord(X: extended): integer;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function Y(YCoord: integer): extended;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    function YCoord(Y: extended): integer;
    // @name is the ratio of distances in screen coordinates to
    // corresponding distances in real-world coordinates.
    // Usually @name is not set directly. See @link(BeginZoom),
    // @link(FinishZoom), @link(ZoomBy) and @link(ZoomByAt).
    property Magnification: double read FMagnification write SetMagnification;
    // @name is the screen X-coordinate that corresponds to a
    // real-world X-coordinate of zero.
    property OriginX: double read FOriginX write SetOriginX;
    // @name is the screen Y-coordinate that corresponds to a
    // real-world Y-coordinate of zero.
    property OriginY: double read FOriginY write SetOriginY;
    // @name is used to allow either vertical or horizontal exaggeration.
    // @name affects how screen coordinates map to real-world coordinates.
    // See @link(ExaggerationDirection).
    property Exaggeration: Double read FExaggeration write SetExaggeration;
    // @name controls the direction in which @link(Exaggeration) is applied.
    property ExaggerationDirection: TExaggerationDirection
      read FExaggerationDirection write SetExaggerationDirection default
      edVertical;
    { @name indicates the direction in which the X-coordinates increase.
      hdRight indicates they increase towards the right,
      hdLeft indicates they increase towards the left.}
    property HorizontalDirection: THorizontalDirection
      read FHorizontalDirection write SetHorizontalDirection;
    { @name indicates the direction in which the Y-coordinates increase.
      vdDown indicates they increase downward,
      vdUp indicates they increase upward.}
    property VerticalDirection: TVerticalDirection read FVerticalDirection write
      SetVerticalDirection;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Mf6.NameFileReaderUnit, Mf6.CustomMf6PersistentUnit, Mf6.DisFileReaderUnit,
  System.Math, Mf6.NpfFileReaderUnit, ColorSchemes;

{$R *.fmx}

procedure TForm2.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FGrid := nil;
//    edSimulationNameFile.Text := OpenDialog1.FileName;
    FMf6Simulation.Free;
    FMf6Simulation := TMf6Simulation.Create('Simulation');
    FMf6Simulation.ReadSimulation(OpenDialog1.FileName);
    SkPaintBox1.DrawCacheKind := TSkDrawCacheKind.Never;
  end;
end;

function TForm2.Color2AlphaColor(AColor: TColor): TAlphaColor;
//var
//  C1: Integer;
//  C2: Integer;
//  C3: Integer;
begin
//  C1 := ($FF0000 and AColor) shr 16;
//  C2 := ($FF00 and AColor);
//  C3 := ($FF0000 and AColor) shl 16;
//  result := $FF000000 or C1 or C2 or C3;
  result := $FF000000 or AColor;
end;

procedure TForm2.DecrementThreadCount(Sender: TObject);
begin
  AtomicDecrement(FThreadCount);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FOriginX := 0;
  FOriginY := 0;
  FMagnification := 1;
  FExaggeration := 1;
  FVerticalDirection := vdUp;
  FCellList := TCellList.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  while FThreadCount > 0 do
  begin
    Sleep(1000)
  end;
  FCellList.Free;
  FMf6Simulation.Free;
end;

procedure TForm2.SetExaggeration(const Value: Double);
begin
  FExaggeration := Value;
end;

procedure TForm2.SetExaggerationDirection(const Value: TExaggerationDirection);
begin
  FExaggerationDirection := Value;
end;

procedure TForm2.SetHorizontalDirection(const Value: THorizontalDirection);
begin
  FHorizontalDirection := Value;
end;

procedure TForm2.SetMagnification(const Value: double);
begin
  FMagnification := Value;
end;

procedure TForm2.SetOriginX(const Value: double);
begin
  FOriginX := Value;
end;

procedure TForm2.SetOriginY(const Value: double);
begin
  FOriginY := Value;
end;

procedure TForm2.SetTopPosition(const XCoordinate, YCoordinate: Double);
var
  DeltaX, DeltaY: double;
begin
  DeltaX := (X(Round(SkPaintBox1.Width)) - X(0)) / 2;
  DeltaY := (Y(0) - Y(Round(SkPaintBox1.Height))) / 2;
  OriginX := XCoordinate - DeltaX;
  OriginY := YCoordinate - DeltaY;

end;

procedure TForm2.SetVerticalDirection(const Value: TVerticalDirection);
begin
  FVerticalDirection := Value;
end;

procedure TForm2.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
const
  scale = 256.0;
  R = 0.45 * scale;
  TAU = 6.2831853;
var
  PathBuilder: ISkPathBuilder;
  theta: single;
  LPaint: ISkPaint;
  AModel: TModel;
  Packages: TFlowPackages;
  APackage: TPackage;
  DisPackage: TDis;
  GridData: TDisGridData;
  GridLimit: TGridLimit;
  ModelXWidth, ModelYWidth, ModelHeight: double;
  MinY, MaxY, MinX, MaxX: double;
  ColumnPosition: Double;
  ColX: Integer;
  ColTop: Integer;
  ColBottom: Integer;
  RowStart: Integer;
  RowEnd: Integer;
  RowPosition: Double;
  RowY: Integer;
  NpfPackage: TNpf;
  ColumnPositions: TIntegerDynArray;
  RowPositions: TIntegerDynArray;
  Kx: TDArray3D;
  ACellPath: ISkPath;
  Fraction: Double;
  Range: double;
  MyThread: TMyThread;
begin
  SkPaintBox1.DrawCacheKind := TSkDrawCacheKind.Raster;

  PathBuilder := TSkPathBuilder.Create;
  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);


  if not Assigned(FPath) then
  begin
    PathBuilder.moveTo(R, 0.0);
    for var i := 1 to 7 do
    begin
      theta := 3 * i * TAU / 7;
      PathBuilder.lineTo(R * cos(theta), R * sin(theta));
    end;
    FPath := PathBuilder.Detach;
  end;

  if not Assigned(FPath2) then
  begin
    PathBuilder.moveTo(R, 0.0);
    for var i := 1 to 5 do
    begin
      theta := 3 * i * TAU / 7;
      PathBuilder.lineTo(R * cos(theta), R * sin(theta));
    end;
    FPath2 := PathBuilder.Detach;
  end;

  ACanvas.clear(TAlphaColorRec.White);
//  ACanvas.translate(0.5 * scale, 0.75 * scale);
//  // no line
////  LPaint.Color := 0;
//  // blue line
//  LPaint.Color := TAlphaColorRec.Blue;
//  // thin line
////  LPaint.StrokeWidth := 2;
//  // thick line.
//  LPaint.StrokeWidth := 4;
//
//  ACanvas.DrawPath(FPath, LPaint);
//
//  LPaint.Style := TSkPaintStyle.Fill;
//  LPaint.Color := TAlphaColorRec.Darkgoldenrod;
//  ACanvas.DrawPath(FPath, LPaint);
//
//
//  LPaint.Style := TSkPaintStyle.Stroke;
//  LPaint.Color := TAlphaColorRec.Blue;
//
//  ACanvas.translate(0.5 * scale, 0.75 * scale);
//  ACanvas.DrawPath(FPath2, LPaint);
//
//  LPaint.Style := TSkPaintStyle.Fill;
//  LPaint.Color := TAlphaColorRec.Darkgoldenrod;
//  ACanvas.DrawPath(FPath2, LPaint);


  DisPackage := nil;
  NpfPackage := nil;
  if (not Assigned(FGrid)) and (FMf6Simulation <> nil) then
  begin
    for var ModelIndex := 0 to FMf6Simulation.Models.Count - 1 do
    begin
      AModel := FMf6Simulation.Models[ModelIndex];
      if AnsiSameText(AModel.ModelType, 'GWF6') then
      begin
        Packages :=  (AModel.FName as TFlowNameFile).NfPackages as TFlowPackages;
        for var PackageIndex := 0 to Packages.Count - 1 do
        begin
          APackage := Packages[PackageIndex];
          if AnsiSameText(APackage.FileType, 'DIS6') then
          begin
            DisPackage := APackage.Package as TDis;
            break;
          end;
        end;

        if DisPackage <> nil then
        begin
          FRotationAngle := -DisPackage.Options.ANGROT;
        
          for var PackageIndex := 0 to Packages.Count - 1 do
          begin
            APackage := Packages[PackageIndex];
            if AnsiSameText(APackage.FileType, 'NPF6') then
            begin
              NpfPackage := APackage.Package as TNpf;
              break;
            end;
          end;

        end;
        break;
      end;
    end;

    if DisPackage <> nil then
    begin
      GridData := DisPackage.GridData;
      GridLimit.MinX := DisPackage.Options.XORIGIN;
      GridLimit.MaxX := DisPackage.Options.XORIGIN;
      for var ColIndex := 0 to Length(GridData.DELR) - 1 do
      begin
        GridLimit.MaxX := GridLimit.MaxX + GridData.DELR[ColIndex]
      end;
      GridLimit.MinY:= DisPackage.Options.YORIGIN;
      GridLimit.MaxY := DisPackage.Options.YORIGIN;
      for var RowIndex := 0 to Length(GridData.DELC) - 1 do
      begin
        GridLimit.MaxY := GridLimit.MaxY + GridData.DELC[RowIndex]
      end;
      ModelXWidth := GridLimit.MaxX - GridLimit.MinX;
      ModelYWidth := GridLimit.MaxY - GridLimit.MinY;
      Magnification := 0.9 *
        Min(SkPaintBox1.Width / ModelXWidth,
        SkPaintBox1.Height / ModelYWidth);
      SetTopPosition((GridLimit.MinX + GridLimit.MaxX)/2,
        (GridLimit.MinY + GridLimit.MaxY)/2);

      SetLength(ColumnPositions, Length(GridData.DELR)+1);

      ColTop  := YCoord(GridLimit.MaxY);
      ColBottom  := YCoord(GridLimit.MinY);
      ColumnPosition := GridLimit.MinX;
      ColX := XCoord(ColumnPosition);
      ColumnPositions[0] := ColX;
      PathBuilder.moveTo(ColX, ColBottom);
        PathBuilder.lineTo(ColX, ColTop);
      for var ColIndex := 0 to Length(GridData.DELR) - 1 do
      begin
        ColumnPosition := ColumnPosition + GridData.DELR[ColIndex];
        ColX := XCoord(ColumnPosition);
        ColumnPositions[ColIndex+1] := ColX;

        PathBuilder.moveTo(ColX, ColBottom);
          PathBuilder.lineTo(ColX, ColTop);
      end;

      SetLength(RowPositions, Length(GridData.DELC)+1);

      RowStart := XCoord(GridLimit.MinX);
      RowEnd := XCoord(GridLimit.MaxX);
      RowPosition := GridLimit.MaxY;
      RowY := YCoord(RowPosition);
      RowPositions[0] := RowY;
      PathBuilder.moveTo(RowStart, RowY);
        PathBuilder.lineTo(RowEnd, RowY);

      for var RowIndex := 0 to Length(GridData.DELC) - 1 do
      begin
        RowPosition := RowPosition - GridData.DELC[RowIndex];
        RowY := YCoord(RowPosition);
        RowPositions[RowIndex+1] := RowY;

        PathBuilder.moveTo(RowStart, RowY);
          PathBuilder.lineTo(RowEnd, RowY);
      end;

      FGrid := PathBuilder.Detach;

      if NpfPackage <> nil then
      begin

        Inc(FThreadCount);
        MyThread := TMyThread.Create(FCellList);
        MyThread.OnTerminate := DecrementThreadCount;
        MyThread.Start;
        FCellList := TCellList.Create;

        for var RowIndex := 0 to Length(RowPositions) - 2 do
        begin
          for var ColIndex := 0 to Length(ColumnPositions) - 2 do
          begin
            PathBuilder.moveTo(ColumnPositions[ColIndex], RowPositions[RowIndex]);
            PathBuilder.LineTo(ColumnPositions[ColIndex+1], RowPositions[RowIndex]);
            PathBuilder.LineTo(ColumnPositions[ColIndex+1], RowPositions[RowIndex+1]);
            PathBuilder.LineTo(ColumnPositions[ColIndex], RowPositions[RowIndex+1]);
            PathBuilder.LineTo(ColumnPositions[ColIndex], RowPositions[RowIndex]);
            FCellList.Add(PathBuilder.Detach);
          end;
        end;

        Kx := NpfPackage.GridData.K;
        if Kx <> nil then
        begin
          Assert(Length(Kx) > 0);
          Assert(Length(Kx[0]) = Length(RowPositions)-1);
          Assert(Length(Kx[0,0]) = Length(ColumnPositions)-1);
          MinK := Kx[0,0,0];
          MaxK := MinK;
          SetLength(KxValues, (Length(RowPositions)-1) * (Length(ColumnPositions)-1));
          var CellIndex: Integer := 0;
          for var RowIndex := 0 to Length(Kx[0]) - 1 do
          begin
            for var ColIndex := 0 to Length(Kx[0,0]) - 1 do
            begin
              KxValues[CellIndex] := Kx[0,RowIndex,ColIndex];
              Inc(CellIndex);
              if Kx[0,RowIndex,ColIndex] < MinK then
              begin
                MinK := Kx[0,RowIndex,ColIndex]
              end;
              if Kx[0,RowIndex,ColIndex] > MaxK then
              begin
                MaxK := Kx[0,RowIndex,ColIndex]
              end;
            end;
          end;
        end;
      end;
    end;
  end;
          
  ACanvas.Translate(SkPaintBox1.Width /4, SkPaintBox1.Height/4);
  ACanvas.Scale(0.5,0.5);
  ACanvas.Rotate(FRotationAngle, SkPaintBox1.Width /2, SkPaintBox1.Height/2);
  
  if Assigned(FGrid) then
  begin
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.Color := TAlphaColorRec.Black;
    LPaint.StrokeWidth := 1;

//    ACanvas.translate(-1 * scale, -1.5 * scale);
    ACanvas.DrawPath(FGrid, LPaint);

    Range := MaxK - MinK;
    for var CellIndex := 0 to FCellList.Count - 1 do
    begin
      ACellPath := FCellList[CellIndex];
      if Range = 0 then
      begin
        Fraction := 0.5;
      end
      else
      begin
        Fraction :=   (KxValues[CellIndex] - MinK)/Range;
      end;
      LPaint.Style := TSkPaintStyle.Fill;
      LPaint.Color := Color2AlphaColor(FracAndSchemeToColor(0, Fraction, 0.6, 1)); 
      ACanvas.DrawPath(ACellPath, LPaint);
      
//      LPaint.Style := TSkPaintStyle.Stroke;
//      LPaint.Color := TAlphaColorRec.Black; 
//      ACanvas.DrawPath(ACellPath, LPaint);
    end;
  end;
end;

function TForm2.X(XCoord: integer): extended;
begin
  if HorizontalDirection = hdLeft then
  begin
    XCoord := Round(SkPaintBox1.Width) - XCoord;
  end;
  if ExaggerationDirection = edHorizontal then
  begin
    result := XCoord / Magnification / FExaggeration + OriginX;
  end
  else
  begin
    result := XCoord / Magnification + OriginX;
  end;
end;

function TForm2.XCoord(X: extended): integer;
var
  temp: extended;
begin
  try
    if ExaggerationDirection = edHorizontal then
    begin
      temp := (X - OriginX) * FExaggeration * Magnification;
    end
    else
    begin
      temp := (X - OriginX) * Magnification;
    end;
    if HorizontalDirection = hdLeft then
    begin
      temp := ClientWidth - temp;
    end;
    if temp > High(Integer) then
    begin
      result := High(Integer);
    end
    else if temp < Low(Integer) then
    begin
      result := Low(Integer);
    end
    else
    begin
      result := Round(temp);
    end;
  except
    on EOverflow do
    begin
      if X > OriginX then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
    on EInvalidOp do
    begin
      if X > OriginX then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
  end;
end;

function TForm2.Y(YCoord: integer): extended;
begin
  if VerticalDirection = vdUp then
  begin
    YCoord := Round(SkPaintBox1.Height) - YCoord;
  end;

  if ExaggerationDirection = edVertical then
  begin
    result := YCoord / Magnification / FExaggeration + OriginY;
  end
  else
  begin
    result := YCoord / Magnification + OriginY;
  end;
end;

function TForm2.YCoord(Y: extended): integer;
var
  temp: extended;
begin
  try
    if ExaggerationDirection = edVertical then
    begin
      temp := (Y - OriginY) * FExaggeration * Magnification
    end
    else
    begin
      temp := (Y - OriginY) * Magnification;
    end;
    if VerticalDirection = vdUp then
    begin
      temp := ClientHeight - temp;
    end;
    if temp > High(Integer) then
    begin
      result := High(Integer);
    end
    else if temp < Low(Integer) then
    begin
      result := Low(Integer);
    end
    else
    begin
      result := Round(temp);
    end;
  except on EOverflow do
    begin
      if Y > OriginY then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
    on EInvalidOp do
    begin
      if Y > OriginY then
      begin
        result := High(Integer);
      end
      else
      begin
        result := Low(Integer);
      end;
    end;
  end;
end;

{ TMyThread }

constructor TMyThread.Create(CellList: TCellList);
begin
  FCellList := CellList;
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TMyThread.Execute;
begin
  FCellList.Free;
  
end;

end.
