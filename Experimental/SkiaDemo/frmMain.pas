unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Skia.Canvas, FMX.StdCtrls, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.Edit, FMX.Objects, FastGEO,
  Mf6.SimulationNameFileReaderUnit, System.Generics.Collections, FMX.Menus,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.EditBox, FMX.NumberBox,
  DrawFrameUnit;

type

  TFGridLimit = record
    MinX: double;
    MaxX: double;
    MinY: double;
    MaxY: double;
    MinZ: double;
    MaxZ: double;
  end;

  TCellList = TList<ISkPath>;

  TForm2 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnMultiply: TButton;
    btnDivide: TButton;
    LabelX: TLabel;
    LabelY: TLabel;
    NumberBox1: TNumberBox;
    Memo1: TMemo;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnMultiplyClick(Sender: TObject);
    procedure btnDivideClick(Sender: TObject);
    procedure NumberBox1Change(Sender: TObject);
  private
    FMf6Simulation: TMf6Simulation;
    FGrid: ISkPath;
    FMagnification: Double;
//    FHorizontalDirection: THorizontalDirection;
    FVerticalDirection: TVerticalDirection;
    FExaggerationDirection: TExaggerationDirection;
    FExaggeration: Double;
    FCellList: TCellList;
    FMinK: double;
    FMaxK: double;
    FKxValues: TDoubleDynArray;
    FThreadCount: Integer;
    FRotationAngle: Double;
    FGridAngle: double;
    FModelXCenter, FModelYCenter: double;
    FDisplayMag: double;
    FStartMove: Boolean;
    FStartPoint: TPointF;
    FStopPoint: TPointF;
    FMove: TPointF;
    FGridLimit: TFGridLimit;
    ModelXCenter, ModelYCenter: double;
    procedure SetMagnification(const Value: double);
    procedure SetExaggerationDirection(const Value: TExaggerationDirection);
//    procedure SetHorizontalDirection(const Value: THorizontalDirection);
    procedure SetVerticalDirection(const Value: TVerticalDirection);
    procedure SetExaggeration(const Value: Double);
    procedure SetTopPosition(const XCoordinate, YCoordinate: Double);
    function Color2AlphaColor(AColor: TColor): TAlphaColor;
    procedure DecrementThreadCount(Sender: TObject);
    { Private declarations }
  public
    property Mf6Simulation: TMf6Simulation read FMf6Simulation;
    property GridAngle: double read FGridAngle;
    property GridLimit: TFGridLimit read FGridLimit write FGridLimit;
    {X converts a screen coordinate into a real-number X coordinate.}
    function X(XCoord: single): extended;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    function XCoord(X: extended): single;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function Y(YCoord: single): extended;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    function YCoord(Y: extended): single;
    function ScreenToReal(APoint: TPoint2D): TPoint2D;
    function RealToScreen(APoint: TPoint2D): TPoint2D;
    // @name is the ratio of distances in screen coordinates to
    // corresponding distances in real-world coordinates.
    // Usually @name is not set directly. See @link(BeginZoom),
    // @link(FinishZoom), @link(ZoomBy) and @link(ZoomByAt).
    property Magnification: double read FMagnification write SetMagnification;
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
//    property HorizontalDirection: THorizontalDirection
//      read FHorizontalDirection write SetHorizontalDirection;
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
  System.Math, Mf6.NpfFileReaderUnit, ColorSchemes, FreeThread;

{$R *.fmx}

procedure TForm2.btnOpenFileClick(Sender: TObject);
//var
//  MyThread: TMyThread;
begin
  if OpenDialog1.Execute then
  begin
    Cursor := crHourGlass;
    try
      FGrid := nil;

      FMf6Simulation.Free;

//      Inc(FThreadCount);
//      MyThread := TMyThread.Create(FMf6Simulation);
//      MyThread.OnTerminate := DecrementThreadCount;
//      MyThread.Start;

      FMf6Simulation := TMf6Simulation.Create('Simulation');
      FMf6Simulation.ReadSimulation(OpenDialog1.FileName);
      SkPaintBox1.DrawCacheKind := TSkDrawCacheKind.Never;
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TForm2.btnDivideClick(Sender: TObject);
begin
  Magnification := Magnification/2;
  SkPaintBox1.Redraw;
end;

procedure TForm2.btnMultiplyClick(Sender: TObject);
begin
  Magnification := Magnification*2;
  SkPaintBox1.Redraw;
end;

function TForm2.Color2AlphaColor(AColor: TColor): TAlphaColor;
begin
  if AColor < 0 then
  begin
    AColor := TColors.ColorToRGB(AColor)
  end;
  Assert(AColor >= 0);
  result := $FF000000 or AColor;
end;

procedure TForm2.DecrementThreadCount(Sender: TObject);
begin
  AtomicDecrement(FThreadCount);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FMagnification := 1;
  FExaggeration := 1;
  FVerticalDirection := vdUp;
  FCellList := TCellList.Create;
  FDisplayMag := 1;
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

procedure TForm2.NumberBox1Change(Sender: TObject);
begin
  if NumberBox1.Value > 0 then
  begin
    Exaggeration := NumberBox1.Value;
    SkPaintBox1.Redraw;
  end;
end;

function TForm2.RealToScreen(APoint: TPoint2D): TPoint2D;
begin
  result.x := XCoord(APoint.x);
  result.y := YCoord(APoint.y);
end;

function TForm2.ScreenToReal(APoint: TPoint2D): TPoint2D;
begin
  result.x := X(APoint.x);
  result.y := Y(APoint.y);
end;

procedure TForm2.SetExaggeration(const Value: Double);
begin
  FExaggeration := Value;
end;

procedure TForm2.SetExaggerationDirection(const Value: TExaggerationDirection);
begin
  FExaggerationDirection := Value;
end;

//procedure TForm2.SetHorizontalDirection(const Value: THorizontalDirection);
//begin
//  FHorizontalDirection := Value;
//end;

procedure TForm2.SetMagnification(const Value: double);
begin
  FMagnification := Value;
end;

procedure TForm2.SetTopPosition(const XCoordinate, YCoordinate: Double);
var
  DeltaX, DeltaY: double;
begin
  Exit;

  DeltaX := (X(Round(SkPaintBox1.Width)) - X(0)) / 2;
  DeltaY := (Y(0) - Y(Round(SkPaintBox1.Height))) / 2;

end;

procedure TForm2.SetVerticalDirection(const Value: TVerticalDirection);
begin
  FVerticalDirection := Value;
end;

procedure TForm2.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  PathBuilder: ISkPathBuilder;
  theta: single;
  LPaint: ISkPaint;
  AModel: TModel;
  Packages: TFlowPackages;
  APackage: TPackage;
  DisPackage: TDis;
  GridData: TDisGridData;
  ColumnPosition: Double;
  ColX: single;
  ColTop: single;
  ColBottom: single;
  RowStart: single;
  RowEnd: single;
  RowPosition: Double;
  RowY: single;
  NpfPackage: TNpf;
  ColumnPositions: TSingleDynArray;
  RowPositions: TSingleDynArray;
  Kx: TDArray3D;
  ACellPath: ISkPath;
  Fraction: Double;
  Range: double;
  MyThread: TMyThread;
  LocalMagnification: double;
  RowIndex, ColIndex: Integer;
  PathData: TPathData;
  APoint: TPoint2D;
  ModelXWidth, ModelYWidth {, ModelHeight}: double;
  MinY, MaxY, MinX, MaxX: double;
  FXOrigin: double;
  FYOrigin: double;
  function RotateFromGridCoordinatesToRealWorldCoordinates(
    const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
  begin
    result := APoint;
    if FGridAngle <> 0 then
    begin
      result.x := result.x - FXOrigin;
      result.y := result.y - FYOrigin;
      temp.X := Cos(-FGridAngle) * result.X - Sin(-FGridAngle) * result.Y;
      temp.Y := Sin(-FGridAngle) * result.X + Cos(-FGridAngle) * result.Y;
      result := temp;
      result.x := result.x + FXOrigin;
      result.y := result.y + FYOrigin;
    end;
    // Reverse Y direction because Skia y coordinate increases DOWNWARD
    // instead of UPWARD.
    result.y := -result.y;
  end;
begin
  SkPaintBox1.DrawCacheKind := TSkDrawCacheKind.Raster;

  PathBuilder := TSkPathBuilder.Create;
  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);

  ACanvas.clear(TAlphaColorRec.White);

  DisPackage := nil;
  NpfPackage := nil;
  if (not Assigned(FGrid)) and (FMf6Simulation <> nil) then
  begin
    Cursor := crHourGlass;
    try
      FMove.X := 0;
      FMove.Y := 0;
      memo1.Lines.Clear;
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
            FGridAngle := FRotationAngle/180*pi;

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
        FXOrigin := DisPackage.Options.XORIGIN;
        FYOrigin := DisPackage.Options.YORIGIN;
        FGridLimit.MinX := DisPackage.Options.XORIGIN;
        FGridLimit.MaxX := DisPackage.Options.XORIGIN;
        for ColIndex := 0 to Length(GridData.DELR) - 1 do
        begin
          FGridLimit.MaxX := FGridLimit.MaxX + GridData.DELR[ColIndex]
        end;
        FGridLimit.MinY:= DisPackage.Options.YORIGIN;
        FGridLimit.MaxY := DisPackage.Options.YORIGIN;
        for RowIndex := 0 to Length(GridData.DELC) - 1 do
        begin
          FGridLimit.MaxY := FGridLimit.MaxY + GridData.DELC[RowIndex]
        end;
        ModelXWidth := FGridLimit.MaxX - FGridLimit.MinX;
        ModelYWidth := FGridLimit.MaxY - FGridLimit.MinY;

        FModelXCenter := (FGridLimit.MaxX + FGridLimit.MinX)/2;
        FModelYCenter := (FGridLimit.MaxY + FGridLimit.MinY)/2;

        Magnification := 1;

        SetTopPosition((FGridLimit.MinX + FGridLimit.MaxX)/2,
          (FGridLimit.MinY + FGridLimit.MaxY)/2);

        SetLength(ColumnPositions, Length(GridData.DELR)+1);

        ColTop  := FGridLimit.MaxY;
        ColBottom  := FGridLimit.MinY;
        ColumnPosition := FGridLimit.MinX;
        ColX := ColumnPosition;
        ColumnPositions[0] := ColX;

        APoint.x := ColX;
        APoint.y := ColBottom;
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        PathBuilder.moveTo(APoint.x, APoint.y);

        APoint.x := ColX;
        APoint.y := ColTop;
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        PathBuilder.lineTo(APoint.x, APoint.y);

        for ColIndex := 0 to Length(GridData.DELR) - 1 do
        begin
          ColumnPosition := ColumnPosition + GridData.DELR[ColIndex];
          ColX := {XCoord}(ColumnPosition);
          ColumnPositions[ColIndex+1] := ColX;

          APoint.x := ColX;
          APoint.y := ColBottom;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);

          PathBuilder.moveTo(APoint.x, APoint.y);

          APoint.x := ColX;
          APoint.y := ColTop;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          PathBuilder.lineTo(APoint.x, APoint.y);
        end;

        SetLength(RowPositions, Length(GridData.DELC)+1);


        RowStart := {XCoord}(FGridLimit.MinX);
        RowEnd := {XCoord}(FGridLimit.MaxX);
        RowPosition := FGridLimit.MinY;
        RowY := {YCoord}(RowPosition);
        RowPositions[0] := RowY;

        APoint.x := RowStart;
        APoint.y := RowY;
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        PathBuilder.moveTo(APoint.x, APoint.y);

        APoint.x := RowEnd;
        APoint.y := RowY;
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        PathBuilder.lineTo(APoint.x, APoint.y);

        for RowIndex := 0 to Length(GridData.DELC) -1 do
        begin
          RowPosition := (RowPosition + GridData.DELC[RowIndex]);
          RowY := {YCoord}(RowPosition);
          RowPositions[RowIndex+1] := RowY;

          APoint.x := RowStart;
          APoint.y := RowY;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          PathBuilder.moveTo(APoint.x, APoint.y);

          APoint.x := RowEnd;
          APoint.y := RowY;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          PathBuilder.lineTo(APoint.x, APoint.y);
        end;

        FGrid := PathBuilder.Detach;

        if NpfPackage <> nil then
        begin
          Inc(FThreadCount);
          MyThread := TMyThread.Create(FCellList);
          MyThread.OnTerminate := DecrementThreadCount;
          MyThread.Start;
          FCellList := TCellList.Create;

          for RowIndex := Length(RowPositions) - 2 downto 0 do
          begin
            for ColIndex := 0 to Length(ColumnPositions) - 2 do
            begin
              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex];
              APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              PathBuilder.moveTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex+1];
              APoint.y := RowPositions[RowIndex];
              APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex+1];
              APoint.y := RowPositions[RowIndex+1];
              APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex+1];
              APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex];
              APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              PathBuilder.LineTo(APoint.x, APoint.y);

              FCellList.Add(PathBuilder.Detach);
            end;
          end;


          Kx := NpfPackage.GridData.K;
          if Kx <> nil then
          begin
            Assert(Length(Kx) > 0);
            Assert(Length(Kx[0]) = Length(RowPositions)-1);
            Assert(Length(Kx[0,0]) = Length(ColumnPositions)-1);
            FMinK := Kx[0,0,0];
            FMaxK := FMinK;
            SetLength(FKxValues, (Length(RowPositions)-1) * (Length(ColumnPositions)-1));
            var CellIndex: Integer := 0;
            for RowIndex := 0 to Length(Kx[0]) - 1 do
            begin
              for ColIndex := 0 to Length(Kx[0,0]) - 1 do
              begin
                FKxValues[CellIndex] := Kx[0,RowIndex,ColIndex];
                Inc(CellIndex);
                if Kx[0,RowIndex,ColIndex] < FMinK then
                begin
                  FMinK := Kx[0,RowIndex,ColIndex]
                end;
                if Kx[0,RowIndex,ColIndex] > FMaxK then
                begin
                  FMaxK := Kx[0,RowIndex,ColIndex]
                end;
              end;
            end;
          end;
        end;
      end;
      if (ModelXWidth = 0) or (ModelYWidth = 0) then
      begin
        Magnification := 1;
      end
      else
      begin
        APoint.x := ColumnPositions[0];
        APoint.y := RowPositions[0];
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        APoint.y := -APoint.y;

        MinX := APoint.x;
        MaxX := APoint.x;
        MinY := APoint.y;
        MaxY := APoint.Y;

        APoint.x := ColumnPositions[Length(ColumnPositions)-1];
        APoint.y := RowPositions[0];
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        APoint.y := -APoint.y;
        MinX := Min(MinX, APoint.x);
        MinY := Min(MinY, APoint.y);
        MaxX := Max(MaxX, APoint.x);
        MaxY := Max(MaxY, APoint.y);

        APoint.x := ColumnPositions[0];
        APoint.y := RowPositions[Length(RowPositions)-1];
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        APoint.y := -APoint.y;
        MinX := Min(MinX, APoint.x);
        MinY := Min(MinY, APoint.y);
        MaxX := Max(MaxX, APoint.x);
        MaxY := Max(MaxY, APoint.y);

        APoint.x := ColumnPositions[Length(ColumnPositions)-1];
        APoint.y := RowPositions[Length(RowPositions)-1];
        APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        APoint.y := -APoint.y;
        MinX := Min(MinX, APoint.x);
        MinY := Min(MinY, APoint.y);
        MaxX := Max(MaxX, APoint.x);
        MaxY := Max(MaxY, APoint.y);

        ModelXWidth := MaxX-MinX;
        ModelYWidth := MaxY-MinY;

        APoint.x := (MinX+MaxX)/2;
        APoint.Y := (MinY+MaxY)/2;

        ModelXCenter := APoint.x;
        ModelYCenter := APoint.Y;

        Magnification :=  0.9 *
          Min(SkPaintBox1.Width / ModelXWidth,
          SkPaintBox1.Height / ModelYWidth);

        ModelXCenter := XCoord(ModelXCenter);
        ModelYCenter := YCoord(ModelYCenter);
        Memo1.Lines.Add((ModelXCenter).ToString + ', ' + (ModelYCenter).ToString);
        FMove.X := SkPaintBox1.Width/2 - ModelXCenter;
        FMove.Y := (SkPaintBox1.Height/2 - ModelYCenter);
      end;

    finally
      Cursor := crDefault;
    end;
  end;

  LocalMagnification := Magnification;

  FDisplayMag := LocalMagnification;
  Label2.Text := LocalMagnification.ToString;

  ACanvas.Translate(FMove.X, FMove.Y);
  if ExaggerationDirection = edHorizontal then
  begin
    ACanvas.Scale(LocalMagnification*FExaggeration,LocalMagnification);
  end
  else
  begin
    ACanvas.Scale(LocalMagnification,LocalMagnification*FExaggeration);
  end;

  if Assigned(FGrid) then
  begin
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.Color := TAlphaColorRec.Black;
    LPaint.StrokeWidth := 1;

    Range := FMaxK - FMinK;
    for var CellIndex := 0 to FCellList.Count - 1 do
    begin
      ACellPath := FCellList[CellIndex];
      if Range = 0 then
      begin
        Fraction := 0.5;
      end
      else
      begin
        Fraction :=   (FKxValues[CellIndex] - FMinK)/Range;
      end;
      LPaint.Style := TSkPaintStyle.Fill;
      LPaint.Color := Color2AlphaColor(FracAndSchemeToColor(0, Fraction, 0.6, 1));
      ACanvas.DrawPath(ACellPath, LPaint);

      LPaint.StrokeWidth := 2.5;
      LPaint.Style := TSkPaintStyle.Stroke;
      LPaint.Color := TAlphaColorRec.Black;
      ACanvas.DrawPath(ACellPath, LPaint);
    end;

    LPaint.AntiAlias := True;
    LPaint.StrokeWidth := 5;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.Color := TAlphaColorRec.Purple;
    ACanvas.DrawPath(FGrid, LPaint);
  end;
end;

procedure TForm2.SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Memo1.BeginUpdate;
  try
    Memo1.Lines.Clear;
    Memo1.Lines.Add(X.ToString + ', ' + Y.ToString);
  finally
    Memo1.EndUpdate;
  end;

  if ssShift in Shift then
  begin
    FStartMove := True;
    FStartPoint := PointF(X, Y);
  end;
end;

procedure TForm2.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  APoint: TPoint2D;
begin
  if FDisplayMag <> 0 then
  begin
   Label1.Text := X.ToString + ' ' + Y.ToString;

   APoint.x := X;
   APoint.y := Y;
   APoint := ScreenToReal(APoint);

   LabelX.Text := 'X = '
//    +
//   (((X-FMove.X)/FDisplayMag)
//   ).ToString
//   + sLineBreak
   + self.X(x).ToString
   + sLineBreak
   + Xcoord(self.X(x)).ToString
//   + APoint.x.ToString
   ;


   LabelY.Text := 'Y = '
//   + (FGridLimit.MaxY + FGridLimit.MinY -
//   ((Y-FMove.Y)/FDisplayMag/FExaggeration)
//   ).ToString
//   + sLineBreak
    + self.Y(Y).ToString
    + sLineBreak
    + YCoord(self.Y(Y)).ToString
//   + APoint.y.ToString
   ;
  end;

   Label2.Text := FDisplayMag.ToString;
end;

procedure TForm2.SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FStartMove then
  begin
    FStartMove := False;
    FStopPoint := PointF(X, Y);
    FMove := (FStopPoint - FStartPoint) + FMove;
    SkPaintBox1.Redraw;
  end;
end;

function TForm2.X(XCoord: single): extended;
begin
//  if HorizontalDirection = hdLeft then
//  begin
//    XCoord := SkPaintBox1.Width - XCoord;
//  end;
  if ExaggerationDirection = edHorizontal then
  begin
    result := (XCoord-FMove.X)/Magnification/ FExaggeration
  end
  else
  begin
    result := (XCoord-FMove.X)/Magnification
  end;
end;

function TForm2.XCoord(X: extended): single;
begin
  if ExaggerationDirection = edHorizontal then
  begin
    result := X * FExaggeration  *Magnification + FMove.X;
  end
  else
  begin
    result := x*Magnification + FMove.X;
  end;
end;

function TForm2.Y(YCoord: single): extended;
begin
  if ExaggerationDirection = edVertical then
  begin
    result := (-YCoord+FMove.Y)/Magnification/FExaggeration;
  end
  else
  begin
    result := (-YCoord+FMove.Y)/Magnification;
  end;
end;

function TForm2.YCoord(Y: extended): single;
begin
  if ExaggerationDirection = edVertical then
  begin
    result := -(Y * FExaggeration * Magnification - FMove.Y);
  end
  else
  begin
    result := -(Y * Magnification - FMove.Y);
  end;
end;

end.
