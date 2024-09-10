unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Skia.Canvas, FMX.StdCtrls, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.Edit, FMX.Objects, FastGEO,
  Mf6.SimulationNameFileReaderUnit, System.Generics.Collections, FMX.Menus,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  // @name indicates whether exaggeration is applied in
  // a vertical or horizontal direction.
  // See TQRbwZoomBox2.@link(TQRbwZoomBox2.ExaggerationDirection).
  TExaggerationDirection = (edVertical, edHorizontal);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.VerticalDirection).}
  TVerticalDirection = (vdDown, vdUp);
  { See TQRbwZoomBox2.@link(TQRbwZoomBox2.HorizontalDirection).}
  THorizontalDirection = (hdRight, hdLeft);

  TFGridLimit = record
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
    FAnObject: TObject;
  public
    procedure Execute; override;
    constructor Create(AnObject: TObject);
  end;

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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
  private
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
    FModelMove: TPointF;
    FGridLimit: TFGridLimit;
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
    function X(XCoord: single): extended;
    {XCoord converts a real-number X coordinate into a screen coordinate.}
    function XCoord(X: extended): single;
    {Y converts a screen coordinate into a real-number Y coordinate.}
    function Y(YCoord: single): extended;
    {YCoord converts a real-number Y coordinate into a screen coordinate.}
    function YCoord(Y: extended): single;
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
//var
//  C1: Integer;
//  C2: Integer;
//  C3: Integer;
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
  FOriginX := 0;
  FOriginY := 0;
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
  {$IFDEF FullSizeTest}
  OriginX := 0;
  OriginY := 0;
  Exit;
  {$ENDIF}

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
  function RotateFromGridCoordinatesToRealWorldCoordinates(
    const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
  begin
    result := APoint;
    if FGridAngle <> 0 then
    begin
      result.x := result.x - FGridLimit.MinX;
      result.y := result.y - FGridLimit.MaxY;
      temp.X := Cos(FGridAngle) * result.X - Sin(FGridAngle) * result.Y;
      temp.Y := Sin(FGridAngle) * result.X + Cos(FGridAngle) * result.Y;
      result := temp;
      result.x := result.x + FGridLimit.MinX;
      result.y := result.y + FGridLimit.MaxY;
    end;
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
            FGridAngle := FRotationAngle*Pi/180;

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

        {$IFDEF FullSizeTest}
        Magnification := 1;
        {$ENDIF}

        SetTopPosition((FGridLimit.MinX + FGridLimit.MaxX)/2,
          (FGridLimit.MinY + FGridLimit.MaxY)/2);

        SetLength(ColumnPositions, Length(GridData.DELR)+1);

        ColTop  := {YCoord}(FGridLimit.MaxY);
        ColBottom  := {YCoord}(FGridLimit.MinY);
        ColumnPosition := FGridLimit.MinX;
        ColX := {XCoord}(ColumnPosition);
        ColumnPositions[0] := ColX;

        APoint.x := ColX;
        APoint.y := ColBottom;
        if FGridAngle <> 0 then
        begin
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        end;
        PathBuilder.moveTo(APoint.x, APoint.y);

        APoint.x := ColX;
        APoint.y := ColTop;
        if FGridAngle <> 0 then
        begin
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        end;
        PathBuilder.lineTo(APoint.x, APoint.y);

        for ColIndex := 0 to Length(GridData.DELR) - 1 do
        begin
          ColumnPosition := ColumnPosition + GridData.DELR[ColIndex];
          ColX := {XCoord}(ColumnPosition);
          ColumnPositions[ColIndex+1] := ColX;

          APoint.x := ColX;
          APoint.y := ColBottom;
          if FGridAngle <> 0 then
          begin
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          end;

          PathBuilder.moveTo(APoint.x, APoint.y);

          APoint.x := ColX;
          APoint.y := ColTop;
          if FGridAngle <> 0 then
          begin
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          end;
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
        if FGridAngle <> 0 then
        begin
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        end;
        PathBuilder.moveTo(APoint.x, APoint.y);

        APoint.x := RowEnd;
        APoint.y := RowY;
        if FGridAngle <> 0 then
        begin
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        end;
        PathBuilder.lineTo(APoint.x, APoint.y);

        for RowIndex := 0 to Length(GridData.DELC) -1 do
        begin
          RowPosition := (RowPosition + GridData.DELC[RowIndex]);
          RowY := {YCoord}(RowPosition);
          RowPositions[RowIndex+1] := RowY;

          APoint.x := RowStart;
          APoint.y := RowY;
          if FGridAngle <> 0 then
          begin
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          end;
          PathBuilder.moveTo(APoint.x, APoint.y);

          APoint.x := RowEnd;
          APoint.y := RowY;
          if FGridAngle <> 0 then
          begin
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          end;
          PathBuilder.lineTo(APoint.x, APoint.y);

//          PathBuilder.moveTo(RowStart, RowY);
//            PathBuilder.lineTo(RowEnd, RowY);
        end;

        FGrid := PathBuilder.Detach;

//        Memo1.Lines.BeginUpdate;
//        Memo1.Lines.Clear;
//        try
//          PathData := SkPathToPathData(FGrid);
//          for var Index := 0 to PathData.Count - 1 do
//          begin
//            Memo1.Lines.Add(PathData.Points[Index].Point.x.ToString + ' ' + PathData.Points[Index].Point.y.ToString)
//          end;
//        finally
//          Memo1.Lines.EndUpdate;
//        end;

        if NpfPackage <> nil then
        begin

          Inc(FThreadCount);
          MyThread := TMyThread.Create(FCellList);
          MyThread.OnTerminate := DecrementThreadCount;
          MyThread.Start;
          FCellList := TCellList.Create;

          for RowIndex := 0 to Length(RowPositions) - 2 do
          begin
            for ColIndex := 0 to Length(ColumnPositions) - 2 do
            begin

              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex];
              if FGridAngle <> 0 then
              begin
                APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              end;
              PathBuilder.moveTo(APoint.x, APoint.y);


              APoint.x := ColumnPositions[ColIndex+1];
              APoint.y := RowPositions[RowIndex];
              if FGridAngle <> 0 then
              begin
                APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              end;
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex+1];
              APoint.y := RowPositions[RowIndex+1];
              if FGridAngle <> 0 then
              begin
                APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              end;
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex+1];
              if FGridAngle <> 0 then
              begin
                APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              end;
              PathBuilder.LineTo(APoint.x, APoint.y);

              APoint.x := ColumnPositions[ColIndex];
              APoint.y := RowPositions[RowIndex];
              if FGridAngle <> 0 then
              begin
                APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
              end;
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
        if FGridAngle <> 0 then
        begin
          APoint.x := RowStart;
          APoint.y := ColBottom;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);

          MinX := APoint.x;
          MaxX := APoint.x;
          MinY := APoint.y;
          MaxY := APoint.Y;

          APoint.x := RowEnd;
          APoint.y := ColBottom;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          MinX := Min(MinX, APoint.x);
          MinY := Min(MinY, APoint.y);
          MaxX := Max(MaxX, APoint.x);
          MaxY := Max(MaxY, APoint.y);

          APoint.x := RowStart;
          APoint.y := ColTop;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          MinX := Min(MinX, APoint.x);
          MinY := Min(MinY, APoint.y);
          MaxX := Max(MaxX, APoint.x);
          MaxY := Max(MaxY, APoint.y);

          APoint.x := RowEnd;
          APoint.y := ColTop;
          APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          MinX := Min(MinX, APoint.x);
          MinY := Min(MinY, APoint.y);
          MaxX := Max(MaxX, APoint.x);
          MaxY := Max(MaxY, APoint.y);

          ModelXWidth := MaxX-MinX;
          ModelYWidth := MaxY-MinY;
        end;


        Magnification :=// 0.9 *
          Min(SkPaintBox1.Width / ModelXWidth,
          SkPaintBox1.Height / ModelYWidth);
      end;

    finally
      Cursor := crDefault;
    end;
  end;

  LocalMagnification := Magnification;

  FDisplayMag := LocalMagnification;
  Label2.Text := LocalMagnification.ToString;

  try

    ACanvas.Translate(FMove.X, FMove.Y);

    ACanvas.Scale(LocalMagnification,LocalMagnification);

    if Assigned(FGrid) then
    begin

//        Memo2.Lines.BeginUpdate;
//        try
//          Memo2.Lines.Clear;
//          PathData := SkPathToPathData(FGrid);
//          for var Index := 0 to PathData.Count - 1 do
//          begin
//            Memo2.Lines.Add(PathData.Points[Index].Point.x.ToString + ' ' + PathData.Points[Index].Point.y.ToString)
//          end;
//        finally
//          Memo2.Lines.EndUpdate;
//        end;

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
  finally
//    ACanvas.Restore;
  end;
end;

procedure TForm2.SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if ssShift in Shift then
  begin
    FStartMove := True;
    FStartPoint := PointF(X, Y);
  end;
end;

procedure TForm2.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FDisplayMag <> 0 then
  begin
   Label1.Text := X.ToString + ' ' + Y.ToString;
//   + sLineBreak

   Label4.Text := 'X = ' + ({FGridLimit.MaxX +} {FGridLimit.MinX + }
   (((X-FMove.X)/FDisplayMag)
//   -FModelMove.X
   )).ToString;


   Label5.Text := 'Y = ' + (FGridLimit.MaxY + FGridLimit.MinY -
   (((Y-FMove.Y)/FDisplayMag)
//   -FModelMove.Y
   )).ToString;
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
    FModelMove := (FStopPoint - FStartPoint)/FDisplayMag + FModelMove;
    Label3.Text := (FModelMove.X).ToString + ', ' + (FModelMove.Y).ToString;
    SkPaintBox1.Redraw;
  end;
end;

function TForm2.X(XCoord: single): extended;
begin
  if HorizontalDirection = hdLeft then
  begin
    XCoord := SkPaintBox1.Width - XCoord;
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

function TForm2.XCoord(X: extended): single;
var
  temp: extended;
begin
  try
    {$IFDEF FullSizeTest}
    if ExaggerationDirection = edHorizontal then
    begin
      temp := X * FExaggeration;
    end
    else
    begin
      temp := X;
    end;
    {$ELSE}
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
    {$ENDIF}
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
      result := temp;
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

function TForm2.Y(YCoord: single): extended;
begin
  if VerticalDirection = vdUp then
  begin
    YCoord := SkPaintBox1.Height - YCoord;
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

function TForm2.YCoord(Y: extended): single;
var
  temp: extended;
begin
  try
  {$IFDEF FullSizeTest}
    if ExaggerationDirection = edVertical then
    begin
      temp := Y * FExaggeration
    end
    else
    begin
      temp := Y
    end;
//    if VerticalDirection = vdUp then
//    begin
//      temp := ClientHeight - temp;
//    end;
  {$ELSE}
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
    {$ENDIF}
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
      result := temp;
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

constructor TMyThread.Create(AnObject: TObject);
begin
  FAnObject := AnObject;
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TMyThread.Execute;
begin
  FAnObject.Free;
  
end;

end.
