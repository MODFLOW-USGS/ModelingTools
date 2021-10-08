unit GuiSettingsUnit;

interface

uses Classes, Forms, frameViewUnit, ZoomBox2, RbwRuler, System.Math;

type
  TGuiSettings = class(TPersistent)
  private
    procedure SetWindowState(Value : TWindowState);
    function GetWindowState: TWindowState;
    procedure SetWidth(Value : integer);
    function GetWidth: integer;
    procedure SetTopY(Value : double);
    function GetTopY: double;
    procedure SetTopX(Value : double);
    function GetTopX: double;
    procedure SetTopViewWidth(Value : integer);
    function GetTopViewWidth: integer;
    procedure SetTopViewHeight(Value : integer);
    function GetTopViewHeight: integer;
    procedure SetTop(Value : integer);
    function GetTop: integer;
    procedure SetSideY(Value : double);
    function GetSideY: double;
    procedure SetSideX(Value : double);
    function GetSideX: double;
    procedure SetSideWidth(Value : integer);
    function GetSideWidth: integer;
    procedure SetMagnificationTop(Value : double);
    function GetMagnificationTop: double;
    procedure SetMagnificationSide(Value : double);
    function GetMagnificationSide: double;
    procedure SetMagnificationFront(Value : double);
    function GetMagnificationFront: double;
    procedure SetLeft(Value : integer);
    function GetLeft: integer;
    procedure SetFrontY(Value : double);
    function GetFrontY: double;
    procedure SetFrontX(Value : double);
    function GetFrontX: double;
    procedure SetHeight(Value : integer);
    function GetHeight: integer;
    procedure SetFrontHeight(Value : integer);
    function GetFrontHeight: integer;
    function GetFrontHorizontalDigits: integer;
    function GetFrontHorizontalPrecision: integer;
    function GetFrontVerticalDigits: integer;
    function GetFrontVerticalPrecision: integer;
    function GetSideHorizontalDigits: integer;
    function GetSideHorizontalPrecision: integer;
    function GetSideVerticalDigits: integer;
    function GetSideVerticalPrecision: integer;
    function GetTopHorizontalDigits: integer;
    function GetTopHorizontalPrecision: integer;
    function GetTopVerticalDigits: integer;
    function GetTopVerticalPrecision: integer;
    procedure SetFrontHorizontalDigits(const Value: integer);
    procedure SetFrontHorizontalPrecision(const Value: integer);
    procedure SetFrontVerticalDigits(const Value: integer);
    procedure SetFrontVerticalPrecision(const Value: integer);
    procedure SetSideHorizontalDigits(const Value: integer);
    procedure SetSideHorizontalPrecision(const Value: integer);
    procedure SetSideVerticalDigits(const Value: integer);
    procedure SetSideVerticalPrecision(const Value: integer);
    procedure SetTopHorizontalDigits(const Value: integer);
    procedure SetTopHorizontalPrecision(const Value: integer);
    procedure SetTopVerticalDigits(const Value: integer);
    procedure SetTopVerticalPrecision(const Value: integer);
    function GetFrontHorizontalDesiredSpacing: integer;
    function GetFrontVerticalDesiredSpacing: integer;
    function GetSideHorizontalDesiredSpacing: integer;
    function GetSideVerticalDesiredSpacing: integer;
    function GetTopHorizontalDesiredSpacing: integer;
    function GetTopVerticalDesiredSpacing: integer;
    procedure SetFrontHorizontalDesiredSpacing(const Value: integer);
    procedure SetFrontVerticalDesiredSpacing(const Value: integer);
    procedure SetSideHorizontalDesiredSpacing(const Value: integer);
    procedure SetSideVerticalDesiredSpacing(const Value: integer);
    procedure SetTopHorizontalDesiredSpacing(const Value: integer);
    procedure SetTopVerticalDesiredSpacing(const Value: integer);
    function GetTopView: TframeView;
    function GetTopZoomBox: TQRbwZoomBox2;
    function GetSideView: TframeView;
    function GetSideZoomBox: TQRbwZoomBox2;
    function GetFrontView: TframeView;
    function GetFrontZoomBox: TQRbwZoomBox2;
    function GetFrontHorizontalRuler: TRbwRuler;
    function GetFrontVerticalRuler: TRbwRuler;
    function GetSideVerticalRuler: TRbwRuler;
    function GetSideHorizontalRuler: TRbwRuler;
    function GetTopVerticalRuler: TRbwRuler;
    function GetTopHorizontalRuler: TRbwRuler;
  published
    // @name stores the height in pixels of the front view of the model.
    property FrontHeight: integer read GetFrontHeight write SetFrontHeight;

    // @name is the height of the main form in pixels.
    property Height: integer read GetHeight write SetHeight;

    // @name stores the reference X-coordinate for the front view of the model.
    property FrontX: double read GetFrontX write SetFrontX;

    // @name stores the reference Y-coordinate for the front view of the model.
    property FrontY: double read GetFrontY write SetFrontY;

    // @name is the X-coordinate of the main form in pixels.
    property Left: integer read GetLeft write SetLeft;

    // @name is the magnification of the front view of the model.
    property MagnificationFront: double read GetMagnificationFront
      write SetMagnificationFront;

    // @name is the magnification of the side view of the model.
    property MagnificationSide: double read GetMagnificationSide
      write SetMagnificationSide;

    // @name is the magnification of the top view of the model.
    property MagnificationTop: double read GetMagnificationTop
      write SetMagnificationTop;

    // @name is the width in pixels of the side view of the model.
    property SideWidth: integer read GetSideWidth write SetSideWidth;

    // @name stores the reference X-coordinate for the side view of the model.
    property SideX: double read GetSideX write SetSideX;

    // @name stores the reference Y-coordinate for the front view of the model.
    property SideY: double read GetSideY write SetSideY;

    // @name is the Y-coordinate of the main form in pixels.
    property Top: integer read GetTop write SetTop;

    // @name is the height of the top view of the model in pixels
    property TopViewHeight: integer read GetTopViewHeight
      write SetTopViewHeight;

    // @name is the width of the top view of the model in pixels
    property TopViewWidth: integer read GetTopViewWidth write SetTopViewWidth;

    // @name stores the reference X-coordinate for the top view of the model.
    property TopX: double read GetTopX write SetTopX;

    // @name stores the reference Y-coordinate for the top view of the model.
    property TopY: double read GetTopY write SetTopY;

    // @name is the width of the main form in GoPhast in pixels.
    property Width: integer read GetWidth write SetWidth;

    // @name stores whether the model is maximized, minimized, or normal.
    property WindowState: TWindowState read GetWindowState write SetWindowState;

    property TopHorizontalDigits: integer read GetTopHorizontalDigits write SetTopHorizontalDigits;
    property TopHorizontalPrecision: integer read GetTopHorizontalPrecision write SetTopHorizontalPrecision;
    property TopHorizontalDesiredSpacing: integer read GetTopHorizontalDesiredSpacing write SetTopHorizontalDesiredSpacing;

    property TopVerticalDigits: integer read GetTopVerticalDigits write SetTopVerticalDigits;
    property TopVerticalPrecision: integer read GetTopVerticalPrecision write SetTopVerticalPrecision;
    property TopVerticalDesiredSpacing: integer read GetTopVerticalDesiredSpacing write SetTopVerticalDesiredSpacing;

    property FrontHorizontalDigits: integer read GetFrontHorizontalDigits write SetFrontHorizontalDigits;
    property FrontHorizontalPrecision: integer read GetFrontHorizontalPrecision write SetFrontHorizontalPrecision;
    property FrontHorizontalDesiredSpacing: integer read GetFrontHorizontalDesiredSpacing write SetFrontHorizontalDesiredSpacing;

    property FrontVerticalDigits: integer read GetFrontVerticalDigits write SetFrontVerticalDigits;
    property FrontVerticalPrecision: integer read GetFrontVerticalPrecision write SetFrontVerticalPrecision;
    property FrontVerticalDesiredSpacing: integer read GetFrontVerticalDesiredSpacing write SetFrontVerticalDesiredSpacing;

    property SideHorizontalDigits: integer read GetSideHorizontalDigits write SetSideHorizontalDigits;
    property SideHorizontalPrecision: integer read GetSideHorizontalPrecision write SetSideHorizontalPrecision;
    property SideHorizontalDesiredSpacing: integer read GetSideHorizontalDesiredSpacing write SetSideHorizontalDesiredSpacing;

    property SideVerticalDigits: integer read GetSideVerticalDigits write SetSideVerticalDigits;
    property SideVerticalPrecision: integer read GetSideVerticalPrecision write SetSideVerticalPrecision;
    property SideVerticalDesiredSpacing: integer read GetSideVerticalDesiredSpacing write SetSideVerticalDesiredSpacing;

  end;

implementation

uses frmGoPhastUnit;

function TGuiSettings.GetFrontHeight: integer;
begin
  if frmGoPhast.pnlBottom <> nil then
  begin
    result := frmGoPhast.pnlBottom.Height;
  end
  else
  begin
    result := 100;
  end;
end;

procedure TGuiSettings.SetFrontHeight(Value : integer);
begin
  if frmGoPhast.pnlBottom <> nil then
  begin
    if Value < 1 then
    begin
      Value := 1;
    end;
    frmGoPhast.pnlBottom.Height := Value;
    if (frmGoPhast.sbMain <> nil) and (frmGoPhast.splitHoriz <> nil) then
    begin
      if frmGoPhast.pnlBottom.Top > frmGoPhast.sbMain.Top then
      begin
        frmGoPhast.sbMain.Top := frmGoPhast.pnlBottom.Top +
          frmGoPhast.pnlBottom.Height;
      end;
      if frmGoPhast.splitHoriz.Top > frmGoPhast.pnlBottom.Top then
      begin
        frmGoPhast.splitHoriz.Top :=
          frmGoPhast.pnlBottom.Top - frmGoPhast.splitHoriz.Height;
      end;
    end;
  end;
end;

procedure TGuiSettings.SetFrontHorizontalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

procedure TGuiSettings.SetFrontHorizontalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetFrontHorizontalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

procedure TGuiSettings.SetFrontVerticalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

procedure TGuiSettings.SetFrontVerticalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetFrontVerticalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

function TGuiSettings.GetHeight: integer;
begin
  if frmGoPhast.WindowState = wsNormal then
  begin
    result := frmGoPhast.Height;
  end
  else
  begin
    result := frmGoPhast.OldHeight;
  end;
end;

procedure TGuiSettings.SetHeight(Value : integer);
begin
  frmGoPhast.Height := Value;
end;

function TGuiSettings.GetFrontHorizontalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetFrontHorizontalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetFrontHorizontalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 0;
  end;
end;

function TGuiSettings.GetFrontVerticalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetFrontVerticalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetFrontVerticalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetFrontVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 0;
  end;
end;

function TGuiSettings.GetFrontX: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginX;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetFrontX(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginX := Value;
  end;
end;

function TGuiSettings.GetFrontY: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginY;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetFrontY(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginY := Value;
  end;
end;

function TGuiSettings.GetLeft: integer;
begin
  result := frmGoPhast.Left;
end;

procedure TGuiSettings.SetLeft(Value : integer);
begin
  frmGoPhast.Left := Value;
end;

function TGuiSettings.GetMagnificationFront: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.Magnification;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TGuiSettings.SetMagnificationFront(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  ZoomBox := GetFrontZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.Magnification := Value;
  end;
end;

function TGuiSettings.GetMagnificationSide: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.Magnification;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TGuiSettings.SetMagnificationSide(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.Magnification := Value;
  end;
end;

function TGuiSettings.GetMagnificationTop: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.Magnification;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TGuiSettings.SetMagnificationTop(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.Magnification := Value;
  end;
end;

function TGuiSettings.GetSideHorizontalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetSideHorizontalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetSideHorizontalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 0;
  end;
end;

function TGuiSettings.GetSideVerticalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetSideVerticalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetSideVerticalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetSideWidth: integer;
var
  SideFrameView: TframeView;
begin
  SideFrameView := GetSideView;
  if SideFrameView <> nil then
  begin
    result := SideFrameView.Width;
  end
  else
  begin
    result := 100;
  end;
end;

procedure TGuiSettings.SetSideHorizontalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

procedure TGuiSettings.SetSideHorizontalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetSideHorizontalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

procedure TGuiSettings.SetSideVerticalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

procedure TGuiSettings.SetSideVerticalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetSideVerticalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetSideVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

procedure TGuiSettings.SetSideWidth(Value : integer);
var
  SideFrameView: TframeView;
begin
  SideFrameView := GetSideView;
  if SideFrameView <> nil then
  begin
    if Value < 1 then
    begin
      Value := 1;
    end;
    SideFrameView.Width := Value;
    if (frmGoPhast.frame3DView <> nil)
      and (frmGoPhast.splitVertBottom <> nil)
      and (frmGoPhast.splitVertTop <> nil) then
    begin
      frmGoPhast.frame3DView.Width := Value;
      if frmGoPhast.splitVertBottom.Left > frmGoPhast.frame3DView.Left then
      begin
        frmGoPhast.splitVertBottom.Left :=
          frmGoPhast.frame3DView.Left - frmGoPhast.splitVertBottom.Width;
      end;
      if frmGoPhast.splitVertTop.Left > SideFrameView.Left then
      begin
        frmGoPhast.splitVertTop.Left :=
          SideFrameView.Left - frmGoPhast.splitVertTop.Width;
      end;
    end;
  end;
end;

function TGuiSettings.GetSideX: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginX;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetSideX(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginX := Value;
  end;
end;

function TGuiSettings.GetSideY: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginY;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetSideY(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetSideZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginY := Value;
  end;
end;

function TGuiSettings.GetTop: integer;
begin
  result := frmGoPhast.Top;
end;

function TGuiSettings.GetTopHorizontalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetTopHorizontalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetTopHorizontalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetTop(Value : integer);
begin
  frmGoPhast.Top := Value;
end;

function TGuiSettings.GetTopVerticalDesiredSpacing: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDesiredSpacing;
  end
  else
  begin
    result := 50;
  end;
end;

function TGuiSettings.GetTopVerticalDigits: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerDigits;
  end
  else
  begin
    result := 5;
  end;
end;

function TGuiSettings.GetTopVerticalPrecision: integer;
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    result := Ruler.RulerPrecision;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetTopHorizontalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

procedure TGuiSettings.SetTopHorizontalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetTopHorizontalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopHorizontalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

function TGuiSettings.GetTopViewHeight: integer;
var
  TopFrameView: TframeView;
begin
  TopFrameView := GetTopView;
  if TopFrameView <> nil then
  begin
    result := TopFrameView.Height;
  end
  else
  begin
    result := 100;
  end;
end;

procedure TGuiSettings.SetTopVerticalDesiredSpacing(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDesiredSpacing := Value;
  end;
end;

function TGuiSettings.GetTopView: TframeView;
begin
  result := frmGoPhast.frameTopView;
end;

function TGuiSettings.GetTopZoomBox: TQRbwZoomBox2;
var
  TopFrameView: TframeView;
begin
  TopFrameView := GetTopView;
  if TopFrameView <> nil then
  begin
    result := TopFrameView.ZoomBox;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetSideView: TframeView;
begin
  result := frmGoPhast.frameSideView;
end;

function TGuiSettings.GetFrontView: TframeView;
begin
  result := frmGoPhast.frameFrontView;
end;

function TGuiSettings.GetSideZoomBox: TQRbwZoomBox2;
var
  SideFrameView: TframeView;
begin
  SideFrameView := GetSideView;
  if SideFrameView <> nil then
  begin
    result := SideFrameView.ZoomBox;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetFrontZoomBox: TQRbwZoomBox2;
var
  FrontFrameView: TframeView;
begin
  FrontFrameView := GetFrontView;
  if FrontFrameView <> nil then
  begin
    result := FrontFrameView.ZoomBox;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetFrontHorizontalRuler: TRbwRuler;
var
  FrontFrameView: TframeView;
begin
  FrontFrameView := GetFrontView;
  if FrontFrameView <> nil then
  begin
    result := FrontFrameView.rulHorizontal;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetTopVerticalRuler: TRbwRuler;
var
  FrameTopView: TframeView;
begin
  FrameTopView := GetTopView;
  if FrameTopView <> nil then
  begin
    result := FrameTopView.rulVertical;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetTopHorizontalRuler: TRbwRuler;
var
  FrameTopView: TframeView;
begin
  FrameTopView := GetTopView;
  if FrameTopView <> nil then
  begin
    result := FrameTopView.rulHorizontal;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetSideVerticalRuler: TRbwRuler;
var
  FrameSideView: TframeView;
begin
  FrameSideView := GetSideView;
  if FrameSideView <> nil then
  begin
    result := FrameSideView.rulVertical;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetSideHorizontalRuler: TRbwRuler;
var
  FrameSideView: TframeView;
begin
  FrameSideView := GetSideView;
  if FrameSideView <> nil then
  begin
    result := FrameSideView.rulHorizontal;
  end
  else
  begin
    result := nil;
  end;
end;

function TGuiSettings.GetFrontVerticalRuler: TRbwRuler;
var
  FrontFrameView: TframeView;
begin
  FrontFrameView := GetFrontView;
  if FrontFrameView <> nil then
  begin
    result := FrontFrameView.rulVertical;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TGuiSettings.SetTopVerticalDigits(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerDigits := Value;
  end;
end;

procedure TGuiSettings.SetTopVerticalPrecision(const Value: integer);
var
  Ruler: TRbwRuler;
begin
  Ruler := GetTopVerticalRuler;
  if Ruler <> nil then
  begin
    Ruler.RulerPrecision := Value;
  end;
end;

procedure TGuiSettings.SetTopViewHeight(Value : integer);
var
  TopFrameView: TframeView;
begin
  TopFrameView := GetTopView;
  if TopFrameView <> nil then
  begin
    TopFrameView.Height := value;
  end;
end;

function TGuiSettings.GetTopViewWidth: integer;
var
  TopFrameView: TframeView;
begin
  TopFrameView := GetTopView;
  if TopFrameView <> nil then
  begin
    result := TopFrameView.Width;
  end
  else
  begin
    result := 100;
  end;
end;

procedure TGuiSettings.SetTopViewWidth(Value : integer);
var
  TopFrameView: TframeView;
begin
  TopFrameView := GetTopView;
  if TopFrameView <> nil then
  begin
    TopFrameView.Width := value;
  end;
end;

function TGuiSettings.GetTopX: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginX;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetTopX(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginX := Value;
  end;
end;

function TGuiSettings.GetTopY: double;
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    result := ZoomBox.OriginY;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TGuiSettings.SetTopY(Value : double);
var
  ZoomBox: TQRbwZoomBox2;
begin
  ZoomBox := GetTopZoomBox;
  if ZoomBox <> nil then
  begin
    ZoomBox.OriginY := Value;
  end;
end;

function TGuiSettings.GetWidth: integer;
begin
  if frmGoPhast.WindowState = wsNormal then
  begin
    result := frmGoPhast.Width
  end
  else
  begin
    result := frmGoPhast.OldWidth
  end;

end;

procedure TGuiSettings.SetWidth(Value : integer);
begin
  frmGoPhast.Width := Max(Value, 775);
end;

function TGuiSettings.GetWindowState: TWindowState;
begin
  if frmGoPhast.WindowState = wsMinimized then
  begin
    result := wsNormal;
  end
  else
  begin
    result := frmGoPhast.WindowState;
  end;
end;

procedure TGuiSettings.SetWindowState(Value : TWindowState);
begin
  frmGoPhast.WindowState := Value;
end;

end.
