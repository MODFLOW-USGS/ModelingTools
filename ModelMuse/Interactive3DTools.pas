unit Interactive3DTools;

interface

uses SysUtils, Classes, QControls, arcball;

Type
  T3DInteractor = class(TComponent)
  private
    FZScale: double;
    FZoomFactor: double;
    FXStart: integer;
    FYStart: integer;
    FZStart: double;
    FXPanStart: double;
    FYPanStart: double;
    FXPan: double;
    FYPan: double;
    FZooming: boolean;
    FPanning: boolean;
    FXOffset: integer;
    FYOffset: integer;
    FTheBall: TArcBall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure Render(Sender: TObject); virtual;
    procedure Resize(Sender: TObject); virtual;
    procedure SetDefaultOrientation;
  end;

const
  PanFactor = 0.25;
  ZOffset = -50;
  ViewAngle = 45; // in degrees.
  nearPosition = 1;
  farPosition = 1E3;

implementation

uses frmGoPhastUnit, CursorsFoiledAgain, Math, OpenGL12x, frmColorsUnit;

{ T3DInteractor }

constructor T3DInteractor.Create(AOwner: TComponent);
begin
  inherited;
  FZoomFactor := 1;
  FTheBall := TArcBall.Create;
  FTheBall.Init(Min(frmGoPhast.glWidModelView.ClientWidth,
    frmGoPhast.glWidModelView.ClientHeight) div 2);
  SetDefaultOrientation;
  frmGoPhast.glWidModelView.OnMouseDown := MouseDown;
  frmGoPhast.glWidModelView.OnMouseMove := MouseMove;
  frmGoPhast.glWidModelView.OnMouseUp := MouseUp;
  frmGoPhast.glWidModelView.OnRender := Render;
  frmGoPhast.glWidModelView.OnResize := Resize;
end;

destructor T3DInteractor.Destroy;
begin
  FTheBall.Free;
  inherited;
end;

procedure T3DInteractor.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not frmGoPhast.glWidModelView.Started then
    Exit;

  if Button = mbLeft then
  begin
    if ssShift in Shift then
    begin
      // Pan
      FXStart := X;
      FYStart := Y;
      FXPanStart := FXPan;
      FYPanStart := FYPan;
      FPanning := True;
      frmGoPhast.glWidModelView.Cursor := crHandFlat;
    end
    else
    begin
      // Rotate
      FTheBall.BeginDrag(X - FXOffset, Y - FYOffset);
      frmGoPhast.glWidModelView.Invalidate;
    end;
  end
  else if Button = mbRight then
  begin
    // Zoom
    FYStart := Y;
    FZStart := FZoomFactor;
    FZooming := True;
    frmGoPhast.glWidModelView.Cursor := crZoomByY;
  end;
end;

procedure T3DInteractor.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin
  if not frmGoPhast.glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > frmGoPhast.glWidModelView.Height then
    begin
      Y := frmGoPhast.glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / frmGoPhast.glWidModelView.Height;
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;
    //ZScale := ZStart*(1-Factor);//*ZFactor;
    frmGoPhast.glWidModelView.Invalidate;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    frmGoPhast.glWidModelView.Invalidate;
  end
  else if FTheBall.Dragging then
  begin
    FTheBall.MouseMove(X - FXOffset, Y - FYOffset);
    frmGoPhast.glWidModelView.Invalidate;
  end;
end;

procedure T3DInteractor.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin
  if not frmGoPhast.glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > frmGoPhast.glWidModelView.Height then
    begin
      Y := frmGoPhast.glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / frmGoPhast.glWidModelView.Height;
    //ZScale := ZStart*(1-Factor);
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;

    frmGoPhast.glWidModelView.Invalidate;
    FZooming := False;
    frmGoPhast.glWidModelView.Cursor := crDefault;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    frmGoPhast.glWidModelView.Invalidate;
    FPanning := False;
    frmGoPhast.glWidModelView.Cursor := crDefault;
  end
  else if FTheBall.Dragging then
  begin
    FTheBall.EndDrag(X - FXOffset, Y - FYOffset);
    frmGoPhast.glWidModelView.Invalidate;
  end;
end;

procedure T3DInteractor.Render(Sender: TObject);
var
  errorCode: TGLuint;
  XMove, YMove, ZMove: single;
  light_position: array[0..3] of GLfloat;
  //  Colors: array[0..3] of TGLint;
  //const
  //  XY = 40.0;
  //  light_position : array[0..3] of GLfloat = ( 5.0, 40.0, -4.0, 2.0 );
  //  light_position : array[0..3] of GLfloat = ( XY, XY, 40.0, 0.0 );
  //  light_position2 : array[0..3] of GLfloat = ( -XY, -XY, 40.0, 0.0 );
begin
  if not frmGoPhast.glWidModelView.Started then
    Exit;

  light_position[0] := ColorValues.X;
  light_position[1] := ColorValues.Y;
  light_position[2] := ColorValues.Z;
  light_position[3] := 0;

  if (frmGoPhast.Model = nil) or (csReading in frmGoPhast.Model.ComponentState) then
    Exit;
  // White background
  glClearColor(1.0, 1.0, 1.0, 0.0);

  //  glShadeModel(GL_FLAT);
  glShadeModel(GL_SMOOTH);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLightfv(GL_LIGHT0, GL_POSITION, @light_position);

  //  glEnable(GL_CULL_FACE);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glPushMatrix;

  XMove := 0;
  YMove := 0;
  ZMove := 0;
  if frmGoPhast.Model.PhastGrid.CanDraw3D then
  begin
    with frmGoPhast.Model.PhastGrid do
    begin
      XMove := -(ColumnPosition[0] + ColumnPosition[ColumnCount]) / 2;
      YMove := -(RowPosition[0] + RowPosition[RowCount]) / 2;
      ZMove := -(LayerElevation[0] + LayerElevation[LayerCount]) / 2;
    end;
  end;

  // The descriptions below are written as if they apply to the model
  // when they actually affect the coordinate system.  Because they
  // actually affect the coordinate system, the order in which they affect
  // the model is the reverse of the order they appear.  For example
  // XPan and YPan are applied last and moving the center of the model
  // to the origin is applied first.

  // XPan and YPan moves the model around on the screen.
  glTranslatef(FXPan, FYPan, 0);
  // ZOffset moves the model back behind the near cut-off position.
  glTranslatef(0, 0, ZOffset);
  // Rotate the model based on the grid angle after converting
  // from radians to degrees.
  glRotatef(frmGoPhast.PhastGrid.GridAngle / Pi * 180, 0.0, 0.0, 1.0);
  // Respond to "rolling" of the model.
  glMultMatrixf(@FTheBall.Matrix);
  // Use ZScale to scale the model to a size that can be
  // confortably seen.
  // Apply the vertical exaggeration to the model.
  glScalef(FZScale * FZoomFactor, FZScale * FZoomFactor,
    FZScale * FZoomFactor * frmGoPhast.Model.Exaggeration);
  // Move the center of the model to the origin so that all
  // "rolling" is relative to the center of the model.
  glTranslatef(XMove, YMove, ZMove);
  glPushMatrix;

  glEnable(GL_DEPTH_TEST);

  frmGoPhast.Model.PhastGrid.Draw3D;
  if frmGoPhast.tb3DObjects.Down then
  begin
    frmGoPhast.Model.DrawScreenObjects3D;
  end;
  glPopMatrix;

  glLineWidth(2);
  glPointSize(5);

  glColor3ub(0, 0, 0);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  //  glDisable(GL_LIGHTING);
  FTheBall.Render;
  //  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  errorCode := glGetError;

  if errorCode <> GL_NO_ERROR then
    Beep;
  glPopMatrix;

  glFlush;

  errorCode := glGetError;

  if errorCode <> GL_NO_ERROR then
    Beep;
  //raise Exception.Create('Error in Render'#13 + gluErrorString(errorCode));
end;

procedure T3DInteractor.Resize(Sender: TObject);
var
  errorCode: TGLuint;
begin
  if not frmGoPhast.glWidModelView.Started then
    Exit;
  if FTheBall <> nil then
  begin
    FTheBall.Init(Min(frmGoPhast.glWidModelView.ClientWidth, frmGoPhast.glWidModelView.ClientHeight)
      div 2, frmGoPhast.glWidModelView.ClientWidth / frmGoPhast.glWidModelView.ClientHeight);
  end;
  frmGoPhast.glWidModelView.PerspectiveProjection(ViewAngle, nearPosition, farPosition);

  if frmGoPhast.glWidModelView.ClientWidth > frmGoPhast.glWidModelView.ClientHeight then
  begin
    FXOffset := (frmGoPhast.glWidModelView.ClientWidth - frmGoPhast.glWidModelView.ClientHeight)
      div 2;
    FYOffset := 0;
  end
  else
  begin
    FXOffset := 0;
    FYOffset := (frmGoPhast.glWidModelView.ClientHeight - frmGoPhast.glWidModelView.ClientWidth)
      div 2;
  end;

  errorCode := glGetError;
  if errorCode <> GL_NO_ERROR then
    raise Exception.Create('Error in Render'#13 + gluErrorString(errorCode));
end;

procedure T3DInteractor.SetDefaultOrientation;
var
  Angle: double;
  TanAngle: double;
  Z1, Z2: single;
begin
  if frmGoPhast.Model.PhastGrid.CanDraw3D then
  begin
    Angle := ViewAngle / 2 / 180 * Pi;
    TanAngle := Tan(Angle);
    with frmGoPhast.Model.PhastGrid do
    begin
      Z1 := (ColumnPosition[ColumnCount] - ColumnPosition[0]) / 2;
      Z2 := (RowPosition[RowCount] - RowPosition[0]) / 2;
      {ZScale := (nearPosition - ZOffset +
        ((LayerElevation[LayerCount] - LayerElevation[0])
        /2/Model.Exaggeration))
        *TanAngle/Max(Z1, Z2); }

      FZScale := ((nearPosition - ZOffset) * TanAngle)
        / (Max(Z1, Z2) + (LayerElevation[LayerCount] - LayerElevation[0])
        * TanAngle * frmGoPhast.Model.Exaggeration);

      FZScale := FZScale * 0.8;
    end;
  end;
  FXPan := 0;
  FYPan := 0;
  FZoomFactor := 1;
  FTheBall.RestoreDefaultOrientation;
  frmGoPhast.glWidModelView.Invalidate;
end;

end.
