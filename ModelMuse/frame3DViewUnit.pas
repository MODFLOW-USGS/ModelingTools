{@abstract(The main purpose of @name is to define @link(Tframe3DView) which
  is used to encapsulate the interaction with the 3D view
  of the @link(TPhastModel).)

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frame3DViewUnit;

{$IF CompilerVersion>=23}
{$EXCESSPRECISION OFF}
{$IFEND}
interface

uses
  Winapi.Windows, System.UITypes, SysUtils, Types, Classes, Variants, Graphics,
  Controls, Forms,
  Dialogs, GLWidget, arcball, OpenGL;

type
  {@abstract(@name is used to encapsulate the interaction with the 3D view
    of the @link(TPhastModel).)}
  Tframe3DView = class(TFrame)
    // @name is used to display a 3D view of the @link(TPhastModel).
    // See @link(glWidModelViewMouseDown),
    // @link(glWidModelViewMouseMove),
    // @link(glWidModelViewMouseUp),
    // @link(glWidModelViewRender), and
    // @link(glWidModelViewResize).
    glWidModelView: TGLWidget;
    // @name responds to OnMouseDown events in @link(glWidModelView)
    // by starting a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // @name responds to OnMouseMove events in @link(glWidModelView)
    // by continuing a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // @name responds to OnMouseUp events in @link(glWidModelView)
    // by finishing a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name responds to OnRender events in @link(glWidModelView)
    // by drawing a 3D view of the model in @link(glWidModelView).
    procedure glWidModelViewRender(Sender: TObject);
    // @name responds to OnResize events in @link(glWidModelView)
    // by reinitializing some variables related to @link(glWidModelView).
    procedure glWidModelViewResize(Sender: TObject);
  private
    // @name is set to @True during panning operations.
    FPanning: boolean;
    // @name is used to handle rotation operations.
    FTheBall: TArcBall;
    // If the width of the @link(glWidModelView) is greater tha its height,
    // @name represents how far the center of the @link(glWidModelView)
    // is offset in the X direction from where it would be if it were square.
    FXOffset: integer;
    // @name represents how far the model has been panned in the X direction.
    FXPan: double;
    // @name represents how far the model had been panned in the X direction
    // at the beginning of a panning operations.
    FXPanStart: double;
    // @name is the X-coordinate of the position of the mouse in
    // pixels at the beginning of an operation.
    FXStart: integer;
    // If the height of the @link(glWidModelView) is greater tha its width,
    // @name represents how far the center of the @link(glWidModelView)
    // is offset in the Y direction from where it would be if it were square.
    FYOffset: integer;
    // @name represents how far the model has been panned in the Y direction.
    FYPan: double;
    // @name represents how far the model had been panned in the Y direction
    // at the beginning of a panning operations.
    FYPanStart: double;
    // @name is the Y-coordinate of the position of the mouse in
    // pixels at the beginning of an operation.
    FYStart: integer;
    // @name represents the factor by which the view of the model has
    // been increased or decreased.
    FZoomFactor: double;
    // @name is set to true while zooming in or out.
    FZooming: boolean;
    // @name represents the factor by which the view of the model has
    // been increased or decreased when the model is first viewed.
    FZScale: double;
    // @name stores @link(FZoomFactor) at the beginning of a zooming operation.
    FZStart: double;
    FListsCreated: Boolean;
    FAxesGLIndex: GLuint;
    FAxesCreated: Boolean;
    procedure RecordAxes;
    procedure DrawAxes;
    procedure DrawGrid;
    procedure DrawMesh;
    procedure DrawVectors(Magnification: Double);
    { Private declarations }
  public
    // @name creates an instance of @classname.
    constructor Create(AOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name sets the orientation of the 3D view of model so that
    // it is seen from above with the magnification set up so the whole
    // model can be seen.
    procedure SetDefaultOrientation;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses frmGoPhastUnit, CursorsFoiledAgain, Math, frmColorsUnit,
  PhastModelUnit, AbstractGridUnit, GoPhastTypes,
  VectorDisplayUnit, DrawMeshTypesUnit;

resourcestring
  StrErrorInRenderS = 'The 3D view encountered an error and will be hidden '
  + 'until you restart ModelMuse. The error message was "%s."';

const
  PanFactor = 0.25;
  ZOffset = -50;
  ViewAngle = 45; // in degrees.
  nearPosition = 1;
  farPosition = 1E3;
  ThinLine = 1.0;

{ Tframe3DView }

constructor Tframe3DView.Create(AOwner: TComponent);
begin
  inherited;
  FZoomFactor := 1;
  FTheBall := TArcBall.Create;
  FTheBall.Init(Min(glWidModelView.ClientWidth,
    glWidModelView.ClientHeight) div 2);
end;

destructor Tframe3DView.Destroy;
begin
  FTheBall.Free;
  inherited;
end;

procedure Tframe3DView.SetDefaultOrientation;
var
  Angle: double;
  TanAngle: double;
  Z1, Z2: single;
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  Limits: TGridLimit;
//  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
begin
  Angle := ViewAngle / 2 / 180 * Pi;
  TanAngle := Tan(Angle);
  Grid := frmGoPhast.PhastModel.Grid;
  if (Grid <> nil) and Grid.CanDraw3D then
  begin
    Z1 := (Grid.ColumnPosition[Grid.ColumnCount] - Grid.ColumnPosition[0]) / 2;
    Z2 := (Grid.RowPosition[Grid.RowCount] - Grid.RowPosition[0]) / 2;

    FZScale := ((nearPosition - ZOffset) * TanAngle)
      / (Max(Z1, Z2) +
        Abs(Grid.HighestElevation - Grid.LowestElevation)
      * TanAngle * frmGoPhast.PhastModel.Exaggeration);

    FZScale := FZScale * 0.8;
  end
  else
  begin
//    Mesh := frmGoPhast.PhastModel.Mesh3D;
    DrawMesh := frmGoPhast.PhastModel.DrawMesh;
    if (DrawMesh <> nil) and DrawMesh.CanDraw3D then
    begin
      Limits := DrawMesh.MeshLimits(vdTop, 0);
      Z1 := (Limits.MaxX - Limits.MinX)/2;
      Z2 := (Limits.MaxY - Limits.MinY)/2;
      Limits := DrawMesh.MeshLimits(vdFront, DrawMesh.CrossSection.Angle);

      if Max(Z1, Z2) <> 0 then
      begin
        FZScale := ((nearPosition - ZOffset) * TanAngle)
          / (Max(Z1, Z2) +
            Abs(Limits.MaxZ - Limits.MinZ)
          * TanAngle * frmGoPhast.PhastModel.Exaggeration);
      end
      else
      begin
        FZScale := 1;
      end;

      FZScale := FZScale * 0.8;
    end;
  end;
  FXPan := 0;
  FYPan := 0;
  FZoomFactor := 1;
  FTheBall.RestoreDefaultOrientation;
  glWidModelView.Invalidate;
end;

procedure Tframe3DView.DrawGrid;
var
  ChildModel: TChildModel;
  XMove: Single;
  ChildIndex: Integer;
  YMove: Single;
  ZMove: Single;
  Grid: TCustomModelGrid;
begin
  //   Exit;
  //  glEnable(GL_CULL_FACE);
//  XMove := 0;
//  YMove := 0;
//  ZMove := 0;
  Grid := frmGoPhast.PhastModel.Grid;

  XMove := -(Grid.ColumnPosition[0]
    + Grid.ColumnPosition[Grid.ColumnCount]) / 2;
  YMove := -(Grid.RowPosition[0]
    + Grid.RowPosition[Grid.RowCount]) / 2;
  ZMove := -(Grid.CellElevation[ZeroBasedID(0,
    Grid.RowCount div 2, Grid.ColumnCount div 2)]
    + Grid.CellElevation[ZeroBasedID(Grid.LayerCount,
    Grid.RowCount div 2, Grid.ColumnCount div 2)]) / 2;

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
  glRotatef(Grid.GridAngle / Pi * 180, 0, 0, 1);
  // Respond to "rolling" of the model.
  glMultMatrixf(@FTheBall.Matrix);
  // Use ZScale to scale the model to a size that can be
  // confortably seen.
  // Apply the vertical exaggeration to the model.
  glScalef(FZScale * FZoomFactor, FZScale * FZoomFactor, FZScale
    * FZoomFactor * frmGoPhast.PhastModel.Exaggeration);
  // Move the center of the model to the origin so that all
  // "rolling" is relative to the center of the model.
  glTranslatef(XMove, YMove, ZMove);
  glPushMatrix;
  glEnable(GL_DEPTH_TEST);

  Grid.Draw3D;
  frmGoPhast.PhastModel.PathLines.Draw3D;
  frmGoPhast.PhastModel.TimeSeries.Draw3D;
  frmGoPhast.PhastModel.EndPoints.Draw3D;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.PathLines.Draw3D;
        ChildModel.TimeSeries.Draw3D;
        ChildModel.EndPoints.Draw3D;
      end;
    end;
  end;
  if frmGoPhast.tb3DObjects.Down then
  begin
    glPushMatrix;
    glRotatef(-Grid.GridAngle / Pi * 180, 0, 0, 1);
    frmGoPhast.PhastModel.DrawScreenObjects3D;
    glPopMatrix;
  end;

  DrawVectors(FZScale * FZoomFactor);

  glPopMatrix;
end;

procedure Tframe3DView.DrawAxes;
begin
  if not FAxesCreated then
  begin
    RecordAxes;
  end;


  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  try
    glTranslated(-0.8, -0.8, -0.8);
    glscaled(glWidModelView.ClientHeight/Max(1,glWidModelView.ClientWidth), 1, 1);
    glMultMatrixf(@FTheBall.Matrix);
    glCallList(FAxesGLIndex);
  finally
    glPopMatrix;
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;


end;

procedure Tframe3DView.glWidModelViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not glWidModelView.Started then
  begin
    Exit;
  end;

  if (glWidModelView.Width <= 1) or (glWidModelView.Height <= 1) then
  begin
    Exit;
  end;

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
      glWidModelView.Cursor := crHandFlat;
    end
    else
    begin
      // Rotate
      FTheBall.BeginDrag(X - FXOffset, Y - FYOffset);
      glWidModelView.Invalidate;
    end;
  end
  else if Button = mbRight then
  begin
    // Zoom
    FYStart := Y;
    FZStart := FZoomFactor;
    FZooming := True;
    glWidModelView.Cursor := crZoomByY;
  end;
end;

procedure Tframe3DView.glWidModelViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin

  if not glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > glWidModelView.Height then
    begin
      Y := glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / glWidModelView.Height;
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;
    //ZScale := ZStart*(1-Factor);//*ZFactor;
    glWidModelView.Invalidate;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    glWidModelView.Invalidate;
  end
  else if FTheBall.Dragging then
  begin
    // rotating
    FTheBall.MouseMove(X - FXOffset, Y - FYOffset);
    glWidModelView.Invalidate;
  end;
end;

procedure Tframe3DView.glWidModelViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin
  if not glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > glWidModelView.Height then
    begin
      Y := glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / glWidModelView.Height;
    //ZScale := ZStart*(1-Factor);
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;

    glWidModelView.Invalidate;
    FZooming := False;
    glWidModelView.Cursor := crDefault;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    glWidModelView.Invalidate;
    FPanning := False;
    glWidModelView.Cursor := crDefault;
  end
  else if FTheBall.Dragging then
  begin
    FTheBall.EndDrag(X - FXOffset, Y - FYOffset);
    glWidModelView.Invalidate;
  end;
end;

procedure Tframe3DView.DrawVectors(Magnification: Double);
var
  LocalModel: TPhastModel;
  VelocityVectors: TVectorCollection;
  VItem: TVectorItem;
begin
  Magnification := Magnification*10;
  LocalModel := frmGoPhast.PhastModel;
  LocalModel.MaxVectors.PlotVectors3D(Magnification);
  LocalModel.MidVectors.PlotVectors3D(Magnification);
  LocalModel.MinVectors.PlotVectors3D(Magnification);
  VelocityVectors := LocalModel.VelocityVectors;
  if VelocityVectors.SelectedItem >= 0 then
  begin
    VItem := VelocityVectors.Items[VelocityVectors.SelectedItem] as TVectorItem;
    VItem.Vectors.PlotVectors3D(Magnification);
  end;
end;

procedure Tframe3DView.DrawMesh;
var
  XMove: Single;
  YMove: Single;
  ZMove: Single;
//  Mesh: TSutraMesh3D;
  Limits: TGridLimit;
  Mesh: IDrawMesh;
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glPushMatrix;
  try
//    XMove := 0;
//    YMove := 0;
//    ZMove := 0;

    Mesh := frmGoPhast.PhastModel.DrawMesh;

    Limits := Mesh.MeshLimits(vdTop, 0);
    XMove := -(Limits.MaxX + Limits.MinX)/2;
    YMove := -(Limits.MaxY + Limits.MinY)/2;
    Limits := Mesh.MeshLimits(vdFront, Mesh.CrossSection.Angle);
    ZMove := -(Limits.MaxZ + Limits.MinZ)/2;

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

    // Respond to "rolling" of the model.
    glMultMatrixf(@FTheBall.Matrix);
    // Use ZScale to scale the model to a size that can be
    // confortably seen.
    // Apply the vertical exaggeration to the model.
    glScalef(FZScale * FZoomFactor, FZScale * FZoomFactor, FZScale
      * FZoomFactor * frmGoPhast.PhastModel.Exaggeration);
    // Move the center of the model to the origin so that all
    // "rolling" is relative to the center of the model.
    glTranslatef(XMove, YMove, ZMove);
    glPushMatrix;
    try
      glEnable(GL_DEPTH_TEST);

      Mesh.Draw3D;
      if frmGoPhast.DisvUsed then
      begin
        frmGoPhast.PhastModel.PathLines.Draw3D;
        frmGoPhast.PhastModel.TimeSeries.Draw3D;
        frmGoPhast.PhastModel.EndPoints.Draw3D;
      end;

      DrawVectors(FZScale * FZoomFactor);

    finally
      glPopMatrix;
    end;


  finally
    glPopMatrix;
  end;
end;

procedure Tframe3DView.glWidModelViewRender(Sender: TObject);
var
  errorCode: GLuint;
  light_position: array[0..3] of GLfloat;
  //  Colors: array[0..3] of TGLint;
  //const
  //  XY = 40.0;
  //  light_position : array[0..3] of GLfloat = ( 5.0, 40.0, -4.0, 2.0 );
  //  light_position : array[0..3] of GLfloat = ( XY, XY, 40.0, 0.0 );
  //  light_position2 : array[0..3] of GLfloat = ( -XY, -XY, 40.0, 0.0 );
begin
  try
//    {$IFDEF Win64}
//    glWidModelView.Visible := False;
//    {$ENDIF}
    if not glWidModelView.Started then
    begin
      Exit;
    end;

    if Height <= 2 then
    begin
      Exit;
    end;

    light_position[0] := ColorValues.X;
    light_position[1] := ColorValues.Y;
    light_position[2] := ColorValues.Z;
    light_position[3] := 0;

    // White background
    glClearColor(1.0, 1.0, 1.0, 0.0);

  //  Exit;

    //  glShadeModel(GL_FLAT);
    glShadeModel(GL_SMOOTH);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLightfv(GL_LIGHT0, GL_POSITION, @light_position);

  //  Beep;

  //  Exit;

    if not frmGoPhast.CanDraw then
    begin
      Exit;
    end;

    if (frmGoPhast.PhastModel = nil)
      or (csReading in frmGoPhast.PhastModel.ComponentState) then
    begin
      Exit;
    end;

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glPushMatrix;
    try
      if frmGoPhast.PhastModel.Grid <> nil then
      begin
        if not frmGoPhast.PhastModel.Grid.CanDraw3D then
        begin
          Exit;
        end;
        DrawGrid;
      end
      else if frmGoPhast.PhastModel.DrawMesh <> nil then
      begin
        if not frmGoPhast.PhastModel.DrawMesh.CanDraw3D then
        begin
          Exit;
        end;
        DrawMesh;

      end
      else
      begin
        Exit;
      end;

      glLineWidth(2);
      glPointSize(5);
      glColor3ub(0, 0, 0);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_LIGHTING);
      FTheBall.Render;
      DrawAxes;

      //  glEnable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      errorCode := glGetError;

      if errorCode <> GL_NO_ERROR then
      begin
        Beep;
        glWidModelView.Visible := False;
        Exit;
      end;

    finally
      glPopMatrix;
    end;

    errorCode := glGetError;

    if errorCode <> GL_NO_ERROR then
    begin
//      {$IFDEF DEBUG}
//      if errorCode = GL_INVALID_ENUM then
//      begin
//        ShowMessage('GL_INVALID_ENUM');
//      end
//      else if errorCode = GL_INVALID_VALUE then
//      begin
//        ShowMessage('GL_INVALID_VALUE');
//      end
//      else if errorCode = GL_INVALID_OPERATION then
//      begin
//        ShowMessage('GL_INVALID_OPERATION');
//      end
////      else if errorCode = GL_INVALID_FRAMEBUFFER_OPERATION then
////      begin
////        ShowMessage('GL_INVALID_FRAMEBUFFER_OPERATION');
////      end
//      else if errorCode = GL_OUT_OF_MEMORY then
//      begin
//        ShowMessage('GL_OUT_OF_MEMORY');
//      end
//      else if errorCode = GL_STACK_UNDERFLOW then
//      begin
//        ShowMessage('GL_STACK_UNDERFLOW');
//      end
//      else if errorCode = GL_STACK_OVERFLOW then
//      begin
//        ShowMessage('GL_STACK_OVERFLOW');
//      end
//      else
//      begin
//        ShowMessage('Unknown error');
//      end;
//
//      {$ELSE}
      Beep;
//      {$ENDIF}
    end;
  except
    glWidModelView.Visible := False;
  end;
end;

procedure Tframe3DView.glWidModelViewResize(Sender: TObject);
var
  errorCode: GLuint;
begin
  if not glWidModelView.Visible then
  begin
    Exit;
  end;
  if not glWidModelView.Started then
    Exit;
  if (FTheBall <> nil) and (glWidModelView.ClientHeight > 0) then
  begin
    FTheBall.Init(Min(glWidModelView.ClientWidth, glWidModelView.ClientHeight)
      div 2, glWidModelView.ClientWidth / glWidModelView.ClientHeight);
  end;
  glWidModelView.PerspectiveProjection(ViewAngle, nearPosition, farPosition);

  if glWidModelView.ClientWidth > glWidModelView.ClientHeight then
  begin
    FXOffset := (glWidModelView.ClientWidth
      - glWidModelView.ClientHeight) div 2;
    FYOffset := 0;
  end
  else
  begin
    FXOffset := 0;
    FYOffset := (glWidModelView.ClientHeight
      - glWidModelView.ClientWidth) div 2;
  end;

  errorCode := glGetError;
  if errorCode <> GL_NO_ERROR then
  begin
    glWidModelView.Visible := False;
    Beep;
    MessageDlg(Format(StrErrorInRenderS, [gluErrorString(errorCode)]),
      mtError, [mbOK], 0);
//    raise Exception.Create(Format(StrErrorInRenderS, [gluErrorString(errorCode)]));
  end;
end;

procedure Tframe3DView.RecordAxes;
var
  X, Y, Z: double;
  X2, Y2, Z2: double;
begin
  if not FListsCreated then
  begin
    FAxesGLIndex := glGenLists(1);
    FListsCreated := True;
  end;

  glNewList(FAxesGLIndex, GL_COMPILE);
  try

    glLineWidth(ThinLine);
    glBegin(GL_LINES);
    try
      X := 0.0;
      Y := 0.0;
      Z := 0.0;
      X2 := 0.2;
      Y2 := 0.2;
      Z2 := 0.2;

      glColor3d(0.0, 0.0, 1.0);
      glVertex3d(X, Y, Z);
      glVertex3d(X2, Y, Z);

      glColor3d(0.0, 1.0, 0.0);
      glVertex3d(X, Y, Z);
      glVertex3d(X, Y2, Z);

      glColor3d(1.0, 0.0, 0.0);
      glVertex3d(X, Y, Z);
      glVertex3d(X, Y, Z2);

      glColor3d(0.0, 0.0, 0.0);
    finally
      glEnd;
    end;

  finally
    glEndList;
  end;
  FAxesCreated := True;

end;

end.
