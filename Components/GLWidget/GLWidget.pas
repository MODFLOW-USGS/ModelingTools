unit GLWidget;

{$IF CompilerVersion>=23}
{$EXCESSPRECISION OFF}
{$IFEND}

// Cross-platform OpenGL Control and Utils
// by Qingrui Li

// Modified by Richard B. Winston April 21, 2004 to allow TGLWidget to be a CLX
// control under Windows and to use OpenGL12x instead of OpenGLx. OpenGL12x
// is a slightly modified version of the cross-platform OpenGL interface from
// the JEDI-SDL project.

// Modified June 28, 2011 to work with Delphi XE.

interface

// For TGLWidget to be a CLX control under Windows,
// uncomment the next compiler directive prior to installing the component
// AND define 'CLX' in any CLX project that uses the component.
// Another option would be to just define CLX in a CLX project and create
// the component at runtime.
// (Check under "Project|Options|Directories/Conditionals|Conditional defines".)

{Usage hint: Make sure that the parent control of a TGLWidget has
 the ParentDoubleBuffered and ParentBackground properties set to False.}

{ $DEFINE CLX}

{$IFDEF LINUX}
  {$DEFINE CLX}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  // Delphi 2009 & C++ Builder 2009
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
{$ENDIF}

uses
{$IFDEF LINUX}
  XLib, QGraphics, QControls, QForms, Qt,
{$ELSE}
  {$IFDEF CLX}
  Windows, Messages,  QGraphics, QControls, Qt,
  {$ELSE}
  Windows, Messages, Graphics, Controls,
  {$ENDIF}
{$ENDIF}
  Types, SysUtils, Classes, OpenGL12x, Math3D;

{#BACKUP OpenGL12x.pas}
{#BACKUP Math3D.pas}

type
  TTextureOptions = set of (toMipmaps, toRGBA, toCompress);
  TRCOptions = set of (roDepth, roStencil, roAccum, roAlpha);

  TGLWidget = class(TCustomControl)
  private
{$IFDEF MSWINDOWS}
    FDC: Windows.HDC;
    FRC: THandle;
{$ENDIF}
{$IFDEF LINUX}
    FDC: THandle;
    FRC: GLXContext;
{$ENDIF}
    FOptions: TRCOptions;
    FOnRender: TNotifyEvent;
    function NotDesigning: boolean;
  protected
{$IFDEF CLX}
    function WidgetFlags: integer; override;
{$ELSE}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
{$ENDIF}
    procedure Resize; override;
    procedure DoRender;
    procedure Paint; override;
    procedure Activate;
  public
    constructor Create(o: TComponent); override;
    destructor Destroy; override;
    procedure InitGL;
    procedure ScreenProjection;
    procedure PerspectiveProjection(fov, znear, zfar: double);
    function Started: boolean;
{$IFDEF LINUX}
    property RC: GLXContext read FRC;
{$ENDIF}
{$IFDEF MSWINDOWS}
    property RC: THandle read FRC;
{$ENDIF}
    property Canvas;
  published
    property Options: TRCOptions read FOptions write FOptions;
    property Align;
    property Anchors;
    property Constraints;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    // OnRender is called when doing OpenGL rendering
    property OnRender: TNotifyEvent read FOnRender write FOnRender;
  end;

  TBitmapFont = class
  private
    FFont: TFont;
    FListBase: Cardinal;
    FCount: integer;
    FCanvas: TCanvas;
  public
    constructor Create(canvas: TCanvas);
    destructor Destroy; override;
    procedure WriteText(X, Y: single; const t: string; Align: TAlignment = taLeftJustify);
  end;

var
  HighQuality: boolean = true;

{$IFDEF MSWINDOWS}
FActivated: boolean;
function CreateRC(dc: Windows.HDC; opt: TRCOptions): THandle;
procedure MakeCurrent(dc, rc: THandle);
procedure DestroyRC(rc: THandle);
{$ELSE}
// drawable = QWidget_winId(handle)
function CreateRC(drawable: cardinal; opt: TRCOptions): GLXContext;
procedure MakeCurrent(drawable: cardinal; rc: GLXContext);
procedure SwapBuffers(drawable: cardinal);
procedure DestroyRC(rc: GLXContext);
{$ENDIF}

procedure HighQualityScene;
procedure TexImage2D(Width, Height, Format: integer; pData: Pointer; opt: TTextureOptions = []);
procedure BuildTexture(b: TBitmap; var texture: cardinal; opt: TTextureOptions = []);
function RoundUpToPowerOf2(v: integer): integer;
function RoundDownToPowerOf2(v: integer): integer;
function IsPowerOf2(v: integer): boolean;

procedure Register;

implementation

var
  max_texture_max_anistropic: Single = 2.0;

procedure Register;
begin
  RegisterComponents('Qingrui', [TGLWidget]);
end;

procedure Error(const m: string);
begin
  raise Exception.Create(m);
end;

{$IFDEF MSWINDOWS}



function CreateRC(dc: Windows.HDC; opt: TRCOptions): THandle;
var
  OtherOpt: OpenGL12x.TRCOptions;
  ColorBits, StencilBits,
    AccumBits, AuxBuffers, Layer: Integer;
  Palette: Windows.HPALETTE;
begin
  OtherOpt := [opDoubleBuffered];
  ColorBits := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
  if roStencil in opt then StencilBits := 8 else StencilBits := 0;
  if roAccum in opt then AccumBits := 64 else AccumBits := 0;
  AuxBuffers := 0;
  Layer := PFD_MAIN_PLANE;

  result := CreateRenderingContext(dc, OtherOpt, ColorBits, StencilBits,
    AccumBits, AuxBuffers, Layer, Palette);
  wglMakeCurrent(dc, Result);
  LoadOpenGL;
end;

procedure MakeCurrent(dc, rc: THandle);
begin
  if not wglMakeCurrent(dc, rc) then
  begin
    Beep;
    FActivated := False;
  end
  else
  begin
    FActivated := True;
  end;
end;

procedure DestroyRC(rc: THandle);
begin
  if rc = 0 then Exit;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);
end;

{$ELSE} // Linux

function CreateRC(drawable: cardinal; opt: TRCOptions): GLXContext;
var
  display: PDisplay;
  vinfo: PXVisualInfo;
  attr: array[0..23] of integer;
  idx: integer;
  BBool: TGLboolean;
  procedure AddAttr(v: Cardinal);
  begin
    attr[idx] := v;
    inc(idx);
  end;
begin
  display := Application.Display;
  FillChar(attr, sizeof(attr), 0);
  idx := 0;
  AddAttr(GLX_USE_GL);
  AddAttr(GLX_RGBA);
  AddAttr(GLX_DOUBLEBUFFER);
//  AddAttr(GLX_BUFFER_SIZE); AddAttr(32);

  AddAttr(GLX_RED_SIZE); AddAttr(4);
  AddAttr(GLX_GREEN_SIZE); AddAttr(4);
  AddAttr(GLX_BLUE_SIZE); AddAttr(4);

  if roDepth in opt then
  begin
    AddAttr(GLX_DEPTH_SIZE);
    AddAttr(16);
  end;
  if roStencil in opt then
  begin
    AddAttr(GLX_STENCIL_SIZE);
    AddAttr(8);
  end;
  if roAccum in opt then
  begin
    AddAttr(GLX_ACCUM_RED_SIZE);
    AddAttr(16);
    AddAttr(GLX_ACCUM_GREEN_SIZE);
    AddAttr(16);
    AddAttr(GLX_ACCUM_BLUE_SIZE);
    AddAttr(16);
    AddAttr(GLX_ACCUM_ALPHA_SIZE);
    AddAttr(16);
  end;
  if roAlpha in opt then
  begin
    AddAttr(GLX_ALPHA_SIZE);
    AddAttr(8);
  end;
  LoadOpenGL;
  vinfo := glXChooseVisual(display, XDefaultScreen(display), @attr[0]);
  if vinfo = nil then Error('no visual satisfies requirement');
  BBool := True;
  result := glXCreateContext(display, vinfo, nil, BBool);
  XFree(vinfo);
  if result = nil then Error('cannot create gl context');
  if not glXMakeCurrent(display, drawable, result) then
    Error('glXMakeCurrent failed');
end;

procedure MakeCurrent(drawable: cardinal; rc: GLXContext);
begin
  glXMakeCurrent(Application.Display, drawable, rc);
end;

procedure SwapBuffers(drawable: cardinal);
begin
  glXSwapBuffers(Application.Display, drawable);
end;

procedure DestroyRC(rc: GLXContext);
begin
  if rc = nil then Exit;
  glXMakeCurrent(Application.Display, 0, nil); //Corected in 1.1
  glXDestroyContext(Application.Display, rc);
end;

{$ENDIF}

procedure HighQualityScene;
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
//  if GL_EXT_separate_specular_color then
//    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL_EXT, GL_SEPARATE_SPECULAR_COLOR);
  if GL_EXT_texture_filter_anisotropic then
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @max_texture_max_anistropic);
end;

function RoundUpToPowerOf2(v: integer): integer;
begin
  result := 1;
  while v > result do result := result shl 1;
end;

function RoundDownToPowerOf2(v: integer): integer;
begin
  result := 1;
  while v > result do result := result shl 1;
  if v <> result then result := result shr 1;
end;

function IsPowerOf2(v: integer): boolean;
begin
  result := RoundUpToPowerOf2(v) = v;
end;

procedure TexImage2D(Width, Height, Format: integer; pData: Pointer; opt: TTextureOptions);
var
  iformat: Cardinal;
  mem: Pointer;
  newWidth, newHeight, stride: integer;
begin
  if toMipmaps in opt then
  begin
    if HighQuality then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      if GL_EXT_texture_filter_anisotropic then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, max_texture_max_anistropic);
    end
    else begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    end;
  end
  else begin
    if HighQuality then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end
    else begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    end;
  end;
{  if GL_EXT_texture_edge_clamp then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE_EXT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE_EXT);
  end;
}
  if GL_ARB_texture_compression and (toCompress in opt) then
  begin
    if (toRGBA in opt) and ((Format = GL_RGBA) or (Format = GL_BGRA_EXT) or (Format = 4))
    then iformat := GL_COMPRESSED_RGBA_ARB
    else iformat := GL_COMPRESSED_RGB_ARB;
  end
  else begin
    if (toRGBA in opt) and ((Format = GL_RGBA) or (Format = GL_BGRA_EXT) or (Format = 4))
    then iformat := GL_RGBA
    else iformat := GL_RGB;
  end;

  if toMipmaps in opt then
    gluBuild2DMipmaps(GL_TEXTURE_2D, iformat, Width, Height, Format, GL_UNSIGNED_BYTE, pData)
  else if IsPowerOf2(Width) and IsPowerOf2(Height) then
    glTexImage2D(GL_TEXTURE_2D, 0, iformat, Width, Height, 0, Format, GL_UNSIGNED_BYTE, pData)
  else begin
    newWidth := RoundUpToPowerOf2(Width);
    newHeight := RoundUpToPowerOf2(Height);
    case Format of
      GL_RGBA, GL_BGRA_EXT, 4: stride := 4;
      else stride := 3;
    end;
    GetMem(mem, newWidth * newHeight * stride);
    gluScaleImage(Format, Width, Height, GL_UNSIGNED_BYTE, pData, newWidth, newHeight, GL_UNSIGNED_BYTE, mem);
    glTexImage2D(GL_TEXTURE_2D, 0, iformat, newWidth, newHeight, 0, Format, GL_UNSIGNED_BYTE, mem);
    FreeMem(mem);
  end;
end;

procedure BuildTexture(b: TBitmap; var texture: cardinal; opt: TTextureOptions);
var
  f: Cardinal;
{$IFDEF LINUX}
  data: Pointer;
  i: integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  case b.PixelFormat of
    pf32bit: f := GL_BGRA_EXT;
    //pf24bit: f := GL_BGR_EXT;
  else
    b.PixelFormat := pf32bit;
    f := GL_BGRA_EXT;
  end;
  if texture = 0 then glGenTextures(1, @texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  TexImage2D(b.Width, b.Height, f, b.Scanline[b.Height-1], opt);
{$ELSE}
  if b.PixelFormat = pf32bit then
    f := GL_BGRA_EXT
  else begin
    b.PixelFormat := pf32bit;
    f := GL_BGRA_EXT;
    Exclude(opt, toRGBA);
  end;
  if texture = 0 then glGenTextures(1, @texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  GetMem(data, b.Width * b.Height * 4);
  for i := 0 to b.Height - 1 do
    Move(b.Scanline[b.Height-1 - i]^, Pointer(cardinal(data) + cardinal(i*4*b.Width))^, 4*b.Width);
  TexImage2D(b.Width, b.Height, f, data, opt);
  FreeMem(data);
{$ENDIF}
end;

{ TGLWidget }

constructor TGLWidget.Create(o: TComponent);
begin
  inherited;
  FDC := 0;
{$IFDEF LINUX}
  FRC := nil;
{$ENDIF}
{$IFDEF MSWINDOWS}
  FRC := 0;
{$ENDIF}
  ControlStyle := ControlStyle + [csOpaque];
  {$IFDEF Delphi_2009_UP}
  ParentBackground := False;
  {$ENDIF}
end;

destructor TGLWidget.Destroy;
begin
  if NotDesigning and Started then
  begin
    DestroyRC(FRC);
{$IFDEF LINUX}
    FRC := nil;
{$ELSE}
    FRC := 0;
{$ENDIF}
    FDC := 0;
{$IFDEF CLX}
    Canvas.Stop;
{$ENDIF}
  end;
  inherited;
end;

procedure TGLWidget.DoRender;
begin
  try
    Activate;
  {$IFDEF MSWindows}
    if FActivated then
  {$ENDIF}
    FOnRender(self);
  {$IFDEF MSWindows}
    if FActivated then
  {$ENDIF}
    begin
      SwapBuffers(FDC);
    end;
  except
    Visible := False;
  end;
end;

procedure TGLWidget.InitGL;
begin
  if NotDesigning then
  begin
    if Started then Exit;
    InitOpenGL;

{$IFDEF CLX}
    try
      Canvas.Start;
{$ENDIF}
  {$IFDEF MSWINDOWS}
     {$IFDEF CLX}
      FDC := GetDC(QWidget_winId(ChildHandle));
      FRC := CreateRC(FDC, FOptions);
    {$ELSE}
      FDC := Canvas.Handle;
      FRC := CreateRC(FDC, FOptions);
    {$ENDIF}
  {$ELSE}
      FDC := QWidget_winId(ChildHandle);
      FRC := CreateRC(FDC, FOptions);
  {$ENDIF}
{$IFDEF CLX}
    except
      Canvas.Stop;
      raise
    end;
{$ENDIF}
    HighQualityScene;
  end;
end;

function TGLWidget.NotDesigning: boolean;
begin
  result := not (csDesigning in ComponentState);
end;

function TGLWidget.Started: boolean;
begin
{$IFDEF MSWINDOWS}
  result := (FDC <> 0) and (FRC <> 0);
{$ENDIF}
{$IFDEF LINUX}
  result := (FDC <> 0) and (FRC <> nil);
{$ENDIF}
end;

procedure TGLWidget.Paint;
begin
  if NotDesigning and Started and Assigned(FOnRender) then
  begin
    DoRender;
  end
  else if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Pen.Color := clBlack;
      Brush.Color := Color;
      Rectangle(ClientRect);
      MoveTo(0, 0);
      LineTo(Width-1, Height-1);
      MoveTo(Width-1, 0);
      LineTo(0, Height-1);
    end;
  end;
end;

procedure TGLWidget.PerspectiveProjection(fov, znear, zfar: double);
begin
  if not Started then Exit;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(fov, ClientWidth / ClientHeight, znear, zfar);
end;

procedure TGLWidget.ScreenProjection;
begin
  if not Started then Exit;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ClientWidth, 0, ClientHeight, -32768, 32767);
end;

procedure TGLWidget.Resize;
begin
  if NotDesigning then
  begin
    Activate;
  {$IFDEF MSWindows}
    if FActivated then
  {$ENDIF}
    if Started and (ClientWidth > 0) and (ClientHeight > 0) then
    begin
      glViewport(0, 0, ClientWidth, ClientHeight);
    end;
  end;
  Invalidate;
  inherited;
end;

{$IFDEF CLX}

function TGLWidget.WidgetFlags: integer;
begin
  result := inherited WidgetFlags or integer(WidgetFlags_WRepaintNoErase)
    or integer(WidgetFlags_WResizeNoErase);
end;

{$ELSE}

procedure TGLWidget.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := CS_VREDRAW or CS_HREDRAW or CS_OWNDC or CS_DBLCLKS;
  end;
end;

procedure TGLWidget.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

{$ENDIF}

procedure TGLWidget.Activate;
{$IFDEF LINUX}
var
  display: PDisplay;
{$ENDIF}
begin
  try
    if not Started then
      InitGL;
  {$IFDEF MSWINDOWS}
    MakeCurrent(FDC, FRC);
  {$ENDIF}
  {$IFDEF LINUX}
    display := Application.Display;
    glXMakeCurrent(display, FDC, FRC)
  {$ENDIF}
  except
    Visible := False;
  end;
end;


{ TBitmapFont }

constructor TBitmapFont.Create(canvas: TCanvas);
begin
  FCanvas := canvas;
  FFont := TFont.Create;
  FFont.Assign(canvas.Font);
  FCount := 256;
  FListBase := glGenLists(FCount);
{$IFDEF MSWINDOWS}
  {$IFDEF CLX}
  wglUseFontBitmaps(QPainter_handle(Canvas.Handle), 0, FCount, FListBase);
  {$ELSE}
  wglUseFontBitmaps(canvas.Handle, 0, FCount, FListBase);
  {$ENDIF}
{$ELSE}
  glXUseXFont(QFont_handle(FFont.Handle), 0, FCount, FListBase);
{$ENDIF}
end;

destructor TBitmapFont.Destroy;
begin
  FFont.Free;
  glDeleteLists(FListBase, FCount);
  inherited;
end;

procedure TBitmapFont.WriteText(X, Y: single; const t: string; Align: TAlignment);
begin
  glPushAttrib(GL_LIST_BIT);
    glListBase(FListBase);
    if Align = taCenter then
    begin
      FCanvas.Font := FFont;
      X := X - FCanvas.TextWidth(t) / 2;
    end
    else if Align = taRightJustify then
    begin
      FCanvas.Font := FFont;
      X := X - FCanvas.TextWidth(t);
    end;
    glRasterPos2f(X, Y);
    glCallLists(Length(t), GL_BYTE, @t[1]);
  glPopAttrib;
end;

end.
