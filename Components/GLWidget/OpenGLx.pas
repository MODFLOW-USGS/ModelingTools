unit OpenGLx;

// Cross-platform OpenGL Interface
// by LI Qingrui

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  LibC, XLib, Types,
{$ENDIF}
  Math3D;

const
{$IFDEF MSWINDOWS}
  opengl32 = 'OpenGL32.DLL';
  glu32 = 'GLU32.DLL';
  glut = 'GLUT32.DLL';
{$ELSE}
  opengl32 = 'libGL.so.1'; // please make link to valid LIBName/Version
  glu32 = 'libGLU.so';
  glut = 'libglut.so';
{$ENDIF}

type
  // ----------- GL types ----------------------------

  GLenum      = UINT;
  TGLenum     = GLenum;
  PGLenum     = ^TGLenum;

  GLboolean   = ByteBool;
  TGLboolean  = GLboolean;
  PGLboolean  = ^TGLboolean;

  GLbitfield  = UINT;
  TGLbitfield = GLbitfield;
  PGLbitfield = ^TGLbitfield;

  GLbyte      = ShortInt;
  TGLbyte     = GLbyte;
  PGLbyte     = ^TGLbyte;

  GLshort     = SmallInt;
  TGLshort    = GLshort;
  PGLshort    = ^TGLshort;

  GLint       = Integer;
  TGLint      = GLint;
  PGLint      = ^TGLint;

  GLsizei     = Integer;
  TGLsizei    = GLsizei;
  PGLsizei    = ^TGLsizei;

  GLubyte     = Byte;
  TGLubyte    = GLubyte;
  PGLubyte    = ^TGLubyte;

  GLushort    = Word;
  TGLushort   = GLushort;
  PGLushort   = ^TGLushort;

  GLuint      = Cardinal;
  TGLuint     = GLuint;
  PGLuint     = ^TGLuint;

  GLfloat     = Single;
  TGLfloat    = GLfloat;
  PGLfloat    = ^TGLfloat;

  GLclampf    = Single;
  TGLclampf   = GLclampf;
  PGLclampf   = ^TGLclampf;

  GLdouble    = Double;
  TGLdouble   = GLdouble;
  PGLdouble   = ^TGLdouble;

  GLclampd    = Double;
  TGLclampd   = GLclampd;
  PGLclampd   = ^TGLclampd;

  // ----------- GLU types ----------------------------

  GLUNurbs = record end;
  TGLUNurbs = GLUNurbs;

  GLUQuadric = record end;
  TGLUQuadric = GLUQuadric;

  GLUTesselator = record end;
  TGLUTesselator = GLUTesselator;

  PGLUNurbs = ^TGLUNurbs;
  PGLUQuadric = ^TGLUQuadric;
  PGLUTesselator = ^TGLUTesselator;

  // backwards compatibility
  GLUNurbsObj = GLUNurbs;
  GLUQuadricObj = GLUQuadric;
  GLUTesselatorObj = GLUTesselator;
  GLUTriangulatorObj = GLUTesselator;

  PGLUNurbsObj = PGLUNurbs;
  PGLUQuadricObj = PGLUQuadric;
  PGLUTesselatorObj = PGLUTesselator;
  PGLUTriangulatorObj = PGLUTesselator;

  // Callback function prototypes
  // GLUQuadricCallback
  TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GLUTessCallback
  TGLUTessBeginProc = procedure(AType: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessEndProc = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessCombineProc = procedure(Coords: PGLdouble; VertexData: Pointer; Weight: PGLfloat; var OutData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  TGLUTessCombineDataProc = procedure(Coords: PGLdouble; VertexData: Pointer; Weight: PGLfloat; var OutData: Pointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GLUNurbsCallback
  TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

const
  // ----------- GL Constants ----------------------------

  // errors
  GL_NO_ERROR                                       = 0;
  GL_INVALID_ENUM                                   = $0500;
  GL_INVALID_VALUE                                  = $0501;
  GL_INVALID_OPERATION                              = $0502;
  GL_STACK_OVERFLOW                                 = $0503;
  GL_STACK_UNDERFLOW                                = $0504;
  GL_OUT_OF_MEMORY                                  = $0505;

  // attribute bits
  GL_CURRENT_BIT                                    = $00000001;
  GL_POINT_BIT                                      = $00000002; 
  GL_LINE_BIT                                       = $00000004; 
  GL_POLYGON_BIT                                    = $00000008; 
  GL_POLYGON_STIPPLE_BIT                            = $00000010;
  GL_PIXEL_MODE_BIT                                 = $00000020; 
  GL_LIGHTING_BIT                                   = $00000040;
  GL_FOG_BIT                                        = $00000080; 
  GL_DEPTH_BUFFER_BIT                               = $00000100; 
  GL_ACCUM_BUFFER_BIT                               = $00000200; 
  GL_STENCIL_BUFFER_BIT                             = $00000400; 
  GL_VIEWPORT_BIT                                   = $00000800; 
  GL_TRANSFORM_BIT                                  = $00001000; 
  GL_ENABLE_BIT                                     = $00002000;
  GL_COLOR_BUFFER_BIT                               = $00004000; 
  GL_HINT_BIT                                       = $00008000; 
  GL_EVAL_BIT                                       = $00010000; 
  GL_LIST_BIT                                       = $00020000; 
  GL_TEXTURE_BIT                                    = $00040000; 
  GL_SCISSOR_BIT                                    = $00080000;
  GL_ALL_ATTRIB_BITS                                = $000FFFFF; 

  // client attribute bits
  GL_CLIENT_PIXEL_STORE_BIT                         = $00000001; 
  GL_CLIENT_VERTEX_ARRAY_BIT                        = $00000002; 
  GL_CLIENT_ALL_ATTRIB_BITS                         = $FFFFFFFF; 

  // boolean values
  GL_FALSE                                          = Boolean(0);
  GL_TRUE                                           = Boolean(1);

  // primitives
  GL_POINTS                                         = $0000; 
  GL_LINES                                          = $0001; 
  GL_LINE_LOOP                                      = $0002; 
  GL_LINE_STRIP                                     = $0003;
  GL_TRIANGLES                                      = $0004; 
  GL_TRIANGLE_STRIP                                 = $0005;
  GL_TRIANGLE_FAN                                   = $0006; 
  GL_QUADS                                          = $0007; 
  GL_QUAD_STRIP                                     = $0008; 
  GL_POLYGON                                        = $0009; 

  // blending
  GL_ZERO                                           = 0;
  GL_ONE                                            = 1; 
  GL_SRC_COLOR                                      = $0300; 
  GL_ONE_MINUS_SRC_COLOR                            = $0301; 
  GL_SRC_ALPHA                                      = $0302; 
  GL_ONE_MINUS_SRC_ALPHA                            = $0303; 
  GL_DST_ALPHA                                      = $0304;
  GL_ONE_MINUS_DST_ALPHA                            = $0305; 
  GL_DST_COLOR                                      = $0306; 
  GL_ONE_MINUS_DST_COLOR                            = $0307; 
  GL_SRC_ALPHA_SATURATE                             = $0308; 
  GL_BLEND_DST                                      = $0BE0; 
  GL_BLEND_SRC                                      = $0BE1; 
  GL_BLEND                                          = $0BE2; 

  // blending (GL 1.2 ARB imaging)
  GL_BLEND_COLOR                                    = $8005;
  GL_CONSTANT_COLOR                                 = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR                       = $8002; 
  GL_CONSTANT_ALPHA                                 = $8003; 
  GL_ONE_MINUS_CONSTANT_ALPHA                       = $8004; 
  GL_FUNC_ADD                                       = $8006; 
  GL_MIN                                            = $8007;
  GL_MAX                                            = $8008; 
  GL_FUNC_SUBTRACT                                  = $800A;
  GL_FUNC_REVERSE_SUBTRACT                          = $800B; 

  // color table GL 1.2 ARB imaging
  GL_COLOR_TABLE                                    = $80D0; 
  GL_POST_CONVOLUTION_COLOR_TABLE                   = $80D1; 
  GL_POST_COLOR_MATRIX_COLOR_TABLE                  = $80D2; 
  GL_PROXY_COLOR_TABLE                              = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE             = $80D4; 
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE            = $80D5; 
  GL_COLOR_TABLE_SCALE                              = $80D6; 
  GL_COLOR_TABLE_BIAS                               = $80D7; 
  GL_COLOR_TABLE_FORMAT                             = $80D8; 
  GL_COLOR_TABLE_WIDTH                              = $80D9;
  GL_COLOR_TABLE_RED_SIZE                           = $80DA; 
  GL_COLOR_TABLE_GREEN_SIZE                         = $80DB; 
  GL_COLOR_TABLE_BLUE_SIZE                          = $80DC; 
  GL_COLOR_TABLE_ALPHA_SIZE                         = $80DD; 
  GL_COLOR_TABLE_LUMINANCE_SIZE                     = $80DE; 
  GL_COLOR_TABLE_INTENSITY_SIZE                     = $80DF; 

  // convolutions GL 1.2 ARB imaging
  GL_CONVOLUTION_1D                                 = $8010; 
  GL_CONVOLUTION_2D                                 = $8011;
  GL_SEPARABLE_2D                                   = $8012;
  GL_CONVOLUTION_BORDER_MODE                        = $8013; 
  GL_CONVOLUTION_FILTER_SCALE                       = $8014; 
  GL_CONVOLUTION_FILTER_BIAS                        = $8015; 
  GL_REDUCE                                         = $8016; 
  GL_CONVOLUTION_FORMAT                             = $8017;
  GL_CONVOLUTION_WIDTH                              = $8018; 
  GL_CONVOLUTION_HEIGHT                             = $8019;
  GL_MAX_CONVOLUTION_WIDTH                          = $801A; 
  GL_MAX_CONVOLUTION_HEIGHT                         = $801B; 
  GL_POST_CONVOLUTION_RED_SCALE                     = $801C; 
  GL_POST_CONVOLUTION_GREEN_SCALE                   = $801D; 
  GL_POST_CONVOLUTION_BLUE_SCALE                    = $801E; 
  GL_POST_CONVOLUTION_ALPHA_SCALE                   = $801F; 
  GL_POST_CONVOLUTION_RED_BIAS                      = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS                    = $8021; 
  GL_POST_CONVOLUTION_BLUE_BIAS                     = $8022; 
  GL_POST_CONVOLUTION_ALPHA_BIAS                    = $8023; 

  // histogram GL 1.2 ARB imaging
  GL_HISTOGRAM                                      = $8024;
  GL_PROXY_HISTOGRAM                                = $8025; 
  GL_HISTOGRAM_WIDTH                                = $8026; 
  GL_HISTOGRAM_FORMAT                               = $8027; 
  GL_HISTOGRAM_RED_SIZE                             = $8028; 
  GL_HISTOGRAM_GREEN_SIZE                           = $8029; 
  GL_HISTOGRAM_BLUE_SIZE                            = $802A; 
  GL_HISTOGRAM_ALPHA_SIZE                           = $802B; 
  GL_HISTOGRAM_LUMINANCE_SIZE                       = $802C;
  GL_HISTOGRAM_SINK                                 = $802D; 
  GL_MINMAX                                         = $802E;
  GL_MINMAX_FORMAT                                  = $802F;
  GL_MINMAX_SINK                                    = $8030; 

  // buffers
  GL_NONE                                           = 0; 
  GL_FRONT_LEFT                                     = $0400;
  GL_FRONT_RIGHT                                    = $0401; 
  GL_BACK_LEFT                                      = $0402;
  GL_BACK_RIGHT                                     = $0403; 
  GL_FRONT                                          = $0404; 
  GL_BACK                                           = $0405; 
  GL_LEFT                                           = $0406; 
  GL_RIGHT                                          = $0407; 
  GL_FRONT_AND_BACK                                 = $0408; 
  GL_AUX0                                           = $0409;
  GL_AUX1                                           = $040A; 
  GL_AUX2                                           = $040B; 
  GL_AUX3                                           = $040C; 
  GL_AUX_BUFFERS                                    = $0C00; 
  GL_DRAW_BUFFER                                    = $0C01; 
  GL_READ_BUFFER                                    = $0C02;
  GL_DOUBLEBUFFER                                   = $0C32; 
  GL_STEREO                                         = $0C33; 

  // depth buffer
  GL_DEPTH_RANGE                                    = $0B70; 
  GL_DEPTH_TEST                                     = $0B71; 
  GL_DEPTH_WRITEMASK                                = $0B72; 
  GL_DEPTH_CLEAR_VALUE                              = $0B73;
  GL_DEPTH_FUNC                                     = $0B74; 
  GL_NEVER                                          = $0200;
  GL_LESS                                           = $0201;
  GL_EQUAL                                          = $0202; 
  GL_LEQUAL                                         = $0203; 
  GL_GREATER                                        = $0204; 
  GL_NOTEQUAL                                       = $0205; 
  GL_GEQUAL                                         = $0206;
  GL_ALWAYS                                         = $0207; 

  // accumulation buffer
  GL_ACCUM                                          = $0100; 
  GL_LOAD                                           = $0101; 
  GL_RETURN                                         = $0102; 
  GL_MULT                                           = $0103; 
  GL_ADD                                            = $0104; 
  GL_ACCUM_CLEAR_VALUE                              = $0B80;

  // feedback buffer
  GL_FEEDBACK_BUFFER_POINTER                        = $0DF0; 
  GL_FEEDBACK_BUFFER_SIZE                           = $0DF1; 
  GL_FEEDBACK_BUFFER_TYPE                           = $0DF2; 

  // feedback types
  GL_2D                                             = $0600; 
  GL_3D                                             = $0601; 
  GL_3D_COLOR                                       = $0602; 
  GL_3D_COLOR_TEXTURE                               = $0603; 
  GL_4D_COLOR_TEXTURE                               = $0604; 

  // feedback tokens
  GL_PASS_THROUGH_TOKEN                             = $0700; 
  GL_POINT_TOKEN                                    = $0701;
  GL_LINE_TOKEN                                     = $0702;
  GL_POLYGON_TOKEN                                  = $0703; 
  GL_BITMAP_TOKEN                                   = $0704; 
  GL_DRAW_PIXEL_TOKEN                               = $0705; 
  GL_COPY_PIXEL_TOKEN                               = $0706; 
  GL_LINE_RESET_TOKEN                               = $0707;

  // fog
  GL_EXP                                            = $0800; 
  GL_EXP2                                           = $0801; 
  GL_FOG                                            = $0B60; 
  GL_FOG_INDEX                                      = $0B61; 
  GL_FOG_DENSITY                                    = $0B62; 
  GL_FOG_START                                      = $0B63; 
  GL_FOG_END                                        = $0B64;
  GL_FOG_MODE                                       = $0B65; 
  GL_FOG_COLOR                                      = $0B66; 

  // pixel mode, transfer
  GL_PIXEL_MAP_I_TO_I                               = $0C70; 
  GL_PIXEL_MAP_S_TO_S                               = $0C71;
  GL_PIXEL_MAP_I_TO_R                               = $0C72; 
  GL_PIXEL_MAP_I_TO_G                               = $0C73; 
  GL_PIXEL_MAP_I_TO_B                               = $0C74; 
  GL_PIXEL_MAP_I_TO_A                               = $0C75; 
  GL_PIXEL_MAP_R_TO_R                               = $0C76; 
  GL_PIXEL_MAP_G_TO_G                               = $0C77; 
  GL_PIXEL_MAP_B_TO_B                               = $0C78; 
  GL_PIXEL_MAP_A_TO_A                               = $0C79;

  // vertex arrays
  GL_VERTEX_ARRAY_POINTER                           = $808E;
  GL_NORMAL_ARRAY_POINTER                           = $808F; 
  GL_COLOR_ARRAY_POINTER                            = $8090; 
  GL_INDEX_ARRAY_POINTER                            = $8091; 
  GL_TEXTURE_COORD_ARRAY_POINTER                    = $8092; 
  GL_EDGE_FLAG_ARRAY_POINTER                        = $8093;

  // stenciling
  GL_STENCIL_TEST                                   = $0B90; 
  GL_STENCIL_CLEAR_VALUE                            = $0B91; 
  GL_STENCIL_FUNC                                   = $0B92; 
  GL_STENCIL_VALUE_MASK                             = $0B93; 
  GL_STENCIL_FAIL                                   = $0B94; 
  GL_STENCIL_PASS_DEPTH_FAIL                        = $0B95; 
  GL_STENCIL_PASS_DEPTH_PASS                        = $0B96;
  GL_STENCIL_REF                                    = $0B97; 
  GL_STENCIL_WRITEMASK                              = $0B98; 
  GL_KEEP                                           = $1E00; 
  GL_REPLACE                                        = $1E01; 
  GL_INCR                                           = $1E02; 
  GL_DECR                                           = $1E03;

  // color material
  GL_COLOR_MATERIAL_FACE                            = $0B55; 
  GL_COLOR_MATERIAL_PARAMETER                       = $0B56; 
  GL_COLOR_MATERIAL                                 = $0B57; 

  // points
  GL_POINT_SMOOTH                                   = $0B10;
  GL_POINT_SIZE                                     = $0B11; 
  GL_POINT_SIZE_RANGE                               = $0B12;
  GL_POINT_SIZE_GRANULARITY                         = $0B13;

  // lines
  GL_LINE_SMOOTH                                    = $0B20; 
  GL_LINE_WIDTH                                     = $0B21; 
  GL_LINE_WIDTH_RANGE                               = $0B22;
  GL_LINE_WIDTH_GRANULARITY                         = $0B23; 
  GL_LINE_STIPPLE                                   = $0B24;
  GL_LINE_STIPPLE_PATTERN                           = $0B25; 
  GL_LINE_STIPPLE_REPEAT                            = $0B26; 

  // polygons
  GL_POLYGON_MODE                                   = $0B40; 
  GL_POLYGON_SMOOTH                                 = $0B41; 
  GL_POLYGON_STIPPLE                                = $0B42;
  GL_EDGE_FLAG                                      = $0B43; 
  GL_CULL_FACE                                      = $0B44; 
  GL_CULL_FACE_MODE                                 = $0B45; 
  GL_FRONT_FACE                                     = $0B46; 
  GL_CW                                             = $0900; 
  GL_CCW                                            = $0901;
  GL_POINT                                          = $1B00; 
  GL_LINE                                           = $1B01; 
  GL_FILL                                           = $1B02; 

  // display lists
  GL_LIST_MODE                                      = $0B30; 
  GL_LIST_BASE                                      = $0B32; 
  GL_LIST_INDEX                                     = $0B33;
  GL_COMPILE                                        = $1300; 
  GL_COMPILE_AND_EXECUTE                            = $1301;

  // lighting
  GL_LIGHTING                                       = $0B50; 
  GL_LIGHT_MODEL_LOCAL_VIEWER                       = $0B51; 
  GL_LIGHT_MODEL_TWO_SIDE                           = $0B52; 
  GL_LIGHT_MODEL_AMBIENT                            = $0B53;
  GL_LIGHT_MODEL_COLOR_CONTROL                      = $81F8; // GL 1.2
  GL_SHADE_MODEL                                    = $0B54;
  GL_NORMALIZE                                      = $0BA1; 
  GL_AMBIENT                                        = $1200; 
  GL_DIFFUSE                                        = $1201; 
  GL_SPECULAR                                       = $1202; 
  GL_POSITION                                       = $1203; 
  GL_SPOT_DIRECTION                                 = $1204; 
  GL_SPOT_EXPONENT                                  = $1205;
  GL_SPOT_CUTOFF                                    = $1206; 
  GL_CONSTANT_ATTENUATION                           = $1207; 
  GL_LINEAR_ATTENUATION                             = $1208; 
  GL_QUADRATIC_ATTENUATION                          = $1209; 
  GL_EMISSION                                       = $1600; 
  GL_SHININESS                                      = $1601;
  GL_AMBIENT_AND_DIFFUSE                            = $1602; 
  GL_COLOR_INDEXES                                  = $1603; 
  GL_FLAT                                           = $1D00; 
  GL_SMOOTH                                         = $1D01; 
  GL_LIGHT0                                         = $4000; 
  GL_LIGHT1                                         = $4001; 
  GL_LIGHT2                                         = $4002; 
  GL_LIGHT3                                         = $4003;
  GL_LIGHT4                                         = $4004; 
  GL_LIGHT5                                         = $4005;
  GL_LIGHT6                                         = $4006;
  GL_LIGHT7                                         = $4007;

  // matrix modes
  GL_MATRIX_MODE                                    = $0BA0; 
  GL_MODELVIEW                                      = $1700;
  GL_PROJECTION                                     = $1701; 
  GL_TEXTURE                                        = $1702;

  // gets
  GL_CURRENT_COLOR                                  = $0B00; 
  GL_CURRENT_INDEX                                  = $0B01; 
  GL_CURRENT_NORMAL                                 = $0B02; 
  GL_CURRENT_TEXTURE_COORDS                         = $0B03; 
  GL_CURRENT_RASTER_COLOR                           = $0B04;
  GL_CURRENT_RASTER_INDEX                           = $0B05; 
  GL_CURRENT_RASTER_TEXTURE_COORDS                  = $0B06; 
  GL_CURRENT_RASTER_POSITION                        = $0B07; 
  GL_CURRENT_RASTER_POSITION_VALID                  = $0B08; 
  GL_CURRENT_RASTER_DISTANCE                        = $0B09; 
  GL_MAX_LIST_NESTING                               = $0B31;
  GL_VIEWPORT                                       = $0BA2; 
  GL_MODELVIEW_STACK_DEPTH                          = $0BA3; 
  GL_PROJECTION_STACK_DEPTH                         = $0BA4; 
  GL_TEXTURE_STACK_DEPTH                            = $0BA5; 
  GL_MODELVIEW_MATRIX                               = $0BA6; 
  GL_PROJECTION_MATRIX                              = $0BA7; 
  GL_TEXTURE_MATRIX                                 = $0BA8; 
  GL_ATTRIB_STACK_DEPTH                             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH                      = $0BB1; 

  GL_SINGLE_COLOR                                   = $81F9; // GL 1.2
  GL_SEPARATE_SPECULAR_COLOR                        = $81FA; // GL 1.2

  // alpha testing
  GL_ALPHA_TEST                                     = $0BC0; 
  GL_ALPHA_TEST_FUNC                                = $0BC1;
  GL_ALPHA_TEST_REF                                 = $0BC2; 

  GL_LOGIC_OP_MODE                                  = $0BF0; 
  GL_INDEX_LOGIC_OP                                 = $0BF1; 
  GL_LOGIC_OP                                       = $0BF1; 
  GL_COLOR_LOGIC_OP                                 = $0BF2; 
  GL_SCISSOR_BOX                                    = $0C10; 
  GL_SCISSOR_TEST                                   = $0C11; 
  GL_INDEX_CLEAR_VALUE                              = $0C20;
  GL_INDEX_WRITEMASK                                = $0C21; 
  GL_COLOR_CLEAR_VALUE                              = $0C22; 
  GL_COLOR_WRITEMASK                                = $0C23; 
  GL_INDEX_MODE                                     = $0C30; 
  GL_RGBA_MODE                                      = $0C31; 
  GL_RENDER_MODE                                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT                    = $0C50; 
  GL_POINT_SMOOTH_HINT                              = $0C51; 
  GL_LINE_SMOOTH_HINT                               = $0C52; 
  GL_POLYGON_SMOOTH_HINT                            = $0C53; 
  GL_FOG_HINT                                       = $0C54; 
  GL_TEXTURE_GEN_S                                  = $0C60; 
  GL_TEXTURE_GEN_T                                  = $0C61; 
  GL_TEXTURE_GEN_R                                  = $0C62;
  GL_TEXTURE_GEN_Q                                  = $0C63; 
  GL_PIXEL_MAP_I_TO_I_SIZE                          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE                          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE                          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE                          = $0CB3; 
  GL_PIXEL_MAP_I_TO_B_SIZE                          = $0CB4; 
  GL_PIXEL_MAP_I_TO_A_SIZE                          = $0CB5; 
  GL_PIXEL_MAP_R_TO_R_SIZE                          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE                          = $0CB7; 
  GL_PIXEL_MAP_B_TO_B_SIZE                          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE                          = $0CB9; 
  GL_UNPACK_SWAP_BYTES                              = $0CF0; 
  GL_UNPACK_LSB_FIRST                               = $0CF1; 
  GL_UNPACK_ROW_LENGTH                              = $0CF2; 
  GL_UNPACK_SKIP_ROWS                               = $0CF3; 
  GL_UNPACK_SKIP_PIXELS                             = $0CF4; 
  GL_UNPACK_ALIGNMENT                               = $0CF5;
  GL_PACK_SWAP_BYTES                                = $0D00; 
  GL_PACK_LSB_FIRST                                 = $0D01; 
  GL_PACK_ROW_LENGTH                                = $0D02; 
  GL_PACK_SKIP_ROWS                                 = $0D03; 
  GL_PACK_SKIP_PIXELS                               = $0D04; 
  GL_PACK_ALIGNMENT                                 = $0D05;
  GL_PACK_SKIP_IMAGES                               = $806B; // GL 1.2
  GL_PACK_IMAGE_HEIGHT                              = $806C; // GL 1.2
  GL_UNPACK_SKIP_IMAGES                             = $806D; // GL 1.2
  GL_UNPACK_IMAGE_HEIGHT                            = $806E; // GL 1.2
  GL_MAP_COLOR                                      = $0D10; 
  GL_MAP_STENCIL                                    = $0D11; 
  GL_INDEX_SHIFT                                    = $0D12; 
  GL_INDEX_OFFSET                                   = $0D13;
  GL_RED_SCALE                                      = $0D14; 
  GL_RED_BIAS                                       = $0D15;
  GL_ZOOM_X                                         = $0D16;
  GL_ZOOM_Y                                         = $0D17;
  GL_GREEN_SCALE                                    = $0D18; 
  GL_GREEN_BIAS                                     = $0D19; 
  GL_BLUE_SCALE                                     = $0D1A; 
  GL_BLUE_BIAS                                      = $0D1B;
  GL_ALPHA_SCALE                                    = $0D1C; 
  GL_ALPHA_BIAS                                     = $0D1D;
  GL_DEPTH_SCALE                                    = $0D1E; 
  GL_DEPTH_BIAS                                     = $0D1F; 
  GL_MAX_EVAL_ORDER                                 = $0D30; 
  GL_MAX_LIGHTS                                     = $0D31; 
  GL_MAX_CLIP_PLANES                                = $0D32; 
  GL_MAX_TEXTURE_SIZE                               = $0D33; 
  GL_MAX_3D_TEXTURE_SIZE                            = $8073; // GL 1.2
  GL_MAX_PIXEL_MAP_TABLE                            = $0D34; 
  GL_MAX_ATTRIB_STACK_DEPTH                         = $0D35; 
  GL_MAX_MODELVIEW_STACK_DEPTH                      = $0D36; 
  GL_MAX_NAME_STACK_DEPTH                           = $0D37; 
  GL_MAX_PROJECTION_STACK_DEPTH                     = $0D38; 
  GL_MAX_TEXTURE_STACK_DEPTH                        = $0D39;
  GL_MAX_VIEWPORT_DIMS                              = $0D3A; 
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH                  = $0D3B; 
  GL_MAX_ELEMENTS_VERTICES                          = $80E8; // GL 1.2
  GL_MAX_ELEMENTS_INDICES                           = $80E9; // GL 1.2  
  GL_RESCALE_NORMAL                                 = $803A; // GL 1.2
  GL_SUBPIXEL_BITS                                  = $0D50; 
  GL_INDEX_BITS                                     = $0D51; 
  GL_RED_BITS                                       = $0D52;
  GL_GREEN_BITS                                     = $0D53; 
  GL_BLUE_BITS                                      = $0D54;
  GL_ALPHA_BITS                                     = $0D55;
  GL_DEPTH_BITS                                     = $0D56;
  GL_STENCIL_BITS                                   = $0D57; 
  GL_ACCUM_RED_BITS                                 = $0D58; 
  GL_ACCUM_GREEN_BITS                               = $0D59; 
  GL_ACCUM_BLUE_BITS                                = $0D5A;
  GL_ACCUM_ALPHA_BITS                               = $0D5B; 
  GL_NAME_STACK_DEPTH                               = $0D70;
  GL_AUTO_NORMAL                                    = $0D80; 
  GL_MAP1_COLOR_4                                   = $0D90; 
  GL_MAP1_INDEX                                     = $0D91; 
  GL_MAP1_NORMAL                                    = $0D92; 
  GL_MAP1_TEXTURE_COORD_1                           = $0D93; 
  GL_MAP1_TEXTURE_COORD_2                           = $0D94; 
  GL_MAP1_TEXTURE_COORD_3                           = $0D95;
  GL_MAP1_TEXTURE_COORD_4                           = $0D96; 
  GL_MAP1_VERTEX_3                                  = $0D97; 
  GL_MAP1_VERTEX_4                                  = $0D98; 
  GL_MAP2_COLOR_4                                   = $0DB0; 
  GL_MAP2_INDEX                                     = $0DB1; 
  GL_MAP2_NORMAL                                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1                           = $0DB3; 
  GL_MAP2_TEXTURE_COORD_2                           = $0DB4; 
  GL_MAP2_TEXTURE_COORD_3                           = $0DB5; 
  GL_MAP2_TEXTURE_COORD_4                           = $0DB6; 
  GL_MAP2_VERTEX_3                                  = $0DB7; 
  GL_MAP2_VERTEX_4                                  = $0DB8; 
  GL_MAP1_GRID_DOMAIN                               = $0DD0; 
  GL_MAP1_GRID_SEGMENTS                             = $0DD1;
  GL_MAP2_GRID_DOMAIN                               = $0DD2; 
  GL_MAP2_GRID_SEGMENTS                             = $0DD3;
  GL_TEXTURE_1D                                     = $0DE0;
  GL_TEXTURE_2D                                     = $0DE1;
  GL_TEXTURE_3D                                     = $806F; // GL 1.2
  GL_SELECTION_BUFFER_POINTER                       = $0DF3; 
  GL_SELECTION_BUFFER_SIZE                          = $0DF4; 
  GL_POLYGON_OFFSET_UNITS                           = $2A00;
  GL_POLYGON_OFFSET_POINT                           = $2A01; 
  GL_POLYGON_OFFSET_LINE                            = $2A02;
  GL_POLYGON_OFFSET_FILL                            = $8037; 
  GL_POLYGON_OFFSET_FACTOR                          = $8038; 
  GL_TEXTURE_BINDING_1D                             = $8068; 
  GL_TEXTURE_BINDING_2D                             = $8069; 
  GL_VERTEX_ARRAY                                   = $8074; 
  GL_NORMAL_ARRAY                                   = $8075; 
  GL_COLOR_ARRAY                                    = $8076;
  GL_INDEX_ARRAY                                    = $8077; 
  GL_TEXTURE_COORD_ARRAY                            = $8078; 
  GL_EDGE_FLAG_ARRAY                                = $8079; 
  GL_VERTEX_ARRAY_SIZE                              = $807A; 
  GL_VERTEX_ARRAY_TYPE                              = $807B; 
  GL_VERTEX_ARRAY_STRIDE                            = $807C;
  GL_NORMAL_ARRAY_TYPE                              = $807E; 
  GL_NORMAL_ARRAY_STRIDE                            = $807F; 
  GL_COLOR_ARRAY_SIZE                               = $8081; 
  GL_COLOR_ARRAY_TYPE                               = $8082; 
  GL_COLOR_ARRAY_STRIDE                             = $8083; 
  GL_INDEX_ARRAY_TYPE                               = $8085; 
  GL_INDEX_ARRAY_STRIDE                             = $8086; 
  GL_TEXTURE_COORD_ARRAY_SIZE                       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE                       = $8089; 
  GL_TEXTURE_COORD_ARRAY_STRIDE                     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE                         = $808C;
  GL_COLOR_MATRIX                                   = $80B1; // GL 1.2 ARB imaging
  GL_COLOR_MATRIX_STACK_DEPTH                       = $80B2; // GL 1.2 ARB imaging
  GL_MAX_COLOR_MATRIX_STACK_DEPTH                   = $80B3; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_RED_SCALE                    = $80B4; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_GREEN_SCALE                  = $80B5; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_BLUE_SCALE                   = $80B6; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_ALPHA_SCALE                  = $80B7; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_RED_BIAS                     = $80B8; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_GREEN_BIAS                   = $80B9; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_BLUE_BIAS                    = $80BA; // GL 1.2 ARB imaging
  GL_POST_COLOR_MATRIX_ALPHA_BIAS                   = $80BB; // GL 1.2 ARB imaging

  // evaluators
  GL_COEFF                                          = $0A00;
  GL_ORDER                                          = $0A01; 
  GL_DOMAIN                                         = $0A02; 
  
  // texture mapping
  GL_TEXTURE_WIDTH                                  = $1000; 
  GL_TEXTURE_HEIGHT                                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT                        = $1003; 
  GL_TEXTURE_COMPONENTS                             = $1003; 
  GL_TEXTURE_BORDER_COLOR                           = $1004; 
  GL_TEXTURE_BORDER                                 = $1005; 
  GL_TEXTURE_RED_SIZE                               = $805C; 
  GL_TEXTURE_GREEN_SIZE                             = $805D; 
  GL_TEXTURE_BLUE_SIZE                              = $805E; 
  GL_TEXTURE_ALPHA_SIZE                             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE                         = $8060; 
  GL_TEXTURE_INTENSITY_SIZE                         = $8061;
  GL_TEXTURE_PRIORITY                               = $8066;
  GL_TEXTURE_RESIDENT                               = $8067;
  GL_BGR                                            = $80E0; // v 1.2
  GL_BGRA                                           = $80E1; // v 1.2
  GL_S                                              = $2000; 
  GL_T                                              = $2001;
  GL_R                                              = $2002; 
  GL_Q                                              = $2003;
  GL_MODULATE                                       = $2100; 
  GL_DECAL                                          = $2101; 
  GL_TEXTURE_ENV_MODE                               = $2200; 
  GL_TEXTURE_ENV_COLOR                              = $2201; 
  GL_TEXTURE_ENV                                    = $2300; 
  GL_EYE_LINEAR                                     = $2400; 
  GL_OBJECT_LINEAR                                  = $2401;
  GL_SPHERE_MAP                                     = $2402; 
  GL_TEXTURE_GEN_MODE                               = $2500; 
  GL_OBJECT_PLANE                                   = $2501; 
  GL_EYE_PLANE                                      = $2502; 
  GL_NEAREST                                        = $2600; 
  GL_LINEAR                                         = $2601;
  GL_NEAREST_MIPMAP_NEAREST                         = $2700; 
  GL_LINEAR_MIPMAP_NEAREST                          = $2701; 
  GL_NEAREST_MIPMAP_LINEAR                          = $2702; 
  GL_LINEAR_MIPMAP_LINEAR                           = $2703; 
  GL_TEXTURE_MAG_FILTER                             = $2800; 
  GL_TEXTURE_MIN_FILTER                             = $2801; 
  GL_TEXTURE_WRAP_R                                 = $8072; // GL 1.2
  GL_TEXTURE_WRAP_S                                 = $2802;
  GL_TEXTURE_WRAP_T                                 = $2803; 
  GL_CLAMP_TO_EDGE                                  = $812F; // GL 1.2
  GL_TEXTURE_MIN_LOD                                = $813A; // GL 1.2
  GL_TEXTURE_MAX_LOD                                = $813B; // GL 1.2
  GL_TEXTURE_BASE_LEVEL                             = $813C; // GL 1.2
  GL_TEXTURE_MAX_LEVEL                              = $813D; // GL 1.2
  GL_TEXTURE_DEPTH                                  = $8071; // GL 1.2
  GL_PROXY_TEXTURE_1D                               = $8063;
  GL_PROXY_TEXTURE_2D                               = $8064; 
  GL_PROXY_TEXTURE_3D                               = $8070; // GL 1.2
  GL_CLAMP                                          = $2900; 
  GL_REPEAT                                         = $2901; 

  // hints
  GL_DONT_CARE                                      = $1100; 
  GL_FASTEST                                        = $1101; 
  GL_NICEST                                         = $1102;

  // data types
  GL_BYTE                                           = $1400; 
  GL_UNSIGNED_BYTE                                  = $1401; 
  GL_SHORT                                          = $1402; 
  GL_UNSIGNED_SHORT                                 = $1403;
  GL_INT                                            = $1404; 
  GL_UNSIGNED_INT                                   = $1405; 
  GL_FLOAT                                          = $1406; 
  GL_2_BYTES                                        = $1407; 
  GL_3_BYTES                                        = $1408; 
  GL_4_BYTES                                        = $1409; 
  GL_DOUBLE                                         = $140A; 
  GL_DOUBLE_EXT                                     = $140A;

  // logic operations
  GL_CLEAR                                          = $1500;
  GL_AND                                            = $1501;
  GL_AND_REVERSE                                    = $1502; 
  GL_COPY                                           = $1503; 
  GL_AND_INVERTED                                   = $1504; 
  GL_NOOP                                           = $1505;
  GL_XOR                                            = $1506; 
  GL_OR                                             = $1507;
  GL_NOR                                            = $1508; 
  GL_EQUIV                                          = $1509; 
  GL_INVERT                                         = $150A; 
  GL_OR_REVERSE                                     = $150B; 
  GL_COPY_INVERTED                                  = $150C; 
  GL_OR_INVERTED                                    = $150D; 
  GL_NAND                                           = $150E;
  GL_SET                                            = $150F; 

  // PixelCopyType
  GL_COLOR                                          = $1800; 
  GL_DEPTH                                          = $1801; 
  GL_STENCIL                                        = $1802;

  // pixel formats
  GL_COLOR_INDEX                                    = $1900; 
  GL_STENCIL_INDEX                                  = $1901; 
  GL_DEPTH_COMPONENT                                = $1902; 
  GL_RED                                            = $1903; 
  GL_GREEN                                          = $1904; 
  GL_BLUE                                           = $1905;
  GL_ALPHA                                          = $1906; 
  GL_RGB                                            = $1907;
  GL_RGBA                                           = $1908;
  GL_LUMINANCE                                      = $1909;
  GL_LUMINANCE_ALPHA                                = $190A; 

  // pixel type
  GL_BITMAP                                         = $1A00;

  // rendering modes
  GL_RENDER                                         = $1C00; 
  GL_FEEDBACK                                       = $1C01; 
  GL_SELECT                                         = $1C02; 

  // implementation strings
  GL_VENDOR                                         = $1F00; 
  GL_RENDERER                                       = $1F01;
  GL_VERSION                                        = $1F02; 
  GL_EXTENSIONS                                     = $1F03; 

  // pixel formats
  GL_R3_G3_B2                                       = $2A10; 
  GL_ALPHA4                                         = $803B;
  GL_ALPHA8                                         = $803C; 
  GL_ALPHA12                                        = $803D; 
  GL_ALPHA16                                        = $803E; 
  GL_LUMINANCE4                                     = $803F; 
  GL_LUMINANCE8                                     = $8040; 
  GL_LUMINANCE12                                    = $8041; 
  GL_LUMINANCE16                                    = $8042; 
  GL_LUMINANCE4_ALPHA4                              = $8043;
  GL_LUMINANCE6_ALPHA2                              = $8044; 
  GL_LUMINANCE8_ALPHA8                              = $8045;
  GL_LUMINANCE12_ALPHA4                             = $8046;
  GL_LUMINANCE12_ALPHA12                            = $8047;
  GL_LUMINANCE16_ALPHA16                            = $8048; 
  GL_INTENSITY                                      = $8049; 
  GL_INTENSITY4                                     = $804A; 
  GL_INTENSITY8                                     = $804B;
  GL_INTENSITY12                                    = $804C; 
  GL_INTENSITY16                                    = $804D;
  GL_RGB4                                           = $804F; 
  GL_RGB5                                           = $8050; 
  GL_RGB8                                           = $8051; 
  GL_RGB10                                          = $8052; 
  GL_RGB12                                          = $8053; 
  GL_RGB16                                          = $8054; 
  GL_RGBA2                                          = $8055;
  GL_RGBA4                                          = $8056;
  GL_RGB5_A1                                        = $8057;
  GL_RGBA8                                          = $8058;
  GL_RGB10_A2                                       = $8059;
  GL_RGBA12                                         = $805A;
  GL_RGBA16                                         = $805B;
  UNSIGNED_BYTE_3_3_2                               = $8032; // GL 1.2
  UNSIGNED_BYTE_2_3_3_REV                           = $8362; // GL 1.2
  UNSIGNED_SHORT_5_6_5                              = $8363; // GL 1.2
  UNSIGNED_SHORT_5_6_5_REV                          = $8364; // GL 1.2
  UNSIGNED_SHORT_4_4_4_4                            = $8033; // GL 1.2
  UNSIGNED_SHORT_4_4_4_4_REV                        = $8365; // GL 1.2
  UNSIGNED_SHORT_5_5_5_1                            = $8034; // GL 1.2
  UNSIGNED_SHORT_1_5_5_5_REV                        = $8366; // GL 1.2
  UNSIGNED_INT_8_8_8_8                              = $8035; // GL 1.2
  UNSIGNED_INT_8_8_8_8_REV                          = $8367; // GL 1.2
  UNSIGNED_INT_10_10_10_2                           = $8036; // GL 1.2
  UNSIGNED_INT_2_10_10_10_REV                       = $8368; // GL 1.2

  // interleaved arrays formats
  GL_V2F                                            = $2A20;
  GL_V3F                                            = $2A21;
  GL_C4UB_V2F                                       = $2A22; 
  GL_C4UB_V3F                                       = $2A23;
  GL_C3F_V3F                                        = $2A24; 
  GL_N3F_V3F                                        = $2A25; 
  GL_C4F_N3F_V3F                                    = $2A26; 
  GL_T2F_V3F                                        = $2A27; 
  GL_T4F_V4F                                        = $2A28; 
  GL_T2F_C4UB_V3F                                   = $2A29; 
  GL_T2F_C3F_V3F                                    = $2A2A;
  GL_T2F_N3F_V3F                                    = $2A2B; 
  GL_T2F_C4F_N3F_V3F                                = $2A2C; 
  GL_T4F_C4F_N3F_V4F                                = $2A2D; 

  // clip planes
  GL_CLIP_PLANE0                                    = $3000;
  GL_CLIP_PLANE1                                    = $3001;
  GL_CLIP_PLANE2                                    = $3002;
  GL_CLIP_PLANE3                                    = $3003;
  GL_CLIP_PLANE4                                    = $3004;
  GL_CLIP_PLANE5                                    = $3005;

  // miscellaneous
  GL_DITHER                                         = $0BD0;

  // ----- extensions enumerants -----

  // EXT_bgra
  GL_BGR_EXT                                 = $80E0;
  GL_BGRA_EXT                                = $80E1;

  // ----------- GLU Constant ----------------------------

  // Errors: (return value 0 = no error)
  GLU_INVALID_ENUM                                  = 100900;
  GLU_INVALID_VALUE                                 = 100901;
  GLU_OUT_OF_MEMORY                                 = 100902;
  GLU_INCOMPATIBLE_GL_VERSION                       = 100903;

  // StringName
  GLU_VERSION                                       = 100800;
  GLU_EXTENSIONS                                    = 100801;

  // Boolean
  GLU_TRUE                                          = GL_TRUE;
  GLU_FALSE                                         = GL_FALSE;

  // Quadric constants
  // QuadricNormal
  GLU_SMOOTH                                        = 100000;
  GLU_FLAT                                          = 100001;
  GLU_NONE                                          = 100002;

  // QuadricDrawStyle
  GLU_POINT                                         = 100010;
  GLU_LINE                                          = 100011;
  GLU_FILL                                          = 100012;
  GLU_SILHOUETTE                                    = 100013;

  // QuadricOrientation
  GLU_OUTSIDE                                       = 100020;
  GLU_INSIDE                                        = 100021;

  // Tesselation constants
  GLU_TESS_MAX_COORD                                = 1.0e150;

  // TessProperty
  GLU_TESS_WINDING_RULE                             = 100140;
  GLU_TESS_BOUNDARY_ONLY                            = 100141;
  GLU_TESS_TOLERANCE                                = 100142;

  // TessWinding
  GLU_TESS_WINDING_ODD                              = 100130;
  GLU_TESS_WINDING_NONZERO                          = 100131;
  GLU_TESS_WINDING_POSITIVE                         = 100132;
  GLU_TESS_WINDING_NEGATIVE                         = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO                      = 100134;

  // TessCallback
  GLU_TESS_BEGIN                                    = 100100; // TGLUTessBeginProc
  GLU_TESS_VERTEX                                   = 100101; // TGLUTessVertexProc
  GLU_TESS_END                                      = 100102; // TGLUTessEndProc
  GLU_TESS_ERROR                                    = 100103; // TGLUTessErrorProc
  GLU_TESS_EDGE_FLAG                                = 100104; // TGLUTessEdgeFlagProc
  GLU_TESS_COMBINE                                  = 100105; // TGLUTessCombineProc
  GLU_TESS_BEGIN_DATA                               = 100106; // TGLUTessBeginDataProc
  GLU_TESS_VERTEX_DATA                              = 100107; // TGLUTessVertexDataProc
  GLU_TESS_END_DATA                                 = 100108; // TGLUTessEndDataProc
  GLU_TESS_ERROR_DATA                               = 100109; // TGLUTessErrorDataProc
  GLU_TESS_EDGE_FLAG_DATA                           = 100110; // TGLUTessEdgeFlagDataProc
  GLU_TESS_COMBINE_DATA                             = 100111; // TGLUTessCombineDataProc

  // TessError
  GLU_TESS_ERROR1                                   = 100151;
  GLU_TESS_ERROR2                                   = 100152;
  GLU_TESS_ERROR3                                   = 100153;
  GLU_TESS_ERROR4                                   = 100154;
  GLU_TESS_ERROR5                                   = 100155;
  GLU_TESS_ERROR6                                   = 100156;
  GLU_TESS_ERROR7                                   = 100157;
  GLU_TESS_ERROR8                                   = 100158;

  GLU_TESS_MISSING_BEGIN_POLYGON                    = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR                    = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON                      = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR                      = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE                          = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK                    = GLU_TESS_ERROR6;

  // NURBS constants

  // NurbsProperty
  GLU_AUTO_LOAD_MATRIX                              = 100200;
  GLU_CULLING                                       = 100201;
  GLU_SAMPLING_TOLERANCE                            = 100203;
  GLU_DISPLAY_MODE                                  = 100204;
  GLU_PARAMETRIC_TOLERANCE                          = 100202;
  GLU_SAMPLING_METHOD                               = 100205;
  GLU_U_STEP                                        = 100206;
  GLU_V_STEP                                        = 100207;

  // NurbsSampling
  GLU_PATH_LENGTH                                   = 100215;
  GLU_PARAMETRIC_ERROR                              = 100216;
  GLU_DOMAIN_DISTANCE                               = 100217;

  // NurbsTrim
  GLU_MAP1_TRIM_2                                   = 100210;
  GLU_MAP1_TRIM_3                                   = 100211;

  // NurbsDisplay
  GLU_OUTLINE_POLYGON                               = 100240;
  GLU_OUTLINE_PATCH                                 = 100241;

  // NurbsErrors
  GLU_NURBS_ERROR1                                  = 100251;
  GLU_NURBS_ERROR2                                  = 100252;
  GLU_NURBS_ERROR3                                  = 100253;
  GLU_NURBS_ERROR4                                  = 100254;
  GLU_NURBS_ERROR5                                  = 100255;
  GLU_NURBS_ERROR6                                  = 100256;
  GLU_NURBS_ERROR7                                  = 100257;
  GLU_NURBS_ERROR8                                  = 100258;
  GLU_NURBS_ERROR9                                  = 100259;
  GLU_NURBS_ERROR10                                 = 100260;
  GLU_NURBS_ERROR11                                 = 100261;
  GLU_NURBS_ERROR12                                 = 100262;
  GLU_NURBS_ERROR13                                 = 100263;
  GLU_NURBS_ERROR14                                 = 100264;
  GLU_NURBS_ERROR15                                 = 100265;
  GLU_NURBS_ERROR16                                 = 100266;
  GLU_NURBS_ERROR17                                 = 100267;
  GLU_NURBS_ERROR18                                 = 100268;
  GLU_NURBS_ERROR19                                 = 100269;
  GLU_NURBS_ERROR20                                 = 100270;
  GLU_NURBS_ERROR21                                 = 100271;
  GLU_NURBS_ERROR22                                 = 100272;
  GLU_NURBS_ERROR23                                 = 100273;
  GLU_NURBS_ERROR24                                 = 100274;
  GLU_NURBS_ERROR25                                 = 100275;
  GLU_NURBS_ERROR26                                 = 100276;
  GLU_NURBS_ERROR27                                 = 100277;
  GLU_NURBS_ERROR28                                 = 100278;
  GLU_NURBS_ERROR29                                 = 100279;
  GLU_NURBS_ERROR30                                 = 100280;
  GLU_NURBS_ERROR31                                 = 100281;
  GLU_NURBS_ERROR32                                 = 100282;
  GLU_NURBS_ERROR33                                 = 100283;
  GLU_NURBS_ERROR34                                 = 100284;
  GLU_NURBS_ERROR35                                 = 100285;
  GLU_NURBS_ERROR36                                 = 100286;
  GLU_NURBS_ERROR37                                 = 100287;

  // Contours types -- obsolete!
  GLU_CW                                            = 100120;
  GLU_CCW                                           = 100121;
  GLU_INTERIOR                                      = 100122;
  GLU_EXTERIOR                                      = 100123;
  GLU_UNKNOWN                                       = 100124;

  // Names without "TESS_" prefix
  GLU_BEGIN                                         = GLU_TESS_BEGIN;
  GLU_VERTEX                                        = GLU_TESS_VERTEX;
  GLU_END                                           = GLU_TESS_END;
  GLU_ERROR                                         = GLU_TESS_ERROR;
  GLU_EDGE_FLAG                                     = GLU_TESS_EDGE_FLAG;

// ----------- GL Functions ----------------------------

procedure glAccum(op: TGLuint; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glAreTexturesResident(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glArrayElement(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glBegin(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glBindTexture(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glBitmap(width: TGLsizei; height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCallList(list: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCallLists(n: TGLsizei; atype: TGLEnum; lists: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClear(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClearAccum(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClearColor(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClearDepth(depth: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClearIndex(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClearStencil(s: TGLint ); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3b(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3d(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3f(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3i(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3s(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3ub(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3ui(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3us(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor3usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4b(red, green, blue, alpha: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4d(red, green, blue, alpha: TGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4f(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4i(red, green, blue, alpha: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4s(red, green, blue, alpha: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4sv(v: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4ub(red, green, blue, alpha: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4ui(red, green, blue, alpha: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4us(red, green, blue, alpha: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColor4usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorMask(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorMaterial(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyPixels(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyTexImage1D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyTexImage2D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyTexSubImage1D(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyTexSubImage2D(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCullFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDeleteLists(list: TGLuint; range: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDeleteTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDepthFunc(func: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDepthMask(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDepthRange(zNear, zFar: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDisable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDisableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDrawArrays(mode: TGLEnum; first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDrawBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDrawElements(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glDrawPixels(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEdgeFlag(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEdgeFlagPointer(stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEdgeFlagv(flag: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEnable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEnableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEnd; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEndList; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord1d(u: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord1dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord1f(u: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord1fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord2d(u: TGLdouble; v: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord2dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord2f(u, v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalCoord2fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalMesh1(mode: TGLEnum; i1, i2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalMesh2(mode: TGLEnum; i1, i2, j1, j2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalPoint1(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glEvalPoint2(i, j: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFeedbackBuffer(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFinish; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFlush; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFogf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFogfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFogi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFogiv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFrontFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glFrustum(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glGenLists(range: TGLsizei): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGenTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetBooleanv(pname: TGLEnum; params: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetDoublev(pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glGetError: TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetFloatv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetIntegerv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMapdv(target, query: TGLEnum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMapfv(target, query: TGLEnum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMapiv(target, query: TGLEnum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetPixelMapfv(map: TGLEnum; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetPixelMapuiv(map: TGLEnum; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetPixelMapusv(map: TGLEnum; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetPointerv(pname: TGLEnum; var params); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glGetString(name: TGLEnum): PChar; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexImage(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexLevelParameterfv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexLevelParameteriv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glHint(target, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexd(c: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexdv(c: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexf(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexfv(c: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexi(c: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexiv(c: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexs(c: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexsv(c: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexub(c: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glIndexubv(c: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glInitNames; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glInterleavedArrays(format: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glIsEnabled(cap: TGLEnum): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glIsList(list: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glIsTexture(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightModelf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightModelfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightModeli(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightModeliv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightf(light, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLighti(light, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLineStipple(factor: TGLint; pattern: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLineWidth(width: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glListBase(base: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLoadIdentity; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLoadMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLoadMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLoadName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glLogicOp(opcode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMap1d(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMap1f(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat);   {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMap2d(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,
  vorder: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMap2f(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,
  vorder: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMapGrid1d(un: TGLint; u1, u2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMapGrid1f(un: TGLint; u1, u2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMapGrid2d(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMapGrid2f(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMateriali(face, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMatrixMode(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMultMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMultMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNewList(list: TGLuint; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3b(nx, ny, nz: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3d(nx, ny, nz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3f(nx, ny, nz: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3i(nx, ny, nz: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3s(nx, ny, nz: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormal3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glNormalPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glOrtho(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPassThrough(token: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelMapfv(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelMapuiv(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelMapusv(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelStoref(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelStorei(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelTransferi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPixelZoom(xfactor, yfactor: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPointSize(size: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPolygonMode(face, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPolygonOffset(factor, units: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPopAttrib; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPopClientAttrib; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPopMatrix; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPopName; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPrioritizeTextures(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPushAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPushClientAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPushMatrix; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glPushName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2s(x, y: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRasterPos4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glReadBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glReadPixels(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectd(x1, y1, x2, y2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectdv(v1, v2: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectf(x1, y1, x2, y2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectfv(v1, v2: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRecti(x1, y1, x2, y2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectiv(v1, v2: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRects(x1, y1, x2, y2: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRectsv(v1, v2: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
function glRenderMode(mode: TGLEnum): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRotated(angle, x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glRotatef(angle, x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glScaled(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glScalef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glScissor(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glSelectBuffer(size: TGLsizei; buffer: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glShadeModel(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glStencilFunc(func: TGLEnum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glStencilMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glStencilOp(fail, zfail, zpass: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1d(s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1f(s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1i(s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1s(s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord1sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2d(s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2f(s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2i(s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2s(s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3d(s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3f(s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3i(s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3s(s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4d(s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4f(s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4i(s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4s(s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoord4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexCoordPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexEnvf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexEnvi(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGend(coord, pname: TGLEnum; param: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGenf(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGeni(coord, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexImage1D(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,
  atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexImage2D(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;
  format, atype: TGLEnum; Pixels:Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexSubImage1D(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;
  pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexSubImage2D(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,
  atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTranslated(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTranslatef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2s(x, y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertex4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glVertexPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glViewport(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;

// GL 1.2
procedure glDrawRangeElements(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;
  indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glTexImage3D(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;
  border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;

// GL 1.2 ARB imaging
procedure glBlendColor(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glBlendEquation(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorSubTable(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyColorSubTable(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorTable(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
  table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyColorTable(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorTableParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glColorTableParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetColorTable(target, format, Atype: TGLEnum; table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetColorTableParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetColorTableParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionFilter1D(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
  image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionFilter2D(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;
  image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyConvolutionFilter1D(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glCopyConvolutionFilter2D(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetConvolutionFilter(target, internalformat, Atype: TGLEnum; image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glSeparableFilter2D(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,
  column: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetSeparableFilter(target, format, Atype: TGLEnum; row, column, span: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glConvolutionParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetConvolutionParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetConvolutionParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glHistogram(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glResetHistogram(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetHistogram(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetHistogramParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetHistogramParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glMinmax(target, internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glResetMinmax(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMinmax(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMinmaxParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;
procedure glGetMinmaxParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external opengl32;

// GL utility functions and procedures
function gluErrorString(errCode: TGLEnum): PChar; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluGetString(name: TGLEnum): PChar; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluOrtho2D(left, right, bottom, top: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluPickMatrix(x, y, width, height: TGLdouble; viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluProject(objx, objy, objz: TGLdouble; modelMatrix: TMatrix4d; projMatrix: TMatrix4d; viewport: TVector4i;
  winx, winy, winz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluUnProject(winx, winy, winz: TGLdouble; modelMatrix: TMatrix4d; projMatrix: TMatrix4d; viewport: TVector4i;
  objx, objy, objz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluScaleImage(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,
  heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluBuild1DMipmaps(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;
  data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluBuild2DMipmaps(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;
  Data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluNewQuadric: PGLUquadric; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluDeleteQuadric(state: PGLUquadric); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluQuadricNormals(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluCylinder(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,
  stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
  startAngle, sweepAngle: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluQuadricCallback(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluNewTess: PGLUtesselator; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluDeleteTess(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessBeginContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessVertex(tess: PGLUtesselator; coords: TVector3d; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessEndContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessProperty(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessNormal(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluTessCallback(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluGetTessProperty(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
function gluNewNurbsRenderer: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluBeginSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluBeginCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluEndCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluEndSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluBeginTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluEndTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluPwlCurve(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; modelMatrix, projMatrix: TMatrix4f; viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluNurbsCallback(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluBeginPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluNextContour(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;
procedure gluEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external glu32;

//-------- GL Extensions ----------------

var
  GL_ARB_multitexture,
  GL_EXT_separate_specular_color,
  GL_EXT_texture_filter_anisotropic,
  GL_ARB_texture_compression,
  GL_EXT_texture_edge_clamp: boolean;

const

  // GL_ARB_texture_compression
  GL_COMPRESSED_ALPHA_ARB                    = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB                = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB          = $84EB;
  GL_COMPRESSED_INTENSITY_ARB                = $84EC;
  GL_COMPRESSED_RGB_ARB                      = $84ED;
  GL_COMPRESSED_RGBA_ARB                     = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB            = $84EF;
  GL_TEXTURE_IMAGE_SIZE_ARB                  = $86A0;
  GL_TEXTURE_COMPRESSED_ARB                  = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB      = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB          = $86A3;

  // GL_EXT_separate_specular_color
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT           = $81F8;
  GL_SINGLE_COLOR_EXT                        = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT             = $81FA;

  // GL_EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT              = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT          = $84FF;

  // GL_EXT_texture_edge_clamp
  GL_CLAMP_TO_EDGE_EXT                       = $812F;

  // GL_ARB_multisample
  GL_MULTISAMPLE_ARB                         = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB            = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB                 = $809F;
  GL_SAMPLE_COVERAGE_ARB                     = $80A0;
  GL_SAMPLE_BUFFERS_ARB                      = $80A8;
  GL_SAMPLES_ARB                             = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB               = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB              = $80AB;
  GL_MULTISAMPLE_BIT_ARB                     = $20000000;

var
  // ARB_multitexture
  glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1dVARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1fARBP: procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1fVARB: procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1iVARB: procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1sARBP: procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord1sVARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2dARB: procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2fARB: procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2iARB: procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2sARB: procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord2svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3dARB: procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3fARB: procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3iARB: procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3sARB: procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord3svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4dARB: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4fARB: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4iARB: procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4sARB: procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glMultiTexCoord4svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  glClientActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

//-------- Platform Support -------------

{$IFDEF MSWINDOWS}
type
  HGLRC = THandle;

function wglGetProcAddress(ProcName: PChar): Pointer; stdcall; external opengl32;
{$ENDIF}

{$IFDEF LINUX}
const
  GLX_USE_GL                                        = 1; 
  GLX_BUFFER_SIZE                                   = 2; 
  GLX_LEVEL                                         = 3; 
  GLX_RGBA                                          = 4; 
  GLX_DOUBLEBUFFER                                  = 5; 
  GLX_STEREO                                        = 6; 
  GLX_AUX_BUFFERS                                   = 7; 
  GLX_RED_SIZE                                      = 8; 
  GLX_GREEN_SIZE                                    = 9; 
  GLX_BLUE_SIZE                                     = 10; 
  GLX_ALPHA_SIZE                                    = 11; 
  GLX_DEPTH_SIZE                                    = 12;
  GLX_STENCIL_SIZE                                  = 13;
  GLX_ACCUM_RED_SIZE                                = 14;
  GLX_ACCUM_GREEN_SIZE                              = 15;
  GLX_ACCUM_BLUE_SIZE                               = 16;
  GLX_ACCUM_ALPHA_SIZE                              = 17;

  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                                    = 1;
  GLX_BAD_ATTRIBUTE                                 = 2;
  GLX_NO_EXTENSION                                  = 3;
  GLX_BAD_VISUAL                                    = 4;
  GLX_BAD_CONTEXT                                   = 5;
  GLX_BAD_VALUE                                     = 6;
  GLX_BAD_ENUM                                      = 7;

type
  GLXContext     = THandle; //Pointer;
  GLXPixmap      = XID;
  GLXDrawable    = XID;

function glXChooseVisual(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl; external opengl32;
procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo; pixmap: Pixmap): GLXPixmap; cdecl; external opengl32;
procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
function glXQueryExtension(dpy: PDisplay; errorb: PGLInt; event: PGLInt): TGLboolean; cdecl; external opengl32;
function glXQueryVersion(dpy: PDisplay; maj: PGLInt; min: PGLINT): TGLboolean; cdecl; external opengl32;
function glXIsDirect(dpy: PDisplay; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
procedure glXWaitGL; cdecl; external opengl32;
procedure glXWaitX; cdecl; external opengl32;
procedure glXUseXFont(font: Font; first: TGLInt; count: TGLInt; list: TGLint); cdecl; external opengl32;
{$ENDIF}

//-------- Helper Functions ----------------

procedure LoadGLExtensions;

implementation

procedure LoadGLExtensions;
var buffer: string;
begin
  Buffer := glGetString(GL_EXTENSIONS);
  GL_ARB_texture_compression := Pos('GL_ARB_texture_compression', Buffer) > 0;
  GL_EXT_separate_specular_color := Pos('GL_EXT_separate_specular_color', Buffer) > 0;
  GL_EXT_texture_filter_anisotropic := Pos('GL_EXT_texture_filter_anisotropic', Buffer) > 0;
  GL_EXT_texture_edge_clamp := Pos('GL_EXT_texture_edge_clamp', Buffer) > 0;
  GL_ARB_multitexture := Pos('GL_ARB_multitexture', Buffer) > 0;
{$IFDEF MSWINDOWS}
  if GL_ARB_multitexture then
  begin
    glMultiTexCoord1dARB := wglGetProcAddress('glMultiTexCoord1dARB');
    glMultiTexCoord1dVARB := wglGetProcAddress('glMultiTexCoord1dVARB');
    glMultiTexCoord1fARBP := wglGetProcAddress('glMultiTexCoord1fARBP');
    glMultiTexCoord1fVARB := wglGetProcAddress('glMultiTexCoord1fVARB');
    glMultiTexCoord1iARB := wglGetProcAddress('glMultiTexCoord1iARB');
    glMultiTexCoord1iVARB := wglGetProcAddress('glMultiTexCoord1iVARB');
    glMultiTexCoord1sARBP := wglGetProcAddress('glMultiTexCoord1sARBP');
    glMultiTexCoord1sVARB := wglGetProcAddress('glMultiTexCoord1sVARB');
    glMultiTexCoord2dARB := wglGetProcAddress('glMultiTexCoord2dARB');
    glMultiTexCoord2dvARB := wglGetProcAddress('glMultiTexCoord2dvARB');
    glMultiTexCoord2fARB := wglGetProcAddress('glMultiTexCoord2fARB');
    glMultiTexCoord2fvARB := wglGetProcAddress('glMultiTexCoord2fvARB');
    glMultiTexCoord2iARB := wglGetProcAddress('glMultiTexCoord2iARB');
    glMultiTexCoord2ivARB := wglGetProcAddress('glMultiTexCoord2ivARB');
    glMultiTexCoord2sARB := wglGetProcAddress('glMultiTexCoord2sARB');
    glMultiTexCoord2svARB := wglGetProcAddress('glMultiTexCoord2svARB');
    glMultiTexCoord3dARB := wglGetProcAddress('glMultiTexCoord3dARB');
    glMultiTexCoord3dvARB := wglGetProcAddress('glMultiTexCoord3dvARB');
    glMultiTexCoord3fARB := wglGetProcAddress('glMultiTexCoord3fARB');
    glMultiTexCoord3fvARB := wglGetProcAddress('glMultiTexCoord3fvARB');
    glMultiTexCoord3iARB := wglGetProcAddress('glMultiTexCoord3iARB');
    glMultiTexCoord3ivARB := wglGetProcAddress('glMultiTexCoord3ivARB');
    glMultiTexCoord3sARB := wglGetProcAddress('glMultiTexCoord3sARB');
    glMultiTexCoord3svARB := wglGetProcAddress('glMultiTexCoord3svARB');
    glMultiTexCoord4dARB := wglGetProcAddress('glMultiTexCoord4dARB');
    glMultiTexCoord4dvARB := wglGetProcAddress('glMultiTexCoord4dvARB');
    glMultiTexCoord4fARB := wglGetProcAddress('glMultiTexCoord4fARB');
    glMultiTexCoord4fvARB := wglGetProcAddress('glMultiTexCoord4fvARB');
    glMultiTexCoord4iARB := wglGetProcAddress('glMultiTexCoord4iARB');
    glMultiTexCoord4ivARB := wglGetProcAddress('glMultiTexCoord4ivARB');
    glMultiTexCoord4sARB := wglGetProcAddress('glMultiTexCoord4sARB');
    glMultiTexCoord4svARB := wglGetProcAddress('glMultiTexCoord4svARB');
    glActiveTextureARB := wglGetProcAddress('glActiveTextureARB');
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
  end;
{$ENDIF}
end;

initialization
{
Set8087CW procedure
Description

The floating-point unit control word controls the precision of floating point
calculations, the rounding mode, and whether certain floating-point operations
trigger exceptions. See Intel's processor documentation for details.

This routine allows the programmer to have direct access to the CW. Be aware
that using this routine to change the value of the 8087CW will change the
behavior of the program’s FP calculations. It is the programmer’s responsibility
to reset it.

It is recommended that you disable all floating-point exceptions when using
OpenGL to render 3D graphics. To do this, call Set8087CW(0x133f) in your main
form’s OnCreate event before calling any OpenGL functions.}

  Set8087CW($133F);

end.

