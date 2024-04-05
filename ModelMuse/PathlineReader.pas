unit PathlineReader;

interface

uses System.UITypes, Windows, Classes, SysUtils, GoPhastTypes, ColorSchemes, Graphics, GR32,
  OpenGL, RealListUnit, QuadtreeClass, Generics.Collections, XBase1, FastGEO;

type
  IDisplayer = interface
    ['{873DE8B9-E2D6-41AE-BA40-EDA0AC9E1BE5}']
    procedure GetMinMaxValues(var MaxValue, MinValue: Double);
  end;

  TShowChoice = (scAll, scSpecified, scStart, scEnd);
  TAnsiCharArray = array of AnsiChar;
  TPathlineVersion = (pv5, pv6_0, pv7_2, pvUnknown);

  TShowIntegerLimit = class(TPersistent)
  private
    FEndLimit: integer;
    FUseLimit: boolean;
    FStartLimit: integer;
    procedure SetEndLimit(const Value: integer);
    procedure SetStartLimit(const Value: integer);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UseLimit: boolean read FUseLimit write SetUseLimit;
    property StartLimit: integer read FStartLimit write SetStartLimit;
    property EndLimit: integer read FEndLimit write SetEndLimit;
  end;

  TShowFloatLimit = class(TPersistent)
  private
    FEndLimit: double;
    FUseLimit: boolean;
    FStartLimit: double;
    procedure SetEndLimit(const Value: double);
    procedure SetStartLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UseLimit: boolean read FUseLimit write SetUseLimit;
    property StartLimit: double read FStartLimit write SetStartLimit;
    property EndLimit: double read FEndLimit write SetEndLimit;
  end;

  TPathLineDisplayLimits = class(TPersistent)
  private
    FLimitToCurrentIn2D: boolean;
    FLayerLimits: TShowIntegerLimit;
    FRowLimits: TShowIntegerLimit;
    FColumnLimits: TShowIntegerLimit;
    FShowChoice: TShowChoice;
    FTimeLimits: TShowFloatLimit;
    FParticleGroupLimits: TShowIntegerLimit;
    FLineNumberLimits: TShowIntegerLimit;
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetColumnLimits(const Value: TShowIntegerLimit);
    procedure SetLayerLimits(const Value: TShowIntegerLimit);
    procedure SetRowLimits(const Value: TShowIntegerLimit);
    procedure SetShowChoice(const Value: TShowChoice);
    procedure SetTimeLimits(const Value: TShowFloatLimit);
    procedure SetParticleGroupLimits(const Value: TShowIntegerLimit);
    procedure SetLineNumberLimits(const Value: TShowIntegerLimit);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TShowChoice read FShowChoice write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property ColumnLimits: TShowIntegerLimit read FColumnLimits
      write SetColumnLimits;
    property RowLimits: TShowIntegerLimit read FRowLimits write SetRowLimits;
    property LayerLimits: TShowIntegerLimit read FLayerLimits
      write SetLayerLimits;
    property TimeLimits: TShowFloatLimit read FTimeLimits write SetTimeLimits;
    property ParticleGroupLimits: TShowIntegerLimit read FParticleGroupLimits
      write SetParticleGroupLimits;
    property LineNumberLimits: TShowIntegerLimit read FLineNumberLimits
      write SetLineNumberLimits;
  end;

  TColorLimitChoice = (clcNone, clcTime, clcLogTime, clcXPrime, clcYPrime, clcZ, clcGroup);

  TPathlineColorLimits = class(TPersistent)
  private
    FColoringChoice: TColorLimitChoice;
    FMaxColorLimit: double;
    FMinColorLimit: double;
    FUseLimit: boolean;
    procedure SetColoringChoice(const Value: TColorLimitChoice);
    procedure SetMinColorLimit(const Value: double);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TColorLimitChoice read FColoringChoice
      write SetColoringChoice default clcTime;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TPathLine = class;
  TPathLineV6 = class;
  TPathLineV7 = class;
  TCustomPathLine = class;

  TPathLinePoint = class(TCollectionItem)
  private
    FLayer: integer;
    FTimeStep: integer;
    FLocalZ: double;
    FZ: double;
    FX: double;
    FY: double;
    FTime: double;
    FRow: integer;
    FColumn: integer;
    FXPrime: double;
    FYPrime: double;
    FHasV6Data: Boolean;
    FModpathVersion: TPathlineVersion;
    function GetAbsoluteTime: double;
    procedure SetHasV6Data(const Value: Boolean);
    procedure SetModpathVersion(const Value: TPathlineVersion);
  protected
    function CheckLimits(Limits: TPathLineDisplayLimits): boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TPathLineDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
      const Segment: TSegment2D; const DisvUsed: Boolean): boolean;
    function ShouldShowLine(Limits: TPathLineDisplayLimits): boolean;
    function ParentLine: TCustomPathLine;
    property AbsoluteTime: double read GetAbsoluteTime;
  published
    // Real world X coordinate
    property X: double read FX write FX;
    // Real world Y coordinate
    property Y: double read FY write FY;
    property Z: double read FZ write FZ;
    // X position in grid coordinates;
    property XPrime: double read FXPrime write FXPrime;
    // Y position in grid coordinates;
    property YPrime: double read FYPrime write FYPrime;
    property LocalZ: double read FLocalZ write FLocalZ;
    property Time: double read FTime write FTime;
    property Layer: integer read FLayer write FLayer;
    property Row: integer read FRow write FRow;
    property Column: integer read FColumn write FColumn;
    property TimeStep: integer read FTimeStep write FTimeStep;
    // @name indicates whether the particle has information about
    // GridIndex, LocalX, and LocalY.
    property HasV6Data: Boolean read FHasV6Data write SetHasV6Data stored False;
    property ModpathVersion: TPathlineVersion read FModpathVersion
      write SetModpathVersion;
  end;

  TPathLinePointV6 = class(TPathLinePoint)
  private
    FTimePointIndex: integer;
    FLineSegmentIndex: Integer;
    FParticleGroup: integer;
    FGridIndex: Integer;
    FLocalX: double;
    FLocalY: double;
  protected
    function CheckLimits(Limits: TPathLineDisplayLimits): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ParticleGroup: integer read FParticleGroup write FParticleGroup;
    property TimePointIndex: integer read FTimePointIndex write FTimePointIndex;
    property LineSegmentIndex: Integer read FLineSegmentIndex write FLineSegmentIndex;
    property GridIndex: Integer read FGridIndex write FGridIndex;
    property LocalX: double read FLocalX write FLocalX;
    property LocalY: double read FLocalY write FLocalY;
  end;

  TPathLinePointV7 = class(TPathLinePointV6)
  private
    FSequenceNumber: Integer;
    FStressPeriod: Integer;
    FCellNumber: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SequenceNumber: Integer read FSequenceNumber write FSequenceNumber;
    property StressPeriod: Integer read FStressPeriod write FStressPeriod;
    property CellNumber: Integer read FCellNumber write FCellNumber;
  end;

  TCustomPathLinePoints = class(TCollection)
  private
    FLength: double;
    function GetPoint(Index: integer): TPathLinePoint;
    function GetLength: double;
  protected
    FPathLine: TCustomPathLine;
  public
    property Points[Index: integer]: TPathLinePoint read GetPoint; default;
    Constructor Create(ItemClass: TCollectionItemClass; PathLine: TCustomPathLine);
    function TestGetMaxTime(var Maxtime: double): boolean;
    property Length: double read GetLength;
  end;

  TPathLinePoints = class(TCustomPathLinePoints)
  public
    Constructor Create(PathLine: TPathLine);
  end;

  TPathLinePointsV6 = class(TCustomPathLinePoints)
  public
    Constructor Create(PathLine: TPathLineV6);
  end;

  TPathLinePointsV7 = class(TCustomPathLinePoints)
  public
    Constructor Create(PathLine: TPathLineV7);
  end;

  TCustomPathLine = class(TCollectionItem)
  private
    FPoints: TCustomPathLinePoints;
    FParticleIndex: Integer;
    procedure SetPoints(const Value: TCustomPathLinePoints);
    function GetLength: Double;
    procedure SetParticleIndex(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    property Length: Double read GetLength;
  published
    property Points: TCustomPathLinePoints read FPoints write SetPoints;
    property ParticleIndex: Integer read FParticleIndex write SetParticleIndex;
  end;

  TPathLine = class(TCustomPathLine)
  public
    Constructor Create(Collection: TCollection); override;
  end;

  TPathLineV6 = class(TCustomPathLine)
  public
    Constructor Create(Collection: TCollection); override;
  end;

  TPathLineV7 = class(TCustomPathLine)
  public
    Constructor Create(Collection: TCollection); override;
  end;

  TCustomPathLines = class(TCollection)
  private
    function GetLine(Index: integer): TCustomPathLine;
  protected
    procedure DefineShapeFileFields(Fields: TStringList); virtual;
    procedure UpdateShapeFileFields(ALine: TCustomPathLine;
      ShapeDataBase: TXBase); virtual;
    procedure ExportShapefile(FileName: string);
    function XbaseFieldName(AName: Ansistring): Ansistring;
  public
    property Lines[Index: integer]: TCustomPathLine read GetLine; default;
    function TestGetMaxTime(var Maxtime: double): boolean;
    function Add: TCustomPathLine;
  end;

  TPathLines = class(TCustomPathLines)
  public
    Constructor Create;
  end;

  TCustomPathLinesV6 = class(TCustomPathLines)
  protected
    procedure UpdateShapeFileFields(ALine: TCustomPathLine;
      ShapeDataBase: TXBase); override;
    procedure DefineShapeFileFields(Fields: TStringList); override;
  end;

  TPathLinesV6 = class(TCustomPathLinesV6)
  public
    Constructor Create;
  end;

  TPathLinesV7 = class(TCustomPathLinesV6)
  public
    Constructor Create;
  end;

  TCustomModpathSettings = class(TPersistent)
  private
    FColorParameters: TColorParameters;
    procedure SetColorParameters(const Value: TColorParameters);
  protected
    FVisible: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ColorParameters: TColorParameters read FColorParameters
      write SetColorParameters;
    property Visible: boolean read FVisible
      write FVisible default True;
  end;

  TPathLineSettings = class(TCustomModpathSettings)
  private
    FDisplayLimits: TPathLineDisplayLimits;
    FColorLimits: TPathlineColorLimits;
    procedure SetDisplayLimits(const Value: TPathLineDisplayLimits);
    procedure SetColorLimits(const Value: TPathlineColorLimits);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property DisplayLimits: TPathLineDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property ColorLimits: TPathlineColorLimits read FColorLimits
      write SetColorLimits;
    property DisplayPathLines: boolean read FVisible
      write FVisible Stored False;
   end;

  TTrackingDirection = (tdForward, tdBackward);

  TPathLineReader = class(TPathLineSettings)
  private
    class var
      FPathlineGLIndex: GLuint;
      FListInitialized: boolean;
  private
    FLinesV5: TPathLines;
    FLinesV6: TPathLinesV6;
    FFileName: string;
    FFile: TFileStream;
    FFileDate: TDateTime;
    FMaxTime: double;
    FMinTime: double;
    FRecordedPathLines: Boolean;
    FDrawingPathLines: Boolean;
    FTopQuadTree: TRbwQuadTree;
    FFrontQuadTree: TRbwQuadTree;
    FSideQuadTree: TRbwQuadTree;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FModpathVersion: TPathlineVersion;
    FReferenceTimeV6: double;
    FTrackingDirectionV6: TTrackingDirection;
    FMaxParticleGroup: integer;
    FMinParticleGroup: integer;
    FMinPositiveTime: double;
    FLinesV7: TPathLinesV7;
    FSelectedLayer: Integer;
    FSelectedRow: Integer;
    FSelectedColumn: Integer;
    procedure SetLinesV5(const Value: TPathLines);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetMaxTime(const Value: double);
    procedure SetMinTime(const Value: double);
    function GetPointColor(MaxValue, MinValue: double;
      Point: TPathLinePoint): TColor;
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function CheckShowLine(Line: TCustomPathLine): Boolean;
    class function GetPathlineGLIndex: GLuint; static;
    procedure ReadFileV5;
    procedure DrawLines3D(LocalLines: TCustomPathLines);
    procedure DrawLines(Orientation: TDataSetOrientation; const BitMap: TPersistent;
      LocalLines: TCustomPathLines);
    procedure Record3DPathLines(LocalLines: TCustomPathLines);
    procedure SetLinesV6(const Value: TPathLinesV6);
    procedure SetLinesV7(const Value: TPathLinesV7);
    procedure ReadFileV6;
    procedure ReadFileV7;
    procedure SetMaxParticleGroup(const Value: integer);
    procedure SetMinParticleGroup(const Value: integer);
    function GetMaxLineNumber: integer;
    procedure SetMinPositiveTime(const Value: double);
    procedure UpdateMinMaxTime(LocalLines: TCustomPathLines);
    function GetHasData: Boolean;
  protected
    class property PathlineGLIndex: GLuint read GetPathlineGLIndex;
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property HasData: Boolean read GetHasData;
    procedure ReadFile;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TPersistent);
    procedure Draw3D;
    procedure Invalidate;
    property TopQuadTree: TRbwQuadTree read FTopQuadTree;
    property FrontQuadTree: TRbwQuadTree read FFrontQuadTree;
    property SideQuadTree: TRbwQuadTree read FSideQuadTree;
    procedure ExportShapefile(FileName: string);
    property MaxLineNumber: integer read GetMaxLineNumber;
    procedure UpdateMinMax;
    procedure Clear;
  published
    // If an additional lines type is added, be sure to update GetHasData
    // and exporting to a Shapefile
    property Lines: TPathLines read FLinesV5 write SetLinesV5;
    property LinesV6: TPathLinesV6 read FLinesV6 write SetLinesV6;
    property LinesV7: TPathLinesV7 read FLinesV7 write SetLinesV7;
    property FileName: string read FFileName write FFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property MaxTime: double read FMaxTime write SetMaxTime;
    property MinTime: double read FMinTime write SetMinTime;
    property MinPositiveTime: double read FMinPositiveTime write SetMinPositiveTime;
    property MinParticleGroup: integer read FMinParticleGroup write SetMinParticleGroup;
    property MaxParticleGroup: integer read FMaxParticleGroup write SetMaxParticleGroup;
    property ModpathVersion: TPathlineVersion read FModpathVersion
      write FModpathVersion;
    // Tracking direction in MODPATH versions 6 and 7.
    property TrackingDirectionV6: TTrackingDirection read FTrackingDirectionV6
      write FTrackingDirectionV6;
    // Reference time in MODPATH versions 6 and 7.
    property ReferenceTimeV6: double read FReferenceTimeV6
      write FReferenceTimeV6;
//    procedure GetOriginOffset(CrossSectionSegment: TSegment2D; var OriginOffset: Double);
  end;

  TPathLinesObjectList = TObjectList<TPathLineReader>;

  TEndpointShowChoice = (escAll, escSpecified);
  TWhereToPlot = (wtpStart, wtpEnd);

  TEndPointDisplayLimits = class(TPersistent)
  private
    FLimitToCurrentIn2D: boolean;
    FStartLayerLimits: TShowIntegerLimit;
    FStartRowLimits: TShowIntegerLimit;
    FStartColumnLimits: TShowIntegerLimit;
    FShowChoice: TEndpointShowChoice;
    FReleaseTimeLimits: TShowFloatLimit;
    FEndZoneLimits: TShowIntegerLimit;
    FEndLayerLimits: TShowIntegerLimit;
    FTrackingTimeLimits: TShowFloatLimit;
    FEndRowLimits: TShowIntegerLimit;
    FEndColumnLimits: TShowIntegerLimit;
    FStartZoneLimits: TShowIntegerLimit;
    FWhereToPlot: TWhereToPlot;
    FParticleGroupLimits: TShowIntegerLimit;
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetStartColumnLimits(const Value: TShowIntegerLimit);
    procedure SetStartLayerLimits(const Value: TShowIntegerLimit);
    procedure SetStartRowLimits(const Value: TShowIntegerLimit);
    procedure SetShowChoice(const Value: TEndpointShowChoice);
    procedure SetReleaseTimeLimits(const Value: TShowFloatLimit);
    procedure SetEndColumnLimits(const Value: TShowIntegerLimit);
    procedure SetEndLayerLimits(const Value: TShowIntegerLimit);
    procedure SetEndRowLimits(const Value: TShowIntegerLimit);
    procedure SetEndZoneLimits(const Value: TShowIntegerLimit);
    procedure SetStartZoneLimits(const Value: TShowIntegerLimit);
    procedure SetTrackingTimeLimits(const Value: TShowFloatLimit);
    procedure SetWhereToPlot(const Value: TWhereToPlot);
    procedure SetParticleGroupLimits(const Value: TShowIntegerLimit);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TEndpointShowChoice read FShowChoice
      write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property StartColumnLimits: TShowIntegerLimit read FStartColumnLimits
      write SetStartColumnLimits;
    property StartRowLimits: TShowIntegerLimit read FStartRowLimits
      write SetStartRowLimits;
    property StartLayerLimits: TShowIntegerLimit read FStartLayerLimits
      write SetStartLayerLimits;
    property StartZoneLimits: TShowIntegerLimit read FStartZoneLimits
      write SetStartZoneLimits;
    property ReleaseTimeLimits: TShowFloatLimit read FReleaseTimeLimits
      write SetReleaseTimeLimits;
    property EndColumnLimits: TShowIntegerLimit read FEndColumnLimits
      write SetEndColumnLimits;
    property EndRowLimits: TShowIntegerLimit read FEndRowLimits
      write SetEndRowLimits;
    property EndLayerLimits: TShowIntegerLimit read FEndLayerLimits
      write SetEndLayerLimits;
    property EndZoneLimits: TShowIntegerLimit read FEndZoneLimits
      write SetEndZoneLimits;
    property TrackingTimeLimits: TShowFloatLimit read FTrackingTimeLimits
      write SetTrackingTimeLimits;
    property WhereToPlot: TWhereToPlot read FWhereToPlot write SetWhereToPlot
      default wtpEnd;
    property ParticleGroupLimits: TShowIntegerLimit read FParticleGroupLimits
      write SetParticleGroupLimits;
  end;

  TEndPoint = class(TCollectionItem)
  private
    FStartTimeStep: integer;
    FStartXPrime: double;
    FStartYPrime: double;
    FEndTimeStep: integer;
    FEndXPrime: double;
    FEndYPrime: double;
    FTerminationCode: integer;
    FReleaseTime: double;
    FTrackingTime: double;
    FStartColumn: integer;
    FStartRow: integer;
    FStartLayer: integer;
    FStartLocalZ: double;
    FStartX: double;
    FStartY: double;
    FStartZ: double;
    FStartZoneCode: integer;
    FEndColumn: integer;
    FEndRow: integer;
    FEndLayer: integer;
    FEndX: double;
    FEndY: double;
    FEndZ: double;
    FEndLocalZ: double;
    FEndZoneCode: integer;
    FParticleNumber: Integer;
  protected
    function CheckLimits(Limits: TEndPointDisplayLimits): boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TEndPointDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
      Segment: TSegment2D; DisvUsed: Boolean; WhereToPlot: TWhereToPlot): boolean;
  published
    // X position in grid coordinates of end point;
    property EndXPrime: double read FEndXPrime write FEndXPrime;
    // Y position in grid coordinates of end point;
    property EndYPrime: double read FEndYPrime write FEndYPrime;
    // X position in grid coordinates of starting point;
    property StartXPrime: double read FStartXPrime write FStartXPrime;
    // Y position in grid coordinates of starting point;
    property StartYPrime: double read FStartYPrime write FStartYPrime;
    property StartTimeStep: integer read FStartTimeStep write FStartTimeStep;
    property EndTimeStep: integer read FEndTimeStep write FEndTimeStep;
    property ParticleNumber: Integer read FParticleNumber write FParticleNumber;
    { Termination Code in version 5 equivalent to Status in versions 6 and 7

     The meaning of the Termination code is different in MODPATH version 5
     and MODPATH version 6.
     Version 5:
       IDCODE = -2; particle is unreleased.
       IDCODE = -1; particle stranded in inactive (dry) cell.
       IDCODE = 0; particle remains active.
       IDCODE = 1; particle discharged normally.
       IDCODE = 2; particle stopped in a specified zone.
     Version 6:
       Pending : Status = 0. Particles that are scheduled to be released but
         have not yet been released. At the start of a simulation, all
         particles have a status of pending.
       Active : Status = 1. Particles that are actively moving in the flow
         system and have not yet reached a termination location.
       NormallyTerminated : Status = 2. Particles that have terminated at a
         boundary or internally at a cell with an internal source/sink.
       ZoneTerminated : Status = 3. Particles that terminated at a cell with
         a specified zone number indicating automatic termination.
       Unreleased : Status = 4. Particles that were not released and were
         tagged as permanently unreleased. The most common situation that
         results in unreleased particles is a dry or inactive cell condition
         at the scheduled release time.
       Stranded : Status = 5. Particles that remain in cells after the cell
         goes dry. Stranded particles sometimes occur in transient simulations.
         Once a particle is stranded, it cannot be reactivated and is
         considered terminated.
     Version 7
        0 = Pending release
        1 = Active
        2 = Terminated at boundary face
        3 = Terminated in weak sink cell
        4 = Terminated in weak source cell
        5 = Terminated in cell with no exit face
        6 = Terminated in cell with specified zone number
        7 = Terminated in an inactive cell
        8 = Permanently unreleased
        9 = Terminated for unknown reason
    }
    property TerminationCode: integer read FTerminationCode write FTerminationCode;
    // equals Initial tracking time in versions 6 and 7
    property ReleaseTime: double read FReleaseTime write FReleaseTime;
    // equals final tracking time in versions 6 and 7
    property TrackingTime: double read FTrackingTime write FTrackingTime;
    property StartColumn: integer read FStartColumn write FStartColumn;
    property StartRow: integer read FStartRow write FStartRow;
    // equals initial layer in versions 6 and 7
    property StartLayer: integer read FStartLayer write FStartLayer;
    // equals initial local z in versions 6 and 7
    property StartLocalZ: double read FStartLocalZ write FStartLocalZ;
    // Real world X coordinate of starting point
    property StartX: double read FStartX write FStartX;
    // Real world Y coordinate of starting point
    property StartY: double read FStartY write FStartY;
    property StartZ: double read FStartZ write FStartZ;
    property StartZoneCode: integer read FStartZoneCode write FStartZoneCode;
    property EndColumn: integer read FEndColumn write FEndColumn;
    property EndRow: integer read FEndRow write FEndRow;
    property EndLayer: integer read FEndLayer write FEndLayer;
    // Real world X coordinate of end point
    property EndX: double read FEndX write FEndX;
    // Real world Y coordinate of end point
    property EndY: double read FEndY write FEndY;
    property EndZ: double read FEndZ write FEndZ;
    property EndLocalZ: double read FEndLocalZ write FEndLocalZ;
    property EndZoneCode: integer read FEndZoneCode write FEndZoneCode;
  end;

  TEndPointV6 = class(TEndPoint)
  private
    FParticleGroup: integer;
    FInitialCellFace: Integer;
    FFinalCellFace: Integer;
    FParticleLabel: string;
  protected
    function CheckLimits(Limits: TEndPointDisplayLimits): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ParticleGroup: integer read FParticleGroup write FParticleGroup;
    property InitialCellFace: Integer read FInitialCellFace write FInitialCellFace;
    property FinalCellFace: Integer read FFinalCellFace write FFinalCellFace;
    property ParticleLabel: string read FParticleLabel write FParticleLabel;
  end;

  TEndPointV7 = class(TEndPointV6)
  private
//    FParticleGroup: integer;
//    FInitialCellFace: Integer;
//    FFinalCellFace: Integer;
    FSequenceNumber: Integer;
    FInitialCellNumber: Integer;
    FInitialLocalX: double;
    FInitialLocalY: double;
    FFinalCellNumber: Integer;
    FFinalLocalX: double;
    FFinalLocalY: double;
  protected
    function CheckLimits(Limits: TEndPointDisplayLimits): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SequenceNumber: Integer read FSequenceNumber write FSequenceNumber;
//    property ParticleGroup: integer read FParticleGroup write FParticleGroup;
//    property InitialCellFace: Integer read FInitialCellFace write FInitialCellFace;
//    property FinalCellFace: Integer read FFinalCellFace write FFinalCellFace;
    property InitialCellNumber: Integer read FInitialCellNumber write FInitialCellNumber;
    property InitialLocalX: double read FInitialLocalX write FInitialLocalX;
    property InitialLocalY: double read FInitialLocalY write FInitialLocalY;
    property FinalCellNumber: Integer read FFinalCellNumber write FFinalCellNumber;
    property FinalLocalX: double read FFinalLocalX write FFinalLocalX;
    property FinalLocalY: double read FFinalLocalY write FFinalLocalY;
  end;

  TModpathVersion = (mpv5, mpv6, mpv7);

  TCustomEndPoints = class(TCollection)
  private
    function GetPoint(Index: integer): TEndPoint;
    procedure ExportShapefileAtStartingLocations(FileName: string);
    procedure ExportShapefileAtEndingLocations(FileName: string);
  protected
//    function GetVersion: TModpathVersion; virtual; abstract;
  public
//    property Version: TModpathVersion read GetVersion;
    property Points[Index: integer]: TEndPoint read GetPoint; default;
  end;

  TEndPoints = class(TCustomEndPoints)
  private
    function GetPoint(Index: integer): TEndPoint;
  protected
//    function GetVersion: TModpathVersion; override;
  public
    Constructor Create;
    property Points[Index: integer]: TEndPoint read GetPoint; default;
  end;

  TEndPointsV6 = class(TCustomEndPoints)
  private
    function GetPoint(Index: integer): TEndPointV6;
  protected
//    function GetVersion: TModpathVersion; override;
  public
    Constructor Create;
    property Points[Index: integer]: TEndPointV6 read GetPoint; default;
  end;

  TEndPointsV7 = class(TCustomEndPoints)
  private
    function GetPoint(Index: integer): TEndPointV7;
  protected
//    function GetVersion: TModpathVersion; override;
  public
    Constructor Create;
    property Points[Index: integer]: TEndPointV7 read GetPoint; default;
  end;

  TEndpointColorLimitChoice = (elcNone, elcReleaseTime, elcTrackingTime,
    elcLogTrackingTime,
    elcStartXPrime, elcStartYPrime, elcStartZ, elcStartZone,
    elcEndXPrime, elcEndYPrime, elcEndZ, elcEndZone, elcParticleGroup);

  TEndPointColorLimits = class(TPersistent)
  private
    FColoringChoice: TEndpointColorLimitChoice;
    FMinColorLimit: double;
    FUseLimit: boolean;
    FMaxColorLimit: double;
    procedure SetColoringChoice(const Value: TEndpointColorLimitChoice);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetMinColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TEndpointColorLimitChoice read FColoringChoice
      write SetColoringChoice default elcTrackingTime;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TEndPointSettings = class(TCustomModpathSettings)
  private
    FColorLimits: TEndPointColorLimits;
    FDisplayLimits: TEndPointDisplayLimits;
    FLegendVisible: boolean;
    FEndPointSize: Integer;
    procedure SetColorLimits(const Value: TEndPointColorLimits);
    procedure SetDisplayLimits(const Value: TEndPointDisplayLimits);
    procedure SetLegendVisible(const Value: boolean);
    procedure SetEndPointSize(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ColorLimits: TEndPointColorLimits read FColorLimits write
      SetColorLimits;
    property DisplayLimits: TEndPointDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property DisplayEndPoints: boolean read FVisible
      write FVisible Stored False;
    property LegendVisible: boolean read FLegendVisible write SetLegendVisible
      default True;
    property EndPointSize: Integer read FEndPointSize write SetEndPointSize;
  end;

  TEndPointReader = class(TEndPointSettings, IDisplayer)
  private
    class var
      FListInitialized: Boolean;
      FEndPointGLIndex: Cardinal;
  private
    FPoints: TEndPoints;
    FFileName: string;
    FFileDate: TDateTime;
    FMaxTrackingTime: double;
    FMinTrackingTime: double;
    FMinReleaseTime: double;
    FMaxReleaseTime: double;
    FMinStartZone: integer;
    FMaxEndZone: integer;
    FMinEndZone: integer;
    FMaxStartZone: integer;
    FDrawingEndPoints: Boolean;
    FRecordedEndPoints: Boolean;
    FTopQuadTree: TRbwQuadTree;
    FFrontQuadTree: TRbwQuadTree;
    FSideQuadTree: TRbwQuadTree;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FPointsV6: TEndPointsV6;
    FModpathVersion: TPathlineVersion;
    FMaxParticleGroup: integer;
    FMinParticleGroup: integer;
    FMinPositiveTrackingTime: double;
    FPointsV7: TEndPointsV7;
    FSelectedLayer: Integer;
    FSelectedRow: Integer;
    FSelectedColumn: Integer;
    procedure SetPoints(const Value: TEndPoints);
    procedure SetFileDate(const Value: TDateTime);
    class function GetEndPointGLIndex: GLuint; static;
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function GetPointColor(MaxValue, MinValue: double;
      Point: TEndPoint): TColor;
    procedure SetMaxTrackingTime(const Value: double);
    procedure SetTrackingMinTime(const Value: double);
    procedure SetMaxReleaseTime(const Value: double);
    procedure SetMinReleaseTime(const Value: double);
    procedure SetMaxEndZone(const Value: integer);
    procedure SetMaxStartZone(const Value: integer);
    procedure SetMinEndZone(const Value: integer);
    procedure SetMinStartZone(const Value: integer);
    procedure Record3DEndPoints;
    procedure SetPointsV6(const Value: TEndPointsV6);
    procedure ReadFileV5;
    procedure ReadFileV6;
    procedure ReadFileV7;
    procedure SetMinMaxValues(LocalPoints: TCustomEndPoints);
    procedure SetMaxParticleGroup(const Value: integer);
    procedure SetMinParticleGroup(const Value: integer);
    procedure SetMinPositiveTrackingTime(const Value: double);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetHasData: Boolean;
    procedure SetPointsV7(const Value: TEndPointsV7);
  protected
    class property EndPointGLIndex: GLuint read GetEndPointGLIndex;
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TPersistent);
    procedure Draw3D;
    procedure Invalidate;
    procedure ReadFile;
    property TopQuadTree: TRbwQuadTree read FTopQuadTree;
    property FrontQuadTree: TRbwQuadTree read FFrontQuadTree;
    property SideQuadTree: TRbwQuadTree read FSideQuadTree;
    procedure ExportShapefileAtStartingLocations(FileName: string);
    procedure ExportShapefileAtEndingLocations(FileName: string);
    // @name resets the minimum and maximum values of @link(Points) and
    // @link(PointsV6).
    procedure UpdateMinMax;
    property HasData: Boolean read GetHasData;
    procedure Clear;
  published
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property FileName: string read FFileName write FFileName;
    property MinReleaseTime: double read FMinReleaseTime
      write SetMinReleaseTime;
    property MaxReleaseTime: double read FMaxReleaseTime
      write SetMaxReleaseTime;
    property MinTrackingTime: double read FMinTrackingTime
      write SetTrackingMinTime;
    property MinPositiveTrackingTime: double read FMinPositiveTrackingTime
      write SetMinPositiveTrackingTime;
    property MaxTrackingTime: double read FMaxTrackingTime
      write SetMaxTrackingTime;
    property MinStartZone: integer read FMinStartZone write SetMinStartZone;
    property MaxStartZone: integer read FMaxStartZone write SetMaxStartZone;
    property MinEndZone: integer read FMinEndZone write SetMinEndZone;
    property MaxEndZone: integer read FMaxEndZone write SetMaxEndZone;
    property MinParticleGroup: integer read FMinParticleGroup write SetMinParticleGroup;
    property MaxParticleGroup: integer read FMaxParticleGroup write SetMaxParticleGroup;
    // If an additional points type is added, be sure to update GetHasData
    // export to Shapefile.
    property Points: TEndPoints read FPoints write SetPoints;
    property PointsV6: TEndPointsV6 read FPointsV6 write SetPointsV6;
    property PointsV7: TEndPointsV7 read FPointsV7 write SetPointsV7;
    property ModpathVersion: TPathlineVersion read FModpathVersion
      write FModpathVersion;
  end;

  TEndPointObjectList = TObjectList<TEndPointReader>;

  TTimeSeriesDisplayLimits = class(TPersistent)
  private
    FShowChoice: TShowChoice;
    FLayerLimits: TShowIntegerLimit;
    FLimitToCurrentIn2D: boolean;
    FRowLimits: TShowIntegerLimit;
    FColumnLimits: TShowIntegerLimit;
    FParticleGroupLimits: TShowIntegerLimit;
    procedure SetShowChoice(const Value: TShowChoice);
    procedure SetColumnLimits(const Value: TShowIntegerLimit);
    procedure SetLayerLimits(const Value: TShowIntegerLimit);
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetRowLimits(const Value: TShowIntegerLimit);
    procedure SetParticleGroupLimits(const Value: TShowIntegerLimit);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TShowChoice read FShowChoice write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property ColumnLimits: TShowIntegerLimit read FColumnLimits
      write SetColumnLimits;
    property RowLimits: TShowIntegerLimit read FRowLimits write SetRowLimits;
    property LayerLimits: TShowIntegerLimit read FLayerLimits
      write SetLayerLimits;
    property ParticleGroupLimits: TShowIntegerLimit read FParticleGroupLimits
      write SetParticleGroupLimits;
  end;

  TTimeSeriesPoint = class(TCollectionItem)
  private
    FTimeStepIndex: integer;
    FParticleIndex: integer;
    FLayer: integer;
    FTrackingTime: double;
    FTimeStep: integer;
    FLocalZ: double;
    FZ: double;
    FX: double;
    FY: double;
    FXPrime: double;
    FRow: integer;
    FYPrime: double;
    FColumn: integer;
  protected
    function CheckLimits(Limits: TTimeSeriesDisplayLimits): boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TTimeSeriesDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
      const Segment: TSegment2D; const DisvUsed: Boolean): boolean;
    function ShouldShowSeries(Limits: TTimeSeriesDisplayLimits): boolean;
  published
    // Time point index
    property TimeStepIndex: integer read FTimeStepIndex write FTimeStepIndex;
    property ParticleIndex: integer read FParticleIndex write FParticleIndex;
    property Layer: integer read FLayer write FLayer;
    property Row: integer read FRow write FRow;
    property Column: integer read FColumn write FColumn;
    property XPrime: double read FXPrime write FXPrime;
    property YPrime: double read FYPrime write FYPrime;
    property X: double read FX write FX;
    property Y: double read FY write FY;
    property Z: double read FZ write FZ;
    property LocalZ: double read FLocalZ write FLocalZ;
    property TrackingTime: double read FTrackingTime write FTrackingTime;
    // cumulative time step
    property TimeStep: integer read FTimeStep write FTimeStep;
  end;

  TTimeSeriesPointV6 = class(TTimeSeriesPoint)
  private
    FParticleGroup: integer;
  protected
    function CheckLimits(Limits: TTimeSeriesDisplayLimits): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ParticleGroup: integer read FParticleGroup write FParticleGroup;
  end;

  TTimeSeriesPointV7 = class(TTimeSeriesPointV6)
  private
    FSequenceNumber: integer;
    FCellNumber: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SequenceNumber: integer read FSequenceNumber write FSequenceNumber;
    property CellNumber: integer read FCellNumber write FCellNumber;
  end;
  
  
  TCustomTimeSeriesPoints = class(TCollection)
  private
    function GetPoint(Index: integer): TTimeSeriesPoint;
  public
    property Points[Index: integer]: TTimeSeriesPoint read GetPoint; default;
  end;

  TTimeSeriesPoints = class(TCustomTimeSeriesPoints)
  public
    Constructor Create;
  end;

  TTimeSeriesPointsV6 = class(TCustomTimeSeriesPoints)
  private
    function GetPoint(Index: integer): TTimeSeriesPointV6;
  public
    Constructor Create;
    property Points[Index: integer]: TTimeSeriesPointV6 read GetPoint; default;
  end;

  TTimeSeriesPointsV7 = class(TCustomTimeSeriesPoints)
  private
    function GetPoint(Index: integer): TTimeSeriesPointV7;
  public
    Constructor Create;
    property Points[Index: integer]: TTimeSeriesPointV7 read GetPoint; default;
  end;

  TCustomTimeSeries = class(TCollectionItem)
  private
    FTimes: TRealList;
    function GetTimes: TRealList; virtual; abstract;
    procedure SetTimes(const Value: TRealList);
  protected
    function GetPoints: TCustomTimeSeriesPoints; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Times: TRealList read GetTimes write SetTimes;
    property Points: TCustomTimeSeriesPoints read GetPoints;
  end;

  // @name represents the position of a particle at different times.
  TTimeSeries = class(TCustomTimeSeries)
  private
    FPoints: TTimeSeriesPoints;
    function GetTimes: TRealList; override;
    procedure SetPoints(const Value: TTimeSeriesPoints);
  protected
    function GetPoints: TCustomTimeSeriesPoints; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Points: TTimeSeriesPoints read FPoints write SetPoints;
  end;

  TTimeSeriesV6 = class(TCustomTimeSeries)
  private
    FPoints: TTimeSeriesPointsV6;
    function GetTimes: TRealList; override;
    procedure SetPoints(const Value: TTimeSeriesPointsV6);
  protected
    function GetPoints: TCustomTimeSeriesPoints; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Points: TTimeSeriesPointsV6 read FPoints write SetPoints;
  end;

  TTimeSeriesV7 = class(TCustomTimeSeries)
  private
    FPoints: TTimeSeriesPointsV7;
    function GetTimes: TRealList; override;
    procedure SetPoints(const Value: TTimeSeriesPointsV7);
  protected
    function GetPoints: TCustomTimeSeriesPoints; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Points: TTimeSeriesPointsV7 read FPoints write SetPoints;
  end;

  TCustomTimeSeriesCollection = class(TCollection)
  private
    function GetSeries(Index: integer): TCustomTimeSeries;
  public
    property Series[Index: integer]: TCustomTimeSeries read GetSeries; default;
  end;

  TTimeSeriesCollection = class(TCustomTimeSeriesCollection)
  private
    function GetSeries(Index: integer): TTimeSeries;
  public
    Constructor Create;
    property Series[Index: integer]: TTimeSeries read GetSeries; default;
  end;

  TTimeSeriesCollectionV6 = class(TCustomTimeSeriesCollection)
  private
    function GetSeries(Index: integer): TTimeSeriesV6;
  public
    Constructor Create;
    property Series[Index: integer]: TTimeSeriesV6 read GetSeries; default;
  end;

  TTimeSeriesCollectionV7 = class(TCustomTimeSeriesCollection)
  private
    function GetSeries(Index: integer): TTimeSeriesV7;
  public
    Constructor Create;
    property Series[Index: integer]: TTimeSeriesV7 read GetSeries; default;
  end;

  TTimeSeriesColorLimitChoice = (tscNone, tscParticleNumber,
    tscXPrime, tscYPrime, tscZ,
    tscStartXPrime, tscStartYPrime, tscStartZ,
    tscEndXPrime, tscEndYPrime, tscEndZ, tscGroup);

  TTimeSeriesColorLimits = class(TPersistent)
  private
    FColoringChoice: TTimeSeriesColorLimitChoice;
    FMinColorLimit: double;
    FUseLimit: boolean;
    FMaxColorLimit: double;
    procedure SetColoringChoice(const Value: TTimeSeriesColorLimitChoice);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetMinColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TTimeSeriesColorLimitChoice read FColoringChoice
      write SetColoringChoice default tscParticleNumber;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TTimeSeriesSettings = class(TCustomModpathSettings)
  private
    FColorLimits: TTimeSeriesColorLimits;
    FDisplayLimits: TTimeSeriesDisplayLimits;
    FTimeSeriesSize: Integer;
    procedure SetColorLimits(const Value: TTimeSeriesColorLimits);
    procedure SetDisplayLimits(const Value: TTimeSeriesDisplayLimits);
    procedure SetTimeSeriesSize(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    Destructor Destroy; override;
  published
    property ColorLimits: TTimeSeriesColorLimits read FColorLimits
      write SetColorLimits;
    property DisplayLimits: TTimeSeriesDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property DisplayTimeSeries: boolean read FVisible
      write FVisible Stored False;
    property TimeSeriesSize: Integer read FTimeSeriesSize write SetTimeSeriesSize;
  end;

  TTimeSeriesReader = class(TTimeSeriesSettings)
  private
    FFileName: string;
    FFileDate: TDateTime;
    FSeries: TTimeSeriesCollection;
    FSeriesV6: TTimeSeriesCollectionV6;
    FSeriesV7: TTimeSeriesCollectionV7;
    FMaxTime: double;
    FMinTime: double;
    FTimeIndex: integer;
    FDrawingTimeSeries: Boolean;
    FTimeSeriesGLIndex: array of GLuint;
    FRecordedTimeSeries: array of Boolean;
    FTimes: TRealList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FMaxParticleGroup: Integer;
    FMinParticleGroup: Integer;
    procedure SetFileDate(const Value: TDateTime);
    procedure SetLines(const Value: TTimeSeriesCollection);
    procedure SetMaxTime(const Value: double);
    procedure SetMinTime(const Value: double);
    procedure SetTimeIndex(const Value: integer);
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function CheckShowSeries(Series: TCustomTimeSeries): Boolean;
    function GetPointColor(MaxValue, MinValue: double;
      Point: TTimeSeriesPoint): TColor;
    function GetRecordedTimeSeries(ATimeIndex: integer): boolean;
    procedure SetRecordedTimeSeries(ATimeIndex: integer; const Value: boolean);
    function GetTimeSeriesGLIndex(ATimeIndex: integer): GLuint;
    procedure Record3DTimeSeries(TimeIndex: integer);
    procedure EnsureGLArrays(ATimeIndex: Integer);
    function GetTimes: TRealList;
    procedure SetTimes(const Value: TRealList);
    procedure SetLinesV6(const Value: TTimeSeriesCollectionV6);
    procedure ReadFileV5;
    procedure ReadFileV6;
    procedure ReadFileV7;
    procedure SetMaxParticleGroup(const Value: Integer);
    procedure SetMinParticleGroup(const Value: Integer);
    procedure FixTimeIndex;
    function GetTimeIndex: integer;
    procedure SetLinesV7(const Value: TTimeSeriesCollectionV7);
    function GetHasData: Boolean;
    function GetActiveSeries: TCustomTimeSeriesCollection;
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure ReadFile;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TPersistent);
    procedure Draw3D;
    property RecordedTimeSeries[ATimeIndex: integer]: boolean read
      GetRecordedTimeSeries write SetRecordedTimeSeries;
    property TimeSeriesGLIndex[ATimeIndex: integer]: GLuint
      read GetTimeSeriesGLIndex;
    property Times: TRealList read GetTimes write SetTimes;
    procedure Invalidate;
    procedure ExportShapefile(FileName: string);
    property HasData: Boolean read GetHasData;
    property ActiveSeries: TCustomTimeSeriesCollection read GetActiveSeries;
  published
    property FileName: string read FFileName write FFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    // If an additional series type is added, be sure to update GetHasData.
    // and ExportShapefile
    property Series: TTimeSeriesCollection read FSeries write SetLines;
    property SeriesV6: TTimeSeriesCollectionV6 read FSeriesV6 write SetLinesV6;
    property SeriesV7: TTimeSeriesCollectionV7 read FSeriesV7 write SetLinesV7;
    property MaxTime: double read FMaxTime write SetMaxTime;
    property MinTime: double read FMinTime write SetMinTime;
    property TimeIndex: integer read GetTimeIndex write SetTimeIndex;
    property MinParticleGroup: Integer read FMinParticleGroup write SetMinParticleGroup;
    property MaxParticleGroup: Integer read FMaxParticleGroup write SetMaxParticleGroup;
  end;

  TimeSeriesObjectList = TObjectList<TTimeSeriesReader>;

  function GetPathlineVersion(const FileName: string): TPathlineVersion;

implementation

uses
  frmGoPhastUnit, ZoomBox2, ModflowGridUnit, BigCanvasMethods,
  ModelMuseUtilities, frmExportShapefileUnit,
  ShapefileUnit, AbstractGridUnit, Dialogs, System.Math, PhastModelUnit,
  ModflowIrregularMeshUnit, LayerStructureUnit;

resourcestring
  StrAbortingTheNumber = 'Aborting. The number of endpoints is zero.';
  StrThisDoesNotAppear = 'This does not appear to be a valid pathline file.';
  StrPathlinesAreColore = 'Pathlines are colored with time rather than log10' +
  '(time) because all times are less than or equal to zero.';
  StrErrorReadingTimese = 'Error reading timeseries file on line %0:d. Error' +
  ' message is "%1:s".';
  StrErrorReadingEndpoi = 'Error reading endpoint file on line %0:d. Error m' +
  'essage is "%1:s".';
  StrErrorReadingPathli = 'Error reading pathline file on line %0:d. Error m' +
  'essage is "%1:s".';
  StrThisTimeSeriesFil = 'This %s file contains an invalid layer nu' +
  'mber. Check to make sure that this file was generated by this model.';
  StrErrorPrematureTer = 'Error: premature termination of file when reading ' +
  'line %d. Check that MODPATH did not crash.';
  StrThereWasAnErrorR = 'There was an error reading this %0:s file. The erro' +
  'r message was "%1:s". If the value that could not be converted was "NaN" ' +
  '(not a number), that means that MODPATH probably attempted an illegal ' +
  'operation such as dividing by zero. That probably means there is ' +
  'something wrong with how you have set up MODPATH in your model.';
  StrThereWasAnErrorW = 'There was an error when reading the file. The error' +
  ' message was "%s"';
  StrErrorReadingTheFo = 'Error reading  line %0:d from what is supposed to ' +
  'be a MODPATH 7 pathline file. This line was "%:ss". ';
  StrProgrammingErrorN = 'Programming error: no MODPATH file type selected.';
  StrThereIsSomethingW = 'There is something wrong with the PATHLINE file. P' +
  'lease contact the developer for support.';
  StrThereWasAnErrorReading = 'There was an error reading this %0:s file. Th' +
  'e error message was "%1:s" Contact the ModelMuse developer for more assis' +
  'tance.';
  StrTheFollowingLines = 'The following lines from the file could not be rea' +
  'd correctly and have been skipped. Only up to 10 lines will be displayed.';

const
  StrSTARTLAY: AnsiString = 'START_LAY';
  StrSTARTROW: AnsiString = 'START_ROW';
  StrSTARTCOL: AnsiString = 'START_COL';
  StrSTARTTIME: AnsiString = 'START_TIME';
  StrENDLAY: AnsiString = 'END_LAY';
  StrENDROW: AnsiString = 'END_ROW';
  StrENDCOL: AnsiString = 'END_COL';
  StrENDTIME: AnsiString = 'END_TIME';
  StrSTARTTS: AnsiString = 'START_TS';
  StrSTARTZONE: AnsiString = 'START_ZONE';
  StrENDTS: AnsiString = 'END_TS';
  StrENDZONE: AnsiString = 'END_ZONE';
  StrENDX: AnsiString = 'END_X';
  StrENDY: AnsiString = 'END_Y';
  StrENDZ: AnsiString = 'END_Z';
  StrTRACKTIME: AnsiString = 'TRACK_TIME';
  StrTERMCODE: AnsiString = 'TERM_CODE';
  StrRELEASET: AnsiString = 'RELEASE_T';
  StrSTARTX: AnsiString = 'START_X';
  StrSTARTY: AnsiString = 'START_Y';
  StrSTARTZ: AnsiString = 'START_Z';
  StrLAYER: AnsiString = 'LAYER';
  StrROW: AnsiString = 'ROW';
  StrCOLUMN: AnsiString = 'COLUMN';
  StrTIMESTEP: AnsiString = 'TIME_STEP';
  StrPARTICLE: AnsiString = 'PARTICLE';
  StrParticleGroup: AnsiString = 'PARTICLE_GROUP';

function DisplayDisvPlot(Model: TCustomModel;
  Orientation: TDataSetOrientation): Boolean;
var
  CrossSectionSegment: TSegment2D;
begin
  result := True;
  if Model.DisvUsed and (Orientation = dsoFront) then
  begin
    CrossSectionSegment := Model.DisvGrid.CrossSection.Segment;
    if (CrossSectionSegment[1].x = CrossSectionSegment[2].x)
      and (CrossSectionSegment[1].y = CrossSectionSegment[2].y) then
    begin
      result := False;
    end;
  end;
end;

function GetOriginOffset(CrossSectionSegment: TSegment2D): Double;
var
  SegmentLine: TLine2D;
  SegmentAngle: Double;
  ClosestPoint: TPoint2D;
  OriginAngle: Double;
begin
  SegmentLine := EquateLine(CrossSectionSegment[1], CrossSectionSegment[2]);
  SegmentAngle := ArcTan2(CrossSectionSegment[1].Y - CrossSectionSegment[2].y,
    CrossSectionSegment[1].x - CrossSectionSegment[2].x);
  ClosestPoint := ClosestPointOnLineFromPoint(SegmentLine,
    EquatePoint(0.0, 0.0));
  result := Distance(ClosestPoint, CrossSectionSegment[1]);
  if result <> 0 then
  begin
    OriginAngle := ArcTan2(CrossSectionSegment[1].Y - ClosestPoint.y,
      CrossSectionSegment[1].x - ClosestPoint.x);
    if Abs(SegmentAngle - OriginAngle) > 0.001 then
    begin
      result := -result;
    end;
  end;
end;

function DisvFrontProjectedXPrime(const CrossSectionSegment: TSegment2D;
  APoint: TPoint2d; out XPrime: double): Boolean;
var
  ProjectedPoint: TPoint2D;
begin
  ProjectedPoint := ClosestPointOnSegmentFromPoint(CrossSectionSegment, APoint);
  result := NotEqual(ProjectedPoint, CrossSectionSegment[1])
    and NotEqual(ProjectedPoint, CrossSectionSegment[2]);
  XPrime := Distance(ProjectedPoint, CrossSectionSegment[1]);
end;

procedure ConvertIndicies(NCol, NRow: Integer;
  var I, K, J: Integer);
begin
  K := (J div (NRow * NCol)) + 1;
  J := J - (K - 1) * (NRow * NCol);
  I := (J div NCol) + 1;
  J := J - (I - 1) * NCol;
  if J = 0 then
  begin
    J := NCol;
    Dec(I);
    if I = 0 then
    begin
      I := NRow;
      Dec(K);
    end;
  end;
end;

procedure ConvertCoordinates(Grid: TModflowGrid; var XPrime, YPrime: single;
  var Point2D: TPoint2D); overload;
begin
  // need to convert X and Y to real world coordinates.
  XPrime := XPrime + Grid.ColumnPosition[0];
  YPrime := YPrime + Grid.RowPosition[Grid.RowCount];
  Point2D.X := XPrime;
  Point2D.Y := YPrime;
  Point2D := Grid.RotateFromGridCoordinatesToRealWorldCoordinates(Point2D);
end;

procedure ConvertCoordinates(Grid: TModflowGrid; var XPrime, YPrime: double;
  var Point2D: TPoint2D); overload;
begin
  // need to convert X and Y to real world coordinates.
  XPrime := XPrime + Grid.ColumnPosition[0];
  YPrime := YPrime + Grid.RowPosition[Grid.RowCount];
  Point2D.X := XPrime;
  Point2D.Y := YPrime;
  Point2D := Grid.RotateFromGridCoordinatesToRealWorldCoordinates(Point2D);
end;

procedure AssignColor(AColor: TColor);
var
  Red: GLubyte;
  Green: GLubyte;
  Blue: GLubyte;
  Colors: array[0..3] of GLfloat;
begin
  ExtractColorComponents(AColor, Red, Green, Blue);

  Colors[0] := Red / 255;
  Colors[1] := Green / 255;
  Colors[2] := Blue / 255;
  Colors[3] := 1;

  glColor3ub(Red, Green, Blue);

//    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);
//    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 0.7);
//    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Colors);
end;

function AnsiCharArrayToStr(CharArray: TAnsiCharArray): string;
var
  StringBuilder: TStringBuilder;
  Index: Integer;
begin
  StringBuilder := TStringBuilder.Create;
  try
    StringBuilder.Capacity := Length(CharArray);
    for Index := 0 to Length(CharArray) - 1 do
    begin
      StringBuilder.Append(CharArray[Index]);
    end;
    result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;


function GetPathlineVersion(const FileName: string): TPathlineVersion;
const
  MP6 = 'MODPATH_PATHLINE_FILE 6 0';
  MP7 = 'MODPATH_PATHLINE_FILE         7         2';
var
  AFile: TFileStream;
  CharArray: TAnsiCharArray;
  AString: AnsiString;
begin
  Assert(FileExists(FileName));
  AFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(CharArray, Length(MP7));
    AFile.Read(CharArray[0], Length(MP7) * SizeOf(AnsiChar));
    AString := AnsiString(AnsiCharArrayToStr(CharArray));
    if AString = MP7 then
    begin
      result := pv7_2;
    end
	else if Copy(AString, 1, Length(MP6)) = MP6 then
	begin
      result := pv6_0;
	end
    else if (Length(AString) > 0) and (AString[1] = '@') then
    begin
      result := pv5;
    end
    else
    begin
      result := pvUnknown;
    end;
  finally
    AFile.Free;
  end;
end;

function GetEndpointVersion(const FileName: string): TPathlineVersion;
const
  MP6 = 'MODPATH_ENDPOINT_FILE 6 0';
  MP7 = 'MODPATH_ENDPOINT_FILE         7         2';
var
  AFile: TFileStream;
  CharArray: TAnsiCharArray;
  AString: AnsiString;
begin
  Assert(FileExists(FileName));
  AFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(CharArray, Length(MP7));
    AFile.Read(CharArray[0], Length(MP7) * SizeOf(AnsiChar));
    AString := AnsiString(AnsiCharArrayToStr(CharArray));
	
    if AString = MP7 then
    begin
      result := pv7_2;
    end
    else
    begin
      AString := copy(AString, 1, Length(MP6));
      if AString = MP6 then
      begin
        result := pv6_0;
      end
      else
      begin
        result := pv5;
      end;
    end;
  finally
    AFile.Free;
  end;
end;

function GetTimeSeriesVersion(const FileName: string): TPathlineVersion;
const
  MP6 = 'MODPATH_TIMESERIES_FILE 6 0';
  MP7 = 'MODPATH_TIMESERIES_FILE         7         2';
var
  AFile: TFileStream;
  CharArray: TAnsiCharArray;
  AString: AnsiString;
begin
  Assert(FileExists(FileName));
  AFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(CharArray, Length(MP7));
    AFile.Read(CharArray[0], Length(MP7) * SizeOf(AnsiChar));
    AString := AnsiString(AnsiCharArrayToStr(CharArray));
    if AString = MP7 then
    begin
      result := pv7_2;
    end
	else if Copy(AString, 1, Length(MP6)) = MP6 then
	begin
      result := pv6_0;
	end
    else
    begin
      result := pv5;
    end;
  finally
    AFile.Free;
  end;
end;

{ TPathLine }

constructor TPathLine.Create(Collection: TCollection);
begin
  inherited;
  FPoints := TPathLinePoints.Create(self);
end;

procedure TCustomPathLine.Assign(Source: TPersistent);
var
  SourcePathLines: TCustomPathLine;
begin
  if Source is TCustomPathLine then
  begin
    SourcePathLines := TCustomPathLine(Source);
    FPoints.Assign(SourcePathLines.FPoints);
    ParticleIndex := SourcePathLines.ParticleIndex;
  end
  else
  begin
    inherited;
  end;
end;

destructor TCustomPathLine.Destroy;
begin
  FPoints.Free;
  inherited;
end;

function TCustomPathLine.GetLength: Double;
begin
  result := FPoints.Length;
end;

procedure TCustomPathLine.SetParticleIndex(const Value: Integer);
begin
  FParticleIndex := Value;
end;

procedure TCustomPathLine.SetPoints(const Value: TCustomPathLinePoints);
begin
  FPoints.Assign(Value);
end;

{ TPathLineReader }

procedure TPathLineReader.Assign(Source: TPersistent);
var
  PathLineReader: TPathLineReader;
begin
  if Source is TPathLineReader then
  begin
    PathLineReader := TPathLineReader(Source);
    Lines := PathLineReader.Lines;
    LinesV6 := PathLineReader.LinesV6;
    LinesV7 := PathLineReader.LinesV7;
    FileName := PathLineReader.FileName;
    MinTime := PathLineReader.MinTime;
    MaxTime := PathLineReader.MaxTime;
    FileDate := PathLineReader.FileDate;
    ModpathVersion := PathLineReader.ModpathVersion;
    TrackingDirectionV6 := PathLineReader.TrackingDirectionV6;
    ReferenceTimeV6 := PathLineReader.ReferenceTimeV6;
    MinParticleGroup := PathLineReader.MinParticleGroup;
    MaxParticleGroup := PathLineReader.MaxParticleGroup;
    TopQuadTree.Clear;
    FrontQuadTree.Clear;
    SideQuadTree.Clear;
  end;
  inherited;
end;

constructor TPathLineReader.Create(Model: TBaseModel);
begin
  inherited Create;
  FModel := Model;
  FLinesV5 := TPathLines.Create;
  FLinesV6 := TPathLinesV6.Create;
  FLinesV7 := TPathLinesV7.Create;
  FTopQuadTree := TRbwQuadTree.Create(nil);
  FFrontQuadTree := TRbwQuadTree.Create(nil);
  FSideQuadTree := TRbwQuadTree.Create(nil);
end;

destructor TPathLineReader.Destroy;
begin
  FSideQuadTree.Free;
  FFrontQuadTree.Free;
  FTopQuadTree.Free;
  FLinesV7.Free;
  FLinesV6.Free;
  FLinesV5.Free;
  inherited;
end;

procedure TPathLineReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TPersistent);
begin
  if not DisplayDisvPlot(FModel as TCustomModel, Orientation) then
  begin
    Exit;
  end;

  case ModpathVersion of
    pv5:
      begin
        DrawLines(Orientation,BitMap,Lines);
      end;
    pv6_0:
      begin
        DrawLines(Orientation,BitMap,LinesV6);
      end;
    pv7_2:
      begin
        DrawLines(Orientation,BitMap,LinesV7);
      end;
    pvUnknown: ; // do nothing.
    else
      Assert(False);
  end;
end;

procedure TPathLineReader.Draw3D;
begin
  case ModpathVersion of
    pv5:
      begin
        DrawLines3D(Lines);
      end;
    pv6_0:
      begin
        DrawLines3D(LinesV6);
      end;
    pv7_2:
      begin
        DrawLines3D(LinesV7);
      end;
    pvUnknown: ; // do nothing.
    else
      Assert(False);
  end;
end;
function TPathLineReader.CheckShowLine(Line: TCustomPathLine): Boolean;
var
  APoint: TPathLinePoint;
begin
  result := True;
  case DisplayLimits.ShowChoice of
    scAll, scSpecified:
      begin
      end;
    scStart:
      begin
        APoint := Line.Points[0];
        result := APoint.ShouldShowLine(DisplayLimits);
      end;
    scEnd:
      begin
        APoint := Line.Points[Line.Points.Count - 1];
        result := APoint.ShouldShowLine(DisplayLimits);
      end;
  else
    Assert(False);
  end;
end;

procedure TPathLineReader.Clear;
begin
  Lines.Clear;
  LinesV6.Clear;
  LinesV7.Clear;
end;

function TPathLineReader.GetHasData: Boolean;
begin
  result := (Lines.Count > 0) or (LinesV6.Count > 0) or (LinesV7.Count > 0)
end;

function TPathLineReader.GetMaxLineNumber: integer;
begin
  result := -1;
  case ModpathVersion of
    pv5:
      begin
        result := Lines.count;
      end;
    pv6_0:
      begin
        result := LinesV6.count;
      end;
    pv7_2:
      begin
        result := LinesV7.count;
      end;
    pvUnknown:
      begin
        result := 0;
      end
    else
      Assert(False);
  end;
end;

procedure TPathLineReader.GetMinMaxValues(var MaxValue: Double;
  var MinValue: Double);
var
  Grid: TModflowGrid;
  LocalModel: TCustomModel;
  Disv: TModflowDisvGrid;
  MeshLmits: TGridLimit;
begin
  LocalModel := FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if LocalModel.DisvUsed then
  begin
    Disv := LocalModel.DisvGrid;
  end
  else
  begin
    Disv := nil;
  end;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    if (ColorLimits.ColoringChoice = clcLogTime)
      and (MinPositiveTime <= 0) then
    begin
      ColorLimits.ColoringChoice := clcTime;
      Beep;
      MessageDlg(StrPathlinesAreColore, mtWarning, [mbOK], 0);
    end;
    case ColorLimits.ColoringChoice of
      clcNone:
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      clcTime:
        begin
          MinValue := MinTime;
          MaxValue := MaxTime;
        end;
      clcLogTime:
        begin
          MinValue := Log10(MinPositiveTime);
          MaxValue := Log10(MaxTime);
        end;
      clcXPrime:
        begin
          MinValue := Grid.ColumnPosition[0];
          MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
        end;
      clcYPrime:
        begin
          MaxValue := Grid.RowPosition[0];
          MinValue := Grid.RowPosition[Grid.RowCount];
        end;
      clcZ:
        begin
        if Disv <> nil then
        begin
          MeshLmits := Disv.MeshLimits(vdFront, Disv.CrossSection.Angle);
          MinValue := MeshLmits.MinZ;
          MaxValue := MeshLmits.MaxZ;
        end
        else
        begin
          MinValue := Grid.LowestElevation;
          MaxValue := Grid.HighestElevation;
        end;
        end;
      clcGroup:
        begin
          MinValue := MinParticleGroup;
          MaxValue := MaxParticleGroup;
        end
    else
      Assert(False);
    end;
  end;
end;

class function TPathLineReader.GetPathlineGLIndex: GLuint;
begin
  if not FListInitialized and frmGoPhast.frame3DView.glWidModelView.Started then
  begin
    FListInitialized := True;
    FPathlineGLIndex := glGenLists(1);
  end;
  result := FPathlineGLIndex;
end;

function TPathLineReader.GetPointColor(MaxValue, MinValue: double;
  Point: TPathLinePoint): TColor;
var
  AValue: Double;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    clcNone:
      begin
        result := clBlack;
        Exit;
      end;
    clcTime:
      begin
        AValue := Point.AbsoluteTime;
      end;
    clcLogTime:
      begin
        if Point.AbsoluteTime > 0 then
        begin
          AValue := Log10(Point.AbsoluteTime);
        end
        else
        begin
          result := clBlack;
          Exit;
        end;
      end;
    clcXPrime:
      begin
        AValue := Point.XPrime;
      end;
    clcYPrime:
      begin
        AValue := Point.YPrime;
      end;
    clcZ:
      begin
        AValue := Point.Z;
      end;
    clcGroup:
      begin
        if Point is TPathLinePointV6 then
        begin
          AValue := TPathLinePointV6(Point).ParticleGroup;
        end
        else
        begin
          result := clBlack;
          Exit;
        end;
      end
    else Assert(False);
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor(1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

procedure TPathLineReader.Invalidate;
begin
  FRecordedPathLines := False;
end;

procedure TPathLineReader.ReadFile;
begin
  Assert(FileExists(FileName));
  ModpathVersion := GetPathlineVersion(FileName);
  try
    case ModpathVersion of
      pv5:
        begin
          ReadFileV5;
        end;
      pv6_0:
        begin
          ReadFileV6;
        end;
      pv7_2:
        begin
          ReadFileV7;
        end;
      pvUnknown:
        begin
          Beep;
          MessageDlg(StrThisDoesNotAppear, mtError, [mbOK], 0);
        end
      else
        Assert(False, StrProgrammingErrorN);
    end;
  except
    on E: EInvalidLayer do
    begin
      FLinesV5.Clear;
      FLinesV6.Clear;
      FLinesV7.Clear;
      Beep;
      MessageDlg(Format(StrThisTimeSeriesFil, ['pathline']), mtError, [mbOK], 0);
    end;
    on E: EConvertError do
    begin
      FLinesV5.Clear;
      FLinesV6.Clear;
      FLinesV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, ['pathline', E.Message]), mtError, [mbOK], 0);
    end;
    on E: EAssertionFailed do
    begin
      FLinesV5.Clear;
      FLinesV6.Clear;
      FLinesV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, ['pathline', E.Message]), mtError, [mbOK], 0);
      raise;
    end;
    on E: EInOutError do
    begin
      FLinesV5.Clear;
      FLinesV6.Clear;
      FLinesV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorReading, ['pathline', E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TPathLineReader.ReadFileV5;
var
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  ALine: string;
  CompactFormat: Boolean;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  Z: single;
  Time: single;
  J: integer;
  TS: integer;
  NRow: integer;
  NCol: integer;
  K: integer;
  I: integer;
  PathLine: TCustomPathLine;
  APoint: TPathLinePoint;
  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
//  LineIndex: Integer;
//  Line: TCustomPathLine;
//  FirstPoint: TPathLinePoint;
//  LastPoint: TPathLinePoint;
//  FirstTimeFound: Boolean;
  FTextFile: TextFile;
  LineIndex: Integer;
//  TimeIndex: Integer;
//  APathlinePoint: TPathLinePoint;
//  FirstPositiveTimeFound: Boolean;
//  LocalLines: TCustomPathLines;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FLinesV5.Count < ParticleIndex do
    begin
      FLinesV5.Add;
    end;

    PathLine := FLinesV5[ParticleIndex-1];

    APoint := PathLine.FPoints.Add as TPathLinePoint;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTime := Time;
    APoint.FLayer := K;
    APoint.FRow := I;
    APoint.FColumn := J;
    APoint.FTimeStep := TS;
    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FLinesV5.Clear;
  FLinesV6.Clear;
  FLinesV7.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      LineIndex := 1;
      While Not Eof(FTextFile) do
      begin
        Inc(LineIndex);
        try
          if CompactFormat then
          begin
            Readln(FTextFile, ParticleIndex, XPrime, YPrime, LocalZ, Z, Time, J, TS);
            ConvertIndicies(NCol, NRow, I, K, J);
          end
          else
          begin
            Readln(FTextFile, ParticleIndex, XPrime, YPrime, LocalZ, Z, Time, J, I, K, TS);
          end;
        except on E: EInOutError do
          begin
            Beep;
            MessageDlg(Format(StrErrorReadingPathli, [LineIndex, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;
//        Time := Abs(Time);

        CreateParticle;
      end;
    finally
      try
        CloseFile(FTextFile);
      except on E: EInOutError do
        begin
          Beep;
          MessageDlg(Format(StrThereWasAnErrorW, [E.Message]), mtError, [mbOK], 0);
//          Exit;
        end;
      end;
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(ParticleIndex, SizeOf(ParticleIndex));
        AFile.Read(XPrime, SizeOf(XPrime));
        AFile.Read(YPrime, SizeOf(YPrime));
        AFile.Read(LocalZ, SizeOf(LocalZ));
        AFile.Read(Z, SizeOf(Z));
        AFile.Read(Time, SizeOf(Time));
        AFile.Read(J, SizeOf(J));
        AFile.Read(TS, SizeOf(TS));

        ConvertIndicies(NCol, NRow, I, K, J);

        CreateParticle;
      end;

    finally
      FFile.Free;
    end;
  end;

//  LocalLines := Lines;
  UpdateMinMaxTime(Lines);

end;

procedure TPathLineReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TPathLineReader.SetLinesV5(const Value: TPathLines);
begin
  FLinesV5.Assign(Value);
end;

procedure TPathLineReader.SetMaxParticleGroup(const Value: integer);
begin
  FMaxParticleGroup := Value;
end;

procedure TPathLineReader.SetMaxTime(const Value: double);
begin
  FMaxTime := Value;
end;

procedure TPathLineReader.SetMinParticleGroup(const Value: integer);
begin
  FMinParticleGroup := Value;
end;

procedure TPathLineReader.SetMinPositiveTime(const Value: double);
begin
  FMinPositiveTime := Value;
end;

procedure TPathLineReader.SetMinTime(const Value: double);
begin
  FMinTime := Value;
end;

{ TPathLinePoint }

procedure TPathLinePoint.Assign(Source: TPersistent);
var
  SourcePoint: TPathLinePoint;
begin
  if Source is TPathLinePoint then
  begin
    SourcePoint := TPathLinePoint(Source);
    X := SourcePoint.X;
    Y := SourcePoint.Y;
    Z := SourcePoint.Z;
    LocalZ := SourcePoint.LocalZ;
    Time := SourcePoint.Time;
    Layer := SourcePoint.Layer;
    Row := SourcePoint.Row;
    Column := SourcePoint.Column;
    TimeStep := SourcePoint.TimeStep;
    XPrime := SourcePoint.XPrime;
    YPrime := SourcePoint.YPrime;
  end
  else
  begin
    inherited;
  end;
end;

function TPathLinePoint.CheckLimits(Limits: TPathLineDisplayLimits): boolean;
var
  LineNumber: Integer;
begin
  result := True;
  if Limits.ColumnLimits.UseLimit then
  begin
    result := (Limits.ColumnLimits.StartLimit <= Column)
      and (Column <= Limits.ColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.RowLimits.UseLimit then
  begin
    result := (Limits.RowLimits.StartLimit <= Row)
      and (Row <= Limits.RowLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.LayerLimits.UseLimit then
  begin
    result := (Limits.LayerLimits.StartLimit <= Layer)
      and (Layer <= Limits.LayerLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.TimeLimits.UseLimit then
  begin
    result := (Limits.TimeLimits.StartLimit <= AbsoluteTime)
      and (AbsoluteTime <= Limits.TimeLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.LineNumberLimits.UseLimit then
  begin
    LineNumber := ParentLine.Index+1;
    result := (Limits.LineNumberLimits.StartLimit <= LineNumber)
      and (LineNumber <= Limits.LineNumberLimits.EndLimit);
    if not result then Exit;
  end;
end;


function TPathLinePoint.GetAbsoluteTime: double;
begin
  result := Abs(Time);
end;

function TPathLinePoint.ParentLine: TCustomPathLine;
begin
  result := (Collection as TCustomPathLinePoints).FPathLine;
end;

procedure TPathLinePoint.SetHasV6Data(const Value: Boolean);
begin
  if FHasV6Data <> Value then
  begin
    FHasV6Data := Value;
    if FHasV6Data then
    begin
      ModpathVersion := pv6_0;
    end;
  end;
end;

procedure TPathLinePoint.SetModpathVersion(const Value: TPathlineVersion);
begin
  FModpathVersion := Value;
end;

function TPathLinePoint.ShouldShow(Limits: TPathLineDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
  const Segment: TSegment2D; const DisvUsed: Boolean): boolean;
var
  ColRowOrLayerToCheck: integer;
  Dummy: Double;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Orientation of
      dsoTop:
        begin
          ColRowOrLayerToCheck := Layer;
        end;
      dsoFront:
        begin
          ColRowOrLayerToCheck := Row;
        end;
      dsoSide:
        begin
          ColRowOrLayerToCheck := Column;
        end;
      else Assert(False);
    end;
    Assert(ColRowOrLayerToCheck >= 1);
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      if Abs(ColRowOrLayerToCheck - CurrentColRowOrLayer) = 1 then
      begin
        case Orientation of
          dsoTop:
            begin
              if CurrentColRowOrLayer < ColRowOrLayerToCheck then
              begin
                if LocalZ = 1 then
                begin
                  Result := True;
                end;
              end
              else
              begin
                if LocalZ = 0 then
                begin
                  Result := True;
                end;
              end;
            end;
          dsoFront:
            begin
              if not (ModpathVersion in [pv6_0, pv7_2]) then
              begin
                Exit;
              end;
              if CurrentColRowOrLayer < ColRowOrLayerToCheck then
              begin
                if (Self as TPathLinePointV6).LocalY = 1 then
                begin
                  Result := True;
                end;
              end
              else
              begin
                if (Self as TPathLinePointV6).LocalY = 0 then
                begin
                  Result := True;
                end;
              end;
            end;
          dsoSide:
            begin
              if not (ModpathVersion in [pv6_0, pv7_2]) then
              begin
                Exit;
              end;
              if CurrentColRowOrLayer < ColRowOrLayerToCheck then
              begin
                if (Self as TPathLinePointV6).LocalX = 1 then
                begin
                  Result := True;
                end;
              end
              else
              begin
                if (Self as TPathLinePointV6).LocalX = 0 then
                begin
                  Result := True;
                end;
              end;
            end;
        else Assert(False);
        end;
      end;
    end;
  end;
  case Limits.ShowChoice of
    scAll, scStart, scEnd:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        result := result and CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;
  if result and DisvUsed and (Orientation = dsoFront) then
  begin
    result := DisvFrontProjectedXPrime(Segment, EquatePoint(X, y), Dummy);
  end;
end;

function TPathLinePoint.ShouldShowLine(Limits: TPathLineDisplayLimits): boolean;
begin
  result := True;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        // do nothing
      end;
    scStart:
      begin
        Assert(Index = 0);
        result := CheckLimits(Limits);
      end;
    scEnd:
      begin
        Assert(Index = Collection.Count-1);
        result := CheckLimits(Limits);
      end;
    else Assert(False);
  end;

end;


{ TPathLinePoints }

constructor TPathLinePoints.Create(PathLine: TPathLine);
begin
  inherited Create(TPathLinePoint, PathLine);
//  FPathLine := PathLine;
end;

function TCustomPathLinePoints.GetLength: double;
var
  PointIndex: Integer;
  P1: TPathLinePoint;
  P2: TPathLinePoint;
begin
  if FLength = 0 then
  begin
    for PointIndex := 1 to Count - 1 do
    begin
      P1 := Points[PointIndex-1];
      P2 := Points[PointIndex];
      FLength := FLength + Distance(P1.X, P1.Y, P1.Z, P2.X, P2.Y, P2.Z);
    end;
  end;
  result := FLength;
end;

function TCustomPathLinePoints.GetPoint(Index: integer): TPathLinePoint;
begin
  result := Items[Index] as TPathLinePoint;
end;

function TCustomPathLinePoints.TestGetMaxTime(var Maxtime: double): boolean;
begin
  result := Count > 0;
  if result then
  begin
    Maxtime := Points[Count -1].AbsoluteTime;
  end;
end;

{ TPathLines }

constructor TPathLines.Create;
begin
  inherited Create(TPathLine);
end;

procedure TCustomPathLines.ExportShapefile(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  LineIndex: Integer;
  ALine: TCustomPathLine;
  PointIndex: Integer;
  APoint: TPathLinePoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      DefineShapeFileFields(Fields);
      try
        InitializeDataBase(FileName, ShapeDataBase, Fields);
      except
        on E: EFOpenError do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPolyLineZ, True);
    try
      for LineIndex := 0 to Count - 1 do
      begin
        ALine := Lines[LineIndex];
        if ALine.Points.Count > 0 then
        begin
          ShapeDataBase.AppendBlank;
          UpdateShapeFileFields(ALine, ShapeDataBase);

          ShapeDataBase.PostChanges;

          Shape := TShapeObject.Create;
          try
            Shape.FShapeType := stPolyLineZ;
            Shape.FNumPoints := ALine.Points.Count;
            SetLength(Shape.FPoints, ALine.Points.Count);
            SetLength(Shape.FZArray, ALine.Points.Count);
            SetLength(Shape.FMArray, ALine.Points.Count);
            Shape.FNumParts := 1;
            SetLength(Shape.FParts, 1);
            Shape.FParts[0] := 0;

            for PointIndex := 0 to ALine.Points.Count - 1 do
            begin
              APoint := ALine.Points[PointIndex];
              Shape.FMArray[PointIndex] := APoint.FTime;
              Shape.FPoints[PointIndex].X := APoint.FX;
              Shape.FPoints[PointIndex].Y := APoint.FY;
              Shape.FZArray[PointIndex] := APoint.FZ;
            end;
          except
            Shape.Free;
            raise;
          end;
          ShapeFileWriter.AddShape(Shape);
        end;
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

procedure TCustomPathLines.UpdateShapeFileFields(ALine: TCustomPathLine;
  ShapeDataBase: TXBase);
var
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
begin
  FirstPoint := ALine.Points[0];
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrSTARTLAY), FirstPoint.FLayer);
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrSTARTROW), FirstPoint.FRow);
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrSTARTCOL), FirstPoint.FColumn);
  ShapeDataBase.UpdFieldNum(XbaseFieldName(StrSTARTTIME), FirstPoint.FTime);
  LastPoint := ALine.Points[ALine.Points.Count - 1];
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrENDLAY), LastPoint.FLayer);
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrENDROW), LastPoint.FRow);
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrENDCOL), LastPoint.FColumn);
  ShapeDataBase.UpdFieldNum(XbaseFieldName(StrENDTIME), LastPoint.FTime);

  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrPARTICLE), ALine.ParticleIndex);

end;

function TCustomPathLines.XbaseFieldName(AName: Ansistring): Ansistring;
begin
  result := AnsiString(Copy(AName, 1, 10));
end;

function TCustomPathLines.Add: TCustomPathLine;
begin
  result := inherited Add as TCustomPathLine;
  result.ParticleIndex := Count;
end;

procedure TCustomPathLines.DefineShapeFileFields(Fields: TStringList);
begin
  Fields.Add(string(XbaseFieldName(StrSTARTLAY)) + '=N');
  Fields.Add(string(XbaseFieldName(StrSTARTROW)) + '=N');
  Fields.Add(string(XbaseFieldName(StrSTARTCOL)) + '=N');
  Fields.Add(string(XbaseFieldName(StrSTARTTIME)) + '=N18,10');
  Fields.Add(string(XbaseFieldName(StrENDLAY)) + '=N');
  Fields.Add(string(XbaseFieldName(StrENDROW)) + '=N');
  Fields.Add(string(XbaseFieldName(StrENDCOL)) + '=N');
  Fields.Add(string(XbaseFieldName(StrENDTIME)) + '=N18,10');
  Fields.Add(string(XbaseFieldName(StrPARTICLE)) + '=N');
end;

function TCustomPathLines.GetLine(Index: integer): TCustomPathLine;
begin
  result := Items[Index] as TCustomPathLine;
end;

function TCustomPathLines.TestGetMaxTime(var Maxtime: double): boolean;
var
  LineIndex: Integer;
  AValue: double;
begin
  result := False;
  for LineIndex := 0 to Count - 1 do
  begin
    if Lines[LineIndex].Points.TestGetMaxTime(AValue) then
    begin
      if result then
      begin
        if AValue > Maxtime then
        begin
          Maxtime := AValue;
        end
      end
      else
      begin
        Maxtime := AValue;
        result := True
      end;
    end;
  end;
end;

{ TPathLineDisplayLimits }

procedure TPathLineDisplayLimits.Assign(Source: TPersistent);
var
  SourceLimits: TPathLineDisplayLimits;
begin
  if Source is TPathLineDisplayLimits then
  begin
    SourceLimits:= TPathLineDisplayLimits(Source);
    ShowChoice := SourceLimits.ShowChoice;
    LimitToCurrentIn2D := SourceLimits.LimitToCurrentIn2D;
    ColumnLimits := SourceLimits.ColumnLimits;
    RowLimits := SourceLimits.RowLimits;
    LayerLimits := SourceLimits.LayerLimits;
    TimeLimits := SourceLimits.TimeLimits;
    ParticleGroupLimits := SourceLimits.ParticleGroupLimits;
    LineNumberLimits := SourceLimits.LineNumberLimits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPathLineDisplayLimits.Create;
begin
  inherited;
  FLayerLimits := TShowIntegerLimit.Create;
  FRowLimits := TShowIntegerLimit.Create;
  FColumnLimits := TShowIntegerLimit.Create;
  FTimeLimits := TShowFloatLimit.Create;
  FParticleGroupLimits := TShowIntegerLimit.Create;
  FLineNumberLimits := TShowIntegerLimit.Create;
  FLimitToCurrentIn2D := True;
end;

destructor TPathLineDisplayLimits.Destroy;
begin
  FLineNumberLimits.Free;
  FParticleGroupLimits.Free;
  FTimeLimits.Free;
  FColumnLimits.Free;
  FRowLimits.Free;
  FLayerLimits.Free;
  inherited;
end;

procedure TPathLineDisplayLimits.SetColumnLimits(const Value: TShowIntegerLimit);
begin
  FColumnLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetLayerLimits(const Value: TShowIntegerLimit);
begin
  FLayerLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TPathLineDisplayLimits.SetLineNumberLimits(
  const Value: TShowIntegerLimit);
begin
  FLineNumberLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetParticleGroupLimits(
  const Value: TShowIntegerLimit);
begin
  FParticleGroupLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetRowLimits(const Value: TShowIntegerLimit);
begin
  FRowLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetShowChoice(const Value: TShowChoice);
begin
  FShowChoice := Value;
end;

procedure TPathLineDisplayLimits.SetTimeLimits(const Value: TShowFloatLimit);
begin
  FTimeLimits.Assign(Value);
end;

{ TShowLimit }

procedure TShowIntegerLimit.Assign(Source: TPersistent);
var
  SourceLimit: TShowIntegerLimit;
begin
  if Source is TShowIntegerLimit then
  begin
    SourceLimit:= TShowIntegerLimit(Source);
    UseLimit := SourceLimit.UseLimit;
    StartLimit := SourceLimit.StartLimit;
    EndLimit := SourceLimit.EndLimit;
  end
  else
  begin
    inherited;
  end;
end;

procedure TShowIntegerLimit.SetEndLimit(const Value: integer);
begin
  FEndLimit := Value;
end;

procedure TShowIntegerLimit.SetStartLimit(const Value: integer);
begin
  FStartLimit := Value;
end;

procedure TShowIntegerLimit.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TShowFloatLimit }

procedure TShowFloatLimit.Assign(Source: TPersistent);
var
  SourceLimit: TShowFloatLimit;
begin
  if Source is TShowFloatLimit then
  begin
    SourceLimit:= TShowFloatLimit(Source);
    UseLimit := SourceLimit.UseLimit;
    StartLimit := SourceLimit.StartLimit;
    EndLimit := SourceLimit.EndLimit;
  end
  else
  begin
    inherited;
  end;
end;

procedure TShowFloatLimit.SetEndLimit(const Value: double);
begin
  FEndLimit := Value;
end;

procedure TShowFloatLimit.SetStartLimit(const Value: double);
begin
  FStartLimit := Value;
end;

procedure TShowFloatLimit.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TPathlineColorLimits }

procedure TPathlineColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TPathlineColorLimits;
begin
  if Source is TPathlineColorLimits then
  begin
    SourceLimits := TPathlineColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPathlineColorLimits.Create;
begin
  inherited;
  FColoringChoice := clcTime;
end;

procedure TPathlineColorLimits.SetColoringChoice(
  const Value: TColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TPathlineColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TPathlineColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TPathlineColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TEndPoints }

constructor TEndPoints.Create;
begin
  inherited Create(TEndPoint)
end;

procedure TCustomEndPoints.ExportShapefileAtEndingLocations(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  PointIndex: Integer;
  APoint: TEndPoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
//    Assert(Version in [mpv5, mpv6]);
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrSTARTLAY) + '=N');
      Fields.Add(string(StrSTARTROW) + '=N');
      Fields.Add(string(StrSTARTCOL) + '=N');

      Fields.Add(string(StrENDLAY) + '=N');
      Fields.Add(string(StrENDROW) + '=N');
      Fields.Add(string(StrENDCOL) + '=N');

      Fields.Add(string(StrSTARTTS) + '=N');
      Fields.Add(string(StrSTARTZONE) + '=N');
      Fields.Add(string(StrENDTS) + '=N');
      Fields.Add(string(StrENDZONE) + '=N');

      Fields.Add(string(StrSTARTX) + '=N18,10');
      Fields.Add(string(StrSTARTY) + '=N18,10');
      Fields.Add(string(StrSTARTZ) + '=N18,10');

      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      Fields.Add(string(StrTERMCODE) + '=N');
      Fields.Add(string(StrRELEASET) + '=N18,10');

      Fields.Add(string(StrPARTICLE) + '=N');

      try
        InitializeDataBase(FileName, ShapeDataBase, Fields);
      except
        on E: EFOpenError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPointZ, True);
    try
      for PointIndex := 0 to Count - 1 do
      begin
        APoint := Points[PointIndex] as TEndPoint;

        ShapeDataBase.AppendBlank;

        ShapeDataBase.UpdFieldInt(StrSTARTLAY, APoint.FStartLayer);
        ShapeDataBase.UpdFieldInt(StrSTARTROW, APoint.FStartRow);
        ShapeDataBase.UpdFieldInt(StrSTARTCOL, APoint.FStartColumn);

        ShapeDataBase.UpdFieldInt(StrENDLAY, APoint.FEndLayer);
        ShapeDataBase.UpdFieldInt(StrENDROW, APoint.FEndRow);
        ShapeDataBase.UpdFieldInt(StrENDCOL, APoint.FEndColumn);

        ShapeDataBase.UpdFieldInt(StrSTARTTS, APoint.FStartTimeStep);
        ShapeDataBase.UpdFieldInt(StrSTARTZONE, APoint.FStartZoneCode);
        ShapeDataBase.UpdFieldInt(StrENDTS, APoint.FEndTimeStep);
        ShapeDataBase.UpdFieldInt(StrENDZONE, APoint.FEndZoneCode);

        ShapeDataBase.UpdFieldNum(StrSTARTX, APoint.FStartX);
        ShapeDataBase.UpdFieldNum(StrSTARTY, APoint.FStartY);
        ShapeDataBase.UpdFieldNum(StrSTARTZ, APoint.FStartZ);

        ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
        ShapeDataBase.UpdFieldInt(StrTERMCODE, APoint.FTerminationCode);
        ShapeDataBase.UpdFieldNum(StrRELEASET, APoint.FReleaseTime);

        ShapeDataBase.UpdFieldNum(StrPARTICLE, APoint.ParticleNumber);

        ShapeDataBase.PostChanges;

        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stPointZ;

          Shape.FNumPoints := 1;
          SetLength(Shape.FPoints, 1);
          SetLength(Shape.FZArray, 1);
          SetLength(Shape.FMArray, 1);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 0);
          Shape.FMArray[0] := -1e40;
          Shape.FPoints[0].X := APoint.FEndX;
          Shape.FPoints[0].Y := APoint.FEndY;
          Shape.FZArray[0] := APoint.FEndZ;
        except
          Shape.Free;
          raise;
        end;
        ShapeFileWriter.AddShape(Shape);
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;

  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

procedure TCustomEndPoints.ExportShapefileAtStartingLocations(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  PointIndex: Integer;
  APoint: TEndPoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
//    Assert(Version in [mpv5, mpv6]);
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrSTARTLAY) + '=N');
      Fields.Add(string(StrSTARTROW) + '=N');
      Fields.Add(string(StrSTARTCOL) + '=N');

      Fields.Add(string(StrENDLAY) + '=N');
      Fields.Add(string(StrENDROW) + '=N');
      Fields.Add(string(StrENDCOL) + '=N');

      Fields.Add(string(StrSTARTTS) + '=N');
      Fields.Add(string(StrSTARTZONE) + '=N');
      Fields.Add(string(StrENDTS) + '=N');
      Fields.Add(string(StrENDZONE) + '=N');

      Fields.Add(string(StrENDX) + '=N18,10');
      Fields.Add(string(StrENDY) + '=N18,10');
      Fields.Add(string(StrENDZ) + '=N18,10');

      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      Fields.Add(string(StrTERMCODE) + '=N');
      Fields.Add(string(StrRELEASET) + '=N18,10');

      Fields.Add(string(StrPARTICLE) + '=N');

      try
        InitializeDataBase(FileName, ShapeDataBase, Fields);
      except
        on E: EFOpenError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPointZ, True);
    try
      for PointIndex := 0 to Count - 1 do
      begin
        APoint := Points[PointIndex] as TEndPoint;

        ShapeDataBase.AppendBlank;

        ShapeDataBase.UpdFieldInt(StrSTARTLAY, APoint.FStartLayer);
        ShapeDataBase.UpdFieldInt(StrSTARTROW, APoint.FStartRow);
        ShapeDataBase.UpdFieldInt(StrSTARTCOL, APoint.FStartColumn);

        ShapeDataBase.UpdFieldInt(StrENDLAY, APoint.FEndLayer);
        ShapeDataBase.UpdFieldInt(StrENDROW, APoint.FEndRow);
        ShapeDataBase.UpdFieldInt(StrENDCOL, APoint.FEndColumn);

        ShapeDataBase.UpdFieldInt(StrSTARTTS, APoint.FStartTimeStep);
        ShapeDataBase.UpdFieldInt(StrSTARTZONE, APoint.FStartZoneCode);
        ShapeDataBase.UpdFieldInt(StrENDTS, APoint.FEndTimeStep);
        ShapeDataBase.UpdFieldInt(StrENDZONE, APoint.FEndZoneCode);

        ShapeDataBase.UpdFieldNum(StrENDX, APoint.FEndX);
        ShapeDataBase.UpdFieldNum(StrENDY, APoint.FEndY);
        ShapeDataBase.UpdFieldNum(StrENDZ, APoint.FEndZ);

        ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
        ShapeDataBase.UpdFieldInt(StrTERMCODE, APoint.FTerminationCode);
        ShapeDataBase.UpdFieldNum(StrRELEASET, APoint.FReleaseTime);

        ShapeDataBase.UpdFieldNum(StrPARTICLE, APoint.ParticleNumber);

        ShapeDataBase.PostChanges;

        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stPointZ;

          Shape.FNumPoints := 1;
          SetLength(Shape.FPoints, 1);
          SetLength(Shape.FZArray, 1);
          SetLength(Shape.FMArray, 1);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 0);
          Shape.FMArray[0] := -1e40;
          Shape.FPoints[0].X := APoint.FStartX;
          Shape.FPoints[0].Y := APoint.FStartY;
          Shape.FZArray[0] := APoint.FStartZ;
        except
          Shape.Free;
          raise;
        end;
        ShapeFileWriter.AddShape(Shape);
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;

end;

function TCustomEndPoints.GetPoint(Index: integer): TEndPoint;
begin
  result := inherited Items[Index] as TEndPoint;
end;

function TEndPoints.GetPoint(Index: integer): TEndPoint;
begin
  result := inherited Items[Index] as TEndPoint;
end;

//function TEndPoints.GetVersion: TModpathVersion;
//begin
//  result := mpv5;
//end;

{ TEndPoint }

procedure TEndPoint.Assign(Source: TPersistent);
var
  SourcePoint: TEndPoint;
begin
  if Source is TEndPoint then
  begin
    SourcePoint := TEndPoint(Source);
    EndXPrime := SourcePoint.EndXPrime;
    EndYPrime := SourcePoint.EndYPrime;
    StartXPrime := SourcePoint.StartXPrime;
    StartYPrime := SourcePoint.StartYPrime;
    StartTimeStep := SourcePoint.StartTimeStep;
    EndTimeStep := SourcePoint.EndTimeStep;
    EndZoneCode := SourcePoint.EndZoneCode;
    EndColumn := SourcePoint.EndColumn;
    EndRow := SourcePoint.EndRow;
    EndLayer := SourcePoint.EndLayer;
    EndX := SourcePoint.EndX;
    EndY := SourcePoint.EndY;
    EndZ := SourcePoint.EndZ;
    TrackingTime := SourcePoint.TrackingTime;
    StartZoneCode := SourcePoint.StartZoneCode;
    StartColumn := SourcePoint.StartColumn;
    StartRow := SourcePoint.StartRow;
    StartLayer := SourcePoint.StartLayer;
    StartX := SourcePoint.StartX;
    StartY := SourcePoint.StartY;
    StartZ := SourcePoint.StartZ;
    StartLocalZ := SourcePoint.StartLocalZ;
    TerminationCode := SourcePoint.TerminationCode;
    ReleaseTime := SourcePoint.ReleaseTime;
    ParticleNumber := SourcePoint.ParticleNumber;
    EndLocalZ := SourcePoint.EndZoneCode;
  end
  else
  begin
    inherited;
  end;
end;

function TEndPoint.ShouldShow(Limits: TEndPointDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
  Segment: TSegment2D; DisvUsed: Boolean; WhereToPlot: TWhereToPlot): boolean;
var
  ColRowOrLayerToCheck: integer;
  Dummy: Double;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Limits.WhereToPlot of
      wtpStart:
        begin
          case Orientation of
            dsoTop:
              begin
                ColRowOrLayerToCheck := StartLayer;
              end;
            dsoFront:
              begin
                ColRowOrLayerToCheck := StartRow;
              end;
            dsoSide:
              begin
                ColRowOrLayerToCheck := StartColumn;
              end;
            else Assert(False);
          end;
        end;
      wtpEnd:
        begin
          case Orientation of
            dsoTop:
              begin
                ColRowOrLayerToCheck := EndLayer;
              end;
            dsoFront:
              begin
                ColRowOrLayerToCheck := EndRow;
              end;
            dsoSide:
              begin
                ColRowOrLayerToCheck := EndColumn;
              end;
            else Assert(False);
          end;
        end;
    end;
    Assert(ColRowOrLayerToCheck >= 1);
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      Exit;
    end;
  end;
  case Limits.ShowChoice of
    escAll:
      begin
        // do nothing
      end;
    escSpecified:
      begin
        result := CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;
  if result and DisvUsed and (Orientation = dsoFront) then
  begin
    case WhereToPlot of
      wtpStart:
        begin
          result := DisvFrontProjectedXPrime(Segment, EquatePoint(StartX, StartY), Dummy);
        end;
      wtpEnd:
        begin
          result := DisvFrontProjectedXPrime(Segment, EquatePoint(EndX, EndY), Dummy);
        end;
    end;
  end;
end;

{ TEndPointReader }

procedure TEndPointReader.Assign(Source: TPersistent);
var
  EndPointSource: TEndPointReader;
begin
  if Source is TEndPointReader then
  begin
    EndPointSource := TEndPointReader(Source);
    FileDate := EndPointSource.FileDate;
    FileName := EndPointSource.FileName;
    MinReleaseTime := EndPointSource.MinReleaseTime;
    MaxReleaseTime := EndPointSource.MaxReleaseTime;
    MinTrackingTime := EndPointSource.MinTrackingTime;
    MinPositiveTrackingTime := EndPointSource.MinPositiveTrackingTime;
    MaxTrackingTime := EndPointSource.MaxTrackingTime;
    MinStartZone := EndPointSource.MinStartZone;
    MaxStartZone := EndPointSource.MaxStartZone;
    MinEndZone := EndPointSource.MinEndZone;
    MaxEndZone := EndPointSource.MaxEndZone;
    MaxParticleGroup := EndPointSource.MaxParticleGroup;
    MinParticleGroup := EndPointSource.MinParticleGroup;
    Points := EndPointSource.Points;
    PointsV6 := EndPointSource.PointsV6;
    PointsV7 := EndPointSource.PointsV7;
    ModpathVersion := EndPointSource.ModpathVersion;
    TopQuadTree.Clear;
    FrontQuadTree.Clear;
    SideQuadTree.Clear;
  end;
  inherited;
end;

procedure TEndPointReader.Clear;
begin
  Points.Clear;
  PointsV6.Clear;
  PointsV7.Clear;
end;

constructor TEndPointReader.Create(Model: TBaseModel);
begin
  inherited Create;
  FModel := Model;
  FTopQuadTree := TRbwQuadTree.Create(nil);
  FFrontQuadTree := TRbwQuadTree.Create(nil);
  FSideQuadTree := TRbwQuadTree.Create(nil);
  FPoints:= TEndPoints.Create;
  FPointsV6 := TEndPointsV6.Create;
  FPointsV7 := TEndPointsV7.Create;
end;

destructor TEndPointReader.Destroy;
begin
  FPointsV7.Free;
  FPointsV6.Free;
  FPoints.Free;
  FSideQuadTree.Free;
  FFrontQuadTree.Free;
  FTopQuadTree.Free;
  inherited;
end;

procedure TEndPointReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TPersistent);
const
  MaxCoord = MaxInt-1;
  MinCoord = -MaxCoord;
var
  EndPointIndex: Integer;
  EndPoint: TEndPoint;
  ColRowOrLayer: integer;
  ZoomBox: TQRbwZoomBox2;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
//  Grid: TModflowGrid;
  AColor: TColor;
  AColor32: TColor32;
  ARect: TRect;
  QuadTree: TRbwQuadTree;
  ShouldInitializeTree: Boolean;
  Limits: TGridLimit;
  LocalPoints: TCustomEndPoints;
  PixelPlus: Integer;
  PixelMinus: Integer;
  DisplayQuadTree: TRbwQuadTree;
  DisplayPointList: TList<TPoint>;
  DisplayColorList: TList<TColor32>;
  UsePoint: Boolean;
  X: double;
  Y: double;
  Data: Pointer;
  LocalModel: TCustomModel;
  DisvUsed: Boolean;
  CrossSectionSegment: TSegment2D;
  OriginOffset: Double;
  DisplayXPrime: Double;
begin
  if not Visible then
  begin
    Exit;
  end;
  LocalModel := FModel as TCustomModel;

  if (LocalModel.LayerCount <= 0) or (LocalModel.RowCount <= 0)
    or (LocalModel.ColumnCount <= 0) then
  begin
    Exit;
  end;

  if not DisplayDisvPlot(LocalModel, Orientation) then
  begin
    Exit;
  end;

  DisvUsed := LocalModel.DisvUsed;
  if DisvUsed then
  begin
    CrossSectionSegment := LocalModel.DisvGrid.CrossSection.Segment;
    OriginOffset := GetOriginOffset(CrossSectionSegment);
  end
  else
  begin
    OriginOffset := 0;
    CrossSectionSegment := EquateSegment(0,0,0,0);
  end;

  PixelPlus := EndPointSize div 2;
  PixelMinus := EndPointSize - PixelPlus;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  QuadTree := nil;
  case Orientation of
    dsoTop:
      begin
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedLayer+1;
        QuadTree := TopQuadTree;
      end;
    dsoFront:
      begin
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedRow+1;
        QuadTree := FrontQuadTree;
      end;
    dsoSide:
      begin
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedColumn+1;
        QuadTree := SideQuadTree;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);
  if ColorLimits.ColoringChoice = elcLogTrackingTime then
  begin
    MaxValue := Log10(MaxValue);
    MinValue := Log10(MinValue);
  end;

  ShouldInitializeTree := QuadTree.Count = 0;
  if not ShouldInitializeTree then
  begin
    case Orientation of
      dsoTop:
        begin
          ShouldInitializeTree := FSelectedLayer <> LocalModel.SelectedLayer;
        end;
      dsoFront:
        begin
          ShouldInitializeTree := FSelectedRow <> LocalModel.SelectedRow;
        end;
      dsoSide:
        begin
          ShouldInitializeTree := FSelectedColumn <> LocalModel.SelectedColumn;
        end;
    end;
  end;
  if ShouldInitializeTree then
  begin
    QuadTree.Clear;
    Limits := LocalModel.DiscretizationLimits(OrientationToViewDirection(Orientation));
    case Orientation of
      dsoTop:
        begin
          FSelectedLayer := LocalModel.SelectedLayer;
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxY;
          QuadTree.YMin := Limits.MinY;
        end;
      dsoFront:
        begin
          FSelectedRow := LocalModel.SelectedRow;
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end;
      dsoSide:
        begin
          FSelectedColumn := LocalModel.SelectedColumn;
          QuadTree.XMax := Limits.MaxY;
          QuadTree.XMin := Limits.MinY;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end
      else Assert(False);
    end;
  end;

  if Points.Count > 0 then
  begin
    LocalPoints := Points;
  end
  else if PointsV6.Count > 0 then
  begin
    LocalPoints := PointsV6;
  end
  else
  begin
    LocalPoints := PointsV7;
  end;

  DisplayQuadTree := TRbwQuadTree.Create(nil);
  DisplayPointList := TList<TPoint>.Create;
  DisplayColorList := TList<TColor32>.Create;
  try
    DisplayQuadTree.XMax := ZoomBox.Width;
    DisplayQuadTree.YMax := ZoomBox.Height;
    for EndPointIndex := LocalPoints.Count - 1 downto 0 do
    begin
      EndPoint := LocalPoints[EndPointIndex] as TEndPoint;
      if EndPoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer,
        CrossSectionSegment, DisvUsed, DisplayLimits.WhereToPlot) then
      begin
        case DisplayLimits.WhereToPlot of
          wtpStart:
            begin
              case Orientation of
                dsoTop:
                  begin
                    ADisplayPoint.X := ZoomBox.XCoord(EndPoint.StartX);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartY);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(EndPoint.StartX, EndPoint.StartY, EndPoint);
                    end;
                  end;
                dsoFront:
                  begin
                    if DisvUsed then
                    begin
                      DisvFrontProjectedXPrime(CrossSectionSegment,
                        EquatePoint(EndPoint.StartX, EndPoint.StartY), DisplayXPrime);
                      DisplayXPrime := DisplayXPrime - OriginOffset;
                    end
                    else
                    begin
                      DisplayXPrime := EndPoint.StartXPrime;
                    end;
                    ADisplayPoint.X := ZoomBox.XCoord(DisplayXPrime);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartZ);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(DisplayXPrime, EndPoint.StartZ, EndPoint);
                    end;
                  end;
                dsoSide:
                  begin
                    ADisplayPoint.X := ZoomBox.XCoord(EndPoint.StartZ);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartYPrime);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(EndPoint.StartZ, EndPoint.StartYPrime, EndPoint);
                    end;
                  end;
                else Assert(False);
              end;
            end;
          wtpEnd:
            begin
              case Orientation of
                dsoTop:
                  begin
                    ADisplayPoint.X := ZoomBox.XCoord(EndPoint.EndX);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndY);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(EndPoint.EndX, EndPoint.EndY, EndPoint);
                    end;
                  end;
                dsoFront:
                  begin
                    if DisvUsed then
                    begin
                      DisvFrontProjectedXPrime(CrossSectionSegment,
                        EquatePoint(EndPoint.EndX, EndPoint.EndY), DisplayXPrime);
                      DisplayXPrime := DisplayXPrime - OriginOffset;
                    end
                    else
                    begin
                      DisplayXPrime := EndPoint.EndXPrime;
                    end;
                    ADisplayPoint.X := ZoomBox.XCoord(DisplayXPrime);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndZ);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(DisplayXPrime, EndPoint.EndZ, EndPoint);
                    end;
                  end;
                dsoSide:
                  begin
                    ADisplayPoint.X := ZoomBox.XCoord(EndPoint.EndZ);
                    ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndYPrime);
                    if ShouldInitializeTree then
                    begin
                      QuadTree.AddPoint(EndPoint.EndZ, EndPoint.EndYPrime, EndPoint);
                    end;
                  end;
                else Assert(False);
              end;
            end;
        end;
        if (ADisplayPoint.X <= MaxCoord)
          and (ADisplayPoint.X >= MinCoord)
          and (ADisplayPoint.Y <= MaxCoord)
          and (ADisplayPoint.Y >= MinCoord) then
        begin
          if DisplayQuadTree.Count > 0 then
          begin
            X := ADisplayPoint.X;
            Y := ADisplayPoint.Y;
            DisplayQuadTree.FirstNearestPoint(X, Y, Data);
            UsePoint := (X <> ADisplayPoint.X)
              or (Y <> ADisplayPoint.Y);
          end
          else
          begin
            UsePoint := True;
          end;

          if UsePoint then
          begin
            AColor := GetPointColor(MaxValue, MinValue, EndPoint);
            AColor32 := Color32(AColor);
            DisplayQuadTree.AddPoint(ADisplayPoint.X, ADisplayPoint.Y, EndPoint);
            DisplayPointList.Add(ADisplayPoint);
            DisplayColorList.Add(AColor32);
          end;
        end;
      end;
    end;
    Assert(DisplayPointList.Count = DisplayColorList.Count);
    for EndPointIndex := DisplayPointList.Count - 1 downto 0 do
    begin
      ADisplayPoint := DisplayPointList[EndPointIndex];
      AColor32 := DisplayColorList[EndPointIndex];
      try
        ARect.Top := ADisplayPoint.Y -PixelMinus;
        ARect.Bottom := ADisplayPoint.Y +PixelPlus;
        ARect.Left := ADisplayPoint.X -PixelMinus;
        ARect.Right := ADisplayPoint.X +PixelPlus;
      except on EIntOverflow do
        begin
          Continue;
        end;
      end;
      DrawBigRectangle32(BitMap, AColor32, AColor32, 0, ARect);
    end;
  finally
    DisplayColorList.Free;
    DisplayPointList.Free;
    DisplayQuadTree.Free;
  end;
end;

procedure TEndPointReader.Draw3D;
var
  LocalModel: TCustomModel;
//var
//  Grid: TModflowGrid;
begin
  if FDrawingEndPoints then
  begin
    Exit;
  end;
  try
    if not HasData then
    begin
      Exit;
    end;
    FDrawingEndPoints := True;


    if (not FRecordedEndPoints) then
    begin
      Record3DEndPoints;
      // FRecordedEndPoints is private and won't be set
      // by overridden versions of RecordFront.
      FRecordedEndPoints := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    LocalModel := FModel as TCustomModel;
//    Grid := (FModel as TCustomModel).ModflowGrid;
//    if Grid = nil then
//    begin
//      Exit;
//    end;
    if (LocalModel.LayerCount <= 0) or (LocalModel.RowCount <= 0)
      or (LocalModel.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(EndPointGLIndex);
  finally
    FDrawingEndPoints := False;
  end;

end;

procedure TEndPointReader.ExportShapefileAtEndingLocations(FileName: string);
begin
  case FModpathVersion of
    pv5: Points.ExportShapefileAtEndingLocations(FileName);
    pv6_0: PointsV6.ExportShapefileAtEndingLocations(FileName);
    pv7_2: PointsV7.ExportShapefileAtEndingLocations(FileName);
    else Assert(False)
  end;
end;

procedure TEndPointReader.ExportShapefileAtStartingLocations(FileName: string);
begin
  case FModpathVersion of
    pv5: Points.ExportShapefileAtStartingLocations(FileName);
    pv6_0: PointsV6.ExportShapefileAtStartingLocations(FileName);
    pv7_2: PointsV7.ExportShapefileAtStartingLocations(FileName);
    else Assert(False)
  end;
end;

class function TEndPointReader.GetEndPointGLIndex: GLuint;
begin
  if not FListInitialized and frmGoPhast.frame3DView.glWidModelView.Started then
  begin
    FListInitialized := True;
    FEndPointGLIndex := glGenLists(1);
  end;
  result := FEndPointGLIndex;
end;

function TEndPointReader.GetHasData: Boolean;
begin
  Result := (Points.Count > 0) or (PointsV6.Count > 0) or (PointsV7.Count > 0)
end;

procedure TEndPointReader.GetMinMaxValues(var MaxValue, MinValue: Double);
var
  Grid: TModflowGrid;
  LocalModel: TCustomModel;
  DisvGrid: TModflowDisvGrid;
  MeshLimits: TGridLimit;
begin
  LocalModel :=  FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if LocalModel.DisvUsed then
  begin
    DisvGrid := LocalModel.DisvGrid;
  end
  else
  begin
    DisvGrid := nil;
  end;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    case ColorLimits.ColoringChoice of
      elcNone: 
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      elcReleaseTime:  
        begin
          MinValue := MinReleaseTime;
          MaxValue := MaxReleaseTime;
          if DisplayLimits.ReleaseTimeLimits.UseLimit then
          begin
            if DisplayLimits.ReleaseTimeLimits.StartLimit > MinValue then
            begin
              MinValue := DisplayLimits.ReleaseTimeLimits.StartLimit;
            end;
            if DisplayLimits.ReleaseTimeLimits.EndLimit < MaxValue then
            begin
              MaxValue := DisplayLimits.ReleaseTimeLimits.EndLimit;
            end;
          end;
        end;
      elcTrackingTime:
        begin
          MinValue := MinTrackingTime;
          MaxValue := MaxTrackingTime;
          if DisplayLimits.TrackingTimeLimits.UseLimit then
          begin
            if DisplayLimits.TrackingTimeLimits.StartLimit > MinValue then
            begin
              MinValue := DisplayLimits.TrackingTimeLimits.StartLimit;
            end;
            if DisplayLimits.TrackingTimeLimits.EndLimit < MaxValue then
            begin
              MaxValue := DisplayLimits.TrackingTimeLimits.EndLimit;
            end;
          end;
        end;
      elcLogTrackingTime:
        begin
          MinValue := MinPositiveTrackingTime;
          MaxValue := MaxTrackingTime;
          if DisplayLimits.TrackingTimeLimits.UseLimit then
          begin
            if (DisplayLimits.TrackingTimeLimits.StartLimit > 0)
              and (DisplayLimits.TrackingTimeLimits.StartLimit > MinPositiveTrackingTime) then
            begin
              MinValue := DisplayLimits.TrackingTimeLimits.StartLimit;
            end;
            if (DisplayLimits.TrackingTimeLimits.EndLimit > 0)
              and (DisplayLimits.TrackingTimeLimits.EndLimit < MaxTrackingTime) then
            begin
              MaxValue := DisplayLimits.TrackingTimeLimits.EndLimit;
            end;
          end;
        end;
      elcStartXPrime, elcEndXPrime:
        begin
//          if Grid <> nil then
//          begin
            MinValue := Grid.ColumnPosition[0];
            MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
//          end
//          else
//          begin
//            MinValue := 0;
//            MaxValue := 0;
//          end;
        end;
      elcStartYPrime, elcEndYPrime:
        begin
//          if Grid <> nil then
//          begin
            MaxValue := Grid.RowPosition[0];
            MinValue := Grid.RowPosition[Grid.RowCount];
//          end
//          else
//          begin
//            MinValue := 0;
//            MaxValue := 0;
//          end;
        end;
      elcStartZ, elcEndZ:
        begin
          if not LocalModel.DisvUsed then
          begin
            MinValue := Grid.LowestElevation;
            MaxValue := Grid.HighestElevation;
          end
          else
          begin
            MeshLimits := DisvGrid.MeshLimits(vdFront, 0);
            MinValue := MeshLimits.MinZ;
            MaxValue := MeshLimits.MaxZ;
          end;
        end;
      elcStartZone:
        begin
          MinValue := MinStartZone;
          MaxValue := MaxStartZone;
        end; 
      elcEndZone:
        begin
          MinValue := MinEndZone;
          MaxValue := MaxEndZone;
        end;
      elcParticleGroup:
        begin
          MinValue := MinParticleGroup;
          MaxValue := MaxParticleGroup;
        end
      else Assert(False);
    end;
  end;
end;

function TEndPointReader.GetPointColor(MaxValue, MinValue: double;
  Point: TEndPoint): TColor;
var
  AValue: Double;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    elcNone: 
      begin
        result := clBlack;
        Exit;
      end;
    elcReleaseTime:
      begin
        AValue := Point.ReleaseTime;
      end;
    elcTrackingTime:
      begin
        AValue := Point.TrackingTime;
      end;
    elcLogTrackingTime:
      begin
        if Point.TrackingTime > 0 then
        begin
          AValue := log10(Point.TrackingTime);
        end
        else
        begin
          result := clBlack;
          Exit;
        end;
      end;
    elcStartXPrime:
      begin
        AValue := TEndPoint(Point).StartXPrime
      end;
    elcStartYPrime:
      begin
        AValue := TEndPoint(Point).StartYPrime
      end;
    elcStartZ:
      begin
        AValue := Point.StartZ;
      end;
    elcStartZone:
      begin
        AValue := Point.StartZoneCode
      end;
    elcEndXPrime:
      begin
        AValue := TEndPoint(Point).EndXPrime
      end;
    elcEndYPrime:
      begin
        AValue := TEndPoint(Point).EndYPrime
      end;
    elcEndZ:
      begin
        AValue := Point.EndZ;
      end;
    elcEndZone:
      begin
        AValue := Point.EndZoneCode
      end;
    elcParticleGroup:
      begin
        if Point is TEndPointV6 then
        begin
          AValue := TEndPointV6(Point).ParticleGroup
        end
        else
        begin
          result := clBlack;
          Exit;
        end;
      end;
    else Assert(False);
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor(1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

procedure TEndPointReader.Invalidate;
begin
  FRecordedEndPoints := False;
end;

function TEndPointReader.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure GetZ(Grid: TModflowGrid; J, I, K: integer; LocalZ: single; var Z: single);
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  Z1: Real;
  Z2: Real;
  Thickness: Real;
begin
  Column := J -1;
  Row := I -1;
  Layer := (Grid.Model as TCustomModel).ModflowLayerToDataSetLayer(K);
  if (Column < Grid.ColumnCount) and (Row < Grid.RowCount)
    and (Layer < Grid.LayerCount) then
  begin
    Z1 := Grid.CellElevation[ZeroBasedID(Layer+1, Row, Column)];
  end
  else
  begin
    Z := 0;
    Exit;
  end;
  if LocalZ < 0 then
  begin
    Z2 := Grid.CellElevation[ZeroBasedID(Layer+2, Row, Column)];
    Thickness := Z1-Z2;
  end
  else
  begin
    Z2 := Grid.CellElevation[ZeroBasedID(Layer, Row, Column)];
    Thickness := Z2-Z1;
  end;
  Z := Z1 + Thickness*LocalZ;
end;

procedure TEndPointReader.ReadFile;
begin
  FModpathVersion := GetEndPointVersion(FFileName);
  try
    case FModpathVersion of
      pv5: ReadFileV5;
      pv6_0: ReadFileV6;
      pv7_2: ReadFileV7;
      else Assert(False);
    end;
  except
    on EInvalidLayer do
    begin
      Points.Clear;
      PointsV6.Clear;
      PointsV7.Clear;
      Beep;
      MessageDlg(Format(StrThisTimeSeriesFil, ['end point']), mtError, [mbOK], 0);
    end;
    on E: EConvertError do
    begin
      Points.Clear;
      PointsV6.Clear;
      PointsV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, ['end point', E.Message]), mtError, [mbOK], 0);
    end;
    on E: EInOutError do
    begin
      Points.Clear;
      PointsV6.Clear;
      PointsV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorReading, ['end point', E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TEndPointReader.SetMinMaxValues(LocalPoints: TCustomEndPoints);
var
  PointIndex: Integer;
  EndPoint: TEndPoint;
  FoundFirstPositiveTrackingTime: Boolean;
begin
  FoundFirstPositiveTrackingTime := False;
  for PointIndex := 0 to LocalPoints.Count - 1 do
  begin
    EndPoint := LocalPoints[PointIndex];
    if PointIndex = 0 then
    begin
      MinTrackingTime := EndPoint.TrackingTime;
      MaxTrackingTime := EndPoint.TrackingTime;
      MinReleaseTime := EndPoint.ReleaseTime;
      MaxReleaseTime := EndPoint.ReleaseTime;
      MinStartZone := EndPoint.StartZoneCode;
      MaxStartZone := EndPoint.StartZoneCode;
      MinEndZone := EndPoint.EndZoneCode;
      MaxEndZone := EndPoint.EndZoneCode;
      if EndPoint.TrackingTime > 0 then
      begin
        FoundFirstPositiveTrackingTime := true;
        MinPositiveTrackingTime := EndPoint.TrackingTime;
      end;
    end
    else
    begin
      if EndPoint.TrackingTime > 0 then
      begin
        if FoundFirstPositiveTrackingTime then
        begin
          if EndPoint.TrackingTime < MinPositiveTrackingTime then
          begin
            MinPositiveTrackingTime := EndPoint.TrackingTime;
          end;
        end
        else
        begin
          FoundFirstPositiveTrackingTime := true;
          MinPositiveTrackingTime := EndPoint.TrackingTime;
        end;
      end;
      if EndPoint.TrackingTime < MinTrackingTime then
      begin
        MinTrackingTime := EndPoint.TrackingTime;
      end;
      if EndPoint.TrackingTime > MaxTrackingTime then
      begin
        MaxTrackingTime := EndPoint.TrackingTime;
      end;
      if EndPoint.ReleaseTime < MinReleaseTime then
      begin
        MinReleaseTime := EndPoint.ReleaseTime;
      end;
      if EndPoint.ReleaseTime > MaxReleaseTime then
      begin
        MaxReleaseTime := EndPoint.ReleaseTime;
      end;
      if EndPoint.StartZoneCode < MinStartZone then
      begin
        MinStartZone := EndPoint.StartZoneCode;
      end;
      if EndPoint.StartZoneCode > MaxStartZone then
      begin
        MaxStartZone := EndPoint.StartZoneCode;
      end;
      if EndPoint.EndZoneCode < MinEndZone then
      begin
        MinEndZone := EndPoint.EndZoneCode;
      end;
      if EndPoint.EndZoneCode > MaxEndZone then
      begin
        MaxEndZone := EndPoint.EndZoneCode;
      end;
    end;
  end;
end;

procedure TEndPointReader.SetMinParticleGroup(const Value: integer);
begin
  FMinParticleGroup := Value;
end;

procedure TEndPointReader.SetMinPositiveTrackingTime(const Value: double);
begin
  FMinPositiveTrackingTime := Value;
end;

procedure TEndPointReader.ReadFileV5;
var
  Grid: TModflowGrid;
  ADate: TDateTime;
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  NRow: Integer;
  NCol: Integer;
  FTextFile: TextFile;
  ALine: string;
  CompactFormat: boolean;
  FFile: TFileStream;
  Description: array[0..79] of AnsiChar;
  Terminator: array[0..3] of AnsiChar;
  EndZoneCode: integer;
  EndJ: integer;
  EndI: integer;
  EndK: integer;
  StartJ: integer;
  StartI: integer;
  StartK: integer;
  StartZoneCode: integer;
  IPCODE: integer;
  EndXPrime: single;
  EndYPrime: single;
  EndZ: single;
  EndLocalZ: single;
  TrackingTime: single;
  StartXPrime: single;
  StartYPrime: single;
  StartLocalZ: single;
  ReleaseTime: single;
  StartTimeStep: integer;
//  PointIndex: Integer;
  StartZ: single;
//  EndPoint: TEndPoint;
  ParticleID: Integer;
  LineIndex: Integer;
  procedure CreateEndPoint;
  var
    APoint: TEndPoint;
    Point2D: TPoint2D;
  begin
    APoint := TEndPoint.Create(FPoints);
    APoint.EndZoneCode := EndZoneCode;
    APoint.EndColumn := EndJ;
    APoint.EndRow := EndI;
    APoint.EndLayer := EndK;

    ConvertCoordinates(Grid, EndXPrime, EndYPrime, Point2D);

    APoint.EndX := Point2D.x;
    APoint.EndY := Point2D.Y;
    APoint.EndZ := EndZ;
    APoint.EndXPrime := EndXPrime;
    APoint.EndYPrime := EndYPrime;
    APoint.EndLocalZ := EndLocalZ;
    APoint.TrackingTime := TrackingTime;
    APoint.StartZoneCode := StartZoneCode;
    APoint.StartColumn := StartJ;
    APoint.StartRow := StartI;
    APoint.StartLayer := StartK;

    ConvertCoordinates(Grid, StartXPrime, StartYPrime, Point2D);

    APoint.StartX := Point2D.x;
    APoint.StartY := Point2D.y;
    APoint.StartZ := StartZ;
    APoint.StartXPrime := StartXPrime;
    APoint.StartYPrime := StartYPrime;
    APoint.StartTimeStep := StartTimeStep;
    if IPCODE < 0 then
    begin
      APoint.TerminationCode := IPCODE;
      APoint.EndTimeStep := 0;
    end
    else
    begin
      APoint.TerminationCode := IPCODE mod 10;
      APoint.EndTimeStep := IPCODE div 10;
    end;
    APoint.ReleaseTime := ReleaseTime;
    APoint.ParticleNumber := ParticleID;
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  Points.Clear;
  PointsV6.Clear;
  PointsV7.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      ParticleID := 1;
      LineIndex := 1;
      While Not Eof(FTextFile) do
      begin
        Inc(LineIndex);
        try
          if CompactFormat then
          begin
            Readln(FTextFile, EndZoneCode, EndJ, EndXPrime, EndYPrime,
              EndLocalZ, TrackingTime, StartXPrime, StartYPrime, StartLocalZ,
              StartJ, StartZoneCode, StartTimeStep, IPCODE, ReleaseTime);
            ConvertIndicies(NCol, NRow, EndI, EndK, EndJ);
            ConvertIndicies(NCol, NRow, StartI, StartK, StartJ);
            GetZ(Grid, EndJ, EndI, EndK, EndLocalZ, EndZ);
            GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);
          end
          else
          begin
            Readln(FTextFile, EndZoneCode, EndJ, EndI, EndK, EndXPrime, EndYPrime,
              EndZ, EndLocalZ, TrackingTime, StartXPrime, StartYPrime, StartLocalZ,
              StartJ, StartI, StartK, StartZoneCode, StartTimeStep, IPCODE,
              ReleaseTime);
            GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);
          end;
        except on E: EInOutError do
          begin
            Beep;
            MessageDlg(Format(StrErrorReadingEndpoi, [LineIndex, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;

        CreateEndPoint;
        Inc(ParticleID);
      end;
    finally
      CloseFile(FTextFile);
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(EndZoneCode, SizeOf(EndZoneCode));
        AFile.Read(EndJ, SizeOf(EndJ));
        AFile.Read(EndXPrime, SizeOf(EndXPrime));
        AFile.Read(EndYPrime, SizeOf(EndYPrime));
        AFile.Read(EndLocalZ, SizeOf(EndLocalZ));
        AFile.Read(TrackingTime, SizeOf(TrackingTime));
        AFile.Read(StartXPrime, SizeOf(StartXPrime));
        AFile.Read(StartYPrime, SizeOf(StartYPrime));
        AFile.Read(StartLocalZ, SizeOf(StartLocalZ));
        AFile.Read(StartJ, SizeOf(StartJ));
        AFile.Read(StartZoneCode, SizeOf(StartZoneCode));
        AFile.Read(StartTimeStep, SizeOf(StartTimeStep));
        AFile.Read(IPCODE, SizeOf(IPCODE));
        AFile.Read(ReleaseTime, SizeOf(ReleaseTime));

        ConvertIndicies(NCol, NRow, EndI, EndK, EndJ);
        ConvertIndicies(NCol, NRow, StartI, StartK, StartJ);
        GetZ(Grid, EndJ, EndI, EndK, EndLocalZ, EndZ);
        GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);

        CreateEndPoint;
      end;
    finally
      FFile.Free;
    end;
  end;

  SetMinMaxValues(Points)
end;

{$HINTS OFF}
procedure TEndPointReader.ReadFileV6;
var
  Grid: TModflowGrid;
  ADate: TDateTime;
//  AFile: TFileStream;
//  AChar: AnsiChar;
//  IsTextFile: Boolean;
//  NRow: Integer;
//  NCol: Integer;
  FTextFile: TextFile;
  ALine: string;
//  CompactFormat: boolean;
//  FFile: TFileStream;
//  Description: array[0..79] of AnsiChar;
//  Terminator: array[0..3] of AnsiChar;
  EndZoneCode: integer;
  FinalColumn: integer;
  FinalRow: integer;
  FinalLayer: integer;
  StartColumn: integer;
  StartRow: integer;
  StartLayer: integer;
  StartZoneCode: integer;
//  IPCODE: integer;
  EndXPrime: double;
  EndYPrime: double;
  EndZ: double;
  EndLocalZ: double;
  TrackingTime: double;
  StartXPrime: double;
  StartYPrime: double;
  StartLocalZ: double;
  ReleaseTime: double;
  StartTimeStep: integer;
  StartZ: single;
  Splitter: TStringList;
  TrackingDirection: Integer;
  TotalCount: Integer;
  ReleaseCount: Integer;
  MaximumID: Integer;
  ReferenceTime: double;
  Pending: Integer;
  Active: Integer;
  NormallyTerminated: Integer;
  ZoneTerminated: Integer;
  Unreleased: Integer;
  Stranded: Integer;
  GroupCount: integer;
  GroupIndex: Integer;
  GroupName: string;
  ParticleID: Integer;
  ParticleGroup: Integer;
  Status: Integer;
  InitialGrid: Integer;
  StartCellFace: Integer;
  StartLocalX: double;
  StartLocalY: double;
  FinalGrid: Integer;
  FinalCellFace: Integer;
  EndLocalX: double;
  EndLocalY: double;
  PointLabel: string;
  FirstLine: boolean;
  procedure CreateEndPoint;
  var
    APoint: TEndPointV6;
    Point2D: TPoint2D;
  begin
    APoint := TEndPointV6.Create(FPointsV6);
    APoint.EndZoneCode := EndZoneCode;
    APoint.EndColumn := FinalColumn;
    APoint.EndRow := FinalRow;
    APoint.EndLayer := FinalLayer;

    ConvertCoordinates(Grid, EndXPrime, EndYPrime, Point2D);

    APoint.EndX := Point2D.x;
    APoint.EndY := Point2D.Y;
    APoint.EndZ := EndZ;
    APoint.EndXPrime := EndXPrime;
    APoint.EndYPrime := EndYPrime;
    APoint.EndLocalZ := EndLocalZ;
    APoint.TrackingTime := TrackingTime;
    APoint.StartZoneCode := StartZoneCode;
    APoint.StartColumn := StartColumn;
    APoint.StartRow := StartRow;
    APoint.StartLayer := StartLayer;

    ConvertCoordinates(Grid, StartXPrime, StartYPrime, Point2D);

    APoint.StartX := Point2D.x;
    APoint.StartY := Point2D.y;
    APoint.StartZ := StartZ;
    APoint.StartXPrime := StartXPrime;
    APoint.StartYPrime := StartYPrime;
    APoint.StartTimeStep := StartTimeStep;
    APoint.TerminationCode := Status;
    APoint.ReleaseTime := ReleaseTime;
    APoint.ParticleGroup := ParticleGroup;
    APoint.InitialCellFace := StartCellFace;
    APoint.FinalCellFace := FinalCellFace;
    APoint.ParticleLabel := PointLabel;
    APoint.ParticleNumber := ParticleID;
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  Points.Clear;
  PointsV6.Clear;
  PointsV7.Clear;

//  NRow := Grid.RowCount;
//  NCol := Grid.ColumnCount;

  AssignFile(FTextFile, FFileName);
  Splitter:= TStringList.Create;
  try
    Splitter.Delimiter := ' ';

    Reset(FTextFile);
    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'MODPATH_ENDPOINT_FILE 6 0');

    // line 2
    Readln(FTextFile, ALine);
    Splitter.DelimitedText := Trim(ALine);
    Assert(Splitter.Count = 5);
    TrackingDirection := StrToInt(Splitter[0]);
    TotalCount := StrToInt(Splitter[1]);
    ReleaseCount := StrToInt(Splitter[2]);
    MaximumID := StrToInt(Splitter[3]);
    ReferenceTime := FortranStrToFloat(Splitter[4]);

    // line 3
    Readln(FTextFile, ALine);
    Splitter.DelimitedText := Trim(ALine);
    Assert(Splitter.Count = 6);
    Pending := StrToInt(Splitter[0]);
    Active := StrToInt(Splitter[1]);
    NormallyTerminated := StrToInt(Splitter[2]);
    ZoneTerminated := StrToInt(Splitter[3]);
    Unreleased := StrToInt(Splitter[4]);
    Stranded := StrToInt(Splitter[5]);

    if TotalCount = 0 then
    begin
      Beep;
      MessageDlg(StrAbortingTheNumber, mtWarning, [mbOK], 0);
      Exit;
    end;

    // line 4
    Readln(FTextFile, GroupCount);
    Assert(GroupCount > 0);

    for GroupIndex := 0 to GroupCount - 1 do
    begin
      Readln(FTextFile, GroupName);
    end;

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'END HEADER');
    FirstLine := True;
    While Not Eof(FTextFile) do
    begin
      Readln(FTextFile, ALine);
      Splitter.DelimitedText := Trim(ALine);
      While Splitter.Count > 30 do
      begin
        // the Label has spaces in it.
        // The MODPATH file was not created by ModelMuse.
        Splitter[Splitter.Count-2] := Splitter[Splitter.Count-2]
          + ' ' + Splitter[Splitter.Count-1];
        Splitter.Delete(Splitter.Count-1);
      end;
      Assert(Splitter.Count = 30);

      ParticleID := StrToInt(Splitter[0]);
      ParticleGroup := StrToInt(Splitter[1]);
      Status := StrToInt(Splitter[2]);
      ReleaseTime := FortranStrtoFloat(Splitter[3]);
      TrackingTime := FortranStrtoFloat(Splitter[4]);
      InitialGrid := StrToInt(Splitter[5]);
      StartLayer := StrToInt(Splitter[6]);
      StartRow := StrToInt(Splitter[7]);
      StartColumn := StrToInt(Splitter[8]);
      StartCellFace := StrToInt(Splitter[9]);
      StartZoneCode := StrToInt(Splitter[10]);
      StartLocalX := FortranStrtoFloat(Splitter[11]);
      StartLocalY := FortranStrtoFloat(Splitter[12]);
      StartLocalZ := FortranStrtoFloat(Splitter[13]);
      StartXPrime := FortranStrtoFloat(Splitter[14]);
      StartYPrime := FortranStrtoFloat(Splitter[15]);
      StartZ := FortranStrtoFloat(Splitter[16]);
      FinalGrid := StrToInt(Splitter[17]);
      FinalLayer := StrToInt(Splitter[18]);
      FinalRow := StrToInt(Splitter[19]);
      FinalColumn := StrToInt(Splitter[20]);
      FinalCellFace := StrToInt(Splitter[21]);
      EndZoneCode := StrToInt(Splitter[22]);
      EndLocalX := FortranStrtoFloat(Splitter[23]);
      EndLocalY := FortranStrtoFloat(Splitter[24]);
      EndLocalZ := FortranStrtoFloat(Splitter[25]);
      EndXPrime := FortranStrtoFloat(Splitter[26]);
      EndYPrime := FortranStrtoFloat(Splitter[27]);
      EndZ := FortranStrtoFloat(Splitter[28]);
      PointLabel := Splitter[29];

      CreateEndPoint;

      if FirstLine then
      begin
        FirstLine := False;
        MinParticleGroup := ParticleGroup;
        MaxParticleGroup := ParticleGroup;
      end
      else
      begin
        if ParticleGroup < MinParticleGroup then
        begin
          MinParticleGroup := ParticleGroup;
        end;
        if ParticleGroup > MaxParticleGroup then
        begin
          MaxParticleGroup := ParticleGroup;
        end;
      end;
    end;
  finally
    Splitter.Free;
    CloseFile(FTextFile);
  end;

  SetMinMaxValues(PointsV6)
end;
{$HINTS ON}


{$HINTS OFF}
procedure TEndPointReader.ReadFileV7;
var
  Grid: TModflowGrid;
  ADate: TDateTime;
//  AFile: TFileStream;
//  AChar: AnsiChar;
//  IsTextFile: Boolean;
  NRow: Integer;
  NCol: Integer;
//  NLay: Integer;
  FTextFile: TextFile;
  ALine: string;
//  CompactFormat: boolean;
//  FFile: TFileStream;
//  Description: array[0..79] of AnsiChar;
//  Terminator: array[0..3] of AnsiChar;
  EndZoneCode: integer;
  FinalColumn: integer;
  FinalRow: integer;
  FinalLayer: integer;
  StartColumn: integer;
  StartRow: integer;
  StartLayer: integer;
  StartZoneCode: integer;
//  IPCODE: integer;
//  EndXPrime: double;
//  EndYPrime: double;
//  EndZ: double;
  EndLocalZ: double;
  EndGlobalX: double;
  EndGlobalY: double;
  EndGlobalZ: double;
  TrackingTime: double;
//  StartXPrime: double;
//  StartYPrime: double;
  StartLocalZ: double;
  ReleaseTime: double;
//  StartTimeStep: integer;
  StartGlobalX: Double;
  StartGlobalY: Double;
  StartGlobalZ: Double;
  Splitter: TStringList;
  TrackingDirection: Integer;
  TotalCount: Integer;
  ReleaseCount: Integer;
  MaximumID: Integer;
  ReferenceTime: double;
  XOrigin: double;
  YOrigin: double;
  Origin: TPoint2D;
  AngRot: double;
  Pending: Integer;
  Active: Integer;
  BoundaryFaceTerminated: Integer;
  WeakSinkTerminated: Integer;
  WeakSourceTerminated: Integer;
  StrongSinkTerminated: Integer;
  ZoneTerminated: Integer;
  InactiveTerminated: Integer;
  Unreleased: Integer;
  UnknownTerminated: Integer;
//  Stranded: Integer;
  GroupCount: integer;
  GroupIndex: Integer;
  GroupName: string;
  SequenceNumber: Integer;
  ParticleID: Integer;
  ParticleGroup: Integer;
  Status: Integer;
  InitialCellNumber: Integer;
  StartCellFace: Integer;
  StartLocalX: double;
  StartLocalY: double;
//  FinalGrid: Integer;
  FinalCellFace: Integer;
  EndLocalX: double;
  EndLocalY: double;
//  PointLabel: string;
  FirstLine: boolean;
  LocalModel: TCustomModel;
//  DisvGrid: TModflowDisvGrid;
  FinalCellNumber: Integer;
  GroupNames: TStringList;
  APoint2D: TPoint2D;
  ODistance: double;
  XOffSet: double;
  YOffSet: double;
  OAngle : double;
  ErrorLines: TStringList;
  function RotateToRealWorldCoordinates(
    const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
    DistanceP: double;
    AngleP: double;
  begin
    result := APoint;
    if AngRot <> 0 then
    begin
      DistanceP := Distance(result, Origin);
      AngleP := ArcTan2(result.Y - Origin.Y, result.x - Origin.x) / Pi *180;
      temp := ProjectPoint(Origin, AngleP + AngRot,DistanceP);
      result := temp;
    end;
  end;

  procedure CellNumberToRowCol(CellNumber: Integer; out ARow, ACol: Integer);
//  var
//    Layer: Integer;
  begin
    if Grid = nil then
    begin
      ARow := 0;
      ACol := 0;
    end
    else
    begin
//      Layer := (CellNumber -1) div (NRow * NCol) + 1;
      CellNumber := (CellNumber -1) mod (NRow * NCol);
      ARow := CellNumber div NCol + 1;
      ACol := CellNumber mod ARow + 1;
    end;
  end;
  procedure PositionToRowCol(XPrime, YPrime: double; out ARow, ACol: Integer);
  var
    APoint: TPoint2D;
    ACell: T2DTopCell;
  begin
    APoint.x := XPrime;
    APoint.y := YPrime;
    ACell := Grid.TopContainingCell(APoint, eaBlocks, False);
    ARow := ACell.Row+1;
    ACol := ACell.Col+1;
  end;

  procedure CreateEndPoint;
  var
    APoint: TEndPointV7;
//    Point2D: TPoint2D;
  begin
    APoint := TEndPointV7.Create(FPointsV7);

    APoint.EndXPrime := EndGlobalX + XOffSet;
    APoint.EndYPrime := EndGlobalY + YOffSet;
    APoint.EndZ := EndGlobalZ;
    APoint.EndZoneCode := EndZoneCode;
    APoint2D.X := EndGlobalX + XOrigin;
    APoint2D.Y := EndGlobalY + YOrigin;
    APoint2D := RotateToRealWorldCoordinates(APoint2D);
    APoint.EndX := APoint2D.x;
    APoint.EndY := APoint2D.Y;
    APoint.EndLocalZ := EndLocalZ;
    APoint.TrackingTime := TrackingTime;
    APoint.StartZoneCode := StartZoneCode;
    APoint.StartXPrime := StartGlobalX + XOffSet;
    APoint.StartYPrime := StartGlobalY + YOffSet;
    APoint.StartZ := StartGlobalZ;
    APoint2D.X := StartGlobalX + XOrigin;
    APoint2D.Y := StartGlobalY + YOrigin;
    APoint2D := RotateToRealWorldCoordinates(APoint2D);
    APoint.StartX := APoint2D.X;
    APoint.StartY := APoint2D.Y;
    APoint.ReleaseTime := ReleaseTime;
    APoint.ParticleGroup := ParticleGroup;
    APoint.InitialCellFace := StartCellFace;
    APoint.FinalCellFace := FinalCellFace;
    APoint.ParticleNumber := ParticleID;
    APoint.SequenceNumber := SequenceNumber;
    APoint.InitialCellNumber := InitialCellNumber;
    APoint.InitialLocalX := StartLocalX;
    APoint.InitialLocalY := StartLocalY;
    APoint.StartLocalZ := StartLocalZ;
    APoint.FinalCellNumber := FinalCellNumber;
    APoint.FinalLocalX := EndLocalX;
    APoint.FinalLocalY := EndLocalY;
    APoint.TerminationCode := Status;
    APoint.ParticleLabel := GroupNames[ParticleGroup-1];

    if LocalModel.DisvUsed then
    begin
      PositionToRowCol(APoint.StartXPrime, APoint.StartYPrime, StartRow, StartColumn);
      PositionToRowCol(APoint.EndXPrime, APoint.EndYPrime, FinalRow, FinalColumn);
    end
    else
    begin
      CellNumberToRowCol(InitialCellNumber, StartRow, StartColumn);
      CellNumberToRowCol(FinalCellNumber, FinalRow, FinalColumn);
    end;

    APoint.StartColumn := StartColumn;
    APoint.StartRow := StartRow;
    APoint.StartLayer := StartLayer;
    APoint.EndColumn := FinalColumn;
    APoint.EndRow := FinalRow;
    APoint.EndLayer := FinalLayer;

  end;
begin
  LocalModel := FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  Points.Clear;
  PointsV6.Clear;
  PointsV7.Clear;

  if Grid <> nil then
  begin
    NRow := Grid.RowCount;
    NCol := Grid.ColumnCount;
  end
  else
  begin

  end;

  AssignFile(FTextFile, FFileName);
  GroupNames := TStringList.Create;
  Splitter:= TStringList.Create;
  ErrorLines := TStringList.Create;
  try
    Splitter.Delimiter := ' ';

    Reset(FTextFile);
    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'MODPATH_ENDPOINT_FILE         7         2');

    // line 2
    Readln(FTextFile, ALine);
    Splitter.DelimitedText := Trim(ALine);
    Assert(Splitter.Count = 8);
    TrackingDirection := StrToInt(Splitter[0]);
    TotalCount := StrToInt(Splitter[1]);
    ReleaseCount := StrToInt(Splitter[2]);
    MaximumID := StrToInt(Splitter[3]);
    ReferenceTime := FortranStrToFloat(Splitter[4]);
    XOrigin := FortranStrToFloat(Splitter[5]);
    YOrigin := FortranStrToFloat(Splitter[6]);
    AngRot := FortranStrToFloat(Splitter[7]);

    if LocalModel.ModelSelection <> msModflow2015 then
    begin
      // Even if a grid metadata file is present, the data from it is not
      // recorded in the pathlinle file.
      APoint2D := Grid.TwoDElementCorner(0, Grid.RowCount);
      XOrigin := APoint2D.x;
      YOrigin := APoint2D.y;
      AngRot := Grid.GridAngle * 180 / Pi;
    end;
    Origin.x := XOrigin;
    Origin.y := YOrigin;


    if TotalCount = 0 then
    begin
      Beep;
      MessageDlg(StrAbortingTheNumber, mtWarning, [mbOK], 0);
      Exit;
    end;

    // line 3
    Readln(FTextFile, ALine);
    Splitter.DelimitedText := Trim(ALine);
    Assert(Splitter.Count = 10);
    Pending := StrToInt(Splitter[0]);
    Active := StrToInt(Splitter[1]);
    BoundaryFaceTerminated := StrToInt(Splitter[2]);
    WeakSinkTerminated := StrToInt(Splitter[3]);
    WeakSourceTerminated := StrToInt(Splitter[4]);
    StrongSinkTerminated := StrToInt(Splitter[5]);
    ZoneTerminated := StrToInt(Splitter[6]);
    InactiveTerminated := StrToInt(Splitter[7]);

    Unreleased := StrToInt(Splitter[8]);
    UnknownTerminated := StrToInt(Splitter[9]);


    // line 4
    Readln(FTextFile, GroupCount);
    Assert(GroupCount > 0);

    // Item 5
    for GroupIndex := 0 to GroupCount - 1 do
    begin
      Readln(FTextFile, GroupName);
      GroupNames.Add(GroupName);
    end;

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'END HEADER');

    ODistance := Sqrt(Sqr(Origin.X) + Sqr(Origin.Y));
    OAngle := ArcTan2(Origin.x, Origin.y);
    XOffSet := Sin(AngRot/180*Pi + OAngle)*ODistance;
    YOffSet := Cos(AngRot/180*Pi + OAngle)*ODistance;

    FirstLine := True;
    While Not Eof(FTextFile) do
    begin
      Readln(FTextFile, ALine);
      Splitter.DelimitedText := Trim(ALine);
      Assert(Splitter.Count = 26);

      try
      SequenceNumber := StrToInt(Splitter[0]);
      ParticleGroup := StrToInt(Splitter[1]);
      ParticleID := StrToInt(Splitter[2]);
      Status := StrToInt(Splitter[3]);
      ReleaseTime := FortranStrtoFloat(Splitter[4]);
      TrackingTime := FortranStrtoFloat(Splitter[5]);
      InitialCellNumber := StrToInt(Splitter[6]);
      StartLayer := StrToInt(Splitter[7]);
      StartLocalX := FortranStrtoFloat(Splitter[8]);
      StartLocalY := FortranStrtoFloat(Splitter[9]);
      StartLocalZ := FortranStrtoFloat(Splitter[10]);
      StartGlobalX := FortranStrtoFloat(Splitter[11]);
      StartGlobalY := FortranStrtoFloat(Splitter[12]);
      StartGlobalZ := FortranStrtoFloat(Splitter[13]);
      StartZoneCode := StrToInt(Splitter[14]);
      StartCellFace := StrToInt(Splitter[15]);
      FinalCellNumber := StrToInt(Splitter[16]);
      FinalLayer := StrToInt(Splitter[17]);
      EndLocalX := FortranStrtoFloat(Splitter[18]);
      EndLocalY := FortranStrtoFloat(Splitter[19]);
      EndLocalZ := FortranStrtoFloat(Splitter[20]);
      EndGlobalX := FortranStrtoFloat(Splitter[21]);
      EndGlobalY := FortranStrtoFloat(Splitter[22]);
      EndGlobalZ := FortranStrtoFloat(Splitter[23]);
      EndZoneCode := StrToInt(Splitter[24]);
      FinalCellFace := StrToInt(Splitter[25]);

      CreateEndPoint;

      if FirstLine then
      begin
        FirstLine := False;
        MinParticleGroup := ParticleGroup;
        MaxParticleGroup := ParticleGroup;
      end
      else
      begin
        if ParticleGroup < MinParticleGroup then
        begin
          MinParticleGroup := ParticleGroup;
        end;
        if ParticleGroup > MaxParticleGroup then
        begin
          MaxParticleGroup := ParticleGroup;
        end;
      end;
      except on EConvertError do
        begin
          if ErrorLines.Count < 10 then
          begin
            ErrorLines.Add(ALine);
          end;
        end;
      end;
    end;
    if ErrorLines.Count > 0 then
    begin
      Beep;
      MessageDlg(StrTheFollowingLines + sLineBreak + ErrorLines.Text, mtError, [mbOK], 0);
    end;
  finally
    ErrorLines.Free;
    Splitter.Free;
    GroupNames.Free;
    CloseFile(FTextFile);
  end;

  SetMinMaxValues(PointsV7)
end;
{$HINTS ON}

procedure TEndPointReader.Record3DEndPoints;
var
  Grid: TModflowGrid;
  ColRowOrLayer: Integer;
  MaxValue: Double;
  MinValue: Double;
  PointIndex: Integer;
  EndPoint: TEndPoint;
  AColor: TColor;
  LocalPoints: TCustomEndPoints;
  LocalModel: TCustomModel;
begin
  if not Visible then
  begin
    Exit;
  end;
  LocalModel := FModel as TCustomModel;
  if LocalModel.DisvUsed then
  begin
    if LocalModel.DisvGrid = nil then
    begin
      Exit;
    end
    else if LocalModel.DisvGrid.TwoDGrid.ElementCount = 0 then
    begin
      Exit;
    end;
  end
  else
  begin
    Grid := (FModel as TCustomModel).ModflowGrid;
    if Grid = nil then
    begin
      Exit;
    end;
    if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
      or (Grid.ColumnCount <= 0) then
    begin
      Exit;
    end;
  end;
  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(EndPointGLIndex, GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      if ColorLimits.ColoringChoice = elcLogTrackingTime then
      begin
        MaxValue := Log10(MaxValue);
        MinValue := Log10(MinValue);
      end;
      glLineWidth(1);

      glBegin(GL_POINTS);
      if Points.Count > 0 then
      begin
        LocalPoints := Points;
      end
      else if PointsV6.Count > 0 then
      begin
        LocalPoints := PointsV6;
      end
      else
      begin
        LocalPoints := PointsV7;
      end;
//      Assert(LocalPoints.Version in [mpv5, mpv6]);
      for PointIndex := 0 to LocalPoints.Count - 1 do
      begin
        EndPoint := LocalPoints[PointIndex];
        // A dummy segment can be used because the segment isn't used
        // if the orientation is dso3D.
        if EndPoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer,
          EquateSegment(0,0,0,0), False, DisplayLimits.WhereToPlot) then
        begin
          AColor := GetPointColor(MaxValue, MinValue, EndPoint);
          AssignColor(AColor);
          case DisplayLimits.WhereToPlot of
            wtpStart:
              begin
                glVertex3f(EndPoint.StartXPrime, EndPoint.StartYPrime, EndPoint.StartZ);
              end;
            wtpEnd:
              begin
                glVertex3f(EndPoint.EndXPrime, EndPoint.EndYPrime, EndPoint.EndZ);
              end;
            else Assert(False);
          end;
        end;
      end;
      glEnd;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

procedure TEndPointReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TEndPointReader.SetMaxEndZone(const Value: integer);
begin
  FMaxEndZone := Value;
end;

procedure TEndPointReader.SetMaxParticleGroup(const Value: integer);
begin
  FMaxParticleGroup := Value;
end;

procedure TEndPointReader.SetMaxReleaseTime(const Value: double);
begin
  FMaxReleaseTime := Value;
end;

procedure TEndPointReader.SetMaxStartZone(const Value: integer);
begin
  FMaxStartZone := Value;
end;

procedure TEndPointReader.SetMaxTrackingTime(const Value: double);
begin
  FMaxTrackingTime := Value;
end;

procedure TEndPointReader.SetMinEndZone(const Value: integer);
begin
  FMinEndZone := Value;
end;

procedure TEndPointReader.SetMinReleaseTime(const Value: double);
begin
  FMinReleaseTime := Value;
end;

procedure TEndPointReader.SetMinStartZone(const Value: integer);
begin
  FMinStartZone := Value;
end;

procedure TEndPointReader.SetTrackingMinTime(const Value: double);
begin
  FMinTrackingTime := Value;
end;

procedure TEndPointReader.UpdateMinMax;
begin
  if Points.Count > 0 then
  begin
    SetMinMaxValues(Points);
  end;
  if PointsV6.Count > 0 then
  begin
    SetMinMaxValues(PointsV6);
  end;
  if PointsV7.Count > 0 then
  begin
    SetMinMaxValues(PointsV7);
  end;
end;

function TEndPointReader._AddRef: Integer;
begin
  Result := -1;
end;

function TEndPointReader._Release: Integer;
begin
  Result := -1;
end;

procedure TEndPointReader.SetPoints(const Value: TEndPoints);
begin
  FPoints.Assign(Value);
end;

procedure TEndPointReader.SetPointsV6(const Value: TEndPointsV6);
begin
  FPointsV6.Assign(Value);
end;

procedure TEndPointReader.SetPointsV7(const Value: TEndPointsV7);
begin
  FPointsV7.Assign(Value)
end;

{ TEndPointDisplayLimits }

procedure TEndPointDisplayLimits.Assign(Source: TPersistent);
var
  SourceLimits: TEndPointDisplayLimits;
begin
  if Source is TEndPointDisplayLimits then
  begin
    SourceLimits := TEndPointDisplayLimits(Source);
    ShowChoice := SourceLimits.ShowChoice;
    LimitToCurrentIn2D := SourceLimits.LimitToCurrentIn2D;
    StartColumnLimits := SourceLimits.StartColumnLimits;
    StartRowLimits := SourceLimits.StartRowLimits;
    StartLayerLimits := SourceLimits.StartLayerLimits;
    StartZoneLimits := SourceLimits.StartZoneLimits;
    ReleaseTimeLimits := SourceLimits.ReleaseTimeLimits;
    EndColumnLimits := SourceLimits.EndColumnLimits;
    EndRowLimits := SourceLimits.EndRowLimits;
    EndLayerLimits := SourceLimits.EndLayerLimits;
    EndZoneLimits := SourceLimits.EndZoneLimits;
    TrackingTimeLimits := SourceLimits.TrackingTimeLimits;
    ParticleGroupLimits := SourceLimits.ParticleGroupLimits;
    WhereToPlot := SourceLimits.WhereToPlot;
  end
  else
  begin
    inherited;
  end;
end;

constructor TEndPointDisplayLimits.Create;
begin
  inherited;
  FStartLayerLimits := TShowIntegerLimit.Create;
  FStartRowLimits := TShowIntegerLimit.Create;
  FStartColumnLimits := TShowIntegerLimit.Create;
  FStartZoneLimits := TShowIntegerLimit.Create;
  FEndLayerLimits := TShowIntegerLimit.Create;
  FEndRowLimits := TShowIntegerLimit.Create;
  FEndColumnLimits := TShowIntegerLimit.Create;
  FEndZoneLimits := TShowIntegerLimit.Create;
  FReleaseTimeLimits := TShowFloatLimit.Create;
  FTrackingTimeLimits := TShowFloatLimit.Create;
  FParticleGroupLimits := TShowIntegerLimit.Create;
  FWhereToPlot := wtpEnd;
end;

destructor TEndPointDisplayLimits.Destroy;
begin
  FParticleGroupLimits.Free;
  FTrackingTimeLimits.Free;
  FReleaseTimeLimits.Free;
  FEndZoneLimits.Free;
  FEndColumnLimits.Free;
  FEndRowLimits.Free;
  FEndLayerLimits.Free;
  FStartZoneLimits.Free;
  FStartColumnLimits.Free;
  FStartRowLimits.Free;
  FStartLayerLimits.Free;
  inherited;
end;

procedure TEndPointDisplayLimits.SetEndColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FEndColumnLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FEndLayerLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndRowLimits(
  const Value: TShowIntegerLimit);
begin
  FEndRowLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndZoneLimits(
  const Value: TShowIntegerLimit);
begin
  FEndZoneLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TEndPointDisplayLimits.SetParticleGroupLimits(
  const Value: TShowIntegerLimit);
begin
  FParticleGroupLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetReleaseTimeLimits(
  const Value: TShowFloatLimit);
begin
  FReleaseTimeLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetShowChoice(
  const Value: TEndpointShowChoice);
begin
  FShowChoice := Value;
end;

procedure TEndPointDisplayLimits.SetStartColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FStartColumnLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FStartLayerLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartRowLimits(
  const Value: TShowIntegerLimit);
begin
  FStartRowLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartZoneLimits(
  const Value: TShowIntegerLimit);
begin
  FStartZoneLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetTrackingTimeLimits(
  const Value: TShowFloatLimit);
begin
  FTrackingTimeLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetWhereToPlot(const Value: TWhereToPlot);
begin
  FWhereToPlot := Value;
end;

{ TEndPointColorLimits }

procedure TEndPointColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TEndPointColorLimits;
begin
  if Source is TEndPointColorLimits then
  begin
    SourceLimits := TEndPointColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TEndPointColorLimits.Create;
begin
  inherited;
  FColoringChoice := elcTrackingTime;
end;

procedure TEndPointColorLimits.SetColoringChoice(
  const Value: TEndpointColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TEndPointColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TEndPointColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TEndPointColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TTimeSeriesReader }

procedure TTimeSeriesReader.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeriesReader;
begin
  if Source is TTimeSeriesReader then
  begin
    SourceSeries := TTimeSeriesReader(Source);
    FileName := SourceSeries.FileName;
    FileDate := SourceSeries.FileDate;
    Series := SourceSeries.Series;
    SeriesV6 := SourceSeries.SeriesV6;
    SeriesV7 := SourceSeries.SeriesV7;
    MaxTime := SourceSeries.MaxTime;
    MinTime := SourceSeries.MinTime;
    TimeIndex := SourceSeries.TimeIndex;
    Times := SourceSeries.Times;
    MinParticleGroup := SourceSeries.MinParticleGroup;
    MaxParticleGroup := SourceSeries.MaxParticleGroup;
  end;
  inherited;
end;

function TTimeSeriesReader.CheckShowSeries(Series: TCustomTimeSeries): Boolean;
var
  APoint: TTimeSeriesPoint;
begin
  result := True;
  case DisplayLimits.ShowChoice of
    scAll, scSpecified:
      begin
      end;
    scStart:
      begin
        APoint := Series.Points[0];
        result := APoint.ShouldShowSeries(DisplayLimits);
      end;
    scEnd:
      begin
        APoint := Series.Points[Series.Points.Count - 1];
        result := APoint.ShouldShowSeries(DisplayLimits);
      end;
  else
    Assert(False);
  end;
end;

constructor TTimeSeriesReader.Create(Model: TBaseModel);
begin
  inherited Create;
  FModel := Model;
  FTimes := nil;
  FSeries:= TTimeSeriesCollection.Create;
  FSeriesV6:= TTimeSeriesCollectionV6.Create;
  FSeriesV7:= TTimeSeriesCollectionV7.Create;
end;

destructor TTimeSeriesReader.Destroy;
begin
  FSeriesV7.Free;
  FSeriesV6.Free;
  FSeries.Free;
  FTimes.Free;
  inherited;
end;

procedure TTimeSeriesReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TPersistent);
const
  MaxCoord = MaxInt -2;
  MinCoord = -MaxCoord;
var
  TimeSeriesIndex: Integer;
  TimeSeries: TCustomTimeSeries;
  APoint: TTimeSeriesPoint;
  ColRowOrLayer: integer;
  ZoomBox: TQRbwZoomBox2;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
  Grid: TModflowGrid;
  AColor: TColor;
  AColor32: TColor32;
  ARect: TRect;
  TimeToPlot: Double;
  PlotIndex: Integer;
  LocalSeries: TCustomTimeSeriesCollection;
  DisplayQuadTree: TRbwQuadTree;
  DisplayPointList: TList<TPoint>;
  DisplayColorList: TList<TColor32>;
  X: Double;
  Y: Double;
  UsePoint: Boolean;
  PixelPlus: Integer;
  PixelMinus: Integer;
  Data: Pointer;
  LocalModel: TCustomModel;
  DisvUsed: Boolean;
  CrossSectionSegment: TSegment2D;
  OriginOffset: Double;
  DisplayXPrime: Double;
begin
  if not Visible then
  begin
    Exit;
  end;
  if TimeIndex < 0 then
  begin
    Exit;
  end;

  LocalSeries := ActiveSeries;

  if LocalSeries.Count = 0 then
  begin
    Exit;
  end;

  if not DisplayDisvPlot(FModel as TCustomModel, Orientation) then
  begin
    Exit;
  end;

  LocalModel := FModel as TCustomModel;
  DisvUsed := LocalModel.DisvUsed;
  if DisvUsed then
  begin
    CrossSectionSegment := LocalModel.DisvGrid.CrossSection.Segment;
    OriginOffset := GetOriginOffset(CrossSectionSegment);
  end
  else
  begin
    OriginOffset := 0;
    CrossSectionSegment := EquateSegment(0,0,0,0);
  end;


  Grid := LocalModel.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  case Orientation of
    dsoTop:
      begin
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := Grid.SelectedLayer+1;
      end;
    dsoFront:
      begin
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := Grid.SelectedRow+1;
      end;
    dsoSide:
      begin
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := Grid.SelectedColumn+1;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);

  TimeToPlot := Times[TimeIndex];
  DisplayQuadTree := TRbwQuadTree.Create(nil);
  DisplayPointList := TList<TPoint>.Create;
  DisplayColorList := TList<TColor32>.Create;
  try
    DisplayQuadTree.XMax := ZoomBox.Width;
    DisplayQuadTree.YMax := ZoomBox.Height;
    for TimeSeriesIndex := LocalSeries.Count - 1 downto 0 do
    begin
      TimeSeries := LocalSeries[TimeSeriesIndex];
      PlotIndex := TimeSeries.Times.IndexOf(TimeToPlot);
      if PlotIndex >= 0 then
      begin
        if CheckShowSeries(TimeSeries) then
        begin
          APoint := TimeSeries.Points[PlotIndex];
          if APoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer,
            CrossSectionSegment, DisvUsed) then
          begin
            case Orientation of
              dsoTop:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
                end;
              dsoFront:
                begin
                  if DisvUsed then
                  begin
                    DisvFrontProjectedXPrime(CrossSectionSegment,
                      EquatePoint(APoint.X, APoint.Y), DisplayXPrime);
                    DisplayXPrime := DisplayXPrime - OriginOffset;
                  end
                  else
                  begin
                    DisplayXPrime := APoint.XPrime;
                  end;
                  ADisplayPoint.X := ZoomBox.XCoord(DisplayXPrime);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Z);
                end;
              dsoSide:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.Z);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.YPrime);
                end;
              else Assert(False);
            end;
            if (ADisplayPoint.X <= MaxCoord)
              and (ADisplayPoint.X >= MinCoord)
              and (ADisplayPoint.Y <= MaxCoord)
              and (ADisplayPoint.Y >= MinCoord) then
            begin
              if DisplayQuadTree.Count > 0 then
              begin
                X := ADisplayPoint.X;
                Y := ADisplayPoint.Y;
                DisplayQuadTree.FirstNearestPoint(X, Y, Data);
                UsePoint := (X <> ADisplayPoint.X)
                  or (Y <> ADisplayPoint.Y);
              end
              else
              begin
                UsePoint := True;
              end;
              if UsePoint then
              begin
                AColor := GetPointColor(MaxValue, MinValue, APoint);
                AColor32 := Color32(AColor);
                DisplayQuadTree.AddPoint(ADisplayPoint.X, ADisplayPoint.Y, APoint);
                DisplayPointList.Add(ADisplayPoint);
                DisplayColorList.Add(AColor32);
              end;
            end;
          end;
        end;
      end;
    end;

    PixelPlus := TimeSeriesSize div 2;
    PixelMinus := TimeSeriesSize - PixelPlus;

    Assert(DisplayPointList.Count = DisplayColorList.Count);
    for TimeSeriesIndex := DisplayPointList.Count - 1 downto 0 do
    begin
      ADisplayPoint := DisplayPointList[TimeSeriesIndex];
      AColor32 := DisplayColorList[TimeSeriesIndex];
      try
        ARect.Top := ADisplayPoint.Y -PixelMinus;
        ARect.Bottom := ADisplayPoint.Y +PixelPlus;
        ARect.Left := ADisplayPoint.X -PixelMinus;
        ARect.Right := ADisplayPoint.X +PixelPlus;
      except on EIntOverflow do
        begin
          Continue;
        end;
      end;
      DrawBigRectangle32(BitMap, AColor32, AColor32, 0, ARect);
    end;

  finally
    DisplayColorList.Free;
    DisplayPointList.Free;
    DisplayQuadTree.Free;
  end;
end;

procedure TTimeSeriesReader.Draw3D;
var
  Grid: TModflowGrid;
  LocalSeries: TCustomTimeSeriesCollection;
begin
  if TimeIndex < 0 then
  begin
    Exit;
  end;
  if FDrawingTimeSeries then
  begin
    Exit;
  end;
  LocalSeries := ActiveSeries;
  if LocalSeries.Count = 0 then
  begin
    Exit;
  end;
  try
    FDrawingTimeSeries := True;


    if (not RecordedTimeSeries[TimeIndex]) then
    begin
      Record3DTimeSeries(TimeIndex);
      // FRecordedTimeSeries is private and won't be set
      // by overridden versions of RecordFront.
      RecordedTimeSeries[TimeIndex] := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    Grid := (FModel as TCustomModel).ModflowGrid;
    if Grid = nil then
    begin
      Exit;
    end;
    if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
      or (Grid.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(TimeSeriesGLIndex[TimeIndex]);
  finally
    FDrawingTimeSeries := False;
  end;

end;

procedure TTimeSeriesReader.EnsureGLArrays(ATimeIndex: Integer);
var
  Index: Integer;
  OldLength: Integer;
  GLIndex: GLuint;
  MaxPoints: Integer;
begin
  Assert(Length(FRecordedTimeSeries) = Length(FTimeSeriesGLIndex));
  if ATimeIndex >= Length(FRecordedTimeSeries) then
  begin
    MaxPoints := Times.Count;
    Assert((MaxPoints > ATimeIndex) or (MaxPoints = 0));
    OldLength := Length(FRecordedTimeSeries);
    SetLength(FRecordedTimeSeries, MaxPoints);
    SetLength(FTimeSeriesGLIndex, MaxPoints);
    GLIndex := glGenLists(MaxPoints - OldLength);
    for Index := OldLength to MaxPoints-1 do
    begin
      FRecordedTimeSeries[Index] := False;
      FTimeSeriesGLIndex[Index] := GLIndex;
      Inc(GLIndex);
    end;
  end;
end;

procedure TTimeSeriesReader.ExportShapefile(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  ATime: Double;
  SeriesIndex: Integer;
  ASeries: TCustomTimeSeries;
  PlotIndex: Integer;
  APoint: TTimeSeriesPoint;
  Shape: TShapeObject;
  PointCount: Integer;
  PlotTimeIndex: Integer;
  LocalSeries: TCustomTimeSeriesCollection;
begin
  LocalSeries := ActiveSeries;
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      try
        InitializeDataBase(FileName, ShapeDataBase, Fields);
      except
        on E: EFOpenError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stMultiPointZ, True);
    try
      for PlotTimeIndex := 0 to Times.Count - 1 do
      begin
        ATime := Times[PlotTimeIndex];
        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stMultiPointZ;
          SetLength(Shape.FMArray, LocalSeries.Count);
          SetLength(Shape.FParts, LocalSeries.Count);
          SetLength(Shape.FPoints, LocalSeries.Count);
          SetLength(Shape.FZArray, LocalSeries.Count);

          PointCount := 0;
          APoint := nil;
          for SeriesIndex := 0 to LocalSeries.Count - 1 do
          begin
            ASeries := LocalSeries[SeriesIndex];
            PlotIndex := ASeries.Times.IndexOf(ATime);
            if PlotIndex >= 0 then
            begin
              APoint := ASeries.Points[PlotIndex];
              Shape.FMArray[PointCount] := APoint.FParticleIndex;
              Shape.FZArray[PointCount] := APoint.FZ;
              Shape.FParts[PointCount] := PointCount;
              Shape.FPoints[PointCount].x := APoint.FX;
              Shape.FPoints[PointCount].y := APoint.FY;
              Inc(PointCount);
            end;
          end;
          SetLength(Shape.FMArray, PointCount);
          SetLength(Shape.FParts, PointCount);
          SetLength(Shape.FPoints, PointCount);
          SetLength(Shape.FZArray, PointCount);
          if PointCount > 0 then
          begin
            Shape.FNumParts := PointCount;
            Shape.FNumPoints := PointCount;
            Shape.FNumPoints := PointCount;

            Assert(APoint <> nil);
            ShapeFileWriter.AddShape(Shape);

            ShapeDataBase.AppendBlank;
            ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
            ShapeDataBase.PostChanges;
          end
          else
          begin
            Shape.Free;
          end;
        except
          Shape.Free;
          raise;
        end;
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

function TTimeSeriesReader.GetActiveSeries: TCustomTimeSeriesCollection;
begin
  if Series.Count > 0 then
  begin
    result := Series;
  end
  else if SeriesV6.Count > 0 then
  begin
    result := SeriesV6;
  end
  else
  begin
    result := SeriesV7;
  end;
end;

function TTimeSeriesReader.GetHasData: Boolean;
begin
  result := (Series.Count > 0) or (SeriesV6.Count > 0) or (SeriesV7.Count >0);
end;

procedure TTimeSeriesReader.GetMinMaxValues(var MaxValue, MinValue: Double);
var
  Grid: TModflowGrid;
  LocalModel: TCustomModel;
  MeshLimits: TGridLimit;
begin
  LocalModel := FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    case ColorLimits.ColoringChoice of
      tscNone:
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      tscParticleNumber:
        begin
          MinValue := 1;
          MaxValue := ActiveSeries.Count;
        end;
      tscXPrime, tscStartXPrime, tscEndXPrime:
        begin
          MinValue := Grid.ColumnPosition[0];
          MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
        end;
      tscYPrime, tscStartYPrime, tscEndYPrime:
        begin
          MaxValue := Grid.RowPosition[0];
          MinValue := Grid.RowPosition[Grid.RowCount];
        end;
      tscZ, tscStartZ, tscEndZ:
        begin
          if LocalModel.DisvUsed then
          begin
            MeshLimits := LocalModel.DisvGrid.MeshLimits(vdFront, 0);
            MinValue := MeshLimits.MinZ;
            MaxValue := MeshLimits.MaxZ;
          end
          else
          begin
            MinValue := Grid.LowestElevation;
            MaxValue := Grid.HighestElevation;
          end;
        end;
      tscGroup:
        begin
          MinValue := MinParticleGroup;
          MaxValue := MaxParticleGroup;
        end
      else Assert(False);
    end;
  end;
end;

function TTimeSeriesReader.GetPointColor(MaxValue, MinValue: double;
  Point: TTimeSeriesPoint): TColor;
var
  AValue: Double;
  StartPoint: TTimeSeriesPoint;
  EndPoint: TTimeSeriesPoint;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    tscNone:
      begin
        result := clBlack;
        Exit;
      end;
    tscParticleNumber:
      begin
        AValue := Point.ParticleIndex;
      end;
    tscXPrime:
      begin
        AValue := Point.XPrime;
      end;
    tscStartXPrime:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.XPrime;
      end;
    tscEndXPrime:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.XPrime;
      end;
    tscYPrime:
      begin
        AValue := Point.YPrime;
      end;
    tscStartYPrime:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.YPrime;
      end;
    tscEndYPrime:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.YPrime;
      end;
    tscZ:
      begin
        AValue := Point.Z;
      end;
    tscStartZ:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.Z;
      end;
    tscEndZ:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.Z;
      end;
    tscGroup:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        if StartPoint is TTimeSeriesPointV6 then
        begin
          AValue := TTimeSeriesPointV6(StartPoint).ParticleGroup;
        end
        else
        begin
          result := clBlack;
          Exit;
        end;
      end
    else Assert(False);
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor
        (1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

function TTimeSeriesReader.GetRecordedTimeSeries(ATimeIndex: integer): boolean;
begin
  EnsureGLArrays(ATimeIndex);
  result := FRecordedTimeSeries[ATimeIndex];
end;

function TTimeSeriesReader.GetTimeIndex: integer;
begin
  FixTimeIndex;
  result := FTimeIndex;
end;

function TTimeSeriesReader.GetTimes: TRealList;
var
  MaxCount: integer;
  MaxIndex: integer;
  Index: Integer;
  ASeries: TCustomTimeSeries;
  APoint: TTimeSeriesPoint;
  SeriesIndex: Integer;
  LocalSeries: TCustomTimeSeriesCollection;
begin
  if FTimes = nil then
  begin
    FTimes := TRealList.Create;

    LocalSeries := ActiveSeries;

    if LocalSeries.Count > 0 then
    begin
      MaxCount := 0;
      MaxIndex := 0;
      for Index := 0 to LocalSeries.Count - 1 do
      begin
        ASeries := LocalSeries[Index];
        if ASeries.Points.Count > MaxCount then
        begin
          MaxCount := ASeries.Points.Count;
          MaxIndex := Index;
        end;
      end;
      ASeries := LocalSeries[MaxIndex];
      if ASeries.Points.Count > 0 then
      begin
        FTimes.Capacity := ASeries.Points.Count;
        FTimes.Sorted := True;
        for SeriesIndex := 0 to LocalSeries.Count - 1 do
        begin
          ASeries := LocalSeries[SeriesIndex];
          for Index := 0 to ASeries.Points.Count - 1 do
          begin
            APoint := ASeries.Points[Index];
            FTimes.AddUnique(APoint.TrackingTime);
          end;
        end;
      end;
    end;
  end;
  result := FTimes;
end;

function TTimeSeriesReader.GetTimeSeriesGLIndex(ATimeIndex: integer): GLuint;
begin
  EnsureGLArrays(ATimeIndex);
  result := FTimeSeriesGLIndex[ATimeIndex];
end;

procedure TTimeSeriesReader.Invalidate;
var
  Index: Integer;
begin
  for Index := 0 to Length(FRecordedTimeSeries) - 1 do
  begin
    FRecordedTimeSeries[Index] := False;
  end;
end;

procedure TTimeSeriesReader.FixTimeIndex;
begin
  inherited;
  if FTimeIndex >= Times.Count then
  begin
    FTimeIndex := Times.Count-1;
  end;
end;

procedure TTimeSeriesReader.ReadFile;
var
  version: TPathlineVersion;
begin
  version := GetTimeSeriesVersion(FFileName);
  try
    case version of
      pv5: ReadFileV5;
      pv6_0: ReadFileV6;
      pv7_2: ReadFileV7;
      else Assert(False);
    end;
  except
    on EInvalidLayer do
    begin
      FSeries.Clear;
      FSeriesV6.Clear;
      FSeriesV7.Clear;
      Beep;
      MessageDlg(Format(StrThisTimeSeriesFil, ['time series']), mtError, [mbOK], 0);
    end;
    on E: EConvertError do
    begin
      FSeries.Clear;
      FSeriesV6.Clear;
      FSeriesV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, ['time series', E.Message]), mtError, [mbOK], 0);
    end;
    on E: EInOutError do
    begin
      FSeries.Clear;
      FSeriesV6.Clear;
      FSeriesV7.Clear;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorReading, ['time series', E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TTimeSeriesReader.ReadFileV5;
var
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  ALine: string;
  CompactFormat: Boolean;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  Z: single;
  TrackingTime: single;
  J: integer;
  TS: integer;
  NRow: integer;
  NCol: integer;
  K: integer;
  I: integer;
  TimeSeries: TTimeSeries;
  APoint: TTimeSeriesPoint;
  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
  LineIndex: Integer;
  Line: TTimeSeries;
  FirstPoint: TTimeSeriesPoint;
  LastPoint: TTimeSeriesPoint;
  FirstTimeFound: Boolean;
  FTextFile: TextFile;
  FFile: TFileStream;
  TimeStepIndex: integer;
  MaxPoints: Integer;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FSeries.Count < ParticleIndex do
    begin
      FSeries.Add;
    end;

    TimeSeries := FSeries[ParticleIndex-1];

    APoint := TimeSeries.FPoints.Add as TTimeSeriesPoint;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.ParticleIndex := ParticleIndex;
    APoint.FTimeStepIndex := TimeStepIndex;
    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTrackingTime := TrackingTime;
    APoint.FLayer := K;
    APoint.FRow := I;
    APoint.FColumn := J;
    APoint.FTimeStep := TS;
    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  FreeAndNil(FTimes);
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FSeries.Clear;
  FSeriesV6.Clear;
  FSeriesV7.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      LineIndex := 1;
      While Not Eof(FTextFile) do
      begin
        Inc(LineIndex);
        try
          if CompactFormat then
          begin
            Readln(FTextFile, TimeStepIndex, ParticleIndex, J, XPrime, YPrime,
              LocalZ, TrackingTime, TS);
            ConvertIndicies(NCol, NRow, I, K, J);
            GetZ(Grid, J, I, K, LocalZ, Z);
          end
          else
          begin
            Readln(FTextFile, TimeStepIndex, ParticleIndex, J, I, K, XPrime,
              YPrime, Z, LocalZ, TrackingTime, TS);
          end;
        except on E: EInOutError do
          begin
            Beep;
            MessageDlg(Format(StrErrorReadingTimese, [LineIndex, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;

        CreateParticle;
      end;
    finally
      CloseFile(FTextFile);
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(TimeStepIndex, SizeOf(TimeStepIndex));
        AFile.Read(ParticleIndex, SizeOf(ParticleIndex));
        AFile.Read(J, SizeOf(J));
        AFile.Read(XPrime, SizeOf(XPrime));
        AFile.Read(YPrime, SizeOf(YPrime));
        AFile.Read(LocalZ, SizeOf(LocalZ));
        AFile.Read(Z, SizeOf(Z));
        AFile.Read(TrackingTime, SizeOf(TrackingTime));
        AFile.Read(TS, SizeOf(TS));

        ConvertIndicies(NCol, NRow, I, K, J);
        GetZ(Grid, J, I, K, LocalZ, Z);

        CreateParticle;
      end;

    finally
      FFile.Free;
    end;
  end;

  MaxPoints := 0;
  FirstTimeFound := False;
  if Series.Count > 0 then
  begin
    for LineIndex := 0 to Series.Count - 1 do
    begin
      Line := Series[LineIndex];
      if Line.Points.Count > MaxPoints then
      begin
        MaxPoints := Line.Points.Count;
      end;
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count-1];
        if not FirstTimeFound then
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            MinTime := FirstPoint.TrackingTime;
            MaxTime := LastPoint.TrackingTime;
          end
          else
          begin
            MinTime := LastPoint.TrackingTime;
            MaxTime := FirstPoint.TrackingTime;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            if FirstPoint.TrackingTime < MinTime then
            begin
              MinTime := FirstPoint.TrackingTime
            end;
            if LastPoint.TrackingTime > MaxTime then
            begin
              MaxTime := LastPoint.TrackingTime;
            end;
          end
          else
          begin
            if LastPoint.TrackingTime < MinTime then
            begin
              MinTime := LastPoint.TrackingTime
            end;
            if FirstPoint.TrackingTime > MaxTime then
            begin
              MaxTime := FirstPoint.TrackingTime;
            end;
          end;
        end;
      end;
    end;
  end;
  if TimeIndex >= MaxPoints  then
  begin
    TimeIndex := MaxPoints-1;
  end;
end;

{$HINTS OFF}
procedure TTimeSeriesReader.ReadFileV6;
var
//  AFile: TFileStream;
//  AChar: AnsiChar;
//  IsTextFile: Boolean;
  ALine: string;
//  CompactFormat: Boolean;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  LocalX: single;
  LocalY: single;
  Z: single;
  TrackingTime: single;
  Column: integer;
  TS: integer;
//  NRow: integer;
//  NCol: integer;
  Layer: integer;
  Row: integer;
  TimeSeries: TTimeSeriesV6;
  APoint: TTimeSeriesPointV6;
//  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
//  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
  LineIndex: Integer;
  Line: TTimeSeries;
  FirstPoint: TTimeSeriesPoint;
  LastPoint: TTimeSeriesPoint;
  FirstTimeFound: Boolean;
  FTextFile: TextFile;
//  FFile: TFileStream;
  TimeStepIndex: integer;
  MaxPoints: Integer;
  Splitter: TStringList;
  TrackingDirection: Integer;
  ReferenceTime: double;
  ParticleGroup: integer;
  GridNumber: Integer;
  FirstRow: boolean;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FSeriesV6.Count < ParticleIndex do
    begin
      FSeriesV6.Add;
    end;

    TimeSeries := FSeriesV6[ParticleIndex-1];

    APoint := TimeSeries.FPoints.Add as TTimeSeriesPointV6;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.ParticleIndex := ParticleIndex;
    APoint.FTimeStepIndex := TimeStepIndex;
    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTrackingTime := TrackingTime;
    APoint.FLayer := Layer;
    APoint.FRow := Row;
    APoint.FColumn := Column;
    APoint.FTimeStep := TS;
    APoint.ParticleGroup := ParticleGroup;
    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  FreeAndNil(FTimes);
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FSeries.Clear;
  FSeriesV6.Clear;
  FSeriesV7.Clear;

//  NRow := Grid.RowCount;
//  NCol := Grid.ColumnCount;

  Splitter:= TStringList.Create;
  AssignFile(FTextFile, FFileName);
  try
    Splitter.Delimiter := ' ';
    Reset(FTextFile);

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'MODPATH_TIMESERIES_FILE 6 0');

    Readln(FTextFile, ALine);
    Splitter.DelimitedText := ALine;
    Assert(Splitter.Count = 2);
    TrackingDirection := StrToInt(Splitter[0]);
    ReferenceTime := FortranStrToFloat(Splitter[1]);

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'END HEADER');

    FirstRow := true;
    While Not Eof(FTextFile) do
    begin
      Readln(FTextFile, ALine);

      Splitter.DelimitedText := ALine;
//      while Splitter.Count > 15 do
//      begin
//        // the particle group name has spaces in it.
//        // The MODPATH file was not created by ModelMuse.
//        Splitter[4] := Splitter[4] + ' ' + Splitter[5];
//        Splitter.Delete(5);
//      end;
      Assert(Splitter.Count = 15);

      TimeStepIndex := StrToInt(Splitter[0]);
      TS := StrToInt(Splitter[1]);
      TrackingTime := FortranStrToFloat(Splitter[2]);
      ParticleIndex := StrToInt(Splitter[3]);
      ParticleGroup := StrToInt(Splitter[4]);
      XPrime := FortranStrToFloat(Splitter[5]);
      YPrime := FortranStrToFloat(Splitter[6]);
      Z := FortranStrToFloat(Splitter[7]);
      GridNumber := StrToInt(Splitter[8]);
      Layer := StrToInt(Splitter[9]);
      Row := StrToInt(Splitter[10]);
      Column := StrToInt(Splitter[11]);
      LocalX := FortranStrToFloat(Splitter[12]);
      Localy := FortranStrToFloat(Splitter[13]);
      LocalZ := FortranStrToFloat(Splitter[14]);

      CreateParticle;

      if FirstRow then
      begin
        FirstRow := False;
        MinParticleGroup := ParticleGroup;
        MaxParticleGroup := ParticleGroup;
      end
      else
      begin
        if ParticleGroup < MinParticleGroup then
        begin
          MinParticleGroup := ParticleGroup
        end;
        if ParticleGroup > MaxParticleGroup then
        begin
          MaxParticleGroup := ParticleGroup
        end;
      end;
    end;
  finally
    Splitter.Free;
    CloseFile(FTextFile);
  end;

  MaxPoints := 0;
  FirstTimeFound := False;
  if Series.Count > 0 then
  begin
    for LineIndex := 0 to Series.Count - 1 do
    begin
      Line := Series[LineIndex];
      if Line.Points.Count > MaxPoints then
      begin
        MaxPoints := Line.Points.Count;
      end;
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count-1];
        if not FirstTimeFound then
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            MinTime := FirstPoint.TrackingTime;
            MaxTime := LastPoint.TrackingTime;
          end
          else
          begin
            MinTime := LastPoint.TrackingTime;
            MaxTime := FirstPoint.TrackingTime;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            if FirstPoint.TrackingTime < MinTime then
            begin
              MinTime := FirstPoint.TrackingTime
            end;
            if LastPoint.TrackingTime > MaxTime then
            begin
              MaxTime := LastPoint.TrackingTime;
            end;
          end
          else
          begin
            if LastPoint.TrackingTime < MinTime then
            begin
              MinTime := LastPoint.TrackingTime
            end;
            if FirstPoint.TrackingTime > MaxTime then
            begin
              MaxTime := FirstPoint.TrackingTime;
            end;
          end;
        end;
      end;
    end;
  end;
  if TimeIndex >= MaxPoints  then
  begin
    TimeIndex := MaxPoints-1;
  end;
end;
{$HINTS ON}

{$HINTS OFF}
procedure TTimeSeriesReader.ReadFileV7;
var
  ALine: string;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  LocalX: single;
  LocalY: single;
  Z: single;
  TrackingTime: single;
  Column: integer;
  TS: integer;
  Layer: integer;
  Row: integer;
  TimeSeries: TTimeSeriesV7;
  APoint: TTimeSeriesPointV7;
//  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
//  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
  LineIndex: Integer;
  Line: TTimeSeries;
  FirstPoint: TTimeSeriesPoint;
  LastPoint: TTimeSeriesPoint;
  FirstTimeFound: Boolean;
  FTextFile: TextFile;
  TimeStepIndex: integer;
  MaxPoints: Integer;
  Splitter: TStringList;
  TrackingDirection: Integer;
  ReferenceTime: double;
  ParticleGroup: integer;
  FirstRow: boolean;
  XOrigin: double;
  YOrigin: double;
  AngRot: double;
  Origin: TPoint2D;
  ODistance: Double;
  OAngle: Double;
  XOffSet: Double;
  YOffSet: Double;
  SequenceNumber: Integer;
  CellNumber: Integer;
  NRow: Integer;
  NCol: Integer;
  LocalModel: TCustomModel;
  APoint2D: TPoint2D;
  ErrorLines: TStringList;
  function RotateToRealWorldCoordinates(
    const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
    DistanceP: double;
    AngleP: double;
  begin
    result := APoint;
    if AngRot <> 0 then
    begin
      DistanceP := Distance(result, Origin);
      AngleP := ArcTan2(result.Y - Origin.Y, result.x - Origin.x) / Pi *180;
      temp := ProjectPoint(Origin, AngleP + AngRot,DistanceP);
      result := temp;
    end;
  end;

  procedure CellNumberToRowCol(CellNumber: Integer; out ARow, ACol: Integer);
  begin
    if Grid = nil then
    begin
      ARow := 0;
      ACol := 0;
    end
    else
    begin
      CellNumber := (CellNumber -1) mod (NRow * NCol);
      ARow := CellNumber div NCol + 1;
      ACol := CellNumber mod ARow + 1;
    end;
  end;
  procedure PositionToRowCol(XPrime, YPrime: double; out ARow, ACol: Integer);
  var
    APoint: TPoint2D;
    ACell: T2DTopCell;
  begin
    APoint.x := XPrime;
    APoint.y := YPrime;
    ACell := Grid.TopContainingCell(APoint, eaBlocks, False);
    ARow := ACell.Row+1;
    ACol := ACell.Col+1;
  end;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FSeriesV7.Count < ParticleIndex do
    begin
      FSeriesV7.Add;
    end;

    TimeSeries := FSeriesV7[ParticleIndex-1];

    APoint := TimeSeries.FPoints.Add as TTimeSeriesPointV7;

    APoint.FXPrime := XPrime + XOffSet;
    APoint.FYPrime := YPrime + YOffSet;

    if LocalModel.DisvUsed then
    begin
      PositionToRowCol(APoint.FXPrime, APoint.FYPrime, Row, Column);
    end
    else
    begin
      CellNumberToRowCol(CellNumber, Row, Column)
    end;

    Point2D.X := XPrime + XOrigin;
    Point2D.Y := YPrime + YOrigin;
    Point2D := RotateToRealWorldCoordinates(Point2D);

    APoint.ParticleIndex := ParticleIndex;
    APoint.FTimeStepIndex := TimeStepIndex;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTrackingTime := TrackingTime;
    APoint.FLayer := Layer;
    APoint.FRow := Row;
    APoint.FColumn := Column;
    APoint.FTimeStep := TS;
    APoint.ParticleGroup := ParticleGroup;
    APoint.SequenceNumber := SequenceNumber;
    APoint.CellNumber := CellNumber;

    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  LocalModel := FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  FreeAndNil(FTimes);
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FSeries.Clear;
  FSeriesV6.Clear;
  FSeriesV7.Clear;
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;

  Splitter := TStringList.Create;
  ErrorLines := TStringList.Create;
  AssignFile(FTextFile, FFileName);
  try
    Splitter.Delimiter := ' ';
    Reset(FTextFile);

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'MODPATH_TIMESERIES_FILE         7         2');

    Readln(FTextFile, ALine);
    Splitter.DelimitedText := ALine;
    Assert(Splitter.Count = 5);
    TrackingDirection := StrToInt(Splitter[0]);
    ReferenceTime := FortranStrToFloat(Splitter[1]);
    XOrigin := FortranStrToFloat(Splitter[2]);
    YOrigin := FortranStrToFloat(Splitter[3]);
    AngRot := FortranStrToFloat(Splitter[4]);
    if LocalModel.ModelSelection <> msModflow2015 then
    begin
      // Even if a grid metadata file is present, the data from it is not
      // recorded in the pathlinle file.
      APoint2D := Grid.TwoDElementCorner(0, Grid.RowCount);
      XOrigin := APoint2D.x;
      YOrigin := APoint2D.y;
      AngRot := Grid.GridAngle * 180 / Pi;
    end;
    Origin.x := XOrigin;
    Origin.y := YOrigin;

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'END HEADER');

    ODistance := Sqrt(Sqr(Origin.X) + Sqr(Origin.Y));
    OAngle := ArcTan2(Origin.x, Origin.y);
    XOffSet := Sin(AngRot/180*Pi + OAngle)*ODistance;
    YOffSet := Cos(AngRot/180*Pi + OAngle)*ODistance;

    FirstRow := true;
    While Not Eof(FTextFile) do
    begin
      Readln(FTextFile, ALine);

      Splitter.DelimitedText := ALine;
      Assert(Splitter.Count = 14);

      try
        TimeStepIndex := StrToInt(Splitter[0]);
        TS := StrToInt(Splitter[1]);
        TrackingTime := FortranStrToFloat(Splitter[2]);
        SequenceNumber := StrToInt(Splitter[3]);
        ParticleGroup := StrToInt(Splitter[4]);
        ParticleIndex := StrToInt(Splitter[5]);
        CellNumber := StrToInt(Splitter[6]);
        LocalX := FortranStrToFloat(Splitter[7]);
        Localy := FortranStrToFloat(Splitter[8]);
        LocalZ := FortranStrToFloat(Splitter[9]);
        XPrime := FortranStrToFloat(Splitter[10]);
        YPrime := FortranStrToFloat(Splitter[11]);
        Z := FortranStrToFloat(Splitter[12]);
        Layer := StrToInt(Splitter[13]);

        CreateParticle;

        if FirstRow then
        begin
          FirstRow := False;
          MinParticleGroup := ParticleGroup;
          MaxParticleGroup := ParticleGroup;
        end
        else
        begin
          if ParticleGroup < MinParticleGroup then
          begin
            MinParticleGroup := ParticleGroup
          end;
          if ParticleGroup > MaxParticleGroup then
          begin
            MaxParticleGroup := ParticleGroup
          end;
        end;
      except on EConvertError do
        begin
          if ErrorLines.Count < 10 then
          begin
            ErrorLines.Add(ALine);
          end;
        end;
      end;
    end;
    if ErrorLines.Count > 0 then
    begin
      Beep;
      MessageDlg(StrTheFollowingLines + sLineBreak + ErrorLines.Text, mtError, [mbOK], 0);
    end;
  finally
    ErrorLines.Free;
    Splitter.Free;
    CloseFile(FTextFile);
  end;

  MaxPoints := 0;
  FirstTimeFound := False;
  if Series.Count > 0 then
  begin
    for LineIndex := 0 to Series.Count - 1 do
    begin
      Line := Series[LineIndex];
      if Line.Points.Count > MaxPoints then
      begin
        MaxPoints := Line.Points.Count;
      end;
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count-1];
        if not FirstTimeFound then
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            MinTime := FirstPoint.TrackingTime;
            MaxTime := LastPoint.TrackingTime;
          end
          else
          begin
            MinTime := LastPoint.TrackingTime;
            MaxTime := FirstPoint.TrackingTime;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            if FirstPoint.TrackingTime < MinTime then
            begin
              MinTime := FirstPoint.TrackingTime
            end;
            if LastPoint.TrackingTime > MaxTime then
            begin
              MaxTime := LastPoint.TrackingTime;
            end;
          end
          else
          begin
            if LastPoint.TrackingTime < MinTime then
            begin
              MinTime := LastPoint.TrackingTime
            end;
            if FirstPoint.TrackingTime > MaxTime then
            begin
              MaxTime := FirstPoint.TrackingTime;
            end;
          end;
        end;
      end;
    end;
  end;
  if TimeIndex >= MaxPoints  then
  begin
    TimeIndex := MaxPoints-1;
  end;
end;
{$HINTS ON}

procedure TTimeSeriesReader.Record3DTimeSeries(TimeIndex: integer);
var
  Grid: TModflowGrid;
  ColRowOrLayer: Integer;
  MaxValue: Double;
  MinValue: Double;
  PointIndex: Integer;
  TimeSeriesPoint: TTimeSeriesPoint;
  AColor: TColor;
  ASeries: TCustomTimeSeries;
  TimeToPlot: Double;
  PlotIndex: Integer;
  LocalSeries: TCustomTimeSeriesCollection;
begin
  if not Visible then
  begin
    Exit;
  end;
  if TimeIndex < 0 then
  begin
    Exit;
  end;
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  if Times.Count = 0 then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(TimeSeriesGLIndex[TimeIndex], GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      glLineWidth(1);

      TimeToPlot := Times[TimeIndex];

      glBegin(GL_POINTS);

      LocalSeries := ActiveSeries;

      for PointIndex := 0 to LocalSeries.Count - 1 do
      begin
        ASeries := LocalSeries[PointIndex];
        PlotIndex := ASeries.Times.IndexOf(TimeToPlot);

        if PlotIndex >= 0 then
        begin
          if CheckShowSeries(ASeries) then
          begin
            TimeSeriesPoint := ASeries.Points[PlotIndex];
            // A dummy segment can be used because the segment isn't used
            // if the orientation is dso3D;
            if TimeSeriesPoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer,
              EquateSegment(0,0,0,0), False) then
            begin
              AColor := GetPointColor(MaxValue, MinValue, TimeSeriesPoint);
              AssignColor(AColor);
              glVertex3f(TimeSeriesPoint.XPrime, TimeSeriesPoint.YPrime, TimeSeriesPoint.Z);
            end;
          end;
        end;
      end;
      glEnd;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

procedure TTimeSeriesReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TTimeSeriesReader.SetLines(const Value: TTimeSeriesCollection);
begin
  FSeries.Assign(Value);
end;

procedure TTimeSeriesReader.SetLinesV6(const Value: TTimeSeriesCollectionV6);
begin
  FSeriesV6.Assign(Value);
end;

procedure TTimeSeriesReader.SetLinesV7(const Value: TTimeSeriesCollectionV7);
begin
  FSeriesV7.Assign(Value);
end;

procedure TTimeSeriesReader.SetMaxParticleGroup(const Value: Integer);
begin
  FMaxParticleGroup := Value;
end;

procedure TTimeSeriesReader.SetMaxTime(const Value: double);
begin
  FMaxTime := Value;
end;

procedure TTimeSeriesReader.SetMinParticleGroup(const Value: Integer);
begin
  FMinParticleGroup := Value;
end;

procedure TTimeSeriesReader.SetMinTime(const Value: double);
begin
  FMinTime := Value;
end;

procedure TTimeSeriesReader.SetRecordedTimeSeries(ATimeIndex: integer;
  const Value: boolean);
begin
  EnsureGLArrays(ATimeIndex);
  FRecordedTimeSeries[ATimeIndex] := Value;
end;

procedure TTimeSeriesReader.SetTimeIndex(const Value: integer);
begin
  FTimeIndex := Value;
end;

procedure TTimeSeriesReader.SetTimes(const Value: TRealList);
begin
  if (Value = nil) or (Value.Count = 0) then
  begin
    FreeAndNil(FTimes);
  end
  else
  begin
    if FTimes = nil then
    begin
      FTimes := TRealList.Create;
    end;
    FTimes.Assign(Value);
  end;
end;

{ TTimeSeries }

procedure TTimeSeries.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeries;
begin
  if Source is TTimeSeries then
  begin
    SourceSeries := TTimeSeries(Source);
    Points := SourceSeries.Points;
  end;
  inherited;
end;

constructor TTimeSeries.Create(Collection: TCollection);
begin
  inherited;
  FPoints:= TTimeSeriesPoints.Create;
end;

destructor TTimeSeries.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TTimeSeries.SetPoints(const Value: TTimeSeriesPoints);
begin
  FPoints.Assign(Value);
end;

procedure TCustomTimeSeries.SetTimes(const Value: TRealList);
begin
  if (Value = nil) or (Value.Count = 0) then
  begin
    FreeAndNil(FTimes);
  end
  else
  begin
    if FTimes = nil then
    begin
      FTimes := TRealList.Create;
    end;
    FTimes.Assign(Value);
  end;
end;

procedure TCustomTimeSeries.Assign(Source: TPersistent);
var
  SourceSeries: TCustomTimeSeries;
begin
  if Source is TCustomTimeSeries then
  begin
    SourceSeries := TCustomTimeSeries(Source);
    Times := SourceSeries.Times;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCustomTimeSeries.Create(Collection: TCollection);
begin
  inherited;
  FTimes := nil;
end;

destructor TCustomTimeSeries.Destroy;
begin
  FTimes.Free;
  inherited;
end;

function TTimeSeries.GetPoints: TCustomTimeSeriesPoints;
begin
  result := FPoints;
end;

function TTimeSeries.GetTimes: TRealList;
var
  APoint: TTimeSeriesPoint;
  PointIndex: Integer;
begin
  if FTimes = nil then
  begin
    FTimes := TRealList.Create;
    for PointIndex := 0 to Points.Count - 1 do
    begin
      APoint := Points[PointIndex];
      FTimes.Add(APoint.TrackingTime);
    end;
    FTimes.Sorted := True;
  end;
  result := FTimes;
end;

{ TTimeSeriesPoint }

procedure TTimeSeriesPoint.Assign(Source: TPersistent);
var
  SourcePoint: TTimeSeriesPoint;
begin
  if Source is TTimeSeriesPoint then
  begin
    SourcePoint:= TTimeSeriesPoint(Source);
    TimeStepIndex := SourcePoint.TimeStepIndex;
    ParticleIndex := SourcePoint.ParticleIndex;
    Layer := SourcePoint.Layer;
    Row := SourcePoint.Row;
    Column := SourcePoint.Column;
    XPrime := SourcePoint.XPrime;
    YPrime := SourcePoint.YPrime;
    X := SourcePoint.X;
    Y := SourcePoint.Y;
    Z := SourcePoint.Z;
    LocalZ := SourcePoint.LocalZ;
    TrackingTime := SourcePoint.TrackingTime;
    TimeStep := SourcePoint.TimeStep;
  end
  else
  begin
    inherited;
  end;
end;

function TTimeSeriesPoint.CheckLimits(
  Limits: TTimeSeriesDisplayLimits): boolean;
begin
  result := True;
  if Limits.ColumnLimits.UseLimit then
  begin
    result := (Limits.ColumnLimits.StartLimit <= Column)
      and (Column <= Limits.ColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.RowLimits.UseLimit then
  begin
    result := (Limits.RowLimits.StartLimit <= Row)
      and (Row <= Limits.RowLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.LayerLimits.UseLimit then
  begin
    result := (Limits.LayerLimits.StartLimit <= Layer)
      and (Layer <= Limits.LayerLimits.EndLimit);
    if not result then Exit;
  end;

end;

function TTimeSeriesPoint.ShouldShow(Limits: TTimeSeriesDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer;
  const Segment: TSegment2D; const DisvUsed: Boolean): boolean;
var
  ColRowOrLayerToCheck: Integer;
  Dummy: Double;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Orientation of
      dsoTop:
        begin
          ColRowOrLayerToCheck := Layer;
        end;
      dsoFront:
        begin
          ColRowOrLayerToCheck := Row;
        end;
      dsoSide:
        begin
          ColRowOrLayerToCheck := Column;
        end;
      else Assert(False);
    end;
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      Exit;
    end;
  end;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified, scStart, scEnd:
      begin
        result := CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;
  if result and DisvUsed and (Orientation = dsoFront) then
  begin
    result := DisvFrontProjectedXPrime(Segment, EquatePoint(X, y), Dummy);
  end;
end;

function TTimeSeriesPoint.ShouldShowSeries(
  Limits: TTimeSeriesDisplayLimits): boolean;
begin
  result := True;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        // do nothing
      end;
    scStart:
      begin
        Assert(Index = 0);
        result := CheckLimits(Limits);
      end;
    scEnd:
      begin
        Assert(Index = Collection.Count-1);
        result := CheckLimits(Limits);
      end;
    else Assert(False);
  end;

end;

{ TTimeSeriesPoints }

constructor TTimeSeriesPoints.Create;
begin
  inherited Create(TTimeSeriesPoint);
end;

function TCustomTimeSeriesPoints.GetPoint(Index: integer): TTimeSeriesPoint;
begin
  result := Items[Index] as TTimeSeriesPoint;
end;

{ TTimeSeriesCollection }

constructor TTimeSeriesCollection.Create;
begin
  inherited Create(TTimeSeries);
end;

function TTimeSeriesCollection.GetSeries(Index: integer): TTimeSeries;
begin
  result := Items[Index] as TTimeSeries;
end;

{ TTimeSeriesDisplayLimits }

procedure TTimeSeriesDisplayLimits.Assign(Source: TPersistent);
var
  TimeSeriesSource: TTimeSeriesDisplayLimits;
begin
  if Source is TTimeSeriesDisplayLimits then
  begin
    TimeSeriesSource := TTimeSeriesDisplayLimits(Source);
    ShowChoice := TimeSeriesSource.ShowChoice;
    LimitToCurrentIn2D := TimeSeriesSource.LimitToCurrentIn2D;
    ColumnLimits := TimeSeriesSource.ColumnLimits;
    RowLimits := TimeSeriesSource.RowLimits;
    LayerLimits := TimeSeriesSource.LayerLimits;
    ParticleGroupLimits := TimeSeriesSource.ParticleGroupLimits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesDisplayLimits.Create;
begin
  inherited;
  FLimitToCurrentIn2D := True;
  FLayerLimits:= TShowIntegerLimit.Create;
  FRowLimits:= TShowIntegerLimit.Create;
  FColumnLimits:= TShowIntegerLimit.Create;
  FParticleGroupLimits := TShowIntegerLimit.Create;
end;

destructor TTimeSeriesDisplayLimits.Destroy;
begin
  FParticleGroupLimits.Free;
  FColumnLimits.Free;
  FRowLimits.Free;
  FLayerLimits.Free;
  inherited;
end;

procedure TTimeSeriesDisplayLimits.SetColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FColumnLimits.Assign(Value);
end;

procedure TTimeSeriesDisplayLimits.SetLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FLayerLimits.Assign(Value)
end;

procedure TTimeSeriesDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TTimeSeriesDisplayLimits.SetParticleGroupLimits(
  const Value: TShowIntegerLimit);
begin
  FParticleGroupLimits.Assign(Value);
end;

procedure TTimeSeriesDisplayLimits.SetRowLimits(const Value: TShowIntegerLimit);
begin
  FRowLimits.Assign(Value);
end;

procedure TTimeSeriesDisplayLimits.SetShowChoice(const Value: TShowChoice);
begin
  FShowChoice := Value;
end;

{ TTimeSeriesColorLimits }

procedure TTimeSeriesColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TTimeSeriesColorLimits;
begin
  if Source is TTimeSeriesColorLimits then
  begin
    SourceLimits := TTimeSeriesColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesColorLimits.Create;
begin
  inherited;
  FColoringChoice := tscParticleNumber;
end;

procedure TTimeSeriesColorLimits.SetColoringChoice(
  const Value: TTimeSeriesColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TTimeSeriesColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TTimeSeriesColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TTimeSeriesColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

procedure TPathLineSettings.SetDisplayLimits(const Value: TPathLineDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TPathLineSettings.Assign(Source: TPersistent);
var
  PathLineSettings: TPathLineSettings;
begin
  if Source is TPathLineSettings then
  begin
    PathLineSettings := TPathLineSettings(Source);
    DisplayLimits := PathLineSettings.DisplayLimits;
    ColorLimits := PathLineSettings.ColorLimits;
  end;
  inherited;
end;

constructor TPathLineSettings.Create;
begin
  inherited;
  FDisplayLimits:= TPathLineDisplayLimits.Create;
  FColorLimits := TPathlineColorLimits.Create;
end;

destructor TPathLineSettings.Destroy;
begin
  FColorLimits.Free;
  FDisplayLimits.Free;
  inherited;
end;

procedure TPathLineSettings.SetColorLimits(const Value: TPathlineColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TEndPointSettings.Assign(Source: TPersistent);
var
  EndPointSettings: TEndPointSettings;
begin
  if Source is TEndPointSettings then
  begin
    EndPointSettings := TEndPointSettings(Source);
    ColorLimits := EndPointSettings.ColorLimits;
    DisplayLimits := EndPointSettings.DisplayLimits;
    LegendVisible := EndPointSettings.LegendVisible;
    EndPointSize := EndPointSettings.EndPointSize;
  end;
  inherited;
end;

constructor TEndPointSettings.Create;
begin
  inherited;
  FColorLimits := TEndPointColorLimits.Create;
  FDisplayLimits:= TEndPointDisplayLimits.Create;
  FLegendVisible := True;
  FEndPointSize := 4;
end;

destructor TEndPointSettings.Destroy;
begin
  FDisplayLimits.Free;
  FColorLimits.Free;
  inherited;
end;

procedure TEndPointSettings.SetColorLimits(const Value: TEndPointColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TEndPointSettings.SetDisplayLimits(const Value: TEndPointDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TEndPointSettings.SetEndPointSize(const Value: Integer);
begin
  FEndPointSize := Value;
end;

procedure TEndPointSettings.SetLegendVisible(const Value: boolean);
begin
  FLegendVisible := Value;
end;

procedure TTimeSeriesSettings.Assign(Source: TPersistent);
var
  SourceSettings: TTimeSeriesSettings;
begin
  if Source is TTimeSeriesSettings then
  begin
    SourceSettings := TTimeSeriesSettings(Source);
    ColorLimits := SourceSettings.ColorLimits;
    DisplayLimits := SourceSettings.DisplayLimits;
    TimeSeriesSize := SourceSettings.TimeSeriesSize;
  end;
  inherited;
end;

constructor TTimeSeriesSettings.Create;
begin
  inherited;
  FDisplayLimits := TTimeSeriesDisplayLimits.Create;
  FColorLimits := TTimeSeriesColorLimits.Create;
  FTimeSeriesSize := 4;
end;

destructor TTimeSeriesSettings.Destroy;
begin
  FColorLimits.Free;
  FDisplayLimits.Free;
  inherited;
end;

procedure TTimeSeriesSettings.SetColorLimits(const Value: TTimeSeriesColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TTimeSeriesSettings.SetDisplayLimits(const Value: TTimeSeriesDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TTimeSeriesSettings.SetTimeSeriesSize(const Value: Integer);
begin
  FTimeSeriesSize := Value;
end;

procedure TCustomModpathSettings.Assign(Source: TPersistent);
var
  Settings: TCustomModpathSettings;
begin
  if Source is TCustomModpathSettings then
  begin
    Settings := TCustomModpathSettings(Source);
    ColorParameters := Settings.ColorParameters;
    Visible := Settings.Visible;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCustomModpathSettings.Create;
begin
  inherited;
  FColorParameters:= TColorParameters.Create;
  FVisible := True;
end;

destructor TCustomModpathSettings.Destroy;
begin
  FColorParameters.Free;
  inherited;
end;

procedure TCustomModpathSettings.SetColorParameters(const Value: TColorParameters);
begin
  FColorParameters.Assign(Value);
end;

{ TPathLinePointsV6 }

constructor TPathLinePointsV6.Create(PathLine: TPathLineV6);
begin
  inherited Create(TPathLinePointV6, PathLine);
end;

{ TPathLineV6 }

constructor TPathLineV6.Create(Collection: TCollection);
begin
  inherited;
  FPoints := TPathLinePointsV6.Create(self);
end;

{ TPathLinesV6 }

constructor TPathLinesV6.Create;
begin
  inherited Create(TPathLineV6);
end;

procedure TCustomPathLinesV6.UpdateShapeFileFields(ALine: TCustomPathLine;
  ShapeDataBase: TXBase);
var
  FirstPoint: TPathLinePoint;
begin
  inherited;
  FirstPoint := ALine.Points[0];
  ShapeDataBase.UpdFieldInt(XbaseFieldName(StrParticleGroup),
    (FirstPoint as TPathLinePointV6).ParticleGroup);
end;

procedure TCustomPathLinesV6.DefineShapeFileFields(Fields: TStringList);
begin
  inherited;
  Fields.Add(string(XbaseFieldName(StrParticleGroup) + '=N'));
end;

{ TPathLineReader }

procedure TPathLineReader.DrawLines(Orientation: TDataSetOrientation;
  const BitMap: TPersistent; LocalLines: TCustomPathLines);
const
  MaxCoord = MaxInt -2;
  MinCoord = -MaxCoord;
var
  LineIndex: Integer;
  Line: TCustomPathLine;
  APoint: TPathLinePoint;
  ColRowOrLayer: integer;
  PointIndex: Integer;
  ShowPriorPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
  Points: array [0..1] of TPoint;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
  AColor: TColor;
  AColor32: TColor32;
  QuadTree: TRbwQuadTree;
  ShouldInitializeTree: Boolean;
  Limits: TGridLimit;
  ARect: TRect;
  LocalModel: TCustomModel;
  CrossSectionSegment: TSegment2D;
  DisvUsed: Boolean;
  DisplayXPrime: Double;
  OriginOffset: double;
//  CrossSectionSegment: TLine2D;
begin

  if not Visible then
  begin
    Exit;
  end;
  LocalModel := FModel as TCustomModel;

  DisvUsed := LocalModel.DisvUsed;
  if DisvUsed then
  begin
    CrossSectionSegment := LocalModel.DisvGrid.CrossSection.Segment;
    OriginOffset := GetOriginOffset(CrossSectionSegment);
  end
  else
  begin
    OriginOffset := 0;
    CrossSectionSegment := EquateSegment(0,0,0,0);
  end;

  if (LocalModel.LayerCount <= 0) or (LocalModel.RowCount <= 0)
    or (LocalModel.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  QuadTree := nil;
  case Orientation of
    dsoTop:
      begin
        QuadTree := TopQuadTree;
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedLayer+1;
      end;
    dsoFront:
      begin
        QuadTree := FrontQuadTree;
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedRow+1;
      end;
    dsoSide:
      begin
        QuadTree := SideQuadTree;
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := LocalModel.SelectedColumn+1;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);

  ShouldInitializeTree := QuadTree.Count = 0;
  if not ShouldInitializeTree then
  begin
    case Orientation of
      dsoTop:
        begin
          ShouldInitializeTree := FSelectedLayer <> LocalModel.SelectedLayer;
        end;
      dsoFront:
        begin
          ShouldInitializeTree := FSelectedRow <> LocalModel.SelectedRow;
        end;
      dsoSide:
        begin
          ShouldInitializeTree := FSelectedColumn <> LocalModel.SelectedColumn;
        end;
    end;
  end;
  if ShouldInitializeTree then
  begin
    QuadTree.Clear;
    Limits := LocalModel.DiscretizationLimits(OrientationToViewDirection(Orientation));
    case Orientation of
      dsoTop:
        begin
          FSelectedLayer := LocalModel.SelectedLayer;
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxY;
          QuadTree.YMin := Limits.MinY;
        end;
      dsoFront:
        begin
          FSelectedRow := LocalModel.SelectedRow;
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end;
      dsoSide:
        begin
          FSelectedColumn := LocalModel.SelectedColumn;
          QuadTree.XMax := Limits.MaxY;
          QuadTree.XMin := Limits.MinY;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end
      else Assert(False);
    end;
  end;
  for LineIndex := 0 to LocalLines.Count - 1 do
  begin
    Line := LocalLines[LineIndex];
    if Line.Points.Count > 0 then
    begin
      if CheckShowLine(Line) then
      begin
        ShowPriorPoint := False;
        Points[0].X := 0;
        Points[0].Y := 0;
        for PointIndex := 0 to Line.Points.Count - 1 do
        begin
          APoint := Line.Points[PointIndex];
          if APoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer,
            CrossSectionSegment, DisvUsed) then
          begin
            case Orientation of
              dsoTop:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(APoint.X, APoint.Y, APoint);
                  end;
                end;
              dsoFront:
                begin
                  if LocalModel.DisvUsed then
                  begin
                    DisvFrontProjectedXPrime(CrossSectionSegment,
                      EquatePoint(APoint.X, APoint.Y), DisplayXPrime);
                    DisplayXPrime := DisplayXPrime - OriginOffset;
                  end
                  else
                  begin
                    DisplayXPrime := APoint.XPrime;
                  end;
                  ADisplayPoint.X := ZoomBox.XCoord(DisplayXPrime);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Z);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(DisplayXPrime, APoint.Z, APoint);
                  end;
                end;
              dsoSide:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.Z);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.YPrime);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(APoint.Z, APoint.YPrime, APoint);
                  end;
                end;
              else Assert(False);
            end;
            if Line.Points.Count = 1 then
            begin

              if (ADisplayPoint.X <= MaxCoord)
                and (ADisplayPoint.X >= MinCoord)
                and (ADisplayPoint.Y <= MaxCoord)
                and (ADisplayPoint.Y >= MinCoord) then
              begin
                AColor := GetPointColor(MaxValue, MinValue, APoint);
                AColor32 := Color32(AColor);
                ARect.Top := ADisplayPoint.Y -2;
                ARect.Bottom := ADisplayPoint.Y +2;
                ARect.Left := ADisplayPoint.X -2;
                ARect.Right := ADisplayPoint.X +2;
                DrawBigRectangle32(BitMap, AColor32, AColor32, 1, ARect);
              end;
            end
            else
            begin
              Points[1] := ADisplayPoint;
              if ShowPriorPoint then
              begin
                AColor := GetPointColor(MaxValue, MinValue, APoint);
                AColor32 := Color32(AColor);
                DrawBigPolyline32(BitMap, AColor32, 1, Points, True);
              end;
              Points[0] := ADisplayPoint;
              ShowPriorPoint := True;
            end;
          end
          else
          begin
            ShowPriorPoint := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPathLineReader.DrawLines3D(LocalLines: TCustomPathLines);
var
  LocalModel: TCustomModel;
//var
//  Grid: TModflowGrid;
begin
  if FDrawingPathLines then
  begin
    Exit;
  end;
  if LocalLines.Count = 0 then
  begin
    Exit;
  end;
  try
    FDrawingPathLines := True;


    if (not FRecordedPathLines) then
    begin
      Record3DPathLines(LocalLines);
      // FRecordedPathLines is private and won't be set
      // by overridden versions of RecordFront.
      FRecordedPathLines := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    LocalModel := FModel as TCustomModel;
//    Grid := (FModel as TCustomModel).ModflowGrid;
//    if Grid = nil then
//    begin
//      Exit;
//    end;
    if (LocalModel.LayerCount <= 0) or (LocalModel.RowCount <= 0)
      or (LocalModel.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(PathlineGLIndex);
  finally
    FDrawingPathLines := False;
  end;

end;

procedure TPathLineReader.ExportShapefile(FileName: string);
begin
  case ModpathVersion of
    pv5: Lines.ExportShapefile(FileName);
    pv6_0: LinesV6.ExportShapefile(FileName);
    pv7_2: LinesV7.ExportShapefile(FileName);
    else Assert(False);
  end;
end;

procedure TPathLineReader.UpdateMinMax;
begin
  if Lines.Count > 0 then
  begin
    UpdateMinMaxTime(Lines);
  end;
  if LinesV6.Count > 0 then
  begin
    UpdateMinMaxTime(LinesV6);
  end;
  if LinesV7.Count > 0 then
  begin
    UpdateMinMaxTime(LinesV7);
  end;
end;

procedure TPathLineReader.UpdateMinMaxTime(LocalLines: TCustomPathLines);
var
  LineIndex: Integer;
  TimeIndex: Integer;
  Line: TCustomPathLine;
  APathlinePoint: TPathLinePoint;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  FirstTimeFound: Boolean;
  FirstPositiveTimeFound: Boolean;
begin
  FirstTimeFound := False;
  FirstPositiveTimeFound := False;
  if LocalLines.Count > 0 then
  begin
    for LineIndex := 0 to LocalLines.Count - 1 do
    begin
      Line := LocalLines[LineIndex];
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count - 1];
        if not FirstTimeFound then
        begin
          if FirstPoint.AbsoluteTime < LastPoint.AbsoluteTime then
          begin
            MinTime := FirstPoint.AbsoluteTime;
            MaxTime := LastPoint.AbsoluteTime;
          end
          else
          begin
            MinTime := LastPoint.AbsoluteTime;
            MaxTime := FirstPoint.AbsoluteTime;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.AbsoluteTime < LastPoint.AbsoluteTime then
          begin
            if FirstPoint.AbsoluteTime < MinTime then
            begin
              MinTime := FirstPoint.AbsoluteTime;
            end;
            if LastPoint.AbsoluteTime > MaxTime then
            begin
              MaxTime := LastPoint.AbsoluteTime;
            end;
          end
          else
          begin
            if LastPoint.AbsoluteTime < MinTime then
            begin
              MinTime := LastPoint.AbsoluteTime;
            end;
            if FirstPoint.AbsoluteTime > MaxTime then
            begin
              MaxTime := FirstPoint.AbsoluteTime;
            end;
          end;
        end;
        for TimeIndex := 0 to Line.Points.Count - 1 do
        begin
          APathlinePoint := Line.Points[TimeIndex];
          if APathlinePoint.AbsoluteTime > 0 then
          begin
            if FirstPositiveTimeFound then
            begin
              if MinPositiveTime > APathlinePoint.AbsoluteTime then
              begin
                MinPositiveTime := APathlinePoint.AbsoluteTime;
              end;
            end
            else
            begin
              MinPositiveTime := APathlinePoint.AbsoluteTime;
            end;
            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPathLineReader.ReadFileV6;
var
  ALine: string;
  ParticleIndex: integer;
  XPrime: double;
  YPrime: double;
  LocalZ: double;
  Z: double;
  Time: double;
  Column: integer;
  TS: integer;
  Layer: integer;
  Row: integer;
  PathLine: TCustomPathLine;
  APoint: TPathLinePointV6;
  Grid: TModflowGrid;
  ADate: TDateTime;
  FTextFile: TextFile;
  TrackDirection: integer;
  RefTime: double;
  ParticleGroup: integer;
  TimePointIndex: integer;
  GridIndex: integer;
  LineSegmentIndex: integer;
  LocalX: double;
  LocalY: double;
  Splitter: TStringList;
  FirstLine: boolean;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FLinesV6.Count < ParticleIndex do
    begin
      FLinesV6.Add;
    end;

    PathLine := FLinesV6[ParticleIndex-1];

    APoint := PathLine.FPoints.Add as TPathLinePointV6;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTime := Time;
    APoint.FLayer := Layer;
    APoint.FRow := Row;
    APoint.FColumn := Column;
    APoint.FTimeStep := TS;

    APoint.ParticleGroup := ParticleGroup;
    APoint.TimePointIndex := TimePointIndex;
    APoint.LineSegmentIndex := LineSegmentIndex;
    APoint.ModpathVersion := pv6_0;
//    APoint.HasV6Data := True;
    APoint.GridIndex := GridIndex;
    APoint.LocalX := LocalX;
    APoint.LocalY := LocalY;

    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := (FModel as TCustomModel).ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FLinesV5.Clear;
  FLinesV6.Clear;
  FLinesV7.Clear;
//  NRow := Grid.RowCount;
//  NCol := Grid.ColumnCount;

  Splitter:= TStringList.Create;
  AssignFile(FTextFile, FFileName);
  try
    Splitter.Delimiter := ' ';
    Reset(FTextFile);
    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'MODPATH_PATHLINE_FILE 6 0');
    Readln(FTextFile, TrackDirection, RefTime);
    case TrackDirection of
      1:
        begin
          TrackingDirectionV6 := tdForward;
        end;
      2:
        begin
          TrackingDirectionV6 := tdBackward;
        end;
      else Assert(False);
    end;
    ReferenceTimeV6 := RefTime;

    Readln(FTextFile, ALine);
    Assert(Trim(ALine) = 'END HEADER');
    FirstLine := True;
    While Not Eof(FTextFile) do
    begin
      Readln(FTextFile, ALine);
      if ALine <> '' then
      begin
        Splitter.DelimitedText := ALine;
//        while Splitter.Count > 16 do
//        begin
//          // the particle group name has spaces in it.
//        // The MODPATH file was not created by ModelMuse.
//          Splitter[1] := Splitter[1] + ' ' + Splitter[2];
//          Splitter.Delete(2);
//        end;
        Assert(Splitter.Count = 16);

        ParticleIndex := StrToInt(Splitter[0]);
        ParticleGroup := StrToInt(Splitter[1]);
        TimePointIndex := StrToInt(Splitter[2]);
        TS := StrToInt(Splitter[3]);
        Time := FortranStrToFloat(Splitter[4]);
        XPrime := FortranStrToFloat(Splitter[5]);
        YPrime := FortranStrToFloat(Splitter[6]);
        Z := FortranStrToFloat(Splitter[7]);
        Layer := StrToInt(Splitter[8]);
        Row := StrToInt(Splitter[9]);
        Column := StrToInt(Splitter[10]);
        GridIndex := StrToInt(Splitter[11]);
        LocalX := FortranStrToFloat(Splitter[12]);
        LocalY := FortranStrToFloat(Splitter[13]);
        LocalZ := FortranStrToFloat(Splitter[14]);
        LineSegmentIndex := StrToInt(Splitter[15]);

        CreateParticle;

        if FirstLine then
        begin
          MinParticleGroup := ParticleGroup;
          MaxParticleGroup := ParticleGroup;
          FirstLine := False;
        end
        else
        begin
          if ParticleGroup < MinParticleGroup then
          begin
            MinParticleGroup := ParticleGroup;
          end;
          if ParticleGroup > MaxParticleGroup then
          begin
            MaxParticleGroup := ParticleGroup;
          end;
        end;
      end;
    end;
  finally
    Splitter.Free;
    try
      CloseFile(FTextFile);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrThereWasAnErrorW, [E.Message]), mtError, [mbOK], 0);
//        Exit;
      end;
    end;
  end;

  UpdateMinMaxTime(LinesV6);
end;

procedure TPathLineReader.ReadFileV7;
var
  ALine: string;
  ParticleIndex: integer;
  XPrime: double;
  YPrime: double;
  LocalZ: double;
  Z: double;
  Time: double;
  Column: integer;
//  TS: integer;
  Layer: integer;
  Row: integer;
  PathLine: TCustomPathLine;
  APoint: TPathLinePointV7;
  Grid: TModflowGrid;
  ADate: TDateTime;
  FTextFile: TextFile;
  TrackDirection: integer;
  RefTime: double;
  ParticleGroup: integer;
  LocalX: double;
  LocalY: double;
  Splitter: TStringList;
  FirstLine: boolean;
  XOrigin: double;
  YOrigin: double;
  AngRot: double;
  SequenceNumber: Integer;
  ParticleCount: Integer;
  NRow: Integer;
  NCol: Integer;
  CellNumber: Integer;
  TimeStep: Integer;
  StressPeriod: Integer;
  Origin: TPoint2D;
  LocalModel: TCustomModel;
  ODistance: Double;
  OAngle: Double;
  XOffSet: Double;
  YOffSet: Double;
  PIndex: Integer;
  APoint2D: TPoint2D;
  LineIndex: Integer;
  ErrorLines: TStringList;
  function RotateToRealWorldCoordinates(
    const APoint: TPoint2D): TPoint2D;
  var
    temp: TPoint2D;
    DistanceP: double;
    AngleP: double;
  begin
    result := APoint;
    if AngRot <> 0 then
    begin
      DistanceP := Distance(result, Origin);
      AngleP := ArcTan2(result.Y - Origin.Y, result.x - Origin.x) / Pi *180;
      temp := ProjectPoint(Origin, AngleP + AngRot,DistanceP);
      result := temp;
    end;
  end;

  procedure CellNumberToRowCol(CellNumber: Integer; out ARow, ACol: Integer);
  begin
    if Grid = nil then
    begin
      ARow := 0;
      ACol := 0;
    end
    else
    begin
      CellNumber := (CellNumber -1) mod (NRow * NCol);
      ARow := CellNumber div NCol + 1;
      ACol := CellNumber -(ARow-1)*NCol + 1;
    end;
  end;
  procedure PositionToRowCol(XPrime, YPrime: double; out ARow, ACol: Integer);
  var
    APoint: TPoint2D;
    ACell: T2DTopCell;
  begin
    APoint.x := XPrime;
    APoint.y := YPrime;
    ACell := Grid.TopContainingCell(APoint, eaBlocks, False);
    ARow := ACell.Row+1;
    ACol := ACell.Col+1;
  end;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FLinesV7.Count < ParticleIndex do
    begin
      FLinesV7.Add;
    end;

    PathLine := FLinesV7[ParticleIndex-1];

    APoint := PathLine.FPoints.Add as TPathLinePointV7;

    APoint.FXPrime := XPrime + XOffSet;
    APoint.FYPrime := YPrime + YOffSet;

    if LocalModel.DisvUsed then
    begin
      PositionToRowCol(APoint.FXPrime, APoint.FYPrime, Row, Column);
    end
    else
    begin
      CellNumberToRowCol(CellNumber, Row, Column)
    end;

    Point2D.X := XPrime + XOrigin;
    Point2D.Y := YPrime + YOrigin;
    Point2D := RotateToRealWorldCoordinates(Point2D);
	

    APoint.CellNumber := CellNumber;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTime := Time;
    APoint.FLayer := Layer;
    APoint.FRow := Row;
    APoint.FColumn := Column;

    APoint.ParticleGroup := ParticleGroup;
    APoint.TimePointIndex := 0;
    APoint.LineSegmentIndex := 0;
    APoint.ModpathVersion := pv7_2;
    APoint.GridIndex := 0;
    APoint.LocalX := LocalX;
    APoint.LocalY := LocalY;

    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);

    APoint.StressPeriod := StressPeriod;
    APoint.TimeStep := TimeStep;
    APoint.SequenceNumber := SequenceNumber;
  end;
begin
  LocalModel := FModel as TCustomModel;
  Grid := LocalModel.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FLinesV5.Clear;
  FLinesV7.Clear;
  FLinesV7.Clear;
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;

  LineIndex := 0;
  Splitter:= TStringList.Create;
  ErrorLines := TStringList.Create;
  AssignFile(FTextFile, FFileName);
  try
    try
      Splitter.Delimiter := ' ';
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      if Trim(ALine) <> 'MODPATH_PATHLINE_FILE         7         2' then
      begin
        Beep;
        MessageDlg(StrThereIsSomethingW, mtError, [mbOK], 0);
        Exit;
      end;
      Assert(Trim(ALine) = 'MODPATH_PATHLINE_FILE         7         2');
      Inc(LineIndex);
      Readln(FTextFile, TrackDirection, RefTime, XOrigin, YOrigin, AngRot);
      Inc(LineIndex);

      if LocalModel.ModelSelection <> msModflow2015 then
      begin
        // Even if a grid metadata file is present, the data from it is not
        // recorded in the pathlinle file.
        APoint2D := Grid.TwoDElementCorner(0, Grid.RowCount);
        XOrigin := APoint2D.x;
        YOrigin := APoint2D.y;
        AngRot := Grid.GridAngle * 180 / Pi;
      end;

      Origin.X := XOrigin;
      Origin.Y := YOrigin;

      case TrackDirection of
        1:
          begin
            TrackingDirectionV6 := tdForward;
          end;
        2:
          begin
            TrackingDirectionV6 := tdBackward;
          end;
        else Assert(False);
      end;
      ReferenceTimeV6 := RefTime;

      Readln(FTextFile, ALine);
      Inc(LineIndex);
      Assert(Trim(ALine) = 'END HEADER');

      ODistance := Sqrt(Sqr(Origin.X) + Sqr(Origin.Y));
      OAngle := ArcTan2(Origin.x, Origin.y);
      XOffSet := Sin(AngRot/180*Pi + OAngle)*ODistance;
      YOffSet := Cos(AngRot/180*Pi + OAngle)*ODistance;

      FirstLine := True;
      While Not Eof(FTextFile) do
      begin
        Readln(FTextFile, ALine);
        Inc(LineIndex);
        if ALine <> '' then
        begin
          Splitter.DelimitedText := ALine;
          Assert(Splitter.Count = 4);

          SequenceNumber := StrToInt(Splitter[0]);
          ParticleGroup := StrToInt(Splitter[1]);
          ParticleIndex := StrToInt(Splitter[2]);
          ParticleCount := StrToInt(Splitter[3]);

          for PIndex := 0 to ParticleCount - 1 do
          begin
            Readln(FTextFile, ALine);
            Inc(LineIndex);
            if ALine = '' then
            begin
              Beep;
              MessageDlg(Format(StrErrorPrematureTer, [LineIndex]), mtError, [mbOK], 0);
            end;
            Splitter.DelimitedText := ALine;
            if Splitter.Count <> 11 then
            begin
              Beep;
              MessageDlg(StrThereIsSomethingW, mtError, [mbOK], 0);
              Exit;
            end;
            Assert(Splitter.Count = 11, Format(StrErrorReadingTheFo, [LineIndex,ALine]));

            try
              CellNumber := StrToInt(Splitter[0]);
              XPrime := FortranStrToFloat(Splitter[1]);
              YPrime := FortranStrToFloat(Splitter[2]);
              Z := FortranStrToFloat(Splitter[3]);
              Time := FortranStrToFloat(Splitter[4]);
              LocalX := FortranStrToFloat(Splitter[5]);
              LocalY := FortranStrToFloat(Splitter[6]);
              LocalZ := FortranStrToFloat(Splitter[7]);
              Layer := StrToInt(Splitter[8]);
              StressPeriod := StrToInt(Splitter[9]);
              TimeStep := StrToInt(Splitter[10]);

              CreateParticle;

              if FirstLine then
              begin
                MinParticleGroup := ParticleGroup;
                MaxParticleGroup := ParticleGroup;
                FirstLine := False;
              end
              else
              begin
                if ParticleGroup < MinParticleGroup then
                begin
                  MinParticleGroup := ParticleGroup;
                end;
                if ParticleGroup > MaxParticleGroup then
                begin
                  MaxParticleGroup := ParticleGroup;
                end;
              end;
            except on EConvertError do
              begin
                if ErrorLines.Count < 10 then
                begin
                  ErrorLines.Add(ALine);
                end;
              end;
            end;
          end;
        end;
      end;
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrThereWasAnErrorW, [E.Message]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
    if ErrorLines.Count > 0 then
    begin
      Beep;
      MessageDlg(StrTheFollowingLines + sLineBreak + ErrorLines.Text, mtError, [mbOK], 0);
    end;
  finally
    ErrorLines.Free;
    Splitter.Free;
    try
      CloseFile(FTextFile);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrThereWasAnErrorW, [E.Message]), mtError, [mbOK], 0);
      end;
    end;
  end;

  UpdateMinMaxTime(LinesV7);
end;

procedure TPathLineReader.Record3DPathLines(LocalLines: TCustomPathLines);
var
  LineIndex: Integer;
  Line: TCustomPathLine;
  APoint: TPathLinePoint;
  ColRowOrLayer: integer;
  PointIndex: Integer;
  ShowPriorPoint: Boolean;
  MaxValue, MinValue: double;
//  Grid: TModflowGrid;
  AColor: TColor;
  LineVisible: boolean;
  PriorPoint: TPathLinePoint;
  LocalModel: TCustomModel;
//  NewLine: Boolean;

  procedure StartLine;
  begin
    if not LineVisible then
    begin
      glBegin(GL_LINE_STRIP);
      LineVisible := True;
      AColor := GetPointColor(MaxValue, MinValue, PriorPoint);
      AssignColor(AColor);

      glVertex3f(PriorPoint.XPrime, PriorPoint.YPrime, PriorPoint.Z);
    end;
  end;
  procedure EndLine;
  begin
    if LineVisible then
    begin
      glEnd;
      LineVisible := False;
    end;
  end;
begin

  if not Visible then
  begin
    Exit;
  end;
  LocalModel := FModel as TCustomModel;
//  Grid := (FModel as TCustomModel).ModflowGrid;
//  if Grid = nil then
//  begin
//    Exit;
//  end;
  if (LocalModel.LayerCount <= 0) or (LocalModel.RowCount <= 0)
    or (LocalModel.ColumnCount <= 0) then
  begin
    Exit;
  end;



  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(PathlineGLIndex, GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      glLineWidth(1);

      LineVisible := False;
      for LineIndex := 0 to LocalLines.Count - 1 do
      begin
        Line := LocalLines[LineIndex];
        if Line.Points.Count > 0 then
        begin
          if CheckShowLine(Line) then
          begin
            PriorPoint := nil;
            ShowPriorPoint := False;
            for PointIndex := 0 to Line.Points.Count - 1 do
            begin
              APoint := Line.Points[PointIndex];
              // A dummy cross section segment can be used because
              // it won't be used when the data set orientation is
              // dso3D.
              if APoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer,
                EquateSegment(0,0,0,0), False) then
              begin

                if ShowPriorPoint then
                begin
                  StartLine;

                  AColor := GetPointColor(MaxValue, MinValue, APoint);
                  AssignColor(AColor);
                  glVertex3f(APoint.XPrime, APoint.YPrime, APoint.Z);
                end;
                ShowPriorPoint := True;
                PriorPoint := APoint;
              end
              else
              begin
                ShowPriorPoint := False;
                EndLine;
              end;
            end;
          end;
          EndLine;
        end;
      end;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

procedure TPathLineReader.SetLinesV6(const Value: TPathLinesV6);
begin
  FLinesV6.Assign(Value);
end;

procedure TPathLineReader.SetLinesV7(const Value: TPathLinesV7);
begin
  FLinesV7.Assign(Value);
end;

{ TCustomPathLinePoints }

constructor TCustomPathLinePoints.Create(ItemClass: TCollectionItemClass; PathLine: TCustomPathLine);
begin
  inherited Create(ItemClass);
  FPathLine := PathLine;
end;

{ TPathLinePointV6 }

procedure TPathLinePointV6.Assign(Source: TPersistent);
var
  SourcePoint: TPathLinePointV6;
begin
  if Source is TPathLinePointV6 then
  begin
    SourcePoint := TPathLinePointV6(Source);
    ParticleGroup := SourcePoint.ParticleGroup;
    TimePointIndex := SourcePoint.TimePointIndex;
    LineSegmentIndex := SourcePoint.LineSegmentIndex;

    ModpathVersion := SourcePoint.ModpathVersion;
    GridIndex := SourcePoint.GridIndex;
    LocalX := SourcePoint.LocalX;
    LocalY := SourcePoint.LocalY;
  end;
  inherited;

end;

function TPathLinePointV6.CheckLimits(Limits: TPathLineDisplayLimits): boolean;
begin
  result := inherited;
  if result then
  begin
    if Limits.ParticleGroupLimits.UseLimit then
    begin
      result := (Limits.ParticleGroupLimits.StartLimit <= ParticleGroup)
        and (ParticleGroup <= Limits.ParticleGroupLimits.EndLimit);
      if not result then Exit;
    end;
  end;
end;

{ TEndPointsV6 }

constructor TEndPointsV6.Create;
begin
  inherited Create(TEndPointV6)
end;

function TEndPointsV6.GetPoint(Index: integer): TEndPointV6;
begin
  result := inherited Items[Index] as TEndPointV6;
end;

//function TEndPointsV6.GetVersion: TModpathVersion;
//begin
//  result := mpv6;
//end;

{ TEndPointV6 }

procedure TEndPointV6.Assign(Source: TPersistent);
var
  SourcePoint: TEndPointV6;
begin
  if Source is TEndPointV6 then
  begin
    SourcePoint := TEndPointV6(Source);

    ParticleGroup := SourcePoint.ParticleGroup;
    InitialCellFace := SourcePoint.InitialCellFace;
    FinalCellFace := SourcePoint.FinalCellFace;
    ParticleLabel := SourcePoint.ParticleLabel;
  end;
  inherited;

end;

function TEndPointV6.CheckLimits(Limits: TEndPointDisplayLimits): boolean;
begin
  result := True;
  if Limits.EndLayerLimits.UseLimit then
  begin
    result := (Limits.EndLayerLimits.StartLimit <= EndLayer)
      and (EndLayer <= Limits.EndLayerLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndZoneLimits.UseLimit then
  begin
    result := (Limits.EndZoneLimits.StartLimit <= EndZoneCode)
      and (EndZoneCode <= Limits.EndZoneLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.TrackingTimeLimits.UseLimit then
  begin
    result := (Limits.TrackingTimeLimits.StartLimit <= TrackingTime)
      and (TrackingTime <= Limits.TrackingTimeLimits.EndLimit);
    if not result then Exit;
  end;

  if Limits.StartLayerLimits.UseLimit then
  begin
    result := (Limits.StartLayerLimits.StartLimit <= StartLayer)
      and (StartLayer <= Limits.StartLayerLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartZoneLimits.UseLimit then
  begin
    result := (Limits.StartZoneLimits.StartLimit <= StartZoneCode)
      and (StartZoneCode <= Limits.StartZoneLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.ReleaseTimeLimits.UseLimit then
  begin
    result := (Limits.ReleaseTimeLimits.StartLimit <= ReleaseTime)
      and (ReleaseTime <= Limits.ReleaseTimeLimits.StartLimit);
    if not result then Exit;
  end;

  if Limits.EndColumnLimits.UseLimit then
  begin
    result := (Limits.EndColumnLimits.StartLimit <= EndColumn)
      and (EndColumn <= Limits.EndColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndRowLimits.UseLimit then
  begin
    result := (Limits.EndRowLimits.StartLimit <= EndRow)
      and (EndRow <= Limits.EndRowLimits.EndLimit);
    if not result then Exit;
  end;

  if Limits.StartColumnLimits.UseLimit then
  begin
    result := (Limits.StartColumnLimits.StartLimit <= StartColumn)
      and (StartColumn <= Limits.StartColumnLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartRowLimits.UseLimit then
  begin
    result := (Limits.StartRowLimits.StartLimit <= StartRow)
      and (StartRow <= Limits.StartRowLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.ParticleGroupLimits.UseLimit then
  begin
    result := (Limits.ParticleGroupLimits.StartLimit <= ParticleGroup)
      and (ParticleGroup <= Limits.ParticleGroupLimits.EndLimit);
    if not result then Exit;
  end;
end;

{ TTimeSeriesPointV6 }

procedure TTimeSeriesPointV6.Assign(Source: TPersistent);
begin
  if Source is TTimeSeriesPointV6 then
  begin
    ParticleGroup := TTimeSeriesPointV6(Source).ParticleGroup;
  end;
  inherited;

end;

function TTimeSeriesPointV6.CheckLimits(
  Limits: TTimeSeriesDisplayLimits): boolean;
begin
  result := inherited;
  if result then
  begin
    if Limits.ParticleGroupLimits.UseLimit then
    begin
      result := (Limits.ParticleGroupLimits.StartLimit <= ParticleGroup)
        and (ParticleGroup <= Limits.ParticleGroupLimits.EndLimit);
      if not result then Exit;
    end;
  end;
end;

{ TTimeSeriesPointsV6 }

constructor TTimeSeriesPointsV6.Create;
begin
  inherited Create(TTimeSeriesPointV6);
end;

function TTimeSeriesPointsV6.GetPoint(Index: integer): TTimeSeriesPointV6;
begin
  result := inherited Items[Index] as TTimeSeriesPointV6
end;

{ TTimeSeriesV6 }

procedure TTimeSeriesV6.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeriesV6;
begin
  if Source is TTimeSeriesV6 then
  begin
    SourceSeries := TTimeSeriesV6(Source);
    Points := SourceSeries.Points;
  end;
  inherited;
end;

constructor TTimeSeriesV6.Create(Collection: TCollection);
begin
  inherited;
  FPoints:= TTimeSeriesPointsV6.Create;
end;

destructor TTimeSeriesV6.Destroy;
begin
  FPoints.Free;
  inherited;
end;

function TTimeSeriesV6.GetPoints: TCustomTimeSeriesPoints;
begin
  result := FPoints;
end;

function TTimeSeriesV6.GetTimes: TRealList;
var
  APoint: TTimeSeriesPoint;
  PointIndex: Integer;
begin
  if FTimes = nil then
  begin
    FTimes := TRealList.Create;
    for PointIndex := 0 to Points.Count - 1 do
    begin
      APoint := Points[PointIndex];
      FTimes.Add(APoint.TrackingTime);
    end;
    FTimes.Sorted := True;
  end;
  result := FTimes;
end;

procedure TTimeSeriesV6.SetPoints(const Value: TTimeSeriesPointsV6);
begin
  FPoints.Assign(Value);
end;

{ TTimeSeriesCollectionV6 }

constructor TTimeSeriesCollectionV6.Create;
begin
  inherited Create(TTimeSeriesV6);
end;

function TTimeSeriesCollectionV6.GetSeries(Index: integer): TTimeSeriesV6;
begin
  result := Items[Index] as TTimeSeriesV6;
end;

{ TCustomTimeSeriesCollection }

function TCustomTimeSeriesCollection.GetSeries(
  Index: integer): TCustomTimeSeries;
begin
  result := Items[Index] as TCustomTimeSeries;
end;

{ TCustomEndPoint }

//procedure TCustomEndPoint.Assign(Source: TPersistent);
//var
//  SourcePoint: TCustomEndPoint;
//begin
//  if Source is TCustomEndPoint then
//  begin
//    SourcePoint := TCustomEndPoint(Source);
//  end
//  else
//  begin
//    inherited;
//  end;
//end;

function TEndPoint.CheckLimits(Limits: TEndPointDisplayLimits): boolean;
begin
  result := True;
  if Limits.EndLayerLimits.UseLimit then
  begin
    result := (Limits.EndLayerLimits.StartLimit <= EndLayer)
      and (EndLayer <= Limits.EndLayerLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndZoneLimits.UseLimit then
  begin
    result := (Limits.EndZoneLimits.StartLimit <= EndZoneCode)
      and (EndZoneCode <= Limits.EndZoneLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.TrackingTimeLimits.UseLimit then
  begin
    result := (Limits.TrackingTimeLimits.StartLimit <= TrackingTime)
      and (TrackingTime <= Limits.TrackingTimeLimits.EndLimit);
    if not result then Exit;
  end;

  if Limits.StartLayerLimits.UseLimit then
  begin
    result := (Limits.StartLayerLimits.StartLimit <= StartLayer)
      and (StartLayer <= Limits.StartLayerLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartZoneLimits.UseLimit then
  begin
    result := (Limits.StartZoneLimits.StartLimit <= StartZoneCode)
      and (StartZoneCode <= Limits.StartZoneLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.ReleaseTimeLimits.UseLimit then
  begin
    result := (Limits.ReleaseTimeLimits.StartLimit <= ReleaseTime)
      and (ReleaseTime <= Limits.ReleaseTimeLimits.StartLimit);
    if not result then Exit;
  end;

  if Limits.EndColumnLimits.UseLimit then
  begin
    result := (Limits.EndColumnLimits.StartLimit <= EndColumn)
      and (EndColumn <= Limits.EndColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndRowLimits.UseLimit then
  begin
    result := (Limits.EndRowLimits.StartLimit <= EndRow)
      and (EndRow <= Limits.EndRowLimits.EndLimit);
    if not result then Exit;
  end;

  if Limits.StartColumnLimits.UseLimit then
  begin
    result := (Limits.StartColumnLimits.StartLimit <= StartColumn)
      and (StartColumn <= Limits.StartColumnLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartRowLimits.UseLimit then
  begin
    result := (Limits.StartRowLimits.StartLimit <= StartRow)
      and (StartRow <= Limits.StartRowLimits.StartLimit);
    if not result then Exit;
  end;

end;
{ TEndPointV7 }

procedure TEndPointV7.Assign(Source: TPersistent);
var
  EndPoint: TEndPointV7;
begin
  if Source is TEndPointV7 then
  begin
    EndPoint := TEndPointV7(Source);
    SequenceNumber := EndPoint.SequenceNumber;
    ParticleGroup := EndPoint.ParticleGroup;
    InitialCellFace := EndPoint.InitialCellFace;
    FinalCellFace := EndPoint.FinalCellFace;
    InitialCellNumber := EndPoint.InitialCellNumber;
    InitialLocalX := EndPoint.InitialLocalX;
    InitialLocalY := EndPoint.InitialLocalY;
    FinalCellNumber := EndPoint.FinalCellNumber;
    FinalLocalX := EndPoint.FinalLocalX;
    FinalLocalY := EndPoint.FinalLocalY;
  end;
  inherited;
end;

function TEndPointV7.CheckLimits(Limits: TEndPointDisplayLimits): boolean;
begin
  result := inherited;
  if result then
  begin
    if Limits.ParticleGroupLimits.UseLimit then
    begin
      result := (Limits.ParticleGroupLimits.StartLimit <= ParticleGroup)
        and (ParticleGroup <= Limits.ParticleGroupLimits.EndLimit);
      if not result then Exit;
    end;
  end;
end;

{ TEndPointsV7 }

constructor TEndPointsV7.Create;
begin
  inherited Create(TEndPointV7)
end;

function TEndPointsV7.GetPoint(Index: integer): TEndPointV7;
begin
  result := inherited Items[Index] as TEndPointV7;
end;

//function TEndPointsV7.GetVersion: TModpathVersion;
//begin
//  result := mpv7
//end;

{ TPathLinePointV7 }

procedure TPathLinePointV7.Assign(Source: TPersistent);
var  
  SourcePoint: TPathLinePointV7;
begin
  if Source is TPathLinePointV7 then
  begin
    SourcePoint := TPathLinePointV7(Source);
    SequenceNumber := SourcePoint.SequenceNumber;
    StressPeriod := SourcePoint.StressPeriod;
    CellNumber := SourcePoint.CellNumber;
  end;
  inherited;

end;

{ TPathLinePointsV7 }

constructor TPathLinePointsV7.Create(PathLine: TPathLineV7);
begin
  inherited Create(TPathLinePointV7, PathLine);
end;

{ TPathLineV7 }

constructor TPathLineV7.Create(Collection: TCollection);
begin
  inherited;
  FPoints := TPathLinePointsV7.Create(self);
end;

{ TPathLinesV7 }

constructor TPathLinesV7.Create;
begin
  inherited Create(TPathLineV7);
end;

{ TTimeSeriesPointV7 }

procedure TTimeSeriesPointV7.Assign(Source: TPersistent);
var
  SourcePoint: TTimeSeriesPointV7;
begin
  if Source is TTimeSeriesPointV7 then
  begin
    SourcePoint := TTimeSeriesPointV7(Source);
    SequenceNumber := SourcePoint.SequenceNumber;
    CellNumber := SourcePoint.CellNumber;
  end;
  inherited;

end;

{ TTimeSeriesPointsV7 }

constructor TTimeSeriesPointsV7.Create;
begin
  inherited Create(TTimeSeriesPointV7);
end;

function TTimeSeriesPointsV7.GetPoint(Index: integer): TTimeSeriesPointV7;
begin
  result := inherited Items[Index] as TTimeSeriesPointV7;
end;

{ TTimeSeriesV7 }

procedure TTimeSeriesV7.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeriesV7;
begin
  if Source is TTimeSeriesV7 then
  begin
    SourceSeries := TTimeSeriesV7(Source);
    Points := SourceSeries.Points;
  end;
end;

constructor TTimeSeriesV7.Create(Collection: TCollection);
begin
  inherited;
  FPoints:= TTimeSeriesPointsV7.Create;
end;

destructor TTimeSeriesV7.Destroy;
begin
  FPoints.Free;
  inherited;
end;

function TTimeSeriesV7.GetPoints: TCustomTimeSeriesPoints;
begin
  result := FPoints;
end;

function TTimeSeriesV7.GetTimes: TRealList;
var
  APoint: TTimeSeriesPoint;
  PointIndex: Integer;
begin
  if FTimes = nil then
  begin
    FTimes := TRealList.Create;
    for PointIndex := 0 to Points.Count - 1 do
    begin
      APoint := Points[PointIndex];
      FTimes.Add(APoint.TrackingTime);
    end;
    FTimes.Sorted := True;
  end;
  result := FTimes;
end;

procedure TTimeSeriesV7.SetPoints(const Value: TTimeSeriesPointsV7);
begin
  FPoints.Assign(Value);
end;

{ TTimeSeriesCollectionV7 }

constructor TTimeSeriesCollectionV7.Create;
begin
  inherited Create(TTimeSeriesV7);
end;

function TTimeSeriesCollectionV7.GetSeries(Index: integer): TTimeSeriesV7;
begin
  result := Items[Index] as TTimeSeriesV7;
end;

end.
