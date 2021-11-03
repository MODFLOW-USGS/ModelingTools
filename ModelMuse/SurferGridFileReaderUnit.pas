unit SurferGridFileReaderUnit;

// See http://www.geospatialdesigns.com/surfer6_format.htm,
// http://www.geospatialdesigns.com/surfer7_format.htm,
// and http://www.goldensoftware.com/index.php?option=com_fss&view=kb&prodid=2&kbartid=970&Itemid=182
// for the format of Surfer .grd files.

interface

uses SysUtils, Classes, RasterValuesAlongSegmentsUnit, FastGEO, System.IOUtils,
  System.Generics.Collections, IntListUnit;

type
  TSurferFileType = (sft6, sft7, sftAscii);

  TGridData6Header = record
    ID: array [0..3] of AnsiChar;
    nx: Smallint;
    ny: Smallint;
    Xlo: double;
    Xhi: double;
    Ylo: double;
    Yhi: double;
    Zlo: double;
    Zhi: double;
  end;

  TSurferPoint = record
    X: double;
    Y: double;
    Z: double;
  end;

  TSurfer6Grid = record
    Header: TGridData6Header;
    Points: array of TSurferPoint;
  end;

  EGrdReadError = class(Exception);

  TSurfer7Header = record
    ID: longint;
    Size: longint;
  end;

  TGrid7Header = record
    // number of rows in the grid
    nRow: longint;
    // number of columns in the grid
    nCol: longint;
    // X coordinate of the lower left corner of the grid
    xLL: double;
    // Y coordinate of the lower left corner of the grid
    yLL: double;
    // spacing between adjacent nodes in the X direction (between columns)
    xSize: double;
    // spacing between adjacent nodes in the Y direction (between rows)
    ySize: double;
    // minimum Z value within the grid
    zMin: double;
    // maximum Z value within the grid
    zMax: double;
    // Rotation is not currently used.
    Rotation: double;
    // nodes are blanked if greater or equal to this value
    BlankValue: double;
  end;

  TSurfer7Grid = record
    Header: TGrid7Header;
    Points: array of TSurferPoint;
  end;

  TSurferZArray = array of array of Double;

  TSurferRaster6 = class(TInterfacedObject, IRaster)
  private
    FHeader: TGridData6Header;
    FZ: TSurferZArray;
    FDeltaX: double;
    FDeltaY: double;
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: Integer): Double;
    function GetIgnore(XIndex, YIndex: Integer): Boolean;
    procedure SetCellSizes;
  public
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Z[XIndex, YIndex: Integer]: double read GetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

  TCustomSurferRaster7 = class abstract (TInterfacedObject, IRaster)
  private
    function GetLowerLeft: TPoint2D; virtual;
    function GetXCount: integer; virtual;
    function GetYCount: integer; virtual;
    function GetXSpacing: double;
    function GetYSpacing: double;
    function GetHeader: TGrid7Header; virtual;
    procedure SetHeader(const Value: TGrid7Header); virtual;
    function GetZ(XIndex, YIndex: Integer): Double; virtual; abstract;
//    function GetIgnore(XIndex, YIndex: Integer): Boolean; virtual; abstract;
    procedure SetZ(XIndex, YIndex: integer; const Value: double);
      virtual; abstract;
  protected
    FHeader: TGrid7Header;
    function GetIgnore(XIndex, YIndex: integer): Boolean; virtual;
  public
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Header: TGrid7Header read GetHeader write SetHeader;
    property Z[XIndex, YIndex: Integer]: double read GetZ write SetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

  TSurferRaster7 = class(TCustomSurferRaster7)
  private
//    FHeader: TGrid7Header;
    FZ: TSurferZArray;
//    function GetLowerLeft: TPoint2D;
    function GetZ(XIndex, YIndex: Integer): Double; override;
    procedure SetHeader(const Value: TGrid7Header); override;
    procedure SetZ(XIndex, YIndex: Integer; const Value: double); override;
  public
    procedure SaveToFile(FileName: string);
  end;

  TSurferRaster7File = class(TCustomSurferRaster7, IRasterFile)
  const
    BufferedRowCount = 101;
  private
    FFileStream: TFileStream;
    FZPosition: Int64;
    FFileName: string;
    FCachedZ: array of array of double;
    FCachedRows: TIntegerList;
    FCacheQueue: TQueue<Integer>;
    FModified: Boolean;
    function GetZ(XIndex, YIndex: Integer): Double; override;
    procedure SetZ(XIndex, YIndex: Integer; const Value: double); override;
    function GetOffset(XIndex, YIndex: Integer): Int64;
    function GetFileName: string;
    procedure UpdateCachedRows(const YIndex: Integer);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    property FileName: string read GetFileName;
    procedure Flush;
  end;

  TRowPosition = class(TObject)
  private
    FRow: Integer;
    FListPosition: Integer;
    FModified: Boolean;
  public
    property Row: Integer read FRow write FRow;
    property ListPosition: Integer read FListPosition;
    property Modified: Boolean read FModified write FModified;
    constructor Create;
  end;

  TRowPositionQueue = class(TObject)
  private
    FList: TList;
    FCurrentPosition: integer;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read FCount;
    procedure Enqueue(Item: TRowPosition);
    procedure MoveToEnd(Item: TRowPosition);
    function Dequeue: TRowPosition;
    function Peek: TRowPosition;
  end;

  TSurferRaster7File2 = class(TCustomSurferRaster7, IRasterFile)
//  const
//    BufferedRowCount = 300;
  private
    FFileStream: TFileStream;
    FZPosition: Int64;
    FFileName: string;
    FCachedZ: array of array of double;
//    FModified: Boolean;
    FCachedQueue2: TRowPositionQueue;
    FCachedRows2: TObjectList<TRowPosition>;
    FBufferedRowCount: integer;
    FHeaderPosition: Int64;
    function GetZ(XIndex, YIndex: Integer): Double; override;
    procedure SetZ(XIndex, YIndex: Integer; const Value: double); override;
    function GetOffset(XIndex, YIndex: Int64): Int64;
    function GetFileName: string;
    procedure UpdateCachedRows(const YIndex: Integer);
    procedure SetBufferedRowCount(const Value: integer);
    procedure SaveModifiedRows;
    function GetPoint(XIndex, YIndex: Integer): TSurferPoint;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    property FileName: string read GetFileName;
    procedure Flush;
    property IgnoreValue: Double read FHeader.BlankValue;
    property BufferedRowCount: integer read FBufferedRowCount
      write SetBufferedRowCount;
    property Points[XIndex, YIndex: Integer]: TSurferPoint read GetPoint;
  end;

  TSurferRaster7File2ObjectList = TObjectList<TSurferRaster7File2>;

  // @name has data about only a block within an existing Surfer Grid version 7 file
  TSurferRaster7FileBlock = class(TCustomSurferRaster7, IRasterFlush)
  private
    FXStart: Integer;
    FYStart: Integer;
    FEdgeCount: Integer;
    FHeaderPosition: Int64;
    FZPosition: Int64;
    ZValues: Array of array of double;
    FRasterStream: TStream;
    FModified: Boolean;
    procedure ReadZValues;
    function GetLowerLeft: TPoint2D; override;
    function GetXCount: integer; override;
    function GetYCount: integer; override;
    function GetHeader: TGrid7Header; override;
    procedure SetHeader(const Value: TGrid7Header); override;
    function GetZ(XIndex, YIndex: Integer): Double; override;
    procedure SetZ(XIndex, YIndex: integer; const Value: double);
      override;
  public
    constructor Create(RasterStream: TStream; XStart, YStart, EdgeCount: Integer);
    Destructor Destroy; override;
    procedure Flush;
  end;



function SurferFileType(const FileName: string): TSurferFileType;

procedure ReadSurfer6GrdFile(FileName: string; out SurferGrid: TSurfer6Grid); overload;
procedure ReadSurfer7GrdFile(FileName: string; out Surfer7Grid: TSurfer7Grid); overload;
procedure ReadSurferAsciiFile(FileName: string; out SurferGrid: TSurfer6Grid); overload;

procedure ReadSurfer6GrdFile(FileName: string; SurferRaster: TSurferRaster6); overload;
procedure ReadSurfer7GrdFile(FileName: string; SurferRaster: TSurferRaster7); overload;
procedure ReadSurferAsciiFile(FileName: string; SurferRaster: TSurferRaster6); overload;

implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  System.Math;

resourcestring
  StrSIsNotASurferG = '%s is not a Surfer grid file.';
  StrSIsNotASurfer6 = '%s is not a Surfer 6 grid file.';
  StrErrorReadingS = 'Error reading %s.';
  StrSIsNotASurferA = '%s is not a Surfer ASCII grid file.';
  StrSIsNotASurfer7 = '%s is not a Surfer 7 grid file.';
  StrSDoesNotExist = '%s does not exist.';

function SurferFileType(const FileName: string): TSurferFileType;
var
  GrdFile: TFileStream;
  ID: array [0..3] of AnsiChar;
begin
  result := sft6;
  if not FileExists(FileName) then
  begin
    raise EGrdReadError.Create(Format(StrSDoesNotExist, [FileName]));
  end;
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(ID[0], SizeOf(ID));
    if ID = 'DSBB' then
    begin
      result := sft6;
    end
    else if ID = 'DSRB' then
    begin
      result := sft7;
    end
    else if ID = 'DSAA' then
    begin
      result := sftAscii;
    end
    else
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferG, [FileName]));
    end;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurfer6GrdFile(FileName: string; out SurferGrid: TSurfer6Grid);
var
  GrdFile: TFileStream;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  XLength: Double;
  X: Array of Double;
  Y: Array of Double;
  YLength: Double;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(SurferGrid.Header, SizeOf(SurferGrid.Header));
    if SurferGrid.Header.ID <> 'DSBB' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer6, [FileName]));
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(ZValues, SurferGrid.Header.nx * SurferGrid.Header.ny);
    GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(single));

    XLength := SurferGrid.Header.Xhi - SurferGrid.Header.Xlo;
    SetLength(X, SurferGrid.Header.nx);
    for ColIndex := 0 to SurferGrid.Header.nx - 1 do
    begin
      X[ColIndex] := SurferGrid.Header.Xlo
        + XLength*ColIndex/(SurferGrid.Header.nx - 1);
    end;

    YLength := SurferGrid.Header.Yhi - SurferGrid.Header.Ylo;
    SetLength(Y, SurferGrid.Header.ny);
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      Y[RowIndex] := SurferGrid.Header.Ylo
        + YLength*RowIndex/(SurferGrid.Header.ny - 1);
    end;

    SetLength(SurferGrid.Points, SurferGrid.Header.nx * SurferGrid.Header.ny);
    Index := 0;
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      for ColIndex := 0 to SurferGrid.Header.nx - 1 do
      begin
        SurferGrid.Points[Index].X := X[ColIndex];
        SurferGrid.Points[Index].Y := Y[RowIndex];
        SurferGrid.Points[Index].Z := ZValues[Index];
        Inc(Index);
      end;
    end;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurfer6GrdFile(FileName: string; SurferRaster: TSurferRaster6);
var
  GrdFile: TFileStream;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(SurferRaster.FHeader, SizeOf(SurferRaster.FHeader));
    if SurferRaster.FHeader.ID <> 'DSBB' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer6, [FileName]));
    end;
    if (SurferRaster.FHeader.nx <= 1) or (SurferRaster.FHeader.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(SurferRaster.FZ, SurferRaster.FHeader.ny, SurferRaster.FHeader.nx);
    SetLength(ZValues, SurferRaster.FHeader.ny* SurferRaster.FHeader.nx);
    GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(single));

    Index := 0;
    for RowIndex := 0 to SurferRaster.FHeader.ny - 1 do
    begin
      for ColIndex := 0 to SurferRaster.FHeader.nx - 1 do
      begin
        SurferRaster.FZ[RowIndex, ColIndex] := ZValues[Index];
        Inc(Index);
      end;
    end;
    SurferRaster.SetCellSizes;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurferAsciiFile(FileName: string; out SurferGrid: TSurfer6Grid);
var
  GrdFile: TextFile;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  XLength: Double;
  X: Array of Double;
  Y: Array of Double;
  YLength: Double;
begin
  Assert(FileExists(FileName));
  AssignFile(GrdFile, FileName);
  Reset(GrdFile);
  try
    for Index := 0 to 3 do
    begin
      Read(GrdFile, SurferGrid.Header.ID[Index]);
    end;
    ReadLn(GrdFile);
    ReadLn(GrdFile, SurferGrid.Header.nx, SurferGrid.Header.ny);
    ReadLn(GrdFile, SurferGrid.Header.Xlo, SurferGrid.Header.Xhi);
    ReadLn(GrdFile, SurferGrid.Header.Ylo, SurferGrid.Header.Yhi);
    ReadLn(GrdFile, SurferGrid.Header.Zlo, SurferGrid.Header.Zhi);
    if SurferGrid.Header.ID <> 'DSAA' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferA, [FileName]));
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(ZValues, SurferGrid.Header.nx * SurferGrid.Header.ny);
    for Index := 0 to SurferGrid.Header.nx*SurferGrid.Header.ny - 1 do
    begin
      Read(GrdFile, ZValues[Index]);
    end;

    XLength := SurferGrid.Header.Xhi - SurferGrid.Header.Xlo;
    SetLength(X, SurferGrid.Header.nx);
    for ColIndex := 0 to SurferGrid.Header.nx - 1 do
    begin
      X[ColIndex] := SurferGrid.Header.Xlo
        + XLength*ColIndex/(SurferGrid.Header.nx - 1);
    end;

    YLength := SurferGrid.Header.Yhi - SurferGrid.Header.Ylo;
    SetLength(Y, SurferGrid.Header.ny);
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      Y[RowIndex] := SurferGrid.Header.Ylo
        + YLength*RowIndex/(SurferGrid.Header.ny - 1);
    end;

    SetLength(SurferGrid.Points, SurferGrid.Header.nx * SurferGrid.Header.ny);
    Index := 0;
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      for ColIndex := 0 to SurferGrid.Header.nx - 1 do
      begin
        SurferGrid.Points[Index].X := X[ColIndex];
        SurferGrid.Points[Index].Y := Y[RowIndex];
        SurferGrid.Points[Index].Z := ZValues[Index];
        Inc(Index);
      end;
    end;
  finally
    CloseFile(GrdFile);
  end;
end;

procedure ReadSurferAsciiFile(FileName: string; SurferRaster: TSurferRaster6);
var
  GrdFile: TextFile;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Assert(FileExists(FileName));
  AssignFile(GrdFile, FileName);
  Reset(GrdFile);
  try
    for Index := 0 to 3 do
    begin
      Read(GrdFile, SurferRaster.FHeader.ID[Index]);
    end;
    ReadLn(GrdFile);
    ReadLn(GrdFile, SurferRaster.FHeader.nx, SurferRaster.FHeader.ny);
    ReadLn(GrdFile, SurferRaster.FHeader.Xlo, SurferRaster.FHeader.Xhi);
    ReadLn(GrdFile, SurferRaster.FHeader.Ylo, SurferRaster.FHeader.Yhi);
    ReadLn(GrdFile, SurferRaster.FHeader.Zlo, SurferRaster.FHeader.Zhi);
    if SurferRaster.FHeader.ID <> 'DSAA' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferA, [FileName]));
    end;
    if (SurferRaster.FHeader.nx <= 1) or (SurferRaster.FHeader.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(SurferRaster.FZ, SurferRaster.FHeader.ny, SurferRaster.FHeader.nx);
    for RowIndex := 0 to SurferRaster.FHeader.ny - 1 do
    begin
      for ColIndex := 0 to SurferRaster.FHeader.nx - 1 do
      begin
        Read(GrdFile, SurferRaster.FZ[RowIndex, ColIndex]);
      end;
    end;
    SurferRaster.SetCellSizes;
  finally
    CloseFile(GrdFile);
  end;
end;

procedure ReadSurfer7GrdFile(FileName: string; out Surfer7Grid: TSurfer7Grid);
var
  GrdFile: TFileStream;
  Header: TSurfer7Header;
  ZValues: array of double;
  X: array of double;
  Y: array of double;
  ColIndex: Integer;
  RowIndex: Integer;
  Index: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(Header, SizeOf(Header));
    if Header.ID <> $42525344 then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
    end;
    GrdFile.Position := GrdFile.Position + Header.Size;
    While GrdFile.Position < GrdFile.Size do
    begin
      GrdFile.Read(Header, SizeOf(Header));
      if Header.ID = $44495247 then
      begin
        // Grid section
        GrdFile.Read(Surfer7Grid.Header, SizeOf(Surfer7Grid.Header));

        // Data section must follow immediately after grid section;
        GrdFile.Read(Header, SizeOf(Header));
        Assert(Header.ID = $41544144);
        SetLength(ZValues, Surfer7Grid.Header.nRow * Surfer7Grid.Header.nCol);
        GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(double));

        SetLength(X, Surfer7Grid.Header.nCol);
        for ColIndex := 0 to Surfer7Grid.Header.nCol - 1 do
        begin
          X[ColIndex] := Surfer7Grid.Header.xLL
            + ColIndex * Surfer7Grid.Header.xSize
        end;

        SetLength(Y, Surfer7Grid.Header.nRow);
        for RowIndex := 0 to Surfer7Grid.Header.nRow - 1 do
        begin
          Y[RowIndex] := Surfer7Grid.Header.yLL
            + RowIndex * Surfer7Grid.Header.ySize
        end;

        SetLength(Surfer7Grid.Points,
          Surfer7Grid.Header.nRow * Surfer7Grid.Header.nCol);
        Index := 0;
        for RowIndex := 0 to Surfer7Grid.Header.nRow - 1 do
        begin
          for ColIndex := 0 to Surfer7Grid.Header.nCol - 1 do
          begin
            Surfer7Grid.Points[Index].X := X[ColIndex];
            Surfer7Grid.Points[Index].Y := Y[RowIndex];
            Surfer7Grid.Points[Index].Z := ZValues[Index];
            Inc(Index);
          end;
        end;
        Exit;
      end
      else if Header.ID = $41544144 then
      begin
        // data section
        Assert(False);
      end
      else if Header.ID = $49544c46 then
      begin
        // Fault Info section
        GrdFile.Position := GrdFile.Position + Header.Size;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    GrdFile.Free;
  end;
  Assert(False);
end;

procedure ReadSurfer7GrdFile(FileName: string; SurferRaster: TSurferRaster7);
var
  GrdFile: TFileStream;
  Header: TSurfer7Header;
  ZValues: array of double;
  ColIndex: Integer;
  RowIndex: Integer;
  Index: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(Header, SizeOf(Header));
    if Header.ID <> $42525344 then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
    end;
    GrdFile.Position := GrdFile.Position + Header.Size;
    While GrdFile.Position < GrdFile.Size do
    begin
      GrdFile.Read(Header, SizeOf(Header));
      if Header.ID = $44495247 then
      begin
        // Grid section
        GrdFile.Read(SurferRaster.FHeader, SizeOf(SurferRaster.FHeader));

        // Data section must follow immediately after grid section;
        GrdFile.Read(Header, SizeOf(Header));
        Assert(Header.ID = $41544144);
        SetLength(ZValues, SurferRaster.FHeader.nRow * SurferRaster.FHeader.nCol);
        GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(double));


        SetLength(SurferRaster.FZ, SurferRaster.FHeader.nRow,
          SurferRaster.FHeader.nCol);
        Index := 0;
        for RowIndex := 0 to SurferRaster.FHeader.nRow - 1 do
        begin
          for ColIndex := 0 to SurferRaster.FHeader.nCol - 1 do
          begin
            SurferRaster.FZ[RowIndex, ColIndex] := ZValues[Index];
            Inc(Index);
          end;
        end;
        Exit;
      end
      else if Header.ID = $41544144 then
      begin
        // data section
        Assert(False);
      end
      else if Header.ID = $49544c46 then
      begin
        // Fault Info section
        GrdFile.Position := GrdFile.Position + Header.Size;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    GrdFile.Free;
  end;
  Assert(False);
end;

{ TSurferRaster6 }

function TSurferRaster6.GetIgnore(XIndex, YIndex: Integer): Boolean;
const
  BlankValue = 1.70141e+38;
  Epsilon = 1e-8;
begin
  result := Abs(Z[XIndex, YIndex] - BlankValue)/BlankValue < Epsilon;
end;

function TSurferRaster6.GetLowerLeft: TPoint2D;
begin
  result.x := FHeader.Xlo - FDeltaX/2;
  result.y := FHeader.Ylo - FDeltaY/2;
end;

function TSurferRaster6.GetXCount: integer;
begin
  result := FHeader.nx;
end;

function TSurferRaster6.GetXSpacing: Double;
begin
  Result := FDeltaX;
end;

function TSurferRaster6.GetYCount: integer;
begin
  result := FHeader.ny;
end;

function TSurferRaster6.GetYSpacing: Double;
begin
  Result := FDeltaY;
end;

function TSurferRaster6.GetZ(XIndex, YIndex: Integer): Double;
begin
  result := FZ[YIndex, XIndex];
end;

procedure TSurferRaster6.SetCellSizes;
begin
  FDeltaX := (FHeader.Xhi - FHeader.Xlo)/(FHeader.nx-1);
  FDeltaY := (FHeader.Yhi - FHeader.Ylo)/(FHeader.nY-1);
end;

{ TSurferRaster7 }

function TCustomSurferRaster7.GetLowerLeft: TPoint2D;
begin
  result.x := FHeader.xLL - FHeader.xSize/2;
  result.y := FHeader.yLL - FHeader.ySize/2;
end;

function TSurferRaster7.GetZ(XIndex, YIndex: Integer): Double;
begin
  result := FZ[YIndex,XIndex];
end;

procedure TSurferRaster7.SetHeader(const Value: TGrid7Header);
begin
  inherited;
  SetLength(FZ, FHeader.nRow, FHeader.nCol);
end;

procedure TSurferRaster7.SetZ(XIndex, YIndex: Integer; const Value: double);
begin
  FZ[YIndex,XIndex] := Value;
end;

procedure TSurferRaster7.SaveToFile(FileName: string);
var
  FileStream: TFileStream;
  AHeader: TSurfer7Header;
  Version: integer;
  RowIndex: Integer;
begin
  FileStream := TFile.Create(FileName);
  try
    Version := 1;
    AHeader.ID := $42525344;
    AHeader.Size := SizeOf(Version);
    FileStream.Write(AHeader, SizeOf(AHeader));
    FileStream.Write(Version, SizeOf(Version));

    AHeader.ID := $44495247;
    AHeader.Size := SizeOf(FHeader);
    FileStream.Write(AHeader, SizeOf(AHeader));
    FileStream.Write(FHeader, SizeOf(FHeader));

    AHeader.ID := $41544144;
    AHeader.Size := SizeOf(double) * FHeader.nRow * FHeader.nCol;
    FileStream.Write(AHeader, SizeOf(AHeader));

    for RowIndex := 0 to FHeader.nRow - 1 do
    begin
      FileStream.Write(FZ[RowIndex, 0], SizeOf(double)*FHeader.nCol);
    end;
  finally
    FileStream.Free;
  end;
end;

function TCustomSurferRaster7.GetXCount: integer;
begin
  result := FHeader.nCol;
end;

function TCustomSurferRaster7.GetYCount: integer;
begin
  result := FHeader.nRow;
end;

function TCustomSurferRaster7.GetXSpacing: double;
begin
  result := FHeader.xSize;
end;

function TCustomSurferRaster7.GetYSpacing: double;
begin
  result := FHeader.ySize;
end;

procedure TCustomSurferRaster7.SetHeader(const Value: TGrid7Header);
begin
  FHeader := Value;
end;

{ TSurferRaster7File }

constructor TSurferRaster7File.Create(FileName: string);
var
  FileHeader: TSurfer7Header;
begin
  Assert(FileExists(FileName));
  FFileName := FileName;
  FFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
  FFileStream.Read(FileHeader, SizeOf(FileHeader));
  if FileHeader.ID <> $42525344 then
  begin
    raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
  end;
  FFileStream.Position := FFileStream.Position + FileHeader.Size;
  While FFileStream.Position < FFileStream.Size do
  begin
    FFileStream.Read(FileHeader, SizeOf(FileHeader));
    if FileHeader.ID = $44495247 then
    begin
      // Grid section
      FFileStream.Read(FHeader, SizeOf(FHeader));

      // Data section must follow immediately after grid section;
      FFileStream.Read(FileHeader, SizeOf(FileHeader));
      Assert(FileHeader.ID = $41544144);
      FZPosition := FFileStream.Position;
      break;
    end;
  end;
  SetLength(FCachedZ, FHeader.nRow);
  FCachedRows := TIntegerList.Create;
  FCachedRows.Sorted := True;
  FCacheQueue := TQueue<Integer>.Create;
end;

destructor TSurferRaster7File.Destroy;
begin
  Flush;

  FCacheQueue.Free;
  FFileStream.Free;
  FCachedRows.Free;
  inherited;
end;

procedure TSurferRaster7File.Flush;
var
  Position: Int64;
  OldRow: Integer;
  RowIndex: Integer;
begin
  if FModified then
  begin
    if FCachedRows <> nil then
    begin
      for RowIndex := 0 to FCachedRows.Count -1 do
      begin
        OldRow := FCachedRows[RowIndex];
        Position := GetOffset(0, OldRow);
        FFileStream.Seek(Position, soBeginning);
        FFileStream.WriteBuffer(FCachedZ[OldRow, 0], XCount * SizeOf(Double));
      end;
    end;
    FModified := False;
  end;
//  while FCacheQueue.Count > 0 do
//  begin
//    OldRow := FCacheQueue.Dequeue;
//    Position := GetOffset(0, OldRow);
//    FFileStream.Seek(Position, soBeginning);
//    FFileStream.WriteBuffer(FCachedZ[OldRow, 0], XCount * SizeOf(Double));
//    SetLength(FCachedZ[OldRow, 0], 0);
//  end;
//  FCachedRows.Clear;
end;

procedure TSurferRaster7File.UpdateCachedRows(const YIndex: Integer);
var
  OldRow: Integer;
  Position: Int64;
  CachedRow: Integer;
begin
  CachedRow := FCachedRows.IndexOf(YIndex);
  if CachedRow < 0 then
  begin
    if FCachedRows.Count > BufferedRowCount then
    begin
      OldRow := FCacheQueue.Dequeue;
      Position := GetOffset(0, OldRow);
      FFileStream.Seek(Position, soBeginning);
      FFileStream.WriteBuffer(FCachedZ[OldRow, 0], XCount * SizeOf(Double));
      SetLength(FCachedZ[OldRow], 0);
      FCachedRows.Delete(FCachedRows.IndexOf(OldRow));
    end;
    SetLength(FCachedZ[YIndex], XCount);
    FCachedRows.Add(YIndex);
    FCacheQueue.Enqueue(YIndex);
    Position := GetOffset(0, YIndex);
    FFileStream.Seek(Position, soBeginning);
    FFileStream.ReadBuffer(FCachedZ[YIndex, 0], XCount * SizeOf(Double));
//    CachedRow := YIndex;
  end;
end;

function TSurferRaster7File.GetFileName: string;
begin
  result := FFileName;
end;

//function TSurferRaster7File.GetIgnore(XIndex, YIndex: Integer): Boolean;
//begin
//
//end;

function TSurferRaster7File.GetOffset(XIndex, YIndex: Integer): Int64;
begin
  result := ((YIndex * XCount) + XIndex)* SizeOf(Double) + FZPosition;
end;

function TSurferRaster7File.GetZ(XIndex, YIndex: Integer): Double;
begin
  UpdateCachedRows(YIndex);
  result := FCachedZ[YIndex,XIndex]
end;

procedure TSurferRaster7File.SetZ(XIndex, YIndex: Integer; const Value: double);
//var
//  Position: Int64;
begin
  FModified := True;
  UpdateCachedRows(YIndex);
  FCachedZ[YIndex,XIndex] := Value;
//  Position := GetOffset(XIndex, YIndex);
//  FFileStream.Seek(Position, soBeginning);
//  FFileStream.Write(Value, SizeOf(double));
end;

function TCustomSurferRaster7.GetHeader: TGrid7Header;
begin
  result := FHeader;
end;

function TCustomSurferRaster7.GetIgnore(XIndex, YIndex: integer): Boolean;
begin
  result := Z[XIndex, YIndex] >= FHeader.BlankValue;
end;

{ TSurferRaster7File2 }

constructor TSurferRaster7File2.Create(FileName: string);
var
  Header: TSurfer7Header;
  RowIndex: Integer;
  ARowObj: TRowPosition;
begin
  FBufferedRowCount := 800;
  Assert(FileExists(FileName));
  FFileName := FileName;
  FFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
  FFileStream.Read(Header, SizeOf(Header));
  if Header.ID <> $42525344 then
  begin
    raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
  end;
  FFileStream.Position := FFileStream.Position + Header.Size;
  While FFileStream.Position < FFileStream.Size do
  begin
    FFileStream.Read(Header, SizeOf(Header));
    if Header.ID = $44495247 then
    begin
      // Grid section
      FHeaderPosition := FFileStream.Position;
      FFileStream.Read(FHeader, SizeOf(FHeader));

      // Data section must follow immediately after grid section;
      FFileStream.Read(Header, SizeOf(Header));
      Assert(Header.ID = $41544144);
      FZPosition := FFileStream.Position;
      break;
    end;
  end;
  SetLength(FCachedZ, FHeader.nRow);
//  FCachedRows := TIntegerList.Create;
//  FCachedRows.Sorted := True;
//  FCacheQueue := TQueue<Integer>.Create;
  FCachedQueue2:= TRowPositionQueue.Create;
  FCachedRows2 := TObjectList<TRowPosition>.Create;
  FCachedRows2.Capacity := FHeader.nRow;
  for RowIndex := 0 to FHeader.nRow - 1 do
  begin
    ARowObj := TRowPosition.Create;
    ARowObj.Row := RowIndex;
    FCachedRows2.Add(ARowObj);
  end;
end;

destructor TSurferRaster7File2.Destroy;
begin
  Flush;


  FCachedRows2.Free;
  FCachedQueue2.Free;
//  FCacheQueue.Free;
  FFileStream.Free;
//  FCachedRows.Free;
  inherited;
end;

procedure TSurferRaster7File2.Flush;
var
  Position: Int64;
//  OldRow: Integer;
  RowIndex: Integer;
  OldRow: TRowPosition;
begin

//  if FModified then
//  begin
    for RowIndex := 0 to FCachedRows2.Count -1 do
    begin
      OldRow := FCachedRows2[RowIndex] as TRowPosition;
      if (OldRow.ListPosition >= 0) and OldRow.Modified then
      begin
        Position := GetOffset(0, OldRow.Row);
        FFileStream.Seek(Position, soBeginning);
        FFileStream.WriteBuffer(FCachedZ[OldRow.Row, 0], XCount * SizeOf(Double));
      end;
    end;
//    FModified := False;
//  end;
  FFileStream.Seek(FHeaderPosition, soBeginning);
  FFileStream.WriteBuffer(FHeader, SizeOf(FHeader));
end;

function TSurferRaster7File2.GetFileName: string;
begin
  result := FFileName;
end;

function TSurferRaster7File2.GetOffset(XIndex, YIndex: Int64): Int64;
begin
  result := ((YIndex * XCount) + XIndex)* SizeOf(Double) + FZPosition;
end;

function TSurferRaster7File2.GetPoint(XIndex, YIndex: Integer): TSurferPoint;
begin
  Result.X := Header.xLL + XIndex * Header.xSize;
  Result.Y := Header.YLL + YIndex * Header.ySize;
  Result.Z := Z[XIndex, YIndex];
end;

function TSurferRaster7File2.GetZ(XIndex, YIndex: Integer): Double;
begin
  UpdateCachedRows(YIndex);
  result := FCachedZ[YIndex,XIndex]
end;

procedure TSurferRaster7File2.SetBufferedRowCount(const Value: integer);
begin
  FBufferedRowCount := Value;
  SaveModifiedRows;
end;

procedure TSurferRaster7File2.SetZ(XIndex, YIndex: Integer;
  const Value: double);
begin
//  FModified := True;
  UpdateCachedRows(YIndex);
  FCachedZ[YIndex,XIndex] := Value;
  FCachedRows2[YIndex].Modified := True;
end;

procedure TSurferRaster7File2.SaveModifiedRows;
var
  Position: Int64;
//  CachedRow: TRowPosition;
  OldRow: TRowPosition;
begin
  while FCachedQueue2.Count > BufferedRowCount do
  begin
    OldRow := FCachedQueue2.Peek;
    if OldRow.Modified then
    begin
      Position := GetOffset(0, OldRow.Row);
      Assert(Position >= 0);
      Assert(FFileStream.Size >= Position + XCount * SizeOf(Double));
      Assert(Length(FCachedZ[OldRow.Row]) = XCount);
      FFileStream.Seek(Position, soBeginning);
      FFileStream.WriteBuffer(FCachedZ[OldRow.Row, 0], XCount * SizeOf(Double));
      OldRow.Modified := False;
    end;
    SetLength(FCachedZ[OldRow.Row], 0);
    FCachedQueue2.Dequeue;
    Assert(OldRow.ListPosition < 0);
//      FCachedRows.Delete(FCachedRows.IndexOf(OldRow));
  end;
end;

procedure TSurferRaster7File2.UpdateCachedRows(const YIndex: Integer);
var
//  OldRow: Integer;
  Position: Int64;
  CachedRow: TRowPosition;
//  OldRow: TRowPosition;
//  CachedRow: Integer;
begin
  Assert(YIndex >= 0);
  Assert(YIndex < YCount);

  CachedRow := FCachedRows2[YIndex];
  if CachedRow.ListPosition < 0 then
  begin
    FCachedQueue2.Enqueue(CachedRow);
    SaveModifiedRows;
//    if FCachedQueue2.Count > BufferedRowCount then
//    begin
//      OldRow := FCachedQueue2.Peek;
//      if OldRow.Modified then
//      begin
//        Position := GetOffset(0, OldRow.Row);
//        Assert(Position >= 0);
//        Assert(FFileStream.Size >= Position + XCount * SizeOf(Double));
//        Assert(Length(FCachedZ[OldRow.Row]) = XCount);
//        FFileStream.Seek(Position, soBeginning);
//        FFileStream.WriteBuffer(FCachedZ[OldRow.Row, 0], XCount * SizeOf(Double));
//        OldRow.Modified := False;
//      end;
//      SetLength(FCachedZ[OldRow.Row], 0);
//      FCachedQueue2.Dequeue;
//      Assert(OldRow.ListPosition < 0);
////      FCachedRows.Delete(FCachedRows.IndexOf(OldRow));
//    end;
    SetLength(FCachedZ[YIndex], XCount);
//    FCachedRows.Add(YIndex);
    Assert(CachedRow.ListPosition >= 0);
    Position := GetOffset(0, YIndex);
    FFileStream.Seek(Position, soBeginning);
    FFileStream.ReadBuffer(FCachedZ[YIndex, 0], XCount * SizeOf(Double));
  end
  else if (FCachedQueue2.FList.Count - CachedRow.ListPosition)
    > (BufferedRowCount div 2) then
  begin
    FCachedQueue2.MoveToEnd(CachedRow);
  end;
end;

{ TRowPositionQueue }

constructor TRowPositionQueue.Create;
begin
  inherited;
  FList := TList.Create;
end;

function TRowPositionQueue.Dequeue: TRowPosition;
var
  index: Integer;
  AnItem: TRowPosition;
begin
  result := nil;
  if FCurrentPosition < FList.Count then
  begin
    repeat
      Result := FList[FCurrentPosition];
      Inc(FCurrentPosition)
    until (FCurrentPosition >= FList.Count) or (result <> nil);
  end;
  if result <> nil then
  begin
    Dec(FCount);
    FList[Pred(FCurrentPosition)] := nil;
    result.FListPosition := -1;
  end;
  if Count <= FList.Count div 2 then
  begin
    FList.Pack;
    FCurrentPosition := 0;
    for index := 0 to FList.Count - 1 do
    begin
      AnItem := FList[index];
      AnItem.FListPosition := index;
    end;
  end;
end;

destructor TRowPositionQueue.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TRowPositionQueue.Enqueue(Item: TRowPosition);
begin
  Item.FListPosition := FList.Add(Item);
  Inc(FCount);
end;

procedure TRowPositionQueue.MoveToEnd(Item: TRowPosition);
begin
  Assert(FList[Item.ListPosition] = Item);
  FList[Item.ListPosition] := nil;
  Item.FListPosition := FList.Add(Item);
end;

function TRowPositionQueue.Peek: TRowPosition;
begin
  result := nil;
  if FCurrentPosition < FList.Count then
  begin
    repeat
      Result := FList[FCurrentPosition];
      Inc(FCurrentPosition)
    until (FCurrentPosition >= FList.Count) or (result <> nil);
    Dec(FCurrentPosition);
  end;
end;

{ TRowPosition }

constructor TRowPosition.Create;
begin
  FListPosition := -1;
  FModified := False;
end;

{ TSurferRaster7FileBlock }

constructor TSurferRaster7FileBlock.Create(RasterStream: TStream; XStart,
  YStart, EdgeCount: Integer);
var
  Header: TSurfer7Header;
begin
  Assert(RasterStream <> nil);
  FXStart := XStart;
  FYStart := YStart;
  FEdgeCount := EdgeCount;
  FRasterStream := RasterStream;

  RasterStream.Position := 0;
  RasterStream.Read(Header, SizeOf(Header));
  if Header.ID <> $42525344 then
  begin
    raise EGrdReadError.Create('Error reading Surfer grid version 7 file.');
  end;
  RasterStream.Position := RasterStream.Position + Header.Size;
  While RasterStream.Position < RasterStream.Size do
  begin
    RasterStream.Read(Header, SizeOf(Header));
    if Header.ID = $44495247 then
    begin
      // Grid section
      FHeaderPosition := RasterStream.Position;
      RasterStream.Read(FHeader, SizeOf(FHeader));

      // Data section must follow immediately after grid section;
      RasterStream.Read(Header, SizeOf(Header));
      Assert(Header.ID = $41544144);
      FZPosition := RasterStream.Position;
      break;
    end;
  end;

end;

destructor TSurferRaster7FileBlock.Destroy;
begin
  if FModified then
  begin
    Flush;
  end;
  inherited;
end;

procedure TSurferRaster7FileBlock.Flush;
var
  RowIndex: Integer;
  ZRowStart: Int64;
  RowLength: Integer;
begin
  if Length(ZValues) <> 0 then
  begin
    RowLength := Min(FEdgeCount, FHeader.nCol - FXStart);
    for RowIndex := 0 to FEdgeCount - 1 do
    begin
      if (RowIndex + FYStart) >= FHeader.nRow then
      begin
        break;
      end;
      ZRowStart := FZPosition + ((FYStart+RowIndex)*FHeader.nCol + FXStart)*SizeOf(double);
      FRasterStream.Position := ZRowStart;
      FRasterStream.Write(ZValues[RowIndex], RowLength*SizeOf(double))
    end;
    FModified := False;
    SetLength(ZValues, 0);
  end;
end;

function TSurferRaster7FileBlock.GetHeader: TGrid7Header;
begin
  result := inherited;
  result.xLL := result.xLL + FXStart*FHeader.XSize;
  result.yLL := result.yLL + FYStart*FHeader.YSize;
  result.nRow := FEdgeCount;
  result.nCol := FEdgeCount;
end;

function TSurferRaster7FileBlock.GetLowerLeft: TPoint2D;
begin
  result := inherited;
  result.x := result.x + FXStart*FHeader.XSize;
  result.Y := result.Y + FYStart*FHeader.YSize;
end;

function TSurferRaster7FileBlock.GetXCount: integer;
begin
  result := FEdgeCount;
end;

function TSurferRaster7FileBlock.GetYCount: integer;
begin
  result := FEdgeCount;
end;

function TSurferRaster7FileBlock.GetZ(XIndex, YIndex: Integer): Double;
begin
  ReadZValues;
  result := ZValues[XIndex, YIndex];
end;

procedure TSurferRaster7FileBlock.ReadZValues;
var
  RowIndex: Integer;
  ZRowStart: Int64;
  ColIndex: Integer;
  RowLength: Integer;
begin
  if Length(ZValues) = 0 then
  begin
    SetLength(ZValues, FEdgeCount, FEdgeCount);
    for RowIndex := 0 to FEdgeCount - 1 do
    begin
      for ColIndex := 0 to FEdgeCount - 1 do
      begin
        ZValues[ColIndex,RowIndex] := FHeader.BlankValue;
      end;
    end;
    RowLength := Min(FEdgeCount, FHeader.nCol - FXStart);
    for RowIndex := 0 to FEdgeCount - 1 do
    begin
      if (RowIndex + FYStart) >= FHeader.nRow then
      begin
        break;
      end;
      ZRowStart := FZPosition + ((FYStart+RowIndex)*FHeader.nCol + FXStart)*SizeOf(double);
      FRasterStream.Position := ZRowStart;
      FRasterStream.Read(ZValues[RowIndex], RowLength*SizeOf(double))
    end;
    FModified := False;
  end;
end;

procedure TSurferRaster7FileBlock.SetHeader(const Value: TGrid7Header);
begin
  Assert(False);
end;

procedure TSurferRaster7FileBlock.SetZ(XIndex, YIndex: integer;
  const Value: double);
begin
  ReadZValues;
  ZValues[XIndex, YIndex] := Value;
  FModified := True;
end;

end.
