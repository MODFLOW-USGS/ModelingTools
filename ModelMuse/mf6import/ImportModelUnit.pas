unit ImportModelUnit;

interface

uses
  Modflow6ConstantsUnit,
  Mf6Variables,
  Mf6Functions, System.Classes, System.SysUtils, System.Generics.Collections;

type
  TAnsiStringArray = Array of AnsiString;
  TCIntArray = array of cint;
  TDoubleArray = array of double;
  TLongBoolArray = array of LongBool;

  TStressPeriod = record
    Length: Double;
    NStep: Integer;
    TSMult: double;
    Transient: Boolean;
  end;

  TStressPeriods = array of TStressPeriod;

  TPackage = record
    PackageType: AnsiString;
    PackageName: AnsiString
  end;

  TPackages = array of TPackage;

  TVertex = record
    VertexNumber: Integer;
    X: double;
    Y: double;
  end;

  TVertices = array of TVertex;

  TCell = record
    CellNumber: integer;
    CellX: double;
    CellY: double;
    VertexNumbers: TCIntArray;
  end;

  TCells = array of TCell;

  TMfCell = record
    Layer: Integer;
    Row: Integer;
    Column: Integer;
  end;

  TImportedPackage = class(TObject)
    PackageName: AnsiString;
    constructor Create(APackageName: AnsiString); virtual;
  end;

  TImportedDiscretization = class(TImportedPackage)
    Mf6GridType: TMf6GridType;
    PackageID: AnsiString;
    MFGridShape: TCIntArray;
    LengthUnit: Tmf6LengthUnit;
    RotationAngle: Double;
    XOrigin: Double;
    YOrigin: Double;
    Vertices: TVertices;
    Cells: TCells;
    DelR: TDoubleArray;
    DelC: TDoubleArray;
    GridTop: TDoubleArray;
    GridBottom: TDoubleArray;
    IDomain: TCIntArray;
  end;

  TImportedNpf = class(TImportedPackage)
    NpfAveraging: TNpfAveraging;
    THICKSTRT: Boolean;
    VARIABLECV: Boolean;
    DEWATERED: Boolean;
    Perched: Boolean;
    REWET: Boolean;
    WETFCT: Double;
    IWETIT: Integer;
    IHDWET: Integer;
    K22OVERK: Boolean;
    K33OVERK: Boolean;
    XT3D: Boolean;
    XT3D_RHS: Boolean;
    CellType: TCIntArray;
    K: TDoubleArray;
    K22: TDoubleArray;
    K33: TDoubleArray;
    Angle1: TDoubleArray;
    Angle2: TDoubleArray;
    Angle3: TDoubleArray;
    WetDry: TDoubleArray;
  end;

  TImportedIms = class(TObject)
    PrintOption: Integer;
    NoPtc: Integer;
    ATS_OUTER_MAXIMUM_FRACTION: Double;
    OUTER_DVCLOSE: Double;
    OUTER_MAXIMUM: Integer;
    Gamma: Double;
    INNER_MAXIMUM: Integer;
    INNER_DVCLOSE: Double;
    INNER_RCLOSE: Double;
    LINEAR_ACCELERATION: Integer;
    RELAXATION_FACTOR: Double;
    PRECONDITIONER_LEVELS: Integer;
    PRECONDITIONER_DROP_TOLERANCE: Double;
    NUMBER_ORTHOGONALIZATIONS: Integer;
    Method: Integer;
    UnderRelax: Integer;
    UNDER_RELAXATION_THETA: Double;
    UNDER_RELAXATION_KAPPA: Double;
    UNDER_RELAXATION_MOMENTUM: Double;
    BackTrackNumber: Integer;
    BACKTRACKING_TOLERANCE: Double;
    BACKTRACKING_REDUCTION_FACTOR: Double;
    BACKTRACKING_RESIDUAL_LIMIT: Double;
    SCALING_METHOD: Integer;
    REORDERING_METHOD: Integer;
  end;

  TImportedStorage = class(TImportedPackage)
    STORAGECOEFFICIENT: Integer;
    SS_CONFINED_ONLY: Integer;
    ICONVERT: TCIntArray;
    SS: TDoubleArray;
    SY: TDoubleArray;
  end;

  TImportedInitialConditions = class(TImportedPackage)
    STRT: TDoubleArray;
  end;

  TCustomBoundaryStressPeriod = class(TObject)
    NodeList: TCIntArray;
    BoundNames: TAnsiStringArray;
    AuxValues: TDoubleArray;
    Bound: TDoubleArray;
    NBOUND: Integer;
  end;

  TCustomImportedBoundary = class(TImportedPackage)
    Naux: Integer;
    AuxNames: TAnsiStringArray;
    AuxMult: Integer;
    Mover: Integer;
    MAXBOUND: Integer;
    procedure GetPackageValues(ModelName: AnsiString); virtual;
  end;

  TChdStressPeriod = class(TCustomBoundaryStressPeriod)
  end;

  TChdStressPeriods = TObjectList<TChdStressPeriod>;

  TImportedChd = class(TCustomImportedBoundary)
    StressPeriods: TChdStressPeriods;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
  end;

  TImportedChdList = TObjectList<TImportedChd>;

  TGhbStressPeriod = class(TCustomBoundaryStressPeriod)
    function BHead(CellIndex: Integer): double;
    function Cond(CellIndex: Integer): double;
  end;

  TGhbStressPeriods = TObjectList<TGhbStressPeriod>;

  TImportedGhb = class(TCustomImportedBoundary)
    StressPeriods: TGhbStressPeriods;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
  end;

  TImportedGhbList = TObjectList<TImportedGhb>;

  TRivStressPeriod = class(TCustomBoundaryStressPeriod)
    function Stage(CellIndex: Integer): double;
    function Cond(CellIndex: Integer): double;
    function RBot(CellIndex: Integer): double;
  end;

  TRivStressPeriods = TObjectList<TRivStressPeriod>;

  TImportedRiv = class(TCustomImportedBoundary)
    StressPeriods: TRivStressPeriods;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
  end;

  TImportedRivList = TObjectList<TImportedRiv>;

  TWelStressPeriod = class(TCustomBoundaryStressPeriod)
  end;

  TWelStressPeriods = TObjectList<TWelStressPeriod>;

  TImportedWel = class(TCustomImportedBoundary)
    StressPeriods: TWelStressPeriods;
    AUTO_FLOW_REDUCE: Double;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
    procedure GetPackageValues(ModelName: AnsiString); override;
  end;

  TImportedWelList = TObjectList<TImportedWel>;

  TDrnStressPeriod = class(TCustomBoundaryStressPeriod)
    function Elev(CellIndex: Integer): double;
    function Cond(CellIndex: Integer): double;
  end;

  TDrnStressPeriods = TObjectList<TDrnStressPeriod>;

  TImportedDrn = class(TCustomImportedBoundary)
    StressPeriods: TDrnStressPeriods;
    AUXDEPTHNAME: Integer;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
    procedure GetPackageValues(ModelName: AnsiString); override;
  end;

  TImportedDrnList = TObjectList<TImportedDrn>;

  // note, the rate is the volumetric rate = rate * surface area.
  TRchStressPeriod = class(TCustomBoundaryStressPeriod)
  end;

  TRchStressPeriods = TObjectList<TRchStressPeriod>;

  TImportedRch = class(TCustomImportedBoundary)
    StressPeriods: TRchStressPeriods;
    AUTO_FLOW_REDUCE: Double;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
    procedure GetPackageValues(ModelName: AnsiString); override;
  end;

  TImportedRchList = TObjectList<TImportedRch>;

  TEvtStressPeriod = class(TCustomBoundaryStressPeriod)
    NSEG: Integer;
    SURF_RATE_SPECIFIED: Integer;
    ValuesPerBoundary: integer;
    function Surface(CellIndex: Integer): double;
  // note, the rate is the volumetric rate = rate * surface area.
    function Rate(CellIndex: Integer): double;
    function Depth(CellIndex: Integer): double;
    function Pxdp(CellIndex, SegIndex: Integer): double;
    function Petm(CellIndex, SegIndex: Integer): double;
    function Petm0(CellIndex: Integer): double;
  end;

  TEvtStressPeriods = TObjectList<TEvtStressPeriod>;

  TImportedEvt = class(TCustomImportedBoundary)
    NSEG: Integer;
    StressPeriods: TEvtStressPeriods;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
    procedure GetPackageValues(ModelName: AnsiString); override;
  end;

  TImportedEvtList = TObjectList<TImportedEvt>;

  TMawStressPeriod = class(TCustomBoundaryStressPeriod)
    // can't get well status.
    Rate: TDoubleArray;
    // fwelev, fwcond, and fwrlen are all set to zero for non-flowing wells.
    fwelev: TDoubleArray;
    fwcond: TDoubleArray;
    fwrlen: TDoubleArray;
    WELL_HEAD: TDoubleArray;
    // head limit is set 1E20 if there is no head limit
    HEAD_LIMIT: TDoubleArray;
    // minrate and maxrate are zero in no shut off.
    minrate: TDoubleArray;
    maxrate: TDoubleArray;
    // pump_elevation and scaling_length are set to 1E20 if there is no RATE_SCALING
    pump_elevation: TDoubleArray;
    scaling_length: TDoubleArray;
  end;

  TMawStressPeriods = TObjectList<TMawStressPeriod>;

  TImportedMaw = class(TCustomImportedBoundary)
    NMAWWELLS: Integer;
    NO_WELL_STORAGE: Integer;
    FLOW_CORRECTION: Boolean;
    FLOWING_WELLS: Integer;
    SHUTDOWN_THETA: double;
    SHUTDOWN_KAPPA: double;
    StressPeriods: TMawStressPeriods;
    constructor Create(APackageName: AnsiString); override;
    destructor Destroy; override;
    procedure GetPackageValues(ModelName: AnsiString); override;
  private
    Radius: TDoubleArray;
    Bottom: TDoubleArray;
    Strt: TDoubleArray;
    // CondEqn = 0 -> SPECIFIED
    // CondEqn = 1 -> THIEM
    // CondEqn = 2 -> SKIN
    // CondEqn = 3 -> CUMULATIVE
    // CondEqn = 4 -> MEAN
    CondEqn: TCIntArray;
    NGWFNODES: TCIntArray;
    PckgAux: TDoubleArray;
  end;

  TImportedMawList = TObjectList<TImportedMaw>;



  TImportedModel = class(TObject)
    ModelName: AnsiString;
    ModelType: AnsiString;
    ModelNumber: Integer;
    StoPackageName: AnsiString;
    Packages: TPackages;
    ImportedIms: TImportedIms;
    ImportedDis: TImportedDiscretization;
    ImportedNpf: TImportedNpf;
    ImportedSTO: TImportedStorage;
    ImportedIc: TImportedInitialConditions;
    ImportedChdList: TImportedChdList;
    ImportedGhbList: TImportedGhbList;
    ImportedRivList: TImportedRivList;
    ImportedWelList: TImportedWelList;
    ImportedDrnList: TImportedDrnList;
    ImportedRchList: TImportedRchList;
    ImportedEvtList: TImportedEvtList;
    ImportedMawList: TImportedMawList;
    procedure UpdateChd;
    procedure UpdateGhb;
    procedure UpdateRiv;
    procedure UpdateWel;
    procedure UpdateDrn;
    procedure UpdateRch;
    procedure UpdateEvt;
    procedure UpdateMaw;
    destructor Destroy; override;
  end;

  TImportedModels = TObjectList<TImportedModel>;

procedure ImportModel;

procedure GetStringVariable(VarName: AnsiString; var NameTypes: TAnsiStringArray);
procedure GetIntegerVariable(VarName: AnsiString; var IntArray: TCIntArray);
procedure GetDoubleVariable(VarName: AnsiString; var RealArray: TDoubleArray);
procedure GetLogicalVariable(VarName: AnsiString; var BoolArray: TLongBoolArray);

implementation

uses
  Vcl.Dialogs, System.AnsiStrings, JclAnsiStrings, System.Math;

const
  StrLEN: Ansistring = 'LEN=';

type
  TAnsiCharArray = array[0..255*10] of AnsiChar;
  TDisvCellDict = TDictionary<Integer, TMfCell>;

var
  ErrorMessages: TStringList;
  NameList: TAnsiStringList;
  DisvCellDict: TDisvCellDict = nil;

procedure ExtractStrArray(AString: AnsiString; out Strings: TAnsiStringArray);
var
  StringList: TJclAnsiStringList;
  index: Integer;
  StartIndex: Integer;
  CharIndex: Integer;
  NewString: AnsiString;
begin
  StringList := TAnsiStringList.Create;
  try
    StartIndex := 1;
    for CharIndex := StartIndex to Length(AString) do
    begin
      if AString[CharIndex] = #0 then
      begin
        NewString := Trim(Copy(AString,StartIndex, CharIndex - StartIndex));
        if NewString <> '' then
        begin
          StringList.Add(NewString);
        end;
        StartIndex := CharIndex + 1;
      end;
    end;
    SetLength(Strings, StringList.Count);
    for index := 0 to StringList.Count - 1 do
    begin
      Strings[index] := StringList[index];
    end;
  finally
    StringList.Free;
  end;
end;

procedure ExtractStrArray2(AString: TAnsiCharArray; out Strings: TAnsiStringArray);
var
  StringList: TJclAnsiStringList;
  index: Integer;
//  StartIndex: Integer;
  CharIndex: Integer;
  NewString: AnsiString;
  StringBuilder: TStringBuilder;
begin
  StringList := TAnsiStringList.Create;
  StringBuilder := TStringBuilder.Create;
  try
//    StartIndex := 0;
    for CharIndex := 0 to Length(AString) -1 do
    begin
      if AString[CharIndex] = #0 then
      begin
        NewString := AnsiString(Trim(StringBuilder.ToString));
        if NewString <> '' then
        begin
          StringList.Add(NewString);
        end;
//        StartIndex := CharIndex + 1;
        StringBuilder.Clear;
      end
      else
      begin
        StringBuilder.Append(AString[CharIndex])
      end;
    end;
    SetLength(Strings, StringList.Count);
    for index := 0 to StringList.Count - 1 do
    begin
      Strings[index] := StringList[index];
    end;
  finally
    StringList.Free;
  end;
end;

function GetModelType: TAnsiStringArray;
Const
  ModelTypeVarName = '__INPUT__/SIM/NAM/MTYPE';
begin
  GetStringVariable(ModelTypeVarName, result);
end;

function GetModelName: TAnsiStringArray;
const
  ModelNameVarName = '__INPUT__/SIM/NAM/MNAME';
begin
  GetStringVariable(ModelNameVarName, result);
end;

function GetNumberOfPeriods: Integer;
const
  NPerVanName = 'TDIS/NPER';
var
  IntArray: TCIntArray;
begin
  GetIntegerVariable(NPerVanName, IntArray);
  Assert(Length(IntArray) = 1);
  result := IntArray[0];
end;

function GetNumberOfSteps: TCIntArray;
const
  NStepVanName = 'TDIS/NSTP';
begin
  GetIntegerVariable(NStepVanName, result);
end;

function GetPerLen: TDoubleArray;
const
  PerLenName = 'TDIS/PERLEN';
begin
  GetDoubleVariable(PerLenName, result);
end;

function GetTsmult: TDoubleArray;
const
  TsMultName = 'TDIS/TSMULT';
begin
  GetDoubleVariable(TsMultName, result);
end;

function GetEndOfSimulation: Boolean;
const
  EndSimName = 'TDIS/ENDOFSIMULATION';
var
  BoolArray: TLongBoolArray;
begin
  GetLogicalVariable(EndSimName, BoolArray);
  Assert(Length(BoolArray) = 1);
  result := BoolArray[0];
end;

function GetPackageType(VarName: AnsiString): AnsiString; overload;
var
  Values: TAnsiStringArray;
begin
  GetStringVariable(VarName,  Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetPackageType(ModelName, PackageName: AnsiString): AnsiString; overload;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/'+ PackageName + '/PACKAGE_TYPE';
  result := GetPackageType(VarName)
end;

function GetPackages(ModelName: AnsiString): TPackages;
var
  Index: Integer;
  PackageNameString: AnsiString;
  PackageTypeString: AnsiString;
  PackageTypes: TAnsiStringArray;
  PackageNames: TAnsiStringArray;
  PackageName: AnsiString;
  PackageType: AnsiString;
  PIndex: Integer;
  AVarName: AnsiString;
  ModelNamePosition: Integer;
  PackageTypePostion: Integer;
  StartIndex: Integer;
  VarLength: Integer;
begin
  PackageTypeString := '__INPUT__/' + ModelName + '/PKGTYPES';
  PackageNameString := '__INPUT__/' + ModelName + '/PKGNAMES';
  GetStringVariable(PackageTypeString, PackageTypes);
  GetStringVariable(PackageNameString, PackageNames);
  if Length(PackageNames) = Length(PackageTypes) then
  begin
    SetLength(Result, Length(PackageNames));
    for Index := 0 to Length(PackageNames) - 1 do
    begin
      Result[Index].PackageType := PackageTypes[Index];
      Result[Index].PackageName := PackageNames[Index];
    end;
  end
  else
  begin
    SetLength(Result, Length(PackageTypes));
    for Index := 0 to Length(Result) - 1 do
    begin
      Result[Index].PackageType := '';
      Result[Index].PackageName := '';
    end;
    PIndex := 0;
    for Index := 0 to NameList.Count - 1 do
    begin
      AVarName := NameList[Index];
      ModelNamePosition := Pos(ModelName + '/', AVarName);
      PackageTypePostion := Pos('/PACKAGE_TYPE', AVarName);
      if (ModelNamePosition = 1) and (PackageTypePostion > 1) then
      begin
        PackageType := GetPackageType(AVarName);
        StartIndex:= Length(ModelName + '/')+1;
        VarLength := PackageTypePostion - StartIndex;
        PackageName := MidStr(AVarName, StartIndex, VarLength);
        Assert(PIndex < Length(Result));
        Result[PIndex].PackageType := PackageType + '6';
        Result[PIndex].PackageName := PackageName;
        Inc(PIndex);
      end;
    end;
    if PIndex < Length(result) then
    begin
      SetLength(Result, PIndex);
    end;

  end;

  for Index := 0 to Length(result) - 1 do
  begin
    Writeln(result[Index].PackageType, ' ', result[Index].PackageName)
  end;

end;

function GetModflowGridType(ModelName: AnsiString; var GridShape: TCIntArray): TMf6GridType;
var
  MShapeName: AnsiString;
begin
  MShapeName := '__INPUT__/' + ModelName + '/MODEL_SHAPE';
  GetIntegerVariable(MShapeName, GridShape);
  Assert(Length(GridShape) in [1..3]);
  result := TMf6GridType(Length(GridShape) -1);
end;

function GetTimeUnits: TMf6TimeUnits;
var
  TimeUnits: TCIntArray;
  VarName: AnsiString;
begin
  VarName := 'TDIS/ITMUNI';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, TimeUnits);
    Assert(Length(TimeUnits) = 1);
    Assert(TimeUnits[0] in [0..5]);
    result := TMf6TimeUnits(TimeUnits[0]);
  end
  else
  begin
    result := mtuUndefined;
  end;
end;

function GetLengthUnit(ModelName, DisPackageName: AnsiString): Tmf6LengthUnit;
var
  VarName: AnsiString;
  LengthUnits: TAnsiStringArray;
begin
  Result := mluUndefined;
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/LENGTH_UNITS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetStringVariable(VarName, LengthUnits);
    Assert(Length(LengthUnits) = 1);
    if LengthUnits[0] = 'UNDEFINED' then
    begin
      Result := mluUndefined;
    end
    else if LengthUnits[0] = 'FEET' then
    begin
      Result := mluFeet;
    end
    else if LengthUnits[0] = 'METERS' then
    begin
      Result := mluMeters;
    end
    else if LengthUnits[0] = 'CENTIMETERS' then
    begin
      Result := mluCentimeters;
    end
    else
    begin
      Assert(False)
    end;
  end
  else
  begin
    Result := mluUndefined;
  end;
end;

function GetXOrigin(ModelName, DisPackageName: AnsiString): Double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := ModelName + '/' + DisPackageName + '/XORIGIN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

function GetYOrigin(ModelName, DisPackageName: AnsiString): Double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := ModelName + '/' + DisPackageName + '/YORIGIN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

function GetGridRotation(ModelName, DisPackageName: AnsiString): Double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := ModelName + '/' + DisPackageName + '/ANGROT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

// Get column widths
function GetDelR(ModelName, DisPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/DELR';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

// Get row widths
function GetDelc(ModelName, DisPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/DELC';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetTop(ModelName, DisPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/TOP';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetBottom(ModelName, DisPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/BOTM';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetIDomain(ModelName, DisPackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/IDOMAIN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetVertices(ModelName, DisPackageName: AnsiString): TVertices;
var
  IVarName: AnsiString;
  XVarName: AnsiString;
  YVarName: AnsiString;
  IArray: TCIntArray;
  XArray: TDoubleArray;
  YArray: TDoubleArray;
  Index: Integer;
begin
  IVarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/IV';
  XVarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/XV';
  YVarName := '__INPUT__/' + ModelName + '/' + DisPackageName + '/YV';
  Assert(NameList.IndexOf(IVarName) >= 0);
  Assert(NameList.IndexOf(XVarName) >= 0);
  Assert(NameList.IndexOf(YVarName) >= 0);
  GetIntegerVariable(IVarName, IArray);
  GetDoubleVariable(XVarName, XArray);
  GetDoubleVariable(YVarName, YArray);
  Assert(Length(IArray) = Length(XArray));
  Assert(Length(IArray) = Length(YArray));
  SetLength(result, Length(IArray));
  for Index := 0 to Length(result) - 1 do
  begin
    result[Index].VertexNumber := IArray[Index];
    result[Index].X := XArray[Index];
    result[Index].Y := YArray[Index];
  end;
end;

function GetDisvCells(ModelName, DisPackageName: AnsiString): TCells;
var
  ICellVarName: AnsiString;
  XCellVarName: AnsiString;
  YCellVarName: AnsiString;
  NVertCellVarName: AnsiString;
  IVertCellVarName: AnsiString;
  ICell: TCIntArray;
  XCell: TDoubleArray;
  YCell: TDoubleArray;
  NVert: TCIntArray;
  IVert: TCIntArray;
  CellIndex: Integer;
  VertIndex: Integer;
  VertexCount: cint;
  VIndex: Integer;
  VertexArrayLength: Integer;
begin
  ICellVarName := '__INPUT__/'+ ModelName +'/' + DisPackageName + '/ICELL2D';
  XCellVarName := '__INPUT__/'+ ModelName +'/' + DisPackageName + '/XC';
  YCellVarName := '__INPUT__/'+ ModelName +'/' + DisPackageName + '/YC';
  NVertCellVarName := '__INPUT__/'+ ModelName +'/' + DisPackageName + '/NCVERT';
  IVertCellVarName := '__INPUT__/'+ ModelName +'/' + DisPackageName + '/ICVERT';

  Assert(NameList.IndexOf(ICellVarName) >= 0);
  Assert(NameList.IndexOf(XCellVarName) >= 0);
  Assert(NameList.IndexOf(YCellVarName) >= 0);
  Assert(NameList.IndexOf(NVertCellVarName) >= 0);
  Assert(NameList.IndexOf(IVertCellVarName) >= 0);

  GetIntegerVariable(ICellVarName, ICell);
  GetDoubleVariable(XCellVarName, XCell);
  GetDoubleVariable(YCellVarName, YCell);
  GetIntegerVariable(NVertCellVarName, NVert);
  GetIntegerVariable(IVertCellVarName, IVert);

  Assert(Length(ICell) = Length(XCell));
  Assert(Length(ICell) = Length(YCell));
  Assert(Length(ICell) = Length(NVert));

  VertIndex := 0;
  SetLength(result, Length(ICell));
  VertexArrayLength := Length(IVert);
  for CellIndex := 0 to Length(result) - 1 do
  begin
    result[CellIndex].CellNumber := ICell[CellIndex];
    result[CellIndex].CellX := XCell[CellIndex];
    result[CellIndex].CellY := YCell[CellIndex];
    VertexCount := NVert[CellIndex];
    SetLength(result[CellIndex].VertexNumbers, VertexCount);
    for VIndex := 0 to VertexCount - 1 do
    begin
      Assert(VertIndex < VertexArrayLength);
      result[CellIndex].VertexNumbers[VIndex] := IVert[VertIndex];
      Inc(VertIndex);
    end;
  end;
  Assert(VertIndex = VertexArrayLength);
end;

function GetNpfAveraging(ModelName, NpfPackageName: AnsiString): TNpfAveraging;
var
  VarName: AnsiString;
  Values: TAnsiStringArray;
begin
  Result := naHarmonic;
  VarName := '__INPUT__/'+ ModelName + '/' + NpfPackageName +'/CELLAVG';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetStringVariable(VarName, Values);
    Assert(Length(Values) = 1);
    if Values[0] = 'LOGARITHMIC' then
    begin
      Result := naLog
    end
    else if Values[0] = 'AMT-LMK' then
    begin
      Result := naAritimeticLog
    end
    else if Values[0] = 'AMT-HMK' then
    begin
      Result := naHarmonicLog
    end
    else
    begin
      Assert(False)
    end;
  end
  else
  begin
    Result := naHarmonic;
  end;
end;

function UseTHICKSTRT(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/ITHICKSTRT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseVARIABLECV(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IVARCV';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseDEWATERED(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IDEWATCV';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UsePerched(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IPERCHED';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseREWET(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IREWET';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function GetWETFCT(ModelName, NpfPackageName: AnsiString): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
//  Values: TCIntArray;
begin
  result := Mf6Undefined;
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/WETFCT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    Assert(False);
  end;
end;

function GetIWETIT(ModelName, NpfPackageName: AnsiString): Integer;
var
  VarName: AnsiString;
//  Values: TDoubleArray;
  Values: TCIntArray;
begin
  Result := -1;
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IWETIT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    Assert(False);
  end;
end;

function GetIHDWET(ModelName, NpfPackageName: AnsiString): Integer;
var
  VarName: AnsiString;
//  Values: TDoubleArray;
  Values: TCIntArray;
begin
  result := -1;
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IHDWET';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    Assert(False);
  end;
end;

function UseK22OVERK(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IK22OVERK';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseK33OVERK(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IK33OVERK';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseXT3D(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IXT3D';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function UseXT3D_RHS(ModelName, NpfPackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName + '/IXT3DRHS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0] <> 0;
  end
  else
  begin
    result := False;
  end;
end;

function GetICELLTYPE(ModelName, NpfPackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/ICELLTYPE';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetIntegerVariable(VarName, result);
end;

function GetK(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/K';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetK22(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/K22';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetK33(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/K33';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetAngle1(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/ANGLE1';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetAngle2(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/ANGLE2';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetAngle3(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/ANGLE3';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetWetDry(ModelName, NpfPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := '__INPUT__/' + ModelName + '/' + NpfPackageName +'/WETDRY';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetImsPrintOption(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IPRIMS', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Assert(Values[0] in [0..2]);
    result := Values[0];
    // 0 = NONE
    // 1 = Summary
    // 2 = All
  end
  else
  begin
    result := 0;
  end;
end;

function GetImsNoPTC(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IALLOWPTC', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Assert(Values[0] in [0..1]);
    result := Values[0];
    // 0 = All
    // 1 = First
  end
  else
  begin
    result := -1;
    // -1 = not active
  end;
end;

function GetImsATS_OUTER_MAXIMUM_FRACTION(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/ATSFRAC', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 1/3;
  end;
end;

function GetImsOUTER_DVCLOSE(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/DVCLOSE', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetImsOUTER_MAXIMUM(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/MXITER', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetIntegerVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetImsUNDER_RELAXATION(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/NONMETH', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
    assert(result in [0..3]);
    // 0 = none
    // 1 = SIMPLE
    // 2 = COOLEY
    // 3 = DBD
  end
  else
  begin
    result := 0;
  end;
end;

function GetImsUNDER_RELAXATION_GAMMA(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/GAMMA', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0.2;
  end;
end;

function GetImsUNDER_RELAXATION_THETA(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/THETA', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0.7;
  end;
end;

function GetImsUNDER_RELAXATION_KAPPA(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/AKAPPA', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0.1;
  end;
end;

function GetImsUNDER_RELAXATION_MOMENTUM(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/AMOMENTUM', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0.001;
  end;
end;

function GetImsBACKTRACKING_NUMBER(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IBFLAG', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  10;
  end;
end;

function GetImsBACKTRACKING_TOLERANCE(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/BTOL', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  1E4;
  end;
end;

function GetImsBACKTRACKING_REDUCTION_FACTOR(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/BREDUC', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0.2;
  end;
end;

function GetImsBACKTRACKING_RESIDUAL_LIMIT(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/RES_LIM', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  100;
  end;
end;

function GetImsINNER_MAXIMUM(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/ITER1', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetIntegerVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetImsINNER_DVCLOSE(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/DVCLOSE', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetImsINNER_RCLOSE(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/RCLOSE', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
end;

function GetImsLINEAR_ACCELERATION(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/ILINMETH', [SolutionNumber]));
  Assert(NameList.IndexOf(VarName) >= 0);
  GetIntegerVariable(VarName, Values);
  Assert(Length(Values) = 1);
  result := Values[0];
  Assert(result in [1,2])
  // 1 = CG
  // 2 = BICGSTAB
end;

function GetImsRELAXATION_FACTOR(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/RELAX', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0;
  end;
end;

function GetImsPRECONDITIONER_LEVELS(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/LEVEL', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0;
  end;
end;

function GetImsPRECONDITIONER_DROP_TOLERANCE(SolutionNumber: integer): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/DROPTOL', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  0;
  end;
end;

function GetImsNUMBER_ORTHOGONALIZATIONS(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/NORTH', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result :=  7;
  end;
end;

function GetImsSCALING_METHOD(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/ISCL', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
    Assert(result in [0..2]);
    // 0 = NONE
    // 1 = DIAGONAL
    // 2 = L2NORM
  end
  else
  begin
    result :=  0;
  end;
end;

function GetImsREORDERING_METHOD(SolutionNumber: integer): integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := AnsiString(Format('SLN_%d/IMSLINEAR/IORD', [SolutionNumber]));
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
    Assert(result in [0..2]);
    // 0 = NONE
    // 1 = RCM
    // 2 = MD
  end
  else
  begin
    result :=  0;
  end;
end;

function GetStoSTORAGECOEFFICIENT(ModelName, StoPackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + StoPackageName + '/ISTOR_COEF';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
    Assert(result in [0..1]);
    // 1 = STORAGECOEFFICIENT
  end
  else
  begin
    result := 0;
  end;
end;

function GetStoSS_CONFINED_ONLY(ModelName, StoPackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + StoPackageName + '/ICONF_SS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
    Assert(result in [0..1]);
    // 1 = SS_CONFINED_ONLY
  end
  else
  begin
    result := 0;
  end;
end;

function GetStoICONVERT(ModelName, StoPackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + StoPackageName +'/ICONVERT';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetIntegerVariable(VarName, result);
end;

function GetStoSS(ModelName, StoPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + StoPackageName +'/SS';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetStoSY(ModelName, StoPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + StoPackageName +'/SY';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetIcSTRT(ModelName, IcPackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + IcPackageName +'/STRT';
  Assert(NameList.IndexOf(VarName) >= 0);
  GetDoubleVariable(VarName, result);
end;

function GetNAUX(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/NAUX';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0
  end;
end;

function GetMAXBOUND(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/MAXBOUND';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0
  end;
end;

function GetMOVER(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/IMOVER';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := 0
  end;
end;

function GetAUTO_FLOW_REDUCE(ModelName, PackageName: AnsiString): double;
var
  VarName: AnsiString;
  Values: TCIntArray;
  RValues: TDoubleArray;
begin
  VarName := ModelName + '/' + PackageName +'/IFLOWRED';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    if Values[0] = 1 then
    begin
      VarName := ModelName + '/' + PackageName +'/FLOWRED';
      GetDoubleVariable(VarName, RValues);
      Assert(Length(RValues) = 1);
      result := RValues[0];
    end
    else
    begin
      result := 0;
    end;
  end
  else
  begin
    result := 0
  end;
end;

function GetAUXDEPTHNAME(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/IAUXDDRNCOL';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0
  end;
end;

function GetNSEG(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/NSEG';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 1;
  end;
end;

function GetNMAWWELLS(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/NMAWWELLS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

function GetFLOWING_WELLS(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/IFLOWINGWELLS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

function GetSHUTDOWN_THETA(ModelName, PackageName: AnsiString): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := ModelName + '/' + PackageName +'/THETA';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0.7;
  end;
end;

function GetSHUTDOWN_KAPPA(ModelName, PackageName: AnsiString): double;
var
  VarName: AnsiString;
  Values: TDoubleArray;
begin
  VarName := ModelName + '/' + PackageName +'/KAPPA';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0.0001;
  end;
end;


function GetNO_WELL_STORAGE(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/IMAWISSOPT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0;
  end;
end;

function GetFLOW_CORRECTION(ModelName, PackageName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TLongBoolArray;
//  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/CORRECT_FLOW';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetLogicalVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := False;
  end;
end;

function GetMAW_Radius(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/RADIUS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetMAW_Bottom(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/BOT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetMAW_STRT(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/STRT';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetMAW_CondEqn(ModelName, PackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/IEQN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetMAW_NGWFNODES(ModelName, PackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/NGWFNODES';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, result);
  end
  else
  begin
    result := nil;
  end;
end;

function GetNBOUND(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/NBOUND';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Result := Values[0];
  end
  else
  begin
    result := 0
  end;
end;


function GetAuxName(ModelName, PackageName: AnsiString): TAnsiStringArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/AUXNAME_CST';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetStringVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetAuxMult(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName +'/IAUXMULTCOL';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    result := Values[0];
  end
  else
  begin
    result := -1;
  end;
end;

function GetBoundNames(ModelName, PackageName: AnsiString): TAnsiStringArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/BOUNDNAME_CST';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetStringVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetBound(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/BOUND';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetRate(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/RATE';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetFwElev(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/FWELEV';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetFwCond(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/FWCONDS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetFwRlen(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/FWRLEN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetWELL_HEAD(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/WELL_HEAD';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetHEAD_LIMIT(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/SHUTOFFLEVEL';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetMinRate(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/SHUTOFFMIN';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetMaxRate(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/SHUTOFFMAX';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetPumpElevation(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/PUMPELEV';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetScalingLength(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/REDUCTION_LENGTH';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetNodeList(ModelName, PackageName: AnsiString): TCIntArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/NODELIST';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetAuxValue(ModelName, PackageName: AnsiString): TDoubleArray;
var
  VarName: AnsiString;
begin
  VarName := ModelName + '/' + PackageName +'/AUXVAR';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetDoubleVariable(VarName, result);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;

function GetTransient(ModelName: AnsiString): Boolean;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + 'ISS';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    Assert(Values[0] in [0..1]);
    result := Values[0] = 1;
  end
  else
  begin
    result := False;
  end;
end;

function GetNewDataRead(ModelName, PackageName: AnsiString): Integer;
var
  VarName: AnsiString;
  Values: TCIntArray;
begin
  VarName := ModelName + '/' + PackageName + '/IONPER';
  if NameList.IndexOf(VarName) >= 0 then
  begin
    GetIntegerVariable(VarName, Values);
    Assert(Length(Values) = 1);
    result := Values[0];
  end
  else
  begin
    result := -1;
  end;
end;

procedure AlternateGetStringVariable2(VarName: AnsiString);
var
  Rank: Integer;
  ErrorMessage: TErrorMessage;
  Shape: array of Integer;
  Index: Integer;
begin
  if get_var_rank(PAnsiChar(VarName), Rank) = 0 then
  begin
    if Rank > 0 then
    begin
      if Rank > 1 then
      begin
        WriteLn('Rank > 1 for ', VarName, ': ', Rank);
      end;
      SetLength(Shape, Rank);
      if get_var_shape(PAnsiChar(VarName), @Shape[0]) = 0 then
      begin
        for Index := 0 to Length(Shape) - 1 do
        begin
          Writeln(Shape[Index]);
        end;
      end
      else
      begin
        Writeln('Failed to get variable shape for "', VarName, '".');
        get_last_bmi_error(@ErrorMessage);
        Writeln(ErrorMessage);
      end;
    end
    else
    begin
      Writeln('variable rank "', VarName, '" was set to ', Rank, '.');
    end;
  end
  else
  begin
    Writeln('Failed to get variable rank for "', VarName, '".');
    get_last_bmi_error(@ErrorMessage);
    Writeln(ErrorMessage);

  end;
end;

// AlternateGetStringVariable doesn't work.
procedure AlternateGetStringVariable(VarName: AnsiString);
var
  ResultString: AnsiString;
  var_nbytes: Integer;
  ErrorMessage: TErrorMessage;
  Index: Integer;
begin
  Writeln('Alternate method');
  var_nbytes := -1;
  if get_var_nbytes(PAnsiChar(VarName), var_nbytes) = 0 then
  begin
    if var_nbytes > 0 then
    begin
      SetLength(ResultString, var_nbytes*100);
      for Index := 1 to Length(ResultString) do
      begin
        ResultString[Index] := #0;
      end;
//      if get_value(PAnsiChar(VarName), PAnsiChar(ResultString)) = 0 then
      if get_value(PAnsiChar(VarName), @ResultString[1]) = 0 then
      begin
        Writeln(ResultString);
      end
      else
      begin
        Writeln('Failed to get variable value for "', VarName, '".');
        get_last_bmi_error(@ErrorMessage);
        Writeln(ErrorMessage);
      end;
    end
    else
    begin
      Writeln('variable size in bytes for "', VarName, '" was set to ', var_nbytes, '.');
    end;
  end
  else
  begin
    Writeln('Failed to get variable size in bytes for "', VarName, '".');
    get_last_bmi_error(@ErrorMessage);
    Writeln(ErrorMessage);
  end;

end;

procedure InitializeVariableType(out VariableType: AnsiString);
var
  CharIndex: Integer;
begin
  SetLength(VariableType, BMI_LENVARTYPE);
  for CharIndex := 1 to BMI_LENVARTYPE do
  begin
    VariableType[CharIndex] := #0;
  end;
end;

function GetArraySize(VarName: AnsiString): integer;
var
  Rank: Integer;
  Shape: array of cint;
  RankIndex: Integer;
  Error: TErrorMessage;
begin
  Rank := -1;
  result := -1;
  if get_var_rank(PAnsiChar(VarName), Rank) = 0 then
  begin
//    Writeln('Rank: ', Rank);
  end
  else
  begin
    ErrorMessages.Add('Failed to get Rank for "' + VarName + '".');
    WriteLn('Failed to get Rank for "', VarName, '".');
    if get_last_bmi_error(@Error) = 0 then
    begin
      Writeln(Error);
      ErrorMessages.Add(string(Error));
    end
    else
    begin
      Assert(False);
    end;
    Exit
  end;

  if Rank > 0 then
  begin
    SetLength(Shape, Rank);
    for RankIndex := 0 to Length(Shape) - 1 do
    begin
      Shape[RankIndex] := Mf6Undefined;
    end;
    if get_var_shape(PAnsiChar(VarName), @Shape[0]) = 0 then
    begin
      result := 1;
      for RankIndex := 0 to Length(Shape) - 1 do
      begin
        if Shape[RankIndex] <= 0 then
        begin
          WriteLn(VarName, ' ', Shape[RankIndex]);
          Assert(Shape[RankIndex] = 0);
        end;
        result := result*Shape[RankIndex];
      end;
    end
    else
    begin
      ErrorMessages.Add('Failed to get shape for "' + VarName + '".');
      ErrorMessages.Add(Error);

      WriteLn('Failed to get shape for "', VarName, '".');
      Assert(False);
    end;
  end
  else if Rank = 0 then
  begin
    result := 1;
  end;
end;

procedure GetLogicalVariable(VarName: AnsiString; var BoolArray: TLongBoolArray);
var
  VariableType: AnsiString;
  ArraySize: Integer;
  ValueIndex: Integer;
begin
  InitializeVariableType(VariableType);
  if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
  begin
    Assert(AnsiPos('LOGICAL', VariableType) = 1);
    ArraySize := GetArraySize(VarName);

    if ArraySize > 0 then
    begin
      SetLength(BoolArray, ArraySize);
      if get_value_bool(PAnsiChar(PAnsiChar(VarName)), @BoolArray) = 0 then
      begin
        for ValueIndex := 0 to Min(10, Length(BoolArray)) - 1 do
        begin
          Writeln(BoolArray[ValueIndex])
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;


procedure GetDoubleVariable(VarName: AnsiString; var RealArray: TDoubleArray);
var
  VariableType: AnsiString;
  ArraySize: Integer;
  ValueIndex: Integer;
begin
  InitializeVariableType(VariableType);
  if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
  begin
    Assert(AnsiPos('DOUBLE', VariableType) = 1);
    ArraySize := GetArraySize(VarName);

    if ArraySize > 0 then
    begin
      SetLength(RealArray, ArraySize);
      if get_value_double(PAnsiChar(PAnsiChar(VarName)), @RealArray) = 0 then
      begin
        for ValueIndex := 0 to Min(10, Length(RealArray)) - 1 do
        begin
          Writeln(RealArray[ValueIndex])
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure GetIntegerVariable(VarName: AnsiString; var IntArray: TCIntArray);
var
  VariableType: AnsiString;
//  IntArray: TCIntArray;
  ArraySize: Integer;
  ValueIndex: Integer;
begin
  InitializeVariableType(VariableType);
  if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
  begin
    Assert(AnsiPos('INTEGER', VariableType) = 1);
    ArraySize := GetArraySize(VarName);

    if ArraySize > 0 then
    begin
      SetLength(IntArray, ArraySize);
      if get_value_int(PAnsiChar(PAnsiChar(VarName)), @IntArray) = 0 then
      begin
        for ValueIndex := 0 to Min(10, Length(IntArray)) - 1 do
        begin
          Writeln(IntArray[ValueIndex])
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure GetStringVariable(VarName: AnsiString; var NameTypes: TAnsiStringArray);
var
  VariableType: AnsiString;
  CharIndex: Integer;
  StrLengthPos: Integer;
  ParenPos: Integer;
  VarLength: string;
  StringLength: Integer;
  Rank: Integer;
  NIndex: Integer;
  SavedVarType: AnsiString;
  VariableValues: AnsiString;
  PVarVal: PAnsiChar;
  PPVarVal: PPAnsiChar;
begin
  InitializeVariableType(VariableType);
  if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
  begin
    if AnsiPos('STRING', VariableType) = 1 then
    begin
      StrLengthPos := AnsiPos(StrLEN, VariableType);
      if StrLengthPos > 0 then
      begin
        Inc(StrLengthPos, Length(StrLEN));
        SavedVarType := VariableType;
        VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
        ParenPos := AnsiPos('(', VariableType);
        if ParenPos > 0 then
        begin
          VarLength := Trim(Copy(VariableType,1,ParenPos-1));
          VariableType := Copy(VariableType, ParenPos+1, MAXINT);
          ParenPos := AnsiPos(')', VariableType);
          Assert(ParenPos > 0);
          VariableType := Trim(Copy(VariableType,1,ParenPos-1));
          Rank := StrToInt(VariableType);
          if Rank = 0 then
          begin
            WriteLn('  empty');
            Exit;
          end;
        end
        else
        begin
          VarLength := VariableType;
          Rank := 1;
        end;
        StringLength := Max(StrToInt(VarLength), 255);
      end
      else
      begin
        StringLength := BMI_LENVARTYPE;
        if get_var_rank(PAnsiChar(VarName), Rank) = 0 then
        begin
          Writeln('Rank: ', Rank);
        end
        else
        begin
          WriteLn('Failed to get Rank for "', VarName, '".');
        end;
      end;
        SetLength(VariableValues, (StringLength+1)*Rank);
      begin
        for CharIndex := 1 to Length(VariableValues) do
        begin
          VariableValues[CharIndex] := #0;
        end;
      end;
      PVarVal := PAnsiChar(VariableValues);
      PPVarVal := @PVarVal;
      if get_value_string(PAnsiChar(VarName), PPVarVal) = 0 then
      begin
        begin
        try
          ExtractStrArray(VariableValues, NameTypes);
        except
          Writeln(VarName);
          Exit;
        end;
//            ExtractStrArray(TypeNames[RankIndex], NameTypes);
          if Length(NameTypes) > 0 then
          begin
            for NIndex := 0 to Length(NameTypes) - 1 do
            begin
              WriteLn('  ', Trim(NameTypes[NIndex]));
            end;
          end
          else
          begin
            WriteLn('  no values extracted from "', VarName, '."');
          end;
        end;
      end
      else
      begin
        WriteLn('Failed to get values for "', VarName, '."');
      end;
    end;
  end
  else
  begin
    WriteLn;
    WriteLn('Failed to get variable type for "', VarName, '".');
  end;
end;

procedure GetStringVariables(Names: TStringList);
var
  Index: Integer;
  VarName: AnsiString;
  NameTypes: TAnsiStringArray;
//  VariableType: AnsiString;
//  CharIndex: Integer;
//  StrLengthPos: Integer;
//  ParenPos: Integer;
//  VarLength: string;
//  StringLength: Integer;
//  TypeNames: array of AnsiString;
//  Rank: Integer;
//  NameTypes: TAnsiStringArray;
//  NIndex: Integer;
//  SavedVarType: AnsiString;
//  RankIndex: Integer;
//  VariableValues: TAnsiCharArray;
//  PVarVal: PAnsiChar;
//  PPVarVal: PPAnsiChar;
begin
  for Index := 0 to Names.Count - 1 do
  begin
    VarName := AnsiString(Names[Index]);
    GetStringVariable(VarName, NameTypes);
//    SetLength(VariableType, BMI_LENVARTYPE);
//    for CharIndex := 1 to BMI_LENVARTYPE do
//    begin
//      VariableType[CharIndex] := AnsiChar(nil);
//    end;
//    if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
//    begin
////      Writeln(VarName, ': ', VariableType);
//      if Pos('STRING', VariableType) = 1 then
//      begin
//        Writeln;
//        Writeln(VarName);
//        StrLengthPos := Pos(StrLEN, VariableType);
//        if StrLengthPos > 0 then
//        begin
//          Inc(StrLengthPos, Length(StrLEN));
//          SavedVarType := VariableType;
//          VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
//          ParenPos := Pos('(', VariableType);
//          if ParenPos > 0 then
//          begin
//            VarLength := Trim(Copy(VariableType,1,ParenPos-1));
//            VariableType := Copy(VariableType, ParenPos+1, MAXINT);
//            ParenPos := Pos(')', VariableType);
//            Assert(ParenPos > 0);
//            VariableType := Trim(Copy(VariableType,1,ParenPos-1));
//            Rank := StrToInt(VariableType);
//            if Rank = 0 then
//            begin
//              WriteLn('  empty');
//              Continue;
//            end;
//          end
//          else
//          begin
//            VarLength := VariableType;
//            Rank := 1;
//          end;
//          StringLength := Max(StrToInt(VarLength), 255);
//        end
//        else
//        begin
//          StringLength := BMI_LENVARTYPE;
//          if get_var_rank(PAnsiChar(VarName), Rank) = 0 then
//          begin
//            Writeln('Rank: ', Rank);
//          end
//          else
//          begin
//            WriteLn('Failed to get Rank for "', VarName, '".');
//          end;
//        end;
////        StringLength := StrToInt(VarLength);
////        Rank := 1;
//        RankIndex := 0;
////        SetLength(TypeNames, Rank);
////        SetLength(VariableValues, (StringLength+1)*Rank);
////        for RankIndex := 0 to Rank - 1 do
//        begin
////          SetLength(TypeNames[RankIndex], (StringLength+1)*Rank);
//          for CharIndex := 0 to Length(VariableValues) -1 do
//          begin
////            TypeNames[RankIndex][CharIndex] := AnsiChar(nil);
//            VariableValues[CharIndex] := #0;
//          end;
//        end;
//        PVarVal := @VariableValues;
//        PPVarVal := @PVarVal;
//        if get_value_string(PAnsiChar(VarName), PPVarVal) = 0 then
//        begin
////          for RankIndex := 0 to Rank - 1 do
//          begin
//            ExtractStrArray2(VariableValues, NameTypes);
////            ExtractStrArray(TypeNames[RankIndex], NameTypes);
//            if Length(NameTypes) > 0 then
//            begin
//              for NIndex := 0 to Length(NameTypes) - 1 do
//              begin
//                WriteLn('  ', Trim(NameTypes[NIndex]));
//              end;
//            end
//            else
//            begin
//              WriteLn('  no values extracted from "', VarName, '."');
//              Writeln(SavedVarType);
//              for CharIndex := 0 to Length(VariableValues) - 1 do
//              begin
//                Write(VariableValues[CharIndex]);
//              end;
//              Writeln;
//            end;
//          end;
//        end
//        else
//        begin
//          WriteLn('Failed to get values for "', VarName, '."');
//        end;
//      end;
//    end
//    else
//    begin
//      WriteLn;
//      WriteLn('Failed to get variable type for "', VarName, '".');
//    end;
  end;
end;

procedure GetSimulationValues(Names: TAnsiStringList);
var
  Index: Integer;
  VarName: AnsiString;
  VariableType: AnsiString;
  NameTypes: TAnsiStringArray;
  IntArray: TCIntArray;
  RealArray: TDoubleArray;
  BoolArray: TLongBoolArray;
  WriteValues: Boolean;
  procedure InitializeVarType;
  var
    CharIndex: Integer;
  begin
    for CharIndex := 1 to BMI_LENVARTYPE do
    begin
      VariableType[CharIndex] := #0;
    end;
  end;
begin
  SetLength(VariableType, BMI_LENVARTYPE);
  for Index := 0 to Names.Count - 1 do
  begin
    VarName := AnsiString(Names[Index]);
    WriteValues := Pos('MAW', VarName) > 0;
//    WriteValues := True;
    if not WriteValues then
    begin
      Continue;
    end;
    InitializeVarType;
    if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
    begin
      Writeln(VarName, ': ', VariableType);
      if AnsiPos('STRING', VariableType) = 1 then
      begin
        GetStringVariable(VarName, NameTypes);
      end
      else if AnsiPos('INTEGER', VariableType) = 1 then
      begin
        GetIntegerVariable(VarName, IntArray);
      end
      else if AnsiPos('DOUBLE', VariableType) = 1 then
      begin
        GetDoubleVariable(VarName, RealArray);
      end
      else if AnsiPos('LOGICAL', VariableType) = 1 then
      begin
        GetLogicalVariable(VarName, BoolArray);
      end
      else
      begin
        Writeln('bad type')
      end;
    end
    else
    begin
      Writeln('failed to get variable type for ', VarName);
    end;
  end;
end;

procedure GetSimulationInputs(Names: TAnsiStringList);
var
  Value: array[0..355*6] of Ansichar;
  ABool: array of LongBool;
  ASize: Integer;
  VariableType: AnsiString;
  CharIndex: Integer;
  VarName: AnsiString;
  StrLengthPos: Integer;
  ParenPos: Integer;
  VarLength: AnsiString;
  StringLength: Integer;
  TypeNames: array of AnsiString;
  NameIndex: Integer;
//  TypeNamesAddr: array of PAnsiChar;
  NameTypes: TAnsiStringArray;
  NIndex: Integer;
begin
  VarName := '__INPUT__/MODFLOW/NAM/FTYPE';
  if Names.IndexOf(VarName) >= 0 then
  begin
    Writeln(VarName);
    SetLength(VariableType, BMI_LENVARTYPE);
    for CharIndex := 1 to BMI_LENVARTYPE do
    begin
      VariableType[CharIndex] := AnsiChar(nil);
    end;
    if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
    begin
      Writeln(VarName, ': ', VariableType);
    end;
    StrLengthPos := AnsiPos(StrLEN, VariableType);
    Assert(StrLengthPos > 0);
    Inc(StrLengthPos, Length(StrLEN));
    VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
    ParenPos := AnsiPos('(', VariableType);
    if ParenPos > 0 then
    begin
      VarLength := Trim(Copy(VariableType,1,ParenPos-1));
    end
    else
    begin
      VarLength := VariableType;
    end;
    StringLength := StrToInt(VarLength);
    SetLength(TypeNames, 1);
//    SetLength(TypeNamesAddr, 6);

    for NameIndex := 0 to Length(TypeNames) - 1 do
    begin
      SetLength(TypeNames[NameIndex], (StringLength+1)*6);
      for CharIndex := 1 to (StringLength+1)*6 do
      begin
        TypeNames[NameIndex][CharIndex] := AnsiChar(nil);
      end;
//      TypeNamesAddr[NameIndex] := @TypeNames[NameIndex];
    end;
    if get_value_string(PAnsiChar(VarName), @TypeNames[0]) = 0 then
    begin
      for NameIndex := 0 to Length(TypeNames) - 1 do
      begin
        Writeln(Length(TypeNames[NameIndex]));
        ExtractStrArray(TypeNames[NameIndex], NameTypes);
        for NIndex := 0 to Length(NameTypes) - 1 do
        begin
          WriteLn(Trim(NameTypes[NIndex]));
        end;
      end;
    end
    else
    begin
      WriteLn('Failed get_value_string');
    end;
  end;

  VarName := '__INPUT__/MODFLOW/PKGNAMES';
  if Names.IndexOf(VarName) >= 0 then
  begin
    Writeln(VarName);
    SetLength(VariableType, BMI_LENVARTYPE);
    for CharIndex := 1 to BMI_LENVARTYPE do
    begin
      VariableType[CharIndex] := AnsiChar(nil);
    end;
    if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
    begin
      Writeln(VarName, ': ', VariableType);
    end;
    StrLengthPos := AnsiPos(StrLEN, VariableType);
    Assert(StrLengthPos > 0);
    Inc(StrLengthPos, Length(StrLEN));
    VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
    ParenPos := AnsiPos('(', VariableType);
    if ParenPos > 0 then
    begin
      VarLength := Trim(Copy(VariableType,1,ParenPos-1));
    end
    else
    begin
      VarLength := VariableType;
    end;
    StringLength := StrToInt(VarLength);
    SetLength(TypeNames, 1);
//    SetLength(TypeNamesAddr, 6);

    for NameIndex := 0 to Length(TypeNames) - 1 do
    begin
      SetLength(TypeNames[NameIndex], (StringLength+1)*6);
      for CharIndex := 1 to (StringLength+1)*6 do
      begin
        TypeNames[NameIndex][CharIndex] := AnsiChar(nil);
      end;
//      TypeNamesAddr[NameIndex] := @TypeNames[NameIndex];
    end;
    if get_value_string(PAnsiChar(VarName), @TypeNames[0]) = 0 then
    begin
      for NameIndex := 0 to Length(TypeNames) - 1 do
      begin
        Writeln(Length(TypeNames[NameIndex]));
        ExtractStrArray(TypeNames[NameIndex], NameTypes);
        for NIndex := 0 to Length(NameTypes) - 1 do
        begin
          WriteLn(Trim(NameTypes[NIndex]));
        end;
      end;
    end
    else
    begin
      WriteLn('Failed get_value_string');
    end;
  end;

  VarName := '__INPUT__/MODFLOW/NAM/PNAME';
  if Names.IndexOf(VarName) >= 0 then
  begin
    Writeln(VarName);
    SetLength(VariableType, BMI_LENVARTYPE);
    for CharIndex := 1 to BMI_LENVARTYPE do
    begin
      VariableType[CharIndex] := AnsiChar(nil);
    end;
    if get_var_type(PAnsiChar(VarName), PAnsiChar(VariableType)) = 0 then
    begin
      Writeln(VarName, ': ', VariableType);
    end;
    StrLengthPos := AnsiPos(StrLEN, VariableType);
    Assert(StrLengthPos > 0);
    Inc(StrLengthPos, Length(StrLEN));
    VariableType := Trim(Copy(VariableType, StrLengthPos, MAXINT));
    ParenPos := AnsiPos('(', VariableType);
    if ParenPos > 0 then
    begin
      VarLength := Trim(Copy(VariableType,1,ParenPos-1));
    end
    else
    begin
      VarLength := VariableType;
    end;
    StringLength := StrToInt(VarLength);
    SetLength(TypeNames, 1);
//    SetLength(TypeNamesAddr, 6);

    for NameIndex := 0 to Length(TypeNames) - 1 do
    begin
      SetLength(TypeNames[NameIndex], (StringLength+1)*6);
      for CharIndex := 1 to (StringLength+1)*6 do
      begin
        TypeNames[NameIndex][CharIndex] := AnsiChar(nil);
      end;
//      TypeNamesAddr[NameIndex] := @TypeNames[NameIndex];
    end;
    if get_value_string(PAnsiChar(VarName), @TypeNames[0]) = 0 then
    begin
      for NameIndex := 0 to Length(TypeNames) - 1 do
      begin
        Writeln(Length(TypeNames[NameIndex]));
        ExtractStrArray(TypeNames[NameIndex], NameTypes);
        for NIndex := 0 to Length(NameTypes) - 1 do
        begin
          WriteLn(Trim(NameTypes[NIndex]));
        end;
      end;
    end
    else
    begin
      WriteLn('Failed get_value_string');
    end;
  end;

  if Names.IndexOf('TDIS/ENDOFPERIOD') >= 0 then
  begin
    if get_var_itemsize('TDIS/ENDOFPERIOD', ASize) = 0 then
    begin
      writeln(ASize);
    end
    else
    begin
      writeln('faiure get_var_itemsize');
    end;
    if get_var_nbytes('TDIS/ENDOFPERIOD', ASize) = 0 then
    begin
      writeln(ASize);
    end
    else
    begin
      writeln('faiure get_var_nbytes');
    end;
    SetLength(ABool, 1);
    get_value_bool('TDIS/ENDOFPERIOD', @ABool);
    WriteLn(ABool[0]);
  end
  else
  begin
    WriteLn('FTYPE not found');
  end;

  Exit;
  if Names.IndexOf('__INPUT__/MODFLOW/NAM/FTYPE') >= 0 then
  begin
    get_value_string('__INPUT__/MODFLOW/NAM/FTYPE', @Value);
    WriteLn(Value);
  end
  else
  begin
    WriteLn('FTYPE not found');
  end;
  if Names.IndexOf('__INPUT__/SIM/NAM/TDIS6') >= 0 then
  begin
//    get_value_string('__INPUT__/SIM/NAM/TDIS6', @Value);
//    WriteLn(Value);
  end
  else
  begin
    WriteLn('TDIS not found');
  end;
end;

function GetTimeDiscretization: TStressPeriods;
var
  Nper: Integer;
  NStep: TCIntArray;
  PerLen: TDoubleArray;
  Tsmult: TDoubleArray;
  PeriodIndex: Integer;
begin
  Nper := GetNumberOfPeriods;
  SetLength(Result, Nper);
//  Writeln(Nper);
  NStep := GetNumberOfSteps;
  PerLen := GetPerLen;
  Tsmult := GetTsmult;
  for PeriodIndex := 0 to Nper - 1 do
  begin
    Result[PeriodIndex].Length := PerLen[PeriodIndex];
    Result[PeriodIndex].NStep := NStep[PeriodIndex];
    Result[PeriodIndex].TSMult := Tsmult[PeriodIndex];
  end;
  Writeln(Ord(GetTimeUnits));
end;

function GetDiscretization(ModelName: AnsiString;
  Packages: TPackages): TImportedDiscretization;
var
  PackageIndex: Integer;
  ACell: TCell;
  VIndex: Integer;
  Index: Integer;
//  DisPackageName: AnsiString;
  PackageType: AnsiString;
begin
  result := nil;
//  DisPackageName := '';
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    PackageType := Packages[PackageIndex].PackageType;
    if (PackageType = 'DISU6') or (PackageType = 'DISV6') or (PackageType = 'DIS6') then
    begin
//      DisPackageName := Packages[PackageIndex].PackageName;
      result := TImportedDiscretization.Create(Packages[PackageIndex].PackageName);
//      result.PackageName := DisPackageName;
      result.Mf6GridType := GetModflowGridType(ModelName, result.MFGridShape);
      case result.Mf6GridType of
        gtDISU: result.PackageID := 'DISU6';
        gtDISV: result.PackageID := 'DISV6';
        gtDIS: result.PackageID := 'DIS6';
      end;
      result.Vertices := nil;
      result.Cells := nil;
      result.DelR := nil;
      result.DelC := nil;
      result.GridTop := nil;
      result.GridBottom := nil;
      result.IDomain := nil;
      if result.PackageName <> '' then
      begin
        result.LengthUnit := GetLengthUnit(ModelName, result.PackageName);
        WriteLn(Ord(result.LengthUnit));
        result.XOrigin := GetXOrigin(ModelName, result.PackageName);
        Writeln(result.XOrigin);
        result.YOrigin := GetYOrigin(ModelName, result.PackageName);
        Writeln(result.YOrigin);
        result.RotationAngle := GetGridRotation(ModelName, result.PackageName);
        Writeln(result.RotationAngle);
        case result.Mf6GridType of
          gtDISU:
            Assert(False);
          gtDISV:
            begin
              result.Vertices := GetVertices(ModelName, result.PackageName);
              for Index := 0 to Length(result.Vertices) - 1 do
              begin
                WriteLn(result.Vertices[Index].VertexNumber, ''#9'',
                  result.Vertices[Index].X, ''#9'', result.Vertices[Index].Y);
              end;
              result.Cells := GetDisvCells(ModelName, result.PackageName);
              for Index := 0 to Length(result.Cells) - 1 do
              begin
                ACell := result.Cells[Index];
                Write(ACell.CellNumber, ''#9'', ACell.CellX, ''#9'', ACell.CellY);
                for VIndex := 0 to Length(ACell.VertexNumbers) - 1 do
                begin
                  Write(''#9'', ACell.VertexNumbers[VIndex]);
                end;
                WriteLn;
              end;
            end;
          gtDIS:
            begin
              result.DelR := GetDelR(ModelName, result.PackageName);
              WriteLn('DelR');
              for Index := 0 to Length(result.DelR) - 1 do
              begin
                Write(' ', result.DelR[Index]);
              end;
              Writeln;
              result.DelC := GetDelC(ModelName, result.PackageName);
              WriteLn('DelC');
              for Index := 0 to Length(result.DelC) - 1 do
              begin
                Write(' ', result.DelC[Index]);
              end;
            end;
        end;
        result.GridTop := GetTop(ModelName, result.PackageName);
        result.GridBottom := GetBottom(ModelName, result.PackageName);
        result.IDomain := GetIDomain(ModelName, result.PackageName);
      end;
      Break;
    end;
  end;
end;

function GetNPF(ModelName: AnsiString; Packages: TPackages): TImportedNpf;
var
  PackageIndex: Integer;
begin
  Result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'NPF6' then
    begin
      result := TImportedNpf.Create(Packages[PackageIndex].PackageName);
      break;
    end;
  end;
  if result <> nil then
  begin
    Assert(result.PackageName <> '');
    result.NpfAveraging := GetNpfAveraging(ModelName, result.PackageName);
    Writeln(Ord(result.NpfAveraging));
    result.THICKSTRT := UseTHICKSTRT(ModelName, result.PackageName);
    WriteLn(result.THICKSTRT);
    result.VARIABLECV := UseVARIABLECV(ModelName, result.PackageName);
    if result.VARIABLECV then
    begin
      result.DEWATERED := UseDEWATERED(ModelName, result.PackageName);
      WriteLn(True, result.DEWATERED);
    end
    else
    begin
      result.DEWATERED := False;
      WriteLn(False);
    end;
    result.Perched := UsePerched(ModelName, result.PackageName);
    WriteLn(result.Perched);
    result.REWET := UseREWET(ModelName, result.PackageName);
    if result.REWET then
    begin
      result.WETFCT := GetWETFCT(ModelName, result.PackageName);
      result.IWETIT := GetIWETIT(ModelName, result.PackageName);
      result.IHDWET := GetIHDWET(ModelName, result.PackageName);
      WriteLn(True, result.WETFCT, result.IWETIT, result.IHDWET);
    end
    else
    begin
      result.WETFCT := 0;
      result.IWETIT := 0;
      result.IHDWET := 0;
      WriteLn(False);
    end;
    result.K22OVERK := UseK22OVERK(ModelName, result.PackageName);
    WriteLn(result.K22OVERK);
    result.K33OVERK := UseK33OVERK(ModelName, result.PackageName);
    WriteLn(result.K33OVERK);
    result.XT3D := UseXT3D(ModelName, result.PackageName);
    WriteLn(result.XT3D);
    result.XT3D_RHS := UseXT3D_RHS(ModelName, result.PackageName);
    WriteLn(result.XT3D_RHS);
    result.CellType := GetICELLTYPE(ModelName, result.PackageName);
    result.K := GetK(ModelName, result.PackageName);
    result.K22 := GetK22(ModelName, result.PackageName);
    result.K33 := GetK33(ModelName, result.PackageName);
    result.Angle1 := GetAngle1(ModelName, result.PackageName);
    result.Angle2 := GetAngle2(ModelName, result.PackageName);
    result.Angle3 := GetAngle3(ModelName, result.PackageName);
    result.WetDry := GetWetDry(ModelName, result.PackageName);
  end;
end;

function GetIms(ImsNumber: Integer): TImportedIms;
var
  SimpleMatch: integer;
  ModelerateMatch: Integer;
  ComplexMatch: integer;
const
  Epsilon = 1E-10;
begin
  Result := TImportedIms.Create;
  result.Method := 1;
  SimpleMatch := 0;
  ModelerateMatch := 0;
  ComplexMatch := 0;
  result.PrintOption := GetImsPrintOption(ImsNumber);
  WriteLn(result.PrintOption);
  result.NoPtc := GetImsNoPTC(ImsNumber);
  WriteLn(result.NoPtc);
  result.ATS_OUTER_MAXIMUM_FRACTION := GetImsATS_OUTER_MAXIMUM_FRACTION(ImsNumber);
  WriteLn(result.ATS_OUTER_MAXIMUM_FRACTION);
  result.OUTER_DVCLOSE := GetImsOUTER_DVCLOSE(ImsNumber);
  if Abs(result.OUTER_DVCLOSE - 1E-3) < Epsilon then
  begin
    Inc(SimpleMatch)
  end
  else if Abs(result.OUTER_DVCLOSE - 1E-2) < Epsilon then
  begin
    Inc(ModelerateMatch)
  end
  else if Abs(result.OUTER_DVCLOSE - 1E-1) < Epsilon then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.OUTER_DVCLOSE);
  result.OUTER_MAXIMUM := GetImsOUTER_MAXIMUM(ImsNumber);
  if result.OUTER_MAXIMUM = 25 then
  begin
    Inc(SimpleMatch);
  end
  else if result.OUTER_MAXIMUM = 50 then
  begin
    Inc(ModelerateMatch);
  end
  else if result.OUTER_MAXIMUM = 100 then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.OUTER_MAXIMUM);

  result.UnderRelax := GetImsUNDER_RELAXATION(ImsNumber);
  WriteLn(result.UnderRelax);
  if result.UnderRelax <> 0 then
  begin
    Inc(ModelerateMatch);
    Inc(ComplexMatch);
    result.Gamma := GetImsUNDER_RELAXATION_GAMMA(ImsNumber);
    WriteLn(result.Gamma);
    if result.Gamma = 0 then
    begin
      Inc(ModelerateMatch);
      Inc(ComplexMatch);
    end;
  end;
  if result.UnderRelax = 3 then
  begin
    Inc(ComplexMatch);
    result.UNDER_RELAXATION_THETA := GetImsUNDER_RELAXATION_THETA(ImsNumber);
    WriteLn(result.UNDER_RELAXATION_THETA);
    result.UNDER_RELAXATION_KAPPA := GetImsUNDER_RELAXATION_KAPPA(ImsNumber);
    WriteLn(result.UNDER_RELAXATION_KAPPA);
    result.UNDER_RELAXATION_MOMENTUM := GetImsUNDER_RELAXATION_MOMENTUM(ImsNumber);
    WriteLn(result.UNDER_RELAXATION_MOMENTUM);
  end;
  if result.UnderRelax = 0 then
  begin
    Inc(SimpleMatch);
    result.UNDER_RELAXATION_THETA := 0.2;
    result.UNDER_RELAXATION_KAPPA := 0.7;
    result.UNDER_RELAXATION_MOMENTUM := 0.001;
  end;

  result.BackTrackNumber := GetImsBACKTRACKING_NUMBER(ImsNumber);
  if result.BackTrackNumber = 0 then
  begin
    Inc(SimpleMatch);
    Inc(ModelerateMatch);
  end
  else if result.BackTrackNumber = 20 then
  begin
    Inc(ComplexMatch);
  end;
  Writeln(result.BackTrackNumber);
  if result.BackTrackNumber > 0 then
  begin
    WriteLn(result.BackTrackNumber);
    result.BACKTRACKING_TOLERANCE := GetImsBACKTRACKING_TOLERANCE(ImsNumber);
    WriteLn(result.BACKTRACKING_TOLERANCE);
    result.BACKTRACKING_REDUCTION_FACTOR := GetImsBACKTRACKING_REDUCTION_FACTOR(ImsNumber);
    WriteLn(result.BACKTRACKING_REDUCTION_FACTOR);
    result.BACKTRACKING_RESIDUAL_LIMIT := GetImsBACKTRACKING_RESIDUAL_LIMIT(ImsNumber);
    WriteLn(result.BACKTRACKING_RESIDUAL_LIMIT);
  end
  else
  begin
    result.BACKTRACKING_TOLERANCE := 1E4;
    result.BACKTRACKING_REDUCTION_FACTOR := 0.2;
    result.BACKTRACKING_RESIDUAL_LIMIT := 100;
  end;

  result.INNER_MAXIMUM := GetImsINNER_MAXIMUM(ImsNumber);
  if result.INNER_MAXIMUM = 50 then
  begin
    Inc(SimpleMatch);
  end
  else if result.INNER_MAXIMUM = 100 then
  begin
    Inc(ModelerateMatch);
  end
  else if result.INNER_MAXIMUM = 500 then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.INNER_MAXIMUM);
  
  result.INNER_DVCLOSE := GetImsINNER_DVCLOSE(ImsNumber);
  WriteLn(result.INNER_DVCLOSE);
  if Abs(result.INNER_DVCLOSE - 1E-3) < Epsilon then
  begin
    Inc(SimpleMatch)
  end
  else if Abs(result.INNER_DVCLOSE - 1E-2) < Epsilon then
  begin
    Inc(ModelerateMatch);
  end
  else if Abs(result.INNER_DVCLOSE - 1E-1) < Epsilon then
  begin
    Inc(ComplexMatch);
  end;
  
  result.INNER_RCLOSE := GetImsINNER_RCLOSE(ImsNumber);
  WriteLn(result.INNER_RCLOSE);
  
  result.LINEAR_ACCELERATION := GetImsLINEAR_ACCELERATION(ImsNumber);
  if result.LINEAR_ACCELERATION = 1 then
  begin
    Inc(SimpleMatch)
  end
  else if result.LINEAR_ACCELERATION = 2 then
  begin
    Inc(ModelerateMatch);
    Inc(ComplexMatch);
  end;
  WriteLn(result.LINEAR_ACCELERATION);
  
  result.RELAXATION_FACTOR := GetImsRELAXATION_FACTOR(ImsNumber);
  if Abs(result.RELAXATION_FACTOR - 0) < Epsilon then
  begin
    Inc(SimpleMatch);
    Inc(ComplexMatch);
  end
  else if Abs(result.RELAXATION_FACTOR - 0.97) < Epsilon then
  begin
    Inc(ModelerateMatch);
  end;
  WriteLn(GetImsRELAXATION_FACTOR(ImsNumber));
  
  result.PRECONDITIONER_LEVELS := GetImsPRECONDITIONER_LEVELS(ImsNumber);
  if Abs(result.PRECONDITIONER_LEVELS - 0) < Epsilon then
  begin
    Inc(SimpleMatch);
    Inc(ModelerateMatch);
  end
  else if Abs(result.PRECONDITIONER_LEVELS - 5) < Epsilon then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.PRECONDITIONER_LEVELS);
  
  result.PRECONDITIONER_DROP_TOLERANCE := GetImsPRECONDITIONER_DROP_TOLERANCE(ImsNumber);
  if Abs(result.PRECONDITIONER_DROP_TOLERANCE - 0) < Epsilon then
  begin
    Inc(SimpleMatch);
    Inc(ModelerateMatch);
  end
  else if Abs(result.PRECONDITIONER_DROP_TOLERANCE - 0.0001) < Epsilon then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.PRECONDITIONER_DROP_TOLERANCE);
  
  result.NUMBER_ORTHOGONALIZATIONS := GetImsNUMBER_ORTHOGONALIZATIONS(ImsNumber);
  if result.NUMBER_ORTHOGONALIZATIONS = 0 then
  begin
    Inc(SimpleMatch);
    Inc(ModelerateMatch);
  end
  else if result.NUMBER_ORTHOGONALIZATIONS = 2 then
  begin
    Inc(ComplexMatch);
  end;
  WriteLn(result.NUMBER_ORTHOGONALIZATIONS);

  result.SCALING_METHOD := GetImsSCALING_METHOD(ImsNumber);
  WriteLn(result.SCALING_METHOD);
  result.REORDERING_METHOD := GetImsREORDERING_METHOD(ImsNumber);
  WriteLn(result.REORDERING_METHOD);
  if ModelerateMatch > SimpleMatch then
  begin
    result.Method := 2;
  end;
  if (result.Method = 2) and (ComplexMatch > ModelerateMatch) then
  begin
    result.Method := 3;
  end;
  case result.Method of
    1: WriteLn('Simple');
    2: WriteLn('Moderate');
    3: WriteLn('Complex');
    else Assert(False);
  end;
       
end;

function GetStoragePackage(ModelName: AnsiString; Packages: TPackages;
 var StoPackageName: AnsiString): TImportedStorage;
var
  PackageIndex: Integer;
begin
  result := nil;
  StoPackageName := '';
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'STO6' then
    begin
      StoPackageName := Packages[PackageIndex].PackageName;
      result := TImportedStorage.Create(Packages[PackageIndex].PackageName);
      result.STORAGECOEFFICIENT := GetStoSTORAGECOEFFICIENT(ModelName, result.PackageName);
      WriteLn(result.STORAGECOEFFICIENT);
      result.SS_CONFINED_ONLY := GetStoSS_CONFINED_ONLY(ModelName, result.PackageName);
      WriteLn(result.SS_CONFINED_ONLY);
      result.ICONVERT := GetStoICONVERT(ModelName, result.PackageName);
      result.SS := GetStoSS(ModelName, result.PackageName);
      result.SY := GetStoSY(ModelName, result.PackageName);
      break;
    end;
  end;
  if (StoPackageName <> '') then
  begin
  end;
end;

function GetInitialConditions(ModelName: AnsiString; Packages: TPackages): TImportedInitialConditions;
var
  PackageIndex: Integer;
begin
  Result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'IC6' then
    begin
      result := TImportedInitialConditions.Create(Packages[PackageIndex].PackageName);
      Result.STRT := GetIcSTRT(ModelName, result.PackageName);
      break;
    end;
  end;
end;

procedure GetCell(NodeNumber: Integer; MFGridShape, IDomain: TCIntArray; var MfCell: TMfCell);
var
  Layer: Integer;
//  Row: Integer;
  Col: Integer;
  CellIndex: Integer;
  CellNumber: Integer;
  NLay: Integer;
  NRow: cint;
  NCol: cint;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  case Length(MFGridShape) of
    1:
      begin
        // DISU
        MfCell.Layer := 0;
        MfCell.Row := 0;
        MfCell.Column := 0;
      end;
    2:
      begin
        if DisvCellDict = nil then
        begin
          DisvCellDict := TDisvCellDict.Create;
          Layer := -1;
          Col := -1;
          CellNumber := 0;
          for CellIndex := 0 to Length(IDomain) - 1 do
          begin
            if (CellIndex mod MFGridShape[1]) = 0 then
            begin
              Inc(Layer);
              Col := -1;
            end;
            Inc(Col);
            if IDomain[CellIndex] > 0 then
            begin
              Inc(CellNumber);
              MfCell.Layer := Layer + 1;
              MfCell.Row := 1;
              MfCell.Column := Col + 1;
              DisvCellDict.Add(CellNumber, MfCell)
            end;
          end;
        end;
        // DISV
        if not DisvCellDict.TryGetValue(NodeNumber, MfCell) then
        begin
          MfCell.Layer := 0;
          MfCell.Row := 0;
          MfCell.Column := 0;
        end;
      end;
    3:
      begin
        if DisvCellDict = nil then
        begin
          DisvCellDict := TDisvCellDict.Create;
          // DIS
          Dec(NodeNumber);
          NLay := MFGridShape[0];
          NRow := MFGridShape[1];
          NCol := MFGridShape[2];
          Assert(Length(IDomain) = NLay*NRow*NCol);
          CellNumber := 0;
          CellIndex := 0;
          for LayerIndex := 0 to NLay - 1 do
          begin
            for RowIndex := 0 to NRow - 1 do
            begin
              for ColIndex := 0 to NCol - 1 do
              begin
                if IDomain[CellIndex] > 0 then
                begin
                  Inc(CellNumber);
                  MfCell.Layer := LayerIndex + 1;
                  MfCell.Row := RowIndex+1;
                  MfCell.Column := ColIndex + 1;
                  DisvCellDict.Add(CellNumber, MfCell)
                end;
                Inc(CellIndex);
              end;
            end;
          end;
        end;
        if not DisvCellDict.TryGetValue(NodeNumber, MfCell) then
        begin
          MfCell.Layer := 0;
          MfCell.Row := 0;
          MfCell.Column := 0;
        end;
      end;
  end;
end;

function GetChdPackages(ModelName: AnsiString; Packages: TPackages): TImportedChdList;
var
  PackageIndex: Integer;
  ImportedPackage: TImportedChd;
begin
  result := nil;
//  ChdPackageName := '';
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'CHD6' then
    begin
      if result = nil then
      begin
        result := TImportedChdList.Create;
      end;
      ImportedPackage := TImportedChd.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.PackageName := Packages[PackageIndex].PackageName;
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

function GetRchPackages(ModelName: AnsiString; Packages: TPackages): TImportedRchList;
var
  PackageIndex: Integer;
  ImportedPackage: TImportedRch;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'RCH6' then
    begin
      if result = nil then
      begin
        result := TImportedRchList.Create;
      end;
      ImportedPackage := TImportedRch.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.PackageName := Packages[PackageIndex].PackageName;
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

function GetEvtPackages(ModelName: AnsiString; Packages: TPackages): TImportedEvtList;
var
//  EvtPackageName: AnsiString;
  PackageIndex: Integer;
  ImportedPackage: TImportedEvt;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'EVT6' then
    begin
      if result = nil then
      begin
        result := TImportedEvtList.Create;
      end;
      ImportedPackage := TImportedEvt.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

function GetMawPackages(ModelName: AnsiString; Packages: TPackages): TImportedMawList;
var
  PackageIndex: Integer;
  ImportedPackage: TImportedMaw;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'MAW6' then
    begin
      if result = nil then
      begin
        result := TImportedMawList.Create;
      end;
      ImportedPackage := TImportedMaw.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

function GetGhbPackages(ModelName: AnsiString; Packages: TPackages): TImportedGhbList;
var
  PackageIndex: Integer;
  ImportedGhb: TImportedGhb;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'GHB6' then
    begin
      if result = nil then
      begin
        result := TImportedGhbList.Create;
      end;
      ImportedGhb := TImportedGhb.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedGhb);
      ImportedGhb.GetPackageValues(ModelName);
    end;
  end;
end;

function GetDrnPackages(ModelName: AnsiString; Packages: TPackages): TImportedDrnList;
var
  PackageIndex: Integer;
  ImportedDrn: TImportedDrn;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'DRN6' then
    begin
      if result = nil then
      begin
        result := TImportedDrnList.Create;
      end;
      ImportedDrn := TImportedDrn.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedDrn);
      ImportedDrn.GetPackageValues(ModelName);
    end;
  end;
end;

function GetRivPackages(ModelName: AnsiString; Packages: TPackages): TImportedRivList;
var
  PackageIndex: Integer;
  ImportedPackage: TImportedRiv;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'RIV6' then
    begin
      if result = nil then
      begin
        result := TImportedRivList.Create;
      end;
      ImportedPackage := TImportedRiv.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

function GetWelPackages(ModelName: AnsiString; Packages: TPackages): TImportedWelList;
var
  PackageIndex: Integer;
  ImportedPackage: TImportedWel;
begin
  result := nil;
  for PackageIndex := 0 to Length(Packages) - 1 do
  begin
    if Packages[PackageIndex].PackageType = 'WEL6' then
    begin
      if result = nil then
      begin
        result := TImportedWelList.Create;
      end;
      ImportedPackage := TImportedWel.Create(Packages[PackageIndex].PackageName);
      result.Add(ImportedPackage);
      ImportedPackage.GetPackageValues(ModelName);
    end;
  end;
end;

procedure UpdateChd(ModelName: AnsiString; MFGridShape: TCIntArray;
  IDomain: TCIntArray; ImportedChd: TImportedChd);
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TChdStressPeriod;
begin
  if ImportedChd <> nil then
  begin
    StressPeriod := TChdStressPeriod.Create;
    ImportedChd.StressPeriods.Add(StressPeriod);
    StressPeriod.NodeList := GetNodeList(ModelName, ImportedChd.PackageName);
    StressPeriod.Bound := GetBound(ModelName, ImportedChd.PackageName);
    StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedChd.PackageName);
    StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedChd.PackageName);
    Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.Bound));
    if StressPeriod.BoundNames <> nil then
    begin
      Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
    end;
    if StressPeriod.AuxValues <> nil then
    begin
      Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
        = ImportedChd.Naux);
    end;
    AuxIndex := 0;
    for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
    begin
      GetCell(StressPeriod.NodeList[CellIndex], MFGridShape, IDomain, MfCell);
      Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
        MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Bound[CellIndex]);
      for AIndex := 0 to ImportedChd.Naux - 1 do
      begin
        Write(' ', StressPeriod.AuxValues[AuxIndex]);
        Inc(AuxIndex);
      end;
      if StressPeriod.BoundNames <> nil then
      begin
        Write(' ', StressPeriod.BoundNames[CellIndex]);
      end;
      Writeln;
    end;
  end;
end;

function GetModels(var ModelNames: TAnsiStringArray): TImportedModels;
var
  ModelTypes: TAnsiStringArray;
  ModelIndex: Integer;
  ImportedModel: TImportedModel;
begin
  Result := TImportedModels.Create;
  ModelTypes := GetModelType;
  ModelNames := GetModelName;
  Assert(Length(ModelNames) = Length(ModelTypes));
  for ModelIndex := 0 to Length(ModelNames) - 1 do
  begin
    ImportedModel := TImportedModel.Create;
    Result.Add(ImportedModel);
    ImportedModel.ModelName := ModelNames[ModelIndex];
    ImportedModel.ModelType := ModelTypes[ModelIndex];
    ImportedModel.ModelNumber := ModelIndex+1;
    WriteLn(ModelNames[ModelIndex], ' ', ModelTypes[ModelIndex]);
    ImportedModel.Packages := GetPackages(ModelNames[ModelIndex]);
    ImportedModel.ImportedDis := GetDiscretization(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedNpf := GetNPF(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedIms := GetIms(ImportedModel.ModelNumber);
    ImportedModel.ImportedSTO := GetStoragePackage(ModelNames[ModelIndex],
      ImportedModel.Packages, ImportedModel.StoPackageName);
    ImportedModel.ImportedIc := GetInitialConditions(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedChdList := GetChdPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedGhbList := GetGhbPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedRivList := GetRivPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedWelList := GetWelPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedDrnList := GetDrnPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedRchList := GetRchPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedEvtList := GetEvtPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
    ImportedModel.ImportedMawList := GetMawPackages(ModelNames[ModelIndex],
      ImportedModel.Packages);
  end;
end;

procedure ImportModel;
var
  version : PAnsiChar;
  vstr : TMfName;
  component : PAnsiChar;
  cname : TMfComponentName;
  GridType : TGridType;
  cnt : cint;
  GridID: cint;
  GridShape: TGridShape;
  Names: array of AnsiChar;
  NameIndex: Integer;
  NameBuilder: TStringBuilder;
//  NameList2: TStringList;
  CharIndex: Integer;
  AChar: AnsiChar;
  AName: string;
  NperID: array[0..255] of AnsiChar;
  NperIDP: PAnsiChar;
  NperIDArray: cintArray;
  varNames: TVarNames;

  ErrorMessage: TErrorMessage;
//  NameStart: Integer;
  currentTime: double;
//  rank: cint;
  GridSize: cint;
  Index: Integer;
  InnerIndex: Integer;
  X, Y: array of double;
  NVertString: AnsiString;
  NCPL_String: AnsiString;
  NVERT: cint;
  NCPL: cint;
  DISV_NCVERT: AnsiString;
  DISV_ICVERT: AnsiString;
  DISV_XC: AnsiString;
  DISV_YC: AnsiString;
  MODEL_SHAPE: AnsiString;
  DIS_TOP: AnsiString;
  DIS_BOT: AnsiString;
  DIS_LENUNI: AnsiString;
  NPF_K: AnsiString;
  VertPerCell: array of cint;
  Verticies: array of cint;
  VertLength: integer;
  VertIndex: Integer;
  ModelShapeArray: array of cint;
  DimCount: Integer;
  Mf6GridType: TMf6GridType;
  ModelTop: array of Double;
  ModelBottom: array of Double;
  ArrayLength: Integer;
//  LenUni: cint;
  LenUniArray: array of cint;
//  k: array of Double;
  Mode: array of cint;
  ModeString: AnsiString;
  VariableName: AnsiString;
  VariableType: AnsiString;
  Rank: Integer;
  RankIndex: Integer;
  ModelNames: TAnsiStringArray;
  MFGridShape: TCIntArray;
  Packages: TPackages;
//  PackageID: Ansistring;
  K: TDoubleArray;
  ImsNumber: Integer;
  StoPackageName: AnsiString;
//  current_time: Double;
//  TimeStep: Double;
  NStep: TCIntArray;
  PeriodIndex: Integer;
  StepIndex: Integer;
  NewStressPeriodTransientDataRead: Integer;
  Transient: Boolean;
  TDIS: TStressPeriods;
//  PackageIndex: Integer;
  SetMode: Boolean;
  IDomain: TCIntArray;
  ImportedChd: TImportedChd;
  ImportedDis: TImportedDiscretization;
  ImportedNpf: TImportedNpf;
  ImportedIms: TImportedIms;
  ImportedSTO: TImportedStorage;
  ImportedIc: TImportedInitialConditions;
  ImportedModels: TImportedModels;
  ModelIndex: Integer;
  ImportedChdList: TImportedChdList;
begin
  with TFileOpenDialog.Create(nil) do
  try
    Options := [fdoPickFolders];
    if Execute then
    begin
     SetCurrentDir(FileName);
    end
    else
    begin
      Exit;
    end;
  finally
    Free;
  end;

  version := @vstr;
  component := @cname;
//  PGridType := @GridType;
  writeln ('Hello, mf6 world.');
  initialize();
  get_version(vstr);
  get_component_name(cname);
  writeln('Component name: ' + cname);
  writeln('MF6 DLL version: ' + version);
  writeln('MF6 DLL component: ' + component);
//  count := @cnt;
  get_output_item_count(cnt);
  write('MF6 output item count: ');
  writeln(cnt);

  SetLength(Names, (cnt+1)*BMI_LENVARADDRESS);
  SetLength(varNames,  cnt+1);
//
//  NamesP := @Names;
  get_output_var_names(Addr(Names[0]));

  NVertString := '';
  NCPL_String := '';
  DISV_NCVERT := '';
  DISV_ICVERT := '';
  DISV_XC := '';
  DISV_YC := '';
  MODEL_SHAPE := '';
  DIS_TOP := '';
  DIS_BOT := '';
  DIS_LENUNI := '';
  NPF_K := '';
  ImportedModels := nil;
  ImportedDis := nil;
  NameBuilder := TStringBuilder.Create;
  NameList := TAnsiStringList.Create;
//  NameList2 := TStringList.Create;
  try
    for CharIndex := 0 to Length(Names) - 1 do
    begin
      AChar := Names[CharIndex];
      if Ord(AChar) = 0 then
      begin
        AName := NameBuilder.ToString;
        if AName <> '' then
        begin
          NameList.Add(AnsiString(AName));
          NameBuilder.Clear;
        end;
      end
      else
      begin
        NameBuilder.Append(AChar);
      end;
    end;
    for NameIndex := 0 to NameList.Count - 1 do
    begin
      Writeln(NameList[NameIndex]);
    end;
        GetSimulationValues(NameList);

    // Set mode
    SetMode := True;
    if SetMode then
    begin
      if (NameList.Count > 0) and (NameList[0] = 'SIM/ISIM_MODE') then
      begin
        SetLength(Mode, 1);
        ModeString := AnsiString(NameList[0]);
//        ModeString := 'SIM/ISIM_MODE';
        if get_value_int(PAnsiChar(ModeString), @Mode) = 0 then
        begin
          Writeln('Mode: ', Mode[0]);
          Mode[0] := 0;
          if set_value_int(PAnsiChar(ModeString), @Mode) = 0 then
          begin
            Writeln('success Setting Mode: ');
          end
          else
          begin
            Writeln('Error Setting Mode: ');
            get_last_bmi_error(@ErrorMessage);
            Writeln(ErrorMessage);
          end;
        end
        else
        begin
          Writeln('Error Getting Mode: ');
          get_last_bmi_error(@ErrorMessage);
          Writeln(ErrorMessage);
        end;
      end;
    end;
    ImportedModels := GetModels(ModelNames);

    TDIS := GetTimeDiscretization;
//    GetStringVariable('MODFLOW/CHD-1/BOUNDNAME_CST');

//    update();

//    GetSimulationValues(NameList);
//    GetSimulationInputs(NameList);
    Packages := GetPackages(ModelNames[0]);
    Mf6GridType := GetModflowGridType(ModelNames[0], MFGridShape);
    Writeln(Ord(Mf6GridType));
    ImportedDis := GetDiscretization(ModelNames[0], Packages);
    if ImportedDis = nil then
    begin
      IDomain := nil;
    end
    else
    begin
      IDomain := ImportedDis.IDomain;
    end;
    ImportedNpf := GetNPF(ModelNames[0], Packages);

    ImsNumber := 1;
    ImportedIms := GetIms(ImsNumber);
    ImportedSTO := GetStoragePackage(ModelNames[0], Packages, StoPackageName);
    ImportedIc := GetInitialConditions(ModelNames[0], Packages);

    ImportedChdList := GetChdPackages(ModelNames[0], Packages);
    if ImportedChdList <> nil then
    begin
      ImportedChd := ImportedChdList[0];
    end
    else
    begin
      ImportedChd := nil;
    end;

    NStep := GetNumberOfSteps;
//    NewStressPeriodTransientDataRead := 1;

    Transient := False;
    for PeriodIndex := 0 to Length(NStep) - 1 do
    begin
      for StepIndex := 0 to NStep[PeriodIndex] - 1 do
      begin
        if not GetEndOfSimulation then
        begin
          update;
        end;

        GetSimulationValues(NameList);
        if StepIndex = 0 then
        begin
          for ModelIndex := 0 to ImportedModels.Count - 1 do
          begin
            ImportedModels[ModelIndex].UpdateChd;
            ImportedModels[ModelIndex].UpdateGhb;
            ImportedModels[ModelIndex].UpdateRiv;
            ImportedModels[ModelIndex].UpdateWel;
            ImportedModels[ModelIndex].UpdateDrn;
            ImportedModels[ModelIndex].UpdateRch;
            ImportedModels[ModelIndex].UpdateEvt;
            ImportedModels[ModelIndex].UpdateMaw;
          end;
        end;

        if (StepIndex = NStep[PeriodIndex] - 1) then
        begin
          if StoPackageName <> '' then
          begin
            NewStressPeriodTransientDataRead :=
              GetNewDataRead(ModelNames[0], StoPackageName);

            if NewStressPeriodTransientDataRead = PeriodIndex+1 then
            begin
              Transient := GetTransient(ModelNames[0]);
            end;
            TDIS[PeriodIndex].Transient := Transient;
          end
          else
          begin
            TDIS[PeriodIndex].Transient := False;
          end;
        end;
      end;
    end;
    for PeriodIndex := 0 to Length(TDIS) - 1 do
    begin
      if TDIS[PeriodIndex].Transient then
      begin
        Writeln(TDIS[PeriodIndex].Length, ' ', TDIS[PeriodIndex].NStep, ' ',
           TDIS[PeriodIndex].TSMult, ' Transient')
      end
      else
      begin
        Writeln(TDIS[PeriodIndex].Length, ' ', TDIS[PeriodIndex].NStep, ' ',
           TDIS[PeriodIndex].TSMult, ' Steady-State')
      end;
    end;


//    ModelName

    for Index := 0 to NameList.Count - 1 do
    begin
      VariableName := AnsiString(NameList[Index]);
      SetLength(VariableType, BMI_LENVARTYPE);
      for CharIndex := 1 to BMI_LENVARTYPE do
      begin
        VariableType[CharIndex] := AnsiChar(nil);
      end;
      if get_var_type(PAnsiChar(VariableName), PAnsiChar(VariableType)) = 0 then
      begin
        WriteLn('Variable Type: ', VariableName, ' = ', VariableType);
      end
      else
      begin
        WriteLn('Failure get_var_type at Index = ', Index);
        get_last_bmi_error(@ErrorMessage);
        Writeln(ErrorMessage);
        break
      end;
      if get_var_rank(PAnsiChar(VariableName), Rank) = 0 then
      begin
        WriteLn('Variable Rank: ', VariableName, ' = ', Rank);
        SetLength(ModelShapeArray,Rank);
        for RankIndex := 0 to Length(ModelShapeArray) - 1 do
        begin
          ModelShapeArray[RankIndex] := Mf6Undefined;
        end;
        if Rank > 0 then
        begin
          if get_var_shape(PAnsiChar(VariableName), @ModelShapeArray[0]) = 0 then
          begin
            for RankIndex := 0 to Length(ModelShapeArray) - 1 do
            begin
              WriteLn(ModelShapeArray[RankIndex]);
            end;
          end
          else
          begin
            WriteLn('Failure get_var_shape at Index = ', Index);
          end;
        end;
      end
      else
      begin
        WriteLn('Failure get_var_rank at Index = ', Index);
        get_last_bmi_error(@ErrorMessage);
        Writeln(ErrorMessage);
//        if Pos('0', VariableType) <= 0 then
//        begin
//          break
//        end;
      end;
    end;

//    for NameIndex := 0 to cnt - 1 do
//    begin
//      NameStart := NameIndex * BMI_LENVARADDRESS;
//      AName := string(PAnsiChar(Addr(Names[NameStart])));
//      if Pos('/DISV/NVERT', AName) > 0 then
//      begin
//        NVertString := AnsiString(AName);
//      end;
//      if Pos('/DISV/NCPL', AName) > 0 then
//      begin
//        NCPL_String := AnsiString(AName);
//      end;
//      if Pos('/DISV/NCVERT', AName) > 0 then
//      begin
//        DISV_NCVERT := AnsiString(AName);
//      end;
//      if Pos('/DISV/ICVERT', AName) > 0 then
//      begin
//        DISV_ICVERT := AnsiString(AName);
//      end;
//      if Pos('/DISV/XC', AName) > 0 then
//      begin
//        DISV_XC := AnsiString(AName);
//      end;
//      if Pos('/DISV/YC', AName) > 0 then
//      begin
//        DISV_YC := AnsiString(AName);
//      end;
//      if Pos('MODEL_SHAPE', AName) > 0 then
//      begin
//        MODEL_SHAPE := AnsiString(AName);
//      end;
//      if Pos('/DIS/TOP', AName) > 0 then
//      begin
//        DIS_TOP := AnsiString(AName);
//      end;
//      if Pos('/DIS/BOT', AName) > 0 then
//      begin
//        DIS_BOT := AnsiString(AName);
//      end;
//      if Pos('/DIS/LENUNI', AName) > 0 then
//      begin
//        DIS_LENUNI := AnsiString(AName);
//      end;
//      if (Pos('/NPF/K', AName) > 0) and (AName[Length(AName)] = 'K') then
//      begin
//        NPF_K := AnsiString(AName);
//      end;
//
//
//
//      NameList2.Add(AName);
//    end;
//    Assert(NameList.Count = NameList2.Count);
//    Assert(NameList.CommaText = NameList2.CommaText);
  finally
//    NameList2.Free;
    NameList.Free;
    NameBuilder.Free;
    DisvCellDict.Free;
    DisvCellDict := nil;

    ImportedModels.Free;
    ImportedIc.Free;
    ImportedSTO.Free;
    ImportedIms.Free;
    ImportedNpf.Free;
    ImportedDis.Free;
    ImportedChd.Free;
  end;

  if NPF_K <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
//    ArrayLength := 1;
    if get_var_shape(PAnsiChar(NPF_K), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('k dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;
      SetLength(K, ArrayLength);
      if get_value_double(PAnsiChar(NPF_K), @K) = 0 then
      begin
        for Index := 0 to Length(K) - 1 do
        begin
          WriteLn(Index, ' ', K[Index]);
        end;
      end;
    end;
  end;

  if DIS_LENUNI <> '' then
  begin
    SetLength(LenUniArray, 1);
    if get_value_int(PAnsiChar(DIS_LENUNI), @LenUniArray) = 0 then
    begin
      Writeln('LENUNI: ', LenUniArray[0]);
    end;
  end;


  if DIS_TOP <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
//    ArrayLength := 1;
    if get_var_shape(PAnsiChar(DIS_TOP), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('Top dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;

      SetLength(ModelTop, ArrayLength);
      if get_value_double(PAnsiChar(DIS_TOP), @ModelTop) = 0 then
      begin
        for Index := 0 to Length(ModelTop) - 1 do
        begin
          WriteLn(Index, ' ', ModelTop[Index]);
        end;
      end;
    end;
  end;

  if DIS_BOT <> '' then
  begin
    SetLength(ModelShapeArray,MAXMEMRANK);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    if get_var_shape(PAnsiChar(DIS_BOT), @ModelShapeArray[0]) = 0 then
    begin
      ArrayLength := 1;
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Writeln('Botm dimension: ', Index, ' ', ModelShapeArray[Index]);
          ArrayLength := ArrayLength * ModelShapeArray[Index]
        end;
      end;
      SetLength(ModelBottom, ArrayLength);
      if get_value_double(PAnsiChar(DIS_BOT), @ModelBottom) = 0 then
      begin
        for Index := 0 to Length(ModelBottom) - 1 do
        begin
          WriteLn(Index, ' ', ModelBottom[Index]);
        end;
      end;
    end;
  end;


  DimCount := 0;
  if MODEL_SHAPE <> '' then
  begin
    SetLength(ModelShapeArray,3);
    for Index := 0 to Length(ModelShapeArray) - 1 do
    begin
      ModelShapeArray[Index] := Mf6Undefined;
    end;
    if get_value_int(PAnsiChar(MODEL_SHAPE), @ModelShapeArray) = 0 then
    begin
      for Index := 0 to Length(ModelShapeArray) - 1 do
      begin
        if ModelShapeArray[Index] <> Mf6Undefined then
        begin
          Inc(DimCount);
        end;
      end;
    end;
    Mf6GridType := TMf6GridType(DimCount-1);
  end;

  NVERT := 0;
  SetLength(NperIDArray, 1);
  if NVertString <> '' then
  begin
    if get_value_int(PAnsiChar(NVertString), @NperIDArray) = 0 then
    begin
      Writeln('Success');
      NVERT := NperIDArray[0];
    end
  end;

  NCPL := 0;
  if NCPL_String <> '' then
  begin
    if get_value_int(PAnsiChar(NCPL_String), @NperIDArray) = 0 then
    begin
      Writeln('Success');
      NCPL := NperIDArray[0];
    end
  end;

  if (NCPL > 0) and (DISV_NCVERT <> '') and (DISV_ICVERT <> '')
    and (DISV_XC <> '') and (DISV_YC <> '') then
  begin
    SetLength(VertPerCell, NCPL);
    SetLength(X, NCPL);
    SetLength(Y, NCPL);
    if (get_value_int(PAnsiChar(DISV_NCVERT), @VertPerCell) = 0)
      and (get_value_double(PAnsiChar(DISV_XC), @X) = 0)
      and (get_value_double(PAnsiChar(DISV_YC), @Y) = 0) then
    begin
      Writeln('Success');
      VertLength := 0;
      for Index := 0 to Length(VertPerCell) - 1 do
      begin
        VertLength := VertLength + VertPerCell[Index];
      end;
      SetLength(Verticies, VertLength);
      if get_value_int(PAnsiChar(DISV_ICVERT), @Verticies) = 0 then
      begin
        Writeln('Success');
        VertIndex := 0;
        for Index := 0 to Length(VertPerCell) - 1 do
        begin
          Write(Index,' ', VertPerCell[Index], ' ', X[Index], ' ', Y[Index]);
          for InnerIndex := 0 to VertPerCell[Index] - 1 do
          begin
            Write(' ', Verticies[VertIndex]);
            Inc(VertIndex);
          end;
          Writeln;
        end;
      end;
    end
  end;

  SetLength(NperIDArray, 1);
  NperID := 'TDIS/NPER';
  NperIDP := @NperID;
  if get_value_int(NperIDP, @NperIDArray) = 0 then
  begin
    Writeln('Success');
  end
  else
  begin
    get_last_bmi_error(@ErrorMessage);
    Writeln(ErrorMessage);
  end;
  writeln('NPER: ', NperIDArray[0]);

  GridID := 1;
  if get_grid_type(@GridID, GridType) = 0 then
  begin
    writeln('grid type: ' + GridType);
  end
  else
  begin
    writeln('Failure');
  end;

  if get_grid_face_count(@GridID, cnt) = 0 then
  begin
  writeln('face count: ', cnt);
  end
  else
  begin
    writeln('Failure');
  end;

  GridShape[0] := Mf6Undefined;
  GridShape[1] := Mf6Undefined;
  GridShape[2] := Mf6Undefined;

//  GridShapeP := @GridShape;
  if get_grid_shape(@GridID, GridShape) = 0 then
  begin
    WriteLn('GridShape[0], NLAY: ', GridShape[0]);
    WriteLn('GridShape[1], NROW: ', GridShape[1]);
    WriteLn('GridShape[2], NCOL: ', GridShape[2]);
    GridSize := 1;
    for Index := 0 to Length(GridShape) - 1 do
    begin
      if GridShape[Index] <> Mf6Undefined then
      begin
        GridSize := GridSize * GridShape[Index]
      end;
    end;
    writeln('GridSize: ', GridSize);
  end
  else
  begin
    writeln('Failure');
  end;
  get_current_time(currentTime);
  Writeln('Current time: ',  currentTime);
  get_end_time(currentTime);
  Writeln('end time: ',  currentTime);

  SetLength(X, NVERT);
  SetLength(Y, NVERT);
  for Index := 0 to Length(X) - 1 do
  begin
    X[Index] := Mf6Undefined;
    Y[Index] := Mf6Undefined;
  end;

  GridID := 1;
  if (NVERT > 0) and (get_grid_x(@GridID, Addr(X[0])) = 0)
    and (get_grid_y(@GridID, Addr(Y[0])) = 0) then
  begin
    for Index := 0 to Length(X) - 1 do
    begin
      if X[Index] = Mf6Undefined then
      begin
        Assert(False)
      end;
      if Y[Index] = Mf6Undefined then
      begin
        Assert(False)
      end;
      writeln(Index, X[Index], Y[Index]);
    end;
  end
  else
  begin
    writeln('Failure get_grid_x');
  end;

  GridID := 1;
  GridSize := 0;
  if get_grid_size(@GridID, GridSize) = 0 then
  begin
    // grid size should not be zero.
    writeln('grid size: ', GridSize);
  end
  else
  begin
    writeln('Failure get_grid_size');
  end;

  if not GetEndOfSimulation then
  begin
    update;
  end;

  get_output_item_count(cnt);
  write('MF6 output item count: ');
  writeln(cnt);

  get_current_time(currentTime);
  Writeln('Current time: ',  currentTime);

  finalize();
end;

{ TImportedModel }

destructor TImportedModel.Destroy;
begin
  ImportedMawList.Free;
  ImportedEvtList.Free;
  ImportedRchList.Free;
  ImportedDrnList.Free;
  ImportedWelList.Free;
  ImportedRivList.Free;
  ImportedGhbList.Free;
  ImportedChdList.Free;
  ImportedIc.Free;
  ImportedSTO.Free;
  ImportedDis.Free;
  ImportedNpf.Free;
  ImportedIms.Free;
  inherited;
end;

procedure TImportedModel.UpdateChd;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TChdStressPeriod;
  PackageIndex: Integer;
  ImportedChd: TImportedChd;
begin
  if ImportedChdList <> nil then
  begin
    for PackageIndex := 0 to ImportedChdList.Count - 1 do
    begin
      ImportedChd := ImportedChdList[PackageIndex];
      StressPeriod := TChdStressPeriod.Create;
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedChd.PackageName);
      ImportedChd.StressPeriods.Add(StressPeriod);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedChd.PackageName);
      StressPeriod.Bound := GetBound(ModelName, ImportedChd.PackageName);
      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedChd.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedChd.PackageName);
      Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.Bound));
      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedChd.Naux);
      end;
      AuxIndex := 0;
      if ImportedDis <> nil then
      begin
        for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
        begin
          GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
          Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
            MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Bound[CellIndex]);
          for AIndex := 0 to ImportedChd.Naux - 1 do
          begin
            Write(' ', StressPeriod.AuxValues[AuxIndex]);
            Inc(AuxIndex);
          end;
          if StressPeriod.BoundNames <> nil then
          begin
            Write(' ', StressPeriod.BoundNames[CellIndex]);
          end;
          Writeln;
        end;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateDrn;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TDrnStressPeriod;
  PackageIndex: Integer;
  ImportedDrn: TImportedDrn;
begin
  if ImportedDrnList <> nil then
  begin
    for PackageIndex := 0 to ImportedDrnList.Count - 1 do
    begin
      ImportedDrn := ImportedDrnList[PackageIndex];
      StressPeriod := TDrnStressPeriod.Create;
      ImportedDrn.StressPeriods.Add(StressPeriod);
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedDrn.PackageName);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedDrn.PackageName);

      StressPeriod.Bound := GetBound(ModelName, ImportedDrn.PackageName);

      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedDrn.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedDrn.PackageName);

      Assert(Length(StressPeriod.NodeList)*2 = Length(StressPeriod.Bound));

      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedDrn.Naux);
      end;
      AuxIndex := 0;
      for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
      begin
        GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
        Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
          MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Elev(CellIndex),
          ' ', StressPeriod.Cond(CellIndex));
        for AIndex := 0 to ImportedDrn.Naux - 1 do
        begin
          Write(' ', StressPeriod.AuxValues[AuxIndex]);
          Inc(AuxIndex);
        end;
        if StressPeriod.BoundNames <> nil then
        begin
          Write(' ', StressPeriod.BoundNames[CellIndex]);
        end;
        Writeln;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateEvt;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TEvtStressPeriod;
  PackageIndex: Integer;
  ImportedEvt: TImportedEvt;
  SegIndex: Integer;
begin
  if ImportedEvtList <> nil then
  begin
    for PackageIndex := 0 to ImportedEvtList.Count - 1 do
    begin
      ImportedEvt := ImportedEvtList[PackageIndex];
      StressPeriod := TEvtStressPeriod.Create;
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedEvt.PackageName);
      StressPeriod.NSEG := ImportedEvt.NSEG;

      ImportedEvt.StressPeriods.Add(StressPeriod);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedEvt.PackageName);

      StressPeriod.Bound := GetBound(ModelName, ImportedEvt.PackageName);
      StressPeriod.SURF_RATE_SPECIFIED := (Length(StressPeriod.Bound) div ImportedEvt.MAXBOUND)
        - 3 - (ImportedEvt.NSEG -1)*2;
      Assert(StressPeriod.SURF_RATE_SPECIFIED in [0,1]);
      StressPeriod.ValuesPerBoundary := 3 + (ImportedEvt.NSEG -1)*2
        + StressPeriod.SURF_RATE_SPECIFIED;

      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedEvt.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedEvt.PackageName);

//      Assert(Length(StressPeriod.NodeList)*2 = Length(StressPeriod.Bound));

      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedEvt.Naux);
      end;
      AuxIndex := 0;
      for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
      begin
        GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
        Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
          MfCell.Row, ' ', MfCell.Column,
          ' ', StressPeriod.Surface(CellIndex),
          ' ', StressPeriod.Rate(CellIndex),
          ' ', StressPeriod.Depth(CellIndex));
        for SegIndex := 0 to StressPeriod.NSEG - 2 do
        begin
          Write(
            ' ', StressPeriod.Pxdp(CellIndex, SegIndex),
            ' ', StressPeriod.Petm(CellIndex, SegIndex));
        end;
        if StressPeriod.SURF_RATE_SPECIFIED <> 0 then
        begin
          Write(' ', StressPeriod.Petm0(CellIndex));
        end;
        for AIndex := 0 to ImportedEvt.Naux - 1 do
        begin
          Write(' ', StressPeriod.AuxValues[AuxIndex]);
          Inc(AuxIndex);
        end;
        if StressPeriod.BoundNames <> nil then
        begin
          Write(' ', StressPeriod.BoundNames[CellIndex]);
        end;
        Writeln;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateGhb;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TGhbStressPeriod;
  PackageIndex: Integer;
  ImportedGhb: TImportedGhb;
begin
  if ImportedGhbList <> nil then
  begin
    for PackageIndex := 0 to ImportedGhbList.Count - 1 do
    begin
      ImportedGhb := ImportedGhbList[PackageIndex];
      StressPeriod := TGhbStressPeriod.Create;
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedGhb.PackageName);
      ImportedGhb.StressPeriods.Add(StressPeriod);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedGhb.PackageName);

      StressPeriod.Bound := GetBound(ModelName, ImportedGhb.PackageName);

      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedGhb.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedGhb.PackageName);

      Assert(Length(StressPeriod.NodeList)*2 = Length(StressPeriod.Bound));

      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedGhb.Naux);
      end;
      AuxIndex := 0;
      for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
      begin
        GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
        Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
          MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.BHead(CellIndex),
          ' ', StressPeriod.Cond(CellIndex));
        for AIndex := 0 to ImportedGhb.Naux - 1 do
        begin
          Write(' ', StressPeriod.AuxValues[AuxIndex]);
          Inc(AuxIndex);
        end;
        if StressPeriod.BoundNames <> nil then
        begin
          Write(' ', StressPeriod.BoundNames[CellIndex]);
        end;
        Writeln;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateMaw;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TMawStressPeriod;
  PackageIndex: Integer;
  ImportedMaw: TImportedMaw;
begin
  if ImportedMawList <> nil then
  begin
    for PackageIndex := 0 to ImportedMawList.Count - 1 do
    begin
      ImportedMaw := ImportedMawList[PackageIndex];
      StressPeriod := TMawStressPeriod.Create;
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedMaw.PackageName);
      ImportedMaw.StressPeriods.Add(StressPeriod);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedMaw.PackageName);

      StressPeriod.Bound := GetBound(ModelName, ImportedMaw.PackageName);
      StressPeriod.Rate := GetRate(ModelName, ImportedMaw.PackageName);
      StressPeriod.fwelev := GetFwElev(ModelName, ImportedMaw.PackageName);
      StressPeriod.fwcond := GetFwCond(ModelName, ImportedMaw.PackageName);
      StressPeriod.fwrlen := GetFwRlen(ModelName, ImportedMaw.PackageName);
      StressPeriod.WELL_HEAD := GetWELL_HEAD(ModelName, ImportedMaw.PackageName);
      StressPeriod.HEAD_LIMIT := GetHEAD_LIMIT(ModelName, ImportedMaw.PackageName);
      StressPeriod.minrate := GetMinRate(ModelName, ImportedMaw.PackageName);
      StressPeriod.maxrate := GetMaxRate(ModelName, ImportedMaw.PackageName);
      StressPeriod.pump_elevation := GetPumpElevation(ModelName, ImportedMaw.PackageName);
      StressPeriod.scaling_length := GetScalingLength(ModelName, ImportedMaw.PackageName);

      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedMaw.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedMaw.PackageName);

//      Assert(Length(StressPeriod.NodeList)*3 = Length(StressPeriod.Bound));

      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedMaw.Naux);
      end;
      AuxIndex := 0;
      for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
      begin
        GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
//        Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
//          MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Stage(CellIndex),
//          ' ', StressPeriod.Cond(CellIndex),  ' ', StressPeriod.RBot(CellIndex));
        for AIndex := 0 to ImportedMaw.Naux - 1 do
        begin
          Write(' ', StressPeriod.AuxValues[AuxIndex]);
          Inc(AuxIndex);
        end;
        if StressPeriod.BoundNames <> nil then
        begin
          Write(' ', StressPeriod.BoundNames[CellIndex]);
        end;
        Writeln;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateRch;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TRchStressPeriod;
  PackageIndex: Integer;
  ImportedRch: TImportedRch;
begin
  if ImportedRchList <> nil then
  begin
    for PackageIndex := 0 to ImportedRchList.Count - 1 do
    begin
      ImportedRch := ImportedRchList[PackageIndex];
      StressPeriod := TRchStressPeriod.Create;
      ImportedRch.StressPeriods.Add(StressPeriod);
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedRch.PackageName);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedRch.PackageName);
      StressPeriod.Bound := GetBound(ModelName, ImportedRch.PackageName);
      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedRch.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedRch.PackageName);
      Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.Bound));
      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedRch.Naux);
      end;
      AuxIndex := 0;
      if ImportedDis <> nil then
      begin
        for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
        begin
          GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
          Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
            MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Bound[CellIndex]);
          for AIndex := 0 to ImportedRch.Naux - 1 do
          begin
            Write(' ', StressPeriod.AuxValues[AuxIndex]);
            Inc(AuxIndex);
          end;
          if StressPeriod.BoundNames <> nil then
          begin
            Write(' ', StressPeriod.BoundNames[CellIndex]);
          end;
          Writeln;
        end;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateRiv;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TRivStressPeriod;
  PackageIndex: Integer;
  ImportedRiv: TImportedRiv;
begin
  if ImportedRivList <> nil then
  begin
    for PackageIndex := 0 to ImportedRivList.Count - 1 do
    begin
      ImportedRiv := ImportedRivList[PackageIndex];
      StressPeriod := TRivStressPeriod.Create;
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedRiv.PackageName);
      ImportedRiv.StressPeriods.Add(StressPeriod);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedRiv.PackageName);

      StressPeriod.Bound := GetBound(ModelName, ImportedRiv.PackageName);

      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedRiv.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedRiv.PackageName);

      Assert(Length(StressPeriod.NodeList)*3 = Length(StressPeriod.Bound));

      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedRiv.Naux);
      end;
      AuxIndex := 0;
      for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
      begin
        GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape, ImportedDis.IDomain, MfCell);
        Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
          MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Stage(CellIndex),
          ' ', StressPeriod.Cond(CellIndex),  ' ', StressPeriod.RBot(CellIndex));
        for AIndex := 0 to ImportedRiv.Naux - 1 do
        begin
          Write(' ', StressPeriod.AuxValues[AuxIndex]);
          Inc(AuxIndex);
        end;
        if StressPeriod.BoundNames <> nil then
        begin
          Write(' ', StressPeriod.BoundNames[CellIndex]);
        end;
        Writeln;
      end;
    end;
  end;
end;

procedure TImportedModel.UpdateWel;
var
  AuxIndex: Integer;
  CellIndex: Integer;
  MfCell: TMfCell;
  AIndex: Integer;
  StressPeriod: TWelStressPeriod;
  PackageIndex: Integer;
  ImportedWel: TImportedWel;
begin
  if ImportedWelList <> nil then
  begin
    for PackageIndex := 0 to ImportedWelList.Count - 1 do
    begin
      ImportedWel := ImportedWelList[PackageIndex];
      StressPeriod := TWelStressPeriod.Create;
      ImportedWel.StressPeriods.Add(StressPeriod);
      StressPeriod.NBOUND := GetNBOUND(ModelName, ImportedWel.PackageName);
      StressPeriod.NodeList := GetNodeList(ModelName, ImportedWel.PackageName);
      StressPeriod.Bound := GetBound(ModelName, ImportedWel.PackageName);
      StressPeriod.BoundNames := GetBoundNames(ModelName, ImportedWel.PackageName);
      StressPeriod.AuxValues := GetAuxValue(ModelName, ImportedWel.PackageName);
      Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.Bound));
      if StressPeriod.BoundNames <> nil then
      begin
        Assert(Length(StressPeriod.NodeList) = Length(StressPeriod.BoundNames));
      end;
      if StressPeriod.AuxValues <> nil then
      begin
        Assert((Length(StressPeriod.AuxValues) div Length(StressPeriod.NodeList))
          = ImportedWel.Naux);
      end;
      AuxIndex := 0;
      if ImportedDis <> nil then
      begin
        for CellIndex := 0 to Length(StressPeriod.NodeList) - 1 do
        begin
          GetCell(StressPeriod.NodeList[CellIndex], ImportedDis.MFGridShape,
            ImportedDis.IDomain, MfCell);
          Write(StressPeriod.NodeList[CellIndex], ' ', MfCell.Layer, ' ',
            MfCell.Row, ' ', MfCell.Column, ' ', StressPeriod.Bound[CellIndex]);
          for AIndex := 0 to ImportedWel.Naux - 1 do
          begin
            Write(' ', StressPeriod.AuxValues[AuxIndex]);
            Inc(AuxIndex);
          end;
          if StressPeriod.BoundNames <> nil then
          begin
            Write(' ', StressPeriod.BoundNames[CellIndex]);
          end;
          Writeln;
        end;
      end;
    end;
  end;
end;

{ TImportedChd }

constructor TImportedChd.Create;
begin
  inherited;
  StressPeriods := TChdStressPeriods.Create;
end;

destructor TImportedChd.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

{ TImportedGhb }

constructor TImportedGhb.Create;
begin
  inherited;
  StressPeriods := TGhbStressPeriods.Create
end;

destructor TImportedGhb.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

{ TGhbStressPeriod }

function TGhbStressPeriod.BHead(CellIndex: Integer): double;
begin
  result := Bound[CellIndex*2];
end;

function TGhbStressPeriod.Cond(CellIndex: Integer): double;
begin
  result := Bound[CellIndex*2+1];
end;

{ TRivStressPeriod }

function TRivStressPeriod.Cond(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*3+1];
end;

function TRivStressPeriod.RBot(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*3+2];
end;

function TRivStressPeriod.Stage(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*3];
end;

{ TImportedRiv }

constructor TImportedRiv.Create;
begin
  inherited;
  StressPeriods:= TRivStressPeriods.Create;
end;

destructor TImportedRiv.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

{ TCustomImportedBoundary }

procedure TCustomImportedBoundary.GetPackageValues(ModelName: AnsiString);
begin
  Naux := GetNAUX(ModelName, PackageName);
  MAXBOUND := GetMAXBOUND(ModelName, PackageName);
  Mover := GetMOVER(ModelName, PackageName);
  if Naux > 0 then
  begin
    AuxNames := GetAuxName(ModelName, PackageName);
    Assert(Length(AuxNames) = Naux);
    AuxMult := GetAuxMult(ModelName, PackageName);
  end
  else
  begin
    AuxNames := nil;
    AuxMult := -1;
  end;
end;

{ TImportedWelv }

constructor TImportedWel.Create;
begin
  inherited;
  StressPeriods := TWelStressPeriods.Create;
end;

destructor TImportedWel.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

procedure TImportedWel.GetPackageValues(ModelName: AnsiString);
begin
  inherited;
  AUTO_FLOW_REDUCE := GetAUTO_FLOW_REDUCE(ModelName, PackageName);
end;

{ TImportedDrn }

constructor TImportedDrn.Create;
begin
  inherited;
  StressPeriods := TDrnStressPeriods.Create
end;

destructor TImportedDrn.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

procedure TImportedDrn.GetPackageValues(ModelName: AnsiString);
begin
  inherited;
  AUXDEPTHNAME := GetAUXDEPTHNAME(ModelName, PackageName);
end;

{ TDrnStressPeriod }

function TDrnStressPeriod.Cond(CellIndex: Integer): double;
begin
  result := Bound[CellIndex*2+1];
end;

function TDrnStressPeriod.Elev(CellIndex: Integer): double;
begin
  result := Bound[CellIndex*2];
end;

{ TImportedRch }

constructor TImportedRch.Create;
begin
  inherited;
  StressPeriods := TRchStressPeriods.Create;
end;

destructor TImportedRch.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

procedure TImportedRch.GetPackageValues(ModelName: AnsiString);
begin
  inherited;
  // FIXED_CELL is not available through the API
end;

{ TEvtStressPeriod }


function TEvtStressPeriod.Depth(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary+2];
end;

function TEvtStressPeriod.Petm(CellIndex, SegIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary+3 + SegIndex*2 + 1];
end;

function TEvtStressPeriod.Petm0(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary+3 + (NSEG-1) *2];
end;

function TEvtStressPeriod.Pxdp(CellIndex, SegIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary+3 + SegIndex*2];
end;

function TEvtStressPeriod.Rate(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary+1];
end;

function TEvtStressPeriod.Surface(CellIndex: Integer): double;
begin
  Result := Bound[CellIndex*ValuesPerBoundary];
end;

{ TImportedEvt }

constructor TImportedEvt.Create;
begin
  inherited;
  StressPeriods := TEvtStressPeriods.Create;
end;

destructor TImportedEvt.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

procedure TImportedEvt.GetPackageValues(ModelName: AnsiString);
begin
  inherited;
  NSEG := GetNSEG(ModelName, PackageName);
  // can't get SURF_RATE_SPECIFIED
  // can't get FIXED_CELL
end;

{ TImportedMaw }

constructor TImportedMaw.Create;
begin
  inherited;
  StressPeriods := TMawStressPeriods.Create;
end;

destructor TImportedMaw.Destroy;
begin
  StressPeriods.Free;
  inherited;
end;

procedure TImportedMaw.GetPackageValues(ModelName: AnsiString);
var
  BoundNames: TAnsiStringArray;
begin
  inherited;
  NMAWWELLS := GetNMAWWELLS(ModelName, PackageName);
  NO_WELL_STORAGE := GetNO_WELL_STORAGE(ModelName, PackageName);
  FLOW_CORRECTION := GetFLOW_CORRECTION(ModelName, PackageName);
  FLOWING_WELLS := GetFLOWING_WELLS(ModelName, PackageName);
  SHUTDOWN_THETA := GetSHUTDOWN_THETA(ModelName, PackageName);
  SHUTDOWN_KAPPA := GetSHUTDOWN_KAPPA(ModelName, PackageName);
  Radius := GetMAW_Radius(ModelName, PackageName);
  Bottom := GetMAW_Bottom(ModelName, PackageName);
  Strt := GetMAW_STRT(ModelName, PackageName);
  CondEqn := GetMAW_CondEqn(ModelName, PackageName);
  NGWFNODES := GetMAW_NGWFNODES(ModelName, PackageName);
  PckgAux := GetAuxValue(ModelName, PackageName);
  BoundNames := GetBoundNames(ModelName, PackageName);
end;

{ TImportedPackage }

constructor TImportedPackage.Create(APackageName: AnsiString);
begin
  inherited Create;
  PackageName := APackageName;
end;

initialization
  ErrorMessages:= TStringList.Create;

finalization
  ErrorMessages.Free;


end.
