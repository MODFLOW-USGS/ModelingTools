unit SutraImporter;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.StrUtils, FastGEO, GoPhastTypes, RealListUnit;

Type
  TSutraVersion = (sv2_0, sv2_1, sv2_2, sv3_0);
  TSutraMeshDimensions = (smd2, smd3);
  TSutraMeshType = (smtRegular, smtBlockwise, smtLayered, smtIrregular);
  TSutraOrdering = (soAcross, soWithin);
  TSutraScheduleType = (sstTimeList, sstTimeCycle, sstStepList, sstStepCycle);

  EImportSutraError = class(Exception);

  TSutraSchedule = class(TObject)
  private
    FSCHNAM: string;
    FTimes: TList<double>;
    function GetTime(Index: Integer): double;
    function GetTimeCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    property SCHNAM: string read FSCHNAM;
    property TimeCount: integer read GetTimeCount;
    property Times[Index: Integer]: double read GetTime;
  end;

  TSutraSchedules = TObjectList<TSutraSchedule>;

  TSutraFluidSource = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    QINC: double;
    UINC: double;
    property Active: Boolean read GetActive;
  end;

  TSutraFluidSources = TList<TSutraFluidSource>;

  TSutraUSource = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    QUINC: double;
    property Active: Boolean read GetActive;
  end;

  TSutraUSources = TList<TSutraUSource>;

  TSutraSpecifiedPressure = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    PBC: double;
    UBC: double;
    property Active: Boolean read GetActive;
  end;

  TSutraSpecifiedPressures = TList<TSutraSpecifiedPressure>;

  TSutraSpecifiedU = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    UBC: double;
    property Active: Boolean read GetActive;
  end;

  TSutraSpecifiedUs = TList<TSutraSpecifiedU>;

  TSutraLimitType = (sltQ, sltP, sltNone);
  TSutraUSpecificationType = (sustDirect, sustRelative);

  TSutraGeneralizedFlow = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    PBG1: double;
    QPBG1: double;
    PBG2: double;
    QPBG2: double;
    CPQL1: TSutraLimitType;
    CPQL2: TSutraLimitType;
    UPBGI: double;
    USpectype: TSutraUSpecificationType;
    UPBGO: double;
    property Active: Boolean read GetActive;
  end;

  TSutraGeneralizedFlows = TList<TSutraGeneralizedFlow>;

  TSutraGeneralizedTransport = record
  private
    function GetActive: Boolean;
  public
    NodeNumber: Integer;
    UBG1: double;
    QUBG1: double;
    UBG2: double;
    QUBG2: double;
    property Active: Boolean read GetActive;
  end;

  TSutraGeneralizedTransports = TList<TSutraGeneralizedTransport>;

  TCustomSutraReader = class(TObject)
  private
    FSplitter: TStringList;
    FFile: TStreamReader;
    FCurrentFile: TStreamReader;
    FEmbeddedFiles: TObjectList<TStreamReader>;
  protected
    function ReadNextNonCommentLine: string;
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;
  end;

  TSutraInputReader = class(TCustomSutraReader)
  private
    FVersion: TSutraVersion;
    FMeshDimensions: TSutraMeshDimensions;
    FMeshType: TSutraMeshType;

    FNN1: Integer;
    FNN2: Integer;
    FNN3: Integer;
    FNLAYS: Integer;
    FNNLAY: Integer;
    FNELAY: Integer;
    FOrdering: TSutraOrdering;
    FNBLK1: Integer;
    FNBLK2: Integer;
    FLDIV2: Integer;
    FNBLK3: Integer;
    FLDIV3: Integer;
    FNN: Integer;
    FNE: Integer;
    FNPBC: Integer;
    FNUBC: Integer;
    FNSOP: Integer;
    FNSOU: Integer;
    FNPBG: Integer;
    FNUBG: Integer;
    FNOBS: Integer;
    FNSCH: Integer;
    FLDIV1: Integer;

    FSchedules: TSutraSchedules;
    FScaleX: Extended;
    FScaleY: Extended;
    FScaleZ: Extended;
    FNodes: array of TPoint3D;
    FFluidSources: TSutraFluidSources;
    FUSources: TSutraUSources;
    FSpecifiedPressures: TSutraSpecifiedPressures;
    FSpecifiedUs: TSutraSpecifiedUs;
    FGeneralizedFlows: TSutraGeneralizedFlows;
    FGeneralizedTransports: TSutraGeneralizedTransports;
    FScheduleDictionary: TDictionary<String, TSutraSchedule>;
    function ReadNextNonCommentLine: string;
    procedure ReadDataSet1;
    procedure ReadDataSet2A;
    procedure ReadDataSet2B;
    procedure ReadDataSet3;
    procedure ReadDataSet4;
    procedure ReadDataSet5;
    procedure ReadDataSet6;
    procedure ReadDataSet7A;
    procedure ReadDataSet7B;
    procedure ReadDataSet7C;
    procedure ReadDataSet8A;
    procedure ReadDataSet8B;
    procedure ReadDataSet8C;
    procedure ReadDataSet8D;
    procedure ReadDataSet8E;
    procedure ReadDataSet9;
    procedure ReadDataSet10;
    procedure ReadDataSet11;
    procedure ReadDataSet12;
    procedure ReadDataSet13;
    procedure ReadDataSet14A;
    procedure ReadDataSet14B;
    procedure ReadDataSet15A;
    procedure ReadDataSet15B;
    procedure ReadDataSet17;
    procedure ReadDataSet18;
    procedure ReadDataSet19;
    procedure ReadDataSet20;
    procedure ReadDataSet21A;
    procedure ReadDataSet21B;
    procedure ReadDataSet22;
    function GetNode(Index: Integer): TPoint3D;
    function GetNodeCount: Integer;
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;
    procedure ReadInputFile;
    function GetScheduleByName(const AName: string): TSutraSchedule;
    property NodeCount: Integer read GetNodeCount;
    property Nodes[Index: Integer]: TPoint3D read GetNode;
    property FluidSources: TSutraFluidSources read FFluidSources;
    property USources: TSutraUSources read FUSources;
    property SpecifiedPressures: TSutraSpecifiedPressures
     read FSpecifiedPressures;
    property SpecifiedUs: TSutraSpecifiedUs read FSpecifiedUs;
    property GeneralizedFlows: TSutraGeneralizedFlows read FGeneralizedFlows;
    property GeneralizedTransports: TSutraGeneralizedTransports
      read FGeneralizedTransports;
    property Version: TSutraVersion read FVersion;

  end;

  TBcsInputReader = class(TCustomSutraReader)
  private
    FInputFileReader: TSutraInputReader;
    FSpecifiedPressureIndices: TOneDIntegerArray;
    FSpecifiedUIndices: TOneDIntegerArray;
    FFluidSourceIndices: TOneDIntegerArray;
    FMassEnergySourcesIndices: TOneDIntegerArray;
    FGeneralizedPressBoundaryIndices: TOneDIntegerArray;
    FGeneralizedTransportIndices: TOneDIntegerArray;
    FTimeStep: Integer;
    FAllTimes: TRealList;
    FBoundaryTimes: TRealList;
//    FStepToUse: Integer;
    FCurrentStep: Integer;
    FStepsInFile: TList<Integer>;
    FNSOP1: Integer;
    FNSOU1: Integer;
    FNPBC1: Integer;
    FNUBC1: Integer;
    FNPBG1: Integer;
    FNUBG1: Integer;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadDataSet3;
    procedure ReadDataSet4;
    procedure ReadDataSet5;
    procedure ReadDataSet6;
    procedure ReadDataSet7A;
    procedure ReadDataSet7B;
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;
    property InputFileReader: TSutraInputReader read FInputFileReader
      write FInputFileReader;
    procedure ReadBcsFile(TimeStep: integer);
    procedure ReadAStep;
  end;

  TSutraFilReader = class(TObject)
  private
    FSutraFile: TStreamReader;
    FBcsFileReaders: TObjectList<TBcsInputReader>;
    FSplitter: TStringList;
    FInputFileReader: TSutraInputReader;
    function ReadNextNonCommentLine: string;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    property InputFileReader: TSutraInputReader read FInputFileReader;
    procedure ReadInput(TimeStep: integer);
  end;


implementation

uses
  ModelMuseUtilities;

resourcestring
  StrTheSpecifiedTimeS = 'The specified time step "%0:d" is too large.' +
  ' There aren''t that many time steps in the simulation.';

{ TSutraInputReader }

constructor TSutraInputReader.Create(const FileName: string);
begin
  inherited;
  FSchedules := TSutraSchedules.Create;
  FFluidSources := TSutraFluidSources.Create;
  FUSources := TSutraUSources.Create;
  FSpecifiedPressures := TSutraSpecifiedPressures.Create;
  FSpecifiedUs := TSutraSpecifiedUs.Create;
  FGeneralizedFlows := TSutraGeneralizedFlows.Create;
  FGeneralizedTransports := TSutraGeneralizedTransports.Create;
end;

destructor TSutraInputReader.Destroy;
begin
  FScheduleDictionary.Free;
  FGeneralizedTransports.Free;
  FGeneralizedFlows.Free;
  FSpecifiedUs.Free;
  FSpecifiedPressures.Free;
  FUSources.Free;
  FFluidSources.Free;
  FSchedules.Free;
  inherited;
end;

function TSutraInputReader.GetNode(Index: Integer): TPoint3D;
begin
  result := FNodes[Index];
end;

function TSutraInputReader.GetNodeCount: Integer;
begin
  result := Length(FNodes);
end;

function TSutraInputReader.GetScheduleByName(
  const AName: string): TSutraSchedule;
var
  Index: Integer;
  ASchedule: TSutraSchedule;
begin
  result := nil;
  if FScheduleDictionary = nil then
  begin
    FScheduleDictionary := TDictionary<string, TSutraSchedule>.Create;
    for Index := 0 to FSchedules.Count - 1 do
    begin
      ASchedule := FSchedules[Index];
      FScheduleDictionary.Add(UpperCase(ASchedule.SCHNAM), ASchedule);
    end;
  end;
  FScheduleDictionary.TryGetValue(UpperCase(AName), result);
end;

procedure TSutraInputReader.ReadDataSet1;
begin
  ReadNextNonCommentLine;
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet10;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet11;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet12;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet13;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet14A;
var
  ALine: string;
begin
  ALine := ReadNextNonCommentLine;
  ALine := ReplaceText(ALine, '"', '''');
  FSplitter.DelimitedText := ALine;
  Assert(FSplitter.Count >= 5);
  Assert(UpperCase(FSplitter[0]) = 'NODE');
  FScaleX := FortranStrToFloat(FSplitter[1]);
  FScaleY := FortranStrToFloat(FSplitter[2]);
  FScaleZ := FortranStrToFloat(FSplitter[3]);
end;

procedure TSutraInputReader.ReadDataSet14B;
var
  NodeIndex: Integer;
  APoint: TPoint3D;
  ALine: string;
  NodeNumber: Integer;
begin
  SetLength(FNodes, FNN);
  for NodeIndex := 0 to FNN - 1 do
  begin
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    NodeNumber := StrToInt(FSplitter[0]);
    APoint.x := FortranStrToFloat(FSplitter[2]) * FScaleX;
    APoint.y := FortranStrToFloat(FSplitter[3]) * FScaleY;
    if FMeshDimensions = smd3 then
    begin
      APoint.z := FortranStrToFloat(FSplitter[4]) * FScaleZ;
    end
    else
    begin
      APoint.z := 0;
    end;
    FNodes[NodeNumber-1] := APoint;
  end;
end;

procedure TSutraInputReader.ReadDataSet15A;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet15B;
var
  Index: Integer;
begin
  for Index := 0 to FNE - 1 do
  begin
    ReadNextNonCommentLine;
  end;
end;

procedure TSutraInputReader.ReadDataSet17;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraFluidSource;
begin
  FFluidSources.Capacity := FNSOP;
  if FNSOP > 0 then
  begin
    for BoundaryIndex := 0 to FNSOP - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 3);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.QINC := FortranStrToFloat(FSplitter[1]);
      ABoundary.UINC := FortranStrToFloat(FSplitter[2]);
      FFluidSources.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet18;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraUSource;
begin
  FUSources.Capacity := FNSOU;
  if FNSOU > 0 then
  begin
    for BoundaryIndex := 0 to FNSOU - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 2);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.QUINC := FortranStrToFloat(FSplitter[1]);
      FUSources.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet19;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraSpecifiedPressure;
begin
  FSpecifiedPressures.Capacity := FNPBC;
  if FNPBC > 0 then
  begin
    for BoundaryIndex := 0 to FNPBC - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 3);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.PBC := FortranStrToFloat(FSplitter[1]);
      ABoundary.UBC := FortranStrToFloat(FSplitter[2]);
      FSpecifiedPressures.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet20;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraSpecifiedU;
begin
  FSpecifiedUs.Capacity := FNSOU;
  if FNSOU > 0 then
  begin
    for BoundaryIndex := 0 to FNSOU - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 2);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.UBC := FortranStrToFloat(FSplitter[1]);
      FSpecifiedUs.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet21A;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraGeneralizedFlow;
  Selection: string;
begin
  FGeneralizedFlows.Capacity := FNPBG;
  if FNPBG > 0 then
  begin
    for BoundaryIndex := 0 to FNPBG - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      ALine := ReplaceText(ALine, '"', '''');
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 10);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.PBG1 := FortranStrToFloat(FSplitter[1]);
      ABoundary.QPBG1 := FortranStrToFloat(FSplitter[2]);
      ABoundary.PBG2 := FortranStrToFloat(FSplitter[3]);
      ABoundary.QPBG2 := FortranStrToFloat(FSplitter[4]);
      Selection := UpperCase(FSplitter[5]);
      if Selection = 'Q' then
      begin
        ABoundary.CPQL1 := sltQ;
      end
      else if Selection = 'P' then
      begin
        ABoundary.CPQL1 := sltP;
      end
      else if Selection = 'N' then
      begin
        ABoundary.CPQL1 := sltNone;
      end
      else
      begin
        Assert(False);
      end;
      Selection := UpperCase(FSplitter[6]);
      if Selection = 'Q' then
      begin
        ABoundary.CPQL2 := sltQ;
      end
      else if Selection = 'P' then
      begin
        ABoundary.CPQL2 := sltP;
      end
      else if Selection = 'N' then
      begin
        ABoundary.CPQL2 := sltNone;
      end
      else
      begin
        Assert(False);
      end;
      ABoundary.UPBGI := FortranStrToFloat(FSplitter[7]);

      Selection := UpperCase(FSplitter[8]);
      if Selection = 'DIR' then
      begin
        ABoundary.USpectype := sustDirect;
      end
      else if Selection = 'REL' then
      begin
        ABoundary.USpectype := sustRelative;
      end
      else
      begin
        Assert(False);
      end;

      ABoundary.UPBGO := FortranStrToFloat(FSplitter[9]);
      FGeneralizedFlows.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet21B;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraGeneralizedTransport;
begin
  FGeneralizedTransports.Capacity := FNUBG;
  if FNUBG > 0 then
  begin
    for BoundaryIndex := 0 to FNUBG - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      ALine := ReplaceText(ALine, '"', '''');
      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count >= 10);
      ABoundary.NodeNumber := StrToInt(FSplitter[0]);
      ABoundary.UBG1 := FortranStrToFloat(FSplitter[1]);
      ABoundary.QUBG1 := FortranStrToFloat(FSplitter[2]);
      ABoundary.UBG2 := FortranStrToFloat(FSplitter[3]);
      ABoundary.QUBG2 := FortranStrToFloat(FSplitter[4]);
      FGeneralizedTransports.Add(ABoundary);
    end;
    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter[0] = '0');
  end;
end;

procedure TSutraInputReader.ReadDataSet22;
var
  Index: Integer;
begin
  for Index := 0 to FNE do
  begin
    ReadNextNonCommentLine;
  end;
end;

procedure TSutraInputReader.ReadDataSet2A;
var
  ALine: string;
begin
  ALine := ReadNextNonCommentLine;
  ALine := ReplaceText(ALine, '"', '');
  ALine := ReplaceText(ALine, '''', '');
  FSplitter.DelimitedText := UpperCase(ALine);
  Assert(FSplitter.Count >= 2);
  Assert(FSplitter[0] = 'SUTRA');
  if FSplitter[1] = 'VERSION' then
  begin
    if FSplitter[2] = '2D3D.1' then
    begin
      FVersion := sv2_0;
    end
    else if FSplitter[2] = '2.0' then
    begin
      FVersion := sv2_0;
    end
    else if FSplitter[2] = '2.1' then
    begin
      FVersion := sv2_1;
    end
    else if FSplitter[2] = '2.2' then
    begin
      FVersion := sv2_2;
    end
    else if FSplitter[2] = '3.0' then
    begin
      FVersion := sv3_0;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    FVersion := sv2_0;
  end;
end;

procedure TSutraInputReader.ReadDataSet2B;
var
  ALine: string;
//  FLDIV1: TObject;
begin
  ALine := ReadNextNonCommentLine;
  ALine := ReplaceText(ALine, '"', '');
  ALine := ReplaceText(ALine, '''', '');
  FSplitter.DelimitedText := UpperCase(ALine);
  Assert(FSplitter.Count >= 2);
  if FSplitter[0] = '2D' then
  begin
    FMeshDimensions := smd2;
  end
  else if FSplitter[0] = '3D' then
  begin
    FMeshDimensions := smd3;
  end
  else
  begin
    Assert(False);
  end;
  if FSplitter[1] = 'REGULAR' then
  begin
    FMeshType := smtRegular;
    FNN1 := StrToInt(FSplitter[2]);
    FNN2 := StrToInt(FSplitter[3]);
    if FMeshDimensions = smd3 then
    begin
      FNN3 := StrToInt(FSplitter[4]);
    end
    else
    begin
      FNN3 := 1;
    end;
  end
  else if FSplitter[1] = 'BLOCKWISE' then
  begin
    FMeshType := smtBlockwise;

    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    FNBLK1 := StrToInt(FSplitter[0]);
    FLDIV1 := StrToInt(FSplitter[0]);

    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    FNBLK2 := StrToInt(FSplitter[0]);
    FLDIV2 := StrToInt(FSplitter[0]);

    ALine := ReadNextNonCommentLine;
    FSplitter.DelimitedText := ALine;
    FNBLK3 := StrToInt(FSplitter[0]);
    FLDIV3 := StrToInt(FSplitter[0]);
  end
  else if FSplitter[1] = 'LAYERED' then
  begin
    Assert(FMeshDimensions = smd3);
    FMeshType := smtLayered;
    FNLAYS := StrToInt(FSplitter[2]);
    FNNLAY := StrToInt(FSplitter[3]);
    FNELAY := StrToInt(FSplitter[4]);
    if FSplitter[5] = 'ACROSS' then
    begin
      FOrdering := soAcross;
    end
    else if FSplitter[5] = 'WITHIN' then
    begin
      FOrdering := soWithin;
    end
    else
    begin
      Assert(False);
    end;
  end
  else if FSplitter[1] = 'IRREGULAR' then
  begin
    FMeshType := smtIrregular;
  end
end;

procedure TSutraInputReader.ReadDataSet3;
var
  ALine: string;
begin
  ALine := ReadNextNonCommentLine;
  FSplitter.DelimitedText := ALine;
  if FVersion in [sv2_0, sv2_1, sv2_2] then
  begin
    Assert(FSplitter.Count >= 7);
  end
  else
  begin
    Assert(FVersion = sv3_0);
    Assert(FSplitter.Count >= 9);
  end;
  FNN := StrToInt(FSplitter[0]);
  FNE := StrToInt(FSplitter[1]);
  FNPBC := StrToInt(FSplitter[2]);
  FNUBC := StrToInt(FSplitter[3]);
  FNSOP := StrToInt(FSplitter[4]);
  FNSOU := StrToInt(FSplitter[5]);
  if FVersion = sv3_0 then
  begin
    FNPBG := StrToInt(FSplitter[6]);
    FNUBG := StrToInt(FSplitter[7]);
    FNOBS := StrToInt(FSplitter[8]);
  end
  else
  begin
    FNPBG := 0;
    FNUBG := 0;
    FNOBS := StrToInt(FSplitter[6]);
  end;
end;

procedure TSutraInputReader.ReadDataSet4;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet5;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet6;
var
  ALine: string;
  ScheduleIndex: Integer;
  SCHTYP: string;
  ScheduleType: TSutraScheduleType;
  CREFT: string;
  SCALT: Double;
  NTLIST: Integer;
  TimeIndex: Integer;
  ASchedule: TSutraSchedule;
  ATime: Extended;
  NextIndex: Integer;
  NTMAX: Integer;
  TIMEI: double;
  TIMEL: double;
  TIMEC: double;
  NTCYC: Integer;
  TCMULT: Extended;
  TCMIN: Extended;
  TCMAX: Extended;
  AbsoluteTime: Boolean;
  CycleIndex: Integer;
  TimeSteps: TSutraSchedule;
  StepIndex: Integer;
  NSLIST: Integer;
  AStep: Integer;
  NSMAX: Integer;
  ISTEPI: Integer;
  ISTEPL: Integer;
  ISTEPC: Integer;
begin
  ALine := ReadNextNonCommentLine;
//  ALine := ReplaceText(ALine, '"', '''');
  FSplitter.DelimitedText := ALine;
  Assert(FSplitter.Count >= 3);
  FNSCH := StrToInt(FSplitter[0]);
  for ScheduleIndex := 0 to FNSCH - 1 do
  begin
    ALine := ReadNextNonCommentLine;
    ALine := ReplaceText(ALine, '"', '''');
    FSplitter.DelimitedText := ALine;

    ASchedule := TSutraSchedule.Create;
    FSchedules.Add(ASchedule);
    ASchedule.FSCHNAM := FSplitter[0];
    SCHTYP := UpperCase(FSplitter[1]);
    if SCHTYP = 'TIME LIST' then
    begin
      ScheduleType := sstTimeList;
    end
    else if SCHTYP = 'TIME CYCLE' then
    begin
      ScheduleType := sstTimeCycle;
    end
    else if SCHTYP = 'STEP LIST' then
    begin
      ScheduleType := sstStepList;
    end
    else if SCHTYP = 'STEP CYCLE' then
    begin
      ScheduleType := sstStepCycle;
    end
    else
    begin
      ScheduleType := sstTimeList;
      Assert(False);
    end;
    case ScheduleType of
      sstTimeList:
        begin
          CREFT := FSplitter[2];
          SCALT := FortranStrToFloat(FSplitter[3]);
          NTLIST := StrToInt(FSplitter[4]);
          ASchedule.FTimes.Capacity := NTLIST;
          NextIndex := 5;
          for TimeIndex := 0 to NTLIST - 1 do
          begin
            if NextIndex >= FSplitter.Count then
            begin
              ALine := ReadNextNonCommentLine;
              FSplitter.DelimitedText := ALine;
              NextIndex := 0
            end;
            ATime := FortranStrToFloat(FSplitter[NextIndex])*SCALT;
            ASchedule.FTimes.Add(ATime);
            Inc(NextIndex);
          end;
        end;
      sstTimeCycle:
        begin
          CREFT := UpperCase(FSplitter[2]);
          SCALT := FortranStrToFloat(FSplitter[3]);
          NTMAX := StrToInt(FSplitter[4]);
          TIMEI := FortranStrToFloat(FSplitter[5]);
          TIMEL := FortranStrToFloat(FSplitter[6]);
          TIMEC := FortranStrToFloat(FSplitter[7]);
          NTCYC := StrToInt(FSplitter[8]);
          TCMULT := FortranStrToFloat(FSplitter[9]);
          TCMIN := FortranStrToFloat(FSplitter[9]);
          TCMAX := FortranStrToFloat(FSplitter[9]);

          if CREFT = 'ABSOLUTE' then
          begin
            AbsoluteTime := True;
          end
          else
          begin
            AbsoluteTime := False;
            Assert(CREFT = 'ELAPSED');
          end;

          ASchedule.FTimes.Capacity := NTMAX+1;
          ASchedule.FTimes.Add(TIMEI);
          ATime := TIMEI;
          for CycleIndex := 1 to NTMAX do
          begin
            if (CycleIndex mod NTCYC) = 0 then
            begin
              TIMEC := TIMEC * TCMULT;
              if TIMEC < TCMIN then
              begin
                TIMEC := TCMIN;
              end
              else if TIMEC > TCMAX then
              begin
                TIMEC := TCMAX;
              end;
            end;
            ATime := ATime + TIMEC*SCALT;
            if ATime >= TIMEL then
            begin
              break;
            end;
            ASchedule.FTimes.Add(ATime);
          end;
        end;
      sstStepList:
        begin
          Assert(FSchedules.Count > 1);
          TimeSteps := FSchedules[0];
          NSLIST := StrToInt(FSplitter[2]);

          NextIndex := 3;
          for TimeIndex := 0 to NSLIST - 1 do
          begin
            if NextIndex >= FSplitter.Count then
            begin
              ALine := ReadNextNonCommentLine;
              FSplitter.DelimitedText := ALine;
              NextIndex := 0
            end;
            AStep := StrToInt(FSplitter[NextIndex]);
            if AStep >= TimeSteps.TimeCount then
            begin
              break;
            end;
            ASchedule.FTimes.Add(TimeSteps.Times[AStep]);
            Inc(NextIndex);
          end;
        end;
      sstStepCycle:
        begin
          Assert(FSchedules.Count > 1);
          TimeSteps := FSchedules[0];

          NSMAX := StrToInt(FSplitter[2]);
          ISTEPI := StrToInt(FSplitter[3]);
          ISTEPL := StrToInt(FSplitter[4]);
          ISTEPC := StrToInt(FSplitter[5]);

          AStep := ISTEPI;
          ASchedule.FTimes.Add(TimeSteps.Times[AStep]);
          for StepIndex := 0 to NSMAX - 1 do
          begin
            AStep := AStep + ISTEPC;
            if AStep >= ISTEPL then
            begin
              break;
            end;
            if AStep >= TimeSteps.TimeCount then
            begin
              break;
            end;
            ASchedule.FTimes.Add(TimeSteps.Times[AStep]);
          end;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end;
  ALine := ReadNextNonCommentLine;
  Assert(ALine = '-');
end;

procedure TSutraInputReader.ReadDataSet7A;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet7B;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet7C;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet8A;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet8B;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet8C;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet8D;
var
  Index: Integer;
begin
  if FNOBS > 0 then
  begin
    for Index := 0 to FNOBS do
    begin
      ReadNextNonCommentLine;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TSutraInputReader.ReadDataSet8E;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadDataSet9;
begin
  ReadNextNonCommentLine;
end;

procedure TSutraInputReader.ReadInputFile;
begin
  ReadDataSet1;
  ReadDataSet2A;
  ReadDataSet2B;
  ReadDAtaSet3;
  ReadDAtaSet4;
  ReadDAtaSet5;
  ReadDAtaSet6;
  ReadDataSet7A;
  ReadDataSet7B;
  ReadDataSet7C;
  ReadDataSet8A;
  ReadDataSet8B;
  ReadDataSet8C;
  ReadDataSet8D;
  ReadDataSet8E;
  ReadDAtaSet9;
  ReadDAtaSet10;
  ReadDAtaSet11;
  ReadDAtaSet12;
  ReadDAtaSet13;
  ReadDAtaSet14A;
  ReadDAtaSet14B;
  ReadDAtaSet15A;
  ReadDAtaSet15B;
  ReadDataSet17;
  ReadDataSet18;
  ReadDataSet19;
  ReadDataSet20;
  ReadDataSet21A;
  ReadDataSet21B;
  ReadDataSet22;
end;

function TSutraInputReader.ReadNextNonCommentLine: string;
const
  InsertCommand = '@INSERT';
var
  ALine: string;
  EmbeddedFileName: string;
  EmbeddedFile: TStreamReader;
begin
  result := '';
  ALine := '';
  while (not FCurrentFile.EndOfStream) or (FEmbeddedFiles.Count > 0) do
  begin
    ALine := FCurrentFile.ReadLine;
    if (ALine <> '') and (ALine[1] <> '#') then
    begin
      if UpperCase(Copy(Trim(ALine), 1, Length(InsertCommand)))
        = InsertCommand then
      begin
        FSplitter.DelimitedText := ALine;
        Assert(FSplitter.Count >= 3);
        EmbeddedFileName := FSplitter[2];
        EmbeddedFile := TFile.OpenText(EmbeddedFileName);
        FEmbeddedFiles.Add(EmbeddedFile);
        FCurrentFile := EmbeddedFile;
        Continue;
      end;
      result := ALine;
      Exit;
    end;
    if FCurrentFile.EndOfStream and (FEmbeddedFiles.Count > 0) then
    begin
      FEmbeddedFiles.Delete(FEmbeddedFiles.Count -1);
      if FEmbeddedFiles.Count > 0 then
      begin
        FCurrentFile := FEmbeddedFiles[FEmbeddedFiles.Count -1];
      end
      else
      begin
        FCurrentFile := FFile;
      end;
    end;
  end;
end;

{ TSutraSchedule }

constructor TSutraSchedule.Create;
begin
  FTimes := TList<double>.Create;
end;

destructor TSutraSchedule.Destroy;
begin
  FTimes.Free;
  inherited;
end;

function TSutraSchedule.GetTime(Index: Integer): double;
begin
  result := FTimes[Index]
end;

function TSutraSchedule.GetTimeCount: integer;
begin
  result := FTimes.Count;
end;

{ TCustomSutraReader }

constructor TCustomSutraReader.Create(const FileName: string);
begin
  try
    FFile := TFile.OpenText(FileName);
  except on E: Exception do
    Exception.RaiseOuterException(EImportSutraError.Create(
      Format('Error opening %0:s. The error was "%1:s"', [FileName, E.Message])));
  end;
  FCurrentFile := FFile;
  FEmbeddedFiles := TObjectList<TStreamReader>.Create;
  FSplitter := TStringList.Create;
  FSplitter.QuoteChar := '''';
end;

destructor TCustomSutraReader.Destroy;
begin
  FSplitter.Free;
  FEmbeddedFiles.Free;
  FFile.Free;
  inherited;
end;

function TCustomSutraReader.ReadNextNonCommentLine: string;
const
  InsertCommand = '@INSERT';
var
  ALine: string;
  EmbeddedFileName: string;
  EmbeddedFile: TStreamReader;
begin
  result := '';
  ALine := '';
  while (not FCurrentFile.EndOfStream) or (FEmbeddedFiles.Count > 0) do
  begin
    ALine := FCurrentFile.ReadLine;
    if (ALine <> '') and (ALine[1] <> '#') then
    begin
      if UpperCase(Copy(Trim(ALine), 1, Length(InsertCommand))) = InsertCommand
      then
      begin
        FSplitter.DelimitedText := ALine;
        Assert(FSplitter.Count >= 3);
        EmbeddedFileName := FSplitter[2];
        EmbeddedFile := TFile.OpenText(EmbeddedFileName);
        FEmbeddedFiles.Add(EmbeddedFile);
        FCurrentFile := EmbeddedFile;
        Continue;
      end;
      result := ALine;
      Exit;
    end;
    if FCurrentFile.EndOfStream and (FEmbeddedFiles.Count > 0) then
    begin
      FEmbeddedFiles.Delete(FEmbeddedFiles.Count - 1);
      if FEmbeddedFiles.Count > 0 then
      begin
        FCurrentFile := FEmbeddedFiles[FEmbeddedFiles.Count - 1];
      end
      else
      begin
        FCurrentFile := FFile;
      end;
    end;
  end;
end;

{ TBcsInputReader }

constructor TBcsInputReader.Create(const FileName: string);
begin
  inherited;
  FAllTimes := TRealList.Create;
  FBoundaryTimes := TRealList.Create;
  FStepsInFile := TList<Integer>.Create;
end;

destructor TBcsInputReader.Destroy;
begin
  FStepsInFile.Free;
  FBoundaryTimes.Free;
  FAllTimes.Free;
  inherited;
end;

procedure TBcsInputReader.ReadAStep;
begin
  if FStepsInFile.IndexOf(FCurrentStep) >= 0 then
  begin
    ReadDataSet2;
    ReadDataSet3;
    ReadDataSet4;
    ReadDataSet5;
    ReadDataSet6;
    ReadDataSet7A;
    ReadDataSet7B;
  end;
  Inc(FCurrentStep);
end;

procedure TBcsInputReader.ReadBcsFile(TimeStep: integer);
begin
  FTimeStep := TimeStep;
  Assert(InputFileReader <> nil);
  ReadDataSet1;
end;

procedure TBcsInputReader.ReadDataSet1;
var
  ASchedule: TSutraSchedule;
  TimeSteps: TSutraSchedule;
  TimeIndex: Integer;
//  TimeToUse: Double;
  BCSSCH: string;
begin
  TimeSteps := InputFileReader.GetScheduleByName('TIME_STEPS');
  FAllTimes.Capacity := TimeSteps.TimeCount;
  for TimeIndex := 0 to TimeSteps.TimeCount - 1 do
  begin
    FAllTimes.Add(TimeSteps.Times[TimeIndex]);
  end;
  FAllTimes.Sorted := True;

  FSplitter.DelimitedText := Trim(ReadNextNonCommentLine);
  BCSSCH := FSplitter[0];
  if BCSSCH = 'STEP_0' then
  begin
    FBoundaryTimes.Capacity := 1;
    FBoundaryTimes.Add(TimeSteps.Times[0]);
  end
  else if BCSSCH = 'STEP_1' then
  begin
    FBoundaryTimes.Capacity := 1;
    FBoundaryTimes.Add(TimeSteps.Times[1]);
  end
  else if BCSSCH = 'STEPS_1&UP' then
  begin
    FBoundaryTimes.Capacity := TimeSteps.TimeCount-1;
    for TimeIndex := 1 to TimeSteps.TimeCount - 1 do
    begin
      FBoundaryTimes.Add(TimeSteps.Times[TimeIndex]);
    end;
  end
  else
  begin
    ASchedule := InputFileReader.GetScheduleByName(BCSSCH);
    FBoundaryTimes.Capacity := ASchedule.TimeCount;
    for TimeIndex := 0 to ASchedule.TimeCount - 1 do
    begin
      FBoundaryTimes.Add(ASchedule.Times[TimeIndex]);
    end;
  end;
  FBoundaryTimes.Sorted := True;
  if FTimeStep >= FAllTimes.Count then
  begin
    raise EImportSutraError.Create(Format(StrTheSpecifiedTimeS, [FTimeStep]));
  end;
//  Assert(FTimeStep < FAllTimes.Count, Format(StrTheSpecifiedTimeS, [FTimeStep]));
//  TimeToUse := FAllTimes[FTimeStep];
//  FStepToUse := FBoundaryTimes.IndexOfClosest(TimeToUse);
  FCurrentStep := 0;

  FStepsInFile.Capacity := FBoundaryTimes.Count;
  for TimeIndex := 0 to FBoundaryTimes.Count - 1 do
  begin
    FStepsInFile.Add(FAllTimes.IndexOfClosest(FBoundaryTimes[TimeIndex]));
  end;
end;

procedure TBcsInputReader.ReadDataSet2;
var
  ALine: string;
begin
  ALine := ReadNextNonCommentLine;
  ALine := ReplaceText(ALine, '"', '');
  FSplitter.DelimitedText := ALine;
  FNSOP1 := StrToInt(FSplitter[1]);
  FNSOU1 := StrToInt(FSplitter[2]);
  FNPBC1 := StrToInt(FSplitter[3]);
  FNUBC1 := StrToInt(FSplitter[4]);
  if FInputFileReader.Version = sv3_0 then
  begin
    FNPBG1 := StrToInt(FSplitter[5]);
    FNUBG1 := StrToInt(FSplitter[6]);
  end
  else
  begin
    FNPBG1 := 0;
    FNUBG1 := 0;
  end;
end;

procedure TBcsInputReader.ReadDataSet3;
var
  Index: Integer;
  LineIndex: Integer;
  ALine: string;
  IQCP1: Integer;
  BoundaryIndex: Integer;
  FluidSources: TSutraFluidSources;
  ABoundary: TSutraFluidSource;
begin
  if FNSOP1 > 0 then
  begin
    FluidSources := InputFileReader.FluidSources;
    if FFluidSourceIndices = nil then
    begin
      SetLength(FFluidSourceIndices, InputFileReader.FNN+1);
      for Index := 0 to FluidSources.Count - 1 do
      begin
        FFluidSourceIndices[FluidSources[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNSOP1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      IQCP1 := StrToInt(FSplitter[0]);
      BoundaryIndex := FFluidSourceIndices[Abs(IQCP1)];
      ABoundary := FluidSources[BoundaryIndex];
      ABoundary.NodeNumber := IQCP1;
      if IQCP1 > 0 then
      begin
        ABoundary.QINC := FortranStrToFloat(FSplitter[1]);
        ABoundary.UINC := FortranStrToFloat(FSplitter[2]);
      end;
      FluidSources[BoundaryIndex] := ABoundary;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TBcsInputReader.ReadDataSet4;
var
  Index: Integer;
  USources: TSutraUSources;
  LineIndex: Integer;
  ALine: string;
  IQCU1: Integer;
  BoundaryIndex: Integer;
  ABoundary: TSutraUSource;
begin
  if FNSOU1 > 0 then
  begin
    USources := InputFileReader.USources;
    if FMassEnergySourcesIndices = nil then
    begin
      SetLength(FMassEnergySourcesIndices, InputFileReader.FNN+1);
      for Index := 0 to USources.Count - 1 do
      begin
        FMassEnergySourcesIndices[USources[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNSOU1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      IQCU1 := StrToInt(FSplitter[0]);
      BoundaryIndex := FMassEnergySourcesIndices[Abs(IQCU1)];
      ABoundary := USources[BoundaryIndex];
      ABoundary.NodeNumber := IQCU1;
      if IQCU1 > 0 then
      begin
        ABoundary.QUINC := FortranStrToFloat(FSplitter[1]);
      end;
      USources[BoundaryIndex] := ABoundary;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TBcsInputReader.ReadDataSet5;
var
  Index: Integer;
  SpecifiedPressures: TSutraSpecifiedPressures;
  LineIndex: Integer;
  ALine: string;
  IPBC1: Integer;
  BoundaryIndex: Integer;
  ABoundary: TSutraSpecifiedPressure;
begin
  if FNPBC1 > 0 then
  begin
    SpecifiedPressures := InputFileReader.SpecifiedPressures;
    if FSpecifiedPressureIndices = nil then
    begin
      SetLength(FSpecifiedPressureIndices, InputFileReader.FNN+1);
      for Index := 0 to SpecifiedPressures.Count - 1 do
      begin
        FSpecifiedPressureIndices[SpecifiedPressures[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNPBC1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      IPBC1 := StrToInt(FSplitter[0]);
      BoundaryIndex := FSpecifiedPressureIndices[Abs(IPBC1)];
      ABoundary := SpecifiedPressures[BoundaryIndex];
      ABoundary.NodeNumber := IPBC1;
      if IPBC1 > 0 then
      begin
        ABoundary.PBC := FortranStrToFloat(FSplitter[1]);
        ABoundary.UBC := FortranStrToFloat(FSplitter[2]);
      end;
      SpecifiedPressures[BoundaryIndex] := ABoundary;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TBcsInputReader.ReadDataSet6;
var
  Index: Integer;
  SpecifiedUs: TSutraSpecifiedUs;
  LineIndex: Integer;
  ALine: string;
  IUBC1: Integer;
  BoundaryIndex: Integer;
  ABoundary: TSutraSpecifiedU;
begin
  if FNUBC1 > 0 then
  begin
    SpecifiedUs := InputFileReader.SpecifiedUs;
    if FSpecifiedUIndices = nil then
    begin
      SetLength(FSpecifiedUIndices, InputFileReader.FNN+1);
      for Index := 0 to SpecifiedUs.Count - 1 do
      begin
        FSpecifiedUIndices[SpecifiedUs[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNUBC1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      IUBC1 := StrToInt(FSplitter[0]);
      BoundaryIndex := FSpecifiedUIndices[Abs(IUBC1)];
      ABoundary := SpecifiedUs[BoundaryIndex];
      ABoundary.NodeNumber := IUBC1;
      if IUBC1 > 0 then
      begin
        ABoundary.UBC := FortranStrToFloat(FSplitter[1]);
      end;
      SpecifiedUs[BoundaryIndex] := ABoundary;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TBcsInputReader.ReadDataSet7A;
var
  Index: Integer;
  GeneralizedFlows: TSutraGeneralizedFlows;
  LineIndex: Integer;
  ALine: string;
  IPBG: Integer;
  BoundaryIndex: Integer;
  ABoundary: TSutraGeneralizedFlow;
  Selection: string;
begin
  if FNPBG1 > 0 then
  begin
    GeneralizedFlows := InputFileReader.GeneralizedFlows;
    if FGeneralizedPressBoundaryIndices = nil then
    begin
      SetLength(FGeneralizedPressBoundaryIndices, InputFileReader.FNN+1);
      for Index := 0 to GeneralizedFlows.Count - 1 do
      begin
        FGeneralizedPressBoundaryIndices[GeneralizedFlows[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNUBC1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      ALine := ReplaceText(ALine, '"', '');
      FSplitter.DelimitedText := ALine;
      IPBG := StrToInt(FSplitter[0]);
      BoundaryIndex := FGeneralizedPressBoundaryIndices[Abs(IPBG)];
      ABoundary := GeneralizedFlows[BoundaryIndex];
      ABoundary.NodeNumber := IPBG;
      if IPBG > 0 then
      begin
        ABoundary.PBG1 := FortranStrToFloat(FSplitter[1]);
        ABoundary.QPBG1 := FortranStrToFloat(FSplitter[2]);
        ABoundary.PBG2 := FortranStrToFloat(FSplitter[3]);
        ABoundary.QPBG2 := FortranStrToFloat(FSplitter[4]);
        Selection := UpperCase(FSplitter[5]);

        if Selection = 'Q' then
        begin
          ABoundary.CPQL1 := sltQ;
        end
        else if Selection = 'P' then
        begin
          ABoundary.CPQL1 := sltP;
        end
        else if Selection = 'N' then
        begin
          ABoundary.CPQL1 := sltNone;
        end
        else
        begin
          Assert(False);
        end;
        Selection := UpperCase(FSplitter[6]);
        if Selection = 'Q' then
        begin
          ABoundary.CPQL2 := sltQ;
        end
        else if Selection = 'P' then
        begin
          ABoundary.CPQL2 := sltP;
        end
        else if Selection = 'N' then
        begin
          ABoundary.CPQL2 := sltNone;
        end
        else
        begin
          Assert(False);
        end;

        ABoundary.UPBGI := FortranStrToFloat(FSplitter[7]);
        Selection := UpperCase(FSplitter[8]);
        if Selection = 'DIR' then
        begin
          ABoundary.USpectype := sustDirect;
        end
        else if Selection = 'REL' then
        begin
          ABoundary.USpectype := sustRelative;
        end
        else
        begin
          Assert(False);
        end;
        ABoundary.UPBGO := FortranStrToFloat(FSplitter[9]);
      end;
      GeneralizedFlows[BoundaryIndex] := ABoundary;
    end;
    ReadNextNonCommentLine;
  end;
end;

procedure TBcsInputReader.ReadDataSet7B;
var
  Index: Integer;
  GeneralizedTransports: TSutraGeneralizedTransports;
  LineIndex: Integer;
  ALine: string;
  IUBG: Integer;
  BoundaryIndex: Integer;
  ABoundary: TSutraGeneralizedTransport;
begin
  if FNUBG1 > 0 then
  begin
    GeneralizedTransports := InputFileReader.GeneralizedTransports;
    if FGeneralizedTransportIndices = nil then
    begin
      SetLength(FGeneralizedTransportIndices, InputFileReader.FNN+1);
      for Index := 0 to GeneralizedTransports.Count - 1 do
      begin
        FGeneralizedTransportIndices[GeneralizedTransports[Index].NodeNumber] := Index;
      end;
    end;
    for LineIndex := 0 to FNUBC1 - 1 do
    begin
      ALine := ReadNextNonCommentLine;
      FSplitter.DelimitedText := ALine;
      IUBG := StrToInt(FSplitter[0]);
      BoundaryIndex := FGeneralizedTransportIndices[Abs(IUBG)];
      ABoundary := GeneralizedTransports[BoundaryIndex];
      ABoundary.NodeNumber := IUBG;
      if IUBG > 0 then
      begin
        ABoundary.UBG1 := FortranStrToFloat(FSplitter[1]);
        ABoundary.QUBG1 := FortranStrToFloat(FSplitter[2]);
        ABoundary.UBG2 := FortranStrToFloat(FSplitter[3]);
        ABoundary.QUBG2 := FortranStrToFloat(FSplitter[4]);
      end;
      GeneralizedTransports[BoundaryIndex] := ABoundary;
    end;

  end;
end;

{ TSutraFluidSource }

function TSutraFluidSource.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraUSource }

function TSutraUSource.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraSpecifiedPressure }

function TSutraSpecifiedPressure.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraSpecifiedU }

function TSutraSpecifiedU.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraGeneralizedFlow }

function TSutraGeneralizedFlow.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraGeneralizedTransport }

function TSutraGeneralizedTransport.GetActive: Boolean;
begin
  result := NodeNumber > 0;
end;

{ TSutraFilReader }

constructor TSutraFilReader.Create(const FileName: string);
begin
  FSutraFile := TFile.OpenText(FileName);
  FBcsFileReaders := TObjectList<TBcsInputReader>.Create;
  FSplitter := TStringList.Create;
  FSplitter.QuoteChar := '''';
end;

destructor TSutraFilReader.Destroy;
begin
  FInputFileReader.Free;
  FSplitter.Free;
  FBcsFileReaders.Free;
  FSutraFile.Free;
end;

procedure TSutraFilReader.ReadInput(TimeStep: integer);
var
  BcsReader: TBcsInputReader;
  ReaderIndex: Integer;
  TimeIndex: Integer;
begin
  while (not FSutraFile.EndOfStream) do
  begin
    FSplitter.DelimitedText := ReadNextNonCommentLine;
    if UpperCase(FSplitter[0]) = 'INP' then
    begin
      FInputFileReader := TSutraInputReader.Create(FSplitter[2]);
    end
    else if UpperCase(FSplitter[0]) = 'BCS' then
    begin
      BcsReader := TBcsInputReader.Create(FSplitter[2]);
      FBcsFileReaders.Add(BcsReader);
    end;
  end;
  Assert(FInputFileReader <> nil);
  FInputFileReader.ReadInputFile;
  for ReaderIndex := 0 to FBcsFileReaders.Count - 1 do
  begin
    BcsReader := FBcsFileReaders[ReaderIndex];
    BcsReader.InputFileReader := FInputFileReader;
    BcsReader.ReadBcsFile(TimeStep);
  end;
  for TimeIndex := 0 to TimeStep do
  begin
    for ReaderIndex := 0 to FBcsFileReaders.Count - 1 do
    begin
      BcsReader := FBcsFileReaders[ReaderIndex];
      BcsReader.ReadAStep;
    end;
  end;
end;

function TSutraFilReader.ReadNextNonCommentLine: string;
var
  ALine: string;
begin
  result := '';
  ALine := '';
  while (not FSutraFile.EndOfStream) do
  begin
    ALine := FSutraFile.ReadLine;
    if (ALine <> '') and (ALine[1] <> '#') then
    begin
      result := ALine;
      Exit;
    end;
  end;
end;

end.
