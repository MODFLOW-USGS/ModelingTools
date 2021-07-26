unit ImportSutraUnit;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.StrUtils, FastGEO;

Type
  TSutraVersion = (sv2_0, sv2_1, sv2_2, sv3_0);
  TSutraMeshDimensions = (smd2, smd3);
  TSutraMeshType = (smtRegular, smtBlockwise, smtLayered, smtIrregular);
  TSutraOrdering = (soAcross, soWithin);
  TSutraScheduleType = (sstTimeList, sstTimeCycle, sstStepList, sstStepCycle);

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
    NodeNumber: Integer;
    QINC: double;
    UINC: double;
  end;

  TSutraFluidSources = TList<TSutraFluidSource>;

  TSutraUSource = record
    NodeNumber: Integer;
    QUINC: double;
  end;

  TSutraUSources = TList<TSutraUSource>;

  TSutraSpecifiedPressure = record
    NodeNumber: Integer;
    PBC: double;
    UBC: double;
  end;

  TSutraSpecifiedPressures = TList<TSutraSpecifiedPressure>;

  TSutraSpecifiedU = record
    NodeNumber: Integer;
    UBC: double;
  end;

  TSutraSpecifiedUs = TList<TSutraSpecifiedU>;

  TSutraInputReader = class(TObject)
  private
    FFile: TStreamReader;
    FCurrentFile: TStreamReader;
    FEmbeddedFiles: TObjectList<TStreamReader>;
    FSplitter: TStringList;
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
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;
    procedure ReadInputFile;
  end;

implementation

uses
  ModelMuseUtilities;

{ TSutraInputReader }

constructor TSutraInputReader.Create(const FileName: string);
begin
  FFile := TFile.OpenText(FileName);
  FCurrentFile := FFile;
  FEmbeddedFiles := TObjectList<TStreamReader>.Create;
  FSplitter := TStringList.Create;
  FSchedules := TSutraSchedules.Create;
  FFluidSources := TSutraFluidSources.Create;
  FUSources := TSutraUSources.Create;
  FSpecifiedPressures := TSutraSpecifiedPressures.Create;
  FSpecifiedUs := TSutraSpecifiedUs.Create;
end;

destructor TSutraInputReader.Destroy;
begin
  FSpecifiedUs.Free;
  FSpecifiedPressures.Free;
  FUSources.Free;
  FFluidSources.Free;
  FSchedules.Free;
  FSplitter.Free;
  FEmbeddedFiles.Free;
  FFile.Free;
  inherited;
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
    APoint.y := FortranStrToFloat(FSplitter[2]) * FScaleY;
    if FMeshDimensions = smd3 then
    begin
      APoint.z := FortranStrToFloat(FSplitter[2]) * FScaleZ;
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

procedure TSutraInputReader.ReadDataSet18;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraUSource;
begin
  FUSources.Capacity := FNSOU;
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

procedure TSutraInputReader.ReadDataSet19;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraSpecifiedPressure;
begin
  FSpecifiedPressures.Capacity := FNPBC;
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

procedure TSutraInputReader.ReadDataSet20;
var
  BoundaryIndex: Integer;
  ALine: string;
  ABoundary: TSutraSpecifiedU;
begin
  FSpecifiedUs.Capacity := FNSOU;
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

procedure TSutraInputReader.ReadDataSet2A;
var
  ALine: string;
begin
  ALine := ReadNextNonCommentLine;
  ALine := ReplaceText(ALine, '"', '');
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
  FSplitter.DelimitedText := ALine;
  Assert(FSplitter.Count >= 3);
  FNSCH := StrToInt(FSplitter[0]);
  for ScheduleIndex := 0 to FNSCH - 1 do
  begin
    ALine := ReadNextNonCommentLine;
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
            AbsoluteTime := True
          end
          else
          begin
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
            ASchedule.FTimes.Add(TimeSteps.Times[AStep-1]);
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
          ASchedule.FTimes.Add(TimeSteps.Times[AStep-1]);
          for StepIndex := 0 to NSMAX - 1 do
          begin
            AStep := AStep + ISTEPC;
            if AStep >= ISTEPL then
            begin
              break;
            end;
            ASchedule.FTimes.Add(TimeSteps.Times[AStep-1]);
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
begin
  if FNOBS > 0 then
  begin
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

end.
