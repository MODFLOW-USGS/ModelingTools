unit ModpathResponseFileWriterUnit;

interface

uses SysUtils, PhastModelUnit, ModflowPackageSelectionUnit, CustomModflowWriterUnit,
  DataSetUnit, ScreenObjectUnit, ModflowGridUnit, GoPhastTypes;

type
  // MODPATH version 5
  TModpathResponseFileWriter = class(TCustomModflowWriter)
  private
    FOptions: TModpathSelection;
    FNewBudgetFile: Boolean;
    FArchive: Boolean;
    function GetCBF_Option(const AFileName: string): TCompositeBudgetFileOption;
    function CompositeBudgetFileSize: Int64;
    function RespondToLargeBudgetFile(
      CBF_Option: TCompositeBudgetFileOption): string;
    procedure WriteResponse;
    procedure WriteRspFile(NameOfFile: string; const AFileName: string);
  protected
    class function Extension: string; override;
  public
    FLargeBudgetFileResponse: string;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string; NewBudgetFile: boolean);
  end;

  // MODPATH version 6
  TModpathSimFileWriter = class(TCustomModflowWriter)
  private
    FOptions: TModpathSelection;
    FNameFile: string;
    FTimePointOption: Integer;
    FModelName: string;
    FMpathVersion: TMpathVersion;
    FScreenObjects: TScreenObjectList;
    FCellList: TCellAssignmentList;
    FParticleID: Integer;
    FModflowGrid: TModflowGrid;
    FCellNumbers: TThreeDIntegerArray;
    procedure ArchiveOutputFileName(var AFileName: string);
    // Comments.
    procedure WriteDataSet0;
    // ModpathNameFile
    procedure WriteDataSet1(IsArchive: Boolean);
    // ModpathListingFile
    procedure WriteDataSet2(Archive: Boolean);
    {SimulationType TrackingDirection WeakSinkOption WeakSourceOption ,
    ReferenceTimeOption StopOption ParticleGenerationOption TimePointOption
    BudgetOutputOption ZoneArrayOption RetardationOption
    AdvectiveObservationsOption}
    procedure WriteDataSet3;
    // EndpointFile
    procedure WriteDataSet4(Archive: Boolean);
    // [PathlineFile] — only if SimulationType = 2
    procedure WriteDataSet5(Archive: Boolean);
    // [TimeseriesFile] — only if SimulationType = 3
    procedure WriteDataSet6(Archive: Boolean);
    // [AdvectiveObservationsFile] — only if AdvectiveObservationsOption = 2
    // and SimulationType = 3
    procedure WriteDataSet7(Archive: Boolean);
    procedure WriteCheckBudget;
    procedure WriteReferenceTimeOption;
    // [ReferenceTime] — only if ReferenceTimeOption = 1
    procedure WriteReferenceTime;
    procedure WriteStopTimeOption;
    // [StopTime] — only if StopOption = 3
    procedure WriteStopTime;
    // [StartingLocationsFile] — only if ParticleGenerationOption = 2
    procedure WriteDataSet22;
    procedure WriteTimePointOptionMp7;
    procedure WriteTimeCountAndIntervalMp7;
    procedure WriteTimeCountMp7;
    procedure WriteTimesMp7;
    procedure WriteZoneDataOptionMp7;
    // [TimePointCount] — only if TimePointOption = 2 or TimePointOption = 3
    procedure WriteDataSet23;
    // [ReleaseTimeIncrement] — only if TimePointOption =2
    procedure WriteDataSet24;
    // [TimePoints(TimePointCount)] — only if TimePointOption = 3
    procedure WriteDataSet25;
    {Item 26 [CellBudgetCount] – only if BudgetOutputOption = 3
    Item 27 [Grid Layer Row Column] – only if BudgetOutputOption = 3.
    Repeat item 27 CellBudgetCount times.}
    procedure WriteDataSets26and27;
    // [TraceFile] – only if BudgetOutputOption = 4
    procedure WriteTraceFileName(Archive: Boolean);
    // [TraceID] – only if BudgetOutputOption = 4
    procedure WriteTraceID;
    // [StopZone]
    procedure WriteStopZone;
    // [Zone(NCOL,NROW)] –U2DINT
    procedure WriteZones;
    procedure WriteRetardationFactorOptionMp7;

    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
    {Item 32 [RetardationFactor(NCOL,NROW)] –U2DREL
    Item 33 [RetardationFactorCB(NCOL,NROW)] –U2DREL – only if LAYCBD in the MODFLOW Discretization File is not equal to 0}
    procedure WriteRetardation;
    procedure WriteParticleGroupCount;
    procedure WriteParticleGroupName(AScreenObject: TScreenObject);
    procedure WriteReleaseTimes(AScreenObject: TScreenObject);
    procedure WriteStartingLocationFileOption;
    procedure WriteStartingLocations(AScreenObject: TScreenObject);

    procedure SaveFile(NameOfFile: string; IsArchive: Boolean);
    procedure SaveFileMp7(NameOfFile: string; IsArchive: Boolean);
    procedure FillScreenObjectList;
    procedure GetParticleCount(AScreenObject: TScreenObject; var ParticleCount: Integer);
    procedure GetCellCount(AScreenObject: TScreenObject; var CellCount: Integer);

    procedure SpecifyCellNumberArray;
  protected
    function PackageID_Comment(APackage: TModflowPackageSelection): string; override;
  public
    class function Extension: string; override;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  LayerStructureUnit, ModpathStartingLocationsWriter,
  ModpathParticleUnit, frmProgressUnit, Forms, frmGoPhastUnit,
  frmErrorsAndWarningsUnit, ModflowTimeUnit, ArchiveNodeInterface, System.Generics.Collections, FastGEO,
  DataSetNamesUnit;

resourcestring
  StrWritingDataSets32and33 = '  Writing Data Sets 32 and 33.';
  StrThereIsAnIllegal = 'There is an illegal value (less than or equal to ze' +
  'ro) in %s';
  StrInSAllValuesSh = 'In %s, all values should be greater than or equal to ' +
  '1';
  StrLayerRowColumn = 'Layer, Row, Column = (%0:d, %1:d, %2:d)';
  StrBecauseThe0sStr = 'Because the %0:s stress period is not steady-state, ' +
  'the particles will end at the %1:s of the model simulation. See the docum' +
  'entation for StopOption in the MODPATH documentation.';
  StrLast = 'last';
  StrEnd = 'end';
  StrFirst = 'first';
  StrBeginning = 'beginning';
  StrTheMODPATHStopOpti = 'The MODPATH StopOption may not work as expected.';
  StrInvalidMODPATHStop = 'Invalid MODPATH StopZone number';
  StrInMODPATHVersion5 = 'In MODPATH version 5, the zone number in which to ' +
  'stop particles must be greater than 1. Edit this in the MODFLOW Packages ' +
  'and Programs dialog box.';
  StrInvalidMODPATHPart = 'Invalid MODPATH particle release time';
  StrTheLastForward = 'The last particle release time is after the end of th' +
  'e simulation. Edit this in the MODFLOW Packages and Programs dialog box.';
  StrTheLastBackward = 'The last particle release time is before the beginni' +
  'ng of the simulation. Edit this in the MODFLOW Packages and Programs dial' +
  'og box.';
  StrInvalidSimulationT = 'Invalid simulation type';
  StrACombinedPathline = 'A combined pathline and timeseries version is not ' +
  'allowed in this version of MODPATH. Try version 7.';
  StrInvalidMODPATHPartMethod = 'Invalid MODPATH particle specification method';
  StrWhenDISVGridsAre = 'When DISV grids are used, the object position can n' +
  'ot be used to specify the MODPATH particle position.';
  StrInTheFollowingObj = 'In the following objects, the MODPATH particles ar' +
  'e specified to be at the object location. However, the object(s) either d' +
  'on''t have a single elevation formula or don''t consist only of points. B' +
  'ecause of this the particles will be placed at cell centers.';

const
  KDefaultParticleGroupName = 'Default ';

{ TModpathResponseFileWriter }

constructor TModpathResponseFileWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
 inherited Create(Model, EvaluationType);
 FArrayWritingFormat := awfModflow;
 FOptions := Model.ModflowPackages.ModPath;
end;

class function TModpathResponseFileWriter.Extension: string;
begin
  result := '.mprsp';
end;

function TModpathResponseFileWriter.GetCBF_Option(
  const AFileName: string): TCompositeBudgetFileOption;
var
  CompositeBudgetFileName: string;
  BudgetFileName: string;
  CompositeDate: TDateTime;
  BudgetDate: TDateTime;
begin

  if FNewBudgetFile or FArchive then
  begin
    result := cbfGenerateNew;
    Exit;
  end;
  CompositeBudgetFileName := ChangeFileExt(AFileName, '.cbf');
  BudgetFileName := ChangeFileExt(AFileName, StrCbcExt);
  if FileExists(CompositeBudgetFileName) and FileExists(BudgetFileName) then
  begin
    if FileAge(CompositeBudgetFileName, CompositeDate)
      and FileAge(BudgetFileName, BudgetDate) then
    begin
      if (CompositeDate > BudgetDate) then
      begin
        result := cbfUseOldFile;
      end
      else
      begin
        result := cbfGenerateNew;
      end;
    end
    else
    begin
      result := cbfGenerateNew;
    end;
  end
  else
  begin
    result := cbfGenerateNew;
  end;
end;

function TModpathResponseFileWriter.CompositeBudgetFileSize: Int64;
var
  NSTEPS: Int64;
  Grid: TModflowGrid;
  NROW: Int64;
  NLAY: Int64;
  NHLAY: Int64;
  GroupIndex: integer;
  Group: TLayerGroup;
  NRPTS: Int64;
  NREC: Int64;
  NCOL: Int64;

begin
  // based on the subroutine  CBFSIZ in the MODPATH source code.
  NSTEPS := Model.ModflowFullStressPeriods.NumberOfSteps;
  Grid := Model.ModflowGrid;
  NROW := Grid.RowCount;
  NCOL := Grid.ColumnCount;
  NLAY := Model.ModflowLayerCount;
  NHLAY := 0;
  for GroupIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Group := Model.LayerStructure.LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      if Group.AquiferType > 0 then
      begin
        NHLAY := NHLAY + Group.LayerCount;
      end;
    end;
  end;

  NRPTS := (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY;
  NREC := (1 + (1+NRPTS)*NSTEPS);
  result := 4*(NCOL+1)*NREC;
end;

procedure TModpathResponseFileWriter.WriteResponse;
begin
    WriteString('@RESPONSE:');
    NewLine;
end;

procedure TModpathResponseFileWriter.WriteFile(const AFileName: string;
  NewBudgetFile: boolean);
var
  NameOfFile: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMODPATHStop);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMODPATHPart);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMODPATHPartMethod);


  FLargeBudgetFileResponse := '';
  FNewBudgetFile := NewBudgetFile;

  FArchive := False;
  NameOfFile := FileName(AFileName);
  WriteRspFile(NameOfFile, AFileName);

  FArchive := True;
  NameOfFile := NameOfFile + ArchiveExt;
  WriteRspFile(NameOfFile, AFileName);

  Model.AddModpathInputFile(NameOfFile);

end;

procedure TModpathResponseFileWriter.WriteRspFile(NameOfFile: string;
  const AFileName: string);
var
  CBF_Option: TCompositeBudgetFileOption;
  LastReleaseTime: Double;
  ReferenceTime: Real;
  ComputeLocations: Boolean;
  Index: Integer;
begin
  FInputFileName := NameOfFile;
  OpenFile(NameOfFile);
  try
    // interactive input
    WriteString('@[MODPATH 5.0]');
    NewLine;
    for Index := 0 to FOptions.Comments.Count - 1 do
    begin
      WriteString('@ ' + FOptions.Comments[Index]);
      NewLine;
    end;
    // MODPATH name file
    // 'ENTER THE NAME FILE:';
    WriteString('* ENTER THE NAME FILE:');
    NewLine;
    WriteResponse;
    WriteString(ExtractFileName(AFileName));
    NewLine;
    ReferenceTime := 0;
    CBF_Option := GetCBF_Option(AFileName);
    if not Model.ModflowStressPeriods.TransientModel then
    begin
      // 'DO YOU WANT TO STOP COMPUTING PATHS AFTER A SPECIFIED LENGTH OF TIME ?';
      WriteString('* DO YOU WANT TO STOP COMPUTING PATHS AFTER A SPECIFIED LENGTH OF TIME ?');
      NewLine;
      WriteResponse;
      if FOptions.StopAfterMaxTime then
      begin
        WriteString('Y');
      end
      else
      begin
        WriteString('N');
      end;
      NewLine;
      if FOptions.StopAfterMaxTime then
      begin
        // 'ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR';
        WriteString('* ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.MaxTime);
        WriteFloat(1);
        NewLine;
      end;
    end
    else
    begin
      WriteString('* DEFINE A REFERENCE TIME FOR RELEASING PARTICLES ...');
      NewLine;
      WriteString('*   SELECT AN OPTION:');
      NewLine;
      WriteString('*      1 = SPECIFY BY ENTERING A STRESS PERIOD AND TIME STEP');
      NewLine;
      WriteString('*      2 = SPECIFY BY ENTERING A VALUE OF SIMULATION TIME');
      NewLine;
      //  'DEFINE A REFERENCE TIME FOR RELEASING PARTICLES ...';
      //  '  SELECT AN OPTION:';
      //  '     1 = SPECIFY BY ENTERING A STRESS PERIOD AND TIME STEP';
      //  '     2 = SPECIFY BY ENTERING A VALUE OF SIMULATION TIME';
      WriteResponse;
      WriteInteger(2);
      NewLine;
      //  '  ENTER: REFERENCE TIME  &  TIME UNITS CONVERSION FACTOR';
      // Offset the reference time by the beginning of the first stress period.
      WriteString('*   ENTER: REFERENCE TIME  &  TIME UNITS CONVERSION FACTOR');
      NewLine;
      case FOptions.TrackingDirection of
        tdForward:
          begin
            ReferenceTime := FOptions.ReferenceTime;
          end;
        tdBackward:
          begin
            ReferenceTime := FOptions.BackwardsTrackingReleaseTime;
          end;
      else
        //              - PhastModel.ModflowStressPeriods[0].StartTime;
        Assert(False);
      end;
      WriteResponse;
      WriteFloat(ReferenceTime);
      WriteInteger(1);
      NewLine;
      //  '  ENTER: STRESS PERIOD & TIME STEP ';
      //  '  ENTER: RELATIVE TIME WITHIN TIME STEP';
      //  '         (VALUE FROM 0 TO 1)';
      //  'STOP COMPUTING PATHS AT A SPECIFIED VALUE OF TRACKING TIME ?';
      WriteString('* STOP COMPUTING PATHS AT A SPECIFIED VALUE OF TRACKING TIME ?');
      NewLine;
      WriteResponse;
      if FOptions.StopAfterMaxTime then
      begin
        WriteString('Y');
      end
      else
      begin
        WriteString('N');
      end;
      NewLine;
      if FOptions.StopAfterMaxTime then
      begin
        //  'ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR'
        WriteString('*   ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.MaxTime);
        WriteFloat(1);
        NewLine;
      end;
      //  'SPECIFY AN OPTION FOR READING HEAD AND FLOW RATE DATA:';
      //  '  1 = READ STANDARD MODFLOW UNFORMATTED FILES & GENERATE A';
      //  '      COMPOSITE BUDGET FILE';
      //  '  2 = READ FROM AN EXISTING COMPOSITE BUDGET FILE';
      WriteString('* SPECIFY AN OPTION FOR READING HEAD AND FLOW RATE DATA:');
      NewLine;
      WriteString('*   1 = READ STANDARD MODFLOW UNFORMATTED FILES & GENERATE A');
      NewLine;
      WriteString('*       COMPOSITE BUDGET FILE');
      NewLine;
      WriteString('*   2 = READ FROM AN EXISTING COMPOSITE BUDGET FILE');
      NewLine;
      // Create a new CBF file if the CBF file doesn't exist or if
      // is older the the budget file.
      WriteResponse;
      case CBF_Option of
        cbfGenerateNew:
          WriteInteger(1);
        cbfUseOldFile:
          WriteInteger(2);
      else
        Assert(False);
      end;
      NewLine;
    end;
    WriteString('* SELECT THE OUTPUT MODE:');
    NewLine;
    WriteString('*     1 = ENDPOINTS');
    NewLine;
    WriteString('*     2 = PATHLINE');
    NewLine;
    WriteString('*     3 = TIME SERIES');
    NewLine;
    //  'SELECT THE OUTPUT MODE:';
    //  '    1 = ENDPOINTS';
    //  '    2 = PATHLINE';
    //  '    3 = TIME SERIES';
    WriteResponse;
    case FOptions.OutputMode of
      mopEndpoints:
        WriteInteger(1);
      mopPathline:
        WriteInteger(2);
      mopTimeSeries:
        WriteInteger(3);
    else
      Assert(False);
    end;
    NewLine;
    if (FOptions.OutputMode in [mopPathline, mopTimeSeries]) then
    begin
      ComputeLocations := False;
      case FOptions.TimeSeriesMethod of
        tsmUniform:
          ComputeLocations := FOptions.TimeSeriesMaxCount > 0;
        tsmIndividual:
          ComputeLocations := FOptions.OutputTimes.Count > 0;
      else
        Assert(False);
      end;
      if FOptions.OutputMode = mopPathline then
      begin
        //  'DO YOU WANT TO COMPUTE LOCATIONS AT SPECIFIC POINTS IN TIME?';
        WriteString('* DO YOU WANT TO COMPUTE LOCATIONS AT SPECIFIC POINTS IN TIME?');
        NewLine;
        WriteResponse;
        if ComputeLocations then
        begin
          WriteString('Y');
        end
        else
        begin
          WriteString('N');
        end;
        NewLine;
      end;
      if ComputeLocations then
      begin
        //  'HOW SHOULD POINTS IN TIME BE SPECIFIED ?';
        //  '    1 = WITH A CONSTANT TIME INTERVAL';
        //  '    2 = VALUES OF TIME POINTS ARE READ FROM A FILE';
        WriteString('* HOW SHOULD POINTS IN TIME BE SPECIFIED ?');
        NewLine;
        WriteString('*     1 = WITH A CONSTANT TIME INTERVAL');
        NewLine;
        WriteString('*     2 = VALUES OF TIME POINTS ARE READ FROM A FILE');
        NewLine;
        WriteResponse;
        case FOptions.TimeSeriesMethod of
          tsmUniform:
            WriteInteger(1);
          tsmIndividual:
            WriteInteger(2);
        else
          Assert(False);
        end;
        NewLine;
      end;
      if ComputeLocations and (FOptions.TimeSeriesMethod = tsmUniform) then
      begin
        //  'ENTER: TIME INTERVAL & TIME UNITS CONVERSION FACTOR';
        WriteString('* ENTER: TIME INTERVAL & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.TimeSeriesInterval);
        WriteFloat(1);
        NewLine;
        //  'ENTER THE MAXIMUM NUMBER OF TIME POINTS ALLOWED';
        WriteString('* ENTER THE MAXIMUM NUMBER OF TIME POINTS ALLOWED');
        NewLine;
        WriteResponse;
        WriteInteger(FOptions.TimeSeriesMaxCount);
        NewLine;
        LastReleaseTime := FOptions.TimeSeriesInterval * FOptions.TimeSeriesMaxCount;
        case FOptions.TrackingDirection of
          tdForward:
            begin
              if ReferenceTime + LastReleaseTime > Model.ModflowFullStressPeriods.Last.EndTime then
              begin
                frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHPart, StrTheLastForward);
              end;
            end;
          tdBackward:
            begin
              if ReferenceTime - LastReleaseTime < Model.ModflowFullStressPeriods.First.StartTime then
              begin
                frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHPart, StrTheLastBackward);
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    //  'HOW ARE STARTING LOCATIONS TO BE ENTERED?';
    //  '    1 = FROM AN EXISTING DATA FILE';
    //  '    2 = ARRAYS OF PARTICLES WILL BE GENERATED INTERNALLY';
    WriteString('* HOW ARE STARTING LOCATIONS TO BE ENTERED?');
    NewLine;
    WriteString('*     1 = FROM AN EXISTING DATA FILE');
    NewLine;
    WriteString('*     2 = ARRAYS OF PARTICLES WILL BE GENERATED INTERNALLY');
    NewLine;
    WriteResponse;
    WriteInteger(1);
    NewLine;
    //  'ENTER NAME OF DATA FILE CONTAINING STARTING LOCATIONS:';
    //  'DO YOU WANT TO STORE INTERNALLY-GENERATED STARTING LOCATIONS ON DISK ?';
    //  'ENTER A FILE NAME:';
    //  'IN WHICH DIRECTION SHOULD PARTICLES BE TRACKED?';
    //  '    1 = FORWARD IN THE DIRECTION OF FLOW';
    //  '    2 = BACKWARDS TOWARD RECHARGE LOCATIONS';
    WriteString('* IN WHICH DIRECTION SHOULD PARTICLES BE TRACKED?');
    NewLine;
    WriteString('*     1 = FORWARD IN THE DIRECTION OF FLOW');
    NewLine;
    WriteString('*     2 = BACKWARDS TOWARD RECHARGE LOCATIONS');
    NewLine;
    WriteResponse;
    case FOptions.TrackingDirection of
      tdForward:
        WriteInteger(1);
      tdBackward:
        WriteInteger(2);
    else
      Assert(False);
    end;
    NewLine;
    //  'HOW SHOULD PARTICLES BE TREATED WHEN THEY ENTER CELLS WITH INTERNAL SINKS ?';
    //  '    1 = PASS THROUGH WEAK SINK CELLS';
    //  '    2 = STOP AT WEAK SINK CELLS';
    //  '    3 = STOP AT WEAK SINK CELLS THAT EXCEED A SPECIFIED STRENGTH';
    WriteString('* HOW SHOULD PARTICLES BE TREATED WHEN THEY ENTER CELLS WITH INTERNAL SINKS ?');
    NewLine;
    WriteString('*     1 = PASS THROUGH WEAK SINK CELLS');
    NewLine;
    WriteString('*     2 = STOP AT WEAK SINK CELLS');
    NewLine;
    WriteString('*     3 = STOP AT WEAK SINK CELLS THAT EXCEED A SPECIFIED STRENGTH');
    NewLine;
    WriteResponse;
    case FOptions.WeakSink of
      wsPassThrough:
        WriteInteger(1);
      wsStop:
        WriteInteger(2);
      wsThreshold:
        WriteInteger(3);
    else
      Assert(False);
    end;
    NewLine;
    if FOptions.WeakSink = wsThreshold then
    begin
      //  'ENTER A NUMBER BETWEEN 0 AND 1:';
      //  '    (0.0 => NONE OF THE INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)';
      //  '    (1.0 => ALL INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)';
      WriteString('* ENTER A NUMBER BETWEEN 0 AND 1:');
      NewLine;
      WriteString('*     (0.0 => NONE OF THE INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)');
      NewLine;
      WriteString('*     (1.0 => ALL INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)');
      NewLine;
      WriteResponse;
      WriteFloat(FOptions.WeakSinkThreshold);
      NewLine;
    end;
    WriteString('* DO YOU WANT TO STOP PARTICLES WHENEVER THEY ENTER ONE SPECIFIC ZONE ?');
    NewLine;
    //  'DO YOU WANT TO STOP PARTICLES WHENEVER THEY ENTER ONE SPECIFIC ZONE ?';
    WriteResponse;
    if FOptions.StopInZone then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;
    if FOptions.StopInZone then
    begin
      //  'ENTER THE ZONE NUMBER (MUST BE > 1):';
      WriteString('* ENTER THE ZONE NUMBER (MUST BE > 1):');
      NewLine;
      WriteResponse;
      WriteInteger(FOptions.StopZoneNumber);
      if FOptions.StopZoneNumber <= 1 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHStop, StrInMODPATHVersion5);
      end;
      NewLine;
      if FOptions.OutputMode = mopEndpoints then
      begin
        //  'SPECIFY WHICH ENDPOINTS TO RECORD:';
        //  '   1 = ENDPOINT DATA RECORDED FOR ALL PARTICLES';
        //  '   2 = ENDPOINT DATA RECORDED ONLY FOR PARTICLES';
        //  '        TERMINATING IN ZONE ';
        WriteResponse;
        case FOptions.EndpointWrite of
          ewAll:
            WriteInteger(1);
          ewInStoppingZone:
            WriteInteger(2);
        else
          Assert(False);
        end;
        NewLine;
      end;
    end;
    FLargeBudgetFileResponse := RespondToLargeBudgetFile(CBF_Option);
    if FOptions.StopInZone then
    begin
      //  'DO YOU WANT TO CHANGE ANY OF THE ZONE CODES IN THE IBOUND ARRAY ?';
      WriteString('* DO YOU WANT TO CHANGE ANY OF THE ZONE CODES IN THE IBOUND ARRAY ?');
      NewLine;
      WriteResponse;
      WriteString('N');
      NewLine;
    end;
    //  'DO YOU WANT TO COMPUTE VOLUMETRIC BUDGETS FOR ALL CELLS ?';
    WriteString('* DO YOU WANT TO COMPUTE VOLUMETRIC BUDGETS FOR ALL CELLS ?');
    NewLine;
    WriteResponse;
    if FOptions.ComputeBudgetInAllCells then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;
    if FOptions.ComputeBudgetInAllCells then
    begin
      //  'SPECIFY AN ERROR TOLERANCE (IN PERCENT):';
      WriteString('* SPECIFY AN ERROR TOLERANCE (IN PERCENT):');
      NewLine;
      WriteResponse;
      WriteFloat(FOptions.ErrorTolerance);
      NewLine;
    end;
    //  ' DO YOU WANT TO CHECK DATA CELL BY CELL ?';
    WriteString('*  DO YOU WANT TO CHECK DATA CELL BY CELL ?');
    NewLine;
    WriteResponse;
    WriteString('N');
    NewLine;
    //  'SUMMARIZE FINAL STATUS OF PARTICLES IN SUMMARY.PTH FILE ?';
    WriteString('* SUMMARIZE FINAL STATUS OF PARTICLES IN SUMMARY.PTH FILE ?');
    NewLine;
    WriteResponse;
    if FOptions.Summarize then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;
  finally
    CloseFile;
  end;
end;

function TModpathResponseFileWriter.RespondToLargeBudgetFile(
  CBF_Option: TCompositeBudgetFileOption): string;
const
  MAXSIZ = 150000000;
var
  BigFile: Boolean;
  CBFileSize: Int64;
begin
  result := '';
  if CBF_Option = cbfGenerateNew then
  begin
    CBFileSize := CompositeBudgetFileSize;
    if FOptions.MaximumSize = 0 then
    begin
      BigFile := CBFileSize > MAXSIZ;
    end
    else
    begin
      BigFile := CBFileSize > FOptions.MaximumSize;
    end;
    if BigFile then
    begin
      //        WriteString('* THIS RUN WILL GENERATE A COMPOSITE BUDGET FILE THAT CONTAINS:');
      //        NewLine;
      //        KCBFileSize := CBFileSize / 1024;
      //        MCBFileSize := KCBFileSize/ 1024;
      //        if KCBFileSize < 500 then
      //        begin
      //          WriteString('* ' + IntToStr(CBFileSize) + ' BYTES ('
      //            + FloatToStr(KCBFileSize) + ' KB)');
      //          NewLine;
      //        end
      //        else
      //        begin
      //          WriteString('* ' + IntToStr(CBFileSize) + ' BYTES ('
      //            + FloatToStr(MCBFileSize) + ' MB)');
      //          NewLine;
      //        end;
      //        WriteString('*  YOU CAN CONTINUE OR STOP NOW.');
      //        NewLine;
      //        WriteString('*  SELECT AN OPTION:');
      //        NewLine;
      //        WriteString('*       1 = CONTINUE');
      //        NewLine;
      //        WriteString('*       2 = STOP NOW, DO NOT GENERATE THE FILE');
      //        NewLine;
      //        WriteResponse;
      if FOptions.MakeBigBudgetFile then
      begin
        result := '1';
      end
      else
      begin
        result := '2';
      end;
    end;
  end;
end;

{ TModpathSimFileWriter }

procedure TModpathSimFileWriter.ArchiveOutputFileName(var AFileName: string);
begin
  AFileName := '..\..\output\output.' + FModelName + '_Modpath\' + AFileName;
end;

constructor TModpathSimFileWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited Create(Model, EvaluationType);
  FArrayWritingFormat := awfModflow;
  FOptions := Model.ModflowPackages.ModPath;
  FMpathVersion := FOptions.MpathVersion;
  FScreenObjects := TScreenObjectList.Create;
  FCellList := TCellAssignmentList.Create;
end;

destructor TModpathSimFileWriter.Destroy;
begin
  FCellList.Free;
  FScreenObjects.Free;
  inherited;
end;

class function TModpathSimFileWriter.Extension: string;
begin
  result := '.mpsim';
end;

procedure TModpathSimFileWriter.FillScreenObjectList;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[Index];
    if (not ScreenObject.Deleted) and ScreenObject.ModpathParticles.Used then
    begin
      FScreenObjects.Add(ScreenObject);
    end;
  end;
end;

function TModpathSimFileWriter.PackageID_Comment(
  APackage: TModflowPackageSelection): string;
begin
  result := File_Comment(APackage.PackageIdentifier + ' Simulation file');
end;

procedure TModpathSimFileWriter.WriteZoneDataOptionMp7;
var
  ZoneDataOption: Integer;
begin
  ZoneDataOption := Ord(FOptions.StopInZone) + 1;
  WriteInteger(ZoneDataOption);
  WriteString(' # Item 20: ZoneDataOption');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteCheckBudget;
var
  BudgetDataArray: TDataArray;
  CellBudgetCount: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IDomainDataArray: TDataArray;
  CellList: TGenericIntegerList;
  CellID: Integer;
  CellIndex: Integer;
begin
   // See WriteDataSets26and27

  BudgetDataArray := Model.DataArrayManager.GetDataSetByName(KModpathBudget);
  BudgetDataArray.Initialize;
  if Model.ModelSelection = msModflow2015 then
  begin
    IDomainDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    IDomainDataArray.Initialize;
  end
  else
  begin
    IDomainDataArray := nil;
  end;

//  CellBudgetCount := 0;
  CellList := TGenericIntegerList.Create;
  CellID := 0;
  try
    for LayerIndex := 0 to BudgetDataArray.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to BudgetDataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to BudgetDataArray.ColumnCount - 1 do
          begin

            if (IDomainDataArray = nil) or (IDomainDataArray.
              IntegerData[LayerIndex, RowIndex, ColIndex] > 0) then
            begin
              Inc(CellID);
              if BudgetDataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
              begin
//                Inc(CellBudgetCount);
                CellList.Add(CellID);
              end;
            end;
          end;
        end;
      end;
    end;
    CellBudgetCount := CellList.Count;
    WriteInteger(CellBudgetCount);
    WriteString(' # Item 9: BudgetCellCount');
    NewLine;

    for CellIndex := 0 to CellList.Count - 1 do
    begin
      WriteInteger(CellList[CellIndex]);
      NewLine;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: integer;
begin
  Assert(DataArray <> nil);
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, DataSetName + ' '
        + Model.ModflowLayerBottomDescription(LayerIndex), StrNoValueAssigned, DataSetName);
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet0;
begin
  WriteCommentLine(PackageID_Comment(FOptions));
  WriteCommentLines(FOptions.Comments);
end;

procedure TModpathSimFileWriter.WriteDataSet1(IsArchive: Boolean);
begin
  frmProgressMM.AddMessage(StrWritingDataSet1);
  if IsArchive then
  begin
    Model.AddModpathInputFile(FNameFile + ArchiveExt);
  end;
  WriteString(ExtractFileName(FNameFile));
//  WriteString(' # Data Set 1. ModpathNameFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteStartingLocationFileOption;
begin
  WriteString('INTERNAL');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteStartingLocations(
  AScreenObject: TScreenObject);
var
  LocationStyle: Integer;
  Particles: TParticles;
  ParticleCount: Integer;
  ParticleIdOption: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
  IDomainDataArray: TDataArray;
//  CellCount: Integer;
  ReleaseTimes: TModpathTimes;
  Layer: Integer;
  ParticleIndex: Integer;
  TimeIndex: Integer;
  TimeOffset: Double;
  APoint: TPoint2D;
  ParticleLines: TParticleLines;
  FStartTime: Double;
  FEndTime: Double;
  LocalXYZ: string;
  LocalX: double;
  LocalY: double;
  GlobalZ: Double;
  LocalZ: double;
  CellNumber: Integer;
  Drape: Integer;
begin
  WriteInteger(1);
  WriteString(' # Starting Location Data Item 1: InputStyle');
  NewLine;

  if Model.ModelSelection = msModflow2015 then
  begin
    LocationStyle := 2;
    IDomainDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    IDomainDataArray.Initialize
  end
  else
  begin
    LocationStyle := 1;
    IDomainDataArray := nil;
  end;
  WriteInteger(LocationStyle);
  WriteString(' # Starting Location Data Item 2: LocationStyle');
  NewLine;

  Particles := AScreenObject.ModpathParticles.Particles;

  FStartTime := Model.ModflowFullStressPeriods.First.StartTime;
  FEndTime := Model.ModflowFullStressPeriods.Last.EndTime;
  ParticleLines := TParticleLines.Create(AScreenObject,
    FOptions.TrackingDirection,
    FStartTime, FEndTime);
  try

    GetParticleCount(AScreenObject, ParticleCount);
    ReleaseTimes := AScreenObject.ModpathParticles.ReleaseTimes;
    ParticleCount := ParticleCount * ReleaseTimes.Count;
    WriteInteger(ParticleCount);
    ParticleIdOption := 1;
    WriteInteger(ParticleIdOption);
    WriteString(' # Starting Location Data Item 3: ParticleCount ParticleIdOption');
    NewLine;

    Drape := 0;
    for CellIndex := 0 to FCellList.Count - 1 do
    begin
      ACell := FCellList[CellIndex];
      if Model.IsLayerSimulated(ACell.Layer) then
      begin
        if (IDomainDataArray = nil) or
          (IDomainDataArray.IntegerData[ACell.Layer, ACell.Row, ACell.Column] > 0) then
        begin
          Layer := Model.DataSetLayerToModflowLayer(ACell.Layer);
          for TimeIndex := 0 to ReleaseTimes.Count -1 do
          begin
            TimeOffset := ReleaseTimes[TimeIndex].Time;
            if Particles <> nil then
            begin
              for ParticleIndex := 0 to Particles.Count - 1 do
              begin
                Inc(FParticleID);
                WriteInteger(FParticleID);
                if LocationStyle = 1 then
                begin
                  WriteInteger(Layer);
                  WriteInteger(ACell.Row + 1);
                  WriteInteger(ACell.Column + 1);
                end
                else
                begin
                  CellNumber := FCellNumbers[ACell.Layer, ACell.Row, ACell.Column];
                  WriteInteger(CellNumber);
                end;
                if Model.IsLayerSimulated(ACell.Layer) then
                begin
                  LocalXYZ := ParticleLines.SimulatedLocations[ParticleIndex];
                end
                else
                begin
                  LocalXYZ := ParticleLines.NonSimulatedLocations[ParticleIndex];
                end;
                WriteString(' ' + LocalXYZ);
                WriteFloat(TimeOffset);
                WriteInteger(Drape);

  //              Inc(ParticleCount);
  //              ParticleLabel := ParticleLabelBase + Format(FormatString, [ParticleCount]);
  //              WriteString(ParticleLabel);
  //              WriteString(' # Data Set 12: Grid Layer Row Column LocalX LocalY LocalZ Label');
                NewLine;
              end;
            end
            else
            begin
              if FModflowGrid = nil then
              begin
                Continue;
                frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHPartMethod,
                  StrWhenDISVGridsAre, AScreenObject);
              end;

              Inc(FParticleID);
              WriteInteger(FParticleID);
              if LocationStyle = 1 then
              begin
                WriteInteger(Layer);
                WriteInteger(ACell.Row + 1);
                WriteInteger(ACell.Column + 1);
              end
              else
              begin
                CellNumber := FCellNumbers[ACell.Layer, ACell.Row, ACell.Column];
                WriteInteger(CellNumber);
              end;
              APoint := AScreenObject.Points[ACell.Section];

              APoint := FModflowGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
              LocalX := (APoint.x - FModflowGrid.ColumnPosition[ACell.Column]) / FModflowGrid.ColumnWidth[ACell.Column];
              WriteFloat(LocalX);
              LocalY := 1 - ((FModflowGrid.RowPosition[ACell.Row] - APoint.y) / FModflowGrid.RowWidth[ACell.Row]);
              WriteFloat(LocalY);
              GlobalZ := AScreenObject.Higher3DElevations[Model][ACell.Layer, ACell.Row, ACell.Column];
              LocalZ := (GlobalZ - FModflowGrid.CellElevation[ACell.Column, ACell.Row, ACell.Layer + 1]) / FModflowGrid.CellThickness[ACell.Column, ACell.Row, ACell.Layer];
              if not Model.IsLayerSimulated(ACell.Layer) then
              begin
                LocalZ := LocalZ - 1;
              end;
              WriteFloat(LocalZ);
              WriteFloat(TimeOffset);
              WriteInteger(Drape);
  //            Inc(ParticleCount);
  //            ParticleLabel := ParticleLabelBase + Format(FormatString, [ParticleCount]);
  //            WriteString(ParticleLabel);
  //            WriteString(' # Data Set 12: Grid Layer Row Column LocalX LocalY LocalZ Label');
              NewLine;
            end;
          end;



        end;
      end;
    end;
  finally
    ParticleLines.Free;
  end;

end;

procedure TModpathSimFileWriter.WriteStopTime;
var
  StopTime: Real;
begin
  if FOptions.StopOption = soTrackingTime then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet10);
    StopTime := FOptions.StopTime;
    WriteFloat(StopTime);
    if FMpathVersion = mp6 then
    begin
      WriteString(' # Data Set 10: StopTime');
    end
    else
    begin
      WriteString(' # Item 15: StopTime');
    end;
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet2(Archive: Boolean);
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet2);
  AFileName := ChangeFileExt(FNameFile, '.mplst');
  Model.AddModpathOutputFile(AFileName);
  AFileName := ExtractFileName(AFileName);
  if Archive then
  begin
    ArchiveOutputFileName(AFileName);
  end;
  WriteString(AFileName);
//  WriteString(' # Data Set 2. ModpathListingFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet22;
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet22);
  AFileName := ChangeFileExt(FNameFile,
    TModpathStartingLocationsWriter.Extension);
  Model.AddModpathInputFile(AFileName);
  AFileName := ExtractFileName(AFileName);
  WriteString(AFileName);
//  WriteString(' # Data Set 22. StartingLocationsFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet23;
var
  TimePointCount: integer;
begin
  if FTimePointOption in [2,3] then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet23);
    TimePointCount := -1;
    case FTimePointOption of
      2:
        begin
          TimePointCount := FOptions.TimeSeriesMaxCount;
        end;
      3:
        begin
          TimePointCount := FOptions.OutputTimes.Count;
        end;
      else Assert(False);
    end;
    WriteInteger(TimePointCount);
    WriteString(' # Data Set 23: TimePointCount');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet24;
var
  ReleaseTimeIncrement: Double;
begin
  if FTimePointOption = 2 then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet24);
    ReleaseTimeIncrement := FOptions.TimeSeriesInterval;
    WriteFloat(ReleaseTimeIncrement);
    WriteString(' # Data Set 24: ReleaseTimeIncrement');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet25;
var
  Index: Integer;
  Item: TModpathTimeItem;
begin
  if FTimePointOption = 3 then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet25);
    for Index := 0 to FOptions.OutputTimes.Count - 1 do
    begin
      Item := FOptions.
        OutputTimes.Items[Index] as TModpathTimeItem;
      WriteFloat(Item.Time);
      if Index = FOptions.OutputTimes.Count - 1 then
      begin
        WriteString(' # Data Set 25: TimePoints');
      end;
      if (((Index + 1) mod 10) = 0) or (Index = FOptions.OutputTimes.Count - 1) then
      begin
        NewLine;
      end;
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteTimeCountMp7;
var
  TimePointCount: integer;
begin
  if FTimePointOption = 2 then
  begin
//    frmProgressMM.AddMessage(StrWritingDataSet23);
//    TimePointCount := -1;
    TimePointCount := FOptions.OutputTimes.Count;
    WriteInteger(TimePointCount);
    WriteString(' # Item 18: TimePointCount');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteTimeCountAndIntervalMp7;
var
  TimePointInterval: Double;
  TimePointCount: integer;
begin
  if FTimePointOption = 1 then
  begin
//    frmProgressMM.AddMessage(StrWritingDataSet23);
    TimePointCount := FOptions.TimeSeriesMaxCount;
    TimePointInterval := FOptions.TimeSeriesInterval;
    WriteInteger(TimePointCount);
    WriteFloat(TimePointInterval);
    WriteString(' # Data Set 17: TimePointCount TimePointInterval');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteTimePointOptionMp7;
//var
//  TimePointOption: Integer;
begin
  if (FOptions.OutputMode in [mopTimeSeries, mopPathAndTime]) then
  begin
    // TimePointOption in version 7 is one less than the equivalents in version 6.
    case FOptions.TimeSeriesMethod of
      tsmUniform:
        begin
          if FOptions.TimeSeriesMaxCount > 0 then
          begin
            FTimePointOption := 1;
          end
          else
          begin
            FTimePointOption := 0;
          end;
        end;
      tsmIndividual:
        begin
          if FOptions.OutputTimes.Count > 0 then
          begin
            FTimePointOption := 2;
          end
          else
          begin
            FTimePointOption := 0;
          end;
        end;
      else Assert(False);
    end;
    // TimePointOption in version 7 is one less than the equivalents in version 6.
//    TimePointOption := FTimePointOption - 1;
    WriteInteger(FTimePointOption);
    WriteString(' # Item 16: TimePointOption');
    NewLine;
  end
  else
  begin
    FTimePointOption := 0;
  end;
end;

procedure TModpathSimFileWriter.WriteTimesMp7;
var
  Index: Integer;
  Item: TModpathTimeItem;
begin
  if FTimePointOption = 2 then
  begin
//    frmProgressMM.AddMessage(StrWritingDataSet25);
    for Index := 0 to FOptions.OutputTimes.Count - 1 do
    begin
      Item := FOptions.
        OutputTimes.Items[Index] as TModpathTimeItem;
      WriteFloat(Item.Time);
      if Index = FOptions.OutputTimes.Count - 1 then
      begin
        WriteString(' # Item 19: TimePoints');
      end;
      if (((Index + 1) mod 10) = 0) or (Index = FOptions.OutputTimes.Count - 1) then
      begin
        NewLine;
      end;
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteTraceFileName(Archive: Boolean);
var
  AFileName: string;
begin
  if FOptions.BudgetChecking = bcTrace then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet28);
    AFileName := ChangeFileExt(FNameFile, '.trace');
    frmGoPhast.PhastModel.AddModpathOutputFile(AFileName);
    AFileName := ExtractFileName(AFileName);
    if Archive then
    begin
      ArchiveOutputFileName(AFileName);
    end;
    WriteString(AFileName);
//    WriteString(' # Data Set 29. TraceFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteTraceID;
var
  TraceID: integer;
  TraceParticleGroup: Integer;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  TotalCount: Integer;
  ParticleCount: Integer;
begin
  if FOptions.BudgetChecking = bcTrace then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet29);

    TraceParticleGroup := 0;
    TraceID := FOptions.TraceID;


    if FMpathVersion = mp7 then
    begin
      TotalCount := 0;
      for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
      begin
        AScreenObject := FScreenObjects[ScreenObjectIndex];
        GetParticleCount(AScreenObject, ParticleCount);
        Inc(TotalCount, ParticleCount);
        if TraceID <= TotalCount then
        begin
          TraceParticleGroup := ScreenObjectIndex + 1;
          break;
        end;
      end;
      WriteInteger(TraceParticleGroup);
    end;

    WriteInteger(TraceID);
    if FMpathVersion = mp7 then
    begin
      WriteString(' # Data Set 29: TraceParticleGroup TraceID');
    end
    else
    begin
      WriteString(' # Data Set 29: TraceID');
    end;
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet3;
const
  ReferenceTimeOption = 1;
  ParticleGenerationOption = 2;
  // TimeSeriesOutputOption has not yet been implemented in the released
  // version of MODPATH. When it is, this should be changed.
  TimeSeriesOutputOption = 0;
var
  SimulationType: integer;
  TrackingDirection: Integer;
  WeakSinkOption: integer;
  WeakSouceOption: integer;
  StopOption: integer;
  BudgetOutputOption: Integer;
  ZoneArrayOption: Integer;
  RetardationOption: Integer;
  AdvectiveObservationsOption: Integer;
  StressPeriod: TModflowStressPeriod;
  Warning: string;
  TraceMode: Integer;
begin
  frmProgressMM.AddMessage(StrWritingDataSet3);
  SimulationType := Ord(FOptions.OutputMode) + 1;
  if (SimulationType = 4) and (FMpathVersion in [mp5, mp6]) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidSimulationT,
      StrACombinedPathline);
  end;
  TrackingDirection := Ord(FOptions.TrackingDirection) + 1;
  WeakSinkOption := Ord(FOptions.WeakSink) + 1;
  if WeakSinkOption > 2 then
  begin
    WeakSinkOption := 2;
  end;
  WeakSouceOption := Ord(FOptions.WeakSource) + 1;
  if WeakSouceOption > 2 then
  begin
    WeakSouceOption := 2;
  end;
  StopOption := Ord(FOptions.StopOption) + 1;
  if (StopOption = 2) and (FMpathVersion <> mp7) then
  begin
    if TrackingDirection = 1 then
    begin
      // forward tracking
      StressPeriod := Model.ModflowFullStressPeriods.Last;
    end
    else
    begin
      // backwards tracking
      Assert(TrackingDirection = 2);
      StressPeriod := Model.ModflowFullStressPeriods.First;
    end;
    if StressPeriod.StressPeriodType <> sptSteadyState then
    begin
      if TrackingDirection = 1 then
      begin
        // forward tracking
        Warning := Format(StrBecauseThe0sStr, [StrLast, StrEnd]);
      end
      else
      begin
        // backwards tracking
        Assert(TrackingDirection = 2);
        Warning := Format(StrBecauseThe0sStr, [StrFirst, StrBeginning]);
      end;
      frmErrorsAndWarnings.AddWarning(Model, StrTheMODPATHStopOpti, Warning);
    end;
  end;
  FTimePointOption := 0;
  if (FOptions.OutputMode in [mopPathline, mopTimeSeries]) then
  begin
    case FOptions.TimeSeriesMethod of
      tsmUniform:
        begin
          if FOptions.TimeSeriesMaxCount > 0 then
          begin
            FTimePointOption := 2;
          end
          else
          begin
            FTimePointOption := 1;
          end;
        end;
      tsmIndividual:
        begin
          if FOptions.OutputTimes.Count > 0 then
          begin
            FTimePointOption := 3;
          end
          else
          begin
            FTimePointOption := 1;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    FTimePointOption := 1;
  end;
  BudgetOutputOption := Ord(FOptions.BudgetChecking) + 1;
  TraceMode := 0;
  if FMpathVersion = mp7 then
  begin
    if FOptions.BudgetChecking = bcTrace then
    begin
      BudgetOutputOption := 3;
      TraceMode := 1;
    end;
  end;


  ZoneArrayOption := Ord(FOptions.StopInZone) + 1;
  RetardationOption := Ord(FOptions.RetardationOption) + 1;
  if FOptions.OutputMode = mopTimeSeries then
  begin
    AdvectiveObservationsOption := Ord(FOptions.AdvectiveObservations) + 1;
  end
  else
  begin
    AdvectiveObservationsOption := 1;
  end;
  WriteInteger(SimulationType);
  WriteInteger(TrackingDirection);
  WriteInteger(WeakSinkOption);
  WriteInteger(WeakSouceOption);
  if FMpathVersion <> mp7 then
  begin
    WriteInteger(ReferenceTimeOption);
    WriteInteger(StopOption);
    WriteInteger(ParticleGenerationOption);
    WriteInteger(FTimePointOption);
  end;
  WriteInteger(BudgetOutputOption);
  if FMpathVersion <> mp7 then
  begin
    WriteInteger(ZoneArrayOption);
    WriteInteger(RetardationOption);
    WriteInteger(AdvectiveObservationsOption);
  end;
  if FMpathVersion = mp7 then
  begin
    WriteInteger(TraceMode);
    WriteInteger(TimeSeriesOutputOption);
  end;
  if FMpathVersion <> mp7 then
  begin
    WriteString(' # Data Set 3: SimulationType, TrackingDirection, '
      + 'WeakSinkOption, WeakSouceOption, ReferenceTimeOption, StopOption, '
      + 'ParticleGenerationOption, TimePointOption, BudgetOutputOption, '
      + 'ZoneArrayOption, RetardationOption, AdvectiveObservationsOption');
  end
  else
  begin
    WriteString(' # Data Set 3: SimulationType, TrackingDirection, '
      + 'WeakSinkOption, WeakSouceOption, BudgetOutputOption, '
      + 'TraceMode, TimeSeriesOutputOption');
  end;
  NewLine;
end;

procedure TModpathSimFileWriter.WriteStopZone;
var
  StopZone: integer;
begin
  if FOptions.StopInZone then
  begin
//    frmProgressMM.AddMessage(StrWritingDataSet30);
    StopZone := FOptions.StopZoneNumber;
    WriteInteger(StopZone);
    if FMpathVersion = mp6 then
    begin
      WriteString(' # Data Set 30: StopZone');
    end
    else
    begin
      WriteString(' # Data Set 21: StopZone');
    end;
    NewLine;

    // removed from code
//    if StopZone >= 1 then
//    begin
//      // undocumented. See lines 346-366 of MP6MPBAS1.FOR
//      StopZone := FOptions.StopZoneNumber;
//      WriteInteger(StopZone);
//      WriteString(' # Data Set 30a: StopZone');
//      NewLine;
//    end;
  end;
end;

procedure TModpathSimFileWriter.WriteZones;
var
  ZoneDataArray: TDataArray;
  ID: string;
begin
  if FOptions.StopInZone then
  begin
//    frmProgressMM.AddMessage(StrWritingDataSet31);
    ZoneDataArray := Model.DataArrayManager.GetDataSetByName(StrModpathZone);
    if FMpathVersion = mp6 then
    begin
      ID :=  'Data Set 31: Zone';
    end
    else
    begin
      ID :=  'Item 22: Zones';
    end;
    WriteDataSet(ID, ZoneDataArray);
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet4(Archive: Boolean);
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet4);
  AFileName := ChangeFileExt(FNameFile, '.end');
  Model.AddModpathOutputFile(AFileName);
  AFileName := ExtractFileName(AFileName);
  if Archive then
  begin
    ArchiveOutputFileName(AFileName);
  end;
  WriteString(AFileName);
//  WriteString(' # Data Set 4. EndpointFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet5(Archive: Boolean);
var
  AFileName: string;
begin
  if FOptions.OutputMode in [mopPathline, mopPathAndTime] then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet5);
    AFileName := ChangeFileExt(FNameFile, '.path');
    Model.AddModpathOutputFile(AFileName);
    AFileName := ExtractFileName(AFileName);
    if Archive then
    begin
      ArchiveOutputFileName(AFileName);
    end;
    WriteString(AFileName);
//    WriteString(' # Data Set 5. PathlineFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet6(Archive: Boolean);
var
  AFileName: string;
begin
  if FOptions.OutputMode in [mopTimeSeries, mopPathAndTime] then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet6);
    AFileName := ChangeFileExt(FNameFile, '.ts');
    Model.AddModpathOutputFile(AFileName);
    AFileName := ExtractFileName(AFileName);
    if Archive then
    begin
      ArchiveOutputFileName(AFileName);
    end;
    WriteString(AFileName);
//    WriteString(' # Data Set 6. TimeSeriesFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet7(Archive: Boolean);
var
  AFileName: string;
begin
  if FMpathVersion = mp7 then
  begin
    Exit;
  end;
  if (FOptions.AdvectiveObservations = aoAll)
    and (FOptions.OutputMode = mopTimeSeries) then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet7);
    AFileName := ChangeFileExt(FNameFile, '.advobs');
    Model.AddModpathOutputFile(AFileName);
    if Archive then
    begin
      ArchiveOutputFileName(AFileName);
    end;
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
//    WriteString(' # Data Set 7. AdvectionObservationsFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteReferenceTime;
var
  ReferenceTime: Real;
begin
  // ReferenceTimeOption is always set to 1
  // so this data set is always exported and
  // data set 9 is never exported.
  frmProgressMM.AddMessage(StrWritingDataSet8);
  ReferenceTime := FOptions.ReferenceTime
    - Model.ModflowStressPeriods[0].StartTime;
  WriteFloat(ReferenceTime);
  if FMpathVersion = mp6 then
  begin
    WriteString(' # Data Set 8: ReferenceTime');
  end
  else
  begin
    WriteString(' # Item 12: ReferenceTime');
  end;
  NewLine;
end;

procedure TModpathSimFileWriter.WriteReferenceTimeOption;
const
  ReferenceTimeOption = 1;
begin
  WriteInteger(ReferenceTimeOption);
  WriteString(' # Item 11: ReferenceTimeOption');
  NewLIne;
end;

procedure TModpathSimFileWriter.WriteReleaseTimes(
  AScreenObject: TScreenObject);
var
//  ReleaseTimes: TModpathTimes;
  ReleaseOption: Integer;
//  ReleaseTimeItem: TModpathTimeItem;
//  ReleaseTimeCount: Integer;
//  TimeIndex: Integer;
  ReleaseTime: double;
begin
//  ReleaseTimes := AScreenObject.ModpathParticles.ReleaseTimes;
//  if ReleaseTimes.Count = 1 then
//  begin
    ReleaseOption := 1;
//  end
//  else
//  begin
//    ReleaseOption := 3;
//  end;
  WriteInteger(ReleaseOption);
  WriteString(' # Item 27: ReleaseOption');
  NewLine;

//  if ReleaseOption = 1 then
//  begin
//    ReleaseTimeItem := ReleaseTimes[0];
    ReleaseTime := 0;
    WriteFloat(ReleaseTime);
    WriteString(' # Item 28: ReleaseTime');
    NewLine;
//  end
//  else
//  begin
//    ReleaseTimeCount := ReleaseTimes.Count;
//    WriteInteger(ReleaseTimeCount);
//    WriteString(' # Item 30: ReleaseTimeCount');
//    NewLine;
//
//    for TimeIndex := 0 to ReleaseTimes.Count - 1 do
//    begin
//      ReleaseTimeItem := ReleaseTimes[TimeIndex];
//      WriteFloat(ReleaseTimeItem.Time);
//      WriteString(' # Item 28: ReleaseTime');
//      NewLine;
//    end;
//  end;
end;

procedure TModpathSimFileWriter.WriteRetardationFactorOptionMp7;
var
  RetardationFactorOption: Integer;
begin
  RetardationFactorOption := Ord(FOptions.RetardationOption) + 1;
  WriteInteger(RetardationFactorOption);
  NewLine;
end;

procedure TModpathSimFileWriter.WriteStopTimeOption;
var
  StopTimeOption: Integer;
begin
  StopTimeOption := Ord(FOptions.StopOption) + 1;
  WriteInteger(StopTimeOption);
  WriteString(' # Item 14: StopTimeOption');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSets26and27;
const
  Grid = 1;
var
  DataArray: TDataArray;
  CellBudgetCount: integer;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
begin
  if FOptions.BudgetChecking = bcList then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet26);
    DataArray := Model.DataArrayManager.GetDataSetByName(KModpathBudget);
    DataArray.Initialize;
    CellBudgetCount := 0;
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              Inc(CellBudgetCount);
            end;
          end;
        end;
      end;
    end;
    WriteInteger(CellBudgetCount);
    WriteString(' # Data Set 26: CellBudgetCount');
    NewLine;

    frmProgressMM.AddMessage(StrWritingDataSet27);
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              WriteInteger(Grid);
              WriteInteger(LayerIndex+1);
              WriteInteger(RowIndex+1);
              WriteInteger(ColIndex+1);
              WriteString(' # Data Set 27: Grid, Layer, Row, Column');
              NewLine;
            end;
          end;
        end;
      end;
    end;

  end;
end;

procedure TModpathSimFileWriter.WriteRetardation;
var
  RetardationDataArray: TDataArray;
  LayerIndex: Integer;
  DataSetName: string;
  ActiveDataArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  AValue: Double;
  ErrorRoot: string;
  WarningRoot: string;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    if FOptions.RetardationOption = roUsed then
    begin
      frmProgressMM.AddMessage(StrWritingDataSets32and33);
      RetardationDataArray := Model.DataArrayManager.GetDataSetByName(KModpathRetardation);
      Assert(RetardationDataArray <> nil);
      for LayerIndex := 0 to Model.LayerCount - 1 do
      begin
        if Model.IsLayerSimulated(LayerIndex) then
        begin
          if FMpathVersion = mp6 then
          begin
            DataSetName := 'Data Set 32: RetardationFactor ';
          end
          else
          begin
            DataSetName := 'Item 24: Retardation ';
          end;
        end
        else
        begin
          DataSetName := 'Data Set 32: RetardationFactorCB ';
        end;
        WriteArray(RetardationDataArray, LayerIndex, DataSetName
          + Model.ModflowLayerBottomDescription(LayerIndex), StrNoValueAssigned, DataSetName);
      end;

      ErrorRoot := Format(StrThereIsAnIllegal,
        [RetardationDataArray.DisplayName]);
      WarningRoot := Format(StrInSAllValuesSh,
        [RetardationDataArray.DisplayName]);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
      frmErrorsAndWarnings.RemoveWarningGroup(Model, WarningRoot);
      ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
      for LayerIndex := 0 to Model.LayerCount - 1 do
      begin
        for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
          begin
            if ActiveDataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              AValue := RetardationDataArray.RealData[LayerIndex,RowIndex,ColIndex];
              if AValue <= 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, ErrorRoot,
                  Format(StrLayerRowColumn,
                  [LayerIndex+1, RowIndex+1, ColIndex+1]));
              end
              else if AValue < 1 then
              begin
                frmErrorsAndWarnings.AddWarning(Model, WarningRoot,
                  Format(StrLayerRowColumn,
                  [LayerIndex+1, RowIndex+1, ColIndex+1]));
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModpathSimFileWriter.SaveFile(NameOfFile: string;
  IsArchive: Boolean);
begin
  OpenFile(NameOfFile);
  try
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet4(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet5(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet6(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet7(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteReferenceTime;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteStopTime;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet22;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet23;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet24;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet25;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets26and27;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteTraceFileName(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteTraceID;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteStopZone;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteZones;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteRetardation;

  finally
    CloseFile
  end;
end;

procedure TModpathSimFileWriter.SaveFileMp7(NameOfFile: string;
  IsArchive: Boolean);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  OpenFile(NameOfFile);
  try
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet4(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet5(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet6(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteTraceFileName(IsArchive);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteTraceID;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // detailed budgets not currently supported.
    WriteCheckBudget;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Items 11 and 12.
    WriteReferenceTimeOption;
    WriteReferenceTime;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Item 14
    WriteStopTimeOption;

    // Item 15
    WriteStopTime;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Item 16
    WriteTimePointOptionMp7;

    // Item 17
    WriteTimeCountAndIntervalMp7;

    // Item 18
    WriteTimeCountMp7;

    // Item 19
    WriteTimesMp7;

    // Item 20
    WriteZoneDataOptionMp7;

    // Item 21;
    WriteStopZone;

    // Item 22.
    WriteZones;

    // Item 23
    WriteRetardationFactorOptionMp7;

    // Item 24
    WriteRetardation;

    // Item 25
    WriteParticleGroupCount;

    if Model.DisvUsed then
    begin
      FModflowGrid := nil;
    end
    else
    begin
      FModflowGrid := Model.ModflowGrid;
    end;

    FParticleID := 0;
    for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
    begin
      AScreenObject := FScreenObjects[ScreenObjectIndex];

      // Item 26
      WriteParticleGroupName(AScreenObject);

      // Items 27-31
      WriteReleaseTimes(AScreenObject);

      // Item 32
      WriteStartingLocationFileOption;

      WriteStartingLocations(AScreenObject);
    end;

  finally
    CloseFile
  end;
end;

procedure TModpathSimFileWriter.SpecifyCellNumberArray;
var
  ID: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  TestArray: TDataArray;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    TestArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);

  end
  else
  begin
    TestArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  end;
  SetLength(FCellNumbers, TestArray.LayerCount, TestArray.RowCount, TestArray.ColumnCount);
  ID := 0;
  for LayerIndex := 0 to TestArray.LayerCount -1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      for RowIndex := 0 to TestArray.RowCount -1 do
      begin
        For ColIndex := 0 to TestArray.ColumnCount -1 do
        begin
            Inc(ID);
            FCellNumbers[LayerIndex, RowIndex, ColIndex] := ID;
        end;
      end;
    end
    else
    begin
      for RowIndex := 0 to TestArray.RowCount -1 do
      begin
        For ColIndex := 0 to TestArray.ColumnCount -1 do
        begin
          FCellNumbers[LayerIndex, RowIndex, ColIndex] := 0;
        end;
      end;
    end;
  end;

end;

procedure TModpathSimFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  ADirectory: string;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheMODPATHStopOpti);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSimulationT);
  FNameFile := AFileName;
  NameOfFile := FileName(AFileName);

  FModelName := ExtractFileName(AFileName);
  FModelName := ChangeFileExt(FModelName , '');

  if FMpathVersion = mp6 then
  begin
    SaveFile(NameOfFile, False);
    SaveFile(NameOfFile + ArchiveExt, True);
  end
  else
  begin
    FillScreenObjectList;
    SpecifyCellNumberArray;
    SaveFileMp7(NameOfFile, False);
    SaveFileMp7(NameOfFile + ArchiveExt, True);
  end;

  Model.AddModpathInputFile(NameOfFile + ArchiveExt);
  ADirectory := IncludeTrailingPathDelimiter(ExtractFileDir(NameOfFile));
  if FMpathVersion = mp6 then
  begin
    Model.AddModpathOutputFile(ADirectory + 'MPATH6.LOG');
  end
  else if FMpathVersion = mp7 then
  begin
    Model.AddModpathOutputFile(ADirectory + 'mpath7.log');
  end;

//  Model.AddModelInputFile(NameOfFile);
end;

procedure TModpathSimFileWriter.GetCellCount(AScreenObject: TScreenObject;
  var CellCount: Integer);
var
  IDomainDataArray: TDataArray;
  ACell: TCellAssignment;
  CellIndex: Integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    IDomainDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    IDomainDataArray.Initialize
  end
  else
  begin
    IDomainDataArray := nil;
  end;
  FCellList.Clear;
  AScreenObject.GetModpathCellList(FCellList, Model);
  CellCount := 0;
  for CellIndex := 0 to FCellList.Count - 1 do
  begin
    ACell := FCellList[CellIndex];
    if Model.IsLayerSimulated(ACell.Layer) then
    begin
      if (IDomainDataArray = nil)
        or (IDomainDataArray.IntegerData[
        ACell.Layer, ACell.Row, ACell.Column] > 0) then
      begin
        Inc(CellCount);
      end;
    end;
  end;
end;

procedure TModpathSimFileWriter.GetParticleCount(AScreenObject: TScreenObject;
  var ParticleCount: Integer);
var
  Particles: TParticles;
  CellCount: Integer;
begin
  GetCellCount(AScreenObject, CellCount);
  Particles := AScreenObject.ModpathParticles.Particles;
  if Particles <> nil then
  begin
    ParticleCount := CellCount * Particles.Count;
  end
  else
  begin
    ParticleCount := CellCount;
    if (AScreenObject.ElevationCount <> TElevationCount.ecOne)
      or (AScreenObject.Count <> AScreenObject.SectionCount) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrInTheFollowingObj,
        AScreenObject.Name, AScreenObject);
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteParticleGroupCount;
var
  ParticleGroupCount: Integer;
begin
  ParticleGroupCount := FScreenObjects.Count;
  WriteInteger(ParticleGroupCount);
  WriteString(' # Item 26: ParticleGroupCount');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteParticleGroupName(AScreenObject: TScreenObject);
var
  ParticleGroupName: string;
begin
  ParticleGroupName := Copy(AScreenObject.Name, 1, 16);
  WriteString(ParticleGroupName);
//  WriteString(KDefaultParticleGroupName);
  NewLine;
end;

end.
