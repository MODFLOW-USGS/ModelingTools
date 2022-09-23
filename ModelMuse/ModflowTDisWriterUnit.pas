unit ModflowTDisWriterUnit;

interface

uses System.Classes, SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TTemporalDiscretizationWriter = class(TCustomModflowWriter)
  private
    FAtsCount: Integer;
    FAtsFileName: string;
    FSpeciesIndex: Integer;
    procedure WriteDataSet0;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteStressPeriods;
    procedure WriteAtsFile;
    procedure WriteFileInternal;
    procedure WriteGwtFilesInternal(const AFileName: string);
  public
    class function Extension: string; override; // abstract;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, GoPhastTypes, ModflowTimeUnit, Vcl.Forms,
  ModflowPackageSelectionUnit;

resourcestring
  StrWritingTemporalDis = 'Writing Temporal Discretization (TDIS) Package in' +
  'put.';

{ TTemporalDiscretizationWriter }

class function TTemporalDiscretizationWriter.Extension: string;
begin
  Result := '.tdis';
end;

procedure TTemporalDiscretizationWriter.WriteAtsFile;
var
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  AStressPeriod: TModflowStressPeriod;
begin
  FInputFileName := FAtsFileName;
  Model.AddModelInputFile(FAtsFileName);
  OpenFile(FInputFileName);
  try
    WriteCommentLine(File_Comment('ATS'));
    NewLine;

    WriteBeginDimensions;
    WriteString('  MAXATS');
    WriteInteger(FAtsCount);
    NewLine;
    WriteEndDimensions;


    WriteString('BEGIN PERIODDATA');
    NewLine;
    StressPeriods := Model.ModflowFullStressPeriods;
    for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
    begin
      AStressPeriod := StressPeriods[StressPeriodIndex];
      if AStressPeriod.AtsUsed then
      begin
        WriteInteger(StressPeriodIndex+1);
        WriteFloat(AStressPeriod.AtsInitialStepSize);
        WriteFloat(AStressPeriod.AtsMinimumStepSize);
        WriteFloat(AStressPeriod.AtsMaximumStepSize);
        WriteFloat(AStressPeriod.AtsAdjustmentFactor);
        WriteFloat(AStressPeriod.AtsFailureFactor);
        NewLine;
      end;
    end;
    WriteString('END PERIODDATA');
    NewLine


  finally
    CloseFile
  end;
end;

procedure TTemporalDiscretizationWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('TDIS'));
  NewLine;
end;

procedure TTemporalDiscretizationWriter.WriteDimensions;
var
  NPER: integer;
begin
  NPER := Model.ModflowFullStressPeriods.Count;
  WriteString('BEGIN DIMENSIONS');
  NewLine;

  WriteString('  NPER');
  WriteInteger(NPER);
  NewLine;

  WriteString('END DIMENSIONS');
  NewLine;
  NewLine;
end;

procedure TTemporalDiscretizationWriter.WriteFile(const AFileName: string);
var
  FTYPE: string;
  NameOfFile: string;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FTYPE := 'TDIS6';

  if Model.PackageGeneratedExternally(FTYPE) then
  begin
    Exit;
  end;
  FSpeciesIndex := -1;
  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  FNameOfFile := NameOfFile;

  Model.SimNameWriter.TDisFileName := FNameOfFile;
  WriteFileInternal;

  if FAtsCount > 0 then
  begin
    WriteAtsFile;
  end;
  WriteGwtFilesInternal(AFileName);
end;

procedure TTemporalDiscretizationWriter.WriteGwtFilesInternal(const AFileName: string);
var
  GwtSimulationChoice: TGwtSimulationChoice;
  Limit: Integer;
  SpeciesIndex: Integer;
  ASpecies: string;
begin
  GwtSimulationChoice := Model.ModflowPackages.GwtProcess.GwtSimulationChoice;
  if Model.GwtUsed and (GwtSimulationChoice in [gscTransportTogether, gscEachSpeciesSeparate]) then
  begin
    if GwtSimulationChoice = gscTransportTogether then
    begin
      Limit := 1;
    end
    else
    begin
      Limit := Model.MobileComponents.Count;
    end;
    for SpeciesIndex := 0 to Limit - 1 do
    begin
      FSpeciesIndex := SpeciesIndex;
      ASpecies := '.' + Model.MobileComponents[SpeciesIndex].Name;
      FNameOfFile := ChangeFileExt(AFileName, ASpecies) + Extension;
      Model.SimNameWriter.GwtTDisFileNames[SpeciesIndex] := FNameOfFile;
      WriteFileInternal;
    end;
  end;
end;

procedure TTemporalDiscretizationWriter.WriteFileInternal;
begin
  Model.AddModelInputFile(FNameOfFile);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingTemporalDis);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    Application.ProcessMessages;
    WriteDataSet0;
    WriteOptions;
    WriteDimensions;
    WriteStressPeriods;
  finally
    CloseFile;
  end;
end;

procedure TTemporalDiscretizationWriter.WriteOptions;
var
  TimeUnit: Integer;
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  AStressPeriod: TModflowStressPeriod;
begin
  TimeUnit := Model.ModflowOptions.TimeUnit;
  WriteBeginOptions;

  WriteString('  TIME_UNITS ');
  case TimeUnit of
    0: WriteString('unknown');
    1: WriteString('seconds');
    2: WriteString('minutes');
    3: WriteString('hours');
    4: WriteString('days');
    5: WriteString('years');
  end;
  NewLine;

  FAtsCount := 0;
  StressPeriods := Model.ModflowFullStressPeriods;
  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    AStressPeriod := StressPeriods[StressPeriodIndex];
    if AStressPeriod.AtsUsed then
    begin
      Inc(FAtsCount);
    end;
  end;
  if FAtsCount > 0 then
  begin
    FAtsFileName := ChangeFileExt(FInputFileName, '.ats');
    WriteString('  ATS6 FILEIN ');
    WriteString(ExtractFileName(FAtsFileName));
    NewLine;
  end;

  WriteEndOptions;
end;

procedure TTemporalDiscretizationWriter.WriteStressPeriods;
var
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  AStressPeriod: TModflowStressPeriod;
begin
  StressPeriods := Model.ModflowFullStressPeriods;

  WriteString('BEGIN PERIODDATA');
  NewLine;

  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    AStressPeriod := StressPeriods[StressPeriodIndex];
    WriteString('  ');
    WriteFloat(AStressPeriod.PeriodLength);
    if FSpeciesIndex <0 then
    begin
      WriteInteger(AStressPeriod.NumberOfSteps);
      WriteFloat(AStressPeriod.TimeStepMultiplier);
    end
    else
    begin
      WriteInteger(AStressPeriod.GwtNumSteps[FSpeciesIndex]);
      WriteFloat(AStressPeriod.GwtMultiplier[FSpeciesIndex]);
    end;
    WriteString(' # perlen nstp tsmult, Stress period ');
    WriteInteger(StressPeriodIndex+1);
    NewLine;
  end;

  WriteString('END PERIODDATA');
  NewLine;
end;

end.
