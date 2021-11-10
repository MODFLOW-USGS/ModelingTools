unit ModflowTDisWriterUnit;

interface

uses System.Classes, SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TTemporalDiscretizationWriter = class(TCustomModflowWriter)
  private
    FAtsCount: Integer;
    FAtsFileName: string;
    procedure WriteDataSet0;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteStressPeriods;
    procedure WriteAtsFile;
  public
    class function Extension: string; override; // abstract;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, GoPhastTypes, ModflowTimeUnit, Vcl.Forms;

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
  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  Model.SimNameWriter.TDisFileName := NameOfFile;
  Model.AddModelInputFile(NameOfFile);
//  WriteToNameFile(FTYPE, -1, NameOfFile, foInput);
  OpenFile(NameOfFile);
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

  {$IFDEF ATS}
  if FAtsCount > 0 then
  begin
    WriteAtsFile;
  end;
  {$ENDIF}
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

  {$IFDEF ATS}
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
  {$ENDIF}

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
    WriteInteger(AStressPeriod.NumberOfSteps);
    WriteFloat(AStressPeriod.TimeStepMultiplier);
    WriteString(' # perlen nstp tsmult, Stress period ');
    WriteInteger(StressPeriodIndex+1);
    NewLine;
  end;

  WriteString('END PERIODDATA');
  NewLine;
end;

end.
