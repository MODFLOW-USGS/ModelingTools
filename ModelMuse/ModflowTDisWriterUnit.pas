unit ModflowTDisWriterUnit;

interface

uses System.Classes, SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TTemporalDiscretizationWriter = class(TCustomModflowWriter)
  private
    procedure WriteDataSet0;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteStressPeriods;
  public
    class function Extension: string; override; // abstract;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, GoPhastTypes, ModflowTimeUnit;

resourcestring
  StrWritingTemporalDis = 'Writing Temporal Discretization (TDIS) Package in' +
  'put.';

{ TTemporalDiscretizationWriter }

class function TTemporalDiscretizationWriter.Extension: string;
begin
  Result := '.tdis';
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
  Model.SimNameWriter.TDisFileName := NameOfFile;
  Model.AddModelInputFile(NameOfFile);
//  WriteToNameFile(FTYPE, -1, NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingTemporalDis);
    frmProgressMM.AddMessage(StrWritingDataSet0);
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
