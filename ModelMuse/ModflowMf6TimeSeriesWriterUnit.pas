unit ModflowMf6TimeSeriesWriterUnit;

interface

uses
  CustomModflowWriterUnit, Modflow6TimeSeriesCollectionsUnit;

type
  TMf6TimeSeriesWriter = class(TCustomModflowWriter)
  private
    FTimeSeries: TTimesSeriesCollection;
    procedure WriteAttributes;
    procedure WriteTimeSeries;
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
  public
    // result is time series file name.
    function WriteFile(PackageInputFileName: string;
      TimeSeries: TTimesSeriesCollection): string;
  end;


implementation

uses
  frameModflow6TimeSeriesUnit, GoPhastTypes, ModflowParameterUnit,
  Modflow6TimeSeriesUnit;

{ TMf6TimeSeriesWriter }

class function TMf6TimeSeriesWriter.Extension: string;
begin
  result := '.ts';
end;

procedure TMf6TimeSeriesWriter.WriteAttributes;
var
  SeriesIndex: Integer;
  TimeSeries: TMf6TimeSeries;
  Param: TModflowSteadyParameter;
  ScaleFactor: Double;
begin
  WriteString('BEGIN ATTRIBUTES');
  NewLine;

  if FTimeSeries.Count > 1 then
  begin
    WriteString('  NAMES');
  end
  else
  begin
    WriteString('  NAME');
  end;

  for SeriesIndex := FTimeSeries.Count - 1 downto 0 do
  begin
    WriteString(' ' + FTimeSeries[SeriesIndex].TimeSeries.SeriesName);
  end;
  NewLine;

  if FTimeSeries.Count > 1 then
  begin
    WriteString('  METHODS');
  end
  else
  begin
    WriteString('  METHOD');
  end;

  for SeriesIndex := FTimeSeries.Count - 1 downto 0 do
  begin
    TimeSeries := FTimeSeries[SeriesIndex].TimeSeries;
    case TimeSeries.InterpolationMethod of
      mimStepwise:
        begin
          WriteString(' STEPWISE');
        end;
      mimLinear:
        begin
          WriteString(' LINEAR');
        end;
      mimLinearEnd:
        begin
          WriteString(' LINEAREND');
        end;
      else
        Assert(False);
    end;
  end;
  NewLine;

  if FTimeSeries.Count > 1 then
  begin
    WriteString('  SFACS');
  end
  else
  begin
    WriteString('  SFAC');
  end;

  for SeriesIndex := FTimeSeries.Count - 1 downto 0 do
  begin
    TimeSeries := FTimeSeries[SeriesIndex].TimeSeries;
    ScaleFactor := TimeSeries.ScaleFactor;
    Param := Model.GetPestParameterByName(TimeSeries.ScaleFactorParameter);
    if Param <> nil then
    begin
      case TimeSeries.ParamMethod of
        ppmMultiply:
          begin
            ScaleFactor := ScaleFactor * Param.Value;
          end;
        ppmAdd:
          begin
            ScaleFactor := ScaleFactor + Param.Value;
          end;
        else
          Assert(False);
      end;
      FPestParamUsed := True;
      Model.WritePValAndTemplate(Param.ParameterName, Param.Value, Param, True);
    end;

    WritePestTemplateFormulaOrValue(ScaleFactor, '',
      TimeSeries.ScaleFactorParameter, TimeSeries.ParamMethod,
      nil, nil);
  end;
  NewLine;

  WriteString('END ATTRIBUTES');
  NewLine;
  NewLine;
end;

function TMf6TimeSeriesWriter.WriteFile(PackageInputFileName: string;
  TimeSeries: TTimesSeriesCollection): string;
begin
  FTimeSeries := TimeSeries;
  FNameOfFile := PackageInputFileName + '.'
    + string(TimeSeries.GroupName) + Extension;
  result := FNameOfFile;
  FInputFileName := FNameOfFile;
  FPestParamUsed := False;
  WriteFileInternal;
  if Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;
end;

procedure TMf6TimeSeriesWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;
    WriteAttributes;
    WriteTimeSeries;
  finally
    CloseFile;
  end;
end;

procedure TMf6TimeSeriesWriter.WriteTimeSeries;
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  TimeSeries: TMf6TimeSeries;
  StartTime: Double;
begin
  WriteString('BEGIN TIMESERIES');
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  NewLine;
  for TimeIndex := 0 to FTimeSeries.TimeCount - 1 do
  begin
    WriteFloat(FTimeSeries.Times[TimeIndex].Value - StartTime);
    for SeriesIndex := FTimeSeries.Count - 1 downto 0 do
    begin
      TimeSeries := FTimeSeries[SeriesIndex].TimeSeries;
      WriteFloat(TimeSeries[TimeIndex].Value);
    end;
    NewLine;
  end;
  WriteString('END TIMESERIES');
  NewLine;
end;

end.
