unit ModflowTimeSeriesWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowTimeSeriesUnit, PhastModelUnit;

type
  TTimeSeriesWriter = class(TCustomModflowWriter)
  private
    FTimeSeries: TTimeSeries;
    procedure WriteAttributes;
    procedure WriteTimeSeries;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(FileName: string);
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType;
	  TimeSeries: TTimeSeries); reintroduce;
  end;


implementation

uses
  GoPhastTypes;

{ TTimeSeriesWriter }

constructor TTimeSeriesWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType; TimeSeries: TTimeSeries);
begin
  inherited Create(AModel, EvaluationType);
  FTimeSeries := TimeSeries;
end;

class function TTimeSeriesWriter.Extension: string;
begin
  result := '';
  Assert(False);
end;

procedure TTimeSeriesWriter.WriteAttributes;
var
  SeriesIndex: Integer;
begin
  WriteString('BEGIN ATTRIBUTES');
  NewLine;

  if FTimeSeries.SeriesCount = 1 then
  begin
    WriteString('  NAME');
  end
  else
  begin
    WriteString('  NAMES');
  end;

  for SeriesIndex := 0 to FTimeSeries.SeriesCount - 1 do
  begin
    WriteString(' ');
    WriteString(FTimeSeries.SeriesNames[SeriesIndex]);
  end;
  NewLine;

  if FTimeSeries.UniformInterp then
  begin
    WriteString('  METHOD');
    case FTimeSeries.InterpolationMethods[0] of
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
  end
  else
  begin
    WriteString('  METHODS');
    for SeriesIndex := 0 to FTimeSeries.SeriesCount -1 do
    begin
      case FTimeSeries.InterpolationMethods[SeriesIndex] of
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
  end;
  NewLine;

  if FTimeSeries.UniformScaleFactor then
  begin
    WriteString('  SFAC');
    WriteFloat(FTimeSeries.ScaleFactors[0]);
  end
  else
  begin
    WriteString('  SFACS');
    for SeriesIndex := 0 to FTimeSeries.SeriesCount -1 do
    begin
      WriteFloat(FTimeSeries.ScaleFactors[SeriesIndex]);
    end;
  end;
  NewLine;

  WriteString('END ATTRIBUTES');
  NewLine;
  NewLine;
end;

procedure TTimeSeriesWriter.WriteFile(FileName: string);
begin
  Model.AddModelInputFile(FileName);
  OpenFile(FileName);
  try
    WriteAttributes;
    WriteTimeSeries;
  finally
    CloseFile;
  end;
end;

procedure TTimeSeriesWriter.WriteTimeSeries;
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
begin
  WriteString('BEGIN TIMESERIES');
  NewLine;
  for TimeIndex := 0 to FTimeSeries.TimeCount - 1 do
  begin
    WriteFloat(FTimeSeries.Times[TimeIndex]);
    for SeriesIndex := 0 to FTimeSeries.SeriesCount - 1 do
    begin
      WriteFloat(FTimeSeries.Values[SeriesIndex, TimeIndex]);
    end;
    NewLine;
  end;
  WriteString('END TIMESERIES');
  NewLine;
end;

end.
