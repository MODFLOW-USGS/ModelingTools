unit ModflowTimeSeriesWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowTimeSeriesUnit, PhastModelUnit, System.Math;

type
  TTimeSeriesWriter = class(TCustomModflowWriter)
  private
    FTimeSeries: TTimeSeries;
    FSeriesStart: Integer;
    FMaxExportesSeries: Integer;
    FShouldWriteTemplate: boolean;
    FWritingTemplate: Boolean;
    FParameterDelimiter: AnsiChar;
    procedure WriteAttributes;
    procedure WriteTimeSeries;
  protected
    class function Extension: string; override;
  public
    property ShouldWriteTemplate: boolean read FShouldWriteTemplate
      write FShouldWriteTemplate;
    property ParameterDelimiter: AnsiChar read FParameterDelimiter write FParameterDelimiter;
    procedure WriteFile(FileName: string);
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType;
	    TimeSeries: TTimeSeries; SeriesStart: Integer); reintroduce;
  end;

Const
  // @name is the maximum number of series values printed per line.
  // The purpose of limiting the number of values per line is to keep the line
  // length less than 2000 characters which is the maximum length allowed by
  // PEST in pest templates.
  // Each value takes up 22 characters on the line. Each line also contains
  // a time value.
  MaxSeries = 80;
  // Unfortunately, MODFLOW seems to run into problems when the number of
  // time series files is too high. Multiply the desired number by 8000 to
  // temporarily get around this limitation. Unfortunately, this doesn't work
  // either.
//  MaxSeries = 80*8000;

implementation

uses
  GoPhastTypes;

{ TTimeSeriesWriter }

constructor TTimeSeriesWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType; TimeSeries: TTimeSeries; SeriesStart: Integer);
begin
  inherited Create(AModel, EvaluationType);
  FTimeSeries := TimeSeries;
  FSeriesStart := SeriesStart;
  FMaxExportesSeries := Min(SeriesStart + MaxSeries, TimeSeries.SeriesCount);
  FShouldWriteTemplate := False;
  FParameterDelimiter := ' ';
end;

class function TTimeSeriesWriter.Extension: string;
begin
  result := '';
  Assert(False);
end;

procedure TTimeSeriesWriter.WriteAttributes;
var
  SeriesIndex: Integer;
  ParamID: String;
begin
  if FWritingTemplate then
  begin
    WriteString('ptf ');
    WriteString(ParameterDelimiter);
    NewLine;
  end;

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

  for SeriesIndex := FSeriesStart to FMaxExportesSeries - 1 do
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
    for SeriesIndex := FSeriesStart to FMaxExportesSeries - 1 do
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

  if not FWritingTemplate then
  begin
    if FTimeSeries.UniformScaleFactor then
    begin
      WriteString('  SFAC');
      WriteFloat(FTimeSeries.ScaleFactors[0]);
    end
    else
    begin
      Assert(False);
      WriteString('  SFACS');
      for SeriesIndex := FSeriesStart to FMaxExportesSeries - 1 do
      begin
        WriteFloat(FTimeSeries.ScaleFactors[SeriesIndex]);
      end;
    end;
  end
  else
  begin
    Assert( FTimeSeries.UniformScaleFactor);
    WriteString('  SFAC');
    ParamID := ' ' + string(ParameterDelimiter) + FTimeSeries.ParameterName;
    while Length(ParamID) < 23 do
    begin
      ParamID := ParamID + ' ';
    end;
    ParamID := ParamID + string(ParameterDelimiter);
    WriteString(ParamID)
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
  if ShouldWriteTemplate then
  begin
    FileName := FileName + '.tpl';
    FWritingTemplate := True;

    OpenFile(FileName);
    try
      WriteAttributes;
      WriteTimeSeries;
    finally
      CloseFile;
    end;
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
    for SeriesIndex := FSeriesStart to FMaxExportesSeries - 1 do
    begin
      WriteFloat(FTimeSeries.Values[SeriesIndex, TimeIndex]);
    end;
    NewLine;
  end;
  WriteString('END TIMESERIES');
  NewLine;
end;

end.
