unit ReadMnwiOutput;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ObExtractorTypes;

type
  TMnwiObsType = (motQin, motQout, motQnet, motQCumu, motHwell);

  TMnwiOutRecord = record
    WellID: string;
    TOTIM: double;
    Qin: double;
    Qout: double;
    Qnet: double;
    QCumu: double;
    hwell: double;
    Additional: TDoubleArray;
  end;

  TMnwiObsValue = class(TCustomWeightedObsValue)
    ObsType: TMnwiObsType;
  end;

  { TMnwiObsExtractor }

  TMnwiObsExtractor = class(TCustomObsExtractor)
  private
    function GetItem(Index: integer): TMnwiObsValue;
    function Value(MnwiRecord: TMnwiOutRecord; Index: Integer): double;
    property Items[Index: integer]: TMnwiObsValue read GetItem; default;
  public
    procedure ExtractSimulatedValues; override;
  end;

implementation

uses RealListUnit;

function ConvertLine(AMnwiOutLine: string): TMnwiOutRecord;
const
  MinNumValues = 6;
var
  Splitter: TStringList;
  AddValuesCount: Integer;
  AddValueIndex: Integer;
begin
  result.WellID := Trim(Copy(AMnwiOutLine, 1, 20));
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    Splitter.DelimitedText := Trim(Copy(AMnwiOutLine, 22, MAXINT));
    if Pos('(Well is inactive)', AMnwiOutLine) >= 1 then
    begin
      SetLength(result.Additional, 0);
      result.TOTIM := FortranStrToFloat(Splitter[0]);
      result.Qin := MissingValue;
      result.Qout := MissingValue;
      result.Qnet := MissingValue;
      result.QCumu := MissingValue;
      result.hwell := MissingValue;
    end
    else
    begin
      Assert(Splitter.Count >= MinNumValues);
      AddValuesCount := Splitter.Count-MinNumValues;
      SetLength(result.Additional, AddValuesCount);
      result.TOTIM := FortranStrToFloat(Splitter[0]);
      result.Qin := FortranStrToFloat(Splitter[1]);
      result.Qout := FortranStrToFloat(Splitter[2]);
      result.Qnet := FortranStrToFloat(Splitter[3]);
      result.QCumu := FortranStrToFloat(Splitter[4]);
      result.hwell := FortranStrToFloat(Splitter[5]);
      for AddValueIndex := 0 to Pred(AddValuesCount) do
      begin
        result.Additional[AddValueIndex] :=
          FortranStrToFloat(Splitter[MinNumValues + AddValueIndex]);
      end;
    end;

  finally
    Splitter.Free;
  end;
end;

{ TMnwiObsExtractor }

function TMnwiObsExtractor.Value(MnwiRecord: TMnwiOutRecord; Index: Integer
  ): double;
var
  AddIndex: Integer;
begin
  Assert(Index >= 0);
  AddIndex := Index - Ord(motHwell) -1;
  result := MissingValue;
  if AddIndex >= 0 then
  begin
    if AddIndex < Length(MnwiRecord.Additional) then
    begin
      result := MnwiRecord.Additional[AddIndex];
    end
    else
    begin
      result := MissingValue;
    end;
  end
  else
  begin
    case TMnwiObsType(Index) of
      motQin:
        begin
          result := MnwiRecord.Qin;
        end;
      motQout:
        begin
          result := MnwiRecord.Qout;
        end;
      motQnet:
        begin
          result := MnwiRecord.Qnet;
        end;
      motQCumu:
        begin
          result := MnwiRecord.QCumu;
        end;
      motHwell:
        begin
          result := MnwiRecord.hwell;
        end;
      else
        Assert(False);
    end;
  end;
end;

function TMnwiObsExtractor.GetItem(Index: integer): TMnwiObsValue;
begin
  result := FObsValueList[Index] as TMnwiObsValue;
end;

procedure TMnwiObsExtractor.ExtractSimulatedValues;
const
  Epsilon = 1e-6;
var
  ObsLines: TStringList;
  ObsRecords: array of TMnwiOutRecord;
  LineIndex: Integer;
  ObsIndex: Integer;
  Times: TRealList;
  Obs: TMnwiObsValue;
  RecordIndex: integer;
  ObsRecord: TMnwiOutRecord;
  FirstRecord: TMnwiOutRecord;
  SecondRecord: TMnwiOutRecord;
  procedure InterpolateValues;
  var
    FirstValue: double;
    SecondValue: double;
  begin
    FirstValue := Value(FirstRecord, Ord(Obs.ObsType));
    SecondValue := Value(SecondRecord, Ord(Obs.ObsType));
    if (FirstValue = MissingValue) or (SecondValue = MissingValue) then
    begin
      Obs.SimulatedValue := MissingValue;
    end
    else
    begin
      Obs.SimulatedValue := FirstValue
        + (SecondValue - FirstValue)
        / (SecondRecord.TOTIM - FirstRecord.TOTIM)
        * (Obs.ObsTime - FirstRecord.TOTIM);
    end;
  end;
begin
  Assert(ModelOutputFileName <> '');
  Assert(ObsCount > 0);
  Times := TRealList.Create;
  ObsLines := TStringList.Create;
  try
    ObsLines.LoadFromFile(ModelOutputFileName);
    Times.Capacity := ObsLines.Count-1;
    SetLength(ObsRecords, ObsLines.Count-1);
    for LineIndex := 1 to Pred(ObsLines.Count) do
    begin
      ObsRecords[LineIndex-1] := ConvertLine(ObsLines[LineIndex]);
      Times.Add(ObsRecords[LineIndex-1].TOTIM);
    end;
    Times.Sorted := True;
    for ObsIndex := 0 to Pred(ObsCount) do
    begin
      Obs := Items[ObsIndex];
      RecordIndex := Times.IndexOfClosest(Obs.ObsTime);
      Assert(RecordIndex >= 0);
      ObsRecord := ObsRecords[RecordIndex];
      if Abs(Obs.ObsTime - ObsRecord.TOTIM) <= Epsilon then
      begin
        Obs.SimulatedValue := Value(ObsRecord, Ord(Obs.ObsType));
      end
      else
      begin
        if (Obs.ObsTime > ObsRecord.TOTIM)
          and (RecordIndex+1 < Length(ObsRecords)) then
        begin
          FirstRecord := ObsRecord;
          SecondRecord := ObsRecords[RecordIndex+1];
          InterpolateValues;
        end
        else if (Obs.ObsTime < ObsRecord.TOTIM)
          and (RecordIndex >= 1) then
        begin
          FirstRecord := ObsRecords[RecordIndex-1];
          SecondRecord := ObsRecord;
          InterpolateValues;
        end
        else
        begin
          if (Obs.ObsTime > ObsRecord.TOTIM) then
          begin
            Obs.SimulatedValue := MissingValue;
          end
          else
          begin
            Obs.SimulatedValue := Value(ObsRecord, Ord(Obs.ObsType));
          end;
        end;
      end;
    end;
  finally
    ObsLines.Free;
    Times.Free;
  end;
end;

end.

