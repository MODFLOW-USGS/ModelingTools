unit readgageoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObExtractorTypes;

type
  TLakeOutRecord = record
    Time: double;
    Values: TDoubleArray;
  end;

  TLakeGageObsValue = class(TCustomObsValue)
    ObsType: string;
  end;

  { TGageObsExtractor }

  TGageObsExtractor = class(TCustomObsExtractor)
  private
    function GetItem(Index: integer): TLakeGageObsValue;
    function Value(LakeGageRecord: TLakeOutRecord; Index: Integer): double;
    property Items[Index: integer]: TLakeGageObsValue read GetItem; default;
  public
    procedure ExtractSimulatedValues; override;
  end;

implementation

uses RealListUnit;

var
  LakeGageOutputTypes: TStringList;




procedure InitializeLakeGageOutputTypes;
begin
  LakeGageOutputTypes := TStringList.Create;
  LakeGageOutputTypes.Add('Stage(H)');
  LakeGageOutputTypes.Add('Volume');
  LakeGageOutputTypes.Add('Precip.');
  LakeGageOutputTypes.Add('Evap.');
  LakeGageOutputTypes.Add('Runoff');
  LakeGageOutputTypes.Add('GW-Inflw');
  LakeGageOutputTypes.Add('GW-Outflw');
  LakeGageOutputTypes.Add('SW-Inflw');
  LakeGageOutputTypes.Add('SW-Outflw');
  LakeGageOutputTypes.Add('Withdrawal');
  LakeGageOutputTypes.Add('Lake-Inflx');
  LakeGageOutputTypes.Add('Total-Cond.');
  LakeGageOutputTypes.Add('Del-H-TS');
  LakeGageOutputTypes.Add('Del-V-TS');
  LakeGageOutputTypes.Add('Del-H-Cum');
  LakeGageOutputTypes.Add('Del-V-Cum');

end;

{ TGageObsExtractor }

function TGageObsExtractor.GetItem(Index: integer): TLakeGageObsValue;
begin
  result := FObsValueList[Index] as TLakeGageObsValue;
end;

function TGageObsExtractor.Value(LakeGageRecord: TLakeOutRecord; Index: Integer
  ): double;
begin
  if Index >= 0 then
  begin
    result := LakeGageRecord.Values[Index];
  end
  else
  begin
    result := MissingValue;
  end;
end;

procedure TGageObsExtractor.ExtractSimulatedValues;
const
  Epsilon = 1e-6;
var
  ObsLines: TStringList;
  ObsRecords: array of TLakeOutRecord;
  LineIndex: Integer;
  ObsIndex: Integer;
  Times: TRealList;
  Obs: TLakeGageObsValue;
  RecordIndex: integer;
  ObsRecord: TLakeOutRecord;
  FirstRecord: TLakeOutRecord;
  SecondRecord: TLakeOutRecord;
  HeaderLine: string;
  Splitter: TStringList;
  ObsTypeIndex: Integer;
  ItemPosition: Integer;
  ValueIndex: Integer;
  LakePositions : array of Integer;
  function ObsTypeToIndex(const ObsType: string): integer;
  begin
    result := LakeGageOutputTypes.IndexOf(ObsType);
    if result >= 0 then
    begin
      result := LakePositions[result];
    end;
  end;
  procedure InterpolateValues;
  var
    FirstValue: double;
    SecondValue: double;
  begin
    FirstValue := Value(FirstRecord, ObsTypeToIndex(Obs.ObsType));
    SecondValue := Value(SecondRecord, ObsTypeToIndex(Obs.ObsType));
    if (FirstValue = MissingValue) or (SecondValue = MissingValue) then
    begin
      Obs.SimulatedValue := MissingValue;
    end
    else
    begin
      Obs.SimulatedValue := FirstValue
        + (SecondValue - FirstValue)
        / (SecondRecord.Time - FirstRecord.Time)
        * (Obs.ObsTime - FirstRecord.Time);
    end;
  end;
begin
  Assert(OutputFileName <> '');
  Assert(ObsCount > 0);
  Times := TRealList.Create;
  ObsLines := TStringList.Create;
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    ObsLines.LoadFromFile(OutputFileName);
    Times.Capacity := ObsLines.Count-2;
    SetLength(ObsRecords, ObsLines.Count-2);
    HeaderLine := RemoveQuotes(Trim(ObsLines[1]));
    Splitter.DelimitedText := HeaderLine;
    SetLength(LakePositions, LakeGageOutputTypes.Count);
    for ObsTypeIndex := 0 to Pred(LakeGageOutputTypes.Count) do
    begin
      ItemPosition := Splitter.IndexOf(LakeGageOutputTypes[ObsTypeIndex]);
      if ItemPosition >= 0 then
      begin
        LakePositions[ObsTypeIndex] := ItemPosition-2;
      end
      else
      begin
        LakePositions[ObsTypeIndex] := -1;
      end;
    end;

    for LineIndex := 2 to Pred(ObsLines.Count) do
    begin
      Splitter.DelimitedText := ObsLines[LineIndex];
      SetLength(ObsRecords[LineIndex-2].Values, Splitter.Count-1);
      ObsRecords[LineIndex-2].Time := StrToFloat(Splitter[0]);
      for ValueIndex := 1 to Pred(Splitter.Count) do
      begin
        ObsRecords[LineIndex-2].Values[ValueIndex-1] :=
          StrToFloat(Splitter[ValueIndex-1]);
      end;
      Times.Add(ObsRecords[LineIndex-2].Time);
    end;

    Times.Sorted := True;
    for ObsIndex := 0 to Pred(ObsCount) do
    begin
      Obs := Items[ObsIndex];
      RecordIndex := Times.IndexOfClosest(Obs.ObsTime);
      Assert(RecordIndex >= 0);
      ObsRecord := ObsRecords[RecordIndex];

      if Abs(Obs.ObsTime - ObsRecord.Time) <= Epsilon then
      begin
        Obs.SimulatedValue := Value(ObsRecord, ObsTypeToIndex(Obs.ObsType));
      end
      else
      begin
        if (Obs.ObsTime > ObsRecord.Time)
          and (RecordIndex+1 < Length(ObsRecords)) then
        begin
          FirstRecord := ObsRecord;
          SecondRecord := ObsRecords[RecordIndex+1];
          InterpolateValues;
        end
        else if (Obs.ObsTime < ObsRecord.Time)
          and (RecordIndex >= 1) then
        begin
          FirstRecord := ObsRecords[RecordIndex-1];
          SecondRecord := ObsRecord;
          InterpolateValues;
        end
        else
        begin
          if (Obs.ObsTime > ObsRecord.Time) then
          begin
            Obs.SimulatedValue := MissingValue;
          end
          else
          begin
            Obs.SimulatedValue := Value(ObsRecord, ObsTypeToIndex(Obs.ObsType));
          end;
        end;
      end;
    end;

  finally
    Splitter.Free;
    ObsLines.Free;
    Times.Free;
  end;

end;

Initialization
  InitializeLakeGageOutputTypes;

Finalization
  LakeGageOutputTypes.Free;

end.

