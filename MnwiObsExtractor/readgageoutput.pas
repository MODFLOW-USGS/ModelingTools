unit readgageoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObExtractorTypes;

type
  TGageOutRecord = record
    Time: double;
    Values: TDoubleArray;
  end;

  TGageObsValue = class(TCustomObsValue)
    ObsType: string;
  end;

  { TCustomGageObsExtractor }

  TCustomGageObsExtractor = class(TCustomObsExtractor)
  private
    FGageOutputTypes: TStringList;
    function GetItem(Index: integer): TGageObsValue;
    function Value(LakeGageRecord: TGageOutRecord; Index: Integer): double;
    property Items[Index: integer]: TGageObsValue read GetItem; default;
  public
    procedure ExtractSimulatedValues; override;
  end;

  { TLakeGageObsExtractor }

  TLakeGageObsExtractor = class(TCustomGageObsExtractor)
    Constructor Create;
  end;

  { TSfrGageObsExtractor }

  TSfrGageObsExtractor = class(TCustomGageObsExtractor)
    Constructor Create;
  end;

var
  LakeGageOutputTypes: TStringList;
  LakeGageUnits: TStringList;
  StreamGageOutputTypes: TStringList;

implementation

uses RealListUnit;

procedure InitializeGageOutputTypes;
begin
  LakeGageOutputTypes := TStringList.Create;
  LakeGageUnits := TStringList.Create;
  LakeGageOutputTypes.Add('Stage(H)');    LakeGageUnits.Add('L');
  LakeGageOutputTypes.Add('Volume');      LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Precip.');     LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Evap.');       LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Runoff');      LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('GW-Inflw');    LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('GW-Outflw');   LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('SW-Inflw');    LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('SW-Outflw');   LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Withdrawal');  LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Lake-Inflx');  LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Total-Cond.'); LakeGageUnits.Add('L2/T');
  LakeGageOutputTypes.Add('Del-H-TS');    LakeGageUnits.Add('L/T');
  LakeGageOutputTypes.Add('Del-V-TS');    LakeGageUnits.Add('L3/T');
  LakeGageOutputTypes.Add('Del-H-Cum');   LakeGageUnits.Add('L');
  LakeGageOutputTypes.Add('Del-V-Cum');   LakeGageUnits.Add('L3');

  StreamGageOutputTypes := TStringList.Create;
  StreamGageOutputTypes.Add('Stage');
  StreamGageOutputTypes.Add('Flow');
  StreamGageOutputTypes.Add('Depth');
  StreamGageOutputTypes.Add('Width');
  StreamGageOutputTypes.Add('Midpt-Flow');
  StreamGageOutputTypes.Add('Precip.');
  StreamGageOutputTypes.Add('ET');
  StreamGageOutputTypes.Add('Runoff');
  StreamGageOutputTypes.Add('Conductance');
  StreamGageOutputTypes.Add('HeadDiff');
  StreamGageOutputTypes.Add('Hyd.Grad.');

end;

{ TSfrGageObsExtractor }

constructor TSfrGageObsExtractor.Create;
begin
  FGageOutputTypes := StreamGageOutputTypes;
end;

{ TLakeGageObsExtractor }

constructor TLakeGageObsExtractor.Create;
begin
  inherited;
  FGageOutputTypes := LakeGageOutputTypes;
end;

{ TCustomGageObsExtractor }

function TCustomGageObsExtractor.GetItem(Index: integer): TGageObsValue;
begin
  result := FObsValueList[Index] as TGageObsValue;
end;

function TCustomGageObsExtractor.Value(LakeGageRecord: TGageOutRecord; Index: Integer
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

procedure TCustomGageObsExtractor.ExtractSimulatedValues;
const
  Epsilon = 1e-6;
var
  ObsLines: TStringList;
  ObsRecords: array of TGageOutRecord;
  LineIndex: Integer;
  ObsIndex: Integer;
  Times: TRealList;
  Obs: TGageObsValue;
  RecordIndex: integer;
  ObsRecord: TGageOutRecord;
  FirstRecord: TGageOutRecord;
  SecondRecord: TGageOutRecord;
  HeaderLine: string;
  Splitter: TStringList;
  ObsTypeIndex: Integer;
  ItemPosition: Integer;
  ValueIndex: Integer;
  LakePositions : array of Integer;
  function ObsTypeToIndex(const ObsType: string): integer;
  begin
    result := FGageOutputTypes.IndexOf(ObsType);
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
    SetLength(LakePositions, FGageOutputTypes.Count);
    for ObsTypeIndex := 0 to Pred(FGageOutputTypes.Count) do
    begin
      ItemPosition := Splitter.IndexOf(FGageOutputTypes[ObsTypeIndex]);
      if ItemPosition >= 0 then
      begin
        LakePositions[ObsTypeIndex] := ItemPosition-1;
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
  InitializeGageOutputTypes;

Finalization
  LakeGageOutputTypes.Free;
  LakeGageUnits.Free;
  StreamGageOutputTypes.Free;

end.

