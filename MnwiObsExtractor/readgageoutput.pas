unit readgageoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObExtractorTypes, RealListUnit;

type
  TGageOutRecord = record
    Time: double;
    Values: TDoubleArray;
  end;

  TGageOutArray = array of TGageOutRecord;

  TGageObsValue = class(TCustomWeightedObsValue)
    ObsType: string;
  end;

  { TCustomGageObsExtractor }

  TCustomGageObsExtractor = class(TCustomObsExtractor)
  private
    FGageOutputTypes: TStringList;
    FTimes: TRealList;
    FGagePositions : array of Integer;
    FObsRecords: TGageOutArray;
    function GetItem(Index: integer): TGageObsValue;
    function Value(LakeGageRecord: TGageOutRecord; Index: Integer): double;
    property Items[Index: integer]: TGageObsValue read GetItem; default;
    function ObsTypeToIndex(const ObsType: string): integer;
    procedure ReadGageFile;
  protected
    procedure AssignDerivedValues; virtual;
  public
    procedure ExtractSimulatedValues; override;
    Constructor Create;
    destructor Destroy; override;
  end;

  { TLakeGageObsExtractor }

  TLakeGageObsExtractor = class(TCustomGageObsExtractor)
  public
    Constructor Create;
  end;

  { TSfrGageObsExtractor }

  TSfrGageObsExtractor = class(TCustomGageObsExtractor)
  protected
    procedure AssignDerivedValues; override;
  public
    Constructor Create;
  end;

var
  LakeGageOutputTypes: TStringList;
  LakeGageUnits: TStringList;
  StreamGageOutputTypes: TStringList;

implementation

procedure TCustomGageObsExtractor.ReadGageFile;
var
  ValueIndex: Integer;
  ItemPosition: Integer;
  ObsTypeIndex: Integer;
  Splitter: TStringList;
  HeaderLine: string;
  LineIndex: Integer;
  ObsLines: TStringList;
begin
  ObsLines := TStringList.Create;
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    ObsLines.LoadFromFile(ModelOutputFileName);
    FTimes.Capacity := ObsLines.Count-2;
    SetLength(FObsRecords, ObsLines.Count-2);
    HeaderLine := RemoveQuotes(Trim(ObsLines[1]));
    Splitter.DelimitedText := HeaderLine;
    SetLength(FGagePositions, FGageOutputTypes.Count);
    for ObsTypeIndex := 0 to Pred(FGageOutputTypes.Count) do
    begin
      ItemPosition := Splitter.IndexOf(FGageOutputTypes[ObsTypeIndex]);
      if ItemPosition >= 0 then
      begin
        FGagePositions[ObsTypeIndex] := ItemPosition-1;
      end
      else
      begin
        FGagePositions[ObsTypeIndex] := -1;
      end;
    end;

    for LineIndex := 2 to Pred(ObsLines.Count) do
    begin
      Splitter.DelimitedText := ObsLines[LineIndex];
      SetLength(FObsRecords[LineIndex-2].Values, Splitter.Count-1);
      FObsRecords[LineIndex-2].Time := StrToFloat(Splitter[0]);
      for ValueIndex := 1 to Pred(Splitter.Count) do
      begin
        FObsRecords[LineIndex-2].Values[ValueIndex-1] :=
          StrToFloat(Splitter[ValueIndex]);
      end;
      FTimes.Add(FObsRecords[LineIndex-2].Time);
    end;
  finally
    Splitter.Free;
    ObsLines.Free;
  end;
end;

procedure TCustomGageObsExtractor.AssignDerivedValues;
begin
  // do nothing.
end;

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

procedure TSfrGageObsExtractor.AssignDerivedValues;
var
  ConductanceIndex: Integer;
  HeadDiffIndex: Integer;
  ObIndex: Integer;
  Gw_FlowIndex: Integer;
  Conductance: double;
  HeadDiff: double;
begin
  //inherited AssignDerivedValues;
  Gw_FlowIndex := FGageOutputTypes.Add('GW_FLOW')+1;
  SetLength(FGagePositions,FGageOutputTypes.Count);
  FGagePositions[FGageOutputTypes.Count-1] := Gw_FlowIndex;
  ConductanceIndex := ObsTypeToIndex('Conductance');
  HeadDiffIndex := ObsTypeToIndex('HeadDiff');
  if (ConductanceIndex >= 0) and (HeadDiffIndex >= 0) then
  begin
    for ObIndex := 0 to Pred(Length(FObsRecords)) do
    begin
      SetLength(FObsRecords[ObIndex].Values, FGageOutputTypes.Count);
      Conductance := FObsRecords[ObIndex].Values[ConductanceIndex];
      HeadDiff := FObsRecords[ObIndex].Values[HeadDiffIndex];
      FObsRecords[ObIndex].Values[Gw_FlowIndex-1] := Conductance * HeadDiff;
    end;
  end;
end;

constructor TSfrGageObsExtractor.Create;
begin
  inherited;
  FGageOutputTypes.Assign(StreamGageOutputTypes);
end;

{ TLakeGageObsExtractor }

constructor TLakeGageObsExtractor.Create;
begin
  inherited;
  FGageOutputTypes.Assign(LakeGageOutputTypes);
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

function TCustomGageObsExtractor.ObsTypeToIndex(const ObsType: string): integer;
begin
  result := FGageOutputTypes.IndexOf(ObsType);
  if result >= 0 then
  begin
    result := FGagePositions[result]-1;
  end;
end;

procedure TCustomGageObsExtractor.ExtractSimulatedValues;
const
  Epsilon = 1e-6;
var
  ObsIndex: Integer;
  Obs: TGageObsValue;
  RecordIndex: integer;
  ObsRecord: TGageOutRecord;
  FirstRecord: TGageOutRecord;
  SecondRecord: TGageOutRecord;
  //ObsTypeIndex: Integer;
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
  Assert(ModelOutputFileName <> '');
  Assert(ObsCount > 0);
  FTimes := TRealList.Create;
  try
    ReadGageFile;

    AssignDerivedValues;

    FTimes.Sorted := True;
    for ObsIndex := 0 to Pred(ObsCount) do
    begin
      Obs := Items[ObsIndex];
      RecordIndex := FTimes.IndexOfClosest(Obs.ObsTime);
      Assert(RecordIndex >= 0);
      ObsRecord := FObsRecords[RecordIndex];

      if Abs(Obs.ObsTime - ObsRecord.Time) <= Epsilon then
      begin
        //ObsTypeIndex := ObsTypeToIndex(Obs.ObsType);
        Obs.SimulatedValue := Value(ObsRecord, ObsTypeToIndex(Obs.ObsType));
      end
      else
      begin
        if (Obs.ObsTime > ObsRecord.Time)
          and (RecordIndex+1 < Length(FObsRecords)) then
        begin
          FirstRecord := ObsRecord;
          SecondRecord := FObsRecords[RecordIndex+1];
          InterpolateValues;
        end
        else if (Obs.ObsTime < ObsRecord.Time)
          and (RecordIndex >= 1) then
        begin
          FirstRecord := FObsRecords[RecordIndex-1];
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
    FTimes.Free;
  end;

end;

constructor TCustomGageObsExtractor.Create;
begin
  inherited;
  FGageOutputTypes := TStringList.Create;
end;

destructor TCustomGageObsExtractor.Destroy;
begin
  FGageOutputTypes.Free;
  inherited Destroy;
end;

Initialization
  InitializeGageOutputTypes;

Finalization
  LakeGageOutputTypes.Free;
  LakeGageUnits.Free;
  StreamGageOutputTypes.Free;

end.

