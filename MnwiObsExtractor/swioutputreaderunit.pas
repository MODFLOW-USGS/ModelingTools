unit SwiOutputReaderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObExtractorTypes, Generics.Collections, SwiObsReaderUnit;

type
  //TFileFormat = (ffAscii, ffBinarySingle, ffBinaryDouble);

  { TSwiObsValue }

  TSwiCell = record
    Number: Integer;
    Fraction: double;
    Name: string;
    Value: double;
  end;

  TSwiCellList = specialize TList<TSwiCell>;

  TSwiObsValue = class(TCustomWeightedObsValue)
  private
    FObsType: string;
    FZetaSurfaceNumber: Integer;
    FSwiCellList: TSwiCellList;
    procedure SetObsType(AValue: string);
    procedure SetZetaSurfaceNumber(AValue: Integer);
    function InterpolatedValue(SwiObs: TSwiObs; ZoneCount: Integer): double;
  public
    constructor Create;
    destructor Destroy; override;
    property ObsType: string read FObsType write SetObsType;
    property ZetaSurfaceNumber: Integer read FZetaSurfaceNumber
      write SetZetaSurfaceNumber;
    property SwiCellList: TSwiCellList read FSwiCellList;
  end;


  { TSwiObsExtractor }

  TSwiObsExtractor = class(TCustomObsExtractor)
  private
    FFileFormat: TSwiFileFormat;
    FNumberOfZetaSurfaces: Integer;
    FTotalNumberOfObs: Integer;
    procedure SetFileFormat(AValue: TSwiFileFormat);
    procedure SetNumberOfZetaSurfaces(AValue: Integer);
    procedure SetTotalNumberOfObs(AValue: Integer);
  public
    property NumberOfZetaSurfaces: Integer read FNumberOfZetaSurfaces write SetNumberOfZetaSurfaces;
    property FileFormat: TSwiFileFormat read FFileFormat write SetFileFormat;
    property TotalNumberOfObs: Integer read FTotalNumberOfObs write SetTotalNumberOfObs;
    procedure ExtractSimulatedValues; override;
  end;

implementation

uses SimpleTextWriter;

{ TSwiObsValue }

procedure TSwiObsValue.SetObsType(AValue: string);
begin
  if FObsType=AValue then Exit;
  FObsType:=AValue;
end;

procedure TSwiObsValue.SetZetaSurfaceNumber(AValue: Integer);
begin
  if FZetaSurfaceNumber=AValue then Exit;
  FZetaSurfaceNumber:=AValue;
end;

function TSwiObsValue.InterpolatedValue(SwiObs: TSwiObs; ZoneCount: Integer
  ): double;
var
  ObsIndex: Integer;
  AnSwiObs: TSwiCell;
  ABinaryObs: TSwiCell;
  TimeIndex: integer;
  BeforeTimeIndex: Integer;
  SwiTime: Double;
  AfterTimeIndex: Integer;
  DeltaT: Double;
  LocalObsName: string;
  ObsZeta: Integer;
  TotalFraction: double;
  BeforeValue: double;
  AfterValue: double;
  Fraction: Double;
  ObsPosition: integer;
const
  Epsilon = 0.01;
  procedure HandleSwiObs(AnObs: TSwiCell);
  var
    ObsIndex: integer;
    AValue: Double;
  begin
    TotalFraction := TotalFraction + AnObs.Fraction;
    ObsIndex := (AnObs.Number-1)*ZoneCount + ZetaSurfaceNumber -1;
    AValue := SwiObs.ObservationValue[BeforeTimeIndex, ObsIndex];
    BeforeValue := BeforeValue + AValue*AnObs.Fraction;
    if BeforeTimeIndex <> AfterTimeIndex then
    begin
      AValue := SwiObs.ObservationValue[AfterTimeIndex, ObsIndex];
      AfterValue := AfterValue + AValue*AnObs.Fraction;
    end;
  end;
begin
  Assert(FSwiCellList.Count > 0);
  Assert(SwiObs.TimeCount > 0);
  TimeIndex := SwiObs.IndexOfClosestTime(Time);
  SwiTime := SwiObs.Times[TimeIndex];
  if SwiTime = Time then
  begin
    BeforeTimeIndex := TimeIndex;
    AfterTimeIndex := TimeIndex;
  end
  else if(Time < SwiTime) then
  begin
    BeforeTimeIndex := TimeIndex-1;
    AfterTimeIndex := TimeIndex;
  end
  else
  begin
    BeforeTimeIndex := TimeIndex;
    AfterTimeIndex := TimeIndex+1;
  end;

  if BeforeTimeIndex < 0 then
  begin
    BeforeTimeIndex := 0;
  end;
  if AfterTimeIndex >= SwiObs.TimeCount then
  begin
    AfterTimeIndex := SwiObs.TimeCount -1;
  end;

  if BeforeTimeIndex <> AfterTimeIndex then
  begin
    DeltaT := SwiObs.Times[AfterTimeIndex] - SwiObs.Times[BeforeTimeIndex];
    if Abs(Time - SwiTime)/DeltaT < Epsilon then
    begin
      BeforeTimeIndex := TimeIndex;
      AfterTimeIndex := TimeIndex;
    end;
  end;

  TotalFraction := 0;
  BeforeValue := 0;
  AfterValue := 0;
  if SwiObs.FileFormat = sffAscii then
  begin
    for ObsIndex := 0 to FSwiCellList.Count - 1 do
    begin
      AnSwiObs := FSwiCellList[ObsIndex];
      ObsPosition := (AnSwiObs.Number-1)*ZoneCount + ZetaSurfaceNumber -1;
      LocalObsName := SwiObs.ObsName[ObsPosition];
      ObsZeta := StrToInt(RightStr(LocalObsName, 3));
      Assert(ObsZeta = ZetaSurfaceNumber);
      LocalObsName := Copy(LocalObsName, 1, Length(LocalObsName)-3);
      Assert(UpperCase(AnSwiObs.Name) = UpperCase(LocalObsName));
      HandleSwiObs(AnSwiObs);
    end;
  end
  else
  begin
    for ObsIndex := 0 to FSwiCellList.Count - 1 do
    begin
      ABinaryObs := FSwiCellList[ObsIndex];
      HandleSwiObs(ABinaryObs);
    end;
  end;
  Assert(TotalFraction > 0);
  if BeforeTimeIndex = AfterTimeIndex then
  begin
    result := BeforeValue/TotalFraction;
  end
  else
  begin
    BeforeValue := BeforeValue/TotalFraction;
    AfterValue := AfterValue/TotalFraction;
    DeltaT := SwiObs.Times[AfterTimeIndex] - SwiObs.Times[BeforeTimeIndex];
    Fraction := (Time - SwiObs.Times[BeforeTimeIndex])/DeltaT;
    result := BeforeValue + Fraction*(AfterValue-BeforeValue);
  end;
end;

constructor TSwiObsValue.Create;
begin
  FSwiCellList := TSwiCellList.Create;
end;

destructor TSwiObsValue.Destroy;
begin
  FSwiCellList.Free;
  inherited Destroy;
end;

{ TSwiObsExtractor }

procedure TSwiObsExtractor.SetFileFormat(AValue: TSwiFileFormat);
begin
  if FFileFormat=AValue then Exit;
  FFileFormat:=AValue;
end;

procedure TSwiObsExtractor.SetNumberOfZetaSurfaces(AValue: Integer);
begin
  if FNumberOfZetaSurfaces=AValue then Exit;
  FNumberOfZetaSurfaces:=AValue;
end;

procedure TSwiObsExtractor.SetTotalNumberOfObs(AValue: Integer);
begin
  if FTotalNumberOfObs=AValue then Exit;
  FTotalNumberOfObs:=AValue;
end;

procedure TSwiObsExtractor.ExtractSimulatedValues;
var
  SwiObs: TSwiObs;
  Reader: TSimpleStreamReader;
  BinaryReader: TFileStream;
  SwiObservation: TSwiObsValue;
  ObsIndex: Integer;
begin
  Reader := nil;
  BinaryReader := nil;
  SwiObs:= TSwiObs.Create;
  try
    case FileFormat of
      sffAscii:
        begin
          Reader :=  TSimpleStreamReader.Create(ModelOutputFileName);
          SwiObs.ReadAsciiSwiObs(Reader);
        end;
      sffBinarySingle, sffBinaryDouble:
        begin
          BinaryReader := TFileStream.Create(ModelOutputFileName,
            fmOpenRead or fmShareDenyWrite);
          SwiObs.ReadBinarySwiObs(BinaryReader, TotalNumberOfObs, FileFormat);
        end;
      else
        Assert(False);
    end;

    for ObsIndex := 0 to Pred(FObsValueList.Count) do
    begin
      SwiObservation := FObsValueList[ObsIndex] as TSwiObsValue;
      SwiObservation.SimulatedValue := SwiObservation.InterpolatedValue(SwiObs, NumberOfZetaSurfaces);
    end;
  finally
    BinaryReader.Free;
    Reader.Free;
    SwiObs.Free;
  end;
end;

end.

