unit SwiObsReaderUnit;

{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  streamex,
{$ENDIF}
  Classes,
  RealListUnit, 
  Generics.Collections,
  SysUtils;

type
  TDoubleArray = array of Double;
  TValues = TList<TDoubleArray>;
  TSwiFileFormat = (sffAscii, sffBinarySingle, sffBinaryDouble);

  { TSwiObs }

  TSwiObs = class(TObject)
  private
    FTimes: TRealList;
    FNames: TStringList;
    FValues: TValues;
    function GetObservationValue(TimeIndex, ObsIndex: Integer): double;
    function GetObsName(ObsIndex: Integer): string;
    function GetTime(TimeIndex: Integer): Double;
    function GetTimeCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Times[Index: Integer]: Double read GetTime;
    function IndexOfClosestTime(AValue: double): integer;
    property ObservationValue[TimeIndex, ObsIndex: Integer]: double
      read GetObservationValue;
    property ObsName[ObsIndex: Integer]: string read GetObsName;
    property TimeCount: Integer read GetTimeCount;
    procedure ReadAsciiSwiObs(Reader: TStreamReader);
    procedure ReadBinarySwiObs(Reader: TFileStream; NumberOfObs: Integer;
      FileFormat: TSwiFileFormat);
  end;

implementation

uses
  SwiObsUtilities;

resourcestring
  StrErrorConvertingS = 'Error converting "%s" to a real number.';

{ TSwiObs }

constructor TSwiObs.Create;
begin
  FTimes := TRealList.Create;
  FNames := TStringList.Create;
  FValues := TValues.Create;
  FNames.Delimiter := ' ';
end;

destructor TSwiObs.Destroy;
begin
  FValues.Free;
  FNames.Free;
  FTimes.Free;
  inherited Destroy;
end;

function TSwiObs.GetObservationValue(TimeIndex, ObsIndex: Integer): double;
begin
  result := FValues[TimeIndex][ObsIndex];
end;

function TSwiObs.GetObsName(ObsIndex: Integer): string;
begin
  Result := FNames[ObsIndex];
end;

function TSwiObs.GetTime(TimeIndex: Integer): Double;
begin
  result := FTimes[TimeIndex];
end;

function TSwiObs.GetTimeCount: Integer;
begin
  result := FTimes.Count;
end;

function TSwiObs.IndexOfClosestTime(AValue: double): integer;
begin
  result := FTimes.IndexOfClosest(AValue);
end;

procedure TSwiObs.ReadAsciiSwiObs(Reader: TStreamReader);
var
  Splitter: TStringList;
  Values: TDoubleArray;
  ALine: string;
  AValue: Extended;
  ValueIndex: Integer;
begin
  Assert(Reader <> nil);
  ALine := Reader.ReadLine;
  Assert(ALine <> '', 'The SWI Observation file is empty.');
  FNames.DelimitedText := ALine;
  FNames.Delete(0);
  Assert(FNames.Count > 0);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    ALine := Reader.ReadLine;
    while ALine <> '' do
    begin
      SetLength(Values, FNames.Count);
      Splitter.DelimitedText := ALine;
      Assert(Splitter.Count = FNames.Count+1);
      if TryFortranStrToFloat(Splitter[0], AValue) then
      begin
        FTimes.Add(AValue)
      end
      else
      begin
        Assert(False, Format(StrErrorConvertingS, [Splitter[0]]));
      end;

      for ValueIndex := 1 to Splitter.Count - 1 do
      begin
        if TryFortranStrToFloat(Splitter[ValueIndex], AValue) then
        begin
          Values[ValueIndex-1] := AValue;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingS, [Splitter[ValueIndex]]));
        end;
      end;
      FValues.Add(Values);

      ALine := Reader.ReadLine;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TSwiObs.ReadBinarySwiObs(Reader: TFileStream; NumberOfObs: Integer;
  FileFormat: TSwiFileFormat);
var
  Values: TDoubleArray;
  Time: Double;
  ASingleValue: single;
  ADoubleValue: double;
  ObsIndex: Integer;
begin
  while Reader.Position < Reader.Size do
  begin
    SetLength(Values, NumberOfObs);

    ASingleValue := 0;
    Time := 0;
    ADoubleValue := 0;

    case FileFormat of
      sffBinarySingle:
        begin
          Reader.Read(ASingleValue, SizeOf(Single));
          Time := ASingleValue;
        end;
      sffBinaryDouble: Reader.Read(Time, SizeOf(double));
      else Assert(False);
    end;
    FTimes.Add(Time);

    for ObsIndex := 0 to NumberOfObs - 1 do
    begin
      case FileFormat of
        sffBinarySingle:
          begin
            Reader.Read(ASingleValue, SizeOf(Single));
            ADoubleValue := ASingleValue;
          end;
        sffBinaryDouble: Reader.Read(ADoubleValue, SizeOf(double));
        else Assert(False);
      end;
      Values[ObsIndex] := ADoubleValue;
    end;

    FValues.Add(Values);
  end;
end;

end.
