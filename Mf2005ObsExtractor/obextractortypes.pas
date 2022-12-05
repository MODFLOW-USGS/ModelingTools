unit ObExtractorTypes;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

const
  MissingValue = -1e31;

type
  TDoubleArray = array of double;

  TCustomObsValue = class(TObject)
    ObsName: string;
    ObsTime: double;
    SimulatedValue: double;
    ObservedValue: double;
  end;

  TCustomWeightedObsValue = class(TCustomObsValue)
    Weight: double;
    Print: Boolean;
  end;

{$ifdef FPC}
  TWeightedObsValueList = specialize TList<TCustomWeightedObsValue>;
  TCustomWeightedObsValueObjectList = specialize TObjectList<TCustomWeightedObsValue>;
  TCustomObsValueDictionary = specialize TDictionary<string, TCustomObsValue>;
{$ELSE}
  TWeightedObsValueList = TList<TCustomWeightedObsValue>;
  TCustomWeightedObsValueObjectList = TObjectList<TCustomWeightedObsValue>;
  TCustomObsValueDictionary = TDictionary<string, TCustomObsValue>;
{$ENDIF}

  { TCustomObsExtractor }

  TCustomObsExtractor = class(TObject)
  private
    FOutputFileName: string;
    function GetObsCount: integer;
    procedure SetOutputFileName(AValue: string);
  protected
    FObsValueList: TWeightedObsValueList;
  public
    Constructor Create;
    destructor Destroy; override;
    property ModelOutputFileName: string read FOutputFileName
      write SetOutputFileName;
    procedure AddObs(Obs: TCustomWeightedObsValue);
    property ObsCount: integer read GetObsCount;
    procedure ExtractSimulatedValues; virtual; abstract;
  end;

function FortranStrToFloat(AString: string): Extended;

function RemoveQuotes(AString: string): string;

implementation

uses
  System.Math, System.StrUtils;

function FortranStrToFloat(AString: string): Extended;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := StrToFloat(AString);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;


function RemoveQuotes(AString: string): string;
begin
  Assert(Length(AString) > 0);
  if (AString[1] = '"') and (AString[Length(AString)] = '"') then
  begin
    AString := Copy(AString, 2, Length(AString)-2);
  end
  else if (AString[1] = '''') and (AString[Length(AString)] = '''') then
  begin
    AString := Copy(AString, 2, Length(AString)-2);
  end;
  result := AString;
end;

{ TCustomObsExtractor }

function TCustomObsExtractor.GetObsCount: integer;
begin
  result := FObsValueList.Count;
end;

procedure TCustomObsExtractor.SetOutputFileName(AValue: string);
begin
  if FOutputFileName=AValue then Exit;
  FOutputFileName:=AValue;
end;

constructor TCustomObsExtractor.Create;
begin
  FObsValueList := TWeightedObsValueList.Create;
  FOutputFileName := '';
end;

destructor TCustomObsExtractor.Destroy;
begin
  FObsValueList.Free;
  inherited Destroy;
end;

procedure TCustomObsExtractor.AddObs(Obs: TCustomWeightedObsValue);
begin
  FObsValueList.Add(Obs);
end;


end.

