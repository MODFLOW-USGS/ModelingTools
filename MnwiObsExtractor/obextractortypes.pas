unit ObExtractorTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

const
  MissingValue = -1e-31;

type
  TDoubleArray = array of double;

  TCustomObsValue = class(TObject)
    ObsName: string;
    ObsTime: double;
    SimulatedValue: double;
    ObservedValue: double;
    Weight: double;
    Print: Boolean;
  end;

  TObsValueList = specialize TList<TCustomObsValue>;
  TCustomObsValueObjectList = specialize TObjectList<TCustomObsValue>;
  TCustomObsValueDictionary = specialize TDictionary<string, TCustomObsValue>;

  { TCustomObsExtractor }

  TCustomObsExtractor = class(TObject)
  private
    FOutputFileName: string;
    function GetObsCount: integer;
    procedure SetOutputFileName(AValue: string);
    //function GetItem(Index: integer): TMnwiObsValue;
    //function Value(MnwiRecord: TMnwiOutRecord; Index: Integer): double;
    //property Items[Index: integer]: TMnwiObsValue read GetItem; default;
  protected
    FObsValueList: TObsValueList;
  public
    Constructor Create;
    destructor Destroy; override;
    property OutputFileName: string read FOutputFileName
      write SetOutputFileName;
    procedure AddObs(Obs: TCustomObsValue);
    property ObsCount: integer read GetObsCount;
    procedure ExtractSimulatedValues; virtual; abstract;
  end;


function RemoveQuotes(AString: string): string;

implementation

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
  FObsValueList := TObsValueList.Create;
  FOutputFileName := '';
end;

destructor TCustomObsExtractor.Destroy;
begin
  FObsValueList.Free;
  inherited Destroy;
end;

procedure TCustomObsExtractor.AddObs(Obs: TCustomObsValue);
begin
  FObsValueList.Add(Obs);
end;


end.

