unit Mt3dObservationResultsUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Dialogs, GoPhastTypes,
  System.Generics.Collections;

type
  TObsNameDictionary = TDictionary<string, string>;

  TMt3dObsCollection = class;

  TMt3dObsResult = class(TPhastCollectionItem)
  private
    FName: string;
    FStoredSimulatedValue: TRealStorage;
    FStoredTime: TRealStorage;
    FMt3dObsCollection: TMt3dObsCollection;
    FScreenObjectName: string;
    procedure SetName(const Value: string);
    procedure SetSimulatedValue(const Value: double);
    procedure SetStoredSimulatedValue(const Value: TRealStorage);
    procedure SetStoredTime(const Value: TRealStorage);
    procedure SetTime(const Value: double);
    function GetSimulatedValue: double;
    function GetTime: double;
    procedure SetScreenObjectName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Time: double read GetTime write SetTime;
    property SimulatedValue: double read GetSimulatedValue write SetSimulatedValue;
  published
    property Name: string read FName write SetName;
    property ScreenObjectName: string read FScreenObjectName write SetScreenObjectName;
    property StoredTime: TRealStorage read FStoredTime write SetStoredTime;
    property StoredSimulatedValue: TRealStorage read FStoredSimulatedValue
      write SetStoredSimulatedValue;
  end;

  TMt3dObsCollection = class(TPhastCollection)
  private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
    FFileName: string;
    FFileDate: TDateTime;
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ReadFromFile(const AFileName: string; ScreenObjectNameDictionary: TObsNameDictionary): boolean;
    function Add: TMt3dObsResult;
    function GetItem(Index: Integer): TMt3dObsResult;
    procedure SetItem(Index: Integer; const Value: TMt3dObsResult);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetFileName(const Value: string);
    property Items[Index: Integer]: TMt3dObsResult read GetItem
      write SetItem; default;
    procedure CalculateMaxValues;
  published
    property FileName: string read FFileName write SetFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
  end;


implementation

uses
  System.IOUtils;

resourcestring
  StrTheFileFromWhich = 'The file from which you are attempting to read ' +
  'MT3D bservation results, %s, does not exist.';

{ TMt3dObsResult }

procedure TMt3dObsResult.Assign(Source: TPersistent);
var
  ObsSource: TMt3dObsResult;
begin
  if Source is TMt3dObsResult then
  begin
    ObsSource := TMt3dObsResult(Source);
    Name := ObsSource.Name;
    Time := ObsSource.Time;
    SimulatedValue := ObsSource.SimulatedValue;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMt3dObsResult.Create(Collection: TCollection);
var
  InvalidateModelEvent: TNotifyEvent;
  LocalModel: TBaseModel;
begin
  FMt3dObsCollection := Collection as TMt3dObsCollection;
  LocalModel := FMt3dObsCollection.FModel;
  if LocalModel = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := LocalModel.DoInvalidate;
  end;
  FStoredSimulatedValue := TRealStorage.Create(InvalidateModelEvent);
  FStoredTime := TRealStorage.Create(InvalidateModelEvent);
  inherited;

end;

destructor TMt3dObsResult.Destroy;
begin
  FStoredTime.Free;
  FStoredSimulatedValue.Free;
  inherited;
end;

function TMt3dObsResult.GetSimulatedValue: double;
begin
  result := FStoredSimulatedValue.Value;
end;

function TMt3dObsResult.GetTime: double;
begin
  result := FStoredTime.Value;
end;

procedure TMt3dObsResult.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

procedure TMt3dObsResult.SetScreenObjectName(const Value: string);
begin
  SetStringProperty(FScreenObjectName, Value);
end;

procedure TMt3dObsResult.SetSimulatedValue(const Value: double);
begin
  FStoredSimulatedValue.Value := Value;
end;

procedure TMt3dObsResult.SetStoredSimulatedValue(const Value: TRealStorage);
begin
  FStoredSimulatedValue.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredTime(const Value: TRealStorage);
begin
  FStoredTime.Assign(Value);
end;

procedure TMt3dObsResult.SetTime(const Value: double);
begin
  FStoredTime.Value := Value;
end;

{ TMt3dObsCollection }

function TMt3dObsCollection.Add: TMt3dObsResult;
begin
  result := inherited Add as TMt3dObsResult
end;

procedure TMt3dObsCollection.Assign(Source: TPersistent);
var
  SourceCollection: TMt3dObsCollection;
begin
  if Source is TMt3dObsCollection then
  begin
    SourceCollection := TMt3dObsCollection(Source);
    FileName := SourceCollection.FileName;
    FileDate := SourceCollection.FileDate;
  end;
  inherited;
end;


procedure TMt3dObsCollection.CalculateMaxValues;
begin

end;

constructor TMt3dObsCollection.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TMt3dObsResult, InvalidateModelEvent);
end;

destructor TMt3dObsCollection.Destroy;
begin

  inherited;
end;

function TMt3dObsCollection.GetItem(Index: Integer): TMt3dObsResult;
begin
  result := inherited Items[Index] as TMt3dObsResult
end;

function TMt3dObsCollection.ReadFromFile(const AFileName: string; ScreenObjectNameDictionary: TObsNameDictionary): boolean;
const
  NameLength = 12;
  HeaderLength = 15;
var
  FileReader: TFileStream;
  Header: array[0..HeaderLength-1] of AnsiChar;
  ANameArray: array[0..NameLength-1] of AnsiChar;
  AName: string;
  Time: single;
  Value: Single;
  AnItem: TMt3dObsResult;
  ScreenObjectName: string;
begin
  result := False;
  if not TFile.Exists(AFileName) then
  begin
    Beep;
    MessageDlg(Format(StrTheFileFromWhich, [AFileName]), mtError, [mbOK], 0);
    Exit;
  end;

  Assert(ScreenObjectNameDictionary <> nil);
  FileReader := TFile.OpenRead(AFileName);
  try
    FileReader.Read(Header[0], SizeOf(AnsiChar)*HeaderLength);
    repeat
      FileReader.Read(ANameArray[0], SizeOf(AnsiChar)*NameLength);
      AName := Trim(String(ANameArray));
      if AName = '' then
      begin
        break;
      end;
      FileReader.Read(Time, SizeOf(Time));
      FileReader.Read(Value, SizeOf(Value));
      AnItem := Add;
      AnItem.Name := AName;
      AnItem.Time := Time;
      AnItem.SimulatedValue := Value;

      if not ScreenObjectNameDictionary.TryGetValue(AName, ScreenObjectName) then
      begin
        ScreenObjectName := ''
      end;

      AnItem.ScreenObjectName := ScreenObjectName;

    until FileReader.Position = FileReader.Size;
  finally
    FileReader.Free;
  end;

  FileName := AFileName;
  FileDate := TFile.GetLastWriteTime(FileName);

end;

procedure TMt3dObsCollection.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TMt3dObsCollection.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TMt3dObsCollection.SetItem(Index: Integer;
  const Value: TMt3dObsResult);
begin
  inherited Items[Index] := Value;
end;

end.
