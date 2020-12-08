unit PilotPointDataUnit;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, DataSetUnit,
  OrderedCollectionUnit, ModflowParameterUnit;

type
  TPilotPointFileObject = class(TObject)
  private
    FFileName: string;
    FLayer: Integer;
    FCount: Integer;
    FParameterIndex: Integer;
    FDataArray: TDataArray;
    FParameter: TModflowSteadyParameter;
    FParamFamily: string;
    procedure SetCount(const Value: Integer);
    procedure SetDataArray(const Value: TDataArray);
    procedure SetFileName(const Value: string);
    procedure SetLayer(const Value: Integer);
    procedure SetParameter(const Value: TModflowSteadyParameter);
    procedure SetParameterIndex(const Value: Integer);
    procedure SetParamFamily(const Value: string);
  public
    function ParameterName(Index: Integer): string;
    property DataArray: TDataArray read FDataArray write SetDataArray;
    property Parameter: TModflowSteadyParameter read FParameter write SetParameter;
    property ParameterIndex: Integer read FParameterIndex write SetParameterIndex;
    property FileName: string read FFileName write SetFileName;
    property Layer: Integer read FLayer write SetLayer;
    property Count: Integer read FCount write SetCount;
    property ParamFamily: string read FParamFamily write SetParamFamily;
  end;

  TPilotPointFiles = TObjectList<TPilotPointFileObject>;

  TStoredPilotParamDataItem = class(TCollectionItem)
  private
    FBaseParamName: string;
    FCount: Integer;
    FParamFamily: string;
    procedure SetBaseParamName(const Value: string);
    procedure SetCount(const Value: Integer);
    procedure SetParamFamily(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignPilotPointFileObject(Source: TPilotPointFileObject);
    function ParameterName(Index: Integer): string;
  published
    property ParamFamily: string read FParamFamily write SetParamFamily;
    property Count: Integer read FCount write SetCount;
    property BaseParamName: string read FBaseParamName write SetBaseParamName;
  end;

  TStoredPilotParamDataCollection = class(TCollection)
  private
    function GetAnItem(Index: Integer): TStoredPilotParamDataItem;
    procedure SetAnItem(Index: Integer; const Value: TStoredPilotParamDataItem);
  public
    constructor Create;
    procedure AssignPilotPointFileObjects(Source: TPilotPointFiles);
    procedure AddPilotPointFileObjects(Source: TPilotPointFiles);
    function Add: TStoredPilotParamDataItem;
    property Items[Index: Integer]: TStoredPilotParamDataItem read GetAnItem
      write SetAnItem; default;
  end;



implementation

function ParName(Index, Count: Integer; const ParamFamily: string): string;
begin
  Assert(Index > 0);
  Assert(Index <= Count);
  Assert(ParamFamily <> '');
  result := Format('%0:s%1:d', [ParamFamily, Index]);
end;


{ TPilotPointFileObject }

function TPilotPointFileObject.ParameterName(Index: Integer): string;
begin
  result := ParName(Index, Count, ParamFamily);
end;

procedure TPilotPointFileObject.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

procedure TPilotPointFileObject.SetDataArray(const Value: TDataArray);
begin
  FDataArray := Value;
end;

procedure TPilotPointFileObject.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TPilotPointFileObject.SetLayer(const Value: Integer);
begin
  FLayer := Value;
end;

procedure TPilotPointFileObject.SetParameter(
  const Value: TModflowSteadyParameter);
begin
  FParameter := Value;
end;

procedure TPilotPointFileObject.SetParameterIndex(const Value: Integer);
begin
  FParameterIndex := Value;
end;

procedure TPilotPointFileObject.SetParamFamily(const Value: string);
begin
  FParamFamily := Value;
end;

{ TStoredPilotParamDataItem }

procedure TStoredPilotParamDataItem.Assign(Source: TPersistent);
var
  SourceItem: TStoredPilotParamDataItem;
begin
  if Source is TStoredPilotParamDataItem then
  begin
    SourceItem := TStoredPilotParamDataItem(Source);
    ParamFamily := SourceItem.ParamFamily;
    Count := SourceItem.Count;
    BaseParamName := SourceItem.BaseParamName;
  end
  else
  begin
    inherited;
  end;
end;

procedure TStoredPilotParamDataItem.AssignPilotPointFileObject(
  Source: TPilotPointFileObject);
begin
  ParamFamily := Source.ParamFamily;
  Count := Source.Count;
  BaseParamName := Source.Parameter.ParameterName;
end;

function TStoredPilotParamDataItem.ParameterName(Index: Integer): string;
begin
  result := ParName(Index, Count, ParamFamily);
end;

procedure TStoredPilotParamDataItem.SetBaseParamName(const Value: string);
begin
  FBaseParamName := Value;
end;

procedure TStoredPilotParamDataItem.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

procedure TStoredPilotParamDataItem.SetParamFamily(const Value: string);
begin
  FParamFamily := Value;
end;

{ TStoredPilotParamDataCollection }

function TStoredPilotParamDataCollection.Add: TStoredPilotParamDataItem;
begin
  result := inherited Add as TStoredPilotParamDataItem
end;

procedure TStoredPilotParamDataCollection.AddPilotPointFileObjects(
  Source: TPilotPointFiles);
var
  index: Integer;
begin
  Capacity := Count + Source.Count;
  for index := 0 to Source.Count - 1 do
  begin
    Add.AssignPilotPointFileObject(Source[index]);
  end;
end;

procedure TStoredPilotParamDataCollection.AssignPilotPointFileObjects(
  Source: TPilotPointFiles);
begin
  Clear;
  AddPilotPointFileObjects(Source);
end;

constructor TStoredPilotParamDataCollection.Create;
begin
  inherited Create(TStoredPilotParamDataItem)
end;

function TStoredPilotParamDataCollection.GetAnItem(
  Index: Integer): TStoredPilotParamDataItem;
begin
  result := inherited Items[index] as TStoredPilotParamDataItem
end;

procedure TStoredPilotParamDataCollection.SetAnItem(Index: Integer;
  const Value: TStoredPilotParamDataItem);
begin
  inherited Items[index] := Value;
end;

end.
