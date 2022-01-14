unit PilotPointDataUnit;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, DataSetUnit,
  OrderedCollectionUnit, ModflowParameterUnit, GoPhastTypes, PestObsGroupUnit,
  PointCollectionUnit;

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
    FValues: TList<double>;
    FPoints: TSimplePointCollection;
    procedure SetCount(const Value: Integer);
    procedure SetDataArray(const Value: TDataArray);
    procedure SetFileName(const Value: string);
    procedure SetLayer(const Value: Integer);
    procedure SetParameter(const Value: TModflowSteadyParameter);
    procedure SetParameterIndex(const Value: Integer);
    procedure SetParamFamily(const Value: string);
    function GetValue(Index: Integer): double;
    procedure SetPoints(const Value: TSimplePointCollection);
  public
    Constructor Create;
    destructor Destroy; override;
    function ParameterName(Index: Integer): string;
    property DataArray: TDataArray read FDataArray write SetDataArray;
    property Parameter: TModflowSteadyParameter read FParameter write SetParameter;
    property ParameterIndex: Integer read FParameterIndex write SetParameterIndex;
    property FileName: string read FFileName write SetFileName;
    property Layer: Integer read FLayer write SetLayer;
    property Count: Integer read FCount write SetCount;
    property ParamFamily: string read FParamFamily write SetParamFamily;
    property Values[Index: Integer]: double read GetValue;
    procedure AddValue(AValue: double);
    property Points: TSimplePointCollection read FPoints write SetPoints;
  end;

  TPilotPointFiles = TObjectList<TPilotPointFileObject>;

  TStoredPilotParamDataItem = class(TCollectionItem)
  private
    FBaseParamName: string;
    FCount: Integer;
    FParamFamily: string;
    FFileName: string;
    FValues: TRealCollection;
    FLayer: Integer;
    FDataArrayName: string;
    FPoints: TSimplePointCollection;
    FObsGroupName: string;
    FPestObsGroup: TPestObservationGroup;
    procedure SetBaseParamName(const Value: string);
    procedure SetCount(const Value: Integer);
    procedure SetParamFamily(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetValues(const Value: TRealCollection);
    procedure SetLayer(const Value: Integer);
    procedure SetDataArrayName(const Value: string);
    procedure SetPoints(const Value: TSimplePointCollection);
    procedure SetObsGroupName(const Value: string);
    procedure SetPestObsGroup(const Value: TPestObservationGroup);
    function GetObsGroupName: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignPilotPointFileObject(Source: TPilotPointFileObject);
    function ParameterName(Index: Integer): string;
    property PestObsGroup: TPestObservationGroup read FPestObsGroup write SetPestObsGroup;
  published
    property ParamFamily: string read FParamFamily write SetParamFamily;
    property Count: Integer read FCount write SetCount;
    property BaseParamName: string read FBaseParamName write SetBaseParamName;
    property FileName: string read FFileName write SetFileName;
    property Values: TRealCollection read FValues write SetValues;
    property Layer: Integer read FLayer write SetLayer;
    Property DataArrayName: string read FDataArrayName write SetDataArrayName;
    property Points: TSimplePointCollection read FPoints write SetPoints;
    property ObsGroupName: string read GetObsGroupName write SetObsGroupName;
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

procedure TPilotPointFileObject.AddValue(AValue: double);
begin
  FValues.Add(AValue)
end;

constructor TPilotPointFileObject.Create;
begin
  FValues:= TList<double>.Create;
  FPoints := TSimplePointCollection.Create;
end;

destructor TPilotPointFileObject.Destroy;
begin
  FPoints.Free;
  FValues.Free;
  inherited;
end;

function TPilotPointFileObject.GetValue(Index: Integer): double;
begin
  result := FValues[Index];
end;

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

procedure TPilotPointFileObject.SetPoints(const Value: TSimplePointCollection);
begin
  FPoints.Assign(Value);
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
    FileName := SourceItem.FileName;
    Values := SourceItem.Values;
    Layer := SourceItem.Layer;
    DataArrayName := SourceItem.DataArrayName;
    Points := SourceItem.Points;
    ObsGroupName := SourceItem.ObsGroupName;
    PestObsGroup := SourceItem.PestObsGroup;
  end
  else
  begin
    inherited;
  end;
end;

procedure TStoredPilotParamDataItem.AssignPilotPointFileObject(
  Source: TPilotPointFileObject);
var
  VIndex: Integer;
begin
  ParamFamily := Source.ParamFamily;
  Count := Source.Count;
  BaseParamName := Source.Parameter.ParameterName;
  FileName := Source.FileName;
  FValues.Clear;
  FValues.Capacity := Source.Count;
  for VIndex := 0 to Source.Count - 1 do
  begin
    FValues.Add.Value := Source.Values[VIndex];
  end;
  Layer := Source.Layer;
  DataArrayName := Source.DataArray.Name;
  Points := Source.Points;
end;

constructor TStoredPilotParamDataItem.Create(Collection: TCollection);
var
  Dummy: TNotifyEvent;
begin
  inherited;
  Dummy := nil;
  FValues := TRealCollection.Create(Dummy);
  FPoints := TSimplePointCollection.Create;
end;

destructor TStoredPilotParamDataItem.Destroy;
begin
  FPoints.Free;
  FValues.Free;
  inherited;
end;

function TStoredPilotParamDataItem.GetObsGroupName: string;
begin
  if FPestObsGroup <> nil then
  begin
    result := FPestObsGroup.ObsGroupName;
  end
  else
  begin
    result := FObsGroupName;
  end;
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

procedure TStoredPilotParamDataItem.SetDataArrayname(const Value: string);
begin
  FDataArrayName := Value;
end;

procedure TStoredPilotParamDataItem.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TStoredPilotParamDataItem.SetLayer(const Value: Integer);
begin
  FLayer := Value;
end;

procedure TStoredPilotParamDataItem.SetObsGroupName(const Value: string);
begin
  FObsGroupName := Value;
end;

procedure TStoredPilotParamDataItem.SetParamFamily(const Value: string);
begin
  FParamFamily := Value;
end;

procedure TStoredPilotParamDataItem.SetPestObsGroup(
  const Value: TPestObservationGroup);
begin
  FPestObsGroup := Value;
  if FPestObsGroup <> nil then
  begin
    FObsGroupName := FPestObsGroup.ObsGroupName;
  end;
end;

procedure TStoredPilotParamDataItem.SetPoints(
  const Value: TSimplePointCollection);
begin
  FPoints.Assign(Value);
end;

procedure TStoredPilotParamDataItem.SetValues(const Value: TRealCollection);
begin
  FValues.Assign(Value);
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
