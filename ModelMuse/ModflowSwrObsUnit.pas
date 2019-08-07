unit ModflowSwrObsUnit;

interface

uses
  Classes, GoPhastTypes, ModflowSwrStructureUnit;

type
  TSwrObsType = (sotStage, sotDepth, sotBottom, sotFlow, sotStructure, sotBaseFlow);
  TSwrObsTypes = set of TSwrObsType;

  TSwrObsItem = class(TPhastCollectionItem)
  private
    FObsName: string;
    FObservationReach: Integer;
    FConnectedReach: integer;
    FObsType: TSwrObsType;
    FObservationLayer: Integer;
    FStructureName: string;
    FStructure: TStructure;
    procedure SetConnectedReach(const Value: integer);
    procedure SetObservationReach(const Value: Integer);
    procedure SetObsName(const Value: string);
    procedure SetObsType(const Value: TSwrObsType);
    procedure SetObservationLayer(const Value: Integer);
    procedure SetStructureName(const Value: string);
    function GetStructureName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    // COBSNAME
    property ObsName: string read FObsName write SetObsName;
    property ObsType: TSwrObsType read FObsType write SetObsType;
    property ObservationReach: Integer read FObservationReach write SetObservationReach;
    property ConnectedReachOrStructure: integer read FConnectedReach write SetConnectedReach;
    property ObservationLayer: Integer read FObservationLayer write SetObservationLayer;
    property StructureName: string read GetStructureName write SetStructureName;
  end;

  TSwrObsCollection = class(TPhastCollection)
    function GetItem(Index: Integer): TSwrObsItem;
    procedure SetItem(Index: Integer; const Value: TSwrObsItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TSwrObsItem read GetItem write SetItem; default;
    function Add: TSwrObsItem;
    procedure Loaded;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit;

{ TSwrObsItem }

procedure TSwrObsItem.Assign(Source: TPersistent);
var
  SourceItem: TSwrObsItem;
begin
  if Source is TSwrObsItem then
  begin
    SourceItem := TSwrObsItem(Source);
    ObsName := SourceItem.ObsName;
    ObsType := SourceItem.ObsType;
    ObservationReach := SourceItem.ObservationReach;
    ConnectedReachOrStructure := SourceItem.ConnectedReachOrStructure;
    ObservationLayer := SourceItem.ObservationLayer;
    StructureName := SourceItem.StructureName;
  end
  else
  begin
    inherited;
  end;
end;

function TSwrObsItem.GetStructureName: string;
begin
  if FStructure = nil then
  begin
    result := FStructureName
  end
  else
  begin
    result := FStructure.Name;
  end;
end;

procedure TSwrObsItem.SetConnectedReach(const Value: integer);
begin
  SetIntegerProperty(FConnectedReach, Value);
end;

procedure TSwrObsItem.SetObservationLayer(const Value: Integer);
begin
  SetIntegerProperty(FObservationLayer, Value);
end;

procedure TSwrObsItem.SetObservationReach(const Value: Integer);
begin
  SetIntegerProperty(FObservationReach, Value);
end;

procedure TSwrObsItem.SetObsName(const Value: string);
begin
  SetStringProperty(FObsName, Value);
end;

procedure TSwrObsItem.SetObsType(const Value: TSwrObsType);
begin
  if FObsType <> Value then
  begin
    FObsType := Value;
    InvalidateModel;
  end;
end;

procedure TSwrObsItem.SetStructureName(const Value: string);
var
  LocalModel: TPhastModel;
begin
  SetStringProperty(FStructureName, Value);
  LocalModel := frmGoPhast.PhastModel;

  FStructure := nil;
  if FStructureName <> '' then
  begin
    FStructure :=LocalModel.SwrStructures.GetStructureByName(FStructureName)
  end;
end;

{ TSwrObsCollection }

function TSwrObsCollection.Add: TSwrObsItem;
begin
  Result := inherited Add as TSwrObsItem;
end;

constructor TSwrObsCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TSwrObsItem, InvalidateModelEvent);
end;

function TSwrObsCollection.GetItem(Index: Integer): TSwrObsItem;
begin
  Result := inherited Items[Index] as TSwrObsItem
end;

procedure TSwrObsCollection.Loaded;
var
  index: Integer;
  AnItem: TSwrObsItem;
begin

  for index := 0 to Count - 1 do
  begin
    AnItem := Items[index];
    AnItem.StructureName := AnItem.StructureName;
  end;
end;

procedure TSwrObsCollection.SetItem(Index: Integer; const Value: TSwrObsItem);
begin
  inherited Items[Index] := Value;
end;

end.
