unit InputDataObservationsUnit;

interface

uses
  ObsInterfaceUnit, System.Generics.Collections;

type
  TIndividualObs = record
    Index: Integer;
    Value: double;
    Weight: Double;
  end;

  TInputObservation = class(TObject, IObservationItem)
  private
    FName: string;
    FObservationGroup: string;
    FIndividualObs: TList<TIndividualObs>;
    function GetName: string;
    function GetExportedName: string;
    function GetGUID: string;
    function GetScreenObject: TObject;
    function GetObservedValue: double;
    procedure SetObservedValue(const Value: double);
    function GetWeight: Double;
    function GetObservationGroup: string;

    function ObservationType: string;

    procedure SetName(const Value: string);
    procedure SetObservationGroup(const Value: string);
    function GetCount: Integer;
    function GetItem(Index: Integer): TIndividualObs;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      virtual; stdcall;

    // @name must contain at most 20 characters.
    property Name: string read GetName write SetName;
    property ExportedName: string read GetExportedName;
    property GUID: string read GetGUID;
    procedure ReplaceGUID;
    property ScreenObject: TObject read GetScreenObject;
    property ObservedValue: double read GetObservedValue;
    property Weight: Double read GetWeight;
    // @name must contain at most 12 characters.
    property ObservationGroup: string read GetObservationGroup
      write SetObservationGroup;

    constructor Create;
    destructor Destroy; override;
    function AddIndividualObs(Obs: TIndividualObs): Integer;
    property Items[Index: Integer]: TIndividualObs read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TInputObservationObjectList = TObjectList<TInputObservation>;

implementation

{ TInputObservation }

function TInputObservation.AddIndividualObs(Obs: TIndividualObs): Integer;
begin
  result := FIndividualObs.Add(Obs);
end;

constructor TInputObservation.Create;
begin
  inherited;
  FIndividualObs := TList<TIndividualObs>.Create;
end;

destructor TInputObservation.Destroy;
begin
  FIndividualObs.Free;
  inherited;
end;

function TInputObservation.GetCount: Integer;
begin
  result := FIndividualObs.Count;
end;

function TInputObservation.GetExportedName: string;
begin
  result := FName;
end;

function TInputObservation.GetGUID: string;
begin
  result := '';
end;

function TInputObservation.GetItem(Index: Integer): TIndividualObs;
begin
  result := FIndividualObs[Index];
end;

function TInputObservation.GetName: string;
begin
  result := FName;
end;

function TInputObservation.GetObservationGroup: string;
begin
  result := FObservationGroup
end;

function TInputObservation.GetObservedValue: double;
var
  Index: Integer;
  SumProducts: Double;
  SumWeights: Double;
  Obs: TIndividualObs;
begin
  if FIndividualObs.Count = 1 then
  begin
    result := FIndividualObs[0].Value;
    Exit;
  end;
  result := 0;
  SumProducts := 0;
  SumWeights := 0;
  for Index := 0 to FIndividualObs.Count - 1 do
  begin
    Obs := FIndividualObs[Index];
    SumProducts := SumProducts + Obs.Value * Obs.Weight;
    SumWeights := SumWeights + Obs.Weight;
  end;
  if SumWeights > 0 then
  begin
    result := SumProducts/SumWeights;
  end;
end;

function TInputObservation.GetScreenObject: TObject;
begin
  result := nil;
end;

function TInputObservation.GetWeight: Double;
var
  Index: Integer;
  SumWeights: Double;
  Obs: TIndividualObs;
begin
  if FIndividualObs.Count = 1 then
  begin
    result := FIndividualObs[0].Weight;
    Exit;
  end;
  result := 0;
  SumWeights := 0;
  for Index := 0 to FIndividualObs.Count - 1 do
  begin
    Obs := FIndividualObs[Index];
    SumWeights := SumWeights + Obs.Weight;
  end;
  if SumWeights > 0 then
  begin
    result := SumWeights/FIndividualObs.Count;
  end;
end;

function TInputObservation.ObservationType: string;
begin
  Result := ''
end;

function TInputObservation.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure TInputObservation.ReplaceGUID;
begin

end;

procedure TInputObservation.SetName(const Value: string);
begin
  FName := Copy(Value, 1, 20);
end;

procedure TInputObservation.SetObservationGroup(const Value: string);
begin
  FObservationGroup := Copy(Value, 1, 12);
end;

procedure TInputObservation.SetObservedValue(const Value: double);
begin
  Assert(False);
end;

function TInputObservation._AddRef: Integer;
begin
  result := 1;
end;

function TInputObservation._Release: Integer;
begin
  result := 1;
end;

end.
