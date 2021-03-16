unit FootprintBoundary;

interface

uses
  Classes, ModflowBoundaryUnit, GoPhastTypes, SubscriptionUnit,
  FormulaManagerUnit;

type
  TFootprintWell = class(TModflowSteadyBoundary)
  private
    FWithdrawalFormula: TFormulaObject;
    FWithdrawalObserver: TObserver;
    function GetWithdrawal: string;
    procedure SetWithdrawal(const Value: string);
    function GetWithdrawalObserver: TObserver;
    property WithdrawalObserver: TObserver
      read GetWithdrawalObserver;
  protected
    procedure HandleChangedValue(Observer: TObserver); override;
    function GetUsedObserver: TObserver; override;
    procedure CreateFormulaObjects; override;
    procedure CreateObservers; override;
    function BoundaryObserverPrefix: string; override;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure InvalidateDisplay;
  published
    property Withdrawal: string read GetWithdrawal write SetWithdrawal;
  end;

implementation

const
  PumpingRatePosition = 0;


{ TFootprintWell }

procedure TFootprintWell.Assign(Source: TPersistent);
begin
  if Source is TFootprintWell then
  begin
    IsUsed := TFootprintWell(Source).IsUsed;
    Withdrawal := TFootprintWell(Source).Withdrawal;
  end
  else
  begin
    inherited;
  end;
end;

function TFootprintWell.BoundaryObserverPrefix: string;
begin
  result := 'Footprint_Boundary_';
end;

constructor TFootprintWell.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  Withdrawal := '0';
end;

procedure TFootprintWell.CreateFormulaObjects;
begin
  FWithdrawalFormula := CreateFormulaObjectBlocks(dsoTop);
end;

procedure TFootprintWell.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(WithdrawalObserver);
  end;
end;

destructor TFootprintWell.Destroy;
begin
  Withdrawal := '0';
  StopTalkingToAnyone;
  inherited;
end;

function TFootprintWell.GetWithdrawal: string;
begin
  Result := FWithdrawalFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(PumpingRatePosition);
  end;
end;

function TFootprintWell.GetWithdrawalObserver: TObserver;
begin
  if FWithdrawalObserver = nil then
  begin
    { TODO -cFootprint : Replace nil with a data array or other observer}
    CreateObserver('Footprint_Pumping_Rate',
      FWithdrawalObserver, nil);
  end;
  result := FWithdrawalObserver;
end;

function TFootprintWell.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    { TODO -cFootprint : Replace nil with a data array or other observer}
    CreateObserver('Footprint_Well_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFootprintWell.HandleChangedValue(Observer: TObserver);
begin
  // invalidate display here.
  { TODO -cfootprint : Does this need to be finished. }
end;

procedure TFootprintWell.InvalidateDisplay;
begin
  if Used and (ParentModel <> nil) then
  begin
    HandleChangedValue(WithdrawalObserver);
  end;
end;

procedure TFootprintWell.SetWithdrawal(const Value: string);
begin
  UpdateFormulaBlocks(Value, PumpingRatePosition, FWithdrawalFormula);
end;

end.
