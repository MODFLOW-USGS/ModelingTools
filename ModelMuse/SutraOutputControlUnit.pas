unit SutraOutputControlUnit;

interface

uses
  GoPhastTypes, Classes;

type
  TSutraListingOption = (sloNPrint, sloCNodal, sloCElment, sloCIncid, sloCPandS,
    sloCVel, sloCCorT, sloCBudg, sloCScrn, sloCPause);
  TSutraListingOptions = set of TSutraListingOption;

  TSutraNodeEleOption = (neoPrintFirst, neoNumber, neoCoordinates,
    neoPressure, neoU, neoSaturation, neoVelocities);
  TSutraNodeEleOptions = set of TSutraNodeEleOption;

  TSutraOutputControl = class(TGoPhastPersistent)
  private
    FSpecifiedPressurePrintFrequency: integer;
    FNE_PrintFrequency: integer;
    FListingOptions: TSutraListingOptions;
    FFluidSourcePrintFrequency: integer;
    FSoluteEnergySourcePrintFrequency: integer;
    FListAll: Boolean;
    FNodeElementOptions: TSutraNodeEleOptions;
    FListingPrintFrequency: integer;
    FSpecifiedConcTempPrintFrequency: integer;
    FMaxObsPerLine: integer;
    FGeneralizedTransportPrintFrequency: integer;
    FGeneralizedFlowPrintFrequency: integer;
    FLakePrintFrequency: Integer;
    procedure SetElementOptions(const Value: TSutraNodeEleOptions);
    procedure SetFluidSourcePrintFrequency(const Value: integer);
    procedure SetListAll(const Value: Boolean);
    procedure SetListingOptions(const Value: TSutraListingOptions);
    procedure SetListingPrintFrequency(const Value: integer);
    procedure SetMaxObsPerLine(const Value: integer);
    procedure SetNE_PrintFrequency(const Value: integer);
    procedure SetSoluteEnergySourcePrintFrequency(const Value: integer);
    procedure SetSpecifiedConcTempPrintFrequency(const Value: integer);
    procedure SetSpecifiedPressurePrintFrequency(const Value: integer);
    procedure SetGeneralizedFlowPrintFrequency(const Value: integer);
    procedure SetGeneralizedTransportPrintFrequency(const Value: integer);
    procedure SetLakePrintFrequency(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure InitializeVariables;
  published
    // NPRINT (also affected by @link(ListingOptions).
    property ListingPrintFrequency: integer read FListingPrintFrequency
      write SetListingPrintFrequency stored True;
    property ListingOptions: TSutraListingOptions read FListingOptions
      write SetListingOptions stored True;
    // NCOLPR, LCOLPR
    property NE_PrintFrequency: integer read FNE_PrintFrequency
      write SetNE_PrintFrequency stored True;
    property NodeElementOptions: TSutraNodeEleOptions read FNodeElementOptions
      write SetElementOptions stored True;
    // NOBLIN
    property MaxObsPerLine: integer read FMaxObsPerLine write SetMaxObsPerLine
      stored True;
    // NBCFPR
    property FluidSourcePrintFrequency: integer read FFluidSourcePrintFrequency
      write SetFluidSourcePrintFrequency stored True;
    // NBCSPR
    property SoluteEnergySourcePrintFrequency: integer
      read FSoluteEnergySourcePrintFrequency
      write SetSoluteEnergySourcePrintFrequency stored True;
    // NBCPPR
    property SpecifiedPressurePrintFrequency: integer
      read FSpecifiedPressurePrintFrequency
      write SetSpecifiedPressurePrintFrequency stored True;
    // NBCUPR
    property SpecifiedConcTempPrintFrequency: integer
      read FSpecifiedConcTempPrintFrequency
      write SetSpecifiedConcTempPrintFrequency stored True;
    // NBGPPR
    property GeneralizedFlowPrintFrequency: integer
      read FGeneralizedFlowPrintFrequency
      write SetGeneralizedFlowPrintFrequency stored True;
    // NBGUPR
    property GeneralizedTransportPrintFrequency: integer
      read FGeneralizedTransportPrintFrequency
      write SetGeneralizedTransportPrintFrequency stored True;
    // Lake output cycle (NLAKPR)
    property LakePrintFrequency: Integer read FLakePrintFrequency write SetLakePrintFrequency;
    // CINACT
    property ListAll: Boolean read FListAll write SetListAll stored True;
  end;

implementation

uses
  frmSutraOutputControlUnit;

{ TSutraOutputControl }

procedure TSutraOutputControl.Assign(Source: TPersistent);
var
  OutputControlSource: TSutraOutputControl;
begin
  if Source is TSutraOutputControl then
  begin
    OutputControlSource := TSutraOutputControl(Source);

    ListingPrintFrequency := OutputControlSource.ListingPrintFrequency;
    ListingOptions := OutputControlSource.ListingOptions;
    NE_PrintFrequency := OutputControlSource.NE_PrintFrequency;
    NodeElementOptions := OutputControlSource.NodeElementOptions;
    MaxObsPerLine := OutputControlSource.MaxObsPerLine;
    FluidSourcePrintFrequency := OutputControlSource.FluidSourcePrintFrequency;
    SoluteEnergySourcePrintFrequency :=
      OutputControlSource.SoluteEnergySourcePrintFrequency;
    SpecifiedPressurePrintFrequency :=
      OutputControlSource.SpecifiedPressurePrintFrequency;
    SpecifiedConcTempPrintFrequency :=
      OutputControlSource.SpecifiedConcTempPrintFrequency;
    GeneralizedFlowPrintFrequency :=
      OutputControlSource.GeneralizedFlowPrintFrequency;
    GeneralizedTransportPrintFrequency :=
      OutputControlSource.GeneralizedTransportPrintFrequency;
    LakePrintFrequency := OutputControlSource.LakePrintFrequency;
    ListAll := OutputControlSource.ListAll;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraOutputControl.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;
  InitializeVariables;
end;

procedure TSutraOutputControl.InitializeVariables;
begin
  FListingPrintFrequency := 9999;
  FListingOptions := [sloNPrint, sloCPandS,
    sloCVel, sloCCorT, sloCBudg, sloCScrn];
  FNE_PrintFrequency := 9999;
  // By default, don't print node number. It interferes with Model Viewer
  // being able to read the files.
  FNodeElementOptions := [neoPrintFirst, {neoNumber,} neoCoordinates,
    neoPressure, neoU, neoSaturation, neoVelocities];
  FMaxObsPerLine := 10000;
  FFluidSourcePrintFrequency := 9999;
  FSoluteEnergySourcePrintFrequency := 9999;
  FSpecifiedPressurePrintFrequency := 9999;
  FSpecifiedConcTempPrintFrequency := 9999;
  FGeneralizedFlowPrintFrequency := 9999;
  FGeneralizedTransportPrintFrequency := 9999;
  FLakePrintFrequency := 9999;
  FListAll := True;
end;

procedure TSutraOutputControl.SetElementOptions(const Value
  : TSutraNodeEleOptions);
begin
  if FNodeElementOptions <> Value then
  begin
    FNodeElementOptions := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOutputControl.SetFluidSourcePrintFrequency
  (const Value: integer);
begin
  SetIntegerProperty(FFluidSourcePrintFrequency, Value);
end;

procedure TSutraOutputControl.SetGeneralizedFlowPrintFrequency(
  const Value: integer);
begin
  SetIntegerProperty(FGeneralizedFlowPrintFrequency, Value);
end;

procedure TSutraOutputControl.SetGeneralizedTransportPrintFrequency(
  const Value: integer);
begin
  SetIntegerProperty(FGeneralizedTransportPrintFrequency, Value);
end;

procedure TSutraOutputControl.SetLakePrintFrequency(const Value: Integer);
begin
  SetIntegerProperty(FLakePrintFrequency, Value);
end;

procedure TSutraOutputControl.SetListAll(const Value: Boolean);
begin
  SetBooleanProperty(FListAll, Value);
  FListAll := Value;
end;

procedure TSutraOutputControl.SetListingOptions(const Value
  : TSutraListingOptions);
begin
  if FListingOptions <> Value then
  begin
    FListingOptions := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOutputControl.SetListingPrintFrequency(const Value: integer);
begin
  SetIntegerProperty(FListingPrintFrequency, Value);
end;

procedure TSutraOutputControl.SetMaxObsPerLine(const Value: integer);
begin
  SetIntegerProperty(FMaxObsPerLine, Value);
end;

procedure TSutraOutputControl.SetNE_PrintFrequency(const Value: integer);
begin
  SetIntegerProperty(FNE_PrintFrequency, Value);
end;

procedure TSutraOutputControl.SetSoluteEnergySourcePrintFrequency
  (const Value: integer);
begin
  SetIntegerProperty(FSoluteEnergySourcePrintFrequency, Value);
end;

procedure TSutraOutputControl.SetSpecifiedConcTempPrintFrequency
  (const Value: integer);
begin
  SetIntegerProperty(FSpecifiedConcTempPrintFrequency, Value);
end;

procedure TSutraOutputControl.SetSpecifiedPressurePrintFrequency
  (const Value: integer);
begin
  SetIntegerProperty(FSpecifiedPressurePrintFrequency, Value);
end;

end.
