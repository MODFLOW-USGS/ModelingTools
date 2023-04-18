unit ModflowCfpPipeUnit;

interface

uses Classes, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SysUtils;

type
  // @name controls data set 25 in CFP
  TCfpPipeBoundary = class(TModflowSteadyBoundary)
  private
    FDiameter: IFormulaObject;
    FTortuosity: IFormulaObject;
    FRoughnessHeight: IFormulaObject;
    FLowerCriticalR: IFormulaObject;
    FHigherCriticalR: IFormulaObject;
    FConductancePermeability: IFormulaObject;
    FElevation: IFormulaObject;
    FConductancePermeabilityObserver: TObserver;
    FDiameterObserver: TObserver;
    FHigherCriticalRObserver: TObserver;
    FLowerCriticalRObserver: TObserver;
    FRoughnessHeightObserver: TObserver;
    FTortuosityObserver: TObserver;
    FElevationObserver: TObserver;
    FRecordPipeValues: boolean;
    FRecordNodeValues: boolean;
    function GetConductancePermeability: string;
    function GetDiameter: string;
    function GetHigherCriticalR: string;
    function GetLowerCriticalR: string;
    function GetRoughnessHeight: string;
    function GetTortuosity: string;
    procedure SetConductancePermeability(const Value: string);
    procedure SetDiameter(const Value: string);
    procedure SetHigherCriticalR(const Value: string);
    procedure SetLowerCriticalR(const Value: string);
    procedure SetRoughnessHeight(const Value: string);
    procedure SetTortuosity(const Value: string);
    function GetConductancePermeabilityObserver: TObserver;
    function GetDiameterObserver: TObserver;
    function GetHigherCriticalRObserver: TObserver;
    function GetLowerCriticalRObserver: TObserver;
    function GetRoughnessHeightObserver: TObserver;
    function GetTortuosityObserver: TObserver;
    function GetElevation: string;
    procedure SetElevation(const Value: string);
    function GetElevationObserver: TObserver;
    procedure SetRecordNodeValues(const Value: boolean);
    procedure SetRecordPipeValues(const Value: boolean);
    function GetBoundaryFormula(Index: Integer): string;
    procedure SetBoundaryFormula(Index: Integer; const Value: string);
  protected
    procedure HandleChangedValue(Observer: TObserver); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    function GetUsedObserver: TObserver; override;
    procedure CreateFormulaObjects; override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; override;
    property DiameterObserver: TObserver read GetDiameterObserver;
    property TortuosityObserver: TObserver read GetTortuosityObserver;
    property RoughnessHeightObserver: TObserver read GetRoughnessHeightObserver;
    property LowerCriticalRObserver: TObserver read GetLowerCriticalRObserver;
    property HigherCriticalRObserver: TObserver read GetHigherCriticalRObserver;
    property ConductancePermeabilityObserver: TObserver
      read GetConductancePermeabilityObserver;
    property ElevationObserver: TObserver read GetElevationObserver;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    property BoundaryFormula[Index: Integer]: string read GetBoundaryFormula
      write SetBoundaryFormula;
  published
    // data set 25 DIAMETER
    property Diameter: string read GetDiameter write SetDiameter;
    // data set 25 TORTUOSITY
    property Tortuosity: string read GetTortuosity write SetTortuosity;
    // data set 25 RHEIGHT
    property RoughnessHeight: string read GetRoughnessHeight
      write SetRoughnessHeight;
    // data set 25 LCRITREY_P
    property LowerCriticalR: string read GetLowerCriticalR
      write SetLowerCriticalR;
    // data set 25 TCRITREY_P
    property HigherCriticalR: string read GetHigherCriticalR
      write SetHigherCriticalR;
    // data set 29 K_EXCHANGE
    property ConductancePermeability: string read GetConductancePermeability
      write SetConductancePermeability;
    // data set 12 ELEVATION
    property Elevation: string read GetElevation write SetElevation;
    property RecordNodeValues: boolean read FRecordNodeValues
      write SetRecordNodeValues;
    property RecordPipeValues: boolean read FRecordPipeValues
      write SetRecordPipeValues;
  end;

//procedure RemoveHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);
//procedure RestoreHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);

implementation

uses
  PhastModelUnit, DataSetUnit, DataSetNamesUnit;

const
  DiameterPosition = 0;
  TortuosityPosition = 1;
  RoughnessHeightPosition = 2;
  LowerCriticalRPosition = 3;
  HigherCriticalRPosition = 4;
  ConductancePermeabilityPosition = 5;
  ElevationPosition = 6;

//procedure RemoveHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);
//begin
//  (Subject as TCfpPipeBoundary).RemoveSubscription(Sender, AName);
//end;
//
//procedure RestoreHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);
//begin
//  (Subject as TCfpPipeBoundary).RestoreSubscription(Sender, AName);
//end;


{ TCfpPipeBoundary }

procedure TCfpPipeBoundary.Assign(Source: TPersistent);
var
  SourceCfp: TCfpPipeBoundary;
begin
  if Source is TCfpPipeBoundary then
  begin
    SourceCfp := TCfpPipeBoundary(Source);
    Diameter := SourceCfp.Diameter;
    Tortuosity := SourceCfp.Tortuosity;
    RoughnessHeight := SourceCfp.RoughnessHeight;
    LowerCriticalR := SourceCfp.LowerCriticalR;
    HigherCriticalR := SourceCfp.HigherCriticalR;
    ConductancePermeability := SourceCfp.ConductancePermeability;
    Elevation := SourceCfp.Elevation;
    RecordNodeValues := SourceCfp.RecordNodeValues;
    RecordPipeValues := SourceCfp.RecordPipeValues;
  end;
  inherited;
end;

function TCfpPipeBoundary.BoundaryObserverPrefix: string;
begin
  result := 'CfpPipeBoundary_';
end;

constructor TCfpPipeBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

  Diameter := '0.1';
  Tortuosity := '1';
  RoughnessHeight := '0.01';
  LowerCriticalR := '10';
  HigherCriticalR := '20';
  ConductancePermeability := '5';
  Elevation := '0';
end;

procedure TCfpPipeBoundary.CreateFormulaObjects;
begin
  FDiameter := CreateFormulaObjectBlocks(dso3D);
  FTortuosity := CreateFormulaObjectBlocks(dso3D);
  FRoughnessHeight := CreateFormulaObjectBlocks(dso3D);
  FLowerCriticalR := CreateFormulaObjectBlocks(dso3D);
  FHigherCriticalR := CreateFormulaObjectBlocks(dso3D);
  FConductancePermeability := CreateFormulaObjectBlocks(dso3D);
  FElevation := CreateFormulaObjectBlocks(dso3D);
end;

procedure TCfpPipeBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(DiameterObserver);
    FObserverList.Add(TortuosityObserver);
    FObserverList.Add(RoughnessHeightObserver);
    FObserverList.Add(LowerCriticalRObserver);
    FObserverList.Add(HigherCriticalRObserver);
    FObserverList.Add(ConductancePermeabilityObserver);
    FObserverList.Add(ElevationObserver);
  end;

end;

destructor TCfpPipeBoundary.Destroy;
begin
  Diameter := '0';
  Tortuosity := '0';
  RoughnessHeight := '0';
  LowerCriticalR := '0';
  HigherCriticalR := '0';
  ConductancePermeability := '0';
  Elevation := '0';
  inherited;
end;

function TCfpPipeBoundary.GetBoundaryFormula(Index: Integer): string;
begin
  case Index of
    DiameterPosition: result := Diameter;
    TortuosityPosition: result := Tortuosity;
    RoughnessHeightPosition: result := RoughnessHeight;
    LowerCriticalRPosition: result := LowerCriticalR;
    HigherCriticalRPosition: result := HigherCriticalR;
    ConductancePermeabilityPosition: result := ConductancePermeability;
    ElevationPosition: result := Elevation;
    else Assert(False);
  end;
end;

function TCfpPipeBoundary.GetConductancePermeability: string;
begin
  Result := FConductancePermeability.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ConductancePermeabilityPosition);
  end;
end;

function TCfpPipeBoundary.GetConductancePermeabilityObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FConductancePermeabilityObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KPipeConductanceOrPer);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_ConductancePerm_', FConductancePermeabilityObserver, DataArray);
  end;
  result := FConductancePermeabilityObserver;
end;

function TCfpPipeBoundary.GetDiameter: string;
begin
  Result := FDiameter.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DiameterPosition);
  end;
end;

function TCfpPipeBoundary.GetDiameterObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FDiameterObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KPipeDiameter);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_Diameter_', FDiameterObserver, DataArray);
  end;
  result := FDiameterObserver;
end;

function TCfpPipeBoundary.GetElevation: string;
begin
  Result := FElevation.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ElevationPosition);
  end;
end;

function TCfpPipeBoundary.GetElevationObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FElevationObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpNodeElevation);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_Elevation_', FElevationObserver, DataArray);
  end;
  result := FElevationObserver;

end;

function TCfpPipeBoundary.GetHigherCriticalR: string;
begin
  Result := FHigherCriticalR.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(HigherCriticalRPosition);
  end;
end;

function TCfpPipeBoundary.GetHigherCriticalRObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FHigherCriticalRObserver = nil then
  begin
    if ParentModel <>  nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KUpperCriticalR);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_HigherCriticalR_', FHigherCriticalRObserver, DataArray);
  end;
  result := FHigherCriticalRObserver;
end;

function TCfpPipeBoundary.GetLowerCriticalR: string;
begin
  Result := FLowerCriticalR.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(LowerCriticalRPosition);
  end;
end;

function TCfpPipeBoundary.GetLowerCriticalRObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FLowerCriticalRObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KLowerCriticalR);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_LowerCriticalR_', FLowerCriticalRObserver, DataArray);
  end;
  result := FLowerCriticalRObserver;
end;

procedure TCfpPipeBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FDiameter as TObject then
  begin
    List.Add(FObserverList[DiameterPosition]);
  end;
  if Sender = FTortuosity as TObject then
  begin
    List.Add(FObserverList[TortuosityPosition]);
  end;
  if Sender = FRoughnessHeight as TObject then
  begin
    List.Add(FObserverList[RoughnessHeightPosition]);
  end;
  if Sender = FLowerCriticalR as TObject then
  begin
    List.Add(FObserverList[LowerCriticalRPosition]);
  end;
  if Sender = FHigherCriticalR as TObject then
  begin
    List.Add(FObserverList[HigherCriticalRPosition]);
  end;
  if Sender = FConductancePermeability as TObject then
  begin
    List.Add(FObserverList[ConductancePermeabilityPosition]);
  end;
  if Sender = FElevation as TObject then
  begin
    List.Add(FObserverList[ElevationPosition]);
  end;
end;

function TCfpPipeBoundary.GetRoughnessHeight: string;
begin
  Result := FRoughnessHeight.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RoughnessHeightPosition);
  end;

end;

function TCfpPipeBoundary.GetRoughnessHeightObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FRoughnessHeightObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KRoughnessHeight);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_RoughnessHeight_', FRoughnessHeightObserver, DataArray);
  end;
  result := FRoughnessHeightObserver;
end;

function TCfpPipeBoundary.GetTortuosity: string;
begin
  Result := FTortuosity.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(TortuosityPosition);
  end;

end;

function TCfpPipeBoundary.GetTortuosityObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FTortuosityObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KTortuosity);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_Tortuosity_', FTortuosityObserver, DataArray);
  end;
  result := FTortuosityObserver;
end;

function TCfpPipeBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('CFP_Pipe_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TCfpPipeBoundary.HandleChangedValue(Observer: TObserver);
begin
  // invalidate display here.
  { TODO -cCFP : Does this need to be finished?}
end;

procedure TCfpPipeBoundary.SetBoundaryFormula(Index: Integer;
  const Value: string);
begin
  case Index of
    DiameterPosition:
      Diameter := Value;
    TortuosityPosition:
      Tortuosity := Value;
    RoughnessHeightPosition:
      RoughnessHeight := Value;
    LowerCriticalRPosition:
      LowerCriticalR := Value;
    HigherCriticalRPosition:
      HigherCriticalR := Value;
    ConductancePermeabilityPosition:
      ConductancePermeability := Value;
    ElevationPosition:
      Elevation := Value;
    else Assert(False);
  end;
end;

procedure TCfpPipeBoundary.SetConductancePermeability(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePermeabilityPosition, FConductancePermeability);
end;

procedure TCfpPipeBoundary.SetDiameter(const Value: string);
begin
  UpdateFormulaBlocks(Value, DiameterPosition, FDiameter);
end;

procedure TCfpPipeBoundary.SetElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, ElevationPosition, FElevation);
end;

procedure TCfpPipeBoundary.SetHigherCriticalR(const Value: string);
begin
  UpdateFormulaBlocks(Value, HigherCriticalRPosition, FHigherCriticalR);
end;

procedure TCfpPipeBoundary.SetLowerCriticalR(const Value: string);
begin
  UpdateFormulaBlocks(Value, LowerCriticalRPosition, FLowerCriticalR);
end;

procedure TCfpPipeBoundary.SetRecordNodeValues(const Value: boolean);
begin
  if FRecordNodeValues <> Value then
  begin
   FRecordNodeValues := Value;
   InvalidateModel;
  end;
end;

procedure TCfpPipeBoundary.SetRecordPipeValues(const Value: boolean);
begin
  if FRecordPipeValues <> Value then
  begin
   FRecordPipeValues := Value;
   InvalidateModel;
  end;
end;

procedure TCfpPipeBoundary.SetRoughnessHeight(const Value: string);
begin
  UpdateFormulaBlocks(Value, RoughnessHeightPosition, FRoughnessHeight);
end;

procedure TCfpPipeBoundary.SetTortuosity(const Value: string);
begin
  UpdateFormulaBlocks(Value, TortuosityPosition, FTortuosity);
end;





//procedure TCfpPipeBoundary.ModelChanged(Sender: TObject);
//begin
//  InvalidateModel;
//end;

end.
