unit SutraGeneralFlowNodesUnit;

interface

uses
  SutraGeneralBoundaryUnit, GoPhastTypes;

type
   TValueAndAnnotation = record
     Value: Double;
     Annotation: string;
   end;

  TGeneralFlowNode = record
    // IPBG1
    NodeNumber: Integer;
    Active: boolean;
    // PBG11
    P1: TValueAndAnnotation;
    // PBG21
    P2: TValueAndAnnotation;
    // QPBG11
    Q1: TValueAndAnnotation;
    // QPBG21
    Q2: TValueAndAnnotation;
    // UPBGI1
    U1: TValueAndAnnotation;
    // UPBGO1
    U2: TValueAndAnnotation;
    // CPQL11
    Limit1: TSutraLimitType;
    // CPQL21
    Limit2: TSutraLimitType;
    // CUPBGO1
    ExitSpecification: TSutraExitSpecificationMethod;
    FLayer: Integer;
    FCol: Integer;
    FUseBCTime: boolean;
    class function Create(NN: Integer; Pressure1, Pressure2,
      Flow1, Flow2, TempOrConc1, TempOrConc2: TValueAndAnnotation;
      Lim1, Lim2: TSutraLimitType;
      ExitSpec: TSutraExitSpecificationMethod;
      Layer, Col: integer; UseBCTime: boolean): TGeneralFlowNode; static;
    class function CreateInactive(NN: Integer;
      Layer, Col: integer): TGeneralFlowNode; static;
  end;

  IGeneralFlowNodes = interface(IUnknown) ['{CEBD20D1-BE89-4374-ACA1-F45853A01A73}']
    procedure Add(Node: TGeneralFlowNode);
    procedure Clear;
    function ToArray: TArray<TGeneralFlowNode>;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetTimeIndex: Integer;
    property TimeIndex: Integer read GetTimeIndex;
  end;

  TGeneralTransportNode = record
    // IUBG1
    NodeNumber: Integer;
    Active: boolean;
    // UBG11
    FUValue1: TValueAndAnnotation;
    // QUBG11
    FSoluteEnergyInflow: TValueAndAnnotation;
    // UBG21
    FUValue2: TValueAndAnnotation;
    // QUBG21
    FSoluteEnergyOutflow: TValueAndAnnotation;
    FLayer: Integer;
    FCol: Integer;
    FUseBCTime: boolean;
    class function Create(NN: Integer; UValue1, SoluteEnergyInflow,
      UValue2, SoluteEnergyOutflow: TValueAndAnnotation;
      Layer, Col: integer; UseBCTime: boolean): TGeneralTransportNode; static;
    class function CreateInactive(NN: Integer;
      Layer, Col: integer): TGeneralTransportNode; static;
  end;

  IGeneralTransportNodes = interface(IUnknown) ['{8BFC0F0E-4BE9-4F5A-8E3A-511D5A720478}']
    procedure Add(Node: TGeneralTransportNode);
    procedure Clear;
    function ToArray: TArray<TGeneralTransportNode>;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function GetTimeIndex: Integer;
    property TimeIndex: Integer read GetTimeIndex;
  end;


implementation

{ TGeneralFlowNode }

class function TGeneralFlowNode.Create(NN: Integer; Pressure1,
  Pressure2, Flow1, Flow2, TempOrConc1, TempOrConc2: TValueAndAnnotation; Lim1,
  Lim2: TSutraLimitType;
  ExitSpec: TSutraExitSpecificationMethod;
  Layer, Col: integer; UseBCTime: boolean): TGeneralFlowNode;
begin
  Result.NodeNumber := NN;
  Result.Active := True;
  Result.P1 := Pressure1;
  Result.P2 := Pressure2;
  Result.Q1 := Flow1;
  Result.Q2 := Flow2;
  Result.U1 := TempOrConc1;
  Result.U2 := TempOrConc2;
  Result.Limit1 := Lim1;
  Result.Limit2 := Lim2;
  Result.ExitSpecification := ExitSpec;
  Result.FLayer := Layer;
  Result.FCol := Col;
  Result.FUseBCTime := UseBCTime;
end;

class function TGeneralFlowNode.CreateInactive(NN: Integer;
  Layer, Col: integer): TGeneralFlowNode;
begin
  Result.NodeNumber := NN;
  Result.Active := False;
  Result.FLayer := Layer;
  Result.FCol := Col;
end;

{ TGeneralTransportNode }

class function TGeneralTransportNode.Create(NN: Integer; UValue1,
  SoluteEnergyInflow, UValue2,
  SoluteEnergyOutflow: TValueAndAnnotation; Layer, Col: integer; UseBCTime: boolean): TGeneralTransportNode;
begin
  Result.NodeNumber := NN;
  Result.Active := True;
  Result.FUValue1 := UValue1;
  Result.FSoluteEnergyInflow := SoluteEnergyInflow;
  Result.FUValue2 := UValue2;
  Result.FSoluteEnergyOutflow := SoluteEnergyOutflow;
  Result.FLayer := Layer;
  Result.FCol := Col;
  Result.FUseBCTime := UseBCTime;
end;

class function TGeneralTransportNode.CreateInactive(
  NN: Integer; Layer, Col: integer): TGeneralTransportNode;
begin
  Result.NodeNumber := NN;
  Result.Active := False;
  Result.FLayer := Layer;
  Result.FCol := Col;
end;

end.
