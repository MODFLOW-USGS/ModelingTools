unit SutraGeneralFlowWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, System.SysUtils, CustomModflowWriterUnit,
  System.Generics.Collections, SutraGeneralFlowNodesUnit,
  System.Generics.Defaults, SutraBoundaryUnit, PhastModelUnit,
  SutraGeneralBoundaryUnit, RealListUnit, IntListUnit, GoPhastTypes,
  Vcl.Dialogs, System.Classes, SutraBoundaryWriterUnit, SutraOptionsUnit,
  SparseDataSets;

type
  TGeneralFlowNodes = class(TList<TGeneralFlowNode>, IGeneralFlowNodes)
  private
    FRefCount: Integer;
    FTimeIndex: Integer;
    function GetCount: Integer;
    function GetTimeIndex: Integer;
    procedure SetTimeIndex(const Value: Integer);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure Add(Node: TGeneralFlowNode);
    property Count: Integer read GetCount;
    function ToArray: TArray<TGeneralFlowNode>; reintroduce;
    property TimeIndex: Integer read GetTimeIndex write SetTimeIndex;
  end;

  TGenFlowInteractionStringList = class(TLakeInteractionStringList)
  private
    FFlowInteraction: TGeneralizedFlowInteractionType;
    procedure SetFlowInteraction(const Value: TGeneralizedFlowInteractionType);
  public
    property FlowInteraction: TGeneralizedFlowInteractionType
      read FFlowInteraction write SetFlowInteraction;
  end;

  TSutraGeneralFlowWriter = class(TCustomFileWriter)
  private
//    FNameOfFile: string;
    FPressure1TimeLists: TObjectList<TSutraTimeList>;
    FPressure2TimeLists: TObjectList<TSutraTimeList>;
    FFlow1TimeLists: TObjectList<TSutraTimeList>;
    FFlow2TimeLists: TObjectList<TSutraTimeList>;
    FU1TimeLists: TObjectList<TSutraTimeList>;
    FU2TimeLists: TObjectList<TSutraTimeList>;
    FLimit1Lists: TObjectList<TSutraLimitTypeList>;
    FLimit2Lists: TObjectList<TSutraLimitTypeList>;
    FGeneralBoundaries: TList<IGeneralFlowNodes>;
    FExitSpecificationLists: TObjectList<TSutraExitSpecificationMethodList>;
    FTimes: TRealList;
    FBcsFileNames: TGenFlowInteractionStringList;
    FUseBctime: T3DSparseBooleanArray;
    FBcopgFileName: string;
//    FNodeNumbers: T3DSparseIntegerArray;
    procedure Evaluate;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2(TimeIndex: integer);
    procedure WriteDataSet7A(TimeIndex: integer);
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
  public
    constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(AFileName: string;
      GeneralBoundaries: TList<IGeneralFlowNodes>;
      BcsFileNames: TGenFlowInteractionStringList);
    procedure UpdateDisplay(GeneralBoundaries: TList<IGeneralFlowNodes>);
    property BcopgFileName: string read FBcopgFileName;
  end;

const
  KGeneralizedFlow = 'GenFlow';

implementation

uses
  frmErrorsAndWarningsUnit, ScreenObjectUnit,
  SutraTimeScheduleUnit, frmGoPhastUnit, DataSetUnit,
  SutraMeshUnit, SutraFileWriterUnit, SparseArrayUnit,
  ModflowCellUnit, CellLocationUnit;

resourcestring
  StrGeneralizedflowBou = 'generalized-flow boundary condition';

{ TGeneralFlowNodes }

procedure TGeneralFlowNodes.Add(Node: TGeneralFlowNode);
begin
  inherited Add(Node);
end;

function TGeneralFlowNodes.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TGeneralFlowNodes.GetTimeIndex: Integer;
begin
  result := FTimeIndex;
end;

function TGeneralFlowNodes.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TGeneralFlowNodes.SetTimeIndex(const Value: Integer);
begin
  FTimeIndex := Value;
end;

function TGeneralFlowNodes.ToArray: TArray<TGeneralFlowNode>;
type
  TNodeComparer = TComparer<TGeneralFlowNode>;
var
  Comparer: IComparer<TGeneralFlowNode>;
begin
  result := inherited;
  // sort the nodes in ascending order.
  Comparer := TNodeComparer.Construct(
    function(const L, R: TGeneralFlowNode): Integer
    begin
      result := L.NodeNumber - R.NodeNumber;
    end
    );
  TArray.Sort<TGeneralFlowNode>(Result, Comparer );
end;

function TGeneralFlowNodes._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGeneralFlowNodes._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TSutraGeneralFlowWriter }

constructor TSutraGeneralFlowWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
//var
//  Mesh: TSutraMesh3D;
//  NumberOfLayers: Integer;
//  NumberOfRows: Integer;
//  NumberOfColumns: Integer;
begin
  inherited;
  FBcopgFileName :='';
  FPressure1TimeLists := TObjectList<TSutraTimeList>.Create;
  FPressure2TimeLists := TObjectList<TSutraTimeList>.Create;
  FFlow1TimeLists := TObjectList<TSutraTimeList>.Create;
  FFlow2TimeLists := TObjectList<TSutraTimeList>.Create;
  FU1TimeLists := TObjectList<TSutraTimeList>.Create;
  FU2TimeLists := TObjectList<TSutraTimeList>.Create;

  FLimit1Lists := TObjectList<TSutraLimitTypeList>.Create;
  FLimit2Lists := TObjectList<TSutraLimitTypeList>.Create;
  FExitSpecificationLists := TObjectList<TSutraExitSpecificationMethodList>.Create;

  FTimes := TRealList.Create;
  FUseBctime := T3DSparseBooleanArray.Create(GetQuantum(Model.LayerCount+1),
    GetQuantum(Model.RowCount+1), GetQuantum(Model.ColumnCount+1));

//  Mesh := Model.SutraMesh;
//  if Mesh <> nil then
//  begin
//    if ((Model.Mesh as TSutraMesh3D).MeshType = mt3D) then
//    begin
//      NumberOfLayers := frmGoPhast.PhastModel.
//        SutraLayerStructure.LayerCount+1;
//    end
//    else
//    begin
//      NumberOfLayers := frmGoPhast.PhastModel.
//        SutraLayerStructure.LayerCount;
//    end;
//    NumberOfRows := 1;
//    NumberOfColumns := Mesh.Mesh2D.Nodes.Count;
//  end
//  else
//  begin
//    NumberOfLayers := 0;
//    NumberOfRows := 0;
//    NumberOfColumns := 0;
//  end;
//  FNodeNumbers := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
//    GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
end;

destructor TSutraGeneralFlowWriter.Destroy;
begin
//  FNodeNumbers.Free;
  FUseBctime.Free;
  FTimes.Free;

  FExitSpecificationLists.Free;
  FLimit2Lists.Free;
  FLimit1Lists.Free;

  FU2TimeLists.Free;
  FU1TimeLists.Free;
  FFlow2TimeLists.Free;
  FFlow1TimeLists.Free;
  FPressure2TimeLists.Free;
  FPressure1TimeLists.Free;
//  FNodeNumbers.Free;
  inherited;
end;

procedure TSutraGeneralFlowWriter.Evaluate;
var
  SimulationType: TSimulationType;
  TransientAllowed: Boolean;
  RootError: string;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraGeneralFlowBoundary;
  DisplayTimeIndex: Integer;
  TIndex: Integer;
  BoundaryValues: TSutraBoundaryValueArray;
  DisplayTime: Double;
  Item: TSutraGeneralFlowItem;
  SutraTimeOptions: TSutraTimeOptions;
  AllTimes: TRealList;
  LowerLimitList: TSutraLimitTypeList;
  AnItem: TSutraGeneralFlowItem;
  TimeIndex: Integer;
  UpperLimitList: TSutraLimitTypeList;
  ExitSpecList: TSutraExitSpecificationMethodList;
  ListIndex: Integer;
  SutraTimeList: TSutraTimeList;
  LastUsed: TIntegerList;
  ATime: Double;
  P1SutraTimeList: TSutraTimeList;
  UsedIndex: Integer;
  P1Data: TDataArray;
  P2SutraTimeList: TSutraTimeList;
  P2Data: TDataArray;
  Flow1SutraTimeList: TSutraTimeList;
  Flow1Data: TDataArray;
  Flow2SutraTimeList: TSutraTimeList;
  Flow2Data: TDataArray;
  U1SutraTimeList: TSutraTimeList;
  U1Data: TDataArray;
  U2SutraTimeList: TSutraTimeList;
  U2Data: TDataArray;
  LayerIndex: Integer;
  Mesh: TSutraMesh3D;
  ColIndex: integer;
  LayerLimit: Integer;
  NodeNumber: Integer;
  P1: TValueAndAnnotation;
  P2: TValueAndAnnotation;
  Q1: TValueAndAnnotation;
  Q2: TValueAndAnnotation;
  U1: TValueAndAnnotation;
  U2: TValueAndAnnotation;
  LowerLimitType: TSutraLimitType;
  UpperLimitType: TSutraLimitType;
  ExitSpecMethod: TSutraExitSpecificationMethod;
  NodeList: IGeneralFlowNodes;
  InnerIndex: integer;
  FlowNodes: TGeneralFlowNodes;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  UseBCTime: Boolean;
//  PestNames: TStringList;
  LowerPressurePestNames: TStringList;
  LowerFlowRatePestNames: TStringList;
  HigherPressurePestNames: TStringList;
  HigherFlowRatePestNames: TStringList;
  UInPestNames: TStringList;
  UOutPestNames: TStringList;
//  SeriesPestName: TStringList;
  LowerPressurePestNamesList: TStringListObjectList;
  LowerFlowRatePestNamesList: TStringListObjectList;
  HigherPressurePestNamesList: TStringListObjectList;
  UInPestNamesList: TStringListObjectList;
  UOutPestNamesList: TStringListObjectList;
  LowerPressureSeriesPestNames: TStringList;
  LowerFlowRateSeriesPestNames: TStringList;
  HigherPressureSeriesPestNames: TStringList;
  HigherFlowRateSeriesPestNames: TStringList;
  UInSeriesPestNames: TStringList;
  UOutSeriesPestNames: TStringList;
  LowerPressureSeriesPestMethods: TPestMethodList;
  LowerFlowRateSeriesPestMethods: TPestMethodList;
  HigherPressureSeriesPestMethods: TPestMethodList;
  HigherFlowRateSeriesPestMethods: TPestMethodList;
  UInSeriesPestMethods: TPestMethodList;
  UOutSeriesPestMethods: TPestMethodList;
  P1SeriesName: string;
  P1SeriesMethod: TPestParamMethod;
  P1Name: string;
  P2SeriesName: string;
  P2SeriesMethod: TPestParamMethod;
  P2Name: string;
  Flow1SeriesName: string;
  Flow1SeriesMethod: TPestParamMethod;
  Flow1Name: string;
  Flow2SeriesName: string;
  Flow2SeriesMethod: TPestParamMethod;
  Flow2Name: string;
  U1SeriesName: string;
  U1SeriesMethod: TPestParamMethod;
  U1Name: string;
  U2SeriesName: string;
  U2SeriesMethod: TPestParamMethod;
  U2Name: string;
  CellLocation: TCellLocation;
  CellLocPointer: PCellLocation;
  HigherFlowRatePestNamesList: TStringListObjectList;
  BoundaryActiveData: TDataArray;
  ModifiedValue: Double;
  procedure InitializeTimeList(ListOfTimeLists: TObjectList<TSutraTimeList>;
    FormulaIndex: Integer; Descripion: string; PestNames: TStringList);
  var
    TimeList: TSutraTimeList;
    AnItem: TSutraGeneralFlowItem;
    TimeIndex: Integer;
    Formula: string;
  begin
    TimeList := TSutraTimeList.Create(Model, ScreenObject);
    ListOfTimeLists.Add(TimeList);
    if FEvaluationType = etDisplay then
    begin
      AnItem := ABoundary.Values[DisplayTimeIndex]
        as TSutraGeneralFlowItem;
      BoundaryValues[0].Time := AnItem.StartTime;
      Formula := AnItem.BoundaryFormula[FormulaIndex];
      AssignPestFormula(Formula, ABoundary.PestBoundaryFormula[FormulaIndex],
        ABoundary.PestBoundaryMethod[FormulaIndex], PestNames);
      BoundaryValues[0].Formula := Formula;
      BoundaryValues[0].UsedFormula := AnItem.UsedFormula;
    end
    else
    begin
      for TimeIndex := 0 to ABoundary.Values.Count - 1 do
      begin
        AnItem := ABoundary.Values[TimeIndex]
          as TSutraGeneralFlowItem;
        BoundaryValues[TimeIndex].Time := FixTime(AnItem, AllTimes);
        Formula := AnItem.BoundaryFormula[FormulaIndex];
        AssignPestFormula(Formula, ABoundary.PestBoundaryFormula[FormulaIndex],
          ABoundary.PestBoundaryMethod[FormulaIndex], PestNames);
        BoundaryValues[TimeIndex].Formula := Formula;
        BoundaryValues[TimeIndex].UsedFormula := AnItem.UsedFormula;
      end;
    end;
    TimeList.Initialize(BoundaryValues);
  end;
  procedure AssignNodeNumber;
  var
    Node: TSutraNode3D;
  begin
    NodeNumber := -1;
    case Mesh.meshType of
      mt2D, mtProfile:
        begin
          NodeNumber := Mesh.Mesh2D.Nodes[ColIndex].Number;
        end;
      mt3D:
        begin
          Node := Mesh.NodeArray[LayerIndex,ColIndex];
          if Node.Active then
          begin
            NodeNumber := Node.Number;
          end;
        end;
      else Assert(False);
    end;
  end;
begin
  CellLocPointer := Addr(CellLocation);
  SimulationType := Model.SutraOptions.SimulationType;
  TransientAllowed := SimulationType = stTransientFlowTransientTransport;
  RootError := Format(StrTheFollowingObjectSutra, [StrGeneralizedflowBou]);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, RootError);

  SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
  SutraTimeOptions.CalculateAllTimes;
  AllTimes := SutraTimeOptions.AllTimes;

  DisplayTime := 0;
  if FEvaluationType = etDisplay then
  begin
    DisplayTime := Model.ThreeDDisplayTime;
    SetLength(BoundaryValues, 1);
  end;

  LowerPressurePestNamesList := TStringListObjectList.Create;
  LowerFlowRatePestNamesList := TStringListObjectList.Create;
  HigherPressurePestNamesList := TStringListObjectList.Create;
  HigherFlowRatePestNamesList := TStringListObjectList.Create;
  UInPestNamesList := TStringListObjectList.Create;
  UOutPestNamesList := TStringListObjectList.Create;
  LowerPressureSeriesPestNames := TStringList.Create;
  LowerFlowRateSeriesPestNames := TStringList.Create;
  HigherPressureSeriesPestNames := TStringList.Create;
  HigherFlowRateSeriesPestNames := TStringList.Create;
  UInSeriesPestNames := TStringList.Create;
  UOutSeriesPestNames := TStringList.Create;
  LowerPressureSeriesPestMethods := TPestMethodList.Create;
  LowerFlowRateSeriesPestMethods := TPestMethodList.Create;
  HigherPressureSeriesPestMethods := TPestMethodList.Create;
  HigherFlowRateSeriesPestMethods := TPestMethodList.Create;
  UInSeriesPestMethods := TPestMethodList.Create;
  UOutSeriesPestMethods := TPestMethodList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.SutraBoundaries.GeneralFlowBoundary;
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        if (FBcsFileNames <> nil)
          and ((FBcsFileNames.LakeInteraction <> ABoundary.LakeInteraction)
          or (FBcsFileNames.FlowInteraction <> ABoundary.LakeInteractionType))
          then
        begin
          Continue;
        end;

        if not TransientAllowed and (ABoundary.Values.Count > 1) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, RootError, ScreenObject.Name,
            ScreenObject);
        end;
        DisplayTimeIndex := 0;
        if FEvaluationType = etDisplay then
        begin
          for TIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            Item := ABoundary.Values[TIndex] as TSutraGeneralFlowItem;
            if Item.StartTime <= DisplayTime then
            begin
              DisplayTimeIndex := TIndex
            end
            else
            begin
              break;
            end;
          end;
        end
        else
        begin
          SetLength(BoundaryValues, ABoundary.Values.Count);
        end;

        LowerPressurePestNames := TStringList.Create;
        LowerPressurePestNamesList.Add(LowerPressurePestNames);
        LowerFlowRatePestNames := TStringList.Create;
        LowerFlowRatePestNamesList.Add(LowerFlowRatePestNames);
        HigherPressurePestNames := TStringList.Create;
        HigherPressurePestNamesList.Add(HigherPressurePestNames);
        HigherFlowRatePestNames := TStringList.Create;
        HigherFlowRatePestNamesList.Add(HigherFlowRatePestNames);
        UInPestNames := TStringList.Create;
        UInPestNamesList.Add(UInPestNames);
        UOutPestNames := TStringList.Create;
        UOutPestNamesList.Add(UOutPestNames);

        LowerPressureSeriesPestNames.Add(ABoundary.PestBoundaryFormula[LowerPressurePosition]);
        LowerFlowRateSeriesPestNames.Add(ABoundary.PestBoundaryFormula[LowerFlowRatePosition]);
        HigherPressureSeriesPestNames.Add(ABoundary.PestBoundaryFormula[HigherPressurePosition]);
        HigherFlowRateSeriesPestNames.Add(ABoundary.PestBoundaryFormula[HigherFlowRatePosition]);
        UInSeriesPestNames.Add(ABoundary.PestBoundaryFormula[UInPosition]);
        UOutSeriesPestNames.Add(ABoundary.PestBoundaryFormula[UOutPosition]);

        LowerPressureSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[LowerPressurePosition]);
        LowerFlowRateSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[LowerFlowRatePosition]);
        HigherPressureSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[HigherPressurePosition]);
        HigherFlowRateSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[HigherFlowRatePosition]);
        UInSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[UInPosition]);
        UOutSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[UOutPosition]);

        InitializeTimeList(FPressure1TimeLists, LowerPressurePosition,
          'Lower_Pressure', LowerPressurePestNames);
        InitializeTimeList(FPressure2TimeLists, HigherPressurePosition,
          'Higher_Pressure', HigherPressurePestNames);
        InitializeTimeList(FFlow1TimeLists, LowerFlowRatePosition,
          'Lower_Flow_Rate', LowerFlowRatePestNames);
        InitializeTimeList(FFlow2TimeLists, HigherFlowRatePosition,
          'Higher_Flow_Rate', HigherFlowRatePestNames);
        InitializeTimeList(FU1TimeLists, UInPosition,
          'Lower_Temp_or_Conc', UInPestNames);
        InitializeTimeList(FU2TimeLists, UoutPosition,
          'Higher_Temp_or_Conc', UOutPestNames);

        LowerLimitList := TSutraLimitTypeList.Create;
        FLimit1Lists.Add(LowerLimitList);
        if FEvaluationType = etDisplay then
        begin
          AnItem := ABoundary.Values[DisplayTimeIndex]
            as TSutraGeneralFlowItem;
          LowerLimitList.Add(AnItem.LowerLimitType)
        end
        else
        begin
          for TimeIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            AnItem := ABoundary.Values[TimeIndex]
              as TSutraGeneralFlowItem;
            LowerLimitList.Add(AnItem.LowerLimitType)
          end;
        end;

        UpperLimitList := TSutraLimitTypeList.Create;
        FLimit2Lists.Add(UpperLimitList);
        if FEvaluationType = etDisplay then
        begin
          AnItem := ABoundary.Values[DisplayTimeIndex]
            as TSutraGeneralFlowItem;
          UpperLimitList.Add(AnItem.UpperLimitType)
        end
        else
        begin
          for TimeIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            AnItem := ABoundary.Values[TimeIndex]
              as TSutraGeneralFlowItem;
            UpperLimitList.Add(AnItem.UpperLimitType)
          end;
        end;

        ExitSpecList := TSutraExitSpecificationMethodList.Create;
        FExitSpecificationLists.Add(ExitSpecList);
        if FEvaluationType = etDisplay then
        begin
          AnItem := ABoundary.Values[DisplayTimeIndex]
            as TSutraGeneralFlowItem;
          ExitSpecList.Add(AnItem.ExitSpecMethod)
        end
        else
        begin
          for TimeIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            AnItem := ABoundary.Values[TimeIndex]
              as TSutraGeneralFlowItem;
            ExitSpecList.Add(AnItem.ExitSpecMethod)
          end;
        end;

        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count -1 do
          begin
            ACell := CellList[CellIndex];
            FUseBctime.Items[ACell.Layer, ACell.Row, ACell.Column] := ABoundary.UseBCTime;
//            FNodeNumbers[ACell.Layer, ACell.Row, ACell.Column] := 1;
          end;
        finally
          CellList.Free;
        end;

      end;
    end;

    for ListIndex := 0 to FPressure1TimeLists.Count - 1 do
    begin
      SutraTimeList := FPressure1TimeLists[ListIndex];
      for TimeIndex := 0 to SutraTimeList.Count - 1 do
      begin
        FTimes.AddUnique(SutraTimeList.Times[TimeIndex]);
      end;
    end;

    FGeneralBoundaries.Clear;
    for timeIndex := 0 to FTimes.Count - 1 do
    begin
      FlowNodes := TGeneralFlowNodes.Create;
      FlowNodes.TimeIndex := AllTimes.IndexOf(FTimes[timeIndex]);
      Assert(FlowNodes.TimeIndex >= 0);
      FGeneralBoundaries.Add(FlowNodes);
    end;

    Mesh := Model.Mesh as TSutraMesh3D;
    LastUsed := TIntegerList.Create;
    try
      for ListIndex := 0 to FPressure1TimeLists.Count - 1 do
      begin
        LastUsed.Add(0);
      end;

      for TimeIndex := 0 to FTimes.Count - 1 do
      begin
        ATime := FTimes[TimeIndex];
        NodeList := FGeneralBoundaries[TimeIndex];

        for ListIndex := 0 to FPressure1TimeLists.Count - 1 do
        begin
          UsedIndex := -1;
          P1SutraTimeList :=FPressure1TimeLists[ListIndex];
          for TIndex := LastUsed[ListIndex] to P1SutraTimeList.Count - 1 do
          begin
            UsedIndex := TIndex;
            if P1SutraTimeList.Times[TIndex] > ATime then
            begin
              Dec(UsedIndex);
              break;
            end;
          end;
          if UsedIndex >= 0 then
          begin
            LastUsed[ListIndex] :=  UsedIndex;
            // Get data sets for selected time
            P1Data := P1SutraTimeList[UsedIndex];
            BoundaryActiveData := P1SutraTimeList.UsedItems[UsedIndex];
            if P1Data = nil then
            begin
              // inactive
              for InnerIndex := 0 to P1SutraTimeList.Count - 1 do
              begin
                P1Data := P1SutraTimeList[InnerIndex];
                if P1Data <> nil then
                begin
                  break;
                end;
              end;
              if P1Data <> nil then
              begin
                if Mesh.meshType = mt3d then
                begin
                  LayerLimit := Mesh.LayerCount;
                end
                else
                begin
                  LayerLimit := 0
                end;
                for LayerIndex := 0 to LayerLimit do
                begin
                  for ColIndex := 0 to Mesh.Mesh2D.Nodes.Count - 1 do
                  begin
                    if P1Data.IsValue[LayerIndex,0,ColIndex] then
                    begin
                      AssignNodeNumber;
//                      if NodeNumber >= 0 then
                      begin
                        NodeList.Add(TGeneralFlowNode.CreateInactive(NodeNumber,
                          LayerIndex, ColIndex));
                      end;
                    end;
                  end;
                end;
              end;
              Continue;
            end;

            P1SeriesName := LowerPressureSeriesPestNames[ListIndex];
            P1SeriesMethod := LowerPressureSeriesPestMethods[ListIndex];
            P1Name := LowerPressurePestNamesList[ListIndex][UsedIndex];

            P2SeriesName := HigherPressureSeriesPestNames[ListIndex];
            P2SeriesMethod := HigherPressureSeriesPestMethods[ListIndex];
            P2Name := HigherPressurePestNamesList[ListIndex][UsedIndex];

            P2SutraTimeList := FPressure2TimeLists[ListIndex];
            P2Data := P2SutraTimeList[UsedIndex];
            Assert(P2Data <> nil);

            Flow1SeriesName := LowerFlowRateSeriesPestNames[ListIndex];
            Flow1SeriesMethod := LowerFlowRateSeriesPestMethods[ListIndex];
            Flow1Name := LowerFlowRatePestNamesList[ListIndex][UsedIndex];

            Flow1SutraTimeList := FFlow1TimeLists[ListIndex];
            Flow1Data := Flow1SutraTimeList[UsedIndex];
            Assert(Flow1Data <> nil);

            Flow2SeriesName := HigherFlowRateSeriesPestNames[ListIndex];
            Flow2SeriesMethod := HigherFlowRateSeriesPestMethods[ListIndex];
            Flow2Name := HigherFlowRatePestNamesList[ListIndex][UsedIndex];

            Flow2SutraTimeList := FFlow2TimeLists[ListIndex];
            Flow2Data := Flow2SutraTimeList[UsedIndex];
            Assert(Flow2Data <> nil);

            U1SeriesName := UInSeriesPestNames[ListIndex];
            U1SeriesMethod := UInSeriesPestMethods[ListIndex];
            U1Name := UInPestNamesList[ListIndex][UsedIndex];

            U1SutraTimeList := FU1TimeLists[ListIndex];
            U1Data := U1SutraTimeList[UsedIndex];
            Assert(U1Data <> nil);

            U2SeriesName := UOutSeriesPestNames[ListIndex];
            U2SeriesMethod := UOutSeriesPestMethods[ListIndex];
            U2Name := UOutPestNamesList[ListIndex][UsedIndex];

            U2SutraTimeList := FU2TimeLists[ListIndex];
            U2Data := U2SutraTimeList[UsedIndex];
            Assert(U2Data <> nil);

            LowerLimitList := FLimit1Lists[ListIndex];
            LowerLimitType := LowerLimitList[UsedIndex];

            UpperLimitList := FLimit2Lists[ListIndex];
            UpperLimitType := UpperLimitList[UsedIndex];

            ExitSpecList := FExitSpecificationLists[ListIndex];
            ExitSpecMethod := ExitSpecList[UsedIndex];

            if Mesh.meshType = mt3d then
            begin
              LayerLimit := Mesh.LayerCount;
            end
            else
            begin
              LayerLimit := 0
            end;
            for LayerIndex := 0 to LayerLimit do
            begin
              for ColIndex := 0 to Mesh.Mesh2D.Nodes.Count - 1 do
              begin
                if P1Data.IsValue[LayerIndex,0,ColIndex] then
                begin
                  CellLocation.Layer := LayerIndex;
                  CellLocation.Row := 0;
                  CellLocation.Column := ColIndex;

                  AssignNodeNumber;
                  if NodeNumber < 0 then
                  begin
                    Continue;
                  end;

                  if not BoundaryActiveData.BooleanData[LayerIndex,0,ColIndex] then
                  begin
                    NodeList.Add(TGeneralFlowNode.CreateInactive(
                      NodeNumber, LayerIndex, ColIndex));
                    Continue;
                  end;

                  P1.Value := P1Data.RealData[LayerIndex,0,ColIndex];
                  P1.Annotation := P1Data.Annotation[LayerIndex,0,ColIndex];
                  if (P1Name <> '') or (P1SeriesName <> '') then
                  begin
                    P1.Formula := GetPestTemplateFormula(P1.Value, P1Name,
                      P1SeriesName, P1SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(P1.Formula);
                  end
                  else
                  begin
                    P1.Formula := '';
                  end;

                  Assert(P2Data.IsValue[LayerIndex,0,ColIndex]);
                  P2.Value := P2Data.RealData[LayerIndex,0,ColIndex];
                  P2.Annotation := P2Data.Annotation[LayerIndex,0,ColIndex];
                  if (P2Name <> '') or (P2SeriesName <> '') then
                  begin
                    P2.Formula := GetPestTemplateFormula(P2.Value, P2Name,
                      P2SeriesName, P2SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(P2.Formula);
                  end
                  else
                  begin
                    P2.Formula := '';
                  end;

                  Assert(Flow1Data.IsValue[LayerIndex,0,ColIndex]);
                  Q1.Value := Flow1Data.RealData[LayerIndex,0,ColIndex];
                  Q1.Annotation := Flow1Data.Annotation[LayerIndex,0,ColIndex];
                  if (Flow1Name <> '') or (Flow1SeriesName <> '') then
                  begin
                    Q1.Formula := GetPestTemplateFormula(Q1.Value, Flow1Name,
                      Flow1SeriesName, Flow1SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(Q1.Formula);
                  end
                  else
                  begin
                    Q1.Formula := '';
                  end;

                  Assert(Flow2Data.IsValue[LayerIndex,0,ColIndex]);
                  Q2.Value := Flow2Data.RealData[LayerIndex,0,ColIndex];
                  Q2.Annotation := Flow2Data.Annotation[LayerIndex,0,ColIndex];
                  if (Flow2Name <> '') or (Flow2SeriesName <> '') then
                  begin
                    Q2.Formula := GetPestTemplateFormula(Q2.Value, Flow2Name,
                      Flow2SeriesName, Flow2SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(Q1.Formula);
                  end
                  else
                  begin
                    Q2.Formula := '';
                  end;

                  Assert(U1Data.IsValue[LayerIndex,0,ColIndex]);
                  U1.Value := U1Data.RealData[LayerIndex,0,ColIndex];
                  U1.Annotation := U1Data.Annotation[LayerIndex,0,ColIndex];
                  if (U1Name <> '') or (U1SeriesName <> '') then
                  begin
                    U1.Formula := GetPestTemplateFormula(U1.Value, U1Name,
                      U1SeriesName, U1SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(U1.Formula);
                  end
                  else
                  begin
                    U1.Formula := '';
                  end;

                  Assert(U2Data.IsValue[LayerIndex,0,ColIndex]);
                  U2.Value := U2Data.RealData[LayerIndex,0,ColIndex];
                  U2.Annotation := U2Data.Annotation[LayerIndex,0,ColIndex];
                  if (U2Name <> '') or (U2SeriesName <> '') then
                  begin
                    U2.Formula := GetPestTemplateFormula(U2.Value, U2Name,
                      U2SeriesName, U2SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(U2.Formula);
                  end
                  else
                  begin
                    U2.Formula := '';
                  end;

                  if FUseBCTime.IsValue[LayerIndex, 0,ColIndex] then
                  begin
                    UseBCTime := FUseBCTime.Items[LayerIndex, 0,ColIndex];
                  end
                  else
                  begin
                    UseBCTime := False;
                  end;

//                  if NodeNumber >= 0 then
                  begin
                    NodeList.Add(TGeneralFlowNode.Create(NodeNumber, P1, P2, Q1, Q2,
                      U1, U2, LowerLimitType, UpperLimitType, ExitSpecMethod,
                      LayerIndex, ColIndex, UseBCTime));
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      LastUsed.Free;
    end;
  finally
    LowerPressurePestNamesList.Free;
    LowerFlowRatePestNamesList.Free;
    HigherPressurePestNamesList.Free;
    HigherFlowRatePestNamesList.Free;
    UInPestNamesList.Free;
    UOutPestNamesList.Free;;
    LowerPressureSeriesPestNames.Free;
    LowerFlowRateSeriesPestNames.Free;
    HigherPressureSeriesPestNames.Free;
    HigherFlowRateSeriesPestNames.Free;
    UInSeriesPestNames.Free;
    UOutSeriesPestNames.Free;
    LowerPressureSeriesPestMethods.Free;
    LowerFlowRateSeriesPestMethods.Free;
    HigherPressureSeriesPestMethods.Free;
    HigherFlowRateSeriesPestMethods.Free;
    UInSeriesPestMethods.Free;
    UOutSeriesPestMethods.Free;
  end;

end;

class function TSutraGeneralFlowWriter.Extension: string;
begin
  result := '.GfbBCS';
end;

procedure TSutraGeneralFlowWriter.UpdateDisplay(
  GeneralBoundaries: TList<IGeneralFlowNodes>);
begin
  FGeneralBoundaries := GeneralBoundaries;

  try
    Evaluate;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TSutraGeneralFlowWriter.WriteFileInternal;
var
  TimeIndex: Integer;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;
    WriteDataSet0;
    WriteDataSet1;
    for TimeIndex := 0 to FTimes.Count - 1 do
    begin
      WriteDataSet2(TimeIndex);
      WriteDataSet7A(TimeIndex);
    end;
  finally
    CloseFile;
  end;
end;

procedure TSutraGeneralFlowWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('Generalized Flow'));
end;

procedure TSutraGeneralFlowWriter.WriteDataSet1;
var
  SimulationType: TSimulationType;
  BCSSCH: string;
begin
  WriteCommentLine('Data set 1');
  SimulationType := Model.SutraOptions.SimulationType;
  case SimulationType of
    stSteadyFlowSteadyTransport, stSteadyFlowTransientTransport:
      begin
        BCSSCH := 'STEP_0';
      end;
      stTransientFlowTransientTransport:
      begin
        // write schedule name here.
        BCSSCH := KGeneralizedFlow;
      end;
  else
    Assert(False);
  end;
  WriteString('''' + BCSSCH + '''');
  WriteString(' # BCSSCH');
  NewLine;
end;

procedure TSutraGeneralFlowWriter.WriteDataSet2(TimeIndex: integer);
var
  NodeList: IGeneralFlowNodes;
  BCSID: String;
  NSOP1: Integer;
  NSOU1: Integer;
  NPBC1: Integer;
  NUBC1: Integer;
  NPBG1: Integer;
  NUBG1: Integer;
begin
  WriteCommentLine('Data set 2; Time = ' + FloatToStr(FTimes[TimeIndex]));
  NodeList := FGeneralBoundaries[TimeIndex];
  BCSID := '''generalized flow boundaries''';
  NSOP1 := 0;
  NSOU1 := 0;
  NPBC1 := 0;
  NUBC1 := 0;
  NPBG1 := NodeList.Count;
  NUBG1 := 0;

  WriteString(BCSID);
  WriteInteger(NSOP1);
  WriteInteger(NSOU1);
  WriteInteger(NPBC1);
  WriteInteger(NUBC1);
  WriteInteger(NPBG1);
  WriteInteger(NUBG1);
  WriteString(' # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1, NPBG1, NUBG1');
  NewLine;
end;

procedure TSutraGeneralFlowWriter.WriteDataSet7A(TimeIndex: integer);
var
  NodeIndex: Integer;
  NodeArray: TArray<TGeneralFlowNode>;
  ANode: TGeneralFlowNode;
//  NodeList: IGeneralFlowNodes;
begin
  if FGeneralBoundaries[TimeIndex].Count > 0 then
  begin
    WriteCommentLine('Data set 7A');
    NodeArray := FGeneralBoundaries[TimeIndex].ToArray;
    for NodeIndex := 0 to Length(NodeArray) - 1 do
    begin
      ANode := NodeArray[NodeIndex];
      if ANode.Active then
      begin
        WriteInteger(ANode.NodeNumber+1);
        if WritingTemplate and (ANode.P1.Formula <> '') then
        begin
          WriteString(ANode.P1.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.P1.Value);
        end;
        if WritingTemplate and (ANode.Q1.Formula <> '') then
        begin
          WriteString(ANode.Q1.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.Q1.Value);
        end;
        if WritingTemplate and (ANode.P2.Formula <> '') then
        begin
          WriteString(ANode.P2.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.P2.Value);
        end;
        if WritingTemplate and (ANode.Q2.Formula <> '') then
        begin
          WriteString(ANode.Q2.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.Q2.Value);
        end;
        WriteLimit(ANode.Limit1);
        WriteLimit(ANode.Limit2);
        if WritingTemplate and (ANode.U1.Formula <> '') then
        begin
          WriteString(ANode.U1.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.U1.Value);
        end;
        WriteExitSpec(ANode.ExitSpecification);
        if WritingTemplate and (ANode.U2.Formula <> '') then
        begin
          WriteString(ANode.U2.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.U2.Value);
        end;
      end
      else
      begin
        WriteInteger(-(ANode.NodeNumber+1));
      end;
      NewLine;
    end;
    WriteString('0');
    NewLine;
  end;

end;

procedure TSutraGeneralFlowWriter.WriteFile(AFileName: string;
  GeneralBoundaries: TList<IGeneralFlowNodes>;
  BcsFileNames: TGenFlowInteractionStringList);
var
  LakeExtension: string;
  FlowTypeExtension: string;
  FileRoot: string;
begin
  FBcsFileNames := BcsFileNames;
  if (Model.ModelSelection = msSutra22) then
  begin
    if BcsFileNames <> nil then
    begin
      BcsFileNames.Add('');
    end;
    Exit;
  end;

  FGeneralBoundaries := GeneralBoundaries;

  Evaluate;

  if FPressure1TimeLists.count > 0 then
  begin
    if BcsFileNames <> nil then
    begin
      case BcsFileNames.LakeInteraction of
        lbiActivate:
          begin
            LakeExtension := '.ActivateLake';
          end;
        lbiNoChange:
          begin
            LakeExtension := '.NoChangeLake';
          end;
        lbiInactivate:
          begin
            LakeExtension := '.InactivateLake';
          end;
         lbiUseDefaults:
           begin
             LakeExtension := '';
           end;
         else
           Assert(False);
      end;

      case BcsFileNames.FlowInteraction of
        gfitFluidSource:
          begin
            FlowTypeExtension := '.F'
          end;
        gfitSpecifiedPressure:
          begin
            FlowTypeExtension := '.P'
          end;
        gfitUseDefaults:
          begin
            FlowTypeExtension := '';
          end;
         else
           Assert(False);
      end;
    end
    else
    begin
      LakeExtension := '';
      FlowTypeExtension := '';
    end;

    FileRoot := ChangeFileExt(AFileName, '');
    FNameOfFile := FileRoot + LakeExtension
      + FlowTypeExtension + Extension;
    FInputFileName := FNameOfFile;

    if BcsFileNames <> nil then
    begin
      if (BcsFileNames.LakeInteraction <> lbiUseDefaults)
        or (BcsFileNames.FlowInteraction <> gfitUseDefaults) then
      begin
        BcsFileNames.Add(FNameOfFile);
      end
      else
      begin
        BcsFileNames.Add('');
      end;
    end;

    WriteFileInternal;

    SutraFileWriter.AddBoundaryFile(FNameOfFile);
    FBcopgFileName := ChangeFileExt(FileRoot, '.bcopg');
    SutraFileWriter.AddFile(sftBcopg, BcopgFileName);

    if  Model.PestUsed and FPestParamUsed then
    begin
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;
    end;

  end
  else
  begin
    if BcsFileNames <> nil then
    begin
      BcsFileNames.Add('')
    end;
  end;
end;

{ TGenFlowInteractionStringList }

procedure TGenFlowInteractionStringList.SetFlowInteraction(
  const Value: TGeneralizedFlowInteractionType);
begin
  FFlowInteraction := Value;
end;

end.
