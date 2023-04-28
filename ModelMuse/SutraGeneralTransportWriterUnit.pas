unit SutraGeneralTransportWriterUnit;

interface

uses
  System.UITypes, Winapi.Windows, System.SysUtils, CustomModflowWriterUnit,
  System.Generics.Collections, SutraGeneralFlowNodesUnit,
  System.Generics.Defaults, SutraBoundaryUnit, PhastModelUnit,
  SutraGenTransBoundUnit, RealListUnit, IntListUnit, GoPhastTypes, Vcl.Dialogs,
  System.Classes, SutraBoundaryWriterUnit, SutraOptionsUnit, SparseDataSets;

type
  TGeneralTransportNodes = class(TList<TGeneralTransportNode>, IGeneralTransportNodes)
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
    procedure Add(Node: TGeneralTransportNode);
    property Count: Integer read GetCount;
    function ToArray: TArray<TGeneralTransportNode>; reintroduce;
    property TimeIndex: Integer read GetTimeIndex write SetTimeIndex;
  end;

  TGenTransportInteractionStringList = class(TLakeInteractionStringList)
  private
    FTransportInteraction: TGeneralizedTransportInteractionType;
    procedure SetTransportInteraction(const Value: TGeneralizedTransportInteractionType);
  public
    property TransportInteraction: TGeneralizedTransportInteractionType
      read FTransportInteraction write SetTransportInteraction;
  end;

  TSutraGeneralTransportWriter = class(TCustomFileWriter)
  private
//    FNameOfFile: string;
    FU1TimeLists: TObjectList<TSutraTimeList>;
    FU2TimeLists: TObjectList<TSutraTimeList>;
    FInflowUTimeLists: TObjectList<TSutraTimeList>;
    FOutflowUTimeLists: TObjectList<TSutraTimeList>;
    FGeneralBoundaries: TList<IGeneralTransportNodes>;
    FTimes: TRealList;
    FBcsFileNames: TGenTransportInteractionStringList;
    FUseBctime: T3DSparseBooleanArray;
    FBcougFileName: string;
//    FNodeNumbers: T3DSparseIntegerArray;
    procedure Evaluate;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2(TimeIndex: integer);
    procedure WriteDataSet7B(TimeIndex: integer);
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
  public
    constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(AFileName: string;
      GeneralBoundaries: TList<IGeneralTransportNodes>;
      BcsFileNames: TGenTransportInteractionStringList);
    procedure UpdateDisplay(GeneralBoundaries: TList<IGeneralTransportNodes>);
    property BcougFileName: string read FBcougFileName;
  end;

const
  KGeneralizedTransport = 'GenTrans';

implementation

uses
  frmErrorsAndWarningsUnit, ScreenObjectUnit,
  SutraTimeScheduleUnit, frmGoPhastUnit, DataSetUnit,
  SutraMeshUnit, SutraFileWriterUnit, SparseArrayUnit, ModflowCellUnit,
  CellLocationUnit;

resourcestring
  StrGeneralizedtranspor = 'generalized-transport boundary condition';

{ TGeneralTranasportNodes }

procedure TGeneralTransportNodes.Add(Node: TGeneralTransportNode);
begin
  inherited Add(Node);
end;

function TGeneralTransportNodes.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TGeneralTransportNodes.GetTimeIndex: Integer;
begin
  result := FTimeIndex;
end;

function TGeneralTransportNodes.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TGeneralTransportNodes.SetTimeIndex(const Value: Integer);
begin
  FTimeIndex := Value;
end;

function TGeneralTransportNodes.ToArray: TArray<TGeneralTransportNode>;
type
  TNodeComparer = TComparer<TGeneralTransportNode>;
var
  Comparer: IComparer<TGeneralTransportNode>;
begin
  result := inherited;
  // sort the nodes in ascending order.
  Comparer := TNodeComparer.Construct(
    function(const L, R: TGeneralTransportNode): Integer
    begin
      result := L.NodeNumber - R.NodeNumber;
    end
    );
  TArray.Sort<TGeneralTransportNode>(Result, Comparer );
end;

function TGeneralTransportNodes._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGeneralTransportNodes._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TSutraGeneralTransportWriter }

constructor TSutraGeneralTransportWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
//var
//  Mesh: TSutraMesh3D;
//  NumberOfLayers: Integer;
//  NumberOfRows: Integer;
//  NumberOfColumns: Integer;
begin
  inherited;
  FU1TimeLists := TObjectList<TSutraTimeList>.Create;
  FU2TimeLists := TObjectList<TSutraTimeList>.Create;
  FInflowUTimeLists := TObjectList<TSutraTimeList>.Create;
  FOutflowUTimeLists := TObjectList<TSutraTimeList>.Create;

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

destructor TSutraGeneralTransportWriter.Destroy;
begin
//  FNodeNumbers.Free;
  FUseBctime.Free;
  FTimes.Free;

  FOutflowUTimeLists.Free;
  FInflowUTimeLists.Free;
  FU2TimeLists.Free;
  FU1TimeLists.Free;

  inherited;
end;

procedure TSutraGeneralTransportWriter.Evaluate;
var
  SimulationType: TSimulationType;
  TransientAllowed: Boolean;
  RootError: string;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraGeneralTransportBoundary;
  DisplayTimeIndex: Integer;
  TIndex: Integer;
  BoundaryValues: TSutraBoundaryValueArray;
  DisplayTime: Double;
  Item: TSutraGenTransportItem;
  SutraTimeOptions: TSutraTimeOptions;
  AllTimes: TRealList;
  TimeIndex: Integer;
  ListIndex: Integer;
  SutraTimeList: TSutraTimeList;
  LastUsed: TIntegerList;
  ATime: Double;
  U1SutraTimeList: TSutraTimeList;
  UsedIndex: Integer;
  U1Data: TDataArray;
  U2SutraTimeList: TSutraTimeList;
  U2Data: TDataArray;
  QU1SutraTimeList: TSutraTimeList;
  QU1Data: TDataArray;
  QU2SutraTimeList: TSutraTimeList;
  QU2Data: TDataArray;
  LayerIndex: Integer;
  Mesh: TSutraMesh3D;
  ColIndex: integer;
  LayerLimit: Integer;
  NodeNumber: Integer;
//  ActiveDataSet: TDataArray;
  U1: TValueAndAnnotation;
  U2: TValueAndAnnotation;
  QU1: TValueAndAnnotation;
  QU2: TValueAndAnnotation;
  NodeList: IGeneralTransportNodes;
  InnerIndex: integer;
  TransNodes: TGeneralTransportNodes;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  UseBCTime: Boolean;
  CellLocation: TCellLocation;
  CellLocPointer: PCellLocation;
  LowerUPestNamesList: TStringListObjectList;
  LowerFlowUPestNamesList: TStringListObjectList;
  HigherUPestNamesList: TStringListObjectList;
  HigherFlowUPestNamesList: TStringListObjectList;
  LowerUSeriesPestNames: TStringList;
  LowerFlowUSeriesPestNames: TStringList;
  HigherUSeriesPestNames: TStringList;
  HigherFlowUSeriesPestNames: TStringList;
  LowerUSeriesPestMethods: TPestMethodList;
  LowerFlowUSeriesPestMethods: TPestMethodList;
  HigherUSeriesPestMethods: TPestMethodList;
  HigherFlowUSeriesPestMethods: TPestMethodList;
  U1SeriesName: string;
  U1SeriesMethod: TPestParamMethod;
  U1Name: string;
  U2SeriesName: string;
  U2SeriesMethod: TPestParamMethod;
  U2Name: string;
  QU1SeriesName: string;
  QU1SeriesMethod: TPestParamMethod;
  QU1Name: string;
  QU2SeriesName: string;
  QU2SeriesMethod: TPestParamMethod;
  QU2Name: string;
  LowerUPestNames: TStringList;
  LowerFlowUPestNames: TStringList;
  HigherUPestNames: TStringList;
  HigherFlowUPestNames: TStringList;
  BoundaryActiveData: TDataArray;
  ModifiedValue: Double;
  procedure InitializeTimeList(ListOfTimeLists: TObjectList<TSutraTimeList>;
    FormulaIndex: Integer; Descripion: string; PestNames: TStringList);
  var
    TimeList: TSutraTimeList;
    AnItem: TSutraGenTransportItem;
    TimeIndex: Integer;
    Formula: string;
  begin
    TimeList := TSutraTimeList.Create(Model, ScreenObject);
    ListOfTimeLists.Add(TimeList);
    if FEvaluationType = etDisplay then
    begin
      AnItem := ABoundary.Values[DisplayTimeIndex]
        as TSutraGenTransportItem;
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
          as TSutraGenTransportItem;
        BoundaryValues[TimeIndex].Time := FixTime(AnItem, AllTimes);
        Formula := AnItem.BoundaryFormula[FormulaIndex];
        AssignPestFormula(Formula, ABoundary.PestBoundaryFormula[FormulaIndex],
          ABoundary.PestBoundaryMethod[FormulaIndex], PestNames);
        BoundaryValues[TimeIndex].Formula := Formula;
//        BoundaryValues[TimeIndex].Formula := AnItem.BoundaryFormula[FormulaIndex];
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
  RootError := Format(StrTheFollowingObjectSutra, [StrGeneralizedtranspor]);
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

  // Create objects for storing PEST parameters that apply to
  // individual time steps for all objects.
  // There will be one TStringList list for each TScreenObject.
  LowerUPestNamesList := TStringListObjectList.Create;
  LowerFlowUPestNamesList := TStringListObjectList.Create;
  HigherUPestNamesList := TStringListObjectList.Create;
  HigherFlowUPestNamesList := TStringListObjectList.Create;

  // Create string lists for storing PEST parameters that apply
  // to all time steps for all objects.
  LowerUSeriesPestNames := TStringList.Create;
  LowerFlowUSeriesPestNames := TStringList.Create;
  HigherUSeriesPestNames := TStringList.Create;
  HigherFlowUSeriesPestNames := TStringList.Create;

  // Create lists for storing how PEST parameters that apply
  // to all time steps are to be applied.
  LowerUSeriesPestMethods := TPestMethodList.Create;
  LowerFlowUSeriesPestMethods := TPestMethodList.Create;
  HigherUSeriesPestMethods := TPestMethodList.Create;
  HigherFlowUSeriesPestMethods := TPestMethodList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.SutraBoundaries.GenTransportBoundary;
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        // If lakes are used, separate BCS files are needed for each type
        // of lake interaction.
        if (FBcsFileNames <> nil)
          and ((FBcsFileNames.LakeInteraction <> ABoundary.LakeInteraction)
          or (FBcsFileNames.TransportInteraction <> ABoundary.LakeInteractionType))
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
          // Identify the item that should be applied for the
          // selected display time.
          for TIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            Item := ABoundary.Values[TIndex] as TSutraGenTransportItem;
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

        // Create lists of PEST parameter names to apply to a single object
        // for each time step.
        LowerUPestNames := TStringList.Create;
        LowerUPestNamesList.Add(LowerUPestNames);
        LowerFlowUPestNames := TStringList.Create;
        LowerFlowUPestNamesList.Add(LowerFlowUPestNames);
        HigherUPestNames := TStringList.Create;
        HigherUPestNamesList.Add(HigherUPestNames);
        HigherFlowUPestNames := TStringList.Create;
        HigherFlowUPestNamesList.Add(HigherFlowUPestNames);

        // Store the PEST names that apply to all stress periods.
        // The value that is stored is either the name of a PEST parameter
        // or the name of a data set that will be directly modified by PEST.
        LowerUSeriesPestNames.Add(ABoundary.PestBoundaryFormula[LowerUPosition]);
        LowerFlowUSeriesPestNames.Add(ABoundary.PestBoundaryFormula[LowerFlowUPosition]);
        HigherUSeriesPestNames.Add(ABoundary.PestBoundaryFormula[HigherUPosition]);
        HigherFlowUSeriesPestNames.Add(ABoundary.PestBoundaryFormula[HigherFlowUPosition]);

        // Store the method used to apply the PEST names.
        LowerUSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[LowerUPosition]);
        LowerFlowUSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[LowerFlowUPosition]);
        HigherUSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[HigherUPosition]);
        HigherFlowUSeriesPestMethods.Add(ABoundary.PestBoundaryMethod[HigherFlowUPosition]);

        InitializeTimeList(FU1TimeLists, LowerUPosition,
          'Lower_U', LowerUPestNames);
        InitializeTimeList(FU2TimeLists, HigherUPosition,
          'Higher_U', HigherUPestNames);
        InitializeTimeList(FInflowUTimeLists, LowerFlowUPosition,
          'U_Inflow_Rate', LowerFlowUPestNames);
        InitializeTimeList(FOutflowUTimeLists, HigherFlowUPosition,
          'U_Outflow_Rate', HigherFlowUPestNames);

        // The value of UseBCTime at each node.
        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count -1 do
          begin
            ACell := CellList[CellIndex];
            FUseBctime.Items[ACell.Layer, ACell.Row, ACell.Column] := ABoundary.UseBCTime;
          end;
        finally
          CellList.Free;
        end;
      end;
    end;

    // make a list of times in all the time lists.
    for ListIndex := 0 to FU1TimeLists.Count - 1 do
    begin
      SutraTimeList := FU1TimeLists[ListIndex];
      for TimeIndex := 0 to SutraTimeList.Count - 1 do
      begin
        FTimes.AddUnique(SutraTimeList.Times[TimeIndex]);
      end;
    end;

    // Create a list for storeing nodes for each time step at which
    // values will be changed.
    FGeneralBoundaries.Clear;
    for timeIndex := 0 to FTimes.Count - 1 do
    begin
      TransNodes := TGeneralTransportNodes.Create;
      TransNodes.TimeIndex := AllTimes.IndexOf(FTimes[timeIndex]);
      Assert(TransNodes.TimeIndex >= 0);
      FGeneralBoundaries.Add(TransNodes);
    end;

    Mesh := Model.SutraMesh;
    // LastUsed will store the index of the data set that was used
    // in the previous time step.
    LastUsed := TIntegerList.Create;
    try
      for ListIndex := 0 to FU1TimeLists.Count - 1 do
      begin
        LastUsed.Add(0);
      end;

      // For each time at which something is changed,
      // process the time lists.
      for TimeIndex := 0 to FTimes.Count - 1 do
      begin
        ATime := FTimes[TimeIndex];
        NodeList := FGeneralBoundaries[TimeIndex];

        for ListIndex := 0 to FU1TimeLists.Count - 1 do
        begin
          // For each list, identify the index of the data set that
          // applies for the current time step. Start searching with the one
          // that was used for the previous time step as stored in LastUsed.
          UsedIndex := -1;
          U1SutraTimeList :=FU1TimeLists[ListIndex];
          for TIndex := LastUsed[ListIndex] to U1SutraTimeList.Count - 1 do
          begin
            UsedIndex := TIndex;
            if U1SutraTimeList.Times[TIndex] > ATime then
            begin
              Dec(UsedIndex);
              break;
            end;
          end;
          if UsedIndex >= 0 then
          begin
            // Store the index of the data set used for the current time step.
            LastUsed[ListIndex] :=  UsedIndex;
            // Get data sets for selected time.
            U1Data := U1SutraTimeList[UsedIndex];
            BoundaryActiveData := U1SutraTimeList.UsedItems[UsedIndex];
            if U1Data = nil then
            begin
              // inactive in this time period.
              for InnerIndex := 0 to U1SutraTimeList.Count - 1 do
              begin
                U1Data := U1SutraTimeList[InnerIndex];
                if U1Data <> nil then
                begin
                  break;
                end;
              end;
              if U1Data <> nil then
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
                    if U1Data.IsValue[LayerIndex,0,ColIndex] then
                    begin
                      AssignNodeNumber;
                      if NodeNumber >= 0 then
                      begin
                        // Create an inactive general transport boundary at each
                        // active node in the grid where the object
                        // for the current time list defines one.
                        NodeList.Add(TGeneralTransportNode.CreateInactive(
                          NodeNumber, LayerIndex, ColIndex));
                      end;
                    end;
                  end;
                end;
              end;
              Continue;
            end;

            // Retrieve the PEST names and methods for the object that defined
            // the current set of lists.
            // Also get the data for the current time list and time step.
            U1SeriesName := LowerUSeriesPestNames[ListIndex];
            U1SeriesMethod := LowerUSeriesPestMethods[ListIndex];
            U1Name := LowerUPestNamesList[ListIndex][UsedIndex];

            // U1SutraTimeList and U1Data have already been retrieved.

            U2SeriesName := HigherUSeriesPestNames[ListIndex];
            U2SeriesMethod := HigherUSeriesPestMethods[ListIndex];
            U2Name := HigherUPestNamesList[ListIndex][UsedIndex];

            U2SutraTimeList := FU2TimeLists[ListIndex];
            U2Data := U2SutraTimeList[UsedIndex];
            Assert(U2Data <> nil);

            QU1SeriesName := LowerFlowUSeriesPestNames[ListIndex];
            QU1SeriesMethod := LowerFlowUSeriesPestMethods[ListIndex];
            QU1Name := LowerFlowUPestNamesList[ListIndex][UsedIndex];

            QU1SutraTimeList := FInflowUTimeLists[ListIndex];
            QU1Data := QU1SutraTimeList[UsedIndex];
            Assert(QU1Data <> nil);

            QU2SeriesName := HigherFlowUSeriesPestNames[ListIndex];
            QU2SeriesMethod := HigherFlowUSeriesPestMethods[ListIndex];
            QU2Name := HigherFlowUPestNamesList[ListIndex][UsedIndex];

            QU2SutraTimeList := FOutflowUTimeLists[ListIndex];
            QU2Data := QU2SutraTimeList[UsedIndex];
            Assert(QU2Data <> nil);

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
                if U1Data.IsValue[LayerIndex,0,ColIndex] then
                begin
                  CellLocation.Layer := LayerIndex;
                  CellLocation.Row := 0;
                  CellLocation.Column := ColIndex;

                  AssignNodeNumber;
                  if NodeNumber < 0 then
                  begin
                    // don't bother assigning data for inactive nodes.
                    Continue;
                  end;

                  if not BoundaryActiveData.BooleanData[LayerIndex,0,ColIndex] then
                  begin
                    NodeList.Add(TGeneralTransportNode.CreateInactive(
                      NodeNumber, LayerIndex, ColIndex));
                    Continue;
                  end;

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

                  Assert(QU1Data.IsValue[LayerIndex,0,ColIndex]);
                  QU1.Value := QU1Data.RealData[LayerIndex,0,ColIndex];
                  QU1.Annotation := QU1Data.Annotation[LayerIndex,0,ColIndex];
                  if (QU1Name <> '') or (QU1SeriesName <> '') then
                  begin
                    QU1.Formula := GetPestTemplateFormula(QU1.Value, QU1Name,
                      QU1SeriesName, QU1SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(QU1.Formula);
                  end
                  else
                  begin
                    QU1.Formula := '';
                  end;

                  Assert(QU2Data.IsValue[LayerIndex,0,ColIndex]);
                  QU2.Value := QU2Data.RealData[LayerIndex,0,ColIndex];
                  QU2.Annotation := QU2Data.Annotation[LayerIndex,0,ColIndex];
                  if (QU2Name <> '') or (QU2SeriesName <> '') then
                  begin
                    QU2.Formula := GetPestTemplateFormula(QU2.Value, QU2Name,
                      QU2SeriesName, QU2SeriesMethod, CellLocPointer, nil, ModifiedValue);
                    ExtendedTemplateFormula(QU2.Formula);
                  end
                  else
                  begin
                    QU2.Formula := '';
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
                    NodeList.Add(TGeneralTransportNode.Create(
                      NodeNumber, U1, QU1, U2, QU2, LayerIndex, ColIndex, UseBCTime));
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
    LowerUPestNamesList.Free;
    LowerFlowUPestNamesList.Free;
    HigherUPestNamesList.Free;
    HigherFlowUPestNamesList.Free;
    LowerUSeriesPestNames.Free;
    LowerFlowUSeriesPestNames.Free;
    HigherUSeriesPestNames.Free;
    HigherFlowUSeriesPestNames.Free;
    LowerUSeriesPestMethods.Free;
    LowerFlowUSeriesPestMethods.Free;
    HigherUSeriesPestMethods.Free;
    HigherFlowUSeriesPestMethods.Free;
  end;
end;

class function TSutraGeneralTransportWriter.Extension: string;
begin
  result := '.GtbBCS';
end;

procedure TSutraGeneralTransportWriter.UpdateDisplay(
  GeneralBoundaries: TList<IGeneralTransportNodes>);
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

procedure TSutraGeneralTransportWriter.WriteFileInternal;
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
      WriteDataSet7B(TimeIndex);
    end;
  finally
    CloseFile;
  end;
end;

procedure TSutraGeneralTransportWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('Generalized Transport'));
end;

procedure TSutraGeneralTransportWriter.WriteDataSet1;
var
  SimulationType: TSimulationType;
  BCSSCH: string;
begin
  WriteCommentLine('Data set 1');
  SimulationType := Model.SutraOptions.SimulationType;
  case SimulationType of
    stSteadyFlowSteadyTransport:
      begin
        BCSSCH := 'STEP_1';
      end;
    stSteadyFlowTransientTransport:
      begin
        BCSSCH := 'STEP_1';
      end;
    stTransientFlowTransientTransport:
      begin
        // write schedule name here.
        BCSSCH := KGeneralizedTransport;
      end;
  else
    Assert(False);
  end;
  WriteString('''' + BCSSCH + '''');
  WriteString(' # BCSSCH');
  NewLine;
end;

procedure TSutraGeneralTransportWriter.WriteDataSet2(TimeIndex: integer);
var
  NodeList: IGeneralTransportNodes;
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
  BCSID := '''generalized transport boundaries''';
  NSOP1 := 0;
  NSOU1 := 0;
  NPBC1 := 0;
  NUBC1 := 0;
  NPBG1 := 0;
  NUBG1 := NodeList.Count;

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

procedure TSutraGeneralTransportWriter.WriteDataSet7B(TimeIndex: integer);
var
  NodeIndex: Integer;
  NodeArray: TArray<TGeneralTransportNode>;
  ANode: TGeneralTransportNode;
//  NodeList: IGeneralFlowNodes;
begin
  if FGeneralBoundaries[TimeIndex].Count > 0 then
  begin
    WriteCommentLine('Data set 7B');
    NodeArray := FGeneralBoundaries[TimeIndex].ToArray;
    for NodeIndex := 0 to Length(NodeArray) - 1 do
    begin
      ANode := NodeArray[NodeIndex];
      if ANode.Active then
      begin
        WriteInteger(ANode.NodeNumber+1);
        if WritingTemplate and (ANode.FUValue1.Formula <> '') then
        begin
          WriteString(ANode.FUValue1.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.FUValue1.Value);
        end;
        if WritingTemplate and (ANode.FSoluteEnergyInflow.Formula <> '') then
        begin
          WriteString(ANode.FSoluteEnergyInflow.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.FSoluteEnergyInflow.Value);
        end;
        if WritingTemplate and (ANode.FUValue2.Formula <> '') then
        begin
          WriteString(ANode.FUValue2.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.FUValue2.Value);
        end;
        if WritingTemplate and (ANode.FSoluteEnergyOutflow.Formula <> '') then
        begin
          WriteString(ANode.FSoluteEnergyOutflow.Formula);
          FPestParamUsed := True;
        end
        else
        begin
          WriteFloat(ANode.FSoluteEnergyOutflow.Value);
        end;
//        WriteFloat(ANode.FUValue1.Value);
//        WriteFloat(ANode.FSoluteEnergyInflow.Value);
//        WriteFloat(ANode.FUValue2.Value);
//        WriteFloat(ANode.FSoluteEnergyOutflow.Value);
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

procedure TSutraGeneralTransportWriter.WriteFile(AFileName: string;
  GeneralBoundaries: TList<IGeneralTransportNodes>;
  BcsFileNames: TGenTransportInteractionStringList);
var
  LakeExtension: string;
  TransportTypeExtension: string;
//  LakeInteraction: TLakeBoundaryInteraction;
//  TransportInteraction: TGeneralizedTransportInteractionType;
  FileRoot: string;
begin
  if Model.ModelSelection = msSutra22 then
  begin
    if BcsFileNames <> nil then
    begin
      BcsFileNames.Add('');
    end;
    Exit;
  end;
  FBcsFileNames := BcsFileNames;

  FGeneralBoundaries := GeneralBoundaries;

  Evaluate;

  if FU1TimeLists.Count > 0 then
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

      case BcsFileNames.TransportInteraction of
        gtitSoluteSource:
          begin
            TransportTypeExtension := '.S'
          end;
        gtitSpecifiedConcentration:
          begin
            TransportTypeExtension := '.U'
          end;
        gtitUseDefaults:
          begin
            TransportTypeExtension := '';
          end;
         else 
           Assert(False);
      end;
    end
    else
    begin
      LakeExtension := '';
      TransportTypeExtension := ''
    end;
    FileRoot := ChangeFileExt(AFileName, '');
    FNameOfFile := FileRoot + LakeExtension
      + TransportTypeExtension + Extension;
    FInputFileName := FNameOfFile;

    if BcsFileNames <> nil then
    begin
      if (BcsFileNames.LakeInteraction <> lbiUseDefaults)
        or (BcsFileNames.TransportInteraction <> gtitUseDefaults) then
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
    FBcougFileName := ChangeFileExt(FileRoot, '.bcoug');
    SutraFileWriter.AddFile(sftBcoug, FBcougFileName);

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
      BcsFileNames.Add('');
    end;
  end;
end;

{ TGenTransportInteractionStringList }

procedure TGenTransportInteractionStringList.SetTransportInteraction(
  const Value: TGeneralizedTransportInteractionType);
begin
  FTransportInteraction := Value;
end;

end.
