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
    procedure Evaluate;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2(TimeIndex: integer);
    procedure WriteDataSet7B(TimeIndex: integer);
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
  SutraMeshUnit, SutraFileWriterUnit, SparseArrayUnit;

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
begin
  inherited;
  FU1TimeLists := TObjectList<TSutraTimeList>.Create;
  FU2TimeLists := TObjectList<TSutraTimeList>.Create;
  FInflowUTimeLists := TObjectList<TSutraTimeList>.Create;
  FOutflowUTimeLists := TObjectList<TSutraTimeList>.Create;

  FTimes := TRealList.Create;
  FUseBctime := T3DSparseBooleanArray.Create(GetQuantum(Model.LayerCount+1),
    GetQuantum(Model.RowCount+1), GetQuantum(Model.ColumnCount+1));
end;

destructor TSutraGeneralTransportWriter.Destroy;
begin
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
//  AnItem: TSutraGenTransportItem;
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
  procedure InitializeTimeList(ListOfTimeLists: TObjectList<TSutraTimeList>;
    FormulaIndex: Integer; Descripion: string);
  var
    TimeList: TSutraTimeList;
    AnItem: TSutraGenTransportItem;
    TimeIndex: Integer;
  begin
    TimeList := TSutraTimeList.Create(Model, ScreenObject);
    ListOfTimeLists.Add(TimeList);
    if FEvaluationType = etDisplay then
    begin
      AnItem := ABoundary.Values[DisplayTimeIndex]
        as TSutraGenTransportItem;
      BoundaryValues[0].Time := AnItem.StartTime;
        BoundaryValues[0].Formula := AnItem.BoundaryFormula[FormulaIndex];
        BoundaryValues[0].UsedFormula := AnItem.UsedFormula;
    end
    else
    begin
      for TimeIndex := 0 to ABoundary.Values.Count - 1 do
      begin
        AnItem := ABoundary.Values[TimeIndex]
          as TSutraGenTransportItem;
        BoundaryValues[TimeIndex].Time := FixTime(AnItem, AllTimes);
        BoundaryValues[TimeIndex].Formula := AnItem.BoundaryFormula[FormulaIndex];
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

      InitializeTimeList(FU1TimeLists, LowerUPosition, 'Lower_U');
      InitializeTimeList(FU2TimeLists, HigherUPosition, 'Higher_U');
      InitializeTimeList(FInflowUTimeLists, LowerFlowUPosition, 'U_Inflow_Rate');
      InitializeTimeList(FOutflowUTimeLists, HigherFlowUPosition, 'U_Outflow_Rate');

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

  for ListIndex := 0 to FU1TimeLists.Count - 1 do
  begin
    SutraTimeList := FU1TimeLists[ListIndex];
    for TimeIndex := 0 to SutraTimeList.Count - 1 do
    begin
      FTimes.AddUnique(SutraTimeList.Times[TimeIndex]);
    end;
  end;

  FGeneralBoundaries.Clear;
  for timeIndex := 0 to FTimes.Count - 1 do
  begin
    TransNodes := TGeneralTransportNodes.Create;
    TransNodes.TimeIndex := AllTimes.IndexOf(FTimes[timeIndex]);
    Assert(TransNodes.TimeIndex >= 0);
    FGeneralBoundaries.Add(TransNodes);
  end;

  Mesh := Model.SutraMesh;
//  ActiveDataSet := Model.DataArrayManager.GetDataSetByName(rsActive);
  LastUsed := TIntegerList.Create;
  try
    for ListIndex := 0 to FU1TimeLists.Count - 1 do
    begin
      LastUsed.Add(0);
    end;

    for TimeIndex := 0 to FTimes.Count - 1 do
    begin
      ATime := FTimes[TimeIndex];
      NodeList := FGeneralBoundaries[TimeIndex];

      for ListIndex := 0 to FU1TimeLists.Count - 1 do
      begin
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
          LastUsed[ListIndex] :=  UsedIndex;
          // Get data sets for selected time
          U1Data := U1SutraTimeList[UsedIndex];
          if U1Data = nil then
          begin
            // inactive
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
                      NodeList.Add(TGeneralTransportNode.CreateInactive(
                        NodeNumber, LayerIndex, ColIndex));
                    end;
                  end;
                end;
              end;
            end;
            Continue;
          end;

          U2SutraTimeList := FU2TimeLists[ListIndex];
          U2Data := U2SutraTimeList[UsedIndex];
          Assert(U2Data <> nil);

          QU1SutraTimeList := FInflowUTimeLists[ListIndex];
          QU1Data := QU1SutraTimeList[UsedIndex];
          Assert(QU1Data <> nil);

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
                AssignNodeNumber;
                if NodeNumber < 0 then
                begin
                  Continue;
                end;
                U1.Value := U1Data.RealData[LayerIndex,0,ColIndex];
                U1.Annotation := U1Data.Annotation[LayerIndex,0,ColIndex];

                Assert(U2Data.IsValue[LayerIndex,0,ColIndex]);
                U2.Value := U2Data.RealData[LayerIndex,0,ColIndex];
                U2.Annotation := U2Data.Annotation[LayerIndex,0,ColIndex];

                Assert(QU1Data.IsValue[LayerIndex,0,ColIndex]);
                QU1.Value := QU1Data.RealData[LayerIndex,0,ColIndex];
                QU1.Annotation := QU1Data.Annotation[LayerIndex,0,ColIndex];

                Assert(QU2Data.IsValue[LayerIndex,0,ColIndex]);
                QU2.Value := QU2Data.RealData[LayerIndex,0,ColIndex];
                QU2.Annotation := QU2Data.Annotation[LayerIndex,0,ColIndex];

                if FUseBCTime.IsValue[LayerIndex, 0,ColIndex] then
                begin
                  UseBCTime := FUseBCTime.Items[LayerIndex, 0,ColIndex];
                end
                else
                begin
                  UseBCTime := False;
                end;

                if NodeNumber >= 0 then
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
    NodeArray := FGeneralBoundaries[TimeIndex].ToArray;
    for NodeIndex := 0 to Length(NodeArray) - 1 do
    begin
      ANode := NodeArray[NodeIndex];
      if ANode.Active then
      begin
        WriteInteger(ANode.NodeNumber+1);
        WriteFloat(ANode.FUValue1.Value);
        WriteFloat(ANode.FSoluteEnergyInflow.Value);
        WriteFloat(ANode.FUValue2.Value);
        WriteFloat(ANode.FSoluteEnergyOutflow.Value);
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
  TimeIndex: Integer;
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
//    FInputFileName := FNameOfFile;
    OpenFile(FNameOfFile);
    try
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
      WriteDataSet0;
      WriteDataSet1;
      for TimeIndex := 0 to FTimes.Count - 1 do
      begin
        WriteDataSet2(TimeIndex);
        WriteDataSet7B(TimeIndex);
      end;
      SutraFileWriter.AddBoundaryFile(FNameOfFile);
      FBcougFileName := ChangeFileExt(FileRoot, '.bcoug');
//      if BcsFileNames <> nil then
//      begin
//        LakeInteraction := BcsFileNames.LakeInteraction;
//        TransportInteraction := BcsFileNames.TransportInteraction;
//      end
//      else
//      begin
//        LakeInteraction := lbiUseDefaults;
//        TransportInteraction := gtitUseDefaults;
//      end;
      SutraFileWriter.AddFile(sftBcoug, FBcougFileName);
    finally
      CloseFile;
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
