unit SutraBoundaryWriterUnit;

interface

uses Windows,
  CustomModflowWriterUnit, GoPhastTypes, SutraBoundariesUnit,
  Generics.Collections, PhastModelUnit, DataSetUnit, SparseDataSets,
  SysUtils, RealListUnit, SutraBoundaryUnit, ModflowBoundaryUnit,
  System.Classes, SutraOptionsUnit;

type
  TBoundaryNodes = class(TDictionary<Integer, TBoundaryNode>, IBoundaryNodes)
  private
    FRefCount: Integer;
    function GetCount: Integer;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AddUnique(Node: TBoundaryNode);
    property Count: Integer read GetCount;
    function ToArray: TArray<TPair<Integer,TBoundaryNode>>; reintroduce;
  end;

  TSutraFluxCheckList = class(TCustomTimeList)
  protected
    procedure CheckSameModel(const Data: TDataArray); override;
  public
    procedure Initialize; override;
  end;

  TLakeInteractionStringList = class(TStringList)
  private
    FLakeInteraction: TLakeBoundaryInteraction;
    procedure SetLakeInteraction(const Value: TLakeBoundaryInteraction);
  public
    constructor Create;
    property LakeInteraction: TLakeBoundaryInteraction read FLakeInteraction write SetLakeInteraction;
  end;

  TLakeInteractionStringLists = TObjectList<TLakeInteractionStringList>;

  TSutraBoundaryWriter = class(TCustomFileWriter)
  private
    FBoundaryType: TSutraBoundaryType;
    FPQTimeLists: TObjectList<TSutraTimeList>;
    FUTimeLists: TObjectList<TSutraTimeList>;
    FNodeNumbers: T3DSparseIntegerArray;
    FCount: Integer;
    FIBoundaryNodes: IBoundaryNodes;
    FTime1: Double;
    FBcsFileNames: TLakeInteractionStringList;
    FUseBctime: T3DSparseBooleanArray;
    procedure Evaluate;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2(TimeIndex: integer; PQTimeList,
      UTimeList: TSutraMergedTimeList);
    procedure WriteDataSet3(TimeIndex: integer; PQTimeList,
      UTimeList: TSutraMergedTimeList);
    procedure WriteDataSet4(TimeIndex: integer; UTimeList: TSutraMergedTimeList);
    procedure WriteDataSet5(TimeIndex: integer; PQTimeList,
      UTimeList: TSutraMergedTimeList);
    procedure WriteDataSet6(TimeIndex: integer; UTimeList: TSutraMergedTimeList);
  protected
    class function Extension: string; override;
  public
    constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      BoundaryType: TSutraBoundaryType); reintroduce;
    destructor Destroy; override;
    // @name calls @link(Evaluate).
    procedure UpdateMergeLists(PQTimeList, UTimeList: TSutraMergedTimeList);
    procedure WriteFile(FileName: string; BoundaryNodes: IBoundaryNodes;
      BcsFileNames: TLakeInteractionStringList);
  end;

function FixTime(AnItem: TCustomBoundaryItem; AllTimes: TRealList): double; overload;

const
  KFluidFlux = 'FluidFlux';
  KUFlux = 'UFlux';
  KSpecifiedP = 'SpecifiedP';
  KSpecifiedU = 'SpecifiedU';

implementation

uses
  ScreenObjectUnit,
  frmGoPhastUnit, SutraTimeScheduleUnit,
  RbwParser, SutraMeshUnit, SparseArrayUnit, Math, SutraFileWriterUnit,
  frmErrorsAndWarningsUnit, System.Generics.Defaults;

resourcestring
  StrFluidSource = 'Fluid Source';
  StrMassOrEnergySourc = 'Mass or Energy Source';
  StrSpecifiedPressureS = 'Specified Pressure Source';
  StrSpecifiedConcentrat = 'Specified Concentration or Temperature Source';

{ TSutraBoundaryWriter }

constructor TSutraBoundaryWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; BoundaryType: TSutraBoundaryType);
var
  Mesh: TSutraMesh3D;
  NumberOfLayers: Integer;
  NumberOfRows: Integer;
  NumberOfColumns: Integer;
  TimeOptions: TSutraTimeOptions;
begin
  inherited Create(Model, EvaluationType);
  FBcsFileNames := nil;
  TimeOptions := (Model as TPhastModel).SutraTimeOptions;
  TimeOptions.CalculateAllTimes;
  if TimeOptions.AllTimes.Count > 1 then
  begin
    FTime1 := TimeOptions.AllTimes[1];
  end
  else
  begin
    FTime1 := TimeOptions.AllTimes[0];
  end;
  FBoundaryType := BoundaryType;
  FPQTimeLists := TObjectList<TSutraTimeList>.Create;
  FUTimeLists := TObjectList<TSutraTimeList>.Create;
  Mesh := Model.SutraMesh;
  if Mesh <> nil then
  begin

    if ((Model.Mesh as TSutraMesh3D).MeshType = mt3D)
//      and (EvaluatedAt = eaNodes)
      {and (Orientation = dso3D)} then
    begin
      NumberOfLayers := frmGoPhast.PhastModel.
        SutraLayerStructure.LayerCount+1;
    end
    else
    begin
      NumberOfLayers := frmGoPhast.PhastModel.
        SutraLayerStructure.LayerCount;
    end;
    NumberOfRows := 1;
//    case EvaluatedAt of
//      eaBlocks: NumberOfColumns := Mesh.Elements.Count;
      {eaNodes:} NumberOfColumns := Mesh.Mesh2D.Nodes.Count;
//      else Assert(False);
//    end;
  end
  else
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
  end;
  FNodeNumbers := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
    GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  FUseBctime := T3DSparseBooleanArray.Create(GetQuantum(NumberOfLayers),
    GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
end;

destructor TSutraBoundaryWriter.Destroy;
begin
  FNodeNumbers.Free;
  FPQTimeLists.Free;
  FUTimeLists.Free;
  FUseBctime.Free;
  inherited;
end;

function FixTime(AnItem:TCustomBoundaryItem; AllTimes: TRealList): double;
var
  ParentCollection: TCollection;
  NextItem: TCustomBoundaryItem;
  SimulationType: TSimulationType;
begin
  SimulationType := frmGoPhast.PhastModel.SutraOptions.SimulationType;
  result := AnItem.StartTime;
  if (SimulationType in [stSteadyFlowTransientTransport,
    stTransientFlowTransientTransport]) and (AnItem.Index = 0)
    and (AnItem.StartTime = AllTimes[0]) then
  begin
    ParentCollection := AnItem.Collection;// as TCustomSutraBoundaryCollection;
    if ParentCollection.Count = 1 then
    begin
      result := AllTimes[1];
    end
    else
    begin
      NextItem := ParentCollection.Items[1] as TCustomBoundaryItem;
      if NextItem.StartTime <> AllTimes[1] then
      begin
        result := AllTimes[1];
      end;
    end;
  end;
end;


procedure TSutraBoundaryWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraBoundary;
  TimeList: TSutraTimeList;
  BoundaryValues: TSutraBoundaryValueArray;
  DisplayTimeIndex: Integer;
  Item: TCustomSutraBoundaryItem;
  AssocItem: TCustomSutraAssociatedBoundaryItem;
  SutraTimeOptions: TSutraTimeOptions;
  DisplayTime: Double;
  TIndex: Integer;
  TimeIndex: Integer;
  AllTimes: TRealList;
  TransientAllowed: Boolean;
  SimulationType: TSimulationType;
  BoundaryIdentifier: string;
  RootError: string;
  CellIndex: Integer;
  ACell: TCellAssignment;
  CellList: TCellAssignmentList;
begin
  TransientAllowed := False;
  SimulationType := Model.SutraOptions.SimulationType;
  SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
  SutraTimeOptions.CalculateAllTimes;
  AllTimes := SutraTimeOptions.AllTimes;
  if FEvaluationType = etDisplay then
  begin
    DisplayTime := Model.ThreeDDisplayTime;
    SetLength(BoundaryValues, 1);
  end
  else
  begin
    DisplayTime := 0;
  end;
  
  case FBoundaryType of
    sbtFluidSource:
    begin
      TransientAllowed := SimulationType = stTransientFlowTransientTransport;
      BoundaryIdentifier := StrFluidSource;
    end;
    sbtMassEnergySource:
    begin
      TransientAllowed := SimulationType in [stSteadyFlowTransientTransport,
        stTransientFlowTransientTransport];
      BoundaryIdentifier := StrMassOrEnergySourc;
    end;
    sbtSpecPress:
    begin
      TransientAllowed := SimulationType = stTransientFlowTransientTransport;
      BoundaryIdentifier := StrSpecifiedPressureS;
    end;
    sbtSpecConcTemp:
    begin
      TransientAllowed := SimulationType in [stSteadyFlowTransientTransport,
        stTransientFlowTransientTransport];
      BoundaryIdentifier := StrSpecifiedConcentrat;
    end;
  else
    Assert(False);
  end;
  RootError := Format(StrTheFollowingObjectSutra, [BoundaryIdentifier]);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, RootError);

  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    ABoundary := nil;
    case FBoundaryType of
      sbtFluidSource:
      begin
        ABoundary := ScreenObject.SutraBoundaries.FluidSource;
      end;
      sbtMassEnergySource:
      begin
        ABoundary := ScreenObject.SutraBoundaries.MassEnergySource;
      end;
      sbtSpecPress:
      begin
        ABoundary := ScreenObject.SutraBoundaries.SpecifiedPressure;
      end;
      sbtSpecConcTemp:
      begin
        ABoundary := ScreenObject.SutraBoundaries.SpecifiedConcTemp;
      end;
    else
      Assert(False);
    end;
    if ABoundary.Used then
    begin
      
      
      if (FBcsFileNames <> nil) and
        (FBcsFileNames.LakeInteraction <> ABoundary.LakeInteraction) then
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
          Item := ABoundary.Values[TIndex] as TCustomSutraBoundaryItem;
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
      if FBoundaryType in [sbtFluidSource, sbtSpecPress] then
      begin
        TimeList := TSutraTimeList.Create(Model, ScreenObject);
        FPQTimeLists.Add(TimeList);
        if FEvaluationType = etDisplay then
        begin
          AssocItem := ABoundary.Values[DisplayTimeIndex]
            as TCustomSutraAssociatedBoundaryItem;
          BoundaryValues[0].Time := AssocItem.StartTime;
          BoundaryValues[0].UsedFormula := AssocItem.UsedFormula;
          BoundaryValues[0].Formula := AssocItem.PQFormula;
//          BoundaryValues[0].LakeInteraction := ABoundary.LakeInteraction;
        end
        else
        begin
          for TimeIndex := 0 to ABoundary.Values.Count - 1 do
          begin
            AssocItem := ABoundary.Values[TimeIndex]
              as TCustomSutraAssociatedBoundaryItem;
            BoundaryValues[TimeIndex].Time := FixTime(AssocItem, AllTimes);
            BoundaryValues[TimeIndex].UsedFormula := AssocItem.UsedFormula;
            BoundaryValues[TimeIndex].Formula := AssocItem.PQFormula;
//            BoundaryValues[TimeIndex].LakeInteraction := ABoundary.LakeInteraction;
          end;
        end;
        TimeList.Initialize(BoundaryValues);
      end;

      TimeList := TSutraTimeList.Create(Model, ScreenObject);
      FUTimeLists.Add(TimeList);
      if FEvaluationType = etDisplay then
      begin
        Item := ABoundary.Values[DisplayTimeIndex] as TCustomSutraBoundaryItem;
        BoundaryValues[0].Time := Item.StartTime;
        BoundaryValues[0].UsedFormula := Item.UsedFormula;
        BoundaryValues[0].Formula := Item.UFormula;
//        BoundaryValues[0].LakeInteraction := ABoundary.LakeInteraction;
      end
      else
      begin
        for TimeIndex := 0 to ABoundary.Values.Count - 1 do
        begin
          Item := ABoundary.Values[TimeIndex] as TCustomSutraBoundaryItem;
          BoundaryValues[TimeIndex].Time := FixTime(Item, AllTimes);
          BoundaryValues[TimeIndex].UsedFormula := Item.UsedFormula;
          BoundaryValues[TimeIndex].Formula := Item.UFormula;
//          BoundaryValues[TimeIndex].LakeInteraction := ABoundary.LakeInteraction;
        end;
      end;
      TimeList.Initialize(BoundaryValues);

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
end;

class function TSutraBoundaryWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraBoundaryWriter.UpdateMergeLists(PQTimeList,
  UTimeList: TSutraMergedTimeList);
var
  Times: TRealList;
  TimeIndex: Integer;
  AList: TSutraTimeList;
  ListIndex: Integer;
  DataArray: TTransientRealSparseDataSet;
  PQDataSet: TTransientRealSparseDataSet;
  StartTimeIndex: Integer;
  NextTimeIndex: Integer;
  TimeListIndex: Integer;
  DataSetIndex: Integer;
  APQTimeList: TSutraTimeList;
  AUTimeList: TSutraTimeList;
  MergedPQDataSet: TDataArray;
  MergedUDataSet: TDataArray;
  UDataSet: TTransientRealSparseDataSet;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  PositiveFluxes: TSutraFluxCheckList;
  PositiveUFluxes: TSutraFluxCheckList;
  PositiveDataSet: TDataArray;
  PositiveUDataSet: TDataArray;
  Mesh: TSutraMesh3D;
  ANode: TSutraNode3D;
  ANode2D: TSutraNode2D;
begin
  Evaluate;
  Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  FNodeNumbers.Clear;
  Times := TRealList.Create;
  try
    Times.Sorted := True;
    for ListIndex := 0 to FPQTimeLists.Count - 1 do
    begin
      AList := FPQTimeLists[ListIndex];
      for TimeIndex := 0 to AList.Count - 1 do
      begin
        Times.AddUnique(AList.Times[TimeIndex]);
      end;
    end;
    PQTimeList.Clear;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      DataArray := TTransientRealSparseDataSet.Create(Model, Mesh.LayerCount+1,
        1, Mesh.Mesh2D.Nodes.Count);
      DataArray.DataType := rdtDouble;
      DataArray.Orientation := dso3D;
      DataArray.UpdateDimensions(Mesh.LayerCount+1, 1, Mesh.Mesh2D.Nodes.Count);
//      DataArray.SetDimensions(False);
      PQTimeList.Add(Times[TimeIndex], DataArray)
    end;

    Times.Clear;
    Times.Sorted := True;
    for ListIndex := 0 to FUTimeLists.Count - 1 do
    begin
      AList := FUTimeLists[ListIndex];
      for TimeIndex := 0 to AList.Count - 1 do
      begin
        Times.AddUnique(AList.Times[TimeIndex]);
      end;
    end;
    UTimeList.Clear;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      DataArray := TTransientRealSparseDataSet.Create(Model, Mesh.LayerCount+1,
        1, Mesh.Mesh2D.Nodes.Count);
      DataArray.DataType := rdtDouble;
      DataArray.Orientation := dso3D;
      DataArray.UpdateDimensions(Mesh.LayerCount+1, 1, Mesh.Mesh2D.Nodes.Count);
//      DataArray.SetDimensions(False);
      UTimeList.Add(Times[TimeIndex], DataArray)
    end;

    case FBoundaryType of
      sbtFluidSource:
        begin
          PositiveFluxes := TSutraFluxCheckList.Create(nil);
          PositiveUFluxes := TSutraFluxCheckList.Create(nil);
          try
            for TimeIndex := 0 to Times.Count - 1 do
            begin
              DataArray := TTransientRealSparseDataSet.Create(Model,
                Mesh.LayerCount+1, 1, Mesh.Mesh2D.Nodes.Count);
              DataArray.DataType := rdtDouble;
              PositiveFluxes.Add(Times[TimeIndex], DataArray);

              DataArray := TTransientRealSparseDataSet.Create(Model,
                Mesh.LayerCount+1, 1, Mesh.Mesh2D.Nodes.Count);
              DataArray.DataType := rdtDouble;
              PositiveUFluxes.Add(Times[TimeIndex], DataArray);
            end;

            Assert(PQTimeList.Count = UTimeList.Count);
            for TimeListIndex := 0 to FPQTimeLists.Count - 1 do
            begin
              APQTimeList := FPQTimeLists[TimeListIndex];
              AUTimeList := FUTimeLists[TimeListIndex];
              Assert(APQTimeList.Count = AUTimeList.Count);
              for DataSetIndex := 0 to APQTimeList.Count - 1 do
              begin
                Assert(APQTimeList.Times[DataSetIndex] = AUTimeList.Times
                  [DataSetIndex]);
                PQDataSet := APQTimeList[DataSetIndex]
                  as TTransientRealSparseDataSet;
                UDataSet := AUTimeList[DataSetIndex]
                  as TTransientRealSparseDataSet;
                Assert(PQDataSet <> nil);
                Assert(UDataSet <> nil);

                PositiveDataSet := PositiveFluxes[DataSetIndex];
                PositiveUDataSet := PositiveUFluxes[DataSetIndex];
                Assert(PositiveDataSet <> nil);
                Assert(PositiveUDataSet <> nil);

                StartTimeIndex :=
                  Times.IndexOf(APQTimeList.Times[DataSetIndex]);
                if DataSetIndex < APQTimeList.Count - 1 then
                begin
                  NextTimeIndex :=
                    Times.IndexOf(APQTimeList.Times[DataSetIndex + 1])-1;
                end
                else
                begin
                  NextTimeIndex := Times.Count-1;
                end;

                for TimeIndex := StartTimeIndex to NextTimeIndex do
                begin
                  MergedPQDataSet := PQTimeList[TimeIndex];
                  MergedUDataSet := UTimeList[TimeIndex];
                  PositiveDataSet := PositiveFluxes[TimeIndex];
                  PositiveUDataSet := PositiveUFluxes[TimeIndex];
                  for LayerIndex := PQDataSet.MinLayer to PQDataSet.MaxLayer do
                  begin
                    for RowIndex := PQDataSet.MinRow to PQDataSet.MaxRow do
                    begin
                      for ColIndex := PQDataSet.MinColumn to PQDataSet.MaxColumn do
                      begin
                        Assert(PQDataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                          = UDataSet.IsValue[LayerIndex, RowIndex, ColIndex]);
                        if PQDataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        then
                        begin
                          Assert(UDataSet.IsValue[LayerIndex, RowIndex,
                            ColIndex]);
                          FNodeNumbers[LayerIndex, RowIndex, ColIndex] := 1;
                          if MergedPQDataSet.IsValue[LayerIndex, RowIndex,
                            ColIndex] then
                          begin
                            MergedPQDataSet.RealData[LayerIndex, RowIndex,
                              ColIndex] := MergedPQDataSet.RealData
                              [LayerIndex, RowIndex, ColIndex] +
                              PQDataSet.RealData[LayerIndex, RowIndex,
                              ColIndex];
                            MergedPQDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex] := MergedPQDataSet.Annotation
                              [LayerIndex, RowIndex, ColIndex] + ' plus ' +
                              PQDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex];
                            MergedUDataSet.RealData[LayerIndex, RowIndex,
                              ColIndex] := MergedUDataSet.RealData
                              [LayerIndex, RowIndex, ColIndex] +
                              UDataSet.RealData[LayerIndex, RowIndex, ColIndex];
                            MergedUDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex] := MergedUDataSet.Annotation
                              [LayerIndex, RowIndex, ColIndex] + ' plus ' +
                              UDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex];
                          end
                          else
                          begin
                            MergedPQDataSet.RealData[LayerIndex, RowIndex,
                              ColIndex] := PQDataSet.RealData
                              [LayerIndex, RowIndex, ColIndex];
                            MergedPQDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex] := PQDataSet.Annotation
                              [LayerIndex, RowIndex, ColIndex];
                            MergedUDataSet.RealData[LayerIndex, RowIndex,
                              ColIndex] := UDataSet.RealData[LayerIndex,
                              RowIndex, ColIndex];
                            MergedUDataSet.Annotation[LayerIndex, RowIndex,
                              ColIndex] := UDataSet.Annotation
                              [LayerIndex, RowIndex, ColIndex];
                          end;
                          if PQDataSet.RealData[LayerIndex, RowIndex,
                            ColIndex] > 0 then
                          begin
                            Assert(PositiveDataSet.IsValue[LayerIndex, RowIndex,
                              ColIndex] = PositiveUDataSet.IsValue[LayerIndex,
                              RowIndex, ColIndex]);
                            if PositiveDataSet.IsValue[LayerIndex, RowIndex,
                              ColIndex] then
                            begin
                              PositiveDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex] := PositiveDataSet.RealData
                                [LayerIndex, RowIndex, ColIndex] +
                                PQDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex];
                              PositiveDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex] := PositiveDataSet.Annotation
                                [LayerIndex, RowIndex, ColIndex] + ' plus ' +
                                PQDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex];

                              PositiveUDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex] := PositiveUDataSet.RealData
                                [LayerIndex, RowIndex, ColIndex] +
                                PQDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex] * UDataSet.RealData
                                [LayerIndex, RowIndex, ColIndex];
                              PositiveUDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex] := PositiveUDataSet.Annotation
                                [LayerIndex, RowIndex, ColIndex] + ' plus ' +
                                UDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex];
                            end
                            else
                            begin
                              PositiveDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex] := PQDataSet.RealData
                                [LayerIndex, RowIndex, ColIndex];
                              PositiveDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex] := PQDataSet.Annotation
                                [LayerIndex, RowIndex, ColIndex];
                              PositiveUDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex] := PQDataSet.RealData
                                [LayerIndex, RowIndex, ColIndex] *
                                UDataSet.RealData[LayerIndex, RowIndex,
                                ColIndex];
                              PositiveUDataSet.Annotation[LayerIndex, RowIndex,
                                ColIndex] := UDataSet.Annotation
                                [LayerIndex, RowIndex, ColIndex];
                            end;
                          end;
                        end
                        else
                        begin
                          Assert(not UDataSet.IsValue[LayerIndex, RowIndex,
                            ColIndex])
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;

            for TimeIndex := 0 to PQTimeList.Count - 1 do
            begin
              MergedPQDataSet := PQTimeList[TimeIndex];
              MergedUDataSet := UTimeList[TimeIndex];
              Assert(MergedPQDataSet <> nil);
              Assert(MergedUDataSet <> nil);
              PositiveDataSet := PositiveFluxes[TimeIndex];
              PositiveUDataSet := PositiveUFluxes[TimeIndex];
              Assert(PositiveDataSet <> nil);
              Assert(PositiveUDataSet <> nil);
              for LayerIndex := 0 to MergedPQDataSet.LayerCount - 1 do
              begin
                for RowIndex := 0 to MergedPQDataSet.RowCount - 1 do
                begin
                  for ColIndex := 0 to MergedPQDataSet.ColumnCount - 1 do
                  begin
                    Assert(MergedPQDataSet.IsValue[LayerIndex, RowIndex,
                      ColIndex] = MergedUDataSet.IsValue[LayerIndex, RowIndex,
                      ColIndex]);
                    if MergedPQDataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      and (MergedPQDataSet.RealData[LayerIndex, RowIndex,
                      ColIndex] > 0) then
                    begin
                      Assert(PositiveDataSet.IsValue[LayerIndex, RowIndex,
                        ColIndex]);
                      Assert(PositiveUDataSet.IsValue[LayerIndex, RowIndex,
                        ColIndex]);
                      MergedUDataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
                        PositiveUDataSet.RealData[LayerIndex, RowIndex,
                        ColIndex] / PositiveDataSet.RealData[LayerIndex,
                        RowIndex, ColIndex];
                      MergedUDataSet.Annotation[LayerIndex, RowIndex, ColIndex]
                        := PositiveUDataSet.Annotation[LayerIndex,
                        RowIndex, ColIndex]
                    end;
                  end;
                end;
              end;
            end;
          finally
            PositiveUFluxes.Free;
            PositiveFluxes.Free;
          end;
        end;
      sbtMassEnergySource:
        begin
          Assert(PQTimeList.Count = 0);
          for TimeListIndex := 0 to FUTimeLists.Count - 1 do
          begin
            AUTimeList := FUTimeLists[TimeListIndex];
            for DataSetIndex := 0 to AUTimeList.Count - 1 do
            begin
              UDataSet := AUTimeList[DataSetIndex]
                as TTransientRealSparseDataSet;
              Assert(UDataSet <> nil);
              StartTimeIndex := Times.IndexOf(AUTimeList.Times[DataSetIndex]);
              if DataSetIndex < AUTimeList.Count - 1 then
              begin
                NextTimeIndex :=
                  Times.IndexOf(AUTimeList.Times[DataSetIndex + 1])-1;
              end
              else
              begin
                NextTimeIndex := Times.Count-1;
              end;

              for TimeIndex := StartTimeIndex to NextTimeIndex do
              begin
                MergedUDataSet := UTimeList[TimeIndex];
                for LayerIndex := UDataSet.MinLayer to UDataSet.MaxLayer do
                begin
                  for RowIndex := UDataSet.MinRow to UDataSet.MaxRow do
                  begin
                    for ColIndex := UDataSet.MinColumn to UDataSet.MaxColumn do
                    begin
                      if UDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        FNodeNumbers[LayerIndex, RowIndex, ColIndex] := 1;
                        if MergedUDataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        then
                        begin
                          MergedUDataSet.RealData[LayerIndex, RowIndex,
                            ColIndex] := MergedUDataSet.RealData
                            [LayerIndex, RowIndex, ColIndex] + UDataSet.RealData
                            [LayerIndex, RowIndex, ColIndex];
                          MergedUDataSet.Annotation[LayerIndex, RowIndex,
                            ColIndex] := MergedUDataSet.Annotation
                            [LayerIndex, RowIndex, ColIndex] + ' plus ' +
                            UDataSet.Annotation[LayerIndex, RowIndex, ColIndex];
                        end
                        else
                        begin
                          MergedUDataSet.RealData[LayerIndex, RowIndex,
                            ColIndex] := UDataSet.RealData[LayerIndex, RowIndex,
                            ColIndex];
                          MergedUDataSet.Annotation[LayerIndex, RowIndex,
                            ColIndex] := UDataSet.Annotation[LayerIndex,
                            RowIndex, ColIndex];
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      sbtSpecPress:
        begin
          Assert(PQTimeList.Count = UTimeList.Count);
          for TimeListIndex := 0 to FPQTimeLists.Count - 1 do
          begin
            APQTimeList := FPQTimeLists[TimeListIndex];
            AUTimeList := FUTimeLists[TimeListIndex];
            Assert(APQTimeList.Count = AUTimeList.Count);
            for DataSetIndex := 0 to APQTimeList.Count - 1 do
            begin
              Assert(APQTimeList.Times[DataSetIndex] = AUTimeList.Times
                [DataSetIndex]);
              PQDataSet := APQTimeList[DataSetIndex]
                as TTransientRealSparseDataSet;
              UDataSet := AUTimeList[DataSetIndex]
                as TTransientRealSparseDataSet;
              Assert(PQDataSet <> nil);
              Assert(UDataSet <> nil);
              StartTimeIndex := Times.IndexOf(APQTimeList.Times[DataSetIndex]);
              if DataSetIndex < APQTimeList.Count - 1 then
              begin
                NextTimeIndex :=
                  Times.IndexOf(APQTimeList.Times[DataSetIndex + 1])-1;
              end
              else
              begin
                NextTimeIndex := Times.Count-1;
              end;

              for TimeIndex := StartTimeIndex to NextTimeIndex do
              begin
                MergedPQDataSet := PQTimeList[TimeIndex];
                MergedUDataSet := UTimeList[TimeIndex];
                for LayerIndex := PQDataSet.MinLayer to PQDataSet.MaxLayer do
                begin
                  for RowIndex := PQDataSet.MinRow to PQDataSet.MaxRow do
                  begin
                    for ColIndex := PQDataSet.MinColumn to PQDataSet.MaxColumn do
                    begin
                      Assert(PQDataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        = UDataSet.IsValue[LayerIndex, RowIndex, ColIndex]);
                      if PQDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        FNodeNumbers[LayerIndex, RowIndex, ColIndex] := 1;
                        MergedPQDataSet.RealData[LayerIndex, RowIndex, ColIndex]
                          := PQDataSet.RealData[LayerIndex, RowIndex, ColIndex];
                        MergedPQDataSet.Annotation[LayerIndex, RowIndex,
                          ColIndex] := PQDataSet.Annotation[LayerIndex,
                          RowIndex, ColIndex];
                        MergedUDataSet.RealData[LayerIndex, RowIndex, ColIndex]
                          := UDataSet.RealData[LayerIndex, RowIndex, ColIndex];
                        MergedUDataSet.Annotation[LayerIndex, RowIndex,
                          ColIndex] := UDataSet.Annotation[LayerIndex, RowIndex,
                          ColIndex];
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      sbtSpecConcTemp:
        begin
          Assert(PQTimeList.Count = 0);
          for TimeListIndex := 0 to FUTimeLists.Count - 1 do
          begin
            AUTimeList := FUTimeLists[TimeListIndex];
            for DataSetIndex := 0 to AUTimeList.Count - 1 do
            begin
              UDataSet := AUTimeList[DataSetIndex]
                as TTransientRealSparseDataSet;
              Assert(UDataSet <> nil);
              StartTimeIndex := Times.IndexOf(AUTimeList.Times[DataSetIndex]);
              if DataSetIndex < AUTimeList.Count - 1 then
              begin
                NextTimeIndex :=
                  Times.IndexOf(AUTimeList.Times[DataSetIndex + 1])-1;
              end
              else
              begin
                NextTimeIndex := Times.Count-1;
              end;

              for TimeIndex := StartTimeIndex to NextTimeIndex do
              begin
                MergedUDataSet := UTimeList[TimeIndex];
                for LayerIndex := UDataSet.MinLayer to UDataSet.MaxLayer do
                begin
                  for RowIndex := UDataSet.MinRow to UDataSet.MaxRow do
                  begin
                    for ColIndex := UDataSet.MinColumn to UDataSet.MaxColumn do
                    begin
                      if UDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        FNodeNumbers[LayerIndex, RowIndex, ColIndex] := 1;
                        MergedUDataSet.RealData[LayerIndex, RowIndex, ColIndex]
                          := UDataSet.RealData[LayerIndex, RowIndex, ColIndex];
                        MergedUDataSet.Annotation[LayerIndex, RowIndex,
                          ColIndex] := UDataSet.Annotation[LayerIndex, RowIndex,
                          ColIndex];
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  finally
    Times.Free;
  end;

  FCount := 0;
  if (FNodeNumbers.MinLayer >= 0) {and (Mesh.MeshType = mt3D)} then
  begin
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          if FNodeNumbers.IsValue[LayerIndex,RowIndex,ColIndex] then
          begin
            Assert(RowIndex = 0);
            if Mesh.MeshType in [mt2D, mtProfile] then
            begin
              Assert(LayerIndex = 0);
              ANode2D := Mesh.Mesh2D.Nodes[ColIndex];
              FNodeNumbers[LayerIndex,RowIndex,ColIndex] := ANode2D.Number;
              Inc(FCount);
            end
            else
            begin
              ANode := Mesh.NodeArray[LayerIndex, ColIndex];
              if ANode.Active then
              begin
                FNodeNumbers[LayerIndex,RowIndex,ColIndex] := ANode.Number;
                Inc(FCount);
              end
              else
              begin
                FNodeNumbers.RemoveValue(LayerIndex,RowIndex,ColIndex);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSutraBoundaryWriter.WriteDataSet0;
var
  Comment: string;
begin
  case FBoundaryType of
    sbtFluidSource: Comment := '# Fluid Flux Boundary Condition File';
    sbtMassEnergySource: Comment := '# Mass/Energy Source Boundary Condition File';
    sbtSpecPress: Comment := '# Specified Pressure Boundary Condition File';
    sbtSpecConcTemp: Comment := '# Specified Temperature/Concentration Boundary Condition File';
  end;
  WriteString(File_Comment(Comment));
  NewLine;
end;

procedure TSutraBoundaryWriter.WriteDataSet1;
var
  // may be up to 10 characters long
  BCSSCH: string;
  SimulationType: TSimulationType;
begin
  WriteCommentLine('Data set 1');
  SimulationType := Model.SutraOptions.SimulationType;

  case FBoundaryType of
    sbtFluidSource:
      begin
        case SimulationType of
          stSteadyFlowSteadyTransport, stSteadyFlowTransientTransport:
            begin
              BCSSCH := 'STEP_0';
            end;
            stTransientFlowTransientTransport:
            begin
              BCSSCH := KFluidFlux;
            end;
        else
          Assert(False);
        end;
      end;
    sbtMassEnergySource:
      begin
        case SimulationType of
          stSteadyFlowSteadyTransport:
            begin
              BCSSCH := 'STEP_1';
            end;
          stSteadyFlowTransientTransport,
            stTransientFlowTransientTransport:
            begin
              BCSSCH := KUFlux;
            end;
        else
          Assert(False);
        end;
      end;
    sbtSpecPress:
      begin
        case SimulationType of
          stSteadyFlowSteadyTransport, stSteadyFlowTransientTransport:
            begin
              BCSSCH := 'STEP_0';
            end;
            stTransientFlowTransientTransport:
            begin
              BCSSCH := KSpecifiedP;
            end;
        else
          Assert(False);
        end;
      end;
    sbtSpecConcTemp:
      begin
        case SimulationType of
          stSteadyFlowSteadyTransport:
            begin
              BCSSCH := 'STEP_1';
            end;
          stSteadyFlowTransientTransport,
            stTransientFlowTransientTransport:
            begin
              BCSSCH := KSpecifiedU;
            end;
        else
          Assert(False);
        end;
      end;
  end;
  WriteString('''' + BCSSCH + '''');
  WriteString(' # BCSSCH');
  NewLine;
end;

procedure TSutraBoundaryWriter.WriteDataSet2(TimeIndex: integer; PQTimeList,
  UTimeList: TSutraMergedTimeList);
var
  // May be up to 40 characters long and may include spaces.
  BCSID: string;
  NSOP1: Integer;
  NSOU1: Integer;
  NPBC1: Integer;
  NUBC1: Integer;
  NPBG1: Integer;
  NUBG1: Integer;
  PriorUDataArray: TRealSparseDataSet;
  UDataArray: TRealSparseDataSet;
  PriorPQDataArray: TRealSparseDataSet;
  PQDataArray: TRealSparseDataSet;
  LayerIndex: Integer;
  RowIndex: Integer;
  Count: Integer;
  ColIndex: Integer;
  StartLayer: Integer;
  StartRow: Integer;
  StartCol: Integer;
begin
  if TimeIndex < PQTimeList.Count then
  begin
    WriteCommentLine('Data set 2; Time = ' + FloatToStr(PQTimeList.Times[TimeIndex]));
  end
  else
  begin
    WriteCommentLine('Data set 2');
  end;

  NSOP1 := 0;
  NSOU1 := 0;
  NPBC1 := 0;
  NUBC1 := 0;
  NPBG1 := 0;
  NUBG1 := 0;
  if TimeIndex = 0 then
  begin
    Count := FCount;
  end
  else
  begin
    Count := 0;
    PriorUDataArray :=   UTimeList[TimeIndex-1] as TRealSparseDataSet;
    UDataArray :=   UTimeList[TimeIndex] as TRealSparseDataSet;
    if TimeIndex < PQTimeList.Count then
    begin
      PriorPQDataArray :=   PQTimeList[TimeIndex-1] as TRealSparseDataSet;
      PQDataArray :=   PQTimeList[TimeIndex] as TRealSparseDataSet;
    end
    else
    begin
      PriorPQDataArray := nil;
      PQDataArray := nil;
    end;
    if (PriorUDataArray.MinLayer >= 0) or (UDataArray.MinLayer >= 0) then
    begin
      if (PriorUDataArray.MinLayer >= 0) and (UDataArray.MinLayer >= 0) then
      begin
        StartLayer := Min(PriorUDataArray.MinLayer, UDataArray.MinLayer);
        StartRow := Min(PriorUDataArray.MinRow, UDataArray.MinRow);
        StartCol := Min(PriorUDataArray.MinColumn, UDataArray.MinColumn);
      end
      else if (PriorUDataArray.MinLayer >= 0) then
      begin
        StartLayer := PriorUDataArray.MinLayer;
        StartRow := PriorUDataArray.MinRow;
        StartCol := PriorUDataArray.MinColumn;
      end
      else
      begin
        StartLayer := UDataArray.MinLayer;
        StartRow := UDataArray.MinRow;
        StartCol := UDataArray.MinColumn;
      end;

      for LayerIndex := StartLayer to
        Max(PriorUDataArray.MaxLayer, UDataArray.MaxLayer) do
      begin
        for RowIndex := StartRow to
          Max(PriorUDataArray.MaxRow, UDataArray.MaxRow) do
        begin
          for ColIndex := StartCol to
            Max(PriorUDataArray.MaxColumn, UDataArray.MaxColumn) do
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex, ColIndex]
              <> PriorUDataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Inc(Count);
            end
            else if UDataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              if UDataArray.RealData[LayerIndex, RowIndex, ColIndex]
                <> PriorUDataArray.RealData[LayerIndex, RowIndex, ColIndex] then
              begin
                Inc(Count);
              end
              else
              begin
                if PQDataArray <> nil then
                begin
                  if PQDataArray.RealData[LayerIndex, RowIndex, ColIndex]
                    <> PriorPQDataArray.RealData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    Inc(Count);
                  end
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  case FBoundaryType of
    sbtFluidSource:
      begin
        BCSID := '''Fluid sources''';
        NSOP1 := Count;
      end;
    sbtMassEnergySource:
      begin
        BCSID := '''Mass/Energy sources''';
        NSOU1 := Count;
      end;
    sbtSpecPress:
      begin
        BCSID := '''Specified Pressure''';
        NPBC1 := Count;
      end;
    sbtSpecConcTemp:
      begin
        BCSID := '''Specified Temperature or Concentration''';
        NUBC1 := Count;
      end;
  end;

  WriteString(BCSID);
  WriteInteger(NSOP1);
  WriteInteger(NSOU1);
  WriteInteger(NPBC1);
  WriteInteger(NUBC1);
  if Model.ModelSelection <> msSutra22 then
  begin
    WriteInteger(NPBG1);
    WriteInteger(NUBG1);
  end;
  WriteString(' # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1');
  if Model.ModelSelection <> msSutra22 then
  begin
    WriteString(', NPBG1, NUBG1');
  end;
  NewLine;
end;

procedure TSutraBoundaryWriter.WriteDataSet3(TimeIndex: integer; PQTimeList,
  UTimeList: TSutraMergedTimeList);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  UDataArray: TDataArray;
  PQDataArray: TDataArray;
  IQCP1: NativeInt;
  QINC1: double;
  UINC1: double;
  Changed: Boolean;
  PriorUDataArray: TDataArray;
  PriorPQDataArray: TDataArray;
  AnyChanged: Boolean;
  UseBCTime: Boolean;
//  ActiveNodeDataArray: TDataArray;
  procedure WriteALine;
  begin
    AnyChanged := True;
    WriteInteger(IQCP1);
    if IQCP1 > 0 then
    begin
      WriteFloat(QINC1);
      WriteFloat(UINC1);
    end;
    WriteString(' # Data Set 3: IQCP1');
    if IQCP1 > 0 then
    begin
      WriteString(', QINC1, UINC1');
    end;
    NewLine;
  end;
begin
  if FBoundaryType <> sbtFluidSource then
  begin
    Exit;
  end;
  if TimeIndex < PQTimeList.Count then
  begin
    WriteCommentLine('Data set 3; Time = ' + FloatToStr(PQTimeList.Times[TimeIndex]));
  end
  else
  begin
    WriteCommentLine('Data set 3');
  end;

//  ActiveNodeDataArray := Model.DataArrayManager.GetDataSetByName(KNodeActive);
  UDataArray := UTimeList[TimeIndex];
  PQDataArray := PQTimeList[TimeIndex];
  if FNodeNumbers.MaxLayer < 0 then
  begin
    Exit;
  end;
  AnyChanged := False;
  if TimeIndex = 0 then
  begin
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          if FNodeNumbers.IsValue[LayerIndex, RowIndex,ColIndex] then
//            and (FNodeNumbers[LayerIndex, RowIndex,ColIndex] <> 0) then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              IQCP1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IQCP1 > 0);
              QINC1 := PQDataArray.RealData[LayerIndex, RowIndex,ColIndex];
              UINC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              Assert(not PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              IQCP1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IQCP1 < 0);
              QINC1 := 0.0;
              UINC1 := 0.0;
            end;
            WriteALine;
            if PQTimeList.Times[0] > FTime1 then
            begin
              QINC1 := 0.0;
              UINC1 := 0.0;
            end;
            if FUseBCTime.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              UseBCTime := FUseBCTime.Items[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              UseBCTime := False;
            end;
            FIBoundaryNodes.AddUnique(TBoundaryNode.Create(Abs(IQCP1), QINC1, UINC1, UseBCTime));
          end;
        end;
      end;
    end;
  end
  else
  begin
    PriorUDataArray := UTimeList[TimeIndex-1];
    PriorPQDataArray := PQTimeList[TimeIndex-1];
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          Changed := False;
          if PriorUDataArray.IsValue[LayerIndex, RowIndex,ColIndex]
            <> UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
          begin
            Changed := True;
          end
          else
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              Assert(PriorPQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              Changed := (UDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                <> PriorUDataArray.RealData[LayerIndex, RowIndex,ColIndex])
                or (PQDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                <> PriorPqDataArray.RealData[LayerIndex, RowIndex,ColIndex])
            end;
          end;
          if Changed then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              IQCP1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IQCP1 > 0);
              QINC1 := PQDataArray.RealData[LayerIndex, RowIndex,ColIndex];
              UINC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              Assert(not PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
              IQCP1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IQCP1 < 0);
              QINC1 := 0.0;
              UINC1 := 0.0;
            end;
            WriteALine;
          end;
        end;
      end;
    end;
  end;
  if AnyChanged then
  begin
    WriteInteger(0);
    NewLine;
  end;
end;

procedure TSutraBoundaryWriter.WriteDataSet4(TimeIndex: integer;
  UTimeList: TSutraMergedTimeList);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  UDataArray: TDataArray;
  IQCU1: NativeInt;
  QUINC1: double;
  Changed: Boolean;
  PriorUDataArray: TDataArray;
  AnyChanged: Boolean;
  UseBCTime: Boolean;
//  ActiveNodeDataArray: TDataArray;
  procedure WriteALine;
  begin
    AnyChanged := True;
    WriteInteger(IQCU1);
    if IQCU1 > 0 then
    begin
      WriteFloat(QUINC1);
    end;
    WriteString(' # Data Set 4: IQCU1');
    if IQCU1 > 0 then
    begin
      WriteString(', QUINC1');
    end;
    NewLine;
  end;
begin
  if FBoundaryType <> sbtMassEnergySource then
  begin
    Exit;
  end;
  if TimeIndex < UTimeList.Count then
  begin
    WriteCommentLine('Data set 4; Time = ' + FloatToStr(UTimeList.Times[TimeIndex]));
  end
  else
  begin
    WriteCommentLine('Data set 4');
  end;
//  WriteCommentLine('Data set 4');
  if FNodeNumbers.MaxLayer < 0 then
  begin
    Exit;
  end;

//  ActiveNodeDataArray := Model.DataArrayManager.GetDataSetByName(KNodeActive);
  UDataArray := UTimeList[TimeIndex];
  AnyChanged := False;
  if TimeIndex = 0 then
  begin
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          if FNodeNumbers.IsValue[LayerIndex, RowIndex,ColIndex] then
//            and (FNodeNumbers[LayerIndex, RowIndex,ColIndex] <> 0) then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              IQCU1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IQCU1 > 0);
              QUINC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              IQCU1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IQCU1 < 0);
              QUINC1 := 0.0;
            end;
            WriteALine;
            if UTimeList.Times[0] > FTime1 then
            begin
              QUINC1 := 0.0;
            end;
            if FUseBCTime.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              UseBCTime := FUseBCTime.Items[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              UseBCTime := False;
            end;
            FIBoundaryNodes.AddUnique(TBoundaryNode.Create(Abs(IQCU1), 0, QUINC1, UseBCTime));
          end;
        end;
      end;
    end;
  end
  else
  begin
    PriorUDataArray := UTimeList[TimeIndex-1];
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          Changed := False;
          if PriorUDataArray.IsValue[LayerIndex, RowIndex,ColIndex]
            <> UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
          begin
            Changed := True;
          end
          else
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Changed := (UDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                <> PriorUDataArray.RealData[LayerIndex, RowIndex,ColIndex])
            end;
          end;
          if Changed then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              IQCU1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IQCU1 > 0);
              QUINC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              IQCU1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IQCU1 < 0);
              QUINC1 := 0.0;
            end;
            WriteALine;
          end;
        end;
      end;
    end;
  end;
  if AnyChanged then
  begin
    WriteInteger(0);
    NewLine;
  end;
end;

procedure TSutraBoundaryWriter.WriteDataSet5(TimeIndex: integer; PQTimeList,
  UTimeList: TSutraMergedTimeList);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  UDataArray: TDataArray;
  PQDataArray: TDataArray;
  IPBC1: NativeInt;
  PBC1: double;
  UBC1: double;
  Changed: Boolean;
  PriorUDataArray: TDataArray;
  PriorPQDataArray: TDataArray;
  AnyChanged: Boolean;
  UseBCTime: Boolean;
//  ActiveNodeDataArray: TDataArray;
  procedure WriteALine;
  begin
    AnyChanged := True;
    WriteInteger(IPBC1);
    if IPBC1 > 0 then
    begin
      WriteFloat(PBC1);
      WriteFloat(UBC1);
    end;
    WriteString(' # Data Set 5: IPBC1');
    if IPBC1 > 0 then
    begin
      WriteString(', PBC1, UBC1');
    end;
    NewLine;
  end;
begin
  if FBoundaryType <> sbtSpecPress then
  begin
    Exit;
  end;
  if TimeIndex < PQTimeList.Count then
  begin
    WriteCommentLine('Data set 5; Time = ' + FloatToStr(PQTimeList.Times[TimeIndex]));
  end
  else
  begin
    WriteCommentLine('Data set 5');
  end;
//  WriteCommentLine('Data set 5');
  if FNodeNumbers.MaxLayer < 0 then
  begin
    Exit;
  end;
//  ActiveNodeDataArray := Model.DataArrayManager.GetDataSetByName(KNodeActive);
  UDataArray := UTimeList[TimeIndex];
  PQDataArray := PQTimeList[TimeIndex];
  AnyChanged := False;
  if (FNodeNumbers.MaxLayer >= 0) then
  begin
    if (TimeIndex = 0) then
    begin
      for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
      begin
        for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
        begin
          for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
          begin
            if FNodeNumbers.IsValue[LayerIndex, RowIndex,ColIndex] then
//              and (FNodeNumbers[LayerIndex, RowIndex,ColIndex] <> 0) then
            begin
              if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                IPBC1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
                Assert(IPBC1 > 0);
                PBC1 := PQDataArray.RealData[LayerIndex, RowIndex,ColIndex];
                UBC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
              end
              else
              begin
                Assert(not PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                IPBC1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
                Assert(IPBC1 < 0);
                PBC1 := 0.0;
                UBC1 := 0.0;
              end;
              WriteALine;
              if PQTimeList.Times[0] > FTime1 then
              begin
                PBC1 := 0.0;
                UBC1 := 0.0;
              end;
              if FUseBCTime.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                UseBCTime := FUseBCTime.Items[LayerIndex, RowIndex,ColIndex];
              end
              else
              begin
                UseBCTime := False;
              end;
              FIBoundaryNodes.AddUnique(TBoundaryNode.Create(Abs(IPBC1), PBC1, UBC1, UseBCTime));
            end;
          end;
        end;
      end;
    end
    else
    begin
      PriorUDataArray := UTimeList[TimeIndex-1];
      PriorPQDataArray := PQTimeList[TimeIndex-1];
      for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
      begin
        for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
        begin
          for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
          begin
            Changed := False;
            if PriorUDataArray.IsValue[LayerIndex, RowIndex,ColIndex]
              <> UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Changed := True;
            end
            else
            begin
              if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                Assert(PriorPQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                Changed := (UDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                  <> PriorUDataArray.RealData[LayerIndex, RowIndex,ColIndex])
                  or (PQDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                  <> PriorPqDataArray.RealData[LayerIndex, RowIndex,ColIndex])
              end;
            end;
            if Changed then
            begin
              if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                Assert(PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                IPBC1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
                Assert(IPBC1 > 0);
                PBC1 := PQDataArray.RealData[LayerIndex, RowIndex,ColIndex];
                UBC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
              end
              else
              begin
                Assert(not PQDataArray.IsValue[LayerIndex, RowIndex,ColIndex]);
                IPBC1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
                Assert(IPBC1 < 0);
                PBC1 := 0.0;
                UBC1 := 0.0;
              end;
              WriteALine;
            end;
          end;
        end;
      end;
    end;
  end;
  if AnyChanged then
  begin
    WriteInteger(0);
    NewLine;
  end;
end;

procedure TSutraBoundaryWriter.WriteDataSet6(TimeIndex: integer;
  UTimeList: TSutraMergedTimeList);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  UDataArray: TDataArray;
  IUBC1: NativeInt;
  UBC1: double;
  Changed: Boolean;
  PriorUDataArray: TDataArray;
  AnyChanged: Boolean;
  UseBCTime: Boolean;
//  ActiveNodeDataArray: TDataArray;
  procedure WriteALine;
  begin
    AnyChanged := True;
    WriteInteger(IUBC1);
    if IUBC1 > 0 then
    begin
      WriteFloat(UBC1);
    end;
    WriteString(' # Data Set 6: IUBC1');
    if IUBC1 > 0 then
    begin
      WriteString(', UBC1');
    end;
    NewLine;
  end;
begin
  if FBoundaryType <> sbtSpecConcTemp then
  begin
    Exit;
  end;
  if TimeIndex < UTimeList.Count then
  begin
    WriteCommentLine('Data set 6; Time = ' + FloatToStr(UTimeList.Times[TimeIndex]));
  end
  else
  begin
    WriteCommentLine('Data set 6');
  end;
  if FNodeNumbers.MaxLayer < 0 then
  begin
    Exit;
  end;
//  ActiveNodeDataArray := Model.DataArrayManager.GetDataSetByName(KNodeActive);
  UDataArray := UTimeList[TimeIndex];
  AnyChanged := False;
  if TimeIndex = 0 then
  begin
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          if FNodeNumbers.IsValue[LayerIndex, RowIndex,ColIndex] then
//            and (FNodeNumbers[LayerIndex, RowIndex,ColIndex] <> 0) then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              IUBC1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IUBC1 > 0);
              UBC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              IUBC1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IUBC1 < 0);
              UBC1 := 0.0;
            end;
            WriteALine;
            if UTimeList.Times[0] > FTime1 then
            begin
              UBC1 := 0.0;
            end;
              if FUseBCTime.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                UseBCTime := FUseBCTime.Items[LayerIndex, RowIndex,ColIndex];
              end
              else
              begin
                UseBCTime := False;
              end;
            FIBoundaryNodes.AddUnique(TBoundaryNode.Create(Abs(IUBC1), 0, UBC1, UseBCTime));
          end;
        end;
      end;
    end;
  end
  else
  begin
    PriorUDataArray := UTimeList[TimeIndex-1];
    for LayerIndex := FNodeNumbers.MinLayer to FNodeNumbers.MaxLayer do
    begin
      for RowIndex := FNodeNumbers.MinRow to FNodeNumbers.MaxRow do
      begin
        for ColIndex := FNodeNumbers.MinCol to FNodeNumbers.MaxCol do
        begin
          Changed := False;
          if PriorUDataArray.IsValue[LayerIndex, RowIndex,ColIndex]
            <> UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
          begin
            Changed := True;
          end
          else
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              Changed := (UDataArray.RealData[LayerIndex, RowIndex,ColIndex]
                <> PriorUDataArray.RealData[LayerIndex, RowIndex,ColIndex])
            end;
          end;
          if Changed then
          begin
            if UDataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
            begin
              IUBC1 := FNodeNumbers[LayerIndex, RowIndex,ColIndex] + 1;
              Assert(IUBC1 > 0);
              UBC1 := UDataArray.RealData[LayerIndex, RowIndex,ColIndex];
            end
            else
            begin
              IUBC1 := -FNodeNumbers[LayerIndex, RowIndex,ColIndex] - 1;
              Assert(IUBC1 < 0);
              UBC1 := 0.0;
            end;
            WriteALine;
          end;
        end;
      end;
    end;
  end;
  if AnyChanged then
  begin
    WriteInteger(0);
    NewLine;
  end;
end;

procedure TSutraBoundaryWriter.WriteFile(FileName: string;
  BoundaryNodes: IBoundaryNodes; BcsFileNames: TLakeInteractionStringList);
var
  UTimeList: TSutraMergedTimeList;
  PQTimeList: TSutraMergedTimeList;
  TimeIndex: Integer;
  SimulationType: TSimulationType;
  FirstTimeSpecified: Boolean;
  InitialTime: double;
  LakeExtension: string;
//  LakeInteraction: TLakeBoundaryInteraction;
  FileRoot: string;
begin
  FBcsFileNames := BcsFileNames;
  FIBoundaryNodes := BoundaryNodes;
//  FIBoundaryNodes.Clear;

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
  end
  else
  begin
    LakeExtension := '';
  end;
  FileRoot := ChangeFileExt(FileName, '');
  FileName := ChangeFileExt(FileName, LakeExtension);

  case FBoundaryType of
    sbtFluidSource: FileName := FileName + '.FluxBcs';
    sbtMassEnergySource: FileName := FileName + '.UFluxBcs';
    sbtSpecPress: FileName := FileName + '.SPecPBcs';
    sbtSpecConcTemp: FileName := FileName + '.SPecUBcs';
    else Assert(False);
  end;

  UTimeList := TSutraMergedTimeList.Create(Model);
  PQTimeList := TSutraMergedTimeList.Create(Model);
  try
    // UpdateMergeLists calls Evaluate.
    UpdateMergeLists(PQTimeList, UTimeList);

    if (PQTimeList.Count > 0) or (UTimeList.Count > 0) then
    begin
      SimulationType := Model.SutraOptions.SimulationType;
      if (FBoundaryType in [sbtFluidSource, sbtSpecPress])
        and (SimulationType = stSteadyFlowSteadyTransport) then
      begin
        FirstTimeSpecified := False;
        InitialTime := (Model as TPhastModel).SutraTimeOptions.InitialTime;
        if (PQTimeList.Count > 0) and (PQTimeList.Times[0] = InitialTime) then
        begin
          FirstTimeSpecified := True;
        end
        else if (UTimeList.Count > 0) and (UTimeList.Times[0] = InitialTime) then
        begin
          FirstTimeSpecified := True;
        end;
        if not FirstTimeSpecified then
        begin
          PQTimeList.Clear;
          UTimeList.Clear;
          if BcsFileNames <> nil then
          begin
            BcsFileNames.Add('');
          end;
          Exit;
        end;
      end;

      OpenFile(FileName);
      try
        if (BcsFileNames <> nil) then
        begin
          if (BcsFileNames.LakeInteraction <> lbiUseDefaults) then
          begin
            BcsFileNames.Add(FileName);
          end
          else
          begin
            BcsFileNames.Add('');
          end;
        end;
        WriteDataSet0;
        WriteDataSet1;
        for TimeIndex := 0 to UTimeList.Count - 1 do
        begin
          WriteDataSet2(TimeIndex, PQTimeList, UTimeList);
          WriteDataSet3(TimeIndex, PQTimeList, UTimeList);
          WriteDataSet4(TimeIndex, UTimeList);
          WriteDataSet5(TimeIndex, PQTimeList, UTimeList);
          WriteDataSet6(TimeIndex, UTimeList);
        end;
        SutraFileWriter.AddBoundaryFile(FileName);
        case FBoundaryType of
          sbtFluidSource:
            begin
              SutraFileWriter.AddFile(sftBcof, ChangeFileExt(FileRoot, '.bcof'));
            end;
          sbtMassEnergySource:
            begin
              SutraFileWriter.AddFile(sftBcos, ChangeFileExt(FileRoot, '.bcos'));
            end;
          sbtSpecPress:
            begin
              SutraFileWriter.AddFile(sftBcop, ChangeFileExt(FileRoot, '.bcop'));
            end;
          sbtSpecConcTemp:
            begin
              SutraFileWriter.AddFile(sftBcou, ChangeFileExt(FileRoot
              , '.bcou'));
            end;
        end;
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
  finally
    PQTimeList.Free;
    UTimeList.Free;
  end;
end;

{ TSutraFluxCheckList }

procedure TSutraFluxCheckList.CheckSameModel(const Data: TDataArray);
begin
  if (Data <> nil) and (Model <> nil) then
  begin
    Assert(Model = Data.Model);
  end;
end;

procedure TSutraFluxCheckList.Initialize;
begin
  // do nothig.
end;

{ TBoundaryNodes }

procedure TBoundaryNodes.AddUnique(Node: TBoundaryNode);
begin
  if not ContainsKey(Node.NodeNumber) then
  begin
    Add(Node.NodeNumber, Node);
  end;

end;

function TBoundaryNodes.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TBoundaryNodes.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TBoundaryNodes.ToArray: TArray<TPair<Integer, TBoundaryNode>>;
type
  TNodeComparer = TComparer<TPair<Integer, TBoundaryNode>>;
var
  Comparer: IComparer<TPair<Integer, TBoundaryNode>>;
begin
  result := inherited;
  // sort the nodes in ascending order.
  Comparer := TNodeComparer.Construct(
    function(const L, R: TPair<Integer, TBoundaryNode>): Integer
    begin
      result := L.Key - R.Key;
    end
    );
  TArray.Sort<TPair<Integer, TBoundaryNode>>(Result, Comparer );
end;

function TBoundaryNodes._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TBoundaryNodes._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TLakeInteractionStringList }

constructor TLakeInteractionStringList.Create;
begin
  inherited;
  FLakeInteraction := lbiUseDefaults;
end;

procedure TLakeInteractionStringList.SetLakeInteraction(
  const Value: TLakeBoundaryInteraction);
begin
  FLakeInteraction := Value;
end;

end.
