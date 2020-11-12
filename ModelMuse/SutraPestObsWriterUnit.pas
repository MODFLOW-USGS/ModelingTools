unit SutraPestObsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, System.SysUtils, ScreenObjectUnit,
  System.Classes, SutraPestObsUnit, System.Generics.Collections;

type
  TExportType = (etInstructions, etExtractedValues);

  TObservationNodeStartDictionary = TDictionary<TScreenObject,Integer>;
  TLineIndexDictionary = TDictionary<Integer,Integer>;

  TSutraPestObsWriterWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    FSutraLakeObjects: TScreenObjectList;
    FSutraObsObjects: TScreenObjectList;
    FSutraAllStateObsObjects: TScreenObjectList;
    FDerivedObsList: TStringList;
    FExportType: TExportType;
    FSutraFluxObs: TSutraFluxObs;
    procedure Evaluate;
    procedure WriteOptions;
    procedure WriteObservationsFileNames;
    procedure WriteIdentifiers;
    procedure WriteDerivedObservations;
    procedure WriteObcIdentifiers;
    procedure WriteLakeIdentifiers;
    procedure WriteSpecPresIdentifiers;
    procedure WriteSpecFlowIdentifiers;
    procedure WriteSpecConcIdentifiers;
    procedure WriteGenFlowIdentifiers;
    procedure WriteGenTransportIdentifiers;
    function CellAssignmentToNodeNumber(ACell: TCellAssignment): Integer;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

const
  StrSoei = '.soe_i';
  StrSoeev = '.soe_ev';

implementation

uses
  GoPhastTypes, SutraMeshUnit, FastGEO, ModelMuseUtilities,
  PestObsUnit, ObservationComparisonsUnit, frmErrorsAndWarningsUnit,
  SutraBoundariesUnit, FluxObservationUnit, RealListUnit, ModflowCellUnit,
  SutraOptionsUnit, SutraGeneralBoundaryUnit, SutraBoundaryUnit,
  SutraGenTransBoundUnit, IntListUnit, ObsInterfaceUnit;

resourcestring
  StrTheObservationComp = 'The observation comparison item "%s" could not be' +
  ' exported. Check that it is defined correctly for this model.';
  StrUnableToExportObs = 'Unable to export observations';

  { TSutraPestObsWriterWriter }

constructor TSutraPestObsWriterWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FSutraLakeObjects := TScreenObjectList.Create;
  FSutraObsObjects := TScreenObjectList.Create;
  FSutraAllStateObsObjects := TScreenObjectList.Create;

//  FSutraSpecPresObsObjects := TScreenObjectList.Create;
//  FSutraFluidFlowObsObjects := TScreenObjectList.Create;
//  FSutraSpecUObsObjects := TScreenObjectList.Create;
//  FSutraGenPresObsObjects := TScreenObjectList.Create;
//  FSutraGenPresTransObjects := TScreenObjectList.Create;

  FDerivedObsList := TStringList.Create;
end;

destructor TSutraPestObsWriterWriter.Destroy;
begin
  FDerivedObsList.Free;

//  FSutraGenPresTransObjects.Free;
//  FSutraGenPresObsObjects.Free;
//  FSutraSpecUObsObjects.Free;
//  FSutraFluidFlowObsObjects.Free;
//  FSutraSpecPresObsObjects.Free;

  FSutraAllStateObsObjects.Free;
  FSutraLakeObjects.Free;
  FSutraObsObjects.Free;
  inherited;
end;

procedure TSutraPestObsWriterWriter.Evaluate;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SutraStateObs: TSutraStateObservations;
  SutraBoundaries: TSutraBoundaries;
begin
  for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    SutraBoundaries := AScreenObject.SutraBoundaries;
    SutraStateObs := SutraBoundaries.SutraStateObs;
//    AScreenObject.SutraBoundaries.SutraStateObs.Used;
    if SutraStateObs.Used then
    begin
      if SutraStateObs.HasNonLakeBoundary then
      begin
        FSutraObsObjects.Add(AScreenObject)
      end;
      if SutraStateObs.HasLakeBoundary then
      begin
        FSutraLakeObjects.Add(AScreenObject)
      end;
      FSutraAllStateObsObjects.Add(AScreenObject);
    end;

    FSutraFluxObs := Model.SutraFluxObs;
  end;
end;

class function TSutraPestObsWriterWriter.Extension: string;
begin
  Result := '.soe';
  Assert(False);
end;

procedure TSutraPestObsWriterWriter.WriteDerivedObservations;
var
  LineIndex: Integer;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SutraStateObs: TSutraStateObservations;
  DerivedObsIndex: Integer;
  CompItem: TObsCompareItem;
  Item: TSutraStateObsItem;
  SutraComparisons: TGlobalObservationComparisons;
  ItemIndex: Integer;
//  GlobalCompItem: TGlobalObsComparisonItem;
  ObservationList: TObservationList;
  FObsItemDictionary: TObsItemDictionary;
  ObsItem: TCustomObservationItem;
  GloCompItem: TGlobalObsComparisonItem;
  PriorItem1: IObservationItem;
  PriorItem2: IObservationItem;
  ErrorMessage: string;
  GroupIndex: Integer;
  SpecPresGroup: TSutraSpecPressureObservations;
  SpecPresItem: TSutraSpecPressObsItem;
  FluidFlowGroup: TSutraFluidFlowObservations;
  FluidFlowItem: TSutraFluidFlowObsItem;
  SpecConcGroup: TSutraSpecConcObservations;
  SpecConcItem: TSutraSpecConcObsItem;
  GenFlowGroup: TSutraGenPressObservations;
  GenFlowItem: TSutraSpecPressObsItem;
  GenTransGroup: TSutraGenTransObservations;
  GenTransItem: TSutraGenTransObsItem;
//  SpecPresGroup: TSutraSpecPressureObservationGroup;
begin
  WriteString('BEGIN DERIVED_OBSERVATIONS');
  NewLine;
  for LineIndex := 0 to FDerivedObsList.Count - 1 do
  begin
    WriteString(FDerivedObsList[LineIndex]);
    NewLine;
  end;

  for ObjectIndex := 0 to FSutraAllStateObsObjects.Count - 1 do
  begin
    AScreenObject := FSutraAllStateObsObjects[ObjectIndex];
    SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;

    for DerivedObsIndex := 0 to SutraStateObs.Comparisons.Count - 1 do
    begin
      CompItem := SutraStateObs.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      Item := SutraStateObs[CompItem.Index1];
      WriteString(Item.ExportedName);
      WriteString(' - ');
      Item := SutraStateObs[CompItem.Index2];
      WriteString(Item.ExportedName);
      NewLine;
    end;
  end;

  for GroupIndex := 0 to FSutraFluxObs.SpecPres.Count - 1 do
  begin
    SpecPresGroup := FSutraFluxObs.SpecPres[GroupIndex].ObsGroup;
    for DerivedObsIndex := 0 to SpecPresGroup.Comparisons.Count - 1 do
    begin
      CompItem := SpecPresGroup.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      SpecPresItem := SpecPresGroup[CompItem.Index1];
      WriteString(SpecPresItem.Name);
      WriteString(' - ');
      SpecPresItem := SpecPresGroup[CompItem.Index2];
      WriteString(SpecPresItem.Name);
      NewLine;
    end;
  end;

  for GroupIndex := 0 to FSutraFluxObs.FluidFlow.Count - 1 do
  begin
    FluidFlowGroup := FSutraFluxObs.FluidFlow[GroupIndex].ObsGroup;
    for DerivedObsIndex := 0 to FluidFlowGroup.Comparisons.Count - 1 do
    begin
      CompItem := FluidFlowGroup.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      FluidFlowItem := FluidFlowGroup[CompItem.Index1];
      WriteString(FluidFlowItem.Name);
      WriteString(' - ');
      FluidFlowItem := FluidFlowGroup[CompItem.Index2];
      WriteString(FluidFlowItem.Name);
      NewLine;
    end;
  end;

  for GroupIndex := 0 to FSutraFluxObs.SpecConc.Count - 1 do
  begin
    SpecConcGroup := FSutraFluxObs.SpecConc[GroupIndex].ObsGroup;
    for DerivedObsIndex := 0 to SpecConcGroup.Comparisons.Count - 1 do
    begin
      CompItem := SpecConcGroup.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      SpecConcItem := SpecConcGroup[CompItem.Index1];
      WriteString(SpecConcItem.Name);
      WriteString(' - ');
      SpecConcItem := SpecConcGroup[CompItem.Index2];
      WriteString(SpecConcItem.Name);
      NewLine;
    end;
  end;

  for GroupIndex := 0 to FSutraFluxObs.GenFlow.Count - 1 do
  begin
    GenFlowGroup := FSutraFluxObs.GenFlow[GroupIndex].ObsGroup;
    for DerivedObsIndex := 0 to GenFlowGroup.Comparisons.Count - 1 do
    begin
      CompItem := GenFlowGroup.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      GenFlowItem := GenFlowGroup[CompItem.Index1];
      WriteString(GenFlowItem.Name);
      WriteString(' - ');
      GenFlowItem := GenFlowGroup[CompItem.Index2];
      WriteString(GenFlowItem.Name);
      NewLine;
    end;
  end;

  for GroupIndex := 0 to FSutraFluxObs.GenTrans.Count - 1 do
  begin
    GenTransGroup := FSutraFluxObs.GenTrans[GroupIndex].ObsGroup;
    for DerivedObsIndex := 0 to GenTransGroup.Comparisons.Count - 1 do
    begin
      CompItem := GenTransGroup.Comparisons[DerivedObsIndex];
      WriteString('  OBSNAME ');
      WriteString(CompItem.Name);
      WriteString(' PRINT');
      NewLine;
      WriteString('    FORMULA ');
      GenTransItem := GenTransGroup[CompItem.Index1];
      WriteString(GenTransItem.Name);
      WriteString(' - ');
      GenTransItem := GenTransGroup[CompItem.Index2];
      WriteString(GenTransItem.Name);
      NewLine;
    end;
  end;

  ObservationList := TObservationList.Create;
  FObsItemDictionary := TObsItemDictionary.Create;
  try
    Model.FillObsItemList(ObservationList);

    if ObservationList.Count > 0 then
    begin

      for ItemIndex := 0 to ObservationList.Count - 1 do
      begin
        ObsItem := ObservationList[ItemIndex];
        FObsItemDictionary.Add(ObsItem.GUID, ObsItem);
      end;

      SutraComparisons := Model.SutraGlobalObservationComparisons;
      for ItemIndex := 0 to SutraComparisons.Count - 1 do
      begin
        GloCompItem := SutraComparisons[ItemIndex];
        if FObsItemDictionary.TryGetValue(GloCompItem.GUID1, PriorItem1)
          and FObsItemDictionary.TryGetValue(GloCompItem.GUID2, PriorItem2) then
        begin
          WriteString('  OBSNAME ');
          WriteString(GloCompItem.Name);
          WriteString(' PRINT');
          NewLine;
          WriteString('    FORMULA ');
          WriteString((PriorItem1 as TCustomObservationItem).ExportedName);
          WriteString(' - ');
          WriteString((PriorItem2 as TCustomObservationItem).ExportedName);
          NewLine;
        end
        else
        begin
          ErrorMessage := Format(StrTheObservationComp, [GloCompItem.Name]);
          frmErrorsAndWarnings.AddWarning(Model, StrUnableToExportObs, ErrorMessage)
        end;
      end;
    end;
  finally
    ObservationList.Free;
    FObsItemDictionary.Free;
  end;

  WriteString('END DERIVED_OBSERVATIONS');
  NewLine;
end;

procedure TSutraPestObsWriterWriter.WriteFile(const AFileName: string);
begin
  if not Model.PestUsed then
  begin
    Exit;
  end;
  Evaluate;

  FExportType := etInstructions;
  FFileName := ChangeFileExt(AFileName, StrSoei);
//  FInputFileName := FFileName;
  OpenFile(FFileName);
  try
    WriteOptions;
    WriteObservationsFileNames;
    WriteIdentifiers;
    WriteDerivedObservations;
  finally
    CloseFile;
  end;

  FExportType := etExtractedValues;
  FFileName := ChangeFileExt(AFileName, StrSoeev);
  OpenFile(FFileName);
  try
    WriteOptions;
    WriteObservationsFileNames;
    WriteIdentifiers;
    WriteDerivedObservations;
  finally
    CloseFile;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteGenFlowIdentifiers;
var
  CellLists: TObjectList<TCellAssignmentList>;
  FactorsValuesList: TObjectList<TRealList>;
  CellLocationList: TCellLocationList;
  FlowBuilder: TStringBuilder;
  ResultantBuilder: TStringBuilder;
  Mesh3D: TSutraMesh3D;
  GroupIndex: Integer;
  GenFlowGroup: TSutraGenPressureObservationGroup;
  ObservationFactors: TObservationFactors;
  ObjectIndex: Integer;
  Formula: string;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  FactorsValues: TRealList;
  FactorAnnotation: string;
  DataIdentifier: string;
  ListIndex: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
//  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  GenFlowObs: TSutraSpecPressObsItem;
  AFactor: Double;
  FlowFormula: string;
  ResultantFormula: string;
  DerivedFormula: string;
  StartDictionary: TObservationNodeStartDictionary;
  LakeInteraction: TLakeBoundaryInteraction;
  GenLakeInteractionType: TGeneralizedFlowInteractionType;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraGeneralFlowBoundary;
  NodeStart: Integer;
  NodeStarts: TList<Integer>;
  procedure UpdateNodeStart(AScreenObject: TScreenObject);
  var
    CellList: TCellAssignmentList;
  begin
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      StartDictionary.Add(AScreenObject, NodeStart);
      NodeStart := NodeStart + CellList.Count;
    finally
      CellList.Free;
    end;
  end;
begin
  NodeStarts := TList<Integer>.Create;
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
  FlowBuilder := TStringBuilder.Create;
  ResultantBuilder := TStringBuilder.Create;
  StartDictionary := TObservationNodeStartDictionary.Create;
  try
    // The position of each node in the main SUTRA input file will
    // be used as the ID so it is important to replicate the order
    // that they are added to the input file as determined in
    // TfrmGoPhast.ExportSutra.
    NodeStart := 1;
    if not Model.SutraLakesUsed then
    begin
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
          UpdateNodeStart(ScreenObject);
        end;
      end;
    end
    else
    begin
      for LakeInteraction := Low(TLakeBoundaryInteraction)
        to High(TLakeBoundaryInteraction) do
      begin
        for GenLakeInteractionType := Low(TGeneralizedFlowInteractionType)
          to High(TGeneralizedFlowInteractionType) do
        begin
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
              if ((LakeInteraction = ABoundary.LakeInteraction)
                and (GenLakeInteractionType = ABoundary.LakeInteractionType))
                then
              begin
                UpdateNodeStart(ScreenObject);
              end;
            end;
          end;
        end;
      end;
    end;


    Mesh3D := Model.SutraMesh;
    for GroupIndex := 0 to FSutraFluxObs.GenFlow.Count - 1 do
    begin
      CellLists.Clear;
      NodeStarts.Clear;
      FactorsValuesList.Clear;
      GenFlowGroup := FSutraFluxObs.GenFlow[GroupIndex];
      ObservationFactors := GenFlowGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Formula := ObservationFactors[ObjectIndex].Factor;
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if not StartDictionary.TryGetValue(AScreenObject, NodeStart) then
        begin
          Continue;
        end;
        NodeStarts.Add(NodeStart);

        CellList := TCellAssignmentList.Create;
        CellLists.Add(CellList);
        FactorsValues := TRealList.Create;
        FactorsValuesList.Add(FactorsValues);
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        CellList.AssignCellLocationList(CellLocationList);
        AScreenObject.AssignValuesWithCellList(Formula, Model, CellLocationList,
          FactorsValues, FactorAnnotation, DataIdentifier);
      end;
      Assert(CellLists.Count = NodeStarts.Count);
      Assert(CellLists.Count = FactorsValuesList.Count);
      for ListIndex := 0 to CellLists.Count - 1 do
      begin
        CellList := CellLists[ListIndex];
        NodeStart := NodeStarts[ListIndex];
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          NodeNumber := CellAssignmentToNodeNumber(ACell);
//          ID := IntToStr(NodeNumber);
          ID := IntToStr(NodeStart + CellIndex);

          if GenFlowGroup.ObsGroup.HasObsIndex(0)
            or GenFlowGroup.ObsGroup.HasObsIndex(1) then
          begin
            WriteString('  ID ');
            WriteString(ID);
            WriteString(' PGF');
            WriteInteger(NodeNumber);
            NewLine;
            for ObsIndex := 0 to GenFlowGroup.ObsGroup.Count - 1 do
            begin
              GenFlowObs := GenFlowGroup.ObsGroup[ObsIndex];
              if GenFlowObs.ObsTypeIndex in [0,1] then
              begin
                WriteString('    OBSNAME ');
                WriteString('PGF');
                WriteString(ID);
                WriteString('_');
                WriteString(IntToStr(ObsIndex));
                WriteString('_');
                WriteString(IntToStr(NodeNumber));
                WriteFloat(GenFlowObs.Time);
                NewLine;
              end;
            end;
          end;

          if GenFlowGroup.ObsGroup.HasObsIndex(1)
            or GenFlowGroup.ObsGroup.HasObsIndex(2) then
          begin
            WriteString('  ID ');
            WriteString(ID);
            WriteString(' PGR');
            WriteInteger(NodeNumber);
            NewLine;
            for ObsIndex := 0 to GenFlowGroup.ObsGroup.Count - 1 do
            begin
              GenFlowObs := GenFlowGroup.ObsGroup[ObsIndex];
              if GenFlowObs.ObsTypeIndex in [1,2] then
              begin
                WriteString('    OBSNAME ');
                WriteString('PGR');
                WriteString(ID);
                WriteString('_');
                WriteString(IntToStr(ObsIndex));
                WriteString('_');
                WriteString(IntToStr(NodeNumber));
                WriteFloat(GenFlowObs.Time);
                NewLine;
              end;
            end;
          end;

        end;
      end;

      for ObsIndex := 0 to GenFlowGroup.ObsGroup.Count - 1 do
      begin
        GenFlowObs := GenFlowGroup.ObsGroup[ObsIndex];

        FlowBuilder.Clear;
        ResultantBuilder.Clear;
        for ListIndex := 0 to CellLists.Count - 1 do
        begin
          NodeStart := NodeStarts[ListIndex];
          CellList := CellLists[ListIndex];
          FactorsValues := FactorsValuesList[ListIndex];
          Assert(CellList.Count = FactorsValues.Count);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            AFactor := FactorsValues[CellIndex];

            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
//            ID := IntToStr(NodeNumber);
            ID := IntToStr(CellIndex + NodeStart);

            if (ListIndex > 0) or (CellIndex > 0) then
            begin
              FlowBuilder.Append(' + ');
              ResultantBuilder.Append(' + ');
            end;
            if AFactor <> 1 then
            begin
              FlowBuilder.Append(AFactor);
              ResultantBuilder.Append(AFactor);

              FlowBuilder.Append('*');
              ResultantBuilder.Append('*');
            end;

            FlowBuilder.Append('PGF');
            ResultantBuilder.Append('PGR');

            FlowBuilder.Append(ID);
            ResultantBuilder.Append(ID);

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(ObsIndex);
            ResultantBuilder.Append(ObsIndex);

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(NodeNumber);
            ResultantBuilder.Append(NodeNumber);
          end;
        end;
        FlowFormula := FlowBuilder.ToString;
        ResultantFormula := ResultantBuilder.ToString;

        case GenFlowObs.ObsTypeIndex of
          0:
            begin
              DerivedFormula := FlowFormula;
            end;
          1:
            begin
              DerivedFormula := '(' + ResultantFormula + ')/(' + FlowFormula + ')';
            end;
          2:
            begin
              DerivedFormula := ResultantFormula;
            end;
          else
            begin
              Assert(False);
            end;
        end;

        FDerivedObsList.Add('  OBSNAME ' + GenFlowObs.Name + ' PRINT');
        FDerivedObsList.Add('    FORMULA ' + DerivedFormula);
      end;

    end;
  finally
    StartDictionary.Free;
    ResultantBuilder.Free;
    FlowBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
    NodeStarts.Free;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteGenTransportIdentifiers;
var
  CellLists: TObjectList<TCellAssignmentList>;
  FactorsValuesList: TObjectList<TRealList>;
  CellLocationList: TCellLocationList;
  ResultantBuilder: TStringBuilder;
  Mesh3D: TSutraMesh3D;
  GroupIndex: Integer;
  GenTransGroup: TSutraGenTransObservationGroup;
  ObservationFactors: TObservationFactors;
  ObjectIndex: Integer;
  Formula: string;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  FactorsValues: TRealList;
  FactorAnnotation: string;
  DataIdentifier: string;
  ListIndex: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  GenTransObs: TSutraGenTransObsItem;
  AFactor: Double;
  ResultantFormula: string;
  DerivedFormula: string;
  LakeInteraction: TLakeBoundaryInteraction;
  GenLakeTransInteractionType: TGeneralizedTransportInteractionType;
  StartDictionary: TObservationNodeStartDictionary;
  NodeStart: Integer;
  NodeStarts: TList<Integer>;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraGeneralTransportBoundary;
  procedure UpdateNodeStart(AScreenObject: TScreenObject);
  var
    CellList: TCellAssignmentList;
  begin
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      StartDictionary.Add(AScreenObject, NodeStart);
      NodeStart := NodeStart + CellList.Count;
    finally
      CellList.Free;
    end;
  end;
begin
  StartDictionary := TObservationNodeStartDictionary.Create;
  NodeStarts := TList<Integer>.Create;
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
//  FlowBuilder := TStringBuilder.Create;
  ResultantBuilder := TStringBuilder.Create;
  try
    NodeStart := 1;
    if not Model.SutraLakesUsed then
    begin
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
          UpdateNodeStart(ScreenObject);
        end;
      end;
    end
    else
    begin
      for LakeInteraction := Low(TLakeBoundaryInteraction)
        to High(TLakeBoundaryInteraction) do
      begin
        for GenLakeTransInteractionType := Low(TGeneralizedTransportInteractionType)
          to High(TGeneralizedTransportInteractionType) do
        begin
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
              if ((LakeInteraction = ABoundary.LakeInteraction)
                and (GenLakeTransInteractionType = ABoundary.LakeInteractionType))
                then
              begin
                UpdateNodeStart(ScreenObject);
              end;
            end;
          end;
        end;
      end;
    end;

    Mesh3D := Model.SutraMesh;
    for GroupIndex := 0 to FSutraFluxObs.GenTrans.Count - 1 do
    begin
      CellLists.Clear;
      NodeStarts.Clear;
      FactorsValuesList.Clear;
      GenTransGroup := FSutraFluxObs.GenTrans[GroupIndex];
      ObservationFactors := GenTransGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Formula := ObservationFactors[ObjectIndex].Factor;
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if not StartDictionary.TryGetValue(AScreenObject, NodeStart) then
        begin
          Continue;
        end;
        NodeStarts.Add(NodeStart);

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        CellLists.Add(CellList);
        FactorsValues := TRealList.Create;
        FactorsValuesList.Add(FactorsValues);
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        CellList.AssignCellLocationList(CellLocationList);
        AScreenObject.AssignValuesWithCellList(Formula, Model, CellLocationList,
          FactorsValues, FactorAnnotation, DataIdentifier);
      end;
      Assert(CellLists.Count = NodeStarts.Count);
      Assert(CellLists.Count = FactorsValuesList.Count);
      for ListIndex := 0 to CellLists.Count - 1 do
      begin
        NodeStart := NodeStarts[ListIndex];
        CellList := CellLists[ListIndex];
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
          NodeNumber := Node3D.Number + 1;
//          ID := IntToStr(NodeNumber);
          ID := IntToStr(NodeStart + CellIndex);

//          if GenTransGroup.ObsGroup.HasObsIndex(0) then
          begin
            WriteString('  ID ');
            WriteString(ID);
            WriteString(' UGR');
            WriteInteger(NodeNumber);
            NewLine;
            for ObsIndex := 0 to GenTransGroup.ObsGroup.Count - 1 do
            begin
              GenTransObs := GenTransGroup.ObsGroup[ObsIndex];
//              if GenTransObs.ObsTypeIndex = 0 then
              begin
                WriteString('    OBSNAME ');
                WriteString('UGR');
                WriteString(ID);
                WriteString('_');
                WriteString(IntToStr(ObsIndex));
                WriteString('_');
                WriteString(IntToStr(NodeNumber));
                WriteFloat(GenTransObs.Time);
                NewLine;
              end;
            end;
          end;

//          if GenTransGroup.ObsGroup.HasObsIndex(1)
//            or GenTransGroup.ObsGroup.HasObsIndex(2) then
//          begin
//            WriteString('  ID ');
//            WriteString(ID);
//            WriteString(' UR');
//            NewLine;
//            for ObsIndex := 0 to GenTransGroup.ObsGroup.Count - 1 do
//            begin
//              GenTransObs := GenTransGroup.ObsGroup[ObsIndex];
////              if GenTransObs.ObsTypeIndex in [1,2] then
//              begin
//                WriteString('    OBSNAME ');
//                WriteString('UR');
//                WriteString(ID);
//                WriteString('_');
//                WriteString(IntToStr(ObsIndex));
//                WriteFloat(GenTransObs.Time);
//                NewLine;
//              end;
//            end;
//          end;

        end;
      end;

      for ObsIndex := 0 to GenTransGroup.ObsGroup.Count - 1 do
      begin
        GenTransObs := GenTransGroup.ObsGroup[ObsIndex];

//        FlowBuilder.Clear;
        ResultantBuilder.Clear;
        for ListIndex := 0 to CellLists.Count - 1 do
        begin
          NodeStart := NodeStarts[ListIndex];
          CellList := CellLists[ListIndex];
          FactorsValues := FactorsValuesList[ListIndex];
          Assert(CellList.Count = FactorsValues.Count);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            AFactor := FactorsValues[CellIndex];

            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
//            ID := IntToStr(NodeNumber);
            ID := IntToStr(CellIndex + NodeStart);

            if (ListIndex > 0) or (CellIndex > 0) then
            begin
//              FlowBuilder.Append(' + ');
              ResultantBuilder.Append(' + ');
            end;
            if AFactor <> 1 then
            begin
//              FlowBuilder.Append(AFactor);
              ResultantBuilder.Append(AFactor);

//              FlowBuilder.Append('*');
              ResultantBuilder.Append('*');
            end;

//            FlowBuilder.Append('UR');
            ResultantBuilder.Append('UGR');

//            FlowBuilder.Append(ID);
            ResultantBuilder.Append(ID);

//            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

//            FlowBuilder.Append(ObsIndex);
            ResultantBuilder.Append(ObsIndex);

//            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

//            FlowBuilder.Append(NodeNumber);
            ResultantBuilder.Append(NodeNumber);
          end;
        end;
//        FlowFormula := FlowBuilder.ToString;
        ResultantFormula := ResultantBuilder.ToString;

//        case GenTransObs.ObsTypeIndex of
////          0:
////            begin
////              DerivedFormula := FlowFormula;
////            end;
//          0:
//            begin
//              DerivedFormula := '(' + ResultantFormula + ')/(' + FlowFormula + ')';
//            end;
//          1:
//            begin
              DerivedFormula := ResultantFormula;
//            end;
//          else
//            begin
//              Assert(False);
//            end;
//        end;

        FDerivedObsList.Add('  OBSNAME ' + GenTransObs.Name + ' PRINT');
        FDerivedObsList.Add('    FORMULA ' + DerivedFormula);
      end;

    end;
  finally
    NodeStarts.Free;
    ResultantBuilder.Free;
//    FlowBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
    StartDictionary.Free;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteObcIdentifiers;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SutraStateObs: TSutraStateObservations;
  TimeIndex: Integer;
  StateObs: TSutraStateObsItem;
  ID: string;
begin
  for ObjectIndex := 0 to FSutraObsObjects.Count - 1 do
  begin
    AScreenObject := FSutraObsObjects[ObjectIndex];
    SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
    for TimeIndex := 0 to SutraStateObs.Count - 1 do
    begin
      StateObs := SutraStateObs[TimeIndex];
      if StateObs.ObsType <> StrLakeStage then
      begin
        ID := Copy(AScreenObject.Name, 1, MaxBoundNameLength);
        WriteString('  ID ');
        WriteString(ID);
        case StateObs.ObsTypeIndex of
          0:
            begin
              WriteString(' P');
            end;
          1:
            begin
              WriteString(' U');
            end;
          2:
            begin
              WriteString(' S');
            end;
        end;
        NewLine;

        WriteString('    OBSNAME ');
        WriteString(StateObs.Name);
        case StateObs.ObsTypeIndex of
          0:
            begin
              StateObs.ExportedName := StateObs.Name + '_P';
              WriteString('_P');
            end;
          1:
            begin
              StateObs.ExportedName := StateObs.Name + '_U';
              WriteString('_U');
            end;
          2:
            begin
              StateObs.ExportedName := StateObs.Name + '_S';
              WriteString('_S');
            end;
        end;
        WriteFloat(StateObs.Time);
        WriteString(' PRINT');
        NewLine;
      end;
    end;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteIdentifiers;
begin
  FDerivedObsList.Clear;
  WriteString('BEGIN IDENTIFIERS');
  NewLine;

  WriteObcIdentifiers;
  WriteLakeIdentifiers;
  WriteSpecPresIdentifiers;
  WriteSpecFlowIdentifiers;
  WriteSpecConcIdentifiers;
  WriteGenFlowIdentifiers;
  WriteGenTransportIdentifiers;

  WriteString('END IDENTIFIERS');
  NewLine;
  NewLine;
end;

procedure TSutraPestObsWriterWriter.WriteLakeIdentifiers;
var

  CellList: TCellAssignmentList;
  Mesh3D: TSutraMesh3D;
  Mesh2D: TSutraMesh2D;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ACell: TCellAssignment;
  Node3D: TSutraNode3D;
  Element2D: TSutraElement2D;
  ClosestNode: TSutraNode2D;
  NodePoint: TPoint2D;
  ObjectPoint: TPoint2D;
  NodeDistance: double;
  NodeIndex: Integer;
  Node2D: TSutraNode2D;
  TestDistance: double;
  NodeNumber: Integer;
  ID: string;
  SutraStateObs: TSutraStateObservations;
  TimeIndex: Integer;
  StateObs: TSutraStateObsItem;
begin
  CellList := TCellAssignmentList.Create;
  try
    if FSutraLakeObjects.Count > 0 then
    begin
      Mesh3D := Model.SutraMesh;
      Mesh2D := Mesh3D.Mesh2D;
      for ObjectIndex := 0 to FSutraLakeObjects.Count - 1 do
      begin
        AScreenObject := FSutraLakeObjects[ObjectIndex];
        CellList.Clear;
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alFirstVertex, Model);

        if CellList.Count > 0 then
        begin
          ACell := CellList[0];

          if AScreenObject.EvaluatedAt = eaNodes then
          begin
            Node3D := Mesh3D.NodeArray[0, ACell.Column];
          end
          else
          begin
            Element2D := Mesh2D.Elements[ACell.Column];
            ClosestNode := Element2D.Nodes[0].Node;
            NodePoint := ClosestNode.Location;
            ObjectPoint := AScreenObject.Points[0];
            NodeDistance := Distance(NodePoint, ObjectPoint);
            for NodeIndex := 1 to Element2D.NodeCount - 1 do
            begin
              Node2D := Element2D.Nodes[NodeIndex].Node;
              NodePoint := Node2D.Location;
              TestDistance := Distance(NodePoint, ObjectPoint);
              if TestDistance < NodeDistance then
              begin
                NodeDistance := TestDistance;
                ClosestNode := Node2D;
              end;
            end;
            Node3D := Mesh3D.NodeArray[0, ClosestNode.Number];
          end;
          NodeNumber := Node3D.Number + 1;

          ID := IntToStr(NodeNumber);
          WriteString('  ID ');
          WriteString(ID);
          WriteString(' LKST');
          NewLine;

          SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
          for TimeIndex := 0 to SutraStateObs.Count - 1 do
          begin
            StateObs := SutraStateObs[TimeIndex];
            if StateObs.ObsType = StrLakeStage then
            begin
              WriteString('    OBSNAME ');
              StateObs.ExportedName := StateObs.Name;
              WriteString(StateObs.Name);
              WriteFloat(StateObs.Time);
              WriteString(' PRINT');
              NewLine;
            end;
          end;
        end;
      end;
    end;
  finally
    CellList.Free;
  end
end;

procedure TSutraPestObsWriterWriter.WriteObservationsFileNames;
var
//  ObjectIndex: Integer;
//  OutputFileNameRoot: string;
//  SutraStateObs: TSutraStateObservations;
  FileName: string;
//  FileIndex: Integer;
begin
//  OutputFileNameRoot := ExtractFileName(ChangeFileExt(FFileName, '')) + '_';
  WriteString('BEGIN OBSERVATION_FILES');
  NewLine;

  if FSutraObsObjects.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '')) + '.obc';
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' OBC');
    NewLine;
  end;

  if FSutraLakeObjects.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.lkst'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' LKST');
    NewLine;
  end;

  if FSutraFluxObs.SpecPres.HasObservations then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcop'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOP');
    NewLine;
  end;

  if FSutraFluxObs.FluidFlow.HasObservations then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcof'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOF');
    NewLine;
  end;

  if FSutraFluxObs.SpecConc.HasObservations then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcou'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOU');
    NewLine;
  end;

  if FSutraFluxObs.GenFlow.HasObservations then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcopg'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOPG');
    NewLine;
  end;

  if FSutraFluxObs.GenTrans.HasObservations then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcoug'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOUG');
    NewLine;
  end;

  WriteString('END OBSERVATION_FILES');
  NewLine;
  NewLine;
end;

procedure TSutraPestObsWriterWriter.WriteOptions;
begin
  WriteString('BEGIN OPTIONS');
  NewLine;

  WriteString('  LISTING ');
  case FExportType of
    etInstructions:
      begin
        WriteString(QuoteFileName(ChangeFileExt(FFileName, '.soeOut')));
      end;
    etExtractedValues:
      begin
        WriteString(QuoteFileName(ChangeFileExt(FFileName, '.soeList')));
      end;
    else
      Assert(False);
  end;
  NewLine;

  case FExportType of
    etInstructions:
      begin
        WriteString('  INSTRUCTION ');
        WriteString(QuoteFileName(ChangeFileExt(FFileName, '.soeIns.txt')));
      end;
    etExtractedValues:
      begin
        WriteString('  VALUES ');
        WriteString(QuoteFileName(ChangeFileExt(FFileName, '.soeValues')));
      end;
    else
      Assert(False);
  end;
  NewLine;

  WriteString('END OPTIONS');
  NewLine;
  NewLine;
end;

procedure TSutraPestObsWriterWriter.WriteSpecConcIdentifiers;
var
  CellLists: TObjectList<TCellAssignmentList>;
  FactorsValuesList: TObjectList<TRealList>;
  CellLocationList: TCellLocationList;
  ResultantBuilder: TStringBuilder;
  Mesh3D: TSutraMesh3D;
  GroupIndex: Integer;
  SpecConcGroup: TSutraSpecConcObservationGroup;
  ObservationFactors: TObservationFactors;
  ObjectIndex: Integer;
  Formula: string;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  FactorsValues: TRealList;
  FactorAnnotation: string;
  DataIdentifier: string;
  ListIndex: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
//  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  SpecConcObs: TSutraSpecConcObsItem;
  AFactor: Double;
  ResultantFormula: string;
  DerivedFormula: string;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraSpecifiedConcTempBoundary;
  TimeList: TRealList;
  NodeNumbers: TIntegerList;
  UsedNodeNumbers: TIntegerList;
  NodeNumberDictionary: TLineIndexDictionary;
  LineIndex: Integer;
  NodeIndex: Integer;
  TimeIndex: Integer;
  procedure GetNodeNumbers(AScreenObject: TScreenObject);
  var
    CellList: TCellAssignmentList;
    CellIndex: Integer;
  begin
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        NodeNumber := CellAssignmentToNodeNumber(ACell);
        NodeNumbers.AddUnique(NodeNumber);
      end;
    finally
      CellList.Free;
    end;
  end;
begin
  if FSutraFluxObs.SpecConc.Count = 0 then
  begin
    Exit;
  end;
  TimeList := TRealList.Create;
  NodeNumbers := TIntegerList.Create;
  UsedNodeNumbers := TIntegerList.Create;
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
  ResultantBuilder := TStringBuilder.Create;
  NodeNumberDictionary := TLineIndexDictionary.Create;
  try
    Mesh3D := Model.SutraMesh;
    NodeNumbers.Sorted := True;
    UsedNodeNumbers.Sorted := True;
    TimeList.Sorted := True;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.SutraBoundaries.SpecifiedConcTemp;
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        GetNodeNumbers(ScreenObject);
      end;
    end;

    for LineIndex := 0 to NodeNumbers.Count - 1 do
    begin
      NodeNumber := NodeNumbers[LineIndex];
      NodeNumberDictionary.Add(NodeNumber, LineIndex+1);
    end;

    for GroupIndex := 0 to FSutraFluxObs.SpecConc.Count - 1 do
    begin
      SpecConcGroup := FSutraFluxObs.SpecConc[GroupIndex];

      for ObsIndex := 0 to SpecConcGroup.ObsGroup.Count - 1 do
      begin
        SpecConcObs := SpecConcGroup.ObsGroup[ObsIndex];
        TimeList.AddUnique(SpecConcObs.Time);
      end;


      ObservationFactors := SpecConcGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        try
          AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
            UsedNodeNumbers.AddUnique(NodeNumber);
          end;
        finally
          CellList.Free;
        end;
      end;
    end;

    for NodeIndex := 0 to UsedNodeNumbers.Count - 1 do
    begin
      NodeNumber := UsedNodeNumbers[NodeIndex];
      Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
      ID := IntToStr(LineIndex);
      WriteString('  ID ');
      WriteString(ID);
      WriteString(' UR');
      WriteInteger(NodeNumber);
      NewLine;

      for TimeIndex := 0 to TimeList.Count -1 do
      begin
        WriteString('    OBSNAME ');
        WriteString('UR');
        WriteString(ID);
        WriteString('_');
        WriteString(IntToStr(NodeNumber));
        WriteString('_');
        WriteString(IntToStr(TimeIndex));
        WriteFloat(TimeList[TimeIndex]);
        NewLine;
      end;
    end;

    for GroupIndex := 0 to FSutraFluxObs.SpecConc.Count - 1 do
    begin
      CellLists.Clear;
      FactorsValuesList.Clear;
      SpecConcGroup := FSutraFluxObs.SpecConc[GroupIndex];
      ObservationFactors := SpecConcGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Formula := ObservationFactors[ObjectIndex].Factor;
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        CellLists.Add(CellList);
        FactorsValues := TRealList.Create;
        FactorsValuesList.Add(FactorsValues);
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        CellList.AssignCellLocationList(CellLocationList);
        AScreenObject.AssignValuesWithCellList(Formula, Model, CellLocationList,
          FactorsValues, FactorAnnotation, DataIdentifier);
      end;
      Assert(CellLists.Count = FactorsValuesList.Count);

      for ObsIndex := 0 to SpecConcGroup.ObsGroup.Count - 1 do
      begin
        SpecConcObs := SpecConcGroup.ObsGroup[ObsIndex];
        TimeIndex := TimeList.IndexOf(SpecConcObs.Time);

        ResultantBuilder.Clear;
        for ListIndex := 0 to CellLists.Count - 1 do
        begin
          CellList := CellLists[ListIndex];
          FactorsValues := FactorsValuesList[ListIndex];
          Assert(CellList.Count = FactorsValues.Count);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            AFactor := FactorsValues[CellIndex];

            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
            Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
            ID := IntToStr(LineIndex);

            if (ListIndex > 0) or (CellIndex > 0) then
            begin
              ResultantBuilder.Append(' + ');
            end;
            if AFactor <> 1 then
            begin
              ResultantBuilder.Append(AFactor);

              ResultantBuilder.Append('*');
            end;

            ResultantBuilder.Append('UR');

            ResultantBuilder.Append(ID);

            ResultantBuilder.Append('_');

            ResultantBuilder.Append(NodeNumber);

            ResultantBuilder.Append('_');

            ResultantBuilder.Append(TimeIndex);
          end;
        end;
        ResultantFormula := ResultantBuilder.ToString;

        DerivedFormula := ResultantFormula;

        FDerivedObsList.Add('  OBSNAME ' + SpecConcObs.Name + ' PRINT');
        FDerivedObsList.Add('    FORMULA ' + DerivedFormula);
      end;

    end;
  finally
    NodeNumberDictionary.Free;
    ResultantBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
    NodeNumbers.Free;
    UsedNodeNumbers.Free;
    TimeList.Free;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteSpecFlowIdentifiers;
var
  CellLists: TObjectList<TCellAssignmentList>;
  FactorsValuesList: TObjectList<TRealList>;
  CellLocationList: TCellLocationList;
  FlowBuilder: TStringBuilder;
  ResultantBuilder: TStringBuilder;
  Mesh3D: TSutraMesh3D;
  GroupIndex: Integer;
  FluidFlowGroup: TSutraFluidFlowObservationGroup;
  ObservationFactors: TObservationFactors;
  ObjectIndex: Integer;
  Formula: string;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  FactorsValues: TRealList;
  FactorAnnotation: string;
  DataIdentifier: string;
  ListIndex: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
//  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  FFObs: TSutraFluidFlowObsItem;
  AFactor: Double;
  FlowFormula: string;
  ResultantFormula: string;
  DerivedFormula: string;
//  LakeInteraction: TLakeBoundaryInteraction;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraFluidBoundary;
  TimeList: TRealList;
  NodeNumbers: TIntegerList;
  UsedNodeNumbers: TIntegerList;
  LineIndex: Integer;
  NodeNumberDictionary: TLineIndexDictionary;
  NodeIndex: Integer;
  TimeIndex: Integer;
  procedure GetNodeNumbers(AScreenObject: TScreenObject);
  var
    CellList: TCellAssignmentList;
    CellIndex: Integer;
  begin
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        NodeNumber := CellAssignmentToNodeNumber(ACell);
        NodeNumbers.AddUnique(NodeNumber);
      end;
    finally
      CellList.Free;
    end;
  end;
begin
  if FSutraFluxObs.FluidFlow.Count = 0 then
  begin
    Exit;
  end;
  TimeList := TRealList.Create;
  NodeNumbers := TIntegerList.Create;
  UsedNodeNumbers := TIntegerList.Create;
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
  FlowBuilder := TStringBuilder.Create;
  ResultantBuilder := TStringBuilder.Create;
  NodeNumberDictionary := TLineIndexDictionary.Create;
  try
    Mesh3D := Model.SutraMesh;
    NodeNumbers.Sorted := True;
    UsedNodeNumbers.Sorted := True;
    TimeList.Sorted := True;

    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.SutraBoundaries.FluidSource;
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        GetNodeNumbers(ScreenObject);
      end;
    end;

    for LineIndex := 0 to NodeNumbers.Count - 1 do
    begin
      NodeNumber := NodeNumbers[LineIndex];
      NodeNumberDictionary.Add(NodeNumber, LineIndex+1);
    end;

    for GroupIndex := 0 to FSutraFluxObs.FluidFlow.Count - 1 do
    begin
      FluidFlowGroup := FSutraFluxObs.FluidFlow[GroupIndex];

      for ObsIndex := 0 to FluidFlowGroup.ObsGroup.Count - 1 do
      begin
        FFObs := FluidFlowGroup.ObsGroup[ObsIndex];
        TimeList.AddUnique(FFObs.Time);
      end;

      ObservationFactors := FluidFlowGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        try
          AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
            UsedNodeNumbers.AddUnique(NodeNumber);
          end;
        finally
          CellList.Free;
        end;
      end;
    end;

    for NodeIndex := 0 to UsedNodeNumbers.Count - 1 do
    begin
      NodeNumber := UsedNodeNumbers[NodeIndex];
      Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
      ID := IntToStr(LineIndex);
      WriteString('  ID ');
      WriteString(ID);
      WriteString(' FF');
      WriteInteger(NodeNumber);
      NewLine;

      for TimeIndex := 0 to TimeList.Count -1 do
      begin
        WriteString('    OBSNAME ');
        WriteString('FF');
        WriteString(ID);
        WriteString('_');
        WriteString(IntToStr(NodeNumber));
        WriteString('_');
        WriteString(IntToStr(TimeIndex));
        WriteFloat(TimeList[TimeIndex]);
        NewLine;
      end;

      WriteString('  ID ');
      WriteString(ID);
      WriteString(' FR');
      WriteInteger(NodeNumber);
      NewLine;

      for TimeIndex := 0 to TimeList.Count -1 do
      begin
        WriteString('    OBSNAME ');
        WriteString('FR');
        WriteString(ID);
        WriteString('_');
        WriteString(IntToStr(NodeNumber));
        WriteString('_');
        WriteString(IntToStr(TimeIndex));
        WriteFloat(TimeList[TimeIndex]);
        NewLine;
      end;
    end;

    for GroupIndex := 0 to FSutraFluxObs.FluidFlow.Count - 1 do
    begin
      CellLists.Clear;
      FactorsValuesList.Clear;
      FluidFlowGroup := FSutraFluxObs.FluidFlow[GroupIndex];
      ObservationFactors := FluidFlowGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Formula := ObservationFactors[ObjectIndex].Factor;
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        CellLists.Add(CellList);
        FactorsValues := TRealList.Create;
        FactorsValuesList.Add(FactorsValues);
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        CellList.AssignCellLocationList(CellLocationList);
        AScreenObject.AssignValuesWithCellList(Formula, Model, CellLocationList,
          FactorsValues, FactorAnnotation, DataIdentifier);
      end;
      Assert(CellLists.Count = FactorsValuesList.Count);

      for ObsIndex := 0 to FluidFlowGroup.ObsGroup.Count - 1 do
      begin
        FFObs := FluidFlowGroup.ObsGroup[ObsIndex];
        TimeIndex := TimeList.IndexOf(FFObs.Time);

        FlowBuilder.Clear;
        ResultantBuilder.Clear;
        for ListIndex := 0 to CellLists.Count - 1 do
        begin
          CellList := CellLists[ListIndex];
          FactorsValues := FactorsValuesList[ListIndex];
          Assert(CellList.Count = FactorsValues.Count);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            AFactor := FactorsValues[CellIndex];

            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
            Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
            ID := IntToStr(LineIndex);

            if (ListIndex > 0) or (CellIndex > 0) then
            begin
              FlowBuilder.Append(' + ');
              ResultantBuilder.Append(' + ');
            end;
            if AFactor <> 1 then
            begin
              FlowBuilder.Append(AFactor);
              ResultantBuilder.Append(AFactor);

              FlowBuilder.Append('*');
              ResultantBuilder.Append('*');
            end;

            FlowBuilder.Append('FF');
            ResultantBuilder.Append('FR');

            FlowBuilder.Append(ID);
            ResultantBuilder.Append(ID);

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(NodeNumber);
            ResultantBuilder.Append(NodeNumber);

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(TimeIndex);
            ResultantBuilder.Append(TimeIndex);
          end;
        end;
        FlowFormula := FlowBuilder.ToString;
        ResultantFormula := ResultantBuilder.ToString;

        case FFObs.ObsTypeIndex of
          0:
            begin
              DerivedFormula := '(' + ResultantFormula + ')/(' + FlowFormula + ')';
            end;
          1:
            begin
              DerivedFormula := ResultantFormula;
            end;
          else
            begin
              Assert(False);
            end;
        end;

        FDerivedObsList.Add('  OBSNAME ' + FFObs.Name + ' PRINT');
        FDerivedObsList.Add('    FORMULA ' + DerivedFormula);
      end;

    end;
  finally
    NodeNumberDictionary.Free;
    ResultantBuilder.Free;
    FlowBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
    NodeNumbers.Free;
    UsedNodeNumbers.Free;
    TimeList.Free;
  end;
end;

function TSutraPestObsWriterWriter.CellAssignmentToNodeNumber
  (ACell: TCellAssignment): Integer;
var
  Mesh3D: TSutraMesh3D;
  Node3D: TSutraNode3D;
  Node2D: TSutraNode2D;
begin
  Mesh3D := Model.SutraMesh;
  if Mesh3D.MeshType = mt3D then
  begin
    Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
    result := Node3D.Number + 1;
  end
  else
  begin
    Node2D := Mesh3D.Mesh2D.Nodes[ACell.Column];
    result := Node2D.Number + 1;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteSpecPresIdentifiers;
var
  CellLists: TObjectList<TCellAssignmentList>;
  FactorsValuesList: TObjectList<TRealList>;
  CellLocationList: TCellLocationList;
  FlowBuilder: TStringBuilder;
  ResultantBuilder: TStringBuilder;
  Mesh3D: TSutraMesh3D;
  GroupIndex: Integer;
  SpecPresGroup: TSutraSpecPressureObservationGroup;
  ObservationFactors: TObservationFactors;
  ObjectIndex: Integer;
  Formula: string;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  FactorsValues: TRealList;
  FactorAnnotation: string;
  DataIdentifier: string;
  ListIndex: Integer;
  CellIndex: Integer;
  ACell: TCellAssignment;
//  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  SPObs: TSutraSpecPressObsItem;
  AFactor: Double;
  FlowFormula: string;
  ResultantFormula: string;
  DerivedFormula: string;
//  StartDictionary: TObservationNodeStartDictionary;
//  LakeInteraction: TLakeBoundaryInteraction;
//  NodeStart: Integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ABoundary: TSutraSpecifiedPressureBoundary;
//  NodeStarts: TList<Integer>;
  NodeNumbers: TIntegerList;
  UsedNodeNumbers: TIntegerList;
  NodeNumberDictionary: TLineIndexDictionary;
  LineIndex: Integer;
  NodeIndex: Integer;
  TimeList: TRealList;
  TimeIndex: Integer;
  procedure GetNodeNumbers(AScreenObject: TScreenObject);
  var
    CellList: TCellAssignmentList;
    CellIndex: Integer;
  begin
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        NodeNumber := CellAssignmentToNodeNumber(ACell);
        NodeNumbers.AddUnique(NodeNumber);
      end;
    finally
      CellList.Free;
    end;
  end;
begin
  if FSutraFluxObs.SpecPres.Count = 0 then
  begin
    Exit;
  end;
  TimeList := TRealList.Create;
  NodeNumbers := TIntegerList.Create;
  UsedNodeNumbers := TIntegerList.Create;
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
  FlowBuilder := TStringBuilder.Create;
  ResultantBuilder := TStringBuilder.Create;
  NodeNumberDictionary := TLineIndexDictionary.Create;
  try
    Mesh3D := Model.SutraMesh;
    NodeNumbers.Sorted := True;
    UsedNodeNumbers.Sorted := True;
    TimeList.Sorted := True;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ABoundary := ScreenObject.SutraBoundaries.SpecifiedPressure;
      if (ABoundary <> nil) and ABoundary.Used then
      begin
        GetNodeNumbers(ScreenObject);
      end;
    end;

    for LineIndex := 0 to NodeNumbers.Count - 1 do
    begin
      NodeNumber := NodeNumbers[LineIndex];
      NodeNumberDictionary.Add(NodeNumber, LineIndex+1);
    end;

    for GroupIndex := 0 to FSutraFluxObs.SpecPres.Count - 1 do
    begin
      SpecPresGroup := FSutraFluxObs.SpecPres[GroupIndex];

      for ObsIndex := 0 to SpecPresGroup.ObsGroup.Count - 1 do
      begin
        SPObs := SpecPresGroup.ObsGroup[ObsIndex];
        TimeList.AddUnique(SPObs.Time);
      end;

      ObservationFactors := SpecPresGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        try
          AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            NodeNumber := CellAssignmentToNodeNumber(ACell);
            UsedNodeNumbers.AddUnique(NodeNumber);
          end;
        finally
          CellList.Free;
        end;
      end;
    end;

    for NodeIndex := 0 to UsedNodeNumbers.Count - 1 do
    begin
      NodeNumber := UsedNodeNumbers[NodeIndex];
      Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
//      ID := IntToStr(NodeNumber);
      ID := IntToStr(LineIndex);
      WriteString('  ID ');
      WriteString(ID);
      WriteString(' PF');
      WriteInteger(NodeNumber);
      NewLine;

      for TimeIndex := 0 to TimeList.Count -1 do
      begin
        WriteString('    OBSNAME ');
        WriteString('PF');
        WriteString(ID);
        WriteString('_');
        WriteString(IntToStr(NodeNumber));
        WriteString('_');
        WriteString(IntToStr(TimeIndex));
        WriteFloat(TimeList[TimeIndex]);
        NewLine;
      end;

      WriteString('  ID ');
      WriteString(ID);
      WriteString(' PR');
      WriteInteger(NodeNumber);
      NewLine;

      for TimeIndex := 0 to TimeList.Count -1 do
      begin
        WriteString('    OBSNAME ');
        WriteString('PR');
        WriteString(ID);
        WriteString('_');
        WriteString(IntToStr(NodeNumber));
        WriteString('_');
        WriteString(IntToStr(TimeIndex));
        WriteFloat(TimeList[TimeIndex]);
        NewLine;
      end;
    end;

    for GroupIndex := 0 to FSutraFluxObs.SpecPres.Count - 1 do
    begin
      CellLists.Clear;
      FactorsValuesList.Clear;
      SpecPresGroup := FSutraFluxObs.SpecPres[GroupIndex];
      ObservationFactors := SpecPresGroup.ObsGroup.ObservationFactors;
      for ObjectIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Formula := ObservationFactors[ObjectIndex].Factor;
        AScreenObject := ObservationFactors[ObjectIndex].
          ScreenObject as TScreenObject;
        if AScreenObject = nil then
        begin
          Continue;
        end;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;

        Assert(AScreenObject <> nil);
        CellList := TCellAssignmentList.Create;
        CellLists.Add(CellList);
        FactorsValues := TRealList.Create;
        FactorsValuesList.Add(FactorsValues);
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        CellList.AssignCellLocationList(CellLocationList);
        AScreenObject.AssignValuesWithCellList(Formula, Model, CellLocationList,
          FactorsValues, FactorAnnotation, DataIdentifier);
      end;
      Assert(CellLists.Count = FactorsValuesList.Count);

      for ObsIndex := 0 to SpecPresGroup.ObsGroup.Count - 1 do
      begin
        SPObs := SpecPresGroup.ObsGroup[ObsIndex];
        TimeIndex := TimeList.IndexOf(SPObs.Time);

        FlowBuilder.Clear;
        ResultantBuilder.Clear;
        for ListIndex := 0 to CellLists.Count - 1 do
        begin
          CellList := CellLists[ListIndex];
          FactorsValues := FactorsValuesList[ListIndex];
          Assert(CellList.Count = FactorsValues.Count);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            AFactor := FactorsValues[CellIndex];

            NodeNumber := CellAssignmentToNodeNumber(ACell);
//            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
//            NodeNumber := Node3D.Number + 1;
            Assert(NodeNumberDictionary.TryGetValue(NodeNumber, LineIndex));
            ID := IntToStr(LineIndex);

            if (ListIndex > 0) or (CellIndex > 0) then
            begin
              FlowBuilder.Append(' + ');
              ResultantBuilder.Append(' + ');
            end;
            if AFactor <> 1 then
            begin
              FlowBuilder.Append(AFactor);
              ResultantBuilder.Append(AFactor);

              FlowBuilder.Append('*');
              ResultantBuilder.Append('*');
            end;

            FlowBuilder.Append('PF');
            ResultantBuilder.Append('PR');

            FlowBuilder.Append(ID);
            ResultantBuilder.Append(ID);

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(IntToStr(NodeNumber));
            ResultantBuilder.Append(IntToStr(NodeNumber));

            FlowBuilder.Append('_');
            ResultantBuilder.Append('_');

            FlowBuilder.Append(IntToStr(TimeIndex));
            ResultantBuilder.Append(IntToStr(TimeIndex));
          end;
        end;
        FlowFormula := FlowBuilder.ToString;
        ResultantFormula := ResultantBuilder.ToString;

        case SPObs.ObsTypeIndex of
          0:
            begin
              DerivedFormula := FlowFormula;
            end;
          1:
            begin
              DerivedFormula := '(' + ResultantFormula + ')/(' + FlowFormula + ')';
            end;
          2:
            begin
              DerivedFormula := ResultantFormula;
            end;
          else
            begin
              Assert(False);
            end;
        end;

        FDerivedObsList.Add('  OBSNAME ' + SPObs.Name + ' PRINT');
        FDerivedObsList.Add('    FORMULA ' + DerivedFormula);
      end;

    end

  finally
    NodeNumberDictionary.Free;
    ResultantBuilder.Free;
    FlowBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
    NodeNumbers.Free;
    UsedNodeNumbers.Free;
    TimeList.Free;
  end;
end;

end.
