unit SutraPestObsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, System.SysUtils, ScreenObjectUnit,
  System.Classes, SutraPestObsUnit, System.Generics.Collections;

type
  TExportType = (etInstructions, etExtractedValues);

  TSutraPestObsWriterWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    FSutraLakeObjects: TScreenObjectList;
    FSutraObsObjects: TScreenObjectList;
    FSutraAllStateObsObjects: TScreenObjectList;
//    FSutraSpecPresObsObjects: TScreenObjectList;
//    FSutraFluidFlowObsObjects: TScreenObjectList;
//    FSutraSpecUObsObjects: TScreenObjectList;
//    FSutraGenPresObsObjects: TScreenObjectList;
//    FSutraGenPresTransObjects: TScreenObjectList;
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
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  GoPhastTypes, SutraMeshUnit, FastGEO, ModelMuseUtilities,
  PestObsUnit, ObservationComparisonsUnit, frmErrorsAndWarningsUnit,
  SutraBoundariesUnit, FluxObservationUnit, RealListUnit, ModflowCellUnit;

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
  {
    FSutraSpecPresObsObjects: TScreenObjectList;
    FSutraFluidFlowObsObjects: TScreenObjectList;
    FSutraSpecUObsObjects: TScreenObjectList;
    FSutraGenPresObsObjects: TScreenObjectList;
    FSutraGenPresTransObjects: TScreenObjectList;
}

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
  PriorItem1: TCustomObservationItem;
  PriorItem2: TCustomObservationItem;
  ErrorMessage: string;
  GroupIndex: Integer;
  SpecPresGroup: TSutraSpecPressureObservations;
  SpecPresItem: TSutraSpecPressObsItem;
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
          WriteString(PriorItem1.ExportedName);
          WriteString(' - ');
          WriteString(PriorItem2.ExportedName);
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
  FFileName := ChangeFileExt(AFileName, '.soe_i');
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
  FFileName := ChangeFileExt(AFileName, '.soe_ev');
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
        ID := Copy(AScreenObject.Name, 1, 40);
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
  ObjectIndex: Integer;
  OutputFileNameRoot: string;
  SutraStateObs: TSutraStateObservations;
  FileName: string;
begin
  OutputFileNameRoot := ExtractFileName(ChangeFileExt(FFileName, '')) + '_';
  WriteString('BEGIN OBSERVATION_FILES');
  NewLine;

  for ObjectIndex := 0 to FSutraObsObjects.Count - 1 do
  begin
    SutraStateObs := FSutraObsObjects[ObjectIndex].SutraBoundaries.SutraStateObs;
    FileName := OutputFileNameRoot + SutraStateObs.ScheduleName + '.obc';
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

  if FSutraFluxObs.SpecPres.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcop'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOP');
    NewLine;
  end;

  if FSutraFluxObs.FluidFlow.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcof'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOF');
    NewLine;
  end;

{
  if FSutraFluxObs.SpecConc.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcou'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOU');
    NewLine;
  end;

  if FSutraFluxObs.GenFlow.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcopg'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOPG');
    NewLine;
  end;

  if FSutraFluxObs.GenTrans.Count > 0 then
  begin
    FileName := ExtractFileName(ChangeFileExt(FFileName, '.bcoug'));
    WriteString('  FILENAME ');
    WriteString(FileName);
    WriteString(' BCOUG');
    NewLine;
  end;
  }

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
        WriteString('  OUTPUT ');
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

procedure TSutraPestObsWriterWriter.WriteSpecFlowIdentifiers;
begin

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
  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  ID: string;
  ObsIndex: Integer;
  SPObs: TSutraSpecPressObsItem;
  AFactor: Double;
  FlowFormula: string;
  ResultantFormula: string;
  DerivedFormula: string;
begin
  CellLists := TObjectList<TCellAssignmentList>.Create;
  FactorsValuesList := TObjectList<TRealList>.Create;
  CellLocationList := TCellLocationList.Create;
  FlowBuilder := TStringBuilder.Create;
  ResultantBuilder := TStringBuilder.Create;
  try
    Mesh3D := Model.SutraMesh;
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
      for ListIndex := 0 to CellLists.Count - 1 do
      begin
        CellList := CellLists[ListIndex];
//        FactorsValues := FactorsValuesList[ListIndex];
//        Assert(CellList.Count = FactorsValues.Count);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
//          AFactor := FactorsValues[CellIndex];
          Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
          NodeNumber := Node3D.Number + 1;
          ID := IntToStr(NodeNumber);

          if SpecPresGroup.ObsGroup.HasObsIndex(0)
            or SpecPresGroup.ObsGroup.HasObsIndex(1) then
          begin
            WriteString('  ID ');
            WriteString(ID);
            WriteString(' PF');
            NewLine;
            for ObsIndex := 0 to SpecPresGroup.ObsGroup.Count - 1 do
            begin
              SPObs := SpecPresGroup.ObsGroup[ObsIndex];
              if SPObs.ObsTypeIndex in [0,1] then
              begin
                WriteString('    OBSNAME ');
                WriteString('PF');
                WriteString(ID);
                WriteString('_');
                WriteString(IntToStr(ObsIndex));
                WriteFloat(SPObs.Time);
                NewLine;
              end;
            end;
          end;

          if SpecPresGroup.ObsGroup.HasObsIndex(1)
            or SpecPresGroup.ObsGroup.HasObsIndex(2) then
          begin
            WriteString('  ID ');
            WriteString(ID);
            WriteString(' PR');
            NewLine;
            for ObsIndex := 0 to SpecPresGroup.ObsGroup.Count - 1 do
            begin
              SPObs := SpecPresGroup.ObsGroup[ObsIndex];
              if SPObs.ObsTypeIndex in [1,2] then
              begin
                WriteString('    OBSNAME ');
                WriteString('PR');
                WriteString(ID);
                WriteString('_');
                WriteString(IntToStr(ObsIndex));
                WriteFloat(SPObs.Time);
                NewLine;
              end;
            end;
          end;

        end;
      end;

      for ObsIndex := 0 to SpecPresGroup.ObsGroup.Count - 1 do
      begin
        SPObs := SpecPresGroup.ObsGroup[ObsIndex];

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

            Node3D := Mesh3D.NodeArray[ACell.Layer, ACell.Column];
            NodeNumber := Node3D.Number + 1;
            ID := IntToStr(NodeNumber);

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

            FlowBuilder.Append(ObsIndex);
            ResultantBuilder.Append(ObsIndex);
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

    end;
  finally
    ResultantBuilder.Free;
    FlowBuilder.Free;
    CellLocationList.Free;
    CellLists.Free;
    FactorsValuesList.Free;
  end;
end;

end.
