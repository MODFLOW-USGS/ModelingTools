unit SutraPestObsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, System.SysUtils, ScreenObjectUnit,
  System.Classes;

type
  TExportType = (etInstructions, etExtractedValues);

  TSutraPestObsWriterWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    FSutraLakeObjects: TScreenObjectList;
    FSutraObsObjects: TScreenObjectList;
    FSutraAllObsObjects: TScreenObjectList;
    FDerivedObsList: TStringList;
    FExportType: TExportType;
    procedure Evaluate;
    procedure WriteOptions;
    procedure WriteObservationsFileNames;
    procedure WriteIdentifiers;
    procedure WriteDerivedObservations;

  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  SutraPestObsUnit, GoPhastTypes, SutraMeshUnit, FastGEO, ModelMuseUtilities,
  PestObsUnit, ObservationComparisonsUnit, frmErrorsAndWarningsUnit;

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
  FSutraAllObsObjects := TScreenObjectList.Create;
  FDerivedObsList := TStringList.Create;
end;

destructor TSutraPestObsWriterWriter.Destroy;
begin
  FDerivedObsList.Free;
  FSutraAllObsObjects.Free;
  FSutraLakeObjects.Free;
  FSutraObsObjects.Free;
  inherited;
end;

procedure TSutraPestObsWriterWriter.Evaluate;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SutraStateObs: TSutraStateObservations;
begin
  for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
    AScreenObject.SutraBoundaries.SutraStateObs.Used;
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
      FSutraAllObsObjects.Add(AScreenObject);
    end;
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
begin
  WriteString('BEGIN DERIVED_OBSERVATIONS');
  NewLine;
  for LineIndex := 0 to FDerivedObsList.Count - 1 do
  begin
    WriteString(FDerivedObsList[LineIndex]);
    NewLine;
  end;

  for ObjectIndex := 0 to FSutraAllObsObjects.Count - 1 do
  begin
    AScreenObject := FSutraAllObsObjects[ObjectIndex];
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

procedure TSutraPestObsWriterWriter.WriteIdentifiers;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ID: string;
  SutraStateObs: TSutraStateObservations;
  TimeIndex: Integer;
  StateObs: TSutraStateObsItem;
  ACell: TCellAssignment;
  Mesh3D: TSutraMesh3D;
  Node3D: TSutraNode3D;
  NodeNumber: Integer;
  Mesh2D: TSutraMesh2D;
  Element2D: TSutraElement2D;
  NodeIndex: Integer;
  Node2D: TSutraNode2D;
  CellList: TCellAssignmentList;
  APoint: TPoint2D;
  DerivedLine: string;
begin
  FDerivedObsList.Clear;
  WriteString('BEGIN IDENTIFIERS');
  NewLine;
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
              WriteString('_P');
            end;
          1:
            begin
              WriteString('_U');
            end;
          2:
            begin
              WriteString('_S');
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

  if FSutraLakeObjects.Count > 0 then
  begin
    Mesh3D := Model.SutraMesh;
    Mesh2D := Mesh3D.Mesh2D;
    CellList := TCellAssignmentList.Create;
    try
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
            NodeNumber := Node3D.Number + 1;

            ID := IntToStr(NodeNumber);
            WriteString('  ID ');
            WriteString(ID);
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
//                WriteString(ID);
                WriteFloat(StateObs.Time);
                WriteString(' PRINT');
                NewLine;
              end;
            end;
          end
          else
          begin
            SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
            Element2D := Mesh2D.Elements[ACell.Column];
            for TimeIndex := 0 to SutraStateObs.Count - 1 do
            begin
              StateObs := SutraStateObs[TimeIndex];
              if StateObs.ObsType = StrLakeStage then
              begin
                StateObs.ExportedName := StateObs.Name;
                FDerivedObsList.Add(Format('  OBSNAME %s PRINT', [StateObs.Name]));
                APoint := AScreenObject.Points[0];
                DerivedLine := Format('    INTERPOLATE %0:g %1:g ', [APoint.x, APoint.y]);
                for NodeIndex := 0 to Element2D.NodeCount - 1 do
                begin
                  Node2D := Element2D.Nodes[NodeIndex].Node;
                  Node3D := Mesh3D.NodeArray[0, Node2D.Number];
                  NodeNumber := Node3D.Number + 1;
                  ID := IntToStr(NodeNumber);

                  DerivedLine := Format('%0:s %1:s%2:s_%3:d',
                    [DerivedLine, StateObs.Name, ID, TimeIndex]);
                end;
                FDerivedObsList.Add(DerivedLine);
              end;
            end;
            for NodeIndex := 0 to Element2D.NodeCount - 1 do
            begin
              Node2D := Element2D.Nodes[NodeIndex].Node;
              Node3D := Mesh3D.NodeArray[0, Node2D.Number];
              NodeNumber := Node3D.Number + 1;

              ID := IntToStr(NodeNumber);
              WriteString('  ID ');
              WriteString(ID);
              NewLine;

              WriteString('    LOCATION');
              WriteFloat(Node2D.X);
              WriteFloat(Node2D.Y);
              NewLine;

              for TimeIndex := 0 to SutraStateObs.Count - 1 do
              begin
                StateObs := SutraStateObs[TimeIndex];
                if StateObs.ObsType = StrLakeStage then
                begin
                  WriteString('    OBSNAME ');
                  WriteString(StateObs.Name);
                  WriteString(ID);
                  WriteString('_');
                  WriteString(IntToStr(TimeIndex));
                  WriteFloat(StateObs.Time);
  //                WriteString(' PRINT');
                  NewLine;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      CellList.Free;
    end;
  end;
  WriteString('END IDENTIFIERS');
  NewLine;
  NewLine;
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

end.
