unit SutraPestObsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, System.SysUtils, ScreenObjectUnit;

type
  TSutraPestObsWriterWriter = class(TCustomFileWriter)
  private
    FFileName: string;
    FSutraLakeObjects: TScreenObjectList;
    FSutraObsObjects: TScreenObjectList;
    procedure Evaluate;
    procedure WriteOptions;
    procedure WriteObservationsFileNames;
    procedure WriteIdentifiers;
    procedure WriteDeriviedObservations;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    procedure WriteFile(const FileName: string);
  end;


implementation

uses
  SutraPestObsUnit, GoPhastTypes, SutraMeshUnit;

{ TSutraPestObsWriterWriter }

constructor TSutraPestObsWriterWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FSutraLakeObjects := TScreenObjectList.Create;
  FSutraObsObjects := TScreenObjectList.Create;

end;

destructor TSutraPestObsWriterWriter.Destroy;
begin
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
    end;
  end;
end;

procedure TSutraPestObsWriterWriter.WriteDeriviedObservations;
begin

end;

procedure TSutraPestObsWriterWriter.WriteFile(const FileName: string);
begin
  Evaluate;

  FFileName := ChangeFileExt(FileName, '.soe');
  OpenFile(FFileName);
  try
    WriteOptions;
    WriteObservationsFileNames;
    WriteIdentifiers;
    WriteDeriviedObservations;
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
begin
  for ObjectIndex := 0 to FSutraObsObjects.Count - 1 do
  begin
    AScreenObject := FSutraObsObjects[ObjectIndex];
    ID := Copy(AScreenObject.Name, 1, 40);
    WriteString('  ID');
    NewLine;

    SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
    for TimeIndex := 0 to SutraStateObs.Count - 1 do
    begin
      StateObs := SutraStateObs[TimeIndex];
      if StateObs.ObsType <> StrLakeStage then
      begin
        WriteString('    OBSNAME ');
        WriteString(StateObs.Name);
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
            WriteString('  ID');
            NewLine;

            SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
            for TimeIndex := 0 to SutraStateObs.Count - 1 do
            begin
              StateObs := SutraStateObs[TimeIndex];
              if StateObs.ObsType = StrLakeStage then
              begin
                WriteString('    OBSNAME ');
                WriteString(StateObs.Name);
                WriteFloat(StateObs.Time);
                WriteString(' PRINT');
                NewLine;
              end;
            end;
          end
          else
          begin
            Element2D := Mesh2D.Elements[ACell.Column];
            for NodeIndex := 0 to Element2D.NodeCount - 1 do
            begin
              Node2D := Element2D.Nodes[NodeIndex].Node;
              Node3D := Mesh3D.NodeArray[0, Node2D.Number];
              NodeNumber := Node3D.Number + 1;

              ID := IntToStr(NodeNumber);
              WriteString('  ID');
              NewLine;

              WriteString('  LOCATION');
              WriteFloat(Node2D.X);
              WriteFloat(Node2D.Y);
              NewLine;

              SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
              for TimeIndex := 0 to SutraStateObs.Count - 1 do
              begin
                StateObs := SutraStateObs[TimeIndex];
                if StateObs.ObsType = StrLakeStage then
                begin
                  WriteString('    OBSNAME ');
                  WriteString(StateObs.Name);
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
  begin                                             NewLine;
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

end;

end.
