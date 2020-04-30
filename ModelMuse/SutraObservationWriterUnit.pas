unit SutraObservationWriterUnit;

interface

uses
  CustomModflowWriterUnit, Generics.Collections, PhastModelUnit, FastGEO,
  GoPhastTypes, SysUtils, SutraBoundariesUnit, Classes, SutraBoundaryUnit;

type
  TObsType = (otExact, otNode, otElement);

  TObsLocation = class(TObject)
  strict private
    FLocation: TPoint3d;
  private
    FNodeOrElementNumber: integer;
    FObsType: TObsType;
    function GetCenter2D: TPoint2D;
    procedure SetCenter2D(const Value: TPoint2D);
  public
    property X: double read FLocation.x write FLocation.x;
    property Y: double read FLocation.y write FLocation.y;
    property Z: double read FLocation.Z write FLocation.Z;
    property Location: TPoint3d read FLocation write FLocation;
    property Center2D: TPoint2D read GetCenter2D write SetCenter2D;
    property ObsType: TObsType read FObsType write FObsType;
    property NodeOrElementNumber: integer read FNodeOrElementNumber write FNodeOrElementNumber;
  end;

  TObsLocationList = TObjectList<TObsLocation>;

  TObsGroup = class(TObject)
  private
    FLocations: TObsLocationList;
    FObsName: AnsiString;
    FObsSchedule: AnsiString;
    FTimeValues: TRealCollection;
    FObservationFormat: TObservationFormat;
    function GetObsName: AnsiString;
    procedure SetObsName(const Value: AnsiString);
    procedure SetTimeValues(const Value: TRealCollection);
    procedure SetObservationFormat(const Value: TObservationFormat);
  public
    constructor Create;
    destructor Destroy; override;
    property ObsName: AnsiString read GetObsName Write SetObsName;
    property ObsSchedule: AnsiString read FObsSchedule write FObsSchedule;
    property TimeValues: TRealCollection read FTimeValues write SetTimeValues;
    property ObservationFormat: TObservationFormat read FObservationFormat write SetObservationFormat;
  end;

  TObsGroupList = TObjectList<TObsGroup>;

  TSutraObservationWriter = class(TCustomFileWriter)
  private
    FObsGroups: TObsGroupList;
    FNOBS: Integer;
    FUsedFormats: TObservationFormats;
    FFileName: string;
    FBuffer: TStringBuilder;
    FObservationList: TStringList;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteObservationGroups;
  protected
    class function Extension: string; override;
  public
    procedure NewLine; override;
    procedure WriteString(const Value: AnsiString); overload; override;
    Constructor Create(AModel: TCustomModel;
      EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const FileName: string; ObservationList: TStringList; out NOBS: integer);
  end;

implementation

uses
  ScreenObjectUnit, SparseDataSets, OctTreeClass,
  SutraMeshUnit, SutraOutputControlUnit, SutraFileWriterUnit, SutraPestObsUnit;

{ TSutraObservationWriter }

constructor TSutraObservationWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FBuffer := TStringBuilder.Create;
  FObsGroups := TObsGroupList.Create;
  FUsedFormats := [];
end;

destructor TSutraObservationWriter.Destroy;
begin
  FObsGroups.Free;
  FBuffer.Free;
  inherited;
end;

procedure TSutraObservationWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: TSutraObservations;
  CellList: TCellAssignmentList;
  Mesh: TSutraMesh3D;
  ACell: TCellAssignment;
  AnObsGroup: TObsGroup;
  OctTree: TRbwOctTree;
  Obs: TObsLocation;
  X, Y, Z: double;
  AnElement: TSutraElement3D;
  ANode: TSutraNode3D;
  APointer: Pointer;
  AnElement2D: TSutraElement2D;
  ANode2D: TSutraNode2D;
  CenterPoint2D: TPoint2D;
  CenterPoint3D: TPoint3D;
  NodeOrElementNumber: Integer;
  ObsType: TObsType;
  SutraStateObs: TSutraStateObservations;
  TimeIndex: Integer;
  procedure GetObservationLocations;
  var
    CellIndex: Integer;
  begin
    OctTree.Clear;
    if (ScreenObject.SectionCount = ScreenObject.Count)
      and (ScreenObject.ViewDirection = vdTop)
      and ((ScreenObject.ElevationCount = ecOne)
      or (Mesh.MeshType in [mt2D, mtProfile]))
    then
    begin
      // make observations at the exact locations of the points
      // in the objects.

      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];

        if Mesh.MeshType = mt3D then
        begin
          case ScreenObject.EvaluatedAt of
            eaBlocks:
              begin
                AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
                if not AnElement.Active then
                begin
                  Continue;
                end;
              end;
            eaNodes:
              begin
                ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
                if not ANode.Active then
                begin
                  Continue;
                end;
              end;
          end;
        end;

        X := ACell.SutraX;
        Y := ACell.SutraY;
        Z := ACell.SutraZ;
        Obs := nil;
        if OctTree.Count > 0 then
        begin
          OctTree.FirstNearestPoint(X, Y, Z, APointer);
          if (X <> ACell.SutraX) or (Y <> ACell.SutraY) or
            (Z <> ACell.SutraZ) then
          begin
            Obs := nil;
          end
          else
          begin
            Obs := APointer;
          end;
        end;
        if Obs = nil then
        begin
          Obs := TObsLocation.Create;
          Obs.X := ACell.SutraX;
          Obs.Y := ACell.SutraY;
          Obs.Z := ACell.SutraZ;
          Obs.NodeOrElementNumber := 0;
          Obs.ObsType := otExact;
          OctTree.AddPoint(Obs.X, Obs.Y, Obs.Z, Obs);
          AnObsGroup.FLocations.Add(Obs);
        end;
      end;
    end
    else
    begin
      // make observations at nodes or at the centers of elements.
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        ObsType := otExact;
        NodeOrElementNumber := -1;
        case ScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              ObsType := otElement;
              case Mesh.MeshType of
                mt2D, mtProfile:
                  begin
                    AnElement2D := Mesh.Mesh2D.Elements[ACell.Column];
                    CenterPoint2D := AnElement2D.Center;
                    CenterPoint3D.x := CenterPoint2D.x;
                    CenterPoint3D.y := CenterPoint2D.y;
                    CenterPoint3D.z := 0;
                    NodeOrElementNumber := AnElement2D.DisplayNumber;
                  end;
                mt3D:
                  begin
                    AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
                    if AnElement.Active then
                    begin
                      CenterPoint3D := AnElement.CenterLocation;
                    end
                    else
                    begin
                      Continue;
                    end;
                    NodeOrElementNumber := AnElement.DisplayNumber;
                  end;
                else Assert(False);
              end;
            end;
          eaNodes:
            begin
              ObsType := otNode;
              case Mesh.MeshType of
                mt2D, mtProfile:
                  begin
                    ANode2D := Mesh.Mesh2D.Nodes[ACell.Column];
                    CenterPoint2D := ANode2D.Location;
                    CenterPoint3D.x := CenterPoint2D.x;
                    CenterPoint3D.y := CenterPoint2D.y;
                    CenterPoint3D.z := 0;
                    NodeOrElementNumber := ANode2D.DisplayNumber;
                  end;
                mt3D:
                  begin
                    ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
                    if ANode.Active then
                    begin
                      CenterPoint3D := ANode.NodeLocation;
                    end
                    else
                    begin
                      Continue;
                    end;
                    NodeOrElementNumber := ANode.DisplayNumber;
                  end;
                else Assert(False);
              end;
            end;
        else
          Assert(False);
        end;
        X := CenterPoint3D.X;
        Y := CenterPoint3D.Y;
        Z := CenterPoint3D.Z;
        Assert(ObsType <> otExact);
        Assert(NodeOrElementNumber >= 0);
        Obs := nil;
        if OctTree.Count > 0 then
        begin
          OctTree.FirstNearestPoint(X, Y, Z, APointer);
          if (X <> CenterPoint3D.X) or (Y <> CenterPoint3D.Y) or
            (Z <> CenterPoint3D.Z) then
          begin
            Obs := nil;
          end
          else
          begin
            Obs := APointer;
          end;
        end;
        if Obs = nil then
        begin
          Obs := TObsLocation.Create;
          Obs.Location := CenterPoint3D;
          Obs.NodeOrElementNumber := NodeOrElementNumber;
          Obs.ObsType := ObsType;
          OctTree.AddPoint(Obs.X, Obs.Y, Obs.Z, Obs);
          AnObsGroup.FLocations.Add(Obs);
        end;
      end;
    end;

  end;
begin
  OctTree := TRbwOctTree.Create(nil);
  try
    FObsGroups.Clear;
    Mesh := Model.SutraMesh;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      Observations := ScreenObject.SutraBoundaries.Observations;
      if Observations.Used then
      begin
        AnObsGroup := TObsGroup.Create;
        FObsGroups.Add(AnObsGroup);
        AnObsGroup.ObsName := Observations.ObservationName;
        AnObsGroup.ObservationFormat := Observations.ObservationFormat;
        AnObsGroup.ObsSchedule := AnsiString(Observations.ExportScheduleName);
        AnObsGroup.TimeValues := Observations.Times;

        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetCellsToAssign({Mesh,} '0', nil, nil, CellList,
            alAll, Model);
          GetObservationLocations;

//          OctTree.Clear;
//          if (ScreenObject.SectionCount = ScreenObject.Count)
//            and (ScreenObject.ViewDirection = vdTop)
//            and ((ScreenObject.ElevationCount = ecOne)
//            or (Mesh.MeshType in [mt2D, mtProfile]))
//          then
//          begin
//            // make observations at the exact locations of the points
//            // in the objects.
//
//            for CellIndex := 0 to CellList.Count - 1 do
//            begin
//              ACell := CellList[CellIndex];
//
//              if Mesh.MeshType = mt3D then
//              begin
//                case ScreenObject.EvaluatedAt of
//                  eaBlocks:
//                    begin
//                      AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
//                      if not AnElement.Active then
//                      begin
//                        Continue;
//                      end;
//                    end;
//                  eaNodes:
//                    begin
//                      ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
//                      if not ANode.Active then
//                      begin
//                        Continue;
//                      end;
//                    end;
//                end;
//              end;
//
//              X := ACell.SutraX;
//              Y := ACell.SutraY;
//              Z := ACell.SutraZ;
//              Obs := nil;
//              if OctTree.Count > 0 then
//              begin
//                OctTree.FirstNearestPoint(X, Y, Z, APointer);
//                if (X <> ACell.SutraX) or (Y <> ACell.SutraY) or
//                  (Z <> ACell.SutraZ) then
//                begin
//                  Obs := nil;
//                end
//                else
//                begin
//                  Obs := APointer;
//                end;
//              end;
//              if Obs = nil then
//              begin
//                Obs := TObsLocation.Create;
//                Obs.X := ACell.SutraX;
//                Obs.Y := ACell.SutraY;
//                Obs.Z := ACell.SutraZ;
//                Obs.NodeOrElementNumber := 0;
//                Obs.ObsType := otExact;
//                OctTree.AddPoint(Obs.X, Obs.Y, Obs.Z, Obs);
//                AnObsGroup.FLocations.Add(Obs);
//              end;
//            end;
//          end
//          else
//          begin
//            // make observations at nodes or at the centers of elements.
//            for CellIndex := 0 to CellList.Count - 1 do
//            begin
//              ACell := CellList[CellIndex];
//              ObsType := otExact;
//              NodeOrElementNumber := -1;
//              case ScreenObject.EvaluatedAt of
//                eaBlocks:
//                  begin
//                    ObsType := otElement;
//                    case Mesh.MeshType of
//                      mt2D, mtProfile:
//                        begin
//                          AnElement2D := Mesh.Mesh2D.Elements[ACell.Column];
//                          CenterPoint2D := AnElement2D.Center;
//                          CenterPoint3D.x := CenterPoint2D.x;
//                          CenterPoint3D.y := CenterPoint2D.y;
//                          CenterPoint3D.z := 0;
//                          NodeOrElementNumber := AnElement2D.DisplayNumber;
//                        end;
//                      mt3D:
//                        begin
//                          AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
//                          if AnElement.Active then
//                          begin
//                            CenterPoint3D := AnElement.CenterLocation;
//                          end
//                          else
//                          begin
//                            Continue;
//                          end;
//                          NodeOrElementNumber := AnElement.DisplayNumber;
//                        end;
//                      else Assert(False);
//                    end;
//                  end;
//                eaNodes:
//                  begin
//                    ObsType := otNode;
//                    case Mesh.MeshType of
//                      mt2D, mtProfile:
//                        begin
//                          ANode2D := Mesh.Mesh2D.Nodes[ACell.Column];
//                          CenterPoint2D := ANode2D.Location;
//                          CenterPoint3D.x := CenterPoint2D.x;
//                          CenterPoint3D.y := CenterPoint2D.y;
//                          CenterPoint3D.z := 0;
//                          NodeOrElementNumber := ANode2D.DisplayNumber;
//                        end;
//                      mt3D:
//                        begin
//                          ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
//                          if ANode.Active then
//                          begin
//                            CenterPoint3D := ANode.NodeLocation;
//                          end
//                          else
//                          begin
//                            Continue;
//                          end;
//                          NodeOrElementNumber := ANode.DisplayNumber;
//                        end;
//                      else Assert(False);
//                    end;
//                  end;
//              else
//                Assert(False);
//              end;
//              X := CenterPoint3D.X;
//              Y := CenterPoint3D.Y;
//              Z := CenterPoint3D.Z;
//              Assert(ObsType <> otExact);
//              Assert(NodeOrElementNumber >= 0);
//              Obs := nil;
//              if OctTree.Count > 0 then
//              begin
//                OctTree.FirstNearestPoint(X, Y, Z, APointer);
//                if (X <> CenterPoint3D.X) or (Y <> CenterPoint3D.Y) or
//                  (Z <> CenterPoint3D.Z) then
//                begin
//                  Obs := nil;
//                end
//                else
//                begin
//                  Obs := APointer;
//                end;
//              end;
//              if Obs = nil then
//              begin
//                Obs := TObsLocation.Create;
//                Obs.Location := CenterPoint3D;
//                Obs.NodeOrElementNumber := NodeOrElementNumber;
//                Obs.ObsType := ObsType;
//                OctTree.AddPoint(Obs.X, Obs.Y, Obs.Z, Obs);
//                AnObsGroup.FLocations.Add(Obs);
//              end;
//            end;
//          end;
        finally
          CellList.Free;
        end;
      end;

      if Model.PestUsed then
      begin
        SutraStateObs := ScreenObject.SutraBoundaries.SutraStateObs;
        if SutraStateObs.Used and SutraStateObs.HasNonLakeBoundary then
        begin
          AnObsGroup := TObsGroup.Create;
          FObsGroups.Add(AnObsGroup);
          AnObsGroup.ObsName := ScreenObject.Name;
          AnObsGroup.ObservationFormat := ofOBC;
          AnObsGroup.ObsSchedule := AnsiString(ScreenObject.Name);
          for TimeIndex := 0 to SutraStateObs.Count - 1 do
          begin
            AnObsGroup.TimeValues.Add.Value := SutraStateObs[TimeIndex].Time;
          end;

          CellList := TCellAssignmentList.Create;
          try
            ScreenObject.GetCellsToAssign({Mesh,} '0', nil, nil, CellList,
              alFirstVertex, Model);

            GetObservationLocations;
          finally
            CellList.Free;
          end;
        end;
      end;
    end;
  finally
    OctTree.Free;
  end;
end;

class function TSutraObservationWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraObservationWriter.NewLine;
begin
//  inherited;
  FObservationList.Add(FBuffer.ToString);
  FBuffer.Clear;
end;

procedure TSutraObservationWriter.WriteDataSet1;
var
  OutputControl: TSutraOutputControl;
  NOBLIN: Integer;
begin
  OutputControl := Model.SutraOutputControl;
  NOBLIN := OutputControl.MaxObsPerLine;
  WriteInteger(NOBLIN);
  NewLine;
end;

procedure TSutraObservationWriter.WriteFile(const FileName: string; ObservationList: TStringList;
  out NOBS: integer);
var
  ObsRoot: string;
begin
  FObservationList := ObservationList;
  Evaluate;

//  FFileName := FileName;
  FFileName := ChangeFileExt(FileName, '.8d');
//  OpenFile(FFileName);
  try
    WriteDataSet1;
    WriteObservationGroups;
    ObsRoot := ChangeFileExt(FFileName, '');
    if ofOBS in FUsedFormats then
    begin
      SutraFileWriter.AddFile(sftObs, ChangeFileExt(ObsRoot, '.obs'));
    end;
    if ofOBC in FUsedFormats then
    begin
      SutraFileWriter.AddFile(sftObc, ChangeFileExt(ObsRoot, '.obc'));
    end;
//    Model.AddModelInputFile(FFileName);
  finally
    if FBuffer.Length > 0 then
    begin
      NewLine;
    end;
//    CloseFile;
  end;
  NOBS := FNOBS;
end;

procedure TSutraObservationWriter.WriteObservationGroups;
const
  MaxObsNameLength = 40;
var
  GroupIndex: Integer;
  Group: TObsGroup;
  LocationIndex: Integer;
  ObsLocation: TObsLocation;
  OBSNAM: AnsiString;
  OBSSCH: AnsiString;
  OBSFMT: AnsiString;
  MeshType: TMeshType;
  OutputFileName: string;
  GroupName: AnsiString;
begin
  FNOBS := 0;
  MeshType := Model.SutraMesh.MeshType;
  for GroupIndex := 0 to FObsGroups.Count - 1 do
  begin
    Group := FObsGroups[GroupIndex];

    GroupName := Group.ObsName;
//    OBSNAM := '''' + OBSNAM + ''' ';

    OutputFileName := ChangeFileExt(FFileName, '') + '_';

    OBSSCH := '''' + Group.ObsSchedule + ''' ';
    OutputFileName := OutputFileName + string(Group.ObsSchedule);

    case Group.ObservationFormat of
      ofOBS:
        begin
          OBSFMT := ' ''OBS''';
          OutputFileName := ChangeFileExt(OutputFileName, '.OBS');
        end;
      ofOBC:
        begin
          OBSFMT := ' ''OBC''';
          OutputFileName := ChangeFileExt(OutputFileName, '.OBC');
        end
      else Assert(False);
    end;

    Model.AddModelOutputFile(OutputFileName);

    Include(FUsedFormats, Group.ObservationFormat);

    for LocationIndex := 0 to Group.FLocations.Count - 1 do
    begin
      ObsLocation := Group.FLocations[LocationIndex];

      OBSNAM := GroupName;
      if Group.FLocations.Count > 1 then
      begin
        case ObsLocation.ObsType of
          otExact:
            begin
              Assert(False);
            end;
          otNode:
            begin
              OBSNAM := AnsiString(string(OBSNAM) + 'N' + IntToStr(ObsLocation.NodeOrElementNumber));
            end;
          otElement:
            begin
              OBSNAM := AnsiString(string(OBSNAM) + 'E' + IntToStr(ObsLocation.NodeOrElementNumber));
            end;
          else
            Assert(False);
        end;
      end;
    OBSNAM := '''' + OBSNAM + ''' ';
      WriteString(OBSNAM);

      WriteFloat(ObsLocation.X);
      WriteFloat(ObsLocation.Y);
      if MeshType = mt3D then
      begin
        WriteFloat(ObsLocation.Z);
      end;

      WriteString(' ');
      WriteString(OBSSCH);
      WriteString(OBSFMT);
      NewLine;
      Inc(FNOBS);
    end;
  end;
  WriteString('-');
  NewLine;
end;

procedure TSutraObservationWriter.WriteString(const Value: AnsiString);
begin
//  inherited;
  FBuffer.Append(Value);
end;

{ TObsGroup }

constructor TObsGroup.Create;
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FLocations := TObsLocationList.Create;
  InvalidateModelEvent := nil;
  FTimeValues:= TRealCollection.Create(InvalidateModelEvent);
end;

destructor TObsGroup.Destroy;
begin
  FTimeValues.Free;
  FLocations.Free;
  inherited;
end;

function TObsGroup.GetObsName: AnsiString;
begin
  result := FObsName;
end;

procedure TObsGroup.SetObservationFormat(const Value: TObservationFormat);
begin
  FObservationFormat := Value;
end;

procedure TObsGroup.SetObsName(const Value: AnsiString);
const
  MaxObsNameLength = 40;
begin
  FObsName := Copy(Value, 1, MaxObsNameLength);
end;

procedure TObsGroup.SetTimeValues(const Value: TRealCollection);
begin
  FTimeValues.Assign(Value);
end;

{ TObsLocation }

function TObsLocation.GetCenter2D: TPoint2D;
begin
  Result.x := FLocation.x;
  Result.y := FLocation.y;
end;

procedure TObsLocation.SetCenter2D(const Value: TPoint2D);
begin
  FLocation.x := Value.x;
  FLocation.y := Value.y;
end;

end.
