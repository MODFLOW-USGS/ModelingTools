unit Modflow6ObsWriterUnit;

interface

uses
  System.Generics.Collections, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowCellUnit, PhastModelUnit, QuadTreeClass,
  Modflow6ObsUnit, ScreenObjectUnit, System.SysUtils, ModflowMawWriterUnit,
  ModflowSfr6WriterUnit, ModflowLakMf6WriterUnit, System.Classes,
  ModflowUzfMf6WriterUnit, ModflowCSubWriterUnit, Vcl.Forms;

type
  THeadDrawdownObservationLocation = record
    FCell: TCellLocation;
    FName: string;
  end;

  THeadDrawdownObservationLocationList = TList<THeadDrawdownObservationLocation>;

  TFlowObservationLocation = record
    FCell: TCellLocation;
    FOtherCell: TCellLocation;
    FName: string;
  end;

  TCustomMf6ObservationWriter = class(TCustomTransientWriter)
  private
    FDirectObsLines: TStrings;
    FFileNameLines: TStrings;
    FCalculatedObsLines: TStrings;
    procedure WriteOptions;
    procedure WriteCell(Cell: TCellLocation);
    function WriteCellName(Cell: TCellLocation): string;
  protected
    FNameOfFile: string;
    function Package: TModflowPackageSelection; override;
    property DirectObsLines: TStrings read FDirectObsLines write FDirectObsLines;
    property CalculatedObsLines: TStrings read FCalculatedObsLines write FCalculatedObsLines;
    property FileNameLines: TStrings read FFileNameLines write FFileNameLines;
  end;

  TModflow6Obs_Writer = class(TCustomMf6ObservationWriter)
  private
    FHorizontalCells: TList<TCellAssignment>;
    FQuadTree: TRbwQuadTree;
    FNodeNumberQuadTree: TRbwQuadTree;
    function LocationQuadTree: TRbwQuadTree;
    function NodeNumberQuadTree: TRbwQuadTree;
    procedure HandleFlowObs(AScreenObject: TScreenObject; Obs: TModflow6Obs;
      ACell: TCellAssignment);
    procedure WriteFileOut;
  protected
    FHeadObs: THeadDrawdownObservationLocationList;
    FDrawdownObs: THeadDrawdownObservationLocationList;
    FFlowObs: TList<TFlowObservationLocation>;
    class function Extension: string; override;
    procedure Evaluate; override;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; override;
    // @name is the file extension used for the observation output file.
    class function ObservationOutputExtension: string; override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

  TCustomListObsWriter = class(TCustomMf6ObservationWriter)
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  end;

  TModflow6FlowObsWriter = class(TCustomListObsWriter)
  private
    FObsType: string;
    FOutputExtension: string;
    FObsList: TBoundaryFlowObservationLocationList;
    FToMvrObsList: TBoundaryFlowObservationLocationList;
    FObGeneral: TObGeneral;
    procedure WriteFlowObs(ObsType: string;
      List, ToMvrList: TBoundaryFlowObservationLocationList);
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TBoundaryFlowObservationLocationList; ObsType: string;
      ToMvrObsList: TBoundaryFlowObservationLocationList;
      OutputExtension: string; ObGeneral: TObGeneral); reintroduce;
    // @name destroys the current instance of @classname.
    procedure WriteFile(const AFileName: string);
  end;

  TMawObsWriter = class(TCustomMf6ObservationWriter)
  private
    FObsList: TMawObservationList;
    procedure WriteMawObs;
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TMawObservationList); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

  TSfrObsWriter = class(TCustomMf6ObservationWriter)
  private
    FObsList: TSfr6ObservationList;
    procedure WriteSfrObs;
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TSfr6ObservationList); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

  TLakObsWriter = class(TCustomMf6ObservationWriter)
  private
    FObsList: TLakObservationList;
    procedure WriteLakObs;
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TLakObservationList); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

  TUzfObsWriter = class(TCustomMf6ObservationWriter)
  private
    FObsList: TUzfObservationList;
    procedure WriteUzfObs;
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TUzfObservationList); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

  TCSubObsWriter = class(TCustomMf6ObservationWriter)
  private
    FObsList: TCSubObservationList;
    procedure WriteCSubObs;
  protected
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType;
      ObsList: TCSubObservationList); reintroduce;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, DataSetUnit,
  AbstractGridUnit, MeshRenumberingTypes, GoPhastTypes, FastGEO,
  ModflowIrregularMeshUnit, ModflowUnitNumbers, frmErrorsAndWarningsUnit,
  ModflowMawUnit, ModflowSfr6Unit, ModflowLakMf6Unit, ModflowUzfMf6Unit,
  ModflowUzfWriterUnit, PestHeadObsWeightsUnit, ModflowCsubUnit, System.Math;

resourcestring
  StrNoHeadDrawdownO = 'No head, drawdown, or groundwater flow observations ' +
  'defined';
  StrBecauseNoHeadDra = 'Because no head, drawdown, or groundwater flow obse' +
  'rvations have been defined, the OBS6 file for the name file will not be c' +
  'reated.';
  StrEvaluatingHeadDra = 'Evaluating head, drawdown, and groundwater flow ob' +
  'servations.';
  StrWritingOBS6InputF = 'Writing OBS6 input for heads, drawdown, and ground' +
  'water flow.';
  StrWritingFlowObserva = 'Writing flow observations';
  StrWritingMAWObservat = 'Writing MAW observations';
  StrWritingSFRObservat = 'Writing SFR observations';
  StrWritingLAKObservat = 'Writing LAK observations';
  StrNonuniqueSFRObser = 'Non-unique SFR observation names';
  StrTheFollowingSFROb = 'The following SFR observation name "%0:s" is repea' +
  'ted more than once';
  StrWritingUZFObservat = 'Writing UZF observations';
  StrNonuniqueUZFObser = 'Non-unique UZF observation names';
  StrNonuniqueLakeObse = 'Non-unique Lake observation names';
  StrNonuniqueCSUBObse = 'Non-unique CSUB observation names';
  StrTheFollowingCSUBOb = 'The following CSUB observation name "%0:s" is repea' +
  'ted more than once';
  StrTheFollowingUZFOb = 'The following UZF observation name "%0:s" is repea' +
  'ted more than once';
  StrErrorInDefiningHe = 'Error in defining head observation';
  StrSDoesNotDefineA = '%s does not define a head observation because it doe' +
  's not intersect any active cell.';


{ TModflow6Obs_Writer }

constructor TModflow6Obs_Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FHeadObs := THeadDrawdownObservationLocationList.Create;
  FDrawdownObs := THeadDrawdownObservationLocationList.Create;
  FFlowObs := TList<TFlowObservationLocation>.Create;
  FHorizontalCells := TList<TCellAssignment>.Create;
end;

destructor TModflow6Obs_Writer.Destroy;
begin
  FHorizontalCells.Free;
  FQuadTree.Free;
  FNodeNumberQuadTree.Free;
  FFlowObs.Free;
  FDrawdownObs.Free;
  FHeadObs.Free;
  inherited;
end;

function TModflow6Obs_Writer.LocationQuadTree: TRbwQuadTree;
var
  ElementIndex: Integer;
  AnElement: IElement2D;
  PriorNode: INode2D;
  NodeIndex: Integer;
  ANode: INode2D;
  Mesh: IMesh3D;
  MeshLimits: TGridLimit;
begin
  if FQuadTree = nil then
  begin
    Mesh := Model.Mesh3D;
    Assert(Mesh <> nil);
    FQuadTree := TRbwQuadTree.Create(nil);
    MeshLimits := Mesh.MeshLimits(vdTop, 0);
    FQuadTree.XMin := MeshLimits.MinX;
    FQuadTree.YMin := MeshLimits.MinY;
    FQuadTree.XMax := MeshLimits.MaxX;
    FQuadTree.YMax := MeshLimits.MaxY;
    for ElementIndex := 0 to Mesh.Mesh2DI.ElementCount - 1 do
    begin
      AnElement := Mesh.Mesh2DI.ElementsI2D[ElementIndex];
      PriorNode := AnElement.NodesI[AnElement.NodeCount-1];
      for NodeIndex := 0 to AnElement.NodeCount - 1 do
      begin
        ANode := AnElement.NodesI[NodeIndex];
        FQuadTree.AddPoint(ANode.Location.x, ANode.Location.y,
          Pointer(AnElement.ElementNumber));
        FQuadTree.AddPoint(PriorNode.Location.x, PriorNode.Location.y,
          Pointer(AnElement.ElementNumber));
        PriorNode := ANode;
      end;
    end;
  end;
  result := FQuadTree;
end;

function TModflow6Obs_Writer.NodeNumberQuadTree: TRbwQuadTree;
var
  ElementIndex: Integer;
  AnElement: IElement2D;
  PriorNode: INode2D;
  NodeIndex: Integer;
  ANode: INode2D;
  Mesh: IMesh3D;
//  MeshLimits: TGridLimit;
begin
  if FNodeNumberQuadTree = nil then
  begin
    Mesh := Model.Mesh3D;
    Assert(Mesh <> nil);
    FNodeNumberQuadTree := TRbwQuadTree.Create(nil);
    FNodeNumberQuadTree.XMin := 0;
    FNodeNumberQuadTree.YMin := 0;
    FNodeNumberQuadTree.XMax := Mesh.Mesh2DI.NodeCount-1;
    FNodeNumberQuadTree.YMax := Mesh.Mesh2DI.NodeCount-1;
    for ElementIndex := 0 to Mesh.Mesh2DI.ElementCount - 1 do
    begin
      AnElement := Mesh.Mesh2DI.ElementsI2D[ElementIndex];
      PriorNode := AnElement.NodesI[AnElement.NodeCount-1];
      for NodeIndex := 0 to AnElement.NodeCount - 1 do
      begin
        ANode := AnElement.NodesI[NodeIndex];
        FNodeNumberQuadTree.AddPoint(ANode.NodeNumber, PriorNode.NodeNumber,
          Pointer(AnElement.ElementNumber));
//        FNodeNumberQuadTree.AddPoint(PriorNode.NodeNumber, ANode.NodeNumber,
//          Pointer(AnElement.ElementNumber));
        PriorNode := ANode;
      end;
    end;
  end;
  result := FNodeNumberQuadTree;
end;

class function TModflow6Obs_Writer.ObservationExtension: string;
begin
  result := '.ob_gw';
end;

class function TModflow6Obs_Writer.ObservationOutputExtension: string;
begin
  result := '.ob_gw_out';
end;

procedure TModflow6Obs_Writer.Evaluate;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Obs: TModflow6Obs;
  ActiveDataArray: TDataArray;
  Grid: TCustomModelGrid;
  Mesh: IMesh3D;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  HeadDrawdown: THeadDrawdownObservationLocation;
  OuterHorizCellIndex: Integer;
  OuterCell: TCellAssignment;
  InnerHorizCellIndex: Integer;
  InnerCell: TCellAssignment;
  IsNeighbor: Boolean;
  OuterMf6Cell: TModflowIrregularCell2D;
  InnerMf6Cell: TModflowIrregularCell2D;
  FlowObs: TFlowObservationLocation;
  OtherObsDefined: Boolean;
  DisvCell: TModflowIrregularCell2D;
  ObsCells: TObsWeights;
  Width: double;
  Center: double;
  ObservationPoint: TPoint2D;
  ObservationRowOffset: double;
  ObservationColumnOffset: double;
  NeighborCells: TCellLocationList;
  CellDisv2D: TModflowIrregularCell2D;
  NeighborLocation: TCellLocation;
  NeighborIndex: Integer;
  ObsIndex: Integer;
  APoint: TPoint2D;
  ObservationIndex: Integer;
  Observation: TMf6CalibrationObs;
  ObservationName: string;
  InterpolateFormula: TStringBuilder;
  NameIndex: Integer;
  function GetLocation(ACell: TCellLocation): TPoint2D;
  begin
    if Model.DisvUsed then
    begin
      result := Mesh.Mesh2DI.ElementsI2D[ACell.Column].Center;
    end
    else
    begin
      result := Grid.TwoDElementCenter(ACell.Column, ACell.Row);
    end;
  end;
begin
  if Model.PestUsed then
  begin
    // These two properties need to be specified outside of TModflow6Obs_Writer;
    Assert(DirectObsLines <> nil);
    Assert(CalculatedObsLines <> nil);
    Assert(FileNameLines <> nil);
  end;

  ObsIndex := 1;
  ActiveDataArray := Model.DataArrayManager.GetDatasetByName(rsActive);
  Grid := Model.Grid;
  if Grid = nil then
  begin
    Mesh := Model.Mesh3D;
    Assert(Mesh <> nil);
  end;

  OtherObsDefined := False;
  for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    AScreenObject := Model.ScreenObjects[ObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if not AScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Obs := AScreenObject.Modflow6Obs;
    if (Obs = nil) or not Obs.Used then
    begin
      Continue;
    end;
    if (Obs.MawObs <> []) or (Obs.SfrObs <> [])
      or (Obs.LakObs <> []) or (Obs.UzfObs <> [])
      or (Obs.CSubObs.CSubObsSet <> []) then
    begin
      OtherObsDefined := True;
    end;

    if (Obs.General * [ogHead, ogDrawdown] <> [])
      or (Obs.GroundwaterFlowObs and (Obs.GwFlowObsChoices <> [])) then
    begin
      CellList := TCellAssignmentList.Create;
      try
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        FHorizontalCells.Clear;
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];

          if ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, ACell.Column] then
          begin
            if Model.PestUsed and (CellList.Count = 1) and
              (Obs.CalibrationObservations.ObGenerals * [ogHead, ogDrawdown] <> [])  then
            begin
              // find neighbors
              NeighborCells := TCellLocationList.Create;
              try
                if Model.DisvUsed then
                begin
                  ACell := CellList[0];
                  NeighborLocation.Row := 0;
                  DisvCell := Model.DisvGrid.TwoDGrid.Cells[ACell.Column];
                  GetObsWeights(DisvCell, AScreenObject.Points[0], ObsCells, 1e-10);
                  for NeighborIndex := 0 to Length(ObsCells) - 1 do
                  begin
                    CellDisv2D := ObsCells[NeighborIndex];
                    NeighborLocation.Column := CellDisv2D.ElementNumber;
                    NeighborLocation.Layer := ACell.Layer;
                    NeighborCells.Add(NeighborLocation);
                  end;
                end
                else
                begin
                  ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
                    AScreenObject.Points[0]);
                  ACell := CellList[0];

                  Width := Grid.RowWidth[ACell.Row];
                  Center := Grid.RowCenter(ACell.Row);
                  ObservationRowOffset := -(ObservationPoint.y - Center)/Width;

                  Width := Grid.ColumnWidth[ACell.Column];
                  Center := Grid.ColumnCenter(ACell.Column);
                  ObservationColumnOffset := (ObservationPoint.x - Center)/Width;

                  NeighborCells.Add(ACell.Cell);

                  NeighborLocation.Layer := ACell.Layer;
                  if Sign(ObservationColumnOffset) = 0 then
                  begin
                    if Sign(ObservationRowOffset) <> 0 then
                    begin
                      NeighborLocation.Column := ACell.Column;
                      NeighborLocation.Row := ACell.Row + Sign(ObservationRowOffset);
                      if (NeighborLocation.Row >= 0)
                        and (NeighborLocation.Row < Grid.RowCount)
                        And ActiveDataArray.BooleanData[
                          NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                        then
                      begin
                        NeighborCells.Add(NeighborLocation)
                      end;
                    end;
                  end
                  else if Sign(ObservationRowOffset) = 0 then
                  begin
                    NeighborLocation.Column := ACell.Column + Sign(ObservationColumnOffset);
                    NeighborLocation.Row := ACell.Row;
                    if (NeighborLocation.Column >= 0)
                      and (NeighborLocation.Column < Grid.ColumnCount)
                      And ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                  end
                  else if Sign(ObservationRowOffset) = Sign(ObservationColumnOffset) then
                  begin
                    // column direction first
                    NeighborLocation.Column := ACell.Column + Sign(ObservationColumnOffset);
                    NeighborLocation.Row := ACell.Row;
                    if (NeighborLocation.Column >= 0)
                      and (NeighborLocation.Column < Grid.ColumnCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                    NeighborLocation.Row := ACell.Row + Sign(ObservationRowOffset);
                    if (NeighborLocation.Row >= 0)
                      and (NeighborLocation.Row < Grid.RowCount)
                      and (NeighborLocation.Column >= 0)
                      and (NeighborLocation.Column < Grid.ColumnCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                    NeighborLocation.Column := ACell.Column;
                    if (NeighborLocation.Row >= 0)
                      and (NeighborLocation.Row < Grid.RowCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                  end
                  else
                  begin
                    // row direction first
                    NeighborLocation.Column := ACell.Column;
                    NeighborLocation.Row := ACell.Row + Sign(ObservationRowOffset);;
                    if (NeighborLocation.Row >= 0)
                      and (NeighborLocation.Row < Grid.RowCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                    NeighborLocation.Column := ACell.Column + Sign(ObservationColumnOffset);
                    if (NeighborLocation.Column >= 0)
                      and (NeighborLocation.Column < Grid.ColumnCount)
                      and (NeighborLocation.Row >= 0)
                      and (NeighborLocation.Row < Grid.RowCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                    NeighborLocation.Row := ACell.Row;
                    if (NeighborLocation.Column >= 0)
                      and (NeighborLocation.Column < Grid.ColumnCount)
                      and ActiveDataArray.BooleanData[
                        NeighborLocation.Layer, NeighborLocation.Row, NeighborLocation.Column]
                      then
                    begin
                      NeighborCells.Add(NeighborLocation)
                    end;
                  end;
                  for ObservationIndex := 0 to Obs.CalibrationObservations.Count - 1 do
                  begin
                    Observation := Obs.CalibrationObservations[ObservationIndex];
                    Observation.InterpObsNames.Clear;
                  end;
                  for NeighborIndex := 0 to NeighborCells.Count - 1 do
                  begin
                    NeighborLocation := NeighborCells[NeighborIndex];
                    APoint := GetLocation(NeighborLocation);
                    HeadDrawdown.FCell := NeighborLocation;
                    if ogHead in Obs.CalibrationObservations.ObGenerals then
                    begin
                      HeadDrawdown.FName := Obs.Name + '_H' + IntToStr(ObsIndex);
                      FHeadObs.Add(HeadDrawdown);
                      DirectObsLines.Add(Format('  ID %0:s', [HeadDrawdown.FName]));
                      DirectObsLines.Add(Format('  LOCATION %0:g %1:g', [APoint.x, APoint.y]));
                      for ObservationIndex := 0 to Obs.CalibrationObservations.Count - 1 do
                      begin
                        Observation := Obs.CalibrationObservations[ObservationIndex];
                        if (Observation.ObSeries = osGeneral)
                          and (Observation.ObGeneral = ogHead) then
                        begin
                          ObservationName := Format('%0:s_%1:d',
                            [HeadDrawdown.FName, ObservationIndex+1]);
                          DirectObsLines.Add(Format('  OBSNAME %0:s %2:g',
                            [ObservationName, Observation.Time]));
                          Observation.InterpObsNames.Add(ObservationName);
                        end;
                      end;
                    end;
                    if ogDrawdown in Obs.CalibrationObservations.ObGenerals  then
                    begin
                      HeadDrawdown.FName := Obs.Name + '_D' + IntToStr(ObsIndex);
                      FDrawdownObs.Add(HeadDrawdown);
                      DirectObsLines.Add(Format('  ID %0:s', [HeadDrawdown.FName]));
                      DirectObsLines.Add(Format('  LOCATION %0:g %1:g', [APoint.x, APoint.y]));
                      for ObservationIndex := 0 to Obs.CalibrationObservations.Count - 1 do
                      begin
                        Observation := Obs.CalibrationObservations[ObservationIndex];
                        if (Observation.ObSeries = osGeneral)
                          and (Observation.ObGeneral = ogDrawdown) then
                        begin
                          ObservationName := Format('%0:s_%1:d',
                            [HeadDrawdown.FName, ObservationIndex+1]);
                          DirectObsLines.Add(Format('  OBSNAME %0:s %2:g',
                            [ObservationName, Observation.Time]));
                          Observation.InterpObsNames.Add(ObservationName);
                        end;
                      end;
                    end;
                    Inc(ObsIndex);
                  end;
                  for ObservationIndex := 0 to Obs.CalibrationObservations.Count - 1 do
                  begin
                    Observation := Obs.CalibrationObservations[ObservationIndex];
                    if (Observation.ObSeries = osGeneral)
                      and (Observation.ObGeneral in [ogHead, ogDrawdown]) then
                    begin
                      CalculatedObsLines.Add(Format('OBSNAME %s PRINT', [Observation.Name]));
                      InterpolateFormula := TStringBuilder.Create;
                      try
                        InterpolateFormula.Append('INTERPOLATE ');
                        APoint := AScreenObject.Points[0];
                        InterpolateFormula.Append(APoint.x);
                        InterpolateFormula.Append(' ');
                        InterpolateFormula.Append(APoint.y);
                        for NameIndex := 0 to Observation.InterpObsNames.Count - 1 do
                        begin
                          InterpolateFormula.Append(' ');
                          InterpolateFormula.Append(Observation.InterpObsNames[NameIndex]);
                        end;
                        CalculatedObsLines.Add(InterpolateFormula.ToString);
                      finally
                        InterpolateFormula.Free;
                      end;
                    end;
                  end;
                end;
              finally
                NeighborCells.Free;
              end;
            end
            else if (Obs.General * [ogHead, ogDrawdown] <> []) then
            begin
              HeadDrawdown.FCell := ACell.Cell;
              HeadDrawdown.FName := Obs.Name;
              if CellList.Count > 1 then
              begin
                HeadDrawdown.FName := HeadDrawdown.FName
                  + WriteCellName(ACell.Cell);
              end;
              if ogHead in Obs.General then
              begin
                FHeadObs.Add(HeadDrawdown);
              end;
              if ogDrawdown in Obs.General then
              begin
                FDrawdownObs.Add(HeadDrawdown);
              end;
            end;
            if Obs.GroundwaterFlowObs then
            begin
              HandleFlowObs(AScreenObject, Obs, ACell);
            end;
          end;
        end;
        if FHorizontalCells.Count > 1 then
        begin
          for OuterHorizCellIndex := 0 to FHorizontalCells.Count - 2 do
          begin
            OuterCell := FHorizontalCells[OuterHorizCellIndex];
            if Grid <> nil then
            begin
              OuterMf6Cell := nil;
            end
            else
            begin
              OuterMf6Cell := Mesh.Mesh2dI.Elements[OuterCell.Column] as TModflowIrregularCell2D;
            end;

            for InnerHorizCellIndex := OuterHorizCellIndex+1 to FHorizontalCells.Count - 1 do
            begin
              InnerCell := FHorizontalCells[InnerHorizCellIndex];
              IsNeighbor := False;
              if Grid <> nil then
              begin
                IsNeighbor := (OuterCell.Layer = InnerCell.Layer)
                  and (((OuterCell.Row = InnerCell.Row)
                    and (Abs(OuterCell.Column - InnerCell.Column) = 1))
                  or ((OuterCell.Column = InnerCell.Column)
                    and (Abs(OuterCell.Row - InnerCell.Row) = 1)))
              end
              else
              begin
                if OuterCell.Layer = InnerCell.Layer then
                begin
                  InnerMf6Cell := Mesh.Mesh2dI.Elements[InnerCell.Column]
                    as TModflowIrregularCell2D;
                  IsNeighbor := OuterMf6Cell.IsNeighbor(InnerMf6Cell);
                end;
              end;
              if IsNeighbor then
              begin
                FlowObs.FCell := OuterCell.Cell;
                FlowObs.FOtherCell := InnerCell.Cell;
                FlowObs.FName := Obs.Name;
                FFlowObs.Add(FlowObs);
              end;
            end;
          end;
        end;
      finally
        CellList.Free;
      end;
    end;
  end;

  if (FHeadObs.Count = 0) and (FDrawdownObs.Count = 0) and (FFlowObs.Count = 0)
    and not OtherObsDefined then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrNoHeadDrawdownO,
      StrBecauseNoHeadDra);
  end;
end;

class function TModflow6Obs_Writer.Extension: string;
begin
  result := ObservationExtension;
end;

procedure TModflow6Obs_Writer.HandleFlowObs(AScreenObject: TScreenObject;
Obs: TModflow6Obs; ACell: TCellAssignment);
var
  CellOutline: TElementOutline;
  APoint: TPoint2D;
  ASegment: TSegment2D;
  ClosestSegment: Integer;
  ClosestDistance: Double;
  SegmentIndex: Integer;
  TestDistance: Double;
  OtherCol: Integer;
  OtherRow: Integer;
  FlowObs: TFlowObservationLocation;
  AnElement: IElement2D;
  X: Double;
  Y: Double;
  FirstData: TPointerArray;
  SecondData: TPointerArray;
  TestList: System.Generics.Collections.TList<Integer>;
  OtherElementIndex: Integer;
  LocationIndex: Integer;
  AnElementNumber: Integer;
  ActiveDataArray: TDataArray;
  Grid: TCustomModelGrid;
  Mesh: IMesh3D;
  OtherCell: TCellLocation;
  MeshCell: TModflowIrregularCell2D;
  PriorNode: TModflowNode;
  NodeIndex: Integer;
  ANode: TModflowNode;
  StoredElementNumbers: TPointerArray;
  OtherElementNumber: Integer;
begin
  ActiveDataArray := Model.DataArrayManager.GetDatasetByName(rsActive);
  Grid := Model.Grid;
  if Grid = nil then
  begin
    Mesh := Model.Mesh3D;
    Assert(Mesh <> nil);
  end;
  if (gfoNearestNeighbor in Obs.GwFlowObsChoices)
    and not (gfoAllNeighbors in Obs.GwFlowObsChoices) then
  begin
    if AScreenObject.Count = 1 then
    begin
      if Grid <> nil then
      begin
        CellOutline := Grid.TopElementOutline[ACell.Row, ACell.Column];
      end
      else
      begin
        CellOutline := Mesh.Mesh2DI.GetElementOutline(ACell.Column);
      end;
      APoint := AScreenObject.Points[0];
      ASegment := CellOutline.Segments[0];
      ClosestSegment := 0;
      ClosestDistance := Distance(APoint, ASegment);
      for SegmentIndex := 1 to CellOutline.Count - 1 do
      begin
        ASegment := CellOutline.Segments[SegmentIndex];
        TestDistance := Distance(APoint, ASegment);
        if TestDistance < ClosestDistance then
        begin
          ClosestSegment := SegmentIndex;
          ClosestDistance := TestDistance;
        end;
      end;
      if Grid <> nil then
      begin
        OtherCol := ACell.Column;
        OtherRow := ACell.Row;
        case ClosestSegment of
          0:
            begin
              Dec(OtherCol);
            end;
          1:
            begin
              Inc(OtherRow);
            end;
          2:
            begin
              Inc(OtherCol);
            end;
          3:
            begin
              Dec(OtherRow);
            end;
        else
          Assert(False);
        end;
        if (OtherCol >= 0) and (OtherRow >= 0)
          and (OtherCol < Grid.ColumnCount)
          and (OtherRow < Grid.RowCount) then
        begin
          if ActiveDataArray.BooleanData[ACell.Layer, OtherRow, OtherCol] then
          begin
            FlowObs.FCell := ACell.Cell;
            FlowObs.FOtherCell.Layer := ACell.Layer;
            FlowObs.FOtherCell.Row := OtherRow;
            FlowObs.FOtherCell.Column := OtherCol;
            FlowObs.FName := Obs.Name;
            FFlowObs.Add(FlowObs);
          end;
        end;
      end
      else
      begin
        AnElement := Mesh.Mesh2DI.ElementsI2D[ACell.Column];
        ASegment := CellOutline.Segments[ClosestSegment];
        X := ASegment[1].X;
        Y := ASegment[1].Y;
        LocationQuadTree.FindClosestPointsData(X, Y, FirstData);
        X := ASegment[2].X;
        Y := ASegment[2].Y;
        LocationQuadTree.FindClosestPointsData(X, Y, SecondData);
        TestList := TList<Integer>.Create;
        try
          OtherElementIndex := -1;
          for LocationIndex := 0 to Length(FirstData) - 1 do
          begin
            AnElementNumber := Integer(FirstData[LocationIndex]);
            if AnElement.ElementNumber <> AnElementNumber then
            begin
              TestList.Add(AnElementNumber);
            end;
          end;
          for LocationIndex := 0 to Length(SecondData) - 1 do
          begin
            AnElementNumber := Integer(SecondData[LocationIndex]);
            if (AnElement.ElementNumber <> AnElementNumber)
              and (TestList.IndexOf(AnElementNumber) >= 0) then
            begin
              OtherElementIndex := AnElementNumber;
              break;
            end;
          end;
          if OtherElementIndex >= 0 then
          begin
            if ActiveDataArray.BooleanData[ACell.Layer, 0, OtherElementIndex] then
            begin
              FlowObs.FCell := ACell.Cell;
              FlowObs.FOtherCell.Layer := ACell.Layer;
              FlowObs.FOtherCell.Row := 0;
              FlowObs.FOtherCell.Column := OtherElementIndex;
              FlowObs.FName := Obs.Name;
              FFlowObs.Add(FlowObs);
            end;
          end;
        finally
          TestList.Free;
        end;
      end;
    end
    else
    begin
      FHorizontalCells.Add(ACell)
    end;
  end
  else if (gfoAllNeighbors in Obs.GwFlowObsChoices) then
  begin
    if Grid <> nil then
    begin
      OtherCell := ACell.Cell;
      Dec(OtherCell.Column);
      if (OtherCell.Column > 0)
        and ActiveDataArray.BooleanData[OtherCell.Layer, OtherCell.Row,
        OtherCell.Column] then
      begin
        FlowObs.FCell := ACell.Cell;
        FlowObs.FOtherCell := OtherCell;
        FlowObs.FName := Obs.Name;
        FFlowObs.Add(FlowObs);
      end;
      OtherCell := ACell.Cell;
      Inc(OtherCell.Column);
      if (OtherCell.Column < Grid.ColumnCount)
        and ActiveDataArray.BooleanData[OtherCell.Layer, OtherCell.Row,
        OtherCell.Column] then
      begin
        FlowObs.FCell := ACell.Cell;
        FlowObs.FOtherCell := OtherCell;
        FlowObs.FName := Obs.Name;
        FFlowObs.Add(FlowObs);
      end;

      OtherCell := ACell.Cell;
      Dec(OtherCell.Row);
      if (OtherCell.Row > 0)
        and ActiveDataArray.BooleanData[OtherCell.Layer, OtherCell.Row,
        OtherCell.Column] then
      begin
        FlowObs.FCell := ACell.Cell;
        FlowObs.FOtherCell := OtherCell;
        FlowObs.FName := Obs.Name;
        FFlowObs.Add(FlowObs);
      end;
      OtherCell := ACell.Cell;
      Inc(OtherCell.Row);
      if (OtherCell.Row < Grid.RowCount)
        and ActiveDataArray.BooleanData[OtherCell.Layer, OtherCell.Row,
        OtherCell.Column] then
      begin
        FlowObs.FCell := ACell.Cell;
        FlowObs.FOtherCell := OtherCell;
        FlowObs.FName := Obs.Name;
        FFlowObs.Add(FlowObs);
      end;
    end
    else
    begin
      MeshCell := Mesh.Mesh2DI.ElementsI2D[ACell.Column] as TModflowIrregularCell2D;
      PriorNode := MeshCell.ElementCorners[MeshCell.NodeCount-1];
      for NodeIndex := 0 to MeshCell.NodeCount - 1 do
      begin
        ANode := MeshCell.ElementCorners[NodeIndex];
        X := PriorNode.NodeNumber;
        Y := ANode.NodeNumber;

        NodeNumberQuadTree.FindClosestPointsData(X, Y, StoredElementNumbers);
        if (X = PriorNode.NodeNumber) and (Y = ANode.NodeNumber)
          and (Length(StoredElementNumbers) > 0) then
        begin
          Assert(Length(StoredElementNumbers) = 1);
          OtherElementNumber := Integer(StoredElementNumbers[0]);

          FlowObs.FCell := ACell.Cell;
          FlowObs.FOtherCell := ACell.Cell;
          FlowObs.FOtherCell.Column := OtherElementNumber;
          FlowObs.FName := Obs.Name;
          if ActiveDataArray.BooleanData[FlowObs.FOtherCell.Layer,
            FlowObs.FOtherCell.Row, FlowObs.FOtherCell.Column] then
          begin
            FFlowObs.Add(FlowObs);
          end;
        end;

        PriorNode := ANode;
      end;
    end;
  end;
  if (gfoAbove in Obs.GwFlowObsChoices) then
  begin
    FlowObs.FCell := ACell.Cell;
    FlowObs.FOtherCell := ACell.Cell;
    Dec(FlowObs.FOtherCell.Layer);
    FlowObs.FName := Obs.Name;
    if (FlowObs.FOtherCell.Layer >= 0)
      and ActiveDataArray.BooleanData[FlowObs.FOtherCell.Layer,
      FlowObs.FOtherCell.Row, FlowObs.FOtherCell.Column] then
    begin
      FFlowObs.Add(FlowObs);
    end;
  end;
  if (gfoBelow in Obs.GwFlowObsChoices) then
  begin
    FlowObs.FCell := ACell.Cell;
    FlowObs.FOtherCell := ACell.Cell;
    Inc(FlowObs.FOtherCell.Layer);
    FlowObs.FName := Obs.Name;
    if (FlowObs.FOtherCell.Layer < Model.LayerCount)
      and ActiveDataArray.BooleanData[FlowObs.FOtherCell.Layer,
      FlowObs.FOtherCell.Row, FlowObs.FOtherCell.Column] then
    begin
      FFlowObs.Add(FlowObs);
    end;
  end;
end;

procedure TModflow6Obs_Writer.WriteFile(const AFileName: string);
{$IFDEF PEST}
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Obs: TModflow6Obs;
  CellList: TCellAssignmentList;
  ACell: TCellAssignment;
  DisvCell: TModflowIrregularCell2D;
  ObsCells: TObsWeights;
  index: Integer;
  AnotherDisvCell: TModflowIrregularCell2D;
{$ENDIF}
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoHeadDrawdownO);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrErrorInDefiningHe);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrOBS6) then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrEvaluatingHeadDra);
  Evaluate;
  if (FHeadObs.Count = 0) and (FDrawdownObs.Count = 0) and (FFlowObs.Count = 0) then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrOBS6, -1, FNameOfFile, foInput, Model);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingOBS6InputF);
    WriteDataSet0;
    WriteOptions;
    WriteFileOut;

  finally
    CloseFile;
  end;

{$IFDEF PEST}
  if not Model.PestUsed then
  begin
    Exit;
  end;

  FNameOfFile := ChangeFileExt(FNameOfFile, '.pestobs');
  OpenFile(FNameOfFile);
  try
    for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      AScreenObject := Model.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if not AScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Obs := AScreenObject.Modflow6Obs;

      if Obs <> nil then
      begin
        if ogHead in Obs.General then
        begin
          CellList := TCellAssignmentList.Create;
          try
            AScreenObject.GetCellsToAssign('', nil, nil, CellList, alAll, Model);
            if Model.DisvUsed then
            begin
              if CellList.Count > 0 then
              begin
                ACell := CellList[0];
                DisvCell := Model.DisvGrid.TwoDGrid.Cells[ACell.Column];
                GetObsWeights(DisvCell, AScreenObject.Points[0], ObsCells, 1e-10);
                WriteString(AScreenObject.Name);
                WriteInteger(DisvCell.DisplayNumber);
                WriteFloat(AScreenObject.Points[0].x);
                WriteFloat(AScreenObject.Points[0].y);
                NewLine;
                for index := 0 to Length(ObsCells) - 1 do
                begin
                  AnotherDisvCell := ObsCells[index];
                  WriteInteger(AnotherDisvCell.DisplayNumber);
                  WriteFloat(AnotherDisvCell.Location.x);
                  WriteFloat(AnotherDisvCell.Location.y);
                  NewLine;
                end;
                NewLine;
              end
              else
              begin
                frmErrorsAndWarnings.AddWarning(Model, StrErrorInDefiningHe,
                  Format(StrSDoesNotDefineA, [AScreenObject.Name]), AScreenObject);
              end;
            end;
          finally
            CellList.Free;
          end;
        end;
      end;

    end;
  finally
    CloseFile;
  end;
{$ENDIF}
end;

procedure TModflow6Obs_Writer.WriteFileOut;
var
  ObsPackage: TMf6ObservationUtility;
  OutputExtension: string;
  OutputFileName: string;
  obsnam: string;
  FlowObs: TFlowObservationLocation;
  ObsIndex: Integer;
  ObsType: string;
  ObsFileFormat: string;
  procedure WriteHeadDrawdownOutput(ObsType: string; List: THeadDrawdownObservationLocationList);
  var
    ObsIndex: Integer;
    HeadObs: THeadDrawdownObservationLocation;
    OutputExtension: string;
  begin
    if List.count > 0 then
    begin
      ObsFileFormat := '';
      WriteString('BEGIN CONTINUOUS FILEOUT ');
      case ObsPackage.OutputFormat of
        ofText:
          begin
            OutputExtension := ObservationOutputExtension + '_' + ObsType + '.csv';
            ObsFileFormat := 'TEXT';
          end;
        ofBinary:
          begin
            OutputExtension := ObservationOutputExtension + '_' + ObsType + '.bin';
            ObsFileFormat := 'BINARY';
          end;
        else
          Assert(False);
      end;
      OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
      if Model.PestUsed then
      begin
        Assert(FileNameLines <> nil);
        FileNameLines.Add(Format('FILENAME "%0:s" %1:s',
          [OutputFileName, ObsFileFormat]));
      end;
      Model.AddModelOutputFile(OutputFileName);
      OutputFileName := ExtractFileName(OutputFileName);
      WriteString(OutputFileName);
      if ObsPackage.OutputFormat = ofBinary then
      begin
        WriteString(' BINARY');
      end;
      NewLine;

      for ObsIndex := 0 to List.Count - 1 do
      begin
        HeadObs := List[ObsIndex];
        obsnam := HeadObs.FName;
        if obsnam = '' then
        begin
          obsnam := Format('Obs%d', [ObsIndex+1]);
        end;
        Assert(Length(obsnam) <= 40);
        WriteString('  ''');
        WriteString(obsnam);
        WriteString(''' ');
        WriteString(ObsType);
        WriteCell(HeadObs.FCell);
        NewLine;
      end;

      WriteString('END CONTINUOUS');
      NewLine;
      NewLine;
    end
  end;
begin
  ObsPackage := Package as TMf6ObservationUtility;
  WriteHeadDrawdownOutput('head', FHeadObs);
  WriteHeadDrawdownOutput('drawdown', FDrawdownObs);

  if FFlowObs.count > 0 then
  begin
    ObsType := 'flow-ja-face';
    WriteString('BEGIN CONTINUOUS FILEOUT ');
    case ObsPackage.OutputFormat of
      ofText:
        begin
          OutputExtension := ObservationOutputExtension + '_' + ObsType + '.csv';
        end;
      ofBinary:
        begin
          OutputExtension := ObservationOutputExtension + '_' + ObsType + '.bin';
        end;
      else
        Assert(False);
    end;
    OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
    Model.AddModelOutputFile(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    if ObsPackage.OutputFormat = ofBinary then
    begin
      WriteString(' BINARY');
    end;
    NewLine;

    for ObsIndex := 0 to FFlowObs.Count - 1 do
    begin
      FlowObs := FFlowObs[ObsIndex];
      obsnam := FlowObs.FName;
      Assert(Length(obsnam) <= 40);
      WriteString('  ''');
      WriteString(obsnam);
      WriteString(''' ');
      WriteString(ObsType);
      WriteCell(FlowObs.FCell);
      WriteString('   ');
      WriteCell(FlowObs.FOtherCell);
      NewLine;
    end;

    WriteString('END CONTINUOUS');
    NewLine;
  end
end;


{ TCustomMf6ObservationWriter }

procedure TCustomMf6ObservationWriter.WriteOptions;
var
  ObsPackage: TMf6ObservationUtility;
begin
  ObsPackage := Package as TMf6ObservationUtility;

  WriteBeginOptions;

  case ObsPackage.OutputFormat of
    ofText:
      begin
        WriteString('    DIGITS ');
        WriteInteger(ObsPackage.Digits);
        NewLine
      end;
    ofBinary:
      begin
//        WriteString('    PRECISION ');
//        case ObsPackage.OutputPrecision of
//          opSingle:
//            begin
//              WriteString('SINGLE');
//            end;
//          opDouble:
//            begin
//              WriteString('DOUBLE');
//            end;
//          else
//            Assert(False);
//        end;
//        NewLine
      end;
    else
      Assert(False);
  end;

  if Model.ModflowOutputControl.PrintObservations then
  begin
    WriteString('    PRINT_INPUT');
    NewLine;
  end;

  WriteEndOptions;
end;

function TCustomMf6ObservationWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mf6ObservationUtility;
end;

procedure TCustomMf6ObservationWriter.WriteCell(Cell: TCellLocation);
begin
  WriteInteger(Cell.Layer+1);
  if not Model.DisvUsed then
  begin
    WriteInteger(Cell.Row+1);
  end;
  WriteInteger(Cell.Column+1);
end;

function TCustomMf6ObservationWriter.WriteCellName(Cell: TCellLocation): string;
begin
  if Model.DisvUsed then
  begin
    result := Format('_%0:d_%1:d', [Cell.Layer+1, Cell.Column+1]);
  end
  else
  begin
    result := Format('_%0:d_%1:d_%2:d', [Cell.Layer+1, Cell.Row+1, Cell.Column+1]);
  end;
end;


procedure TModflow6FlowObsWriter.WriteFlowObs(ObsType: string;
  List, ToMvrList: TBoundaryFlowObservationLocationList);
var
  ObsIndex: Integer;
//  HeadObs: THeadDrawdownObservationLocation;
  OutputExtension: string;
  FlowObs: TBoundaryFlowObservationLocation;
  ObsPackage: TMf6ObservationUtility;
  OutputFileName: string;
  obsnam: string;
  OutputFormat: string;
begin
  if (List.count > 0) or (ToMvrList.count > 0) then
  begin
    ObsPackage := Package as TMf6ObservationUtility;
    WriteString('BEGIN CONTINUOUS FILEOUT ');
    OutputFormat := '';
    case ObsPackage.OutputFormat of
      ofText:
        begin
          OutputExtension := FOutputExtension + '_' + ObsType + '.csv';
          OutputFormat := 'TEXT';
        end;
      ofBinary:
        begin
          OutputExtension := FOutputExtension + '_' + ObsType + '.bin';
          OutputFormat := 'BINARY';
        end;
      else
        Assert(False);
    end;
    OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
    Model.AddModelOutputFile(OutputFileName);
    if Model.PestUsed then
    begin
      Assert(FileNameLines <> nil);
      FileNameLines.Add(Format('FILENAME "%0:s" %1:s',
        [OutputFileName, OutputFormat]));
    end;
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    if ObsPackage.OutputFormat = ofBinary then
    begin
      WriteString(' BINARY');
    end;
    NewLine;

    for ObsIndex := 0 to List.Count - 1 do
    begin
      FlowObs := List[ObsIndex];
      obsnam := FlowObs.FName;
      if obsnam = '' then
      begin
        obsnam := Format('FlowObs%d', [ObsIndex+1]);
      end;
      Assert(Length(obsnam) <= 40);
      WriteString('  ''');
      WriteString(obsnam);
      WriteString(''' ');
      WriteString(ObsType);
      if FlowObs.FBoundName = '' then
      begin
        WriteCell(FlowObs.FCell);
      end
      else
      begin
        WriteString(' ');
        WriteString(FlowObs.FBoundName);
      end;
      NewLine;

      if FObGeneral in FlowObs.FMf6Obs.CalibrationObservations.ObGenerals then
      begin

      end;
    end;

    for ObsIndex := 0 to ToMvrList.Count - 1 do
    begin
      FlowObs := ToMvrList[ObsIndex];
      obsnam := FlowObs.FName;
      Assert(Length(obsnam) <= 40);
      WriteString('  ''');
      WriteString(obsnam);
      WriteString(''' ');
      WriteString('to-mvr');
      if FlowObs.FBoundName = '' then
      begin
        WriteCell(FlowObs.FCell);
      end
      else
      begin
        WriteString(' ');
        WriteString(FlowObs.FBoundName);
      end;
      NewLine;
    end;

    WriteString('END CONTINUOUS');
    NewLine;
    NewLine;
  end
end;

{ TModflow6FlowObsWriter }

constructor TModflow6FlowObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType;
  ObsList: TBoundaryFlowObservationLocationList;  ObsType: string;
  ToMvrObsList: TBoundaryFlowObservationLocationList;
  OutputExtension: string; ObGeneral: TObGeneral);
begin
  inherited Create(Model, EvaluationType);
  FObGeneral := ObGeneral;
  FObsType := ObsType;
  FOutputExtension := OutputExtension;
  FObsList := ObsList;
  FToMvrObsList := ToMvrObsList;
end;

procedure TCustomListObsWriter.Evaluate;
begin
  // do nothing;

end;

class function TCustomListObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TModflow6FlowObsWriter.WriteFile(const AFileName: string);
begin
//  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoHeadDrawdownO);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingFlowObserva);
  Assert((FObsList.Count > 0) or (FToMvrObsList.Count > 0));
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteFlowObs(FObsType, FObsList, FToMvrObsList);
  finally
    CloseFile;
  end;

end;


{ TMawObsWriter }

constructor TMawObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; ObsList: TMawObservationList);
begin
  inherited Create(Model, EvaluationType);
  FObsList := ObsList;
end;

procedure TMawObsWriter.Evaluate;
begin
  // do nothing;

end;

class function TMawObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TMawObsWriter.WriteFile(const AFileName: string);
begin
//  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoHeadDrawdownO);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingMAWObservat);
  Assert(FObsList.Count > 0);
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteMawObs;
  finally
    CloseFile;
  end;
end;

procedure TMawObsWriter.WriteMawObs;
var
  ObTypes: TMawObs;
  ObsIndex: Integer;
  ObsPackage: TMf6ObservationUtility;
  OutputTypeExtension: string;
  AnObsType: TMawOb;
  OutputExtension: string;
  OutputFileName: string;
  AnObs: TMawObservation;
  obsnam: string;
  ObservationType: string;
  boundname: string;
  IconIndex: Integer;
begin
  ObTypes := [];
  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    ObTypes := ObTypes + FObsList[ObsIndex].FObsTypes;
  end;
  ObsPackage := Package as TMf6ObservationUtility;
  case ObsPackage.OutputFormat of
    ofText:
      begin
        OutputTypeExtension := '.csv';
      end;
    ofBinary:
      begin
        OutputTypeExtension := '.bin';
      end;
    else
      Assert(False);
  end;
  for AnObsType in ObTypes do
  begin
    case AnObsType of
      moHead:
        begin
          OutputExtension := '.maw_head_ob' + OutputTypeExtension;
          ObservationType := 'head';
        end;
      moFromMvr:
        begin
          OutputExtension := '.maw_from_mvr_ob' + OutputTypeExtension;
          ObservationType := 'from-mvr';
        end;
      moFlowRate:
        begin
          OutputExtension := '.maw_flow_rate_ob' + OutputTypeExtension;
          ObservationType := 'maw';
        end;
      moFlowRateCells:
        begin
          OutputExtension := '.maw_cell_flow_rate_ob' + OutputTypeExtension;
          ObservationType := 'maw';
        end;
      moPumpRate:
        begin
          OutputExtension := '.maw_pump_rate_ob' + OutputTypeExtension;
          ObservationType := 'rate';
        end;
      moRateToMvr:
        begin
          OutputExtension := '.maw_pump_rate_to_mvr_ob' + OutputTypeExtension;
          ObservationType := 'rate-to-mvr';
        end;
      moFlowingWellFlowRate:
        begin
          OutputExtension := '.maw_flowing_well_rate_ob' + OutputTypeExtension;
          ObservationType := 'fw-rate';
        end;
      moFlowWellToMvr:
        begin
          OutputExtension := '.maw_flowing_well_to_mvr_ob' + OutputTypeExtension;
          ObservationType := 'fw-to-mvr';
        end;
      moStorageFlowRate:
        begin
          OutputExtension := '.maw_storage_flow_rate_ob' + OutputTypeExtension;
          ObservationType := 'storage';
        end;
      moConstantFlowRate:
        begin
          OutputExtension := '.maw_constant_flow_rate_ob' + OutputTypeExtension;
          ObservationType := 'constant';
        end;
      moConductance:
        begin
          OutputExtension := '.maw_conductance_ob' + OutputTypeExtension;
          ObservationType := 'conductance';
        end;
      moConductanceCells:
        begin
          OutputExtension := '.maw_cell_conductance_ob' + OutputTypeExtension;
          ObservationType := 'conductance';
        end;
      moFlowingWellConductance:
        begin
          OutputExtension := '.maw_flowing_well_conductance_ob' + OutputTypeExtension;
          ObservationType := 'fw-conductance';
        end;
      else
        Assert(False);
    end;
    WriteString('BEGIN CONTINUOUS FILEOUT ');
    OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
    Model.AddModelOutputFile(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    if ObsPackage.OutputFormat = ofBinary then
    begin
      WriteString(' BINARY');
    end;
    NewLine;

    for ObsIndex := 0 to FObsList.Count - 1 do
    begin
      AnObs := FObsList[ObsIndex];
      if AnObsType in AnObs.FObsTypes then
      begin
        obsnam := AnObs.FName;
        if obsnam = '' then
        begin
          obsnam := Format('MawObs%d', [ObsIndex+1]);
        end;

        frmProgressMM.AddMessage(Format('  Exporting %s', [obsnam]));
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        Assert(Length(obsnam) <= 40);
        boundname := Trim(AnObs.FBoundName);
        boundname := Copy(boundname, 1, 40);
        boundname := ' ' + boundname + ' ';
        if AnObsType in [moFlowRateCells, moConductanceCells] then
        begin
          for IconIndex := 1 to AnObs.FCount do
          begin
            WriteString(' ''');
            WriteString(obsnam);
            WriteString('_');
            WriteString(IntToStr(IconIndex));
            WriteString(''' ');
            WriteString(ObservationType);
            WriteInteger(AnObs.FWellNumber);
            WriteInteger(IconIndex);
            NewLine;
          end;
        end
        else
        begin
          obsnam := ' ''' + obsnam + ''' ';
          WriteString(obsnam);
          WriteString(ObservationType);
          WriteString(boundname);
          NewLine;
        end;
      end;
    end;

    WriteString('END CONTINUOUS');
    NewLine;
    NewLine;
  end;
end;

{ TSfrObsWriter }

constructor TSfrObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; ObsList: TSfr6ObservationList);
begin
  inherited Create(Model, EvaluationType);
  FObsList := ObsList;
end;

procedure TSfrObsWriter.Evaluate;
begin
  // do nothing
end;

class function TSfrObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TSfrObsWriter.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingSFRObservat);
  Assert(FObsList.Count > 0);
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteSfrObs;
  finally
    CloseFile;
  end;
end;

procedure TSfrObsWriter.WriteSfrObs;
var
  ObTypes: TSfrObs;
  ObsIndex: Integer;
  ObsPackage: TMf6ObservationUtility;
  OutputTypeExtension: string;
  AnObsType: TSfrOb;
  OutputExtension: string;
  OutputFileName: string;
  AnObs: TSfr6Observation;
  obsnam: string;
  ObservationType: string;
  boundname: string;
  ReachIndex: Integer;
  ReachNumber: Integer;
  ReachNumberStr: string;
  ObsNames: TStringList;
  Root: string;
  procedure CheckForDuplicateObsNames;
  begin
    if ObsNames.IndexOf(obsnam) >= 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNonuniqueSFRObser,
        Format(StrTheFollowingSFROb, [obsnam]));
    end
    else
    begin
      ObsNames.Add(obsnam);
    end;
  end;
begin
  ObTypes := [];
  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    ObTypes := ObTypes + FObsList[ObsIndex].FObsTypes;
  end;
  ObsPackage := Package as TMf6ObservationUtility;
  case ObsPackage.OutputFormat of
    ofText:
      begin
        OutputTypeExtension := '.csv';
      end;
    ofBinary:
      begin
        OutputTypeExtension := '.bin';
      end;
    else
      Assert(False);
  end;
  ObsNames := TStringList.Create;
  try
    ObsNames.Sorted := True;
    for AnObsType in ObTypes do
    begin
      case AnObsType of
        soStage:
          begin
            OutputExtension := '.sfr_stage_ob' + OutputTypeExtension;
            ObservationType := 'stage';
          end;
        soExtInflow:
          begin
            OutputExtension := '.sfr_ext-inflow_ob' + OutputTypeExtension;
            ObservationType := 'ext-inflow';
          end;
        soInflow:
          begin
            OutputExtension := '.sfr_inflow_ob' + OutputTypeExtension;
            ObservationType := 'inflow';
          end;
        soFromMvr:
          begin
            OutputExtension := '.sfr_from_mvr_ob' + OutputTypeExtension;
            ObservationType := 'from-mvr';
          end;
        soRainfall:
          begin
            OutputExtension := '.sfr_rainfall_ob' + OutputTypeExtension;
            ObservationType := 'rainfall';
          end;
        soRunoff:
          begin
            OutputExtension := '.sfr_runoff_ob' + OutputTypeExtension;
            ObservationType := 'runoff';
          end;
        soSfr:
          begin
            OutputExtension := '.sfr_gw_exchange_ob' + OutputTypeExtension;
            ObservationType := 'sfr';
          end;
        soEvaporation:
          begin
            OutputExtension := '.sfr_evaporation_ob' + OutputTypeExtension;
            ObservationType := 'evaporation';
          end;
        soOutflow:
          begin
            OutputExtension := '.sfr_outflow_ob' + OutputTypeExtension;
            ObservationType := 'outflow';
          end;
        soExternalOutflow:
          begin
            OutputExtension := '.sfr_ext-outflow_ob' + OutputTypeExtension;
            ObservationType := 'ext-outflow';
          end;
        soToMvr:
          begin
            OutputExtension := '.sfr_to_mvr_ob' + OutputTypeExtension;
            ObservationType := 'to-mvr';
          end;
        soUpstreamFlow:
          begin
            OutputExtension := '.sfr_upstream-flow_ob' + OutputTypeExtension;
            ObservationType := 'upstream-flow';
          end;
        soDownstreamFlow:
          begin
            OutputExtension := '.sfr_downstream-flow_ob' + OutputTypeExtension;
            ObservationType := 'downstream-flow';
          end;
        else
          Assert(False);
      end;
      WriteString('BEGIN CONTINUOUS FILEOUT ');
      OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
      Model.AddModelOutputFile(OutputFileName);
      OutputFileName := ExtractFileName(OutputFileName);
      WriteString(OutputFileName);
      if ObsPackage.OutputFormat = ofBinary then
      begin
        WriteString(' BINARY');
      end;
      NewLine;

      for ObsIndex := 0 to FObsList.Count - 1 do
      begin
        AnObs := FObsList[ObsIndex];
        if AnObsType in AnObs.FObsTypes then
        begin
          Root := AnObs.FName;
          if Root = '' then
          begin
            Root := Format('SfrObs%d', [ObsIndex+1]);
          end;
          Assert(Length(Root) <= 40);
          boundname := Trim(AnObs.FBoundName);
          boundname := Copy(boundname, 1, 40);
          boundname := ' ' + boundname + ' ';
          case AnObs.FSfrObsLocation of
            solAll:
              begin
                if AnObsType = soStage then
                begin
                  ReachNumberStr := IntToStr(AnObs.FCount + AnObs.FReachStart);
                  While Length(Root) + 1 + Length(ReachNumberStr) > 40 do
                  begin
                    Root := Copy(Root, 1, Length(Root)-1);
                  end;

//                  Root := Root
                  for ReachIndex := 1 to AnObs.FCount do
                  begin
                    ReachNumber := ReachIndex + AnObs.FReachStart;
                    WriteString(' ''');
                    obsnam := Root + '_' + IntToStr(ReachNumber);
                    WriteString(obsnam);
                    WriteString(''' ');
                    WriteString(ObservationType);
                    WriteInteger(ReachNumber);
                    NewLine;
                    CheckForDuplicateObsNames;
                  end;
                end
                else
                begin
                  obsnam := Root;
                  WriteString(' ''');
                  WriteString(obsnam);
                  WriteString(''' ');
                  WriteString(ObservationType);
                  WriteString(boundname);
                  NewLine;
                  CheckForDuplicateObsNames;
                end;
              end;
            solFirst:
              begin
                ReachNumber := 1 + AnObs.FReachStart;
                ReachNumberStr := IntToStr(ReachNumber);
                While Length(Root) + 1 + Length(ReachNumberStr) > 40 do
                begin
                  Root := Copy(Root, 1, Length(Root)-1);
                end;
                obsnam := Root;
                WriteString(' ''');
                WriteString(obsnam);
                WriteString(''' ');
                WriteString(ObservationType);
                WriteInteger(ReachNumber);
                NewLine;
                CheckForDuplicateObsNames;
              end;
            solLast:
              begin
                ReachNumber := AnObs.FCount + AnObs.FReachStart;
                ReachNumberStr := IntToStr(ReachNumber);
                While Length(Root) + 1 + Length(ReachNumberStr) > 40 do
                begin
                  Root := Copy(Root, 1, Length(Root)-1);
                end;
                obsnam := Root;
                WriteString(' ''');
                WriteString(obsnam);
                WriteString(''' ');
                WriteString(ObservationType);
                WriteInteger(ReachNumber);
                NewLine;
                CheckForDuplicateObsNames;
              end;
            solIndividual:
              begin
                ReachNumberStr := IntToStr(AnObs.FCount + AnObs.FReachStart);
                While Length(Root) + 1 + Length(ReachNumberStr) > 40 do
                begin
                  Root := Copy(Root, 1, Length(Root)-1);
                end;
                for ReachIndex := 1 to AnObs.FCount do
                begin
                  ReachNumber := ReachIndex + AnObs.FReachStart;
                  WriteString(' ''');
                  obsnam := Root + '_' + IntToStr(ReachNumber);
                  WriteString(obsnam);
                  WriteString(''' ');
                  WriteString(ObservationType);
                  WriteInteger(ReachNumber);
                  NewLine;
                end;
                CheckForDuplicateObsNames;
              end
            else
              Assert(False);
          end;
        end;
      end;

      WriteString('END CONTINUOUS');
      NewLine;
      NewLine;
    end;
  finally
    ObsNames.Free;
  end;
end;

{ TLakObsWriter }

constructor TLakObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; ObsList: TLakObservationList);
begin
  inherited Create(Model, EvaluationType);
  FObsList := ObsList;
end;

procedure TLakObsWriter.Evaluate;
begin
  // do nothing
end;

class function TLakObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TLakObsWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNonuniqueLakeObse);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingLAKObservat);
  Assert(FObsList.Count > 0);
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteLakObs;
  finally
    CloseFile;
  end;
end;

procedure TLakObsWriter.WriteLakObs;
var
  ObTypes: TLakObs;
  ObsIndex: Integer;
  ObsPackage: TMf6ObservationUtility;
  OutputTypeExtension: string;
  AnObsType: TLakOb;
  OutputExtension: string;
  OutputFileName: string;
  AnObs: TLakObservation;
  obsnam: string;
  ObservationType: string;
  boundname: string;
begin
  ObTypes := [];
  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    ObTypes := ObTypes + FObsList[ObsIndex].FObsTypes;
  end;
  ObsPackage := Package as TMf6ObservationUtility;
  case ObsPackage.OutputFormat of
    ofText:
      begin
        OutputTypeExtension := '.csv';
      end;
    ofBinary:
      begin
        OutputTypeExtension := '.bin';
      end;
    else
      Assert(False);
  end;
  for AnObsType in ObTypes do
  begin
    case AnObsType of
      loStage:
        begin
          OutputExtension := '.lak_stage_ob' + OutputTypeExtension;
          ObservationType := 'stage';
        end;
      loExternalInflow:
        begin
          OutputExtension := '.lak_ext-inflow_ob' + OutputTypeExtension;
          ObservationType := 'ext-inflow';
        end;
      loSimOutletInflow:
        begin
          OutputExtension := '.lak_outlet-inflow_ob' + OutputTypeExtension;
          ObservationType := 'outlet-inflow';
        end;
      loSumInflow:
        begin
          OutputExtension := '.lak_inflow_ob' + OutputTypeExtension;
          ObservationType := 'inflow';
        end;
      loFromMvr:
        begin
          OutputExtension := '.lak_from_MVR_ob' + OutputTypeExtension;
          ObservationType := 'from-mvr';
        end;
      loRain:
        begin
          OutputExtension := '.lak_rainfall_ob' + OutputTypeExtension;
          ObservationType := 'rainfall';
        end;
      loRunoff:
        begin
          OutputExtension := '.lak_runoff_ob' + OutputTypeExtension;
          ObservationType := 'runoff';
        end;
      loFlowRate:
        begin
          OutputExtension := '.lak_flow_ob' + OutputTypeExtension;
          ObservationType := 'lak';
        end;
      loWithdrawal:
        begin
          OutputExtension := '.lak_withdrawal_ob' + OutputTypeExtension;
          ObservationType := 'withdrawal';
        end;
      loEvap:
        begin
          OutputExtension := '.lak_evaporation_ob' + OutputTypeExtension;
          ObservationType := 'evaporation';
        end;
      loExternalOutflow:
        begin
          OutputExtension := '.lak_ext-outflow_ob' + OutputTypeExtension;
          ObservationType := 'ext-outflow';
        end;
      loToMvr:
        begin
          OutputExtension := '.lak_to_mvr_ob' + OutputTypeExtension;
          ObservationType := 'to-mvr';
        end;
      loStorage:
        begin
          OutputExtension := '.lak_storage_ob' + OutputTypeExtension;
          ObservationType := 'storage';
        end;
      loConstantFlow:
        begin
          OutputExtension := '.lak_constant_flow_ob' + OutputTypeExtension;
          ObservationType := 'constant';
        end;
      loOutlet:
        begin
          OutputExtension := '.lak_outlet_ob' + OutputTypeExtension;
          ObservationType := 'outlet';
        end;
      loVolume:
        begin
          OutputExtension := '.lak_volume_ob' + OutputTypeExtension;
          ObservationType := 'volume';
        end;
      loSurfaceArea:
        begin
          OutputExtension := '.lak_surface-area_ob' + OutputTypeExtension;
          ObservationType := 'surface-area';
        end;
      loWettedArea:
        begin
          OutputExtension := '.lak_wetted-area_ob' + OutputTypeExtension;
          ObservationType := 'wetted-area';
        end;
      loConductance:
        begin
          OutputExtension := '.lak_conductance_ob' + OutputTypeExtension;
          ObservationType := 'conductance';
        end;
    end;

    WriteString('BEGIN CONTINUOUS FILEOUT ');
    OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
    Model.AddModelOutputFile(OutputFileName);
    OutputFileName := ExtractFileName(OutputFileName);
    WriteString(OutputFileName);
    if ObsPackage.OutputFormat = ofBinary then
    begin
      WriteString(' BINARY');
    end;
    NewLine;

    for ObsIndex := 0 to FObsList.Count - 1 do
    begin
      AnObs := FObsList[ObsIndex];
      if AnObsType in AnObs.FObsTypes then
      begin
        obsnam := AnObs.FName;
        if obsnam = '' then
        begin
          obsnam := Format('Lak%d', [ObsIndex+1]);
        end;
        Assert(Length(obsnam) <= 40);
        boundname := Trim(AnObs.FBoundName);
        boundname := Copy(boundname, 1, 40);
        boundname := ' ' + boundname + ' ';
        obsnam := ' ''' + obsnam + ''' ';
        WriteString(obsnam);
        WriteString(ObservationType);
        WriteString(boundname);
        NewLine;
      end;
    end;

    WriteString('END CONTINUOUS');
    NewLine;
    NewLine;
  end;
end;

{ TUzfObsWriter }

constructor TUzfObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; ObsList: TUzfObservationList);
begin
  inherited Create(Model, EvaluationType);
  FObsList := ObsList;
end;

procedure TUzfObsWriter.Evaluate;
begin
  // do nothing
end;

class function TUzfObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TUzfObsWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNonuniqueUZFObser);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingUZFObservat);
  Assert(FObsList.Count > 0);
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteUzfObs;
  finally
    CloseFile;
  end;
end;

procedure TUzfObsWriter.WriteUzfObs;
var
  ObTypes: TUzfObs;
  ObsIndex: Integer;
  ObsPackage: TMf6ObservationUtility;
  OutputTypeExtension: string;
  AnObsType: TUzfOb;
  OutputExtension: string;
  OutputFileName: string;
  AnObs: TUzfObservation;
  obsnam: string;
  ObservationType: string;
  boundname: string;
  ObsNames: TStringList;
  Root: string;
  CellIndex: Integer;
  obsname: string;
  procedure CheckForDuplicateObsNames;
  begin
    if ObsNames.IndexOf(obsnam) >= 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNonuniqueUZFObser,
        Format(StrTheFollowingUZFOb, [obsnam]));
    end
    else
    begin
      ObsNames.Add(obsnam);
    end;
  end;
begin
  ObTypes := [];
  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    ObTypes := ObTypes + FObsList[ObsIndex].FObsTypes;
  end;
  ObsPackage := Package as TMf6ObservationUtility;
  case ObsPackage.OutputFormat of
    ofText:
      begin
        OutputTypeExtension := '.csv';
      end;
    ofBinary:
      begin
        OutputTypeExtension := '.bin';
      end;
    else
      Assert(False);
  end;
  ObsNames := TStringList.Create;
  try
    ObsNames.Sorted := True;
    for AnObsType in ObTypes do
    begin
      case AnObsType of
        uoGW_Recharge:
          begin
            OutputExtension := '.uzf-gwrch_ob' + OutputTypeExtension;
            ObservationType := 'uzf-gwrch';
          end;
        uoGW_Discharge:
          begin
            OutputExtension := '.uzf-gwd_ob' + OutputTypeExtension;
            ObservationType := 'uzf-gwd';
          end;
        uoDischargeToMvr:
          begin
            OutputExtension := '.uzf-gwd-to-mvr_ob' + OutputTypeExtension;
            ObservationType := 'uzf-gwd-to-mvr';
          end;
        uoSatZoneEvapotranspiration:
          begin
            OutputExtension := '.uzf-gwet_ob' + OutputTypeExtension;
            ObservationType := 'uzf-gwet';
          end;
        uoInfiltration:
          begin
            OutputExtension := '.infiltration_ob' + OutputTypeExtension;
            ObservationType := 'infiltration';
          end;
        uoMvrInflow:
          begin
            OutputExtension := '.from-mvr_ob' + OutputTypeExtension;
            ObservationType := 'from-mvr';
          end;
        uoRejectInfiltration:
          begin
            OutputExtension := '.rej-inf_ob' + OutputTypeExtension;
            ObservationType := 'rej-inf';
          end;
        uoRejectInfiltrationToMvr:
          begin
            OutputExtension := '.rej-inf-to-mvr_ob' + OutputTypeExtension;
            ObservationType := 'rej-inf-to-mvr';
          end;
        uoUnsatZoneEvapotranspiration:
          begin
            OutputExtension := '.uzet_ob' + OutputTypeExtension;
            ObservationType := 'uzet';
          end;
        uoStorage:
          begin
            OutputExtension := '.storage_ob' + OutputTypeExtension;
            ObservationType := 'storage';
          end;
        uoNetInfiltration:
          begin
            OutputExtension := '.net-infiltration_ob' + OutputTypeExtension;
            ObservationType := 'net-infiltration';
          end;
        uoWaterContent:
          begin
            OutputExtension := '.water-content_ob' + OutputTypeExtension;
            ObservationType := 'water-content';
          end;
      end;

      WriteString('BEGIN CONTINUOUS FILEOUT ');
      OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
      Model.AddModelOutputFile(OutputFileName);
      OutputFileName := ExtractFileName(OutputFileName);
      WriteString(OutputFileName);
      if ObsPackage.OutputFormat = ofBinary then
      begin
        WriteString(' BINARY');
      end;
      NewLine;

      for ObsIndex := 0 to FObsList.Count - 1 do
      begin
        AnObs := FObsList[ObsIndex];
        if AnObsType in AnObs.FObsTypes then
        begin
          Root := AnObs.FName;
          if Root = '' then
          begin
            Root := Format('UzfObs%d', [ObsIndex+1]);
          end;
          Assert(Length(Root) <= 40);
          boundname := Trim(AnObs.FBoundName);
          boundname := Copy(boundname, 1, 40);
          boundname := ' ' + boundname + ' ';

          if AnObs.FCells = nil then
          begin
            if AnObsType = uoWaterContent then
            begin
              for CellIndex := 0 to Length(AnObs.FUzfBoundNumber) - 1 do
              begin
                obsname := '  ' + Root
                  + IntToStr(AnObs.FUzfBoundNumber[CellIndex]) + ' ';
                WriteString(obsname);
                WriteString(ObservationType);
                WriteInteger(AnObs.FUzfBoundNumber[CellIndex]);
                WriteFloat(AnObs.FDepthFractions[CellIndex]);
                NewLine;
              end;
            end
            else
            begin
              obsname := '  ' + Root + ' ';
              WriteString(obsname);
              WriteString(ObservationType);
              WriteString(boundname);
              NewLine;
            end;
          end
          else
          begin
            for CellIndex := 0 to Length(AnObs.FUzfBoundNumber) - 1 do
            begin
              obsname := '  ' + Root
                + IntToStr(AnObs.FUzfBoundNumber[CellIndex]) + ' ';
              WriteString(obsname);
              WriteString(ObservationType);
              WriteInteger(AnObs.FUzfBoundNumber[CellIndex]);
              if AnObsType = uoWaterContent then
              begin
                WriteFloat(AnObs.FDepthFractions[CellIndex]);
              end;
              NewLine;
            end;
          end;
        end;
      end;

      WriteString('END CONTINUOUS');
      NewLine;
      NewLine;
    end;
  finally
    ObsNames.Free;
  end;
end;

{ TCSubObsWriter }

constructor TCSubObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType; ObsList: TCSubObservationList);
begin
  inherited Create(Model, EvaluationType);
  FObsList := ObsList;
end;

procedure TCSubObsWriter.Evaluate;
begin
//  do nothing

end;

class function TCSubObsWriter.Extension: string;
begin
  Result := '';
  Assert(False);
end;

procedure TCSubObsWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNonuniqueCSUBObse);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := AFileName;

  frmProgressMM.AddMessage(StrWritingUZFObservat);
  Assert(FObsList.Count > 0);
  Model.AddModelInputFile(FNameOfFile);

  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    WriteOptions;
    WriteCSubObs;
  finally
    CloseFile;
  end;
end;

procedure TCSubObsWriter.WriteCSubObs;
var
  ObTypes: TSubObsSet;
  ObsIndex: Integer;
  ObsPackage: TMf6ObservationUtility;
  OutputTypeExtension: string;
  AnObsType: TCSubOb;
  OutputExtension: string;
  OutputFileName: string;
  AnObs: TCSubObservation;
  obsnam: string;
  ObservationType: string;
  boundname: string;
  ObsNames: TStringList;
  Root: string;
  CellIndex: Integer;
  obsname: string;
  ACell: TCellLocation;
  DisvUsed: Boolean;
  IBIndex: Integer;
  icsubno: Integer;
  ScreenObject: TScreenObject;
  DelayBedIndex: Integer;
  idcellno: Integer;
  NDELAYCELLS: Integer;
  procedure CheckForDuplicateObsNames;
  begin
    if ObsNames.IndexOf(obsnam) >= 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrNonuniqueCSUBObse,
        Format(StrTheFollowingCSUBOb, [obsnam]));
    end
    else
    begin
      ObsNames.Add(obsnam);
    end;
  end;
begin
  DisvUsed := Model.DisvUsed;
  ObTypes := [];
  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    ObTypes := ObTypes + FObsList[ObsIndex].FObsTypes;
  end;
  ObsPackage := Package as TMf6ObservationUtility;
  NDELAYCELLS := Model.ModflowPackages.CSubPackage.NumberOfDelayCells;
  case ObsPackage.OutputFormat of
    ofText:
      begin
        OutputTypeExtension := '.csv';
      end;
    ofBinary:
      begin
        OutputTypeExtension := '.bin';
      end;
    else
      Assert(False);
  end;
  ObsNames := TStringList.Create;
  try
    ObsNames.Sorted := True;
    for AnObsType in ObTypes do
    begin
      case AnObsType of
        coCSub:
          begin
            OutputExtension := '.csub_ob' + OutputTypeExtension;
            ObservationType := 'csub';
          end;
        coInelastCSub:
          begin
            OutputExtension := '.inelastic-csub_ob' + OutputTypeExtension;
            ObservationType := 'inelastic-csub';
          end;
        coElastCSub:
          begin
            OutputExtension := '.elastic-csub_ob' + OutputTypeExtension;
            ObservationType := 'elastic-csub';
          end;
        coCoarseCSub:
          begin
            OutputExtension := '.coarse-csub_ob' + OutputTypeExtension;
            ObservationType := 'coarse-csub';
          end;
        coCSubCell:
          begin
            OutputExtension := '.csub-cell_ob' + OutputTypeExtension;
            ObservationType := 'csub-cell';
          end;
        coWcompCSubCell:
          begin
            OutputExtension := '.wcomp-csub-cell_ob' + OutputTypeExtension;
            ObservationType := 'wcomp-csub-cell';
          end;
        coSk:
          begin
            OutputExtension := '.sk_ob' + OutputTypeExtension;
            ObservationType := 'sk';
          end;
        coSke:
          begin
            OutputExtension := '.ske_ob' + OutputTypeExtension;
            ObservationType := 'ske';
          end;
        coSkCell:
          begin
            OutputExtension := '.sk-cell_ob' + OutputTypeExtension;
            ObservationType := 'sk-cell';
          end;
        coSkeCell:
          begin
            OutputExtension := '.ske-cell_ob' + OutputTypeExtension;
            ObservationType := 'ske-cell';
          end;
        coEStressCell:
          begin
            OutputExtension := '.estress-cell_ob' + OutputTypeExtension;
            ObservationType := 'estress-cell';
          end;
        coGStressCell:
          begin
            OutputExtension := '.gstress-cell_ob' + OutputTypeExtension;
            ObservationType := 'gstress-cell';
          end;
        coIntbedComp:
          begin
            OutputExtension := '.interbed-compaction_ob' + OutputTypeExtension;
            ObservationType := 'interbed-compaction';
          end;
        coInelastComp:
          begin
            OutputExtension := '.inelastic-compaction_ob' + OutputTypeExtension;
            ObservationType := 'inelastic-compaction';
          end;
        coElastComp:
          begin
            OutputExtension := '.elastic-compaction_ob' + OutputTypeExtension;
            ObservationType := 'elastic-compaction';
          end;
        coCoarseCompaction:
          begin
            OutputExtension := '.coarse-compaction_ob' + OutputTypeExtension;
            ObservationType := 'coarse-compaction';
          end;
        coCompCell:
          begin
            OutputExtension := '.compaction-cell_ob' + OutputTypeExtension;
            ObservationType := 'compaction-cell';
          end;
        coThickness:
          begin
            OutputExtension := '.thickness_ob' + OutputTypeExtension;
            ObservationType := 'thickness';
          end;
        coCoarseThickness:
          begin
            OutputExtension := '.coarse-thickness_ob' + OutputTypeExtension;
            ObservationType := 'coarse-thickness';
          end;
        coThickCell:
          begin
            OutputExtension := '.thickness-cell_ob' + OutputTypeExtension;
            ObservationType := 'thickness-cell';
          end;
        coTheta:
          begin
            OutputExtension := '.theta_ob' + OutputTypeExtension;
            ObservationType := 'theta';
          end;
        coCoarseTheta:
          begin
            OutputExtension := '.coarse-theta_ob' + OutputTypeExtension;
            ObservationType := 'coarse-theta';
          end;
        coThetaCell:
          begin
            OutputExtension := '.theta-cell_ob' + OutputTypeExtension;
            ObservationType := 'theta-cell';
          end;
        coDelayFlowTop:
          begin
            OutputExtension := '.delay-flowtop_ob' + OutputTypeExtension;
            ObservationType := 'delay-flowtop';
          end;
        coDelayFlowBot:
          begin
            OutputExtension := '.delay-flowbot_ob' + OutputTypeExtension;
            ObservationType := 'delay-flowbot';
          end;
        coDelayHead:
          begin
            OutputExtension := '.delay-head_ob' + OutputTypeExtension;
            ObservationType := 'delay-head';
          end;
        coDelayGStress:
          begin
            OutputExtension := '.delay-gstress_ob' + OutputTypeExtension;
            ObservationType := 'delay-gstress';
          end;
        coDelayEStress:
          begin
            OutputExtension := '.delay-estress_ob' + OutputTypeExtension;
            ObservationType := 'delay-estress';
          end;
        coDelayPreConStress:
          begin
            OutputExtension := '.delay-preconstress_ob' + OutputTypeExtension;
            ObservationType := 'delay-preconstress';
          end;
        coDelayComp:
          begin
            OutputExtension := '.delay-compaction_ob' + OutputTypeExtension;
            ObservationType := 'delay-compaction';
          end;
        coDelayThickness:
          begin
            OutputExtension := '.delay-thickness_ob' + OutputTypeExtension;
            ObservationType := 'delay-thickness';
          end;
        coDelayTheta:
          begin
            OutputExtension := '.delay-theta_ob' + OutputTypeExtension;
            ObservationType := 'delay-theta';
          end;
        coPreConsStressCell:
          begin
            OutputExtension := '.preconstress-cell_ob' + OutputTypeExtension;
            ObservationType := 'preconstress-cell';
          end;
        else
          Assert(False);
      end;

      WriteString('BEGIN CONTINUOUS FILEOUT ');
      OutputFileName := ChangeFileExt(FNameOfFile, OutputExtension);
      Model.AddModelOutputFile(OutputFileName);
      OutputFileName := ExtractFileName(OutputFileName);
      WriteString(OutputFileName);
      if ObsPackage.OutputFormat = ofBinary then
      begin
        WriteString(' BINARY');
      end;
      NewLine;

      for ObsIndex := 0 to FObsList.Count - 1 do
      begin
        AnObs := FObsList[ObsIndex];
        ScreenObject := AnObs.FScreenObject as TScreenObject;
        if AnObsType in AnObs.FObsTypes then
        begin
          Root := AnObs.FName;
          if Root = '' then
          begin
            Root := Format('CSubObs%d', [ObsIndex+1]);
          end;
          Assert(Length(Root) <= 40);
          boundname := Trim(AnObs.FBoundName);
          boundname := Copy(boundname, 1, 40);
          boundname := ' ' + boundname + ' ';

          obsname := '  ''' + Root + ''' ';

          case AnObsType of
            coCSub, coInelastCSub, coElastCSub, coSk, coSke, coIntbedComp,
            coInelastComp, coElastComp, coThickness:
              begin
                if (ScreenObject.ModflowCSub <> nil)
                  and ScreenObject.ModflowCSub.CSubPackageData.Used then
                begin
                  WriteString(obsname);
                  WriteString(ObservationType);
                  WriteString(boundname);
                  NewLine;
                end
                else
                begin
                  for DelayBedIndex := 0 to Length(AnObs.FDelayCellNumbers) - 1 do
                  begin
                    idcellno := AnObs.FDelayCellNumbers[DelayBedIndex];
                    for IBIndex := 0 to Length(AnObs.FInterbedNumbers) - 1 do
                    begin
                      icsubno := AnObs.FInterbedNumbers[IBIndex];
                      obsname := Format(' ''%0:s_%1:d_%2:d'' ', [Root, icsubno, idcellno]);
                      WriteString(obsname);
                      WriteString(ObservationType);
                      WriteInteger(icsubno);
                      NewLine;
                    end;
                  end;
                end;
              end;
            coCoarseCSub, coCSubCell, coWcompCSubCell, coSkCell, coSkeCell,
            coEStressCell, coGStressCell, coCoarseCompaction, coCompCell,
            coCoarseThickness, coThickCell, coCoarseTheta, coThetaCell,
            coPreConsStressCell:
              begin
                for CellIndex := 0 to Length(AnObs.FCells) - 1 do
                begin
                  ACell := AnObs.FCells[CellIndex];
                  obsname := ' ''' + Root + WriteCellName(ACell) + ''' ';
                  WriteString(obsname);
                  WriteString(ObservationType);
                  WriteCell(ACell);
                  NewLine;
                end;
              end;
            coTheta:
              begin
                for IBIndex := 0 to Length(AnObs.FInterbedNumbers) - 1 do
                begin
                  icsubno := AnObs.FInterbedNumbers[IBIndex];
                  obsname := ' ''' + Root + '_' + IntToStr(icsubno) + ''' ';
                  WriteString(obsname);
                  WriteString(ObservationType);
                  WriteInteger(icsubno);
                  NewLine;
                end;
              end;
            coDelayFlowTop, coDelayFlowBot:
              begin
                for IBIndex := 0 to Length(AnObs.FInterbedNumbers) - 1 do
                begin
                  if AnObs.FDelayInterbeds[IBIndex] then
                  begin
                    icsubno := AnObs.FInterbedNumbers[IBIndex];
                    obsname := ' ''' + Root + '_' + IntToStr(icsubno) + ''' ';
                    WriteString(obsname);
                    WriteString(ObservationType);
                    WriteInteger(icsubno);
                    NewLine;
                  end;
                end;
              end;
            coDelayHead, coDelayGStress, coDelayEStress, coDelayPreConStress,
            coDelayComp, coDelayThickness, coDelayTheta:
              begin
                for IBIndex := 0 to Length(AnObs.FInterbedNumbers) - 1 do
                begin
                  if AnObs.FDelayInterbeds[IBIndex] then
                  begin
                    icsubno := AnObs.FInterbedNumbers[IBIndex];
                    for DelayBedIndex := 0 to Length(AnObs.FDelayCellNumbers) - 1 do
                    begin
                      idcellno := AnObs.FDelayCellNumbers[DelayBedIndex];
                      if (1 <= idcellno) and (idcellno <> NDELAYCELLS) then
                      begin
                        obsname := Format(' ''%0:s_%1:d_%2:d'' ', [Root, icsubno, idcellno]);
                        WriteString(obsname);
                        WriteString(ObservationType);
                        WriteInteger(icsubno);
                        WriteInteger(idcellno);
                        NewLine;
                      end;
                    end;
                  end;

                end;
              end;
            else
              Assert(False);
          end;

        end;
      end;

      WriteString('END CONTINUOUS');
      NewLine;
      NewLine;
    end;
  finally
    ObsNames.Free;
  end;
end;

end.
