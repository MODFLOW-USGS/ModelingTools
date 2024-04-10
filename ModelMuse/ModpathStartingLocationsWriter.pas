unit ModpathStartingLocationsWriter;

interface

uses Classes, SysUtils, Contnrs , PhastModelUnit, ScreenObjectUnit,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, AbstractGridUnit,
  ModpathParticleUnit;

type
  TParticleLines = class(TObject)
  private
    FSimulatedLocations: TStringList;
    FNonSimulatedLocations: TStringList;
    FReleaseTimes: TStringList;
    FTrackingDirection: TTrackingDirection;
    function GetNonSimulatedLocation(Index: integer): string;
    function GetSimulatedLocation(Index: integer): string;
  public
    Constructor Create(ScreenObject: TScreenObject;
      TrackingDirection: TTrackingDirection; StartTime, EndTime: Real);
    Destructor Destroy; override;
    procedure UpdateLocationLines(Lines: TStringList;
      Layer, Row, Column: integer; SimulatedLayer: boolean);
    property SimulatedLocations[Index: integer]: string read GetSimulatedLocation;
    property NonSimulatedLocations[Index: integer]: string read GetNonSimulatedLocation;
  end;

  // MODPATH versions 5 and 6
  TModpathStartingLocationsWriter = class(TCustomModflowWriter)
  private
    FParticleLines: TList;
    FCellList: TCellAssignmentList;
    FParticleGrid: array of array of array of TParticleLines;
    FStartingLocations: TStringList;
    FStartTime: Real;
    FEndTime: Real;
    FOptions: TModpathSelection;
    FReferenceTime: Real;
    FUsedObjects: TList;
    FModflowGrid: TCustomModelGrid;
    FReleaseTimes: TModpathTimes;
    FReleaseOption: Integer;
    FLocationCount: Integer;
    FParticles: TParticles;
    procedure AssignParticleLocationsToElements;
    procedure UpdateParticleLines;
    procedure WriteLines;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure SetReferenceTime;
    procedure GetUsedScreenObjects;
    procedure WriteDataSet6;
    procedure WriteDataSet7(ScreenObject: TScreenObject);
    procedure WriteDataSet8(ScreenObject: TScreenObject);
    procedure WriteDataSet10;
    procedure WriteDataSet11;
    procedure WriteDataSet12(ScreenObject: TScreenObject);
  public
    function PackageID_Comment(APackage: TModflowPackageSelection): string; override;
    class function Extension: string; override;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFileVersion5(const AFileName: string);
    procedure WriteFileVersion6(const AFileName: string);
  end;

implementation

uses
  ModflowTimeUnit, frmErrorsAndWarningsUnit,
  frmGoPhastUnit, Math, FastGEO,
  GoPhastTypes, CellLocationUnit;

resourcestring
  StrAStartingTimeFor = 'Starting times for the MODPATH particles defined '
    + 'with the following objects are not valid. Adjust the beginning and '
    + 'ending time for MODPATH or adjust the release time.';
  StrInvalidMODPATHRefe = 'Invalid MODPATH reference time';
  StrTheReferenceTimeF = 'The reference time for a MODPATH simulation must b' +
  'e %s of the simulation specified in the MODFLOW Time dialog box. The refe' +
  'rence time is specified in the MODFLOW Packages and Programs dialog box.';
  StrGreaterOrEqualTo = 'greater or equal to than the initial time';
  StrLessThanOrEqualT = 'less than or equal to the final time';
  StrTheFollowingObject = 'The following objects were not used for defining ' +
  'MODPATH particles because the number of elevation formulas is not equal t' +
  'o 1 or because it was not a point object.';
  StrInTheFollowingObj = 'In the following objects, no MODPATH release times' +
  ' have been specified';
//  StrNoMODPATHStarting = 'No MODPATH starting locations defined';
//  StrNoObjectsDefineSt = 'No objects define starting locations for MODPATH';

{ TModpathStartingLocationsWriter }

constructor TModpathStartingLocationsWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfModflow;
  FParticleLines := TObjectList.Create;
  FCellList:= TCellAssignmentList.Create;
  FStartingLocations := TStringList.Create;
end;

destructor TModpathStartingLocationsWriter.Destroy;
begin
  FStartingLocations.Free;
  FCellList.Free;
  FParticleLines.Free;
  inherited;
end;

class function TModpathStartingLocationsWriter.Extension: string;
begin
  result := '.strt';
end;

function TModpathStartingLocationsWriter.PackageID_Comment(
  APackage: TModflowPackageSelection): string;
begin
  result := File_Comment(APackage.PackageIdentifier + ' Starting Locations file');
end;

procedure TModpathStartingLocationsWriter.WriteFileVersion5(const AFileName: string);
var
  NameOfFile: string;
  StressPeriods: TModflowStressPeriods;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheFollowingObj);
  frmErrorsAndWarnings.BeginUpdate;
  try
    StressPeriods := Model.ModflowStressPeriods;
    if StressPeriods.CompletelyTransient then
    begin
      FStartTime := Model.ModflowPackages.ModPath.BeginningTime;
    end
    else
    begin
      FStartTime := StressPeriods[0].StartTime;
    end;
    if StressPeriods.TransientModel then
    begin
      FEndTime := Model.ModflowPackages.ModPath.EndingTime;
    end
    else
    begin
      FEndTime := StressPeriods[StressPeriods.Count-1].EndTime;
    end;
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrAStartingTimeFor);

    NameOfFile := FileName(AFileName);
    Model.AddModpathInputFile(NameOfFile);
    FInputFileName := NameOfFile;
    OpenFile(NameOfFile);
    try
      AssignParticleLocationsToElements;
      UpdateParticleLines;
      WriteLines;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet1;
const
  InputStyle = 2;
begin
  // Data set 1
  WriteInteger(InputStyle);
  WriteString(' # Data Set 1: InputStyle');
  NewLine;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet12(
  ScreenObject: TScreenObject);
var
  CellIndex: Integer;
  Cell: TCellAssignment;
  Layer: Integer;
  ParticleIndex: Integer;
  LocalXYZ: string;
  ParticleLabel: string;
  APoint: TPoint2D;
  LocalX: Double;
  LocalY: Double;
  GlobalZ: Double;
  LocalZ: Double;
  Digits: Integer;
  MaxLabelBaseLength: Integer;
  ParticleLabelBase: string;
  FormatString: string;
  ParticleCount: Integer;
  ParticleLines: TParticleLines;
const
  Grid = 1;
begin

  // Data Set 12
  ParticleLabelBase := ScreenObject.Name;
  if FLocationCount <= 0 then
  begin
    Digits := 0;
  end
  else
  begin
    Digits := Trunc(Log10(FLocationCount))+1;
  end;
  MaxLabelBaseLength := 39 - Digits;
  FormatString := '%.' + IntToStr(Digits) + 'd';
  if Length(ParticleLabelBase) > MaxLabelBaseLength then
  begin
    SetLength(ParticleLabelBase, MaxLabelBaseLength);
  end;
  ParticleLabelBase := ParticleLabelBase + '-';
  ParticleCount := 0;
  ParticleLines := TParticleLines.Create(ScreenObject,
    FOptions.TrackingDirection,
    FStartTime, FEndTime);
  try
//          WriteDataSet12(ScreenObject, Particles, ParticleLabelBase,
//            FormatString, ParticleCount, ParticleLines);


    for CellIndex := 0 to FCellList.Count - 1 do
    begin
      Cell := FCellList[CellIndex];
      Layer := Model.DataSetLayerToModflowLayer(Cell.Layer);
      if not Model.IsLayerSimulated(Cell.Layer) then
      begin
        Dec(Layer);
      end;
      if FParticles <> nil then
      begin
        for ParticleIndex := 0 to FParticles.Count - 1 do
        begin
          WriteInteger(Grid);
          WriteInteger(Layer);
          WriteInteger(Cell.Row + 1);
          WriteInteger(Cell.Column + 1);
          if Model.IsLayerSimulated(Cell.Layer) then
          begin
            LocalXYZ := ParticleLines.SimulatedLocations[ParticleIndex];
          end
          else
          begin
            LocalXYZ := ParticleLines.NonSimulatedLocations[ParticleIndex];
          end;
          WriteString(' ' + LocalXYZ);
          Inc(ParticleCount);
          ParticleLabel := ParticleLabelBase + Format(FormatString, [ParticleCount]);
          WriteString(ParticleLabel);
          WriteString(' # Data Set 12: Grid Layer Row Column LocalX LocalY LocalZ Label');
          NewLine;
        end;
      end
      else
      begin
        WriteInteger(Grid);
        WriteInteger(Layer);
        WriteInteger(Cell.Row + 1);
        WriteInteger(Cell.Column + 1);
        APoint := ScreenObject.Points[Cell.Section];
        APoint := FModflowGrid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
        LocalX := (APoint.x - FModflowGrid.ColumnPosition[Cell.Column]) / FModflowGrid.ColumnWidth[Cell.Column];
        WriteFloat(LocalX);
        LocalY := 1 - ((FModflowGrid.RowPosition[Cell.Row] - APoint.y) / FModflowGrid.RowWidth[Cell.Row]);
        WriteFloat(LocalY);
        GlobalZ := ScreenObject.Higher3DElevations[Model][Cell.Layer, Cell.Row, Cell.Column];
        LocalZ := (GlobalZ - FModflowGrid.CellElevation[Cell.ZeroBasedID.LayerPlus1])
           / FModflowGrid.CellThickness[Cell.ZeroBasedID];
        if not Model.IsLayerSimulated(Cell.Layer) then
        begin
          LocalZ := LocalZ - 1;
        end;
        WriteFloat(LocalZ);
        Inc(ParticleCount);
        ParticleLabel := ParticleLabelBase + Format(FormatString, [ParticleCount]);
        WriteString(ParticleLabel);
        WriteString(' # Data Set 12: Grid Layer Row Column LocalX LocalY LocalZ Label');
        NewLine;
      end;
    end;
  finally
    ParticleLines.Free;
  end;
end;

procedure TModpathStartingLocationsWriter.WriteFileVersion6(
  const AFileName: string);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  NameOfFile: string;
  StressPeriods: TModflowStressPeriods;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheFollowingObj);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMODPATHRefe);
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoMODPATHStarting);
  StressPeriods := Model.ModflowStressPeriods;
  FStartTime := StressPeriods[0].StartTime;
  FEndTime := StressPeriods[StressPeriods.Count-1].EndTime;
  NameOfFile := FileName(AFileName);
  Model.AddModpathInputFile(NameOfFile);
    FINPUTFILENAME := NAMEOFFILE;
  OpenFile(NameOfFile);
  try
    FModflowGrid := Model.Grid;
    FOptions := Model.ModflowPackages.ModPath;
    WriteDataSet0;
    WriteDataSet1;
    SetReferenceTime;

    FUsedObjects := TList.Create;
    try
      GetUsedScreenObjects;
      WriteDataSet6;

      for Index := 0 to FUsedObjects.Count - 1 do
      begin
        ScreenObject := FUsedObjects[Index];
        WriteDataSet7(ScreenObject);

        FParticles := ScreenObject.ModpathParticles.Particles;

        FCellList.Clear;
        ScreenObject.GetModpathCellList(FCellList, Model);
        if FParticles <> nil then
        begin
          FLocationCount := FCellList.Count * FParticles.Count;
        end
        else
        begin
          FLocationCount := FCellList.Count;
          if (ScreenObject.ElevationCount <> TElevationCount.ecOne)
            or (ScreenObject.Count <> ScreenObject.SectionCount) then
          begin
            frmErrorsAndWarnings.AddWarning(Model,
              StrTheFollowingObject, ScreenObject.Name, ScreenObject);
            Continue;
          end;
        end;
        WriteDataSet8(ScreenObject);


        // Data Set 10
        if FReleaseOption = 3 then
        begin
          WriteDataSet10;
          WriteDataSet11;
        end;

        WriteDataSet12(ScreenObject);
      end;
    finally
      FUsedObjects.Free;
    end;
  finally
    CloseFile;
  end;

end;

procedure TModpathStartingLocationsWriter.WriteDataSet11;
var
  TimeIndex: Integer;
  ATime: Double;
begin
  // Data Set 11
  for TimeIndex := 1 to FReleaseTimes.Count - 1 do
  begin
    ATime := FReleaseTimes[TimeIndex].Time;
    // - FOptions.ReferenceTime;
    WriteFloat(ATime);
    if ((TimeIndex mod 10) = 0) and (TimeIndex <> FReleaseTimes.Count - 1) then
    begin
      NewLine;
    end;
  end;
  if FReleaseTimes.Count > 0 then
  begin
    WriteString('Data Set 11: MultipleReleaseTimes');
    NewLine;
  end;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet10;
var
  ReleaseEventCount: Integer;
begin
  ReleaseEventCount := FReleaseTimes.Count - 1;
  WriteInteger(ReleaseEventCount);
  WriteString(' # Data Set 10: ReleaseEventCount');
  NewLine;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet8(ScreenObject: TScreenObject);
var
  ReleaseStartTime: Double;
begin
  // data set 8
  WriteInteger(FLocationCount);
  FReleaseTimes := ScreenObject.ModpathParticles.ReleaseTimes;
  if FReleaseTimes.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInTheFollowingObj,
      ScreenObject.Name, ScreenObject);
    Exit;
  end;
  ReleaseStartTime := FReleaseTimes[0].Time;
  WriteFloat(ReleaseStartTime);
  if FReleaseTimes.Count = 1 then
  begin
    FReleaseOption := 1;
  end
  else
  begin
    FReleaseOption := 3;
  end;
  WriteInteger(FReleaseOption);
  WriteString(' # Data Set 8: LocationCount ReleaseStartTime ReleaseOption');
  NewLine;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet7(ScreenObject: TScreenObject);
var
  GroupName: string;
begin
  // data set 7
  GroupName := ScreenObject.Name;
  if Length(GroupName) > 16 then
  begin
    SetLength(GroupName, 16);
  end;
  WriteString(GroupName);
  WriteString(' # Data Set 7: GroupName');
  NewLine;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet6;
var
  GroupCount: Integer;
begin
  // Data set 6
  GroupCount := FUsedObjects.Count;
  WriteInteger(GroupCount);
  WriteString(' # Data Set 6: GroupCount');
  NewLine;
end;

procedure TModpathStartingLocationsWriter.GetUsedScreenObjects;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[Index];
    if (not ScreenObject.Deleted) and ScreenObject.ModpathParticles.Used then
    begin
      FUsedObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TModpathStartingLocationsWriter.SetReferenceTime;
begin
  if FOptions.ReferenceTime < Model.ModflowStressPeriods.First.StartTime then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHRefe, Format(StrTheReferenceTimeF, [StrGreaterOrEqualTo]));
  end;
  if FOptions.ReferenceTime > Model.ModflowStressPeriods.Last.EndTime then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidMODPATHRefe, Format(StrTheReferenceTimeF, [StrLessThanOrEqualT]));
  end;
  FReferenceTime := FOptions.ReferenceTime - Model.ModflowStressPeriods[0].StartTime;
end;

procedure TModpathStartingLocationsWriter.WriteDataSet0;
begin
  // Data set 0
  WriteCommentLine(PackageID_Comment(FOptions));
  WriteCommentLines(FOptions.Comments);
end;

procedure TModpathStartingLocationsWriter.WriteLines;
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to FStartingLocations.Count - 1 do
  begin
    WriteString(FStartingLocations[LineIndex]);
    NewLine;
  end;
end;

procedure TModpathStartingLocationsWriter.UpdateParticleLines;
var
  LayerIndex: Integer;
  SimulatedLayer: Boolean;
  RowIndex: Integer;
  ColumnIndex: Integer;
  ParticleLines: TParticleLines;
begin
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    SimulatedLayer := Model.IsLayerSimulated(LayerIndex);
    for RowIndex := 0 to Model.Grid.RowCount - 1 do
    begin
      for ColumnIndex := 0 to Model.Grid.ColumnCount - 1 do
      begin
        ParticleLines := FParticleGrid[LayerIndex, RowIndex, ColumnIndex];
        if ParticleLines <> nil then
        begin
          ParticleLines.UpdateLocationLines(FStartingLocations,
            LayerIndex + 1, RowIndex + 1, ColumnIndex + 1, SimulatedLayer);
        end;
      end;
    end;
  end;
end;

procedure TModpathStartingLocationsWriter.AssignParticleLocationsToElements;
var
  ScreenObject: TScreenObject;
  Index: Integer;
  Cell: TCellAssignment;
  ObjectIndex: Integer;
  ParticleLines: TParticleLines;
  LocalModel: TCustomModel;
begin
  LocalModel := Model;
  SetLength(FParticleGrid, LocalModel.Grid.LayerCount,
    LocalModel.Grid.RowCount, LocalModel.Grid.ColumnCount);
  for Index := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[Index];
    if (not ScreenObject.Deleted) and ScreenObject.ModpathParticles.Used then
    begin
      ParticleLines := TParticleLines.Create(ScreenObject,
        LocalModel.ModflowPackages.ModPath.TrackingDirection,
        FStartTime, FEndTime);
      FParticleLines.Add(ParticleLines);
      FCellList.Clear;
      ScreenObject.GetModpathCellList(FCellList, LocalModel);
      for ObjectIndex := 0 to FCellList.Count - 1 do
      begin
        Cell := FCellList[ObjectIndex];
        FParticleGrid[Cell.Layer, Cell.Row, Cell.Column] := ParticleLines;
      end;
    end;
  end;
end;

{ TParticleLines }

constructor TParticleLines.Create(ScreenObject: TScreenObject;
TrackingDirection: TTrackingDirection; StartTime, EndTime: Real);
var
  TimeIndex: Integer;
  ParticleReleaseTimes: TModpathTimes;
  TimeItem: TModpathTimeItem;
  Particles: TParticles;
  Index: Integer;
  ParticleItem: TParticleLocation;
  XYString: string;
  ReleaseTimeErrorDetected: boolean;
begin

  Assert(ScreenObject <> nil);
  Assert(not ScreenObject.Deleted);
  Assert(ScreenObject.ModpathParticles.Used);
  FTrackingDirection := TrackingDirection;
  FSimulatedLocations:= TStringList.Create;
  FNonSimulatedLocations:= TStringList.Create;
  FReleaseTimes:= TStringList.Create;
  ParticleReleaseTimes := ScreenObject.ModpathParticles.ReleaseTimes;
  FReleaseTimes.Capacity := ParticleReleaseTimes.Count;
  ReleaseTimeErrorDetected := False;
  for TimeIndex := 0 to ParticleReleaseTimes.Count - 1 do
  begin
    TimeItem := ParticleReleaseTimes.Items[TimeIndex] as TModpathTimeItem;
    if (not ReleaseTimeErrorDetected) and (FTrackingDirection = tdForward) and
      ((TimeItem.Time < 0) or (TimeItem.Time > EndTime-StartTime)) then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrAStartingTimeFor,
        ScreenObject.Name, ScreenObject);
      ReleaseTimeErrorDetected := True;
    end;
    FReleaseTimes.Add(FortranFloatToStr(TimeItem.Time));
  end;
  Particles := ScreenObject.ModpathParticles.Particles;
  if Particles <> nil then
  begin
    for Index := 0 to Particles.Count - 1 do
    begin
      ParticleItem := Particles.Items[Index] as TParticleLocation;
      XYString := FortranFloatToStr(ParticleItem.X) + ' '
        + FortranFloatToStr(1-ParticleItem.Y) + ' ';
      FSimulatedLocations.Add(XYString + FortranFloatToStr(ParticleItem.Z)
        + ' 0 0 0 ');
      FNonSimulatedLocations.Add(XYString + FortranFloatToStr(1-ParticleItem.Z)
        + ' 0 0 0 ');
    end;
  end;
end;

destructor TParticleLines.Destroy;
begin
  FSimulatedLocations.Free;
  FNonSimulatedLocations.Free;
  FReleaseTimes.Free;
  inherited;
end;

function TParticleLines.GetNonSimulatedLocation(Index: integer): string;
begin
  result := FNonSimulatedLocations[Index];
end;

function TParticleLines.GetSimulatedLocation(Index: integer): string;
begin
  result := FSimulatedLocations[Index];
end;

procedure TParticleLines.UpdateLocationLines(Lines: TStringList; Layer, Row,
  Column: integer; SimulatedLayer: boolean);
var
  CellLine: string;
  TimeIndex: Integer;
  TimeString: string;
  ParticleIndex: Integer;
  TimeCount: integer;
begin
  CellLine := IntToStr(Column) + ' '
    + IntToStr(Row) + ' ' + IntToStr(Layer) + ' ';
  TimeCount := FReleaseTimes.Count;
  if FTrackingDirection = tdBackward then
  begin
    TimeCount := 1;
  end;
  for TimeIndex := 0 to TimeCount - 1 do
  begin
    case FTrackingDirection of
      tdForward:
        begin
          TimeString := FReleaseTimes[TimeIndex];
        end;
      tdBackward:
        begin
          TimeString := '0';
        end;
      else Assert(False);
    end;
    if SimulatedLayer then
    begin
      for ParticleIndex := 0 to FSimulatedLocations.Count - 1 do
      begin
        Lines.Add(CellLine + FSimulatedLocations[ParticleIndex]
          + TimeString + ' # J I K X Y Z JCODE ICODE KCODE TRELEAS');
      end;
    end
    else
    begin
      for ParticleIndex := 0 to FNonSimulatedLocations.Count - 1 do
      begin
        Lines.Add(CellLine + FNonSimulatedLocations[ParticleIndex]
          + TimeString + ' # J I K X Y Z JCODE ICODE KCODE TRELEAS');
      end;
    end;
  end;
end;

end.
