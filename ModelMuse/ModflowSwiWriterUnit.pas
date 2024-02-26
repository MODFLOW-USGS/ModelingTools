unit ModflowSwiWriterUnit;

interface

uses System.UITypes,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Classes,
  PhastModelUnit, GoPhastTypes, ModflowSwiObsUnit, System.Generics.Collections,
  InterpolatedObsCellUnit, DataSetUnit;

type
  TInterpolatedID = class(TObject)
    Name: string;
    Time: double;
    Statistic: Double;
    StatFlag: TStatFlag;
    ObservedValue: double;
    function Weight: double;
  end;

  TInterpolatedIDObjectList = TObjectList<TInterpolatedID>;

  TInterpolatedObs = class(TBaseInterpolatedObs)
//    FCells: TInterpolatedObsCellObjectList;
    IDs: TInterpolatedIDObjectList;
    ZetaSurface: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TInterpolatedObsObjectList = class(TObjectList<TInterpolatedObs>)
    function ObCellCount: integer;
  end;

  TDataToWrite = (dtwObservedValues, dtwInstructions);

  TSwiWriter = class(TCustomPackageWriter)
  private
//    FNameOfFile: string;
    FSwiPackage: TSwiPackage;
    FObservations: TStringList;
    FObservationNames: TStringList;
    FPurpose: TObservationPurpose;
    FInterpolatedObs: TInterpolatedObsObjectList;
    ISWIOBS: integer;
    NOBS: integer;
    NSRF: integer;
    FObsOutputFileName: string;
    FDataToWrite: TDataToWrite;
    FPestDataArrays: TList<TDataArray>;
    procedure EvaluateObservations;
    procedure EvaluateInterpolatedObservations;
    procedure WriteDataSet1;
    procedure WriteDataSet2a;
    procedure WriteDataSet2b;
    procedure WriteDataSet3a;
    procedure WriteDataSet3b;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    // For use with SWI_ObsExtractor
    procedure WriteInterpolatedZetaFile(const AFileName: string);
    procedure WriteInterpolatedSwiFileOptions(const AFileName: string; ArchiveFile: Boolean = False);
    procedure WriteInterpolatedIDs;
    procedure WriteSwiObsExtBatchFile(const AFileName: string; ArchiveFile: Boolean);
    // For use with general purpose observation extractor
    procedure WritePestScript(const AFileName: string);
    procedure WritePestScripts;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(AModel: TCustomModel;
      EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, SysUtils, frmProgressUnit,
  AbstractGridUnit, frmErrorsAndWarningsUnit, ScreenObjectUnit, System.Math,
  BasisFunctionUnit, FastGEO, InterpolatedObsResourceUnit, ArchiveNodeInterface,
  PestParamRoots, PestPropertiesUnit, DataSetNamesUnit;

resourcestring
  StrTheSWIObservations = 'The SWI observations at the following cells have ' +
  'observation names that are longer than maximum allowed number of characte' +
  'rs. The name will be truncated. (Layer, Row, Column)';
  Str0d1d2d = '(%0:d, %1:d, %2:d)';
  Str0s1d2d3d = '%0:s, %1:d %2:d %3:d # Data set 8 OBSNAM LAYER ROW COLUMN';
  StrZETASurface0dLa = 'ZETA Surface %0:d Layer %1:d';
  StrSSZLayerD = 'SSZ Layer %d';
  StrISOURCELayerD = 'ISOURCE Layer %d';
  StrSWINotSupported = 'SWI not supported';
  StrTheSWIPackageIsN = 'The SWI package is not supported in this version of' +
  ' MODFLOW.';

{ TSwiWriter }

constructor TSwiWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited Create(AModel, EvaluationType);
  FPurpose := AModel.ObservationPurpose;
  FObservations := TStringList.Create;
  FObservationNames := TStringList.Create;
  FInterpolatedObs := TInterpolatedObsObjectList.Create;
  FPestDataArrays := TList<TDataArray>.Create;
end;

destructor TSwiWriter.Destroy;
begin
  FPestDataArrays.Free;
  FInterpolatedObs.Free;
  FObservationNames.Free;
  FObservations.Free;
  inherited;
end;

procedure TSwiWriter.EvaluateInterpolatedObservations;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: TSwiObsBoundary;
  CellList: TSwiObsCellList;
  ACell: TSwi_Cell;
  RowOffset: Double;
  ColumnOffset: Double;
  ActiveDataArray: TDataArray;
  CenterCell: TInterpolatedObsCell;
  Cell1: TInterpolatedObsCell;
  Cell3: TInterpolatedObsCell;
  Cell2: TInterpolatedObsCell;
  ColDirection: integer;
  RowDirection: integer;
  Grid: TCustomModelGrid;
  NewRow: Integer;
  NewColumn: Integer;
  InterpObs: TInterpolatedObs;
  Direction: T2DDirection;
  Element: TElement;
  CellIndex: Integer;
  SwiCell: TInterpolatedObsCell;
  ALocation: TPoint2D;
  Fractions: TOneDRealArray;
  IdIndex: Integer;
  AnSwiItem: TSwiObsItem;
  ID_Item: TInterpolatedID;
  ObsIndex: integer;
  ObsName: string;
begin
  ObsIndex := 0;
  FObservationNames.CaseSensitive := False;
  FObservationNames.Sorted := true;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Grid := Model.Grid;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Observations := ScreenObject.ModflowSwiObservations;
    if (Observations <> nil) and Observations.Used then
    begin
      if Observations.Purpose = FPurpose then
      begin
        Observations.EvaluateSwiObservations(FPurpose, Model);
//        FInterpolatedObs.Add(Observations);

        if Observations.CellListCount = 0 then
        begin
//          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
//          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage, ScreenObject);
          Continue;
        end;

        CellList := Observations.CellLists[0];
        if CellList.Count = 0 then
        begin
//          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
//          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage, ScreenObject);
          Continue;
        end;

        if CellList.Count > 1 then
        begin
          // Warn that if an object selects multiple cells, on the first one will be used.

//          ErrorMessage := Format(StrObjectS, [ScreenObject.Name]);
//          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage, ScreenObject);
        end;

        ACell := CellList[0];

        ActiveDataArray.Initialize;
        if not ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, ACell.Column] then
        begin
          // warn about observation in inactive cell

          Continue;
        end;

        RowOffset := Observations.Values.ObservationRowOffset;
        ColumnOffset := Observations.Values.ObservationColumnOffset;

        ColDirection := Sign(ColumnOffset);
        RowDirection := Sign(RowOffset);

        CenterCell := TInterpolatedObsCell.Create;
        CenterCell.Layer := ACell.Layer;
        CenterCell.Row := ACell.Row;
        CenterCell.Col := ACell.Column;

        Cell1 := nil;
        Cell2 := nil;
        Cell3 := nil;
        Direction := dirX;

        NewRow := 0;
        if RowDirection <> 0 then
        begin
          NewRow := ACell.Row + RowDirection;
          if (NewRow >= 0) and (NewRow < Grid.RowCount)
            and ActiveDataArray.BooleanData[ACell.Layer, NewRow, ACell.Column]  then
          begin
            Cell1 := TInterpolatedObsCell.Create;
            Cell1.Layer := ACell.Layer;
            Cell1.Row := NewRow;
            Cell1.Col := ACell.Column;
            Direction := dirY;
          end;
        end;

        NewColumn := 0;
        if ColDirection <> 0 then
        begin
          NewColumn := ACell.Column + ColDirection;
          if (NewColumn >= 0) and (NewColumn < Grid.ColumnCount)
            and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, NewColumn]  then
          begin
            Cell3 := TInterpolatedObsCell.Create;
            Cell3.Layer := ACell.Layer;
            Cell3.Row := ACell.Row;
            Cell3.Col := NewColumn;
            Direction := dirX;
          end;
        end;

        if (RowDirection <> 0) and (ColDirection <> 0)
          and (Cell1 <> nil) and (Cell3 <> nil)
          and ActiveDataArray.BooleanData[ACell.Layer, NewRow, NewColumn]  then
        begin
          Cell2 := TInterpolatedObsCell.Create;
          Cell2.Layer := ACell.Layer;
          Cell2.Row := NewRow;
          Cell2.Col := NewColumn;
        end;

        InterpObs := TInterpolatedObs.Create;
        FInterpolatedObs.Add(InterpObs);

        InterpObs.ZetaSurface := Observations.ZetaSurfaceNumber;

        if (RowDirection = 0) or (ColDirection = 0) then
        begin
          InterpObs.FCells.Add(CenterCell);
          if (Cell1 <> nil) then
          begin
            InterpObs.FCells.Add(Cell1);
          end;
          if (Cell3 <> nil) then
          begin
            InterpObs.FCells.Add(Cell3);
          end;
        end
        else
        begin
          if RowDirection = ColDirection then
          begin
            if (Cell3 <> nil) then
            begin
              InterpObs.FCells.Add(Cell3);
            end;
            InterpObs.FCells.Add(CenterCell);
            if (Cell1 <> nil) then
            begin
              InterpObs.FCells.Add(Cell1);
            end;
            if (Cell2 <> nil) then
            begin
              InterpObs.FCells.Add(Cell2);
            end;
          end
          else
          begin
            InterpObs.FCells.Add(CenterCell);
            if (Cell3 <> nil) then
            begin
              InterpObs.FCells.Add(Cell3);
            end;
            if (Cell2 <> nil) then
            begin
              InterpObs.FCells.Add(Cell2);
            end;
            if (Cell1 <> nil) then
            begin
              InterpObs.FCells.Add(Cell1);
            end;
          end;
        end;

        if InterpObs.FCells.Count = 1 then
        begin
          InterpObs.FCells[0].Fraction := 1;
        end
        else
        begin
          SetLength(Element, InterpObs.FCells.Count);
          for CellIndex := 0 to InterpObs.FCells.Count - 1 do
          begin
            SwiCell := InterpObs.FCells[CellIndex];
            Element[CellIndex] := Grid.UnrotatedTwoDElementCenter
              (SwiCell.Col, SwiCell.Row);
          end;
          ALocation := Grid.
            RotateFromRealWorldCoordinatesToGridCoordinates(ScreenObject.Points[0]);
          GetBasisFunctionFractions(Element, ALocation, Fractions, Direction);
          for CellIndex := 0 to InterpObs.FCells.Count - 1 do
          begin
            SwiCell := InterpObs.FCells[CellIndex];
            SwiCell.Fraction := Fractions[CellIndex];
          end;
        end;

        for CellIndex := 0 to InterpObs.FCells.Count - 1 do
        begin
          SwiCell := InterpObs.FCells[CellIndex];
          repeat
            Inc(ObsIndex);
            ObsName := Format('Obs_%d', [ObsIndex]);
          until (FObservationNames.IndexOf(ObsName) < 0);
          SwiCell.Name := ObsName;
        end;

        for IdIndex := 0 to Observations.Values.Count - 1 do
        begin
          AnSwiItem := Observations.Values.SwiItems[IdIndex];
          ID_Item := TInterpolatedID.Create;
          InterpObs.IDs.Add(ID_Item);
          ID_Item.Name := AnSwiItem.Name;
          ID_Item.Time := AnSwiItem.Time;
          ID_Item.Statistic := AnSwiItem.Statistic;
          ID_Item.StatFlag := AnSwiItem.StatFlag;
          ID_Item.ObservedValue := AnSwiItem.ObservedValue;
        end;

      end;
    end;
  end;
end;

procedure TSwiWriter.EvaluateObservations;
const
  MaxObsNameLength = 12;
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  Grid: TCustomModelGrid;
  Layer: integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  ObsName: string;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(KSWI_Observation_Name);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  Grid := Model.Grid;
  for LayerIndex := 0 to Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        for ColumnIndex := 0 to Grid.ColumnCount - 1 do
        begin
          ObsName := Trim(DataArray.StringData[LayerIndex, RowIndex, ColumnIndex]);
          if ObsName <> '' then
          begin
            if Length(ObsName) > MaxObsNameLength then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrTheSWIObservations,
                Format(Str0d1d2d, [LayerIndex+1, RowIndex+1,
                ColumnIndex+1]));
              ObsName := Copy(ObsName, 1, MaxObsNameLength);
            end;
            ObsName := StringReplace(ObsName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);

            FObservations.Add(Format(Str0s1d2d3d,
              [ObsName, Layer, RowIndex+1, ColumnIndex+1]));

            FObservationNames.Add(ObsName);
          end;
        end;
      end;
    end;
  end;
end;

class function TSwiWriter.Extension: string;
begin
  result := '.swi';
end;

function TSwiWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SwiPackage;
end;

procedure TSwiWriter.WriteDataSet1;
var
  ISTRAT, ISWIZT, ISWIBD: integer;
  OPTIONS: string;
  ZetaFileName: string;
  BaseName: string;
begin
  WriteString('# Data Set 1: NSRF, ISTRAT, NOBS, ISWIZT, ISWIBD, ISWIOBS');
  if FSwiPackage.Adaptive then
  begin
    WriteString(' [Options]');
  end;
  NewLine;
  NSRF := FSwiPackage.NumberOfSurfaces;
  ISTRAT := Ord(FSwiPackage.DensityChoice);
  ISWIOBS := 0;
  case FSwiPackage.ObsChoice of
    socNone: ISWIOBS := 0;
    socAscii: ISWIOBS := Model.UnitNumbers.UnitNumber(StrSWI_Obs);
    socBinary: ISWIOBS := -Model.UnitNumbers.UnitNumber(StrSWI_Obs);
    else Assert(False);
  end;
  if FSwiPackage.SaveZeta then
  begin
    ISWIZT := Model.UnitNumbers.UnitNumber(StrSWI_Zeta);
  end
  else
  begin
    ISWIZT := 0
  end;
  GetFlowUnitNumber(ISWIBD);
  if ISWIOBS <> 0 then
  begin
    EvaluateObservations;
    EvaluateInterpolatedObservations;
    NOBS := FObservations.Count + FInterpolatedObs.ObCellCount;
    if NOBS = 0 then
    begin
      ISWIOBS := 0;
    end;
  end
  else
  begin
    NOBS := 0
  end;

  BaseName := ChangeFileExt(FNameOfFile, '');
  if ISWIOBS <> 0 then
  begin
    FObsOutputFileName := ChangeFileExt(BaseName, '.swi_obs');
    if not WritingTemplate then
    begin
      if ISWIOBS > 0 then
      begin
        WriteToNameFile(StrData, ISWIOBS, FObsOutputFileName, foOutput, Model);
      end
      else
      begin
        WriteToNameFile(StrDataBinary, -ISWIOBS, FObsOutputFileName, foOutput, Model);
      end;
    end;
  end;
  if ISWIZT <> 0 then
  begin
    ZetaFileName := ChangeFileExt(BaseName, strZeta);
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDataBinary, ISWIZT, ZetaFileName, foOutput, Model);
    end;
  end;
  if FSwiPackage.Adaptive then
  begin
    OPTIONS := ' ADAPTIVE';
  end
  else
  begin
    OPTIONS := '';
  end;
  WriteInteger(NSRF);
  WriteInteger(ISTRAT);
  WriteInteger(NOBS);
  WriteInteger(ISWIZT);
  WriteInteger(ISWIBD);
  WriteInteger(ISWIOBS);
  WriteString(OPTIONS);
  NewLine;
end;

procedure TSwiWriter.WriteDataSet2a;
var
  NSOLVER, IPRSOL, MUTSOL: integer;
begin
  NSOLVER := Ord(FSwiPackage.Solver) + 1;
  IPRSOL := FSwiPackage.SolverPrintoutInterval;
  MUTSOL := Ord(FSwiPackage.SolverPrintChoice);
  WriteInteger(NSOLVER);
  WriteInteger(IPRSOL);
  WriteInteger(MUTSOL);
  WriteString(' # Data Set 2a: NSOLVER, IPRSOL, MUTSOL');
  NewLine;
end;

procedure TSwiWriter.WriteDataSet2b;
var
  MXITER, ITER1, NPCOND: Integer;
  ZCLOSE, RCLOSE, RELAX: double;
  NBPOL: Integer;
  DAMP, DAMPT: double;
begin
  if FSwiPackage.Solver = ssPCG then
  begin
    MXITER := FSwiPackage.MXITER;
    ITER1 := FSwiPackage.ITER1;
    NPCOND := Ord(FSwiPackage.NPCOND)+1;
    ZCLOSE := FSwiPackage.ZCLOSE.Value;
    RCLOSE := FSwiPackage.RCLOSE.Value;
    RELAX := FSwiPackage.RELAX.Value;
    NBPOL := Ord(FSwiPackage.NBPOL)+1;
    DAMP := -FSwiPackage.DAMP.Value;
    DAMPT := FSwiPackage.DAMPT.Value;

    WriteInteger(MXITER);
    WriteInteger(ITER1);
    WriteInteger(NPCOND);
    WriteFloat(ZCLOSE);
    WriteFloat(RCLOSE);
    WriteFloat(RELAX);
    WriteInteger(NBPOL);
    WriteFloat(DAMP);
    WriteFloat(DAMPT);

    WriteString(' # Data Set 2b: MXITER ITER1 NPCOND ZCLOSE RCLOSE RELAX NBPOL DAMP DAMPT');
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet3a;
var
  TOESLOPE, TIPSLOPE, ALPHA, BETA: double;
begin
  TOESLOPE := FSwiPackage.ToeSlope.Value;
  TIPSLOPE := FSwiPackage.TipSlope.Value;
  ALPHA := FSwiPackage.Alpha.Value;
  BETA := FSwiPackage.Beta.Value;

  WriteFloat(TOESLOPE);
  WriteFloat(TIPSLOPE);
  WriteFloat(ALPHA);
  WriteFloat(BETA);

  WriteString(' # Data Set 3a: TOESLOPE TIPSLOPE [ALPHA] [BETA]');
  NewLine;
end;

procedure TSwiWriter.WriteDataSet3b;
var
  NADPTMX, NADPTMN: Integer;
  ADPTFCT: double;
begin
  if FSwiPackage.Adaptive then
  begin
    NADPTMX := FSwiPackage.MaxAdaptiveTimeSteps;
    NADPTMN := FSwiPackage.MinAdaptiveTimeSteps;
    ADPTFCT := FSwiPackage.AdaptiveFactor.Value;
    WriteInteger(NADPTMX);
    WriteInteger(NADPTMN);
    WriteFloat(ADPTFCT);
    WriteString(' # Data Set 3b: NADPTMX NADPTMN ADPTFCT');
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet4;
var
  Index: Integer;
begin
  WriteU2DRELHeader(' # Data Set 4: NU', matStructured, 'NU');
  case FSwiPackage.DensityChoice of
    dcLinear: Assert(FSwiPackage.ZoneDimensionlessDensities.Count =
      FSwiPackage.NumberOfSurfaces + 2);
    dcZoned: Assert(FSwiPackage.ZoneDimensionlessDensities.Count =
      FSwiPackage.NumberOfSurfaces + 1);
    else Assert(False);
  end;
  for Index := 0 to FSwiPackage.ZoneDimensionlessDensities.Count - 1 do
  begin
    WriteFloat(FSwiPackage.ZoneDimensionlessDensities[Index].Value);
    if (Index+1) mod 10 = 0 then
    begin
      NewLine;
    end;
  end;
  if FSwiPackage.ZoneDimensionlessDensities.Count mod 10 <> 0 then
  begin
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet5;
var
  SurfaceIndex: Integer;
  DataArrayName: string;
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  for SurfaceIndex := 1 to FSwiPackage.NumberOfSurfaces do
  begin
    DataArrayName := KActive_Surface_Elevation + IntToStr(SurfaceIndex);
    ADataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
    Assert(ADataArray <> nil);
    ADataArray.Initialize;
    for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
        WriteArray(ADataArray, LayerIndex,
          Format(StrZETASurface0dLa, [SurfaceIndex, Layer]), StrNoValueAssigned,
          'ZETA', LayerIndex = Model.Grid.LayerCount - 1);
      end;
    end;
    if ADataArray.PestParametersUsed and (FPestDataArrays.IndexOf(ADataArray) < 0) then
    begin
      FPestDataArrays.Add(ADataArray);
    end;
    if ADataArray.UseValuesForObservations and ADataArray.PestParametersUsed then
    begin
      Model.AddInputObsDataSet(ADataArray);
    end;
  end;
end;

procedure TSwiWriter.WriteDataSet6;
var
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  ADataArray := Model.DataArrayManager.GetDataSetByName(KEffectivePorosity);
  Assert(ADataArray <> nil);
  ADataArray.Initialize;
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      WriteArray(ADataArray, LayerIndex,
        Format(StrSSZLayerD, [Layer]), StrNoValueAssigned, 'SSZ',
        LayerIndex = Model.Grid.LayerCount - 1);
    end;
  end;
  if ADataArray.PestParametersUsed and (FPestDataArrays.IndexOf(ADataArray) < 0) then
  begin
    FPestDataArrays.Add(ADataArray);
  end;
  if ADataArray.UseValuesForObservations and ADataArray.PestParametersUsed then
  begin
    Model.AddInputObsDataSet(ADataArray);
  end;
end;

procedure TSwiWriter.WriteDataSet7;
var
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  ADataArray := Model.DataArrayManager.GetDataSetByName(KSourceFluidDensityZone);
  Assert(ADataArray <> nil);
  ADataArray.Initialize;
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      WriteArray(ADataArray, LayerIndex,
        Format(StrISOURCELayerD, [Layer]), StrNoValueAssigned, 'ISOURCE',
        LayerIndex = Model.Grid.LayerCount - 1);
    end;
  end;
end;

procedure TSwiWriter.WriteDataSet8;
var
  LineIndex: Integer;
  InterpObsIndex: Integer;
  SwiObs: TInterpolatedObs;
  CellIndex: Integer;
  SwiCell: TInterpolatedObsCell;
  ObsName: string;
  ObsNumber: integer;
begin
  ObsNumber := 0;
  for InterpObsIndex := 0 to FInterpolatedObs.Count - 1 do
  begin
    SwiObs := FInterpolatedObs[InterpObsIndex];
    for CellIndex := 0 to SwiObs.FCells.Count - 1 do
    begin
      SwiCell := SwiObs.FCells[CellIndex];
      ObsName := SwiCell.Name;
      WriteString(ObsName);
      WriteInteger(SwiCell.Layer+1);
      WriteInteger(SwiCell.Row+1);
      WriteInteger(SwiCell.Col+1);
      NewLine;
      Inc(ObsNumber);
      SwiCell.ObsNumber := ObsNumber;
    end;
  end;
  for LineIndex := 0 to FObservations.Count - 1 do
  begin
    WriteString(FObservations[LineIndex]);
    NewLine;
  end;
end;

procedure TSwiWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheSWIObservations);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSWINotSupported);
  FSwiPackage := Model.ModflowPackages.SwiPackage;
  if not FSwiPackage.IsSelected then
  begin
    Exit;
  end;
  if not (Model.ModelSelection in [msModflow, msModflowNWT, msModflowFmp, msModflowOwhm2]) then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrSWINotSupported, StrTheSWIPackageIsN);
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrSWI2) then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  if not WritingTemplate then
  begin
    WriteToNameFile(StrSWI2, Model.UnitNumbers.UnitNumber(StrSWI2),
        FNameOfFile, foInput, Model);
  end;
  OpenFile(FNameOfFile);
  try
      frmProgressMM.AddMessage('Writing SWI Package input.');

      WriteDataSet0;
      WriteDataSet1;
      WriteDataSet2a;
      WriteDataSet2b;
      WriteDataSet3a;
      WriteDataSet3b;
      WriteDataSet4;
      WriteDataSet5;
      WriteDataSet6;
      WriteDataSet7;
      WriteDataSet8;

  finally
    CloseFile;
  end;

  if Model.PestStatus in [psObservations, psActive] then
  begin
    FDataToWrite := dtwObservedValues;
    WriteInterpolatedZetaFile(AFileName);
    WriteSwiObsExtBatchFile(AFileName, False);
    WriteSwiObsExtBatchFile(AFileName, True);
    FDataToWrite := dtwInstructions;
    WriteInterpolatedZetaFile(AFileName);
    WriteSwiObsExtBatchFile(AFileName, False);
    WriteSwiObsExtBatchFile(AFileName, True);
  end
  else
  begin
    WriteInterpolatedZetaFile(AFileName);
    WriteSwiObsExtBatchFile(AFileName, False);
    WriteSwiObsExtBatchFile(AFileName, True);
  end;

  WritePestScript(AFileName);
  WritePestScripts;
end;

procedure TSwiWriter.WriteInterpolatedIDs;
var
  InterpIndex: Integer;
  InterpItem: TInterpolatedObs;
  IdIndex: Integer;
  ID_Item: TInterpolatedID;
  CellIndex: Integer;
  ACell: TInterpolatedObsCell;
  StartTime: real;
begin
  StartTime := Model.ModflowStressPeriods.First.StartTime;
  WriteString(StrBEGINOBSERVATIONS);
  NewLine;

  for InterpIndex := 0 to FInterpolatedObs.Count - 1 do
  begin
    InterpItem := FInterpolatedObs[InterpIndex];

    for IdIndex := 0 to InterpItem.IDs.Count - 1 do
    begin
      ID_Item := InterpItem.IDs[IdIndex];

      WriteString('  ');
      WriteString(StrBEGINOBSERVATION);
      NewLine;

      WriteString('    ');
      WriteString(StrZetaNAME);
      WriteString(' ');
      WriteString(ID_Item.Name);
      NewLine;

      WriteString('    ');
      WriteString(StrZetaTIME);
      WriteString(' ');
      WriteFloat(ID_Item.Time - StartTime);
      NewLine;

      WriteString('    ');
      WriteString(StrZETASURFACENUMBER);
      WriteString(' ');
      WriteInteger(InterpItem.ZetaSurface);
      NewLine;

      WriteString('    ');
      WriteString(StrOBSERVEDVALUE);
      WriteString(' ');
      WriteFloat(ID_Item.ObservedValue);
      NewLine;

      for CellIndex := 0 to InterpItem.FCells.Count - 1 do
      begin
        ACell := InterpItem.FCells[CellIndex];
        WriteString('    ');
        WriteString(StrSWIOBSERVATION);
        WriteInteger(ACell.ObsNumber);
        WriteFloat(ACell.Fraction);
        WriteString(' ');
        WriteString(ACell.Name);
        NewLine;
      end;

      WriteString('  ');
      WriteString(StrENDOBSERVATION);
      NewLine;
    end;
  end;

  WriteString(StrENDOBSERVATIONS);
  NewLine;
end;

procedure TSwiWriter.WriteInterpolatedSwiFileOptions(const AFileName: string;
  ArchiveFile: Boolean = False);
var
  OutputFileName: string;
  SwiObsOutput: string;
  OutputPrefix: string;
  Modelname: string;
  ModflowOutputPrefix: string;
begin
  WriteString(StrBEGINFILEOPTIONS);
  NewLine;

  if Model.PestStatus in [psObservations, psActive] then
  begin
    WriteString('  INSTRUCTION_FILE_FORMAT PEST');
    NewLine;

    case FDataToWrite of
      dtwObservedValues:
        begin
          WriteString('  DATA_TO_WRITE VALUES');
          NewLine;
        end;
      dtwInstructions:
        begin
          WriteString('  DATA_TO_WRITE INSTRUCTIONS');
          NewLine;
        end;
      else
        Assert(False);
    end;
  end;

  OutputFileName := ChangeFileExt(AFileName, '.swi_obsi_out');
  Model.AddSwiObsExtOutputFile(OutputFileName);
  OutputFileName := ExtractFileName(OutputFileName);
  Modelname := ExtractFileName(ChangeFileExt(AFileName, ''));
  if ArchiveFile then
  begin
    OutputPrefix := '..\..\output\output.' + Modelname + '_SWI_Observation_Extractor\';
    OutputFileName := OutputPrefix + OutputFileName;
  end;
  WriteString('  ');
  WriteString(StrOUTPUTFILE);
  WriteString(' "');
  WriteString(OutputFileName);
  WriteString('"');
  NewLine;

  WriteString('  ');
  WriteString(StrSWIOBSFILE);
  WriteString(' "');
  SwiObsOutput := ExtractFileName(FObsOutputFileName);
  if ArchiveFile then
  begin
    ModflowOutputPrefix := '..\..\output\output.' + Modelname + '\';
    SwiObsOutput := ModflowOutputPrefix + SwiObsOutput;
  end;
  WriteString(SwiObsOutput);
  WriteString('"');
  NewLine;

  WriteString('  ');
  WriteString(StrSWIOBSFORMAT);
  WriteString(' ');
  if ISWIOBS > 0 then
  begin
    WriteString(StrASCII);
  end
  else
  begin
    Assert(ISWIOBS < 0);
    WriteString(StrBINARY);
    WriteString(' ');
    case FSwiPackage.ModflowPrecision of
      mpSingle:
        WriteString(StrSINGLE);
      mpDouble:
        WriteString(StrDOUBLE);
    else
      Assert(False);
    end;
  end;
  NewLine;

  WriteString('  ');
  WriteString(StrTOTALNUMBEROFOBSE);
  WriteInteger(NOBS);
  NewLine;

  WriteString('  ');
  WriteString(StrNUMBEROFZETASURFA);
  WriteInteger(NSRF);
  NewLine;
  WriteString(StrENDFILEOPTIONS);
  NewLine;
end;

procedure TSwiWriter.WriteInterpolatedZetaFile(const AFileName: string);
var
  ArchiveFileName: string;
begin
  if FInterpolatedObs.Count > 0 then
  begin
    if Model.PestStatus in [psObservations, psActive] then
    begin
      case FDataToWrite of
        dtwObservedValues:
          begin
            FNameOfFile := ChangeFileExt(AFileName, '.swi_obsiv');
          end;
        dtwInstructions:
          begin
            FNameOfFile := ChangeFileExt(AFileName, '.swi_obsit');
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      FNameOfFile := ChangeFileExt(AFileName, '.swi_obsi');
    end;
//    Model.AddSwiObsExtInputFile(FNameOfFile);
    OpenFile(FNameOfFile);
    try
      WriteInterpolatedSwiFileOptions(AFileName);

      NewLine;

      WriteInterpolatedIDs;

    finally
      CloseFile;
    end;

    ArchiveFileName := FNameOfFile + ArchiveExt;
    Model.AddSwiObsExtInputFile(ArchiveFileName);
    OpenFile(ArchiveFileName);
    try
      WriteInterpolatedSwiFileOptions(AFileName, True);

      NewLine;

      WriteInterpolatedIDs;

    finally
      CloseFile;
    end;
  end;
end;

procedure TSwiWriter.WritePestScript(const AFileName: string);
var
  ActiveDataArray: TDataArray;
  StartTime: Double;
  ObsIndex: Integer;
  Obs: TInterpolatedObs;
  IdIndex: Integer;
  ID_Item: TInterpolatedID;
  CellIndex: Integer;
  ACell: TInterpolatedObsCell;
//  Weight: double;
begin
  if (Model.PestStatus in [psObservations, psActive])
    and (FInterpolatedObs.Count > 0) then
  begin
    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;

    StartTime := Model.ModflowFullStressPeriods.First.StartTime;

    FNameOfFile := ChangeFileExt(AFileName, '.swi_obs_Script');
    Model.FileNameLines.Add(
      Format('  SWI %s', [ExtractFileName(FNameOfFile)]));

    OpenFile(FNameOfFile);
    try
      WriteString('BEGIN OBSERVATIONS');
      NewLine;

      WriteString('  FILENAME "');
      WriteString(ExtractFileName(FObsOutputFileName));
      WriteString('"');
      NewLine;

      WriteString('  ');
      WriteString(StrSWIOBSFORMAT);
      WriteString(' ');
      if ISWIOBS > 0 then
      begin
        WriteString(StrASCII);
      end
      else
      begin
        Assert(ISWIOBS < 0);
        WriteString(StrBINARY);
        WriteString(' ');
        case FSwiPackage.ModflowPrecision of
          mpSingle:
            WriteString(StrSINGLE);
          mpDouble:
            WriteString(StrDOUBLE);
        else
          Assert(False);
        end;
      end;
      NewLine;

      WriteString('  ');
      WriteString(StrTOTALNUMBEROFOBSE);
      WriteInteger(NOBS);
      NewLine;

      WriteString('  ');
      WriteString(StrNUMBEROFZETASURFA);
      WriteInteger(NSRF);
      NewLine;


      for ObsIndex := 0 to FInterpolatedObs.Count - 1 do
      begin
        Obs := FInterpolatedObs[ObsIndex];
        for IdIndex := 0 to Obs.IDs.Count - 1 do
        begin
          ID_Item := Obs.IDs[IdIndex];

          WriteString('  OBSERVATION ');
          WriteString(ID_Item.Name);
          WriteString(' "');
          WriteString('Zeta');
          WriteString('" ');
          WriteFloat(ID_Item.Time - StartTime);
          WriteFloat(ID_Item.ObservedValue);
          WriteFloat(ID_Item.Weight);
//          WriteInteger(Obs.ZetaSurface);
          WriteString(' PRINT');
          NewLine;

          WriteString('  ');
          WriteString(StrZETASURFACENUMBER);
          WriteString(' ');
          WriteInteger(Obs.ZetaSurface);
          NewLine;

          for CellIndex := 0 to Obs.FCells.Count - 1 do
          begin
            ACell := Obs.FCells[CellIndex];
            WriteString('    ');
            WriteString(StrSWIOBSERVATION);
            WriteInteger(ACell.ObsNumber);
            WriteFloat(ACell.Fraction);
            WriteString(' ');
            WriteString(ACell.Name);
            NewLine;
          end;

        end;
      end;

      WriteString('END OBSERVATIONS');
    finally
      CloseFile;
    end;
  end;
end;

procedure TSwiWriter.WritePestScripts;
var
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  for DataArrayIndex := 0 to FPestDataArrays.Count - 1 do
  begin
    ADataArray := FPestDataArrays[DataArrayIndex];
    WritePestZones(ADataArray, FInputFileName,
      Format(StrSWId, [DataArrayIndex+1]),
      Format('SW%d', [DataArrayIndex+1]));
  end;
end;

procedure TSwiWriter.WriteSwiObsExtBatchFile(const AFileName: string; ArchiveFile: Boolean);
var
  BatchFileName: string;
  BatchFile: TStringList;
  Modelname: string;
  OutputPrefix: string;
  SwiObsExtractor: string;
begin
  if FInterpolatedObs.Count > 0 then
  begin
    BatchFileName := ExtractFileDir(AFileName);
    if Model.PestStatus in [psObservations, psActive] then
    begin
      case FDataToWrite of
        dtwObservedValues:
          begin
            BatchFileName := IncludeTrailingPathDelimiter(BatchFileName)
              + 'RunSwiObsExtractor.bat';
          end;
        dtwInstructions:
          begin
            BatchFileName := IncludeTrailingPathDelimiter(BatchFileName)
              + 'GenerateSwiTemplate.bat';
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      BatchFileName := IncludeTrailingPathDelimiter(BatchFileName)
        + 'RunSwiObsExtractor.bat';
    end;
    if ArchiveFile then
    begin
      BatchFileName := BatchFileName + ArchiveExt;
    end;
    Model.AddSwiObsExtInputFile(BatchFileName);

    BatchFile := TStringList.Create;
    try
      if ArchiveFile then
      begin
        BatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');

        Modelname := ExtractFileName(ChangeFileExt(AFileName, ''));
        OutputPrefix := '..\..\output\output.' + Modelname + '_SWI_Observation_Extractor\';

        BatchFile.Add(Format('if not exist "%0:sNUL" mkdir "%0:s"', [OutputPrefix]));
        SwiObsExtractor := '..\..\bin\SwiObsExtractor.exe ';
      end
      else
      begin
        SwiObsExtractor := 'SwiObsExtractor.exe ';
      end;

      BatchFile.Add(SwiObsExtractor + ExtractFileName(FNameOfFile) {+ ' /wait'});
      if Model.PestStatus in [psObservations, psActive] then
      begin
        if FDataToWrite = dtwInstructions then
        begin
          BatchFile.Add('pause');
        end;
      end;

      BatchFile.SaveToFile(BatchFileName);
    finally
      BatchFile.Free;
    end;
  end;
end;

{ TInterpolatedObs }

constructor TInterpolatedObs.Create;
begin
//  FCells := TInterpolatedObsCellObjectList.Create;
  IDs := TInterpolatedIDObjectList.Create;
  inherited;
end;

destructor TInterpolatedObs.Destroy;
begin
//  FCells.Free;
  IDs.Free;
  inherited;
end;

function TInterpolatedObsObjectList.ObCellCount: integer;
var
  AnItem: TInterpolatedObs;
begin
  result := 0;
  for AnItem in Self do
  begin
    Inc(result, AnItem.FCells.Count);
  end;
end;

{ TInterpolatedID }

function TInterpolatedID.Weight: double;
begin
  result := 0;
  case StatFlag of
    stVariance: Result := 1/Statistic;
    stStandardDev: Result := 1/Sqr(Statistic);
    stCoefVar:  Result := 1/Sqr(Statistic * ObservedValue);
    stWeight: Result := Statistic;
    stSquaredWeight: Result := Sqrt(Statistic);
    else
      Assert(False);
  end;
end;

end.
