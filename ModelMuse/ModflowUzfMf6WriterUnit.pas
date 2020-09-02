unit ModflowUzfMf6WriterUnit;

interface

uses System.UITypes,Winapi.Windows, SysUtils, Classes, Contnrs, RbwParser,
  PhastModelUnit, Vcl.Forms,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, OrderedCollectionUnit, ModflowBoundaryDisplayUnit,
  Vcl.Dialogs, ModflowCellUnit, GoPhastTypes, System.Generics.Collections,
  ModflowUzfMf6Unit, SparseDataSets, Modflow6ObsUnit;

type
  TUzfObservation = record
    FName: string;
    FBoundName: string;
    FObsTypes: TUzfObs;
    FDepthFractions: TOneDRealArray;
    FScreenObject: TScreenObject;
    FUzfBoundNumber: TOneDIntegerArray;
    FCells: array of TCellLocation;
    FModflow6Obs: TModflow6Obs;
  end;
  TUzfObservationList = TList<TUzfObservation>;


  TModflowUzfMf6Writer = class(TCustomParameterTransientWriter)
  private
    FNameOfFile: string;
    FUzfPackage: TUzfMf6PackageSelection;
    FUzfCellNumbers: TThreeDIntegerArray;
  	FUzflandflagLayers:  TTwoDIntegerArray;
    FUzfObjectArray: T3DSparsePointerArray;
    FObsList: TUzfObservationList;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
    procedure WriteStressPeriods; reintroduce;
    procedure IdentifyWaterContentObsLocations;
  protected
    function Package: TModflowPackageSelection; override;
    procedure Evaluate; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    class function Extension: string; override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; reintroduce;
    function ObservationsUsed: Boolean;  reintroduce;
    class function ObservationExtension: string;  override;
//    class function ObservationOutputExtension: string;  override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;
implementation

uses
  ModflowUnitNumbers, frmProgressUnit, DataSetUnit,
  ModflowMvrWriterUnit, ModflowMvrUnit, System.Generics.Defaults,
  Modflow6ObsWriterUnit, ModflowIrregularMeshUnit,
  ModflowGridUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrWritingUZF6Package = 'Writing UZF6 Package input.';
  StrNoUZFBoundariesAr = 'No UZF boundaries are defined in the UZF package';
  StrTheUZFPackageIsA = 'The UZF package is active but no UZF boundaries hav' +
  'e been defined.';
  StrInTheUZFPackageResid = 'In the UZF package, residual water content must' +
  ' be greater than zero.';
  StrInTheUZFPackageInitResid = 'In the UZF package, initial water content must be' +
  ' greater or equal to residual water content.';
  StrInTheUZFPackageInitSat = 'In the UZF package, initial water content mus' +
  't be less than the saturated water content.';
  StrInTheUZFPackageSatResid = 'In the UZF package, saturated water content ' +
  'must be greater than residual water content.';
  StrAtLayerRowColuResid = 'At (Layer, Row, Column) (%0:d, %1:d, %2:d), resi' +
  'dual water content is set to %3:g by "%4:s".';
  StrAtLayerRowColuSatResid = 'At (Layer, Row, Column) (%0:d, %1:d, %2:d), s' +
  'aturated water content is set to %3:g by "%4:s" and residual water conten' +
  't is set to %5:g by "%6:s".';
  StrAtLayerRowColuInitResid = 'At (Layer, Row, Column) (%0:d, %1:d, %2:d), ' +
  'initial water content is set to %3:g by "%4:s" and residual water content' +
  ' is set to %5:g by "%6:s".';
  StrAtLayerRowColuInitSat = 'At (Layer, Row, Column) (%0:d, %1:d, %2:d), in' +
  'itial water content is set to %3:g by "%4:s" and saturated water content ' +
  'is set to %5:g by "%6:s".';

type
  TUzfCellList = TList<TMvrReceiver>;
  TListOfUzfCellLists= TObjectList<TUzfCellList>;

{ TModflowUzfMf6Writer }

function TModflowUzfMf6Writer.CellType: TValueCellType;
begin
  result := TUzfMf6_Cell;
end;

constructor TModflowUzfMf6Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FUzfObjectArray := T3DSparsePointerArray.Create(GetQuantum(Model.LayerCount),
    GetQuantum(Model.RowCount), GetQuantum(Model.ColumnCount));
  FObsList := TUzfObservationList.Create;
end;

destructor TModflowUzfMf6Writer.Destroy;
begin
  FObsList.Free;
  FUzfObjectArray.Free;
  inherited;
end;

procedure TModflowUzfMf6Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TUzfMf6Boundary;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
//  BoundName: string;
  Obs: TUzfObservation;
  MfObs: TModflow6Obs;
begin
  FUzfPackage := Package as TUzfMf6PackageSelection;
  inherited;
  CellList := TCellAssignmentList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowUzfMf6Boundary;
      if Boundary <> nil then
      begin
        if ObservationsUsed and IsMf6Observation(ScreenObject) then
        begin
          MfObs := ScreenObject.Modflow6Obs;
          Obs.FName := MfObs.Name;
          Obs.FBoundName := ScreenObject.Name;
          Obs.FObsTypes := MfObs.UzfObs;
          Obs.FScreenObject := ScreenObject;
          Obs.FCells := nil;
          Obs.FModflow6Obs := MfObs;
          FObsList.Add(Obs);
        end;
        CellList.Clear;
        ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          FUzfObjectArray[ACell.Layer, ACell.Row, ACell.Column] := ScreenObject;
        end;
      end
      else if ObservationsUsed and IsMf6Observation(ScreenObject) then
      begin
        MfObs := ScreenObject.Modflow6Obs;
        Obs.FName := MfObs.Name;
        Obs.FBoundName := ScreenObject.Name;
        Obs.FObsTypes := MfObs.UzfObs;
        Obs.FScreenObject := ScreenObject;
        Obs.FModflow6Obs := MfObs;
        CellList.Clear;
        ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        SetLength(Obs.FCells, CellList.Count);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          Obs.FCells[CellIndex] := ACell.Cell;
        end;

        FObsList.Add(Obs);
      end;
    end;
  finally
    CellList.Free;
  end;
end;

class function TModflowUzfMf6Writer.Extension: string;
begin
  result := '.uzf';
end;

function TModflowUzfMf6Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowUzfMf6Boundary;
end;

procedure TModflowUzfMf6Writer.IdentifyWaterContentObsLocations;
var
  ObsIndex: Integer;
  UzfObs: TUzfObservation;
  CellList: TCellAssignmentList;
  CellNumbers: TList<Integer>;
  CellIndex: Integer;
  ACell: TCellAssignment;
  DepthFraction: double;
  DisvGrid: TModflowDisvGrid;
  Grid: TModflowGrid;
  ACellLocation: TCellLocation;
  DepthFractions: TList<double>;
begin
  if ObservationsUsed then
  begin
    if Model.DisvUsed then
    begin
      DisvGrid := Model.DisvGrid;
      Grid := nil;
    end
    else
    begin
      Grid := Model.ModflowGrid;
      DisvGrid := nil;
    end;

    CellNumbers := TList<Integer>.Create;
    DepthFractions := TList<double>.Create;
    CellList := TCellAssignmentList.Create;
    try
      for ObsIndex := 0 to FObsList.Count - 1 do
      begin
        UzfObs := FObsList[ObsIndex];
        if UzfObs.FCells = nil then
        begin
          if (uoWaterContent in UzfObs.FObsTypes) then
          begin
            CellList.Clear;
            CellNumbers.Clear;
            DepthFractions.Clear;
            DepthFraction := UzfObs.FScreenObject.Modflow6Obs.UzfObsDepthFraction;
            UzfObs.FScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
            for CellIndex := 0 to CellList.Count - 1 do
            begin
              ACell := CellList[CellIndex];
              if (FUzfObjectArray[ACell.Layer, ACell.Row, ACell.Column] = UzfObs.FScreenObject) then
              begin
                CellNumbers.Add(FUzfCellNumbers[ACell.Layer, ACell.Row, ACell.Column]);
                if Assigned(DisvGrid) then
                begin
                  DepthFractions.Add(DepthFraction
                    * DisvGrid.CellThickness(ACell.Layer, ACell.Row, ACell.Column));
                end
                else
                begin
                  DepthFractions.Add(DepthFraction
                    * Grid.LayerThickness(ACell.Layer, ACell.Row, ACell.Column));
                end;
              end;
            end;
            SetLength(UzfObs.FUzfBoundNumber, CellNumbers.Count);
            SetLength(UzfObs.FDepthFractions, CellNumbers.Count);
            for CellIndex := 0 to CellNumbers.Count - 1 do
            begin
              UzfObs.FUzfBoundNumber[CellIndex] := CellNumbers[CellIndex];
              UzfObs.FDepthFractions[CellIndex] := DepthFractions[CellIndex];
            end;
            FObsList[ObsIndex] := UzfObs;
          end;
        end
        else
        begin
          CellList.Clear;
          CellNumbers.Clear;
          DepthFractions.Clear;
          DepthFraction := UzfObs.FScreenObject.Modflow6Obs.UzfObsDepthFraction;
          for CellIndex := 0 to Length(UzfObs.FCells) - 1 do
          begin
            ACellLocation := UzfObs.FCells[CellIndex];
            if (FUzfCellNumbers[ACellLocation.Layer, ACellLocation.Row, ACellLocation.Column] > 0) then
            begin
              CellNumbers.Add(FUzfCellNumbers[ACellLocation.Layer, ACellLocation.Row, ACellLocation.Column]);
              if Assigned(DisvGrid) then
              begin
                DepthFractions.Add(DepthFraction
                  * DisvGrid.CellThickness(ACellLocation.Layer, ACellLocation.Row, ACellLocation.Column));
              end
              else
              begin
                DepthFractions.Add(DepthFraction
                  * Grid.LayerThickness(ACellLocation.Layer, ACellLocation.Row, ACellLocation.Column));
              end;
            end;
          end;
          SetLength(UzfObs.FUzfBoundNumber, CellNumbers.Count);
          SetLength(UzfObs.FDepthFractions, CellNumbers.Count);
          for CellIndex := 0 to CellNumbers.Count - 1 do
          begin
            UzfObs.FUzfBoundNumber[CellIndex] := CellNumbers[CellIndex];
            UzfObs.FDepthFractions[CellIndex] := DepthFractions[CellIndex];
          end;
          FObsList[ObsIndex] := UzfObs;
        end;
      end;
    finally
      CellList.Free;
      CellNumbers.Free;
      DepthFractions.Free;
    end;
  end;
end;

function TModflowUzfMf6Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (MfObs.UzfObs <> []);
end;

class function TModflowUzfMf6Writer.ObservationExtension: string;
begin
  result := '.ob_uzf';
end;

function TModflowUzfMf6Writer.ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowUzfMf6Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.UzfMf6Package;
end;

function TModflowUzfMf6Writer.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

procedure TModflowUzfMf6Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  Infiltration: TModflowBoundaryDisplayTimeList;
  InfiltrationArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
  TimeIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  Evaluate;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if Values.Count = 0 then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;

  try
    Infiltration := TimeLists[0];
    for TimeIndex := 0 to Values.Count - 1 do
    begin
      InfiltrationArray := Infiltration[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      CellList := Values[TimeIndex];
//      CellList.CheckRestore;
      AssignTransient2DArray(InfiltrationArray, 0, CellList, 0,
        rdtDouble, Model.ModflowPackages.UzfPackage.AssignmentMethod);
      Model.AdjustDataArray(InfiltrationArray);
      CellList.Cache;
    end;


  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

end;

procedure TModflowUzfMf6Writer.WriteDimensions;
var
  UzfDataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IDOMAINDataArray: TDataArray;
  uzfcells: Integer;
begin
  WriteBeginDimensions;

  IDOMAINDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  UzfDataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
  IDOMAINDataArray.Initialize;
  UzfDataArray.Initialize;
  uzfcells := 0;
  for LayerIndex := 0 to UzfDataArray.LayerCount - 1 do
  begin
    for RowIndex := 0 to UzfDataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to UzfDataArray.ColumnCount - 1 do
      begin
        if UzfDataArray.IsValue[LayerIndex, RowIndex, ColIndex]
          and (IDOMAINDataArray.IntegerData[LayerIndex, RowIndex, ColIndex] > 0) then
        begin
          Inc(uzfcells);
        end;
      end;
    end;
  end;

  WriteString('  NUZFCELLS');
  WriteInteger(uzfcells);
  NewLine;

  WriteString('  NTRAILWAVES');
  WriteInteger(FUzfPackage.NumberOfTrailingWaves);
  NewLine;

  WriteString('  NWAVESETS');
  WriteInteger(FUzfPackage.NumberOfWaveSets);
  NewLine;

  WriteEndDimensions
end;

procedure TModflowUzfMf6Writer.WriteFile(const AFileName: string);
var
  ObsFileName: string;
  ObsWriter: TUzfObsWriter;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoUZFBoundariesAr);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheUZFPackageResid);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheUZFPackageSatResid);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheUZFPackageInitResid);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheUZFPackageInitSat);

  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit
  end;
  if not Model.ModflowPackages.UzfMf6Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrUZF6) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingUZF6Package);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrUZF6, 0, FNameOfFile, foInput, Model);

  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDimensions);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingPackageData);
    WritePackageData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingStressPerio);
    WriteStressPeriods;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;


  if FObsList.Count > 0 then
  begin
    IdentifyWaterContentObsLocations;
    ObsFileName := ChangeFileExt(FNameOfFile, ObservationExtension);
    ObsWriter := TUzfObsWriter.Create(Model, etExport, FObsList);
    try
      ObsWriter.WriteFile(ObsFileName);
    finally
      ObsWriter.Free;
    end;
  end;
end;

procedure TModflowUzfMf6Writer.WriteOptions;
var
  budgetfile: string;
  ObsFileName: string;
  CsvFile: string;
begin
  WriteBeginOptions;

  PrintListInputOption;
  PrintFlowsOption;
  WriteSaveFlowsOption;

  if FUzfPackage.SaveBudgetFile then
  begin
    WriteString('  BUDGET FILEOUT ');
    budgetfile := ChangeFileExt(FNameOfFile, '.uzf_budget');
    Model.AddModelOutputFile(budgetfile);
    budgetfile := ExtractFileName(budgetfile);
    WriteString(budgetfile);
    NewLine;
  end;

  if FUzfPackage.WriteConvergenceData then
  begin
    WriteString('  PACKAGE_CONVERGENCE FILEOUT ');
    CsvFile := ChangeFileExt(FNameOfFile, '.SfrConvergence.CSV');
    Model.AddModelOutputFile(CsvFile);
    CsvFile := ExtractFileName(CsvFile);
    WriteString(CsvFile);
    NewLine;
  end;

  WriteBoundNamesOption;

  if (MvrWriter <> nil) then
  begin
    if spcUzf in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;


  if FUzfPackage.SimulateET then
  begin
    WriteString('  SIMULATE_ET');
    NewLine;
  end;

  if FUzfPackage.GroundwaterET = ugecLinear then
  begin
    WriteString('  LINEAR_GWET');
    NewLine;
  end;

  if FUzfPackage.GroundwaterET = ugecSquare then
  begin
    WriteString('  SQUARE_GWET');
    NewLine;
  end;

  if FUzfPackage.SimulateGroundwaterSeepage then
  begin
    WriteString('  SIMULATE_GWSEEP');
    NewLine;
  end;

  if FUzfPackage.UnsatET = uuecWaterContent then
  begin
    WriteString('  UNSAT_ETWC');
    NewLine;
  end;

  if FUzfPackage.UnsatET = uuecCapillaryPressure then
  begin
    WriteString('  UNSAT_ETAE');
    NewLine;
  end;

  if FObsList.Count > 0 then
  begin
    ObsFileName := ExtractFileName(ChangeFileExt(FNameOfFile, ObservationExtension));
    WriteString('  OBS6 FILEIN ');
    WriteString(ObsFileName);
    NewLine;
  end;

  WriteEndOptions;
end;

procedure TModflowUzfMf6Writer.WritePackageData;
var
  IDOMAINDataArray: TDataArray;
  SurfaceDepressionDepthDataArray: TDataArray;
  VerticalSaturatedKDataArray: TDataArray;
  ReisidualWaterContentDataArray: TDataArray;
  SaturatedWaterContentDataArray: TDataArray;
  InitialUnsaturatedWaterContentDataArray: TDataArray;
  BrooksCoreyEpsilonDataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  CellNumber: Integer;
  DisvUsed: Boolean;
  InnerLayerIndex: Integer;
  landflag: Integer;
  ivertcon: Integer;
  surfdep: Double;
  vks: Double;
  thtr: Double;
  thts: Double;
  thti: Double;
  eps: Double;
  ScreenObject: TScreenObject;
begin
  WriteBeginPackageData;

  IDOMAINDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  SurfaceDepressionDepthDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
  VerticalSaturatedKDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);
  ReisidualWaterContentDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
  SaturatedWaterContentDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
  InitialUnsaturatedWaterContentDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
  BrooksCoreyEpsilonDataArray :=
    Model.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);

  SurfaceDepressionDepthDataArray.Initialize;
  VerticalSaturatedKDataArray.Initialize;
  ReisidualWaterContentDataArray.Initialize;
  SaturatedWaterContentDataArray.Initialize;
  InitialUnsaturatedWaterContentDataArray.Initialize;
  BrooksCoreyEpsilonDataArray.Initialize;

  CellNumber := 0;
  SetLength(FUzfCellNumbers, IDOMAINDataArray.LayerCount,
    IDOMAINDataArray.RowCount, IDOMAINDataArray.ColumnCount);
  SetLength(FUzflandflagLayers, IDOMAINDataArray.RowCount, IDOMAINDataArray.ColumnCount);
  for LayerIndex := 0 to IDOMAINDataArray.LayerCount - 1 do
  begin
    for RowIndex := 0 to IDOMAINDataArray.RowCount - 1 do
    begin
      for ColumnIndex := 0 to IDOMAINDataArray.ColumnCount - 1 do
      begin
        if SurfaceDepressionDepthDataArray.IsValue[LayerIndex, RowIndex, ColumnIndex]
          and (IDOMAINDataArray.IntegerData[LayerIndex, RowIndex, ColumnIndex] > 0) then
        begin
          Inc(CellNumber);
          FUzfCellNumbers[LayerIndex, RowIndex, ColumnIndex] := CellNumber;
        end
        else
        begin
          FUzfCellNumbers[LayerIndex, RowIndex, ColumnIndex] := 0;
        end;
      end;
    end;
  end;

  for RowIndex := 0 to IDOMAINDataArray.RowCount - 1 do
  begin
    for ColumnIndex := 0 to IDOMAINDataArray.ColumnCount - 1 do
    begin
	    FUzflandflagLayers[RowIndex, ColumnIndex] := -1;
	  end;
	end;
  
  DisvUsed := Model.DisvUsed;
  for LayerIndex := 0 to IDOMAINDataArray.LayerCount - 1 do
  begin
    for RowIndex := 0 to IDOMAINDataArray.RowCount - 1 do
    begin
      for ColumnIndex := 0 to IDOMAINDataArray.ColumnCount - 1 do
      begin
        if FUzfCellNumbers[LayerIndex, RowIndex, ColumnIndex] > 0 then
        begin
          WriteString('  ');
          WriteInteger(FUzfCellNumbers[LayerIndex, RowIndex, ColumnIndex]);
          WriteInteger(LayerIndex+1);
          if not DisvUsed then
          begin
            WriteInteger(RowIndex+1);
          end;
          WriteInteger(ColumnIndex+1);

          landflag := 1;
          for InnerLayerIndex := LayerIndex-1 downto 0 do
          begin
            if FUzfCellNumbers[InnerLayerIndex, RowIndex, ColumnIndex] > 0 then
            begin
              landflag := 0;
              Break;
            end;
          end;
          WriteInteger(landflag);
		  
          if landflag = 1 then
          begin
            FUzflandflagLayers[RowIndex, ColumnIndex]  := LayerIndex;
          end;

          ivertcon := 0;
          for InnerLayerIndex := LayerIndex+1 to IDOMAINDataArray.LayerCount -1 do
          begin
            if FUzfCellNumbers[InnerLayerIndex, RowIndex, ColumnIndex] > 0 then
            begin
              ivertcon := FUzfCellNumbers[InnerLayerIndex, RowIndex, ColumnIndex];
              Break;
            end;
          end;
          WriteInteger(ivertcon);

          surfdep := SurfaceDepressionDepthDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(surfdep);

          vks := VerticalSaturatedKDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(vks);

          thtr := ReisidualWaterContentDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(thtr);
          if thtr <= 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrInTheUZFPackageResid,
              Format(StrAtLayerRowColuResid,
              [LayerIndex+1, RowIndex+1, ColumnIndex+1, thtr,
              ReisidualWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex]]));
          end;


          thts := SaturatedWaterContentDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(thts);
          if thts - thtr <= 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrInTheUZFPackageSatResid,
              Format(StrAtLayerRowColuSatResid,
              [LayerIndex+1, RowIndex+1, ColumnIndex+1, thts,
              SaturatedWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex],
              thtr, ReisidualWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex]]));
          end;

          thti := InitialUnsaturatedWaterContentDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(thti);

          if thti - thtr < 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrInTheUZFPackageInitResid,
              Format(StrAtLayerRowColuInitResid,
              [LayerIndex+1, RowIndex+1, ColumnIndex+1, thti,
              InitialUnsaturatedWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex],
              thtr, ReisidualWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex]]));
          end;

          if thti >= thts then
          begin
            frmErrorsAndWarnings.AddError(Model, StrInTheUZFPackageInitSat,
              Format(StrAtLayerRowColuInitSat,
              [LayerIndex+1, RowIndex+1, ColumnIndex+1, thti,
              InitialUnsaturatedWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex],
              thts, SaturatedWaterContentDataArray.Annotation[LayerIndex, RowIndex, ColumnIndex]]));
          end;

          eps := BrooksCoreyEpsilonDataArray.RealData[LayerIndex, RowIndex, ColumnIndex];
          WriteFloat(eps);

          ScreenObject := FUzfObjectArray[LayerIndex, RowIndex, ColumnIndex];
          if ScreenObject <> nil then
          begin
            WriteString(' ');
            WriteString(ScreenObject.Name);
          end;
          NewLine;
        end;
      end;
    end;
  end;

  WriteEndPackageData;
end;

procedure TModflowUzfMf6Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
end;

procedure TModflowUzfMf6Writer.WriteParameterDefinitions(const DS3,
  DS3Instances, DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
end;

procedure TModflowUzfMf6Writer.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  CellList: TValueCellList;
  CellIndex: Integer;
  UzfCell: TUzfMf6_Cell;
  UsedUzfCells: array of array of array of TUzfMf6_Cell;
  IDOMAINDataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  MvrKey: TMvrRegisterKey;
  BoundaryIndex: Integer;
  MvrReceiver: TMvrReceiver;
  MoverWriter: TModflowMvrWriter;
  AUzfCellList: TUzfCellList;
  ListOfUzfCellLists: TListOfUzfCellLists;
  ReceiverComparer: IEqualityComparer<TMvrReceiverKey>;
  ReceiverDictionary: TDictionary<TMvrReceiverKey, TUzfCellList>;
  CellListIndex: Integer;
begin
  if MvrWriter <> nil then
  begin
    MoverWriter := MvrWriter as TModflowMvrWriter;
  end
  else
  begin
    MoverWriter := nil;
  end;
  MvrReceiver.ReceiverKey.ReceiverPackage := rpcUzf;
  IDOMAINDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  ReceiverComparer := TMvrReceiverKeyComparer.Create;

  ListOfUzfCellLists := TListOfUzfCellLists.Create;
  try
    for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      ListOfUzfCellLists.Clear;
      // In each stress period create lists of cells for each screen object
      // and add their numbers to TMvrReceiverValues.UzfCells
      BoundaryIndex := 0;
      SetLength(UsedUzfCells, IDOMAINDataArray.LayerCount,
        IDOMAINDataArray.RowCount, IDOMAINDataArray.ColumnCount);
      for LayerIndex := 0 to IDOMAINDataArray.LayerCount - 1 do
      begin
        for RowIndex := 0 to IDOMAINDataArray.RowCount - 1 do
        begin
          for ColumnIndex := 0 to IDOMAINDataArray.ColumnCount - 1 do
          begin
            UsedUzfCells[LayerIndex, RowIndex, ColumnIndex] := nil;
          end;
        end;
      end;

      if Values.Count = 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrNoUZFBoundariesAr,
          StrTheUZFPackageIsA);
        Exit;
      end;

      CellList := Values[StressPeriodIndex];
      WriteBeginPeriod(StressPeriodIndex);
      MvrReceiver.ReceiverKey.StressPeriod := StressPeriodIndex;

      // Eliminate duplicate cells at the same location.
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        UzfCell := CellList[CellIndex] as TUzfMf6_Cell;
        if IDOMAINDataArray.IntegerData[UzfCell.Layer, UzfCell.Row, UzfCell.Column] > 0 then
        begin
          Assert(FUzfCellNumbers[UzfCell.Layer, UzfCell.Row, UzfCell.Column] > 0);
          Assert(UsedUzfCells[UzfCell.Layer, UzfCell.Row, UzfCell.Column] = nil);
          UsedUzfCells[UzfCell.Layer, UzfCell.Row, UzfCell.Column] := UzfCell;
        end;
      end;

      ReceiverDictionary := TDictionary<TMvrReceiverKey, TUzfCellList>.
        Create(ReceiverComparer);
      try
        for LayerIndex := 0 to IDOMAINDataArray.LayerCount - 1 do
        begin
          for RowIndex := 0 to IDOMAINDataArray.RowCount - 1 do
          begin
            for ColumnIndex := 0 to IDOMAINDataArray.ColumnCount - 1 do
            begin
              UzfCell := UsedUzfCells[LayerIndex, RowIndex, ColumnIndex];
              if (UzfCell <> nil) and
                (FUzflandflagLayers[RowIndex, ColumnIndex] = UzfCell.Layer) then
              begin
                WriteString('  ');
                WriteInteger(FUzfCellNumbers[LayerIndex, RowIndex, ColumnIndex]);
                WriteFloat(UzfCell.Infiltration);
                WriteFloat(UzfCell.PotentialET);
                WriteFloat(UzfCell.ExtinctionDepth);
                WriteFloat(UzfCell.ExtinctionWaterContent);
                WriteFloat(UzfCell.AirEntryPotential);
                WriteFloat(UzfCell.RootPotential);
                WriteFloat(UzfCell.RootActivity);
                NewLine;
                MvrReceiver.ReceiverKey.ScreenObject := UzfCell.ScreenObject;

                Inc(BoundaryIndex);
                MvrReceiver.ReceiverValues.Index := BoundaryIndex;

                if UzfCell.MvrUsed and (MoverWriter <> nil) then
                begin
                  MvrKey.StressPeriod := StressPeriodIndex;
                  MvrKey.Index := BoundaryIndex;
                  MvrKey.SourceKey.MvrIndex := UzfCell.MvrIndex;
                  MvrKey.SourceKey.ScreenObject := UzfCell.ScreenObject;
                  MoverWriter.AddMvrSource(MvrKey);
                end;

                if MoverWriter <> nil then
                begin
                  if not ReceiverDictionary.TryGetValue(MvrReceiver.ReceiverKey, AUzfCellList) then
                  begin
                    AUzfCellList := TUzfCellList.Create;
                    ListOfUzfCellLists.Add(AUzfCellList);
                    ReceiverDictionary.Add(MvrReceiver.ReceiverKey, AUzfCellList);
                  end;
                  AUzfCellList.Add(MvrReceiver);
                end;

              end;
            end;
          end;
        end;
        WriteEndPeriod;

        for CellListIndex := 0 to ListOfUzfCellLists.Count - 1 do
        begin
          AUzfCellList := ListOfUzfCellLists[CellListIndex];
          Assert(AUzfCellList.Count >= 1);
          MvrReceiver := AUzfCellList[0];
          SetLength(MvrReceiver.ReceiverValues.UzfCells, AUzfCellList.Count);
          for CellIndex := 0 to AUzfCellList.Count - 1 do
          begin
            MvrReceiver.ReceiverValues.UzfCells[CellIndex]
              := AUzfCellList[CellIndex].ReceiverValues.Index;
          end;
          MoverWriter.AddMvrReceiver(MvrReceiver);
        end;
      finally
        ReceiverDictionary.Free;
      end;

    end;
  finally
    ListOfUzfCellLists.Free;
  end;
end;

end.
