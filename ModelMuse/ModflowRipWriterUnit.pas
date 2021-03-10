unit ModflowRipWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, CustomModflowWriterUnit, ModflowPackageSelectionUnit, System.Classes,
  GoPhastTypes, System.Contnrs, PhastModelUnit, System.Generics.Collections,
  ModflowRipUnit, ModflowCellUnit, System.SysUtils, RbwParser,
  ModflowRipPlantGroupsUnit, ModflowBoundaryDisplayUnit, Vcl.Dialogs;

type
  TRipStressPeriodPolygons = class(TList<TRip_Cell>)
  private
    FStressPeriod: Integer;
  public
    property StressPeriod: Integer read FStressPeriod write FStressPeriod;
  end;

  TRipCell = class(TObject)
  private
    FStressPeriods: TObjectList<TRipStressPeriodPolygons>;
    FCurrentStressPeriod: integer;
    function GetCurrentStressPeriod: TRipStressPeriodPolygons;
    function GetNextStressPeriod: TRipStressPeriodPolygons;
  public
    constructor Create;
    destructor Destroy; override;
    property CurrentStressPeriod: TRipStressPeriodPolygons
      read GetCurrentStressPeriod;
    property NextStressPeriod: TRipStressPeriodPolygons read GetNextStressPeriod;
    function AddStressPeriod(StressPeriod: Integer): TRipStressPeriodPolygons;
    procedure ResetCurrentStressPeriod;
  end;

  TRipCellList = TList<TRipCell>;

  TModflowRipWriter = class(TCustomTransientWriter)
  private
    // After @link(Evaluate) is called,
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
//    FValues: TList;
    MAXRIP: Integer;
    MAXPOLY: Integer;
    FCells: array of array of TRipCell;
    FCellList: TObjectList<TRipCell>;
    FCompiler: TRbwParser;
    FPlantGroups: TRipPlantGroups;
    FNameOfFile: string;
    procedure CountCellsAndPolygons;
    function GetCell(Col, Row: Integer): TRipCell;
    procedure SetCell(Col, Row: Integer; const Value: TRipCell);
    procedure ClearTimeLists(AModel: TBaseModel);
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3to5;
    function FormulaToRealValue(var AFormula: string): double;
    procedure WriteDataSet3(APlantGroup: TRipPlantGroup);
    procedure WriteDataSet4(APlantGroup: TRipPlantGroup);
    procedure WriteDataSet5(APlantGroup: TRipPlantGroup);
    procedure WriteStressPeriod(StressPeriodIndex: Integer);
    procedure WriteStressPeriods;
    procedure WriteDataSet6(CellList: TRipCellList; StressPeriodIndex: integer);
    procedure GetCellsForCurrentStessPeriod(CellList: TRipCellList;
      StressPeriodIndex: integer);
    procedure WriteDataSets7and8(CellList: TRipCellList; StressPeriodIndex: integer);
    procedure WriteDataSet7(ARipCell: TRipCell);
    procedure WriteDataSet8(ARipCell: TRipCell; StressPeriodIndex: integer);
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    procedure Evaluate; override;
    property Cells[Col, Row: Integer]: TRipCell read GetCell write SetCell;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  frmProgressUnit, ScreenObjectUnit, ModflowUnitNumbers, Vcl.Forms,
  frmGoPhastUnit, frmFormulaErrorsUnit,
  GIS_Functions, AbstractGridUnit, frmErrorsAndWarningsUnit, DataSetUnit;

resourcestring
  StrWritingRIPPackage = 'Writing RIP Package input.';
  StrPlantGroupS = 'Plant Group: %s';
  StrSaturatedExtinction = 'Saturated Extinction Depth (Sxd)';
  StrTheFormulaHasBeen = 'The formula has been reset to "0".';
  StrActiveRootDepthA = 'Active root depth (Ard)';
  StrMaximumETRmax = 'Maximum ET (Rmax)';
  StrSaturatedExtinctionEvap = 'Saturated extinction evaporation (Rsxd)';
  StrNoPlantGroupsDefi = 'No plant groups defined';
  StrNoPlantGroupsWere = 'No plant groups were defined for the Riparean Evap' +
  'otranspiration package. You can define plant groups in "%:0s|%1:s"';
  StrNoTranspirationRat = 'No Transpiration Rate Curve defined for the follo' +
  'wing Plant Groups';
  StrRIPCoverageExceeds = 'RIP Coverage exceeds 1 in the following cells';
  StrStressPeriod0d = 'Stress Period: %0:d; (Layer, Row, Col): (%1:d, %2:d, ' +
  '%3:d)';
  StrStressPeriodD = 'Stress Period %d';
  StrDataSet6SI = ' # Data Set 6, %s: ITMP';
  StrRipDepthError = 'One or more plant groups in the RIP package have trans' +
  'piration curves in which the dimensionless depths do not add up to 1.';
  StrRipRateError = 'One or more plant groups in the RIP package have transp' +
  'iration curves in which the relative rates do not result in a maximum rel' +
  'ative rate of 1.';
  StrOneOrMoreRiparian = 'One or more Riparian ET boundaries are in inactive' +
  ' cells';
  StrObject0sLayerRip = 'Object: %0:s; (Layer,Row,Column): (%1:d, %2:d, %3:d); ' +
  'Stress period: %4:d';
  StrTheRIPPackageIsN = 'The RIP package is not supported by MT3DMS.';
  StrMT3DMSVersion53D = 'MT3DMS version 5.3 and MT3D-USGS do not suppport the RIP packag' +
  'e.';

{ TModflowRipWriter }

procedure TModflowRipWriter.ClearTimeLists(AModel: TBaseModel);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TRipBoundary;
begin
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
    Boundary := ScreenObject.ModflowRipBoundary;
    if Boundary <> nil then
    begin
      Boundary.ClearTimeLists(AModel);
    end;
  end;

end;

procedure TModflowRipWriter.CountCellsAndPolygons;
var
  StressPeriodIndex: Integer;
  CellList: TValueCellList;
  CellIndex: Integer;
  ACell: TRip_Cell;
  RipCell: TRipCell;
  RipStressPeriod: TRipStressPeriodPolygons;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataArray.Initialize;
  MAXRIP := 0;
  MAXPOLY := 0;
  for StressPeriodIndex := 0 to Values.Count - 1 do
  begin
    CellList := Values[StressPeriodIndex];
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      ACell := CellList[CellIndex] as TRip_Cell;
      if not ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, ACell.Column] then
      begin
        ScreenObject := ACell.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model, StrOneOrMoreRiparian,
          Format(StrObject0sLayerRip, [ScreenObject.Name, ACell.Layer+1,
          ACell.Row+1, ACell.Column+1, StressPeriodIndex+1]), ScreenObject);
      end;
      RipCell := Cells[ACell.Column, ACell.Row];
      if RipCell = nil then
      begin
        RipCell := TRipCell.Create;
        Cells[ACell.Column, ACell.Row] := RipCell;
        FCellList.Add(RipCell);
        Inc(MAXRIP);
      end;
      RipStressPeriod := RipCell.CurrentStressPeriod;
      if (RipStressPeriod = nil)
         or (RipStressPeriod.StressPeriod <> StressPeriodIndex) then
      begin
        RipStressPeriod := RipCell.AddStressPeriod(StressPeriodIndex);
      end;
      RipStressPeriod.Add(ACell);
      if RipStressPeriod.Count > MAXPOLY then
      begin
        MAXPOLY := RipStressPeriod.Count;
      end;
    end;
  end;
end;

constructor TModflowRipWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  FCellList := TObjectList<TRipCell>.Create;
  if Model.Grid <> nil then
  begin
    SetLength(FCells, Model.Grid.ColumnCount, Model.Grid.RowCount);
    for RowIndex := 0 to Model.Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
      begin
        FCells[ColIndex,RowIndex] := nil;
      end;
    end;
  end;
end;

destructor TModflowRipWriter.Destroy;
begin
  FCellList.Free;
//  FValues.Free;
  inherited;
end;

procedure TModflowRipWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TRipBoundary;
  NoAssignmentErrorRoot: string;
  LocalModel: TPhastModel;
  RipErrors: TStringList;
  ErrorIndex: Integer;
begin
  NoAssignmentErrorRoot :=
    Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoPlantGroupsDefi);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoTranspirationRat);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRIPCoverageExceeds);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRipDepthError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRipRateError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreRiparian);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheRIPPackageIsN);

  if Model.ModflowPackages.Mt3dBasic.IsSelected then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTheRIPPackageIsN,
      StrMT3DMSVersion53D);
  end;

  LocalModel := Model.ParentModel as TPhastModel;
  FPlantGroups := LocalModel.RipPlantGroups;

  RipErrors := TStringList.Create;
  try
    RipErrors.Assign(FPlantGroups.PlantGroupWithBadDepthSegments);
    for ErrorIndex := 0 to RipErrors.Count - 1 do
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrRipDepthError,
        RipErrors[ErrorIndex]);
    end;
    RipErrors.Assign(FPlantGroups.PlantGroupWithBadRateSegments);
    for ErrorIndex := 0 to RipErrors.Count - 1 do
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrRipRateError,
        RipErrors[ErrorIndex]);
    end;
  finally
    RipErrors.Free;
  end;

  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowRipBoundary;
    if Boundary <> nil then
    begin
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
          ScreenObject.Name, ScreenObject);
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingS,
        [ScreenObject.Name]));
      Boundary.GetCellValues(Values, nil, Model);
    end;
  end;
  CountCellsAndPolygons;
end;

class function TModflowRipWriter.Extension: string;
begin
  result := '.rip';
end;

function TModflowRipWriter.GetCell(Col, Row: Integer): TRipCell;
begin
  result := FCells[Col, Row];
end;

function TModflowRipWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.RipPackage;
end;

procedure TModflowRipWriter.SetCell(Col, Row: Integer; const Value: TRipCell);
begin
  Assert(FCells[Col, Row] = nil);
  FCells[Col, Row] := Value;
end;

procedure TModflowRipWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataArrayList: TList;
  TimeIndex: Integer;
  TimeListIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  CellList: TValueCellList;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    DataArrayList := TList.Create;
    try
      Evaluate;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        // Values.Count can be zero if no objects define the boundary condition.
        if (Values.Count <> 0) or (DisplayTimeList.Count = 0) then
        begin
          Assert(Values.Count = DisplayTimeList.Count);
        end;
      end;

      // For each stress period, transfer values from
      // the cells lists to the data arrays.
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        CellList := Values[TimeIndex];
        if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
          for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];
            DataArray := DisplayTimeList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;
          UpdateCellDisplay(CellList, DataArrayList,
            [0..TimeLists.Count-1]);
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataArrayList.Free;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

end;

procedure TModflowRipWriter.WriteDataSet1;
var
  IRIPCB: Integer;
  IRIPCB1: Integer;
  NameOfFile: string;
begin
  GetFlowUnitNumber(IRIPCB);
  if (Package as TRipPackage).WritePlantGroupET = wpgWrite then
  begin
    IRIPCB1 := Model.UnitNumbers.UnitNumber(StrRipPlantGroupET);
    NameOfFile := ChangeFileExt(FNameOfFile, '.rip_et');
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, Model.UnitNumbers.UnitNumber(StrRipPlantGroupET),
            NameOfFile, foOutput, Model);
    end;
  end
  else
  begin
    IRIPCB1 := 0
  end;

  WriteInteger(MAXRIP);
  WriteInteger(MAXPOLY);
  WriteInteger(IRIPCB);
  WriteInteger(IRIPCB1);
  WriteString(' # Data Set 1: MAXRIP MAXPOLY IRIPCB IRIPCB1');
  NewLine;

end;

procedure TModflowRipWriter.WriteDataSet2;
var
  MAXTS: integer;
  PlantGroupIndex: Integer;
  APlantGroup: TRipPlantGroup;
  MXSEG: Integer;
begin
  MAXTS := FPlantGroups.Count;
  if MAXTS = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoPlantGroupsDefi,
      Format(StrNoPlantGroupsWere,
      [frmGoPhast.miModel.Caption, frmGoPhast.miRipareanETPlantGroups.Caption]));
  end;
  MXSEG := 0;
  for PlantGroupIndex := 0 to FPlantGroups.Count - 1 do
  begin
    APlantGroup := FPlantGroups[PlantGroupIndex];
    if APlantGroup.ETSegments.Count > MXSEG then
    begin
      MXSEG := APlantGroup.ETSegments.Count;
    end;
    if APlantGroup.ETSegments.Count = 0 then
    begin
    frmErrorsAndWarnings.AddError(Model, StrNoTranspirationRat,
      APlantGroup.Name);
    end;
  end;

  WriteInteger(MAXTS);
  WriteInteger(MXSEG);
  WriteString(' # Data Set 2: MAXTS MXSEG');
  NewLine;

end;

function TModflowRipWriter.FormulaToRealValue(var AFormula: string): double;
begin
  UpdateCurrentModel(Model);
  UpdateCurrentScreenObject(nil);
  try
    FCompiler.Compile(AFormula);
  except on E: ErbwParserError do
    begin
      AFormula := '0';
      raise;
      Exit;
    end;
  end;
  FCompiler.CurrentExpression.Evaluate;
  result := FCompiler.CurrentExpression.DoubleResult;
end;

procedure TModflowRipWriter.WriteDataSets3to5;
var
  LocalModel: TPhastModel;
  PlantGroups: TRipPlantGroups;
  PlantGroupIndex: Integer;
  APlantGroup: TRipPlantGroup;
begin
  LocalModel := Model.ParentModel as TPhastModel;
  FCompiler := LocalModel.GetCompiler(dsoTop, eaBlocks);

  PlantGroups := LocalModel.RipPlantGroups;
  for PlantGroupIndex := 0 to PlantGroups.Count - 1 do
  begin
    APlantGroup := PlantGroups[PlantGroupIndex];
    WriteDataSet3(APlantGroup);
    WriteDataSet4(APlantGroup);
    WriteDataSet5(APlantGroup);
  end;
end;

procedure TModflowRipWriter.WriteDataSets7and8(CellList: TRipCellList; StressPeriodIndex: integer);
var
  CellIndex: Integer;
  ARipCell: TRipCell;
begin
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    ARipCell := CellList[CellIndex];
    WriteDataSet7(ARipCell);
    WriteDataSet8(ARipCell, StressPeriodIndex);
  end;
end;

procedure TModflowRipWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
    if not Package.IsSelected then
    begin
      Exit
    end;
    if Model.ModelSelection <> msModflowFmp then
    begin
      Exit
    end;
    if Model.PackageGeneratedExternally('RIP') then
    begin
      Exit;
    end;

    Evaluate;

    NameOfFile := FileName(AFileName);
    FNameOfFile := NameOfFile;
    FInputFileName := FNameOfFile;
    WriteToNameFile(StrRip, Model.UnitNumbers.UnitNumber(StrRip),
      NameOfFile, foInput, Model);
    ClearTimeLists(Model);

    OpenFile(NameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingRIPPackage);

      frmProgressMM.AddMessage(StrWritingDataSet1);
      WriteDataSet1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet2);
      WriteDataSet2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSets3to5);
      WriteDataSets3to5;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSets6to8);
      WriteStressPeriods;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

    finally
      CloseFile;
    end;
end;

procedure TModflowRipWriter.WriteDataSet7(ARipCell: TRipCell);
var
  Layer: Integer;
  NPOLY: Integer;
  RipValueCell: TRip_Cell;
  Column: Integer;
  Row: Integer;
begin
  RipValueCell := ARipCell.CurrentStressPeriod.First;
  Layer := Model.DataSetLayerToModflowLayer(RipValueCell.Layer);
  Row := RipValueCell.Row + 1;
  Column := RipValueCell.Column + 1;
  NPOLY := ARipCell.CurrentStressPeriod.Count;
  WriteInteger(Layer);
  WriteInteger(Row);
  WriteInteger(Column);
  WriteInteger(NPOLY);
  WriteString(' # Data Set 7: Layer Row Column NPOLY');
  NewLine;
end;

procedure TModflowRipWriter.GetCellsForCurrentStessPeriod(CellList: TRipCellList;
  StressPeriodIndex: integer);
var
  ColIndex: Integer;
  ARipCell: TRipCell;
  StressPeriodCells: TRipStressPeriodPolygons;
  RowIndex: Integer;
  Grid: TCustomModelGrid;
begin
  Grid := Model.Grid;
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Grid.ColumnCount - 1 do
    begin
      ARipCell := Cells[ColIndex, RowIndex];
      if ARipCell <> nil then
      begin
        StressPeriodCells := ARipCell.CurrentStressPeriod;
        while (StressPeriodCells <> nil)
          and (StressPeriodCells.StressPeriod < StressPeriodIndex) do
        begin
          StressPeriodCells := ARipCell.NextStressPeriod;
        end;
        if (StressPeriodCells <> nil)
          and (StressPeriodCells.StressPeriod = StressPeriodIndex) then
        begin
          CellList.Add(ARipCell);
        end;
      end;
    end;
  end;
end;

procedure TModflowRipWriter.WriteDataSet6(CellList: TRipCellList;
  StressPeriodIndex: integer);
var
  ITMP: Integer;
  StressPeriodString: string;
begin
  StressPeriodString := Format(StrStressPeriodD, [StressPeriodIndex+1]);
  ITMP := CellList.Count;
  WriteInteger(ITMP);
  WriteString(Format(StrDataSet6SI, [StressPeriodString]));
  NewLine;
end;

procedure TModflowRipWriter.WriteDataSet8(ARipCell: TRipCell; StressPeriodIndex: integer);
const
  Epsilon = 1E-8;
  MaxCoverage = 1 + Epsilon;
var
  RipCellList: TRipStressPeriodPolygons;
  CellIndex: Integer;
  RipValueCell: TRip_Cell;
  PlantGroupIndex: Integer;
  HSURF: Double;
  fCov: Double;
  TotalCoverage: Double;
  Layer: Integer;
begin
  TotalCoverage := 0;
  RipCellList := ARipCell.CurrentStressPeriod;
  for CellIndex := 0 to RipCellList.Count - 1 do
  begin
    RipValueCell := RipCellList[CellIndex];
    HSURF := RipValueCell.LandElevation;
    WriteFloat(HSURF);
    for PlantGroupIndex := 0 to FPlantGroups.Count - 1 do
    begin
      fCov := RipValueCell.Coverage[PlantGroupIndex];
      WriteFloat(fCov);
      TotalCoverage := TotalCoverage + fCov;
    end;
    WriteString(' # Data Set 8: HSURF fCov(1) ... fCov(MAXTS)');
    NewLine;
  end;
  if TotalCoverage > MaxCoverage then
  begin
    RipValueCell := RipCellList.First;
    Layer := Model.DataSetLayerToModflowLayer(RipValueCell.Layer);
    frmErrorsAndWarnings.AddError(Model, StrRIPCoverageExceeds,
      Format(StrStressPeriod0d,
      [StressPeriodIndex+1, Layer, RipValueCell.Row+1, RipValueCell.Column+1]));
  end;
end;

procedure TModflowRipWriter.WriteStressPeriod(StressPeriodIndex: Integer);
var
  CellList: TRipCellList;
begin
  CellList := TRipCellList.Create;
  try
    GetCellsForCurrentStessPeriod(CellList, StressPeriodIndex);
    WriteDataSet6(CellList, StressPeriodIndex);
    WriteDataSets7and8(CellList, StressPeriodIndex);
  finally
    CellList.Free;
  end;
end;

procedure TModflowRipWriter.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  CellIndex: Integer;
  ACell: TRipCell;
begin
  for CellIndex := 0 to FCellList.Count - 1 do
  begin
    ACell := FCellList[CellIndex];
    ACell.ResetCurrentStressPeriod;
  end;
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    WriteStressPeriod(StressPeriodIndex);
  end;
end;

procedure TModflowRipWriter.WriteDataSet4(APlantGroup: TRipPlantGroup);
var
  EtSegmentIndex: Integer;
  ASegment: TRipETSegment;
begin
  for EtSegmentIndex := 0 to APlantGroup.ETSegments.Count - 1 do
  begin
    ASegment := APlantGroup.ETSegments[EtSegmentIndex];
    WriteFloat(ASegment.ActiveRootDepth);
  end;
  WriteString(' # Data Set 4: fdh(1) fdh(2) ... fdh(NuSeg)');
  NewLine;
end;

procedure TModflowRipWriter.WriteDataSet5(APlantGroup: TRipPlantGroup);
var
  EtSegmentIndex: Integer;
  ASegment: TRipETSegment;
begin
  for EtSegmentIndex := 0 to APlantGroup.ETSegments.Count - 1 do
  begin
    ASegment := APlantGroup.ETSegments[EtSegmentIndex];
    WriteFloat(ASegment.FluxSegment);
  end;
  WriteString(' # Data Set 5: fdR(1) fdR(2) ... fdR(NuSeg)');
  NewLine;
end;

procedure TModflowRipWriter.WriteDataSet3(APlantGroup: TRipPlantGroup);
var
  Rmax: Double;
  RIPNM: string;
  Ard: Double;
  NuSeg: Integer;
  AFormula: string;
  Sxd: Double;
  Rsxd: Double;
begin
  RIPNM := Format('"%s"', [APlantGroup.Name]);

  try
    AFormula := APlantGroup.SaturatedExtinctionDepth;
    Sxd := FormulaToRealValue(AFormula);
    if AFormula <> APlantGroup.SaturatedExtinctionDepth then
    begin
      APlantGroup.SaturatedExtinctionDepth := AFormula;
    end;
  except
    on E: ErbwParserError do
    begin
      Sxd := 0;
      frmFormulaErrors.AddFormulaError(Format(StrPlantGroupS,
        [APlantGroup.Name]), StrSaturatedExtinction,
        APlantGroup.SaturatedExtinctionDepth, StrTheFormulaHasBeen);
      APlantGroup.SaturatedExtinctionDepth := '0';
    end;
  end;

  try
    AFormula := APlantGroup.ActiveRootDepth;
    Ard := FormulaToRealValue(AFormula);
    if AFormula <> APlantGroup.ActiveRootDepth then
    begin
      APlantGroup.ActiveRootDepth := AFormula;
    end;
  except
    on E: ErbwParserError do
    begin
      Ard := 0;
      frmFormulaErrors.AddFormulaError(Format(StrPlantGroupS,
        [APlantGroup.Name]), StrActiveRootDepthA,
        APlantGroup.ActiveRootDepth, StrTheFormulaHasBeen);
      APlantGroup.ActiveRootDepth := '0';
    end;
  end;

  try
    AFormula := APlantGroup.MaxET;
    Rmax := FormulaToRealValue(AFormula);
    if AFormula <> APlantGroup.MaxET then
    begin
      APlantGroup.MaxET := AFormula;
    end;
  except
    on E: ErbwParserError do
    begin
      Rmax := 0;
      frmFormulaErrors.AddFormulaError(Format(StrPlantGroupS,
        [APlantGroup.Name]), StrMaximumETRmax,
        APlantGroup.MaxET, StrTheFormulaHasBeen);
      APlantGroup.MaxET := '0';
    end;
  end;

  try
    AFormula := APlantGroup.SatExtinctionEvap;
    Rsxd := FormulaToRealValue(AFormula);
    if AFormula <> APlantGroup.SatExtinctionEvap then
    begin
      APlantGroup.SatExtinctionEvap := AFormula;
    end;
  except
    on E: ErbwParserError do
    begin
      Rsxd := 0;
      frmFormulaErrors.AddFormulaError(Format(StrPlantGroupS,
        [APlantGroup.Name]), StrSaturatedExtinctionEvap,
        APlantGroup.SatExtinctionEvap, StrTheFormulaHasBeen);
      APlantGroup.SatExtinctionEvap := '0';
    end;
  end;

  NuSeg := APlantGroup.ETSegments.Count;
  // data set 3
  WriteString(RIPNM);
  WriteFloat(Sxd);
  WriteFloat(Ard);
  WriteFloat(Rmax);
  WriteFloat(Rsxd);
  WriteInteger(NuSeg);
  WriteString(' # Data Set 3: RIPNM Sxd Ard Rmax Rsxd NuSeg');
  NewLine;
end;

{ TRipCell }

function TRipCell.AddStressPeriod(
  StressPeriod: Integer): TRipStressPeriodPolygons;
begin
  result := TRipStressPeriodPolygons.Create;
  result.StressPeriod := StressPeriod;
  FCurrentStressPeriod := FStressPeriods.Add(Result);
end;

constructor TRipCell.Create;
begin
  inherited;
  FStressPeriods := TObjectList<TRipStressPeriodPolygons>.Create;
  FCurrentStressPeriod := -1;
end;

destructor TRipCell.Destroy;
begin
  FStressPeriods.Free;
  inherited;
end;

function TRipCell.GetCurrentStressPeriod: TRipStressPeriodPolygons;
begin
  if (FStressPeriods.Count = 0) or (FCurrentStressPeriod >= FStressPeriods.Count) then
  begin
    result := nil;
  end
  else
  begin
    result := FStressPeriods[FCurrentStressPeriod];
  end;
end;

function TRipCell.GetNextStressPeriod: TRipStressPeriodPolygons;
begin
  Inc(FCurrentStressPeriod);
  result := CurrentStressPeriod;
end;

procedure TRipCell.ResetCurrentStressPeriod;
begin
  FCurrentStressPeriod := 0;
end;

end.
