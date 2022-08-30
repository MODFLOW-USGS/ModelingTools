unit ModflowDRN_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowDrnUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes, Modflow6ObsUnit;

type
  TModflowDRN_Writer = class(TFluxObsWriter)
  private
    NPDRN: integer;
    MXL: integer;
    FCells: array of array of TDrn_Cell;
    MXACTD: integer;
    FShouldWriteFile: Boolean;
    FAbbreviation: string;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
    procedure InitializeCells;
    procedure WriteFileInternal;
  protected
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    class function ObservationExtension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    function ObsNameWarningString: string; override;
    procedure Evaluate; override;
    procedure CheckCell(ValueCell: TValueCell; const PackageName: string); override;
    procedure DoBeforeWriteCells; override;
//    function ObsTypeMF6: string; override;
    procedure WriteListOptions(InputFileName: string); override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    procedure WriteMoverOption; override;
    Class function Mf6ObType: TObGeneral; override;
    function ObsFactors: TFluxObservationGroups; override;
  public
    procedure WriteFile(const AFileName: string);
    // Write flow observation package input for MF2005 etc. but not MF 6
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
    class function ObservationOutputExtension: string; override;
    class function Extension: string; override;
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit,
  RbwParser, DataSetUnit, Forms, FastGEO, ModflowMvrWriterUnit, ModflowMvrUnit,
  ModflowParameterUnit, PestPropertiesUnit;

resourcestring
  StrTheFollowingDrain = 'The following Drain observation names may be valid' +
  ' for MODFLOW but they are not valid for UCODE.';
  StrTheFollowingDrainPest = 'The following Drain observation names may be valid' +
  ' for MODFLOW but they are not valid for PEST.';
  StrWritingDRNPackage = 'Writing DRN Package input.';
  StrDrainElevationIsB = 'Drain elevation is below the bottom of the cell at' +
  ' the following locations.';
  StrLargeDrainElevatioDetailed = 'Large drain elevation gradient between %0:s and %' +
  '1:s. Amount: %2:g';
  StrLargeDrainElevatio = 'Large drain elevation gradient';
  StrHighDrainConductan = 'High Drain conductance compared to the cell-to-cell '
  + 'conductance may cause numerical difficulties';
  StrNegativeOrZeroDrainConductance = 'Transmissivity is negative or zero in cell containing a drain.';
  StrNoDrainsDefined = 'No drains defined';
  StrTheDrainPackageIs = 'The Drain package is not included in the model bec' +
  'ause no drains are defined.';
//  StrLayerRowColumn = 'Layer, Row Column = %0:d, %1:d, %2:d';

{ TModflowDRN_Writer }

function TModflowDRN_Writer.CellType: TValueCellType;
begin
  result := TDrn_Cell;
end;

procedure TModflowDRN_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
const
  HighConductanceContrast = 1E6;
var
  Drn_Cell: TDrn_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
  OtherCell: TDrn_Cell;
  CellBottomElevation: Real;
  AqCond: Double;
  Ratio: Extended;
  Delta: double;
  procedure CheckGradient;
  var
    DeltaDrnElevation: double;
    WarningMessage: string;
//    OtherCellBottomElevation: Real;
//    DeltaCellElevation: Real;
    Cell1: string;
    Cell2: string;
    Point1: TPoint2D;
    Point2: TPoint2D;
    Gradient: Extended;
  begin
    if (OtherCell <> nil)
      and (Drn_Cell.ElevTimeSeriesName <> '')
      and (OtherCell.ElevTimeSeriesName <> '') then
    begin
      DeltaDrnElevation := Abs(Drn_Cell.Elevation - OtherCell.Elevation);
      Point1 := Model.Grid.TwoDElementCenter(Drn_Cell.Column, Drn_Cell.Row);
      Point2 := Model.Grid.TwoDElementCenter(OtherCell.Column, OtherCell.Row);
      Gradient := DeltaDrnElevation/Distance(Point1, Point2);
      if Gradient > HighGradient  then
      begin
        ScreenObject := Drn_Cell.ScreenObject as TScreenObject;
        Cell1 := Format(StrLayerRowColObject, [
          Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name]);
        ScreenObject := OtherCell.ScreenObject as TScreenObject;
        Cell2 := Format(StrLayerRowColObject, [
          OtherCell.Layer+1, OtherCell.Row+1, OtherCell.Column+1, ScreenObject.Name]);
        WarningMessage := Format(StrLargeDrainElevatioDetailed,
          [Cell1, Cell2, Gradient]);
        frmErrorsAndWarnings.AddWarning(Model, StrLargeDrainElevatio,
          WarningMessage, ScreenObject);
      end;
    end;
  end;
begin
  inherited;
  Drn_Cell := ValueCell as TDrn_Cell;
  if Length(FCells) = 0 then
  begin
    InitializeCells;
  end;
  FCells[Drn_Cell.Row, Drn_Cell.Column] := Drn_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  CellBottomElevation := Model.DiscretiztionElevation[
    Drn_Cell.Column, Drn_Cell.Row, Drn_Cell.Layer+1];
  if ActiveDataArray.BooleanData[Drn_Cell.Layer, Drn_Cell.Row, Drn_Cell.Column]
    then
  begin
    if (Drn_Cell.Elevation < CellBottomElevation)
      and (Drn_Cell.ElevTimeSeriesName <> '')
     then
    begin
      Delta := CellBottomElevation - Drn_Cell.Elevation;
      ScreenObject := Drn_Cell.ScreenObject as TScreenObject;
      if Model.ModelSelection = msModflowNWT then
      begin
        frmErrorsAndWarnings.AddError(Model, StrDrainElevationIsB,
          Format(StrLayerRowColObjectAmount, [
          Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrDrainElevationIsB,
          Format(StrLayerRowColObjectAmount, [
          Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end;
    end;
    AqCond := AquiferConductance(Drn_Cell.Layer, Drn_Cell.Row, Drn_Cell.Column);
    if AqCond > 0 then
    begin
      if Drn_Cell.ConductanceTimeSeriesName = '' then
      begin
        Ratio := Drn_Cell.Conductance/AqCond;
        if Ratio > HighConductanceContrast then
        begin
          ScreenObject := Drn_Cell.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddWarning(Model,StrHighDrainConductan,
            Format(StrLayerRowColObjectAmount, [
            Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name, Ratio]),
            ScreenObject);
        end;
      end;
    end
    else
    begin
      ScreenObject := Drn_Cell.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model,StrNegativeOrZeroDrainConductance,
        Format(StrLayerRowColObject, [
        Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name]),
        ScreenObject);
    end;
  end;
  if not Model.DisvUsed then
  begin
    if Drn_Cell.Row > 0 then
    begin
      OtherCell := FCells[Drn_Cell.Row-1,Drn_Cell.Column];
      CheckGradient;
    end;
    if Drn_Cell.Column > 0 then
    begin
      OtherCell := FCells[Drn_Cell.Row,Drn_Cell.Column-1];
      CheckGradient;
    end;
    if Drn_Cell.Row < Model.Grid.RowCount-1 then
    begin
      OtherCell := FCells[Drn_Cell.Row+1,Drn_Cell.Column];
      CheckGradient;
    end;
    if Drn_Cell.Column < Model.Grid.ColumnCount-1 then
    begin
      OtherCell := FCells[Drn_Cell.Row,Drn_Cell.Column+1];
      CheckGradient;
    end;
  end;
end;

procedure TModflowDRN_Writer.DoBeforeWriteCells;
begin
  inherited;
  InitializeCells;
end;

procedure TModflowDRN_Writer.Evaluate;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoDrainsDefined);
  inherited Evaluate;
end;

class function TModflowDRN_Writer.Extension: string;
begin
  result := '.drn';
end;

function TModflowDRN_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowDrnBoundary;
end;

class function TModflowDRN_Writer.ObservationExtension: string;
begin
  result := '.ob_drob';
end;

class function TModflowDRN_Writer.ObservationOutputExtension: string;
begin
  result := '.drob_out';
end;

function TModflowDRN_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrobPackage;
end;

function TModflowDRN_Writer.ObsFactors: TFluxObservationGroups;
begin
  result := Model.DrainObservations;
end;

function TModflowDRN_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowDRN_Writer.ObsNameWarningString: string;
begin
  if Model.PestStatus in [psObservations, psActive] then
  begin
    result := StrTheFollowingDrainPest;
  end
  else
  begin
    result := StrTheFollowingDrain;
  end;
end;

function TModflowDRN_Writer.ObsType: string;
begin
  result := 'drn';
end;

function TModflowDRN_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrnPackage;
end;

procedure TModflowDRN_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Drn_Cell: TDrn_Cell;
  LocalLayer: integer;
  MvrKey: TMvrRegisterKey;
  ParameterName: string;
  MultiplierValue: double;
begin
    { Add PEST support for PEST here }
    // handle pest data
    // handle multiply or add
  Inc(FBoundaryIndex);

  Drn_Cell := Cell as TDrn_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Drn_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Drn_Cell.Row+1);
  end;
  WriteInteger(Drn_Cell.Column+1);

  WriteValueOrFormula(Drn_Cell, DrnElevationPosition);

  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
    and WritingTemplate
    and (Drn_Cell.ConductanceParameterName <> '') then
  begin
    // PEST parameters are not allowed to be combined
    // with MF-2005 style parameters.
    Assert(Drn_Cell.ConductancePest = '');
    ParameterName := Drn_Cell.ConductanceParameterName;
    if Drn_Cell.ConductanceParameterValue = 0 then
    begin
      MultiplierValue := 0.0;
    end
    else
    begin
      MultiplierValue := Drn_Cell.Conductance
        / Drn_Cell.ConductanceParameterValue;
    end;
    WriteModflowParamFormula(ParameterName, Drn_Cell.ConductancePest,
      MultiplierValue, Drn_Cell);
  end
  else
  begin
    WriteValueOrFormula(Drn_Cell, DrnConductancePosition);
  end;

  WriteIface(Drn_Cell.IFace);
  WriteBoundName(Drn_Cell);
  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d Elevation '
      + VariableIdentifiers);
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Elevation '
      + VariableIdentifiers);
  end;

  NewLine;

  if Drn_Cell.MvrUsed and (MvrWriter <> nil) and not WritingTemplate then
  begin
    MvrKey.StressPeriod := FStressPeriod;
    MvrKey.Index := FBoundaryIndex;
    MvrKey.SourceKey.MvrIndex := Drn_Cell.MvrIndex;
    MvrKey.SourceKey.ScreenObject := Drn_Cell.ScreenObject;
    TModflowMvrWriter(MvrWriter).AddMvrSource(MvrKey);
  end;
end;

procedure TModflowDRN_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPDRN, MXL);
  if NPDRN > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPDRN);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPDRN MXL');
    NewLine;
  end;
end;

procedure TModflowDRN_Writer.WriteDataSet2;
var
  Option: String;
  IDRNCB: Integer;
begin
  CountCells(MXACTD);
  GetFlowUnitNumber(IDRNCB);
  GetOption(Option);

  WriteInteger(MXACTD);
  WriteInteger(IDRNCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTD IDRNCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

function TModflowDRN_Writer.ParameterType: TParameterType;
begin
  result := ptDRN;
end;

procedure TModflowDRN_Writer.WriteDataSets3And4;
const
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign, nil, nil);
end;

procedure TModflowDRN_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Cond IFACE';
var
  VI: string;
begin
  VI := VariableIdentifiers;
  if Model.modelSelection = msModflow2015 then
  begin
    VI := VI + ' boundname';
  end;
  WriteStressPeriods(VI, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowDRN_Writer.WriteFile(const AFileName: string);
var
//  NameOfFile: string;
  ShouldWriteObservationFile: Boolean;
begin
  FPestParamUsed := False;
  if MvrWriter <> nil then
  begin
    Assert(MvrWriter is TModflowMvrWriter);
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDrainElevationIsB);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDrainElevationIsB);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeDrainElevatio);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighDrainConductan);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNegativeOrZeroDrainConductance);
    if not Package.IsSelected then
    begin
      Exit
    end;
    if Model.ModelSelection = msModflow2015 then
    begin
      FAbbreviation := 'DRN6';
    end
    else
    begin
      FAbbreviation := StrDRN;
    end;
    FShouldWriteFile := not Model.PackageGeneratedExternally(FAbbreviation);
    ShouldWriteObservationFile := ObservationPackage.IsSelected
      and not Model.PackageGeneratedExternally(StrDROB);

    if not FShouldWriteFile and not ShouldWriteObservationFile then
    begin
      Exit;
    end;

    FNameOfFile := FileName(AFileName);
    FInputFileName := FNameOfFile;
//    FNameOfFile := NameOfFile;

    if FShouldWriteFile or ShouldWriteObservationFile then
    begin
      Evaluate;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ClearTimeLists(Model);
    end;
    if not FShouldWriteFile then
    begin
      Exit;
    end;
    FNameOfFile := FileName(AFileName);

    WriteFileInternal;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

  if Model.ModelSelection = msModflow2015 then
  begin
    WriteModflow6FlowObs(NameOfFile, FEvaluationType);
  end;

  if  Model.PestUsed and (FPestParamUsed
    or ((Model.ModelSelection = msModflow2015) and (FParamValues.Count > 0))) then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;

  WritingTemplate := False;
end;

procedure TModflowDRN_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDRNPackage);

    WriteTemplateHeader;

    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if Model.ModelSelection = msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingOptions);
      WriteOptionsMF6(FNameOfFile);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDimensions);
      WriteDimensionsMF6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if MAXBOUND = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoDrainsDefined,
          StrTheDrainPackageIs);
        Exit;
      end;
    end
    else
    begin
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

      if MXACTD = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoDrainsDefined,
          StrTheDrainPackageIs);
        Exit;
      end;
    end;

    if FShouldWriteFile and not WritingTemplate then
    begin
      WriteToNameFile(FAbbreviation, Model.UnitNumbers.UnitNumber(StrDRN),
        NameOfFile, foInput, Model, False, 'DRN-1');
    end;

//      if Model.ModelSelection <> msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingDataSets3and4);
      WriteDataSets3And4;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets5to7);
    WriteDataSets5To7;
  finally
    CloseFile;
  end;
end;

procedure TModflowDRN_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQDR NQCDR NQTDR IUDROBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTDR';
  DataSet3Comment = ' # Data Set 3: NQOBDR NQCLDR';
  PackageAbbreviation = StrDROB;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    WriteFluxObsFile(AFileName, StrIUDROBSV, PackageAbbreviation,
      DataSet1Comment, DataSet2Comment, DataSet3Comment,
      Model.DrainObservations, Purpose);
//  end
//  else
//  begin
//    WriteFluxObsFileMF6(AFileName, StrIUDROBSV, PackageAbbreviation,
//      DataSet1Comment, DataSet2Comment, DataSet3Comment,
//      Model.DrainObservations, Purpose);
  end;
end;

procedure TModflowDRN_Writer.WriteListOptions(InputFileName: string);
//var
//  DrnPackage: TDrnPackage;
begin
  inherited;
  WriteMf6ParamListOption;
//  DrnPackage := Package as TDrnPackage;
//  if DrnPackage.NewtonFormulation = nfOn then
//  begin
//    WriteString('    NEWTON');
//    NewLine;
//  end;
end;

procedure TModflowDRN_Writer.WriteMoverOption;
begin
  if (MvrWriter <> nil) then
  begin
    if spcDrn in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;
end;

procedure TModflowDRN_Writer.InitializeCells;
var
  RowIndex: Integer;
  ColIndex: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  if Model.Grid <> nil then
  begin
    RowCount := Model.Grid.RowCount;
    ColumnCount := Model.Grid.ColumnCount;
  end
  else
  begin
    RowCount := Model.DisvGrid.RowCount;
    ColumnCount := Model.DisvGrid.ColumnCount;
  end;
  SetLength(FCells, RowCount, ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      FCells[RowIndex, ColIndex] := nil;
    end;
  end;
end;

function TModflowDRN_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := ((AScreenObject.Modflow6Obs <> nil)
    and (ogDrain in AScreenObject.Modflow6Obs.General))
    or IsFlowObs(AScreenObject);
end;

function TModflowDRN_Writer.IsMf6ToMvrObservation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
//    and AScreenObject.Modflow6Obs.Used
    and (ogMvr in AScreenObject.Modflow6Obs.General);
end;

class function TModflowDRN_Writer.Mf6ObType: TObGeneral;
begin
   result := ogDrain;
end;

procedure TModflowDRN_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TDrn_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  InitializeCells;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TDrn_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'DRN');
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Condfact is set equal to zero, the
  // dummy boundaries have no effect.
  for CellIndex := CellList.Count to NLST - 1 do
  begin
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(1);
    WriteFloat(0);
    WriteFloat(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Stage Condfact IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
