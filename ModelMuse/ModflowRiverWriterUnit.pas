unit ModflowRiverWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowRivUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, GoPhastTypes,
  Modflow6ObsUnit, FluxObservationUnit;

type
  TModflowRIV_Writer = class(TFluxObsWriter)
  private
    NPRIV: integer;
    MXL: integer;
    FCells: array of array of TRiv_Cell;
    MXACTC: integer;
    FShouldWriteFile: Boolean;
    FAbbreviation: string;
    FPestParamUsed: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
    procedure InitializeCells;
    procedure WriteFileInternal;
  protected
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    class function ObservationExtension: string; override;
    function ObsNameWarningString: string; override;
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
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
    class function ObservationOutputExtension: string; override;
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit, Forms,
  DataSetUnit, FastGEO, ModflowMvrWriterUnit, ModflowMvrUnit;

resourcestring
  StrInTheFollowingRiv = 'In the following river cells, the stage is equal to or below t' +
  'he river bottom.';
  StrLayerDRowDC = 'Layer: %0:d, Row %1:d, Column %2:d. Amount: %3:g.';
  StrTheFollowingRiver = 'The following River observation names may be valid' +
  ' for MODFLOW but they are not valid for UCODE.';
  StrTheFollowingRiverPest = 'The following River observation names may be valid' +
  ' for MODFLOW but they are not valid for PEST.';
  StrWritingRIVPackage = 'Writing RIV Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrRiverStageIsBelow = 'River stage is below the bottom of the cell at the' +
  ' following locations.';
  StrRiverBottomIsBelo = 'River bottom is below the bottom of the cell at th' +
  'e following locations.';
  StrRiverStageIsBelowBottom = 'River stage is below the river bottom at the' +
  ' following locations.';
  StrLargeRiverStageGrDetailed = 'Large river stage gradient between %0:s an' +
  'd %1:s. Amount: %2:g';
  StrLargeRiverStageGr = 'Large river stage gradient';
  StrHighRiverConductan = 'High River conductance compared to the cell-to-ce' +
  'll conductance may cause numerical difficulties';
  StrZeroRiverConductan = 'Transmissivity is negative or zero in cell containing a River';
  StrWritingOptions = '  Writing Options';
  StrNoRiverCellsDefin = 'No River cells defined';
  StrBecauseNoRiverCel = 'Because no river cells have been defined, the Rive' +
  'r package will not be included in the model.';

{ TModflowRIV_Writer }

function TModflowRIV_Writer.CellType: TValueCellType;
begin
  result := TRiv_Cell;
end;

procedure TModflowRIV_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
const
  HighConductanceContrast = 1E6;
var
  Riv_Cell: TRiv_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
  OtherCell: TRiv_Cell;
  CellBottomElevation: Real;
  AqCond: Double;
  Ratio: Extended;
  Delta: double;
  procedure CheckGradient;
  var
    DeltaRivElevation: double;
//    OtherCellBottomElevation: Real;
//    DeltaCellElevation: double;
    Cell1: string;
    Cell2: string;
    WarningMessage: string;
    Point1: TPoint2D;
    Point2: TPoint2D;
    Gradient: Extended;
  begin
    if OtherCell <> nil then
    begin
      DeltaRivElevation := Abs(Riv_Cell.RiverStage - OtherCell.RiverStage);
      Point1 := Model.Grid.TwoDElementCenter(Riv_Cell.Column, Riv_Cell.Row);
      Point2 := Model.Grid.TwoDElementCenter(OtherCell.Column, OtherCell.Row);
      Gradient := DeltaRivElevation/Distance(Point1, Point2);
//      OtherCellBottomElevation := Model.Grid.CellElevation[
//        OtherCell.Column, OtherCell.Row, OtherCell.Layer+1];
//      DeltaCellElevation := Abs(OtherCellBottomElevation - CellBottomElevation);
      if Gradient > HighGradient then
      begin
        ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
        Cell1 := Format(StrLayerRowColObject, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name]);
        ScreenObject := OtherCell.ScreenObject as TScreenObject;
        Cell2 := Format(StrLayerRowColObject, [
          OtherCell.Layer+1, OtherCell.Row+1, OtherCell.Column+1, ScreenObject.Name]);
        WarningMessage := Format(StrLargeRiverStageGrDetailed,
          [Cell1, Cell2, Gradient]);
        frmErrorsAndWarnings.AddWarning(Model, StrLargeRiverStageGr,
          WarningMessage, ScreenObject);
      end;
    end;
  end;
begin
  inherited;
  Riv_Cell := ValueCell as TRiv_Cell;
  if Length(FCells) = 0 then
  begin
    InitializeCells;
  end;
  FCells[Riv_Cell.Row, Riv_Cell.Column] := Riv_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  CellBottomElevation := Model.DiscretiztionElevation[
    Riv_Cell.Column, Riv_Cell.Row, Riv_Cell.Layer+1];
  if ActiveDataArray.BooleanData[Riv_Cell.Layer, Riv_Cell.Row, Riv_Cell.Column] then
  begin
    if (Riv_Cell.RiverStage < CellBottomElevation) then
    begin
      Delta := CellBottomElevation-Riv_Cell.RiverStage;
      ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
      if Model.ModelSelection = msModflowNWT then
      begin
        frmErrorsAndWarnings.AddError(Model, StrRiverStageIsBelow,
          Format(StrLayerRowColObjectAmount, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrRiverStageIsBelow,
          Format(StrLayerRowColObjectAmount, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end;
    end;

    if (Riv_Cell.RiverBottom < CellBottomElevation) then
    begin
      Delta := CellBottomElevation - Riv_Cell.RiverBottom;
      ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
      if Model.ModelSelection = msModflowNWT then
      begin
        frmErrorsAndWarnings.AddError(Model, StrRiverBottomIsBelo,
          Format(StrLayerRowColObjectAmount, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrRiverBottomIsBelo,
          Format(StrLayerRowColObjectAmount, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Delta]),
          ScreenObject);
      end;
    end;

    if Riv_Cell.RiverStage <= Riv_Cell.RiverBottom then
    begin
      Delta := Riv_Cell.RiverBottom - Riv_Cell.RiverStage;
      ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddError(Model, StrRiverStageIsBelowBottom,
        Format(StrLayerRowColObjectAmount, [
        Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Delta]),
        ScreenObject);
    end;

    AqCond := AquiferConductance(Riv_Cell.Layer, Riv_Cell.Row, Riv_Cell.Column);
    if AqCond > 0 then
    begin
      Ratio := Riv_Cell.Conductance/AqCond;
      if Ratio > HighConductanceContrast then
      begin
        ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddWarning(Model,StrHighRiverConductan,
          Format(StrLayerRowColObjectAmount, [
          Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name, Ratio]),
          ScreenObject);
      end;
    end
    else
    begin
      ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddWarning(Model,StrZeroRiverConductan,
        Format(StrLayerRowColObject, [
        Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name]),
        ScreenObject);
    end;
  end;
  if not Model.DisvUsed then
  begin
    if Riv_Cell.Row > 0 then
    begin
      OtherCell := FCells[Riv_Cell.Row-1,Riv_Cell.Column];
      CheckGradient;
    end;
    if Riv_Cell.Column > 0 then
    begin
      OtherCell := FCells[Riv_Cell.Row,Riv_Cell.Column-1];
      CheckGradient;
    end;
    if Riv_Cell.Row < Model.Grid.RowCount-1 then
    begin
      OtherCell := FCells[Riv_Cell.Row+1,Riv_Cell.Column];
      CheckGradient;
    end;
    if Riv_Cell.Column < Model.Grid.ColumnCount-1 then
    begin
      OtherCell := FCells[Riv_Cell.Row,Riv_Cell.Column+1];
      CheckGradient;
    end;
  end;
end;

procedure TModflowRIV_Writer.DoBeforeWriteCells;
begin
  inherited;
  InitializeCells;
end;

class function TModflowRIV_Writer.Extension: string;
begin
  result := '.riv';
end;

function TModflowRIV_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowRivBoundary;
end;

class function TModflowRIV_Writer.ObservationExtension: string;
begin
  result := '.ob_rvob';
end;

class function TModflowRIV_Writer.ObservationOutputExtension: string;
begin
  result := '.rvob_out';
end;

function TModflowRIV_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.RvobPackage;
end;

function TModflowRIV_Writer.ObsFactors: TFluxObservationGroups;
begin
  result := Model.RiverObservations;
end;

function TModflowRIV_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowRIV_Writer.ObsNameWarningString: string;
begin
  if Model.PestUsed then
  begin
    result := StrTheFollowingRiverPest;
  end
  else
  begin
    result := StrTheFollowingRiver;
  end;
end;

function TModflowRIV_Writer.ObsType: string;
begin
  result := 'riv';
end;

function TModflowRIV_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.RivPackage;
end;

procedure TModflowRIV_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Riv_Cell: TRiv_Cell;
  LocalLayer: integer;
  ScreenObject: TScreenObject;
  Delta: double;
  MvrKey: TMvrRegisterKey;
  ParameterName: string;
  MultiplierValue: double;
  DataArray: TDataArray;
begin
    { TODO -cPEST : Add PEST support for PEST here }
    // handle pest parameter
    // handle multiply or add
  Inc(FBoundaryIndex);

  Riv_Cell := Cell as TRiv_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Riv_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Riv_Cell.Row+1);
  end;
  WriteInteger(Riv_Cell.Column+1);

  if (Riv_Cell.ConductancePest <> '')
    or (Riv_Cell.RiverStagePest <> '')
    or (Riv_Cell.RiverBottomPest <> '')
    or (Riv_Cell.ConductancePestSeries <> '')
    or (Riv_Cell.RiverStagePestSeries <> '')
    or (Riv_Cell.RiverBottomPestSeries <> '') then
  begin
    FPestParamUsed := True;
  end;

  if Model.PestUsed and WritingTemplate and
    ((Riv_Cell.RiverStagePest <> '') or (Riv_Cell.RiverStagePestSeries <> '')) then
  begin
    WritePestTemplateFormula(Riv_Cell.RiverStage, Riv_Cell.RiverStagePest,
      Riv_Cell.RiverStagePestSeries, Riv_Cell.RiverStagePestSeriesMethod, Riv_Cell);
  end
  else
  begin
    WriteFloat(Riv_Cell.RiverStage);
    if Riv_Cell.RiverStagePest <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.RiverStagePest);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
    if Riv_Cell.RiverStagePestSeries <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.RiverStagePestSeries);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
  end;

//  WriteFloat(Riv_Cell.RiverStage);

  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
    and WritingTemplate
    and ( Riv_Cell.ConductanceParameterName <> '') then
  begin
    // PEST parameters are not allowed to be combined
    // with MF-2005 style parameters.
    ParameterName := Riv_Cell.ConductanceParameterName;
    if Riv_Cell.ConductanceParameterValue = 0 then
    begin
      MultiplierValue := 0.0;
    end
    else
    begin
      MultiplierValue := Riv_Cell.Conductance / Riv_Cell.ConductanceParameterValue;
    end;
    WriteModflowParamFormula(ParameterName, Riv_Cell.ConductancePest,
      MultiplierValue, Riv_Cell);
  end
  else if Model.PestUsed and WritingTemplate
    and ((Riv_Cell.ConductancePest <> '') or (Riv_Cell.ConductancePestSeries <> '')) then
  begin
    WritePestTemplateFormula(Riv_Cell.Conductance, Riv_Cell.ConductancePest,
      Riv_Cell.ConductancePestSeries, Riv_Cell.ConductancePestSeriesMethod,
      Riv_Cell);
  end
  else
  begin
    WriteFloat(Riv_Cell.Conductance);
    if Riv_Cell.ConductancePest <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.ConductancePest);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
    if Riv_Cell.ConductancePestSeries <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.ConductancePestSeries);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
  end;


//  else
//  begin
//    WriteFloat(Riv_Cell.Conductance);
//  end;
//  if Riv_Cell.TimeSeriesName = '' then
//  begin
//    WriteFloat(Riv_Cell.Conductance);
//  end
//  else
//  begin
//    WriteString(' ');
//    WriteString(Riv_Cell.TimeSeriesName);
//    WriteString(' ');
//  end;

//  WriteFloat(Riv_Cell.Conductance);

  if Model.PestUsed and WritingTemplate and
    ((Riv_Cell.RiverBottomPest <> '') or (Riv_Cell.RiverBottomPestSeries <> '')) then
  begin
    WritePestTemplateFormula(Riv_Cell.RiverBottom, Riv_Cell.RiverBottomPest,
      Riv_Cell.RiverBottomPestSeries, Riv_Cell.RiverBottomPestSeriesMethod, Riv_Cell);
  end
  else
  begin
    WriteFloat(Riv_Cell.RiverBottom);
    if Riv_Cell.RiverBottomPest <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.RiverBottomPest);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
    if Riv_Cell.RiverBottomPestSeries <> '' then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Riv_Cell.RiverBottomPestSeries);
      if DataArray <> nil then
      begin
        AddUsedPestDataArray(DataArray);
      end;
    end;
  end;

//  WriteFloat(Riv_Cell.RiverBottom);



  WriteIface(Riv_Cell.IFace);
  WriteBoundName(Riv_Cell);
  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d Stage '
      + VariableIdentifiers);
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Stage '
      + VariableIdentifiers);
  end;
//  WriteString(' ' + Riv_Cell.ConductanceAnnotation);
  NewLine;
  if Riv_Cell.RiverStage <= Riv_Cell.RiverBottom then
  begin
    Delta := Riv_Cell.RiverBottom - Riv_Cell.RiverStage;
    ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
    if Model.ModelSelection = msModflowNWT then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInTheFollowingRiv,
        Format(StrLayerRowColObjectAmount, [Riv_Cell.Layer+1, Riv_Cell.Row+1,
        Riv_Cell.Column+1, ScreenObject.Name, Delta]), ScreenObject);
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrInTheFollowingRiv,
        Format(StrLayerRowColObjectAmount, [Riv_Cell.Layer+1, Riv_Cell.Row+1,
        Riv_Cell.Column+1, ScreenObject.Name, Delta]), ScreenObject);
    end;
  end;

  if Riv_Cell.MvrUsed and (MvrWriter <> nil) and not WritingTemplate then
  begin
    MvrKey.StressPeriod := FStressPeriod;
    MvrKey.Index := FBoundaryIndex;
    MvrKey.SourceKey.MvrIndex := Riv_Cell.MvrIndex;
    MvrKey.SourceKey.ScreenObject := Riv_Cell.ScreenObject;
    TModflowMvrWriter(MvrWriter).AddMvrSource(MvrKey);
  end;
end;

procedure TModflowRIV_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPRIV, MXL);
  if NPRIV > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPRIV);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPRIV MXL');
    NewLine;
  end;
end;

procedure TModflowRIV_Writer.WriteDataSet2;
var
  Option: String;
  IRIVCB: Integer;
begin
  CountCells(MXACTC);
  GetFlowUnitNumber(IRIVCB);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteInteger(IRIVCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTC IRIVCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

function TModflowRIV_Writer.ParameterType: TParameterType;
begin
  result := ptRIV;
end;

procedure TModflowRIV_Writer.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact Rbot IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot, umAssign, nil, nil);
end;

procedure TModflowRIV_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Cond Rbot IFACE';
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

procedure TModflowRIV_Writer.WriteFile(const AFileName: string);
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
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRiverStageIsBelow);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRiverStageIsBelow);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRiverBottomIsBelo);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRiverBottomIsBelo);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeRiverStageGr);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrHighRiverConductan);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoRiverCellsDefin);


    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheFollowingRiv);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheFollowingRiv);
    if not Package.IsSelected then
    begin
      Exit
    end;
    if Model.ModelSelection = msModflow2015 then
    begin
      FAbbreviation := 'RIV6';
    end
    else
    begin
      FAbbreviation := StrRIV;
    end;
    FShouldWriteFile := not Model.PackageGeneratedExternally(FAbbreviation);
    ShouldWriteObservationFile := ObservationPackage.IsSelected
      and not Model.PackageGeneratedExternally(StrRVOB);

    if not FShouldWriteFile and not ShouldWriteObservationFile then
    begin
      Exit;
    end;
    FNameOfFile := FileName(AFileName);
    FInputFileName := FNameOfFile;
//    FFileName := NameOfFile;
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

    WriteFileInternal;
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
end;

procedure TModflowRIV_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQRV NQCRV NQTRV IURVOBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTRV';
  DataSet3Comment = ' # Data Set 3: NQOBRV NQCLRV';
  PackageAbbreviation = StrRVOB;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    WriteFluxObsFile(AFileName, StrIURVOBSV, PackageAbbreviation,
      DataSet1Comment, DataSet2Comment, DataSet3Comment,
      Model.RiverObservations, Purpose);
  end;
end;

procedure TModflowRIV_Writer.WriteListOptions(InputFileName: string);
//var
//  RivPackage: TRivPackage;
begin
  inherited;
  WriteMf6ParamListOption;
//  RivPackage := Package as TRivPackage;
//  if RivPackage.NewtonFormulation = nfOn then
//  begin
//    WriteString('    NEWTON');
//    NewLine;
//  end;
end;

procedure TModflowRIV_Writer.WriteMoverOption;
begin
  if (MvrWriter <> nil) then
  begin
    if spcRiv in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;
end;

procedure TModflowRIV_Writer.InitializeCells;
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

procedure TModflowRIV_Writer.WriteFileInternal;
begin
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingRIVPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if Model.ModelSelection = msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingOptions);
      WriteOptionsMF6(NameOfFile);
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
        frmErrorsAndWarnings.AddWarning(Model, StrNoRiverCellsDefin, StrBecauseNoRiverCel);
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

      if MXACTC = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoRiverCellsDefin, StrBecauseNoRiverCel);
        Exit;
      end;
    end;

    if FShouldWriteFile and not WritingTemplate then
    begin
      WriteToNameFile(FAbbreviation, Model.UnitNumbers.UnitNumber(StrRIV),
        NameOfFile, foInput, Model);
    end;

//      if Model.ModelSelection <> msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingDataSets3and4);
      WriteDataSets3And4;
      Application.ProcessMessages;
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

function TModflowRIV_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := ((AScreenObject.Modflow6Obs <> nil)
    and (ogRiv in AScreenObject.Modflow6Obs.General))
    or IsFlowObs(AScreenObject);
end;

function TModflowRIV_Writer.IsMf6ToMvrObservation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
//    and AScreenObject.Modflow6Obs.Used
    and (ogMvr in AScreenObject.Modflow6Obs.General);
end;

class function TModflowRIV_Writer.Mf6ObType: TObGeneral;
begin
  result := ogRiv
end;

procedure TModflowRIV_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TRiv_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  InitializeCells;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TRiv_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'RIV');
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
    WriteFloat(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Stage Condfact Rbot IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
