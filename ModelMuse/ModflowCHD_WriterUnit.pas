unit ModflowCHD_WriterUnit;

interface

uses SysUtils, Classes, RbwParser, CustomModflowWriterUnit,
  PhastModelUnit, ModflowConstantHeadBoundaryUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, ModflowPackageSelectionUnit, ModflowCellUnit,
  OrderedCollectionUnit, FluxObservationUnit, GoPhastTypes, Modflow6ObsUnit,
  ScreenObjectInterfaceUnit;

type
  TModflowCHD_Writer = class(TFluxObsWriter)
  private
    NPCHD: integer;
    MXL: integer;
    FShouldWriteFile: Boolean;
    FAbbreviation: string;
    FChdBoundary: TChdPackage;
    FChdCells: array of array of array of TCHD_Cell;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
    procedure WriteFileInternal;
  protected
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    class function ObservationExtension: string; override;
    procedure WriteObservationCells(Variables, DataSets: TList;
      var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
      ScreenObject: TScreenObject; ObsFactor: TObservationFactor); override;
    function ObsNameWarningString: string; override;
    procedure Evaluate; override;
    procedure CheckCell(ValueCell: TValueCell;
      const PackageName: string); override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    procedure WriteListOptions(InputFileName: string); override;
    Class function Mf6ObType: TObGeneral; override;
    function ObsFactors: TFluxObservationGroups; override;
    procedure DoBeforeWriteCells; override;
    procedure WriteAdditionalAuxVariables; override;
    procedure GetITMP(var ITMP: integer; TimeIndex: integer;
      var List: TValueCellList); override;
    function GetActiveCellCount(CellList: TValueCellList): Integer; override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
    procedure ShowNoBoundaryError(const NoDefinedErrorRoot: string); override;
    class function ObservationOutputExtension: string; override;
    class function Extension: string; override;
  end;

implementation

uses frmErrorsAndWarningsUnit,
  ModflowUnitNumbers, frmProgressUnit,
  ModflowGridUnit, Forms, Mt3dmsChemSpeciesUnit,
  PestPropertiesUnit;

resourcestring
  StrErrorInCHDPackage = 'Error in CHD package';
  StrTheCHDPackageIsA = 'The CHD package is active but no CHD boundaries are' +
  ' assigned';
  StrTheFollowingCHDOb = 'The following CHD observation names may be valid f' +
  'or MODFLOW but they are not valid for UCODE.' + StrUcodeExplanation;
  StrTheFollowingCHDObPest = 'The following CHD observation names may be valid f' +
  'or MODFLOW but they are not valid for PEST.';
  StrWritingCHDPackage = 'Writing CHD Package input.';
  StrStartingHead = 'starting head';
  StrEndingHead = 'ending head';
  StrInvalidCHDS = 'Invalid CHD %s';
  StrTheCHD0sInLay = 'The CHD %0:s in (Layer, Row, Column) = (%1:d, %2:d, %3' +
  ':d) defined by the object %4:s is below the bottom of the cell.';
  StrMODFLOW6DoesNotA = 'MODFLOW 6 does not allow more than one CHD boundary' +
  ' in the same cell';
  StrTwoSpecifiedHead = 'Two specified head (CHD) boundaries are defined in ' +
  'cell (Layer,Row,Column = (%0:d,%1:d,%2:d) by the objects "%3:s" and "%4:s' +
  '".';
  StrCHDPackageIncompat = 'CHD package incompatible with SWR option ISWRONLY';
  StrBecauseTheCHDPack = 'Because the CHD package can not be used with the S' +
  'WR package when only surface water routing is used, the CHD package will ' +
  'not be included in this model.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';

{ TModflowCHD_Writer }

function TModflowCHD_Writer.CellType: TValueCellType;
begin
  result := TCHD_Cell;
end;

procedure TModflowCHD_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
var
  CHD_Cell: TCHD_Cell;
  LayerBottom: double;
  ScreenObject: TScreenObject;
begin
  CHD_Cell := ValueCell as TCHD_Cell;
  if not CHD_Cell.Active then
  begin
    Exit;
  end;
  inherited;
  if Model.DisvUsed then
  begin
    LayerBottom := Model.DisvGrid.ElementArrayI[
      CHD_Cell.Layer, CHD_Cell.Column].LowerElevation;
  end
  else
  begin
    LayerBottom := Model.Grid.CellElevation[ZeroBasedID(
      CHD_Cell.Layer+1, CHD_Cell.Row, CHD_Cell.Column)];
  end;
  ScreenObject := nil;
  if CHD_Cell.StartingHead < LayerBottom then
  begin
    ScreenObject := CHD_Cell.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddWarning(Model, Format(StrInvalidCHDS, [StrStartingHead]),
    Format(StrTheCHD0sInLay,
      [StrStartingHead, CHD_Cell.Layer+1, CHD_Cell.Row+1, CHD_Cell.Column+1, ScreenObject.Name]), ScreenObject);
  end;
  if CHD_Cell.EndingHead < LayerBottom then
  begin
    if ScreenObject = nil then
    begin
      ScreenObject := CHD_Cell.ScreenObject as TScreenObject;
    end;
    frmErrorsAndWarnings.AddWarning(Model, Format(StrInvalidCHDS, [StrEndingHead]),
    Format(StrTheCHD0sInLay,
      [StrEndingHead, CHD_Cell.Layer+1, CHD_Cell.Row+1, CHD_Cell.Column+1, ScreenObject.Name]), ScreenObject);
  end;
end;

procedure TModflowCHD_Writer.DoBeforeWriteCells;
begin
//  inherited;
  FChdCells := nil;
end;

procedure TModflowCHD_Writer.Evaluate;
var
  ParamCount, ParamCellCount: Integer;
  MXACTC: Integer;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrErrorInCHDPackage);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCHDStartingHeadSe);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCHDEndingHeadSet);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCHDConcentrationSe);
    frmErrorsAndWarnings.RemoveWarningGroup(Model,  Format(StrInvalidCHDS, [StrStartingHead]));
    frmErrorsAndWarnings.RemoveWarningGroup(Model,  Format(StrInvalidCHDS, [StrEndingHead]));
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMODFLOW6DoesNotA);
    inherited Evaluate;
    CountParametersAndParameterCells(ParamCount, ParamCellCount);
    CountCells(MXACTC);
    if (ParamCellCount = 0) and (MXACTC = 0) and (FEvaluationType = etExport) then
    begin
      frmErrorsAndWarnings.AddError(Model, StrErrorInCHDPackage,
        StrTheCHDPackageIsA);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowCHD_Writer.Extension: string;
begin
  result := '.chd';
end;

procedure TModflowCHD_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPCHD, MXL);
  if NPCHD > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPCHD);
    WriteInteger(MXL);
    WriteString(' # PARAMETER NPCHD MXL');
    NewLine;
  end;
end;

procedure TModflowCHD_Writer.WriteDataSet2;
var
  MXACTC: integer;
  Option: String;
begin
  CountCells(MXACTC);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteString(Option);
  WriteString(' # MXACTC');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowCHD_Writer.WriteAdditionalAuxVariables;
var
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
begin
  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ASpecies := Model.MobileComponents[SpeciesIndex];
      WriteString(' ' + ASpecies.Name);
    end;
  end;
  if FChdBoundary.UseMultiplier then
  begin
    WriteString(' multiplier');
  end;
end;

procedure TModflowCHD_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  CHD_Cell: TCHD_Cell;
  LocalLayer: integer;
  ParameterName: string;
  MultiplierValue: double;
  OtherCell: TCHD_Cell;
  CurrentScreenObject: TScreenObject;
  OtherScreenObject: TScreenObject;
  SpeciesIndex: Integer;
//  ASpecies: TMobileChemSpeciesItem;
begin
    { Add PEST support for PEST here }
    // handle pest parameter
    // handle multiply or add
  CHD_Cell := Cell as TCHD_Cell;
  if not CHD_Cell.Active then
  begin
    Exit;
  end;

  if (Model.ModelSelection = msModflow2015) then
  begin
    if Length(FChdCells) = 0 then
    begin
      SetLength(FChdCells, Model.LayerCount, Model.RowCount, Model.ColumnCount);
    end;
    if FChdCells[CHD_Cell.Layer, CHD_Cell.Row, CHD_Cell.Column] <> nil then
    begin
      OtherCell := FChdCells[CHD_Cell.Layer, CHD_Cell.Row, CHD_Cell.Column];
      CurrentScreenObject := CHD_Cell.ScreenObject as TScreenObject;
      OtherScreenObject := OtherCell.ScreenObject as TScreenObject;
      frmErrorsAndWarnings.AddError(Model, StrMODFLOW6DoesNotA,
        Format(StrTwoSpecifiedHead, [CHD_Cell.Layer+1, CHD_Cell.Row+1,
        CHD_Cell.Column+1, CurrentScreenObject.Name, OtherScreenObject.Name]),
        CurrentScreenObject);
    end
    else
    begin
      FChdCells[CHD_Cell.Layer, CHD_Cell.Row, CHD_Cell.Column] := CHD_Cell;
    end;
  end;

  LocalLayer := Model.
    DataSetLayerToModflowLayer(CHD_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(CHD_Cell.Row+1);
  end;
  WriteInteger(CHD_Cell.Column+1);

  if (CHD_Cell.HeadTimeSeriesName = '') then
  begin
    if (CHD_Cell.StartHeadPest <> '')
      or (CHD_Cell.EndHeadPest <> '')
      or (CHD_Cell.StartHeadPestSeriesName <> '')
      or (CHD_Cell.EndHeadPestSeriesName <> '') then
    begin
      FPestParamUsed := True;
    end;
  end;

  if FChdBoundary.UseMultiplier then
  begin
    if (CHD_Cell.MultiplierTimeSeriesName = '') then
    begin
      if (CHD_Cell.MultiplierPest <> '')
        or (CHD_Cell.MultiplierPestSeriesName <> '') then
      begin
        FPestParamUsed := True;
      end;
    end;
  end;

  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      if (CHD_Cell.ConcentrationPestNames[SpeciesIndex] <> '')
       or (CHD_Cell.ConcentrationPestSeriesNames[SpeciesIndex] <> '') then
      begin
        FPestParamUsed := True;
      end;
    end;
  end;

  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
    and WritingTemplate
    and ( CHD_Cell.HeadParameterName <> '')
    and (CHD_Cell.HeadTimeSeriesName = '') then
  begin
    ParameterName := CHD_Cell.HeadParameterName;
    if CHD_Cell.HeadParameterValue = 0 then
    begin
      MultiplierValue := 0.0;
    end
    else
    begin
      MultiplierValue := CHD_Cell.StartingHead / CHD_Cell.HeadParameterValue;
    end;
    WriteModflowParamFormula(ParameterName, CHD_Cell.StartHeadPest,
      MultiplierValue, CHD_Cell);
  end
  else
  begin
    WriteValueOrFormula(CHD_Cell, ChdStartHeadPosition);
  end;

  if (Model.ModelSelection <> msModflow2015) then
  begin
    WriteValueOrFormula(CHD_Cell, ChdEndHeadPosition);
  end;
  WriteIface(CHD_Cell.IFace);

  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      WriteValueOrFormula(CHD_Cell, ChdStartConcentration + SpeciesIndex);
    end;
  end;

  if FChdBoundary.UseMultiplier and (Model.ModelSelection = msModflow2015) then
  begin
    WriteValueOrFormula(CHD_Cell, ChdMultiplierPosition);
  end;

  WriteBoundName(CHD_Cell);

  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d '
      + VariableIdentifiers);
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column '
      + VariableIdentifiers);
  end;
  NewLine;
end;

procedure TModflowCHD_Writer.WriteDataSets3And4;
const
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Shdfact Ehdfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign, nil, nil);
end;

procedure TModflowCHD_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Shead Ehead IFACE';
  VariableIdentifiersMF6 = 'Shead IFACE';
var
  VarID: string;
  SpeciesIndex: Integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    VarID := VariableIdentifiersMF6;
    if Model.GwtUsed then
    begin
      for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
      begin
        VarID := VarID + ' ' + Model.MobileComponents[SpeciesIndex].Name;
      end;
    end;
    if FChdBoundary.UseMultiplier then
    begin
      VarID := VarID + ' multiplier';
    end;
    VarID := VarID + ' boundname';
  end
  else
  begin
    VarID := VariableIdentifiers;
  end;
  WriteStressPeriods(VarID, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCHD_Writer.WriteListOptions(InputFileName: string);
begin
  inherited;
  if FChdBoundary.UseMultiplier then
  begin
    WriteString('  AUXMULTNAME multiplier');
    NewLine;
  end;
  WriteMf6ParamListOption;
end;


procedure TModflowCHD_Writer.WriteFile(const AFileName: string);
var
  ShouldWriteObservationFile: Boolean;
begin
  FrmErrorsAndWarnings.RemoveWarningGroup(Model, StrCHDPackageIncompat);

  if not Package.IsSelected then
  begin
    Exit
  end;

  if (Model.ModelSelection <> msModflow2015)
    and Model.ModflowPackages.SwrPackage.IsSelected
    and (Model.ModflowPackages.SwrPackage.OnlyUseSWR) then
  begin
    FrmErrorsAndWarnings.AddWarning(Model, StrCHDPackageIncompat,
      StrBecauseTheCHDPack);
    Exit;
  end;

  if Model.ModelSelection = msModflow2015 then
  begin
    FAbbreviation := 'CHD6';
  end
  else
  begin
    FAbbreviation := StrCHD;
  end;
  FShouldWriteFile := not Model.PackageGeneratedExternally(FAbbreviation);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not Model.PackageGeneratedExternally(StrCHOB);

  if not FShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
//  FFileName := NameOfFile;
  if FShouldWriteFile then
  begin
    WriteToNameFile(FAbbreviation, Model.UnitNumbers.UnitNumber(StrCHD),
      NameOfFile, foInput, Model, False, 'CHD-1');
  end;
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
  FPestParamUsed := False;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteFileInternal;

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

procedure TModflowCHD_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingCHDPackage);
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
    end;

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets5to7);
    WriteDataSets5To7;
  finally
    CloseFile;
  end;

end;

procedure TModflowCHD_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQCH NQCCH NQTCH IUCHOBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTCH';
  DataSet3Comment = ' # Data Set 3: NQOBCH NQCLCH';
  PackageAbbreviation = StrCHOB;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    WriteFluxObsFile(AFileName, StrIUCHOBSV, PackageAbbreviation,
      DataSet1Comment, DataSet2Comment, DataSet3Comment,
      Model.HeadFluxObservations, Purpose);
  end;
end;

procedure TModflowCHD_Writer.WriteObservationCells(Variables, DataSets: TList;
  var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
  ScreenObject: TScreenObject; ObsFactor: TObservationFactor);
var
  List, CellList: TValueCellList;
  TimeIndex: Integer;
  CellIndex: Integer;
  ACell: TValueCell;
  ParamIndex: Integer;
  LocalParamValues: TList;
  TempCells: TList;
  CellArray: array of array of array of TValueCell;
  Grid: TModflowGrid;
begin
  TempCells := TList.Create;
  try
    Grid := Model.ModflowGrid;
    SetLength(CellArray, Grid.LayerCount, Grid.RowCount, Grid.ColumnCount);

    for TimeIndex := 0 to Values.Count - 1 do
    begin
      List := Values[TimeIndex];
      for CellIndex := 0 to List.Count - 1 do
      begin
        ACell := List[CellIndex];
        if (ACell.ScreenObject = ScreenObject as IScreenObject)
          and (CellArray[ACell.Layer, ACell.Row, ACell.Column] = nil) then
        begin
          TempCells.Add(ACell);
          CellArray[ACell.Layer, ACell.Row, ACell.Column] := ACell;
        end;
      end;
    end;
    for ParamIndex := 0 to ParamValues.Count - 1 do
    begin
      LocalParamValues := ParamValues.Objects[ParamIndex] as TList;
      for TimeIndex := 0 to LocalParamValues.Count - 1 do
      begin
        CellList := LocalParamValues[TimeIndex];
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          if (ACell.ScreenObject = ScreenObject as IScreenObject)
            and (CellArray[ACell.Layer, ACell.Row, ACell.Column] = nil) then
          begin
            TempCells.Add(ACell);
            CellArray[ACell.Layer, ACell.Row, ACell.Column] := ACell;
          end;
        end;
      end;
    end;
    for CellIndex := 0 to TempCells.Count - 1 do
    begin
      ACell := TempCells[CellIndex];
      WriteObservationCell(ACell, DataSet5, Expression, DataSets, Variables,
        ObsFactor);
    end;

  finally
    TempCells.Free;
  end;


end;

procedure TModflowCHD_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TCHD_Cell;
  CellIndex: Integer;
  ACell: TCHD_Cell;
begin
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TCHD_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'CHD');
  end;

  Cell := nil;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[CellIndex] as TCHD_Cell;
    if ACell.Active then
    begin
      Cell := ACell;
      break;
    end;
  end;

  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Shdfact Ehdfact are set equal to zero,
  // and because the cell location is at an existing boundary,
  // dummy boundaries have no effect. (Zero will be added to the
  // specified head at the location of an existing specified head
  // boundary.)
  if Cell <> nil then
  begin
    for CellIndex := CellList.Count to NLST - 1 do
    begin
      WriteInteger(Cell.Layer + 1);
      WriteInteger(Cell.Row + 1);
      WriteInteger(Cell.Column + 1);
      WriteFloat(0);
      WriteFloat(0);
      WriteInteger(0);
      WriteString(
        ' # Data Set 4b: Layer Row Column Shdfact Ehdfact IFACE (Dummy boundary)');
      NewLine;
    end;
  end;
end;

function TModflowCHD_Writer.GetActiveCellCount(
  CellList: TValueCellList): Integer;
var
  CellIndex: Integer;
  ChdCell: TCHD_Cell;
begin
  result := 0;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    ChdCell := CellList[CellIndex] as TCHD_Cell;
    if ChdCell.Active then
    begin
      Inc(result);
    end;
  end;
end;

function TModflowCHD_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowChdBoundary;
end;

procedure TModflowCHD_Writer.GetITMP(var ITMP: integer; TimeIndex: integer;
  var List: TValueCellList);
begin
  inherited;
  if ITMP <> -1 then
  begin
    ITMP := GetActiveCellCount(List);
  end;
end;

function TModflowCHD_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := ((AScreenObject.Modflow6Obs <> nil)
    and (ogCHD in AScreenObject.Modflow6Obs.General))
    or IsFlowObs(AScreenObject);
end;

class function TModflowCHD_Writer.Mf6ObType: TObGeneral;
begin
  result := ogCHD
end;

class function TModflowCHD_Writer.ObservationExtension: string;
begin
  result := '.ob_chob';
end;

class function TModflowCHD_Writer.ObservationOutputExtension: string;
begin
  result := '.chob_out';
end;

function TModflowCHD_Writer.ObservationPackage: TModflowPackageSelection;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    result := Model.ModflowPackages.Mf6ObservationUtility;
  end
  else
  begin
    result := Model.ModflowPackages.ChobPackage;
  end;
end;

function TModflowCHD_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowCHD_Writer.ObsFactors: TFluxObservationGroups;
begin
  result := Model.HeadFluxObservations;
end;

function TModflowCHD_Writer.ObsNameWarningString: string;
begin
  if Model.PestStatus in [psObservations, psActive] then
  begin
    result := StrTheFollowingCHDObPest;
  end
  else
  begin
    result := StrTheFollowingCHDOb;
  end;
end;

function TModflowCHD_Writer.ObsType: string;
begin
  result := 'chd';
end;

//function TModflowCHD_Writer.ObsTypeMF6: string;
//begin
//  result := ' chd-flow'
//end;

function TModflowCHD_Writer.Package: TModflowPackageSelection;
begin
  FChdBoundary := Model.ModflowPackages.ChdBoundary;
  result := FChdBoundary
end;

function TModflowCHD_Writer.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

procedure TModflowCHD_Writer.ShowNoBoundaryError(
  const NoDefinedErrorRoot: string);
begin
  if FEvaluationType = etExport then
  begin
    inherited;
  end;
end;

end.
