unit ModflowCHD_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, RbwParser, CustomModflowWriterUnit,
  PhastModelUnit, ModflowConstantHeadBoundaryUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, ModflowPackageSelectionUnit, ModflowCellUnit,
  OrderedCollectionUnit, FluxObservationUnit, GoPhastTypes;

type
  TModflowCHD_Writer = class(TFluxObsWriter)
  private
    NPCHD: integer;
    MXL: integer;
//    FFileName: string;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
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
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    class function ObservationExtension: string; override;
    class function ObservationOutputExtension: string; override;
    procedure WriteObservationCells(Variables, DataSets: TList;
      var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
      ScreenObject: TScreenObject; ObsFactor: TObservationFactor); override;
    function ObsNameWarningString: string; override;
    procedure Evaluate; override;
    function ObsTypeMF6: string; override;
    procedure CheckCell(ValueCell: TValueCell;
      const PackageName: string); override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function ObservationsUsed: Boolean; override;
    procedure WriteListOptions(InputFileName: string); override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit,
  ModflowGridUnit, Forms;

resourcestring
  StrErrorInCHDPackage = 'Error in CHD package';
  StrTheCHDPackageIsA = 'The CHD package is active but no CHD boundaries are' +
  ' assigned';
  StrTheFollowingCHDOb = 'The following CHD observation names may be valid f' +
  'or MODFLOW but they are not valid for UCODE.';
  StrWritingCHDPackage = 'Writing CHD Package input.';
  StrStartingHead = 'starting head';
  StrEndingHead = 'ending head';
  StrInvalidCHDS = 'Invalid CHD %s';
  StrTheCHD0sInLay = 'The CHD %0:s in (Layer, Row, Column) = (%1:d, %2:d, %3' +
  ':d) defined by the object %4:s is below the bottom of the cell.';
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
  inherited;
  CHD_Cell := ValueCell as TCHD_Cell;
  if Model.DisvUsed then
  begin
    LayerBottom := Model.DisvGrid.ElementArrayI[
      CHD_Cell.Layer, CHD_Cell.Column].LowerElevation;
  end
  else
  begin
    LayerBottom := Model.Grid.CellElevation[
      CHD_Cell.Column, CHD_Cell.Row, CHD_Cell.Layer+1];
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

procedure TModflowCHD_Writer.Evaluate;
var
  ParamCount, ParamCellCount: Integer;
  MXACTC: Integer;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrErrorInCHDPackage);
    frmErrorsAndWarnings.RemoveWarningGroup(Model,  Format(StrInvalidCHDS, [StrStartingHead]));
    frmErrorsAndWarnings.RemoveWarningGroup(Model,  Format(StrInvalidCHDS, [StrEndingHead]));
    inherited;
    CountParametersAndParameterCells(ParamCount, ParamCellCount);
    CountCells(MXACTC);
    if (ParamCellCount = 0) and (MXACTC = 0) then
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

procedure TModflowCHD_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  CHD_Cell: TCHD_Cell;
  LocalLayer: integer;
begin
  CHD_Cell := Cell as TCHD_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(CHD_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(CHD_Cell.Row+1);
  end;
  WriteInteger(CHD_Cell.Column+1);

  if CHD_Cell.TimeSeriesName = '' then
  begin
    WriteFloat(CHD_Cell.StartingHead);
  end
  else
  begin
    WriteString(' ');
    WriteString(CHD_Cell.TimeSeriesName);
    WriteString(' ');
  end;


//  WriteFloat(CHD_Cell.StartingHead);
  if (Model.ModelSelection <> msModflow2015) then
  begin
    WriteFloat(CHD_Cell.EndingHead);
  end;
  WriteIface(CHD_Cell.IFace);
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
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
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
  VariableIdentifiersMF6 = 'Shead IFACE boundname';
var
  VarID: string;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    VarID := VariableIdentifiersMF6;
  end
  else
  begin
    VarID := VariableIdentifiers;
  end;
  WriteStressPeriods(VarID, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCHD_Writer.WriteListOptions(InputFileName: string);
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


procedure TModflowCHD_Writer.WriteFile(const AFileName: string);
var
//  NameOfFile: string;
  ShouldWriteFile: Boolean;
  ShouldWriteObservationFile: Boolean;
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Abbreviation := 'CHD6';
  end
  else
  begin
    Abbreviation := StrCHD;
  end;
  ShouldWriteFile := not Model.PackageGeneratedExternally(Abbreviation);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not Model.PackageGeneratedExternally(StrCHOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
//  FFileName := NameOfFile;
  if ShouldWriteFile then
  begin
    WriteToNameFile(Abbreviation, Model.UnitNumbers.UnitNumber(StrCHD),
      NameOfFile, foInput, Model);
  end;
  if ShouldWriteFile or ShouldWriteObservationFile then
  begin
    Evaluate;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ClearTimeLists(Model);
  end;
  if not ShouldWriteFile then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingCHDPackage);
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

//    if Model.ModelSelection <> msModflow2015 then
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

  if Model.ModelSelection = msModflow2015 then
  begin
    WriteModflow6FlowObs(NameOfFile, FEvaluationType);
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
//  end
//  else
//  begin
//    WriteFluxObsFileMF6(AFileName, StrIUCHOBSV, PackageAbbreviation,
//      DataSet1Comment, DataSet2Comment, DataSet3Comment,
//      Model.HeadFluxObservations, Purpose);
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
        if (ACell.ScreenObject = ScreenObject)
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
          if (ACell.ScreenObject = ScreenObject)
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
begin
  Cell := nil;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TCHD_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'CHD');
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Shdfact Ehdfact are set equal to zero,
  // and because the cell location is at an exsiting boundary,
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

function TModflowCHD_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowChdBoundary;
end;


function TModflowCHD_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
    and AScreenObject.Modflow6Obs.Used
    and AScreenObject.Modflow6Obs.ChdFlowObs;
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
  result := Model.ModflowPackages.ChobPackage;
end;

function TModflowCHD_Writer.ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
//  result := inherited ObservationsUsed;
//  if result then
//  begin
//    Model.ModflowPackages.
//  end;
end;

function TModflowCHD_Writer.ObsNameWarningString: string;
begin
  result := StrTheFollowingCHDOb;
end;

function TModflowCHD_Writer.ObsType: string;
begin
  result := 'chd';
end;

function TModflowCHD_Writer.ObsTypeMF6: string;
begin
  result := ' chd-flow'
end;

function TModflowCHD_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.ChdBoundary;
end;

function TModflowCHD_Writer.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

end.
