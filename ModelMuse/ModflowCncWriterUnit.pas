unit ModflowCncWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowGwtSpecifiedConcUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes, Modflow6ObsUnit;

type
  TCustomSimpleGwtBoundaryWriter = class abstract(TCustomListWriter)
  protected
    FGwtFile: string;
    procedure PrintOptions;
    procedure PrintDimensions;
    procedure PrintStressPeriods; virtual; abstract;
    procedure WriteFileInternal;
    function CellType: TValueCellType; override;
    procedure WriteCell(Cell: TValueCell; const DataSetIdentifier,
      VariableIdentifiers: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteMF6_ListParm(DataSetIdentifier, VariableIdentifiers,
      ErrorRoot: string; const TimeIndex: Integer); override;
  end;

  TModflowCncWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FCncPackage: TGwtCncPackage;
    FSpeciesName: string;
    FBoundaryFound: Boolean;
    procedure PrintStressPeriods; override;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    class function ObservationExtension: string; override;
    Class function Mf6GwtObType: TObGwt; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;

  TModflowSrcWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FSrcPackage: TGwtSrcPackage;
    FSpeciesName: string;
    FBoundaryFound: Boolean;
    procedure PrintStressPeriods; override;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    class function ObservationExtension: string; override;
    Class function Mf6GwtObType: TObGwt; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;

implementation

{ TModflowCncWriter }

class function TModflowCncWriter.Extension: string;
begin
  result := '.cnc';
end;

function TModflowCncWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.GwtCncBoundary;
  if result <> nil then
  begin
    if ScreenObject.GwtCncBoundary.ChemSpecies <> FSpeciesName then
    begin
      result := nil;
    end
    else
    begin
      FBoundaryFound := True;
    end;
  end;
end;

function TModflowCncWriter.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := ((AScreenObject.Modflow6Obs <> nil)
    and (ogwtCNC in AScreenObject.Modflow6Obs.GwtObs))
end;

class function TModflowCncWriter.Mf6GwtObType: TObGwt;
begin
  result := ogwtCNC;
end;

function TModflowCncWriter.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

class function TModflowCncWriter.ObservationExtension: string;
begin
  result := '.ob_cnc';
end;

class function TModflowCncWriter.ObservationOutputExtension: string;
begin
  result := '.ob_cnc_out';
end;

function TModflowCncWriter.ObsType: string;
begin
  result := 'cnc'
end;

function TModflowCncWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtCncPackage;
end;

procedure TModflowCncWriter.PrintStressPeriods;
const
  D7PName =      ' # Never Used';
  D7PNameIname = ' # Never Used';
  DS5 = ' # Data Set 5: ITMP ';
  DataSetIdentifier = 'Stress periods:';
  VariableIdentifiers = 'Conc IFACE boundname';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCncWriter.WriteFile(const AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FCncPackage := Model.ModflowPackages.GwtCncPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'CNC6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  WriteFileInternal;

  WriteModflow6GwtFlowObs(NameOfFile, FEvaluationType, SpeciesIndex);

  if  Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TCustomSimpleGwtBoundaryWriter.PrintOptions;
begin
  WriteOptionsMF6(FGwtFile);
end;

procedure TCustomSimpleGwtBoundaryWriter.PrintDimensions;
begin
  WriteDimensionsMF6;
end;

procedure TCustomSimpleGwtBoundaryWriter.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    PrintOptions;
    PrintDimensions;
    PrintStressPeriods;
  finally
    CloseFile;
  end;
end;

function TCustomSimpleGwtBoundaryWriter.CellType: TValueCellType;
begin
  result := TCnc_Cell
end;

procedure TCustomSimpleGwtBoundaryWriter.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Cnc_Cell: TCnc_Cell;
  LocalLayer: Integer;
begin
  { Add PEST support for PEST here }
  // handle pest data
  // handle multiply or add
  Inc(FBoundaryIndex);
  Cnc_Cell := Cell as TCnc_Cell;
  LocalLayer := Model.DataSetLayerToModflowLayer(Cnc_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Cnc_Cell.Row + 1);
  end;
  WriteInteger(Cnc_Cell.Column + 1);
  WriteValueOrFormula(Cnc_Cell, CncConcentrationPosition);
  WriteIface(Cnc_Cell.IFace);
  WriteBoundName(Cnc_Cell);
  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d ' +
      VariableIdentifiers);
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column ' +
      VariableIdentifiers);
  end;
  NewLine;
end;

function TCustomSimpleGwtBoundaryWriter.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

procedure TCustomSimpleGwtBoundaryWriter.WriteParameterDefinitions(const DS3,
  DS3Instances, DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
end;

procedure TCustomSimpleGwtBoundaryWriter.WriteParameterCells
  (CellList: TValueCellList; NLST: Integer; const VariableIdentifiers,
  DataSetIdentifier: string; AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
end;

procedure TCustomSimpleGwtBoundaryWriter.WriteMF6_ListParm(DataSetIdentifier,
  VariableIdentifiers, ErrorRoot: string; const TimeIndex: Integer);
begin
  // do nothing
end;

{ TModflowSrcWriter }

class function TModflowSrcWriter.Extension: string;
begin
  result := '.src';
end;

function TModflowSrcWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.GwtSrcBoundary;
  if result <> nil then
  begin
    if ScreenObject.GwtSrcBoundary.ChemSpecies <> FSpeciesName then
    begin
      result := nil;
    end
    else
    begin
      FBoundaryFound := True;
    end;
  end;
end;

function TModflowSrcWriter.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := ((AScreenObject.Modflow6Obs <> nil)
    and (ogwtSRC in AScreenObject.Modflow6Obs.GwtObs))
end;

class function TModflowSrcWriter.Mf6GwtObType: TObGwt;
begin
  result := ogwtSRC;
end;

function TModflowSrcWriter.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

class function TModflowSrcWriter.ObservationExtension: string;
begin
  result := '.ob_src';
end;

class function TModflowSrcWriter.ObservationOutputExtension: string;
begin
  result := '.ob_src_out';
end;

function TModflowSrcWriter.ObsType: string;
begin
  result := 'src'
end;

function TModflowSrcWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtSrcPackage;
end;

procedure TModflowSrcWriter.PrintStressPeriods;
const
  D7PName =      ' # Never Used';
  D7PNameIname = ' # Never Used';
  DS5 = ' # Data Set 5: ITMP ';
  DataSetIdentifier = 'Stress periods:';
  VariableIdentifiers = 'smassrate IFACE boundname';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowSrcWriter.WriteFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GwtUsed then
  begin
    Exit
  end;
  FSrcPackage := Model.ModflowPackages.GwtSrcPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'SRC6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  WriteModflow6GwtFlowObs(NameOfFile, FEvaluationType, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  WriteFileInternal;

  if  Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

end.
