unit ModflowCncWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowGwtSpecifiedConcUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes, Modflow6ObsUnit;

type
  TModflowCncWriter = class(TCustomListWriter)
  private
    FSpeciesIndex: Integer;
    FCncPackage: TGwtCncPackage;
    FSpeciesName: string;
    FBoundaryFound: Boolean;
    FGwtFile: string;
    procedure PrintOptions;
    procedure PrintDimensions;
    procedure PrintStressPeriods;
    procedure WriteFileInternal;
//    procedure Evaluate;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
//    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
//      DS5, D7PNameIname, D7PName: string); override;
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
      ErrorRoot: string; const TimeIndex: integer); override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
  end;

implementation

{ TModflowCncWriter }

function TModflowCncWriter.CellType: TValueCellType;
begin
  result := TCnc_Cell
end;

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

function TModflowCncWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GwtCncPackage;
end;

function TModflowCncWriter.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

procedure TModflowCncWriter.PrintDimensions;
begin
  WriteDimensionsMF6;
end;

procedure TModflowCncWriter.PrintOptions;
begin
  WriteOptionsMF6(FGwtFile);
end;

procedure TModflowCncWriter.PrintStressPeriods;
const
  D7PName =      ' # Never Used';
  D7PNameIname = ' # Never Used';
  DS5 = ' # Data Set 5: ITMP ';
  DataSetIdentifier = 'Stress periods:';
  VariableIdentifiers = 'Conc IFACE boundname';
//var
//  VI: string;
begin
//  VI := VariableIdentifiers;
//  if Model.modelSelection = msModflow2015 then
//  begin
//    VI := VI + ' boundname';
//  end;
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCncWriter.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Cnc_Cell: TCnc_Cell;
  LocalLayer: integer;
//  MvrKey: TMvrRegisterKey;
//  ParameterName: string;
//  MultiplierValue: double;
begin
    { Add PEST support for PEST here }
    // handle pest data
    // handle multiply or add
  Inc(FBoundaryIndex);

  Cnc_Cell := Cell as TCnc_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Cnc_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Cnc_Cell.Row+1);
  end;
  WriteInteger(Cnc_Cell.Column+1);

  WriteValueOrFormula(Cnc_Cell, CncConcentrationPosition);

//  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
//    and WritingTemplate
//    and (Cnc_Cell.ConductanceParameterName <> '') then
//  begin
//    // PEST parameters are not allowed to be combined
//    // with MF-2005 style parameters.
//    Assert(Cnc_Cell.ConductancePest = '');
//    ParameterName := Cnc_Cell.ConductanceParameterName;
//    if Cnc_Cell.ConductanceParameterValue = 0 then
//    begin
//      MultiplierValue := 0.0;
//    end
//    else
//    begin
//      MultiplierValue := Cnc_Cell.Conductance
//        / Cnc_Cell.ConductanceParameterValue;
//    end;
//    WriteModflowParamFormula(ParameterName, Cnc_Cell.ConductancePest,
//      MultiplierValue, Cnc_Cell);
//  end
//  else
//  begin
//    WriteValueOrFormula(Cnc_Cell, DrnConductancePosition);
//  end;

  WriteIface(Cnc_Cell.IFace);
  WriteBoundName(Cnc_Cell);
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

//  if Cnc_Cell.MvrUsed and (MvrWriter <> nil) and not WritingTemplate then
//  begin
//    MvrKey.StressPeriod := FStressPeriod;
//    MvrKey.Index := FBoundaryIndex;
//    MvrKey.SourceKey.MvrIndex := Cnc_Cell.MvrIndex;
//    MvrKey.SourceKey.ScreenObject := Cnc_Cell.ScreenObject;
//    TModflowMvrWriter(MvrWriter).AddMvrSource(MvrKey);
//  end;
end;

procedure TModflowCncWriter.WriteFile(const AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
begin
  FSpeciesIndex := SpeciesIndex;
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

  if  Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TModflowCncWriter.WriteFileInternal;
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

procedure TModflowCncWriter.WriteMF6_ListParm(DataSetIdentifier,
  VariableIdentifiers, ErrorRoot: string; const TimeIndex: integer);
begin
  // do nothing
end;

procedure TModflowCncWriter.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);

end;

procedure TModflowCncWriter.WriteParameterDefinitions(const DS3, DS3Instances,
  DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);

end;

//procedure TModflowCncWriter.WriteStressPeriods(const VariableIdentifiers,
//  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
//begin
//  inherited;
//
//end;

end.
