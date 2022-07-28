unit ModflowCncWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowGwtSpecifiedConcUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes, Modflow6ObsUnit;

type
  TModflowCncWriter = class(TCustomParameterTransientWriter)
  private
    FSpeciesIndex: Integer;
    FCncPackage: TGwtCncPackage;
    FSpeciesName: string;
    FBoundaryFound: Boolean;
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
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;

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
//  WriteOptionsMF6(FNameOfFile);
end;

procedure TModflowCncWriter.PrintStressPeriods;
begin

end;

procedure TModflowCncWriter.WriteFile(const AFileName: string; SpeciesIndex: Integer);
var
  Abbreviation: string;
  GwtFile: string;
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

  Abbreviation := 'IST6';

  GwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := GwtFile;

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

end;

procedure TModflowCncWriter.WriteParameterDefinitions(const DS3, DS3Instances,
  DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  inherited;

end;

procedure TModflowCncWriter.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin
  inherited;

end;

end.
