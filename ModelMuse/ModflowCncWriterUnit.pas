unit ModflowCncWriterUnit;

interface

uses System.SysUtils, SubscriptionUnit, System.Classes, Mt3dmsChemSpeciesUnit,
  CustomModflowWriterUnit,
  ModflowGwtSpecifiedConcUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit,
  GoPhastTypes, Modflow6ObsUnit, ModflowBoundaryDisplayUnit, Vcl.Dialogs;

type
  TCustomSimpleGwtBoundaryWriter = class abstract(TCustomListWriter)
  protected
    FGwtFile: string;
    FSpeciesIndex: Integer;
    FSpeciesName: string;
    FPackage: TMultiplierPackage;
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
    procedure WriteMultiplier; virtual; abstract;
  public
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists;
      Species: Integer);
  end;

  TModflowCncWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FCncPackage: TGwtCncPackage;
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
    procedure WriteAdditionalAuxVariables; override;
    procedure WriteListOptions(InputFileName: string); override;
    procedure WriteMultiplier; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;

  TModflowSrcWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FSrcPackage: TGwtSrcPackage;
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
    procedure WriteAdditionalAuxVariables; override;
    procedure WriteListOptions(InputFileName:string); override;
    procedure WriteMultiplier; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;

    TModflowCtpWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FCtpPackage: TGweCtpPackage;
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
    procedure WriteAdditionalAuxVariables; override;
    procedure WriteListOptions(InputFileName: string); override;
    procedure WriteMultiplier; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;

  TModflowEslWriter = class(TCustomSimpleGwtBoundaryWriter)
  protected
    FEslPackage: TGweEslPackage;
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
    procedure WriteAdditionalAuxVariables; override;
    procedure WriteListOptions(InputFileName:string); override;
    procedure WriteMultiplier; override;
  public
    procedure WriteFile(const AFileName: string; SpeciesIndex: Integer);
    class function ObservationOutputExtension: string; override;
  end;


implementation

uses
  frmProgressUnit, DataSetUnit;

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
var
  Mf6Obs: TModflow6Obs;
begin
  Mf6Obs := AScreenObject.Modflow6Obs;
  result := (Mf6Obs <> nil) and (((FSpeciesIndex in Mf6Obs.Genus)
    and (ogwtCNC in Mf6Obs.GwtObs))
    or (ogwtCNC in Mf6Obs.CalibrationObservations.GwtObs[FSpeciesIndex]))
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

procedure TModflowCncWriter.WriteAdditionalAuxVariables;
begin
  inherited;
  if FCncPackage.UseMultiplier then
  begin
    writeString(' multiplier');
  end;
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
  if not Model.MobileComponents[SpeciesIndex].UsedForGWT then
  begin
    Exit;
  end;
  FCncPackage := Model.ModflowPackages.GwtCncPackage;
  FPackage := FCncPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FSpeciesIndex := SpeciesIndex;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'CNC6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;
  FInputFileName := FGwtFile;

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

procedure TModflowCncWriter.WriteListOptions(InputFileName:string);
begin
  inherited;
  WriteMultiplier;
end;

procedure TModflowCncWriter.WriteMultiplier;
begin
  if FCncPackage.UseMultiplier then
  begin
    WriteString('  AUXMULTNAME multiplier');
    NewLine;
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
    WriteTemplateHeader;
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
  if not Cnc_Cell.Active then
  begin
    Exit;
  end;
  LocalLayer := Model.DataSetLayerToModflowLayer(Cnc_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Cnc_Cell.Row + 1);
  end;
  WriteInteger(Cnc_Cell.Column + 1);
  WriteValueOrFormula(Cnc_Cell, CncConcentrationPosition);
  WriteIface(Cnc_Cell.IFace);

  if FPackage.UseMultiplier then
  begin
    WriteValueOrFormula(Cnc_Cell, CncMultiplierPosition);
  end;

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
var
  Mf6Obs: TModflow6Obs;
begin
  Mf6Obs := AScreenObject.Modflow6Obs;
  result := (Mf6Obs <> nil) and (((FSpeciesIndex in Mf6Obs.Genus)
    and (ogwtSRC in Mf6Obs.GwtObs))
    or (ogwtSRC in Mf6Obs.CalibrationObservations.GwtObs[FSpeciesIndex]))
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

procedure TModflowSrcWriter.WriteAdditionalAuxVariables;
begin
  inherited;
  if FSrcPackage.UseMultiplier then
  begin
    writeString(' multiplier');
  end;
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
  if not Model.MobileComponents[SpeciesIndex].UsedForGWT then
  begin
    Exit;
  end;
  FSrcPackage := Model.ModflowPackages.GwtSrcPackage;
  FPackage := FSrcPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FSpeciesIndex := SpeciesIndex;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'SRC6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;
  FInputFileName := FGwtFile;

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

procedure TModflowSrcWriter.WriteListOptions;
begin
  inherited;
  WriteMultiplier;
end;

procedure TModflowSrcWriter.WriteMultiplier;
begin
  if FSrcPackage.UseMultiplier then
  begin
    WriteString('  AUXMULTNAME multiplier');
    NewLine;
  end;
end;

procedure TCustomSimpleGwtBoundaryWriter.UpdateDisplay
  (TimeLists: TModflowBoundListOfTimeLists; Species: Integer);
var
  Index: Integer;
  DataSets: TList;
  UsedIndicies: TByteSet;
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  DataSetIndex: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
  CellList: TValueCellList;
  DataIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  FSpeciesIndex := Species;
  FSpeciesName := Model.MobileComponents[FSpeciesIndex].Name;
  try
    DataSets := TList.Create;
    try
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
        DataSets.Clear;
      for DataIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[DataIndex];
        for TimeIndex := 0 to TimeList.Count - 1 do
        begin
          DataArray := TimeList[TimeIndex] as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;
      end;
      for Index := 0 to Values.Count - 1 do
      begin
        CellList := Values[Index];
        UsedIndicies := [];
        UpdateCellDisplay(CellList, DataSets, [], nil, UsedIndicies);
      end;
      for DataSetIndex := 0 to DataSets.Count - 1 do
      begin
        DataArray := DataSets[DataSetIndex];
        DataArray.UpToDate := True;
        DataArray.CacheData;
      end;
      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
    end;
  except
    on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

{ TModflowCtpWriter }

class function TModflowCtpWriter.Extension: string;
begin
  result := '.ctp';
end;

function TModflowCtpWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.GweCtpBoundary;
  if result <> nil then
  begin
    if ScreenObject.GweCtpBoundary.ChemSpecies <> FSpeciesName then
    begin
      result := nil;
    end
    else
    begin
      FBoundaryFound := True;
    end;
  end;
end;

function TModflowCtpWriter.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  Mf6Obs: TModflow6Obs;
begin
  Mf6Obs := AScreenObject.Modflow6Obs;
  result := (Mf6Obs <> nil) and (((FSpeciesIndex in Mf6Obs.Genus)
    and (ogwtCTP in Mf6Obs.GwtObs))
    or (ogwtCTP in Mf6Obs.CalibrationObservations.GwtObs[FSpeciesIndex]))
end;

class function TModflowCtpWriter.Mf6GwtObType: TObGwt;
begin
  result := ogwtCTP;
end;

function TModflowCtpWriter.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

class function TModflowCtpWriter.ObservationExtension: string;
begin
  result := '.ob_ctp';
end;

class function TModflowCtpWriter.ObservationOutputExtension: string;
begin
  result := '.ob_ctp_out';
end;

function TModflowCtpWriter.ObsType: string;
begin
  result := 'ctp'
end;

function TModflowCtpWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GweCtpPackage;
end;

procedure TModflowCtpWriter.PrintStressPeriods;
const
  D7PName =      ' # Never Used';
  D7PNameIname = ' # Never Used';
  DS5 = ' # Data Set 5: ITMP ';
  DataSetIdentifier = 'Stress periods:';
  VariableIdentifiers = 'Temperature IFACE boundname';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCtpWriter.WriteAdditionalAuxVariables;
begin
  inherited;
  if FCtpPackage.UseMultiplier then
  begin
    writeString(' multiplier');
  end;
end;

procedure TModflowCtpWriter.WriteFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GweUsed then
  begin
    Exit
  end;
  if not Model.MobileComponents[SpeciesIndex].UsedForGWE then
  begin
    Exit;
  end;
  FCtpPackage := Model.ModflowPackages.GweCtpPackage;
  FPackage := FCtpPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FSpeciesIndex := SpeciesIndex;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'CTP6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;
  FInputFileName := FGwtFile;

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

procedure TModflowCtpWriter.WriteListOptions(InputFileName: string);
begin
  inherited;
  WriteMultiplier;
end;

procedure TModflowCtpWriter.WriteMultiplier;
begin
  if FCtpPackage.UseMultiplier then
  begin
    WriteString('  AUXMULTNAME multiplier');
    NewLine;
  end;
end;

{ TModflowEslWriter }

class function TModflowEslWriter.Extension: string;
begin
  result := '.esl';
end;

function TModflowEslWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.GweEslBoundary;
  if result <> nil then
  begin
    if ScreenObject.GweEslBoundary.ChemSpecies <> FSpeciesName then
    begin
      result := nil;
    end
    else
    begin
      FBoundaryFound := True;
    end;
  end;
end;

function TModflowEslWriter.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  Mf6Obs: TModflow6Obs;
begin
  Mf6Obs := AScreenObject.Modflow6Obs;
  result := (Mf6Obs <> nil) and (((FSpeciesIndex in Mf6Obs.Genus)
    and (ogwtESL in Mf6Obs.GwtObs))
    or (ogwtESL in Mf6Obs.CalibrationObservations.GwtObs[FSpeciesIndex]))
end;

class function TModflowEslWriter.Mf6GwtObType: TObGwt;
begin
  result := ogwtESL;
end;

function TModflowEslWriter.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

class function TModflowEslWriter.ObservationExtension: string;
begin
  result := '.ob_esl';
end;

class function TModflowEslWriter.ObservationOutputExtension: string;
begin
  result := '.ob_esl_out';
end;

function TModflowEslWriter.ObsType: string;
begin
  result := 'esl'
end;

function TModflowEslWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GweEslPackage;
end;

procedure TModflowEslWriter.PrintStressPeriods;
const
  D7PName =      ' # Never Used';
  D7PNameIname = ' # Never Used';
  DS5 = ' # Data Set 5: ITMP ';
  DataSetIdentifier = 'Stress periods:';
  VariableIdentifiers = 'senerrate IFACE boundname';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowEslWriter.WriteAdditionalAuxVariables;
begin
  inherited;
  if FEslPackage.UseMultiplier then
  begin
    writeString(' multiplier');
  end;
end;

procedure TModflowEslWriter.WriteFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if not Model.GweUsed then
  begin
    Exit
  end;
  if not Model.MobileComponents[SpeciesIndex].UsedForGWE then
  begin
    Exit;
  end;
  FEslPackage := Model.ModflowPackages.GweEslPackage;
  FPackage := FEslPackage;
  FSpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FSpeciesIndex := SpeciesIndex;

  Evaluate;

  if not FBoundaryFound then
  begin
    Exit;
  end;

  Abbreviation := 'ESL6';

  FGwtFile := GwtFileName(AFileName, SpeciesIndex);
  FNameOfFile := FGwtFile;
  FInputFileName := FGwtFile;

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

procedure TModflowEslWriter.WriteListOptions(InputFileName: string);
begin
  inherited;
  WriteMultiplier;
end;

procedure TModflowEslWriter.WriteMultiplier;
begin
  if FEslPackage.UseMultiplier then
  begin
    WriteString('  AUXMULTNAME multiplier');
    NewLine;
  end;
end;

end.
