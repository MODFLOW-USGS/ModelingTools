unit ModflowMawWriterUnit;

interface

uses
  System.UITypes, Winapi.Windows, System.Classes, Vcl.Forms, System.SysUtils,
  System.Generics.Collections, CustomModflowWriterUnit, ScreenObjectUnit,
  GoPhastTypes, ModflowPackageSelectionUnit, PhastModelUnit, ModflowMawUnit,
  ModflowBoundaryDisplayUnit, Modflow6ObsUnit;

type
  TMawObservation = record
    FName: string;
    FWellNumber: Integer;
    FBoundName: string;
    FCount: Integer;
    FObsTypes: TMawObs;
    FModflow6Obs: TModflow6Obs;
  end;
  TMawObservationList = TList<TMawObservation>;

type
  TMwtObservation = record
    FName: string;
    FWellNumber: Integer;
    FBoundName: string;
    FCount: Integer;
    FObsTypes: TMwtObs;
    FSpecies: Integer;
    FModflow6Obs: TModflow6Obs;
  end;
  TMwtObservationList = TList<TMwtObservation>;
  TMwtObservationLists = TObjectList<TMwtObservationList>;

  TModflowMAW_Writer = class(TCustomTransientWriter)
  private
    FMawObjects: TScreenObjectList;
    FMawNames: TStringList;
    FWellProperties: array of TMawSteadyWellRecord;
    FWellConnections: TList<TMawSteadyConnectionRecord>;
    FMawPackage: TMawPackage;
    FFlowingWells: Boolean;
    FMawObservations: TMawObservationList;
    FGwtObservations: TMwtObservationLists;
    FSpeciesIndex: Integer;
    procedure AssignWellScreensAndWellProperties;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackageData;
    procedure WriteConnectionData;
    procedure WriteStressPeriods;
    function CheckOK(AScreenObject: TScreenObject;
      Boundary: TMawBoundary): Boolean;
    procedure WriteFileInternal;
    // MWT
    procedure WriteGwtOptions;
    procedure WriteGwtPackageData;
    procedure WriteGwtStressPeriods;
    procedure WriteGwtFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
    procedure Evaluate; override;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; override;
    class function GwtObservationExtension: string;
    // @name is the file extension used for the observation output file.
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function IsMf6GwtObservation(AScreenObject: TScreenObject;
      SpeciesIndex: Integer): Boolean;
//    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean; override;
    function Mf6ObservationsUsed: Boolean; override;
    procedure WriteAdditionalAuxVariables;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteMwtFile(const AFileName: string; SpeciesIndex: Integer);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    procedure UpdateSteadyData;
    class function ObservationOutputExtension: string; override;
    class function Extension: string; override;
  end;

const
  StrMAW1 = 'MAW-1';
  StrMawbud = '.maw_bud';

implementation

uses
  frmProgressUnit,
  frmErrorsAndWarningsUnit, ModflowCellUnit, RbwParser, frmFormulaErrorsUnit,
  DataSetUnit, GIS_Functions, AbstractGridUnit, System.Math, ModflowUnitNumbers,
  MeshRenumberingTypes, Vcl.Dialogs, Modflow6ObsWriterUnit,
  ModflowMvrUnit, ModflowMvrWriterUnit, ModflowParameterUnit,
  Mt3dmsChemUnit, Mt3dmsChemSpeciesUnit, GwtStatusUnit, DataSetNamesUnit,
  CellLocationUnit;

resourcestring
  StrTheFollowingObject = 'The following objects can not be used to define m' +
  'ulti-aquifer wells in the MAW package because they do not contain exactly one ' +
  'vertex.';
  StrECountZero = 'The following objects can not be used to define multi-aqu' +
  'ifer wells in the MAW package because they do not have an elevation count' +
  ' of zero.';
  StrNoTransientDataFo = 'No transient data for the MAW package has been def' +
  'ined for the following objects. They will be skipped.';
  StrNoWellScreensInT = 'No well screens in the MAW package have been define' +
  'd for the following objects. They will be skipped';
  StrBecauseTheFollowin = 'Because the following objects do not set values o' +
  'f intersected cells, they can not define MAW wells.';
  StrAssignedBy0sUsi = 'Assigned by %0:s using the formula ''%1:s''';
  StrMAWRadius = 'MAW Radius';
  StrMAWBottom = 'MAW Bottom';
  StrMAWInitialHead = 'MAW Initial Head';
  StrMAWScreenBottom = 'MAW Screen Bottom';
  StrMAWSkinK = 'MAW Skin K';
  StrMAWSkinRadius = 'MAW Skin Radius';
  StrWritingMAWPackag = '  Writing MAW Package Data';
  StrWritingMAWStre = '    Writing MAW Stress Period %d';
  StrWritingMAWConnec = '  Writing MAW Connection Data';
  StrWritingMAWStress = '  Writing MAW Stress Period Data';
  StrTheFollowingObjectGrid = 'The following objects do not intersect the grid s' +
  'o they will not be used to define wells in the MAW package.';
  StrWritingMAWPackage = 'Writing MAW Package input.';
  StrMAWScreenTop = 'MAW Screen Top';
  StrTheMultiaquiferWe = 'The multi-aquifer well package was not included in' +
  ' the model because no multi-aquifer wells have been defined.'
  + sLineBreak
  + 'Multi-aquifer wells can be defined with point objects on the top view of '
  + 'the model that have zero Z-formulas.';
  StrMAWPackageSkipped = 'MAW package skipped.';
  StrMoreThanOneMAWSc = 'More than one MAW screen in the same cell';
  StrTheObject0sDefi = 'The object %0:s defines two or more MAW  well screen' +
  's in the same cell (%1:d, %2:d, %3:d). This is only allowed if MEAN is selcted for the conductance equation.';
  StrMAWSkinRadiusLess = 'MAW skin radius less than or equal to well radius';
  StrInSTheMAWSkin = 'In %s, the MAW skin radius less than or equal to well ' +
  'radius';
  StrEvaluatingS = '    Evaluating %s';
  StrEvaluatingMAWPacka = 'Evaluating MAW Package data.';
  StrWritingMAWObservat = 'Writing MAW observations';
  StrFormulaErrorInMAW = 'Formula Error in MAW';
  StrThereWasAnErrorI = 'There was an error in a MAW formula in %s.';
  StrMAWWellScreenInva = 'MAW Well screen invalid';
  StrWellScreen0dOf = 'Well screen %0:d of the MAW boundary defined by %1:s ' +
  'does not intersect any active cells.';
  StrMAWWellScreensInv = 'MAW Well screens invalid';
  StrNoneOfTheWellScr = 'None of the well screens of the MAW boundary define' +
  'd by %0:s intersect any active cells.';
  StrFlowingWellReducti = 'Flowing well reduction length <= 0';
  StrIn0sInStressPe = 'In %0:s in stress period %1:d, the flowing well reduc' +
  'tion length is less than or equal to zero.';
  StrMAWWellRadiusToo = 'MAW well radius too large';
  StrTheWellRadiusFor = 'The well radius for the multiaquifer well defined b' +
  'y %s makes the well diameter larger than the cell size.';
  StrMAWWellSkinRadius = 'MAW well skin radius too large';
  StrTheWellSkinRadius = 'The well skin radius for the multiaquifer well def' +
  'ined by %s makes the well skin diameter larger than the cell size.';
  StrMAWStartingHeadLe = 'MAW starting head less than well bottom';
  StrMAWStartingHeadTo = 'MAW starting head too low at (%0:d, %1:d, %2:d)';
  StrMAWTimeExtended = 'MAW Time extended';
  StrIn0sTheLastDe = 'In %0:s, the last defined time was %1:g which was befo' +
  're the end of the model. It has been extended to %2:g';
  StrInactiveMAWPeriod = 'Inactive MAW period added';
  StrIn0sThereWasA = 'In %0:s, there was a gap in time from %1:g to %2:g. An' +
  ' inactive period has been inserted to fill that time.';

{ TModflowMAW_Writer }

constructor TModflowMAW_Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
var
  index: Integer;
begin
  inherited;
  FMawObjects := TScreenObjectList.Create;
  FMawNames := TStringList.Create;
  FWellConnections := TList<TMawSteadyConnectionRecord>.Create;
  FMawObservations := TMawObservationList.Create;
  FMawPackage := Package as TMawPackage;
  FGwtObservations := TMwtObservationLists.Create;
  if Model.GwtUsed then
  begin
    for index := 0 to Model.MobileComponents.Count - 1 do
    begin
      FGwtObservations.Add(TMwtObservationList.Create);
    end;
  end;
end;

destructor TModflowMAW_Writer.Destroy;
begin
  FGwtObservations.Free;
  FMawObservations.Free;
  FWellConnections.Free;
  FMawObjects.Free;
  FMawNames.Free;
  inherited;
end;

function TModflowMAW_Writer.CheckOK(AScreenObject: TScreenObject;
  Boundary: TMawBoundary): Boolean;
begin
  result := True;
  if AScreenObject.Count <> 1 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingObject,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
  if AScreenObject.ElevationCount <> ecZero then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrECountZero,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
  if not AScreenObject.SetValuesOfIntersectedCells then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrBecauseTheFollowin,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
  if Boundary.Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrNoTransientDataFo,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
  if Boundary.WellScreens.Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrNoWellScreensInT,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
  if AScreenObject.Segments[Model].Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingObjectGrid,
      AScreenObject.Name, AScreenObject);
    result := False;
  end;
end;

procedure TModflowMAW_Writer.Evaluate;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Boundary: TMawBoundary;
  Dummy: TStringList;
  MawItem: TMawItem;
  StartTime: Double;
  FirstItem: TMawItem;
  ItemIndex: Integer;
  EndTime: Double;
  Item1: TMawItem;
  Item2: TMawItem;
  NewItem: TMawItem;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingObject);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrECountZero);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoTransientDataFo);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoWellScreensInT);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrBecauseTheFollowin);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingObjectGrid);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMAWPackageSkipped);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMAWWellScreenInva);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMoreThanOneMAWSc);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMAWSkinRadiusLess);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFormulaErrorInMAW);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMAWWellScreensInv);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFlowingWellReducti);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMAWWellRadiusToo);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMAWWellSkinRadius);

  FFlowingWells := False;

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  EndTime := Model.ModflowFullStressPeriods.Last.Endtime;
//  StartTime := Model.ModflowStressPeriods.First.StartTime;

  FMawObjects.Clear;
  FMawNames.Clear;
  Dummy := TStringList.Create;
  try
    for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      AScreenObject := Model.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if not AScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := AScreenObject.ModflowMawBoundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;
      if not CheckOK(AScreenObject, Boundary) then
      begin
        Continue;
      end;

      frmProgressMM.AddMessage(Format(StrEvaluatingS, [AScreenObject.Name]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      MawItem := Boundary.Values.First as TMawItem;
      if MawItem.StartTime > StartTime then
      begin
        FirstItem := Boundary.Values.Insert(0) as TMawItem;
        FirstItem.Assign(MawItem);
        FirstItem.StartTime := StartTime;
        FirstItem.EndTime := MawItem.StartTime;
        FirstItem.MawStatus := mwInactive;
      end;

      MawItem := Boundary.Values.Last as TMawItem;
      if MawItem.Endtime < Endtime then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrMAWTimeExtended,
          Format(StrIn0sTheLastDe, [AScreenObject.Name, MawItem.Endtime,
          Endtime]), AScreenObject);
        MawItem.Endtime := Endtime;
      end;

      for ItemIndex := Boundary.Values.Count - 2 downto 0 do
      begin
        Item1 := Boundary.Values[ItemIndex] as TMawItem;
        Item2 := Boundary.Values[ItemIndex+1] as TMawItem;
        if Item1.EndTime < Item2.StartTime then
        begin
          NewItem := Boundary.Values.Add as TMawItem;
          NewItem.Assign(Item1);
          NewItem.MawStatus := mwInactive;;
          NewItem.Starttime := Item1.EndTime;
          NewItem.Endtime := Item2.Starttime;
          NewItem.Index := ItemIndex+1;
          frmErrorsAndWarnings.AddWarning(Model, StrInactiveMAWPeriod,
            Format(StrIn0sThereWasA,
            [AScreenObject.Name, NewItem.Starttime, NewItem.Endtime]), AScreenObject);
        end;
      end;

      Boundary.WellNumber := FMawObjects.Add(AScreenObject) + 1;
      FMawNames.AddObject(AScreenObject.Name, AScreenObject);
      Boundary.GetCellValues(Values, Dummy, Model, self);

      if not FFlowingWells then
      begin
        for ItemIndex := 0 to Boundary.Values.Count - 1 do
        begin
          MawItem := Boundary.Values[ItemIndex] as TMawItem;
          if MawItem.FlowingWell = fwFlowing then
          begin
            FFlowingWells := True;
            break;
          end;
        end;
      end;
      FTimeSeriesNames.AddStrings(Boundary.Mf6TimeSeriesNames);

    end;
  finally
    Dummy.Free;
  end;
  FMawNames.Sorted := True;

  AssignWellScreensAndWellProperties;
end;

class function TModflowMAW_Writer.Extension: string;
begin
  result := '.maw6';
end;

class function TModflowMAW_Writer.GwtObservationExtension: string;
begin
  result := '.ob_mwt';
end;

function TModflowMAW_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (MfObs.MawObs <> []);
end;

function TModflowMAW_Writer.IsMf6GwtObservation(
  AScreenObject: TScreenObject; SpeciesIndex: Integer): Boolean;
var
  MfObs: TModflow6Obs;
begin
  MfObs := AScreenObject.Modflow6Obs;
  Result := (MfObs <> nil) and MfObs.Used and (((MfObs.MwtObs <> [])
    and (SpeciesIndex in MfObs.Genus))
    or (MfObs.CalibrationObservations.MwtObs[SpeciesIndex] <> []) );
end;

class function TModflowMAW_Writer.ObservationExtension: string;
begin
  result := '.ob_maw';
end;

class function TModflowMAW_Writer.ObservationOutputExtension: string;
begin
  result := '.ob_maw_out';
end;

procedure TModflowMAW_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMAWPackage);
    Application.ProcessMessages;

    WriteTemplateHeader;

    WriteDataSet0;

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

    frmProgressMM.AddMessage(StrWritingMAWPackag);
    WritePackageData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingMAWConnec);
    Application.ProcessMessages;
    WriteConnectionData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingMAWStress);
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

procedure TModflowMAW_Writer.WriteGwtFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;

    WriteDataSet0;

    WriteGwtOptions;
    WriteGwtPackageData;
    WriteGwtStressPeriods;

  finally
    CloseFile;
  end;
end;

procedure TModflowMAW_Writer.WriteGwtOptions;
var
  ASpecies: TMobileChemSpeciesItem;
  budgetfile: string;
  BaseFileName: string;
  concentrationfile: string;
  budgetCsvFile: string;
  MawPackage: TMawPackage;
  NameOfFile: string;
begin
  WriteBeginOptions;
  try
    WriteString('    FLOW_PACKAGE_NAME ');
    WriteString(StrMAW1);
    NewLine;

    Assert(FSpeciesIndex >= 0);
    Assert(FSpeciesIndex < Model.MobileComponents.Count);
    WriteString('    FLOW_PACKAGE_AUXILIARY_NAME ');
    ASpecies := Model.MobileComponents[FSpeciesIndex];
    WriteString(' ' + ASpecies.Name);
    NewLine;

    WriteString('    BOUNDNAMES');
    NewLine;

    WriteTimeSeriesFiles(FInputFileName);

    PrintListInputOption;
    PrintConcentrationOption;
    PrintFlowsOption;
    WriteSaveFlowsOption;

    MawPackage := Model.ModflowPackages.MawPackage;
    BaseFileName := ChangeFileExt(FNameOfFile, '');
    BaseFileName := ChangeFileExt(BaseFileName, '') + '.' + ASpecies.Name;

    if MawPackage.SaveGwtConcentration then
    begin
      WriteString('    CONCENTRATION FILEOUT ');
      concentrationfile := BaseFileName + '.mwt_conc';
      Model.AddModelOutputFile(concentrationfile);
      concentrationfile := ExtractFileName(concentrationfile);
      WriteString(concentrationfile);
      NewLine;
    end;

    if MawPackage.SaveGwtBudget then
    begin
      WriteString('    BUDGET FILEOUT ');
      budgetfile := BaseFileName + '.mwt_budget';
      Model.AddModelOutputFile(budgetfile);
      budgetfile := ExtractFileName(budgetfile);
      WriteString(budgetfile);
      NewLine;
    end;

    if MawPackage.SaveGwtBudgetCsv then
    begin
      WriteString('    BUDGETCSV FILEOUT ');
      budgetCsvFile := BaseFileName + '.mwt_budget.csv';
      Model.AddModelOutputFile(budgetCsvFile);
      budgetCsvFile := ExtractFileName(budgetCsvFile);
      WriteString(budgetCsvFile);
      NewLine;
    end;

    if FGwtObservations[FSpeciesIndex].Count > 0 then
    begin
      WriteString('    OBS6 FILEIN ');
      NameOfFile := BaseFileName + GwtObservationExtension;
      Model.AddModelInputFile(NameOfFile);
      NameOfFile := ExtractFileName(NameOfFile);
      WriteString(NameOfFile);
      NewLine;
    end;

  //  [TS6 FILEIN <ts6_filename>]
  //  [OBS6 FILEIN <obs6_filename>]
  finally
    WriteEndOptions
  end;
end;

procedure TModflowMAW_Writer.WriteGwtPackageData;
var
  WellIndex: Integer;
  AWell: TMawSteadyWellRecord;
  BoundName: string;
begin
  WriteBeginPackageData;

  WriteString('# <mawno> <strt> [<boundname>]');
  NewLine;

  for WellIndex := 0 to Length(FWellProperties) - 1 do
  begin
    AWell := FWellProperties[WellIndex];
    WriteInteger(AWell.WellNumber);

    WriteFormulaOrValueBasedOnAPestName(
      AWell.StartingConcentrations.ValuePestNames[FSpeciesIndex],
      AWell.StartingConcentrations.Values[FSpeciesIndex],
      AWell.Layer, AWell.Row, AWell.Column);

    BoundName := Copy(AWell.BoundName, 1, MaxBoundNameLength);
    BoundName := ' ''' + BoundName + ''' ';
    WriteString(BoundName);

    NewLine;
  end;

  WriteEndPackageData;
end;

procedure TModflowMAW_Writer.WriteGwtStressPeriods;
var
  StressPeriodIndex: Integer;
  Cells: TValueCellList;
  CellIndex: Integer;
  ACell: TMawCell;
  MoverWriter: TModflowMvrWriter;
  MvrReceiver: TMvrReceiver;
  MvrSource: TMvrRegisterKey;
//  AScreenObject: TScreenObject;
  GwtStatus: TGwtBoundaryStatus;
  FormulaIndex: Integer;
begin
  if MvrWriter <> nil then
  begin
    MoverWriter := MvrWriter as TModflowMvrWriter;
  end
  else
  begin
    MoverWriter := nil;
  end;
  MvrReceiver.ReceiverKey.ReceiverPackage := rpcMaw;
  MvrReceiver.ReceiverValues.StreamCells := nil;
  MvrReceiver.ReceiverValues.StreamReachNumbers := nil;

  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(
      StrWritingMAWStre, [StressPeriodIndex+1]));

    MvrSource.StressPeriod := StressPeriodIndex;

    WriteBeginPeriod(StressPeriodIndex);
    WriteString('# <wellno> <<mwtsetting>>');
    NewLine;
    MvrReceiver.ReceiverKey.StressPeriod := StressPeriodIndex;

    Cells := Values[StressPeriodIndex];

    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TMawCell;

      MvrReceiver.ReceiverKey.ScreenObject :=
        ACell.MawBoundary.ScreenObject as TScreenObject;
      MvrReceiver.ReceiverValues.Index := ACell.WellNumber;
      if (MoverWriter <> nil) and not WritingTemplate then
      begin
        MoverWriter.AddMvrReceiver(MvrReceiver);
      end;

      WriteInteger(ACell.WellNumber);

      case ACell.MawStatus of
        mwInactive:
          begin
            GwtStatus := gbsInactive;
          end;
        mwActive, mwConstantHead:
          begin
            GwtStatus := ACell.GwtStatus[FSpeciesIndex];
          end;
        else
          begin
            GwtStatus := gbsInactive;
            Assert(False);
          end;
      end;
      WriteString(' STATUS');
      case GwtStatus of
        gbsInactive:
          begin
            WriteString(' INACTIVE');
          end;
        gbsActive:
          begin
            WriteString(' ACTIVE');
          end;
        gbsConstant:
          begin
            WriteString(' CONSTANT');
          end;
        else
          Assert(False);
      end;
      NewLine;

      case GwtStatus of
        gbsActive:
          begin
            WriteInteger(ACell.WellNumber);
            WriteString(' RATE');
            FormulaIndex := MawGwtStart
              + MawGwtConcCount*FSpeciesIndex + MawGwtInjectionConcentrationsPosition;
//              + MawGwtConcCount*MawGwtInjectionConcentrationsPosition + FSpeciesIndex;
            WriteValueOrFormula(ACell, FormulaIndex);
            NewLine;
          end;
        gbsInactive:
          begin
            // do nothing
          end;
        gbsConstant:
          begin
            WriteInteger(ACell.WellNumber);
            WriteString(' CONCENTRATION');
            FormulaIndex := MawGwtStart
              + MawGwtConcCount*FSpeciesIndex + MawGwtSpecifiedConcentrationPosition;
//              + MawGwtConcCount*MawGwtSpecifiedConcentrationPosition + FSpeciesIndex;
            WriteValueOrFormula(ACell, FormulaIndex);
            NewLine;
          end;
        else
          Assert(False);
      end;
    end;

    WriteEndPeriod;
  end;
end;

procedure TModflowMAW_Writer.WriteMwtFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  SpeciesName: string;
  Abbreviation: string;
  ObsWriter: TMwtObsWriter;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Abbreviation := 'MWT6';
  end
  else
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  if not Model.MobileComponents[SpeciesIndex].UsedForGWT then
  begin
    Exit;
  end;
  FPestParamUsed := False;
  FSpeciesIndex :=  SpeciesIndex;
  SpeciesName := Model.MobileComponents[FSpeciesIndex].Name;
  FNameOfFile := ChangeFileExt(AFileName, '') + '.' + SpeciesName + '.mwt';
  FInputFileName := FNameOfFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  frmErrorsAndWarnings.BeginUpdate;
  try
    WriteGwtFileInternal;

    if FGwtObservations[SpeciesIndex].Count > 0 then
    begin
      ObsWriter := TMwtObsWriter.Create(Model, etExport,
        FGwtObservations[SpeciesIndex], SpeciesIndex);
      try
        ObsWriter.WriteFile(ChangeFileExt(FNameOfFile, GwtObservationExtension));
      finally
        ObsWriter.Free;
      end;
    end;

    if  Model.PestUsed and FPestParamUsed then
    begin
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteGwtFileInternal;
    end;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

end;

function TModflowMAW_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowMAW_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.MawPackage;
end;

procedure TModflowMAW_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  WellIndex: integer;
  DataSets: TList;
  UsedIndicies: TByteSet;
  TimeIndex: Integer;
  DataTypeIndex: Integer;
  TimeListIndex: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  DataSetIndex: Integer;
  WellElevationTimeList: TModflowBoundaryDisplayTimeList;
  WellList: TValueCellList;
  SpeciesIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

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

      WellElevationTimeList := TimeLists[0];
      for TimeIndex := 0 to WellElevationTimeList.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for WellIndex := 0 to Values.Count - 1 do
        begin
          WellList := Values[WellIndex];// as TValueCellList;
          UsedIndicies := [];
          for DataTypeIndex := MawFlowingWellElevationPosition to
            MawFlowingWellReductionLengthPosition do
          begin
//            if Boundary.DataTypeUsed(DataTypeIndex) then
            begin
              Include(UsedIndicies, DataTypeIndex);
            end;
          end;

          if Model.GwtUsed then
          begin
            DataTypeIndex := MawGwtStart;
            for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
            begin
              Include(UsedIndicies, DataTypeIndex);
              Inc(DataTypeIndex);
              Include(UsedIndicies, DataTypeIndex);
              Inc(DataTypeIndex);
            end;
          end;

          UpdateCellDisplay(WellList, DataSets, [], nil, UsedIndicies);
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          DataArray := DataSets[DataSetIndex];
          DataArray.UpToDate := True;
          DataArray.CacheData;
        end;
      end;
      SetTimeListsUpToDate(TimeLists);
    finally
      DataSets.Free;
      (Model as TPhastModel).InvalidateAllDynamicLists;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TModflowMAW_Writer.UpdateSteadyData;
var
  RadiusDataArray: TModflowBoundaryDisplayDataArray;
  WellIndex: Integer;
  AWell: TMawSteadyWellRecord;
  WellConnection: TMawSteadyConnectionRecord;
  ConnectionIndex: Integer;
  Index: Integer;
  BottomDataArray: TModflowBoundaryDisplayDataArray;
  InitialHeadDataArray: TModflowBoundaryDisplayDataArray;
  ScreenTopArray: TModflowBoundaryDisplayDataArray;
  ScreenBottomArray: TModflowBoundaryDisplayDataArray;
  SkinKArray: TModflowBoundaryDisplayDataArray;
  SkinRadiusArray: TModflowBoundaryDisplayDataArray;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  Model.UpdateModflowFullStressPeriods;

  Model.ModflowFullStressPeriods.BeginUpdate;
  try
    for Index := Model.ModflowFullStressPeriods.Count - 1 downto 1 do
    begin
      Model.ModflowFullStressPeriods.Delete(Index);
    end;
  finally
    Model.ModflowFullStressPeriods.EndUpdate;
  end;

  Evaluate;


  RadiusDataArray := Model.DataArrayManager.GetDataSetByName(KMAWRadius)
    as TModflowBoundaryDisplayDataArray;
  if RadiusDataArray <> nil then
  begin
    RadiusDataArray.Clear;

    if Length(FWellProperties) > 0 then
    begin
      WellIndex := 0;
      AWell := FWellProperties[WellIndex];
      for ConnectionIndex := 0 to FWellConnections.Count - 1 do
      begin
        WellConnection := FWellConnections[ConnectionIndex];
        if AWell.WellNumber <> WellConnection.WellNumber then
        begin
          Inc(WellIndex);
          AWell := FWellProperties[WellIndex];
          Assert(AWell.WellNumber = WellConnection.WellNumber);
        end;
        RadiusDataArray.AddDataValue(AWell.RadiusAnnotation,
          AWell.Radius, WellConnection.Cell.Column,
          WellConnection.Cell.Row, WellConnection.Cell.Layer);
      end;

      RadiusDataArray.ComputeAverage;
    end;
    RadiusDataArray.UpToDate := True;
  end;

  BottomDataArray := Model.DataArrayManager.GetDataSetByName(KMAWBottom)
    as TModflowBoundaryDisplayDataArray;
  if BottomDataArray <> nil then
  begin
    BottomDataArray.Clear;

    if Length(FWellProperties) > 0 then
    begin
      WellIndex := 0;
      AWell := FWellProperties[WellIndex];
      for ConnectionIndex := 0 to FWellConnections.Count - 1 do
      begin
        WellConnection := FWellConnections[ConnectionIndex];
        if AWell.WellNumber <> WellConnection.WellNumber then
        begin
          Inc(WellIndex);
          AWell := FWellProperties[WellIndex];
          Assert(AWell.WellNumber = WellConnection.WellNumber);
        end;
        BottomDataArray.AddDataValue(AWell.BottomAnnotation,
          AWell.Bottom, WellConnection.Cell.Column,
          WellConnection.Cell.Row, WellConnection.Cell.Layer);
      end;

      BottomDataArray.ComputeAverage;
    end;
    BottomDataArray.UpToDate := True;
  end;

  InitialHeadDataArray := Model.DataArrayManager.GetDataSetByName(KMAWInitialHead)
    as TModflowBoundaryDisplayDataArray;
  if InitialHeadDataArray <> nil then
  begin
    InitialHeadDataArray.Clear;

    if Length(FWellProperties) > 0 then
    begin
      WellIndex := 0;
      AWell := FWellProperties[WellIndex];
      for ConnectionIndex := 0 to FWellConnections.Count - 1 do
      begin
        WellConnection := FWellConnections[ConnectionIndex];
        if AWell.WellNumber <> WellConnection.WellNumber then
        begin
          Inc(WellIndex);
          AWell := FWellProperties[WellIndex];
          Assert(AWell.WellNumber = WellConnection.WellNumber);
        end;
        InitialHeadDataArray.AddDataValue(AWell.StartingHeadAnnotation,
          AWell.StartingHead, WellConnection.Cell.Column,
          WellConnection.Cell.Row, WellConnection.Cell.Layer);
      end;

      InitialHeadDataArray.ComputeAverage;
    end;
    InitialHeadDataArray.UpToDate := True;
  end;

  ScreenTopArray := Model.DataArrayManager.GetDataSetByName(KMAWScreenTop)
    as TModflowBoundaryDisplayDataArray;
  if ScreenTopArray <> nil then
  begin
    ScreenTopArray.Clear;

    for ConnectionIndex := 0 to FWellConnections.Count - 1 do
    begin
      WellConnection := FWellConnections[ConnectionIndex];
      ScreenTopArray.AddDataValue(WellConnection.ScreenTopAnnotation,
        WellConnection.ScreenTop, WellConnection.Cell.Column,
        WellConnection.Cell.Row, WellConnection.Cell.Layer);
    end;

    ScreenTopArray.ComputeAverage;
    ScreenTopArray.UpToDate := True;
  end;

  ScreenBottomArray := Model.DataArrayManager.GetDataSetByName(KMAWScreenBottom)
    as TModflowBoundaryDisplayDataArray;
  if ScreenBottomArray <> nil then
  begin
    ScreenBottomArray.Clear;

    for ConnectionIndex := 0 to FWellConnections.Count - 1 do
    begin
      WellConnection := FWellConnections[ConnectionIndex];
      ScreenBottomArray.AddDataValue(WellConnection.ScreenBottomAnnotation,
        WellConnection.ScreenBottom, WellConnection.Cell.Column,
        WellConnection.Cell.Row, WellConnection.Cell.Layer);
    end;

    ScreenBottomArray.ComputeAverage;
    ScreenBottomArray.UpToDate := True;
  end;

  SkinKArray := Model.DataArrayManager.GetDataSetByName(KMAWSkinK)
    as TModflowBoundaryDisplayDataArray;
  if SkinKArray <> nil then
  begin
    SkinKArray.Clear;

    for ConnectionIndex := 0 to FWellConnections.Count - 1 do
    begin
      WellConnection := FWellConnections[ConnectionIndex];
      SkinKArray.AddDataValue(WellConnection.SkinKAnnotation,
        WellConnection.SkinK, WellConnection.Cell.Column,
        WellConnection.Cell.Row, WellConnection.Cell.Layer);
    end;

    SkinKArray.ComputeAverage;
    SkinKArray.UpToDate := True;
  end;

  SkinRadiusArray := Model.DataArrayManager.GetDataSetByName(KMAWSkinRadius)
    as TModflowBoundaryDisplayDataArray;
  if SkinRadiusArray <> nil then
  begin
    SkinRadiusArray.Clear;

    for ConnectionIndex := 0 to FWellConnections.Count - 1 do
    begin
      WellConnection := FWellConnections[ConnectionIndex];
      SkinRadiusArray.AddDataValue(WellConnection.SkinRadiusAnnotation,
        WellConnection.SkinRadius, WellConnection.Cell.Column,
        WellConnection.Cell.Row, WellConnection.Cell.Layer);
    end;

    SkinRadiusArray.ComputeAverage;
    SkinRadiusArray.UpToDate := True;
  end;

end;

procedure TModflowMAW_Writer.WriteAdditionalAuxVariables;
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
end;

procedure TModflowMAW_Writer.WriteConnectionData;
var
  ConnectionIndex: Integer;
  AWellConnection: TMawSteadyConnectionRecord;
  ExistingConnections: array of array of array of String;
  ExistingName: string;
  ObjectIndex: Integer;
  ASCreenObject: TScreenObject;
begin
  SetLength(ExistingConnections, Model.LayerCount, Model.RowCount,
    Model.ColumnCount);

  WriteBeginConnectionData;
  WriteString('# <wellno> <icon> <cellid(ncelldim)> <scrn_top> <scrn_bot> <hk_skin> <radius_skin>');
  NewLine;

  for ConnectionIndex := 0 to FWellConnections.Count - 1 do
  begin
    AWellConnection := FWellConnections[ConnectionIndex];

    ObjectIndex := FMawNames.IndexOf(AWellConnection.ScreenObjectName);
    Assert(ObjectIndex >= 0);
    ASCreenObject := FMawNames.Objects[ObjectIndex] as TScreenObject;
    ExistingName := ExistingConnections[AWellConnection.Cell.Layer,
      AWellConnection.Cell.Row,
      AWellConnection.Cell.Column];
    if ExistingName = AWellConnection.ScreenObjectName then
    begin
      if (ASCreenObject.ModflowMawBoundary.ConductanceMethod <> mcmMean) then
      begin
        frmErrorsAndWarnings.AddError(Model, StrMoreThanOneMAWSc,
          Format(StrTheObject0sDefi, [ExistingName,
          AWellConnection.Cell.Layer+1,
          AWellConnection.Cell.Row+1,
          AWellConnection.Cell.Column+1]), ASCreenObject);
      end;
    end;
    ExistingConnections[AWellConnection.Cell.Layer,
    AWellConnection.Cell.Row,
    AWellConnection.Cell.Column] := AWellConnection.ScreenObjectName;

    WriteInteger(AWellConnection.WellNumber);
    WriteInteger(AWellConnection.ConnectionNumber);
    WriteInteger(AWellConnection.Cell.Layer+1);
    if not Model.DisvUsed then
    begin
      WriteInteger(AWellConnection.Cell.Row+1);
    end;
    WriteInteger(AWellConnection.Cell.Column+1);

    WriteFormulaOrValueBasedOnAPestName(AWellConnection.ScreenTopPestName,
      AWellConnection.ScreenTop, AWellConnection.Cell.Layer,
      AWellConnection.Cell.Row, AWellConnection.Cell.Column);
    WriteFormulaOrValueBasedOnAPestName(AWellConnection.ScreenBottomPestName,
      AWellConnection.ScreenBottom, AWellConnection.Cell.Layer,
      AWellConnection.Cell.Row, AWellConnection.Cell.Column);
    WriteFormulaOrValueBasedOnAPestName(AWellConnection.SkinKPestName,
      AWellConnection.SkinK, AWellConnection.Cell.Layer,
      AWellConnection.Cell.Row, AWellConnection.Cell.Column);
    WriteFormulaOrValueBasedOnAPestName(AWellConnection.SkinRadiusPestName,
      AWellConnection.SkinRadius, AWellConnection.Cell.Layer,
      AWellConnection.Cell.Row, AWellConnection.Cell.Column);

//    WriteFloat(AWellConnection.ScreenTop);
//    WriteFloat(AWellConnection.ScreenBottom);
//    WriteFloat(AWellConnection.SkinK);
//    WriteFloat(AWellConnection.SkinRadius);
    NewLine;
  end;

  WriteEndConnectionData;
end;

procedure TModflowMAW_Writer.WriteDimensions;
var
  NMAWWELLS: Integer;
begin
  NMAWWELLS := Length(FWellProperties);
  WriteBeginDimensions;
  WriteString('  NMAWWELLS');
  WriteInteger(NMAWWELLS);
  NewLine;
  WriteEndDimensions;
end;

procedure TModflowMAW_Writer.WriteFile(const AFileName: string);
var
  ObsWriter: TMawObsWriter;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrMAW6) then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMAWStartingHeadLe);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMAWPackageSkipped);


  frmProgressMM.AddMessage(StrEvaluatingMAWPacka);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  Evaluate;

  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrMAWPackageSkipped,
      StrTheMultiaquiferWe);
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrMAW6, 0, FNameOfFile, foInput, Model, False, StrMAW1);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FInputFileName := FNameOfFile;
  WriteFileInternal;

  if FMawObservations.Count > 0 then
  begin
    frmProgressMM.AddMessage(StrWritingMAWObservat);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ObsWriter := TMawObsWriter.Create(Model, etExport, FMawObservations);
    try
      ObsWriter.WriteFile(ChangeFileExt(FNameOfFile, ObservationExtension));
    finally
      ObsWriter.Free;
    end;
  end;

  if Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TModflowMAW_Writer.WriteOptions;
var
  AFileName: string;
  NameOfFile: string;
  BaseName: string;
begin
  WriteBeginOptions;

  if Model.BuoyancyUsed then
  begin
    if Model.BuoyancyDensityUsed then
    begin
      WriteString('  AUXILIARY DENSITY');
      NewLine;
    end
    else
    begin
      WriteString('  AUXILIARY');
      WriteAdditionalAuxVariables;
      NewLine;
    end;
  end;

  WriteBoundNamesOption;

  PrintListInputOption;

  if  FMawPackage.PrintHead then
  begin
    WriteString('  PRINT_HEAD');
    NewLine;
  end;

  BaseName := ChangeFileExt(FNameOfFile, '');
  if  FMawPackage.SaveMnwHeads then
  begin
    WriteString('  HEAD FILEOUT ');
    AFileName := ChangeFileExt(BaseName, '.maw_head');
    Model.AddModelOutputFile(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
    NewLine;
  end;

  WriteTimeSeriesFiles(FInputFileName);

  PrintFlowsOption;
  WriteSaveFlowsOption;

  if  FMawPackage.SaveMnwFlows or Model.SeparateGwtUsed then
  begin
    WriteString('  BUDGET FILEOUT ');
    AFileName := ChangeFileExt(BaseName, StrMawbud);
    Model.AddModelOutputFile(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
    NewLine;
  end;

  if not FMawPackage.IncludeWellStorage then
  begin
    WriteString('  NO_WELL_STORAGE');
    NewLine;
  end;

  if FFlowingWells then
  begin
    WriteString('  FLOWING_WELLS');
    NewLine;
  end;

  WriteString('  SHUTDOWN_THETA');
  WriteFloat(FMawPackage.ShutDownTheta);
  NewLine;

  WriteString('  SHUTDOWN_KAPPA');
  WriteFloat(FMawPackage.ShutDownKappa);
  NewLine;

  if FMawObservations.Count > 0 then
  begin
    WriteString('    OBS6 FILEIN ');
    NameOfFile := ObservationFileName(BaseName);
    Model.AddModelInputFile(NameOfFile);
    NameOfFile := ExtractFileName(NameOfFile);
    WriteString(NameOfFile);
    NewLine;
  end;

  if Model.ModflowPackages.MvrPackage.IsSelected then
  begin
    if (MvrWriter <> nil) then
    begin
      if spcMaw in TModflowMvrWriter(MvrWriter).UsedPackages then
      begin
        WriteString('  MOVER');
        NewLine
      end;
    end;
  end;

//  WriteGwtlAuxVariables;

  WriteEndOptions
end;

procedure TModflowMAW_Writer.WritePackageData;
var
  WellIndex: Integer;
  AWell: TMawSteadyWellRecord;
  BoundName: string;
  SpeciesIndex: Integer;
begin
  WriteBeginPackageData;

  WriteString('# <wellno> <radius> <bottom> <strt> <condeqn> <ngwfnodes> [<aux(naux)>] [<boundname>]');
  NewLine;

  for WellIndex := 0 to Length(FWellProperties) - 1 do
  begin
    AWell := FWellProperties[WellIndex];
    WriteInteger(AWell.WellNumber);

    if AWell.StartingHead < AWell.Bottom then
    begin
      frmErrorsAndWarnings.AddError(Model, StrMAWStartingHeadLe,
        Format(StrMAWStartingHeadTo, [AWell.Layer+1,
        AWell.Row + 1, AWell.Column + 1]))
    end;

    WriteFormulaOrValueBasedOnAPestName(AWell.RadiusPestName,
      AWell.Radius, AWell.Layer, AWell.Row,
      AWell.Column);
    WriteFormulaOrValueBasedOnAPestName(AWell.BottomPestName,
      AWell.Bottom, AWell.Layer, AWell.Row,
      AWell.Column);
    WriteFormulaOrValueBasedOnAPestName(AWell.StartingHeadPestName,
      AWell.StartingHead, AWell.Layer, AWell.Row,
      AWell.Column);

//    WriteFloat(AWell.Radius);
//    WriteFloat(AWell.Bottom);
//    WriteFloat(AWell.StartingHead);

    case AWell.ConductanceMethod of
      mcmSpecified:
        begin
          WriteString(' SPECIFIED');
        end;
      mcmTheim, mcmThiem:
        begin
          WriteString(' THIEM');
        end;
      mcmSkin:
        begin
          WriteString(' SKIN');
        end;
      mcmCumulative:
        begin
          WriteString(' CUMULATIVE');
        end;
      mcmMean:
        begin
          WriteString(' MEAN');
        end;
      else
        Assert(False);
    end;
    WriteInteger(AWell.CellCount);

    if Model.BuoyancyUsed then
    begin
      if Model.BuoyancyDensityUsed then
      begin
        WriteFloat(0);
      end
      else if Model.GwtUsed then
      begin
        for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
        begin
          WriteFloat(0);
        end;
      end;
    end;

    BoundName := Copy(AWell.BoundName, 1, MaxBoundNameLength);
    BoundName := ' ''' + BoundName + ''' ';
    WriteString(BoundName);

    NewLine;
  end;

  WriteEndPackageData;
end;

procedure TModflowMAW_Writer.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  Cells: TValueCellList;
  CellIndex: Integer;
  ACell: TMawCell;
  MoverWriter: TModflowMvrWriter;
  MvrReceiver: TMvrReceiver;
  MvrSource: TMvrRegisterKey;
  AScreenObject: TScreenObject;
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
begin
  if MvrWriter <> nil then
  begin
    MoverWriter := MvrWriter as TModflowMvrWriter;
  end
  else
  begin
    MoverWriter := nil;
  end;
  MvrReceiver.ReceiverKey.ReceiverPackage := rpcMaw;
  MvrReceiver.ReceiverValues.StreamCells := nil;
  MvrReceiver.ReceiverValues.StreamReachNumbers := nil;

  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(
      StrWritingMAWStre, [StressPeriodIndex+1]));

    MvrSource.StressPeriod := StressPeriodIndex;

    WriteBeginPeriod(StressPeriodIndex);
    WriteString('# <wellno> <mawsetting>');
    NewLine;
    MvrReceiver.ReceiverKey.StressPeriod := StressPeriodIndex;

    Cells := Values[StressPeriodIndex];

    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TMawCell;

      MvrReceiver.ReceiverKey.ScreenObject :=
        ACell.MawBoundary.ScreenObject as TScreenObject;
      MvrReceiver.ReceiverValues.Index := ACell.WellNumber;
      if (MoverWriter <> nil) and not WritingTemplate then
      begin
        MoverWriter.AddMvrReceiver(MvrReceiver);
      end;

      WriteInteger(ACell.WellNumber);
      WriteString(' STATUS');
      case ACell.MawStatus of
        mwActive:
          begin
            WriteString(' ACTIVE');
          end;
        mwInactive:
          begin
            WriteString(' INACTIVE,');
          end;
        mwConstantHead:
          begin
            WriteString(' CONSTANT');
          end;
      end;
      NewLine;

      case ACell.FlowingWell of
        fwNotFlowing:
          begin
            // do nothing
          end;
        fwFlowing:
          begin
            WriteInteger(ACell.WellNumber);
            WriteString(' FLOWING_WELL');
            WriteValueOrFormula(ACell, MawFlowingWellElevationPosition);
//            WriteFloat(ACell.FlowingWellElevation);

            WriteValueOrFormula(ACell, MawFlowingWellConductancePosition);
//            WriteFloat(ACell.FlowingWellConductance);

            WriteValueOrFormula(ACell, MawFlowingWellReductionLengthPosition);
//            WriteFloat(ACell.FlowingWellReductionLength);
            if ACell.FlowingWellReductionLength <= 0 then
            begin
              AScreenObject := ACell.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model, StrFlowingWellReducti,
                Format(StrIn0sInStressPe,
                [AScreenObject.Name, StressPeriodIndex+1]),
                AScreenObject);
            end;
            NewLine;
          end;
      end;

      if ACell.MawStatus <> mwInactive then
      begin
        WriteInteger(ACell.WellNumber);
        WriteString(' RATE');
        WriteValueOrFormula(ACell, MawRatePosition);
//        WriteFloat(ACell.Rate);
        NewLine;
      end;

        // Formerly, head was exported for inactive cells.
        // Was this to compensate for a bug in MODFLOW that has now been fixed?
//      if ACell.MawStatus in [mwInactive, mwConstantHead] then
      if ACell.MawStatus = mwConstantHead then
      begin
        WriteInteger(ACell.WellNumber);
        WriteString(' WELL_HEAD');
        WriteValueOrFormula(ACell, MawWellHeadPosition);
//        WriteFloat(ACell.WellHead);
        NewLine;
      end;

      if ACell.HeadLimitChoice then
      begin
        WriteInteger(ACell.WellNumber);
        WriteString(' HEAD_LIMIT');
        WriteValueOrFormula(ACell, MawHeadLimitPosition);
//        WriteFloat(ACell.HeadLimit);
        NewLine;
      end;

      if ACell.ShutOff then
      begin
        WriteInteger(ACell.WellNumber);
        WriteString(' SHUT_OFF');
        WriteValueOrFormula(ACell, MawMinRatePosition);
//        WriteFloat(ACell.MinRate);
        WriteValueOrFormula(ACell, MawMaxRatePosition);
//        WriteFloat(ACell.MaxRate);
        NewLine;
      end;

      if ACell.RateScaling then
      begin
        WriteInteger(ACell.WellNumber);
        WriteString(' RATE_SCALING');
        WriteValueOrFormula(ACell, MawPumpElevationPosition);
//        WriteFloat(ACell.PumpElevation);
        WriteValueOrFormula(ACell, MawScalingLengthPosition);
//        WriteFloat(ACell.ScalingLength);
        NewLine;
      end;

      if Model.BuoyancyUsed then
      begin
        if Model.BuoyancyDensityUsed then
        begin
          WriteString('  ');
          WriteInteger(ACell.WellNumber);
          WriteString('  AUXILIARY Density');
          WriteValueOrFormula(ACell, MawDensityPosition);
          NewLine;
        end
        else
        begin
          if Model.GwtUsed then
          begin
            for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
            begin
              WriteString('  ');
              WriteInteger(ACell.WellNumber);
              WriteString('  AUXILIARY ');
              ASpecies := Model.MobileComponents[SpeciesIndex];
              WriteString(' ' + ASpecies.Name);
              WriteFloat(0);
              NewLine;
            end;
          end;
        end;
      end;

      if ACell.MvrUsed and (MvrWriter <> nil)
        and (ACell.MawStatus <> mwInactive) and not WritingTemplate then
      begin
        MvrSource.Index := ACell.WellNumber;
        MvrSource.SourceKey.MvrIndex := ACell.MvrIndex;
        MvrSource.SourceKey.ScreenObject := ACell.MawBoundary.ScreenObject as TScreenObject;
        MoverWriter.AddMvrSource(MvrSource);
      end;
    end;

    WriteEndPeriod;
  end;
end;

procedure TModflowMAW_Writer.AssignWellScreensAndWellProperties;
var
  AScreenObject: TScreenObject;
  Boundary: TMawBoundary;
  UsedVariables: TStringList;
  Compiler: TRbwParser;
  CellList: TValueCellList;
  WellIndex: Integer;
  AWell: TMawCell;
  Expression: TExpression;
  AWellRecord: TMawSteadyWellRecord;
  Formula: string;
  AWellConnection: TMawSteadyConnectionRecord;
  ScreenIndex: Integer;
  AWellScreen: TMawWellScreenItem;
  Cell: TCellLocation;
  Grid: TCustomModelGrid;
  LayerIndex: Integer;
  CellTop: double;
  CellBottom: double;
  Mesh: IMesh3D;
  AWellSteady: TMawSteadyWellRecord;
  MfObs: TModflow6Obs;
  Obs: TMawObservation;
  ConnectionFound: Boolean;
  ValidScreensFound: Boolean;
  IDomainArray: TDataArray;
  CellSize: Double;
  PestParamName: string;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  SpeciesCount: Integer;
  StartingConcentrations: TStringConcCollection;
  SpeciesIndex: Integer;
  Species: TMobileChemSpeciesItem;
  PropertyFormula: string;
  FormulaItem: TStringConcValueItem;
  PropertyName: string;
  IsGwtObs: Boolean;
//  index: Integer;
  IsMf6Obs: Boolean;
  MwtObs: TMwtObservation;
  procedure CompileFormula(Formula: string; const FormulaName: string;
    var OutFormula: string; out PestParamName: string;
    out Column, Row, Layer: Integer; SpecifiedLayer: Integer = 0);
  var
    VarIndex: Integer;
    VarName: string;
    VarPosition: Integer;
    Variable: TCustomValue;
    AnotherDataSet: TDataArray;
//    Column: Integer;
//    Row: Integer;
//    Layer: Integer;
    ASegment: TCellElementSegment;
    Param: TModflowSteadyParameter;
    DataArray: TDataArray;
  begin
    Param := Model.GetPestParameterByName(Formula);
    if Param <> nil then
    begin
      Param.IsUsedInTemplate := True;
      Formula := FortranFloatToStr(Param.Value);
      PestParamName := Param.ParameterName;
    end
    else
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(Formula);
      if (DataArray <> nil) and DataArray.PestParametersUsed then
      begin
        PestParamName := DataArray.Name;
      end
      else
      begin
        PestParamName := '';
      end;
    end;

    OutFormula := Formula;
    try
      Compiler.Compile(OutFormula);
    except
      on E: ErbwParserError do
      begin
        frmFormulaErrors.AddFormulaError(AScreenObject.Name, FormulaName,
          OutFormula, E.Message);
        frmErrorsAndWarnings.AddError(Model, StrFormulaErrorInMAW,
          Format(StrThereWasAnErrorI, [AScreenObject.Name]) , AScreenObject);
        OutFormula := '0';
        Compiler.Compile(OutFormula);
      end;
    end;
    UsedVariables.Clear;
    Expression := Compiler.CurrentExpression;
    AScreenObject.InitilizeVariablesWithNilDataSet(Expression, Model, nil, UsedVariables, Compiler);
    for VarIndex := 0 to UsedVariables.Count - 1 do
    begin
      VarName := UsedVariables[VarIndex];
      VarPosition := Compiler.IndexOfVariable(VarName);
      if VarPosition >= 0 then
      begin
        Variable := Compiler.Variables[VarPosition];
        AnotherDataSet := Model.DataArrayManager.GetDataSetByName(VarName);
        if AnotherDataSet <> nil then
        begin
          Column := AWell.Column;
          Row := AWell.Row;
          Layer := SpecifiedLayer;
          Assert(AnotherDataSet.DataType = Variable.ResultType);
          if AnotherDataSet.Orientation = dsoTop then
          begin
            Layer := 0;
          end;
          if AnotherDataSet.Orientation = dsoFront then
          begin
            Row := 0;
          end;
          if AnotherDataSet.Orientation = dsoSide then
          begin
            Column := 0;
          end;
          case Variable.ResultType of
            rdtDouble:
              begin
                TRealVariable(Variable).Value := AnotherDataSet.RealData[Layer, Row, Column];
              end;
            rdtInteger:
              begin
                TIntegerVariable(Variable).Value := AnotherDataSet.IntegerData[Layer, Row, Column];
              end;
            rdtBoolean:
              begin
                TBooleanVariable(Variable).Value := AnotherDataSet.BooleanData[Layer, Row, Column];
              end;
            rdtString:
              begin
                TStringVariable(Variable).Value := AnotherDataSet.StringData[Layer, Row, Column];
              end;
          else
            Assert(False);
          end;
        end;
      end;
    end;
    UpdateCurrentModel(Model);
    Column := AWell.Column;
    Row := AWell.Row;
    Layer := SpecifiedLayer;
    UpdateGlobalLocations(Column, Row, Layer, eaBlocks, Model);
    UpdateCurrentScreenObject(AScreenObject);
    ASegment := AScreenObject.Segments[Model][0];
    UpdateCurrentSegment(ASegment);
  end;
  function LayerCount: integer;
  begin
    if Grid <> nil then
    begin
      result := Grid.LayerCount;
    end
    else
    begin
      Assert(Mesh <> nil);
      result := Mesh.LayerCount;
    end;
  end;
  function GetCellTop(const Column, Row, Layer: integer): double;
  begin
    if Grid <> nil then
    begin
      result := Grid.CellElevation[Column, Row, Layer];
    end
    else
    begin
      Assert(Mesh <> nil);
      result := Mesh.ElementArrayI[Layer, Column].UpperElevation;
    end;
  end;
  function GetCellBottom(const Column, Row, Layer: integer): double;
  begin
    if Grid <> nil then
    begin
      result := Grid.CellElevation[Column, Row, Layer+1];
    end
    else
    begin
      Assert(Mesh <> nil);
      result := Mesh.ElementArrayI[Layer, Column].LowerElevation;
    end;
  end;
begin
  // evaluate well screens and well steady data.
  FWellConnections.Capacity := FMawObjects.Count;
  Grid := Model.Grid;
  Mesh := Model.Mesh3D;
  IDomainArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);

  UsedVariables := TStringList.Create;
  try
    Compiler := Model.GetCompiler(dsoTop, eaBlocks);
    if Values.Count > 0 then
    begin
      CellList := Values[0];
      Assert(CellList.Count = FMawObjects.Count);
      SetLength(FWellProperties, CellList.Count);
      for WellIndex := 0 to CellList.Count - 1 do
      begin
        AWell := CellList[WellIndex] as TMawCell;
        AScreenObject := FMawObjects[WellIndex];
        Boundary := AScreenObject.ModflowMawBoundary;

        AWellRecord.WellNumber := Boundary.WellNumber;
        AWellRecord.ScreenObjectName := AScreenObject.Name;

        CompileFormula(Boundary.Radius, StrMAWRadius, Formula, PestParamName,
         Column, Row, Layer);
        Expression.Evaluate;
        AWellRecord.Radius := Expression.DoubleResult;
        AWellRecord.RadiusAnnotation := Format(
          StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
        AWellRecord.RadiusPestName := PestParamName;
        AWellRecord.Column := Column;
        AWellRecord.Row := Row;
        AWellRecord.Layer := 0;

        CompileFormula(Boundary.Bottom, StrMAWBottom, Formula, PestParamName,
          Column, Row, Layer);
        Expression.Evaluate;
        AWellRecord.Bottom := Expression.DoubleResult;
        AWellRecord.BottomAnnotation := Format(
          StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
        AWellRecord.BottomPestName := PestParamName;

        CompileFormula(Boundary.InitialHead, StrMAWInitialHead, Formula,
          PestParamName, Column, Row, Layer);
        Expression.Evaluate;
        AWellRecord.StartingHead := Expression.DoubleResult;
        AWellRecord.StartingHeadAnnotation := Format(
          StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
        AWellRecord.StartingHeadPestName := PestParamName;

        AWellRecord.ConductanceMethod := Boundary.ConductanceMethod;

        AWellRecord.BoundName := AScreenObject.Name;

        SpeciesCount := 0;
        if Model.GwtUsed then
        begin
          SpeciesCount := Model.MobileComponents.Count;
          AWellRecord.StartingConcentrations.SpeciesCount := SpeciesCount;
          StartingConcentrations := Boundary.StartingConcentrations;
          while StartingConcentrations.Count < Model.MobileComponents.Count do
          begin
            FormulaItem := StartingConcentrations.Add;
            FormulaItem.Value := '0.';
          end;

          for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
          begin
            Species := Model.MobileComponents[SpeciesIndex];
            PropertyName := Format('StartingConcentration_%s', [Species.Name]);
            PropertyFormula := StartingConcentrations[SpeciesIndex].Value;
            if PropertyFormula = '' then
            begin
              PropertyFormula := '0'; 
            end;

            CompileFormula(PropertyFormula, PropertyName, Formula,
              PestParamName, Column, Row, Layer);
            Expression.Evaluate;
            AWellRecord.StartingConcentrations.
              Values[SpeciesIndex] := Expression.DoubleResult;
            AWellRecord.StartingConcentrations.
              ValueAnnotations[SpeciesIndex] := AScreenObject.IntersectAnnotation(Formula, nil);
            AWellRecord.StartingConcentrations.
              ValuePestNames[SpeciesIndex] := PestParamName;
          end;
        end
        else
        begin
          AWellRecord.StartingConcentrations.SpeciesCount := SpeciesCount;
        end;

        AWellConnection.WellNumber := Boundary.WellNumber;
        AWellConnection.ScreenObjectName := AScreenObject.Name;



        Cell.Column := AWell.Column;
        Cell.Row := AWell.Row;
        Cell.Layer := AWell.Layer;

        AWellRecord.CellCount := 0;

        ValidScreensFound := False;
        for ScreenIndex := 0 to Boundary.WellScreens.Count - 1 do
        begin
          AWellScreen := Boundary.WellScreens[ScreenIndex] as TMawWellScreenItem;

          CompileFormula(AWellScreen.ScreenTop, StrMAWScreenTop, Formula,
            PestParamName, Column, Row, Layer);
          Expression.Evaluate;
          AWellConnection.ScreenTop := Expression.DoubleResult;
          AWellConnection.ScreenTopAnnotation := Format(
            StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
          AWellConnection.ScreenTopPestName := PestParamName;

          CompileFormula(AWellScreen.ScreenBottom, StrMAWScreenBottom, Formula,
            PestParamName, Column, Row, Layer);
          Expression.Evaluate;
          AWellConnection.ScreenBottom := Expression.DoubleResult;
          AWellConnection.ScreenBottomAnnotation := Format(
            StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
          AWellConnection.ScreenBottomPestName := PestParamName;

          ConnectionFound := False;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            CellTop := GetCellTop(Cell.Column, Cell.Row, LayerIndex);
            CellBottom := GetCellBottom(Cell.Column, Cell.Row, LayerIndex);

            if (Min(CellTop, AWellConnection.ScreenTop)
              - Max(CellBottom, AWellConnection.ScreenBottom) > 0) then
            begin
              if IDomainArray.IntegerData[LayerIndex, Cell.Row, Cell.Column] > 0 then
              begin
                Cell.Layer := LayerIndex;
                AWellConnection.Cell := Cell;

                CompileFormula(AWellScreen.SkinK, StrMAWSkinK, Formula,
                  PestParamName, Column, Row, Layer, LayerIndex);
                Expression.Evaluate;
                AWellConnection.SkinK := Expression.DoubleResult;
                AWellConnection.SkinKAnnotation := Format(
                  StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
                AWellConnection.SkinKPestName := PestParamName;

                CompileFormula(AWellScreen.SkinRadius, StrMAWSkinRadius,
                  Formula, PestParamName, Column, Row, Layer, LayerIndex);
                Expression.Evaluate;
                AWellConnection.SkinRadius := Expression.DoubleResult;
                AWellConnection.SkinRadiusAnnotation := Format(
                  StrAssignedBy0sUsi, [AScreenObject.Name, Formula]);
                AWellConnection.SkinRadiusPestName := PestParamName;

                if (AWellConnection.SkinRadius <= AWellRecord.Radius)
                  and not (Boundary.ConductanceMethod in [mcmSpecified, mcmThiem]) then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrMAWSkinRadiusLess,
                    Format(StrInSTheMAWSkin, [AScreenObject.Name]), AScreenObject);
                end;

                CellSize := Model.ShortestHorizontalBlockEdge[
                  AWellConnection.Cell.Layer, AWellConnection.Cell.Row,
                  AWellConnection.Cell.Column];

                if AWellRecord.Radius * 2 >= CellSize then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrMAWWellRadiusToo,
                    Format(StrTheWellRadiusFor, [AScreenObject.Name]), AScreenObject);
                end
                else if
                  (Boundary.ConductanceMethod in [mcmSkin, mcmCumulative, mcmMean])
                  and (AWellConnection.SkinRadius * 2 >= CellSize )then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrMAWWellSkinRadius,
                    Format(StrTheWellSkinRadius, [AScreenObject.Name]), AScreenObject);
                end;


                Inc(AWellRecord.CellCount);
                AWellConnection.ConnectionNumber := AWellRecord.CellCount;

                FWellConnections.Add(AWellConnection);
                ConnectionFound := True;
                ValidScreensFound := True;
              end;
            end;
          end;
          if not ConnectionFound then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrMAWWellScreenInva,
              Format(StrWellScreen0dOf, [ScreenIndex+1, AScreenObject.Name]), AScreenObject);
          end;
        end;
        if not ValidScreensFound then
        begin
          frmErrorsAndWarnings.AddError(Model, StrMAWWellScreensInv,
            Format(StrNoneOfTheWellScr, [AScreenObject.Name]), AScreenObject);
        end;

        FWellProperties[WellIndex] := AWellRecord;
      end;
    end;
  finally
    UsedVariables.Free;
  end;


  for WellIndex := 0 to Length(FWellProperties) - 1 do
  begin
    AWellSteady := FWellProperties[WellIndex];
    AScreenObject := Model.GetScreenObjectByName(AWellSteady.ScreenObjectName);

    IsGwtObs := False;
    if Model.GwtUsed then
    begin
      for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
      begin
        if IsMf6GwtObservation(AScreenObject, SpeciesIndex) then
        begin
          IsGwtObs := True;
          Break;
        end;
      end;
    end;
    IsMf6Obs := IsMf6Observation(AScreenObject);

    if IsGwtObs or IsMf6Obs then
    begin
      Boundary := AScreenObject.ModflowMawBoundary;
      MfObs := AScreenObject.Modflow6Obs;
      if IsMf6Obs then
      begin
        Obs.FName := MfObs.Name;
        Obs.FBoundName := AScreenObject.Name;
        Obs.FWellNumber := Boundary.WellNumber;
        Obs.FObsTypes := MfObs.MawObs;
        Obs.FModflow6Obs := MfObs;
        if (moFlowRateCells in Obs.FObsTypes)
          or (moConductanceCells in Obs.FObsTypes) then
        begin
          Obs.FCount := AWellSteady.CellCount;
        end
        else
        begin
          Obs.FCount := 0;
        end;
        FMawObservations.Add(Obs);
      end;
      if IsGwtObs then
      begin
        for SpeciesIndex := 0 to Model.MobileComponents.Count -1 do
        begin
          if Mf6ObservationsUsed
            and IsMf6GwtObservation(AScreenObject, SpeciesIndex) then
          begin
            MfObs := AScreenObject.Modflow6Obs;
            MwtObs.FBoundName := AScreenObject.Name;
            MwtObs.FObsTypes :=
              MfObs.CalibrationObservations.MwtObs[SpeciesIndex];
            if SpeciesIndex in MfObs.Genus then
            begin
              MwtObs.FObsTypes := MwtObs.FObsTypes + MfObs.MwtObs;
            end;
            MwtObs.FWellNumber := Boundary.WellNumber;
            if (mtoMwtCells in MwtObs.FObsTypes) then
            begin
              MwtObs.FCount := AWellSteady.CellCount;
            end
            else
            begin
              MwtObs.FCount := 0;
            end;
            MwtObs.FModflow6Obs := MfObs;
            MwtObs.FName := MfObs.Name + '_' + IntToStr(SpeciesIndex);
            FGwtObservations[SpeciesIndex].Add(MwtObs)
          end;
        end
      end;
    end;
  end;

end;

end.
