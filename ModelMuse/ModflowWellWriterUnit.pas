unit ModflowWellWriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowWellUnit,
  ScreenObjectUnit, ModflowBoundaryUnit, ModflowPackageSelectionUnit,
  ModflowCellUnit, OrderedCollectionUnit, ModflowBoundaryDisplayUnit,
  ModflowTransientListParameterUnit, GoPhastTypes, System.Generics.Collections,
  PhastModelUnit, Modflow6ObsUnit;

type
  TModflowWEL_Writer = class(TCustomListWriter)
  private
    NPWEL: integer;
    MXL: integer;
    FMaxTabLines: Integer;
    FTabFileCount: Integer;
    FWellBoundaryList: TList<TMfWellBoundary>;
    MXACTW: integer;
    FAbbreviation: string;
//    FPestParamUsed: Boolean;
    procedure WriteNWT_Options;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet2b;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
    procedure WriteFileInternal;
  protected
    function CellType: TValueCellType; override;
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
    procedure Evaluate; override;
    procedure WriteListOptions(InputFileName: string); override;
    procedure GetITMP(var ITMP: integer; TimeIndex: integer;
      var List: TValueCellList); override;
    procedure WriteAndCheckCells(const VariableIdentifiers: string;
      const DataSetIdentifier: string; List: TValueCellList;
      TimeIndex: integer); override;
    function BoundariesPresent: boolean; override;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    procedure WriteMoverOption; override;
    Class function Mf6ObType: TObGeneral; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    // @name is the file extension used for the observation output file.
    class function ObservationOutputExtension: string; override;
    class function Extension: string; override;
  end;


implementation

uses ModflowUnitNumbers, frmProgressUnit, Forms, frmErrorsAndWarningsUnit,
  System.IOUtils, ModflowTimeSeriesUnit, ModflowTimeSeriesWriterUnit,
  ModflowMvrWriterUnit, ModflowMvrUnit, DataSetUnit;

resourcestring
  StrWritingWELPackage = 'Writing WEL Package input.';
  StrNoWellsDefined = 'No wells defined';
  StrTheWellPackageIs = 'The Well package is not being included in the model' +
  ' because no wells are defined.';

{ TModflowWEL_Writer }

function TModflowWEL_Writer.BoundariesPresent: boolean;
var
  WellPackage: TWellPackage;
begin
  result := inherited;
  if not result then
  begin
    WellPackage := Package as TWellPackage;
    result := WellPackage.UseTabFilesInThisModel;
  end;
end;

function TModflowWEL_Writer.CellType: TValueCellType;
begin
  result := TWell_Cell;
end;

constructor TModflowWEL_Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FWellBoundaryList := TList<TMfWellBoundary>.Create;
end;

destructor TModflowWEL_Writer.Destroy;
begin
  FWellBoundaryList.Free;
  inherited;
end;

procedure TModflowWEL_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMfWellBoundary;
  TabLines: Integer;
  AStringList: TStringList;
  LineIndex: Integer;
  ALine: string;
  WellPackage: TWellPackage;
  StartTime: double;
  EndTime: double;
  AnItem: TWellItem;
  NewItem: TWellItem;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrWellFormulaError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoWellsDefined);
  WellPackage := Package as TWellPackage;
  if WellPackage.UseTabFilesInThisModel then
  begin
    StartTime := Model.ModflowFullStressPeriods.First.StartTime;
    EndTime := Model.ModflowFullStressPeriods.Last.EndTime;
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
      Boundary := ScreenObject.ModflowWellBoundary;
      if Boundary <> nil then
      begin
        if Boundary.Values.Count > 0 then
        begin
          AnItem :=  Boundary.Values.First as TWellItem;
          if AnItem.StartTime > StartTime then
          begin
            NewItem := Boundary.Values.Add as TWellItem;
            NewItem.Index := 0;
            NewItem.StartTime := StartTime;
            NewItem.EndTime := AnItem.StartTime;
            NewItem.PumpingRate := '0.';
          end;
          AnItem :=  Boundary.Values.Last as TWellItem;
          if AnItem.Endtime < Endtime then
          begin
            NewItem := Boundary.Values.Add as TWellItem;
            NewItem.StartTime := AnItem.Endtime;
            NewItem.EndTime := Endtime;
            NewItem.PumpingRate := '0.';
          end;
        end;
      end;
    end;
  end;

  inherited;

  FTabFileCount := 0;
  FMaxTabLines := 0;
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
    Boundary := ScreenObject.ModflowWellBoundary;
    if Boundary <> nil then
    begin
      if WellPackage.UseTabFilesInThisModel and (Boundary.MaxTabCells > 0) then
      begin
        FTabFileCount := FTabFileCount + Boundary.MaxTabCells;
        if Boundary.TabFileName <> '' then
        begin
//          TabLines := 0;
          AStringList := TStringList.Create;
          try
            AStringList.LoadFromFile(Boundary.TabFileName);
            TabLines := AStringList.Count;
            for LineIndex := 0 to AStringList.Count - 1 do
            begin
              ALine := AStringList[LineIndex];
              if (ALine <> '') and (ALine[1] <> '#') then
              begin
                break;
              end
              else
              begin
                Dec(TabLines);
              end;
            end;
          finally
            AStringList.Free;
          end;
          if TabLines > FMaxTabLines then
          begin
            FMaxTabLines := TabLines;
          end;
          Boundary.TabFileLines := TabLines;
        end
        else
        begin
          if Boundary.Values.Count > FMaxTabLines then
          begin
            FMaxTabLines := Boundary.Values.Count;
          end;
        end;
        FWellBoundaryList.Add(Boundary);
      end;
    end;
  end;
end;

class function TModflowWEL_Writer.Extension: string;
begin
  result := '.wel';
end;

function TModflowWEL_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowWellBoundary;
end;

procedure TModflowWEL_Writer.GetITMP(var ITMP: integer; TimeIndex: integer;
  var List: TValueCellList);
var
  WellPackage: TWellPackage;
begin
  WellPackage := Package as TWellPackage;
  if WellPackage.UseTabFilesInThisModel and (FTabFileCount > 0) then
  begin
    if TimeIndex = 0 then
    begin
      ITMP := FTabFileCount;
    end
    else
    begin
      ITMP := 0;
    end;
    List := nil;
  end
  else
  begin
    inherited;
  end;
end;

function TModflowWEL_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
//    and AScreenObject.Modflow6Obs.Used
    and (ogWell in AScreenObject.Modflow6Obs.General);
end;

function TModflowWEL_Writer.IsMf6ToMvrObservation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
//    and AScreenObject.Modflow6Obs.Used
    and (ogMvr in AScreenObject.Modflow6Obs.General);
end;

class function TModflowWEL_Writer.Mf6ObType: TObGeneral;
begin
  result := ogWell;
end;

class function TModflowWEL_Writer.ObservationExtension: string;
begin
  result := '.ob_wel';
end;

class function TModflowWEL_Writer.ObservationOutputExtension: string;
begin
  result := '.ob_wel_out';
end;

function TModflowWEL_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowWEL_Writer.ObsType: string;
begin
  result := 'wel';
end;

function TModflowWEL_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.WelPackage;
end;

function TModflowWEL_Writer.ParameterType: TParameterType;
begin
  result := ptQ;
end;

procedure TModflowWEL_Writer.WriteAndCheckCells(const VariableIdentifiers,
  DataSetIdentifier: string; List: TValueCellList; TimeIndex: integer);
var
  BoundaryIndex: Integer;
  ABoundary: TMfWellBoundary;
  CellIndex: Integer;
  ValueIndex: Integer;
  BoundaryStorage: TWellStorage;
  ACell: TWellRecord;
  FirstBoundaryStorage: TWellStorage;
  TABUNIT: Integer;
  TABVAL: Integer;
  TABLAY: Integer;
  TABROW: Integer;
  FirstCell: TWellRecord;
  TabNumber: Integer;
  TabFileName: string;
  TABCOL: Integer;
  TabFile: TFileStream;
  StartingTime: Double;
  ATime: Double;
  TimeString: AnsiString;
//  ARate: double;
  PriorEndingTime: Double;
  RateString: Ansistring;
  WellPackage: TWellPackage;
begin
  WellPackage := Package as TWellPackage;
  if WellPackage.UseTabFilesInThisModel and (FTabFileCount > 0) then
  begin
    if TimeIndex = 0 then
    begin
      TabNumber := 1;
      StartingTime := Model.ModflowFullStressPeriods.First.StartTime;
      for BoundaryIndex := 0 to FWellBoundaryList.Count - 1 do
      begin
        ABoundary := FWellBoundaryList[BoundaryIndex];
        FirstBoundaryStorage := ABoundary.Values.Boundaries[0, Model] as TWellStorage;
        for CellIndex := 0 to Length(FirstBoundaryStorage.WellArray) - 1 do
        begin
          FirstCell := FirstBoundaryStorage.WellArray[CellIndex];
          TABUNIT := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
          if ABoundary.TabFileName <> '' then
          begin
            TABVAL := ABoundary.TabFileLines;
          end
          else
          begin
            TABVAL := ABoundary.Values.Count;
          end;
          TABLAY := Model.DataSetLayerToModflowLayer(FirstCell.Cell.Layer);
          TABROW := FirstCell.Cell.Row + 1;
          TABCOL := FirstCell.Cell.Column + 1;
          WriteInteger(TABUNIT);
          WriteInteger(TABVAL);
          WriteInteger(TABLAY);
          WriteInteger(TABROW);
          WriteInteger(TABCOL);
          WriteString(' # Data Set 6: TABUNIT, TABVAL, TABVAL, TABROW, TABCOL');
          NewLine;

          TabFileName := ChangeFileExt(FNameOfFile, '.wel_tab' + IntToStr(TabNumber));
          Inc(TabNumber);
          if not WritingTemplate then
          begin
            WriteToNameFile(StrDATA, TABUNIT, TabFileName, foInput, Model);
          end;

          if ABoundary.TabFileName <> '' then
          begin
            TFile.Copy(ABoundary.TabFileName, TabFileName, True);
          end
          else
          begin
            TabFile := TFile.Create(TabFileName);
            try
              PriorEndingTime := StartingTime;
              for ValueIndex := 0 to ABoundary.Values.Count - 1 do
              begin
                if ValueIndex < ABoundary.Values.BoundaryCount[Model] then
                begin
                  BoundaryStorage := ABoundary.Values.Boundaries[
                    ValueIndex, Model] as TWellStorage;
                  Assert(Length(BoundaryStorage.WellArray) = Length(FirstBoundaryStorage.WellArray));
                  ACell := BoundaryStorage.WellArray[CellIndex];
                  Assert(FirstCell.Cell = ACell.Cell);

                  if BoundaryStorage.StartingTime > PriorEndingTime then
                  begin
                    ATime := PriorEndingTime - StartingTime;
                    TimeString := AnsiString(FreeFormattedReal(ATime));
                    TabFile.Write(TimeString[1], Length(TimeString)*SizeOf(AnsiChar));
                    RateString := ' 0';
                    TabFile.Write(RateString[1], Length(RateString)*SizeOf(AnsiChar));
                    TabFile.Write(sLineBreak[1], Length(sLineBreak)*SizeOf(AnsiChar));
                  end;
                  ATime := BoundaryStorage.StartingTime - StartingTime;
                  TimeString := AnsiString(FreeFormattedReal(ATime));
                  TabFile.Write(TimeString[1], Length(TimeString)*SizeOf(AnsiChar));
                  RateString := AnsiString(FreeFormattedReal(ACell.PumpingRate));
                  TabFile.Write(RateString[1], Length(RateString)*SizeOf(AnsiChar));
                  TabFile.Write(sLineBreak[1], Length(sLineBreak)*SizeOf(AnsiChar));

                  PriorEndingTime := BoundaryStorage.EndingTime;
                end;
              end;
            finally
              TabFile.Free;
            end;
          end;
        end;

      end;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowWEL_Writer.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Well_Cell: TWell_Cell;
  LocalLayer: integer;
  MvrKey: TMvrRegisterKey;
  ParameterName: string;
  MultiplierValue: double;
//  DataArray: TDataArray;
begin
    { TODO -cPEST : Add PEST support for PEST here }
    // handle pest parameter
    // handle multiply or add
  Inc(FBoundaryIndex);

  Well_Cell := Cell as TWell_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Well_Cell.Layer);
  WriteInteger(LocalLayer);
  if not Model.DisvUsed then
  begin
    WriteInteger(Well_Cell.Row+1);
  end;
  WriteInteger(Well_Cell.Column+1);

  if (Well_Cell.PumpingRatePest <> '')
    or (Well_Cell.PumpingRatePestSeries <> '') then
  begin
    FPestParamUsed := True;
  end;

//  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
//    and WritingTemplate
//    and ( Well_Cell.PumpingParameterName <> '') then
//  begin
//    ParameterName := Well_Cell.PumpingParameterName;
//    if Well_Cell.PumpingParameterValue = 0 then
//    begin
//      MultiplierValue := 0.0;
//    end
//    else
//    begin
//      MultiplierValue := Well_Cell.PumpingRate / Well_Cell.PumpingParameterValue;
//    end;
//    WriteTemplateFormula(ParameterName, MultiplierValue, ppmMultiply);
//  end
//  else
//  begin
//    WriteFloat(Well_Cell.PumpingRate);
//  end;

  if Model.PestUsed and (Model.ModelSelection = msModflow2015)
    and WritingTemplate
    and (Well_Cell.PumpingParameterName <> '') then
  begin
    // PEST parameters are not allowed to be combined
    // with MF-2005 style parameters.
    Assert(Well_Cell.PumpingRatePest = '');
    ParameterName := Well_Cell.PumpingParameterName;
    if Well_Cell.PumpingParameterValue = 0 then
    begin
      MultiplierValue := 0.0;
    end
    else
    begin
      MultiplierValue := Well_Cell.PumpingRate
        / Well_Cell.PumpingParameterValue;
    end;
    WriteModflowParamFormula(ParameterName, Well_Cell.PumpingRatePest,
      MultiplierValue, Well_Cell);
//    WriteTemplateFormula(ParameterName, MultiplierValue, ppmMultiply);
  end
  else
  begin
    WriteValueOrFormula(Well_Cell, WelPumpingRatePosition);
  end;

  WriteIface(Well_Cell.IFace);
  WriteBoundName(Well_Cell);
  if Model.DisvUsed then
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer cell2d '
      + VariableIdentifiers + ' ');
  end
  else
  begin
    WriteString(' # ' + DataSetIdentifier + ' Layer Row Column '
      + VariableIdentifiers + ' ');
  end;
  // The annotation identifies the object used to define the well.
  // This can be helpful in identifying when used with PEST.
  WriteString(Well_Cell.PumpingRateAnnotation);
  NewLine;

  if Well_Cell.MvrUsed and (MvrWriter <> nil) and not WritingTemplate then
  begin
    MvrKey.StressPeriod := FStressPeriod;
    MvrKey.Index := FBoundaryIndex;
    MvrKey.SourceKey.MvrIndex := Well_Cell.MvrIndex;
    MvrKey.SourceKey.ScreenObject := Well_Cell.ScreenObject;
    TModflowMvrWriter(MvrWriter).AddMvrSource(MvrKey);
  end;
end;

procedure TModflowWEL_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPWEL, MXL);
  if NPWEL > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPWEL);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPWEL MXL');
    NewLine;
  end;
end;

procedure TModflowWEL_Writer.WriteDataSet2;
var
  Option: String;
  IWELCB: Integer;
begin
  CountCells(MXACTW);
  GetFlowUnitNumber(IWELCB);
  GetOption(Option);

  WriteInteger(MXACTW);
  WriteInteger(IWELCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTW IWELCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowWEL_Writer.WriteDataSet2b;
var
  PhiRamp: Double;
  IUNITRAMP: integer;
  NameOfOutputFile: string;
  WellPackage: TWellPackage;
begin
  Assert(Model.ModelSelection = msModflowNWT);
  WellPackage := Package as TWellPackage;
  PhiRamp := WellPackage.PhiRamp;

  if PhiRamp <> 0 then
  begin
    IUNITRAMP := Model.UnitNumbers.UnitNumber(StrPHIRAMPOut);
    WriteString('SPECIFY');
    WriteFloat(PhiRamp);
    WriteInteger(IUNITRAMP);
    WriteString(' # Data Set 2b: SPECIFY PHIRAMP IUNITRAMP');
    NewLine;
    NameOfOutputFile := ChangeFileExt(FNameOfFile, '') + '.wel_dewater.txt';
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, IUNITRAMP, NameOfOutputFile, foOutput, Model);
    end;
  end;
end;

procedure TModflowWEL_Writer.WriteDataSets3And4;
const
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Qfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign, nil, nil);
end;

procedure TModflowWEL_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Q IFACE';
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

procedure TModflowWEL_Writer.WriteFile(const AFileName: string);
begin
  FPestParamUsed := False;
  if MvrWriter <> nil then
  begin
    Assert(MvrWriter is TModflowMvrWriter);
  end;

  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    FAbbreviation := 'WEL6';
  end
  else
  begin
    FAbbreviation := StrWEL;
  end;
  if Model.PackageGeneratedExternally(FAbbreviation) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;

  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  WriteFileInternal;

  if Model.ModelSelection = msModflow2015 then
  begin
    WriteModflow6FlowObs(FNameOfFile, FEvaluationType);
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

procedure TModflowWEL_Writer.WriteFileInternal;
begin
  ClearTimeLists(Model);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingWELPackage);
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

      if MAXBOUND = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoWellsDefined, StrTheWellPackageIs);
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

      if (Model.ModelSelection = msModflowNWT) and (Model.NWT_Format = nf1_1) then
      begin
        frmProgressMM.AddMessage('  Writing OPTIONS block.');
        WriteNWT_Options;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet2);
      WriteDataSet2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if MXACTW = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrNoWellsDefined, StrTheWellPackageIs);
        Exit;
      end;
    end;

    if not WritingTemplate then
    begin
      WriteToNameFile(FAbbreviation, Model.UnitNumbers.UnitNumber(StrWEL),
        FNameOfFile, foInput, Model);
    end;

    if (Model.ModelSelection = msModflowNWT) and (Model.NWT_Format = nf1_0) then
    begin
      frmProgressMM.AddMessage('  Writing Data Set 2b.');
      WriteDataSet2b;
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
  end
end;

procedure TModflowWEL_Writer.WriteListOptions(InputFileName: string);
var
  PhiRamp: Double;
  WellPackage: TWellPackage;
begin
  inherited;
  WellPackage := Package as TWellPackage;
  PhiRamp := WellPackage.PhiRamp;
  if PhiRamp > 0 then
  begin
    WriteString('    AUTO_FLOW_REDUCE');
    WriteFloat(PhiRamp);
    NewLine;
  end;

  WriteMf6ParamListOption;

//  WriteNoNewtown;
//  if WellPackage.NewtonFormulation = nfOff then
//  begin
//    WriteString('    NO_NEWTON');
//    NewLine;
//  end;
end;

procedure TModflowWEL_Writer.WriteMoverOption;
begin
  if (MvrWriter <> nil) then
  begin
    if spcWel in TModflowMvrWriter(MvrWriter).UsedPackages then
    begin
      WriteString('  MOVER');
      NewLine
    end;
  end;
end;

procedure TModflowWEL_Writer.WriteNWT_Options;
var
  PhiRamp: Double;
  IUNITRAMP: integer;
  NameOfOutputFile: string;
  WellPackage: TWellPackage;
//  NwtFormat: TNwtFormat;
//  Factor: Double;
begin
  Assert(Model.ModelSelection = msModflowNWT);
  WellPackage := Package as TWellPackage;
  PhiRamp := WellPackage.PhiRamp;

  WriteString('OPTIONS');
  NewLine;

  if PhiRamp <> 0 then
  begin
    IUNITRAMP := Model.UnitNumbers.UnitNumber(StrPHIRAMPOut);
    WriteString('SPECIFY');
    WriteFloat(PhiRamp);
    WriteInteger(IUNITRAMP);
//    WriteString(' # Data Set 2b: SPECIFY PHIRAMP IUNITRAMP');
    NewLine;
    NameOfOutputFile := ChangeFileExt(FNameOfFile, '') + '.wel_dewater.txt';
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, IUNITRAMP, NameOfOutputFile, foOutput, Model);
    end;
  end;

  if WellPackage.UseTabFilesInThisModel then
  begin
    WriteString('TABFILES');
    WriteInteger(FTabFileCount);
    WriteInteger(FMaxTabLines);
    NewLine;
  end;

  WriteString('END');
  NewLine;

end;

procedure TModflowWEL_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TWell_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TWell_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'WEL');
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
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Bhead Qfact IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
