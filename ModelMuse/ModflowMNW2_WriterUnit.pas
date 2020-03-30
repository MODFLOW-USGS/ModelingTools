unit ModflowMNW2_WriterUnit;

interface

uses System.Types, System.UITypes, Generics.Collections,
  Winapi.Windows, CustomModflowWriterUnit, ModflowPackageSelectionUnit, Classes, Contnrs,
  PhastModelUnit, ScreenObjectUnit, ModflowCellUnit, ModflowMnw2Unit, SysUtils,
  ModflowBoundaryDisplayUnit, RbwParser, Vcl.Dialogs, PestObsUnit;

type
  TMultinodeWell = class(TObject)
  private
    FCells: TValueCellList;
    FScreenObject: TScreenObject;
    function GetCell(Index: integer): TValueCell;
    function GetCellCount: integer;
    function WellBoundary: TMnw2Boundary;
    function VerticalWell: boolean;
    function ConstantWellLossParameters: boolean;
    procedure GetVerticalWellElevations(out Ztop, Zbotm: double);
  public
    property Cells[Index: integer]: TValueCell read GetCell;
    property CellCount: integer read GetCellCount;
    Destructor Destroy; override;
    class function IsScreenObjectVertical(
      AScreenObject: TScreenObject): Boolean;
  end;
  
  TObsFileLink = class(TObject)
  private
    FileName: string;
    WellBoundary: TMnw2Boundary;
  end;
  
  TObsFileLinks = TObjectList<TObsFileLink>;

  TModflowMNW2_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    FValues: TList;
    FWells: TList;
    FWellNames: TStringList;
    NNODES: Integer;
    FMnwiWells: TList;
    FMnwPackage: TMultinodeWellSelection;
    FObsLinks: TObsFileLinks;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet2A(WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2B(WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2C(ConstantWellLossParameters: Boolean;
      WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2D(ConstantWellLossParameters: boolean;
      WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2E(Well: TMultinodeWell; WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2F(WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2G(WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2H(WellBoundary: TMnw2Boundary);
    procedure WriteDataSets3and4;
    procedure WriteDataSet3(StartTime: Double; StressPeriod: integer);
    procedure WriteDataSet4A(WellBoundary: TMnw2Boundary;
      TimeItem: TMnw2TimeItem);
    function GetQCut(Item: TMnw2TimeItem): Integer;
    procedure WriteDataSet4B(WellBoundary: TMnw2Boundary;
      TimeItem: TMnw2TimeItem);
    procedure WriteDataSet4(StartTime: Double);
    procedure EvaluateMnwi;
    procedure WriteMnwiDataSet1(AFileName: string);
    procedure WriteMnwiDataSet2;
    procedure WriteMnwiDataSet3(AFileName: string);
    procedure EvaluateVerticalScreenFormula(var Expression: TExpression;
      const ADataName: string; var Formula: string; Compiler: TRbwParser; WellBoundary: TMnw2Boundary);
    procedure CheckWells;
    function CountNodes: integer;
    procedure WriteObsScript(const AFileName: string);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    procedure Evaluate;
  public
    class procedure AdjustWellID(var WELLID: string);
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteMnwiFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;
  
resourcestring
  StrInvalidMnwTable = 'The following objects define multinode wells in w' +
  'hich LIFTqmax is less than one or more values in the head capacity table.';
  StrInvalidMnwTable2 = 'The following objects define multinode wells in w' +
  'hich LIFTq0 is greater than one or more values in the head capacity table.';
  StrWhenPumpingCapacit = 'When pumping capacity affects the flow rate in MN' +
  'W2 wells, the withdrawal rate is treated as the maximum possible pumping ' +
  'rate and should be greater than the largest pumping rate in the pump ' +
  'capacity table. The following objects specify wells that do not meet this'
  + ' criterion.';

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, frmErrorsAndWarningsUnit, GoPhastTypes,
  ModflowTimeUnit, ModflowBoundaryUnit, frmFormulaErrorsUnit, Math, Forms,
  DataSetUnit, ObservationComparisonsUnit, ModelMuseUtilities;

resourcestring
  SignError = 'The deactivation pumping rate and reactivation pumping rate '
    + 'should have the same sign as the pumping rate.';
  RelativeSizeError = 'The reactivation pumping rate should be larger than '
    + 'the deactivation pumping rate.';
  SizeError = 'The reactivation pumping rate and '
    + 'deactivation pumping rate should be smaller than the pumping rate.';
  InvalidFractionError =
    'The deactivation pumping rate and reactivation pumping rate '
    + 'should both be between 0 and 1.';
  StrObjectSStartin = 'Object = %0:s; Starting time = %1:g';
  LossTypeError = 'In the MNW2 package, a LOSSTYPE of "NONE" is only '
    + 'valid if the well has only one cell. Objects in which this is violated'
    + ' are listed below.';
  StrTheFollowingObject = 'The following objects define WELLIDs in the MNW2 ' +
  'that are not unique.';
  StrVerticalScreensAre = 'Vertical screens are not allowed in wells in whic' +
  'h the LOSSTYPE is "NONE".';
  StrObject = 'Object = %s';
  StrObject0sWELLI = 'Object = %0:s; WELLID = %1:s';
  StrNoMultinodeWellsD = 'No Multinode wells defined.';
  StrTheMNW2PackageIs = 'The MNW2 package is active but no multinode wells h' +
  'ave been defined.';
  StrEvaluatingMNWIPack = 'Evaluating MNWI Package data.';
  StrEvaluatingS = '    Evaluating %s';
  StrEvaluatingMNW2Pack = 'Evaluating MNW2 Package data.';
  StrWritingMNW2Package = 'Writing MNW2 Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingMNWIPackage = 'Writing MNWI Package input.';
  StrTheMNW2PackageMt3dms = 'The MNW2 package is not supported by MT3DMS.';
  StrAlthoughMT3DMSVers = 'Although MT3DMS version 5.3 supports the MNW1 pac' +
  'kage, is does not suppport the MNW2 package. MT3D-USGS does support the MNW2 package.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
  StrNoWellID = 'The following objects failed to define WELLIDs in the MNW2 package.';

//  StrTheFollowingObject

{ TModflowMNW2_Writer }

constructor TModflowMNW2_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FValues := TObjectList.Create;
  FWells := TObjectList.Create;
  FWellNames:= TStringList.Create;
  FWellNames.CaseSensitive := False;
  FWellNames.Sorted := True;
  FMnwiWells := TList.Create;
  FObsLinks := TObsFileLinks.Create;
end;

destructor TModflowMNW2_Writer.Destroy;
begin
  FObsLinks.Free;
  FMnwiWells.Free;
  FWellNames.Free;
  FWells.Free;
  FValues.Free;
  inherited;
end;

procedure TModflowMNW2_Writer.EvaluateMnwi;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMnw2Boundary;
begin
  frmProgressMM.AddMessage(StrEvaluatingMNWIPack);
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
    Boundary := ScreenObject.ModflowMnw2Boundary;
    if (Boundary = nil) or not Boundary.Used then
    begin
      Continue;
    end;
    frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
    if Boundary.Observations.Count > 0 then
    begin
      Boundary.SaveMnwiInfo := True;
    end;
    if Boundary.SaveMnwiInfo
      or Boundary.SaveExternalFlows
      or Boundary.SaveInternalFlows then
    begin
      FMnwiWells.Add(Boundary);
    end;
  end;
end;

procedure TModflowMNW2_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMnw2Boundary;
  Dummy: TStringList;
  Well: TMultinodeWell;
  Item: TCustomModflowBoundaryItem;
  StressPeriod: TModflowStressPeriod;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmProgressMM.AddMessage(StrEvaluatingMNW2Pack);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoMultinodeWellsD);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, SignError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, RelativeSizeError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, SizeError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidFractionError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMnwTable);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMnwTable2);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, LossTypeError);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingObject);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoWellID);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrVerticalScreensAre);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrWhenPumpingCapacit);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheMNW2PackageMt3dms);

    if Model.ModflowPackages.Mt3dBasic.IsSelected
      and (Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheMNW2PackageMt3dms,
        StrAlthoughMT3DMSVers);
    end;

    Dummy := TStringList.Create;
    try
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
        Boundary := ScreenObject.ModflowMnw2Boundary;
        if (Boundary = nil) or not Boundary.Used then
        begin
          Continue;
        end;
        if Boundary.Values.Count = 0 then
        begin
          Boundary.Values.Add;
        end;

        Item := Boundary.Values[0] as TCustomModflowBoundaryItem;
        StressPeriod := Model.ModflowFullStressPeriods.First;
        Item.StartTime := StressPeriod.StartTime;
        StressPeriod := Model.ModflowFullStressPeriods.Last;
        Item.EndTime := StressPeriod.EndTime;

        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        Boundary.GetCellValues(FValues, Dummy, Model);
        if (FValues.Count >= 1) then
        begin
          Assert(FValues.Count = 1);
          Well := TMultinodeWell.Create;
          Well.FCells := FValues.Extract(FValues[0]);
          if Well.FCells.Count > 0 then
          begin
            FWells.Add(Well);
            Well.FScreenObject := ScreenObject;
          end
          else
          begin
            Well.Free;
          end;
        end;
      end;
      CheckWells;
    finally
      Dummy.Free;
    end;
    if FWells.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoMultinodeWellsD, StrTheMNW2PackageIs);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowMNW2_Writer.Extension: string;
begin
  result := '.mnw2';
end;

function TModflowMNW2_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mnw2Package;
end;

procedure TModflowMNW2_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  WellRadiusTimes: TModflowBoundaryDisplayTimeList;
  List: TValueCellList;
  Well : TMultinodeWell;
  WellIndex: integer;
  DataSets: TList;
  UsedIndicies: TByteSet;
  TimeIndex: Integer;
  Boundary: TMnw2Boundary;
  DataTypeIndex: Integer;
  TimeListIndex: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  DataSetIndex: Integer;
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

      if FWells.Count = 0 then
      begin
        SetTimeListsUpToDate(TimeLists);
        Exit;
      end;

      WellRadiusTimes := TimeLists[0];
      for TimeIndex := 0 to WellRadiusTimes.Count - 1 do
      begin
        DataSets.Clear;

        for TimeListIndex := 0 to TimeLists.Count - 1 do
        begin
          TimeList := TimeLists[TimeListIndex];
          DataArray := TimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataSets.Add(DataArray);
        end;

        for WellIndex := 0 to FWells.Count - 1 do
        begin
          Well := FWells[WellIndex];
          Boundary := Well.WellBoundary;
          UsedIndicies := [];
          for DataTypeIndex := WellRadiusPosition to PartialPenetrationPosition do
          begin
            if Boundary.DataTypeUsed(DataTypeIndex) then
            begin
              Include(UsedIndicies, DataTypeIndex);
            end;
          end;

          if UsedIndicies <> [] then
          begin
            List := Well.FCells;
  //          List.CheckRestore;

            UpdateCellDisplay(List, DataSets, [], nil, UsedIndicies);
            List.Cache;
          end;
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
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

class procedure TModflowMNW2_Writer.AdjustWellID(var WELLID: string);
begin
  if Pos(' ', WELLID) > 0 then
  begin
    WELLID := '"' + WELLID + '"';
  end;
  WELLID := WELLID + ' ';
end;

procedure TModflowMNW2_Writer.CheckWells;
var
  TimeItem: TMnw2TimeItem;
  WellBoundary: TMnw2Boundary;
  TimeIndex: Integer;
  WellIndex: Integer;
//  ScreenObject: TScreenObject;
  Well: TMultinodeWell;
  Limit: integer;
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    if WellBoundary.ConstrainPumping then
    begin
      if WellBoundary.ConstantConstraints then
      begin
        Limit := 1;
      end
      else
      begin
        Limit := WellBoundary.TimeValues.Count;
      end;
//      ScreenObject := Well.FScreenObject;
      for TimeIndex := 0 to Limit - 1 do
      begin
        TimeItem := WellBoundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
        case TimeItem.LimitMethod of
          mlmNoMinimum:
            begin
            end;
          mlmRate:
            begin
              if TimeItem.PumpingRateValue <> 0 then
              begin
                if (Sign(TimeItem.PumpingRateValue)
                  <> Sign(TimeItem.InactivationPumpingRateValue))
                  or (Sign(TimeItem.PumpingRateValue)
                  <> Sign(TimeItem.ReactivationPumpingRateValue))
                  or (Sign(TimeItem.InactivationPumpingRateValue)
                  <> Sign(TimeItem.ReactivationPumpingRateValue)) then
                begin
                  frmErrorsAndWarnings.AddError(Model, SignError,
                    Format(StrObjectSStartin,
                    [Well.FScreenObject.Name, TimeItem.StartTime]),
                    Well.FScreenObject);
//                  frmErrorsAndWarnings.AddError(Model, SignError,
//                    'Object = ' + Well.FScreenObject.Name
//                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if Abs(TimeItem.InactivationPumpingRateValue) >=
                  Abs(TimeItem.ReactivationPumpingRateValue) then
                begin
                  frmErrorsAndWarnings.AddError(Model, RelativeSizeError,
                    Format(StrObjectSStartin,
                    [Well.FScreenObject.Name, TimeItem.StartTime]),
                    Well.FScreenObject);
//                  frmErrorsAndWarnings.AddError(Model, RelativeSizeError,
//                    'Object = ' + Well.FScreenObject.Name
//                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if (Abs(TimeItem.InactivationPumpingRateValue)
                  >= Abs(TimeItem.PumpingRateValue))
                  or (Abs(TimeItem.ReactivationPumpingRateValue)
                  >= Abs(TimeItem.PumpingRateValue)) then
                begin
                  frmErrorsAndWarnings.AddError(Model, SizeError,
                    Format(StrObjectSStartin,
                    [Well.FScreenObject.Name, TimeItem.StartTime]),
                    Well.FScreenObject);
//                  frmErrorsAndWarnings.AddError(Model, SizeError,
//                    'Object = ' + Well.FScreenObject.Name
//                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
              end;
            end;
          mlmFraction:
            begin
              if TimeItem.PumpingRateValue <> 0 then
              begin
                if (TimeItem.InactivationPumpingRateValue < 0)
                  or (TimeItem.InactivationPumpingRateValue > 1)
                  or (TimeItem.ReactivationPumpingRateValue < 0)
                  or (TimeItem.ReactivationPumpingRateValue > 1) then
                begin
                  frmErrorsAndWarnings.AddError(Model, InvalidFractionError,
                    Format(StrObjectSStartin,
                    [Well.FScreenObject.Name, TimeItem.StartTime]),
                    Well.FScreenObject);
//                  frmErrorsAndWarnings.AddError(Model, InvalidFractionError,
//                    'Object = ' + Well.FScreenObject.Name
//                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if Abs(TimeItem.InactivationPumpingRateValue) >=
                  Abs(TimeItem.ReactivationPumpingRateValue) then
                begin
                  frmErrorsAndWarnings.AddError(Model, RelativeSizeError,
                    Format(StrObjectSStartin,
                    [Well.FScreenObject.Name, TimeItem.StartTime]),
                    Well.FScreenObject);
//                  frmErrorsAndWarnings.AddError(Model, RelativeSizeError,
//                    'Object = ' + Well.FScreenObject.Name
//                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
              end;
            end;
        else
          // do nothing
          Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TModflowMNW2_Writer.EvaluateVerticalScreenFormula(
  var Expression: TExpression; const ADataName: string; var Formula: string;
  Compiler: TRbwParser; WellBoundary: TMnw2Boundary);
  var
    LocalScreenObject: TScreenObject;
begin
  try
    Compiler.Compile(Formula);
  except
    on E: ERbwParserError do
    begin
      LocalScreenObject := WellBoundary.ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name, '', Formula, E.Message + '; Error in formula for ' + ADataName + '.');
      Formula := '0';
      Compiler.Compile(Formula);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  Expression.Evaluate;
end;

procedure TModflowMNW2_Writer.WriteDataSet1;
const
  OPTION = ' AUXILIARY IFACE';
var
  MNWMAX: integer;
  IWL2CB: integer;
  MNWPRNT: integer;
  NODTOT: Integer;
  UseNODTOT: boolean;
begin
  MNWMAX := FWells.Count;
  UseNODTOT := Model.ModelSelection in
    [msModflow, msModflowLGR2, msModflowNWT, msModflowFmp, msModflowCfp];
  if UseNODTOT then
  begin
    NODTOT := CountNodes;
    MNWMAX := -MNWMAX;
  end
  else
  begin
    NODTOT := 0;
  end;
  GetFlowUnitNumber(IWL2CB);
  MNWPRNT := Ord(Model.ModflowPackages.Mnw2Package.PrintOption);
  WriteInteger(MNWMAX);
  if UseNODTOT then
  begin
    WriteInteger(NODTOT);
  end;
  WriteInteger(IWL2CB);
  WriteInteger(MNWPRNT);
  WriteString(OPTION);
  if UseNODTOT then
  begin
    WriteString(' # DataSet 1: MNWMAX, NODTOT, IWL2CB, MNWPRNT, OPTION');
  end
  else
  begin
    WriteString(' # DataSet 1: MNWMAX, IWL2CB, MNWPRNT, OPTION');
  end;
  NewLine;
end;

function TModflowMNW2_Writer.CountNodes: integer;
var
  WellIndex: Integer;
  Well: TMultinodeWell;
  WellBoundary: TMnw2Boundary;
  Local_NNODES: Integer;
  LayerCount: Integer;
begin
  result := 0;
  LayerCount := Model.ModflowGrid.LayerCount;
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    Local_NNODES := Well.FCells.Count;
    if Well.VerticalWell then
    begin
      if WellBoundary.VerticalScreens.Count > 0 then
      begin
        Local_NNODES := Max(WellBoundary.VerticalScreens.Count, LayerCount);
      end
      else if WellBoundary.LossType <> mltNone then
      begin
        Local_NNODES := LayerCount;
      end;
    end;
    Inc(result, Local_NNODES);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2;
var
  WellIndex: Integer;
  Well: TMultinodeWell;
  WellBoundary: TMnw2Boundary;
  ConstantWellLossParameters: Boolean;
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    WriteDataSet2A(WellBoundary, Well);
    WriteDataSet2B(WellBoundary, Well);
    ConstantWellLossParameters := Well.ConstantWellLossParameters;
    WriteDataSet2C(ConstantWellLossParameters, WellBoundary, Well);
    WriteDataSet2D(ConstantWellLossParameters, WellBoundary, Well);
    WriteDataSet2E(Well, WellBoundary);
    WriteDataSet2F(WellBoundary);
    WriteDataSet2G(WellBoundary);
    WriteDataSet2H(WellBoundary);
  end;
end;

procedure TModflowMNW2_Writer.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrMNW2) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrMNW2, Model.UnitNumbers.UnitNumber(StrMNW2),
    FNameOfFile, foInput, Model);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMNW2Package);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

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

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3and4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiFile(const AFileName: string);
var
  NameOfMnwiFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrMNWI) then
  begin
    Exit;
  end;
  EvaluateMnwi;
  FMnwPackage := Model.ModflowPackages.Mnw2Package;
  if (FMnwiWells.Count > 0) or FMnwPackage.CreateWellFile
    or FMnwPackage.SummarizeByWell or FMnwPackage.SummarizeByNode then
  begin
    NameOfMnwiFile := ChangeFileExt(AFileName, '.mnwi');
    WriteToNameFile(StrMNWI, Model.UnitNumbers.UnitNumber(StrMNWI),
      NameOfMnwiFile, foInput, Model);

    OpenFile(NameOfMnwiFile);
    try
      frmProgressMM.AddMessage(StrWritingMNWIPackage);
      WriteMnwiDataSet1(AFileName);
      WriteMnwiDataSet2;
      WriteMnwiDataSet3(AFileName);

    finally
      CloseFile;
    end;

    WriteObsScript(AFileName);

  end;
end;

procedure TModflowMNW2_Writer.WriteObsScript(
  const AFileName: string);
//const
//  MarkerDelimiter = '@';
var
  ScriptFileName: string;
//  ExtractorListFileName: string;
//  ExtractorObsFileName: string;
  ObjectIndex: Integer;
  Link: TObsFileLink;
  Boundary: TMnw2Boundary;
  ObsIndex: Integer;
  Obs: TMnw2ObsItem;
  ComparisonsUsed: Boolean;
  CompIndex: Integer;
  CompItem: TObsCompareItem;
//  InstructionFileName: string;
//  ComparisonIndex: Integer;
//  GloCompItem: TGlobalObsComparisonItem;
//  FObsItemDictionary: TDictionary<string, TCustomObservationItem>;
//  PriorItem1: TCustomObservationItem;
//  PriorItem2: TCustomObservationItem;
  StartTime: Double;
  function GetObName(ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('Mnw2', ObjectIndex, Obs);
  end;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  if FObsLinks.Count = 0 then
  begin
    Exit;
  end;

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  ScriptFileName := ChangeFileExt(AFileName, '.mnw_script');

  OpenFile(ScriptFileName);
  try
//    ExtractorListFileName := ChangeFileExt(AFileName, '.mnw_lst');
//    ExtractorObsFileName := ChangeFileExt(AFileName, '.mnw_obs.csv');
//
//    // FILENAMES block
//    WriteString('BEGIN FILENAMES');
//    NewLine;
//    WriteString('  LISTING_FILE ');
//    WriteString(ExtractFileName(ExtractorListFileName));
//    NewLine;
//    WriteString('  OBSERVATIONS_FILE ');
//    WriteString(ExtractFileName(ExtractorObsFileName));
//    NewLine;
//    WriteString('END FILENAMES');
//    NewLine;
//    NewLine;

    ComparisonsUsed := False;
    // OBSERVATIONS block
    WriteString('BEGIN OBSERVATIONS');
    NewLine;
    for ObjectIndex := 0 to FObsLinks.Count - 1 do
    begin
      Link := FObsLinks[ObjectIndex];
      Boundary := Link.WellBoundary;
      WriteString('  # ');
      WriteString('Observations defined in ');
      WriteString((Boundary.ScreenObject as TScreenObject).Name);
      NewLine;

      WriteString('  FILENAME ');
      WriteString(Link.FileName);
      NewLine;
      for ObsIndex := 0 to Boundary.Observations.Count - 1 do
      begin
        Obs := Boundary.Observations[ObsIndex];
//          FObsItemDictionary.Add(Obs.GUID, Obs);
        WriteString('  OBSERVATION ');
        WriteString(GetObName(ObjectIndex, Obs));
        case Obs.ObsType of
          motQin: WriteString(' Qin ');
          motQout: WriteString(' Qout ');
          motQnet: WriteString(' Qnet ');
          motQCumu: WriteString(' QCumu ');
          motHwell: WriteString(' Hwell ');
          else Assert(False);
        end;
        WriteFloat(Obs.Time - StartTime);
        WriteFloat(Obs.ObservedValue);
        WriteFloat(Obs.Weight);
        WriteString(' PRINT');
        NewLine;
      end;

      if Boundary.Observations.Comparisons.Count > 0 then
      begin
        ComparisonsUsed := True;
      end;
    end;
    WriteString('END OBSERVATIONS');

    // DERIVED_OBSERVATIONS block
    if ComparisonsUsed then
    begin
      NewLine;
      NewLine;
      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      for ObjectIndex := 0 to FObsLinks.Count - 1 do
      begin
        Link := FObsLinks[ObjectIndex];
        Boundary := Link.WellBoundary;
        if Boundary.Observations.Comparisons.Count > 0 then
        begin
          WriteString('  # ');
          WriteString('Observation comparisons defined in ');
          WriteString((Boundary.ScreenObject as TScreenObject).Name);
          NewLine;

          for CompIndex := 0 to Boundary.Observations.Comparisons.Count - 1 do
          begin
            WriteString('  DIFFERENCE ');
            CompItem := Boundary.Observations.Comparisons[CompIndex];
            WriteString(GetObName(ObjectIndex, CompItem));
            WriteString(' ');
            Obs := Boundary.Observations[CompItem.Index1];
            WriteString(Obs.ExportedName);
            WriteString(' ');
            Obs := Boundary.Observations[CompItem.Index2];
            WriteString(Obs.ExportedName);
            WriteFloat(CompItem.ObservedValue);
            WriteFloat(CompItem.Weight);
            WriteString(' PRINT');
            NewLine;
          end;
        end;
      END;

      {
      if Model.GlobalObservationComparisons.Count > 0 then
      begin
        WriteString('  # ');
        WriteString('Global observation comparisons');
        NewLine;
      end;
      for ComparisonIndex := 0 to Model.GlobalObservationComparisons.Count - 1 do
      begin
        GloCompItem := Model.GlobalObservationComparisons[ComparisonIndex];
        if FObsItemDictionary.TryGetValue(GloCompItem.GUID1, PriorItem1)
          and FObsItemDictionary.TryGetValue(GloCompItem.GUID2, PriorItem2) then
        begin
          WriteString('  DIFFERENCE ');
//            CompItem := Boundary.Observations.Comparisons[CompIndex];
          WriteString(GloCompItem.Name);
          WriteString(' ');
          WriteString(PriorItem1.ExportedName);
          WriteString(' ');
          WriteString(PriorItem2.ExportedName);
          WriteFloat(GloCompItem.ObservedValue);
          WriteFloat(GloCompItem.Weight);
          WriteString(' PRINT');
          NewLine;
        end;
      end;
      }

      WriteString('END DERIVED_OBSERVATIONS');
    end;

  finally
    CloseFile;
  end;

    {
    InstructionFileName := ExtractorObsFileName + '.ins';
    OpenFile(InstructionFileName);
    try
      WriteString('pif ');
      WriteString(MarkerDelimiter);
      NewLine;

      for ObjectIndex := 0 to FObsLinks.Count - 1 do
      begin
        Link := FObsLinks[ObjectIndex];
        Boundary := Link.WellBoundary;

        WriteString('l1');
        NewLine;
        for ObsIndex := 0 to Boundary.Observations.Count - 1 do
        begin
          Obs := Boundary.Observations[ObsIndex];
          WriteString('l1 ');
          WriteString(MarkerDelimiter);
          WriteString('"');
          WriteString(GetObName(ObjectIndex, Obs));
          WriteString('",');
          WriteString(MarkerDelimiter);
          WriteString(' !');
          WriteString(GetObName(ObjectIndex, Obs));
          WriteString('! ');
          WriteString(MarkerDelimiter);
          WriteString(',');
          WriteString(MarkerDelimiter);
          NewLine;
        end;

        if Boundary.Observations.Comparisons.Count > 0 then
        begin
          ComparisonsUsed := True;
        end;
      end;

      if ComparisonsUsed then
      begin
        for ObjectIndex := 0 to FObsLinks.Count - 1 do
        begin
          Link := FObsLinks[ObjectIndex];
          Boundary := Link.WellBoundary;
          if Boundary.Observations.Comparisons.Count > 0 then
          begin
            for CompIndex := 0 to Boundary.Observations.Comparisons.Count - 1 do
            begin
              CompItem := Boundary.Observations.Comparisons[CompIndex];
              WriteString('l1 ');
              WriteString(MarkerDelimiter);
              WriteString('"');
              WriteString(GetObName(ObjectIndex, CompItem));
              WriteString('",');
              WriteString(MarkerDelimiter);
              WriteString(' !');
              WriteString(GetObName(ObjectIndex, CompItem));
              WriteString('! ');
              WriteString(MarkerDelimiter);
              WriteString(',');
              WriteString(MarkerDelimiter);
              NewLine;
            end;
          end;
        END;

        for ComparisonIndex := 0 to Model.GlobalObservationComparisons.Count - 1 do
        begin
          GloCompItem := Model.GlobalObservationComparisons[ComparisonIndex];
          if FObsItemDictionary.TryGetValue(GloCompItem.GUID1, PriorItem1)
            and FObsItemDictionary.TryGetValue(GloCompItem.GUID2, PriorItem2) then
          begin
            WriteString('l1 ');
            WriteString(MarkerDelimiter);
            WriteString('"');
            WriteString(GloCompItem.Name);
            WriteString('",');
            WriteString(MarkerDelimiter);
            WriteString(' !');
            WriteString(GloCompItem.Name);
            WriteString('! ');
            WriteString(MarkerDelimiter);
            WriteString(',');
            WriteString(MarkerDelimiter);
            NewLine;
          end;
        end;

      end

    finally
      CloseFile;
    end;
    }
//  finally
//    FObsItemDictionary.Free;
//  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet3(AFileName: string);

var
  QBHflag: Integer;
  QNDflag: Integer;
  UNIT_Number: Integer;
  WELLID: string;
  Boundary: TMnw2Boundary;
  WellIndex: Integer;

  OutputFileName: string;
  ObsFileLink: TObsFileLink;
begin
  AFileName := ExtractFileName(AFileName);
  AFileName := ChangeFileExt(AFileName, '');
  frmProgressMM.AddMessage(StrWritingDataSet3);
  for WellIndex := 0 to FMnwiWells.Count - 1 do
  begin
    Boundary := FMnwiWells[WellIndex];
    WELLID := Boundary.WellID;
    OutputFileName := AFileName + '_' + WELLID;
    OutputFileName := StringReplace(OutputFileName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
    OutputFileName := ChangeFileExt(OutputFileName, '.mnwi_out');

    if Pos(' ', WELLID) > 0 then
    begin
      WELLID := '"' + WELLID + '"';
    end;
    WELLID := WELLID + ' ';
    UNIT_Number := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
//    Inc(StartUnitNumber);
    if Boundary.SaveExternalFlows then
    begin
      QNDflag := 1;
    end
    else
    begin
      QNDflag := 0;
    end;
    if Boundary.SaveInternalFlows then
    begin
      QBHflag := 1;
    end
    else
    begin
      QBHflag := 0;
    end;
    WriteString(WELLID);
    WriteInteger(UNIT_Number);
    WriteInteger(QNDflag);
    WriteInteger(QBHflag);
    WriteString(' # WELLID, UNIT, QNDflag, QBHflag');
    NewLine;

    WriteToNameFile(StrDATA, UNIT_Number,
      OutputFileName, foOutput, Model);

  {$IFDEF PEST}
    if Boundary.Observations.Count > 0 then
    begin
      ObsFileLink := TObsFileLink.Create;
      FObsLinks.Add(ObsFileLink);
      ObsFileLink.FileName := OutputFileName;
      ObsFileLink.WellBoundary := Boundary;
    end;
  {$ENDIF}
  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet2;
var
  MNWOBS: Integer;
begin
  frmProgressMM.AddMessage(StrWritingDataSet2);

  MNWOBS := FMnwiWells.Count;
  WriteInteger(MNWOBS);
  WriteString(' # Data Set 2: MNWOBS');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet1(AFileName: string);
var
  WEL1flag: Integer;
  QSUMflag: Integer;
  BYNDflag: Integer;
  OutputFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet1);
  AFileName := ExtractFileName(AFileName);
  AFileName := ChangeFileExt(AFileName, '');
  if FMnwPackage.CreateWellFile then
  begin
    WEL1flag := Model.UnitNumbers.UnitNumber(StrMNWI_Wells);
    OutputFileName := ChangeFileExt(AFileName, '.wel_out');
    WriteToNameFile(StrDATA, WEL1flag,
      OutputFileName, foOutput, Model);
  end
  else
  begin
    WEL1flag := 0;
  end;
  if FMnwPackage.SummarizeByWell then
  begin
    QSUMflag := Model.UnitNumbers.UnitNumber(StrMNWI_SummarizeByWell);
    OutputFileName := ChangeFileExt(AFileName, '.QSUM_out');
    WriteToNameFile(StrDATA, QSUMflag,
      OutputFileName, foOutput, Model);
  end
  else
  begin
    QSUMflag := 0;
  end;
  if FMnwPackage.SummarizeByNode then
  begin
    BYNDflag := Model.UnitNumbers.UnitNumber(StrMNWI_SummarizeByNode);
    OutputFileName := ChangeFileExt(AFileName, '.BYND_out');
    WriteToNameFile(StrDATA, BYNDflag,
      OutputFileName, foOutput, Model);
  end
  else
  begin
    BYNDflag := 0;
  end;
  WriteInteger(WEL1flag);
  WriteInteger(QSUMflag);
  WriteInteger(BYNDflag);
  WriteString(' # Data Set 1: WEL1flag, QSUMflag, BYNDflag');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet4(StartTime: Double);
var
  WellBoundary: TMnw2Boundary;
  TimeIndex: Integer;
  TimeItem: TMnw2TimeItem;
  WellIndex: Integer;
  Well: TMultinodeWell;
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    TimeIndex := WellBoundary.TimeValues.IndexOfContainedStartTime(StartTime);
    if TimeIndex >= 0 then
    begin
      TimeItem := WellBoundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
      WriteDataSet4A(WellBoundary, TimeItem);
      WriteDataSet4B(WellBoundary, TimeItem);
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet4B(WellBoundary: TMnw2Boundary;
  TimeItem: TMnw2TimeItem);
var
  Qfrcmx: Double;
  Qfrcmn: Double;
  Comment: string;
  QCut: Integer;
  Hlim: Double;
begin
  if WellBoundary.ConstrainPumping and
    not WellBoundary.ConstantConstraints then
  begin
    Hlim := TimeItem.LimitingWaterLevelValue;
    QCut := GetQCut(TimeItem);
    Comment := ' # Data Set 4B: Hlim, QCut';
    WriteFloat(Hlim);
    WriteInteger(QCut);
    if QCut <> 0 then
    begin
      Qfrcmn := TimeItem.InactivationPumpingRateValue;
      Qfrcmx := TimeItem.ReactivationPumpingRateValue;
      Comment := Comment + ', Qfrcmn, Qfrcmx';
      WriteFloat(Qfrcmn);
      WriteFloat(Qfrcmx);
    end;
    WriteString(Comment);
    NewLine;
  end;
end;

function TModflowMNW2_Writer.GetQCut(Item: TMnw2TimeItem): Integer;
begin
  result := 0;
  case Item.LimitMethod of
    mlmNoMinimum:
      result := 0;
    mlmRate:
      result := 1;
    mlmFraction:
      result := -1;
    else
      Assert(False);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet4A(WellBoundary: TMnw2Boundary;
  TimeItem: TMnw2TimeItem);
var
  WELLID: string;
  QDes: Double;
  Comment: string;
  CapMult: Double;
  IFACE: TIface;
begin
  WELLID := WellBoundary.WellID;
  if Pos(' ', WELLID) > 0 then
  begin
    WELLID := '"' + WELLID + '"';
  end;
  WELLID := WELLID + ' ';
  QDes := TimeItem.PumpingRateValue;
  WriteString(WELLID);
  WriteFloat(QDes);
  Comment := ' # Data Set 4A: WELLID, QDes';
  if WellBoundary.AdjustPumping then
  begin
    CapMult := TimeItem.HeadCapacityMultiplierValue;
    WriteFloat(CapMult);
    Comment := Comment + ', CapMult';
  end;
  IFACE := (WellBoundary.ScreenObject as TScreenObject).IFace;
  WriteIFACE(IFACE);
  Comment := Comment + ', IFACE';
  WriteString(Comment);
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet3(StartTime: Double;
  StressPeriod: integer);
var
  TimeIndex: Integer;
  WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell;
  WellIndex: Integer;
  ITMP: Integer;
begin
  ITMP := 0;
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    TimeIndex := WellBoundary.TimeValues.IndexOfContainedStartTime(StartTime);
    if TimeIndex >= 0 then
    begin
      Inc(ITMP);
    end;
  end;
  WriteInteger(ITMP);
  WriteString(' # Data Set 3, Stress Period '
    + IntToStr(StressPeriod+1) + ': ITMP');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet2H(WellBoundary: TMnw2Boundary);
var
  Qn: Double;
  LIFTn: Double;
  Item: TLiftItem;
  LiftIndex: Integer;
  LiftError1: boolean;
  LiftError2: Boolean;
  ScreenObject: TScreenObject;
begin
  LiftError1 := False;
  LiftError2 := False;
  if WellBoundary.AdjustPumping then
  begin
    WellBoundary.LiftValues.Sort;
    for LiftIndex := 0 to WellBoundary.LiftValues.Count - 1 do
    begin
      Item := WellBoundary.LiftValues.Items[LiftIndex] as TLiftItem;
      LIFTn := Item.Lift;
      Qn := Item.Q;
      WriteFloat(LIFTn);
      WriteFloat(Qn);
      WriteString(' Data Set 2H: LIFTn, Qn');
      NewLine;
      if LIFTn > WellBoundary.MaximumLift then
      begin
        LiftError1 := True;
      end;
      if LIFTn < WellBoundary.LiftAtMaxRate then
      begin
        LiftError2 := True;
      end;
    end;
  end;
  if LiftError1 or LiftError2 then
  begin
    ScreenObject := WellBoundary.ScreenObject as TScreenObject;
    if LiftError1 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidMnwTable,
        ScreenObject.Name, ScreenObject);

    end;
    if LiftError2 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidMnwTable2,
        ScreenObject.Name, ScreenObject);

    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSets3and4;
var
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  for StressPeriodIndex := 0 to
    Model.ModflowFullStressPeriods.Count - 1 do
  begin
    StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
    WriteDataSet3(StressPeriod.StartTime, StressPeriodIndex);
    WriteDataSet4(StressPeriod.StartTime);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2G(WellBoundary: TMnw2Boundary);
var
  Hlift: Double;
  HWtol: Double;
  LIFTqmax: Double;
  LIFTq0: Double;
begin
  if WellBoundary.AdjustPumping then
  begin
    Hlift := WellBoundary.ReferenceHead;
    LIFTq0 := WellBoundary.MaximumLift;
    LIFTqmax := WellBoundary.LiftAtMaxRate;
    HWtol := WellBoundary.WellTolerance;
    WriteFloat(Hlift);
    WriteFloat(LIFTq0);
    WriteFloat(LIFTqmax);
    WriteFloat(HWtol);
    WriteString(' Data Set 2G: Hlift, LIFTq0, LIFTqmax, HWtol');
    NewLine;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2F(WellBoundary: TMnw2Boundary);
var
  Qfrcmn: Double;
  Qfrcmx: Double;
  Comment: string;
  QCut: Integer;
  Hlim: Double;
  Item: TMnw2TimeItem;
begin
  if WellBoundary.ConstrainPumping and WellBoundary.ConstantConstraints then
  begin
    Item := WellBoundary.TimeValues[0] as TMnw2TimeItem;
    Hlim := Item.LimitingWaterLevelValue;
    QCut := GetQCut(Item);
    Comment := ' # Data Set 2F: Hlim, QCut';
    WriteFloat(Hlim);
    WriteInteger(QCut);
    if QCut <> 0 then
    begin
      Qfrcmn := Item.InactivationPumpingRateValue;
      Qfrcmx := Item.ReactivationPumpingRateValue;
      Comment := Comment + ', Qfrcmn, Qfrcmx';
      WriteFloat(Qfrcmn);
      WriteFloat(Qfrcmx);
    end;
    WriteString(Comment);
    NewLine;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2E(Well: TMultinodeWell;
  WellBoundary: TMnw2Boundary);
var
  PumpLocation: TCellLocation;
  Zpump: Double;
begin
  if WellBoundary.SpecifyPump then
  begin
    if Well.VerticalWell then
    begin
      Zpump := WellBoundary.PumpElevation;
      WriteFloat(Zpump);
      WriteString(' # Data Set 2E: Zpump');
      NewLine;
    end
    else
    begin
      PumpLocation := WellBoundary.TargetCellLocation(Model);
      WriteInteger(PumpLocation.Layer);
      WriteInteger(PumpLocation.Row);
      WriteInteger(PumpLocation.Column);
      WriteString(' # Data Set 2E: PUMPLAY, PUMPROW, PUMPCOL');
      NewLine;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2D(
  ConstantWellLossParameters: boolean; WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  Comment: string;
  COL: Integer;
  ROW: Integer;
  Zbotm: Double;
  Ztop: Double;
  Cell: TMnw2_Cell;
  PP: Double;
  LAY: Integer;
  CellIndex: Integer;
  ScreenIndex: Integer;
  VerticalScreen: TVerticalScreen;
  Compiler: TRbwParser;
  Formula: string;
  Expression: TExpression;
  Rw: double;
  Rskin: double;
  Kskin: double;
  B: double;
  C: double;
  P: Double;
  CWC: double;
  procedure WriteOptionalData;
  var
    Rw: Double;
    Rskin: Double;
    Kskin: Double;
    B: Double;
    C: Double;
    P: Double;
    CWC: Double;
  begin
    if not ConstantWellLossParameters then
    begin
      case WellBoundary.LossType of
        mltNone: Assert(False);
        mltThiem:
          begin
            Rw := Cell.WellRadius;
            WriteFloat(Rw);
            Comment := Comment +', Rw';
          end;
        mltSkin:
          begin
            Rw := Cell.WellRadius;
            Rskin := Cell.SkinRadius;
            Kskin := Cell.SkinK;
            WriteFloat(Rw);
            WriteFloat(Rskin);
            WriteFloat(Kskin);
            Comment := Comment +', Rw, Rskin, Kskin';
          end;
        mltEquation:
          begin
            Rw := Cell.WellRadius;
            B := Cell.B;
            C := Cell.C;
            P := Cell.P;
            WriteFloat(Rw);
            WriteFloat(B);
            WriteFloat(C);
            WriteFloat(P);
            Comment := Comment +', Rw, B, C, P';
          end;
        mtlSpecify:
          begin
            CWC := Cell.CellToWellConductance;
            WriteFloat(CWC);
            Comment := Comment +', CWC';
          end;
        else Assert(False);
      end;
    end;
  end;
begin
  if Well.VerticalWell and (WellBoundary.LossType <> mltNone) then
  begin
    Cell := Well.Cells[0] as TMnw2_Cell;
    ROW := Cell.Row+1;
    COL := Cell.Column+1;
    if WellBoundary.VerticalScreens.Count > 0 then
    begin
      for ScreenIndex := 0 to WellBoundary.VerticalScreens.Count - 1 do
      begin
        VerticalScreen := WellBoundary.
          VerticalScreens.Items[ScreenIndex] as TVerticalScreen;
        Ztop := VerticalScreen.ZTop;
        Zbotm := VerticalScreen.ZBottom;
        WriteFloat(Ztop);
        WriteFloat(Zbotm);
        WriteInteger(ROW);
        WriteInteger(COL);
        Comment := ' # Data Set 2D; Ztop, Zbotm, ROW, COL';

        Compiler := Model.rpThreeDFormulaCompiler;

        case WellBoundary.LossType of
          mltNone: Assert(False);
          mltThiem:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);
              Comment := Comment +', Rw';
            end;
          mltSkin:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);

              Formula := VerticalScreen.SkinRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rskin', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.SkinRadius then
              begin
                VerticalScreen.SkinRadius := Formula;
              end;
              Rskin := Expression.DoubleResult;
              WriteFloat(Rskin);

              Formula := VerticalScreen.SkinK;
              EvaluateVerticalScreenFormula(Expression, 'Kskin', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.SkinK then
              begin
                VerticalScreen.SkinK := Formula;
              end;
              Kskin := Expression.DoubleResult;
              WriteFloat(Kskin);

              Comment := Comment +', Rw, Rskin, Kskin';
            end;
          mltEquation:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);

              Formula := VerticalScreen.B;
              EvaluateVerticalScreenFormula(Expression, 'B', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.B then
              begin
                VerticalScreen.B := Formula;
              end;
              B := Expression.DoubleResult;
              WriteFloat(B);


              Formula := VerticalScreen.C;
              EvaluateVerticalScreenFormula(Expression, 'C', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.C then
              begin
                VerticalScreen.C := Formula;
              end;
              C := Expression.DoubleResult;
              WriteFloat(C);

              Formula := VerticalScreen.P;
              EvaluateVerticalScreenFormula(Expression, 'P', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.P then
              begin
                VerticalScreen.P := Formula;
              end;
              P := Expression.DoubleResult;
              WriteFloat(P);

              Comment := Comment +', Rw, B, C, P';
            end;
          mtlSpecify:
            begin
              Formula := VerticalScreen.CellToWellConductance;
              EvaluateVerticalScreenFormula(Expression, 'CWC', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.CellToWellConductance then
              begin
                VerticalScreen.CellToWellConductance := Formula;
              end;
              CWC := Expression.DoubleResult;
              WriteFloat(CWC);

              Comment := Comment +', CWC';
            end;
          else Assert(False);
        end;
        WriteString(Comment);
        NewLine;
      end;
    end
    else
    begin
      Well.GetVerticalWellElevations(Ztop, Zbotm);
      WriteFloat(Ztop);
      WriteFloat(Zbotm);
      WriteInteger(ROW);
      WriteInteger(COL);
      Comment := ' # Data Set 2D; Ztop, Zbotm, ROW, COL';
      WriteOptionalData;
      WriteString(Comment);
      NewLine;
    end;
  end
  else
  begin
    for CellIndex := 0 to Well.CellCount - 1 do
    begin
      Cell := Well.Cells[CellIndex] as TMnw2_Cell;
      LAY := Model.DataSetLayerToModflowLayer(Cell.Layer);
//      LAY := Cell.Layer+1;
      ROW := Cell.Row+1;
      COL := Cell.Column+1;
      WriteInteger(LAY);
      WriteInteger(ROW);
      WriteInteger(COL);
      Comment := ' # Data Set 2D; LAY, ROW, COL';
      WriteOptionalData;
      if WellBoundary.PartialPenetrationCorrection then
      begin
        PP := Cell.PartialPenetration;
        WriteFloat(PP);
        Comment := Comment + ', PP';
      end;
      WriteString(Comment);
      NewLine;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2C(
  ConstantWellLossParameters: Boolean; WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  CWC: Double;
  P: Double;
  C: Double;
  B: Double;
  Kskin: Double;
  Rskin: Double;
  Rw: Double;
  Cell: TMnw2_Cell;
begin
  case WellBoundary.LossType of
    mltNone:
      begin
      end;
    mltThiem:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
        end
        else
        begin
          Rw := -1;
        end;
        WriteFloat(Rw);
        WriteString(' # Data Set 2C; Rw');
        NewLine;
      end;
    mltSkin:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
          Rskin := Cell.SkinRadius;
          Kskin := Cell.SkinK;
        end
        else
        begin
          Rw := -1;
          Rskin := -1;
          Kskin := -1;
        end;
        WriteFloat(Rw);
        WriteFloat(Rskin);
        WriteFloat(Kskin);
        WriteString(' # Data Set 2C; Rw, Rskin, Kskin');
        NewLine;
      end;
    mltEquation:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
          B := Cell.B;
          C := Cell.C;
          P := Cell.P;
        end
        else
        begin
          Rw := -1;
          B := -1;
          C := -1;
          P := -1;
        end;
        WriteFloat(Rw);
        WriteFloat(B);
        WriteFloat(C);
        WriteFloat(P);
        WriteString(' # Data Set 2C; Rw, B, C, P');
        NewLine;
      end;
    mtlSpecify:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          CWC := Cell.CellToWellConductance;
        end
        else
        begin
          CWC := -1;
        end;
        WriteFloat(CWC);
        WriteString(' # Data Set 2C; CWC');
        NewLine;
      end;
  else
    Assert(False);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2B(WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  Qlimit: Integer;
  PUMPLOC: Integer;
  LOSSTYPE: string;
  PUMPCAP: Integer;
  PPFLAG: Integer;
  ItemIndex: Integer;
  TimeItem: TMnw2TimeItem;
  LiftItem: TLiftItem;
  ScreenObject: TScreenObject;
const
  LossTypes: array[Low(TMnwLossType)..High(TMnwLossType)] of string =
    ('NONE ', 'THIEM ', 'SKIN ', 'GENERAL ', 'SPECIFYcwc ');
begin
  if WellBoundary.LossType = mltNone then
  begin
    if NNODES <> 1 then
    begin
      frmErrorsAndWarnings.AddError(Model,
        LossTypeError, Well.FScreenObject.Name, Well.FScreenObject);
    end;
  end;
  LOSSTYPE := LossTypes[WellBoundary.LossType];
  if WellBoundary.SpecifyPump then
  begin
    if Well.VerticalWell then
    begin
      PUMPLOC := -1;
    end
    else
    begin
      PUMPLOC := 1;
    end;
  end
  else
  begin
    PUMPLOC := 0;
  end;
  if WellBoundary.ConstrainPumping then
  begin
    if WellBoundary.ConstantConstraints then
    begin
      Qlimit := 1;
    end
    else
    begin
      Qlimit := -1;
    end;
  end
  else
  begin
    Qlimit := 0;
  end;
  if WellBoundary.PartialPenetrationCorrection then
  begin
    PPFLAG := 1;
  end
  else
  begin
    PPFLAG := 0;
  end;
  if WellBoundary.AdjustPumping then
  begin
    PUMPCAP := WellBoundary.LiftValues.Count;
  end
  else
  begin
    PUMPCAP := 0;
  end;
  WriteString(LOSSTYPE);
  WriteInteger(PUMPLOC);
  WriteInteger(Qlimit);
  WriteInteger(PPFLAG);
  WriteInteger(PUMPCAP);
  WriteString(' # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP');
  NewLine;

  if PUMPCAP > 0 then
  begin
    for ItemIndex := 0 to WellBoundary.TimeValues.Count - 1 do
    begin
      TimeItem := WellBoundary.TimeValues.Items[ItemIndex] as TMnw2TimeItem;
      if TimeItem.PumpingRateValue < 0 then
      begin
        LiftItem := WellBoundary.LiftValues.Items[
          WellBoundary.LiftValues.Count-1] as TLiftItem;
        if Abs(TimeItem.PumpingRateValue)
          < LiftItem.Q*TimeItem.HeadCapacityMultiplierValue then
        begin
          ScreenObject := WellBoundary.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model, StrWhenPumpingCapacit,
            ScreenObject.Name, ScreenObject);
        end;
        break;
      end;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2A(WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  WELLID: string;
begin
  WELLID := Trim(WellBoundary.WellID);
  if FWellNames.IndexOf(WELLID) >= 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrTheFollowingObject,
      Format(StrObject0sWELLI, [Well.FScreenObject.Name, WELLID]),
      Well.FScreenObject);
  end;
  if WELLID = '' then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoWellID,
      Format(StrObject, [Well.FScreenObject.Name]),
      Well.FScreenObject);
  end;
  FWellNames.Add(WELLID);
  AdjustWellID(WELLID);
  NNODES := Well.FCells.Count;
  if Well.VerticalWell then
  begin
    if WellBoundary.LossType = mltNone then
    begin
      if WellBoundary.VerticalScreens.Count > 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrVerticalScreensAre,
          Format(StrObject0sWELLI, [Well.FScreenObject.Name, WELLID]),
          Well.FScreenObject);
      end;
    end;
    if WellBoundary.VerticalScreens.Count > 0 then
    begin
      NNODES := -WellBoundary.VerticalScreens.Count;
    end
    else if WellBoundary.LossType <> mltNone then
    begin
      NNODES := -1;
    end;
  end;
  WriteString(WELLID);
  WriteInteger(NNODES);
  WriteString(' # Data Set 2A: WELLID, NNODES');
  NewLine;
end;

{ TMultinodeWell }

function TMultinodeWell.ConstantWellLossParameters: boolean;
var
  WellRadius: double;
  Cell: TMnw2_Cell;
  CellIndex: Integer;
  SkinRadius: Double;
  SkinK: Double;
  B: Double;
  C: Double;
  CellToWellConductance: Double;
  P: Double;
begin
  Assert(CellCount> 0);
  Result := True;
  case WellBoundary.LossType of
    mltNone: result := True;
    mltThiem:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := WellRadius = Cell.WellRadius;
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mltSkin:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        SkinRadius := Cell.SkinRadius;
        SkinK := Cell.SkinK;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := (WellRadius = Cell.WellRadius)
            and (SkinRadius = Cell.SkinRadius)
            and (SkinK = Cell.SkinK);
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mltEquation:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        B := Cell.B;
        C := Cell.C;
        P := Cell.P;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := (WellRadius = Cell.WellRadius)
            and (B = Cell.B)
            and (C = Cell.C)
            and (P = Cell.P);
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mtlSpecify:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        CellToWellConductance := Cell.CellToWellConductance;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := CellToWellConductance = Cell.CellToWellConductance;
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

destructor TMultinodeWell.Destroy;
begin
  FCells.Free;
  inherited;
end;

class function TMultinodeWell.IsScreenObjectVertical(
  AScreenObject: TScreenObject): Boolean; 
begin
  result := (AScreenObject.Count = 1)
    and (AScreenObject.ViewDirection = vdTop)
    and (AScreenObject.ElevationCount = ecTwo);
end;

function TMultinodeWell.GetCell(Index: integer): TValueCell;
begin
  result := FCells[Index];
end;

function TMultinodeWell.GetCellCount: integer;
begin
  result := FCells.Count;
end;

procedure TMultinodeWell.GetVerticalWellElevations(out Ztop, Zbotm: double);
begin
  Zbotm := FScreenObject.BottomElevation;
  Ztop := FScreenObject.TopElevation;
end;

function TMultinodeWell.VerticalWell: boolean;
begin
  Result := IsScreenObjectVertical(FScreenObject);
end;

function TMultinodeWell.WellBoundary: TMnw2Boundary;
begin
  result := FScreenObject.ModflowMnw2Boundary;
  assert(result <> nil);
end;

end.
