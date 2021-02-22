unit ModflowRCH_WriterUnit;

interface

uses System.UITypes,Windows, SysUtils, Classes, Contnrs,
  CustomModflowWriterUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, ModflowPackageSelectionUnit, OrderedCollectionUnit,
  ModflowCellUnit, PhastModelUnit, ModflowBoundaryDisplayUnit, Vcl.Dialogs,
  DataSetUnit, SparseDataSets, Modflow6ObsUnit;

Type
  TModflowRCH_Writer = class(TCustomTransientArrayWriter)
  private
    NPRCH: integer;
    NRCHOP: integer;
    FRchPackage: TRchPackageSelection;
    FTimeIndex: Integer;
    FAbbreviation: string;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To8;
    procedure WriteCells(CellList: TValueCellList; const DataSetIdentifier,
      VariableIdentifiers: string);
    procedure WriteOptions(InputFileName: string);
    procedure WriteDimensions;
    procedure WriteCellsMF6(RchRateList: TValueCellList);
    procedure WriteFileInternal;
  protected
    function CellType: TValueCellType; override;
    function Prefix: string; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    procedure Evaluate; override;
    procedure HandleMissingArrayData; override;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; override;
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; override;
    function ObsType: string; override;
    function Mf6ObservationsUsed: Boolean; override;
    Class function Mf6ObType: TObGeneral; override;
  public
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    // @name is the file extension used for the observation output file.
    class function ObservationOutputExtension: string; override;
  end;

implementation

uses RbwParser, ModflowUnitNumbers, ModflowTransientListParameterUnit,
  frmErrorsAndWarningsUnit, ModflowRchUnit, GoPhastTypes,
  frmProgressUnit, Forms, ModflowOutputControlUnit, System.Math,
  SparseArrayUnit;

resourcestring
  StrNoRechargeDefined = 'No recharge defined';
  StrTheRechargePackage = 'The recharge package is active but no recharge ha' +
  's been defined for any stress period.';
  StrWritingRCHPackage = 'Writing RCH Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to8 = '  Writing Data Sets 5 to 8.';
  StrWritingStressP = '    Writing Stress Period %d';
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  StrNoParametersHaveB = 'No parameters have been defined for the Recharge p' +
  'ackage for the following Stress periods.';
  StrTheRechargeRatesA = 'The recharge rates are negative in the following c' +
  'ells. Although allowed by MODFLOW, negative recharge rates are often due ' +
  'to errors in specifying the rechaarge rate. If this is the recharge rate ' +
  'for a parameter and multiple parameters appply to the same cell, the ' +
  'recharge may not be negative because the rates for each parameter are ' +
  'added together';
  StrRowColumn0d = '(Row,Column) = (%0:d, %1:d)' + sLineBreak + '%2:s';
  StrRechargeHasNotBee = 'Recharge has not been defined in one or more stres' +
  's periods';

{ TModflowRCH_Writer }

function TModflowRCH_Writer.CellType: TValueCellType;
begin
  result := TRch_Cell;
end;

procedure TModflowRCH_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TRchBoundary;
begin
  inherited Evaluate;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheRechargeRatesA);
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowRchBoundary;
    if Boundary <> nil then
    begin
      Boundary.GetRechargeLayerCells(FLayers, Model);
      Boundary.RechargeLayers.ClearBoundaries(Model);
    end;
  end;
end;

class function TModflowRCH_Writer.Extension: string;
begin
  result := '.rch';
end;

function TModflowRCH_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowRchBoundary;
end;

procedure TModflowRCH_Writer.HandleMissingArrayData;
begin
  inherited;
  frmErrorsAndWarnings.AddWarning(Model, StrRechargeHasNotBee,
    Format(StrStressPeriodD, [FTimeIndex+1]));
end;

function TModflowRCH_Writer.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := (AScreenObject.Modflow6Obs <> nil)
    and (ogRch in AScreenObject.Modflow6Obs.General);
end;

class function TModflowRCH_Writer.Mf6ObType: TObGeneral;
begin
  result := ogRch;
end;

class function TModflowRCH_Writer.ObservationExtension: string;
begin
  result := '.ob_rch';
end;

class function TModflowRCH_Writer.ObservationOutputExtension: string;
begin
  result := '.ob_rch_out';
end;

function TModflowRCH_Writer.Mf6ObservationsUsed: Boolean;
begin
  result := (Model.ModelSelection = msModflow2015)
    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

function TModflowRCH_Writer.ObsType: string;
begin
  result := 'rch';
end;

function TModflowRCH_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.RchPackage;
end;

function TModflowRCH_Writer.ParameterType: TParameterType;
begin
  result := ptRCH;
end;

function TModflowRCH_Writer.Prefix: string;
begin
  result := 'R'
end;

procedure TModflowRCH_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  List: TValueCellList;
  ParameterValues: TList;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  Comment: string;
  ParamDefArrays: TList;
  RechRateTimes: TModflowBoundaryDisplayTimeList;
  RechLayerTimes: TModflowBoundaryDisplayTimeList;
  RechRateArray: TModflowBoundaryDisplayDataArray;
  RechLayerArray: TModflowBoundaryDisplayDataArray;
  DefArrayList: TList;
  Index: Integer;
  ATimeList: TModflowBoundaryDisplayTimeList;
const
  D7PNameIname = '';
  D7PName = '';
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

   try
    ParameterValues := TList.Create;
    try
      Evaluate;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ClearTimeLists(Model);
      ParamDefArrays := TObjectList.Create;
      try
        EvaluateParameterDefinitions(ParamDefArrays, StrOneOrMoreSParam,
          Model.ModflowPackages.RchPackage.AssignmentMethod);
        NPRCH := ParameterCount;
        NRCHOP := Ord(Model.ModflowPackages.RchPackage.LayerOption) + 1;
        RechRateTimes := TimeLists[0];
        RechLayerTimes := TimeLists[1];

        Comment := '# Data Set 8: IRCH';
        if Values.Count = 0 then
        begin
          SetTimeListsUpToDate(TimeLists);
          Exit;
        end;
        for TimeIndex := 0 to Values.Count - 1 do
        begin
          RechRateArray := RechRateTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          if RechLayerTimes = nil then
          begin
            RechLayerArray := nil;
          end
          else
          begin
            RechLayerArray := RechLayerTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
          end;

          ParametersUsed := TStringList.Create;
          try
            RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
              ParametersUsed, ParameterValues, True);
            List := Values[TimeIndex];
  //          List.CheckRestore;

            if NPRCH = 0 then
            begin
              // data set 6
              AssignTransient2DArray(RechRateArray, 0, List, 0, rdtDouble,
                Model.ModflowPackages.RchPackage.AssignmentMethod);
            end
            else
            begin
              // data set 7
              DefArrayList := ParamDefArrays[TimeIndex];
              UpdateTransient2DArray(RechRateArray, DefArrayList);
            end;
            Model.AdjustDataArray(RechRateArray);
            RechRateArray.CacheData;

            // Data set 8
            if RechLayerArray <> nil then
            begin
              if (Model.ModflowPackages.RchPackage.
                LayerOption = loSpecified)
                and not Model.ModflowPackages.RchPackage.
                TimeVaryingLayers and (ParameterCount > 0) then
              begin
                RetrieveParametersForStressPeriod(D7PNameIname, D7PName, 0,
                  ParametersUsed, ParameterValues, True);
                List := Values[0];
              end;
              UpdateLayerDisplay(List, ParameterValues, TimeIndex,
                RechLayerArray);
              RechLayerArray.CacheData;
            end;
            List.Cache;
          finally
            ParametersUsed.Free;
          end;
        end;
        for Index := 0 to TimeLists.Count - 1 do
        begin
          ATimeList := TimeLists[Index];
          if ATimeList <> nil then
          begin
            ATimeList.SetUpToDate(True);
          end;
        end;
      finally
        ParamDefArrays.Free;
      end;
    finally
      ParameterValues.Free;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TModflowRCH_Writer.WriteDataSet1;
begin
  NPRCH := ParameterCount;
  if NPRCH > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPRCH);
    WriteString(' # PARAMETER NPRCH');
    NewLine;
  end;
end;

procedure TModflowRCH_Writer.WriteDataSet2;
var
  IRCHCB: integer;
begin
  NRCHOP := Ord(Model.ModflowPackages.RchPackage.LayerOption) + 1;
  GetFlowUnitNumber(IRCHCB);

  WriteInteger(NRCHOP);
  WriteInteger(IRCHCB);
  WriteString(' # DataSet 2: NRCHOP IRCHCB');
  NewLine
end;

procedure TModflowRCH_Writer.WriteDataSets3And4;
const
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NCLU';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, FRchPackage.AssignmentMethod,
    FRchPackage.MultiplierArrayNames, FRchPackage.ZoneArrayNames);
end;

procedure TModflowRCH_Writer.WriteDataSets5To8;
const
  D7PName =      ' # Data Set 7: PARNAM IRCHPF';
  D7PNameIname = ' # Data Set 7: PARNAM Iname IRCHPF';
  DS5 = ' # Data Set 5: INRECH INIRCH';
  DataSetIdentifier = 'Data Set 6:';
var
  VariableIdentifiers: string;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    VariableIdentifiers := 'RECHARGE';
  end
  else
  begin
    VariableIdentifiers := 'RECH';
  end;

  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowRCH_Writer.WriteDimensions;
//var
//  MAXBOUND: Integer;
var
  RchRateList: TValueCellList;
  TimeIndex: Integer;
  MAXBOUND: Integer;
  NPRCH: Integer;
  MXL: Integer;
//  NETSEG: integer;
begin
  // Layered data doesn't use the Dimensions block.
//  Exit;

  Assert(Model.ModelSelection = msModflow2015);
  WriteBeginDimensions;

  MAXBOUND := 0;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    RchRateList := Values[TimeIndex];
    MAXBOUND := Max(MAXBOUND, CountCellsMF6(RchRateList, FRchPackage.LayerOption));
  end;

  CountParametersAndParameterCells(NPRCH, MXL);
  MAXBOUND := MAXBOUND + MXL;

//  MAXBOUND := Model.Grid.ColumnCount * Model.Grid.RowCount;
  WriteString('  MAXBOUND');
  WriteInteger(MAXBOUND);
  NewLine;

  WriteEndDimensions;
end;
//  Assert(Model.ModelSelection = msModflow2015);
//  Exit;
//
//  // Dimensions not used for structured models.
//  WriteBeginDimensions;
//
////  MAXBOUND := Model.Grid.ColumnCount * Model.Grid.RowCount;
////  WriteString('  MAXBOUND');
////  WriteInteger(MAXBOUND);
////  NewLine;
//
//  WriteEndDimensions;
//end;

procedure TModflowRCH_Writer.WriteFile(const AFileName: string);
begin
//  OutputDebugString('SAMPLING ON');
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRechargeHasNotBee);

  if not Package.IsSelected then
  begin
    Exit
  end;

  if Model.ModelSelection = msModflow2015 then
  begin
    FAbbreviation := 'RCH6';
  end
  else
  begin
    FAbbreviation := StrRCH;
  end;

  if Model.PackageGeneratedExternally(FAbbreviation) then
  begin
    Exit;
  end;

  FRchPackage := Package as TRchPackageSelection;
  FRchPackage.MultiplierArrayNames.Clear;
  FRchPackage.ZoneArrayNames.Clear;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(FAbbreviation, Model.UnitNumbers.UnitNumber(StrRCH),
    FNameOfFile, foInput, Model);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  ClearTimeLists(Model);

  WriteFileInternal;
//  OpenFile(FileName(AFileName));
//  try
//    frmProgressMM.AddMessage(StrWritingRCHPackage);
//    frmProgressMM.AddMessage(StrWritingDataSet0);
//    WriteDataSet0;
//    Application.ProcessMessages;
//    if not frmProgressMM.ShouldContinue then
//    begin
//      Exit;
//    end;
//
//    if Model.ModelSelection <> msModflow2015 then
//    begin
//      frmProgressMM.AddMessage(StrWritingDataSet1);
//      WriteDataSet1;
//      Application.ProcessMessages;
//      if not frmProgressMM.ShouldContinue then
//      begin
//        Exit;
//      end;
//
//      frmProgressMM.AddMessage(StrWritingDataSet2);
//      WriteDataSet2;
//      Application.ProcessMessages;
//      if not frmProgressMM.ShouldContinue then
//      begin
//        Exit;
//      end;
//
//      frmProgressMM.AddMessage(StrWritingDataSets3and4);
//      WriteDataSets3And4;
//      Application.ProcessMessages;
//      if not frmProgressMM.ShouldContinue then
//      begin
//        Exit;
//      end;
//    end
//    else
//    begin
//      WriteOptions(FNameOfFile);
//      WriteDimensions;
//    end;
//
//    frmProgressMM.AddMessage(StrWritingDataSets5to8);
//    WriteDataSets5To8;
//  finally
//    CloseFile;
////    Clear;
//  end;

  if Model.ModelSelection = msModflow2015 then
  begin
    WriteModflow6FlowObs(FNameOfFile, FEvaluationType);
  end;

  if (Model.ModelSelection = msModflow2015) and Model.PestUsed
    and (FParamValues.Count > 0) then
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
  //  OutputDebugString('SAMPLING OFF');
end;

procedure TModflowRCH_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingRCHPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if Model.ModelSelection <> msModflow2015 then
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

      frmProgressMM.AddMessage(StrWritingDataSets3and4);
      WriteDataSets3And4;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end
    else
    begin
      WriteOptions(FNameOfFile);
      WriteDimensions;
      WriteBoundaryArrayParams;
    end;


    frmProgressMM.AddMessage(StrWritingDataSets5to8);
    WriteDataSets5To8;
  finally
    CloseFile;
//    Clear;
  end;
end;

procedure TModflowRCH_Writer.WriteOptions(InputFileName: string);
begin
  Assert(Model.ModelSelection = msModflow2015);
  WriteBeginOptions;

//  WriteString('  READASARRAYS');
//  NewLine;
  WriteString('  AUXILIARY IFACE');
  NewLine;

  PrintOutputOptions;

  WriteBoundNamesOption;

  if FRchPackage.LayerOption <> loTopActive then
  begin
    WriteString('  FIXED_CELL');
    NewLine;
  end;

  WriteMF6ObsOption(InputFileName);
  WriteMf6ParamListOption;

  WriteEndOptions;
end;

procedure TModflowRCH_Writer.WriteCells(CellList: TValueCellList;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  RechargeRate: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  ActiveDataArray: TDataArray;
  LayerIndex: Integer;
  AValue: Double;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;

  try
    WriteTransient2DArray(Comment, DataTypeIndex, DataType,
      DefaultValue, CellList, Model.ModflowPackages.RchPackage.AssignmentMethod,
      True, RechargeRate, VariableIdentifiers,False);

    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    for RowIndex := 0 to RechargeRate.RowCount - 1 do
    begin
      for ColIndex := 0 to RechargeRate.ColumnCount - 1 do
      begin
        AValue := RechargeRate.RealData[0,RowIndex,ColIndex];
        if AValue < 0 then
        begin
          for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
          begin
            if ActiveDataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrTheRechargeRatesA,
                Format(StrRowColumn0d, [RowIndex+1, ColIndex+1,
                RechargeRate.Annotation[0,RowIndex,ColIndex]]));
              break;
            end;
          end;
        end;
      end;
    end;
  finally
    RechargeRate.Free;
  end;
end;

procedure TModflowRCH_Writer.WriteCellsMF6(RchRateList: TValueCellList);
var
  CellIndex: Integer;
  RchCell: TRch_Cell;
  IDomain: TDataArray;
  UsedLocations: T2DSparseBooleanArray;
  Layer: Integer;
  ParameterName: string;
  MultiplierValue: double;
begin
    { TODO -cPEST : Add PEST support for PEST here }
    // handle pest parameter
    // handle multiply or add
  IDomain := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
//  Assert(DepthSurfaceCellList.Count = RchRateList.Count);
  UsedLocations := T2DSparseBooleanArray.Create(SPASmall, SPASmall);
  try
    for CellIndex := RchRateList.Count - 1 downto 0 do
    begin
      RchCell := RchRateList[CellIndex] as TRch_Cell;
      if (FRchPackage.LayerOption = loTop) and (RchCell.Layer <> 0) then
      begin
        RchCell.Layer := 0;
      end;
      Layer := RchCell.Layer;
      if OkLocationMF6(IDomain, UsedLocations, Layer, RchCell.Row,
        RchCell.Column, FRchPackage.LayerOption) then
      begin
        UsedLocations.Items[RchCell.Row, RchCell.Column] := True;
        WriteInteger(Layer+1);
        if not Model.DisvUsed then
        begin
          WriteInteger(RchCell.Row+1);
        end;
        WriteInteger(RchCell.Column+1);

        if Model.PestUsed and (Model.ModelSelection = msModflow2015)
          and WritingTemplate
          and ( RchCell.RechargeParameterName <> '') then
        begin
          ParameterName := RchCell.RechargeParameterName;
          if RchCell.RechargeParameterValue = 0 then
          begin
            MultiplierValue := 0.0;
          end
          else
          begin
            MultiplierValue := RchCell.RechargeRate / RchCell.RechargeParameterValue;
          end;
          WriteTemplateFormula(ParameterName, MultiplierValue, ppmMultiply);
        end
        else
        begin
          WriteFloat(RchCell.RechargeRate);
        end;


//        if RchCell.TimeSeriesName = '' then
//        begin
//          WriteFloat(RchCell.RechargeRate);
//        end
//        else
//        begin
//          WriteString(' ');
//          WriteString(RchCell.TimeSeriesName);
//          WriteString(' ');
//        end;

        WriteIface(RchCell.IFace);

        WriteBoundName(RchCell);

        NewLine;
      end;
    end;
  finally
    UsedLocations.Free;
  end;

end;

procedure TModflowRCH_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  NP: Integer;
  RechRateList, PriorRechRateList: TValueCellList;
  ParameterValues: TValueCellList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  INRECH, INIRCH: Integer;
  Comment: string;
  RchRateList: TValueCellList;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParameterValuesMf6: TList;
  ErrorMessage: string;
begin
  inherited;
  ErrorMessage := Format(StrOneOrMoreSParam, [' RCH']);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParametersHaveB);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoRechargeDefined);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorMessage);
    ParameterValues := TValueCellList.Create(CellType);
    try
      ParameterValues.OwnsObjects := False;
      Comment := '# Data Set 8: IRCH';
      if Values.Count = 0 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrNoRechargeDefined,
          StrTheRechargePackage);
      end;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        FTimeIndex := TimeIndex;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        frmProgressMM.AddMessage(Format(StrWritingStressP, [TimeIndex+1]));
        ParametersUsed := TStringList.Create;
        try
          if Model.ModelSelection = msModflow2015 then
          begin
            WriteBeginPeriod(TimeIndex);
            RchRateList := Values[TimeIndex];

            if RchRateList.Count > 0 then
            begin
              WriteCellsMF6(RchRateList);
            end;

            for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
            begin
              Application.ProcessMessages;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
              Param := Model.ModflowTransientParameters[ParamIndex];
              if Param.ParameterType = ParameterType then
              begin
                Position := ParamValues.IndexOf(Param.ParameterName);
                // The parameter name is erased from ParamValues in
                // CountParametersAndParameterCells if there are no cells
                // associated with it.
                if Position < 0 then
                begin
                  frmErrorsAndWarnings.AddWarning(Model,
                    ErrorMessage, Param.ParameterName);
                  Continue;
                end;
                ParameterValuesMf6 := ParamValues.Objects[Position] as TList;
                RchRateList := ParameterValuesMf6[TimeIndex];
                WriteCellsMF6(RchRateList);

              end;
            end;

            WriteEndPeriod;
            Continue;
          end;

          RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
            ParametersUsed, ParameterValues, True);

          NP := ParametersUsed.Count;
          RechRateList := Values[TimeIndex];
          // data set 5;
          if NPRCH > 0 then
          begin
            INRECH := NP;
          end
          else
          begin
           if (TimeIndex > 0) then
            begin
              PriorRechRateList := Values[TimeIndex-1];
              if PriorRechRateList.AreRealValuesIdentical(RechRateList, 0) then
              begin
                INRECH := -1;
  //              RechRateList.Cache;
              end
              else
              begin
                INRECH := 1;
              end;
              PriorRechRateList.Cache;
            end
            else
            begin
              INRECH := 1;
            end;
          end;
          INIRCH := 1;

          if Model.ModelSelection <> msModflow2015 then
          begin
            WriteInteger(INRECH);
            WriteInteger(INIRCH);
            WriteString(DS5 + ' Stress period ' + IntToStr(TimeIndex+1));
            NewLine;
          end;

          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;

          WriteBeginPeriod(TimeIndex);

          if INRECH > 0 then
          begin
            if NPRCH = 0 then
            begin
              // data set 6
              WriteCells(RechRateList, DataSetIdentifier, VariableIdentifiers);
            end
            else
            begin
              // data set 7
              if ParametersUsed.Count = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, StrNoParametersHaveB,
                  IntToStr(TimeIndex+1));
              end;
              for ParamIndex := 0 to ParametersUsed.Count - 1 do
              begin
                WriteString(ParametersUsed[ParamIndex]);
                NewLine;
              end;
            end;
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              RechRateList.Cache;
              Exit;
            end;
          end;

          // Data set 8
          WriteLayerSelection(RechRateList, ParameterValues, TimeIndex, Comment, 'IRCH');
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            RechRateList.Cache;
            Exit;
          end;
          if TimeIndex = Values.Count - 1 then
          begin
            RechRateList.Cache;
          end;

          WriteEndPeriod;

        finally
          ParametersUsed.Free;
        end;
      end;
    finally
      ParameterValues.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

end.
