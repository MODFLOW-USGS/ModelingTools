unit Mt3dmsBtnWriterUnit;

interface

uses System.UITypes,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms, PhastModelUnit,
  ModflowTimeUnit, Classes, Winapi.Windows;

type
  TMt3dmsBtnWriter = class(TCustomModflowWriter)
  private
//    FNameOfFile: string;
    NOBS: integer;
    FMt3dBasic: TMt3dBasic;
    procedure WriteDataSets1And2;
    procedure WriteDataSet3Options;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet10;
    procedure WriteDataSet11;
    procedure WriteDataSet12;
    // write initial concentrations
    procedure WriteDataSet13;
    procedure WriteDataSet14;
    procedure WriteDataSet15;
    procedure WriteDataSet16;
    procedure WriteDataSet17;
    procedure WriteDataSet18;
    procedure WriteDataSet19;
    procedure WriteDataSet20;
    procedure WriteDataSet21to23;
  protected
    class function Extension: string; override;
  public
    procedure WriteDataSet22(StressPeriod: TModflowStressPeriod);
    procedure WriteDataSet23(StressPeriod: TModflowStressPeriod);
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

resourcestring
  StrTimeDataForMT3DMS = 'Time data for MT3DMS or MT3D-USGS undefined';
  StrNoTimeDataHasBee = 'No time data has been defined for stress period %d.' +
  '  Select Model|MODFLOW Time... to define it';

implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  ModflowUnitNumbers, frmProgressUnit, SysUtils, GoPhastTypes,
  DataSetUnit, Mt3dmsChemSpeciesUnit, ModflowOutputControlUnit, Mt3dmsTimesUnit,
  frmErrorsAndWarningsUnit, ReadModflowArrayUnit, Dialogs;

resourcestring
  StrSInTheMT3DMSBTN = '%s in the MT3DMS or MT3D-USGS BTN package';
  StrDZLayerD = 'Data Set 10: DZ Layer: %d';
  StrPRSITYLayerD = 'Data Set 11: PRSITY, Layer: %d';
  StrICBUNDLayerD = 'Data Set 12: ICBUND, Layer: %d';
  StrDataSet14CINACT = ' # Data Set 14: CINACT, THKMIN';
  StrMaximumNumberOfOb = 'Maximum number of observation locations exceeded.';
  StrByDefaultMT3DMSO = 'By default, MT3DMS or MT3D-USGS only allows up to 200 observatio' +
  'n locations to be specified using %s. You must either specify fewer locat' +
  'ions or modify the MT3DMS or MT3D-USGS source code and recompile to overcome this limi' +
  'tation. Another option is to use the TOB package. The limitation does not '
  + 'apply to observations specified through the TOB package.';
  StrWritingMT3DMSBTNP = 'Writing MT3DMS or MT3D-USGS BTN Package input.';
  StrWritingDataSets1and2 = '  Writing Data Sets 1 and 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
//  StrWritingDataSet7 = '  Writing Data Set 7.';
//  StrWritingDataSet8 = '  Writing Data Set 8.';
//  StrWritingDataSet9 = '  Writing Data Set 9.';
//  StrWritingDataSet10 = '  Writing Data Set 10.';
//  StrWritingDataSet11 = '  Writing Data Set 11.';
//  StrWritingDataSet12 = '  Writing Data Set 12.';
//  StrWritingDataSet13 = '  Writing Data Set 13.';
//  StrWritingDataSet14 = '  Writing Data Set 14.';
//  StrWritingDataSet15 = '  Writing Data Set 15.';
//  StrWritingDataSet16 = '  Writing Data Set 16.';
//  StrWritingDataSet17 = '  Writing Data Set 17.';
//  StrWritingDataSet18 = '  Writing Data Set 18.';
//  StrWritingDataSet19 = '  Writing Data Set 19.';
//  StrWritingDataSet20 = '  Writing Data Set 20.';
//  StrWritingDataSet21 = '  Writing Data Set 21.';
  StrTheFile0sWas = 'The file "%0:s" was designated as the file containing i' +
  'nitial concentrations for the species "%1:s". However, the file does not ' +
  'exist.';
  StrInitialConcentratio = 'Initial concentration file does not exist.';
  StrTheSpecifiedStress = 'The specified stress period, time step, and trans' +
  'port step were not found in the file "%s".';
  StrMissingTransportSt = 'Missing transport step in initial concentration f' +
  'ile.';
  StrDataForLayer0d = 'Data for layer %0:d in the specified stress period, t' +
  'ime step, and transport step were not found in the file "%1:s".';
  StrMissingDataInInit = 'Missing data in initial concentration file';
  StrTheNumberOfRowsO = 'The number of rows or columns in the file "%s" do n' +
  'ot match the number in the model.';
  StrIncorrectNumberOf = 'Incorrect number of rows or columns in initial con' +
  'centration file.';
  StrDataSet13SCONC = 'Data Set 13: SCONC: %0:s, Layer: %1:d';
  StrSomethingIsWrongW = 'Something is wrong with the file you specied for t' +
  'he initial concentration. Please check that you used the right version of' +
  ' MT3DMS or MT3D-USGS to create the file. The file is namead %s.';
  StrTheInitialConcentr = 'The initial concentration file that you specified' +
  ', "%s" can not be used because it is empty.';
  StrAllCellsInactiveI = 'All cells inactive in MT3DMS or MT3D-USGS';
  StrEveryCellInTheMo = 'Every cell in the model is inactive for MT3DMS or MT3D-USGS as s' +
  'pecified in the %s data set';
  StrNoMobileSpeciesDe = 'No mobile species defined';
  StrYouMustDefineAtL = 'You must define at least one mobile species to use ' +
  'MT3DMS.';

{ TMt3dmsBtnWriter }

constructor TMt3dmsBtnWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FMt3dBasic := Model.ModflowPackages.Mt3dBasic;
  FArrayWritingFormat := awfMt3dms;
end;

class function TMt3dmsBtnWriter.Extension: string;
begin
  result := '.btn';
end;

procedure TMt3dmsBtnWriter.WriteDataSet10;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(rsMT3DMS_Layer_Thickness);
  DataArray.Initialize;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, Format(StrDZLayerD,
        [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'DZ');
    end;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet11;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(rsPorosity);
  DataArray.Initialize;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, Format(StrPRSITYLayerD,
        [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'PRSITY');
    end;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet12;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrMT3DMSActive);
  DataArray.Initialize;

  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrAllCellsInactiveI);
  if (DataArray.IsUniform = iuTrue) and (not DataArray.BooleanData[0,0,0]) then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrAllCellsInactiveI,
      Format(StrEveryCellInTheMo, [DataArray.DisplayName]));
  end;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, Format(StrICBUNDLayerD,
        [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'ICBUND');
    end;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet13;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  StressPeriod: integer;
  TimeStep: integer;
  TransportStep: integer;
  InitConcUnit: Integer;
  procedure WriteInitialConc;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
    FileName: string;
    ErrorMessage: string;
    AFileStream: TFileStream;
    APrecision: TModflowPrecision;
    KPER: Integer;
    KSTP: Integer;
    NTRANS: Integer;
    TOTIM: TModflowDouble;
    DESC: TModflowDesc;
    NCOL, NROW, ILAY: Integer;
    AnArray: TModflowDoubleArray;
    DataWritten: Boolean;
    LIndex: Integer;
    ColIndex: Integer;
    RowIndex: Integer;
    NeedNewLine: Boolean;
    RelFileName: string;

  begin
    DataWritten := False;
    if Item.UseInitialConcentrationFile then
    begin
      FileName := Item.InitialConcentrationFileName;
      if FileExists(FileName) then
      begin
        if FMt3dBasic.InitialChoice = micSpecifyTimeStep then
        begin
          KPER := -1;
          KSTP := -1;
          NTRANS := -1;
          AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareCompat);
          try
            if AFileStream.Size = 0 then
            begin
              Beep;
              ErrorMessage := Format(StrTheInitialConcentr, [FileName]);
              MessageDlg(ErrorMessage, mtError, [mbOK], 0);
              Exit;
            end;

            try
              APrecision := CheckArrayPrecision(AFileStream);
            except on EPrecisionReadError do
              begin
                Beep;
                MessageDlg(Format(StrSomethingIsWrongW, [FileName]), mtError, [mbOK], 0);
                Exit;
              end;
            end;

            While AFileStream.Position < AFileStream.Size do
            begin
              case APrecision of
                mpSingle:
                  begin
                    ReadSinglePrecisionMt3dmsBinaryRealArray(AFileStream, NTRANS,
                      KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                  end;
                mpDouble:
                  begin
                    ReadDoublePrecisionMt3dmsBinaryRealArray(AFileStream, NTRANS,
                      KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                  end;
                else Assert(False);
              end;
              if KPER > StressPeriod then
              begin
                break
              end
              else if (KPER = StressPeriod) and (KSTP > TimeStep) then
              begin
                break;
              end
              else if (KPER = StressPeriod) and (KSTP = TimeStep)
                and (NTRANS >= TransportStep) then
              begin
                break;
              end;
            end;

            if (StressPeriod <> KPER) or (KSTP <> TimeStep)
              or (TransportStep <> NTRANS) then
            begin
              ErrorMessage := Format(StrTheSpecifiedStress, [FileName]);
              frmErrorsAndWarnings.AddError(Model, StrMissingTransportSt,
                ErrorMessage);
            end
            else
            begin
              LIndex := 0;
              for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
              begin
                if Model.IsLayerSimulated(LayerIndex) then
                begin
                  Inc(LIndex);
                  if LIndex <> ILAY then
                  begin
                    ErrorMessage := Format(StrDataForLayer0d, [LIndex, FileName]);
                    frmErrorsAndWarnings.AddError(Model, StrMissingDataInInit,
                      ErrorMessage);
                    Exit;
                  end;

                  if (Model.Grid.RowCount <> NROW) or (Model.Grid.ColumnCount <> NCOL) then
                  begin
                    ErrorMessage := Format(StrTheNumberOfRowsO, [FileName]);
                    frmErrorsAndWarnings.AddError(Model, StrIncorrectNumberOf,
                      ErrorMessage);
                    Exit;
                  end;
                  WriteU2DRELHeader(Format(StrDataSet13SCONC,
                    [Item.Name, LIndex]), matStructured, 'SCONC');
                  for RowIndex := 0 to Model.Grid.RowCount - 1 do
                  begin
                    NeedNewLine := False;
                    for ColIndex := 0 to Model.Grid.ColumnCount - 1 do
                    begin
                      WriteFloat(AnArray[RowIndex, ColIndex]);
                      NeedNewLine := ((ColIndex + 1) mod 10) <> 0;
                      if not NeedNewLine then
                      begin
                        NewLine;
                      end;
                    end;
                    if NeedNewLine then
                    begin
                      NewLine;
                    end;
                  end;

                  if AFileStream.Position < AFileStream.Size then
                  begin
                    case APrecision of
                      mpSingle:
                        begin
                          ReadSinglePrecisionMt3dmsBinaryRealArray(AFileStream, NTRANS,
                            KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                        end;
                      mpDouble:
                        begin
                          ReadDoublePrecisionMt3dmsBinaryRealArray(AFileStream, NTRANS,
                            KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                        end;
                      else Assert(False);
                    end;
                  end
                end;
              end;
              DataWritten := True;
            end;
          finally
            AFileStream.Free;
          end;
        end
        else
        begin
          RelFileName := ExtractRelativePath(FNameOfFile, FileName);
          WriteToMt3dMsNameFile(StrDATABINARY, InitConcUnit,
            RelFileName, foInput, Model);

          LIndex := 0;
          for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
          begin
            if Model.IsLayerSimulated(LayerIndex) then
            begin
              Inc(LIndex);
              WriteString(FixedFormattedInteger(-InitConcUnit, 10));
              WriteString('        1.                    ');
              WriteString(FixedFormattedInteger(IPRN_Real, 10));
              WriteString(Format(' # ' + StrDataSet13SCONC, [Item.Name, LIndex]));
              NewLine;
            end;
          end;
          Inc(InitConcUnit);
          DataWritten := True;
        end;
      end
      else
      begin
        ErrorMessage := Format(StrTheFile0sWas, [FileName, Item.Name]);
        frmErrorsAndWarnings.AddError(Model, StrInitialConcentratio,
          ErrorMessage);
      end;
    end;

    if not DataWritten then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(
        Item.InitialConcDataArrayName);
      DataArray.Initialize;
      for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
      begin
        if Model.IsLayerSimulated(LayerIndex) then
        begin
          WriteArray(DataArray, LayerIndex,
            Format(StrDataSet13SCONC,
            [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]),
            StrNoValueAssigned, 'SCONC');
        end;
      end;
    end;
  end;
begin
  InitConcUnit := ConcentrationStartUnit;
  StressPeriod := FMt3dBasic.InitialConcentrationStressPeriod;
  TimeStep := FMt3dBasic.InitialConcentrationTimeStep;
  TransportStep := FMt3dBasic.InitialConcentrationTransportStep;
  if Model.MobileComponents.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoMobileSpeciesDe,
      StrYouMustDefineAtL);
  end;
  for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
  begin
    Item := Model.MobileComponents[SpeciesIndex];
    WriteInitialConc;
  end;
  for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
  begin
    Item := Model.ImmobileComponents[SpeciesIndex];
    WriteInitialConc;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet14;
var
  CINACT: Double;
  THKMIN: Double;
begin
  CINACT := FMt3dBasic.InactiveConcentration;
  THKMIN := FMt3dBasic.MinimumSaturatedFraction;
  WriteF10Float(CINACT);
  WriteF10Float(THKMIN);
  WriteString(StrDataSet14CINACT);
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet15;
var
  IFMTCN: Integer;
  IFMTNP: Integer;
  IFMTRF: Integer;
  IFMTDP: Integer;
  SAVUCN: string;
  Mt3dmsOutputControl: TMt3dmsOutputControl;
begin
  IFMTCN := IPRN_Mt3dms_Real;
  IFMTNP := IPRN_Mt3dms_Integer;
  IFMTRF := IPRN_Mt3dms_Real;
  IFMTDP := IPRN_Mt3dms_Real;
  Mt3dmsOutputControl := Model.Mt3dmsOutputControl;
  if Mt3dmsOutputControl.SaveConcentrations then
  begin
    SAVUCN := '         T';
  end
  else
  begin
    SAVUCN := '         F';
  end;
  WriteI10Integer(IFMTCN , 'MT3DMS, Basic Package, IFMTCN');
  WriteI10Integer(IFMTNP , 'MT3DMS, Basic Package, IFMTNP');
  WriteI10Integer(IFMTRF , 'MT3DMS, Basic Package, IFMTRF');
  WriteI10Integer(IFMTDP , 'MT3DMS, Basic Package, IFMTDP');
  WriteString(SAVUCN);
  WriteString(' # Data set 15: IFMTCN IFMTNP IFMTRF IFMTDP SAVUCN');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet16;
var
  NPRS: integer;
  Mt3dmsOutputControl: TMt3dmsOutputControl;
begin
  Mt3dmsOutputControl := Model.Mt3dmsOutputControl;
  NPRS := 0;
  case Mt3dmsOutputControl.OutputFreqChoice of
    mofSpecifiedTimes:
      begin
        NPRS :=  Mt3dmsOutputControl.OutputTimes.Count;
      end;
    mofEndOfSimulation:
      begin
        NPRS := 0;
      end;
    mofPeriodic:
      begin
        NPRS := -Mt3dmsOutputControl.PeriodicOutputCount;
      end;
    else
      Assert(False);
  end;
  WriteI10Integer(NPRS, 'MT3DMS or MT3D-USGS Basic package, NPRS');
  WriteString(' # Data Set 16, NPRS');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet17;
const
  MaxItemsPerLine = 8;
var
  Mt3dmsOutputControl: TMt3dmsOutputControl;
  Index: Integer;
  Item: TOuptputTimeItem;
  NewLineNeeded: Boolean;
begin
  NewLineNeeded := True;
  Mt3dmsOutputControl := Model.Mt3dmsOutputControl;
  if Mt3dmsOutputControl.OutputFreqChoice = mofSpecifiedTimes then
  begin
    for Index := 0 to Mt3dmsOutputControl.OutputTimes.Count - 1 do
    begin
      Item := Mt3dmsOutputControl.OutputTimes[Index];
      WriteF10Float(Item.ObservationTime);
      if ((Index + 1) mod MaxItemsPerLine) = 0 then
      begin
        NewLine;
        NewLineNeeded := False;
      end
      else
      begin
        NewLineNeeded := True;
      end;
    end;
    if NewLineNeeded then
    begin
      NewLine;
    end;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet18;
var
  LayerIndex, RowIndex, ColIndex: Integer;
  DataArray: TDataArray;
  Mt3dmsOutputControl: TMt3dmsOutputControl;
  NPROBS: Integer;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(STR_MT3DMS_Observation_Locations);
  DataArray.Initialize;
  NOBS := 0;
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
        begin
          if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
          begin
            Inc(NOBS);
          end;
        end;
      end;
    end;
  end;
  Mt3dmsOutputControl := Model.Mt3dmsOutputControl;
  NPROBS := Mt3dmsOutputControl.ObservationFrequency;

  WriteI10Integer(NOBS, 'MT3DMS or MT3D-USGS Basic Package, NOBS');
  WriteI10Integer(NPROBS, 'MT3DMS or MT3D-USGS Basic Package, NPROBS');
  WriteString(' # Data set 18, NOBS NPROBS');
  NewLine;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMaximumNumberOfOb);
  if NOBS > 200 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrMaximumNumberOfOb,
      Format(StrByDefaultMT3DMSO, [DataArray.Name]));
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet19;
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  MFLayer: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BaseName: string;
  SpeciesIndex: Integer;
  UnitNumber: Integer;
  Chem: TChemSpeciesItem;
  OutputFileName: string;
  Directory: string;
begin
  if NOBS > 0 then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      STR_MT3DMS_Observation_Locations);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        MFLayer := Model.DataSetLayerToModflowLayer(LayerIndex);
        for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
          begin
            if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              WriteI10Integer(MFLayer, 'MT3DMS, Data set 19, KOBS');
              WriteI10Integer(RowIndex+1, 'MT3DMS, Data set 19, IOBS');
              WriteI10Integer(ColIndex+1, 'MT3DMS, Data set 19, JOBS');
              NewLine;
            end;
          end;
        end;
      end;
    end;
    BaseName := ExtractFileName(FNameOfFile);
    BaseName := ChangeFileExt(BaseName, '');
    Directory := ExtractFileDir(FNameOfFile);
    Directory := IncludeTrailingPathDelimiter(Directory);
    // Generate Name file lines for chemical species
    for SpeciesIndex := 0 to Model.MobileComponents.Count-1 do
    begin
      UnitNumber := Mt3dObsStart + SpeciesIndex;
      Chem := Model.MobileComponents[SpeciesIndex];
      OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + strMtObs;
      WriteToMt3dMsNameFile(strDATA, UnitNumber, OutputFileName, foOutput, Model);
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count-1 do
    begin
      UnitNumber := Mt3dUcnImmobileStart + Model.MobileComponents.Count + SpeciesIndex;
      Chem := Model.ImmobileComponents[SpeciesIndex];
      OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + strMtObs;
      WriteToMt3dMsNameFile(strDATA, UnitNumber, OutputFileName, foOutput, Model);
    end;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet20;
var
  Mt3dmsOutputControl: TMt3dmsOutputControl;
  CHKMAS: string;
  NPRMAS: Integer;
begin
  Mt3dmsOutputControl := Model.Mt3dmsOutputControl;
  if Mt3dmsOutputControl.SummarizeMassBalance then
  begin
    CHKMAS := '         T';
  end
  else
  begin
    CHKMAS := '         F';
  end;
  NPRMAS := Mt3dmsOutputControl.MassBalanceFrequency;
  WriteString(CHKMAS);
  WriteI10Integer(NPRMAS, 'MT3DMS or MT3D-USGS Basic package, NPRMAS');
  WriteString(' # Data Set 20: CHKMAS NPRMAS');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet21to23;
begin
  Model.ModflowFullStressPeriods.WriteMt3dmsStressPeriods(self);
end;

procedure TMt3dmsBtnWriter.WriteDataSet22(StressPeriod: TModflowStressPeriod);
begin
  // do nothing. Data Set 22 is never needed with MODFLOW.
end;

procedure TMt3dmsBtnWriter.WriteDataSet23(StressPeriod: TModflowStressPeriod);
var
  Mt3dmsTime: TMt3dmsTimeItem;
  DT0: Double;
  MXSTRN: Integer;
  TTSMULT: Double;
  TTSMAX: Double;
begin
  Mt3dmsTime := Model.Mt3dmsTimes.GetItemFromTime(StressPeriod.StartTime);
  if Mt3dmsTime = nil then
  begin
    frmErrorsAndWarnings.AddError(Model, StrTimeDataForMT3DMS,
      Format(StrNoTimeDataHasBee, [StressPeriod.Index+1]) );
    Exit;
  end;
  DT0 := Mt3dmsTime.StepSize;
  MXSTRN := Mt3dmsTime.MaxSteps;
  TTSMULT := Mt3dmsTime.TimeStepMultiplier;
  TTSMAX := Mt3dmsTime.MaxStepSize;
  WriteF10Float(DT0);
  WriteI10Integer(MXSTRN, 'Error in MT3DMS or MT3D-USGS BTN Data Set 23: MXSTRN');
  WriteF10Float(TTSMULT);
  WriteF10Float(TTSMAX);
  WriteString(' # Data Set 23: DT0, MXSTRN, TTSMULT, TTSMAX');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet3;
var
  NLAY: Integer;
  NROW: Integer;
  NCOL: Integer;
  NPER: Integer;
  NCOMP: Integer;
  MCOMP: Integer;
  SpeciesIndex: integer;
  UnitNumber: integer;
  BaseName: string;
  Chem: TChemSpeciesItem;
  OutputFileName: string;
  Mt3dmsChemReact: TMt3dmsChemReaction;
  SecondUcnFileCreated: boolean;
  Directory: string;
  IATS: Integer;
begin
  NLAY := Model.ModflowLayerCount;
  NROW := Model.ModflowGrid.RowCount;
  NCOL := Model.ModflowGrid.ColumnCount;
  NPER := Model.ModflowFullStressPeriods.Count;
  MCOMP := Model.MobileComponents.Count;
  NCOMP := MCOMP + Model.ImmobileComponents.Count;
  IATS := 0;
  WriteI10Integer(NLAY, Format(StrSInTheMT3DMSBTN, ['NLAY']));
  WriteI10Integer(NROW, Format(StrSInTheMT3DMSBTN, ['NROW']));
  WriteI10Integer(NCOL, Format(StrSInTheMT3DMSBTN, ['NCOL']));
  WriteI10Integer(NPER, Format(StrSInTheMT3DMSBTN, ['NPER']));
  WriteI10Integer(NCOMP, Format(StrSInTheMT3DMSBTN, ['NCOMP']));
  WriteI10Integer(MCOMP, Format(StrSInTheMT3DMSBTN, ['MCOMP']));
  WriteI10Integer(IATS, Format(StrSInTheMT3DMSBTN, ['IATS']));
  WriteString(' # Data Set 3: NLAY NROW NCOL NPER NCOMP MCOMP, IATS');
  NewLine;

  Mt3dmsChemReact := Model.ModflowPackages.Mt3dmsChemReact;
  SecondUcnFileCreated := Mt3dmsChemReact.IsSelected
    and (Mt3dmsChemReact.SorptionChoice >= scLinear);

  BaseName := ExtractFileName(FNameOfFile);
  BaseName := ChangeFileExt(BaseName, '');
  Directory := ExtractFileDir(FNameOfFile);
  Directory := IncludeTrailingPathDelimiter(Directory);
  // Generate Name file lines for chemical species
  for SpeciesIndex := 0 to Model.MobileComponents.Count-1 do
  begin
    UnitNumber := Mt3dUcnMobileStart + SpeciesIndex;
    Chem := Model.MobileComponents[SpeciesIndex];
    OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + StrMt3dConcFile;
    WriteToMt3dMsNameFile(strDATABINARY, UnitNumber, OutputFileName, foOutput, Model);
    if FileExists(OutputFileName) then
    begin
      DeleteFile(OutputFileName);
    end;
    if SecondUcnFileCreated then
    begin
      UnitNumber := UnitNumber + 100;
      OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + '_S'+ StrMt3dConcFile;
      WriteToMt3dMsNameFile(strDATABINARY, UnitNumber, OutputFileName, foOutput, Model);
      if FileExists(OutputFileName) then
      begin
        DeleteFile(OutputFileName);
      end;
    end;
  end;
  for SpeciesIndex := 0 to Model.ImmobileComponents.Count-1 do
  begin
    UnitNumber := Mt3dUcnImmobileStart + SpeciesIndex;
    Chem := Model.ImmobileComponents[SpeciesIndex];
    OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + StrMt3dConcFile;
    WriteToMt3dMsNameFile(strDATABINARY, UnitNumber, OutputFileName, foOutput, Model);
    if FileExists(OutputFileName) then
    begin
      DeleteFile(OutputFileName);
    end;
    if SecondUcnFileCreated then
    begin
      UnitNumber := UnitNumber + 100;
      OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + '_S'+ StrMt3dConcFile;
      WriteToMt3dMsNameFile(strDATABINARY, UnitNumber, OutputFileName, foOutput, Model);
      if FileExists(OutputFileName) then
      begin
        DeleteFile(OutputFileName);
      end;
    end;
  end;
  for SpeciesIndex := 0 to Model.MobileComponents.Count-1 do
  begin
    UnitNumber := Mt3dMassStart + SpeciesIndex;
    Chem := Model.MobileComponents[SpeciesIndex];
    OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + '._mas';
    WriteToMt3dMsNameFile(strDATA, UnitNumber, OutputFileName, foOutput, Model);
  end;
  for SpeciesIndex := 0 to Model.ImmobileComponents.Count-1 do
  begin
    UnitNumber := Mt3dMassStart + Model.MobileComponents.Count + SpeciesIndex;
    Chem := Model.ImmobileComponents[SpeciesIndex];
    OutputFileName := Directory + GenerateNewRoot(BaseName + '_' + Chem.Name) + '._mas';
    WriteToMt3dMsNameFile(strDATA, UnitNumber, OutputFileName, foOutput, Model);
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet3Options;
var
  NewLineNeeded: Boolean;
begin
  if FMt3dBasic.Mt3dVersion <> mvUSGS then
  begin
    Exit;
  end;

  NewLineNeeded := False;
  if (muoDryCell in FMt3dBasic.Mt3dUsgsOptions)
    and (FMt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    WriteString('DRYCELL ');
    NewLineNeeded := True;
  end;

  if (muoFtlPrint in FMt3dBasic.Mt3dUsgsOptions)
    and (FMt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    WriteString('FTLPRINT ');
    NewLineNeeded := True;
  end;

  if not (muoWetDryPrint in FMt3dBasic.Mt3dUsgsOptions)
    and (FMt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    WriteString('NOWETDRYPRINT ');
    NewLineNeeded := True;
  end;

  if not (muoIncludeDryCellBudget in FMt3dBasic.Mt3dUsgsOptions)
    and (FMt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    WriteString('OMITDRYCELLBUDGET ');
    NewLineNeeded := True;
  end;

  if (muoAltSorbedWeight in FMt3dBasic.Mt3dUsgsOptions)
    and (FMt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    WriteString('ALTWTSORB ');
    NewLineNeeded := True;
  end;

  if NewLineNeeded then
  begin
    NewLine;
  end;
end;

procedure TMt3dmsBtnWriter.WriteDataSet4;
var
  TUNIT, LUNIT, MUNIT: string;
begin
  case Model.ModflowOptions.TimeUnit of
    0: TUNIT := '   ?';
    1: TUNIT := ' sec';
    2: TUNIT := ' min';
    3: TUNIT := '  hr';
    4: TUNIT := ' day';
    5: TUNIT := '  yr';
    else Assert(False);
  end;
  case Model.ModflowOptions.LengthUnit of
    0: LUNIT := '   ?';
    1: LUNIT := '  ft';
    2: LUNIT := '   m';
    3: LUNIT := '  cm';
    else Assert(False);
  end;
  MUNIT := FMt3dBasic.MassUnit;
  if MUNIT = '' then
  begin
    MUNIT := '   ?';
  end;
  while Length(MUNIT) < 4 do
  begin
    MUNIT := ' ' + MUNIT;
  end;
  if Length(MUNIT) > 4 then
  begin
    SetLength(MUNIT, 4);
  end;
  WriteString(TUNIT);
  WriteString(LUNIT);
  WriteString(MUNIT);
  WriteString(' # Data Set 4: TUNIT LUNIT MUNIT');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet5;
begin
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSet6;
var
  LAYCON: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYCON := Model.Laytyp;
  for LayerIndex := 0 to Length(LAYCON) - 1 do
  begin
    // In BCF package, strip off interblock transmissivity method
    LAYCON[LayerIndex] := (LAYCON[LayerIndex] mod 10);
    WriteI2Integer(LAYCON[LayerIndex], 'MT3DMS or MT3D-USGS Basic Package, LAYCON');
    if (((LayerIndex+1) mod 40) = 0) and (LayerIndex < Length(LAYCON) - 1) then
    begin
      NewLine;
    end;
  end;
  WriteString(' # Data Set 6: LAYCON');
  NewLine;
end;

procedure TMt3dmsBtnWriter.WriteDataSets1And2;
const
  MaxHeadingLineLength = 80;
var
  LineIndex: Integer;
  Heading: string;
begin
  for LineIndex := 0 to 1 do
  begin
    if LineIndex < FMt3dBasic.Comments.Count then
    begin
      Heading := FMt3dBasic.Comments[LineIndex];
    end
    else
    begin
      Heading := '';
    end;
    if Length(Heading) > MaxHeadingLineLength then
    begin
      SetLength(Heading, MaxHeadingLineLength);
    end;
    WriteString(Heading);
    NewLine;
  end;
end;

procedure TMt3dmsBtnWriter.WriteFile(const AFileName: string);
begin
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTimeDataForMT3DMS);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInitialConcentratio);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingTransportSt);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingDataInInit);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrIncorrectNumberOf);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoMobileSpeciesDe);



    FNameOfFile := FileName(AFileName);
    // PackageGeneratedExternally needs to be updated for MT3DMS
    if Model.PackageGeneratedExternally(StrBTN) then
    begin
      Exit;
    end;

    // write to MT3DMS or MT3D-USGS name file.
    WriteToMt3dMsNameFile(StrBTN, Mt3dBtn,
      FNameOfFile, foInput, Model);

    FInputFileName := FNameOfFile;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingMT3DMSBTNP);
      frmProgressMM.AddMessage(StrWritingDataSets1and2);
      WriteDataSets1And2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet3);
      WriteDataSet3Options;
      WriteDataSet3;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet4);
      WriteDataSet4;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet5);
      WriteDataSet5;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;


      frmProgressMM.AddMessage(StrWritingDataSet6);
      WriteDataSet6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 7
      frmProgressMM.AddMessage(StrWritingDataSet7);
      Model.ModflowGrid.WriteDELR(self);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 8
      frmProgressMM.AddMessage(StrWritingDataSet8);
      Model.ModflowGrid.WriteDELC(self);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 9
      frmProgressMM.AddMessage(StrWritingDataSet9);
      Model.ModflowGrid.WriteTOP(self);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Model.DataArrayManager.CacheDataArrays;

      frmProgressMM.AddMessage(StrWritingDataSet10);
      WriteDataSet10;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet11);
      WriteDataSet11;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet12);
      WriteDataSet12;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet13);
      WriteDataSet13;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet14);
      WriteDataSet14;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet15);
      WriteDataSet15;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet16);
      WriteDataSet16;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet17);
      WriteDataSet17;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet18);
      WriteDataSet18;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet19);
      WriteDataSet19;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet20);
      WriteDataSet20;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet21);
      WriteDataSet21to23;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

end.
