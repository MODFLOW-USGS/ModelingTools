unit ModflowLPF_WriterUnit;

interface

uses Types, SysUtils, GoPhastTypes, CustomModflowWriterUnit, PhastModelUnit,
  ModflowParameterUnit, OrderedCollectionUnit, ModflowPackageSelectionUnit,
  LayerStructureUnit, System.Generics.Collections, DataSetUnit;

type
  TCustomLpfWriter = class(TCustomFlowPackageWriter)
  private
    FPestDataArrays: TList<TDataArray>;
    procedure CheckParamZones;
  protected
    NPLPF: integer;
    FParameterUsed: array[ptLPF_HK..ptLPF_VKCB] of boolean;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    function ParameterDataSetNumber: integer; virtual; abstract;
    procedure WriteParameters;
    procedure WriteLayerData; virtual; abstract;
    procedure WriteHK(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
    procedure WriteHANI(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
    procedure WriteVKA(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
    procedure WriteVANI(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
    procedure WriteSS(TransientModel: Boolean; Group: TLayerGroup; ArrayIndex,
      MFLayerIndex: Integer);
    procedure WriteSY(TransientModel: Boolean; Group: TLayerGroup; ArrayIndex,
      MFLayerIndex: Integer);
    procedure WriteVKCB(LayerIndex, MFLayerIndex, ArrayIndex: Integer);
    procedure WritePestScripts;
  public
    constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
  end;

  TModflowLPF_Writer = class(TCustomLpfWriter)
  private
    FConvertibleLayerPresent: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteWETDRY(Group: TLayerGroup; ArrayIndex: Integer; MFLayerIndex: Integer);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    function ParameterDataSetNumber: integer; override;
    procedure WriteLayerData; override;
  public
    procedure WriteFile(const AFileName: string);
//    procedure WritePestFile(const AFileName: string);
  end;

const
  AllLpfParameters : TParameterTypes = [ptLPF_HK,
    ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB];
  SteadyLpfParameters : TParameterTypes = [ptLPF_HK,
    ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_VKCB];

resourcestring
  StrParameterZonesNot = 'Parameter zones not defined.';
  StrTheNOPARCHECKOpt = 'The "NOPARCHECK" option can be used to speed up MOD' +
  'FLOW but it keeps MODFLOW from checking that required input data have bee' +
  'n fully defined. Use with caution.';

implementation

uses ModflowUnitNumbers, ModflowOutputControlUnit,
  frmErrorsAndWarningsUnit, frmProgressUnit, Forms, PestParamRoots;

resourcestring
  StrVKCBParameterImpro = 'VKCB parameter improperly defined.';
  StrSIsAVKCBParame = '%s  is a VKCB parameter but all the layers in the mod' +
  'el are simulated';
  StrParameter0sInSim = 'Parameter %0:s in the LPF package is skipped becaus' +
  'e it is not applied to any cells.  Check that %1:s is set to "True" in at' +
  ' least one non-simulated unit.';
  StrParameter0sInNonSim = 'Parameter %0:s in the LPF package is skipped bec' +
  'ause it is not applied to any cells.  Check that %1:s is set to "True" in' +
  ' at least one simulated unit.';
  StrWritingLPFPackage = 'Writing LPF Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
//  StrWritingDataSet7 = '  Writing Data Set 7.';
  StrWritingDataSet16 = '  Writing Data Set 16 for layer %d';
  StrWETDRY0sLayer1 = 'WETDRY %0:s Layer %1:d';
  StrWritingDataSetN = '  Writing Data Set %0:d for layer %1:d';
  StrNegativeSValue = 'Negative %s value in Layer %d';
  StrNegativeOrZeroS = 'Negative or zero %s value';
  StrWritingDataSetParam = '  Writing Data Set %0:d for parameter: %1:s';
  StrLargeContrastInHy = 'Large contrast in hydraulic conductivity in layer %d (may cause numerical problems)';
  StrZeroSValue = 'Zero %s value in Layer %d';
  StrMissingParameterNa = 'Missing parameter name';
  StrInThe0sThereIs = 'In the %0:s there is a %1:s parameter that doesn''t h' +
  'ave a name';
  StrParameterValueEqua = 'Parameter value equals 0.';
  StrInThe0sThePar = 'In the %0:s, the parameter named "%1:s" has a value of' +
  ' zero.';
  StrNoZoneArrayForPa = 'No zone array for parameter';
  StrInThe0sNoZone = 'In the %0:s, no zone array has been defined for the pa' +
  'rameter named "%1:s". Therefore, the parameter will apply to every cell i' +
  'n the grid';
  StrTheNOPARCHECKOptLPF = 'The "NOPARCHECK" option in the LPF package is selec' +
  'ted';
  StrTheFollowingLPFPa = 'The following LPF parameters are applied to all ce' +
  'lls because no zones are defined for them.';
  StrParameterNotUsed = 'Parameter not used';
  StrTheParameterSIs = 'The parameter %s is not being used because the zone ' +
  'array for it is false in every model layer.';

{ TModflowLPF_Writer }

procedure TModflowLPF_Writer.WriteDataSet1;
var
  ILPFCB: integer;
  HDRY: double;
  Options: string;
  LocalPackage: TLpfSelection;
  LayerCount: Integer;
  ValidParamTypes: TParameterTypes;
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  NCLU: Integer;
  Clusters: TOneDIntegerArray;
  UniformLayers: TBooleanDynArray;
begin
  ILPFCB := 0;
  GetFlowUnitNumber(ILPFCB);
  HDRY := Model.ModflowOptions.HDry;
  if Model.ModflowFullStressPeriods.TransientModel then
  begin
    NPLPF := Model.ModflowSteadyParameters.CountParameters(AllLpfParameters);
  end
  else
  begin
    NPLPF := Model.ModflowSteadyParameters.CountParameters(SteadyLpfParameters);
  end;

  if NPLPF > 0 then
  begin
    LayerCount := Model.ModflowLayerCount;
    if Model.ModflowFullStressPeriods.TransientModel then
    begin
      ValidParamTypes := AllLpfParameters;
    end
    else
    begin
      ValidParamTypes := SteadyLpfParameters;
    end;
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      Param := Model.ModflowSteadyParameters.Items[ParamIndex];
      if Param.ParameterType in ValidParamTypes then
      begin
        FParameterUsed[Param.ParameterType] := True;
        if not Param.UseZone then
        begin
          if Param.ParameterType = ptLPF_VKCB then
          begin
            NCLU := Model.ModflowConfiningBedCount;
          end
          else
          begin
            NCLU := Model.ModflowLayerCount;
          end;
//          SetLength(Clusters, 0);
        end
        else
        begin
          IdentifyZoneClusters(NCLU, Clusters, UniformLayers, LayerCount, Param);
          if NCLU = 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrParameterNotUsed,
              Format(StrTheParameterSIs, [Param.ParameterName]));
          end;
        end;
        if NCLU = 0 then
        begin
          Dec(NPLPF);
        end;
      end;
    end;
  end;


  LocalPackage := Package as TLpfSelection;
  Options := '';
  if LocalPackage.UseStorageCoefficient then
  begin
    Options := ' STORAGECOEFFICIENT';
  end;
  if LocalPackage.UseConstantCV then
  begin
    Options := Options + ' CONSTANTCV';
  end;
  if LocalPackage.UseSaturatedThickness then
  begin
    Options := Options + ' THICKSTRT';
  end;
  if not LocalPackage.UseCvCorrection then
  begin
    Options := Options + ' NOCVCORRECTION';
  end;
  if not LocalPackage.UseVerticalFlowCorrection then
  begin
    Options := Options + ' NOVFC';
  end;
  if LocalPackage.NoParCheck then
  begin
    Options := Options + ' NOPARCHECK';
    frmErrorsAndWarnings.AddWarning(Model, StrTheNOPARCHECKOptLPF, StrTheNOPARCHECKOpt)
  end;

  WriteInteger(ILPFCB);
  WriteFloat(HDRY);
  WriteInteger(NPLPF);
  if Options <> '' then
  begin
    WriteString(Options);
  end;
  WriteString(' # ILPFCB, HDRY, NPLPF');
  if Options <> '' then
  begin
    WriteString(' Options');
  end;
  NewLine;
end;

procedure TCustomLpfWriter.WriteDataSet2;
var
  LAYTYP: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYTYP := Model.Laytyp;
  for LayerIndex := 0 to Length(LAYTYP) - 1 do
  begin
    WriteInteger(LAYTYP[LayerIndex]);
  end;
  WriteString(' # LAYTYP');
  NewLine;
end;

procedure TCustomLpfWriter.WriteDataSet3;
var
  LAYAVG: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYAVG := Model.Layavg;
  for LayerIndex := 0 to Length(LAYAVG) - 1 do
  begin
    WriteInteger(LAYAVG[LayerIndex]);
  end;
  WriteString(' # LAYAVG');
  NewLine;
end;

procedure TCustomLpfWriter.WriteDataSet4;
var
  CHANI: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  CHANI := Model.Chani;
  for LayerIndex := 0 to Length(CHANI) - 1 do
  begin
    WriteInteger(CHANI[LayerIndex]);
  end;
  WriteString(' # CHANI');
  NewLine;
end;

procedure TCustomLpfWriter.WriteDataSet5;
var
  LAYVKA: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYVKA := Model.Layvka;
  for LayerIndex := 0 to Length(LAYVKA) - 1 do
  begin
    WriteInteger(LAYVKA[LayerIndex]);
  end;
  WriteString(' # LAYVKA');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet6;
var
  index: Integer;
  LAYTYP: TOneDIntegerArray;
begin
  LAYTYP := Model.Laytyp;
  FConvertibleLayerPresent := False;
  for index := 0 to Model.ModflowLayerCount - 1 do
  begin
    if Model.ModflowWettingOptions.WettingActive
      and (LAYTYP[index] <> 0) then
    begin
      WriteInteger(1);
      FConvertibleLayerPresent := True;
    end
    else
    begin
      WriteInteger(0);
    end;
  end;
  WriteString(' # LAYWET');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet7;
var
  WETFCT: double;
  IWETIT: integer;
  IHDWET: integer;
begin
  if Model.ModflowWettingOptions.WettingActive
    and FConvertibleLayerPresent then
  begin
    WETFCT := Model.ModflowWettingOptions.WettingFactor;
    IWETIT := Model.ModflowWettingOptions.WettingIterations;
    IHDWET := Model.ModflowWettingOptions.WettingEquation;
    WriteFloat(WETFCT);
    WriteInteger(IWETIT);
    WriteInteger(IHDWET);
    WriteString(' # WETFCT, IWETIT, IHDWET');
    NewLine;
  end;
end;

procedure TModflowLPF_Writer.WriteLayerData;
var
  Group: TLayerGroup;
  MFLayerIndex: integer;
  LayerIndex: integer;
  ArrayIndex: integer;
  TransientModel: boolean;
begin
  MFLayerIndex := 0;
  TransientModel := Model.ModflowFullStressPeriods.TransientModel;
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Inc(MFLayerIndex);
      ArrayIndex := LayerIndex;
      Group := Model.GetLayerGroupByLayer(LayerIndex);
      // Data Set 10;
      WriteHK(Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 11
      WriteHANI(Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 12
      if Group.VerticalHydraulicConductivityMethod = 0 then
      begin
        WriteVKA(Group, ArrayIndex, MFLayerIndex);
      end
      else
      begin
        WriteVANI(Group, ArrayIndex, MFLayerIndex);
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 13
      WriteSS(TransientModel, Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 14
      WriteSY(TransientModel, Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 15
      WriteVKCB(LayerIndex, MFLayerIndex, ArrayIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 16
      WriteWETDRY(Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

    end;
  end;
end;

//procedure TModflowLPF_Writer.WritePestFile(const AFileName: string);
//var
//  ValidParamTypes: TParameterTypes;
//  PestArrayWriter: TPestDataArrayWriter;
//begin
//  if not Model.ModflowPackages.LpfPackage.IsSelected then
//  begin
//    Exit
//  end;
//  if Model.ModflowFullStressPeriods.TransientModel then
//  begin
//    ValidParamTypes := AllLpfParameters;
//  end
//  else
//  begin
//    ValidParamTypes := SteadyLpfParameters;
//  end;
//  PestArrayWriter := TPestDataArrayWriter.Create(Model, etExport);
//  try
//    PestArrayWriter.WriteParamTypeArrays(ValidParamTypes, AFileName);
//  finally
//    PestArrayWriter.Free;
//  end;
//end;

procedure TCustomLpfWriter.WritePestScripts;
var
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  for DataArrayIndex := 0 to FPestDataArrays.Count - 1 do
  begin
    ADataArray := FPestDataArrays[DataArrayIndex];
    WritePestZones(ADataArray, FInputFileName,
      Format(StrLPFd, [DataArrayIndex+1]),
      Format('LP%d', [DataArrayIndex+1]));
  end;
end;

procedure TModflowLPF_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Model.ModflowPackages.LpfPackage.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.BeginUpdate;
  try
    CheckSpecifiedHeadsConnected;
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterZonesNot);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrVKCBParameterImpro);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheNOPARCHECKOptLPF);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterNotUsed);

    NameOfFile := FileName(AFileName);
    FInputFileName := NameOfFile;
    WriteToNameFile(StrLPF, Model.UnitNumbers.UnitNumber(StrLPF),
      NameOfFile, foInput, Model);
    FInputFileName := NameOfFile;
    OpenFile(NameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingLPFPackage);
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

      frmProgressMM.AddMessage(StrWritingDataSet3);
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

      frmProgressMM.AddMessage(StrWritingDataSet7);
      WriteDataSet7;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteParameters;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteLayerData;

      WritePestScripts;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowLPF_Writer.WriteWETDRY(Group: TLayerGroup; ArrayIndex: Integer; MFLayerIndex: Integer);
var
  DataArray: TDataArray;
begin
  if Model.ModflowWettingOptions.WettingActive and (Group.AquiferType <> 0) then
  begin
    { TODO : Consider supporting LAYWET }
    frmProgressMM.AddMessage(Format(StrWritingDataSet16, [MFLayerIndex]));
    DataArray := Model.DataArrayManager.GetDataSetByName(rsWetDry);
    Assert(DataArray <> nil);
    WriteArray(DataArray, ArrayIndex,
    Format(StrWETDRY0sLayer1, [Group.AquiferName, MFLayerIndex]), StrNoValueAssigned, 'WETDRY');
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
end;

procedure TCustomLpfWriter.WriteVKCB(LayerIndex, MFLayerIndex, ArrayIndex: Integer);
var
  DataArray: TDataArray;
  NextGroup: TLayerGroup;
  NoError: Boolean;
begin
  if (LayerIndex < Model.LayerCount - 1) then
  begin
    NextGroup := Model.GetLayerGroupByLayer(LayerIndex + 1);
    if not NextGroup.RunTimeSimulated then
    begin
      if FParameterUsed[ptLPF_VKCB] then
      begin
        WriteInteger(IPRN_Real);
        WriteString(' # VKCB ' + NextGroup.AquiferName
          + ' Layer ' + IntToStr(MFLayerIndex));
        NewLine;
      end
      else
      begin
        frmProgressMM.AddMessage(Format(StrWritingDataSetN,
          [ParameterDataSetNumber + 7, MFLayerIndex]));
        DataArray := Model.DataArrayManager.GetDataSetByName(rsModflow_CBKz);
        Assert(DataArray <> nil);
        WriteArray(DataArray, ArrayIndex + 1, 'VKCB ' + NextGroup.AquiferName
          + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'VKCB');
        NoError := CheckArray(DataArray, ArrayIndex, Format(StrNegativeSValue, [rsKz, ArrayIndex + 1]),
          cvmGreaterEqual, 0, etError);
        if NoError then
        begin
          CheckArray(DataArray, ArrayIndex, Format(StrZeroSValue, [rsKz, ArrayIndex + 1]),
            cvmGreater, 0, etWarning);
        end;
        if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
        begin
          FPestDataArrays.Add(DataArray);
        end;
      end;
    end;
  end;
end;

procedure TCustomLpfWriter.WriteSY(TransientModel: Boolean; Group: TLayerGroup;
  ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
  NoError: Boolean;
begin
  if TransientModel and (Group.AquiferType <> 0) then
  begin
    if FParameterUsed[ptLPF_SY] then
    begin
      WriteInteger(IPRN_Real);
      WriteString(' # SY ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
      NewLine;
    end
    else
    begin
      frmProgressMM.AddMessage(Format(StrWritingDataSetN,
        [ParameterDataSetNumber + 6, MFLayerIndex]));
      DataArray := Model.DataArrayManager.GetDataSetByName(rsSpecificYield);
      Assert(DataArray <> nil);
      WriteArray(DataArray, ArrayIndex, 'SY ' + Group.AquiferName
        + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'SY');
      NoError := CheckArray(DataArray, ArrayIndex,
        Format(StrNegativeSValue, [rsSpecificYield, ArrayIndex + 1]),
        cvmGreaterEqual, 0, etError);
      if NoError then
      begin
        CheckArray(DataArray, ArrayIndex,
          Format(StrZeroSValue, [rsSpecificYield, ArrayIndex + 1]),
          cvmGreater, 0, etWarning);
      end;
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;
  end;
end;

procedure TCustomLpfWriter.WriteSS(TransientModel: Boolean; Group: TLayerGroup;
  ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
  NoError: Boolean;
begin
  if TransientModel then
  begin
    { TODO : Check on SPECIFICSTORAGE option in MODFLOW-2005. }
    if FParameterUsed[ptLPF_SS] then
    begin
      WriteInteger(IPRN_Real);
      WriteString(' # SS ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
      NewLine;
    end
    else
    begin
      frmProgressMM.AddMessage(Format(StrWritingDataSetN,
        [ParameterDataSetNumber + 5, MFLayerIndex]));
      DataArray := Model.DataArrayManager.GetDataSetByName(rsSpecific_Storage);
      Assert(DataArray <> nil);
      WriteArray(DataArray, ArrayIndex, 'SS ' + Group.AquiferName
        + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'SS');
      NoError := CheckArray(DataArray, ArrayIndex, Format(StrNegativeSValue,
        [rsSpecific_Storage, ArrayIndex + 1]), cvmGreaterEqual, 0, etError);
      if NoError then
      begin
        CheckArray(DataArray, ArrayIndex, Format(StrZeroSValue,
          [rsSpecific_Storage, ArrayIndex + 1]), cvmGreater, 0, etWarning);
      end;
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;
  end;
end;

procedure TCustomLpfWriter.WriteVANI(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
begin
  if FParameterUsed[ptLPF_VANI] then
  begin
    WriteInteger(IPRN_Real);
    WriteString(' # VKA ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
    NewLine;
  end
  else
  begin
    frmProgressMM.AddMessage(Format(StrWritingDataSetN,
      [ParameterDataSetNumber + 4, MFLayerIndex]));
    DataArray := Model.DataArrayManager.GetDataSetByName(rsVerticalAnisotropy);
    Assert(DataArray <> nil);
    WriteArray(DataArray, ArrayIndex, 'VKA ' + Group.AquiferName
      + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'VKA');
    if Model.Grid.LayerCount > 1 then
    begin
      CheckArray(DataArray, ArrayIndex, Format(StrNegativeOrZeroS,
        [rsVerticalAnisotropy]), cvmGreater, 0, etError);
    end;
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
end;

procedure TCustomLpfWriter.WriteVKA(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
  NoError: Boolean;
begin
  if FParameterUsed[ptLPF_VK] then
  begin
    WriteInteger(IPRN_Real);
    WriteString(' # VKA ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
    NewLine;
  end
  else
  begin
    frmProgressMM.AddMessage(Format(StrWritingDataSetN,
      [ParameterDataSetNumber + 4, MFLayerIndex]));
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKz);
    Assert(DataArray <> nil);
    WriteArray(DataArray, ArrayIndex, 'VKA ' + Group.AquiferName
      + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'VKA');
    if Model.Grid.LayerCount > 1 then
    begin
      NoError := CheckArray(DataArray, ArrayIndex, Format(StrNegativeSValue,
        [rsKz, ArrayIndex + 1]), cvmGreaterEqual, 0, etError);
      if NoError then
      begin
        CheckArray(DataArray, ArrayIndex, Format(StrZeroSValue,
          [rsKz, ArrayIndex + 1]), cvmGreater, 0, etWarning);
      end;
    end;
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
end;

procedure TCustomLpfWriter.WriteHANI(Group: TLayerGroup; ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
  NoError: Boolean;
begin
  if FParameterUsed[ptLPF_HANI] then
  begin
    WriteInteger(IPRN_Real);
    WriteString(' # HANI ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
    NewLine;
  end
  else
  begin
    frmProgressMM.AddMessage(Format(StrWritingDataSetN,
      [ParameterDataSetNumber + 3, MFLayerIndex]));
    DataArray := Model.DataArrayManager.GetDataSetByName(rsHorizontalAnisotropy);
    Assert(DataArray <> nil);
    WriteArray(DataArray, ArrayIndex, 'HANI ' + Group.AquiferName
      + ' Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'HANI');
    NoError := CheckArray(DataArray, ArrayIndex, Format(StrNegativeSValue,
        [rsHorizontalAnisotropy, ArrayIndex + 1]), cvmGreaterEqual, 0, etError);
    if NoError then
    begin
      CheckArray(DataArray, ArrayIndex, Format(StrZeroSValue,
        [rsHorizontalAnisotropy, ArrayIndex + 1]), cvmGreater, 0, etWarning);
    end;
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
end;

procedure TCustomLpfWriter.WriteHK(Group: TLayerGroup;
  ArrayIndex, MFLayerIndex: Integer);
var
  DataArray: TDataArray;
  NoError: Boolean;
begin
  if FParameterUsed[ptLPF_HK] then
  begin
    WriteInteger(IPRN_Real);
    WriteString(' # HK ' + Group.AquiferName + ' Layer ' + IntToStr(MFLayerIndex));
    NewLine;
  end
  else
  begin
    frmProgressMM.AddMessage(Format(StrWritingDataSetN,
      [ParameterDataSetNumber + 2, MFLayerIndex]));
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
    Assert(DataArray <> nil);
    WriteArray(DataArray, ArrayIndex, 'HK ' + Group.AquiferName + ' Layer '
      + IntToStr(MFLayerIndex), StrNoValueAssigned, 'HK');
    NoError := CheckArray(DataArray, ArrayIndex, Format(StrNegativeSValue,
      [rsKx, ArrayIndex + 1]), cvmGreaterEqual, 0, etError);
    if NoError then
    begin
      CheckArray(DataArray, ArrayIndex, Format(StrZeroSValue,
        [rsKx, ArrayIndex + 1]), cvmGreater, 0, etWarning);
    end;
    CheckArray(DataArray, ArrayIndex, Format(StrLargeContrastInHy, [ArrayIndex + 1]),
      cvmGradient, 1e6, etWarning);
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
end;

constructor TCustomLpfWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
var
  Index: TParameterType;
begin
  inherited;
  for Index := Low(TParameterType) to High(TParameterType) do
  begin
    if Index in AllLpfParameters then FParameterUsed[Index] := False;
  end;
  FPestDataArrays := TList<TDataArray>.Create;
end;

destructor TCustomLpfWriter.Destroy;
begin
  FPestDataArrays.Free;
  inherited;
end;

function TModflowLPF_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.LpfPackage;
end;

function TModflowLPF_Writer.ParameterDataSetNumber: integer;
begin
  result := 8;
end;

class function TModflowLPF_Writer.Extension: string;
begin
  result := '.lpf';
end;

procedure TCustomLpfWriter.CheckParamZones;
var
  ValidParamTypes: TParameterTypes;
  ParamCounts: array[TParameterType] of Integer;
  PTypeIndex: TParameterType;
  Param: TModflowSteadyParameter;
  ParamIndex: Integer;
begin
  for PTypeIndex := Low(TParameterType) to High(TParameterType) do
  begin
    ParamCounts[PTypeIndex] := 0;
  end;

  if Model.ModflowFullStressPeriods.TransientModel then
  begin
    ValidParamTypes := AllLpfParameters;
  end
  else
  begin
    ValidParamTypes := SteadyLpfParameters;
  end;
  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    Param := Model.ModflowSteadyParameters.Items[ParamIndex];
    if Param.ParameterType in ValidParamTypes then
    begin
      Inc(ParamCounts[Param.ParameterType]);
    end;
  end;

  for PTypeIndex := Low(TParameterType) to High(TParameterType) do
  begin
    if ParamCounts[PTypeIndex] > 1 then
    begin
      for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
      begin
        Param := Model.ModflowSteadyParameters.Items[ParamIndex];
        if (Param.ParameterType = PTypeIndex)
          and not Param.UseZone then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingLPFPa,
          Param.ParameterName);
        end;
      end;
    end;
  end;

end;

procedure TCustomLpfWriter.WriteParameters;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  PARNAM: string;
  PARTYP: string;
  PARVAL: double;
  NCLU: integer;
  ValidParamTypes: TParameterTypes;
  LayerCount: integer;
  Clusters: TOneDIntegerArray;
  UniformLayers: TBooleanDynArray;
  ClusterIndex: Integer;
  LAYER: integer;
  MLTARR: string;
  ZONARR: string;
  Error: string;
const
  IZ = 1;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingParameterNa);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrParameterValueEqua);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoZoneArrayForPa);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingLPFPa);
  LayerCount := Model.ModflowLayerCount;
  if NPLPF > 0 then
  begin
    CheckParamZones;

    if Model.ModflowFullStressPeriods.TransientModel then
    begin
      ValidParamTypes := AllLpfParameters;
    end
    else
    begin
      ValidParamTypes := SteadyLpfParameters;
    end;

    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      Param := Model.ModflowSteadyParameters.Items[ParamIndex];
      if Param.ParameterType in ValidParamTypes then
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        Param.ClearArrayNames;
        PARNAM := Param.ParameterName;
        case Param.ParameterType of
          ptLPF_HK: PARTYP := 'HK';
          ptLPF_HANI: PARTYP := 'HANI';
          ptLPF_VK: PARTYP := 'VK';
          ptLPF_VANI: PARTYP := 'VANI';
          ptLPF_SS: PARTYP := 'SS';
          ptLPF_SY: PARTYP := 'SY';
          ptLPF_VKCB: PARTYP := 'VKCB';
          else Assert(False);
        end;
        if Trim(PARNAM) = '' then
        begin
          frmErrorsAndWarnings.AddError(Model, StrMissingParameterNa,
            Format(StrInThe0sThereIs, [Package.PackageIdentifier, PARTYP]));
        end;
        FParameterUsed[Param.ParameterType] := True;
        PARTYP := ' ' + PARTYP;
        PARVAL := Param.Value;
        if PARVAL = 0 then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrParameterValueEqua,
          Format(StrInThe0sThePar, [Package.PackageIdentifier, PARNAM]));
        end;

        if (Param.ParameterType = ptLPF_VKCB) and
          (Model.ModflowConfiningBedCount = 0) then
        begin
          frmErrorsAndWarnings.AddError(Model, StrVKCBParameterImpro,
            Format(StrSIsAVKCBParame, [Param.ParameterName]));
        end;

        if not Param.UseZone then
        begin
          if Param.ParameterType = ptLPF_VKCB then
          begin
            NCLU := Model.ModflowConfiningBedCount;
          end
          else
          begin
            NCLU := Model.ModflowLayerCount;
          end;
          SetLength(Clusters, 0);
          frmErrorsAndWarnings.AddWarning(Model, StrNoZoneArrayForPa,
            Format(StrInThe0sNoZone, [Package.PackageIdentifier, Param.ParameterName]));
        end
        else
        begin
          IdentifyZoneClusters(NCLU, Clusters, UniformLayers, LayerCount, Param);
        end;
        if NCLU = 0 then
        begin
          if Param.ParameterType = ptLPF_VKCB then
          begin
            Error := Format(StrParameter0sInSim,
              [Param.ParameterName, Param.ZoneName]);
          end
          else
          begin
            Error := Format(StrParameter0sInNonSim,
              [Param.ParameterName, Param.ZoneName]);
          end;
          frmErrorsAndWarnings.AddError(Model, StrParameterZonesNot,
            Error);
          Continue;
        end;

        // Data set 8
        frmProgressMM.AddMessage(Format(StrWritingDataSetParam,
          [ParameterDataSetNumber, PARNAM]));
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(PARVAL);
        WriteInteger(NCLU);
        WriteString(' # PARNAM, PARTYP, PARVAL, NCLU');
        NewLine;

        Model.WritePValAndTemplate(PARNAM,PARVAL, Param);

        // Data set 9
        frmProgressMM.AddMessage(Format(StrWritingDataSetParam,
          [ParameterDataSetNumber+1, PARNAM]));
        for ClusterIndex := 0 to NCLU - 1 do
        begin
          if Param.UseZone then
          begin
            LAYER := Clusters[ClusterIndex];
            if UniformLayers[ClusterIndex] then
            begin
              ZONARR := 'ALL';
            end
            else
            begin
              ZONARR := Param.ZoneArrayName(LAYER, Model);
              UsedZoneArrayNames.Add(ZONARR);
            end;
          end
          else
          begin
            LAYER := ClusterIndex + 1;
            ZONARR := 'ALL'
          end;
          ZONARR := ' ' + ZONARR;
          if Param.UseMultiplier then
          begin
            MLTARR := Param.MultiplierArrayName(LAYER, Model);
            UsedMultiplierArrayNames.Add(MLTARR);
          end
          else
          begin
            MLTARR := 'NONE';
          end;
          MLTARR := ' ' + MLTARR;
          WriteInteger(Layer);
          WriteString(MLTARR + ' ');
          WriteString(ZONARR + ' ');
          if Param.UseZone then
          begin
            WriteInteger(IZ);
          end;
          WriteString(' # Layer, MLTARR, ZONARR');
          if Param.UseZone then
          begin
            WriteString(' IZ');
          end;
          NewLine;
        end;
        Model.DataArrayManager.CacheDataArrays;
      end;
    end;
  end;
end;

end.
