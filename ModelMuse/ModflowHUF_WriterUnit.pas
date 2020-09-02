unit ModflowHUF_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, PhastModelUnit, 
  ModflowPackageSelectionUnit, HufDefinition;

type
  TModflowHUF_Writer = class(TCustomFlowPackageWriter)
  private
    FHufPackage: THufPackageSelection;
    FNameOfFile: string;
    FTransient: Boolean;
    FConvertibleLayerPresent: Boolean;
    procedure CheckSytpParameters;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSets6to8;
    procedure WriteDataSet6(HGU: THydrogeologicUnit);
    procedure WriteDataSet7(HGU: THydrogeologicUnit);
    procedure WriteDataSet8(HGU: THydrogeologicUnit);
    procedure WriteDataSet9;
    procedure WriteDataSets10and11;
    procedure WriteDataSet12;
    procedure CheckElevations;
    procedure CheckHufKx;
    procedure CheckHufSS;
    procedure CheckHufSy;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
  end;

implementation

uses Math, Contnrs , ModflowUnitNumbers, frmProgressUnit, OrderedCollectionUnit,
  GoPhastTypes, DataSetUnit, frmErrorsAndWarningsUnit, ModflowParameterUnit,
  Forms;

const
  HufSteadyAndTransientParameters = [ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY];
  HufSteadyParameters = [ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI];

resourcestring
  StrNoVKparameter = 'No VK parameter defined for the following hydrogeologic units';
  StrWritingDataSet6 = '  Writing Data Set 6 for %s';
  StrWritingDataSet7 = '  Writing Data Set 7 for %s';
  StrWritingDataSet8 = '  Writing Data Set 8 for %s';
  StrWritingHUF2Package = 'Writing HUF2 Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet9 = '  Writing Data Set 9.';
  StrWritingDataSets10and11 = '  Writing Data Sets 10 and 11.';
//  StrWritingDataSet12 = '  Writing Data Set 12.';
  StrCheckingElevation = '  Checking elevations.';
  GapWarning = 'Gap between hydrogeologic units';
  OverlapWarning = 'Overlap between hydrogeologic units';
  WarningFormat = 'Column: %0:d; Row: %1:d; Higher unit: %2:s; Lower unit: %3:s; Amount: %4:g';
  StrLargeContrastInHy = 'Large contrast in hydraulic conductivity (may caus' +
  'e numerical problems)';
  StrCheckingHydraulicC = 'Checking hydraulic conductivity';
  StrHydraulicConductivi = 'Hydraulic conductivity is less than or equal to ' +
  'zero.';
  StrTheFollowingLPFSY = 'The following LPF SYTP parameters are applied to a' +
  'll cells because no zones are defined for them.';

type
  THguSort = class(TObject)
    HGU: THydrogeologicUnit;
    Top: double;
    Bottom: double;
  end;

function HguSorter(Item1, Item2: pointer): integer;
var
  HguSort1: THguSort;
  HguSort2: THguSort;
begin
  HguSort1 := Item1;
  HguSort2 := Item2;
  result := Sign(HguSort2.Top - HguSort1.Top);
  if result = 0 then
  begin
    result := Sign(HguSort2.Bottom - HguSort1.Bottom);
  end;
end;

{ TModflowHUF_Writer }

procedure TModflowHUF_Writer.CheckElevations;
var
  ActiveDataArray: TDataArray;
  HguList: TList;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  CellUsed: Boolean;
  HguIndex: Integer;
  HGU: THydrogeologicUnit;
  ThickArray: TDataArray;
  Thickness: Double;
  SortItem, SortItem1, SortItem2: THguSort;
  TopArray: TDataArray;
  Delta: Double;
  DataArrayManager: TDataArrayManager;
  Gridtop: Real;
  GridBottom: Real;
const
  Epsilon = 1e-2;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, GapWarning);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, OverlapWarning);

    DataArrayManager := Model.DataArrayManager;
    ActiveDataArray := DataArrayManager.GetDataSetByName(rsActive);

    ActiveDataArray.Initialize;

    HguList := TObjectList.Create;
    try
      for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
      begin
        for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
        begin
          Gridtop := 0;
          CellUsed := False;
          for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
          begin
            if Model.IsLayerSimulated(LayerIndex) then
            begin
              if ActiveDataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
              begin
                CellUsed := True;
                Gridtop := Model.ModflowGrid.CellElevation[ColIndex,RowIndex,LayerIndex];
                Break;
              end;
            end;
          end;
          if CellUsed then
          begin
            GridBottom := 0;
            for LayerIndex := Model.ModflowGrid.LayerCount - 1 downto 0 do
            begin
              if Model.IsLayerSimulated(LayerIndex) then
              begin
                if ActiveDataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
                begin
                  GridBottom := Model.ModflowGrid.
                    CellElevation[ColIndex,RowIndex,LayerIndex+1];
                  Break;
                end;
              end;
            end;
            for HguIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
            begin
              HGU := Model.HydrogeologicUnits[HguIndex];
              ThickArray := DataArrayManager.GetDataSetByName(HGU.ThickessDataArrayName);
              ThickArray.Initialize;
              DataArrayManager.AddDataSetToCache(ThickArray);
              Thickness := ThickArray.RealData[0, RowIndex, ColIndex];
              if Thickness > 0 then
              begin
                SortItem := THguSort.Create;
                HguList.Add(SortItem);
                SortItem.HGU := HGU;
                TopArray := DataArrayManager.GetDataSetByName(HGU.TopDataArrayName);
                TopArray.Initialize;
                DataArrayManager.AddDataSetToCache(TopArray);
                SortItem.Top := TopArray.RealData[0, RowIndex, ColIndex];
                SortItem.Bottom := SortItem.Top - Thickness;
              end;
            end;
            HguList.Sort(HguSorter);
            for HguIndex := 1 to HguList.Count - 1 do
            begin
              SortItem1 := HguList[HguIndex-1];
              if (SortItem1.Bottom >= Gridtop)
                or (SortItem1.Top <= GridBottom) then
              begin
                Continue;
              end;
              SortItem2 := HguList[HguIndex];
              if (SortItem2.Bottom >= Gridtop)
                or (SortItem2.Top <= GridBottom) then
              begin
                Continue;
              end;
              Delta := SortItem1.Bottom - SortItem2.Top;
              if Delta > Epsilon then
              begin
                frmErrorsAndWarnings.AddWarning(Model, GapWarning,
                  Format(WarningFormat, [ColIndex+1, RowIndex+1,
                    SortItem1.HGU.HufName, SortItem2.HGU.HufName, Delta]));
              end
              else if Delta < -Epsilon then
              begin
                frmErrorsAndWarnings.AddWarning(Model, OverlapWarning,
                  Format(WarningFormat, [ColIndex+1, RowIndex+1,
                    SortItem1.HGU.HufName, SortItem2.HGU.HufName, -Delta]));
              end;
            end;
            HguList.Clear;
          end;
        end;
      end;
    finally
      HguList.Free;
    end;

    DataArrayManager.AddDataSetToCache(ActiveDataArray);
    DataArrayManager.CacheDataArrays;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowHUF_Writer.CheckHufKx;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrHUFKxName);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      CheckArray(DataArray, LayerIndex, StrLargeContrastInHy,
        cvmGradient, 1e6, etWarning);
      CheckArray(DataArray, LayerIndex, StrHydraulicConductivi,
        cvmGreater, 0, etWarning);
    end;
  end;
end;

procedure TModflowHUF_Writer.CheckHufSS;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  if Model.ModflowStressPeriods.TransientModel then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(StrHUFSSName);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex)
        and (Model.Laytyp[LayerIndex] = 0) then
      begin
        CheckArray(DataArray, LayerIndex, 'Specific Storage less or equal to zero',
          cvmGreater, 0, etWarning);
      end;
    end;
  end;
end;

procedure TModflowHUF_Writer.CheckHufSy;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  if Model.ModflowStressPeriods.TransientModel then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(StrHUFAverageSYName);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex)
        and (Model.Laytyp[LayerIndex] <> 0) then
      begin
        CheckArray(DataArray, LayerIndex, 'Specific Yield less or equal to zero',
          cvmGreater, 0, etWarning);
      end;
    end;
  end;
end;

procedure TModflowHUF_Writer.CheckSytpParameters;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
begin
  if Model.ModflowSteadyParameters.CountParameters([ptHUF_SYTP]) > 1 then
  begin
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      Param := Model.ModflowSteadyParameters.Items[ParamIndex];
      if (Param.ParameterType = ptHUF_SYTP)
        and not Param.UseZone then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingLPFSY,
          Param.ParameterName);
      end;
    end;
  end;
end;

constructor TModflowHUF_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FHufPackage := Package as THufPackageSelection;
end;

class function TModflowHUF_Writer.Extension: string;
begin
  result := '.huf';
end;

function TModflowHUF_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.HufPackage;
end;

procedure TModflowHUF_Writer.WriteDataSet1;
var
  IHUFCB: integer;
  HDRY: double;
  NHUF: integer;
  NPHUF: integer;
  IOHUFHEADS: integer;
  IOHUFFLOWS: integer;
  NameOfFile: string;
begin
  IHUFCB := 0;
  GetFlowUnitNumber(IHUFCB);
  HDRY := Model.ModflowOptions.HDry;
  NHUF := Model.HydrogeologicUnits.Count;
  FTransient := Model.ModflowFullStressPeriods.TransientModel;
  if FTransient then
  begin
    NPHUF := Model.HufParameters.CountParameters(HufSteadyAndTransientParameters) +
      Model.ModflowSteadyParameters.CountParameters([ptHUF_SYTP]);
  end
  else
  begin
    NPHUF := Model.HufParameters.CountParameters(HufSteadyParameters);
  end;
  if FHufPackage.SaveHeads then
  begin
    IOHUFHEADS := Model.UnitNumbers.UnitNumber(StrIOHUFHEADS);
    if Model.ModflowOutputControl.HeadOC.FormatDefined then
    begin
      NameOfFile := ChangeFileExt(FNameOfFile, StrHuffhd);
      WriteToNameFile(StrDATA, IOHUFHEADS,
        NameOfFile, foOutput, Model);
    end
    else
    begin
      NameOfFile := ChangeFileExt(FNameOfFile, StrHufbhd);
      WriteToNameFile(StrDATABINARY, IOHUFHEADS,
        NameOfFile, foOutput, Model);
    end;
  end
  else
  begin
    IOHUFHEADS := 0;
  end;
  if FHufPackage.SaveFlows then
  begin
    IOHUFFLOWS := Model.UnitNumbers.UnitNumber(StrIOHUFFLOWS);
    NameOfFile := ChangeFileExt(FNameOfFile, StrHufflow);
    WriteToNameFile(StrDATABINARY, IOHUFFLOWS,
      NameOfFile, foOutput, Model);
  end
  else
  begin
    IOHUFFLOWS := 0;
  end;

  WriteInteger(IHUFCB);
  WriteFloat(HDRY);
  WriteInteger(NHUF);
  WriteInteger(NPHUF);
  WriteInteger(IOHUFHEADS);
  WriteInteger(IOHUFFLOWS);
  WriteString(' # Data set 1 IHUFCB HDRY NHUF NPHUF IOHUFHEADS IOHUFFLOWS');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet12;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  HGUNAM: string;
  PRINTCODE: Integer;
  PrintIndex: TPrintParam;
  PrintItem: TPrintItem;
  PRINTFLAGS: string;
begin
  for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
  begin
    HGU := Model.HydrogeologicUnits[UnitIndex];
    HGUNAM := HGU.HufName;
    PRINTCODE := HGU.PrintFormat;
    PRINTFLAGS := '';
    for PrintIndex := Low(TPrintParam) to High(TPrintParam) do
    begin
      PrintItem := HGU.PrintItems[PrintIndex];
      if PrintItem.ShouldPrint then
      begin
        PRINTFLAGS := PRINTFLAGS + ' ' + PrintItem.PrintString;
      end;
    end;
    if PRINTFLAGS <> '' then
    begin
      WriteString('PRINT ');
      WriteString(HGUNAM);
      WriteInteger(PRINTCODE);
      WriteString(PRINTFLAGS);
      NewLine;
    end;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet2;
var
  LTHUF: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LTHUF := Model.Laytyp;
  for LayerIndex := 0 to Length(LTHUF) - 1 do
  begin
    WriteInteger(LTHUF[LayerIndex]);
  end;
  WriteString(' # Data set 2: LTHUF');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet3;
var
  index: Integer;
  LTHUF: TOneDIntegerArray;
begin
  LTHUF := Model.Laytyp;
  FConvertibleLayerPresent := False;
  for index := 0 to Model.ModflowLayerCount - 1 do
  begin
    if Model.ModflowWettingOptions.WettingActive
      and (LTHUF[index] <> 0) then
    begin
      WriteInteger(1);
      FConvertibleLayerPresent := True;
    end
    else
    begin
      WriteInteger(0);
    end;
  end;

  WriteString(' # Data set 3: LAYWT');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet4;
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
    WriteString(' # Data set 4: WETFCT, IWETIT, IHDWET');
    NewLine;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet5;
var
  MFLayerIndex: Integer;
  DataArrayLayerIndex: Integer;
  DataArray: TDataArray;
  LayerDescription: string;
  LTHUF: TOneDIntegerArray;
begin
  LTHUF := Model.Laytyp;
  if Model.ModflowWettingOptions.WettingActive then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsWetDry);
    Assert(DataArray <> nil);

    try
      for MFLayerIndex := 1 to Model.ModflowLayerCount do
      begin
        if LTHUF[MFLayerIndex-1] <> 0 then
        begin
          DataArrayLayerIndex := Model.
            ModflowLayerToDataSetLayer(MFLayerIndex);

          LayerDescription := Model.
            ModflowLayerBottomDescription(DataArrayLayerIndex);

          WriteArray(DataArray, DataArrayLayerIndex,
            'Data set 5: WETDRY ' + LayerDescription, StrNoValueAssigned,
            'WETDRY');
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;
    finally
      Model.DataArrayManager.CacheDataArrays;
    end;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet6(HGU: THydrogeologicUnit);
begin
  WriteString(HGU.HufName);
  WriteString(' # Data set 6: HGUNAM');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet7(HGU: THydrogeologicUnit);
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(HGU.TopDataArrayName);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, ' Data set 7: TOP ' + HGU.HufName, StrNoValueAssigned, 'TOP');
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowHUF_Writer.WriteDataSet8(HGU: THydrogeologicUnit);
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(HGU.ThickessDataArrayName);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, ' Data set 8:  THCK ' + HGU.HufName, StrNoValueAssigned, 'THCK');
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowHUF_Writer.WriteDataSet9;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  HGUHANI: double;
  HGUVANI: double;
  HGUNAM: string;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoVKparameter);
    for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
    begin
      HGU := Model.HydrogeologicUnits[UnitIndex];
      HGUNAM := HGU.HufName;
    
      if HGU.UsesHaniParam then
      begin
        HGUHANI := 0;
      end
      else
      begin
        HGUHANI := HGU.HorizontalAnisotropy;
      end;

      HGUVANI := 0;
      case HGU.VK_Method of
        vkVK: HGUVANI := 0;
        vkVANI: HGUVANI := HGU.VerticalAnisotropy;
        else Assert(False);
      end;
      if HGUVANI = 0 then
      begin
        if (not HGU.UsesVkParam)
          and (Model.ModflowLayerCount > 1) then
        begin
          frmErrorsAndWarnings.AddError(Model, StrNoVKparameter, HGU.HufName);
        end;
      end;
      WriteString(HGUNAM);
      WriteFloat(HGUHANI);
      WriteFloat(HGUVANI);
      WriteString(' # Data set 9: HGUNAM HGUHANI HGUVANI');
      NewLine;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSets10and11;
const
  NoClusters = 'The following parameters in the HUF2 package are not used with '
    + 'any hydrogeologic units.';
var
  ParamIndex: Integer;
  Parameter: THufParameter;
  HufUnitIndex: Integer;
  HGU: THydrogeologicUnit;
  UsedParam: THufUsedParameter;
  UsedParameters: TList;
  PARNAM: string;
  PARTYP: string;
  PARVAL: Double;
  NCLU: Integer;
  Param: TModflowSteadyParameter;
  ClusterIndex: Integer;
  UsedHufUnits: TList;
  HGUNAM: string;
  Mltarr: string;
  Zonarr: string;
  IZ: string;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, NoClusters);
    UsedParameters := TList.Create;
    UsedHufUnits := TList.Create;
    try
      for ParamIndex := 0 to Model.HufParameters.Count - 1 do
      begin
        Parameter := Model.HufParameters[ParamIndex];
        if FTransient then
        begin
          if not (Parameter.ParameterType in HufSteadyAndTransientParameters) then
          begin
            Continue;
          end;
        end
        else
        begin
          if not (Parameter.ParameterType in HufSteadyParameters) then
          begin
            Continue;
          end;
        end;
        UsedParameters.Clear;
        UsedHufUnits.Clear;
        for HufUnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
        begin
          HGU := Model.HydrogeologicUnits[HufUnitIndex];
          UsedParam := HGU.UsesParameter(Parameter);
          if UsedParam <> nil then
          begin
            UsedParameters.Add(UsedParam);
            UsedHufUnits.Add(HGU);
          end;
        end;

        if UsedParameters.Count = 0 then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            NoClusters, Parameter.ParameterName);
        end;

        PARNAM := ExpandString(Parameter.ParameterName, 10);
        case Parameter.ParameterType of
          ptHUF_HK: PARTYP := 'HK';
          ptHUF_HANI: PARTYP := 'HANI';
          ptHUF_VK: PARTYP := 'VK';
          ptHUF_VANI: PARTYP := 'VANI';
          ptHUF_SS: PARTYP := 'SS';
          ptHUF_SY: PARTYP := 'SY';
          else Assert(False);
        end;
        PARTYP := ExpandString(' ' + PARTYP, 10);
        PARVAL := Parameter.Value;
        NCLU := UsedParameters.Count;
      
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(PARVAL);
        WriteInteger(NCLU);
        WriteString(' # Data set 10: PARNAM PARTYP Parval NCLU');
        NewLine;
        Model.WritePValAndTemplate(PARNAM,PARVAL);

        for ClusterIndex := 0 to UsedParameters.Count - 1 do
        begin
          UsedParam := UsedParameters[ClusterIndex];
          HGU := UsedHufUnits[ClusterIndex];
          HGUNAM := ExpandString(HGU.HufName, 10);

          if UsedParam.UseMultiplier then
          begin
            UsedParam.GenerateMultiplierArrayName;
            Mltarr := UsedParam.MultiplierArrayName;
            UsedMultiplierArrayNames.Add(Mltarr);
          end
          else
          begin
            Mltarr := ' NONE      ';
          end;
          Mltarr := ExpandString(Mltarr, 10);

          if UsedParam.UseZone then
          begin
            UsedParam.GenerateZoneArrayName;
            Zonarr := UsedParam.ZoneArrayName;
            UsedZoneArrayNames.Add(Zonarr);
            IZ := ' 1';
          end
          else
          begin
            Zonarr := ' ALL       ';
            IZ := '';
          end;
          Zonarr := ExpandString(Zonarr, 10);

          WriteString(HGUNAM + ' ');
          WriteString(Mltarr + ' ');
          WriteString(Zonarr + ' ');
          WriteString(IZ);
          WriteString(' # Data Set 11: HGUNAM Mltarr Zonarr IZ');
          NewLine;
        end;
      end;
    finally
      UsedParameters.Free;
      UsedHufUnits.Free;
    end;

    if FTransient then
    begin
      for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
      begin
        Param := Model.ModflowSteadyParameters.Items[ParamIndex];
        if Param.ParameterType = ptHUF_SYTP then
        begin
          Param.ClearArrayNames;
          PARNAM := Param.ParameterName;
          PARTYP := ' SYTP';
          PARVAL := Param.Value;
          NCLU := 1;
    
          WriteString(PARNAM);
          WriteString(PARTYP);
          WriteFloat(PARVAL);
          WriteInteger(NCLU);
          WriteString(' # Data set 10: PARNAM PARTYP Parval NCLU');
          NewLine;
          Model.WritePValAndTemplate(PARNAM,PARVAL);

          HGUNAM := 'SYTP';

          if Param.UseMultiplier then
          begin
            Mltarr := Param.MultiplierArrayName(1, Model);
            UsedMultiplierArrayNames.Add(Mltarr);
          end
          else
          begin
            Mltarr := ' NONE ';
          end;

          if Param.UseZone then
          begin
            Zonarr := Param.ZoneArrayName(1, Model);
            IZ := ' 1';
            UsedZoneArrayNames.Add(Zonarr);
          end
          else
          begin
            Zonarr := ' ALL ';
            IZ := '';
          end;

          WriteString(HGUNAM + ' ');
          WriteString(Mltarr + ' ');
          WriteString(Zonarr + ' ');
          WriteString(IZ);
          WriteString(' # Data Set 11: HGUNAM Mltarr Zonarr IZ');
          NewLine;
        end;
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSets6to8;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
begin
  for UnitIndex := 0 to Model.HydrogeologicUnits.Count - 1 do
  begin
    HGU := Model.HydrogeologicUnits[UnitIndex];
    frmProgressMM.AddMessage(Format(StrWritingDataSet6, [HGU.HufName]));
    WriteDataSet6(HGU);
    frmProgressMM.AddMessage(Format(StrWritingDataSet7, [HGU.HufName]));
    WriteDataSet7(HGU);
    frmProgressMM.AddMessage(Format(StrWritingDataSet8, [HGU.HufName]));
    WriteDataSet8(HGU);
  end;
end;

procedure TModflowHUF_Writer.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingLPFSY);
  if not FHufPackage.IsSelected then
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

  CheckSpecifiedHeadsConnected;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrHUF2, Model.UnitNumbers.UnitNumber(StrHUF2),
    FNameOfFile, foInput, Model);
  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    CheckSytpParameters;
    frmProgressMM.AddMessage(StrWritingHUF2Package);
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

    WriteDataSets6to8;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet9);
    WriteDataSet9;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets10and11);
    WriteDataSets10and11;
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

    frmProgressMM.AddMessage(StrCheckingElevation);
    CheckElevations;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrCheckingHydraulicC);
    CheckHufKx;
    CheckHufSS;
    CheckHufSy;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

end.
