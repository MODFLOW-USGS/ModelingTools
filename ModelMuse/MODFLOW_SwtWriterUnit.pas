unit MODFLOW_SwtWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, IntListUnit,
  PhastModelUnit, Classes;

type
  TModflowSWT_Writer = class(TCustomSubWriter)
  private
    FSwtPackage: TSwtPackageSelection;
    FNameOfFile: string;
    // model layer assignments for each system of no-delay interbeds
    FLNWT: TIntegerList;
    FTHICK_List: TList;
    FSse_List: TList;
    FSsv_List: TList;
    FCr_List: TList;
    FCc_List: TList;
    FVOID_List: TList;
    FSUB_List: TList;
    procedure RetrieveArrays;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSets7to13;
    procedure WriteDataSet14;
    procedure WriteDataSet15;
    procedure WriteDataSet16;
    procedure WriteDataSet17;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
  end;


implementation

uses
  ModflowUnitNumbers, frmProgressUnit, LayerStructureUnit,
  ModflowSubsidenceDefUnit, DataSetUnit, frmErrorsAndWarningsUnit, SysUtils,
  Forms, GoPhastTypes;

resourcestring
  StrNoSWTLayersDefine = 'No SWT layers defined';
  StrInTheSWTPackage = 'In the SWT package, no systems of interbeds have bee' +
  'n defined in the MODFLOW Layer Groups dialog box.';
  StrWritingSWTPackage = 'Writing SWT Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
  StrWritingDataSets7to13 = '  Writing Data Sets 7 to 13.';
  StrNoWatertableSubsi = 'No water-table subsidence layers defined.';
  StrNoWatertableSubsiDetailed = 'No water-table subsidence beds  in the Water-Table' +
  ' Subsidence package have been defined for any layer in the model. They mu' +
  'st be defined in the "Model|MODFLOW Layers" dialog box.);'#13;
//  StrWritingDataSet14 = '  Writing Data Set 14.';
//  StrWritingDataSet15 = '  Writing Data Set 15.';
//  StrWritingDataSet16 = '  Writing Data Set 16.';
//  StrWritingDataSet17 = '  Writing Data Set 17.';

{ TModflowSWT_Writer }

constructor TModflowSWT_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FLNWT := TIntegerList.Create;
  FTHICK_List := TList.Create;
  FSse_List := TList.Create;
  FSsv_List := TList.Create;
  FCr_List := TList.Create;
  FCc_List := TList.Create;
  FVOID_List := TList.Create;
  FSUB_List := TList.Create;
end;

destructor TModflowSWT_Writer.Destroy;
begin
  FSUB_List.Free;
  FVOID_List.Free;
  FCc_List.Free;
  FCr_List.Free;
  FSsv_List.Free;
  FSse_List.Free;
  FTHICK_List.Free;
  FLNWT.Free;
  inherited;
end;

class function TModflowSWT_Writer.Extension: string;
begin
  result := '.swt';
end;

function TModflowSWT_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SwtPackage;
end;

procedure TModflowSWT_Writer.RetrieveArrays;
var
  Layers: TLayerStructure;
  MFLayer_Group: Integer;
  GroupIndex: Integer;
  Group: TLayerGroup;
  SubsidenceIndex: Integer;
  WT_Item: TSwtWaterTableItem;
  CompressibleThicknessDataArray: TDataArray;
  InitialElasticSkeletalSpecificStorageDataArray: TDataArray;
  InitialInelasticSkeletalSpecificStorageDataArray: TDataArray;
  RecompressionIndexDataArray: TDataArray;
  CompressionIndexDataArray: TDataArray;
  InitialVoidRatioDataArray: TDataArray;
  InitialCompactionDataArray: TDataArray;
  LayerIndex: Integer;
begin
  Layers := Model.LayerStructure;
  MFLayer_Group := 0;
  for GroupIndex := 1 to Layers.Count - 1 do
  begin
    Group := Layers.LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      for SubsidenceIndex := 0 to Group.WaterTableLayers.Count - 1 do
      begin
        WT_Item := Group.WaterTableLayers[SubsidenceIndex];
        CompressibleThicknessDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableCompressibleThicknessDataArrayName);
        Assert(CompressibleThicknessDataArray <> nil);

        InitialElasticSkeletalSpecificStorageDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName);
        if FSwtPackage.CompressionSource = csSpecificStorage then
        begin
          Assert(InitialElasticSkeletalSpecificStorageDataArray <> nil);
        end;

        InitialInelasticSkeletalSpecificStorageDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName);
        if FSwtPackage.CompressionSource = csSpecificStorage then
        begin
          Assert(InitialInelasticSkeletalSpecificStorageDataArray <> nil);
        end;

        RecompressionIndexDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableRecompressionIndexDataArrayName);
        if FSwtPackage.CompressionSource = csCompressionReComp then
        begin
          Assert(RecompressionIndexDataArray <> nil);
        end;

        CompressionIndexDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableCompressionIndexDataArrayName);
        if FSwtPackage.CompressionSource = csCompressionReComp then
        begin
          Assert(CompressionIndexDataArray <> nil);
        end;

        InitialVoidRatioDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableInitialVoidRatioDataArrayName);
        Assert(InitialVoidRatioDataArray <> nil);

        InitialCompactionDataArray := Model.DataArrayManager.GetDataSetByName(
          WT_Item.WaterTableInitialCompactionDataArrayName);
        Assert(InitialCompactionDataArray <> nil);

        if Group.LayerCount = 1 then
        begin
          FLNWT.Add(MFLayer_Group+1);
          FTHICK_List.Add(CompressibleThicknessDataArray);
          FSse_List.Add(InitialElasticSkeletalSpecificStorageDataArray);
          FSsv_List.Add(InitialInelasticSkeletalSpecificStorageDataArray);
          FCr_List.Add(RecompressionIndexDataArray);
          FCc_List.Add(CompressionIndexDataArray);
          FVOID_List.Add(InitialVoidRatioDataArray);
          FSUB_List.Add(InitialCompactionDataArray);
        end
        else
        begin
          if WT_Item.UseInAllLayers then
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              FLNWT.Add(MFLayer_Group+LayerIndex);
              FTHICK_List.Add(CompressibleThicknessDataArray);
              FSse_List.Add(InitialElasticSkeletalSpecificStorageDataArray);
              FSsv_List.Add(InitialInelasticSkeletalSpecificStorageDataArray);
              FCr_List.Add(RecompressionIndexDataArray);
              FCc_List.Add(CompressionIndexDataArray);
              FVOID_List.Add(InitialVoidRatioDataArray);
              FSUB_List.Add(InitialCompactionDataArray);
            end;
          end
          else
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              if WT_Item.UsedLayers.GetItemByLayerNumber(LayerIndex) <> nil then
              begin
                FLNWT.Add(MFLayer_Group+LayerIndex);
                FTHICK_List.Add(CompressibleThicknessDataArray);
                FSse_List.Add(InitialElasticSkeletalSpecificStorageDataArray);
                FSsv_List.Add(InitialInelasticSkeletalSpecificStorageDataArray);
                FCr_List.Add(RecompressionIndexDataArray);
                FCc_List.Add(CompressionIndexDataArray);
                FVOID_List.Add(InitialVoidRatioDataArray);
                FSUB_List.Add(InitialCompactionDataArray);
              end;
            end;
          end;
        end;
      end;
      Inc(MFLayer_Group,Group.LayerCount);
    end;
  end;
  Assert(FLNWT.Count = FTHICK_List.Count);
  Assert(FLNWT.Count = FSse_List.Count);
  Assert(FLNWT.Count = FSsv_List.Count);
  Assert(FLNWT.Count = FCr_List.Count);
  Assert(FLNWT.Count = FCc_List.Count);
  Assert(FLNWT.Count = FVOID_List.Count);
  Assert(FLNWT.Count = FSUB_List.Count);
end;

procedure TModflowSWT_Writer.WriteDataSet1;
var
  ISWTCB, ISWTOC, NSYSTM, ITHK, IVOID, ISTPCS, ICRCC: integer;
begin
  GetFlowUnitNumber(ISWTCB);
  ISWTOC := FSwtPackage.PrintChoices.Count;
  NSYSTM := Model.LayerStructure.WaterTableCount;
  ITHK := Ord(FSwtPackage.ThickResponse);
  IVOID := Ord(FSwtPackage.VoidRatioResponse);
  ISTPCS := Ord(FSwtPackage.PreconsolidationSource);
  ICRCC := Ord(FSwtPackage.CompressionSource);

  if NSYSTM = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoWatertableSubsi,
      StrNoWatertableSubsiDetailed);
  end;

  WriteInteger(ISWTCB);
  WriteInteger(ISWTOC);
  WriteInteger(NSYSTM);
  WriteInteger(ITHK);
  WriteInteger(IVOID);
  WriteInteger(ISTPCS);
  WriteInteger(ICRCC);
  WriteString(' # Data Set 1: ISWTCB ISWTOC NSYSTM ITHK IVOID ISTPCS ICRCC');
  NewLine;
end;

procedure TModflowSWT_Writer.WriteDataSet14;
var
  DataArray: TDataArray;
  MFLayerIndex: Integer;
//  GroupIndex: Integer;
//  Group: TLayerGroup;
  LayerIndex: Integer;
  ArrayIndex: Integer;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrInitialPreOffsets);

  MFLayerIndex := 0;
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
      ArrayIndex := Model.
        ModflowLayerToDataSetLayer(MFLayerIndex);

      WriteArray(DataArray, ArrayIndex,
        'Data Set 14: PCSOFF, Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'PCSOFF');
    end;
  end;

{  for GroupIndex := 1 to Model.LayerStructure.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    Group := Model.LayerStructure.LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount -1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        Inc(MFLayerIndex);
        ArrayIndex := Model.
          ModflowLayerToDataSetLayer(MFLayerIndex);

        WriteArray(DataArray, ArrayIndex,
          'Data Set 14: PCSOFF, Layer ' + IntToStr(MFLayerIndex));
      end;
    end;
  end;  }
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteDataSet15;
var
  DataArray: TDataArray;
  MFLayerIndex: Integer;
//  GroupIndex: Integer;
//  Group: TLayerGroup;
  LayerIndex: Integer;
  ArrayIndex: Integer;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrInitialPreconsolida);

  MFLayerIndex := 0;
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
      ArrayIndex := Model.
        ModflowLayerToDataSetLayer(MFLayerIndex);

      WriteArray(DataArray, ArrayIndex,
        'Data Set 15: PCS, Layer ' + IntToStr(MFLayerIndex), StrNoValueAssigned, 'PCS');
    end;
  end;


{  for GroupIndex := 1 to Model.LayerStructure.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    Group := Model.LayerStructure.LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount -1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        Inc(MFLayerIndex);
        ArrayIndex := Model.
          ModflowLayerToDataSetLayer(MFLayerIndex);

        WriteArray(DataArray, ArrayIndex,
          'Data Set 15: PCS, Layer ' + IntToStr(MFLayerIndex));
      end;
    end;
  end;  }
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteDataSet16;
var
  PrintFormats: TSwtPrintFormats;
  Ifm1: Integer;
  Ifm2: Integer;
  Ifm3: Integer;
  Ifm4: Integer;
  Ifm5: Integer;
  Ifm6: Integer;
  Ifm7: Integer;
  Ifm8: Integer;
  Ifm9: Integer;
  Ifm10: Integer;
  Ifm11: Integer;
  Ifm12: Integer;
  Ifm13: Integer;
  Save1: Boolean;
  Save2: Boolean;
  Save3: Boolean;
  Save4: Boolean;
  Save5: Boolean;
  Save6: Boolean;
  Save7: Boolean;
  Save8: Boolean;
  Save9: Boolean;
  Save10: Boolean;
  Save11: Boolean;
  Save12: Boolean;
  Save13: Boolean;
  Index: Integer;
  PrintChoice: TSwtPrintItem;
  Iun1: Integer;
  SwtFileName: string;
//  AFileName: string;
  Iun2: Integer;
  Iun3: Integer;
  Iun4: Integer;
  Iun5: Integer;
  Iun6: Integer;
  Iun7: Integer;
  Iun8: Integer;
  Iun9: Integer;
  Iun10: Integer;
  Iun11: Integer;
  Iun12: Integer;
  Iun13: Integer;
  function GetCombinedUnitNumber: integer;
  begin
    result := Model.UnitNumbers.UnitNumber(StrSwtSUB_Out);
    if SwtFileName = '' then
    begin
      SwtFileName := ExtractFileName(ChangeFileExt(FNameOfFile, StrSwtOut));
      WriteToNameFile(StrDATABINARY, result,
        SwtFileName, foOutput, Model);
    end;
  end;
  function HandleUnitNumber(Save: boolean; const Key, Extension: string): integer;
  var
    AFileName: string;
  begin
    result := 0;
    if Save then
    begin
      case FSwtPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            result := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            result := Model.UnitNumbers.UnitNumber(Key);
            AFileName := ExtractFileName(ChangeFileExt(FNameOfFile, Extension));
            WriteToNameFile(StrDATABINARY, result, AFileName, foOutput, Model);
          end
        else Assert(False);
      end;
    end;
  end;
begin
  SwtFileName := '';
  PrintFormats := FSwtPackage.PrintFormats;
  Ifm1 := PrintFormats.SubsidenceFormat+1;
  Ifm2 := PrintFormats.CompactionByModelLayerFormat+1;
  Ifm3 := PrintFormats.CompactionByInterbedSystemFormat+1;
  Ifm4 := PrintFormats.VerticalDisplacementFormat+1;
  Ifm5 := PrintFormats.PreconsolidationStress+1;
  Ifm6 := PrintFormats.DeltaPreconsolidationStress+1;
  Ifm7 := PrintFormats.GeostaticStress+1;
  Ifm8 := PrintFormats.DeltaGeostaticStress+1;
  Ifm9 := PrintFormats.EffectiveStress+1;
  Ifm10 := PrintFormats.DeltaEffectiveStress+1;
  Ifm11 := PrintFormats.VoidRatio+1;
  Ifm12 := PrintFormats.ThicknessCompressibleSediments+1;
  Ifm13 := PrintFormats.LayerCenterElevation+1;

  Save1 := False;
  Save2 := False;
  Save3 := False;
  Save4 := False;
  Save5 := False;
  Save6 := False;
  Save7 := False;
  Save8 := False;
  Save9 := False;
  Save10 := False;
  Save11 := False;
  Save12 := False;
  Save13 := False;
  for Index := 0 to FSwtPackage.PrintChoices.Count - 1 do
  begin
    PrintChoice := FSwtPackage.PrintChoices[Index];
    Save1 := Save1 or PrintChoice.SaveSubsidence;
    Save2 := Save2 or PrintChoice.SaveCompactionByModelLayer;
    Save3 := Save3 or PrintChoice.SaveCompactionByInterbedSystem;
    Save4 := Save4 or PrintChoice.SaveVerticalDisplacement;
    Save5 := Save5 or PrintChoice.SavePreconsolidationStress;
    Save6 := Save6 or PrintChoice.SaveDeltaPreconsolidationStress;
    Save7 := Save7 or PrintChoice.SaveGeostaticStress;
    Save8 := Save8 or PrintChoice.SaveDeltaGeostaticStress;
    Save9 := Save9 or PrintChoice.SaveEffectiveStress;
    Save10 := Save10 or PrintChoice.SaveDeltaEffectiveStress;
    Save11 := Save11 or PrintChoice.SaveVoidRatio;
    Save12 := Save12 or PrintChoice.SaveThicknessCompressibleSediments;
    Save13 := Save13 or PrintChoice.SaveLayerCenterElevation;
    if Save1 and Save2 and Save3 and Save4 and Save5 and Save6 and Save6
      and Save8 and Save9 and Save10 and Save11 and Save12 and Save13 then
    begin
      break;
    end;
  end;

  Iun1 := HandleUnitNumber(Save1, StrSwtSUB_Out, StrSwtSubOut);
  Iun2 := HandleUnitNumber(Save2, StrSwtComML_Out, StrSwtComMLOut);
  Iun3 := HandleUnitNumber(Save3, StrSwtCOM_IS_Out, StrSwtComIsOut);
  Iun4 := HandleUnitNumber(Save4, StrSwt_VD_Out, StrSwtVDOut);
  Iun5 := HandleUnitNumber(Save5, StrSwt_PreCon_Out, StrSwtPreConStrOut);
  Iun6 := HandleUnitNumber(Save6, StrSwt_DeltaPreCon_Out, StrSwtDeltaPreConStrOu);
  Iun7 := HandleUnitNumber(Save7, StrSwt_GeoStat_Out, StrSwtGeoStatOut);
  Iun8 := HandleUnitNumber(Save8, StrSwt_DeltaGeoStat_Out, StrSwtDeltaGeoStatOut);
  Iun9 := HandleUnitNumber(Save9, StrSwt_EffStress_Out, StrSwtEffStressOut);
  Iun10 := HandleUnitNumber(Save10, StrSwt_DeltaEffStress_Out, StrSwtDeltaEffStressOu);
  Iun11 := HandleUnitNumber(Save11, StrSwt_VoidRatio_Out, StrSwtVoidRatioOut);
  Iun12 := HandleUnitNumber(Save12, StrSwt_ThickCompSed_Out, StrSwtThickCompSedOut);
  Iun13 := HandleUnitNumber(Save13, StrSwt_LayerCentElev_Out, StrSwtLayerCentElevOut);

  WriteInteger(Ifm1 );
  WriteInteger(Iun1 );
  WriteInteger(Ifm2 );
  WriteInteger(Iun2 );
  WriteInteger(Ifm3 );
  WriteInteger(Iun3 );
  WriteInteger(Ifm4 );
  WriteInteger(Iun4 );
  WriteInteger(Ifm5 );
  WriteInteger(Iun5 );
  WriteInteger(Ifm6 );
  WriteInteger(Iun6 );
  WriteInteger(Ifm7 );
  WriteInteger(Iun7 );
  WriteInteger(Ifm8 );
  WriteInteger(Iun8 );
  WriteInteger(Ifm9 );
  WriteInteger(Iun9 );
  WriteInteger(Ifm10);
  WriteInteger(Iun10);
  WriteInteger(Ifm11);
  WriteInteger(Iun11);
  WriteInteger(Ifm12);
  WriteInteger(Iun12);
  WriteInteger(Ifm13);
  WriteInteger(Iun13);
  WriteString(' # Data Set 16: Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 ... Ifm13 Iun13');
  NewLine;
end;

procedure TModflowSWT_Writer.WriteDataSet17;
var
  PrintChoiceIndex: Integer;
  PrintChoice: TSwtPrintItem;
  ISP1: Integer;
  ITS1: Integer;
  ISP2: Integer;
  ITS2: Integer;
  Ifl1: Integer;
  Ifl2: Integer;
  Ifl3: Integer;
  Ifl4: Integer;
  Ifl5: Integer;
  Ifl6: Integer;
  Ifl7: Integer;
  Ifl8: Integer;
  Ifl9: Integer;
  Ifl10: Integer;
  Ifl11: Integer;
  Ifl12: Integer;
  Ifl13: Integer;
  Ifl14: Integer;
  Ifl15: Integer;
  Ifl16: Integer;
  Ifl17: Integer;
  Ifl18: Integer;
  Ifl19: Integer;
  Ifl20: Integer;
  Ifl21: Integer;
  Ifl22: Integer;
  Ifl23: Integer;
  Ifl24: Integer;
  Ifl25: Integer;
  Ifl26: Integer;
begin
  FSwtPackage.PrintChoices.ReportErrors;
  for PrintChoiceIndex := 0 to FSwtPackage.PrintChoices.Count -1 do
  begin
    PrintChoice := FSwtPackage.PrintChoices[PrintChoiceIndex];
    if PrintChoice.StartTime <= PrintChoice.EndTime then
    begin
      GetStartAndEndTimeSteps(ITS2, ISP2, ITS1, ISP1, PrintChoice);
      Ifl1 := Ord(PrintChoice.PrintSubsidence);
      Ifl2  := Ord(PrintChoice.SaveSubsidence);
      Ifl3 := Ord(PrintChoice.PrintCompactionByModelLayer);
      Ifl4 := Ord(PrintChoice.SaveCompactionByModelLayer);
      Ifl5 := Ord(PrintChoice.PrintCompactionByInterbedSystem);
      Ifl6 := Ord(PrintChoice.SaveCompactionByInterbedSystem);
      Ifl7 := Ord(PrintChoice.PrintVerticalDisplacement);
      Ifl8 := Ord(PrintChoice.SaveVerticalDisplacement);
      Ifl9 := Ord(PrintChoice.PrintPreconsolidationStress);
      Ifl10 := Ord(PrintChoice.SavePreconsolidationStress);
      Ifl11 := Ord(PrintChoice.PrintDeltaPreconsolidationStress);
      Ifl12 := Ord(PrintChoice.SaveDeltaPreconsolidationStress);
      Ifl13 := Ord(PrintChoice.PrintGeostaticStress);
      Ifl14 := Ord(PrintChoice.SaveGeostaticStress);
      Ifl15 := Ord(PrintChoice.PrintDeltaGeostaticStress);
      Ifl16 := Ord(PrintChoice.SaveDeltaGeostaticStress);
      Ifl17 := Ord(PrintChoice.PrintEffectiveStress);
      Ifl18 := Ord(PrintChoice.SaveEffectiveStress);
      Ifl19 := Ord(PrintChoice.PrintDeltaEffectiveStress);
      Ifl20 := Ord(PrintChoice.SaveDeltaEffectiveStress);
      Ifl21 := Ord(PrintChoice.PrintVoidRatio);
      Ifl22 := Ord(PrintChoice.SaveVoidRatio);
      Ifl23 := Ord(PrintChoice.PrintThicknessCompressibleSediments);
      Ifl24 := Ord(PrintChoice.SaveThicknessCompressibleSediments);
      Ifl25 := Ord(PrintChoice.PrintLayerCenterElevation);
      Ifl26 := Ord(PrintChoice.SaveLayerCenterElevation);
      WriteInteger(ISP1);
      WriteInteger(ISP2);
      WriteInteger(ITS1);
      WriteInteger(ITS2);
      WriteInteger(Ifl1);
      WriteInteger(Ifl2);
      WriteInteger(Ifl3);
      WriteInteger(Ifl4);
      WriteInteger(Ifl5);
      WriteInteger(Ifl6);
      WriteInteger(Ifl7);
      WriteInteger(Ifl8);
      WriteInteger(Ifl9);
      WriteInteger(Ifl10);
      WriteInteger(Ifl11);
      WriteInteger(Ifl12);
      WriteInteger(Ifl13);
      WriteInteger(Ifl14);
      WriteInteger(Ifl15);
      WriteInteger(Ifl16);
      WriteInteger(Ifl17);
      WriteInteger(Ifl18);
      WriteInteger(Ifl19);
      WriteInteger(Ifl20);
      WriteInteger(Ifl21);
      WriteInteger(Ifl22);
      WriteInteger(Ifl23);
      WriteInteger(Ifl24);
      WriteInteger(Ifl25);
      WriteInteger(Ifl26);
      WriteString(' # Data Set 17: ISP1 ISP2 ITS1 ITS2 Ifl1 Ifl2 Ifl3 ... Ifl26');
      NewLine;
    end;
  end;

end;

procedure TModflowSWT_Writer.WriteDataSet2;
var
  Index: Integer;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoSWTLayersDefine);
  if FLNWT.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoSWTLayersDefine,
      StrInTheSWTPackage);
    Exit;
  end;
  for Index := 0 to FLNWT.Count - 1 do
  begin
    WriteFreeInteger(FLNWT[Index]);
  end;
  WriteString(' # Data Set 2: LNWT');
  NewLine;
end;

procedure TModflowSWT_Writer.WriteDataSet3;
var
//  IZCFL IZCFM IGLFL IGLFM IESTFL IESTFM IPCSFL IPCSFM ISTFL ISTFM: integer;
  IZCFL, IZCFM, IGLFL, IGLFM, IESTFL, IESTFM, IPCSFL, IPCSFM, ISTFL, ISTFM: integer;
  InitialPrint: TSwtInitialPrint;
begin
  InitialPrint := FSwtPackage.InitialPrint;
  IZCFL := Ord(InitialPrint.PrintInitialLayerCenterElevations);
  IZCFM := InitialPrint.InitialLayerCenterElevationFormat+1;

  IGLFL := Ord(InitialPrint.PrintInitialGeostaticStress);
  IGLFM := InitialPrint.InitialGeostaticStressFormat+1;

  IESTFL := Ord(InitialPrint.PrintInitialEffectiveStress);
  IESTFM := InitialPrint.InitialEffectiveStressFormat+1;

  IPCSFL := Ord(InitialPrint.PrintInitialPreconsolidationStress);
  IPCSFM := InitialPrint.InitialPreconsolidationStressFormat+1;

  ISTFL := Ord(InitialPrint.PrintInitialEquivalentStorageProperties);
  ISTFM := InitialPrint.InitialEquivalentStoragePropertiesFormat+1;

  WriteInteger(IZCFL);
  WriteInteger(IZCFM);
  WriteInteger(IGLFL);
  WriteInteger(IGLFM);
  WriteInteger(IESTFL);
  WriteInteger(IESTFM);
  WriteInteger(IPCSFL);
  WriteInteger(IPCSFM);
  WriteInteger(ISTFL);
  WriteInteger(ISTFM);
  WriteString(' # Data Set 3: IZCFL IZCFM IGLFL IGLFM IESTFL IESTFM IPCSFL IPCSFM ISTFL ISTFM');
  NewLine;
end;

procedure TModflowSWT_Writer.WriteDataSet4;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrGeostaticStress);
  WriteArray(DataArray, 0, 'Data Set 4: GL0', StrNoValueAssigned, 'GL0');
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteDataSet5;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrSpecificGravityUns);
  WriteArray(DataArray, 0, 'Data Set 5: SGM', StrNoValueAssigned, 'SGM');
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteDataSet6;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrSpecificGravitySat);
  WriteArray(DataArray, 0, 'Data Set 6: SGS', StrNoValueAssigned, 'SGS');
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteDataSets7to13;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to FTHICK_List.Count - 1 do
  begin
    DataArray := FTHICK_List[Index];
    WriteArray(DataArray, 0, 'Data set 7: THICK', StrNoValueAssigned, 'THICK');
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    if FSwtPackage.CompressionSource = csSpecificStorage then
    begin
      DataArray := FSse_List[Index];
      WriteArray(DataArray, 0, 'Data set 8: Sse', StrNoValueAssigned, 'Sse');
      Model.DataArrayManager.AddDataSetToCache(DataArray);

      DataArray := FSsv_List[Index];
      WriteArray(DataArray, 0, 'Data set 9: Ssv', StrNoValueAssigned, 'Ssv');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
    end;

    if FSwtPackage.CompressionSource = csCompressionReComp then
    begin
      DataArray := FCr_List[Index];
      WriteArray(DataArray, 0, 'Data set 10: Cr', StrNoValueAssigned, 'Cr');
      Model.DataArrayManager.AddDataSetToCache(DataArray);

      DataArray := FCc_List[Index];
      WriteArray(DataArray, 0, 'Data set 11: Cc', StrNoValueAssigned, 'Cc');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
    end;

    DataArray := FVOID_List[Index];
    WriteArray(DataArray, 0, 'Data set 12: VOID', StrNoValueAssigned, 'VOID');
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    DataArray := FSUB_List[Index];
    WriteArray(DataArray, 0, 'Data set 13: SUB', StrNoValueAssigned, 'SUB');
    Model.DataArrayManager.AddDataSetToCache(DataArray);
  end;
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSWT_Writer.WriteFile(const AFileName: string);
begin
  FSwtPackage := Package as TSwtPackageSelection;
  if not FSwtPackage.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSWT) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoWatertableSubsi);

    FNameOfFile := FileName(AFileName);
    WriteToNameFile(StrSWT, Model.UnitNumbers.UnitNumber(StrSWT),
      FNameOfFile, foInput, Model);

    RetrieveArrays;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingSWTPackage);

      WriteDataSet0;

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

      frmProgressMM.AddMessage(StrWritingDataSets7to13);
      WriteDataSets7to13;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if FSwtPackage.PreconsolidationSource = pcOffsets then
      begin
        frmProgressMM.AddMessage(StrWritingDataSet14);
        WriteDataSet14;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;

      if FSwtPackage.PreconsolidationSource = pcSpecified then
      begin
        frmProgressMM.AddMessage(StrWritingDataSet15);
        WriteDataSet15;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;

      if FSwtPackage.PrintChoices.Count > 0 then
      begin
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
      end;

    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

end.
