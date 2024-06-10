unit Mt3dmsRctWriterUnit;

interface

uses
  System.Classes, CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms,
  PhastModelUnit;

type
  TMt3dmsRctWriter = class(TCustomModflowWriter)
  private
    ISOTHM: Integer;
    IREACT: Integer;
    IGETSC: Integer;
    IREACTION: Integer;
    Frec_FileName: string;
    procedure WriteDataSet1;
    procedure WriteDataSet2A;
    procedure WriteDataSet2B;
    procedure WriteDataSet2C;
    procedure WriteDataSet3A;
    procedure WriteDataSet3B;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSet9a;
    procedure WriteDataSet9b;
    procedure WriteRecDataSet1;
    procedure WriteRecDataSet2;
    procedure WriteRecDataSet3;
    procedure WriteRecDataSet4;
    procedure WriteRecDataSet5;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, ModflowUnitNumbers, DataSetUnit, SysUtils, RbwParser,
  Mt3dmsChemSpeciesUnit, GoPhastTypes, frmErrorsAndWarningsUnit,
  DataSetNamesUnit;

resourcestring
  StrSInTheMT3DMSRCT = '%s in the MT3DMS or MT3D-USGS RCT package';
  StrRHOBLayerD = 'Data Set 2A: RHOB Layer: %d';
  StrPRSITY2LayerD = 'Data Set 2B: PRSITY2 Layer: %d';
  StrWritingMT3DMSRCTP = 'Writing MT3DMS or MT3D-USGS RCT Package input.';
  StrWritingDataSet2A = '  Writing Data Set 2A.';
  StrWritingDataSet2B = '  Writing Data Set 2B.';
  StrWritingDataSet2C = '  Writing Data Set 2C.';
  StrMonodAndFirstorde = 'Monod and first-order chain reactions are only sim' +
  'ulated in MT3D-USGS';
  StrInvalidIREACTInMT = 'Invalid IREACT in MT3D';
  StrWritingDataSet3a = '  Writing Data Set 3a.';
  StrWritingDataSet3b = '  Writing Data Set 3b.';

{ TMt3dmsRctWriter }

constructor TMt3dmsRctWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfMt3dms;
end;

class function TMt3dmsRctWriter.Extension: string;
begin
  result := '.rct';
end;

procedure TMt3dmsRctWriter.WriteDataSet1;
var
  ChemPkg: TMt3dmsChemReaction;
  IRCTOP: Integer;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;

  if ChemPkg.SorptionChoice = scDualWithDifferingConstants then
  begin
    ISOTHM := -6;
  end
  else
  begin
    ISOTHM := Ord(ChemPkg.SorptionChoice);
  end;

  case ChemPkg.KineticChoice of
    kcNone: IREACT := 0;
    kcFirstOrder: IREACT := 1;
    kcMonod: IREACT := 2;
    kcFirstOrderChain: IREACT := 3;
    kcZeroOrder: IREACT := 100;
    else Assert(False);
  end;
  if (IREACT in [2,3]) and (Model.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUsgs) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidIREACTInMT, StrMonodAndFirstorde);
  end;
  IRCTOP := 2;
  IGETSC := Ord(ChemPkg.OtherInitialConcChoice);
  IREACTION := Ord(ChemPkg.ReactionChoice);
  WriteI10Integer(ISOTHM, Format(StrSInTheMT3DMSRCT, ['ISOTHM']));
  WriteI10Integer(IREACT, Format(StrSInTheMT3DMSRCT, ['IREACT']));
  WriteI10Integer(IRCTOP, Format(StrSInTheMT3DMSRCT, ['IRCTOP']));
  WriteI10Integer(IGETSC, Format(StrSInTheMT3DMSRCT, ['IGETSC']));
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUsgs then
  begin
    WriteI10Integer(IREACTION, Format(StrSInTheMT3DMSRCT, ['IREACTION']));
  end;
  WriteString(' # Data Set 1: ISOTHM IREACT IRCTOP IGETSC');
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUsgs then
  begin
    WriteString(' IREACTION');
  end;
  NewLine;
end;

procedure TMt3dmsRctWriter.WriteDataSet2A;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  if (ISOTHM in [1,2,3,4,6]) or (ISOTHM = -6) or (IREACTION = 2) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsBulkDensity);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format(StrRHOBLayerD,
          [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'RHOB');
      end;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet2B;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  if (ISOTHM in [5,6]) or (ISOTHM = -6) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsImmobPorosity);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format(StrPRSITY2LayerD,
          [Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'PRSITY2');
      end;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet2C;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteSorbedInitialConc;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.SorbOrImmobInitialConcDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 2C: SRCONC: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'SRCONC');
      end;
    end;
  end;
begin
  if IGETSC > 0 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteSorbedInitialConc;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteSorbedInitialConc;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet3A;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteSP1;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.FirstSorbParamDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 3a: SP1: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'SP1');
      end;
    end;
  end;
begin
  if ISOTHM <> 0 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteSP1;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteSP1;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet3B;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteSP1IM;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.ImmobilePartioningCoefficientDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 3b: SP1IM: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'SP1');
      end;
    end;
  end;
begin
  if ISOTHM = -6 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteSP1IM;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteSP1IM;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet4;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteSP2;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.SecondSorbParamDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 4: SP2: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'SP2');
      end;
    end;
  end;
begin
  if ISOTHM <> 0 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteSP2;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteSP2;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet5;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteRC1;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.ReactionRateDisolvedDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 5: RC1: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'RC1');
      end;
    end;
  end;
begin
  if IREACT > 0 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteRC1;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteRC1;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet6;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteRC2;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.ReactionRateSorbedDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 6: RC2: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'RC2');
      end;
    end;
  end;
begin
  if IREACT > 0 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteRC2;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteRC2;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet7;
var
  SpeciesIndex: Integer;
  Item: TChemSpeciesItem;
  procedure WriteRC3;
  var
    LayerIndex: Integer;
    DataArray: TDataArray;
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(
      Item.HalfSaturationConstantDataArrayName);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, Format('Data Set: 7: RC3: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'RC3');
      end;
    end;
  end;
begin
  if IREACT = 2 then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      Item := Model.MobileComponents[SpeciesIndex];
      WriteRC3;
    end;
    for SpeciesIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      Item := Model.ImmobileComponents[SpeciesIndex];
      WriteRC3;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet8;
var
  ChemPkg: TMt3dmsChemReaction;
  YieldCoefficients: TStringList;
  Compiler: TRbwParser;
  ItemIndex: Integer;
  CurrentExpression: TExpression;
  Formula: string;
  Value: Double;
begin
  if IREACT = 3 then
  begin
    ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
    YieldCoefficients := ChemPkg.YieldCoefficients;
    Compiler := Model.GetCompiler(dso3D, eaBlocks);
    for ItemIndex := 0 to YieldCoefficients.Count - 1 do
    begin
      Formula := YieldCoefficients[ItemIndex];
      try
        Compiler.Compile(Formula);
      except on ERbwParserError do
        begin
          Formula := '0.0';
          Compiler.Compile(Formula);
        end;
      end;
      CurrentExpression := Compiler.CurrentExpression;
      if not (CurrentExpression.ResultType in [rdtDouble, rdtInteger]) then
      begin
        Formula := '0.0';
        Compiler.Compile(Formula);
        CurrentExpression := Compiler.CurrentExpression;
      end;
      CurrentExpression.Evaluate;
      Value := CurrentExpression.DoubleResult;
      WriteF10Float(Value);
      NewLine;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet9a;
var
  ChemPkg: TMt3dmsChemReaction;
begin
  if IREACTION = 1 then
  begin
    ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
    WriteI10Integer(ChemPkg.ElectronDonor + 1, 'IED in RCT');
    WriteI10Integer(ChemPkg.ElectronAcceptor + 1, 'IEA in RCT');
    WriteF10Float(ChemPkg.StochiometricRatio);
    WriteString(' # IED, IEA, F');
    NewLine;
  end;
end;

procedure TMt3dmsRctWriter.WriteDataSet9b;
begin
  if IREACTION = 2 then
  begin
    Frec_FileName := ChangeFileExt(FInputFileName, '.Kinetic');
    WriteString(ExtractFileName(Frec_FileName));
  end;
end;

procedure TMt3dmsRctWriter.WriteFile(const AFileName: string);
//var
//  NameOfFile: string;
begin
  if not Model.ModflowPackages.Mt3dmsChemReact.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidIREACTInMT);

  if Model.PackageGeneratedExternally(StrRCT) then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrRCT, Mt3dRct,
    FNameOfFile, foInput, Model);
//  WriteToNameFile(StrBAS, Model.UnitNumbers.UnitNumber(StrBAS),
//    FNameOfFile, foInput);

  // PackageGeneratedExternally needs to be updated for MT3DMS
  FInputFileName := FNameOfFile;
  OpenFile(FInputFileName);
  try
    frmProgressMM.AddMessage(StrWritingMT3DMSRCTP);

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2A);
    WriteDataSet2A;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2B);
    WriteDataSet2B;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2C);
    WriteDataSet2C;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet3a);
    WriteDataSet3A;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet3b);
    WriteDataSet3b;
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

    frmProgressMM.AddMessage(StrWritingDataSet8);
    WriteDataSet8;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 9a');
    WriteDataSet9a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 9a');
    WriteDataSet9b;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;

  if IREACTION = 2 then
  begin
    FInputFileName := Frec_FileName;
    OpenFile(FInputFileName);
    try
      frmProgressMM.AddMessage('  rec_File');
//      WriteDataSet0;

      WriteRecDataSet1;
      WriteRecDataSet2;
      WriteRecDataSet3;
      WriteRecDataSet4;
      WriteRecDataSet5;
    finally
      CloseFile;
    end;
  end;
end;

procedure TMt3dmsRctWriter.WriteRecDataSet1;
var
  ChemPkg: TMt3dmsChemReaction;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
  WriteInteger(ChemPkg.ElectronDonorCount);
  WriteInteger(ChemPkg.ElectronAcceptorCount);
  WriteInteger(ChemPkg.SpecialCases.Count);
  WriteInteger(Ord(ChemPkg.SolidFE));
  WriteString(' # NED, NEA, NSPECIAL, IFESLD');
  NewLine;
end;

procedure TMt3dmsRctWriter.WriteRecDataSet2;
var
  ChemPkg: TMt3dmsChemReaction;
  Index: Integer;
  SpecialCase: TRctSpecialCase;
  ISPEC: Integer;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
  for Index := 0 to ChemPkg.SpecialCases.Count - 1 do
  begin
    SpecialCase := ChemPkg.SpecialCases[Index];
    ISPEC := Model.MobileComponents.IndexOfName(SpecialCase.Species);
    if ISPEC >= 0 then
    begin
      Inc(ISPEC);
    end
    else
    begin
      ISPEC := Model.ImmobileComponents.IndexOfName(SpecialCase.Species);
      if ISPEC >= 0 then
      begin
        ISPEC := ISPEC + Model.MobileComponents.Count + 1;
      end;
    end;
    WriteInteger(ISPEC);
    case SpecialCase.Treatment of
      stSolid: WriteString(' SOLID');
      stMaxEC: WriteString(' MAXEC');
      stStore: WriteString(' STORE');
      else Assert(False);
    end;
    WriteFloat(SpecialCase.EFCMAX);
    WriteString(' # ISPEC, SPECIAL(ISPEC), EFCMAX for ');
    WriteString(SpecialCase.Species);
    NewLine;
  end;
end;

procedure TMt3dmsRctWriter.WriteRecDataSet3;
var
  ChemPkg: TMt3dmsChemReaction;
  Index: Integer;
  EAProp: TEAProperties;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
  for Index := 0 to ChemPkg.EAProperties.Count - 1 do
  begin
    EAProp := ChemPkg.EAProperties[Index];
    WriteFloat(EAProp.HalfSaturation);
    WriteFloat(EAProp.InhibitionConstant);
    WriteString(' # HSC, IC for ');
    WriteString(EAProp.Species);
    NewLine;
  end;
end;

procedure TMt3dmsRctWriter.WriteRecDataSet4;
var
  ChemPkg: TMt3dmsChemReaction;
  Index: Integer;
  Decay: TSpeciesAssociatedValue;
  ValueIndex: Integer;
  AValue: Double;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
  for Index := 0 to ChemPkg.DecayRates.Count - 1 do
  begin
    Decay := ChemPkg.DecayRates[Index];
    for ValueIndex := 0 to Decay.Values.Count - 1 do
    begin
      AValue := Decay.Values[ValueIndex].Value;
      WriteFloat(AValue);
    end;
    WriteString(' # DECAYRATE(1:NED) for ');
    WriteString(Decay.Species);
    NewLine;
  end;
end;

procedure TMt3dmsRctWriter.WriteRecDataSet5;
var
  ChemPkg: TMt3dmsChemReaction;
  Index: Integer;
  Yield: TSpeciesAssociatedValue;
  ValueIndex: Integer;
  AValue: Double;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;
  for Index := 0 to ChemPkg.Yields.Count - 1 do
  begin
    Yield := ChemPkg.Yields[Index];
    for ValueIndex := 0 to Yield.Values.Count - 1 do
    begin
      AValue := Yield.Values[ValueIndex].Value;
      WriteFloat(AValue);
    end;
    WriteString(' # YIELDC(1:NED) for ');
    WriteString(Yield.Species);
    NewLine;
  end;
end;

end.
