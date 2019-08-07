unit Mt3dmsRctWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Forms, PhastModelUnit;

type
  TMt3dmsRctWriter = class(TCustomModflowWriter)
  private
    ISOTHM: Integer;
    IREACT: Integer;
    IGETSC: Integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2A;
    procedure WriteDataSet2B;
    procedure WriteDataSet2C;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmProgressUnit, ModflowUnitNumbers, DataSetUnit, SysUtils,
  Mt3dmsChemSpeciesUnit, GoPhastTypes;

resourcestring
  StrSInTheMT3DMSRCT = '%s in the MT3DMS or MT3D-USGS RCT package';
  StrRHOBLayerD = 'Data Set 2A: RHOB Layer: %d';
  StrPRSITY2LayerD = 'Data Set 2B: PRSITY2 Layer: %d';
  StrWritingMT3DMSRCTP = 'Writing MT3DMS or MT3D-USGS RCT Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
  StrWritingDataSet2A = '  Writing Data Set 2A.';
  StrWritingDataSet2B = '  Writing Data Set 2B.';
  StrWritingDataSet2C = '  Writing Data Set 2C.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';

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
const
  IREACTION = 0;
var
  ChemPkg: TMt3dmsChemReaction;
  IRCTOP: Integer;
begin
  ChemPkg := Model.ModflowPackages.Mt3dmsChemReact;

  ISOTHM := Ord(ChemPkg.SorptionChoice);
  case ChemPkg.KineticChoice of
    kcNone: IREACT := 0;
    kcFirstOrder: IREACT := 1;
    kcZeroOrder: IREACT := 100;
    else Assert(False);
  end;
  IRCTOP := 2;
  IGETSC := Ord(ChemPkg.OtherInitialConcChoice);
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
  if ISOTHM in [1,2,3,4,6] then
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
  if ISOTHM in [5,6] then
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

procedure TMt3dmsRctWriter.WriteDataSet3;
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
        WriteArray(DataArray, LayerIndex, Format('Data Set: 3: SP1: %0:s, Layer: %1:d',
          [Item.Name, Model.DataSetLayerToModflowLayer(LayerIndex)]), StrNoValueAssigned, 'SP1');
      end;
    end;
  end;
begin
  if ISOTHM > 0 then
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
  if ISOTHM > 0 then
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

procedure TMt3dmsRctWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Model.ModflowPackages.Mt3dmsChemReact.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);

  if Model.PackageGeneratedExternally(StrRCT) then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrRCT, Mt3dRct,
    NameOfFile, foInput, Model);
//  WriteToNameFile(StrBAS, Model.UnitNumbers.UnitNumber(StrBAS),
//    NameOfFile, foInput);

  // PackageGeneratedExternally needs to be updated for MT3DMS
  OpenFile(NameOfFile);
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

  finally
    CloseFile;
  end;
end;

end.
