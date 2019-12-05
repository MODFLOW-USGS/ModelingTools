unit ModflowBCF_WriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, PhastModelUnit,
  LayerStructureUnit, SysUtils, DataSetUnit;

type
  TModflowBCF_Writer = class(TCustomFlowPackageWriter)
  private
    FCheckingVCont: Boolean;
    FActiveDataArray: TDataArray;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSets4to9;
    procedure WriteDataSet4(Layer: Integer; TransientModel: Boolean;
      AquiferType: integer);
    procedure WriteDataSet5or6(AquiferType: Integer; Layer: Integer);
    procedure WriteDataSet7(LayerIndex: Integer);
    procedure WriteDataSet8(AquiferType: Integer; TransientModel: Boolean; Layer: Integer);
    procedure WriteDataSet9(AquiferType: Integer; Layer: Integer);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    function ShouldCheckCell(LayerIndex, RowIndex, ColIndex: Integer): Boolean; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, GoPhastTypes, 
  Forms, frmErrorsAndWarningsUnit;

resourcestring
  StrWritingDataForL = '  Writing data for layer %d.';
  StrWritingBCFPackage = 'Writing BCF Package input.';
  StrLargeContrastInTr = 'Large contrast in transmissivity (may cause numerical problems)';
  StrLargeContrastInHy = 'Large contrast in hydraulic conductivity (may caus' +
  'e numerical problems)';
  StrZeroTrans = 'Transmissivity equals zero.';
  StrZeroK = 'Hydraulic conductivity equals zero.';
  StrNegativeOrZeroVer = 'Negative or zero vertical conductance';
  StrNegativeOrZeroSpe = 'Negative or zero specific yield';
  StrNegativeOrZeroSpeSto = 'Negative or zero specific storage';

{ TModflowBCF_Writer }

class function TModflowBCF_Writer.Extension: string;
begin
  result := '.bcf';
end;

procedure TModflowBCF_Writer.WriteDataSet5or6(AquiferType: Integer;
  Layer: Integer);
var
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := Model.DataArrayManager;
  case AquiferType of
    0, 2:
      begin
        // Data set 5
        DataArray := DataArrayManager.GetDataSetByName(StrTransmissivity);
        Assert(DataArray <> nil);
        WriteArray(DataArray, Layer, 'Tran', StrNoValueAssigned, 'Tran');
        CheckArray(DataArray, Layer, StrLargeContrastInTr,
          cvmGradient, 1e6, etWarning);
        CheckArray(DataArray, Layer, StrZeroTrans,
          cvmGreater, 0, etWarning);
      end;
    1, 3:
      begin
        // Data set 6
        DataArray := DataArrayManager.GetDataSetByName(rsKx);
        Assert(DataArray <> nil);
        WriteArray(DataArray, Layer, 'HY', StrNoValueAssigned, 'HY');
        CheckArray(DataArray, Layer, StrLargeContrastInHy,
          cvmGradient, 1e6, etWarning);
        CheckArray(DataArray, Layer, StrZeroK,
          cvmGreater, 0, etWarning);
      end;
  end;
end;

function TModflowBCF_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.BcfPackage;
end;

function TModflowBCF_Writer.ShouldCheckCell(LayerIndex, RowIndex,
  ColIndex: Integer): Boolean;
begin
  if FCheckingVCont then
  begin
    result := FActiveDataArray.BooleanData[LayerIndex+1, RowIndex, ColIndex];
  end
  else
  begin
    result := inherited;
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet1;
var
  IBCFCB: Integer;
  HDRY: Real;
  IWDFLG: integer;
  WETFCT: Real;
  IWETIT: Integer;
  IHDWET: Integer;
begin
  IBCFCB := 0;
  GetFlowUnitNumber(IBCFCB);
  HDRY := Model.ModflowOptions.HDry;
  IWDFLG := Ord(Model.ModflowWettingOptions.WettingActive);
  WETFCT := Model.ModflowWettingOptions.WettingFactor;
  IWETIT := Model.ModflowWettingOptions.WettingIterations;
  IHDWET := Model.ModflowWettingOptions.WettingEquation;
  WriteInteger(IBCFCB);
  WriteFloat(HDRY);
  WriteInteger(IWDFLG);
  WriteFloat(WETFCT);
  WriteInteger(IWETIT);
  WriteInteger(IHDWET);
  WriteString(' # IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSet2;
var
  Ltype: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  Ltype := Model.Laytyp;
  for LayerIndex := 0 to Length(Ltype) - 1 do
  begin
    WriteInteger(Ltype[LayerIndex]);
  end;
  WriteString(' # Ltype');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSet3;
var
  LayerIndex: Integer;
  Trpy: TOneDRealArray;
begin
  WriteU2DRELHeader('TRPY', matStructured, 'TRPY');
  Trpy := Model.Trpy;
  for LayerIndex := 0 to Length(Trpy) - 1 do
  begin
    WriteFloat(Trpy[LayerIndex]);
  end;
  WriteString(' # TRPY');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSets4to9;
var
  Group: TLayerGroup;
  LayerIndex: Integer;
  TransientModel: Boolean;
  AquiferType: Integer;
begin
  TransientModel := Model.ModflowStressPeriods.TransientModel;
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
        frmProgressMM.AddMessage(Format(StrWritingDataForL,
          [LayerIndex+1]));
        Group := Model.GetLayerGroupByLayer(LayerIndex);

        AquiferType := Group.AquiferType;
        if (AquiferType = 1) and (LayerIndex > 0) then
        begin
          AquiferType := 3;
        end;
        WriteDataSet4(LayerIndex, TransientModel, AquiferType);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet5or6(AquiferType, LayerIndex);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet7(LayerIndex);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet8(AquiferType, TransientModel, LayerIndex);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet9(AquiferType, LayerIndex);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
    end;
  end;
end;

procedure TModflowBCF_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
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
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeContrastInTr);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrZeroTrans);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrLargeContrastInHy);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrZeroK);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNegativeOrZeroSpe);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNegativeOrZeroVer);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNegativeOrZeroSpeSto);

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrBCF, Model.UnitNumbers.UnitNumber(StrBCF),
    NameOfFile, foInput, Model);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingBCFPackage);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets4to9;
  finally
    CloseFile;
  end;

end;

procedure TModflowBCF_Writer.WriteDataSet9(AquiferType: Integer;
  Layer: Integer);
var
  DataArray: TDataArray;
begin
  if Model.ModflowWettingOptions.WettingActive
    and (AquiferType in [1, 3]) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsWetDry);
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'WETDRY', StrNoValueAssigned, 'WETDRY');
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet8(AquiferType: Integer;
  TransientModel: Boolean; Layer: Integer);
var
  DataArray: TDataArray;
begin
  if TransientModel and (AquiferType in [2, 3]) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsSpecificYield);
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'Sf2', StrNoValueAssigned, 'Sf2');
    CheckArray(DataArray, Layer, StrNegativeOrZeroSpe,
      cvmGreater, 0, etWarning);
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet7(LayerIndex: Integer);
var
  DataArray: TDataArray;
begin
  if LayerIndex < Model.LayerCount-1 then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(StrVerticalConductance);
    FActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);

    Assert(DataArray <> nil);
    WriteArray(DataArray, LayerIndex, 'Vcont', StrNoValueAssigned, 'Vcont');
    FCheckingVCont := True;
    try
      CheckArray(DataArray, LayerIndex, StrNegativeOrZeroVer,
        cvmGreater, 0, etWarning);
    finally
      FCheckingVCont := False;
    end;
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet4(Layer: Integer;
  TransientModel: Boolean; AquiferType: integer);
var
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
  ErrorMessage: string;
begin
  if TransientModel then
  begin
    DataArrayManager := Model.DataArrayManager;
    DataArray := nil;
    case AquiferType of
      0, 2, 3:
        begin
          // confined storage coeficient
          DataArray := DataArrayManager.GetDataSetByName(StrConfinedStorageCoe);
          ErrorMessage := StrNegativeOrZeroSpeSto;
        end;
      1:
        begin
          // specific yield
          DataArray := DataArrayManager.GetDataSetByName(rsSpecificYield);
          ErrorMessage := StrNegativeOrZeroSpe;
        end;
    else
      Assert(False);
    end;
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'Sf1', StrNoValueAssigned, 'Sf1');
    CheckArray(DataArray, Layer, ErrorMessage,
      cvmGreater, 0, etWarning);
  end;
end;

end.
