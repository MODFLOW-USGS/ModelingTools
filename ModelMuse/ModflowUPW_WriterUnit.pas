unit ModflowUPW_WriterUnit;

interface

uses
  ModflowLPF_WriterUnit, ModflowPackageSelectionUnit;

type
  TModflowUPW_Writer = class(TCustomLpfWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet6;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    function ParameterDataSetNumber: integer; override;
    procedure WriteLayerData; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  Forms, frmErrorsAndWarningsUnit, ModflowUnitNumbers, CustomModflowWriterUnit,
  frmProgressUnit, LayerStructureUnit, GoPhastTypes;

resourcestring
  StrWritingUPWPackage = 'Writing UPW Package input.';
  StrTheNOPARCHECKOptUPW = 'The "NOPARCHECK" option in the UPW package is se' +
  'lected';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';

{ TModflowLPF_Writer }

class function TModflowUPW_Writer.Extension: string;
begin
  result := '.upw';
end;

function TModflowUPW_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.UpwPackage;
end;

function TModflowUPW_Writer.ParameterDataSetNumber: integer;
begin
  result := 7;
end;

procedure TModflowUPW_Writer.WriteDataSet1;
var
  IUPWCB: Integer;
  HDRY: Real;
  NPUPW: Integer;
  IPHDRY: integer;
  Options: string;
  LocalPackage: TUpwPackageSelection;
begin
  LocalPackage := Model.ModflowPackages.UpwPackage;
  IUPWCB := 0;
  GetFlowUnitNumber(IUPWCB);
  HDRY := Model.ModflowOptions.HDry;
  if Model.ModflowFullStressPeriods.TransientModel then
  begin
    NPUPW := Model.ModflowSteadyParameters.CountParameters(AllLpfParameters);
  end
  else
  begin
    NPUPW := Model.ModflowSteadyParameters.CountParameters(SteadyLpfParameters);
  end;
  NPLPF := NPUPW;
  IPHDRY := Ord(LocalPackage.HDryPrintOption);

  Options := '';
  if LocalPackage.NoParCheck then
  begin
    Options := Options + ' NOPARCHECK';
    frmErrorsAndWarnings.AddWarning(Model, StrTheNOPARCHECKOptUPW, StrTheNOPARCHECKOpt)
  end;

  WriteInteger(IUPWCB);
  WriteFloat(HDRY);
  WriteInteger(NPUPW);
  WriteInteger(IPHDRY);
  if Options <> '' then
  begin
    WriteString(Options)
  end;
  WriteString(' # Data Set 1, IUPWCB HDRY NPUPW IPHDRY');
  if Options <> '' then
  begin
    WriteString(' Options');
  end;
  NewLine;
end;

procedure TModflowUPW_Writer.WriteDataSet6;
var
  index: Integer;
begin
  for index := 0 to Model.ModflowLayerCount - 1 do
  begin
    WriteInteger(0);
  end;
  WriteString(' # LAYWET');
  NewLine;
end;

procedure TModflowUPW_Writer.WriteFile(const AFileName: string);
//var
//  NameOfFile: string;
begin
  if not Model.ModflowPackages.UpwPackage.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally(Model) then
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
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrParameterZonesNot);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheNOPARCHECKOptUPW);

    FNameOfFile := FileName(AFileName);
    WriteToNameFile(StrUPW, Model.UnitNumbers.UnitNumber(StrUPW),
      FNameOfFile, foInput, Model);
    FInputFileName := FNameOfFile;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingUPWPackage);
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

procedure TModflowUPW_Writer.WriteLayerData;
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
      // Data Set 9;
      WriteHK(Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 10
      WriteHANI(Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 11
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

      // Data set 12
      WriteSS(TransientModel, Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 13
      WriteSY(TransientModel, Group, ArrayIndex, MFLayerIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data set 14
      WriteVKCB(LayerIndex, MFLayerIndex, ArrayIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;
  end;
end;

end.
