unit ModflowNPF_WriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Vcl.Forms, DataSetUnit;

type
  TNpfWriter = class(TCustomFlowPackageWriter)
  private
    FNpfPackage: TNpfPackage;
    procedure WriteDataSet1;
    procedure WriteIcelltype;
    procedure WriteHK;
    procedure WriteVK;
    procedure WriteWETDRY;
    procedure WriteHANI;
    procedure WriteAngle1;
    procedure WriteAngle2;
    procedure WriteAngle3;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, ModflowUnitNumbers, frmProgressUnit, GoPhastTypes,
  ModflowOptionsUnit, ModflowOutputControlUnit, PhastModelUnit,
  System.SysUtils, PlProcUnit, PestParamRoots;

resourcestring
  StrWritingNPFPackage = 'Writing NPF Package input.';

{ TNpfWriter }

class function TNpfWriter.Extension: string;
begin
  Result := '.npf';
end;

function TNpfWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.NpfPackage;
end;

procedure TNpfWriter.WriteAngle1;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing XT3D angle1');
  DataArray := Model.DataArrayManager.GetDataSetByName(KXT3DAngle1);
  WriteMf6_DataSet(DataArray, 'angle1');
  WritePestZones(DataArray, FInputFileName, NPF_Angle1);
end;

procedure TNpfWriter.WriteAngle2;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing XT3D angle2');
  DataArray := Model.DataArrayManager.GetDataSetByName(KXT3DAngle2);
  WriteMf6_DataSet(DataArray, 'angle2');
  WritePestZones(DataArray, FInputFileName, NPF_Angle2);
end;

procedure TNpfWriter.WriteAngle3;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing XT3D angle3');
  DataArray := Model.DataArrayManager.GetDataSetByName(KXT3DAngle3);
  WriteMf6_DataSet(DataArray, 'angle3');
  WritePestZones(DataArray, FInputFileName, NPF_Angle3);
end;

procedure TNpfWriter.WriteDataSet1;
var
//  HNOFlO: Real;
//  HDRY: Real;
  NpfPackage: TNpfPackage;
  Wetting: TWettingOptions;
  WETFCT: Real;
  IWETIT: Integer;
  IHDWET: Integer;
  ModflowOptions: TModflowOptions;
//  ModflowWettingOptions: TWettingOptions
begin
  ModflowOptions := Model.ModflowOptions;
//  ModflowWettingOptions := Model.ModflowWettingOptions;
  WriteBeginOptions;

  WriteSaveFlowsOption;

//  HNOFlO := Model.ModflowOptions.HNoFlow;
//  WriteString('  HNOFLO ');
//  WriteFloat(HNOFlO);
//  NewLine;
//
//  HDRY := Model.ModflowOptions.HDry;
//  WriteString('  HDRY ');
//  WriteFloat(HDRY);
//  NewLine;

  NpfPackage := Model.ModflowPackages.NpfPackage;

  if (NpfPackage.CellAveraging <> caHarmonic) and not NpfPackage.UseXT3D then
  begin
    case NpfPackage.CellAveraging of
      caHarmonic: ; // do nothing
      caLogarithmic: WriteString('  ALTERNATIVE_CELL_AVERAGING LOGARITHMIC');
      caArithLog: WriteString('  ALTERNATIVE_CELL_AVERAGING AMT-LMK');
      caArithHarm: WriteString('  ALTERNATIVE_CELL_AVERAGING AMT-HMK');
      else Assert(False);
    end;
    NewLine;
  end;

  if NpfPackage.UseSaturatedThickness and not NpfPackage.UseXT3D then
  begin
    WriteString('  THICKSTRT');
    NewLine;
  end;

  if NpfPackage.TimeVaryingVerticalConductance and not NpfPackage.UseXT3D
    and not ModflowOptions.NewtonMF6 then
  begin
    WriteString('  VARIABLECV');
    if NpfPackage.Dewatered then
    begin
      WriteString(' DEWATERED');
    end;
    NewLine;
  end;

  if NpfPackage.Perched and not NpfPackage.UseXT3D
    and not ModflowOptions.NewtonMF6 then
  begin
    WriteString('  PERCHED');
    NewLine;
  end;

  Wetting := Model.ModflowWettingOptions;
  if Wetting.WettingActive and not ModflowOptions.NewtonMF6 then
  begin
    WriteString('  REWET');
//    NewLine;

    WETFCT := Wetting.WettingFactor;
    WriteString('  WETFCT ');
    WriteFloat(WETFCT);
//    NewLine;

    IWETIT := Wetting.WettingIterations;
    WriteString('  IWETIT ');
    WriteInteger(IWETIT);
//    NewLine;

    IHDWET := Wetting.WettingEquation;
    WriteString('  IHDWET ');
    WriteInteger(IHDWET);
    NewLine;
  end;

//  WriteNoNewtown;

  if NpfPackage.UseXT3D then
  begin
    WriteString('  XT3D');
    if NpfPackage.Xt3dOnRightHandSide then
    begin
      WriteString(' RHS');
    end;
    NewLine;
  end;

  if NpfPackage.SaveSpecificDischarge then
  begin
    WriteString('  SAVE_SPECIFIC_DISCHARGE');
    NewLine;
  end;

  if FNpfPackage.UseHorizontalAnisotropy then
  begin
    WriteString('  K22OVERK');
    NewLine;
  end;

  if FNpfPackage.UseVerticalAnisotropy then
  begin
    WriteString('  K33OVERK');
    NewLine;
  end;
  //  if not NpfPackage.UseNewtonRaphson then
//  begin
//    WriteString('  NO_NEWTON');
//    NewLine;
//  end;

//  if NpfPackage.ApplyHeadDampening then
//  begin
//    WriteString('  NEWTON_HEAD_DAMPENING');
//    NewLine;
//  end;

  WriteEndOptions;
end;

procedure TNpfWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  FNpfPackage := Model.ModflowPackages.NpfPackage;
  if not FNpfPackage.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    CheckSpecifiedHeadsConnected;
    NameOfFile := FileName(AFileName);
    FInputFileName := NameOfFile;
    WriteToNameFile(StrNPF, -1, NameOfFile, foInput, Model);
    OpenFile(NameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingNPFPackage);
      frmProgressMM.AddMessage(StrWritingDataSet0);
      WriteDataSet0;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingOptions);
      WriteDataSet1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      NewLine;
      WriteBeginGridData;

      WriteIcelltype;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteHK;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteVK;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;


      WriteWETDRY;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteHANI;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteAngle1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteAngle2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteAngle3;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteEndGridData;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TNpfWriter.WriteHANI;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing K22');
  if FNpfPackage.UseHorizontalAnisotropy then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KKyOverKx);
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKy);
  end;
  WriteMf6_DataSet(DataArray, 'K22');
  WritePestZones(DataArray, FInputFileName, NPF_K22);
end;

procedure TNpfWriter.WriteHK;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing K');
  DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
  WriteMf6_DataSet(DataArray, 'K');
  WritePestZones(DataArray, FInputFileName, NPF_K);
end;

procedure TNpfWriter.WriteIcelltype;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing ICELLTYPE');
  DataArray := Model.DataArrayManager.GetDataSetByName(KCellType);
  WriteMf6_DataSet(DataArray, 'ICELLTYPE');
end;

procedure TNpfWriter.WriteVK;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing K33');
  if FNpfPackage.UseVerticalAnisotropy then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KKzOverKx);
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKz);
  end;
  WriteMf6_DataSet(DataArray, 'K33');
  WritePestZones(DataArray, FInputFileName, NPF_K33);
end;

procedure TNpfWriter.WriteWETDRY;
var
  DataArray: TDataArray;
begin
  if Model.ModflowWettingOptions.WettingActive then
  begin
    frmProgressMM.AddMessage('  Writing WETDRY');
    DataArray := Model.DataArrayManager.GetDataSetByName(rsWetDry);
    WriteMf6_DataSet(DataArray, 'WETDRY');
    WritePestZones(DataArray, FInputFileName, NPF_WETDRY);
  end;
end;

end.
