unit ModflowStoWriterUnit;

interface

uses
  System.Classes, CustomModflowWriterUnit, ModflowPackageSelectionUnit, Vcl.Forms;

type
  TStoPackageWriter = class(TCustomPackageWriter)
  private
    procedure WriteOptions;
    procedure WriteICONVERT;
    procedure WriteSS;
    procedure WriteSY;
    procedure WriteStressPeriods;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowOutputControlUnit, GoPhastTypes, frmProgressUnit, DataSetUnit,
  PhastModelUnit, ModflowTimeUnit;

{ TStoPackageWriter }

class function TStoPackageWriter.Extension: string;
begin
  result := '.sto';
end;

function TStoPackageWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.StoPackage;
end;

procedure TStoPackageWriter.WriteFile(const AFileName: string);
var
  FTYPE: string;
  NameOfFile: string;
begin
  if not Model.ModflowPackages.StoPackage.IsSelected then
  begin
    Exit
  end;
  FTYPE := 'STO6';
  if Model.PackageGeneratedExternally(FTYPE) then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  FInputFileName := NameOfFile;
  WriteToNameFile(FTYPE, -1, NameOfFile, foInput, Model);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage('Writing STO package input');
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteBeginGridData;

    WriteICONVERT;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteSS;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteSY;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteEndGridData;

    WriteStressPeriods;


  finally
    CloseFile;
  end;

end;

procedure TStoPackageWriter.WriteICONVERT;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing ICONVERT');
  DataArray := Model.DataArrayManager.GetDataSetByName(KConvertible);
  WriteMf6_DataSet(DataArray, 'ICONVERT');
end;

procedure TStoPackageWriter.WriteOptions;
var
  StoPackage: TStoPackage;
begin
  WriteBeginOptions;
  WriteSaveFlowsOption;

  StoPackage := Model.ModflowPackages.StoPackage;
  if StoPackage.StorageChoice = scStorageCoefficient then
  begin
    WriteString('  STORAGECOEFFICIENT');
    NewLine;
  end;

//  WriteNoNewtown;
//  if StoPackage.NewtonFormulation = nfOff then
//  begin
//    WriteString('  NO_NEWTON');
//    NewLine;
//  end;

  WriteEndOptions;
end;

procedure TStoPackageWriter.WriteSS;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing SS');
  if not Model.ModflowPackages.CSubPackage.IsSelected then
  begin
    DataArray:= nil;
    case (Package as TStoPackage).StorageChoice of
      scSpecificStorage:
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(rsSpecific_Storage);
        end;
      scStorageCoefficient:
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(StrConfinedStorageCoe);
        end;
      else
        Assert(False);
    end;
    Assert(DataArray <> nil);
    WriteMf6_DataSet(DataArray, 'SS');
  end
  else
  begin
    WriteConstantU2DREL('', 0, matUnstructured, 'SS');
  end;
end;

procedure TStoPackageWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  PriorState: TStressPeriodType;
begin
  PriorState := sptSteadyState;
  for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
    if (TimeIndex = 0) or (PriorState <> StressPeriod.StressPeriodType) then
    begin
      WriteBeginPeriod(TimeIndex);
//      WriteString('BEGIN PERIOD ');
//      WriteInteger(TimeIndex+1);
//      NewLine;

      case StressPeriod.StressPeriodType of
        sptSteadyState: WriteString('  STEADY-STATE');
        sptTransient: WriteString('  TRANSIENT');
        else
          Assert(False);
      end;
      NewLine;

      WriteEndPeriod;
//      WriteString('END PERIOD');
//      NewLine;
//      NewLine;
    end;
    PriorState := StressPeriod.StressPeriodType;
  end;
end;

procedure TStoPackageWriter.WriteSY;
var
  DataArray: TDataArray;
begin
  frmProgressMM.AddMessage('  Writing SY');
  DataArray := Model.DataArrayManager.GetDataSetByName(rsSpecificYield);
  WriteMf6_DataSet(DataArray, 'SY');
end;

end.
