unit ModflowLVDA_WriterUnit;

interface

uses Types, SysUtils, Classes, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowLVDA_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    procedure CheckLvdaParameters;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
    function PackageID_Comment: string; reintroduce;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  PhastModelUnit, OrderedCollectionUnit, ModflowParameterUnit, frmProgressUnit, 
  GoPhastTypes, frmErrorsAndWarningsUnit, ModflowUnitNumbers, Forms;

resourcestring
  StrParameter0NonSim = 'Parameter %0:s is not applied to any cells.  Check ' +
  'that %1:s is set to "True" in at least one non-simulated unit.';
  StrParameter0Simulated = 'Parameter %0:s is not applied to any cells.  Che' +
  'ck that %1:s is set to "True" in at least one simulated unit.';
  StrParameterZonesNot = 'Parameter zones not defined.';
  StrWritingDataSet8 = '  Writing Data Set 8 for parameter: %s';
  StrWritingDataSet9 = '  Writing Data Set 9 for parameter: %s';
  StrWritingLVDAPackage = 'Writing LVDA Package input.';
  StrTheFollowingLPFLV = 'The following LPF LVDA parameters are applied to a' +
  'll cells because no zones are defined for them.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSets2and3 = '  Writing Data Sets 2 and 3.';

{ TModflowLVDA_Writer }

procedure TModflowLVDA_Writer.CheckLvdaParameters;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
begin
  if Model.ModflowSteadyParameters.CountParameters([ptHUF_LVDA]) > 1 then
  begin
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      Param := Model.ModflowSteadyParameters.Items[ParamIndex];
      if (Param.ParameterType = ptHUF_LVDA)
        and not Param.UseZone then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingLPFLV,
          Param.ParameterName);
      end;
    end;
  end;
end;

class function TModflowLVDA_Writer.Extension: string;
begin
  result := '.lvda';
end;

function TModflowLVDA_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.HufPackage;
end;

function TModflowLVDA_Writer.PackageID_Comment: string;
begin
  result := 'LVDA file created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName + ' version '
    + IModelVersion + '.';
end;

procedure TModflowLVDA_Writer.WriteDataSet0;
begin
  WriteCommentLine(PackageID_Comment);
end;

procedure TModflowLVDA_Writer.WriteDataSet1;
var
  NPLVDA: Integer;
begin
  NPLVDA := Model.ModflowSteadyParameters.CountParameters([ptHUF_LVDA]);
  WriteInteger(NPLVDA);
  WriteString(' # Data Set 1: NPLVDA');
  NewLine;
end;

procedure TModflowLVDA_Writer.WriteDataSets2and3;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  PARNAM: string;
  PARTYP: string;
  PARVAL: Double;
  LayerCount: Integer;
  NCLU: Integer;
  Clusters: TOneDIntegerArray;
  ClusterIndex: Integer;
  LAYER: Integer;
  UniformLayers: TBooleanDynArray;
  ZONARR: string;
  MLTARR: string;
  Error: string;
const
  IZ = 1;
begin
  LayerCount := Model.ModflowLayerCount;
  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    Param := Model.ModflowSteadyParameters.Items[ParamIndex];
    if Param.ParameterType = ptHUF_LVDA then
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      Param.ClearArrayNames;
      PARNAM := Param.ParameterName;
      PARTYP := ' LVDA';
      PARVAL := Param.Value;

      if not Param.UseZone then
      begin
        NCLU := Model.ModflowLayerCount;
        SetLength(Clusters, 0);
      end
      else
      begin
        IdentifyZoneClusters(NCLU, Clusters, UniformLayers, LayerCount, Param);
        if NCLU = 0 then
        begin
//          Error := 'Parameter ' + Param.ParameterName
//            + ' is not applied to any cells.  Check that '
//            + Param.ZoneName + ' is set to "True" in at least one ';
          if Param.ParameterType = ptLPF_VKCB then
          begin
            Error := Format(StrParameter0NonSim,
              [Param.ParameterName, Param.ZoneName]);
//              + ' is not applied to any cells.  Check that '
//              + Param.ZoneName + ' is set to "True" in at least one ';
//            Error := Error + 'non-simulated unit.';
//            Error := Error + 'non-simulated unit.';
          end
          else
          begin
            Error := Format(StrParameter0Simulated,
              [Param.ParameterName, Param.ZoneName]);
//            Error := 'Parameter ' + Param.ParameterName
//              + ' is not applied to any cells.  Check that '
//              + Param.ZoneName + ' is set to "True" in at least one ';
//            Error := Error + 'non-simulated unit.';
//            Error := Error + 'non-simulated unit.';
          end;
          frmErrorsAndWarnings.AddError(Model, StrParameterZonesNot,
            Error);
        end;
      end;

      // Data set 8
      frmProgressMM.AddMessage(Format(StrWritingDataSet8, [PARNAM]));
      WriteString(PARNAM);
      WriteString(PARTYP);
      WriteFloat(PARVAL);
      WriteInteger(NCLU);
      WriteString(' # PARNAM, PARTYP, PARVAL, NCLU');
      NewLine;

      Model.WritePValAndTemplate(PARNAM,PARVAL);

      // Data set 9
      frmProgressMM.AddMessage(Format(StrWritingDataSet9, [PARNAM]));
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

procedure TModflowLVDA_Writer.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingLPFLV);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrLVDA) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if Model.ModflowSteadyParameters.CountParameters([ptHUF_LVDA]) = 0 then
  begin
    Exit;
  end
  else
  begin
    CheckLvdaParameters;
  end;

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterZonesNot);

    FNameOfFile := FileName(AFileName);
    WriteToNameFile(StrLVDA, Model.UnitNumbers.UnitNumber(StrLVDA),
      FNameOfFile, foInput, Model);
    FInputFileName := FNameOfFile;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingLVDAPackage);
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

      frmProgressMM.AddMessage(StrWritingDataSets2and3);
      WriteDataSets2and3;
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
