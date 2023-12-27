unit Modflow6ModelImporter;

interface

uses
  System.Classes, System.IOUtils, Vcl.Dialogs, System.SysUtils, System.UITypes,
  Mf6.SimulationNameFileReaderUnit, System.Math;

  // The first name in NameFiles must be the name of the groundwater flow
  // simulation name file (mfsim.nam). Any additional names must be associated
  // transport simulation name files (mfsim.nam)

type
  TModflow6Importer = class(TObject)
  private
    FErrorMessages: TStringList;
    FSimulation: TMf6Simulation;
    procedure ImportFlowModelTiming;
  public
    procedure ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmSelectFlowModelUnit,
  Mf6.TDisFileReaderUnit, ModflowTimeUnit, ModflowOptionsUnit,
  Mf6.AtsFileReaderUnit;

resourcestring
  StrTheNameFileSDoe = 'The name file %s does not exist.';

procedure TModflow6Importer.ImportFlowModelTiming;
var
  PhastModel: TPhastModel;
  MFStressPeriods: TModflowStressPeriods;
  StressPeriods: TTDis;
  MfOptions: TModflowOptions;
  TimeUnits: string;
  ValidUnits: TStringList;
  MfTimeUnit: Integer;
  SPIndex: Integer;
  SPData: TPeriod;
  MfStressPeriod: TModflowStressPeriod;
  StartTime: double;
  AtsIndex: Integer;
  AtsPeriod: TAtsPeriod;
begin
  StressPeriods := FSimulation.Timing.TDis;


  PhastModel := frmGoPhast.PhastModel;
  MfOptions := PhastModel.ModflowOptions;
  TimeUnits := UpperCase(StressPeriods.Options.TimeUnits);
  if StressPeriods.Options.StartDate <> '' then
  begin
    MfOptions.Description.Add('Start Date = ' + StressPeriods.Options.StartDate);
    FErrorMessages.Add('Warning: The start date of the model has been added as a comment to the model description')
  end;

  ValidUnits := TStringList.Create;
  try
    ValidUnits.Add('UNKNOWN');
    ValidUnits.Add('SECONDS');
    ValidUnits.Add('MINUTES');
    ValidUnits.Add('HOURS');
    ValidUnits.Add('DAYS');
    ValidUnits.Add('YEARS');
    MfTimeUnit := ValidUnits.IndexOf(TimeUnits);
    if MfTimeUnit < 0 then
    begin
      MfTimeUnit := 0;
    end;
    MfOptions.TimeUnit := MfTimeUnit;
  finally
    ValidUnits.Free;
  end;


  MFStressPeriods := PhastModel.ModflowStressPeriods;
  MFStressPeriods.Capacity := StressPeriods.Dimensions.NPER;
  StartTime := 0.0;
  for SPIndex := 0 to StressPeriods.PeriodData.Count - 1 do
  begin
    SPData := StressPeriods.PeriodData[SPIndex];
    MfStressPeriod := MFStressPeriods.Add;
    MfStressPeriod.StartTime := StartTime;
    StartTime := StartTime + SPData.PerLen;
    MfStressPeriod.EndTime := StartTime;
    MfStressPeriod.PeriodLength := SPData.PerLen;
    MfStressPeriod.TimeStepMultiplier := SPData.TSMult;

    if SPData.NSTP > 1 then
    begin
      if SPData.TSMULT = 1 then
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN / SPData.NSTP;
      end
      else
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN * (SPData.TSMULT - 1)
          / (IntPower(SPData.TSMULT, SPData.NSTP) - 1);
      end;
    end
    else
    begin
      MfStressPeriod.MaxLengthOfFirstTimeStep := MfStressPeriod.PeriodLength;
    end;
  end;

  if StressPeriods.Ats <> nil then
  begin
    for AtsIndex := 0 to StressPeriods.Ats.Count - 1 do
    begin
      AtsPeriod := StressPeriods.Ats.AtsPeriod[AtsIndex];
      if AtsPeriod.iperats <= 0 then
      begin
        FErrorMessages.Add('ATS period data for iperats <= 0 is skipped ')
      end
      else if AtsPeriod.iperats > MFStressPeriods.Count then
      begin
        FErrorMessages.Add('ATS period data for iperats > NPER is skipped ')
      end
      else
      begin
        MfStressPeriod := MFStressPeriods[AtsPeriod.iperats-1];
        MfStressPeriod.AtsUsed := True;
        MfStressPeriod.AtsInitialStepSize := AtsPeriod.dt0;
        MfStressPeriod.AtsMinimumStepSize := AtsPeriod.dtmin;
        MfStressPeriod.AtsMaximumStepSize := AtsPeriod.dtmax;
        MfStressPeriod.AtsAdjustmentFactor := AtsPeriod.dtadj;
        MfStressPeriod.AtsFailureFactor := AtsPeriod.dtfailadj;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
var
  FileIndex: Integer;
  OutFile: string;
  ListFile: TStringList;
  PhastModel: TPhastModel;
  ModelIndex: Integer;
  AModel: TModel;
  FlowModelNames: TStringList;
  FlowModelImported: Boolean;
  frmSelectFlowModel: TfrmSelectFlowModel;
  ModelNameFile: string;
  FlowModel: TModel;
begin
  FErrorMessages := ErrorMessages;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    if not TFile.Exists(NameFiles[FileIndex]) then
    begin
      Beep;
      MessageDlg(Format(StrTheNameFileSDoe, [NameFiles[FileIndex]]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.Clear;
  PhastModel.ModelSelection := msModflow2015;

  FlowModelImported := False;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    FSimulation := TMf6Simulation.Create('Simulation');
    try
      FSimulation.ReadSimulation(NameFiles[FileIndex]);
      OutFile := ChangeFileExt(NameFiles[FileIndex], '.lst');
      if TFile.Exists(OutFile) then
      begin
        ListFile := TStringList.Create;
        try
          ListFile.LoadFromFile(OutFile);
          if ListFile.Count > 0 then
          begin
            ErrorMessages.Add('The following errors were encountered when reading ' + NameFiles[FileIndex]);
            ErrorMessages.AddStrings(ListFile);
            ErrorMessages.Add('');
          end;
        finally
          ListFile.Free;
        end;
      end
      else
      begin
        ErrorMessages.Add(OutFile + ' does not exist.')
      end;

      if FSimulation.Exchanges.Count > 0 then
      begin
        ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
        ErrorMessages.Add('ModelMuse does not currently support MODFLOW 6 exchanges');
      end;

      FlowModelNames := TStringList.Create;
      try
        for ModelIndex := 0 to FSimulation.Models.Count - 1 do
        begin
          AModel := FSimulation.Models[ModelIndex];
          if AModel.ModelType = 'GWF6' then
          begin
            FlowModelNames.Add(AModel.NameFile)
          end;
        end;
        if FlowModelImported and (FlowModelNames.Count > 0) then
        begin
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
          ErrorMessages.Add('Another flow model name file was already in another simulation name file');
          Exit;
        end;
        if FlowModelNames.Count > 1 then
        begin
          frmSelectFlowModel := TfrmSelectFlowModel.Create(nil);
          try
            frmSelectFlowModel.rgFlowModels.Items := FlowModelNames;
            frmSelectFlowModel.rgFlowModels.ItemIndex := 0;
            if frmSelectFlowModel.ShowModal = mrOK then
            begin
              ModelNameFile := frmSelectFlowModel.rgFlowModels.Items[frmSelectFlowModel.rgFlowModels.ItemIndex];
            end
            else
            begin
              Exit;
            end;
          finally
            frmSelectFlowModel.Free
          end;
        end
        else
        begin
          ModelNameFile := FlowModelNames[0];
        end;
        Assert(ModelNameFile <> '');
        FlowModel := FSimulation.Models.GetModelByNameFile(ModelNameFile);
        ImportFlowModelTiming;

      finally
        FlowModelNames.Free
      end;

    finally
      FSimulation.Free;
      FSimulation := nil;
    end;
  end;
end;

end.
