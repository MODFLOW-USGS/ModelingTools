unit Modflow6ModelImporter;

interface

uses
  System.Classes, System.IOUtils, Vcl.Dialogs, System.SysUtils, System.UITypes,
  Mf6.SimulationNameFileReaderUnit, System.Math, Mf6.CustomMf6PersistentUnit;

  // The first name in NameFiles must be the name of the groundwater flow
  // simulation name file (mfsim.nam). Any additional names must be associated
  // transport simulation name files (mfsim.nam)

type
  TModflow6Importer = class(TObject)
  private
    FErrorMessages: TStringList;
    FSimulation: TMf6Simulation;
    FFlowModel: TModel;
    procedure ImportFlowModelTiming;
    procedure ImportSimulationOptions;
    procedure ImportSolutionGroups;
    procedure ImportFlowModel;
    procedure ImportDis(Package: TPackage);
  public
    procedure ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmSelectFlowModelUnit,
  Mf6.TDisFileReaderUnit, ModflowTimeUnit, ModflowOptionsUnit,
  Mf6.AtsFileReaderUnit, ModflowPackageSelectionUnit, ModflowOutputControlUnit,
  Mf6.NameFileReaderUnit, Mf6.DisFileReaderUnit;

resourcestring
  StrTheNameFileSDoe = 'The name file %s does not exist.';

procedure TModflow6Importer.ImportDis(Package: TPackage);
var
  Dis: TDis;
  XOrigin: Extended;
  YOrigin: Extended;
  GridAngle: Extended;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  ColumnPositions: TOneDRealArray;
  RowPositions: TOneDRealArray;
  Delr: TDArray1D;
  Position: Extended;
  ColIndex: Integer;
  Delc: TDArray1D;
  AngleToLL: Extended;
  DistanceToLL: Extended;
  RowIndex: Integer;
begin
  Model := frmGoPhast.PhastModel;
  MfOptions := Model.ModflowOptions;

  Dis := Package.Package as TDis;
  MfOptions.LengthUnit := Dis.Options.LENGTH_UNITS;
  MfOptions.WriteBinaryGridFile := not Dis.Options.NOGRB;

  XOrigin := Dis.Options.XORIGIN;
  YOrigin := Dis.Options.YORIGIN;
  GridAngle := Dis.Options.ANGROT * Pi / 180;

  Delr := Dis.GridData.DELR;
  SetLength(ColumnPositions, Length(Delr) + 1);
  Delc := Dis.GridData.DELC;
  SetLength(RowPositions, Length(Delc) + 1);

  if GridAngle = 0 then
  begin
    Position := XOrigin;
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := YOrigin;
    RowPositions[Length(RowPositions)-1] := Position;
    for ColIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[ColIndex];
      RowPositions[Length(RowPositions)-1 -ColIndex] := Position;
    end;
  end
  else
  begin
    AngleToLL := ArcTan2(YOrigin, XOrigin);
    DistanceToLL := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

    Position := DistanceToLL * Cos(AngleToLL - GridAngle);
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := DistanceToLL * Sin(AngleToLL - GridAngle);
    RowPositions[Length(RowPositions)-1] := Position;
    for RowIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[RowIndex];
      RowPositions[Length(RowPositions) - RowIndex - 2] := Position;
    end;
  end;

  Model.ModflowGrid.BeginGridChange;
  try
    Model.ModflowGrid.GridAngle := GridAngle;
    Model.ModflowGrid.ColumnPositions := ColumnPositions;
    Model.ModflowGrid.RowPositions := RowPositions;
  finally
    Model.ModflowGrid.EndGridChange;
  end;

end;

procedure TModflow6Importer.ImportFlowModel;
var
  NameFile: TFlowNameFile;
  Options: TFlowNameFileOptions;
  Packages: TFlowPackages;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  OC: TModflowOutputControl;
  PackageIndex: Integer;
  APackage: TPackage;
begin
  if FFlowModel <> nil then
  begin
    Model := frmGoPhast.PhastModel;

    NameFile := FFlowModel.FName as TFlowNameFile;

    Options := NameFile.NfOptions;
    MfOptions := Model.ModflowOptions;
    MfOptions.NewtonMF6 := Options.NEWTON;
    MfOptions.UnderRelaxationMF6 := Options.UNDER_RELAXATION;
    if Options.PRINT_INPUT then
    begin
      OC := Model.ModflowOutputControl;
      OC.PrintInputCellLists := True;
      OC.PrintInputArrays := True;
    end;

    Packages := NameFile.NfPackages;

    for PackageIndex := 0 to Packages.Count - 1 do
    begin
      APackage := Packages[PackageIndex];
      if APackage.FileType = 'DIS6' then
      begin
        ImportDis(APackage);
        break
      end
      else if APackage.FileType = 'DISV6' then
      begin
        break;
      end
      else if APackage.FileType = 'DISU6' then
      begin
        break;
      end
      else
      begin
        Continue;
      end;
    end;
  end;
end;

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
  ExchangeIndex: Integer;
  Exchange: TExchange;
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

      for ExchangeIndex := 0 to FSimulation.Exchanges.Count - 1 do
      begin
        Exchange := FSimulation.Exchanges[ExchangeIndex];
        if not AnsiSameText(Exchange.ExchangeType, 'GWF6-GWT6') then
        begin
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
          ErrorMessages.Add('ModelMuse does not currently support MODFLOW 6 exchanges');
          break;
        end;
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
        ModelNameFile := '';
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
        if ModelNameFile <> '' then
        begin
          FFlowModel := FSimulation.Models.GetModelByNameFile(ModelNameFile);
        end
        else
        begin
          FFlowModel := nil;
        end;
        ImportSimulationOptions;
        ImportFlowModelTiming;
        ImportSolutionGroups;
        ImportFlowModel;

      finally
        FlowModelNames.Free
      end;

    finally
      FSimulation.Free;
      FSimulation := nil;
    end;
  end;
end;

procedure TModflow6Importer.ImportSimulationOptions;
var
  Model: TPhastModel;
  SmsPkg: TSmsPackageSelection;
  OC: TModflowOutputControl;
begin
  Model := frmGoPhast.PhastModel;
  SmsPkg := Model.ModflowPackages.SmsPackage;
  SmsPkg.ContinueModel := FSimulation.Options.ContinueOption;
  if FSimulation.Options.NoCheckOption then
  begin
    SmsPkg.CheckInput := ciDontCheck
  end;
  case FSimulation.Options.MemPrint of
    Mf6.SimulationNameFileReaderUnit.mpNone:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpNone;
      end;
    Mf6.SimulationNameFileReaderUnit.mpSummary:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpSummary;
      end;
    Mf6.SimulationNameFileReaderUnit.mpAll:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpAll;
      end;
  end;
  SmsPkg.MaxErrors := FSimulation.Options.MaxErrors;
  if FSimulation.Options.PrintInputOption then
  begin
    OC := Model.ModflowOutputControl;
    OC.PrintInputCellLists := True;
  end;
end;

procedure TModflow6Importer.ImportSolutionGroups;
var
  GroupIndex: Integer;
  Group: TSolutionGroup;
  SolutionIndex: Integer;
  Solution: TSolution;
  ModelIndex: Integer;
  Model: TPhastModel;
begin
  // for now, this just imports Mxiter.
  if FFlowModel = nil then
  begin
    Exit;
  end;
  Model := frmGoPhast.PhastModel;
  for GroupIndex := 0 to FSimulation.SolutionGroupCount - 1 do
  begin
    Group := FSimulation.SolutionGroups[GroupIndex];
    for SolutionIndex := 0 to Group.Count - 1 do
    begin
      Solution := Group.Solutions[SolutionIndex];
      for ModelIndex := 0 to Solution.FSolutionModelNames.Count - 1 do
      begin
        if AnsiSameText(Solution.FSolutionModelNames[ModelIndex], FFlowModel.ModelName) then
        begin
          Model.ModflowPackages.SmsPackage.SolutionGroupMaxIteration
            := Group.Mxiter
        end;
      end;
    end;
  end;
end;

end.
