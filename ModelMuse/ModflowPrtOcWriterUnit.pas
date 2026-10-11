unit ModflowPrtOcWriterUnit;

interface

uses
 CustomModflowWriterUnit, ModflowPackageSelectionUnit, Vcl.Forms,
  System.SysUtils, System.Classes;

Type
  TPrtOcWriter = class(TCustomFlowPackageWriter)
  private
    FPrtModel: TPrtModel;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteTrackTimes;
    procedure WriteStressPeriods;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    property PrtModel: TPrtModel read FPrtModel write FPrtModel;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, PhastModelUnit, ModflowTimeUnit, frmProgressUnit,
  GoPhastTypes;

resourcestring
  STRACK_USERTIMENotSelected = 'TRACK_USERTIME not selected';
  SAlthoughTracktimesHaveBeenSelect = 'Although tracktimes have been selected '
  + 'in model: "%s", TRACK_USERTIME was not selected so they will not be used.';

{ TPrtOcWriter }

class function TPrtOcWriter.Extension: string;
begin
  result := '.oc';
end;

function TPrtOcWriter.Package: TModflowPackageSelection;
begin
  result := nil;
  Assert(False);
end;

procedure TPrtOcWriter.WriteDimensions;
begin
  WriteBeginDimensions;
  try
    WriteString('  NTRACKTIMES ');
    WriteInteger(PrtModel.TrackTimes.Count);
    NewLine;
  finally
    WriteEndDimensions;
  end;
end;

procedure TPrtOcWriter.WriteFile(const AFileName: string);
begin
  Assert(PrtModel <> nil);
  if not PrtModel.IsSelected then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    FNameOfFile := ChangeFileExt(AFileName, '') + '.' + PrtModel.ModelName + Extension;
    FInputFileName := FNameOfFile;
    WriteToPrtNameFile('OC6', FNameOfFile, PrtModel, 'OC6');
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage('Writing PRT OC data');
      frmProgressMM.AddMessage(StrWritingDataSet0);
      WriteCommentLine(File_Comment('OC Package for PRT model'));

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

      WriteDimensions;

      frmProgressMM.AddMessage('  Writing TRACKTIMES');
      WriteTrackTimes;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingStressPerio);
      WriteStressPeriods;
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

procedure TPrtOcWriter.WriteOptions;
var
  BaseNameOfFile: string;
  BudgetFileOut: string;
  BudgetCsvFileOut: string;
  TrackFileOut: string;
  TrackCsvFileOut: string;
  procedure WriteSingleQuoteMF6;
  begin
    WriteString('''');
  end;
begin
  BaseNameOfFile := ChangeFileExt(FNameOfFile, '');
  WriteBeginOptions;
  try
    if pofBinaryBudget in PrtModel.PrtOutputFiles then
    begin
      BudgetFileOut := BaseNameOfFile + '.cbb';
      Model.AddModelOutputFile(BudgetFileOut);
      WriteString('  BUDGET FILEOUT ');
      WriteSingleQuoteMF6;
      WriteString(ExtractFileName(BudgetFileOut));
      WriteSingleQuoteMF6;
      NewLine
    end;

    if pofoCsvBudget in PrtModel.PrtOutputFiles then
    begin
      BudgetCsvFileOut := BaseNameOfFile + '.bud.csv';
      Model.AddModelOutputFile(BudgetCsvFileOut);
      WriteString('  BUDGETCSV FILEOUT ');
      WriteSingleQuoteMF6;
      WriteString(ExtractFileName(BudgetCsvFileOut));
      WriteSingleQuoteMF6;
      NewLine
    end;

    if pofBinaryTrack in PrtModel.PrtOutputFiles then
    begin
      TrackFileOut := BaseNameOfFile + '.trk';
      Model.AddModelOutputFile(TrackFileOut);
      Model.AddModelOutputFile(TrackFileOut + '.hdr');
      WriteString('  TRACK FILEOUT ');
      WriteSingleQuoteMF6;
      WriteString(ExtractFileName(TrackFileOut));
      WriteSingleQuoteMF6;
      NewLine
    end;

    if pofCsvTrack in PrtModel.PrtOutputFiles then
    begin
      TrackCsvFileOut := BaseNameOfFile + '.trk.csv';
      Model.AddModelOutputFile(TrackCsvFileOut);
      WriteString('  TRACKCSV FILEOUT ');
      WriteSingleQuoteMF6;
      WriteString(ExtractFileName(TrackCsvFileOut));
      WriteSingleQuoteMF6;
      NewLine
    end;

    if ptoRelease in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_RELEASE');
      NewLine
    end;

    if ptoExit in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_EXIT');
      NewLine
    end;

    if ptoSubFeatureExit in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_SUBFEATURE_EXIT');
      NewLine
    end;

    if ptoTimeStep in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_TIMESTEP');
      NewLine
    end;

    if ptoTerminate in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_TERMINATE');
      NewLine
    end;

    if ptoWeakSink in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_WEAKSINK');
      NewLine
    end;

    if (ptoUserTime in PrtModel.PrtTrackingOptions)  then
    begin
      WriteString('  TRACK_USERTIME');
      NewLine
    end
    else
    begin
      if (PrtModel.TrackTimes.Count > 0) then
      begin
        frmErrorsAndWarnings.AddWarning(Model, STRACK_USERTIMENotSelected,
          Format(SAlthoughTracktimesHaveBeenSelect, [PrtModel.ModelName]));
      end;
    end;

    if ptoDropped in PrtModel.PrtTrackingOptions then
    begin
      WriteString('  TRACK_DROPPED');
      NewLine
    end;

    if PrtModel.ScratchBuffer then
    begin
      WriteString('  SCRATCH_BUFFER');
      NewLine;
    end;
  finally
    WriteEndOptions;
  end;
end;

procedure TPrtOcWriter.WriteStressPeriods;
var
  StressPeriods: TModflowStressPeriods;
  APeriodItem: TPrpPeriodDataItem;
  StartPeriod: Integer;
  EndPeriod: Integer;
begin
  if PrtModel.PeriodData.Count > 0 then
  begin
    StressPeriods := (Model as TPhastModel).ModflowFullStressPeriods;
    EndPeriod := -1;
    for var PeriodIndex := 0 to PrtModel.PeriodData.Count - 1 do
    begin
      APeriodItem := PrtModel.PeriodData[PeriodIndex];
      StartPeriod := StressPeriods.FindStressPeriod(APeriodItem.StartTime);
      if EndPeriod >= 0 then
      begin
        if (StartPeriod > EndPeriod+1) then
        begin
          WriteBeginPeriod(EndPeriod+1);
          WriteEndPeriod;
        end;
      end;
      EndPeriod := StressPeriods.FindEndStressPeriod(APeriodItem.EndTime);
      WriteBeginPeriod(StartPeriod);

      try
        if APeriodItem.OCMethod in [pomBoth, pomPrint] then
        begin
          if APeriodItem.All then
          begin
            WriteString('  PRINT BUDGET ALL');
            NewLine;
          end;

          if APeriodItem.First then
          begin
            WriteString('  PRINT BUDGET FIRST');
            NewLine;
          end;

          if APeriodItem.Last then
          begin
            WriteString('  PRINT BUDGET LAST');
            NewLine;
          end;

          if APeriodItem.Frequency > 0 then
          begin
            WriteString('  PRINT BUDGET FREQUENCY ');
            WriteInteger(APeriodItem.Frequency);
            NewLine;
          end;

          if APeriodItem.Steps.Count > 0 then
          begin
            WriteString('  PRINT BUDGET STEPS ');
            for var StepIndex := 0 to APeriodItem.Steps.Count - 1 do
            begin
              WriteInteger(APeriodItem.Steps[StepIndex].Value);
            end;
            NewLine;
          end;
        end;

        if APeriodItem.OCMethod in [pomBoth, pomSave] then
        begin
          if APeriodItem.All then
          begin
            WriteString('  SAVE BUDGET ALL');
            NewLine;
          end;

          if APeriodItem.First then
          begin
            WriteString('  SAVE BUDGET FIRST');
            NewLine;
          end;

          if APeriodItem.Last then
          begin
            WriteString('  SAVE BUDGET LAST');
            NewLine;
          end;

          if APeriodItem.Frequency > 0 then
          begin
            WriteString('  SAVE BUDGET FREQUENCY ');
            WriteInteger(APeriodItem.Frequency);
            NewLine;
          end;

          if APeriodItem.Steps.Count > 0 then
          begin
            WriteString('  SAVE BUDGET STEPS ');
            for var StepIndex := 0 to APeriodItem.Steps.Count - 1 do
            begin
              WriteInteger(APeriodItem.Steps[StepIndex].Value);
            end;
            NewLine;
          end;
        end;

      finally
        WriteEndPeriod;
      end;

      if (EndPeriod >= 0) then
      begin
        if EndPeriod + 1 < StressPeriods.Count then
        begin
          WriteBeginPeriod(EndPeriod+1);
          WriteEndPeriod;
        end;
      end;
    end;
  end;
end;

procedure TPrtOcWriter.WriteTrackTimes;
var
  StartingTime: double;
  StressPeriods: TModflowStressPeriods;
  ATime: double;
begin
  if PrtModel.TrackTimes.Count > 0 then
  begin
    WriteString('BEGIN TRACKTIMES');
    NewLine;

    try
      StressPeriods := (Model as TPhastModel).ModflowFullStressPeriods;
      StartingTime := StressPeriods.First.StartTime;

      for var TimeIndex := 0 to PrtModel.TrackTimes.Count - 1 do
      begin
        ATime := PrtModel.TrackTimes[TimeIndex].Value;
        WriteFloat(ATime-StartingTime);
        NewLine;
      end;
    finally
      WriteString('End TRACKTIMES');
      NewLine;
    end;
  end;
end;

end.
