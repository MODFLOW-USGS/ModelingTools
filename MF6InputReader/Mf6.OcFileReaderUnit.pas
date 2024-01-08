unit Mf6.OcFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TOcOptions = class(TCustomMf6Persistent)
  private
    FBudgetFile: string;
    FBudgetCsvFile: string;
    FHeadFile: string;
    FHeadPrintFormat: TPrintFormat;
    FConcentrationFile: string;
    FConcentrationPrintFormat: TPrintFormat;
    FFullBudgetFileName: string;
    FFullHeadFileName: string;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GeConcentrationFile: Boolean;
    function GetBudgetCsvFile: Boolean;
    function GetBudgetFile: Boolean;
    function GetHeadFile: Boolean;
  protected
    procedure Initialize; override;
  public
    // The name of the budget file in this file can be used
    // to identify the corresponding model in the flow model interface file (*.fmi).
    property BudgetFile: Boolean read GetBudgetFile;
    property BudgetCsvFile: Boolean read GetBudgetCsvFile;
    // The name of the head file in this file can be used
    // to identify the corresponding model in the flow model interface file (*.fmi).
    property HeadFile: Boolean read GetHeadFile;
    property HeadPrintFormat: TPrintFormat read FHeadPrintFormat;
    property ConcentrationFile: Boolean read GeConcentrationFile;
    property ConcentrationPrintFormat: TPrintFormat read FConcentrationPrintFormat;
    property FullBudgetFileName: string read FFullBudgetFileName;
    property FullHeadFileName: string read FFullHeadFileName;
  end;

  TPrintSaveOption = (psoAll, psoFirst, psoLast, psoFrequency, psoStep, psoUndefined);

  TPrintSave = record
    FPS_Option: TPrintSaveOption;
    FFrequency: Integer;
    FSteps: TArray<Integer>;
    procedure Initialize;
  end;

  TPrintSaveList = TList<TPrintSave>;

  TOcPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FPrintBudget: TPrintSaveList;
    FSaveBudget: TPrintSaveList;
    FPrintHead: TPrintSaveList;
    FSaveHead: TPrintSaveList;
    FPrintConcentration: TPrintSaveList;
    FSaveConcentration: TPrintSaveList;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TOcPeriodList = TObjectList<TOcPeriod>;

  TOc = class(TPackageReader)
  private
    FOptions: TOcOptions;
    FPeriods: TOcPeriodList;
    function GetFullBudgetFileName: string;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property Options: TOcOptions read FOptions;
    property FullBudgetFileName: string read GetFullBudgetFileName;
  end;



implementation

resourcestring
  StrUnrecognizedOCPERI = 'Unrecognized OC PERIOD data in the following line' +
  '.';

{ TOcOptions }

function TOcOptions.GeConcentrationFile: Boolean;
begin
  result := FConcentrationFile <> ''
end;

function TOcOptions.GetBudgetCsvFile: Boolean;
begin
  result := FBudgetCsvFile <> ''
end;

function TOcOptions.GetBudgetFile: Boolean;
begin
  result := FBudgetFile <> ''
end;

function TOcOptions.GetHeadFile: Boolean;
begin
  result := FHeadFile <> ''
end;

procedure TOcOptions.Initialize;
begin
  FHeadPrintFormat.Initialize;
  FConcentrationPrintFormat.Initialize;
  inherited;
end;

procedure TOcOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  PackageName: string;
  CaseSensitiveLine: string;
begin
  Initialize;
  PackageName := 'OC';
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    RestoreStream(Stream);
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      if FSplitter[0] = 'BUDGET' then
      begin
        if FSplitter[1] = 'FILEOUT' then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          FBudgetFile := FSplitter[2];
          FFullBudgetFileName := ExpandFileName(FBudgetFile);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BUDGETCSV' then
      begin
        if FSplitter[1] = 'FILEOUT' then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          FBudgetCsvFile := FSplitter[2];
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'HEAD' then
      begin
        if FSplitter[1] = 'FILEOUT' then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          FHeadFile := FSplitter[2];
          FFullHeadFileName := ExpandFileName(FHeadFile);
        end
        else if FSplitter[1] = 'PRINT_FORMAT' then
        begin
          ReadPrintFormat(ErrorLine, Unhandled, PackageName, FHeadPrintFormat);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'CONCENTRATION' then
      begin
        if FSplitter[1] = 'FILEOUT' then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          FConcentrationFile := FSplitter[2];
        end
        else if FSplitter[1] = 'PRINT_FORMAT' then
        begin
          ReadPrintFormat(ErrorLine, Unhandled, PackageName, FConcentrationPrintFormat);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [PackageName]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TOcPeriod }

constructor TOcPeriod.Create(PackageType: string);
begin
  FPrintBudget := TPrintSaveList.Create;
  FSaveBudget := TPrintSaveList.Create;
  FPrintHead := TPrintSaveList.Create;
  FSaveHead := TPrintSaveList.Create;
  FPrintConcentration := TPrintSaveList.Create;
  FSaveConcentration := TPrintSaveList.Create;
  inherited;

end;

destructor TOcPeriod.Destroy;
begin
  FPrintBudget.Free;
  FSaveBudget.Free;
  FPrintHead.Free;
  FSaveHead.Free;
  FPrintConcentration.Free;
  FSaveConcentration.Free;
  inherited;
end;

procedure TOcPeriod.Initialize;
begin
  inherited;
  FPrintBudget.Clear;
  FSaveBudget.Clear;
  FPrintHead.Clear;
  FSaveHead.Clear;
  FPrintConcentration.Clear;
  FSaveConcentration.Clear;
end;

procedure TOcPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  List: TPrintSaveList;
  PrintSave: TPrintSave;
  Steps: TList<Integer>;
  StepIndex: Integer;
  AStep: Integer;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    RestoreStream(Stream);
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;
    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      List := nil;
      if FSplitter[0] = 'SAVE' then
      begin
        if FSplitter[1] = 'BUDGET' then
        begin
          List := FSaveBudget;
        end
        ELSE if FSplitter[1] = 'HEAD' then
        begin
          List := FSaveHead;
        end
        ELSE if FSplitter[1] = 'CONCENTRATION' then
        begin
          List := FSaveConcentration;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedOCPERI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'PRINT' then
      begin
        if FSplitter[1] = 'BUDGET' then
        begin
          List := FPrintBudget;
        end
        ELSE if FSplitter[1] = 'HEAD' then
        begin
          List := FPrintHead;
        end
        ELSE if FSplitter[1] = 'CONCENTRATION' then
        begin
          List := FPrintConcentration;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedOCPERI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedOCPERI);
        Unhandled.WriteLine(ErrorLine);
      end;
      if List <> nil then
      begin
        PrintSave.Initialize;
        if FSplitter[2] = 'ALL' then
        begin
          PrintSave.FPS_Option := psoAll;
        end
        else if FSplitter[2] = 'FIRST' then
        begin
          PrintSave.FPS_Option := psoFirst;
        end
        else if FSplitter[2] = 'LAST' then
        begin
          PrintSave.FPS_Option := psoLast;
        end
        else if FSplitter[2] = 'FREQUENCY' then
        begin
          PrintSave.FPS_Option := psoFrequency;
          if FSplitter.Count >= 4 then
          begin
            PrintSave.FFrequency := StrToInt(FSplitter[3]);
          end
          else
          begin
            Unhandled.WriteLine(StrUnrecognizedOCPERI);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if FSplitter[2] = 'STEPS' then
        begin
          PrintSave.FPS_Option := psoStep;
          Steps := TList<Integer>.Create;
          try
            Steps.Capacity := FSplitter.Count -3;
            for StepIndex := 3 to FSplitter.Count - 1 do
            begin
              if TryStrToInt(FSplitter[StepIndex], AStep) then
              begin
                Steps.Add(AStep)
              end
              else
              begin
                Break;
              end;
            end;
            PrintSave.FSteps := Steps.ToArray;
          finally
            Steps.Free;
          end;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedOCPERI);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        List.Add(PrintSave);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedOCPERI);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TPrintSave }

procedure TPrintSave.Initialize;
begin
  FPS_Option := psoUndefined;
  FFrequency := -1;
  SetLength(FSteps, 0);
end;

{ TOc }

constructor TOc.Create(PackageType: string);
begin
  FOptions := TOcOptions.Create(PackageType);
  FPeriods := TOcPeriodList.Create;
  inherited;

end;

destructor TOc.Destroy;
begin
  FOptions.Free;
  FPeriods.Free;
  inherited;
end;

function TOc.GetFullBudgetFileName: string;
begin
  result := FOptions.FullBudgetFileName;
end;

procedure TOc.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPer: Integer;
  APeriod: TOcPeriod;
begin
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;

    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'BEGIN' then
      begin
        if FSplitter[1] = 'OPTIONS' then
        begin
          FOptions.Read(Stream, Unhandled);
        end
        else if FSplitter[1] = 'PERIOD' then
        begin
          if (FSplitter.Count >= 3)
            and TryStrToInt(FSplitter[2], IPer) then
          begin
            APeriod := TOcPeriod.Create(FPackageType);
            FPeriods.Add(APeriod);
            APeriod.IPer := IPer;
            APeriod.Read(Stream, Unhandled);
          end
          else
          begin
            Unhandled.WriteLine('Unrecognized OC data in the following line.');
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized OC data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized OC data in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

end.
