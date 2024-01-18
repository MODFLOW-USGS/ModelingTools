unit Mf6.SimulationNameFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.TDisFileReaderUnit, Mf6.NameFileReaderUnit,
  Mf6.ImsFileReaderUnit;

type
  TMemPrint = (mpNone, mpSummary, mpAll);

  TSimulationOptions = class(TCustomMf6Persistent)
  private
    FContinueOption: Boolean;
    FNoCheckOption: Boolean;
    FMemPrint: TMemPrint;
    FPrintInputOption: Boolean;
    FMaxErrors: Integer;
    procedure SetContinueOption(const Value: Boolean);
    procedure SetMaxErrors(const Value: Integer);
    procedure SetMemPrint(const Value: TMemPrint);
    procedure SetNoCheckOption(const Value: Boolean);
    procedure SetPrintInputOption(const Value: Boolean);
  protected
    procedure Initialize; override;
  public
    property ContinueOption: Boolean read FContinueOption write SetContinueOption;
    property NoCheckOption: Boolean read FNoCheckOption write SetNoCheckOption;
    property MemPrint: TMemPrint read FMemPrint write SetMemPrint;
    property MaxErrors: Integer read FMaxErrors write SetMaxErrors;
    property PrintInputOption: Boolean read FPrintInputOption write SetPrintInputOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TTiming = class(TCustomMf6Persistent)
  private
    FTisFileName: string;
    FTDis: TTDis;
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    procedure ReadInput(Unhandled: TStreamWriter);
    property TDis: TTDis read FTDis;
  end;

  TModel = class(TObject)
  private
    function GetFullBudgetFileName: string;
  public
    ModelType: string;
    NameFile: string;
    ModelName: string;
    FName: TCustomNameFile;
    destructor Destroy; override;
    procedure ReadInput(Unhandled: TStreamWriter; const NPER: integer);
    property FullBudgetFileName: string read GetFullBudgetFileName;
  end;

  TModelList = TList<TModel>;
  TObjectModelList = TObjectList<TModel>;

  TModels = class(TCustomMf6Persistent)
  private
    FModels: TObjectModelList;
    function GetCount: Integer;
    function GetModel(Index: Integer): TModel;
  protected
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    procedure ReadInput(Unhandled: TStreamWriter; const NPER: integer);
    function GetModelByName(ModelName: string): TModel;
    function GetModelByNameFile(NameFile: string): TModel;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TModel read GetModel; default;
  end;

  TExchange = class(TObject)
    ExchangeType: string;
    ExchangeFile: string;
    ExchangeModelNameA: string;
    ExchangeModelNameB: string;
  end;

  TExchangeList = TObjectList<TExchange>;

  TExchanges = class(TCustomMf6Persistent)
  private
    FExchanges: TExchangeList;
    function GetCount: Integer;
    function GetExchange(Index: Integer): TExchange;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TExchange read GetExchange; default;
  end;

  TSolution = class(TObject)
    SolutionType: string;
    SolutionFileName: string;
    FSolutionModelNames: TStringList;
    FIms: TIms;
    constructor Create;
    destructor Destroy; override;
    procedure ReadInput(Unhandled: TStreamWriter);
  end;

  TSolutionList = TObjectList<TSolution>;

  TSolutionGroup = class(TCustomMf6Persistent)
  private
    FSolutionNumber: Integer;
    FSolutions: TSolutionList;
    FMxiter: Integer;
    function GetCount: Integer;
    function GetSolutions(Index: Integer): TSolution;
    procedure SetSolutions(Index: Integer; const Value: TSolution);
  protected
    procedure Initialize; override;
  public
    constructor Create(SolutionNumber: Integer; PackageType: string); reintroduce;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    procedure ReadInput(Unhandled: TStreamWriter);
    property Count: Integer read GetCount;
    property Solutions[Index: Integer]: TSolution read GetSolutions write SetSolutions;
    property Mxiter: Integer read FMxiter;
  end;

  TSolutionGroups = TObjectList<TSolutionGroup>;

  TMf6Simulation = class(TCustomMf6Persistent)
  private
    FSimulationOptions: TSimulationOptions;
    FTiming: TTiming;
    FModels: TModels;
    FExchanges: TExchanges;
    FSimulationFile: TStreamReader;
    FSolutionGroups: TSolutionGroups;
    FExchangePackages: TPackageList;
    function GetExchanges: TExchanges;
    function GetSolutionGroup(Index: Integer): TSolutionGroup;
    function GetSolutionGroupCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure ReadSimulation(NameFile: string);
    property Models: TModels read FModels;
    property Exchanges: TExchanges read GetExchanges;
    property Timing: TTiming read FTiming;
    property Options: TSimulationOptions read FSimulationOptions;
    property SolutionGroupCount: Integer read GetSolutionGroupCount;
    property SolutionGroups[Index: Integer]: TSolutionGroup read GetSolutionGroup;
  end;

implementation

uses
  Mf6.ExchangeFileReaderUnit, Mf6.OcFileReaderUnit, Mf6.FmiFileReaderUnit;



{ TMf6Simulation }

constructor TMf6Simulation.Create(PackageType: string);
begin
  inherited;
  FSimulationOptions := TSimulationOptions.Create(PackageType);
  FTiming := TTiming.Create(PackageType);
  FModels := TModels.Create(PackageType);
  FExchanges := TExchanges.Create(PackageType);
  FSolutionGroups := TSolutionGroups.Create;
  FExchangePackages := TPackageList.Create;
end;

destructor TMf6Simulation.Destroy;
begin
  FExchangePackages.Free;
  FSolutionGroups.Free;
  FExchanges.Free;
  FModels.Free;
  FTiming.Free;
  FSimulationOptions.Free;
  inherited;
end;

function TMf6Simulation.GetExchanges: TExchanges;
begin
  result := FExchanges;
end;

function TMf6Simulation.GetSolutionGroup(Index: Integer): TSolutionGroup;
begin
  result := FSolutionGroups[Index];
end;

function TMf6Simulation.GetSolutionGroupCount: Integer;
begin
  result := FSolutionGroups.Count;
end;

procedure TMf6Simulation.ReadSimulation(NameFile: string);
var
  FOutFile: TStreamWriter;
  ALine: string;
  GroupNumber: Integer;
  SolutionGroup: TSolutionGroup;
  ErrorLine: string;
  GroupIndex: Integer;
  ExcIndex: Integer;
  ExcPackage: TPackage;
  GwfGwfReader: TGwfGwf;
  GwtGwtReader: TGwtGwt;
  Model1: TModel;
  Model2: TModel;
  NPER: Integer;
begin
  SetCurrentDir(ExtractFileDir(NameFile));
  try
    FSimulationFile := TFile.OpenText(NameFile);
    FOutFile := TFile.CreateText(ChangeFileExt(NameFile, '.lst'));
    try
      while not FSimulationFile.EndOfStream do
      begin
        ALine := FSimulationFile.ReadLine;
        ErrorLine := ALine;
        ALine := StripFollowingComments(ALine);
        if ALine = '' then
        begin
          Continue;
        end;

        ALine := UpperCase(ALine);
        if Pos('BEGIN', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('BEGIN')+1,1)) <> '' then
          begin
            FOutFile.WriteLine('Unrecognized simulation option in the following line.');
            FOutFile.WriteLine(ErrorLine);
            Continue;
          end;
          ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
          if Pos('OPTIONS', ALine) = 1 then
          begin
            if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
            begin
              FOutFile.WriteLine('Unrecognized simulation option in the following line.');
              FOutFile.WriteLine(ErrorLine);
              Continue;
            end;
            FSimulationOptions.Read(FSimulationFile, FOutFile)
          end
          else if Pos('TIMING', ALine) = 1 then
          begin
            if Trim(Copy(ALine,Length('TIMING')+1,1)) <> '' then
            begin
              FOutFile.WriteLine('Unrecognized simulation option in the following line.');
              FOutFile.WriteLine(ErrorLine);
              Continue;
            end;
            FTiming.Read(FSimulationFile, FOutFile)
          end
          else if Pos('MODELS', ALine) = 1 then
          begin
            if Trim(Copy(ALine,Length('MODELS')+1,1)) <> '' then
            begin
              FOutFile.WriteLine('Unrecognized simulation option in the following line.');
              FOutFile.WriteLine(ErrorLine);
              Continue;
            end;
            FModels.Read(FSimulationFile, FOutFile);
          end
          else if Pos('EXCHANGES', ALine) = 1 then
          begin
            if Trim(Copy(ALine,Length('EXCHANGES')+1,1)) <> '' then
            begin
              FOutFile.WriteLine('Unrecognized simulation option in the following line.');
              FOutFile.WriteLine(ErrorLine);
              Continue;
            end;
            FExchanges.Read(FSimulationFile, FOutFile);
          end
          else if Pos('SOLUTIONGROUP', ALine) = 1 then
          begin
            if Trim(Copy(ALine,Length('SOLUTIONGROUP')+1,1)) <> '' then
            begin
              FOutFile.WriteLine('Unrecognized simulation option in the following line.');
              FOutFile.WriteLine(ErrorLine);
              Continue;
            end;
            FSplitter.DelimitedText := ALine;
            if TryStrToInt(FSplitter[1], GroupNumber) then
            begin
              SolutionGroup := TSolutionGroup.Create(GroupNumber, 'SolutionGroup-' + GroupNumber.ToString);
              FSolutionGroups.Add(SolutionGroup);
              SolutionGroup.Read(FSimulationFile, FOutFile);
            end
            else
            begin
              FOutFile.WriteLine('Unable to read solution group number in the following line.');
              FOutFile.WriteLine(ErrorLine);
            end;
          end
          else
          begin
            FOutFile.WriteLine('Unrecognized simulation option in the following line.');
            FOutFile.WriteLine(ErrorLine);
          end;
        end;
      end;

      FTiming.ReadInput(FOutFile);
      NPER := FTiming.FTDis.NPER;
      FModels.ReadInput(FOutFile, NPER);
      for GroupIndex := 0 to FSolutionGroups.Count - 1 do
      begin
        FSolutionGroups[GroupIndex].ReadInput(FOutFile);
      end;

      for ExcIndex := 0 to FExchanges.FExchanges.Count - 1 do
      begin
        ExcPackage := TPackage.Create;
        FExchangePackages.Add(ExcPackage);
        ExcPackage.FileType := FExchanges.FExchanges[ExcIndex].ExchangeType;
        ExcPackage.FileName := FExchanges.FExchanges[ExcIndex].ExchangeFile;
        Model1 := FModels.GetModelByName(FExchanges.FExchanges[ExcIndex].ExchangeModelNameA);
        Model2 := FModels.GetModelByName(FExchanges.FExchanges[ExcIndex].ExchangeModelNameB);
        if (Model1 = nil) or (Model2 = nil) then
        begin
          FOutFile.WriteLine('One or more of the following model names was not recognized in a simulation exchange.');
          FOutFile.WriteLine(FExchanges.FExchanges[ExcIndex].ExchangeModelNameA);
          FOutFile.WriteLine(FExchanges.FExchanges[ExcIndex].ExchangeModelNameB);
        end;
        ExcPackage.PackageName := '';


        if ExcPackage.FileType = 'GWF6-GWF6' then
        begin

          GwfGwfReader := TGwfGwf.Create(FPackageType);

          GwfGwfReader.Dimensions := (Model1.FName as TFlowNameFile).Dimensions;
          GwfGwfReader.FDimensions2 := (Model2.FName as TFlowNameFile).Dimensions;
          ExcPackage.Package := GwfGwfReader;
          ExcPackage.ReadPackage(FOutFile, NPER);
        end
        else if ExcPackage.FileType = 'GWT6-GWT6' then
        begin
          GwtGwtReader := TGwtGwt.Create(FPackageType);
          GwtGwtReader.Dimensions := (Model1.FName as TTransportNameFile).Dimensions;
          GwtGwtReader.FDimensions2 := (Model2.FName as TTransportNameFile).Dimensions;
          ExcPackage.Package := GwtGwtReader;
          ExcPackage.ReadPackage(FOutFile, NPER);
        end
        else if ExcPackage.FileType = 'GWF6-GWT6' then
        begin
          // As of MODFLOW 6.4.2,
          // a file exists but nothing is read from the file.
          // Therefore, nothing is done here.
        end
        else
        begin
          FOutFile.WriteLine('Unrecognized groundwater exchange type.');
          FOutFile.WriteLine(ExcPackage.FileType);
        end;

      end;

      FSimulationFile.Close;
      FOutFile.Close;
    finally
      FSimulationFile.Free;
      FOutFile.Free;
    end;

    except on E: Exception do
    begin
      WriteLn('ERROR');
      WriteLn(E.Message);
    end;
  end;
end;

{ TSimulationOptions }

procedure TSimulationOptions.Initialize;
begin
  inherited;
  FContinueOption := False;
  FMaxErrors := -1;
  FMemPrint := mpNone;
  FNoCheckOption := False;
  FPrintInputOption := False;
end;

procedure TSimulationOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  AValue: string;
  IntValue: Integer;
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

    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      Continue;
    end;

    AValue := FSplitter[0];

    if AValue = 'CONTINUE' then
    begin
      ContinueOption := True;
    end
    else if AValue = 'NOCHECK' then
    begin
      NoCheckOption := True;
    end
    else if AValue = 'MEMORY_PRINT_OPTION' then
    begin
      AValue := FSplitter[1];
      if AValue = 'NONE' then
      begin
        MemPrint := mpNone;
      end
      else if AValue = 'SUMMARY' then
      begin
        MemPrint := mpSummary;
      end
      else if AValue = 'ALL' then
      begin
        MemPrint := mpAll;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized MEMORY_PRINT_OPTION in ');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if AValue = 'MAXERRORS' then
    begin
      AValue := FSplitter[1];
      if TryStrToInt(AValue, IntValue) then
      begin
        MaxErrors := IntValue;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized MAXERRORS in ');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if AValue = 'PRINT_INPUT' then
    begin
      PrintInputOption := True;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized simulation option in ');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

procedure TSimulationOptions.SetContinueOption(const Value: Boolean);
begin
  FContinueOption := Value;
end;

procedure TSimulationOptions.SetMaxErrors(const Value: Integer);
begin
  FMaxErrors := Value;
end;

procedure TSimulationOptions.SetMemPrint(const Value: TMemPrint);
begin
  FMemPrint := Value;
end;

procedure TSimulationOptions.SetNoCheckOption(const Value: Boolean);
begin
  FNoCheckOption := Value;
end;

procedure TSimulationOptions.SetPrintInputOption(const Value: Boolean);
begin
  FPrintInputOption := Value;
end;

{ TTiming }

destructor TTiming.Destroy;
begin
  FTDis.Free;
  inherited;
end;

procedure TTiming.Initialize;
begin
  inherited;
  FTisFileName := ''
end;

procedure TTiming.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'TIMING', Unhandled) then
    begin
      Exit
    end;


    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'TIMING') then
    begin
      // do nothing
    end
    else if Pos('TDIS6', UpperCase(ALine)) = 1 then
    begin
      ALine := Trim(Copy(ALine, Length('TDIS6')+1, MaxInt));
      FSplitter.DelimitedText := ALine;
      ALine := FSplitter[0];
      if ALine[1] = '''' then
      begin
        Assert(ALine[Length(ALine)] = '''');
        ALine := Copy(ALine, 2, Length(ALine) -2);
      end;
      FTisFileName := ALine;
      Continue;
    end
    else
    begin
      Unhandled.WriteLine('Error reading the following timing line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

procedure TTiming.ReadInput(Unhandled: TStreamWriter);
var
  TDisFile: TStreamReader;
begin
  if TFile.Exists(FTisFileName) then
  begin
    try
      TDisFile := TFile.OpenText(FTisFileName);
      try
        try
          FTDis := TTDis.Create('DIS6');
          FTDis.Read(TDisFile, Unhandled);
          FTDis.ReadInput(Unhandled);
        except on E: Exception do
          begin
            Unhandled.WriteLine('ERROR');
            Unhandled.WriteLine(E.Message);
          end;
        end;
      finally
        TDisFile.Free;
      end;
    except on E: Exception do
      begin
        Unhandled.WriteLine('ERROR');
        Unhandled.WriteLine(E.Message);
      end;
    end;
  end
  else
  begin
    Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
      [FTisFileName]));
  end;
end;

{ TModels }

constructor TModels.Create(PackageType: string);
begin
  FModels := TObjectModelList.Create;
  inherited
end;

destructor TModels.Destroy;
begin
  FModels.Free;
  inherited;
end;

function TModels.GetCount: Integer;
begin
  result := FModels.Count;
end;

function TModels.GetModel(Index: Integer): TModel;
begin
  result := FModels[Index];
end;

function TModels.GetModelByName(ModelName: string): TModel;
var
  Index: Integer;
  AModel: TModel;
begin
  result := nil;
  for Index := 0 to FModels.Count - 1 do
  begin
    AModel := FModels[Index];
    if AnsiSameText(ModelName, AModel.ModelName) then
    begin
      result := AModel;
      Exit;
    end;
  end;
end;

function TModels.GetModelByNameFile(NameFile: string): TModel;
var
  Index: Integer;
  AModel: TModel;
begin
  result := nil;
  for Index := 0 to FModels.Count - 1 do
  begin
    AModel := FModels[Index];
    if AnsiSameText(NameFile, AModel.NameFile) then
    begin
      result := AModel;
      Exit;
    end;
  end;
end;

procedure TModels.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  AModel: TModel;
  SectionName: string;
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

    FSplitter.DelimitedText := ALine;

    SectionName := 'MODELS';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;


    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      AModel := TModel.Create;
      FModels.Add(AModel);
      AModel.ModelType := UpperCase(FSplitter[0]);
      AModel.NameFile := FSplitter[1];
      AModel.ModelName := FSplitter[2];
      ALine := UpperCase(FSplitter[0]);
      if (ALine <> 'GWF6') and (ALine <> 'GWT6') then
      begin
        Unhandled.WriteLine('Error reading the model type in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Error reading the following model line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

procedure TModels.ReadInput(Unhandled: TStreamWriter; const NPER: integer);
var
  ModelIndex: Integer;
begin
  for ModelIndex := 0 to FModels.Count - 1 do
  begin
    FModels[ModelIndex].ReadInput(Unhandled, NPER);
  end;
end;

{ TExchanges }

constructor TExchanges.Create(PackageType: string);
begin
  FExchanges := TExchangeList.Create;
  inherited;

end;

destructor TExchanges.Destroy;
begin
  FExchanges.Free;
  inherited;
end;

function TExchanges.GetCount: Integer;
begin
  result := FExchanges.Count;
end;

function TExchanges.GetExchange(Index: Integer): TExchange;
begin
  result := FExchanges[Index];
end;

procedure TExchanges.Initialize;
begin
  inherited;
  FExchanges.Clear;
end;

procedure TExchanges.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  AnExchange: TExchange;
  SectionName: string;
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

    FSplitter.DelimitedText := ALine;

    SectionName := 'EXCHANGES';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 4 then
    begin
      AnExchange := TExchange.Create;
      FExchanges.Add(AnExchange);
      AnExchange.ExchangeType := UpperCase(FSplitter[0]);
      AnExchange.ExchangeFile := FSplitter[1];
      AnExchange.ExchangeModelNameA := FSplitter[2];
      AnExchange.ExchangeModelNameB := FSplitter[3];
      ALine := UpperCase(FSplitter[0]);
      if (ALine <> 'GWF6-GWF6') and (ALine <> 'GWF6-GWT6') and (ALine <> 'GWT6-GWT6') then
      begin
        Unhandled.WriteLine('Error reading the exchange type in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Error reading the following exchange line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSolution }

constructor TSolution.Create;
begin
  inherited;
  FSolutionModelNames := TStringList.Create;
end;

destructor TSolution.Destroy;
begin
  FIms.Free;
  FSolutionModelNames.Free;
  inherited;
end;

procedure TSolution.ReadInput(Unhandled: TStreamWriter);
var
  ImsFileStream: TStreamReader;
begin
  FreeAndNil(FIms);
  if TFile.Exists(SolutionFileName) then
  begin
    try
      ImsFileStream := TFile.OpenText(SolutionFileName);
      try
        FIms := TIms.Create('IMS');
        FIms.Read(ImsFileStream, Unhandled);
      finally
        ImsFileStream.Free;
      end;
    except on E: Exception do
      begin
        Unhandled.WriteLine('ERROR');
        Unhandled.WriteLine(E.Message);
      end;
    end;
  end
  else
  begin
    Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
      [SolutionFileName]));
  end;
end;

{ TSolutionGroup }

constructor TSolutionGroup.Create(SolutionNumber: Integer; PackageType: string);
begin
  FSolutionNumber := SolutionNumber;
  FSolutions := TSolutionList.Create;
  inherited Create(PackageType);
end;

destructor TSolutionGroup.Destroy;
begin
  FSolutions.Free;
  inherited;
end;

function TSolutionGroup.GetCount: Integer;
begin
  result := FSolutions.Count;
end;

function TSolutionGroup.GetSolutions(Index: Integer): TSolution;
begin
  result := FSolutions[Index];
end;

procedure TSolutionGroup.Initialize;
begin
  inherited;
  FSolutions.Clear;
  FMxiter := 1;
end;

procedure TSolutionGroup.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  ASolution: TSolution;
  ModelIndex: Integer;
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

    FSplitter.DelimitedText := ALine;

    SectionName := 'SOLUTIONGROUP';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
    begin
      if UpperCase(FSplitter[0]) = 'MXITER' then
      begin
        if not TryStrToInt(FSplitter[1], FMxiter) then
        begin
          Unhandled.WriteLine('Invalid value for MXITER in solution group in the following line');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter.Count >= 3 then
      begin
        ASolution := TSolution.Create;
        FSolutions.Add(ASolution);
        ASolution.SolutionType := UpperCase(FSplitter[0]);
        ASolution.SolutionFileName := FSplitter[1];
        ASolution.FSolutionModelNames.Capacity := FSplitter.Count-2;
        for ModelIndex := 2 to FSplitter.Count - 1 do
        begin
          ASolution.FSolutionModelNames.Add(FSplitter[ModelIndex]);
        end;
        if ASolution.SolutionType <> 'IMS6' then
        begin
          Unhandled.WriteLine(Format('Unrecognized solution type option "%s" in the following line',
            [ASolution.SolutionType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized solution group option in the following line');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized solution group option in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

procedure TSolutionGroup.ReadInput(Unhandled: TStreamWriter);
var
  SolutionIndex: Integer;
begin
  for SolutionIndex := 0 to FSolutions.Count - 1 do
  begin
    FSolutions[SolutionIndex].ReadInput(Unhandled);
  end;
end;

procedure TSolutionGroup.SetSolutions(Index: Integer; const Value: TSolution);
begin

end;

{ TModel }

destructor TModel.Destroy;
begin
  FName.Free;
  inherited;
end;

function TModel.GetFullBudgetFileName: string;
var
  FlowNameFile: TFlowNameFile;
  TransportNameFile: TTransportNameFile;
  PackageIndex: Integer;
  APackage: TPackage;
begin
  if ModelType = 'GWF6' then
  begin
    FlowNameFile := FName as TFlowNameFile;
    for PackageIndex := 0 to FlowNameFile.NfPackages.Count - 1 do
    begin
      APackage := FlowNameFile.NfPackages[PackageIndex];
      if APackage.FileType = 'OC6' then
      begin
        result := (APackage.Package as TOc).FullBudgetFileName;
      end;
    end;
  end
  else if ModelType = 'GWT6' then
  begin
    TransportNameFile := FName as TTransportNameFile;
    for PackageIndex := 0 to TransportNameFile.NfPackages.Count - 1 do
    begin
      APackage := TransportNameFile.NfPackages[PackageIndex];
      if APackage.FileType = 'FMI6' then
      begin
        result := (APackage.Package as TFmi).FullBudgetFileName;
      end;
    end;
  end
  else
  begin
    Assert(False)
  end;
end;

procedure TModel.ReadInput(Unhandled: TStreamWriter; const NPER: integer);
var
  NameFileStream: TStreamReader;
begin
  FreeAndNil(FName);
  if TFile.Exists(NameFile) then
  begin
    try
      NameFileStream := TFile.OpenText(NameFile);
      try
        if ModelType = 'GWF6' then
        begin
          FName := TFlowNameFile.Create(ModelType);
        end
        else if ModelType = 'GWT6' then
        begin
          FName := TTransportNameFile.Create(ModelType);
        end
        else
        begin
          Unhandled.WriteLine(Format('Unable read unrecognized file type %s', [ModelType]));
        end;
        if FName <> nil then
        begin
          FName.Read(NameFileStream, Unhandled);
          FName.ReadInput(Unhandled, NPER);
        end;
      finally
        NameFileStream.Free;
      end;
    except on E: Exception do
      begin
        Unhandled.WriteLine('ERROR');
        Unhandled.WriteLine(E.Message);
      end;
    end
  end
  else
  begin
    Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
      [NameFile]));
  end;
end;

end.
