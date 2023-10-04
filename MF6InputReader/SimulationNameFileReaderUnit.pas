unit SimulationNameFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

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
  protected
    procedure Initialize; override;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TModel = class(TObject)
    ModelType: string;
    NameFile: string;
    ModelName: string;
  end;

  TModelList = TObjectList<TModel>;

  TModels = class(TCustomMf6Persistent)
  private
    FModels: TModelList;
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TMf6Simulation = class(TCustomMf6Persistent)
  private
    FSimulationOptions: TSimulationOptions;
    FTiming: TTiming;
    FModels: TModels;
    FSimulationFile: TStreamReader;
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadSimulation(NameFile: string);
  end;

implementation



{ TMf6Simulation }

constructor TMf6Simulation.Create;
begin
  inherited;
  FSimulationOptions := TSimulationOptions.Create;
  FTiming := TTiming.Create;
  FModels := FModels.Create;
end;

destructor TMf6Simulation.Destroy;
begin
  FModels.Free;
  FTiming.Free;
  FSimulationOptions.Free;
  FSimulationFile.Free;
  inherited;
end;

procedure TMf6Simulation.Initialize;
begin

end;

procedure TMf6Simulation.ReadSimulation(NameFile: string);
var
  FOutFile: TStreamWriter;
  ALine: string;
begin
  FSimulationFile := TFile.OpenText(NameFile);
  FOutFile := TFile.CreateText(ChangeFileExt(NameFile, '.lst'));
  try
    while not FSimulationFile.EndOfStream do
    begin
      ALine := FSimulationFile.ReadLine;
      ALine := StripFollowingComments(Trim(ALine));
      if ALine = '' then
      begin
        Continue;
      end;

      if Pos('BEGIN', ALine) = 1 then
      begin
        ALine := Trim(Copy(ALine, 6, MaxInt)) ;
        if Pos('OPTIONS', ALine) = 1 then
        begin
          FSimulationOptions.Read(FSimulationFile, FOutFile)
        end;

        if Pos('TIMING', ALine) = 1 then
        begin
          FTiming.Read(FSimulationFile, FOutFile)
        end;

        if Pos('MODELS', ALine) = 1 then
        begin
          FModels.Read(FSimulationFile, FOutFile);
        end;
      end;
    end;
  finally
    FSimulationFile.Free;
    FOutFile.Free;
  end;
end;

{ TSimulationOptions }

procedure TSimulationOptions.Initialize;
begin
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
  Splitter: TStringList;
  AValue: string;
  IntValue: Integer;
begin
  Initialize;
  Splitter := TStringList.Create;
  try
    Splitter.QuoteChar := '''';
    Splitter.Delimiter := ',';
    while not Stream.EndOfStream do
    begin
      ALine := Stream.ReadLine;
      ErrorLine := ALine;
      ALine := StripFollowingComments(ALine);
      ALine := Trim(ALine);
      if ALine = '' then
      begin
        Continue;
      end;
      ALine := UpperCase(ALine);
      Splitter.DelimitedText := ALine;
      AValue := Splitter[0];

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
        AValue := Splitter[1];
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
        AValue := Splitter[1];
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
      else if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
      begin
        Exit
      end;
    end;
  finally
    Splitter.Free;
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

procedure TTiming.Initialize;
begin
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
    ErrorLine := ALine;
    ALine := StripFollowingComments(Trim(ALine));
    if ALine = '' then
    begin
      Continue;
    end;

    if Pos('TDIS6', ALine) = 1 then
    begin
      ALine := Trim(Copy(ALine, 6, MaxInt));
      if ALine[1] = '''' then
      begin
        Assert(ALine[Length(ALine)] = '''');
        ALine := Copy(ALine, 2, Length(ALine) -2);
      end;
      FTisFileName := ALine;
      Continue;
    end
    else if ReadEndOfSection(ALine, ErrorLine, 'TIMING', Unhandled) then
    begin
      Exit
    end
    else
    begin
      Unhandled.WriteLine('Error reading the following timing line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TModels }

constructor TModels.Create;
begin
  FModels := TModelList.Create;
  inherited
end;

destructor TModels.Destroy;
begin
  FModels.Free;
  inherited;
end;

procedure TModels.Initialize;
begin
  inherited;

end;

procedure TModels.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  Splitter: TStringList;
  ErrorLine: string;
  AModel: TModel;
  SectionName: string;
begin
  Initialize;
  Splitter := TStringList.Create;
  try
    Splitter.QuoteChar := '''';
    Splitter.Delimiter := ',';
    while not Stream.EndOfStream do
    begin
      ALine := Stream.ReadLine;
      ErrorLine := ALine;
      ALine := StripFollowingComments(Trim(ALine));
      if ALine = '' then
      begin
        Continue;
      end;

      Splitter.DelimitedText := ALine;

      SectionName := 'MODELS';
      if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
      begin
        Exit;
      end;

      if Splitter.Count = 3 then
      begin
        AModel := TModel.Create;
        FModels.Add(AModel);
        AModel.ModelType := UpperCase(Splitter[0]);
        AModel.NameFile := Splitter[1];
        AModel.ModelName := Splitter[2];
        ALine := UpperCase(Splitter[0]);
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
  finally
    Splitter.Free;
  end;
end;

end.
