unit SimulationNameFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils;

type
  TMemPrint = (mpNone, mpSummary, mpAll);

  TSimulationOptions = class(TPersistent)
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
    procedure Initialize;
  protected
    function StripFollowingComments(AValue: string): string;
  public
    constructor Create;
    property ContinueOption: Boolean read FContinueOption write SetContinueOption;
    property NoCheckOption: Boolean read FNoCheckOption write SetNoCheckOption;
    property MemPrint: TMemPrint read FMemPrint write SetMemPrint;
    property MaxErrors: Integer read FMaxErrors write SetMaxErrors;
    property PrintInputOption: Boolean read FPrintInputOption write SetPrintInputOption;
    procedure ReadOptions(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TMf6Simulation = class(TPersistent)
  private
    FSimulationOptions: TSimulationOptions;
    FSimulationFile: TStreamReader;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadSimulation(NameFile: string);
  end;

implementation



{ TMf6Simulation }

constructor TMf6Simulation.Create;
begin
  inherited;
  FSimulationOptions := TSimulationOptions.Create;
end;

destructor TMf6Simulation.Destroy;
begin
  FSimulationFile.Free;
  FSimulationOptions.Free;
  inherited;
end;

procedure TMf6Simulation.ReadSimulation(NameFile: string);
begin
  FSimulationFile := TFile.OpenText(NameFile)
end;

{ TSimulationOptions }

constructor TSimulationOptions.Create;
begin
  Initialize;
end;

procedure TSimulationOptions.Initialize;
begin
  FContinueOption := False;
  FMaxErrors := -1;
  FMemPrint := mpNone;
  FNoCheckOption := False;
  FPrintInputOption := False;
end;

procedure TSimulationOptions.ReadOptions(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Splitter: TStringList;
  AValue: string;
  CommentPosition: Integer;
  IntValue: Integer;

begin
  Initialize;
  Splitter := TStringList.Create;
  try
    Splitter.QuoteChar := '''';
    Splitter.Delimiter := ',';
    repeat
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
      else if AValue = 'END' then
      begin
        AValue := Splitter[1];
        if AValue = 'OPTIONS' then
        begin
          Break;
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized option in ');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized option in ');
        Unhandled.WriteLine(ErrorLine);
      end;
      if Stream.EndOfStream then
      begin
        break;
      end;
    until False;
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

function TSimulationOptions.StripFollowingComments(AValue: string): string;
var
  CommentPosition: Integer;
  SingleQuotePostion: Integer;
  CharIndex: Integer;
  QuoteCount: Integer;
begin
  SingleQuotePostion := Pos('''', AValue);
  if SingleQuotePostion = 0 then
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Trim(Copy(AValue, 1, CommentPosition-1));
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Trim(Copy(AValue, 1, CommentPosition-1));
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Trim(Copy(AValue, 1, CommentPosition-1));
    end;
  end
  else
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '#' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex-1);
            Break;
          end;
        end;
      end;
    end;

    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '!' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex-1);
            Break;
          end;
        end;
      end;
    end;

    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if Copy(AValue, CharIndex, 2) = '//' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex-1);
            Break;
          end;
        end;
      end;
    end;
  end;

  result := AValue;
end;

end.
