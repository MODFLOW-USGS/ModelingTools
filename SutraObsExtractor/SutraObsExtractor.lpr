program SutraObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FastGEO, BasisFunctionUnit, SubPolygonUnit,
  DisclaimerTextUnit, CustApp, SutraInputFileReader, CustomInputReader,
  CustomOutputFileReader, SutraOutputFileReader, RbwParser;

type

  { TSutraObsExtractor }

  TSutraObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSutraObsExtractor }

procedure TSutraObsExtractor.DoRun;
var
  ErrorMsg: String;
  InputHandler : TSutraInputHandler;
  FileName: string;
  P: PChar;
  Opts: TStringList;
  NonOpts: TStringList;
begin
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  try
    // quick check parameters
    ErrorMsg:=CheckOptions('hf:', ['help', 'file:'], Opts, NonOpts);
    if ErrorMsg<>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    FileName := GetOptionValue('f', 'file');
    if (FileName = '') and (NonOpts.Count = 1) then
    begin
      FileName := NonOpts[0];
    end;
    if Pos('''', FileName) > 0 then
    begin
      P := PChar(FileName);
      FileName := AnsiExtractQuotedStr(P, '''');
    end
    else  if Pos('"', FileName) > 0 then
    begin
      P := PChar(FileName);
      FileName := AnsiExtractQuotedStr(P, '"');
    end;
    if FileName <> '' then
    begin
      if not FileExists(FileName) then
      begin
        WriteLn(FileName, ' was not found.');
        Terminate;
        Exit;
      end;
      WriteLn('Processing ', FileName);
      InputHandler := TSutraInputHandler.Create;
      try
        try
          InputHandler.ReadAndProcessInputFile(FileName);
          WriteLn('Normal termination');

        Except on E: Exception do
          begin
            WriteLn(E.message);
          end;
        end;
      finally
        InputHandler.Free;
      end;
    end
    else begin
      WriteHelp;
      Terminate;
      Exit;
    end;

  { add your program here }
  finally
    Opts.Free;
    NonOpts.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TSutraObsExtractor.Create(TheOwner: TComponent);
var
  LineIndex: Integer;
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  WriteLn;
  for LineIndex := 0 to Disclaimer.Count - 1 do
  begin
    WriteLn(Disclaimer[LineIndex]);
  end;
  WriteLn;
end;

destructor TSutraObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TSutraObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h', ' Displays this help message');
  writeln('Usage: ', ExeName, ' -f <filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' --file=<filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' <filename>', ' processes the filename indicated by <filename>');
end;

var
  Application: TSutraObsExtractor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  StartTime := Now;
  Application:=TSutraObsExtractor.Create(nil);
  try
  Application.Title:='SUTRA Observation Extractor';
    Application.Run;
  finally
    Application.Free;
  end;
  ElapsedTime := Now - StartTime;
  writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
end.

