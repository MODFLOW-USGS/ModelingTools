program Mf6ObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, OutputFileReader, InputFileReader,
  CustomInputReader, CustomOutputFileReader, DisclaimerTextUnit;

type

  { TMf6ObsExtractor }

  TMf6ObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMf6ObsExtractor }

procedure TMf6ObsExtractor.DoRun;
var
  ErrorMsg: String;
  InputHandler : TInputHandler;
  FileName: string;
  P: PChar;
  Opts: TStringList;
  NonOpts: TStringList;
  //I: integer;
begin
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  try
    ErrorMsg:=CheckOptions('hf:', ['help', 'file:'], Opts, NonOpts);
    if ErrorMsg<>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;
    // quick check parameters

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
      InputHandler := TInputHandler.Create;
      try
        try
          InputHandler.ReadAndProcessInputFile(FileName);
          WriteLn('normal termination');
        Except on E: Exception do
          begin
            WriteLn('');
            WriteLn('ERROR');
            WriteLn(E.message);
            WriteLn('');
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
  finally
    Opts.Free;
    NonOpts.Free;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TMf6ObsExtractor.Create(TheOwner: TComponent);
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

destructor TMf6ObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TMf6ObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h', ' Displays this help message');
  writeln('Usage: ', ExeName, ' -f <filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' --file=<filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' <filename>', ' processes the filename indicated by <filename>');
end;

var
  Application: TMf6ObsExtractor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  StartTime := Now;
  Application:=TMf6ObsExtractor.Create(nil);
  try
    Application.Title:='MODFLOW 6 Observation Extractor';
    Application.Run;
  finally
    Application.Free;
  end;
  ElapsedTime := Now - StartTime;
  writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
end.

