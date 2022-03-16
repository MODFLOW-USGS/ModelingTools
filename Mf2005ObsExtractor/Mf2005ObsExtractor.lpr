program Mf2005ObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ReadMnwiOutput, RealListUnit,
  InterpolatedObsResourceUnit, DisclaimerTextUnit, readinstructions,
  readgageoutput, ObExtractorTypes, ReadNameFile, SimpleTextWriter,
  SubsidenceObsExtractor, SwiObsUtilities, SwiOutputReaderUnit,
  SwiObsReaderUnit, SwtObsExtractor;

type

  TMf2005ObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMf2005ObsExtractor }

procedure TMf2005ObsExtractor.DoRun;
var
  ErrorMsg: String;
  FileName: string;
  P: PChar;
  Opts: TStringList;
  NonOpts: TStringList;
  NameFileReader: TNameFileReader;
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

      NameFileReader := TNameFileReader.Create;
      try
        try
          NameFileReader.ReadNameFile(FileName);
          NameFileReader.RunScripts;

        Except on E: Exception do
          begin
            WriteLn(E.message);
          end;
        end;
      finally
        NameFileReader.Free;
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

constructor TMf2005ObsExtractor.Create(TheOwner: TComponent);
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

destructor TMf2005ObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TMf2005ObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h', ' Displays this help message');
  writeln('Usage: ', ExeName, ' -f <filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' --file=<filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' <filename>', ' processes the filename indicated by <filename>');

end;

var
  Application: TMf2005ObsExtractor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;

{$R *.res}

begin
  StartTime := Now;
  Application:=TMf2005ObsExtractor.Create(nil);
  try
    Application.Title:='Mf2005ObsExtractor';
    Application.Run;
  finally
    Application.Free;
  end;
  ElapsedTime := Now - StartTime;
  writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
end.

