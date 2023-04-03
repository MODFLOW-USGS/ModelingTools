program Mf2005ObsExtractor;

{$APPTYPE CONSOLE}

{$R *.res}

{#BACKUP Mf2005ObsExtractor.lpr}
{#BACKUP Mf2005ObsExtractor.lpi}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Classes,
  System.SysUtils,
  obextractortypes in 'obextractortypes.pas',
  readgageoutput in 'readgageoutput.pas',
  readinstructions in 'readinstructions.pas',
  readmnwioutput in 'readmnwioutput.pas',
  readnamefile in 'readnamefile.pas',
  subsidenceobsextractor in 'subsidenceobsextractor.pas',
  SwiObsUtilities in 'SwiObsUtilities.pas',
  swioutputreaderunit in 'swioutputreaderunit.pas',
  swtobsextractor in 'swtobsextractor.pas',
  RealListUnit in '..\ModelMuse\RealListUnit.pas',
  SwiObsReaderUnit in '..\SWI_ObsExtractor\SwiObsReaderUnit.pas',
  ReadModflowArrayUnit in '..\ModelMuse\ReadModflowArrayUnit.pas',
  InterpolatedObsResourceUnit in '..\ModelMuse\InterpolatedObsResourceUnit.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

type
TMf2005ObsExtractor = class(TComponent)
  protected
    procedure DoRun;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

function HasOption(Const C: Char; Const S : String): Boolean;
var
  ParamIndex: Integer;
  Param: string;
//  OptionIndex: Integer;
  Option: string;
begin
  result := False;
  Option := '-' + C;
  for ParamIndex := 1 to ParamCount do
  begin
    Param := ParamStr(ParamIndex);
    if SameText(Param, Option) then
    begin
      result := True;
      Exit;
    end;
  end;
  Option := '--' + S;
  for ParamIndex := 1 to ParamCount - 1 do
  begin
    Param := ParamStr(ParamIndex);
    if SameText(Param, Option) then
    begin
      result := True;
      Exit;
    end;
  end;
end;

function GetOptionValue(Const C: Char; Const S : String): string;
var
  ParamIndex: Integer;
  Param: string;
//  OptionIndex: Integer;
  Option: string;
begin
  result := '';
  Option := '-' + C;
  for ParamIndex := 1 to ParamCount-1 do
  begin
    Param := ParamStr(ParamIndex);
    if SameText(Param, Option) then
    begin
      result := ParamStr(ParamIndex+1);
      Exit;
    end;
  end;
  Option := '--' + S;
  for ParamIndex := 1 to ParamCount - 1 do
  begin
    Param := ParamStr(ParamIndex);
    if SameText(Param, Option) then
    begin
      result := ParamStr(ParamIndex+1);
      Exit;
    end;
  end;
end;

{ TMf2005ObsExtractor }

procedure TMf2005ObsExtractor.DoRun;
var
  FileName: string;
  P: PChar;
  NameFileReader: TNameFileReader;
begin
    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Exit;
    end;

    FileName := GetOptionValue('f', 'file');
    if (FileName = '') and (ParamCount >= 1) then
    begin
      FileName := ParamStr(1);
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
      Exit;
    end;
  { add your program here }

  // stop program loop
//  Terminate;
end;

constructor TMf2005ObsExtractor.Create(TheOwner: TComponent);
var
  LineIndex: Integer;
begin
  inherited Create(TheOwner);
//  StopOnException:=True;
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
var
  ExeName: string;
begin
  ExeName := ParamStr(0);
  writeln('Usage: ', ExeName, ' -h', ' Displays this help message');
  writeln('Usage: ', ExeName, ' -f <filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' --file=<filename>', ' processes the filename indicated by <filename>');
  writeln('Usage: ', ExeName, ' <filename>', ' processes the filename indicated by <filename>');

end;

var
  Application: TMf2005ObsExtractor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  try
    StartTime := Now;
    Application:=TMf2005ObsExtractor.Create(nil);
    try
//      Application.Title:='MF2005_ObsExtractor';
      Application.DoRun;
    finally
      Application.Free;
    end;
    ElapsedTime := Now - StartTime;
    writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
