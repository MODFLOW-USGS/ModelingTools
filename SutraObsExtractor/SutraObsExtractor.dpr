program SutraObsExtractor;

{$APPTYPE CONSOLE}

{$R *.res}

{#BACKUP SutraObsExtractor.lpr}
{#BACKUP SutraObsExtractor.lpi}

uses
  Classes,
  System.SysUtils,
  sutrainputfilereader in 'sutrainputfilereader.pas',
  sutraoutputfilereader in 'sutraoutputfilereader.pas',
  custominputreader in '..\Mf6ObsExtractor\custominputreader.pas',
  customoutputfilereader in '..\Mf6ObsExtractor\customoutputfilereader.pas',
  BasisFunctionUnit in '..\ModelMuse\BasisFunctionUnit.pas',
  FastGEO in '..\ModelMuse\FastGEO.pas',
  SubPolygonUnit in '..\ModelMuse\SubPolygonUnit.pas',
  RbwParser in '..\Components\RbwParser\RbwParser.pas',
  DisclaimerTextUnit in '..\ModelMuse\DisclaimerTextUnit.pas';

type
  TSutraObsExtractor = class(TComponent)
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


{ TSutraObsExtractor }

procedure TSutraObsExtractor.DoRun;
var
//  ErrorMsg: String;
  InputHandler : TSutraInputHandler;
  FileName: string;
  P: PChar;
//  Opts: TStringList;
//  NonOpts: TStringList;
begin
//  Opts := TStringList.Create;
//  NonOpts := TStringList.Create;
//  try
//    // quick check parameters
//    ErrorMsg:=CheckOptions('hf:', ['help', 'file:'], Opts, NonOpts);
//    if ErrorMsg<>'' then begin
//      ShowException(Exception.Create(ErrorMsg));
//      Terminate;
//      Exit;
//    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
//      Terminate;
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
//        Terminate;
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
//      Terminate;
      Exit;
    end;

  { add your program here }
//  finally
//    Opts.Free;
//    NonOpts.Free;
//  end;

  // stop program loop
//  Terminate;
end;

constructor TSutraObsExtractor.Create(TheOwner: TComponent);
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

destructor TSutraObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TSutraObsExtractor.WriteHelp;
var
  ExeName: string;
begin
  ExeName := ParamStr(0);
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
  try
    StartTime := Now;
    Application:= TSutraObsExtractor.Create(nil);
  //  Application.Title:='MODFLOW 6 Observation Extractor';
    try
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
