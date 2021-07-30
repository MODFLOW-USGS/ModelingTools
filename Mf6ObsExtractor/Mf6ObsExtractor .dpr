program Mf6ObsExtractor ;

{$APPTYPE CONSOLE}

//{$R *.res}

uses
  Classes,
  System.SysUtils,
  custominputreader in 'custominputreader.pas',
  customoutputfilereader in 'customoutputfilereader.pas',
  inputfilereader in 'inputfilereader.pas',
  outputfilereader in 'outputfilereader.pas',
  FastGEO in '..\ModelMuse\FastGEO.pas',
  BasisFunctionUnit in '..\ModelMuse\BasisFunctionUnit.pas',
  SubPolygonUnit in '..\ModelMuse\SubPolygonUnit.pas';

type
  TMf6ObsExtractor = class(TComponent)
  protected
    procedure DoRun; {$IFDEF FPC} override; {$endif}
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


{ TMf6ObsExtractor }

constructor TMf6ObsExtractor.Create;
begin
//  StopOnException:=True;
end;

destructor TMf6ObsExtractor.Destroy;
begin

  inherited;
end;

procedure TMf6ObsExtractor.DoRun;
var
//  ErrorMsg: String;
  InputHandler : TInputHandler;
  FileName: string;
  P: PChar;
//  Opts: TStringList;
//  NonOpts: TStringList;
  //I: integer;
begin
//  Opts := TStringList.Create;
//  NonOpts := TStringList.Create;
//  try
//    ErrorMsg := '';
//    ErrorMsg:=CheckOptions('hf:', ['help', 'file'], Opts, NonOpts);
//    if ErrorMsg<>'' then begin
//      raise Exception.Create(ErrorMsg);
//      Terminate;
//      Exit;
//    end;
    // quick check parameters

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
//      Terminate;
      Exit;
    end;

    FileName := '';
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
//      Terminate;
      Exit;
    end;
//  finally
//    Opts.Free;
//    NonOpts.Free;
//  end;

  { add your program here }

  // stop program loop
//  Terminate;
end;

procedure TMf6ObsExtractor.WriteHelp;
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
  Application: TMf6ObsExtractor;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  try
    StartTime := Now;
    Application:= TMf6ObsExtractor.Create(nil);
  //  Application.Title:='MODFLOW 6 Observation Extractor';
    Application.DoRun;
    Application.Free;
    ElapsedTime := Now - StartTime;
    writeln('Elapsed time: ' + TimeToStr(ElapsedTime));
  except on E: Exception do
    begin
      WriteLn(E.Message);
    end;

  end;
end.
