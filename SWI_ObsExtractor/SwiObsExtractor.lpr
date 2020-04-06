program SwiObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, RealListUnit, SimpleTextWriter, SwiObsUtilities,
  SwiObsInputReader, SwiObsReaderUnit, DisclaimerTextUnit,
  InterpolatedObsResourceUnit, CustApp
  { you can add units after this };

type

  { TSwiObsExtractor }

  TSwiObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSwiObsExtractor }

procedure TSwiObsExtractor.DoRun;
var
  ErrorMsg: String;
  InputFile: string;
  Extractor: TSwiObservationExtractor;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
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

  { add your program here }
  try
    //ExtractSwiObservations;

    if ParamCount > 0 then
    begin
      InputFile := ParamStr(1)
    end
    else
    begin
      WriteLn('What is the name of the input file?');
      Readln(InputFile);
    end;
    if FileExists(InputFile) then
    begin
      Extractor := TSwiObservationExtractor.Create(InputFile);
      try

      finally
        Extractor.Free;
      end;
    end
    else
    begin
      raise Exception.Create(Format('The input file "%s" does not exist.', [InputFile]));
    end;


    Writeln('');
    Writeln('normal termination of ', ExtractFileName(ParamStr(0)));
    Writeln('');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  // stop program loop
  Terminate;
end;

constructor TSwiObsExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSwiObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TSwiObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TSwiObsExtractor;
begin
  Application:=TSwiObsExtractor.Create(nil);
  Application.Title:='SwiObsExtractor';
  Application.Run;
  Application.Free;
end.

