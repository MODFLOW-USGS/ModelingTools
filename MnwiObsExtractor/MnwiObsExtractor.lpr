program MnwiObsExtractor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ReadMnwiOutput, RealListUnit,
  ReadMnwiInstructions;

type

  { TMnwiObsExtractor }

  TMnwiObsExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMnwiObsExtractor }

procedure TMnwiObsExtractor.DoRun;
var
  ErrorMsg: String;
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

  // stop program loop
  Terminate;
end;

constructor TMnwiObsExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMnwiObsExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure TMnwiObsExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMnwiObsExtractor;
begin
  Application:=TMnwiObsExtractor.Create(nil);
  Application.Title:='MnwiObsExtractor';
  Application.Run;
  Application.Free;
end.

