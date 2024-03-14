program GetValuesFromArrayFiles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  ExtractValuesFromFileUnit, SimpleTextWriter, DisclaimerTextUnit;



type

  { TGetValuesFromArrayFiles }

  TGetValuesFromArrayFiles = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGetValuesFromArrayFiles }

procedure TGetValuesFromArrayFiles.DoRun;
var
  ErrorMsg: String;
  ArrayFileHandler: TArrayFileHandler;
  LineIndex: Integer;
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

  for LineIndex := 0 to Disclaimer.Count -1 do
  begin
    writeln(Disclaimer[LineIndex]);
  end;
  writeln;

  ArrayFileHandler := TArrayFileHandler.Create;
  try
    ArrayFileHandler.ConvertValues;
  finally
    ArrayFileHandler.Free;
  end;

  writeln('normal termination');
  // stop program loop
  Terminate;
end;

constructor TGetValuesFromArrayFiles.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGetValuesFromArrayFiles.Destroy;
begin
  inherited Destroy;
end;

procedure TGetValuesFromArrayFiles.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  Writeln('Usage: a command line parameters must be supplied.');
  Writeln('The parameter must be the name of the file defining the observations.');
  Writeln('The first line in that file is the name of the file from which values are to be extracted.');
  Writeln('The remaining lines list observation names followed by alternating index values (1-based) ');
  Writeln('in the array and weights associated with those values.');
  Writeln('The weights must be greater than or equal to zero');
  Writeln('and at least one weight should be greater than zero.');
  Writeln('For example:');
  Writeln;
  Writeln('MyFile.array');
  Writeln('Obs1 1 0.5 18 3.3, 7 0.2');
  Writeln('Obs2 16 0.4 25 2.1, 32 1.3');
end;

var
  Application: TGetValuesFromArrayFiles;
begin
  Application:=TGetValuesFromArrayFiles.Create(nil);
  Application.Title:='GetValuesFromArrayFiles';
  Application.Run;
  Application.Free;
end.

