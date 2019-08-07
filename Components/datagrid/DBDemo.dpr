program DBDemo;

uses
  Forms,
  DBDemoUnit1 in 'DBDemoUnit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
