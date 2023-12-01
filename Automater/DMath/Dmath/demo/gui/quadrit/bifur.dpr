program bifur;

uses
  Forms,
  bifur1 in 'bifur1.pas' {Form1},
  GraphOpt in '..\dialogs\graphopt.pas' {GraphOptDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TGraphOptDlg, GraphOptDlg);
  Application.Run;
end.
