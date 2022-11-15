program ReplaceHttp;

uses
  Vcl.Forms,
  frmReplaceHttpUnit in 'frmReplaceHttpUnit.pas' {frmReplaceHttpWithHttps};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmReplaceHttpWithHttps, frmReplaceHttpWithHttps);
  Application.Run;
end.
