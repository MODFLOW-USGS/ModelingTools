unit frmDuplicateNamesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls;

type
  TDupResponse = (drCancel, drNew, drAssignAndKeep, drAssignAndDelete);

  TfrmDuplicateNames = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    pnlTop: TPanel;
    lblTop: TLabel;
    memoDuplicateNames: TMemo;
    rgResponse: TRadioGroup;
    btn1: TBitBtn;
    procedure rgResponseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDuplicateNames: TfrmDuplicateNames;

  function DupNameTreatment(Lines: TStrings): TDupResponse;

implementation

{$R *.dfm}

function DupNameTreatment(Lines: TStrings): TDupResponse;
begin
  frmDuplicateNames := TfrmDuplicateNames.Create(nil);
  try
    frmDuplicateNames.memoDuplicateNames.Lines := Lines;
    frmDuplicateNames.ShowModal;
    if frmDuplicateNames.ModalResult = mrOk then
    begin
      result := TDupResponse(frmDuplicateNames.rgResponse.ItemIndex+1);
    end
    else
    begin
      result := drCancel;
    end;
  finally
    frmDuplicateNames.Free;
  end;
end;


procedure TfrmDuplicateNames.rgResponseClick(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := True;
end;

end.
