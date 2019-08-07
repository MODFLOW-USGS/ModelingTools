unit frmUpdateDataSetsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls;

type
  TfrmUpdateDataSets = class(TfrmCustomGoPhast)
    Label1: TLabel;
    btnUpdate: TButton;
    btnCreate: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function AskIfNewDataSet(var AlreadyAsked, PriorResponse: Boolean): boolean;

var
  frmUpdateDataSets: TfrmUpdateDataSets;

implementation

function AskIfNewDataSet(var AlreadyAsked, PriorResponse: Boolean): boolean;
begin
  if not AlreadyAsked then
  begin
    AlreadyAsked := True;
    frmUpdateDataSets := TfrmUpdateDataSets.Create(nil);
    try
      frmUpdateDataSets.ShowModal;
      PriorResponse := frmUpdateDataSets.ModalResult <> mrOK;
    finally
      frmUpdateDataSets.Free;
    end;
  end;
  result := PriorResponse;
end;

{$R *.dfm}

end.

