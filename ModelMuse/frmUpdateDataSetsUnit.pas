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

function AskIfNewDataSet(var AlreadyAsked: Boolean;
  var PriorResponse: TDatasetResponse): TDatasetResponse;

var
  frmUpdateDataSets: TfrmUpdateDataSets;

implementation

function AskIfNewDataSet(var AlreadyAsked: Boolean;
  var PriorResponse: TDatasetResponse): TDatasetResponse;
begin
  if not AlreadyAsked then
  begin
    AlreadyAsked := True;
    frmUpdateDataSets := TfrmUpdateDataSets.Create(nil);
    try
      frmUpdateDataSets.ShowModal;
      case frmUpdateDataSets.ModalResult of
        mrOK:
          begin
            PriorResponse := drUpdate;
          end;
        mrCancel:
          begin
            PriorResponse := drAbort;
          end;
        mrIgnore:
          begin
            PriorResponse := drNew
          end;
        else
          begin
            PriorResponse := drAbort;
          end;
      end;
    finally
      frmUpdateDataSets.Free;
    end;
  end;
  result := PriorResponse;
end;

{$R *.dfm}

end.

