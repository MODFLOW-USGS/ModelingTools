unit frmSutraTimeAdjustChoiceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls;

type
  TAdjustChoice = (acUseSchedule, acConvert);

  TfrmSutraTimeAdjustChoice = class(TfrmCustomGoPhast)
    lblMessage: TLabel;
    rgTimeTreatment: TRadioGroup;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
  private
    { Private declarations }
  public
    function AdjustChoice: TAdjustChoice;
    { Public declarations }
  end;

var
  frmSutraTimeAdjustChoice: TfrmSutraTimeAdjustChoice;

implementation

{$R *.dfm}

{ TfrmSutraTimeAdjustChoice }

function TfrmSutraTimeAdjustChoice.AdjustChoice: TAdjustChoice;
begin
  result := TAdjustChoice(rgTimeTreatment.ItemIndex);
end;

end.
