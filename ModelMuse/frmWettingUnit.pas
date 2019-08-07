unit frmWettingUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ArgusDataEntry, StdCtrls, JvExStdCtrls,
  JvCheckBox, Buttons, ExtCtrls, Mask, JvExMask, JvSpin, JvCombobox, JvListComb;

type
  TfrmWetting = class(TfrmCustomGoPhast)
    cbWetting: TJvCheckBox;
    lblWetFact: TLabel;
    lblCheckDry: TLabel;
    lblWettingEquation: TLabel;
    adeWettingFact: TRbwDataEntry;
    comboWettingEquation: TJvImageComboBox;
    seCheckDry: TJvSpinEdit;
    Panel4: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWetting: TfrmWetting;

implementation

{$R *.dfm}

end.
