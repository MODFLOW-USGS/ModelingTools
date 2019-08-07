unit frmAnisotropyUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, frmCustomGoPhastUnit, QButtons, clxDataEntry;

type
  TfrmAnisotropy = class(TfrmCustomGoPhast)
    adeAnisotropy: TArgusDataEntry;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAnisotropy: TfrmAnisotropy;

implementation

{$R *.dfm}

end.
