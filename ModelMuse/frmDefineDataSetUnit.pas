unit frmDefineDataSetUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, frmCustomGoPhastUnit, QExtCtrls, QButtons;

type
  TfrmDefineDataSet = class(TfrmCustomGoPhast)
    rgDataType: TRadioGroup;
    rgOrientation: TRadioGroup;
    edName: TEdit;
    lblName: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDefineDataSet: TfrmDefineDataSet;

implementation

{$R *.dfm}

end.
