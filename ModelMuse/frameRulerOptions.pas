unit frameRulerOptions;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, clxDataEntry, QComCtrls;

type
  TFrame1 = class(TFrame)
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    ArgusDataEntry1: TArgusDataEntry;
    Label3: TLabel;
    Label4: TLabel;
    lblPreview: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
