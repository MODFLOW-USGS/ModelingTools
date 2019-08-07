unit frameTopUnit1;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  Replacement, QExtCtrls, QRbwRuler;

type
  TframeTop = class(TFrame)
    pnlTopLeft: TPanel;
    rulerTopNS: TRbwRuler;
    rulTopEW: TRbwRuler;
    Panel1: TPanel;
    zbTop: TQrbwZoomBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
