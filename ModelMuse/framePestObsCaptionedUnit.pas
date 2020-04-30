unit framePestObsCaptionedUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePestObsUnit, frameGridUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TframePestObsCaptioned = class(TframePestObs)
    pnlCaption: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePestObsCaptioned: TframePestObsCaptioned;

implementation

{$R *.dfm}

end.
