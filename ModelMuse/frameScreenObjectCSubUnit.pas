unit frameScreenObjectCSubUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectTabbedUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TframeScreenObjectCSub = class(TframeScreenObjectTabbed)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameScreenObjectCSub: TframeScreenObjectCSub;

implementation

{$R *.dfm}

end.
