unit framePackageFmp4Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TframePackageFmp4 = class(TframePackage)
    CategoryPanelGroup1: TCategoryPanelGroup;
    CategoryPanel1: TCategoryPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePackageFmp4: TframePackageFmp4;

implementation

{$R *.dfm}

end.
