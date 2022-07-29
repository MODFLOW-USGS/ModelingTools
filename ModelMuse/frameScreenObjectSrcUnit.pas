unit frameScreenObjectSrcUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ScreenObjectUnit, ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectSrc = class(TframeCustomGwtBoundary)
  private
    { Private declarations }
  protected
    function GetVariableName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
      override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectSrc: TframeScreenObjectSrc;

implementation

resourcestring
  StrMassLoading = 'Mass Loading';

{$R *.dfm}

{ TframeScreenObjectSrc }

procedure TframeScreenObjectSrc.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGwtSrcBoundary;
end;

function TframeScreenObjectSrc.GetBoundary(
  ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GwtSrcBoundary;
end;

function TframeScreenObjectSrc.GetVariableName: string;
begin
  result := StrMassLoading;
end;

end.
