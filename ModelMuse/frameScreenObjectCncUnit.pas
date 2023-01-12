unit frameScreenObjectCncUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ScreenObjectUnit,
  ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectCnc = class(TframeCustomGwtBoundary)
  private
  protected
    function GetVariableName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary; override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
    { Private declarations }
  end;

var
  frameScreenObjectCnc: TframeScreenObjectCnc;

implementation

uses
  GoPhastTypes, frmGoPhastUnit, ModflowTimeUnit,
  frmCustomGoPhastUnit;

resourcestring
  StrSpecifiedConcentrat = 'Specified Concentration';
{$R *.dfm}

{ TframeScreenObjectCnc }

procedure TframeScreenObjectCnc.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGwtCncBoundary;
end;

function TframeScreenObjectCnc.GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GwtCncBoundary;
end;

function TframeScreenObjectCnc.GetVariableName: string;
begin
  result := StrSpecifiedConcentrat;
end;

Initialization
  RegisterClass(TframeScreenObjectCnc);

end.
