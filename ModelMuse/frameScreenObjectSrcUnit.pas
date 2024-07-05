unit frameScreenObjectSrcUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ScreenObjectUnit, ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectSrc = class(TframeCustomGwtBoundary)
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
  protected
    function GetVariableName: string; override;
    function GetMultiplierName: string; override;
    function GetActiveName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
      override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectSrc: TframeScreenObjectSrc;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrMassLoading = 'Mass Loading';

{$R *.dfm}

{ TframeScreenObjectSrc }

procedure TframeScreenObjectSrc.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGwtSrcBoundary;
end;

function TframeScreenObjectSrc.GetActiveName: string;
begin
  result := StrMassLoading + ' Active';
end;

function TframeScreenObjectSrc.GetBoundary(
  ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GwtSrcBoundary;
end;

function TframeScreenObjectSrc.GetMultiplierName: string;
begin
  result := StrMassLoading + ' Multiplier';
end;

function TframeScreenObjectSrc.GetVariableName: string;
begin
  result := StrMassLoading;
end;

procedure TframeScreenObjectSrc.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if CanSelect  and (ACol = 4) then
  begin
    CanSelect := frmGoPhast.PhastModel.ModflowPackages.GwtSrcPackage.UseMultiplier;
  end;
end;

end.
