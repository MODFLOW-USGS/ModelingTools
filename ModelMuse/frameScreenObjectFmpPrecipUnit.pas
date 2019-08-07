unit frameScreenObjectFmpPrecipUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectFmpBoundaryUnit,
  Grids, RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask,
  JvExMask, JvSpin, ExtCtrls, ScreenObjectUnit, ModflowBoundaryUnit;

type
  TframeScreenObjectFmpPrecip = class(TframeScreenObjectFmpBoundary)
  private
    { Private declarations }
  protected
    function GetBoundary(AScreenObject: TScreenObject): TModflowBoundary; override;
    procedure CreateScreenObjectBoundary(AScreenObject: TScreenObject); override;
    function CreateNewBoundary: TModflowBoundary; override;
    procedure InitializeControls; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectFmpPrecip: TframeScreenObjectFmpPrecip;

implementation

uses
  ModflowFmpPrecipitationUnit;

resourcestring
  StrPrecipitationPFLX = 'Precipitation (PFLX)';

{$R *.dfm}

{ TframeScreenObjectFmpPrecip }

function TframeScreenObjectFmpPrecip.CreateNewBoundary: TModflowBoundary;
begin
  result := TFmpPrecipBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectFmpPrecip.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFarmPrecip;
end;

function TframeScreenObjectFmpPrecip.GetBoundary(
  AScreenObject: TScreenObject): TModflowBoundary;
begin
  result := AScreenObject.ModflowFmpPrecip;
end;

procedure TframeScreenObjectFmpPrecip.InitializeControls;
begin
  inherited;
  rdgModflowBoundary.Cells[Ord(pcValue), 0] := StrPrecipitationPFLX;
end;

end.
