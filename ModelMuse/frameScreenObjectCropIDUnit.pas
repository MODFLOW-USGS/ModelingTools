unit frameScreenObjectCropIDUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectFmpBoundaryUnit,
  Grids, RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask,
  JvExMask, JvSpin, ExtCtrls, ScreenObjectUnit, ModflowBoundaryUnit;

type
  TframeScreenObjectCropID = class(TframeScreenObjectFmpBoundary)
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
  frameScreenObjectCropID: TframeScreenObjectCropID;

implementation

uses
  ModflowFmpCropSpatialUnit, frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrCropID_ICID = 'Crop ID (ICID)';
  StrLandUseID = 'Land Use ID';

{$R *.dfm}

{ TframeScreenObjectCropID }

function TframeScreenObjectCropID.CreateNewBoundary: TModflowBoundary;
begin
  result := TFmpCropIDBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectCropID.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFarmCropID;
end;

function TframeScreenObjectCropID.GetBoundary(
  AScreenObject: TScreenObject): TModflowBoundary;
begin
  result := AScreenObject.ModflowFmpCropID;
end;

procedure TframeScreenObjectCropID.InitializeControls;
begin
  inherited;
  if frmGoPhast.ModelSelection = msModflowFmp then
  begin
    rdgModflowBoundary.Cells[Ord(pcValue), 0] := StrCropID_ICID;
  end
  else
  begin
    rdgModflowBoundary.Cells[Ord(pcValue), 0] := StrLandUseID;
  end;
end;

end.
