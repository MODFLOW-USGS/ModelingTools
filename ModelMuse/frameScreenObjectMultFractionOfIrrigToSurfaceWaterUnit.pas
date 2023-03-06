unit frameScreenObjectMultFractionOfIrrigToSurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  frameScreenObjectCustomFmp4MultBoundaryUnit, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask, JvSpin,
  Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4LandUseBoundaryUnit;

type
  TframeScreenObjectMultFractionOfIrrigToSurfaceWater = class(TframeScreenObjectCustomFmp4MultBoundary)
  private
    { Private declarations }
  protected
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4LandUseBoundary; override;
    function CreateBoundary: TFmp4LandUseBoundary; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectMultFractionOfIrrigToSurfaceWater: TframeScreenObjectMultFractionOfIrrigToSurfaceWater;

implementation

uses
  ModflowFmp4FractionOfIrrigToSurfaceWaterUnit;

{$R *.dfm}

{ TframeScreenObjectMultFractionOfIrrigToSurfaceWater }

function TframeScreenObjectMultFractionOfIrrigToSurfaceWater.CreateBoundary: TFmp4LandUseBoundary;
begin
  result := TFmp4MultFractionOfIrrigToSurfaceWaterBoundary.Create(nil, nil);
end;

function TframeScreenObjectMultFractionOfIrrigToSurfaceWater.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4LandUseBoundary;
begin
  result := Item.ScreenObject.ModflowFmp4MultFractionOfIrrigToSurfaceWater;
end;

end.
