unit frameScreenObjectFmp4FractionOfIrrigToSurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater = class(TframeScreenObjectCustomFmp4Boundary)
  private
    { Private declarations }
  protected
    function GetValueDescription: string; override;
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4Boundary; override;
    function CreateBoundary: TFmp4Boundary; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectFmp4FractionOfIrrigToSurfaceWater: TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater;

implementation

uses
  ModflowFmp4FractionOfIrrigToSurfaceWaterUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4FractionOfIrrigToSurfaceWaterUnit }

function TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4FractionOfIrrigToSurfaceWaterBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4FractionOfIrrigToSurfaceWater;
end;

function TframeScreenObjectFmp4FractionOfIrrigToSurfaceWater.GetValueDescription: string;
begin
  result := TFmp4FractionOfIrrigToSurfaceWaterBoundary.ValueDescription;
end;

end.
