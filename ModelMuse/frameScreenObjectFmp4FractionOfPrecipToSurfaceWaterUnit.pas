unit frameScreenObjectFmp4FractionOfPrecipToSurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4FractionOfPrecipToSurfaceWater: TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater;

implementation

uses
  ModflowFmp4FractionOfPrecipToSurfaceWaterUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater }

function TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4FractionOfPrecipToSurfaceWaterBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4FractionOfPrecipToSurfaceWater;
end;

function TframeScreenObjectFmp4FractionOfPrecipToSurfaceWater.GetValueDescription: string;
begin
  result := TFmp4FractionOfPrecipToSurfaceWaterBoundary.ValueDescription;
end;

end.
