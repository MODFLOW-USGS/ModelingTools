unit frameScreenObjectFmp4BarePrecipitationConsumptionFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4BarePrecipitationConsumptionFraction = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4BarePrecipitationConsumptionFraction: TframeScreenObjectFmp4BarePrecipitationConsumptionFraction;

implementation

uses
  ModflowFmp4BarePrecipitationConsumptionFractionUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4BarePrecipitationConsumptionFraction }

function TframeScreenObjectFmp4BarePrecipitationConsumptionFraction.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4BarePrecipitationConsumptionFractionBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4BarePrecipitationConsumptionFraction.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.Fmp4BarePrecipitationConsumptionFractionBoundary
end;

function TframeScreenObjectFmp4BarePrecipitationConsumptionFraction.GetValueDescription: string;
begin
  result := TFmp4BarePrecipitationConsumptionFractionBoundary.ValueDescription;
end;

end.
