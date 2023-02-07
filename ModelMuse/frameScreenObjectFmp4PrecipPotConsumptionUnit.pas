unit frameScreenObjectFmp4PrecipPotConsumptionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4PrecipPotConsumption = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4PrecipPotConsumption: TframeScreenObjectFmp4PrecipPotConsumption;

implementation

uses
  ModflowFmp4PrecipPotConsumptionUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4PrecipPotConsumption }

function TframeScreenObjectFmp4PrecipPotConsumption.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4PrecipPotConsumptionBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4PrecipPotConsumption.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmpPrecipPotConsumption;
end;

function TframeScreenObjectFmp4PrecipPotConsumption.GetValueDescription: string;
begin
  result := TFmp4PrecipPotConsumptionBoundary.ValueDescription;
end;

end.
