unit frameScreenObjectFmp4EvaporationIrrigationFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4EvaporationIrrigationFraction = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4EvaporationIrrigationFraction: TframeScreenObjectFmp4EvaporationIrrigationFraction;

implementation

uses
  ModflowFmp4EvaporationIrrigationFractionUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4EvaporationIrrigationFraction }

function TframeScreenObjectFmp4EvaporationIrrigationFraction.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4EvaporationIrrigationFractionBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4EvaporationIrrigationFraction.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4EvaporationIrrigationFraction;
end;

function TframeScreenObjectFmp4EvaporationIrrigationFraction.GetValueDescription: string;
begin
  result := TFmp4EvaporationIrrigationFractionBoundary.ValueDescription;
end;

end.
