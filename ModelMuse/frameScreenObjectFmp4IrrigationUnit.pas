unit frameScreenObjectFmp4IrrigationUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4Irrigation = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4Irrigation: TframeScreenObjectFmp4Irrigation;

implementation

uses
  ModflowFmp4IrrigationSpatialUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4Irrigation }

function TframeScreenObjectFmp4Irrigation.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4IrrigationBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4Irrigation.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4Irrigation;
end;

function TframeScreenObjectFmp4Irrigation.GetValueDescription: string;
begin
  result := TFmp4IrrigationBoundary.ValueDescription;
end;

end.
