unit frameScreenObjectLandUseAreaFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectLandUseAreaFraction = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectLandUseAreaFraction: TframeScreenObjectLandUseAreaFraction;

implementation

uses
  ModflowFmp4LandUseAreaFractionUnit;

{$R *.dfm}

{ TframeScreenObjectLandUseAreaFraction }

function TframeScreenObjectLandUseAreaFraction.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4LandUseAreaFractionBoundary.Create(nil, nil);
end;

function TframeScreenObjectLandUseAreaFraction.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4LandUseAreaFraction;
end;

function TframeScreenObjectLandUseAreaFraction.GetValueDescription: string;
begin
  result := TFmp4LandUseAreaFractionBoundary.ValueDescription;
end;

end.
