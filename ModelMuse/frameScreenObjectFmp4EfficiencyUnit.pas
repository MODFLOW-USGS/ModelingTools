unit frameScreenObjectFmp4EfficiencyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects,
  ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4Efficiency = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4Efficiency: TframeScreenObjectFmp4Efficiency;

implementation

uses
  ModflowFmp4EfficiencyUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4Efficiency }


function TframeScreenObjectFmp4Efficiency.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4EfficiencyBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4Efficiency.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.Fmp4EfficiencyBoundary
end;

function TframeScreenObjectFmp4Efficiency.GetValueDescription: string;
begin
  result := TFmp4EfficiencyBoundary.ValueDescription;
end;

end.
