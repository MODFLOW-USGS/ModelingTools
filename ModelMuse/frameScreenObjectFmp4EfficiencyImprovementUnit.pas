unit frameScreenObjectFmp4EfficiencyImprovementUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4EfficiencyImprovement = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4EfficiencyImprovement: TframeScreenObjectFmp4EfficiencyImprovement;

implementation

uses
  ModflowFmp4EfficiencyImprovementUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4EfficiencyImprovement }

function TframeScreenObjectFmp4EfficiencyImprovement.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4EfficiencyImprovementBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4EfficiencyImprovement.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.Fmp4EfficiencyImprovementBoundary
end;

function TframeScreenObjectFmp4EfficiencyImprovement.GetValueDescription: string;
begin
  result := TFmp4EfficiencyImprovementBoundary.ValueDescription;
end;

end.
