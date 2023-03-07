unit frameScreenObjectFmp4AddedDemandUnit;
// frameScreenObjectFmp4AddedDemand

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4AddedDemand = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4AddedDemand: TframeScreenObjectFmp4AddedDemand;

implementation

uses
  ModflowFmp4AddedDemandUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4AddedDemand }

function TframeScreenObjectFmp4AddedDemand.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4AddedDemandBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4AddedDemand.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4AddedDemand;
end;

function TframeScreenObjectFmp4AddedDemand.GetValueDescription: string;
begin
  result := TFmp4AddedDemandBoundary.ValueDescription;
end;

end.
