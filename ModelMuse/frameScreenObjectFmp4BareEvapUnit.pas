unit frameScreenObjectFmp4BareEvapUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4BareEvap = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4BareEvap: TframeScreenObjectFmp4BareEvap;

implementation

uses
  ModflowFmp4PotentialEvapBareUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4BareEvap }

function TframeScreenObjectFmp4BareEvap.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4BareEvapBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4BareEvap.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmpBareEvap;
end;

function TframeScreenObjectFmp4BareEvap.GetValueDescription: string;
begin
  result := TFmp4BareEvapBoundary.ValueDescription;
end;

end.
