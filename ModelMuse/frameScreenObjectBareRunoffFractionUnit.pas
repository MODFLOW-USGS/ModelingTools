unit frameScreenObjectBareRunoffFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectBareRunoffFraction = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectBareRunoffFraction: TframeScreenObjectBareRunoffFraction;

implementation

uses
  ModflowFmp4BareRunoffFractionUnit;

{$R *.dfm}

{ TframeScreenObjectBareRunoffFraction }

function TframeScreenObjectBareRunoffFraction.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4BareRunoffFractionBoundary.Create(nil, nil);
end;

function TframeScreenObjectBareRunoffFraction.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.Fmp4BareRunoffFractionBoundary
end;

function TframeScreenObjectBareRunoffFraction.GetValueDescription: string;
begin
  result := TFmp4BareRunoffFractionBoundary.ValueDescription;
end;

end.
