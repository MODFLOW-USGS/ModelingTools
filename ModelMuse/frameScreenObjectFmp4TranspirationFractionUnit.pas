unit frameScreenObjectFmp4TranspirationFractionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4TranspirationFraction = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4TranspirationFraction: TframeScreenObjectFmp4TranspirationFraction;

implementation

uses
  ModflowFmp4TranspirationFractionUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4Boundary1 }

function TframeScreenObjectFmp4TranspirationFraction.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4TranspirationFractionBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4TranspirationFraction.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4TranspirationFraction;
end;

function TframeScreenObjectFmp4TranspirationFraction.GetValueDescription: string;
begin
  result := TFmp4TranspirationFractionBoundary.ValueDescription;
end;

end.
