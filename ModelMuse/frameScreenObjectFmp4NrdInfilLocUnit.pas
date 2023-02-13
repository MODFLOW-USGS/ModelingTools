unit frameScreenObjectFmp4NrdInfilLocUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4IntBoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4IntBoundaryUnit;

type
  TframeScreenObjectFmp4NrdInfilLoc = class(TframeScreenObjectCustomFmp4IntBoundary)
  private
    { Private declarations }
  protected
    function GetValueDescription: string; override;
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4IntBoundary; override;
    function CreateBoundary: TFmp4IntBoundary; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectFmp4NrdInfilLoc: TframeScreenObjectFmp4NrdInfilLoc;

implementation

uses
  ModflowFmp4NrdInfilLocationUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4NrdInfilLoc }

function TframeScreenObjectFmp4NrdInfilLoc.CreateBoundary: TFmp4IntBoundary;
begin
  result := TFmp4NrdInfilLocationBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4NrdInfilLoc.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4IntBoundary;
begin
  result := Item.ScreenObject.ModflowFmp4NrdInfilLocationBoundary;
end;

function TframeScreenObjectFmp4NrdInfilLoc.GetValueDescription: string;
begin
  result := TFmp4NrdInfilLocationBoundary.ValueDescription;
end;

end.
