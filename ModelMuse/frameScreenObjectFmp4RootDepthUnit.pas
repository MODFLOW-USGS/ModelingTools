unit frameScreenObjectFmp4RootDepthUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4RootDepth = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4RootDepth: TframeScreenObjectFmp4RootDepth;

implementation

uses
  ModflowFmp4RootDepthUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4Boundary1 }

function TframeScreenObjectFmp4RootDepth.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4RootDepthBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4RootDepth.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4RootDepth;
end;

function TframeScreenObjectFmp4RootDepth.GetValueDescription: string;
begin
  result := TFmp4RootDepthBoundary.ValueDescription;
end;

end.
