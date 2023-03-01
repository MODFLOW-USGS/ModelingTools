unit frameScreenObjectMultRootDepthUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  frameScreenObjectCustomFmp4MultBoundaryUnit, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask, JvSpin,
  Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4LandUseBoundaryUnit;

type
  TframeScreenObjectMultRootDepth = class(TframeScreenObjectCustomFmp4MultBoundary)
  private
    { Private declarations }
  protected
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4LandUseBoundary; override;
    function CreateBoundary: TFmp4LandUseBoundary; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectMultRootDepth: TframeScreenObjectMultRootDepth;

implementation

uses
  ModflowFmp4RootDepthUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4MultBoundary1 }

function TframeScreenObjectMultRootDepth.CreateBoundary: TFmp4LandUseBoundary;
begin
  result := TFmp4MultRootDepthBoundary.Create(nil, nil);
end;

function TframeScreenObjectMultRootDepth.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4LandUseBoundary;
begin
  result := Item.ScreenObject.ModflowFmp4MultRootDepth;
end;

end.
