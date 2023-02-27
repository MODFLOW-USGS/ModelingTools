unit frameScreenObjectFmp4ConsumptiveUseUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4ConsumptiveUse = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4ConsumptiveUse: TframeScreenObjectFmp4ConsumptiveUse;

implementation

uses
  ModflowFmp4ConsumptiveUseUnit;

{$R *.dfm}

{ TframeScreenObjectFmp4ConsumptiveUse }

function TframeScreenObjectFmp4ConsumptiveUse.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4ConsumptiveUseBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4ConsumptiveUse.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4ConsumptiveUse;
end;

function TframeScreenObjectFmp4ConsumptiveUse.GetValueDescription: string;
begin
  result := TFmp4ConsumptiveUseBoundary.ValueDescription;
end;

end.
