unit frameScreenObjecFmp4ConsumptiveUseUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjecFmp4ConsumptiveUse = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjecFmp4ConsumptiveUse: TframeScreenObjecFmp4ConsumptiveUse;

implementation

uses
  ModflowFmp4ConsumptiveUseUnit;

{$R *.dfm}

{ TframeScreenObjecFmp4ConsumptiveUse }

function TframeScreenObjecFmp4ConsumptiveUse.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4ConsumptiveUseBoundary.Create(nil, nil);
end;

function TframeScreenObjecFmp4ConsumptiveUse.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4ConsumptiveUse;
end;

function TframeScreenObjecFmp4ConsumptiveUse.GetValueDescription: string;
begin
  result := TFmp4ConsumptiveUseBoundary.ValueDescription;
end;

end.
