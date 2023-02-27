unit frameScreenObjectFmp4CropCoefficientUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4CropCoefficient = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4CropCoefficient: TframeScreenObjectFmp4CropCoefficient;

implementation

uses
  ModflowFmp4CropCoefficientUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4Boundary1 }

function TframeScreenObjectFmp4CropCoefficient.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4CropCoefficientBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4CropCoefficient.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4CropCoefficient;
end;

function TframeScreenObjectFmp4CropCoefficient.GetValueDescription: string;
begin
  result := TFmp4CropCoefficientBoundary.ValueDescription;
end;

end.
