unit frameScreenObjectMultCropCoefficientsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  frameScreenObjectCustomFmp4MultBoundaryUnit, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask, JvSpin,
  Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4LandUseBoundaryUnit;

type
  TframeScreenObjectMultCropCoefficients = class(TframeScreenObjectCustomFmp4MultBoundary)
  private
    { Private declarations }
  protected
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4LandUseBoundary; override;
    function CreateBoundary: TFmp4LandUseBoundary; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectMultCropCoefficients: TframeScreenObjectMultCropCoefficients;

implementation

uses
  ModflowFmp4CropCoefficientUnit;

{$R *.dfm}

{ TframeScreenObjectMultCropCoefficients }

function TframeScreenObjectMultCropCoefficients.CreateBoundary: TFmp4LandUseBoundary;
begin
  result := TFmp4MultCropCoefficientBoundary.Create(nil, nil);
end;

function TframeScreenObjectMultCropCoefficients.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4LandUseBoundary;
begin
  result := Item.ScreenObject.ModflowFmp4MultCropCoefficient;
end;

end.
