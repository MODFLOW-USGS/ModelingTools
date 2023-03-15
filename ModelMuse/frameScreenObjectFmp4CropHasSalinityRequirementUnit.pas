unit frameScreenObjectFmp4CropHasSalinityRequirementUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCustomFmp4BoundaryUnit,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, ModflowFmp4BoundaryUnit;

type
  TframeScreenObjectFmp4CropHasSalinityRequirement = class(TframeScreenObjectCustomFmp4Boundary)
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
  frameScreenObjectFmp4CropHasSalinityRequirement: TframeScreenObjectFmp4CropHasSalinityRequirement;

implementation

uses
  ModflowFmp4CropHasSalinityDemandUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4Boundary1 }

function TframeScreenObjectFmp4CropHasSalinityRequirement.CreateBoundary: TFmp4Boundary;
begin
  result := TFmp4CropHasSalinityDemandBoundary.Create(nil, nil);
end;

function TframeScreenObjectFmp4CropHasSalinityRequirement.GetBoundary(
  Item: TScreenObjectEditItem): TFmp4Boundary;
begin
  result := Item.ScreenObject.ModflowFmp4CropHasSalinityDemand;
end;

function TframeScreenObjectFmp4CropHasSalinityRequirement.GetValueDescription: string;
begin
  result := TFmp4CropHasSalinityDemandBoundary.ValueDescription;
end;

end.
