unit frameScreenObjectFmpEvapUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectFmpBoundaryUnit,
  Grids, RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask,
  JvExMask, JvSpin, ExtCtrls, ScreenObjectUnit, ModflowBoundaryUnit;

type
  TframeScreenObjectFmpEvap = class(TframeScreenObjectFmpBoundary)
  private
    { Private declarations }
  protected
    function GetBoundary(AScreenObject: TScreenObject): TModflowBoundary; override;
    procedure CreateScreenObjectBoundary(AScreenObject: TScreenObject); override;
    function CreateNewBoundary: TModflowBoundary; override;
    procedure InitializeControls; override;
  public
    { Public declarations }
  end;

var
  frameScreenObjectFmpEvap: TframeScreenObjectFmpEvap;

implementation

uses
  ModflowFmpEvapUnit;

resourcestring
  StrReferenceEvapotrans = 'Reference evapotranspiration (ETR)';

{$R *.dfm}

{ TframeScreenObjectFmpBoundary1 }

function TframeScreenObjectFmpEvap.CreateNewBoundary: TModflowBoundary;
begin
  result := TFmpRefEvapBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectFmpEvap.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFarmRefEvap;
end;

function TframeScreenObjectFmpEvap.GetBoundary(
  AScreenObject: TScreenObject): TModflowBoundary;
begin
  result := AScreenObject.ModflowFmpRefEvap;
end;

procedure TframeScreenObjectFmpEvap.InitializeControls;
begin
  inherited;
  rdgModflowBoundary.Cells[Ord(pcValue), 0] := StrReferenceEvapotrans;
end;

end.
