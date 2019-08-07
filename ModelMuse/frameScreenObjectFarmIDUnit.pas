unit frameScreenObjectFarmIDUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectFmpBoundaryUnit,
  Grids, RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask,
  JvExMask, JvSpin, ExtCtrls, ScreenObjectUnit, ModflowBoundaryUnit;

type
  TframeScreenObjectFarmID = class(TframeScreenObjectFmpBoundary)
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
  frameScreenObjectFarmID: TframeScreenObjectFarmID;

implementation

uses
  ModflowFmpFarmIdUnit;

{$R *.dfm}

resourcestring
  StrFarmID_IFID = 'Farm ID (IFID)';


{ TframeScreenObjectFarmID }

function TframeScreenObjectFarmID.CreateNewBoundary: TModflowBoundary;
begin
  result := TFmpFarmIDBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectFarmID.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFarmID;
end;

function TframeScreenObjectFarmID.GetBoundary(
  AScreenObject: TScreenObject): TModflowBoundary;
begin
  result := AScreenObject.ModflowFmpFarmID;
end;

procedure TframeScreenObjectFarmID.InitializeControls;
begin
  inherited;
  rdgModflowBoundary.Cells[Ord(pcValue), 0] := StrFarmID_IFID;
end;

end.
