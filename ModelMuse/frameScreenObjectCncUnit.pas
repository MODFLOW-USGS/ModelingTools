unit frameScreenObjectCncUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ScreenObjectUnit, Mt3dmsChemSpeciesUnit,
  ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectCnc = class(TframeCustomGwtBoundary)
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  protected
    function GetVariableName: string; override;
    function GetMultiplierName: string; override;
    function GetActiveName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary; override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
    function ChemSpeciesAllowed(ChemSpeciesItem: TChemSpeciesItem): Boolean; override;
    { Private declarations }
  end;

var
  frameScreenObjectCnc: TframeScreenObjectCnc;

implementation

uses
  GoPhastTypes, frmGoPhastUnit;

resourcestring
  StrSpecifiedConcentrat = 'Specified Concentration';
{$R *.dfm}

{ TframeScreenObjectCnc }

function TframeScreenObjectCnc.ChemSpeciesAllowed(
  ChemSpeciesItem: TChemSpeciesItem): Boolean;
begin
  result := inherited;
  if result then
  begin
    result := ChemSpeciesItem.UsedForGWT;
  end;
end;

procedure TframeScreenObjectCnc.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGwtCncBoundary;
end;

function TframeScreenObjectCnc.GetActiveName: string;
begin
  result := StrSpecifiedConcentrat + ' Active';
end;

function TframeScreenObjectCnc.GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GwtCncBoundary;
end;

function TframeScreenObjectCnc.GetMultiplierName: string;
begin
  result := StrSpecifiedConcentrat + ' Multiplier';
end;

function TframeScreenObjectCnc.GetVariableName: string;
begin
  result := StrSpecifiedConcentrat;
end;

procedure TframeScreenObjectCnc.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if CanSelect  and (ACol = 4) then
  begin
    CanSelect := frmGoPhast.PhastModel.ModflowPackages.GwtCncPackage.UseMultiplier;
  end;
end;

Initialization
  RegisterClass(TframeScreenObjectCnc);

end.
