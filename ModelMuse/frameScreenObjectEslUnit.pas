unit frameScreenObjectEslUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ScreenObjectUnit, Mt3dmsChemSpeciesUnit,
  ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectEsl = class(TframeCustomGwtBoundary)
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: LongInt;
        var CanSelect: Boolean);
  private
    { Private declarations }
  public
    function GetVariableName: string; override;
    function GetMultiplierName: string; override;
    function GetActiveName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary;
      override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
    function ChemSpeciesAllowed(ChemSpeciesItem: TChemSpeciesItem): Boolean; override;
    { Public declarations }
  end;

var
  frameScreenObjectEsl: TframeScreenObjectEsl;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrEnergySource = 'Energy Source';

{$R *.dfm}

function TframeScreenObjectEsl.ChemSpeciesAllowed(
  ChemSpeciesItem: TChemSpeciesItem): Boolean;
begin
  result := inherited;
  if result then
  begin
    result := ChemSpeciesItem.UsedForGWE;
  end;
end;

procedure TframeScreenObjectEsl.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGweEslBoundary;
end;

function TframeScreenObjectEsl.GetActiveName: string;
begin
  result := StrEnergySource + ' Active';
end;

function TframeScreenObjectEsl.GetBoundary(
  ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GweEslBoundary;
end;

function TframeScreenObjectEsl.GetMultiplierName: string;
begin
  result := StrEnergySource + ' Multiplier';
end;

function TframeScreenObjectEsl.GetVariableName: string;
begin
  result := StrEnergySource;
end;

procedure TframeScreenObjectEsl.rdgModflowBoundarySelectCell(Sender: TObject;
    ACol, ARow: LongInt; var CanSelect: Boolean);
begin
  inherited;
  if CanSelect  and (ACol = 4) then
  begin
    CanSelect := frmGoPhast.PhastModel.ModflowPackages.GweEslPackage.UseMultiplier;
  end;
end;

end.
