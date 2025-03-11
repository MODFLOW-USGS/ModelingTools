unit frameScreenObjectCtpUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtBoundaryUnit, Vcl.Grids,
  RbwDataGrid4, ArgusDataEntry, Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ScreenObjectUnit, Mt3dmsChemSpeciesUnit,
  ModflowGwtSpecifiedConcUnit;

type
  TframeScreenObjectCtp = class(TframeCustomGwtBoundary)
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: LongInt;
        var CanSelect: Boolean);
  public
    function GetVariableName: string; override;
    function GetMultiplierName: string; override;
    function GetActiveName: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TCncBoundary; override;
    procedure CreateNewBoundary(ScreenObject: TScreenObject); override;
    function ChemSpeciesAllowed(ChemSpeciesItem: TChemSpeciesItem): Boolean; override;
    { Public declarations }
  end;

var
  frameScreenObjectCtp: TframeScreenObjectCtp;

implementation

uses
  frmGoPhastUnit;

resourcestring
  StrSpecifiedTemperatur = 'Specified Temperature';

{$R *.dfm}

function TframeScreenObjectCtp.ChemSpeciesAllowed(
  ChemSpeciesItem: TChemSpeciesItem): Boolean;
begin
  result := inherited;
  if result then
  begin
    result := ChemSpeciesItem.UsedForGWE;
  end;
end;

procedure TframeScreenObjectCtp.CreateNewBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateGweCtpBoundary;
end;

function TframeScreenObjectCtp.GetActiveName: string;
begin
  result := StrSpecifiedTemperatur + ' Active';
end;

function TframeScreenObjectCtp.GetBoundary(
  ScreenObject: TScreenObject): TCncBoundary;
begin
  result := ScreenObject.GweCtpBoundary;
end;

function TframeScreenObjectCtp.GetMultiplierName: string;
begin
  result := StrSpecifiedTemperatur + ' Multiplier';
end;

function TframeScreenObjectCtp.GetVariableName: string;
begin
  result := StrSpecifiedTemperatur;
end;

procedure TframeScreenObjectCtp.rdgModflowBoundarySelectCell(Sender: TObject;
    ACol, ARow: LongInt; var CanSelect: Boolean);
begin
  inherited;
  inherited;
  if CanSelect  and (ACol = 4) then
  begin
    CanSelect := frmGoPhast.PhastModel.ModflowPackages.GweCtpPackage.UseMultiplier;
  end;
end;

Initialization
  RegisterClass(TframeScreenObjectCtp);

end.
