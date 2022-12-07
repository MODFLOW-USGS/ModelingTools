unit framePackageFmp4AllotmentsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageFmp4Allotments = class(TframePackage)
    comboSURFACE_WATER: TComboBox;
    lblSURFACE_WATER: TLabel;
    comboGROUNDWATER: TComboBox;
    lblGROUNDWATER: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4Allotments: TframePackageFmp4Allotments;

implementation

{$R *.dfm}

{ TframePackageFmp4Allotments }

procedure TframePackageFmp4Allotments.GetData(
  Package: TModflowPackageSelection);
var
  AllotmentPackage: TFarmProcess4Allotments;
begin
  inherited;
  AllotmentPackage := Package as TFarmProcess4Allotments;
  GetFarmOption(comboSURFACE_WATER, AllotmentPackage.SurfaceWaterChoice);
  GetFarmOption(comboGROUNDWATER, AllotmentPackage.GroundWaterChoice);

end;

procedure TframePackageFmp4Allotments.SetData(
  Package: TModflowPackageSelection);
var
  AllotmentPackage: TFarmProcess4Allotments;
begin
  inherited;
  AllotmentPackage := Package as TFarmProcess4Allotments;
  AllotmentPackage.SurfaceWaterChoice := SetFarmOption(comboSURFACE_WATER);
  AllotmentPackage.GroundWaterChoice := SetFarmOption(comboGROUNDWATER);

end;

end.
