unit framePackageFmp4SurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, ArgusDataEntry,
  ModflowPackageSelectionUnit;

type
  TframePackageFmp4SurfaceWater = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    comboNon_Routed_Delivery: TComboBox;
    lblNon_Routed_Delivery: TLabel;
    comboNrdInfilLocation: TComboBox;
    lblNrdInfilLocation: TLabel;
    comboSemi_Routed_Delivery: TComboBox;
    lblSemi_Routed_Delivery: TLabel;
    comboSemi_Routed_DeliveryLimits: TComboBox;
    lblSemi_Routed_DeliveryLimits: TLabel;
    rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TRbwDataEntry;
    lblSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TLabel;
    comboReturnFlow: TComboBox;
    lblReturnFlow: TLabel;
    cbRebuild_Fully_Routed_Return: TCheckBox;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4SurfaceWater: TframePackageFmp4SurfaceWater;

implementation

{$R *.dfm}

{ TframePackageFmp4SurfaceWater }

procedure TframePackageFmp4SurfaceWater.GetData(
  Package: TModflowPackageSelection);
var
  SurfWatPackage: TFarmProcess4SurfaceWater;
  PrintIndex: TSurfaceWaterPrint;
begin
  cpnlPrint.Collapse;
  if cpnlgrp1.VertScrollBar.Visible then
  begin
    cpnlgrp1.VertScrollBar.Position := 0;
  end;
  inherited;
  SurfWatPackage := Package as TFarmProcess4SurfaceWater;

  for PrintIndex := Low(TSurfaceWaterPrint) to High(TSurfaceWaterPrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in SurfWatPackage.SurfaceWaterPrints;
  end;

  GetFarmOption(comboNon_Routed_Delivery, SurfWatPackage.Non_Routed_Delivery);
  GetFarmOption(comboNrdInfilLocation, SurfWatPackage.Nrd_Infiltration_Location);
  GetFarmOption(comboSemi_Routed_Delivery, SurfWatPackage.Semi_Routed_Delivery);
  GetFarmOption(comboSemi_Routed_DeliveryLimits, SurfWatPackage.Semi_Routed_Delivery_Limits);
  rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue := SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance;
  GetFarmOption(comboReturnFlow, SurfWatPackage.ReturnChoice);
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;

end;

procedure TframePackageFmp4SurfaceWater.SetData(
  Package: TModflowPackageSelection);
var
  SurfWatPackage: TFarmProcess4SurfaceWater;
  PrintChoice: TSurfaceWaterPrints;
  PrintIndex: TSurfaceWaterPrint;
begin
  inherited;
  SurfWatPackage := Package as TFarmProcess4SurfaceWater;

  PrintChoice := [];
  for PrintIndex := Low(TSurfaceWaterPrint) to High(TSurfaceWaterPrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(PrintChoice, PrintIndex)
    end;
    SurfWatPackage.SurfaceWaterPrints := PrintChoice;
  end;

  SurfWatPackage.Non_Routed_Delivery := SetFarmOption(comboNon_Routed_Delivery);
  SurfWatPackage.Nrd_Infiltration_Location := SetFarmOption(comboNrdInfilLocation);
  SurfWatPackage.Semi_Routed_Delivery := SetFarmOption(comboSemi_Routed_Delivery);
  SurfWatPackage.Semi_Routed_Delivery_Limits := SetFarmOption(comboSemi_Routed_DeliveryLimits);
  SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance := rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue;
  SurfWatPackage.ReturnChoice := SetFarmOption(comboReturnFlow);
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;

end;

end.
