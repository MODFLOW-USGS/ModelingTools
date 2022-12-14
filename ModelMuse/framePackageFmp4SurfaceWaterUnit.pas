unit framePackageFmp4SurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, ArgusDataEntry,
  ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4;

type
  TSalinityFlushColumns = (sfcName, sfcFrequency, sfcOption, sfcSfac, sfcFile,
    sfcSfacFile);
  TSalinityFlushRows = (sfrName, sfrNonRoutDel, sfrNrdInfil, sfrSrd,
    sfrSemiLower, sfrSemiUpper, sfrReturnChoic);

  TframePackageFmp4SurfaceWater = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TRbwDataEntry;
    lblSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TLabel;
    cbRebuild_Fully_Routed_Return: TCheckBox;
    cpnlSalinityFlush: TCategoryPanel;
    rdgSalinityFlush: TRbwDataGrid4;
    procedure rdgSalinityFlushSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    procedure InitializeGrid;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4SurfaceWater: TframePackageFmp4SurfaceWater;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageFmp4SurfaceWater }

var
  RateVolume: TStringList;

procedure TframePackageFmp4SurfaceWater.GetData(
  Package: TModflowPackageSelection);
var
  SurfWatPackage: TFarmProcess4SurfaceWater;
  PrintIndex: TSurfaceWaterPrint;
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFrequency), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.ItemIndex[Ord(sfcFrequency), ARow] := Ord(FarmProperty.FarmOption);
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfac), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcSfac), ARow] := FarmProperty.UnitConversionScaleFactor;
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcFile), ARow] := FarmProperty.ExternalFileName;
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfacFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSalinityFlush.Cells[Ord(sfcSfacFile), ARow] := FarmProperty.ExternalScaleFileName;
    end;
  end;
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

  rdgSalinityFlush.BeginUpdate;
  try
    GetFarmProperty(SurfWatPackage.Non_Routed_Delivery, Ord(sfrNonRoutDel));
    rdgSalinityFlush.ItemIndex[Ord(sfcOption), Ord(sfrNonRoutDel)] := Ord(SurfWatPackage.NRDOption);
    GetFarmProperty(SurfWatPackage.Nrd_Infiltration_Location, Ord(sfrNrdInfil));
    GetFarmProperty(SurfWatPackage.Semi_Routed_Delivery, Ord(sfrSrd));
    GetFarmProperty(SurfWatPackage.SemiRoutedDeliveryLowerLimit, Ord(sfrSemiLower));
    GetFarmProperty(SurfWatPackage.SemiRoutedDeliveryUpperLimit, Ord(sfrSemiUpper));
    GetFarmProperty(SurfWatPackage.ReturnChoice, Ord(sfrReturnChoic));
  finally
    rdgSalinityFlush.EndUpdate;
  end;

  rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue := SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance;
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;
end;

procedure TframePackageFmp4SurfaceWater.InitializeGrid;
begin
  rdgSalinityFlush.BeginUpdate;
  try
    rdgSalinityFlush.FixedCols := 1;

    rdgSalinityFlush.Cells[Ord(sfcFrequency), Ord(sfrName)] := StrFrequency;
    rdgSalinityFlush.Cells[Ord(sfcOption), Ord(sfrName)] := 'Option';
    rdgSalinityFlush.Cells[Ord(sfcSfac), Ord(sfrName)] := StrUnitConversionScal;
    rdgSalinityFlush.Cells[Ord(sfcFile), Ord(sfrName)] := StrExternallyGenerated;
    rdgSalinityFlush.Cells[Ord(sfcSfacFile), Ord(sfrName)] := StrExternallyGeneratedSfac;

    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrNonRoutDel)] := 'Non-routed delivery';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrNrdInfil)] := 'NRD  infiltration coll.';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrSrd)] := 'Semi-routed deliveries';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrSemiLower)] := 'SRD lower-limit';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrSemiUpper)] := 'SRD upper-limit';
    rdgSalinityFlush.Cells[Ord(sfcName), Ord(sfrReturnChoic)] := 'Return flow specification';

  finally
    rdgSalinityFlush.EndUpdate;
  end;
end;

procedure TframePackageFmp4SurfaceWater.Loaded;
begin
  inherited;
  cpnlPrint.Collapse;
  cpnlOptions.Collapse;
  InitializeGrid;
end;

procedure TframePackageFmp4SurfaceWater.rdgSalinityFlushSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if ACol = Ord(sfcOption) then
  begin
    CanSelect := ARow = Ord(sfrNonRoutDel);
  end;
end;

procedure TframePackageFmp4SurfaceWater.SetData(
  Package: TModflowPackageSelection);
var
  SurfWatPackage: TFarmProcess4SurfaceWater;
  PrintChoice: TSurfaceWaterPrints;
  PrintIndex: TSurfaceWaterPrint;
  function RowToFarmOption(ARow: TSalinityFlushRows): TFarmOption;
  begin
    result := TFarmOption(
      rdgSalinityFlush.ItemIndex[Ord(sfcFrequency), Ord(ARow)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TSalinityFlushRows);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFrequency), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.FarmOption := RowToFarmOption(ARow);
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfac), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.UnitConversionScaleFactor :=
        rdgSalinityFlush.Cells[Ord(sfcSfac), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalFileName :=
        rdgSalinityFlush.Cells[Ord(sfcFile), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSalinityFlushSelectCell(rdgSalinityFlush, Ord(sfcSfacFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalScaleFileName :=
        rdgSalinityFlush.Cells[Ord(sfcSfacFile), Ord(ARow)];
    end;
  end;
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

  SetFarmProperty(SurfWatPackage.Non_Routed_Delivery, sfrNonRoutDel);
  SurfWatPackage.NRDOption := TNRDOption(rdgSalinityFlush.ItemIndex[Ord(sfcOption), Ord(sfrNonRoutDel)]);

  SetFarmProperty(SurfWatPackage.Nrd_Infiltration_Location, sfrNrdInfil);
  SetFarmProperty(SurfWatPackage.Semi_Routed_Delivery, sfrSrd);
  SetFarmProperty(SurfWatPackage.SemiRoutedDeliveryLowerLimit, sfrSemiLower);
  SetFarmProperty(SurfWatPackage.SemiRoutedDeliveryUpperLimit, sfrSemiUpper);
  SetFarmProperty(SurfWatPackage.ReturnChoice, sfrReturnChoic);

  SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance := rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue;
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;
end;

initialization
  RateVolume := TStringList.Create;
  RateVolume.Add('Rate');
  RateVolume.Add('Volume');

finalization
  RateVolume.Free;

end.
