unit framePackageFmp4SurfaceWaterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, ArgusDataEntry,
  ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4;

type
  TSurfaceWaterColumns = (sfcName, sfcFrequency, sfcOption, sfcSfac, sfcFile,
    sfcSfacFile);
  TSurfaceWaterRows = (sfrName, sfrNonRoutDel, {sfrNrdInfil,} sfrSrd,
    sfrSemiLower, sfrSemiUpper, sfrNoReturn, sfrSemiReturn, sfrFullReturn);

  TframePackageFmp4SurfaceWater = class(TframePackage)
    cpnlgrp1: TCategoryPanelGroup;
    cpnlPrint: TCategoryPanel;
    clbPrint: TCheckListBox;
    cpnlOptions: TCategoryPanel;
    rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TRbwDataEntry;
    lblSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TLabel;
    cbRebuild_Fully_Routed_Return: TCheckBox;
    cpnlSurfaceWater: TCategoryPanel;
    rdgSurfaceWater: TRbwDataGrid4;
    procedure rdgSurfaceWaterSelectCell(Sender: TObject; ACol, ARow: Integer;
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
  ReturnOptions: TStringList;

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
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcFrequency), ARow, CanSelect);
    rdgSurfaceWater.ItemIndex[Ord(sfcFrequency), ARow] := Ord(FarmProperty.FarmOption);

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcSfac), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSurfaceWater.Cells[Ord(sfcSfac), ARow] := FarmProperty.UnitConversionScaleFactor;
    end;

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSurfaceWater.Cells[Ord(sfcFile), ARow] := FarmProperty.ExternalFileName;
    end;

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcSfacFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgSurfaceWater.Cells[Ord(sfcSfacFile), ARow] := FarmProperty.ExternalScaleFileName;
    end;
  end;
  var
    CanSelect: Boolean;
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

  rdgSurfaceWater.BeginUpdate;
  try
    GetFarmProperty(SurfWatPackage.Non_Routed_Delivery, Ord(sfrNonRoutDel));
    rdgSurfaceWater.Cells[Ord(sfcOption), Ord(sfrNonRoutDel)] :=
      RateVolume[Ord(SurfWatPackage.NRDOption)];
//    GetFarmProperty(SurfWatPackage.Nrd_Infiltration_Location, Ord(sfrNrdInfil));
    GetFarmProperty(SurfWatPackage.Semi_Routed_Delivery, Ord(sfrSrd));
    GetFarmProperty(SurfWatPackage.SemiRoutedDeliveryLowerLimit, Ord(sfrSemiLower));
    GetFarmProperty(SurfWatPackage.SemiRoutedDeliveryUpperLimit, Ord(sfrSemiUpper));
    GetFarmProperty(SurfWatPackage.NoReturnFlow, Ord(sfrNoReturn));
    GetFarmProperty(SurfWatPackage.SemiRoutedReturn, Ord(sfrSemiReturn));
    GetFarmProperty(SurfWatPackage.RoutedReturn, Ord(sfrFullReturn));
    rdgSurfaceWater.Cells[Ord(sfcOption), Ord(sfrFullReturn)] :=
      ReturnOptions[Ord(SurfWatPackage.ReturnOption)];
  finally
    rdgSurfaceWater.EndUpdate;
  end;

  rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue := SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance;
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;

  CanSelect := True;
  rdgSurfaceWaterSelectCell(rdgSurfaceWater, 1, 1, CanSelect);

end;

procedure TframePackageFmp4SurfaceWater.InitializeGrid;
begin
  rdgSurfaceWater.BeginUpdate;
  try
    rdgSurfaceWater.FixedCols := 1;

    rdgSurfaceWater.Cells[Ord(sfcFrequency), Ord(sfrName)] := StrFrequency;
    rdgSurfaceWater.Cells[Ord(sfcOption), Ord(sfrName)] := 'Key Word';
    rdgSurfaceWater.Cells[Ord(sfcSfac), Ord(sfrName)] := StrUnitConversionScal;
    rdgSurfaceWater.Cells[Ord(sfcFile), Ord(sfrName)] := StrExternallyGenerated;
    rdgSurfaceWater.Cells[Ord(sfcSfacFile), Ord(sfrName)] := StrExternallyGeneratedSfac;

    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrNonRoutDel)] := 'Non-routed delivery';
//    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrNrdInfil)] := 'NRD  infiltration location';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrSrd)] := 'Semi-routed deliveries';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrSemiLower)] := 'SRD lower-limit';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrSemiUpper)] := 'SRD upper-limit';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrNoReturn)] := 'No return flow choice';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrSemiReturn)] := 'Semi-routed return';
    rdgSurfaceWater.Cells[Ord(sfcName), Ord(sfrFullReturn)] := 'Routed return';

  finally
    rdgSurfaceWater.EndUpdate;
  end;
end;

procedure TframePackageFmp4SurfaceWater.Loaded;
begin
  inherited;
  cpnlPrint.Collapse;
  cpnlOptions.Collapse;
  InitializeGrid;
end;

procedure TframePackageFmp4SurfaceWater.rdgSurfaceWaterSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
begin
  inherited;
  if ACol = Ord(sfcOption) then
  begin
    CanSelect := ARow in [Ord(sfrNonRoutDel), Ord(sfrFullReturn)];
    if CanSelect and not rdgSurfaceWater.Drawing then
    begin
      Column := rdgSurfaceWater.Columns[ACol];
      if ARow = Ord(sfrNonRoutDel) then
      begin
        Column.PickList := RateVolume;
      end
      else
      begin
        Column.PickList := ReturnOptions;
      end;
    end;
  end;

//  if ARow = Ord(sfrNrdInfil) then
//  begin
//    CanSelect := False;
//    // The option to specify infiltration locations is not included
//    // as of MODFLOW-OWHM version 2.3. If it is included, uncomment
//    // the following lines. A change would also need to be makde in
//    // the initialization section of frameDeliveryGridUnit
//
//    {
//    if ACol in [Ord(sfcSfac), Ord(sfcSfacFile)] then
//    begin
//      CanSelect := False;
//    end;
//    if ACol = Ord(sfcFile) then
//    begin
//      CanSelect := rdgSurfaceWater.ItemIndex[Ord(sfcFrequency), ARow] > 0;
//    end;
//    }
//  end;

  if (ACol = Ord(sfcFrequency)) then
  begin
    if ARow in [Ord(sfrSemiLower), Ord(sfrSemiUpper)] then
    begin
      CanSelect := DontUseStaticTransient.IndexOf(
        rdgSurfaceWater.Cells[ACol, Ord(sfrSrd)])  > 0;
    end;
    if not rdgSurfaceWater.Drawing then
    begin
      Column := rdgSurfaceWater.Columns[ACol];
      if ARow in [Ord(sfrFullReturn), Ord(sfrSemiLower), Ord(sfrSemiUpper)] then
      begin
        Column.PickList := DontUse_Use;
      end
      else
      begin
        Column.PickList := DontUseStaticTransient;
      end;
    end;
  end;

  if ACol in [Ord(sfcOption), Ord(sfcSfac), Ord(sfcFile), Ord(sfcSfacFile)] then
  begin
    if rdgSurfaceWater.Cells[Ord(sfcFrequency), ARow] = 'Don''t use' then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframePackageFmp4SurfaceWater.SetData(
  Package: TModflowPackageSelection);
var
  SurfWatPackage: TFarmProcess4SurfaceWater;
  PrintChoice: TSurfaceWaterPrints;
  PrintIndex: TSurfaceWaterPrint;
  function RowToFarmOption(ARow: TSurfaceWaterRows): TFarmOption;
  var
    ItemIndex: Integer;
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcFrequency), Ord(ARow), CanSelect);
    result := TFarmOption(
      rdgSurfaceWater.ItemIndex[Ord(sfcFrequency), Ord(ARow)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TSurfaceWaterRows);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcFrequency), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.FarmOption := RowToFarmOption(ARow);
    end;

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcSfac), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.UnitConversionScaleFactor :=
        rdgSurfaceWater.Cells[Ord(sfcSfac), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalFileName :=
        rdgSurfaceWater.Cells[Ord(sfcFile), Ord(ARow)];
    end;

    CanSelect := True;
    rdgSurfaceWaterSelectCell(rdgSurfaceWater, Ord(sfcSfacFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalScaleFileName :=
        rdgSurfaceWater.Cells[Ord(sfcSfacFile), Ord(ARow)];
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
  SurfWatPackage.NRDOption := TNRDOption(RateVolume.IndexOf(
    rdgSurfaceWater.Cells[Ord(sfcOption), Ord(sfrNonRoutDel)]));

//  SetFarmProperty(SurfWatPackage.Nrd_Infiltration_Location, sfrNrdInfil);
  SetFarmProperty(SurfWatPackage.Semi_Routed_Delivery, sfrSrd);
  SetFarmProperty(SurfWatPackage.SemiRoutedDeliveryLowerLimit, sfrSemiLower);
  SetFarmProperty(SurfWatPackage.SemiRoutedDeliveryUpperLimit, sfrSemiUpper);
//  SetFarmProperty(SurfWatPackage.ReturnChoice, sfrReturnChoic);

  SetFarmProperty(SurfWatPackage.NoReturnFlow, sfrNoReturn);
  SetFarmProperty(SurfWatPackage.SemiRoutedReturn, sfrSemiReturn);
  SetFarmProperty(SurfWatPackage.RoutedReturn, sfrFullReturn);
  SurfWatPackage.ReturnOption := TReturnOption(ReturnOptions.IndexOf(
    rdgSurfaceWater.Cells[Ord(sfcOption), Ord(sfrFullReturn)]));


  SurfWatPackage.Semi_Routed_Delivery_Closure_Tolerance := rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE.RealValue;
  cbRebuild_Fully_Routed_Return.Checked := SurfWatPackage.Rebuild_Fully_Routed_Return;
end;

initialization
  RateVolume := TStringList.Create;
  RateVolume.Add('Rate');
  RateVolume.Add('Volume (per stress period)');

  ReturnOptions := TStringList.Create;
  ReturnOptions.Add('Non-Diversions');
  ReturnOptions.Add('Any');

finalization
  RateVolume.Free;
  ReturnOptions.Free;

end.
