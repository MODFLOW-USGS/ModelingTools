unit framePackageFmp4AllotmentsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4;

type
  TAllotmentColumns = (acName, acFrequency, acOption, asSFAC, acExtFile,
    acExtSfacFile);
  TAllotmentRows = (arName, arSurfaceWater, arGroundwater);

  TframePackageFmp4Allotments = class(TframePackage)
    rdgAllotments: TRbwDataGrid4;
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
  framePackageFmp4Allotments: TframePackageFmp4Allotments;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageFmp4Allotments }

procedure TframePackageFmp4Allotments.GetData(
  Package: TModflowPackageSelection);
var
  AllotmentPackage: TFarmProcess4Allotments;
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  begin
    rdgAllotments.ItemIndex[Ord(acFrequency), ARow] := Ord(FarmProperty.FarmOption);
    rdgAllotments.Cells[Ord(asSFAC), ARow] := FarmProperty.UnitConversionScaleFactor;
    rdgAllotments.Cells[Ord(acExtFile), ARow] := FarmProperty.ExternalFileName;
    rdgAllotments.Cells[Ord(acExtSfacFile), ARow] := FarmProperty.ExternalScaleFileName;
  end;
begin
  inherited;
  AllotmentPackage := Package as TFarmProcess4Allotments;
  rdgAllotments.BeginUpdate;
  try
    GetFarmProperty(AllotmentPackage.SurfaceWater, Ord(arSurfaceWater));
    GetFarmProperty(AllotmentPackage.GroundWater, Ord(arGroundwater));
    rdgAllotments.ItemIndex[Ord(acOption), Ord(arSurfaceWater)] :=
      Ord(AllotmentPackage.SurfaceWaterAllotmentMethod);
    rdgAllotments.ItemIndex[Ord(acOption), Ord(arGroundwater)] :=
      Ord(AllotmentPackage.GroundWaterAllotmentMethod);
  finally
    rdgAllotments.EndUpdate;
  end
end;

procedure TframePackageFmp4Allotments.InitializeGrid;
begin
  rdgAllotments.BeginUpdate;
  try
    rdgAllotments.FixedCols := 1;

    rdgAllotments.Cells[Ord(acFrequency), Ord(arName)] := StrFrequency;
    rdgAllotments.Cells[Ord(acOption), Ord(arName)] := 'Option';
    rdgAllotments.Cells[Ord(asSFAC), Ord(arName)] := StrUnitConversionScal;
    rdgAllotments.Cells[Ord(acExtFile), Ord(arName)] := StrExternallyGenerated;
    rdgAllotments.Cells[Ord(acExtSfacFile), Ord(arName)] := StrExternallyGeneratedSfac;

    rdgAllotments.Cells[Ord(acName), Ord(arSurfaceWater)] := 'Surface Water Allotment';
    rdgAllotments.Cells[Ord(acName), Ord(arGroundwater)] := 'Groundwater Allotment';
  finally
    rdgAllotments.EndUpdate;
  end;
end;

procedure TframePackageFmp4Allotments.Loaded;
begin
  inherited;
  InitializeGrid
end;

procedure TframePackageFmp4Allotments.SetData(
  Package: TModflowPackageSelection);
var
  AllotmentPackage: TFarmProcess4Allotments;
  function RowToFarmOption(ARow: TAllotmentRows): TFarmOption;
  begin
    result := TFarmOption(rdgAllotments.ItemIndex[Ord(acFrequency), Ord(ARow)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TAllotmentRows);
  var
    CanSelect: Boolean;
  begin
      FarmProperty.FarmOption := RowToFarmOption(ARow);
      FarmProperty.UnitConversionScaleFactor :=
        rdgAllotments.Cells[Ord(asSFAC), Ord(ARow)];
      FarmProperty.ExternalFileName :=
        rdgAllotments.Cells[Ord(acExtFile), Ord(ARow)];
      FarmProperty.ExternalScaleFileName :=
        rdgAllotments.Cells[Ord(acExtSfacFile), Ord(ARow)];
  end;
begin
  inherited;
  AllotmentPackage := Package as TFarmProcess4Allotments;
  SetFarmProperty(AllotmentPackage.SurfaceWater, arSurfaceWater);
  SetFarmProperty(AllotmentPackage.GroundWater, arGroundwater);
 AllotmentPackage.SurfaceWaterAllotmentMethod := TAllotmentMethod(
   rdgAllotments.ItemIndex[Ord(acOption), Ord(arSurfaceWater)]);
 AllotmentPackage.GroundWaterAllotmentMethod := TAllotmentMethod(
   rdgAllotments.ItemIndex[Ord(acOption), Ord(arGroundwater)]);

end;

end.
