unit framePackageFmp4SoilsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4;

type
  TSoilColumns = (scName, scFrequency, scArrayList, scScaleFactor,
    scExternalFile, scScaleExternal);
  TSoilRows =  (srName, srCapFringe, srSurfK);

  TframePackageFmp4Soils = class(TframePackage)
    rdgSoils: TRbwDataGrid4;
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
  framePackageFmp4Soils: TframePackageFmp4Soils;

implementation

uses
  GoPhastTypes;

resourcestring
  StrCapillaryFringe = 'Capillary fringe';
  StrSurfaceK = 'Surface K';
  StrUse = 'Use';

{$R *.dfm}

{ TframePackageFmpSoils }

procedure TframePackageFmp4Soils.GetData(Package: TModflowPackageSelection);
var
  SoilPkg: TFarmProcess4Soil;
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  begin
    rdgSoils.ItemIndex[Ord(scFrequency), ARow] := Ord(FarmProperty.FarmOption);
    rdgSoils.ItemIndex[Ord(scArrayList), ARow] := Ord(FarmProperty.ArrayList);
    rdgSoils.Cells[Ord(scScaleFactor), ARow] := FarmProperty.UnitConversionScaleFactor;
    rdgSoils.Cells[Ord(scExternalFile), ARow] := FarmProperty.ExternalFileName;
    rdgSoils.Cells[Ord(scScaleExternal), ARow] := FarmProperty.ExternalScaleFileName;
  end;
begin
  inherited;
  SoilPkg := Package as TFarmProcess4Soil;
  rdgSoils.BeginUpdate;
  try
    GetFarmProperty(SoilPkg.CapFringe, Ord(srCapFringe));
    GetFarmProperty(SoilPkg.SurfVertK, Ord(srSurfK));
  finally
    rdgSoils.EndUpdate;
  end;
end;

procedure TframePackageFmp4Soils.InitializeGrid;
begin
  rdgSoils.BeginUpdate;
  try
    rdgSoils.FixedCols := 1;

    rdgSoils.Cells[Ord(scName), Ord(srCapFringe)] := StrCapillaryFringe;
    rdgSoils.Cells[Ord(scName), Ord(srSurfK)] := StrSurfaceK;

    rdgSoils.Cells[Ord(scFrequency), Ord(srName)] := StrUse;
    rdgSoils.Cells[Ord(scArrayList), Ord(srName)] := StrArrayOrList;
    rdgSoils.Cells[Ord(scScaleFactor), Ord(srName)] := StrUnitConversionScal;
    rdgSoils.Cells[Ord(scExternalFile), Ord(srName)] := StrExternallyGenerated;
    rdgSoils.Cells[Ord(scScaleExternal), Ord(srName)] := StrExternallyGeneratedSfac;
  finally
    rdgSoils.EndUpdate;
  end;
end;

procedure TframePackageFmp4Soils.Loaded;
begin
  inherited;
  InitializeGrid;
end;

procedure TframePackageFmp4Soils.SetData(Package: TModflowPackageSelection);
var
  SoilPkg: TFarmProcess4Soil;
  function RowToFarmOption(ARow: TSoilRows): TFarmOption;
  begin
    result := TFarmOption(rdgSoils.ItemIndex[Ord(scFrequency), Ord(ARow)]);
  end;
  function RowToArrayList(ARow: TSoilRows): TArrayList;
  begin
    result := TArrayList(rdgSoils.ItemIndex[Ord(scArrayList), Ord(ARow)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TSoilRows);
  begin
    FarmProperty.FarmOption := RowToFarmOption(ARow);
    FarmProperty.ArrayList := RowToArrayList(ARow);
    FarmProperty.UnitConversionScaleFactor :=
      rdgSoils.Cells[Ord(scScaleFactor), Ord(ARow)];
    FarmProperty.ExternalFileName :=
      rdgSoils.Cells[Ord(scExternalFile), Ord(ARow)];
    FarmProperty.ExternalScaleFileName :=
      rdgSoils.Cells[Ord(scScaleExternal), Ord(ARow)];
  end;
begin
  inherited;
  SoilPkg := Package as TFarmProcess4Soil;
  SetFarmProperty(SoilPkg.CapFringe, srCapFringe);
  SetFarmProperty(SoilPkg.SurfVertK, srSurfK);
end;

end.
