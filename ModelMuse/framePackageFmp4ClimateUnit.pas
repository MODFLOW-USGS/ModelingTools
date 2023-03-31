unit framePackageFmp4ClimateUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.Grids, RbwDataGrid4,
  ArgusDataEntry;

type
  TClimateColumns = (ccName, ccFrequency, ccOther, ccScaleFactor,
    ccExternalFile, ccScaleExternal);
  TClimateRows =  (crName, crPrecip, crRefEt, crPotEtBare, crDirRech,
    crPrecipPotCons);

  TframePackageFmp4Climate = class(TframePackage)
    rdgClimate: TRbwDataGrid4;
    rdeRefEtToBare: TRbwDataEntry;
    lblRefEtToBare: TLabel;
    procedure rdgClimateSelectCell(Sender: TObject; ACol, ARow: Integer;
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
  framePackageFmp4Climate: TframePackageFmp4Climate;

implementation

uses
  GoPhastTypes;

resourcestring
  StrUse = 'Frequency';
  StrKeyWord = 'Key Word';
  StrPrecipitation = 'Precipitation';
  StrReferenceET = 'Reference ET';
  StrPotentialETRateOf = 'Potential ET rate of bare soil';
  StrDirectRechargeBene = 'Direct recharge beneath roots';
  StrPrecipitationPotent = 'Precipitation potential consumption';

{$R *.dfm}

var
  FluxRate: TStringList;
  LengthFraction: TStringList;


{ TframePackageFmp4Climate }

procedure TframePackageFmp4Climate.GetData(Package: TModflowPackageSelection);
var
  ClimatePkg: TFarmProcess4Climate;
  procedure GetFarmProperty(FarmProperty: TFarmProperty; ARow: Integer);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccFrequency), ARow, CanSelect);
    if CanSelect then
    begin
      rdgClimate.ItemIndex[Ord(ccFrequency), ARow] := Ord(FarmProperty.FarmOption);
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccScaleFactor), ARow, CanSelect);
    if CanSelect then
    begin
      rdgClimate.Cells[Ord(ccScaleFactor), ARow] := FarmProperty.UnitConversionScaleFactor;
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccExternalFile), ARow, CanSelect);
    if CanSelect then
    begin
      rdgClimate.Cells[Ord(ccExternalFile), ARow] := FarmProperty.ExternalFileName;
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccScaleExternal), ARow, CanSelect);
    if CanSelect then
    begin
      rdgClimate.Cells[Ord(ccScaleExternal), ARow] := FarmProperty.ExternalScaleFileName;
    end;
  end;
begin
  inherited;
  ClimatePkg := Package as TFarmProcess4Climate;

  rdeRefEtToBare.RealValue := ClimatePkg.RefEtToBare;

  GetFarmProperty(ClimatePkg.Precipitation, Ord(crPrecip));
  GetFarmProperty(ClimatePkg.ReferenceET, Ord(crRefEt));
  GetFarmProperty(ClimatePkg.Potential_Evaporation_Bare, Ord(crPotEtBare));
  GetFarmProperty(ClimatePkg.Direct_Recharge, Ord(crDirRech));
  GetFarmProperty(ClimatePkg.Precipitation_Potential_Consumption, Ord(crPrecipPotCons));

  rdgClimate.Cells[Ord(ccOther), Ord(crDirRech)] := FluxRate[Ord(ClimatePkg.DirectRechargeOption)];
  rdgClimate.Cells[Ord(ccOther), Ord(crPrecipPotCons)] := LengthFraction[Ord(ClimatePkg.PrecipPotConsum)];
end;

procedure TframePackageFmp4Climate.InitializeGrid;
begin
  rdgClimate.BeginUpdate;
  try

    rdgClimate.FixedCols := 1;
    rdgClimate.Cells[Ord(ccFrequency), Ord(crName)] := StrUse;
    rdgClimate.Cells[Ord(ccOther), Ord(crName)] := StrKeyWord;
    rdgClimate.Cells[Ord(ccScaleFactor), Ord(crName)] := StrUnitConversionScal;
    rdgClimate.Cells[Ord(ccExternalFile), Ord(crName)] := StrExternallyGenerated;
    rdgClimate.Cells[Ord(ccScaleExternal), Ord(crName)] := StrExternallyGeneratedSfac;

    rdgClimate.Cells[Ord(ccName), Ord(crPrecip)] := StrPrecipitation;
    rdgClimate.Cells[Ord(ccName), Ord(crRefEt)] := StrReferenceET;
    rdgClimate.Cells[Ord(ccName), Ord(crPotEtBare)] := StrPotentialETRateOf;
    rdgClimate.Cells[Ord(ccName), Ord(crDirRech)] := StrDirectRechargeBene;
    rdgClimate.Cells[Ord(ccName), Ord(crPrecipPotCons)] := StrPrecipitationPotent;

  finally
    rdgClimate.EndUpdate;
  end;
end;

procedure TframePackageFmp4Climate.Loaded;
begin
  inherited;
  InitializeGrid
end;

procedure TframePackageFmp4Climate.rdgClimateSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ClimateRows: TClimateRows;
  Column: TRbwColumn4;
  ItemIndex: Integer;
begin
  inherited;
  if ACol = Ord(ccOther) then
  begin
    ClimateRows := TClimateRows(ARow);
    CanSelect := ClimateRows in [crDirRech, crPrecipPotCons];
    if CanSelect and not rdgClimate.Drawing then
    begin
      Column := rdgClimate.Columns[Ord(ccOther)];
      if ClimateRows = crDirRech then
      begin
        Column.PickList := FluxRate;
      end
      else
      begin
        Column.PickList := LengthFraction;
      end;
    end;
  end;
  if ACol = Ord(ccExternalFile) then
  begin
    CanSelect := rdgClimate.ItemIndex[Ord(ccFrequency), ARow] > 0;
  end;
  if ACol in [Ord(ccScaleFactor), Ord(ccScaleExternal)] then
  begin
    ItemIndex := rdgClimate.ItemIndex[Ord(ccFrequency), ARow];
    CanSelect := ItemIndex > 0;
    if CanSelect then
    begin
      if ItemIndex = 2 then
      begin
        CanSelect := (rdgClimate.Cells[Ord(ccExternalFile), ARow] = '');
      end;
    end;
  end;
end;

procedure TframePackageFmp4Climate.SetData(Package: TModflowPackageSelection);
var
  ClimatePkg: TFarmProcess4Climate;
  function RowToFarmOption(ARow: TClimateRows): TFarmOption;
  begin
    result := TFarmOption(rdgClimate.ItemIndex[Ord(ccFrequency), Ord(ARow)]);
  end;
  procedure SetFarmProperty(FarmProperty: TFarmProperty; ARow: TClimateRows);
  var
    CanSelect: Boolean;
  begin
    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccFrequency), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.FarmOption := RowToFarmOption(ARow);
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccScaleFactor), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.UnitConversionScaleFactor :=
        rdgClimate.Cells[Ord(ccScaleFactor), Ord(ARow)];
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccExternalFile), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalFileName :=
        rdgClimate.Cells[Ord(ccExternalFile), Ord(ARow)];
    end;

    CanSelect := True;
    rdgClimateSelectCell(rdgClimate, Ord(ccScaleExternal), Ord(ARow), CanSelect);
    if CanSelect then
    begin
      FarmProperty.ExternalScaleFileName :=
        rdgClimate.Cells[Ord(ccScaleExternal), Ord(ARow)];
    end;
  end;
begin
  inherited;
  ClimatePkg := Package as TFarmProcess4Climate;

  ClimatePkg.RefEtToBare := rdeRefEtToBare.RealValue;

  SetFarmProperty(ClimatePkg.Precipitation, crPrecip);
  SetFarmProperty(ClimatePkg.ReferenceET, crRefEt);
  SetFarmProperty(ClimatePkg.Potential_Evaporation_Bare, crPotEtBare);
  SetFarmProperty(ClimatePkg.Direct_Recharge, crDirRech);
  SetFarmProperty(ClimatePkg.Precipitation_Potential_Consumption, crPrecipPotCons);

  ClimatePkg.DirectRechargeOption := TDirectRechargeOption(FluxRate.IndexOf(
    rdgClimate.Cells[Ord(ccOther), Ord(crDirRech)]));
  ClimatePkg.PrecipPotConsum := TPrecipPotConsum(LengthFraction.IndexOf(
    rdgClimate.Cells[Ord(ccOther), Ord(crPrecipPotCons)]));
end;

initialization

  FluxRate := TStringList.Create;
  FluxRate.Add('Flux');
  FluxRate.Add('Rate');
  LengthFraction := TStringList.Create;
  LengthFraction.Add('By length');
  LengthFraction.Add('By fraction');


finalization

  FluxRate.Free;
  LengthFraction.Free;

end.
