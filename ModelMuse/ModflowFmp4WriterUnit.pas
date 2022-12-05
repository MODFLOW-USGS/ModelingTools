{@name writes the FMP input file for MODFLOW-OWHM version 2.}
unit ModflowFmp4WriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, ModflowFmpWellUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, ScreenObjectUnit, ModflowBoundaryUnit;

type
  TModflowFmp4Writer = class(TCustomListWriter)
  private
    procedure WriteGobalDimension;
    procedure WriteOutput;
    procedure WriteWaterBalanceSubregion;
    procedure WriteSoil;
    procedure WriteClimate;
    procedure WriteSurfaceWater;
    procedure WriteSupplyWell;
    procedure WriteAllotments;
    procedure WriteLandUse;
    procedure WriteSalinityFlush;
    procedure WriteSurfaceWaterIrrigation;
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses
  GoPhastTypes;

{ TModflowFmp4Writer }

function TModflowFmp4Writer.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

constructor TModflowFmp4Writer.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;

end;

destructor TModflowFmp4Writer.Destroy;
begin

  inherited;
end;

class function TModflowFmp4Writer.Extension: string;
begin
  Result := '.fmp';
end;

function TModflowFmp4Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
end;

function TModflowFmp4Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FarmProcess4;
end;

procedure TModflowFmp4Writer.WriteAllotments;
begin

end;

procedure TModflowFmp4Writer.WriteClimate;
begin

end;

procedure TModflowFmp4Writer.WriteFile(const AFileName: string);
begin
  if (not Package.IsSelected)
{$IFDEF OWHMV2}
  or (Model.ModelSelection <> msModflowOwhm2)
{$ENDIF}
  then
  begin
    Exit
  end;

end;

procedure TModflowFmp4Writer.WriteFileInternal;
begin

end;

procedure TModflowFmp4Writer.WriteGobalDimension;
begin

end;

procedure TModflowFmp4Writer.WriteLandUse;
begin

end;

procedure TModflowFmp4Writer.WriteOutput;
begin

end;

procedure TModflowFmp4Writer.WriteSalinityFlush;
begin

end;

procedure TModflowFmp4Writer.WriteSoil;
begin

end;

procedure TModflowFmp4Writer.WriteSupplyWell;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceWater;
begin

end;

procedure TModflowFmp4Writer.WriteSurfaceWaterIrrigation;
begin

end;

procedure TModflowFmp4Writer.WriteWaterBalanceSubregion;
begin

end;

end.
