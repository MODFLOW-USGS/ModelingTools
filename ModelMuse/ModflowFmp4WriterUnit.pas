{@name writes the FMP input file for MODFLOW-OWHM version 2.}
unit ModflowFmp4WriterUnit;

interface

uses
  CustomModflowWriterUnit;

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
  end;


implementation

{ TModflowFmp4Writer }

procedure TModflowFmp4Writer.WriteAllotments;
begin

end;

procedure TModflowFmp4Writer.WriteClimate;
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
