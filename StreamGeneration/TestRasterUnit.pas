unit TestRasterUnit;

interface

uses
  PitRemovalUnit, Winapi.Windows, GenericRasterUnit;

type
  TTestRaster = class(TCustomGenericRaster)
  private
    procedure SetIgnore(XIndex, YIndex: Integer; const Value: Boolean);
    procedure SetZ(XIndex, YIndex: Integer; const Value: double);
  public
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property Z[XIndex, YIndex: Integer]: double read GetZ write SetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore write SetIgnore;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TTestRaster }

destructor TTestRaster.Destroy;
begin
//  Beep;
  inherited;
end;

procedure TTestRaster.SetIgnore(XIndex, YIndex: Integer; const Value: Boolean);
begin
  FIgnore[XIndex, YIndex] := Value;
end;

procedure TTestRaster.SetZ(XIndex, YIndex: Integer; const Value: double);
begin
  FZ[XIndex, YIndex] := Value;
end;

end.
