unit InterpolatedObsCellUnit;

interface

uses
  System.Generics.Collections;

type
  TInterpolatedObsCell = class(TObject)
    Layer: integer;
    Row: Integer;
    Col: Integer;
    Name: string;
    Fraction: double;
    ObsNumber: integer;
  end;

  TInterpolatedObsCellObjectList = TObjectList<TInterpolatedObsCell>;

  TBaseInterpolatedObs = class(TObject)
  protected
    FCells: TInterpolatedObsCellObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    property Cells: TInterpolatedObsCellObjectList read FCells;
  end;

  TBaseInterpolatedObsObjectList = TObjectList<TBaseInterpolatedObs>;


implementation



{ TCustomInterpolatedObs }

constructor TBaseInterpolatedObs.Create;
begin
  FCells := TInterpolatedObsCellObjectList.Create;
end;

destructor TBaseInterpolatedObs.Destroy;
begin
  FCells.Free;
  inherited;
end;

end.
