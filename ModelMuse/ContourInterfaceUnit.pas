unit ContourInterfaceUnit;

interface

uses
  FastGEO;

type
  T2DGridPoint = record
    P: TPoint2D;
    Value: TFloat;
    Active: boolean;
  end;

  P2DGridPoint = ^T2DGridPoint;
  TGridSquare = record
    // Points to 4 grid points (square)
    GridPoint : Array [0..3] of P2DGridPoint;
  end;

  T2DGrid = array of array of T2DGridPoint;
  TSquares = array of array of TGridSquare;

implementation

end.
