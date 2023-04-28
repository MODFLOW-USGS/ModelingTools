unit AbstractGridInterfaceUnit;

interface

uses
  GoPhastTypes, FastGEO;

type
  ICustomModelGrid = interface
    ['{65266ECC-8EA8-4A78-B3CA-645441E6EC7A}']
    function RotateFromGridCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;  overload;
    procedure RotateFromGridCoordinatesToRealWorldCoordinates(var X, Y: TFloat); overload;
  end;

  IModelForCustomModelGrid = interface
    ['{46AC4F44-D94E-4361-B4F7-2ABA162AA991}']
    function GetGridI: ICustomModelGrid;
    property GridI: ICustomModelGrid read GetGridI;
  end;

implementation

end.
