unit ModelCellInterfaceUnit;

interface

uses
  GoPhastTypes, ScreenObjectInterfaceUnit;

type
  IModelForTValueCell = interface
    ['{481F8586-F327-4CC1-B830-A94018E991E9}']
    function GetScreenObjectInterfaceByName(AName: string): IScreenObject;
  end;

implementation

end.
