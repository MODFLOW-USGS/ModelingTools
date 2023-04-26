unit ScreenObjectInterfaceUnit;

interface

uses
  GoPhastTypes, ModflowBoundaryInterfaceUnit;

type
  IScreenObject = interface(IInterface)
    ['{07E0F57F-E191-42E4-924F-634C12C5000B}']
    function GetMfBoundaryI(ParamType: TParameterType): IModflowParamBoundary;
    function RchParameters: IModflowParameters;
    function EvtParameters: IModflowParameters;
    function EtsParameters: IModflowParameters;
    function ChdParameters: IModflowParameters;
    function GhbParameters: IModflowParameters;
    function WelParameters: IModflowParameters;
    function RivParameters: IModflowParameters;
    function DrnParameters: IModflowParameters;
    function DrtParameters: IModflowParameters;
    procedure DeleteSfrParameter(const ParameterName: string);
    procedure DeleteHfbParameter(const ParameterName: string);
    function StrParameters: IModflowParameters;
    function FmpWellParameters: IModflowParameters;
    procedure HandleChangedHfbParameter(const ParameterName: string);
    // See @link(TElevationCount).
    function GetElevationCount: TElevationCount;
    property ElevationCount: TElevationCount read GetElevationCount;
  end;

implementation

end.
