unit DataArrayInterfaceUnit;

interface

uses
  GoPhastTypes, System.Generics.Collections, System.Classes, RbwParser;

type
  IDataArray = interface(IInterface)
    ['{A6E5A6B8-72D1-42BE-AA4E-20FB5A76BAD6}']
    function GetName: TComponentName;
    procedure SetName(const Value: TComponentName);
    property Name: TComponentName read GetName write SetName;
    procedure Free;
    procedure Initialize;
    function GetDataType: TRbwDataType;
    function GetLayerCount: integer;
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
    function GetBooleanData(const Layer, Row, Col: integer): boolean;
    function GetIntegerData(const Layer, Row, Col: integer): integer;
    function GetRealData(const Layer, Row, Col: integer): double;
    function GetStringData(const Layer, Row, Col: integer): string;

    property LayerCount: integer read GetLayerCount;
    property DataType: TRbwDataType read GetDataType;
    property IsValue[const Layer, Row, Col: Integer]: boolean read
      GetIsValue;
    property BooleanData[const Layer, Row, Col: integer]: boolean read
      GetBooleanData;
    property IntegerData[const Layer, Row, Col: integer]: integer read
      GetIntegerData;
    property RealData[const Layer, Row, Col: integer]: double read GetRealData;
    property StringData[const Layer, Row, Col: integer]: string read
      GetStringData;
  end;

  TIDataArrayList = class(TList<IDataArray>);

implementation

end.
