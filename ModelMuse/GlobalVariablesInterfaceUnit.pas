unit GlobalVariablesInterfaceUnit;

interface

uses
  System.Classes, GoPhastTypes, OrderedCollectionInterfaceUnit,
  SubscriptionInterfaceUnit, RbwParser;

type
  IGlobalVariable = interface(IObserver)
    ['{ECDE707F-3E8E-43FA-B97C-2B818722D706}']
    function GetFormat: TRbwDataType;
    procedure SetFormat(const Value: TRbwDataType);
    property Format: TRbwDataType read GetFormat write SetFormat;
    function GetComment: string;
    procedure SetComment(const Value: string);
    property Comment: string read GetComment write SetComment;
    function GetBooleanValue: boolean;
    function GetIntegerValue: integer;
    function GetRealValue: double;
    function GetStringValue: string;
    procedure SetBooleanValue(const Value: boolean);
    procedure SetIntegerValue(const Value: integer);
    procedure SetRealValue(const Value: double);
    procedure SetStringValue(const Value: string);
    property RealValue: double read GetRealValue write SetRealValue;
    property IntegerValue: integer read GetIntegerValue write SetIntegerValue;
    property BooleanValue: boolean read GetBooleanValue write SetBooleanValue;
    property StringValue: string read GetStringValue write SetStringValue;
  end;

  IGlobalVariables = interface(IOrderedCollection)
    ['{7214C305-16D1-423D-B0A2-9B8E0986E79A}']
    function GetGlobalVariableNames: TStringList;
    property GlobalVariableNames: TStringList read GetGlobalVariableNames;
    function IndexOfVariable(Name: string): integer;
    function GetVariableByName(Const Name: string): IGlobalVariable;
    function GetVariable(Index: integer): IGlobalVariable;
    procedure SetVariable(Index: integer; const Value: IGlobalVariable);
    property Variables[Index: integer]: IGlobalVariable read GetVariable
      write SetVariable; default;
    procedure Clear;
    procedure Delete(Index: Integer);
  end;

implementation

end.
