unit ColorSchemesInterface;

interface

uses
  GoPhastTypes, Graphics, System.Classes;

type
  IColorItem = interface(IInterface)
    ['{D06811FF-EA91-40C7-A6D5-63BA1A42F01A}']
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetFraction: double;
    procedure SetFraction(const Value: double);
    property Color: TColor read GetColor write SetColor;
    property Fraction: double read GetFraction write SetFraction;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    property Index : Integer read GetIndex write SetIndex;
  end;

  IColorCollection = interface(IInterface)
    ['{1246E69C-AF09-4794-9A4D-8896EAE17492}']
    function GetItem(Index: Integer): IColorItem;
    procedure SetItem(Index: Integer; const Value: IColorItem);
    function Add: IColorItem;
    property Items[Index: Integer]: IColorItem read GetItem write SetItem; default;
    procedure Clear;
    function GetCount: integer;
    property Count: Integer read GetCount;
    procedure SortColors;
  end;

  IUserDefinedColorSchemeItem = interface(IInterface)
    ['{1856660C-90AE-48B1-9A42-ADB9EED86CB0}']
    function GetName: string;
    procedure SetName(const Value: string);
    function GetColorI: IColorCollection;
    procedure SetColorI(const Value: IColorCollection);
    property Name: string read GetName write SetName;
    property ColorsI: IColorCollection read GetColorI write SetColorI;
    procedure Assign(Source: TPersistent);
  end;

  IUserDefinedColorSchemeCollection = interface(IInterface)
    ['{3A075A8B-90D6-4F13-93DE-239EF4A9713D}']
    function GetItem(Index: Integer): IUserDefinedColorSchemeItem;
    procedure SetItem(Index: Integer; const Value: IUserDefinedColorSchemeItem);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    property Items[Index: Integer]: IUserDefinedColorSchemeItem
      read GetItem write SetItem; default;
    property Count: Integer read GetCount write SetCount;
    procedure Assign(Source: TPersistent);
  end;

  IModelForTUserDefinedColorSchemeCollection = interface(IModelMuseModel)
    ['{18FF0542-7AD6-4544-9A9D-520C55C61477}']
    function GetColorSchemesI: IUserDefinedColorSchemeCollection;
    procedure SetColorSchemesI(const Value: IUserDefinedColorSchemeCollection);
    property ColorSchemesI: IUserDefinedColorSchemeCollection read GetColorSchemesI write SetColorSchemesI;
  end;


implementation

end.
