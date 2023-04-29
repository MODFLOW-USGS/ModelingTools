unit FormulaManagerInterfaceUnit;

interface

uses
  GoPhastTypes, RbwParser, System.Generics.Collections,
  ScreenObjectInterfaceUnit;

type
  TChangeSubscription = procedure (Sender: TObject;
    Subject: TObject; const AName: string);
  PChangeSubscription = ^TChangeSubscription;

  IFormulaObject = interface
    ['{D58A6272-CCF2-4A46-94C0-8372D9BD7B45}']
    function GetParser: TRbwParser;
    procedure SetParser(const Value: TRbwParser);
    property Parser: TRbwParser read GetParser write SetParser;
    procedure AddSubscriptionEvents(OnRemoveSubscription,
      OnRestoreSubscription: TChangeSubscription; Subject: TObject);
    function GetFormula: string;
    property Formula: string read GetFormula;
    function GetScreenObject: IScreenObject;
    procedure SetScreenObject(const Value: IScreenObject);
    property ScreenObject: IScreenObject read GetScreenObject write SetScreenObject;
  end;

  TIformulaList = class(TList<IFormulaObject>);

implementation

end.
