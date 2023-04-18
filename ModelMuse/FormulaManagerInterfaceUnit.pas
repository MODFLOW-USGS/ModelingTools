unit FormulaManagerInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  TChangeSubscription = procedure (Sender: TObject;
    Subject: TObject; const AName: string);
  PChangeSubscription = ^TChangeSubscription;


implementation

end.
