unit Modflow6DynamicTimeSeriesInterfaceUnit;

interface

uses
  System.Classes, GoPhastTypes, SubscriptionUnit,
  FormulaManagerUnit, RbwParser;

type
  IModelForDynamicTimeSeries = interface(IModelMuseModel)
    ['{3ED8161F-DA7D-49D4-BB2C-AC774A26B683}']
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    property FormulaCompiler[const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt]:TRbwParser read GetCompiler;
    function AddFormulaObject: TFormulaObject;
    procedure RemoveFormulaObject(FormulaObject: TFormulaObject;
      OnRemoveSubscription, OnRestoreSubscription:TChangeSubscription;
      Subject: TObject);
    function GetObserverByName(const ObserverName: string): TObserver;
  end;

implementation

end.
