unit OrderedCollectionInterfaceUnit;

interface

uses
  GoPhastTypes, DataSetUnit, RbwParser, ModelMuseInterfaceUnit,
  FormulaManagerUnit, SubscriptionUnit;

type
  ICustomModelInterfaceForTOrderedCollection = interface(IModelMuseModel)
    ['{24F678A0-6BF6-4CA1-AC98-17C5E5038DD0}']
    procedure RemoveVariables(const DataSet: TDataArray); overload;
    procedure RemoveVariables(const DataSetName: String;
      Orientation: TDataSetOrientation; EvaluatedAt: TEvaluatedAt); overload;
    function GetDataArrayInterface: ISimpleDataArrayManager;
    procedure UpdateFormulaDependencies(OldFormula: string;
      var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
    function CreateBlockFormulaObject(Orientation: TDataSetOrientation): TFormulaObject;
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    property FormulaCompiler[const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt]:TRbwParser read GetCompiler;
    procedure ChangeFormula(var FormulaObject: TFormulaObject;
      NewFormula: string; EvaluatedAt: TEvaluatedAt; OnRemoveSubscription,
      OnRestoreSubscription: TChangeSubscription; Subject: TObject);
    function GetClearing: Boolean;
    property Clearing: Boolean read GetClearing;
//    function GetDataArrayManager: TDataArrayManager;
//    property DataArrayManager: TDataArrayManager read GetDataArrayManager;
  end;

  IModelForTGwtPestMethodCollection = interface(IModelMuseModel)
    ['{B8CAB9A3-1233-40DD-A9ED-2E546FC3074F}']
    function GetMobileComponentCount: Integer;
  end;

  IModelForTLandUsePestMethodCollection = interface(IModelMuseModel)
    ['{B60D8E20-E259-485E-B66D-15E4E52249A1}']
    function CropCount: integer;
  end;

implementation

end.
