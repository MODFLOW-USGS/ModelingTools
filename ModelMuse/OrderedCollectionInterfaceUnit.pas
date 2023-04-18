unit OrderedCollectionInterfaceUnit;

interface

uses
  GoPhastTypes, RbwParser, ModelMuseInterfaceUnit,
  FormulaManagerInterfaceUnit,
  SubscriptionInterfaceUnit,
  ScreenObjectInterfaceUnit,
  DataArrayInterfaceUnit;

type
  IModelForTOrderedCollection = interface(IModelMuseModel)
    ['{24F678A0-6BF6-4CA1-AC98-17C5E5038DD0}']
    procedure RemoveVariables(const DataSet: IDataArray); overload;
    procedure RemoveVariables(const DataSetName: String;
      Orientation: TDataSetOrientation; EvaluatedAt: TEvaluatedAt); overload;
    function GetDataArrayInterface: ISimpleDataArrayManager;
    procedure UpdateFormulaDependencies(OldFormula: string;
      var NewFormula: string; Observer: IObserver; Compiler: TRbwParser);
//    function CreateBlockFormulaObject(Orientation: TDataSetOrientation): TObject;
    function CreateBlockFormulaObjectI(Orientation: TDataSetOrientation): IFormulaObject;
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    property FormulaCompiler[const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt]:TRbwParser read GetCompiler;
    procedure ChangeFormula(var FormulaObject: IFormulaObject;
      NewFormula: string; EvaluatedAt: TEvaluatedAt; OnRemoveSubscription,
      OnRestoreSubscription: TChangeSubscription; Subject: TObject);
    function GetClearing: Boolean;
    property Clearing: Boolean read GetClearing;
    function GetScreenObjectInterface(const Index: integer): IScreenObject;
    function GetScreenObjectCount: integer;
    property ScreenObjectCount: integer read GetScreenObjectCount;
    property ScreenObjectInterfaces[const Index: integer]: IScreenObject
      read GetScreenObjectInterface;
    procedure InvalidateMfRchRate(Sender: TObject);
    procedure InvalidateMfEvtEvapRate(Sender: TObject);
    procedure InvalidateMfEtsEvapRate(Sender: TObject);
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

  IModelForTPilotPointObsGrp = interface(IModelMuseModel)
    ['{1276892C-199B-4E89-A956-2A6FB136B0B8}']
    function GetObsGroupFromName(const Value: string): TObject;
  end;

  IPhastModelForTLayerOwnerCollection = interface(IModelForTOrderedCollection)
    ['{C7B8776C-8183-4F60-8232-B7AA15A9554F}']
    function GetChildModelCount: Integer;
    function GetChildModel(Index: Integer): IModelForTOrderedCollection;
  end;

  IModelForTModflowParameter = interface(IModelForTOrderedCollection)
    ['{EF8B92F8-B3CA-4444-9CEC-8D364E8D0F70}']
    procedure NotifyHufKx;
    procedure NotifyHufKy;
    procedure NotifyHufKz;
    procedure NotifyHufSS;
    procedure NotifyHufSy;
  end;

  IOrderedItem = interface
    ['{6A44459F-D802-4A96-9D31-A6C8E4602DA8}']
    function IsSame(AnotherItem: IOrderedItem): boolean; overload;
  end;

  IOrderedCollection = interface
    ['{B1C9519C-75D5-406A-ACEC-4FF0FC1D39C7}']
  end;

implementation

end.
