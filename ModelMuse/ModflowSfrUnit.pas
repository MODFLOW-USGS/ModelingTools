unit ModflowSfrUnit;

interface

uses Classes, RealListUnit, OrderedCollectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, ModflowSfrReachUnit, ModflowSfrChannelUnit, GoPhastTypes,
  ModflowSfrSegment, ModflowSfrUnsatSegment, ModflowSfrTable, ModflowSfrFlows,
  ModflowSfrEquationUnit, ModflowSfrParamIcalcUnit, PestObsUnit,
  SubscriptionUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit;

type
  TGageLocation = (glNone, glFirst, glLast, glAll);

  {@name tells how the external flow file is specified.
  ffcNone = no external flow file.
  ffcFileName = an external file created by the user.
  ffcSpecify = the data is stored in ModelMuse.
  }
  TFlowFileChoice = (ffcNone, ffcFileName, ffcSpecify);

  {For external flow files whose data is stored in ModelMuse, @name indicates
  what to use as the reference time.}
  TFlowFileReferenceTime = (ffrtModelMuseZero, ffrtStartOfModel);

  {When an external flow file has its data stored in ModelMuse,
  @name represents one line of the data.}
  TFlowFileItem = class(TPhastCollectionItem)
  private
    FTime: double;
    FInflow: double;
    procedure SetInflow(const Value: double);
    procedure SetTime(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Time: double read FTime write SetTime;
    property Inflow: double read FInflow write SetInflow;
  end;

  TFlowFileCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetItem(index: Integer): TFlowFileItem;
    procedure SetItem(index: Integer; const Value: TFlowFileItem);
  public
    function Add: TFlowFileItem;
    constructor Create(Model: TBaseModel);
    property Items[index: Integer]: TFlowFileItem read GetItem write SetItem; default;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TExternalFlowProperties = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FFlowFileData: TFlowFileCollection;
    FFullFlowFileName: string;
    FFlowFileChoice: TFlowFileChoice;
    FReferenceTimeChoice: TFlowFileReferenceTime;
    function GetReferenceTimeChoice: TFlowFileReferenceTime;
    function SaveFlowFileData: Boolean;
    procedure SetFlowFileChoice(const Value: TFlowFileChoice);
    procedure SetFlowFileData(const Value: TFlowFileCollection);
    procedure SetFullFlowFileName(const Value: string);
    procedure SetReferenceTimeChoice(const Value: TFlowFileReferenceTime);
    function GetFlowFileName: string;
    procedure SetFlowFileName(const Value: string);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Clear;
    property FullFlowFileName: string read FFullFlowFileName write SetFullFlowFileName;
  published
    property FlowFileChoice: TFlowFileChoice read FFlowFileChoice
      write SetFlowFileChoice;
    property ReferenceTimeChoice: TFlowFileReferenceTime
      read GetReferenceTimeChoice write SetReferenceTimeChoice;
    property FlowFileData: TFlowFileCollection read FFlowFileData
      write SetFlowFileData stored SaveFlowFileData;
    property FlowFileName: string read GetFlowFileName write SetFlowFileName;
  end;

  TSfrObs = class(TCustomTimeObservationItem)
  private
    FObsType: Integer;
    procedure SetObsType(const Value: Integer);
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(Value: Integer); override;
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    function ObservationType: string; override;
    function Units: string; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ObsType: Integer read FObsType write SetObsType stored True;
    property GUID;
  end;

  TSfrObservations = class(TCustomComparisonCollection)
  private
    FGageOutputName: string;
    FGageOutputNames: TStringList;
    function GetSfrItem(Index: Integer): TSfrObs;
    procedure SetSfrItem(Index: Integer; const Value: TSfrObs);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    destructor Destroy; override;
    property Items[Index: Integer]: TSfrObs read GetSfrItem
      write SetSfrItem; default;
    function Add: TSfrObs;
    property GageOutputName: string read FGageOutputName write FGageOutputName;
    property GageOutputNames: TStringList read FGageOutputNames;
  end;

  // @name represents the MODFLOW Stream Flow Routing boundaries associated with
  // a single @link(TScreenObject).
  //
  // @link(TModflowBoundary.Values) is a @link(TSfrCollection)
  // and represents the stream properties at each reach.
  //
  // @seealso(TSfrCollection)
  TSfrBoundary = class(TModflowBoundary)
  private
    FSegmentNumber: integer;
    FChannelValues: TSfrChannelCollection;
    FUpstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FUpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FTableCollection: TSfrTableCollection;
    FSegmentFlows: TSfrSegmentFlowCollection;
    FEquationValues: TSfrEquationCollection;
    FIFSROPT: integer;
    FParamIcalc: TSfrParamIcalcCollection;
    FGage0: boolean;
    FGage1: boolean;
    FGage2: boolean;
    FGage3: boolean;
    FGage5: boolean;
    FGage6: boolean;
    FGage7: boolean;
    FGageLocation: TGageLocation;
    FExternalFlow: TExternalFlowProperties;
    FObservations: TSfrObservations;
    FPestPrecipMethod: TPestParamMethod;
    FPestUpstreamKMethod: TPestParamMethod;
    FPestDownstreamWidthMethod: TPestParamMethod;
    FPestDownstreamBedThicknessMethod: TPestParamMethod;
    FPestDownstreamBedElevationMethod: TPestParamMethod;
    FPestUpstreamWidthMethod: TPestParamMethod;
    FPestEvapMethod: TPestParamMethod;
    FPestDownstreamDepthMethod: TPestParamMethod;
    FPestUpstreamBedThicknessMethod: TPestParamMethod;
    FPestUpstreamBedElevationMethod: TPestParamMethod;
    FPestFlowMethod: TPestParamMethod;
    FPestRunoffMethod: TPestParamMethod;
    FPestUpstreamDepthMethod: TPestParamMethod;
    FPestDownstreamKMethod: TPestParamMethod;
    FPestUpstreamKFormula: IFormulaObject;
    FPestUpstreamBedThicknessFormula: IFormulaObject;
    FPestUpstreamBedElevationFormula: IFormulaObject;
    FPestUpstreamWidthFormula: IFormulaObject;
    FPestUpstreamDepthFormula: IFormulaObject;
    FPestDownstreamKFormula: IFormulaObject;
    FPestDownstreamBedThicknessFormula: IFormulaObject;
    FPestDownstreamBedElevationFormula: IFormulaObject;
    FPestDownstreamWidthFormula: IFormulaObject;
    FPestDownstreamDepthFormula: IFormulaObject;
    FPestFlowFormula: IFormulaObject;
    FPestPrecipFormula: IFormulaObject;
    FPestEvapFormula: IFormulaObject;
    FPestRunoffFormula: IFormulaObject;
    FPestDownstreamBedElevationObserver: TObserver;
    FPestDownstreamBedThicknessObserver: TObserver;
    FPestDownstreamDepthObserver: TObserver;
    FPestDownstreamKObserver: TObserver;
    FPestDownstreamWidthObserver: TObserver;
    FPestEvapObserver: TObserver;
    FPestFlowObserver: TObserver;
    FPestPrecipObserver: TObserver;
    FPestRunoffObserver: TObserver;
    FPestUpstreamBedElevationObserver: TObserver;
    FPestUpstreamBedThicknessObserver: TObserver;
    FPestUpstreamDepthObserver: TObserver;
    FPestUpstreamKObserver: TObserver;
    FPestUpstreamWidthObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetSegmentNumber(const Value: integer);
    procedure SetChannelValues(const Value: TSfrChannelCollection);
    procedure SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetUpstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetTableCollection(const Value: TSfrTableCollection);
    procedure SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
    procedure SetEquationValues(const Value: TSfrEquationCollection);
    function GetISFROPT: integer;
    procedure SetParamIcalc(const Value: TSfrParamIcalcCollection);
    procedure InvalidateDisplayTimeLists;
    procedure SetGage0(const Value: boolean);
    procedure SetGage1(const Value: boolean);
    procedure SetGage2(const Value: boolean);
    procedure SetGage5(const Value: boolean);
    procedure SetGage6(const Value: boolean);
    procedure SetGage7(const Value: boolean);
    procedure SetGage3(const Value: boolean);
    function GetOutTypes: TByteSet;
    procedure SetGageLocation(const Value: TGageLocation);
    procedure SetExternalFlow(const Value: TExternalFlowProperties);
    procedure SetObservations(const Value: TSfrObservations);
    function GetPestDownstreamBedElevationFormula: string;
    function GetPestDownstreamBedThicknessFormula: string;
    function GetPestDownstreamDepthFormula: string;
    function GetPestDownstreamKFormula: string;
    function GetPestDownstreamWidthFormula: string;
    function GetPestEvapFormula: string;
    function GetPestFlowFormula: string;
    function GetPestPrecipFormula: string;
    function GetPestRunoffFormula: string;
    function GetPestUpstreamBedElevationFormula: string;
    function GetPestUpstreamBedThicknessFormula: string;
    function GetPestUpstreamDepthFormula: string;
    function GetPestUpstreamKFormula: string;
    function GetPestUpstreamWidthFormula: string;
    procedure SetPestDownstreamBedElevationFormula(const Value: string);
    procedure SetPestDownstreamBedElevationMethod(
      const Value: TPestParamMethod);
    procedure SetPestDownstreamBedThicknessFormula(const Value: string);
    procedure SetPestDownstreamBedThicknessMethod(
      const Value: TPestParamMethod);
    procedure SetPestDownstreamDepthFormula(const Value: string);
    procedure SetPestDownstreamDepthMethod(const Value: TPestParamMethod);
    procedure SetPestDownstreamKFormula(const Value: string);
    procedure SetPestDownstreamKMethod(const Value: TPestParamMethod);
    procedure SetPestDownstreamWidthFormula(const Value: string);
    procedure SetPestDownstreamWidthMethod(const Value: TPestParamMethod);
    procedure SetPestEvapFormula(const Value: string);
    procedure SetPestEvapMethod(const Value: TPestParamMethod);
    procedure SetPestFlowFormula(const Value: string);
    procedure SetPestFlowMethod(const Value: TPestParamMethod);
    procedure SetPestPrecipFormula(const Value: string);
    procedure SetPestPrecipMethod(const Value: TPestParamMethod);
    procedure SetPestRunoffFormula(const Value: string);
    procedure SetPestRunoffMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamBedElevationFormula(const Value: string);
    procedure SetPestUpstreamBedElevationMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamBedThicknessFormula(const Value: string);
    procedure SetPestUpstreamBedThicknessMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamDepthFormula(const Value: string);
    procedure SetPestUpstreamDepthMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamKFormula(const Value: string);
    procedure SetPestUpstreamKMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamWidthFormula(const Value: string);
    procedure SetPestUpstreamWidthMethod(const Value: TPestParamMethod);
    function GetPestDownstreamBedElevationObserver: TObserver;
    function GetPestDownstreamBedThicknessObserver: TObserver;
    function GetPestDownstreamDepthObserver: TObserver;
    function GetPestDownstreamKObserver: TObserver;
    function GetPestDownstreamWidthObserver: TObserver;
    function GetPestEvapObserver: TObserver;
    function GetPestFlowObserver: TObserver;
    function GetPestPrecipObserver: TObserver;
    function GetPestRunoffObserver: TObserver;
    function GetPestUpstreamBedElevationObserver: TObserver;
    function GetPestUpstreamBedThicknessObserver: TObserver;
    function GetPestUpstreamDepthObserver: TObserver;
    function GetPestUpstreamKObserver: TObserver;
    function GetPestUpstreamWidthObserver: TObserver;
    procedure InvalidateUpstreamKData(Sender: TObject);
    procedure InvalidateDownstreamKData(Sender: TObject);
    procedure InvalidateUpstreamBedThicknessData(Sender: TObject);
    procedure InvalidateDownstreamBedThicknessData(Sender: TObject);
    procedure InvalidateUpstreamBedElevationData(Sender: TObject);
    procedure InvalidateDownstreamBedElevationData(Sender: TObject);
    procedure InvalidateUpstreamWidthData(Sender: TObject);
    procedure InvalidateDownstreamWidthData(Sender: TObject);
    procedure InvalidateUpstreamDepthData(Sender: TObject);
    procedure InvalidateDownstreamDepthData(Sender: TObject);
    procedure InvalidateFlowData(Sender: TObject);
    procedure InvalidatePrecipData(Sender: TObject);
    procedure InvalidateEvapData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
//    function GetPestDownstreamBrooksCoreyExponentFormula: string;
//    function GetPestDownstreamInitialWaterContentFormula: string;
//    function GetPestDownstreamSaturatedWaterContentFormula: string;
//    function GetPestDownstreamVerticalSaturatedKFormula: string;
//    function GetPestUpstreamBrooksCoreyExponentFormula: string;
//    function GetPestUpstreamInitialWaterContentFormula: string;
//    function GetPestUpstreamSaturatedWaterContentFormula: string;
//    function GetPestUpstreamVerticalSaturatedKFormula: string;
//    procedure SetPestDownstreamBrooksCoreyExponentFormula(const Value: string);
//    procedure SetPestDownstreamBrooksCoreyExponentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestDownstreamInitialWaterContentFormula(const Value: string);
//    procedure SetPestDownstreamInitialWaterContentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestDownstreamSaturatedWaterContentFormula(
//      const Value: string);
//    procedure SetPestDownstreamSaturatedWaterContentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestDownstreamVerticalSaturatedKFormula(const Value: string);
//    procedure SetPestDownstreamVerticalSaturatedKMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestUpstreamBrooksCoreyExponentFormula(const Value: string);
//    procedure SetPestUpstreamBrooksCoreyExponentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestUpstreamInitialWaterContentFormula(const Value: string);
//    procedure SetPestUpstreamInitialWaterContentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestUpstreamSaturatedWaterContentFormula(const Value: string);
//    procedure SetPestUpstreamSaturatedWaterContentMethod(
//      const Value: TPestParamMethod);
//    procedure SetPestUpstreamVerticalSaturatedKFormula(const Value: string);
//    procedure SetPestUpstreamVerticalSaturatedKMethod(
//      const Value: TPestParamMethod);
//    procedure InvalidateUpstreamSaturatedWaterContentData(Sender: TObject);
//    procedure InvalidateDownstreamSaturatedWaterContentData(Sender: TObject);
//    procedure InvalidateUpstreamInitialWaterContentData(Sender: TObject);
//    procedure InvalidateDownstreamInitialWaterContentData(Sender: TObject);
//    procedure InvalidateUpstreamBrooksCoreyExponentData(Sender: TObject);
//    procedure InvalidateDownstreamBrooksCoreyExponentData(Sender: TObject);
//    procedure InvalidateUpstreamVerticalSaturatedKData(Sender: TObject);
//    procedure InvalidateDownstreamVerticalSaturatedKData(Sender: TObject);
//    function GetPestDownstreamBrooksCoreyExponentObserver: TObserver;
//    function GetPestDownstreamInitialWaterContentObserver: TObserver;
//    function GetPestDownstreamSaturatedWaterContentObserver: TObserver;
//    function GetPestDownstreamVerticalSaturatedKObserver: TObserver;
//    function GetPestUpstreamBrooksCoreyExponentObserver: TObserver;
//    function GetPestUpstreamInitialWaterContentObserver: TObserver;
//    function GetPestUpstreamSaturatedWaterContentObserver: TObserver;
//    function GetPestUpstreamVerticalSaturatedKObserver: TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TSfr_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestUpstreamKObserver: TObserver read GetPestUpstreamKObserver;
    property PestDownstreamKObserver: TObserver read GetPestDownstreamKObserver;
    property PestUpstreamBedThicknessObserver: TObserver read GetPestUpstreamBedThicknessObserver;
    property PestDownstreamBedThicknessObserver: TObserver read GetPestDownstreamBedThicknessObserver;
    property PestUpstreamBedElevationObserver: TObserver read GetPestUpstreamBedElevationObserver;
    property PestDownstreamBedElevationObserver: TObserver read GetPestDownstreamBedElevationObserver;
    property PestUpstreamWidthObserver: TObserver read GetPestUpstreamWidthObserver;
    property PestDownstreamWidthObserver: TObserver read GetPestDownstreamWidthObserver;
    property PestUpstreamDepthObserver: TObserver read GetPestUpstreamDepthObserver;
    property PestDownstreamDepthObserver: TObserver read GetPestDownstreamDepthObserver;

//    property PestUpstreamSaturatedWaterContentObserver: TObserver read GetPestUpstreamSaturatedWaterContentObserver;
//    property PestDownstreamSaturatedWaterContentObserver: TObserver read GetPestDownstreamSaturatedWaterContentObserver;
//    property PestUpstreamInitialWaterContentObserver: TObserver read GetPestUpstreamInitialWaterContentObserver;
//    property PestDownstreamInitialWaterContentObserver: TObserver read GetPestDownstreamInitialWaterContentObserver;
//    property PestUpstreamBrooksCoreyExponentObserver: TObserver read GetPestUpstreamBrooksCoreyExponentObserver;
//    property PestDownstreamBrooksCoreyExponentObserver: TObserver read GetPestDownstreamBrooksCoreyExponentObserver;
//    property PestUpstreamVerticalSaturatedKObserver: TObserver read GetPestUpstreamVerticalSaturatedKObserver;
//    property PestDownstreamVerticalSaturatedKObserver: TObserver read GetPestDownstreamVerticalSaturatedKObserver;

    property PestFlowObserver: TObserver read GetPestFlowObserver;
    property PestPrecipObserver: TObserver read GetPestPrecipObserver;
    property PestEvapObserver: TObserver read GetPestEvapObserver;
    property PestRunoffObserver: TObserver read GetPestRunoffObserver;
  public
    procedure InvalidateSegmentNumberArray;
    procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // @link(TSfrStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW SFR parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TSfrStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    property ISFROPT: integer read GetISFROPT write FIFSROPT;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    procedure Clear; override;
    property OutTypes: TByteSet read GetOutTypes;
    procedure FixCollections;
    procedure ReplaceGUID;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    // @name was mispelled. It is now @link(SegmentNumber).
    property SegementNumber: integer read FSegmentNumber
      write SetSegmentNumber stored False;
    property SegmentNumber: integer read FSegmentNumber
      write SetSegmentNumber;
    property ChannelValues: TSfrChannelCollection read FChannelValues
      write SetChannelValues;
    property UpstreamSegmentValues: TSfrSegmentCollection
      read FUpstreamSegmentValues write SetUpstreamSegmentValues;
    property DownstreamSegmentValues: TSfrSegmentCollection
      read FDownstreamSegmentValues write SetDownstreamSegmentValues;
    property UpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FUpstreamUnsatSegmentValues write SetUpstreamUnsatSegmentValues;
    property DownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FDownstreamUnsatSegmentValues write SetDownstreamUnsatSegmentValues;
    property TableCollection: TSfrTableCollection read FTableCollection
      write SetTableCollection;
    property SegmentFlows: TSfrSegmentFlowCollection read FSegmentFlows
      write SetSegmentFlows;
    property EquationValues: TSfrEquationCollection read FEquationValues
      write SetEquationValues;
    property ParamIcalc: TSfrParamIcalcCollection read FParamIcalc
      write SetParamIcalc;
    property Gage0: boolean read FGage0 write SetGage0;
    property Gage1: boolean read FGage1 write SetGage1;
    property Gage2: boolean read FGage2 write SetGage2;
    property Gage3: boolean read FGage3 write SetGage3;
    property Gage5: boolean read FGage5 write SetGage5;
    property Gage6: boolean read FGage6 write SetGage6;
    property Gage7: boolean read FGage7 write SetGage7;
    property GageLocation: TGageLocation read FGageLocation
      write SetGageLocation;
    property ExternalFlow: TExternalFlowProperties read FExternalFlow
      write SetExternalFlow;
    property Observations: TSfrObservations read FObservations
      write SetObservations;
    property PestUpstreamKFormula: string read GetPestUpstreamKFormula
      write SetPestUpstreamKFormula;
    property PestUpstreamKMethod: TPestParamMethod
      read FPestUpstreamKMethod write SetPestUpstreamKMethod;
    property PestDownstreamKFormula: string read GetPestDownstreamKFormula
      write SetPestDownstreamKFormula;
    property PestDownstreamKMethod: TPestParamMethod
      read FPestDownstreamKMethod write SetPestDownstreamKMethod;
    property PestUpstreamBedThicknessFormula: string
      read GetPestUpstreamBedThicknessFormula
      write SetPestUpstreamBedThicknessFormula;
    property PestUpstreamBedThicknessMethod: TPestParamMethod
      read FPestUpstreamBedThicknessMethod
      write SetPestUpstreamBedThicknessMethod;
    property PestDownstreamBedThicknessFormula: string
      read GetPestDownstreamBedThicknessFormula
      write SetPestDownstreamBedThicknessFormula;
    property PestDownstreamBedThicknessMethod: TPestParamMethod
      read FPestDownstreamBedThicknessMethod
      write SetPestDownstreamBedThicknessMethod;
    property PestUpstreamBedElevationFormula: string
      read GetPestUpstreamBedElevationFormula
      write SetPestUpstreamBedElevationFormula;
    property PestUpstreamBedElevationMethod: TPestParamMethod
      read FPestUpstreamBedElevationMethod
      write SetPestUpstreamBedElevationMethod;
    property PestDownstreamBedElevationFormula: string
      read GetPestDownstreamBedElevationFormula
      write SetPestDownstreamBedElevationFormula;
    property PestDownstreamBedElevationMethod: TPestParamMethod
      read FPestDownstreamBedElevationMethod
      write SetPestDownstreamBedElevationMethod;
    property PestUpstreamWidthFormula: string read GetPestUpstreamWidthFormula
      write SetPestUpstreamWidthFormula;
    property PestUpstreamWidthMethod: TPestParamMethod
      read FPestUpstreamWidthMethod write SetPestUpstreamWidthMethod;
    property PestDownstreamWidthFormula: string
      read GetPestDownstreamWidthFormula
      write SetPestDownstreamWidthFormula;
    property PestDownstreamWidthMethod: TPestParamMethod
      read FPestDownstreamWidthMethod write SetPestDownstreamWidthMethod;
    property PestUpstreamDepthFormula: string read GetPestUpstreamDepthFormula
      write SetPestUpstreamDepthFormula;
    property PestUpstreamDepthMethod: TPestParamMethod
      read FPestUpstreamDepthMethod write SetPestUpstreamDepthMethod;
    property PestDownstreamDepthFormula: string
      read GetPestDownstreamDepthFormula
      write SetPestDownstreamDepthFormula;
    property PestDownstreamDepthMethod: TPestParamMethod
      read FPestDownstreamDepthMethod write SetPestDownstreamDepthMethod;
    property PestFlowFormula: string read GetPestFlowFormula
      write SetPestFlowFormula;
    property PestFlowMethod: TPestParamMethod
      read FPestFlowMethod write SetPestFlowMethod;
    property PestPrecipFormula: string read GetPestPrecipFormula
      write SetPestPrecipFormula;
    property PestPrecipMethod: TPestParamMethod
      read FPestPrecipMethod write SetPestPrecipMethod;
    property PestEvapFormula: string read GetPestEvapFormula
      write SetPestEvapFormula;
    property PestEvapMethod: TPestParamMethod
      read FPestEvapMethod write SetPestEvapMethod;
    property PestRunoffFormula: string read GetPestRunoffFormula
      write SetPestRunoffFormula;
    property PestRunoffMethod: TPestParamMethod
      read FPestRunoffMethod write SetPestRunoffMethod;
  end;

resourcestring
  StrIncompleteSFRData = 'Incomplete SFR data';

const
  UpstreamKPosition = 0;
  UpstreamBedThicknessPosition = 1;
  UpstreamBedElevationPosition = 2;
  UpstreamWidthPosition = 3;
  UpstreamDepthPosition = 4;

//  UpstreamSaturatedWaterContentPosition = 5;
//  UpstreamInitialWaterContentPosition = 6;
//  UpstreamBrooksCoreyExponentPosition = 7;
//  UpstreamVerticalSaturatedKPosition = 8;

  DownstreamKPosition = 5;
  DownstreamBedThicknessPosition = 6;
  DownstreamBedElevationPosition = 7;
  DownstreamWidthPosition = 8;
  DownstreamDepthPosition = 9;

//  DownstreamSaturatedWaterContentPosition = 14;
//  DownstreamInitialWaterContentPosition = 15;
//  DownstreamBrooksCoreyExponentPosition = 16;
//  DownstreamVerticalSaturatedKPosition = 17;

  FlowPosition = 10;
  PrecipPosition = 11;
  EvapPosition = 12;
  RunoffPosition = 13;

var
  StreamGageOutputTypes: TStringList;
  StreamGageOutputTypeUnits: TStringList;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  SysUtils, frmGoPhastUnit;

{ TSfrBoundary }

procedure TSfrBoundary.Assign(Source: TPersistent);
var
  Sfr: TSfrBoundary;
  Index: Integer;
begin
  if Source is TSfrBoundary then
  begin
    Sfr := TSfrBoundary(Source);
    if Used <> Sfr.Used then
    begin
      if (ParentModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;
    ISFROPT := Sfr.ISFROPT;
    SegmentNumber := Sfr.SegmentNumber;

    ChannelValues := Sfr.ChannelValues;
    UpstreamSegmentValues := Sfr.UpstreamSegmentValues;
    DownstreamSegmentValues := Sfr.DownstreamSegmentValues;
    UpstreamUnsatSegmentValues := Sfr.UpstreamUnsatSegmentValues;
    DownstreamUnsatSegmentValues := Sfr.DownstreamUnsatSegmentValues;
    TableCollection := Sfr.TableCollection;
    SegmentFlows := Sfr.SegmentFlows;
    EquationValues := Sfr.EquationValues;
    ParamIcalc := Sfr.ParamIcalc;
    Gage0 := Sfr.Gage0;
    Gage1 := Sfr.Gage1;
    Gage2 := Sfr.Gage2;
    Gage3 := Sfr.Gage3;
    Gage5 := Sfr.Gage5;
    Gage6 := Sfr.Gage6;
    Gage7 := Sfr.Gage7;
    GageLocation := Sfr.GageLocation;
    ExternalFlow := Sfr.ExternalFlow;
    Observations := Sfr.Observations;

    for Index := UpstreamKPosition to RunoffPosition do
    begin
      PestBoundaryFormula[Index] := Sfr.PestBoundaryFormula[Index];
      PestBoundaryMethod[Index] := Sfr.PestBoundaryMethod[Index];
    end;

  end;
  inherited;
end;

procedure TSfrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TSfr_Cell;
  BoundaryValues: TSfrRecord;
  BoundaryIndex: Integer;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSfrStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSfrStorage;
  TimeIndex := 0;
  if TimeIndex < ValueTimeList.Count then
  begin
    Cells := ValueTimeList[TimeIndex];
  end
  else
  begin
    Cells := TValueCellList.Create(TSfr_Cell);
    ValueTimeList.Add(Cells);
  end;

  if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.SfrArray) then
  begin
    Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.SfrArray)
  end;
//  Cells.CheckRestore;
  for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SfrArray) - 1 do
  begin
    BoundaryValues := LocalBoundaryStorage.SfrArray[BoundaryIndex];
    Cell := TSfr_Cell.Create;
    Assert(ScreenObject <> nil);
    Cell.IFace := (ScreenObject as TScreenObject).IFace;
    Cells.Add(Cell);
    Cell.StressPeriod := TimeIndex;
    Cell.Values := BoundaryValues;
    Cell.ScreenObject := ScreenObject;
    LocalModel.AdjustCellPosition(Cell);
  end;
  Cells.Cache;
  LocalBoundaryStorage.CacheData;
end;

class function TSfrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSfrCollection;
end;

function TSfrBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestSfr2005_';
end;

procedure TSfrBoundary.Clear;
begin
  inherited;
  ChannelValues.Clear;
  UpstreamSegmentValues.Clear;
  DownstreamSegmentValues.Clear;
  UpstreamUnsatSegmentValues.Clear;
  DownstreamUnsatSegmentValues.Clear;
  TableCollection.Clear;
  SegmentFlows.Clear;
  EquationValues.Clear;
  ParamIcalc.Clear;
end;

constructor TSfrBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  OnInvalidateModelEvent: TNotifyEvent;
  Index: Integer;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited;
  if Model <> nil then
  begin
    ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end;
  FParamIcalc := TSfrParamIcalcCollection.Create(self, Model as TCustomModel, ScreenObject);
  FChannelValues := TSfrChannelCollection.Create(self, Model as TCustomModel, ScreenObject);
  FUpstreamSegmentValues := TSfrSegmentCollection.Create(self, Model as TCustomModel, ScreenObject);
  FUpstreamSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamSegmentValues := TSfrSegmentCollection.Create(self, Model as TCustomModel, ScreenObject);
  FDownstreamSegmentValues.AssignmentLocation := alLastVertex;
  FUpstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model as TCustomModel, ScreenObject);
  FUpstreamUnsatSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model as TCustomModel, ScreenObject);
  FDownstreamUnsatSegmentValues.AssignmentLocation := alLastVertex;
  FTableCollection := TSfrTableCollection.Create(self, Model as TCustomModel, ScreenObject);
  FSegmentFlows := TSfrSegmentFlowCollection.Create(self, Model as TCustomModel, ScreenObject);
  FEquationValues := TSfrEquationCollection.Create(self, Model as TCustomModel, ScreenObject);
  FExternalFlow := TExternalFlowProperties.Create(Model as TCustomModel);

  FObservations := TSfrObservations.Create(OnInvalidateModelEvent, ScreenObject);

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  for Index := UpstreamKPosition to RunoffPosition do
  begin
    PestBoundaryFormula[Index] := '';
    PestBoundaryMethod[Index] := DefaultBoundaryMethod(Index);
  end;
end;

procedure TSfrBoundary.CreateFormulaObjects;
begin
  FPestUpstreamKFormula := CreateFormulaObjectBlocks(dso3D);
  FPestUpstreamBedThicknessFormula := CreateFormulaObjectBlocks(dso3D);
  FPestUpstreamBedElevationFormula := CreateFormulaObjectBlocks(dso3D);
  FPestUpstreamWidthFormula := CreateFormulaObjectBlocks(dso3D);
  FPestUpstreamDepthFormula := CreateFormulaObjectBlocks(dso3D);

//  FPestUpstreamSaturatedWaterContentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestUpstreamInitialWaterContentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestUpstreamBrooksCoreyExponentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestUpstreamVerticalSaturatedKFormula := CreateFormulaObjectBlocks(dso3D);

  FPestDownstreamKFormula := CreateFormulaObjectBlocks(dso3D);
  FPestDownstreamBedThicknessFormula := CreateFormulaObjectBlocks(dso3D);
  FPestDownstreamBedElevationFormula := CreateFormulaObjectBlocks(dso3D);
  FPestDownstreamWidthFormula := CreateFormulaObjectBlocks(dso3D);
  FPestDownstreamDepthFormula := CreateFormulaObjectBlocks(dso3D);

//  FPestDownstreamSaturatedWaterContentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestDownstreamInitialWaterContentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestDownstreamBrooksCoreyExponentFormula := CreateFormulaObjectBlocks(dso3D);
//  FPestDownstreamVerticalSaturatedKFormula := CreateFormulaObjectBlocks(dso3D);

  FPestFlowFormula := CreateFormulaObjectBlocks(dso3D);
  FPestPrecipFormula := CreateFormulaObjectBlocks(dso3D);
  FPestEvapFormula := CreateFormulaObjectBlocks(dso3D);
  FPestRunoffFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TSfrBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestUpstreamKObserver);
    FObserverList.Add(PestUpstreamBedThicknessObserver);
    FObserverList.Add(PestUpstreamBedElevationObserver);
    FObserverList.Add(PestUpstreamWidthObserver);
    FObserverList.Add(PestUpstreamDepthObserver);

//    FObserverList.Add(PestUpstreamSaturatedWaterContentObserver);
//    FObserverList.Add(PestUpstreamInitialWaterContentObserver);
//    FObserverList.Add(PestUpstreamBrooksCoreyExponentObserver);
//    FObserverList.Add(PestUpstreamVerticalSaturatedKObserver);

    FObserverList.Add(PestDownstreamKObserver);
    FObserverList.Add(PestDownstreamBedThicknessObserver);
    FObserverList.Add(PestDownstreamBedElevationObserver);
    FObserverList.Add(PestDownstreamWidthObserver);
    FObserverList.Add(PestDownstreamDepthObserver);

//    FObserverList.Add(PestDownstreamSaturatedWaterContentObserver);
//    FObserverList.Add(PestDownstreamInitialWaterContentObserver);
//    FObserverList.Add(PestDownstreamBrooksCoreyExponentObserver);
//    FObserverList.Add(PestDownstreamVerticalSaturatedKObserver);

    FObserverList.Add(PestFlowObserver);
    FObserverList.Add(PestPrecipObserver);
    FObserverList.Add(PestEvapObserver);
    FObserverList.Add(PestRunoffObserver);
  end;
end;

class function TSfrBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UpstreamKPosition:
      begin
        result := ppmMultiply;
      end;
    UpstreamBedThicknessPosition:
      begin
        result := ppmMultiply;
      end;
    UpstreamBedElevationPosition:
      begin
        result := ppmAdd;
      end;
    UpstreamWidthPosition:
      begin
        result := ppmMultiply;
      end;
    UpstreamDepthPosition:
      begin
        result := ppmMultiply;
      end;

//    UpstreamSaturatedWaterContentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    UpstreamInitialWaterContentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    UpstreamBrooksCoreyExponentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    UpstreamVerticalSaturatedKPosition:
//      begin
//        result := ppmMultiply;
//      end;

    DownstreamKPosition:
      begin
        result := ppmMultiply;
      end;
    DownstreamBedThicknessPosition:
      begin
        result := ppmMultiply;
      end;
    DownstreamBedElevationPosition:
      begin
        result := ppmAdd;
      end;
    DownstreamWidthPosition:
      begin
        result := ppmMultiply;
      end;
    DownstreamDepthPosition:
      begin
        result := ppmMultiply;
      end;

//    DownstreamSaturatedWaterContentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    DownstreamInitialWaterContentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    DownstreamBrooksCoreyExponentPosition:
//      begin
//        result := ppmMultiply;
//      end;
//    DownstreamVerticalSaturatedKPosition:
//      begin
//        result := ppmMultiply;
//      end;

    FlowPosition:
      begin
        result := ppmMultiply;
      end;
    PrecipPosition:
      begin
        result := ppmMultiply;
      end;
    EvapPosition:
      begin
        result := ppmMultiply;
      end;
    RunoffPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TSfrBoundary.Destroy;
var
  Index: Integer;
begin
  FreeAndNil(FObservations);
  FreeAndNil(FExternalFlow);
  FreeAndNil(FEquationValues);
  FreeAndNil(FSegmentFlows);
  FreeAndNil(FTableCollection);
  FreeAndNil(FDownstreamUnsatSegmentValues);
  FreeAndNil(FUpstreamUnsatSegmentValues);
  FreeAndNil(FDownstreamSegmentValues);
  FreeAndNil(FUpstreamSegmentValues);
  FreeAndNil(FChannelValues);
  FreeAndNil(FParamIcalc);
  for Index := UpstreamKPosition to RunoffPosition do
  begin
    PestBoundaryFormula[Index] := '';
  end;
  inherited;
end;

procedure TSfrBoundary.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
var
  LocalModel: TCustomModel;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  Item: TCustomModflowBoundaryItem;
begin
  inherited;
  ChannelValues.EvaluateBoundaries(AModel);
  UpstreamSegmentValues.EvaluateArrayBoundaries(AModel, Writer);
  DownstreamSegmentValues.EvaluateArrayBoundaries(AModel, Writer);
  LocalModel := AModel as TCustomModel;
  if (UpstreamUnsatSegmentValues.Count > 0)
    or (DownstreamUnsatSegmentValues.Count > 0) then
  begin
    FirstUsedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
    LastUsedTime := LocalModel.ModflowFullStressPeriods[
      LocalModel.ModflowFullStressPeriods.Count - 1].EndTime;
    if UpstreamUnsatSegmentValues.Count > 0 then
    begin
      Item := UpstreamUnsatSegmentValues[0] as TCustomModflowBoundaryItem;
      Item.StartTime := FirstUsedTime;
      Item.EndTime := LastUsedTime;
    end;
    if DownstreamUnsatSegmentValues.Count > 0 then
    begin
      Item := DownstreamUnsatSegmentValues[0] as TCustomModflowBoundaryItem;
      Item.StartTime := FirstUsedTime;
      Item.EndTime := LastUsedTime;
    end;
  end;
  UpstreamUnsatSegmentValues.EvaluateArrayBoundaries(AModel, Writer);
  DownstreamUnsatSegmentValues.EvaluateArrayBoundaries(AModel, Writer);

  EquationValues.EvaluateBoundaries;
  TableCollection.EvaluateBoundaries;
  SegmentFlows.EvaluateBoundaries;
end;

procedure TSfrBoundary.FixCollections;
var
  ItemIndex: Integer;
begin
  if ChannelValues.Count >
    ParamIcalc.Count then
  begin
    while ChannelValues.Count
      > ParamIcalc.Count do
    begin
      ChannelValues.Last.Free;
    end;
    for ItemIndex := 0 to ChannelValues.Count - 1 do
    begin
      ChannelValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (ChannelValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if UpstreamSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while UpstreamSegmentValues.Count
      > ParamIcalc.Count do
    begin
      UpstreamSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to UpstreamSegmentValues.Count - 1 do
    begin
      UpstreamSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (UpstreamSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if DownstreamSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while DownstreamSegmentValues.Count
      > ParamIcalc.Count do
    begin
      DownstreamSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to DownstreamSegmentValues.Count - 1 do
    begin
      DownstreamSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (DownstreamSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if UpstreamUnsatSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while UpstreamUnsatSegmentValues.Count
      > ParamIcalc.Count do
    begin
      UpstreamUnsatSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to UpstreamUnsatSegmentValues.Count - 1 do
    begin
      UpstreamUnsatSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (UpstreamUnsatSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if DownstreamUnsatSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while DownstreamUnsatSegmentValues.Count
      > ParamIcalc.Count do
    begin
      DownstreamUnsatSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to DownstreamUnsatSegmentValues.Count - 1 do
    begin
      DownstreamUnsatSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (DownstreamUnsatSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if SegmentFlows.Count >
    ParamIcalc.Count then
  begin
    while SegmentFlows.Count
      > ParamIcalc.Count do
    begin
      SegmentFlows.Last.Free;
    end;
    for ItemIndex := 0 to SegmentFlows.Count - 1 do
    begin
      SegmentFlows[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (SegmentFlows[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if EquationValues.Count >
    ParamIcalc.Count then
  begin
    while EquationValues.Count
      > ParamIcalc.Count do
    begin
      EquationValues.Last.Free;
    end;
    for ItemIndex := 0 to EquationValues.Count - 1 do
    begin
      EquationValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (EquationValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
end;

procedure TSfrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TSfrStorage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSfrStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TSfrBoundary.GetISFROPT: integer;
begin
  if ParentModel <> nil then
  begin
    result := (ParentModel as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end
  else
  begin
    Result := FIFSROPT;
  end;
end;

function TSfrBoundary.GetOutTypes: TByteSet;
begin
  result := [];
  if Gage0 then
  begin
    Include(result, 0);
  end;
  if Gage1 then
  begin
    Include(result, 1);
    Exclude(result, 0);
  end;
  if Gage2 then
  begin
    Include(result, 2);
    Exclude(result, 0);
  end;
  if Gage3 then
  begin
    Include(result, 3);
    Exclude(result, 0);
  end;
  if (result = [1,2,3])
    or ((Observations.Count > 0)  and (GageLocation <> glAll)) then
  begin
    result := [4];
  end;
  if Gage5 then
  begin
    Include(result, 5);
  end;
  if Gage6 then
  begin
    Include(result, 6);
  end;
  if Gage7 then
  begin
    Include(result, 7);
  end;
end;

function TSfrBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    UpstreamKPosition:
      begin
        result := PestUpstreamKFormula;
      end;
    UpstreamBedThicknessPosition:
      begin
        result := PestUpstreamBedThicknessFormula;
      end;
    UpstreamBedElevationPosition:
      begin
        result := PestUpstreamBedElevationFormula;
      end;
    UpstreamWidthPosition:
      begin
        result := PestUpstreamWidthFormula;
      end;
    UpstreamDepthPosition:
      begin
        result := PestUpstreamDepthFormula;
      end;

//    UpstreamSaturatedWaterContentPosition:
//      begin
//        result := PestUpstreamSaturatedWaterContentFormula;
//      end;
//    UpstreamInitialWaterContentPosition:
//      begin
//        result := PestUpstreamInitialWaterContentFormula;
//      end;
//    UpstreamBrooksCoreyExponentPosition:
//      begin
//        result := PestUpstreamBrooksCoreyExponentFormula;
//      end;
//    UpstreamVerticalSaturatedKPosition:
//      begin
//        result := PestUpstreamVerticalSaturatedKFormula;
//      end;

    DownstreamKPosition:
      begin
        result := PestDownstreamKFormula;
      end;
    DownstreamBedThicknessPosition:
      begin
        result := PestDownstreamBedThicknessFormula;
      end;
    DownstreamBedElevationPosition:
      begin
        result := PestDownstreamBedElevationFormula;
      end;
    DownstreamWidthPosition:
      begin
        result := PestDownstreamWidthFormula;
      end;
    DownstreamDepthPosition:
      begin
        result := PestDownstreamDepthFormula;
      end;

//    DownstreamSaturatedWaterContentPosition:
//      begin
//        result := PestDownstreamSaturatedWaterContentFormula;
//      end;
//    DownstreamInitialWaterContentPosition:
//      begin
//        result := PestDownstreamInitialWaterContentFormula;
//      end;
//    DownstreamBrooksCoreyExponentPosition:
//      begin
//        result := PestDownstreamBrooksCoreyExponentFormula;
//      end;
//    DownstreamVerticalSaturatedKPosition:
//      begin
//        result := PestDownstreamVerticalSaturatedKFormula;
//      end;

    FlowPosition:
      begin
        result := PestFlowFormula;
      end;
    PrecipPosition:
      begin
        result := PestPrecipFormula;
      end;
    EvapPosition:
      begin
        result := PestEvapFormula;
      end;
    RunoffPosition:
      begin
        result := PestRunoffFormula;
      end;
    else
      begin
        result := '';
        Assert(False);
      end;
  end;
end;

function TSfrBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UpstreamKPosition:
      begin
        result := PestUpstreamKMethod;
      end;
    UpstreamBedThicknessPosition:
      begin
        result := PestUpstreamBedThicknessMethod;
      end;
    UpstreamBedElevationPosition:
      begin
        result := PestUpstreamBedElevationMethod;
      end;
    UpstreamWidthPosition:
      begin
        result := PestUpstreamWidthMethod;
      end;
    UpstreamDepthPosition:
      begin
        result := PestUpstreamDepthMethod;
      end;

//    UpstreamSaturatedWaterContentPosition:
//      begin
//        result := PestUpstreamSaturatedWaterContentMethod;
//      end;
//    UpstreamInitialWaterContentPosition:
//      begin
//        result := PestUpstreamInitialWaterContentMethod;
//      end;
//    UpstreamBrooksCoreyExponentPosition:
//      begin
//        result := PestUpstreamBrooksCoreyExponentMethod;
//      end;
//    UpstreamVerticalSaturatedKPosition:
//      begin
//        result := PestUpstreamVerticalSaturatedKMethod;
//      end;

    DownstreamKPosition:
      begin
        result := PestDownstreamKMethod;
      end;
    DownstreamBedThicknessPosition:
      begin
        result := PestDownstreamBedThicknessMethod;
      end;
    DownstreamBedElevationPosition:
      begin
        result := PestDownstreamBedElevationMethod;
      end;
    DownstreamWidthPosition:
      begin
        result := PestDownstreamWidthMethod;
      end;
    DownstreamDepthPosition:
      begin
        result := PestDownstreamDepthMethod;
      end;

//    DownstreamSaturatedWaterContentPosition:
//      begin
//        result := PestDownstreamSaturatedWaterContentMethod;
//      end;
//    DownstreamInitialWaterContentPosition:
//      begin
//        result := PestDownstreamInitialWaterContentMethod;
//      end;
//    DownstreamBrooksCoreyExponentPosition:
//      begin
//        result := PestDownstreamBrooksCoreyExponentMethod;
//      end;
//    DownstreamVerticalSaturatedKPosition:
//      begin
//        result := PestDownstreamVerticalSaturatedKMethod;
//      end;

    FlowPosition:
      begin
        result := PestFlowMethod;
      end;
    PrecipPosition:
      begin
        result := PestPrecipMethod;
      end;
    EvapPosition:
      begin
        result := PestEvapMethod;
      end;
    RunoffPosition:
      begin
        result := PestRunoffMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSfrBoundary.GetPestDownstreamBedElevationFormula: string;
begin
  Result := FPestDownstreamBedElevationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DownstreamBedElevationPosition);
  end;
end;

function TSfrBoundary.GetPestDownstreamBedElevationObserver: TObserver;
begin
  if FPestDownstreamBedElevationObserver = nil then
  begin
    CreateObserver('PestDownstreamBedElevation_', FPestDownstreamBedElevationObserver, nil);
    FPestDownstreamBedElevationObserver.OnUpToDateSet := InvalidateDownstreamBedElevationData;
  end;
  result := FPestDownstreamBedElevationObserver;
end;

function TSfrBoundary.GetPestDownstreamBedThicknessFormula: string;
begin
  Result := FPestDownstreamBedThicknessFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DownstreamBedThicknessPosition);
  end;
end;

function TSfrBoundary.GetPestDownstreamBedThicknessObserver: TObserver;
begin
  if FPestDownstreamBedThicknessObserver = nil then
  begin
    CreateObserver('PestDownstreamBedThickness_', FPestDownstreamBedThicknessObserver, nil);
    FPestDownstreamBedThicknessObserver.OnUpToDateSet := InvalidateDownstreamBedThicknessData;
  end;
  result := FPestDownstreamBedThicknessObserver;
end;

//function TSfrBoundary.GetPestDownstreamBrooksCoreyExponentFormula: string;
//begin
//  Result := FPestDownstreamBrooksCoreyExponentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(DownstreamBrooksCoreyExponentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestDownstreamBrooksCoreyExponentObserver: TObserver;
//begin
//  if FPestDownstreamBrooksCoreyExponentObserver = nil then
//  begin
//    CreateObserver('PestDownstreamBrooksCoreyExponent_', FPestDownstreamBrooksCoreyExponentObserver, nil);
//    FPestDownstreamBrooksCoreyExponentObserver.OnUpToDateSet := InvalidateDownstreamBrooksCoreyExponentData;
//  end;
//  result := FPestDownstreamBrooksCoreyExponentObserver;
//end;

function TSfrBoundary.GetPestDownstreamDepthFormula: string;
begin
  Result := FPestDownstreamDepthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DownstreamDepthPosition);
  end;
end;

function TSfrBoundary.GetPestDownstreamDepthObserver: TObserver;
begin
  if FPestDownstreamDepthObserver = nil then
  begin
    CreateObserver('PestDownstreamDepth_', FPestDownstreamDepthObserver, nil);
    FPestDownstreamDepthObserver.OnUpToDateSet := InvalidateDownstreamDepthData;
  end;
  result := FPestDownstreamDepthObserver;
end;

//function TSfrBoundary.GetPestDownstreamInitialWaterContentFormula: string;
//begin
//  Result := FPestDownstreamInitialWaterContentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(DownstreamInitialWaterContentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestDownstreamInitialWaterContentObserver: TObserver;
//begin
//  if FPestDownstreamInitialWaterContentObserver = nil then
//  begin
//    CreateObserver('PestDownstreamInitialWaterContent_', FPestDownstreamInitialWaterContentObserver, nil);
//    FPestDownstreamInitialWaterContentObserver.OnUpToDateSet := InvalidateDownstreamInitialWaterContentData;
//  end;
//  result := FPestDownstreamInitialWaterContentObserver;
//end;

function TSfrBoundary.GetPestDownstreamKFormula: string;
begin
  Result := FPestDownstreamKFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DownstreamKPosition);
  end;
end;

function TSfrBoundary.GetPestDownstreamKObserver: TObserver;
begin
  if FPestDownstreamKObserver = nil then
  begin
    CreateObserver('PestDownstreamK_', FPestDownstreamKObserver, nil);
    FPestDownstreamKObserver.OnUpToDateSet := InvalidateDownstreamKData;
  end;
  result := FPestDownstreamKObserver;
end;

//function TSfrBoundary.GetPestDownstreamSaturatedWaterContentFormula: string;
//begin
//  Result := FPestDownstreamSaturatedWaterContentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(DownstreamSaturatedWaterContentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestDownstreamSaturatedWaterContentObserver: TObserver;
//begin
//  if FPestDownstreamSaturatedWaterContentObserver = nil then
//  begin
//    CreateObserver('PestDownstreamSaturatedWaterContent_', FPestDownstreamSaturatedWaterContentObserver, nil);
//    FPestDownstreamSaturatedWaterContentObserver.OnUpToDateSet := InvalidateDownstreamSaturatedWaterContentData;
//  end;
//  result := FPestDownstreamSaturatedWaterContentObserver;
//end;

//function TSfrBoundary.GetPestDownstreamVerticalSaturatedKFormula: string;
//begin
//  Result := FPestDownstreamVerticalSaturatedKFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(DownstreamVerticalSaturatedKPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestDownstreamVerticalSaturatedKObserver: TObserver;
//begin
//  if FPestDownstreamVerticalSaturatedKObserver = nil then
//  begin
//    CreateObserver('PestDownstreamVerticalSaturatedK_', FPestDownstreamVerticalSaturatedKObserver, nil);
//    FPestDownstreamVerticalSaturatedKObserver.OnUpToDateSet := InvalidateDownstreamVerticalSaturatedKData;
//  end;
//  result := FPestDownstreamVerticalSaturatedKObserver;
//end;

function TSfrBoundary.GetPestDownstreamWidthFormula: string;
begin
  Result := FPestDownstreamWidthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DownstreamWidthPosition);
  end;
end;

function TSfrBoundary.GetPestDownstreamWidthObserver: TObserver;
begin
  if FPestDownstreamWidthObserver = nil then
  begin
    CreateObserver('PestDownstreamWidth_', FPestDownstreamWidthObserver, nil);
    FPestDownstreamWidthObserver.OnUpToDateSet := InvalidateDownstreamWidthData;
  end;
  result := FPestDownstreamWidthObserver;
end;

function TSfrBoundary.GetPestEvapFormula: string;
begin
  Result := FPestEvapFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(EvapPosition);
  end;
end;

function TSfrBoundary.GetPestEvapObserver: TObserver;
begin
  if FPestEvapObserver = nil then
  begin
    CreateObserver('PestEvap_', FPestEvapObserver, nil);
    FPestEvapObserver.OnUpToDateSet := InvalidateEvapData;
  end;
  result := FPestEvapObserver;
end;

function TSfrBoundary.GetPestFlowFormula: string;
begin
  Result := FPestFlowFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(FlowPosition);
  end;
end;

function TSfrBoundary.GetPestFlowObserver: TObserver;
begin
  if FPestFlowObserver = nil then
  begin
    CreateObserver('PestFlow_', FPestFlowObserver, nil);
    FPestFlowObserver.OnUpToDateSet := InvalidateFlowData;
  end;
  result := FPestFlowObserver;
end;

function TSfrBoundary.GetPestPrecipFormula: string;
begin
  Result := FPestPrecipFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(PrecipPosition);
  end;
end;

function TSfrBoundary.GetPestPrecipObserver: TObserver;
begin
  if FPestPrecipObserver = nil then
  begin
    CreateObserver('PestPrecip_', FPestPrecipObserver, nil);
    FPestPrecipObserver.OnUpToDateSet := InvalidatePrecipData;
  end;
  result := FPestPrecipObserver;
end;

function TSfrBoundary.GetPestRunoffFormula: string;
begin
  Result := FPestRunoffFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RunoffPosition);
  end;
end;

function TSfrBoundary.GetPestRunoffObserver: TObserver;
begin
  if FPestRunoffObserver = nil then
  begin
    CreateObserver('PestRunoff_', FPestRunoffObserver, nil);
    FPestRunoffObserver.OnUpToDateSet := InvalidateRunoffData;
  end;
  result := FPestRunoffObserver;
end;

function TSfrBoundary.GetPestUpstreamBedElevationFormula: string;
begin
  Result := FPestUpstreamBedElevationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UpstreamBedElevationPosition);
  end;
end;

function TSfrBoundary.GetPestUpstreamBedElevationObserver: TObserver;
begin
  if FPestUpstreamBedElevationObserver = nil then
  begin
    CreateObserver('PestUpstreamBedElevation_', FPestUpstreamBedElevationObserver, nil);
    FPestUpstreamBedElevationObserver.OnUpToDateSet := InvalidateUpstreamBedElevationData;
  end;
  result := FPestUpstreamBedElevationObserver;
end;

function TSfrBoundary.GetPestUpstreamBedThicknessFormula: string;
begin
  Result := FPestUpstreamBedThicknessFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UpstreamBedThicknessPosition);
  end;
end;

function TSfrBoundary.GetPestUpstreamBedThicknessObserver: TObserver;
begin
  if FPestUpstreamBedThicknessObserver = nil then
  begin
    CreateObserver('PestUpstreamBedThickness_', FPestUpstreamBedThicknessObserver, nil);
    FPestUpstreamBedThicknessObserver.OnUpToDateSet := InvalidateUpstreamBedThicknessData;
  end;
  result := FPestUpstreamBedThicknessObserver;
end;

//function TSfrBoundary.GetPestUpstreamBrooksCoreyExponentFormula: string;
//begin
//  Result := FPestUpstreamBrooksCoreyExponentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(UpstreamBrooksCoreyExponentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestUpstreamBrooksCoreyExponentObserver: TObserver;
//begin
//  if FPestUpstreamBrooksCoreyExponentObserver = nil then
//  begin
//    CreateObserver('PestUpstreamBrooksCoreyExponent_', FPestUpstreamBrooksCoreyExponentObserver, nil);
//    FPestUpstreamBrooksCoreyExponentObserver.OnUpToDateSet := InvalidateUpstreamBrooksCoreyExponentData;
//  end;
//  result := FPestUpstreamBrooksCoreyExponentObserver;
//end;

function TSfrBoundary.GetPestUpstreamDepthFormula: string;
begin
  Result := FPestUpstreamDepthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UpstreamDepthPosition);
  end;
end;

function TSfrBoundary.GetPestUpstreamDepthObserver: TObserver;
begin
  if FPestUpstreamDepthObserver = nil then
  begin
    CreateObserver('PestUpstreamDepth_', FPestUpstreamDepthObserver, nil);
    FPestUpstreamDepthObserver.OnUpToDateSet := InvalidateUpstreamDepthData;
  end;
  result := FPestUpstreamDepthObserver;
end;

//function TSfrBoundary.GetPestUpstreamInitialWaterContentFormula: string;
//begin
//  Result := FPestUpstreamInitialWaterContentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(UpstreamInitialWaterContentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestUpstreamInitialWaterContentObserver: TObserver;
//begin
//  if FPestUpstreamInitialWaterContentObserver = nil then
//  begin
//    CreateObserver('PestUpstreamInitialWaterContent_', FPestUpstreamInitialWaterContentObserver, nil);
//    FPestUpstreamInitialWaterContentObserver.OnUpToDateSet := InvalidateUpstreamInitialWaterContentData;
//  end;
//  result := FPestUpstreamInitialWaterContentObserver;
//end;

function TSfrBoundary.GetPestUpstreamKFormula: string;
begin
  Result := FPestUpstreamKFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UpstreamKPosition);
  end;
end;

function TSfrBoundary.GetPestUpstreamKObserver: TObserver;
begin
  if FPestUpstreamKObserver = nil then
  begin
    CreateObserver('PestUpstreamK_', FPestUpstreamKObserver, nil);
    FPestUpstreamKObserver.OnUpToDateSet := InvalidateUpstreamKData;
  end;
  result := FPestUpstreamKObserver;
end;

//function TSfrBoundary.GetPestUpstreamSaturatedWaterContentFormula: string;
//begin
//  Result := FPestUpstreamSaturatedWaterContentFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(UpstreamSaturatedWaterContentPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestUpstreamSaturatedWaterContentObserver: TObserver;
//begin
//  if FPestUpstreamSaturatedWaterContentObserver = nil then
//  begin
//    CreateObserver('PestUpstreamSaturatedWaterContent_', FPestUpstreamSaturatedWaterContentObserver, nil);
//    FPestUpstreamSaturatedWaterContentObserver.OnUpToDateSet := InvalidateUpstreamSaturatedWaterContentData;
//  end;
//  result := FPestUpstreamSaturatedWaterContentObserver;
//end;

//function TSfrBoundary.GetPestUpstreamVerticalSaturatedKFormula: string;
//begin
//  Result := FPestUpstreamVerticalSaturatedKFormula.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetBoundaryObserver(UpstreamVerticalSaturatedKPosition);
//  end;
//end;
//
//function TSfrBoundary.GetPestUpstreamVerticalSaturatedKObserver: TObserver;
//begin
//  if FPestUpstreamVerticalSaturatedKObserver = nil then
//  begin
//    CreateObserver('PestUpstreamVerticalSaturatedK_', FPestUpstreamVerticalSaturatedKObserver, nil);
//    FPestUpstreamVerticalSaturatedKObserver.OnUpToDateSet := InvalidateUpstreamVerticalSaturatedKData;
//  end;
//  result := FPestUpstreamVerticalSaturatedKObserver;
//end;

function TSfrBoundary.GetPestUpstreamWidthFormula: string;
begin
  Result := FPestUpstreamWidthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UpstreamWidthPosition);
  end;
end;

function TSfrBoundary.GetPestUpstreamWidthObserver: TObserver;
begin
  if FPestUpstreamWidthObserver = nil then
  begin
    CreateObserver('PestUpstreamWidth_', FPestUpstreamWidthObserver, nil);
    FPestUpstreamWidthObserver.OnUpToDateSet := InvalidateUpstreamWidthData;
  end;
  result := FPestUpstreamWidthObserver;
end;

procedure TSfrBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestUpstreamKFormula as TObject then
  begin
    if UpstreamKPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UpstreamKPosition]);
    end;
  end;
  if Sender = FPestUpstreamBedThicknessFormula as TObject then
  begin
    if UpstreamBedThicknessPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UpstreamBedThicknessPosition]);
    end;
  end;
  if Sender = FPestUpstreamBedElevationFormula as TObject then
  begin
    if UpstreamBedElevationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UpstreamBedElevationPosition]);
    end;
  end;
  if Sender = FPestUpstreamWidthFormula as TObject then
  begin
    if UpstreamWidthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UpstreamWidthPosition]);
    end;
  end;
  if Sender = FPestUpstreamDepthFormula as TObject then
  begin
    if UpstreamDepthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UpstreamDepthPosition]);
    end;
  end;

  if Sender = FPestDownstreamKFormula as TObject then
  begin
    if DownstreamKPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DownstreamKPosition]);
    end;
  end;
  if Sender = FPestDownstreamBedThicknessFormula as TObject then
  begin
    if DownstreamBedThicknessPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DownstreamBedThicknessPosition]);
    end;
  end;
  if Sender = FPestDownstreamBedElevationFormula as TObject then
  begin
    if DownstreamBedElevationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DownstreamBedElevationPosition]);
    end;
  end;
  if Sender = FPestDownstreamWidthFormula as TObject then
  begin
    if DownstreamWidthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DownstreamWidthPosition]);
    end;
  end;
  if Sender = FPestDownstreamDepthFormula as TObject then
  begin
    if DownstreamDepthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DownstreamDepthPosition]);
    end;
  end;

  if Sender = FPestFlowFormula as TObject then
  begin
    if FlowPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[FlowPosition]);
    end;
  end;
  if Sender = FPestPrecipFormula as TObject then
  begin
    if PrecipPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[PrecipPosition]);
    end;
  end;
  if Sender = FPestEvapFormula as TObject then
  begin
    if EvapPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[EvapPosition]);
    end;
  end;
  if Sender = FPestRunoffFormula as TObject then
  begin
    if RunoffPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RunoffPosition]);
    end;
  end;

end;

function TSfrBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestSFR_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TSfrBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TSfrBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TSfrBoundary.SetChannelValues(const Value: TSfrChannelCollection);
begin
  FChannelValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamSegmentValues(
  const Value: TSfrSegmentCollection);
begin
  FDownstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FDownstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetEquationValues(const Value: TSfrEquationCollection);
begin
  FEquationValues.Assign(Value);
end;

procedure TSfrBoundary.SetExternalFlow(const Value: TExternalFlowProperties);
begin
  FExternalFlow.Assign(Value);
end;

procedure TSfrBoundary.SetGage0(const Value: boolean);
begin
  if FGage0 <> Value then
  begin
    InvalidateModel;
    FGage0 := Value;
  end;
end;

procedure TSfrBoundary.SetGage1(const Value: boolean);
begin
  if FGage1 <> Value then
  begin
    InvalidateModel;
    FGage1 := Value;
  end;
end;

procedure TSfrBoundary.SetGage2(const Value: boolean);
begin
  if FGage2 <> Value then
  begin
    InvalidateModel;
    FGage2 := Value;
  end;
end;

procedure TSfrBoundary.SetGage3(const Value: boolean);
begin
  if FGage3 <> Value then
  begin
    InvalidateModel;
    FGage3 := Value;
  end;
end;

procedure TSfrBoundary.SetGage5(const Value: boolean);
begin
  if FGage5 <> Value then
  begin
    InvalidateModel;
    FGage5 := Value;
  end;
end;

procedure TSfrBoundary.SetGage6(const Value: boolean);
begin
  if FGage6 <> Value then
  begin
    InvalidateModel;
    FGage6 := Value;
  end;
end;

procedure TSfrBoundary.SetGage7(const Value: boolean);
begin
  if FGage7 <> Value then
  begin
    InvalidateModel;
    FGage7 := Value;
  end;
end;

procedure TSfrBoundary.SetGageLocation(const Value: TGageLocation);
begin
  if FGageLocation <> Value then
  begin
    InvalidateModel;
    FGageLocation := Value;
  end;
end;

procedure TSfrBoundary.SetObservations(const Value: TSfrObservations);
begin
  FObservations.Assign(Value);
end;

procedure TSfrBoundary.SetParamIcalc(const Value: TSfrParamIcalcCollection);
begin
  FParamIcalc.Assign(Value);
end;

procedure TSfrBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    UpstreamKPosition:
      begin
        PestUpstreamKFormula := Value;
      end;
    UpstreamBedThicknessPosition:
      begin
        PestUpstreamBedThicknessFormula := Value;
      end;
    UpstreamBedElevationPosition:
      begin
        PestUpstreamBedElevationFormula := Value;
      end;
    UpstreamWidthPosition:
      begin
        PestUpstreamWidthFormula := Value;
      end;
    UpstreamDepthPosition:
      begin
        PestUpstreamDepthFormula := Value;
      end;

//    UpstreamSaturatedWaterContentPosition:
//      begin
//        PestUpstreamSaturatedWaterContentFormula := Value;
//      end;
//    UpstreamInitialWaterContentPosition:
//      begin
//        PestUpstreamInitialWaterContentFormula := Value;
//      end;
//    UpstreamBrooksCoreyExponentPosition:
//      begin
//        PestUpstreamBrooksCoreyExponentFormula := Value;
//      end;
//    UpstreamVerticalSaturatedKPosition:
//      begin
//        PestUpstreamVerticalSaturatedKFormula := Value;
//      end;

    DownstreamKPosition:
      begin
        PestDownstreamKFormula := Value;
      end;
    DownstreamBedThicknessPosition:
      begin
        PestDownstreamBedThicknessFormula := Value;
      end;
    DownstreamBedElevationPosition:
      begin
        PestDownstreamBedElevationFormula := Value;
      end;
    DownstreamWidthPosition:
      begin
        PestDownstreamWidthFormula := Value;
      end;
    DownstreamDepthPosition:
      begin
        PestDownstreamDepthFormula := Value;
      end;

//    DownstreamSaturatedWaterContentPosition:
//      begin
//        PestDownstreamSaturatedWaterContentFormula := Value;
//      end;
//    DownstreamInitialWaterContentPosition:
//      begin
//        PestDownstreamInitialWaterContentFormula := Value;
//      end;
//    DownstreamBrooksCoreyExponentPosition:
//      begin
//        PestDownstreamBrooksCoreyExponentFormula := Value;
//      end;
//    DownstreamVerticalSaturatedKPosition:
//      begin
//        PestDownstreamVerticalSaturatedKFormula := Value;
//      end;

    FlowPosition:
      begin
        PestFlowFormula := Value;
      end;
    PrecipPosition:
      begin
        PestPrecipFormula := Value;
      end;
    EvapPosition:
      begin
        PestEvapFormula := Value;
      end;
    RunoffPosition:
      begin
        PestRunoffFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSfrBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    UpstreamKPosition:
      begin
        PestUpstreamKMethod := Value;
      end;
    UpstreamBedThicknessPosition:
      begin
        PestUpstreamBedThicknessMethod := Value;
      end;
    UpstreamBedElevationPosition:
      begin
        PestUpstreamBedElevationMethod := Value;
      end;
    UpstreamWidthPosition:
      begin
        PestUpstreamWidthMethod := Value;
      end;
    UpstreamDepthPosition:
      begin
        PestUpstreamDepthMethod := Value;
      end;

//    UpstreamSaturatedWaterContentPosition:
//      begin
//        PestUpstreamSaturatedWaterContentMethod := Value;
//      end;
//    UpstreamInitialWaterContentPosition:
//      begin
//        PestUpstreamInitialWaterContentMethod := Value;
//      end;
//    UpstreamBrooksCoreyExponentPosition:
//      begin
//        PestUpstreamBrooksCoreyExponentMethod := Value;
//      end;
//    UpstreamVerticalSaturatedKPosition:
//      begin
//        PestUpstreamVerticalSaturatedKMethod := Value;
//      end;

    DownstreamKPosition:
      begin
        PestDownstreamKMethod := Value;
      end;
    DownstreamBedThicknessPosition:
      begin
        PestDownstreamBedThicknessMethod := Value;
      end;
    DownstreamBedElevationPosition:
      begin
        PestDownstreamBedElevationMethod := Value;
      end;
    DownstreamWidthPosition:
      begin
        PestDownstreamWidthMethod := Value;
      end;
    DownstreamDepthPosition:
      begin
        PestDownstreamDepthMethod := Value;
      end;

//    DownstreamSaturatedWaterContentPosition:
//      begin
//        PestDownstreamSaturatedWaterContentMethod := Value;
//      end;
//    DownstreamInitialWaterContentPosition:
//      begin
//        PestDownstreamInitialWaterContentMethod := Value;
//      end;
//    DownstreamBrooksCoreyExponentPosition:
//      begin
//        PestDownstreamBrooksCoreyExponentMethod := Value;
//      end;
//    DownstreamVerticalSaturatedKPosition:
//      begin
//        PestDownstreamVerticalSaturatedKMethod := Value;
//      end;

    FlowPosition:
      begin
        PestFlowMethod := Value;
      end;
    PrecipPosition:
      begin
        PestPrecipMethod := Value;
      end;
    EvapPosition:
      begin
        PestEvapMethod := Value;
      end;
    RunoffPosition:
      begin
        PestRunoffMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSfrBoundary.SetPestDownstreamBedElevationFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, DownstreamBedElevationPosition, FPestDownstreamBedElevationFormula);
end;

procedure TSfrBoundary.SetPestDownstreamBedElevationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDownstreamBedElevationMethod, Value);
end;

procedure TSfrBoundary.SetPestDownstreamBedThicknessFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, DownstreamBedThicknessPosition, FPestDownstreamBedThicknessFormula);
end;

procedure TSfrBoundary.SetPestDownstreamBedThicknessMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDownstreamBedThicknessMethod, Value);
end;

procedure TSfrBoundary.SetPestDownstreamDepthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DownstreamDepthPosition, FPestDownstreamDepthFormula);
end;

procedure TSfrBoundary.SetPestDownstreamDepthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDownstreamDepthMethod, Value);
end;

procedure TSfrBoundary.SetPestDownstreamKFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DownstreamKPosition, FPestDownstreamKFormula);
end;

procedure TSfrBoundary.SetPestDownstreamKMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDownstreamKMethod, Value);
end;

//procedure TSfrBoundary.SetPestDownstreamSaturatedWaterContentFormula(
//  const Value: string);
//begin
//  UpdateFormulaBlocks(Value, DownstreamSaturatedWaterContentPosition, FPestDownstreamSaturatedWaterContentFormula);
//end;
//
//procedure TSfrBoundary.SetPestDownstreamSaturatedWaterContentMethod(
//  const Value: TPestParamMethod);
//begin
//  SetPestParamMethod(FPestDownstreamSaturatedWaterContentMethod, Value);
//end;

//procedure TSfrBoundary.SetPestDownstreamVerticalSaturatedKFormula(
//  const Value: string);
//begin
//  UpdateFormulaBlocks(Value, DownstreamVerticalSaturatedKPosition, FPestDownstreamVerticalSaturatedKFormula);
//end;
//
//procedure TSfrBoundary.SetPestDownstreamVerticalSaturatedKMethod(
//  const Value: TPestParamMethod);
//begin
//  SetPestParamMethod(FPestDownstreamVerticalSaturatedKMethod, Value);
//end;

procedure TSfrBoundary.SetPestDownstreamWidthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DownstreamWidthPosition, FPestDownstreamWidthFormula);
end;

procedure TSfrBoundary.SetPestDownstreamWidthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDownstreamWidthMethod, Value);
end;

procedure TSfrBoundary.SetPestEvapFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, EvapPosition, FPestEvapFormula);
end;

procedure TSfrBoundary.SetPestEvapMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestEvapMethod, Value);
end;

procedure TSfrBoundary.SetPestFlowFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, FlowPosition, FPestFlowFormula);
end;

procedure TSfrBoundary.SetPestFlowMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestFlowMethod, Value);
end;

procedure TSfrBoundary.SetPestPrecipFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, PrecipPosition, FPestPrecipFormula);
end;

procedure TSfrBoundary.SetPestPrecipMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestPrecipMethod, Value);
end;

procedure TSfrBoundary.SetPestRunoffFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RunoffPosition, FPestRunoffFormula);
end;

procedure TSfrBoundary.SetPestRunoffMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRunoffMethod, Value);
end;

procedure TSfrBoundary.SetPestUpstreamBedElevationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UpstreamBedElevationPosition, FPestUpstreamBedElevationFormula);
end;

procedure TSfrBoundary.SetPestUpstreamBedElevationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamBedElevationMethod, Value);
end;

procedure TSfrBoundary.SetPestUpstreamBedThicknessFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UpstreamBedThicknessPosition, FPestUpstreamBedThicknessFormula);
end;

procedure TSfrBoundary.SetPestUpstreamBedThicknessMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamBedThicknessMethod, Value);
end;

procedure TSfrBoundary.SetPestUpstreamDepthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UpstreamDepthPosition, FPestUpstreamDepthFormula);
end;

procedure TSfrBoundary.SetPestUpstreamDepthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamDepthMethod, Value);
end;

//procedure TSfrBoundary.SetPestUpstreamInitialWaterContentFormula(
//  const Value: string);
//begin
//  UpdateFormulaBlocks(Value, UpstreamInitialWaterContentPosition, FPestUpstreamInitialWaterContentFormula);
//end;
//
//procedure TSfrBoundary.SetPestUpstreamInitialWaterContentMethod(
//  const Value: TPestParamMethod);
//begin
//  SetPestParamMethod(FPestUpstreamInitialWaterContentMethod, Value);
//end;

procedure TSfrBoundary.SetPestUpstreamKFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UpstreamKPosition, FPestUpstreamKFormula);
end;

procedure TSfrBoundary.SetPestUpstreamKMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamKMethod, Value);
end;

procedure TSfrBoundary.SetPestUpstreamWidthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UpstreamWidthPosition, FPestUpstreamWidthFormula);
end;

procedure TSfrBoundary.SetPestUpstreamWidthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamWidthMethod, Value);
end;

procedure TSfrBoundary.SetSegmentNumber(const Value: integer);
begin
  if FSegmentNumber <> Value then
  begin
    InvalidateModel;
    InvalidateSegmentNumberArray;
    FSegmentNumber := Value;
    if (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel
        and (ParentModel <> nil) then
    begin
      (ParentModel as TPhastModel).DischargeRoutingUpdate;
    end;
  end;
end;

procedure TSfrBoundary.SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
begin
  FSegmentFlows.Assign(Value);
end;

procedure TSfrBoundary.SetTableCollection(const Value: TSfrTableCollection);
begin
  FTableCollection.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
begin
  FUpstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FUpstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean; AModel: TBaseModel);
begin
  // it isn't clear whether or not inherited should be called.
  inherited;

  AddBoundaryTimes(ParamIcalc, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(ChannelValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(UpstreamSegmentValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(DownstreamSegmentValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(TableCollection, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(SegmentFlows, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(EquationValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);

// The unsatured data is stored in a TCustomModflowBoundaryItem
// but the start and end times are not used.

//  AddBoundaryTimes(UpstreamUnsatSegmentValues, Times, StartTestTime,
//    EndTestTime, StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(DownstreamUnsatSegmentValues, Times, StartTestTime,
//    EndTestTime, StartRangeExtended, EndRangeExtended);
end;

procedure TSfrBoundary.InvalidateDisplayTimeLists;
var
  Model: TPhastModel;
begin
  Model := ParentModel as TPhastModel;
  if Model.Clearing then
  begin
    Exit;
  end;
  Model.InvalidateMfSfrSegmentReachAndIcalc(self);
  Model.InvalidateMfSfrIprior(self);
  Model.InvalidateMfSfrVerticalUnsatK(self);
  Model.InvalidateMfSfrReachLength(self);
  Model.InvalidateMfSfrStreamTop(self);
  Model.InvalidateMfSfrStreamSlope(self);
  Model.InvalidateMfSfrStreamThickness(self);
  Model.InvalidateMfSfrStreamK(self);
  Model.InvalidateMfSfrSaturatedWaterContent(self);
  Model.InvalidateMfSfrInitialWaterContent(self);
  Model.InvalidateMfSfrBrooksCorey(self);
  Model.InvalidateMfSfrFlow(self);
  Model.InvalidateMfSfrRunoff(self);
  Model.InvalidateMfSfrPrecipitation(self);
  Model.InvalidateMfSfrEvapotranspiration(self);
  Model.InvalidateMfSfrChannelRoughness(self);
  Model.InvalidateMfSfrBankRoughness(self);
  Model.InvalidateMfSfrDepthCoefficient(self);
  Model.InvalidateMfSfrDepthExponent(self);
  Model.InvalidateMfSfrWidthCoefficient(self);
  Model.InvalidateMfSfrWidthExponent(self);
  Model.InvalidateMfSfrUpstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrDownstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrUpstreamWidth(self);
  Model.InvalidateMfSfrDownstreamWidth(self);
  Model.InvalidateMfSfrUpstreamThickness(self);
  Model.InvalidateMfSfrDownstreamThickness(self);
  Model.InvalidateMfSfrUpstreamElevation(self);
  Model.InvalidateMfSfrDownstreamElevation(self);
  Model.InvalidateMfSfrUpstreamDepth(self);
  Model.InvalidateMfSfrDownstreamDepth(self);
  Model.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrUpstreamBrooksCorey(self);
  Model.InvalidateMfSfrDownstreamBrooksCorey(self);
  Model.InvalidateMfSfrUpstreamUnsatKz(self);
  Model.InvalidateMfSfrDownstreamUnsatKz(self);
end;

procedure TSfrBoundary.InvalidateDownstreamBedElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrDownstreamElevation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrDownstreamElevation(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateDownstreamBedThicknessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrDownstreamThickness(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrDownstreamThickness(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateDownstreamBrooksCoreyExponentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrDownstreamBrooksCorey(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrDownstreamBrooksCorey(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateDownstreamDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrDownstreamDepth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrDownstreamDepth(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateDownstreamInitialWaterContentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateDownstreamKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateDownstreamSaturatedWaterContentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
//    end;
//  end;
//end;

//procedure TSfrBoundary.InvalidateDownstreamVerticalSaturatedKData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrDownstreamUnsatKz(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrDownstreamUnsatKz(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateDownstreamWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrDownstreamWidth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrDownstreamWidth(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateEvapData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrEvapotranspiration(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrEvapotranspiration(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateFlowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrFlow(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrFlow(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidatePrecipData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrPrecipitation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrPrecipitation(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateRunoffData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrRunoff(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrRunoff(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateSegmentNumberArray;
begin
  if (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel
    and (ParentModel <> nil) and not (ParentModel as TPhastModel).Clearing then
  begin
    (ParentModel as TPhastModel).InvalidateMfSfrSegmentReachAndIcalc(self);
  end;
end;

procedure TSfrBoundary.InvalidateUpstreamBedElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrUpstreamElevation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrUpstreamElevation(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.InvalidateUpstreamBedThicknessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrUpstreamThickness(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrUpstreamThickness(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateUpstreamBrooksCoreyExponentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrUpstreamBrooksCorey(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrUpstreamBrooksCorey(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateUpstreamDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrUpstreamDepth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrUpstreamDepth(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateUpstreamInitialWaterContentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateUpstreamKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);
      end;
    end;
  end;
end;

//procedure TSfrBoundary.InvalidateUpstreamSaturatedWaterContentData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
//    end;
//  end;
//end;

//procedure TSfrBoundary.InvalidateUpstreamVerticalSaturatedKData(
//  Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
////  if ParentModel = nil then
////  begin
////    Exit;
////  end;
////  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    PhastModel.InvalidateMfSfrDownstreamBrooksCorey(self);
//
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      ChildModel.InvalidateMfSfrDownstreamBrooksCorey(self);
//    end;
//  end;
//end;

procedure TSfrBoundary.InvalidateUpstreamWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfSfrUpstreamWidth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfSfrUpstreamWidth(self);
      end;
    end;
  end;
end;

procedure TSfrBoundary.ReplaceGUID;
begin
  Observations.ReplaceGUID;
end;

function TSfrBoundary.Used: boolean;
var
  LocalISFROPT: integer;
begin
  LocalISFROPT := ISFROPT;
  result := inherited Used;
  if result then
  begin
    result := (LocalISFROPT in [1,2,3])
      or ChannelValues.Used
      or ((LocalISFROPT in [0,4,5]) and
        UpstreamSegmentValues.Used and DownstreamSegmentValues.Used)
      or ((LocalISFROPT in [4,5]) and
        UpstreamUnsatSegmentValues.Used and DownstreamUnsatSegmentValues.Used)
      or TableCollection.Used
      or SegmentFlows.Used
      or EquationValues.Used;
  end;
end;

{ TFlowFileItem }

procedure TFlowFileItem.Assign(Source: TPersistent);
var
  SourceItem: TFlowFileItem;
begin
  if Source is TFlowFileItem then
  begin
    SourceItem := TFlowFileItem(Source);
    Time := SourceItem.Time;
    Inflow := SourceItem.Inflow;
  end
  else
  begin
    inherited;
  end;
end;

procedure TFlowFileItem.SetInflow(const Value: double);
begin
  SetRealProperty(FInflow, Value);
end;

procedure TFlowFileItem.SetTime(const Value: double);
begin
  SetRealProperty(FTime, Value);
end;

{ TFlowFileCollection }

function TFlowFileCollection.Add: TFlowFileItem;
begin
  result := inherited Add as TFlowFileItem;
end;

constructor TFlowFileCollection.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TFlowFileItem, InvalidateModelEvent);
end;

function TFlowFileCollection.GetItem(index: Integer): TFlowFileItem;
begin
  result := inherited Items[Index] as TFlowFileItem;
end;

procedure TFlowFileCollection.SetItem(index: Integer;
  const Value: TFlowFileItem);
begin
  inherited Items[Index] := Value;
end;

{ TExternalFlowProperties }

procedure TExternalFlowProperties.Assign(Source: TPersistent);
var
  SourceProp: TExternalFlowProperties;
begin
  if Source is TExternalFlowProperties then
  begin
    SourceProp := TExternalFlowProperties(Source);
    FlowFileChoice := SourceProp.FlowFileChoice;
    ReferenceTimeChoice := SourceProp.ReferenceTimeChoice;
    FlowFileData := SourceProp.FlowFileData;
    FullFlowFileName := SourceProp.FullFlowFileName;
    FlowFileName := SourceProp.FlowFileName;
  end
  else
  begin
    inherited;
  end;
end;

procedure TExternalFlowProperties.Clear;
begin
  FFlowFileData.Clear;
  FFullFlowFileName := '';
  FFlowFileChoice := ffcNone;
  FReferenceTimeChoice := ffrtModelMuseZero;
end;

constructor TExternalFlowProperties.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.DoInvalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FFlowFileData := TFlowFileCollection.Create(Model);
  Clear;
end;

destructor TExternalFlowProperties.Destroy;
begin
  FFlowFileData.Free;
  inherited;
end;

//function TExternalFlowProperties.GetFlowFileChoice: TFlowFileChoice;
//begin
//  result := FFlowFileChoice;
//  case FFlowFileChoice of
//    ffcNone: result := FFlowFileChoice;
//    ffcFileName:
//      begin
//        if FlowFileName = '' then
//        begin
//          result := ffcNone;
//        end
//        else
//        begin
//          result := FFlowFileChoice;
//        end;
//      end;
//    ffcSpecify:
//      begin
//        if FFlowFileData.Count = 0 then
//        begin
//          result := ffcNone;
//        end
//        else
//        begin
//          result := FFlowFileChoice;
//        end;
//      end;
//    else Assert(False);
//  end;
//end;

function TExternalFlowProperties.GetFlowFileName: string;
var
  LocalModel: TPhastModel;
begin
  if Model = nil then
  begin
    result := FullFlowFileName;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    result := ExtractRelativePath(LocalModel.ModelFileName, FullFlowFileName);
  end;
end;

function TExternalFlowProperties.GetReferenceTimeChoice: TFlowFileReferenceTime;
begin
  if FFlowFileChoice = ffcFileName then
  begin
    Result := ffrtStartOfModel;
  end
  else
  begin
    Result := FReferenceTimeChoice;
  end;
end;

function TExternalFlowProperties.SaveFlowFileData: Boolean;
begin
  result := (FlowFileChoice = ffcSpecify) and (FFlowFileData.Count > 0);
end;

procedure TExternalFlowProperties.SetFlowFileChoice(
  const Value: TFlowFileChoice);
begin
  if FFlowFileChoice <> Value then
  begin
    InvalidateModel;
    FFlowFileChoice := Value;
  end;
end;

procedure TExternalFlowProperties.SetFlowFileData(
  const Value: TFlowFileCollection);
begin
  FFlowFileData.Assign(Value);
end;

procedure TExternalFlowProperties.SetFlowFileName(const Value: string);
var
  LocalModel: TPhastModel;
  CurDir: string;
begin
  if Model = nil then
  begin
//    FullFlowFileName := Value;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    CurDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(LocalModel.ModelFileName));
      FullFlowFileName := ExpandFileName(Value);
    finally
      SetCurrentDir(CurDir);
    end;
  end;
end;

procedure TExternalFlowProperties.SetFullFlowFileName(const Value: string);
begin
  if FFullFlowFileName <> Value then
  begin
    InvalidateModel;
    FFullFlowFileName := Value;
  end;
end;

procedure TExternalFlowProperties.SetReferenceTimeChoice(
  const Value: TFlowFileReferenceTime);
begin
  if FReferenceTimeChoice <> Value then
  begin
    InvalidateModel;
    FReferenceTimeChoice := Value;
  end;
end;

procedure InitializeStreamGageOutputTypes;
begin
  StreamGageOutputTypes := TStringList.Create;
  StreamGageOutputTypeUnits := TStringList.Create;
  StreamGageOutputTypes.Add('Stage');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Flow');        StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Depth');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Width');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Midpt-Flow');  StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Precip.');     StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('ET');          StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Runoff');      StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Conductance'); StreamGageOutputTypeUnits.Add('L2/T');
  StreamGageOutputTypes.Add('HeadDiff');    StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Hyd.Grad.');   StreamGageOutputTypeUnits.Add('L/L');
  StreamGageOutputTypes.Add('GW_FLOW');     StreamGageOutputTypeUnits.Add('L3/T');
end;

{ TSfrObs }

procedure TSfrObs.Assign(Source: TPersistent);
begin
  if Source is TSfrObs then
  begin
    ObsType := TSfrObs(Source).ObsType;
  end;
  inherited;
end;

function TSfrObs.GetObsTypeIndex: Integer;
begin
  result := ObsType;
end;

function TSfrObs.GetObsTypeString: string;
begin
  result := ObservationType;
end;

function TSfrObs.ObservationType: string;
begin
  if (FObsType >= 0) and (FObsType < StreamGageOutputTypes.Count) then
  begin
    result := StreamGageOutputTypes[FObsType]
  end
  else
  begin
    result := inherited;
  end;
end;

procedure TSfrObs.SetObsType(const Value: Integer);
begin
  SetIntegerProperty(FObsType, Value);
end;

procedure TSfrObs.SetObsTypeIndex(Value: Integer);
begin
  ObsType := Value;
end;

procedure TSfrObs.SetObsTypeString(const Value: string);
begin
  ObsType := StreamGageOutputTypes.IndexOf(Value);
end;

function TSfrObs.Units: string;
begin
  if (FObsType >= 0) and (FObsType < StreamGageOutputTypeUnits.Count) then
  begin
    result := StreamGageOutputTypeUnits[FObsType]
  end
  else
  begin
    result := inherited;
  end;
end;

{ TSfrObservations }

function TSfrObservations.Add: TSfrObs;
begin
  result := inherited Add as TSfrObs;
end;

constructor TSfrObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TSfrObs, InvalidateModelEvent, ScreenObject);
  FGageOutputNames := TStringList.Create;
end;

destructor TSfrObservations.Destroy;
begin
  FGageOutputNames.Free;
  inherited;
end;

function TSfrObservations.GetSfrItem(Index: Integer): TSfrObs;
begin
  result := inherited Items[Index] as TSfrObs;
end;

procedure TSfrObservations.SetSfrItem(Index: Integer; const Value: TSfrObs);
begin
  inherited Items[Index] := Value;
end;

initialization
  InitializeStreamGageOutputTypes;

finalization
  StreamGageOutputTypes.Free;
  StreamGageOutputTypeUnits.Free;

end.

