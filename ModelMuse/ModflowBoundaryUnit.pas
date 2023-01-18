unit ModflowBoundaryUnit;

interface

uses Winapi.Windows, System.UITypes, SysUtils,
  Classes, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowTransientListParameterUnit, DataSetUnit,
  RealListUnit, TempFiles, SubscriptionUnit, FormulaManagerUnit, SparseDataSets,
  System.Generics.Collections, System.StrUtils;

type
    // @name defines how a formula is interpreted.
    // @unorderedlist(
    //   @item(fiSpecific - Formula / the length or area of
    //     intersection between the @link(TScreenObject) and grid cell.)
    //   @item(fiTotal - Formula.)
    // )
    // When fiSpecific is used, the formula will be multiplied by
    // ObjectIntersectLength or ObjectIntersectArea.
    // fiSpecific has no effect for @link(TScreenObject)s that have
    // only one vertex.
  TFormulaInterpretation = (fiSpecific, fiDirect, fiTotal);

  TCustomLocationObservation = class abstract(TOrderedItem)
  private
    FTime: double;
    FComment: string;
    procedure SetComment(const Value: string);
  protected
    function GetTime: double;
    procedure SetTime(const Value: double);
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    // @name indicates the time of this observation.
    property Time: double read GetTime write SetTime;
    property Comment: string read FComment write SetComment;
  end;


  // @@name defines the starting and ending time for a particular
  // boundary condition.  Descendants add an array of records that
  // defining where and with what values the boundary condition applies.
  TCustomBoundaryStorage = class(TObject)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
  protected
    FCached: boolean;
    FCleared: Boolean;
    FTempFileName : string;
    procedure Clear; virtual; abstract;
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); virtual; abstract;
    procedure Store(Compressor: TCompressionStream); virtual; abstract;
    procedure RestoreData;
  public
    StartingTime: double;
    EndingTime: double;
    destructor Destroy; override;
    procedure CacheData;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(AModel: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  // @name defines a time and a formula used in
  // @link(TModflowTimeList.Initialize).
  TBoundaryValue = record
    Time: double;
    Formula: string;
  end;

  // @name defines an array of @link(TBoundaryValue)s used in
  // @link(TModflowTimeList.Initialize).
  TBoundaryValueArray = array of TBoundaryValue;

  TCustomBoundaryItem = class(TFormulaOrderedItem)
  private
    // See @link(StartTime).
    FStartTime: double;
    // See @link(StartTime).
    procedure SetStartTime(Value: double);
  protected
    FObserverList: TObserverObjectList;
    procedure AssignObserverEvents(Collection: TCollection); virtual; abstract;
    procedure CreateFormulaObjects; virtual; abstract;
    procedure GetPropertyObserver(Sender: TObject; List: TList); virtual; abstract;
    procedure RemoveFormulaObjects; virtual; abstract;
    function GetScreenObject: TObject; override;
    procedure ResetItemObserver(Index: integer);
//    procedure UpdateFormulaDependencies(OldFormula: string; var
//      NewFormula: string; Observer: TObserver; Compiler: TRbwParser); override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; virtual; abstract;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      virtual; abstract;
    // @name returns @true if AnotherItem is a @classname and
    // @link(StartTime) is the same in the current
    // @classname and in AnotherItem.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; virtual; abstract;
    function NonBlankFormulas: boolean;
//    procedure UpdateFormula(Value: string; Position: Integer;
//      var FormulaObject: TFormulaObject);
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject; virtual;
    function GetObserver(Index: Integer): TObserver; override;
  public
//    property ScreenObject: TObject read GetScreenObject;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // Descendants of @classname define string properties that are the
    // formulas for the unique features of each boundary condition.
    // @name provides access to those properties without knowing what
    // they are through the virtual abstract methods @link(GetBoundaryFormula)
    // and @link(SetBoundaryFormula).  Descendants must override those
    // methods.  @name is used in
    // @link(TfrmScreenObjectProperties.StoreModflowBoundary
    // TfrmScreenObjectProperties.StoreModflowBoundary) and
    // @link(TfrmScreenObjectProperties.GetModflowBoundary
    // TfrmScreenObjectProperties.GetModflowBoundary).
    // @seealso(TCustomMF_BoundColl)
    property BoundaryFormula[Index: integer]: string read GetBoundaryFormula
      write SetBoundaryFormula;
  published
    // @name indicates the starting time of this boundary.
    property StartTime: double read FStartTime write SetStartTime;
  end;

  TBoundaryItemClass = class of TCustomBoundaryItem;

  // @name represents a boundary for one time interval.
  // @name is stored by @link(TCustomMF_BoundColl).
  TCustomModflowBoundaryItem = class(TCustomBoundaryItem)
  private
    // See @link(EndTime).
    FEndTime: double;
    // See @link(EndTime).
    procedure SetEndTime(Value: double);
  protected
    // @name returns @true if AnotherItem is a @classname and
    // @link(StartTime) and (EndTime) are the same in the current
    // @classname and in AnotherItem.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    // See @link(ConductanceIndex).
    function GetConductanceIndex: Integer; virtual;
    // @name identifies the BoundaryFormula number that corresponds to the
    // conductance if there is one.
    // @name is used in @link(TCustomListArrayBoundColl.EvaluateListBoundaries).
    property ConductanceIndex: Integer read GetConductanceIndex;
  public
    procedure Assign(Source: TPersistent); override;
  published

    // @name indicates the ending time of this boundary.
    property EndTime: double read FEndTime write SetEndTime;
  end;

  TMF_BoundItemClass = class of TCustomModflowBoundaryItem;

  TNoFormulaItem = class(TCustomModflowBoundaryItem)
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  end;

  TModflowParamBoundary = class;
  TModflowTimeList = class;
  TModflowBoundary = class;
  TModflowScreenObjectProperty = class;

  // @name is an abstract ancestor used with MODFLOW boundary conditions
  // that may or may not be defined at cells.
  // For instance @link(TSfrBoundary.EquationValues EquationValues)
  // of the @link(TSfrBoundary SFR boundary) is defined using a direct
  // descendant of @name.  Boundaries that are defined at cells typically
  // descend from @link(TCustomMF_BoundColl) which descends from @name.
  TCustomNonSpatialBoundColl = class(TCustomObjectOrderedCollection)
  private
    // @name is the @link(TModflowScreenObjectProperty) that owns
    // the current @classname.
    FBoundary: TModflowScreenObjectProperty;
    // See @link(Items).
    procedure SetItem(Index: Integer; const Value: TCustomBoundaryItem);
    // See @link(Items).
    function GetItem(Index: Integer): TCustomBoundaryItem;
  protected
    // @name is the @link(TModflowBoundary) that owns @classname.
    property BoundaryGroup: TModflowScreenObjectProperty read FBoundary;
    // @name is the descendant of @link(TCustomModflowBoundaryItem)
    // stored by classname.
    class function ItemClass: TBoundaryItemClass; virtual; abstract;
    // @name deletes any items if the start and end times are identical
    // or if the end time is before the start time.
    // It is overridden in @link(TFhbHeadCollection) in which items can have the
    // same start and end times.
    function ShouldDeleteItemsWithZeroDuration: Boolean; virtual;
    procedure DeleteItemsWithZeroDuration; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    // @name is the @link(TScreenObject) for this boundary.
    // @name provides access to @link(TCustomModflowBoundaryItem) representing
    // the boundary conditions for different time periods.
    property ScreenObject;

    // @name is the @link(TScreenObject) for this boundary.
    // @name provides access to @link(TCustomModflowBoundaryItem) representing
    // the boundary conditions for different time periods.
    property Items[Index: Integer]: TCustomBoundaryItem read GetItem
      write SetItem; default;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is the @link(TScreenObject) for this boundary.
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); virtual;
    function Used: boolean;
    function UsesATime(ATime: Double): Boolean;
    procedure ReplaceATime(OldTime, NewTime: Double);
  end;

  TCustomMF_BoundColl = class;

  TTimeListsModelLink = class(TObject)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    // @name stores a series of @link(TModflowTimeList)s.  They
    // must be in the same order as the order used to access
    // the corresponding @link(TCustomBoundaryItem.BoundaryFormula
    // TCustomBoundaryItem.BoundaryFormula)s.
    // @seealso(TCustomMF_BoundColl.AddTimeList)
    // @seealso(TCustomMF_BoundColl.GetTimeList)
    // @seealso(TCustomMF_BoundColl.TimeLists)
    FTimeLists: TList;
    FBoundary: TCustomMF_BoundColl;
  protected
    procedure CreateTimeLists; virtual; abstract;
    procedure UpdateTimeLists; virtual;
    procedure UpdateGwtTimeLists; virtual;
  public
    property Boundary: TCustomMF_BoundColl read FBoundary;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); virtual;
    Destructor Destroy; override;
    property TimeLists: TList read FTimeLists;
    procedure AddTimeList(List: TModflowTimeList);
    procedure RemoveTimeList(List: TModflowTimeList);
  end;

  TTimeListsModelLinkClass = class of TTimeListsModelLink;

  // @name manages the link between a @link(TPhastModel) or
  // @link(TChildModel) and a @link(TCustomMF_BoundColl)
  TTimeListModelLinkList = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    FBoundary: TCustomMF_BoundColl;
    FCachedResult: TTimeListsModelLink;
  public
    Constructor Create(Boundary: TCustomMF_BoundColl);
    Destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetLink(AModel: TBaseModel): TTimeListsModelLink;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveLink(AModel: TBaseModel);
  end;

  TBoundaryModelLink = class(TObject)
  strict private
    // @name is actually a TObjectList.
    FBoundaries: TList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(AModel: TBaseModel);
    destructor Destroy; override;
    property Boundaries: TList read FBoundaries;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TBoundaryModelLinkList = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    FCachedResult: TBoundaryModelLink;
  public
    constructor Create;
    destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetLink(AModel: TBaseModel): TBoundaryModelLink;
  end;

  // @name represents MODFLOW boundaries for a series of time intervals.
  // Descendants define one or more @link(TModflowTimeList)s  which must be
  // stored in @link(FTimeListLink) in the same order as the order used to access
  // the corresponding @link(TCustomBoundaryItem.BoundaryFormula
  // TCustomBoundaryItem.BoundaryFormula)s.
  TCustomMF_BoundColl = class(TCustomNonSpatialBoundColl)
  private
    // See @link(ParamName).
    FParamName: string;
    // @name stores instances of @link(TCustomBoundaryStorage).
    // @Seealso(AddBoundary)
    // @Seealso(ClearBoundaries)
    // @Seealso(GetBoundaries)
    // @Seealso(Boundaries)
    // @Seealso(SetBoundaryCapacity)
    FBoundaries: TBoundaryModelLinkList;
    // @name provides access to a series of @link(TTimeListsModelLink)s.
    // Each of them stores a series of @link(TModflowTimeList)s
    // associated with a particular model.  They
    // must be in the same order as the order used to access
    // the corresponding @link(TCustomBoundaryItem.BoundaryFormula
    // TCustomBoundaryItem.BoundaryFormula)s.
    // @seealso(AddTimeList)
    // @seealso(GetTimeList)
    // @seealso(TimeLists)
    FTimeListLink: TTimeListModelLinkList;

    // See @link(Boundaries).
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetBoundaries(const Index: integer;
      AModel: TBaseModel): TCustomBoundaryStorage;
    // See @link(ParamName).
    function GetParamName: string;
    // See @link(ParamName).
    procedure SetParamName(Value: string);
    // See @link(Param).
    function GetParam: TModflowTransientListParameter;
    // See @link(Param).
    procedure SetParam(const Value: TModflowTransientListParameter);
    function GetBoundaryCount(AModel: TBaseModel): integer;
    // @name returns a list of TCustomBoundaryStorage;
    function GetBoundaryList(AModel: TBaseModel): TList;
  protected
    property TimeListLink: TTimeListModelLinkList read FTimeListLink;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; virtual; abstract;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // See @link(TimeLists).
    function GetTimeList(Index: integer; AModel: TBaseModel): TModflowTimeList; virtual;
    // @name adds a @link(TCustomBoundaryStorage) to those owned by
    // @classname
    // @Seealso(SetBoundaryCapacity)
    procedure AddBoundary(Value: TCustomBoundaryStorage);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AddSpecificBoundary(AModel: TBaseModel); virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name adds a @link(TModflowTimeList) to those that
    // can be accessed through @link(TimeLists).  The order in which
    // @link(TModflowTimeList)s are added must correspond to the
    // order in which the corresponding
    // @link(TCustomBoundaryItem.BoundaryFormula
    // TCustomBoundaryItem.BoundaryFormula)s are accessed.
    procedure AddTimeList(List: TModflowTimeList; AModel: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is the @link(TModflowBoundary) that owns @classname.
    // @name is used to set the capacity of @link(FBoundaries)
    // before calling @link(AddBoundary).
    procedure SetBoundaryCapacity(Value: integer; AModel: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name sets the @link(TCustomBoundaryStorage.StartingTime
    // TCustomBoundaryStorage.StartingTime) and
    // @link(TCustomBoundaryStorage.StartingTime
    // TCustomBoundaryStorage.EndingTime) of the
    // @link(TCustomBoundaryStorage) at ItemIndex in @link(Boundaries)
    // to the values of @link(TCustomBoundaryItem.StartTime) and
    // @link(TCustomModflowBoundaryItem.EndTime)
    // Descendants used BoundaryCount to set the length of array of records
    // that define where and with what values the boundary condition apply.
    // for the item in @link(Boundaries) at ItemIndex.
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name clears all the @link(TModflowTimeList)s in @link(TimeLists).
    procedure ClearTimeLists(AModel: TBaseModel);
    procedure AssignBoundaryFormula(AModel: TBaseModel;
      const SeriesName: string; SeriesMethod: TPestParamMethod;
      PestItems, TimeSeriesItems: TStringList; const ItemFormula: string; Writer: TObject;
      var BoundaryValue: TBoundaryValue);
    procedure CreateGwtTimeLists(AModel: TBaseModel); virtual;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveModelLink(AModel: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name frees all the @link(TCustomBoundaryStorage) owned by
    // @classname.
    procedure ClearBoundaries(AModel: TBaseModel);
    // @name copies @link(ParamName) from Source and calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name provides access to @link(TCustomBoundaryStorage) for different
    // time periods.  In descendants, these @link(TCustomBoundaryStorage)
    // define the locations, values, and times for the boundaries.
    property Boundaries[const Index: integer;
      AModel: TBaseModel]: TCustomBoundaryStorage read GetBoundaries;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property BoundaryCount[AModel: TBaseModel]: integer read GetBoundaryCount;
    // @name access a list of TCustomBoundaryStorage;
    property BoundaryList[AModel: TBaseModel]: TList read GetBoundaryList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    // @name returns @true if AnOrderedCollection is a @classname and
    // Param = the Param of AnOrderedCollection and the inherited @name
    // returns @true.
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    // @name is the @link(TModflowTransientListParameter) (if any) of the
    // current @classname.
    property Param: TModflowTransientListParameter read GetParam write SetParam;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is the number of @link(TModflowTimeList)s that can be accessed
    // in @link(TimeLists).
    function TimeListCount(AModel: TBaseModel): integer; virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name provides access to the @link(TModflowTimeList)s defined in
    // descendants. @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property TimeLists[Index: integer; AModel: TBaseModel]: TModflowTimeList
      read GetTimeList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name returns @true if Count > 0.
    function DataSetUsed(DataArray: TDataArray;
      AModel: TBaseModel): boolean; virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetBoundaryByStartTime(StartTime: double;
      AModel: TBaseModel): TCustomBoundaryStorage;
    function GetItemByStartTime(StartTime: Double): TCustomModflowBoundaryItem;
    function GetItemContainingTime(Time: Double): TCustomModflowBoundaryItem;
  published
    // @name is the name of the @link(TModflowTransientListParameter)
    // (if any) associated with this @classname.
    property ParamName: string read GetParamName write SetParamName;
  end;

  TMF_BoundCollClass = class of TCustomMF_BoundColl;

  TCustomListArrayBoundColl = class(TCustomMF_BoundColl)
  private
    FListDuplicatesAllowed: Boolean;
    FSectionDuplicatesAllowed: Boolean;
    FCanUsePestParmeters: Boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignArrayCellsWithItem(Item: TCustomModflowBoundaryItem;
      ItemIndex: Integer; DataSets: TList; ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList);
  strict protected
    procedure AssignDirectlySpecifiedValues(AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is a virtual abstract method used to set the values of the
    // cell locations in @link(Boundaries) for a particular time period.
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure CountArrayBoundaryCells(var BoundaryCount: Integer;
      DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel); virtual;
    // @name is a virtual abstract method that descendants use to
    // call (TModflowTimeList.Initialize TModflowTimeList.Initialize).
    // Writer must be a TCustomFileWriter
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); virtual; abstract;
    // @name determines whether or not a single object may define more than
    // one boundary in the same cell. @name is set to @false in
    // @link(TChdCollection) because multiple CHD boundaries in the same
    // cell are added together.
    property ListDuplicatesAllowed: boolean read FListDuplicatesAllowed
      write FListDuplicatesAllowed;
    function OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes; virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name should be called just before a formula is about
    // to be evaluated to make sure that all the required
    // information is up-to-date.  ACell is a @link(TCellAssignment).
    procedure UpdateRequiredListData(DataSets: TList; Variables: TList;
      ACell: TObject; AModel: TBaseModel);
    // @name is called in @link(EvaluateListBoundaries).
    // @name stores the locations of the @link(TCellAssignment)s in ACellList
    // (which is a @link(TCellAssignmentList)) in BoundaryStorage.
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name assigns values to the contents of
    // @link(TCustomBoundaryStorage BoundaryStorage)
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); virtual; abstract;
    // @name when the formula assigned by the user needs to be
    // expanded by the program @name is used to do that.
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      virtual; abstract;
    function AllowInactiveMf6Cells: boolean; virtual;
  public
    property CanUsePestParmeters: Boolean read FCanUsePestParmeters
      write FCanUsePestParmeters;
    // @name is set to @True in @link(TSwrReachCollection),
    // @link(TStrCollection), @link(TSfrMf6Collection),
    // and in @link(TMvrBoundary) when the source is SFR.
    // When @name is set to @True, separate boundaries can be set by the same
    // section of the same object in the same cell.
    property SectionDuplicatesAllowed: Boolean read FSectionDuplicatesAllowed
      write FSectionDuplicatesAllowed;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name determines the locations, times, and values of
    // the boundary condition associated with @classname.  These boundaries
    // are first evaluated in @link(TModflowTimeList)s defined by
    // descedents and accessed through @link(TimeLists). Those data
    // are then transfered to descendants of @link(TCustomBoundaryStorage)
    // by calls to @link(AssignArrayCellsWithItem).
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name determines the locations, times, and values of
    // the boundary condition associated with @classname.
    procedure EvaluateListBoundaries(AModel: TBaseModel);
  end;

  // @name is used for boundary conditions in which the boundary conditions
  // are either in the form of an array of values or where the individual
  // boundary condition cells are linked into a larger structure.
  // @name is used for the EVT, ETS, RCH, RES, LAK, SFR, and UZF packages.
  TCustomMF_ArrayBoundColl = class(TCustomListArrayBoundColl)
  protected
    // @name is called in @link(EvaluateListBoundaries).
    // @name stores the locations of the @link(TCellAssignment)s in ACellList
    // (which is a @link(TCellAssignmentList)) in BoundaryStorage.
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name assigns values to the contents of
    // @link(TCustomBoundaryStorage BoundaryStorage)
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
    // @name when the formula assigned by the user needs to be
    // expanded by the program @name is used to do that.
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
  end;

  // @name is used for boundary conditions in which each section of an object
  // is used to define a separate set of boundary conditions.
  // It is used for the
  // CHD, DRN, DRT, GHB, RIV, and WEL packages.
  TCustomMF_ListBoundColl = class(TCustomListArrayBoundColl)
  protected
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
  end;

  TCustomStringCollection = class;

  // @name is used to set multiple formulas of a similar
  // type in one boundary. A descendent is used in the ETS package for
  // depth and rate fractions. It is also used with concentrations or rates of
  // different chemical species in GWT.
  TCustomStringValueItem = class abstract(TFormulaOrderedItem)
  private
    FObserver: TObserver;
    procedure SetValue(const Value: string);
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
  protected
    FValue: TFormulaObject;
    function StringCollection: TCustomStringCollection;
    function GetScreenObject: TObject; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
  public
    property Observer: TObserver read FObserver;
    property ValueObject: TFormulaObject read FValue write FValue;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TGwtConcStringValueItem = class(TCustomStringValueItem)
    constructor Create(Collection: TCollection); override;
  end;

  TCustomStringCollection = class abstract(TCustomObjectOrderedCollection)
  private
    FParentCollection: TCustomNonSpatialBoundColl;
  protected
    property ParentCollection: TCustomNonSpatialBoundColl read FParentCollection;
  public
    constructor Create(ItemClass: TCollectionItemClass; Model: TBaseModel;
      AScreenObject: TObject; ParentCollection: TCustomNonSpatialBoundColl);
  end;

  TGwtConcStringCollection = class(TCustomStringCollection)
  private
    FUsedForPestSeries: Boolean;
    function GetItems(Index: Integer): TGwtConcStringValueItem;
    procedure SetItems(Index: Integer; const Value: TGwtConcStringValueItem);
    procedure SetUsedForPestSeries(const Value: Boolean);
  public
    property UsedForPestSeries: Boolean read FUsedForPestSeries write SetUsedForPestSeries;
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TCustomMF_BoundColl);
    property Items[Index: Integer]: TGwtConcStringValueItem read GetItems
      write SetItems; default;
    function Add: TGwtConcStringValueItem;
  end;

  // @name is used to store a series of @link(TDataArray)s for boundary
  // conditions in MODFLOW.
  TModflowTimeList = class(TCustomTimeList)
  private
    // See @link(NonParamDescription).
    FNonParamDescription: string;
    // See @link(ParamDescription).
    FParamDescription: string;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FScreenObject: TObject;
  protected
    // @name calls the inherited @link(TCustomTimeList.SetUpToDate)
    // and then calls @link(OnInvalidate) if @link(OnInvalidate) is assigned.
    procedure SetUpToDate(const Value: boolean); override;
  public
    procedure Invalidate; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name takes the times and formulas in BoundaryValues and uses them
    // to determine the locations and values for those times.  These
    // locations and values are stored in @link(TRealSparseDataSet)s
    // accessed through @link(TCustomTimeList.Items Items).
    procedure Initialize(BoundaryValues: TBoundaryValueArray;
      ScreenObject: TObject; UseLgrEdgeCells: TLgrCellTreatment;
      AssignmentLocation: TAssignmentLocation = alAll);
      reintroduce;
    // @name is a description of what this @classname represents when @name is
    // not associated with a parameter.
    // @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property NonParamDescription: string read FNonParamDescription
      write FNonParamDescription;
    // @name is a description of what this @classname represents when @name is
    // associated with a parameter.
    // @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property ParamDescription: string read FParamDescription
      write FParamDescription;
    // @name is the @link(TRbwDataType) of the @link(TDataArray)s contained
    // by @classname
    property DataType: TRbwDataType read FDataType write FDataType;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
  end;

  TModflowTimeLists = TObjectList<TModflowTimeList>;

  // Each @name stores a @link(TCustomMF_BoundColl).
  // @classname is stored by @link(TModflowParameters).
  TModflowParamItem = class(TOrderedItem)
  private
    // See @link(Param).
    FParam: TCustomMF_BoundColl;
    // See @link(Param).
    procedure SetParam(const Value: TCustomMF_BoundColl);
  protected
    // @name is used in @link(Create) to create @link(FParam).
    class function BoundaryClass: TMF_BoundCollClass; virtual; abstract;
  public
    // @name copies a @classname from Source to this @Classname.
    procedure Assign(Source: TPersistent);override;
    // @name creates an instance of @classname.
    Constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name tests whether AnotherItem is the same as this @Classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    // @name returns @link(TCustomNonSpatialBoundColl.Used Param.Used).
    function Used: boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean; virtual;
  published
    // @name is the @link(TCustomMF_BoundColl) used to store a single
    // parameter and its associated data.
    property Param: TCustomMF_BoundColl read FParam write SetParam;
  end;

  TModflowParamItemClass = class of TModflowParamItem;

  // @name stores a series of MODFLOW boundaries
  // associated with a series of MODFLOW parameters.
  // @seealso(TModflowParamItem)
  TModflowParameters = class(TCustomObjectOrderedCollection)
  private
    // See @link(ScreenObject).
//    FScreenObject: TObject;
    // @name is the @link(TModflowBoundary) that owns @classname.
    FBoundary: TModflowParamBoundary;
    // See @link(Items).
    function GetItem(Index: Integer): TModflowParamItem;
    // See @link(Items).
    procedure SetItem(Index: Integer; const Value: TModflowParamItem);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name calls @link(TCustomListArrayBoundColl.EvaluateArrayBoundaries
    // TCustomListArrayBoundColl.EvaluateArrayBoundaries) for each
    // @link(Items)[Index].@link(TCustomMF_ArrayBoundColl Param).
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure EvaluateListBoundaries(AModel: TBaseModel);
    // @name adds as new descendant of @link(TModflowParamItem);
    function Add: TModflowParamItem;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    constructor Create(Boundary: TModflowParamBoundary;
      ItemClass: TModflowParamItemClass; Model: TBaseModel; ScreenObject: TObject); virtual;
    // @name returns the @link(TModflowParamItem) whose
    // @link(TCustomMF_BoundColl.ParamName TModflowParamItem.Param.ParamName)
    // matches AName.
    function GetParamByName(const AName: string): TModflowParamItem;
    // @name inserts a new @link(TModflowParamItem)
    // at the position specified by
    // Index.
    function Insert(Index: Integer): TModflowParamItem;
    // @name provides access to the @link(TModflowParamItem)s stored by this
    // @classname
    property Items[Index: Integer]: TModflowParamItem read GetItem
      write SetItem; default;
    // @name returns the position of the @link(TModflowParamItem)
    // associated with
    // the @link(TModflowTransientListParameter).  It is used in
    // @link(TModflowParamBoundary.DeleteParam
    // TModflowParamBoundary.DeleteParam).
    function IndexOfParam(AParam: TModflowTransientListParameter): integer;
    // @name is @nil or the @link(TScreenObject) that owns this @classname.
//    property ScreenObject: TObject read FScreenObject;

    // @name returns @true if any @link(TModflowParamItem) in @link(Items)
    // returns @true.
    function Used: boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean; virtual;
    function UsesATime(ATime: Double): Boolean;
    procedure ReplaceATime(OldTime, NewTime: Double);
  end;

  TModflowParametersClass = class of TModflowParameters;

  TFormulaProperty = class(TPersistent)
  private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    // See @link(ParentModel).
    FModel: TBaseModel;
    // See @link(ScreenObject).
    FScreenObject: TObject;
  protected
    // @name is a TObjectList;
    FObserverList: TObserverObjectList;
    procedure InvalidateModel;
    procedure UpdateFormulaDependencies(OldFormula: string;
      var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
    procedure UpdateFormulaBlocks(Value: string; Position: integer;
      var FormulaObject: TFormulaObject);
    procedure UpdateFormulaNodes(Value: string; Position: integer;
      var FormulaObject: TFormulaObject);
    procedure ResetBoundaryObserver(Index: integer);
    function CreateFormulaObjectBlocks(
      Orientation: TDataSetOrientation): TFormulaObject;
    function CreateFormulaObjectNodes(
      Orientation: TDataSetOrientation): TFormulaObject;
    procedure CreateObserver(ObserverNameRoot: string; var Observer: TObserver;
      Displayer: TObserver); virtual;
  public
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    // @name is either nil or the the current @link(TPhastModel).
    property ParentModel: TBaseModel read FModel;
    // @name is either @nil or the @link(TScreenObject) that owns
    // this @classname.
    property ScreenObject: TObject read FScreenObject;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; virtual; abstract;
  end;

  TModflowScreenObjectProperty = class(TFormulaProperty)
  private
    FBoundaryObserver: TObserver;
    FMf6TimeSeriesNames: TStringList;
  protected
    procedure GetPropertyObserver(Sender: TObject; List: TList); virtual;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean); virtual;
//    procedure ResetItemObserver(Index: integer);
//    function CreateFormulaObject(Orientation: TDataSetOrientation)
//      : TFormulaObject;
    procedure CreateObserver(ObserverNameRoot: string; var Observer: TObserver;
      Displayer: TObserver); override;
    function BoundaryObserverPrefix: string; virtual; abstract;
    procedure CreateBoundaryObserver;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; virtual;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); virtual;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; virtual;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); virtual;
    procedure SetPestParamMethod(var Field: TPestParamMethod;
      NewValue: TPestParamMethod);
  public
    // @name is used when @classname contains properties that are not transient.
    // @link(TModflowSteadyBoundary.CreateFormulaObjects), @link(CreateBoundaryObserver)
    // should be called in @link(Create) if @name will be used.
    // @seealso(TModflowSteadyBoundary).
    // @seealso(THfbBoundary).
    // @seealso(TSfrMf6Boundary).
    property BoundaryObserver: TObserver read FBoundaryObserver;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure StopTalkingToAnyone; virtual;
    procedure Invalidate;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; virtual;
    property PestBoundaryFormula[FormulaIndex: integer]: string
      read GetPestBoundaryFormula write SetPestBoundaryFormula;
    property PestBoundaryMethod[FormulaIndex: integer]: TPestParamMethod
      read GetPestBoundaryMethod write SetPestBoundaryMethod;
    property Mf6TimeSeriesNames: TStringList read FMf6TimeSeriesNames;
  end;

  TMultiHeadItem = class(TOrderedItem)
  private
    FLayer: integer;
    FProportion: double;
    FUsed: boolean;
    procedure SetLayer(const Value: integer);
    procedure SetProportion(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    property Used: boolean read FUsed write FUsed;
    procedure Assign(Source: TPersistent); override;
  published
    property Layer: integer read FLayer write SetLayer;
    property Proportion: double read FProportion write SetProportion;
  end;

  TMultiHeadCollection = class(TCustomObjectOrderedCollection)
  private
//    FScreenObject: TObject;
    function GetMultiHeadItem(Index: integer): TMultiHeadItem;
  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject);
    property MultiHeadItems[Index: integer]: TMultiHeadItem
      read GetMultiHeadItem; default;
  end;

  TCustomLocationObsBoundary = class(TModflowScreenObjectProperty)
  private
  protected
    function BoundaryObserverPrefix: string; override;
  private
    FObservationName: string;
    FPurpose: TObservationPurpose;
    procedure SetObservationName(Value: string);
    procedure SetPurpose(const Value: TObservationPurpose);
  public
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    procedure Assign(Source: TPersistent); override;
    function GetItemObsName(Item: TCustomLocationObservation): string;
  published
    property ObservationName: string read FObservationName
      write SetObservationName;
    property Purpose: TObservationPurpose read FPurpose write SetPurpose;
  end;

  TCustomMultilayerLocationObsBoundary = class(TCustomLocationObsBoundary)
  private
    FLayerFractions: TMultiHeadCollection;
    procedure SetLayerFractions(const Value: TMultiHeadCollection);
  public
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property LayerFractions: TMultiHeadCollection read FLayerFractions
      write SetLayerFractions;
  end;

  TModflowSteadyBoundary = class(TModflowScreenObjectProperty)
  private
    FUsed: boolean;
    procedure SetUsed(const Value: boolean);
    function GetUsed: boolean;
  protected
    FUsedObserver: TObserver;
    procedure HandleChangedValue(Observer: TObserver); virtual; abstract;
    function GetUsedObserver: TObserver; virtual; abstract;
    procedure CreateFormulaObjects; virtual; abstract;
    procedure CreateObservers; virtual; abstract;
    property UsedObserver: TObserver read GetUsedObserver;
  public
    Procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; override;
  published
    property IsUsed: boolean read GetUsed write SetUsed;
  end;

  // @name represents the MODFLOW boundaries associated with
  // a single @link(TScreenObject).
  // @seealso(TCustomMF_BoundColl)
  TModflowBoundary = class(TModflowScreenObjectProperty)
  private

    // See @link(Values).
    FValues: TCustomMF_BoundColl;

    // See @link(Values).
    procedure SetValues(const Value: TCustomMF_BoundColl);

  protected
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // In descendants, @name fills ValueTimeList with a series of TObjectLists
    // - one for
    // each stress period.  Each such TObjectList is filled with
    // descendants of
    // @link(TValueCell) representing the boundaray condition locations and values
    // for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); virtual; abstract;
    // @name is used in @link(Create) to create @link(FValues).
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ClearBoundaries(AModel: TBaseModel); virtual;
    function BoundaryObserverPrefix: string; override;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveModelLink(AModel: TBaseModel); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ClearTimeLists(AModel: TBaseModel); virtual;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name calls @link(TCustomListArrayBoundColl.EvaluateArrayBoundaries
    // Values.EvaluateArrayBoundaries)
    // Descendents also call @link(TModflowParameters.EvaluateArrayBoundaries
    // Parameters.EvaluateArrayBoundaries).
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure EvaluateListBoundaries(AModel: TBaseModel); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TCustomBoundaryStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    //
    // @name fills ParamList with the names of the
    // MODFLOW parameters for the current boundary condition that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TCustomBoundaryStorage) in
    // @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); virtual; abstract;

    function NonParameterColumns: integer; virtual;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); virtual;
    // If @link(Clear) is overriden @name will probably have to be overriden too.
    function Used: boolean; override;
    procedure InvalidateDisplay; virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean; virtual;
    // If @link(Used) is overriden @name will probably have to be overriden too.
    procedure Clear; virtual;
  published
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: TCustomMF_BoundColl read FValues write SetValues;
  end;

  TModflowParamBoundary = class(TModflowBoundary)
  private
    // See @link(Parameters).
    FParameters: TModflowParameters;
    // See @link(Parameters).
    procedure SetParameters(const Value: TModflowParameters);

  protected
    FCurrentParameter: TModflowTransientListParameter;

  var
    FInterp: TMf6InterpolationMethods;
    // @name is used in @link(Create) when creating @link(FParameters).
    // @name is passed to the constructor of @link(FParameters).
    class function ModflowParamItemClass: TModflowParamItemClass;
      virtual; abstract;
    function ParameterType: TParameterType; virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ClearBoundaries(AModel: TBaseModel); override;
    procedure SetInterp(const Value: TMf6InterpolationMethods);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveModelLink(AModel: TBaseModel); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ClearTimeLists(AModel: TBaseModel); override;
    // @name copies @link(Values) and @link(Parameters) from the Source
    // @classname to this @classname.
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name deletes the @link(TModflowParameter) associated with Param from
    // @link(Parameters).
    procedure DeleteParam(Param: TModflowParameter);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name calls @link(TModflowBoundary.EvaluateArrayBoundaries) and
    // @link(TModflowParameters.EvaluateArrayBoundaries
    // Parameters.EvaluateArrayBoundaries).
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure EvaluateListBoundaries(AModel: TBaseModel); override;
      // @name returns @true if either Values.Used is @true
      // or Parameters.Used is @true.
    function Used: boolean; override;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean; override;
    procedure Clear; override;
    property Interp: TMf6InterpolationMethods read FInterp write SetInterp
      Stored True;
  published
    // @name stores the MODFLOW boundaries that ARE
    // associated with parameters.
    property Parameters: TModflowParameters read FParameters
      write SetParameters;
  end;

  TSpecificModflowBoundary = class(TModflowParamBoundary)
  private
    FFormulaInterpretation: TFormulaInterpretation;
    procedure SetFormulaInterpretation(const Value: TFormulaInterpretation);

  public
    // @name copies @link(FormulaInterpretation) from the Source
    // @classname to this @classname and then calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
  published
    // @name determines whether the a formula represents
    // @unorderedlist(
    //   @item(fiSpecific - formula / the length or area of
    //     intersection between the @link(TScreenObject) and grid cell.)
    //   @item(fiTotal - formula.)
    // )
    property FormulaInterpretation: TFormulaInterpretation
      read FFormulaInterpretation write SetFormulaInterpretation;
  end;

procedure GlobalRemoveModflowBoundaryItemSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRestoreModflowBoundaryItemSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRemoveMFBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRestoreMFBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure RemoveScreenObjectPropertySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure RestoreScreenObjectPropertySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses Math, Contnrs, ScreenObjectUnit, PhastModelUnit, ModflowGridUnit,
  frmFormulaErrorsUnit, frmGoPhastUnit, SparseArrayUnit, GlobalVariablesUnit,
  GIS_Functions, IntListUnit, ModflowCellUnit, frmProgressUnit, Dialogs,
  EdgeDisplayUnit, SolidGeom, frmErrorsAndWarningsUnit, ModflowParameterUnit,
  CustomModflowWriterUnit, ModelMuseUtilities, Modflow6TimeSeriesUnit;

resourcestring
  StrInvalidResultType = 'Invalid result type';
  StrErrorOccursInS = 'Error occurs in %s';
  StrStartingAndEnding = 'Starting and ending times are the same.';
  StrAnModelFeatureIn = 'A model feature in %0:s has identical starting and' +
  ' ending times of %1:g';
  StrItLooksLikeNoStr = 'It looks like no stress periods have been defined f' +
  'or this model. If this is not true, please contact the ModelMuse develope' +
  'r for assistance.';
  StrInvalidStartingBou = 'Invalid starting boundary time';
  StrInvalidEndingBound = 'Invalid ending boundary time';
  StrTheBoundaryConditiStart = 'The boundary condition defined by %s will no' +
  't be used because the first time for it is equal to or after the end of t' +
  'he last stress period';
  StrTheBoundaryConditiEnd = 'The boundary condition defined by %s will not ' +
  'be used because the last time for it is equal to or before the beginning ' +
  'of the first stress period';
  StrBoundaryStartingAn = 'Boundary starting and ending times are the same';
  StrBecauseTheBoundary = 'Because the boundary starting and ending times de' +
  'fined in %s are the same, they will have no effect.';

function SortBoundaryItems(Item1, Item2: pointer): integer;
var
  B1: TCustomBoundaryItem;
  B2: TCustomBoundaryItem;
  Bound1: TCustomModflowBoundaryItem;
  Bound2: TCustomModflowBoundaryItem;
  Index: Integer;
begin
  B1 := Item1;
  B2 := Item2;
  result := Sign(B1.StartTime - B2.StartTime);
  if (result = 0) and (B1 is TCustomModflowBoundaryItem)
    and (B2 is TCustomModflowBoundaryItem) then
  begin
    Bound1 := Item1;
    Bound2 := Item2;
    result := Sign(Bound1.EndTime - Bound2.EndTime);
  end;
  if result = 0 then
  begin
    for Index := 0 to B1.BoundaryFormulaCount - 1 do
    begin
      if B1.BoundaryFormula[Index] <> B2.BoundaryFormula[Index] then
      begin
        if B1.BoundaryFormula[Index] = '' then
        begin
          result := 1;
          Exit;
        end
        else if B2.BoundaryFormula[Index] = '' then
        begin
          result := -1;
          Exit;
        end;
      end;
    end;
  end;
end;

//procedure TCustomBoundaryItem.UpdateFormulaDependencies(
//  OldFormula: string; var NewFormula: string; Observer: TObserver;
//  Compiler: TRbwParser);
//var
//  OldUses: TStringList;
//  NewUses: TStringList;
//  Position: Integer;
//  DS: TObserver;
//  ParentScreenObject: TScreenObject;
//  Index: integer;
//  procedure CompileFormula(var AFormula: string;
//    UsesList: TStringList);
//  begin
//    if AFormula <> '' then
//    begin
//      try
//        Compiler.Compile(AFormula);
//        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
//      except on E: ERbwParserError do
//        begin
//        end;
//      end;
//    end;
//  end;
//begin
//  OldFormula := Trim(OldFormula);
//  NewFormula := Trim(NewFormula);
//  if OldFormula = NewFormula then
//  begin
//    Exit;
//  end;
//  if (frmGoPhast.PhastModel <> nil)
//    and ((frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> []) then
//  begin
//    Exit;
//  end;
//  ParentScreenObject := ScreenObject as TScreenObject;
//  if (ParentScreenObject = nil)
////    or not ParentScreenObject.CanInvalidateModel then
//    // 3
//        {or not ParentScreenObject.CanInvalidateModel} then
//  begin
//    Exit;
//  end;
//  OldUses := TStringList.Create;
//  NewUses := TStringList.Create;
//  try
//    CompileFormula(OldFormula, OldUses);
//    CompileFormula(NewFormula, NewUses);
//    for Index := OldUses.Count - 1 downto 0 do
//    begin
//      Position := NewUses.IndexOf(OldUses[Index]);
//      if Position >= 0 then
//      begin
//        OldUses.Delete(Index);
//        NewUses.Delete(Position);
//      end;
//    end;
//    for Index := 0 to OldUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
//      Assert(DS <> nil);
//      DS.StopsTalkingTo(Observer);
//    end;
//    for Index := 0 to NewUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
//      Assert(DS <> nil);
//      DS.TalksTo(Observer);
//    end;
//  finally
//    NewUses.Free;
//    OldUses.Free;
//  end;
//end;

procedure TCustomModflowBoundaryItem.Assign(Source: TPersistent);
var
  Item: TCustomModflowBoundaryItem;
  ScreenObject: TScreenObject;
  NonSpatColl: TCustomNonSpatialBoundColl;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomModflowBoundaryItem then
  begin
    Item := TCustomModflowBoundaryItem(Source);
    EndTime := Item.EndTime;
  end;
  inherited;
  if StartTime = EndTime then
  begin
    NonSpatColl := Collection as TCustomNonSpatialBoundColl;
    if NonSpatColl.ShouldDeleteItemsWithZeroDuration then
    begin
      ScreenObject := NonSpatColl.ScreenObject as TScreenObject;
      if (ScreenObject <> nil) and (ScreenObject.Model <> nil) then
      begin
        frmErrorsAndWarnings.AddWarning(ScreenObject.Model, StrStartingAndEnding,
          Format(StrAnModelFeatureIn, [ScreenObject.Name, StartTime]), ScreenObject);
      end;
    end;
  end;
end;

function TCustomModflowBoundaryItem.GetConductanceIndex: Integer;
begin
  result := -1;
end;

function TCustomModflowBoundaryItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomModflowBoundaryItem;
begin
  result := inherited;
  if result then
  begin
    result := (AnotherItem is TCustomModflowBoundaryItem);
    if result then
    begin
      Item := TCustomModflowBoundaryItem(AnotherItem);
      result := (Item.EndTime = EndTime)
    end;
  end;
end;

function TCustomBoundaryItem.NonBlankFormulas: boolean;
var
  Index: integer;
begin
  result := True;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    result := BoundaryFormula[Index] <> '';
    if not result then
    begin
      Exit;
    end;
  end;
end;

function TCustomBoundaryItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomBoundaryItem;
begin
  result := (AnotherItem is TCustomBoundaryItem);
  if result then
  begin
    Item := TCustomBoundaryItem(AnotherItem);
    result := (Item.StartTime = StartTime);
  end;
end;

procedure TCustomBoundaryItem.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  if not Observer.UpToDate then
  begin
    Observer.UpToDate := True;
  end;
end;

procedure TCustomModflowBoundaryItem.SetEndTime(Value: double);
begin
  // prevent rounding errors in saved files from being used.
  Value := FortranStrToFloat(FortranFloatToStr(Value));
  if FEndTime <> Value then
  begin
    FEndTime := Value;
    InvalidateModel;
  end;
end;

{ TCustomModflowBoundaryCollection }

procedure TCustomMF_BoundColl.AddBoundary(
  Value: TCustomBoundaryStorage);
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(Value.Model);
  Link.Boundaries.Add(Value);
end;

procedure TCustomMF_BoundColl.AddTimeList(List: TModflowTimeList;
  AModel: TBaseModel);
var
  ATimeLists: TList;
begin
  ATimeLists := FTimeListLink.GetLink(AModel).TimeLists;
  ATimeLists.Add(List);
end;

procedure TCustomMF_BoundColl.Assign(Source: TPersistent);
var
  Index: Integer;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomMF_BoundColl then
  begin
    ParamName :=
      TCustomMF_BoundColl(Source).ParamName;
  end;
  inherited;
  for Index := Count - 1 downto 0 do
  begin
    if not Items[Index].NonBlankFormulas then
    begin
      Delete(Index);
    end;
  end;
end;

procedure TCustomMF_BoundColl.ClearBoundaries(AModel: TBaseModel);
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  Link.Boundaries.Clear;
end;

procedure TCustomMF_BoundColl.ClearTimeLists(AModel: TBaseModel);
var
  Index: Integer;
begin
  for Index := 0 to TimeListCount(AModel) - 1 do
  begin
    TimeLists[Index, AModel].Clear;
  end;
end;

constructor TCustomMF_BoundColl.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited ;
  FTimeListLink:= TTimeListModelLinkList.Create(self);
  FBoundaries:= TBoundaryModelLinkList.Create;
end;

procedure TCustomMF_BoundColl.CreateGwtTimeLists(AModel: TBaseModel);
begin

end;

function TCustomMF_BoundColl.DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean;
var
  TimeListIndex: Integer;
  TimeList: TModflowTimeList;
begin
  result := False;
  for TimeListIndex := 0 to TimeListCount(AModel) - 1 do
  begin
    TimeList := TimeLists[TimeListIndex, AModel];
    result := TimeList.IndexOfDataSet(DataArray) >= 0;
    if result then
    begin
      Exit;
    end;
  end;
end;

destructor TCustomMF_BoundColl.Destroy;
begin
  inherited;
  FBoundaries.Free;
  FTimeListLink.Free;
end;

function TCustomMF_ArrayBoundColl.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
begin
  // this is only used with cell lists.
  Assert(False);
end;

procedure TCustomMF_ArrayBoundColl.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
begin
  // this is only used with cell lists.
  Assert(False);
end;

procedure TCustomMF_ArrayBoundColl.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
begin
  // this is only used with cell lists.
  Assert(False);
end;

//procedure TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries(AModel: TBaseModel);
//begin
//  inherited;
//end;

constructor TCustomListArrayBoundColl.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FListDuplicatesAllowed := True;
  FSectionDuplicatesAllowed := False;
  FCanUsePestParmeters := True;
end;

function TCustomListArrayBoundColl.OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes;
begin
  result := [rdtDouble, rdtInteger];
end;

//procedure TCustomMF_ListBoundColl.EvaluateListBoundaries(AModel: TBaseModel);
//begin
//  inherited;
//end;

procedure TCustomMF_ListBoundColl.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
begin
  // this procedure is only used with arrays.
  Assert(false);
end;

function TCustomMF_BoundColl.GetBoundaries(
  const Index: integer; AModel: TBaseModel): TCustomBoundaryStorage;
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  result := Link.Boundaries[Index];
end;

function TCustomMF_BoundColl.GetBoundaryByStartTime(
  StartTime: double; AModel: TBaseModel): TCustomBoundaryStorage;
var
  Index: Integer;
  Item: TCustomBoundaryStorage;
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  result := nil;
  for Index := 0 to Link.Boundaries.Count - 1 do
  begin
    Item := Link.Boundaries[Index];
    if Item.StartingTime = StartTime then
    begin
      result := Item;
      Exit;
    end;
    if Item.StartingTime < StartTime then
    begin
      result := Item;
    end;
    if Item.EndingTime > StartTime then
    begin
      Exit;
    end;
  end;
  // If the result is nil, either there is a bug or the user has made an error
  // and an appropriate error message should be displayed.
end;

function TCustomMF_BoundColl.GetBoundaryCount(AModel: TBaseModel): integer;
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  result := Link.Boundaries.Count;
end;

function TCustomMF_BoundColl.GetBoundaryList(AModel: TBaseModel): TList;
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  result := Link.Boundaries;
end;

function TCustomMF_BoundColl.GetItemByStartTime(
  StartTime: Double): TCustomModflowBoundaryItem;
var
  ItemIndex: Integer;
  AnItem: TCustomModflowBoundaryItem;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomModflowBoundaryItem;
    if AnItem.StartTime >= StartTime then
    begin
      result := AnItem;
      if AnItem.EndTime > StartTime then
      begin
        Break;
      end;
    end;
  end;
end;

function TCustomMF_BoundColl.GetItemContainingTime(
  Time: Double): TCustomModflowBoundaryItem;
const
  Epsilon = 1e-10;
var
  ItemIndex: Integer;
  AnItem: TCustomModflowBoundaryItem;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomModflowBoundaryItem;
    if ((AnItem.StartTime <= Time) or NearlyTheSame(AnItem.StartTime, Time, Epsilon))
    and (AnItem.EndTime >= Time) then
    begin
      result := AnItem;
    end;
    if AnItem.StartTime > Time then
    begin
      Break;
    end;
  end;
end;

function TCustomMF_BoundColl.GetParam: TModflowTransientListParameter;
var
  Model: TPhastModel;
begin
  if (FParamName <> '') and (self.Model <> nil) then
  begin
    Model := self.Model as TPhastModel;
    result := Model.ModflowTransientParameters.GetParamByName(FParamName);
  end
  else
  begin
    result := nil;
  end;
end;

function TCustomMF_BoundColl.GetParamName: string;
begin
  result := FParamName;
end;

function TCustomMF_BoundColl.GetTimeList(Index: integer; AModel: TBaseModel): TModflowTimeList;
var
  ATimeLists: TList;
begin
  ATimeLists := FTimeListLink.GetLink(AModel).TimeLists;
  result := ATimeLists[Index];
end;

function TCustomMF_BoundColl.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  Collection: TCustomMF_BoundColl;
begin
  result := (AnOrderedCollection is TCustomMF_BoundColl);
  if result then
  begin
    Collection := TCustomMF_BoundColl(AnOrderedCollection);
    result := (Param = Collection.Param)
      and inherited IsSame(AnOrderedCollection);
  end;
end;

procedure TCustomMF_BoundColl.RemoveModelLink(AModel: TBaseModel);
begin
  TimeListLink.RemoveLink(AModel);
end;

procedure TCustomMF_BoundColl.SetBoundaryCapacity(Value: integer; AModel: TBaseModel);
var
  Link: TBoundaryModelLink;
begin
  Link := FBoundaries.GetLink(AModel);
  Link.Boundaries.Capacity := Value;
end;

procedure TCustomMF_BoundColl.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  Boundaries[ItemIndex, AModel].StartingTime := Item.StartTime;
  Boundaries[ItemIndex, AModel].EndingTime := Item.EndTime;
end;

{ TModflowTimeList }
constructor TModflowTimeList.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model);
  FScreenObject := ScreenObject;
end;

procedure TModflowTimeList.Initialize(BoundaryValues: TBoundaryValueArray;
  ScreenObject: TObject; UseLgrEdgeCells: TLgrCellTreatment;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  LocalModel: TCustomModel;
//  Grid: TModflowGrid;
  Formula: string;
  StoredUpToDate: boolean;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  Time1: Double;
  Time2: Double;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if UpToDate then
    Exit;

  LocalScreenObject := ScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  LocalModel := Model as TCustomModel;
  Assert(LocalModel <> nil);

  FirstUsedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
  LastUsedTime := LocalModel.ModflowFullStressPeriods[
    LocalModel.ModflowFullStressPeriods.Count - 1].EndTime;

  FirstUsedTime := Math.Max(FirstUsedTime,
    LocalModel.ModflowStressPeriods[0].StartTime);
  LastUsedTime := Math.Min(LastUsedTime, LocalModel.ModflowStressPeriods[
    LocalModel.ModflowStressPeriods.Count - 1].EndTime);

  StoredUpToDate := LocalModel.UpToDate;
  try

    Clear;
//    Grid := LocalModel.ModflowGrid;
//    Assert(Grid <> nil);

    for Index := 0 to Length(BoundaryValues) - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Time := BoundaryValues[Index].Time;
      Formula := BoundaryValues[Index].Formula;
      DataArray := nil;
      case DataType of
        rdtDouble:
          begin
            DataArray := TTransientRealSparseDataSet.Create(LocalModel);
            DataArray.DataType := rdtDouble;
          end;
        rdtInteger:
          begin
            DataArray := TTransientIntegerSparseDataSet.Create(LocalModel);
            DataArray.DataType := rdtInteger;
          end;
        else Assert(False);
      end;
      DataArray.Name := ValidName(NonParamDescription) + '_' + IntToStr(Index+1);
      DataArray.DisplayName := DataArray.Name;
      try
        Add(Time, DataArray);
      except on E: EInvalidTime do
        begin
          E.Message := E.Message + sLineBreak + Format(StrErrorOccursInS, [LocalScreenObject.Name]);
          raise;
        end;
      end;
      DataArray.UseLgrEdgeCells := UseLgrEdgeCells;
      DataArray.EvaluatedAt := eaBlocks;
      DataArray.Orientation := Orientation;
      DataArray.UpdateDimensions(LocalModel.LayerCount, LocalModel.RowCount,
        LocalModel.ColumnCount);

//      Sections := T3DSparseIntegerArray.Create(SPASmall);
//      FSectionArrays.Add(Sections);

      Time1 := Time;
      Time2 := Time;
      if Index < Length(BoundaryValues) - 1 then
      begin
        Time2 := BoundaryValues[Index+1].Time;
      end;
      if ((Time2 >= FirstUsedTime) or (Index >= Length(BoundaryValues)-1))
        and (Time1 <= LastUsedTime) then
      begin
        try
          LocalScreenObject.AssignValuesToModflowDataSet({Grid,} DataArray,
            Formula, LocalModel, UseLgrEdgeCells, AssignmentLocation);
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(LocalScreenObject.Name, Name,
              Formula, E.Message);
            Formula := '0';
            BoundaryValues[Index].Formula := Formula;
            LocalScreenObject.AssignValuesToModflowDataSet({Grid,} DataArray,
              Formula, LocalModel, UseLgrEdgeCells, AssignmentLocation);
          end;
        end;
      end;
      LocalModel.DataArrayManager.CacheDataArrays;
      DataArray.UpToDate := True;
      DataArray.CacheData;
    end;

  finally
    LocalModel.UpToDate := StoredUpToDate;
  end;
end;

procedure TModflowTimeList.Invalidate;
begin
  if (FScreenObject <> nil)
    and (FScreenObject as TScreenObject).CanInvalidateModel then
  begin
    inherited;
  end;
end;

procedure TCustomMF_BoundColl.SetParam(
  const Value: TModflowTransientListParameter);
begin
  if Value = nil then
  begin
    ParamName := '';
  end
  else
  begin
    ParamName := Value.ParameterName;
  end;
//  FParamNameStorage.Param := Value;
end;

procedure TCustomMF_BoundColl.SetParamName(
  Value: string);
begin
  Value := StringReplace(Value, '"', '', [rfReplaceAll, rfIgnoreCase]);
  if FParamName <> Value then
  begin
    FParamName := Value;
    InvalidateModel;
  end;
end;

procedure TCustomMF_BoundColl.TestIfObservationsPresent(
  var EndOfLastStressPeriod, StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
begin
  ObservationsPresent := False;
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
end;

function TCustomMF_BoundColl.TimeListCount(AModel: TBaseModel): integer;
var
  ATimeLists: TList;
begin
  ATimeLists := FTimeListLink.GetLink(AModel).TimeLists;
  result := ATimeLists.Count;
end;

procedure TCustomListArrayBoundColl.UpdateRequiredListData(DataSets, Variables: TList;
  ACell: TObject; AModel: TBaseModel);
var
  ADataSet: TDataArray;
  Variable: TCustomValue;
  VarIndex: Integer;
  Layer: Integer;
  Cell: TCellAssignment;
begin
  Cell := ACell as TCellAssignment;
  UpdateGlobalLocations(Cell.Column, Cell.Row, Cell.Layer, eaBlocks,
    AModel);
  UpdateCurrentSegment(Cell.Segment);
  UpdateCurrentSection(Cell.Section);
  for VarIndex := 0 to Variables.Count - 1 do
  begin
    Variable := Variables[VarIndex];
    ADataSet := DataSets[VarIndex];
    Layer := -1;
    case ADataSet.Orientation of
      dsoTop:
        begin
          Layer := 0;
        end;
      dso3D:
        begin
          Layer := Cell.Layer;
        end;
    else
      begin
        Assert(False);
      end;
    end;
    case ADataSet.DataType of
      rdtDouble:
        begin
          (Variable as TRealVariable).Value := ADataSet.RealData[Layer, Cell.Row, Cell.Column];
        end;
      rdtInteger:
        begin
          (Variable as TIntegerVariable).Value := ADataSet.IntegerData[Layer, Cell.Row, Cell.Column];
        end;
      rdtBoolean:
        begin
          (Variable as TBooleanVariable).Value := ADataSet.BooleanData[Layer, Cell.Row, Cell.Column];
        end;
      rdtString:
        begin
          (Variable as TStringVariable).Value := ADataSet.StringData[Layer, Cell.Row, Cell.Column];
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TModflowTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value then
  begin
    if Assigned(OnInvalidate) then
    begin
      OnInvalidate(Self);
    end;
  end;

end;

{ TModflowParamItem }

procedure TModflowParamItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TModflowParamItem then
  begin
    Param := TModflowParamItem(Source).Param;
  end;
  inherited;
end;

constructor TModflowParamItem.Create(Collection: TCollection);
var
  BC: TMF_BoundCollClass;
  ParameterCollection: TModflowParameters;
begin
  inherited;
  ParameterCollection := Collection as TModflowParameters;
  AlwaysAssignForeignId := True;
  BC := BoundaryClass;
  FParam := BC.Create(ParameterCollection.FBoundary,
    ParameterCollection.Model, ParameterCollection.ScreenObject);
end;

function TModflowParamItem.DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean;
begin
  result := Param.DataSetUsed(DataArray, AModel);
end;

destructor TModflowParamItem.Destroy;
begin
  FParam.Free;
  inherited;
end;

function TModflowParamItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TModflowParamItem);
  if result then
  begin
    result := FParam.IsSame(TModflowParamItem(AnotherItem).FParam);
  end;
end;

procedure TModflowParamItem.SetParam(
  const Value: TCustomMF_BoundColl);
begin
  FParam.Assign(Value);
end;

function TModflowParamItem.Used: boolean;
begin
  result := Param.Used;
end;

{ TModflowParameters }

function TModflowParameters.Add: TModflowParamItem;
begin
  result := inherited Add as TModflowParamItem;
end;

constructor TModflowParameters.Create(Boundary: TModflowParamBoundary;
  ItemClass: TModflowParamItemClass; Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(ItemClass, Model, ScreenObject);
  FBoundary := Boundary;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
end;

function TModflowParameters.DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean;
var
  Index: Integer;
begin
  result := false;
  for Index := 0 to Count - 1 do
  begin
    result := Items[Index].DataSetUsed(DataArray, AModel);
    if result then
    begin
      Exit;
    end;
  end;
end;

procedure TModflowParameters.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index].Param as TCustomMF_ArrayBoundColl).
      EvaluateArrayBoundaries(AModel, Writer);
  end;
end;

procedure TModflowParameters.EvaluateListBoundaries(AModel: TBaseModel);
var
  Index: Integer;
  ListCollection: TCustomMF_ListBoundColl;
begin
  for Index := 0 to Count - 1 do
  begin
    ListCollection := Items[Index].Param as TCustomMF_ListBoundColl;
    ListCollection.CanUsePestParmeters := False;
    ListCollection.EvaluateListBoundaries(AModel);
  end;
end;

function TModflowParameters.GetItem(Index: Integer): TModflowParamItem;
begin
  result := inherited Items[Index] as TModflowParamItem;
end;

function TModflowParameters.GetParamByName(
  const AName: string): TModflowParamItem;
var
  ParamIndex: Integer;
  Item: TModflowParamItem;
begin
  result := nil;
  for ParamIndex := 0 to Count - 1 do
  begin
    Item := Items[ParamIndex];
    if Item.Param.ParamName = AName then
    begin
      result := Item;
      Exit;
    end;
  end;
end;

function TModflowParameters.IndexOfParam(
  AParam: TModflowTransientListParameter): integer;
var
  ParamIndex: Integer;
  Item: TModflowParamItem;
begin
  result := -1;
  for ParamIndex := 0 to Count - 1 do
  begin
    Item := Items[ParamIndex];
    if Item.Param.ParamName = AParam.ParameterName then
    begin
      result := ParamIndex;
      Exit;
    end;
  end;
end;

function TModflowParameters.Insert(Index: Integer): TModflowParamItem;
begin
  result := inherited Insert(Index) as TModflowParamItem;
end;

procedure TModflowParameters.ReplaceATime(OldTime, NewTime: Double);
var
  Pindex: Integer;
  PItem: TModflowParamItem;
begin
  for Pindex := 0 to Count - 1 do
  begin
    PItem := Items[Pindex];
    PItem.FParam.ReplaceATime(OldTime, NewTime);
  end;
end;

procedure TModflowParameters.SetItem(Index: Integer;
  const Value: TModflowParamItem);
begin
  inherited Items[Index] := Value
end;

function TModflowParameters.Used: boolean;
var
  Index: Integer;
  Item: TModflowParamItem;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    result := Item.Used;
    if result then
    begin
      Exit;
    end;
  end;
end;

function TModflowParameters.UsesATime(ATime: Double): Boolean;
var
  Pindex: Integer;
  PItem: TModflowParamItem;
begin
  result := False;
  for Pindex := 0 to Count - 1 do
  begin
    PItem := Items[Pindex];
    result := PItem.FParam.UsesATime(ATime);
    if result then
    begin
      Exit;
    end;
  end;
end;

{ TModflowBoundary }

procedure TModflowParamBoundary.Assign(Source: TPersistent);
var
  Boundary: TModflowParamBoundary;
begin
  if Source is TModflowParamBoundary then
  begin
    Boundary := TModflowParamBoundary(Source);
//    Values := Boundary.Values;
    Parameters := Boundary.Parameters;
    Interp := Boundary.Interp;
  end;
  inherited;
end;

procedure TModflowParamBoundary.Clear;
begin
  inherited;
  Parameters.Clear;
end;

procedure TModflowParamBoundary.ClearBoundaries(AModel: TBaseModel);
var
  Index: Integer;
  ParamItem: TModflowParamItem;
begin
  inherited;
  for Index := 0 to Parameters.Count - 1 do
  begin
    ParamItem := Parameters[Index];
    ParamItem.Param.ClearBoundaries(AModel);
  end;
end;

procedure TModflowParamBoundary.ClearTimeLists(AModel: TBaseModel);
var
  Index: Integer;
  ParamItem: TModflowParamItem;
begin
  inherited;
  for Index := 0 to Parameters.Count - 1 do
  begin
    ParamItem := Parameters[Index];
    ParamItem.Param.ClearTimeLists(AModel);
  end;
end;

constructor TModflowParamBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//  Assert((Model = nil) or (Model is TPhastModel));
//  FPhastModel := Model;
//  FValues:= BoundaryCollectionClass.Create(self, Model,
//    ScreenObject);
  FParameters := TModflowParameters.Create(self, ModflowParamItemClass, Model,
    ScreenObject);
end;

function TModflowParamBoundary.DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean;
begin
  result := inherited DataSetUsed(DataArray, AModel)
    or FParameters.DataSetUsed(DataArray, AModel);
end;

procedure TModflowParamBoundary.DeleteParam(Param: TModflowParameter);
var
  Index: integer;
begin
  Index := FParameters.IndexOfParam(Param as TModflowTransientListParameter);
  if Index >= 0 then
  begin
    FParameters.Delete(Index);
  end;
end;

destructor TModflowParamBoundary.Destroy;
begin
  FParameters.Free;
//  FValues.Free;
  inherited;
end;

procedure TModflowParamBoundary.EvaluateArrayBoundaries(AModel: TBaseModel;
  Writer: TObject);
var
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  if Model.ModflowTransientParameters.CountParam(ParameterType) > 0 then
  begin
    Parameters.EvaluateArrayBoundaries(AModel, Writer);
  end
  else
  begin
    inherited EvaluateArrayBoundaries(AModel, Writer);
  end;
end;

procedure TModflowParamBoundary.EvaluateListBoundaries(AModel: TBaseModel);
begin
  inherited;
  Parameters.EvaluateListBoundaries(AModel);
end;

procedure TModflowParamBoundary.RemoveModelLink(AModel: TBaseModel);
var
  Index: Integer;
  ParamItem: TModflowParamItem;
begin
  inherited;
  for Index := 0 to Parameters.Count - 1 do
  begin
    ParamItem := Parameters[Index];
    ParamItem.Param.RemoveModelLink(AModel);
  end;
end;

procedure TModflowParamBoundary.SetParameters(const Value: TModflowParameters);
begin
  FParameters.Assign(Value);
end;

procedure TModflowParamBoundary.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean; AModel: TBaseModel);
var
  ParamIndex: Integer;
  Param: TModflowParamItem;
begin
  inherited;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    AddBoundaryTimes(Param.Param, Times, StartTestTime, EndTestTime,
      StartRangeExtended, EndRangeExtended);
  end;
end;

function TModflowParamBoundary.Used: boolean;
begin
  result := inherited Used or Parameters.Used
end;

procedure TSpecificModflowBoundary.Assign(Source: TPersistent);
begin
  if Source is TSpecificModflowBoundary then
  begin
    FormulaInterpretation :=
      TSpecificModflowBoundary(Source).FormulaInterpretation;
  end;
  inherited;
end;

procedure TModflowBoundary.Assign(Source: TPersistent);
var
  Boundary:  TModflowBoundary;
begin
  if Source is TModflowBoundary then
  begin
    Boundary := TModflowBoundary(Source);
    Values := Boundary.Values;
  end
  else
  begin
    inherited;
  end;
end;

function TModflowBoundary.BoundaryObserverPrefix: string;
begin
  result := '';
  Assert(False);
end;

procedure TModflowBoundary.Clear;
begin
  Values.Clear;
end;

procedure TModflowBoundary.ClearBoundaries(AModel: TBaseModel);
begin
  FValues.ClearBoundaries(AModel);
end;

procedure TModflowBoundary.ClearTimeLists(AModel: TBaseModel);
begin
  Values.ClearTimeLists(AModel);
end;

constructor TModflowBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//  Assert((Model = nil) or (Model is TPhastModel));
//  FPhastModel := Model;
  FValues:= BoundaryCollectionClass.Create(self, Model,
    ScreenObject);
end;

function TModflowBoundary.DataSetUsed(DataArray: TDataArray; AModel: TBaseModel): boolean;
begin
  result := Values.DataSetUsed(DataArray, AModel);
end;

destructor TModflowBoundary.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TModflowBoundary.EvaluateArrayBoundaries(AModel: TBaseModel;
  Writer: TObject);
begin
  (Values as TCustomListArrayBoundColl).
    EvaluateArrayBoundaries(AModel, Writer);
end;

procedure TModflowBoundary.EvaluateListBoundaries(AModel: TBaseModel);
begin
  Mf6TimeSeriesNames.Clear;
  (Values as TCustomListArrayBoundColl).
    EvaluateListBoundaries(AModel);
end;

procedure TModflowBoundary.InvalidateDisplay;
begin
  // do nothing
end;

procedure TModflowBoundary.SetValues(const Value: TCustomMF_BoundColl);
begin
  FValues.Assign(Value);
end;

procedure TModflowBoundary.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean; AModel: TBaseModel);
begin
  AddBoundaryTimes(Values, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
end;

function TModflowBoundary.Used: boolean;
begin
  result := (Values <> nil) and Values.Used;
end;

procedure TModflowScreenObjectProperty.AddBoundaryTimes(
  BoundCol: TCustomNonSpatialBoundColl; Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean);
var
  BoundaryIndex: Integer;
  Boundary: TCustomModflowBoundaryItem;
  SP_Epsilon: Double;
  ClosestIndex: Integer;
  ExistingTime: Double;
begin
  SP_Epsilon := (FModel as TCustomModel).SP_Epsilon;
  for BoundaryIndex := 0 to BoundCol.Count - 1 do
  begin
    Boundary := BoundCol[BoundaryIndex] as TCustomModflowBoundaryItem;
    ClosestIndex := Times.IndexOfClosest(Boundary.StartTime);
    if ClosestIndex >= 0 then
    begin
      ExistingTime := Times[ClosestIndex];
      if Abs(ExistingTime-Boundary.StartTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.StartTime);
      end;
    end;
    ClosestIndex := Times.IndexOfClosest(Boundary.EndTime);
    if ClosestIndex >= 0 then
    begin
      ExistingTime := Times[ClosestIndex];
      if Abs(ExistingTime-Boundary.EndTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.EndTime);
      end;
    end;
//    Times.AddUnique(Boundary.StartTime);
//    Times.AddUnique(Boundary.EndTime);
    if (Boundary.StartTime < StartTestTime-SP_Epsilon) then
    begin
      StartRangeExtended := True;
    end;
    if (Boundary.EndTime > EndTestTime+SP_Epsilon) then
    begin
      EndRangeExtended := True;
    end;
//    if (Boundary.StartTime < StartTestTime) then
//    begin
//      StartRangeExtended := True;
//    end;
//    if (Boundary.EndTime > EndTestTime) then
//    begin
//      EndRangeExtended := True;
//    end;
  end;
end;

function TModflowBoundary.NonParameterColumns: integer;
begin
  result := 2 + Values.TimeListCount(frmGoPhast.PhastModel);
end;

procedure TModflowBoundary.RemoveModelLink(AModel: TBaseModel);
begin
  FValues.RemoveModelLink(AModel);
end;

procedure TSpecificModflowBoundary.SetFormulaInterpretation(
  const Value: TFormulaInterpretation);
begin
  if FFormulaInterpretation <> Value then
  begin
    FFormulaInterpretation := Value;
    InvalidateModel;
    InvalidateDisplay;
  end;
end;

{ TCustomNonSpatialBoundColl }

procedure TCustomNonSpatialBoundColl.Assign(Source: TPersistent);
var
  List: TList;
  Index: Integer;
  Item1: TCustomBoundaryItem;
begin
  inherited;
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(SortBoundaryItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item1 := List[Index];
      Item1.Index := Index;
    end;
    DeleteItemsWithZeroDuration;
  finally
    List.Free;
  end;
end;

constructor TCustomNonSpatialBoundColl.Create(Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(ItemClass, Model, ScreenObject);
  FBoundary := Boundary;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
end;

function TCustomNonSpatialBoundColl.Used: boolean;
begin
  result := Count > 0;
end;

function TCustomNonSpatialBoundColl.UsesATime(ATime: Double): Boolean;
var
  TimeIndex: Integer;
  AnItem: TCustomBoundaryItem;
  MFItem: TCustomModflowBoundaryItem;
begin
  result := False;
  for TimeIndex := 0 to Count - 1 do
  begin
    AnItem := Items[TimeIndex];
    if AnItem.FStartTime = ATime then
    begin
      result := True;
      Exit;
    end;
    if AnItem is TCustomModflowBoundaryItem then
    begin
      MFItem := TCustomModflowBoundaryItem(AnItem);
      if MFItem.FEndTime = ATime then
      begin
        result := True;
        Exit;
      end;
    end;
  end;
end;

function TCustomNonSpatialBoundColl._AddRef: Integer;
begin
  Result := 1;
end;

function TCustomNonSpatialBoundColl._Release: Integer;
begin
  Result := 1;
end;

procedure TCustomNonSpatialBoundColl.DeleteItemsWithZeroDuration;
var
  Item1: TCustomBoundaryItem;
  Item2: TCustomBoundaryItem;
  Index: Integer;
begin
  if not ShouldDeleteItemsWithZeroDuration then
  begin
    Exit;
  end;
  for Index := Count - 1 downto 1 do
  begin
    Item1 := Items[Index - 1] as TCustomBoundaryItem;
    if not (Item1 is TCustomModflowBoundaryItem) then
    begin
      break;
    end;
    Item2 := Items[Index] as TCustomModflowBoundaryItem;
    if (TCustomModflowBoundaryItem(Item2).StartTime
      < TCustomModflowBoundaryItem(Item1).EndTime) then
    begin
      TCustomModflowBoundaryItem(Item2).StartTime :=
        TCustomModflowBoundaryItem(Item1).EndTime;
    end;
    if (TCustomModflowBoundaryItem(Item2).StartTime
      >= TCustomModflowBoundaryItem(Item2).EndTime) then
    begin
      Delete(Index);
    end;
  end;
end;

procedure TCustomNonSpatialBoundColl.SetItem(Index: Integer;
  const Value: TCustomBoundaryItem);
begin
  inherited Items[Index] := Value;
end;

function TCustomNonSpatialBoundColl.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := true;
end;

function TCustomNonSpatialBoundColl.GetItem(
  Index: Integer): TCustomBoundaryItem;
begin
  result := inherited Items[Index] as TCustomBoundaryItem
end;

//function TCustomNonSpatialBoundColl.GetScreenObject: TObject;
//begin
//  result := ScreenObject;
//end;

function TCustomNonSpatialBoundColl.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TCustomNonSpatialBoundColl.ReplaceATime(OldTime, NewTime: Double);
const
  Epsilon = 1e-12;
var
  TimeIndex: Integer;
  AnItem: TCustomBoundaryItem;
  MFItem: TCustomModflowBoundaryItem;
begin
  for TimeIndex := 0 to Count - 1 do
  begin
    AnItem := Items[TimeIndex];
    if NearlyTheSame(AnItem.FStartTime, OldTime, Epsilon) then
    begin
      AnItem.FStartTime := NewTime
    end;
    if AnItem is TCustomModflowBoundaryItem then
    begin
      MFItem := TCustomModflowBoundaryItem(AnItem);
      if NearlyTheSame(MFItem.FEndTime, OldTime, Epsilon) then
      begin
        MFItem.FEndTime := NewTime
      end;
    end;
  end;
end;

constructor TCustomBoundaryItem.Create(Collection: TCollection);
var
  Index: integer;
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  inherited;
  OnRemoveSubscription := GlobalRemoveModflowBoundaryItemSubscription;
  OnRestoreSubscription := GlobalRestoreModflowBoundaryItemSubscription;

  CreateFormulaObjects;
  LocalScreenObject := ScreenObject as TScreenObject;
  FObserverList := TObserverObjectList.Create;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    Observer := TObserver.Create(nil);
    FObserverList.Add(Observer);
    if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
    begin
      LocalScreenObject.TalksTo(Observer);
    end;
    BoundaryFormula[Index] := '0';
  end;
  AssignObserverEvents(Collection);
end;

destructor TCustomBoundaryItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  Observer: TObserver;
  Index: integer;
  PhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) then
  begin
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil) and not PhastModel.Clearing
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      for Index := 0 to FObserverList.Count - 1 do
      begin
        Observer := FObserverList[Index];
        LocalScreenObject.StopsTalkingTo(Observer);
      end;
    end;
  end;
  RemoveFormulaObjects;
  FObserverList.Free;
  inherited;
end;

function TCustomBoundaryItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserverList[Index];
end;

function TCustomBoundaryItem.GetScreenObject: TObject;
begin
  result := nil;
  if Collection = nil then
  begin
    Exit;
  end;
  result := (Collection as TCustomNonSpatialBoundColl).ScreenObject;
end;

{ TCustomBoundaryStorage }

constructor TCustomBoundaryStorage.Create(AModel: TBaseModel);
begin
  FModel := AModel;
end;

destructor TCustomBoundaryStorage.Destroy;
begin
  if FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
  end;
  inherited;
end;

procedure TCustomBoundaryStorage.RestoreData;
var
  DecompressionStream: TDecompressionStream;
  Annotations: TStringList;
  MemStream: TMemoryStream;
  Count: Integer;
  Index: Integer;
begin
  Assert(FCached);
  Assert(FCleared);
  Annotations := TStringList.Create;
  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    DecompressionStream := TDecompressionStream.Create(MemStream);
    try
      Annotations.Sorted := True;
      Annotations.Duplicates := dupIgnore;
      DecompressionStream.Read(Count, SizeOf(Count));
      Annotations.Capacity := Count;
      for Index := 0 to Count - 1 do
      begin
        Annotations.Add(ReadCompStringSimple(DecompressionStream));
      end;

      Restore(DecompressionStream, Annotations);
    finally
      DecompressionStream.Free;
    end;
  finally
    MemStream.Free;
    Annotations.Free;
    FCleared := False;
  end;
end;

procedure TCustomBoundaryStorage.CacheData;
var
  MemStream: TMemoryStream;
  Compressor: TCompressionStream;
begin
  if not FCached then
  begin
    if FTempFileName = '' then
    begin
      FTempFileName := TempFileName;
    end;
    MemStream:= TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(clDefault, MemStream);
      try
        MemStream.Position := 0;
        Store(Compressor);
      finally
        Compressor.Free;
      end;
      MemStream.Position := 0;
      ZipAFile(FTempFileName, MemStream);
    finally
      MemStream.Free;
    end;
    FCached := True;
  end;
  Clear;
end;

{ TNoFormulaItem }

procedure TNoFormulaItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing.
end;

function TNoFormulaItem.BoundaryFormulaCount: integer;
begin
  result := 0;
end;

procedure TNoFormulaItem.CreateFormulaObjects;
begin
  // do nothing.
end;

function TNoFormulaItem.GetBoundaryFormula(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

procedure TNoFormulaItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  // do nothing
end;

procedure TNoFormulaItem.RemoveFormulaObjects;
begin
  // do nothing.
end;

procedure TNoFormulaItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  Assert(False);
end;

//procedure TCustomBoundaryItem.UpdateFormula(Value: string;
//  Position: Integer; var FormulaObject: TFormulaObject);
//var
//  ParentModel: TPhastModel;
//  Compiler: TRbwParser;
//  Observer: TObserver;
//begin
//  if FormulaObject.Formula <> Value then
//  begin
//    ParentModel := Model as TPhastModel;
//    if ParentModel <> nil then
//    begin
//      Compiler := ParentModel.rpThreeDFormulaCompiler;
//      Observer := FObserverList[Position];
//      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer, Compiler);
//    end;
//    InvalidateModel;
//
//    if not (csDestroying in frmGoPhast.PhastModel.ComponentState)
//      and not frmGoPhast.PhastModel.Clearing then
//    begin
//      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
//        FormulaObject, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
//        GlobalRemoveModflowBoundaryItemSubscription,
//        GlobalRestoreModflowBoundaryItemSubscription, self);
//    end;
//  end;
//end;

procedure RemoveScreenObjectPropertySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  if Subject is TModflowScreenObjectProperty then
  begin
    TModflowScreenObjectProperty(Subject).RemoveSubscription(Sender, AName);
  end;
end;

procedure RestoreScreenObjectPropertySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  if Subject is TModflowScreenObjectProperty then
  begin
    TModflowScreenObjectProperty(Subject).RestoreSubscription(Sender, AName);
  end;
end;

procedure GlobalRemoveModflowBoundaryItemSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomBoundaryItem).RemoveSubscription(Sender, AName);
end;

procedure GlobalRestoreModflowBoundaryItemSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomBoundaryItem).RestoreSubscription(Sender, AName);
end;

procedure GlobalRemoveMFBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TModflowScreenObjectProperty).RemoveSubscription(Sender, AName);
end;

procedure GlobalRestoreMFBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TModflowScreenObjectProperty).RestoreSubscription(Sender, AName);
end;

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomStringValueItem).RemoveSubscription(Sender, AName);
end;

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomStringValueItem).RestoreSubscription(Sender, AName);
end;

procedure TCustomBoundaryItem.RemoveSubscription(Sender: TObject; const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.StopsTalkingTo(Observer);
    end;
  finally
    Observers.Free;
  end;
end;

procedure TCustomBoundaryItem.RestoreSubscription(Sender: TObject; const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.TalksTo(Observer);
      Observer.UpToDate := False;
    end;
  finally
    Observers.Free;
  end;
end;

procedure TCustomBoundaryItem.Assign(Source: TPersistent);
var
  Item: TCustomBoundaryItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomBoundaryItem then
  begin
    Item := TCustomBoundaryItem(Source);
    StartTime := Item.StartTime;
  end;
  inherited;
end;

function TCustomBoundaryItem.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
      end;
    else Assert(False);
  end;
  result.AddSubscriptionEvents(
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

{ TModflowScreenObjectProperty }

class function TModflowScreenObjectProperty.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

destructor TModflowScreenObjectProperty.Destroy;
var
  LocalScreenObject: TScreenObject;
  PhastModel: TPhastModel;
begin
  FMf6TimeSeriesNames.Free;
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) then
  begin
    PhastModel := FModel as TPhastModel;
    if (PhastModel <> nil) and not PhastModel.Clearing
      and not (csDestroying in PhastModel.ComponentState)
      and not frmGoPhast.PhastModel.Clearing then
    begin
//      for Index := 0 to FObserverList.Count - 1 do
//      begin
//        Observer := FObserverList[Index];
//        LocalScreenObject.StopsTalkingTo(Observer);
//      end;
      if FBoundaryObserver <> nil then
      begin
        LocalScreenObject.StopsTalkingTo(FBoundaryObserver);
      end;
    end;
  end;


  FBoundaryObserver.Free;
//  FObserverList.Free;
  inherited;
end;

function TModflowScreenObjectProperty.GetPestBoundaryFormula(
  FormulaIndex: integer): string;
begin
  result := '';
end;

function TModflowScreenObjectProperty.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

procedure TModflowScreenObjectProperty.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  // do nothing
end;

procedure TModflowScreenObjectProperty.Invalidate;
var
  index: Integer;
  AnObserver: TObserver;
begin
  if FObserverList <> nil then
  begin
    for index := 0 to FObserverList.Count - 1 do
    begin
      AnObserver := FObserverList[index];
      if AnObserver <> nil then
      begin
        AnObserver.UpToDate := True;
        AnObserver.UpToDate := False;
      end;
    end;
  end;
end;

procedure TModflowScreenObjectProperty.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.StopsTalkingTo(Observer);
    end;
  finally
    Observers.Free;
  end;
end;

procedure TModflowScreenObjectProperty.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.TalksTo(Observer);
      Observer.UpToDate := False;
    end;
  finally
    Observers.Free;
  end;
end;

procedure TModflowScreenObjectProperty.SetPestBoundaryFormula(
  FormulaIndex: integer; const Value: string);
begin
  // do nothing, at least for now.
end;

procedure TModflowScreenObjectProperty.SetPestBoundaryMethod(
  FormulaIndex: integer; const Value: TPestParamMethod);
begin
// do nothing, for now at least.
end;

procedure TModflowScreenObjectProperty.SetPestParamMethod(
  var Field: TPestParamMethod; NewValue: TPestParamMethod);
begin
  if Field <> NewValue then
  begin
    Field := NewValue;
    InvalidateModel;
  end;
end;

procedure TModflowScreenObjectProperty.StopTalkingToAnyone;
begin
  if FBoundaryObserver <> nil then
  begin
    FBoundaryObserver.StopTalkingToAnyone;
  end;
end;

{ TTimeListsModelLink }

procedure TTimeListsModelLink.AddTimeList(List: TModflowTimeList);
begin
  TimeLists.Add(List);
end;

constructor TTimeListsModelLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FModel := AModel;
  FBoundary := ABoundary;
  FTimeLists := TList.Create;
  CreateTimeLists;
end;

destructor TTimeListsModelLink.Destroy;
begin
  FTimeLists.Free;
  inherited;
end;


procedure TTimeListsModelLink.RemoveTimeList(List: TModflowTimeList);
begin
  TimeLists.Remove(List);
end;

procedure TTimeListsModelLink.UpdateGwtTimeLists;
begin
// do nothing;
end;

procedure TTimeListsModelLink.UpdateTimeLists;
begin
  // do nothing
end;

{ TTimeListModelLinkList }

constructor TTimeListModelLinkList.Create(Boundary: TCustomMF_BoundColl);
begin
  FCachedResult := nil;
  FList := TObjectList.Create;
  FBoundary := Boundary;
end;

destructor TTimeListModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTimeListModelLinkList.GetLink(
  AModel: TBaseModel): TTimeListsModelLink;
var
  Index: Integer;
  ALink: TTimeListsModelLink;
begin
  if (FCachedResult <> nil) and (FCachedResult.Model = AModel) then
  begin
    result := FCachedResult;
    result.UpdateGwtTimeLists;
    Exit;
  end;
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.Model = AModel then
    begin
      result := ALink;
      result.UpdateGwtTimeLists;
      FCachedResult := result;
      Exit;
    end;
  end;
  result := FBoundary.GetTimeListLinkClass.Create(AModel, FBoundary);
  FList.Add(result);
  FCachedResult := result;
end;

procedure TTimeListModelLinkList.RemoveLink(AModel: TBaseModel);
var
  Index: Integer;
  ALink: TTimeListsModelLink;
  LinkToRemove: TTimeListsModelLink;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.Model = AModel then
    begin
      LinkToRemove := ALink;
      if FCachedResult = LinkToRemove then
      begin
        FCachedResult := nil
      end;
      FList.Delete(Index);
      Break;
    end;
  end;
end;

{ TBoundaryModelLink }

constructor TBoundaryModelLink.Create(AModel: TBaseModel);
begin
  FModel := AModel;
  FBoundaries := TObjectList.Create;
end;

destructor TBoundaryModelLink.Destroy;
begin
  FBoundaries.Free;
  inherited;
end;

{ TBoundaryModelLinkList }

constructor TBoundaryModelLinkList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TBoundaryModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TBoundaryModelLinkList.GetLink(AModel: TBaseModel): TBoundaryModelLink;
var
  Index: Integer;
  ALink: TBoundaryModelLink;
begin
  if (FCachedResult <> nil) and (FCachedResult.Model = AModel) then
  begin
    result := FCachedResult;
    Exit;
  end;
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.Model = AModel then
    begin
      result := ALink;
      FCachedResult := result;
      Exit;
    end;
  end;
  result := TBoundaryModelLink.Create(AModel);
  FList.Add(result);
  FCachedResult := result;
end;

procedure TCustomBoundaryItem.SetStartTime(Value: double);
begin
  // prevent rounding errors in saved files from being used.
  Value := FortranStrToFloat(FortranFloatToStr(Value));
  if FStartTime <> Value then
  begin
    FStartTime := Value;
    InvalidateModel;
  end;
end;

{ TModflowSteadyBoundary }

procedure TModflowSteadyBoundary.Assign(Source: TPersistent);
var
  SourceSteady: TModflowSteadyBoundary;
begin
  if Source is TModflowSteadyBoundary then
  begin
    SourceSteady := TModflowSteadyBoundary(Source);
    IsUsed := SourceSteady.IsUsed;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowSteadyBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;

  CreateObservers;
end;

destructor TModflowSteadyBoundary.Destroy;
begin
  FUsedObserver.Free;
  inherited;
end;

function TModflowSteadyBoundary.GetUsed: boolean;
begin
  result := FUsed;
end;

procedure TModflowSteadyBoundary.SetUsed(const Value: boolean);
var
  ScreenObject: TScreenObject;
begin
  if FUsed <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if FScreenObject <> nil then
    begin
      if ScreenObject.CanInvalidateModel then
      begin
        HandleChangedValue(UsedObserver);
      end;
    end;
    FUsed := Value;
    InvalidateModel;
  end;
end;

function TModflowSteadyBoundary.Used: boolean;
begin
  result := IsUsed;
end;
{ TCustomListArrayBoundColl }

function TCustomListArrayBoundColl.AllowInactiveMf6Cells: boolean;
begin
  result := False;
end;

procedure TCustomListArrayBoundColl.AssignArrayCellsWithItem(
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; DataSets,
  ListOfTimeLists: TList; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  BoundaryCount: Integer;
  DataArray2: TDataArray;
  TimeList2: TCustomTimeList;
  TimeIndex: Integer;
  DataArray1: TDataArray;
  TimeList1: TModflowTimeList;
begin
  Assert(ListOfTimeLists.Count >= 1);
  TimeList1 := ListOfTimeLists[0];
  DataArray1 := TimeList1[ItemIndex];
  DataSets.Add(DataArray1);
  for TimeIndex := 1 to ListOfTimeLists.Count - 1 do
  begin
    TimeList2 := ListOfTimeLists[TimeIndex];
    DataArray2 := TimeList2[ItemIndex];
    DataSets.Add(DataArray2);
    Assert(DataArray1.LayerCount = DataArray2.LayerCount);
    Assert(DataArray1.RowCount = DataArray2.RowCount);
    Assert(DataArray1.ColumnCount = DataArray2.ColumnCount);
  end;
  CountArrayBoundaryCells(BoundaryCount, DataArray1, DataSets, AModel);
  SetBoundaryStartAndEndTime(BoundaryCount, Item, ItemIndex, AModel);
  AssignArrayCellValues(DataSets, ItemIndex, AModel, PestSeries, PestMethods,
    PestItemNames, TimeSeriesNames);
  for TimeIndex := 0 to ListOfTimeLists.Count - 1 do
  begin
    TimeList1 := ListOfTimeLists[TimeIndex];
    TimeList1.FreeItem(ItemIndex);
  end;
end;

procedure TCustomListArrayBoundColl.CountArrayBoundaryCells(
  var BoundaryCount: Integer; DataArray1: TDataArray; DataSets: TList;
  AModel: TBaseModel);
var
  DSIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DataArray2: TDataArray;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryCount := 0;
  DataArray1.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := RowMin to RowMax do
        begin
          for ColIndex := ColMin to ColMax do
          begin
            if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Inc(BoundaryCount);
            end;
          end;
        end;
      end;
    end;
  end;
  for DSIndex := 1 to DataSets.Count - 1 do
  begin
    DataArray2 := DataSets[DSIndex];
    if LayerMin >= 0 then
    begin
      for LayerIndex := LayerMin to LayerMax do
      begin
        if LocalModel.IsLayerSimulated(LayerIndex) then
        begin
          for RowIndex := RowMin to RowMax do
          begin
            for ColIndex := ColMin to ColMax do
            begin
              if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                Assert(DataArray2.IsValue[LayerIndex, RowIndex, ColIndex]);
              end;
            end;
          end;
        end;
      end;
    end;
//    DataArray2.CacheData;
  end;
//  DataArray1.CacheData;
end;

procedure TCustomListArrayBoundColl.EvaluateArrayBoundaries(AModel: TBaseModel;
  Writer: TObject);
var
  ItemIndex: integer;
  Item: TCustomModflowBoundaryItem;
  ListOfTimeLists: TList;
  DataSets: TList;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ItemCount: Integer;
  ExtraItem: TNoFormulaItem;
  LocalModel: TCustomModel;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  TimeIndex: Integer;
  TimeList1: TModflowTimeList;
  PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames: TStringListObjectList;
  TimeSeriesNames: TStringListObjectList;
begin
  if Count = 0 then
  begin
    Exit;
  end;
  Assert(ScreenObject <> nil);
  Assert(AModel <> nil);
  if not (ScreenObject as TScreenObject).UsedModels.UsesModel(AModel) then
  begin
    Exit;
  end;

  LocalModel := AModel as TCustomModel;
  FirstUsedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
  LastUsedTime := LocalModel.ModflowFullStressPeriods[
    LocalModel.ModflowFullStressPeriods.Count - 1].EndTime;

  Item := Items[0] as TCustomModflowBoundaryItem;
  if Item.StartTime >= LastUsedTime  then
  begin
    frmErrorsAndWarnings.AddWarning(LocalModel, StrInvalidStartingBou,
      Format(StrTheBoundaryConditiStart,
      [(ScreenObject as TScreenObject).Name]), ScreenObject);
    Exit;
  end;

  Item := Items[Count-1] as TCustomModflowBoundaryItem;
  if Item.EndTime <= FirstUsedTime  then
  begin
    frmErrorsAndWarnings.AddWarning(LocalModel, StrInvalidEndingBound,
      Format(StrTheBoundaryConditiEnd,
      [(ScreenObject as TScreenObject).Name]), ScreenObject);
    Exit;
  end;

  FirstUsedTime := Max(FirstUsedTime,
    LocalModel.ModflowStressPeriods[0].StartTime);
  LastUsedTime := Min(LastUsedTime, LocalModel.ModflowStressPeriods[
    LocalModel.ModflowStressPeriods.Count - 1].EndTime);

  PestSeries := TStringList.Create;
  PestMethods := TPestMethodList.Create;
  PestItemNames := TStringListObjectList.Create;
  TimeSeriesNames := TStringListObjectList.Create;
  ListOfTimeLists := TList.Create;
  DataSets := TList.Create;
  try
    InitializeTimeLists(ListOfTimeLists, LocalModel, PestSeries, PestMethods,
      PestItemNames, TimeSeriesNames, Writer);
    TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
      ObservationsPresent);
    PriorTime := StartOfFirstStressPeriod;
    ItemCount := 0;
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := Items[ItemIndex] as TCustomModflowBoundaryItem;
      if Item.StartTime = Item.EndTime then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrBoundaryStartingAn,
          Format(StrBecauseTheBoundary,
          [(ScreenObject as TScreenObject).Name]), ScreenObject);
      end;
      if (Item.StartTime > LastUsedTime)
        or (Item.EndTime < FirstUsedTime) then
      begin
        for TimeIndex := 0 to ListOfTimeLists.Count - 1 do
        begin
          TimeList1 := ListOfTimeLists[TimeIndex];
          TimeList1.FreeItem(ItemCount);
        end;
        Inc(ItemCount);
        Continue;
      end;

      if ObservationsPresent then
      begin
        if PriorTime < Item.StartTime then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := PriorTime;
            ExtraItem.FEndTime := Item.StartTime;
            DataSets.Clear;
            AssignArrayCellsWithItem(ExtraItem, ItemCount, DataSets,
              ListOfTimeLists, LocalModel, PestSeries, PestMethods,
              PestItemNames, TimeSeriesNames);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      DataSets.Clear;
      AssignArrayCellsWithItem(Item, ItemCount, DataSets, ListOfTimeLists,
        LocalModel, PestSeries, PestMethods, PestItemNames, TimeSeriesNames);
      Inc(ItemCount);
      if (ItemIndex = Count - 1) and ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := Item.EndTime;
            ExtraItem.FEndTime := EndOfLastStressPeriod;
            DataSets.Clear;
            AssignArrayCellsWithItem(ExtraItem, ItemCount, DataSets,
              ListOfTimeLists, LocalModel, PestSeries, PestMethods,
              PestItemNames, TimeSeriesNames);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
      end;
    end;

  finally
    DataSets.Free;
    ListOfTimeLists.Free;
    TimeSeriesNames.Free;
    PestItemNames.Free;
    PestMethods.Free;
    PestSeries.Free;
  end;
end;

procedure TCustomListArrayBoundColl.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem;
  BoundaryStorage: TCustomBoundaryStorage);
begin

end;


procedure TCustomListArrayBoundColl.EvaluateListBoundaries(AModel: TBaseModel);
var
  ItemIndex: integer;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ItemCount: Integer;
  ExtraItem: TNoFormulaItem;
  AScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  CellList: TCellAssignmentList;
  BoundaryFunctionIndex: Integer;
  Formula: string;
  AnItem: TCustomModflowBoundaryItem;
  NextItem: TCustomModflowBoundaryItem;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  VarIndex: Integer;
  VarName: string;
  VarPosition: Integer;
  Variable: TCustomValue;
  AnotherDataSet: TDataArray;
  GlobalVariable: TGlobalVariable;
  CellIndex: Integer;
  ACell: TCellAssignment;
  SparseArrays: TList;
  EliminateIndicies: TIntegerList;
  SectionIndex: Integer;
  SparseArray: T3DSparseBooleanArray;
  Index: Integer;
  Layer: Integer;
  Variables: TList;
  DataSets: TList;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  MaxArrays: Integer;
  ErrorFormula: string;
  StoredCount: Integer;
  OKTypes: TRbwDataTypes;
  NumberOfLayers: Integer;
  NumberOfRows: Integer;
  NumberOfColumns: Integer;
  IDomainArray: TDataArray;
  UnmodifiedFormula: string;
  PestParam: TModflowSteadyParameter;
  PestParamName: string;
  ADataSet: TDataArray;
  Method: TPestParamMethod;
  PestSeriesName: string;
  TimeSeries: TMf6TimeSeries;
  TimeSeriesName: string;
begin
  if Count = 0 then
  begin
    Exit;
  end;
  AScreenObject := ScreenObject as TScreenObject;
  if not AScreenObject.UsedModels.UsesModel(AModel) then
  begin
    Exit;
  end;

  LocalModel := AModel as TCustomModel;

  if LocalModel.ModflowFullStressPeriods.Count = 0 then
  begin
    Beep;
    MessageDlg(StrItLooksLikeNoStr, mtError, [mbOK], 0);
    Exit;
  end;

  FirstUsedTime := LocalModel.ModflowFullStressPeriods.First.StartTime;
  LastUsedTime := LocalModel.ModflowFullStressPeriods.Last.EndTime;

  AnItem := Items[0] as TCustomModflowBoundaryItem;
  if AnItem.StartTime >= LastUsedTime  then
  begin
    Exit;
  end;

  AnItem := Items[Count-1] as TCustomModflowBoundaryItem;
  if AnItem.EndTime <= FirstUsedTime  then
  begin
    Exit;
  end;

  if (LocalModel.ModelSelection = msModflow2015) then
  begin
    IDomainArray := LocalModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  end
  else
  begin
    IDomainArray := nil;
  end;
  CellList:= TCellAssignmentList.Create;
  UsedVariables:= TStringList.Create;
  EliminateIndicies := TIntegerList.Create;
  try
    Compiler := LocalModel.rpThreeDFormulaCompiler;

    AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, LocalModel);

    if CellList.Count = 0 then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingObjectNoCells, AScreenObject.Name, AScreenObject)
    end;
    // FSectionDuplicatesAllowed is set to True in TSwrReachCollection
    // and TStrCollection.
    // SFR in MODFLOW 6 requires duplicate cells to be used.
    if not FSectionDuplicatesAllowed then
    begin
      // eliminate cells that are at the same location and are part of the
      // same section;
      SparseArrays := TObjectList.Create;
      try
        if FListDuplicatesAllowed then
        begin
          MaxArrays := AScreenObject.SectionCount;
        end
        else
        begin
          MaxArrays := 1;
        end;
        for SectionIndex := 0 to MaxArrays - 1 do
        begin
          AScreenObject.GetModelDimensions(LocalModel, NumberOfLayers,
            NumberOfRows, NumberOfColumns);
          SparseArray := T3DSparseBooleanArray.Create(GetQuantum(NumberOfLayers),
            GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
          SparseArrays.Add(SparseArray)
        end;
        SparseArray := SparseArrays[0];
        for CellIndex := CellList.Count - 1 downto 0 do
        begin
          ACell := CellList[CellIndex];
          if ACell.LgrEdge then
          begin
            EliminateIndicies.Add(CellIndex);
          end
          else if LocalModel.IsLayerSimulated(ACell.Layer) then
          begin
            if FListDuplicatesAllowed then
            begin
              SparseArray := SparseArrays[ACell.Section];
            end;
            Layer := LocalModel.
              DataSetLayerToModflowLayer(ACell.Layer);
            if SparseArray.IsValue[Layer, ACell.Row, ACell.Column] then
            begin
              EliminateIndicies.Add(CellIndex);
            end
            else if (LocalModel.ModelSelection = msModflow2015)
              and not AllowInactiveMf6Cells
              and (IDomainArray.IntegerData[
              ACell.Layer, ACell.Row, ACell.Column] <= 0) then
            begin
              EliminateIndicies.Add(CellIndex);
            end
            else
            begin
              SparseArray.Items[Layer, ACell.Row, ACell.Column] := True;
            end;
          end
          else
          begin
            EliminateIndicies.Add(CellIndex);
          end;
        end;
      finally
        SparseArrays.Free;
      end;
    end;

    for Index := 0 to EliminateIndicies.Count - 1  do
    begin
      CellList.Delete(EliminateIndicies[Index]);
    end;

    ClearBoundaries(AModel);

    TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
      ObservationsPresent);
    PriorTime := StartOfFirstStressPeriod;
    ItemCount := 0;

    for ItemIndex := 0 to Count - 1 do
    begin
      AnItem := Items[ItemIndex] as TCustomModflowBoundaryItem;

      // Skip times earlier than the first time or after
      // the last time.
      if (AnItem.StartTime > LastUsedTime)
        or (AnItem.EndTime <= FirstUsedTime) then
      begin
        if ObservationsPresent then
        begin
          if PriorTime < AnItem.StartTime then
          begin
            AddSpecificBoundary(AModel);
            Inc(ItemCount);
          end;
          Inc(ItemCount);
          AddSpecificBoundary(AModel);
          if AnItem.EndTime < EndOfLastStressPeriod then
          begin
            Inc(ItemCount);
            AddSpecificBoundary(AModel);
          end;
        end
        else
        begin
          Inc(ItemCount);
          AddSpecificBoundary(AModel);
        end;
        Continue;
      end;

      //  Add extra items if this boundary skips a stress period.
      if ObservationsPresent then
      begin
        if PriorTime < AnItem.StartTime then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := PriorTime;
            ExtraItem.FEndTime := AnItem.StartTime;

            Variables := TList.Create;
            DataSets := TList.Create;
            try
              AddSpecificBoundary(AModel);
              SetBoundaryStartAndEndTime(CellList.Count, ExtraItem, 0, AModel);
              AssignListCellLocation(Boundaries[0, AModel],  CellList);
              for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
              begin
                if (AnItem.ConductanceIndex < 0)
                  or (AnItem.ConductanceIndex = BoundaryFunctionIndex) then
                begin
                  // MODFLOW-NWT requires that boundary elevations be higher
                  // than the cell bottom elevation. Therefore, setting
                  // all the formulas to zero can cause problems.
                  Formula := '0';
                end
                else
                begin
                  Formula := AnItem.BoundaryFormula[BoundaryFunctionIndex];
                end;
                Compiler.Compile(Formula);
                Expression := Compiler.CurrentExpression;

                CellList.Clear;
                AScreenObject.GetCellsToAssign(Formula, nil, nil, CellList,
                  alAll, LocalModel);
                for Index := 0 to EliminateIndicies.Count - 1  do
                begin
                  CellList.Delete(EliminateIndicies[Index]);
                end;
                UpdateCurrentScreenObject(AScreenObject);

                // This should be an inactive boundary so no parameter
                // should be applied.
                PestParamName := '';
                PestSeriesName := '';
                Method := ppmMultiply;
                TimeSeriesName := '';

                AssignCellList(Expression, CellList, Boundaries[0, AModel],
                  BoundaryFunctionIndex, Variables, DataSets, LocalModel,
                  AScreenObject, PestParamName, PestSeriesName, Method,
                  TimeSeriesName);

                LocalModel.DataArrayManager.CacheDataArrays;
              end;

            finally
              Variables.Free;
              DataSets.Free;
            end;
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
        PriorTime := AnItem.EndTime;
      end;

      AddSpecificBoundary(AModel);
      SetBoundaryStartAndEndTime(CellList.Count, AnItem, ItemCount, AModel);
      StoredCount := ItemCount;
      AssignListCellLocation(Boundaries[ItemCount, AModel],  CellList);
      for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
      begin
        Formula := AdjustedFormula(BoundaryFunctionIndex, ItemIndex);
        // The UnmodifiedFormula might by a PEST parameter or a TDataArray
        // that is modified by PEST or the name of a time series for MODFLOW 6.
        UnmodifiedFormula := AnItem.BoundaryFormula[BoundaryFunctionIndex];

        TimeSeries := LocalModel.Mf6TimesSeries.GetTimeSeriesByName(UnmodifiedFormula);
        if TimeSeries = nil then
        begin
          if CanUsePestParmeters then
          begin
            PestSeriesName := BoundaryGroup.
              PestBoundaryFormula[BoundaryFunctionIndex];
          end
          else
          begin
            PestSeriesName := '';
          end;
        end
        else
        begin
          PestSeriesName := '';
        end;
        Method := BoundaryGroup.
          PestBoundaryMethod[BoundaryFunctionIndex];
        if (PestSeriesName <> '') and (PestSeriesName <> '0') then
        begin
          ADataSet := LocalModel.DataArrayManager.GetDataSetByName(PestSeriesName);
          if (ADataSet <> nil) and ADataSet.PestParametersUsed then
          begin
            PestSeriesName := ADataSet.Name;
            case Method of
              ppmMultiply:
                begin
                  Formula := '(' + Formula + ') * ' + PestSeriesName;
                end;
              ppmAdd:
                begin
                  Formula := '(' + Formula + ') + ' + PestSeriesName;
                end;
            end;
          end
          else
          begin
            PestParam := LocalModel.GetPestParameterByName(PestSeriesName);
            if PestParam = nil then
            begin
              PestSeriesName := '';
            end
            else
            begin
              PestParam.IsUsedInTemplate := True;
              Case Method of
                ppmMultiply:
                  begin
                    Formula := Format('(%0:s) * %1:g', [Formula, PestParam.Value]);
                  end;
                ppmAdd:
                  begin
                    Formula := Format('(%0:s) + %1:g', [Formula, PestParam.Value]);
                  end;
              End;
              PestSeriesName := PestParam.ParameterName;
            end;
          end;
        end;

        ADataSet := LocalModel.DataArrayManager.GetDataSetByName(UnmodifiedFormula);
        if (ADataSet <> nil) and ADataSet.PestParametersUsed {and CanUsePestParmeters} then
        begin
          PestParamName := ADataSet.Name;
        end
        else if TimeSeries <> nil then
        begin
          PestParamName := '';
        end
        else
        begin
//          if CanUsePestParmeters then
//          begin
            PestParam := LocalModel.GetPestParameterByName(UnmodifiedFormula);
//          end
//          else
//          begin
//            PestParam := nil;
//          end;
          // handle the situation if it is a PEST parameter
          if PestParam = nil then
          begin
            PestParamName := '';
          end
          else
          begin
            PestParam.IsUsedInTemplate := True;
            Formula := ReplaceText(Formula, UnmodifiedFormula,
              FortranFloatToStr(PestParam.Value));
            PestParamName := PestParam.ParameterName;
          end;
        end;
        if TimeSeries <> nil then
        begin
          Formula := '1.';
        end;

        ErrorFormula := Formula;
        try
          Compiler.Compile(Formula)
        except on E: ERbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(AScreenObject.Name, '',
              ErrorFormula, E.Message);
            Formula := '0';
            Compiler.Compile(Formula);
            // send error message
            AnItem.BoundaryFormula[BoundaryFunctionIndex] := Formula;
          end;
        end;
        Expression := Compiler.CurrentExpression;
        if Expression = nil then
        begin
          Formula := '0';
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
        end;
        OKTypes := OkListDataTypes(BoundaryFunctionIndex);
        if not (Expression.ResultType in OKTypes) then
        begin
          frmFormulaErrors.AddFormulaError(AScreenObject.Name, '',
            ErrorFormula, StrInvalidResultType);
          if (rdtInteger in OKTypes) or (rdtDouble in OKTypes) then
          begin
            Formula := '0';
          end
          else if rdtBoolean in OKTypes then
          begin
            Formula := 'False';
          end
          else if rdtString in OKTypes then
          begin
            Formula := '""';
          end;

          Compiler.Compile(Formula);
          // send error message
          AnItem.BoundaryFormula[BoundaryFunctionIndex] := Formula;
          Expression := Compiler.CurrentExpression;
        end;

        Variables := TList.Create;
        DataSets := TList.Create;
        try
          UsedVariables.Assign(Expression.VariablesUsed);
          for VarIndex := 0 to UsedVariables.Count - 1 do
          begin
            VarName := UsedVariables[VarIndex];
            VarPosition := Compiler.IndexOfVariable(VarName);
            Variable := Compiler.Variables[VarPosition];
            AnotherDataSet := LocalModel.DataArrayManager.GetDataSetByName(VarName);
            if AnotherDataSet <> nil then
            begin
              Assert(AnotherDataSet.DataType = Variable.ResultType);
              AnotherDataSet.Initialize;
              LocalModel.DataArrayManager.AddDataSetToCache(AnotherDataSet);
              Variables.Add(Variable);
              DataSets.Add(AnotherDataSet);
            end
            else
            begin
              GlobalVariable := LocalModel.GlobalVariables.GetVariableByName(VarName);
              Assert(GlobalVariable <> nil);
              Assert(Variable.ResultType = GlobalVariable.Format);
            end;
          end;

          UpdateCurrentScreenObject(AScreenObject);
          CellList.Clear;
          AScreenObject.GetCellsToAssign(Formula, nil, nil, CellList,
            alAll, LocalModel);
          for Index := 0 to EliminateIndicies.Count - 1  do
          begin
            CellList.Delete(EliminateIndicies[Index]);
          end;

          if TimeSeries <> nil then
          begin
            PestSeriesName := '' ;
            TimeSeriesName := String(TimeSeries.SeriesName);
          end
          else
          begin
            if CanUsePestParmeters then
            begin
              PestSeriesName := BoundaryGroup.
                PestBoundaryFormula[BoundaryFunctionIndex];
            end
            else
            begin
              PestSeriesName := '';
            end;
            TimeSeriesName := '';
          end;
          Method := BoundaryGroup.
            PestBoundaryMethod[BoundaryFunctionIndex];

          AssignCellList(Expression, CellList, Boundaries[ItemCount, AModel],
            BoundaryFunctionIndex, Variables, DataSets, LocalModel,
            AScreenObject, PestParamName, PestSeriesName, Method, TimeSeriesName);
        finally
          Variables.Free;
          DataSets.Free;
        end;
      end;
      AssignDirectlySpecifiedValues(AnItem, Boundaries[ItemCount, AModel]);

      Inc(ItemCount);

      if ObservationsPresent then
      begin
        if AnItem.EndTime < EndOfLastStressPeriod then
        begin
          if (ItemIndex+1 < Count) then
          begin
            NextItem := Items[ItemIndex+1] as TCustomModflowBoundaryItem;
          end
          else
          begin
            NextItem := nil;
          end;

          if (NextItem = nil) or (AnItem.EndTime < NextItem.StartTime) then
          begin
            ExtraItem := TNoFormulaItem.Create(nil);
            try
              ExtraItem.FStartTime := AnItem.EndTime;
              ExtraItem.FEndTime := EndOfLastStressPeriod;

              Variables := TList.Create;
              DataSets := TList.Create;
              try
                AddSpecificBoundary(AModel);
                SetBoundaryStartAndEndTime(CellList.Count, ExtraItem, ItemCount, AModel);
                AssignListCellLocation(Boundaries[ItemCount, AModel],  CellList);
                for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
                begin
                  if (AnItem.ConductanceIndex < 0)
                    or (AnItem.ConductanceIndex = BoundaryFunctionIndex) then
                  begin
                    // MODFLOW-NWT requires that boundary elevations be higher
                    // than the cell bottom elevation. Therefore, setting
                    // all the formulas to zero can cause problems.
                    Formula := '0';
                  end
                  else
                  begin
                    Formula := AnItem.BoundaryFormula[BoundaryFunctionIndex];
                  end;

                  TimeSeries := LocalModel.Mf6TimesSeries.GetTimeSeriesByName(Formula);
                  if TimeSeries = nil then
                  begin
                    if CanUsePestParmeters then
                    begin
                      PestSeriesName := BoundaryGroup.
                        PestBoundaryFormula[BoundaryFunctionIndex];
                    end
                    else
                    begin
                      PestSeriesName := '';
                    end;
                    TimeSeriesName := '';
                  end
                  else
                  begin
                    PestSeriesName := '';
                    TimeSeriesName := string(TimeSeries.SeriesName);
                  end;
                  Method := BoundaryGroup.
                    PestBoundaryMethod[BoundaryFunctionIndex];
                  if (PestSeriesName <> '') and (PestSeriesName <> '0') then
                  begin
                    ADataSet := LocalModel.DataArrayManager.GetDataSetByName(PestSeriesName);
                    if (ADataSet <> nil) and ADataSet.PestParametersUsed then
                    begin
                      PestSeriesName := ADataSet.Name;
                      case Method of
                        ppmMultiply:
                          begin
                            Formula := '(' + Formula + ') * ' + PestSeriesName;
                          end;
                        ppmAdd:
                          begin
                            Formula := '(' + Formula + ') + ' + PestSeriesName;
                          end;
                      end;
                    end
                    else
                    begin
                      PestParam := LocalModel.GetPestParameterByName(PestSeriesName);
                      if PestParam = nil then
                      begin
                        PestSeriesName := '';
                      end
                      else
                      begin
                        PestParam.IsUsedInTemplate := True;
                        Case Method of
                          ppmMultiply:
                            begin
                              Formula := Format('(%0:s) * %1:g', [Formula, PestParam.Value]);
                            end;
                          ppmAdd:
                            begin
                              Formula := Format('(%0:s) + %1:g', [Formula, PestParam.Value]);
                            end;
                        End;
                        PestSeriesName := PestParam.ParameterName;
                      end;
                    end;
                  end;

                  // The Formula might by a PEST parameter or a TDataArray
                  // that is modified by PEST or a MODFLOW 6 time series.
                  if TimeSeries <> nil then
                  begin
                    PestParamName := '';
                    Formula := '1.';
                  end
                  else
                  begin
                    if CanUsePestParmeters then
                    begin
                      PestParam := LocalModel.GetPestParameterByName(Formula);
                    end
                    else
                    begin
                      PestParam := nil
                    end;
                    if PestParam = nil then
                    begin
                      PestParamName := '';
                    end
                    else
                    begin
                      PestParam.IsUsedInTemplate := True;
                      Formula := FortranFloatToStr(PestParam.Value);
                      PestParamName := PestParam.ParameterName;
                    end;
                  end;
                  Compiler.Compile(Formula);
                  Expression := Compiler.CurrentExpression;

                  CellList.Clear;
                  AScreenObject.GetCellsToAssign(Formula, nil, nil,
                    CellList, alAll, LocalModel);
                  for Index := 0 to EliminateIndicies.Count - 1  do
                  begin
                    CellList.Delete(EliminateIndicies[Index]);
                  end;
                  UpdateCurrentScreenObject(AScreenObject);

                  AssignCellList(Expression, CellList, Boundaries[ItemCount, AModel],
                    BoundaryFunctionIndex, Variables, DataSets, LocalModel,
                    AScreenObject, PestParamName, PestSeriesName, Method,
                    TimeSeriesName);
                end;
              finally
                Variables.Free;
                DataSets.Free;
              end;
              Inc(ItemCount);
            finally
              ExtraItem.Free;
            end;
          end;
        end;
        PriorTime := AnItem.EndTime;
      end;
      Boundaries[StoredCount, AModel].CacheData;
    end;

  finally
    EliminateIndicies.Free;
    UsedVariables.Free;
    CellList.Free;
  end;
end;

procedure TFormulaProperty.ResetBoundaryObserver(Index: integer);
var
  Observer: TObserver;
begin
  if Index < FObserverList.Count then
  begin
    Observer := FObserverList[Index];
    Observer.UpToDate := True;
  end;
end;

procedure TFormulaProperty.UpdateFormulaBlocks(Value: string;
  Position: integer; var FormulaObject: TFormulaObject);
var
  LocalModel: TPhastModel;
  Compiler: TRbwParser;
  Observer: TObserver;
begin
  if FormulaObject.Formula <> Value then
  begin
    LocalModel := ParentModel as TPhastModel;
    if LocalModel <> nil then
    begin
      Compiler := LocalModel.rpThreeDFormulaCompiler;
      Observer := FObserverList[Position];
      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer,
        Compiler);
    end;
    InvalidateModel;
    if not (csDestroying in frmGoPhast.PhastModel.ComponentState)
      and not frmGoPhast.PhastModel.Clearing then
    begin
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FormulaObject, Value,
        frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
        RemoveScreenObjectPropertySubscription,
        RestoreScreenObjectPropertySubscription, self);
    end;
  end;
end;

function TFormulaProperty.CreateFormulaObjectBlocks
  (Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
      end;
  else
    Assert(False);
  end;
  result.AddSubscriptionEvents(RemoveScreenObjectPropertySubscription,
    RestoreScreenObjectPropertySubscription, self);
end;

function TFormulaProperty.CreateFormulaObjectNodes
  (Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompilerNodes;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
      end;
  else
    Assert(False);
  end;
  result.AddSubscriptionEvents(RemoveScreenObjectPropertySubscription,
    RestoreScreenObjectPropertySubscription, self);
end;


procedure TFormulaProperty.CreateObserver(ObserverNameRoot: string;
  var Observer: TObserver; Displayer: TObserver);
var
  ScreenObject: TScreenObject;
  Model: TPhastModel;
begin
  ScreenObject := FScreenObject as TScreenObject;
  Observer := TObserver.Create(nil);
  Observer.UpdateWithName(ObserverNameRoot + ScreenObject.Name);
  if ScreenObject.CanInvalidateModel then
  begin
    Model := FModel as TPhastModel;
    Assert(Model <> nil);
    if Displayer <> nil then
    begin
//      BoundaryObserver.TalksTo(Displayer);
      Observer.TalksTo(Displayer);
    end;
//    BoundaryObserver.TalksTo(Observer);
  end;
end;

constructor TModflowScreenObjectProperty.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  FMf6TimeSeriesNames:= TStringList.Create;
  FMf6TimeSeriesNames.Sorted := True;
  FMf6TimeSeriesNames.Duplicates := dupIgnore;
end;

procedure TModflowScreenObjectProperty.CreateBoundaryObserver;
begin
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  if ScreenObject <> nil then
  begin
    if TScreenObject(FScreenObject).CanInvalidateModel then
    begin
      FBoundaryObserver := TObserver.Create(nil);
      FBoundaryObserver.UpdateWithName(BoundaryObserverPrefix +
        TScreenObject(FScreenObject).Name);
      TScreenObject(FScreenObject).TalksTo(FBoundaryObserver);
      FBoundaryObserver.UpToDate := True;
    end;
  end;
end;

procedure TModflowScreenObjectProperty.CreateObserver(ObserverNameRoot: string;
  var Observer: TObserver; Displayer: TObserver);
var
  ScreenObject: TScreenObject;
  Model: TPhastModel;
begin
  inherited;
  ScreenObject := FScreenObject as TScreenObject;
  if ScreenObject.CanInvalidateModel then
  begin
    Model := FModel as TPhastModel;
    Assert(Model <> nil);
    Assert(BoundaryObserver <> nil);
    if Displayer <> nil then
    begin
      BoundaryObserver.TalksTo(Displayer);
//      Observer.TalksTo(Displayer);
    end;
    BoundaryObserver.TalksTo(Observer);
  end;
end;

{ TCustomLocationObservation }

procedure TCustomLocationObservation.Assign(Source: TPersistent);
var
  SourceItem: TCustomLocationObservation;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomLocationObservation then
  begin
    SourceItem := TCustomLocationObservation(Source);
    Time := SourceItem.Time;
    Comment := SourceItem.Comment;
  end;
  inherited;
end;

function TCustomLocationObservation.GetTime: double;
begin
  result := FTime;
end;

function TCustomLocationObservation.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomLocationObservation;
begin
  result := AnotherItem is TCustomLocationObservation;
  if result then
  begin
    Item := TCustomLocationObservation(AnotherItem);
    result := (Item.Time = Time)
      and (Item.Comment = Comment);
  end;
end;

procedure TCustomLocationObservation.SetComment(const Value: string);
begin
  if FComment <> Value then
  begin
    FComment := Value;
    InvalidateModel;
  end;
end;

procedure TCustomLocationObservation.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    InvalidateModel;
  end;
end;

{ TMultiHeadItem }

procedure TMultiHeadItem.Assign(Source: TPersistent);
var
  SourceItem: TMultiHeadItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TMultiHeadItem then
  begin
    SourceItem := TMultiHeadItem(Source);
    Layer := SourceItem.Layer;
    Proportion := SourceItem.Proportion;
  end;
  inherited;
end;

function TMultiHeadItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMultiHeadItem;
begin
  result := AnotherItem is TMultiHeadItem;
  if result then
  begin
    Item := TMultiHeadItem(AnotherItem);
    result := (Item.Layer = Layer)
      and (Item.Proportion = Proportion);
  end;
end;

procedure TMultiHeadItem.SetLayer(const Value: integer);
begin
  if FLayer <> Value then
  begin
    InvalidateModel;
    FLayer := Value;
  end;
end;

procedure TMultiHeadItem.SetProportion(const Value: double);
begin
  if FProportion <> Value then
  begin
    InvalidateModel;
    FProportion := Value;
  end;
end;

{ TMultiHeadCollection }

constructor TMultiHeadCollection.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(TMultiHeadItem, Model, ScreenObject);
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
end;

function TMultiHeadCollection.GetMultiHeadItem(Index: integer): TMultiHeadItem;
begin
  result := Items[Index] as TMultiHeadItem;
end;

{ TCustomLocationObsBoundary }

procedure TCustomMultilayerLocationObsBoundary.Assign(Source: TPersistent);
var
  LocSource: TCustomMultilayerLocationObsBoundary;
begin
  if Source is TCustomMultilayerLocationObsBoundary then
  begin
    LocSource := TCustomMultilayerLocationObsBoundary(Source);
    LayerFractions := LocSource.LayerFractions;
  end;
  inherited;
end;

constructor TCustomMultilayerLocationObsBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  FLayerFractions := TMultiHeadCollection.Create(Model, ScreenObject);
end;

destructor TCustomMultilayerLocationObsBoundary.Destroy;
begin
  FLayerFractions.Free;
  inherited;
end;

procedure TCustomMultilayerLocationObsBoundary.SetLayerFractions(
  const Value: TMultiHeadCollection);
begin
  FLayerFractions.Assign(Value);
end;

procedure TCustomLocationObsBoundary.Assign(Source: TPersistent);
var
  LocSource: TCustomLocationObsBoundary;
begin
  if Source is TCustomLocationObsBoundary then
  begin
    LocSource := TCustomLocationObsBoundary(Source);
    ObservationName := LocSource.ObservationName;
    Purpose := LocSource.Purpose;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomLocationObsBoundary.BoundaryObserverPrefix: string;
begin
  result := '';
  Assert(False);
end;

constructor TCustomLocationObsBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  FPurpose := ofObserved;
end;

procedure TCustomLocationObsBoundary.SetObservationName(Value: string);
begin
  Value := Trim(Value);
  Value := StringReplace(Value, ' ', '_', [rfReplaceAll]);
  Value := StringReplace(Value, '"', '', [rfReplaceAll]);
  Value := StringReplace(Value, '''', '', [rfReplaceAll]);
  Value := StringReplace(Value, '/', '_', [rfReplaceAll]);
  Value := StringReplace(Value, '\', '_', [rfReplaceAll]);
  if Length(Value) > 12 then
  begin
    Value := Copy(Value, 1, 12);
  end;
  if FObservationName <> Value then
  begin
    InvalidateModel;
    FObservationName := Value;
  end;
end;

procedure TCustomLocationObsBoundary.SetPurpose(const Value
  : TObservationPurpose);
begin
  if FPurpose <> Value then
  begin
    InvalidateModel;
    FPurpose := Value;
  end;
end;

function TCustomLocationObsBoundary.GetItemObsName
  (Item: TCustomLocationObservation): string;
begin
  // Assert(Item.Collection = Values);
  if Item.Collection.Count = 1 then
  begin
    result := ObservationName;
  end
  else
  begin
    result := ObservationName + '_' + IntToStr(Item.Index + 1);
    if Length(result) > 12 then
    begin
      result := ObservationName + IntToStr(Item.Index + 1);
    end;
    if Length(result) > 12 then
    begin
      // The GUI is designed to prevent this from ever being required.
      SetLength(result, 12);
    end;
  end;
end;

{ TFormulaProperty }

constructor TFormulaProperty.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
  FObserverList := TObserverObjectList.Create;
end;

destructor TFormulaProperty.Destroy;
var
  LocalScreenObject: TScreenObject;
  PhastModel: TPhastModel;
  Index: Integer;
  Observer: TObserver;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) then
  begin
    PhastModel := FModel as TPhastModel;
    if (PhastModel <> nil) and not PhastModel.Clearing
      and not (csDestroying in PhastModel.ComponentState)
      and not frmGoPhast.PhastModel.Clearing then
    begin
      for Index := 0 to FObserverList.Count - 1 do
      begin
        Observer := FObserverList[Index];
        LocalScreenObject.StopsTalkingTo(Observer);
      end;
    end;
  end;

  FObserverList.Free;
  inherited;
end;

procedure TFormulaProperty.InvalidateModel;
begin
  if (ScreenObject <> nil)
      and (ScreenObject as TScreenObject).CanInvalidateModel
      and (FModel <> nil) then
  begin
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent. }
    (FModel as TPhastModel).Invalidate(self)
  end;
end;

//procedure TFormulaProperty.UpdateFormula(Value: string; Position: integer;
//  var FormulaObject: TFormulaObject);
//var
//  LocalModel: TPhastModel;
//  Compiler: TRbwParser;
//  Observer: TObserver;
//begin
//  if FormulaObject.Formula <> Value then
//  begin
//    LocalModel := ParentModel as TPhastModel;
//    if LocalModel <> nil then
//    begin
//      Compiler := LocalModel.rpThreeDFormulaCompiler;
//      Observer := FObserverList[Position];
//      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer,
//        Compiler);
//    end;
//    InvalidateModel;
//    if not (csDestroying in frmGoPhast.PhastModel.ComponentState)
//      and not frmGoPhast.PhastModel.Clearing then
//    begin
//      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FormulaObject, Value,
//        frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
//        RemoveScreenObjectPropertySubscription,
//        RestoreScreenObjectPropertySubscription, self);
//    end;
//  end;
//end;

procedure TFormulaProperty.UpdateFormulaDependencies(OldFormula: string;
  var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
var
  OldUses: TStringList;
  NewUses: TStringList;
  Position: integer;
  DS: TObserver;
  ParentScreenObject: TScreenObject;
  Index: integer;
  procedure CompileFormula(var AFormula: string; UsesList: TStringList);
  begin
    if AFormula <> '' then
    begin
      try
        Compiler.Compile(AFormula);
        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
      except
        on E: ERbwParserError do
        begin
        end;
      end;
    end;
  end;

begin
  OldFormula := Trim(OldFormula);
  NewFormula := Trim(NewFormula);
  if OldFormula = NewFormula then
  begin
    Exit;
  end;
  if (frmGoPhast.PhastModel <> nil) and
    ((frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> []) then
  begin
    Exit;
  end;
  ParentScreenObject := ScreenObject as TScreenObject;
  if (ParentScreenObject = nil)
  // or not ParentScreenObject.CanInvalidateModel then
  // 3
  { or not ParentScreenObject.CanInvalidateModel } then
  begin
    Exit;
  end;
  OldUses := TStringList.Create;
  NewUses := TStringList.Create;
  try
    CompileFormula(OldFormula, OldUses);
    CompileFormula(NewFormula, NewUses);
    for Index := OldUses.Count - 1 downto 0 do
    begin
      Position := NewUses.IndexOf(OldUses[Index]);
      if Position >= 0 then
      begin
        OldUses.Delete(Index);
        NewUses.Delete(Position);
      end;
    end;
    for Index := 0 to OldUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
      Assert(DS <> nil);
      DS.StopsTalkingTo(Observer);
    end;
    for Index := 0 to NewUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
      Assert(DS <> nil);
      DS.TalksTo(Observer);
    end;
  finally
    NewUses.Free;
    OldUses.Free;
  end;
end;

procedure TFormulaProperty.UpdateFormulaNodes(Value: string; Position: integer;
  var FormulaObject: TFormulaObject);
var
  LocalModel: TPhastModel;
  Compiler: TRbwParser;
  Observer: TObserver;
begin
  if FormulaObject.Formula <> Value then
  begin
    LocalModel := ParentModel as TPhastModel;
    if LocalModel <> nil then
    begin
      Compiler := LocalModel.rpThreeDFormulaCompilerNodes;
      Observer := FObserverList[Position];
      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer,
        Compiler);
    end;
    InvalidateModel;
    if not (csDestroying in frmGoPhast.PhastModel.ComponentState)
      and not frmGoPhast.PhastModel.Clearing then
    begin
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FormulaObject, Value,
        frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes,
        RemoveScreenObjectPropertySubscription,
        RestoreScreenObjectPropertySubscription, self);
    end;
  end;
end;

procedure TModflowParamBoundary.SetInterp(const Value
  : TMf6InterpolationMethods);
begin
  if FInterp <> Value then
  begin
    InvalidateModel;
    FInterp := Value;
  end;
end;

procedure TCustomMF_BoundColl.AssignBoundaryFormula(AModel: TBaseModel;
  const SeriesName: string; SeriesMethod: TPestParamMethod;
  PestItems, TimeSeriesItems: TStringList; const ItemFormula: string; Writer: TObject;
  var BoundaryValue: TBoundaryValue);
var
  LocalModel: TCustomModel;
  PestParam: TModflowSteadyParameter;
  Formula: string;
  PestDataArray: TDataArray;
  PestParamSeries: TModflowSteadyParameter;
  CustomWriter: TCustomFileWriter;
  TimeSeries: TMf6TimeSeries;
begin
  CustomWriter := nil;
  LocalModel := AModel as TCustomModel;
  TimeSeries := LocalModel.Mf6TimesSeries.GetTimeSeriesByName(ItemFormula);
  if TimeSeries = nil then
  begin
    TimeSeriesItems.Add('');
    PestParam := LocalModel.GetPestParameterByName(ItemFormula);
    if PestParam = nil then
    begin
      Formula := ItemFormula;
      PestDataArray := LocalModel.DataArrayManager.GetDataSetByName(Formula);
      if (PestDataArray <> nil) and PestDataArray.PestParametersUsed then
      begin
        PestItems.Add(PestDataArray.Name);
        if CustomWriter = nil then
        begin
          CustomWriter := Writer as TCustomFileWriter;
        end;
        CustomWriter.AddUsedPestDataArray(PestDataArray);
      end
      else
      begin
        PestItems.Add('');
      end;
    end
    else
    begin
      PestParam.IsUsedInTemplate := True;
      Formula := FortranFloatToStr(PestParam.Value);
      PestItems.Add(PestParam.ParameterName);
    end;
    if SeriesName <> '' then
    begin
      PestParamSeries := LocalModel.GetPestParameterByName(SeriesName);
      if PestParamSeries = nil then
      begin
        PestDataArray := LocalModel.DataArrayManager.GetDataSetByName(SeriesName);
        if (PestDataArray <> nil) and PestDataArray.PestParametersUsed then
        begin
          case SeriesMethod of
            ppmMultiply:
              begin
                Formula := Format('(%0:s) * %1:s', [Formula, PestDataArray.Name]);
              end;
            ppmAdd:
              begin
                Formula := Format('(%0:s) + %1:s', [Formula, PestDataArray.Name]);
              end;
          end;
          if CustomWriter = nil then
          begin
            CustomWriter := Writer as TCustomFileWriter;
          end;
          CustomWriter.AddUsedPestDataArray(PestDataArray);
        end;
      end
      else
      begin
        PestParamSeries.IsUsedInTemplate := True;
        case SeriesMethod of
          ppmMultiply:
            begin
              Formula := Format('(%0:s) * %1:g',
                [Formula, PestParamSeries.Value]);
            end;
          ppmAdd:
            begin
              Formula := Format('(%0:s) + %1:g',
                [Formula, PestParamSeries.Value]);
            end;
        end;
      end;
    end;
  end
  else
  begin
    if CustomWriter = nil then
    begin
      CustomWriter := Writer as TCustomFileWriter;
      CustomWriter.TimeSeriesNames.Add(string(TimeSeries.SeriesName));
    end;
    PestItems.Add('');
    TimeSeriesItems.Add(string(TimeSeries.SeriesName));
    Formula := '1';
  end;
  BoundaryValue.Formula := Formula;
end;

{ TCustomStringValueItem }

procedure TCustomStringValueItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomStringValueItem then
  begin
    Value := TCustomStringValueItem(Source).Value;
  end;
  inherited;
end;

constructor TCustomStringValueItem.Create(Collection: TCollection);
begin
  inherited;
  FObserver:= TObserver.Create(nil);

  OnRemoveSubscription := StringValueRemoveSubscription;
  OnRestoreSubscription := StringValueRestoreSubscription;
  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  FValue.AddSubscriptionEvents(StringValueRemoveSubscription,
  StringValueRestoreSubscription, self);
end;

destructor TCustomStringValueItem.Destroy;
begin
  Value := '0';
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    StringValueRemoveSubscription,
    StringValueRestoreSubscription, self);
  FObserver.Free;
  inherited;
end;

function TCustomStringValueItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserver;
end;

function TCustomStringValueItem.GetScreenObject: TObject;
begin
  result := StringCollection.ScreenObject;
end;

function TCustomStringValueItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TCustomStringValueItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TCustomStringValueItem then
  begin
    result := Value = TCustomStringValueItem(AnotherItem).Value;
  end
  else
  begin
    result := false;
  end;
end;

procedure TCustomStringValueItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TCustomStringValueItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TCustomStringValueItem.SetValue(const Value: string);
var
  Dummy: Integer;
begin
  Dummy := 0;
  UpdateFormulaBlocks(Value, Dummy, FValue);
end;

function TCustomStringValueItem.StringCollection: TCustomStringCollection;
begin
  result := Collection as TCustomStringCollection;
end;

{ TCustomStringCollection }

constructor TCustomStringCollection.Create(ItemClass: TCollectionItemClass;
  Model: TBaseModel; AScreenObject: TObject; ParentCollection: TCustomNonSpatialBoundColl);
begin
  inherited Create(ItemClass, Model, ScreenObject);
  // ParentCollection might be nil.
  FParentCollection := ParentCollection;
end;

{ TGwtConcStringCollection }

function TGwtConcStringCollection.Add: TGwtConcStringValueItem;
begin
  result := inherited Add as TGwtConcStringValueItem;
end;

constructor TGwtConcStringCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TCustomMF_BoundColl);
begin
  inherited Create(TGwtConcStringValueItem, Model, AScreenObject,
    ParentCollection);
end;

function TGwtConcStringCollection.GetItems(
  Index: Integer): TGwtConcStringValueItem;
begin
  while Index >= Count do
  begin
    Add;
  end;
  result := inherited Items[Index] as  TGwtConcStringValueItem
end;

procedure TGwtConcStringCollection.SetItems(Index: Integer;
  const Value: TGwtConcStringValueItem);
begin
  Assert(Index >= 0);
  while Index >= Count do
  begin
    Add;
  end;
  inherited Items[Index] := Value;
end;

procedure TGwtConcStringCollection.SetUsedForPestSeries(const Value: Boolean);
begin
  FUsedForPestSeries := Value;
end;

{ TGwtConcStringValueItem }

constructor TGwtConcStringValueItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TGwtConcStringCollection then
  begin
    if not TGwtConcStringCollection(Collection).UsedForPestSeries then
    begin
      Value := '0';
    end;
  end
  else
  begin
    Value := '0';
  end;
end;

end.
