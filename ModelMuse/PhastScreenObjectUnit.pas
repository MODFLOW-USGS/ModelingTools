{@abstract(The main purpose of @name is to define @link(TPhastScreenObject)
  which extends @link(TScreenObject) by adding boundary conditions and
  the ability to handle PHAST-style interpolation.)
  @name also defines @link(TCustomPhastBoundaryCondition) and a series of
  descendants.  The descendants represent boundary conditions in GoPhast.

  Another class defined here is @link(TMultiValueScreenObject) which is used
  when importing zones from an existing PHAST model.

  Finally @link(TPhastScreenObjectItem) and @link(TPhastScreenObjectCollection)
  are used to save @link(TPhastSCreenObject)s to a file or read them from
  a file.

  See @link(TPhastInterpolationValues) for an explanation of
  PHAST-style interpolation.}
unit PhastScreenObjectUnit;

interface

uses AbstractGridUnit, Classes, ScreenObjectUnit, DataSetUnit, PhastDataSets, PhastGridUnit,
  Contnrs, RbwParser, GoPhastTypes, SysUtils, SubscriptionUnit;

type
  TPhastScreenObject = class;

  // @name specifies the type of solution in a specified head
  // boundary condition. stAssociated means that the solution
  // is merely associated with the head.  stSpecified specifies
  /// that the solution also has a fixed value.
  TSolutionType = (stAssociated, stSpecified);


  

  {@abstract(@name is and abstract base class.  Its descendants,
    @link(TIntegerPhastBoundaryCondition) and
    @link(TRealPhastBoundaryCondition), are
    used to define the value of one aspect of a boundary
    condition for one time period.)}
  TCustomPhastBoundaryCondition = class(TInterpValuesItem)
  private
    // @name: string;
    // See @link(Expression).
    FExpression: string;
    // @name: TList;
    // @name is a list of the @link(TDataArray)s that are directly used in
    // @link(MixtureFormula).
    FMixtureDataSetList: TList;
    // @name: string;
    // See @link(MixtureFormula).
    FMixtureFormula: string;
    // @name: @link(TObserver);
    // @name is used to monitor changes to the @link(TDataArray)s that are
    // used in @link(MixtureFormula).
    FMixtureObserver: TObserver;
    // @name: double;
    // See @link(Time).
    FTime: double;
    // @name determines the @link(TDataArray)s that are used in
    // @link(MixtureFormula) and has @link(FMixtureObserver) subscribe to
    // each of them.
    procedure AddMixtureSubscriptions;
    // See @link(Distance1).
    function GetDistance1: double;
    // See @link(Distance2).
    function GetDistance2: double;
    // See @link(InterpolationDirection).
    function GetInterpolationDirection: TInterpolationDirection;
    // See @link(MixtureExpression).
    function GetMixtureExpression: string;
    // See @link(ScreenObject).
    function GetScreenObject: TPhastScreenObject;
    // See @link(UsePHAST_Interpolation).
    function GetUsePHAST_Interpolation: boolean;
    // @name causes @link(FMixtureObserver) to unsubscribe from
    // every @link(TDataArray) that it is observing.
    procedure RemoveMixtureSubscriptions;
    // See @link(Distance1).
    procedure SetDistance1(const Value: double);
    // See @link(Distance2).
    procedure SetDistance2(const Value: double);
    // See @link(Expression).
    procedure SetExpression(const Value: string);
    // See @link(InterpolationDirection).
    procedure SetInterpolationDirection(const Value: TInterpolationDirection);
    // See @link(MixtureExpression).
    procedure SetMixtureExpression(const Value: string);
    // See @link(MixtureFormula).
    procedure SetMixtureFormula(const Value: string);
    // See @link(Time).
    procedure SetTime(const Value: double);
    // See @link(UsePHAST_Interpolation).
    procedure SetUsePHAST_Interpolation(const Value: boolean);
  protected
    // See @link(Datatype).
    function GetDatatype: TRbwDataType; virtual; abstract;
    // @name sets @link(FMixtureObserver).UpToDate := True;
    procedure ResetMixtureSubscription;
    // @name transfers the value stored in @link(FMixtureFormula) to
    // @link(MixtureExpression).  See @link(MixtureFormula).
    procedure UpdateMixtureExpression;
  public
    // @name copies values from @classname or @link(TInterpValuesItem)
    // to the object calling @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    destructor Destroy; override;
    // @name gets the @link(TSparseArrayPhastDataSet) to which this
    // @classname applies.
    function GetDataSet: TSparseArrayPhastDataSet;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.MixtureFormula).
    // Mixture Expression can not be set until the entire model has
    // been loaded.  See @link(MixtureFormula).
    // See @link(TPhastInterpolationValues).
    property MixtureExpression: string read GetMixtureExpression write
      SetMixtureExpression;
    // @name is used to read the @link(TPhastScreenObject) to which
    // this @classname applies.
    property ScreenObject: TPhastScreenObject read GetScreenObject;
  published
    // @name specifies the type of data (real number, integer, boolean,
    // or string) stored in the @classname.  However, at the time this
    // was written only real numbers and integers were supported because
    // those were the only types supported in descendants of
    // @link(TSparseArrayPhastDataSet). See @link(GetDataSet).
    property DataType: TRbwDataType read GetDatatype;
    // See TInterpValuesItem.Values.@link(TPhastInterpolationValues.Distance1).
    // See @link(TPhastInterpolationValues).
    property Distance1: double read GetDistance1 write SetDistance1;
    // See TInterpValuesItem.Values.@link(TPhastInterpolationValues.Distance2).
    // See @link(TPhastInterpolationValues).
    property Distance2: double read GetDistance2 write SetDistance2;
    // @name is the formula used to set the value of the @classname when
    // PHAST style interpolation is not used (that is:
    // @link(UsePHAST_Interpolation) = @false).
    // See @link(TPhastInterpolationValues).
    property Expression: string read FExpression write SetExpression;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.InterpolationDirection).
    property InterpolationDirection: TInterpolationDirection
      read GetInterpolationDirection write SetInterpolationDirection;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.MixtureFormula).
    // @name is used to read and write @link(MixtureExpression).
    // After the model is loaded, @link(UpdateMixtureExpression) is called
    // to transfer the value saved in @link(FMixtureFormula) to
    // @link(MixtureExpression).
    // See @link(TPhastInterpolationValues).
    property MixtureFormula: string read GetMixtureExpression write
      SetMixtureFormula;
    // @name is the time at which this @classname takes effect.
    property Time: double read FTime write SetTime;
    // @name indicates whether or not PHAST-style interpolation will be used.
    // See @link(TPhastInterpolationValues).
    property UsePHAST_Interpolation: boolean read GetUsePHAST_Interpolation write
      SetUsePHAST_Interpolation;
  end;

  {@abstract(@name is used to define the value of one aspect of a boundary
    condition for one time period.  The aspect of the boundary condition
    must be a real number.  An example is the head in a specified head
    boundary.)}
  TRealPhastBoundaryCondition = class(TCustomPhastBoundaryCondition)
  private
    // See @link(Value1).
    function GetValue1: double;
    // See @link(Value2).
    function GetValue2: double;
    // See @link(Value1).
    procedure SetValue1(const Value: double);
    // See @link(Value2).
    procedure SetValue2(const Value: double);
  protected
    // @name indicates that this boundary condition
    // represents a real number.
    function GetDatatype: TRbwDataType; override;
  public
    // If PHAST-style interpolation will be used, @name indicates the first
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value1: double read GetValue1 write SetValue1;
    // If PHAST-style interpolation will be used, @name indicates the second
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value2: double read GetValue2 write SetValue2;
  end;

  {@abstract(@name is used to define the value of one aspect of a boundary
    condition for one time period.  The aspect of the boundary condition
    must be an integer.  An example is the associated solution
    in a specified head boundary.)}
  TIntegerPhastBoundaryCondition = class(TCustomPhastBoundaryCondition)
  private
    // See @link(Value1).
    function GetValue1: integer;
    // See @link(Value2).
    function GetValue2: integer;
    // See @link(Value1).
    procedure SetValue1(const Value: integer);
    // See @link(Value2).
    procedure SetValue2(const Value: integer);
  protected
    // @name indicates that this boundary condition
    // represents an integer.
    function GetDatatype: TRbwDataType; override;
  public
    // If PHAST-style interpolation will be used, @name indicates the first
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value1: integer read GetValue1 write SetValue1;
    // If PHAST-style interpolation will be used, @name indicates the second
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value2: integer read GetValue2 write SetValue2;
  end;

  {@abstract(@name is an abstract base class.  Its descendants
    store a series of @link(TCustomPhastBoundaryCondition)s.
    The series define how one aspect of a boundary condition changes
    with time.)}
  TCustomPhastBoundaryCollection = class(TPhastCollection)
  private
    // @name: @link(TPhastScreenObject);
    // See @link(ScreenObject).
    FScreenObject: TPhastScreenObject;
    // @name: @link(TTimeList);
    // See @link(TimeList).
    FTimeList: TTimeList;
    // See @link(TimeList).
    procedure SetTimeList(const Value: TTimeList);
  protected
    // @name specifies the type of data (real number, integer, boolean,
    // or string) stored in the @classname.  However, at the time this
    // was written only real numbers and integers were supported because
    // those were the only types supported in descendants of
    // @link(TSparseArrayPhastDataSet).
    // See TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.GetDataSet).
    function GetDatatype: TRbwDataType; virtual; abstract;
    // @name is the @link(TTimeList) that stores the @link(TDataArray)s
    // for this boundary condition.
    property TimeList: TTimeList read FTimeList write SetTimeList;
    // @name calls TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.UpdateMixtureExpression) for each
    // of its Items.
    procedure UpdateMixtureExpression;
  public
    // @name removes all of the @classname items and invalidates
    // @link(TimeList).
    procedure Clear;
    // @name gets the @link(TSparseArrayPhastDataSet) from @link(TimeList)
    // that starts at ATime.  If there isn't such a data set already,
    // one is created.
    function GetDataSet(const ATime: double): TSparseArrayPhastDataSet;
    // @name is the @link(TPhastScreenObject) that owns this @classname.
    property ScreenObject: TPhastScreenObject read FScreenObject write
      FScreenObject;
  end;

  {@abstract(@name stores a collection of @link(TRealPhastBoundaryCondition)s.)}
  TRealPhastBoundaries = class(TCustomPhastBoundaryCollection)
  protected
    // @name indicates that this boundary condition
    // represents a real number.
    function GetDatatype: TRbwDataType; override;
  public
    // @name creates an instance of @classname and sets the type of
    // @link(TCustomPhastBoundaryCondition) used by the @classname to be
    // @link(TRealPhastBoundaryCondition).
    constructor Create;
  end;

  {@abstract(@name stores a collection of
  // @link(TIntegerPhastBoundaryCondition)s.)}
  TIntegerPhastBoundaries = class(TCustomPhastBoundaryCollection)
  protected
    // @name indicates that this boundary condition
    // represents an integer.
    function GetDatatype: TRbwDataType; override;
  public
    // @name creates an instance of @classname and sets the type of
    // @link(TCustomPhastBoundaryCondition) used by the @classname to be
    // @link(TIntegerPhastBoundaryCondition).
    constructor Create;
  end;

  {@abstract(@name is an abstract base class.  Its descendants are used to
    define all aspects of one type of boundary condition for one
    @link(TPhastScreenObject) including those aspects that vary with time.)}
  TCustomPhastBoundary = class(TPersistent)
  private
    // @name: @link(TPhastScreenObject);
    // See @link(ScreenObject).
    FScreenObject: TPhastScreenObject;
  protected
    // @name clears all the @link(TCustomPhastBoundaryCollection)s that are
    // part of the @classname.
    procedure Clear; virtual; abstract;
    // See @link(ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); virtual;
    // For boundary condition data sets that do not vary with time,
    // @name is used to set the formula to be applied to the data set
    // for this @classname.  If Formula = '', the data set specified
    // by DataSetName will no long be affected by the @link(ScreenObject).
    procedure UpdateBoundaryDataSet(const DataSetName, Formula: string);
    // @name calls TCustomPhastBoundaryCollection.@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) for each of
    // the @link(TCustomPhastBoundaryCollection) it owns.
    procedure UpdateMixtureExpression; virtual; abstract;
  public
    // @name is the @link(TPhastScreenObject) that owns this @classname.
    property ScreenObject: TPhastScreenObject read FScreenObject write
      SetScreenObject;
  end;

  {@abstract(@name represents a boundary condition that only applies to
    one @link(TViewDirection).)}
  TCustomOrientedPhastBoundary = class(TCustomPhastBoundary)
  private
    // @name: @link(TViewDirection);
    // See @link(Orientation).
    FOrientation: TViewDirection;
    // See @link(Orientation).
    procedure SetOrientation(const Value: TViewDirection); virtual;
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
  public
    // If Source is a @classname, @name copies the @link(Orientation)
    // of Source.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the @link(TViewDirection) for this @classname.
    property Orientation: TViewDirection read FOrientation write SetOrientation
      stored False;
  end;

  {@abstract(@name represents a flux boundary in PHAST for one
    @link(TPhastScreenObject).)}
  TFluxBoundary = class(TCustomOrientedPhastBoundary)
  private
    // @name: @link(TIntegerPhastBoundaries);
    // See @link(AssociatedSolution).
    FAssociatedSolution: TIntegerPhastBoundaries;
    // @name: @link(TRealPhastBoundaries);
    // See @link(Flux).
    FFlux: TRealPhastBoundaries;
    // See @link(AssociatedSolution).
    procedure SetAssociatedSolution(const Value: TIntegerPhastBoundaries);
    // See @link(Flux).
    procedure SetFlux(const Value: TRealPhastBoundaries);
    // See TCustomOrientedPhastBoundary.@link(
    // TCustomOrientedPhastBoundary.Orientation).
    // @name sets AssociatedSolution.@link(
    // TCustomPhastBoundaryCollection.TimeList) and
    // Flux.@link(TCustomPhastBoundaryCollection.TimeList).
    procedure SetOrientation(const Value: TViewDirection); override;
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    // @name calls @link(AssociatedSolution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(Flux).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
  public
    // If Source is a @classname, @name copies @link(AssociatedSolution) and
    // @link(Flux).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(AssociatedSolution) and
    // @link(Flux).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name represents how the solution associated with the flux varies
    // through time in the current @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FAssociatedSolution write SetAssociatedSolution;
    // @name represents how the flux varies
    // through time in the current @link(TCustomPhastBoundary.ScreenObject).
    property Flux: TRealPhastBoundaries read FFlux write SetFlux;
  end;

  {@abstract(@name represents a leaky boundary in PHAST for one
    @link(TPhastScreenObject).)}
  TLeakyBoundary = class(TCustomOrientedPhastBoundary)
  private
    // @name: @link(TIntegerPhastBoundaries);
    // See @link(AssociatedSolution).
    FAssociatedSolution: TIntegerPhastBoundaries;
    // @name: @link(TRealPhastBoundaries);
    // See @link(Head).
    FHead: TRealPhastBoundaries;
    // @name: string;
    // See @link(HydraulicConductivity).
    FHydraulicConductivity: string;
    // @name: string;
    // See @link(Thickness).
    FThickness: string;
    // See @link(HydraulicConductivity).
    function GetHydraulicConductivity: string;
    // See @link(AssociatedSolution).
    procedure SetAssociatedSolution(const Value: TIntegerPhastBoundaries);
    // See @link(Head).
    procedure SetHead(const Value: TRealPhastBoundaries);
    // See TCustomOrientedPhastBoundary.@link(
    // TCustomOrientedPhastBoundary.Orientation).
    // @name sets AssociatedSolution.@link(
    // TCustomPhastBoundaryCollection.TimeList) and
    // Head.@link(TCustomPhastBoundaryCollection.TimeList).
    procedure SetOrientation(const Value: TViewDirection); override;
    // See @link(HydraulicConductivity).
    procedure SetHydraulicConductivity(const Value: string);
    // See @link(Thickness).
    procedure SetThickness(const Value: string);
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    // @name calls @link(AssociatedSolution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(Head).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
  public
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // @link(Head), @link(HydraulicConductivity), and @link(Thickness).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(AssociatedSolution) and
    // @link(Head).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // If Source is a @classname, @name copies @link(AssociatedSolution), and
    // @link(Head) but not @link(HydraulicConductivity), and @link(Thickness).
    // @name is used in @link(TUndoSetPhastScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
    // @name resets the formulas for @link(HydraulicConductivity) and
    // @link(Thickness) to '' if
    // (@link(Head).Count = 0) and (@link(AssociatedSolution).Count = 0).
    procedure Reset;
  published
    // @name defines how the associated solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FAssociatedSolution write SetAssociatedSolution;
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Head: TRealPhastBoundaries read FHead write SetHead;
    // @name is the formula for specifying the leaky boundary
    // hydraulic conductivity
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property HydraulicConductivity: string read GetHydraulicConductivity write
      SetHydraulicConductivity;
    // @name is the formula for specifying the leaky boundary
    // thickness
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Thickness: string read FThickness write SetThickness;
  end;

  {@abstract(@name represents a river boundary in PHAST for one
    @link(TPhastScreenObject).)}
  TRiverBoundary = class(TCustomPhastBoundary)
  private
    // @name: @link(TIntegerPhastBoundaries);
    // See @link(AssociatedSolution).
    FAssociatedSolution: TIntegerPhastBoundaries;
    // @name: string;
    // See @link(BedHydraulicConductivity).
    FBedHydraulicConductivity: string;
    // @name: string;
    // See @link(BedThickness).
    FBedThickness: string;
    // @name: string;
    // See @link(Depth).
    FDepth: string;
    // @name: string;
    // See @link(Description).
    FDescription: string;
    // @name: @link(TRealPhastBoundaries);
    // See @link(Head).
    FHead: TRealPhastBoundaries;
    // @name: string;
    // See @link(Width).
    FWidth: string;
    // See @link(AssociatedSolution).
    procedure SetAssociatedSolution(const Value: TIntegerPhastBoundaries);
    // See @link(BedHydraulicConductivity).
    procedure SetBedHydraulicConductivity(const Value: string);
    // See @link(BedThickness).
    procedure SetBedThickness(const Value: string);
    // See @link(Depth).
    procedure SetDepth(const Value: string);
    // See @link(Description).
    procedure SetDescription(const Value: string);
    // See @link(Head).
    procedure SetHead(const Value: TRealPhastBoundaries);
    // See @link(Width).
    procedure SetWidth(const Value: string);
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    // @name calls @link(AssociatedSolution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(Head).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
  public
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // @link(BedHydraulicConductivity), @link(BedThickness), @link(Depth),
    // @link(Description), @link(Head), and @link(Width).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(AssociatedSolution) and
    // @link(Head).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name returns true if all the published properties have been set
    // to values that indicate that a river boundary is present.
    function IsBoundary: boolean;
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // and @link(Head) but not @link(BedHydraulicConductivity),
    // @link(BedThickness), @link(Depth),
    // @link(Description), and @link(Width).
    // @name is used in @link(TUndoSetPhastScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
  published
    // @name defines how the associated solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FAssociatedSolution write SetAssociatedSolution;
    // @name is the formula for specifying the river bed
    // hydraulic conductivity
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property BedHydraulicConductivity: string read FBedHydraulicConductivity
      write SetBedHydraulicConductivity;
    // @name is the formula for specifying the river bed thickness
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property BedThickness: string read FBedThickness write SetBedThickness;
    // @name is the formula for specifying the river depth
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Depth: string read FDepth write SetDepth;
    // @name is the description of the river
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Description: string read FDescription write SetDescription;
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Head: TRealPhastBoundaries read FHead write SetHead;
    // @name is the formula for specifying the river width
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Width: string read FWidth write SetWidth;
  end;

  {@abstract(@name represents a specified head boundary in PHAST for one
    @link(TPhastScreenObject).)}
  TSpecifiedHeadBoundary = class(TCustomPhastBoundary)
  private
    // @name: @link(TRealPhastBoundaries);
    // See @link(Head).
    FHead: TRealPhastBoundaries;
    // @name: @link(TIntegerPhastBoundaries);
    // See @link(AssociatedSolution).
    FSolution: TIntegerPhastBoundaries;
    // @name: @link(TSolutionType);
    // See @link(SolutionType).
    FSolutionType: TSolutionType;
    // See @link(Head).
    procedure SetHead(const Value: TRealPhastBoundaries);
    // See @link(AssociatedSolution).
    procedure SetSolution(const Value: TIntegerPhastBoundaries);
    // See @link(SolutionType).
    procedure SetSolutionType(const Value: TSolutionType);
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    // @name calls @link(AssociatedSolution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(Head).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
  public
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // @link(Head), and @link(SolutionType).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(AssociatedSolution) and
    // @link(Head).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name defines how the solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FSolution write SetSolution;
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Head: TRealPhastBoundaries read FHead write SetHead;
    // @name specifies whether @link(AssociatedSolution) repesents
    // a specified solution or an associated solution.
    property SolutionType: TSolutionType read FSolutionType write
      SetSolutionType;
  end;

  {@abstract(@name is retained only for backwards compatibility.)}
  TSpecifiedSolutionBoundary = class(TCustomPhastBoundary)
  private
    FSolution: TIntegerPhastBoundaries;
    procedure SetSolution(const Value: TIntegerPhastBoundaries);
  protected
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    procedure UpdateMixtureExpression; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  published
    property Solution: TIntegerPhastBoundaries read FSolution write SetSolution;
  end;

  {@abstract(@name an open interval in a well boundary in PHAST.)}
  TWellInterval = class(TCollectionItem)
  private
    // @name: double;
    // See @link(FirstElevation).
    FFirstElevation: double;
    // @name: double;
    // See @link(SecondElevation).
    FSecondElevation: double;
    // See @link(FirstElevation).
    procedure SetFirstElevation(const Value: double);
    // See @link(SecondElevation).
    procedure SetSecondElevation(const Value: double);
  public
    // If Source is a @classname, @name copies @link(FirstElevation) and
    // @link(SecondElevation) from Source.
    procedure Assign(Source: TPersistent); override;
  published
    // @name represents elevation or depth of one end of the well
    // screen in the open interval.
    property FirstElevation: double read FFirstElevation write
      SetFirstElevation;
    // @name represents elevation or depth of the end of the well
    // screen opposite @link(FirstElevation) in the open interval.
    property SecondElevation: double read FSecondElevation write
      SetSecondElevation;
  end;

  {@abstract(@name represents a series of @link(TWellInterval)s.)}
  TWellIntervals = class(TPhastCollection)
  public
    // @name creates an instance of @classname and the type of the
    // Items to be @link(TWellInterval).
    constructor Create;
  end;

  {@abstract(@name represents a well in PHAST for one
    @link(TPhastScreenObject).)}
  TWellBoundary = class(TCustomPhastBoundary)
  private
    // @name: boolean;
    // See @link(AllocateByPressureAndMobility).
    FAllocateByPressureAndMobility: boolean;
    // @name: string;
    // See @link(Description).
    FDescription: string;
    // @name: double;
    // See @link(Diameter).
    FDiameter: double;
    // @name: @link(TRealPhastBoundaries);
    // See @link(InjectionOrPumpingRate).
    FInjectionOrPumpingRate: TRealPhastBoundaries;
    // @name: @link(TWellIntervals);
    // See @link(Intervals).
    FIntervals: TWellIntervals;
    // @name: double;
    // See @link(LandSurfaceDatum).
    FLandSurfaceDatum: double;
    // @name: @link(TIntegerPhastBoundaries);
    // See @link(Solution).
    FSolution: TIntegerPhastBoundaries;
    // @name: @link(TWellElevationFormat);
    // See @link(WellElevationFormat).
    FWellElevationFormat: TWellElevationFormat;
    // See @link(AllocateByPressureAndMobility).
    procedure SetAllocateByPressureAndMobility(const Value: boolean);
    // See @link(Description).
    procedure SetDescription(const Value: string);
    // See @link(Diameter).
    procedure SetDiameter(const Value: double);
    // See @link(InjectionOrPumpingRate).
    procedure SetInjectionOrPumpingRate(const Value: TRealPhastBoundaries);
    // See @link(Intervals).
    procedure SetIntervals(const Value: TWellIntervals);
    // See @link(LandSurfaceDatum).
    procedure SetLandSurfaceDatum(const Value: double);
    // See @link(Solution).
    procedure SetSolution(const Value: TIntegerPhastBoundaries);
    // See @link(WellElevationFormat).
    procedure SetWellElevationFormat(const Value: TWellElevationFormat);
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TPhastScreenObject); override;
    // @name calls @link(Solution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(InjectionOrPumpingRate).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
  public
    // If Source is a @classname, @name copies
    // @link(AllocateByPressureAndMobility), @link(Description),
    // @link(Diameter), @link(InjectionOrPumpingRate), @link(Intervals),
    // @link(LandSurfaceDatum), @link(Solution), and @link(WellElevationFormat).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(InjectionOrPumpingRate) and
    // @link(Solution).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name returns true if all the published properties have been set
    // to values that indicate that a well boundary is present.
    function IsBoundary: boolean;
    // If Source is a @classname, @name copies @link(Solution) and
    // @link(InjectionOrPumpingRate) but not
    // @link(AllocateByPressureAndMobility), @link(Description),
    // @link(Diameter), @link(Intervals),
    // @link(LandSurfaceDatum), and @link(WellElevationFormat).
    // @name is used in @link(TUndoSetPhastScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
  published
    // If @name is @true, PHAST will distribute the pumping amount
    // among layers by taking into account the pressure and permeability in
    // the layers in which the well has an open screen interval.
    // If @name is @false, PHAST will not consider the pressure
    // and permeability in the layers when distributing the pumping
    // amount among layers.
    property AllocateByPressureAndMobility: boolean read
      FAllocateByPressureAndMobility write SetAllocateByPressureAndMobility
      default True;
    // @name is a description of the well.
    property Description: string read FDescription write SetDescription;
    // @name is the diameter of the well.
    property Diameter: double read FDiameter write SetDiameter;
    // @name is the rate at which water is pumped into or out of the porous
    // medium.
    // A positive @name means flow out of the porous medium.
    property InjectionOrPumpingRate: TRealPhastBoundaries read
      FInjectionOrPumpingRate write SetInjectionOrPumpingRate;
    // @name defines the open well intervals.
    // See @link(WellElevationFormat) and @link(LandSurfaceDatum).
    property Intervals: TWellIntervals read FIntervals write SetIntervals;
    // If @link(WellElevationFormat) = wefDepth, @name represents the datum
    // from which the elevations (really depths) in @link(Intervals) are
    // measured. If @link(WellElevationFormat) <> wefDepth. @name is ignored.
    property LandSurfaceDatum: double read FLandSurfaceDatum write
      SetLandSurfaceDatum;
    // @name represents the solution that is pumped into the porous medium.
    // If water is withdrawn, @name has no effect.
    property Solution: TIntegerPhastBoundaries read FSolution write SetSolution;
    // @name determines whether elevations in @link(Intervals) represent
    // true elevations or depths below a datum. (see @link(LandSurfaceDatum).)
    property WellElevationFormat: TWellElevationFormat read FWellElevationFormat
      write SetWellElevationFormat;
  end;

  

  { TODO : Replace TPhastScreenObject with a generic property of
    TScreenObject that can hold everything that is currently in
    TPhastScreenObject and can be replaced with something else for a different
    model. }

  {@abstract(@name is a @link(TScreenObject) that is specialized for use
    with PHAST.  It has properties that define boundary conditions in
    PHAST and it has methods to deal with PHAST-style interpolation.
    (See @link(TPhastInterpolationValues).))}

  TPhastScreenObject = class(TScreenObject)
  private
    // @name: TObjectList;
    FDataSetMixtureSubscriptions: TObjectList;
    // @name: @link(TFluxBoundary);
    // See @link(FluxBoundary).
    FFluxBoundary: TFluxBoundary;
    // @name: @link(TInterpValuesCollection);
    // See @link(InterpValues).
    FInterpValues: TInterpValuesCollection;
    // @name: @link(TLeakyBoundary);
    // See @link(LeakyBoundary).
    FLeakyBoundary: TLeakyBoundary;
    // @name: TRbwParser;
    // @name is assigned in @link(AssignValuesToDataSet).
    FMixtureCompiler: TRbwParser;
    // @name: TExpression;
    // @name is assigned in @link(AssignValuesToDataSet).
    FMixtureExpression: TExpression;
    // @name: TStringList;
    // @name is assigned and Freed in @link(AssignValuesToDataSet).
    FMixtureVariables: TStringList;
    // @name: @link(TRiverBoundary);
    // See @link(RiverBoundary).
    FRiverBoundary: TRiverBoundary;
    // @name: @link(TSpecifiedHeadBoundary);
    // See @link(SpecifiedHeadBoundary).
    FSpecifiedHeadBoundary: TSpecifiedHeadBoundary;
    // @name: @link(TSpecifiedSolutionBoundary);
    // See @link(SpecifiedSolutionBoundary).
    FSpecifiedSolutionBoundary: TSpecifiedSolutionBoundary;
    // @name: @link(TWellBoundary);
    // See @link(WellBoundary).
    FWellBoundary: TWellBoundary;
    // @name is used to assign data to a particular cell when PHAST-style
    // interpolation is used  and the data is integer data.
    // See @link(TPhastInterpolationValues).
    procedure AssignIntegerDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
    // @name is used to assign data to a particular cell when PHAST-style
    // interpolation is used  and the data is real-number data.
    // See @link(TPhastInterpolationValues).
    procedure AssignRealDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
    // See @link(MixtureDataSetFormula).
    function GetMixtureDataSetFormula(const Index: integer): string;
    // The purpose of @name is to (1) determine the formula for
    // a mixture to use with using PHAST-style
    // interpolation, (2) determine the TRbwParser to use with that formula,
    // and (3) compile that formula into a
    // TExpression.  See @link(TPhastInterpolationValues).
    // @param(Compiler is set in @name to the correct TRbwParser to use
    // with the mixture formula.)
    // @param(MixtureFormula is set in @name to the formula for the mixture.)
    // @param(Expression is set in @name to the TExpression that results
    // from compiling MixtureFormula.)
    // @param(DataSet is the @link(TDataArray) to which the mixture formula
    // applies.)
    // @param(OtherData if DataSet is a normal data set, OtherData is
    // a @link(TInterpValuesItem). if DataSet represents a boundary condition,
    // OtherData is a @link(TCustomPhastBoundaryCondition).)
    procedure InitializeMixtureExpression(out Compiler: TRbwParser;
      out MixtureFormula: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject);
    // @name calls TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.ResetMixtureSubscription)
    // for each solution boundary condition.
    procedure ResetBoundaryMixtureSubscriptions;
    // See @link(FluxBoundary).
    procedure SetFluxBoundary(const Value: TFluxBoundary);
    // See @link(InterpValues).
    procedure SetInterpValues(const Value: TInterpValuesCollection);
    // See @link(LeakyBoundary).
    procedure SetLeakyBoundary(const Value: TLeakyBoundary);
    // See @link(MixtureDataSetFormula).
    procedure SetMixtureDataSetFormula(const Index: integer;
      const Value: string);
    // See @link(RiverBoundary).
    procedure SetRiverBoundary(const Value: TRiverBoundary);
    // See @link(SpecifiedHeadBoundary).
    procedure SetSpecifiedHeadBoundary(
      const Value: TSpecifiedHeadBoundary);
    // See @link(SpecifiedSolutionBoundary).
    procedure SetSpecifiedSolutionBoundary(
      const Value: TSpecifiedSolutionBoundary);
    // See @link(WellBoundary).
    procedure SetWellBoundary(const Value: TWellBoundary);
    // @name determines whether @link(FluxBoundary) is stored.
    function StoreFlux: boolean;
    // @name determines whether @link(LeakyBoundary) is stored.
    function StoreLeaky: boolean;
    // @name determines whether @link(RiverBoundary) is stored.
    function StoreRiver: boolean;
    // @name determines whether @link(SpecifiedHeadBoundary) is stored.
    function StoreSpecifiedHead: boolean;
    // @name determines whether @link(WellBoundary) is stored.
    function StoreWell: boolean;
    // @name returns a formula that can be used to identify the type of
    // 3D boundary condition (if any) that is assigned with this @classname.
    function ThreeDBoundaryFormula: string;
    // @name returns a formula that can be used to identify the type of
    // 2D boundary condition (if any) that is assigned with this @classname.
    function TwoDBoundaryFormula: string;
  protected
    {@name assigns a value to a particular cell in DataSet.

      In @name:

      (1) If OtherData = nil
      or not @Link(TInterpValuesItem)(OtherData).Values.UsePHAST_Interpolation,
      the inherited AssignCellValue is called.

      (2) Otherwise,
      values are assigned to DataSet at the location
      LayerIndex, RowIndex, ColIndex using PHAST-style interpolation.
      (See @link(TPhastInterpolationValues).)
      Expression and Compiler are not used in this case and the variables
      listed in UsedVariables are not updated.

      OtherData is set in @Link(IsBoundaryTimeDataSetUsed).
    }
    procedure AssignCellValue(const UsedVariables: TStringList;
      const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer;
      const Compiler: TRbwParser; const Annotation: string;
      const Expression: TExpression; const OtherData: TObject); override;
    // @name returns an integer that indicates what type of boundary condition,
    // if any, are specified by this @classname.
    function BoundaryType: integer; override;
    // @name returns @true if DataSet is a @link(TSparseArrayPhastDataSet)
    // @name also returns @true if DataSet is a @link(TCustomPhastDataSet)
    // and DataSet is specified by this @classname. OtherData may be
    // changed DataSet is a @link(TCustomPhastDataSet).
    function DataSetUsed(const DataSet: TDataArray;
      var OtherData: TObject): boolean; override;
    // @name returns a string that indicates that a location
    // was specified by being enclosed in this @classname and how the value
    // at that location was determined.
    function EncloseAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; override;
    {@name (1) gets the proper DataSetFormula to apply to DataSet,
    (2) gets the proper TRbwParser for DataSet, and (3) compiles
     DataSetFormula to get Expression.  However, it doesn't need to do
     any of that is PHAST-style interpolation is used.
     See @link(TPhastInterpolationValues).}
    procedure InitializeExpression(out Compiler: TRbwParser;
      out DataSetFormula: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject); override;
    // @name returns a string that indicates that a location
    // was specified by being intersected by this @classname and how the value
    // at that location was determined.
    function IntersectAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; override;
    // @name returns @true and, if so, sets OtherData to the relevant
    // @link(TCustomPhastBoundaryCondition).
    function IsBoundaryTimeDataSetUsed(const DataSet: TDataArray;
      out OtherData: TObject): boolean; override;
    // @name is used to access the formula for the Mixture specified by
    // Index when PHAST style interpolation is used with a mixture.
    // See @link(TPhastInterpolationValues).
    property MixtureDataSetFormula[const Index: integer]: string
      read GetMixtureDataSetFormula write SetMixtureDataSetFormula;
    // The purpose of @name is to get First and Last.  They are
    // the indices of the first and last layer, row, or column
    // perpendicular to the plain of @link(TViewDirection) that are
    // enclosed or intersected by the @classname.
    //
    // In commented-out code, First and Last and changed to
    // frmGoPhast.PhastGrid.@link(TCustomGrid.LayerCount) for river
    // data sets.  It has been commented-out because it
    // messes up the display of the river
    // data on the status bar.
    //
    // It would be good to find a way around
    // this problem. As it is, all @name really
    // does is call the inherited @name.
    procedure OtherIndex(const LayerOrRow, RowOrColumn: integer;
      out First, Last: integer; const DataSet: TDataArray); override;
    function PhastBoundaryType: TBoundaryTypes;
    // @name sets UpToDate to @true for all members of
    // @link(FDataSetMixtureSubscriptions)
    procedure ResetMixtureSubscriptions;
    // @name calls inherited.  Then if Value is @True,
    // @name sets all the TObserver.@link(TObserver.UpToDate)
    // to @true for all @link(TObserver)s in
    // @link(FDataSetMixtureSubscriptions).
    procedure SetUpToDate(const Value: boolean); override;
    // @name calls inherited @link(TScreenObject.SetViewDirection)
    // and then sets the @link(TCustomOrientedPhastBoundary.Orientation)
    // of @link(FluxBoundary) and @link(LeakyBoundary).
    procedure SetViewDirection(const Value: TViewDirection); override;
    // @name calls TCustomPhastBoundary.@link(
    // TCustomPhastBoundary.UpdateMixtureExpression)
    // for @link(FluxBoundary), @link(LeakyBoundary),
    // @link(RiverBoundary), @link(SpecifiedHeadBoundary),
    // @link(SpecifiedSolutionBoundary), and @link(WellBoundary).
    procedure UpdateMixtureExpression;
  public
    // @name adds Data set to the list of @link(TDataArray) whose
    // values are specified by this @classname.
    // @name also does what it takes to ensure that
    // DataSet will be notified when there is a change to this
    // @classname.
    function AddDataSet(const DataSet: TDataArray): Integer; override;
    // @name copies @link(InterpValues) and the boundary conditions
    // from source and calls inherited @link(TScreenObject.Assign).
    procedure Assign(Source: TPersistent); override;
    // @name checks to make sure that it should affect DataSet.
    // If so, assigns values to locations within DataSet that it
    // should affect.
    procedure AssignValuesToDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray); override;
    // @name calls inherited @link(TScreenObject.ClearDataSets)
    // and clears @link(InterpValues).
    // @name also unsubscribes to everything in
    // @link(FDataSetMixtureSubscriptions) and clears
    // @link(FDataSetMixtureSubscriptions).
    procedure ClearDataSets; override;
    // @name deletes the @link(TDataArray) specified by Index from
    // the list of @link(TDataArray)s affected by this @classname.
    procedure DeleteDataSet(const Index: Integer); override;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name inserts DataSet into the list of data sets specified by
    // this @classname at the position specified by Index.
    procedure InsertDataSet(const Index: Integer;
      const DataSet: TDataArray); override;
    // @name returns the @link(TCustomPhastBoundaryCollection)
    // one of whose @link(TTimeList)s is TimeList.
    function GetBoundaryCollection(const TimeList: TTimeList):
      TCustomPhastBoundaryCollection;
    // @name notifies the things that depend on this @classname
    // that is has changed.
    procedure Invalidate; override;
    // @name calls @link(ResetMixtureSubscriptions) and
    // @link(ResetBoundaryMixtureSubscriptions)
    // as well as inherited @link(TScreenObject.ResetSubscriptions)
    procedure ResetSubscriptions; override;
    // @name returns @True if this @classname
    // affects a boundary condition.
    function BoundaryTypeUsed: TBoundaryTypes;
  published
    // @name represents the flux boundary (if any) in this @classname.
    property FluxBoundary: TFluxBoundary read FFluxBoundary write
      SetFluxBoundary stored StoreFlux;
    // @name represents the leaky boundary (if any) in this @classname.
    property LeakyBoundary: TLeakyBoundary read FLeakyBoundary write
      SetLeakyBoundary stored StoreLeaky;
    // @name represents the river boundary (if any) in this @classname.
    property RiverBoundary: TRiverBoundary read FRiverBoundary write
      SetRiverBoundary stored StoreRiver;
    // @name represents the specified head boundary (if any) in this @classname.
    property SpecifiedHeadBoundary: TSpecifiedHeadBoundary read
      FSpecifiedHeadBoundary write SetSpecifiedHeadBoundary
      stored StoreSpecifiedHead;
    // @name is retained only for backwards compatibility.
    property SpecifiedSolutionBoundary: TSpecifiedSolutionBoundary read
      FSpecifiedSolutionBoundary write SetSpecifiedSolutionBoundary stored
      False;
    // @name represents the well boundary (if any) in this @classname.
    property WellBoundary: TWellBoundary read FWellBoundary write
      SetWellBoundary stored StoreWell;
    // @name represents the PHAST style interpolation parameters
    // for the data sets affected by this @classname.
    // See @link(TPhastInterpolationValues).
    property InterpValues: TInterpValuesCollection read FInterpValues write
      SetInterpValues;
  end;

  {@abstract(@name is used to set or restore the properties of one or more
    @link(TPhastScreenObject)s.)}
  TUndoSetPhastScreenObjectProperties = class(TUndoSetScreenObjectProperties)
  private
    // @name: array of @link(TSolutionType);
    // The elements in @name specify what sort of solution occurs with each
    // specified head boundary (specified or associated) before being changed.
    // See @link(FNewHeadSolutionType).
    FOldHeadSolutionType: array of TSolutionType;
    // @name: TObjectList;
    // @name specifies the well intervals for each @link(TPhastScreenObject)
    // before being changed.
    FOldIntervalList: TObjectList;
    // @name: array of string;
    // @name specifies the well intervals prior to being changed.
    FOldLeakyHydraulicConductivity: array of string;
    // @name: TObjectList;
    // @name stores the leaky boundaries prior to being changed.
    FOldLeakyList: TObjectList;
    // @name: array of string;
    // @name stores the leaky thickness formulas prior to being changed.
    FOldLeakyThickness: array of string;
    // @name: TObjectList;
    // @name stores the PHAST-style interpolation parameters
    //  prior to being changed.
    // See @link(TPhastInterpolationValues).
    FOldPhastInterpolationList: TObjectList;
    // @name: array of string;
    // @name stores the river bed thickness formulas prior to being changed.
    FOldRiverBedThickness: array of string;
    // @name: array of string;
    // @name stores the river depth formulas prior to being changed.
    FOldRiverDepth: array of string;
    // @name: array of string;
    // @name stores the river descriptions prior to being changed.
    FOldRiverDescription: array of string;
    // @name: array of string;
    // @name stores the river bed hydraulic conductivity formulas
    // prior to being changed.
    FOldRiverHydraulicConductivity: array of string;
    // @name: TObjectList;
    // @name stores the river boundaries
    //  prior to being changed.
    FOldRiverList: TObjectList;
    // @name: array of string;
    // @name stores the river width formulas
    // prior to being changed.
    FOldRiverWidth: array of string;
    // @name: TObjectList;
    // @name stores the specified flux boundaries
    //  prior to being changed.
    FOldSpecifiedFluxList: TObjectList;
    // @name:: TObjectList;
    // @name stores the specified head boundaries
    //  prior to being changed.
    FOldSpecifiedHeadList: TObjectList;
    // @name: array of boolean;
    // @name stores the well
    // @link(TWellBoundary.AllocateByPressureAndMobility)
    // prior to being changed.
    FOldWellAllocation: array of boolean;
    // @name: array of string;
    // @name stores the well descriptions prior to being changed.
    FOldWellDescription: array of string;
    // @name: array of double;
    // @name stores the well diameter prior to being changed.
    FOldWellDiameter: array of double;
    // @name: array of @link(TWellElevationFormat);
    // @name stores the well elevation format prior to being changed.
    FOldWellIntervalFormat: array of TWellElevationFormat;
    // @name: array of double;
    // @name stores the well datum prior to being changed.
    FOldWellLandSurfaceDatum: array of double;
    // @name: TObjectList;
    // @name stores the well boundaries
    //  prior to being changed.
    FOldWellList: TObjectList;
  protected
    // @name calls TPhastScreenObject.@link(
    // TPhastScreenObject.ResetMixtureSubscriptions)
    // and TPhastScreenObject.@link(
    // TPhastScreenObject.ResetBoundaryMixtureSubscriptions) for each
    // @link(TPhastScreenObject).
    procedure ResetScreenObjectDataSetSubscriptions; override;
  public
    // @name: array[btSpecifiedHead..btWell] of boolean;
    // @name specifies whether each type of boundary has changed.
    FBoundaryChanged: array[btSpecifiedHead..btWell] of boolean;
    // @name: @link(TBoundaryTypes);
    // @name specifies the type of boundary that exists after
    // being changed.
    FNewBoundaryType: TBoundaryTypes;
    // @name: @link(TSolutionType);
    // @name specifies the sort of solution occurs with
    // specified head boundaries (specified or associated)
    // after being changed.
    // See @link(FOldHeadSolutionType).
    FNewHeadSolutionType: TSolutionType;
    // @name: @link(TLeakyBoundary);
    // @name specifies the leaky boundary condition of the
    // @link(TPhastScreenObject)s after being changed.
    FNewLeakyBoundary: TLeakyBoundary;
    // @name: string;
    // @name specifies the hydraulic conductivity formula for
    // a leaky boundary after being changed.
    FNewLeakyHydraulicConductivity: string;
    // @name: string;
    // @name specifies the thickness formula for
    // a leaky boundary after being changed.
    FNewLeakyThickness: string;
    // @name: TObjectList;
    // @name stores the PHAST-style interpolation parameters
    //  after being changed.
    // See @link(TPhastInterpolationValues).
    FNewPhastInterpolationList: TObjectList;
    // @name: string;
    // @name specifies the thickness formula for
    // a river boundary after being changed.
    FNewRiverBedThickness: string;
    // @name: @link(TRiverBoundary);
    // @name specifies the river boundary condition of the
    // @link(TPhastScreenObject)s after being changed.
    FNewRiverBoundary: TRiverBoundary;
    // @name: string;
    // @name specifies the depth formula for
    // a river boundary after being changed.
    FNewRiverDepth: string;
    // @name: string;
    FNewRiverDescription: string;
    // @name: string;
    // @name specifies the description for
    // a river boundary after being changed.
    FNewRiverHydraulicConductivity: string;
    // @name: string;
    // @name specifies the width formula for
    // a river boundary after being changed.
    FNewRiverWidth: string;
    // @name: @link(TFluxBoundary);
    // @name specifies the flux boundary of the
    // @link(TPhastScreenObject)s after being changed.
    FNewSpecifiedFluxBoundary: TFluxBoundary;
    // @name: @link(TSpecifiedHeadBoundary);
    // @name specifies the specified head boundary of the
    // @link(TPhastScreenObject)s after being changed.
    FNewSpecifiedHeadBoundary: TSpecifiedHeadBoundary;
    // @name: boolean;
    // @name specifies the well
    // @link(TWellBoundary.AllocateByPressureAndMobility)
    //  after being changed.
    FNewWellAllocation: boolean;
    // @name: @link(TWellBoundary);
    // @name specifies the WellBoundary of the
    // @link(TPhastScreenObject)s after being changed.
    FNewWellBoundary: TWellBoundary;
    // @name: string;
    // @name specifies the description for
    // a well boundary after being changed.
    FNewWellDescription: string;
    // @name: double;
    // @name specifies the diameter formula for
    // a well boundary after being changed.
    FNewWellDiameter: double;
    // @name: @link(TWellElevationFormat);
    // @name specifies the format for
    // a well boundary after being changed.
    FNewWellIntervalFormat: TWellElevationFormat;
    // @name: @link(TWellIntervals);
    // @name specifies the open intervals for
    // a well boundary after being changed.
    FNewWellIntervals: TWellIntervals;
    // @name: double;
    // @name specifies the land surface datum for
    // a well boundary after being changed.
    FNewWellLandSurfaceDatum: double;
    // @name: boolean;
    // @name indicates whether the specified head solution type
    // needs to be set.
    FSetHeadSolutionType: boolean;
    // @name: boolean;
    // @name indicates whether the leaky hydraulic conductivity formula
    // needs to be set.
    FSetLeakyHydraulicConductivity: boolean;
    // @name: boolean;
    // @name indicates whether the leaky thickness formula
    // needs to be set.
    FSetLeakyThickness: boolean;
    // @name: boolean;
    // @name indicates whether the river bed thickness formula
    // needs to be set.
    FSetRiverBedThickness: boolean;
    // @name: boolean;
    // @name indicates whether the river depth formula
    // needs to be set.
    FSetRiverDepth: boolean;
    // @name: boolean;
    // @name indicates whether the river description
    // needs to be set.
    FSetRiverDescription: boolean;
    // @name: boolean;
    // @name indicates whether the river hydraulic conductivity formula
    // needs to be set.
    FSetRiverHydraulicConductivity: boolean;
    // @name: boolean;
    // @name indicates whether the river width formula
    // needs to be set.
    FSetRiverWidth: boolean;
    // @name: boolean;
    // @name indicates whether the well
    // @link(TWellBoundary.AllocateByPressureAndMobility)
    // needs to be set.
    FSetWellAllocation: boolean;
    // @name: boolean;
    // @name indicates whether the well description
    // needs to be set.
    FSetWellDescription: boolean;
    // @name: boolean;
    // @name indicates whether the well diameter formula
    // needs to be set.
    FSetWellDiameter: boolean;
    // @name: boolean;
    // @name indicates whether the well interval format
    // needs to be set.
    FSetWellIntervalFormat: boolean;
    // @name: boolean;
    // @name indicates whether the well intervals
    // needs to be set.
    FSetWellIntervals: boolean;
    // @name: boolean;
    // @name indicates whether the well land surface datum
    // needs to be set.
    FSetWellLandSurfaceDatum: boolean;
    // @name creates an instance of @classname.
    // @param(AListOfScreenObjects contains the @link(TPhastScreenObject)s
    // that will be affected by this @classname.)
    constructor Create(const AListOfScreenObjects: TList);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name changes the properties of the @link(TPhastScreenObject)s.
    procedure DoCommand; override;
    // @name reversed the change in the properties of the
    // @link(TPhastScreenObject)s.
    procedure Undo; override;
  end;

  {@abstract(@name is a @link(TPhastScreenObject) that can store a
    series of integer or real number values associated with a data set.
    @name is used when importing a zone from an existing PHAST model.)}
  TMultiValueScreenObject = class(TPhastScreenObject)
  private
    // @name: @link(TIntegerDataListCollection);
    // See @link(IntegerValues).
    FIntegerValues: TIntegerDataListCollection;
    // @name: @link(TRealDataListCollection);
    // See @link(RealValues).
    FRealValues: TRealDataListCollection;
    // See @link(IntegerValues).
    procedure SetIntegerValues(const Value: TIntegerDataListCollection);
    // See @link(RealValues).
    procedure SetRealValues(const Value: TRealDataListCollection);
  public
   // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name stores integer values associated with one or more data sets.
    property IntegerValues: TIntegerDataListCollection read FIntegerValues write
      SetIntegerValues;
    // @name stores real number values associated with one or more data sets.
    property RealValues: TRealDataListCollection read FRealValues write
      SetRealValues;
  end;

  {@abstract(@name is used in reading a @link(TPhastScreenObject)
    from or writing it to a file.)}
  TPhastScreenObjectItem = class(TScreenObjectItem)
  private
    // name: TStrings.
    // @name stores the formulas for mixtures in the @link(TPhastScreenObject).
    FMixtureFormulas: TStrings;
    // @name fills @link(FMixtureFormulas) with the formulas for mixtures
    // in the @link(TPhastScreenObject) and then
    // returns @link(FMixtureFormulas).
    function GetMixtureFormulas: TStrings;
  protected
    // After a @link(TPhastScreenObject) has been read,
    // @name is called to update its formulas for mixtures.
    procedure UpdateScreenObject; override;
  public
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name is used for reading and storing formulas for mixtures.
    // When it is being read, the value is read from the associated
    // @link(TPhastScreenObject).  When it is being written,
    // it is stored in the private field (FMixtureFormulas) and is
    // later transfered to the @link(TPhastScreenObject) in the protected
    // method @link(UpdateScreenObject).
    property MixtureFormulas: TStrings read GetMixtureFormulas write
      FMixtureFormulas;
  end;

  {@abstract(@name is used to reading a series of @link(TPhastScreenObject)s
    from or writing them to a file.)}
  TPhastScreenObjectCollection = class(TCustomScreenObjectCollection)
  public
    // @name creates an instance of @classname.
    constructor Create;
  end;

implementation

uses GIS_Functions, frmGoPhastUnit, PhastModelUnit,
  frmFormulaErrorsUnit, frmShowHideObjectsUnit;

{ TPhastScreenObject }

function TPhastScreenObject.AddDataSet(const DataSet: TDataArray): Integer;
var
  Item: TInterpValuesItem;
  Subscription: TObserver;
begin
  result := IndexOfDataSet(DataSet);
  if (result < 0) and CanAddDataSet(DataSet) then
  begin
    Subscription := TObserver.Create(nil);
    Subscription.UpdateWithName(DataSet.Name + Name + 'Mixture');
    FDataSetMixtureSubscriptions.Add(Subscription);

    result := inherited AddDataSet(DataSet);

    Subscription.Subscribe(DataSet);
    self.Subscribe(Subscription);

    if FIsUpdating then
    begin
      if DataSet is TCustomPhastDataSet then
      begin
        Item := FInterpValues.Items[result] as TInterpValuesItem;
        Item.Values.DataSet := TCustomPhastDataSet(DataSet);
        Item.Values.MixtureFormula := FloatToStr(0.5);
      end;
    end
    else
    begin
      Item := FInterpValues.Add as TInterpValuesItem;
      if DataSet is TCustomPhastDataSet then
      begin
        Item.Values.Assign(DataSet);
      end;
    end;
  end;
end;

procedure TPhastScreenObject.Assign(Source: TPersistent);
var
  SourceScreenObject: TPhastScreenObject;
begin
  inherited;
  if Source is TPhastScreenObject then
  begin
    SourceScreenObject := TPhastScreenObject(Source);
    InterpValues := SourceScreenObject.InterpValues;
    FluxBoundary := SourceScreenObject.FluxBoundary;
    LeakyBoundary := SourceScreenObject.LeakyBoundary;
    RiverBoundary := SourceScreenObject.RiverBoundary;
    SpecifiedHeadBoundary := SourceScreenObject.SpecifiedHeadBoundary;
    SpecifiedSolutionBoundary := SourceScreenObject.SpecifiedSolutionBoundary;
    WellBoundary := SourceScreenObject.WellBoundary;
  end;
end;

function TPhastScreenObject.DataSetUsed(const DataSet: TDataArray;
  var OtherData: TObject): boolean;
var
  DataSetIndex: integer;
  CouldBeBoundary: boolean;
begin
  if DataSet is TSparseArrayPhastDataSet then
  begin
    result := True;
  end
  else if DataSet is TCustomPhastDataSet then
  begin
    OtherData := nil;
    CouldBeBoundary := False;
    DataSetIndex := IndexOfDataSet(DataSet);
    if DataSetIndex < 0 then
    begin
      CouldBeBoundary := True;
      DataSetIndex := IndexOfBoundaryDataSet(DataSet);
    end;
    result := DataSetIndex >= 0;
    if result and not CouldBeBoundary then
    begin
      OtherData := FInterpValues.Items[DataSetIndex] as TInterpValuesItem;
    end;
  end
  else
  begin
    result := inherited DataSetUsed(DataSet, OtherData);
    if result then
    begin
      if (DataSet = frmGoPhast.PhastModel.TopBoundaryType)
        or (DataSet = frmGoPhast.PhastModel.FrontBoundaryType)
        or (DataSet = frmGoPhast.PhastModel.SideBoundaryType) then
      begin
        result := PhastBoundaryType in [btSpecifiedHead, btFlux, btLeaky]
      end
      else if (DataSet = frmGoPhast.PhastModel.Top2DBoundaryType) then
      begin
        result := PhastBoundaryType in [btRiver, btWell];
      end;
    end;
  end;
end;

procedure TPhastScreenObject.AssignRealDataWithPhastInterpolation(const DataSet: TDataArray;
  const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
  const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
begin
  if DataSet is TRealPhastDataSet then
  begin
    with TRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else if DataSet is TSparseRealPhastDataSet then
  begin
    with TSparseRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else
  begin
    Assert(False);
  end;

  Distance := 0;
  case InterpValue.Values.InterpolationDirection of
    pidX:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.ColumnPosition[ColIndex]
                + frmGoPhast.PhastGrid.ColumnPosition[ColIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.ColumnPosition[ColIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidY:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.RowPosition[RowIndex]
                + frmGoPhast.PhastGrid.RowPosition[RowIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.RowPosition[RowIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidZ:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.LayerElevation[LayerIndex]
                + frmGoPhast.PhastGrid.LayerElevation[LayerIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.LayerElevation[LayerIndex];
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue2;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      Fraction * InterpValue.Values.RealValue1 + (1 - Fraction) *
      InterpValue.Values.RealValue2;
  end;
end;

{ TODO :
See if parts of AssignIntegerDataWithPhastInterpolation can be combined
with parts of AssignRealDataWithPhastInterpolation }

procedure TPhastScreenObject.AssignIntegerDataWithPhastInterpolation(const DataSet: TDataArray;
  const LayerIndex, RowIndex, ColIndex: integer;
  const Comment: string; const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
  RealValue: double;
begin
  if DataSet is TIntegerPhastDataSet then
  begin
    with TIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    with TSparseIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else
  begin
    Assert(false);
  end;

  Distance := 0;
  case InterpValue.Values.InterpolationDirection of
    pidX:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.ColumnPosition[ColIndex]
                + frmGoPhast.PhastGrid.ColumnPosition[ColIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.ColumnPosition[ColIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidY:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.RowPosition[RowIndex]
                + frmGoPhast.PhastGrid.RowPosition[RowIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.RowPosition[RowIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidZ:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := (frmGoPhast.PhastGrid.LayerElevation[LayerIndex]
                + frmGoPhast.PhastGrid.LayerElevation[LayerIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := frmGoPhast.PhastGrid.LayerElevation[LayerIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidMix:
      begin
        UpdateVariables(FMixtureVariables, DataSet,
          LayerIndex, RowIndex, ColIndex, FMixtureCompiler);

        FMixtureExpression.Evaluate;

        Distance := 1 - FMixtureExpression.DoubleResult;
      end;
  else
    Assert(False);
  end;
  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue1;
    Fraction := 1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue2;
    Fraction := 0;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    RealValue := Fraction * InterpValue.Values.IntValue1 + (1 - Fraction) *
      InterpValue.Values.IntValue2;
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      Round(RealValue);
  end;
  // Fraction is needed in all cases in order to read
  // TSparseIntegerPhastDataSet.RealValue
  // or TIntegerPhastDataSet.RealValue
  if DataSet is TIntegerPhastDataSet then
  begin
    TIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    TSparseIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction
  end;
end;

procedure TPhastScreenObject.ClearDataSets;
var
  Index: integer;
  Subscription: TObserver;
  ADataSet: TDataArray;
begin
//  FInterpValues.Clear;

  Assert(FDataSetMixtureSubscriptions.Count = DataSetCount);
  for Index := 0 to FDataSetMixtureSubscriptions.Count -1 do
  begin
    MixtureDataSetFormula[Index] := '0';
    ADataSet := DataSets[Index];
    Subscription := FDataSetMixtureSubscriptions[Index] as TObserver;
    self.Unsubscribe(Subscription);
    Subscription.Unsubscribe(ADataSet);
  end;
  FInterpValues.Clear;

  FDataSetMixtureSubscriptions.Clear;
  inherited;
end;

constructor TPhastScreenObject.Create(AnOwner: TComponent);
begin
  inherited;
  FDataSetMixtureSubscriptions := TObjectList.Create;

  FInterpValues := TInterpValuesCollection.Create;

  FFluxBoundary := TFluxBoundary.Create;
  FLeakyBoundary := TLeakyBoundary.Create;
  FRiverBoundary := TRiverBoundary.Create;
  FSpecifiedHeadBoundary := TSpecifiedHeadBoundary.Create;
  FSpecifiedSolutionBoundary := TSpecifiedSolutionBoundary.Create;
  FWellBoundary := TWellBoundary.Create;

  FFluxBoundary.ScreenObject := self;
  FLeakyBoundary.ScreenObject := self;
  FRiverBoundary.ScreenObject := self;
  FSpecifiedHeadBoundary.ScreenObject := self;
  FSpecifiedSolutionBoundary.ScreenObject := self;
  FWellBoundary.ScreenObject := self;
end;

procedure TPhastScreenObject.DeleteDataSet(const Index: Integer);
var
  Subscription: TObserver;
  DataSet: TDataArray;
begin
  // Get rid of any subscriptions due to the formula.
  MixtureDataSetFormula[Index] := '0';

  Subscription := FDataSetMixtureSubscriptions[Index] as TObserver;
  DataSet := DataSets[Index];
  self.Unsubscribe(Subscription);
  Subscription.Unsubscribe(DataSet);
  FDataSetMixtureSubscriptions.Delete(Index);
  inherited;
  FInterpValues.Delete(Index);
end;

destructor TPhastScreenObject.Destroy;
begin
  FInterpValues.Free;
  FFluxBoundary.Free;
  FLeakyBoundary.Free;
  FRiverBoundary.Free;
  FSpecifiedHeadBoundary.Free;
  FSpecifiedSolutionBoundary.Free;
  FWellBoundary.Free;
  inherited;
  FDataSetMixtureSubscriptions.Free;
end;

procedure TPhastScreenObject.InsertDataSet(const Index: Integer;
  const DataSet: TDataArray);
var
  Item: TInterpValuesItem;
begin
  if IndexOfDataSet(DataSet) < 0 then
  begin
    Item := FInterpValues.Insert(Index) as TInterpValuesItem;
    if DataSet is TCustomPhastDataSet then
    begin
      Item.Values.Assign(DataSet);
    end;
  end;
  inherited;
end;

procedure TPhastScreenObject.SetInterpValues(
  const Value: TInterpValuesCollection);
begin
  FInterpValues.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.InitializeExpression(out Compiler: TRbwParser;
  out DataSetFormula: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
var
  InterpValue: TInterpValuesItem;
  LocalExpression: string;
  ResultTypeOK: boolean;
begin
  if (DataSet = frmGoPhast.PhastModel.TopBoundaryType)
    or (DataSet = frmGoPhast.PhastModel.FrontBoundaryType)
    or (DataSet = frmGoPhast.PhastModel.SideBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := ThreeDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  if (DataSet = frmGoPhast.PhastModel.Top2DBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := TwoDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) then
  begin
    inherited;
  end
  else if InterpValue is TCustomPhastBoundaryCondition then
  begin
    if TCustomPhastBoundaryCondition(InterpValue).UsePHAST_Interpolation then
    begin
      Compiler := nil;
      Expression := nil;
      DataSetFormula := '';
    end
    else
    begin
      Compiler := GetCompiler(DataSet.Orientation);
      try
        LocalExpression := TCustomPhastBoundaryCondition(InterpValue).Expression;
        Compiler.Compile(LocalExpression);
      except on E: ERbwParserError do
        begin
          LocalExpression := '0';
          TCustomPhastBoundaryCondition(InterpValue).Expression := LocalExpression;
          Compiler.Compile(LocalExpression);
        end;
      end;
      DataSetFormula := LocalExpression;
      Expression := Compiler.CurrentExpression;
      ResultTypeOK := (Expression.ResultType = DataSet.Datatype)
        or ((Expression.ResultType = rdtInteger) and (DataSet.Datatype =
        rdtDouble));
      if not ResultTypeOK then
      begin
        raise EInvalidDataType.Create('Invalid data type.');
      end;
    end;
  end
  else if not InterpValue.Values.UsePHAST_Interpolation then
  begin
    inherited;
  end
  else
  begin
    Compiler := nil;
    Expression := nil;
    DataSetFormula := '';
  end;
end;

procedure TPhastScreenObject.AssignCellValue(const UsedVariables: TStringList;
  const DataSet: TDataArray; const LayerIndex, RowIndex,
  ColIndex: integer; const Compiler: TRbwParser; const Annotation: string;
  const Expression: TExpression; const OtherData: TObject);
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    inherited;
    if (InterpValue <> nil) and (DataSet is TCustomPhastDataSet) then
    begin
      TCustomPhastDataSet(DataSet).IsInterpolatedCell[
        LayerIndex, RowIndex, ColIndex] := False;
    end
  end
  else
  begin
    UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex, DataSet.EvaluatedAt);

    case DataSet.Datatype of
      rdtDouble:
        begin
          AssignRealDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtInteger:
        begin
          AssignIntegerDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtBoolean:
        begin
          Assert(False);
        end;
      rdtString:
        begin
          Assert(False);
        end;
    else
      Assert(False);
    end;
  end;
end;

function TPhastScreenObject.EncloseAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    result := inherited EncloseAnnotation(DataSetFormula, OtherData);
  end
  else
  begin
    result := 'Enclosed by ' + Name
      + ': Set by PHAST-style interpolation';
  end;
end;

function TPhastScreenObject.IntersectAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    result := inherited IntersectAnnotation(DataSetFormula, OtherData);
  end
  else
  begin
    result := 'Intersected by ' + Name
      + ': Set by PHAST-style interpolation';
  end;
end;

procedure TPhastScreenObject.SetFluxBoundary(const Value: TFluxBoundary);
begin
  FFluxBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.SetLeakyBoundary(const Value: TLeakyBoundary);
begin
  FLeakyBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.SetRiverBoundary(const Value: TRiverBoundary);
begin
  FRiverBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.SetSpecifiedSolutionBoundary(
  const Value: TSpecifiedSolutionBoundary);
begin
  FSpecifiedSolutionBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.SetWellBoundary(const Value: TWellBoundary);
begin
  FWellBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TPhastScreenObject.SetSpecifiedHeadBoundary(
  const Value: TSpecifiedHeadBoundary);
begin
  FSpecifiedHeadBoundary.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

function TPhastScreenObject.GetBoundaryCollection(
  const TimeList: TTimeList): TCustomPhastBoundaryCollection;
var
  List: TList;
  Index: integer;
  Boundary: TCustomPhastBoundaryCollection;
begin
  result := nil;
  List := TList.Create;
  try
    List.Add(FluxBoundary.Flux);
    List.Add(FluxBoundary.AssociatedSolution);
    List.Add(LeakyBoundary.Head);
    List.Add(LeakyBoundary.AssociatedSolution);
    List.Add(RiverBoundary.Head);
    List.Add(RiverBoundary.AssociatedSolution);
    List.Add(SpecifiedHeadBoundary.Head);
    List.Add(SpecifiedHeadBoundary.AssociatedSolution);
    List.Add(SpecifiedSolutionBoundary.Solution);
    List.Add(WellBoundary.InjectionOrPumpingRate);
    List.Add(WellBoundary.Solution);
    for Index := 0 to List.Count - 1 do
    begin
      Boundary := List[Index];
      if Boundary.TimeList = TimeList then
      begin
        result := Boundary;
        Exit;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TPhastScreenObject.ResetBoundaryMixtureSubscriptions;
var
  Index: integer;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  //  TimeIndex: integer;
  List: TList;
  BoundaryIndex: integer;
  BoundaryCollection: TCustomPhastBoundaryCollection;
begin
  List := TList.Create;
  try
    List.Add(FluxBoundary.AssociatedSolution);
    List.Add(LeakyBoundary.AssociatedSolution);
    List.Add(RiverBoundary.AssociatedSolution);
    List.Add(SpecifiedHeadBoundary.AssociatedSolution);
    List.Add(SpecifiedSolutionBoundary.Solution);
    List.Add(WellBoundary.Solution);
    for BoundaryIndex := 0 to List.Count - 1 do
    begin
      BoundaryCollection := List[BoundaryIndex];
      for Index := 0 to BoundaryCollection.Count - 1 do
      begin
        BoundaryCondition := BoundaryCollection.Items[Index] as
          TCustomPhastBoundaryCondition;
        BoundaryCondition.ResetMixtureSubscription;
      end;
    end;
  finally
    List.Free;
  end;
end;

function TPhastScreenObject.IsBoundaryTimeDataSetUsed(
  const DataSet: TDataArray; out OtherData: TObject): boolean;
var
  Index: integer;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  TimeIndex: integer;
  List: TList;
  BoundaryIndex: integer;
  BoundaryCollection: TCustomPhastBoundaryCollection;
begin
  result := False;
  if DataSet = nil then
    Exit;
  if DataSet.DataType in [rdtBoolean, rdtString] then
    Exit;
  List := TList.Create;
  try
    case DataSet.DataType of
      rdtDouble:
        begin
          List.Add(FluxBoundary.Flux);
          List.Add(LeakyBoundary.Head);
          List.Add(RiverBoundary.Head);
          List.Add(SpecifiedHeadBoundary.Head);
          List.Add(WellBoundary.InjectionOrPumpingRate);
        end;
      rdtInteger:
        begin
          List.Add(FluxBoundary.AssociatedSolution);
          List.Add(LeakyBoundary.AssociatedSolution);
          List.Add(RiverBoundary.AssociatedSolution);
          List.Add(SpecifiedHeadBoundary.AssociatedSolution);
          List.Add(WellBoundary.Solution);
        end;
    else
      Assert(False);
    end;

    for BoundaryIndex := 0 to List.Count - 1 do
    begin
      BoundaryCollection := List[BoundaryIndex];
      for Index := 0 to BoundaryCollection.Count - 1 do
      begin
        BoundaryCondition := BoundaryCollection.Items[Index] as
          TCustomPhastBoundaryCondition;
        TimeIndex :=
          BoundaryCollection.TimeList.IndexOf(BoundaryCondition.Time);
        if TimeIndex >= 0 then
        begin
          if BoundaryCollection.TimeList.Items[TimeIndex] = DataSet then
          begin
            result := True;
            OtherData := BoundaryCondition;
            Exit;
          end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TPhastScreenObject.Invalidate;
var
  DataSet: TDataArray;
  Index: integer;
begin
  inherited;
  if FluxBoundary = nil then
    Exit;
  if FluxBoundary.Flux.Count > 0 then
  begin
    FluxBoundary.Flux.TimeList.Invalidate
  end;
  if FluxBoundary.AssociatedSolution.Count > 0 then
  begin
    FluxBoundary.AssociatedSolution.TimeList.Invalidate
  end;
  if LeakyBoundary.Head.Count > 0 then
  begin
    LeakyBoundary.Head.TimeList.Invalidate;
  end;
  if LeakyBoundary.AssociatedSolution.Count > 0 then
  begin
    LeakyBoundary.AssociatedSolution.TimeList.Invalidate;
  end;
  if (LeakyBoundary.Head.Count > 0) or (LeakyBoundary.AssociatedSolution.Count >
    0) then
  begin
    Index := -1;
    case ViewDirection of
      vdTop:
        begin
          Index :=
            frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsTopLeakyHydraulicConductivity);
        end;
      vdFront:
        begin
          Index :=
            frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsFrontLeakyHydraulicConductivity);
        end;
      vdSide:
        begin
          Index :=
            frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsSideLeakyHydraulicConductivity);
        end;
    else
      Assert(False);
    end;
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
    case ViewDirection of
      vdTop:
        begin
          Index := frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsTopLeakyThickness);
        end;
      vdFront:
        begin
          Index :=
            frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsFrontLeakyThickness);
        end;
      vdSide:
        begin
          Index :=
            frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsSideLeakyThickness);
        end;
    else
      Assert(False);
    end;
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
  end;

  if RiverBoundary.Head.Count > 0 then
  begin
    RiverBoundary.Head.TimeList.Invalidate;
  end;
  if RiverBoundary.AssociatedSolution.Count > 0 then
  begin
    RiverBoundary.AssociatedSolution.TimeList.Invalidate;
  end;
  if (RiverBoundary.Head.Count > 0) or (RiverBoundary.AssociatedSolution.Count >
    0) then
  begin
    Index :=
      frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsRiverHydraulicConductivity);
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
    Index := frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsRiverWidth);
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
    Index := frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsRiverDepth);
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
    Index := frmGoPhast.PhastModel.IndexOfBoundaryDataSet(rsRiverBedThickness);
    if Index >= 0 then
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
      DataSet.Invalidate;
    end;
  end;

  if SpecifiedHeadBoundary.Head.Count > 0 then
  begin
    SpecifiedHeadBoundary.Head.TimeList.Invalidate;
  end;
  if SpecifiedHeadBoundary.AssociatedSolution.Count > 0 then
  begin
    SpecifiedHeadBoundary.AssociatedSolution.TimeList.Invalidate;
  end;
  {if SpecifiedSolutionBoundary.Solution.Count > 0 then
  begin
    SpecifiedSolutionBoundary.Solution.TimeList.Invalidate;
  end;}
  if WellBoundary.InjectionOrPumpingRate.Count > 0 then
  begin
    WellBoundary.InjectionOrPumpingRate.TimeList.Invalidate
  end;
  if WellBoundary.Solution.Count > 0 then
  begin
    WellBoundary.Solution.TimeList.Invalidate;
  end;
end;

procedure TPhastScreenObject.SetViewDirection(const Value: TViewDirection);
begin
  inherited;
  FluxBoundary.Orientation := Value;
  LeakyBoundary.Orientation := Value;
end;

function TPhastScreenObject.StoreFlux: boolean;
begin
  result := (FluxBoundary.Flux.Count > 0)
    or (FluxBoundary.AssociatedSolution.Count > 0);
end;

function TPhastScreenObject.StoreLeaky: boolean;
begin
  result := (LeakyBoundary.Head.Count > 0)
    or (LeakyBoundary.AssociatedSolution.Count > 0);
end;

function TPhastScreenObject.StoreRiver: boolean;
begin
  result := (RiverBoundary.Head.Count > 0)
    or (RiverBoundary.AssociatedSolution.Count > 0);
end;

function TPhastScreenObject.StoreSpecifiedHead: boolean;
begin
  result := (SpecifiedHeadBoundary.Head.Count > 0)
    or (SpecifiedHeadBoundary.AssociatedSolution.Count > 0);
end;

{function TPhastScreenObject.StoreSpecifiedSolution: boolean;
begin
  result := (SpecifiedSolutionBoundary.Solution.Count > 0);
end;  }

function TPhastScreenObject.StoreWell: boolean;
begin
  result := (WellBoundary.InjectionOrPumpingRate.Count > 0)
    or (WellBoundary.Solution.Count > 0);
end;

function TPhastScreenObject.ThreeDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btSpecifiedHead, btFlux, btLeaky] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

function TPhastScreenObject.TwoDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btRiver, btWell] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

procedure ResetScreenObjectMixtureFunction(const DataSetIndex: integer;
  const AScreenObject: TPhastScreenObject; const Compiler: TRbwParser;
  const DataSetDataType: TRbwDataType; const ErrorMessage: string;
  const IsBoundary: boolean);
var
  ScreenObjectFunction: string;
begin
  if IsBoundary then
  begin
    Assert(False);
  end
  else
  begin
    ScreenObjectFunction :=
      AScreenObject.MixtureDataSetFormula[DataSetIndex];
    frmFormulaErrors.AddError(AScreenObject.Name,
      AScreenObject.DataSets[DataSetIndex].Name,
      ScreenObjectFunction, ErrorMessage);
  end;

  Assert(DataSetDataType = rdtInteger);
  ScreenObjectFunction := FloatToStr(0.5);
  AScreenObject.MixtureDataSetFormula[DataSetIndex] := ScreenObjectFunction;

  Compiler.Compile(ScreenObjectFunction);
end;

procedure TPhastScreenObject.InitializeMixtureExpression(out Compiler:
  TRbwParser; out MixtureFormula: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
var
  ResultTypeOK: boolean;
  DI: integer;
  IsBoundary: boolean;
  InterpValue: TInterpValuesItem;
  BoundaryCondition: TCustomPhastBoundaryCondition;
begin
  Compiler := GetCompiler(DataSet.Orientation);
  DI := IndexOfDataSet(DataSet);
  if DI >= 0 then
  begin
    InterpValue := OtherData as TInterpValuesItem;
    MixtureFormula := InterpValue.Values.MixtureFormula;
    IsBoundary := False;
  end
  else
  begin
    IsBoundary := True;
    BoundaryCondition := OtherData as TCustomPhastBoundaryCondition;
    MixtureFormula := BoundaryCondition.MixtureExpression;
    BoundaryCondition.FMixtureObserver.UpToDate := True;
  end;

  try
    Compiler.Compile(MixtureFormula);
  except on E: ERbwParserError do
    begin
      ResetScreenObjectMixtureFunction(DI, self, Compiler,
        DataSet.DataType, E.Message, IsBoundary);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
  if not ResultTypeOK then
  begin
    raise EInvalidDataType.Create('Invalid data type.');
  end;
end;

procedure TPhastScreenObject.AssignValuesToDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray);
var
  OtherData: TObject;
  InterpValue: TInterpValuesItem;
  Compiler: TRbwParser;
  MixtureFormula: string;
  Expression: TExpression;
  UsedVariables: TStringList;
  BoundaryDataSet: boolean;
begin
  OtherData := nil;

  // The boundary data sets should contain an integer value
  // that represents what type of boundary is being assiged.
  BoundaryDataSet := (DataSet = frmGoPhast.PhastModel.TopBoundaryType)
    or (DataSet = frmGoPhast.PhastModel.FrontBoundaryType)
    or (DataSet = frmGoPhast.PhastModel.SideBoundaryType)
    or (DataSet = frmGoPhast.PhastModel.Top2DBoundaryType);

  // if the screen object is deleted, or the data set is not used, quit.
  if Deleted or (DataSet = nil) or
    ((IndexOfDataSet(DataSet) < 0) and (IndexOfBoundaryDataSet(DataSet) < 0)
    and not IsBoundaryTimeDataSetUsed(DataSet, OtherData)
    and not BoundaryDataSet) then
  begin
    Exit;
  end;
  if BoundaryDataSet and (BoundaryType = 0) then
  begin
    Exit;
  end;
  if not DataSetUsed(DataSet, OtherData) then
    Exit;

  FMixtureCompiler := nil;
  FMixtureExpression := nil;
  UsedVariables := nil;
  try
    if (OtherData <> nil) then
    begin
      InterpValue := OtherData as TInterpValuesItem;
      if InterpValue.Values.UsePHAST_Interpolation
        and (InterpValue.Values.InterpolationDirection = pidMix) then
      begin
        InitializeMixtureExpression(Compiler, MixtureFormula,
          Expression, DataSet, OtherData);

        UsedVariables := TStringList.Create;
        InitializeVariables(UsedVariables, DataSet, Expression, Compiler);

        UpdateCurrentScreenObject(self);
        FMixtureCompiler := Compiler;
        FMixtureExpression := Expression;
      end;
    end;
    FMixtureVariables := UsedVariables;
    inherited;
  finally
    UsedVariables.Free;
  end;
end;

function TPhastScreenObject.GetMixtureDataSetFormula(
  const Index: integer): string;
var
  Item: TInterpValuesItem;
begin
  if Index < InterpValues.Count then
  begin
    Item := InterpValues.Items[Index] as TInterpValuesItem;
    result := Item.Values.MixtureFormula;
  end
  else
  begin
    result := ''
  end;
end;

procedure TPhastScreenObject.SetMixtureDataSetFormula(const Index: integer;
  const Value: string);
var
  Observer: TObserver;
  OldUseList: TStringList;
  NewUseList: TStringList;
  UseIndex: integer;
  OtherIndex: integer;
  AFunction, OldFunction: string;
  Compiler: TRbwParser;
  ADataSet: TDataArray;
  DS: TDataArray;
  Item: TInterpValuesItem;
begin
  Item := InterpValues.Items[Index] as TInterpValuesItem;
  AFunction := Item.Values.MixtureFormula;
  Observer := nil;
  if AFunction <> Value then
  begin
    frmGoPhast.InvalidateModel;
    OldFunction := AFunction;
    try
      ADataSet := DataSets[Index];
      Observer := FDataSetMixtureSubscriptions[Index] as TObserver;
      OldUseList := TStringList.Create;
      NewUseList := TStringList.Create;
      try
        Compiler := frmGoPhast.GetCompiler(ADataSet.Orientation,
          ADataSet.EvaluatedAt);
        if AFunction = '' then
        begin
          AFunction := FloatToStr(0.5);
        end;
        try
          Compiler.Compile(AFunction);
          OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
        except on E: ERbwParserError do
            OldUseList.Clear;
        end;

        AFunction := Value;
        Compiler.Compile(AFunction);
        NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);

        Item.Values.MixtureFormula := Value;
        for UseIndex := OldUseList.Count - 1 downto 0 do
        begin
          OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
          if OtherIndex >= 0 then
          begin
            OldUseList.Delete(UseIndex);
            NewUseList.Delete(OtherIndex);
          end;
        end;
        for UseIndex := 0 to OldUseList.Count - 1 do
        begin
          DS := frmGoPhast.PhastModel.GetDataSetByName(OldUseList[UseIndex]);
          Assert(DS <> nil);
          DS.Unsubscribe(Observer);
        end;
        for UseIndex := 0 to NewUseList.Count - 1 do
        begin
          DS := frmGoPhast.PhastModel.GetDataSetByName(NewUseList[UseIndex]);
          Assert(DS <> nil);
          DS.Subscribe(Observer);
        end;
        Invalidate;
      finally
        OldUseList.Free;
        NewUseList.Free;
      end;

      Observer.UpToDate := True;
      Observer.UpToDate := False;
      DataSets[Index].Invalidate;
      Observer.UpToDate := True;
    finally
      if Observer.IsRecursive then
      begin
        Item.Values.MixtureFormula := OldFunction;
      end;
    end;
  end;
end;

procedure TPhastScreenObject.ResetMixtureSubscriptions;
var
  Index: integer;
  Subscription: TObserver;
begin
  for Index := 0 to FDataSetMixtureSubscriptions.Count - 1 do
  begin
    Subscription := FDataSetMixtureSubscriptions[Index] as TObserver;
    Subscription.UpToDate := True;
  end;
end;

procedure TPhastScreenObject.ResetSubscriptions;
begin
  inherited;
  ResetMixtureSubscriptions;
  ResetBoundaryMixtureSubscriptions;
end;

procedure TPhastScreenObject.UpdateMixtureExpression;
begin
  FluxBoundary.UpdateMixtureExpression;
  LeakyBoundary.UpdateMixtureExpression;
  RiverBoundary.UpdateMixtureExpression;
  SpecifiedHeadBoundary.UpdateMixtureExpression;
  SpecifiedSolutionBoundary.UpdateMixtureExpression;
  WellBoundary.UpdateMixtureExpression;
end;

function TPhastScreenObject.PhastBoundaryType: TBoundaryTypes;
begin
  result := btNone;
  if (SpecifiedHeadBoundary.Head.Count > 0)
    or (SpecifiedHeadBoundary.AssociatedSolution.Count > 0) then
  begin
    result := btSpecifiedHead;
  end;

  if (FluxBoundary.Flux.Count > 0)
    or (FluxBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btFlux;
  end;

  if (LeakyBoundary.Head.Count > 0)
    or (LeakyBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btLeaky;
  end;

  if (RiverBoundary.Head.Count > 0)
    or (RiverBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btRiver;
  end;

  if (WellBoundary.InjectionOrPumpingRate.Count > 0)
    or (WellBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btWell;
  end;
end;

function TPhastScreenObject.BoundaryType: integer;
begin
  result := Ord(PhastBoundaryType);
end;

function TPhastScreenObject.BoundaryTypeUsed: TBoundaryTypes;
begin
  result := btNone;
  if (FluxBoundary.Flux.Count > 0)
    or (FluxBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btFlux;
  end;

  if (LeakyBoundary.Head.Count > 0)
    or (LeakyBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btLeaky;
  end;

  if (RiverBoundary.Head.Count > 0)
    or (RiverBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btRiver;
  end;

  if (SpecifiedHeadBoundary.Head.Count > 0)
    or (SpecifiedHeadBoundary.AssociatedSolution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btSpecifiedHead;
  end;

  if (WellBoundary.InjectionOrPumpingRate.Count > 0)
    or (WellBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btWell;
  end;

end;

procedure TPhastScreenObject.OtherIndex(const LayerOrRow, RowOrColumn: integer;
  out First, Last: integer; const DataSet: TDataArray);
var
  IsRiverDataSet: boolean;
begin
  IsRiverDataSet := (RiverBoundary.Head.Count > 0)
    or (RiverBoundary.AssociatedSolution.Count > 0);
  if IsRiverDataSet then
  begin
    IsRiverDataSet := (DataSet = frmGoPhast.PhastModel.Top2DBoundaryType);

    if not IsRiverDataSet then
    begin
      if (DataSet is TSparseArrayPhastDataSet) then
      begin
        IsRiverDataSet :=
          (frmGoPhast.PhastModel.RiverHead.IndexOfDataSet
          (TSparseArrayPhastDataSet(DataSet)) >= 0)
          or
          (frmGoPhast.PhastModel.RiverAssociatedSolution.IndexOfDataSet
          (TSparseArrayPhastDataSet(DataSet)) >= 0);
      end;
      if not IsRiverDataSet then
      begin
        IsRiverDataSet := (frmGoPhast.PhastModel.RiverDataSets.IndexOf(DataSet) >=
          0);
      end;
      if not IsRiverDataSet then
      begin
        if DataSet is TIntegerSparseDataSet then
        begin
          IsRiverDataSet :=
            TIntegerSparseDataSet(DataSet).IsBoundaryTypeDataSet;
        end;
      end;
    end;
  end;
  { TODO :
Changing the layer as woiuld be done here prevents the river data from
being displayed on the status bar.  Find a way around this problem. }

  {if IsRiverDataSet then
  begin
    First := frmGoPhast.PhastGrid.LayerCount;
    Last := First;
  end
  else
  begin  }
    inherited;
//  end;
end;

procedure TPhastScreenObject.SetUpToDate(const Value: boolean);
var
  Index: integer;
  Observer: TObserver;
begin
  inherited;
  if Value then
  begin
    for Index := 0 to FDataSetMixtureSubscriptions.Count -1 do
    begin
      Observer := FDataSetMixtureSubscriptions[Index] as TObserver;
      Observer.UpToDate := True;
    end;
  end;
end;

{ TUndoSetPhastScreenObjectProperties }

constructor TUndoSetPhastScreenObjectProperties.Create(
  const AListOfScreenObjects: TList);
var
  ScreenObjectIndex: integer;
  InterpValuesCollection: TInterpValuesCollection;
  APhastScreenObject: TPhastScreenObject;
  SpecifiedHeadBoundary: TSpecifiedHeadBoundary;
//  SpecifiedSolutionBoundary: TSpecifiedSolutionBoundary;
  FluxBoundary: TFluxBoundary;
  LeakyBoundary: TLeakyBoundary;
  RiverBoundary: TRiverBoundary;
  WellBoundary: TWellBoundary;
  WellIntervals: TWellIntervals;
begin
  inherited;
  FOldPhastInterpolationList := TObjectList.Create;
  FNewPhastInterpolationList := TObjectList.Create;
  FOldSpecifiedHeadList := TObjectList.Create;
  FNewSpecifiedHeadBoundary := TSpecifiedHeadBoundary.Create;
  FOldSpecifiedfluxList := TObjectList.Create;
  FNewSpecifiedFluxBoundary := TFluxBoundary.Create;
  FOldLeakyList := TObjectList.Create;
  FNewLeakyBoundary := TLeakyBoundary.Create;
  FOldRiverList := TObjectList.Create;
  FNewRiverBoundary := TRiverBoundary.Create;
  FOldWellList := TObjectList.Create;
  FNewWellBoundary := TWellBoundary.Create;

  SetLength(FOldLeakyHydraulicConductivity, AListOfScreenObjects.Count);
  SetLength(FOldLeakyThickness, AListOfScreenObjects.Count);
  SetLength(FOldRiverDescription, AListOfScreenObjects.Count);
  SetLength(FOldRiverHydraulicConductivity, AListOfScreenObjects.Count);
  SetLength(FOldRiverWidth, AListOfScreenObjects.Count);
  SetLength(FOldRiverDepth, AListOfScreenObjects.Count);
  SetLength(FOldRiverBedThickness, AListOfScreenObjects.Count);
  SetLength(FOldWellDescription, AListOfScreenObjects.Count);
  SetLength(FOldWellDiameter, AListOfScreenObjects.Count);
  SetLength(FOldWellLandSurfaceDatum, AListOfScreenObjects.Count);
  SetLength(FOldWellAllocation, AListOfScreenObjects.Count);
  SetLength(FOldWellIntervalFormat, AListOfScreenObjects.Count);
  SetLength(FOldHeadSolutionType, AListOfScreenObjects.Count);

  FOldIntervalList := TObjectList.Create;
  FNewWellIntervals := TWellIntervals.Create;

  for ScreenObjectIndex := 0 to AListOfScreenObjects.Count - 1 do
  begin
    APhastScreenObject := AListOfScreenObjects[ScreenObjectIndex];

    InterpValuesCollection := TInterpValuesCollection.Create;
    FOldPhastInterpolationList.Add(InterpValuesCollection);
    InterpValuesCollection.Assign(APhastScreenObject.InterpValues);

    InterpValuesCollection := TInterpValuesCollection.Create;
    FNewPhastInterpolationList.Add(InterpValuesCollection);
    InterpValuesCollection.Assign(APhastScreenObject.InterpValues);

    SpecifiedHeadBoundary := TSpecifiedHeadBoundary.Create;
    FOldSpecifiedHeadList.Add(SpecifiedHeadBoundary);
    SpecifiedHeadBoundary.Assign(APhastScreenObject.SpecifiedHeadBoundary);

    {SpecifiedSolutionBoundary := TSpecifiedSolutionBoundary.Create;
    FOldSpecifiedSolutionList.Add(SpecifiedSolutionBoundary);
    SpecifiedSolutionBoundary.Assign(APhastScreenObject.SpecifiedSolutionBoundary); }

    FluxBoundary := TFluxBoundary.Create;
    FOldSpecifiedFluxList.Add(FluxBoundary);
    FluxBoundary.Assign(APhastScreenObject.FluxBoundary);

    LeakyBoundary := TLeakyBoundary.Create;
    FOldLeakyList.Add(LeakyBoundary);
    LeakyBoundary.PartialAssign(APhastScreenObject.LeakyBoundary);

    RiverBoundary := TRiverBoundary.Create;
    FOldRiverList.Add(RiverBoundary);
    RiverBoundary.PartialAssign(APhastScreenObject.RiverBoundary);

    WellBoundary := TWellBoundary.Create;
    FOldWellList.Add(WellBoundary);
    WellBoundary.PartialAssign(APhastScreenObject.WellBoundary);

    FOldLeakyHydraulicConductivity[ScreenObjectIndex] := APhastScreenObject.
      LeakyBoundary.HydraulicConductivity;

    FOldLeakyThickness[ScreenObjectIndex] := APhastScreenObject.
      LeakyBoundary.Thickness;

    FOldRiverDescription[ScreenObjectIndex] := APhastScreenObject.
      RiverBoundary.Description;

    FOldRiverHydraulicConductivity[ScreenObjectIndex] := APhastScreenObject.
      RiverBoundary.BedHydraulicConductivity;

    FOldRiverWidth[ScreenObjectIndex] := APhastScreenObject.
      RiverBoundary.Width;

    FOldRiverDepth[ScreenObjectIndex] := APhastScreenObject.
      RiverBoundary.Depth;

    FOldRiverBedThickness[ScreenObjectIndex] := APhastScreenObject.
      RiverBoundary.BedThickness;

    FOldWellDescription[ScreenObjectIndex] := APhastScreenObject.
      WellBoundary.Description;

    FOldWellDiameter[ScreenObjectIndex] := APhastScreenObject.
      WellBoundary.Diameter;

    FOldWellLandSurfaceDatum[ScreenObjectIndex] := APhastScreenObject.
      WellBoundary.LandSurfaceDatum;

    FOldWellAllocation[ScreenObjectIndex] := APhastScreenObject.
      WellBoundary.AllocateByPressureAndMobility;

    FOldWellIntervalFormat[ScreenObjectIndex] := APhastScreenObject.
      WellBoundary.WellElevationFormat;

    WellIntervals := TWellIntervals.Create;
    FOldIntervalList.Add(WellIntervals);
    WellIntervals.Assign(APhastScreenObject.WellBoundary.Intervals);

    FOldHeadSolutionType[ScreenObjectIndex] :=
      APhastScreenObject.SpecifiedHeadBoundary.SolutionType;
  end;
end;

destructor TUndoSetPhastScreenObjectProperties.Destroy;
begin
  FOldPhastInterpolationList.Free;
  FNewPhastInterpolationList.Free;
  FOldSpecifiedHeadList.Free;
  FNewSpecifiedHeadBoundary.Free;
  FOldSpecifiedFluxList.Free;
  FNewSpecifiedFluxBoundary.Free;
  FOldLeakyList.Free;
  FNewLeakyBoundary.Free;
  FOldRiverList.Free;
  FNewRiverBoundary.Free;
  FOldWellList.Free;
  FNewWellBoundary.Free;
  FOldIntervalList.Free;
  FNewWellIntervals.Free;
  inherited;
end;

procedure TUndoSetPhastScreenObjectProperties.DoCommand;
var
  ScreenObjectIndex: integer;
  InterpValuesCollection: TInterpValuesCollection;
  APhastScreenObject: TPhastScreenObject;
  //  TempSolutionType: TSolutionType;
begin
  FShouldUpdateShowHideObjects := False;
  inherited;
  FShouldUpdateShowHideObjects := True;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    APhastScreenObject := FListOfScreenObjects[ScreenObjectIndex];

    InterpValuesCollection := FNewPhastInterpolationList[ScreenObjectIndex]
      as TInterpValuesCollection;
    APhastScreenObject.InterpValues.Assign(InterpValuesCollection);

    if FBoundaryChanged[btSpecifiedHead] then
    begin
      if (FNewBoundaryType = btSpecifiedHead) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.SpecifiedHeadBoundary.Head.Count > 0) or
        (APhastScreenObject.SpecifiedHeadBoundary.AssociatedSolution.Count > 0))
          then
      begin
        APhastScreenObject.SpecifiedHeadBoundary := FNewSpecifiedHeadBoundary;
      end
      else
      begin
        APhastScreenObject.SpecifiedHeadBoundary.Clear;
      end;
    end;

    {if FBoundaryChanged[btSpecifiedSolution] then
    begin
      if (FNewBoundaryType = btSpecifiedSolution) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.SpecifiedSolutionBoundary.Solution.Count > 0)) then
      begin
        APhastScreenObject.SpecifiedSolutionBoundary := NewSpecifiedSolutionBoundary;
      end
      else
      begin
        APhastScreenObject.SpecifiedSolutionBoundary.Clear;
      end;
    end; }

    if FBoundaryChanged[btFlux] then
    begin
      if (FNewBoundaryType = btFlux) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.FluxBoundary.Flux.Count > 0) or
        (APhastScreenObject.FluxBoundary.AssociatedSolution.Count > 0)) then
      begin
        APhastScreenObject.FluxBoundary := FNewSpecifiedFluxBoundary;
      end
      else
      begin
        APhastScreenObject.FluxBoundary.Clear;
      end;
    end;

    if FBoundaryChanged[btLeaky] then
    begin
      if (FNewBoundaryType = btLeaky) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.LeakyBoundary.Head.Count > 0) or
        (APhastScreenObject.LeakyBoundary.AssociatedSolution.Count > 0)) then
      begin
        APhastScreenObject.LeakyBoundary.PartialAssign(FNewLeakyBoundary);
      end
      else
      begin
        APhastScreenObject.LeakyBoundary.Clear;
      end;
    end;

    if FBoundaryChanged[btRiver] then
    begin
      if (FNewBoundaryType = btRiver) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.RiverBoundary.Head.Count > 0) or
        (APhastScreenObject.RiverBoundary.AssociatedSolution.Count > 0)) then
      begin
        APhastScreenObject.RiverBoundary.PartialAssign(FNewRiverBoundary);
      end
      else
      begin
        APhastScreenObject.RiverBoundary.Clear;
      end;
    end;

    if FBoundaryChanged[btWell] then
    begin
      if (FNewBoundaryType = btWell) or
        ((FNewBoundaryType = btNone) and
        (APhastScreenObject.WellBoundary.InjectionOrPumpingRate.Count > 0) or
        (APhastScreenObject.WellBoundary.Solution.Count > 0)) then
      begin
        APhastScreenObject.WellBoundary.PartialAssign(FNewWellBoundary);
      end
      else
      begin
        APhastScreenObject.WellBoundary.Clear;
      end;
    end;

    if FSetLeakyHydraulicConductivity then
    begin
      APhastScreenObject.LeakyBoundary.HydraulicConductivity :=
        FNewLeakyHydraulicConductivity
    end;

    if FSetLeakyThickness then
    begin
      APhastScreenObject.LeakyBoundary.Thickness :=
        FNewLeakyThickness
    end;

    if FSetRiverDescription then
    begin
      APhastScreenObject.RiverBoundary.Description :=
        FNewRiverDescription
    end;

    if FSetRiverHydraulicConductivity then
    begin
      APhastScreenObject.RiverBoundary.BedHydraulicConductivity :=
        FNewRiverHydraulicConductivity
    end;

    if FSetRiverWidth then
    begin
      APhastScreenObject.RiverBoundary.Width :=
        FNewRiverWidth
    end;

    if FSetRiverDepth then
    begin
      APhastScreenObject.RiverBoundary.Depth :=
        FNewRiverDepth
    end;

    if FSetRiverBedThickness then
    begin
      APhastScreenObject.RiverBoundary.BedThickness :=
        FNewRiverBedThickness
    end;

    if FSetWellDescription then
    begin
      APhastScreenObject.WellBoundary.Description := FNewWellDescription;
    end;

    if FSetWellDiameter then
    begin
      APhastScreenObject.WellBoundary.Diameter := FNewWellDiameter;
    end;

    if FSetWellLandSurfaceDatum then
    begin
      APhastScreenObject.WellBoundary.LandSurfaceDatum :=
        FNewWellLandSurfaceDatum;
    end;

    if FSetWellAllocation then
    begin
      APhastScreenObject.WellBoundary.AllocateByPressureAndMobility :=
        FNewWellAllocation;
    end;

    if FSetWellIntervalFormat then
    begin
      APhastScreenObject.WellBoundary.WellElevationFormat :=
        FNewWellIntervalFormat;
    end;

    if FSetWellIntervals then
    begin
      APhastScreenObject.WellBoundary.Intervals := FNewWellIntervals
    end;

    if FSetHeadSolutionType then
    begin
      APhastScreenObject.SpecifiedHeadBoundary.SolutionType :=
        FNewHeadSolutionType;
    end;

    APhastScreenObject.LeakyBoundary.Reset;

    APhastScreenObject.Invalidate;
  end;
  UpdateShowHideObjects;
end;

procedure
  TUndoSetPhastScreenObjectProperties.ResetScreenObjectDataSetSubscriptions;
var
  Index: Integer;
  AScreenObject: TPhastScreenObject;
begin
  inherited;

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index] as
      TPhastScreenObject;
    AScreenObject.ResetMixtureSubscriptions;
    AScreenObject.ResetBoundaryMixtureSubscriptions;
  end;
end;

procedure TUndoSetPhastScreenObjectProperties.Undo;
var
  ScreenObjectIndex: integer;
  InterpValuesCollection: TInterpValuesCollection;
  APhastScreenObject: TPhastScreenObject;
begin
  FShouldUpdateShowHideObjects := False;
  inherited;
  FShouldUpdateShowHideObjects := True;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    APhastScreenObject := FListOfScreenObjects[ScreenObjectIndex];

    InterpValuesCollection := FOldPhastInterpolationList[ScreenObjectIndex]
      as TInterpValuesCollection;
    APhastScreenObject.InterpValues.Assign(InterpValuesCollection);

    if FBoundaryChanged[btSpecifiedHead] then
    begin
      APhastScreenObject.SpecifiedHeadBoundary :=
        FOldSpecifiedHeadList[ScreenObjectIndex] as TSpecifiedHeadBoundary;
    end;

    {if FBoundaryChanged[btSpecifiedSolution] then
    begin
      APhastScreenObject.SpecifiedSolutionBoundary :=
        OldSpecifiedSolutionList[ScreenObjectIndex]
        as TSpecifiedSolutionBoundary;
    end;}

    if FBoundaryChanged[btFlux] then
    begin
      APhastScreenObject.FluxBoundary :=
        FOldSpecifiedFluxList[ScreenObjectIndex] as TFluxBoundary;
    end;

    if FBoundaryChanged[btLeaky] then
    begin
      APhastScreenObject.LeakyBoundary.PartialAssign(
        FOldLeakyList[ScreenObjectIndex] as TLeakyBoundary);
    end;

    if FBoundaryChanged[btRiver] then
    begin
      APhastScreenObject.RiverBoundary.PartialAssign(
        FOldRiverList[ScreenObjectIndex] as TRiverBoundary);
    end;

    if FBoundaryChanged[btWell] then
    begin
      APhastScreenObject.WellBoundary.PartialAssign(
        FOldWellList[ScreenObjectIndex] as TWellBoundary);
    end;

    if FSetLeakyHydraulicConductivity then
    begin
      APhastScreenObject.LeakyBoundary.HydraulicConductivity :=
        FOldLeakyHydraulicConductivity[ScreenObjectIndex];
    end;

    if FSetLeakyThickness then
    begin
      APhastScreenObject.LeakyBoundary.Thickness :=
        FOldLeakyThickness[ScreenObjectIndex];
    end;

    if FSetRiverDescription then
    begin
      APhastScreenObject.RiverBoundary.Description :=
        FOldRiverDescription[ScreenObjectIndex];
    end;

    if FSetRiverHydraulicConductivity then
    begin
      APhastScreenObject.RiverBoundary.BedHydraulicConductivity :=
        FOldRiverHydraulicConductivity[ScreenObjectIndex];
    end;

    if FSetRiverWidth then
    begin
      APhastScreenObject.RiverBoundary.Width :=
        FOldRiverWidth[ScreenObjectIndex];
    end;

    if FSetRiverDepth then
    begin
      APhastScreenObject.RiverBoundary.Depth :=
        FOldRiverDepth[ScreenObjectIndex];
    end;

    if FSetRiverBedThickness then
    begin
      APhastScreenObject.RiverBoundary.BedThickness :=
        FOldRiverBedThickness[ScreenObjectIndex];
    end;

    if FSetWellDescription then
    begin
      APhastScreenObject.WellBoundary.Description :=
        FOldWellDescription[ScreenObjectIndex];
    end;

    if FSetWellDiameter then
    begin
      APhastScreenObject.WellBoundary.Diameter :=
        FOldWellDiameter[ScreenObjectIndex];
    end;

    if FSetWellLandSurfaceDatum then
    begin
      APhastScreenObject.WellBoundary.LandSurfaceDatum :=
        FOldWellLandSurfaceDatum[ScreenObjectIndex];
    end;

    if FSetWellAllocation then
    begin
      APhastScreenObject.WellBoundary.AllocateByPressureAndMobility :=
        FOldWellAllocation[ScreenObjectIndex];
    end;

    if FSetWellIntervalFormat then
    begin
      APhastScreenObject.WellBoundary.WellElevationFormat :=
        FOldWellIntervalFormat[ScreenObjectIndex];
    end;

    if FSetWellIntervals then
    begin
      APhastScreenObject.WellBoundary.Intervals :=
        FOldIntervalList[ScreenObjectIndex] as TWellIntervals;
    end;

    if FSetHeadSolutionType then
    begin
      APhastScreenObject.SpecifiedHeadBoundary.SolutionType :=
        FOldHeadSolutionType[ScreenObjectIndex];
    end;

    APhastScreenObject.Invalidate;
  end;
  UpdateShowHideObjects;
end;

{ TCustomPhastBoundaryCondition }

procedure TCustomPhastBoundaryCondition.AddMixtureSubscriptions;
var
  Compiler: TRbwParser;
  TempFormula: string;
  Expression: TExpression;
  UsedVariables: TStringList;
  UseIndex: integer;
  DS: TDataArray;
  AScreenObject: TPhastScreenObject;
  //  TimeList: TTimeList;
begin
  AScreenObject := ScreenObject;
  DS := GetDataSet;
  if (AScreenObject <> nil) and (DS <> nil) then
  begin
    FMixtureObserver.OnUpToDateSet := (DS as
      TSparseArrayPhastDataSet).ATimeList.Changed;
    AScreenObject.Subscribe(FMixtureObserver);

    Compiler := frmGoPhast.GetCompiler(DS.Orientation,
      AScreenObject.EvaluatedAt);
    TempFormula := Values.MixtureFormula;
    try
      Compiler.Compile(TempFormula);
    except on ERbwParserError do
      begin
        TempFormula := FloatToStr(0.5);
        Values.MixtureFormula := TempFormula;
        Compiler.Compile(TempFormula);
      end;
    end;
    Expression := Compiler.CurrentExpression;

    UsedVariables := TStringList.Create;
    try
      UsedVariables.Assign(Expression.VariablesUsed);
      for UseIndex := 0 to UsedVariables.Count - 1 do
      begin
        DS := frmGoPhast.PhastModel.GetDataSetByName(UsedVariables[UseIndex]);
        Assert(DS <> nil);
        DS.Subscribe(FMixtureObserver);
        FMixtureDataSetList.Add(DS);
      end;
    finally
      UsedVariables.Free;
    end;

    FMixtureObserver.UpToDate := True;
    FMixtureObserver.UpToDate := False;
  end;
end;

procedure TCustomPhastBoundaryCondition.Assign(Source: TPersistent);
var
  OtherBoundary: TCustomPhastBoundaryCondition;
  Item: TInterpValuesItem;
begin
  if Source is TCustomPhastBoundaryCondition then
  begin
    OtherBoundary := TCustomPhastBoundaryCondition(Source);
    MixtureExpression := OtherBoundary.MixtureExpression;
    InterpolationDirection := OtherBoundary.InterpolationDirection;
    Expression := OtherBoundary.Expression;
    Time := OtherBoundary.Time;
    // Fix source in case the MixtureExpression was invalid.
    OtherBoundary.MixtureExpression := MixtureExpression;
  end
  else if Source is TInterpValuesItem then
  begin
    Item := TInterpValuesItem(Source);
    MixtureExpression := Item.Values.MixtureFormula;
    InterpolationDirection := Item.Values.InterpolationDirection;
    // Fix source in case the MixtureExpression was invalid.
    Item.Values.MixtureFormula := MixtureExpression;
  end;
  inherited;
end;

constructor TCustomPhastBoundaryCondition.Create(Collection: TCollection);
begin
  inherited;
  FMixtureDataSetList := TList.Create;
  FMixtureObserver := TObserver.Create(nil);
end;

destructor TCustomPhastBoundaryCondition.Destroy;
begin
  RemoveMixtureSubscriptions;
  FMixtureObserver.Free;
  FMixtureDataSetList.Free;
  inherited;
end;

function TCustomPhastBoundaryCondition.GetScreenObject: TPhastScreenObject;
begin
  result := (Collection as TCustomPhastBoundaryCollection).ScreenObject;
end;

function TCustomPhastBoundaryCondition.GetDataSet: TSparseArrayPhastDataSet;
begin
  result := (Collection as TCustomPhastBoundaryCollection).GetDataSet(Time);
end;

function TCustomPhastBoundaryCondition.GetDistance1: double;
begin
  result := Values.Distance1;
end;

function TCustomPhastBoundaryCondition.GetDistance2: double;
begin
  result := Values.Distance2;
end;

function TCustomPhastBoundaryCondition.GetInterpolationDirection:
  TInterpolationDirection;
begin
  result := Values.InterpolationDirection;
end;

function TCustomPhastBoundaryCondition.GetMixtureExpression: string;
begin
  result := Values.MixtureFormula;
end;

function TCustomPhastBoundaryCondition.GetUsePHAST_Interpolation: boolean;
begin
  result := Values.UsePHAST_Interpolation;
end;

procedure TCustomPhastBoundaryCondition.RemoveMixtureSubscriptions;
var
  AScreenObject: TPhastScreenObject;
  DS: TDataArray;
  Index: integer;
begin
  FMixtureObserver.OnUpToDateSet := nil;

  AScreenObject := ScreenObject;
  if AScreenObject <> nil then
  begin
    AScreenObject.UnSubscribe(FMixtureObserver);
  end;

  FMixtureObserver.RemoveAllSubscriptions;
  for Index := 0 to FMixtureDataSetList.Count - 1 do
  begin
    DS := FMixtureDataSetList[Index];
    DS.Unsubscribe(FMixtureObserver);
  end;
  FMixtureDataSetList.Clear;
end;

procedure TCustomPhastBoundaryCondition.ResetMixtureSubscription;
begin
  FMixtureObserver.UpToDate := True;
end;

procedure TCustomPhastBoundaryCondition.SetDistance1(const Value: double);
begin
  if Values.Distance1 <> Value then
  begin
    Values.Distance1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetDistance2(const Value: double);
begin
  if Values.Distance2 <> Value then
  begin
    Values.Distance2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetExpression(const Value: string);
begin
  if FExpression <> Value then
  begin
    FExpression := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetInterpolationDirection(
  const Value: TInterpolationDirection);
begin
  if Values.InterpolationDirection <> Value then
  begin
    Values.InterpolationDirection := Value;
    if Value = pidMix then
    begin
      AddMixtureSubscriptions;
    end
    else
    begin
      RemoveMixtureSubscriptions;
    end;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetMixtureExpression(
  const Value: string);
begin
  if Values.MixtureFormula <> Value then
  begin
    RemoveMixtureSubscriptions;
    Values.MixtureFormula := Value;
    if Values.InterpolationDirection = pidMix then
    begin
      AddMixtureSubscriptions;
    end;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetMixtureFormula(const Value: string);
begin
  FMixtureFormula := Value;
end;

procedure TCustomPhastBoundaryCondition.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetUsePHAST_Interpolation(
  const Value: boolean);
begin
  if Values.UsePHAST_Interpolation <> Value then
  begin
    Values.UsePHAST_Interpolation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.UpdateMixtureExpression;
begin
  MixtureExpression := FMixtureFormula;
end;

{ TRealPhastBoundaryCondition }

function TRealPhastBoundaryCondition.GetDatatype: TRbwDataType;
begin
  result := rdtDouble;
end;

function TRealPhastBoundaryCondition.GetValue1: double;
begin
  result := Values.RealValue1;
end;

function TRealPhastBoundaryCondition.GetValue2: double;
begin
  result := Values.RealValue2;
end;

procedure TRealPhastBoundaryCondition.SetValue1(const Value: double);
begin
  if Values.RealValue1 <> Value then
  begin
    Values.RealValue1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRealPhastBoundaryCondition.SetValue2(const Value: double);
begin
  if Values.RealValue2 <> Value then
  begin
    Values.RealValue2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TIntegerPhastBoundaryCondition }

function TIntegerPhastBoundaryCondition.GetDatatype: TRbwDataType;
begin
  result := rdtInteger;
end;

{function TIntegerPhastBoundaryCondition.GetMixtureExpression: string;
begin
  result := Values.MixtureExpression;
end;  }

function TIntegerPhastBoundaryCondition.GetValue1: integer;
begin
  result := Values.IntValue1;
end;

function TIntegerPhastBoundaryCondition.GetValue2: integer;
begin
  result := Values.IntValue2;
end;

{procedure TIntegerPhastBoundaryCondition.SetMixtureExpression(
  const Value: string);
begin
  if Values.MixtureExpression <> Value then
  begin
    Values.MixtureExpression := Value;
    frmGoPhast.InvalidateModel;
  end;
end;}

procedure TIntegerPhastBoundaryCondition.SetValue1(const Value: integer);
begin
  if Values.IntValue1 <> Value then
  begin
    Values.IntValue1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TIntegerPhastBoundaryCondition.SetValue2(const Value: integer);
begin
  if Values.IntValue2 <> Value then
  begin
    Values.IntValue2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TRealPhastBoundaries }

constructor TRealPhastBoundaries.Create;
begin
  inherited Create(TRealPhastBoundaryCondition);
end;

function TRealPhastBoundaries.GetDatatype: TRbwDataType;
begin
  result := rdtDouble;
end;

{ TIntegerPhastBoundaries }

constructor TIntegerPhastBoundaries.Create;
begin
  inherited Create(TIntegerPhastBoundaryCondition);
end;

function TIntegerPhastBoundaries.GetDatatype: TRbwDataType;
begin
  result := rdtInteger;
end;

{ TFluxBoundary }

procedure TFluxBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TFluxBoundary;
begin
  if Source is TFluxBoundary then
  begin
    SourceBoundary := TFluxBoundary(Source);
    Flux := SourceBoundary.Flux;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
    frmGoPhast.InvalidateModel;
  end;
  inherited;
end;

procedure TFluxBoundary.Clear;
begin
  Flux.Clear;
  AssociatedSolution.Clear;
  frmGoPhast.InvalidateModel;
end;

constructor TFluxBoundary.Create;
begin
  inherited;
  FFlux := TRealPhastBoundaries.Create;
  FAssociatedSolution := TIntegerPhastBoundaries.Create;
  FFlux.PropName := 'Flux';
  FAssociatedSolution.PropName := 'Flux_Associated_Solution';
end;

destructor TFluxBoundary.Destroy;
begin
  FFlux.Free;
  FAssociatedSolution.Free;
  inherited;
end;

procedure TFluxBoundary.SetAssociatedSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FAssociatedSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TFluxBoundary.SetScreenObject(const Value: TPhastScreenObject);
begin
  inherited;
  FAssociatedSolution.ScreenObject := Value;
  FFlux.ScreenObject := Value;
end;

procedure TFluxBoundary.SetFlux(const Value: TRealPhastBoundaries);
begin
  FFlux.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TFluxBoundary.SetOrientation(const Value: TViewDirection);
begin
  inherited;
  case Value of
    vdTop:
      begin
        FFlux.TimeList := frmGoPhast.PhastModel.TopFluxBoundaryFlux;
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.TopFluxBoundaryChemistry;
      end;
    vdFront:
      begin
        FFlux.TimeList := frmGoPhast.PhastModel.FrontFluxBoundaryFlux;
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.FrontFluxBoundaryChemistry;
      end;
    vdSide:
      begin
        FFlux.TimeList := frmGoPhast.PhastModel.SideFluxBoundaryFlux;
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.SideFluxBoundaryChemistry;
      end;
  else
    Assert(False);
  end;
end;

procedure TFluxBoundary.UpdateMixtureExpression;
begin
  FFlux.UpdateMixtureExpression;
  FAssociatedSolution.UpdateMixtureExpression;
end;

{ TLeakyBoundary }

procedure TLeakyBoundary.Reset;
begin
  if (Head.Count = 0) and (AssociatedSolution.Count = 0) then
  begin
    Thickness := '';
    HydraulicConductivity := '';
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TLeakyBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TLeakyBoundary;
begin
  frmGoPhast.InvalidateModel;
  if Source is TLeakyBoundary then
  begin
    SourceBoundary := TLeakyBoundary(Source);
    Head := SourceBoundary.Head;
    Thickness := SourceBoundary.Thickness;
    HydraulicConductivity := SourceBoundary.HydraulicConductivity;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
  end;
  inherited;
end;

procedure TLeakyBoundary.Clear;
begin
  Head.Clear;
  AssociatedSolution.Clear;
  frmGoPhast.InvalidateModel;
end;

constructor TLeakyBoundary.Create;
begin
  inherited;
  FAssociatedSolution := TIntegerPhastBoundaries.Create;
  FHead := TRealPhastBoundaries.Create;
  FHead.PropName := 'Leaky_Head';
  FAssociatedSolution.PropName := 'Leaky_Associated_Solution';
end;

destructor TLeakyBoundary.Destroy;
begin
  FAssociatedSolution.Free;
  FHead.Free;
  inherited;
end;

function TLeakyBoundary.GetHydraulicConductivity: string;
begin
  if (Head.Count > 0) or (AssociatedSolution.Count > 0) then
  begin
    result := FHydraulicConductivity;
  end
  else
  begin
    result := '';
  end;
end;

procedure TLeakyBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TLeakyBoundary;
begin
  if Source is TLeakyBoundary then
  begin
    SourceBoundary := TLeakyBoundary(Source);
    Head := SourceBoundary.Head;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TLeakyBoundary.SetAssociatedSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FAssociatedSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TLeakyBoundary.SetHead(const Value: TRealPhastBoundaries);
begin
  FHead.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TLeakyBoundary.SetHydraulicConductivity(const Value: string);
begin
  FHydraulicConductivity := Trim(Value);
  case Orientation of
    vdTop:
      begin
        UpdateBoundaryDataSet(rsTopLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
    vdFront:
      begin
        UpdateBoundaryDataSet(rsFrontLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
    vdSide:
      begin
        UpdateBoundaryDataSet(rsSideLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
  else
    Assert(False);
  end;
end;

procedure TLeakyBoundary.SetOrientation(const Value: TViewDirection);
begin
  inherited;
  case Value of
    vdTop:
      begin
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.TopLeakyAssociatedSolution;
        FHead.TimeList := frmGoPhast.PhastModel.TopLeakyHead;
      end;
    vdFront:
      begin
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.FrontLeakyAssociatedSolution;
        FHead.TimeList := frmGoPhast.PhastModel.FrontLeakyHead;
      end;
    vdSide:
      begin
        FAssociatedSolution.TimeList :=
          frmGoPhast.PhastModel.SideLeakyAssociatedSolution;
        FHead.TimeList := frmGoPhast.PhastModel.SideLeakyHead;
      end;
  else
    Assert(False);
  end;
end;

procedure TLeakyBoundary.SetThickness(const Value: string);
begin
  if FThickness <> trim(Value) then
  begin
    FThickness := trim(Value);
    frmGoPhast.InvalidateModel;
    case Orientation of
      vdTop:
        begin
          UpdateBoundaryDataSet(rsTopLeakyThickness, FThickness);
        end;
      vdFront:
        begin
          UpdateBoundaryDataSet(rsFrontLeakyThickness, FThickness);
        end;
      vdSide:
        begin
          UpdateBoundaryDataSet(rsSideLeakyThickness, FThickness);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TLeakyBoundary.SetScreenObject(const Value: TPhastScreenObject);
begin
  inherited;
  FAssociatedSolution.ScreenObject := Value;
  FHead.ScreenObject := Value;
end;

procedure TLeakyBoundary.UpdateMixtureExpression;
begin
  FHead.UpdateMixtureExpression;
  FAssociatedSolution.UpdateMixtureExpression;
end;

{ TRiverBoundary }

procedure TRiverBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TRiverBoundary;
begin
  if Source is TRiverBoundary then
  begin
    SourceBoundary := TRiverBoundary(Source);
    Description := SourceBoundary.Description;
    Head := SourceBoundary.Head;
    Width := SourceBoundary.Width;
    Depth := SourceBoundary.Depth;
    BedThickness := SourceBoundary.BedThickness;
    BedHydraulicConductivity := SourceBoundary.BedHydraulicConductivity;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRiverBoundary.Clear;
begin
  Head.Clear;
  AssociatedSolution.Clear;
  frmGoPhast.InvalidateModel;
end;

constructor TRiverBoundary.Create;
begin
  inherited;
  FAssociatedSolution := TIntegerPhastBoundaries.Create;
  FHead := TRealPhastBoundaries.Create;

  FAssociatedSolution.TimeList := frmGoPhast.PhastModel.RiverAssociatedSolution;
  FHead.TimeList := frmGoPhast.PhastModel.RiverHead;
  FHead.PropName := 'River_Head';
  FAssociatedSolution.PropName := 'River_Associated_Solution';
end;

destructor TRiverBoundary.Destroy;
begin
  FAssociatedSolution.Free;
  FHead.Free;
  inherited;
end;

function TRiverBoundary.IsBoundary: boolean;
begin
  result := (Head.Count > 0) and (Width <> '') and (Depth <> '')
    and (BedThickness <> '') and (BedHydraulicConductivity <> '');
  if result and frmGoPhast.PhastModel.SoluteTransport then
  begin
    result := (AssociatedSolution.Count > 0)
  end;
end;

procedure TRiverBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TRiverBoundary;
begin
  if Source is TRiverBoundary then
  begin
    SourceBoundary := TRiverBoundary(Source);
    Head := SourceBoundary.Head;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TRiverBoundary.SetAssociatedSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FAssociatedSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TRiverBoundary.SetBedHydraulicConductivity(const Value: string);
begin
  if FBedHydraulicConductivity <> Trim(Value) then
  begin
    FBedHydraulicConductivity := Trim(Value);
    UpdateBoundaryDataSet(rsRiverHydraulicConductivity,
      FBedHydraulicConductivity);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRiverBoundary.SetBedThickness(const Value: string);
begin
  if FBedThickness <> Trim(Value) then
  begin
    FBedThickness := Trim(Value);
    UpdateBoundaryDataSet(rsRiverBedThickness, FBedThickness);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRiverBoundary.SetScreenObject(const Value: TPhastScreenObject);
begin
  inherited;
  FAssociatedSolution.ScreenObject := Value;
  FHead.ScreenObject := Value;
end;

procedure TRiverBoundary.SetDepth(const Value: string);
begin
  if FDepth <> Trim(Value) then
  begin
    FDepth := Trim(Value);
    UpdateBoundaryDataSet(rsRiverDepth, FDepth);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRiverBoundary.SetDescription(const Value: string);
begin
  if FDescription <> Trim(Value) then
  begin
    FDescription := Trim(Value);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRiverBoundary.SetHead(const Value: TRealPhastBoundaries);
begin
  FHead.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TRiverBoundary.SetWidth(const Value: string);
begin
  if FWidth <> Trim(Value) then
  begin
    FWidth := Trim(Value);
    UpdateBoundaryDataSet(rsRiverWidth, FWidth);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRiverBoundary.UpdateMixtureExpression;
begin
  FHead.UpdateMixtureExpression;
  FAssociatedSolution.UpdateMixtureExpression;
end;

{ TSpecifiedHeadBoundary }

procedure TSpecifiedHeadBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TSpecifiedHeadBoundary;
begin
  if Source is TSpecifiedHeadBoundary then
  begin
    SourceBoundary := TSpecifiedHeadBoundary(Source);
    SolutionType := SourceBoundary.SolutionType;
    Head := SourceBoundary.Head;
    AssociatedSolution := SourceBoundary.AssociatedSolution;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSpecifiedHeadBoundary.Clear;
begin
  Head.Clear;
  AssociatedSolution.Clear;
end;

constructor TSpecifiedHeadBoundary.Create;
begin
  inherited;
  FSolution := TIntegerPhastBoundaries.Create;
  FHead := TRealPhastBoundaries.Create;

  FSolution.TimeList :=
    frmGoPhast.PhastModel.SpecifiedHeadAssociatedSolution;
  FHead.TimeList := frmGoPhast.PhastModel.SpecifiedHeadHead;
  FHead.PropName := 'Specified_Head';
  FSolution.PropName := 'Specified_Head_Solution';
end;

destructor TSpecifiedHeadBoundary.Destroy;
begin
  FSolution.Free;
  FHead.Free;
  inherited;
end;

procedure TSpecifiedHeadBoundary.SetSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TSpecifiedHeadBoundary.SetScreenObject(const Value:
  TPhastScreenObject);
begin
  inherited;
  FSolution.ScreenObject := Value;
  FHead.ScreenObject := Value;
end;

procedure TSpecifiedHeadBoundary.SetHead(const Value: TRealPhastBoundaries);
begin
  FHead.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TSpecifiedHeadBoundary.SetSolutionType(
  const Value: TSolutionType);
begin
  if (FSolutionType <> Value) then
  begin
    FSolutionType := Value;
    UpdateBoundaryDataSet(rsSolutionType, IntToStr(Ord(FSolutionType)));
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    UpdateBoundaryDataSet(rsSolutionType, IntToStr(Ord(FSolutionType)));
  end;
end;

procedure TSpecifiedHeadBoundary.UpdateMixtureExpression;
begin
  FHead.UpdateMixtureExpression;
  FSolution.UpdateMixtureExpression;
end;

{ TSpecifiedSolutionBoundary }

procedure TSpecifiedSolutionBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TSpecifiedSolutionBoundary;
begin
  if Source is TSpecifiedSolutionBoundary then
  begin
    SourceBoundary := TSpecifiedSolutionBoundary(Source);
    Solution := SourceBoundary.Solution;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSpecifiedSolutionBoundary.Clear;
begin
  Solution.Clear;
end;

constructor TSpecifiedSolutionBoundary.Create;
begin
  inherited;
  FSolution := TIntegerPhastBoundaries.Create;
  FSolution.TimeList := frmGoPhast.PhastModel.SpecifiedSolution;
  FSolution.PropName := 'Specified_Solution';
end;

destructor TSpecifiedSolutionBoundary.Destroy;
begin
  FSolution.Free;
  inherited;
end;

procedure TSpecifiedSolutionBoundary.SetScreenObject(
  const Value: TPhastScreenObject);
begin
  inherited;
  FSolution.ScreenObject := Value;
end;

procedure TSpecifiedSolutionBoundary.SetSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

{ TCustomPhastBoundaryCollection }

procedure TCustomPhastBoundaryCollection.Clear;
begin
  inherited;
  if TimeList <> nil then
  begin
    TimeList.Invalidate;
  end;
end;


function TCustomPhastBoundaryCollection.GetDataSet(
  const ATime: double): TSparseArrayPhastDataSet;
var
  TimeIndex: integer;
  AnArray: TSparseArrayPhastDataSet;
begin
  TimeIndex := TimeList.IndexOf(ATime);
  if TimeIndex < 0 then
  begin
    AnArray := nil;
    case GetDatatype of
      rdtDouble:
        begin
          AnArray := TSparseRealPhastDataSet.Create(frmGoPhast.PhastModel);
        end;
      rdtInteger:
        begin
          AnArray := TSparseIntegerPhastDataSet.Create(frmGoPhast.PhastModel);
        end;
    else
      Assert(False);
    end;
    Assert(ScreenObject <> nil);
    case ScreenObject.ViewDirection of
      vdTop:
        begin
          case FTimeList.BoundaryType of
            btRiver, btWell:
              begin
                AnArray.BoundaryDataType := frmGoPhast.PhastModel.Top2DBoundaryType;
              end;
          else
            begin
              AnArray.BoundaryDataType := frmGoPhast.PhastModel.TopBoundaryType;
            end;
          end;

        end;
      vdFront:
        begin
          AnArray.BoundaryDataType := frmGoPhast.PhastModel.FrontBoundaryType;
        end;
      vdSide:
        begin
          AnArray.BoundaryDataType := frmGoPhast.PhastModel.SideBoundaryType;
        end;
    else
      Assert(False);
    end;

    AnArray.Orientation := TimeList.Orientation;
    AnArray.ATimeList := TimeList;
    TimeIndex := TimeList.Add(ATime, AnArray);
  end;
  result := TimeList.Items[TimeIndex];
end;

procedure TSpecifiedSolutionBoundary.UpdateMixtureExpression;
begin
  FSolution.UpdateMixtureExpression;
end;

{ TWellBoundary }

procedure TWellBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TWellBoundary;
begin
  if Source is TWellBoundary then
  begin
    SourceBoundary := TWellBoundary(Source);
    Description := SourceBoundary.Description;
    Solution := SourceBoundary.Solution;
    InjectionOrPumpingRate := SourceBoundary.InjectionOrPumpingRate;
    LandSurfaceDatum := SourceBoundary.LandSurfaceDatum;
    Diameter := SourceBoundary.Diameter;
    AllocateByPressureAndMobility :=
      SourceBoundary.AllocateByPressureAndMobility;
    WellElevationFormat := SourceBoundary.WellElevationFormat;
    Intervals := SourceBoundary.Intervals;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TWellBoundary.Clear;
begin
  InjectionOrPumpingRate.Clear;
  Solution.Clear;
  Intervals.Clear;
  frmGoPhast.InvalidateModel;
end;

constructor TWellBoundary.Create;
begin
  inherited;
  FAllocateByPressureAndMobility := true;
  FSolution := TIntegerPhastBoundaries.Create;
  FInjectionOrPumpingRate := TRealPhastBoundaries.Create;
  FIntervals := TWellIntervals.Create;

  FSolution.TimeList := frmGoPhast.PhastModel.WellSolution;
  FInjectionOrPumpingRate.TimeList :=
    frmGoPhast.PhastModel.WellInjectionOrPumpingRate;
end;

destructor TWellBoundary.Destroy;
begin
  FSolution.Free;
  FInjectionOrPumpingRate.Free;
  FIntervals.Free;
  inherited;
end;

function TWellBoundary.IsBoundary: boolean;
begin
  result := (InjectionOrPumpingRate.Count > 0) and (Intervals.Count > 0);
  if result and frmGoPhast.PhastModel.SoluteTransport then
  begin
    result := (Solution.Count > 0)
  end;
end;

procedure TWellBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TWellBoundary;
begin
  if Source is TWellBoundary then
  begin
    SourceBoundary := TWellBoundary(Source);
    Solution := SourceBoundary.Solution;
    InjectionOrPumpingRate := SourceBoundary.InjectionOrPumpingRate;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TWellBoundary.SetAllocateByPressureAndMobility(
  const Value: boolean);
begin
  FAllocateByPressureAndMobility := Value;
end;

procedure TWellBoundary.SetScreenObject(const Value: TPhastScreenObject);
begin
  inherited;
  Solution.ScreenObject := ScreenObject;
  InjectionOrPumpingRate.ScreenObject := ScreenObject;
end;

procedure TWellBoundary.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TWellBoundary.SetDiameter(const Value: double);
begin
  if FDiameter <> Value then
  begin
    FDiameter := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TWellBoundary.SetInjectionOrPumpingRate(
  const Value: TRealPhastBoundaries);
begin
  FInjectionOrPumpingRate.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TWellBoundary.SetIntervals(const Value: TWellIntervals);
begin
  FIntervals.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TWellBoundary.SetLandSurfaceDatum(const Value: double);
begin
  if FLandSurfaceDatum <> Value then
  begin
    FLandSurfaceDatum := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TWellBoundary.SetSolution(const Value: TIntegerPhastBoundaries);
begin
  FSolution.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TWellBoundary.SetWellElevationFormat(
  const Value: TWellElevationFormat);
begin
  if FWellElevationFormat <> Value then
  begin
    FWellElevationFormat := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TWellBoundary.UpdateMixtureExpression;
begin
  Solution.UpdateMixtureExpression;
  InjectionOrPumpingRate.UpdateMixtureExpression;
end;

{ TWellInterval }

procedure TWellInterval.Assign(Source: TPersistent);
var
  SourceInterval: TWellInterval;
begin
  if Source is TWellInterval then
  begin
    SourceInterval := TWellInterval(Source);
    FirstElevation := SourceInterval.FirstElevation;
    SecondElevation := SourceInterval.SecondElevation;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TWellInterval.SetFirstElevation(const Value: double);
begin
  if FFirstElevation <> Value then
  begin
    FFirstElevation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TWellInterval.SetSecondElevation(const Value: double);
begin
  if FSecondElevation <> Value then
  begin
    FSecondElevation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TWellIntervals }

constructor TWellIntervals.Create;
begin
  inherited Create(TWellInterval);
end;

{ TCustomPhastBoundary }

procedure TCustomPhastBoundary.SetScreenObject(const Value: TPhastScreenObject);
begin
  FScreenObject := Value;
end;

procedure TCustomPhastBoundary.UpdateBoundaryDataSet(const DataSetName,
  Formula: string);
var
  NewPosition: integer;
  BoundaryPosition: integer;
begin
  if ScreenObject <> nil then
  begin
    BoundaryPosition :=
      frmGoPhast.PhastModel.IndexOfBoundaryDataSet(DataSetName);
    Assert(BoundaryPosition >= 0);
    if Formula = '' then
    begin
      NewPosition := ScreenObject.IndexOfBoundaryDataSet(frmGoPhast.PhastModel.
        BoundaryDataSets[BoundaryPosition]);
      if NewPosition >= 0 then
      begin
        ScreenObject.DeleteBoundaryDataSet(NewPosition);
      end;
    end
    else
    begin
      NewPosition :=
        ScreenObject.AddBoundaryDataSet(frmGoPhast.PhastModel.
        BoundaryDataSets[BoundaryPosition]);
      ScreenObject.BoundaryDataSetFormulas[NewPosition] := Formula;
    end
  end;
end;

{ TCustomOrientedPhastBoundary }

procedure TCustomOrientedPhastBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TCustomOrientedPhastBoundary;
begin
  if Source is TCustomOrientedPhastBoundary then
  begin
    SourceBoundary := TCustomOrientedPhastBoundary(Source);
    Orientation := SourceBoundary.Orientation;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomOrientedPhastBoundary.SetScreenObject(
  const Value: TPhastScreenObject);
begin
  inherited;
  if Value <> nil then
  begin
    Orientation := Value.ViewDirection;
  end
end;

procedure TCustomOrientedPhastBoundary.SetOrientation(
  const Value: TViewDirection);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TMultiValueScreenObject }

constructor TMultiValueScreenObject.Create(AnOwner: TComponent);
begin
  inherited;
  FRealValues := TRealDataListCollection.Create(self);
  FIntegerValues := TIntegerDataListCollection.Create(self);
end;

destructor TMultiValueScreenObject.Destroy;
begin
  FRealValues.Free;
  FIntegerValues.Free;
  inherited;
end;

procedure TMultiValueScreenObject.SetIntegerValues(
  const Value: TIntegerDataListCollection);
begin
  FIntegerValues.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

procedure TMultiValueScreenObject.SetRealValues(
  const Value: TRealDataListCollection);
begin
  FRealValues.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

{ TPhastScreenObjectItem }

constructor TPhastScreenObjectItem.Create(Collection: TCollection);
begin
  inherited;
  FMixtureFormulas := TStringList.Create;
end;

destructor TPhastScreenObjectItem.Destroy;
begin
  FMixtureFormulas.Free;
  inherited;
end;

function TPhastScreenObjectItem.GetMixtureFormulas: TStrings;
var
  Index: integer;
begin
  FMixtureFormulas.Clear;
  FMixtureFormulas.Capacity := ScreenObject.DataSetCount;
  for Index := 0 to ScreenObject.DataSetCount - 1 do
  begin
    FMixtureFormulas.Add((ScreenObject as
      TPhastScreenObject).MixtureDataSetFormula[Index]);
  end;
  Result := FMixtureFormulas;
end;

procedure TPhastScreenObjectItem.UpdateScreenObject;
var
  Index: integer;
begin
  inherited;
  (ScreenObject as TPhastScreenObject).FIsUpdating := True;
  try
    for Index := 0 to ScreenObject.DataSetCount - 1 do
    begin
      if Index = (ScreenObject as TPhastScreenObject).InterpValues.Count then
      begin
        (ScreenObject as TPhastScreenObject).InterpValues.Add;
      end;

      if FMixtureFormulas.Count > Index then
      begin
        (ScreenObject as TPhastScreenObject).MixtureDataSetFormula[Index] :=
          FMixtureFormulas[Index];
      end
      else
      begin
        (ScreenObject as TPhastScreenObject).MixtureDataSetFormula[Index] :=
          FloatToStr(0.5);
      end;
    end;
    (ScreenObject as TPhastScreenObject).UpdateMixtureExpression;
  finally
    (ScreenObject as TPhastScreenObject).FIsUpdating := False;
  end;

end;

{ TPhastScreenObjectCollection }

constructor TPhastScreenObjectCollection.Create;
begin
  inherited Create(TPhastScreenObjectItem);
end;

procedure TCustomPhastBoundaryCollection.SetTimeList(
  const Value: TTimeList);
begin
  FTimeList := Value;
end;

procedure TCustomPhastBoundaryCollection.UpdateMixtureExpression;
var
  Index: integer;
  Item: TCustomPhastBoundaryCondition;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomPhastBoundaryCondition;
    Item.UpdateMixtureExpression;
  end;
end;

{ TPhastScreenObjectDelegate }

{procedure TPhastScreenObjectDelegate.Assign(Source: TPersistent);
var
  OtherDelegate: TPhastScreenObjectDelegate;
  ScreenObject: TPhastScreenObject;
begin
  if Source is TPhastScreenObjectDelegate then
  begin
    OtherDelegate := TPhastScreenObjectDelegate(Source);
    FluxBoundary := OtherDelegate.FluxBoundary;
    LeakyBoundary := OtherDelegate.LeakyBoundary;
    RiverBoundary := OtherDelegate.RiverBoundary;
    SpecifiedHeadBoundary := OtherDelegate.SpecifiedHeadBoundary;
    WellBoundary := OtherDelegate.WellBoundary;
  end
  else if Source is TPhastScreenObject then
  begin
    ScreenObject := TPhastScreenObject(Source);
    FluxBoundary := ScreenObject.FluxBoundary;
    LeakyBoundary := ScreenObject.LeakyBoundary;
    RiverBoundary := ScreenObject.RiverBoundary;
    SpecifiedHeadBoundary := ScreenObject.SpecifiedHeadBoundary;
    WellBoundary := ScreenObject.WellBoundary;
  end
  else
  begin
    inherited Assign(Source);
  end;

end;  }

//procedure TPhastScreenObjectDelegate.SetFluxBoundary(
//  const Value: TFluxBoundary);
//begin
//  FFluxBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetInterpValues(
//  const Value: TInterpValuesCollection);
//begin
//  FInterpValues.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetLeakyBoundary(
//  const Value: TLeakyBoundary);
//begin
//  FLeakyBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetRiverBoundary(
//  const Value: TRiverBoundary);
//begin
//  FRiverBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetSpecifiedHeadBoundary(
//  const Value: TSpecifiedHeadBoundary);
//begin
//  FSpecifiedHeadBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetSpecifiedSolutionBoundary(
//  const Value: TSpecifiedSolutionBoundary);
//begin
//  FSpecifiedSolutionBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//procedure TPhastScreenObjectDelegate.SetWellBoundary(
//  const Value: TWellBoundary);
//begin
//  FWellBoundary.Assign(Value);
//  frmGoPhast.InvalidateModel;
//end;
//
//function TPhastScreenObjectDelegate.StoreFlux: boolean;
//begin
//  result := (FluxBoundary.Flux.Count > 0)
//    or (FluxBoundary.AssociatedSolution.Count > 0);
//end;
//
//function TPhastScreenObjectDelegate.StoreLeaky: boolean;
//begin
//  result := (LeakyBoundary.Head.Count > 0)
//    or (LeakyBoundary.AssociatedSolution.Count > 0);
//end;
//
//function TPhastScreenObjectDelegate.StoreRiver: boolean;
//begin
//  result := (RiverBoundary.Head.Count > 0)
//    or (RiverBoundary.AssociatedSolution.Count > 0);
//end;
//
//function TPhastScreenObjectDelegate.StoreSpecifiedHead: boolean;
//begin
//  result := (SpecifiedHeadBoundary.Head.Count > 0)
//    or (SpecifiedHeadBoundary.AssociatedSolution.Count > 0);
//end;
//
//function TPhastScreenObjectDelegate.StoreWell: boolean;
//begin
//  result := (WellBoundary.InjectionOrPumpingRate.Count > 0)
//    or (WellBoundary.Solution.Count > 0);
//end;
initialization
  RegisterClass(TPhastScreenObject);
  RegisterClass(TMultiValueScreenObject);

end.
