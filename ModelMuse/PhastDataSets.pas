{@abstract(@name is used to define a series of classes that represent
  data sets in PHAST.)  The main difference between the @link(TDataArray)s
  defined here and the usual @link(TDataArray)s is that these can use
  "PHAST-style Interpolation" (see @link(TPhastInterpolationValues))
  and some of them are sparse arrays used for boundary conditions.
  }
unit PhastDataSets;

interface

uses SysUtils, Classes, DataSetUnit, GoPhastTypes, RbwParser, SparseArrayUnit,
  SparseDataSets, RealListUnit, ZLib, FormulaManagerUnit;

type
  TCustomPhastDataSet = class;

  {@abstract(@name is used to store a sparse array of
    @link(TInterpolationDirection).)}
  T3DSparseInterpolationDirectionArray = class(T3DSparsePointerArray)
  private
    // @name: integer;
    // @name is the number of @link(TInterpolationDirection)s stored
    // in the @classname.
    FCount: integer;
    // @name: array of @link(TInterpolationDirection);
    // @name stores the @link(TInterpolationDirection)s stored
    // in the @classname.
    FValues: array of TInterpolationDirection;
    // See @link(Items).
    function GetItems(const Index1, Index2, Index3: Integer):
      TInterpolationDirection;
    // See @link(Items).
    procedure SetItems(const Index1, Index2, Index3: Integer;
      const Value: TInterpolationDirection);
  public
    // @name sets the number of @link(TInterpolationDirection)s stored
    // in the @classname to 0.
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(Quantum1, Quantum2, Quantum3: TSPAQuantum);
    // @name retrieves or stores a @link(TInterpolationDirection)
    // by its location.
    property Items[const Index1, Index2, Index3: Integer]:
      TInterpolationDirection read GetItems write SetItems; default;
  end;

  {
    @abstract(@name is used to store data related to
    PHAST-style interpolation and also to PHAST-style mixtures.)

    In PHAST-style interpolation, the user specifies a coordinate direction
    (X, Y, or Z), two distances, and two values.
    If the X, Y, or Z coordinate of the current node is less than or equal
    to the first distance, the first value is used.
    If the X, Y, or Z coordinate of the current node is greater than or equal
    to the second distance, the second value is used.
    For intermediate distances, linear interpolation between the two values
    is used.

    In PHAST-style mixtures, the user specifies two values and a series of
    proportions (between 0 and 1).
    The proportion represents the fraction of value 1 in the mixture.
    One minus the proportion represents the fraction of value 2 in the mixture.
    This is useful for chemical mixtures where Value 1 and Value 2 represent
    indices to two end member solutions.  The fraction tells how much or each
    end member to include in the solution.
  }
  TPhastInterpolationValues = class(TPersistent)
  private
    // See @link(DataSet).
    FDataSet: TCustomPhastDataSet;
    // See @link(Distance1).
    FDistance1: double;
    // See @link(Distance2).
    FDistance2: double;
    // See @link(InterpolationDirection).
    FInterpolationDirection: TInterpolationDirection;
    // See @link(IntValue1).
    FIntValue1: integer;
    // See @link(IntValue2).
    FIntValue2: integer;
    // See @link(MixtureFormula).
    FMixtureFormula: TFormulaObject;
    // See @link(RealValue1).
    FRealValue1: double;
    // See @link(RealValue2).
    FRealValue2: double;
    // See @link(UsePHAST_Interpolation).
    FUsePHAST_Interpolation: boolean;
    function GetDistance1: double;
    // See @link(Distance2).
    function GetDistance2: double;
    // See @link(Distance1).
    procedure SetDistance1(const Value: double);
    // See @link(Distance2).
    procedure SetDistance2(const Value: double);
    // See @link(InterpolationDirection).
    procedure SetInterpolationDirection(
      const Value: TInterpolationDirection);
    // See @link(IntValue1).
    procedure SetIntValue1(const Value: integer);
    // See @link(IntValue2).
    procedure SetIntValue2(const Value: integer);
    // See @link(MixtureFormula).
    procedure SetMixtureFormula(const Value: string);
    // See @link(RealValue1).
    procedure SetRealValue1(const Value: double);
    // See @link(RealValue2).
    procedure SetRealValue2(const Value: double);
    // See @link(UsePHAST_Interpolation).
    procedure SetUsePHAST_Interpolation(const Value: boolean);
    function GetMixtureFormula: string;
  public
    // Use @name to assign the properties of a @link(TCustomPhastDataSet),
    // @link(TCustomPhastBoundaryCondition), or @link(TPhastInterpolationValues)
    // to the current @classname.
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    // @name is the data set to which the @classname is
    // to be applied.
    property DataSet: TCustomPhastDataSet read FDataSet write FDataSet;
    // @name returns @true if Another is identical
    // with the @classname being called.
    function SameAs(Another: TPhastInterpolationValues): boolean;
  published
    // @name is the first distance used for "PHAST-style interpolation.
    // It is not used if InterpolationDirection = pidMix.
    property Distance1: double read GetDistance1 write SetDistance1;
    // @name is the second distance used for PHAST-style interpolation.
    // It is not used if InterpolationDirection = pidMix.
    property Distance2: double read GetDistance2 write SetDistance2;
    // @name determines whether PHAST-style interpolation
    // is used or PHAST-style mixtures.  If PHAST-style interpolation
    // is used, it also determines the coordinate direction.
    property InterpolationDirection: TInterpolationDirection
      read FInterpolationDirection write SetInterpolationDirection;
    // If @link(DataSet) stores integers, @name stores the first
    // integer value for PHAST-style interpolation.
    property IntValue1: integer read FIntValue1 write SetIntValue1;
    // If @link(DataSet) stores integers, @name stores the second
    // integer value for PHAST-style interpolation.
    property IntValue2: integer read FIntValue2 write SetIntValue2;
    // @name is identical to @link(MixtureFormula).  It is only
    // maintained for backwards compatibility.
    property MixtureExpression: string read GetMixtureFormula
      write SetMixtureFormula stored False;
    // @name is the expression used to determine the fraction
    // for "PHAST" style mixtures.
    // It is only used if InterpolationDirection = pidMix.
    property MixtureFormula: string read GetMixtureFormula
      write SetMixtureFormula;
    // If @link(DataSet) stores real numbers, @name stores the first
    // real number value for PHAST-style interpolation.
    property RealValue1: double read FRealValue1 write SetRealValue1;
    // If @link(DataSet) stores real numbers, @name stores the second
    // real number value for PHAST-style interpolation.
    // See @link(TPhastInterpolationValues).
    property RealValue2: double read FRealValue2 write SetRealValue2;
    // @name specifies whether PHAST-style interpolation should be used.
    // (For purposes of programming,
    // PHAST-style mixtures are considered to be PHAST-Interpolation.)
    // See @link(TPhastInterpolationValues).
    property UsePHAST_Interpolation: boolean
      read FUsePHAST_Interpolation write SetUsePHAST_Interpolation;
  end;

  {@abstract(@name is the abstract ancestor of @link(TDataArray)s that use
    PHAST-style interpolation.)}
  TCustomPhastDataSet = class(TDataArray)
  private
    // See @link(Distance1).
    FDistance1: double;
    // See @link(Distance2).
    FDistance2: double;
    // See @link(InterpolationDirection).
    FInterpolationDirection: TInterpolationDirection;
    // @name is a temporary variable used when changing @link(MixtureFormula).
    FMixtureFormula: string;
    // See @link(MixtureUseList).
    FMixtureUseList: TStringList;
    // @name is set to false when @link(MixtureUseList) is out of date.
    FMixtureUseListUpToDate: boolean;
    // See @link(UsePHAST_InterpolationForAllCells).
    FUsePHAST_InterpolationForAllCells: boolean;
    FMixtureFormulaObject: TFormulaObject;
    // See @link(Distance1).
    function GetDistance1: double;
    // See @link(Distance2).
    function GetDistance2: double;
    // See @link(MixtureUseList).
    function GetMixtureUseList: TStringList;
    // @name clears all data regarding PHAST-style interpolation
    procedure ResetCellsPhastInterpolation; virtual; abstract;
    // @name sets the @link(MixtureFormula) to 0.5.
    // @name is called if the @link(MixtureFormula) is invalid.
    procedure ResetMixtureExpression(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // See @link(Distance1).
    procedure SetDistance1(const Value: double);
    // See @link(Distance2).
    procedure SetDistance2(const Value: double);
    // See @link(InterpolationDirection).
    procedure SetInterpolationDirection(
      const Value: TInterpolationDirection);
    // See @link(MixtureFormula).
    procedure SetMixtureFormula(const Value: string);
    // See @link(UsePHAST_InterpolationForAllCells).
    procedure SetUsePHAST_InterpolationForAllCells(const Value: boolean);
    // @name recalculates @link(MixtureUseList).
    procedure UpdateMixtureUseList;
    function GetMixtureFormula: string;
  protected
    procedure SetEvaluatedAt(const Value: TEvaluatedAt); override;
    procedure SetOrientation(const Value: TDataSetOrientation); override;
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure StoreData(Stream: TStream); override;
    // @name is the first of the distances used for PHAST-style interpolation.
    function GetCellDistance1(const ALay, ARow, ACol: integer): double; virtual;
      abstract;
    // @name is the second of the distances used for PHAST-style interpolation.
    function GetCellDistance2(const ALay, ARow, ACol: integer): double; virtual;
      abstract;
    // @name specifies whether the PHAST-style interpolation is done in the
    // X, Y, or Z direction or with a mixture.
    function GetCellInterpolationDirection(const ALay, ARow, ACol: integer):
      TInterpolationDirection; virtual; abstract;
    // See @link(IsInterpolatedCell).
    function GetIsInterpolatedCell(const ALay, ARow, ACol: integer): boolean;
      virtual; abstract;
    // @name calls @inherited @name and then adds the contents of
    // @link(MixtureUseList) if appropriate.
    function GetUseList: TStringList; override;
    // @name stores the PHAST-style interpolation data at all cells where it
    // is appropriate to do so. @name does nothing in @classname but in
    // descendants it is used.
    procedure InitializePhastArrays; virtual;
    // @name holds a list of all the variables used by @link(MixtureFormula).
    property MixtureUseList: TStringList read GetMixtureUseList;
    // See @link(CellDistance1).
    procedure SetCellDistance1(const ALay, ARow, ACol: integer;
      const Value: double); virtual; abstract;
    // See @link(CellDistance2).
    procedure SetCellDistance2(const ALay, ARow, ACol: integer;
      const Value: double); virtual; abstract;
    // See @link(CellInterpolationDirection).
    procedure SetCellInterpolationDirection(const ALay, ARow, ACol: integer;
      const Value: TInterpolationDirection); virtual; abstract;
    // See @link(IsInterpolatedCell).
    procedure SetIsInterpolatedCell(const ALay, ARow, ACol: integer;
      const Value: boolean); virtual; abstract;
  public
    procedure CheckIfUniform; override;
    // @name copies data from Source to this @classname.
    // If Source is a @link(TCustomPhastDataSet) or a
    // @link(TPhastInterpolationValues), PHAST-style interpolation
    // data is copied from Source.
    procedure Assign(Source: TPersistent); override;
    // @name specifies the first PHAST-style interpolation distance for
    // the cell at ALay, ARow, ACol.
    property CellDistance1[const ALay, ARow, ACol: integer]: double read
      GetCellDistance1 write SetCellDistance1;
    // @name specifies the second PHAST-style interpolation distance for
    // the cell at ALay, ARow, ACol.
    property CellDistance2[const ALay, ARow, ACol: integer]: double read
      GetCellDistance2 write SetCellDistance2;
    // @name specifies the @link(TInterpolationDirection) for
    // PHAST-style interpolation distance for the cell at ALay, ARow, ACol.
    property CellInterpolationDirection[const ALay, ARow, ACol: integer]:
      TInterpolationDirection read GetCellInterpolationDirection write
      SetCellInterpolationDirection;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    (* @name sets the values for all the cells in @classname.

@html(
<table
style="margin-left: auto; margin-right: auto; width: 100%; text-align: left;"
border="0" cellpadding="2" cellspacing="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; text-align: center;">
      <img src="GoPhastFlowChart1.jpg" alt="Flow Chart, Part 1"><br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; text-align: center;">
      <em>Flow chart (part 1) illustrating how values are assigned
      to elements or cells.</em><br>
      </td>
    </tr>
  </tbody>
</table>
)

@latex(\begin{figure}[h]
\centering
\includegraphics{GoPhastFlowChart1.jpg}
\caption{Flow chart (part 1) illustrating how values are assigned
to elements or cells.}
\end{figure})


@html(
<table
style="margin-left: auto; margin-right: auto; width: 100%; text-align: left;"
border="0" cellpadding="2" cellspacing="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; text-align: center;">
      <img src="GoPhastFlowChart2.jpg" alt="Flow Chart, Part 2"><br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; text-align: center;">
      <em>Flow chart (part 2) illustrating how values are assigned
      to elements or cells.</em><br>
      </td>
    </tr>
  </tbody>
</table>
)

@latex(\begin{figure}[h]
\centering
\includegraphics{GoPhastFlowChart2.jpg}
\caption{Flow chart (part 2) illustrating how values are assigned
to elements or cells.}
\end{figure})

    *)
    procedure Initialize; override;
    // @name calls @inherited @name and then specifies the
    // @link(MixtureUseList) as being out of date.
    procedure Invalidate; override;
    // @name specifies whether the cell at ALay, ARow, ACol
    // uses PHAST-style interpolation.
    property IsInterpolatedCell[const ALay, ARow, ACol: integer]: boolean read
      GetIsInterpolatedCell write SetIsInterpolatedCell;
    procedure RefreshFormula; override;
  published
    // @name is the first distance used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Distance1: double read GetDistance1 write SetDistance1;
    // @name is the second distance used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Distance2: double read GetDistance2 write SetDistance2;
    // @name is the @link(TInterpolationDirection) used for
    // PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property InterpolationDirection: TInterpolationDirection read
      FInterpolationDirection write SetInterpolationDirection;
    // @name is the formula used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set
    // and @link(InterpolationDirection) = pidMix.
    property MixtureFormula: string read GetMixtureFormula
      write SetMixtureFormula;
    // @name specifies whether PHAST-style interpolation
    // should be applied to all cells.
    property UsePHAST_InterpolationForAllCells: boolean
      read FUsePHAST_InterpolationForAllCells
      write SetUsePHAST_InterpolationForAllCells;
  end;

  //  @abstract(@name is the abstract ancestor of @link(TDataArray)s that use
  //   use PHAST-style interpolation and store data in arrays.)
  // @name is used mainly for aquifer properties
  // because it stores data at every grid locations.
  // It stores most of the data required for PHAST style interpolation.
  // Its descendants store the rest of the data depending on whether integer
  // or real numbers are to be stored.
  //
  // @name is never used directly, it is used as a
  // common ancestor of other classes.
  TArrayPhastDataSet = class(TCustomPhastDataSet)
  private
    // @name : array of array of array of double;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    FCellDistance1: array of array of array of double;
    // @name : array of array of array of double;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    FCellDistance2: array of array of array of double;
    // @name : array of array of array of @link(TInterpolationDirection);
    // See TCustomPhastDataSet.
    // @link(TCustomPhastDataSet.CellInterpolationDirection).
    FCellInterpolationDirection: array of array of array of
      TInterpolationDirection;
    // @name : array of array of array of boolean;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    FIsInterpolatedCell: array of array of array of boolean;
  protected
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    function GetCellDistance1(const ALay, ARow, ACol: integer): double;
      override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    function GetCellDistance2(const ALay, ARow, ACol: integer): double;
      override;
    // See TCustomPhastDataSet.
    // @link(TCustomPhastDataSet.CellInterpolationDirection).
    function GetCellInterpolationDirection(const ALay, ARow, ACol: integer):
      TInterpolationDirection; override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    function GetIsInterpolatedCell(const ALay, ARow, ACol: integer): boolean;
      override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    procedure SetCellDistance1(const ALay, ARow, ACol: integer;
      const Value: double); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    procedure SetCellDistance2(const ALay, ARow, ACol: integer;
      const Value: double); override;
    // See TCustomPhastDataSet.
    // @link(TCustomPhastDataSet.CellInterpolationDirection).
    procedure SetCellInterpolationDirection(const ALay, ARow, ACol: integer;
      const Value: TInterpolationDirection); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    procedure SetIsInterpolatedCell(const ALay, ARow, ACol: integer;
      const Value: boolean); override;
    // @name sets @link(TCustomPhastDataSet.CellDistance1),
    // @link(TCustomPhastDataSet.CellDistance2),
    // @link(TCustomPhastDataSet.CellInterpolationDirection), and
    // @link(TCustomPhastDataSet.IsInterpolatedCell),
    // for every cell to the corresponding values of
    // @name sets @link(TCustomPhastDataSet.Distance1),
    // @link(TCustomPhastDataSet.Distance2),
    // @link(TCustomPhastDataSet.InterpolationDirection), and
    // @link(TCustomPhastDataSet.UsePHAST_InterpolationForAllCells).
    // It also sets
    // @link(TDataArray.Annotation) for every cell.
    procedure InitializePhastArrays; override;
    // @name sets the dimensions of the arrays used to store data.
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name sets @link(TCustomPhastDataSet.IsInterpolatedCell)
    // to false for every cell.
    procedure ResetCellsPhastInterpolation; override;
  end;

  // @abstract(@name is a @link(TArrayPhastDataSet) that stores integers.)
  TIntegerPhastDataSet = class(TArrayPhastDataSet)
  private
    // @name: array of array of array of integer;
    // See @link(CellValue1).
    FCellValue1: array of array of array of integer;
    // @name: array of array of array of integer;
    // See @link(CellValue2).
    FCellValue2: array of array of array of integer;
    // @name: array of array of array of double;
    // See @link(Fraction).
    FFraction: array of array of array of double;
    // @name: integer;
    // See @link(Value1).
    FValue1: integer;
    // @name: integer;
    // See @link(Value2).
    FValue2: integer;
    // See @link(CellValue1).
    function GetCellValue1(const ALay, ARow, ACol: integer): integer;
    // See @link(CellValue2).
    function GetCellValue2(const ALay, ARow, ACol: integer): integer;
    // See @link(Fraction).
    function GetFraction(const ALay, ARow, ACol: integer): double;
    // See @link(RealValue).
    function GetRealValue(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue1).
    procedure SetCellValue1(const ALay, ARow, ACol, Value: integer);
    // See @link(CellValue2).
    procedure SetCellValue2(const ALay, ARow, ACol, Value: integer);
    // See @link(Fraction).
    procedure SetFraction(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(Value1).
    procedure SetValue1(const Value: integer);
    // See @link(Value2).
    procedure SetValue2(const Value: integer);
  protected
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure StoreData(Stream: TStream); override;
    // @name returns @link(RealValue).
    function GetRealData(const Layer, Row, Col: integer): double; override;
    // @name sets @link(TDataArray.IntegerData), and @link(Fraction),
    // for every cell.
    procedure InitializePhastArrays; override;
    // @name checks that the TRbwDataType represents an integer.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name sets the dimensions of the arrays.
    procedure SetDimensions(const SetToZero: boolean); override;
  public
    // @name copies Source to the @classname.  If Source is a @classname
    // or a @link(TPhastInterpolationValues), PHAST-style interpolation data
    // are copied.
    procedure Assign(Source: TPersistent); override;
    // @name represents the first value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue1[const ALay, ARow, ACol: integer]: integer read
      GetCellValue1 write SetCellValue1;
    // @name represents the second value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue2[const ALay, ARow, ACol: integer]: integer read
      GetCellValue2 write SetCellValue2;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name represents the fraction of the first value
    // at each location in the data set.
    property Fraction[const ALay, ARow, ACol: integer]: double read GetFraction
      write SetFraction;
    // @name is a representation of the data as a real number.
    property RealValue[const ALay, ARow, ACol: integer]: double read
      GetRealValue;
    // @name returns true.
    function DisplayRealValue: boolean; override;
  published
    // @name is the first value used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Value1: integer read FValue1 write SetValue1;
    // @name is the second value used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Value2: integer read FValue2 write SetValue2;
  end;

  // @abstract(@name is a @link(TArrayPhastDataSet) that stores real numbers.)
  TRealPhastDataSet = class(TArrayPhastDataSet)
  private
    // @name: array of array of array of double;
    // See @link(CellValue1).
    FCellValue1: array of array of array of double;
    // @name: array of array of array of double;
    // See @link(CellValue2).
    FCellValue2: array of array of array of double;
    // @name: double;
    // See @link(Value1).
    FValue1: double;
    // @name: double;
    // See @link(Value2).
    FValue2: double;
    // See @link(CellValue1).
    function GetCellValue1(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue2).
    function GetCellValue2(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue1).
    procedure SetCellValue1(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(CellValue2).
    procedure SetCellValue2(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(Value1).
    procedure SetValue1(const Value: double);
    // See @link(Value2).
    procedure SetValue2(const Value: double);
  protected
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure StoreData(Stream: TStream); override;
    // @name sets @link(TDataArray.RealData) for each cell in the grid.
    procedure InitializePhastArrays; override;
    // @name checks that the TRbwDataType represents a real number.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name sets the dimensions of the arrays.
    procedure SetDimensions(const SetToZero: boolean); override;
  public
    // @name copies Source to the @classname.  If Source is a @classname
    // or a @link(TPhastInterpolationValues), PHAST-style interpolation data
    // are copied.
    procedure Assign(Source: TPersistent); override;
    // @name represents the first value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue1[const ALay, ARow, ACol: integer]: double read
      GetCellValue1 write SetCellValue1;
    // @name represents the second value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue2[const ALay, ARow, ACol: integer]: double read
      GetCellValue2 write SetCellValue2;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
  published
    // @name is the first value used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Value1: double read FValue1 write SetValue1;
    // @name is the second value used for PHAST-style interpolation
    // when PHAST-style interpolation is applied to the entire data set.
    property Value2: double read FValue2 write SetValue2;
  end;

  TPhastTimeList = class;

  // @abstract(@name is a abstract ancestor for sparse data set.
  // It is used mainly for boundary conditions
  // because it doesn't store data at locations where no data is needed.)
  // It stores most of the data required for PHAST style interpolation.
  // Its descendants store the rest of the data depending on whether integer
  // or real numbers are to be stored.
  //
  // @name is never used directly, it is used as a
  // common ancestor of other classes.
  //
  // Most @name are managed by a @link(TPhastTimeList).
  // Because they are created and destroyed frequently, they
  // generally should not TalkTo or be TalkedTo to.
  // (See TObserver.@link(TObserver.TalksTo).)  Instead
  // notify the @link(TPhastTimeList) via the @link(TCustomTimeList.Invalidate)
  // method of the @link(TPhastTimeList) that owns the @name
  // or TObserver.@link(TObserver.OnUpToDateSet)
  TSparseArrayPhastInterpolationDataSet = class(TCustomPhastDataSet)
  private
    // See TCustomPhastDataSet.@link(TDataArray.Annotation).
    FAnnotation: T3DSparseStringArray;
    // See @link(BoundaryDataType).
    FBoundaryDataType: TDataArray;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    FCellDistance1: T3DSparseRealArray;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    FCellDistance2: T3DSparseRealArray;
    // See TCustomPhastDataSet.
    // @link(TCustomPhastDataSet.InterpolationDirection).
    FCellInterpolationDirection: T3DSparseInterpolationDirectionArray;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    FIsInterpolatedCell: T3DSparseBooleanArray;
  protected
    // See TCustomPhastDataSet.@link(TDataArray.Annotation).
    function GetAnnotation(const Layer, Row, Col: integer): string; override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    function GetCellDistance1(const ALay, ARow, ACol: integer): double;
      override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    function GetCellDistance2(const ALay, ARow, ACol: integer): double;
      override;
    // See TCustomPhastDataSet.
    // @link(TCustomPhastDataSet.InterpolationDirection).
    function GetCellInterpolationDirection(const ALay, ARow, ACol: integer):
      TInterpolationDirection; override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    function GetIsInterpolatedCell(const ALay, ARow, ACol: integer): boolean;
      override;
    // See TCustomPhastDataSet.@link(TDataArray.IsValue).
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name clears
    // TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    procedure ResetCellsPhastInterpolation; override;
    // See TCustomPhastDataSet.@link(TDataArray.Annotation).
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance1).
    procedure SetCellDistance1(const ALay, ARow, ACol: integer;
      const Value: double); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.CellDistance2).
    procedure SetCellDistance2(const ALay, ARow, ACol: integer;
      const Value: double); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.InterpolationDirection).
    procedure SetCellInterpolationDirection(const ALay, ARow, ACol: integer;
      const Value: TInterpolationDirection); override;
    // @name clears all the sparse data arrays.
    procedure SetDimensions(const SetToZero: boolean); override;
    // See TCustomPhastDataSet.@link(TCustomPhastDataSet.IsInterpolatedCell).
    procedure SetIsInterpolatedCell(const ALay, ARow, ACol: integer;
      const Value: boolean); override;
    // See TCustomPhastDataSet.@link(TDataArray.IsValue).
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); override;
    function IsSparseArray: boolean; override;
  public
    // @name is the data set used to indicate what type of boundary
    // is at a specific location.
    property BoundaryDataType: TDataArray read FBoundaryDataType
      write FBoundaryDataType;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns values to all the cells that ought to have values
    // assigned.
    procedure Initialize; override;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  {@abstract(@name is a sparse data set that stores real numbers.  It
    is used mainly for boundary conditions.)
    See @link(TSparseArrayPhastInterpolationDataSet).}
  TSparseRealPhastDataSet = class(TSparseArrayPhastInterpolationDataSet)
  private
    // @name: @link(T3DSparseRealArray);
    // See @link(CellValue1).
    FCellValue1: T3DSparseRealArray;
    // @name: @link(T3DSparseRealArray);
    // See @link(CellValue2).
    FCellValue2: T3DSparseRealArray;
    // @name: @link(T3DSparseRealArray);
    // @name stores the values assigned to cells or elements
    // when PHAST-style interpolation is not used.
    FRealValues: T3DSparseRealArray;
    // @name: double;
    // See @link(Value1).
    FValue1: double;
    // @name: double;
    // See @link(Value2).
    FValue2: double;
    // See @link(CellValue1).
    function GetCellValue1(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue2).
    function GetCellValue2(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue1).
    procedure SetCellValue1(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(CellValue2).
    procedure SetCellValue2(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(Value1).
    procedure SetValue1(const Value: double);
    // See @link(Value2).
    procedure SetValue2(const Value: double);
  protected
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure StoreData(Stream: TStream); override;
    // See TCustomPhastDataSet.@link(TDataArray.IsValue).
    function GetIsValue(const Index1, Index2, Index3: Integer): boolean;
      override;
    // @name returns the values assigned to cells
    // when PHAST-style interpolation
    // is not used.
    function GetRealData(const Layer, Row, Col: integer): double; override;
    // @name Checks that the TRbwDataType represents a real number.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears the sparse arrays used to store data.
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name sets the values assigned to cells when PHAST-style interpolation
    // is not used.
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); override;
  public
    // @name copies Source to the @classname. If Source is a @classname
    // or a @link(TPhastInterpolationValues), PHAST-style interpolation data
    // are copied.
    procedure Assign(Source: TPersistent); override;
    // @name represents the first value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue1[const ALay, ARow, ACol: integer]: double read
      GetCellValue1 write SetCellValue1;
    // @name represents the second value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue2[const ALay, ARow, ACol: integer]: double read
      GetCellValue2 write SetCellValue2;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  published
    // @name represents the global value of the
    // first value used for PHAST-style interpolation.
    // @name is the default first value assigned to
    // @link(ScreenObjectUnit.TScreenObject)s
    // when PHAST-style interpolation is first activated.
    property Value1: double read FValue1 write SetValue1;
    // @name represents the global value of the
    // second value used for PHAST-style interpolation.
    // @name is the default second value assigned to
    // @link(ScreenObjectUnit.TScreenObject)s
    // when PHAST-style interpolation is first activated.
    property Value2: double read FValue2 write SetValue2;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  {@abstract(@name is a sparse data set that stores integers.  It
    is used mainly for boundary conditions.)
    See @link(TSparseArrayPhastInterpolationDataSet).}
  TSparseIntegerPhastDataSet = class(TSparseArrayPhastInterpolationDataSet)
  private
    // @name: @link(T3DSparseIntegerArray);
    // See @link(CellValue1).
    FCellValue1: T3DSparseIntegerArray;
    // @name: @link(T3DSparseIntegerArray);
    // See @link(CellValue2).
    FCellValue2: T3DSparseIntegerArray;
    // @name: @link(T3DSparseRealArray);
    // See @link(Fraction).
    FFraction: T3DSparseRealArray;
    // @name: @link(T3DSparseIntegerArray);
    // @name stores the values assigned to cells or elements
    // when PHAST-style interpolation is not used.
    FIntegerValues: T3DSparseIntegerArray;
    // @name: integer;
    // See @link(Value1).
    FValue1: integer;
    // @name: integer;
    // See @link(Value2).
    FValue2: integer;
    // See @link(CellValue1).
    function GetCellValue1(const ALay, ARow, ACol: integer): integer;
    // See @link(CellValue2).
    function GetCellValue2(const ALay, ARow, ACol: integer): integer;
    // See @link(Fraction).
    function GetFraction(const ALay, ARow, ACol: integer): double;
    // @name is a representation of the data as a real number.
    function GetRealValue(const ALay, ARow, ACol: integer): double;
    // See @link(CellValue1).
    procedure SetCellValue1(const ALay, ARow, ACol, Value: integer);
    // See @link(CellValue2).
    procedure SetCellValue2(const ALay, ARow, ACol, Value: integer);
    // See @link(Fraction).
    procedure SetFraction(const ALay, ARow, ACol: integer;
      const Value: double);
    // See @link(Value1).
    procedure SetValue1(const Value: integer);
    // See @link(Value2).
    procedure SetValue2(const Value: integer);
  protected
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure StoreData(Stream: TStream); override;
    // See @link(TDataArray.IntegerData).
    function GetIntegerData(const Layer, Row, Col: integer): integer; override;
    // See TCustomPhastDataSet.@link(TDataArray.IsValue).
    function GetIsValue(const Index1, Index2, Index3: Integer): boolean;
      override;
    // @name represents the data as a real number.
    function GetRealData(const Layer, Row, Col: integer): double; override;
    // @name checks that the TRbwDataType represents an integer.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears the sparse arrays used to store data.
    procedure SetDimensions(const SetToZero: boolean); override;
    // See @link(TDataArray.IntegerData).
    procedure SetIntegerData(const Layer, Row, Col, Value: integer); override;
  public
    // @name copies Source to the @classname. If Source is a @classname
    // or a @link(TPhastInterpolationValues), PHAST-style interpolation data
    // are copied.
    procedure Assign(Source: TPersistent); override;
    // @name represents the first value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue1[const ALay, ARow, ACol: integer]: integer read
      GetCellValue1 write SetCellValue1;
    // @name represents the second value used for PHAST-style interpolation
    // at each location in the data set.
    property CellValue2[const ALay, ARow, ACol: integer]: integer read
      GetCellValue2 write SetCellValue2;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name returns true.
    function DisplayRealValue: boolean; override;
    // @name represents the fraction of @link(CellValue1) to be applied
    // at ALay, ARow, ACol.
    property Fraction[const ALay, ARow, ACol: integer]: double read GetFraction
      write SetFraction;
    // @name represents the data as a real number.
    property RealValue[const ALay, ARow, ACol: integer]: double read
      GetRealValue;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  published
    // @name represents the global value of the
    // first value used for PHAST-style interpolation.
    // @name is the default first value assigned to
    // @link(ScreenObjectUnit.TScreenObject)s
    // when PHAST-style interpolation is first activated.
    property Value1: integer read FValue1 write SetValue1;
    // @name represents the global value of the
    // second value used for PHAST-style interpolation.
    // @name is the default second value assigned to
    // @link(ScreenObjectUnit.TScreenObject)s
    // when PHAST-style interpolation is first activated.
    property Value2: integer read FValue2 write SetValue2;
  end;

  {@abstract(@name is used to read and store @link(TPhastInterpolationValues).)}
  TInterpValuesItem = class(TCollectionItem)
  private
    // @name: @link(TPhastInterpolationValues);
    // See @link(Values).
    FValues: TPhastInterpolationValues;
    // See @link(Values).
    procedure SetValues(const Value: TPhastInterpolationValues);
  public
    // if Source is a @classname, @name copies Source to the current instance
    // of @classname.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name is the @link(TPhastInterpolationValues) stored by the @classname.
    property Values: TPhastInterpolationValues read FValues write SetValues;
  end;

  {@abstract(@name is used to store
    a collection of @link(TPhastInterpolationValues).)}
  TInterpValuesCollection = class(TPhastCollection)
  private
    // See @link(ItemOfDataSet).
    function GetItemOfDataSet(const DataSet: TDataArray): TInterpValuesItem;
  public
    // @name creates an instance of @classname.
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // @name provides a way to retrieve the @link(TInterpValuesItem)
    // associated with a particular data set.
    property ItemOfDataSet[const DataSet: TDataArray]: TInterpValuesItem
      read GetItemOfDataSet;
  end;

  // @abstract(@name is a TList to which only @link(TIntegerSparseDataSet) can
  // be added.  Adding a @link(TIntegerSparseDataSet) to this list
  // will set
  // TIntegerSparseDataSet.@link(TIntegerSparseDataSet.IsBoundaryTypeDataSet)
  // to true.
  // @name is used in @link(TPhastTimeList).)
  TBoundaryTypeList = class(TList)
    function Add(const DataSet: TIntegerSparseDataSet): integer;
  end;

  {@abstract(@name is used to manage boundary condition data that
    varies with time.)

    The @link(TPhastTimeList.Items) property provides access
    to a series of @link(TSparseArrayPhastInterpolationDataSet)s
    // that define how
    the data vary with time.  Each represents the boundary condition
    data at a different time.  The number of such
    @link(TSparseArrayPhastInterpolationDataSet)s is specified by the
    @link(TCustomTimeList.Count)
    property.  The @link(TCustomTimeList.Times) property gives the time
    for each @link(TSparseArrayPhastInterpolationDataSet).

    New data sets are added to @classname in
    TCustomPhastBoundaryCollection.@link(
    TCustomPhastBoundaryCollection.GetDataSet)}
  TPhastTimeList = class(TCustomTimeList)
  private
    // See @link(BoundaryType).
    FBoundaryType: TPhastBoundaryTypes;
    // See @link(BoundaryTypeDataSets).
    FBoundaryTypeDataSets: TBoundaryTypeList;

    // See @link(Items).
    function GetItems(const Index: integer):
      TSparseArrayPhastInterpolationDataSet;
    // See @link(Items).
    procedure SetItems(const Index: integer;
      const Value: TSparseArrayPhastInterpolationDataSet);
  protected
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean); override;
  public
    // @name adds a @link(TSparseArrayPhastInterpolationDataSet)
    // and its associated
    // time to the @classname.
    function Add(const ATime: double;
      const Data: TSparseArrayPhastInterpolationDataSet): integer;
    // @name specifies the type of boundary condition being specified
    // by this @classname.
    property BoundaryType: TPhastBoundaryTypes read FBoundaryType write
      FBoundaryType;
    // @name holds a series of @link(TIntegerSparseDataSet)s that indicate
    // the type of boundary condition that applies at each location.
    property BoundaryTypeDataSets: TBoundaryTypeList read FBoundaryTypeDataSets;

    // @name removes all the @link(TSparseArrayPhastInterpolationDataSet)s from
    // the @classname.
    procedure Clear; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    constructor Create(Model: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name initializes all the @link(TSparseArrayPhastInterpolationDataSet)s
    // related to the @classname.
    procedure Initialize; override;
    // @name provides access to the
    // @link(TSparseArrayPhastInterpolationDataSet)s
    // stored in @classname.
    property Items[const Index: integer]: TSparseArrayPhastInterpolationDataSet
      read GetItems write SetItems;
    // @name is called after the model is loaded. It updates subscriptions.
    procedure Loaded;
  end;

implementation

uses Contnrs, ScreenObjectUnit, frmGoPhastUnit, frmDataSetsUnits,
  GIS_Functions, frmFormulaErrorsUnit, SubscriptionUnit, PhastModelUnit,
  ModelMuseUtilities;

resourcestring
  StrSetByPHASTstyleI = 'Set by PHAST-style interpolation';
  StrMixtureFormulaFor = 'Mixture formula for: %s';
  StrTheMixtureFormula = 'The mixture formula for %0:s returns a value of th' +
  'e wrong type. The formula is %1:s.';

{ T3DSparseInterpolationDirectionArray }

procedure T3DSparseInterpolationDirectionArray.Clear;
begin
  inherited;
  FCount := 0;
end;

constructor T3DSparseInterpolationDirectionArray.Create(
  Quantum1, Quantum2, Quantum3: TSPAQuantum);
begin
  inherited;
  SetLength(FValues, 4);
end;

function T3DSparseInterpolationDirectionArray.GetItems(const Index1,
  Index2, Index3: Integer): TInterpolationDirection;
var
  resultPtr: Pointer;
begin
  resultPtr := inherited Items[Index1, Index2, Index3];
  Assert(resultPtr <> nil);
  result := FValues[Pred(longint(resultPtr))];
end;

procedure T3DSparseInterpolationDirectionArray.SetItems(const Index1,
  Index2, Index3: Integer; const Value: TInterpolationDirection);
var
  DataPtr: Pointer;
begin
  DataPtr := inherited Items[Index1, Index2, Index3];
  if DataPtr = nil then
  begin
    if Length(FValues) = FCount then
    begin
      SetLength(FValues, FCount + FCount div 4);
    end;
    FValues[FCount] := Value;
    Inc(FCount);
    DataPtr := Pointer(FCount);
    inherited Items[Index1, Index2, Index3] := DataPtr;
  end
  else
  begin
    FValues[Pred(longint(DataPtr))] := Value
  end;
end;

{ TPhastInterpolationValues }

procedure TPhastInterpolationValues.Assign(Source: TPersistent);
var
  CSource: TCustomPhastDataSet;
  ISource: TIntegerPhastDataSet;
  RSource: TRealPhastDataSet;
  SparseISource: TSparseIntegerPhastDataSet;
  SparseRSource: TSparseRealPhastDataSet;
  PhastSource: TPhastInterpolationValues;
  PhastBoundary: TCustomPhastBoundaryCondition;
  RPhastBoundary: TRealPhastBoundaryCondition;
  IPhastBoundary: TIntegerPhastBoundaryCondition;
begin
  if Source is TCustomPhastDataSet then
  begin
    CSource := TCustomPhastDataSet(Source);
    Assert((FDataSet = nil) or (FDataSet = CSource));
    FDataSet := CSource;
    Distance1 := CSource.Distance1;
    Distance2 := CSource.Distance2;
    InterpolationDirection := CSource.InterpolationDirection;
    UsePHAST_Interpolation := CSource.UsePHAST_InterpolationForAllCells;
    MixtureFormula := CSource.MixtureFormula;
    if Source is TIntegerPhastDataSet then
    begin
      ISource := TIntegerPhastDataSet(Source);
      IntValue1 := ISource.Value1;
      IntValue2 := ISource.Value2;
    end
    else if Source is TRealPhastDataSet then
    begin
      RSource := TRealPhastDataSet(Source);
      RealValue1 := RSource.Value1;
      RealValue2 := RSource.Value2;
    end
    else if Source is TSparseIntegerPhastDataSet then
    begin
      SparseISource := TSparseIntegerPhastDataSet(Source);
      IntValue1 := SparseISource.Value1;
      IntValue2 := SparseISource.Value2;
    end
    else if Source is TSparseRealPhastDataSet then
    begin
      SparseRSource := TSparseRealPhastDataSet(Source);
      RealValue1 := SparseRSource.Value1;
      RealValue2 := SparseRSource.Value2;
    end
    else
    begin
      Assert(False);
    end;
  end
  else if Source is TCustomPhastBoundaryCondition then
  begin
    PhastBoundary := TCustomPhastBoundaryCondition(Source);
    Assert((FDataSet = nil) or (FDataSet = PhastBoundary.GetDataSet));
    FDataSet := PhastBoundary.GetDataSet;
    Distance1 := PhastBoundary.Distance1;
    Distance2 := PhastBoundary.Distance2;
    InterpolationDirection := PhastBoundary.InterpolationDirection;
    UsePHAST_Interpolation := PhastBoundary.UsePHAST_Interpolation;
    if Source is TRealPhastBoundaryCondition then
    begin
      RPhastBoundary := TRealPhastBoundaryCondition(Source);
      RealValue1 := RPhastBoundary.Value1;
      RealValue2 := RPhastBoundary.Value2;
    end
    else if Source is TIntegerPhastBoundaryCondition then
    begin
      IPhastBoundary := TIntegerPhastBoundaryCondition(Source);
      IntValue1 := IPhastBoundary.Value1;
      IntValue2 := IPhastBoundary.Value2;
      MixtureFormula := IPhastBoundary.MixtureExpression;
    end
    else
    begin
      Assert(False);
    end;
  end
  else if Source is TPhastInterpolationValues then
  begin
    PhastSource := TPhastInterpolationValues(Source);
    Assert((FDataSet = nil) or (FDataSet = PhastSource.FDataSet));
    FDataSet := PhastSource.FDataSet;
    Distance1 := PhastSource.Distance1;
    Distance2 := PhastSource.Distance2;
    InterpolationDirection := PhastSource.InterpolationDirection;
    UsePHAST_Interpolation := PhastSource.UsePHAST_Interpolation;
    IntValue1 := PhastSource.IntValue1;
    IntValue2 := PhastSource.IntValue2;
    RealValue1 := PhastSource.RealValue1;
    RealValue2 := PhastSource.RealValue2;
    MixtureFormula := PhastSource.MixtureFormula;
  end
  else
  begin
    inherited;
  end;
  frmGoPhast.InvalidateModel;
end;

procedure TPhastInterpolationValues.AssignTo(Dest: TPersistent);
var
  CSource: TCustomPhastDataSet;
  ISource: TIntegerPhastDataSet;
  RSource: TRealPhastDataSet;
  SparseISource: TSparseIntegerPhastDataSet;
  SparseRSource: TSparseRealPhastDataSet;
begin
  if Dest is TCustomPhastDataSet then
  begin
    CSource := TCustomPhastDataSet(Dest);
    CSource.Distance1 := Distance1;
    CSource.Distance2 := Distance2;
    CSource.InterpolationDirection := InterpolationDirection;
    CSource.UsePHAST_InterpolationForAllCells := UsePHAST_Interpolation;
    CSource.MixtureFormula := MixtureFormula;
    if Dest is TIntegerPhastDataSet then
    begin
      ISource := TIntegerPhastDataSet(Dest);
      ISource.Value1 := IntValue1;
      ISource.Value2 := IntValue2;
    end
    else if Dest is TRealPhastDataSet then
    begin
      RSource := TRealPhastDataSet(Dest);
      RSource.Value1 := RealValue1;
      RSource.Value2 := RealValue2;
    end
    else if Dest is TSparseIntegerPhastDataSet then
    begin
      SparseISource := TSparseIntegerPhastDataSet(Dest);
      SparseISource.Value1 := IntValue1;
      SparseISource.Value2 := IntValue2;
    end
    else if Dest is TSparseRealPhastDataSet then
    begin
      SparseRSource := TSparseRealPhastDataSet(Dest);
      SparseRSource.Value1 := RealValue1;
      SparseRSource.Value2 := RealValue2;
    end
    else
    begin
      inherited;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPhastInterpolationValues.Create;
begin
  inherited;
  FMixtureFormula := frmGoPhast.PhastModel.FormulaManager.Add;
  FMixtureFormula.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
end;

destructor TPhastInterpolationValues.Destroy;
begin
  if frmGoPhast.PhastModel <> nil then
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FMixtureFormula, nil, nil, self);
  end;
  inherited;
end;

function TPhastInterpolationValues.GetDistance1: double;
begin
  if FInterpolationDirection = pidMix then
  begin
    result := 0;
  end
  else
  begin
    result := FDistance1;
  end;
end;

function TPhastInterpolationValues.GetDistance2: double;
begin
  if FInterpolationDirection = pidMix then
  begin
    result := 1;
  end
  else
  begin
    result := FDistance2;
  end;
end;

function TPhastInterpolationValues.GetMixtureFormula: string;
begin
  result := FMixtureFormula.Formula;
end;

function TPhastInterpolationValues.SameAs(
  Another: TPhastInterpolationValues): boolean;
begin
  result := (Distance1 = Another.Distance1)
    and (Distance2 = Another.Distance2)
    and (InterpolationDirection = Another.InterpolationDirection)
    and (IntValue1 = Another.IntValue1)
    and (IntValue2 = Another.IntValue2)
    and (MixtureFormula = Another.MixtureFormula)
    and (RealValue1 = Another.RealValue1)
    and (RealValue2 = Another.RealValue2)
    and (UsePHAST_Interpolation = Another.UsePHAST_Interpolation)
end;

procedure TPhastInterpolationValues.SetDistance1(const Value: double);
begin
  if FDistance1 <> Value then
  begin
    FDistance1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetDistance2(const Value: double);
begin
  if FDistance2 <> Value then
  begin
    FDistance2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetInterpolationDirection(
  const Value: TInterpolationDirection);
begin
  if FInterpolationDirection <> Value then
  begin
    FInterpolationDirection := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetIntValue1(const Value: integer);
begin
  if FIntValue1 <> Value then
  begin
    FIntValue1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetIntValue2(const Value: integer);
begin
  if FIntValue2 <> Value then
  begin
    FIntValue2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetMixtureFormula(
  const Value: string);
begin
  if FMixtureFormula.Formula <> Value then
  begin
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FMixtureFormula, Value,
      frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes,
      nil, nil, self);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetRealValue1(const Value: double);
begin
  if FRealValue1 <> Value then
  begin
    FRealValue1 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetRealValue2(const Value: double);
begin
  if FRealValue2 <> Value then
  begin
    FRealValue2 := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPhastInterpolationValues.SetUsePHAST_Interpolation(
  const Value: boolean);
begin
  if FUsePHAST_Interpolation <> Value then
  begin
    FUsePHAST_Interpolation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TCustomPhastDataSet }

procedure TCustomPhastDataSet.CheckIfUniform;
var
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  FIsUniform := iuUnknown;
  if Model.ModelSelection = msPhast then
  begin
    GetLimits(ColLimit, RowLimit, LayerLimit);
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            if IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
            begin
              FIsUniform := iuFalse;
              Exit;
            end;
          end
          else
          begin
            FIsUniform := iuFalse;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited;
end;

constructor TCustomPhastDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FMixtureFormulaObject := TPhastModel(AnOwner).FormulaManager.Add;
  FMixtureFormulaObject.AddSubscriptionEvents(
    GlobalDataArrayRemoveSubscription,
    GlobalDataArrayRestoreSubscription, self);
  Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  FMixtureUseList := TStringList.Create;
end;

procedure TCustomPhastDataSet.Initialize;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  FreeStack: boolean;
  Position: integer;
  ShouldCheck: Boolean;
  StackIndex: Integer;
begin
  if UpToDate and not DimensionsChanged then
  begin
    CheckRestoreData;
    Exit;
  end;
//  FEvalTime := Now;
  FDataCached := False;

  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      Stack[0] := Stack[0] + ' depends on';
      for StackIndex := 1 to Stack.Count - 2 do
      begin
        Stack[StackIndex] := Stack[StackIndex] + ' which depends on';
      end;
      HandleCircularReferenceError(Format(StrCircularReferenceI2, [Name, Stack.Text]), nil);
      Exit;

//      raise ECircularReference.Create(Format(StrCircularReferenceI, [Name]));
    end;
    Position := Stack.Add(Name);

    ShouldCheck := False;
    if Assigned(OnInitialize) then
    begin
      if Assigned(OnShouldUseOnInitialize) then
      begin
        OnShouldUseOnInitialize(self,ShouldCheck);
      end
      else
      begin
        ShouldCheck := True;
      end;
    end;
    if ShouldCheck then
    begin
      OnInitialize(Self);
    end
    else
    begin
      if UsePHAST_InterpolationForAllCells then
      begin
        if DimensionsChanged then
        begin
          SetDimensions(False);
        end
        else
        begin
          RestoreArraySize;
          // where needed, InitializePhastArrays is called from SetDimensions
          // in descendants of TCustomPhastDataSet;
          InitializePhastArrays;
        end;
        for ScreenObjectIndex := 0 to
          frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
          if not AScreenObject.Deleted then
          begin
            AScreenObject.AssignValuesToDataSet(self, FModel, lctUse);
          end;
        end;

        PostInitialize;

        UpToDate := True;
        CheckIfUniform;
      end
      else
      begin
        if DimensionsChanged then
        begin
          SetDimensions(False);
        end
        else
        begin
          RestoreArraySize;
        end;
        ResetCellsPhastInterpolation;
        Stack.Delete(Position);
        inherited;
      end;
    end;
  finally
    if FreeStack then
    begin
      FreeAndNil(Stack);
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    end;
  end;
end;

procedure TCustomPhastDataSet.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IsInterpolated: boolean;
  Distance: double;
  Direction: TInterpolationDirection;
begin
  inherited;
  if Model.ModelSelection = msPhast then
  begin
    DecompressionStream.Read(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
    DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
    DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
    DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(IsInterpolated, SizeOf(IsInterpolated));
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] := IsInterpolated;
      if IsInterpolated then
      begin
        DecompressionStream.Read(Distance, SizeOf(Distance));
        CellDistance1[LayerIndex, RowIndex, ColIndex] := Distance;
        DecompressionStream.Read(Distance, SizeOf(Distance));
        CellDistance2[LayerIndex, RowIndex, ColIndex] := Distance;
        DecompressionStream.Read(Direction, SizeOf(Direction));
        CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] := Direction;
      end;
    end;
  end;
end;

procedure TCustomPhastDataSet.RefreshFormula;
begin
  inherited;
  FMixtureFormula := MixtureFormula;
end;

procedure TCustomPhastDataSet.ResetMixtureExpression(const Compiler: TRbwParser;
  const ErrorMessage: string);
var
  TempFormula: string;
begin
  TempFormula := MixtureFormula;
  frmFormulaErrors.AddFormulaError('', Name, TempFormula, ErrorMessage);
  TempFormula := FortranFloatToStr(0.5);
  FMixtureFormula := TempFormula;
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FMixtureFormulaObject, FMixtureFormula, GetCompiler,
    GlobalDataArrayRemoveSubscription,
    GlobalDataArrayRestoreSubscription, self);
  Compiler.Compile(TempFormula);
end;

procedure TCustomPhastDataSet.SetDistance1(const Value: double);
begin
  if FDistance1 <> Value then
  begin
    FDistance1 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastDataSet.SetDistance2(const Value: double);
begin
  if FDistance2 <> Value then
  begin
    FDistance2 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastDataSet.SetEvaluatedAt(const Value: TEvaluatedAt);
begin
  inherited;
  FMixtureFormulaObject.Parser := GetCompiler;
end;

procedure TCustomPhastDataSet.SetInterpolationDirection(
  const Value: TInterpolationDirection);
begin
  if FInterpolationDirection <> Value then
  begin
    FInterpolationDirection := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastDataSet.SetMixtureFormula(const Value: string);
begin
  FMixtureFormula := MixtureFormula;
  ChangeAFormula(Value, FMixtureFormula, FMixtureUseListUpToDate,
    GetMixtureUseList);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FMixtureFormulaObject, FMixtureFormula, GetCompiler,
    GlobalDataArrayRemoveSubscription,
    GlobalDataArrayRestoreSubscription, self);
end;

procedure TCustomPhastDataSet.SetOrientation(const Value: TDataSetOrientation);
begin
  inherited;
  FMixtureFormulaObject.Parser := GetCompiler;
end;

procedure TCustomPhastDataSet.SetUsePHAST_InterpolationForAllCells(
  const Value: boolean);
begin
  if FUsePHAST_InterpolationForAllCells <> Value then
  begin
    FUsePHAST_InterpolationForAllCells := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TCustomPhastDataSet.StoreData(Stream: TStream);
var
  LayerLimit: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
  Count: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IsInterp: boolean;
  Distance: double;
  Direction: TInterpolationDirection;
begin
  inherited;
  if Model.ModelSelection = msPhast then
  begin
    CountValues(LayerLimit, RowLimit, ColLimit, Count);
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Stream.Write(LayerIndex, SizeOf(LayerIndex));
              Stream.Write(RowIndex, SizeOf(RowIndex));
              Stream.Write(ColIndex, SizeOf(ColIndex));
              IsInterp := IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
              Stream.Write(IsInterp, SizeOf(IsInterp));
              if IsInterp then
              begin
                Distance := CellDistance1[LayerIndex, RowIndex, ColIndex];
                Stream.Write(Distance, SizeOf(Distance));
                Distance := CellDistance2[LayerIndex, RowIndex, ColIndex];
                Stream.Write(Distance, SizeOf(Distance));
                Direction := CellInterpolationDirection[LayerIndex, RowIndex, ColIndex];
                Stream.Write(Direction, SizeOf(Direction));
            end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomPhastDataSet.Assign(Source: TPersistent);
var
  DataSet: TCustomPhastDataSet;
  InterpValues: TPhastInterpolationValues;
begin
  if Source is TCustomPhastDataSet then
  begin
    DataSet := TCustomPhastDataSet(Source);
    Distance1 := DataSet.Distance1;
    Distance2 := DataSet.Distance2;
    InterpolationDirection := DataSet.InterpolationDirection;
    UsePHAST_InterpolationForAllCells :=
      DataSet.UsePHAST_InterpolationForAllCells;
    MixtureFormula := DataSet.MixtureFormula;
    inherited;
  end
  else if Source is TPhastInterpolationValues then
  begin
    InterpValues := TPhastInterpolationValues(Source);
    Distance1 := InterpValues.Distance1;
    Distance2 := InterpValues.Distance2;
    InterpolationDirection := InterpValues.InterpolationDirection;
    UsePHAST_InterpolationForAllCells := InterpValues.UsePHAST_Interpolation;
    MixtureFormula := InterpValues.MixtureFormula;
  end
  else
  begin
    inherited;
  end;

end;

procedure TCustomPhastDataSet.InitializePhastArrays;
begin
  // do nothing;
  // Overridden in descendants.
end;

function TCustomPhastDataSet.GetMixtureFormula: string;
begin
  result := FMixtureFormulaObject.Formula;
end;

function TCustomPhastDataSet.GetMixtureUseList: TStringList;
begin
  // MixtureUseListUpToDate is set to False in Invalidate
  // and to True in UpdateUseList.
  if not FMixtureUseListUpToDate then
  begin
    UpdateMixtureUseList;
  end;
  result := FMixtureUseList;
end;

destructor TCustomPhastDataSet.Destroy;
begin
  if (Model <> nil)
    and (not (csDestroying in Model.ComponentState))
    and not (Model as TPhastModel).Clearing then
  begin
    MixtureFormula := '0.';
  end;
  FMixtureUseList.Free;
  if Model <> nil then
  begin
    (Model as TCustomModel).FormulaManager.Remove(FMixtureFormulaObject,
      GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;
  inherited;
end;

procedure TCustomPhastDataSet.Invalidate;
begin
  inherited;
  FMixtureUseListUpToDate := False;
end;

procedure TCustomPhastDataSet.UpdateMixtureUseList;
var
  Compiler: TRbwParser;
  TempFormula: string;
begin
  Compiler := GetCompiler;
  TempFormula := FMixtureFormula;
  if TempFormula = '' then
  begin
    TempFormula := FortranFloatToStr(0.5);
  end;
  try
    Compiler.Compile(TempFormula);
    FMixtureUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
  except on ERbwParserError do
    begin
      FMixtureUseList.Clear;
      Exit;
    end;
  end;
  FMixtureUseListUpToDate := True;
end;

function TCustomPhastDataSet.GetDistance1: double;
begin
  if InterpolationDirection = pidMix then
  begin
    result := 0;
  end
  else
  begin
    result := FDistance1;
  end;
end;

function TCustomPhastDataSet.GetDistance2: double;
begin
  if InterpolationDirection = pidMix then
  begin
    result := 1;
  end
  else
  begin
    result := FDistance2;
  end;
end;

function TCustomPhastDataSet.GetUseList: TStringList;
var
  OtherList: TStringList;
begin
  result := inherited GetUseList;
  if UsePHAST_InterpolationForAllCells and (InterpolationDirection = pidMix)
    then
  begin
    OtherList := TStringList.Create;
    try
      OtherList.Assign(MixtureUseList);
      result.AddStrings(OtherList);
    finally
      OtherList.Free;
    end;
  end;
end;

{ TArrayPhastDataSet }

function TArrayPhastDataSet.GetCellDistance1(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellDistance1[ALay, ARow, ACol];
end;

function TArrayPhastDataSet.GetCellDistance2(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellDistance2[ALay, ARow, ACol];
end;

function TArrayPhastDataSet.GetCellInterpolationDirection(const ALay,
  ARow, ACol: integer): TInterpolationDirection;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellInterpolationDirection[ALay, ARow, ACol];
end;

function TArrayPhastDataSet.GetIsInterpolatedCell(const ALay, ARow,
  ACol: integer): boolean;
begin
  if IsUniform = iuTrue then
  begin
    result := False;
    Exit;
  end;
  if DimensionsChanged then
    SetDimensions(False);
  result := FIsInterpolatedCell[ALay, ARow, ACol];
//  UpdateEvalTime;
end;

procedure TArrayPhastDataSet.SetCellDistance1(const ALay, ARow,
  ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellDistance1[ALay, ARow, ACol] := Value;
end;

procedure TArrayPhastDataSet.SetCellDistance2(const ALay, ARow,
  ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellDistance2[ALay, ARow, ACol] := Value;
end;

procedure TArrayPhastDataSet.SetCellInterpolationDirection(const ALay, ARow,
  ACol: integer; const Value: TInterpolationDirection);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellInterpolationDirection[ALay, ARow, ACol] := Value;
end;

procedure TArrayPhastDataSet.InitializePhastArrays;
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
  LayIndex, RowIndex, ColIndex: integer;
begin
  GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  for LayIndex := 0 to NumberOfLayers - 1 do
  begin
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        FCellDistance1[LayIndex, RowIndex, ColIndex] := Distance1;
        FCellDistance2[LayIndex, RowIndex, ColIndex] := Distance2;
        FCellInterpolationDirection[LayIndex, RowIndex, ColIndex] :=
          InterpolationDirection;
        FIsInterpolatedCell[LayIndex, RowIndex, ColIndex] :=
          UsePHAST_InterpolationForAllCells;
        Annotation[LayIndex, RowIndex, ColIndex] :=
          StrSetByPHASTstyleI;
      end;
    end;
  end;
end;

procedure TArrayPhastDataSet.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
begin
  inherited;
  if SetToZero or (Model.ModelSelection <> msPhast) then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
  end
  else
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;
  SetLength(FCellDistance1, NumberOfLayers, NumberOfRows, NumberOfColumns);
  SetLength(FCellDistance2, NumberOfLayers, NumberOfRows, NumberOfColumns);
  SetLength(FCellInterpolationDirection, NumberOfLayers, NumberOfRows,
    NumberOfColumns);
  SetLength(FIsInterpolatedCell, NumberOfLayers, NumberOfRows, NumberOfColumns);
  if not (SetToZero or (Model.ModelSelection <> msPhast)) then
  begin
    InitializePhastArrays;
  end;
end;

procedure TArrayPhastDataSet.SetIsInterpolatedCell(const ALay, ARow,
  ACol: integer; const Value: boolean);
begin
  if Model.ModelSelection = msPhast then
  begin
    if DimensionsChanged then
      SetDimensions(False);
    FIsInterpolatedCell[ALay, ARow, ACol] := Value;
  end;
//  UpdateEvalTime;
end;

procedure TArrayPhastDataSet.ResetCellsPhastInterpolation;
var
  LayIndex, RowIndex, ColIndex: integer;
  NumberOfLayers, NumberOfRows, NumberOfColumns: integer;
begin
  if Model.ModelSelection = msPhast then
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
    for LayIndex := 0 to NumberOfLayers - 1 do
    begin
      for RowIndex := 0 to NumberOfRows - 1 do
      begin
        for ColIndex := 0 to NumberOfColumns - 1 do
        begin
          FIsInterpolatedCell[LayIndex, RowIndex, ColIndex] := False;
        end;
      end;
    end;
  end;
end;

{ TIntegerPhastDataSet }

procedure TIntegerPhastDataSet.Assign(Source: TPersistent);
var
  DataSet: TIntegerPhastDataSet;
  InterpValues: TPhastInterpolationValues;
begin
  if Source is TIntegerPhastDataSet then
  begin
    DataSet := TIntegerPhastDataSet(Source);
    Value1 := DataSet.Value1;
    Value2 := DataSet.Value2;
  end
  else if Source is TPhastInterpolationValues then
  begin
    InterpValues := TPhastInterpolationValues(Source);
    Value1 := InterpValues.IntValue1;
    Value2 := InterpValues.IntValue2;
  end;
  inherited;
end;

constructor TIntegerPhastDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  DataType := rdtInteger;
end;

function TIntegerPhastDataSet.DisplayRealValue: boolean;
begin
  Assert(Datatype = rdtInteger);
  result := True;
end;

function TIntegerPhastDataSet.GetCellValue1(const ALay, ARow,
  ACol: integer): integer;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue1[ALay, ARow, ACol];
end;

function TIntegerPhastDataSet.GetCellValue2(const ALay, ARow,
  ACol: integer): integer;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue2[ALay, ARow, ACol];
end;

function TIntegerPhastDataSet.GetFraction(const ALay, ARow,
  ACol: integer): double;
begin
  Assert(IsInterpolatedCell[ALay, ARow, ACol]);
  result := FFraction[ALay, ARow, ACol];
end;

function TIntegerPhastDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  result := RealValue[Layer, Row, Col];
end;

function TIntegerPhastDataSet.GetRealValue(const ALay, ARow,
  ACol: integer): double;
var
  Frac: double;
begin
  if IsInterpolatedCell[ALay, ARow, ACol] then
  begin
    Frac := Fraction[ALay, ARow, ACol];
    result := Frac * CellValue1[ALay, ARow, ACol] + (1 - Frac) *
      CellValue2[ALay, ARow, ACol];
  end
  else
  begin
    result := IntegerData[ALay, ARow, ACol];
  end;
end;

procedure TIntegerPhastDataSet.InitializePhastArrays;
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
  LayIndex, RowIndex, ColIndex: integer;
  Distance: double;
  Frac: double;
  Compiler: TRbwParser;
  TempFormula: string;
  Expression: TExpression;
  ResultTypeOK: boolean;
  TempUseList: TStringList;
  VarIndex: integer;
  VarName: string;
//  DataSetIndex: integer;
  AnotherDataSet: TDataArray;
  VarPosition: integer;
  Variable: TCustomValue;
  LayerToUse, RowToUse, ColToUse: integer;
  RealCellValue: double;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  for LayIndex := 0 to NumberOfLayers - 1 do
  begin
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        FCellValue1[LayIndex, RowIndex, ColIndex] := Value1;
        FCellValue2[LayIndex, RowIndex, ColIndex] := Value2;
      end;
    end;
  end;
  if UsePHAST_InterpolationForAllCells then
  begin
    TempUseList := TStringList.Create;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    try
      if InterpolationDirection = pidMix then
      begin
        TempUseList.Duplicates := dupIgnore;
        TempUseList.Sorted := True;
        TempUseList.Capacity := MixtureUseList.Count;
        TempUseList.Assign(MixtureUseList);

        for VarIndex := 0 to TempUseList.Count - 1 do
        begin
          VarName := TempUseList[VarIndex];
          AnotherDataSet := DataArrayManager.GetDataSetByName(VarName);
          if AnotherDataSet <> nil then
          begin
//            AnotherDataSet := frmGoPhast.PhastModel.DataSets[DataSetIndex];
            Assert(AnotherDataSet <> self);
            AnotherDataSet.Initialize;
            DataArrayManager.AddDataSetToCache(AnotherDataSet);
          end;
        end;
        GlobalEvaluatedAt := EvaluatedAt;

        Compiler := GetCompiler;
        TempFormula := MixtureFormula;
        try
          Compiler.Compile(TempFormula);
        except on E: ERbwParserError do
          begin
            ResetMixtureExpression(Compiler, E.Message);
          end;
        end;

        TempFormula := MixtureFormula;
        Compiler.Compile(TempFormula);
        Expression := Compiler.CurrentExpression;
        ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
        if not ResultTypeOK then
        begin
//          Beep;
//          MessageDlg(Format(StrTheMixtureFormula, [Name, Expression.Decompile]), mtError, [mbOK], 0);
          raise EInvalidDataType.Create(Format(StrTheMixtureFormula, [Name, Expression.Decompile]), Expression.Decompile);
        end;
      end
      else
      begin
        Compiler := nil;
        Expression := nil;
      end;

      Distance := 0;
      for LayIndex := 0 to NumberOfLayers - 1 do
      begin
        for RowIndex := 0 to NumberOfRows - 1 do
        begin
          for ColIndex := 0 to NumberOfColumns - 1 do
          begin
            case InterpolationDirection of
              pidX:
                begin
                  case EvaluatedAt of
                    eaBlocks:
                      begin
                        Distance := (frmGoPhast.PhastGrid.
                          ColumnPosition[ColIndex]
                          + frmGoPhast.PhastGrid.
                          ColumnPosition[ColIndex + 1]) /
                          2;
                      end;
                    eaNodes:
                      begin
                        Distance := frmGoPhast.PhastGrid.
                          ColumnPosition[ColIndex];
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
                        Distance := (frmGoPhast.PhastGrid.
                          LayerElevation[LayIndex]
                          + frmGoPhast.PhastGrid.LayerElevation[LayIndex + 1]) /
                          2;
                      end;
                    eaNodes:
                      begin
                        Distance := frmGoPhast.PhastGrid.
                          LayerElevation[LayIndex];
                      end;
                  else
                    Assert(False);
                  end;
                end;
              pidMix:
                begin
                  for VarIndex := 0 to TempUseList.Count - 1 do
                  begin
                    VarName := TempUseList[VarIndex];
                    VarPosition := Compiler.IndexOfVariable(VarName);
                    Variable := Compiler.Variables[VarPosition];
                    AnotherDataSet :=
                      DataArrayManager.GetDataSetByName(VarName);
                    if AnotherDataSet <> nil then
                    begin
//                      AnotherDataSet :=
//                        frmGoPhast.PhastModel.DataSets[DataSetIndex];
                      Assert(AnotherDataSet <> self);
                      Assert(AnotherDataSet.DataType = Variable.ResultType);
                      if AnotherDataSet.Orientation = dsoTop then
                      begin
                        LayerToUse := 0;
                      end
                      else
                      begin
                        LayerToUse := LayIndex;
                      end;
                      if AnotherDataSet.Orientation = dsoFront then
                      begin
                        RowToUse := 0;
                      end
                      else
                      begin
                        RowToUse := RowIndex;
                      end;
                      if AnotherDataSet.Orientation = dsoSide then
                      begin
                        ColToUse := 0;
                      end
                      else
                      begin
                        ColToUse := ColIndex;
                      end;

                      case Variable.ResultType of
                        rdtDouble:
                          begin
                            TRealVariable(Variable).Value :=
                              AnotherDataSet.RealData[LayerToUse, RowToUse,
                              ColToUse];
                          end;
                        rdtInteger:
                          begin
                            TIntegerVariable(Variable).Value :=
                              AnotherDataSet.IntegerData[LayerToUse, RowToUse,
                              ColToUse];
                          end;
                        rdtBoolean:
                          begin
                            TBooleanVariable(Variable).Value :=
                              AnotherDataSet.BooleanData[LayerToUse, RowToUse,
                              ColToUse];
                          end;
                        rdtString:
                          begin
                            TStringVariable(Variable).Value :=
                              AnotherDataSet.StringData[LayerToUse, RowToUse,
                              ColToUse];
                          end;
                      else
                        Assert(False);
                      end;
                    end;
                  end;

                  try
                    Expression.Evaluate;
                  except on E: ERbwParserError do
                    begin
                      frmFormulaErrors.AddFormulaError('',
                        Format(StrMixtureFormulaFor, [Name]),
                        MixtureFormula, E.Message);
                      MixtureFormula := '0.5';
                      
                      TempFormula := MixtureFormula;
                      Compiler.Compile(TempFormula);

                      Expression := Compiler.CurrentExpression;
                      Expression.Evaluate;
                    end;
                  end;
                  Distance := 1 - Expression.DoubleResult;
                end;
            else
              Assert(False);
            end;
            if Distance <= Distance1 then
            begin
              IntegerData[LayIndex, RowIndex, ColIndex] := Value1;
              //RealCellValue := Value1;
              if InterpolationDirection = pidMix then
              begin
                Frac := 1;
              end
              else
              begin
                Frac := 0;
              end
            end
            else if Distance >= Distance2 then
            begin
              IntegerData[LayIndex, RowIndex, ColIndex] := Value2;
              //RealCellValue := Value2;
              if InterpolationDirection = pidMix then
              begin
                Frac := 0;
              end
              else
              begin
                Frac := 1;
              end
            end
            else
            begin
              Frac := 1 - (Distance - Distance1) / (Distance2 - Distance1);
              RealCellValue := Frac * Value1 + (1 - Frac) * Value2;
              IntegerData[LayIndex, RowIndex, ColIndex] := Round(RealCellValue);
            end;
            Fraction[LayIndex, RowIndex, ColIndex] := Frac;
          end;
        end;
      end;
    finally
      TempUseList.Free;
      DataArrayManager.CacheDataArrays;
    end;
  end;
end;

procedure TIntegerPhastDataSet.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Integer;
  FractionalValue: double;
begin
  inherited ReadData(DecompressionStream);
  if Model.ModelSelection = msPhast then
  begin
    DecompressionStream.Read(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
      DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
      DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue1[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue2[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(FractionalValue, SizeOf(FractionalValue));
      Fraction[LayerIndex, RowIndex, ColIndex] := FractionalValue;
    end;
  end;
end;

procedure TIntegerPhastDataSet.SetCellValue1(const ALay, ARow, ACol,
  Value: integer);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue1[ALay, ARow, ACol] := Value;
end;

procedure TIntegerPhastDataSet.SetCellValue2(const ALay, ARow, ACol,
  Value: integer);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue2[ALay, ARow, ACol] := Value;
end;

procedure TIntegerPhastDataSet.SetDataType(const Value: TRbwDataType);
begin
  assert(Value = rdtInteger);
  inherited;
end;

procedure TIntegerPhastDataSet.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
begin
  if SetToZero or (Model.ModelSelection <> msPhast) then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
  end
  else
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;
  SetLength(FCellValue1, NumberOfLayers, NumberOfRows, NumberOfColumns);
  SetLength(FCellValue2, NumberOfLayers, NumberOfRows, NumberOfColumns);
  SetLength(FFraction, NumberOfLayers, NumberOfRows, NumberOfColumns);
  inherited;
end;

procedure TIntegerPhastDataSet.SetFraction(const ALay, ARow, ACol: integer;
  const Value: double);
begin
  FFraction[ALay, ARow, ACol] := Value;
end;

procedure TIntegerPhastDataSet.SetValue1(const Value: integer);
begin
  if FValue1 <> Value then
  begin
    FValue1 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TIntegerPhastDataSet.SetValue2(const Value: integer);
begin
  if FValue2 <> Value then
  begin
    FValue2 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TIntegerPhastDataSet.StoreData(Stream: TStream);
var
  ColLimit: integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Count: Integer;
  Value: Integer;
  FractionalValue: double;
begin
  inherited StoreData(Stream);
  if Model.ModelSelection = msPhast then
  begin
    GetLimits(ColLimit, RowLimit, LayerLimit);
    Count := 0;
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]
            and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
          begin
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex]
              and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
            begin
              Stream.Write(LayerIndex, SizeOf(LayerIndex));
              Stream.Write(RowIndex, SizeOf(RowIndex));
              Stream.Write(ColIndex, SizeOf(ColIndex));
              Value := CellValue1[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              Value := CellValue2[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              FractionalValue := Fraction[LayerIndex, RowIndex, ColIndex];
              Stream.Write(FractionalValue, SizeOf(FractionalValue));
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TRealPhastDataSet }

procedure TRealPhastDataSet.Assign(Source: TPersistent);
var
  DataSet: TRealPhastDataSet;
  InterpValues: TPhastInterpolationValues;
begin
  if Source is TRealPhastDataSet then
  begin
    DataSet := TRealPhastDataSet(Source);
    Value1 := DataSet.Value1;
    Value2 := DataSet.Value2;
  end
  else if Source is TPhastInterpolationValues then
  begin
    InterpValues := TPhastInterpolationValues(Source);
    Value1 := InterpValues.RealValue1;
    Value2 := InterpValues.RealValue2;
  end;
  inherited;
end;

constructor TRealPhastDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  DataType := rdtDouble;
end;

function TRealPhastDataSet.GetCellValue1(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue1[ALay, ARow, ACol];
end;

function TRealPhastDataSet.GetCellValue2(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue2[ALay, ARow, ACol];
end;

procedure TRealPhastDataSet.InitializePhastArrays;
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
  LayIndex, RowIndex, ColIndex: integer;
  Distance: double;
  Fraction: double;
  LocalModel: TCustomModel;
begin
  inherited;
  GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  for LayIndex := 0 to NumberOfLayers - 1 do
  begin
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        FCellValue1[LayIndex, RowIndex, ColIndex] := Value1;
        FCellValue2[LayIndex, RowIndex, ColIndex] := Value2;
      end;
    end;
  end;
  if UsePHAST_InterpolationForAllCells then
  begin
    Distance := 0;
    LocalModel := Model as TCustomModel;
    for LayIndex := 0 to NumberOfLayers - 1 do
    begin
      for RowIndex := 0 to NumberOfRows - 1 do
      begin
        for ColIndex := 0 to NumberOfColumns - 1 do
        begin
          case InterpolationDirection of
            pidX:
              begin
                case EvaluatedAt of
                  eaBlocks:
                    begin
                      Distance := (LocalModel.PhastGrid.ColumnPosition[ColIndex]
                        + LocalModel.PhastGrid.ColumnPosition[ColIndex + 1]) /
                        2;
                    end;
                  eaNodes:
                    begin
                      Distance := LocalModel.PhastGrid.ColumnPosition[ColIndex];
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
                      Distance := (LocalModel.PhastGrid.RowPosition[RowIndex]
                        + LocalModel.PhastGrid.RowPosition[RowIndex + 1]) / 2;
                    end;
                  eaNodes:
                    begin
                      Distance := LocalModel.PhastGrid.RowPosition[RowIndex];
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
                      Distance := (LocalModel.PhastGrid.LayerElevation[LayIndex]
                        + LocalModel.PhastGrid.LayerElevation[LayIndex + 1]) /
                        2;
                    end;
                  eaNodes:
                    begin
                      Distance := LocalModel.PhastGrid.LayerElevation[LayIndex];
                    end;
                else
                  Assert(False);
                end;
              end;
          else
            Assert(False);
          end;
          if Distance <= Distance1 then
          begin
            RealData[LayIndex, RowIndex, ColIndex] := Value1;
          end
          else if Distance >= Distance2 then
          begin
            RealData[LayIndex, RowIndex, ColIndex] := Value2;
          end
          else
          begin
            Fraction := 1 - (Distance - Distance1) / (Distance2 - Distance1);
            RealData[LayIndex, RowIndex, ColIndex] :=
              Fraction * Value1 + (1 - Fraction) * Value2;
          end;
        end;
      end;
    end;
  end;
end;

procedure TRealPhastDataSet.ReadData(DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: double;
begin
  inherited ReadData(DecompressionStream);
  if Model.ModelSelection = msPhast then
  begin
    DecompressionStream.Read(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
      DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
      DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue1[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue2[LayerIndex, RowIndex, ColIndex] := Value;
    end;
  end;
end;

procedure TRealPhastDataSet.SetCellValue1(const ALay, ARow, ACol: integer;
  const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue1[ALay, ARow, ACol] := Value;
end;

procedure TRealPhastDataSet.SetCellValue2(const ALay, ARow, ACol: integer;
  const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue2[ALay, ARow, ACol] := Value;
end;

procedure TRealPhastDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtDouble);
  inherited;
end;

procedure TRealPhastDataSet.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
begin
  if SetToZero or (Model.ModelSelection <> msPhast) then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
  end
  else
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;
  SetLength(FCellValue1, NumberOfLayers, NumberOfRows, NumberOfColumns);
  SetLength(FCellValue2, NumberOfLayers, NumberOfRows, NumberOfColumns);
  inherited;
end;

procedure TRealPhastDataSet.SetValue1(const Value: double);
begin
  if FValue1 <> Value then
  begin
    FValue1 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TRealPhastDataSet.SetValue2(const Value: double);
begin
  if FValue2 <> Value then
  begin
    FValue2 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;
procedure TRealPhastDataSet.StoreData(Stream: TStream);
var
  ColLimit: integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Count: Integer;
  Value: double;
begin
  inherited StoreData(Stream);
  if Model.ModelSelection = msPhast then
  begin
    GetLimits(ColLimit, RowLimit, LayerLimit);
    Count := 0;
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]
            and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
          begin
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex]
              and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
            begin
              Stream.Write(LayerIndex, SizeOf(LayerIndex));
              Stream.Write(RowIndex, SizeOf(RowIndex));
              Stream.Write(ColIndex, SizeOf(ColIndex));
              Value := CellValue1[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              Value := CellValue2[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TInterpValuesItem }

procedure TInterpValuesItem.Assign(Source: TPersistent);
begin
  if Source is TInterpValuesItem then
  begin
    Values := TInterpValuesItem(Source).Values;
    frmGoPhast.InvalidateModel;
  end
  else
  begin
    inherited
  end;
end;

constructor TInterpValuesItem.Create(Collection: TCollection);
begin
  inherited;
  FValues := TPhastInterpolationValues.Create;
end;

destructor TInterpValuesItem.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TInterpValuesItem.SetValues(
  const Value: TPhastInterpolationValues);
begin
  FValues.Assign(Value);
  frmGoPhast.InvalidateModel;
end;

{ TInterpValuesCollection }

constructor TInterpValuesCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TInterpValuesItem, InvalidateModelEvent);
end;

function TInterpValuesCollection.GetItemOfDataSet(
  const DataSet: TDataArray): TInterpValuesItem;
var
  Index: integer;
  AnItem: TInterpValuesItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TInterpValuesItem;
    if AnItem.Values.DataSet = DataSet then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

{ TSparseArrayPhastInterpolationDataSet }

constructor TSparseArrayPhastInterpolationDataSet.Create(AnOwner: TComponent);
var
  LocalModel: TCustomModel;
  LayerCount, RowCount, ColumnCount: Integer;
begin
  inherited Create(AnOwner);
  if AnOwner <> nil then
  begin
    LocalModel := AnOwner as TCustomModel;
    LayerCount := LocalModel.LayerCount+1;
    RowCount := LocalModel.RowCount+1;
    ColumnCount := LocalModel.MaxColumnCount;
  end
  else
  begin
    LayerCount := 0;
    RowCount := 0;
    ColumnCount := 0;
  end;
  FCellDistance1 := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FCellDistance2 := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FCellInterpolationDirection :=
    T3DSparseInterpolationDirectionArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FIsInterpolatedCell := T3DSparseBooleanArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FAnnotation := T3DSparseStringArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  // Sparase Array data sets are only used for boundary conditions in
  // PHAST and the boundary conditions all apply to nodes.
  EvaluatedAt := eaNodes;
end;

destructor TSparseArrayPhastInterpolationDataSet.Destroy;
begin
  FreeAndNil(FCellDistance1);
  FreeAndNil(FCellDistance2);
  FreeAndNil(FCellInterpolationDirection);
  FreeAndNil(FIsInterpolatedCell);
  FreeAndNil(FAnnotation);
  inherited;
end;

function TSparseArrayPhastInterpolationDataSet.GetAnnotation(const Layer, Row,
  Col: integer): string;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FAnnotation[Layer, Row, Col];
//  UpdateEvalTime;
end;

function TSparseArrayPhastInterpolationDataSet.GetCellDistance1(
  const ALay, ARow, ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellDistance1[ALay, ARow, ACol];
end;

function TSparseArrayPhastInterpolationDataSet.GetCellDistance2(
  const ALay, ARow, ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellDistance2[ALay, ARow, ACol];
end;

function TSparseArrayPhastInterpolationDataSet.GetCellInterpolationDirection(
  const ALay, ARow, ACol: integer): TInterpolationDirection;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellInterpolationDirection[ALay, ARow, ACol];
end;

function TSparseArrayPhastInterpolationDataSet.GetIsInterpolatedCell(
  const ALay, ARow, ACol: integer): boolean;
begin
  if IsUniform = iuTrue then
  begin
    result := False;
    Exit;
  end;
  if DimensionsChanged then
    SetDimensions(False);
  result := FIsInterpolatedCell[ALay, ARow, ACol];
//  UpdateEvalTime;
end;

function TSparseArrayPhastInterpolationDataSet.GetIsValue(
  const Layer, Row, Col: Integer): boolean;
begin
  if FDataCached and FCleared then
  begin
    RestoreData;
  end;
  if BoundaryDataType <> nil then
  begin
    result := BoundaryDataType.IsValue[Layer, Row, Col]
  end
  else
  begin
    result := True
  end;
  result := result
    and FIsInterpolatedCell.IsValue[Layer, Row, Col]
    and FAnnotation.IsValue[Layer, Row, Col];

  if result and FIsInterpolatedCell[Layer, Row, Col] then
  begin
    result := FCellDistance1.IsValue[Layer, Row, Col]
      and FCellDistance2.IsValue[Layer, Row, Col]
      and FCellInterpolationDirection.IsValue[Layer, Row, Col];
  end
end;

procedure TSparseArrayPhastInterpolationDataSet.Initialize;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  FreeStack: boolean;
  StackIndex: Integer;
begin
  // Values are assigned only using screen objects. Neither iterpolation nor
  // global expressions are used.

  if UpToDate and not DimensionsChanged then
    Exit;

//  FEvalTime := Now;
  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      Stack[0] := Stack[0] + ' depends on';
      for StackIndex := 1 to Stack.Count - 2 do
      begin
        Stack[StackIndex] := Stack[StackIndex] + ' which depends on';
      end;
      HandleCircularReferenceError(Format(StrCircularReferenceI2, [Name, Stack.Text]), nil);
      Exit;
//      raise ECircularReference.Create(Format(StrCircularReferenceI, [Name]));
    end;
    Stack.Add(Name);

    GlobalEvaluatedAt := EvaluatedAt;

    SetDimensions(False);

    for ScreenObjectIndex := 0
      to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted then
      begin
        AScreenObject.AssignValuesToDataSet(self, FModel, lctUse);
      end;
    end;
  finally
    if FreeStack then
    begin
      FreeAndNil(Stack)
    end;
  end;

  PostInitialize;

  UpToDate := True;

end;

function TSparseArrayPhastInterpolationDataSet.IsSparseArray: boolean;
begin
  result := True;
end;

procedure TSparseArrayPhastInterpolationDataSet.ResetCellsPhastInterpolation;
begin
  FIsInterpolatedCell.Clear;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetAnnotation(
  const Layer, Row, Col: integer; const Value: string);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FAnnotation[Layer, Row, Col] := Value;
//  UpdateEvalTime;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetCellDistance1(
  const ALay, ARow, ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellDistance1[ALay, ARow, ACol] := Value;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetCellDistance2(
  const ALay, ARow, ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellDistance2[ALay, ARow, ACol] := Value;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetCellInterpolationDirection(
  const ALay, ARow, ACol: integer; const Value: TInterpolationDirection);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellInterpolationDirection[ALay, ARow, ACol] := Value;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetDimensions(
  const SetToZero: boolean);
begin
  inherited;
  if FCellDistance1 <> nil then
  begin
    FCellDistance1.Clear;
  end;
  if FCellDistance2 <> nil then
  begin
    FCellDistance2.Clear;
  end;
  if FCellInterpolationDirection <> nil then
  begin
    FCellInterpolationDirection.Clear;
  end;
  if FIsInterpolatedCell <> nil then
  begin
    FIsInterpolatedCell.Clear;
  end;
  if FAnnotation <> nil then
  begin
    FAnnotation.Clear;
  end;

end;

procedure TSparseArrayPhastInterpolationDataSet.SetIsInterpolatedCell(
  const ALay, ARow, ACol: integer; const Value: boolean);
begin
  if Model.ModelSelection = msPhast then
  begin
    if DimensionsChanged then
      SetDimensions(False);
    FIsInterpolatedCell[ALay, ARow, ACol] := Value;
  end;
//  UpdateEvalTime;
end;

procedure TSparseArrayPhastInterpolationDataSet.SetIsValue(
  const Layer, Row, Col: Integer; const Value: boolean);
begin
  if BoundaryDataType <> nil then
  begin
    BoundaryDataType.IsValue[Layer, Row, Col] := Value;
  end
end;

procedure TSparseArrayPhastInterpolationDataSet.UpdateDimensions(NumberOfLayers,
  NumberOfRows, NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FAnnotation.Free;
    FAnnotation := T3DSparseStringArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FCellDistance1.Free;
    FCellDistance1 := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FCellDistance2.Free;
    FCellDistance2 := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FCellInterpolationDirection.Free;
    FCellInterpolationDirection := T3DSparseInterpolationDirectionArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FIsInterpolatedCell.Free;
    FIsInterpolatedCell := T3DSparseBooleanArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TSparseRealPhastDataSet }

procedure TSparseRealPhastDataSet.Assign(Source: TPersistent);
var
  DataSet: TSparseRealPhastDataSet;
  InterpValues: TPhastInterpolationValues;
begin
  if Source is TSparseRealPhastDataSet then
  begin
    DataSet := TSparseRealPhastDataSet(Source);
    Value1 := DataSet.Value1;
    Value2 := DataSet.Value2;
  end
  else if Source is TPhastInterpolationValues then
  begin
    InterpValues := TPhastInterpolationValues(Source);
    Value1 := InterpValues.RealValue1;
    Value2 := InterpValues.RealValue2;
  end;
  inherited;
end;

constructor TSparseRealPhastDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FCellValue1 := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FCellValue2 := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FRealValues := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtDouble;
end;

destructor TSparseRealPhastDataSet.Destroy;
begin
  FreeAndNil(FCellValue1);
  FreeAndNil(FCellValue2);
  FreeAndNil(FRealValues);
  inherited;
end;

function TSparseRealPhastDataSet.GetCellValue1(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue1[ALay, ARow, ACol];
end;

function TSparseRealPhastDataSet.GetCellValue2(const ALay, ARow,
  ACol: integer): double;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue2[ALay, ARow, ACol];
end;

function TSparseRealPhastDataSet.GetIsValue(const Index1, Index2,
  Index3: Integer): boolean;
begin
  result := inherited GetIsValue(Index1, Index2, Index3)
    and FRealValues.IsValue[Index1, Index2, Index3];
  if result and IsInterpolatedCell[Index1, Index2, Index3] then
  begin
    result := result and FCellValue1.IsValue[Index1, Index2, Index3]
      and FCellValue2.IsValue[Index1, Index2, Index3];
  end;
end;

function TSparseRealPhastDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FRealValues[Layer, Row, Col];
end;

procedure TSparseRealPhastDataSet.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: double;
begin
  inherited ReadData(DecompressionStream);
  if Model.ModelSelection = msPhast then
  begin
    DecompressionStream.Read(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
      DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
      DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue1[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue2[LayerIndex, RowIndex, ColIndex] := Value;
    end;
  end;
end;

procedure TSparseRealPhastDataSet.SetCellValue1(const ALay, ARow,
  ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue1[ALay, ARow, ACol] := Value;
end;

procedure TSparseRealPhastDataSet.SetCellValue2(const ALay, ARow,
  ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue2[ALay, ARow, ACol] := Value;
end;

procedure TSparseRealPhastDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtDouble);
  inherited;
end;

procedure TSparseRealPhastDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FCellValue1 <> nil then
  begin
    FCellValue1.Clear;
  end;
  if FCellValue2 <> nil then
  begin
    FCellValue2.Clear;
  end;
  if FRealValues <> nil then
  begin
    FRealValues.Clear;
  end;
end;

procedure TSparseRealPhastDataSet.SetRealData(const Layer, Row,
  Col: integer; const Value: double);
begin
  if CheckMin and (Value < Min) then
  begin
    FRealValues[Layer, Row, Col] := Min;
  end
  else if CheckMax and (Value > Max) then
  begin
    FRealValues[Layer, Row, Col] := Max;
  end
  else
  begin
    FRealValues[Layer, Row, Col] := Value;
  end;
end;

procedure TSparseRealPhastDataSet.SetValue1(const Value: double);
begin
  if FValue1 <> Value then
  begin
    FValue1 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSparseRealPhastDataSet.SetValue2(const Value: double);
begin
  if FValue2 <> Value then
  begin
    FValue2 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSparseRealPhastDataSet.StoreData(Stream: TStream);
var
  ColLimit: integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Count: Integer;
  Value: double;
begin
  inherited StoreData(Stream);
  if Model.ModelSelection = msPhast then
  begin
    GetLimits(ColLimit, RowLimit, LayerLimit);
    Count := 0;
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]
            and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
          begin
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex]
              and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
            begin
              Stream.Write(LayerIndex, SizeOf(LayerIndex));
              Stream.Write(RowIndex, SizeOf(RowIndex));
              Stream.Write(ColIndex, SizeOf(ColIndex));
              Value := CellValue1[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              Value := CellValue2[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSparseRealPhastDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FCellValue1.Free;
    FCellValue1 := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FCellValue2.Free;
    FCellValue2 := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FRealValues.Free;
    FRealValues := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TSparseIntegerPhastInterpolationDataSet }

procedure TSparseIntegerPhastDataSet.Assign(Source: TPersistent);
var
  DataSet: TSparseIntegerPhastDataSet;
  InterpValues: TPhastInterpolationValues;
begin
  if Source is TSparseIntegerPhastDataSet then
  begin
    DataSet := TSparseIntegerPhastDataSet(Source);
    Value1 := DataSet.Value1;
    Value2 := DataSet.Value2;
  end
  else if Source is TPhastInterpolationValues then
  begin
    InterpValues := TPhastInterpolationValues(Source);
    Value1 := InterpValues.IntValue1;
    Value2 := InterpValues.IntValue2;
  end;
  inherited;
end;

constructor TSparseIntegerPhastDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FCellValue1 := T3DSparseIntegerArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FCellValue2 := T3DSparseIntegerArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FIntegerValues := T3DSparseIntegerArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  FFraction := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtInteger;
end;

destructor TSparseIntegerPhastDataSet.Destroy;
begin
  FreeAndNil(FCellValue1);
  FreeAndNil(FCellValue2);
  FreeAndNil(FIntegerValues);
  FreeAndNil(FFraction);
  inherited;
end;

function TSparseIntegerPhastDataSet.DisplayRealValue: boolean;
begin
  Assert(Datatype = rdtInteger);
  result := True;
end;

function TSparseIntegerPhastDataSet.GetCellValue1(const ALay, ARow,
  ACol: integer): integer;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue1[ALay, ARow, ACol];
end;

function TSparseIntegerPhastDataSet.GetCellValue2(const ALay, ARow,
  ACol: integer): integer;
begin
  if DimensionsChanged then
    SetDimensions(False);
  result := FCellValue2[ALay, ARow, ACol];
end;

function TSparseIntegerPhastDataSet.GetFraction(const ALay, ARow,
  ACol: integer): double;
begin
  Assert(IsValue[ALay, ARow, ACol]);
  Assert(IsInterpolatedCell[ALay, ARow, ACol]);
  result := FFraction[ALay, ARow, ACol];
end;

function TSparseIntegerPhastDataSet.GetIntegerData(const Layer, Row,
  Col: integer): integer;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FIntegerValues[Layer, Row, Col];
end;

function TSparseIntegerPhastDataSet.GetIsValue(const Index1, Index2,
  Index3: Integer): boolean;
begin
  result := inherited GetIsValue(Index1, Index2, Index3)
    and FIntegerValues.IsValue[Index1, Index2, Index3];
  if result and IsInterpolatedCell[Index1, Index2, Index3] then
  begin
    result := result and FCellValue1.IsValue[Index1, Index2, Index3]
      and FCellValue2.IsValue[Index1, Index2, Index3];
  end;
end;

function TSparseIntegerPhastDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  result := RealValue[Layer, Row, Col];
end;

function TSparseIntegerPhastDataSet.GetRealValue(const ALay, ARow,
  ACol: integer): double;
var
  Frac: double;
begin
  Assert(IsValue[ALay, ARow, ACol]);
  if IsInterpolatedCell[ALay, ARow, ACol] then
  begin
    Frac := Fraction[ALay, ARow, ACol];
    result := Frac * CellValue1[ALay, ARow, ACol]
      + (1 - Frac) * CellValue2[ALay, ARow, ACol];
  end
  else
  begin
    result := IntegerData[ALay, ARow, ACol];
  end;
end;

procedure TSparseIntegerPhastDataSet.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Integer;
  FractionalValue: double;
begin
  inherited ReadData(DecompressionStream);
  if Model.ModelSelection = msPhast then
  begin
    DecompressionStream.Read(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(LayerIndex, SizeOf(LayerIndex));
      DecompressionStream.Read(RowIndex, SizeOf(RowIndex));
      DecompressionStream.Read(ColIndex, SizeOf(ColIndex));
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue1[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(Value, SizeOf(Value));
      CellValue2[LayerIndex, RowIndex, ColIndex] := Value;
      DecompressionStream.Read(FractionalValue, SizeOf(FractionalValue));
      Fraction[LayerIndex, RowIndex, ColIndex] := FractionalValue;
    end;
  end;
end;

procedure TSparseIntegerPhastDataSet.SetCellValue1(const ALay, ARow, ACol,
  Value: integer);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue1[ALay, ARow, ACol] := Value;
end;

procedure TSparseIntegerPhastDataSet.SetCellValue2(const ALay, ARow, ACol,
  Value: integer);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FCellValue2[ALay, ARow, ACol] := Value;
end;

procedure TSparseIntegerPhastDataSet.SetDataType(
  const Value: TRbwDataType);
begin
  assert(Value = rdtInteger);
  inherited;
end;

procedure TSparseIntegerPhastDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FCellValue1 <> nil then
  begin
    FCellValue1.Clear;
  end;
  if FCellValue2 <> nil then
  begin
    FCellValue2.Clear;
  end;
  if FIntegerValues <> nil then
  begin
    FIntegerValues.Clear;
  end;
  if FFraction <> nil then
  begin
    FFraction.Clear;
  end;
end;

procedure TSparseIntegerPhastDataSet.SetFraction(const ALay, ARow,
  ACol: integer; const Value: double);
begin
  if DimensionsChanged then
    SetDimensions(False);
  FFraction[ALay, ARow, ACol] := Value;
end;

procedure TSparseIntegerPhastDataSet.SetIntegerData(const Layer, Row, Col,
  Value: integer);
begin
  if CheckMin and (Value < Min) then
  begin
    FIntegerValues[Layer, Row, Col] := Trunc(Min);
  end
  else if CheckMax and (Value > Max) then
  begin
    FIntegerValues[Layer, Row, Col] := Trunc(Max);
  end
  else
  begin
    FIntegerValues[Layer, Row, Col] := Value;
  end;
end;

procedure TSparseIntegerPhastDataSet.SetValue1(const Value: integer);
begin
  if FValue1 <> Value then
  begin
    FValue1 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSparseIntegerPhastDataSet.SetValue2(const Value: integer);
begin
  if FValue2 <> Value then
  begin
    FValue2 := Value;
    Invalidate;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSparseIntegerPhastDataSet.StoreData(Stream: TStream);
var
  ColLimit: integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Count: Integer;
  Value: Integer;
  FractionalValue: Double;
begin
  inherited StoreData(Stream);
  if Model.ModelSelection = msPhast then
  begin
    GetLimits(ColLimit, RowLimit, LayerLimit);
    Count := 0;
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]
            and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
          begin
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex]
              and IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] then
            begin
              Stream.Write(LayerIndex, SizeOf(LayerIndex));
              Stream.Write(RowIndex, SizeOf(RowIndex));
              Stream.Write(ColIndex, SizeOf(ColIndex));
              Value := CellValue1[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              Value := CellValue2[LayerIndex, RowIndex, ColIndex];
              Stream.Write(Value, SizeOf(Value));
              FractionalValue := Fraction[LayerIndex, RowIndex, ColIndex];
              Stream.Write(FractionalValue, SizeOf(FractionalValue));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSparseIntegerPhastDataSet.UpdateDimensions(NumberOfLayers,
  NumberOfRows, NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FCellValue1.Free;
    FCellValue1 := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FCellValue2.Free;
    FCellValue2 := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FFraction.Free;
    FFraction := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
    FIntegerValues.Free;
    FIntegerValues := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TPhastTimeList }

function TPhastTimeList.Add(const ATime: double; const Data:
  TSparseArrayPhastInterpolationDataSet): integer;
var
  Index: integer;
  BoundaryDataSet: TDataArray;
begin
  result := IndexOf(ATime);
  if result >= 0 then
  begin
    Assert(Items[result] = Data);
    Exit;
  end;

  result := inherited Add(ATime, Data);

  for Index := 0 to BoundaryTypeDataSets.Count - 1 do
  begin
    BoundaryDataSet := BoundaryTypeDataSets[Index];
    BoundaryDataSet.TalksTo(Data);
  end;

end;

procedure TPhastTimeList.Clear;
var
  Index: integer;
  Data: TSparseArrayPhastInterpolationDataSet;
  BoundaryTypeIndex: integer;
  BoundaryTypeDataSet: TDataArray;
begin
  for Index := 0 to Count - 1 do
  begin
    Data := Items[Index];
    for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
    begin
      BoundaryTypeDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
      BoundaryTypeDataSet.StopsTalkingTo(Data);
    end;
  end;
  inherited;
end;

constructor TPhastTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FBoundaryTypeDataSets := TBoundaryTypeList.Create;
end;

destructor TPhastTimeList.Destroy;
var
  Index, BoundaryTypeIndex: integer;
  Data: TSparseArrayPhastInterpolationDataSet;
  BoundaryDataSet: TDataArray;
begin
  for Index := 0 to Count - 1 do
  begin
    Data := Items[Index];
    for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
    begin
      BoundaryDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
      BoundaryDataSet.StopsTalkingTo(Data);
    end;
  end;
  FBoundaryTypeDataSets.Free;
  inherited;
end;

function TPhastTimeList.GetItems(
  const Index: integer): TSparseArrayPhastInterpolationDataSet;
begin
  result := inherited Items[Index] as TSparseArrayPhastInterpolationDataSet;
end;

procedure TPhastTimeList.Initialize;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  BoundaryCollection: TCustomPhastBoundaryCollection;
  ItemIndex: integer;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  DataSet: TSparseArrayPhastInterpolationDataSet;
  Index: integer;
  RowIndex, ColIndex, LayerIndex: integer;
  FirstDataSet: TSparseArrayPhastInterpolationDataSet;
  CellValueR: double;
  CellValueI: integer;
  CellAnnotation: string;
  CellInterpolation: boolean;
  CellDistance1: double;
  CellDistance2: double;
  CellInterpolationDirection: TInterpolationDirection;
  CellValue1R: double;
  CellValue2R: double;
  CellValue1I: integer;
  CellValue2I: integer;
  ColLimit, RowLimit, LayerLimit: integer;
  BoundaryTypeIndex: integer;
  BoundaryTypeDataSet: TDataArray;
  procedure UpdateCellVariables(
    const DataSet: TSparseArrayPhastInterpolationDataSet);
  // UpdateCellVariables stores the values of variables for DataSet
  // at the current location.  These may then be used in UpdateDataSet.
  begin
    CellAnnotation := DataSet.Annotation[LayerIndex, RowIndex, ColIndex]
      + '; t = ' + FloatToStr(Times[Index]);
    DataSet.Annotation[LayerIndex, RowIndex, ColIndex] := CellAnnotation;
    CellInterpolation := DataSet.IsInterpolatedCell[LayerIndex, RowIndex,
      ColIndex];
    if CellInterpolation then
    begin
      CellDistance1 := DataSet.CellDistance1[LayerIndex, RowIndex, ColIndex];
      CellDistance2 := DataSet.CellDistance2[LayerIndex, RowIndex, ColIndex];
      CellInterpolationDirection :=
        DataSet.CellInterpolationDirection[LayerIndex, RowIndex, ColIndex];
    end
    else
    begin
      CellDistance1 := 0;
      CellDistance2 := 1;
      CellInterpolationDirection := pidX;
    end;
    case DataSet.DataType of
      rdtDouble:
        begin
          Assert(DataSet is TSparseRealPhastDataSet);
          CellValueR := DataSet.RealData[LayerIndex, RowIndex, ColIndex];
          if CellInterpolation then
          begin
            CellValue1R :=
              TSparseRealPhastDataSet(DataSet).CellValue1[LayerIndex, RowIndex,
              ColIndex];
            CellValue2R :=
              TSparseRealPhastDataSet(DataSet).CellValue2[LayerIndex, RowIndex,
              ColIndex];
          end
          else
          begin
            CellValue1R := 0;
            CellValue2R := 0;
          end;
          CellValue1I := 0;
          CellValue2I := 0;
        end;
      rdtInteger:
        begin
          Assert(DataSet is TSparseIntegerPhastDataSet);
          CellValueI := DataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
          if CellInterpolation then
          begin
            CellValue1I :=
              TSparseIntegerPhastDataSet(DataSet).CellValue1[LayerIndex,
              RowIndex, ColIndex];
            CellValue2I :=
              TSparseIntegerPhastDataSet(DataSet).CellValue2[LayerIndex,
              RowIndex, ColIndex];
          end
          else
          begin
            CellValue1I := 0;
            CellValue2I := 0;
          end;
          CellValue1R := 0;
          CellValue2R := 0;
        end;
    else
      Assert(False);
    end;
  end;
  procedure UpdateDataSet(const DataSet: TSparseArrayPhastInterpolationDataSet);
  begin
    // UpdateDataSet assigns values to DataSet.
    DataSet.Annotation[LayerIndex, RowIndex, ColIndex] := CellAnnotation;
    DataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
      CellInterpolation;
    if CellInterpolation then
    begin
      DataSet.CellDistance1[LayerIndex, RowIndex, ColIndex] := CellDistance1;
      DataSet.CellDistance2[LayerIndex, RowIndex, ColIndex] := CellDistance2;
      DataSet.CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        CellInterpolationDirection;
    end;
    case DataSet.DataType of
      rdtDouble:
        begin
          Assert(DataSet is TSparseRealPhastDataSet);
          DataSet.RealData[LayerIndex, RowIndex, ColIndex] := CellValueR;
          if CellInterpolation then
          begin
            TSparseRealPhastDataSet(DataSet).CellValue1[LayerIndex, RowIndex,
              ColIndex] := CellValue1R;
            TSparseRealPhastDataSet(DataSet).CellValue2[LayerIndex, RowIndex,
              ColIndex] := CellValue2R;
          end;
        end;
      rdtInteger:
        begin
          Assert(DataSet is TSparseIntegerPhastDataSet);
          DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] := CellValueI;
          if CellInterpolation then
          begin
            TSparseIntegerPhastDataSet(DataSet).CellValue1[LayerIndex, RowIndex,
              ColIndex] := CellValue1I;
            TSparseIntegerPhastDataSet(DataSet).CellValue2[LayerIndex, RowIndex,
              ColIndex] := CellValue2I;
          end;
        end;
    else
      Assert(False);
    end;
  end;
begin
  if UpToDate then
    Exit;

  Clear;

  // This assigns an integer value equal to Ord(BoundaryType)
  // to each cell assigned to the boundary condition.
  for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
  begin
    BoundaryTypeDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
    BoundaryTypeDataSet.Initialize;
  end;

  // Assign values to all data sets related the boundary condition.
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if not AScreenObject.Deleted then
    begin
      BoundaryCollection := AScreenObject.GetBoundaryCollection(self);
      if BoundaryCollection <> nil then
      begin
        for ItemIndex := 0 to BoundaryCollection.Count - 1 do
        begin
          BoundaryCondition := BoundaryCollection.Items[ItemIndex] as
            TCustomPhastBoundaryCondition;
          // GetDataSet creates a data set for the BoundaryCondition.Time
          // if it does not already exist.
          DataSet := BoundaryCollection.GetDataSet(BoundaryCondition.Time);
          if DataSet.DimensionsChanged then
          begin
            frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);
//            DataSet.UpdateDimensions(frmGoPhast.Grid.LayerCount,
//              frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);
            DataSet.SetDimensions(False);
          end;

          AScreenObject.AssignValuesToDataSet(DataSet, Model, lctUse);
        end;
      end;
    end;
  end;

  if Count > 0 then
  begin
    FirstDataSet := Items[0];
    case FirstDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColLimit := FirstDataSet.ColumnCount - 1;
          RowLimit := FirstDataSet.RowCount - 1;
          LayerLimit := FirstDataSet.LayerCount - 1;
        end;
      eaNodes:
        begin
          ColLimit := FirstDataSet.ColumnCount;
          RowLimit := FirstDataSet.RowCount;
          if BoundaryType = btRiver then
          begin
            LayerLimit := frmGoPhast.PhastModel.PhastGrid.LayerCount;
          end
          else
          begin
            LayerLimit := FirstDataSet.LayerCount;
          end;
        end;
    else
      Assert(False);
      ColLimit := -1;
      RowLimit := -1;
      LayerLimit := -1;
    end;

    { TODO : 
Check whether river boundaries and 
specified flux boundaries can occur in the same cell.
See OK.gpt example.}

    // you can't have two different PHAST boundary conditions at the
    // same location so disable boundary if the boundary condition type
    // is incorrect.
    for LayerIndex := 0 to LayerLimit do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if FirstDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
            begin
              BoundaryTypeDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
              Assert(BoundaryTypeDataSet.IsValue
                [LayerIndex, RowIndex, ColIndex]);
              if BoundaryTypeDataSet.IntegerData
                [LayerIndex, RowIndex, ColIndex] <> Ord(BoundaryType) then
              begin
                FirstDataSet.IsValue[LayerIndex, RowIndex, ColIndex] := false;
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    // PHAST Boundary conditions can't be turned off so copy values
    // from previous data sets to subsequent ones
    if Count > 1 then
    begin
      for LayerIndex := 0 to LayerLimit do
      begin
        for RowIndex := 0 to RowLimit do
        begin
          for ColIndex := 0 to ColLimit do
          begin
            if FirstDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              // Index is used in UpdateCellVariables
              Index := 0;
              // store the values at the current location for the first
              // data set.
              UpdateCellVariables(FirstDataSet);

              // Loop over the remaining times.
              for Index := 1 to Count - 1 do
              begin
                DataSet := Items[Index];
                // If DataSet has values at the current location
                // store them.  If not assign values that were stored
                // for the previous time.
                if DataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  UpdateCellVariables(DataSet);
                end
                else
                begin
                  UpdateDataSet(DataSet);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // mark all the data sets as being up to date.
  for Index := 0 to Count - 1 do
  begin
    DataSet := Items[Index];
    DataSet.UpToDate := True;
  end;
  SetUpToDate(True);
end;

procedure TPhastTimeList.Loaded;
var
  Index: integer;
  Data: TSparseArrayPhastInterpolationDataSet;
  BoundaryTypeIndex: integer;
  BoundaryTypeDataSet: TDataArray;
begin
  for Index := 0 to Count - 1 do
  begin
    Data := Items[Index];
    for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
    begin
      BoundaryTypeDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
      BoundaryTypeDataSet.TalksTo(Data);
    end;
  end;
end;

procedure TPhastTimeList.SetItems(const Index: integer;
  const Value: TSparseArrayPhastInterpolationDataSet);
begin
  inherited Items[Index] := Value;
end;

procedure TPhastTimeList.SetUpToDate(const Value: boolean);
var
  BoundaryTypeIndex: integer;
  BoundaryTypeDataSet: TDataArray;
begin
  inherited;
  if not Value then
  begin
    if frmGoPhast.PhastModel <> nil then
    begin
      for BoundaryTypeIndex := 0 to BoundaryTypeDataSets.Count - 1 do
      begin
        BoundaryTypeDataSet := BoundaryTypeDataSets[BoundaryTypeIndex];
        BoundaryTypeDataSet.Invalidate;
      end;

      if frmGoPhast.PhastModel.TopTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateTopCellColors := True;
      end;
      if frmGoPhast.PhastModel.FrontTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := True;
      end;
      if frmGoPhast.PhastModel.SideTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := True;
      end;
    end;
  end;
end;

{ TBoundaryTypeList }
function TBoundaryTypeList.Add(
  const DataSet: TIntegerSparseDataSet): integer;
begin
  result := inherited Add(DataSet);
  DataSet.IsBoundaryTypeDataSet := True;
end;

initialization
  RegisterClasses([TIntegerPhastDataSet, TRealPhastDataSet,
    TSparseIntegerPhastDataSet, TSparseRealPhastDataSet]);

end.
