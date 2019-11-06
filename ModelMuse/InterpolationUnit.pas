{@abstract(@name defines concrete @link(TCustom2DInterpolater)s.)}
unit InterpolationUnit;

interface

uses System.UITypes, Windows, PhastModelUnit, SysUtils, Classes, Controls,
  Dialogs, RbwParser, DataSetUnit,
  frmGoPhastUnit, ScreenObjectUnit, FastGEO, GoPhastTypes, QuadTreeClass,
  SfrInterpolatorUnit, NatNeigh, Contnrs;

{ TODO : 
Consider adding bilateral interpolation method for rectangular point
distributions.  Need to define a rectangular point distribution type. }

{ TODO :
Consider adding a finite element basis function interpolation method using
a Delauny triangulation. }

type
  {@abstract(@name is the abstract ancestor of interpolators that support
   anisotropy.)}
  TCustomAnisotropicInterpolator = class(TCustom2DInterpolater)
  private
    // See @link(Anisotropy).
    FAnisotropy: real;
    // See @link(Anisotropy).
    FNodeOrElementQuadTree: TRbwQuadTree;
    procedure SetAnisotropy(const Value: real);
    // @name returns the minimum and maximum coordinates with which the
    // @classname will need to deal. The limits can be used in initializing
    // a @link(TRbwQuadTree).
    procedure GetLimits(var MinY, MaxY, MinX, MaxX: Real;
      const DataSet: TDataArray);
    procedure InitializeNodeOrElementQuadTree;
    function GetNodeOrElementQuadTree: TRbwQuadTree;
  protected
    // @name initializes the limits of @link(TRbwQuadTree QuadTree)
    // to the grid limits.
    procedure InitializeQuadTreeLimits(const QuadTree: TRbwQuadTree;
      const DataSet: TDataArray);
    // @name sets Expression and sets the values of all the
    // variables used by Expression and then evaluates
    // Expression.
    procedure InitializeVariablesAndExpression(const Location: TPoint2D;
      const AScreenObject: TScreenObject; SectionIndex: integer; var Expression: TExpression);
    property NodeOrElementQuadTree: TRbwQuadTree read GetNodeOrElementQuadTree;
  public
    // @name copies the Anisotropy of Source to the item that calls
    // @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name returns @true if AnotherInterpolator has the same parameters
    // as the @classname being called.
    function SameAs(AnotherInterpolator: TCustom2DInterpolater): boolean;
      override;
    procedure Initialize(const DataSet: TDataArray); override;
  published
    // @name is taken into account when measuring distances by multiplying
    // the Y coordinate by the anisotropy before measuring distances in
    // the normal way.
    //
    // @classname doesn't actually do anything with @name.  Descendants
    // must decide what to do with it.
    property Anisotropy: real read FAnisotropy write SetAnisotropy;
  end;

  {@abstract(@name uses inverse distance squared interpolation to return
   a real-number result.)}
  TInvDistSq2DInterpolator = class(TCustomAnisotropicInterpolator)
  public
    // @name is the name of the interpolator displayed to the user.
    class function InterpolatorName: string; override;
    // @name tells with what types of data this interpolator can be used.
    // (real numbers)
    class function ValidReturnTypes: TRbwDataTypes; override;
    // @name returns the value at location determined by inverse
    // distance squared interpolation.
    function RealResult(const Location: TPoint2D): real; override;
  end;

  TCustomPoint2DInterpolator = class(TCustomAnisotropicInterpolator)
  protected
    // @name stores real number data at each point.
    FRealData: array of real;
    // @name stores integer data at each point.
    FIntegerData: array of integer;
    // @name stores boolean data at each point.
    FBooleanData: array of boolean;
    // @name stores string data at each point.
    FStringData: array of string;
    // @name sets the length of @link(FRealData), @link(FIntegerData),
    // @link(FBooleanData) or @link(FStringData) depending on the
    // @link(TRbwDataType) of the data set.
    procedure SetArraySize(const DataSet: TDataArray; Count: Integer); virtual;
    // @name stores a data values at APoint.
    procedure StoreDataValue(Count: Integer; const DataSet: TDataArray;
      APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer);
      virtual; abstract;
    // @name stores data values for all the relevant @link(TScreenObject)s.
    procedure StoreData(Sender: TObject; const DataSet: TDataArray); virtual;
  public
    // @name creates an instance of @classname and assigns
    // @link(TCustom2DInterpolater.OnInitialize).
    constructor Create(AOwner: TComponent); override;
    procedure Finalize(const DataSet: TDataArray); override;
  end;

  {@abstract(@name implements an interpolator that returns the value
   of the @link(TScreenObject) which has a vertex closest
   to the point of interest.)}
  TNearestPoint2DInterpolator = class(TCustomPoint2DInterpolator)
  private
    // @name is used to find the closest node on an object
    // to the location of interest.
    FQuadTree: TRbwQuadTree;
    // @name is used to finde the closest @link(TScreenObject) to the
    // location of interest.
    FScreenObjectQuadTree: TRbwQuadTree;
    // @name is used to assign an annotation to an interpolated values.
    FLastScreenObject: TScreenObject;
    // @name gets the address of the result and sets @link9FLastScreenObject).
    function GetPointerToResult(const Location: TPoint2D): Pointer;
    // @name gets the @link(TScreenObject) closest to Location.
    function GetScreenObjectAtLocation(const Location: TPoint2D): TScreenObject;
    // @name gets a pointer to the closest thing in QuadTree to Location.
    function GetQuadTreePointer(QuadTree: TRbwQuadTree;
      const Location: TPoint2D): Pointer;
  protected
    // @name initializes @link(TRbwQuadtTree.MaxPoints) in
    // @link(FQuadTree) and @link(FScreenObjectQuadTree)
    // after calling inherited.
    procedure SetArraySize(const DataSet: TDataArray; Count: Integer); override;
    // @name initializes
    // @link(FQuadTree) and @link(FScreenObjectQuadTree)
    // and calls inherited.
    procedure StoreData(Sender: TObject; const DataSet: TDataArray); override;
    // @name stores a single data value in
    // @link(FQuadTree) and @link(FScreenObjectQuadTree)
    // and in one of the arrays of @link(TCustomPoint2DInterpolator)
    procedure StoreDataValue(Count: Integer; const DataSet: TDataArray;
      APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer); override;
  public
    // @name clears
    // @link(FQuadTree) and @link(FScreenObjectQuadTree).
    procedure Finalize(const DataSet: TDataArray); override;
    // @name is the name of the interpolator displayed to the user.
    class function InterpolatorName: string; override;
    // @name returns the @link(TRbwDataTypes) valid with @Classname.
    class function ValidReturnTypes: TRbwDataTypes; override;
    // @name gets the boolean value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function BooleanResult(const Location: TPoint2D): boolean; override;
    // @name gets the integer value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function IntegerResult(const Location: TPoint2D): integer; override;
    // @name gets the real-number value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function RealResult(const Location: TPoint2D): real; override;
    // @name gets the string value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function StringResult(const Location: TPoint2D): string; override;
    // @name creates
    // @link(FQuadTree) and @link(FScreenObjectQuadTree).
    constructor Create(AOwner: TComponent); override;
    // @name returns an annotation based on @link(FLastScreenObject)
    function LastAnnotation: string; override;
  end;

  TInvDistSqPoint2DInterpolator = class(TCustomPoint2DInterpolator)
  private
    // @names stores the locations of the data values
    FLocations: array of TPoint2D;
  protected
    // @name sets the size of @link(FLocations) and calls inherited.
    procedure SetArraySize(const DataSet: TDataArray; Count: Integer); override;
    // @name stores a data value and its location.
    procedure StoreDataValue(Count: Integer; const DataSet: TDataArray;
      APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer); override;
  public
    // @name clears @link(FLocations) and calls inherited.
    procedure Finalize(const DataSet: TDataArray); override;
    // @name is the name of the interpolator displayed to the user.
    class function InterpolatorName: string; override;
    // @name tells with what types of data this interpolator can be used.
    // (real numbers)
    class function ValidReturnTypes: TRbwDataTypes; override;
    // @name returns the value at location determined by inverse
    // distance squared interpolation.
    function RealResult(const Location: TPoint2D): real; override;
  end;

  TArrayLocation = record
    Dimen1: Integer;
    Dimen2: Integer;
  end;

  TInterpolateScreenObject = class(TScreenObject)
  public
    property Model: TBaseModel read FModel write FModel;
  end;

  TPointAverageInterpolator = class(TCustomPoint2DInterpolator)
  private
    FValues: TTwoDRealArray;
    FCount: TTwoDIntegerArray;
    FInternalScreenObject: TInterpolateScreenObject;
    FCellList: TCellAssignmentList;
    function PointToArrayLocation(APoint: TPoint2D): TArrayLocation;
  protected
    procedure SetArraySize(const DataSet: TDataArray; Count: Integer); override;
    procedure StoreDataValue(Count: Integer; const DataSet: TDataArray;
      APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer);
      override;
    procedure StoreData(Sender: TObject; const DataSet: TDataArray); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function InterpolatorName: string; override;
    class function ValidReturnTypes: TRbwDataTypes; override;
    function RealResult(const Location: TPoint2D): real; override;
  end;

  TScreenObjectSectionStorage = class(TObject)
    ScreenObject: TScreenObject;
    SectionIndex: integer;
  end;

  {@abstract(@name implements an interpolator that returns the value
   of the @link(TScreenObject) nearest to the point of interest.)}
  TNearest2DInterpolator = class(TCustomAnisotropicInterpolator)
  private
    // @name is a TObjectList.
    // @name stores @link(TScreenObjectSectionStorage)s.
    FStoredLocations: TList;
    // @name is the TExpression used to assign the result of the
    // interpolation.
    FExpression: TExpression;
    // @name is used to find the closest node on an object
    // to the location of interest.
    FQuadTree: TRbwQuadTree;
    // @name stores all the @link(TScreenObject)s
    // that set the @link(TDataArray)s
    // for this @classname by interpolation.
    FListOfTScreenObjects : TList;
    // @name is used in @link(LastAnnotation).
    FNearestScreenObject: TScreenObject;
    // @name gets the TExpression for the nearest @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    // sets the variable for it and evaluates it.
    // If there is no closest @link(TScreenObject), @name returns nil.
    function GetExpression(const Location: TPoint2D): TExpression;
  protected
    //  @name adds all the @link(TScreenObject)s that set
    //  DataSet by interpolation to @link(FQuadTree) at each of the
    // @link(TScreenObject)s nodes
    // and then adds all the @link(TScreenObject)
    // that set DataSet by interpolation to @link(FListOfTScreenObjects).
    procedure StoreData(Sender: TObject; const DataSet: TDataArray);
    // @name returns the @link(TScreenObject) that is closest to Location.
    // ClosestLocation is set to the location on the @link(TScreenObject)
    // that is closest to Location.
    //
    // ClosestLocation may be either at a node or along an edge of the
    // @classname.
    function GetNearestScreenObject(const Location: TPoint2D;
      out ClosestLocation: TPoint2D; out SectionIndex: integer): TScreenObject;
  public
    procedure Finalize(const DataSet: TDataArray); override;
    // @name creates an instance of @classname and assigns
    // @link(TCustom2DInterpolater.OnInitialize).
    constructor Create(AOwner: TComponent); override;
    // @name is the name of the interpolator displayed to the user.
    class function InterpolatorName: string; override;
    // @name destroys an instance of @classname.
    destructor Destroy; override;
    // @name gets the boolean value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function BooleanResult(const Location: TPoint2D): boolean; override;
    // @name gets the integer value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function IntegerResult(const Location: TPoint2D): integer; override;
    // @name gets the real-number value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function RealResult(const Location: TPoint2D): real; override;
    // @name gets the string value at Location of the closest
    // @link(TScreenObject)
    // that sets TCustom2DInterpolater.@link(TCustom2DInterpolater.DataSet)
    // by interpolation,
    function StringResult(const Location: TPoint2D): string; override;
    // @name tells what types of data with which this interpolator can be used.
    // (integer, real-number, boolean, and string)
    class function ValidReturnTypes: TRbwDataTypes; override;
    function LastAnnotation: string; override;
  end;

  TCustomTriangleInterpolator = class(TCustomAnisotropicInterpolator)
  public
    // @name returns @true if interpolation is possible.
    function ShouldInterpolate: boolean; override;
  end;

  TNaturalNeighborInterp = class(TCustomTriangleInterpolator)
  private
    FNatNeigh: TNaturalNeighborInterpolatorTripack;
    FSinNatNeigh: TNaturalNeighborInterpolatorTripack;
    FCosNatNeigh: TNaturalNeighborInterpolatorTripack;
    procedure StoreData(Sender: TObject; const DataSet: TDataArray);
  protected
    procedure SetEpsilonX(const Value: Double); override;
    procedure SetEpsilonY(const Value: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Finalize(const DataSet: TDataArray); override;
    destructor Destroy; override;
    class function InterpolatorName: string; override;
    class function ValidReturnTypes: TRbwDataTypes; override;
    function RealResult(const Location: TPoint2D): real; override;
  end;

  {@abstract(@name uses inverse distance squared interpolation to return
   a real-number result.)}
  TCustomSfrpackInterpolator = class(TCustomTriangleInterpolator)
  private
    FValidData: boolean;
    FSfrInterpolator: TSfrInterpolator;
    FSinSfrInterpolator: TSfrInterpolator;
    FCosSfrInterpolator: TSfrInterpolator;
  public
    // @name tells with what types of data this interpolator can be used.
    // (real numbers)
    class function ValidReturnTypes: TRbwDataTypes; override;
    procedure StoreData(Sender: TObject; const DataSet: TDataArray);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Finalize(const DataSet: TDataArray); override;
  end;

  TLinearSfrpackInterpolator = class(TCustomSfrpackInterpolator)
  public
    class function InterpolatorName: string; override;
    function RealResult(const Location: TPoint2D): real; override;
  end;

  TFittedSurfaceIntepolator = class(TCustomSfrpackInterpolator)
  public
    class function InterpolatorName: string; override;
    function RealResult(const Location: TPoint2D): real; override;
  end;

  EPlProcException = class(Exception);

  TCustomPlProcInterpolator = class(TCustomAnisotropicInterpolator)
  private
    FValues: TPoint3DArray;
    ResultValues: array of double;
    FInputFileName: string;
    FResultLocationFileName: string;
    FScriptFileName: string;
    FFilesToDelete: TStringList;
    FKrigingOutputFileName: string;
    FKrigedResults: TRbwQuadTree;
    procedure WriteScript(const DataSet: TDataArray);
    procedure OnPlProcDone(Sender: TObject; ExitCode: DWORD);
    procedure ReadResults;
  protected
    procedure StoreDataValue(Count: Integer; const DataSet: TDataArray;
      APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer);
    procedure StoreData(Sender: TObject; const DataSet: TDataArray); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function InterpolatorName: string; override;
    class function ValidReturnTypes: TRbwDataTypes; override;
    // @name returns the value at location determined by inverse
    // distance squared interpolation.
    function RealResult(const Location: TPoint2D): real; override;
    procedure Finalize(const DataSet: TDataArray); override;
  end;


implementation

uses Math, AbstractGridUnit, RealListUnit, TripackTypes, GIS_Functions, Types,
  Generics.Collections, MeshRenumberingTypes, frmErrorsAndWarningsUnit,
  PlProcUnit, TempFiles, Vcl.Forms, System.IOUtils, ModelMuseUtilities;

resourcestring
  StrErrorEncoutereredI = 'Error encouterered in initializing %0:s for the ' +
  '%1:s.  Error was %2:s';
  Str0sUsing1s = '%0:s using %1:s';
  StrErrorAssigningValu = 'Error assigning value to interpolate';
  StrErrorMessage0 = 'Error message = "%0:s" in %1:s';

type
  TSortRecord = record
    Location : TPoint2D;
    Value: real;
    OriginalIndex: integer;
    Duplicate: boolean;
  end;
  PSortRecord = ^TSortRecord;

function SortRecordCompare(Item1, Item2: Pointer): integer;
var
  PSort1, PSort2: PSortRecord;
  Value1, Value2: single;
begin
  PSort1 := Item1;
  PSort2 := Item2;
  // reduce precision to single precision
  Value1 := PSort1.Location.X;
  Value2 := PSort2.Location.X;
  result := Sign(Value1 - Value2);
  if result = 0 then
  begin
    // reduce precision to single precision
    Value1 := PSort1.Location.Y;
    Value2 := PSort2.Location.Y;
    result := Sign(Value1 - Value2);
    if result = 0 then
    begin
      result := PSort1.OriginalIndex - PSort2.OriginalIndex;
    end;
  end;
end;

function InvertPoint(APoint: TPoint2D): TPoint2D;
begin
  result.x := APoint.y;
  result.y := APoint.x;
end;

{ TNearest2DInterpolator }

class function TNearest2DInterpolator.InterpolatorName: string;
begin
  result := 'Nearest';
end;

function TNearest2DInterpolator.LastAnnotation: string;
begin
  result := inherited;
  if FNearestScreenObject <> nil then
  begin
    result := Format(Str0sUsing1s, [result, FNearestScreenObject.Name]);
  end;
end;

destructor TNearest2DInterpolator.Destroy;
begin
  FListOfTScreenObjects.Free;
  FStoredLocations.Free;
  inherited;
end;

procedure TNearest2DInterpolator.Finalize(const DataSet: TDataArray);
begin
  FQuadTree.Clear;
  FListOfTScreenObjects.Clear;
  inherited;
end;

{ TInvDistSq2DInterpolator }

class function TInvDistSq2DInterpolator.InterpolatorName: string;
begin
  result := 'Inv. Dist. Sq.';
end;

function TInvDistSq2DInterpolator.RealResult(const Location: TPoint2D):
  real;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
  ScreenObjectDistance: real;
  ClosestLocation: TPoint2D;
  ScreenObjectFunction: string;
  Expression: TExpression;
  VariablesUsed: TStringList;
  Cell: T3DCell;
  TopCell: T2DTopCell;
  ADataSet: TDataArray;
  VariableIndex: integer;
  Variable: TCustomValue;
  Weight: real;
  SumWeights: real;
  Sum: real;
  Value: real;
  Compiler: TRbwParser;
  InnerIndex: integer;
  ListOfScreenObjects: TList;
  LocalLocation: TPoint2D;
  LocalAnisotropy: Real;
  SectionIndex: Integer;
  NearestSegment: TCellElementSegment;
  SumSin: real;
  SumCos: Real;
  ErrorFunction: string;
  LocalModel: TCustomModel;
begin
  SumWeights := 0;
  Sum := 0;
  SumSin:= 0;
  SumCos:= 0;
  LocalModel := CurrentModel as TCustomModel;

  ListOfScreenObjects := TList.Create;
  try
    FillScreenObjectList(ListOfScreenObjects);

    // Check objects in reverse order because later objects should
    // override earlier ones, so if there is a tie, the later one
    // should win.

    Assert(ListOfScreenObjects.Count > 0);
    AScreenObject := ListOfScreenObjects[0];
    LocalLocation := Location;
    LocalAnisotropy := Anisotropy;
    if AScreenObject.ViewDirection = vdSide then
    begin
      LocalLocation := InvertPoint(Location);
      LocalAnisotropy := 1/Anisotropy
    end;

    for Index := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[Index];

      DataSetIndex := AScreenObject.IndexOfDataSet(DataSet);
      Assert(DataSetIndex >= 0);

      ScreenObjectDistance := AScreenObject.DistanceToScreenObject(
        LocalLocation, ClosestLocation, LocalAnisotropy, SectionIndex);


      ScreenObjectFunction := AScreenObject.DataSetFormulas[DataSetIndex];
      ErrorFunction := ScreenObjectFunction;
      Compiler := frmGoPhast.PhastModel.GetCompiler(DataSet.Orientation,
        DataSet.EvaluatedAt);
      try
        Compiler.Compile(ScreenObjectFunction);
      except on E: ERbwParserError do
        begin
          ResetScreenObjectFunction(DataSetIndex, AScreenObject, Compiler,
            DataSet.DataType, E.Message, False, ErrorFunction);
        end;
      end;

      Expression := Compiler.CurrentExpression;
      VariablesUsed := Expression.VariablesUsed;
      if VariablesUsed.Count > 0 then
      begin
        case frmGoPhast.ModelSelection of
          msUndefined: Assert(False);
          msPhast:
            begin
              Cell := frmGoPhast.PhastGrid.GetCell(ClosestLocation,
                AScreenObject.ViewDirection, DataSet.EvaluatedAt);
            end;
          msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
            msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
            begin
              TopCell := LocalModel.Grid.TopContainingCell(ClosestLocation,
                DataSet.EvaluatedAt);
              Cell.Col := TopCell.Col;
              Cell.Row := TopCell.Row;
              Cell.Lay := 0;
            end;
          msSutra22, msSutra30:
            begin
              TopCell := frmGoPhast.PhastModel.Mesh3D.TopContainingCellOrElement(
                ClosestLocation, DataSet.EvaluatedAt);
              Cell.Col := TopCell.Col;
              Cell.Row := TopCell.Row;
              Cell.Lay := 0;
            end;
          else Assert(False);
        end;
        for InnerIndex := 0 to VariablesUsed.Count - 1 do
        begin
          ADataSet :=
            LocalModel.DataArrayManager.GetDataSetByName(VariablesUsed[InnerIndex]);
          if ADataSet <> nil then
          begin
            ADataSet.Initialize;
          end;
        end;
        for InnerIndex := 0 to VariablesUsed.Count - 1 do
        begin
          ADataSet :=
            LocalModel.DataArrayManager.GetDataSetByName(VariablesUsed[InnerIndex]);
          if ADataSet <> nil then
          begin
            VariableIndex := Compiler.IndexOfVariable(
              VariablesUsed[InnerIndex]);
            Variable := Compiler.Variables[VariableIndex];
            Assert(Variable.ResultType = ADataSet.DataType);
            case Variable.ResultType of
              rdtDouble:
                begin
                  TRealVariable(Variable).Value :=
                    ADataSet.RealData[Cell.Lay, Cell.Row, Cell.Col]
                end;
              rdtInteger:
                begin
                  TIntegerVariable(Variable).Value :=
                    ADataSet.IntegerData[Cell.Lay, Cell.Row, Cell.Col]
                end;
              rdtBoolean:
                begin
                  TBooleanVariable(Variable).Value :=
                    ADataSet.BooleanData[Cell.Lay, Cell.Row, Cell.Col]
                end;
              rdtString:
                begin
                  TStringVariable(Variable).Value :=
                    ADataSet.StringData[Cell.Lay, Cell.Row, Cell.Col]
                end;
            else
              Assert(False);
            end;
          end;
        end;
      end;
      UpdateCurrentScreenObject(AScreenObject);
      NearestSegment := AScreenObject.Segments[FModel].ClosestSegment(
        ClosestLocation, LocalAnisotropy);
      UpdateCurrentSegment(NearestSegment);
//      if NearestSegment = nil then
      begin
        UpdateCurrentSection(SectionIndex);
      end;
      AScreenObject.UpdateImportedValues(DataSet);
      EvaluateExpression(Compiler, Expression, AScreenObject);

      UpdateCurrentScreenObject(AScreenObject);
      UpdateCurrentSegment(NearestSegment);
//      if NearestSegment = nil then
      begin
        UpdateCurrentSection(SectionIndex);
      end;
      Value := Expression.DoubleResult;
      if ScreenObjectDistance = 0 then
      begin
        result := Value;
        Exit;
      end
      else
      begin
        Weight := 1 / Sqr(ScreenObjectDistance);
        SumWeights := SumWeights + Weight;

        case DataSet.AngleType of
          atNone:
            begin
              Sum := Sum + Value * Weight;
            end;
          atDegrees:
            begin
              Value := Value/180*Pi;
              SumSin := SumSin + Sin(Value)*Weight;
              SumCos := SumCos + Cos(Value)*Weight;
            end;
          atRadians:
            begin
              SumSin := SumSin + Sin(Value)*Weight;
              SumCos := SumCos + Cos(Value)*Weight;
            end;
          else Assert(False);
        end;

      end;
    end;
    if SumWeights = 0 then
    begin
      result := 0;
    end
    else
    begin
      result := 0;
      case DataSet.AngleType of
        atNone:
          begin
            result := Sum / SumWeights;
          end;
        atDegrees:
          begin
            if (SumSin = 0) and (SumCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(SumSin, SumCos);
              result := result/pi*180;
            end;
          end;
        atRadians:
          begin
            if (SumSin = 0) and (SumCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(SumSin, SumCos);
            end;
          end;
        else Assert(False)
      end;

    end;

  finally
    ListOfScreenObjects.Free;
  end;
end;

class function TInvDistSq2DInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble];
end;

{ TCustomAnisotropicInterpolator }

procedure TCustomAnisotropicInterpolator.Assign(Source: TPersistent);
begin
  if Source is TCustomAnisotropicInterpolator then
  begin
    Anisotropy := TCustomAnisotropicInterpolator(Source).Anisotropy
  end;
  inherited;
end;

constructor TCustomAnisotropicInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  FAnisotropy := 1;
end;

destructor TCustomAnisotropicInterpolator.Destroy;
begin
  FNodeOrElementQuadTree.Free;
  inherited;
end;

procedure TCustomAnisotropicInterpolator.Initialize(const DataSet: TDataArray);
begin
  FreeAndNil(FNodeOrElementQuadTree);
  inherited;
end;

procedure TCustomAnisotropicInterpolator.InitializeNodeOrElementQuadTree;
var
//  Mesh: TSutraMesh2D;
//  AnElement: TSutraElement2D;
  APoint: TPoint2D;
  Index: Integer;
//  ANode: TSutraNode2D;
  Mesh: IMesh2D;
  AnElement: IElement2D;
  ANode: INode2D;
begin
  if FNodeOrElementQuadTree = nil then
  begin
    FNodeOrElementQuadTree := TRbwQuadTree.Create(Nil);
    InitializeQuadTreeLimits(FNodeOrElementQuadTree, DataSet);
    Mesh := (FModel as TCustomModel).Mesh3D.Mesh2DI;
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          for Index := 0 to Mesh.ElementCount - 1 do
          begin
            AnElement := Mesh.ElementsI2D[Index];
            APoint := AnElement.Center;
            APoint.y := APoint.y*Anisotropy;
            FNodeOrElementQuadTree.AddPoint(APoint.x, APoint.y, AnElement);
          end;
        end;
      eaNodes:
        begin
          for Index := 0 to Mesh.NodeCount - 1 do
          begin
            ANode := Mesh.NodesI2D[Index];
            FNodeOrElementQuadTree.AddPoint(ANode.Location.x, ANode.Location.y*Anisotropy, ANode);
          end;
        end;
    end;
  end;
end;

procedure TCustomAnisotropicInterpolator.InitializeQuadTreeLimits(
  const QuadTree: TRbwQuadTree; const DataSet: TDataArray);
var
  MinY: Real;
  MaxY: Real;
  MinX: Real;
  MaxX: Real;
begin
  QuadTree.Clear;
  GetLimits(MinY, MaxY, MinX, MaxX, DataSet);
  QuadTree.Xmax := MaxX;
  QuadTree.Xmin := MinX;
  QuadTree.Ymax := MaxY;
  QuadTree.Ymin := MinY;
end;

function TCustomAnisotropicInterpolator.SameAs(
  AnotherInterpolator: TCustom2DInterpolater): boolean;
begin
  result := inherited SameAs(AnotherInterpolator)
    and (Anisotropy = (AnotherInterpolator
    as TCustomAnisotropicInterpolator).Anisotropy);
end;

procedure TCustomAnisotropicInterpolator.GetLimits(
  var MinY, MaxY, MinX, MaxX: Real; const DataSet: TDataArray);
var
  ScreenObject: TScreenObject;
  Index: Integer;
  ListOfScreenObjects: TList;
  APoint: TPoint2D;
  LocalGrid: TCustomModelGrid;
//  LocalMesh: TSutraMesh3D;
//  Node: TSutraNode2D;
  NodeIndex: Integer;
  LocalMesh: IMesh3D;
  Node: INode2D;
begin
  Assert(DataSet <> nil);
  LocalGrid := (DataSet.Model as TCustomModel).Grid;
  if LocalGrid = nil then
  begin
    LocalMesh := (DataSet.Model as TCustomModel).Mesh3D;
  end
  else
  begin
    LocalMesh := nil;
  end;
  case DataSet.Orientation of
    dsoTop:
      begin
        if LocalGrid = nil then
        begin
          if (LocalMesh = nil) or (LocalMesh.Mesh2DI.NodeCount = 0) then
          begin
            MaxX := 0;
            MinX := 0;
            MaxY := 0;
            MinY := 0;
          end
          else
          begin
            Node := LocalMesh.Mesh2DI.NodesI2D[0];
            MaxX := Node.Location.X;
            MinX := Node.Location.X;
            MaxY := Node.Location.Y;
            MinY := Node.Location.Y;
            for NodeIndex := 1 to LocalMesh.Mesh2DI.NodeCount - 1 do
            begin
              Node := LocalMesh.Mesh2DI.NodesI2D[NodeIndex];
              if Node.Location.X > MaxX then
              begin
                MaxX := Node.Location.X;
              end;
              if Node.Location.X < MinX then
              begin
                MinX := Node.Location.X;
              end;
              if Node.Location.Y > MaxY then
              begin
                MaxY := Node.Location.Y;
              end;
              if Node.Location.Y < MinY then
              begin
                MinY := Node.Location.Y;
              end;
            end;
            MaxY := MaxY*Anisotropy;
            MinY := MinY*Anisotropy;
          end;
        end
        else
        begin
          Assert(LocalGrid.ColumnCount > 0);
          Assert(LocalGrid.RowCount > 0);
          APoint := LocalGrid.TwoDElementCorner(0, 0);
          MaxX := APoint.X;
          MinX := APoint.X;
          MaxY := APoint.Y * Anisotropy;
          MinY := APoint.Y * Anisotropy;
          APoint :=
            LocalGrid.TwoDElementCorner(LocalGrid.ColumnCount, 0);
          if APoint.X > MaxX then
          begin
            MaxX := APoint.X;
          end;
          if APoint.X < MinX then
          begin
            MinX := APoint.X;
          end;
          if APoint.Y * Anisotropy > MaxY then
          begin
            MaxY := APoint.Y * Anisotropy;
          end;
          if APoint.Y * Anisotropy < MinY then
          begin
            MinY := APoint.Y * Anisotropy;
          end;
          APoint := LocalGrid.TwoDElementCorner(LocalGrid.ColumnCount,
            LocalGrid.RowCount);
          if APoint.X > MaxX then
          begin
            MaxX := APoint.X;
          end;
          if APoint.X < MinX then
          begin
            MinX := APoint.X;
          end;
          if APoint.Y * Anisotropy > MaxY then
          begin
            MaxY := APoint.Y * Anisotropy;
          end;
          if APoint.Y * Anisotropy < MinY then
          begin
            MinY := APoint.Y * Anisotropy;
          end;
          APoint := LocalGrid.TwoDElementCorner(0, LocalGrid.RowCount);
          if APoint.X > MaxX then
          begin
            MaxX := APoint.X;
          end;
          if APoint.X < MinX then
          begin
            MinX := APoint.X;
          end;
          if APoint.Y * Anisotropy > MaxY then
          begin
            MaxY := APoint.Y * Anisotropy;
          end;
          if APoint.Y * Anisotropy < MinY then
          begin
            MinY := APoint.Y * Anisotropy;
          end;
        end;
      end;
    dsoFront:
      begin
        Assert(frmGoPhast.Grid.ColumnCount > 0);
        Assert(frmGoPhast.Grid.LayerCount > 0);
        MaxX := frmGoPhast.Grid.ColumnPosition[frmGoPhast.Grid.ColumnCount];
        MinX := frmGoPhast.Grid.ColumnPosition[0];
        MaxY := frmGoPhast.Grid.HighestElevation * Anisotropy;
        MinY := frmGoPhast.Grid.LowestElevation * Anisotropy;
      end;
    dsoSide:
      begin
        Assert(frmGoPhast.Grid.RowCount > 0);
        Assert(frmGoPhast.Grid.LayerCount > 0);
        MaxX := frmGoPhast.Grid.RowPosition[frmGoPhast.Grid.RowCount];
        MinX := frmGoPhast.Grid.RowPosition[0];
        MaxY := frmGoPhast.Grid.HighestElevation * Anisotropy;
        MinY := frmGoPhast.Grid.LowestElevation * Anisotropy;
      end;
    dso3D:
      begin
      end;
  else
    Assert(False);
  end;
  ListOfScreenObjects := TList.Create;
  try
    FillScreenObjectList(ListOfScreenObjects);
    if ListOfScreenObjects.Count > 0 then
    begin
      for Index := 0 to 100 - 1 do
      begin
        ScreenObject := ListOfScreenObjects[Random(ListOfScreenObjects.Count)];
        if ScreenObject.Count > 0 then
        begin
          APoint := ScreenObject.Points[Random(ScreenObject.Count)];
          case ScreenObject.ViewDirection of
            vdTop:
              begin
                if LocalGrid <> nil then
                begin
                  APoint := LocalGrid.
                    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
                end;
              end;
            vdFront: ; // do nothing
            vdSide:
              begin
                APoint := InvertPoint(APoint);
              end;
          end;
          APoint.Y := APoint.Y * Anisotropy;
          if APoint.x < MinX then
          begin
            MinX := APoint.x;
          end
          else if APoint.x > MaxX then
          begin
            MaxX := APoint.x;
          end;
          if APoint.y < MinY then
          begin
            MinY := APoint.y;
          end
          else if APoint.y > MaxY then
          begin
            MaxY := APoint.y;
          end;
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

function TCustomAnisotropicInterpolator.GetNodeOrElementQuadTree: TRbwQuadTree;
begin
  InitializeNodeOrElementQuadTree;
  result := FNodeOrElementQuadTree;
end;

procedure TCustomAnisotropicInterpolator.SetAnisotropy(const Value: real);
begin
  if Value <= 0 then
  begin
    FAnisotropy := 1;
  end
  else
  begin
    FAnisotropy := Value;
  end;
end;

{ TNearestPoint2DInterpolator }

constructor TCustomPoint2DInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  OnInitialize := StoreData;
end;

procedure TCustomPoint2DInterpolator.Finalize(const DataSet: TDataArray);
begin
  inherited;
  SetLength(FRealData, 0);
  SetLength(FIntegerData, 0);
  SetLength(FBooleanData, 0);
  SetLength(FStringData, 0);
end;

procedure TCustomPoint2DInterpolator.SetArraySize(
  const DataSet: TDataArray; Count: Integer);
begin
  case DataSet.DataType of
    rdtDouble:
      begin
        SetLength(FRealData, Count);
      end;
    rdtInteger:
      begin
        SetLength(FIntegerData, Count);
      end;
    rdtBoolean:
      begin
        SetLength(FBooleanData, Count);
      end;
    rdtString:
      begin
        SetLength(FStringData, Count);
      end;
  else
    Assert(False);
  end;
end;

function TNearest2DInterpolator.BooleanResult(
  const Location: TPoint2D): boolean;
var
  Expression: TExpression;
begin
  Expression := GetExpression(Location);
  if Expression = nil then
  begin
    result := inherited BooleanResult(Location);
  end
  else
  begin
    result := FExpression.BooleanResult;
  end;
end;

function TNearest2DInterpolator.IntegerResult(
  const Location: TPoint2D): integer;
var
  Expression: TExpression;
begin
  Expression := GetExpression(Location);
  if Expression = nil then
  begin
    result := inherited IntegerResult(Location);
  end
  else
  begin
    result := FExpression.IntegerResult;
  end;
end;

function TNearest2DInterpolator.RealResult(const Location: TPoint2D): real;
var
  Expression: TExpression;
begin
  Expression := GetExpression(Location);
  if Expression = nil then
  begin
    result := inherited RealResult(Location);
  end
  else
  begin
    result := FExpression.DoubleResult;
  end;
end;

function TNearest2DInterpolator.StringResult(const Location: TPoint2D): string;
var
  Expression: TExpression;
begin
  Expression := GetExpression(Location);
  if Expression = nil then
  begin
    result := inherited StringResult(Location);
  end
  else
  begin
    result := FExpression.StringResult;
  end;
end;

class function TNearest2DInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble, rdtInteger, rdtBoolean, rdtString];
end;

constructor TNearest2DInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  FQuadTree := TRbwQuadTree.Create(self);
  FListOfTScreenObjects := TList.Create;
  FStoredLocations := TObjectList.Create;

  OnInitialize := StoreData;
end;

function TNearest2DInterpolator.GetExpression(
  const Location: TPoint2D): TExpression;
var
  ClosestLocation: TPoint2D;
  SectionIndex: integer;
begin
  result := nil;
  FNearestScreenObject := GetNearestScreenObject(Location, ClosestLocation, SectionIndex);
  if FNearestScreenObject <> nil then
  begin
    InitializeVariablesAndExpression(ClosestLocation, FNearestScreenObject, SectionIndex,
      FExpression);
    result := FExpression;
  end;
end;

procedure TNearest2DInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  PointCount: integer;
  PointIndex: integer;
  APoint: TPoint2D;
  SectionIndex: integer;
  Storage: TScreenObjectSectionStorage;
begin
  InitializeQuadTreeLimits(FQuadTree, DataSet);

  FillScreenObjectList(FListOfTScreenObjects);

  // Check objects in reverse order because later objects should
  // override earlier ones, so if there is a tie, the later one
  // should win.

  for ScreenObjectIndex := FListOfTScreenObjects.Count - 1 downto 0 do
  begin
    AScreenObject := FListOfTScreenObjects[ScreenObjectIndex];

    PointCount := AScreenObject.Count;
    SectionIndex := 0;
    for PointIndex := 0 to PointCount - 1 do
    begin
      if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
      begin
        Inc(SectionIndex);
        if AScreenObject.SectionClosed[SectionIndex-1] then
        begin
          Continue;
        end;
      end;
      APoint := AScreenObject.Points[PointIndex];
      if AScreenObject.ViewDirection = vdSide then
      begin
        APoint := InvertPoint(APoint);
      end;
      Storage := TScreenObjectSectionStorage.Create;
      FStoredLocations.Add(Storage);
      Storage.ScreenObject := AScreenObject;
      Storage.SectionIndex := SectionIndex-1;

      FQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy, Storage);
    end;
    AScreenObject.CacheSegments;
  end;
end;

procedure TCustomAnisotropicInterpolator.InitializeVariablesAndExpression(
  const Location: TPoint2D; const AScreenObject: TScreenObject; SectionIndex: integer;
  var Expression: TExpression);
var
  Index: integer;
  DataSetIndex: Integer;
  ScreenObjectFunction: string;
  Compiler: TRbwParser;
  VariablesUsed: TStringList;
  Cell: T3DCell;
  ADataSet: TDataArray;
  VariableIndex: integer;
  Variable: TCustomValue;
  IsBoundary: boolean;
  TopCell: T2DTopCell;
  NearestSegment: TCellElementSegment;
  Model: TCustomModel;
  ErrorFunction: string;
  APointer: Pointer;
  AnElement2D: IElement2D;
  ANode2D: INode2D;
begin
  Assert(DataSet <> nil);
  DataSetIndex := AScreenObject.IndexOfDataSet(DataSet);
  if DataSetIndex >= 0 then
  begin
    IsBoundary := False;
    AScreenObject.UpdateImportedValues(DataSet);
    ScreenObjectFunction := AScreenObject.DataSetFormulas[DataSetIndex];
  end
  else
  begin
     { TODO : I don't think it can ever get here. }
    IsBoundary := True;
    DataSetIndex := AScreenObject.IndexOfBoundaryDataSet(DataSet);
    Assert(DataSetIndex >= 0);
    ScreenObjectFunction :=
      AScreenObject.BoundaryDataSetFormulas[DataSetIndex];
  end;

  Model := DataSet.Model as TCustomModel;
  Assert(FModel = Model);
  Compiler := Model.GetCompiler(
    DataSet.Orientation, DataSet.EvaluatedAt);
  ErrorFunction := ScreenObjectFunction;
  try
    Compiler.Compile(ScreenObjectFunction);
  except on E: ERbwParserError do
    begin
      ResetScreenObjectFunction(DataSetIndex, AScreenObject, Compiler,
        DataSet.DataType, E.Message, IsBoundary, ErrorFunction);
    end;
  end;

  Expression := Compiler.CurrentExpression;
  VariablesUsed := Expression.VariablesUsed;
  case Model.ModelSelection of
    msPhast:
      begin
        Cell := Model.PhastGrid.GetCell(Location,
          AScreenObject.ViewDirection, DataSet.EvaluatedAt);
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        // With MODFLOW, the only 2D data sets are in the top view.
        Assert(DataSet.Orientation = dsoTop);
        Assert(AScreenObject.ViewDirection = vdTop);
        if Model.DisvUsed then
        begin
          TopCell := Model.DisvGrid.TopContainingCellOrElement(Location,
            DataSet.EvaluatedAt);
        end
        else
        begin
          TopCell := Model.ModflowGrid.TopContainingCell(Location,
            DataSet.EvaluatedAt);
        end;
        Cell.Col := TopCell.Col;
        Cell.Row := TopCell.Row;
        Cell.Lay := 0;
      end;
    msSutra22, msSutra30:
      begin
        // With MODFLOW, the only 2D data sets are in the top view.
        Assert(DataSet.Orientation = dsoTop);
        Assert(AScreenObject.ViewDirection = vdTop);
        TopCell := Model.Mesh3D.TopContainingCellOrElement(Location,
          DataSet.EvaluatedAt);
        if TopCell.Col < 0 then
        begin
//          APoint := Location;
//          APoint.Y := APoint.Y*Anisotropy;
          APointer := NodeOrElementQuadTree.NearestPointsFirstData(
            Location.X, Location.y*Anisotropy);
          case DataSet.EvaluatedAt of
            eaBlocks:
              begin
                AnElement2D := IElement2D(APointer);
                Cell.Col := AnElement2D.ElementNumber;
                Cell.Row := 0;
              end;
            eaNodes:
              begin
                ANode2D := INode2D(APointer);
                Cell.Col := ANode2D.NodeNumber;
                Cell.Row := 0;
              end;
            else
              Assert(False);
          end;
        end
        else
        begin
          Cell.Col := TopCell.Col;
          Cell.Row := TopCell.Row;
        end;
        Cell.Lay := 0;
      end;
    else
      Assert(False);
  end;
  if Cell.Col < 0 then
  begin
    Cell.Col := 0;
//    Assert(False);
  end;
  if Cell.Row < 0 then
  begin
    Cell.Row := 0;
  end;
  case Model.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              if Model.DisvUsed then
              begin
                if Cell.Col >= Model.DisvGrid.ColumnCount then
                begin
                  Cell.Col := Model.DisvGrid.ColumnCount-1;
                end;
                if Cell.Row >= Model.DisvGrid.RowCount then
                begin
                  Cell.Row := Model.DisvGrid.RowCount-1;
                end;
                if Cell.Lay >= Model.DisvGrid.LayerCount then
                begin
                  Cell.Lay := Model.DisvGrid.LayerCount-1;
                end;
              end
              else
              begin
                if Cell.Col >= Model.Grid.ColumnCount then
                begin
                  Cell.Col := Model.Grid.ColumnCount-1;
                end;
                if Cell.Row >= Model.Grid.RowCount then
                begin
                  Cell.Row := Model.Grid.RowCount-1;
                end;
                if Cell.Lay >= Model.Grid.LayerCount then
                begin
                  Cell.Lay := Model.Grid.LayerCount-1;
                end;
              end;
            end;
          eaNodes:
            begin
              if Cell.Col >= Model.Grid.ColumnCount+1 then
              begin
                Cell.Col := Model.Grid.ColumnCount;
              end;
              if Cell.Row >= Model.Grid.RowCount+1 then
              begin
                Cell.Row := Model.Grid.RowCount;
              end;
              if Cell.Lay >= Model.Grid.LayerCount+1 then
              begin
                Cell.Lay := Model.Grid.LayerCount;
              end;
            end;
          else Assert(False);
        end;
      end;
    msSutra22, msSutra30:
      begin
//        Assert(False);
        { TODO -cSUTRA : Adjust cell if needed }
      end;
    else
      Assert(False);
  end;
  NearestSegment := AScreenObject.Segments[FModel].ClosestSegment(
    Location, Anisotropy);
  UpDateGlobalLocations(Cell.Col, Cell.Row, Cell.Lay, DataSet.EvaluatedAt,
    DataSet.Model);
  UpdateCurrentScreenObject(AScreenObject);
  UpdateCurrentSegment(NearestSegment);
//  if NearestSegment = nil then
//  begin
  UpdateCurrentSection(SectionIndex);
//  end;
  if VariablesUsed.Count > 0 then
  begin
    for Index := 0 to VariablesUsed.Count - 1 do
    begin
      ADataSet := Model.DataArrayManager.GetDataSetByName(
        VariablesUsed[Index]);
      if ADataSet <> nil then
      begin
        ADataSet.Initialize;
      end;
    end;
    for Index := 0 to VariablesUsed.Count - 1 do
    begin
      ADataSet := Model.DataArrayManager.GetDataSetByName(
        VariablesUsed[Index]);
      if ADataSet <> nil then
      begin
        VariableIndex := Compiler.IndexOfVariable(VariablesUsed[Index]);
        Variable := Compiler.Variables[VariableIndex];
        Assert(Variable.ResultType = ADataSet.DataType);
        case Variable.ResultType of
          rdtDouble:
            begin
              TRealVariable(Variable).Value :=
                ADataSet.RealData[Cell.Lay, Cell.Row, Cell.Col]
            end;
          rdtInteger:
            begin
              TIntegerVariable(Variable).Value :=
                ADataSet.IntegerData[Cell.Lay, Cell.Row, Cell.Col]
            end;
          rdtBoolean:
            begin
              TBooleanVariable(Variable).Value :=
                ADataSet.BooleanData[Cell.Lay, Cell.Row, Cell.Col]
            end;
          rdtString:
            begin
              TStringVariable(Variable).Value :=
                ADataSet.StringData[Cell.Lay, Cell.Row, Cell.Col]
            end;
        else
          Assert(False);
        end;
      end;
    end;
  end;
  UpDateGlobalLocations(Cell.Col, Cell.Row, Cell.Lay, DataSet.EvaluatedAt,
    DataSet.Model);
  UpdateCurrentScreenObject(AScreenObject);
  UpdateCurrentSegment(NearestSegment);
//  if NearestSegment = nil then
//  begin
  UpdateCurrentSection(SectionIndex);
//  end;
  AScreenObject.UpdateImportedValues(DataSet);
  EvaluateExpression(Compiler, Expression, AScreenObject);
end;

function TNearest2DInterpolator.GetNearestScreenObject(
  const Location: TPoint2D; out ClosestLocation: TPoint2D;
  out SectionIndex: integer): TScreenObject;
var
  Index: integer;
  AScreenObject: TScreenObject;
  ScreenObjectDistance: real;
  X, Y: double;
  Data: TPointerArray;
  LocalAnisotropy: Real;
  LocalLocation: TPoint2D;
  Storage: TScreenObjectSectionStorage;
begin
  result := nil;

  if (FListOfTScreenObjects.Count = 0) or (FQuadTree.Count = 0) then
  begin
    Exit;
  end;

  X := Location.X;
  Y := Location.Y * Anisotropy;
  FQuadTree.FindClosestPointsData(X, Y, Data);
  ClosestLocation.X := X;
  ClosestLocation.Y := Y / Anisotropy;
  if Length(Data) > 0 then
  begin
    Storage := Data[0];
    result := Storage.ScreenObject;
    SectionIndex := Storage.SectionIndex;
  end
  else
  begin
    result := nil;
  end;
  Assert(result <> nil);

  ScreenObjectDistance :=
    Sqrt(Sqr(Location.X - ClosestLocation.X) +
      Sqr((Location.Y - ClosestLocation.Y)*Anisotropy));

  if ScreenObjectDistance = 0 then
  begin
    Exit;
  end;

  LocalAnisotropy := Anisotropy;
  LocalLocation := Location;
  if result.ViewDirection = vdSide then
  begin
    LocalAnisotropy := 1/Anisotropy;
    LocalLocation := InvertPoint(Location);
    ClosestLocation := InvertPoint(ClosestLocation);
  end;

  // Check objects in reverse order because later objects should
  // override earlier ones, so if there is a tie, the later one
  // should win.
  for Index := FListOfTScreenObjects.Count - 1 downto 0 do
  begin
    AScreenObject := FListOfTScreenObjects[Index];
    // ScreenObjectDistance and ClosestLocation are changed if and only
    // if IsAnyPointCloser returns True.
    if AScreenObject.IsAnyPointCloser(LocalLocation, ScreenObjectDistance,
      ClosestLocation, SectionIndex, LocalAnisotropy) then
    begin
      result := AScreenObject;
      if ScreenObjectDistance = 0 then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCustomPoint2DInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  PointCount: integer;
  PointIndex: integer;
  APoint: TPoint2D;
  ListOfScreenObjects: TList;
  SectionIndex: integer;
  Count: integer;
begin
  ListOfScreenObjects := TList.Create;
  try
    FillScreenObjectList(ListOfScreenObjects);

    // Check objects in reverse order because later objects should
    // override earlier ones, so if there is a tie, the later one
    // should win.

    Count := 0;
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];

      PointCount := AScreenObject.Count;
      SectionIndex := 0;
      for PointIndex := 0 to PointCount - 1 do
      begin
        if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
        begin
          Inc(SectionIndex);
          if AScreenObject.SectionClosed[SectionIndex-1] then
          begin
            Continue;
          end;
        end;
        Inc(Count);
      end;
    end;
    SetArraySize(DataSet, Count);
    Count := 0;
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];

      PointCount := AScreenObject.Count;
      SectionIndex := 0;
      for PointIndex := 0 to PointCount - 1 do
      begin
        if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
        begin
          Inc(SectionIndex);
          if AScreenObject.SectionClosed[SectionIndex-1] then
          begin
            Continue;
          end;
        end;
        APoint := AScreenObject.Points[PointIndex];
        if AScreenObject.ViewDirection = vdSide then
        begin
          APoint := InvertPoint(APoint);
        end;

        StoreDataValue(Count, DataSet, APoint, AScreenObject, SectionIndex-1);
        Inc(Count);

      end;
      AScreenObject.CacheSegments;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

{ TNearestPoint2DInterpolator }

function TNearestPoint2DInterpolator.BooleanResult(
  const Location: TPoint2D): boolean;
var
  APointer: Pointer;
begin
  APointer := GetPointerToResult(Location);

  if APointer = nil then
  begin
    result := False;
  end
  else
  begin
    result := PBoolean(APointer)^;
  end;
end;

constructor TNearestPoint2DInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  FQuadTree := TRbwQuadTree.Create(self);
  FScreenObjectQuadTree := TRbwQuadTree.Create(self);
end;

procedure TNearestPoint2DInterpolator.Finalize(const DataSet: TDataArray);
begin
  FQuadTree.Clear;
  FScreenObjectQuadTree.Clear;
  inherited;
end;

function TNearestPoint2DInterpolator.GetQuadTreePointer(QuadTree: TRbwQuadTree;
  const Location: TPoint2D): Pointer;
var
  X: Double;
  Y: Double;
  Data: TPointerArray;
begin
  if QuadTree.Count = 0 then
  begin
    result := nil;
    Exit;
  end;
  X := Location.X;
  Y := Location.Y * Anisotropy;

  if QuadTree.Count > 0 then
  begin
    QuadTree.FindClosestPointsData(X, Y, Data);
    if Length(Data) > 0 then
    begin
      result := Data[0];
    end
    else
    begin
      result := nil;
    end;
  end
  else
  begin
    result := nil;
  end;
end;

function TNearestPoint2DInterpolator.GetScreenObjectAtLocation(
  const Location: TPoint2D): TScreenObject;
begin
  result := GetQuadTreePointer(FScreenObjectQuadTree, Location);
end;

function TNearestPoint2DInterpolator.GetPointerToResult(
  const Location: TPoint2D): Pointer;
//var
//  X: Double;
//  Y: Double;
//  Data: TPointerArray;
begin
  result := GetQuadTreePointer(FQuadTree, Location);
  FLastScreenObject := GetScreenObjectAtLocation(Location);
//  if FQuadTree.Count = 0 then
//  begin
//    result := nil;
//    Exit;
//  end;
//  X := Location.X;
//  Y := Location.Y * Anisotropy;
//
//  if FQuadTree.Count > 0 then
//  begin
//    FQuadTree.FindClosestPointsData(X, Y, Data);
//    if Length(Data) > 0 then
//    begin
//      result := Data[0];
//    end
//    else
//    begin
//      result := nil;
//    end;
//  end
//  else
//  begin
//    result := nil;
//  end;
end;

function TNearestPoint2DInterpolator.IntegerResult(
  const Location: TPoint2D): integer;
var
  APointer: Pointer;
begin
  APointer := GetPointerToResult(Location);

  if APointer = nil then
  begin
    result := 0;
  end
  else
  begin
    result := PInteger(APointer)^;
  end;
end;

class function TNearestPoint2DInterpolator.InterpolatorName: string;
begin
  result := 'Nearest Point';
end;

function TNearestPoint2DInterpolator.LastAnnotation: string;
begin
  result := inherited;
  if FLastScreenObject <> nil then
  begin
    result := Format(Str0sUsing1s, [result, FLastScreenObject.Name]);
  end;
end;

function TNearestPoint2DInterpolator.RealResult(
  const Location: TPoint2D): real;
var
  APointer: Pointer;
begin
  APointer := GetPointerToResult(Location);

  if APointer = nil then
  begin
    result := 0;
  end
  else
  begin
    result := PReal(APointer)^;
  end;
end;

procedure TNearestPoint2DInterpolator.SetArraySize(const DataSet: TDataArray;
  Count: Integer);
begin
  inherited;
  FQuadTree.MaxPoints := Max(100, Count div 1000);
  FScreenObjectQuadTree.MaxPoints := FQuadTree.MaxPoints;
end;

procedure TNearestPoint2DInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
begin
  InitializeQuadTreeLimits(FQuadTree, DataSet);
  InitializeQuadTreeLimits(FScreenObjectQuadTree, DataSet);
  inherited StoreData(Sender, DataSet);
end;

procedure TNearestPoint2DInterpolator.StoreDataValue(Count: Integer;
  const DataSet: TDataArray; APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer);
var
  Expression: TExpression;
begin
  InitializeVariablesAndExpression(APoint, AScreenObject, SectionIndex, Expression);
  try
    case DataSet.DataType of
      rdtDouble:
        begin
          FRealData[Count] := Expression.DoubleResult;
          FQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy,
            Addr(FRealData[Count]));
        end;
      rdtInteger:
        begin
          FIntegerData[Count] := Expression.IntegerResult;
          FQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy,
            Addr(FIntegerData[Count]));
        end;
      rdtBoolean:
        begin
          FBooleanData[Count] := Expression.BooleanResult;
          FQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy,
            Addr(FBooleanData[Count]));
        end;
      rdtString:
        begin
          FStringData[Count] := Expression.StringResult;
          FQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy,
            Addr(FStringData[Count]));
        end;
    else
      Assert(False);
    end;
  except on E: ERbwParserError do
    begin
      frmErrorsAndWarnings.AddError(DataSet.Model, StrErrorAssigningValu,
        Format(StrErrorMessage0, [E.Message, AScreenObject.Name]), AScreenObject);
      Exit;
    end;
  end;
  FScreenObjectQuadTree.AddPoint(APoint.X, APoint.Y * Anisotropy, AScreenObject);
end;

function TNearestPoint2DInterpolator.StringResult(
  const Location: TPoint2D): string;
var
  APointer: Pointer;
begin
  APointer := GetPointerToResult(Location);

  if APointer = nil then
  begin
    result := '';
  end
  else
  begin
    result := PString(APointer)^;
  end;
end;

class function TNearestPoint2DInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble, rdtInteger, rdtBoolean, rdtString];
end;

{ TCustomSfrpackInterpolator }

constructor TCustomSfrpackInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  FSfrInterpolator := TSfrInterpolator.Create;
  FSinSfrInterpolator := TSfrInterpolator.Create;
  FCosSfrInterpolator := TSfrInterpolator.Create;
  OnInitialize := StoreData;
end;

destructor TCustomSfrpackInterpolator.Destroy;
begin
  FSfrInterpolator.Free;
  FSinSfrInterpolator.Free;
  FCosSfrInterpolator.Free;
  inherited;
end;

procedure TCustomSfrpackInterpolator.Finalize(const DataSet: TDataArray);
begin
  inherited;
  FSfrInterpolator.Finalize;
  FSinSfrInterpolator.Finalize;
  FCosSfrInterpolator.Finalize;
end;

procedure TCustomSfrpackInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
var
  ListOfScreenObjects: TList;
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Count: integer;
  PointCount: Integer;
  APoint, Point1, Point2: TPoint2D;
  PointIndex: Integer;
  Expression: TExpression;
  XArray: TFloatArray;
  YArray: TFloatArray;
  ZArray: TFloatArray;
  ZSinArray: TFloatArray;
  ZCosArray: TFloatArray;
  Index: Integer;
  SectionIndex: integer;
  SortArray: array of TSortRecord;
  SortIndex: integer;
  SortList: TList;
  PSort1, PSort2: PSortRecord;
  X_Single1: single;
  X_Single2: single;
  Y_Single1: single;
  Y_Single2: single;
  ErrorMessage: string;
begin
  FValidData := False;
  ListOfScreenObjects := TList.Create;
  try
    // Get a list of the TScreenObjects that set the value of DataSet
    // by interpolation.
    FillScreenObjectList(ListOfScreenObjects);

    // Set the capacity of X, Y, and Z.
    Count := 0;
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];
      Count := Count + AScreenObject.Count;
    end;
    SetLength(SortArray, Count);

    SortIndex := 0;
    // Add values to X, Y, and Z at each unique location.
    // Iterate from the end to the beginning to make sure that
    // if any points are non-unique, the points of the later objects
    // win out.
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];

      SectionIndex := 0;
      PointCount := AScreenObject.Count;
      for PointIndex := 0 to PointCount - 1 do
      begin
        if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
        begin
          if AScreenObject.SectionClosed[SectionIndex] then
          begin
            Inc(SectionIndex);
            Continue;
          end;
        end;
        APoint := AScreenObject.Points[PointIndex];
        if AScreenObject.ViewDirection = vdSide then
        begin
          APoint := InvertPoint(APoint);
        end;
        Point1.X := APoint.x;
        Point1.y := APoint.y * Anisotropy;
        Point2 := Point1;

        SortArray[SortIndex].Location := Point2;
        InitializeVariablesAndExpression(Point2, AScreenObject, SectionIndex, Expression);
        SortArray[SortIndex].Value := Expression.DoubleResult;
        SortArray[SortIndex].Duplicate := False;
        SortArray[SortIndex].OriginalIndex := SortIndex;
        Inc(SortIndex);
        if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
        begin
          Inc(SectionIndex);
        end;
      end;
    end;

    SortList := TList.Create;
    try
      SortList.Capacity := SortIndex;
      for SortIndex := 0 to SortList.Capacity - 1 do
      begin
        SortList.Add(Addr(SortArray[SortIndex]));
      end;

      SortList.Sort(SortRecordCompare);

      Count := 1;
      for SortIndex := 0 to SortList.Count - 2 do
      begin
        PSort1 := SortList[SortIndex];
        PSort2 := SortList[SortIndex+1];
        // reduce precision to single precision.
        X_Single1 := PSort1.Location.x;
        X_Single2 := PSort2.Location.x;
        Y_Single1 := PSort1.Location.y;
        Y_Single2 := PSort2.Location.y;
        PSort2.Duplicate := (X_Single1 = X_Single2)
          and (Y_Single1 = Y_Single2);
        if not PSort2.Duplicate then
        begin
          Inc(Count);
        end;
      end;

      // transfer X, Y, and Z to XArray, YArray, and ZArray.
      SetLength(XArray, Count);
      SetLength(YArray, Count);
      case DataSet.AngleType of
        atNone:
          begin
            SetLength(ZArray, Count);
          end;
        atDegrees, atRadians:
          begin
            SetLength(ZSinArray, Count);
            SetLength(ZCosArray, Count);
          end;
        else Assert(False);
      end;


      SortIndex := 0;
      for Index := 0 to SortList.Count - 1 do
      begin
        if not SortArray[Index].Duplicate then
        begin
          XArray[SortIndex] := SortArray[Index].Location.X;
          YArray[SortIndex] := SortArray[Index].Location.Y;
          case DataSet.AngleType of
            atNone:
              begin
                ZArray[SortIndex] := SortArray[Index].Value;
              end;
            atDegrees:
              begin
                ZSinArray[SortIndex] := Sin(SortArray[Index].Value/180*Pi);
                ZCosArray[SortIndex] := Cos(SortArray[Index].Value/180*Pi);
              end;
            atRadians:
              begin
                ZSinArray[SortIndex] := Sin(SortArray[Index].Value);
                ZCosArray[SortIndex] := Cos(SortArray[Index].Value);
              end;
            else Assert(False);
          end;
          Inc(SortIndex);
        end;
      end;
    finally
      SortList.Free;
    end;

    // Initialize FSfrInterpolator.  If all goes well,
    // indicate that the data is valid.  Otherwise, inform the user
    // of the problem.
    try
      case DataSet.AngleType of
        atNone:
          begin
            FSfrInterpolator.Initialize(XArray, YArray, ZArray);
          end;
        atDegrees, atRadians:
          begin
            FSinSfrInterpolator.Initialize(XArray, YArray, ZSinArray);
            FCosSfrInterpolator.Initialize(XArray, YArray, ZCosArray);
          end;
        else Assert(False);
      end;

      FValidData := True;
    except on E: ESfrError do
      begin
        Beep;
        ErrorMessage := Format(StrErrorEncoutereredI,
          [InterpolatorName, DataSet.Name, E.Message]);
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

class function TCustomSfrpackInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble];
end;

{ TLinearSfrpackInterpolator }

class function TLinearSfrpackInterpolator.InterpolatorName: string;
begin
  result := 'Triangle Interp.';
end;

function TLinearSfrpackInterpolator.RealResult(const Location: TPoint2D): real;
var
  ErrorMessage: string;
  InterpSin: TFloat;
  InterpCos: TFloat;
begin
  if FValidData then
  begin
    try
      result := 0;
      case DataSet.AngleType of
        atNone:
          begin
            result := FSfrInterpolator.Interpolate1(
              Location.x, Location.y * Anisotropy);
          end;
        atDegrees:
          begin
            InterpSin := FSinSfrInterpolator.Interpolate1(
              Location.x, Location.y * Anisotropy);
            InterpCos := FCosSfrInterpolator.Interpolate1(
              Location.x, Location.y * Anisotropy);
            if (InterpSin = 0) and (InterpCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(InterpSin, InterpCos);
              result := result/Pi*180;
            end;
          end;
        atRadians:
          begin
            InterpSin := FSinSfrInterpolator.Interpolate1(
              Location.x, Location.y * Anisotropy);
            InterpCos := FCosSfrInterpolator.Interpolate1(
              Location.x, Location.y * Anisotropy);
            if (InterpSin = 0) and (InterpCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(InterpSin, InterpCos);
            end;
          end;
        else Assert(False);
      end;
    except on E: ESfrError do
      begin
        FValidData := False;
        Beep;
        ErrorMessage := Format(StrErrorEncoutereredI,
          [InterpolatorName, DataSet.Name, E.Message]);
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
        result := 0;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

{ TCurvilinearSfrpackInterpolator }

class function TFittedSurfaceIntepolator.InterpolatorName: string;
begin
  result := 'Fitted Surface';
end;

function TFittedSurfaceIntepolator.RealResult(
  const Location: TPoint2D): real;
var
  ErrorMessage: string;
  InterpSin: TFloat;
  InterpCos: TFloat;
begin
  if FValidData then
  begin
    try
      result := 0;
      case DataSet.AngleType of
        atNone:
          begin
            result := FSfrInterpolator.Interpolate2(
              Location.x, Location.y * Anisotropy);
          end;
        atDegrees:
          begin
            InterpSin := FSinSfrInterpolator.Interpolate2(
              Location.x, Location.y * Anisotropy);
            InterpCos := FCosSfrInterpolator.Interpolate2(
              Location.x, Location.y * Anisotropy);
            if (InterpSin = 0) and (InterpCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(InterpSin, InterpCos);
              result := result/Pi*180;
            end;
          end;
        atRadians:
          begin
            InterpSin := FSinSfrInterpolator.Interpolate2(
              Location.x, Location.y * Anisotropy);
            InterpCos := FCosSfrInterpolator.Interpolate2(
              Location.x, Location.y * Anisotropy);
            if (InterpSin = 0) and (InterpCos = 0) then
            begin
              result := 0;
            end
            else
            begin
              result := ArcTan2(InterpSin, InterpCos);
            end;
          end;
        else Assert(False);
      end;
    except on E: ESfrError do
      begin
        FValidData := False;
        Beep;
        ErrorMessage := Format(StrErrorEncoutereredI,
          [InterpolatorName, DataSet.Name, E.Message]);
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
        result := 0;
      end;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

{ TInvDistSqPoint2DInterpolator }

procedure TInvDistSqPoint2DInterpolator.Finalize(const DataSet: TDataArray);
begin
  inherited;
  SetLength(FLocations, 0);
end;

class function TInvDistSqPoint2DInterpolator.InterpolatorName: string;
begin
  result := 'Point Inv. Dist. Sq.';
end;

function TInvDistSqPoint2DInterpolator.RealResult(
  const Location: TPoint2D): real;
var
  Index: integer;
  PointDistance: real;
  PointLocation: TPoint2D;
  Weight: real;
  SumWeights: real;
  Sum: real;
  Value: real;
  AdjustedLocaton: TPoint2D;
  SumSin: Real;
  SumCos: Real;
begin
  SumWeights := 0;
  Sum := 0;
  SumSin := 0;
  SumCos := 0;

  AdjustedLocaton.x := Location.x;
  AdjustedLocaton.y := Location.y * Anisotropy;
  for Index := 0 to Length(FLocations) -1 do
  begin
    Value := FRealData[Index];
    PointLocation := FLocations[Index];
    PointDistance := Distance(PointLocation, AdjustedLocaton);
    if PointDistance = 0 then
    begin
      result := Value;
      Exit;
    end
    else
    begin
      Weight := 1 / Sqr(PointDistance);
      SumWeights := SumWeights + Weight;
      case DataSet.AngleType of
        atNone:
          begin
            Sum := Sum + Value * Weight;
          end;
        atDegrees:
          begin
            Value := Value/180*Pi;
            SumSin := SumSin + Sin(Value) * Weight;
            SumCos := SumCos + Cos(Value) * Weight;
          end;
        atRadians:
          begin
            SumSin := SumSin + Sin(Value) * Weight;
            SumCos := SumCos + Cos(Value) * Weight;
          end;
        else Assert(False);
      end;

    end;
  end;
  if SumWeights = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := 0;
    case DataSet.AngleType of
      atNone:
        begin
          result := Sum / SumWeights;
        end;
      atDegrees:
        begin
          if (SumSin = 0) and (SumCos = 0) then
          begin
            result := 0;
          end
          else
          begin
            result := ArcTan2(SumSin, SumCos);
            result := result*180/Pi;
          end;
        end;
      atRadians:
        begin
          if (SumSin = 0) and (SumCos = 0) then
          begin
            result := 0;
          end
          else
          begin
            result := ArcTan2(SumSin, SumCos);
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TInvDistSqPoint2DInterpolator.SetArraySize(const DataSet: TDataArray;
  Count: Integer);
begin
  inherited;
  SetLength(FLocations, Count);
end;

procedure TInvDistSqPoint2DInterpolator.StoreDataValue(Count: Integer;
  const DataSet: TDataArray; APoint: TPoint2D; AScreenObject: TScreenObject; SectionIndex: integer);
var
  Expression: TExpression;
begin
  inherited;
  InitializeVariablesAndExpression(APoint, AScreenObject, SectionIndex, Expression);
  Assert(DataSet.DataType = rdtDouble);
  try
    FRealData[Count] := Expression.DoubleResult;
  except on E: ERbwParserError do
    begin
      frmErrorsAndWarnings.AddError(DataSet.Model, StrErrorAssigningValu,
        Format(StrErrorMessage0, [E.Message, AScreenObject.Name]), AScreenObject);
      FRealData[Count] := 0;
      Exit;
    end;
  end;
  FLocations[Count].X := APoint.X;
  FLocations[Count].Y := APoint.Y * Anisotropy;
end;

class function TInvDistSqPoint2DInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble];
end;

function TCustomTriangleInterpolator.ShouldInterpolate: boolean;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  DataSetIndex: Integer;
//  PointCount: integer;
//  PointList: TList<TPoint2D>;
  PointIndex: Integer;
  APoint: TPoint2D;
  QuadTree: TRbwQuadTree;
  X: TFloat;
  Y: TFloat;
  Data: TPointerArray;
begin
  result := False;
//  PointCount := 0;
  QuadTree := TRbwQuadTree.Create(nil);
  try
    for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if AScreenObject.Deleted or not AScreenObject.SetValuesByInterpolation
        then
      begin
        continue;
      end;
      DataSetIndex := AScreenObject.IndexOfDataSet(DataSet);
      if DataSetIndex < 0 then
      begin
        continue;
      end;
      if not AScreenObject.UsedModels.UsesModel(DataSet.Model) then
      begin
        Continue;
      end;
      for PointIndex := 0 to AScreenObject.Count - 1 do
      begin
        APoint := AScreenObject.Points[PointIndex];
        if QuadTree.Count = 0 then
        begin
          QuadTree.AddPoint(APoint.x, APoint.y, nil);
        end
        else
        begin
          X := APoint.x;
          Y := APoint.y;
          QuadTree.FindClosestPointsData(X, Y, Data);
          if (X <> APoint.x) or (Y <> APoint.y) then
          begin
            QuadTree.AddPoint(APoint.x, APoint.y, nil);
            if QuadTree.Count >= 3 then
            begin
              result := True;
              Exit;
            end;
          end;
        end;
//        if PointList.IndexOf(APoint) < 0 then
//        begin
//          PointList.Add(APoint);
//          if PointList.Count >= 3 then
//          begin
//            result := True;
//            Exit;
//          end;
//        end;
      end;
//      PointCount := PointCount + AScreenObject.Count;
//      if PointCount >= 3 then
//      begin
//        result := True;
//        break;
//      end;
    end;
  finally
    QuadTree.Free
  end;
end;

{ TNaturalNeighborInterp }

constructor TNaturalNeighborInterp.Create(AOwner: TComponent);
begin
  inherited;
  OnInitialize := StoreData;
end;

destructor TNaturalNeighborInterp.Destroy;
begin
  FNatNeigh.Free;
  FSinNatNeigh.Free;
  FCosNatNeigh.Free;
  inherited;
end;

procedure TNaturalNeighborInterp.Finalize(const DataSet: TDataArray);
begin
  inherited;
  FreeAndNil(FNatNeigh);
  FreeAndNil(FSinNatNeigh);
  FreeAndNil(FCosNatNeigh);
end;

class function TNaturalNeighborInterp.InterpolatorName: string;
begin
  result := 'Natural Neighbor';
end;

function TNaturalNeighborInterp.RealResult(const Location: TPoint2D): real;
var
  InterpSin: Double;
  InterpCos: Double;
begin
  result := 0;
  case DataSet.AngleType of
    atNone:
      begin
        result := FNatNeigh.Interpolate(Location.x, Location.y);
      end;
    atDegrees:
      begin
        InterpSin := FSinNatNeigh.Interpolate(Location.x, Location.y);
        InterpCos := FCosNatNeigh.Interpolate(Location.x, Location.y);
        if (InterpSin = 0) and (InterpCos = 0) then
        begin
          result := 0;
        end
        else
        begin
          result := ArcTan2(InterpSin, InterpCos);
          result := result/Pi*180;
        end;
      end;
    atRadians:
      begin
        InterpSin := FSinNatNeigh.Interpolate(Location.x, Location.y);
        InterpCos := FCosNatNeigh.Interpolate(Location.x, Location.y);
        if (InterpSin = 0) and (InterpCos = 0) then
        begin
          result := 0;
        end
        else
        begin
          result := ArcTan2(InterpSin, InterpCos)
        end;
      end;
    else Assert(False);
  end;

end;

procedure TNaturalNeighborInterp.SetEpsilonX(const Value: Double);
begin
  inherited;
  if FNatNeigh <> nil then
  begin
    FNatNeigh.EpsilonX := EpsilonX;
  end;
  if FSinNatNeigh <> nil then
  begin
    FSinNatNeigh.EpsilonX := EpsilonX;
  end;
  if FCosNatNeigh <> nil then
  begin
    FCosNatNeigh.EpsilonX := EpsilonX;
  end;
end;

procedure TNaturalNeighborInterp.SetEpsilonY(const Value: Double);
begin
  inherited;
  if FNatNeigh <> nil then
  begin
    FNatNeigh.EpsilonY := EpsilonY;
  end;
  if FSinNatNeigh <> nil then
  begin
    FSinNatNeigh.EpsilonY := EpsilonY;
  end;
  if FCosNatNeigh <> nil then
  begin
    FCosNatNeigh.EpsilonY := EpsilonY;
  end;
end;

procedure TNaturalNeighborInterp.StoreData(Sender: TObject;
  const DataSet: TDataArray);
var  
  ListOfScreenObjects: TList;
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  Count: integer;
  PointCount: Integer;
  APoint, Point1, Point2: TPoint2D;
  PointIndex: Integer;
  Expression: TExpression;
  XArray: TFloatArray;
  YArray: TFloatArray;
  ZArray: TFloatArray;
  ZSinArray: TFloatArray;
  ZCosArray: TFloatArray;
  Index: Integer;
  SectionIndex: integer;
  SortArray: array of TSortRecord;
  SortIndex: integer;
  SortList: TList;
  PSort1, PSort2: PSortRecord;
  X_Single1: single;
  X_Single2: single;
  Y_Single1: single;
  Y_Single2: single;
  MinY: Real;
  MaxY: Real;
  MinX: Real;
  MaxX: Real;
begin
  ListOfScreenObjects := TList.Create;
  try
    // Get a list of the TScreenObjects that set the value of DataSet
    // by interpolation.
    FillScreenObjectList(ListOfScreenObjects);

    // Set the capacity of X, Y, and Z.
    Count := 0;
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];
      Count := Count + AScreenObject.Count;
    end;
    SetLength(SortArray, Count);

    SortIndex := 0;
    // Add values to X, Y, and Z at each unique location.
    // Iterate from the end to the beginning to make sure that
    // if any points are non-unique, the points of the later objects
    // win out.
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];

      SectionIndex := 0;
      PointCount := AScreenObject.Count;
      for PointIndex := 0 to PointCount - 1 do
      begin
        if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
        begin
          Inc(SectionIndex);
          if AScreenObject.SectionClosed[SectionIndex-1] then
          begin
            Continue;
          end;
        end;
        APoint := AScreenObject.Points[PointIndex];
        if AScreenObject.ViewDirection = vdSide then
        begin
          APoint := InvertPoint(APoint);
        end;
        Point1.X := APoint.x;
        Point1.y := APoint.y * Anisotropy;
        Point2 := Point1;

        SortArray[SortIndex].Location := Point2;
        InitializeVariablesAndExpression(Point2, AScreenObject, SectionIndex-1, Expression);
        SortArray[SortIndex].Value := Expression.DoubleResult;
        SortArray[SortIndex].Duplicate := False;
        SortArray[SortIndex].OriginalIndex := SortIndex;
        Inc(SortIndex);
      end;
    end;

    SortList := TList.Create;
    try
      SortList.Capacity := SortIndex;
      for SortIndex := 0 to SortList.Capacity - 1 do
      begin
        SortList.Add(Addr(SortArray[SortIndex]));
      end;

      SortList.Sort(SortRecordCompare);

      Count := 1;
      for SortIndex := 0 to SortList.Count - 2 do
      begin
        PSort1 := SortList[SortIndex];
        PSort2 := SortList[SortIndex+1];
        // reduce precision to single precision.
        X_Single1 := PSort1.Location.x;
        X_Single2 := PSort2.Location.x;
        Y_Single1 := PSort1.Location.y;
        Y_Single2 := PSort2.Location.y;
        PSort2.Duplicate := (Abs(X_Single1 - X_Single2) < 1e-5)
          and (Abs(Y_Single1 - Y_Single2) < 1e-5);
        if not PSort2.Duplicate then
        begin
          Inc(Count);
        end;
      end;

      // transfer X, Y, and Z to XArray, YArray, and ZArray.
      SetLength(XArray, Count);
      SetLength(YArray, Count);
      case DataSet.AngleType of
        atNone:
          begin
            SetLength(ZArray, Count);
          end;
        atDegrees, atRadians:
          begin
            SetLength(ZSinArray, Count);
            SetLength(ZCosArray, Count);
          end;
        else Assert(False);
      end;

      SortIndex := 0;
      for Index := 0 to SortList.Count - 1 do
      begin
        if not SortArray[Index].Duplicate then
        begin
          XArray[SortIndex] := SortArray[Index].Location.X;
          YArray[SortIndex] := SortArray[Index].Location.Y;
          case DataSet.AngleType of
            atNone:
              begin
                ZArray[SortIndex] := SortArray[Index].Value;
              end;
            atDegrees:
              begin
                ZSinArray[SortIndex] := Sin(SortArray[Index].Value/180*Pi);
                ZCosArray[SortIndex] := Cos(SortArray[Index].Value/180*Pi);
              end;
            atRadians:
              begin
                ZSinArray[SortIndex] := Sin(SortArray[Index].Value);
                ZCosArray[SortIndex] := Cos(SortArray[Index].Value);
              end;
            else
              Assert(False);
          end;
          Inc(SortIndex);
        end;
      end;
    finally
      SortList.Free;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
  GetLimits(MinY, MaxY, MinX, MaxX, DataSet);
  case DataSet.AngleType of
    atNone:
      begin
        FNatNeigh := TNaturalNeighborInterpolatorTripack.Create(
          XArray, YArray, ZArray, MinX, MinY, MaxX, MaxY);
        FNatNeigh.EpsilonX := EpsilonX;
        FNatNeigh.EpsilonY := EpsilonY;
      end;
    atDegrees, atRadians:
      begin
        FSinNatNeigh := TNaturalNeighborInterpolatorTripack.Create(
          XArray, YArray, ZSinArray, MinX, MinY, MaxX, MaxY);
        FSinNatNeigh.EpsilonX := EpsilonX;
        FSinNatNeigh.EpsilonY := EpsilonY;
        FCosNatNeigh := TNaturalNeighborInterpolatorTripack.Create(
          XArray, YArray, ZCosArray, MinX, MinY, MaxX, MaxY);
        FCosNatNeigh.EpsilonX := EpsilonX;
        FCosNatNeigh.EpsilonY := EpsilonY;
      end;
    else Assert(False);
  end;

end;

class function TNaturalNeighborInterp.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble];
end;

{ TPointAverageInterpolator }

constructor TPointAverageInterpolator.Create(AOwner: TComponent);
var
  APoint: TPoint2D;
begin
  inherited;
  FInternalScreenObject := TInterpolateScreenObject.Create(nil);
  FInternalScreenObject.ElevationCount := ecZero;
  APoint.x := 0;
  APoint.y := 0;
  FInternalScreenObject.AddPoint(APoint, True);
  FInternalScreenObject.SetValuesOfIntersectedCells := true;
  FInternalScreenObject.Model := frmGoPhast.PhastModel;
  FCellList := TCellAssignmentList.Create
end;

destructor TPointAverageInterpolator.Destroy;
begin
  FCellList.Free;
  FInternalScreenObject.Free;
  inherited;
end;

class function TPointAverageInterpolator.InterpolatorName: string;
begin
  result := 'Point Average';
end;

function TPointAverageInterpolator.PointToArrayLocation(
  APoint: TPoint2D): TArrayLocation;
var
//  AGrid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  ACell: TCellAssignment;
//  Mesh: IMesh3D;
begin
  FCellList.Clear;
  FInternalScreenObject.Points[0] := APoint;
//  AGrid := (FModel as TCustomModel).Grid;
//  if AGrid = nil then
//  begin
//    Mesh := (FModel as TCustomModel).Mesh3D;
    FInternalScreenObject.GetCellsToAssign({Mesh,} '0', nil, DataSet,
      FCellList, alAll, FModel);
//  end
//  else
//  begin
//    FInternalScreenObject.GetCellsToAssign(AGrid, '0', nil, DataSet,
//      FCellList, alAll, FModel);
//  end;
  if FCellList.Count > 0 then
  begin
    Assert(FCellList.Count = 1);
    ACell := FCellList[0];
    case FInternalScreenObject.ViewDirection of
      vdTop:
        begin
          Result.Dimen1 := ACell.Row;
          Result.Dimen2 := ACell.Column;
        end;
      vdFront:
        begin
          Result.Dimen1 := ACell.Layer;
          Result.Dimen2 := ACell.Column;
        end;
      vdSide:
        begin
          Result.Dimen1 := ACell.Layer;
          Result.Dimen2 := ACell.Row;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    Result.Dimen1 := -1;
    Result.Dimen2 := -1;
  end;
end;

function TPointAverageInterpolator.RealResult(const Location: TPoint2D): real;
var
  ALocation: TArrayLocation;
  PointCount: Integer;
begin
  ALocation := PointToArrayLocation(Location);
  if ALocation.Dimen1 >= 0 then
  begin
    PointCount := FCount[ALocation.Dimen1, ALocation.Dimen2];
    if PointCount > 0 then
    begin
      result := FValues[ALocation.Dimen1, ALocation.Dimen2]/PointCount;
    end
    else
    begin
      result := 0;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TPointAverageInterpolator.SetArraySize(const DataSet: TDataArray;
  Count: Integer);
var
  OuterIndex: Integer;
  InnerIndex: Integer;
begin
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        case DataSet.Orientation of
          dsoTop:
            begin
              SetLength(FValues, DataSet.RowCount, DataSet.ColumnCount);
              SetLength(FCount, DataSet.RowCount, DataSet.ColumnCount);
            end;
          dsoFront:
            begin
              SetLength(FValues, DataSet.LayerCount, DataSet.ColumnCount);
              SetLength(FCount, DataSet.LayerCount, DataSet.ColumnCount);
            end;
          dsoSide:
            begin
              SetLength(FValues, DataSet.LayerCount, DataSet.RowCount);
              SetLength(FCount, DataSet.LayerCount, DataSet.RowCount);
            end;
          else Assert(False);
        end;
      end;
    eaNodes:
      begin
        case DataSet.Orientation of
          dsoTop:
            begin
              SetLength(FValues, DataSet.RowCount+1, DataSet.ColumnCount+1);
              SetLength(FCount, DataSet.RowCount+1, DataSet.ColumnCount+1);
            end;
          dsoFront:
            begin
              SetLength(FValues, DataSet.LayerCount+1, DataSet.ColumnCount+1);
              SetLength(FCount, DataSet.LayerCount+1, DataSet.ColumnCount+1);
            end;
          dsoSide:
            begin
              SetLength(FValues, DataSet.LayerCount+1, DataSet.RowCount+1);
              SetLength(FCount, DataSet.LayerCount+1, DataSet.RowCount+1);
            end;
          else Assert(False);
        end;
      end;
  end;
  for OuterIndex := 0 to Length(FValues) - 1 do
  begin
    for InnerIndex := 0 to Length(FValues[0])  - 1 do
    begin
      FValues[OuterIndex, InnerIndex] := 0;
      FCount[OuterIndex, InnerIndex] := 0;
    end;
  end;
end;

procedure TPointAverageInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
begin
  case DataSet.Orientation of
    dsoTop: FInternalScreenObject.ViewDirection := vdTop;
    dsoFront: FInternalScreenObject.ViewDirection := vdFront;
    dsoSide: FInternalScreenObject.ViewDirection := vdSide;
    else Assert(False);
  end;
  FInternalScreenObject.EvaluatedAt := DataSet.EvaluatedAt;
//  FInternalScreenObject.Model := DataSet.Model;
  inherited;
end;

procedure TPointAverageInterpolator.StoreDataValue(Count: Integer;
  const DataSet: TDataArray; APoint: TPoint2D; AScreenObject: TScreenObject;
  SectionIndex: integer);
var
  ALocation: TArrayLocation;
  Expression: TExpression;
begin
  ALocation := PointToArrayLocation(APoint);
  if ALocation.Dimen1 >= 0 then
  begin
    InitializeVariablesAndExpression(APoint, AScreenObject, SectionIndex, Expression);
    Assert(DataSet.DataType = rdtDouble);
    try
      FValues[ALocation.Dimen1, ALocation.Dimen2] :=
        FValues[ALocation.Dimen1, ALocation.Dimen2] + Expression.DoubleResult;
      FCount[ALocation.Dimen1, ALocation.Dimen2] :=
        FCount[ALocation.Dimen1, ALocation.Dimen2] + 1;
    except on E: ERbwParserError do
      begin
        frmErrorsAndWarnings.AddError(DataSet.Model, StrErrorAssigningValu,
          Format(StrErrorMessage0, [E.Message, AScreenObject.Name]), AScreenObject);
        Exit;
      end;
    end;
  end;
end;

class function TPointAverageInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  result := [rdtDouble];
end;

{ TCustomPlProcInterpolator }

constructor TCustomPlProcInterpolator.Create(AOwner: TComponent);
begin
  inherited;
  FFilesToDelete := TStringList.Create;
  FKrigedResults := TRbwQuadTree.Create(nil);
  OnInitialize := StoreData;
end;

destructor TCustomPlProcInterpolator.Destroy;
begin
  FKrigedResults.Free;
  FFilesToDelete.Free;
  inherited;
end;

procedure TCustomPlProcInterpolator.Finalize(const DataSet: TDataArray);
begin
  FKrigedResults.Clear;
  SetLength(FValues, 0);
  inherited;
end;

procedure TCustomPlProcInterpolator.ReadResults;
var
  Locations: TStringList;
  Values: TStringList;
  LineIndex: Integer;
  SplitLocations: TArray;
  X: Double;
  Y: Double;
  Value: Double;
begin
  FKrigedResults.Clear;
  Locations := TStringList.Create;
  Values := TStringList.Create;
  try
    Locations.LoadFromFile(FResultLocationFileName);
    Values.LoadFromFile(FKrigingOutputFileName);
    Locations.Delete(0);
    Assert(Locations.Count = Values.Count);
    SetLength(ResultValues, Values.Count);
    for LineIndex := 0 to Locations.Count - 1 do
    begin
      SplitLocations := Locations[LineIndex].Split([' ']);
      Assert(Length(SplitLocations) = 2);
      X := FortranStrToFloat(SplitLocations[0]);
      Y := FortranStrToFloat(SplitLocations[1]);
      Value := FortranStrToFloat(Values[LineIndex]);
      ResultValues[LineIndex] := Value;
      FKrigedResults.AddPoint(X, Y, Addr(ResultValues[LineIndex]));
    end;
  finally
    Values.Free;
    Locations.Free;
  end;
end;

procedure TCustomPlProcInterpolator.WriteScript(const DataSet: TDataArray);
var
  KrigingFactorsFileName: string;
  Script: TStringList;
  LocalModel: TCustomModel;
begin
  LocalModel := DataSet.Model as TCustomModel;
  KrigingFactorsFileName := ChangeFileExt(FInputFileName, '.KrigFactors');
  FFilesToDelete.Add(KrigingFactorsFileName);

  FKrigingOutputFileName := ChangeFileExt(FInputFileName, '.KrigOutput');
  FFilesToDelete.Add(FKrigingOutputFileName);
  Script := TStringList.Create;
  try
    Script.Add(Format('# read input data for data set %0:s in %1:s', [DataSet.Name, LocalModel.DisplayName]));
    Script.Add('cl_input = read_list_file(skiplines=1,dimensions=2, &');
    Script.Add('  plist=''p_1'';column=4, &');
    Script.Add(Format('  id_type=''indexed'',file=''%s'')', [ExtractFileName(FInputFileName)]));
    Script.Add('');
    Script.Add('# read output data locations');
    Script.Add('cl_loc = read_list_file(skiplines=1,dimensions=2, &');
    Script.Add(Format('  id_type=''indexed'',file=''%s'')', [ExtractFileName(FResultLocationFileName)]));
    Script.Add('');
    Script.Add('# calculate kriging factors');
    Script.Add(Format('calc_kriging_factors_auto_2d(file=''%s'', &', [ExtractFileName(KrigingFactorsFileName)]));
    Script.Add('  source_clist=cl_input, &');
    Script.Add('  target_clist=cl_loc)');
    Script.Add('');
    Script.Add('#create new result list  ');
    Script.Add('resultValues = new_plist(reference_clist=cl_loc, value=0)');
    Script.Add('');
    Script.Add('# perform kriging.');
    Script.Add(Format('resultValues = p_1.krige_using_file(file=''%s'', transform=''none'')', [ExtractFileName(KrigingFactorsFileName)]));
    Script.Add('');
    Script.Add('#Write output');
    Script.Add(Format('write_column_data_file(file=''KrigResults.txt'', plist=resultValues)', [ExtractFileName(FKrigingOutputFileName)]));
    Script.Add('');
    Script.Add('');
    Script.Add(Format('', []));
    Script.SaveToFile(FScriptFileName);
  finally
    Script.Free;
  end;
end;

class function TCustomPlProcInterpolator.InterpolatorName: string;
begin
  result := 'PLPROC Kriging';
end;

procedure TCustomPlProcInterpolator.OnPlProcDone(Sender: TObject;
  ExitCode: DWORD);
begin
  if ExitCode <> 0 then
  begin
    raise EPlProcException.Create('Something went wrong with PLPROC');
  end;
  ReadResults;
end;

function TCustomPlProcInterpolator.RealResult(const Location: TPoint2D): real;
var
  ResultPointer: PDouble;
begin
  ResultPointer := FKrigedResults.NearestPointsFirstData(Location.x, Location.y);
  result := ResultPointer^;
end;

procedure TCustomPlProcInterpolator.StoreData(Sender: TObject;
  const DataSet: TDataArray);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  PointCount: integer;
  PointIndex: integer;
  APoint: TPoint2D;
  ListOfScreenObjects: TList;
  SectionIndex: integer;
  Count: integer;
  StoredLocations: TRbwQuadTree;
  MaxX: Real;
  MaxY: Real;
  MinX: Real;
  MinY: Real;
  X: double;
  Y: double;
  Data: TPointerArray;
  InterpWriter: TInterpolationDataWriter;
  ResultLocations: TResultLocationWriter;
  LocalModel: TCustomModel;
  Limits: TGridLimit;
  PlProcName: string;
  ViewDirection: TViewDirection;
  CurrentDir: string;
begin
  ListOfScreenObjects := TList.Create;
  try
    FillScreenObjectList(ListOfScreenObjects);

    // Check objects in reverse order because later objects should
    // override earlier ones, so if there is a tie, the later one
    // should win.

    Count := 0;

    MaxX := 0;
    MaxY := 0;
    MinX := 0;
    MinY := 0;
    for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
    begin
      AScreenObject := ListOfScreenObjects[ScreenObjectIndex];
      if ScreenObjectIndex = 0 then
      begin
        MaxX := AScreenObject.MaxX;
        MaxY := AScreenObject.MaxY;
        MinX := AScreenObject.MinX;
        MinY := AScreenObject.MinY;
      end
      else
      begin
        MaxX := Max(MaxX, AScreenObject.MaxX);
        MaxY := Max(MaxY, AScreenObject.MaxY);
        MinX := Min(MinX, AScreenObject.MinX);
        MinY := Min(MinY, AScreenObject.MinY);
      end;

      PointCount := AScreenObject.Count;
      for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
      begin
        if AScreenObject.SectionClosed[SectionIndex] then
        begin
          Dec(PointCount);
        end;
      end;

      Inc(Count, PointCount);

    end;
    SetLength(FValues, Count);

    StoredLocations := TRbwQuadTree.Create(nil);
    try
      StoredLocations.XMax := MaxX;
      StoredLocations.YMax := MaxY;
      StoredLocations.XMin := MinX;
      StoredLocations.YMin := MinY;

      Count := 0;
      for ScreenObjectIndex := ListOfScreenObjects.Count - 1 downto 0 do
      begin
        AScreenObject := ListOfScreenObjects[ScreenObjectIndex];

        PointCount := AScreenObject.Count;
        SectionIndex := 0;
        for PointIndex := 0 to PointCount - 1 do
        begin
          if AScreenObject.SectionEnd[SectionIndex] = PointIndex then
          begin
            Inc(SectionIndex);
            if AScreenObject.SectionClosed[SectionIndex-1] then
            begin
              Continue;
            end;
          end;
          APoint := AScreenObject.Points[PointIndex];
          if AScreenObject.ViewDirection = vdSide then
          begin
            APoint := InvertPoint(APoint);
          end;

          if StoredLocations.Count > 0 then
          begin
            X := APoint.X;
            Y := APoint.Y;
            StoredLocations.FindClosestPointsData(X, Y, Data);
            if (X = APoint.X) and (Y = APoint.Y) then
            begin
              Continue;
            end
            else
            begin
              StoredLocations.AddPoint(APoint.X, APoint.Y, nil);
            end;
          end;

          StoreDataValue(Count, DataSet, APoint, AScreenObject, SectionIndex-1);
          Inc(Count);

        end;
        AScreenObject.CacheSegments;
      end;

      SetLength(FValues, Count);
      LocalModel := DataSet.Model as TCustomModel;
      InterpWriter := TInterpolationDataWriter.Create(LocalModel, etExport);
      try
        FInputFileName := GetAppSpecificTempDir + LocalModel.DisplayName
          + '.' + DataSet.Name + InterpWriter.Extension;
        InterpWriter.WriteFile(FValues, FInputFileName);
        FFilesToDelete.Add(FInputFileName);
      finally
        InterpWriter.Free;
      end;

      ResultLocations := TResultLocationWriter.Create(LocalModel, etExport);
      try
        FResultLocationFileName := ChangeFileExt(FInputFileName, ResultLocations.Extension);
        if LocalModel.Grid <> nil then
        begin
          ResultLocations.WriteFile(LocalModel.Grid, DataSet.EvaluatedAt,
            DataSet.Orientation, FResultLocationFileName);
        end
        else
        begin
          ResultLocations.WriteFile(LocalModel.Mesh3D.Mesh2DI,
            DataSet.EvaluatedAt, FResultLocationFileName);
        end;
        FFilesToDelete.Add(FResultLocationFileName);
      finally
        ResultLocations.Free;
      end;
    finally
      StoredLocations.Free;
    end;

    FScriptFileName := ChangeFileExt(FInputFileName, '.plproc_script');
    WriteScript(DataSet);
    FFilesToDelete.Add(FScriptFileName);

    case DataSet.Orientation of
      dsoTop: ViewDirection := vdTop;
      dsoFront: ViewDirection := vdFront;
      dsoSide: ViewDirection := vdSide;
      dso3D: Assert(False);
    end;

    if LocalModel.Grid <> nil then
    begin
      Limits := LocalModel.Grid.GridLimits(ViewDirection);
    end
    else
    begin
      Limits := LocalModel.Mesh3D.MeshLimits(ViewDirection, 0);
    end;

    case DataSet.Orientation of
      dsoTop:
        begin
          FKrigedResults.XMax := Limits.MaxX;
          FKrigedResults.XMin := Limits.MinX;
          FKrigedResults.YMax := Limits.MaxY;
          FKrigedResults.YMin := Limits.MinY;
        end;
      dsoFront:
        begin
          FKrigedResults.XMax := Limits.MaxX;
          FKrigedResults.XMin := Limits.MinX;
          FKrigedResults.YMax := Limits.MaxZ;
          FKrigedResults.YMin := Limits.Minz;
        end;
      dsoSide:
        begin
          FKrigedResults.XMax := Limits.MaxY;
          FKrigedResults.XMin := Limits.MinY;
          FKrigedResults.YMax := Limits.MaxZ;
          FKrigedResults.YMin := Limits.Minz;
        end;
      dso3D:
        Assert(False);
    end;

    PlProcName  := ExtractFileDir(Application.ExeName)
      + '\' + 'plproc64.exe';
    if not TFile.Exists(PlProcName) then
    begin
      PlProcName  := ExtractFileDir(Application.ExeName)
        + '\' + 'plproc32.exe';
      if not TFile.Exists(PlProcName) then
      begin
        raise EPlProcException.Create('Neither plproc64.exe nor plproc32.exe are in the ModelMuse directory.');
      end;
    end;

    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(GetAppSpecificTempDir);
      RunAProgram(PlProcName + ' ' + ExtractFileName(FScriptFileName), OnPlProcDone);
    finally
      SetCurrentDir(CurrentDir)
    end;

  finally
    ListOfScreenObjects.Free;
  end;
end;

procedure TCustomPlProcInterpolator.StoreDataValue(Count: Integer;
  const DataSet: TDataArray; APoint: TPoint2D; AScreenObject: TScreenObject;
  SectionIndex: integer);
var
  Expression: TExpression;
begin
  inherited;
  InitializeVariablesAndExpression(APoint, AScreenObject, SectionIndex, Expression);
  Assert(DataSet.DataType = rdtDouble);
  try
    FValues[Count].x := APoint.x;
    FValues[Count].y := APoint.y;
    FValues[Count].z := Expression.DoubleResult;
  except on E: ERbwParserError do
    begin
      frmErrorsAndWarnings.AddError(DataSet.Model, StrErrorAssigningValu,
        Format(StrErrorMessage0, [E.Message, AScreenObject.Name]), AScreenObject);
      Exit;
    end;
  end;
end;

class function TCustomPlProcInterpolator.ValidReturnTypes: TRbwDataTypes;
begin
  Result := [rdtDouble];
end;

initialization
  RegisterClass(TNearest2DInterpolator);
  RegisterClass(TInvDistSq2DInterpolator);
  RegisterClass(TNearestPoint2DInterpolator);
  RegisterClass(TLinearSfrpackInterpolator);
  RegisterClass(TFittedSurfaceIntepolator);
  RegisterClass(TInvDistSqPoint2DInterpolator);
  RegisterClass(TNaturalNeighborInterp);
  RegisterClass(TPointAverageInterpolator);

  RegisterClass(TCustomPlProcInterpolator);


end.
