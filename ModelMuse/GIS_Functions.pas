{@abstract(@name defines GIS functions that can be used with a TRbwParser
  from within GoPhast.)  Some of the functions require access to
  global variables.  Most of these are set through "Update" procedures
  but one (@link(GlobalEvaluatedAt)) is declared in the interface section.

  For a list of the functions, see the User's Guide.}
unit GIS_Functions;

interface

uses Windows, SysUtils, Classes, RbwParser, ScreenObjectUnit, GoPhastTypes;

type
  TActiveOnLayer = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Active".
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TLayerSlope = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Active", "IDOMAIN", or determines the
      elevations or layers.
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TSpecifiedHeadOnLayer = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Specified_Head".
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TBcfVcont = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Kz", "Confining_Bed_Kz",
      or one of the variables for grid layer elevations.
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TCustomHufExpression = class(TExpression)
  public
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  THufKx = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufKy = class(THufKx)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufKz = class(THufKx)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSS = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSY = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSYTP = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;
{
 @name adds a series of (mostly) GIS function to Parser.
 The functions are defined in the initialization section.
 In addition, several descendants of TSelectExpression
 or TExpression are defined in the
 implementation section and added to RbwParser.SpecialImplentorList.
 @param(Parser is the TRbwParser to which the GIS functions will be added.)
}
procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection; EvalAt: TEvaluatedAt);

{ TODO -cRefactor : Consider replacing Model with an interface. }
//
procedure UpdateCurrentModel(const AModel: TBaseModel);

    { TODO -cRefactor : Consider replacing Model with an interface. }
// @name updates a series of global values related to location.
procedure UpdateGlobalLocations(const Col, Row, Layer: integer;
  const EvaluatedAt: TEvaluatedAt; Model: TBaseModel);

// @name stores AScreenObject in a global variable and resets the global
// variable for TCellElementSegment to nil.
procedure UpdateCurrentScreenObject(const AScreenObject: TScreenObject);

// @name stores ASegment in a global variable and also calls
// @link(UpdateCurrentSection).
procedure UpdateCurrentSegment(const ASegment: TCellElementSegment);

// @name stores SectionIndex in a global variable.
procedure UpdateCurrentSection(const SectionIndex: integer);

{ TODO : Move this to the implentation section. }
var
  // @name is the @link(TEvaluatedAt) of the current context.  This
  // should probably be defined in the implementation section.
  GlobalEvaluatedAt: TEvaluatedAt;

const
  // @name is the name of a function added in @link(AddGIS_Functions).
  rsListRealValue = 'ListRealValue';
  // @name is the name of a function added in @link(AddGIS_Functions).
  rsListIntegerValue = 'ListIntegerValue';
  rsObjectImportedValuesR = 'ObjectImportedValuesR';
  rsObjectImportedValuesI = 'ObjectImportedValuesI';
  rsObjectImportedValuesB = 'ObjectImportedValuesB';
  rsObjectImportedValuesT = 'ObjectImportedValuesT';
  StrImportedHigherElev = 'Imported Higher Elevations';
  StrImportedLowerEleva = 'Imported Lower Elevations';
  StrImportedElevations = 'Imported Elevations';
  StrBcfVCONT = 'BcfVCONT';
  StrHufKx = 'GetHufKx';
  StrHufKy = 'GetHufKy';
  StrHufKz = 'GetHuf_Interlayer_Kz';
  StrHufSs = 'GetHufSs';
  StrHufAverageSy = 'GetHuf_Average_Sy';
  StrHufSy = 'GetHufSy';
  StrHufSytp = 'GetHufSytp';
  StrLayerHeight = 'LayerHeight';
  StrInterpolatedVertexValues = 'InterpolatedVertexValue';
  StrObjectBasisFunction = 'ObjectBasisFunction';
  StrVertexInterpolate = 'VertexInterpolate';
  StrNodeInterpolate = 'NodeInterpolate';
  StrGridNumber = 'GridNumber';
  StrGridName = 'GridName';
  StrParentLayer = 'ParentLayer';
  StrParentRow = 'ParentRow';
  StrParentColumn = 'ParentColumn';
  StrHorizontalSubdivision = 'Horizontal_Subdivision';
  StrVerticalSubdivision = 'Vertical_Subdivision';
  StrSelectedCount = 'SelectedCount';
  ObjectCurrentSegmentAngle = 'ObjectCurrentSegmentAngle';
  ObjectDegrees = 'ObjectCurrentSegmentAngleDegrees';
  ObjectDegreesLimited = 'ObjectCurrentSegmentAngleLimitedDegrees';

  ObjectCurSegLength = 'ObjectCurrentSegmentLength';
  ObjectCurrentVertexX = 'ObjectCurrentVertexX';
  ObjectCurrentVertexY = 'ObjectCurrentVertexY';
  ObjectCurrentVertexZ = 'ObjectCurrentVertexZ';
  StrRasterValueAlongObject = 'RasterValueAlongObject';
  StrRasterSlopeAlongObject = 'RasterSlopeAlongObject';
  StrLowestVertexValue = 'LowestVertexValue';
  StrHighestVertexValue = 'HighestVertexValue';
  StrMeanVertexValue = 'MeanVertexValue';
  StrMedianVertexValue = 'MedianVertexValue';
  StrFirstVertexValue = 'FirstVertexValue';
  StrLastVertexValue = 'LastVertexValue';
//  StrVertexValueSlope = 'VertexValueSlope';


function GetColumnWidth(Column: Integer): Double;
function GetRowWidth(Row: Integer): Double;
function GetLayerHeight(Col, Row, Lay: Integer): Double;

function GetLayerPosition(const Lay, Row, Col: Integer;
  var InvalidIndex: boolean): Double;
function GetLayerCenter(const Lay, Row, Col:  integer): double;

    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
function CurrentModel: TBaseModel;

procedure PushGlobalStack;
procedure PopGlobalStack;

function CurrentObject: TScreenObject;

implementation

uses frmGoPhastUnit, DataSetUnit, FastGEO, LayerStructureUnit, PhastModelUnit,
  ValueArrayStorageUnit, HufDefinition, OrderedCollectionUnit,
  ModflowPackageSelectionUnit, Math, ModflowGridUnit, ModflowParameterUnit,
  frmErrorsAndWarningsUnit, SutraMeshUnit, AbstractGridUnit,
  RasterValuesAlongSegmentsUnit, ModflowPackagesUnit, RealListUnit,
  BasisFunctionUnit, ModflowIrregularMeshUnit, MeshRenumberingTypes,
  SparseDataSets, SparseArrayUnit, Point2DRegressionUnit, JmTypes;

resourcestring
  StrInSVANIParamete = 'In %s, VANI parameters are defined even though that ' +
  'hydrogeologic unit used vertical hydraulic conductivity and vertical anis' +
  'otropy. The VANI parameters will be ignored.';
  StrLayerDRowDCo = 'Layer %0:d, Row %1:d, Column %2:d';
  StrTheDataAccessedTh = 'The data accessed through %s are not real numbers.';
  StrTheDataAccessedThInt = 'The data accessed through %s are not integers.';
  StrTheDataAccessedThBoole = 'The data accessed through %s are not booleans' +
  '.';
  StrTheDataAccessedThStr = 'The data accessed through %s are not text.';
  StrTheSFunctionCan = 'The %s function can only be used with objects.';
  StrThe0sFunctionIs = 'The %0:s function is used but no values have been as' +
  'signed to individual nodes.';
  StrInvalidKeyIn0s = 'Invalid key in %0:s function.';
  StrObject0sInvali = 'Object: %0:s; invalid key: %1:s';
  StrNoImportedDataExi = 'No imported data exists in the object "%s" '
  + 'for the following name(s).';
  StrBecauseHorizontalH = 'Because horizontal hyraulic conductivity is zero ' +
  'in the following cells (Layer, Row, Column), vertical conductivity can no' +
  't be calculated. (This message can be ignored for inactive cells.)';
  StrTheSFunctionDoes = 'The %s function does not apply to locations that are' +
  ' not intersected by an object.';
  StrProblemEvaluating = 'Problem evaluating %s';
  StrThereAreTooFewIm = 'There are too few imported values for %0:s in %1:s.';
  StrLinkedRasterNotFo = 'Linked Raster not found';
  StrTheSFunctionCanTop = 'The %s function can only be used with objects on the' +
  ' top view of the model.';
  StrTheSFunctionCanLength = 'The %s function can only be used along the len' +
  'gth of an Object.';
  StrVertexValueNotFoundError = 'No defined vertex values in %s at the follo' +
  'wing locations and objects';
  StrGisObjectCellError = 'Object: %0:s; (Layer, Row, Column): (%1:d, %2:d, %3:d)';
  StrNoDefinedVertexVa = 'No defined vertex values labeled %0:s in %1:s at t' +
  'he following locations and objects';
  StrInvalidFormulaFor = 'Invalid formula for third dimension formula in the' +
  ' following objects';


var  
  SpecialImplementors: TList;

type
  TNodeInterpolateExpression = class(TSelectExpression)
  public
    procedure Evaluate; override;
  end;

  TGlobalValues = record
    GlobalX, GlobalY, GlobalZ: real;
    GlobalXPrime, GlobalYPrime: real;
    GlobalColumn, GlobalRow, GlobalLayer: integer;
    GlobalCurrentScreenObject: TScreenObject;
    GlobalCurrentSegment: TCellElementSegment;
    GlobalSection: integer;
    { TODO -cRefactor : Consider replacing GlobalCurrentModel with an interface. }
    //
    GlobalCurrentModel: TBaseModel;
    GlobalEvaluatedAt: TEvaluatedAt;
  end;


var
  NodeDistancesSet: boolean;
  FNodeDistances: array of double;

    { TODO -cRefactor : Consider replacing GlobalCurrentModel with an interface. }
    //
  GlobalCurrentModel: TBaseModel;
  GlobalX, GlobalY, GlobalZ: real;
  GlobalXPrime, GlobalYPrime: real;
  GlobalColumn, GlobalRow, GlobalLayer: integer;
  GlobalCurrentScreenObject: TScreenObject;
  GlobalCurrentSegment: TCellElementSegment;
  GlobalSection: integer;
//  GlobalEvalAt: TEvaluatedAt;


  GlobalStack: array of TGlobalValues;

  XFunction: TFunctionRecord;
  YFunction: TFunctionRecord;
  ZFunction: TFunctionRecord;
  XPrimeFunction: TFunctionRecord;
  YPrimeFunction: TFunctionRecord;
  ColumnFunction: TFunctionRecord;
  ElevationToLayerFunction: TFunctionRecord;
  ElevationToModelLayerFunction: TFunctionRecord;
  RowFunction: TFunctionRecord;
  LayerFunction: TFunctionRecord;
  ColumnWidthFunction: TFunctionRecord;
  RowWidthFunction: TFunctionRecord;
  LayerHeightFunction: TFunctionRecord;
  BlockAreaTopFunction: TFunctionRecord;
  BlockAreaFrontFunction: TFunctionRecord;
  BlockAreaSideFunction: TFunctionRecord;
  BlockVolumeFunction: TFunctionRecord;
  ColumnPositionFunction: TFunctionRecord;
  RowPositionFunction: TFunctionRecord;
  LayerPositionFunction: TFunctionRecord;
  ColumnCenterFunction: TFunctionRecord;
  RowCenterFunction: TFunctionRecord;
  LayerCenterFunction: TFunctionRecord;
  ColumnCountFunction: TFunctionRecord;
  RowCountFunction: TFunctionRecord;
  LayerCountFunction: TFunctionRecord;
  ObjectLengthFunction: TFunctionRecord;
  ObjectAreaFunction: TFunctionRecord;
  ObjectIntersectLengthFunction: TFunctionRecord;
  ObjectSectionIntersectLengthFunction: TFunctionRecord;
  ObjectIntersectAreaFunction: TFunctionRecord;
  ObjectNameFunction: TFunctionRecord;

  NodeXFunction: TFunctionRecord;
  NodeYFunction: TFunctionRecord;
  NodeZFunction: TFunctionRecord;
  NodeDistanceFunction: TFunctionRecord;
  CurrentNodeXFunction: TFunctionRecord;
  CurrentNodeYFunction: TFunctionRecord;
  CurrentNodeZFunction: TFunctionRecord;
  CurrentSegmentLengthFunction: TFunctionRecord;
  CurrentSegmentAngleFunction: TFunctionRecord;
  CurrentSegmentAngleDegreesFunction: TFunctionRecord;
  CurrentSegmentAngleLimitedegreesFunction: TFunctionRecord;
  CurrentSectionIndexFunction: TFunctionRecord;
  FractionOfObjectLengthFunction: TFunctionRecord;
  NodeCountFunction: TFunctionRecord;
  InterpolationedValuesFunction: TFunctionRecord;
  ObjectBasisFunction_Function: TFunctionRecord;
  LowestVertexValuesFunction: TFunctionRecord;
  HighestVertexValuesFunction: TFunctionRecord;
  MeanVertexValuesFunction: TFunctionRecord;
  MedianVertexValuesFunction: TFunctionRecord;
  FirstVertexValuesFunction: TFunctionRecord;
  LastVertexValuesFunction: TFunctionRecord;
  RasterValueAlongObject: TFunctionRecord;
  RasterSlopeAlongObject: TFunctionRecord;
  VertexValuesFunction: TFunctionRecord;
//  VertexValueSlopeFunction: TFunctionRecord;

  ListRealValueFunction: TFunctionRecord;
  ListIntegerValueFunction: TFunctionRecord;
  ModflowLayerSimulatedFunction: TFunctionRecord;
  ModflowLayerConfinedFunction: TFunctionRecord;
//  ModflowHufKx: TFunctionRecord;

  ImportedValuesRFunction: TFunctionRecord;
  ImportedValuesIFunction: TFunctionRecord;
  ImportedValuesBFunction: TFunctionRecord;
  ImportedValuesTFunction: TFunctionRecord;

  GridNumberFunction: TFunctionRecord;
  GridNameFunction: TFunctionRecord;
  ParentLayerFunction: TFunctionRecord;
  ParentRowFunction: TFunctionRecord;
  ParentColumnFunction: TFunctionRecord;

  HorizontalSubdivision_Function: TFunctionRecord;
  VerticalSubdivision_Function: TFunctionRecord;

  SelectedCount_Function: TFunctionRecord;

//  DipDirectionRadiansFunction: TFunctionRecord;

  NodeInterpolate: TFunctionClass;
  NodeInterpolateSpecialImplementor: TSpecialImplementor;

  ActiveOnLayer: TFunctionClass;
  ActiveOnLayerSpecialImplementor: TSpecialImplementor;

{  // these functions don't seem to help improve results with XT3D
  SlopeDirectionRadians: TFunctionClass;
  SlopeDirectionRadiansSpecialImplementor: TSpecialImplementor;

  SlopeAngleRadians: TFunctionClass;
  SlopeAngleRadiansSpecialImplementor: TSpecialImplementor;

  BedPerpendicularAngle2Degrees: TFunctionClass;
  BedPerpendicularAngle2DegreesSpecialImplementor: TSpecialImplementor;

  BedPerpendicularAngle3Degrees: TFunctionClass;
  BedPerpendicularAngle3DegreesSpecialImplementor: TSpecialImplementor;
  }

//  HighestActiveLayer: TFunctionClass;
//  HighestActiveLayerSpecialImplementor: TSpecialImplementor;

  SpecifiedHeadOnLayer: TFunctionClass;
  SpecifiedHeadOnLayerSpecialImplementor: TSpecialImplementor;

  BcfVcont: TFunctionClass;
  BcfVcontSpecialImplementor: TSpecialImplementor;

  HufKx: TFunctionClass;
  HufKxSpecialImplementor: TSpecialImplementor;

  HufKy: TFunctionClass;
  HufKySpecialImplementor: TSpecialImplementor;

  HufKz: TFunctionClass;
  HufKzSpecialImplementor: TSpecialImplementor;

  HufSS: TFunctionClass;
  HufSSSpecialImplementor: TSpecialImplementor;

  HufAverageSY: TFunctionClass;
  HufAverageSYSpecialImplementor: TSpecialImplementor;

  HufSY: TFunctionClass;
  HufSYSpecialImplementor: TSpecialImplementor;

  HufSYTP: TFunctionClass;
  HufSYTPSpecialImplementor: TSpecialImplementor;

  InvalidNames: TStringList;

function CurrentModel: TBaseModel;
begin
  result := GlobalCurrentModel;
end;

procedure PushGlobalStack;
var
  Position : integer;
begin
  Position := Length(GlobalStack);
  SetLength(GlobalStack, Position + 1);

  GlobalStack[Position].GlobalX := GlobalX;
  GlobalStack[Position].GlobalY := GlobalY;
  GlobalStack[Position].GlobalZ := GlobalZ;
  GlobalStack[Position].GlobalXPrime := GlobalXPrime;
  GlobalStack[Position].GlobalYPrime := GlobalYPrime;
  GlobalStack[Position].GlobalColumn := GlobalColumn;
  GlobalStack[Position].GlobalRow := GlobalRow;
  GlobalStack[Position].GlobalLayer := GlobalLayer;
  GlobalStack[Position].GlobalCurrentScreenObject := GlobalCurrentScreenObject;
  GlobalStack[Position].GlobalCurrentSegment := GlobalCurrentSegment;
  GlobalStack[Position].GlobalSection := GlobalSection;
  GlobalStack[Position].GlobalCurrentModel := GlobalCurrentModel;
  GlobalStack[Position].GlobalEvaluatedAt := GlobalEvaluatedAt;
end;

procedure PopGlobalStack;
var
  Position: Integer;
begin
  Position := Length(GlobalStack) -1;

  GlobalX := GlobalStack[Position].GlobalX;
  GlobalY := GlobalStack[Position].GlobalY;
  GlobalZ := GlobalStack[Position].GlobalZ;
  GlobalXPrime := GlobalStack[Position].GlobalXPrime;
  GlobalYPrime := GlobalStack[Position].GlobalYPrime;
  GlobalColumn := GlobalStack[Position].GlobalColumn;
  GlobalRow := GlobalStack[Position].GlobalRow;
  GlobalLayer := GlobalStack[Position].GlobalLayer;
  GlobalCurrentScreenObject := GlobalStack[Position].GlobalCurrentScreenObject;
  GlobalCurrentSegment := GlobalStack[Position].GlobalCurrentSegment;
  GlobalSection := GlobalStack[Position].GlobalSection;
  GlobalCurrentModel := GlobalStack[Position].GlobalCurrentModel;
  GlobalEvaluatedAt := GlobalStack[Position].GlobalEvaluatedAt;

  SetLength(GlobalStack, Position);
end;

procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection; EvalAt: TEvaluatedAt);
  procedure AddItem (Item: TFunctionRecord; ShouldAdd: boolean);
  var
    Index: integer;
  begin
    Index := Parser.Functions.IndexOf(Item.Name);
    if (Index > 0) and not ShouldAdd then
    begin
      Parser.Functions.Delete(Index);
    end;
    if (Index < 0) and ShouldAdd then
    begin
      Parser.Functions.Add(Item);
    end;
  end;
var
  Index: Integer;
  Item: TSpecialImplementor;
begin
  Parser.SpecialImplementorList.Clear;
  Parser.SpecialImplementorList.Capacity := SpecialImplementors.Count;
  for Index := 0 to SpecialImplementors.Count - 1 do
  begin
    Item := SpecialImplementors[Index];
    case EvalAt of
      eaBlocks: 
        begin
          Parser.SpecialImplementorList.Add(Item);
        end;
      eaNodes:
        begin
          if Item = ActiveOnLayerSpecialImplementor then
          begin
            if frmGoPhast.ModelSelection in SutraSelection then
            begin
              Parser.SpecialImplementorList.Add(Item);
            end;
          end
          else
          begin
            Parser.SpecialImplementorList.Add(Item);
          end;
        end;
      else Assert(False);
    end;
  end;
  // Make sure the GIS functions are available in Parser.
  AddItem(XFunction, True);
  AddItem(YFunction, True);
  AddItem(ZFunction, True);
  AddItem(XPrimeFunction, True);
  AddItem(YPrimeFunction, True);
  AddItem(ColumnFunction, True);
  AddItem(ElevationToLayerFunction, True);
  AddItem(ElevationToModelLayerFunction, True);
  AddItem(RowFunction, True);
  AddItem(LayerFunction, True);
  AddItem(ColumnWidthFunction, True);
  AddItem(RowWidthFunction, True);
  AddItem(LayerHeightFunction, True);
  AddItem(BlockAreaTopFunction, True);
  AddItem(BlockAreaFrontFunction, True);
  AddItem(BlockAreaSideFunction, True);
  AddItem(BlockVolumeFunction, True);
  AddItem(ColumnCountFunction, True);
  AddItem(RowCountFunction, True);
  AddItem(LayerCountFunction, True);
  AddItem(ColumnPositionFunction, True);
  AddItem(RowPositionFunction, True);
  AddItem(LayerPositionFunction, True);
  AddItem(ColumnCenterFunction, True);
  AddItem(RowCenterFunction, True);
  AddItem(LayerCenterFunction, True);
  AddItem(ObjectLengthFunction, True);
  AddItem(ObjectAreaFunction, True);
  AddItem(ObjectIntersectLengthFunction, True);
  AddItem(ObjectSectionIntersectLengthFunction, True);
  AddItem(ObjectIntersectAreaFunction, True);
  AddItem(ObjectNameFunction, True);
  AddItem(NodeXFunction, True);
  AddItem(NodeYFunction, True);
  AddItem(NodeZFunction, True);
  AddItem(NodeDistanceFunction, True);
  AddItem(CurrentNodeXFunction, True);
  AddItem(CurrentNodeYFunction, True);
  AddItem(CurrentNodeZFunction, True);
  AddItem(CurrentSegmentLengthFunction, True);
  AddItem(CurrentSegmentAngleFunction, True);
  AddItem(CurrentSegmentAngleDegreesFunction, True);
  AddItem(CurrentSegmentAngleLimitedegreesFunction, True);
  AddItem(CurrentSectionIndexFunction, True);
  AddItem(FractionOfObjectLengthFunction, True);
  AddItem(NodeCountFunction, True);
  AddItem(ListRealValueFunction, True);
  AddItem(ListIntegerValueFunction, True);
  AddItem(ModflowLayerSimulatedFunction, True);
  AddItem(ModflowLayerConfinedFunction, True);

//  AddItem(ModflowHufKx, True);
  AddItem(ImportedValuesRFunction, True);
  AddItem(ImportedValuesIFunction, True);
  AddItem(ImportedValuesBFunction, True);
  AddItem(ImportedValuesTFunction, True);
  AddItem(InterpolationedValuesFunction, True);
  AddItem(ObjectBasisFunction_Function, True);
  AddItem(LowestVertexValuesFunction, True);
  AddItem(HighestVertexValuesFunction, True);
  AddItem(MeanVertexValuesFunction, True);
  AddItem(MedianVertexValuesFunction, True);
  AddItem(FirstVertexValuesFunction, True);
  AddItem(LastVertexValuesFunction, True);
  AddItem(RasterValueAlongObject, True);
  AddItem(RasterSlopeAlongObject, True);
  AddItem(VertexValuesFunction, True);
//  AddItem(VertexValueSlopeFunction, True);
  AddItem(GridNumberFunction, True);
  AddItem(GridNameFunction, True);
  AddItem(ParentLayerFunction, True);
  AddItem(ParentRowFunction, True);
  AddItem(ParentColumnFunction, True);
  AddItem(HorizontalSubdivision_Function, True);
  AddItem(VerticalSubdivision_Function, True);
  AddItem(SelectedCount_Function, True);
//  AddItem(DipDirectionRadiansFunction, True);
end;

procedure GetNeighborData(Column, Row, Layer: Integer; EvalAt: TEvaluatedAt;
  var Locations: TPoint2DArray; var Elevations: TOneDRealArray);
var
  IDomainArray: TDataArray;
  ActiveDataArray: TDataArray;
  LocalModel: TCustomModel;
  Grid: TCustomModelGrid;
  Point3D: TPoint3D;
  Count: Integer;
  Mesh3D: IMesh3D;
  Element: IElement2D;
  ElementIndex: Integer;
  ElementList: TIElement2DList;
  OtherElement: IElement2D;
  function IsActive(Column, Row, Layer: Integer): Boolean;
  begin
    if GlobalCurrentModel.ModelSelection = msModflow2015 then
    begin
      result := IDomainArray.IntegerData[Layer, Row, Column] > 0;
    end
    else
    begin
      result := ActiveDataArray.BooleanData[Layer, Row, Column];
    end;
  end;
begin
  Locations := nil;
  Elevations := nil;
  if EvalAt <> eaBlocks then
  begin
    Exit;
  end;
  PushGlobalStack;
  try
    LocalModel :=  GlobalCurrentModel as TCustomModel;
    if GlobalCurrentModel.ModelSelection = msModflow2015 then
    begin
      IDomainArray := LocalModel.DataArrayManager.
        GetDataSetByName(K_IDOMAIN);
      IDomainArray.Initialize;
      ActiveDataArray := nil;
    end
    else
    begin
      ActiveDataArray := LocalModel.DataArrayManager.
        GetDataSetByName(rsActive);
      ActiveDataArray.Initialize;
      IDomainArray := nil;
    end;
  finally
    PopGlobalStack;
  end;
  if not IsActive(Column, Row, Layer) then
  begin ;
    Exit;
  end;
  Grid := LocalModel.Grid;
  if Grid <> nil then
  begin
    SetLength(Locations, 5);
    SetLength(Elevations, 5);
    Point3D :=  Grid.RotatedThreeDElementCenter(Column, Row, Layer);
    Locations[0].X := Point3D.X;
    Locations[0].Y := Point3D.Y;
    Elevations[0] := Point3D.Z;
    Count := 1;
    if (Column > 0) and IsActive(Column-1, Row, Layer) then
    begin
      Point3D :=  Grid.RotatedThreeDElementCenter(Column-1, Row, Layer);
      Locations[Count].X := Point3D.X;
      Locations[Count].Y := Point3D.Y;
      Elevations[Count] := Point3D.Z;
      Inc(Count);
    end;
    if (Row > 0) and IsActive(Column, Row-1, Layer) then
    begin
      Point3D :=  Grid.RotatedThreeDElementCenter(Column, Row-1, Layer);
      Locations[Count].X := Point3D.X;
      Locations[Count].Y := Point3D.Y;
      Elevations[Count] := Point3D.Z;
      Inc(Count);
    end;
    if (Column < Grid.ColumnCount -1) and IsActive(Column+1, Row, Layer) then
    begin
      Point3D :=  Grid.RotatedThreeDElementCenter(Column+1, Row, Layer);
      Locations[Count].X := Point3D.X;
      Locations[Count].Y := Point3D.Y;
      Elevations[Count] := Point3D.Z;
      Inc(Count);
    end;
    if (Row < Grid.RowCount -1) and IsActive(Column, Row+1, Layer) then
    begin
      Point3D :=  Grid.RotatedThreeDElementCenter(Column, Row+1, Layer);
      Locations[Count].X := Point3D.X;
      Locations[Count].Y := Point3D.Y;
      Elevations[Count] := Point3D.Z;
      Inc(Count);
    end;
    if Count <> 5 then
    begin
      SetLength(Locations, Count);
      SetLength(Elevations, Count);
    end;
  end
  else
  begin
    Mesh3D := LocalModel.Mesh3D;
    Element := Mesh3D.Mesh2DI.ElementsI2D[Column];
    ElementList  := TIElement2DList.Create;
    try
      Element.GetNeighborsI(ElementList);
      SetLength(Locations, ElementList.Count+1);
      SetLength(Elevations, ElementList.Count+1);
      Count := 0;
      Locations[Count] := Element.Center;
      if Mesh3D.Is3DMesh then
      begin
        Elevations[Count] :=
          Mesh3D.ElementArrayI[Layer, Element.ElementNumber].CenterElevation;
      end
      else
      begin
        Elevations[Count] := 0;
      end;
      Inc(Count);

      for ElementIndex := 0 to ElementList.Count - 1 do
      begin
        OtherElement := ElementList[ElementIndex];
        if IsActive(OtherElement.ElementNumber, 0, Layer) then
        begin
          Locations[Count] := OtherElement.Center;
          if Mesh3D.Is3DMesh then
          begin
            Elevations[Count] :=
              Mesh3D.ElementArrayI[Layer, OtherElement.ElementNumber].CenterElevation;
          end
          else
          begin
            Elevations[Count] := 0;
          end;
          Inc(Count);
        end;
      end;
    finally
      ElementList .Free;
    end;
  end;
end;

procedure GetCellIndicies(var Column, Row, Layer: Integer;
  Values: array of Pointer; FirstIndex: integer = 0);
begin
  // This function returns the hydraulic conductivity of a cell as calculated
  // using the method in the HUF package. At present, it accounts
  // for KDEP but not for LVDA.
  if (Length(Values) >= FirstIndex+1) and (Values[FirstIndex] <> nil) then
  begin
    Layer := PInteger(Values[FirstIndex])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  if (Length(Values) >= FirstIndex+2) and (Values[FirstIndex+1] <> nil) then
  begin
    Row := PInteger(Values[FirstIndex+1])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Length(Values) >= FirstIndex+3) and (Values[FirstIndex+2] <> nil) then
  begin
    Column := PInteger(Values[FirstIndex+2])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
end;

function NodeDistances(const Index: integer): double;
var
  PointIndex: integer;
  CumulativeDistance: double;
  APoint: TPoint2D;
  PriorPoint: TPoint2D;
  SectionIndex: Integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    if not NodeDistancesSet then
    begin
      SetLength(FNodeDistances, GlobalCurrentScreenObject.Count);
      if GlobalCurrentScreenObject.Count > 0 then
      begin
        CumulativeDistance := 0;
        FNodeDistances[0] := 0;
        PriorPoint := GlobalCurrentScreenObject.Points[0];
        SectionIndex := 0;
        for PointIndex := 1 to GlobalCurrentScreenObject.Count - 1 do
        begin
          APoint := GlobalCurrentScreenObject.Points[PointIndex];
          if GlobalCurrentScreenObject.SectionStart[SectionIndex] <> PointIndex then
          begin
            CumulativeDistance := CumulativeDistance
              + Sqrt(Sqr(APoint.X - PriorPoint.X)
              + Sqr(APoint.Y - PriorPoint.Y));
          end;
          FNodeDistances[PointIndex] := CumulativeDistance;
          PriorPoint := APoint;
          if GlobalCurrentScreenObject.SectionEnd[SectionIndex] = PointIndex then
          begin
            Inc(SectionIndex);
          end;
        end;
      end;
      NodeDistancesSet := True;
    end;
    if (Index < 0) or (Index >= Length(FNodeDistances)) then
    begin
      result := 0;
    end
    else
    begin
      result := FNodeDistances[Index];
    end;
  end;
end;

procedure UpdateGlobalLocations(const Col, Row, Layer: integer;
  const EvaluatedAt: TEvaluatedAt; Model: TBaseModel);
var
  CC2D: TPoint2D;
  CC3D: T3DRealPoint;
  LocalModel: TCustomModel;
  Node: TSutraNode2D;
  Node3D: TSutraNode3D;
  Element: TSutraElement2D;
  ECenter: TPoint2D;
  Element3D: TSutraElement3D;
  SutraMesh: TSutraMesh3D;
  ACell: TModflowDisVCell;
  ANode: TModflowNode;
begin
  GlobalColumn := Col + 1;
  GlobalRow := Row + 1;
  GlobalLayer := Layer + 1;
  GlobalEvaluatedAt := EvaluatedAt;
  UpdateCurrentModel(Model);
  LocalModel := Model as TCustomModel;

  if LocalModel.ModelSelection in SutraSelection then
  begin
    SutraMesh := LocalModel.Mesh as TSutraMesh3D;
    case EvaluatedAt of
      eaBlocks:
        begin
          Element := SutraMesh.Mesh2D.Elements[Col];
          ECenter := Element.Center;
          GlobalX := ECenter.X;
          GlobalY := ECenter.Y;
          if SutraMesh.MeshType in [mt2D, mtProfile] then
          begin
            GlobalZ := 0;
          end
          else
          begin
            Element3D := SutraMesh.ElementArray[Layer,Col];
            GlobalZ := Element3D.CenterElevation;
          end;
        end;
      eaNodes:
        begin
          Node := SutraMesh.Mesh2D.Nodes[Col];
          GlobalX := Node.X;
          GlobalY := Node.Y;
          if SutraMesh.MeshType in [mt2D, mtProfile] then
          begin
            GlobalZ := 0;
          end
          else
          begin
            if SutraMesh.Nodes.Count > 0 then
            begin
              Node3D :=  SutraMesh.NodeArray[Layer,Col];
              GlobalZ := Node3D.Z;
            end
            else
            begin
              GlobalZ := 0;
            end;
          end;
        end;
      else Assert(False);
    end;
    GlobalXPrime := GlobalX;
    GlobalYPrime := GlobalY;
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          CC2D := LocalModel.TwoDElementCenter(Col, Row);
          GlobalX := CC2D.X;
          GlobalY := CC2D.Y;

          if LocalModel.DisvUsed then
          begin
            ACell := LocalModel.DisvGrid.Layers[Layer].Layer[Col];
            GlobalZ := (ACell.Top + ACell.Bottom)/2;
            GlobalXPrime := GlobalX;
            GlobalYPrime := GlobalY;
          end
          else
          begin
            CC3D := LocalModel.Grid.ThreeDElementCenter(Col, Row, Layer);
            GlobalZ := CC3D.Z;
            GlobalXPrime := CC3D.X;
            GlobalYPrime := CC3D.Y;
          end;
        end;
      eaNodes:
        begin
          if  LocalModel.DisvUsed then
          begin
            ANode :=  LocalModel.DisvGrid.TwoDGrid.CellCorners[Col];
            GlobalX := ANode.X;
            GlobalY := ANode.Y;
            { TODO -cMODFLOW 6 : This needs to be updated }
            GlobalZ := 0;
            GlobalXPrime := GlobalX;
            GlobalYPrime := GlobalY;
          end
          else
          begin
            CC2D := LocalModel.Grid.TwoDElementCorner(Col, Row);
            GlobalX := CC2D.X;
            GlobalY := CC2D.Y;

            CC3D := LocalModel.Grid.ThreeDElementCorner(Col, Row, Layer);
            GlobalZ := CC3D.Z;
            GlobalXPrime := CC3D.X;
            GlobalYPrime := CC3D.Y;
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure UpdateCurrentSection(const SectionIndex: integer);
begin
  GlobalSection := SectionIndex;
end;

procedure UpdateCurrentSegment(const ASegment: TCellElementSegment);
begin
  GlobalCurrentSegment := ASegment;
  if GlobalCurrentSegment = nil then
  begin
    UpdateCurrentSection(-1);
  end
  else
  begin
    UpdateCurrentSection(ASegment.SectionIndex);
  end;
end;

procedure UpdateCurrentModel(const AModel: TBaseModel);
begin
  Assert(AModel is TCustomModel);
  GlobalCurrentModel := AModel;
end;

procedure UpdateCurrentScreenObject(const AScreenObject: TScreenObject);
begin
  UpdateCurrentSegment(nil);
  GlobalCurrentScreenObject := AScreenObject;
  NodeDistancesSet := False;
end;

function _SlopeDirectionRadians(Values: array of pointer): double;
var
  Locations: TPoint2DArray;
  Elevations: TOneDRealArray;
  ZValues: TDynDoubleArray;
  Index: Integer;
  Coefficients: TDynDoubleArray;
begin
  GetNeighborData(GlobalColumn-1, GlobalRow-1, GlobalLayer-1, GlobalEvaluatedAt,
    Locations, Elevations);
  SetLength(ZValues, Length(Elevations));
  for Index := 0 to Length(Elevations) - 1 do
  begin
    ZValues[Index] := Elevations[Index];
  end;
  Regress2DPointsExpanded(Locations, ZValues, Coefficients);
  if (Coefficients[2] = 0) and (Coefficients[1] = 0) then
  begin
    result := 0;
  end
  else
  begin
    result := ArcTan2(Coefficients[2], Coefficients[1]);
    if result <= 0 then
    begin
      result := result + Pi;
    end
    else
    begin
      result := result - Pi;
    end;
  end;
end;

function _DipAngleRadians(Values: array of pointer): double;
var
  Locations: TPoint2DArray;
  Elevations: TOneDRealArray;
  ZValues: TDynDoubleArray;
  BeddingCoefficients: TDynDoubleArray;
  Index: Integer;
  HorizontalDistance: double;
  ThreeDDistance: Extended;
  DeltaZ: Double;
  Fraction: double;
  Direction: Double;
begin
  GetNeighborData(GlobalColumn-1, GlobalRow-1, GlobalLayer-1, GlobalEvaluatedAt,
    Locations, Elevations);
  SetLength(ZValues, Length(Elevations));
  for Index := 0 to Length(Elevations) - 1 do
  begin
    ZValues[Index] := Elevations[Index];
  end;
  Regress2DPointsExpanded(Locations, ZValues, BeddingCoefficients);
  Direction := _SlopeDirectionRadians([]);
  DeltaZ := BeddingCoefficients[1]*Cos(Direction) + BeddingCoefficients[2]*Sin(Direction);
  HorizontalDistance := 1;
  ThreeDDistance := Sqrt(1 + Sqr(DeltaZ));
  if ThreeDDistance > 0 then
  begin
    Fraction := HorizontalDistance/ThreeDDistance;
    result := ArcCos(Fraction);
  end
  else
  begin
    result := 0;
  end;
end;

function _BedPerpendicularAngle2Radians(Values: array of pointer;
  var BeddingCoefficients: TDynDoubleArray): double;
var
  Angle1: Double;
  Locations: TPoint2DArray;
  Elevations: TOneDRealArray;
  ZValues: TDynDoubleArray;
  Index: Integer;
  DeltaZ: Double;
  HorizontalDistance: Integer;
  ThreeDDistance: Extended;
  Fraction: Extended;
begin
  Angle1 := PDouble(Values[0])^/180*Pi;
  GetNeighborData(GlobalColumn-1, GlobalRow-1, GlobalLayer-1, GlobalEvaluatedAt,
    Locations, Elevations);
  SetLength(ZValues, Length(Elevations));
  for Index := 0 to Length(Elevations) - 1 do
  begin
    ZValues[Index] := Elevations[Index];
  end;
  Regress2DPointsExpanded(Locations, ZValues, BeddingCoefficients);
  DeltaZ := BeddingCoefficients[1]*Cos(Angle1) + BeddingCoefficients[2]*Sin(Angle1);
  HorizontalDistance := 1;
  ThreeDDistance := Sqrt(1 + Sqr(DeltaZ));
  if ThreeDDistance > 0 then
  begin
    Fraction := HorizontalDistance/ThreeDDistance;
    result := ArcCos(Fraction);
  end
  else
  begin
    result := 0;
  end;
end;

function _BedPerpendicularAngle2Degrees(Values: array of pointer): double;
var
  BeddingCoefficients: TDynDoubleArray;
begin
  result := _BedPerpendicularAngle2Radians(Values, BeddingCoefficients)/pi*180;
end;

function _BedPerpendicularAngle3Radians(Values: array of pointer): double;
var
  Angle1: double;
  BeddingCoefficients: TDynDoubleArray;
  Angle2: Double;
  Locations: TPoint2DArray;
  Elevations: TDynDoubleArray;
  Coefficients: TDynDoubleArray;
  V1, V2: TDynDoubleArray;
  CosAngle: double;
  function VectorLength(Vector: TDynDoubleArray): double;
  var
    Index: Integer;
  begin
    result := 0;
    for Index := 0 to Length(Vector) - 1 do
    begin
      result := result + Sqr(Vector[Index]);
    end;
    result := Sqrt(result);
  end;
  function VectorProduct(Vector1, Vector2: TDynDoubleArray): double;
  var
    Index: Integer;
  begin
    Assert(Length(Vector1) = Length(Vector2));
    result := 0;
    for Index := 0 to Length(Vector1) - 1 do
    begin
      result := result + Vector1[Index] * Vector2[Index];
    end;
  end;
begin
  Angle1 := PDouble(Values[0])^/180*Pi;
  Angle2 := _BedPerpendicularAngle2Radians(Values, BeddingCoefficients);

  SetLength(Locations, 3);
  SetLength(Elevations, 3);

  Locations[0].x := 0;
  Locations[0].y := 0;
  Elevations[0] := BeddingCoefficients[0];

  Locations[1].x := Cos(Angle1);
  Locations[1].y := Sin(Angle1);
  Elevations[1] := BeddingCoefficients[0] + Sin(Angle2);

  Locations[2].x := Cos(Angle1-Pi/2);
  Locations[2].y := Sin(Angle1-Pi/2);
  Elevations[2] := BeddingCoefficients[0];

  Regress2DPoints(Locations, Elevations, Coefficients);

  if (Coefficients[1] <> 0) or (Coefficients[2] <> 0) then
  begin
    SetLength(V1, 3);
    SetLength(V2, 3);

    V1[0] := Coefficients[1];
    V1[1] := Coefficients[2];
    V1[2] := -1;

    V2[0] := BeddingCoefficients[1];
    V2[1] := BeddingCoefficients[2];
    V2[2] := -1;//*Sqrt(Sqr(V2[0]) + Sqr(V2[1]))/Sqrt(Sqr(V1[0]) + Sqr(V1[1]));

    CosAngle := VectorProduct(V1, v2)/(VectorLength(V1) * VectorLength(v2));
    Assert(Abs(CosAngle) -1 < 1e-8);
    CosAngle := Min(CosAngle, 1);
    CosAngle := Max(CosAngle, -1);
    result := ArcCos(CosAngle);
  end
  else
  begin
    result := -_DipAngleRadians(Values);
  end;
end;

function _BedPerpendicularAngle3Degrees(Values: array of pointer): double;
begin
  result := _BedPerpendicularAngle3Radians(Values)/Pi*180;
end;

function _XNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop, vdFront:
          begin
            result := GlobalCurrentScreenObject.Points[Index].X;
          end;
        vdSide:
          begin
            result := 0;
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _YNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y;
          end;
        vdFront:
          begin
            result := 0;
          end;
        vdSide:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _ZNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            result := 0;
          end;
        vdFront:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y;
          end;
        vdSide:
          begin
            result := GlobalCurrentScreenObject.Points[Index].X;
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _NodeDistances(Values: array of pointer): double;
var
  Index: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    result := NodeDistances(Index);
  end;
end;

function _CurrentXNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
        end;
      vdSide:
        begin
          result := 0;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentYNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.Y2;
          end
          else
          begin
            result := GlobalCurrentSegment.Y1;
          end;
        end;
      vdFront:
        begin
          Result:= 0;
        end;
      vdside:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentZNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop:
        begin
          Result:= 0;
        end;
      vdFront:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.Y2;
          end
          else
          begin
            result := GlobalCurrentSegment.Y1;
          end;
        end;
      vdSide:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
//          result := GlobalCurrentSegment.Y1;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentSectionIndex(Values: array of pointer): integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalSection+1;
  end;
end;

function _CurrentSegmentAngle(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
  LocalGrid : TCustomModelGrid;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    result := 0;
  end
  else
  begin
    result := 0;
    if (GlobalCurrentSegment.X1 = GlobalCurrentSegment.X2)
      and (GlobalCurrentSegment.Y1 = GlobalCurrentSegment.Y2) then
    begin
      Exit;
    end;

    Point1 := GlobalCurrentSegment.StartPoint;
    Point2 := GlobalCurrentSegment.EndPoint;

//    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
//    Point2 := GlobalCurrentScreenObject.Points[
//      GlobalCurrentSegment.VertexIndex + 1];

    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          result := ArcTan2(Point2.y - Point1.y, Point2.x - Point1.x);
        end;
      vdSide:
        begin
          result := ArcTan2(Point2.x - Point1.x, Point2.y - Point1.y);
        end;
      else
        Assert(False);
    end;

    if GlobalCurrentScreenObject.ViewDirection = vdTop then
    begin
      LocalGrid := (GlobalCurrentModel as TCustomModel).Grid;
      if LocalGrid <> nil then
      begin
        result := result - LocalGrid.GridAngle;
      end;
    end;
  end;
end;

function _CurrentSegmentAngleDegrees(Values: array of pointer): double;
begin
  result := _CurrentSegmentAngle(Values)*180/Pi;
end;

function _CurrentSegmentAngleLimitedDegrees(Values: array of pointer): double;
begin
  result := _CurrentSegmentAngleDegrees(Values);
  while result > 90 do
  begin
    result := result -180;
  end;
  while result < -90 do
  begin
    result := result +180;
  end;
end;

function _SelectedCount(Values: array of pointer): integer;
var
  CellList: TCellAssignmentList;
  Cells: T3DSparseBooleanArray;
  CellIndex: Integer;
  ACell: TCellAssignment;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    Exit;
  end
  else
  begin
    CellList := TCellAssignmentList.Create;
    try
      GlobalCurrentScreenObject.GetCellsToAssign({Grid,} '0', nil, nil,
        CellList, alAll, GlobalCurrentModel);
      Cells := T3DSparseBooleanArray.Create(SPASmall, SPASmall, SPASmall);
      try
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          if not Cells.IsValue[ACell.Layer, ACell.Row, ACell.Column] then
          begin
            Cells.Items[ACell.Layer, ACell.Row, ACell.Column] := True;
            Inc(Result)
          end;
        end;
      finally
        Cells.Free;
      end;
    finally
      CellList.Free;
    end;
  end;
end;

function _ValueAlongSegment(Values: array of pointer): double;
var
  RasterName: string;
  Raster: IRaster;
  ErrorMessage: string;
//  ASegment: TSegment2D;
  SegmentStart: Integer;
  SegmentEnd: Integer;
  Segments: TCellElementSegmentList;
  PriorSegment: TCellElementSegment;
  ASegmentObject: TCellElementSegment;
  ALine: TPolyLine2D;
  Grid: TCustomModelGrid;
  SegmentIndex: Integer;
  PointIndex: Integer;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
    end;
  end;
  function PointsNearlyTheSame(Point1, Point2: TPoint2D): boolean;
  begin
    result := NearlyTheSame(Point1.X, Point2.X) and
      NearlyTheSame(Point1.Y, Point2.Y);
  end;
begin
  result := 0;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) or (GlobalCurrentModel = nil) then
  begin
    if GlobalCurrentScreenObject = nil then
    begin
      ErrorMessage := Format(StrTheSFunctionCan, [StrRasterValueAlongObject]);
      frmErrorsAndWarnings.AddError(GlobalCurrentModel,
        ErrorMessage, ErrorMessage);
    end
    else
    begin
      ErrorMessage := Format(StrTheSFunctionCanLength,
         [StrRasterValueAlongObject]);
      frmErrorsAndWarnings.AddError(GlobalCurrentModel, ErrorMessage,
        GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
    end;
    Exit;
  end
  else if GlobalCurrentScreenObject.ViewDirection <> vdTop then
  begin
    frmErrorsAndWarnings.AddError(GlobalCurrentModel,
      Format(StrTheSFunctionCanTop, [StrRasterValueAlongObject]),
      GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
    Exit;
  end
  else
  begin
    RasterName := PString(Values[0])^;
    Raster := (GlobalCurrentModel as TCustomModel).
      LinkedRasters.RasterByName(RasterName);
    if Assigned(Raster) then
    begin
      Assert(GlobalCurrentSegment.PositionInSegmentList >= 0);


      Segments := GlobalCurrentScreenObject.Segments[GlobalCurrentModel];

      SegmentStart := GlobalCurrentSegment.PositionInSegmentList;
      PriorSegment := GlobalCurrentSegment;
      for SegmentIndex := SegmentStart - 1 downto 0 do
      begin
        ASegmentObject := Segments[SegmentIndex];
        if (ASegmentObject.Layer = GlobalCurrentSegment.Layer)
          and (ASegmentObject.Row = GlobalCurrentSegment.Row)
          and (ASegmentObject.Col = GlobalCurrentSegment.Col)
          and PointsNearlyTheSame(ASegmentObject.EndPoint,
          PriorSegment.StartPoint) then
        begin
          SegmentStart := SegmentIndex;
          PriorSegment := ASegmentObject;
        end
        else
        begin
          Break
        end;
      end;

      SegmentEnd := GlobalCurrentSegment.PositionInSegmentList;
      PriorSegment := GlobalCurrentSegment;
      for SegmentIndex := SegmentEnd + 1 to Segments.Count -1 do
      begin
        ASegmentObject := Segments[SegmentIndex];
        if (ASegmentObject.Layer = GlobalCurrentSegment.Layer)
          and (ASegmentObject.Row = GlobalCurrentSegment.Row)
          and (ASegmentObject.Col = GlobalCurrentSegment.Col)
          and PointsNearlyTheSame(ASegmentObject.StartPoint,
          PriorSegment.EndPoint) then
        begin
          SegmentEnd := SegmentIndex;
          PriorSegment := ASegmentObject;
        end
        else
        begin
          Break
        end;
      end;

      SetLength(ALine, SegmentEnd-SegmentStart+2);
      ASegmentObject := Segments[SegmentStart];
      ALine[0] := ASegmentObject.Segment[1];
      for SegmentIndex := SegmentStart to SegmentEnd do
      begin
        ASegmentObject := Segments[SegmentIndex];
        ALine[SegmentIndex-SegmentStart+1] := ASegmentObject.Segment[2];
      end;

      Grid := frmGoPhast.Grid;
      if (Grid <> nil) and (Grid.GridAngle <> 0) then
      begin
        for PointIndex := 0 to Length(ALine) - 1 do
        begin
          ALine[PointIndex] := Grid.RotateFromGridCoordinatesToRealWorldCoordinates(ALine[PointIndex]);
        end;
      end;

//      ASegment := GlobalCurrentSegment.Segment;
//      if frmGoPhast.Grid <> nil then
//      begin
//        ASegment[1] := frmGoPhast.Grid.RotateFromGridCoordinatesToRealWorldCoordinates(ASegment[1]);
//        ASegment[2] := frmGoPhast.Grid.RotateFromGridCoordinatesToRealWorldCoordinates(ASegment[2]);
//      end;
      result := GetLineValueFromRaster(ALine, Raster);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(GlobalCurrentModel,
        StrLinkedRasterNotFo, RasterName);
    end;
  end;
end;

function _SlopeAlongSegment(Values: array of pointer): double;
var
  RasterName: string;
  Raster: IRaster;
  ErrorMessage: string;
  ASegment: TSegment2D;
begin
  result := 0;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) or (GlobalCurrentModel = nil) then
  begin
    if GlobalCurrentScreenObject = nil then
    begin
      ErrorMessage := Format(StrTheSFunctionCan, [StrRasterSlopeAlongObject]);
      frmErrorsAndWarnings.AddError(GlobalCurrentModel,
        ErrorMessage, ErrorMessage);
    end
    else
    begin
      ErrorMessage := Format(StrTheSFunctionCanLength,
         [StrRasterSlopeAlongObject]);
      frmErrorsAndWarnings.AddError(GlobalCurrentModel, ErrorMessage,
        GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
    end;
    Exit;
  end
  else if GlobalCurrentScreenObject.ViewDirection <> vdTop then
  begin
    frmErrorsAndWarnings.AddError(GlobalCurrentModel,
      Format(StrTheSFunctionCanTop, [StrRasterSlopeAlongObject]),
      GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
    Exit;
  end
  else
  begin
    RasterName := PString(Values[0])^;
    Raster := (GlobalCurrentModel as TCustomModel).
      LinkedRasters.RasterByName(RasterName);
    if Assigned(Raster) then
    begin
      ASegment := GlobalCurrentSegment.Segment;
      if frmGoPhast.Grid <> nil then
      begin
        ASegment[1] := frmGoPhast.Grid.RotateFromGridCoordinatesToRealWorldCoordinates(ASegment[1]);
        ASegment[2] := frmGoPhast.Grid.RotateFromGridCoordinatesToRealWorldCoordinates(ASegment[2]);
      end;
      result := GetSegmentValueFromRaster(ASegment, Raster);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(GlobalCurrentModel,
        StrLinkedRasterNotFo, RasterName);
    end;
  end;
end;

function _CurrentSegmentLength(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
begin
  result := 0;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    Exit;
  end
  else
  begin
    if GlobalCurrentScreenObject.SectionLength[
      GlobalCurrentSegment.SectionIndex] = 1 then
    begin
      Exit;
    end;
    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
    Point2 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex + 1];
    result := Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y));
  end;
end;

function _ListDataSetRealValue(Values: array of pointer): double;
var
  DataSetName: string;
  Item: TRealDataListItem;
  Index: integer;
begin
  result := 0;
  if (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1)
    or not (GlobalCurrentScreenObject is TMultiValueScreenObject) then
  begin
    Exit;
  end
  else
  begin
    DataSetName := PString(Values[0])^;
    with TMultiValueScreenObject(GlobalCurrentScreenObject) do
    begin
      Item := RealValues.GetItemByName(DataSetName) as TRealDataListItem;
      if Item <> nil then
      begin
        Index := Item.ValueIndex(GlobalColumn - 1, GlobalRow - 1, GlobalLayer -
          1);
        if Index >= 0 then
        begin
          result := Item.Values[Index];
        end;
      end;
    end;
  end;
end;

function _ListDataSetIntegerValue(Values: array of pointer): integer;
var
  DataSetName: string;
  Item: TIntegerDataListItem;
  Index: integer;
begin
  result := 0;
  if (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1)
    or not (GlobalCurrentScreenObject is TMultiValueScreenObject) then
  begin
    Exit;
  end
  else
  begin
    DataSetName := PString(Values[0])^;
    with TMultiValueScreenObject(GlobalCurrentScreenObject) do
    begin
      Item := IntegerValues.GetItemByName(DataSetName) as TIntegerDataListItem;
      if Item <> nil then
      begin
        Index := Item.ValueIndex(GlobalColumn - 1, GlobalRow - 1, GlobalLayer -
          1);
        if Index >= 0 then
        begin
          result := Item.Values[Index];
        end;
      end;
    end;
  end;
end;

function _VertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  DefaultValue: double;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  Point1: TPoint2D;
  CurrentSegmentStartPoint: TPoint2D;
  LocalEpsilon: double;
  Point2: TPoint2D;
  CurrentSegmentEndPoint: TPoint2D;
  SegmentPosition: Integer;
  SegmentStart: Integer;
  Segments: TCellElementSegmentList;
  ASegment: TCellElementSegment;
  SegmentEnd: Integer;
  SegmentIndex: Integer;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < LocalEpsilon;
    end;
  end;
begin
  Assert(Length(Values) >= 2);
  VertexValueName := PString(Values[0])^;
  DefaultValue := PDouble(Values[1])^;
  result := DefaultValue;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.PointPositionValues = nil) then
  begin
    Exit;
  end
  else
  begin
    if GlobalCurrentScreenObject.GettingSegments[GlobalCurrentModel] then
    begin
//      SegmentPosition := GlobalCurrentSegment.PositionInSegmentList;
//      LocalEpsilon := GlobalCurrentSegment.SegmentLength/100000;
      LocalPPV := GlobalCurrentScreenObject.PointPositionValues;
      Item := LocalPPV.Items[GlobalCurrentSegment.VertexIndex] as TPointValuesItem;
      VVIndex := Item.IndexOfName(VertexValueName);
      if VVIndex >= 0 then
      begin
        result := Item.Value[VVIndex];
        Exit;
      end;
    end
    else
    begin
      Segments := GlobalCurrentScreenObject.Segments[GlobalCurrentModel];
      SegmentPosition := GlobalCurrentSegment.PositionInSegmentList;

  //    SegmentPosition := Segments.IndexOf(GlobalCurrentSegment);
      Assert(SegmentPosition >= 0);
      SegmentStart := SegmentPosition;
      for SegmentIndex := SegmentPosition-1 downto 0 do
      begin
        ASegment := Segments[SegmentIndex];
        if (ASegment.Col = GlobalCurrentSegment.Col)
          and (ASegment.Row = GlobalCurrentSegment.Row)
          and (ASegment.Layer = GlobalCurrentSegment.Layer)
          then
        begin
          SegmentStart := SegmentIndex;
        end
        else
        begin
          Break;
        end;
      end;

      SegmentEnd := SegmentPosition;
      for SegmentIndex := SegmentPosition +1 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        if (ASegment.Col = GlobalCurrentSegment.Col)
          and (ASegment.Row = GlobalCurrentSegment.Row)
          and (ASegment.Layer = GlobalCurrentSegment.Layer)
          then
        begin
          SegmentEnd := SegmentIndex;
        end
        else
        begin
          Break;
        end;
      end;

      LocalEpsilon := GlobalCurrentSegment.SegmentLength/100000;
      LocalPPV := GlobalCurrentScreenObject.PointPositionValues;
      Item := LocalPPV.GetItemByPosition(GlobalCurrentSegment.VertexIndex);
      if Item <> nil then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          AValue := Item.Value[VVIndex];
          for SegmentIndex := SegmentEnd downto SegmentStart do
          begin
            ASegment := Segments[SegmentIndex];
            if Item.Position = ASegment.VertexIndex+1 then
            begin
              Point2 := GlobalCurrentScreenObject.Points[
                ASegment.VertexIndex+1];
              CurrentSegmentEndPoint.X := ASegment.X2;
              CurrentSegmentEndPoint.Y := ASegment.Y2;
              if frmGoPhast.Grid <> nil then
              begin
                CurrentSegmentEndPoint := frmGoPhast.Grid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentEndPoint);
              end;
              if NearlyTheSame(CurrentSegmentEndPoint.X, Point2.X)
                and NearlyTheSame(CurrentSegmentEndPoint.Y, Point2.Y) then
              begin
                 result := AValue;
                 Exit;
              end;
            end;
          end;

          for SegmentIndex := SegmentEnd downto SegmentStart do
          begin
            ASegment := Segments[SegmentIndex];
            if Item.Position = ASegment.VertexIndex then
            begin
              Point1 := GlobalCurrentScreenObject.Points[
                ASegment.VertexIndex];
              CurrentSegmentStartPoint.X := ASegment.X1;
              CurrentSegmentStartPoint.Y := ASegment.Y1;
              if frmGoPhast.Grid <> nil then
              begin
                CurrentSegmentStartPoint := frmGoPhast.Grid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentStartPoint);
              end;
              if NearlyTheSame(CurrentSegmentStartPoint.X, Point1.X)
                and NearlyTheSame(CurrentSegmentStartPoint.Y, Point1.Y) then
              begin
                result := AValue;
                Exit;
              end;
            end;
          end;
        end;
	    end;
	  
      for Index := LocalPPV.Count - 1 downto 0 do
      begin
        Item := LocalPPV.Items[Index] as TPointValuesItem;
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          AValue := Item.Value[VVIndex];
          for SegmentIndex := SegmentEnd downto SegmentStart do
          begin
            ASegment := Segments[SegmentIndex];
            if Item.Position = ASegment.VertexIndex+1 then
            begin
              Point2 := GlobalCurrentScreenObject.Points[
                ASegment.VertexIndex+1];
              CurrentSegmentEndPoint.X := ASegment.X2;
              CurrentSegmentEndPoint.Y := ASegment.Y2;
              if frmGoPhast.Grid <> nil then
              begin
                CurrentSegmentEndPoint := frmGoPhast.Grid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentEndPoint);
              end;
              if NearlyTheSame(CurrentSegmentEndPoint.X, Point2.X)
                and NearlyTheSame(CurrentSegmentEndPoint.Y, Point2.Y) then
              begin
                 result := AValue;
                 Exit;
              end;
            end;
          end;

          for SegmentIndex := SegmentEnd downto SegmentStart do
          begin
            ASegment := Segments[SegmentIndex];
            if Item.Position = ASegment.VertexIndex then
            begin
              Point1 := GlobalCurrentScreenObject.Points[
                ASegment.VertexIndex];
              CurrentSegmentStartPoint.X := ASegment.X1;
              CurrentSegmentStartPoint.Y := ASegment.Y1;
              if frmGoPhast.Grid <> nil then
              begin
                CurrentSegmentStartPoint := frmGoPhast.Grid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentStartPoint);
              end;
              if NearlyTheSame(CurrentSegmentStartPoint.X, Point1.X)
                and NearlyTheSame(CurrentSegmentStartPoint.Y, Point1.Y) then
              begin
                result := AValue;
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GetVertexIndiciesInCurrentCell(var FirstVertex, LastVertex: Integer;
  const CallingFunctionName: string; ShowWarning: Boolean): Boolean;
var
  SegmentPosition: Integer;
  Segments: TCellElementSegmentList;
  StartIndex: Integer;
  SegmentIndex: Integer;
  ASegment: TCellElementSegment;
  StopIndex: Integer;
  VertexIndex: Integer;
  APoint2D: TPoint2D;
  LocalGrid: TCustomModelGrid;
//  LocalMesh: TSutraMesh3D;
  ATopCell2D: T2DTopCell;
  LocalMesh: IMesh3D;
begin
  Result := True;

  if GlobalCurrentScreenObject.GettingSegments[GlobalCurrentModel]
    and (GlobalCurrentScreenObject.ViewDirection = vdTop) then
  begin
    LocalGrid := (GlobalCurrentModel as TCustomModel).Grid;
    LocalMesh := (GlobalCurrentModel as TCustomModel).Mesh3D;
    FirstVertex := GlobalCurrentSegment.VertexIndex;
    if FirstVertex < GlobalCurrentScreenObject.
      SectionEnd[GlobalCurrentSegment.SectionIndex] then
    begin
      Inc(FirstVertex);
    end;
    for VertexIndex := FirstVertex - 1 downto
      GlobalCurrentScreenObject.SectionStart[GlobalCurrentSegment.SectionIndex] do
    begin
      APoint2D := GlobalCurrentScreenObject.Points[VertexIndex];
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
              ATopCell2D := LocalGrid.TopContainingCell(APoint2D,
                GlobalCurrentScreenObject.EvaluatedAt, True);
            end
            else
            begin
              Assert(LocalMesh <> nil);
              ATopCell2D := LocalMesh.TopContainingCellOrElement(APoint2D,
                GlobalCurrentScreenObject.EvaluatedAt)
            end;
            if (ATopCell2D.Col = GlobalCurrentSegment.Col)
              and (ATopCell2D.Row = GlobalCurrentSegment.Row) then
            begin
              FirstVertex := VertexIndex;
            end
            else
            begin
              break;
            end;
          end;
        vdFront:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
//              GlobalCurrentScreenObject.GetACol(LocalGrid, APoint2D.x);

              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end
            else
            begin
              Assert(LocalMesh <> nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end;
          end;
        vdSide:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end
            else
            begin
              Assert(LocalMesh <> nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end;
          end;
        else
          Assert(False);
      end;
    end;

    LastVertex := GlobalCurrentSegment.VertexIndex;
    if LastVertex > GlobalCurrentScreenObject.
      SectionStart[GlobalCurrentSegment.SectionIndex] then
    begin
      Dec(LastVertex);
    end;
    if LastVertex < FirstVertex then
    begin
      LastVertex := FirstVertex;
    end;
    for VertexIndex := LastVertex + 1 to
      GlobalCurrentScreenObject.SectionEnd[GlobalCurrentSegment.SectionIndex] do
    begin
      APoint2D := GlobalCurrentScreenObject.Points[VertexIndex];
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
              ATopCell2D := LocalGrid.TopContainingCell(APoint2D,
                GlobalCurrentScreenObject.EvaluatedAt, True);
            end
            else
            begin
              Assert(LocalMesh <> nil);
              ATopCell2D := LocalMesh.TopContainingCellOrElement(APoint2D,
                GlobalCurrentScreenObject.EvaluatedAt)
            end;
            if (ATopCell2D.Col = GlobalCurrentSegment.Col)
              and (ATopCell2D.Row = GlobalCurrentSegment.Row) then
            begin
              LastVertex := VertexIndex;
            end
            else
            begin
              break;
            end;
          end;
        vdFront:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
//              FScreenObject.GetACol(LocalGrid, APoint2D.x);
//              LocalGrid.
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end
            else
            begin
              Assert(LocalMesh <> nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end;
          end;
        vdSide:
          begin
            if LocalGrid <> nil then
            begin
              Assert(LocalMesh = nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end
            else
            begin
              Assert(LocalMesh <> nil);
              case GlobalCurrentScreenObject.EvaluatedAt of
                eaBlocks:
                  begin

                  end;
                eaNodes:
                  begin

                  end;
                else
                  Assert(False);
              end;
            end;
          end;
        else
          Assert(False);
      end;
    end
  end
  else if GlobalCurrentScreenObject.GettingSegments[GlobalCurrentModel] then
  begin
    FirstVertex := GlobalCurrentSegment.VertexIndex;
    LastVertex := FirstVertex;
    frmErrorsAndWarnings.AddError(GlobalCurrentModel, StrInvalidFormulaFor,
      GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
  end
  else
  begin
    SegmentPosition := GlobalCurrentSegment.PositionInSegmentList;
    Segments := GlobalCurrentScreenObject.Segments[GlobalCurrentModel];
    StartIndex := SegmentPosition;
    for SegmentIndex := SegmentPosition -1 downto 0 do
    begin
      ASegment := Segments[SegmentIndex];
      if (ASegment.Col = GlobalCurrentSegment.Col)
        and (ASegment.Row = GlobalCurrentSegment.Row)
        and (ASegment.Layer = GlobalCurrentSegment.Layer)
        and (ASegment.SectionIndex = GlobalCurrentSegment.SectionIndex)
        and ((ASegment.StartPosition = epFirst) or (ASegment.EndPosition = epLast))
        then
      begin
        StartIndex := SegmentIndex;
      end
      else
      begin
        if GlobalCurrentModel.ModelSelection in SutraSelection then
        begin
          if ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex then
          begin
            break;
          end;
          Continue;
        end;
        case GlobalCurrentScreenObject.ViewDirection of
          vdTop:
            begin
              if (ASegment.Col <> GlobalCurrentSegment.Col)
                or (ASegment.Row <> GlobalCurrentSegment.Row)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
          vdFront:
            begin
              if (ASegment.Col <> GlobalCurrentSegment.Col)
                or (ASegment.Layer <> GlobalCurrentSegment.Layer)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
          vdSide:
            begin
              if (ASegment.Row <> GlobalCurrentSegment.Row)
                or (ASegment.Layer <> GlobalCurrentSegment.Layer)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
        end;
      end;
    end;

    StopIndex := SegmentPosition;
    for SegmentIndex := SegmentPosition +1 to Segments.Count-1 do
    begin
      ASegment := Segments[SegmentIndex];
      if (ASegment.Col = GlobalCurrentSegment.Col)
        and (ASegment.Row = GlobalCurrentSegment.Row)
        and (ASegment.Layer = GlobalCurrentSegment.Layer)
        and (ASegment.SectionIndex = GlobalCurrentSegment.SectionIndex)
        and ((ASegment.StartPosition = epFirst) or (ASegment.EndPosition = epLast))
        then
      begin
        StopIndex := SegmentIndex;
      end
      else
      begin
        case GlobalCurrentScreenObject.ViewDirection of
          vdTop:
            begin
              if (ASegment.Col <> GlobalCurrentSegment.Col)
                or (ASegment.Row <> GlobalCurrentSegment.Row)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
          vdFront:
            begin
              if (ASegment.Col <> GlobalCurrentSegment.Col)
                or (ASegment.Layer <> GlobalCurrentSegment.Layer)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
          vdSide:
            begin
              if (ASegment.Row <> GlobalCurrentSegment.Row)
                or (ASegment.Layer <> GlobalCurrentSegment.Layer)
                or (ASegment.SectionIndex <> GlobalCurrentSegment.SectionIndex) then
              begin
                Break;
              end
            end;
        end;
      end;
    end;


    FirstVertex := -1;
    ASegment := Segments[StartIndex];
    if ASegment.StartPosition = epFirst then
    begin
      FirstVertex := ASegment.VertexIndex;
    end
    else if ASegment.EndPosition = epLast then
    begin
      FirstVertex := ASegment.VertexIndex+1;
    end;

    LastVertex := -1;
    ASegment := Segments[StopIndex];
    if ASegment.EndPosition <> epMiddle then
    begin
      LastVertex := Segments[StopIndex].VertexIndex+1;
      if LastVertex >= GlobalCurrentScreenObject.Count then
      begin
        LastVertex := GlobalCurrentScreenObject.Count-1;
      end;
    end
    else if ASegment.StartPosition <> epMiddle then
    begin
      LastVertex := Segments[StopIndex].VertexIndex;
    end
    else if LastVertex < 0 then
    begin
      LastVertex := FirstVertex;
    end;

    if (FirstVertex < 0) then
    begin
      if ShowWarning then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format(StrVertexValueNotFoundError, [CallingFunctionName]),
          Format(StrGisObjectCellError,
          [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
          GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
          GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end;
      Result := False;
      Exit;
    end;
  end;

  if (FirstVertex < 0) or (LastVertex < 0) then
  begin
    Beep;
  end;
  Assert(FirstVertex >= 0);
  Assert(LastVertex >= 0);
  Assert(FirstVertex < GlobalCurrentScreenObject.Count);
  Assert(LastVertex < GlobalCurrentScreenObject.Count);
  Assert(FirstVertex <= LastVertex);
end;

function TestVertexValuesCanBeTested(const FunctionName, VertexValueName: string): Boolean;
begin
  result := True;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.PointPositionValues = nil) then
  begin
    result := False;
    if  (GlobalCurrentScreenObject = nil) then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format(StrTheSFunctionCan, [FunctionName]), '');
      frmErrorsAndWarnings.Show;
    end
    else if (GlobalCurrentSegment = nil) then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,Format(
        StrTheSFunctionDoes, [FunctionName]),
        GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format(StrThe0sFunctionIs, [FunctionName]),
        Format(StrObject0sInvali, [GlobalCurrentScreenObject.Name,
        VertexValueName]), GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

function _LowestVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  FoundFirst: Boolean;
  DefaultValue: Double;
  DefaultValueAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultValueAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultValueAssigned := true;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrLowestVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrLowestVertexValue, not DefaultValueAssigned) then
    begin
      Exit;
    end;

    FoundFirst := False;
    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      if Item.Position > StopVertex then
      begin
        Break;
      end;
      if (StartVertex <= Item.Position) then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          AValue := Item.Value[VVIndex];
          if FoundFirst then
          begin
            if AValue < result then
            begin
              result := AValue;
            end;
          end
          else
          begin
            result := AValue;
            FoundFirst := True;
          end;
        end;
      end;
    end;
    if not FoundFirst then
    begin
      if not DefaultValueAssigned then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format( StrNoDefinedVertexVa, [VertexValueName, StrLowestVertexValue]),
          Format(StrGisObjectCellError,
          [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
          GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
          GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end;
    end;
  end;
end;

function _HighestVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  FoundFirst: Boolean;
  DefaultValue: Double;
  DefaultValueAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultValueAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultValueAssigned := True;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrHighestVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrHighestVertexValue, not DefaultValueAssigned) then
    begin
      Exit;
    end;

    FoundFirst := False;
    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      if Item.Position > StopVertex then
      begin
        Break;
      end;
      if (StartVertex <= Item.Position) then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          AValue := Item.Value[VVIndex];
          if FoundFirst then
          begin
            if AValue > result then
            begin
              result := AValue;
            end;
          end
          else
          begin
            result := AValue;
            FoundFirst := True;
          end;
        end;
      end;
    end;
    if not FoundFirst then
    begin
      if not DefaultValueAssigned then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format( StrNoDefinedVertexVa, [VertexValueName, StrHighestVertexValue]),
          Format(StrGisObjectCellError,
          [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
          GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
          GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end;
    end;
  end;
end;

function _MeanVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  Sum: double;
  Count: integer;
  DefaultValue: double;
  DefaultAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultAssigned := True;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrMeanVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrMeanVertexValue, not DefaultAssigned) then
    begin
      Exit;
    end;

    Sum := 0;
    Count := 0;
    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      if Item.Position > StopVertex then
      begin
        Break;
      end;
      if (StartVertex <= Item.Position) then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          AValue := Item.Value[VVIndex];
          Sum := Sum + AValue;
          Inc(Count);
        end;
      end;
    end;
    if Count = 0 then
    begin
      if not DefaultAssigned then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format( StrNoDefinedVertexVa, [VertexValueName, StrMeanVertexValue]),
          Format(StrGisObjectCellError,
          [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
          GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
          GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end;
    end
    else
    begin
      result := Sum/Count;
    end;
  end;
end;

//function _VertexValueSlope(Values: array of pointer): double;
//var
//  VertexValueName: string;
//  LocalPPV: TPointPositionValues;
////  VertexIndex: integer;
//  DefaultValue: double;
//  StartVertex: integer;
//  StopVertex: integer;
//  Item: TPointValuesItem;
//  VVIndex: Integer;
//  StartIndex: Integer;
//  StartValue: Double;
//  FoundStart: Boolean;
//  FoundStop: Boolean;
//  StopIndex: Integer;
//  StopValue: Double;
//  APoint: TPoint2D;
//  NextPoint: TPoint2D;
//  Index: Integer;
//  PointIndex: Integer;
//begin
//  Assert(Length(Values) >= 1);
//  VertexValueName := PString(Values[0])^;
//  if (Length(Values) > 1) and (Values[1] <> nil) then
//  begin
//    DefaultValue := PDouble(Values[1])^
//  end
//  else
//  begin
//    DefaultValue := 0;
//  end;
//
//  result := DefaultValue;
//  if TestVertexValuesCanBeTested(StrVertexValueSlope, VertexValueName) then
//  begin
//    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
//      StrVertexValueSlope) then
//    begin
//      Exit;
//    end;
//    if StartVertex = StopVertex then
//    begin
//      if StartVertex > 0 then
//      begin
//        Dec(StartVertex);
//      end;
//      if StopVertex < GlobalCurrentScreenObject.Count -1 then
//      begin
//        Inc(StopVertex);
//      end;
//    end;
//    if StartVertex = StopVertex then
//    begin
//      Exit;
//    end;
//
//    StartIndex := -1;
//    StartValue := 0;
//    FoundStart := False;
//    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;
//    for Index := 0 to LocalPPV.Count - 1 do
//    begin
//      Item := LocalPPV.Items[Index] as TPointValuesItem;
//      if (Item.Position >= StartVertex) and FoundStart then
//      begin
//        break;
//      end;
//      VVIndex := Item.IndexOfName(VertexValueName);
//      if VVIndex >= 0 then
//      begin
//        StartIndex := Index;
//        StartValue := Item.Value[VVIndex];
//        FoundStart := True;
//      end;
//    end;
//
//    StopIndex := -1;
//    StopValue := 0;
//    FoundStop := False;
//    for Index := LocalPPV.Count - 1 downto 0 do
//    begin
//      Item := LocalPPV.Items[Index] as TPointValuesItem;
//      if (Item.Position <= StopVertex) and FoundStop then
//      begin
//        break;
//      end;
//      VVIndex := Item.IndexOfName(VertexValueName);
//      if VVIndex >= 0 then
//      begin
//        StopIndex := Index;
//        StopValue := Item.Value[VVIndex];
//        FoundStop := True;
//      end;
//    end;
//
//    if StartIndex < StopIndex then
//    begin
//      result := 0;
//      APoint := GlobalCurrentScreenObject.Points[StartIndex];
//      for PointIndex := StartIndex+1 to StopIndex do
//      begin
//        NextPoint := GlobalCurrentScreenObject.Points[PointIndex];
//        result := Result + Distance(APoint,NextPoint);
//        APoint := NextPoint
//      end;
//      if result <> 0 then
//      begin
//        result := (StartValue-StopValue)/result;
//      end;
//    end;
//  end;
//end;

function _MedianVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  ValueList: TRealList;
  DefaultValue: Double;
  DefaultAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultAssigned := True;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrMedianVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrMedianVertexValue, not DefaultAssigned) then
    begin
      Exit;
    end;

    ValueList := TRealList.Create;
    try
      for Index := 0 to LocalPPV.Count - 1 do
      begin
        Item := LocalPPV.Items[Index] as TPointValuesItem;
        if Item.Position > StopVertex then
        begin
          Break;
        end;
        if (StartVertex <= Item.Position) then
        begin
          VVIndex := Item.IndexOfName(VertexValueName);
          if VVIndex >= 0 then
          begin
            AValue := Item.Value[VVIndex];
            ValueList.Add(AValue);
          end;
        end;
      end;
      if ValueList.Count = 0 then
      begin
        if not DefaultAssigned then
        begin
          frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
            Format( StrNoDefinedVertexVa, [VertexValueName, StrMedianVertexValue]),
            Format(StrGisObjectCellError,
            [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
            GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
            GlobalCurrentScreenObject);
          frmErrorsAndWarnings.Show;
        end;
      end
      else
      begin
        ValueList.Sort;
        if Odd(ValueList.Count) then
        begin
          result := ValueList[ValueList.Count div 2];
        end
        else
        begin
          result := (ValueList[ValueList.Count div 2] + ValueList[(ValueList.Count div 2) - 1])/2;
        end;
      end;
    finally
      ValueList.Free
    end;
  end;
end;

function _FirstVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
//  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  DefaultValue: double;
  DefaultAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultAssigned := True;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrFirstVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrFirstVertexValue, not DefaultAssigned) then
    begin
      Exit;
    end;

    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      if Item.Position > StopVertex then
      begin
        Break;
      end;
      if (StartVertex <= Item.Position) then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          result := Item.Value[VVIndex];
          Exit
        end;
      end;
    end;
    if not DefaultAssigned then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format( StrNoDefinedVertexVa, [VertexValueName, StrFirstVertexValue]),
        Format(StrGisObjectCellError,
        [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
        GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
        GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

function _LastVertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
//  AValue: Double;
  StartVertex: Integer;
  StopVertex: Integer;
  DefaultValue: double;
  DefaultAssigned: Boolean;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  DefaultAssigned := False;
  if (Length(Values) > 1) and (Values[1] <> nil) then
  begin
    DefaultValue := PDouble(Values[1])^;
    DefaultAssigned := True;
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if TestVertexValuesCanBeTested(StrLastVertexValue, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    if not GetVertexIndiciesInCurrentCell(StartVertex, StopVertex,
      StrLastVertexValue, not DefaultAssigned) then
    begin
      Exit;
    end;

    for Index := LocalPPV.Count - 1 downto 0 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      if Item.Position < StartVertex then
      begin
        Break;
      end;
      if (StopVertex >= Item.Position) then
      begin
        VVIndex := Item.IndexOfName(VertexValueName);
        if VVIndex >= 0 then
        begin
          result := Item.Value[VVIndex];
          Exit
        end;
      end;
    end;
    if not DefaultAssigned then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format( StrNoDefinedVertexVa, [VertexValueName, StrLastVertexValue]),
        Format(StrGisObjectCellError,
        [GlobalCurrentScreenObject.Name, GlobalCurrentSegment.Layer+1,
        GlobalCurrentSegment.Row + 1, GlobalCurrentSegment.Col+1]),
        GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

function _InterpolatedVertexValues(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
//  FirstIndex: Integer;
//  MidIndex: Integer;
//  LastIndex: Integer;
  Item: TPointValuesItem;
//  FirstItem: TPointValuesItem;
//  MidItem: TPointValuesItem;
//  LastItem: TPointValuesItem;
  VVIndex: Integer;
//  FirstVVIndex: Integer;
//  MidVVIndex: Integer;
//  LastVVIndex: Integer;
  AValue: Double;
  BeforeValue: Double;
  AfterValue: Double;
  BeforePosition: Integer;
  AfterPosition: Integer;
  Distance1: Double;
  Distance2: Double;
  Point1, Point2: TPoint2D;
  SegmentDistance: double;
  CurrentSegmentStartPoint: TPoint2D;
  CurrentSegmentEndPoint: TPoint2D;
  LocalEpsilon: double;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < LocalEpsilon;
    end;
  end;
begin
  Assert(Length(Values) >= 1);
  VertexValueName := PString(Values[0])^;
  result := 0;
  if TestVertexValuesCanBeTested(StrInterpolatedVertexValues, VertexValueName) then
  begin
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    BeforeValue := 0;
    BeforePosition := -1;
    AfterValue := 0;
    AfterPosition := -1;

    {
    FirstIndex := 0;
    LastIndex := LocalPPV.Count - 1;
    While LastIndex - FirstIndex > 1 do
    begin
      for Index := FirstIndex to LastIndex do
      begin
        FirstItem := LocalPPV.Items[Index] as TPointValuesItem;
        FirstVVIndex := FirstItem.IndexOfName(VertexValueName);
        if FirstVVIndex >= 0 then
        begin
          FirstIndex := Index;
          break;
        end;
      end;
      for Index := LastIndex downto FirstIndex do
      begin
        LastItem := LocalPPV.Items[Index] as TPointValuesItem;
        LastVVIndex := LastItem.IndexOfName(VertexValueName);
        if LastVVIndex >= 0 then
        begin
          LastIndex := Index;
          break;
        end;
      end;
      MidIndex := (FirstIndex + LastIndex) div 2;
      for Index := MidIndex downto FirstIndex do
      begin
        MidItem := LocalPPV.Items[Index] as TPointValuesItem;
        MidVVIndex := MidItem.IndexOfName(VertexValueName);
        if MidVVIndex >= 0 then
        begin
          MidIndex := Index;
          break;
        end;
      end;
      if MidIndex = FirstIndex then
      begin
        MidIndex := (FirstIndex + LastIndex) div 2;
        for Index := MidIndex to LastIndex do
        begin
          MidItem := LocalPPV.Items[Index] as TPointValuesItem;
          MidVVIndex := MidItem.IndexOfName(VertexValueName);
          if MidVVIndex >= 0 then
          begin
            MidIndex := Index;
            break;
          end;
        end;
      end;
      if MidIndex = LastIndex then
      begin
      end;
    end;
    }
    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      VVIndex := Item.IndexOfName(VertexValueName);
      if VVIndex >= 0 then
      begin
        AValue := Item.Value[VVIndex];
        if Item.Position <= GlobalCurrentSegment.VertexIndex then
        begin
          BeforeValue := AValue;
          BeforePosition := Item.Position;
        end
        else
        begin
          AfterValue := AValue;
          AfterPosition := Item.Position;
          break;
        end;
      end;
    end;

    if BeforePosition = -1 then
    begin
      if AfterPosition = -1 then
      begin
        result := 0;
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format(StrInvalidKeyIn0s, [StrInterpolatedVertexValues]),
          Format(StrObject0sInvali, [GlobalCurrentScreenObject.Name,
          VertexValueName]), GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end
      else
      begin
        result := AfterValue;
      end;
    end
    else
    begin
      LocalEpsilon := GlobalCurrentSegment.SegmentLength/100000;
      if AfterPosition = -1 then
      begin
        result := BeforeValue;
      end
      else
      begin
        Point1 := GlobalCurrentScreenObject.Points[
          GlobalCurrentSegment.VertexIndex];
        CurrentSegmentStartPoint.X := GlobalCurrentSegment.X1;
        CurrentSegmentStartPoint.Y := GlobalCurrentSegment.Y1;
        if frmGoPhast.Grid <> nil then
        begin
          CurrentSegmentStartPoint := frmGoPhast.Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentStartPoint);
        end;
        if (BeforePosition = GlobalCurrentSegment.VertexIndex)
          and NearlyTheSame(CurrentSegmentStartPoint.X, Point1.X)
          and NearlyTheSame(CurrentSegmentStartPoint.Y, Point1.Y) then
        begin
          result := BeforeValue;
          Exit;
        end;

        Point2 := GlobalCurrentScreenObject.Points[
          GlobalCurrentSegment.VertexIndex+1];
        CurrentSegmentEndPoint.X := GlobalCurrentSegment.X2;
        CurrentSegmentEndPoint.Y := GlobalCurrentSegment.Y2;
        if frmGoPhast.Grid <> nil then
        begin
          CurrentSegmentEndPoint := frmGoPhast.Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentEndPoint);
        end;
        if (AfterPosition = GlobalCurrentSegment.VertexIndex+1)
          and NearlyTheSame(CurrentSegmentEndPoint.X, Point2.X)
          and NearlyTheSame(CurrentSegmentEndPoint.Y, Point2.Y) then
        begin
           result := AfterValue;
           Exit;
        end;

        Distance1 := NodeDistances(BeforePosition);
        Distance2 := NodeDistances(AfterPosition);
        if Distance1 = Distance2 then
        begin
          result := (BeforeValue + AfterValue)/2;
        end
        else
        begin
          Point2.X := (CurrentSegmentStartPoint.X + CurrentSegmentEndPoint.X) / 2;
          Point2.Y := (CurrentSegmentStartPoint.Y + CurrentSegmentEndPoint.Y) / 2;
          SegmentDistance := (Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y))
            + NodeDistances(GlobalCurrentSegment.VertexIndex));
          result := (SegmentDistance - Distance1)/(Distance2 - Distance1)
            * (AfterValue - BeforeValue) + BeforeValue;
        end;
      end;
    end;
  end;
end;

function _FractionOfObjectLength(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
  LocalModel: TCustomModel;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    result := 0;
  end
  else
  begin
    LocalModel := GlobalCurrentModel as TCustomModel;
    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
    if (LocalModel.Grid <> nil) and (LocalModel.Grid.GridAngle <> 0) then
    begin
      Point1 := LocalModel.Grid.RotateFromRealWorldCoordinatesToGridCoordinates(Point1);
    end;
    if (GlobalCurrentSegment.VertexIndex = 0)
      and (GlobalCurrentSegment.X1 = Point1.X)
      and (GlobalCurrentSegment.Y1 = Point1.Y) then
    begin
      Point2 := GlobalCurrentScreenObject.Points[
        GlobalCurrentScreenObject.Count - 1];
      if (LocalModel.Grid <> nil) and (LocalModel.Grid.GridAngle <> 0) then
      begin
        Point2 := LocalModel.Grid.RotateFromRealWorldCoordinatesToGridCoordinates(Point2);
      end;
      if (GlobalCurrentSegment.X2 = Point2.X)
        and (GlobalCurrentSegment.Y2 = Point2.Y) then
      begin
        result := 0.5;
      end
      else
      begin
        result := 0;
      end;
      Exit;
    end
    else if (GlobalCurrentSegment.VertexIndex =
      GlobalCurrentScreenObject.Count - 2) then
    begin
      Point2 := GlobalCurrentScreenObject.Points[
        GlobalCurrentScreenObject.Count - 1];
      if (LocalModel.Grid <> nil) and (LocalModel.Grid.GridAngle <> 0) then
      begin
        Point2 := LocalModel.Grid.RotateFromRealWorldCoordinatesToGridCoordinates(Point2);
      end;
      if (GlobalCurrentSegment.X2 = Point2.X)
        and (GlobalCurrentSegment.Y2 = Point2.Y) then
      begin
        result := 1;
        Exit;
      end;
    end;
    if GlobalCurrentSegment.EndPosition = epLast then
    begin
      Point2.X := GlobalCurrentSegment.X2;
      Point2.Y := GlobalCurrentSegment.Y2;
    end
    else
    begin
      Point2.X := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
      Point2.Y := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
    end;

    result := (Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y))
      + NodeDistances(GlobalCurrentSegment.VertexIndex))
      / GlobalCurrentScreenObject.ScreenObjectLength;
  end;
end;

function _ObjectNodeCount(Values: array of pointer): integer;
begin
  result := 0;
  if (GlobalCurrentScreenObject <> nil) then
  begin
    result := GlobalCurrentScreenObject.Count
  end;
end;

function _X(Values: array of pointer): double;
begin
  Result := GlobalX;
end;

function _Y(Values: array of pointer): double;
begin
  Result := GlobalY;
end;

function _Z(Values: array of pointer): double;
begin
  Result := GlobalZ;
end;

function RotatedSutraLocation: TPoint2D;
var
  Temp: TPoint2D;
  SutraAngle: Double;
begin
    Result.X := GlobalX;
    Result.Y := GlobalY;
    SutraAngle := GlobalCurrentScreenObject.SutraAngle;
    temp.X := Cos(-SutraAngle) * Result.X - Sin(-SutraAngle) * Result.Y;
    temp.Y := Sin(-SutraAngle) * Result.X + Cos(-SutraAngle) * Result.Y;
    Result := temp;
end;

function _XPrime(Values: array of pointer): double;
begin
  if ((frmGoPhast.ModelSelection in SutraSelection) or frmGoPhast.DisvUsed)
    and (GlobalCurrentScreenObject <> nil)
    and (GlobalCurrentScreenObject.ViewDirection = vdFront)
    and (GlobalCurrentScreenObject.SutraAngle <> 0)
    then
  begin
    result := RotatedSutraLocation.X;
  end
  else
  begin
    Result := GlobalXPrime;
  end;
end;

function _YPrime(Values: array of pointer): double;
begin
  if ((frmGoPhast.ModelSelection in SutraSelection) or frmGoPhast.DisvUsed)
    and (GlobalCurrentScreenObject <> nil)
    and (GlobalCurrentScreenObject.ViewDirection = vdFront)
    and (GlobalCurrentScreenObject.SutraAngle <> 0)
    then
  begin
    result := RotatedSutraLocation.Y;
  end
  else
  begin
    Result := GlobalYPrime;
  end;
end;

function _Column(Values: array of pointer): integer;
begin
  Result := GlobalColumn;
end;

function _Row(Values: array of pointer): integer;
begin
  Result := GlobalRow;
end;

function _ColumnCenter(Values: array of pointer): double;
var
  Col: Integer;
  LocalGrid : TCustomModelGrid;
begin
  if Values[0] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Col := GlobalColumn - 1;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if (Col < 0) or (LocalGrid = nil) or (Col > LocalGrid.ColumnCount-1) then
  begin
    result := 0;
  end
  else
  begin
    result := LocalGrid.ColumnCenter(Col);
  end;
end;

function _RowCenter(Values: array of pointer): double;
var
  Row: Integer;
  LocalGrid : TCustomModelGrid;
begin
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if (Row < 0) or (LocalGrid = nil) or (Row > LocalGrid.RowCount-1) then
  begin
    result := 0;
  end
  else
  begin
    result := LocalGrid.RowCenter(Row);
  end;
end;

function _ElevationToLayer(Values: array of pointer): integer;
var
  Elevation: double;
  Mesh: TSutraMesh3D;
  LayerIndex: Integer;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
begin
  result := -1;
  Elevation := PDouble(Values[0])^;
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := frmGoPhast.PhastModel.PhastGrid.
                GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation)
                +1;
            end;
          eaNodes:
            begin
              if Elevation < frmGoPhast.PhastModel.
                PhastGrid.LowestElevation then
              begin
                result := 0;
              end
              else if Elevation > frmGoPhast.PhastModel.
                PhastGrid.HighestElevation then
              begin
                result := frmGoPhast.PhastModel.PhastGrid.LayerCount + 2
              end
              else
              begin
                result := frmGoPhast.PhastModel.
                  PhastGrid.NearestLayerCenter(Elevation)+1;
              end;
            end;
          else Assert(False);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        Assert(GlobalEvaluatedAt = eaBlocks);
        if not TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          result := TCustomModel(GlobalCurrentModel).ModflowGrid.
            GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation);
        end
        else
        begin
          result := TCustomModel(GlobalCurrentModel).DisvGrid.
            GetContainingLayer(GlobalColumn-1, Elevation);
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := TCustomModel(GlobalCurrentModel).Mesh as TSutraMesh3D;
        Result := 0;
        if (Mesh = nil) or (Mesh.MeshType in [mt2D, mtProfile]) then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              for LayerIndex := 0 to Mesh.LayerCount - 1 do
              begin
                Element:= Mesh.ElementArray[LayerIndex, GlobalColumn-1];
                if Element.Active
                  and (Elevation <= Element.UpperElevation)
                  and (Elevation >= Element.LowerElevation) then
                begin
                  result := LayerIndex+1;
                  Exit;
                end;
              end;
            end;
          eaNodes:
            begin
              for LayerIndex := 0 to Mesh.LayerCount do
              begin
                Node := Mesh.NodeArray[LayerIndex, GlobalColumn-1];
                if Node.Active
                  and (Elevation <= Node.Top)
                  and (Elevation >= Node.Bottom) then
                begin
                  result := LayerIndex+1;
                  Exit;
                end;
              end;
            end;
          else
            Assert(False);
        end;
      end;
    msFootPrint:
      begin
        Result := 0;
      end
    else Assert(False);
  end;
  Inc(result);
end;

function _ElevationToModelLayer(Values: array of pointer): integer;
var
  Elevation: double;
  GroupIndex: Integer;
  NonSimulatedUnits: Integer;
  LayerCount: Integer;
  Group: TLayerGroup;
  ChildModel: TChildModel;
  DisIndex: Integer;
  DisItem: TChildDiscretization;
  PriorGroup: TLayerGroup;
begin
  result := -1;
  Elevation := PDouble(Values[0])^;
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := frmGoPhast.PhastModel.PhastGrid.
                GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation)
                +1;
            end;
          eaNodes:
            begin
              if Elevation < frmGoPhast.PhastModel.
                PhastGrid.LowestElevation then
              begin
                result := 0;
              end
              else if Elevation > frmGoPhast.PhastModel.
                PhastGrid.HighestElevation then
              begin
                result := frmGoPhast.PhastModel.PhastGrid.LayerCount + 2
              end
              else
              begin
                result := frmGoPhast.PhastModel.
                  PhastGrid.NearestLayerCenter(Elevation);
//                result := frmGoPhast.PhastModel.LayerStructure.
//                  DataSetLayerToModflowLayer(result);
              end;
            end;
          else Assert(False);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          Result := _ElevationToLayer(Values);
          Exit;
        end;
        Assert(GlobalEvaluatedAt = eaBlocks);

        result := TCustomModel(GlobalCurrentModel).ModflowGrid.
          GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation);
        if GlobalCurrentModel is TChildModel then
        begin
          ChildModel := TChildModel(GlobalCurrentModel);
          if result >= ChildModel.Grid.LayerCount then
          begin
            result := ChildModel.Grid.LayerCount-1
          end;
          NonSimulatedUnits := 0;
          LayerCount:= 0;
          PriorGroup := nil;
          for DisIndex := 0 to ChildModel.Discretization.count - 1 do
          begin
            DisItem := ChildModel.Discretization[DisIndex];
            if DisItem.LayerGroup.Simulated then
            begin
              LayerCount := LayerCount + DisItem.Discretization;
              if LayerCount > result then
              begin
                break;
              end;
            end
            else
            begin
              Group := DisItem.LayerGroup;
              if Group <> PriorGroup then
              begin
                Inc(NonSimulatedUnits);
                Inc(LayerCount);
              end;
              PriorGroup := Group;
            end;
          end;
          result := result - NonSimulatedUnits + 1;
        end
        else
        begin
          NonSimulatedUnits := 0;
          LayerCount:= 0;
          for GroupIndex := 1 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
          begin
            Group := TCustomModel(GlobalCurrentModel).LayerStructure[GroupIndex];
            if Group.RunTimeSimulated then
            begin
              LayerCount := LayerCount + Group.LayerCount;
            end
            else
            begin
              Inc(NonSimulatedUnits);
              Inc(LayerCount);
            end;
            if LayerCount > result then
            begin
              break;
            end;
          end;
          result := result - NonSimulatedUnits + 1;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Result := _ElevationToLayer(Values);
      end;
    msFootPrint:
      begin
        result := 0;
      end
    else Assert(False);
  end;
end;

function _Layer(Values: array of pointer): integer;
begin
  Result := GlobalLayer;
end;

function GetColumnWidth(Column: Integer): Double;
var
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    result := 0;
    Exit;
  end;
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Column < 0) or (Column >= LocalGrid.ColumnCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := LocalGrid.ColumnWidth[Column];
        end;
      end;
    eaNodes:
      begin
        if (Column < 0) or (Column > frmGoPhast.PhastGrid.ColumnCount) then
        begin
          Result := 0;
        end
        else
        begin
          if (Column = 0) then
          begin
            result := frmGoPhast.PhastGrid.ColumnWidth[Column] / 2;
          end
          else if (Column = frmGoPhast.PhastGrid.ColumnCount) then
          begin
            result := frmGoPhast.PhastGrid.ColumnWidth[Column - 1] / 2;
          end
          else
          begin
            result := (frmGoPhast.PhastGrid.ColumnPosition[Column + 1]
              - frmGoPhast.PhastGrid.ColumnPosition[Column - 1]) / 2;
          end;
        end;
      end;
  else
    begin
      Assert(False);
      result := 0;
    end;
  end;
end;

function _ColumnWidth(Values: array of pointer): double;
var
  Column: integer;
begin
  if Values[0] <> nil then
  begin
    Column := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
  Result := GetColumnWidth(Column);
end;

function GetRowWidth(Row: Integer): Double;
var
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    result := 0;
    Exit;
  end;
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Row < 0) or (Row >= LocalGrid.RowCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := LocalGrid.RowWidth[Row];
        end;
      end;
    eaNodes:
      begin
        if (Row < 0) or (Row > frmGoPhast.PhastGrid.RowCount) then
        begin
          Result := 0;
        end
        else
        begin
          if (Row = 0) then
          begin
            result := frmGoPhast.PhastGrid.RowWidth[Row] / 2;
          end
          else if (Row = frmGoPhast.PhastGrid.RowCount) then
          begin
            result := frmGoPhast.PhastGrid.RowWidth[Row - 1] / 2;
          end
          else
          begin
            result := (frmGoPhast.PhastGrid.RowPosition[Row + 1]
              - frmGoPhast.PhastGrid.RowPosition[Row - 1]) / 2;
          end;
        end;
      end;
  else
    begin
      Assert(False);
      result := 0;
    end;
  end;
end;

function _RowWidth(Values: array of pointer): double;
var
  Row: integer;
begin
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  Result := GetRowWidth(Row);
end;

function _SimulatedModflowLayer(Values: array of pointer): boolean;
var
  Layer: integer;
begin
  Result := True;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if (GlobalCurrentModel as TCustomModel).DisvUsed then
  begin
    Exit;
  end;
  if Values[0] <> nil then
  begin
    Layer := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  result := (GlobalCurrentModel as TCustomModel).IsLayerSimulated(Layer);
end;

function _ConfinedModflowLayer(Values: array of pointer): boolean;
var
  Layer: integer;
begin
  Result := True;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if Values[0] <> nil then
  begin
    Layer := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  result := (GlobalCurrentModel as TCustomModel).IsLayerConfined(Layer);
end;

procedure ExtractColRowLayer(var Lay, Row, Col: Integer;
  Values: array of Pointer);
begin
  if Values[2] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
    Row := PInteger(Values[1])^ - 1;
    Lay := PInteger(Values[2])^ - 1;
  end
  else if Values[1] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
    Lay := PInteger(Values[1])^ - 1;
    Row := GlobalRow - 1;
  end
  else if Values[0] <> nil then
  begin
    Lay := PInteger(Values[0])^ - 1;
    Col := GlobalColumn - 1;
    Row := GlobalRow - 1;
  end
  else
  begin
    Lay := GlobalLayer - 1;
    Col := GlobalColumn - 1;
    Row := GlobalRow - 1;
  end;
end;

function GetLayerHeight(Col, Row, Lay: Integer): Double;
var
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
  LocalModel: TCustomModel;
  DisvGrid: TModflowDisvGrid;
  ACell: TModflowDisVCell;
begin
  result := 0;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              if (Lay < 0) or (Lay >= frmGoPhast.PhastGrid.LayerCount) then
              begin
                Result := 0;
              end
              else
              begin
                Result := frmGoPhast.PhastGrid.LayerThickness[Lay];
              end;
            end;
          eaNodes:
            begin
              if (Lay < 0) or (Lay > frmGoPhast.PhastGrid.LayerCount) then
              begin
                Result := 0;
              end
              else
              begin
                if (Lay = 0) then
                begin
                  result := frmGoPhast.PhastGrid.LayerThickness[Lay] / 2;
                end
                else if (Lay = frmGoPhast.PhastGrid.LayerCount) then
                begin
                  result := frmGoPhast.PhastGrid.LayerThickness[Lay - 1] / 2;
                end
                else
                begin
                  result := (frmGoPhast.PhastGrid.LayerElevation[Lay + 1]
                    - frmGoPhast.PhastGrid.LayerElevation[Lay - 1]) / 2;
                end;
              end;
            end;
        else
          begin
            Assert(False);
            result := 0;
          end;
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        LocalModel := TCustomModel(GlobalCurrentModel);
        if (Lay < 0) or (Lay > LocalModel.ModflowGrid.LayerCount - 1)
          or (Row < 0) or (Row > LocalModel.ModflowGrid.RowCount - 1)
          or (Col < 0) or (Col > LocalModel.ModflowGrid.ColumnCount - 1) then
        begin
          Result := 0;
        end
        else
        begin
          Result := LocalModel.ModflowGrid.CellThickness[Col, Row, Lay];
        end;
      end;
    msModflow2015:
      begin
        LocalModel := TCustomModel(GlobalCurrentModel);
        if LocalModel.DisvUsed then
        begin
          DisvGrid := LocalModel.DisvGrid;
          if (Lay < 0) or (Lay > DisvGrid.Layers.Count - 1)
            or (Row <> 0)
            or (Col < 0) or (Col > DisvGrid.TwoDGrid.ElementCount - 1) then
          begin
            Result := 0;
          end
          else
          begin
            ACell := DisvGrid.Layers[Lay].Layer[Col];
            result := ACell.Thickness;
          end;
        end
        else
        begin
          if (Lay < 0) or (Lay > LocalModel.ModflowGrid.LayerCount - 1)
            or (Row < 0) or (Row > LocalModel.ModflowGrid.RowCount - 1)
            or (Col < 0) or (Col > LocalModel.ModflowGrid.ColumnCount - 1) then
          begin
            Result := 0;
          end
          else
          begin
            Result := LocalModel.ModflowGrid.CellThickness[Col, Row, Lay];
          end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Result := 0;
        Mesh := TCustomModel(GlobalCurrentModel).Mesh as TSutraMesh3D;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              Element := Mesh.ElementArray[Lay, Col];
              Result := Element.UpperElevation - Element.LowerElevation;
            end;
          eaNodes:
            begin
              Node := Mesh.NodeArray[Lay, Col];
              Result := Node.Top - Node.Bottom;
            end;
          else
            Assert(False);
        end;
      end;
    msFootPrint:
      begin
        result := 0;
      end
  else
    Assert(False);
  end;
end;

function _LayerHeight(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  result := GetLayerHeight(Col, Row, Lay);
end;

function _BlockAreaTop(Values: array of pointer): double;
var
  Column: Integer;
  Mesh: TSutraMesh3D;
begin
  result := 0;
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint:
      begin
        result := _ColumnWidth([Values[0]]) * _RowWidth([Values[1]]);
      end;
    msModflow2015:
      begin
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          if Values[0] <> nil then
          begin
            Column := PInteger(Values[0])^ - 1;
          end
          else
          begin
            Column := GlobalColumn - 1;
          end;
          result := TCustomModel(GlobalCurrentModel).DisvGrid.CellArea(Column);
        end
        else
        begin
          result := _ColumnWidth([Values[0]]) * _RowWidth([Values[1]]);
        end;
      end;
    msSutra22, msSutra30:
      begin
        if (frmGoPhast.PhastModel <> nil)
          and (frmGoPhast.PhastModel.Mesh <> nil) then
        begin
          Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
        end
        else
        begin
          Exit;
        end;
        if Values[0] <> nil then
        begin
          Column := PInteger(Values[0])^ - 1;
        end
        else
        begin
          Column := GlobalColumn - 1;
        end;
        if Mesh.Mesh2D.Nodes.Count = 0 then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := Mesh.Mesh2D.Elements[Column].ElementArea
            end;
          eaNodes:
            begin
              result := Mesh.Mesh2D.Nodes[Column].CellArea
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end
end;

function _BlockAreaFront(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
  Mesh: TSutraMesh3D;
  Limits: TLimitsArray;
  Polygons: TCellElementPolygons2D;
  Angle: Extended;
  DisvGrid: TModflowDisvGrid;
begin
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if Values[2] = nil then
        begin
          result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[1]]);
        end
        else
        begin
          result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[2]]);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        if Values[2] = nil then
        begin
          Row := GlobalRow - 1;

          if Values[0] <> nil then
          begin
            Col := PInteger(Values[0])^ - 1;
          end
          else
          begin
            Col := GlobalColumn - 1;
          end;

          if Values[1] <> nil then
          begin
            Layer := PInteger(Values[1])^ - 1;
          end
          else
          begin
            Layer := GlobalLayer - 1;
          end;
        end
        else
        begin
          Col := PInteger(Values[0])^ - 1;
          Row := PInteger(Values[1])^ - 1;
          Layer := PInteger(Values[2])^ - 1;
        end;
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          result := 0;
          DisvGrid := TCustomModel(GlobalCurrentModel).DisvGrid;
          if DisvGrid = nil then
          begin
            Exit;
          end;
          if GlobalCurrentScreenObject = nil then
          begin
            Angle := 0.;
          end
          else
          begin
            Angle := GlobalCurrentScreenObject.SutraAngle;
          end;

          Polygons := DisvGrid.FrontPolygons(Angle, GlobalEvaluatedAt, Limits);
          result := Abs(Area(Polygons[Layer, Col]));
        end
        else
        begin
          CellPoints := TCustomModel(GlobalCurrentModel).ModflowGrid.FrontCellPoints(Row);
          SetLength(CellOutline, 6);
          CellOutline[5] := CellPoints[Col*2,Layer];
          CellOutline[4] := CellPoints[Col*2+1,Layer];
          CellOutline[3] := CellPoints[Col*2+2,Layer];
          CellOutline[2] := CellPoints[Col*2+2,Layer+1];
          CellOutline[1] := CellPoints[Col*2+1,Layer+1];
          CellOutline[0] := CellPoints[Col*2,Layer+1];
          result := Area(CellOutline);
        end;
      end;
    msSutra22, msSutra30:
      begin
        result := 0;
        Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
        if Mesh = nil then
        begin
          Exit;
        end;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        if Values[2] = nil then
        begin
//          Row := GlobalRow - 1;

          if Values[0] <> nil then
          begin
            Col := PInteger(Values[0])^ - 1;
          end
          else
          begin
            Col := GlobalColumn - 1;
          end;

          if Values[1] <> nil then
          begin
            Layer := PInteger(Values[1])^ - 1;
          end
          else
          begin
            Layer := GlobalLayer - 1;
          end;
        end
        else
        begin
          Col := PInteger(Values[0])^ - 1;
//          Row := PInteger(Values[1])^ - 1;
          Layer := PInteger(Values[2])^ - 1;
        end;

        if GlobalCurrentScreenObject = nil then
        begin
          Angle := 0.;
        end
        else
        begin
          Angle := GlobalCurrentScreenObject.SutraAngle;
        end;

        Polygons := Mesh.FrontPolygons(Angle, GlobalEvaluatedAt, Limits);
        result := Abs(Area(Polygons[Layer, Col]));

      end;
    msFootPrint:
      begin
        result := 0;
      end;
    else
      begin
        result := 0;
        Assert(False);
      end;
  end;
end;

function _BlockAreaSide(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
begin
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if Values[2] = nil then
        begin
          result := _RowWidth([Values[0]]) * _LayerHeight([Values[1]]);
        end
        else
        begin
          result := _RowWidth([Values[0]]) * _LayerHeight([Values[2]]);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          result := 0;
          Exit;
        end;
        if Values[2] = nil then
        begin
          Col := GlobalColumn - 1;

          if Values[0] <> nil then
          begin
            Row := PInteger(Values[0])^ - 1;
          end
          else
          begin
            Row := GlobalRow - 1;
          end;

          if Values[1] <> nil then
          begin
            Layer := PInteger(Values[1])^ - 1;
          end
          else
          begin
            Layer := GlobalLayer - 1;
          end;
        end
        else
        begin
          Col := PInteger(Values[0])^ - 1;
          Row := PInteger(Values[1])^ - 1;
          Layer := PInteger(Values[2])^ - 1;
        end;
        CellPoints := TCustomModel(GlobalCurrentModel).ModflowGrid.SideCellPoints(Col);
        SetLength(CellOutline, 6);
        CellOutline[0] := CellPoints[Row*2,Layer];
        CellOutline[1] := CellPoints[Row*2+1,Layer];
        CellOutline[2] := CellPoints[Row*2+2,Layer];
        CellOutline[3] := CellPoints[Row*2+2,Layer+1];
        CellOutline[4] := CellPoints[Row*2+1,Layer+1];
        CellOutline[5] := CellPoints[Row*2,Layer+1];
        result := Area(CellOutline);
      end;
    msSutra22, msSutra30:
      begin
        result := 0;
      end;
    msFootPrint:
      begin
        result := 0;
      end;
    else
      begin
        result := 0;
        Assert(False);
      end;
  end;
end;

function _BlockVolume(Values: array of pointer): double;
var
  Col: Integer;
  Row: Integer;
  Lay: Integer;
  Mesh: TSutraMesh3D;
  AnElement: TSutraElement3D;
  AnNode: TSutraNode3D;
  DisvGrid: TModflowDisvGrid;
begin
  result := 0;
  ExtractColRowLayer(Lay, Row, Col, Values);
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          DisvGrid := TCustomModel(GlobalCurrentModel).DisvGrid;
          result := DisvGrid.CellVolume(Lay, Col);
        end
        else
        begin
          result := GetColumnWidth(Col) * GetRowWidth(Row) *
            GetLayerHeight(Col, Row, Lay);
        end;
      end;
    msSutra22, msSutra30:
      begin
        result := 0;
        Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
        if Mesh = nil then
        begin
          Exit;
        end;
        case Mesh.MeshType of
          mt2D, mtProfile: Exit;
          mt3D:
            begin
              case GlobalEvaluatedAt of
                eaBlocks:
                  begin
                    AnElement := Mesh.ElementArray[Lay,Col];
                    result := AnElement.Volume;
                  end;
                eaNodes:
                  begin
                    AnNode := Mesh.NodeArray[Lay,Col];
                    result := AnNode.Volume;
                  end;
                else
                  Assert(False);
              end;
            end;
          else
            Assert(False);
        end;
      end;
    msFootPrint:
      begin
        result := 0;
      end;
    else
      Assert(False);
  end;
end;

function _ColumnPosition(Values: array of pointer): double;
var
  Column: integer;
  LocalGrid : TCustomModelGrid;
begin
  if (frmGoPhast.ModelSelection in SutraSelection) or
    TCustomModel(GlobalCurrentModel).DisvUsed then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if Values[0] <> nil then
  begin
    Column := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
  if (Column < 0) or (Column > LocalGrid.ColumnCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := LocalGrid.ColumnPosition[Column];
  end;
end;

function _RowPosition(Values: array of pointer): double;
var
  Row: integer;
  LocalGrid : TCustomModelGrid;
begin
  if (frmGoPhast.ModelSelection in SutraSelection) or
    TCustomModel(GlobalCurrentModel).DisvUsed then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Row < 0) or (Row > LocalGrid.RowCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := LocalGrid.RowPosition[Row];
  end;
end;

function GetLayerPosition(const Lay, Row, Col: Integer;
  var InvalidIndex: boolean): Double;
var
  LocalGrid: TModflowGrid;
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
  DisvGrid: TModflowDisvGrid;
begin
  result := 0;
  InvalidIndex := False;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if (Lay < 0) or (Lay > frmGoPhast.PhastGrid.LayerCount) then
        begin
          Result := 0;
          InvalidIndex := True;
        end
        else
        begin
          Result := frmGoPhast.PhastGrid.LayerElevation[Lay];
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        if TCustomModel(GlobalCurrentModel).DisvUsed then
        begin
          DisvGrid := TCustomModel(GlobalCurrentModel).DisvGrid;
          if (Lay < 0) or (Lay > DisvGrid.LayerCount)
           or (Row < 0) or (Row > DisvGrid.RowCount-1)
           or (Col < 0) or (Col > DisvGrid.ColumnCount-1) then
          begin
            Result := 0;
            InvalidIndex := True;
          end
          else
          begin
            Result := DisvGrid.LayerPosition(Lay, Col);
          end;
        end
        else
        begin
          LocalGrid := TCustomModel(GlobalCurrentModel).ModflowGrid;
          LocalGrid.UpdateCellElevations;
          if (Lay < 0) or (Lay > LocalGrid.LayerCount)
           or (Row < 0) or (Row > LocalGrid.RowCount-1)
           or (Col < 0) or (Col > LocalGrid.ColumnCount-1) then
          begin
            Result := 0;
            InvalidIndex := True;
          end
          else
          begin
            Result := LocalGrid.CellElevation[Col, Row, Lay];
          end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := TCustomModel(GlobalCurrentModel).Mesh as TSutraMesh3D;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          result := 0;
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              if (Lay < 0) or (Lay > Mesh.LayerCount)
               or (Row <> 0)
               or (Col < 0) or (Col >= Mesh.Mesh2D.Elements.Count) then
              begin
                Result := 0;
                InvalidIndex := True;
              end
              else
              begin
                if Lay = Mesh.LayerCount then
                begin
                  Element := Mesh.ElementArray[Lay-1, Col];
                  result := Element.LowerElevation;
                end
                else
                begin
                  Element := Mesh.ElementArray[Lay, Col];
                  result := Element.UpperElevation;
                end;
              end;
            end;
          eaNodes:
            begin
              if (Lay < 0) or (Lay > Mesh.LayerCount)
               or (Row <> 0)
               or (Col < 0) or (Col >= Mesh.Mesh2D.Nodes.Count) then
              begin
                Result := 0;
                InvalidIndex := True;
              end
              else
              begin
                Node := Mesh.NodeArray[Lay,Col];
                Result := Node.Z;
              end;
            end;
          else Assert(False);
        end;
      end;
    msFootPrint:
      begin
        result := 0;
      end
  else
    Assert(False);
  end;
end;

function _LayerPosition(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
  DummyInvalidIndex: boolean;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  Result := GetLayerPosition(Lay, Row, Col, DummyInvalidIndex);
end;

function GetLayerCenter(const Lay, Row, Col:  integer): double;
var
  BelowValue: Double;
  AboveValue: Double;
  InvalidIndex: boolean;
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
begin
  result := 0;
  case GlobalCurrentModel.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        BelowValue := GetLayerPosition(Lay, Row, Col, InvalidIndex);
        if InvalidIndex then
        begin
          result := 0;
        end
        else
        begin
          AboveValue := GetLayerPosition(Lay+1, Row, Col, InvalidIndex);
          if InvalidIndex then
          begin
            result := 0;
          end
          else
          begin
            result := (BelowValue + AboveValue)/2;
          end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := (GlobalCurrentModel as TCustomModel).Mesh as TSutraMesh3D;
        result := 0;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              Element:= Mesh.ElementArray[Lay, Col];
              result := Element.CenterElevation;
            end;
          eaNodes:
            begin
              Node:= Mesh.NodeArray[Lay, Col];
              result := Node.Z;
            end;
          else
            Assert(False);
        end;
      end;
    msFootPrint:
      begin
        result := 0;
      end
    else
      Assert(False);
  end;
end;

function _LayerCenter(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  result := GetLayerCenter(Lay, Row, Col);
end;

function _LayerCount(Values: array of pointer): integer;
var
  LocalModel: TCustomModel;
  LocalGrid: TCustomModelGrid;
  LocalMesh: TSutraMesh3D;
begin
  LocalModel := TCustomModel(GlobalCurrentModel);
  LocalGrid := LocalModel.Grid;
  if LocalGrid <> nil then
  begin
    Result := LocalGrid.LayerCount + 1;
  end
  else if TCustomModel(GlobalCurrentModel).DisvUsed then
  begin
    Result := TCustomModel(GlobalCurrentModel).LayerCount + 1;
  end
  else
  begin
    LocalMesh := LocalModel.Mesh as TSutraMesh3D;
    if (LocalMesh = nil) or (LocalMesh.MeshType in [mt2D, mtProfile]) then
    begin
      result := 1;
    end
    else
    begin
      result := LocalMesh.LayerCount+1;
    end;
  end;
end;

function _RowCount(Values: array of pointer): integer;
var
  LocalGrid: TCustomModelGrid;
begin
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    if TCustomModel(GlobalCurrentModel).Mesh3D = nil then
    begin
      result := 0;
    end
    else
    begin
      result := 1;
    end;
  end
  else
  begin
    Result := LocalGrid.RowCount + 1;
  end;
end;

function _ColumnCount(Values: array of pointer): integer;
var
  LocalGrid: TCustomModelGrid;
  Mesh: IMesh3D;
//  Mesh: TSutraMesh3D;
begin
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    result := 0;
    Mesh := TCustomModel(GlobalCurrentModel).Mesh3D;
    if Mesh = nil then
    begin
      Exit;
    end
    else
    begin
      case GlobalEvaluatedAt of
        eaBlocks:
          begin
            result := Mesh.Mesh2DI.ElementCount;
          end;
        eaNodes:
          begin
            result := Mesh.Mesh2DI.NodeCount;
          end;
      end;
    end;
  end
  else
  begin
    Result := LocalGrid.ColumnCount + 1;
  end;
end;

function _ObjectLength(Values: array of pointer): double;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalCurrentScreenObject.ScreenObjectLength;
  end;
end;

function _ObjectArea(Values: array of pointer): double;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalCurrentScreenObject.ScreenObjectArea;
  end;
end;

function _ObjectIntersectLength(Values: array of pointer): double;
var
  Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Column := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Row := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Layer := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectIntersectLength(Column, Row,
      Layer, GlobalCurrentModel);
  end;
end;

function _ObjectIntersectSectionLength(Values: array of pointer): double;
var
  Section, Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Section := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Section := GlobalSection;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Column := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Row := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 4) and (Values[3] <> nil) then
    begin
      Layer := PInteger(Values[3])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectSectionIntersectLength(Column, Row,
      Layer, Section, GlobalCurrentModel);
  end;
end;

function _ObjectIntersectArea(Values: array of pointer): double;
var
  Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Column := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Row := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Layer := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectIntersectArea(
      Column, Row, Layer, GlobalCurrentModel);
  end;
end;

function _ObjectName(Values: array of pointer): string;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := '';
  end
  else
  begin
    result := GlobalCurrentScreenObject.Name;
  end;
end;

function _ObjectFiniteElementBasis(Values: array of pointer): double;
var
  X: Double;
  Y: Double;
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  SectionIndex: Integer;
  DefaultValue: double;
  TriangularNodeValues: TTriangularNodeValues;
  QuadNodeValues: TRectangularNodeValues;
  ValuesFound: array of Boolean;
  StartIndex: Integer;
  EndIndex: Integer;
  PPV_Index: Integer;
  PPV_Item: TPointValuesItem;
  Position: Integer;
  NodeIndex: Integer;
  FoundCount: integer;
  TriElement: TTriangularElement;
  QuadElement: TQuadrilateralElement;
  LocationToEvaluate: TPoint2D;
begin
  if (Length(Values) >= 2) then
  begin
    DefaultValue := PDouble(Values[1])^
  end
  else
  begin
    DefaultValue := 0;
  end;

  result := DefaultValue;
  if (GlobalCurrentScreenObject <> nil) and GlobalCurrentScreenObject.Closed
    and (GlobalCurrentScreenObject.PointPositionValues <> nil) then
  begin
    X := 0;
    Y := 0;
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop:
        begin
          X := GlobalX;
          Y := GlobalY;
        end;
      vdFront:
        begin
          X := GlobalX;
          Y := Globalz;
        end;
      vdSide:
        begin
          X := GlobalZ;
          Y := GlobalY;
        end;
      else
        Assert(False);
    end;
    LocationToEvaluate.x := X;
    LocationToEvaluate.y := Y;
    if GlobalCurrentScreenObject.IsPointInside(X, Y, SectionIndex) then
    begin
      if GlobalCurrentScreenObject.SectionClosed[SectionIndex]
        and (GlobalCurrentScreenObject.SectionLength[SectionIndex] in [4,5]) then
      begin
        Assert(Length(Values) >= 1);
        VertexValueName := PString(Values[0])^;
        LocalPPV := GlobalCurrentScreenObject.PointPositionValues;
        StartIndex := GlobalCurrentScreenObject.SectionStart[SectionIndex];
        EndIndex := GlobalCurrentScreenObject.SectionEnd[SectionIndex]-1;
        if GlobalCurrentScreenObject.SectionLength[SectionIndex] = 4 then
        begin
          // triangular element
          SetLength(TriangularNodeValues, 3);
          SetLength(ValuesFound, 3);
          for NodeIndex := 0 to Length(ValuesFound) -1 do
          begin
            ValuesFound[NodeIndex] := false;
          end;
          FoundCount := 0;
          for PPV_Index := 0 to LocalPPV.Count - 1 do
          begin
            PPV_Item := LocalPPV[PPV_Index];
            if (PPV_Item.Position >= StartIndex) and (PPV_Item.Position <= EndIndex) then
            begin
              Position := PPV_Item.IndexOfName(VertexValueName);
              if Position >= 0 then
              begin
                NodeIndex := PPV_Item.Position - StartIndex;
                TriangularNodeValues[NodeIndex] := PPV_Item.Value[Position];
                if not ValuesFound[NodeIndex] then
                begin
                  Inc(FoundCount);
                end;
                ValuesFound[NodeIndex] := True;
                if FoundCount = 3 then
                begin
                  Break;
                end;
              end;
            end;
          end;
          if FoundCount = 3 then
          begin
            SetLength(TriElement, 3);
            for NodeIndex := StartIndex to EndIndex do
            begin
              TriElement[NodeIndex-StartIndex] :=
                GlobalCurrentScreenObject.Points[NodeIndex];
            end;
            result := TriangularBasisFunction(TriElement, TriangularNodeValues,
              LocationToEvaluate);
          end;
        end
        else
        begin
          // quadrilateral element
          SetLength(QuadNodeValues, 4);
          SetLength(ValuesFound, 4);
          for NodeIndex := 0 to Length(ValuesFound) -1 do
          begin
            ValuesFound[NodeIndex] := false;
          end;
          FoundCount := 0;
          for PPV_Index := 0 to LocalPPV.Count - 1 do
          begin
            PPV_Item := LocalPPV[PPV_Index];
            if (PPV_Item.Position >= StartIndex) and (PPV_Item.Position <= EndIndex) then
            begin
              Position := PPV_Item.IndexOfName(VertexValueName);
              if Position >= 0 then
              begin
                NodeIndex := PPV_Item.Position - StartIndex;
                QuadNodeValues[NodeIndex] := PPV_Item.Value[Position];
                if not ValuesFound[NodeIndex] then
                begin
                  Inc(FoundCount);
                end;
                ValuesFound[NodeIndex] := True;
                if FoundCount = 4 then
                begin
                  Break;
                end;
              end;
            end;
          end;
          if FoundCount = 4 then
          begin
            SetLength(QuadElement, 4);
            for NodeIndex := StartIndex to EndIndex do
            begin
              QuadElement[NodeIndex-StartIndex] :=
                GlobalCurrentScreenObject.Points[NodeIndex];
            end;
            result := QuadrilateralBasisFunction(QuadElement, QuadNodeValues,
              LocationToEvaluate);
          end;
        end;
      end
      else
      begin
        Exit;
      end;
    end
    else
    begin
        Exit;
    end;
  end;

end;

function _NodeInterpolate(Values: array of pointer): double;
begin
  result := 0;
  Assert(False);
end;

function EvaluateBooleanDataSetOnLayer(const DataSetName: string;
  Values: array of pointer): boolean;
var
  DataArray: TDataArray;
  ArrayLength: integer;
  Column, Row, Layer: integer;
begin
  DataArray := TCustomModel(GlobalCurrentModel).
    DataArrayManager.GetDataSetByName(DataSetName);
  Assert(DataArray <> nil);
  PushGlobalStack;
  try
    DataArray.Initialize;
  finally
    PopGlobalStack;
  end;
  ArrayLength := Length(Values);
  Assert(ArrayLength = 1);
  Layer := PInteger(Values[0])^ - 1;
  if Layer < 0 then
  begin
    result := False;
  end
  else
  begin
    Column := GlobalColumn - 1;
    Row := GlobalRow - 1;
    result := DataArray.BooleanData[Layer, Row, Column];
  end;
end;

function _ActiveOnLayer(Values: array of pointer): boolean;
var
  Layer: Integer;
  DataArray: TDataArray;
  ArrayLength: Integer;
  Column: Integer;
  Mesh: IMesh3D;
//  Mesh: TSutraMesh3D;
begin
  result := False;
  if TCustomModel(GlobalCurrentModel).Grid <> nil then
  begin
    DataArray := TCustomModel(GlobalCurrentModel).
      DataArrayManager.GetDataSetByName(rsActive);
    Assert(DataArray <> nil);
    PushGlobalStack;
    try
      DataArray.Initialize;
    finally
      PopGlobalStack;
    end;
    ArrayLength := Length(Values);
    Assert(ArrayLength = 1);
    Layer := PInteger(Values[0])^ - 1;
    if Layer >= DataArray.LayerCount then
    begin
      result := False;
      Exit;
    end;
    result := EvaluateBooleanDataSetOnLayer(rsActive, Values);
  end
  else if TCustomModel(GlobalCurrentModel).Mesh3D <> nil then
  begin
    Mesh := TCustomModel(GlobalCurrentModel).Mesh3D;
//    if Mesh = nil then
//    begin
//      result := False;
//      Exit;
//    end;
    ArrayLength := Length(Values);
    Assert(ArrayLength = 1);
    Layer := PInteger(Values[0])^ - 1;
    if Layer < 0 then
    begin
      result := False;
    end
    else
    begin
      if not Mesh.Is3DMesh then
      begin
        result := Layer = 0;
        Exit;
      end;
      Column := GlobalColumn - 1;
      case GlobalEvaluatedAt of
        eaBlocks:
          begin
            if Layer < Mesh.LayerCount then
            begin
              result := Mesh.ElementArrayI[Layer,Column].Active;
            end
            else
            begin
              result := False;
            end;
          end;
        eaNodes:
          begin
            if Layer <= Mesh.LayerCount then
            begin
              result := Mesh.NodeArrayI[Layer,Column].Active;
            end
            else
            begin
              result := False;
            end;
          end;
      end;
    end;
  end;
end;

function _SpecifiedHeadOnLayer(Values: array of pointer): boolean;
var
  DataArray: TDataArray;
  LocalModel: TCustomModel;
  Packages: TModflowPackages;
begin
  result := False;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  LocalModel := TCustomModel(GlobalCurrentModel);
  DataArray := LocalModel.
    DataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  Packages := LocalModel.ModflowPackages;
  if (DataArray <> nil) and (Packages.ChdBoundary.IsSelected
    or ((LocalModel.ModelSelection <> msModflow2015)
    and Packages.FhbPackage.IsSelected)) then
  begin
    result := EvaluateBooleanDataSetOnLayer(rsModflowSpecifiedHead, Values);
  end;
end;

procedure GetImportedValues(var ImportedValues: TValueArrayStorage;
  const Values: array of Pointer; var ImportedName: string);
begin
  if (Length(Values) > 0) and (Values[0] <> nil) then
  begin
    ImportedName := PString(Values[0])^;
    if ImportedName = StrImportedHigherElev then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedHigherSectionElevations;
    end
    else if ImportedName = StrImportedLowerEleva then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedLowerSectionElevations;
    end
    else if ImportedName = StrImportedElevations then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedSectionElevations;
    end
    else
    begin
      ImportedValues := GlobalCurrentScreenObject.
        ImportedValues.ValuesByName(ImportedName);
    end;
  end
  else
  begin
    ImportedName := '';
    ImportedValues := GlobalCurrentScreenObject.CurrentValues;
  end;
end;

function _ImportedScreenObjectValuesR(Values: array of pointer): double;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ImportedName: string;
  ErrorMessage: string;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesR]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]),
          GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtDouble then
      begin
        ErrorMessage := Format(StrTheDataAccessedTh, [rsObjectImportedValuesR]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.RealValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesI(Values: array of pointer): integer;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesI]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtInteger then
      begin
        ErrorMessage := Format(StrTheDataAccessedThInt, [rsObjectImportedValuesI]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      Assert(ImportedValues.DataType= rdtInteger);
      result := ImportedValues.IntValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesB(Values: array of pointer): boolean;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := False;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesB]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtBoolean then
      begin
        ErrorMessage := Format(StrTheDataAccessedThBoole,
          [rsObjectImportedValuesB]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.BooleanValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesT(Values: array of pointer): string;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := '';
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesT]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtString then
      begin
        ErrorMessage := Format(StrTheDataAccessedThStr,
          [rsObjectImportedValuesT]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.StringValues[Index];
    end;
  end;
end;

function GetDataSetValue(Column: Integer; Row: Integer; Layer: Integer;
  const DataSetName: string): Double;
var
  DataArray: TDataArray;
  AModel: TCustomModel;
begin
  AModel := TCustomModel(GlobalCurrentModel);
  DataArray := AModel.DataArrayManager.GetDataSetByName(DataSetName);
  Assert(DataArray <> nil);
  PushGlobalStack;
  try
    DataArray.Initialize;
  finally
    PopGlobalStack;
  end;
  result := DataArray.RealData[Layer, Row, Column];
end;


procedure UpdateCellValue(var CellValue: Double;
  Param: THufUsedParameter; PhastModel: TCustomModel; Column, Row: Integer;
  var Updated: boolean);
var
  Multiplier: Double;
  ZoneArray: TDataArray;
  Parameter: TModflowParameter;
begin
  if Param.UseZone then
  begin
    ZoneArray := TCustomModel(GlobalCurrentModel).DataArrayManager.
      GetDataSetByName(Param.ZoneDataSetName);
    Assert(ZoneArray <> nil);
    PushGlobalStack;
    try
      ZoneArray.Initialize;
    finally
      PopGlobalStack;
    end;
    if not ZoneArray.BooleanData[0, Row, Column] then
    begin
      Exit;
    end;
  end;
  if Param.UseMultiplier then
  begin
    Multiplier := GetDataSetValue(Column, Row, 0, Param.MultiplierDataSetName);
  end
  else
  begin
    Multiplier := 1;
  end;
  Parameter := Param.Parameter;
  CellValue := CellValue + Multiplier * Parameter.Value;
  Updated := True;
end;

function _BcfGetVcont(Values: array of pointer): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  AModel: TCustomModel;
  KzDataArray: TDataArray;
  TopLayer: Integer;
  KzNonSimDataArray: TDataArray;
  BottomLayer: Integer;
  LayerIndex: Integer;
  Grid: TModflowGrid;
  LayerThickness: Double;
  VK: Double;
  ASum: double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  GetCellIndicies(Column, Row, Layer, Values);
  Grid := AModel.ModflowGrid;
  if (Grid = nil) or (Grid.LayerCount -1 <= Layer)
    or (Layer < 0) then
  begin
    // VCont for bottom layer is always zero.
    Exit;
  end;

  while not AModel.IsLayerSimulated(Layer)
    and (Layer >= 0) do
  begin
    Dec(Layer);
  end;

  KzNonSimDataArray := nil;
  TopLayer := Layer;
  Inc(Layer);
  if not AModel.IsLayerSimulated(Layer) then
  begin
    KzNonSimDataArray := AModel.DataArrayManager.GetDataSetByName(rsModflow_CBKz);
    Assert(KzNonSimDataArray <> nil);
    PushGlobalStack;
    try
      KzNonSimDataArray.Initialize;
    finally
      PopGlobalStack;
    end;
    Inc(Layer);
  end;
  BottomLayer := Layer;

  KzDataArray := AModel.DataArrayManager.GetDataSetByName(rsKz);
  Assert(KzDataArray <> nil);
  PushGlobalStack;
  try
    KzDataArray.Initialize;
  finally
    PopGlobalStack;
  end;

  ASum := 0.0;
  for LayerIndex := TopLayer to BottomLayer do
  begin
    LayerThickness := GetLayerHeight(Column, Row, LayerIndex);
    if LayerThickness <= 0 then
    begin
      Exit;
    end;
    if AModel.IsLayerSimulated(LayerIndex) then
    begin
      VK := KzDataArray.RealData[LayerIndex, Row, Column];
      LayerThickness := LayerThickness/2;
    end
    else
    begin
      VK := KzNonSimDataArray.RealData[LayerIndex, Row, Column];
    end;
    if VK <= 0 then
    begin
      Exit;
    end;
    ASum := ASum + LayerThickness/VK;
  end;
  if ASum > 0 then
  begin
    result := 1/ASum;
  end;

end;

function HguHorizontalAnisotrpy(HufUnit: THydrogeologicUnit; Column, Row: integer): double;
var
  HorizontalAnisotropy: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  HufThickness: Double;
  HaniUsed: boolean;
begin
  result := 1;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  HaniUsed := False;
  HorizontalAnisotropy := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = ptHUF_HANI then
    begin
      UpdateCellValue(HorizontalAnisotropy, Param, AModel, Column, Row, HaniUsed);
    end
  end;
  if not HaniUsed then
  begin
    HorizontalAnisotropy := HufUnit.HorizontalAnisotropy;
  end;
  result := HorizontalAnisotropy;

end;

function HguTransmissivity(HufUnit: THydrogeologicUnit; Column, Row: integer;
  IntervalTop, IntervalBottom: double): double;
var
  KDEP_Used: Boolean;
  HufK: double;
  Lambda: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  KDepMult: double;
  GroundSurface: Double;
  HufTop: Double;
  DepthToTop: Double;
  HufThickness: Double;
  HufBottom: Double;
  DepthToBottom: Double;
  Kx_Used: Boolean;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  KDEP_Used := False;
  Kx_Used := False;
  HufK := 0.;
  Lambda := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = ptHUF_HK then
    begin
      UpdateCellValue(HufK, Param, AModel, Column, Row, Kx_Used);
    end
    else if Parameter.ParameterType = ptHUF_KDEP then
    begin
      UpdateCellValue(Lambda, Param, AModel, Column, Row, KDEP_Used);
    end;
  end;
  if not Kx_Used then
  begin
    Exit;
  end;

  KDepMult := 1.;
  if KDEP_Used then
  begin
    if AModel.ModflowPackages.HufPackage.ReferenceChoice
      = hrcReferenceLayer then
    begin
      GroundSurface := GetDataSetValue(Column, Row, 0, StrHufReferenceSurface);
    end
    else
    begin
      GroundSurface := GetDataSetValue(Column, Row, 0, kModelTop);
    end;

    HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);

    HufBottom := HufTop - HufThickness;

    if IntervalTop < HufTop then
    begin
      HufTop := IntervalTop;
    end;

    if IntervalBottom > HufBottom then
    begin
      HufBottom := IntervalBottom;
    end;

    DepthToTop := GroundSurface - HufTop;
    DepthToBottom := GroundSurface - HufBottom;
    if Abs(2*(DepthToTop-DepthToBottom)/
      (DepthToBottom+DepthToTop)) < 1E-6 then
    begin
      KDepMult := 1.;
    end
    else
    begin
      KDepMult := Power(10.,(-Lambda*DepthToBottom))
        - Power(10.,(-Lambda*DepthToTop));
      KDepMult := KDepMult / (-Lambda*Ln(10.0)*(DepthToBottom-DepthToTop))
    end;
  end;
  // result is in terms of transmissivity at this point.
  result := HufK * KDepMult *HufThickness;

end;

procedure GetLayerTopAndBottom(Head: double; Layer, Row, Column: Integer;
  var LayerBottom, LayerTop: Double; UseHead: boolean = True);
var
  LayerGroup: TLayerGroup;
  PhastModel: TPhastModel;
  DummyInvalidIndex: boolean;
  GLayer: Integer;
begin
  PhastModel := frmGoPhast.PhastModel;
  if GlobalCurrentModel <> PhastModel then
  begin
    GLayer := (GlobalCurrentModel as TChildModel).ChildLayerToParentLayer(Layer);
  end
  else
  begin
    GLayer := Layer;
  end;
  LayerTop := GetLayerPosition(Layer, Row, Column, DummyInvalidIndex);
  LayerBottom := GetLayerPosition(Layer + 1, Row, Column, DummyInvalidIndex);
  LayerGroup := PhastModel.LayerStructure.GetLayerGroupByLayer(GLayer);
  if (LayerGroup.AquiferType <> 0) and UseHead then
  begin
    if Head < LayerTop then
    begin
      LayerTop := Head;
    end;
  end;
end;

function HufUnitParam(HufUnit: THydrogeologicUnit; Column, Row: integer;
  Paramtype: TParameterType; var Updated: boolean): double;
var
  CellValue: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  HufThickness: Double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  CellValue := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = Paramtype then
    begin
      UpdateCellValue(CellValue, Param, AModel, Column, Row, Updated);
    end
  end;

  // result will be divided by total cell thickness later.
  result := CellValue //* HufThickness;
end;

function _GetHufSytp(Values: array of pointer): double;
var
  Column: Integer;
  Row: Integer;
  Index: Integer;
  SteadyParameters: TModflowSteadyParameters;
  AParam: TModflowSteadyParameter;
  DataArrayManager: TDataArrayManager;
  ZoneDataArray: TDataArray;
  AValue: Double;
  MultiplierDataArray: TDataArray;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if (Length(Values) >= 1) and (Values[0] <> nil) then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Length(Values) >= 2) and (Values[1] <> nil) then
  begin
    Column := PInteger(Values[1])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
//  GetCellIndicies(Dummy, Column, Row,  Values);
  SteadyParameters := frmGoPhast.PhastModel.ModflowSteadyParameters;
  DataArrayManager := TCustomModel(GlobalCurrentModel).DataArrayManager;
  for Index := 0 to SteadyParameters.Count - 1 do
  begin
    AParam := SteadyParameters[Index];
    if AParam.ParameterType = ptHUF_SYTP then
    begin
      if AParam.UseZone then
      begin
        ZoneDataArray := DataArrayManager.GetDataSetByName(AParam.ZoneName);
        PushGlobalStack;
        try
          ZoneDataArray.Initialize;
        finally
          PopGlobalStack;
        end;
        if not ZoneDataArray.BooleanData[0, Row, Column] then
        begin
          Continue;
        end;
      end;
      AValue := AParam.Value;
      if AParam.UseMultiplier then
      begin
        MultiplierDataArray := DataArrayManager.GetDataSetByName(AParam.MultiplierName);
        PushGlobalStack;
        try
          MultiplierDataArray.Initialize;
        finally
          PopGlobalStack;
        end;
        AValue := AValue * MultiplierDataArray.RealData[0, Row, Column];
      end;
      result := result + AValue;
    end;
  end;
end;


function _GetHufSy(Values: array of pointer): double;
var
  Head: double;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  AModel: TCustomModel;
  LayerBottom: Double;
  LayerTop: Double;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  Updated: Boolean;
begin
  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  result := 0;
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if AModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
      if HufTop > LayerBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < LayerTop then
        begin
          if HufBottom < LayerBottom then
          begin
            HufBottom := LayerBottom;
          end;
          if HufTop > LayerTop then
          begin
            HufTop := LayerTop
          end;
          HufThickness := HufTop - HufBottom;
          if HufThickness > 0 then
          begin
            if (HufTop >= Head) and (Head >= HufBottom) then
            begin
              result := HufUnitParam(HufUnit, Column, Row, ptHUF_SY, Updated);
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

end;

function HufKHorizontal(Values: array of pointer;
  UseHorizontalAnisotropy: boolean): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  LayerTop: Double;
  LayerBottom: Double;
  HufUnitIndex: Integer;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnit: THydrogeologicUnit;
  AModel: TCustomModel;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  CumulativeHufThickness: double;
  HorizontalAnisotropy: double;
  Head: double;
  HufLayerThickness: Double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if AModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // AModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.

  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;

  GetCellIndicies(Column, Row, Layer, Values, 1);

  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0,
      HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
      if HufTop > LayerBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < LayerTop then
        begin
          if HufBottom < LayerBottom then
          begin
            HufBottom := LayerBottom;
          end;
          if HufTop > LayerTop then
          begin
            HufTop := LayerTop
          end;
          HufLayerThickness := HufTop - HufBottom;
          if HufLayerThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufLayerThickness;
            // result is in terms of transmissivity at this point.
            if UseHorizontalAnisotropy then
            begin
              HorizontalAnisotropy := HguHorizontalAnisotrpy(
                HufUnit, Column, Row);
            end
            else
            begin
              HorizontalAnisotropy := 1.;
            end;
            result := result + HorizontalAnisotropy*
              HguTransmissivity(HufUnit, Column, Row, LayerTop, LayerBottom)
              *HufLayerThickness/HufThickness;
          end;
        end;
      end;
    end;
  end;
  if CumulativeHufThickness > 0 then
  begin
    // convert transmissivity back to hydraulic conductivity.
    result := result/CumulativeHufThickness;
  end;
end;

function _HufKx(Values: array of pointer): double;
begin
  result := HufKHorizontal(Values, False);
  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // PhastModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.
end;

function _HufAverageKY(Values: array of pointer): double;
begin
  result := HufKHorizontal(Values, True);
  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // PhastModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.
  // affected by zone arrays and multiplier arrays of ptHUF_HANI
  // parameters
end;

function _HufKZ(Values: array of pointer): double;
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  AModel: TCustomModel;
  UpperLayerBottom: Double;
  UpperLayerTop: Double;
  LowerLayerBottom: Double;
  LowerLayerTop: Double;
  IntervalTop: Double;
  IntervalBottom: Double;
  HydrogeologicUnits: THydrogeologicUnits;
  CumulativeHufThickness: double;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  VK: double;
  Vani_Used: Boolean;
  Vani: double;
  VK_Used: Boolean;
  UpperHead: double;
  HufIntervalThickness: Double;
  ZeroResultErrorDisplayed: Boolean;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  // affected by everything in _HufKx plus the
  // by zone arrays and multiplier arrays of ptHUF_VK and ptHUF_VANI
  // parameters
  // affected by HufUnit.VK_Method.

  ZeroResultErrorDisplayed := False;
  Assert(Length(Values) >= 1);
  UpperHead := PDouble(Values[0])^;
//  LowerHead := PDouble(Values[2])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;

  if (AModel.ModflowGrid.LayerCount -1 <= Layer)
    or (Layer < 0) then
  begin
    // no vertical hydraulic conductivity is defined
    // below the middle of the bottom layer.
    Exit;
  end;

  GetLayerTopAndBottom(UpperHead, Layer, Row, Column, UpperLayerBottom, UpperLayerTop);

  if UpperLayerTop <= UpperLayerBottom then
  begin
    Exit;
  end;

  GetLayerTopAndBottom(UpperHead, Layer+1, Row, Column,
    LowerLayerBottom, LowerLayerTop, False);

  if LowerLayerTop <= LowerLayerBottom then
  begin
    Exit;
  end;

  IntervalTop := (UpperLayerTop + UpperLayerBottom)/2;
  IntervalBottom := (LowerLayerTop + LowerLayerBottom)/2;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
      if HufTop > IntervalBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < IntervalTop then
        begin
          if HufBottom < IntervalBottom then
          begin
            HufBottom := IntervalBottom;
          end;
          if HufTop > IntervalTop then
          begin
            HufTop := IntervalTop
          end;
          HufIntervalThickness := HufTop - HufBottom;
          if HufIntervalThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufIntervalThickness;

            VK := 0.;
            Vani := 0.;
            Vani_Used := False;
            VK_Used := False;

            for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
            begin
              Param := HufUnit.HufUsedParameters[ParamIndex];
              Parameter := Param.Parameter;
              if Parameter.ParameterType = ptHUF_VK then
              begin
                UpdateCellValue(VK, Param, AModel, Column, Row, VK_Used);
              end
              else if Parameter.ParameterType = ptHUF_VANI then
              begin
                UpdateCellValue(Vani, Param, AModel, Column, Row, Vani_Used);
              end;
            end;

            case HufUnit.VK_Method of
              vkVK:
                begin
                  if Vani_Used then
                  begin
                    frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
                      Format(StrInSVANIParamete, [HufUnit.HufName]),
                      Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
                  end;
                  // do nothing.
                end;
              vkVANI:
                begin
                  if not Vani_Used then
                  begin
                    Vani := HufUnit.VerticalAnisotropy;
                  end;
                  if Vani <= 0 then
                  begin
                    Vani := 1;
                  end;
                  VK := HguTransmissivity(HufUnit, Column, Row, IntervalTop, IntervalBottom)
                    /HufThickness/Vani;


//                  result := result +
//                    HguTransmissivity(HufUnit, Column, Row)/Vani;
                end;
              else Assert(False);
            end;
            if VK = 0 then
            begin
//              result := 0;
              if HufIntervalThickness > 0 then
              begin
                frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
                  StrBecauseHorizontalH,
                  Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
                ZeroResultErrorDisplayed := True;
              end;
            end
            else
            begin
              result := result + HufIntervalThickness/VK;
            end;
          end;
        end;
      end;
    end;
  end;
  if CumulativeHufThickness > 0 then
  begin
    if result = 0 then
    begin
      if not ZeroResultErrorDisplayed then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          StrBecauseHorizontalH,
          Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
      end;
    end
    else
    begin
      result := CumulativeHufThickness/result;
    end;
  end;
end;

{function HufUnitParam(HufUnit: THydrogeologicUnit; Column, Row: integer;
  Paramtype: TParameterType; var Updated: boolean): double;
var
  CellValue: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  PhastModel: TPhastModel;
  HufThickness: Double;
begin
  result := 0;
  PhastModel := frmGoPhast.PhastModel;
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  CellValue := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = Paramtype then
    begin
      UpdateCellValue(CellValue, Param, PhastModel, Column, Row, Updated);
    end
  end;

  // result will be divided by total cell thickness later.
  result := CellValue //* HufThickness;
end; }

function HufAveragParam(Values: array of pointer;
  Paramtype: TParameterType): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  LayerTop: Double;
  LayerBottom: Double;
  HufUnitIndex: Integer;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnit: THydrogeologicUnit;
  AModel: TCustomModel;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  CumulativeHufThickness: double;
  Updated: boolean;
  Head: double;
begin
  // affected by simulated layer
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of Paramtype

  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  result := 0;
  if AModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
      if HufTop > LayerBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < LayerTop then
        begin
          if HufBottom < LayerBottom then
          begin
            HufBottom := LayerBottom;
          end;
          if HufTop > LayerTop then
          begin
            HufTop := LayerTop
          end;
          HufThickness := HufTop - HufBottom;
          if HufThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufThickness;
            result := result + HufThickness*
              HufUnitParam(HufUnit, Column, Row, Paramtype, Updated);
          end;
        end;
      end;
    end;
  end;
  if CumulativeHufThickness > 0 then
  begin
    // convert transmissivity back to hydraulic conductivity.
    result := result/CumulativeHufThickness;
  end;
end;

function _HufSS(Values: array of pointer): double;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  result := HufAveragParam(Values, ptHUF_SS);
end;

function _HufAverageSY(Values: array of pointer): double;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  result := HufAveragParam(Values, ptHUF_SY);
end;

function _GridNumber(Values: array of pointer): integer;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 1;
  end
  else
  begin
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel = GlobalCurrentModel then
        begin
          result := ChildIndex + 2;
          Exit;
        end;
      end;
    end;
  end;
end;

function _GridName(Values: array of pointer): string;
var
  ChildModel: TChildModel;
begin
  result := '';
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 'Parent Grid';
  end
  else
  begin
    if frmGoPhast.PhastModel.LgrUsed and (GlobalCurrentModel <> nil) then
    begin
      ChildModel := GlobalCurrentModel as TChildModel;
      result := ChildModel.ModelName;
    end;
  end;
end;

function _ParentLayer(Values: array of pointer): integer;
begin
  result := _Layer(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildLayerToParentLayer(result-1)+1;
  end;
end;

function _ParentRow(Values: array of pointer): integer;
begin
  result := _Row(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildRowToParentRow(result-1)+1;
  end;
end;

function _ParentColumn(Values: array of pointer): integer;
begin
  result := _Column(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildColToParentCol(result-1)+1;
  end;
end;

function _HorizontalSubdivision(Values: array of pointer): integer;
var
  ChildModel: TChildModel;
begin
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 1;
  end
  else
  begin
    ChildModel := GlobalCurrentModel as TChildModel;
    result := ChildModel.ChildCellsPerParentCell
  end;
end;

function _VerticalSubdivision(Values: array of pointer): integer;
var
  ChildModel: TChildModel;
  Layer: integer;
  DisItem: TChildDiscretization;
begin
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 1;
  end
  else
  begin
    ChildModel := GlobalCurrentModel as TChildModel;
    if Values[0] <> nil then
    begin
      Layer := PInteger(Values[0])^;
    end
    else
    begin
      Layer := GlobalLayer;
    end;
    DisItem := ChildModel.Discretization.GetAnItemByLayer(Layer-1);
    if DisItem <> nil then
    begin
      result := DisItem.Discretization;
    end
    else
    begin
      result := -1;
    end;
  end;
end;

{ TNodeInterpolateExpression }

procedure TNodeInterpolateExpression.Evaluate;
var
  ArrayLength: integer;
  AVariable: TConstant;
  Value1, Value2: double;
  Point1, Point2: TPoint2D;
  SegmentLength: double;
  PositionDistance: double;
  IsFirstPoint: Boolean;
  IsLastPoint: Boolean;
  LocalGrid: TCustomModelGrid;
  function EvaluateLength(const X1, Y1, X2, Y2: double): double;
  begin
    result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
  end;
begin
  if ShouldEvaluate then
  begin
    if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil) then
    begin
      PDouble(FResult)^ := 0;
      Exit;
    end;

    ArrayLength := Length(Data);
    Assert(ArrayLength >= 2);
    if GlobalCurrentSegment.VertexIndex >= ArrayLength - 1 then
    begin
      Assert(Data[ArrayLength - 1].DataType in [rdtDouble, rdtInteger]);
      AVariable := Data[ArrayLength - 1].Datum;
      if AVariable is TExpression then
      begin
        TExpression(AVariable).Evaluate;
      end;
      PDouble(FResult)^ := AVariable.DoubleResult;
      Exit;
    end;

    Assert(Data[GlobalCurrentSegment.VertexIndex].DataType in [rdtDouble,
      rdtInteger]);
    AVariable := Data[GlobalCurrentSegment.VertexIndex].Datum;
    if AVariable is TExpression then
    begin
      TExpression(AVariable).Evaluate;
    end;
    Value1 := AVariable.DoubleResult;
    Point1 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex];
    LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
    if LocalGrid <> nil then
    begin
      Point1 := LocalGrid.
        RotateFromRealWorldCoordinatesToGridCoordinates(Point1);
    end;
    IsFirstPoint := False;
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          IsFirstPoint := (Point1.X = GlobalCurrentSegment.X1)
            and (Point1.Y = GlobalCurrentSegment.Y1)
        end;
      vdSide:
        begin
          IsFirstPoint := (Point1.Y = GlobalCurrentSegment.X1)
            and (Point1.X = GlobalCurrentSegment.Y1)
        end;
      else Assert(False);
    end;
    if IsFirstPoint then
    begin
      PDouble(FResult)^ := Value1;
      Exit;
    end;

    Assert(Data[GlobalCurrentSegment.VertexIndex + 1].DataType in [rdtDouble,
      rdtInteger]);
    AVariable := Data[GlobalCurrentSegment.VertexIndex + 1].Datum;
    if AVariable is TExpression then
    begin
      TExpression(AVariable).Evaluate;
    end;
    Value2 := AVariable.DoubleResult;
    Point2 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex + 1];
    if LocalGrid <> nil then
    begin
      Point2 := LocalGrid.
        RotateFromRealWorldCoordinatesToGridCoordinates(Point2);
    end;
    IsLastPoint := False;
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          IsLastPoint := (Point2.X = GlobalCurrentSegment.X2)
            and (Point2.Y = GlobalCurrentSegment.Y2);
        end;
      vdSide:
        begin
          IsLastPoint := (Point2.Y = GlobalCurrentSegment.X2)
            and (Point2.X = GlobalCurrentSegment.Y2);
        end;
      else Assert(False);
    end;
    if IsLastPoint then
    begin
      PDouble(FResult)^ := Value2;
      Exit;
    end;

    SegmentLength := EvaluateLength(Point1.X, Point1.Y, Point2.X, Point2.Y);

    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          Point2.X := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
          Point2.Y := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
        end;
      vdSide:
        begin
          Point2.Y := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
          Point2.X := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
        end;
      else Assert(False);
    end;
    PositionDistance := EvaluateLength(Point1.X, Point1.Y, Point2.X, Point2.Y);

    PDouble(FResult)^ := PositionDistance / SegmentLength * (Value2 - Value1) +
      Value1;
  end;
end;

{ TActiveOnLayer }

function TActiveOnLayer.GetVariablesUsed: TStringList;
var
  LayerIndex: Integer;
  SutraLayerStructure: TSutraLayerStructure;
  LayerGroup: TSutraLayerGroup;
begin
  result := inherited GetVariablesUsed;
  if (frmGoPhast.ModelSelection in ModelsWithGrid)
    and (frmGoPhast.ModelSelection <> msFootPrint)  then
  begin
    result.Add(rsActive);
  end
  else if frmGoPhast.ModelSelection in SutraSelection then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
    begin
      SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
      for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
      begin
        LayerGroup := SutraLayerStructure[LayerIndex];
        result.Add(LayerGroup.DataArrayName);
      end;
    end;
  end;
end;

function TActiveOnLayer.UsesVariable(const Variable: TCustomVariable): boolean;
var
  SutraLayerStructure: TSutraLayerStructure;
  LayerIndex: Integer;
  LayerGroup: TSutraLayerGroup;
begin
  if (frmGoPhast.ModelSelection in ModelsWithGrid)
    and (frmGoPhast.ModelSelection <> msFootPrint)  then
  begin
    result := inherited UsesVariable(Variable)
      or (Variable.Name = UpperCase(rsActive));
  end
  else
  begin
    result := inherited UsesVariable(Variable);
    if not result then
    begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
    begin
      SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
      for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
      begin
        LayerGroup := SutraLayerStructure[LayerIndex];

        result := (Variable.Name = UpperCase(LayerGroup.DataArrayName));
        if result then
        begin
          exit;
        end;
      end;
    end;
    end;
  end;
end;

{ TSpecifiedHeadOnLayer }

function TSpecifiedHeadOnLayer.GetVariablesUsed: TStringList;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsModflowSpecifiedHead);
end;

function TSpecifiedHeadOnLayer.UsesVariable(
  const Variable: TCustomVariable): boolean;
begin
  result := inherited UsesVariable(Variable)
    or (Variable.Name = UpperCase(rsModflowSpecifiedHead));
end;

{ TBcfVcont }

function TBcfVcont.GetVariablesUsed: TStringList;
var
  Index: Integer;
  GeoUnit: TLayerGroup;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsKz);
  if TCustomModel(GlobalCurrentModel).LayerStructure.NonSimulatedLayersPresent then
  begin
    result.Add(rsModflow_CBKz);
  end;
  for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
  begin
    GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
  end;
end;

function TBcfVcont.UsesVariable(const Variable: TCustomVariable): boolean;
var
  Index: Integer;
  GeoUnit: TLayerGroup;
begin
  result := inherited UsesVariable(Variable)
    or SameText(Variable.Name, rsKz)
    or (SameText(Variable.Name, rsModflow_CBKz)
    and TCustomModel(GlobalCurrentModel).LayerStructure.NonSimulatedLayersPresent);
  if not result then
  begin
    for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
    begin
      GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
      result := SameText(Variable.Name, GeoUnit.DataArrayName);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ THufKx }

function THufKx.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  KDEP_Used: Boolean;
  Index: Integer;
  GeoUnit: TLayerGroup;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);

  InitialHeadUsed := False;
  for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
  begin
    GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  KDEP_Used := False;
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_HK then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end
      else if Parameter.ParameterType = ptHUF_KDEP then
      begin
        KDEP_Used := True;
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;

  if KDEP_Used then
  begin
    case AModel.ModflowPackages.HufPackage.ReferenceChoice of
      hrcModelTop:
        begin
//          kModelTop added previously.
//          result.Add(kModelTop);
        end;
      hrcReferenceLayer:
        begin
          result.Add(StrHufReferenceSurface);
        end;
      else Assert(False);
    end;
  end;

end;

{
function THufKx.UsesVariable(const Variable: TCustomVariable): boolean;
var
  List: TStringList;
begin
  List := GetVariablesUsed;
  result := List.IndexOf(Variable.Name) >= 0;
//  result := inherited UsesVariable(Variable)
//    or SameText(Variable.Name, rsModflow_Initial_Head)
//    or (SameText(Variable.Name, rsModflow_CBKz)
//    and frmGoPhast.PhastModel.LayerStructure.NonSimulatedLayersPresent);
//  if not result then
//  begin
//    for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
//    begin
//      GeoUnit := frmGoPhast.PhastModel.LayerStructure[Index];
//      result := SameText(Variable.Name, GeoUnit.DataArrayName);
//      if result then
//      begin
//        Exit;
//      end;
//    end;
//  end;
end;
}

{ THufKy }

function THufKy.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
begin
  result := inherited GetVariablesUsed;
  AModel := TCustomModel(GlobalCurrentModel);
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_HANI then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufKz }

function THufKz.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
begin
  result := inherited GetVariablesUsed;
  AModel := TCustomModel(GlobalCurrentModel);
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType in [ptHUF_VK, ptHUF_VANI] then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ TCustomHufExpression }

function TCustomHufExpression.UsesVariable(
  const Variable: TCustomVariable): boolean;
var
  List: TStringList;
begin
  List := GetVariablesUsed;
  result := List.IndexOf(Variable.Name) >= 0;
end;

{ THufSS }

function THufSS.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  Index: Integer;
  GeoUnit: TLayerGroup;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);
  InitialHeadUsed := False;
  for Index := 0 to AModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := AModel.LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_SS then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufSY }

function THufSY.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  Index: Integer;
  GeoUnit: TLayerGroup;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);
  InitialHeadUsed := False;
  for Index := 0 to AModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := AModel.LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_SY then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufSYTP }

function THufSYTP.GetVariablesUsed: TStringList;
var
  AtModel: TCustomModel;
  SteadyParameters: TModflowSteadyParameters;
  Index: Integer;
  AParam: TModflowSteadyParameter;
begin
  result := inherited GetVariablesUsed;

  AtModel := TCustomModel(GlobalCurrentModel);
  SteadyParameters := AtModel.ModflowSteadyParameters;
  for Index := 0 to SteadyParameters.Count - 1 do
  begin
    AParam := SteadyParameters[Index];
    if AParam.ParameterType = ptHUF_SYTP then
    begin
      if AParam.UseZone then
      begin
        result.Add(AParam.ZoneName)
      end;
      if AParam.UseMultiplier then
      begin
        result.Add(AParam.MultiplierName)
      end;
    end;
  end;
end;

function CurrentObject: TScreenObject;
begin
  result := GlobalCurrentScreenObject;
end;

resourcestring
  StrGIS = 'GIS|';
  StrGridOrMesh = 'Grid or Mesh|';
  StrObject = 'Object|';
  StrMODFLOW = 'MODFLOW|';
  StrModflowLgr = 'MODFLOW-LGR|';
  StrObjectVertexValue = 'Object_VertexValue|';

{ TLayerSlope }

function TLayerSlope.GetVariablesUsed: TStringList;
var
  ModflowLayerStructure: TLayerStructure;
  LayerIndex: Integer;
  ModflowLayerGroup: TLayerGroup;
  SutraLayerStructure: TSutraLayerStructure;
  LayerGroup: TSutraLayerGroup;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsActive);
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    result.Add(K_IDOMAIN);
  end;

  if (frmGoPhast.ModelSelection in ModflowSelection) then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.LayerStructure <> nil) then
    begin
      ModflowLayerStructure := frmGoPhast.PhastModel.LayerStructure;
      for LayerIndex := 0 to ModflowLayerStructure.Count - 1 do
      begin
        ModflowLayerGroup := ModflowLayerStructure[LayerIndex];
        result.Add(ModflowLayerGroup.DataArrayName);
      end;
    end;
  end
  else if (frmGoPhast.ModelSelection in SutraSelection) then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
    begin
      SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
      for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
      begin
        LayerGroup := SutraLayerStructure[LayerIndex];
        result.Add(LayerGroup.DataArrayName)
      end;
    end;
  end;

end;

function TLayerSlope.UsesVariable(const Variable: TCustomVariable): boolean;
var
  SutraLayerStructure: TSutraLayerStructure;
  LayerIndex: Integer;
  LayerGroup: TSutraLayerGroup;
  ModflowLayerStructure: TLayerStructure;
  ModflowLayerGroup: TLayerGroup;
begin
  result := inherited UsesVariable(Variable)
    or (Variable.Name = UpperCase(rsActive))
    or (Variable.Name = UpperCase(K_IDOMAIN));
  if not result then
  begin

    if (frmGoPhast.ModelSelection in ModflowSelection) then
    begin
      if (frmGoPhast.PhastModel <> nil)
        and (frmGoPhast.PhastModel.LayerStructure <> nil) then
      begin
        ModflowLayerStructure := frmGoPhast.PhastModel.LayerStructure;
        for LayerIndex := 0 to ModflowLayerStructure.Count - 1 do
        begin
          ModflowLayerGroup := ModflowLayerStructure[LayerIndex];

          result := (Variable.Name = UpperCase(ModflowLayerGroup.DataArrayName));
          if result then
          begin
            exit;
          end;
        end;
      end;
    end
    else if (frmGoPhast.ModelSelection in SutraSelection) then
    begin
      if (frmGoPhast.PhastModel <> nil)
        and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
      begin
        SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
        for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
        begin
          LayerGroup := SutraLayerStructure[LayerIndex];

          result := (Variable.Name = UpperCase(LayerGroup.DataArrayName));
          if result then
          begin
            exit;
          end;
        end;
      end;
    end;
  end;
end;

initialization
  SpecialImplementors := TList.Create;

  XFunction.ResultType := rdtDouble;
  XFunction.RFunctionAddr := _X;
  SetLength(XFunction.InputDataTypes, 0);
  XFunction.OptionalArguments := 0;
  XFunction.CanConvertToConstant := False;
  XFunction.Name := 'X';
  XFunction.Prototype := StrGIS+'X';

  YFunction.ResultType := rdtDouble;
  YFunction.RFunctionAddr := _Y;
  SetLength(YFunction.InputDataTypes, 0);
  YFunction.OptionalArguments := 0;
  YFunction.CanConvertToConstant := False;
  YFunction.Name := 'Y';
  YFunction.Prototype := StrGIS+'Y';

  ZFunction.ResultType := rdtDouble;
  ZFunction.RFunctionAddr := _Z;
  SetLength(ZFunction.InputDataTypes, 0);
  ZFunction.OptionalArguments := 0;
  ZFunction.CanConvertToConstant := False;
  ZFunction.Name := 'Z';
  ZFunction.Prototype := StrGIS+'Z';

  XPrimeFunction.ResultType := rdtDouble;
  XPrimeFunction.RFunctionAddr := _XPrime;
  SetLength(XPrimeFunction.InputDataTypes, 0);
  XPrimeFunction.OptionalArguments := 0;
  XPrimeFunction.CanConvertToConstant := False;
  XPrimeFunction.Name := 'X_Prime';
  XPrimeFunction.Prototype := StrGIS+'X_Prime';

  YPrimeFunction.ResultType := rdtDouble;
  YPrimeFunction.RFunctionAddr := _YPrime;
  SetLength(YPrimeFunction.InputDataTypes, 0);
  YPrimeFunction.OptionalArguments := 0;
  YPrimeFunction.CanConvertToConstant := False;
  YPrimeFunction.Name := 'Y_Prime';
  YPrimeFunction.Prototype := StrGIS+'Y_Prime';

  ColumnFunction.ResultType := rdtInteger;
  ColumnFunction.IFunctionAddr := _Column;
  SetLength(ColumnFunction.InputDataTypes, 0);
  ColumnFunction.OptionalArguments := 0;
  ColumnFunction.CanConvertToConstant := False;
  ColumnFunction.Name := 'Column';
  ColumnFunction.Prototype := StrGridOrMesh+'Column';

  ElevationToModelLayerFunction.ResultType := rdtInteger;
  ElevationToModelLayerFunction.IFunctionAddr := _ElevationToModelLayer;
  SetLength(ElevationToModelLayerFunction.InputDataTypes, 1);
  ElevationToModelLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToModelLayerFunction.OptionalArguments := 0;
  ElevationToModelLayerFunction.CanConvertToConstant := False;
  ElevationToModelLayerFunction.Name := 'ElevationToModelLayer';
  ElevationToModelLayerFunction.Prototype := StrGridOrMesh+'ElevationToModelLayer(Elevation)';

  ElevationToLayerFunction.ResultType := rdtInteger;
  ElevationToLayerFunction.IFunctionAddr := _ElevationToLayer;
  SetLength(ElevationToLayerFunction.InputDataTypes, 1);
  ElevationToLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToLayerFunction.OptionalArguments := 0;
  ElevationToLayerFunction.CanConvertToConstant := False;
  ElevationToLayerFunction.Name := 'ElevationToLayer';
  ElevationToLayerFunction.Prototype := StrGridOrMesh+'ElevationToLayer(Elevation)';

  RowFunction.ResultType := rdtInteger;
  RowFunction.IFunctionAddr := _Row;
  SetLength(RowFunction.InputDataTypes, 0);
  RowFunction.OptionalArguments := 0;
  RowFunction.CanConvertToConstant := False;
  RowFunction.Name := 'Row';
  RowFunction.Prototype := StrGridOrMesh+'Row';

  LayerFunction.ResultType := rdtInteger;
  LayerFunction.IFunctionAddr := _Layer;
  SetLength(LayerFunction.InputDataTypes, 0);
  LayerFunction.OptionalArguments := 0;
  LayerFunction.CanConvertToConstant := False;
  LayerFunction.Name := 'Layer';
  LayerFunction.Prototype := StrGridOrMesh+'Layer';

  ColumnWidthFunction.ResultType := rdtDouble;
  ColumnWidthFunction.RFunctionAddr := _ColumnWidth;
  SetLength(ColumnWidthFunction.InputDataTypes, 1);
  ColumnWidthFunction.InputDataTypes[0] := rdtInteger;
  ColumnWidthFunction.OptionalArguments := 1;
  ColumnWidthFunction.CanConvertToConstant := False;
  ColumnWidthFunction.Name := 'ColumnWidth';
  ColumnWidthFunction.Prototype := StrGridOrMesh+'ColumnWidth({Column})';

  RowWidthFunction.ResultType := rdtDouble;
  RowWidthFunction.RFunctionAddr := _RowWidth;
  SetLength(RowWidthFunction.InputDataTypes, 1);
  RowWidthFunction.InputDataTypes[0] := rdtInteger;
  RowWidthFunction.OptionalArguments := 1;
  RowWidthFunction.CanConvertToConstant := False;
  RowWidthFunction.Name := 'RowWidth';
  RowWidthFunction.Prototype := StrGridOrMesh+'RowWidth({Row})';

  LayerHeightFunction.ResultType := rdtDouble;
  LayerHeightFunction.RFunctionAddr := _LayerHeight;
  SetLength(LayerHeightFunction.InputDataTypes, 3);
  LayerHeightFunction.InputDataTypes[0] := rdtInteger;
  LayerHeightFunction.InputDataTypes[1] := rdtInteger;
  LayerHeightFunction.InputDataTypes[2] := rdtInteger;
  LayerHeightFunction.OptionalArguments := 3;
  LayerHeightFunction.CanConvertToConstant := False;
  LayerHeightFunction.Name := StrLayerHeight;
  LayerHeightFunction.Prototype := StrGridOrMesh+''+StrLayerHeight+'({{Col, Row,} Layer})';

  BlockAreaTopFunction.ResultType := rdtDouble;
  BlockAreaTopFunction.RFunctionAddr := _BlockAreaTop;
  SetLength(BlockAreaTopFunction.InputDataTypes, 2);
  BlockAreaTopFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaTopFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaTopFunction.OptionalArguments := 2;
  BlockAreaTopFunction.CanConvertToConstant := False;
  BlockAreaTopFunction.Name := 'BlockAreaTop';
  BlockAreaTopFunction.Prototype := StrGridOrMesh+'BlockAreaTop({Column, Row})';

  BlockAreaFrontFunction.ResultType := rdtDouble;
  BlockAreaFrontFunction.RFunctionAddr := _BlockAreaFront;
  SetLength(BlockAreaFrontFunction.InputDataTypes, 3);
  BlockAreaFrontFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaFrontFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaFrontFunction.InputDataTypes[2] := rdtInteger;
  BlockAreaFrontFunction.OptionalArguments := 3;
  BlockAreaFrontFunction.CanConvertToConstant := False;
  BlockAreaFrontFunction.Name := 'BlockAreaFront';
  BlockAreaFrontFunction.Prototype :=
    StrGridOrMesh+'BlockAreaFront({Column, {Row,} Layer})';

  BlockAreaSideFunction.ResultType := rdtDouble;
  BlockAreaSideFunction.RFunctionAddr := _BlockAreaSide;
  SetLength(BlockAreaSideFunction.InputDataTypes, 3);
  BlockAreaSideFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[2] := rdtInteger;
  BlockAreaSideFunction.OptionalArguments := 3;
  BlockAreaSideFunction.CanConvertToConstant := False;
  BlockAreaSideFunction.Name := 'BlockAreaSide';
  BlockAreaSideFunction.Prototype := StrGridOrMesh+'BlockAreaSide({{Col,} Row, Layer})';

  BlockVolumeFunction.ResultType := rdtDouble;
  BlockVolumeFunction.RFunctionAddr := _BlockVolume;
  SetLength(BlockVolumeFunction.InputDataTypes, 3);
  BlockVolumeFunction.InputDataTypes[0] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[1] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[2] := rdtInteger;
  BlockVolumeFunction.OptionalArguments := 3;
  BlockVolumeFunction.CanConvertToConstant := False;
  BlockVolumeFunction.Name := 'BlockVolume';
  BlockVolumeFunction.Prototype := StrGridOrMesh+'BlockVolume({{Column, Row,} Layer})';

  ColumnPositionFunction.ResultType := rdtDouble;
  ColumnPositionFunction.RFunctionAddr := _ColumnPosition;
  SetLength(ColumnPositionFunction.InputDataTypes, 1);
  ColumnPositionFunction.InputDataTypes[0] := rdtInteger;
  ColumnPositionFunction.OptionalArguments := 1;
  ColumnPositionFunction.CanConvertToConstant := False;
  ColumnPositionFunction.Name := 'ColumnBoundaryPosition';
  ColumnPositionFunction.Prototype := StrGridOrMesh+'ColumnBoundaryPosition({Column})';

  RowPositionFunction.ResultType := rdtDouble;
  RowPositionFunction.RFunctionAddr := _RowPosition;
  SetLength(RowPositionFunction.InputDataTypes, 1);
  RowPositionFunction.InputDataTypes[0] := rdtInteger;
  RowPositionFunction.OptionalArguments := 1;
  RowPositionFunction.CanConvertToConstant := False;
  RowPositionFunction.Name := 'RowBoundaryPosition';
  RowPositionFunction.Prototype := StrGridOrMesh+'RowBoundaryPosition({Row})';

  LayerPositionFunction.ResultType := rdtDouble;
  LayerPositionFunction.RFunctionAddr := _LayerPosition;
  SetLength(LayerPositionFunction.InputDataTypes, 3);
  LayerPositionFunction.InputDataTypes[0] := rdtInteger;
  LayerPositionFunction.InputDataTypes[1] := rdtInteger;
  LayerPositionFunction.InputDataTypes[2] := rdtInteger;
  LayerPositionFunction.OptionalArguments := 3;
  LayerPositionFunction.CanConvertToConstant := False;
  LayerPositionFunction.Name := StrLayerBoundaryPosition;
  LayerPositionFunction.Prototype :=
    StrGridOrMesh+'' + StrLayerBoundaryPosition + '({{Column, Row,} Layer})';

  LayerCenterFunction.ResultType := rdtDouble;
  LayerCenterFunction.RFunctionAddr := _LayerCenter;
  SetLength(LayerCenterFunction.InputDataTypes, 3);
  LayerCenterFunction.InputDataTypes[0] := rdtInteger;
  LayerCenterFunction.InputDataTypes[1] := rdtInteger;
  LayerCenterFunction.InputDataTypes[2] := rdtInteger;
  LayerCenterFunction.OptionalArguments := 3;
  LayerCenterFunction.CanConvertToConstant := False;
  LayerCenterFunction.Name := 'LayerCenter';
  LayerCenterFunction.Prototype :=
    StrGridOrMesh+'LayerCenter({{Column, Row,} Layer})';

  ColumnCenterFunction.ResultType := rdtDouble;
  ColumnCenterFunction.RFunctionAddr := _ColumnCenter;
  SetLength(ColumnCenterFunction.InputDataTypes, 1);
  ColumnCenterFunction.InputDataTypes[0] := rdtInteger;
  ColumnCenterFunction.OptionalArguments := 1;
  ColumnCenterFunction.CanConvertToConstant := False;
  ColumnCenterFunction.Name := 'ColumnCenter';
  ColumnCenterFunction.Prototype :=
    StrGridOrMesh+'ColumnCenter({Column})';

  RowCenterFunction.ResultType := rdtDouble;
  RowCenterFunction.RFunctionAddr := _RowCenter;
  SetLength(RowCenterFunction.InputDataTypes, 1);
  RowCenterFunction.InputDataTypes[0] := rdtInteger;
  RowCenterFunction.OptionalArguments := 1;
  RowCenterFunction.CanConvertToConstant := False;
  RowCenterFunction.Name := 'RowCenter';
  RowCenterFunction.Prototype :=
    StrGridOrMesh+'RowCenter({Row})';

  ColumnCountFunction.ResultType := rdtInteger;
  ColumnCountFunction.IFunctionAddr := _ColumnCount;
  SetLength(ColumnCountFunction.InputDataTypes, 0);
  ColumnCountFunction.OptionalArguments := 0;
  ColumnCountFunction.CanConvertToConstant := False;
  ColumnCountFunction.Name := 'ColumnCount';
  ColumnCountFunction.Prototype := StrGridOrMesh+'ColumnCount';

  RowCountFunction.ResultType := rdtInteger;
  RowCountFunction.IFunctionAddr := _RowCount;
  SetLength(RowCountFunction.InputDataTypes, 0);
  RowCountFunction.OptionalArguments := 0;
  RowCountFunction.CanConvertToConstant := False;
  RowCountFunction.Name := 'RowCount';
  RowCountFunction.Prototype := StrGridOrMesh+'RowCount';

  LayerCountFunction.ResultType := rdtInteger;
  LayerCountFunction.IFunctionAddr := _LayerCount;
  SetLength(LayerCountFunction.InputDataTypes, 0);
  LayerCountFunction.OptionalArguments := 0;
  LayerCountFunction.CanConvertToConstant := False;
  LayerCountFunction.Name := 'LayerCount';
  LayerCountFunction.Prototype := StrGridOrMesh+'LayerCount';

  ObjectLengthFunction.ResultType := rdtDouble;
  ObjectLengthFunction.RFunctionAddr := _ObjectLength;
  SetLength(ObjectLengthFunction.InputDataTypes, 0);
  ObjectLengthFunction.OptionalArguments := 0;
  ObjectLengthFunction.CanConvertToConstant := False;
  ObjectLengthFunction.Name := StrObjectLength;
  ObjectLengthFunction.Prototype := StrObject+'ObjectLength';

  ObjectAreaFunction.ResultType := rdtDouble;
  ObjectAreaFunction.RFunctionAddr := _ObjectArea;
  SetLength(ObjectAreaFunction.InputDataTypes, 0);
  ObjectAreaFunction.OptionalArguments := 0;
  ObjectAreaFunction.CanConvertToConstant := False;
  ObjectAreaFunction.Name := StrObjectArea;
  ObjectAreaFunction.Prototype := StrObject+'ObjectArea';

  ObjectIntersectLengthFunction.ResultType := rdtDouble;
  ObjectIntersectLengthFunction.RFunctionAddr := _ObjectIntersectLength;
  SetLength(ObjectIntersectLengthFunction.InputDataTypes, 3);
  ObjectIntersectLengthFunction.InputDataTypes[0] := rdtInteger;
  ObjectIntersectLengthFunction.InputDataTypes[1] := rdtInteger;
  ObjectIntersectLengthFunction.InputDataTypes[2] := rdtInteger;
  ObjectIntersectLengthFunction.OptionalArguments := 3;
  ObjectIntersectLengthFunction.CanConvertToConstant := False;
  ObjectIntersectLengthFunction.Name := StrObjectIntersectLength;
  ObjectIntersectLengthFunction.Prototype :=
    StrObject+'ObjectIntersectLength({Column, Row, Layer})';

  ObjectSectionIntersectLengthFunction.ResultType := rdtDouble;
  ObjectSectionIntersectLengthFunction.RFunctionAddr := _ObjectIntersectSectionLength;
  SetLength(ObjectSectionIntersectLengthFunction.InputDataTypes, 4);
  ObjectSectionIntersectLengthFunction.InputDataTypes[0] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[1] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[2] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[3] := rdtInteger;
  ObjectSectionIntersectLengthFunction.OptionalArguments := 4;
  ObjectSectionIntersectLengthFunction.CanConvertToConstant := False;
  ObjectSectionIntersectLengthFunction.Name := StrObjectSectionIntersectLength;
  ObjectSectionIntersectLengthFunction.Prototype :=
    StrObject+'ObjectSectionIntersectLength({Section, Column, Row, Layer})';

  ObjectIntersectAreaFunction.ResultType := rdtDouble;
  ObjectIntersectAreaFunction.RFunctionAddr := _ObjectIntersectArea;
  SetLength(ObjectIntersectAreaFunction.InputDataTypes, 3);
  ObjectIntersectAreaFunction.InputDataTypes[0] := rdtInteger;
  ObjectIntersectAreaFunction.InputDataTypes[1] := rdtInteger;
  ObjectIntersectAreaFunction.InputDataTypes[2] := rdtInteger;
  ObjectIntersectAreaFunction.OptionalArguments := 3;
  ObjectIntersectAreaFunction.CanConvertToConstant := False;
  ObjectIntersectAreaFunction.Name := StrObjectIntersectArea;
  ObjectIntersectAreaFunction.Prototype :=
    StrObject+'ObjectIntersectArea({Column, Row, Layer})';

  ObjectNameFunction.ResultType := rdtString;
  ObjectNameFunction.SFunctionAddr := _ObjectName;
  SetLength(ObjectNameFunction.InputDataTypes, 0);
  ObjectNameFunction.OptionalArguments := 0;
  ObjectNameFunction.CanConvertToConstant := False;
  ObjectNameFunction.Name := 'ObjectName';
  ObjectNameFunction.Prototype :=
    StrObject+'ObjectName';

  NodeXFunction.ResultType := rdtDouble;
  NodeXFunction.RFunctionAddr := _XNodePosition;
  SetLength(NodeXFunction.InputDataTypes, 1);
  NodeXFunction.InputDataTypes[0] := rdtInteger;
  NodeXFunction.OptionalArguments := 0;
  NodeXFunction.CanConvertToConstant := False;
  NodeXFunction.Name := 'ObjectVertexX';
  NodeXFunction.Prototype := StrObject+'ObjectVertexX(VertexIndex)';
  SetLength(NodeXFunction.Synonyms, 1);
  NodeXFunction.Synonyms[0] := 'ObjectNodeX';

  NodeYFunction.ResultType := rdtDouble;
  NodeYFunction.RFunctionAddr := _YNodePosition;
  SetLength(NodeYFunction.InputDataTypes, 1);
  NodeYFunction.InputDataTypes[0] := rdtInteger;
  NodeYFunction.OptionalArguments := 0;
  NodeYFunction.CanConvertToConstant := False;
  NodeYFunction.Name := 'ObjectVertexY';
  NodeYFunction.Prototype := StrObject+'ObjectVertexY(VertexIndex)';
  SetLength(NodeYFunction.Synonyms, 1);
  NodeYFunction.Synonyms[0] := 'ObjectNodeY';

  NodeZFunction.ResultType := rdtDouble;
  NodeZFunction.RFunctionAddr := _ZNodePosition;
  SetLength(NodeZFunction.InputDataTypes, 1);
  NodeZFunction.InputDataTypes[0] := rdtInteger;
  NodeZFunction.OptionalArguments := 0;
  NodeZFunction.CanConvertToConstant := False;
  NodeZFunction.Name := 'ObjectVertexZ';
  NodeZFunction.Prototype := StrObject+'ObjectVertexZ(VertexIndex)';
  SetLength(NodeZFunction.Synonyms, 1);
  NodeZFunction.Synonyms[0] := 'ObjectNodeZ';



  NodeDistanceFunction.ResultType := rdtDouble;
  NodeDistanceFunction.RFunctionAddr := _NodeDistances;
  SetLength(NodeDistanceFunction.InputDataTypes, 1);
  NodeDistanceFunction.InputDataTypes[0] := rdtInteger;
  NodeDistanceFunction.OptionalArguments := 0;
  NodeDistanceFunction.CanConvertToConstant := False;
  NodeDistanceFunction.Name := 'ObjectVertexDistance';
  NodeDistanceFunction.Prototype := StrObject+'ObjectVertexDistance(VertexIndex)';
  SetLength(NodeDistanceFunction.Synonyms, 1);
  NodeDistanceFunction.Synonyms[0] := 'ObjectNodeDistance';

  CurrentNodeXFunction.ResultType := rdtDouble;
  CurrentNodeXFunction.RFunctionAddr := _CurrentXNodePosition;
  CurrentNodeXFunction.OptionalArguments := 0;
  CurrentNodeXFunction.CanConvertToConstant := False;
  CurrentNodeXFunction.Name := ObjectCurrentVertexX;
  CurrentNodeXFunction.Prototype := StrObject+'' + ObjectCurrentVertexX;
  SetLength(CurrentNodeXFunction.Synonyms, 1);
  CurrentNodeXFunction.Synonyms[0] := 'ObjectCurrentNodeX';

  CurrentNodeYFunction.ResultType := rdtDouble;
  CurrentNodeYFunction.RFunctionAddr := _CurrentYNodePosition;
  CurrentNodeYFunction.OptionalArguments := 0;
  CurrentNodeYFunction.CanConvertToConstant := False;
  CurrentNodeYFunction.Name := ObjectCurrentVertexY;
  CurrentNodeYFunction.Prototype := StrObject+'' + ObjectCurrentVertexY;
  SetLength(CurrentNodeYFunction.Synonyms, 1);
  CurrentNodeYFunction.Synonyms[0] := 'ObjectCurrentNodeY';

  CurrentNodeZFunction.ResultType := rdtDouble;
  CurrentNodeZFunction.RFunctionAddr := _CurrentZNodePosition;
  CurrentNodeZFunction.OptionalArguments := 0;
  CurrentNodeZFunction.CanConvertToConstant := False;
  CurrentNodeZFunction.Name := ObjectCurrentVertexZ;
  CurrentNodeZFunction.Prototype := StrObject+'' + ObjectCurrentVertexZ;
  SetLength(CurrentNodeZFunction.Synonyms, 1);
  CurrentNodeZFunction.Synonyms[0] := 'ObjectCurrentNodeZ';

  CurrentSegmentAngleFunction.ResultType := rdtDouble;
  CurrentSegmentAngleFunction.RFunctionAddr := _CurrentSegmentAngle;
  CurrentSegmentAngleFunction.OptionalArguments := 0;
  CurrentSegmentAngleFunction.CanConvertToConstant := False;
  CurrentSegmentAngleFunction.Name := ObjectCurrentSegmentAngle;
  CurrentSegmentAngleFunction.Prototype := StrObject+''+ObjectCurrentSegmentAngle;

  CurrentSegmentAngleDegreesFunction.ResultType := rdtDouble;
  CurrentSegmentAngleDegreesFunction.RFunctionAddr := _CurrentSegmentAngleDegrees;
  CurrentSegmentAngleDegreesFunction.OptionalArguments := 0;
  CurrentSegmentAngleDegreesFunction.CanConvertToConstant := False;
  CurrentSegmentAngleDegreesFunction.Name :=ObjectDegrees;
  CurrentSegmentAngleDegreesFunction.Prototype := StrObject+'' + ObjectDegrees;

  CurrentSegmentAngleLimitedegreesFunction.ResultType := rdtDouble;
  CurrentSegmentAngleLimitedegreesFunction.RFunctionAddr := _CurrentSegmentAngleLimitedDegrees;
  CurrentSegmentAngleLimitedegreesFunction.OptionalArguments := 0;
  CurrentSegmentAngleLimitedegreesFunction.CanConvertToConstant := False;
  CurrentSegmentAngleLimitedegreesFunction.Name := ObjectDegreesLimited;
  CurrentSegmentAngleLimitedegreesFunction.Prototype := StrObject+''+ObjectDegreesLimited;

  CurrentSegmentLengthFunction.ResultType := rdtDouble;
  CurrentSegmentLengthFunction.RFunctionAddr := _CurrentSegmentLength;
  CurrentSegmentLengthFunction.OptionalArguments := 0;
  CurrentSegmentLengthFunction.CanConvertToConstant := False;
  CurrentSegmentLengthFunction.Name := ObjectCurSegLength;
  CurrentSegmentLengthFunction.Prototype := StrObject+'' + ObjectCurSegLength;

  CurrentSectionIndexFunction.ResultType := rdtInteger;
  CurrentSectionIndexFunction.IFunctionAddr := _CurrentSectionIndex;
  CurrentSectionIndexFunction.OptionalArguments := 0;
  CurrentSectionIndexFunction.CanConvertToConstant := False;
  CurrentSectionIndexFunction.Name := 'ObjectCurrentSectionIndex';
  CurrentSectionIndexFunction.Prototype := StrObject+'ObjectCurrentSectionIndex';

  FractionOfObjectLengthFunction.ResultType := rdtDouble;
  FractionOfObjectLengthFunction.RFunctionAddr := _FractionOfObjectLength;
  FractionOfObjectLengthFunction.OptionalArguments := 0;
  FractionOfObjectLengthFunction.CanConvertToConstant := False;
  FractionOfObjectLengthFunction.Name := 'FractionOfObjectLength';
  FractionOfObjectLengthFunction.Prototype := StrObject+'FractionOfObjectLength';

  InterpolationedValuesFunction.ResultType := rdtDouble;
  InterpolationedValuesFunction.RFunctionAddr := _InterpolatedVertexValues;
  SetLength(InterpolationedValuesFunction.InputDataTypes, 1);
  InterpolationedValuesFunction.InputDataTypes[0] := rdtString;
  InterpolationedValuesFunction.OptionalArguments := 0;
  InterpolationedValuesFunction.CanConvertToConstant := False;
  InterpolationedValuesFunction.Name := StrInterpolatedVertexValues;
  InterpolationedValuesFunction.Prototype := StrObjectVertexValue+''
    + StrInterpolatedVertexValues + '(Key)';
  InterpolationedValuesFunction.Hidden := False;


  ObjectBasisFunction_Function.ResultType := rdtDouble;
  ObjectBasisFunction_Function.RFunctionAddr := _ObjectFiniteElementBasis;
  SetLength(ObjectBasisFunction_Function.InputDataTypes, 2);
  ObjectBasisFunction_Function.InputDataTypes[0] := rdtString;
  ObjectBasisFunction_Function.InputDataTypes[1] := rdtDouble;
  ObjectBasisFunction_Function.OptionalArguments := -1;
  ObjectBasisFunction_Function.CanConvertToConstant := False;
  ObjectBasisFunction_Function.Name := StrObjectBasisFunction;
  ObjectBasisFunction_Function.Prototype := StrObjectVertexValue+''
    + StrObjectBasisFunction + '(Key, [Default_Value])';
  ObjectBasisFunction_Function.Hidden := False;

  LowestVertexValuesFunction.ResultType := rdtDouble;
  LowestVertexValuesFunction.RFunctionAddr := _LowestVertexValue;
  SetLength(LowestVertexValuesFunction.InputDataTypes, 2);
  LowestVertexValuesFunction.InputDataTypes[0] := rdtString;
  LowestVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  LowestVertexValuesFunction.OptionalArguments := 1;
  LowestVertexValuesFunction.CanConvertToConstant := False;
  LowestVertexValuesFunction.Name := StrLowestVertexValue;
  LowestVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrLowestVertexValue + '(Key [, DefaultValue])';
  LowestVertexValuesFunction.Hidden := False;

  HighestVertexValuesFunction.ResultType := rdtDouble;
  HighestVertexValuesFunction.RFunctionAddr := _HighestVertexValue;
  SetLength(HighestVertexValuesFunction.InputDataTypes, 2);
  HighestVertexValuesFunction.InputDataTypes[0] := rdtString;
  HighestVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  HighestVertexValuesFunction.OptionalArguments := 1;
  HighestVertexValuesFunction.CanConvertToConstant := False;
  HighestVertexValuesFunction.Name := StrHighestVertexValue;
  HighestVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrHighestVertexValue + '(Key [, DefaultValue])';
  HighestVertexValuesFunction.Hidden := False;

  MeanVertexValuesFunction.ResultType := rdtDouble;
  MeanVertexValuesFunction.RFunctionAddr := _MeanVertexValue;
  SetLength(MeanVertexValuesFunction.InputDataTypes, 2);
  MeanVertexValuesFunction.InputDataTypes[0] := rdtString;
  MeanVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  MeanVertexValuesFunction.OptionalArguments := 1;
  MeanVertexValuesFunction.CanConvertToConstant := False;
  MeanVertexValuesFunction.Name := StrMeanVertexValue;
  MeanVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrMeanVertexValue + '(Key [, DefaultValue])';
  MeanVertexValuesFunction.Hidden := False;

  MedianVertexValuesFunction.ResultType := rdtDouble;
  MedianVertexValuesFunction.RFunctionAddr := _MedianVertexValue;
  SetLength(MedianVertexValuesFunction.InputDataTypes, 2);
  MedianVertexValuesFunction.InputDataTypes[0] := rdtString;
  MedianVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  MedianVertexValuesFunction.OptionalArguments := 1;
  MedianVertexValuesFunction.CanConvertToConstant := False;
  MedianVertexValuesFunction.Name := StrMedianVertexValue;
  MedianVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrMedianVertexValue + '(Key [, DefaultValue])';
  MedianVertexValuesFunction.Hidden := False;

  FirstVertexValuesFunction.ResultType := rdtDouble;
  FirstVertexValuesFunction.RFunctionAddr := _FirstVertexValue;
  SetLength(FirstVertexValuesFunction.InputDataTypes, 2);
  FirstVertexValuesFunction.InputDataTypes[0] := rdtString;
  FirstVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  FirstVertexValuesFunction.OptionalArguments := 1;
  FirstVertexValuesFunction.CanConvertToConstant := False;
  FirstVertexValuesFunction.Name := StrFirstVertexValue;
  FirstVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrFirstVertexValue + '(Key [, DefaultValue])';
  FirstVertexValuesFunction.Hidden := False;

  LastVertexValuesFunction.ResultType := rdtDouble;
  LastVertexValuesFunction.RFunctionAddr := _LastVertexValue;
  SetLength(LastVertexValuesFunction.InputDataTypes, 2);
  LastVertexValuesFunction.InputDataTypes[0] := rdtString;
  LastVertexValuesFunction.InputDataTypes[1] := rdtDouble;
  LastVertexValuesFunction.OptionalArguments := 1;
  LastVertexValuesFunction.CanConvertToConstant := False;
  LastVertexValuesFunction.Name := StrLastVertexValue;
  LastVertexValuesFunction.Prototype := StrObjectVertexValue+''
    + StrLastVertexValue + '(Key [, DefaultValue])';
  LastVertexValuesFunction.Hidden := False;

//  VertexValueSlopeFunction.ResultType := rdtDouble;
//  VertexValueSlopeFunction.RFunctionAddr := _VertexValueSlope;
//  SetLength(VertexValueSlopeFunction.InputDataTypes, 2);
//  VertexValueSlopeFunction.InputDataTypes[0] := rdtString;
//  VertexValueSlopeFunction.InputDataTypes[1] := rdtDouble;
//  VertexValueSlopeFunction.OptionalArguments := 1;
//  VertexValueSlopeFunction.CanConvertToConstant := False;
//  VertexValueSlopeFunction.Name := StrVertexValueSlope;
//  VertexValueSlopeFunction.Prototype := StrObject+''
//    + StrVertexValueSlope + '(Key [, DefaultValue])';
//  VertexValueSlopeFunction.Hidden := False;

  RasterValueAlongObject.ResultType := rdtDouble;
  RasterValueAlongObject.RFunctionAddr := _ValueAlongSegment;
  SetLength(RasterValueAlongObject.InputDataTypes, 1);
  RasterValueAlongObject.InputDataTypes[0] := rdtString;
  RasterValueAlongObject.OptionalArguments := 0;
  RasterValueAlongObject.CanConvertToConstant := False;
  RasterValueAlongObject.Name := StrRasterValueAlongObject;
  RasterValueAlongObject.Prototype := StrObject+''
    + StrRasterValueAlongObject + '(RasterName)';
{$IFDEF LinkedRasters}
  RasterValueAlongObject.Hidden := False;
{$ELSE}
  RasterValueAlongObject.Hidden := True;
{$ENDIF}

  RasterSlopeAlongObject.ResultType := rdtDouble;
  RasterSlopeAlongObject.RFunctionAddr := _SlopeAlongSegment;
  SetLength(RasterSlopeAlongObject.InputDataTypes, 1);
  RasterSlopeAlongObject.InputDataTypes[0] := rdtString;
  RasterSlopeAlongObject.OptionalArguments := 0;
  RasterSlopeAlongObject.CanConvertToConstant := False;
  RasterSlopeAlongObject.Name := StrRasterSlopeAlongObject;
  RasterSlopeAlongObject.Prototype := StrObject+''
    + StrRasterSlopeAlongObject + '(RasterName)';
{$IFDEF LinkedRasters}
  RasterSlopeAlongObject.Hidden := False;
{$ELSE}
  RasterSlopeAlongObject.Hidden := True;
{$ENDIF}


  VertexValuesFunction.ResultType := rdtDouble;
  VertexValuesFunction.RFunctionAddr := _VertexValue;
  SetLength(VertexValuesFunction.InputDataTypes, 2);
  VertexValuesFunction.InputDataTypes[0] := rdtString;
  VertexValuesFunction.InputDataTypes[1] := rdtDouble;
  VertexValuesFunction.OptionalArguments := 0;
  VertexValuesFunction.CanConvertToConstant := False;
  VertexValuesFunction.Name := 'VertexValue';
  VertexValuesFunction.Prototype := StrObjectVertexValue+''
    + 'VertexValue' + '(Key, DefaultValue)';
  VertexValuesFunction.Hidden := False;

  NodeCountFunction.ResultType := rdtInteger;
  NodeCountFunction.IFunctionAddr := _ObjectNodeCount;
  NodeCountFunction.OptionalArguments := 0;
  NodeCountFunction.CanConvertToConstant := False;
  NodeCountFunction.Name := 'ObjectVertexCount';
  NodeCountFunction.Prototype := StrObject+'ObjectVertexCount';
  SetLength(NodeCountFunction.Synonyms, 1);
  NodeCountFunction.Synonyms[0] := 'ObjectNodeCount';

  ImportedValuesRFunction.ResultType := rdtDouble;
  ImportedValuesRFunction.RFunctionAddr := _ImportedScreenObjectValuesR;
  SetLength(ImportedValuesRFunction.InputDataTypes, 1);
  ImportedValuesRFunction.InputDataTypes[0] := rdtString;
  ImportedValuesRFunction.OptionalArguments := 1;
  ImportedValuesRFunction.CanConvertToConstant := False;
  ImportedValuesRFunction.Name := rsObjectImportedValuesR;
  ImportedValuesRFunction.Prototype := StrObject+'' + rsObjectImportedValuesR
    + '({Key})';
  ImportedValuesRFunction.Hidden := False;

  ImportedValuesIFunction.ResultType := rdtInteger;
  ImportedValuesIFunction.IFunctionAddr := _ImportedScreenObjectValuesI;
  SetLength(ImportedValuesIFunction.InputDataTypes, 1);
  ImportedValuesIFunction.InputDataTypes[0] := rdtString;
  ImportedValuesIFunction.OptionalArguments := 1;
  ImportedValuesIFunction.CanConvertToConstant := False;
  ImportedValuesIFunction.Name := rsObjectImportedValuesI;
  ImportedValuesIFunction.Prototype := StrObject+'' + rsObjectImportedValuesI
    + '({Key})';
  ImportedValuesIFunction.Hidden := False;

  ImportedValuesBFunction.ResultType := rdtBoolean;
  ImportedValuesBFunction.BFunctionAddr := _ImportedScreenObjectValuesB;
  SetLength(ImportedValuesBFunction.InputDataTypes, 1);
  ImportedValuesBFunction.InputDataTypes[0] := rdtString;
  ImportedValuesBFunction.OptionalArguments := 1;
  ImportedValuesBFunction.CanConvertToConstant := False;
  ImportedValuesBFunction.Name := rsObjectImportedValuesB;
  ImportedValuesBFunction.Prototype := StrObject+'' + rsObjectImportedValuesB
    + '({Key})';
  ImportedValuesBFunction.Hidden := False;

  ImportedValuesTFunction.ResultType := rdtString;
  ImportedValuesTFunction.SFunctionAddr := _ImportedScreenObjectValuesT;
  SetLength(ImportedValuesTFunction.InputDataTypes, 1);
  ImportedValuesTFunction.InputDataTypes[0] := rdtString;
  ImportedValuesTFunction.OptionalArguments := 1;
  ImportedValuesTFunction.CanConvertToConstant := False;
  ImportedValuesTFunction.Name := rsObjectImportedValuesT;
  ImportedValuesTFunction.Prototype := StrObject+'' + rsObjectImportedValuesT
    + '({Key})';
  ImportedValuesTFunction.Hidden := False;

  ListRealValueFunction.ResultType := rdtDouble;
  ListRealValueFunction.RFunctionAddr := _ListDataSetRealValue;
  SetLength(ListRealValueFunction.InputDataTypes, 1);
  ListRealValueFunction.InputDataTypes[0] := rdtString;
  ListRealValueFunction.OptionalArguments := 0;
  ListRealValueFunction.CanConvertToConstant := False;
  ListRealValueFunction.Name := rsListRealValue;
  ListRealValueFunction.Prototype := StrObject+'' + rsListRealValue +
    '("DataSetName")';
  ListRealValueFunction.Hidden := True;

  ListIntegerValueFunction.ResultType := rdtInteger;
  ListIntegerValueFunction.IFunctionAddr := _ListDataSetIntegerValue;
  SetLength(ListIntegerValueFunction.InputDataTypes, 1);
  ListIntegerValueFunction.InputDataTypes[0] := rdtString;
  ListIntegerValueFunction.OptionalArguments := 0;
  ListIntegerValueFunction.CanConvertToConstant := False;
  ListIntegerValueFunction.Name := rsListIntegerValue;
  ListIntegerValueFunction.Prototype := StrObject+'' + rsListIntegerValue +
    '("DataSetName")';
  ListIntegerValueFunction.Hidden := True;

  ModflowLayerSimulatedFunction.ResultType := rdtBoolean;
  ModflowLayerSimulatedFunction.BFunctionAddr := _SimulatedModflowLayer;
  SetLength(ModflowLayerSimulatedFunction.InputDataTypes, 1);
  ModflowLayerSimulatedFunction.InputDataTypes[0] := rdtInteger;
  ModflowLayerSimulatedFunction.OptionalArguments := 1;
  ModflowLayerSimulatedFunction.CanConvertToConstant := False;
  ModflowLayerSimulatedFunction.Name := 'SimulatedLayer';
  ModflowLayerSimulatedFunction.Prototype := StrMODFLOW+'SimulatedLayer({Layer})';

  ModflowLayerConfinedFunction.ResultType := rdtBoolean;
  ModflowLayerConfinedFunction.BFunctionAddr := _ConfinedModflowLayer;
  SetLength(ModflowLayerConfinedFunction.InputDataTypes, 1);
  ModflowLayerConfinedFunction.InputDataTypes[0] := rdtInteger;
  ModflowLayerConfinedFunction.OptionalArguments := 1;
  ModflowLayerConfinedFunction.CanConvertToConstant := False;
  ModflowLayerConfinedFunction.Name := 'ConfinedLayer';
  ModflowLayerConfinedFunction.Prototype := StrMODFLOW+'ConfinedLayer({Layer})';


  GridNumberFunction.ResultType := rdtInteger;
  GridNumberFunction.IFunctionAddr := _GridNumber;
  SetLength(GridNumberFunction.InputDataTypes, 0);
  GridNumberFunction.OptionalArguments := 0;
  GridNumberFunction.CanConvertToConstant := False;
  GridNumberFunction.Name := StrGridNumber;
  GridNumberFunction.Prototype := StrModflowLgr + StrGridNumber;
  GridNumberFunction.Hidden := False;

  GridNameFunction.ResultType := rdtString;
  GridNameFunction.SFunctionAddr := _GridName;
  SetLength(GridNameFunction.InputDataTypes, 0);
  GridNameFunction.OptionalArguments := 0;
  GridNameFunction.CanConvertToConstant := False;
  GridNameFunction.Name := StrGridName;
  GridNameFunction.Prototype := StrModflowLgr + StrGridName;
  GridNameFunction.Hidden := False;

  ParentLayerFunction.ResultType := rdtInteger;
  ParentLayerFunction.IFunctionAddr := _ParentLayer;
  SetLength(ParentLayerFunction.InputDataTypes, 0);
  ParentLayerFunction.OptionalArguments := 0;
  ParentLayerFunction.CanConvertToConstant := False;
  ParentLayerFunction.Name := StrParentLayer;
  ParentLayerFunction.Prototype := StrModflowLgr + StrParentLayer;
  ParentLayerFunction.Hidden := False;

  ParentRowFunction.ResultType := rdtInteger;
  ParentRowFunction.IFunctionAddr := _ParentRow;
  SetLength(ParentRowFunction.InputDataTypes, 0);
  ParentRowFunction.OptionalArguments := 0;
  ParentRowFunction.CanConvertToConstant := False;
  ParentRowFunction.Name := StrParentRow;
  ParentRowFunction.Prototype := StrModflowLgr + StrParentRow;
  ParentRowFunction.Hidden := False;

  ParentColumnFunction.ResultType := rdtInteger;
  ParentColumnFunction.IFunctionAddr := _ParentColumn;
  SetLength(ParentColumnFunction.InputDataTypes, 0);
  ParentColumnFunction.OptionalArguments := 0;
  ParentColumnFunction.CanConvertToConstant := False;
  ParentColumnFunction.Name := StrParentColumn;
  ParentColumnFunction.Prototype := StrModflowLgr + StrParentColumn;
  ParentColumnFunction.Hidden := False;

  HorizontalSubdivision_Function.ResultType := rdtInteger;
  HorizontalSubdivision_Function.IFunctionAddr := _HorizontalSubdivision;
  SetLength(HorizontalSubdivision_Function.InputDataTypes, 0);
  HorizontalSubdivision_Function.OptionalArguments := 0;
  HorizontalSubdivision_Function.CanConvertToConstant := False;
  HorizontalSubdivision_Function.Name := StrHorizontalSubdivision;
  HorizontalSubdivision_Function.Prototype := StrModflowLgr + StrHorizontalSubdivision;
  HorizontalSubdivision_Function.Hidden := False;

  VerticalSubdivision_Function.ResultType := rdtInteger;
  VerticalSubdivision_Function.IFunctionAddr := _VerticalSubdivision;
  SetLength(VerticalSubdivision_Function.InputDataTypes, 1);
  VerticalSubdivision_Function.InputDataTypes[0] := rdtInteger;
  VerticalSubdivision_Function.OptionalArguments := 1;
  VerticalSubdivision_Function.CanConvertToConstant := False;
  VerticalSubdivision_Function.Name := StrVerticalSubdivision;
  VerticalSubdivision_Function.Prototype := StrModflowLgr + StrVerticalSubdivision + '({Layer})';
  VerticalSubdivision_Function.Hidden := False;

  SelectedCount_Function.ResultType := rdtInteger;
  SelectedCount_Function.IFunctionAddr := _SelectedCount;
  SetLength(SelectedCount_Function.InputDataTypes, 0);
  SelectedCount_Function.OptionalArguments := 0;
  SelectedCount_Function.CanConvertToConstant := False;
  SelectedCount_Function.Name := StrSelectedCount;
  SelectedCount_Function.Prototype := StrObject + StrSelectedCount;
  SelectedCount_Function.Hidden := False;

  NodeInterpolate := TFunctionClass.Create;
  NodeInterpolate.InputDataCount := 3;
  NodeInterpolate.OptionalArguments := -1;
  NodeInterpolate.RFunctionAddr := _NodeInterpolate;
  NodeInterpolate.Name := StrVertexInterpolate;
  NodeInterpolate.Prototype := StrObject+'' + StrVertexInterpolate + '(Value1, Value2, ...)';
  NodeInterpolate.Synonyms.Add(StrNodeInterpolate);
  NodeInterpolate.InputDataTypes[0] := rdtDouble;
  NodeInterpolate.InputDataTypes[1] := rdtDouble;
  NodeInterpolate.InputDataTypes[2] := rdtDouble;
  NodeInterpolate.AllowConversionToConstant := False;

  NodeInterpolateSpecialImplementor := TSpecialImplementor.Create;
  NodeInterpolateSpecialImplementor.FunctionClass := NodeInterpolate;
  NodeInterpolateSpecialImplementor.Implementor := TNodeInterpolateExpression;
  SpecialImplementors.Add(NodeInterpolateSpecialImplementor);

  ActiveOnLayer := TFunctionClass.Create;
  ActiveOnLayer.InputDataCount := 1;
  ActiveOnLayer.OptionalArguments := 0;
  ActiveOnLayer.BFunctionAddr := _ActiveOnLayer;
  ActiveOnLayer.Name := 'ActiveOnLayer';
  ActiveOnLayer.Prototype := StrGridOrMesh+'ActiveOnLayer(Layer)';
  ActiveOnLayer.InputDataTypes[0] := rdtInteger;
  ActiveOnLayer.AllowConversionToConstant := False;

  ActiveOnLayerSpecialImplementor := TSpecialImplementor.Create;
  ActiveOnLayerSpecialImplementor.FunctionClass := ActiveOnLayer;
  ActiveOnLayerSpecialImplementor.Implementor := TActiveOnLayer;
  SpecialImplementors.Add(ActiveOnLayerSpecialImplementor);

  {
  // these functions don't seem to help improve results with XT3D

  SlopeDirectionRadians := TFunctionClass.Create;
  SlopeDirectionRadians.InputDataCount := 0;
  SlopeDirectionRadians.OptionalArguments := 0;
  SlopeDirectionRadians.RFunctionAddr := _SlopeDirectionRadians;
  SlopeDirectionRadians.Name := 'SlopeDirectionRadians';
  SlopeDirectionRadians.Prototype := StrGridOrMesh+'SlopeDirectionRadians';
  SlopeDirectionRadians.AllowConversionToConstant := False;

  SlopeDirectionRadiansSpecialImplementor := TSpecialImplementor.Create;
  SlopeDirectionRadiansSpecialImplementor.FunctionClass := SlopeDirectionRadians;
  SlopeDirectionRadiansSpecialImplementor.Implementor := TLayerSlope;
  SpecialImplementors.Add(SlopeDirectionRadiansSpecialImplementor);

  SlopeAngleRadians := TFunctionClass.Create;
  SlopeAngleRadians.InputDataCount := 0;
  SlopeAngleRadians.OptionalArguments := 0;
  SlopeAngleRadians.RFunctionAddr := _DipAngleRadians;
  SlopeAngleRadians.Name := 'SlopeAngleRadians';
  SlopeAngleRadians.Prototype := StrGridOrMesh+'SlopeAngleRadians';
  SlopeAngleRadians.AllowConversionToConstant := False;

  SlopeAngleRadiansSpecialImplementor := TSpecialImplementor.Create;
  SlopeAngleRadiansSpecialImplementor.FunctionClass := SlopeAngleRadians;
  SlopeAngleRadiansSpecialImplementor.Implementor := TLayerSlope;
  SpecialImplementors.Add(SlopeAngleRadiansSpecialImplementor);


  BedPerpendicularAngle2Degrees := TFunctionClass.Create;
  BedPerpendicularAngle2Degrees.InputDataCount := 1;
  BedPerpendicularAngle2Degrees.OptionalArguments := 0;
  BedPerpendicularAngle2Degrees.RFunctionAddr := _BedPerpendicularAngle2Degrees;
  BedPerpendicularAngle2Degrees.Name := 'BedPerpendicularAngle2';
  BedPerpendicularAngle2Degrees.Prototype := StrGridOrMesh+'BedPerpendicularAngle2(Angle1Degrees)';
  BedPerpendicularAngle2Degrees.InputDataTypes[0] := rdtDouble;
  BedPerpendicularAngle2Degrees.AllowConversionToConstant := False;

  BedPerpendicularAngle2DegreesSpecialImplementor := TSpecialImplementor.Create;
  BedPerpendicularAngle2DegreesSpecialImplementor.FunctionClass := BedPerpendicularAngle2Degrees;
  BedPerpendicularAngle2DegreesSpecialImplementor.Implementor := TLayerSlope;
  SpecialImplementors.Add(BedPerpendicularAngle2DegreesSpecialImplementor);

  BedPerpendicularAngle3Degrees := TFunctionClass.Create;
  BedPerpendicularAngle3Degrees.InputDataCount := 1;
  BedPerpendicularAngle3Degrees.OptionalArguments := 0;
  BedPerpendicularAngle3Degrees.RFunctionAddr := _BedPerpendicularAngle3Degrees;
  BedPerpendicularAngle3Degrees.Name := 'BedPerpendicularAngle3';
  BedPerpendicularAngle3Degrees.Prototype := StrGridOrMesh+'BedPerpendicularAngle3(Angle1Degrees)';
  BedPerpendicularAngle3Degrees.InputDataTypes[0] := rdtDouble;
  BedPerpendicularAngle3Degrees.AllowConversionToConstant := False;

  BedPerpendicularAngle3DegreesSpecialImplementor := TSpecialImplementor.Create;
  BedPerpendicularAngle3DegreesSpecialImplementor.FunctionClass := BedPerpendicularAngle3Degrees;
  BedPerpendicularAngle3DegreesSpecialImplementor.Implementor := TLayerSlope;
  SpecialImplementors.Add(BedPerpendicularAngle3DegreesSpecialImplementor);

}


  SpecifiedHeadOnLayer := TFunctionClass.Create;
  SpecifiedHeadOnLayer.InputDataCount := 1;
  SpecifiedHeadOnLayer.OptionalArguments := 0;
  SpecifiedHeadOnLayer.BFunctionAddr := _SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayer.Name := 'SpecifiedHeadOnLayer';
  SpecifiedHeadOnLayer.Prototype := StrMODFLOW+'SpecifiedHeadOnLayer(Layer)';
  SpecifiedHeadOnLayer.InputDataTypes[0] := rdtInteger;
  SpecifiedHeadOnLayer.AllowConversionToConstant := False;

  SpecifiedHeadOnLayerSpecialImplementor := TSpecialImplementor.Create;
  SpecifiedHeadOnLayerSpecialImplementor.FunctionClass := SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayerSpecialImplementor.Implementor := TSpecifiedHeadOnLayer;
  SpecialImplementors.Add(SpecifiedHeadOnLayerSpecialImplementor);

  BcfVcont := TFunctionClass.Create;
  BcfVcont.InputDataCount := 0;
  BcfVcont.OptionalArguments := 3;
  BcfVcont.RFunctionAddr := _BcfGetVcont;
  BcfVcont.Name := StrBcfVCONT;
  BcfVcont.Prototype := StrMODFLOW+'' + StrBcfVCONT + '({Layer, Row, Column})';
  BcfVcont.OptionalType := rdtInteger;
  BcfVcont.AllowConversionToConstant := False;

  BcfVcontSpecialImplementor := TSpecialImplementor.Create;
  BcfVcontSpecialImplementor.FunctionClass := BcfVcont;
  BcfVcontSpecialImplementor.Implementor := TBcfVcont;
  SpecialImplementors.Add(BcfVcontSpecialImplementor);

  HufKx := TFunctionClass.Create;
  HufKx.InputDataCount := 4;
  HufKx.OptionalArguments := 3;
  HufKx.RFunctionAddr := _HufKx;
  HufKx.Name := StrHufKx;
  HufKx.Prototype := StrMODFLOW+'' + StrHufKx + '(Head, {Layer, Row, Column})';
  HufKx.OptionalType := rdtInteger;
  HufKx.AllowConversionToConstant := False;
  HufKx.InputDataTypes[0] := rdtDouble;
  HufKx.InputDataTypes[1] := rdtInteger;
  HufKx.InputDataTypes[2] := rdtInteger;
  HufKx.InputDataTypes[3] := rdtInteger;

  HufKxSpecialImplementor := TSpecialImplementor.Create;
  HufKxSpecialImplementor.FunctionClass := HufKx;
  HufKxSpecialImplementor.Implementor := THufKx;
  SpecialImplementors.Add(HufKxSpecialImplementor);

  HufKy := TFunctionClass.Create;
  HufKy.InputDataCount := 4;
  HufKy.OptionalArguments := 3;
  HufKy.RFunctionAddr := _HufAverageKY;
  HufKy.Name := StrHufKy;
  HufKy.Prototype := StrMODFLOW+'' + StrHufKy + '(Head, {Layer, Row, Column})';
  HufKy.OptionalType := rdtInteger;
  HufKy.AllowConversionToConstant := False;
  HufKy.InputDataTypes[0] := rdtDouble;
  HufKy.InputDataTypes[1] := rdtInteger;
  HufKy.InputDataTypes[2] := rdtInteger;
  HufKy.InputDataTypes[3] := rdtInteger;

  HufKySpecialImplementor := TSpecialImplementor.Create;
  HufKySpecialImplementor.FunctionClass := HufKy;
  HufKySpecialImplementor.Implementor := THufKy;
  SpecialImplementors.Add(HufKySpecialImplementor);

  HufKz := TFunctionClass.Create;
  HufKz.InputDataCount := 4;
  HufKz.OptionalArguments := 3;
  HufKz.RFunctionAddr := _HufKz;
  HufKz.Name := StrHufKz;
  HufKz.Prototype := StrMODFLOW+'' + StrHufKz + '(Head, {Layer, Row, Column})';
  HufKz.OptionalType := rdtInteger;
  HufKz.AllowConversionToConstant := False;
  HufKz.InputDataTypes[0] := rdtDouble;
  HufKz.InputDataTypes[1] := rdtInteger;
  HufKz.InputDataTypes[2] := rdtInteger;
  HufKz.InputDataTypes[3] := rdtInteger;

  HufKzSpecialImplementor := TSpecialImplementor.Create;
  HufKzSpecialImplementor.FunctionClass := HufKz;
  HufKzSpecialImplementor.Implementor := THufKz;
  SpecialImplementors.Add(HufKzSpecialImplementor);

  HufSS := TFunctionClass.Create;
  HufSS.InputDataCount := 4;
  HufSS.OptionalArguments := 3;
  HufSS.RFunctionAddr := _HufSS;
  HufSS.Name := StrHufSs;
  HufSS.Prototype := StrMODFLOW+'' + StrHufSs + '(Head, {Layer, Row, Column})';
  HufSS.OptionalType := rdtInteger;
  HufSS.AllowConversionToConstant := False;
  HufSS.InputDataTypes[0] := rdtDouble;
  HufSS.InputDataTypes[1] := rdtInteger;
  HufSS.InputDataTypes[2] := rdtInteger;
  HufSS.InputDataTypes[3] := rdtInteger;

  HufSSSpecialImplementor := TSpecialImplementor.Create;
  HufSSSpecialImplementor.FunctionClass := HufSS;
  HufSSSpecialImplementor.Implementor := THufSS;
  SpecialImplementors.Add(HufSSSpecialImplementor);

  HufAverageSY := TFunctionClass.Create;
  HufAverageSY.InputDataCount := 4;
  HufAverageSY.OptionalArguments := 3;
  HufAverageSY.RFunctionAddr := _HufAverageSY;
  HufAverageSY.Name := StrHufAverageSy;
  HufAverageSY.Prototype := StrMODFLOW+'' + StrHufAverageSy + '(Head, {Layer, Row, Column})';
  HufAverageSY.OptionalType := rdtInteger;
  HufAverageSY.AllowConversionToConstant := False;
  HufAverageSY.InputDataTypes[0] := rdtDouble;
  HufAverageSY.InputDataTypes[1] := rdtInteger;
  HufAverageSY.InputDataTypes[2] := rdtInteger;
  HufAverageSY.InputDataTypes[3] := rdtInteger;

  HufAverageSYSpecialImplementor := TSpecialImplementor.Create;
  HufAverageSYSpecialImplementor.FunctionClass := HufAverageSY;
  HufAverageSYSpecialImplementor.Implementor := THufSY;
  SpecialImplementors.Add(HufAverageSYSpecialImplementor);

  HufSY := TFunctionClass.Create;
  HufSY.InputDataCount := 4;
  HufSY.OptionalArguments := 3;
  HufSY.RFunctionAddr := _GetHufSy;
  HufSY.Name := StrHufSy;
  HufSY.Prototype := StrMODFLOW+'' + StrHufSy + '(Head, {Layer, Row, Column})';
  HufSY.OptionalType := rdtInteger;
  HufSY.AllowConversionToConstant := False;
  HufSY.InputDataTypes[0] := rdtDouble;
  HufSY.InputDataTypes[1] := rdtInteger;
  HufSY.InputDataTypes[2] := rdtInteger;
  HufSY.InputDataTypes[3] := rdtInteger;

  HufSYSpecialImplementor := TSpecialImplementor.Create;
  HufSYSpecialImplementor.FunctionClass := HufSY;
  HufSYSpecialImplementor.Implementor := THufSY;
  SpecialImplementors.Add(HufSYSpecialImplementor);

  HufSYTP := TFunctionClass.Create;
  HufSYTP.InputDataCount := 2;
  HufSYTP.OptionalArguments := 2;
  HufSYTP.RFunctionAddr := _GetHufSytp;
  HufSYTP.Name := StrHufSytp;
  HufSYTP.Prototype := StrMODFLOW+'' + StrHufSytp + '({Row, Column})';
  HufSYTP.OptionalType := rdtInteger;
  HufSYTP.AllowConversionToConstant := False;
  HufSYTP.InputDataTypes[0] := rdtInteger;
  HufSYTP.InputDataTypes[1] := rdtInteger;

  HufSYTPSpecialImplementor := TSpecialImplementor.Create;
  HufSYTPSpecialImplementor.FunctionClass := HufSYTP;
  HufSYTPSpecialImplementor.Implementor := THufSYTP;
  SpecialImplementors.Add(HufSYTPSpecialImplementor);

  InvalidNames := TStringList.Create;
  InvalidNames.Sorted := True;
  InvalidNames.CaseSensitive := False;

finalization
  InvalidNames.Free;

  NodeInterpolate.Free;
  NodeInterpolateSpecialImplementor.Free;

  ActiveOnLayer.Free;
  ActiveOnLayerSpecialImplementor.Free;

  {
  // these functions don't seem to help improve results with XT3D
  SlopeDirectionRadians.Free;
  SlopeDirectionRadiansSpecialImplementor.Free;

  SlopeAngleRadians.Free;
  SlopeAngleRadiansSpecialImplementor.Free;

  BedPerpendicularAngle2Degrees.Free;
  BedPerpendicularAngle2DegreesSpecialImplementor.Free;

  BedPerpendicularAngle3Degrees.Free;
  BedPerpendicularAngle3DegreesSpecialImplementor.Free;
  }

  SpecifiedHeadOnLayer.Free;
  SpecifiedHeadOnLayerSpecialImplementor.Free;

  BcfVcont.Free;
  BcfVcontSpecialImplementor.Free;

  HufKx.Free;
  HufKxSpecialImplementor.Free;

  HufKy.Free;
  HufKySpecialImplementor.Free;

  HufKz.Free;
  HufKzSpecialImplementor.Free;

  HufSS.Free;
  HufSSSpecialImplementor.Free;

  HufAverageSY.Free;
  HufAverageSYSpecialImplementor.Free;

  HufSY.Free;
  HufSYSpecialImplementor.Free;

  HufSYTP.Free;
  HufSYTPSpecialImplementor.Free;

  SpecialImplementors.Free;

end.

