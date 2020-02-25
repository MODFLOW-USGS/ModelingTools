unit Mt3dmsChemUnit;

interface

uses System.Types, Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes, IntListUnit,
  ModflowMnw2Unit;

const
  // Mass loading source
  ISSYTPE_Mass = 15;
  // constant-concentration cell
  ISSYTPE_ConstConc = -1;

  ISSTYPE_CHD = 1;
  ISSTYPE_WEL = 2;
  ISSTYPE_DRN = 3;
  ISSTYPE_RIV = 4;
  ISSTYPE_GHB = 5;
  ISSTYPE_RCH = 7;
  ISSTYPE_EVT = 8;
//  ISSTYPE_LKT = 18;
  ISSTYPE_STR = 21;
  ISSTYPE_RES = 22;
  ISSTYPE_FHB = 23;
  ISSTYPE_IBS = 24;
  ISSTYPE_TLK = 25;
  ISSTYPE_LAK = 26;
  ISSTYPE_MNW = 27;
  ISSTYPE_DRT = 28;
  ISSTYPE_ETS = 29;
  ISSTYPE_SFR = 30;

type
  {
    @longcode(
  TMt3dmsConcentrationRecord = record
    Cell: TCellLocation;
    Concentration: double;
    StartingTime: double;
    EndingTime: double;
    ConcentrationAnnotation: string;
  end;
    )
    @name stores, the location, time and concentration for
    an MT3DMS point source.
  }
  TMt3dmsConcentrationRecord = record
    Cell: TCellLocation;
    Concentration : array of double;
    StartingTime: double;
    EndingTime: double;
    ConcentrationAnnotation: array of string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TMt3dmsConcentrationRecord)s.
  TMt3dmsConcArray = array of TMt3dmsConcentrationRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of MT3DMS concentration point sources.
  TMt3dmsConcStorage = class(TCustomBoundaryStorage)
  private
    FMt3dmsConclArray: TMt3dmsConcArray;
    function GetMt3dmsConcArray: TMt3dmsConcArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Mt3dmsConcArray: TMt3dmsConcArray read GetMt3dmsConcArray;
  end;

  TStringConcCollection = class;

  TStringConcValueItem = class(TFormulaOrderedItem)
  private
    FValue: TFormulaObject;
    FObserver: TObserver;
    FName: string;
    procedure SetValue(const Value: string);
    function StringCollection: TStringConcCollection;
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    function GetScreenObject: TObject; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
    property Name: string read FName write FName;
  end;

  TStringConcValueItemClass = class of TStringConcValueItem;


  TStringConcCollection = class(TCustomObjectOrderedCollection)
  private
//    FScreenObject: TObject;
    FMt3dmsConcCollection: TCollection;
    function GetItem(Index: integer): TStringConcValueItem;
    procedure SetItem(Index: integer; const Value: TStringConcValueItem);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      Mt3dmsConcCollection: TCollection); overload;
    constructor Create(ItemClass: TStringConcValueItemClass; Model: TBaseModel;
      ScreenObject: TObject; Mt3dmsConcCollection: TCollection); overload;
    property Items[Index: integer]: TStringConcValueItem read GetItem
      write SetItem; default;
    function IndexOfFormulaObject(AFormulaObject: TFormulaObject): integer;
    function Add: TStringConcValueItem;
    procedure RenameItems(const OldSpeciesName, NewSpeciesName: string);
  end;

//  IMt3dConcCollection = interface ['{4B52C524-1429-4F62-935A-4B92F721E5AD}']
//    procedure InvalidateMt3dmsConcData(Sender: TObject);
//    function GetScreenObject: TObject;
//    property ScreenObject: TObject read GetScreenObject;
//  end;

  // @name represents an MT3DMS concentration point source for one time interval.
  // @name is stored by @link(TMt3dmsConcCollection).
  TCustomMt3dmsConcItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(Mt3dmsConcRate).
    FStringConcCollection: TStringConcCollection;
    // See @link(Mt3dmsConcRate).
    procedure SetStringConcCollection(const Value: TStringConcCollection);
    function BoundaryFormulaName(Index: integer): string;
    procedure RenameItems(const OldSpeciesName, NewSpeciesName: string);
    procedure ChangeSpeciesItemPosition(OldIndex, NewIndex: integer);
    function GetCollection: TCustomMF_BoundColl;
  protected
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
    property Collection: TCustomMF_BoundColl read GetCollection;
    function GetMt3dmsConc(Index: integer): string; virtual;
    procedure SetMt3dmsConc(Index: integer; const Value: string); virtual;
  public
    Destructor Destroy; override;
    property Mt3dmsConcRate[Index: integer]: string read GetMt3dmsConc
      write SetMt3dmsConc;
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string);
    procedure DeleteSpecies(SpeciesIndex: integer);
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the concentration of this boundary.
    property StringConcCollection: TStringConcCollection
      read FStringConcCollection write SetStringConcCollection;
  end;

  // @name represents an MT3DMS concentration point source for one time interval.
  // @name is stored by @link(TMt3dmsConcCollection).
  TMt3dmsConcItem = class(TCustomMt3dmsConcItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure InvalidateModel; override;
  end;


    // @name is used to compute the concentrations for a series of
    // MT3DMS point sources over a series of time intervals.
  TCustomMt3dmsConcTimeListLink = class(TTimeListsModelLink)
  private
    FListOfTimeLists: TList;
  protected
    property ListOfTimeLists: TList read FListOfTimeLists;
  public
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
    Destructor Destroy; override;
    procedure UpdateTimeLists; override;
  end;

  TMt3dmsConcTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  // @name represents the concentrations of MT3DMS point sources
  // for a series of time intervals.
  TCustomMt3dmsConcCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateMt3dmsConcData(Sender: TObject);
  protected
    procedure ChangeSpeciesItemPosition(OldIndex, NewIndex: integer);
    procedure ChangeSpeciesTimeListPosition(OldIndex, NewIndex: integer);
    procedure RenameTimeList(const OldSpeciesName, NewSpeciesName: string);
    procedure RenameItems(const OldSpeciesName, NewSpeciesName: string);
    // @name is used in @link(RenameTimeList)
    function ConcName: string; virtual;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // @name calls inherited @name and then sets the length of
    // the @link(TMt3dmsConcStorage.FMt3dmsConclArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
        AModel: TBaseModel); override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string);
    procedure DeleteSpecies(SpeciesIndex: integer);
    procedure CreateTimeLists;
  end;

  // @name represents the concentrations of MT3DMS point sources
  // for a series of time intervals.
  TCustomMt3dmsArrayConcCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateMt3dmsConcData(Sender: TObject);
  protected
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    // @name is used in @link(RenameTimeList)
    function ConcName: string; virtual;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // @name calls inherited @name and then sets the length of
    // the @link(TMt3dmsConcStorage.FMt3dmsConclArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
        AModel: TBaseModel); override;
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string);
    procedure DeleteSpecies(SpeciesIndex: integer);
    procedure CreateTimeLists;
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    procedure RenameTimeList(const OldSpeciesName, NewSpeciesName: string);
    procedure RenameItems(const OldSpeciesName, NewSpeciesName: string);
    procedure ChangeSpeciesItemPosition(OldIndex, NewIndex: integer);
    procedure ChangeSpeciesTimeListPosition(OldIndex, NewIndex: integer);
  end;

  TMt3dmsConcCollection = class(TCustomMt3dmsConcCollection)
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure InvalidateModel; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  // Each @name stores a @link(TMt3dmsConcCollection).
  // @classname is stored by @link(TModflowParameters).
  TMt3dmsConc_Cell = class(TValueCell)
  private
    FValues: TMt3dmsConcentrationRecord;
    FStressPeriod: integer;
    FBoundaryTypes: TIntegerList;
    FMnw2Layers: TIntegerList;
    function GetConcentration(Index: integer): double;
    function GeConcentrationAnnotation(Index: integer): string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer;
      AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer;
      AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer;
      AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream;
      Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    Constructor Create; override;
    destructor Destroy; override;
    property Concentration[Index: integer]: double read GetConcentration;
    property ConcentrationAnnotation[Index: integer]: string
      read GeConcentrationAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property BoundaryTypes: TIntegerList read FBoundaryTypes;
    function PointSinkCount(ColumnCount, RowCount, LayerCount,
      ModflowLayerCount: integer; Model: TBaseModel): integer;
    // @name contains the model layers (starting with 1 as the top layer) that
    // have MNW2 cells defined by vertical screens.
    property Mnw2Layers: TIntegerList read FMnw2Layers;
    property Values: TMt3dmsConcentrationRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    procedure SetConcentrationLength(ALength: Integer);
  end;

  // @name represents the MT3DMS point source associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TMt3dmsConcCollection)
  TCustomMt3dmsConcBoundary = class(TModflowBoundary)
  public
    procedure BoundaryAssignCells(AModel: TBaseModel; ValueTimeList: TList);
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string); virtual;
    procedure RenameSpecies(const OldSpeciesName, NewSpeciesName: string); virtual;
    procedure ChangeSpeciesPosition(OldIndex, NewIndex: integer); virtual;
    procedure DeleteSpecies(SpeciesIndex: integer); virtual;
    procedure CreateTimeLists;
  end;

  TMt3dmsConcBoundary = class(TCustomMt3dmsConcBoundary)
  private
    FSpecifiedConcBoundary: boolean;
    FMassLoadingBoundary: boolean;
    procedure SetMassLoadingBoundary(const Value: boolean);
    procedure SetSpecifiedConcBoundary(const Value: boolean);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TMt3dmsConc_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TMt3dmsConcStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    property SpecifiedConcBoundary: boolean read FSpecifiedConcBoundary
      write SetSpecifiedConcBoundary;
    property MassLoadingBoundary: boolean read FMassLoadingBoundary
      write SetMassLoadingBoundary;
  end;

procedure Mt3dmsStringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure Mt3dmsStringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);


implementation

uses ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, Mt3dmsChemSpeciesUnit,
  ModflowPackageSelectionUnit, ModflowPackagesUnit, PhastModelUnit,
  AbstractGridUnit, Mt3dCtsSystemUnit;

resourcestring
  StrConcentration = ' concentration or mass-loading';
  StrConcentrationMulti = ' concentration or mass-loading multiplier';

procedure Mt3dmsStringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringConcValueItem).RemoveSubscription(Sender, AName);
end;

procedure Mt3dmsStringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringConcValueItem).RestoreSubscription(Sender, AName);
end;


  //const
//  ConcentrationPosition = 0;

{ TCustomMt3dmsConcItem }

procedure TCustomMt3dmsConcItem.Assign(Source: TPersistent);
var
  Index: integer;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomMt3dmsConcItem then
  begin
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      Mt3dmsConcRate[Index] := TCustomMt3dmsConcItem(Source).Mt3dmsConcRate[Index];
    end;
  end;
  inherited;
end;

procedure TMt3dmsConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCustomMt3dmsConcCollection;
  ConcentrationObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TCustomMt3dmsConcCollection;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    ConcentrationObserver := FObserverList[Index];
    ConcentrationObserver.OnUpToDateSet :=
      ParentCollection.InvalidateMt3dmsConcData;
  end;
end;

function TCustomMt3dmsConcItem.BoundaryFormulaName(Index: integer): string;
var
  LocalModel: TCustomModel;
begin
  LocalModel := Collection.Model as TCustomModel;
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
  if Index < LocalModel.MobileComponents.Count then
  begin
    result := LocalModel.MobileComponents[Index].Name;
  end
  else
  begin
    Index := Index - LocalModel.MobileComponents.Count;
    result := LocalModel.ImmobileComponents[Index].Name;
  end;
end;

function TCustomMt3dmsConcItem.BoundaryFormulaCount: integer;
var
  LocalModel: TCustomModel;
begin
  LocalModel := Collection.Model as TCustomModel;
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
//  Mt3dBasic := LocalModel.ModflowPackages.Mt3dBasic;
  result := LocalModel.NumberOfMt3dChemComponents;
end;

procedure TCustomMt3dmsConcItem.ChangeSpeciesItemPosition(OldIndex,
  NewIndex: integer);
begin
  StringConcCollection[OldIndex].Index := NewIndex;
end;

procedure TCustomMt3dmsConcItem.CreateFormulaObjects;
var
  Index: integer;
  Item: TStringConcValueItem;
begin
  if FStringConcCollection = nil then
  begin
    FStringConcCollection := TStringConcCollection.Create(
      Model, ScreenObject, Collection);
  end;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    Item := FStringConcCollection.Add;
    Item.Name := BoundaryFormulaName(Index);
  end;
end;

procedure TCustomMt3dmsConcItem.DeleteSpecies(SpeciesIndex: integer);
var
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  BoundaryFormula[SpeciesIndex] := '0';
  Observer := FObserverList[SpeciesIndex];
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(Observer);
  end;
  FObserverList.Delete(SpeciesIndex);
  StringConcCollection.Delete(SpeciesIndex);
end;

destructor TCustomMt3dmsConcItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to FStringConcCollection.Count - 1 do
  begin
    Mt3dmsConcRate[Index] := '0.';
  end;
  inherited;
  // RemoveFormulaObjects is called by inherited Destroy.
  // It accesses FStringConcCollection.
  FStringConcCollection.Free;
end;

function TCustomMt3dmsConcItem.GetBoundaryFormula(Index: integer): string;
begin
  result := Mt3dmsConcRate[Index]
end;

function TCustomMt3dmsConcItem.GetCollection: TCustomMF_BoundColl;
begin
  result := inherited Collection as TCustomMF_BoundColl;
end;

procedure TCustomMt3dmsConcItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Position: Integer;
begin
  Position := FStringConcCollection.IndexOfFormulaObject(
    Sender as TFormulaObject);

  Assert(Position >= 0);
  List.Add(FObserverList[Position]);
end;

function TCustomMt3dmsConcItem.GetMt3dmsConc(Index: integer): string;
var
  Item: TStringConcValueItem;
begin
  Item := FStringConcCollection[Index];
  result := Item.Value;
  ResetItemObserver(Index);
end;

procedure TCustomMt3dmsConcItem.InsertNewSpecies(SpeciesIndex: integer;
  const Name: string);
var
  Item: TStringConcValueItem;
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  Item := StringConcCollection.Insert(SpeciesIndex) as TStringConcValueItem;
  Item.Name := Name;

  Observer := TObserver.Create(nil);
  FObserverList.Insert(SpeciesIndex, Observer);
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(Observer);
  end;
  BoundaryFormula[SpeciesIndex] := '0';

  AssignObserverEvents(Collection);
end;

procedure TMt3dmsConcItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMt3dmsChemSources(self);
  end;
end;

function TCustomMt3dmsConcItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomMt3dmsConcItem;
  Index: integer;
begin
  result := (AnotherItem is TCustomMt3dmsConcItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCustomMt3dmsConcItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := (Item.Mt3dmsConcRate[Index] = Mt3dmsConcRate[Index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCustomMt3dmsConcItem.RemoveFormulaObjects;
begin
  FStringConcCollection.Clear;
end;

procedure TCustomMt3dmsConcItem.RenameItems(const OldSpeciesName,
  NewSpeciesName: string);
begin
  StringConcCollection.RenameItems(OldSpeciesName, NewSpeciesName);
end;

procedure TCustomMt3dmsConcItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  inherited;
  Mt3dmsConcRate[Index] := Value;
end;

procedure TCustomMt3dmsConcItem.SetMt3dmsConc(Index: integer; const Value: string);
var
  Item: TStringConcValueItem;
begin
  Item := FStringConcCollection[Index];
  Item.Value := Value;
end;

procedure TCustomMt3dmsConcItem.SetStringConcCollection(
  const Value: TStringConcCollection);
begin
  FStringConcCollection.Assign(Value);
end;

{ TMt3dmsConcCollection }

procedure TCustomMt3dmsConcCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMt3dmsConcStorage.Create(AModel));
end;

procedure TCustomMt3dmsConcCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  ConcStorage: TMt3dmsConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(Expression <> nil);

  ConcStorage := BoundaryStorage as TMt3dmsConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with ConcStorage.Mt3dmsConcArray[Index] do
    begin
      Concentration[BoundaryFunctionIndex] := Expression.DoubleResult;
      ConcentrationAnnotation[BoundaryFunctionIndex] := ACell.Annotation;
    end;
  end;
end;

procedure TCustomMt3dmsConcCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  ConcStorage: TMt3dmsConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  ConcStorage := BoundaryStorage as TMt3dmsConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with ConcStorage.Mt3dmsConcArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TCustomMt3dmsConcCollection.ChangeSpeciesItemPosition(OldIndex,
  NewIndex: integer);
var
  ItemIndex: integer;
  AnItem: TCustomMt3dmsConcItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomMt3dmsConcItem;
    AnItem.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  end;
end;

procedure TCustomMt3dmsConcCollection.ChangeSpeciesTimeListPosition(OldIndex,
  NewIndex: integer);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  TimeList: TModflowTimeList;
  TimeLists: TList;
begin
  PhastModel := frmGoPhast.PhastModel;
  TimeLists := TimeListLink.GetLink(PhastModel).TimeLists;
  TimeList := TimeLists[OldIndex];
  TimeLists.Extract(TimeList);
  TimeLists.Insert(NewIndex, TimeList);
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    TimeLists := TimeListLink.GetLink(ChildModel).TimeLists;
    TimeList := TimeLists[OldIndex];
    TimeLists.Extract(TimeList);
    TimeLists.Insert(NewIndex, TimeList);
  end;
end;

function TCustomMt3dmsConcCollection.ConcName: string;
begin
  result := StrConcentration;
end;

procedure TCustomMt3dmsConcCollection.CreateTimeLists;
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
  Link.CreateTimeLists;
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
    Link.CreateTimeLists;
  end;
end;

procedure TCustomMt3dmsConcCollection.DeleteSpecies(SpeciesIndex: integer);
var
  Index: integer;
  Item: TCustomMt3dmsConcItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[index] as TCustomMt3dmsConcItem;
    Item.DeleteSpecies(SpeciesIndex);
  end;
end;

function TMt3dmsConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dmsConcTimeListLink;
end;

function TCustomMt3dmsConcCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TCustomMt3dmsConcItem;
begin
  Item := Items[ItemIndex] as TCustomMt3dmsConcItem;
  result := Item.Mt3dmsConcRate[FormulaIndex];
end;


procedure TCustomMt3dmsConcCollection.InsertNewSpecies(SpeciesIndex: integer;
  const Name: string);
var
  Index: integer;
  Item: TCustomMt3dmsConcItem;
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[index] as TCustomMt3dmsConcItem;
    Item.InsertNewSpecies(SpeciesIndex, Name);
  end;

  PhastModel := frmGoPhast.PhastModel;
  Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
  Link.CreateTimeLists;
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
    Link.CreateTimeLists;
  end;
end;

procedure TMt3dmsConcCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMt3dmsChemSources(self);
  end;
end;

procedure TCustomMt3dmsConcCollection.InvalidateMt3dmsConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
    for Index := 0 to Link.FListOfTimeLists.Count - 1 do
    begin
      TimeList := Link.FListOfTimeLists[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
      for Index := 0 to Link.FListOfTimeLists.Count - 1 do
      begin
        TimeList := Link.FListOfTimeLists[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

class function TMt3dmsConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dmsConcItem;
end;

procedure TCustomMt3dmsConcCollection.RenameItems(const OldSpeciesName,
  NewSpeciesName: string);
var
  ItemIndex: integer;
  AnItem: TCustomMt3dmsConcItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomMt3dmsConcItem;
    AnItem.RenameItems(OldSpeciesName, NewSpeciesName);
  end;
end;

procedure TCustomMt3dmsConcCollection.RenameTimeList(const OldSpeciesName,
  NewSpeciesName: string);
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  Index: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  TimeList: TModflowTimeList;
  OldTimeListName: string;
  NewTimeListName: string;
begin
  if OldSpeciesName <> NewSpeciesName then
  begin
    OldTimeListName := OldSpeciesName + ConcName;
    NewTimeListName := NewSpeciesName + ConcName;

    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
    for Index := 0 to Link.FListOfTimeLists.Count - 1 do
    begin
      TimeList := Link.FListOfTimeLists[Index];
      if TimeList.NonParamDescription = OldTimeListName then
      begin
        TimeList.NonParamDescription := NewTimeListName;
        break;
      end;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
      for Index := 0 to Link.FListOfTimeLists.Count - 1 do
      begin
        TimeList := Link.FListOfTimeLists[Index];
        if TimeList.NonParamDescription = OldTimeListName then
        begin
          TimeList.NonParamDescription := NewTimeListName;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TCustomMt3dmsConcCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  ComponentCount: integer;
  BoundaryIndex: integer;
  Mt3dmsConclArray: TMt3dmsConcArray;
begin
  SetLength((Boundaries[ItemIndex, AModel]
    as TMt3dmsConcStorage).FMt3dmsConclArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  ComponentCount := LocalModel.NumberOfMt3dChemComponents;
  Mt3dmsConclArray := (Boundaries[ItemIndex, AModel]
    as TMt3dmsConcStorage).FMt3dmsConclArray;
  for BoundaryIndex := 0 to BoundaryCount - 1 do
  begin
    SetLength(Mt3dmsConclArray[BoundaryIndex].Concentration, ComponentCount);
    SetLength(Mt3dmsConclArray[BoundaryIndex].ConcentrationAnnotation,
      ComponentCount);
  end;
  inherited;
end;

{ TMt3dmsConc_Cell }

procedure TMt3dmsConc_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
  WriteCompInt(Comp, BoundaryTypes.Count);
  for Index := 0 to BoundaryTypes.Count - 1 do
  begin
    WriteCompInt(Comp, BoundaryTypes[Index]);
  end;

  WriteCompInt(Comp, Mnw2Layers.Count);
  for Index := 0 to Mnw2Layers.Count - 1 do
  begin
    WriteCompInt(Comp, Mnw2Layers[Index]);
  end;
  BoundaryTypes.Clear;
  Mnw2Layers.Clear;
end;

function TMt3dmsConc_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TMt3dmsConc_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TMt3dmsConc_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TMt3dmsConc_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TMt3dmsConc_Cell.PointSinkCount(ColumnCount, RowCount, LayerCount,
  ModflowLayerCount: integer; Model: TBaseModel): integer;
var
  LocalModel: TCustomModel;
  ActiveDataArray: TDataArray;
  TestLayer: Integer;
begin
  result := FBoundaryTypes.Count;
  if BoundaryTypes.IndexOf(ISSTYPE_MNW) >= 0 then
  begin
    if Mnw2Layers.Count > 0 then
    begin
      Inc(result, Mnw2Layers.Count-1);
    end;
  end;
  if BoundaryTypes.IndexOf(ISSTYPE_LAK) >= 0 then
  begin
    Dec(result);
    LocalModel := Model as TCustomModel;
    ActiveDataArray := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;
    TestLayer := Layer+1;
    if TestLayer < LayerCount then
    begin
      if not LocalModel.IsLayerSimulated(TestLayer) then
      begin
        Inc(TestLayer);
      end;
      if ActiveDataArray.BooleanData[TestLayer, Row, Column] then
      begin
        Inc(result);
      end;
    end;
    if Column > 0 then
    begin
      if ActiveDataArray.BooleanData[Layer, Row, Column-1] then
      begin
        Inc(result);
      end;
    end;
    if Row > 0 then
    begin
      if ActiveDataArray.BooleanData[Layer, Row-1, Column] then
      begin
        Inc(result);
      end;
    end;
    if Column+1 < ColumnCount then
    begin
      if ActiveDataArray.BooleanData[Layer, Row, Column+1] then
      begin
        Inc(result);
      end;
    end;
    if Row+1 < RowCount then
    begin
      if ActiveDataArray.BooleanData[Layer, Row+1, Column] then
      begin
        Inc(result);
      end;
    end;
  end;
end;

function TMt3dmsConc_Cell.GetConcentration(Index: integer): double;
begin
  result := Values.Concentration[Index];
end;

constructor TMt3dmsConc_Cell.Create;
begin
  inherited;
  FBoundaryTypes := TIntegerList.Create;
  FMnw2Layers := TIntegerList.Create;
end;

destructor TMt3dmsConc_Cell.Destroy;
begin
  FMnw2Layers.Free;
  FBoundaryTypes.Free;
  inherited;
end;

function TMt3dmsConc_Cell.GeConcentrationAnnotation(Index: integer): string;
begin
  result := Values.ConcentrationAnnotation[Index];
end;

function TMt3dmsConc_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := ConcentrationAnnotation[Index];
end;

function TMt3dmsConc_Cell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := Concentration[Index];
end;

function TMt3dmsConc_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TMt3dmsConc_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TMt3dmsConc_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Conc_Cell: TMt3dmsConc_Cell;
  Index: Integer;
begin
  result := AnotherCell is TMt3dmsConc_Cell;
  if result then
  begin
    Conc_Cell := TMt3dmsConc_Cell(AnotherCell);
    result := IFace = Conc_Cell.IFace;
    if result then
    begin
      for Index := 0 to Length(Values.Concentration) - 1 do
      begin
        result := (Concentration[Index] = Conc_Cell.Concentration[Index]);
        if not result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TMt3dmsConc_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TMt3dmsConc_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
  Count := ReadCompInt(Decomp);
  BoundaryTypes.Capacity := Count;
  for Index := 0 to Count - 1 do
  begin
    BoundaryTypes.Add(ReadCompInt(Decomp))
  end;

  Count := ReadCompInt(Decomp);
  Mnw2Layers.Capacity := Count;
  for Index := 0 to Count - 1 do
  begin
    Mnw2Layers.Add(ReadCompInt(Decomp))
  end;
end;

procedure TMt3dmsConc_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TMt3dmsConc_Cell.SetConcentrationLength(ALength: Integer);
begin
  SetLength(FValues.Concentration, ALength);
  SetLength(FValues.ConcentrationAnnotation, ALength);
end;

procedure TMt3dmsConc_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TMt3dmsConc_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TMt3dmsConcBoundary }

procedure TMt3dmsConcBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TMt3dmsConcBoundary;
begin
  inherited;
  if Source is TMt3dmsConcBoundary then
  begin
    SourceBoundary := TMt3dmsConcBoundary(Source);
    SpecifiedConcBoundary := SourceBoundary.SpecifiedConcBoundary;
    MassLoadingBoundary := SourceBoundary.MassLoadingBoundary;
  end;
end;

procedure TMt3dmsConcBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TMt3dmsConc_Cell;
  BoundaryValues: TMt3dmsConcentrationRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMt3dmsConcStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  Packages: TModflowPackages;
  BoundaryTypes: TIntegerList;
  Mnw2Boundary: TMnw2Boundary;
  ScreenIndex: integer;
  AScreen: TVerticalScreen;
  Grid: TCustomModelGrid;
  TopLayer: integer;
  BottomLayer: Integer;
  LayerIndex: integer;
  ModelTop: Real;
  ModelBottom: Real;
begin
  BoundaryTypes := TIntegerList.Create;
  try
    BoundaryTypes.Sorted := True;
    LocalModel := AModel as TCustomModel;
    Packages := LocalModel.ModflowPackages;

    Assert(ScreenObject <> nil);
    LocalScreenObject := ScreenObject as TScreenObject;

    if SpecifiedConcBoundary then
    begin
      BoundaryTypes.Add(ISSYTPE_ConstConc);
    end;
    if Packages.ChdBoundary.IsSelected then
    begin
      if (LocalScreenObject.ModflowChdBoundary <> nil)
        and LocalScreenObject.ModflowChdBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_CHD);
      end;
    end;
    if Packages.WelPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowWellBoundary <> nil)
        and LocalScreenObject.ModflowWellBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_WEL);
      end;
    end;
    if Packages.DrnPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowDrnBoundary <> nil)
        and LocalScreenObject.ModflowDrnBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_DRN);
      end;
    end;
    if Packages.RivPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowRivBoundary <> nil)
        and LocalScreenObject.ModflowRivBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_RIV);
      end;
    end;
    if Packages.GhbBoundary.IsSelected then
    begin
      if (LocalScreenObject.ModflowGhbBoundary <> nil)
        and LocalScreenObject.ModflowGhbBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_GHB);
      end;
    end;
    if Packages.RchPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowRchBoundary <> nil)
        and LocalScreenObject.ModflowRchBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_RCH);
      end;
    end;
    if Packages.EvtPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowEvtBoundary <> nil)
        and LocalScreenObject.ModflowEvtBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_EVT);
      end;
    end;
    if Packages.StrPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowStrBoundary <> nil)
        and LocalScreenObject.ModflowStrBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_STR);
      end;
    end;
    if Packages.SFrPackage.IsSelected
      and (Packages.Mt3dBasic.Mt3dVersion = mvUSGS) then
    begin
      if (LocalScreenObject.ModflowSfrBoundary <> nil)
        and LocalScreenObject.ModflowSfrBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_SFR);
      end;
    end;
    if Packages.FhbPackage.IsSelected then
    begin
      if ((LocalScreenObject.ModflowFhbFlowBoundary <> nil)
        and LocalScreenObject.ModflowFhbFlowBoundary.Used) then
      begin
        BoundaryTypes.Add(ISSTYPE_FHB);
      end;
      if ((LocalScreenObject.ModflowFhbHeadBoundary <> nil)
        and LocalScreenObject.ModflowFhbHeadBoundary.Used) then
      begin
        BoundaryTypes.Add(ISSTYPE_CHD);
      end;
    end;
    if MassLoadingBoundary then
    begin
      BoundaryTypes.Add(ISSYTPE_Mass);
    end;
    if Packages.ResPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowResBoundary <> nil)
        and LocalScreenObject.ModflowResBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_RES);
      end;
    end;
    if Packages.LakPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowLakBoundary <> nil)
        and LocalScreenObject.ModflowLakBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_LAK);
      end;
    end;
    if Packages.DrtPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowDrtBoundary <> nil)
        and LocalScreenObject.ModflowDrtBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_DRT);
      end;
    end;
    if Packages.EtsPackage.IsSelected then
    begin
      if (LocalScreenObject.ModflowEtsBoundary <> nil)
        and LocalScreenObject.ModflowEtsBoundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_ETS);
      end;
    end;
    Mnw2Boundary := nil;
    if Packages.Mnw2Package.IsSelected then
    begin
      if (LocalScreenObject.ModflowMnw2Boundary <> nil)
        and LocalScreenObject.ModflowMnw2Boundary.Used then
      begin
        Mnw2Boundary := LocalScreenObject.ModflowMnw2Boundary;
        BoundaryTypes.Add(ISSTYPE_MNW);
      end;
    end;
    if Packages.Mnw1Package.IsSelected then
    begin
      if (LocalScreenObject.ModflowMnw1Boundary <> nil)
        and LocalScreenObject.ModflowMnw1Boundary.Used then
      begin
        BoundaryTypes.Add(ISSTYPE_MNW);
      end;
    end;
//    if Packages.SfrPackage.IsSelected then
//    begin
//      if (LocalScreenObject.ModflowSfrBoundary <> nil)
//        and LocalScreenObject.ModflowSfrBoundary.Used then
//      begin
//        BoundaryTypes.Add(ISSTYPE_SFR);
//      end;
//    end;
    if BoundaryTypes.Count = 0 then
    begin
      Exit;
    end;

    Grid := LocalModel.Grid;
    LocalBoundaryStorage := BoundaryStorage as TMt3dmsConcStorage;
    for TimeIndex := 0 to
      LocalModel.ModflowFullStressPeriods.Count - 1 do
    begin
      if TimeIndex < ValueTimeList.Count then
      begin
        Cells := ValueTimeList[TimeIndex];
      end
      else
      begin
        Cells := TValueCellList.Create(TMt3dmsConc_Cell);
        ValueTimeList.Add(Cells);
      end;
      StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
      // Check if the stress period is completely enclosed within the times
      // of the LocalBoundaryStorage;
      if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
        and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
      begin
        if Cells.Capacity < Cells.Count
          + Length(LocalBoundaryStorage.Mt3dmsConcArray) then
        begin
          Cells.Capacity := Cells.Count
            + Length(LocalBoundaryStorage.Mt3dmsConcArray)
        end;
  //      Cells.CheckRestore;
        for BoundaryIndex := 0 to
          Length(LocalBoundaryStorage.Mt3dmsConcArray) - 1 do
        begin
          BoundaryValues := LocalBoundaryStorage.Mt3dmsConcArray[BoundaryIndex];
          Cell := TMt3dmsConc_Cell.Create;
          Cells.Add(Cell);
          Cell.BoundaryTypes.Assign(BoundaryTypes);
          LocalModel.AdjustCellPosition(Cell);
          Cell.IFace := LocalScreenObject.IFace;
          Cell.StressPeriod := TimeIndex;
          Cell.Values := BoundaryValues;
          Cell.ScreenObject := ScreenObject;
          Cell.SetConcentrationLength(Length(Cell.Values.Concentration));
//          SetLength(Cell.Values.Concentration,
//            Length(Cell.Values.Concentration));
          if (Mnw2Boundary <> nil) and (Mnw2Boundary.VerticalScreens.Count > 0) then
          begin
            for ScreenIndex := 0 to Mnw2Boundary.VerticalScreens.Count - 1 do
            begin
              AScreen := Mnw2Boundary.VerticalScreens[ScreenIndex];
              ModelTop := Grid.CellElevation[Cell.Column, Cell.Row,0];
              ModelBottom := Grid.CellElevation[Cell.Column, Cell.Row,Grid.LayerCount];
              if AScreen.ZTop >= ModelTop then
              begin
                TopLayer := 0;
              end
              else if AScreen.ZTop <= ModelBottom then
              begin
                TopLayer := Grid.LayerCount-1;
              end
              else
              begin
                TopLayer := Grid.GetContainingLayer(Cell.Column, Cell.Row,
                  AScreen.ZTop);
              end;

              if AScreen.ZBottom >= ModelTop then
              begin
                BottomLayer := 0;
              end
              else if AScreen.ZBottom <= ModelBottom then
              begin
                BottomLayer := Grid.LayerCount-1;
              end
              else
              begin
                BottomLayer := Grid.GetContainingLayer(Cell.Column, Cell.Row,
                  AScreen.ZBottom);
              end;
              for LayerIndex := TopLayer to BottomLayer do
              begin
                if LocalModel.IsLayerSimulated(LayerIndex) then
                begin
                  Cell.Mnw2Layers.Add(LayerIndex+1);
                end;
              end;

            end;
          end;
        end;
        Cells.Cache;
      end;
    end;
    LocalBoundaryStorage.CacheData;
  finally
    BoundaryTypes.Free;
  end;
end;

class function TMt3dmsConcBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMt3dmsConcCollection;
end;

procedure TCustomMt3dmsConcBoundary.ChangeSpeciesPosition(OldIndex,
  NewIndex: integer);
var
  Concentrations: TCustomMt3dmsConcCollection;
  ArrayConc: TCustomMt3dmsArrayConcCollection;
begin
  if Values is TCustomMt3dmsConcCollection  then
  begin
    Concentrations := TCustomMt3dmsConcCollection(Values);
    Concentrations.ChangeSpeciesItemPosition(OldIndex, NewIndex);
    Concentrations.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);
  end
  else
  begin
    ArrayConc := Values as TCustomMt3dmsArrayConcCollection;
    ArrayConc.ChangeSpeciesItemPosition(OldIndex, NewIndex);
    ArrayConc.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);
  end;
end;

procedure TCustomMt3dmsConcBoundary.CreateTimeLists;
begin
  if Values is TCustomMt3dmsConcCollection then
  begin
    TCustomMt3dmsConcCollection(Values).CreateTimeLists;
  end
  else
  begin
    (Values as TCustomMt3dmsArrayConcCollection).CreateTimeLists;
  end;
end;

procedure TCustomMt3dmsConcBoundary.BoundaryAssignCells(AModel: TBaseModel;
  ValueTimeList: TList);
var
  BoundaryStorage: TMt3dmsConcStorage;
  ValueIndex: Integer;
begin
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel]
        as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

procedure TCustomMt3dmsConcBoundary.DeleteSpecies(SpeciesIndex: integer);
var
  Concentrations: TCustomMt3dmsConcCollection;
  ArrayConc: TCustomMt3dmsArrayConcCollection;
begin
  if Values is TCustomMt3dmsConcCollection then
  begin
    Concentrations := TCustomMt3dmsConcCollection(Values);
    Concentrations.DeleteSpecies(SpeciesIndex);
  end
  else
  begin
    ArrayConc := Values as TCustomMt3dmsArrayConcCollection;
    ArrayConc.DeleteSpecies(SpeciesIndex);
  end;
end;

procedure TMt3dmsConcBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
begin
  EvaluateListBoundaries(AModel);
end;

procedure TCustomMt3dmsConcBoundary.InsertNewSpecies(SpeciesIndex: integer;
  const Name: string);
var
  Concentrations: TCustomMt3dmsConcCollection;
  ArrayConc: TCustomMt3dmsArrayConcCollection;
begin
  if Values is TCustomMt3dmsConcCollection then
  begin
    Concentrations := TCustomMt3dmsConcCollection(Values);
    Concentrations.InsertNewSpecies(SpeciesIndex, Name);
  end
  else
  begin
    ArrayConc := Values as TCustomMt3dmsArrayConcCollection;
    ArrayConc.InsertNewSpecies(SpeciesIndex, Name);
  end;
end;

procedure TMt3dmsConcBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateMt3dmsChemSources(self);
  end;
end;

procedure TCustomMt3dmsConcBoundary.RenameSpecies(const OldSpeciesName,
  NewSpeciesName: string);
var
  Concentrations: TCustomMt3dmsConcCollection;
  ArrayConc: TCustomMt3dmsArrayConcCollection;
begin
  if Values is TCustomMt3dmsConcCollection  then
  begin
    Concentrations := TCustomMt3dmsConcCollection(Values);
    Concentrations.RenameTimeList(OldSpeciesName, NewSpeciesName);
    Concentrations.RenameItems(OldSpeciesName, NewSpeciesName);
  end
  else
  begin
    ArrayConc := Values as TCustomMt3dmsArrayConcCollection;
    ArrayConc.RenameTimeList(OldSpeciesName, NewSpeciesName);
    ArrayConc.RenameItems(OldSpeciesName, NewSpeciesName);
  end;
end;

procedure TMt3dmsConcBoundary.SetMassLoadingBoundary(const Value: boolean);
begin
  if FMassLoadingBoundary <> Value then
  begin
    FMassLoadingBoundary := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsConcBoundary.SetSpecifiedConcBoundary(const Value: boolean);
begin
  if FSpecifiedConcBoundary <> Value then
  begin
    FSpecifiedConcBoundary := Value;
    InvalidateModel;
  end;
end;

{ TMt3dmsConcentrationRecord }

procedure TMt3dmsConcentrationRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
var
  Index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Length(Concentration));
  for Index := 0 to Length(Concentration) - 1 do
  begin
    WriteCompReal(Comp, Concentration[Index]);
  end;
  for Index := 0 to Length(ConcentrationAnnotation) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ConcentrationAnnotation[Index]));
  end;
end;

procedure TMt3dmsConcentrationRecord.RecordStrings(Strings: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to Length(ConcentrationAnnotation) - 1 do
  begin
    Strings.Add(ConcentrationAnnotation[Index]);
  end;
end;

procedure TMt3dmsConcentrationRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  Count := ReadCompInt(Decomp);
  SetLength(Concentration, Count);
  SetLength(ConcentrationAnnotation, Count);
  for Index := 0 to Count - 1 do
  begin
    Concentration[Index] := ReadCompReal(Decomp);
  end;
  for Index := 0 to Count - 1 do
  begin
    ConcentrationAnnotation[Index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

{ TMt3dmsConcStorage }

procedure TMt3dmsConcStorage.Clear;
begin
  SetLength(FMt3dmsConclArray, 0);
  FCleared := True;
end;

procedure TMt3dmsConcStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FMt3dmsConclArray);
    for Index := 0 to Count - 1 do
    begin
      FMt3dmsConclArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMt3dmsConclArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TMt3dmsConcStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMt3dmsConclArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FMt3dmsConclArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TMt3dmsConcStorage.GetMt3dmsConcArray: TMt3dmsConcArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMt3dmsConclArray;
end;

{ TMt3dmsConcTimeListLink }

constructor TCustomMt3dmsConcTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FListOfTimeLists := TObjectList.Create;
  inherited;
end;

procedure TMt3dmsConcTimeListLink.CreateTimeLists;
var
  Index: Integer;
  Mt3dmsConcData: TModflowTimeList;
  Item: TChemSpeciesItem;
  LocalModel: TPhastModel;
begin
  inherited;
  TimeLists.Clear;
  FListOfTimeLists.Clear;
  LocalModel := frmGoPhast.PhastModel;
  for Index := 0 to LocalModel.MobileComponents.Count - 1 do
  begin
    Item := LocalModel.MobileComponents[Index];
    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dmsConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateMt3dmsChemSources;
    end;
    AddTimeList(Mt3dmsConcData);
    FListOfTimeLists.Add(Mt3dmsConcData);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
    Item := LocalModel.ImmobileComponents[Index];
    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dmsConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateMt3dmsChemSources;
    end;
    AddTimeList(Mt3dmsConcData);
    FListOfTimeLists.Add(Mt3dmsConcData);
  end;
end;

destructor TCustomMt3dmsConcTimeListLink.Destroy;
begin
  FListOfTimeLists.Free;
  inherited;
end;

procedure TCustomMt3dmsConcTimeListLink.UpdateTimeLists;
begin
  CreateTimeLists;
end;

{ TStringConcValueItem }

procedure TStringConcValueItem.Assign(Source: TPersistent);
var
  SourceItem: TStringConcValueItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TStringConcValueItem then
  begin
    SourceItem := TStringConcValueItem(Source);
    Value := SourceItem.Value;
    Name := SourceItem.Name;
  end;
  inherited;
end;

constructor TStringConcValueItem.Create(Collection: TCollection);
var
  SCollection: TStringConcCollection;
  Mt3dmsConc: TCustomMt3dmsConcCollection;
  LocalScreenObject: TScreenObject;
  Mt3dmsArrayConc: TCustomMt3dmsArrayConcCollection;
//  CtsCollection: TCtsExternalFlowsCollection;
begin
  inherited;
  FObserver:= TObserver.Create(nil);
  SCollection := StringCollection;
  LocalScreenObject := nil;

  if SCollection.FMt3dmsConcCollection is TCustomMt3dmsConcCollection then
  begin
    Mt3dmsConc := TCustomMt3dmsConcCollection(SCollection.FMt3dmsConcCollection);
    FObserver.OnUpToDateSet := Mt3dmsConc.InvalidateMt3dmsConcData;
    LocalScreenObject := Mt3dmsConc.ScreenObject as TScreenObject;
  end
  else if SCollection.FMt3dmsConcCollection is TCtsExternalFlowsCollection then
  begin
//    CtsCollection := TCtsExternalFlowsCollection(SCollection.FMt3dmsConcCollection);
    FObserver.OnUpToDateSet := nil;
    LocalScreenObject := nil;
  end
  else if SCollection.FMt3dmsConcCollection is TCustomMt3dmsArrayConcCollection then
  begin
    Mt3dmsArrayConc := TCustomMt3dmsArrayConcCollection(SCollection.FMt3dmsConcCollection);
    FObserver.OnUpToDateSet := Mt3dmsArrayConc.InvalidateMt3dmsConcData;
    LocalScreenObject := Mt3dmsArrayConc.ScreenObject as TScreenObject;
  end
  else if SCollection.FMt3dmsConcCollection is TCustomNonSpatialBoundColl then
  begin
//    CtsCollection := TCtsExternalFlowsCollection(SCollection.FMt3dmsConcCollection);
    FObserver.OnUpToDateSet := nil;
    LocalScreenObject := nil;
  end
  else if SCollection.FMt3dmsConcCollection = nil then
  begin
//    CtsCollection := TCtsExternalFlowsCollection(SCollection.FMt3dmsConcCollection);
    FObserver.OnUpToDateSet := nil;
    LocalScreenObject := nil;
  end
  else
  begin
    Assert(False);
  end;
  //

  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(FObserver);
  end;

  OnRemoveSubscription := Mt3dmsStringValueRemoveSubscription;
  OnRestoreSubscription := Mt3dmsStringValueRestoreSubscription;

  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  FValue.AddSubscriptionEvents(Mt3dmsStringValueRemoveSubscription,
    Mt3dmsStringValueRestoreSubscription, self);
end;

destructor TStringConcValueItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  SCollection: TStringConcCollection;
  ConcColl: TCustomMF_BoundColl;
begin
  Value := '0';
  SCollection := StringCollection;
  if SCollection.FMt3dmsConcCollection is TCustomMF_BoundColl then
  begin
    ConcColl := TCustomMF_BoundColl(SCollection.FMt3dmsConcCollection);
    LocalScreenObject := ConcColl.ScreenObject as TScreenObject;
  end
  else
  begin
    LocalScreenObject := nil;
  end;

  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(FObserver);
  end;
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    Mt3dmsStringValueRemoveSubscription,
    Mt3dmsStringValueRestoreSubscription, self);
  FObserver.Free;
  inherited;
end;

function TStringConcValueItem.GetObserver(Index: Integer): TObserver;
begin
  Result := FObserver;
end;

function TStringConcValueItem.GetScreenObject: TObject;
begin
  result := StringCollection.ScreenObject;
end;

function TStringConcValueItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TStringConcValueItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem : TStringConcValueItem;
begin
  if AnotherItem is TStringConcValueItem then
  begin
    OtherItem := TStringConcValueItem(AnotherItem);
    result := (Value = OtherItem.Value)
      and (Name = OtherItem.Name)
  end
  else
  begin
    result := false;
  end;
end;

procedure TStringConcValueItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TStringConcValueItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TStringConcValueItem.SetValue(const Value: string);
var
  Dummy: integer;
begin
  Dummy := 0;
  UpdateFormula(Value, Dummy, FValue);
end;

function TStringConcValueItem.StringCollection: TStringConcCollection;
begin
  result := Collection as TStringConcCollection;
end;

{ TStringConcCollection }

function TStringConcCollection.Add: TStringConcValueItem;
begin
  result := inherited Add as TStringConcValueItem
end;

procedure TStringConcCollection.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TStringConcCollection.Create(ItemClass: TStringConcValueItemClass;
  Model: TBaseModel; ScreenObject: TObject; Mt3dmsConcCollection: TCollection);
begin
  inherited Create(ItemClass, Model, ScreenObject);
//  FScreenObject := ScreenObject;
  FMt3dmsConcCollection := Mt3dmsConcCollection;
end;

constructor TStringConcCollection.Create(Model: TBaseModel;
  ScreenObject: TObject; Mt3dmsConcCollection: TCollection);
begin
  Create(TStringConcValueItem, Model, ScreenObject, Mt3dmsConcCollection);
end;

function TStringConcCollection.GetItem(Index: integer): TStringConcValueItem;
begin
  result := inherited Items[Index] as TStringConcValueItem;
end;

function TStringConcCollection.IndexOfFormulaObject(
  AFormulaObject: TFormulaObject): integer;
var
  Index: Integer;
  AnItem: TStringConcValueItem;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index];
    if AnItem.FValue = AFormulaObject then
    begin
      result := Index;
      break;
    end;
  end;
end;

procedure TStringConcCollection.RenameItems(const OldSpeciesName,
  NewSpeciesName: string);
var
  ItemIndex: integer;
  Item: TStringConcValueItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Item :=Items[ItemIndex];
    if Item.Name = OldSpeciesName then
    begin
      Item.Name := NewSpeciesName;
      break;
    end;
  end;
end;

procedure TStringConcCollection.SetItem(Index: integer;
  const Value: TStringConcValueItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomMt3dmsArrayConcCollection }

procedure TCustomMt3dmsArrayConcCollection.AddSpecificBoundary(
  AModel: TBaseModel);
begin
  AddBoundary(TMt3dmsConcStorage.Create(AModel));
end;

function TCustomMt3dmsArrayConcCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TCustomMt3dmsConcItem;
begin
  Item := Items[ItemIndex] as TCustomMt3dmsConcItem;
  result := Item.Mt3dmsConcRate[FormulaIndex];
end;

procedure TCustomMt3dmsArrayConcCollection.AssignArrayCellValues(
  DataSets: TList; ItemIndex: Integer; AModel: TBaseModel);
var
//  EvapotranspirationSurfaceArray: TDataArray;
//  EvapotranspirationDepthArray: TDataArray;
//  Boundary: TEvtSurfDepthStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
  Boundary: TMt3dmsConcStorage;
  FirstArray: TDataArray;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  Boundary := Boundaries[ItemIndex, AModel] as TMt3dmsConcStorage;
  FirstArray := DataSets[0];
  FirstArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := 0 to FirstArray.LayerCount - 1 do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to FirstArray.RowCount - 1 do
        begin
          for ColIndex := 0 to FirstArray.ColumnCount - 1 do
          begin
            if FirstArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
//              Assert(EvapotranspirationDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.Mt3dmsConcArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                for DataArrayIndex := 0 to DataSets.Count - 1 do
                begin
                  ADataArray := DataSets[DataArrayIndex];
                  Assert(ADataArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                  Concentration[DataArrayIndex] := ADataArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  ConcentrationAnnotation[DataArrayIndex] := ADataArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                end;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  for DataArrayIndex := 0 to DataSets.Count - 1 do
  begin
    ADataArray := DataSets[DataArrayIndex];
    ADataArray.CacheData;
  end;
  Boundary.CacheData;
end;

procedure TCustomMt3dmsArrayConcCollection.AssignCellList(
  Expression: TExpression; ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
  Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject);
var
  ConcStorage: TMt3dmsConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(Expression <> nil);

  ConcStorage := BoundaryStorage as TMt3dmsConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with ConcStorage.Mt3dmsConcArray[Index] do
    begin
      Concentration[BoundaryFunctionIndex] := Expression.DoubleResult;
      ConcentrationAnnotation[BoundaryFunctionIndex] := ACell.Annotation;
    end;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  ConcStorage: TMt3dmsConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  ConcStorage := BoundaryStorage as TMt3dmsConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with ConcStorage.Mt3dmsConcArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.ChangeSpeciesItemPosition(OldIndex,
  NewIndex: integer);
var
  ItemIndex: integer;
  AnItem: TCustomMt3dmsConcItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomMt3dmsConcItem;
    AnItem.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.ChangeSpeciesTimeListPosition(
  OldIndex, NewIndex: integer);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  TimeList: TModflowTimeList;
  TimeLists: TList;
begin
  PhastModel := frmGoPhast.PhastModel;
  TimeLists := TimeListLink.GetLink(PhastModel).TimeLists;
  TimeList := TimeLists[OldIndex];
  TimeLists.Extract(TimeList);
  TimeLists.Insert(NewIndex, TimeList);
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    TimeLists := TimeListLink.GetLink(ChildModel).TimeLists;
    TimeList := TimeLists[OldIndex];
    TimeLists.Extract(TimeList);
    TimeLists.Insert(NewIndex, TimeList);
  end;
end;

function TCustomMt3dmsArrayConcCollection.ConcName: string;
begin
  result := StrConcentration;
end;

procedure TCustomMt3dmsArrayConcCollection.CreateTimeLists;
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
  Link.CreateTimeLists;
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
    Link.CreateTimeLists;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.DeleteSpecies(SpeciesIndex: integer);
var
  Index: integer;
  Item: TCustomMt3dmsConcItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[index] as TCustomMt3dmsConcItem;
    Item.DeleteSpecies(SpeciesIndex);
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.InitializeTimeLists(
  ListOfTimeLists: TList; AModel: TBaseModel);
var
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Boundary: TCustomMt3dmsConcBoundary;
  ScreenObject: TScreenObject;
  ALink: TCustomMt3dmsConcTimeListLink;
  Item: TCustomMt3dmsConcItem;
  SpeciesIndex: Integer;
  TimeList: TModflowTimeList;
  TimeIndex: Integer;
begin
  Boundary := BoundaryGroup as TCustomMt3dmsConcBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  ALink := TimeListLink.GetLink(AModel) as TCustomMt3dmsConcTimeListLink;
  ALink.UpdateTimeLists;
  for SpeciesIndex := 0 to ALink.TimeLists.Count - 1 do
  begin
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TCustomMt3dmsConcItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.BoundaryFormula[SpeciesIndex];
    end;
    TimeList := ALink.TimeLists[SpeciesIndex];
    TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
    Assert(TimeList.Count = Count);
  end;

  ClearBoundaries(AModel);
  if ALink.TimeLists.Count > 0 then
  begin
    TimeList := ALink.TimeLists[0];
    SetBoundaryCapacity(TimeList.Count, AModel);
    for TimeIndex := 0 to TimeList.Count - 1 do
    begin
      AddSpecificBoundary(Model)
    end;
    for SpeciesIndex := 0 to ALink.TimeLists.Count - 1 do
    begin
      TimeList := ALink.TimeLists[SpeciesIndex];
      ListOfTimeLists.Add(TimeList);
    end;
  end;

end;

procedure TCustomMt3dmsArrayConcCollection.InsertNewSpecies(
  SpeciesIndex: integer; const Name: string);
var
  Index: integer;
  Item: TCustomMt3dmsConcItem;
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[index] as TCustomMt3dmsConcItem;
    Item.InsertNewSpecies(SpeciesIndex, Name);
  end;

  PhastModel := frmGoPhast.PhastModel;
  Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
  Link.CreateTimeLists;
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
    Link.CreateTimeLists;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.InvalidateMt3dmsConcData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
    for Index := 0 to Link.FListOfTimeLists.Count - 1 do
    begin
      TimeList := Link.FListOfTimeLists[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
      for Index := 0 to Link.FListOfTimeLists.Count - 1 do
      begin
        TimeList := Link.FListOfTimeLists[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.RenameItems(const OldSpeciesName,
  NewSpeciesName: string);
var
  ItemIndex: integer;
  AnItem: TCustomMt3dmsConcItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TCustomMt3dmsConcItem;
    AnItem.RenameItems(OldSpeciesName, NewSpeciesName);
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.RenameTimeList(const OldSpeciesName,
  NewSpeciesName: string);
var
  PhastModel: TPhastModel;
  Link: TCustomMt3dmsConcTimeListLink;
  Index: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  TimeList: TModflowTimeList;
  OldTimeListName: string;
  NewTimeListName: string;
begin
  if OldSpeciesName <> NewSpeciesName then
  begin
    OldTimeListName := OldSpeciesName + ConcName;
    NewTimeListName := NewSpeciesName + ConcName;

    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TCustomMt3dmsConcTimeListLink;
    for Index := 0 to Link.FListOfTimeLists.Count - 1 do
    begin
      TimeList := Link.FListOfTimeLists[Index];
      if TimeList.NonParamDescription = OldTimeListName then
      begin
        TimeList.NonParamDescription := NewTimeListName;
        break;
      end;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCustomMt3dmsConcTimeListLink;
      for Index := 0 to Link.FListOfTimeLists.Count - 1 do
      begin
        TimeList := Link.FListOfTimeLists[Index];
        if TimeList.NonParamDescription = OldTimeListName then
        begin
          TimeList.NonParamDescription := NewTimeListName;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TCustomMt3dmsArrayConcCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  ComponentCount: integer;
  BoundaryIndex: integer;
  Mt3dmsConclArray: TMt3dmsConcArray;
begin
  SetLength((Boundaries[ItemIndex, AModel]
    as TMt3dmsConcStorage).FMt3dmsConclArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  ComponentCount := LocalModel.NumberOfMt3dChemComponents;
  Mt3dmsConclArray := (Boundaries[ItemIndex, AModel]
    as TMt3dmsConcStorage).FMt3dmsConclArray;
  for BoundaryIndex := 0 to BoundaryCount - 1 do
  begin
    SetLength(Mt3dmsConclArray[BoundaryIndex].Concentration, ComponentCount);
    SetLength(Mt3dmsConclArray[BoundaryIndex].ConcentrationAnnotation,
      ComponentCount);
  end;
  inherited;
end;

end.
