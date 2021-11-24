unit Mt3dLktUnit;

interface

uses
  Classes, Mt3dmsChemUnit, ModflowBoundaryUnit, GoPhastTypes,
  FormulaManagerUnit, SubscriptionUnit, System.Generics.Collections,
  OrderedCollectionUnit, RbwParser, System.ZLib, ModflowCellUnit;

type
  TMt3dLktConcItem = class(TCustomMt3dmsConcItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
//    procedure InvalidateModel; override;
  end;

  TMt3dLktConcTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TMt3dLktConcCollection = class(TCustomMt3dmsArrayConcCollection)
  private
//    procedure InvalidateLktConc(Sender: TObject);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
//    procedure InvalidateModel; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
//    function ConcName: string; override;
  end;

  TLktInitConcRecord = record
    Cell: TCellLocation;
    InitialConcentration: double;
    InitialConcentrationAnnotation: string;
    InitialConcentrationPestName: string;
    InitialConcentrationPestSeriesName: string;
    InitialConcentrationPestSeriesMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TLktInitConcArray = array of TLktInitConcRecord;

  TLktInitConcStorage = class(TCustomBoundaryStorage)
  private
    FLktInitConcArray: TLktInitConcArray;
    function GetLktInitConcArray: TLktInitConcArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property LktInitConcArray: TLktInitConcArray
      read GetLktInitConcArray;
  end;

  TLktInitConcItem = class(TCustomModflowBoundaryItem)
  private
    const
      InitConcPosition = 0;
    var
    FInitConc: TFormulaObject;
    function GetInitConc: string;
    procedure SetInitConc(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Loaded;
  published
    property InitConc: string read GetInitConc write SetInitConc;
  end;

  TLktIntConcTimeListLink = class(TTimeListsModelLink)
  private
    FInitCondData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TLktInitConcCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateInitConcData(Sender: TObject);
    function GetItem(Index: Integer): TLktInitConcItem;
    procedure SetItem(Index: Integer; const Value: TLktInitConcItem);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    procedure Loaded;
    property Items[Index: Integer]: TLktInitConcItem read GetItem write SetItem; default;
  end;


  // @name defines the concentration of Lake precipitation and runoff into
  // a lake in MT3D-USGS.
  TMt3dLktConcBoundary = class(TCustomMt3dmsConcBoundary)
  private
    FRunoffConcentration: TMt3dLktConcCollection;
    FInitialConcentrationObserver: TObserver;
    FInitialConcentrations: TLktInitConcCollection;
    procedure SetRunoffConcentration(const Value: TMt3dLktConcCollection);
    function GetInitialConcentrationObserver: TObserver;
    procedure SetInitialConcentrations(const Value: TLktInitConcCollection);
    property InitialConcentrationObserver: TObserver
      read GetInitialConcentrationObserver;
  protected
    // @name should not be called
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
//    procedure CreateFormulaObjects;
    procedure CreateObservers;
    function BoundaryObserverPrefix: string; override;
  public
    // @name should not be called
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
//    procedure InvalidateDisplay; override;
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    function Used: boolean; override;
    procedure RenameSpecies(const OldSpeciesName, NewSpeciesName: string); override;
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string); override;
    procedure ChangeSpeciesPosition(OldIndex, NewIndex: integer); override;
    procedure DeleteSpecies(SpeciesIndex: integer); override;
    procedure Clear; override;
    procedure Loaded;
  published
    property RunoffConcentration: TMt3dLktConcCollection
      read FRunoffConcentration write SetRunoffConcentration;
    property InitialConcentrations: TLktInitConcCollection
      read FInitialConcentrations write SetInitialConcentrations;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit, GIS_Functions, DataSetUnit;

resourcestring
  StrLKTInitialConcentr = 'LKT Initial Concentration';

const
  InitialConcentrationPosition = 0;

{ TMt3dLktConcCollection }

function TMt3dLktConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dLktConcTimeListLink;
end;

//procedure TMt3dLktConcCollection.InvalidateLktConc(Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  Link: TMt3dLktConcTimeListLink;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
////  ScreenTopDataArray: TDataArray;
//begin
//  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    Link := TimeListLink.GetLink(PhastModel) as TMt3dLktConcTimeListLink;
////    Link.FInitCondData.Invalidate;
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      Link := TimeListLink.GetLink(ChildModel) as TMt3dLktConcTimeListLink;
////      Link.FInitCondData.Invalidate;
//    end;
//
//  end;
//end;

class function TMt3dLktConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dLktConcItem;
end;

{ TMt3dLktConcBoundary }

procedure TMt3dLktConcBoundary.Assign(Source: TPersistent);
var
  LktSource: TMt3dLktConcBoundary;
begin
  if Source is TMt3dLktConcBoundary then
  begin
    LktSource := TMt3dLktConcBoundary(Source);
    RunoffConcentration := LktSource.RunoffConcentration;
//    InitialConcentration := LktSource.InitialConcentration;
    InitialConcentrations := LktSource.InitialConcentrations;
  end;
  inherited;
end;

procedure TMt3dLktConcBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  Assert(False);
end;

class function TMt3dLktConcBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMt3dLktConcCollection;
end;

function TMt3dLktConcBoundary.BoundaryObserverPrefix: string;
begin
  result := 'LKT_';
end;

procedure TMt3dLktConcBoundary.ChangeSpeciesPosition(OldIndex,
  NewIndex: integer);
var
  LktInitConcItem: TLktInitConcItem;
begin
  inherited;
  FRunoffConcentration.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  FRunoffConcentration.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);

  LktInitConcItem := FInitialConcentrations.Items[OldIndex];
  LktInitConcItem.Index := NewIndex;

end;

procedure TMt3dLktConcBoundary.Clear;
begin
  inherited;

  FRunoffConcentration.Clear;
  FInitialConcentrations.Clear;
//  InitialConcentration := '0';
end;

constructor TMt3dLktConcBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
//  FInitConcFormulas := TList<TFormulaObject>.Create;
  inherited;
  CreateBoundaryObserver;
//  CreateFormulaObjects;
  CreateObservers;
  FInitialConcentrations := TLktInitConcCollection.Create(Self, Model, ScreenObject);

  FRunoffConcentration:= TMt3dLktConcCollection.Create(self, Model,
    ScreenObject);
//  InitialConcentration := '0';
end;

//procedure TMt3dLktConcBoundary.CreateFormulaObjects;
//begin
//  FInitialConcentration := CreateFormulaObjectBlocks(dso3D);
//end;

procedure TMt3dLktConcBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(InitialConcentrationObserver);
  end;
end;

procedure TMt3dLktConcBoundary.DeleteSpecies(SpeciesIndex: integer);
begin
  inherited;
  RunoffConcentration.DeleteSpecies(SpeciesIndex);

  FInitialConcentrations.Delete(SpeciesIndex);
end;

destructor TMt3dLktConcBoundary.Destroy;
begin
  FRunoffConcentration.Free;
  FInitialConcentrations.Free;
  inherited;
end;

procedure TMt3dLktConcBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  Assert(False);
end;

//function TMt3dLktConcBoundary.GetChemSpeciesCount: Integer;
//begin
//
//end;

//function TMt3dLktConcBoundary.GetInitialConcentration: string;
//begin
//  Result := FInitialConcentration.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(InitialConcentrationPosition);
//  end;
//end;

//function TMt3dLktConcBoundary.GetInitialConcentrationFormula(
//  Index: Integer): string;
//begin
//  Result := FInitConcFormulas[Index].Formula;
//  if ScreenObject <> nil then
//  begin
////    ResetItemObserver(DiversionStartPosition+Index);
//  end;
//end;

function TMt3dLktConcBoundary.GetInitialConcentrationObserver: TObserver;
begin
  if FInitialConcentrationObserver = nil then
  begin
    { TODO -cFootprint : Replace nil with a data array or other observer}
    CreateObserver('Lake_Transport_Initial_Concentration',
      FInitialConcentrationObserver, nil);
  end;
  result := FInitialConcentrationObserver;
end;

procedure TMt3dLktConcBoundary.InsertNewSpecies(SpeciesIndex: integer;
  const Name: string);
var
  LktInitConcItem: TLktInitConcItem;
begin
  inherited;
  RunoffConcentration.InsertNewSpecies(SpeciesIndex, Name);

  LktInitConcItem := FInitialConcentrations.Add as TLktInitConcItem;
  LktInitConcItem.Index := SpeciesIndex;
end;

procedure TMt3dLktConcBoundary.Loaded;
begin
  FInitialConcentrations.Loaded;
end;

procedure TMt3dLktConcBoundary.RenameSpecies(const OldSpeciesName,
  NewSpeciesName: string);
//var
//  Concentrations: TCustomMt3dmsConcCollection;
begin
  inherited;
  RunoffConcentration.RenameTimeList(OldSpeciesName, NewSpeciesName);
  RunoffConcentration.RenameItems(OldSpeciesName, NewSpeciesName);
end;

//procedure TMt3dLktConcBoundary.SetChemSpeciesCount(const Value: Integer);
//begin
//
//end;

//procedure TMt3dLktConcBoundary.SetInitialConcentration(
//  const Value: string);
//begin
//  UpdateFormulaBlocks(Value, InitialConcentrationPosition, FInitialConcentration);
//
//end;

//procedure TMt3dLktConcBoundary.SetInitialConcentrationFormula(Index: Integer;
//  const Value: string);
//var
//  FormulaObject: TFormulaObject;
//begin
//  FormulaObject := FInitConcFormulas[Index];
////  UpdateFormula(Value, DiversionStartPosition+Index, FormulaObject);
//  FInitConcFormulas[Index] := FormulaObject;
//end;

procedure TMt3dLktConcBoundary.SetInitialConcentrations(
  const Value: TLktInitConcCollection);
begin
  FInitialConcentrations.Assign(Value);
end;

procedure TMt3dLktConcBoundary.SetRunoffConcentration(
  const Value: TMt3dLktConcCollection);
begin
  FRunoffConcentration.Assign(Value);
end;

function TMt3dLktConcBoundary.Used: boolean;
begin
  result := inherited Used or (RunoffConcentration.Count > 0)
    or (InitialConcentrations.Count > 0)
//    or (InitialConcentration <> '0');
end;

{ TLktInitConcItem }

procedure TLktInitConcItem.Assign(Source: TPersistent);
var
  LktItem: TLktInitConcItem;
  Index: integer;
begin
  // if Assign is updated, update IsSame too.
  if Source is TLktInitConcItem then
  begin
    LktItem := TLktInitConcItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := LktItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TLktInitConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLktInitConcCollection;
  InitConcObserver: TObserver;
//  ScreenTopObserver: TObserver;
//  SkinKObserver: TObserver;
//  SkinRadiusObserver: TObserver;
begin
  ParentCollection := Collection as TLktInitConcCollection;

  InitConcObserver := FObserverList[InitConcPosition];
  InitConcObserver.OnUpToDateSet := ParentCollection.InvalidateInitConcData;

end;

function TLktInitConcItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

constructor TLktInitConcItem.Create(Collection: TCollection);
begin
  inherited;
  StartTime := -1;
  EndTime := 0;
end;

procedure TLktInitConcItem.CreateFormulaObjects;
begin
  FInitConc := CreateFormulaObject(dso3D);
end;

destructor TLktInitConcItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TLktInitConcItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    InitConcPosition:
      result := InitConc;
    else Assert(False);
  end;
end;

function TLktInitConcItem.GetInitConc: string;
begin
  Result := FInitConc.Formula;
  ResetItemObserver(InitConcPosition);
end;

procedure TLktInitConcItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FInitConc then
  begin
    List.Add(FObserverList[InitConcPosition]);
  end;
end;

procedure TLktInitConcItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TLktInitConcItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TLktInitConcItem;
  Index: integer;
begin
  result := (AnotherItem is TLktInitConcItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TLktInitConcItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TLktInitConcItem.Loaded;
var
//  ScreenBottomObserver: TObserver;
  InitConcObserver: TObserver;
//  SkinKObserver: TObserver;
//  SkinRadiusObserver: TObserver;
  ParentCollection: TLktInitConcCollection;
  ScreenObject: TScreenObject;
//  InitConcDataArray: TDataArray;
begin
  ParentCollection := Collection as TLktInitConcCollection;
  ScreenObject := ParentCollection.ScreenObject as TScreenObject;

  InitConcObserver := FObserverList[InitConcPosition];
  ScreenObject.TalksTo(InitConcObserver);
  InitConcObserver.OnUpToDateSet := ParentCollection.InvalidateInitConcData;
//  InitConcDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenBottom);
//  if InitConcDataArray <> nil then
//  begin
//    InitConcObserver.TalksTo(InitConcDataArray);
//  end;
end;

procedure TLktInitConcItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitConc,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TLktInitConcItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    InitConcPosition:
      InitConc := Value;
    else Assert(False);
  end;
end;

procedure TLktInitConcItem.SetInitConc(const Value: string);
begin
  UpdateFormulaBlocks(Value, InitConcPosition, FInitConc);
end;

{ TLktInitConcCollection }

procedure TLktInitConcCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TLktInitConcStorage.Create(AModel));
end;

function TLktInitConcCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TLktInitConcItem;
begin
  Item := Items[ItemIndex];
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TLktInitConcCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
begin
  // does anything need to be done here?
  // called from TCustomListArrayBoundColl.AssignArrayCellsWithItem
  // which is called by TCustomListArrayBoundColl.EvaluateArrayBoundaries
//  inherited;

end;

procedure TLktInitConcCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  LktInitConcStorage: TLktInitConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in
    [TLktInitConcItem.InitConcPosition]);
  Assert(Expression <> nil);

  LktInitConcStorage := BoundaryStorage as TLktInitConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with LktInitConcStorage.LktInitConcArray[Index] do
    begin
      case BoundaryFunctionIndex of
        TLktInitConcItem.InitConcPosition:
          begin
            InitialConcentration := Expression.DoubleResult;
            InitialConcentrationAnnotation := ACell.Annotation;
            InitialConcentrationPestName := PestName;
            InitialConcentrationPestSeriesName := PestSeriesName;
            InitialConcentrationPestSeriesMethod := PestSeriesMethod;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TLktInitConcCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  LktIntConcStorage: TLktInitConcStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  LktIntConcStorage := BoundaryStorage as TLktInitConcStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
//    if ACell.LgrEdge then
//    begin
//      Continue;
//    end;
    with LktIntConcStorage.FLktInitConcArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TLktInitConcCollection.GetItem(Index: Integer): TLktInitConcItem;
begin
  result := inherited Items[Index] as TLktInitConcItem
end;

function TLktInitConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TLktIntConcTimeListLink;
end;

procedure TLktInitConcCollection.InvalidateInitConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLktIntConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
//  ScreenTopDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TLktIntConcTimeListLink;
    Link.FInitCondData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLktIntConcTimeListLink;
      Link.FInitCondData.Invalidate;
    end;

//    ScreenTopDataArray := PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenTop);
//    if ScreenTopDataArray <> nil then
//    begin
//      ScreenTopDataArray.Invalidate;
//    end;
  end;
end;

procedure TLktInitConcCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

class function TLktInitConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLktInitConcItem;
end;

procedure TLktInitConcCollection.Loaded;
var
  ItemIndex: Integer;
  LktItem: TLktInitConcItem;
  LakeTransportConc: TDataArray;
  DSIndex: Integer;
  LocalScreenObject: TScreenObject;
begin
  for ItemIndex := 0 to Count -1 do
  begin
    (Items[ItemIndex] as TLktInitConcItem).Loaded;
  end;
  
  if Count > 0 then
  begin
    LktItem := Items[0] as TLktInitConcItem;
    LakeTransportConc := (Model as TCustomModel).DataArrayManager.GetDataSetByName(KLakeTransportConce);
    LocalScreenObject := ScreenObject as TScreenObject; 
    DSIndex := LocalScreenObject.AddDataSet(LakeTransportConc);
    LocalScreenObject.DataSetFormulas[DSIndex] := LktItem.InitConc;
  end;
end;

procedure TLktInitConcCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel]
    as TLktInitConcStorage).FLktInitConcArray, BoundaryCount);
  inherited;
end;

procedure TLktInitConcCollection.SetItem(Index: Integer;
  const Value: TLktInitConcItem);
begin
  inherited Items[Index] := Value;
end;

function TLktInitConcCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

{ TLktIntConcTimeListLink }

procedure TLktIntConcTimeListLink.CreateTimeLists;
begin
  inherited;
  FInitCondData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInitCondData.NonParamDescription := StrLKTInitialConcentr;
  FInitCondData.ParamDescription := StrLKTInitialConcentr;
  if Model <> nil then
  begin
//    FInitCondData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FInitCondData);
end;

destructor TLktIntConcTimeListLink.Destroy;
begin
  FInitCondData.Free;
  inherited;
end;

{ TLktInitConcRecord }

procedure TLktInitConcRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, InitialConcentration);

  WriteCompInt(Comp, Strings.IndexOf(InitialConcentrationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(InitialConcentrationPestName));
  WriteCompInt(Comp, Strings.IndexOf(InitialConcentrationPestSeriesName));
  WriteCompInt(Comp, Ord(InitialConcentrationPestSeriesMethod));
//  WriteCompInt(Comp, Strings.IndexOf(ScreenBottomAnnotation));
//  WriteCompInt(Comp, Strings.IndexOf(SkinKAnnotation));
//  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusAnnotation));
//  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusAnnotation));
end;

procedure TLktInitConcRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(InitialConcentrationAnnotation);
  Strings.Add(InitialConcentrationPestName);
  Strings.Add(InitialConcentrationPestSeriesName);
//  Strings.Add(ScreenBottomAnnotation);
//  Strings.Add(SkinKAnnotation);
//  Strings.Add(SkinRadiusAnnotation);
end;

procedure TLktInitConcRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);

  InitialConcentration := ReadCompReal(Decomp);

  InitialConcentrationAnnotation := Annotations[ReadCompInt(Decomp)];
  InitialConcentrationPestName := Annotations[ReadCompInt(Decomp)];
  InitialConcentrationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  InitialConcentrationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TLktInitConcStorage }

procedure TLktInitConcStorage.Clear;
begin
  SetLength(FLktInitConcArray, 0);
  FCleared := True;
end;

function TLktInitConcStorage.GetLktInitConcArray: TLktInitConcArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FLktInitConcArray;
end;

procedure TLktInitConcStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FLktInitConcArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FLktInitConcArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TLktInitConcStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FLktInitConcArray);
    for Index := 0 to Count - 1 do
    begin
      FLktInitConcArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FLktInitConcArray[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

{ TMt3dLktConcItem }

procedure TMt3dLktConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMt3dLktConcCollection;
//  InitConcObserver: TObserver;
//  ScreenTopObserver: TObserver;
//  SkinKObserver: TObserver;
//  SkinRadiusObserver: TObserver;
begin
  ParentCollection := Collection as TMt3dLktConcCollection;

//  InitConcObserver := FObserverList[InitConcPosition];
//  InitConcObserver.OnUpToDateSet := ParentCollection.InvalidateLktConc;
end;

{ TMt3dLktConcTimeListLink }

procedure TMt3dLktConcTimeListLink.CreateTimeLists;
var
  Index: Integer;
//  Mt3dmsConcData: TModflowTimeList;
//  Item: TChemSpeciesItem;
  LocalModel: TPhastModel;
begin
  TimeLists.Clear;
//  FListOfTimeLists.Clear;
  LocalModel := frmGoPhast.PhastModel;
  for Index := 0 to LocalModel.MobileComponents.Count - 1 do
  begin
//    Item := LocalModel.MobileComponents[Index];
//    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
//    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
//    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
//    if Model <> nil then
//    begin
//      Mt3dmsConcData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateMt3dmsChemSources;
//    end;
//    AddTimeList(Mt3dmsConcData);
//    FListOfTimeLists.Add(Mt3dmsConcData);
    AddTimeList(nil);
//    FListOfTimeLists.Add(nil);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
//    Item := LocalModel.ImmobileComponents[Index];
//    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
//    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
//    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
//    if Model <> nil then
//    begin
//      Mt3dmsConcData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateMt3dmsChemSources;
//    end;
//    AddTimeList(Mt3dmsConcData);
//    FListOfTimeLists.Add(Mt3dmsConcData);
    AddTimeList(nil);
//    FListOfTimeLists.Add(nil);
  end;
end;

end.
