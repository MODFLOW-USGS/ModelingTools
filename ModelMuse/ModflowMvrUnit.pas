unit ModflowMvrUnit;

interface

uses
  ModflowBoundaryUnit, OrderedCollectionUnit, System.Classes,
  FormulaManagerUnit, GoPhastTypes, SubscriptionUnit, RbwParser,
  ModflowCellUnit, System.ZLib;

type
  TMvrType = (mtFactor, mtExcess, mtThreshold, mtUpTo);
  TMvrTypeArray = array of TMvrType;

  TSfrReceiverChoice = (srcFirst, srcNearest);

  TSourcePackageChoice = (spcWel, spcDrn, spcRiv, spcGhb, spcLak, spcMaw,
    spcSfr, spcUzf);
  TSourcePackageChoices = set of TSourcePackageChoice;

  TReceiverPackageChoice = (rpcLak, rpcMaw, rpcSfr, rpcUzf);

  TMvrRecord = record
    Cell: TCellLocation;
    Values : TOneDRealArray;
    MvrTypes: TMvrTypeArray;
    StartingTime: double;
    EndingTime: double;
    MvrIndex: Integer;
    ValueAnnotations: array of string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMvrRecordArray = array of TMvrRecord;

  TMvrSourceStorage = class(TCustomBoundaryStorage)
  private
    FMvrRecordArray: TMvrRecordArray;
    function GetMvrRecordArray: TMvrRecordArray;
    function GetCount: Integer;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property MvrRecordArray: TMvrRecordArray read GetMvrRecordArray;
    property Count: Integer read GetCount;
  end;

  TReceiverItem = class(TOrderedItem)
  private
    FLakeOutlet: Integer;
    FReceiverPackage: TReceiverPackageChoice;
    FSfrReceiverChoice: TSfrReceiverChoice;
    FReceiverObject: TObject;
    FReceiverObjectName: string;
    procedure SetLakeOutlet(const Value: Integer);
    procedure SetReceiverPackage(const Value: TReceiverPackageChoice);
    procedure SetSfrReceiverChoice(const Value: TSfrReceiverChoice);
    function GetReceiverObjectName: string;
    procedure SetReceiverObjectName(const Value: string);
    function GetReceiverObject: TObject;
    procedure SetReceiverObject(const Value: TObject);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    property ReceiverObject: TObject read GetReceiverObject write SetReceiverObject;
    procedure Loaded(AModel: TBaseModel);
  published
    property ReceiverPackage: TReceiverPackageChoice read FReceiverPackage write SetReceiverPackage;
    property SfrReceiverChoice: TSfrReceiverChoice read FSfrReceiverChoice write SetSfrReceiverChoice;
    property LakeOutlet: Integer read FLakeOutlet write SetLakeOutlet;
    property ReceiverObjectName: string read GetReceiverObjectName
      write SetReceiverObjectName;
  end;

  TReceiverCollection = class(TOrderedCollection)
  private
    function GetItem(Index: Integer): TReceiverItem;
    procedure SetItem(Index: Integer; const Value: TReceiverItem);
  public
    procedure Loaded(AModel: TBaseModel);
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TReceiverItem read GetItem write SetItem; default;
    function Add: TReceiverItem;
  end;

  TIndividualMvrItems = class;

  TIndividualMvrItem = class(TFormulaOrderedItem)
  private
    FMvrType: TMvrType;
    FValue: TFormulaObject;
    FObserver: TObserver;
    function GetValue: string;
    procedure SetMvrType(const Value: TMvrType);
    procedure SetValue(const Value: string);
    function ItemCollection: TIndividualMvrItems;
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
    property MvrType: TMvrType read FMvrType write SetMvrType;
    property Value: string read GetValue write SetValue;
  end;

  TIndividualMvrItems = class(TOrderedCollection)
  private
    FScreenObject: TObject;
    FMvrItems: TCollection;
    function GetItem(Index: Integer): TIndividualMvrItem;
    procedure SetItem(Index: Integer; const Value: TIndividualMvrItem);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      MvrItems: TCollection);
    property Items[Index:Integer]: TIndividualMvrItem read GetItem write SetItem; default;
    function IndexOfFormulaObject(AFormulaObject: TFormulaObject): integer;
    function Add: TIndividualMvrItem;
  end;

  TMvrItem = class(TCustomModflowBoundaryItem)
  private
    FItems: TIndividualMvrItems;
    procedure SetItems(const Value: TIndividualMvrItems);
    procedure UpdateObservers;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Items: TIndividualMvrItems read FItems write SetItems;
  end;

  TMvrTimeListLink = class(TTimeListsModelLink)
  private
    FListOfTimeLists: TList;
  protected
    property ListOfTimeLists: TList read FListOfTimeLists;
    procedure CreateTimeLists; override;
  public
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
    Destructor Destroy; override;
  end;

  TMvrItems = class(TCustomMF_ListBoundColl)
  private
    function GetItems(Index: Integer): TMvrItem;
    procedure SetItems(Index: Integer; const Value: TMvrItem);
    procedure InvalidateMvrData(Sender: TObject);
    procedure UpdateObservers;
  protected
    class function ItemClass: TBoundaryItemClass; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
        AModel: TBaseModel); override;
    procedure CreateTimeLists;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    procedure AssignDirectlySpecifiedValues(AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
  public
    property Items[Index: Integer]: TMvrItem read GetItems write SetItems; default;
  end;

  TMvrSourceCell = class(TValueCell)
  private
    FValues: TMvrRecord;
    FStressPeriod: integer;
    function GetMvrValue(Index: integer): double;
    function GetValueAnnotation(Index: integer): string;
    function GetMvrType(Index: Integer): TMvrType;
    function GetMvrValueCount: Integer;
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
    property MvrValues[Index: integer]: double read GetMvrValue;
    property MvrTypes[Index: Integer]: TMvrType read GetMvrType;
    property ValueAnnotations[Index: integer]: string
      read GetValueAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property Values: TMvrRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    procedure SetValueLength(ALength: Integer);
    property MvrIndex: Integer read FValues.MvrIndex;
    property MvrValueCount: Integer read GetMvrValueCount;
  end;

  TMvrBoundary = class(TModflowBoundary)
  private
    FSourcePackageChoice: TSourcePackageChoice;
    FReceivers: TReceiverCollection;
    procedure SetSourcePackageChoice(const Value: TSourcePackageChoice);
    procedure SetReceivers(const Value: TReceiverCollection);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure Loaded;
    procedure UpdateObservers;
  published
    property SourcePackageChoice: TSourcePackageChoice read FSourcePackageChoice
      write SetSourcePackageChoice;
    property Receivers: TReceiverCollection read FReceivers write SetReceivers;
  end;

procedure MvrStringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure MvrStringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses
  ScreenObjectUnit, frmGoPhastUnit, System.Contnrs,
  System.SysUtils, PhastModelUnit, ModflowTimeUnit, GIS_Functions;

const
  ValuePosition = 0;

{ TMvrItem }

procedure TIndividualMvrItem.Assign(Source: TPersistent);
var
  SourceItem: TIndividualMvrItem;
begin
  if Source is TIndividualMvrItem then
  begin
    SourceItem := TIndividualMvrItem(Source);
    MvrType := SourceItem.MvrType;
    Value := SourceItem.Value;
  end;
  inherited;
end;

constructor TIndividualMvrItem.Create(Collection: TCollection);
var
  SCollection: TIndividualMvrItems;
  LocalScreenObject: TScreenObject;
  MvrItems: TMvrItems;
begin
  inherited;
  FObserver:= TObserver.Create(nil);
  SCollection := ItemCollection;

  MvrItems := SCollection.FMvrItems as TMvrItems;
  FObserver.OnUpToDateSet := MvrItems.InvalidateMvrData;
  LocalScreenObject := MvrItems.ScreenObject as TScreenObject;

  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(FObserver);
  end;

  OnRemoveSubscription := MvrStringValueRemoveSubscription;
  OnRestoreSubscription := MvrStringValueRestoreSubscription;

  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
  FValue.AddSubscriptionEvents(MvrStringValueRemoveSubscription,
    MvrStringValueRestoreSubscription, self);
end;

destructor TIndividualMvrItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  SCollection: TIndividualMvrItems;
  MvrItems: TMvrItems;
begin
  Value := '0';
  SCollection := ItemCollection;
  MvrItems := SCollection.FMvrItems as TMvrItems;
  LocalScreenObject := MvrItems.ScreenObject as TScreenObject;

  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(FObserver);
  end;
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    MvrStringValueRemoveSubscription,
    MvrStringValueRestoreSubscription, self);
  FObserver.Free;
  inherited;
end;

function TIndividualMvrItem.GetObserver(Index: Integer): TObserver;
begin
  Result := FObserver;
end;

function TIndividualMvrItem.GetScreenObject: TObject;
begin
  result := ItemCollection.FScreenObject;
end;

function TIndividualMvrItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TIndividualMvrItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TIndividualMvrItem;
begin
  result := (AnotherItem is TIndividualMvrItem);
  if result then
  begin
    SourceItem := TIndividualMvrItem(AnotherItem);
    result := (MvrType = SourceItem.MvrType)
      and (Value = SourceItem.Value);
  end;
end;

function TIndividualMvrItem.ItemCollection: TIndividualMvrItems;
begin
  result := Collection as TIndividualMvrItems;
end;

procedure TIndividualMvrItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TIndividualMvrItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TIndividualMvrItem.SetMvrType(const Value: TMvrType);
begin
  if FMvrType <> Value then
  begin
    FMvrType := Value;
    InvalidateModel;
  end;
end;

procedure TIndividualMvrItem.SetValue(const Value: string);
var
  Dummy: integer;
begin
  Dummy := 0;
  UpdateFormula(Value, Dummy, FValue);
end;

{ TMvrItem }

procedure TMvrItem.Assign(Source: TPersistent);
begin
  if Source is TMvrItem then
  begin
    Items := TMvrItem(Source).Items;
  end;
  inherited;
end;

procedure TMvrItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMvrItems;
  MvrObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TMvrItems;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    MvrObserver := FObserverList[Index];
    MvrObserver.OnUpToDateSet :=
      ParentCollection.InvalidateMvrData;
  end;
end;

function TMvrItem.BoundaryFormulaCount: integer;
var
  BoundaryGroup: TMvrBoundary;
  LocalScreenObject: TScreenObject;
  Observer: TObserver;
begin
  BoundaryGroup := (Collection as TMvrItems).BoundaryGroup as TMvrBoundary;
  if BoundaryGroup <> nil then
  begin
    Result := BoundaryGroup.Receivers.Count;
    if (FObserverList <> nil) and (Result > FObserverList.Count) then
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      while Result > FObserverList.Count do
      begin
        Observer := TObserver.Create(nil);
        FObserverList.Add(Observer);
        if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
        begin
          LocalScreenObject.TalksTo(Observer);
        end;
      end;
      AssignObserverEvents(Collection);
    end;
  end
  else
  begin
    Result := 1;
  end;
end;

procedure TMvrItem.CreateFormulaObjects;
var
  Index: integer;
begin
  if FItems = nil then
  begin
    FItems := TIndividualMvrItems.Create(
      Model, ScreenObject, Collection);
  end;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    {Item :=} FItems.Add;
  end;
end;

destructor TMvrItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TMvrItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TIndividualMvrItem;
begin
  Item := FItems[Index];
  result := Item.Value;
  ResetItemObserver(Index);
end;

procedure TMvrItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Position: Integer;
begin
  Position := FItems.IndexOfFormulaObject(
    Sender as TFormulaObject);

  Assert(Position >= 0);
  List.Add(FObserverList[Position]);
end;

function TMvrItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TMvrItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    result := Items.IsSame(TMvrItem(AnotherItem).Items);
  end;
end;

procedure TMvrItem.RemoveFormulaObjects;
begin
  FItems.Clear;
end;

procedure TMvrItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TIndividualMvrItem;
begin
  Item := FItems[Index];
  Item.Value := Value;
end;

procedure TMvrItem.SetItems(const Value: TIndividualMvrItems);
begin
  FItems.Assign(Value);
end;

procedure TMvrItem.UpdateObservers;
begin
  BoundaryFormulaCount;
end;

{ TMvrBoundary }

procedure TMvrBoundary.Assign(Source: TPersistent);
var
  MvrSource: TMvrBoundary;
begin
  if Source is TMvrBoundary then
  begin
    MvrSource := TMvrBoundary(Source);
    SourcePackageChoice := MvrSource.SourcePackageChoice;
    Receivers := MvrSource.Receivers;
  end;
  inherited;
end;

procedure TMvrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  LocalBoundaryStorage: TMvrSourceStorage;
  LocalModel: TCustomModel;
  TimeIndex: Integer;
  Cells: TValueCellList;
  StressPeriod: TModflowStressPeriod;
  BoundaryIndex: Integer;
  Cell: TMvrSourceCell;
  BoundaryValues: TMvrRecord;
begin
    LocalModel := AModel as TCustomModel;

//    Grid := LocalModel.Grid;
    LocalBoundaryStorage := BoundaryStorage as TMvrSourceStorage;
    for TimeIndex := 0 to
      LocalModel.ModflowFullStressPeriods.Count - 1 do
    begin
      if TimeIndex < ValueTimeList.Count then
      begin
        Cells := ValueTimeList[TimeIndex];
      end
      else
      begin
        Cells := TValueCellList.Create(TMvrSourceCell);
        ValueTimeList.Add(Cells);
      end;
      StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
//      // Check if the stress period is completely enclosed within the times
//      // of the LocalBoundaryStorage;
      if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
        and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
      begin
        if Cells.Capacity < Cells.Count
          + Length(LocalBoundaryStorage.MvrRecordArray) then
        begin
          Cells.Capacity := Cells.Count
            + Length(LocalBoundaryStorage.MvrRecordArray)
        end;
        for BoundaryIndex := 0 to
          Length(LocalBoundaryStorage.MvrRecordArray) - 1 do
        begin
          BoundaryValues := LocalBoundaryStorage.MvrRecordArray[BoundaryIndex];
          BoundaryValues.MvrIndex := BoundaryIndex;
          Cell := TMvrSourceCell.Create;
          Cells.Add(Cell);
//          Cell.BoundaryTypes.Assign(BoundaryTypes);
          LocalModel.AdjustCellPosition(Cell);
//          Cell.IFace := LocalScreenObject.IFace;
          Cell.StressPeriod := TimeIndex;
          Cell.Values := BoundaryValues;
          Cell.ScreenObject := ScreenObject;
          Cell.SetValueLength(Length(Cell.Values.Values));
        end;
        Cells.Cache;
      end;
    end
end;

class function TMvrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMvrItems;
end;

constructor TMvrBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FReceivers := TReceiverCollection.Create(Model);
end;

destructor TMvrBoundary.Destroy;
begin
  FReceivers.Free;
  inherited;
end;

procedure TMvrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TMvrSourceStorage;
  ValueCount: Integer;
begin
  EvaluateListBoundaries(AModel);

  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TMvrSourceStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
  end
end;

procedure TMvrBoundary.Loaded;
begin
  Receivers.Loaded(ParentModel as TCustomModel);
end;

procedure TMvrBoundary.SetReceivers(const Value: TReceiverCollection);
begin
  FReceivers.Assign(Value);
end;

procedure TMvrBoundary.SetSourcePackageChoice(
  const Value: TSourcePackageChoice);
begin
  if FSourcePackageChoice <> Value then
  begin
    FSourcePackageChoice := Value;
    InvalidateModel;
  end;
end;

procedure TMvrBoundary.UpdateObservers;
begin
  (Values as TMvrItems).UpdateObservers;
end;

{ TMvrItems }

function TIndividualMvrItems.Add: TIndividualMvrItem;
begin
  result := inherited Add as TIndividualMvrItem
end;

procedure TIndividualMvrItems.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TIndividualMvrItems.Create(Model: TBaseModel; ScreenObject: TObject;
  MvrItems: TCollection);
begin
  inherited Create(TIndividualMvrItem, Model);
  FScreenObject := ScreenObject;
  FMvrItems := MvrItems;
end;

function TIndividualMvrItems.GetItem(Index: Integer): TIndividualMvrItem;
begin
  result := inherited Items[Index] as TIndividualMvrItem;
end;

function TIndividualMvrItems.IndexOfFormulaObject(
  AFormulaObject: TFormulaObject): integer;
var
  Index: Integer;
  AnItem: TIndividualMvrItem;
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

procedure TIndividualMvrItems.SetItem(Index: Integer;
  const Value: TIndividualMvrItem);
begin
  inherited Items[Index] := value;
end;

{ TMvrItems }

procedure TMvrItems.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMvrSourceStorage.Create(AModel));
end;

function TMvrItems.AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
var
  Item: TMvrItem;
begin
  Item := Items[ItemIndex] as TMvrItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TMvrItems.AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  Assert(False);
//  inherited;

end;

procedure TMvrItems.AssignCellList(Expression: TExpression; ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
  Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject);
var
  MvrStorage: TMvrSourceStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
//  Assert(BoundaryFunctionIndex in [HeadPosition,ConductancePosition]);
  Assert(Expression <> nil);

  MvrStorage := BoundaryStorage as TMvrSourceStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with MvrStorage.MvrRecordArray[Index] do
    begin
      Assert(BoundaryFunctionIndex < Length(Values));
      Values[BoundaryFunctionIndex] := Expression.DoubleResult;
      ValueAnnotations[BoundaryFunctionIndex] := ACell.Annotation;
    end;
  end;
end;

procedure TMvrItems.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem; BoundaryStorage: TCustomBoundaryStorage);
var
  MvrItem: TMvrItem;
  MvrStorage: TMvrSourceStorage;
  IndIndex: Integer;
  IndItem: TIndividualMvrItem;
  CellIndex: Integer;
begin
//  inherited;
  MvrItem := AnItem as TMvrItem;
  MvrStorage := BoundaryStorage as TMvrSourceStorage;
  for IndIndex := 0 to MvrItem.Items.Count - 1 do
  begin
    IndItem := MvrItem.Items[IndIndex];
    for CellIndex := 0 to MvrStorage.Count - 1 do
    begin
      MvrStorage.MvrRecordArray[CellIndex].MvrTypes[IndIndex]
        := IndItem.MvrType;
    end;
//    MvrStorage
  end;
end;

procedure TMvrItems.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  MvrStorage: TMvrSourceStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  MvrStorage := BoundaryStorage as TMvrSourceStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with MvrStorage.MvrRecordArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TMvrItems.CreateTimeLists;
begin
  Assert(False);
end;

function TMvrItems.GetItems(Index: Integer): TMvrItem;
begin
  result := inherited Items[Index] as TMvrItem
end;

function TMvrItems.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMvrTimeListLink;
end;

procedure TMvrItems.InvalidateMvrData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMvrTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TMvrTimeListLink;
    for Index := 0 to Link.FListOfTimeLists.Count - 1 do
    begin
      TimeList := Link.FListOfTimeLists[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMvrTimeListLink;
      for Index := 0 to Link.FListOfTimeLists.Count - 1 do
      begin
        TimeList := Link.FListOfTimeLists[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

class function TMvrItems.ItemClass: TBoundaryItemClass;
begin
  result := TMvrItem;
end;

procedure TMvrItems.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
//  LocalModel: TCustomModel;
  ReceiverCount: integer;
  BoundaryIndex: integer;
  MvrRecordArray: TMvrRecordArray;
begin
  SetLength((Boundaries[ItemIndex, AModel]
    as TMvrSourceStorage).FMvrRecordArray, BoundaryCount);
//  LocalModel := Model as TCustomModel;
  ReceiverCount := (BoundaryGroup as TMvrBoundary).Receivers.count;
  MvrRecordArray := (Boundaries[ItemIndex, AModel]
    as TMvrSourceStorage).FMvrRecordArray;
  for BoundaryIndex := 0 to BoundaryCount - 1 do
  begin
    SetLength(MvrRecordArray[BoundaryIndex].Values, ReceiverCount);
    SetLength(MvrRecordArray[BoundaryIndex].MvrTypes, ReceiverCount);
    SetLength(MvrRecordArray[BoundaryIndex].ValueAnnotations,
      ReceiverCount);
  end;
  inherited;
end;

procedure TMvrItems.SetItems(Index: Integer; const Value: TMvrItem);
begin
  inherited Items[Index] := Value;
end;

procedure TMvrItems.UpdateObservers;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].UpdateObservers;
  end;
end;

{ TReceiverItem }

procedure TReceiverItem.Assign(Source: TPersistent);
var
  ReceiveSource: TReceiverItem;
begin
  if Source is TReceiverItem then
  begin
    ReceiveSource := TReceiverItem(Source);
    ReceiverPackage := ReceiveSource.ReceiverPackage;
    SfrReceiverChoice := ReceiveSource.SfrReceiverChoice;
    LakeOutlet := ReceiveSource.LakeOutlet;
    ReceiverObjectName := ReceiveSource.ReceiverObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TReceiverItem.GetReceiverObject: TObject;
begin
  result := FReceiverObject;
end;

function TReceiverItem.GetReceiverObjectName: string;
var
  ReceiverScreenObject: TScreenObject;
begin
  if FReceiverObject <> nil then
  begin
    ReceiverScreenObject := FReceiverObject as TScreenObject;
    if ReceiverScreenObject.Deleted then
    begin
      result := '';
    end
    else
    begin
      result := ReceiverScreenObject.Name;
    end;
  end
  else
  begin
    result := FReceiverObjectName
  end;
end;

function TReceiverItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  ReceiveSource: TReceiverItem;
begin
  result := AnotherItem is TReceiverItem;
  if result then
  begin
    ReceiveSource := TReceiverItem(AnotherItem);
    result := (ReceiverPackage = ReceiveSource.ReceiverPackage)
      and (SfrReceiverChoice = ReceiveSource.SfrReceiverChoice)
      and (LakeOutlet = ReceiveSource.LakeOutlet)
      and (ReceiverObjectName = ReceiveSource.ReceiverObjectName)
  end;
end;

procedure TReceiverItem.Loaded(AModel: TBaseModel);
begin
  ReceiverObject := (AModel as TCustomModel).GetScreenObjectByName(FReceiverObjectName);
end;

procedure TReceiverItem.SetLakeOutlet(const Value: Integer);
begin
  SetIntegerProperty(FLakeOutlet, Value);
end;

procedure TReceiverItem.SetReceiverObject(const Value: TObject);
var
  ReceiverScreenObject: TScreenObject;
begin
  if Value <> FReceiverObject then
  begin
    FReceiverObject := Value;
    InvalidateModel;
    if FReceiverObject <> nil then
    begin
      ReceiverScreenObject := FReceiverObject as TScreenObject;
      FReceiverObjectName := ReceiverScreenObject.Name;
    end
    else
    begin
      FReceiverObjectName := '';
    end;
  end
end;

procedure TReceiverItem.SetReceiverObjectName(const Value: string);
var
  LocalModel: TCustomModel;
begin
  if Value <> ReceiverObjectName then
  begin
    InvalidateModel;
    if Model <> nil then
    begin
      LocalModel := Model as TCustomModel;
      if csLoading in LocalModel.ComponentState then
      begin
        FReceiverObjectName := Value;
      end
      else
      begin
        ReceiverObject := LocalModel.GetScreenObjectByName(Value);
      end;
    end
    else
    begin
      FReceiverObjectName := Value;
    end;
  end;
end;

procedure TReceiverItem.SetReceiverPackage(const Value: TReceiverPackageChoice);
begin
  if FReceiverPackage <> Value then
  begin
    FReceiverPackage := Value;
    InvalidateModel;
  end;
end;

procedure TReceiverItem.SetSfrReceiverChoice(const Value: TSfrReceiverChoice);
begin
  if FSfrReceiverChoice <> Value then
  begin
    FSfrReceiverChoice := Value;
    InvalidateModel;
  end;
end;

{ TReceiverCollection }

function TReceiverCollection.Add: TReceiverItem;
begin
  result := inherited Add as TReceiverItem;
end;

constructor TReceiverCollection.Create(Model: TBaseModel);
begin
  inherited Create(TReceiverItem, Model);
end;

function TReceiverCollection.GetItem(Index: Integer): TReceiverItem;
begin
  result := inherited Items[Index] as TReceiverItem;
end;

procedure TReceiverCollection.Loaded(AModel: TBaseModel);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded(AModel);
  end;
end;

procedure TReceiverCollection.SetItem(Index: Integer;
  const Value: TReceiverItem);
begin
  inherited Items[Index] := Value;
end;

procedure MvrStringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TIndividualMvrItem).RemoveSubscription(Sender, AName);
end;  

procedure MvrStringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TIndividualMvrItem).RestoreSubscription(Sender, AName);
end;

{ TMvrTimeListLink }

constructor TMvrTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FListOfTimeLists := TObjectList.Create;
  inherited;
end;

procedure TMvrTimeListLink.CreateTimeLists;
var
  Index: Integer;
  MvrData: TModflowTimeList;
begin
  inherited;
  TimeLists.Clear;
  FListOfTimeLists.Clear;
  for Index := 0 to ((Boundary as TMvrItems).BoundaryGroup as TMvrBoundary).Receivers.Count - 1 do
  begin
    MvrData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    MvrData.NonParamDescription := Format('Receiver_%d', [Index+1]);
    MvrData.ParamDescription := MvrData.NonParamDescription;
    if Model <> nil then
    begin
//      MvrData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateMt3dmsChemSources;
    end;
    AddTimeList(MvrData);
    FListOfTimeLists.Add(MvrData);
  end;
end;

destructor TMvrTimeListLink.Destroy;
begin
  FListOfTimeLists.Free;
  inherited;
end;

{ TMvrRecord }

procedure TMvrRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Length(Values));
  for Index := 0 to Length(Values) - 1 do
  begin
    WriteCompReal(Comp, Values[Index]);
  end;
  for Index := 0 to Length(MvrTypes) - 1 do
  begin
    WriteCompInt(Comp, Ord(MvrTypes[Index]));
  end;
  for Index := 0 to Length(ValueAnnotations) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValueAnnotations[Index]));
  end;
  WriteCompInt(Comp, MvrIndex);
end;

procedure TMvrRecord.RecordStrings(Strings: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to Length(ValueAnnotations) - 1 do
  begin
    Strings.Add(ValueAnnotations[Index]);
  end;
end;

procedure TMvrRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  Count := ReadCompInt(Decomp);
  SetLength(Values, Count);
  SetLength(MvrTypes, Count);
  SetLength(ValueAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    Values[Index] := ReadCompReal(Decomp);
  end;
  for Index := 0 to Count - 1 do
  begin
    MvrTypes[Index] := TMvrType(ReadCompInt(Decomp));
  end;
  for Index := 0 to Count - 1 do
  begin
    ValueAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  MvrIndex := ReadCompInt(Decomp)
end;

{ TMvrSourceStorage }

procedure TMvrSourceStorage.Clear;
begin
  SetLength(FMvrRecordArray, 0);
  FCleared := True;
end;

function TMvrSourceStorage.GetCount: Integer;
begin
  result := Length(FMvrRecordArray);
end;

function TMvrSourceStorage.GetMvrRecordArray: TMvrRecordArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMvrRecordArray;
end;

procedure TMvrSourceStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMvrRecordArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FMvrRecordArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMvrSourceStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FMvrRecordArray);
    for Index := 0 to Count - 1 do
    begin
      FMvrRecordArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMvrRecordArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TMvrSourceCell }

procedure TMvrSourceCell.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TMvrSourceCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TMvrSourceCell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TMvrSourceCell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TMvrSourceCell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TMvrSourceCell.GetMvrType(Index: Integer): TMvrType;
begin
  result := FValues.MvrTypes[Index];
end;

function TMvrSourceCell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := ValueAnnotations[Index];
end;

function TMvrSourceCell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := MvrValues[Index];
end;

function TMvrSourceCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TMvrSourceCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TMvrSourceCell.GetMvrValue(Index: integer): double;
begin
  result := FValues.Values[Index];
end;

function TMvrSourceCell.GetMvrValueCount: Integer;
begin
  result := Length(FValues.Values);
end;

function TMvrSourceCell.GetValueAnnotation(Index: integer): string;
begin
  result := FValues.ValueAnnotations[Index];
end;

function TMvrSourceCell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  MvrCell: TMvrSourceCell;
  Index: Integer;
begin
  result := AnotherCell is TMvrSourceCell;
  if result then
  begin
    MvrCell := TMvrSourceCell(AnotherCell);
//    result := IFace = MvrCell.IFace;
    if result then
    begin
      for Index := 0 to Length(FValues.Values) - 1 do
      begin
        result := (FValues.Values[Index] = MvrCell.FValues.Values[Index])
          and (FValues.MvrTypes[Index] = MvrCell.FValues.MvrTypes[Index]);
        if not result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TMvrSourceCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TMvrSourceCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TMvrSourceCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TMvrSourceCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TMvrSourceCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

procedure TMvrSourceCell.SetValueLength(ALength: Integer);
begin
  SetLength(FValues.Values, ALength);
  SetLength(FValues.MvrTypes, ALength);
  SetLength(FValues.ValueAnnotations, ALength);
end;

end.
