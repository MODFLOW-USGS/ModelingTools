unit ModflowTvkUnit;

interface

uses
  Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, GoPhastTypes, ModflowCellUnit,
  FormulaManagerInterfaceUnit, SubscriptionUnit;

const
  KPosition = 0;
  K22Position = 1;
  K33Position = 2;

type
  TTvkRecord = record
    Cell: TCellLocation;
    CellData: TCellDataArray;
    Used: array[0..2] of Boolean;
    procedure Assign(const Item: TTvkRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TTvkArray = array of TTvkRecord;

  TTvkStorage = class(TCustomBoundaryStorage)
  private
    FTvkArray: TTvkArray;
    function GetTvkArray: TTvkArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property TvkArray: TTvkArray read GetTvkArray;
  end;

  TTvkItem = class(TCustomModflowBoundaryItem)
  private
    FFormulaObjects: array of IFormulaObject;
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
    function NonBlankFormulas: boolean; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property K: string index KPosition read GetBoundaryFormula write SetBoundaryFormula;
    property K22: string index K22Position read GetBoundaryFormula write SetBoundaryFormula;
    property K33: string index K33Position read GetBoundaryFormula write SetBoundaryFormula;
  end;

  TTvkTimeListLink = class(TTimeListsModelLink)
  private
    FKData: TModflowTimeList;
    FK22Data: TModflowTimeList;
    FK33Data: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TTvkCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateKxData(Sender: TObject);
    procedure InvalidateKyData(Sender: TObject);
    procedure InvalidateKzData(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
//    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
//      var StartOfFirstStressPeriod: Double;
//      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TTvkStorage.TvkArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
  end;

  TTvk_Cell = class(TValueCell)
  private
    FValues: TTvkRecord;
    StressPeriod: integer;
    function GetValue(Index: Integer): double;
    function GetAnnotation(Index: Integer): string;
    function GetUsed(PropertyIndex: Integer): Boolean;
  protected
    property Values: TTvkRecord read FValues;
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property Used[PropertyIndex: Integer]: Boolean read GetUsed;
    property K: double  index KPosition read GetValue;
    property K22: double  index K22Position read GetValue;
    property K33: double  index K33Position read GetValue;

    property KAnnotation: string index KPosition read GetAnnotation;
    property K22Annotation: string index K22Position read GetAnnotation;
    property K33Annotation: string index K33Position read GetAnnotation;
    // PEST properties
    property KPest: string index KPosition read GetPestName;
    property K22Pest: string index K22Position read GetPestName;
    property K33Pest: string index K33Position read GetPestName;

    property KPestSeries: string index KPosition read GetPestSeriesName;
    property K22PestSeries: string index K22Position read GetPestSeriesName;
    property K33PestSeries: string index K33Position read GetPestSeriesName;

    property KPestSeriesMethod: TPestParamMethod index KPosition
      read GetPestSeriesMethod;
    property K22PestSeriesMethod: TPestParamMethod index K22Position
      read GetPestSeriesMethod;
    property K33PestSeriesMethod: TPestParamMethod index K33Position
      read GetPestSeriesMethod;

    property KTimeSeriesName: string index KPosition read GetMf6TimeSeriesName
      write SetMf6TimeSeriesName;
    property K22TimeSeriesName: string index K22Position read GetMf6TimeSeriesName
      write SetMf6TimeSeriesName;
    property K33TimeSeriesName: string index K33Position read GetMf6TimeSeriesName
      write SetMf6TimeSeriesName;
  end;

  TTvkBoundary = class(TModflowBoundary)
  private
    FPestKMethod: TPestParamMethod;
    FPestK22Method: TPestParamMethod;
    FPestK33Method: TPestParamMethod;

    FPestKFormula: IFormulaObject;
    FPestK22Formula: IFormulaObject;
    FPestK33Formula: IFormulaObject;

    FPestKObserver: TObserver;
    FPestK22Observer: TObserver;
    FPestK33Observer: TObserver;

    FUsedObserver: TObserver;
    function GetPestKFormula: string;
    function GetPestK22Formula: string;
    function GetPestK33Formula: string;
    procedure SetPestKFormula(const Value: string);
    procedure SetPestKMethod(const Value: TPestParamMethod);
    procedure SetPestK22Formula(const Value: string);
    procedure SetPestK22Method(const Value: TPestParamMethod);
    procedure SetPestK33Formula(const Value: string);
    procedure SetPestK33Method(const Value: TPestParamMethod);
    function GetPestKObserver: TObserver;
    function GetPestK22Observer: TObserver;
    function GetPestK33Observer: TObserver;
    procedure InvalidateKData(Sender: TObject);
    procedure InvalidateK22Data(Sender: TObject);
    procedure InvalidateK33Data(Sender: TObject);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRiv_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    property PestKObserver: TObserver read GetPestKObserver;
    property PestK33Observer: TObserver read GetPestK33Observer;
    property PestK22Observer: TObserver read GetPestK22Observer;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property PestKFormula: string read GetPestKFormula
      write SetPestKFormula;
    property PestK22Formula: string read GetPestK22Formula
      write SetPestK22Formula;
    property PestK33Formula: string read GetPestK33Formula
      write SetPestK33Formula;

    property PestKMethod: TPestParamMethod
      read FPestKMethod write SetPestKMethod;
    property PestK22Method: TPestParamMethod read FPestK22Method
      write SetPestK22Method;
    property PestK33Method: TPestParamMethod read FPestK33Method
      write SetPestK33Method;
  end;


implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit, CellLocationUnit, RbwParser,
  Modflow6DynamicTimeSeriesInterfaceUnit, GIS_Functions,
  frmErrorsAndWarningsUnit, ModflowTimeUnit, DataSetNamesUnit;

{ TTvkRecord }

procedure TTvkRecord.Assign(const Item: TTvkRecord);
begin
  Self := Item;
  CellData := Item.CellData;
end;

procedure TTvkRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
begin
  WriteCompCell(Comp, Cell);
  CellData.Cache(Comp, Strings);
  for Index := 0 to Length(Used) -1 do
  begin
    WriteCompBoolean(Comp, Used[index])
  end;
end;

procedure TTvkRecord.RecordStrings(Strings: TStringList);
begin
  CellData.RecordStrings(Strings);
end;

procedure TTvkRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  CellData.Restore(Decomp, Annotations);
  for Index := 0 to Length(Used) -1 do
  begin
    Used[Index] := ReadCompBoolean(Decomp);
  end;
end;

{ TTvkStorage }

procedure TTvkStorage.Clear;
begin
  SetLength(FTvkArray, 0);
  FCleared := True;
end;

function TTvkStorage.GetTvkArray: TTvkArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FTvkArray;
end;

procedure TTvkStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FTvkArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FTvkArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TTvkStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FTvkArray);
    for Index := 0 to Count - 1 do
    begin
      FTvkArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FTvkArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TTvkItem }

procedure TTvkItem.Assign(Source: TPersistent);
var
  Index: integer;
  OtherItem: TTvkItem;
begin
  if Source is TTvkItem then
  begin
    OtherItem := TTvkItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := OtherItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TTvkItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TTvkCollection;
  KObserver: TObserver;
  K22Observer: TObserver;
  K33bserver: TObserver;
begin
  ParentCollection := Collection as TTvkCollection;
  KObserver := FObserverList[KPosition];
  KObserver.OnUpToDateSet := ParentCollection.InvalidateKxData;
  K22Observer := FObserverList[K22Position];
  K22Observer.OnUpToDateSet := ParentCollection.InvalidateKyData;
  K33bserver := FObserverList[K33Position];
  K33bserver.OnUpToDateSet := ParentCollection.InvalidateKzData;
end;

function TTvkItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

constructor TTvkItem.Create(Collection: TCollection);
var
  Index: Integer;
begin
  inherited;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '';
  end;
end;

procedure TTvkItem.CreateFormulaObjects;
var
  Index: integer;
begin
  SetLength(FFormulaObjects, BoundaryFormulaCount);
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    FFormulaObjects[Index] := CreateFormulaObject(dso3D);
  end;
end;

destructor TTvkItem.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;

  inherited;
end;

function TTvkItem.GetBoundaryFormula(Index: integer): string;
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

procedure TTvkItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    if Sender = FFormulaObjects[Index] as TObject then
    begin
      List.Add(FObserverList[Index]);
      Break;
    end;
  end;
end;

procedure TTvkItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateTransientKx(self);
    PhastModel.InvalidateTransientKy(self);
    PhastModel.InvalidateTransientKz(self);
  end;
end;

function TTvkItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TTvkItem;
  Index: integer;
begin
  result := (AnotherItem is TTvkItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TTvkItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = OtherItem.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TTvkItem.NonBlankFormulas: boolean;
var
  Index: integer;
begin
  result := False;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    result := BoundaryFormula[Index] <> '';
    if result then
    begin
      Exit;
    end;
  end;
end;

procedure TTvkItem.RemoveFormulaObjects;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FFormulaObjects[Index],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

procedure TTvkItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

{ TRivTimeListLink }

procedure TTvkTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FKData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FK22Data := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FK33Data := TModflowTimeList.Create(Model, Boundary.ScreenObject);

  FKData.NonParamDescription := StrTransientKx;
  FKData.ParamDescription := ' ' + LowerCase(StrTransientKx);
  FK22Data.NonParamDescription := StrTransientKy;
  FK22Data.ParamDescription := ' ' + LowerCase(StrTransientKy);
  FK33Data.NonParamDescription := StrTransientKz;
  FK33Data.ParamDescription := LowerCase(StrTransientKz);;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FKData.OnInvalidate := LocalModel.InvalidateTransientKx;
    FK22Data.OnInvalidate := LocalModel.InvalidateTransientKy;
    FK33Data.OnInvalidate := LocalModel.InvalidateTransientKz;
  end;
  AddTimeList(FKData);
  AddTimeList(FK22Data);
  AddTimeList(FK33Data);

end;

destructor TTvkTimeListLink.Destroy;
begin
  FKData.Free;
  FK22Data.Free;
  FK33Data.Free;
  inherited;
end;

{ TTvkCollection }

procedure TTvkCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TTvkStorage.Create(AModel));
end;

function TTvkCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TTvkItem;
begin
  Item := Items[ItemIndex] as TTvkItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TTvkCollection.AssignCellList(
  CellAssignmentData: TCellAssignmentData);
var
  TvkStorage: TTvkStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  AllowedIndicies: Set of Byte;
  ErrorMessage: string;
  LocalScreenObject: TScreenObject;
  Expression: TExpression;
  ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer;
  Variables, DataSets: TList;
  AModel: TBaseModel;
  AScreenObject: TObject;
  PestName: string;
  PestSeriesName: string;
  PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string;
  DynamicTimeSeries: IDynamicTimeSeries;
  procedure HandleError(E: Exception);
  begin
    case BoundaryFunctionIndex of
      KPosition:
        begin
          ErrorMessage :=  'Transient Kx set to zero because of a math error';
        end;
      K22Position:
        begin
          ErrorMessage :=  'Transient Kx set to zero because of a math error';
        end;
      K33Position:
        begin
          ErrorMessage :=  'Transient Kz set to zero because of a math error';
        end;
    end;
    with TvkStorage.TvkArray[Index].CellData.Values[BoundaryFunctionIndex] do
    begin
      Value := 0;
      ValueAnnotation := ErrorMessage;
      ValuePestName := PestName;
      ValuePestSeriesName := PestSeriesName;
      ValuePestSeriesMethod := PestSeriesMethod;
      ValueTimeSeriesName := TimeSeriesName;
    end;
    LocalScreenObject := ScreenObject as TScreenObject;

    frmErrorsAndWarnings.AddError(AModel, ErrorMessage,
      Format(StrObject0sLayerError,
      [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
      ACell.Column+1, E.Message]), LocalScreenObject);
  end;
begin
  Expression := CellAssignmentData.Expression;
  ACellList := CellAssignmentData.ACellList;
  BoundaryStorage := CellAssignmentData.BoundaryStorage;
  BoundaryFunctionIndex := CellAssignmentData.BoundaryFunctionIndex;
  Variables := CellAssignmentData.Variables;
  DataSets := CellAssignmentData.DataSets;
  AModel := CellAssignmentData.AModel;
  AScreenObject := CellAssignmentData.AScreenObject;
  PestName := CellAssignmentData.PestName;
  PestSeriesName := CellAssignmentData.PestSeriesName;
  PestSeriesMethod := CellAssignmentData.PestSeriesMethod;
  TimeSeriesName := CellAssignmentData.TimeSeriesName;
  DynamicTimeSeries := CellAssignmentData.DynamicTimeSeries;

  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);

  AllowedIndicies := [KPosition,K22Position, K33Position];

  Assert(BoundaryFunctionIndex in AllowedIndicies);
//  Assert(Expression <> nil);

  TvkStorage := BoundaryStorage as TTvkStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    AssignDynamicTimeSeries(TimeSeriesName, DynamicTimeSeries, ACell);

    TvkStorage.TvkArray[Index].Used[BoundaryFunctionIndex] := Expression <> nil;
    if Expression <> nil then
    begin
      try
        Expression.Evaluate;
        TvkStorage.TvkArray[Index].CellData.PropertyCount := 3;
        with TvkStorage.TvkArray[Index].CellData.Values[BoundaryFunctionIndex] do
        begin
          Value := Expression.DoubleResult;
          ValueAnnotation := ACell.Annotation;
          ValuePestName := PestName;
          ValuePestSeriesName := PestSeriesName;
          ValuePestSeriesMethod := PestSeriesMethod;
          ValueTimeSeriesName := TimeSeriesName;
        end;
      except
        on E: EMathError do
        begin
          HandleError(E);
        end;
        on E: ERbwParserError do
        begin
          HandleError(E);
        end;
      end;
    end;
  end;
end;

procedure TTvkCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  TvkStorage: TTvkStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  TvkStorage := BoundaryStorage as TTvkStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with TvkStorage.TvkArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

class function TTvkCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TTvkTimeListLink;
end;

procedure TTvkCollection.InvalidateKxData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TTvkTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TTvkTimeListLink;
    Link.FKData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TTvkTimeListLink;
        Link.FKData.Invalidate;
      end;
    end;
  end;
end;

procedure TTvkCollection.InvalidateKyData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TTvkTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TTvkTimeListLink;
    Link.FK22Data.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TTvkTimeListLink;
        Link.FK22Data.Invalidate;
      end;
    end;
  end;
end;

procedure TTvkCollection.InvalidateKzData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TTvkTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TTvkTimeListLink;
    Link.FK33Data.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TTvkTimeListLink;
        Link.FK33Data.Invalidate;
      end;
    end;
  end;
end;

procedure TTvkCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateTransientKx(self);
    PhastModel.InvalidateTransientKy(self);
    PhastModel.InvalidateTransientKz(self);
  end;
end;

class function TTvkCollection.ItemClass: TBoundaryItemClass;
begin
  result := TTvkItem;
end;

procedure TTvkCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TTvkStorage).FTvkArray, BoundaryCount);
  inherited;
end;

{ TTvk_Cell }

procedure TTvk_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TTvk_Cell.GetAnnotation(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValueAnnotation;
end;

function TTvk_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TTvk_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TTvk_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TTvk_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TTvk_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValueTimeSeriesName;
end;

function TTvk_Cell.GetPestName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValuePestName;
end;

function TTvk_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  result := Values.CellData.Values[Index].ValuePestSeriesMethod;
end;

function TTvk_Cell.GetPestSeriesName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValuePestSeriesName;
end;

function TTvk_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := GetAnnotation(Index);
end;

function TTvk_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := GetValue(Index);
end;

function TTvk_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TTvk_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TTvk_Cell.GetUsed(PropertyIndex: Integer): Boolean;
begin
  result := FValues.Used[PropertyIndex];
end;

function TTvk_Cell.GetValue(Index: Integer): double;
begin
  result := Values.CellData.Values[Index].Value;
end;

function TTvk_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Tvk_Cell: TTvk_Cell;
begin
  result := AnotherCell is TTvk_Cell;
  if result then
  begin
    Tvk_Cell := TTvk_Cell(AnotherCell);
    result :=
      (K = Tvk_Cell.K)
      and (K22 = Tvk_Cell.K22)
      and (K33 = Tvk_Cell.K33)
      and (Values.Cell = Tvk_Cell.Values.Cell);
  end;
end;

procedure TTvk_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TTvk_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TTvk_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TTvk_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TTvk_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
begin
  Values.CellData.Values[Index].ValueTimeSeriesName := Value;
end;

procedure TTvk_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TTvkBoundary }

procedure TTvkBoundary.Assign(Source: TPersistent);
var
  SourceTvk: TTvkBoundary;
begin
  if Source is TTvkBoundary then
  begin
    SourceTvk := TTvkBoundary(Source);

    PestKFormula := SourceTvk.PestKFormula;
    PestK22Formula := SourceTvk.PestK22Formula;
    PestK33Formula := SourceTvk.PestK33Formula;
    PestKMethod := SourceTvk.PestKMethod;
    PestK22Method := SourceTvk.PestK22Method;
    PestK33Method := SourceTvk.PestK33Method;
  end;
  inherited;
end;

procedure TTvkBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TTvk_Cell;
  BoundaryValues: TTvkRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TTvkStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TTvkStorage;
  Assert(ScreenObject <> nil);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TTvk_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Length(LocalBoundaryStorage.TvkArray) then
      begin
        Cells.Capacity := Length(LocalBoundaryStorage.TvkArray);
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.TvkArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.TvkArray[BoundaryIndex];
        Cell := TTvk_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TTvkBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TTvkCollection
end;

function TTvkBoundary.BoundaryObserverPrefix: string;
begin
  result := 'Pest_Transient_K';
end;

constructor TTvkBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestKFormula := '';
  PestK22Formula := '';
  PestK33Formula := '';
  FPestKMethod := DefaultBoundaryMethod(KPosition);
  FPestK22Method := DefaultBoundaryMethod(K22Position);
  FPestK33Method := DefaultBoundaryMethod(K33Position);

end;

procedure TTvkBoundary.CreateFormulaObjects;
begin
  FPestKFormula := CreateFormulaObjectBlocks(dso3D);
  FPestK22Formula := CreateFormulaObjectBlocks(dso3D);
  FPestK33Formula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TTvkBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestKObserver);
    FObserverList.Add(PestK22Observer);
    FObserverList.Add(PestK33Observer);
  end;
end;

class function TTvkBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

destructor TTvkBoundary.Destroy;
begin
  PestKFormula := '';
  PestK22Formula := '';
  PestK33Formula := '';

  inherited;
end;

procedure TTvkBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TTvkStorage;
  ValueCount: Integer;
begin
  Values.EmptyFormulaOK := True;
  EvaluateListBoundaries(AModel);
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TTvkStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
  end;
end;

function TTvkBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    KPosition:
      begin
        result := PestKFormula;
      end;
    K22Position:
      begin
        result := PestK22Formula;
      end;
    K33Position:
      begin
        result := PestK33Formula;
      end;
    else
      Assert(False);
  end;
end;

function TTvkBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
  case FormulaIndex of
    KPosition:
      begin
        result := PestKMethod;
      end;
    K22Position:
      begin
        result := PestK22Method;
      end;
    K33Position:
      begin
        result := PestK33Method;
      end;
    else
      Assert(False);
  end;
end;

function TTvkBoundary.GetPestK22Formula: string;
begin
  Result := FPestK22Formula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(K22Position);
  end;
end;

function TTvkBoundary.GetPestK22Observer: TObserver;
begin
  if FPestK22Observer = nil then
  begin
    CreateObserver('PestKy_', FPestK22Observer, nil);
    FPestK22Observer.OnUpToDateSet := InvalidateK22Data;
  end;
  result := FPestK22Observer;
end;

function TTvkBoundary.GetPestK33Formula: string;
begin
  Result := FPestK33Formula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(K33Position);
  end;
end;

function TTvkBoundary.GetPestK33Observer: TObserver;
begin
  if FPestK33Observer = nil then
  begin
    CreateObserver('PestKz_', FPestK33Observer, nil);
    FPestK33Observer.OnUpToDateSet := InvalidateK33Data;
  end;
  result := FPestK33Observer;
end;

function TTvkBoundary.GetPestKFormula: string;
begin
  Result := FPestKFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(KPosition);
  end;
end;

function TTvkBoundary.GetPestKObserver: TObserver;
begin
  if FPestKObserver = nil then
  begin
    CreateObserver('PestKx_', FPestKObserver, nil);
    FPestKObserver.OnUpToDateSet := InvalidateKData;
  end;
  result := FPestKObserver;
end;

procedure TTvkBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestKFormula as TObject then
  begin
    if KPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[KPosition]);
    end;
  end;
  if Sender = FPestK22Formula as TObject then
  begin
    if K22Position < FObserverList.Count then
    begin
      List.Add(FObserverList[K22Position]);
    end;
  end;
  if Sender = FPestK33Formula as TObject then
  begin
    if K33Position < FObserverList.Count then
    begin
      List.Add(FObserverList[K33Position]);
    end;
  end;
end;

function TTvkBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('TransientK_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TTvkBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TTvkBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateTransientKx(self);
    Model.InvalidateTransientKy(self);
    Model.InvalidateTransientKz(self);
  end;
end;

procedure TTvkBoundary.InvalidateK22Data(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateTransientKy(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateTransientKy(self);
    end;
  end;
end;

procedure TTvkBoundary.InvalidateK33Data(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateTransientKz(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateTransientKz(self);
    end;
  end;
end;

procedure TTvkBoundary.InvalidateKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateTransientKx(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateTransientKx(self);
    end;
  end;
end;

procedure TTvkBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    KPosition:
      begin
        PestKFormula := Value;
      end;
    K22Position:
      begin
        PestK22Formula := Value;
      end;
    K33Position:
      begin
        PestK33Formula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TTvkBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    KPosition:
      begin
        PestKMethod := Value;
      end;
    K22Position:
      begin
        PestK22Method := Value;
      end;
    K33Position:
      begin
        PestK33Method := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TTvkBoundary.SetPestK22Formula(const Value: string);
begin
  UpdateFormulaBlocks(Value, K22Position, FPestK22Formula);
end;

procedure TTvkBoundary.SetPestK22Method(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestK22Method, Value);
end;

procedure TTvkBoundary.SetPestK33Formula(const Value: string);
begin
  UpdateFormulaBlocks(Value, K33Position, FPestK33Formula);
end;

procedure TTvkBoundary.SetPestK33Method(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestK33Method, Value);
end;

procedure TTvkBoundary.SetPestKFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, KPosition, FPestKFormula);
end;

procedure TTvkBoundary.SetPestKMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestKMethod, Value);
end;

end.
