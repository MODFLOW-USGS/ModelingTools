unit ModflowTvsUnit;

interface

uses
  Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, GoPhastTypes, ModflowCellUnit,
  FormulaManagerInterfaceUnit, SubscriptionUnit;

const
  SSPosition = 0;
  SYPosition = 1;

type
  TTvsRecord = record
    Cell: TCellLocation;
    CellData: TCellDataArray;
    procedure Assign(const Item: TTvsRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TTvsArray = array of TTvsRecord;

  TTvsStorage = class(TCustomBoundaryStorage)
  private
    FTvsArray: TTvsArray;
    function GetTvsArray: TTvsArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property TvsArray: TTvsArray read GetTvsArray;
  end;

  TTvsItem = class(TCustomModflowBoundaryItem)
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
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property SS: string index SSPosition read GetBoundaryFormula write SetBoundaryFormula;
    property SY: string index SYPosition read GetBoundaryFormula write SetBoundaryFormula;
  end;

  TTvsTimeListLink = class(TTimeListsModelLink)
  private
    FSSData: TModflowTimeList;
    FSYData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TTvsCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateSSData(Sender: TObject);
    procedure InvalidateSYData(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TTvsStorage.TvsArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
  end;

  TTvs_Cell = class(TValueCell)
  private
    FValues: TTvsRecord;
    StressPeriod: integer;
    function GetValue(Index: Integer): double;
    function GetAnnotation(Index: Integer): string;
  protected
    property Values: TTvsRecord read FValues;
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
    property SS: double  index SSPosition read GetValue;
    property SY: double  index SYPosition read GetValue;

    property SSAnnotation: string index SSPosition read GetAnnotation;
    property SYAnnotation: string index SYPosition read GetAnnotation;
    // PEST properties
    property SSPest: string index SSPosition read GetPestName;
    property SYPest: string index SYPosition read GetPestName;

    property SSPestSeries: string index SSPosition read GetPestSeriesName;
    property SYPestSeries: string index SYPosition read GetPestSeriesName;

    property SSPestSeriesMethod: TPestParamMethod index SSPosition
      read GetPestSeriesMethod;
    property SY2PestSeriesMethod: TPestParamMethod index SYPosition
      read GetPestSeriesMethod;

    property SSTimeSeriesName: string index SSPosition read GetMf6TimeSeriesName
      write SetMf6TimeSeriesName;
    property SYTimeSeriesName: string index SYPosition read GetMf6TimeSeriesName
      write SetMf6TimeSeriesName;
  end;

  TTvsBoundary = class(TModflowBoundary)
  private
    FPestSSMethod: TPestParamMethod;
    FPestSYMethod: TPestParamMethod;

    FPestSSFormula: IFormulaObject;
    FPestSYFormula: IFormulaObject;

    FPestSSObserver: TObserver;
    FPestSYObserver: TObserver;

    FUsedObserver: TObserver;
    function GetPestSSFormula: string;
    function GetPestSYFormula: string;
    procedure SetPestSSFormula(const Value: string);
    procedure SetPestSSMethod(const Value: TPestParamMethod);
    procedure SetPestSYFormula(const Value: string);
    procedure SetPestSYMethod(const Value: TPestParamMethod);
    function GetPestSSObserver: TObserver;
    function GetPestSYObserver: TObserver;
    procedure InvalidateSSData(Sender: TObject);
    procedure InvalidateSYData(Sender: TObject);
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
    property PestSSObserver: TObserver read GetPestSSObserver;
    property PestSYObserver: TObserver read GetPestSYObserver;
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
    property PestSSFormula: string read GetPestSSFormula
      write SetPestSSFormula;
    property PestSYFormula: string read GetPestSYFormula
      write SetPestSYFormula;

    property PestSSMethod: TPestParamMethod
      read FPestSSMethod write SetPestSSMethod;
    property PestSYMethod: TPestParamMethod read FPestSYMethod
      write SetPestSYMethod;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit, CellLocationUnit, RbwParser,
  Modflow6DynamicTimeSeriesInterfaceUnit, GIS_Functions,
  frmErrorsAndWarningsUnit, ModflowTimeUnit, DataSetNamesUnit;

{ TTvsRecord }

procedure TTvsRecord.Assign(const Item: TTvsRecord);
begin
  Self := Item;
  CellData := Item.CellData;
end;

procedure TTvsRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  CellData.Cache(Comp, Strings);
end;

procedure TTvsRecord.RecordStrings(Strings: TStringList);
begin
  CellData.RecordStrings(Strings);
end;

procedure TTvsRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  CellData.Restore(Decomp, Annotations);
end;

{ TTvsStorage }

procedure TTvsStorage.Clear;
begin
  SetLength(FTvsArray, 0);
  FCleared := True;
end;

function TTvsStorage.GetTvsArray: TTvsArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FTvsArray;
end;

procedure TTvsStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FTvsArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FTvsArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TTvsStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FTvsArray);
    for Index := 0 to Count - 1 do
    begin
      FTvsArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FTvsArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TTvsItem }

procedure TTvsItem.Assign(Source: TPersistent);
var
  Index: integer;
  OtherItem: TTvsItem;
begin
  if Source is TTvsItem then
  begin
    OtherItem := TTvsItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := OtherItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TTvsItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TTvsCollection;
  SSObserver: TObserver;
  SYObserver: TObserver;
begin
  ParentCollection := Collection as TTvsCollection;
  SSObserver := FObserverList[SSPosition];
  SSObserver.OnUpToDateSet := ParentCollection.InvalidateSSData;
  SYObserver := FObserverList[SYPosition];
  SYObserver.OnUpToDateSet := ParentCollection.InvalidateSYData;
end;

function TTvsItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

constructor TTvsItem.Create(Collection: TCollection);
begin
  inherited;

end;

procedure TTvsItem.CreateFormulaObjects;
var
  Index: integer;
begin
  SetLength(FFormulaObjects, BoundaryFormulaCount);
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    FFormulaObjects[Index] := CreateFormulaObject(dso3D);
  end;
end;

destructor TTvsItem.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;

  inherited;
end;

function TTvsItem.GetBoundaryFormula(Index: integer): string;
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

procedure TTvsItem.GetPropertyObserver(Sender: TObject; List: TList);
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

procedure TTvsItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateTransientSS(self);
    PhastModel.InvalidateTransientSY(self);
  end;
end;

function TTvsItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TTvsItem;
  Index: integer;
begin
  result := (AnotherItem is TTvsItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TTvsItem(AnotherItem);
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

procedure TTvsItem.RemoveFormulaObjects;
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

procedure TTvsItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

{ TRivTimeListLink }

procedure TTvsTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FSSData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSYData := TModflowTimeList.Create(Model, Boundary.ScreenObject);

  FSSData.NonParamDescription := StrTransientSS;
  FSSData.ParamDescription := ' ' + LowerCase(StrTransientSS);
  FSYData.NonParamDescription := StrTransientSY;
  FSYData.ParamDescription := ' ' + LowerCase(StrTransientSY);
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FSSData.OnInvalidate := LocalModel.InvalidateTransientSS;
    FSYData.OnInvalidate := LocalModel.InvalidateTransientSY;
  end;
  AddTimeList(FSSData);
  AddTimeList(FSYData);
end;

destructor TTvsTimeListLink.Destroy;
begin
  FSSData.Free;
  FSYData.Free;
  inherited;
end;

{ TTvsCollection }

procedure TTvsCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TTvsStorage.Create(AModel));
end;

function TTvsCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TTvsItem;
begin
  Item := Items[ItemIndex] as TTvsItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TTvsCollection.AssignCellList(
  CellAssignmentData: TCellAssignmentData);
var
  TvkStorage: TTvsStorage;
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
      SSPosition:
        begin
          ErrorMessage :=  'Transient SS set to zero because of a math error';
        end;
      SYPosition:
        begin
          ErrorMessage :=  'Transient SY set to zero because of a math error';
        end;
    end;
    with TvkStorage.TvsArray[Index].CellData.Values[BoundaryFunctionIndex] do
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

  AllowedIndicies := [SSPosition,SYPosition];

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  TvkStorage := BoundaryStorage as TTvsStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    AssignDynamicTimeSeries(TimeSeriesName, DynamicTimeSeries, ACell);

    try
      TvkStorage.TvsArray[Index].CellData.PropertyCount := 2;
      Expression.Evaluate;
      with TvkStorage.TvsArray[Index].CellData.Values[BoundaryFunctionIndex] do
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

procedure TTvsCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  TvkStorage: TTvsStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  TvkStorage := BoundaryStorage as TTvsStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with TvkStorage.TvsArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

class function TTvsCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TTvsTimeListLink;
end;

procedure TTvsCollection.InvalidateSSData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TTvsTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TTvsTimeListLink;
    Link.FSSData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TTvsTimeListLink;
        Link.FSSData.Invalidate;
      end;
    end;
  end;
end;

procedure TTvsCollection.InvalidateSYData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TTvsTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TTvsTimeListLink;
    Link.FSYData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TTvsTimeListLink;
        Link.FSYData.Invalidate;
      end;
    end;
  end;
end;

procedure TTvsCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateTransientSS(self);
    PhastModel.InvalidateTransientSY(self);
  end;
end;

class function TTvsCollection.ItemClass: TBoundaryItemClass;
begin
  result := TTvsItem;
end;

procedure TTvsCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TTvsStorage).FTvsArray, BoundaryCount);
  inherited;
end;

{ TTvs_Cell }

procedure TTvs_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TTvs_Cell.GetAnnotation(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValueAnnotation;
end;

function TTvs_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TTvs_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TTvs_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TTvs_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TTvs_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValueTimeSeriesName;
end;

function TTvs_Cell.GetPestName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValuePestName;
end;

function TTvs_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  result := Values.CellData.Values[Index].ValuePestSeriesMethod;
end;

function TTvs_Cell.GetPestSeriesName(Index: Integer): string;
begin
  result := Values.CellData.Values[Index].ValuePestSeriesName;
end;

function TTvs_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := GetAnnotation(Index);
end;

function TTvs_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := GetValue(Index);
end;

function TTvs_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TTvs_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TTvs_Cell.GetValue(Index: Integer): double;
begin
  result := Values.CellData.Values[Index].Value;
end;

function TTvs_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Tvs_Cell: TTvs_Cell;
begin
  result := AnotherCell is TTvs_Cell;
  if result then
  begin
    Tvs_Cell := TTvs_Cell(AnotherCell);
    result :=
      (SS = Tvs_Cell.SS)
      and (SY = Tvs_Cell.SY)
      and (Values.Cell = Tvs_Cell.Values.Cell);
  end;
end;

procedure TTvs_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TTvs_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TTvs_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TTvs_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TTvs_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
begin
  Values.CellData.Values[Index].ValueTimeSeriesName := Value;
end;

procedure TTvs_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TTvsBoundary }

procedure TTvsBoundary.Assign(Source: TPersistent);
var
  SourceTvk: TTvsBoundary;
begin
  if Source is TTvsBoundary then
  begin
    SourceTvk := TTvsBoundary(Source);

    PestSSFormula := SourceTvk.PestSSFormula;
    PestSYFormula := SourceTvk.PestSYFormula;
    PestSSMethod := SourceTvk.PestSSMethod;
    PestSYMethod := SourceTvk.PestSYMethod;
  end;
  inherited;
end;

procedure TTvsBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TTvs_Cell;
  BoundaryValues: TTvsRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TTvsStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TTvsStorage;
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
      Cells := TValueCellList.Create(TTvs_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Length(LocalBoundaryStorage.TvsArray) then
      begin
        Cells.Capacity := Length(LocalBoundaryStorage.TvsArray);
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.TvsArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.TvsArray[BoundaryIndex];
        Cell := TTvs_Cell.Create;
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

class function TTvsBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TTvsCollection
end;

function TTvsBoundary.BoundaryObserverPrefix: string;
begin
  result := 'Pest_Transient_S';
end;

constructor TTvsBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestSSFormula := '';
  PestSYFormula := '';
  FPestSSMethod := DefaultBoundaryMethod(SSPosition);
  FPestSYMethod := DefaultBoundaryMethod(SYPosition);
end;

procedure TTvsBoundary.CreateFormulaObjects;
begin
  FPestSSFormula := CreateFormulaObjectBlocks(dso3D);
  FPestSYFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TTvsBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestSSObserver);
    FObserverList.Add(PestSYObserver);
  end;
end;

class function TTvsBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

destructor TTvsBoundary.Destroy;
begin
  PestSSFormula := '';
  PestSYFormula := '';

  inherited;
end;

procedure TTvsBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TTvsStorage;
  ValueCount: Integer;
begin
  EvaluateListBoundaries(AModel);
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TTvsStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
  end;
end;

function TTvsBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    SSPosition:
      begin
        result := PestSSFormula;
      end;
    SYPosition:
      begin
        result := PestSYFormula;
      end;
    else
      Assert(False);
  end;
end;

function TTvsBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
  case FormulaIndex of
    SSPosition:
      begin
        result := PestSSMethod;
      end;
    SYPosition:
      begin
        result := PestSYMethod;
      end;
    else
      Assert(False);
  end;
end;

function TTvsBoundary.GetPestSYFormula: string;
begin
  Result := FPestSYFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SYPosition);
  end;
end;

function TTvsBoundary.GetPestSYObserver: TObserver;
begin
  if FPestSYObserver = nil then
  begin
    CreateObserver('PestSy_', FPestSYObserver, nil);
    FPestSYObserver.OnUpToDateSet := InvalidateSYData;
  end;
  result := FPestSYObserver;
end;

function TTvsBoundary.GetPestSSFormula: string;
begin
  Result := FPestSSFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SSPosition);
  end;
end;

function TTvsBoundary.GetPestSSObserver: TObserver;
begin
  if FPestSSObserver = nil then
  begin
    CreateObserver('PestSs_', FPestSSObserver, nil);
    FPestSSObserver.OnUpToDateSet := InvalidateSSData;
  end;
  result := FPestSSObserver;
end;

procedure TTvsBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestSSFormula as TObject then
  begin
    if SSPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[SSPosition]);
    end;
  end;
  if Sender = FPestSYFormula as TObject then
  begin
    if SYPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[SYPosition]);
    end;
  end;
end;

function TTvsBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('TransientS_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TTvsBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TTvsBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateTransientSS(self);
    Model.InvalidateTransientSY(self);
  end;
end;

procedure TTvsBoundary.InvalidateSYData(Sender: TObject);
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
  PhastModel.InvalidateTransientSY(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateTransientSY(self);
    end;
  end;
end;

procedure TTvsBoundary.InvalidateSSData(Sender: TObject);
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
  PhastModel.InvalidateTransientSS(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateTransientSS(self);
    end;
  end;
end;

procedure TTvsBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    SSPosition:
      begin
        PestSSFormula := Value;
      end;
    SYPosition:
      begin
        PestSYFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TTvsBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    SSPosition:
      begin
        PestSSMethod := Value;
      end;
    SYPosition:
      begin
        PestSYMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TTvsBoundary.SetPestSYFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SYPosition, FPestSYFormula);
end;

procedure TTvsBoundary.SetPestSYMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestSYMethod, Value);
end;

procedure TTvsBoundary.SetPestSSFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SSPosition, FPestSSFormula);
end;

procedure TTvsBoundary.SetPestSSMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestSSMethod, Value);
end;

end.
