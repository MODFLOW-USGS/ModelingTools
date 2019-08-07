unit ModflowFarmWell;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes;

type
  TFarmWellRecord = record
    Cell: TCellLocation;
    StartingTime: double;
    EndingTime: double;
    MaxPumpingRate: double;
    MaxPumpingRateAnnotation: string;
    ResetMaxPumpingRate: boolean;
    ResetMaxPumpingRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TFarmWellArray = array of TFarmWellRecord;

  TFarmWellStorage = class(TCustomBoundaryStorage)
  private
    FFarmWellArray: TFarmWellArray;
    function GetFarmWellArray: TFarmWellArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property FarmWellArray: TFarmWellArray read GetFarmWellArray;
  end;

  TFarmWellItem = class(TCustomModflowBoundaryItem)
  private
    const
      MaxPumpingRatePosition = 0;
      ResetMaxPumpingRatePosition = 1;
    var
    // See @link(MaxPumpingRate).
    FMaxPumpingRate: TFormulaObject;
    FResetMaxPumpingRate: TFormulaObject;
    // See @link(MaxPumpingRate).
    function GetMaxPumpingRate: string;
    procedure SetMaxPumpingRate(const Value: string);
    function GetResetMaxPumpingRate: string;
    procedure SetResetMaxPumpingRate(const Value: string);
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
    Destructor Destroy; override;
  published
    // @name is the formula used to set the maximum pumping rate
    // or the maximum pumping rate multiplier of this boundary.
    property MaxPumpingRate: string read GetMaxPumpingRate
      write SetMaxPumpingRate;
    property ResetMaxPumpingRate: string read GetResetMaxPumpingRate
      write SetResetMaxPumpingRate;
  end;

  TFarmWellTimeListLink = class(TTimeListsModelLink)
  private
    FMaxPumpingRateData: TModflowTimeList;
    FResetMaxPumpingRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TFarmWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateMaxPumpingRate(Sender: TObject);
    procedure InvalidateResetMaxPumpingRate(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    class function ItemClass: TBoundaryItemClass; override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel); override;
  end;

  TFarmWellParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TFarmWell_Cell = class(TValueCell)
  private
    Values: TFarmWellRecord;
    StressPeriod: integer;
    function GetMaxPumpingRate: double;
    function GetMaxPumpingRateAnnotation: string;
    function GetResetMaxPumpingRate: boolean;
    function GetResetMaxPumpingRateAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetBooleanValue(Index: integer;
      AModel: TBaseModel): boolean; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetBooleanAnnotation(Index: integer;
      AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property MaxPumpingRate: double read GetMaxPumpingRate;
    property MaxPumpingRateAnnotation: string read GetMaxPumpingRateAnnotation;
    property ResetMaxPumpingRate: boolean read GetResetMaxPumpingRate;
    property ResetMaxPumpingRateAnnotation: string read GetResetMaxPumpingRateAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TFarmWellBoundary = class(TSpecificModflowBoundary)
  private
    FFarmId: TFormulaObject;
    FFarmIdObserver: TObserver;
    function GetFarmId: string;
    procedure SetFarmId(const Value: string);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    // ultimately make this virtual;
    procedure GetCellListValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel);
    procedure InvalidateDisplay; override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
  published
    property FarmId: string read GetFarmId write SetFarmId;
  end;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions;

resourcestring
  StrResetMaxPumpingRa = 'Reset Max pumping rate';

{ TFarmWellRecord }

procedure TFarmWellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, MaxPumpingRate);
  WriteCompBoolean(Comp, ResetMaxPumpingRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(MaxPumpingRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ResetMaxPumpingRateAnnotation));
end;

procedure TFarmWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(MaxPumpingRateAnnotation);
  Strings.Add(ResetMaxPumpingRateAnnotation);
end;

procedure TFarmWellRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  MaxPumpingRate := ReadCompReal(Decomp);
  ResetMaxPumpingRate := ReadCompBoolean(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  MaxPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
  ResetMaxPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFarmWellStorage }

procedure TFarmWellStorage.Clear;
begin
  SetLength(FFarmWellArray, 0);
  FCleared := True;
end;

function TFarmWellStorage.GetFarmWellArray: TFarmWellArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFarmWellArray;
end;

procedure TFarmWellStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFarmWellArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFarmWellArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TFarmWellStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFarmWellArray);
    for Index := 0 to Count - 1 do
    begin
      FFarmWellArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFarmWellArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TFarmWellItem }

procedure TFarmWellItem.Assign(Source: TPersistent);
var
  FarmWell: TFarmWellItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TFarmWellItem then
  begin
    FarmWell := TFarmWellItem(Source);
    MaxPumpingRate := FarmWell.MaxPumpingRate;
    ResetMaxPumpingRate := FarmWell.ResetMaxPumpingRate;
  end;
  inherited;

end;

procedure TFarmWellItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFarmWellCollection;
  MaxPumpingRateObserver: TObserver;
  ResetMaxPumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TFarmWellCollection;

  MaxPumpingRateObserver := FObserverList[MaxPumpingRatePosition];
  MaxPumpingRateObserver.OnUpToDateSet :=
    ParentCollection.InvalidateMaxPumpingRate;

  ResetMaxPumpingRateObserver := FObserverList[ResetMaxPumpingRatePosition];
  ResetMaxPumpingRateObserver.OnUpToDateSet :=
    ParentCollection.InvalidateResetMaxPumpingRate;
end;

function TFarmWellItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TFarmWellItem.CreateFormulaObjects;
begin
  FMaxPumpingRate := CreateFormulaObject(dso3D);
  FResetMaxPumpingRate := CreateFormulaObject(dso3D);
end;

destructor TFarmWellItem.Destroy;
begin
  MaxPumpingRate := '0';
  ResetMaxPumpingRate := 'False';
  inherited;
end;

function TFarmWellItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    MaxPumpingRatePosition: result := MaxPumpingRate;
    ResetMaxPumpingRatePosition: result := ResetMaxPumpingRate;
    else Assert(False);
  end;
end;

function TFarmWellItem.GetMaxPumpingRate: string;
begin
  Result := FMaxPumpingRate.Formula;
  ResetItemObserver(MaxPumpingRatePosition);
end;

procedure TFarmWellItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FMaxPumpingRate then
  begin
    List.Add(FObserverList[MaxPumpingRatePosition]);
  end;
  if Sender = FResetMaxPumpingRate then
  begin
    List.Add(FObserverList[ResetMaxPumpingRatePosition]);
  end;
end;

function TFarmWellItem.GetResetMaxPumpingRate: string;
begin
  Result := FResetMaxPumpingRate.Formula;
//  if not FResetMaxPumpingRate.UpToDate then
//  begin
//    FResetMaxPumpingRate.UpToDate := True;
//  end;
end;

procedure TFarmWellItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    { TODO -cFMP : This needs to be finished. }
//    PhastModel.InvalidateMfDrnConductance(self);
//    PhastModel.InvalidateMfDrnElevation(self);
  end;
end;

function TFarmWellItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFarmWellItem;
begin
  result := (AnotherItem is TFarmWellItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFarmWellItem(AnotherItem);
    result := (Item.MaxPumpingRate = MaxPumpingRate)
      and (Item.ResetMaxPumpingRate = ResetMaxPumpingRate);
  end;
end;

procedure TFarmWellItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaxPumpingRate,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FResetMaxPumpingRate,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TFarmWellItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    MaxPumpingRatePosition:
      MaxPumpingRate := Value;
    ResetMaxPumpingRatePosition:
      ResetMaxPumpingRate := Value;
    else Assert(False);
  end;
end;

procedure TFarmWellItem.SetMaxPumpingRate(const Value: string);
begin
  UpdateFormula(Value, MaxPumpingRatePosition, FMaxPumpingRate);
end;

procedure TFarmWellItem.SetResetMaxPumpingRate(const Value: string);
begin
  UpdateFormula(Value, ResetMaxPumpingRatePosition, FResetMaxPumpingRate);
end;

{ TFarmWellCollection }

procedure TFarmWellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFarmWellStorage.Create(AModel));
end;

function TFarmWellCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TFarmWellBoundary;
  Item: TFarmWellItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  Item := Items[ItemIndex] as TFarmWellItem;
  if FormulaIndex = TFarmWellItem.MaxPumpingRatePosition then
  begin
    Boundary := BoundaryGroup as TFarmWellBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.MaxPumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.MaxPumpingRate
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.MaxPumpingRate
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.MaxPumpingRate;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.MaxPumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.MaxPumpingRate
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.MaxPumpingRate
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    result := Item.BoundaryFormula[FormulaIndex];
  end;
end;

procedure TFarmWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel);
var
  FarmWellStorage: TFarmWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [TFarmWellItem.MaxPumpingRatePosition,
    TFarmWellItem.ResetMaxPumpingRatePosition]);
  Assert(Expression <> nil);

  FarmWellStorage := BoundaryStorage as TFarmWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with FarmWellStorage.FarmWellArray[Index] do
    begin
      case BoundaryFunctionIndex of
        TFarmWellItem.MaxPumpingRatePosition:
          begin
            MaxPumpingRate := Expression.DoubleResult;
            MaxPumpingRateAnnotation := ACell.Annotation;
          end;
        TFarmWellItem.ResetMaxPumpingRatePosition:
          begin
            ResetMaxPumpingRate := Expression.BooleanResult;
            ResetMaxPumpingRateAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TFarmWellCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  FarmWellStorage: TFarmWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  FarmWellStorage := BoundaryStorage as TFarmWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with FarmWellStorage.FarmWellArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TFarmWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFarmWellTimeListLink;
end;

procedure TFarmWellCollection.InvalidateMaxPumpingRate(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFarmWellTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TFarmWellTimeListLink;
    Link.FMaxPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TFarmWellTimeListLink;
      Link.FMaxPumpingRateData.Invalidate;
    end;
  end;
end;

procedure TFarmWellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    { TODO -cFMP : This needs to be finished. }
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

procedure TFarmWellCollection.InvalidateResetMaxPumpingRate(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFarmWellTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TFarmWellTimeListLink;
    Link.FResetMaxPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TFarmWellTimeListLink;
      Link.FResetMaxPumpingRateData.Invalidate;
    end;
  end;
end;

class function TFarmWellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFarmWellItem;
end;

procedure TFarmWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFarmWellStorage).FFarmWellArray,
    BoundaryCount);
  inherited;

end;

{ TFarmWellTimeListLink }

procedure TFarmWellTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FMaxPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FResetMaxPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);

  FMaxPumpingRateData.NonParamDescription := StrMaxPumpingRate;
  FMaxPumpingRateData.ParamDescription := StrMaxPumpingRateMultipl;

  FResetMaxPumpingRateData.NonParamDescription := StrResetMaxPumpingRa;
  FResetMaxPumpingRateData.ParamDescription := ' ' + LowerCase(StrResetMaxPumpingRa);
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    { TODO -cFMP : This needs to be finished. }
//    FMaxPumpingRateData.OnInvalidate := LocalModel.InvalidateMfDrnConductance;
//    FResetMaxPumpingRateData.OnInvalidate := LocalModel.InvalidateMfDrnElevation;
  end;
  AddTimeList(FMaxPumpingRateData);
  AddTimeList(FResetMaxPumpingRateData);
end;

destructor TFarmWellTimeListLink.Destroy;
begin
  FMaxPumpingRateData.Free;
  FResetMaxPumpingRateData.Free;
  inherited;
end;

{ TFarmWellParamItem }

class function TFarmWellParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TFarmWellCollection;
end;

{ TWell_Cell }

procedure TFarmWell_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TFarmWell_Cell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    TFarmWellItem.ResetMaxPumpingRatePosition: result := ResetMaxPumpingRateAnnotation;
    else Assert(False);
  end;
end;

function TFarmWell_Cell.GetBooleanValue(Index: integer;
  AModel: TBaseModel): boolean;
begin
  result := False;
  case Index of
    TFarmWellItem.ResetMaxPumpingRatePosition: result := ResetMaxPumpingRate;
    else Assert(False);
  end;
end;

function TFarmWell_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFarmWell_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TFarmWell_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TFarmWell_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFarmWell_Cell.GetMaxPumpingRate: double;
begin
  result := Values.MaxPumpingRate;
end;

function TFarmWell_Cell.GetMaxPumpingRateAnnotation: string;
begin
  result := Values.MaxPumpingRateAnnotation;
end;

function TFarmWell_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    TFarmWellItem.MaxPumpingRatePosition: result := MaxPumpingRateAnnotation;
    else Assert(False);
  end;
end;

function TFarmWell_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    TFarmWellItem.MaxPumpingRatePosition: result := MaxPumpingRate;
    else Assert(False);
  end;
end;

function TFarmWell_Cell.GetResetMaxPumpingRate: boolean;
begin
  result := Values.ResetMaxPumpingRate;
end;

function TFarmWell_Cell.GetResetMaxPumpingRateAnnotation: string;
begin
  result := Values.ResetMaxPumpingRateAnnotation;
end;

function TFarmWell_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFarmWell_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TFarmWell_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  FarmWellCell: TFarmWell_Cell;
begin
  result := AnotherCell is TFarmWell_Cell;
  if result then
  begin
    FarmWellCell := TFarmWell_Cell(AnotherCell);
    result :=
      (MaxPumpingRate = FarmWellCell.MaxPumpingRate)
      and (ResetMaxPumpingRate = FarmWellCell.ResetMaxPumpingRate)
      and (IFace = FarmWellCell.IFace);
  end;
end;

procedure TFarmWell_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFarmWell_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TFarmWell_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TFarmWell_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TFarmWell_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TFarmWellBoundary }

procedure TFarmWellBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TFarmWell_Cell;
  BoundaryValues: TFarmWellRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFarmWellStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFarmWellStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFarmWell_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.FarmWellArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.FarmWellArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.FarmWellArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.FarmWellArray[BoundaryIndex];
        Cell := TFarmWell_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TFarmWellBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFarmWellCollection;
end;

constructor TFarmWellBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  LocalScreenObject: TScreenObject;
begin
  inherited;
  FFarmId := frmGoPhast.PhastModel.FormulaManager.Add;
  FFarmId.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
  { TODO -cFMP : This needs to be finished. }
//  FFarmId.AddSubscriptionEvents(
//    GlobalRemoveModflowBoundarySubscription,
//    GlobalRestoreModflowBoundarySubscription, self);

  FFarmIdObserver := TObserver.Create(nil);

  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(FFarmIdObserver);
  end;

//  AssignObserverEvents(Collection);

end;

destructor TFarmWellBoundary.Destroy;
var
  LocalScreenObject: TScreenObject;
  PhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) then
  begin
    PhastModel := FModel as TPhastModel;
    if (PhastModel <> nil) and not PhastModel.Clearing
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      LocalScreenObject.StopsTalkingTo(FFarmIdObserver);
    end;
  end;
  { TODO -cFMP : This needs to be finished. }

   // consider using GlobalRemoveScreenObjectDataArraySubscription
   // and GlobalRestoreDataArraySubscription

//  frmGoPhast.PhastModel.FormulaManager.Remove(FFarmId,
//    GlobalRemoveModflowBoundarySubscription,
//    GlobalRestoreModflowBoundarySubscription, self);

  inherited;
end;

procedure TFarmWellBoundary.GetCellListValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TFarmWellStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFarmWellStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TFarmWellStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

procedure TFarmWellBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TFarmWellStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
//  EvaluateArrayBoundaries;
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFarmWellStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TFarmWellStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

function TFarmWellBoundary.GetFarmId: string;
begin
  Result := FFarmId.Formula;
  { TODO -cFMP : This needs to be finished. }
//  ResetItemObserver(0);
end;

procedure TFarmWellBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    { TODO -cFMP : This needs to be finished. }
//    (ParentModel as TPhastModel).InvalidateMfWellPumpage(self);
  end;
end;

class function TFarmWellBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TFarmWellParamItem;
end;

function TFarmWellBoundary.ParameterType: TParameterType;
begin
  result := ptQMAX;
end;

procedure TFarmWellBoundary.SetFarmId(const Value: string);
begin

end;

end.
