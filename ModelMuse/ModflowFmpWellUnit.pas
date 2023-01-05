unit ModflowFmpWellUnit;

interface

uses
  Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes;

type
  TFmpWellRecord = record
    Cell: TCellLocation;
    MaxPumpingRate: double;
    FarmID: Integer;
    PumpOnlyIfCropRequiresWater:  Boolean;
    StartingTime: double;
    EndingTime: double;
    MaxPumpingRateAnnotation: string;
    PumpOnlyIfCropRequiresWaterAnnotation: string;
    FarmIDAnnotation: string;
    Mnw1Well: boolean;
    Mnw2Well: Boolean;
    Mnw2Name: string;

    MaxPumpingRatePestName: string;
    MaxPumpingRatePestSeriesName: string;
    MaxPumpingRatePestSeriesMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TFmpWellArray = array of TFmpWellRecord;

  TFmpWellStorage = class(TCustomBoundaryStorage)
  private
    FFmpWellArray: TFmpWellArray;
    function GetFmpWellArray: TFmpWellArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property FmpWellArray: TFmpWellArray read GetFmpWellArray;
  end;

  TFmpWellItem = class(TCustomModflowBoundaryItem)
  private
    FMaxPumpingRate: TFormulaObject;
    FPumpOnlyIfCropRequiresWater: TFormulaObject;
    FFarmID: TFormulaObject;
    FConstructed: Boolean;
    procedure SetMaxPumpingRate(const Value: string);
    function GetMaxPumpingRate: string;
    procedure SetPumpOnlyIfCropRequiresWater(const Value: string);
    function GetPumpOnlyIfCropRequiresWater: string;
    function GetFarmID: string;
    procedure SetFarmID(const Value: string);
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
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    // @name is the formula used to set the maximum pumping rate
    // or the maximum pumping rate multiplier of this boundary.
    property MaxPumpingRate: string read GetMaxPumpingRate
      write SetMaxPumpingRate;
    property PumpOnlyIfCropRequiresWater: string
      read GetPumpOnlyIfCropRequiresWater write SetPumpOnlyIfCropRequiresWater;
    property FarmID: string read GetFarmID write SetFarmID;
  end;

  TMfFmpWelTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Max pumping rates for a series of
    // FMP Wells over a series of time intervals.
    FMaxPumpingRateData: TModflowTimeList;
    FPumpOnlyIfCropRequiresWaterData: TModflowTimeList;
    FWellIdData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW FMP Well boundaries
  // for a series of time intervals.
  TFmpWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateMaxPumpingRateData(Sender: TObject);
    procedure InvalidatePumpOnlyIfCropRequiresWaterData(Sender: TObject);
    procedure InvalidateFarmIdData(Sender: TObject);
  protected
    function OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TFmpWellStorage.FmpWellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
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
  public
    function TimeListCount(AModel: TBaseModel): integer; override;
  end;

  TFmpWellParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TFmpWell_Cell = class(TValueCell)
  private
    FValues: TFmpWellRecord;
    FStressPeriod: integer;
    function GetMaxPumpingRate: double;
    function GetMaxPumpingRateAnnotation: string;
    function GetPumpOnlyIfCropRequiresWater: boolean;
    function GetPumpOnlyIfCropRequiresWaterAnnotation: string;
    function GetFarmID: integer;
    function GetFarmIDAnnotation: string;
    function GetMnw1: Boolean;
    function GetMnw2: Boolean;
    function GetMnwName: string;
    function GetMaxPumpingRatePestName: string;
    function GetMaxPumpingRatePestSeriesMethod: TPestParamMethod;
    function GetMaxPumpingRatePestSeriesName: string;
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
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
  public
    property MaxPumpingRate: double read GetMaxPumpingRate;
    property MaxPumpingRateAnnotation: string read GetMaxPumpingRateAnnotation;
    property PumpOnlyIfCropRequiresWater: boolean read GetPumpOnlyIfCropRequiresWater;
    property PumpOnlyIfCropRequiresWaterAnnotation: string
      read GetPumpOnlyIfCropRequiresWaterAnnotation;
    property FarmID: integer read GetFarmID;
    property FarmIDAnnotation: string read GetFarmIDAnnotation;
    property Mnw1: Boolean read GetMnw1;
    property Mnw2: Boolean read GetMnw2;
    property MnwName: string read GetMnwName;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // PEST properties
    property MaxPumpingRatePestName: string read GetMaxPumpingRatePestName;
    property MaxPumpingRatePestSeriesName: string read GetMaxPumpingRatePestSeriesName;
    property MaxPumpingRatePestSeriesMethod: TPestParamMethod read GetMaxPumpingRatePestSeriesMethod;

  end;

  TFmpWellBoundary = class(TSpecificModflowBoundary)
  private
    FPestMaxPumpingRateMethod: TPestParamMethod;
    FPestMaxPumpingRateFormula: TFormulaObject;
    FUsedObserver: TObserver;
    FPestMaxPumpingRateObserver: TObserver;
    function GetPestMaxPumpingRateFormula: string;
    function GetPestMaxPumpingRateObserver: TObserver;
    procedure SetPestMaxPumpingRateFormula(const Value: string);
    procedure SetPestMaxPumpingRateMethod(const Value: TPestParamMethod);
    procedure InvalidateMaxPumpingRateData(Sender: TObject);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFmpWell_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
//    procedure GetPropertyObserver(Sender: TObject; List: TList); override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    property PestMaxPumpingRateObserver: TObserver
      read GetPestMaxPumpingRateObserver;
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
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmpWellStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW FMP Well parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TFmpWellStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property PestMaxPumpingRateFormula: string read GetPestMaxPumpingRateFormula
      write SetPestMaxPumpingRateFormula;
    property PestMaxPumpingRateMethod: TPestParamMethod
      read FPestMaxPumpingRateMethod
      write SetPestMaxPumpingRateMethod;
  end;

const
  FmpWellMaxPumpingRatePosition = 0;
  FmpWellFarmIDPosition = 1;
  FmpWellPumpOnlyIfCropRequiresWaterPosition = 2;

resourcestring
  StrMultinodeWellsCan = 'Multinode wells can not be used as farm wells when' +
  ' the Surface-water allotment flag (IALLOTSW) is set to Prior appropriatio' +
  'n system with Water Rights Calls.';

implementation

uses
  ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, ModflowPackageSelectionUnit,
  frmErrorsAndWarningsUnit;

resourcestring
  StrMaxPumpingRateMu = ' max pumping rate multiplier (QMAXfact)';
  StrObjectS = 'Object = "%s"';


{ TFrmWellRecord }

procedure TFmpWellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, MaxPumpingRate);
  WriteCompBoolean(Comp, PumpOnlyIfCropRequiresWater);
  WriteCompInt(Comp, FarmID);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(MaxPumpingRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PumpOnlyIfCropRequiresWaterAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FarmIDAnnotation));
  WriteCompBoolean(Comp, Mnw1Well);
  WriteCompBoolean(Comp, Mnw2Well);
  WriteCompInt(Comp, Strings.IndexOf(Mnw2Name));

  WriteCompInt(Comp, Strings.IndexOf(MaxPumpingRatePestName));
  WriteCompInt(Comp, Strings.IndexOf(MaxPumpingRatePestSeriesName));
  WriteCompInt(Comp, Ord(MaxPumpingRatePestSeriesMethod));
end;

procedure TFmpWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(MaxPumpingRateAnnotation);
  Strings.Add(PumpOnlyIfCropRequiresWaterAnnotation);
  Strings.Add(FarmIDAnnotation);
  Strings.Add(Mnw2Name);

  Strings.Add(MaxPumpingRatePestName);
  Strings.Add(MaxPumpingRatePestSeriesName);
end;

procedure TFmpWellRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  MaxPumpingRate := ReadCompReal(Decomp);
  PumpOnlyIfCropRequiresWater := ReadCompBoolean(Decomp);
  FarmID := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  MaxPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
  PumpOnlyIfCropRequiresWaterAnnotation := Annotations[ReadCompInt(Decomp)];
  FarmIDAnnotation := Annotations[ReadCompInt(Decomp)];
  Mnw1Well := ReadCompBoolean(Decomp);
  Mnw2Well := ReadCompBoolean(Decomp);
  Mnw2Name := Annotations[ReadCompInt(Decomp)];

  MaxPumpingRatePestName := Annotations[ReadCompInt(Decomp)];
  MaxPumpingRatePestSeriesName := Annotations[ReadCompInt(Decomp)];
  MaxPumpingRatePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TFmpWellStorage }

procedure TFmpWellStorage.Clear;
begin
  SetLength(FFmpWellArray, 0);
  FCleared := True;
end;

function TFmpWellStorage.GetFmpWellArray: TFmpWellArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFmpWellArray;
end;

procedure TFmpWellStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFmpWellArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFmpWellArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TFmpWellStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFmpWellArray);
    for Index := 0 to Count - 1 do
    begin
      FFmpWellArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFmpWellArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TFmpWellItem }

procedure TFmpWellItem.Assign(Source: TPersistent);
var
  SourceItem: TFmpWellItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmpWellItem then
  begin
    SourceItem := TFmpWellItem(Source);
    MaxPumpingRate := SourceItem.MaxPumpingRate;
    PumpOnlyIfCropRequiresWater := SourceItem.PumpOnlyIfCropRequiresWater;
    FarmID := SourceItem.FarmID;
  end;
  inherited;
end;

procedure TFmpWellItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmpWellCollection;
  MaxPumpingRateObserver: TObserver;
  PumpOnlyIfCropRequiresWaterObserver: TObserver;
  FarmIDObserver: TObserver;
begin
  ParentCollection := Collection as TFmpWellCollection;

  MaxPumpingRateObserver := FObserverList[FmpWellMaxPumpingRatePosition];
  MaxPumpingRateObserver.OnUpToDateSet :=
    ParentCollection.InvalidateMaxPumpingRateData;

  FarmIDObserver := FObserverList[FmpWellFarmIDPosition];
  FarmIDObserver.OnUpToDateSet := ParentCollection.InvalidateFarmIdData;

  if FmpWellPumpOnlyIfCropRequiresWaterPosition < BoundaryFormulaCount then
  begin
    PumpOnlyIfCropRequiresWaterObserver :=
      FObserverList[FmpWellPumpOnlyIfCropRequiresWaterPosition];
    PumpOnlyIfCropRequiresWaterObserver.OnUpToDateSet :=
      ParentCollection.InvalidatePumpOnlyIfCropRequiresWaterData;
  end;

end;

function TFmpWellItem.BoundaryFormulaCount: integer;
var
  CropIrrigationRequirement: TCropIrrigationRequirement;
begin
  result := -1;
  if not FConstructed  then
  begin
    result := 3;
    Exit;
  end;
  CropIrrigationRequirement :=
    frmGoPhast.PhastModel.ModflowPackages.FarmProcess.CropIrrigationRequirement;
  case CropIrrigationRequirement of
    cirContinuously:
      begin
        result := 2;
      end;
    cirOnlyWhenNeeded:
      begin
        result := 3;
      end;
    else Assert(False);
  end;
end;

constructor TFmpWellItem.Create(Collection: TCollection);
begin
  inherited;
  FConstructed := True;
end;

procedure TFmpWellItem.CreateFormulaObjects;
begin
  FMaxPumpingRate := CreateFormulaObject(dso3D);
  FPumpOnlyIfCropRequiresWater := CreateFormulaObject(dso3D);
  FFarmID := CreateFormulaObject(dso3D);
end;

destructor TFmpWellItem.Destroy;
begin
  MaxPumpingRate := '0';
  PumpOnlyIfCropRequiresWater := 'False';
  FarmID := '0';
  inherited;
end;

function TFmpWellItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRate;
    FmpWellPumpOnlyIfCropRequiresWaterPosition: result := PumpOnlyIfCropRequiresWater;
    FmpWellFarmIDPosition: result := FarmID;
    else Assert(False);
  end;
end;

function TFmpWellItem.GetFarmID: string;
begin
  Result := FFarmID.Formula;
  ResetItemObserver(FmpWellFarmIDPosition);
end;

function TFmpWellItem.GetMaxPumpingRate: string;
begin
  Result := FMaxPumpingRate.Formula;
  ResetItemObserver(FmpWellMaxPumpingRatePosition);
end;

procedure TFmpWellItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FMaxPumpingRate then
  begin
    List.Add(FObserverList[FmpWellMaxPumpingRatePosition]);
  end;
  if Sender = FFarmID then
  begin
    List.Add(FObserverList[FmpWellFarmIDPosition]);
  end;
  if Sender = FPumpOnlyIfCropRequiresWater then
  begin
    List.Add(FObserverList[FmpWellPumpOnlyIfCropRequiresWaterPosition]);
  end;
end;

function TFmpWellItem.GetPumpOnlyIfCropRequiresWater: string;
begin
  Result := FPumpOnlyIfCropRequiresWater.Formula;
  if FmpWellPumpOnlyIfCropRequiresWaterPosition < BoundaryFormulaCount then
  begin
    ResetItemObserver(FmpWellPumpOnlyIfCropRequiresWaterPosition);
  end;
end;

procedure TFmpWellItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfFmpMaxPumpingRate(self);
    PhastModel.InvalidateMfFmpPumpOnlyIfCropRequiresWater(self);
    PhastModel.InvalidateMfFmpWellFarmID(self);
  end;

end;

function TFmpWellItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmpWellItem;
begin
  result := (AnotherItem is TFmpWellItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmpWellItem(AnotherItem);
    result := (Item.MaxPumpingRate = MaxPumpingRate)
      and (Item.PumpOnlyIfCropRequiresWater = PumpOnlyIfCropRequiresWater)
      and (Item.FarmID = FarmID)
  end;
end;

procedure TFmpWellItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaxPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpOnlyIfCropRequiresWater,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFarmID,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmpWellItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    FmpWellMaxPumpingRatePosition: MaxPumpingRate := Value;
    FmpWellFarmIDPosition: FarmID := Value;
    FmpWellPumpOnlyIfCropRequiresWaterPosition: PumpOnlyIfCropRequiresWater := Value;
    else Assert(False);
  end;
end;

procedure TFmpWellItem.SetFarmID(const Value: string);
begin
  UpdateFormulaBlocks(Value, FmpWellFarmIDPosition, FFarmID);
end;

procedure TFmpWellItem.SetMaxPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, FmpWellMaxPumpingRatePosition, FMaxPumpingRate);
end;

procedure TFmpWellItem.SetPumpOnlyIfCropRequiresWater(const Value: string);
begin
  UpdateFormulaBlocks(Value, FmpWellPumpOnlyIfCropRequiresWaterPosition,
    FPumpOnlyIfCropRequiresWater);
end;

{ TMfFmpWelTimeListLink }

procedure TMfFmpWelTimeListLink.CreateTimeLists;
begin
  inherited;
  FMaxPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMaxPumpingRateData.NonParamDescription := StrMaxPumpingRate;
  FMaxPumpingRateData.ParamDescription := StrMaxPumpingRateMu;
  if Model <> nil then
  begin
    FMaxPumpingRateData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfFmpMaxPumpingRate;
  end;
  AddTimeList(FMaxPumpingRateData);

  FWellIdData :=
    TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWellIdData.NonParamDescription :=
    StrFarmID;
  FWellIdData.ParamDescription :=
    ' ' + LowerCase(StrFarmID);
  if Model <> nil then
  begin
    FWellIdData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfFmpWellFarmID;
  end;
  AddTimeList(FWellIdData);

  FPumpOnlyIfCropRequiresWaterData :=
    TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPumpOnlyIfCropRequiresWaterData.NonParamDescription :=
    StrPumpOnlyIfCropRequiresWater;
  FPumpOnlyIfCropRequiresWaterData.ParamDescription :=
    ' ' + LowerCase(StrPumpOnlyIfCropRequiresWater);
  if Model <> nil then
  begin
    FPumpOnlyIfCropRequiresWaterData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfFmpPumpOnlyIfCropRequiresWater;
  end;
  AddTimeList(FPumpOnlyIfCropRequiresWaterData);

end;

destructor TMfFmpWelTimeListLink.Destroy;
begin
  FWellIdData.Free;
  FMaxPumpingRateData.Free;
  FPumpOnlyIfCropRequiresWaterData.Free;
  inherited;
end;

{ TFmpWellCollection }

procedure TFmpWellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmpWellStorage.Create(AModel));
end;

function TFmpWellCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TFmpWellBoundary;
  Item: TFmpWellItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  Boundary := BoundaryGroup as TFmpWellBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  Item := Items[ItemIndex] as TFmpWellItem;
  if FormulaIndex = FmpWellMaxPumpingRatePosition then
  begin
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

procedure TFmpWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject;
  PestName: string; PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  FmpWellStorage: TFmpWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in
    [FmpWellMaxPumpingRatePosition,
    FmpWellPumpOnlyIfCropRequiresWaterPosition,
    FmpWellFarmIDPosition]);
  Assert(Expression <> nil);

  FmpWellStorage := BoundaryStorage as TFmpWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with FmpWellStorage.FmpWellArray[Index] do
    begin
      case BoundaryFunctionIndex of
        FmpWellMaxPumpingRatePosition:
          begin
            MaxPumpingRate := Expression.DoubleResult;
            MaxPumpingRateAnnotation := ACell.Annotation;
            MaxPumpingRatePestName := PestName;
            MaxPumpingRatePestSeriesName := PestSeriesName;
            MaxPumpingRatePestSeriesMethod := PestSeriesMethod;
          end;
        FmpWellFarmIDPosition:
          begin
            FarmID := Expression.IntegerResult;
            FarmIDAnnotation := ACell.Annotation;
          end;
        FmpWellPumpOnlyIfCropRequiresWaterPosition:
          begin
            PumpOnlyIfCropRequiresWater := Expression.BooleanResult;
            PumpOnlyIfCropRequiresWaterAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TFmpWellCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  FmpWellStorage: TFmpWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  FmpWellStorage := BoundaryStorage as TFmpWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with FmpWellStorage.FFmpWellArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

class function TFmpWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfFmpWelTimeListLink;
end;

procedure TFmpWellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfFmpMaxPumpingRate(self);
    PhastModel.InvalidateMfFmpPumpOnlyIfCropRequiresWater(self);
  end;
end;

procedure TFmpWellCollection.InvalidateFarmIdData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMfFmpWelTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMfFmpWelTimeListLink;
    Link.FWellIdData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMfFmpWelTimeListLink;
      Link.FWellIdData.Invalidate;
    end;
  end;
end;

procedure TFmpWellCollection.InvalidateMaxPumpingRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMfFmpWelTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMfFmpWelTimeListLink;
    Link.FMaxPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMfFmpWelTimeListLink;
      Link.FMaxPumpingRateData.Invalidate;
    end;
  end;
end;

procedure TFmpWellCollection.InvalidatePumpOnlyIfCropRequiresWaterData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMfFmpWelTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMfFmpWelTimeListLink;
    Link.FPumpOnlyIfCropRequiresWaterData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMfFmpWelTimeListLink;
      Link.FPumpOnlyIfCropRequiresWaterData.Invalidate;
    end;
  end;
end;

class function TFmpWellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmpWellItem;
end;

function TFmpWellCollection.OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes;
begin
  case BoundaryIndex of
    FmpWellMaxPumpingRatePosition:
      begin
        result := [rdtDouble, rdtInteger];
      end;
    FmpWellFarmIDPosition:
      begin
        result := [rdtInteger];
      end;
    FmpWellPumpOnlyIfCropRequiresWaterPosition:
      begin
        result := [rdtBoolean];
      end;
    else
      Assert(False);
  end;
end;

procedure TFmpWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmpWellStorage).FFmpWellArray,
    BoundaryCount);
  inherited;
end;

function TFmpWellCollection.TimeListCount(AModel: TBaseModel): integer;
var
  CropIrrigationRequirement: TCropIrrigationRequirement;
begin
  result := inherited;
  CropIrrigationRequirement :=
    frmGoPhast.PhastModel.ModflowPackages.FarmProcess.CropIrrigationRequirement;
  case CropIrrigationRequirement of
    cirContinuously:
      begin
        result := result-1;
      end;
    cirOnlyWhenNeeded:
      begin
        // do nothing;
      end;
    else Assert(False);
  end;

end;

{ TFmpWellParamItem }

class function TFmpWellParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TFmpWellCollection;
end;

{ TFmpWell_Cell }

procedure TFmpWell_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TFmpWell_Cell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FmpWellPumpOnlyIfCropRequiresWaterPosition: result :=
      PumpOnlyIfCropRequiresWaterAnnotation;
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetBooleanValue(Index: integer;
  AModel: TBaseModel): boolean;
begin
  result := False;
  case Index of
    FmpWellPumpOnlyIfCropRequiresWaterPosition: result :=
      PumpOnlyIfCropRequiresWater;
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TFmpWell_Cell.GetFarmID: integer;
begin
  result := FValues.FarmID;
end;

function TFmpWell_Cell.GetFarmIDAnnotation: string;
begin
  result := FValues.FarmIDAnnotation;
end;

function TFmpWell_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FmpWellFarmIDPosition: result := FarmIDAnnotation;
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    FmpWellFarmIDPosition: result := FarmID;
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TFmpWell_Cell.GetMaxPumpingRate: double;
begin
  result := FValues.MaxPumpingRate;
end;

function TFmpWell_Cell.GetMaxPumpingRateAnnotation: string;
begin
  result := FValues.MaxPumpingRateAnnotation;
end;

function TFmpWell_Cell.GetMaxPumpingRatePestName: string;
begin
  result := FValues.MaxPumpingRatePestName;
end;

function TFmpWell_Cell.GetMaxPumpingRatePestSeriesMethod: TPestParamMethod;
begin
  result := FValues.MaxPumpingRatePestSeriesMethod;
end;

function TFmpWell_Cell.GetMaxPumpingRatePestSeriesName: string;
begin
  result := FValues.MaxPumpingRatePestSeriesName;
end;

function TFmpWell_Cell.GetMnw1: Boolean;
begin
  result := FValues.Mnw1Well;
end;

function TFmpWell_Cell.GetMnw2: Boolean;
begin
  result := FValues.Mnw2Well;
end;

function TFmpWell_Cell.GetMnwName: string;
begin
  result := FValues.Mnw2Name;
end;

function TFmpWell_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRatePestName;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFmpWell_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRatePestSeriesMethod;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFmpWell_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRatePestSeriesName;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFmpWell_Cell.GetPumpOnlyIfCropRequiresWater: boolean;
begin
  result := FValues.PumpOnlyIfCropRequiresWater;
end;

function TFmpWell_Cell.GetPumpOnlyIfCropRequiresWaterAnnotation: string;
begin
  result := FValues.PumpOnlyIfCropRequiresWaterAnnotation;
end;

function TFmpWell_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRateAnnotation;
    FmpWellFarmIDPosition: result := FarmIDAnnotation;
    FmpWellPumpOnlyIfCropRequiresWaterPosition: result := PumpOnlyIfCropRequiresWaterAnnotation;
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    FmpWellMaxPumpingRatePosition: result := MaxPumpingRate;
    FmpWellFarmIDPosition: result := FarmID;
    FmpWellPumpOnlyIfCropRequiresWaterPosition: result := Ord(PumpOnlyIfCropRequiresWater);
    else Assert(False);
  end;
end;

function TFmpWell_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TFmpWell_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TFmpWell_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  WEFmpL_Cell: TFmpWell_Cell;
begin
  result := AnotherCell is TFmpWell_Cell;
  if result then
  begin
    WEFmpL_Cell := TFmpWell_Cell(AnotherCell);
    result :=
      (MaxPumpingRate = WEFmpL_Cell.MaxPumpingRate)
      and (MaxPumpingRate = WEFmpL_Cell.MaxPumpingRate)
      and (PumpOnlyIfCropRequiresWater = WEFmpL_Cell.PumpOnlyIfCropRequiresWater)
      and (FarmID = WEFmpL_Cell.FarmID)
      and (IFace = WEFmpL_Cell.IFace)
      and (Mnw1 = WEFmpL_Cell.Mnw1)
      and (Mnw2 = WEFmpL_Cell.Mnw2)
      and (MnwName = WEFmpL_Cell.MnwName)
      and (FValues.Cell = WEFmpL_Cell.FValues.Cell)
  end;
end;

procedure TFmpWell_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TFmpWell_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TFmpWell_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TFmpWell_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Column := Layer;
end;

procedure TFmpWell_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Layer;
end;

{ TMfFmpWellBoundary }

procedure TFmpWellBoundary.Assign(Source: TPersistent);
var
  SourceWell: TFmpWellBoundary;
begin
  if Source is TFmpWellBoundary then
  begin
    SourceWell := TFmpWellBoundary(Source);
    PestMaxPumpingRateFormula := SourceWell.PestMaxPumpingRateFormula;
    PestMaxPumpingRateMethod := SourceWell.PestMaxPumpingRateMethod;
  end;
  inherited;
end;

procedure TFmpWellBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TFmpWell_Cell;
  BoundaryValues: TFmpWellRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmpWellStorage;
  LocalModel: TCustomModel;
//  LocalScreenObject: TScreenObject;
  Mnw1Used: Boolean;
  Boundaries: TModflowBoundaries;
  Mnw2Used: Boolean;
  WellId: string;
  FmpPackage: TFarmProcess;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmpWellStorage;
  FmpPackage := LocalModel.ModflowPackages.FarmProcess;

  Boundaries := (ScreenObject as TScreenObject).ModflowBoundaries;

  Mnw1Used := (Boundaries.ModflowMnw1Boundary <> nil)
    and Boundaries.ModflowMnw1Boundary.Used;
  Mnw2Used := (Boundaries.ModflowMnw2Boundary <> nil)
    and Boundaries.ModflowMnw2Boundary.Used;
  if Mnw2Used then
  begin
    WellId := Boundaries.ModflowMnw2Boundary.WellID;
  end
  else
  begin
    WellId := '';
  end;
  if (Mnw1Used or Mnw2Used)
    and (FmpPackage.SurfaceWaterAllotment in [swaPriorWithCalls,
      swaPriorWithoutCalls]) then
  begin
    frmErrorsAndWarnings.AddError(LocalModel, StrMultinodeWellsCan,
      Format(StrObjectS, [(ScreenObject as TScreenObject).Name]), ScreenObject);
  end;

  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFmpWell_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.FmpWellArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.FmpWellArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.FmpWellArray) - 1 do
      begin
        if (Mnw1Used or Mnw2Used) and (BoundaryIndex > 0) then
        begin
          Continue;
        end;
        BoundaryValues := LocalBoundaryStorage.FmpWellArray[BoundaryIndex];
        BoundaryValues.Mnw1Well := Mnw1Used;
        BoundaryValues.Mnw2Well := Mnw2Used;
        BoundaryValues.Mnw2Name := WellId;
        Cell := TFmpWell_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.FStressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TFmpWellBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmpWellCollection;
end;


function TFmpWellBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestMaxWel_';
end;

constructor TFmpWellBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestMaxPumpingRateFormula := '';
  FPestMaxPumpingRateMethod := DefaultBoundaryMethod(FmpWellMaxPumpingRatePosition);

end;

procedure TFmpWellBoundary.CreateFormulaObjects;
begin
  FPestMaxPumpingRateFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TFmpWellBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestMaxPumpingRateObserver);
  end;
end;

class function TFmpWellBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    FmpWellMaxPumpingRatePosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

destructor TFmpWellBoundary.Destroy;
begin
  PestMaxPumpingRateFormula := '';

  inherited;
end;

procedure TFmpWellBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmpWellStorage;
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
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmpWellStorage;
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
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TFmpWellStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

function TFmpWellBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FmpWellMaxPumpingRatePosition of
    FmpWellMaxPumpingRatePosition:
      begin
        result := PestMaxPumpingRateFormula;
      end;
    else
      Assert(False);
  end;
end;

function TFmpWellBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FmpWellMaxPumpingRatePosition of
    FmpWellMaxPumpingRatePosition:
      begin
        result := PestMaxPumpingRateMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TFmpWellBoundary.GetPestMaxPumpingRateFormula: string;
begin
  Result := FPestMaxPumpingRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(FmpWellMaxPumpingRatePosition);
  end;
end;

function TFmpWellBoundary.GetPestMaxPumpingRateObserver: TObserver;
begin
  if FPestMaxPumpingRateObserver = nil then
  begin
    CreateObserver('PestMaxPumpingRate_', FPestMaxPumpingRateObserver, nil);
    FPestMaxPumpingRateObserver.OnUpToDateSet := InvalidateMaxPumpingRateData;
  end;
  result := FPestMaxPumpingRateObserver;
end;

procedure TFmpWellBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestMaxPumpingRateFormula then
  begin
    if FmpWellMaxPumpingRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[FmpWellMaxPumpingRatePosition]);
    end;
  end;
end;

function TFmpWellBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestMaxWell_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFmpWellBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TFmpWellBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfFmpMaxPumpingRate(self);
    Model.InvalidateMfFmpPumpOnlyIfCropRequiresWater(self);
  end;
end;

procedure TFmpWellBoundary.InvalidateMaxPumpingRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfFmpMaxPumpingRate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfFmpMaxPumpingRate(self);
    end;
  end;
end;

class function TFmpWellBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TFmpWellParamItem;
end;

function TFmpWellBoundary.ParameterType: TParameterType;
begin
  result := ptQMAX;
end;

procedure TFmpWellBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FmpWellMaxPumpingRatePosition of
    FmpWellMaxPumpingRatePosition:
      begin
        PestMaxPumpingRateFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TFmpWellBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FmpWellMaxPumpingRatePosition of
    FmpWellMaxPumpingRatePosition:
      begin
        PestMaxPumpingRateMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TFmpWellBoundary.SetPestMaxPumpingRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, FmpWellMaxPumpingRatePosition, FPestMaxPumpingRateFormula);
end;

procedure TFmpWellBoundary.SetPestMaxPumpingRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestMaxPumpingRateMethod, Value);
end;

end.
