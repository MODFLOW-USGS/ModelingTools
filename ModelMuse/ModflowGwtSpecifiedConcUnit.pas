unit ModflowGwtSpecifiedConcUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit, FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, RbwParser, GoPhastTypes,
  System.Generics.Collections, Mt3dmsChemSpeciesUnit,
  Modflow6DynamicTimeSeriesInterfaceUnit, System.Math;

type
  {
    @name stores, the location, time and specifiec concentration for a CNC
    boundary in GWT.
  }
  TCncRecord = record
    Cell: TCellLocation;
    Active: Boolean;
    Concentration: double;
    StartingTime: double;
    EndingTime: double;
    ActiveAnnotation: string;
    ConcentrationAnnotation: string;
    ConcentrationPest: string;
    ConcentrationPestSeriesName: string;
    ConcentrationPestSeriesMethod: TPestParamMethod;
    ConcentrationTimeSeriesName: string;
    // MF6
    Multiplier: double;
    MultiplierAnnotation: string;
    MultiplierPest: string;
    MultiplierPestSeriesName: string;
    MultiplierPestSeriesMethod: TPestParamMethod;
    MultiplierTimeSeriesName: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TCncArray = array of TCncRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of specified concentrations.
  TCncStorage = class(TCustomBoundaryStorage)
  private
    FCncArray: TCncArray;
    function GetCncArray: TCncArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property CncArray: TCncArray read GetCncArray;
  end;

  // @name represents a MODFLOW specified concentration or mass flux
  // for one time interval.
  // @name is stored by @link(TCncCollection or TSrcCollection).
  TCncItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(Concentration).
    FConcentration: IFormulaObject;
    FMultiplier: IFormulaObject;
    FActive: IFormulaObject;
    // See @link(Concentration).
    procedure SetConcentration(const Value: string);
    function GetConcentration: string;
    procedure SetMultiplier(const Value: string);
    function GetMultiplier: string;
    procedure SetActive(const Value: string);
    function GetActive: string;
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property Active: string read GetActive write SetActive;
    property Concentration: string read GetConcentration write SetConcentration;
    property Multiplier: string read GetMultiplier write SetMultiplier;
  end;

  TCncTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the specified concentration
    // or mass flux for a series of
    // boundaries over a series of time intervals.
    FActiveData: TModflowTimeList;
    FConcentrationData: TModflowTimeList;
    FMultiplierData: TModflowTimeList;
    FInvalidateEvent: TNotifyEvent;
    FActiveList: TModflowTimeLists;
    FConcList: TModflowTimeLists;
    FMultiplierList: TModflowTimeLists;
  protected
    procedure CreateTimeLists; override;
    function Description: string; virtual;
    procedure AssignInvalidateEvent; virtual;
    procedure AssignMultInvalidateEvent; virtual;
    procedure AssignActiveInvalidateEvent; virtual;
    property InvalidateEvent: TNotifyEvent read FInvalidateEvent write FInvalidateEvent;
    procedure UpdateGwtTimeLists; override;
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
    procedure RemoveGwtTimeLists(SpeciesIndex: Integer);
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW 6 specified concentration boundaries
  // for a series of time intervals.
  TCncCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateGwtConcentrationsActive(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
    procedure InvalidateGwtMultipliers(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TCncStorage.CncArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    function OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes; override;
  end;

  TCnc_Cell = class(TValueCell)
  private
    FValues: TCncRecord;
    StressPeriod: integer;
    function GetConcentration: double;
    function GetConcentrationAnnotation: string;
    function GetConcentrationPest: string;
    function GetConcentrationPestSeriesMethod: TPestParamMethod;
    function GetConcentrationPestSeriesName: string;
    function GetConcentrationTimeSeriesName: string;
    procedure SetConcentrationTimeSeriesName(const Value: string);
    function GetMultiplier: double;
    function GetMultiplierAnnotation: string;
    function GetMultiplierPest: string;
    function GetMultiplierPestSeriesMethod: TPestParamMethod;
    function GetMultiplierPestSeriesName: string;
    function GetMultiplierTimeSeriesName: string;
    procedure SetMultiplierTimeSeriesName(const Value: string);
    function GetActive: Boolean;
    function GetActiveAnnotation: String;
  protected
    property Values: TCncRecord read FValues;
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetBooleanValue(Index: integer; AModel: TBaseModel): Boolean; override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetBooleanAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
    // MF6
    property Multiplier: double read GetMultiplier;
    property MultiplierAnnotation: string read GetMultiplierAnnotation;
    property MultiplierPest: string read GetMultiplierPest;
    property MultiplierPestSeries: string read GetMultiplierPestSeriesName;
    property MultiplierPestSeriesMethod: TPestParamMethod
      read GetMultiplierPestSeriesMethod;
    property MultiplierTimeSeriesName: string read GetMultiplierTimeSeriesName
      write SetMultiplierTimeSeriesName;
  public
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property Active: Boolean read GetActive;
    property ActiveAnnotation: String read GetActiveAnnotation;
    property Concentration: double read GetConcentration;
    property ConcentrationAnnotation: string read GetConcentrationAnnotation;
    // PEST parameters
    property ConcentrationPest: string read GetConcentrationPest;
    property ConcentrationPestSeries: string read GetConcentrationPestSeriesName;
    property ConcentrationPestSeriesMethod: TPestParamMethod
      read GetConcentrationPestSeriesMethod;
    // Time Series
    property ConcentrationTimeSeriesName: string read GetConcentrationTimeSeriesName
      write SetConcentrationTimeSeriesName;
  end;

  TCncBoundary = class(TModflowBoundary)
  private
    FPestConcentrationMethod: TPestParamMethod;
    FPestConcentrationFormula: IFormulaObject;
    FPestConcentrationObserver: TObserver;
    FPestMultiplierMethod: TPestParamMethod;
    FPestMultiplierFormula: IFormulaObject;
    FPestMultiplierObserver: TObserver;
    FUsedObserver: TObserver;
    FChemSpecies: TChemSpeciesItem;
    FChemSpeciesName: string;
    function GetPestConcentrationFormula: string;
    procedure SetPestConcentrationFormula(const Value: string);
    procedure SetPestConcentrationMethod(const Value: TPestParamMethod);
    function GetPestConcentrationObserver: TObserver;
    function GetPestMultiplierFormula: string;
    procedure SetPestMultiplierFormula(const Value: string);
    procedure SetPestMultiplierMethod(const Value: TPestParamMethod);
    function GetPestMultiplierObserver: TObserver;
    function GetChemSpecies: string;
    procedure SetChemSpecies(const Value: string);
  protected
    procedure InvalidateActiveData(Sender: TObject); virtual;
    procedure InvalidateConcentrationData(Sender: TObject); virtual;
    procedure InvalidateMultiplierData(Sender: TObject); virtual;
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TWell_Cell)s for that stress period.
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
    property PestConcentrationObserver: TObserver
      read GetPestConcentrationObserver;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    function PestObsObserverPrefix: string; virtual;
    function UserObserverPrefix: string; virtual;
    property PestMultiplierObserver: TObserver
      read GetPestMultiplierObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TCncStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
    procedure Loaded;
  published
    property PestConcentrationFormula: string read GetPestConcentrationFormula
      write SetPestConcentrationFormula;
    property PestConcentrationMethod: TPestParamMethod read FPestConcentrationMethod
      write SetPestConcentrationMethod;
    property PestMultiplierFormula: string read GetPestMultiplierFormula
      write SetPestMultiplierFormula;
    property PestMultiplierMethod: TPestParamMethod read FPestMultiplierMethod
      write SetPestMultiplierMethod;
    property ChemSpecies: string read GetChemSpecies write SetChemSpecies;
  end;

  TSrcCollection = class(TCncCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TSrcTimeListLink = class(TCncTimeListLink)
  public
    function Description: string; override;
    procedure AssignInvalidateEvent; override;
    procedure AssignMultInvalidateEvent; override;
    procedure AssignActiveInvalidateEvent; override;
  end;

  TSrcBoundary = class(TCncBoundary)
  protected
    function BoundaryObserverPrefix: string; override;
    function PestObsObserverPrefix: string; override;
    function UserObserverPrefix: string; override;
    procedure InvalidateConcentrationData(Sender: TObject); override;
    procedure InvalidateMultiplierData(Sender: TObject); override;
    procedure InvalidateActiveData(Sender: TObject); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  end;

const
  CncActivePosition = 0;
  CncConcentrationPosition = 1;
  CncMultiplierPosition = 2;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  frmGoPhastUnit, GIS_Functions, frmErrorsAndWarningsUnit, CellLocationUnit;

resourcestring
  StrCNCSpecifiedConcen = 'CNC Specified Concentration';
  StrConcentrationSetTo = 'Concentration set to zero because of a math error';
  StrSRCMassSource = 'SRC: Mass Source';
  StrMultiplierSetTo = 'Multiplier set to one because of a math error';
  StrActiveSetTo = 'Active set to False because of a math error';

{ TCncRecord }

procedure TCncRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompBoolean(Comp, Active);
  WriteCompReal(Comp, Concentration);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ActiveAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationPest));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationPestSeriesName));
  WriteCompInt(Comp, Ord(ConcentrationPestSeriesMethod));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationTimeSeriesName));

  WriteCompReal(Comp, Multiplier);
  WriteCompInt(Comp, Strings.IndexOf(MultiplierAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MultiplierPest));
  WriteCompInt(Comp, Strings.IndexOf(MultiplierPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(MultiplierTimeSeriesName));
  WriteCompInt(Comp, Ord(MultiplierPestSeriesMethod));
end;

procedure TCncRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ActiveAnnotation);
  Strings.Add(ConcentrationAnnotation);
  Strings.Add(ConcentrationPest);
  Strings.Add(ConcentrationPestSeriesName);
  Strings.Add(ConcentrationTimeSeriesName);

  Strings.Add(MultiplierAnnotation);
  Strings.Add(MultiplierPest);
  Strings.Add(MultiplierPestSeriesName);
  Strings.Add(MultiplierTimeSeriesName);
end;

procedure TCncRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Active := ReadCompBoolean(Decomp);
  Concentration := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ActiveAnnotation := Annotations[ReadCompInt(Decomp)];
  ConcentrationAnnotation := Annotations[ReadCompInt(Decomp)];
  ConcentrationPest := Annotations[ReadCompInt(Decomp)];
  ConcentrationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ConcentrationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ConcentrationTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  Multiplier := ReadCompReal(Decomp);
  MultiplierAnnotation := Annotations[ReadCompInt(Decomp)];
  MultiplierPest := Annotations[ReadCompInt(Decomp)];
  MultiplierPestSeriesName := Annotations[ReadCompInt(Decomp)];
  MultiplierTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  MultiplierPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TCncStorage }

procedure TCncStorage.Clear;
begin
  SetLength(FCncArray, 0);
  FCleared := True;
end;

function TCncStorage.GetCncArray: TCncArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FCncArray;
end;

procedure TCncStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FCncArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FCncArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TCncStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FCncArray);
    for Index := 0 to Count - 1 do
    begin
      FCncArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FCncArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TCncItem }

procedure TCncItem.Assign(Source: TPersistent);
var
  CncSource: TCncItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCncItem then
  begin
    CncSource := TCncItem(Source);
    Active := CncSource.Active;
    Concentration := CncSource.Concentration;
    Multiplier := CncSource.Multiplier;
  end;
  inherited;
end;

procedure TCncItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCncCollection;
  ConcentrationObserver: TObserver;
  MultiplierObserver: TObserver;
  ActiveObserver: TObserver;
//  ConcIndex: Integer;
begin
  ParentCollection := Collection as TCncCollection;
  ActiveObserver := FObserverList[CncActivePosition];
  ActiveObserver.OnUpToDateSet := ParentCollection.InvalidateGwtConcentrationsActive;
  ConcentrationObserver := FObserverList[CncConcentrationPosition];
  ConcentrationObserver.OnUpToDateSet := ParentCollection.InvalidateGwtConcentrations;
  MultiplierObserver := FObserverList[CncMultiplierPosition];
  MultiplierObserver.OnUpToDateSet := ParentCollection.InvalidateGwtMultipliers;
end;

function TCncItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

constructor TCncItem.Create(Collection: TCollection);
begin
  inherited;
  Multiplier := '1';
  Active := 'True';
end;

procedure TCncItem.CreateFormulaObjects;
begin
  FActive := CreateFormulaObject(dso3D);
  FConcentration := CreateFormulaObject(dso3D);
  FMultiplier := CreateFormulaObject(dso3D);
end;

destructor TCncItem.Destroy;
begin
  Concentration := '0';
  Multiplier := '0';
  Active := 'False';
  inherited;
end;

function TCncItem.GetActive: string;
begin
  Result := FActive.Formula;
  ResetItemObserver(CncActivePosition);
end;

function TCncItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    CncActivePosition: result := Active;
    CncConcentrationPosition: result := Concentration;
    CncMultiplierPosition: result := Multiplier;
    else
      Assert(False);
  end;
end;

function TCncItem.GetConcentration: string;
begin
  Result := FConcentration.Formula;
  ResetItemObserver(CncConcentrationPosition);
end;

function TCncItem.GetMultiplier: string;
begin
  Result := FMultiplier.Formula;
  ResetItemObserver(CncMultiplierPosition);
end;

procedure TCncItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FActive as TObject then
  begin
    List.Add(FObserverList[CncActivePosition]);
  end;
  if Sender = FConcentration as TObject then
  begin
    List.Add(FObserverList[CncConcentrationPosition]);
  end;
  if Sender = FMultiplier as TObject then
  begin
    List.Add(FObserverList[CncMultiplierPosition]);
  end;
end;

procedure TCncItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  (Collection as TCncCollection).InvalidateModel;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    if Collection is TSrcCollection then
    begin
      PhastModel.InvalidateMassSrcActive(self);
      PhastModel.InvalidateMassSrc(self);
      PhastModel.InvalidateMassSrcMultiplier(self);
    end
    else
    begin
      PhastModel.InvalidateCncActive(self);
      PhastModel.InvalidateCncConcentration(self);
      PhastModel.InvalidateCncMultiplier(self);
    end;
  end;
end;

function TCncItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCncItem;
begin
  result := (AnotherItem is TCncItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCncItem(AnotherItem);
    result := (Item.Concentration = Concentration)
      and (Item.Multiplier = Multiplier)
      and (Item.Active = Active)
  end;
end;

procedure TCncItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FActive,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FConcentration,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMultiplier,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCncItem.SetActive(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncActivePosition, FActive);
end;

procedure TCncItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    CncActivePosition: Active := Value;
    CncConcentrationPosition: Concentration := Value;
    CncMultiplierPosition: Multiplier := Value;
    else
      Assert(False)
  end;
end;

procedure TCncItem.SetConcentration(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncConcentrationPosition, FConcentration);
end;

procedure TCncItem.SetMultiplier(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncMultiplierPosition, FMultiplier);
end;

{ TCncTimeListLink }

procedure TCncTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
var
  ConcActiveTimeList: TModflowTimeList;
  ConcTimeList: TModflowTimeList;
  MultiplierTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;

  ConcActiveTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcActiveTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' Active';
  ConcActiveTimeList.ParamDescription := ConcActiveTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    if self is TSrcTimeListLink then
    begin
      ConcActiveTimeList.OnInvalidate := LocalModel.InvalidateMassSrcActive;
    end
    else
    begin
      ConcActiveTimeList.OnInvalidate := LocalModel.InvalidateCncActive;
    end;
  end;
  AddTimeList(ConcActiveTimeList);
  FActiveList.Add(ConcActiveTimeList);


  ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name;
  ConcTimeList.ParamDescription := ConcTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    if self is TSrcTimeListLink then
    begin
      ConcTimeList.OnInvalidate := LocalModel.InvalidateMassSrc;
    end
    else
    begin
      ConcTimeList.OnInvalidate := LocalModel.InvalidateCncConcentration;
    end;
  end;
  AddTimeList(ConcTimeList);
  FConcList.Add(ConcTimeList);


  MultiplierTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  MultiplierTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' Multiplier';
  MultiplierTimeList.ParamDescription := ' ' + MultiplierTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    if self is TSrcTimeListLink then
    begin
      MultiplierTimeList.OnInvalidate := LocalModel.InvalidateMassSrcMultiplier;
    end
    else
    begin
      MultiplierTimeList.OnInvalidate := LocalModel.InvalidateCncMultiplier;
    end;
  end;
  AddTimeList(MultiplierTimeList);
  FMultiplierList.Add(MultiplierTimeList);

end;

procedure TCncTimeListLink.AssignActiveInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateCncActive;
end;

procedure TCncTimeListLink.AssignInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateCncConcentration;
end;

procedure TCncTimeListLink.AssignMultInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateCncMultiplier;
end;

constructor TCncTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FActiveList := TModflowTimeLists.Create;
  FConcList := TModflowTimeLists.Create;
  FMultiplierList := TModflowTimeLists.Create;
  AssignInvalidateEvent;
  inherited;
end;

procedure TCncTimeListLink.CreateTimeLists;
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FActiveData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FActiveData.NonParamDescription := Description + ' Active';
  FActiveData.ParamDescription := ' ' + Description + ' Active';

  FConcentrationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConcentrationData.NonParamDescription := Description;
  FConcentrationData.ParamDescription := Description;

  FMultiplierData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMultiplierData.NonParamDescription := Description + ' Multiplier';
  FMultiplierData.ParamDescription := ' ' + Description + ' Multiplier';

  if Model <> nil then
  begin
    FActiveData.OnInvalidate := InvalidateEvent;
    FConcentrationData.OnInvalidate := InvalidateEvent;
    FMultiplierData.OnInvalidate := InvalidateEvent;
  end;
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

function TCncTimeListLink.Description: string;
begin
  result := StrCNCSpecifiedConcen;
end;

destructor TCncTimeListLink.Destroy;
begin
  FActiveData.Free;
  FConcentrationData.Free;
  FMultiplierData.Free;
  FActiveList.Free;
  FMultiplierList.Free;
  FConcList.Free;
  inherited;
end;

procedure TCncTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ActiveTimeList: TModflowTimeList;
  ConcTimeList: TModflowTimeList;
  MultiplierTimeList: TModflowTimeList;
begin
  ActiveTimeList := FActiveList[SpeciesIndex];
  RemoveTimeList(ActiveTimeList);
  FActiveList.Delete(SpeciesIndex);

  ConcTimeList := FConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FConcList.Delete(SpeciesIndex);

  MultiplierTimeList := FMultiplierList[SpeciesIndex];
  RemoveTimeList(MultiplierTimeList);
  FMultiplierList.Delete(SpeciesIndex);
end;

procedure TCncTimeListLink.UpdateGwtTimeLists;
var
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
begin
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := FConcList.Count to
      LocalModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
    for SpeciesIndex := LocalModel.MobileComponents.Count to
      FConcList.Count - 1 do
    begin
      RemoveGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

{ TCnCCollection }

procedure TCncCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TCncStorage.Create(AModel));
end;

function TCncCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TCncItem;
begin
  Item := Items[ItemIndex] as TCncItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TCncCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  CncStorage: TCncStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
  AllowedIndicies: Set of Byte;
  ErrorMessage: string;
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
  AllowedIndicies := [CncActivePosition,CncConcentrationPosition,CncMultiplierPosition];

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  CncStorage := BoundaryStorage as TCncStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    AssignDynamicTimeSeries(TimeSeriesName, DynamicTimeSeries, ACell);

    // 2. update locations
    try
      Expression.Evaluate;
      case BoundaryFunctionIndex of
        CncActivePosition:
          begin
            with CncStorage.CncArray[Index] do
            begin
              Active := Expression.BooleanResult;
              ActiveAnnotation := ACell.Annotation;
              ConcentrationPest := '';
              ConcentrationPestSeriesName := '';
              ConcentrationPestSeriesMethod := ppmMultiply;
              ConcentrationTimeSeriesName := '';
            end;
          end;
        CncConcentrationPosition:
          begin
            with CncStorage.CncArray[Index] do
            begin
              Concentration := Expression.DoubleResult;
              ConcentrationAnnotation := ACell.Annotation;
              ConcentrationPest := PestName;
              ConcentrationPestSeriesName := PestSeriesName;
              ConcentrationPestSeriesMethod := PestSeriesMethod;
              ConcentrationTimeSeriesName := TimeSeriesName;
            end;
          end;
        CncMultiplierPosition:
          begin
            with CncStorage.CncArray[Index] do
            begin
              Multiplier := Expression.DoubleResult;
              MultiplierAnnotation := ACell.Annotation;
              MultiplierPest := PestName;
              MultiplierPestSeriesName := PestSeriesName;
              MultiplierPestSeriesMethod := PestSeriesMethod;
              MultiplierTimeSeriesName := TimeSeriesName;
            end;
          end;
        else
          Assert(False);
      end;
    except
      on E: EMathError do
      begin
        case BoundaryFunctionIndex of
          CncActivePosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrActiveSetTo;
                Active := False;
                ActiveAnnotation := ErrorMessage;
                ConcentrationPest := '';
                ConcentrationPestSeriesName := '';
                ConcentrationPestSeriesMethod := ppmMultiply;
                ConcentrationTimeSeriesName := '';
              end;
            end;
          CncConcentrationPosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrConcentrationSetTo;
                Concentration := 0;
                ConcentrationAnnotation := ErrorMessage;
                ConcentrationPest := PestName;
                ConcentrationPestSeriesName := PestSeriesName;
                ConcentrationPestSeriesMethod := PestSeriesMethod;
                ConcentrationTimeSeriesName := TimeSeriesName;
              end;
            end;
          CncMultiplierPosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrMultiplierSetTo;
                Multiplier := 1;
                MultiplierAnnotation := ErrorMessage;
                MultiplierPest := PestName;
                MultiplierPestSeriesName := PestSeriesName;
                MultiplierPestSeriesMethod := PestSeriesMethod;
                MultiplierTimeSeriesName := TimeSeriesName;
              end;
            end;
          else
            Assert(False);
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, ErrorMessage,
          Format(StrObject0sLayerError,
          [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
          ACell.Column+1, E.Message]), LocalScreenObject);
      end;
      on E: ERbwParserError do
      begin
        case BoundaryFunctionIndex of
          CncActivePosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrActiveSetTo;
                Active := False;
                ActiveAnnotation := ErrorMessage;
                ConcentrationPest := '';
                ConcentrationPestSeriesName := '';
                ConcentrationPestSeriesMethod := ppmMultiply;
                ConcentrationTimeSeriesName := '';
              end;
            end;
          CncConcentrationPosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrConcentrationSetTo;
                Concentration := 0;
                ConcentrationAnnotation := ErrorMessage;
                ConcentrationPest := PestName;
                ConcentrationPestSeriesName := PestSeriesName;
                ConcentrationPestSeriesMethod := PestSeriesMethod;
                ConcentrationTimeSeriesName := TimeSeriesName;
              end;
            end;
          CncMultiplierPosition:
            begin
              with CncStorage.CncArray[Index] do
              begin
                ErrorMessage := StrMultiplierSetTo;
                Multiplier := 1;
                MultiplierAnnotation := ErrorMessage;
                MultiplierPest := PestName;
                MultiplierPestSeriesName := PestSeriesName;
                MultiplierPestSeriesMethod := PestSeriesMethod;
                MultiplierTimeSeriesName := TimeSeriesName;
              end;
            end;
          else
            Assert(False);
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, ErrorMessage,
          Format(StrObject0sLayerError,
          [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
          ACell.Column+1, E.Message]), LocalScreenObject);
      end;
    end;
  end;
end;

procedure TCncCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  CncStorage: TCncStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  CncStorage := BoundaryStorage as TCncStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with CncStorage.CncArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

class function TCncCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TCncTimeListLink;
end;

procedure TCncCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCncTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TCncTimeListLink;
    for Index := 0 to Link.FConcList.Count - 1 do
    begin
      TimeList := Link.FConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TCncTimeListLink;
        for Index := 0 to Link.FConcList.Count - 1 do
        begin
          TimeList := Link.FConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TCncCollection.InvalidateGwtConcentrationsActive(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCncTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TCncTimeListLink;
    for Index := 0 to Link.FActiveList.Count - 1 do
    begin
      TimeList := Link.FActiveList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TCncTimeListLink;
        for Index := 0 to Link.FActiveList.Count - 1 do
        begin
          TimeList := Link.FActiveList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TCncCollection.InvalidateGwtMultipliers(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCncTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TCncTimeListLink;
    for Index := 0 to Link.FMultiplierList.Count - 1 do
    begin
      TimeList := Link.FMultiplierList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TCncTimeListLink;
        for Index := 0 to Link.FMultiplierList.Count - 1 do
        begin
          TimeList := Link.FMultiplierList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TCncCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  (BoundaryGroup as TCncBoundary).InvalidateModel;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    if Self is TSrcCollection then
    begin
      PhastModel.InvalidateMassSrc(self);
      PhastModel.InvalidateMassSrcMultiplier(self);
      PhastModel.InvalidateMassSrcActive(self);
    end
    else
    begin
      PhastModel.InvalidateCncConcentration(self);
      PhastModel.InvalidateCncMultiplier(self);
      PhastModel.InvalidateCncActive(self);
    end;
  end;
end;

class function TCncCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCncItem;
end;

function TCncCollection.OkListDataTypes(BoundaryIndex: Integer): TRbwDataTypes;
begin
  case BoundaryIndex of
    CncActivePosition:
      begin
        result := [rdtBoolean];
      end;
    CncConcentrationPosition,CncMultiplierPosition:
      begin
        result := inherited;
      end;
  end;
end;

procedure TCncCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TCncStorage).FCncArray, BoundaryCount);
  inherited;
end;

{ TCnc_Cell }

procedure TCnc_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCnc_Cell.GetActive: Boolean;
begin
  result := Values.Active;
end;

function TCnc_Cell.GetActiveAnnotation: String;
begin
  result := Values.ActiveAnnotation;
end;

function TCnc_Cell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  case Index of
    CncActivePosition: result := ActiveAnnotation;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetBooleanValue(Index: integer; AModel: TBaseModel): Boolean;
begin
  case Index of
    CncActivePosition: result := Active;
    else
      begin
        Result := False;
        Assert(False);
      end;
  end;
end;

function TCnc_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCnc_Cell.GetConcentration: double;
begin
  result := Values.Concentration;
end;

function TCnc_Cell.GetConcentrationAnnotation: string;
begin
  result := Values.ConcentrationAnnotation;
end;

function TCnc_Cell.GetConcentrationPest: string;
begin
  result := Values.ConcentrationPest;
end;

function TCnc_Cell.GetConcentrationPestSeriesMethod: TPestParamMethod;
begin
  result := Values.ConcentrationPestSeriesMethod;
end;

function TCnc_Cell.GetConcentrationPestSeriesName: string;
begin
  result := Values.ConcentrationPestSeriesName;
end;

function TCnc_Cell.GetConcentrationTimeSeriesName: string;
begin
  result := Values.ConcentrationTimeSeriesName;
end;

function TCnc_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TCnc_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TCnc_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCnc_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  case Index of
    CncActivePosition: result := '';
    CncConcentrationPosition: result := ConcentrationTimeSeriesName;
    CncMultiplierPosition: result := MultiplierTimeSeriesName;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetMultiplier: double;
begin
  result := Values.Multiplier;
end;

function TCnc_Cell.GetMultiplierAnnotation: string;
begin
  result := Values.MultiplierAnnotation;
end;

function TCnc_Cell.GetMultiplierPest: string;
begin
  result := Values.MultiplierPest;
end;

function TCnc_Cell.GetMultiplierPestSeriesMethod: TPestParamMethod;
begin
  result := Values.MultiplierPestSeriesMethod;
end;

function TCnc_Cell.GetMultiplierPestSeriesName: string;
begin
  result := Values.MultiplierPestSeriesName;
end;

function TCnc_Cell.GetMultiplierTimeSeriesName: string;
begin
  result := Values.MultiplierTimeSeriesName;
end;

function TCnc_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    CncActivePosition: result := '';
    CncConcentrationPosition: result := ConcentrationPest;
    CncMultiplierPosition: result := MultiplierPest;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    CncActivePosition: result := ppmMultiply;
    CncConcentrationPosition: result := ConcentrationPestSeriesMethod;
    CncMultiplierPosition: result := MultiplierPestSeriesMethod;
    else
      result := inherited;
      Assert(False);
  end;
end;

function TCnc_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    CncActivePosition: result := '';
    CncConcentrationPosition: result := ConcentrationPestSeries;
    CncMultiplierPosition: result := MultiplierPestSeries;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  case Index of
    CncConcentrationPosition: result := ActiveAnnotation;
    CncActivePosition: result := ConcentrationAnnotation;
    CncMultiplierPosition: result := MultiplierAnnotation;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  case Index of
    CncActivePosition:
      begin
        Result := 0.0;
        Assert(False);
      end;
    CncConcentrationPosition: result := Concentration;
    CncMultiplierPosition: result := Multiplier;
    else
      begin
        Result := 0.0;
        Assert(False);
      end;
  end;
end;

function TCnc_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCnc_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TCnc_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Cnc_Cell: TCnc_Cell;
begin
  result := AnotherCell is TCnc_Cell;
  if result then
  begin
    Cnc_Cell := TCnc_Cell(AnotherCell);
    result :=
      (Concentration = Cnc_Cell.Concentration)
        and (Multiplier = Cnc_Cell.Multiplier)
        and (Active = Cnc_Cell.Active)
        and (IFace = Cnc_Cell.IFace)
        and (Values.Cell = Cnc_Cell.Values.Cell);
  end;
end;

procedure TCnc_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCnc_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCnc_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TCnc_Cell.SetConcentrationTimeSeriesName(const Value: string);
begin
  FValues.ConcentrationTimeSeriesName := Value;
end;

procedure TCnc_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TCnc_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
begin
  case Index of
    CncActivePosition:
      begin
        // do nothing
      end;
    CncConcentrationPosition:
      ConcentrationTimeSeriesName := Value;
    CncMultiplierPosition:
      MultiplierTimeSeriesName := Value;
    else
      Assert(False);
  end;
end;

procedure TCnc_Cell.SetMultiplierTimeSeriesName(const Value: string);
begin
  FValues.MultiplierTimeSeriesName := Value;
end;

procedure TCnc_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TCncBoundary }

procedure TCncBoundary.Assign(Source: TPersistent);
var
  SourceCnc: TCncBoundary;
begin
  if Source is TCncBoundary then
  begin
    SourceCnc := TCncBoundary(Source);
    PestConcentrationFormula := SourceCnc.PestConcentrationFormula;
    PestConcentrationMethod := SourceCnc.PestConcentrationMethod;
    PestMultiplierFormula := SourceCnc.PestMultiplierFormula;
    PestMultiplierMethod := SourceCnc.PestMultiplierMethod;
    ChemSpecies := SourceCnc.ChemSpecies;
  end;
  inherited;
end;

procedure TCncBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TCnc_Cell;
  BoundaryValues: TCncRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TCncStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TCncStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
//  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
//    and LocalScreenObject.ModflowMvr.Used
//    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcWel);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCnc_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.CncArray) then
      begin
        Cells.Capacity := Cells.Count + Max(Length(LocalBoundaryStorage.CncArray), Cells.Count div 4);
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.CncArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.CncArray[BoundaryIndex];
        Cell := TCnc_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
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

class function TCncBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TCncCollection;
end;

function TCncBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestConc_';
end;

constructor TCncBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestConcentrationFormula := '';
  FPestConcentrationMethod := DefaultBoundaryMethod(CncConcentrationPosition);
  PestMultiplierFormula := '';
  FPestMultiplierMethod := DefaultBoundaryMethod(CncMultiplierPosition);
end;

procedure TCncBoundary.CreateFormulaObjects;
begin
  FPestConcentrationFormula := CreateFormulaObjectBlocks(dso3D);
  FPestMultiplierFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TCncBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(nil);
    FObserverList.Add(PestConcentrationObserver);
    FObserverList.Add(PestMultiplierObserver);
  end;
end;

class function TCncBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CncActivePosition:
      begin
        result := ppmMultiply;
      end;
    CncConcentrationPosition:
      begin
        result := ppmMultiply;
      end;
    CncMultiplierPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TCncBoundary.Destroy;
begin
  PestConcentrationFormula := '';
  PestMultiplierFormula := '';
  inherited;
end;

procedure TCncBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TCncStorage;
begin
  EvaluateListBoundaries(AModel);

  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TCncStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TCncBoundary.GetChemSpecies: string;
begin
  if FChemSpecies <> nil then
  begin
    Result := FChemSpecies.Name;
  end
  else
  begin
    Result := FChemSpeciesName;
  end;
end;

function TCncBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    CncActivePosition:
      begin
        result := '';
      end;
    CncConcentrationPosition:
      begin
        result := PestConcentrationFormula;
      end;
    CncMultiplierPosition:
      begin
        result := PestMultiplierFormula;
      end;
    else
      Assert(False);
  end;
end;

function TCncBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CncActivePosition:
      begin
        result := ppmMultiply;
      end;
    CncConcentrationPosition:
      begin
        result := PestConcentrationMethod;
      end;
    CncMultiplierPosition:
      begin
        result := PestMultiplierMethod;
      end;
    else
      result := inherited;
      Assert(False);
  end;
end;

function TCncBoundary.GetPestConcentrationFormula: string;
begin
  Result := FPestConcentrationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(CncConcentrationPosition);
  end;
end;

function TCncBoundary.GetPestConcentrationObserver: TObserver;
begin
  if FPestConcentrationObserver = nil then
  begin
    CreateObserver(PestObsObserverPrefix, FPestConcentrationObserver, nil);
    FPestConcentrationObserver.OnUpToDateSet := InvalidateConcentrationData;
  end;
  result := FPestConcentrationObserver;
end;

function TCncBoundary.GetPestMultiplierFormula: string;
begin
  Result := FPestMultiplierFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(CncMultiplierPosition);
  end;
end;

function TCncBoundary.GetPestMultiplierObserver: TObserver;
begin
  if FPestMultiplierObserver = nil then
  begin
    CreateObserver(PestObsObserverPrefix, FPestMultiplierObserver, nil);
    FPestMultiplierObserver.OnUpToDateSet := InvalidateMultiplierData;
  end;
  result := FPestMultiplierObserver;
end;

procedure TCncBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestConcentrationFormula as TObject then
  begin
    if CncConcentrationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[CncConcentrationPosition]);
    end;
  end;
  if Sender = FPestMultiplierFormula as TObject then
  begin
    if CncMultiplierPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[CncMultiplierPosition]);
    end;
  end;
end;

function TCncBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver(UserObserverPrefix, FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TCncBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TCncBoundary.InvalidateActiveData(Sender: TObject);
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
  PhastModel.InvalidateCncActive(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateCncActive(self);
    end;
  end;
end;

procedure TCncBoundary.InvalidateConcentrationData(Sender: TObject);
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
  PhastModel.InvalidateCncConcentration(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateCncConcentration(self);
    end;
  end;
end;

procedure TCncBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateActiveData(self);
    InvalidateConcentrationData(self);
    InvalidateMultiplierData(self);
  end;
end;

procedure TCncBoundary.InvalidateMultiplierData(Sender: TObject);
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
  PhastModel.InvalidateCncMultiplier(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateCncMultiplier(self);
    end;
  end;
end;

procedure TCncBoundary.Loaded;
begin
  ChemSpecies := ChemSpecies;
end;

function TCncBoundary.PestObsObserverPrefix: string;
begin
  result := 'PestConcentration_';
end;

procedure TCncBoundary.SetChemSpecies(const Value: string);
var
  LocalModel: TCustomModel;
begin
  FChemSpeciesName := Value;
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    FChemSpecies := LocalModel.MobileComponents.GetItemByName(Value);
  end;
end;

procedure TCncBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    CncActivePosition:
      begin
        // do nothing
      end;
    CncConcentrationPosition:
      begin
        PestConcentrationFormula := Value;
      end;
    CncMultiplierPosition:
      begin
        PestMultiplierFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TCncBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    CncActivePosition:
      begin
        // do nothing
      end;
    CncConcentrationPosition:
      begin
        PestConcentrationMethod := Value;
      end;
    CncMultiplierPosition:
      begin
        PestMultiplierMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TCncBoundary.SetPestConcentrationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncConcentrationPosition, FPestConcentrationFormula);
end;

procedure TCncBoundary.SetPestConcentrationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConcentrationMethod, Value);
end;

procedure TCncBoundary.SetPestMultiplierFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncMultiplierPosition, FPestMultiplierFormula);
end;

procedure TCncBoundary.SetPestMultiplierMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestMultiplierMethod, Value);
end;

function TCncBoundary.UserObserverPrefix: string;
begin
  result := 'PestConc_Used_';
end;

{ TSrcTimeListLink }

procedure TSrcTimeListLink.AssignInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateMassSrc;
end;

procedure TSrcTimeListLink.AssignMultInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateMassSrcMultiplier;
end;

procedure TSrcTimeListLink.AssignActiveInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateMassSrcActive;
end;

function TSrcTimeListLink.Description: string;
begin
  result := StrSRCMassSource;
end;

{ TSrcBoundary }

class function TSrcBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSrcCollection;
end;

function TSrcBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestSrc_';
end;

procedure TSrcBoundary.InvalidateConcentrationData(Sender: TObject);
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
  PhastModel.InvalidateMassSrc(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateMassSrc(self);
    end;
  end;
end;

procedure TSrcBoundary.InvalidateMultiplierData(Sender: TObject);
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
  PhastModel.InvalidateMassSrcMultiplier(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateMassSrcMultiplier(self);
    end;
  end;
end;

procedure TSrcBoundary.InvalidateActiveData(Sender: TObject);
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
  PhastModel.InvalidateMassSrcActive(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateMassSrcActive(self);
    end;
  end;
end;

function TSrcBoundary.PestObsObserverPrefix: string;
begin
  result := 'PestMassSource_';
end;

function TSrcBoundary.UserObserverPrefix: string;
begin
  result := 'PestSrc_Used_';
end;

{ TsrcCollection }

class function TsrcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSrcTimeListLink;
end;

end.
