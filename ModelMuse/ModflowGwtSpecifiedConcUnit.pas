unit ModflowGwtSpecifiedConcUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes,
  RealListUnit, System.Generics.Collections, Mt3dmsChemSpeciesUnit;

type
  {
    @name stores, the location, time and specifiec concentration for a CNC
    boundary in GWT.
  }
  TCncRecord = record
    Cell: TCellLocation;
    Concentration: double;
    StartingTime: double;
    EndingTime: double;
    ConcentrationAnnotation: string;
    ConcentrationPest: string;
    ConcentrationPestSeriesName: string;
    ConcentrationPestSeriesMethod: TPestParamMethod;
    ConcentrationTimeSeriesName: string;
    // GWT Concentrations
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
    FConcentration: TFormulaObject;
    // See @link(Concentration).
    procedure SetConcentration(const Value: string);
    function GetConcentration: string;
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
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property Concentration: string read GetConcentration write SetConcentration;
  end;

  TCncTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the specified concentration
    // or mass flux for a series of
    // boundaries over a series of time intervals.
    FConcentrationData: TModflowTimeList;
    FInvalidateEvent: TNotifyEvent;
    FConcList: TModflowTimeLists;

  protected
    procedure CreateTimeLists; override;
    function Description: string; virtual;
    procedure AssignInvalidateEvent; virtual;
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
    procedure InvalidateGwtConcentrations(Sender: TObject);
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
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
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
  protected
    property Values: TCncRecord read FValues;
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
    property Concentration: double read GetConcentration;
    property ConcentrationAnnotation: string read GetConcentrationAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
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
    FPestConcentrationFormula: TFormulaObject;
    FPestConcentrationObserver: TObserver;
    FUsedObserver: TObserver;
    FChemSpecies: TChemSpeciesItem;
    FChemSpeciesName: string;
    function GetPestConcentrationFormula: string;
    procedure SetPestConcentrationFormula(const Value: string);
    procedure SetPestConcentrationMethod(const Value: TPestParamMethod);
    function GetPestConcentrationObserver: TObserver;
    function GetChemSpecies: string;
    procedure SetChemSpecies(const Value: string);
  protected
    procedure InvalidateConcentrationData(Sender: TObject); virtual;
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
  end;

  TSrcBoundary = class(TCncBoundary)
  protected
    function BoundaryObserverPrefix: string; override;
    function PestObsObserverPrefix: string; override;
    function UserObserverPrefix: string; override;
    procedure InvalidateConcentrationData(Sender: TObject); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  end;

const
  CncConcentrationPosition = 0;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, frmErrorsAndWarningsUnit,
  ModflowTimeSeriesUnit, ModflowMvrUnit, frmFormulaErrorsUnit;

resourcestring
  StrCNCSpecifiedConcen = 'CNC Specified Concentration';
  StrConcentrationSetTo = 'Concentration set to zero because of a math error';
  StrSRCMassSource = 'SRC: Mass Source';

{ TCncRecord }

procedure TCncRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Concentration);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationPest));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationPestSeriesName));
  WriteCompInt(Comp, Ord(ConcentrationPestSeriesMethod));
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationTimeSeriesName));
end;

procedure TCncRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConcentrationAnnotation);
  Strings.Add(ConcentrationPest);
  Strings.Add(ConcentrationPestSeriesName);
  Strings.Add(ConcentrationTimeSeriesName);
end;

procedure TCncRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Concentration := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConcentrationAnnotation := Annotations[ReadCompInt(Decomp)];
  ConcentrationPest := Annotations[ReadCompInt(Decomp)];
  ConcentrationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ConcentrationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ConcentrationTimeSeriesName := Annotations[ReadCompInt(Decomp)];
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
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
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
    Concentration := CncSource.Concentration;
  end;
  inherited;
end;

procedure TCncItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCncCollection;
  ConcentrationObserver: TObserver;
//  ConcIndex: Integer;
begin
  ParentCollection := Collection as TCncCollection;
  ConcentrationObserver := FObserverList[CncConcentrationPosition];
  ConcentrationObserver.OnUpToDateSet := ParentCollection.InvalidateGwtConcentrations;
end;

function TCncItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TCncItem.CreateFormulaObjects;
begin
  FConcentration := CreateFormulaObject(dso3D);
end;

destructor TCncItem.Destroy;
begin
  Concentration := '0';
  inherited;
end;

function TCncItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    CncConcentrationPosition: result := Concentration;
    else
      Assert(False);
  end;
end;

function TCncItem.GetConcentration: string;
begin
  Result := FConcentration.Formula;
  ResetItemObserver(CncConcentrationPosition);
end;

procedure TCncItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  List.Add(FObserverList[CncConcentrationPosition]);
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
      PhastModel.InvalidateMassSrc(self);
    end
    else
    begin
      PhastModel.InvalidateCncConcentration(self);
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
  end;
end;

procedure TCncItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConcentration,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCncItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    CncConcentrationPosition: Concentration := Value;
    else
      Assert(False)
  end;
end;

procedure TCncItem.SetConcentration(const Value: string);
begin
  UpdateFormulaBlocks(Value, CncConcentrationPosition, FConcentration);
end;

{ TCncTimeListLink }

procedure TCncTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
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
end;

procedure TCncTimeListLink.AssignInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateCncConcentration;
end;

constructor TCncTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FConcList := TModflowTimeLists.Create;
  AssignInvalidateEvent;
  inherited;
end;

procedure TCncTimeListLink.CreateTimeLists;
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FConcentrationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConcentrationData.NonParamDescription := Description;
  FConcentrationData.ParamDescription := Description;
  if Model <> nil then
  begin
    FConcentrationData.OnInvalidate := InvalidateEvent;
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
  FConcentrationData.Free;
  FConcList.Free;
  inherited;
end;

procedure TCncTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
begin
  ConcTimeList := FConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FConcList.Delete(SpeciesIndex);
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

procedure TCncCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName, PestSeriesName: string;
  PestSeriesMethod: TPestParamMethod; TimeSeriesName: string);
var
  CncStorage: TCncStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
  AllowedIndicies: Set of Byte;
  ErrorMessage: string;
begin
  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);
  AllowedIndicies := [0];

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  CncStorage := BoundaryStorage as TCncStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    try
      Expression.Evaluate;
      case BoundaryFunctionIndex of
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
        else
          Assert(False);
      end;
    except
      on E: EMathError do
      begin
        case BoundaryFunctionIndex of
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
    end
    else
    begin
      PhastModel.InvalidateCncConcentration(self);
    end;
  end;
end;

class function TCncCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCncItem;
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
    CncConcentrationPosition: result := ConcentrationTimeSeriesName;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    CncConcentrationPosition: result := ConcentrationPest;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    CncConcentrationPosition: result := ConcentrationPestSeriesMethod;
    else
      result := inherited;
      Assert(False);
  end;
end;

function TCnc_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    CncConcentrationPosition: result := ConcentrationPestSeries;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  case Index of
    CncConcentrationPosition: result := ConcentrationAnnotation;
    else
      Assert(False);
  end;
end;

function TCnc_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  case Index of
    CncConcentrationPosition: result := Concentration;
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
    CncConcentrationPosition:
      Mf6TimeSeriesName[Index] := Value;
    else
      Assert(False);
  end;
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
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.CncArray)
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
        Cell.ScreenObject := ScreenObject;
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
end;

procedure TCncBoundary.CreateFormulaObjects;
begin
  FPestConcentrationFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TCncBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestConcentrationObserver);
  end;
end;

class function TCncBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CncConcentrationPosition:
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
    CncConcentrationPosition:
      begin
        result := PestConcentrationFormula;
      end;
    else
      Assert(False);
  end;
end;

function TCncBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CncConcentrationPosition:
      begin
        result := PestConcentrationMethod;
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

procedure TCncBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestConcentrationFormula then
  begin
    if CncConcentrationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[CncConcentrationPosition]);
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
    InvalidateConcentrationData(self);
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
    CncConcentrationPosition:
      begin
        PestConcentrationFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TCncBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    CncConcentrationPosition:
      begin
        PestConcentrationMethod := Value;
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

function TCncBoundary.UserObserverPrefix: string;
begin
  result := 'PestConc_Used_';
end;

{ TSrcTimeListLink }

procedure TSrcTimeListLink.AssignInvalidateEvent;
begin
  InvalidateEvent := (Model as TCustomModel).InvalidateMassSrc;
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
