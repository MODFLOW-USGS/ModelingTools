unit Mt3dmsTobUnit;

interface

uses
  ZLib, Classes, ModflowCellUnit, OrderedCollectionUnit, Contnrs, GoPhastTypes,
  ModflowBoundaryUnit, DataSetUnit, System.Generics.Collections;

type
  TMt3dmsTobRecord = record
    Cell: TCellLocation;
    Time: double;
    ObsType: TObservationType;
    ObsFreq: integer;
    ComponentIndex: Integer;
    Weight: double;
    Concentration: Double;
    ConcentrationAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMt3dmsTobItem = class(TCustomLocationObservation)
  private
    FConcentration: double;
    FWeight: double;
    FSpeciesName: string;
    FObservationType: TObservationType;
    FObservationFrequency: integer;
    procedure SetConcentration(const Value: double);
    procedure SetWeight(const Value: double);
    function GetComponentIndex: integer;
    procedure SetSpeciesName(const Value: string);
    procedure SetObservationType(const Value: TObservationType);
    procedure SetObservationFrequency(const Value: integer);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
  public
    property ComponentIndex: integer read GetComponentIndex;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the observed concentration
    // of this concentration observation.
    property Concentration: double read FConcentration write SetConcentration;
    property Weight: double read FWeight write SetWeight;
    property SpeciesName: string read FSpeciesName write SetSpeciesName;
    property ObservationType: TObservationType read FObservationType
      write SetObservationType;
    property ObservationFrequency: integer read FObservationFrequency
      write SetObservationFrequency;
  end;

  TMt3dmsConcObsTimeList = class;

  TMt3dmsConcObsTimesModelLink = class(TObject)
  private
    FObsTimes: TMt3dmsConcObsTimeList;
    FModel: TBaseModel;
  public
    Constructor Create(AModel: TBaseModel);
    Destructor Destroy; override;
  end;

  TMt3dmsConcObsTimesModelLinkList = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    function GetLink(AModel: TBaseModel): TMt3dmsConcObsTimesModelLink;
  public
    property Links[AModel: TBaseModel]: TMt3dmsConcObsTimesModelLink read GetLink;
    Constructor Create;
    Destructor Destroy; override;
    procedure RemoveLink(AModel: TBaseModel);
  end;

  TMt3dmsTransObservations = class;

  // @name represents MODFLOW Head observations
  // for a series of times.
  TMt3dmsTobCollection = class(TCustomObjectOrderedCollection)
  private
    FBoundary: TMt3dmsTransObservations;
    FObservationConcentrations: TMt3dmsConcObsTimeList;
    FObsTimesModelLinkList: TMt3dmsConcObsTimesModelLinkList;
    FObservationRowOffset: double;
    FObservationColumnOffset: double;
//    FScreenObject: TObject;
    function GetMt3dmsTobItems(Index: integer): TMt3dmsTobItem;
    function GetObservationConcentrations(AModel: TBaseModel): TMt3dmsConcObsTimeList;
  protected
    procedure InvalidateModel; override;
  public
    procedure RemoveModelLink(AModel: TBaseModel);
//    property ScreenObject: TObject read FScreenObject;
    // @name creates an instance of @classname
    constructor Create(Boundary: TMt3dmsTransObservations; Model: TBaseModel;
      ScreenObject: TObject);
    procedure EvaluateConcentrationObservations(AModel: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    property TobItems[Index: integer]: TMt3dmsTobItem read GetMt3dmsTobItems;
    // ROFF
    property ObservationRowOffset: double read FObservationRowOffset;
    // COFF
    property ObservationColumnOffset: double read FObservationColumnOffset;
    property ObservationConcentrations[AModel: TBaseModel]: TMt3dmsConcObsTimeList
      read GetObservationConcentrations;
    function CountObservationTimes(StartTime, EndTime: double): integer;
    procedure RenameItems(const OldSpeciesName, NewSpeciesName: string);
    procedure DeleteSpecies(const SpeciesName: string);
  end;

  TMt3dmsTob_Cell = class(TValueCell)
  private
    Values: TMt3dmsTobRecord;
    function GetConcentration: double;
    function GetTime: double;
    function GetConcentrationAnnotation: string;
    function GetWeight: double;
    function GetComponentIndex: integer;
  protected
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
  public
    property Concentration: double read GetConcentration;
    property Weight: Double read GetWeight;
    property ComponentIndex: integer read GetComponentIndex;
    property Time: double read GetTime;
    property ConcentrationAnnotation: string read GetConcentrationAnnotation;
    property ObsType: TObservationType read Values.ObsType;
    property ObsFreq: Integer read Values.ObsFreq;
  end;

  TMt3dmsTobsCellList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TMt3dmsTob_Cell;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Add(Cell: TMt3dmsTob_Cell);
    property Count: integer read GetCount;
    property Items[Index: integer]: TMt3dmsTob_Cell read GetItem; default;
  end;

  // @name represents the MODFLOW Head observations associated with
  // a single @link(TScreenObject).
  TMt3dmsTransObservations = class(TCustomMultilayerLocationObsBoundary)
  private
    FValues: TMt3dmsTobCollection;
    procedure SetValues(const Value: TMt3dmsTobCollection);
    function GetCellList(Index: integer): TMt3dmsTobsCellList;
    function GetCellListCount: integer;
  public
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name checks that the Purpose parameter matches @link(Purpose)
    // and, if so, calls @link(TMt3dmsTobCollection.EvaluateConcentrationObservations
    // Values.EvaluateConcentrationObservations)
    procedure EvaluateConcentrationObservations(Purpose: TObservationPurpose;
      AModel: TBaseModel);
    function Used: boolean; override;
    procedure Clear; virtual;
    property CellLists[Index: integer]: TMt3dmsTobsCellList read GetCellList;
    property CellListCount: integer read GetCellListCount;
    procedure RemoveModelLink(AModel: TBaseModel);
    procedure RenameSpecies(const OldSpeciesName, NewSpeciesName: string);
    procedure DeleteSpecies(const SpeciesName: string);
  published
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: TMt3dmsTobCollection read FValues write SetValues;
  end;

  // @name is used to store a series of @link(TDataArray)s for concentration
  // observations in MT3DMS.
  TMt3dmsConcObsTimeList = class(TCustomTimeList)
  private
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FCellList: TList;
    FSpeciesList: TList<Integer>;
    function GetCellList(Index: integer): TMt3dmsTobsCellList;
  protected
    // @name calls the inherited @link(TCustomTimeList.SetUpToDate)
    // and then calls @link(OnInvalidate) if @link(OnInvalidate) is assigned.
    procedure SetUpToDate(const Value: boolean); override;
  public
    function Add(const ATime: double; const Data: TDataArray; Species: Integer): integer; reintroduce;
    procedure Clear; override;
    constructor Create(Model: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;

    // @name takes the times and formulas in BoundaryValues and uses them
    // to determine the locations and values for those times.  These
    // locations and values are stored in @link(TRealSparseDataSet)s
    // accessed through @link(TCustomTimeList.Items Items).
    procedure Initialize(ObservationValues: TMt3dmsTobCollection;
      ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel); reintroduce;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property CellLists[Index: integer]: TMt3dmsTobsCellList read GetCellList; default;
  end;

resourcestring
  StrConcentratonObservationsError = 'Concentraton observations can only be '
    + 'defined using objects with a single vertex.  The following objects need '
    + 'to be fixed.';
  StrInTheFollowingObj = 'In the following objects, no chemical species is d' +
  'efined in one or more times for the MT3DMS or MT3D-USGS transport observation package.';
  Mt3dTobErrorRoot = 'Error: Duplicate concentration observation times';

implementation

uses
  PhastModelUnit, ScreenObjectUnit, ModflowGridUnit, FastGEO,
  frmErrorsAndWarningsUnit, SysUtils, RealListUnit;

resourcestring
  EarlyTimeWarning = 'Concentration observation times earlier than the beginning of the first stress period will be ignored.';
  LateTimeWarning = 'Concentration observation times later than the end of the last stress period will be ignored.';
{ TMt3dmsTobRecord }

procedure TMt3dmsTobRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, ComponentIndex);
  WriteCompReal(Comp, Time);
  WriteCompReal(Comp, Weight);
  WriteCompReal(Comp, Concentration);
  WriteCompInt(Comp, Strings.IndexOf(ConcentrationAnnotation));
end;

procedure TMt3dmsTobRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConcentrationAnnotation);
end;

procedure TMt3dmsTobRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  ComponentIndex := ReadCompInt(Decomp);
  Time := ReadCompReal(Decomp);
  Weight := ReadCompReal(Decomp);
  Concentration := ReadCompReal(Decomp);
  ConcentrationAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TMt3dmsTobItem }

procedure TMt3dmsTobItem.Assign(Source: TPersistent);
var
  SourceItem: TMt3dmsTobItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TMt3dmsTobItem then
  begin
    SourceItem := TMt3dmsTobItem(Source);
    Concentration := SourceItem.Concentration;
    Weight := SourceItem.Weight;
    SpeciesName := SourceItem.SpeciesName;
    ObservationType := SourceItem.ObservationType;
    ObservationFrequency := SourceItem.ObservationFrequency;
  end;
  inherited;
end;

function TMt3dmsTobItem.GetComponentIndex: integer;
begin
  result := -1;
  if Model <> nil then
  begin
    result := (Model as TCustomModel).IndexOfMt3dmsSpeciesName(SpeciesName);
  end;
end;

procedure TMt3dmsTobItem.InvalidateModel;
begin
  (Collection as TMt3dmsTobCollection).InvalidateModel;
end;

function TMt3dmsTobItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMt3dmsTobItem;
begin
  result := inherited IsSame(AnotherItem) and (AnotherItem is TMt3dmsTobItem);
  if result then
  begin
    Item := TMt3dmsTobItem(AnotherItem);
    result := (Item.Concentration = Concentration)
      and (Item.Weight = Weight)
      and (Item.ComponentIndex = ComponentIndex)
      and (Item.ObservationType = ObservationType)
      and (Item.ObservationFrequency = ObservationFrequency);
  end;
end;

procedure TMt3dmsTobItem.SetConcentration(const Value: double);
begin
  if FConcentration <> Value then
  begin
    FConcentration := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTobItem.SetObservationFrequency(const Value: integer);
begin
  SetIntegerProperty(FObservationFrequency, Value);
end;

procedure TMt3dmsTobItem.SetObservationType(const Value: TObservationType);
begin
  if FObservationType <> Value then
  begin
    FObservationType := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTobItem.SetSpeciesName(const Value: string);
begin
  if FSpeciesName <> Value then
  begin
    FSpeciesName := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTobItem.SetWeight(const Value: double);
begin
  if FWeight <> Value then
  begin
    FWeight := Value;
    InvalidateModel;
  end;
end;

{ TMt3dmsConcObsTimesModelLink }

constructor TMt3dmsConcObsTimesModelLink.Create(AModel: TBaseModel);
begin
  FModel := AModel;
  FObsTimes := TMt3dmsConcObsTimeList.Create(FModel);
end;

destructor TMt3dmsConcObsTimesModelLink.Destroy;
begin
  FObsTimes.Free;
  inherited;
end;

{ TMt3dmsConcObsTimesModelLinkList }

constructor TMt3dmsConcObsTimesModelLinkList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TMt3dmsConcObsTimesModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMt3dmsConcObsTimesModelLinkList.GetLink(
  AModel: TBaseModel): TMt3dmsConcObsTimesModelLink;
var
  ModelIndex: Integer;
  Item: TMt3dmsConcObsTimesModelLink;
begin
  for ModelIndex := 0 to FList.Count - 1 do
  begin
    Item := FList[ModelIndex];
    if Item.FModel = AModel then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := TMt3dmsConcObsTimesModelLink.Create(AModel);
  FList.Add(result);
  if AModel <> nil then
  begin
    result.FObsTimes.OnInvalidate :=
      (AModel as TCustomModel).InvalidateMt3dTobConcs;
  end;
end;

procedure TMt3dmsConcObsTimesModelLinkList.RemoveLink(AModel: TBaseModel);
var
  Index: Integer;
  ALink: TMt3dmsConcObsTimesModelLink;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.FModel = AModel then
    begin
      FList.Delete(Index);
      Break;
    end;
  end;
end;

{ TMt3dmsTobCollection }

function TMt3dmsTobCollection.CountObservationTimes(StartTime,
  EndTime: double): integer;
var
  Index: Integer;
  Item: TMt3dmsTobItem;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := TobItems[Index];
    if (StartTime <= Item.Time) and (Item.Time <= EndTime) then
    begin
      Inc(result);
    end;
  end;
end;

constructor TMt3dmsTobCollection.Create(Boundary: TMt3dmsTransObservations;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(TMt3dmsTobItem, Model, ScreenObject);
  FBoundary := Boundary;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
  FObsTimesModelLinkList := TMt3dmsConcObsTimesModelLinkList.Create;
end;

procedure TMt3dmsTobCollection.DeleteSpecies(const SpeciesName: string);
var
  ItemIndex: integer;
  AnItem: TMt3dmsTobItem;
begin
  for ItemIndex := Count - 1 downto 0 do
  begin
    AnItem := Items[ItemIndex] as TMt3dmsTobItem;
    if AnsiCompareText(SpeciesName, AnItem.SpeciesName) = 0 then
    begin
      Delete(ItemIndex);
    end;
  end;
end;

destructor TMt3dmsTobCollection.Destroy;
begin
  FObsTimesModelLinkList.Free;
  inherited;
end;

procedure TMt3dmsTobCollection.EvaluateConcentrationObservations(
  AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  ObservationPoint: TPoint2D;
  Row: Integer;
  Column: Integer;
  Width: Real;
  Center: Real;
  CellList: TMt3dmsTobsCellList;
  Cell : TMt3dmsTob_Cell;
begin
  ObservationConcentrations[AModel].Initialize(self, ScreenObject, True, AModel);

  if FObservationConcentrations.FCellList.Count > 0 then
  begin
    CellList := FObservationConcentrations.FCellList[0];
    if CellList.Count > 0 then
    begin
      Assert(ScreenObject <> nil);
      LocalScreenObject := ScreenObject as TScreenObject;
      Assert(LocalScreenObject.ViewDirection = vdTop);
      Assert(Model <> nil);
      LocalModel := AModel as TCustomModel;
      Grid := LocalModel.ModflowGrid;
      Assert(Grid <> nil);

      if LocalScreenObject.Count > 1 then
      begin
        FObservationRowOffset := -1000;
        FObservationColumnOffset := -1000;
        frmErrorsAndWarnings.AddError(FObservationConcentrations.Model,
          StrConcentratonObservationsError, LocalScreenObject.Name,
          LocalScreenObject)
      end
      else
      begin
        Assert(LocalScreenObject.Count = 1);
        ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
          LocalScreenObject.Points[0]);

        Cell := CellList[0];
        Row := Cell.Row;
        Column := Cell.Column;

        Width := Grid.RowWidth[Row];
        Center := Grid.RowCenter(Row);
        FObservationRowOffset := -(ObservationPoint.y - Center)/Width;

        Width := Grid.ColumnWidth[Column];
        Center := Grid.ColumnCenter(Column);
        FObservationColumnOffset := (ObservationPoint.x - Center)/Width;
      end;
    end;
  end;
end;

function TMt3dmsTobCollection.GetMt3dmsTobItems(Index: integer): TMt3dmsTobItem;
begin
  result := Items[Index] as TMt3dmsTobItem;
end;

function TMt3dmsTobCollection.GetObservationConcentrations(
  AModel: TBaseModel): TMt3dmsConcObsTimeList;
begin
  FObservationConcentrations := FObsTimesModelLinkList.Links[AModel].FObsTimes;
  result := FObservationConcentrations;
end;

procedure TMt3dmsTobCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  FBoundary.InvalidateModel;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMt3dTobConcs(self);
  end;
end;

procedure TMt3dmsTobCollection.RemoveModelLink(AModel: TBaseModel);
begin
  FObsTimesModelLinkList.RemoveLink(AModel);
end;

procedure TMt3dmsTobCollection.RenameItems(const OldSpeciesName,
  NewSpeciesName: string);
var
  ItemIndex: integer;
  AnItem: TMt3dmsTobItem;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex] as TMt3dmsTobItem;
    if AnsiCompareText(OldSpeciesName, AnItem.SpeciesName) = 0 then
    begin
      AnItem.SpeciesName := NewSpeciesName;
    end;
  end;
end;

{ TMt3dmsTob_Cell }

procedure TMt3dmsTob_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
end;

function TMt3dmsTob_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TMt3dmsTob_Cell.GetComponentIndex: integer;
begin
  result := Values.ComponentIndex;
end;

function TMt3dmsTob_Cell.GetConcentration: double;
begin
  result := Values.Concentration;
end;

function TMt3dmsTob_Cell.GetConcentrationAnnotation: string;
begin
  result := Values.ConcentrationAnnotation;
end;

function TMt3dmsTob_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TMt3dmsTob_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    0: Result := ComponentIndex;
    else Assert(False);
  end;
end;

function TMt3dmsTob_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TMt3dmsTob_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := ConcentrationAnnotation;
    else Assert(False);
  end;
end;

function TMt3dmsTob_Cell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := Concentration;
    1: result := Weight;
    else Assert(False);
  end;
end;

function TMt3dmsTob_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TMt3dmsTob_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TMt3dmsTob_Cell.GetTime: double;
begin
  result := Values.Time;
end;

function TMt3dmsTob_Cell.GetWeight: double;
begin
  result := Values.Weight;
end;

procedure TMt3dmsTob_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TMt3dmsTob_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
end;

procedure TMt3dmsTob_Cell.SetColumn(const Value: integer);
begin
  inherited;
  Values.Cell.Column := Value;
end;

procedure TMt3dmsTob_Cell.SetLayer(const Value: integer);
begin
  inherited;
  Values.Cell.Layer := Value;
end;

procedure TMt3dmsTob_Cell.SetRow(const Value: integer);
begin
  inherited;
  Values.Cell.Row := Value;
end;

{ TMt3dmsTobsCellList }

procedure TMt3dmsTobsCellList.Add(Cell: TMt3dmsTob_Cell);
begin
  FList.Add(Cell);
end;

constructor TMt3dmsTobsCellList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TMt3dmsTobsCellList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMt3dmsTobsCellList.GetCount: integer;
begin
  result := FList.Count;
end;

function TMt3dmsTobsCellList.GetItem(Index: integer): TMt3dmsTob_Cell;
begin
  result := FList[Index];
end;

{ TMt3dmsTobBoundary }

procedure TMt3dmsTransObservations.Assign(Source: TPersistent);
var
  TransObsSource: TMt3dmsTransObservations;
begin
  if Source is TMt3dmsTransObservations then
  begin
    TransObsSource := TMt3dmsTransObservations(Source);
    Values := TransObsSource.Values;
  end;
  inherited;
end;

procedure TMt3dmsTransObservations.Clear;
begin
  Values.Clear;
end;

constructor TMt3dmsTransObservations.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FValues:= TMt3dmsTobCollection.Create(self, Model, ScreenObject);
end;

procedure TMt3dmsTransObservations.DeleteSpecies(const SpeciesName: string);
begin
  Values.DeleteSpecies(SpeciesName);
end;

destructor TMt3dmsTransObservations.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TMt3dmsTransObservations.EvaluateConcentrationObservations(
  Purpose: TObservationPurpose; AModel: TBaseModel);
begin
  if self.Purpose = Purpose then
  begin
    Values.EvaluateConcentrationObservations(AModel);
  end;
end;

function TMt3dmsTransObservations.GetCellList(Index: integer): TMt3dmsTobsCellList;
begin
  result := Values.FObservationConcentrations.CellLists[Index];
end;

function TMt3dmsTransObservations.GetCellListCount: integer;
begin
  result := Values.FObservationConcentrations.FCellList.Count;
end;

procedure TMt3dmsTransObservations.RemoveModelLink(AModel: TBaseModel);
begin
  Values.RemoveModelLink(AModel);
end;

procedure TMt3dmsTransObservations.RenameSpecies(const OldSpeciesName,
  NewSpeciesName: string);
begin
  Values.RenameItems(OldSpeciesName, NewSpeciesName);
end;

procedure TMt3dmsTransObservations.SetValues(const Value: TMt3dmsTobCollection);
begin
  FValues.Assign(Value);
end;

function TMt3dmsTransObservations.Used: boolean;
begin
  result := FValues.Count > 0;
end;

{ TMt3dmsConcObsTimeList }

function TMt3dmsConcObsTimeList.Add(const ATime: double; const Data: TDataArray;
  Species: Integer): integer;
begin
  CheckSameModel(Data);
  result := IndexOf(ATime);
  if result >= 0 then
  begin
    while Result < Count do
    begin
      if FSpeciesList[Result] = Species then
      begin
        Break;
      end
      else
      begin
        Inc(Result);
        if Result >= Count then
        begin
          Result := -1;
          break;
        end
        else if FTimes[Result] <> ATime then
        begin
          Result := -1;
          break
        end;
      end;
    end;
    if Result >= 0 then
    begin
      Assert(FData[result] = Data);
      Exit;
    end;
  end;

  result := FTimes.Add(ATime);
  if result >= FData.Count then
  begin
    FData.Add(Data);
    FSpeciesList.Add(Species);
  end
  else
  begin
    FData.Insert(result, Data);
    FSpeciesList.Insert(result,Species);
  end;

  if Data <> nil then
  begin
    Data.Limits := Limits;
    Data.Max := Max;
    Data.Min := Min;
    Data.CheckMax := CheckMax;
    Data.CheckMin := CheckMin;
  end;

  Invalidate;
end;

procedure TMt3dmsConcObsTimeList.Clear;
begin
  inherited;
  FCellList.Clear;
end;

constructor TMt3dmsConcObsTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FCellList := TObjectList.Create;
  FSpeciesList := TList<Integer>.Create;
end;

destructor TMt3dmsConcObsTimeList.Destroy;
begin
  FSpeciesList.Free;
  FCellList.Free;
  inherited;
end;

function TMt3dmsConcObsTimeList.GetCellList(
  Index: integer): TMt3dmsTobsCellList;
begin
  result := FCellList[Index];
end;

procedure TMt3dmsConcObsTimeList.Initialize(
  ObservationValues: TMt3dmsTobCollection; ScreenObject: TObject;
  UseLgrEdgeCells: boolean; AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  Value: double;
  StoredUpToDate: boolean;
  CellList: TMt3dmsTobsCellList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Cell: TMt3dmsTob_Cell;
  Times: TRealList;
  DuplicateTimes: string;
  EarliestAllowedTime: double;
  LatestAllowedTime: double;
  EarlyTimes: string;
  LateTimes: string;
  ActiveDataSet: TDataArray;
  Weight: Double;
  ComponentIndex: Integer;
  TimeItem: TMt3dmsTobItem;
  ObsType: TObservationType;
  ObsFreq: Integer;
  ComponentIndexError: Boolean;
begin
  if UpToDate then
    Exit;

  FSpeciesList.Clear;

  LocalScreenObject := ScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  LocalModel := AModel as TCustomModel;
  EarliestAllowedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
  LatestAllowedTime := LocalModel.ModflowFullStressPeriods[
    LocalModel.ModflowFullStressPeriods.Count-1].EndTime;
  Assert(LocalModel <> nil);
  StoredUpToDate := LocalModel.UpToDate;
  Times := TRealList.Create;
  try
    DuplicateTimes := '';
    EarlyTimes := '';
    LateTimes := '';
    Times.Sorted := True;
    Clear;
    Grid := LocalModel.ModflowGrid;
    Assert(Grid <> nil);

    ComponentIndexError := False;
    for Index := 0 to ObservationValues.Count - 1 do
    begin
      CellList := TMt3dmsTobsCellList.Create;
      FCellList.Add(CellList);

      TimeItem := ObservationValues.TobItems[Index];
      Time := 0;
      ObsFreq := 0;
      ObsType := TimeItem.ObservationType;
      case ObsType of
        otTime:
          begin
            Time := TimeItem.Time;
            if Times.IndexOf(Time) >= 0 then
            begin
              DuplicateTimes := DuplicateTimes + ' ' + FloatToStr(Time);
//              Continue;
            end;
          end;
        otFrequency:
          begin
            ObsFreq := TimeItem.ObservationFrequency;
          end;
        else
          Assert(False)
      end;


      if Time < EarliestAllowedTime then
      begin
        EarlyTimes := EarlyTimes + ' ' + FloatToStr(Time);
        Continue;
      end;

      if Time > LatestAllowedTime then
      begin
        LateTimes := LateTimes + ' ' + FloatToStr(Time);
        Continue;
      end;

      if ObsType = otTime then
      begin
        Times.Add(Time);
      end;
      Value := ObservationValues.TobItems[Index].Concentration;
      Weight := ObservationValues.TobItems[Index].Weight;
      ComponentIndex := ObservationValues.TobItems[Index].ComponentIndex;

      if ComponentIndex < 0 then
      begin
        ComponentIndexError := True;
      end;

      DataArray := TRealSparseDataSet.Create(LocalModel);
      Add(Time, DataArray, ComponentIndex);
      DataArray.EvaluatedAt := eaBlocks;
      DataArray.Orientation := dso3D;
      DataArray.UpdateDimensions(LocalModel.LayerCount, LocalModel.RowCount,
        LocalModel.ColumnCount);

      ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
      ActiveDataSet.Initialize;

      LocalScreenObject.AssignNumericValueToDataSet({Grid,} DataArray, Value,
        Model);
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.IsValue[LayerIndex, RowIndex,ColIndex]
              and ActiveDataSet.BooleanData[LayerIndex, RowIndex,ColIndex] then
            begin
              Cell := TMt3dmsTob_Cell.Create;
              CellList.Add(Cell);
              Cell.Values.Cell.Layer := LayerIndex;
              Cell.Values.Cell.Row := RowIndex;
              Cell.Values.Cell.Column := ColIndex;
              Cell.Values.Concentration := DataArray.RealData[LayerIndex, RowIndex,ColIndex];
              Cell.Values.ConcentrationAnnotation := DataArray.Annotation[LayerIndex, RowIndex,ColIndex];
              Cell.Values.Weight := Weight;
              Cell.Values.ComponentIndex := ComponentIndex;
              Cell.Values.Time := Time;
              Cell.Values.ObsType := ObsType;
              Cell.Values.ObsFreq := ObsFreq;
            end;
          end;
        end;
      end;
    end;
    if DuplicateTimes <> '' then
    begin
      DuplicateTimes := Format(StrErrorObjectDuplicateTimes,
        [LocalScreenObject.Name, DuplicateTimes]);
      frmErrorsAndWarnings.AddWarning(Model, Mt3dTobErrorRoot, DuplicateTimes,
        LocalScreenObject);
    end;
    if EarlyTimes <> '' then
    begin
      EarlyTimes := Format(StrErrorObjectEarlyTimes,
        [LocalScreenObject.Name, EarlyTimes]);
      frmErrorsAndWarnings.AddWarning(Model, EarlyTimeWarning, EarlyTimes,
        LocalScreenObject);
    end;
    if LateTimes <> '' then
    begin
      LateTimes := Format(StrErrorObjectLateTimes,
        [LocalScreenObject.Name, LateTimes]);
      frmErrorsAndWarnings.AddWarning(Model, LateTimeWarning, LateTimes,
        LocalScreenObject);
    end;
    if ComponentIndexError then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInTheFollowingObj,
        LocalScreenObject.Name, LocalScreenObject);
    end;
  finally
    LocalModel.UpToDate := StoredUpToDate;
    Times.Free;
  end;
end;

procedure TMt3dmsConcObsTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value then
  begin
    if Assigned(OnInvalidate) then
    begin
      OnInvalidate(Self);
    end;
  end;
end;

end.
