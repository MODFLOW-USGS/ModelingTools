unit ModflowRipUnit;

interface

uses
  System.Classes, System.ZLib, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  System.Generics.Collections, OrderedCollectionUnit,
  GoPhastTypes, RbwParser, Modflow6DynamicTimeSeriesInterfaceUnit;

type
  TRipRecord = record
    Cell: TCellLocation;
    // HSURF
    LandElevation: double;
    // fCov
    Coverages: array of double;
    StartingTime: double;
    EndingTime: double;
    LandElevationAnnotation: string;
    CoverageAnnotations: array of string;

    LandElevationPest: string;
    LandElevationPestSeriesName: string;
    LandElevationPestSeriesMethod: TPestParamMethod;

    CoveragePests: array of string;
    CoveragePestSeriesNames: array of string;
    CoveragePestSeriesMethods: array of TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TRipArray = array of TRipRecord;

  TRipStorage = class(TCustomBoundaryStorage)
  private
    FRipArray: TRipArray;
    function GetRipArray: TRipArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RipArray: TRipArray read GetRipArray;
  end;

  TRipItem = class(TCustomModflowBoundaryItem)
  private
    FLandElevation: IFormulaObject;
    FCoverages: TList<IFormulaObject>;
    FCoverageFormulas: TStrings;
    FCoverageID: TIntegerCollection;
    function GetLandElevation: string;
    procedure SetLandElevation(const Value: string);
    function GetCoverages: TStrings;
    procedure SetCoverageID(const Value: TIntegerCollection);
    procedure SetCoverages(const Value: TStrings);
    procedure CoverageFormulaChanged(Sender: TObject);
    procedure AddRequiredObservers;
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
    function GetConductanceIndex: Integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure AddPlantGroup(PlantGroupID: Integer);
    procedure RemovePlantGroup(PlantGroupID: Integer);
  published
    property LandElevation: string read GetLandElevation write SetLandElevation;
    property Coverages: TStrings read GetCoverages write SetCoverages;
    property CoverageID: TIntegerCollection read FCoverageID write SetCoverageID;
  end;

  TRipTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Land Elevations for a series of
    // Riparean ET Boundaries over a series of time intervals.
    FLandElevation: TModflowTimeList;
    // @name is used to compute the coverages for a series of different plant
    // coverages in
    // Riparean ET Boundaries over a series of time intervals.
    FCoverages: TModflowTimeLists;
  protected
    procedure CreateTimeLists; override;
  public
    Constructor Create(AModel: TBaseModel; ABoundary: TCustomMF_BoundColl); override;
    Destructor Destroy; override;
  end;

  TRipCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateLandData(Sender: TObject);
    procedure InvalidateCoverageData(Sender: TObject);
    function GetItems(Index: Integer): TRipItem;
    procedure SetItems(Index: Integer; const Value: TRipItem);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRipStorage.RipArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); override;
  public
    procedure RemovePlantGroup(PlantGroupID: Integer);
    procedure AddPlantGroup(PlantGroupID: Integer);
    property Items[Index: Integer]: TRipItem read GetItems
      write SetItems; default;
  end;

  TRip_Cell = class(TValueCell)
  private
    Values: TRipRecord;
    StressPeriod: integer;
    function GetLandElevationAnnotation: string;
    function GetCoverage(Index: Integer): double;
    function GetCoverageAnnotation(Index: Integer): string;
    function GetLandElevation: double;
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
    property LandElevation: double read GetLandElevation;
    property Coverage[Index: Integer]: double read GetCoverage;
    property LandElevationAnnotation: string read GetLandElevationAnnotation;
    property CoverageAnnotation[Index: Integer]: string
      read GetCoverageAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TRipBoundary = class(TModflowBoundary)
  private
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRip_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TRipStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // The RIP package does not have parameters so ParamList is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    procedure RemovePlantGroup(PlantGroupID: Integer);
    procedure AddPlantGroup(PlantGroupID: Integer);
    function IsSame(AnRipBoundary: TRipBoundary): Boolean;
  end;


implementation

uses
  frmGoPhastUnit, ModflowRipPlantGroupsUnit, System.SysUtils, PhastModelUnit,
  SubscriptionUnit, ScreenObjectUnit, GIS_Functions, ModflowTimeUnit,
  CellLocationUnit;

resourcestring
  StrRipareanETLandElev = 'Riparean ET Land Elevation';
  StrCoverage = ' Coverage';

const
  LandElevationPosition = 0;

{ TRipRecord }

procedure TRipRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, LandElevation);
  WriteCompInt(Comp, Length(Coverages));
  for index := 0 to Length(Coverages) - 1 do
  begin
    WriteCompReal(Comp, Coverages[index]);
  end;
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(LandElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(LandElevationPest));
  WriteCompInt(Comp, Strings.IndexOf(LandElevationPestSeriesName));
  WriteCompInt(Comp, Ord(LandElevationPestSeriesMethod));
  for index := 0 to Length(Coverages) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(CoverageAnnotations[index]));
  end;
  for index := 0 to Length(Coverages) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(CoveragePests[index]));
  end;
  for index := 0 to Length(Coverages) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(CoveragePestSeriesNames[index]));
  end;
  for index := 0 to Length(Coverages) - 1 do
  begin
    WriteCompInt(Comp, Ord(CoveragePestSeriesMethods[index]));
  end;
end;

procedure TRipRecord.RecordStrings(Strings: TStringList);
var
  index: Integer;
begin
  Strings.Add(LandElevationAnnotation);
  Strings.Add(LandElevationPest);
  Strings.Add(LandElevationPestSeriesName);
  for index := 0 to Length(Coverages) - 1 do
  begin
    Strings.Add(CoverageAnnotations[index]);
  end;
  for index := 0 to Length(Coverages) - 1 do
  begin
    Strings.Add(CoveragePests[index]);
  end;
  for index := 0 to Length(Coverages) - 1 do
  begin
    Strings.Add(CoveragePestSeriesNames[index]);
  end;
end;

procedure TRipRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  ArrayLength: Integer;
  index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  LandElevation := ReadCompReal(Decomp);
  ArrayLength := ReadCompInt(Decomp);
  SetLength(Coverages, ArrayLength);

  for index := 0 to Length(Coverages) - 1 do
  begin
    Coverages[index] := ReadCompReal(Decomp);
  end;
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  LandElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  LandElevationPest := Annotations[ReadCompInt(Decomp)];
  LandElevationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  LandElevationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  SetLength(CoverageAnnotations, ArrayLength);
  SetLength(CoveragePests, ArrayLength);
  SetLength(CoveragePestSeriesNames, ArrayLength);
  SetLength(CoveragePestSeriesMethods, ArrayLength);
  for index := 0 to Length(CoverageAnnotations) - 1 do
  begin
    CoverageAnnotations[index] := Annotations[ReadCompInt(Decomp)];
  end;
  for index := 0 to Length(CoveragePests) - 1 do
  begin
    CoveragePests[index] := Annotations[ReadCompInt(Decomp)];
  end;
  for index := 0 to Length(CoveragePestSeriesNames) - 1 do
  begin
    CoveragePestSeriesNames[index] := Annotations[ReadCompInt(Decomp)];
  end;
  for index := 0 to Length(CoveragePestSeriesMethods) - 1 do
  begin
    CoveragePestSeriesMethods[index] := TPestParamMethod(ReadCompInt(Decomp));
  end;
end;

{ TRipStorage }

procedure TRipStorage.Clear;
begin
  SetLength(FRipArray, 0);
  FCleared := True;
end;

function TRipStorage.GetRipArray: TRipArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRipArray;
end;

procedure TRipStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRipArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRipArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TRipStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRipArray);
    for Index := 0 to Count - 1 do
    begin
      FRipArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRipArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TRipItem }

procedure TRipItem.AddPlantGroup(PlantGroupID: Integer);
var
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  Assert(FCoverageID.IndexOf(PlantGroupID) < 0);
  FCoverageID.Add.Value := PlantGroupID;
  FCoverages.Add(CreateFormulaObject(dsoTop));

  Observer := TObserver.Create(nil);
  FObserverList.Add(Observer);
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(Observer);
  end;
  BoundaryFormula[Index] := '0';

  AssignObserverEvents(Collection);

  FCoverageFormulas.Add('0');
end;

procedure TRipItem.Assign(Source: TPersistent);
var
  RipItem: TRipItem;
begin
  if Source is TRipItem then
  begin
    RipItem := TRipItem(Source);
    LandElevation := RipItem.LandElevation;
    Coverages := RipItem.Coverages;
    CoverageID := RipItem.CoverageID;
  end;
  inherited;

end;

procedure TRipItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRipCollection;
  LandElevationObserver: TObserver;
  AnObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TRipCollection;
  LandElevationObserver := FObserverList[LandElevationPosition];
  LandElevationObserver.OnUpToDateSet := ParentCollection.InvalidateLandData;
  for Index := 1 to FObserverList.Count - 1 do
  begin
    AnObserver := FObserverList[Index];
    AnObserver.OnUpToDateSet := ParentCollection.InvalidateCoverageData;
  end;
end;

function TRipItem.BoundaryFormulaCount: integer;
begin
  result := 1 + frmGoPhast.PhastModel.RipPlantGroups.Count;
end;

procedure TRipItem.CoverageFormulaChanged(Sender: TObject);
var
  Index: integer;
  AFormulaObject: IFormulaObject;
begin
  if Model = nil then
  begin
    Exit;
  end;
//  AddRequiredObservers;
//  if FObserverList.Count < BoundaryFormulaCount then
//  begin
//    while FObserverList.Count < BoundaryFormulaCount do
//    begin
//      Observer := TObserver.Create(nil);
//      FObserverList.Add(Observer);
//      LocalScreenObject := ScreenObject as TScreenObject;
//      if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
//      begin
//        LocalScreenObject.TalksTo(Observer);
//      end;
//    end;
//    AssignObserverEvents(Collection);
//  end;
  while FCoverageFormulas.Count > FCoverages.Count do
  begin
    FCoverages.Add(CreateFormulaObject(dsoTop));
  end;
  AddRequiredObservers;
  while FCoverageFormulas.Count < FCoverages.Count do
  begin
    AFormulaObject := FCoverages[FCoverages.Count-1];
    FCoverages.Delete(FCoverages.Count-1);
    frmGoPhast.PhastModel.FormulaManager.Remove(AFormulaObject,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
  for Index := 0 to FCoverages.Count - 1 do
  begin
    AFormulaObject := FCoverages[Index];
    UpdateFormulaBlocks(FCoverageFormulas[Index], Index+1, AFormulaObject);
    FCoverages[Index] := AFormulaObject;
  end;
end;

constructor TRipItem.Create(Collection: TCollection);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := (Model as TCustomModel).DoInvalidate;
  end;
  FCoverages := TList<IFormulaObject>.Create;
  FCoverageFormulas := TStringList.Create;
  (FCoverageFormulas as TStringList).OnChange := CoverageFormulaChanged;
  FCoverageID := TIntegerCollection.Create(InvalidateModelEvent);
  inherited;
end;

procedure TRipItem.CreateFormulaObjects;
var
  Index: Integer;
begin
  inherited;
  FLandElevation := CreateFormulaObject(dsoTop);
  Assert(FCoverages.Count = 0);
  for Index := 0 to frmGoPhast.PhastModel.RipPlantGroups.Count - 1 do
  begin
    FCoverages.Add(CreateFormulaObject(dsoTop));
  end;
end;

destructor TRipItem.Destroy;
begin
  inherited;
  FCoverages.Free;
  FCoverageFormulas.Free;
  FCoverageID.Free;
end;

function TRipItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    LandElevationPosition:
      result := LandElevation;
    else
      begin
        Result := FCoverages[Index-1].Formula;
        ResetItemObserver(Index);
      end;
  end;
end;

function TRipItem.GetConductanceIndex: Integer;
begin
  Result := -1;
end;

function TRipItem.GetCoverages: TStrings;
begin
  if Model <> nil then
  begin
    AddRequiredObservers;
    while FCoverageFormulas.Count <
      frmGophast.PhastModel.RipPlantGroups.Count do
    begin
      FCoverageID.Add.Value :=
        frmGophast.PhastModel.RipPlantGroups[FCoverageFormulas.Count].ID;
      FCoverageFormulas.Add('0');
    end;

    FCoverageFormulas.Capacity := FCoverageFormulas.Count;
  end;
  result := FCoverageFormulas;
end;

function TRipItem.GetLandElevation: string;
begin
  Result := FLandElevation.Formula;
  ResetItemObserver(LandElevationPosition);
end;

procedure TRipItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Position: Integer;
begin
  if Sender = FLandElevation as TObject then
  begin
    List.Add(FObserverList[LandElevationPosition]);
  end
  else
  begin
    Position := FCoverages.IndexOf(Sender as TFormulaObject);
    if Position >= 0 then
    begin
      List.Add(FObserverList[Position + 1]);
    end;
  end;
end;

procedure TRipItem.InvalidateModel;
begin
  inherited;

end;

function TRipItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRipItem;
  FormulaIndex: Integer;
begin
  result := (AnotherItem is TRipItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRipItem(AnotherItem);
    result := (Item.FCoverages.Count = FCoverages.Count)
      and (Item.LandElevation = LandElevation);
    if result then
    begin
      for FormulaIndex := 0 to FCoverages.Count - 1 do
      begin
        Result := Item.FCoverages[FormulaIndex].Formula =
          FCoverages[FormulaIndex].Formula;
        if not Result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TRipItem.RemoveFormulaObjects;
var
  CoverageIndex: Integer;
  FormulaManager: TFormulaManager;
  ACoverage: IFormulaObject;
begin
  FormulaManager := frmGoPhast.PhastModel.FormulaManager;
  FormulaManager.Remove(FLandElevation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  for CoverageIndex := 0 to FCoverageFormulas.Count - 1 do
  begin
    ACoverage := FCoverages[CoverageIndex];
    FormulaManager.Remove(ACoverage,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

procedure TRipItem.RemovePlantGroup(PlantGroupID: Integer);
var
  CoverageIndex: Integer;
  ACoverage: IFormulaObject;
  LocalScreenObject: TScreenObject;
  Observer: TObserver;
begin
  CoverageIndex := FCoverageID.IndexOf(PlantGroupID);
  if CoverageIndex >= 0 then
  begin
    ACoverage := FCoverages[CoverageIndex];
    frmGoPhast.PhastModel.FormulaManager.Remove(ACoverage,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
    FCoverageFormulas.Delete(CoverageIndex);
    FCoverageID.Delete(CoverageIndex);
    Observer := FObserverList[CoverageIndex];
    LocalScreenObject := ScreenObject as TScreenObject;
    if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
    begin
      LocalScreenObject.StopsTalkingTo(Observer);
    end;
    Observer.StopTalkingToAnyone;
    FObserverList.Delete(CoverageIndex);
  end;
end;

procedure TRipItem.AddRequiredObservers;
var
  LocalScreenObject: TScreenObject;
  Observer: TObserver;
begin
  if FObserverList.Count < BoundaryFormulaCount then
  begin
    while FObserverList.Count < BoundaryFormulaCount do
    begin
      Observer := TObserver.Create(nil);
      FObserverList.Add(Observer);
      LocalScreenObject := ScreenObject as TScreenObject;
      if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
      begin
        LocalScreenObject.TalksTo(Observer);
      end;
    end;
    AssignObserverEvents(Collection);
  end;
end;

procedure TRipItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  FormulaObject: IFormulaObject;
begin
  case Index of
    LandElevationPosition:
      LandElevation := Value;
    else
      begin
        FormulaObject := FCoverages[Index-1];
        UpdateFormulaBlocks(Value, Index-1, FormulaObject);
        FCoverages[Index-1] := FormulaObject;
      end;
  end;
end;

procedure TRipItem.SetCoverageID(const Value: TIntegerCollection);
begin
  FCoverageID.Assign(Value);
end;

procedure TRipItem.SetCoverages(const Value: TStrings);
var
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  if Model <> nil then
  begin
    While FObserverList.Count < Value.Count do
    begin
      Observer := TObserver.Create(nil);
      FObserverList.Add(Observer);
      LocalScreenObject := ScreenObject as TScreenObject;
      if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
      begin
        LocalScreenObject.TalksTo(Observer);
      end;
      BoundaryFormula[Index] := '0';
    end;
    AssignObserverEvents(Collection);
  end;
  FCoverageFormulas.Assign(Value);
end;

procedure TRipItem.SetLandElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, LandElevationPosition, FLandElevation);
end;

{ TRipTimeListLink }

constructor TRipTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  FCoverages := TModflowTimeLists.Create;
  inherited;
end;

procedure TRipTimeListLink.CreateTimeLists;
var
  PlantGrouopIndex: Integer;
  RipPlantGroups: TRipPlantGroups;
  APlantGroup: TRipPlantGroup;
  ATimeList: TModflowTimeList;
  LocalModel: TCustomModel;
begin
  inherited;
  FLandElevation := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLandElevation.NonParamDescription := StrRipareanETLandElev;
  FLandElevation.ParamDescription := ' ' + LowerCase(StrRipareanETLandElev);

  LocalModel := nil;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FLandElevation.OnInvalidate := LocalModel.InvalidateRipGroundSurface;
  end;

  AddTimeList(FLandElevation);

  RipPlantGroups := frmGoPhast.PhastModel.RipPlantGroups;
  for PlantGrouopIndex := 0 to RipPlantGroups.Count - 1 do
  begin
    APlantGroup := RipPlantGroups[PlantGrouopIndex];
    ATimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    ATimeList.NonParamDescription := APlantGroup.Name + StrCoverage;
    ATimeList.ParamDescription := LowerCase(APlantGroup.Name + StrCoverage);
    FCoverages.Add(ATimeList);
    if Model <> nil then
    begin
      ATimeList.OnInvalidate := LocalModel.InvalidateRipCoverages;
    end;
    AddTimeList(ATimeList);
  end;
end;

destructor TRipTimeListLink.Destroy;
begin
  FLandElevation.Free;
  FCoverages.Free;
  inherited;
end;

{ TRipCollection }

procedure TRipCollection.AddPlantGroup(PlantGroupID: Integer);
var
  index: Integer;
  RipItem: TRipItem;
begin
  for index := 0 to Count - 1 do
  begin
    RipItem := Items[index];
    RipItem.AddPlantGroup(PlantGroupID);
  end;
end;

procedure TRipCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TRipStorage.Create(AModel));
end;

function TRipCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TRipItem;
begin
  Item := Items[ItemIndex] as TRipItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TRipCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  RipStorage: TRipStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  PlantGroupCount: Integer;
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

  PlantGroupCount := frmGoPhast.PhastModel.RipPlantGroups.Count;
  Assert(BoundaryFunctionIndex in [LandElevationPosition..PlantGroupCount]);
  Assert(Expression <> nil);

  RipStorage := BoundaryStorage as TRipStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with RipStorage.RipArray[Index] do
    begin
      case BoundaryFunctionIndex of
        LandElevationPosition:
          begin
            LandElevation := Expression.DoubleResult;
            LandElevationAnnotation := ACell.Annotation;
            LandElevationPest := PestName;
            LandElevationPestSeriesName := PestSeriesName;
            LandElevationPestSeriesMethod := PestSeriesMethod;
          end;
        else
          begin
            Coverages[BoundaryFunctionIndex-1] := Expression.DoubleResult;
            CoverageAnnotations[BoundaryFunctionIndex-1] := ACell.Annotation;
            CoveragePests[BoundaryFunctionIndex-1] := PestName;
            CoveragePestSeriesNames[BoundaryFunctionIndex-1] := PestSeriesName;
            CoveragePestSeriesMethods[BoundaryFunctionIndex-1] := PestSeriesMethod;
          end;
      end;
    end;
  end;
end;

procedure TRipCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  RipStorage: TRipStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  RipStorage := BoundaryStorage as TRipStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with RipStorage.RipArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TRipCollection.GetItems(Index: Integer): TRipItem;
begin
  result := inherited Items[Index] as TRipItem;
end;

class function TRipCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TRipTimeListLink;
end;

procedure TRipCollection.InvalidateCoverageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRipTimeListLink;
  ChildIndex: Integer;
  CoverageIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TRipTimeListLink;
    for CoverageIndex := 0 to Link.FCoverages.Count - 1 do
    begin
      Link.FCoverages[CoverageIndex].Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TRipTimeListLink;
      end;
    for CoverageIndex := 0 to Link.FCoverages.Count - 1 do
    begin
      Link.FCoverages[CoverageIndex].Invalidate;
    end;
    end;
  end;
end;

procedure TRipCollection.InvalidateLandData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRipTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRipTimeListLink;
    Link.FLandElevation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TRipTimeListLink;
        Link.FLandElevation.Invalidate;
      end;
    end;
  end;
end;

class function TRipCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRipItem;
end;

procedure TRipCollection.RemovePlantGroup(PlantGroupID: Integer);
var
  index: Integer;
  RipItem: TRipItem;
begin
  for index := 0 to Count - 1 do
  begin
    RipItem := Items[index];
    RipItem.RemovePlantGroup(PlantGroupID);
  end;
end;

procedure TRipCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  Index: Integer;
  PlantGroupCount: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).FRipArray,
    BoundaryCount);
  PlantGroupCount := frmGoPhast.PhastModel.RipPlantGroups.Count;
  for Index := 0 to BoundaryCount-1 do
  begin
    SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).
      FRipArray[Index].Coverages, PlantGroupCount);
    SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).
      FRipArray[Index].CoverageAnnotations, PlantGroupCount);
    SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).
      FRipArray[Index].CoveragePests, PlantGroupCount);
    SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).
      FRipArray[Index].CoveragePestSeriesNames, PlantGroupCount);
    SetLength((Boundaries[ItemIndex, AModel] as TRipStorage).
      FRipArray[Index].CoveragePestSeriesMethods, PlantGroupCount);
  end;
  inherited;
end;

procedure TRipCollection.SetItems(Index: Integer; const Value: TRipItem);
begin
  inherited Items[Index] := Value;
end;

procedure TRipCollection.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  Boundary: TRipBoundary;
begin
  Boundary := BoundaryGroup as TRipBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

{ TRip_Cell }

procedure TRip_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRip_Cell.GetLandElevationAnnotation: string;
begin
  result := Values.LandElevationAnnotation;
end;

function TRip_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRip_Cell.GetCoverage(Index: Integer): double;
begin
  result := Values.Coverages[Index];
end;

function TRip_Cell.GetCoverageAnnotation(Index: Integer): string;
begin
  result := Values.CoverageAnnotations[Index];
end;

function TRip_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TRip_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TRip_Cell.GetLandElevation: double;
begin
  result := Values.LandElevation;
end;

function TRip_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRip_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    LandElevationPosition: result := LandElevationAnnotation;
    else result := CoverageAnnotation[Index-1];
  end;
end;

function TRip_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  case Index of
    LandElevationPosition: result := LandElevation;
    else
      begin
        result := Coverage[Index-1];
      end;
  end;
end;

function TRip_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRip_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TRip_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  RIP_Cell: TRip_Cell;
  index: integer;
begin
  result := AnotherCell is TRip_Cell;
  if result then
  begin
    RIP_Cell := TRip_Cell(AnotherCell);
    result := (LandElevation = RIP_Cell.LandElevation)
      and (Length(Values.Coverages) = Length(RIP_Cell.Values.Coverages));
    if result then
    begin
      for index := 0 to Length(Values.Coverages) - 1 do
      begin
        result := (Coverage[index] =  Values.Coverages[index]);
        if not Result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TRip_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRip_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRip_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TRip_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TRip_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TRipBoundary }

procedure TRipBoundary.AddPlantGroup(PlantGroupID: Integer);
begin
  (Values as TRipCollection).AddPlantGroup(PlantGroupID);
end;

procedure TRipBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRip_Cell;
  BoundaryValues: TRipRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRipStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TRipStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRip_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon
      >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon
      <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Length(LocalBoundaryStorage.RipArray) then
      begin
        Cells.Capacity := Length(LocalBoundaryStorage.RipArray);
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RipArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RipArray[BoundaryIndex];
        Cell := TRip_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        SetLength(Cell.Values.Coverages, Length(Cell.Values.Coverages));
        SetLength(Cell.Values.CoverageAnnotations, Length(Cell.Values.Coverages));
        SetLength(Cell.Values.CoveragePests, Length(Cell.Values.Coverages));
        SetLength(Cell.Values.CoveragePestSeriesNames, Length(Cell.Values.Coverages));
        SetLength(Cell.Values.CoveragePestSeriesMethods, Length(Cell.Values.Coverages));
        Cell.ScreenObject := ScreenObjectI;
        LocalModel.AdjustCellPosition(Cell);


      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TRipBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TRipCollection;
end;

procedure TRipBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TRipStorage;
  ValueCount: Integer;
begin
  EvaluateListBoundaries(AModel);
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TRipStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
  end;
end;

procedure TRipBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateRipGroundSurface(self);
    Model.InvalidateRipCoverages(self);
  end;
end;

function TRipBoundary.IsSame(AnRipBoundary: TRipBoundary): Boolean;
begin
  result := (Values as TRipCollection).IsSame(
    AnRipBoundary.Values as TRipCollection);
end;

procedure TRipBoundary.RemovePlantGroup(PlantGroupID: Integer);
begin
  (Values as TRipCollection).RemovePlantGroup(PlantGroupID);
end;

procedure TRipBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
begin
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  ObservationsPresent := False;
end;

end.
