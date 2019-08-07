unit ModflowUzfMf6Unit;

interface

uses Windows, ZLib, SysUtils, ModflowCellUnit, System.Classes,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, GoPhastTypes,
  SubscriptionUnit;

type
  TUzfOb = (uoGW_Recharge, uoGW_Discharge, uoDischargeToMvr,
    uoSatZoneEvapotranspiration, uoInfiltration, uoMvrInflow,
    uoRejectInfiltration, uoRejectInfiltrationToMvr,
    uoUnsatZoneEvapotranspiration, uoStorage, uoNetInfiltration, uoWaterContent);
  TUzfObs = set of TUzfOb;

  TUzfMf6Record = record
    Cell: TCellLocation;
    StartingTime: double;
    EndingTime: double;

    Infiltration: double;
    PotentialET: double;
    ExtinctionDepth: double;
    ExtinctionWaterContent: double;
    AirEntryPotential: double;
    RootPotential: double;
    RootActivity: double;

    InfiltrationAnnotation: string;
    PotentialETAnnotation: string;
    ExtinctionDepthAnnotation: string;
    ExtinctionWaterContentAnnotation: string;
    AirEntryPotentialAnnotation: string;
    RootPotentialAnnotation: string;
    RootActivityAnnotation: string;

    MvrUsed: Boolean;
    MvrIndex: Integer;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TUzfMf6Array = array of TUzfMf6Record;

  TUzfMf6Storage = class(TCustomBoundaryStorage)
  private
    FUzfMf6Array: TUzfMf6Array;
    function GetUzfMf6Array: TUzfMf6Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property UzfMf6Array: TUzfMf6Array read GetUzfMf6Array;
  end;

  TUzfMf6Item = class(TCustomModflowBoundaryItem)
  private
    // See @link(UzfExtinctDepth).
    FInfiltration: TFormulaObject;
    FPotentialET: TFormulaObject;
    FUzfExtinctDepth: TFormulaObject;
    FExtinctionWaterContent: TFormulaObject;
    FAirEntryPotential: TFormulaObject;
    FRootPotential: TFormulaObject;
    FRootActivity: TFormulaObject;
    // See @link(UzfExtinctDepth).
    function GetAirEntryPotential: string;
    function GetExtinctionDepth: string;
    function GetExtinctionWaterContent: string;
    function GetInfiltration: string;
    function GetPotentialET: string;
    function GetRootActivity: string;
    function GetRootPotential: string;
    procedure SetAirEntryPotential(const Value: string);
    procedure SetExtinctionDepth(const Value: string);
    procedure SetExtinctionWaterContent(const Value: string);
    procedure SetInfiltration(const Value: string);
    procedure SetPotentialET(const Value: string);
    procedure SetRootActivity(const Value: string);
    procedure SetRootPotential(const Value: string);
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
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    // finf (always used)
    property Infiltration: string read GetInfiltration write SetInfiltration;
    // pet (Use only if SIMULATE_ET has been specified.)
    property PotentialET: string read GetPotentialET write SetPotentialET;
    // extdp (Use only if SIMULATE_ET has been specified.)
    property ExtinctionDepth: string read GetExtinctionDepth write SetExtinctionDepth;
    // extwc (Use only if SIMULATE_ET and UNSAT_ETWC have been specified.)
    property ExtinctionWaterContent: string read GetExtinctionWaterContent write SetExtinctionWaterContent;
    // ha (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property AirEntryPotential: string read GetAirEntryPotential write SetAirEntryPotential;
    // hroot (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property RootPotential: string read GetRootPotential write SetRootPotential;
    // rootact (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property RootActivity: string read GetRootActivity write SetRootActivity;
  end;

  TUzfMf6TimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the extinction depths for a series of
    // cells over a series of time intervals.
    FInfiltrationData: TModflowTimeList;
    FPotentialETData: TModflowTimeList;
    FExtinctionDepthData: TModflowTimeList;
    FExtinctionWaterContentData: TModflowTimeList;
    FAirEntryPotentialData: TModflowTimeList;
    FRootPotentialData: TModflowTimeList;
    FRootActivityData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TUzfMf6Collection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateUzfInfiltrationData(Sender: TObject);
    procedure InvalidateUzfPotentialETData(Sender: TObject);
    procedure InvalidateUzfExtinctionDepthData(Sender: TObject);
    procedure InvalidateUzfExtinctionWaterContentData(Sender: TObject);
    procedure InvalidateUzfAirEntryPotentialData(Sender: TObject);
    procedure InvalidateUzfRootPotentialData(Sender: TObject);
    procedure InvalidateUzfRootActivityData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TUzfMf6_Cell = class(TValueCell)
  private
    FValues: TUzfMf6Record;
    FStressPeriod: integer;
    function GetInfiltration: double;
    function GetPotentialET: double;
    function GetExtinctionDepth: double;
    function GetExtinctionWaterContent: double;
    function GetAirEntryPotential: double;
    function GetRootPotential: double;
    function GetRootActivity: double;
    function GetInfiltrationAnnotation: string;
    function GetPotentialETAnnotation: string;
    function GetExtinctionDepthAnnotation: string;
    function GetExtinctionWaterContentAnnotation: string;
    function GetAirEntryPotentialAnnotation: string;
    function GetRootPotentialAnnotation: string;
    function GetRootActivityAnnotation: string;
    function GetMvrIndex: Integer;
    function GetMvrUsed: Boolean;
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
    property Infiltration: double read GetInfiltration;
    property PotentialET: double read GetPotentialET;
    property ExtinctionDepth: double read GetExtinctionDepth;
    property ExtinctionWaterContent: double read GetExtinctionWaterContent;
    property AirEntryPotential: double read GetAirEntryPotential;
    property RootPotential: double read GetRootPotential;
    property RootActivity: double read GetRootActivity;
    property InfiltrationAnnotation: string read GetInfiltrationAnnotation;
    property PotentialETAnnotation: string read GetPotentialETAnnotation;
    property ExtinctionDepthAnnotation: string read GetExtinctionDepthAnnotation;
    property ExtinctionWaterContentAnnotation: string read GetExtinctionWaterContentAnnotation;
    property AirEntryPotentialAnnotation: string read GetAirEntryPotentialAnnotation;
    property RootPotentialAnnotation: string read GetRootPotentialAnnotation;
    property RootActivityAnnotation: string read GetRootActivityAnnotation;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TUzfMf6Boundary = class(TModflowBoundary)
  private

    FSurfaceDepressionDepth: TFormulaObject;
    FVerticalSaturatedK: TFormulaObject;
    FResidualWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FInitialWaterContent: TFormulaObject;
    FBrooksCoreyEpsilon: TFormulaObject;

    FSurfaceDepressionDepthObserver: TObserver;
    FVerticalSaturatedKObserver: TObserver;
    FResidualWaterContentObserver: TObserver;
    FSaturatedWaterContentObserver: TObserver;
    FInitialWaterContentObserver: TObserver;
    FBrooksCoreyEpsilonObserver: TObserver;

    function GetBrooksCoreyEpsilon: string;
    function GetInitialWaterContent: string;
    function GetResidualWaterContent: string;
    function GetSaturatedWaterContent: string;
    function GetSurfaceDepressionDepth: string;
    function GetVerticalSaturatedK: string;
    procedure SetBrooksCoreyEpsilon(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetResidualWaterContent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetSurfaceDepressionDepth(const Value: string);
    procedure SetVerticalSaturatedK(const Value: string);
    function GetBrooksCoreyEpsilonObserver: TObserver;
    function GetInitialWaterContentObserver: TObserver;
    function GetResidualWaterContentObserver: TObserver;
    function GetSaturatedWaterContentObserver: TObserver;
    function GetSurfaceDepressionDepthObserver: TObserver;
    function GetVerticalSaturatedKObserver: TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TUzfMf6_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;

    property SurfaceDepressionDepthObserver: TObserver read GetSurfaceDepressionDepthObserver;
    property VerticalSaturatedKObserver: TObserver read GetVerticalSaturatedKObserver;
    property ResidualWaterContentObserver: TObserver read GetResidualWaterContentObserver;
    property SaturatedWaterContentObserver: TObserver read GetSaturatedWaterContentObserver;
    property InitialWaterContentObserver: TObserver read GetInitialWaterContentObserver;
    property BrooksCoreyEpsilonObserver: TObserver read GetBrooksCoreyEpsilonObserver;

  public
    procedure Assign(Source: TPersistent);override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TUzfMf6Storage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    // surfdep
    property SurfaceDepressionDepth: string read GetSurfaceDepressionDepth
      write SetSurfaceDepressionDepth;
    // vks
    property VerticalSaturatedK: string read GetVerticalSaturatedK
      write SetVerticalSaturatedK;
    // thtr
    property ResidualWaterContent: string read GetResidualWaterContent
      write SetResidualWaterContent;
    // thts
    property SaturatedWaterContent: string read GetSaturatedWaterContent
      write SetSaturatedWaterContent;
    // thti
    property InitialWaterContent: string read GetInitialWaterContent
      write SetInitialWaterContent;
    // eps
    property BrooksCoreyEpsilon: string read GetBrooksCoreyEpsilon
      write SetBrooksCoreyEpsilon;
  end;

const
  UzfMf6InfiltrationPosition = 0;
  UzfMf6PotentialETPosition = 1;
  UzfMf6ExtinctionDepthPosition = 2;
  UzfMf6ExtinctionWaterContentPosition = 3;
  UzfMf6AirEntryPotentialPosition = 4;
  UzfMf6RootPotentialPosition = 5;
  UzfMf6RootActivityPosition = 6;


implementation

uses
  frmGoPhastUnit, PhastModelUnit, DataSetUnit,
  ScreenObjectUnit, ModflowTimeUnit;

resourcestring
  StrUZFInfiltrationDat = 'UZF infiltration';
  StrUZFPotentialET = 'UZF potential ET';
  StrUZFExtinctionDepth = 'UZF extinction depth';
  StrUZFExtinctionWater = 'UZF extinction water content';
  StrUZFAirEntryPotent = 'UZF air entry potential';
  StrUZFRootPotential = 'UZF root potential';
  StrUZFRootActivity = 'UZF root activity';

const
  SurfaceDepressionDepthPosition = 0;
  VerticalSaturatedKPosition = 1;
  ResidualWaterContentPosition = 2;
  SaturatedWaterContentPosition = 3;
  InitialWaterContentPosition = 4;
  BrooksCoreyEpsilonPosition = 5;

{ TUzfMf6Record }

procedure TUzfMf6Record.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompReal(Comp, Infiltration);
  WriteCompReal(Comp, PotentialET);
  WriteCompReal(Comp, ExtinctionDepth);
  WriteCompReal(Comp, ExtinctionWaterContent);
  WriteCompReal(Comp, AirEntryPotential);
  WriteCompReal(Comp, RootPotential);
  WriteCompReal(Comp, RootActivity);

  WriteCompInt(Comp, Strings.IndexOf(InfiltrationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PotentialETAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(AirEntryPotentialAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RootPotentialAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RootActivityAnnotation));

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);

end;

procedure TUzfMf6Record.RecordStrings(Strings: TStringList);
begin
  Strings.Add(InfiltrationAnnotation);
  Strings.Add(PotentialETAnnotation);
  Strings.Add(ExtinctionDepthAnnotation);
  Strings.Add(ExtinctionWaterContentAnnotation);
  Strings.Add(AirEntryPotentialAnnotation);
  Strings.Add(RootPotentialAnnotation);
  Strings.Add(RootActivityAnnotation);
end;

procedure TUzfMf6Record.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  Infiltration := ReadCompReal(Decomp);
  PotentialET := ReadCompReal(Decomp);
  ExtinctionDepth := ReadCompReal(Decomp);
  ExtinctionWaterContent := ReadCompReal(Decomp);
  AirEntryPotential := ReadCompReal(Decomp);
  RootPotential := ReadCompReal(Decomp);
  RootActivity := ReadCompReal(Decomp);

  InfiltrationAnnotation := Annotations[ReadCompInt(Decomp)];
  PotentialETAnnotation := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthAnnotation := Annotations[ReadCompInt(Decomp)];
  ExtinctionWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  AirEntryPotentialAnnotation := Annotations[ReadCompInt(Decomp)];
  RootPotentialAnnotation := Annotations[ReadCompInt(Decomp)];
  RootActivityAnnotation := Annotations[ReadCompInt(Decomp)];

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);

end;

{ TUzfMf6Storage }

procedure TUzfMf6Storage.Clear;
begin
  SetLength(FUzfMf6Array, 0);
  FCleared := True;
end;

function TUzfMf6Storage.GetUzfMf6Array: TUzfMf6Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FUzfMf6Array;
end;

procedure TUzfMf6Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FUzfMf6Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FUzfMf6Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TUzfMf6Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin

  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FUzfMf6Array);
    for Index := 0 to Count - 1 do
    begin
      FUzfMf6Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FUzfMf6Array[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

{ TUzfMf6Item }

procedure TUzfMf6Item.Assign(Source: TPersistent);
var
  UzfMf6Item: TUzfMf6Item;
begin
  if Source is TUzfMf6Item then
  begin
    UzfMf6Item := TUzfMf6Item(Source);
    Infiltration := UzfMf6Item.Infiltration;
    PotentialET := UzfMf6Item.PotentialET;
    ExtinctionDepth := UzfMf6Item.ExtinctionDepth;
    ExtinctionWaterContent := UzfMf6Item.ExtinctionWaterContent;
    AirEntryPotential := UzfMf6Item.AirEntryPotential;
    RootPotential := UzfMf6Item.RootPotential;
    RootActivity := UzfMf6Item.RootActivity;
  end;
  inherited;
end;

procedure TUzfMf6Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TUzfMf6Collection;
  Observer: TObserver;
begin
  ParentCollection := Collection as TUzfMf6Collection;

  Observer := FObserverList[UzfMf6InfiltrationPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfInfiltrationData;

  Observer := FObserverList[UzfMf6PotentialETPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfPotentialETData;

  Observer := FObserverList[UzfMf6ExtinctionDepthPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfExtinctionDepthData;

  Observer := FObserverList[UzfMf6ExtinctionWaterContentPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfExtinctionWaterContentData;

  Observer := FObserverList[UzfMf6AirEntryPotentialPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfAirEntryPotentialData;

  Observer := FObserverList[UzfMf6RootPotentialPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfRootPotentialData;

  Observer := FObserverList[UzfMf6RootActivityPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfRootActivityData;
end;

function TUzfMf6Item.BoundaryFormulaCount: integer;
begin
  result := 7;
end;

procedure TUzfMf6Item.CreateFormulaObjects;
begin
  inherited;
  FInfiltration := CreateFormulaObject(dso3D);
  FPotentialET := CreateFormulaObject(dso3D);
  FUzfExtinctDepth := CreateFormulaObject(dso3D);
  FExtinctionWaterContent := CreateFormulaObject(dso3D);
  FAirEntryPotential := CreateFormulaObject(dso3D);
  FRootPotential := CreateFormulaObject(dso3D);
  FRootActivity := CreateFormulaObject(dso3D);
end;

destructor TUzfMf6Item.Destroy;
begin
  Infiltration := '0';
  PotentialET := '0';
  ExtinctionDepth := '0';
  ExtinctionWaterContent := '0';
  AirEntryPotential := '0';
  RootPotential := '0';
  RootActivity := '0';
  inherited;
end;

function TUzfMf6Item.GetAirEntryPotential: string;
begin
  Result := FAirEntryPotential.Formula;
  ResetItemObserver(UzfMf6AirEntryPotentialPosition);
end;

function TUzfMf6Item.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UzfMf6InfiltrationPosition: result := Infiltration;
    UzfMf6PotentialETPosition: result := PotentialET;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepth;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContent;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotential;
    UzfMf6RootPotentialPosition: result := RootPotential;
    UzfMf6RootActivityPosition: result := RootActivity;
    else Assert(False);
  end;
end;

function TUzfMf6Item.GetExtinctionDepth: string;
begin
  Result := FUzfExtinctDepth.Formula;
  ResetItemObserver(UzfMf6ExtinctionDepthPosition);
end;

function TUzfMf6Item.GetExtinctionWaterContent: string;
begin
  Result := FExtinctionWaterContent.Formula;
  ResetItemObserver(UzfMf6ExtinctionWaterContentPosition);
end;

function TUzfMf6Item.GetInfiltration: string;
begin
  Result := FInfiltration.Formula;
  ResetItemObserver(UzfMf6InfiltrationPosition);
end;

function TUzfMf6Item.GetPotentialET: string;
begin
  Result := FPotentialET.Formula;
  ResetItemObserver(UzfMf6PotentialETPosition);
end;

procedure TUzfMf6Item.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if (Sender = FInfiltration) then
  begin
    List.Add( FObserverList[UzfMf6InfiltrationPosition]);
  end
  else if (Sender = FPotentialET) then
  begin
    List.Add( FObserverList[UzfMf6PotentialETPosition]);
  end
  else if (Sender = FUzfExtinctDepth) then
  begin
    List.Add( FObserverList[UzfMf6ExtinctionDepthPosition]);
  end
  else if (Sender = FExtinctionWaterContent) then
  begin
    List.Add( FObserverList[UzfMf6ExtinctionWaterContentPosition]);
  end
  else if (Sender = FAirEntryPotential) then
  begin
    List.Add( FObserverList[UzfMf6AirEntryPotentialPosition]);
  end
  else if (Sender = FRootPotential) then
  begin
    List.Add( FObserverList[UzfMf6RootPotentialPosition]);
  end
  else if (Sender = FRootActivity) then
  begin
    List.Add( FObserverList[UzfMf6RootActivityPosition]);
  end
  else
  begin
    Assert(False);
  end;
end;

function TUzfMf6Item.GetRootActivity: string;
begin
  Result := FRootActivity.Formula;
  ResetItemObserver(UzfMf6RootActivityPosition);
end;

function TUzfMf6Item.GetRootPotential: string;
begin
  Result := FRootPotential.Formula;
  ResetItemObserver(UzfMf6RootPotentialPosition);
end;

function TUzfMf6Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TUzfMf6Item;
begin
  result := (AnotherItem is TUzfMf6Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TUzfMf6Item(AnotherItem);
    result := (Item.Infiltration = Infiltration)
      and (Item.PotentialET = PotentialET)
      and (Item.ExtinctionDepth = ExtinctionDepth)
      and (Item.ExtinctionWaterContent = ExtinctionWaterContent)
      and (Item.AirEntryPotential = AirEntryPotential)
      and (Item.RootPotential = RootPotential)
      and (Item.RootActivity = RootActivity)
  end;
end;

procedure TUzfMf6Item.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInfiltration,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPotentialET,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUzfExtinctDepth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FExtinctionWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FAirEntryPotential,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRootPotential,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRootActivity,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TUzfMf6Item.SetAirEntryPotential(const Value: string);
begin
  UpdateFormula(Value, UzfMf6AirEntryPotentialPosition, FAirEntryPotential);
end;

procedure TUzfMf6Item.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    UzfMf6InfiltrationPosition:
      Infiltration := Value;
    UzfMf6PotentialETPosition:
      PotentialET := Value;
    UzfMf6ExtinctionDepthPosition:
      ExtinctionDepth := Value;
    UzfMf6ExtinctionWaterContentPosition:
      ExtinctionWaterContent := Value;
    UzfMf6AirEntryPotentialPosition:
      AirEntryPotential := Value;
    UzfMf6RootPotentialPosition:
      RootPotential := Value;
    UzfMf6RootActivityPosition:
      RootActivity := Value;
    else Assert(False);
  end;
end;

procedure TUzfMf6Item.SetExtinctionDepth(const Value: string);
begin
  UpdateFormula(Value, UzfMf6ExtinctionDepthPosition, FUzfExtinctDepth);
end;

procedure TUzfMf6Item.SetExtinctionWaterContent(const Value: string);
begin
  UpdateFormula(Value, UzfMf6ExtinctionWaterContentPosition, FExtinctionWaterContent);
end;

procedure TUzfMf6Item.SetInfiltration(const Value: string);
begin
  UpdateFormula(Value, UzfMf6InfiltrationPosition, FInfiltration);
end;

procedure TUzfMf6Item.SetPotentialET(const Value: string);
begin
  UpdateFormula(Value, UzfMf6PotentialETPosition, FPotentialET);
end;

procedure TUzfMf6Item.SetRootActivity(const Value: string);
begin
  UpdateFormula(Value, UzfMf6RootActivityPosition, FRootActivity);
end;

procedure TUzfMf6Item.SetRootPotential(const Value: string);
begin
  UpdateFormula(Value, UzfMf6RootPotentialPosition, FRootPotential);
end;

{ TUzfMf6Collection }

procedure TUzfMf6Collection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TUzfMf6Storage.Create(AModel));
end;

procedure TUzfMf6Collection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  InfiltrationArray: TDataArray;
  PotentialETArray: TDataArray;
  ExtinctionDepthArray: TDataArray;
  ExtinctionWaterContentArray: TDataArray;
  AirEntryPotentialArray: TDataArray;
  RootPotentialArray: TDataArray;
  RootActivityArray: TDataArray;
  Boundary: TUzfMf6Storage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;

  InfiltrationArray := DataSets[UzfMf6InfiltrationPosition];
  PotentialETArray := DataSets[UzfMf6PotentialETPosition];
  ExtinctionDepthArray := DataSets[UzfMf6ExtinctionDepthPosition];
  ExtinctionWaterContentArray := DataSets[UzfMf6ExtinctionWaterContentPosition];
  AirEntryPotentialArray := DataSets[UzfMf6AirEntryPotentialPosition];
  RootPotentialArray := DataSets[UzfMf6RootPotentialPosition];
  RootActivityArray := DataSets[UzfMf6RootActivityPosition];

  Boundary := Boundaries[ItemIndex, AModel] as TUzfMf6Storage;
  InfiltrationArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := RowMin to RowMax do
        begin
          for ColIndex := ColMin to ColMax do
          begin
            if InfiltrationArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(PotentialETArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(ExtinctionDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(ExtinctionWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(AirEntryPotentialArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(RootPotentialArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(RootActivityArray.IsValue[LayerIndex, RowIndex, ColIndex]);

              with Boundary.UzfMf6Array[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;

                Infiltration := InfiltrationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                InfiltrationAnnotation := InfiltrationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                PotentialET := PotentialETArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                PotentialETAnnotation := PotentialETArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                ExtinctionDepth := ExtinctionDepthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                ExtinctionDepthAnnotation := ExtinctionDepthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                ExtinctionWaterContent := ExtinctionWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                ExtinctionWaterContentAnnotation := ExtinctionWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                AirEntryPotential := AirEntryPotentialArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                AirEntryPotentialAnnotation := AirEntryPotentialArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                RootPotential := RootPotentialArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RootPotentialAnnotation := RootPotentialArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                RootActivity := RootActivityArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RootActivityAnnotation := RootActivityArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  InfiltrationArray.CacheData;
  PotentialETArray.CacheData;
  ExtinctionDepthArray.CacheData;
  ExtinctionWaterContentArray.CacheData;
  AirEntryPotentialArray.CacheData;
  RootPotentialArray.CacheData;
  RootActivityArray.CacheData;

  Boundary.CacheData;
end;

function TUzfMf6Collection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  Result := TUzfMf6TimeListLink;
end;

procedure TUzfMf6Collection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TUzfMf6Item;
  ScreenObject: TScreenObject;
  ALink: TUzfMf6TimeListLink;
  InfiltrationData: TModflowTimeList;
  PotentialETData: TModflowTimeList;
  ExtinctionDepthData: TModflowTimeList;
  ExtinctionWaterContentData: TModflowTimeList;
  AirEntryPotentialData: TModflowTimeList;
  RootPotentialData: TModflowTimeList;
  RootActivityData: TModflowTimeList;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Infiltration;
  end;
  ALink := TimeListLink.GetLink(AModel) as TUzfMf6TimeListLink;
  InfiltrationData := ALink.FInfiltrationData;
  InfiltrationData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(InfiltrationData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.PotentialET;
  end;
  PotentialETData := ALink.FPotentialETData;
  PotentialETData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(PotentialETData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.ExtinctionDepth;
  end;
  ExtinctionDepthData := ALink.FExtinctionDepthData;
  ExtinctionDepthData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(ExtinctionDepthData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.ExtinctionWaterContent;
  end;
  ExtinctionWaterContentData := ALink.FExtinctionWaterContentData;
  ExtinctionWaterContentData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(ExtinctionWaterContentData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.AirEntryPotential;
  end;
  AirEntryPotentialData := ALink.FAirEntryPotentialData;
  AirEntryPotentialData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(AirEntryPotentialData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RootPotential;
  end;
  RootPotentialData := ALink.FRootPotentialData;
  RootPotentialData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RootPotentialData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RootActivity;
  end;
  RootActivityData := ALink.FRootActivityData;
  RootActivityData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RootActivityData.Count = Count);

  ClearBoundaries(AModel);
  SetBoundaryCapacity(InfiltrationData.Count, AModel);
  for TimeIndex := 0 to InfiltrationData.Count - 1 do
  begin
    AddBoundary(TUzfMf6Storage.Create(AModel));
  end;
  ListOfTimeLists.Add(InfiltrationData);
  ListOfTimeLists.Add(PotentialETData);
  ListOfTimeLists.Add(ExtinctionDepthData);
  ListOfTimeLists.Add(ExtinctionWaterContentData);
  ListOfTimeLists.Add(AirEntryPotentialData);
  ListOfTimeLists.Add(RootPotentialData);
  ListOfTimeLists.Add(RootActivityData);
end;

procedure TUzfMf6Collection.InvalidateUzfAirEntryPotentialData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FAirEntryPotentialData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FAirEntryPotentialData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfExtinctionDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FExtinctionDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FExtinctionDepthData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfExtinctionWaterContentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FExtinctionWaterContentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FExtinctionWaterContentData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfInfiltrationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FInfiltrationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FInfiltrationData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfPotentialETData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FPotentialETData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FPotentialETData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfRootActivityData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FRootActivityData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FRootActivityData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfRootPotentialData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FRootPotentialData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FRootPotentialData.Invalidate;
    end;
  end;
end;

class function TUzfMf6Collection.ItemClass: TBoundaryItemClass;
begin
  result := TUzfMf6Item;
end;

procedure TUzfMf6Collection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TUzfMf6Storage).
    FUzfMf6Array, BoundaryCount);
  inherited;
end;

{ TUzfMf6TimeListLink }

procedure TUzfMf6TimeListLink.CreateTimeLists;
begin
  inherited;

  FInfiltrationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInfiltrationData.NonParamDescription := StrUZFInfiltrationDat ;
  FInfiltrationData.ParamDescription := ' ' + StrUZFInfiltrationDat;
  AddTimeList(FInfiltrationData);
  if Model <> nil then
  begin
    FInfiltrationData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6Infiltration;
  end;

  FPotentialETData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPotentialETData.NonParamDescription := StrUZFPotentialET ;
  FPotentialETData.ParamDescription := ' ' + StrUZFPotentialET;
  AddTimeList(FPotentialETData);
  if Model <> nil then
  begin
    FPotentialETData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6PotentialET;
  end;

  FExtinctionDepthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FExtinctionDepthData.NonParamDescription := StrUZFExtinctionDepth ;
  FExtinctionDepthData.ParamDescription := ' ' + StrUZFExtinctionDepth;
  AddTimeList(FExtinctionDepthData);
  if Model <> nil then
  begin
    FExtinctionDepthData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6ExtinctionDepth;
  end;

  FExtinctionWaterContentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FExtinctionWaterContentData.NonParamDescription := StrUZFExtinctionWater ;
  FExtinctionWaterContentData.ParamDescription := ' ' + StrUZFExtinctionWater;
  AddTimeList(FExtinctionWaterContentData);
  if Model <> nil then
  begin
    FExtinctionWaterContentData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6ExtinctionWaterContent;
  end;

  FAirEntryPotentialData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FAirEntryPotentialData.NonParamDescription := StrUZFAirEntryPotent ;
  FAirEntryPotentialData.ParamDescription := ' ' + StrUZFAirEntryPotent;
  AddTimeList(FAirEntryPotentialData);
  if Model <> nil then
  begin
    FAirEntryPotentialData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6AirEntryPotential;
  end;

  FRootPotentialData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRootPotentialData.NonParamDescription := StrUZFRootPotential ;
  FRootPotentialData.ParamDescription := ' ' + StrUZFRootPotential;
  AddTimeList(FRootPotentialData);
  if Model <> nil then
  begin
    FRootPotentialData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6RootPotential;
  end;

  FRootActivityData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRootActivityData.NonParamDescription := StrUZFRootActivity ;
  FRootActivityData.ParamDescription := ' ' + StrUZFRootActivity;
  AddTimeList(FRootActivityData);
  if Model <> nil then
  begin
    FRootActivityData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6RootActivity;
  end;
end;

destructor TUzfMf6TimeListLink.Destroy;
begin
  FInfiltrationData.Free;
  FPotentialETData.Free;
  FExtinctionDepthData.Free;
  FExtinctionWaterContentData.Free;
  FAirEntryPotentialData.Free;
  FRootPotentialData.Free;
  FRootActivityData.Free;
  inherited;
end;

{ TUzfMf6_Cell }

procedure TUzfMf6_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TUzfMf6_Cell.GetAirEntryPotential: double;
begin
  result := FValues.AirEntryPotential;
end;

function TUzfMf6_Cell.GetAirEntryPotentialAnnotation: string;
begin
  result := FValues.AirEntryPotentialAnnotation;
end;

function TUzfMf6_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TUzfMf6_Cell.GetExtinctionDepth: double;
begin
  result := FValues.ExtinctionDepth;
end;

function TUzfMf6_Cell.GetExtinctionDepthAnnotation: string;
begin
  result := FValues.ExtinctionDepthAnnotation;
end;

function TUzfMf6_Cell.GetExtinctionWaterContent: double;
begin
  result := FValues.ExtinctionWaterContent;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentAnnotation: string;
begin
  result := FValues.ExtinctionDepthAnnotation;
end;

function TUzfMf6_Cell.GetInfiltration: double;
begin
  result := FValues.Infiltration;
end;

function TUzfMf6_Cell.GetInfiltrationAnnotation: string;
begin
  result := FValues.InfiltrationAnnotation;
end;

function TUzfMf6_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TUzfMf6_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TUzfMf6_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TUzfMf6_Cell.GetMvrIndex: Integer;
begin
  result := FValues.MvrIndex;
end;

function TUzfMf6_Cell.GetMvrUsed: Boolean;
begin
  result := FValues.MvrUsed;
end;

function TUzfMf6_Cell.GetPotentialET: double;
begin
  result := FValues.PotentialET;
end;

function TUzfMf6_Cell.GetPotentialETAnnotation: string;
begin
  result := FValues.PotentialETAnnotation;
end;

function TUzfMf6_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationAnnotation;
    UzfMf6PotentialETPosition: result := PotentialETAnnotation;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthAnnotation;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentAnnotation;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialAnnotation;
    UzfMf6RootPotentialPosition: result := RootPotentialAnnotation;
    UzfMf6RootActivityPosition: result := RootActivityAnnotation;
    else Assert(False);
  end;
end;

function TUzfMf6_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    UzfMf6InfiltrationPosition: result := Infiltration;
    UzfMf6PotentialETPosition: result := PotentialET;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepth;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContent;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotential;
    UzfMf6RootPotentialPosition: result := RootPotential;
    UzfMf6RootActivityPosition: result := RootActivity;
    else Assert(False);
  end;
end;

function TUzfMf6_Cell.GetRootActivity: double;
begin
  result := FValues.RootActivity;
end;

function TUzfMf6_Cell.GetRootActivityAnnotation: string;
begin
  result := FValues.RootActivityAnnotation;
end;

function TUzfMf6_Cell.GetRootPotential: double;
begin
  result := FValues.RootPotential;
end;

function TUzfMf6_Cell.GetRootPotentialAnnotation: string;
begin
  result := FValues.RootPotentialAnnotation;
end;

function TUzfMf6_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TUzfMf6_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TUzfMf6_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Uzf_Cell: TUzfMf6_Cell;
begin
  result := AnotherCell is TUzfMf6_Cell;
  if result then
  begin
    Uzf_Cell := TUzfMf6_Cell(AnotherCell);
    result :=
      (Infiltration = Uzf_Cell.Infiltration)
      and (PotentialET = Uzf_Cell.PotentialET)
      and (ExtinctionDepth = Uzf_Cell.ExtinctionDepth)
      and (ExtinctionWaterContent = Uzf_Cell.ExtinctionWaterContent)
      and (AirEntryPotential = Uzf_Cell.AirEntryPotential)
      and (RootPotential = Uzf_Cell.RootPotential)
      and (RootActivity = Uzf_Cell.RootActivity)
  end;
end;

procedure TUzfMf6_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TUzfMf6_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TUzfMf6_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TUzfMf6_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TUzfMf6_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TUzfMf6Boundary }

procedure TUzfMf6Boundary.Assign(Source: TPersistent);
var
  UzfSource: TUzfMf6Boundary;
begin
  if Source is TUzfMf6Boundary then
  begin
    UzfSource := TUzfMf6Boundary(Source);
    SurfaceDepressionDepth := UzfSource.SurfaceDepressionDepth;
    VerticalSaturatedK := UzfSource.VerticalSaturatedK;
    ResidualWaterContent := UzfSource.ResidualWaterContent;
    SaturatedWaterContent := UzfSource.SaturatedWaterContent;
    InitialWaterContent := UzfSource.InitialWaterContent;
    BrooksCoreyEpsilon := UzfSource.BrooksCoreyEpsilon;
  end;
  inherited;
end;

procedure TUzfMf6Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TUzfMf6_Cell;
  BoundaryValues: TUzfMf6Record;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TUzfMf6Storage;
  LocalModel: TCustomModel;
  MvrUsed: Boolean;
  LocalScreenObject: TScreenObject;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TUzfMf6Storage;
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TUzfMf6_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >=
      LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <=
      LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.UzfMf6Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.UzfMf6Array)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.UzfMf6Array) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.UzfMf6Array[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed;
        BoundaryValues.MvrIndex := BoundaryIndex;
        Cell := TUzfMf6_Cell.Create;
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

class function TUzfMf6Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TUzfMf6Collection;
end;

function TUzfMf6Boundary.BoundaryObserverPrefix: string;
begin
  result := 'UzfBoundary_';
end;

constructor TUzfMf6Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

end;

procedure TUzfMf6Boundary.CreateFormulaObjects;
begin
  inherited;
  FSurfaceDepressionDepth := CreateFormulaObjectBlocks(dsoTop);
  FVerticalSaturatedK := CreateFormulaObjectBlocks(dsoTop);
  FResidualWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FSaturatedWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FInitialWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FBrooksCoreyEpsilon := CreateFormulaObjectBlocks(dsoTop);

end;

procedure TUzfMf6Boundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(SurfaceDepressionDepthObserver);
    FObserverList.Add(VerticalSaturatedKObserver);
    FObserverList.Add(ResidualWaterContentObserver);
    FObserverList.Add(SaturatedWaterContentObserver);
    FObserverList.Add(InitialWaterContentObserver);
    FObserverList.Add(BrooksCoreyEpsilonObserver);
  end;
end;

destructor TUzfMf6Boundary.Destroy;
begin
  SurfaceDepressionDepth := '1';
  VerticalSaturatedK := '0';
  ResidualWaterContent := '0';
  SaturatedWaterContent := '0';
  InitialWaterContent := '0';
  BrooksCoreyEpsilon := '3.5';

  inherited;
end;

function TUzfMf6Boundary.GetBrooksCoreyEpsilon: string;
begin
  Result := FBrooksCoreyEpsilon.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(BrooksCoreyEpsilonPosition);
  end;
end;

function TUzfMf6Boundary.GetBrooksCoreyEpsilonObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBrooksCoreyEpsilonObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_BrooksCoreyEpsilon_', FBrooksCoreyEpsilonObserver, DataArray);
  end;
  result := FBrooksCoreyEpsilonObserver;
end;

procedure TUzfMf6Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TUzfMf6Storage;
begin
  EvaluateArrayBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TUzfMf6Storage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TUzfMf6Boundary.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(InitialWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetInitialWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FInitialWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Initial_Water_Content_', FInitialWaterContentObserver, DataArray);
  end;
  result := FInitialWaterContentObserver;
end;

procedure TUzfMf6Boundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FSurfaceDepressionDepth then
  begin
    List.Add(FObserverList[SurfaceDepressionDepthPosition]);
  end
  else if Sender = FVerticalSaturatedK then
  begin
    List.Add(FObserverList[VerticalSaturatedKPosition]);
  end
  else if Sender = FResidualWaterContent then
  begin
    List.Add(FObserverList[ResidualWaterContentPosition]);
  end
  else if Sender = FSaturatedWaterContent then
  begin
    List.Add(FObserverList[SaturatedWaterContentPosition]);
  end
  else if Sender = FInitialWaterContentObserver then
  begin
    List.Add(FObserverList[InitialWaterContentPosition]);
  end
  else if Sender = FBrooksCoreyEpsilon then
  begin
    List.Add(FObserverList[BrooksCoreyEpsilonPosition]);
  end;
end;

function TUzfMf6Boundary.GetResidualWaterContent: string;
begin
  Result := FResidualWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ResidualWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetResidualWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FResidualWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Residual_Content_', FResidualWaterContentObserver, DataArray);
  end;
  result := FResidualWaterContentObserver;
end;

function TUzfMf6Boundary.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(SaturatedWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetSaturatedWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FSaturatedWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Saturated_Water_Content_', FSaturatedWaterContentObserver, DataArray);
  end;
  result := FSaturatedWaterContentObserver;
end;

function TUzfMf6Boundary.GetSurfaceDepressionDepth: string;
begin
  Result := FSurfaceDepressionDepth.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(SurfaceDepressionDepthPosition);
  end;
end;

function TUzfMf6Boundary.GetSurfaceDepressionDepthObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FSurfaceDepressionDepthObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Surface_Depression_Depth_', FSurfaceDepressionDepthObserver, DataArray);
  end;
  result := FSurfaceDepressionDepthObserver;
end;

function TUzfMf6Boundary.GetVerticalSaturatedK: string;
begin
  Result := FVerticalSaturatedK.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(VerticalSaturatedKPosition);
  end;
end;

function TUzfMf6Boundary.GetVerticalSaturatedKObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FVerticalSaturatedKObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Kz_', FVerticalSaturatedKObserver, DataArray);
  end;
  result := FVerticalSaturatedKObserver;
end;

procedure TUzfMf6Boundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateUzfMf6Infiltration(self);
    Model.InvalidateUzfMf6PotentialET(self);
    Model.InvalidateUzfMf6ExtinctionDepth(self);
    Model.InvalidateUzfMf6ExtinctionWaterContent(self);
    Model.InvalidateUzfMf6AirEntryPotential(self);
    Model.InvalidateUzfMf6RootPotential(self);
    Model.InvalidateUzfMf6RootActivity(self);
  end;
end;

procedure TUzfMf6Boundary.SetBrooksCoreyEpsilon(const Value: string);
begin
  UpdateFormulaBlocks(Value, BrooksCoreyEpsilonPosition, FBrooksCoreyEpsilon);
end;

procedure TUzfMf6Boundary.SetInitialWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, InitialWaterContentPosition, FInitialWaterContent);
end;

procedure TUzfMf6Boundary.SetResidualWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, ResidualWaterContentPosition, FResidualWaterContent);
end;

procedure TUzfMf6Boundary.SetSaturatedWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, SaturatedWaterContentPosition, FSaturatedWaterContent);
end;

procedure TUzfMf6Boundary.SetSurfaceDepressionDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, SurfaceDepressionDepthPosition, FSurfaceDepressionDepth);
end;

procedure TUzfMf6Boundary.SetVerticalSaturatedK(const Value: string);
begin
  UpdateFormulaBlocks(Value, VerticalSaturatedKPosition, FVerticalSaturatedK);
end;

end.
