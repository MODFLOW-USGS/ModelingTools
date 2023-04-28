unit Mt3dSftUnit;

interface

uses ModflowCellUnit, Mt3dmsChemUnit, System.Classes, ModflowBoundaryUnit,
  GoPhastTypes, SubscriptionUnit,
  System.Generics.Collections, System.SysUtils, System.ZLib, DataSetUnit,
  OrderedCollectionUnit, OrderedCollectionInterfaceUnit,
  GlobalVariablesInterfaceUnit;

type
  TSftSteady = class(TObject)
  private
    FCell: TCellLocation;
    FInitConcentrations: TList<Double>;
    FDispersions: TList<Double>;
    FInitConcentrationsAnnotations: TStringList;
    FDispersionsAnnotations: TStringList;
    FIsObservation: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Cell: TCellLocation read FCell write FCell;
    property InitConcentrations: TList<Double> read FInitConcentrations;
    property Dispersions: TList<Double> read FDispersions;
    property InitConcentrationsAnnotations: TStringList read FInitConcentrationsAnnotations;
    property DispersionsAnnotations: TStringList read FDispersionsAnnotations;
    property IsObservation: Boolean read FIsObservation;
  end;

  TSftSteadyObjectList = TObjectList<TSftSteady>;

  TCustomSftItem = class(TCustomMt3dmsConcItem)
  protected
    function GetObserver(Index: Integer): TObserver; override;
    procedure Loaded;
    function GetMt3dmsConc(Index: integer): string; override;
    procedure SetMt3dmsConc(Index: integer; const Value: string); override;
  end;

  TMt3dSftInitConcItem = class(TCustomSftItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
  end;

  TMt3dSftInitConcTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TCustomSftSteadyCollection = class(TCustomMt3dmsArrayConcCollection)
  private
    procedure Loaded;
  protected
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  end;

  TMt3dSftInitConcCollection = class(TCustomSftSteadyCollection)
  private
    procedure InvalidateSftInitConc(Sender: TObject);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
//    procedure InvalidateModel; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TMt3dSftDispItem = class(TCustomSftItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
//    procedure InvalidateModel; override;
  end;

  TMt3dSftDispTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TMt3dSftDispCollection = class(TCustomSftSteadyCollection)
  private
    procedure InvalidateSftDisp(Sender: TObject);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
//    procedure InvalidateModel; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TSftBoundaryType = (sbtHeadwater, sbtPrecipitation, sbtRunoff, sbtConstConc);

  TSftReachItem = class(TCustomMt3dmsConcItem)
  private
    function GetBoundaryType: TSftBoundaryType;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure InvalidateModel; override;
    property BoundaryType: TSftBoundaryType read GetBoundaryType;
  end;

  TCustomSftReachTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  private
    FBoundaryType: TSftBoundaryType;
    procedure SetBoundaryType(const Value: TSftBoundaryType);
  protected
    procedure CreateTimeLists; override;
  public
    property BoundaryType: TSftBoundaryType read FBoundaryType write SetBoundaryType;
  end;

  THeadWaterSftReachTimeListLink = class(TCustomSftReachTimeListLink)
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
  end;

  TPrecipitationSftReachTimeListLink = class(TCustomSftReachTimeListLink)
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
  end;

  TRunoffSftReachTimeListLink = class(TCustomSftReachTimeListLink)
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
  end;

  TConstConcSftReachTimeListLink = class(TCustomSftReachTimeListLink)
    Constructor Create(AModel: TBaseModel;
      ABoundary: TCustomMF_BoundColl); override;
  end;

  TCustomMt3dSftReachCollection = class(TCustomMt3dmsArrayConcCollection)
  private
    FBoundaryType: TSftBoundaryType;
    procedure InvalidateSfrConc(Sender: TObject);
    procedure SetBoundaryType(const Value: TSftBoundaryType);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure InvalidateModel; override;
    function ConcName: string; override;
    procedure CountArrayBoundaryCells(var BoundaryCount: Integer;
      DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
  public
    property BoundaryType: TSftBoundaryType read FBoundaryType write SetBoundaryType;
  end;

  THeadWaterMt3dSftReachCollection = class(TCustomMt3dSftReachCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: IModelForTOrderedCollection; ScreenObject: TObject); override;
  end;

  TPrecipitationMt3dSftReachCollection = class(TCustomMt3dSftReachCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: IModelForTOrderedCollection; ScreenObject: TObject); override;
  end;

  TRunoffMt3dSftReachCollection = class(TCustomMt3dSftReachCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: IModelForTOrderedCollection; ScreenObject: TObject); override;
  end;

  TConstConcMt3dSftReachCollection = class(TCustomMt3dSftReachCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: IModelForTOrderedCollection; ScreenObject: TObject); override;
  end;

  TSftObsLocation = (solNone, solFirst, solLast, solAll);

  TMt3dmsSftConc_Cell = class(TMt3dmsConc_Cell)
  private
    FBoundaryType: TSftBoundaryType;
    // @name starts at zero.
    FReachNumber: Integer;
  protected
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream;
      Annotations: TStringList); override;
  public
    property BoundaryType: TSftBoundaryType read FBoundaryType;
    // @name starts at zero.
    property ReachNumber: Integer read FReachNumber;
  end;

  TMt3dSftBoundary = class(TCustomMt3dmsConcBoundary)
  private
    FObsLocation: TSftObsLocation;
    FPrecipitation: TPrecipitationMt3dSftReachCollection;
    FRunOff: TRunoffMt3dSftReachCollection;
    FConstConc: TConstConcMt3dSftReachCollection;
    FInitialConcentration: TMt3dSftInitConcCollection;
    FDispersionCoefficient: TMt3dSftDispCollection;
    FCurrentBoundaryType: TSftBoundaryType;
    FStartingReachNumber: Integer;
    FConstConcObserver: TObserver;
    FHeadWaterObserver: TObserver;
    FPrecipObserver: TObserver;
    FRunoffObserver: TObserver;
    procedure SetObsLocation(const Value: TSftObsLocation);
    procedure SetConstConc(const Value: TConstConcMt3dSftReachCollection);
    procedure SetPrecipitation(
      const Value: TPrecipitationMt3dSftReachCollection);
    procedure SetRunOff(const Value: TRunoffMt3dSftReachCollection);
    procedure AssignSftCells(BoundaryStorage: TMt3dmsConcStorage;
      ValueTimeList: TList);
    procedure SetDispersionCoefficient(const Value: TMt3dSftDispCollection);
    procedure SetInitialConcentration(const Value: TMt3dSftInitConcCollection);
    procedure CreateObservers;
    function GetConstConcObserver: TObserver;
    function GetHeadWaterObserver: TObserver;
    function GetPrecipObserver: TObserver;
    function GetRunoffObserver: TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TMt3dmsSftConc_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure ClearBoundaries(AModel: TBaseModel); override;
    function BoundaryObserverPrefix: string; override;
    property HeadWaterObserver: TObserver read GetHeadWaterObserver;
    property PrecipObserver: TObserver read GetPrecipObserver;
    property RunoffObserver: TObserver read GetRunoffObserver;
    property ConstConcObserver: TObserver read GetConstConcObserver;

  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TMt3dmsConcStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure Clear; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    procedure GetPrecipCells(PrecipTimeList: TList; AModel: TBaseModel);
    procedure GetRunOffCells(PrecipTimeList: TList; AModel: TBaseModel);
    procedure GetConstConcCells(PrecipTimeList: TList; AModel: TBaseModel);
    procedure Loaded;
    procedure InsertNewSpecies(SpeciesIndex: integer; const Name: string); override;
    procedure RenameSpecies(const OldSpeciesName, NewSpeciesName: string); override;
    procedure ChangeSpeciesPosition(OldIndex, NewIndex: integer); override;
    procedure DeleteSpecies(SpeciesIndex: integer); override;
    function Used: boolean; override;
    procedure AssignInitConcAndDisp(AModel: TBaseModel; SftSteadyList: TSftSteadyObjectList);
    property StartingReachNumber: Integer read FStartingReachNumber write FStartingReachNumber;
    procedure InvalidateDisplay; override;
  published
    property InitialConcentration: TMt3dSftInitConcCollection read FInitialConcentration
      write SetInitialConcentration;
    property DispersionCoefficient: TMt3dSftDispCollection read FDispersionCoefficient
      write SetDispersionCoefficient;
    property ObsLocation: TSftObsLocation read FObsLocation write SetObsLocation;
    property Precipitation: TPrecipitationMt3dSftReachCollection read FPrecipitation write SetPrecipitation;
    property RunOff: TRunoffMt3dSftReachCollection read FRunOff write SetRunOff;
    property ConstConc: TConstConcMt3dSftReachCollection read FConstConc write SetConstConc;
  end;

implementation

uses
  PhastModelUnit, Mt3dmsChemSpeciesUnit, RbwParser,
  frmGoPhastUnit, ModflowTimeUnit, ScreenObjectUnit, frmFormulaErrorsUnit,
  GlobalVariablesUnit, GIS_Functions, ModflowSfrReachUnit, DataSetNamesUnit,
  CellLocationUnit;

resourcestring
  StrHeadwater = 'Headwater';
  StrPrecipitation = 'Precipitation';
  StrRunoff = 'Runoff';
  StrConstantConcentrati = 'Constant Concentration';
  StrSFTInitialConcentr = 'SFT Initial Concentration';
  StrFormulaDoesNotRes = 'Formula does not result in a real number';
  StrSFTDispersion = 'SFT Dispersion';
  StrAssignedBy0sWit = 'Assigned by %0:s with the formula %1:s';

const
  InitialConcPosition = 0;
  DispCoeffPosition = 1;


{ TSftReachItem }

procedure TSftReachItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCustomMt3dSftReachCollection;
  ConcentrationObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TCustomMt3dSftReachCollection;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    ConcentrationObserver := FObserverList[Index];
    ConcentrationObserver.OnUpToDateSet :=
      ParentCollection.InvalidateSfrConc;
  end;
end;

function TSftReachItem.GetBoundaryType: TSftBoundaryType;
var
  ParentCollection: TCustomMt3dSftReachCollection;
begin
  ParentCollection := Collection as TCustomMt3dSftReachCollection;
  result := ParentCollection.BoundaryType;
end;

procedure TSftReachItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    case BoundaryType of
      sbtHeadwater:
        begin
          PhastModel.InvalidateSftHeadwatersConc(self);
        end;
      sbtPrecipitation:
        begin
          PhastModel.InvalidateSftPrecipConc(self);
        end;
      sbtRunoff:
        begin
          PhastModel.InvalidateSftRunoffConc(self);
        end;
      sbtConstConc:
        begin
          PhastModel.InvalidateSftConstConc(self);
        end;
      else
        Assert(False);
    end;
  end;
end;

{ TSftReachTimeListLink }

procedure TCustomSftReachTimeListLink.CreateTimeLists;
var
  Index: Integer;
  SftConcData: TModflowTimeList;
  Item: TChemSpeciesItem;
  LocalModel: TPhastModel;
begin
  inherited;
  TimeLists.Clear;
  ListOfTimeLists.Clear;
  LocalModel := frmGoPhast.PhastModel;
  for Index := 0 to LocalModel.MobileComponents.Count - 1 do
  begin
    Item := LocalModel.MobileComponents[Index];
    SftConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    case FBoundaryType of
      sbtHeadwater:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrHeadwater;
          SftConcData.ParamDescription := Item.Name +  StrHeadwater;
        end;
      sbtPrecipitation:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrPrecipitation;
          SftConcData.ParamDescription := Item.Name +  StrPrecipitation;
        end;
      sbtRunoff:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrRunoff;
          SftConcData.ParamDescription := Item.Name +  StrRunoff;
        end;
      sbtConstConc:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrConstantConcentrati;
          SftConcData.ParamDescription := Item.Name +  StrConstantConcentrati;
        end;
      else
        Assert(False);
    end;
    if Model <> nil then
    begin
//      SftConcData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateUztRechConc;
    end;
    AddTimeList(SftConcData);
    ListOfTimeLists.Add(SftConcData);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
    Item := LocalModel.ImmobileComponents[Index];
    SftConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    case FBoundaryType of
      sbtHeadwater:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrHeadwater;
          SftConcData.ParamDescription := Item.Name +  StrHeadwater;
        end;
      sbtPrecipitation:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrPrecipitation;
          SftConcData.ParamDescription := Item.Name +  StrPrecipitation;
        end;
      sbtRunoff:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrRunoff;
          SftConcData.ParamDescription := Item.Name +  StrRunoff;
        end;
      sbtConstConc:
        begin
          SftConcData.NonParamDescription := Item.Name +  StrConstantConcentrati;
          SftConcData.ParamDescription := Item.Name +  StrConstantConcentrati;
        end;
      else
        Assert(False);
    end;
    if Model <> nil then
    begin
      case FBoundaryType of
        sbtHeadwater:
          begin
//            SftConcData.OnInvalidate :=
//              (Model as TCustomModel).InvalidateUztRechConc;
          end;
        sbtPrecipitation:
          begin
//            SftConcData.OnInvalidate :=
//              (Model as TCustomModel).InvalidateUztRechConc;
          end;
        sbtRunoff:
          begin
//            SftConcData.OnInvalidate :=
//              (Model as TCustomModel).InvalidateUztRechConc;
          end;
        sbtConstConc:
          begin
//            SftConcData.OnInvalidate :=
//              (Model as TCustomModel).InvalidateUztRechConc;
          end;
        else
          Assert(False);
      end;
    end;
    AddTimeList(SftConcData);
    ListOfTimeLists.Add(SftConcData);
  end;
end;

procedure TCustomSftReachTimeListLink.SetBoundaryType(const Value: TSftBoundaryType);
begin
  FBoundaryType := Value;
end;

{ TCustomMt3dSftReachCollection }

procedure TCustomMt3dSftReachCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  ConcArray: TDataArray;
//  LgrReachLengthArray: TDataArray;
  Boundary: TMt3dmsConcStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalScreenObject: TScreenObject;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TCustomModel;
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  LocalModel := AModel as TCustomModel;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;

  ConcArray := DataSets[0];
//  LgrReachLengthArray := DataSets[1];

  BoundaryIndex := -1;
  Boundary := Boundaries[ItemIndex, AModel] as TMt3dmsConcStorage;

  LocalScreenObject := ScreenObject as TScreenObject;
  for SegmentIndex := 0 to LocalScreenObject.Segments[LocalModel].Count - 1 do
  begin
    Segment := LocalScreenObject.Segments[LocalModel][SegmentIndex];
    ColIndex := Segment.Col;
    RowIndex := Segment.Row;
    LayerIndex := Segment.Layer;
    if not LocalModel.IsLayerSimulated(LayerIndex) then
    begin
      Continue;
    end;
    if not ConcArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
      Continue;
    end;
    if (ColIndex = PriorCol)
      and (RowIndex = PriorRow)
      and (LayerIndex = PriorLayer) then
    begin
      Continue
    end;
    Inc(BoundaryIndex);
    PriorCol := Segment.Col;
    PriorRow := Segment.Row;
    PriorLayer := Segment.Layer;

    Assert(BoundaryIndex < Length(Boundary.Mt3dmsConcArray));
    if ConcArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
//      Boundary := Boundaries[ItemIndex] as TSfrStorage;
      Assert(BoundaryIndex < Length(Boundary.Mt3dmsConcArray));
      with Boundary.Mt3dmsConcArray[BoundaryIndex] do
      begin
        Cell.Layer := LayerIndex;
        Cell.Row := RowIndex;
        Cell.Column := ColIndex;

        for DataArrayIndex := 0 to DataSets.Count - 1 do
        begin
          ADataArray := DataSets[DataArrayIndex];
          Assert(ADataArray.IsValue[LayerIndex, RowIndex, ColIndex]);
          Concentration[DataArrayIndex] := ADataArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          ConcentrationAnnotation[DataArrayIndex] := ADataArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;

      end;
    end;
  end;
  ConcArray.CacheData;
//  LgrReachLengthArray.CacheData;

  Boundary.CacheData;
end;

function TCustomMt3dSftReachCollection.ConcName: string;
begin
    case FBoundaryType of
      sbtHeadwater:
        begin
          result := StrHeadwater;
        end;
      sbtPrecipitation:
        begin
          result := StrPrecipitation;
        end;
      sbtRunoff:
        begin
          result := StrRunoff;
        end;
      sbtConstConc:
        begin
          result := StrConstantConcentrati;
        end;
      else
        Assert(False);
    end;
end;

procedure TCustomMt3dSftReachCollection.CountArrayBoundaryCells(
  var BoundaryCount: Integer; DataArray1: TDataArray; DataSets: TList;
  AModel: TBaseModel);
begin
//  inherited;
  CountArrayBoundaryCellsSfr(BoundaryCount, DataArray1, DataSets, AModel,
    ScreenObject as TScreenObject);
end;

procedure TCustomMt3dSftReachCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    case BoundaryType of
      sbtHeadwater:
        begin
          PhastModel.InvalidateSftHeadwatersConc(self);
        end;
      sbtPrecipitation:
        begin
          PhastModel.InvalidateSftPrecipConc(self);
        end;
      sbtRunoff:
        begin
          PhastModel.InvalidateSftRunoffConc(self);
        end;
      sbtConstConc:
        begin
          PhastModel.InvalidateSftConstConc(self);
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TCustomMt3dSftReachCollection.InvalidateSfrConc(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCustomSftReachTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  index: Integer;
  ATimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TCustomSftReachTimeListLink;
    for index := 0 to Link.TimeLists.Count - 1 do
    begin
      ATimeList := Link.TimeLists[index];
      ATimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TCustomSftReachTimeListLink;
        for index := 0 to Link.TimeLists.Count - 1 do
        begin
          ATimeList := Link.TimeLists[index];
          ATimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

class function TCustomMt3dSftReachCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSftReachItem;
end;

procedure TCustomMt3dSftReachCollection.SetBoundaryType(
  const Value: TSftBoundaryType);
begin
  FBoundaryType := Value;
end;

{ TCustomMt3dSftReachBoundary }


{ THeadWaterSftReachTimeListLink }

constructor THeadWaterSftReachTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  FBoundaryType := sbtHeadwater;
end;

{ TPrecipitationSftReachTimeListLink }

constructor TPrecipitationSftReachTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  FBoundaryType := sbtPrecipitation;
end;

{ TRunoffSftReachTimeListLink }

constructor TRunoffSftReachTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  FBoundaryType := sbtRunoff;
end;

{ TConstConcSftReachTimeListLink }

constructor TConstConcSftReachTimeListLink.Create(AModel: TBaseModel;
  ABoundary: TCustomMF_BoundColl);
begin
  inherited;
  FBoundaryType := sbtConstConc;
end;

{ THeadWaterMt3dSftReachCollection }

constructor THeadWaterMt3dSftReachCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: IModelForTOrderedCollection;
  ScreenObject: TObject);
begin
  inherited;
  FBoundaryType := sbtHeadwater;
end;

class function THeadWaterMt3dSftReachCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := THeadWaterSftReachTimeListLink;
end;

{ TPrecipitationMt3dSftReachCollection }

constructor TPrecipitationMt3dSftReachCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: IModelForTOrderedCollection;
  ScreenObject: TObject);
begin
  inherited;
  FBoundaryType := sbtPrecipitation;
end;

class function TPrecipitationMt3dSftReachCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TPrecipitationSftReachTimeListLink;
end;

{ TRunoffMt3dSftReachCollection }

constructor TRunoffMt3dSftReachCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: IModelForTOrderedCollection;
  ScreenObject: TObject);
begin
  inherited;
  FBoundaryType := sbtRunoff;
end;

class function TRunoffMt3dSftReachCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TRunoffSftReachTimeListLink;
end;

{ TConstConcMt3dSftReachCollection }

constructor TConstConcMt3dSftReachCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: IModelForTOrderedCollection;
  ScreenObject: TObject);
begin
  inherited;
  FBoundaryType := sbtConstConc;
end;

class function TConstConcMt3dSftReachCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TConstConcSftReachTimeListLink;
end;


{ TMt3dSftBoundary }

procedure TMt3dSftBoundary.Assign(Source: TPersistent);
var
  SftBoundary: TMt3dSftBoundary;
begin
  if Source is TMt3dSftBoundary then
  begin
    SftBoundary := TMt3dSftBoundary(Source);
    InitialConcentration := SftBoundary.InitialConcentration;
    DispersionCoefficient := SftBoundary.DispersionCoefficient;
    ObsLocation := SftBoundary.ObsLocation;

    Precipitation := SftBoundary.Precipitation;
    RunOff := SftBoundary.RunOff;
    ConstConc := SftBoundary.ConstConc;
  end;
  inherited;

end;

procedure TMt3dSftBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TMt3dmsSftConc_Cell;
  BoundaryValues: TMt3dmsConcentrationRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMt3dmsConcStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
//  Grid: TCustomModelGrid;
begin
  LocalModel := AModel as TCustomModel;

  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;

  LocalBoundaryStorage := BoundaryStorage as TMt3dmsConcStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TMt3dmsSftConc_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count
        + Length(LocalBoundaryStorage.Mt3dmsConcArray) then
      begin
        Cells.Capacity := Cells.Count
          + Length(LocalBoundaryStorage.Mt3dmsConcArray)
      end;
      // Cells.CheckRestore;
      for BoundaryIndex := 0 to
        Length(LocalBoundaryStorage.Mt3dmsConcArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.Mt3dmsConcArray[BoundaryIndex];
        if (FCurrentBoundaryType = sbtHeadWater) and (BoundaryIndex > 0) then
        begin
          Continue;
        end;
        Cell := TMt3dmsSftConc_Cell.Create;
        Cells.Add(Cell);

        LocalModel.AdjustCellPosition(Cell);
        Cell.IFace := LocalScreenObject.IFace;
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
        Cell.SetConcentrationLength(Length(Cell.Values.Concentration));
        Cell.FBoundaryType := FCurrentBoundaryType;
        Cell.FReachNumber := BoundaryIndex + StartingReachNumber;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TMt3dSftBoundary.AssignInitConcAndDisp(AModel: TBaseModel;
  SftSteadyList: TSftSteadyObjectList);
var
  LocalScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  SteadyProp: TSftSteady;
  Parser: TRbwParser;
  InitConcExpressions: TList<TExpression>;
  DispExpressions: TList<TExpression>;
  FormulaIndex: Integer;
  AFormula: string;
  Expression: TExpression;
  UsedVariables: TStringList;
  VarIndex: Integer;
  VarName: string;
  VarPosition: Integer;
  Variable: TCustomValue;
  LocalModel: TCustomModel;
  AnotherDataSet: TDataArray;
  Variables: TList<TCustomValue>;
  DataSets: TDataArrayList;
  GlobalVariable: IGlobalVariable;
  ExpressionIndex: Integer;
  InitConcExpressionAnnotations: TStringList;
  DispExpressionAnnotations: TStringList;
  InitItem: TMt3dSftInitConcItem;
  InitDispItem: TMt3dSftDispItem;
  procedure UpdateRequiredData;
  var
    ADataSet: TDataArray;
    Variable: TCustomValue;
    VarIndex: Integer;
    Layer: Integer;
    Cell: TCellAssignment;
  begin
    Cell := ACell;
    UpdateGlobalLocations(Cell.Column, Cell.Row, Cell.Layer, eaBlocks,
      AModel);
    UpdateCurrentSegment(Cell.Segment);
    UpdateCurrentSection(Cell.Section);
    for VarIndex := 0 to Variables.Count - 1 do
    begin
      Variable := Variables[VarIndex];
      ADataSet := DataSets[VarIndex];
      Layer := -1;
      case ADataSet.Orientation of
        dsoTop:
          begin
            Layer := 0;
          end;
        dso3D:
          begin
            Layer := Cell.Layer;
          end;
      else
        begin
          Assert(False);
        end;
      end;
      case ADataSet.DataType of
        rdtDouble:
          begin
            (Variable as TRealVariable).Value := ADataSet.RealData[Layer, Cell.Row, Cell.Column];
          end;
        rdtInteger:
          begin
            (Variable as TIntegerVariable).Value := ADataSet.IntegerData[Layer, Cell.Row, Cell.Column];
          end;
        rdtBoolean:
          begin
            (Variable as TBooleanVariable).Value := ADataSet.BooleanData[Layer, Cell.Row, Cell.Column];
          end;
        rdtString:
          begin
            (Variable as TStringVariable).Value := ADataSet.StringData[Layer, Cell.Row, Cell.Column];
          end;
      else
        Assert(False);
      end;
    end;
  end;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalModel := (AModel as TCustomModel);
  Parser := LocalModel.rpThreeDFormulaCompiler;
  InitConcExpressions := TList<TExpression>.Create;
  DispExpressions := TList<TExpression>.Create;
  InitConcExpressionAnnotations := TStringList.Create;
  DispExpressionAnnotations := TStringList.Create;
  UsedVariables := TStringList.Create;
  Variables := TList<TCustomValue>.Create;
  DataSets := TDataArrayList.Create;
  try
    UsedVariables.Sorted := True;
    UsedVariables.Duplicates := dupIgnore;
    InitItem := InitialConcentration[0] as TMt3dSftInitConcItem;
    for FormulaIndex := 0 to InitItem.BoundaryFormulaCount - 1 do
    begin
      AFormula := InitItem.BoundaryFormula[FormulaIndex];
      try
        Parser.Compile(AFormula);
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
            StrSFTInitialConcentr, AFormula, E.Message);
          AFormula := '0';
          Parser.Compile(AFormula);
        end;
      end;
      Expression := Parser.CurrentExpression;
      if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
      begin
        frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
          StrSFTInitialConcentr, AFormula, StrFormulaDoesNotRes);
        AFormula := '0';
        Parser.Compile(AFormula);
        Expression := Parser.CurrentExpression;
      end;
      InitConcExpressions.Add(Expression);
      InitConcExpressionAnnotations.Add(Format(StrAssignedBy0sWit, [LocalScreenObject.Name, AFormula]));
      UsedVariables.AddStrings(Expression.VariablesUsed);
    end;
    InitDispItem := DispersionCoefficient[0] as TMt3dSftDispItem;
    for FormulaIndex := 0 to InitDispItem.BoundaryFormulaCount - 1 do
    begin
      AFormula := InitDispItem.BoundaryFormula[FormulaIndex];
      try
        Parser.Compile(AFormula);
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
            StrSFTDispersion, AFormula, E.Message);
          AFormula := '0';
          Parser.Compile(AFormula);
        end;
      end;
      Expression := Parser.CurrentExpression;
      if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
      begin
        frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
          StrSFTDispersion, AFormula, StrFormulaDoesNotRes);
        AFormula := '0';
        Parser.Compile(AFormula);
        Expression := Parser.CurrentExpression;
      end;
      DispExpressions.Add(Expression);
      DispExpressionAnnotations.Add(Format(StrAssignedBy0sWit, [LocalScreenObject.Name, AFormula]));
      UsedVariables.AddStrings(Expression.VariablesUsed);
    end;

    for VarIndex := 0 to UsedVariables.Count - 1 do
    begin
      VarName := UsedVariables[VarIndex];
      VarPosition := Parser.IndexOfVariable(VarName);
      Variable := Parser.Variables[VarPosition];
      AnotherDataSet := LocalModel.DataArrayManager.GetDataSetByName(VarName);
      if AnotherDataSet <> nil then
      begin
        Assert(AnotherDataSet.DataType = Variable.ResultType);
        AnotherDataSet.Initialize;
        LocalModel.DataArrayManager.AddDataSetToCache(AnotherDataSet);
        Variables.Add(Variable);
        DataSets.Add(AnotherDataSet);
      end
      else
      begin
        GlobalVariable := LocalModel.GlobalVariables.GetVariableByName(VarName);
        Assert(GlobalVariable <> nil);
        Assert(Variable.ResultType = GlobalVariable.Format);
      end;
    end;



    CellList := TCellAssignmentList.Create;
    try
      LocalScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, AModel);
      UpdateCurrentScreenObject(LocalScreenObject);

      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex];
        SteadyProp := TSftSteady.Create;
        SftSteadyList.Add(SteadyProp);
        SteadyProp.Cell := ACell.Cell;
        UpdateRequiredData;

        case ObsLocation of
          solNone:
            begin
              SteadyProp.FIsObservation := False;
            end;
          solFirst:
            begin
              SteadyProp.FIsObservation := CellIndex = 0;
            end;
          solLast:
            begin
              SteadyProp.FIsObservation := CellIndex = (CellList.Count - 1);
            end;
          solAll:
            begin
              SteadyProp.FIsObservation := True;
            end;
          else
            Assert(False);
        end;

        SteadyProp.InitConcentrations.Capacity := InitConcExpressions.Count;
        SteadyProp.InitConcentrationsAnnotations.Capacity := InitConcExpressions.Count;
        SteadyProp.Dispersions.Capacity := DispExpressions.Count;
        SteadyProp.DispersionsAnnotations.Capacity := DispExpressions.Count;

        for ExpressionIndex := 0 to InitConcExpressions.Count - 1 do
        begin
          Expression := InitConcExpressions[ExpressionIndex];
          Expression.Evaluate;
          SteadyProp.InitConcentrations.Add(Expression.DoubleResult);
          SteadyProp.InitConcentrationsAnnotations.Add(
            InitConcExpressionAnnotations[ExpressionIndex]);
        end;
        for ExpressionIndex := 0 to DispExpressions.Count - 1 do
        begin
          Expression := DispExpressions[ExpressionIndex];
          Expression.Evaluate;
          SteadyProp.Dispersions.Add(Expression.DoubleResult);
          SteadyProp.DispersionsAnnotations.Add(
            DispExpressionAnnotations[ExpressionIndex]);
        end;
      end;
    finally
      CellList.Free;
    end;
  finally
    Variables.Free;
    DataSets.Free;
    InitConcExpressions.Free;
    DispExpressions.Free;
    UsedVariables.Free;
    InitConcExpressionAnnotations.Free;
    DispExpressionAnnotations.Free;
  end;
end;

procedure TMt3dSftBoundary.AssignSftCells(BoundaryStorage: TMt3dmsConcStorage;
  ValueTimeList: TList);
var
  Cell: TMt3dmsSftConc_Cell;
  BoundaryValues: TMt3dmsConcentrationRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMt3dmsConcStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;// as TEvtStorage;
  for TimeIndex := 0 to
    (ParentModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TMt3dmsSftConc_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Mt3dmsConcArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.Mt3dmsConcArray[BoundaryIndex];
        Cell := TMt3dmsSftConc_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.SetConcentrationLength(Length(BoundaryValues.Concentration));
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMt3dSftBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := THeadWaterMt3dSftReachCollection;
end;

function TMt3dSftBoundary.BoundaryObserverPrefix: string;
begin
  result := 'Sft';
end;

procedure TMt3dSftBoundary.ChangeSpeciesPosition(OldIndex, NewIndex: integer);
var
  SftItem: TCustomBoundaryItem;
begin
  inherited;
  Precipitation.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  Precipitation.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);

  RunOff.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  RunOff.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);

  ConstConc.ChangeSpeciesItemPosition(OldIndex, NewIndex);
  ConstConc.ChangeSpeciesTimeListPosition(OldIndex, NewIndex);

  SftItem := FInitialConcentration.Items[OldIndex];
  SftItem.Index := NewIndex;

  SftItem := FDispersionCoefficient.Items[OldIndex];
  SftItem.Index := NewIndex;

end;

procedure TMt3dSftBoundary.Clear;
begin
  inherited;
  FPrecipitation.Clear;
  FRunOff.Clear;
  FConstConc.Clear;

  FInitialConcentration.Clear;
  FDispersionCoefficient.Clear;
end;

procedure TMt3dSftBoundary.ClearBoundaries(AModel: TBaseModel);
begin
  inherited;
  Precipitation.ClearBoundaries(AModel);
  RunOff.ClearBoundaries(AModel);
  ConstConc.ClearBoundaries(AModel);

end;

constructor TMt3dSftBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
//  CreateFormulaObjects;
  CreateBoundaryObserver;

  CreateObservers;

  FPrecipitation := TPrecipitationMt3dSftReachCollection.Create(Self, Model as TCustomModel, ScreenObject);
  FRunOff := TRunoffMt3dSftReachCollection.Create(Self, Model as TCustomModel, ScreenObject);
  FConstConc := TConstConcMt3dSftReachCollection.Create(Self, Model as TCustomModel, ScreenObject);

  FInitialConcentration := TMt3dSftInitConcCollection.Create(Self, Model as TCustomModel, ScreenObject);

  FDispersionCoefficient:= TMt3dSftDispCollection.Create(self, Model as TCustomModel,
    ScreenObject);

  FObsLocation := solNone;
end;


procedure TMt3dSftBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(HeadWaterObserver);
    FObserverList.Add(PrecipObserver);
    FObserverList.Add(RunoffObserver);
    FObserverList.Add(ConstConcObserver);
  end;
end;

procedure TMt3dSftBoundary.DeleteSpecies(SpeciesIndex: integer);
begin
  inherited;
  Precipitation.DeleteSpecies(SpeciesIndex);
  RunOff.DeleteSpecies(SpeciesIndex);
  ConstConc.DeleteSpecies(SpeciesIndex);

  FInitialConcentration.DeleteSpecies(SpeciesIndex);
  FDispersionCoefficient.DeleteSpecies(SpeciesIndex);
end;

destructor TMt3dSftBoundary.Destroy;
begin
//  InitialConcentration := '0';
//  DispersionCoefficient := '0';
  FPrecipitation.Free;
  FRunOff.Free;
  FConstConc.Free;


  FInitialConcentration.Free;
  FDispersionCoefficient.Free;
  inherited;
end;

procedure TMt3dSftBoundary.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  Precipitation.EvaluateArrayBoundaries(AModel, Writer);
  RunOff.EvaluateArrayBoundaries(AModel, Writer);
  ConstConc.EvaluateArrayBoundaries(AModel, Writer);
end;

procedure TMt3dSftBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TMt3dmsConcStorage;
begin
  FCurrentBoundaryType := sbtHeadwater;

  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;

  FCurrentBoundaryType := sbtPrecipitation;
  for ValueIndex := 0 to Precipitation.Count - 1 do
  begin
    if ValueIndex < Precipitation.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Precipitation.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;

  FCurrentBoundaryType := sbtRunoff;
  for ValueIndex := 0 to RunOff.Count - 1 do
  begin
    if ValueIndex < RunOff.BoundaryCount[AModel] then
    begin
      BoundaryStorage := RunOff.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;

  FCurrentBoundaryType := sbtConstConc;
  for ValueIndex := 0 to ConstConc.Count - 1 do
  begin
    if ValueIndex < ConstConc.BoundaryCount[AModel] then
    begin
      BoundaryStorage := ConstConc.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

procedure TMt3dSftBoundary.GetConstConcCells(PrecipTimeList: TList;
  AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TMt3dmsConcStorage;
begin
  for ValueIndex := 0 to ConstConc.Count - 1 do
  begin
    if ValueIndex < ConstConc.BoundaryCount[AModel] then
    begin
      BoundaryStorage := ConstConc.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignSftCells(BoundaryStorage, PrecipTimeList);
    end;
  end;
end;


function TMt3dSftBoundary.GetConstConcObserver: TObserver;
begin
  if FConstConcObserver = nil then
  begin
    CreateObserver('SFT_ConstConc', FConstConcObserver, nil);
  end;
  result := FConstConcObserver;
end;

function TMt3dSftBoundary.GetHeadWaterObserver: TObserver;
begin
  if FHeadWaterObserver = nil then
  begin
    CreateObserver('SFT_HeadWaterConc', FHeadWaterObserver, nil);
  end;
  result := FHeadWaterObserver;
end;

procedure TMt3dSftBoundary.GetPrecipCells(PrecipTimeList: TList;
  AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TMt3dmsConcStorage;
begin
  for ValueIndex := 0 to Precipitation.Count - 1 do
  begin
    if ValueIndex < Precipitation.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Precipitation.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignSftCells(BoundaryStorage, PrecipTimeList);
    end;
  end;
end;


function TMt3dSftBoundary.GetPrecipObserver: TObserver;
begin
  if FPrecipObserver = nil then
  begin
    CreateObserver('SFT_PrecipConc', FPrecipObserver, nil);
  end;
  result := FPrecipObserver;
end;

procedure TMt3dSftBoundary.GetRunOffCells(PrecipTimeList: TList;
  AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TMt3dmsConcStorage;
begin
  for ValueIndex := 0 to RunOff.Count - 1 do
  begin
    if ValueIndex < RunOff.BoundaryCount[AModel] then
    begin
      BoundaryStorage := RunOff.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignSftCells(BoundaryStorage, PrecipTimeList);
    end;
  end;
end;

function TMt3dSftBoundary.GetRunoffObserver: TObserver;
begin
  if FRunoffObserver = nil then
  begin
    CreateObserver('SFT_Runoff', FRunoffObserver, nil);
  end;
  result := FRunoffObserver;
end;

procedure TMt3dSftBoundary.InsertNewSpecies(SpeciesIndex: integer;
  const Name: string);
//var
//  SftItem: TCollectionItem;
begin
  inherited;
  Precipitation.InsertNewSpecies(SpeciesIndex, Name);
  RunOff.InsertNewSpecies(SpeciesIndex, Name);
  ConstConc.InsertNewSpecies(SpeciesIndex, Name);

  FInitialConcentration.InsertNewSpecies(SpeciesIndex, Name);
  FDispersionCoefficient.InsertNewSpecies(SpeciesIndex, Name);

//  SftItem := FInitialConcentration.Add;
//  SftItem.Index := SpeciesIndex;
//
//  SftItem := FDispersionCoefficient.Add;
//  SftItem.Index := SpeciesIndex;
end;

procedure TMt3dSftBoundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateSftHeadwatersConc(self);
    LocalModel.InvalidateSftPrecipConc(self);
    LocalModel.InvalidateSftRunoffConc(self);
    LocalModel.InvalidateSftConstConc(self);
  end;
end;

procedure TMt3dSftBoundary.Loaded;
begin
  FInitialConcentration.Loaded;
  FDispersionCoefficient.Loaded;
end;

procedure TMt3dSftBoundary.RenameSpecies(const OldSpeciesName,
  NewSpeciesName: string);
begin
  inherited;
  Precipitation.RenameTimeList(OldSpeciesName, NewSpeciesName);
  Precipitation.RenameItems(OldSpeciesName, NewSpeciesName);

  RunOff.RenameTimeList(OldSpeciesName, NewSpeciesName);
  RunOff.RenameItems(OldSpeciesName, NewSpeciesName);

  ConstConc.RenameTimeList(OldSpeciesName, NewSpeciesName);
  ConstConc.RenameItems(OldSpeciesName, NewSpeciesName);

end;

procedure TMt3dSftBoundary.SetConstConc(
  const Value: TConstConcMt3dSftReachCollection);
begin
  FConstConc.Assign(Value);
end;

procedure TMt3dSftBoundary.SetDispersionCoefficient(
  const Value: TMt3dSftDispCollection);
begin
  FDispersionCoefficient.Assign(Value);
end;

procedure TMt3dSftBoundary.SetInitialConcentration(
  const Value: TMt3dSftInitConcCollection);
begin
  FInitialConcentration.Assign(Value);
end;

procedure TMt3dSftBoundary.SetObsLocation(const Value: TSftObsLocation);
begin
  if FObsLocation <> Value then
  begin
    FObsLocation := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dSftBoundary.SetPrecipitation(
  const Value: TPrecipitationMt3dSftReachCollection);
begin
  FPrecipitation.Assign(Value);
end;

procedure TMt3dSftBoundary.SetRunOff(
  const Value: TRunoffMt3dSftReachCollection);
begin
  FRunOff.Assign(Value);
end;

function TMt3dSftBoundary.Used: boolean;
begin
  result := inherited Used
    or Precipitation.Used
    or RunOff.Used
    or ConstConc.Used
    or (InitialConcentration.Count > 0)
    or (DispersionCoefficient.Count > 0)
    or (ObsLocation <> solNone)
end;

{ TMt3dSftInitConcItem }

procedure TMt3dSftInitConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMt3dSftInitConcCollection;
  InitConcObserver: TObserver;
  ComponentIndex: Integer;
  DataSetName: string;
  DataArray: TDataArray;
begin
  if Model <> nil then
  begin
    ParentCollection := Collection as TMt3dSftInitConcCollection;
    for ComponentIndex := 0 to FObserverList.Count - 1 do
    begin
      InitConcObserver := FObserverList[ComponentIndex];
      InitConcObserver.OnUpToDateSet := ParentCollection.InvalidateSftInitConc;

      DataSetName := KSFTInitialConcentra + IntToStr(ComponentIndex+1);
      DataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(DataSetName);
      if DataArray <> nil then
      begin
        InitConcObserver.TalksTo(DataArray);
      end;
    end;
  end;
end;

function TCustomSftItem.GetMt3dmsConc(Index: integer): string;
var
  Observer: TObserver;
begin
  result := inherited;
  Observer := FObserverList[Index];
  if not Observer.UpToDate then
  begin
    Observer.UpToDate := True;
  end;
end;

function TCustomSftItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserverList[Index];
end;

procedure TCustomSftItem.Loaded;
var
  ComponentIndex: Integer;
  OldFormula: string;
begin
  inherited;
  AssignObserverEvents(Collection);
  // Update subscriptions.
  for ComponentIndex := 0 to FObserverList.Count - 1 do
  begin
    OldFormula := Mt3dmsConcRate[ComponentIndex];
    Mt3dmsConcRate[ComponentIndex] := '0';
    Mt3dmsConcRate[ComponentIndex] := OldFormula;
  end;
end;

procedure TCustomSftItem.SetMt3dmsConc(Index: integer; const Value: string);
var
  OldFormula: String;
  NewFormula: String;
  ParentModel: TPhastModel;
  Compiler: TRbwParser;
  Observer: TObserver;
begin
  OldFormula := GetMt3dmsConc(Index);
  inherited;
  NewFormula := Value;

  if (Model <> nil) and (OldFormula <> NewFormula) then
  begin
    ParentModel := Model as TPhastModel;
    Compiler := ParentModel.rpThreeDFormulaCompiler;

    Observer := FObserverList[Index];
    UpdateFormulaDependencies(OldFormula, NewFormula, Observer,
      Compiler);
  end;

end;


{ TMt3dSftInitConcTimeListLink }

procedure TMt3dSftInitConcTimeListLink.CreateTimeLists;
var
  Index: Integer;
  LocalModel: TPhastModel;
begin
  TimeLists.Clear;
  LocalModel := frmGoPhast.PhastModel;
  for Index := 0 to LocalModel.MobileComponents.Count - 1 do
  begin
    AddTimeList(nil);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
    AddTimeList(nil);
  end;
end;

{ TMt3dSftInitConcCollection }

class function TMt3dSftInitConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dSftInitConcTimeListLink;
end;

procedure TMt3dSftInitConcCollection.InvalidateSftInitConc(Sender: TObject);
var
  PhastModel: TPhastModel;
//  Link: TMt3dSftInitConcTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NCOMP: Integer;
  CompIndex: Integer;
  DataSetName: string;
  DataArray: TDataArray;
//  ScreenTopDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    NCOMP := PhastModel.NumberOfMt3dChemComponents;
    for CompIndex := 1 to NCOMP do
    begin
      DataSetName := KSFTInitialConcentra + IntToStr(CompIndex);
      DataArray := PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
      DataArray.Invalidate
    end;

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;

      if ChildModel <> nil then
      begin
        for CompIndex := 1 to NCOMP do
        begin
          DataSetName := KSFTInitialConcentra + IntToStr(CompIndex);
          DataArray := ChildModel.DataArrayManager.GetDataSetByName(DataSetName);
          DataArray.Invalidate
        end;
      end;
    end;

  end;
end;

class function TMt3dSftInitConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dSftInitConcItem;
end;

procedure TCustomSftSteadyCollection.Loaded;
var
  index: Integer;
begin
  inherited;
  for index := 0 to Count - 1 do
  begin
    (Items[Index] as TCustomSftItem).Loaded;
  end;
end;

function TCustomSftSteadyCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

{ TMt3dSftDispItem }

procedure TMt3dSftDispItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMt3dSftDispCollection;
  DispObserver: TObserver;
//  InitConcObserver: TObserver;
  ComponentIndex: Integer;
  DataSetName: string;
  DataArray: TDataArray;
begin
  if Model <> nil then
  begin
    ParentCollection := Collection as TMt3dSftDispCollection;
    for ComponentIndex := 0 to FObserverList.Count - 1 do
    begin
      DispObserver := FObserverList[ComponentIndex];
      DispObserver.OnUpToDateSet := ParentCollection.InvalidateSftDisp;

      DataSetName := KSFTDispersion + IntToStr(ComponentIndex+1);
      DataArray := (Model as TCustomModel).DataArrayManager.GetDataSetByName(DataSetName);
      if DataArray <> nil then
      begin
        DispObserver.TalksTo(DataArray);
      end;
    end;
  end;
end;

{ TMt3dSftDispTimeListLink }

procedure TMt3dSftDispTimeListLink.CreateTimeLists;
var
  Index: Integer;
//  Mt3dmsConcData: TModflowTimeList;
//  Item: TChemSpeciesItem;
  LocalModel: TPhastModel;
//  NCOMP: Integer;
begin
  TimeLists.Clear;
//  FListOfTimeLists.Clear;
  LocalModel := frmGoPhast.PhastModel;
//  NCOMP := LocalModel.NumberOfMt3dChemComponents;

  for Index := 0 to LocalModel.MobileComponents.Count - 1 do
  begin
//    Item := LocalModel.MobileComponents[Index];
//    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
//    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
//    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
//    if Model <> nil then
//    begin
//      Mt3dmsConcData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateMt3dmsChemSources;
//    end;
//    AddTimeList(Mt3dmsConcData);
//    FListOfTimeLists.Add(Mt3dmsConcData);
    AddTimeList(nil);
//    FListOfTimeLists.Add(nil);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
//    Item := LocalModel.ImmobileComponents[Index];
//    Mt3dmsConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
//    Mt3dmsConcData.NonParamDescription := Item.Name +  StrConcentration;
//    Mt3dmsConcData.ParamDescription := Item.Name +  StrConcentrationMulti;
//    if Model <> nil then
//    begin
//      Mt3dmsConcData.OnInvalidate :=
//        (Model as TCustomModel).InvalidateMt3dmsChemSources;
//    end;
//    AddTimeList(Mt3dmsConcData);
//    FListOfTimeLists.Add(Mt3dmsConcData);
    AddTimeList(nil);
//    FListOfTimeLists.Add(nil);
  end;
end;

{ TMt3dSftDispCollection }

class function TMt3dSftDispCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dSftDispTimeListLink;
end;

procedure TMt3dSftDispCollection.InvalidateSftDisp(Sender: TObject);
var
  PhastModel: TPhastModel;
//  Link: TMt3dSftDispTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NCOMP: Integer;
  CompIndex: Integer;
  DataSetName: string;
  DataArray: TDataArray;
//  ScreenTopDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    NCOMP := PhastModel.NumberOfMt3dChemComponents;
    for CompIndex := 1 to NCOMP do
    begin
      DataSetName := KSFTDispersion + IntToStr(CompIndex);
      DataArray := PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
      DataArray.Invalidate
    end;

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;

      if ChildModel <> nil then
      begin
        for CompIndex := 1 to NCOMP do
        begin
          DataSetName := KSFTDispersion + IntToStr(CompIndex);
          DataArray := ChildModel.DataArrayManager.GetDataSetByName(DataSetName);
          DataArray.Invalidate
        end;
      end;
    end;

  end;
end;

class function TMt3dSftDispCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dSftDispItem;
end;

//procedure TMt3dSftDispCollection.Loaded;
//var
//  index: Integer;
//begin
//  inherited;
//  for index := 0 to Count - 1 do
//  begin
//    (Items[Index] as TMt3dSftDispItem).Loaded;
//  end;
//end;

//function TMt3dSftDispCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
//begin
//  result := False;
//end;

{ TSftSteady }

constructor TSftSteady.Create;
begin
  inherited;
  FInitConcentrations := TList<Double>.Create;
  FDispersions := TList<Double>.Create;
  FInitConcentrationsAnnotations := TStringList.Create;
  FDispersionsAnnotations := TStringList.Create;
  FIsObservation := False;
end;

destructor TSftSteady.Destroy;
begin
  FInitConcentrations.Free;
  FDispersions.Free;
  FInitConcentrationsAnnotations.Free;
  FDispersionsAnnotations.Free;
  inherited;
end;

{ TMt3dmsSftConc_Cell }

procedure TMt3dmsSftConc_Cell.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  inherited;
  WriteCompInt(Comp, Ord(FBoundaryType));
  WriteCompInt(Comp, FReachNumber);

end;

procedure TMt3dmsSftConc_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FBoundaryType := TSftBoundaryType(ReadCompInt(Decomp));
  FReachNumber := ReadCompInt(Decomp);
end;

end.
