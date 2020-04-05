unit ModflowSfrUnit;

interface

uses Classes, RbwParser, RealListUnit, OrderedCollectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, ModflowSfrReachUnit, ModflowSfrChannelUnit, GoPhastTypes,
  ModflowSfrSegment, ModflowSfrUnsatSegment, ModflowSfrTable, ModflowSfrFlows,
  ModflowSfrEquationUnit, ModflowSfrParamIcalcUnit, PestObsUnit;

type
  TGageLocation = (glNone, glFirst, glLast, glAll);

  {@name tells how the external flow file is specified.
  ffcNone = no external flow file.
  ffcFileName = an external file created by the user.
  ffcSpecify = the data is stored in ModelMuse.
  }
  TFlowFileChoice = (ffcNone, ffcFileName, ffcSpecify);

  {For external flow files whose data is stored in ModelMuse, @name indicates
  what to use as the reference time.}
  TFlowFileReferenceTime = (ffrtModelMuseZero, ffrtStartOfModel);

  {When an external flow file has its data stored in ModelMuse,
  @name represents one line of the data.}
  TFlowFileItem = class(TPhastCollectionItem)
  private
    FTime: double;
    FInflow: double;
    procedure SetInflow(const Value: double);
    procedure SetTime(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Time: double read FTime write SetTime;
    property Inflow: double read FInflow write SetInflow;
  end;

  TFlowFileCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetItem(index: Integer): TFlowFileItem;
    procedure SetItem(index: Integer; const Value: TFlowFileItem);
  public
    function Add: TFlowFileItem;
    constructor Create(Model: TBaseModel);
    property Items[index: Integer]: TFlowFileItem read GetItem write SetItem; default;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TExternalFlowProperties = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FFlowFileData: TFlowFileCollection;
    FFullFlowFileName: string;
    FFlowFileChoice: TFlowFileChoice;
    FReferenceTimeChoice: TFlowFileReferenceTime;
    function GetReferenceTimeChoice: TFlowFileReferenceTime;
    function SaveFlowFileData: Boolean;
    procedure SetFlowFileChoice(const Value: TFlowFileChoice);
    procedure SetFlowFileData(const Value: TFlowFileCollection);
    procedure SetFullFlowFileName(const Value: string);
    procedure SetReferenceTimeChoice(const Value: TFlowFileReferenceTime);
    function GetFlowFileName: string;
    procedure SetFlowFileName(const Value: string);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Clear;
    property FullFlowFileName: string read FFullFlowFileName write SetFullFlowFileName;
  published
    property FlowFileChoice: TFlowFileChoice read FFlowFileChoice
      write SetFlowFileChoice;
    property ReferenceTimeChoice: TFlowFileReferenceTime
      read GetReferenceTimeChoice write SetReferenceTimeChoice;
    property FlowFileData: TFlowFileCollection read FFlowFileData
      write SetFlowFileData stored SaveFlowFileData;
    property FlowFileName: string read GetFlowFileName write SetFlowFileName;
  end;

  TSfrObs = class(TCustomTimeObservationItem)
  private
    FObsType: Integer;
    procedure SetObsType(const Value: Integer);
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function ObservationType: string; override;
    function Units: string; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ObsType: Integer read FObsType write SetObsType stored True;
    property GUID;
  end;

  TSfrObservations = class(TCustomComparisonCollection)
  private
    FGageOutputName: string;
    function GetSfrItem(Index: Integer): TSfrObs;
    procedure SetSfrItem(Index: Integer; const Value: TSfrObs);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TSfrObs read GetSfrItem
      write SetSfrItem; default;
    function Add: TSfrObs;
    property GageOutputName: string read FGageOutputName write FGageOutputName;
  end;

  // @name represents the MODFLOW Stream Flow Routing boundaries associated with
  // a single @link(TScreenObject).
  //
  // @link(TModflowBoundary.Values) is a @link(TSfrCollection)
  // and represents the stream properties at each reach.
  //
  // @seealso(TSfrCollection)
  TSfrBoundary = class(TModflowBoundary)
  private
    FSegmentNumber: integer;
    FChannelValues: TSfrChannelCollection;
    FUpstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FUpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FTableCollection: TSfrTableCollection;
    FSegmentFlows: TSfrSegmentFlowCollection;
    FEquationValues: TSfrEquationCollection;
    FIFSROPT: integer;
    FParamIcalc: TSfrParamIcalcCollection;
    FGage0: boolean;
    FGage1: boolean;
    FGage2: boolean;
    FGage3: boolean;
    FGage5: boolean;
    FGage6: boolean;
    FGage7: boolean;
    FGageLocation: TGageLocation;
    FExternalFlow: TExternalFlowProperties;
    FObservations: TSfrObservations;
    procedure SetSegmentNumber(const Value: integer);
    procedure SetChannelValues(const Value: TSfrChannelCollection);
    procedure SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetUpstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetTableCollection(const Value: TSfrTableCollection);
    procedure SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
    procedure SetEquationValues(const Value: TSfrEquationCollection);
    function GetISFROPT: integer;
    procedure SetParamIcalc(const Value: TSfrParamIcalcCollection);
    procedure InvalidateDisplayTimeLists;
    procedure SetGage0(const Value: boolean);
    procedure SetGage1(const Value: boolean);
    procedure SetGage2(const Value: boolean);
    procedure SetGage5(const Value: boolean);
    procedure SetGage6(const Value: boolean);
    procedure SetGage7(const Value: boolean);
    procedure SetGage3(const Value: boolean);
    function GetOutTypes: TByteSet;
    procedure SetGageLocation(const Value: TGageLocation);
    procedure SetExternalFlow(const Value: TExternalFlowProperties);
    procedure SetObservations(const Value: TSfrObservations);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TSfr_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure InvalidateSegmentNumberArray;
    procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // @link(TSfrStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW SFR parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TSfrStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel); override;
    function Used: boolean; override;
    property ISFROPT: integer read GetISFROPT write FIFSROPT;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    procedure Clear; override;
    property OutTypes: TByteSet read GetOutTypes;
    procedure FixCollections;
  published
    // @name was mispelled. It is now @link(SegmentNumber).
    property SegementNumber: integer read FSegmentNumber
      write SetSegmentNumber stored False;
    property SegmentNumber: integer read FSegmentNumber
      write SetSegmentNumber;
    property ChannelValues: TSfrChannelCollection read FChannelValues
      write SetChannelValues;
    property UpstreamSegmentValues: TSfrSegmentCollection
      read FUpstreamSegmentValues write SetUpstreamSegmentValues;
    property DownstreamSegmentValues: TSfrSegmentCollection
      read FDownstreamSegmentValues write SetDownstreamSegmentValues;
    property UpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FUpstreamUnsatSegmentValues write SetUpstreamUnsatSegmentValues;
    property DownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FDownstreamUnsatSegmentValues write SetDownstreamUnsatSegmentValues;
    property TableCollection: TSfrTableCollection read FTableCollection
      write SetTableCollection;
    property SegmentFlows: TSfrSegmentFlowCollection read FSegmentFlows
      write SetSegmentFlows;
    property EquationValues: TSfrEquationCollection read FEquationValues
      write SetEquationValues;
    property ParamIcalc: TSfrParamIcalcCollection read FParamIcalc
      write SetParamIcalc;
    property Gage0: boolean read FGage0 write SetGage0;
    property Gage1: boolean read FGage1 write SetGage1;
    property Gage2: boolean read FGage2 write SetGage2;
    property Gage3: boolean read FGage3 write SetGage3;
    property Gage5: boolean read FGage5 write SetGage5;
    property Gage6: boolean read FGage6 write SetGage6;
    property Gage7: boolean read FGage7 write SetGage7;
    property GageLocation: TGageLocation read FGageLocation
      write SetGageLocation;
    property ExternalFlow: TExternalFlowProperties read FExternalFlow
      write SetExternalFlow;
    property Observations: TSfrObservations read FObservations write SetObservations
    {$IFNDEF PEST}
      stored False
    {$ENDIF}
      ;
  end;

resourcestring
  StrIncompleteSFRData = 'Incomplete SFR data';

var
  StreamGageOutputTypes: TStringList;
  StreamGageOutputTypeUnits: TStringList;

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  SysUtils;

{ TSfrBoundary }

procedure TSfrBoundary.Assign(Source: TPersistent);
var
  Sfr: TSfrBoundary;
begin
  if Source is TSfrBoundary then
  begin
    Sfr := TSfrBoundary(Source);
    if Used <> Sfr.Used then
    begin
      if (ParentModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;
    ISFROPT := Sfr.ISFROPT;
    SegmentNumber := Sfr.SegmentNumber;

    ChannelValues := Sfr.ChannelValues;
    UpstreamSegmentValues := Sfr.UpstreamSegmentValues;
    DownstreamSegmentValues := Sfr.DownstreamSegmentValues;
    UpstreamUnsatSegmentValues := Sfr.UpstreamUnsatSegmentValues;
    DownstreamUnsatSegmentValues := Sfr.DownstreamUnsatSegmentValues;
    TableCollection := Sfr.TableCollection;
    SegmentFlows := Sfr.SegmentFlows;
    EquationValues := Sfr.EquationValues;
    ParamIcalc := Sfr.ParamIcalc;
    Gage0 := Sfr.Gage0;
    Gage1 := Sfr.Gage1;
    Gage2 := Sfr.Gage2;
    Gage3 := Sfr.Gage3;
    Gage5 := Sfr.Gage5;
    Gage6 := Sfr.Gage6;
    Gage7 := Sfr.Gage7;
    GageLocation := Sfr.GageLocation;
    ExternalFlow := Sfr.ExternalFlow;
    Observations := Sfr.Observations;
  end;
  inherited;
end;

procedure TSfrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TSfr_Cell;
  BoundaryValues: TSfrRecord;
  BoundaryIndex: Integer;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSfrStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSfrStorage;
  TimeIndex := 0;
  if TimeIndex < ValueTimeList.Count then
  begin
    Cells := ValueTimeList[TimeIndex];
  end
  else
  begin
    Cells := TValueCellList.Create(TSfr_Cell);
    ValueTimeList.Add(Cells);
  end;

  if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.SfrArray) then
  begin
    Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.SfrArray)
  end;
//  Cells.CheckRestore;
  for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SfrArray) - 1 do
  begin
    BoundaryValues := LocalBoundaryStorage.SfrArray[BoundaryIndex];
    Cell := TSfr_Cell.Create;
    Assert(ScreenObject <> nil);
    Cell.IFace := (ScreenObject as TScreenObject).IFace;
    Cells.Add(Cell);
    Cell.StressPeriod := TimeIndex;
    Cell.Values := BoundaryValues;
    Cell.ScreenObject := ScreenObject;
    LocalModel.AdjustCellPosition(Cell);
  end;
  Cells.Cache;
  LocalBoundaryStorage.CacheData;
end;

class function TSfrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSfrCollection;
end;

procedure TSfrBoundary.Clear;
begin
  inherited;
  ChannelValues.Clear;
  UpstreamSegmentValues.Clear;
  DownstreamSegmentValues.Clear;
  UpstreamUnsatSegmentValues.Clear;
  DownstreamUnsatSegmentValues.Clear;
  TableCollection.Clear;
  SegmentFlows.Clear;
  EquationValues.Clear;
  ParamIcalc.Clear;
end;

constructor TSfrBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.Invalidate;
  end;
  inherited;
  if Model <> nil then
  begin
    ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end;
  FParamIcalc := TSfrParamIcalcCollection.Create(self, Model, ScreenObject);
  FChannelValues := TSfrChannelCollection.Create(self, Model, ScreenObject);
  FUpstreamSegmentValues := TSfrSegmentCollection.Create(self, Model, ScreenObject);
  FUpstreamSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamSegmentValues := TSfrSegmentCollection.Create(self, Model, ScreenObject);
  FDownstreamSegmentValues.AssignmentLocation := alLastVertex;
  FUpstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model, ScreenObject);
  FUpstreamUnsatSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model, ScreenObject);
  FDownstreamUnsatSegmentValues.AssignmentLocation := alLastVertex;
  FTableCollection := TSfrTableCollection.Create(self, Model, ScreenObject);
  FSegmentFlows := TSfrSegmentFlowCollection.Create(self, Model, ScreenObject);
  FEquationValues := TSfrEquationCollection.Create(self, Model, ScreenObject);
  FExternalFlow := TExternalFlowProperties.Create(Model);

  FObservations := TSfrObservations.Create(OnInvalidateModelEvent, ScreenObject);
end;

destructor TSfrBoundary.Destroy;
begin
  FreeAndNil(FObservations);
  FreeAndNil(FExternalFlow);
  FreeAndNil(FEquationValues);
  FreeAndNil(FSegmentFlows);
  FreeAndNil(FTableCollection);
  FreeAndNil(FDownstreamUnsatSegmentValues);
  FreeAndNil(FUpstreamUnsatSegmentValues);
  FreeAndNil(FDownstreamSegmentValues);
  FreeAndNil(FUpstreamSegmentValues);
  FreeAndNil(FChannelValues);
  FreeAndNil(FParamIcalc);
  inherited;
end;

procedure TSfrBoundary.EvaluateArrayBoundaries(AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  Item: TCustomModflowBoundaryItem;
begin
  inherited;
  ChannelValues.EvaluateBoundaries(AModel);
  UpstreamSegmentValues.EvaluateArrayBoundaries(AModel);
  DownstreamSegmentValues.EvaluateArrayBoundaries(AModel);
  LocalModel := AModel as TCustomModel;
  if (UpstreamUnsatSegmentValues.Count > 0)
    or (DownstreamUnsatSegmentValues.Count > 0) then
  begin
    FirstUsedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
    LastUsedTime := LocalModel.ModflowFullStressPeriods[
      LocalModel.ModflowFullStressPeriods.Count - 1].EndTime;
    if UpstreamUnsatSegmentValues.Count > 0 then
    begin
      Item := UpstreamUnsatSegmentValues[0] as TCustomModflowBoundaryItem;
      Item.StartTime := FirstUsedTime;
      Item.EndTime := LastUsedTime;
    end;
    if DownstreamUnsatSegmentValues.Count > 0 then
    begin
      Item := DownstreamUnsatSegmentValues[0] as TCustomModflowBoundaryItem;
      Item.StartTime := FirstUsedTime;
      Item.EndTime := LastUsedTime;
    end;
  end;
  UpstreamUnsatSegmentValues.EvaluateArrayBoundaries(AModel);
  DownstreamUnsatSegmentValues.EvaluateArrayBoundaries(AModel);

  EquationValues.EvaluateBoundaries;
  TableCollection.EvaluateBoundaries;
  SegmentFlows.EvaluateBoundaries;
end;

procedure TSfrBoundary.FixCollections;
var
  ItemIndex: Integer;
begin
  if ChannelValues.Count >
    ParamIcalc.Count then
  begin
    while ChannelValues.Count
      > ParamIcalc.Count do
    begin
      ChannelValues.Last.Free;
    end;
    for ItemIndex := 0 to ChannelValues.Count - 1 do
    begin
      ChannelValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (ChannelValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if UpstreamSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while UpstreamSegmentValues.Count
      > ParamIcalc.Count do
    begin
      UpstreamSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to UpstreamSegmentValues.Count - 1 do
    begin
      UpstreamSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (UpstreamSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if DownstreamSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while DownstreamSegmentValues.Count
      > ParamIcalc.Count do
    begin
      DownstreamSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to DownstreamSegmentValues.Count - 1 do
    begin
      DownstreamSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (DownstreamSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if UpstreamUnsatSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while UpstreamUnsatSegmentValues.Count
      > ParamIcalc.Count do
    begin
      UpstreamUnsatSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to UpstreamUnsatSegmentValues.Count - 1 do
    begin
      UpstreamUnsatSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (UpstreamUnsatSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if DownstreamUnsatSegmentValues.Count >
    ParamIcalc.Count then
  begin
    while DownstreamUnsatSegmentValues.Count
      > ParamIcalc.Count do
    begin
      DownstreamUnsatSegmentValues.Last.Free;
    end;
    for ItemIndex := 0 to DownstreamUnsatSegmentValues.Count - 1 do
    begin
      DownstreamUnsatSegmentValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (DownstreamUnsatSegmentValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if SegmentFlows.Count >
    ParamIcalc.Count then
  begin
    while SegmentFlows.Count
      > ParamIcalc.Count do
    begin
      SegmentFlows.Last.Free;
    end;
    for ItemIndex := 0 to SegmentFlows.Count - 1 do
    begin
      SegmentFlows[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (SegmentFlows[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
  if EquationValues.Count >
    ParamIcalc.Count then
  begin
    while EquationValues.Count
      > ParamIcalc.Count do
    begin
      EquationValues.Last.Free;
    end;
    for ItemIndex := 0 to EquationValues.Count - 1 do
    begin
      EquationValues[ItemIndex].StartTime :=
        ParamIcalc[ItemIndex].StartTime;
      (EquationValues[ItemIndex] as TCustomModflowBoundaryItem).EndTime :=
        (ParamIcalc[ItemIndex] as TCustomModflowBoundaryItem).EndTime;
    end;
  end;
end;

procedure TSfrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TSfrStorage;
begin
  EvaluateArrayBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSfrStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TSfrBoundary.GetISFROPT: integer;
begin
  if ParentModel <> nil then
  begin
    result := (ParentModel as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end
  else
  begin
    Result := FIFSROPT;
  end;
end;

function TSfrBoundary.GetOutTypes: TByteSet;
begin
  result := [];
  if Gage0 then
  begin
    Include(result, 0);
  end;
  if Gage1 then
  begin
    Include(result, 1);
    Exclude(result, 0);
  end;
  if Gage2 then
  begin
    Include(result, 2);
    Exclude(result, 0);
  end;
  if Gage3 then
  begin
    Include(result, 3);
    Exclude(result, 0);
  end;
  if (result = [1,2,3])
    or ((Observations.Count > 0)  and (GageLocation <> glAll)) then
  begin
    result := [4];
  end;
  if Gage5 then
  begin
    Include(result, 5);
  end;
  if Gage6 then
  begin
    Include(result, 6);
  end;
  if Gage7 then
  begin
    Include(result, 7);
  end;
end;

procedure TSfrBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TSfrBoundary.SetChannelValues(const Value: TSfrChannelCollection);
begin
  FChannelValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamSegmentValues(
  const Value: TSfrSegmentCollection);
begin
  FDownstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FDownstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetEquationValues(const Value: TSfrEquationCollection);
begin
  FEquationValues.Assign(Value);
end;

procedure TSfrBoundary.SetExternalFlow(const Value: TExternalFlowProperties);
begin
  FExternalFlow.Assign(Value);
end;

procedure TSfrBoundary.SetGage0(const Value: boolean);
begin
  if FGage0 <> Value then
  begin
    InvalidateModel;
    FGage0 := Value;
  end;
end;

procedure TSfrBoundary.SetGage1(const Value: boolean);
begin
  if FGage1 <> Value then
  begin
    InvalidateModel;
    FGage1 := Value;
  end;
end;

procedure TSfrBoundary.SetGage2(const Value: boolean);
begin
  if FGage2 <> Value then
  begin
    InvalidateModel;
    FGage2 := Value;
  end;
end;

procedure TSfrBoundary.SetGage3(const Value: boolean);
begin
  if FGage3 <> Value then
  begin
    InvalidateModel;
    FGage3 := Value;
  end;
end;

procedure TSfrBoundary.SetGage5(const Value: boolean);
begin
  if FGage5 <> Value then
  begin
    InvalidateModel;
    FGage5 := Value;
  end;
end;

procedure TSfrBoundary.SetGage6(const Value: boolean);
begin
  if FGage6 <> Value then
  begin
    InvalidateModel;
    FGage6 := Value;
  end;
end;

procedure TSfrBoundary.SetGage7(const Value: boolean);
begin
  if FGage7 <> Value then
  begin
    InvalidateModel;
    FGage7 := Value;
  end;
end;

procedure TSfrBoundary.SetGageLocation(const Value: TGageLocation);
begin
  if FGageLocation <> Value then
  begin
    InvalidateModel;
    FGageLocation := Value;
  end;
end;

procedure TSfrBoundary.SetObservations(const Value: TSfrObservations);
begin
  FObservations.Assign(Value);
end;

procedure TSfrBoundary.SetParamIcalc(const Value: TSfrParamIcalcCollection);
begin
  FParamIcalc.Assign(Value);
end;

procedure TSfrBoundary.SetSegmentNumber(const Value: integer);
begin
  if FSegmentNumber <> Value then
  begin
    InvalidateModel;
    InvalidateSegmentNumberArray;
    FSegmentNumber := Value;
    if (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel
        and (ParentModel <> nil) then
    begin
      (ParentModel as TPhastModel).DischargeRoutingUpdate;
    end;
  end;
end;

procedure TSfrBoundary.SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
begin
  FSegmentFlows.Assign(Value);
end;

procedure TSfrBoundary.SetTableCollection(const Value: TSfrTableCollection);
begin
  FTableCollection.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
begin
  FUpstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FUpstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean; AModel: TBaseModel);
begin
  // it isn't clear whether or not inherited should be called.
  inherited;
  AddBoundaryTimes(ParamIcalc, Times, StartTestTime, EndTestTime,
    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(ChannelValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(UpstreamSegmentValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(DownstreamSegmentValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(TableCollection, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(SegmentFlows, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(EquationValues, Times, StartTestTime, EndTestTime,
//    StartRangeExtended, EndRangeExtended);

// The unsatured data is stored in a TCustomModflowBoundaryItem
// but the start and end times are not used.

//  AddBoundaryTimes(UpstreamUnsatSegmentValues, Times, StartTestTime,
//    EndTestTime, StartRangeExtended, EndRangeExtended);
//  AddBoundaryTimes(DownstreamUnsatSegmentValues, Times, StartTestTime,
//    EndTestTime, StartRangeExtended, EndRangeExtended);
end;

procedure TSfrBoundary.InvalidateDisplayTimeLists;
var
  Model: TPhastModel;
begin
  Model := ParentModel as TPhastModel;
  Model.InvalidateMfSfrSegmentReachAndIcalc(self);
  Model.InvalidateMfSfrIprior(self);
  Model.InvalidateMfSfrVerticalUnsatK(self);
  Model.InvalidateMfSfrReachLength(self);
  Model.InvalidateMfSfrStreamTop(self);
  Model.InvalidateMfSfrStreamSlope(self);
  Model.InvalidateMfSfrStreamThickness(self);
  Model.InvalidateMfSfrStreamK(self);
  Model.InvalidateMfSfrSaturatedWaterContent(self);
  Model.InvalidateMfSfrInitialWaterContent(self);
  Model.InvalidateMfSfrBrooksCorey(self);
  Model.InvalidateMfSfrFlow(self);
  Model.InvalidateMfSfrRunoff(self);
  Model.InvalidateMfSfrPrecipitation(self);
  Model.InvalidateMfSfrEvapotranspiration(self);
  Model.InvalidateMfSfrChannelRoughness(self);
  Model.InvalidateMfSfrBankRoughness(self);
  Model.InvalidateMfSfrDepthCoefficient(self);
  Model.InvalidateMfSfrDepthExponent(self);
  Model.InvalidateMfSfrWidthCoefficient(self);
  Model.InvalidateMfSfrWidthExponent(self);
  Model.InvalidateMfSfrUpstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrDownstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrUpstreamWidth(self);
  Model.InvalidateMfSfrDownstreamWidth(self);
  Model.InvalidateMfSfrUpstreamThickness(self);
  Model.InvalidateMfSfrDownstreamThickness(self);
  Model.InvalidateMfSfrUpstreamElevation(self);
  Model.InvalidateMfSfrDownstreamElevation(self);
  Model.InvalidateMfSfrUpstreamDepth(self);
  Model.InvalidateMfSfrDownstreamDepth(self);
  Model.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrUpstreamBrooksCorey(self);
  Model.InvalidateMfSfrDownstreamBrooksCorey(self);
  Model.InvalidateMfSfrUpstreamUnsatKz(self);
  Model.InvalidateMfSfrDownstreamUnsatKz(self);
end;

procedure TSfrBoundary.InvalidateSegmentNumberArray;
begin
  if (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel
    and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateMfSfrSegmentReachAndIcalc(self);
  end;
end;

function TSfrBoundary.Used: boolean;
var
  LocalISFROPT: integer;
begin
  LocalISFROPT := ISFROPT;
  result := inherited Used;
  if result then
  begin
    result := (LocalISFROPT in [1,2,3])
      or ChannelValues.Used
      or ((LocalISFROPT in [0,4,5]) and
        UpstreamSegmentValues.Used and DownstreamSegmentValues.Used)
      or ((LocalISFROPT in [4,5]) and
        UpstreamUnsatSegmentValues.Used and DownstreamUnsatSegmentValues.Used)
      or TableCollection.Used
      or SegmentFlows.Used
      or EquationValues.Used;
  end;
end;

{ TFlowFileItem }

procedure TFlowFileItem.Assign(Source: TPersistent);
var
  SourceItem: TFlowFileItem;
begin
  if Source is TFlowFileItem then
  begin
    SourceItem := TFlowFileItem(Source);
    Time := SourceItem.Time;
    Inflow := SourceItem.Inflow;
  end
  else
  begin
    inherited;
  end;
end;

procedure TFlowFileItem.SetInflow(const Value: double);
begin
  SetRealProperty(FInflow, Value);
end;

procedure TFlowFileItem.SetTime(const Value: double);
begin
  SetRealProperty(FTime, Value);
end;

{ TFlowFileCollection }

function TFlowFileCollection.Add: TFlowFileItem;
begin
  result := inherited Add as TFlowFileItem;
end;

constructor TFlowFileCollection.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TFlowFileItem, InvalidateModelEvent);
end;

function TFlowFileCollection.GetItem(index: Integer): TFlowFileItem;
begin
  result := inherited Items[Index] as TFlowFileItem;
end;

procedure TFlowFileCollection.SetItem(index: Integer;
  const Value: TFlowFileItem);
begin
  inherited Items[Index] := Value;
end;

{ TExternalFlowProperties }

procedure TExternalFlowProperties.Assign(Source: TPersistent);
var
  SourceProp: TExternalFlowProperties;
begin
  if Source is TExternalFlowProperties then
  begin
    SourceProp := TExternalFlowProperties(Source);
    FlowFileChoice := SourceProp.FlowFileChoice;
    ReferenceTimeChoice := SourceProp.ReferenceTimeChoice;
    FlowFileData := SourceProp.FlowFileData;
    FullFlowFileName := SourceProp.FullFlowFileName;
    FlowFileName := SourceProp.FlowFileName;
  end
  else
  begin
    inherited;
  end;
end;

procedure TExternalFlowProperties.Clear;
begin
  FFlowFileData.Clear;
  FFullFlowFileName := '';
  FFlowFileChoice := ffcNone;
  FReferenceTimeChoice := ffrtModelMuseZero;
end;

constructor TExternalFlowProperties.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.Invalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FFlowFileData := TFlowFileCollection.Create(Model);
  Clear;
end;

destructor TExternalFlowProperties.Destroy;
begin
  FFlowFileData.Free;
  inherited;
end;

//function TExternalFlowProperties.GetFlowFileChoice: TFlowFileChoice;
//begin
//  result := FFlowFileChoice;
//  case FFlowFileChoice of
//    ffcNone: result := FFlowFileChoice;
//    ffcFileName:
//      begin
//        if FlowFileName = '' then
//        begin
//          result := ffcNone;
//        end
//        else
//        begin
//          result := FFlowFileChoice;
//        end;
//      end;
//    ffcSpecify:
//      begin
//        if FFlowFileData.Count = 0 then
//        begin
//          result := ffcNone;
//        end
//        else
//        begin
//          result := FFlowFileChoice;
//        end;
//      end;
//    else Assert(False);
//  end;
//end;

function TExternalFlowProperties.GetFlowFileName: string;
var
  LocalModel: TPhastModel;
begin
  if Model = nil then
  begin
    result := FullFlowFileName;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    result := ExtractRelativePath(LocalModel.ModelFileName, FullFlowFileName);
  end;
end;

function TExternalFlowProperties.GetReferenceTimeChoice: TFlowFileReferenceTime;
begin
  if FFlowFileChoice = ffcFileName then
  begin
    Result := ffrtStartOfModel;
  end
  else
  begin
    Result := FReferenceTimeChoice;
  end;
end;

function TExternalFlowProperties.SaveFlowFileData: Boolean;
begin
  result := (FlowFileChoice = ffcSpecify) and (FFlowFileData.Count > 0);
end;

procedure TExternalFlowProperties.SetFlowFileChoice(
  const Value: TFlowFileChoice);
begin
  if FFlowFileChoice <> Value then
  begin
    InvalidateModel;
    FFlowFileChoice := Value;
  end;
end;

procedure TExternalFlowProperties.SetFlowFileData(
  const Value: TFlowFileCollection);
begin
  FFlowFileData.Assign(Value);
end;

procedure TExternalFlowProperties.SetFlowFileName(const Value: string);
var
  LocalModel: TPhastModel;
  CurDir: string;
begin
  if Model = nil then
  begin
//    FullFlowFileName := Value;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    CurDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(LocalModel.ModelFileName));
      FullFlowFileName := ExpandFileName(Value);
    finally
      SetCurrentDir(CurDir);
    end;
  end;
end;

procedure TExternalFlowProperties.SetFullFlowFileName(const Value: string);
begin
  if FFullFlowFileName <> Value then
  begin
    InvalidateModel;
    FFullFlowFileName := Value;
  end;
end;

procedure TExternalFlowProperties.SetReferenceTimeChoice(
  const Value: TFlowFileReferenceTime);
begin
  if FReferenceTimeChoice <> Value then
  begin
    InvalidateModel;
    FReferenceTimeChoice := Value;
  end;
end;

procedure InitializeStreamGageOutputTypes;
begin
  StreamGageOutputTypes := TStringList.Create;
  StreamGageOutputTypeUnits := TStringList.Create;
  StreamGageOutputTypes.Add('Stage');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Flow');        StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Depth');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Width');       StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Midpt-Flow');  StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Precip.');     StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('ET');          StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Runoff');      StreamGageOutputTypeUnits.Add('L3/T');
  StreamGageOutputTypes.Add('Conductance'); StreamGageOutputTypeUnits.Add('L2/T');
  StreamGageOutputTypes.Add('HeadDiff');    StreamGageOutputTypeUnits.Add('L');
  StreamGageOutputTypes.Add('Hyd.Grad.');   StreamGageOutputTypeUnits.Add('L/L');
  StreamGageOutputTypes.Add('GW_FLOW');     StreamGageOutputTypeUnits.Add('L3/T');
end;

{ TSfrObs }

procedure TSfrObs.Assign(Source: TPersistent);
begin
  if Source is TSfrObs then
  begin
    ObsType := TSfrObs(Source).ObsType;
  end;
  inherited;
end;

function TSfrObs.GetObsTypeIndex: Integer;
begin
  result := ObsType;
end;

function TSfrObs.ObservationType: string;
begin
  if (FObsType >= 0) and (FObsType < StreamGageOutputTypes.Count) then
  begin
    result := StreamGageOutputTypes[FObsType]
  end
  else
  begin
    result := inherited;
  end;
end;

procedure TSfrObs.SetObsType(const Value: Integer);
begin
  SetIntegerProperty(FObsType, Value);
end;

procedure TSfrObs.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := Value;
end;

function TSfrObs.Units: string;
begin
  if (FObsType >= 0) and (FObsType < StreamGageOutputTypeUnits.Count) then
  begin
    result := StreamGageOutputTypeUnits[FObsType]
  end
  else
  begin
    result := inherited;
  end;
end;

{ TSfrObservations }

function TSfrObservations.Add: TSfrObs;
begin
  result := inherited Add as TSfrObs;
end;

constructor TSfrObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TSfrObs, InvalidateModelEvent, ScreenObject);
end;

function TSfrObservations.GetSfrItem(Index: Integer): TSfrObs;
begin
  result := inherited Items[Index] as TSfrObs;
end;

procedure TSfrObservations.SetSfrItem(Index: Integer; const Value: TSfrObs);
begin
  inherited Items[Index] := Value;
end;

initialization
  InitializeStreamGageOutputTypes;

finalization
  StreamGageOutputTypes.Free;
  StreamGageOutputTypeUnits.Free;

end.

