unit Mt3dmsFluxObservationsUnit;

interface

uses
  GoPhastTypes, FluxObservationUnit, Classes, SysUtils, Generics.Collections;

type
  TMt3dmsFluxObservation = class(TCustomFluxObservationItem)
  private
    FSpecies: string;
    FWeight: double;
    FObservationFrequency: integer;
    FObservationType: TObservationType;
    procedure SetSpecies(const Value: string);
    procedure SetWeight(const Value: double);
    procedure SetObservationFrequency(const Value: integer);
    procedure SetObservationType(const Value: TObservationType);
  public
    // If Source is a @classname, @name copies the published properties
    // of @classname from the source.
    procedure Assign(Source: TPersistent); override;
  published
    property Species: string read FSpecies write SetSpecies;
    property Weight: double read FWeight write SetWeight;
    property ObservationType: TObservationType read FObservationType
      write SetObservationType;
    property ObservationFrequency: integer read FObservationFrequency
      write SetObservationFrequency;
  end;

  {@name is a collection of @link(TMt3dmsFluxObservation)s.}
  TMt3dmsFluxObservations = class(TCustomTFluxObservations)
  private
    // See @link(Items).
    function GetItems(Index: integer): TMt3dmsFluxObservation;
    // See @link(Items).
    procedure SetItems(Index: integer; const Value: TMt3dmsFluxObservation);
  public
    // If Source is a @classname, @name copies the contents
    // of @classname from the source.
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // @name provides read and write access to the @link(TMt3dmsFluxObservation)s
    // stored in @classname.
    property Items[Index: integer]: TMt3dmsFluxObservation read GetItems
      write SetItems; default;
    // @name adds a new @link(TMt3dmsFluxObservation) to the @link(Items).
    function Add: TMt3dmsFluxObservation;
  end;

  {@name indicates the type of flux observation}
  TMt3dmsFluxObsType = (mfotHead, mfotWell, mfotDrain, mfotRiver, mfotGHB,
    mfotRecharge, mfotEVT, mfotMassLoading, mfotSTR, mfotReservoir,
    mfotFHB_Head, mfotFHB_Flow, mfotIBS, mfotTransLeakage, mfotLake, mfotMNW1,
    mfotDRT, mfotETS);

  // @name defines one group of MT3DMS or MT3D-USGS flux observation.  Each group
  // includes the same flux cells and the same observation times.
  TMt3dmsFluxObservationGroup = class(TCustomFluxObservationGroup)
  private
    // See @link(ObservationTimes).
    FObservationTimes: TMt3dmsFluxObservations;
    // See @link(ObservationTimes).
    procedure SetObservationTimes(const Value: TMt3dmsFluxObservations);
  public
    // if Source is a @classname, name copies the published
    // properties of Source.
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // @name checks that the observations times are valid.
    // If some are not, a description of the errors will be
    // added to ErrorRoots and ErrorMessages
    procedure CheckObservationTimes(ErrorRoots, ErrorMessages: TStringList);
    function FluxObsType: TMt3dmsFluxObsType;
    function NumberOfCells(Model: TBaseModel): Integer;
  published
    // @name stores the observation times and observed fluxes at
    // those times.
    property ObservationTimes: TMt3dmsFluxObservations read FObservationTimes
      write SetObservationTimes;
  end;

  // @name is a collection of @link(TFluxObservationGroup)s.
  TMt3dmsFluxObservationGroups = class(TCustomFluxObservationGroups)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FFluxObservationType: TMt3dmsFluxObsType;
    // See @link(Items).
    function GetItems(Index: integer): TMt3dmsFluxObservationGroup;
    // See @link(Items).
    procedure SetItems(Index: integer; const Value: TMt3dmsFluxObservationGroup);
  public
    constructor Create(Model: TBaseModel);
   // @name provides read and write access to the @link(TMt3dmsFluxObservationGroup)
   // stored in the @classname.
    property Items[Index: integer]: TMt3dmsFluxObservationGroup read GetItems
      write SetItems; default;
    // @name adds a new @link(TMt3dmsFluxObservationGroup) to @link(Items).
    function Add: TMt3dmsFluxObservationGroup;
    property FluxObservationType: TMt3dmsFluxObsType read FFluxObservationType
      write FFluxObservationType;
    // @name calls @link(TMt3dmsFluxObservationGroup.CheckObservationTimes)
    // for each @link(TMt3dmsFluxObservationGroup) in @link(Items).
    procedure CheckObservationTimes(ErrorRoots, ErrorMessages: TStringList);
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TMt3dFluxGroupList = TList<TMt3dmsFluxObservationGroups>;

  function FluxObsTypeTo_issType(FluxObsType: TMt3dmsFluxObsType): integer;

implementation

uses
  ModflowBoundaryUnit, ScreenObjectUnit, Mt3dmsChemUnit, PhastModelUnit;

resourcestring
  StrInvalidTobObservation = 'Invalid transport observation time in the'
    + ' TOB observation: "%s."';
  StrTobIsNotUsedIn = '%0:g is not used in %1:s in the %2:s package';
  StrInvalidObjectsInclTob = 'Invalid objects included in the TOB transport'
    + ' observation "%s."';
  StrTheTobPackageIs = 'The %0:s  package is not used in %1:s.';

function FluxObsTypeTo_issType(FluxObsType: TMt3dmsFluxObsType): integer;
begin
  result := -999;
  case FluxObsType of
    mfotHead:
      begin
        result := ISSTYPE_CHD;
      end;
    mfotWell:
      begin
        result := ISSTYPE_WEL;
      end;
    mfotDrain:
      begin
        result := ISSTYPE_DRN;
      end;
    mfotRiver:
      begin
        result := ISSTYPE_RIV;
      end;
    mfotGHB:
      begin
        result := ISSTYPE_GHB;
      end;
    mfotRecharge:
      begin
        result := ISSTYPE_RCH;
      end;
    mfotEVT:
      begin
        result := ISSTYPE_EVT;
      end;
    mfotMassLoading:
      begin
        result := ISSYTPE_Mass;
      end;
    mfotSTR:
      begin
        result := ISSTYPE_STR;
      end;
    mfotReservoir:
      begin
        result := ISSTYPE_RES;
      end;
    mfotFHB_Head:
      begin
        result := ISSTYPE_CHD;
      end;
    mfotFHB_Flow:
      begin
        result := ISSTYPE_FHB;
      end;
    mfotIBS:
      begin
        result := ISSTYPE_IBS;
      end;
    mfotTransLeakage:
      begin
        result := ISSTYPE_TLK;
      end;
    mfotLake:
      begin
        result := ISSTYPE_LAK;
      end;
    mfotMNW1:
      begin
        result := ISSTYPE_MNW;
      end;
    mfotDRT:
      begin
        result := ISSTYPE_DRT;
      end;
    mfotETS:
      begin
        result := ISSTYPE_ETS;
      end;
    else Assert(False);
  end;
end;

{ TMt3dmsFluxObservation }

procedure TMt3dmsFluxObservation.Assign(Source: TPersistent);
var
  FluxObs: TMt3dmsFluxObservation;
begin
  if Source is TMt3dmsFluxObservation then
  begin
    FluxObs := TMt3dmsFluxObservation(Source);
    Species := FluxObs.Species;
    Weight := FluxObs.Weight;
    ObservationType := FluxObs.ObservationType;
    ObservationFrequency := FluxObs.ObservationFrequency;
  end;
  inherited;
end;

procedure TMt3dmsFluxObservation.SetObservationFrequency(const Value: integer);
begin
  SetIntegerProperty(FObservationFrequency, Value);
end;

procedure TMt3dmsFluxObservation.SetObservationType(
  const Value: TObservationType);
begin
  if FObservationType <> Value then
  begin
    InvalidateModel;
    FObservationType := Value;
  end;
end;

procedure TMt3dmsFluxObservation.SetSpecies(const Value: string);
begin
  if FSpecies <> Value then
  begin
    InvalidateModel;
    FSpecies := Value;
  end;
end;

procedure TMt3dmsFluxObservation.SetWeight(const Value: double);
begin
  if FWeight <> Value then
  begin
    InvalidateModel;
    FWeight := Value;
  end;
end;

{ TMt3dmsFluxObservations }

function TMt3dmsFluxObservations.Add: TMt3dmsFluxObservation;
begin
  result := inherited Add as TMt3dmsFluxObservation
end;

constructor TMt3dmsFluxObservations.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TMt3dmsFluxObservation, InvalidateModelEvent);
end;

function TMt3dmsFluxObservations.GetItems(
  Index: integer): TMt3dmsFluxObservation;
begin
  result := inherited Items[Index] as TMt3dmsFluxObservation;
end;

procedure TMt3dmsFluxObservations.SetItems(Index: integer;
  const Value: TMt3dmsFluxObservation);
begin
  inherited Items[Index] := Value;
end;

{ TMt3dmsFluxObservationGroup }

procedure TMt3dmsFluxObservationGroup.Assign(Source: TPersistent);
var
  SourceItem: TMt3dmsFluxObservationGroup;
begin
  if Source is TMt3dmsFluxObservationGroup then
  begin
    SourceItem := TMt3dmsFluxObservationGroup(Source);
    ObservationTimes := SourceItem.ObservationTimes;
  end;
  inherited;
end;

procedure TMt3dmsFluxObservationGroup.CheckObservationTimes(ErrorRoots,
  ErrorMessages: TStringList);
  function TimeUsedInBoundary(Time: double; Boundary:TModflowBoundary): boolean;
  var
    Item: TCustomModflowBoundaryItem;
    ParamIndex: Integer;
    Param: TModflowParamItem;
    ItemIndex: Integer;
    ParamBoundary: TModflowParamBoundary;
  begin
    result := False;
    for ItemIndex := 0 to Boundary.Values.Count - 1 do
    begin
      Item := Boundary.Values[ItemIndex] as TCustomModflowBoundaryItem;
      result := (Item.StartTime <= Time) and (Time <= Item.EndTime);
      if result then Exit;
    end;
    if Boundary is TModflowParamBoundary then
    begin
      ParamBoundary := TModflowParamBoundary(Boundary);
      for ParamIndex := 0 to ParamBoundary.Parameters.Count - 1 do
      begin
        Param := ParamBoundary.Parameters[ParamIndex];
        for ItemIndex := 0 to Param.Param.Count - 1 do
        begin
          Item := Param.Param.Items[ItemIndex] as TCustomModflowBoundaryItem;
          result := (Item.StartTime <= Time) and (Time <= Item.EndTime);
          if result then Exit;
        end;
      end;
    end;
  end;
var
  ObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
  TimeIndex: Integer;
  Time: Double;
  ErrorRoot: string;
  ErrorMessage: string;
  BoundaryPackageID: string;
begin
  for ObjectIndex := 0 to ObservationFactors.Count - 1 do
  begin
    ScreenObject :=
      ObservationFactors[ObjectIndex].ScreenObject as TScreenObject;
    Boundary := nil;
    case FluxObsType of
      mfotHead:
        begin
          Boundary := ScreenObject.ModflowChdBoundary;
          BoundaryPackageID := 'CHD';
        end;
      mfotWell:
        begin
          Boundary := ScreenObject.ModflowWellBoundary;
          BoundaryPackageID := 'WEL';
        end;
      mfotDrain:
        begin
          Boundary := ScreenObject.ModflowDrnBoundary;
          BoundaryPackageID := 'DRN';
        end;
      mfotRiver:
        begin
          Boundary := ScreenObject.ModflowRivBoundary;
          BoundaryPackageID := 'RIV';
        end;
      mfotGHB:
        begin
          Boundary := ScreenObject.ModflowGhbBoundary;
          BoundaryPackageID := 'GHB';
        end;
      mfotRecharge:
        begin
          Boundary := ScreenObject.ModflowRchBoundary;
          BoundaryPackageID := 'RCH';
        end;
      mfotEVT:
        begin
          Boundary := ScreenObject.ModflowEvtBoundary;
          BoundaryPackageID := 'EVT';
        end;
      mfotMassLoading:
        begin
          Boundary := ScreenObject.Mt3dmsConcBoundary;
          if (Boundary <> nil) and not ScreenObject.Mt3dmsConcBoundary.MassLoadingBoundary then
          begin
            Boundary := nil;
          end;
          BoundaryPackageID := 'SSM';
        end;
      mfotSTR:
        begin
          Boundary := ScreenObject.ModflowStrBoundary;
          BoundaryPackageID := 'STR';
        end;
      mfotReservoir:
        begin
          Boundary := ScreenObject.ModflowResBoundary;
          BoundaryPackageID := 'RES';
        end;
      mfotFHB_Head:
        begin
          Boundary := ScreenObject.ModflowFhbHeadBoundary;
          BoundaryPackageID := 'FHB';
        end;
      mfotFHB_Flow:
        begin
          Boundary := ScreenObject.ModflowFhbFlowBoundary;
          BoundaryPackageID := 'FHB';
        end;
      mfotIBS:
        begin
          Assert(False);
        end;
      mfotTransLeakage:
        begin
          Assert(False);
        end;
      mfotLake:
        begin
          Boundary := ScreenObject.ModflowLakBoundary;
          BoundaryPackageID := 'LAK';
        end;
      mfotMNW1:
        begin
          Assert(False);
        end;
      mfotDRT:
        begin
          Boundary := ScreenObject.ModflowDrtBoundary;
          BoundaryPackageID := 'DRT';
        end;
      mfotETS:
        begin
          Boundary := ScreenObject.ModflowEtsBoundary;
          BoundaryPackageID := 'ETS';
        end;
      else Assert(False);
    end;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for TimeIndex := 0 to ObservationTimes.Count - 1 do
      begin
        Time := ObservationTimes[TimeIndex].Time;
        if not TimeUsedInBoundary(Time, Boundary) then
        begin
          ErrorRoot := Format(StrInvalidTobObservation,
            [ObservationName]);
          ErrorMessage := Format(StrTobIsNotUsedIn,
            [Time, ScreenObject.Name, BoundaryPackageID]);
          ErrorRoots.Add(ErrorRoot);
          ErrorMessages.AddObject(ErrorMessage, ScreenObject);
        end;
      end;
    end
    else
    begin
      ErrorRoot := Format(StrInvalidObjectsInclTob, [ObservationName]);
      ErrorMessage := Format(StrTheTobPackageIs,
        [BoundaryPackageID, ScreenObject.Name]);
      ErrorRoots.Add(ErrorRoot);
      ErrorMessages.AddObject(ErrorMessage, ScreenObject);
    end;
  end;

end;

constructor TMt3dmsFluxObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObservationTimes := TMt3dmsFluxObservations.Create(OnInvalidateModel);
end;

destructor TMt3dmsFluxObservationGroup.Destroy;
begin
  FObservationTimes.Free;
  inherited;
end;

function TMt3dmsFluxObservationGroup.FluxObsType: TMt3dmsFluxObsType;
begin
  result := (Collection as TMt3dmsFluxObservationGroups).FluxObservationType;
end;

function TMt3dmsFluxObservationGroup.NumberOfCells(Model: TBaseModel): Integer;
var
  ObjectIndex: Integer;
  ObsFactor: TObservationFactor;
  ScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  LocalModel: TCustomModel;
begin
  result := 0;
  LocalModel := Model as TCustomModel;
  for ObjectIndex := 0 to ObservationFactors.Count - 1 do
  begin
    ObsFactor := ObservationFactors[ObjectIndex];
    ScreenObject := ObsFactor.ScreenObject as TScreenObject;
    Assert(ScreenObject <> nil);
    CellList := TCellAssignmentList.Create;
    try
      ScreenObject.GetCellsToAssign({LocalModel.Grid,} '0', nil, nil, CellList, alAll, LocalModel);
      result := result + CellList.Count;
    finally
      CellList.Free;
    end;
  end;
end;

procedure TMt3dmsFluxObservationGroup.SetObservationTimes(
  const Value: TMt3dmsFluxObservations);
begin
  FObservationTimes.Assign(Value);
end;

{ TMt3dmsFluxObservationGroups }

function TMt3dmsFluxObservationGroups.Add: TMt3dmsFluxObservationGroup;
begin
  result := inherited Add as TMt3dmsFluxObservationGroup;
end;

procedure TMt3dmsFluxObservationGroups.CheckObservationTimes(ErrorRoots,
  ErrorMessages: TStringList);
var
  Index: integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].CheckObservationTimes(ErrorRoots, ErrorMessages);
  end;
end;

constructor TMt3dmsFluxObservationGroups.Create(Model: TBaseModel);
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
  inherited Create(TMt3dmsFluxObservationGroup, Model);
end;

function TMt3dmsFluxObservationGroups.GetItems(
  Index: integer): TMt3dmsFluxObservationGroup;
begin
  result := inherited Items[Index] as TMt3dmsFluxObservationGroup;
end;

procedure TMt3dmsFluxObservationGroups.SetItems(Index: integer;
  const Value: TMt3dmsFluxObservationGroup);
begin
  inherited Items[Index] := Value;
end;

end.
