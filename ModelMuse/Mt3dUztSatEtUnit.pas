unit Mt3dUztSatEtUnit;

interface

uses
  Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes, IntListUnit,
  ModflowMnw2Unit, Mt3dmsChemUnit;

type
  // @name represents an MT3DMS concentration point source for one time interval.
  // @name is stored by @link(TMt3dmsConcCollection).
  TMt3dUztSatEtConcItem = class(TCustomMt3dmsConcItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure InvalidateModel; override;
  end;

  TMt3dUztSatEtConcTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TMt3dUztSatEtConcCollection = class(TCustomMt3dmsArrayConcCollection)
  private
    procedure InvalidateUztSatEtConc(Sender: TObject);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure InvalidateModel; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function ConcName: string; override;
  end;

  // @name defines the concentration of UZF evapotranspiration from the
  // unsaturated zone in MT3D-USGS.
  TMt3dUztSatEtConcBoundary = class(TCustomMt3dmsConcBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TMt3dmsConc_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TMt3dmsConcStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
  end;

implementation

uses
  Mt3dmsChemSpeciesUnit, PhastModelUnit, frmGoPhastUnit, ModflowTimeUnit,
  AbstractGridUnit, ScreenObjectUnit, ModflowPackagesUnit;

resourcestring
  StrSatEtConcentration = ' saturated ET concentration';
  StrSatEtConcentrationMulti = ' saturated ET concentration multiplier';

{ TMt3dUztSatEtConcItem }

procedure TMt3dUztSatEtConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMt3dUztSatEtConcCollection;
  ConcentrationObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TMt3dUztSatEtConcCollection;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    ConcentrationObserver := FObserverList[Index];
    ConcentrationObserver.OnUpToDateSet :=
      ParentCollection.InvalidateUztSatEtConc;
  end;
end;

procedure TMt3dUztSatEtConcItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateUztSatConc(self);
  end;
end;

{ TMt3dUztSatEtConcTimeListLink }

procedure TMt3dUztSatEtConcTimeListLink.CreateTimeLists;
var
  Index: Integer;
  Mt3dSatEtConcData: TModflowTimeList;
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
    Mt3dSatEtConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dSatEtConcData.NonParamDescription := Item.Name +  StrSatEtConcentration;
    Mt3dSatEtConcData.ParamDescription := Item.Name +  StrSatEtConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dSatEtConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateUztSatConc;
    end;
    AddTimeList(Mt3dSatEtConcData);
    ListOfTimeLists.Add(Mt3dSatEtConcData);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
    Item := LocalModel.ImmobileComponents[Index];
    Mt3dSatEtConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dSatEtConcData.NonParamDescription := Item.Name +  StrSatEtConcentration;
    Mt3dSatEtConcData.ParamDescription := Item.Name +  StrSatEtConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dSatEtConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateUztSatConc;
    end;
    AddTimeList(Mt3dSatEtConcData);
    ListOfTimeLists.Add(Mt3dSatEtConcData);
  end;
end;

{ TMt3dUztSatEtConcCollection }

function TMt3dUztSatEtConcCollection.ConcName: string;
begin
  result := StrSatEtConcentration;
end;

function TMt3dUztSatEtConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dUztSatEtConcTimeListLink;
end;

procedure TMt3dUztSatEtConcCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMt3dmsChemSources(self);
  end;
end;

procedure TMt3dUztSatEtConcCollection.InvalidateUztSatEtConc(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMt3dUztSatEtConcTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMt3dUztSatEtConcTimeListLink;
    for index := 0 to Link.TimeLists.Count - 1 do
    begin
      ATimeList := Link.TimeLists[index];
      ATimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMt3dUztSatEtConcTimeListLink;
      for index := 0 to Link.TimeLists.Count - 1 do
      begin
        ATimeList := Link.TimeLists[index];
        ATimeList.Invalidate;
      end;
    end;
  end;
end;

class function TMt3dUztSatEtConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dUztSatEtConcItem;
end;

{ TMt3dUztSatEtConcBoundary }

procedure TMt3dUztSatEtConcBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TMt3dmsConc_Cell;
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
      Cells := TValueCellList.Create(TMt3dmsConc_Cell);
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
        Cell := TMt3dmsConc_Cell.Create;
        Cells.Add(Cell);

        LocalModel.AdjustCellPosition(Cell);
        Cell.IFace := LocalScreenObject.IFace;
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        Cell.SetConcentrationLength(Length(Cell.Values.Concentration));
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMt3dUztSatEtConcBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMt3dUztSatEtConcCollection;
end;

procedure TMt3dUztSatEtConcBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TMt3dmsConcStorage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TMt3dmsConcStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

procedure TMt3dUztSatEtConcBoundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateUztSatConc(self);
  end;
end;

end.
