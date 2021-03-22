unit Mt3dUztRchUnit;

interface

uses
  Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes, IntListUnit,
  ModflowMnw2Unit, Mt3dmsChemUnit;

type
  // @name represents an MT3DMS concentration point source for one time interval.
  // @name is stored by @link(TMt3dmsConcCollection).
  TMt3dUztRchConcItem = class(TCustomMt3dmsConcItem)
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure InvalidateModel; override;
  end;

  TMt3dUztRchConcTimeListLink = class(TCustomMt3dmsConcTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TMt3dUztRchConcCollection = class(TCustomMt3dmsArrayConcCollection)
  private
    procedure InvalidateUztRechConc(Sender: TObject);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure InvalidateModel; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function ConcName: string; override;
  end;

  // @name defines the concentration of UZF infiltration from the
  // unsaturated zone in MT3D-USGS.
  TMt3dUztRchConcBoundary = class(TCustomMt3dmsConcBoundary)
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
  StrRechConcentration = ' unsaturated recharge concentration';
  StrRechConcentrationMulti = ' unsaturated recharge concentration multiplier';

{ TMt3dUztRchConcItem }

procedure TMt3dUztRchConcItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMt3dUztRchConcCollection;
  ConcentrationObserver: TObserver;
  Index: integer;
begin
  ParentCollection := Collection as TMt3dUztRchConcCollection;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    ConcentrationObserver := FObserverList[Index];
    ConcentrationObserver.OnUpToDateSet :=
      ParentCollection.InvalidateUztRechConc;
  end;
end;

procedure TMt3dUztRchConcItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateUztRechConc(self);
  end;
end;

{ TMt3dUztRchConcTimeListLink }

procedure TMt3dUztRchConcTimeListLink.CreateTimeLists;
var
  Index: Integer;
  Mt3dRechConcData: TModflowTimeList;
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
    Mt3dRechConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dRechConcData.NonParamDescription := Item.Name +  StrRechConcentration;
    Mt3dRechConcData.ParamDescription := Item.Name +  StrRechConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dRechConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateUztRechConc;
    end;
    AddTimeList(Mt3dRechConcData);
    ListOfTimeLists.Add(Mt3dRechConcData);
  end;
  for Index := 0 to LocalModel.ImmobileComponents.Count - 1 do
  begin
    Item := LocalModel.ImmobileComponents[Index];
    Mt3dRechConcData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
    Mt3dRechConcData.NonParamDescription := Item.Name +  StrRechConcentration;
    Mt3dRechConcData.ParamDescription := Item.Name +  StrRechConcentrationMulti;
    if Model <> nil then
    begin
      Mt3dRechConcData.OnInvalidate :=
        (Model as TCustomModel).InvalidateUztRechConc;
    end;
    AddTimeList(Mt3dRechConcData);
    ListOfTimeLists.Add(Mt3dRechConcData);
  end;
end;

{ TMt3dUztRchConcCollection }

function TMt3dUztRchConcCollection.ConcName: string;
begin
  result := StrRechConcentration;
end;

function TMt3dUztRchConcCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMt3dUztRchConcTimeListLink;
end;

procedure TMt3dUztRchConcCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateUztRechConc(self);
  end;
end;

procedure TMt3dUztRchConcCollection.InvalidateUztRechConc(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMt3dUztRchConcTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMt3dUztRchConcTimeListLink;
    for index := 0 to Link.TimeLists.Count - 1 do
    begin
      ATimeList := Link.TimeLists[index];
      ATimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMt3dUztRchConcTimeListLink;
      for index := 0 to Link.TimeLists.Count - 1 do
      begin
        ATimeList := Link.TimeLists[index];
        ATimeList.Invalidate;
      end;
    end;
  end;
end;

class function TMt3dUztRchConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMt3dUztRchConcItem;
end;

{ TMt3dUztRchConcBoundary }

procedure TMt3dUztRchConcBoundary.AssignCells(
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

class function TMt3dUztRchConcBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMt3dUztRchConcCollection;
end;

procedure TMt3dUztRchConcBoundary.GetCellValues(ValueTimeList: TList;
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

procedure TMt3dUztRchConcBoundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateUztRechConc(self);
  end;
end;

end.
