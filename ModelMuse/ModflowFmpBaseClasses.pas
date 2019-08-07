unit ModflowFmpBaseClasses;

interface

uses
ModflowBoundaryUnit, FormulaManagerUnit, Classes,
  OrderedCollectionUnit, SysUtils, GoPhastTypes, RealListUnit;

type
  TCustomFarmItem = class(TCustomModflowBoundaryItem)
  protected
    FFormulaObjects: array of TFormulaObject;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    // @name copies Source to this @classname.
    procedure CreateFormulaObjects; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TCustomZeroFarmItem = class(TCustomFarmItem)
  protected
    procedure SetFormulasToZero;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  end;

  TCustomFarmCollection = class(TCustomNonSpatialBoundColl)
  public
    constructor Create(Model: TBaseModel); reintroduce;
    function ItemByStartTime(ATime: Double): TCustomBoundaryItem;
    procedure UpdateTimes(Times: TRealList;
      StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean);
  end;

  TDefineGlobalObject = class(TObject)
  private
    FModel: TBaseModel;
    FOldName: string;
    FNewName: string;
    FComment: string;
  public
    constructor Create(Model: TBaseModel; const OldName, NewName, Comment: string);
    procedure Rename;
    procedure SetValue(Value: Integer);
  end;


implementation

uses
  frmGoPhastUnit, PhastModelUnit, GlobalVariablesUnit, RbwParser;

{ TCustomFarmItem }

procedure TCustomFarmItem.Assign(Source: TPersistent);
var
  Index: integer;
  OtherItem: TCustomFarmItem;
begin
  if Source is TCustomFarmItem then
  begin
    OtherItem := TCustomFarmItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := OtherItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TCustomFarmItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

procedure TCustomFarmItem.CreateFormulaObjects;
var
  Index: integer;
begin
  SetLength(FFormulaObjects, BoundaryFormulaCount);
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    FFormulaObjects[Index] := CreateFormulaObject(dso3D);
  end;
end;

procedure TCustomFarmItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    if Sender = FFormulaObjects[Index] then
    begin
      List.Add(FObserverList[Index]);
      Break;
    end;
  end;
end;

function TCustomFarmItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCustomFarmItem;
  Index: integer;
begin
  result := (AnotherItem is TCustomFarmItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TCustomFarmItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = OtherItem.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCustomFarmItem.RemoveFormulaObjects;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FFormulaObjects[Index],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

{ TCustomZeroFarmItem }

constructor TCustomZeroFarmItem.Create(Collection: TCollection);
begin
  inherited;
  SetFormulasToZero;
end;

destructor TCustomZeroFarmItem.Destroy;
begin
  SetFormulasToZero;
  inherited;
end;

procedure TCustomZeroFarmItem.SetFormulasToZero;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
end;

{ TCustomFarmCollection }

constructor TCustomFarmCollection.Create(Model: TBaseModel);
begin
  inherited Create(nil, Model, nil);
end;

function TCustomFarmCollection.ItemByStartTime(
  ATime: Double): TCustomBoundaryItem;
var
  TimeIndex: Integer;
  AnItem: TCustomBoundaryItem;
begin
  result := nil;
  for TimeIndex := 0 to Count - 1 do
  begin
    AnItem := Items[TimeIndex];
    if AnItem.StartTime <= ATime then
    begin
      result := AnItem;

      if AnItem is TCustomModflowBoundaryItem then
      begin
        if TCustomModflowBoundaryItem(AnItem).EndTime > ATime then
        begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TCustomFarmCollection.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean);
var
  BoundaryIndex: Integer;
  Boundary: TCustomModflowBoundaryItem;
  CosestIndex: Integer;
  ExistingTime: Double;
  SP_Epsilon: Extended;
begin
  SP_Epsilon := (Model as TCustomModel).SP_Epsilon;
  for BoundaryIndex := 0 to Count - 1 do
  begin
    Boundary := Items[BoundaryIndex] as TCustomModflowBoundaryItem;
    CosestIndex := Times.IndexOfClosest(Boundary.StartTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.StartTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.StartTime);
      end;
    end;
    CosestIndex := Times.IndexOfClosest(Boundary.EndTime);
    if CosestIndex >= 0 then
    begin
      ExistingTime := Times[CosestIndex];
      if Abs(ExistingTime-Boundary.EndTime) >  SP_Epsilon then
      begin
        Times.AddUnique(Boundary.EndTime);
      end;
    end;
    if (Boundary.StartTime < StartTestTime-SP_Epsilon) then
    begin
      StartRangeExtended := True;
    end;
    if (Boundary.EndTime > EndTestTime+SP_Epsilon) then
    begin
      EndRangeExtended := True;
    end;
  end;
end;

{ TDefineGlobalObject }

constructor TDefineGlobalObject.Create(Model: TBaseModel; const OldName,
  NewName, Comment: string);
begin
  FModel := Model;
  FOldName := OldName;
  FNewName := NewName;
  FComment := Comment;
end;

procedure TDefineGlobalObject.Rename;
var
  LocalModel: TPhastModel;
  NewVariables: TGlobalVariables;
  Variable: TGlobalVariable;
  OldNames: TStringList;
  NewNames: TStringList;
begin
  LocalModel := (FModel as TPhastModel);

  NewVariables := TGlobalVariables.Create(nil);
  try
    NewVariables.Assign(LocalModel.GlobalVariables);

    Variable := NewVariables.GetVariableByName(FOldName);
    if Variable <> nil then
    begin

      OldNames := TStringList.Create;
      NewNames := TStringList.Create;
      try
        OldNames.Add(FOldName);
        NewNames.Add(FNewName);
        LocalModel.UpdateFormulas(OldNames, NewNames);
        Variable.Name := FNewName;
        LocalModel.GlobalVariables := NewVariables;
        LocalModel.FormulaManager.RestoreSubscriptions;
      finally
        NewNames.Free;
        OldNames.Free;
      end;
    end;
  finally
    NewVariables.Free;
  end;
end;

procedure TDefineGlobalObject.SetValue(Value: Integer);
var
  LocalModel: TPhastModel;
  Variable: TGlobalVariable;
begin
  LocalModel := (FModel as TPhastModel);
  Variable := LocalModel.GlobalVariables.GetVariableByName(FNewName);
  if Variable = nil then
  begin
    Variable := (LocalModel.GlobalVariables.Add as TGlobalVariableItem).Variable;
    Variable.Format := rdtInteger;
    Variable.Name := FNewName;
  end;
  Variable.IntegerValue := Value;
  Variable.Locked := True;
  Variable.Comment := FComment;
end;

end.
