unit ModflowCfpFixedUnit;

interface

uses Classes, ZLib, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit, ModflowCellUnit,
  SysUtils, OrderedCollectionUnit;

type
  CfpRecord = record
    Cell: TCellLocation;
    Value1: double;
    Value2: double;
    Value3: double;
    Value1Used: Boolean;
    Value2Used: Boolean;
    Value3Used: Boolean;
    StartingTime: double;
    Value1Annotation: string;
    Value2Annotation: string;
    Value3Annotation: string;
    procedure Assign(const Item: CfpRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TCfpBoundaryType = (cbtFixedHead, cbtWell, cbtCauchy, cbtLimitedHead);

  TCfpTimeItem = class(TCustomBoundaryItem)
  private
    const
    Value1Position = 0;
    Value2Position  = 1;
    Value3Position = 2;
  var
    FFormulaObjects: array of IFormulaObject;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  published
    property Value1: string index Value1Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property Value2: string index Value2Position read GetBoundaryFormula
      write SetBoundaryFormula;
    property Value3: string index Value3Position read GetBoundaryFormula
      write SetBoundaryFormula;
  end;

  TCfpCollection = class(TCustomMF_ListBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;


  // @name is used to control data set 27 in the CFP process via the
  // @link(TDataArray) "CfpFixedHeads."
//  TCfpFixedBoundary = class(TModflowSteadyBoundary)
  TCfpFixedBoundary = class(TModflowBoundary)
  private
    FFixedHead: IFormulaObject;
    FFixedHeadObserver: TObserver;
    FValue2: IFormulaObject;
    FValue2Observer: TObserver;
    FValue3: IFormulaObject;
    FValue3Observer: TObserver;
    FBoundaryType: TCfpBoundaryType;
    FTimeDependent: Boolean;
    FUsed: Boolean;
    function GetFixedHead: string;
    procedure SetFixedHead(const Value: string);
    function GetFixedHeadObserver: TObserver;
    function GetValue2: string;
    function GetValue3: string;
    procedure SetValue2(const Value: string);
    procedure SetValue3(const Value: string);
    function GetValue2Observer: TObserver;
    function GetValue3Observer: TObserver;
    procedure SetBoundaryType(const Value: TCfpBoundaryType);
    procedure SetTimeDependent(const Value: Boolean);
    function GetUsed: boolean;
    procedure SetUsed(const Value: boolean);
  protected
//    procedure HandleChangedValue(Observer: TObserver); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
//    function GetUsedObserver: TObserver; override;
    procedure CreateFormulaObjects;
    procedure RemoveFormulaObjects;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers;
    property FixedHeadObserver: TObserver read GetFixedHeadObserver;
    property Value2Observer: TObserver read GetValue2Observer;
    property Value3Observer: TObserver read GetValue3Observer;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean;  override;
  published
    property FixedHead: string read GetFixedHead write SetFixedHead;
    property Value2: string read GetValue2 write SetValue2;
    property Value3: string read GetValue3 write SetValue3;
    property BoundaryType: TCfpBoundaryType read FBoundaryType write SetBoundaryType;
    property TimeDependent: Boolean read FTimeDependent write SetTimeDependent;
    property IsUsed: boolean read GetUsed write SetUsed;
  end;

implementation

uses
  PhastModelUnit, DataSetUnit, DataSetNamesUnit, frmGoPhastUnit;

const
  FixedHeadPosition = 0;
  Value2Position = 1;
  Value3Position = 2;

{ TCfpFixedBoundary }

procedure TCfpFixedBoundary.Assign(Source: TPersistent);
var
  SourceCfp: TCfpFixedBoundary;
begin
  if Source is TCfpFixedBoundary then
  begin
    SourceCfp := TCfpFixedBoundary(Source);
    FixedHead := SourceCfp.FixedHead;
    Value2 := SourceCfp.Value2;
    Value3 := SourceCfp.Value3;
    BoundaryType := SourceCfp.BoundaryType;
    TimeDependent := SourceCfp.TimeDependent;
    IsUsed := SourceCfp.IsUsed;
  end;
  inherited;
end;

procedure TCfpFixedBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
begin
  Assert(False);
end;

class function TCfpFixedBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TCfpCollection;
end;

function TCfpFixedBoundary.BoundaryObserverPrefix: string;
begin
  result := 'CfpFixedBoundary_';
end;

constructor TCfpFixedBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;
  FixedHead := '0';
  Value2 := '';
  Value3 := '';
end;

procedure TCfpFixedBoundary.CreateFormulaObjects;
begin
  FFixedHead := CreateFormulaObjectBlocks(dso3D);
  FValue2 := CreateFormulaObjectBlocks(dso3D);
  FValue3 := CreateFormulaObjectBlocks(dso3D);
end;

procedure TCfpFixedBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(FixedHeadObserver);
    FObserverList.Add(Value2Observer);
    FObserverList.Add(Value3Observer);
  end;
end;

destructor TCfpFixedBoundary.Destroy;
begin
  FixedHead := '0';
  Value2 := '';
  Value3 := '';
  RemoveFormulaObjects;
  inherited;
end;

procedure TCfpFixedBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  Assert(False);
end;

function TCfpFixedBoundary.GetFixedHead: string;
begin
  Result := FFixedHead.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(FixedHeadPosition);
  end;
end;

function TCfpFixedBoundary.GetFixedHeadObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FFixedHeadObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_FixedHead_', FFixedHeadObserver, DataArray);
  end;
  result := FFixedHeadObserver;
end;

procedure TCfpFixedBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FFixedHead as TObject then
  begin
    List.Add(FObserverList[FixedHeadPosition]);
  end;
  if Sender = FValue2 as TObject then
  begin
    List.Add(FObserverList[Value2Position]);
  end;
  if Sender = FValue3 as TObject then
  begin
    List.Add(FObserverList[Value3Position]);
  end;
end;

function TCfpFixedBoundary.GetUsed: boolean;
begin
  result := FUsed;
end;

//function TCfpFixedBoundary.GetUsedObserver: TObserver;
//var
//  Model: TPhastModel;
//  DataArray: TDataArray;
//begin
//  if FUsedObserver = nil then
//  begin
//    if ParentModel <> nil then
//    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
//    end
//    else
//    begin
//      DataArray := nil;
//    end;
//    CreateObserver('CFP_Fixed_Used_', FUsedObserver, DataArray);
//  end;
//  result := FUsedObserver;
//end;

function TCfpFixedBoundary.GetValue2: string;
begin
  Result := FValue2.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Value2Position);
  end;
end;

function TCfpFixedBoundary.GetValue2Observer: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FValue2Observer = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpValue2);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_Value2_', FValue2Observer, DataArray);
  end;
  result := FValue2Observer;
end;

function TCfpFixedBoundary.GetValue3: string;
begin
  Result := FValue3.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Value3Position);
  end;
end;

function TCfpFixedBoundary.GetValue3Observer: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FValue3Observer = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpValue3);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('Cfp_Value3_', FValue3Observer, DataArray);
  end;
  result := FValue3Observer;
end;

procedure TCfpFixedBoundary.RemoveFormulaObjects;
begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FFixedHead,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FValue2,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FValue3,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
end;

//procedure TCfpFixedBoundary.HandleChangedValue(Observer: TObserver);
//begin
//  // invalidate display here.
//  { TODO -cCFP : Does this need to be finished?}
//end;

procedure TCfpFixedBoundary.SetBoundaryType(const Value: TCfpBoundaryType);
begin
  if FBoundaryType <> Value then
  begin
    InvalidateModel;
    FBoundaryType := Value;
  end;
end;

procedure TCfpFixedBoundary.SetFixedHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, FixedHeadPosition, FFixedHead);
end;

procedure TCfpFixedBoundary.SetTimeDependent(const Value: Boolean);
begin
  if FTimeDependent <> Value then
  begin
    InvalidateModel;
    FTimeDependent := Value;
  end;
end;

procedure TCfpFixedBoundary.SetUsed(const Value: boolean);
begin
  FUsed := Value
end;

procedure TCfpFixedBoundary.SetValue2(const Value: string);
begin
  UpdateFormulaBlocks(Value, Value2Position, FValue2);
end;

procedure TCfpFixedBoundary.SetValue3(const Value: string);
begin
  UpdateFormulaBlocks(Value, Value3Position, FValue3);
end;

function TCfpFixedBoundary.Used: boolean;
begin
  if TimeDependent then
  begin
    result := inherited;
  end
  else
  begin
    result := IsUsed;
  end;
end;

{ TCfpTimeItem }


{ TCfpTimeItem }

procedure TCfpTimeItem.AssignObserverEvents(Collection: TCollection);
begin
//  inherited;
  // do nothing at least for now. This will have to be updated
  // if it is necessary to visualize transient data.
end;

function TCfpTimeItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

procedure TCfpTimeItem.CreateFormulaObjects;
var
  Index: Integer;
begin
  SetLength(FFormulaObjects, BoundaryFormulaCount);
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    FFormulaObjects[Index] := CreateFormulaObject(dso3D);
  end;
end;

function TCfpTimeItem.GetBoundaryFormula(Index: integer): string;
var
  FormulaObject: IFormulaObject;
begin
  FormulaObject := FFormulaObjects[Index];
  FormulaObject.ScreenObject := ScreenObjectI;
  try
    Result := FormulaObject.Formula;
  finally
    FormulaObject.ScreenObject := nil;
  end;
  ResetItemObserver(Index);
end;

procedure TCfpTimeItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  for Index := 0 to Length(FFormulaObjects) - 1 do
  begin
    if (Sender = FFormulaObjects[Index] as TObject) then
    begin
      List.Add(FObserverList[Value1Position]);
    end;
  end;
end;

function TCfpTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherCfp: TCfpTimeItem;
begin
  result := (AnotherItem is TCfpTimeItem) and inherited;
  if result then
  begin
    OtherCfp := TCfpTimeItem(AnotherItem);
    result := (Value1 = OtherCfp.Value1)
      and (Value2 = OtherCfp.Value2)
      and (Value3 = OtherCfp.Value3);
  end;
end;

procedure TCfpTimeItem.RemoveFormulaObjects;
var
  Index: Integer;
begin
  for Index := 0 to Length(FFormulaObjects) - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FFormulaObjects[Index],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

procedure TCfpTimeItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
end;

{ CfpRecord }

procedure CfpRecord.Assign(const Item: CfpRecord);
begin
  Cell := Item.Cell;
  Value1 := Item.Value1;
  Value2 := Item.Value2;
  Value3 := Item.Value3;
  Value1Used := Item.Value1Used;
  Value2Used := Item.Value2Used;
  Value3Used := Item.Value3Used;
  StartingTime := Item.StartingTime;
  Value1Annotation := Item.Value1Annotation;
  Value2Annotation := Item.Value2Annotation;
  Value3Annotation := Item.Value3Annotation;
end;

procedure CfpRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Value1);
  WriteCompReal(Comp, Value2);
  WriteCompReal(Comp, Value3);

  WriteCompBoolean(Comp, Value1Used);
  WriteCompBoolean(Comp, Value2Used);
  WriteCompBoolean(Comp, Value3Used);

  WriteCompReal(Comp, StartingTime);

  WriteCompInt(Comp, Strings.IndexOf(Value1Annotation));
  WriteCompInt(Comp, Strings.IndexOf(Value2Annotation));
  WriteCompInt(Comp, Strings.IndexOf(Value3Annotation));
end;

procedure CfpRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(Value1Annotation);
  Strings.Add(Value2Annotation);
  Strings.Add(Value3Annotation);

end;

procedure CfpRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Value1 := ReadCompReal(Decomp);
  Value2 := ReadCompReal(Decomp);
  Value3 := ReadCompReal(Decomp);

  Value1Used := ReadCompBoolean(Decomp);
  Value2Used := ReadCompBoolean(Decomp);
  Value2Used := ReadCompBoolean(Decomp);

  StartingTime := ReadCompReal(Decomp);

  Value1Annotation := Annotations[ReadCompInt(Decomp)];
  Value2Annotation := Annotations[ReadCompInt(Decomp)];
  Value3Annotation := Annotations[ReadCompInt(Decomp)];
end;

{ TCfpCollection }

class function TCfpCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCfpTimeItem;
end;

end.
