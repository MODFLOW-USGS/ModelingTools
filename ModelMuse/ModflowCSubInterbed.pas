unit ModflowCSubInterbed;

interface

uses
  OrderedCollectionUnit, System.Classes, GoPhastTypes, System.SysUtils,
  DataSetUnit;

type
  TCSubInterbedType = (itDelay, itNoDelay);

  TCSubInterbed = class(TOrderedItem)
  private
    FName: string;
    FInterbedType: TCSubInterbedType;
    FDelayKvName: string;
    FEquivInterbedNumberName: string;
    FInitialDelayHeadOffset: string;
    FInitialElasticSpecificStorage: string;
    FInitialInelasticSpecificStorage: string;
    FInitialOffset: string;
    FInitialPorosity: string;
    FThickness: string;
    procedure SetInterbedType(const Value: TCSubInterbedType);
    procedure SetName(const Value: string);
    procedure RenameInterbed(const NewInterbedName: string);
    procedure CreateOrRenameDataArray(var InterbedName: string;
      Extension, DisplayExtension: string; const NewInterbedName: string;
      AModel: TBaseModel);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DataArrayUsed(ADataArray: TDataArray): Boolean;
  published
    property Name: string read FName write SetName;
    property InterbedType: TCSubInterbedType read FInterbedType write SetInterbedType;
  end;

  TCSubInterbeds = class(TLayerOwnerCollection)
  private
    function GetItem(Index: Integer): TCSubInterbed;
    procedure SetItem(Index: Integer; const Value: TCSubInterbed);
  public
    function Add: TCSubInterbed;
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TCSubInterbed read GetItem write SetItem; default;
    function DataArrayUsed(ADataArray: TDataArray): Boolean;
  end;

implementation

uses
  PhastModelUnit, GlobalVariablesUnit, frmGoPhastUnit, RbwParser,
  ModflowPackagesUnit;

const
  KDelayKv = 'DelayKv';
  KEquivInterbedNumber = 'EquivInterbedNumber';
  KInitialDelayHeadOffs = 'InitialDelayHeadOffset';
  KInitialElasticSpecif = 'InitialElasticSpecificStorage';
  KInitialInelasticSpec = 'InitialInelasticSpecificStorage';
  KInitialOffset = 'InitialOffset';
  KInitialPorosity = 'InitialPorosity';
  KThickness = 'Thickness';
  
resourcestring  
  StrDelayKv = KDelayKv;
  StrEquivInterbedNumber = KEquivInterbedNumber;
  StrInitialDelayHeadOffs = KInitialDelayHeadOffs;
  StrInitialElasticSpecif = KInitialElasticSpecif;
  StrInitialInelasticSpec = KInitialInelasticSpec;
  StrInitialOffset = KInitialOffset;
  StrInitialPorosity = KInitialPorosity;
  StrThickness = KThickness;

{ TCSubInterbed }

procedure TCSubInterbed.Assign(Source: TPersistent);
var
  InterbedSource: TCSubInterbed;
begin
  if Source is TCSubInterbed then
  begin
    InterbedSource := TCSubInterbed(Source);
    Name := InterbedSource.Name;
    InterbedType := InterbedSource.InterbedType;
  end
  else
  begin
    inherited;
  end;
end;

function TCSubInterbed.IsSame(AnotherItem: TOrderedItem): boolean;
var
  InterbedSource: TCSubInterbed;
begin
  result := AnotherItem is TCSubInterbed;
  if result then
  begin
    InterbedSource := TCSubInterbed(AnotherItem);
    result := (Name = InterbedSource.Name)
      and (InterbedType = InterbedSource.InterbedType);
  end;
end;

procedure TCSubInterbed.RenameInterbed(const NewInterbedName: string);
//var
//  LocalModel: TCustomModel;
//  DataArray: TDataArray;
begin
  if Model <> nil then
  begin
    CreateOrRenameDataArray(FDelayKvName, KDelayKv, StrDelayKv, NewInterbedName, Model);
    CreateOrRenameDataArray(FEquivInterbedNumberName, KEquivInterbedNumber, StrEquivInterbedNumber, NewInterbedName, Model);
    CreateOrRenameDataArray(FInitialDelayHeadOffset, KInitialDelayHeadOffs, StrInitialDelayHeadOffs, NewInterbedName, Model);
    CreateOrRenameDataArray(FInitialElasticSpecificStorage, KInitialElasticSpecif, StrInitialElasticSpecif, NewInterbedName, Model);
    CreateOrRenameDataArray(FInitialInelasticSpecificStorage, KInitialInelasticSpec, StrInitialInelasticSpec, NewInterbedName, Model);
    CreateOrRenameDataArray(FInitialOffset, KInitialOffset, StrInitialOffset, NewInterbedName, Model);
    CreateOrRenameDataArray(FInitialPorosity, KInitialPorosity, StrInitialPorosity, NewInterbedName, Model);
    CreateOrRenameDataArray(FThickness, KThickness, StrThickness, NewInterbedName, Model);


{


    LocalModel := Model as TCustomModel;
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FThickessArrayName);
    Assert(DataArray <> nil);
    DataArray.CheckMin := True;
    DataArray.Min := 0;
    HufUsedParameters.RenameLayer(NewInterbedName);
}    
  end;
end;

procedure TCSubInterbed.CreateOrRenameDataArray(var InterbedName: string;
  Extension, DisplayExtension: string; const NewInterbedName: string; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  InterbedDisplayName: string;
  GlobalVariables: TGlobalVariables;
  Basename: string;
  Index: Integer;
begin
  LocalModel := AModel as TCustomModel;
  Assert(LocalModel <> nil);
  DataArrayManager := LocalModel.DataArrayManager;
  if InterbedName = '' then
  begin
    DataArray := nil;
  end
  else
  begin
    DataArray := DataArrayManager.GetDataSetByName(InterbedName);
  end;
  Basename := ValidName(NewInterbedName) + '_';
  InterbedName := Basename + Extension;
  InterbedDisplayName := Basename + DisplayExtension;
  GlobalVariables := frmGoPhast.PhastModel.GlobalVariables;
  Index := 1;
  while (GlobalVariables.IndexOfVariable(InterbedName) >= 0)
    or (GlobalVariables.IndexOfVariable(InterbedDisplayName) >= 0) do
  begin
    Basename := ValidName(NewInterbedName) + IntToStr(Index) + '_';
    Inc(Index);
    InterbedName := Basename + Extension;
    InterbedDisplayName := Basename + DisplayExtension;
  end;
  if DataArray = nil then
  begin
    DataArray := DataArrayManager.GetDataSetByName(InterbedName);
  end;
  if DataArray = nil then
  begin
    DataArray := DataArrayManager.CreateNewDataArray(TRealSparseDataSet, InterbedName,
      '0.', InterbedDisplayName, StandardLock + [dcFormula], rdtDouble, eaBlocks,
      dso3D, StrSubsidence + '|' + NewInterbedName);
    LocalModel.UpdateDataArrayDimensions(DataArray);
    DataArray.OnDataSetUsed := LocalModel.CSubInterbedDataArrayUsed;
    (Collection as TCSubInterbeds).AddOwnedDataArray(DataArray);
  end
  else
  begin
    LocalModel.RenameDataArray(DataArray, InterbedName, InterbedDisplayName);
    DataArray.Lock := StandardLock + [dcFormula];
    DataArray.OnDataSetUsed := LocalModel.CSubInterbedDataArrayUsed;
    DataArray.Classification := StrSubsidence + '|' + NewInterbedName;
  end;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      CreateOrRenameDataArray(InterbedName, Extension, DisplayExtension,
        NewInterbedName, ChildModel);
    end;
  end;
end;

function TCSubInterbed.DataArrayUsed(ADataArray: TDataArray): Boolean;
var
  AName: string;
begin
  Assert(ADataArray <> nil);
  AName := ADataArray.name;
  result := (AName = FDelayKvName)
    or (AName = FEquivInterbedNumberName)
    or (AName = FInitialDelayHeadOffset)
    or (AName = FInitialElasticSpecificStorage)
    or (AName = FInitialInelasticSpecificStorage)
    or (AName = FInitialOffset)
    or (AName = FInitialPorosity)
    or (AName = FThickness);
end;

procedure TCSubInterbed.SetInterbedType(const Value: TCSubInterbedType);
begin
  if FInterbedType <> Value then
  begin
    FInterbedType := Value;
    InvalidateModel;
  end;
end;

procedure TCSubInterbed.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    RenameInterbed(Value);
  end;
  SetCaseSensitiveStringProperty(FName, Value);
end;

{ TCSubInterbeds }

function TCSubInterbeds.Add: TCSubInterbed;
begin
  result := inherited Add as TCSubInterbed;
end;

constructor TCSubInterbeds.Create(Model: TBaseModel);
begin
  inherited Create(TCSubInterbed, Model);
end;

function TCSubInterbeds.DataArrayUsed(ADataArray: TDataArray): Boolean;
var
  Index: Integer;
begin
  result := False;
  for Index := 0 to Count -1 do 
  begin
    result := Items[Index].DataArrayUsed(ADataArray);
    if result then
    begin
      Exit;
    end;
  end;
end;

function TCSubInterbeds.GetItem(Index: Integer): TCSubInterbed;
begin
  result := inherited Items[index] as TCSubInterbed;
end;

procedure TCSubInterbeds.SetItem(Index: Integer; const Value: TCSubInterbed);
begin
  inherited Items[index] := Value;
end;

end.
