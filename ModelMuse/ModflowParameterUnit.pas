unit ModflowParameterUnit;

interface

uses SysUtils, Classes, OrderedCollectionUnit, GoPhastTypes;

type
  TModflowSteadyParameters = Class;

  // @name is used for parameters in MODFLOW that have multiplier and zone
  // arrays and do not vary in time.
  TModflowSteadyParameter = class(TModflowParameter)
  private
    // See @link(MultiplierName).
    FMultiplierName: string;
    // See @link(UseMultiplier).
    FUseMultiplier: boolean;
    // See @link(UseZone).
    FUseZone: boolean;
    // See @link(ZoneName).
    FZoneName: string;
    // @name lists the Multiplier array names exported to MODFLOW.
    // @seealso(MultiplierArrayName)
    FMultiplierArrayNames: TStringList;
    // @name lists the Zone array names exported to MODFLOW.
    // @seealso(ZoneArrayName)
    FZoneArrayNames: TStringList;
    FNamesToRemove: TStringList;
    FUsePilotPoints: Boolean;
    FUseHorizontalSpatialContinuityPriorInfo: Boolean;
    FHorizontalSpatialContinuityGroupName: string;
    FVertSpatialContinuityGroupName: string;
    FUseVertSpatialContinuityPriorInfo: Boolean;
    FIsUsedInTemplate: Boolean;
    FStoredVertSpatialContinuityPriorInfoWeight: TRealStorage;
    FStoredHorizontalSpatialContinuityPriorInfoWeight: TRealStorage;
    FUsedDirectly: Boolean;
    // See @link(MultiplierName).
    procedure SetMultiplierName(const Value: string);
    // See @link(UseMultiplier).
    procedure SetUseMultiplier(const Value: boolean);
    // See @link(UseZone).
    procedure SetUseZone(const Value: boolean);
    // See @link(ZoneName).
    procedure SetZoneName(const Value: string);
    // @name is used in @link(SetParameterName) to change the name of the
    // @link(TDataArray) used to define multiplier arrays related to this
    // parameter.
    procedure UpdateMultiplierName(const NewRoot: string);
    // @name is used in @link(SetParameterName) to change the name of the
    // @link(TDataArray) used to define zone arrays related to this
    // parameter.
    procedure UpdateZoneName(const NewRoot: string);
    // @name is the @link(TModflowSteadyParameters) that owns this @classname.
    function Collection: TModflowSteadyParameters;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is used to update @link(MultiplierArrayName)
    // and @link(ZoneArrayName).
    procedure FillArrayNameList(List: TStringList; const ArrayTypeID: string;
      Const DefaultArrayRoot: string; AModel: TBaseModel);
    procedure CreateNewDataSetVariables(const OldName, NewName: string);
    procedure RemoveOldDataSetVariables;
    procedure UpdateHfbParameterNames(const Value: string);
    procedure UpdateFormulas(const OldName, NewName: string);
    procedure UnlockDataSets;
    procedure UnLockGlobalVariables;
    procedure SetUsePilotPoints(const Value: Boolean);
    procedure InvalidatePestDataArrays(CheckAll: Boolean = False);
    procedure SetUseHorizontalSpatialContinuityPriorInfo(const Value: Boolean);
    procedure SetHorizontalSpatialContinuityGroupName(const Value: string);
    procedure SetUseVertSpatialContinuityPriorInfo(const Value: Boolean);
    procedure SetVertSpatialContinuityGroupName(const Value: string);
    procedure SetIsUsedInTemplate(const Value: Boolean);
    procedure SetStoredHorizontalSpatialContinuityPriorInfoWeight(
      const Value: TRealStorage);
    procedure SetStoredVertSpatialContinuityPriorInfoWeight(
      const Value: TRealStorage);
    function GetHorizontalSpatialContinuityPriorInfoWeight: double;
    function GetVertSpatialContinuityPriorInfoWeight: double;
    procedure SetHorizontalSpatialContinuityPriorInfoWeight(
      const Value: double);
    procedure SetVertSpatialContinuityPriorInfoWeight(const Value: double);
    procedure SetUsedDirectly(const Value: Boolean);
  protected
    // Besides setting the name of the parameter, @name also updates the
    // names of the @link(TDataArray)s used to define multiplier and zone
    // arrays.
    procedure SetParameterName(const Value: string);override;
    procedure SetValue(AValue : double); override;
    procedure SetParameterType(const Value: TParameterType); override;
  public
    procedure ClearArrayNames;
    // @name copies the source @link(TModflowSteadyParameter)
    // to the current one.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys an instance of @classname.
    destructor Destroy; override;
    // @name calls inherited @name and then checks whether
    // @link(MultiplierName), @link(UseMultiplier), @link(UseZone),
    // and @link(MultiplierArrayName) are the same as in the source item.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    property IsUsedInTemplate: Boolean read FIsUsedInTemplate write SetIsUsedInTemplate;
    property HorizontalSpatialContinuityPriorInfoWeight: double
      read GetHorizontalSpatialContinuityPriorInfoWeight
      Write SetHorizontalSpatialContinuityPriorInfoWeight;
    property VertSpatialContinuityPriorInfoWeight: double
      read GetVertSpatialContinuityPriorInfoWeight
      Write SetVertSpatialContinuityPriorInfoWeight;
  published
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name lists the multiplier array names exported to MODFLOW.
    function MultiplierArrayName(ModflowLayer: integer; AModel: TBaseModel): string;
    // @name is the name of the @link(TDataArray) used to define
    // MODFLOW multiplier arrays.
    property MultiplierName: string read FMultiplierName
      write SetMultiplierName;
    // @name specifies whether this parameter will use a multiplier array.
    property UseMultiplier: boolean read FUseMultiplier write SetUseMultiplier stored True;
    // @name specifies whether this parameter will use a zone array.
    property UseZone: boolean read FUseZone write SetUseZone stored True;
    // @name is the name of the @link(TDataArray) used to define
    // MODFLOW zone arrays.
    property ZoneName: string read FZoneName write SetZoneName;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name lists the zone array names exported to MODFLOW.
    function ZoneArrayName(ModflowLayer: integer; AModel: TBaseModel): string;
    property UsePilotPoints: Boolean read FUsePilotPoints
      write SetUsePilotPoints Stored True;
    property UseHorizontalSpatialContinuityPriorInfo: Boolean
      read FUseHorizontalSpatialContinuityPriorInfo
      write SetUseHorizontalSpatialContinuityPriorInfo Stored True;
    property HorizontalSpatialContinuityGroupName: string
      read FHorizontalSpatialContinuityGroupName
      write SetHorizontalSpatialContinuityGroupName Stored True;
    property StoredHorizontalSpatialContinuityPriorInfoWeight: TRealStorage
      read FStoredHorizontalSpatialContinuityPriorInfoWeight
      write SetStoredHorizontalSpatialContinuityPriorInfoWeight Stored True;
    Property UseVertSpatialContinuityPriorInfo: Boolean
      read FUseVertSpatialContinuityPriorInfo
      write SetUseVertSpatialContinuityPriorInfo Stored True;
    property VertSpatialContinuityGroupName: string
      read FVertSpatialContinuityGroupName
      write SetVertSpatialContinuityGroupName Stored True;
    property StoredVertSpatialContinuityPriorInfoWeight: TRealStorage
      read FStoredVertSpatialContinuityPriorInfoWeight
      write SetStoredVertSpatialContinuityPriorInfoWeight Stored True;
    // @name is set to @True if this parameter is substituted directly
    // into a template rather than being the parent of pilot point
    // parameters.
    property UsedDirectly: Boolean read FUsedDirectly write SetUsedDirectly
      stored True;
  end;

  // @name is a collection of @link(TModflowSteadyParameter)s.
  // It is used to store the parameters for the LPF package and other
  // similar packages that use multiplier and zone arrays.
  TModflowSteadyParameters = class(TLayerOwnerCollection)
  strict private
    // @name is used to ensure that all the multiplier and zone arrays are
    // unique.  See @link(IsArrayNameUnique).
    FArrayNames: TStringList;
  private
    // See @link(Items).
    function GetItems(Index: integer): TModflowSteadyParameter;
    // See @link(Items).
    procedure SetItems(Index: integer; const Value: TModflowSteadyParameter);
    // @name returns @true if MultiplierName (the name of a multiplier or
    // zone array exported to MODFLOW) is unique.
    function IsArrayNameUnique(const ArrayName: string): boolean;
    // @name adds a multiplier or zone array to @link(FArrayNames).
    procedure AddArrayName(const ArrayName: string);
    // @name is the number of multiplier and zone array names.
    function ArrayNameCount: integer;
  public
    procedure Clear;
    procedure ClearArrayNames;
    procedure RemoveOldDataSetVariables;
    // @name is used to access the @link(TModflowSteadyParameter) owned
    // by this @classname.
    property Items[Index: integer]: TModflowSteadyParameter read GetItems
      write SetItems; default;
    // @name copies the source @link(TModflowSteadyParameters) this this
    // @classname.
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    // @name destroys this instance of @classname.
    destructor Destroy; override;
    // @name returns the number of parameters that match ParamTypes.
    function CountParameters(ParamTypes: TParameterTypes): integer;
    function IsDataSetUsed(AnObject: TObject): boolean;
    function GetParamByName(Const AName: string): TModflowSteadyParameter;
    procedure SetAllParametersUnused;
  end;

var
  // @name is used to the store the names of the
  // multiplier arrays that are exported to MODFLOW.
  UsedMultiplierArrayNames: TStringList;
  // @name is used to the store the names of the
  // zone arrays that are exported to MODFLOW.
  UsedZoneArrayNames: TStringList;

const
  MaxLengthModflowParameterName = 10;

implementation

uses Math, RbwParser, PhastModelUnit, DataSetUnit, 
  ScreenObjectUnit, ModflowHfbUnit, frmGoPhastUnit,
  LockedGlobalVariableChangers, DataSetNamesUnit;

{ TModflowSteadyParameter }

procedure TModflowSteadyParameter.Assign(Source: TPersistent);
Var
  SourceParameter: TModflowSteadyParameter;
begin
  // if Assign is updated, update IsSame too.
  inherited;
  if Source is TModflowSteadyParameter then
  begin
    SourceParameter := TModflowSteadyParameter(Source);
    MultiplierName := SourceParameter.MultiplierName;
    ZoneName := SourceParameter.ZoneName;
    UseMultiplier := SourceParameter.UseMultiplier;
    UseZone := SourceParameter.UseZone;
    UsePilotPoints := SourceParameter.UsePilotPoints;
    UseHorizontalSpatialContinuityPriorInfo :=
      SourceParameter.UseHorizontalSpatialContinuityPriorInfo;
    HorizontalSpatialContinuityGroupName :=
      SourceParameter.HorizontalSpatialContinuityGroupName;
    UseVertSpatialContinuityPriorInfo :=
      SourceParameter.UseVertSpatialContinuityPriorInfo;
    VertSpatialContinuityGroupName :=
      SourceParameter.VertSpatialContinuityGroupName;
    HorizontalSpatialContinuityPriorInfoWeight :=
      SourceParameter.HorizontalSpatialContinuityPriorInfoWeight;
    VertSpatialContinuityPriorInfoWeight :=
      SourceParameter.VertSpatialContinuityPriorInfoWeight;
  end;
end;

procedure TModflowSteadyParameter.ClearArrayNames;
begin
  FMultiplierArrayNames.Clear;
  FZoneArrayNames.Clear;
end;

function TModflowSteadyParameter.Collection: TModflowSteadyParameters;
begin
  result := inherited Collection as TModflowSteadyParameters;
end;

constructor TModflowSteadyParameter.Create(Collection: TCollection);
begin
  inherited;
  FUsedDirectly := True;
  FUseHorizontalSpatialContinuityPriorInfo := True;
  FUseVertSpatialContinuityPriorInfo := True;
  FMultiplierArrayNames:= TStringList.Create;
  FZoneArrayNames:= TStringList.Create;
  FNamesToRemove:= TStringList.Create;

  FStoredHorizontalSpatialContinuityPriorInfoWeight := TRealStorage.Create;
  FStoredHorizontalSpatialContinuityPriorInfoWeight.Value := 1;
  FStoredHorizontalSpatialContinuityPriorInfoWeight.OnChange := OnInvalidateModelEvent;
  FStoredVertSpatialContinuityPriorInfoWeight := TRealStorage.Create;
  FStoredVertSpatialContinuityPriorInfoWeight.Value := 1;
  FStoredVertSpatialContinuityPriorInfoWeight.OnChange := OnInvalidateModelEvent;

end;

destructor TModflowSteadyParameter.Destroy;
var
  Model: TPhastModel;
  HfbBoundary: THfbBoundary;
  ScreenObject: TScreenObject;
  Index: integer;
begin
  UnLockGlobalVariables;
  UnlockDataSets;
  FStoredVertSpatialContinuityPriorInfoWeight.Free;
  FStoredHorizontalSpatialContinuityPriorInfoWeight.Free;
  FNamesToRemove.Free;
  FMultiplierArrayNames.Free;
  FZoneArrayNames.Free;
  if ParameterType = ptHFB then
  begin
    Model := Collection.Model as TPhastModel;
    if Model <> nil then
    begin
      for Index := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[Index];
        HfbBoundary := ScreenObject.ModflowHfbBoundary;
        if (HfbBoundary <> nil)
          and (HfbBoundary.ParameterName = FParameterName)
          and (HfbBoundary.ParameterName <> '') then
        begin
          HfbBoundary.ParameterName := '';
        end;
      end;
    end;
  end;

  inherited;
end;

procedure TModflowSteadyParameter.InvalidatePestDataArrays(CheckAll: Boolean = False);
var
  LocalModel: TCustomModel;
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  if (Model <> nil) and ((ParameterType = ptPEST) or CheckAll) then
  begin
    LocalModel := Model as TCustomModel;
    for DataArrayIndex := 0 to LocalModel.DataArrayManager.DataSetCount - 1 do
    begin
      ADataArray := LocalModel.DataArrayManager[DataArrayIndex];
      if ADataArray.PestParametersUsed
        and (ADataArray.UsedPestParameters.IndexOf(ParameterName) >= 0) then
      begin
        ADataArray.Invalidate;
      end;
    end;
  end;
end;

function TModflowSteadyParameter.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherParameter: TModflowSteadyParameter;
begin
  AnotherParameter := AnotherItem as TModflowSteadyParameter;
  result := inherited IsSame(AnotherItem) and
    (MultiplierName = AnotherParameter.MultiplierName) and
    (UseMultiplier = AnotherParameter.UseMultiplier) and
    (UseZone = AnotherParameter.UseZone) and
    (ZoneName = AnotherParameter.ZoneName) and
    (UseHorizontalSpatialContinuityPriorInfo = AnotherParameter.UseHorizontalSpatialContinuityPriorInfo) and
    (HorizontalSpatialContinuityGroupName = AnotherParameter.HorizontalSpatialContinuityGroupName) and
    (UseVertSpatialContinuityPriorInfo = AnotherParameter.UseVertSpatialContinuityPriorInfo) and
    (VertSpatialContinuityGroupName = AnotherParameter.VertSpatialContinuityGroupName) and
    (HorizontalSpatialContinuityPriorInfoWeight = AnotherParameter.HorizontalSpatialContinuityPriorInfoWeight) and
    (VertSpatialContinuityPriorInfoWeight = AnotherParameter.VertSpatialContinuityPriorInfoWeight);

end;

procedure TModflowSteadyParameter.FillArrayNameList(List: TStringList;
  const ArrayTypeID: string; Const DefaultArrayRoot: string; AModel: TBaseModel);
var
  LayerCount: integer;
  LayerIndex: Integer;
  MaxParamRootLength: integer;
  ParamRoot: string;
  ArrayName: string;
begin
  LayerCount := (AModel as TCustomModel). ModflowLayerCount;
  // The maximum length of the name of a multiplier or zone array is
  // 10 characters. ArrayTypeID will have a length of 2 characters.
  MaxParamRootLength := 7 - Trunc(Log10(LayerCount));
  ParamRoot := Copy(ParameterName,1,MaxParamRootLength);
  for LayerIndex := 1 to LayerCount do
  begin
    ArrayName := ParamRoot + ArrayTypeID + IntToStr(LayerIndex);
    if not Collection.IsArrayNameUnique(ArrayName) then
    begin
      ArrayName := DefaultArrayRoot + IntToStr(Collection.ArrayNameCount +1);
    end;
    List.Add(ArrayName);
    Collection.AddArrayName(ArrayName);
  end;
end;

function TModflowSteadyParameter.GetHorizontalSpatialContinuityPriorInfoWeight: double;
begin
  result := StoredHorizontalSpatialContinuityPriorInfoWeight.Value;
end;

function TModflowSteadyParameter.GetVertSpatialContinuityPriorInfoWeight: double;
begin
  result := StoredVertSpatialContinuityPriorInfoWeight.Value;
end;

function TModflowSteadyParameter.MultiplierArrayName(
  ModflowLayer: integer; AModel: TBaseModel): string;
begin
  Assert(ModflowLayer >= 1);
  Assert(UseMultiplier);
  if FMultiplierArrayNames.Count = 0 then
  begin
    FillArrayNameList(FMultiplierArrayNames, '_M', 'MULT_', AModel);
  end;
  result := FMultiplierArrayNames[ModflowLayer-1];
end;

procedure TModflowSteadyParameter.UnlockDataSets;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(FMultiplierName);
      if DataArray <> nil then
      begin
        DataArray.Lock := DataArray.Lock - [dcName];
      end;
      DataArray := Model.DataArrayManager.GetDataSetByName(FZoneName);
      if DataArray <> nil then
      begin
        DataArray.Lock := DataArray.Lock - [dcName];
      end;
    end;
  end;
end;

procedure TModflowSteadyParameter.UnLockGlobalVariables;
var
  Model: TPhastModel;
  Unlocker: TDefineGlobalStringObject;
begin
  if (ParameterType = ptPest) and (Collection.Model <> nil) then
  begin
    Model := Collection.Model as TPhastModel;
    if ([csLoading, csDestroying] * Model.ComponentState) = [] then
    begin
      Unlocker := TDefineGlobalStringObject.Create(Collection.Model as TCustomModel,
        ParameterName, ParameterName, StrParameterType);
      try
        Unlocker.Locked := False;
      finally
        Unlocker.Free;
      end;
    end;
  end;
end;

procedure TModflowSteadyParameter.SetMultiplierName(const Value: string);
var
  Model: TPhastModel;
//  DataArrayIndex : integer;
  DataArray: TDataArray;
  NewFormula: string;
  AnOrientation: TDataSetOrientation;
  Classification: string;
begin
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(FMultiplierName);
      if UseMultiplier then
      begin
        if DataArray <> nil then
        begin
          if (FMultiplierName <> '') and (Value <> '') and
            (FMultiplierName <> Value) then
          begin
            UpdateFormulas(FMultiplierName, Value);
          end;
          CreateNewDataSetVariables(DataArray.Name, Value);
          DataArray.Name := Value;
          DataArray.DisplayName := Value;
          DataArray.Lock := DataArray.Lock + [dcName];
        end
        else
        begin
          NewFormula := '1.';

          Classification := ParmeterTypeToStr(ParameterType) + ' Parameter';
          if ParameterType = ptHUF_SYTP then
          begin
            AnOrientation := dsoTop;
            Classification := StrHUF + '|' + Classification;
          end
          else if ParameterType = ptHUF_LVDA then
          begin
            AnOrientation := dso3D;
            Classification := StrHUF + '|' + Classification;
          end
          else
          begin
            AnOrientation := dso3D;
          end;

          DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
            Value, NewFormula, Value,
            [dcName, dcType, dcOrientation, dcEvaluatedAt], rdtDouble,
            eaBlocks, AnOrientation, Classification);
          Collection.AddOwnedDataArray(DataArray);
        end;
        DataArray.UpdateDimensions(Model.LayerCount,
          Model.RowCount, Model.ColumnCount);
        DataArray.OnDataSetUsed := Model.ParameterDataSetUsed;
      end
      else
      begin
        if DataArray <> nil then
        begin
//          DataArray := Model.DataSets[DataArrayIndex];
          DataArray.Lock := DataArray.Lock - [dcName];
          DataArray.OnDataSetUsed := nil;
        end
      end;
    end;
  end;
  if FMultiplierName <> Value then
  begin
    FMultiplierName := Value;
    InvalidateModel;
  end;
end;

procedure TModflowSteadyParameter.UpdateMultiplierName(const NewRoot: string);
var
  NewName: string;
  LocalModel: TPhastModel;
begin
  if UseMultiplier then
  begin
    NewName := NewRoot + '_Multiplier';
    LocalModel := frmGoPhast.PhastModel;
    if (UpperCase(FMultiplierName) <> UpperCase(NewName))
      and (LocalModel.DataArrayManager.GetDataSetByName(NewName) = nil) then
    begin
      NewName := GenerateNewName(NewName);
    end;
    MultiplierName := NewName;
  end;
end;

procedure TModflowSteadyParameter.UpdateZoneName(const NewRoot: string);
var
  NewName: string;
  LocalModel: TPhastModel;
begin
  if UseZone then
  begin
    NewName := NewRoot + '_Zone';
    LocalModel := frmGoPhast.PhastModel;
    if (UpperCase(FZoneName) <> UpperCase(NewName))
     and (LocalModel.DataArrayManager.GetDataSetByName(NewName) = nil) then
    begin
      NewName := GenerateNewName(NewName);
    end;

    ZoneName := NewName;
  end;
end;

function TModflowSteadyParameter.ZoneArrayName(ModflowLayer: integer; AModel: TBaseModel): string;
begin
  Assert(ModflowLayer >= 1);
  Assert(UseZone);
  if FZoneArrayNames.Count = 0 then
  begin
    FillArrayNameList(FZoneArrayNames, '_Z', 'ZONE_', AModel);
  end;
  result := FZoneArrayNames[ModflowLayer-1];
end;

procedure TModflowSteadyParameter.UpdateFormulas(const OldName, NewName: string);
var
  OldNames: TStringList;
  NewNames: TStringList;
//  CompilerList: TList;
//  CompilerIndex: Integer;
//  Compiler: TRbwParser;
//  VariableIndex: Integer;
//  VarIndex: Integer;
begin
  NewNames := TStringList.Create;
  OldNames := TStringList.Create;
  try
    NewNames.Add(NewName);
    OldNames.Add(OldName);
    frmGoPhast.PhastModel.UpdateFormulas(OldNames, NewNames);
  finally
    OldNames.Free;
    NewNames.Free;
  end;
end;

procedure TModflowSteadyParameter.UpdateHfbParameterNames(const Value: string);
var
  Model: TPhastModel;
  HfbBoundary: THfbBoundary;
  ScreenObject: TScreenObject;
  Index: integer;
begin
  if ParameterType = ptHFB then
  begin
    Model := Collection.Model as TPhastModel;
    if Model <> nil then
    begin
      for Index := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[Index];
        HfbBoundary := ScreenObject.ModflowHfbBoundary;
        if (HfbBoundary.ParameterName = FParameterName) and (HfbBoundary.ParameterName <> '') then
        begin
          HfbBoundary.ParameterName := Value;
        end;
      end;
    end;
  end;
end;

procedure TModflowSteadyParameter.SetParameterName(const Value: string);
var
  NewName: string;
  ChangeGlobal: TDefineGlobalStringObject;
begin
  NewName := CorrectParamName(Value);
  Assert(Length(NewName) <= MaxLengthModflowParameterName);
  if FParameterName <> NewName then
  begin
    InvalidatePestDataArrays;
    UpdateMultiplierName(NewName);
    UpdateZoneName(NewName);
    UpdateHfbParameterNames(NewName);
    InvalidateModel;
    if ParameterType = ptPEST then
    begin
      ChangeGlobal := TDefineGlobalStringObject.Create(Model as TCustomModel, FParameterName, NewName,
        StrParameterType);
      try
        ChangeGlobal.Rename;
        ChangeGlobal.SetValue(NewName);
      finally
        ChangeGlobal.Free;
      end;
    end;
    FParameterName := NewName;
  end;
end;

procedure TModflowSteadyParameter.SetParameterType(const Value: TParameterType);
begin
  if (ParameterType <> Value) then
  begin
    if ptPEST in [ParameterType, Value] then
    begin
      InvalidatePestDataArrays(True);
    end;
  end;
  inherited;

end;

procedure TModflowSteadyParameter.SetStoredHorizontalSpatialContinuityPriorInfoWeight(
  const Value: TRealStorage);
begin
  FStoredHorizontalSpatialContinuityPriorInfoWeight.Assign(Value);
end;

procedure TModflowSteadyParameter.SetStoredVertSpatialContinuityPriorInfoWeight(
  const Value: TRealStorage);
begin
  FStoredVertSpatialContinuityPriorInfoWeight.Assign(Value);
end;

procedure TModflowSteadyParameter.SetHorizontalSpatialContinuityPriorInfoWeight(
  const Value: double);
begin
  StoredHorizontalSpatialContinuityPriorInfoWeight.Value := Value;
end;

procedure TModflowSteadyParameter.SetHorizontalSpatialContinuityGroupName(
  const Value: string);
begin
  SetCaseSensitiveStringProperty(FHorizontalSpatialContinuityGroupName, Value);
end;

procedure TModflowSteadyParameter.SetIsUsedInTemplate(const Value: Boolean);
begin
  FIsUsedInTemplate := Value;
end;

procedure TModflowSteadyParameter.SetUseMultiplier(const Value: boolean);
begin
  if FUseMultiplier <> Value then
  begin
    FUseMultiplier := Value;
    InvalidateModel;
  end;
  UpdateMultiplierName(FParameterName);
end;

procedure TModflowSteadyParameter.SetUsePilotPoints(const Value: Boolean);
begin
  if FUsePilotPoints <> Value then
  begin
    FUsePilotPoints := Value;
    InvalidateModel;
  end;
end;

procedure TModflowSteadyParameter.SetUseVertSpatialContinuityPriorInfo(
  const Value: Boolean);
begin
  SetBooleanProperty(FUseVertSpatialContinuityPriorInfo, Value);
end;

procedure TModflowSteadyParameter.SetUsedDirectly(const Value: Boolean);
begin
  FUsedDirectly := Value;
end;

procedure TModflowSteadyParameter.SetUseHorizontalSpatialContinuityPriorInfo(
  const Value: Boolean);
begin
  SetBooleanProperty(FUseHorizontalSpatialContinuityPriorInfo, Value);
end;

procedure TModflowSteadyParameter.SetUseZone(const Value: boolean);
begin
  if FUseZone <> Value then
  begin
    FUseZone := Value;
    InvalidateModel;
  end;
  UpdateZoneName(FParameterName);
end;

procedure TModflowSteadyParameter.SetValue(AValue: double);
begin
  if AValue <> Value then
  begin
    InvalidatePestDataArrays;
  end;
  inherited;
end;

procedure TModflowSteadyParameter.SetVertSpatialContinuityGroupName(
  const Value: string);
begin
  SetCaseSensitiveStringProperty(FVertSpatialContinuityGroupName, Value);
end;

procedure TModflowSteadyParameter.SetVertSpatialContinuityPriorInfoWeight(
  const Value: double);
begin
  StoredVertSpatialContinuityPriorInfoWeight.Value := Value;
end;

procedure TModflowSteadyParameter.CreateNewDataSetVariables(
  const OldName, NewName: string);
var
  Model: TPhastModel;
  Parser : TRbwParser;
  VariableIndex: integer;
  Index: integer;
  Variable: TCustomVariable;
begin
  if OldName = NewName then
  begin
    Exit;
  end;
  Model := Collection.Model as TPhastModel;
  if UpperCase(OldName) = UpperCase(NewName) then
  begin
    for Index := 0 to Model.ParserCount - 1 do
    begin
      Parser := Model.Parsers[Index];
      VariableIndex := Parser.IndexOfVariable(OldName);
      if VariableIndex >= 0 then
      begin
        Parser.RenameVariable(VariableIndex, NewName, NewName);
      end;
    end;
  end
  else
  begin
    FNamesToRemove.Add(OldName);
    for Index := 0 to Model.ParserCount - 1 do
    begin
      Parser := Model.Parsers[Index];
      VariableIndex := Parser.IndexOfVariable(OldName);
      if VariableIndex >= 0 then
      begin
        Variable := Parser.Variables[VariableIndex] as TCustomVariable;
        case Variable.ResultType of
          rdtDouble:
            begin
              Parser.CreateVariable(NewName, Variable.Classification, 0.0, NewName);
            end;
          rdtInteger:
            begin
              Parser.CreateVariable(NewName, Variable.Classification, 0, NewName);
            end;
          rdtBoolean:
            begin
              Parser.CreateVariable(NewName, Variable.Classification, False, NewName);
            end;
          rdtString:
            begin
              Parser.CreateVariable(NewName, Variable.Classification, '', NewName);
            end;
        end;
  //      Parser.RenameVariable(VariableIndex, NewName);
      end;
    end;
  end;
end;

procedure TModflowSteadyParameter.RemoveOldDataSetVariables;
var
  Model: TPhastModel;
  Parser : TRbwParser;
  VariableIndex: integer;
  Index: integer;
  Variable: TCustomVariable;
  NameIndex: Integer;
  OldName: string;
begin
  Model := Collection.Model as TPhastModel;
  for NameIndex := 0 to FNamesToRemove.Count - 1 do
  begin
    OldName := FNamesToRemove[NameIndex];
    for Index := 0 to Model.ParserCount - 1 do
    begin
      Parser := Model.Parsers[Index];
      VariableIndex := Parser.IndexOfVariable(OldName);
      if VariableIndex >= 0 then
      begin
        Variable := Parser.Variables[VariableIndex] as TCustomVariable;
        Parser.RemoveVariable(Variable);
      end;
    end;
  end;
  FNamesToRemove.Clear;
end;

procedure TModflowSteadyParameter.SetZoneName(const Value: string);
var
  Model: TPhastModel;
//  DataArrayIndex : integer;
  DataArray: TDataArray;
  NewFormula: string;
  AnOrientation: TDataSetOrientation;
  Classification: string;
begin
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(FZoneName);
      if UseZone then
      begin
        if DataArray <> nil then
        begin
          if (FZoneName <> '') and (Value <> '') and
            (FZoneName <> Value) then
          begin
            UpdateFormulas(FZoneName, Value);
          end;
          CreateNewDataSetVariables(DataArray.Name, Value);
          DataArray.Name := Value;
          DataArray.DisplayName := Value;
          DataArray.Lock := DataArray.Lock + [dcName];
        end
        else
        begin
          NewFormula := 'False';

          Classification := ParmeterTypeToStr(ParameterType) + ' Parameter';
          if ParameterType = ptHUF_SYTP then
          begin
            AnOrientation := dsoTop;
            Classification := StrHUF + '|' + Classification;
          end
          else if ParameterType = ptHUF_LVDA then
          begin
            AnOrientation := dso3D;
            Classification := StrHUF + '|' + Classification;
          end
          else
          begin
            AnOrientation := dso3D;
          end;

          DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
            Value, NewFormula, Value,
            [dcName, dcType, dcOrientation, dcEvaluatedAt], rdtBoolean,
            eaBlocks, AnOrientation, Classification);
          Collection.AddOwnedDataArray(DataArray);
        end;
        DataArray.UpdateDimensions(Model.LayerCount,
          Model.RowCount, Model.ColumnCount);
        DataArray.OnDataSetUsed := Model.ParameterDataSetUsed;
      end
      else
      begin
        if DataArray <> nil then
        begin
          DataArray.Lock := DataArray.Lock - [dcName];
          DataArray.OnDataSetUsed := nil;
        end
      end;
    end;
  end;
  if FZoneName <> Value then
  begin
    FZoneName := Value;
    InvalidateModel;
  end;
end;

{ TModflowSteadyParameters }

procedure TModflowSteadyParameters.AddArrayName(
  const ArrayName: string);
begin
  FArrayNames.Add(ArrayName);
end;

procedure TModflowSteadyParameters.Assign(Source: TPersistent);
var
  SourceParameters: TModflowSteadyParameters;
  Index: Integer;
begin
  inherited;
  if Source is TModflowSteadyParameters then
  begin
    SourceParameters := TModflowSteadyParameters(Source);
    if Count = SourceParameters.Count then
    begin
      for Index := 0 to Count - 1 do
      begin
        Items[Index].Assign(SourceParameters.Items[Index]);
      end;
    end
    else
    begin
      inherited;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowSteadyParameters.Clear;
begin
  inherited;
  FArrayNames.Clear;
end;

procedure TModflowSteadyParameters.ClearArrayNames;
begin
  FArrayNames.Clear;
end;

function TModflowSteadyParameters.CountParameters(
  ParamTypes: TParameterTypes): integer;
var
  Index: Integer;
  Param: TModflowSteadyParameter;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Param := Items[Index];
    if Param.ParameterType in ParamTypes then
    begin
      Inc(result);
    end;
  end;
end;

constructor TModflowSteadyParameters.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TModflowSteadyParameter, Model);
  FArrayNames:= TStringList.Create;
  FArrayNames.Sorted := True;
end;

destructor TModflowSteadyParameters.Destroy;
begin
  FArrayNames.Free;
  inherited;
end;

function TModflowSteadyParameters.GetItems(
  Index: integer): TModflowSteadyParameter;
begin
  result := inherited Items[Index] as TModflowSteadyParameter
end;

function TModflowSteadyParameters.GetParamByName(
  const AName: string): TModflowSteadyParameter;
var
  Index: Integer;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    if SameText(AName, Items[Index].ParameterName) then
    begin
      result :=  Items[Index];
      Exit;
    end;
  end;
end;

function TModflowSteadyParameters.IsArrayNameUnique(
  const ArrayName: string): boolean;
begin
  result := FArrayNames.IndexOf(ArrayName) < 0;
end;

function TModflowSteadyParameters.IsDataSetUsed(AnObject: TObject): boolean;
var
  Index: Integer;
  Item: TModflowSteadyParameter;
  DataArray : TDataArray;
begin
  DataArray := AnObject as TDataArray;
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    result := Item.UseMultiplier and (Item.MultiplierName = DataArray.Name);
    if result then
    begin
      break;
    end;
    result := Item.UseZone and (Item.ZoneName = DataArray.Name);
    if result then
    begin
      break;
    end;
  end;
end;

procedure TModflowSteadyParameters.RemoveOldDataSetVariables;
var
  Index: Integer;
  Param: TModflowSteadyParameter;
begin
  for Index := 0 to Count - 1 do
  begin
    Param := Items[Index];
    Param.RemoveOldDataSetVariables;
  end;
end;

function TModflowSteadyParameters.ArrayNameCount: integer;
begin
  result := FArrayNames.Count;
end;

procedure TModflowSteadyParameters.SetAllParametersUnused;
var
  ParamIndex: Integer;
begin
  for ParamIndex := 0 to Count - 1 do
  begin
    Items[ParamIndex].UsedDirectly := False;
  end;
end;

procedure TModflowSteadyParameters.SetItems(Index: integer;
  const Value: TModflowSteadyParameter);
begin
  inherited Items[Index] := Value;
end;

initialization
  UsedMultiplierArrayNames := TStringList.Create;
  UsedZoneArrayNames := TStringList.Create;
  UsedMultiplierArrayNames.Sorted := True;
  UsedZoneArrayNames.Sorted := True;

finalization
  UsedMultiplierArrayNames.Free;
  UsedZoneArrayNames.Free;

end.
