unit RequiredDataSetsUndoUnit;

interface

uses Classes, GoPhastTypes, UndoItems, PhastModelUnit, frmShowHideObjectsUnit,
  ModflowParameterUnit, HufDefinition, ModflowTransientListParameterUnit,
  ModflowPackageSelectionUnit, ModflowOutputControlUnit, PestParamGroupsUnit,
  DataArrayInterfaceUnit;

type
  TCustomCreateRequiredDataSetsUndo = class(TCustomUndo)
  private
    FNewSteadyModflowParameterDataSets: TIDataArrayList;
    FNewPackageDataSets: TList;
  // If a @link(TDataArray) whose name is DataSetName does not exist and
  // ArrayNeeded returns @true, then @name will create a new
  // @link(TDataArray). Orientation, DataType, DataSetName, ArrayNeeded,
  // NewFormula, and Classification will be used to assign properties to
  // the new @link(TDataArray).
  //
  // @name is called by @link(UpdatePackageLayers).
//    procedure UpdateDataArray(Model: TCustomModel; Index: integer);
    // @name checks if all the data sets for the selected packages
    // exist and creates them if they do not.
    procedure UpdatePackageLayers;
    procedure UpdateOnPostInitialize;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    // causes all the @link(TDataArray)s that are required to be created
    // if they do not already exist.
    procedure UpdatedRequiredDataSets;
  end;

  TCustomUndoChangeParameters = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewSteadyParameters: TModflowSteadyParameters;
    FOldSteadyParameters: TModflowSteadyParameters;
    FNewTransientParameters: TModflowTransientListParameters;
    FOldTransientParameters: TModflowTransientListParameters;
    FOldHufModflowParameters: THufModflowParameters;
    FNewHufModflowParameters: THufModflowParameters;
    FNewSfrParamInstances: TSfrParamInstances;
    FOldSfrParamInstances: TSfrParamInstances;
    FOldParamGroups: TPestParamGroups;
    FNewParamGroups: TPestParamGroups;
    FOldModflowBoundaries: TList;
    FScreenObjects: TList;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters;
      var NewHufModflowParameters: THufModflowParameters;
      var NewSfrParamInstances: TSfrParamInstances;
      var NewParamGroups: TPestParamGroups);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TUndoModelSelectionChange = class(TCustomCreateRequiredDataSetsUndo)
  private
    FOldModelSelection: TModelSelection;
    FNewModelSelection: TModelSelection;
    FUpwSelected: boolean;
    FLpfSelected: boolean;
    FNwtSelected: boolean;
    FPcgSelected: boolean;
    FPcgnSelected: boolean;
    FOldChildModels: TChildModelEditCollection;
    FNewChildModels: TChildModelEditCollection;
    FNpfSelected: boolean;
    FBcfSelected: Boolean;
    FHufSelected: Boolean;
    FSipSelected: Boolean;
    FGmgSelected: Boolean;
    FDe4Selected: Boolean;
    FSmsSelected: Boolean;
    FHeadOutputFileType: TOutputFileType;
    FStoragePackageIsSelected: Boolean;
  protected
    function Description: string; override;
    procedure UpdateCellTypeMf6;
  public
    Constructor Create(NewModelSelection: TModelSelection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses DataSetUnit, frmGoPhastUnit, frmGridValueUnit, ScreenObjectUnit,
  ModflowPackagesUnit, frmDisplayDataUnit, System.SysUtils, contnrs,
  DataSetNamesUnit;

resourcestring
  StrChangeModelSelecti = 'change model selection';
  StrChangeParameters = 'Change parameters';

constructor TCustomCreateRequiredDataSetsUndo.Create;
begin
  FNewSteadyModflowParameterDataSets := TIDataArrayList.Create;
  FNewPackageDataSets := TList.Create;
end;

destructor TCustomCreateRequiredDataSetsUndo.Destroy;
begin
  FNewPackageDataSets.Free;
  FNewSteadyModflowParameterDataSets.Free;

  inherited;
end;

procedure TCustomCreateRequiredDataSetsUndo.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets :=
    FNewSteadyModflowParameterDataSets;
end;

procedure TCustomCreateRequiredDataSetsUndo.Undo;
begin
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets :=
    FNewSteadyModflowParameterDataSets;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdatePackageLayers;
var
  Model: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Model:= frmGoPhast.PhastModel;

  Model.DataArrayManager.CreateInitialDataSets;
  for ChildIndex := 0 to Model.ChildModels.Count - 1 do
  begin
    ChildModel := Model.ChildModels[ChildIndex].ChildModel;
    ChildModel.DataArrayManager.CreateInitialDataSets;
  end;
  UpdateFrmDisplayData;
  UpdateFrmGridValue;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdateOnPostInitialize;
begin
  frmGoPhast.PhastModel.UpdateOnPostInitialize;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdatedRequiredDataSets;
begin
  UpdatePackageLayers;
  UpdateOnPostInitialize;
  if frmShowHideObjects <> nil then
  begin
    frmShowHideObjects.UpdateScreenObjects;
  end;
  UpdateFrmGridValue;
end;

{ TUndoModelSelectionChange }

constructor TUndoModelSelectionChange.Create(NewModelSelection: TModelSelection);
var
  Packages: TModflowPackages;
  ChildIndex: Integer;
  Item: TChildModelEdit;
  DisIndex: Integer;
  DisItem: TChildDiscretization;
begin
  inherited Create;
  FOldModelSelection := frmGoPhast.ModelSelection;
  FNewModelSelection := NewModelSelection;
  FHeadOutputFileType := frmGoPhast.PhastModel.ModflowOutputControl.HeadOC.OutputFileType;
  FStoragePackageIsSelected := frmGoPhast.PhastModel.ModflowPackages.StoPackage.IsSelected;

  Packages := frmGoPhast.PhastModel.ModflowPackages;
  FUpwSelected := Packages.UpwPackage.IsSelected;
  FLpfSelected := Packages.LpfPackage.IsSelected;
  FNpfSelected := Packages.NpfPackage.IsSelected;
  FBcfSelected := Packages.BcfPackage.IsSelected;
  FHufSelected := Packages.HufPackage.IsSelected;

  FNwtSelected := Packages.NwtPackage.IsSelected;
  FPcgSelected := Packages.PcgPackage.IsSelected;
  FPcgnSelected := Packages.PcgnPackage.IsSelected;
  FSipSelected := Packages.SipPackage.IsSelected;
  FGmgSelected := Packages.GmgPackage.IsSelected;
  FDe4Selected := Packages.De4Package.IsSelected;
  FSmsSelected := Packages.SmsPackage.IsSelected;

  FOldChildModels := TChildModelEditCollection.Create;
  FOldChildModels.Capacity := frmGoPhast.PhastModel.ChildModels.Count;
  FOldChildModels.Assign(frmGoPhast.PhastModel.ChildModels);

  FNewChildModels := TChildModelEditCollection.Create;
  FNewChildModels.Capacity := frmGoPhast.PhastModel.ChildModels.Count;
  FNewChildModels.Assign(frmGoPhast.PhastModel.ChildModels);

  if NewModelSelection in [msModflowLGR] then
  begin
    for ChildIndex := 0 to FNewChildModels.Count - 1 do
    begin
      Item := (FNewChildModels.Items[ChildIndex] as TChildModelEdit);
      if not Odd(Item.ChildCellsPerParentCell) then
      begin
        Item.ChildCellsPerParentCell := Item.ChildCellsPerParentCell + 1;
      end;
      for DisIndex := 0 to Item.Discretization.Count - 1 do
      begin
        DisItem := Item.Discretization.Items[DisIndex];
        if not Odd(DisItem.Discretization) then
        begin
          DisItem.Discretization := DisItem.Discretization + 1;
        end;
      end;
    end;
  end;

end;

function TUndoModelSelectionChange.Description: string;
begin
  result := StrChangeModelSelecti;
end;

destructor TUndoModelSelectionChange.Destroy;
begin
  FOldChildModels.Free;
  FNewChildModels.Free;
  inherited;
end;

procedure TUndoModelSelectionChange.DoCommand;
var
  PhastModel: TPhastModel;
begin
  inherited;
  frmGoPhast.tbSelectClick(frmGoPhast.tbSelect);

  PhastModel := frmGoPhast.PhastModel;
  if (FNewModelSelection = msModflow2015) 
    and PhastModel.ModflowPackages.NwtPackage.IsSelected then
  begin
    PhastModel.ModflowOptions.NewtonMF6 := True;
  end;
  
  PhastModel.ModelSelection := FNewModelSelection;
  PhastModel.ChildModels.Assign(FNewChildModels);
  if (PhastModel.ModelSelection = msModflow2015)
    and PhastModel.ModflowStressPeriods.TransientModel then
  begin
    PhastModel.ModflowPackages.StoPackage.IsSelected := True;
  end;
  UpdatedRequiredDataSets;
  UpdateCellTypeMf6;

  if (PhastModel.ModelSelection in ModflowSelection) and (PhastModel.Grid <> nil) then
  begin
    PhastModel.ModflowGrid.UpdateCellElevations;
  end;
end;

procedure TUndoModelSelectionChange.Undo;
var
  Packages: TModflowPackages;
begin
  inherited;
  frmGoPhast.tbSelectClick(frmGoPhast.tbSelect);
  frmGoPhast.ModelSelection := FOldModelSelection;
  frmGoPhast.PhastModel.ModflowOutputControl.HeadOC.OutputFileType := FHeadOutputFileType;
  frmGoPhast.PhastModel.ModflowPackages.StoPackage.IsSelected := FStoragePackageIsSelected;

  frmGoPhast.PhastModel.ChildModels.Assign(FOldChildModels);
  Packages := frmGoPhast.PhastModel.ModflowPackages;
  Packages.UpwPackage.IsSelected := FUpwSelected;
  Packages.LpfPackage.IsSelected := FLpfSelected;
  Packages.NpfPackage.IsSelected := FNpfSelected;
  Packages.BcfPackage.IsSelected := FBcfSelected;
  Packages.HufPackage.IsSelected := FHufSelected;

  Packages.NwtPackage.IsSelected := FNwtSelected;
  Packages.PcgPackage.IsSelected := FPcgSelected;
  Packages.PcgnPackage.IsSelected := FPcgnSelected;
  Packages.SipPackage.IsSelected := FSipSelected;
  Packages.GmgPackage.IsSelected := FGmgSelected;
  Packages.De4Package.IsSelected := FDe4Selected;
  Packages.SmsPackage.IsSelected := FSmsSelected;

  UpdatedRequiredDataSets;
  if (frmGoPhast.PhastModel.ModelSelection in ModflowSelection)
    and (frmGoPhast.PhastModel.Grid <> nil) then
  begin
    frmGoPhast.PhastModel.ModflowGrid.UpdateCellElevations;
  end;
end;

procedure TUndoModelSelectionChange.UpdateCellTypeMf6;
var
  CellTypeDataArray: TDataArray;
  LayerCount: Integer;
  Laytyp: TOneDIntegerArray;
  Formula : TStringBuilder;
  LayerIndex: Integer;
  FirstValue: Integer;
  Uniform: Boolean;
begin
  if (FOldModelSelection in ModflowSelection)
    and (FNewModelSelection = msModflow2015) then
  begin
    CellTypeDataArray := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(KCellType);
    Assert(CellTypeDataArray <> nil);
    Laytyp := frmGoPhast.PhastModel.LayerStructure.Laytyp;
    LayerCount := Length(Laytyp);
    if LayerCount > 0 then
    begin
      FirstValue := Laytyp[0];
      Uniform := True;
      for LayerIndex := 1 to LayerCount - 1 do
      begin
        Uniform := FirstValue = Laytyp[LayerIndex];
        if not Uniform then
        begin
          Break;
        end;
      end;

      if Uniform then
      begin
        CellTypeDataArray.Formula := FirstValue.ToString;
      end
      else
      begin
        Formula := TStringBuilder.Create;
        try
          Formula.Append('IfI((Layer > ');
          Formula.Append(LayerCount);
          Formula.Append('), 0, CaseI(Layer');
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            Formula.Append(', ');
            Formula.Append(Laytyp[LayerIndex]);
          end;
          Formula.Append('))');

          CellTypeDataArray.Formula := Formula.ToString;
        finally
          Formula.Free;
        end;
      end;
    end;

  end;
end;

{ TUndoChangeParameters }

constructor TCustomUndoChangeParameters.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var NewHufModflowParameters: THufModflowParameters;
  var NewSfrParamInstances: TSfrParamInstances;
  var NewParamGroups: TPestParamGroups);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  OldBoundary: TModflowBoundaries;
  InvalidateModelEvent: TNotifyEvent;
begin
  inherited Create;
  FOldModflowBoundaries := TObjectList.Create;
  FScreenObjects := TList.Create;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    FScreenObjects.Add(ScreenObject);
    OldBoundary := TModflowBoundaries.Create(nil);
    OldBoundary.Model := nil;
    OldBoundary.Assign(ScreenObject.ModflowBoundaries);
    FOldModflowBoundaries.Add(OldBoundary);
  end;

  FNewSteadyParameters:= NewSteadyParameters;
  // TCustomUndoChangeParameters takes ownership of NewSteadyParameters.
  NewSteadyParameters := nil;

  FNewTransientParameters := NewTransientParameters;
  // TCustomUndoChangeParameters takes ownership of NewTransientParameters.
  NewTransientParameters := nil;

  // TCustomUndoChangeParameters takes ownership of NewHufModflowParameters.
  FNewHufModflowParameters := NewHufModflowParameters;
  NewHufModflowParameters := nil;

  FNewSfrParamInstances := NewSfrParamInstances;
  NewSfrParamInstances := nil;

  FNewParamGroups := NewParamGroups;
  NewParamGroups := nil;

  FOldSteadyParameters:= TModflowSteadyParameters.Create(nil);
  FOldSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FOldTransientParameters:= TModflowTransientListParameters.Create(nil);
  FOldTransientParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
  FOldHufModflowParameters := THufModflowParameters.Create(nil);
  FOldHufModflowParameters.Assign(frmGoPhast.PhastModel.HufParameters);
  FOldSfrParamInstances := TSfrParamInstances.Create(nil);
  FOldSfrParamInstances.Assign(frmGoPhast.PhastModel.ModflowPackages.
    SfrPackage.ParameterInstances);

  InvalidateModelEvent := nil;
  FOldParamGroups := TPestParamGroups.Create(InvalidateModelEvent);
  FOldParamGroups.Assign(frmGoPhast.PhastModel.ParamGroups);

//  FExistingScreenObjects := TScreenObjectEditCollection.Create;
//  FExistingScreenObjects.OwnScreenObject := False;


end;

function TCustomUndoChangeParameters.Description: string;
begin
  result := StrChangeParameters;
end;

destructor TCustomUndoChangeParameters.Destroy;
begin
  FOldParamGroups.Free;
  FNewParamGroups.Free;

  FOldHufModflowParameters.Free;
  FNewHufModflowParameters.Free;
  FNewSteadyParameters.Free;
  FOldSteadyParameters.Free;
  FNewTransientParameters.Free;
  FOldTransientParameters.Free;
  FNewSfrParamInstances.Free;
  FOldSfrParamInstances.Free;
  FOldModflowBoundaries.Free;
  FScreenObjects.Free;
//  FExistingScreenObjects.Free;
  inherited;
end;

procedure TCustomUndoChangeParameters.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  frmGoPhast.PhastModel.ClearPestParmDictionary;
  frmGoPhast.PhastModel.ModflowSteadyParameters.ClearNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters := FNewSteadyParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveOldDataSetVariables;
  frmGoPhast.PhastModel.ModflowTransientParameters := FNewTransientParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;
  frmGoPhast.PhastModel.HufParameters := FNewHufModflowParameters;
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances :=
    FNewSfrParamInstances;

  if FNewParamGroups <> nil then
  begin
    frmGoPhast.PhastModel.ParamGroups := FNewParamGroups
  end;

  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ChildModel.ModflowPackages.SfrPackage.ParameterInstances := FNewSfrParamInstances;
  end;
  UpdatedRequiredDataSets;
end;

procedure TCustomUndoChangeParameters.Undo;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  OldBoundary: TModflowBoundaries;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  frmGoPhast.PhastModel.ClearPestParmDictionary;
  frmGoPhast.PhastModel.ModflowSteadyParameters  := FOldSteadyParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveOldDataSetVariables;
  frmGoPhast.PhastModel.ModflowTransientParameters := FOldTransientParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;
  frmGoPhast.PhastModel.HufParameters := FOldHufModflowParameters;
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances :=
    FOldSfrParamInstances;
  frmGoPhast.PhastModel.ParamGroups := FOldParamGroups;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ChildModel.ModflowPackages.SfrPackage.ParameterInstances := FOldSfrParamInstances;
  end;
  UpdatedRequiredDataSets;
  for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[ScreenObjectIndex];
    OldBoundary := FOldModflowBoundaries[ScreenObjectIndex];
    ScreenObject.ModflowBoundaries.Model := frmGoPhast.PhastModel;
    ScreenObject.ModflowBoundaries.Assign(OldBoundary);
    ScreenObject.ModflowBoundaries.Model := nil;
  end;
end;

end.
