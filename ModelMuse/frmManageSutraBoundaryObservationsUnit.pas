unit frmManageSutraBoundaryObservationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  JvExStdCtrls, JvListBox, ArgusDataEntry,
  RbwDataGrid4, JvExComCtrls, JvComCtrls, JvEdit,
  Vcl.ComCtrls, RbwParser, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus, JvExExtCtrls,
  JvNetscapeSplitter, framePestObsUnit, FluxObservationUnit, SutraPestObsUnit,
  ScreenObjectUnit, JvBoxProcs, UndoItems, System.UITypes, System.Types;

type
  TfrmManageSutraBoundaryObservations = class(TfrmCustomGoPhast)
    spltr1: TJvNetscapeSplitter;
    pmSelectEditAvailable: TPopupMenu;
    miSelectAvailable: TMenuItem;
    miEditAvailable: TMenuItem;
    miGotoAvailable: TMenuItem;
    miHideAvailable: TMenuItem;
    pmSelectEditUsed: TPopupMenu;
    miSelectUsed: TMenuItem;
    miEditUsed: TMenuItem;
    miGoToUsed: TMenuItem;
    miHideUsed: TMenuItem;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnDeleteObservation: TButton;
    btnAddObservation: TButton;
    rparserThreeDFormulaElements: TRbwParser;
    tvFluxObservations: TTreeView;
    pnlMain: TPanel;
    pnlTop: TPanel;
    lblObservationName: TLabel;
    lblTreatment: TLabel;
    edObservationName: TJvEdit;
    comboTreatment: TComboBox;
    pcMain: TJvPageControl;
    tabObservationsTimes: TTabSheet;
    tabObjects: TTabSheet;
    lblSrcLabel: TLabel;
    lblDstLabel: TLabel;
    lblFactor: TLabel;
    lbSrcList: TJvListBox;
    btnIncBtn: TButton;
    btnIncAllBtn: TButton;
    btnExclBtn: TButton;
    btnExclAllBtn: TButton;
    lbDstList: TJvListBox;
    edFactorFormula: TJvEdit;
    btnFactorFormula: TButton;
    frameSutraFluxObs: TframePestObs;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnIncBtnClick(Sender: TObject);
    procedure btnIncAllBtnClick(Sender: TObject);
    procedure btnExclBtnClick(Sender: TObject);
    procedure btnExclAllBtnClick(Sender: TObject);
    procedure lbSrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbSrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure lbDstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbDstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbDstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAddObservationClick(Sender: TObject);
    procedure btnDeleteObservationClick(Sender: TObject);
    procedure edObservationNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edFactorFormulaChange(Sender: TObject);
    procedure edObservationNameExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFluxObservationsChange(Sender: TObject; Node: TTreeNode);
    procedure pmSelectEditUsedPopup(Sender: TObject);
    procedure pmSelectEditAvailablePopup(Sender: TObject);
    procedure miSelectAvailableClick(Sender: TObject);
    procedure miEditAvailableClick(Sender: TObject);
    procedure miGotoAvailableClick(Sender: TObject);
    procedure miHideAvailableClick(Sender: TObject);
    procedure miSelectUsedClick(Sender: TObject);
    procedure miEditUsedClick(Sender: TObject);
    procedure miGoToUsedClick(Sender: TObject);
    procedure miHideUsedClick(Sender: TObject);
    procedure btnFactorFormulaClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFluxObs: TSutraFluxObs;
    FSpecPresObjects: TScreenObjectList;
    FSpecFluidFlowObjects: TScreenObjectList;
    FSpecConcObjects: TScreenObjectList;
    FGenFluidFlowObjects: TScreenObjectList;
    FGenTransObjects: TScreenObjectList;
    FSelectedObservation: TCustomSutraFluxObservations;
    FSpecPresNode: TTreeNode;
    FFluidFlowNode: TTreeNode;
    FSpecConcNode: TTreeNode;
    FGenFlowNode: TTreeNode;
    FGenTransNode: TTreeNode;
    FSelectedGroup: TCustomSutraFluxObservationGroup;
    FUpdatingFormula: Boolean;
    procedure SetSelectedGroup(const Value: TCustomSutraFluxObservationGroup);
    property SelectedGroup: TCustomSutraFluxObservationGroup read FSelectedGroup write SetSelectedGroup;
    procedure GetData;
    procedure SetData;
    procedure SetSelectedObservation(const Value: TCustomSutraFluxObservations);
    property SelectedObservation: TCustomSutraFluxObservations
      read FSelectedObservation write SetSelectedObservation;
    procedure DisplayObservation(Value: TCustomSutraFluxObservations);
    procedure RecordObservation(Value: TCustomSutraFluxObservations);
    procedure UpdateObjectsInSelectedObservation;
    procedure DisplayFactor;
    procedure SetSelectedGroupAndObservation(TreeView: TTreeView);
    procedure AssignObsNames;
    procedure UpdateFactor;
    function CheckFormula(FunctionString: string; ShowError: boolean): boolean;
    procedure AssignFactor(NewFormula: string);
    procedure SelectObjects(ListBox: TJvListBox);
    procedure GetSelectedObjects(ListBox: TJvListBox; ScreenObjects: TScreenObjectList);
    procedure EditAnObject(ListBox: TJvListBox);
    procedure GoToAnObject(ListBox: TJvListBox);
    procedure HideObjects(ListBox: TJvListBox);
    procedure GetGlobalVariables;
    procedure CreateVariables;
    { Private declarations }
  public
    procedure SetButtons;
    { Public declarations }
  end;

  TUndoEditSutraFluxObservations = class(TCustomUndo)
  private
    FOldSutraFluxObs: TSutraFluxObs;
    FNewSutraFluxObs: TSutraFluxObs;
  protected
    function Description: string; override;
    procedure AssignNewObservations(NewSutraFluxObs: TSutraFluxObs);
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;


var
  frmManageSutraBoundaryObservations: TfrmManageSutraBoundaryObservations;

implementation

uses
  frmGoPhastUnit, SutraBoundariesUnit, UndoItemsScreenObjects, frmGoToUnit,
  GIS_Functions, GoPhastTypes, DataSetUnit, PhastModelUnit, frmFormulaUnit;

resourcestring
  StrErrorInFormulaS = 'Error in formula: %s';
  StrErrorTheFormulaI = 'Error: the formula is does not result in a real num' +
  'ber';
  StrDoYouWantToSave = 'Do you want to save your changes first?';
  StrEditSUTRAFluxObse = 'edit SUTRA flux observations';
  StrSpecPresObs = 'Spec Pres Obs';
  StrSpecFluidFlowObs = 'Spec Fluid Flow Obs';
  StrSpecConcObs = 'Spec Conc Obs';
  StrGenFlowObs = 'Gen Flow Obs';
  StrGenTransportObs = 'Gen Transport Obs';

{$R *.dfm}

procedure TfrmManageSutraBoundaryObservations.AssignFactor(NewFormula: string);
var
  FactorObject: TObservationFactor;
  ObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  for Index := 0 to lbDstList.Items.Count - 1 do
  begin
    if lbDstList.Selected[Index] then
    begin
      ScreenObject := lbDstList.Items.Objects[Index] as TScreenObject;
      ObjectIndex := FSelectedObservation.ObservationFactors.
        IndexOfScreenObject(ScreenObject);
      if (ObjectIndex >= 0) then
      begin
        FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
        FactorObject.Factor := NewFormula;
      end;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.AssignObsNames;
begin
  // do nothing
end;

procedure TfrmManageSutraBoundaryObservations.btnAddObservationClick(
  Sender: TObject);
var
  ANode: TTreeNode;
  ObsName: string;
  ParentNode : TTreeNode;
  NodeList: TList;
  Observations: TCustomSutraFluxObservationGroups;
  ObservationGroup: TCustomSutraFluxObservations;
  ObsItem: TCustomSutraFluxObservationGroup;
begin
  inherited;
  NodeList := TList.Create;
  try
    NodeList.Add(FSpecPresNode);
    NodeList.Add(FFluidFlowNode);
    NodeList.Add(FSpecConcNode);
    NodeList.Add(FGenFlowNode);
    NodeList.Add(FGenTransNode);
    NodeList.Pack;
    if (tvFluxObservations.Selected = nil) and (NodeList.Count > 0) then
    begin
      tvFluxObservations.Selected := NodeList[0];
    end;
    if tvFluxObservations.Selected = nil then
    begin
      Exit;
    end;
    if NodeList.IndexOf(tvFluxObservations.Selected) >= 0 then
    begin
      ParentNode := tvFluxObservations.Selected;
    end
    else
    begin
      ParentNode := tvFluxObservations.Selected.Parent;
    end;
  finally
    NodeList.Free;
  end;

  if ParentNode = FSpecPresNode then
  begin
    ObsName := 'SpecPresObs';
  end
  else if ParentNode = FFluidFlowNode then
  begin
    ObsName := 'SpecFlowObs';
  end
  else if ParentNode = FSpecConcNode then
  begin
    ObsName := 'SpecConcObs';
  end
  else if ParentNode = FGenFlowNode then
  begin
    ObsName := 'GenFlowObs';
  end
  else if ParentNode = FGenTransNode then
  begin
    ObsName := 'GenTransObs';
  end
  else
  begin
    Assert(False);
  end;

  Observations := ParentNode.Data;
  ObsItem  := Observations.Add as TCustomSutraFluxObservationGroup;
  ObservationGroup := ObsItem.ObservationGroup;
  ObservationGroup.ObservationName := ObsName
    + IntToStr(ParentNode.Count+1);
  ANode := tvFluxObservations.Items.AddChild(ParentNode,
    ObservationGroup.ObservationName);
  ANode.Data := ObsItem;
  tvFluxObservations.Selected := ANode;
  SetSelectedGroupAndObservation(tvFluxObservations);
end;

procedure TfrmManageSutraBoundaryObservations.btnDeleteObservationClick(
  Sender: TObject);
var
  ParentNode: TTreeNode;
  Observations: TCustomSutraFluxObservationGroups;
  Item: TCustomSutraFluxObservationGroup;
  AnObject: TObject;
begin
  inherited;
  Assert(tvFluxObservations.Selected <> nil);
  AnObject := tvFluxObservations.Selected.Data;
  Assert(AnObject is TCustomSutraFluxObservationGroup);
  Item := tvFluxObservations.Selected.Data;
  ParentNode := tvFluxObservations.Selected.Parent;
  Assert(ParentNode <> nil);
  Observations := ParentNode.Data;
  tvFluxObservations.Items.Delete(tvFluxObservations.Selected);
  SelectedGroup := nil;
  SelectedObservation := nil;
  Observations.Remove(Item);
end;

procedure TfrmManageSutraBoundaryObservations.btnExclAllBtnClick(
  Sender: TObject);
begin
  BoxMoveAllItems(lbDstList, lbSrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageSutraBoundaryObservations.btnExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(lbDstList, lbSrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageSutraBoundaryObservations.btnFactorFormulaClick(
  Sender: TObject);
var
  FirstScreenObject: TScreenObject;
  Index: Integer;
  Variable: TCustomValue;
  FunctionString: string;
  ObjectIndex: Integer;
  FactorObject: TObservationFactor;
begin
  inherited;
  FirstScreenObject := nil;
  for Index := 0 to lbDstList.Items.Count - 1 do
  begin
    if lbDstList.Selected[Index] then
    begin
      FirstScreenObject := lbDstList.Items.Objects[Index] as TScreenObject;
      break;
    end;
  end;
  if FirstScreenObject = nil then
  begin
    Exit;
  end;

  FunctionString := edFactorFormula.Text;
  if FunctionString = '' then
  begin
    ObjectIndex := FSelectedObservation.ObservationFactors.
      IndexOfScreenObject(FirstScreenObject);
    Assert(ObjectIndex >= 0);
    FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
    FunctionString := FactorObject.Factor;
  end;

//  with TfrmFormula.Create(nil) do
  with frmFormula do
  begin
    try
      Initialize;
      IncludeGIS_Functions(eaBlocks);
      RemoveGetVCont;
      RemoveHufFunctions;
      PopupParent := self;

      for Index := 0 to rparserThreeDFormulaElements.VariableCount - 1 do
      begin
        Variable := rparserThreeDFormulaElements.Variables[Index];
        if rbFormulaParser.IndexOfVariable(Variable.Name) < 0 then
        begin
          rbFormulaParser.RegisterVariable(Variable);
        end;
      end;

      IncludeTimeSeries := False;
      UpdateTreeList;
      Formula := FunctionString;

      ShowModal;
      if ResultSet then
      begin
        FunctionString := Formula;
      end
      else
      begin
        if FunctionString = '' then
          FunctionString := '0';
      end;
    finally
      Initialize;
//      Free;
    end;
  end;

  CheckFormula(FunctionString, True)
end;

procedure TfrmManageSutraBoundaryObservations.btnIncAllBtnClick(
  Sender: TObject);
begin
  BoxMoveAllItems(lbSrcList, lbDstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageSutraBoundaryObservations.btnIncBtnClick(Sender: TObject);
begin
  inherited;
  BoxMoveSelectedItems(lbSrcList, lbDstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageSutraBoundaryObservations.btnOkClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

function TfrmManageSutraBoundaryObservations.CheckFormula(
  FunctionString: string; ShowError: boolean): boolean;
var
  CompiledFormula: TExpression;
begin
  result := True;
  try
    rparserThreeDFormulaElements.Compile(FunctionString);
  except on E: ErbwParserError do
    begin
      edFactorFormula.Color := clRed;
      if ShowError then
      begin
        Beep;
        MessageDlg(Format(StrErrorInFormulaS, [E.Message]), mtError, [mbOK], 0);
      end;
      result := False;
      Exit;
    end
  end;

  CompiledFormula := rparserThreeDFormulaElements.CurrentExpression;
  // check that the formula is OK.
  if not (CompiledFormula.ResultType in [rdtDouble, rdtInteger]) then
  begin
    edFactorFormula.Color := clRed;
    if ShowError then
    begin
      Beep;
      MessageDlg(StrErrorTheFormulaI,
        mtError, [mbOK], 0);
    end;
    result := False;
  end
  else
  begin
    edFactorFormula.Color := clWindow;
    if ShowError then
    begin
      FunctionString := CompiledFormula.Decompile;
      if FunctionString <> edFactorFormula.Text then
      begin
        edFactorFormula.Text := FunctionString;
        if Assigned(edFactorFormula.OnChange) then
        begin
          edFactorFormula.OnChange(edFactorFormula);
        end;
        if Assigned(edFactorFormula.OnExit) then
        begin
          edFactorFormula.OnExit(edFactorFormula);
        end;
      end;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.CreateVariables;
var
  Index: Integer;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataArray := DataArrayManager.DataSets[Index];
    if not DataArray.Visible then
    begin
      Continue;
    end;
    if DataArray.EvaluatedAt = eaNodes then
    begin
      case DataArray.DataType of
        rdtDouble: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0.0, DataArray.DisplayName);
        rdtInteger: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0, DataArray.DisplayName);
        rdtBoolean: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, False, DataArray.DisplayName);
        rdtString: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, '', DataArray.DisplayName);
      end;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.DisplayFactor;
var
  FirstFormula: string;
  FoundFormula: boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
  ObjectIndex: integer;
  FactorObject: TObservationFactor;
begin
  edFactorFormula.Enabled := lbDstList.Enabled and (lbDstList.SelCount > 0);
  btnFactorFormula.Enabled := edFactorFormula.Enabled;
  if edFactorFormula.Enabled then
  begin
    FoundFormula := False;
    FirstFormula := '';
    for Index := 0 to lbDstList.Items.Count - 1 do
    begin
      if lbDstList.Selected[Index] then
      begin
        ScreenObject := lbDstList.Items.Objects[Index] as TScreenObject;
        if FSelectedObservation <> nil then
        begin
          ObjectIndex := FSelectedObservation.ObservationFactors.
            IndexOfScreenObject(ScreenObject);
          Assert(ObjectIndex >= 0);
          FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
          if FoundFormula then
          begin
            if FirstFormula <> FactorObject.Factor then
            begin
              FirstFormula := '';
            end;
          end
          else
          begin
            FirstFormula := FactorObject.Factor;
            FoundFormula := True;
          end;
        end;
      end;
    end;
    edFactorFormula.Text := FirstFormula;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.DisplayObservation(
  Value: TCustomSutraFluxObservations);
var
  AvailableObjects: TScreenObjectList;
  CurrentObjects: TList;
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  frameSutraFluxObs.InitializeControls;
  if Value <> nil then
  begin
    AvailableObjects := nil;
    if Value is TSutraGenPressObservations then
    begin
      // test for TSutraGenPressObservations before testing for
      // TSutraSpecPressureObservations because TSutraGenPressObservations
      // descends from TSutraGenPressObservations
      frameSutraFluxObs.SpecifyObservationTypes(SutraSpecPressureObsTypes);
      AvailableObjects := FGenFluidFlowObjects;
    end
    else if Value is TSutraSpecPressureObservations then
    begin
      frameSutraFluxObs.SpecifyObservationTypes(SutraSpecPressureObsTypes);
      AvailableObjects := FSpecPresObjects;
    end
    else if Value is TSutraFluidFlowObservations then
    begin
      frameSutraFluxObs.SpecifyObservationTypes(SutraSpecFluidFlowObsTypes);
      AvailableObjects := FSpecFluidFlowObjects;
    end
    else if Value is TSutraSpecConcObservations then
    begin
      frameSutraFluxObs.SpecifyObservationTypes(SutraSpecConcObsTypes);
      AvailableObjects := FSpecConcObjects;
    end
    else if Value is TSutraGenPressObservations then
    begin
      frameSutraFluxObs.SpecifyObservationTypes(SutraSpecPressureObsTypes);
      AvailableObjects := FGenFluidFlowObjects;
    end
    else if Value is TSutraGenTransObservations then
    begin
      frameSutraFluxObs.SpecifyObservationTypes(SutraGenUObsTypes);
      AvailableObjects := FGenTransObjects;
    end;
    Assert(AvailableObjects <> nil);
    edObservationName.Text := Value.ObservationName;
    frameSutraFluxObs.GetData(Value);
    CurrentObjects := TList.Create;
    try
      for Index := 0 to Value.ObservationFactors.Count - 1 do
      begin
        ScreenObject := Value.ObservationFactors[Index].
          ScreenObject as TScreenObject;
        CurrentObjects.Add(ScreenObject);
      end;
      lbSrcList.Items.Clear;
      lbDstList.Items.Clear;

      for Index := 0 to AvailableObjects.Count - 1 do
      begin
        ScreenObject := AvailableObjects[Index];
        if CurrentObjects.IndexOf(ScreenObject) >= 0 then
        begin
          lbDstList.Items.AddObject(ScreenObject.Name, ScreenObject);
          if ScreenObject.Selected then
          begin
            lbDstList.Selected[lbDstList.Count - 1] := True;
          end;
        end
        else
        begin
          lbSrcList.Items.AddObject(ScreenObject.Name, ScreenObject);
          if ScreenObject.Selected then
          begin
            lbSrcList.Selected[lbSrcList.Count - 1] := True;
          end;
        end;
      end;
    finally
      CurrentObjects.Free;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.edFactorFormulaChange(
  Sender: TObject);
begin
  inherited;
  UpdateFactor;
end;

procedure TfrmManageSutraBoundaryObservations.EditAnObject(ListBox: TJvListBox);
var
  ScreenObject: TScreenObject;
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count = 1 then
    begin
      ScreenObject := ScreenObjects[0];
      SelectAScreenObject(ScreenObject);
      if (MessageDlg(StrDoYouWantToSave, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        SetData;
      end;
      Close;
      frmGoPhast.EditScreenObjects;
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.edObservationNameChange(
  Sender: TObject);
var
  AnObject: TObject;
  AnItem: TCustomSutraFluxObservationGroup;
begin
  inherited;
  if (FSelectedObservation <> nil) then
  begin
    AnObject := tvFluxObservations.Selected.Data;
    Assert(AnObject is TCustomSutraFluxObservationGroup);
    AnItem := tvFluxObservations.Selected.Data;
    Assert(AnItem.ObservationGroup = FSelectedObservation);
    FSelectedObservation.ObservationName := string(AnsiString(edObservationName.Text));
    tvFluxObservations.Selected.Text := edObservationName.Text;
    AssignObsNames;
//    rdgGroupNames.Cells[1, FSelectedObservation.Index+1] :=
//      SelectedObservation.ObservationName;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.edObservationNameExit(
  Sender: TObject);
begin
  inherited;
  edObservationName.Text := string(AnsiString(StringReplace(edObservationName.Text,
    ' ', '_', [rfReplaceAll])));
end;

procedure TfrmManageSutraBoundaryObservations.FormCreate(Sender: TObject);
begin
  inherited;
  FFluxObs := TSutraFluxObs.Create(nil);
  FSpecPresObjects := TScreenObjectList.Create;
  FSpecFluidFlowObjects := TScreenObjectList.Create;
  FSpecConcObjects := TScreenObjectList.Create;
  FGenFluidFlowObjects := TScreenObjectList.Create;
  FGenTransObjects := TScreenObjectList.Create;
  frameSutraFluxObs.InitializeControls;

  AddGIS_Functions(rparserThreeDFormulaElements,
    frmGoPhast.PhastModel.ModelSelection, eaNodes);
  GetGlobalVariables;
  CreateVariables;

//  pcGroup.ActivePageIndex := 0;

  GetData;

end;

procedure TfrmManageSutraBoundaryObservations.FormDestroy(Sender: TObject);
begin
  inherited;
  FFluxObs.Free;
  FSpecFluidFlowObjects.Free;
  FSpecPresObjects.Free;
  FSpecConcObjects.Free;
  FGenFluidFlowObjects.Free;
  FGenTransObjects.Free;

end;

procedure TfrmManageSutraBoundaryObservations.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  btnIncBtn.Left := 4 + (tabObjects.Width - btnIncBtn.Width) div 2;
  btnIncAllBtn.Left := btnIncBtn.Left;
  btnExclBtn.Left := btnIncBtn.Left;
  btnExclAllBtn.Left := btnIncBtn.Left;
  lbSrcList.Width := (tabObjects.Width - (8 + 7 + btnIncBtn.Width + 7 + 8)) div 2;
  lblSrcLabel.Left := lbSrcList.Left;
  lbDstList.Width := lbSrcList.Width;
  lbDstList.Left := btnIncBtn.Left + btnIncBtn.Width + 7;
  lblDstLabel.Left := lbDstList.Left;
end;

procedure TfrmManageSutraBoundaryObservations.FormResize(Sender: TObject);
begin
  inherited;
  btnIncBtn.Left := 4 + (tabObjects.Width - btnIncBtn.Width) div 2;
  btnIncAllBtn.Left := btnIncBtn.Left;
  btnExclBtn.Left := btnIncBtn.Left;
  btnExclAllBtn.Left := btnIncBtn.Left;
  lbSrcList.Width := (tabObjects.Width - (8 + 7 + btnIncBtn.Width + 7 + 8)) div 2;
  lblSrcLabel.Left := lbSrcList.Left;
  lbDstList.Width := lbSrcList.Width;
  lbDstList.Left := btnIncBtn.Left + btnIncBtn.Width + 7;
  lblDstLabel.Left := lbDstList.Left;
end;

procedure TfrmManageSutraBoundaryObservations.FormShow(Sender: TObject);
begin
  inherited;
  ListClick(nil);
//  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options - [goEditing];
//  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options + [goEditing];
  pcMain.ActivePageIndex := 0;
end;

procedure TfrmManageSutraBoundaryObservations.GetData;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  SutraBoundaries: TSutraBoundaries;
  ParentNode: TTreeNode;
  SpecPresObsItem: TSutraSpecPressureObservationGroup;
  ANode: TTreeNode;
  SpecFlowObsItem: TSutraFluidFlowObservationGroup;
  SpecConcObsItem: TSutraSpecConcObservationGroup;
  SpecGenTransItem: TSutraGenTransObservationGroup;
  SpecGenFlowItem: TSutraGenPressureObservationGroup;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;

    SutraBoundaries := ScreenObject.SutraBoundaries;
    if SutraBoundaries.SpecifiedPressure.Used then
    begin
      FSpecPresObjects.Add(ScreenObject);
    end;
    if SutraBoundaries.FluidSource.Used then
    begin
      FSpecFluidFlowObjects.Add(ScreenObject);
    end;
    if SutraBoundaries.SpecifiedConcTemp.Used then
    begin
      FSpecConcObjects.Add(ScreenObject);
    end;
    if SutraBoundaries.GeneralFlowBoundary.Used then
    begin
      FGenFluidFlowObjects.Add(ScreenObject);
    end;
    if SutraBoundaries.GenTransportBoundary.Used then
    begin
      FGenTransObjects.Add(ScreenObject);
    end;
  end;

  FFluxObs.Assign(frmGoPhast.PhastModel.SutraFluxObs);

  ParentNode := tvFluxObservations.Items.Add(nil, StrSpecPresObs);
  ParentNode.Data := FFluxObs.SpecPres;
  FSpecPresNode := ParentNode;
  for Index := 0 to FFluxObs.SpecPres.Count - 1 do
  begin
    SpecPresObsItem := FFluxObs.SpecPres[Index];
    ANode := tvFluxObservations.Items.AddChild(ParentNode,
      SpecPresObsItem.ObsGroup.ObservationName);
    ANode.Data := SpecPresObsItem;
  end;

  ParentNode := tvFluxObservations.Items.Add(nil, StrSpecFluidFlowObs);
  ParentNode.Data := FFluxObs.FluidFlow;
  FFluidFlowNode := ParentNode;
  for Index := 0 to FFluxObs.FluidFlow.Count - 1 do
  begin
    SpecFlowObsItem := FFluxObs.FluidFlow[Index];
    ANode := tvFluxObservations.Items.AddChild(ParentNode,
      SpecFlowObsItem.ObsGroup.ObservationName);
    ANode.Data := SpecFlowObsItem;
  end;

  ParentNode := tvFluxObservations.Items.Add(nil, StrSpecConcObs);
  ParentNode.Data := FFluxObs.SpecConc;
  FSpecConcNode := ParentNode;
  for Index := 0 to FFluxObs.SpecConc.Count - 1 do
  begin
    SpecConcObsItem := FFluxObs.SpecConc[Index];
    ANode := tvFluxObservations.Items.AddChild(ParentNode,
      SpecConcObsItem.ObsGroup.ObservationName);
    ANode.Data := SpecConcObsItem;
  end;

  if frmGoPhast.ModelSelection in [msSutra30, msSutra40] then
  begin
    ParentNode := tvFluxObservations.Items.Add(nil, StrGenFlowObs);
    ParentNode.Data := FFluxObs.GenFlow;
    FGenFlowNode := ParentNode;
    for Index := 0 to FFluxObs.GenFlow.Count - 1 do
    begin
      SpecGenFlowItem := FFluxObs.GenFlow[Index];
      ANode := tvFluxObservations.Items.AddChild(ParentNode,
        SpecGenFlowItem.ObsGroup.ObservationName);
      ANode.Data := SpecGenFlowItem;
    end;

    ParentNode := tvFluxObservations.Items.Add(nil, StrGenTransportObs);
    ParentNode.Data := FFluxObs.GenTrans;
    FGenTransNode := ParentNode;
    for Index := 0 to FFluxObs.GenTrans.Count - 1 do
    begin
      SpecGenTransItem := FFluxObs.GenTrans[Index];
      ANode := tvFluxObservations.Items.AddChild(ParentNode,
        SpecGenTransItem.ObsGroup.ObservationName);
      ANode.Data := SpecGenTransItem;
    end;
  end
  else
  begin
    FGenFlowNode := nil;
    FGenTransNode := nil;
  end;

end;

procedure TfrmManageSutraBoundaryObservations.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    CompilerList.Add(rparserThreeDFormulaElements);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.GetSelectedObjects(
  ListBox: TJvListBox; ScreenObjects: TScreenObjectList);
var
  Index: Integer;
begin
  for Index := 0 to ListBox.Items.Count - 1 do
  begin
    if ListBox.Selected[Index] then
    begin
      ScreenObjects.Add(ListBox.Items.Objects[Index] as TScreenObject);
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.GoToAnObject(ListBox: TJvListBox);
var
  ScreenObject: TScreenObject;
  UndoShowHide: TUndoShowHideScreenObject;
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count = 1 then
    begin
      ScreenObject := ScreenObjects[0];
      if not ScreenObject.Visible then
      begin
        UndoShowHide := TUndoShowHideScreenObject.Create;
        UndoShowHide.AddScreenObjectToChange(ScreenObject);
        frmGoPhast.UndoStack.Submit(UndoShowHide);
      end;
      GoToObject(ScreenObject);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.HideObjects(ListBox: TJvListBox);
var
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count > 0 then
    begin
      HideMultipleScreenObjects(ScreenObjects);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.ListClick(Sender: TObject);
begin
  SetButtons;
  DisplayFactor;
end;

procedure TfrmManageSutraBoundaryObservations.miEditAvailableClick(
  Sender: TObject);
begin
  inherited;
  EditAnObject(lbSrcList);

end;

procedure TfrmManageSutraBoundaryObservations.miEditUsedClick(Sender: TObject);
begin
  inherited;
  EditAnObject(lbDstList);
end;

procedure TfrmManageSutraBoundaryObservations.miGotoAvailableClick(
  Sender: TObject);
begin
  inherited;
  GoToAnObject(lbSrcList);
end;

procedure TfrmManageSutraBoundaryObservations.miGoToUsedClick(Sender: TObject);
begin
  inherited;
  GoToAnObject(lbDstList);
end;

procedure TfrmManageSutraBoundaryObservations.miHideAvailableClick(
  Sender: TObject);
begin
  inherited;
  HideObjects(lbSrcList);
end;

procedure TfrmManageSutraBoundaryObservations.miHideUsedClick(Sender: TObject);
begin
  inherited;
  HideObjects(lbDstList);
end;

procedure TfrmManageSutraBoundaryObservations.miSelectAvailableClick(
  Sender: TObject);
begin
  inherited;
  SelectObjects(lbSrcList);
end;

procedure TfrmManageSutraBoundaryObservations.miSelectUsedClick(
  Sender: TObject);
begin
  inherited;
  SelectObjects(lbDstList);
end;

procedure TfrmManageSutraBoundaryObservations.pmSelectEditAvailablePopup(
  Sender: TObject);
begin
  inherited;
  miSelectAvailable.Enabled := lbSrcList.SelCount > 0;
  miEditAvailable.Enabled := lbSrcList.SelCount = 1;
  miGotoAvailable.Enabled := lbSrcList.SelCount = 1;
  miHideAvailable.Enabled := lbSrcList.SelCount > 0;
end;

procedure TfrmManageSutraBoundaryObservations.pmSelectEditUsedPopup(
  Sender: TObject);
begin
  inherited;
  miSelectUsed.Enabled := lbDstList.SelCount > 0;
  miEditUsed.Enabled := lbDstList.SelCount = 1;
  miGotoUsed.Enabled := lbDstList.SelCount = 1;
  miHideUsed.Enabled := lbDstList.SelCount > 0;
end;

procedure TfrmManageSutraBoundaryObservations.lbDstListDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if Source = lbSrcList then
    btnIncBtnClick(lbDstList)
  else
  if Source = lbDstList then
  begin
    BoxMoveFocusedItem(lbDstList, lbDstList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.lbDstListDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(lbDstList, Source, X, Y, State, Accept, lbDstList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageSutraBoundaryObservations.lbDstListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not lbDstList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(lbDstList, lbDstList.ItemIndex + Incr);
      UpdateObjectsInSelectedObservation;
      Key := 0;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.lbSrcListDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
//  inherited;
  if Source = lbDstList then
    btnExclBtnClick(lbSrcList)
  else
  if Source = lbSrcList then
  begin
    BoxMoveFocusedItem(lbSrcList, lbSrcList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.lbSrcListDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  BoxDragOver(lbSrcList, Source, X, Y, State, Accept, lbSrcList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageSutraBoundaryObservations.lbSrcListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not lbSrcList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(lbSrcList, lbSrcList.ItemIndex + Incr);
      Key := 0;
      UpdateObjectsInSelectedObservation;
    end;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.RecordObservation(
  Value: TCustomSutraFluxObservations);
begin
  if Value <> nil then
  begin
    Value.ObservationName := edObservationName.Text;
    frameSutraFluxObs.SetData(Value, edObservationName.Text);
  end;
end;

procedure TfrmManageSutraBoundaryObservations.SelectObjects(
  ListBox: TJvListBox);
var
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count > 0 then
    begin
      SelectMultipleScreenObjects(ScreenObjects);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (lbSrcList.Items.Count = 0);
  DstEmpty := (lbDstList.Items.Count = 0);
  btnIncBtn.Enabled := not SrcEmpty and (lbSrcList.SelCount > 0);
  btnIncAllBtn.Enabled := not SrcEmpty;
  btnExclBtn.Enabled := not DstEmpty and (lbDstList.SelCount > 0);
  btnExclAllBtn.Enabled := not DstEmpty;
end;

procedure TfrmManageSutraBoundaryObservations.SetData;
var
  Undo: TUndoEditSutraFluxObservations;
begin
  SelectedObservation := nil;
  Undo := TUndoEditSutraFluxObservations.Create;
  try
    Undo.AssignNewObservations(FFluxObs);
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.UndoStack.Submit(Undo);

end;

procedure TfrmManageSutraBoundaryObservations.SetSelectedGroup(
  const Value: TCustomSutraFluxObservationGroup);
begin
  FSelectedGroup := Value;
end;

procedure TfrmManageSutraBoundaryObservations.SetSelectedGroupAndObservation(
  TreeView: TTreeView);
var
  AnObject: TObject;
  GroupSelected: boolean;
  Item: TCustomSutraFluxObservationGroup;
begin
  GroupSelected := (TreeView.Selected <> nil)
    and
    ((TreeView.Selected.Data = FFluxObs.SpecPres)
    or (TreeView.Selected.Data = FFluxObs.FluidFlow)
    or (TreeView.Selected.Data = FFluxObs.SpecConc)
    or (TreeView.Selected.Data = FFluxObs.GenFlow)
    or (TreeView.Selected.Data = FFluxObs.GenTrans)
    );

  btnDeleteObservation.Enabled := (TreeView.Selected <> nil)
    and not GroupSelected;

//  if tabObservationsTimes.TabVisible then
//  begin
////    rdgFluxObsTimesExit(nil);
//  end;
  if (TreeView.Selected = nil) then
  begin
    SelectedGroup := nil;
    SelectedObservation := nil;
  end
  else
  begin
    AnObject := TreeView.Selected.Data;
    if AnObject is TCustomSutraFluxObservationGroups then
    begin
      SelectedGroup := TreeView.Selected.Data;
      SelectedObservation := nil;
    end
    else
    begin
      SelectedGroup := TreeView.Selected.Parent.Data;
      AnObject := TreeView.Selected.Data;
      Assert(AnObject is TCustomSutraFluxObservationGroup);
      Item := TreeView.Selected.Data;
      SelectedObservation := Item.ObservationGroup;
    end;
  end;
  DisplayFactor;
end;

procedure TfrmManageSutraBoundaryObservations.SetSelectedObservation(
  const Value: TCustomSutraFluxObservations);
begin
  if FSelectedObservation <> Value then
  begin
    RecordObservation(FSelectedObservation);
    FSelectedObservation := Value;
    DisplayObservation(FSelectedObservation);
  end;
  pcMain.Enabled := FSelectedObservation <> nil;
end;

procedure TfrmManageSutraBoundaryObservations.tvFluxObservationsChange(
  Sender: TObject; Node: TTreeNode);
begin
  inherited;
  SetSelectedGroupAndObservation(tvFluxObservations);
end;

procedure TfrmManageSutraBoundaryObservations.UpdateFactor;
var
  NewFormula: string;
begin
  if FUpdatingFormula then Exit;
  FUpdatingFormula := True;
  try
    NewFormula := edFactorFormula.Text;
    if (NewFormula = '') or not CheckFormula(NewFormula, False) then
    begin
      Exit;
    end;
    NewFormula := edFactorFormula.Text;
    AssignFactor(NewFormula);
  finally
    FUpdatingFormula := False;
  end;
end;

procedure TfrmManageSutraBoundaryObservations.UpdateObjectsInSelectedObservation;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  DestinationList: TList;
begin
  DestinationList := TList.Create;
  try
    for Index := 0 to lbDstList.Items.Count - 1 do
    begin
      DestinationList.Add(lbDstList.Items.Objects[Index]);
    end;

    for Index := FSelectedObservation.ObservationFactors.Count - 1 downto 0 do
    begin
      ScreenObject := FSelectedObservation.
        ObservationFactors.Items[Index].ScreenObject as TScreenObject;
      if DestinationList.IndexOf(ScreenObject) < 0 then
      begin
        FSelectedObservation.ObservationFactors.Delete(Index);
      end;
    end;

    for Index := 0 to DestinationList.Count - 1 do
    begin
      ScreenObject := DestinationList[Index];
      FSelectedObservation.AddObject(ScreenObject);
    end;
  finally
    DestinationList.Free;
  end;
  DisplayFactor;
end;

{ TUndoEditSutraFluxObservations }

procedure TUndoEditSutraFluxObservations.AssignNewObservations(
  NewSutraFluxObs: TSutraFluxObs);
begin
  FNewSutraFluxObs := TSutraFluxObs.Create(nil);
  FNewSutraFluxObs.Assign(NewSutraFluxObs);
end;

constructor TUndoEditSutraFluxObservations.Create;
begin
  FOldSutraFluxObs := TSutraFluxObs.Create(nil);
  FOldSutraFluxObs.Assign(frmGoPhast.PhastModel.SutraFluxObs);
end;

function TUndoEditSutraFluxObservations.Description: string;
begin
  Result := StrEditSUTRAFluxObse;
end;

destructor TUndoEditSutraFluxObservations.Destroy;
begin
  FOldSutraFluxObs.Free;
  FNewSutraFluxObs.Free;
  inherited;
end;

procedure TUndoEditSutraFluxObservations.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SutraFluxObs := FNewSutraFluxObs;
end;

procedure TUndoEditSutraFluxObservations.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SutraFluxObs := FOldSutraFluxObs;

end;

end.
