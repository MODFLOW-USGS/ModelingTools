unit frmContaminantTreatmentSystemsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  JvExStdCtrls, JvListBox, frameGridUnit, Mt3dCtsSystemUnit, UndoItems,
  RbwController, RbwDataGrid4, GoPhastTypes, RbwParser, System.UITypes;

type
  TCstWellColumns = (cwcStartTime, cwcEndTime, cwcExtractionWells, cwcInjectionWells);
  TCstExternalFlowColumns = (cefcStartTime, cefcEndTime, cstOutflow, cstInflow, cstInflowConc);
  TInjectionOptionColumns = (iocStartTime, iocEndTime, iocTreament1, iocValue1);

  TUndoEditCTS = class(TCustomUndo)
  private
    FOldCTS: TCtsSystemCollection;
    FNewCts: TCtsSystemCollection;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
    constructor Create(var NewCts: TCtsSystemCollection);
    destructor Destroy; override;
  end;

  TfrmContaminantTreatmentSystems = class(TfrmCustomGoPhast)
    tvTreatmentSystems: TTreeView;
    splttr1: TJvNetscapeSplitter;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnCancelBtn: TBitBtn;
    btnOkBtn: TBitBtn;
    btnDeleteSystem: TButton;
    btnAddSystem: TButton;
    pnlMain: TPanel;
    pcMain: TPageControl;
    tabWells: TTabSheet;
    tabExternalFlows: TTabSheet;
    frameExternalFlows: TframeGrid;
    tabTreatments: TTabSheet;
    pnlTreatmentOptions: TPanel;
    lblTreatmentOption: TLabel;
    comboTreatmentOption: TComboBox;
    pcTreatments: TPageControl;
    tabDefaultOptions: TTabSheet;
    frameDefaultOptions: TframeGrid;
    tabIndividualWellOptions: TTabSheet;
    splttr2: TJvNetscapeSplitter;
    tvIndividualObjectOptions: TTreeView;
    pnlInidividualWellOptions: TPanel;
    pnl2: TPanel;
    cbUseDefaultOptions: TCheckBox;
    frameIndividualWellOptions: TframeGrid;
    pnlTop: TPanel;
    edSystemName: TLabeledEdit;
    frameWells: TframeGrid;
    rcSystem: TRbwController;
    rparserThreeDFormulaElements: TRbwParser;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure tvTreatmentSystemsChange(Sender: TObject; Node: TTreeNode);
    procedure comboTreatmentOptionChange(Sender: TObject);
    procedure tvIndividualObjectOptionsChange(Sender: TObject; Node: TTreeNode);
    procedure btnAddSystemClick(Sender: TObject);
    procedure btnDeleteSystemClick(Sender: TObject);
    procedure btnOkBtnClick(Sender: TObject);
    procedure frameWellsGridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure edSystemNameChange(Sender: TObject);
    procedure UpdateNextTimeCell(AGrid: TRbwDataGrid4; ACol, ARow: Integer);
    procedure frameWellsGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameExternalFlowsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameDefaultOptionsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameIndividualWellOptionsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameExternalFlowsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure pcTreatmentsChange(Sender: TObject);
    procedure cbUseDefaultOptionsClick(Sender: TObject);
  private
    FCtsSystems: TCtsSystemCollection;
    FWellObjects: TStringList;
    FSelectedSystem: TCtsSystem;
    NCOMP: Integer;
    FWellItem: TIndividualWellInjectionItem;
    FGettingData: Boolean;
    procedure GetData;
    procedure SetData;
    procedure InitializeControls;
    procedure UpdateInjectionWells;
    procedure CreateBoundaryFormula(const DataGrid: TRbwDataGrid4;
      const ACol, ARow: integer; Formula: string;
      const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmContaminantTreatmentSystems: TfrmContaminantTreatmentSystems;

implementation

uses
  frmGoPhastUnit, ModflowPackageSelectionUnit, PhastModelUnit, ScreenObjectUnit,
  ModflowTimeUnit, frmEditSelectedWellsUnit,
  Mt3dmsChemUnit, frmFormulaUnit, frmConvertChoiceUnit, GIS_Functions;

resourcestring
  StrPercentage = 'Percentage';
  StrConcentrationChange = 'Concentration change';
  StrMass = 'Mass';
  StrConcentration = 'Concentration';
  StrExtractionWellsIW = 'Extraction Wells (IWEXT)';
  StrInjectionWellsIWI = 'Injection Wells (IWINJ)';
  StrExternalOutflowQO = 'External Outflow (QOUTCTS)';
  StrExternalInflowQIN = 'External Inflow (QINCTS)';
  StrExternalInflowConc = 'External Inflow Concentration %s (CINCTS)';
  StrTreatmentOptionS = 'Treatment Option %s (IOPTINJ)';
  StrTreatmentValueS = 'Treatment Value %s (CMCHGINJ)';
  StrChangeMT3DUSGSCon = 'change MT3D-USGS Contaminant Treatment System';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrErrorIn0sRow = 'Error in %0:s Row: %1:d Column: %2:d. %3:s';

{$R *.dfm}

{ TfrmContaminantTreatmentSystems }


procedure TfrmContaminantTreatmentSystems.btnAddSystemClick(
  Sender: TObject);
var
  NewSystem: TCtsSystemItem;
begin
  inherited;
  NewSystem := FCtsSystems.Add;
  NewSystem.CtsSystem.Name := Format('CTS %d', [FCtsSystems.count]);
  tvTreatmentSystems.Selected :=
    tvTreatmentSystems.Items.AddObject(nil, NewSystem.CtsSystem.Name, NewSystem);
end;

procedure TfrmContaminantTreatmentSystems.btnDeleteSystemClick(Sender: TObject);
var
  ASystemItem: TCtsSystemItem;
begin
  inherited;
  ASystemItem := tvTreatmentSystems.Selected.Data;

  tvIndividualObjectOptionsChange(tvIndividualObjectOptions, nil);
  tvTreatmentSystemsChange(tvTreatmentSystems, nil);

  ASystemItem.Free;
  tvTreatmentSystems.Selected.Free;
end;

procedure TfrmContaminantTreatmentSystems.btnOkBtnClick(Sender: TObject);
begin
  inherited;
  tvIndividualObjectOptionsChange(tvIndividualObjectOptions, nil);
  tvTreatmentSystemsChange(tvTreatmentSystems, nil);
  SetData;
end;

procedure TfrmContaminantTreatmentSystems.cbUseDefaultOptionsClick(
  Sender: TObject);
begin
  inherited;
  frameIndividualWellOptions.Enabled := not cbUseDefaultOptions.Checked;
end;

procedure TfrmContaminantTreatmentSystems.comboTreatmentOptionChange(
  Sender: TObject);
begin
  inherited;
  tabIndividualWellOptions.TabVisible :=
    TTreatmentDistribution(comboTreatmentOption.ItemIndex) = tlIndividual;
end;

procedure TfrmContaminantTreatmentSystems.CreateBoundaryFormula(
  const DataGrid: TRbwDataGrid4; const ACol, ARow: integer; Formula: string;
  const Orientation: TDataSetOrientation; const EvaluatedAt: TEvaluatedAt);
var
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
begin
  if Formula = '' then
  begin
    Formula := '0';
  end;
  // CreateBoundaryFormula creates an Expression for a boundary condition
  // based on the text in DataGrid at ACol, ARow. Orientation, and EvaluatedAt
  // are used to chose the TRbwParser.
  TempCompiler := rparserThreeDFormulaElements;
  try
    TempCompiler.Compile(Formula);

  except on E: ERbwParserError do
    begin
      Beep;
      raise ERbwParserError.Create(Format(StrErrorInFormulaS,
        [E.Message]));
      Exit;
    end
  end;
  CompiledFormula := TempCompiler.CurrentExpression;

  ResultType := rdtDouble;

  if (ResultType = CompiledFormula.ResultType) or
    ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
      then
  begin
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end
  else
  begin
    Formula := AdjustFormula(Formula, CompiledFormula.ResultType, ResultType);
    TempCompiler.Compile(Formula);
    CompiledFormula := TempCompiler.CurrentExpression;
    DataGrid.Cells[ACol, ARow] := CompiledFormula.DecompileDisplay;
  end;
  if Assigned(DataGrid.OnSetEditText) then
  begin
    DataGrid.OnSetEditText(DataGrid, ACol, ARow, DataGrid.Cells[ACol, ARow]);
  end;
end;

procedure TfrmContaminantTreatmentSystems.edSystemNameChange(Sender: TObject);
begin
  inherited;
  if tvTreatmentSystems.Selected <> nil then
  begin
    tvTreatmentSystems.Selected.Text := edSystemName.Text;
  end;
end;

procedure TfrmContaminantTreatmentSystems.FormCreate(Sender: TObject);
begin
  inherited;
  FCtsSystems := TCtsSystemCollection.Create(nil);
  FWellObjects := TStringList.Create;
  GetData;
end;

procedure TfrmContaminantTreatmentSystems.FormDestroy(Sender: TObject);
begin
  inherited;
  FCtsSystems.Free;
  FWellObjects.Free;
end;

procedure TfrmContaminantTreatmentSystems.frameDefaultOptionsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(Sender as TRBWDataGrid4, ACol, ARow);
end;

procedure TfrmContaminantTreatmentSystems.frameExternalFlowsGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  DataGrid: TRbwDataGrid4;
  Orientation: TDataSetOrientation;
  EvaluatedAt: TEvaluatedAt;
  NewValue: string;
  ProblemGrid: string;
begin
  inherited;
  DataGrid := Sender as TRbwDataGrid4;
  Orientation := dso3D;
  EvaluatedAt := eaBlocks;

  NewValue := DataGrid.Cells[ACol, ARow];
  if (NewValue = '') then
  begin
    NewValue := '0';
  end;

//  with TfrmFormula.Create(self) do
  with frmFormula do
  begin
    try
      Initialize;
      // GIS functions are not included and
      // Data sets are not included
      // because the variables will be evaluated for screen objects and
      // not at specific locations.

      PopupParent := self;

      // Show the functions and global variables.
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        try
          CreateBoundaryFormula(DataGrid, ACol, ARow, Formula, Orientation,
            EvaluatedAt);
        except on E: Exception do
          begin
            Beep;
            if DataGrid = frameExternalFlows.Grid then
            begin
              ProblemGrid := tabExternalFlows.Caption;
            end
            else if DataGrid = frameDefaultOptions.Grid then
            begin
              ProblemGrid := 'Default treatment options';
            end
            else if DataGrid = frameIndividualWellOptions.Grid then
            begin
              ProblemGrid := 'Individual well treatment options';
            end
            else
            begin
              Assert(False);
            end;
            MessageDlg(Format(StrErrorIn0sRow,
              [ProblemGrid,
              ARow + 1, ACol+1, E.Message]), mtError,[mbOK], 0);
            Exit;
          end;
        end;
      end;
    finally
      Initialize;
//      Free;
    end;
  end;

end;

procedure TfrmContaminantTreatmentSystems.frameExternalFlowsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(Sender as TRBWDataGrid4, ACol, ARow);
end;

procedure TfrmContaminantTreatmentSystems.frameIndividualWellOptionsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(Sender as TRBWDataGrid4, ACol, ARow);
end;

procedure TfrmContaminantTreatmentSystems.frameWellsGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  AvailableWells: TStringList;
  SelectedWells: TStringList;
begin
  inherited;
  AvailableWells := TStringList.Create;
  SelectedWells := TStringList.Create;
  frmEditSelectedWells := TfrmEditSelectedWells.Create(nil);
  try
    AvailableWells.Assign(FWellObjects);
    SelectedWells.DelimitedText := frameWells.Grid.Cells[ACol, ARow];
    frmEditSelectedWells.GetData(AvailableWells, SelectedWells);
    if frmEditSelectedWells.ShowModal = mrOk then
    begin
      frmEditSelectedWells.SetData(SelectedWells);
      frameWells.Grid.Cells[ACol, ARow] := StringReplace(
        SelectedWells.DelimitedText, ',', ', ', [rfReplaceAll, rfIgnoreCase]);
      if ACol = Ord(cwcInjectionWells) then
      begin
        UpdateInjectionWells;
      end;
    end;
  finally
    AvailableWells.Free;
    SelectedWells.Free;
    FreeAndNil(frmEditSelectedWells);
  end;
end;

procedure TfrmContaminantTreatmentSystems.frameWellsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(Sender as TRBWDataGrid4, ACol, ARow);
  if (not FGettingData) and (ACol = Ord(cwcInjectionWells)) then
  begin
    UpdateInjectionWells;
  end;
end;

procedure TfrmContaminantTreatmentSystems.InitializeControls;
var
  LocalModel: TPhastModel;
  StressPeriods: TModflowStressPeriods;
  CompIndex: Integer;
  ColIndex: Integer;
  Grid: TRbwDataGrid4;
  TreatmentOptions: TStringList;
  AColumn: TRbwColumn4;
  procedure InitializeOptionsGrid;
  var
    CompIndex: Integer;
  begin
    Grid.Cells[Ord(cefcStartTime), 0] := StrStartingTime;
    Grid.Cells[Ord(cefcEndTime), 0] := StrEndingTime;
    ColIndex := Ord(iocTreament1);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      Grid.Cells[ColIndex, 0] := Format(StrTreatmentOptionS, [LocalModel.Mt3dSpecesName[CompIndex]]);
      AColumn := Grid.Columns[ColIndex];
      AColumn.ComboUsed := True;
      AColumn.PickList := TreatmentOptions;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
      Inc(ColIndex);

      Grid.Cells[ColIndex, 0] := Format(StrTreatmentValueS, [LocalModel.Mt3dSpecesName[CompIndex]]);
      AColumn := Grid.Columns[ColIndex];
      AColumn.ButtonUsed := True;
      AColumn.ButtonCaption := 'F()';
      AColumn.ButtonWidth := 35;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
      Inc(ColIndex);
    end;
  end;
begin
  LocalModel := frmGoPhast.PhastModel;

  pcMain.ActivePageIndex := 0;
  pcTreatments.ActivePageIndex := 0;

  NCOMP := LocalModel.NumberOfMt3dChemComponents;
  frameExternalFlows.Grid.ColCount := NCOMP + 4;
  frameDefaultOptions.Grid.ColCount := NCOMP*2+ 2;
  frameIndividualWellOptions.Grid.ColCount := NCOMP*2+ 2;

  TreatmentOptions := TStringList.Create;
  try
    TreatmentOptions.Add(StrPercentage);
    TreatmentOptions.Add(StrConcentrationChange);
    TreatmentOptions.Add(StrMass);
    TreatmentOptions.Add(StrConcentration);

    frameExternalFlows.Grid.BeginUpdate;
    frameDefaultOptions.Grid.BeginUpdate;
    frameWells.Grid.BeginUpdate;
    frameIndividualWellOptions.Grid.BeginUpdate;
    try
      StressPeriods := LocalModel.ModflowStressPeriods;
      StressPeriods.FillPickListWithStartTimes(frameWells.Grid, 0);
      StressPeriods.FillPickListWithEndTimes(frameWells.Grid, 1);
      StressPeriods.FillPickListWithStartTimes(frameExternalFlows.Grid, 0);
      StressPeriods.FillPickListWithEndTimes(frameExternalFlows.Grid, 1);
      StressPeriods.FillPickListWithStartTimes(frameDefaultOptions.Grid, 0);
      StressPeriods.FillPickListWithEndTimes(frameDefaultOptions.Grid, 1);
      StressPeriods.FillPickListWithStartTimes(frameIndividualWellOptions.Grid, 0);
      StressPeriods.FillPickListWithEndTimes(frameIndividualWellOptions.Grid, 1);

      Grid := frameWells.Grid;
      Grid.Cells[Ord(cwcStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(cwcEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(cwcExtractionWells), 0] := StrExtractionWellsIW;
      Grid.Cells[Ord(cstInflow), 0] := StrInjectionWellsIWI;

      Grid := frameExternalFlows.Grid;
      Grid.Cells[Ord(cefcStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(cefcEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(cstOutflow), 0] := StrExternalOutflowQO;
      Grid.Cells[Ord(cstInflow), 0] := StrExternalInflowQIN;
      for CompIndex := 0 to NCOMP - 1 do
      begin
        ColIndex := Ord(cstInflowConc) + CompIndex;
        Grid.Cells[ColIndex, 0] := Format(StrExternalInflowConc,
          [LocalModel.Mt3dSpecesName[CompIndex]]);
        AColumn := Grid.Columns[ColIndex];
        AColumn.ButtonUsed := True;
        AColumn.ButtonCaption := 'F()';
        AColumn.ButtonWidth := 35;
        AColumn.AutoAdjustColWidths := True;
        AColumn.AutoAdjustRowHeights := True;
        AColumn.WordWrapCaptions := True;
      end;

      Grid := frameDefaultOptions.Grid;
      InitializeOptionsGrid;

      Grid := frameIndividualWellOptions.Grid;
      InitializeOptionsGrid;

    finally
      frameExternalFlows.Grid.EndUpdate;
      frameDefaultOptions.Grid.EndUpdate;
      frameWells.Grid.EndUpdate;
      frameIndividualWellOptions.Grid.EndUpdate;
    end;
  finally
    TreatmentOptions.Free;
  end;

end;

procedure TfrmContaminantTreatmentSystems.pcTreatmentsChange(Sender: TObject);
begin
  inherited;
  if (pcTreatments.ActivePage = tabIndividualWellOptions)
    and (tvIndividualObjectOptions.Selected = nil)
    and (tvIndividualObjectOptions.Items.Count > 0) then
  begin
    tvIndividualObjectOptions.Selected := tvIndividualObjectOptions.Items.GetFirstNode;
  end;
end;

procedure TfrmContaminantTreatmentSystems.UpdateInjectionWells;
var
  InjectionWells: TStringList;
  Splitter: TStringList;
  RowIndex: Integer;
  SelectedWellName: string;
  WellIndex: Integer;
  WellItem: TIndividualWellInjectionItem;
  SelectedNode: TTreeNode;
  ANode: TTreeNode;
begin
  InjectionWells := TStringList.Create;
  Splitter := TStringList.Create;
  try
    InjectionWells.Sorted := True;
    InjectionWells.Duplicates := dupIgnore;
    for RowIndex := 1 to frameWells.Grid.RowCount - 1 do
    begin
      Splitter.DelimitedText := frameWells.Grid.Cells[Ord(cwcInjectionWells), RowIndex];
      InjectionWells.AddStrings(Splitter);
    end;
    if tvIndividualObjectOptions.Selected <> nil then
    begin
      SelectedWellName := tvIndividualObjectOptions.Selected.Text;
    end
    else
    begin
      SelectedWellName := '';
    end;
    tvIndividualObjectOptions.Selected := nil;
    tvIndividualObjectOptionsChange(tvIndividualObjectOptions, nil);
    for WellIndex := 0 to InjectionWells.Count - 1 do
    begin
      if FSelectedSystem.Injections.GetItemByObjectName(InjectionWells[WellIndex]) = nil then
      begin
        WellItem := FSelectedSystem.Injections.Add;
        WellItem.InjectionWellObjectName := InjectionWells[WellIndex];
        WellItem.UseDefaultInjectionOptions := True;
      end;
    end;
    for WellIndex := FSelectedSystem.Injections.Count - 1 downto 0 do
    begin
      if InjectionWells.IndexOf(FSelectedSystem.Injections[WellIndex].InjectionWellObjectName) < 0 then
      begin
        FSelectedSystem.Injections.Delete(WellIndex);
      end;
    end;
    tvIndividualObjectOptions.Items.Clear;
    SelectedNode := nil;
    for WellIndex := 0 to FSelectedSystem.Injections.Count - 1 do
    begin
      WellItem := FSelectedSystem.Injections[WellIndex];
      ANode := tvIndividualObjectOptions.Items.AddObject(nil, WellItem.InjectionWellObjectName, WellItem);
      if WellItem.InjectionWellObjectName = SelectedWellName then
      begin
        SelectedNode := ANode;
      end;
    end;
    if SelectedNode <> nil then
    begin
      tvIndividualObjectOptions.Selected := SelectedNode;
      tvIndividualObjectOptionsChange(tvIndividualObjectOptions, SelectedNode);
    end;
  finally
    InjectionWells.Free;
    Splitter.Free;
  end;
end;

procedure TfrmContaminantTreatmentSystems.GetData;
var
  WellPackageChoice: TCtsWellPackageChoice;
  LocalModel: TPhastModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SystemIndex: Integer;
  ASystem: TCtsSystem;
//  StressPeriods: TModflowStressPeriods;
  SystemItem: TCtsSystemItem;
  CompilerList: TList;
begin
  FGettingData := True;
  try

    AddGIS_Functions(rparserThreeDFormulaElements, frmGoPhast.ModelSelection,
      eaBlocks);
    CompilerList := TList.Create;
    try
      CompilerList.Add(rparserThreeDFormulaElements);
      frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
    finally
      CompilerList.Free;
    end;

    LocalModel := frmGoPhast.PhastModel;

    InitializeControls;

    WellPackageChoice := LocalModel.ModflowPackages.Mt3dCts.WellPackageChoice;

    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      case WellPackageChoice of
        cwpcMnw2:
          begin
            if (AScreenObject.ModflowMnw2Boundary <> nil)
              and AScreenObject.ModflowMnw2Boundary.Used then
            begin
              FWellObjects.AddObject(AScreenObject.Name, AScreenObject);
            end;
          end;
        cwpcWel:
          begin
            if (AScreenObject.ModflowWellBoundary <> nil)
              and AScreenObject.ModflowWellBoundary.Used then
            begin
              FWellObjects.AddObject(AScreenObject.Name, AScreenObject);
            end;
          end;
        else
          Assert(False);
      end;
    end;
    FWellObjects.Sorted := True;

    FCtsSystems.Assign(LocalModel.CtsSystems);
    for SystemIndex := 0 to FCtsSystems.Count - 1 do
    begin
      SystemItem := FCtsSystems[SystemIndex];
      ASystem := SystemItem.CtsSystem;
      tvTreatmentSystems.Items.AddObject(nil, ASystem.Name, SystemItem);
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmContaminantTreatmentSystems.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoEditCTS.Create(FCtsSystems));
end;

procedure TfrmContaminantTreatmentSystems.tvIndividualObjectOptionsChange(
  Sender: TObject; Node: TTreeNode);
var
  InjIndex: Integer;
  InjItem: TCtsInjectionTimeItem;
  Treatment: TInjectionOptionItem;
  ColIndex: Integer;
  CompIndex: Integer;
  Grid: TRbwDataGrid4;
  TreatmentOptionIndex: Integer;
  Value: string;
begin
  inherited;
  if FWellItem <> nil then
  begin
    FWellItem.UseDefaultInjectionOptions := cbUseDefaultOptions.Checked;
    FWellItem.Injections.Count := frameIndividualWellOptions.seNumber.AsInteger;

    for InjIndex := 0 to FWellItem.Injections.Count - 1 do
    begin
      InjItem := FWellItem.Injections[InjIndex];
      InjItem.StartTime :=
        frameIndividualWellOptions.Grid.RealValueDefault[Ord(iocStartTime), InjIndex+1, -1];
      InjItem.EndTime :=
        frameIndividualWellOptions.Grid.RealValueDefault[Ord(iocEndTime), InjIndex+1, 0];
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        TreatmentOptionIndex := frameIndividualWellOptions.Grid.ItemIndex
          [ColIndex, InjIndex+1];
        if TreatmentOptionIndex >= 0 then
        begin
          Treatment.TreatmentOption := TTreatmentOption(TreatmentOptionIndex);
        end;
        Inc(ColIndex);
        Value := frameIndividualWellOptions.Grid.Cells[ColIndex, InjIndex+1];
        if Value <> '' then
        begin
          Treatment.Value := Value;
        end;
        Inc(ColIndex);
      end;
    end;
  end;
  if Node <> nil then
  begin
    FWellItem := Node.Data;
  end
  else
  begin
    FWellItem := nil;
  end;
  pnlInidividualWellOptions.Enabled := FWellItem <> nil;
  if FWellItem <> nil then
  begin
    cbUseDefaultOptions.Checked := FWellItem.UseDefaultInjectionOptions;
    Grid := frameIndividualWellOptions.Grid;
    ClearGrid(Grid);
    frameIndividualWellOptions.seNumber.AsInteger := FWellItem.Injections.Count;

    for InjIndex := 0 to FWellItem.Injections.Count - 1 do
    begin
      InjItem := FWellItem.Injections[InjIndex];
      Grid.RealValue[Ord(iocStartTime), InjIndex+1] := InjItem.StartTime;
      Grid.RealValue[Ord(iocEndTime), InjIndex+1] := InjItem.EndTime;
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        Grid.ItemIndex[ColIndex, InjIndex+1] := Ord(Treatment.TreatmentOption);
        Inc(ColIndex);
        Grid.Cells[ColIndex, InjIndex+1] := Treatment.Value;
        Inc(ColIndex);
      end;
    end;
  end;
end;

procedure TfrmContaminantTreatmentSystems.tvTreatmentSystemsChange(
  Sender: TObject; Node: TTreeNode);
var
  ExtractionIndex: Integer;
  CtsObject: TCtsObjectItem;
  ExternalFlowIndex: Integer;
  ExternalFlowsItem: TCtsExternalFlowsItem;
  CompIndex: Integer;
  DefaultInjIndex: Integer;
  InjItem: TCtsInjectionTimeItem;
  Treatment: TInjectionOptionItem;
  ColIndex: Integer;
  WellIndex: Integer;
  WellItem: TIndividualWellInjectionItem;
  SystemItem: TCtsSystemItem;
  SpeciesIndex: Integer;
  SpeciesItem: TStringConcValueItem;
  LocalModel: TPhastModel;
  Grid: TRbwDataGrid4;
  Value: string;
  TreatmentOptionIndex: Integer;
begin
  inherited;
  LocalModel := frmGoPhast.PhastModel;
  if FSelectedSystem <> nil then
  begin
    tvIndividualObjectOptionsChange(nil, nil);

    FSelectedSystem.Name := edSystemName.Text;

    FSelectedSystem.CtsObjects.Count := frameWells.seNumber.AsInteger;
    for ExtractionIndex := 0 to FSelectedSystem.CtsObjects.Count - 1 do
    begin
      CtsObject := FSelectedSystem.CtsObjects[ExtractionIndex];
      CtsObject.StartTime :=
        frameWells.Grid.RealValueDefault[Ord(cwcStartTime), ExtractionIndex+1, -1];
      CtsObject.EndTime :=
        frameWells.Grid.RealValueDefault[Ord(cwcEndTime), ExtractionIndex+1, 0];
      CtsObject.ExtractionWellObjects.DelimitedText :=
        frameWells.Grid.Cells[Ord(cwcExtractionWells), ExtractionIndex+1];
      CtsObject.InjectionWellObjects.DelimitedText :=
        frameWells.Grid.Cells[Ord(cwcInjectionWells), ExtractionIndex+1];
    end;

    FSelectedSystem.ExternalFlows.Count := frameExternalFlows.seNumber.AsInteger;
    for ExternalFlowIndex := 0 to FSelectedSystem.ExternalFlows.Count - 1 do
    begin
      ExternalFlowsItem := FSelectedSystem.ExternalFlows[ExternalFlowIndex];
      ExternalFlowsItem.StartTime :=
        frameExternalFlows.Grid.RealValueDefault[Ord(cefcStartTime), ExternalFlowIndex+1, -1];
      ExternalFlowsItem.EndTime :=
        frameExternalFlows.Grid.RealValueDefault[Ord(cefcEndTime), ExternalFlowIndex+1, 0];
      Value := frameExternalFlows.Grid.Cells[Ord(cstOutflow), ExternalFlowIndex+1];
      if Value <> '' then
      begin
        ExternalFlowsItem.Outflow := Value;
      end;

      Value := frameExternalFlows.Grid.Cells[Ord(cstInflow), ExternalFlowIndex+1];
      if Value <> '' then
      begin
        ExternalFlowsItem.Inflow := Value;
      end;
      While ExternalFlowsItem.InflowConcentrations.Count < NCOMP do
      begin
        SpeciesIndex := ExternalFlowsItem.InflowConcentrations.Count;
        SpeciesItem := ExternalFlowsItem.InflowConcentrations.Add;
        SpeciesItem.Value := '0';
        SpeciesItem.Name := LocalModel.Mt3dSpecesName[SpeciesIndex];
      end;
      for CompIndex := 0 to NCOMP - 1 do
      begin
        SpeciesItem := ExternalFlowsItem.InflowConcentrations[CompIndex];
        Value := frameExternalFlows.Grid.Cells[Ord(cstInflowConc) + CompIndex, ExternalFlowIndex+1];
        if Value <> '' then
        begin
          SpeciesItem.Value := Value;
        end;
        SpeciesItem.Name := LocalModel.Mt3dSpecesName[CompIndex];
      end;
    end;

    FSelectedSystem.TreatmentDistribution :=
      TTreatmentDistribution(comboTreatmentOption.ItemIndex);

    FSelectedSystem.DefaultInjectionOptions.Count := frameDefaultOptions.seNumber.AsInteger;

    for DefaultInjIndex := 0 to FSelectedSystem.DefaultInjectionOptions.Count - 1 do
    begin
      InjItem := FSelectedSystem.DefaultInjectionOptions[DefaultInjIndex];
      InjItem.StartTime :=
        frameDefaultOptions.Grid.RealValueDefault[Ord(iocStartTime), DefaultInjIndex+1, -1];
      InjItem.EndTime :=
        frameDefaultOptions.Grid.RealValueDefault[Ord(iocEndTime), DefaultInjIndex+1, 0];
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];

        TreatmentOptionIndex := frameDefaultOptions.Grid.ItemIndex[
          ColIndex, DefaultInjIndex+1];
        if TreatmentOptionIndex >= 0 then
        begin
          Treatment.TreatmentOption := TTreatmentOption(TreatmentOptionIndex);
        end;
        Inc(ColIndex);

        Value := frameDefaultOptions.Grid.Cells[
          ColIndex, DefaultInjIndex+1];
        if Value <> '' then
        begin
          Treatment.Value := Value;
        end;
        Inc(ColIndex);
      end;
    end;

  end;
  if Node <> nil then
  begin
    SystemItem := Node.Data;
    if SystemItem <> nil then
    begin
      FSelectedSystem := SystemItem.CtsSystem;
    end
    else
    begin
      FSelectedSystem := nil;
    end;
  end
  else
  begin
    FSelectedSystem := nil;
  end;
  rcSystem.Enabled := (FSelectedSystem <> nil);
  if FSelectedSystem <> nil then
  begin
    edSystemName.Text := FSelectedSystem.Name;

    Grid := frameWells.Grid;
    ClearGrid(Grid);
    frameWells.seNumber.AsInteger := FSelectedSystem.CtsObjects.Count;
    for ExtractionIndex := 0 to FSelectedSystem.CtsObjects.Count - 1 do
    begin
      CtsObject := FSelectedSystem.CtsObjects[ExtractionIndex];
      Grid.RealValue[Ord(cwcStartTime), ExtractionIndex+1] :=
        CtsObject.StartTime;
      Grid.RealValue[Ord(cwcEndTime), ExtractionIndex+1] :=
        CtsObject.EndTime;
      Grid.Cells[Ord(cwcExtractionWells), ExtractionIndex+1] :=
        CtsObject.ExtractionWellObjects.DelimitedText;
      Grid.Cells[Ord(cwcInjectionWells), ExtractionIndex+1] :=
        CtsObject.InjectionWellObjects.DelimitedText;
    end;

    Grid := frameExternalFlows.Grid;
    ClearGrid(Grid);
    frameExternalFlows.seNumber.AsInteger := FSelectedSystem.ExternalFlows.Count;
    for ExternalFlowIndex := 0 to FSelectedSystem.ExternalFlows.Count - 1 do
    begin
      ExternalFlowsItem := FSelectedSystem.ExternalFlows[ExternalFlowIndex];
      Grid.RealValue[Ord(cefcStartTime), ExternalFlowIndex+1] :=
        ExternalFlowsItem.StartTime;
      Grid.RealValue[Ord(cefcEndTime), ExternalFlowIndex+1] :=
        ExternalFlowsItem.EndTime;
      Grid.Cells[Ord(cstOutflow), ExternalFlowIndex+1] :=
        ExternalFlowsItem.Outflow;
      Grid.Cells[Ord(cstInflow), ExternalFlowIndex+1] :=
        ExternalFlowsItem.Inflow;
      While ExternalFlowsItem.InflowConcentrations.Count < NCOMP do
      begin
        SpeciesIndex := ExternalFlowsItem.InflowConcentrations.Count;
        SpeciesItem := ExternalFlowsItem.InflowConcentrations.Add;
        SpeciesItem.Value := '0';
        SpeciesItem.Name := LocalModel.Mt3dSpecesName[SpeciesIndex];
      end;
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Grid.Cells[Ord(cstInflowConc) + CompIndex, ExternalFlowIndex+1] :=
          ExternalFlowsItem.InflowConcentrations[CompIndex].Value;
      end;
    end;

    comboTreatmentOption.ItemIndex := Ord(FSelectedSystem.TreatmentDistribution);

    Grid := frameDefaultOptions.Grid;
    ClearGrid(Grid);
    frameDefaultOptions.seNumber.AsInteger :=
      FSelectedSystem.DefaultInjectionOptions.Count;
    for DefaultInjIndex := 0 to FSelectedSystem.DefaultInjectionOptions.Count - 1 do
    begin
      InjItem := FSelectedSystem.DefaultInjectionOptions[DefaultInjIndex];
      Grid.RealValue[Ord(iocStartTime), DefaultInjIndex+1] :=
        InjItem.StartTime;
      Grid.RealValue[Ord(iocEndTime), DefaultInjIndex+1] :=
        InjItem.EndTime;
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        Grid.ItemIndex[ColIndex, DefaultInjIndex+1] :=
          Ord(Treatment.TreatmentOption);
        Inc(ColIndex);
        Grid.Cells[ColIndex, DefaultInjIndex+1] :=
          Treatment.Value;
        Inc(ColIndex);
      end;
    end;

    tvIndividualObjectOptions.Items.Clear;
    for WellIndex := 0 to FSelectedSystem.Injections.Count - 1 do
    begin
      WellItem := FSelectedSystem.Injections[WellIndex];
      tvIndividualObjectOptions.Items.AddObject(nil,
        WellItem.InjectionWellObjectName, WellItem);
    end;

  end;
end;

procedure TfrmContaminantTreatmentSystems.UpdateNextTimeCell(
  AGrid: TRbwDataGrid4; ACol, ARow: Integer);
begin
  if not FGettingData then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(AGrid, ACol, ARow);
  end;
end;

{ TUndoEditCTS }

constructor TUndoEditCTS.Create(var NewCts: TCtsSystemCollection);
begin
  FOldCTS := TCtsSystemCollection.Create(nil);
  FOldCTS.Assign(frmGoPhast.PhastModel.CtsSystems);
  FNewCts := NewCts;
  NewCts := nil;
end;

function TUndoEditCTS.Description: string;
begin
  result := StrChangeMT3DUSGSCon;
end;

destructor TUndoEditCTS.Destroy;
begin
  FOldCTS.Free;
  FNewCts.Free;
  inherited;
end;

procedure TUndoEditCTS.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.CtsSystems := FNewCts;
  frmGoPhast.PhastModel.CtsSystems.Loaded;
end;

procedure TUndoEditCTS.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.CtsSystems := FOldCTS;
end;

end.
