unit frmModflowRipPlantGroupsUnit;

interface

uses System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls, frameGridUnit, ArgusDataEntry,
  ModflowRipPlantGroupsUnit, UndoItems, Vcl.Mask, JvExMask, JvToolEdit,
  UndoItemsScreenObjects, System.Generics.Collections, GrayTabs;

type
  TFluxCurveColumns = (fccN, fccFdh, fccFdr);

  TfrmModflowRipPlantGroups = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlRipPlantGroupSelection: TPanel;
    grdpnl1: TGridPanel;
    btnAddPlantGroup: TSpeedButton;
    btnInsertPlantGroup: TSpeedButton;
    btnDeletePlantGroup: TSpeedButton;
    tvPlantGroups: TTreeView;
    pgcMain: TPageControl;
    tsProperties: TTabSheet;
    tabTranspirationRateCurve: TTabSheet;
    frameTranspirationCurve: TframeGrid;
    pbTranspirationCurve: TPaintBox;
    lbledtRipName: TLabeledEdit;
    lblSatExtDepth: TLabel;
    lblActiveRootDepth: TLabel;
    lblMaxEtFlus: TLabel;
    lblMaxEvapFlux: TLabel;
    cedSatExtDepth: TJvComboEdit;
    cedActiveRootDepth: TJvComboEdit;
    cedMaxEtFlux: TJvComboEdit;
    cedSatExtinctEvapFlux: TJvComboEdit;
    splMain: TSplitter;
    procedure FormCreate(Sender: TObject); override;
    procedure pbTranspirationCurvePaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure tvPlantGroupsChange(Sender: TObject; Node: TTreeNode);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddPlantGroupClick(Sender: TObject);
    procedure btnInsertPlantGroupClick(Sender: TObject);
    procedure btnDeletePlantGroupClick(Sender: TObject);
    procedure frameTranspirationCurveGridEndUpdate(Sender: TObject);
    procedure EditFormula(Sender: TObject);
    procedure frameTranspirationCurveGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure ChangeComboEdColor(Sender: TObject);
  private
    FSelectedPlantGroup: TRipPlantGroup;
    FPlantGroups: TRipPlantGroups;
    procedure SetSelectedPlantGroup(const Value: TRipPlantGroup);
    procedure AssignPlantGroupPropertiesFromControls(PlantGroup: TRipPlantGroup);
    procedure AssignControlsFromPlantGroupProperties(PlantGroup: TRipPlantGroup);
    procedure GetData;
    procedure FillTreeView;
    procedure SelectFirstTreeViewNode;
    procedure SetData;
    function AddNewPlantGroup: TRipPlantGroup;
    function AddPlantGroupToTreeView(APlantGroup: TRipPlantGroup): TTreeNode;
    procedure UpdateTreeNodeText(APlantGroup: TRipPlantGroup);
    { Private declarations }
  public
    property SelectedPlantGroup: TRipPlantGroup read FSelectedPlantGroup
      write SetSelectedPlantGroup;
    { Public declarations }
  end;

  TUndoChangePlantGroups = class(TUndoSetScreenObjectProperties)
  private
    FNewPlantGroups: TRipPlantGroups;
    FOldPlantGroups: TRipPlantGroups;
    procedure UpdateTimeLists;
  protected
    function Description: string; override;
  public
    constructor Create(var NewPlantGroups: TRipPlantGroups;
      const AListOfScreenObjects: TList; var NewScreenObjects,
      OldScreenObjects: TScreenObjectEditCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;

var
  frmModflowRipPlantGroups: TfrmModflowRipPlantGroups;

implementation

uses
  RbwDataGrid4, xygraph, frmGoPhastUnit,
  frmFormulaUnit, ScreenObjectUnit, ModflowRipUnit, ModflowPackageSelectionUnit,
  PhastModelUnit, RbwParser, GoPhastTypes;

resourcestring
  StrTranspirationRateC = 'Transpiration rate curves have not been defined f' +
  'or the following plant groups: '#13#10'%s'#13#10'Click "Cancel" to fix th' +
  'is before closing the dialog box. ';
  StrDimensionlessActive = 'Dimensionless active root depth segment (fdh)';
  StrN = 'N';
  StrDimensionlessFluxS = 'Dimensionless flux segment (fdR)';
  StrRate = 'Rate';
  StrElevation = 'Elevation';
  StrChangeRiparianETP = 'change Riparian ET plant groups';
  StrTheTotalDimensionl = 'The total dimensionless depth do not add up to on' +
  'e for the following plant groups: '#13#10'%s'#13#10'Click "Cancel" to fix' +
  ' this before closing the dialog box. ';
  StrTheMaximumDimensio = 'The maximum dimensionless ET is not one for the f' +
  'ollowing plant groups: '#13#10'%s'#13#10'Click "Cancel" to fix this befor' +
  'e closing the dialog box. ';

{$R *.dfm}

procedure TfrmModflowRipPlantGroups.AssignControlsFromPlantGroupProperties(
  PlantGroup: TRipPlantGroup);
var
  Grid: TRbwDataGrid4;
  ETSegments: TRipETSegments;
  Index: Integer;
  RowIndex: Integer;
  EtItem: TRipETSegment;
begin
  lbledtRipName.Text := PlantGroup.Name;
  cedSatExtDepth.Text := PlantGroup.SaturatedExtinctionDepth;
  cedActiveRootDepth.Text := PlantGroup.ActiveRootDepth;
  cedMaxEtFlux.Text := PlantGroup.MaxET;
  cedSatExtinctEvapFlux.Text := PlantGroup.SatExtinctionEvap;

//  Index := 0;
  Grid := frameTranspirationCurve.Grid;
  ETSegments := PlantGroup.ETSegments;

  Grid.BeginUpdate;
  try
    ClearGrid(Grid);
    frameTranspirationCurve.seNumber.AsInteger := ETSegments.Count;
    for Index := 0 to ETSegments.Count - 1 do
    begin
      EtItem := ETSegments[Index];
      RowIndex := Index+1;
      Grid.IntegerValue[Ord(fccN), RowIndex] := RowIndex;
      Grid.RealValue[Ord(fccFdh), RowIndex] := EtItem.ActiveRootDepth;
      Grid.RealValue[Ord(fccFdr), RowIndex] := EtItem.FluxSegment;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmModflowRipPlantGroups.AssignPlantGroupPropertiesFromControls(
  PlantGroup: TRipPlantGroup);
var
  Index: Integer;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  fdh: Extended;
  fdR: Extended;
  ETSegments: TRipETSegments;
  EtItem: TRipETSegment;
begin
  PlantGroup.Name := lbledtRipName.Text;
//  PlantGroup.SaturatedExtinctionDepth := rdeSatExtDepth.RealValue;
  PlantGroup.SaturatedExtinctionDepth := cedSatExtDepth.Text;
  PlantGroup.ActiveRootDepth := cedActiveRootDepth.Text;
  PlantGroup.MaxET := cedMaxEtFlux.Text;
  PlantGroup.SatExtinctionEvap := cedSatExtinctEvapFlux.Text;

  Index := 0;
  Grid := frameTranspirationCurve.Grid;
  ETSegments := PlantGroup.ETSegments;
  for RowIndex := 1 to frameTranspirationCurve.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(fccFdh), RowIndex], fdh)
      and TryStrToFloat(Grid.Cells[Ord(fccFdr), RowIndex], fdR) then
    begin
      if Index >= ETSegments.Count then
      begin
        EtItem := ETSegments.Add;
      end
      else
      begin
        EtItem := ETSegments[Index];
      end;
      EtItem.ActiveRootDepth := fdh;
      EtItem.FluxSegment := fdR;
      Inc(Index);
    end;
  end;
  ETSegments.Count := Index;

end;

procedure TfrmModflowRipPlantGroups.btnAddPlantGroupClick(Sender: TObject);
var
  NewPlantGroup: TRipPlantGroup;
  NewNode: TTreeNode;
begin
  inherited;
  NewPlantGroup := AddNewPlantGroup;
  NewNode := AddPlantGroupToTreeView(NewPlantGroup);
  tvPlantGroups.Selected := NewNode;
end;

procedure TfrmModflowRipPlantGroups.btnDeletePlantGroupClick(Sender: TObject);
var
  SelectedNode: TTreeNode;
  APlantGroup: TRipPlantGroup;
begin
  inherited;
  SelectedPlantGroup := nil;
  SelectedNode := tvPlantGroups.Selected;
  APlantGroup := SelectedNode.Data;
  FPlantGroups.Remove(APlantGroup);
  tvPlantGroups.Items.Delete(SelectedNode);
end;

procedure TfrmModflowRipPlantGroups.btnInsertPlantGroupClick(Sender: TObject);
var
  SelectedNode: TTreeNode;
  APlantGroup: TRipPlantGroup;
  NewPlantGroup: TRipPlantGroup;
begin
  inherited;
  SelectedPlantGroup := nil;
  SelectedNode := tvPlantGroups.Selected;
  if SelectedNode = nil then
  begin
    btnAddPlantGroupClick(nil)
  end
  else
  begin
    APlantGroup := SelectedNode.Data;
    NewPlantGroup := AddNewPlantGroup;
    NewPlantGroup.Index := APlantGroup.Index;

    tvPlantGroups.Items.InsertObject(SelectedNode, NewPlantGroup.Name,
      NewPlantGroup);
  end;
end;

procedure TfrmModflowRipPlantGroups.btnOKClick(Sender: TObject);
var
  PlantGroupIndex: Integer;
  ErrorGroups: TStringList;
  APlantGroup: TRipPlantGroup;
begin
  inherited;
  SelectedPlantGroup := nil;
  ErrorGroups := TStringList.Create;
  try
    for PlantGroupIndex := 0 to FPlantGroups.Count - 1 do
    begin
      APlantGroup := FPlantGroups[PlantGroupIndex];
      if APlantGroup.ETSegments.Count = 0 then
      begin
        ErrorGroups.Add(APlantGroup.Name);
      end;
    end;
    if ErrorGroups.Count > 0 then
    begin
      Beep;
      if MessageDlg(Format(StrTranspirationRateC, [ErrorGroups.text]),
        mtWarning, [mbOK, mbCancel], 0, mbCancel) = mrCancel then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
    ErrorGroups.Assign(FPlantGroups.PlantGroupWithBadDepthSegments);
    if ErrorGroups.Count > 0 then
    begin
      Beep;
      if MessageDlg(Format(StrTheTotalDimensionl, [ErrorGroups.text]),
        mtWarning, [mbOK, mbCancel], 0, mbCancel) = mrCancel then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
    ErrorGroups.Assign(FPlantGroups.PlantGroupWithBadRateSegments);
    if ErrorGroups.Count > 0 then
    begin
      Beep;
      if MessageDlg(Format(StrTheMaximumDimensio, [ErrorGroups.text]),
        mtWarning, [mbOK, mbCancel], 0, mbCancel) = mrCancel then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
  finally
    ErrorGroups.Free;
  end;
  SetData;
end;

procedure TfrmModflowRipPlantGroups.ChangeComboEdColor(Sender: TObject);
var
  Edit: TJvComboEdit;
  NewValue: string;
  Compiler: TRbwParser;
  EdColor: TColor;
begin
  inherited;
  Edit := Sender as TJvComboEdit;

  NewValue := Edit.Text;
  Compiler := frmGoPhast.PhastModel.GetCompiler(dso3D, eaBlocks);
  EdColor := clWindow;
  try
    try
      Compiler.Compile(NewValue)
    except on ERbwParserError do
      begin
        EdColor := clRed;
      end;
    end;
  finally
    Edit.Color := EdColor;
  end;
end;

procedure TfrmModflowRipPlantGroups.FillTreeView;
var
  TreeNodes: TTreeNodes;
  index: Integer;
  APlantGroup: TRipPlantGroup;
begin
  TreeNodes := tvPlantGroups.Items;
  TreeNodes.Clear;
  for index := 0 to FPlantGroups.Count - 1 do
  begin
    APlantGroup := FPlantGroups[index];
    AddPlantGroupToTreeView(APlantGroup);
  end;
end;

procedure TfrmModflowRipPlantGroups.SelectFirstTreeViewNode;
begin
  if tvPlantGroups.Items.Count > 0 then
  begin
    tvPlantGroups.Selected := tvPlantGroups.Items[0];
    tvPlantGroupsChange(tvPlantGroups, tvPlantGroups.Selected);
  end
  else
  begin
    SelectedPlantGroup := nil;
  end;
end;

procedure TfrmModflowRipPlantGroups.FormCreate(Sender: TObject);
var
  Grid: TRbwDataGrid4;
begin
  inherited;
  pgcMain.ActivePageIndex := 0;
  Grid := frameTranspirationCurve.Grid;
  Grid.BeginUpdate;
  try
    Grid.Cells[Ord(fccN), 0] := StrN;
    Grid.Cells[Ord(fccFdh), 0] := StrDimensionlessActive;
    Grid.Cells[Ord(fccFdr), 0] := StrDimensionlessFluxS;
  finally
    Grid.EndUpdate;
  end;
  FPlantGroups := TRipPlantGroups.Create(nil);
  GetData;
end;

procedure TfrmModflowRipPlantGroups.FormDestroy(Sender: TObject);
begin
  inherited;
  FPlantGroups.Free;
end;

procedure TfrmModflowRipPlantGroups.frameTranspirationCurveGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  Total: double;
  RowIndex: Integer;
  AValue: double;
  OKTotal: Boolean;
begin
  inherited;
  if (ARow >= frameTranspirationCurve.Grid.FixedRows)
    and (ACol in [Ord(fccFdh), Ord(fccFdr)]) then
  begin
    Total := 0;
    for RowIndex := 1 to ARow do
    begin
      AValue := frameTranspirationCurve.Grid.RealValueDefault[ACol, RowIndex, 0];
      Total := Total + AValue;
    end;
    OKTotal := (0 <= Total) and (Total <= 1.001);
    if (ACol = Ord(fccFdh)) and (ARow = frameTranspirationCurve.Grid.RowCount-1) then
    begin
      if Total < 0.999 then
      begin
        OKTotal := False;
      end;
    end;
    if not OKTotal then
    begin
      frameTranspirationCurve.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmModflowRipPlantGroups.frameTranspirationCurveGridEndUpdate(
  Sender: TObject);
begin
  inherited;
  frameTranspirationCurve.GridEndUpdate(Sender);
  pbTranspirationCurve.Invalidate;
end;

procedure TfrmModflowRipPlantGroups.GetData;
begin
  FPlantGroups.Assign(frmGoPhast.PhastModel.RipPlantGroups);
  FillTreeView;
  SelectFirstTreeViewNode;
end;

procedure TfrmModflowRipPlantGroups.pbTranspirationCurvePaint(Sender: TObject);
var
  fdh, fdR: Extended;
  DepthList: TList<Double>;
  RateList: TList<Double>;
  RowIndex: Integer;
  TotalDepth: double;
  Grid: TRbwDataGrid4;
  MinX: double;
  MaxX: double;
  MinY: double;
  MaxY: double;
  index: Integer;
  Data: Tdatatype;
  TotalRate: double;
begin
  inherited;

  TotalDepth := 0;
  TotalRate := 0;

  Grid := frameTranspirationCurve.Grid;
  DepthList := TList<Double>.Create;
  RateList := TList<Double>.Create;
  try
    DepthList.Add(0);
    RateList.Add(0);
    for RowIndex := 1 to frameTranspirationCurve.seNumber.AsInteger do
    begin
      if TryStrToFloat(Grid.Cells[Ord(fccFdh), RowIndex], fdh)
        and TryStrToFloat(Grid.Cells[Ord(fccFdr), RowIndex], fdR) then
      begin
        TotalDepth := TotalDepth + fdh;
        DepthList.Add(TotalDepth);
        TotalRate := TotalRate + fdR;
        RateList.Add(TotalRate);
      end;
    end;

    if DepthList.Count > 1 then
    begin
      MinX := 0;
      MaxX := 0;
      MinY := 0;
      MaxY := TotalDepth;
      for index := 1 to RateList.Count - 1 do
      begin
        if RateList[index] > MaxX then
        begin
          MaxX := RateList[index]
        end;
      end;

      if (MaxX > MinX) and (MaxY > MinY) then
      begin
        xysetdataarray(Data, RateList.Count, 1);
        try
          xycleargraph(pbTranspirationCurve,clWhite,clBlack,1);

          xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

          xyxaxis(clBlack,MinX,MaxX,
            (MaxX-MinX)/10,0,StrRate,1,False,False,True, 2);

          xyyaxis(clBlack,MinY,MaxY,
            (MaxY-MinY)/10,0,StrElevation,5,False,False,True, 2);

          for index := 0 to RateList.Count - 1 do
          begin
            Data[index+1, 0] := RateList[index];
            Data[index+1, 1] := DepthList[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,0,2);

          xyfinish;
        except on E: exception do
          begin
            ShowMessage(e.message);
            Exit;
          end;
        end;
      end;

    end;
  finally
    RateList.Free;
    DepthList.Free;
  end;
end;

procedure TfrmModflowRipPlantGroups.SetData;
var
  RemovedGroups: TList<Integer>;
  index: Integer;
  OldPlantGroups: TRipPlantGroups;
  AScreenObject: TScreenObject;
  Undo: TUndoChangePlantGroups;
  RipBoundary: TRipBoundary;
  GroupIndex: integer;
  RipBoundaries: TList<TRipBoundary>;
  AddedGroups: TList<Integer>;
  APlantGroup: TRipPlantGroup;
  OldObjects: TScreenObjectEditCollection;
  NewObjects: TScreenObjectEditCollection;
  ObjectItem: TScreenObjectEditItem;
  ScreenObjects: TList;
begin
  SelectedPlantGroup := nil;

  RemovedGroups := TList<Integer>.Create;
  AddedGroups := TList<Integer>.Create;
  RipBoundaries := TList<TRipBoundary>.Create;
  OldObjects := TScreenObjectEditCollection.Create;
  NewObjects := TScreenObjectEditCollection.Create;
  ScreenObjects := TList.Create;
  try
    OldObjects.OwnScreenObject := True;
    NewObjects.OwnScreenObject := True;
    OldPlantGroups := frmGoPhast.PhastModel.RipPlantGroups;
    for index := 0 to OldPlantGroups.Count - 1 do
    begin
      RemovedGroups.Add(OldPlantGroups[index].ID);
    end;
    for index := 0 to FPlantGroups.Count - 1 do
    begin
      APlantGroup := FPlantGroups[index];
      RemovedGroups.Remove(APlantGroup.ID);
      AddedGroups.Add(APlantGroup.ID);
    end;
    for index := 0 to OldPlantGroups.Count - 1 do
    begin
      AddedGroups.Remove(OldPlantGroups[index].ID);
    end;
    if (RemovedGroups.Count > 0) or (AddedGroups.Count > 0) then
    begin
      for index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[index];
        RipBoundary := AScreenObject.ModflowRipBoundary;
        if (RipBoundary <> nil) and RipBoundary.Used then
        begin
          RipBoundaries.Add(RipBoundary);
          ObjectItem := OldObjects.Add;
          ObjectItem.ScreenObject := TScreenObject.Create(nil);
          ObjectItem.ScreenObject.Assign(AScreenObject);
          ScreenObjects.Add(AScreenObject);
        end;
      end;
      for index := 0 to ScreenObjects.Count - 1 do
      begin
        AScreenObject := ScreenObjects[index];
        ObjectItem := NewObjects.Add;
        ObjectItem.ScreenObject := TScreenObject.Create(nil);
        ObjectItem.ScreenObject.Assign(AScreenObject);
        RipBoundary := ObjectItem.ScreenObject.ModflowRipBoundary;
        for GroupIndex := 0 to RemovedGroups.Count - 1 do
        begin
          RipBoundary.RemovePlantGroup(RemovedGroups[GroupIndex]);
        end;
        for GroupIndex := 0 to AddedGroups.Count - 1 do
        begin
          RipBoundary.AddPlantGroup(AddedGroups[GroupIndex]);
        end;
      end;
    end;
    Undo := TUndoChangePlantGroups.Create(FPlantGroups, ScreenObjects,
      NewObjects, OldObjects);
  finally
    RemovedGroups.Free;
    RipBoundaries.Free;
    AddedGroups.Free;
    OldObjects.Free;
    NewObjects.Free;
    ScreenObjects.Free;
  end;
  Undo.UpdateObservations;
  frmGoPhast.UndoStack.Submit(Undo);
end;

function TfrmModflowRipPlantGroups.AddNewPlantGroup: TRipPlantGroup;
begin
  SelectedPlantGroup := nil;
  result := FPlantGroups.Add;
  result.Name := Format('Plant Group %d', [FPlantGroups.Count]);
end;

function TfrmModflowRipPlantGroups.AddPlantGroupToTreeView(
  APlantGroup: TRipPlantGroup): TTreeNode;
var
  Sibling: TTreeNode;
begin
  if tvPlantGroups.Items.Count > 0 then
  begin
    Sibling := tvPlantGroups.Items[tvPlantGroups.Items.Count - 1];
  end
  else
  begin
    Sibling := nil;
  end;
  result := tvPlantGroups.Items.AddObject(Sibling, APlantGroup.Name, APlantGroup);
end;

procedure TfrmModflowRipPlantGroups.UpdateTreeNodeText(APlantGroup: TRipPlantGroup);
var
  ANdde: TTreeNode;
begin
  ANdde := tvPlantGroups.Items[APlantGroup.Index];
  Assert(ANdde.Data = APlantGroup);
  ANdde.Text := APlantGroup.Name;
end;

procedure TfrmModflowRipPlantGroups.SetSelectedPlantGroup(
  const Value: TRipPlantGroup);
begin
  if (FSelectedPlantGroup <> Value) and (FSelectedPlantGroup <> nil) then
  begin
    AssignPlantGroupPropertiesFromControls(FSelectedPlantGroup);
    UpdateTreeNodeText(FSelectedPlantGroup);
  end;
  FSelectedPlantGroup := Value;
  if (FSelectedPlantGroup <> nil) then
  begin
    AssignControlsFromPlantGroupProperties(FSelectedPlantGroup)
  end;
  lbledtRipName.Enabled := FSelectedPlantGroup <> nil;
  cedSatExtDepth.Enabled := lbledtRipName.Enabled;
  cedActiveRootDepth.Enabled := lbledtRipName.Enabled;
  cedMaxEtFlux.Enabled := lbledtRipName.Enabled;
  cedSatExtinctEvapFlux.Enabled := lbledtRipName.Enabled;
  frameTranspirationCurve.Enabled := lbledtRipName.Enabled;
end;

procedure TfrmModflowRipPlantGroups.tvPlantGroupsChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    SelectedPlantGroup := Node.Data;
  end;
end;

procedure TfrmModflowRipPlantGroups.EditFormula(Sender: TObject);
var
  Edit: TJvComboEdit;
  NewValue: string;
begin
  inherited;
  Edit := Sender as TJvComboEdit;

  NewValue := Edit.Text;
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
      // because the variables will not be evaluated
      // at specific locations.

      PopupParent := self;

      // Show the functions and global variables.
      UpdateTreeList;

      // put the formula in the TfrmFormula.
      Formula := NewValue;
      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        Edit.Text := Formula;
      end;
    finally
      Initialize;
//      Free;
    end;
  end;
end;

{ TUndoChangePlantGroups }

constructor TUndoChangePlantGroups.Create(var NewPlantGroups: TRipPlantGroups;
  const AListOfScreenObjects: TList; var NewScreenObjects,
  OldScreenObjects: TScreenObjectEditCollection);
var
  DummyList: TList;
begin
  DummyList := nil;
  inherited Create(AListOfScreenObjects, NewScreenObjects, OldScreenObjects, DummyList);
  FNewPlantGroups := NewPlantGroups;
  NewPlantGroups := nil;
  FOldPlantGroups := TRipPlantGroups.Create(nil);
  FOldPlantGroups.Assign(frmGoPhast.PhastModel.RipPlantGroups);
end;

function TUndoChangePlantGroups.Description: string;
begin
  result := StrChangeRiparianETP
end;

destructor TUndoChangePlantGroups.Destroy;
begin
  FNewPlantGroups.Free;
  FOldPlantGroups.Free;
  inherited;
end;

procedure TUndoChangePlantGroups.DoCommand;
begin
  frmGoPhast.PhastModel.RipPlantGroups := FNewPlantGroups;
  inherited;
  UpdateTimeLists;
end;

procedure TUndoChangePlantGroups.Redo;
begin
  DoCommand;
  inherited;
end;

procedure TUndoChangePlantGroups.UpdateTimeLists;
var
  RipPackage: TRipPackage;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  RipPackage := frmGoPhast.PhastModel.ModflowPackages.RipPackage;
  RipPackage.UpdateCoverageTimeLists;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    RipPackage := ChildModel.ModflowPackages.RipPackage;
    RipPackage.UpdateCoverageTimeLists;
  end;
end;

procedure TUndoChangePlantGroups.Undo;
begin
  frmGoPhast.PhastModel.RipPlantGroups := FOldPlantGroups;
  inherited;
  UpdateTimeLists;
end;

end.
