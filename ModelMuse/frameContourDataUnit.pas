unit frameContourDataUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameCustomColorUnit, ExtCtrls, ArgusDataEntry, Grids, RbwDataGrid4,
  frameDisplayLimitUnit, SsButtonEd, RbwStringTreeCombo, StdCtrls, ComCtrls,
  JvExComCtrls, JvUpDown, JvExControls, JvxSlider, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvCheckBox, DataSetUnit, VirtualTrees, Vcl.CheckLst,
  JvExCheckLst, JvCheckListBox;

type
  TframeContourData = class(TframeCustomColor)
    btnEditContours: TButton;
    cbSpecifyContours: TJvCheckBox;
    fdContourFont: TFontDialog;
    cbLabelContours: TCheckBox;
    btnContourFont: TButton;
    comboAlgorithm: TComboBox;
    lblAlgorithm: TLabel;
    lblContourInterval: TLabel;
    rdeContourInterval: TRbwDataEntry;
    seLabelSpacing: TJvSpinEdit;
    lblSpacing: TLabel;
    lblModel: TLabel;
    clbxModel: TJvCheckListBox;
    procedure cbSpecifyContoursClick(Sender: TObject);
    procedure btnEditContoursClick(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure cbLogTransformClick(Sender: TObject);
    procedure btnContourFontClick(Sender: TObject);
    procedure cbLabelContoursClick(Sender: TObject);
    procedure clbxModelClickCheck(Sender: TObject);
  private
    FContours: TContours;
    FGettingData: Boolean;
    FContourFont: TFont;
    FUpdatingLegend: Boolean;
    FSelectingNode: Boolean;
    FSettingData: Boolean;
    procedure UpdateTopFrontAndSideItems;
    procedure HandleSelectedObject(AnObject: TObject);
    procedure SetMinMaxLabels;
    procedure HandleLimitChoice(DataSet: TDataArray);
    function GetContourDataSet: TDataArray;
    { Private declarations }
  protected
    function GetSelectedArray: TDataArray; override;
    procedure Loaded; override;
    function CanColorDataSet(DataArray: TDataArray): boolean; override;
  public
    procedure GetData; override;
    procedure SetData; override;
    procedure ContourData(AnObject: TObject);
    procedure UpdateLabelsAndLegend;
    procedure UpdateContours;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frameContourData: TframeContourData;

implementation

uses
  RbwParser, GoPhastTypes, frmGoPhastUnit, ColorSchemes, frmFormulaErrorsUnit,
  frmErrorsAndWarningsUnit, frmProgressUnit, PhastModelUnit, LegendUnit,
  frmSpecifyContoursUnit, ClassificationUnit, Math, AbstractGridUnit,
  SutraMeshUnit, MeshRenumberingTypes;

resourcestring
  StrMinValueS = ' (Min value = %s)';
  StrMaxValueS = ' (Max value = %s)';
  StrWhenUsingLogTrans = 'When contouring log transformed data, you may not ' +
  'specify limits less than or equal to zero. You must correct this on the ' +
  'Filter tab.';

{$R *.dfm}

{ TframeContourData }

procedure TframeContourData.btnContourFontClick(Sender: TObject);
begin
  inherited;
  fdContourFont.Font := FContourFont;
  if fdContourFont.Execute then
  begin
    FContourFont.Assign(fdContourFont.Font);
  end;
end;

procedure TframeContourData.btnEditContoursClick(Sender: TObject);
var
  frmSpecifyContours: TfrmSpecifyContours;
begin
  inherited;
  Application.CreateForm(TfrmSpecifyContours, frmSpecifyContours);
  try
    frmSpecifyContours.GetData(FContours, comboColorScheme.ItemIndex,
      seCycles.AsInteger, seColorExponent.Value, cbLogTransform.Checked);
    if frmSpecifyContours.ShowModal = mrOK then
    begin
      frmSpecifyContours.SetData(FContours);
    end;
  finally
    frmSpecifyContours.Free;
  end;
end;

function TframeContourData.CanColorDataSet(DataArray: TDataArray): boolean;
begin
  Result := inherited CanColorDataSet(DataArray) and DataArray.Visible;
end;

procedure TframeContourData.cbLabelContoursClick(Sender: TObject);
begin
  inherited;
  btnContourFont.Enabled := cbLabelContours.Checked;
end;

procedure TframeContourData.cbLogTransformClick(Sender: TObject);
var
  AnArray: TOneDRealArray;
  Index: Integer;
  Count: Integer;
begin
  inherited;
  if (FContours <> nil)
    and (FContours.LogTransform <> cbLogTransform.Checked) then
  begin
    AnArray := Copy(FContours.ContourValues);
    if cbLogTransform.Checked then
    begin
      Count := 0;
      for Index := 0 to Length(AnArray) - 1 do
      begin
        if AnArray[Index] > 0 then
        begin
          AnArray[Count] := Log10(AnArray[Index]);
          Inc(Count);
        end;
      end;
    end
    else
    begin
      Count := 0;
      for Index := 0 to Length(AnArray) - 1 do
      begin
        AnArray[Index] := Power(10.,AnArray[Index]);
        Inc(Count);
      end;
      SetLength(AnArray, Count);
    end;
    SetLength(AnArray, Count);
    FContours.ContourValues := AnArray;
    FContours.LogTransform := cbLogTransform.Checked
  end;
end;

procedure TframeContourData.cbSpecifyContoursClick(Sender: TObject);
begin
  inherited;
  btnEditContours.Enabled := cbSpecifyContours.Checked;
end;

procedure TframeContourData.clbxModelClickCheck(Sender: TObject);
var
  ItemIndex: Integer;
  AllChecked: Boolean;
begin
  inherited;
  if clbxModel.Selected[0] then
  begin
    for ItemIndex := 1 to clbxModel.Items.Count - 1 do
    begin
      clbxModel.Checked[ItemIndex] := clbxModel.Checked[0];
    end;
  end
  else
  begin
    AllChecked := True;
    for ItemIndex := 1 to clbxModel.Items.Count - 1 do
    begin
      AllChecked := AllChecked and clbxModel.Checked[ItemIndex];
    end;
    clbxModel.Checked[0] := AllChecked;
  end;
end;

procedure TframeContourData.ContourData(AnObject: TObject);
var
  Index: Integer;
  DataSet: TDataArray;
  ContourColors: TColorParameters;
  DataArrayManager: TDataArrayManager;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ContourDataSetAssigned: boolean;
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  ChildDataArray: TDataArray;
  Mesh: IMesh3D;
  ModelIndex: Integer;
  AModel: TCustomModel;
begin
  Application.ProcessMessages;
  frmProgressMM.ShouldContinue := True;
  FreeAndNil(FStoredLegend);
  if AnObject <> nil then
  begin
    frmProgressMM.btnAbort.Visible := False;
    frmProgressMM.Caption := 'Progress';
    frmProgressMM.Show;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataArrayManager.AddDataSetToCache(DataArrayManager.DataSets[Index]);
    end;
    DataArrayManager.CacheDataArrays;

    ContourDataSetAssigned := ((frmGoPhast.Grid <> nil) and
      (frmGoPhast.Grid.ThreeDContourDataSet <> nil))
      or ((frmGoPhast.PhastModel.Mesh3D <> nil) and
      (frmGoPhast.PhastModel.Mesh3D.ThreeDContourDataSet <> nil));

    if ContourDataSetAssigned
      and (rgUpdateLimitChoice.ItemIndex = 1)
      and not cbSpecifyContours.Checked then
    begin
      FStoredLegend := TLegend.Create(nil);
      FStoredLegend.Assign(frmGoPhast.PhastModel.ContourLegend);
    end;
  end;
  if (AnObject = nil) then
  begin
    Grid := frmGoPhast.Grid;

    if Grid <> nil then
    begin
      Grid.TopContourDataSet := nil;
      Grid.FrontContourDataSet := nil;
      Grid.SideContourDataSet := nil;
      Grid.ThreeDContourDataSet := nil;
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.Grid.TopContourDataSet := nil;
          ChildModel.Grid.FrontContourDataSet := nil;
          ChildModel.Grid.SideContourDataSet := nil;
          ChildModel.Grid.ThreeDContourDataSet := nil;
        end;
      end;
    end
    else
    begin
      Mesh :=  frmGoPhast.PhastModel.Mesh3D;
      Assert(Mesh <> nil);
      Mesh.TopContourDataSet := nil;
      Mesh.ThreeDContourDataSet := nil;
    end;
    FLegend.ValueSource := nil;
  end
  else if (AnObject is TDataArray) then
  begin
    if frmGoPhast.ModelSelection in [msModflowLGR, msModflowLGR2] then
    begin
      for ModelIndex := 1 to clbxModel.Items.Count - 1 do
      begin
        AModel := clbxModel.Items.Objects[ModelIndex] as TCustomModel;
        AModel.CanDrawContours := clbxModel.Checked[ModelIndex];
      end;
    end
    else
    begin
      frmGoPhast.PhastModel.CanDrawContours := True;
    end;
    DataSet := TDataArray(AnObject);
    DataSet.ContourAlg := TContourAlg(comboAlgorithm.ItemIndex);
    DataSet.ContourInterval.Value := StrToFloat(rdeContourInterval.Text);
    AssignLimits(DataSet.DataType, DataSet.ContourLimits);
    if FContours <> nil then
    begin
      FContours.SpecifyContours := cbSpecifyContours.Checked;
    end;
    DataSet.Contours := FContours;

    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(DataSet.Name);
        ChildDataArray.ContourAlg := DataSet.ContourAlg;
        ChildDataArray.ContourInterval.Value := DataSet.ContourInterval.Value;
        ChildDataArray.Contours := DataSet.Contours;
      end;
    end;

    if (frmGoPhast.PhastModel.TopContourDataSet = DataSet)
      or (frmGoPhast.PhastModel.FrontContourDataSet = DataSet)
      or (frmGoPhast.PhastModel.SideContourDataSet = DataSet)
      or (frmGoPhast.PhastModel.ThreeDContourDataSet = DataSet)
      then
    begin
      FreeAndNil(FStoredLegend);
    end;

    Grid := frmGoPhast.Grid;
    if Grid <> nil then
    begin
      if Grid.ThreeDContourDataSet <> DataSet then
      begin
        comboMethod.ItemIndex := 0;
      end;
      Grid.ThreeDContourDataSet := DataSet;
      if FTopItems.IndexOfObject(AnObject) >= 0 then
      begin
        Grid.TopContourDataSet := DataSet;
      end
      else
      begin
        Grid.TopContourDataSet := nil;
      end;
      if FFrontItems.IndexOfObject(AnObject) >= 0 then
      begin
        Grid.FrontContourDataSet := DataSet;
      end
      else
      begin
        Grid.FrontContourDataSet := nil;
      end;
      if FSideItems.IndexOfObject(AnObject) >= 0 then
      begin
        Grid.SideContourDataSet := DataSet;
      end
      else
      begin
        Grid.SideContourDataSet := nil;
      end;
    end
    else
    begin
      Mesh :=  frmGoPhast.PhastModel.Mesh3D;
      Assert(Mesh <> nil);
      if Mesh.ThreeDContourDataSet <> DataSet then
      begin
        comboMethod.ItemIndex := 0;
      end;
      if DataSet.Orientation = dso3D then
      begin
        Mesh.ThreeDContourDataSet := DataSet;
      end
      else
      begin
        Mesh.ThreeDContourDataSet := nil;
      end;
      if FTopItems.IndexOfObject(AnObject) >= 0 then
      begin
        Mesh.TopContourDataSet := DataSet;
      end
      else
      begin
        Mesh.TopContourDataSet := nil;
      end;
    end;
    FLegend.ValueSource := DataSet;
    FLegend.ColoringLimits := DataSet.ContourLimits;
  end;
//  if frmGoPhast.Grid <> nil then
//  begin
    frmGoPhast.PhastModel.DiscretizationChanged;
//  end;
  ContourColors := frmGoPhast.PhastModel.ContourColorParameters;
  ContourColors.ColorScheme := comboColorScheme.ItemIndex;
  ContourColors.ColorCycles := seCycles.AsInteger;
  ContourColors.ColorExponent := seColorExponent.Value;
  tabLegend.TabVisible := FLegend.ValueSource <> nil;
  FLegend.ColorParameters := ContourColors;
end;

destructor TframeContourData.Destroy;
begin
  FContourFont.Free;
  FContours.Free;
  inherited;
end;

function TframeContourData.GetContourDataSet: TDataArray;
begin
  result := nil;
  if frmGoPhast.Grid <> nil then
  begin
    result := frmGoPhast.Grid.TopContourDataSet;
    if result = nil then
    begin
      result := frmGoPhast.Grid.FrontContourDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.Grid.SideContourDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.Grid.ThreeDContourDataSet;
    end;
  end
  else if  frmGoPhast.PhastModel.Mesh3D <> nil then
  begin
    result := frmGoPhast.PhastModel.Mesh3D.TopContourDataSet;
//    if result = nil then
//    begin
//      result := frmGoPhast.PhastModel.Mesh.FrontContourDataSet;
//    end;
//    if result = nil then
//    begin
//      result := frmGoPhast.PhastModel.Mesh.SideContourDataSet;
//    end;
    if result = nil then
    begin
      result := frmGoPhast.PhastModel.Mesh3D.ThreeDContourDataSet;
    end;
  end;
end;

procedure TframeContourData.GetData;
var
  ContourColors: TColorParameters;
  VirtNoneNode: PVirtualNode;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ItemIndex: Integer;
  AllCanDrawContours: Boolean;
begin
  inherited;
  Handle;

  if FSettingData then
  begin
    Exit;
  end;

  FGettingData := True;
  try
    if frmGoPhast.ModelSelection in [msModflowLGR, msModflowLGR2] then
    begin
      clbxModel.Items.Clear;
      clbxModel.Visible := True;
      lblModel.Visible := True;
      clbxModel.Items.Add('All');
      clbxModel.Items.AddObject('Parent', frmGoPhast.PhastModel);
      ItemIndex := 1;
      clbxModel.Checked[ItemIndex] := frmGoPhast.PhastModel.CanDrawContours;
      AllCanDrawContours := frmGoPhast.PhastModel.CanDrawContours;
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          clbxModel.Items.AddObject(ChildModel.ModelName, ChildModel);
          Inc(ItemIndex);
          clbxModel.Checked[ItemIndex] := ChildModel.CanDrawContours;
          AllCanDrawContours := AllCanDrawContours and ChildModel.CanDrawContours
        end;
      end;
      clbxModel.Checked[0] := AllCanDrawContours;
    end
    else
    begin
      clbxModel.Visible := False;
      lblModel.Visible := False;
    end;
    if frmGoPhast.PhastModel.ColorSchemes.Count > 0 then
    begin
      UpdateColorSchemes;
    end;
    cbLabelContours.Checked := frmGoPhast.PhastModel.ShowContourLabels;
    FContourFont.Assign(frmGoPhast.PhastModel.ContourFont);
    virttreecomboDataSets.Tree.Clear;
    FFrontItems.Clear;
    FSideItems.Clear;
    FTopItems.Clear;

    VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
    virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

    if (frmGoPhast.PhastModel = nil)
      or (csDestroying in frmGoPhast.PhastModel.ComponentState)
      or frmGoPhast.PhastModel.Clearing then
    begin
      Exit;
    end;

    seLabelSpacing.AsInteger := frmGoPhast.PhastModel.ContourLabelSpacing;

    GetDataSets;
    UpdateTopFrontAndSideItems;

    virttreecomboDataSetsChange(nil);
  finally
    FGettingData := False;
  end;

  ContourColors := frmGoPhast.PhastModel.ContourColorParameters;
  comboColorScheme.ItemIndex := ContourColors.ColorScheme;
  seCycles.Value := ContourColors.ColorCycles;
  jsColorExponent.Value := Round(ContourColors.ColorExponent*100);
  seColorExponent.Value := ContourColors.ColorExponent;
end;

function TframeContourData.GetSelectedArray: TDataArray;
begin
  if frmGoPhast.Grid = nil then
  begin
    result := frmGoPhast.PhastModel.Mesh3D.TopContourDataSet;
    if result = nil then
    begin
      result := frmGoPhast.PhastModel.Mesh3D.ThreeDContourDataSet;
    end;
  end
  else
  begin
    result := frmGoPhast.Grid.ThreeDContourDataSet;
    if result = nil then
    begin
      result := frmGoPhast.Grid.TopContourDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.Grid.FrontContourDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.Grid.SideContourDataSet;
    end;
  end;

end;

procedure TframeContourData.HandleLimitChoice(DataSet: TDataArray);
var
  ContourDataSet: TDataArray;
begin
  case rgUpdateLimitChoice.ItemIndex of
    0:
      begin
        ReadLimits(DataSet.DataType, DataSet.ContourLimits);
      end;
    1:
      begin
        ContourDataSet := GetContourDataSet;
        if (ContourDataSet <> nil) and (DataSet.DataType = ContourDataSet.DataType) then
        begin
          ReadLimits(ContourDataSet.DataType, ContourDataSet.ContourLimits);
        end
        else
        begin
          ReadLimits(DataSet.DataType, DataSet.ContourLimits);
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TframeContourData.HandleSelectedObject(AnObject: TObject);
var
  DataSet: TDataArray;
  Contours: TContours;
  ContourDataSet: TDataArray;
begin
  if (AnObject = nil) then
  begin
    frameCheck3DMin.Enabled := False;
    frameCheck3DMax.Enabled := False;
    reComment.Enabled := False;
    cbSpecifyContours.Checked := False;
    cbSpecifyContours.Enabled := False;
    rdgValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.AsInteger := 0;
    seNumberOfValuesToIgnoreChange(nil);
    cbLogTransform.Enabled := False;
    rdgValuesToIgnore.Cells[0,1] := '';
    rdeContourInterval.Enabled := False;
  end
  else if (AnObject is TDataArray) then
  begin
    reComment.Enabled := True;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    DataSet := TDataArray(AnObject);
    comboAlgorithm.ItemIndex := Ord(DataSet.ContourAlg);
    if comboAlgorithm.ItemIndex < 0 then
    begin
      comboAlgorithm.ItemIndex := 0;
    end;
    rdgValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;

    reComment.Text := DataSet.Comment;
    cbLogTransform.Enabled := DataSet.DataType = rdtDouble;
    HandleLimitChoice(DataSet);
    cbSpecifyContours.Enabled := True;
    rdeContourInterval.Enabled := True;
    if DataSet.DataType = rdtDouble then
    begin
      rdeContourInterval.DataType := dtReal;
    end
    else
    begin
      rdeContourInterval.DataType := dtInteger;
    end;

    case rgUpdateLimitChoice.ItemIndex of
      0:
        begin
          Contours := DataSet.Contours;
          cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
        end;
      1:
        begin
          ContourDataSet := GetContourDataSet;
          if (ContourDataSet <> nil) and (DataSet.DataType = ContourDataSet.DataType) then
          begin
            Contours := ContourDataSet.Contours;
            DataSet.Contours := Contours;
            DataSet.ContourInterval.Value := ContourDataSet.ContourInterval.Value;
            cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
          end
          else
          begin
            Contours := DataSet.Contours;
            cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
          end;
        end;
      else
        Assert(False);
    end;
    rdeContourInterval.Text := FloatToStr(DataSet.ContourInterval.Value);
  end;
  UpdateLabelsAndLegend;
end;

procedure TframeContourData.Loaded;
begin
  inherited;
  FLegend := frmGoPhast.PhastModel.ContourLegend;
  FLegend.LegendType := ltContour;

  cbSpecifyContours.Parent := tabSelection;
  btnEditContours.Parent := tabSelection;
  FContourFont := TFont.Create;
end;

procedure TframeContourData.SetData;
var
  AnObject: TObject;
begin
  if cbLogTransform.Checked then
  begin
    if (not frameCheck3DMin.OkLogLimit) or (not frameCheck3DMax.OkLogLimit) then
    begin
      Beep;
      MessageDlg(StrWhenUsingLogTrans, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  FSettingData := True;
  try
    frmFormulaErrors.sgErrors.BeginUpdate;
    Screen.Cursor := crHourGlass;
    try
      frmGoPhast.PhastModel.ContourFont := FContourFont;
      frmGoPhast.PhastModel.ShowContourLabels := cbLabelContours.Checked;
      frmGoPhast.PhastModel.ContourLabelSpacing := seLabelSpacing.AsInteger;

      frmGoPhast.PhastModel.InvalidateContours;

      RetrieveSelectedObject(AnObject);
      ContourData(AnObject);

      if (AnObject <> nil) and frmErrorsAndWarnings.HasMessages then
      begin
        frmErrorsAndWarnings.Show;
      end;

    finally
      Screen.Cursor := crDefault;
      frmFormulaErrors.sgErrors.EndUpdate;
    end;
    UpdateLabelsAndLegend;
    frmProgressMM.Hide;
  finally
    FSettingData := False;
  end;
end;

procedure TframeContourData.SetMinMaxLabels;
var
  AnObject: TObject;
  DataSet: TDataArray;
begin
  RetrieveSelectedObject(AnObject);
  lblLowerLimit.Caption := StrLowerLimit;
  lblUpperLimit.Caption := StrUpperLimit;
  if (AnObject = nil) then
  begin
    FLegend.ValueSource := nil;
  end
  else if (AnObject is TDataArray) then
  begin
    if (frmGoPhast.ModelSelection in ModflowSelection) and (not frmGoPhast.DisvUsed) then
    begin
      frmGoPhast.ModflowGrid.UpdateCellElevations;
    end;
    DataSet := TDataArray(AnObject);
    lblLowerLimit.Caption := StrLowerLimit
      + Format(StrMinValueS, [DataSet.MinValue]);
    lblUpperLimit.Caption := StrUpperLimit
      + Format(StrMaxValueS, [DataSet.MaxValue]);
    FLegend.ValueSource := DataSet;
  end;
end;

procedure TframeContourData.UpdateContours;
var
  DataArray: TDataArray;
  AnObject: TObject;
begin
  RetrieveSelectedObject(AnObject);
  if (AnObject <> nil) then
  begin
    DataArray := AnObject as TDataArray;
    if DataArray.Contours = nil then
    begin
      FreeAndNil(FContours);
    end
    else
    begin
      if FContours = nil then
      begin
        FContours := TContours.Create;
      end;
      FContours.Assign(DataArray.Contours);
    end;
  end;
end;

procedure TframeContourData.UpdateLabelsAndLegend;
begin
  if FGettingData or FUpdatingLegend or (frmGoPhast.PhastModel = nil)
    or (csDestroying in frmGoPhast.PhastModel.ComponentState)
    or frmGoPhast.PhastModel.Clearing then
  begin
    Exit;
  end;
  FUpdatingLegend := True;
  try
    if not FSelectingNode {and not FSettingData} then
    begin
      GetData;
    end;
    SetMinMaxLabels;
    FLegend.Contours := FContours;
    UpdateLegend;
  finally
    FUpdatingLegend := False;
  end;
end;

procedure TframeContourData.UpdateTopFrontAndSideItems;
begin
  StoreDataSetsInLists;

  FinalizeList(FTopItems);
  FinalizeList(FFrontItems);
  FinalizeList(FSideItems);
end;

procedure TframeContourData.virttreecomboDataSetsChange(Sender: TObject);
var
  AnObject: TObject;
begin
  inherited;
  ResetTreeText;
  RetrieveSelectedObject(AnObject);
  HandleSelectedObject(AnObject);
  UpdateContours;
end;

procedure TframeContourData.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectingNode := True;
  try
    inherited;
//    SetSelectedNode(Sender, Node);
  finally
    FSelectingNode := False;
  end;
//  SetSelectedNode(Sender, Node);
end;

procedure TframeContourData.virttreecomboDataSetsTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

end.
