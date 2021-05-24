unit frameColorGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameCustomColorUnit, ExtCtrls, ArgusDataEntry, Grids, RbwDataGrid4,
  frameDisplayLimitUnit, SsButtonEd, RbwStringTreeCombo, StdCtrls, ComCtrls,
  JvExComCtrls, JvUpDown, JvExControls, JvxSlider, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvCombobox, DataSetUnit, VirtualTrees;

type
  TframeColorGrid = class(TframeCustomColor)
    lblTime: TLabel;
    udTime: TJvUpDown;
    comboTime3D: TJvComboBox;
    procedure comboTime3DChange(Sender: TObject);
    procedure udTimeChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure virttreecomboDataSetsTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsChange(Sender: TObject);
  private
    FEdgeEdits: TList;
    FBoundaryClassifications: TList;
    FGettingData: Boolean;
    FUpdatingLegend: Boolean;
    FSelectingNode: Boolean;
    FSettingData: Boolean;
    procedure GetBoundaryConditions;
    procedure UpdateTopFrontAndSideItems;
    procedure SetTimeComboColor;
    procedure StoreBoundaryDataSetsInLists;
    procedure StoreTimelistsInLists;
    procedure HandleSelectedObject(AnObject: TObject);
    procedure SetMinMaxLabels;
    procedure HandleLimitChoice(DataSet: TDataArray);
  protected
    function GetSelectedArray: TDataArray; override;
    procedure Loaded; override;
    { Private declarations }
  public
    procedure GetData; override;
    // @name sets the @link(TDataArray) used to color the @link(TCustomModelGrid).
    procedure SetData; override;
    procedure UpdateLabelsAndLegend;
    procedure ColorGrid(AnObject: TObject);
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frameColorGrid: TframeColorGrid;

implementation

uses
  frmGoPhastUnit, ColorSchemes, frmFormulaErrorsUnit,
  frmErrorsAndWarningsUnit, frmCustomGoPhastUnit, PhastModelUnit, GoPhastTypes,
  RbwParser, frmProgressUnit, LegendUnit, Contnrs, RealListUnit,
  ClassificationUnit, SutraMeshUnit, ModflowHfbDisplayUnit, ModelMuseUtilities;

resourcestring
  StrProgress = 'Progress';
  StrMinValueS = ' (Min value = %s)';
  StrMaxValueS = ' (Max value = %s)';

{$R *.dfm}

{ TframeColorGrid }

procedure TframeColorGrid.ColorGrid(AnObject: TObject);
var
  GridColors: TColorParameters;
  EdgeEdit: TEdgeDisplayEdit;
  Time: Double;
  ADataArray: TDataArray;
  TimeIndex: Integer;
  TimeList: TCustomTimeList;
  DataSet: TDataArray;
  Index: Integer;
  Is3DSelected: Boolean;
  DataArrayManager: TDataArrayManager;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
  ChildIndex: Integer;
  ThreeDDataSet: TDataArray;
begin
  Application.ProcessMessages;
  frmProgressMM.ShouldContinue := True;

  ThreeDDataSet := frmGoPhast.PhastModel.ThreeDDataSet;
  Is3DSelected := ThreeDDataSet <> nil;
  FreeAndNil(FStoredLegend);
  if AnObject <> nil then
  begin
    frmProgressMM.btnAbort.Visible := False;
    frmProgressMM.Caption := StrProgress;
    frmProgressMM.Show;

    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataArrayManager.AddDataSetToCache(DataArrayManager.DataSets[Index]);
    end;
    DataArrayManager.CacheDataArrays;
    if (ThreeDDataSet <> nil)
      and (rgUpdateLimitChoice.ItemIndex = 1) then
    begin
      FStoredLegend := TLegend.Create(nil);
      FStoredLegend.Assign(frmGoPhast.PhastModel.ColorLegend);
    end;
  end;
  if (AnObject = nil) then
  begin
    frmGoPhast.PhastModel.TopDataSet := nil;
    frmGoPhast.PhastModel.TopTimeList := nil;
    frmGoPhast.PhastModel.FrontDataSet := nil;
    frmGoPhast.PhastModel.FrontTimeList := nil;
    frmGoPhast.PhastModel.SideDataSet := nil;
    frmGoPhast.PhastModel.SideTimeList := nil;
    frmGoPhast.PhastModel.ThreeDDataSet := nil;
    frmGoPhast.PhastModel.ThreeDTimeList := nil;
    frmGoPhast.PhastModel.EdgeDisplay := nil;
    comboMethod.ItemIndex := 0;
    FLegend.ValueSource := nil;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.TopDataSet := nil;
      ChildModel.TopTimeList := nil;
      ChildModel.FrontDataSet := nil;
      ChildModel.FrontTimeList := nil;
      ChildModel.SideDataSet := nil;
      ChildModel.SideTimeList := nil;
      ChildModel.ThreeDDataSet := nil;
      ChildModel.ThreeDTimeList := nil;
      ChildModel.EdgeDisplay := nil;
    end;
  end
  else if (AnObject is TDataArray) then
  begin
    frmGoPhast.PhastModel.EdgeDisplay := nil;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.EdgeDisplay := nil;
    end;
    DataSet := TDataArray(AnObject);
    AssignLimits(DataSet.DataType, DataSet.Limits);
    if frmGoPhast.PhastModel.ThreeDDataSet <> DataSet then
    begin
      comboMethod.ItemIndex := 0;
    end;
    if (DataSet.Orientation = dso3D) then
    begin
      frmGoPhast.PhastModel.ThreeDDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.PhastModel.ThreeDDataSet := nil;
    end;

    frmGoPhast.PhastModel.ThreeDTimeList := nil;
    if FTopItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.TopDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.PhastModel.TopDataSet := nil;
    end;
    frmGoPhast.PhastModel.TopTimeList := nil;
    if FFrontItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.FrontDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.PhastModel.FrontDataSet := nil;
    end;
    frmGoPhast.PhastModel.FrontTimeList := nil;
    if FSideItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.SideDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.PhastModel.SideDataSet := nil;
    end;
    frmGoPhast.PhastModel.SideTimeList := nil;
    FLegend.ValueSource := DataSet;
    FLegend.ColoringLimits := DataSet.Limits;
  end
  else if AnObject is TCustomTimeList then
  begin
    frmGoPhast.PhastModel.EdgeDisplay := nil;
    TimeList := TCustomTimeList(AnObject);
    AssignLimits(TimeList.DataType, TimeList.Limits);
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildTimeList := ChildModel.GetTimeListByName(TimeList.Name);
      AssignLimits(ChildTimeList.DataType, ChildTimeList.Limits);
      ChildModel.EdgeDisplay := nil;
    end;

    Time := FortranStrToFloat(comboTime3D.Text);
    if (frmGoPhast.PhastModel.ThreeDTimeList <> TimeList)
      or (Time <> frmGoPhast.PhastModel.ThreeDDisplayTime) then
    begin
      comboMethod.ItemIndex := 0;
      TimeList.Invalidate;
//      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
//      begin
//        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
//        ChildTimeList := ChildModel.GetTimeListByName(TimeList.Name);
//        ChildTimeList.Invalidate;
//      end;
    end;

    if TimeList.UpToDate then
    begin
      for TimeIndex := 0 to TimeList.Count - 1 do
      begin
        ADataArray := TimeList[TimeIndex];
        AssignLimits(ADataArray.DataType, ADataArray.Limits);
      end;
    end;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildTimeList := ChildModel.GetTimeListByName(TimeList.Name);
      if ChildTimeList.UpToDate then
      begin
        for TimeIndex := 0 to ChildTimeList.Count - 1 do
        begin
          ADataArray := ChildTimeList[TimeIndex];
          AssignLimits(ADataArray.DataType, ADataArray.Limits);
        end;
      end;
    end;
    frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(TimeList, Time);
    if FTopItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.UpdateTopTimeDataSet(TimeList, Time);
    end
    else
    begin
      frmGoPhast.PhastModel.TopDataSet := nil;
      frmGoPhast.PhastModel.TopTimeList := nil;
    end;
    if FFrontItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.UpdateFrontTimeDataSet(TimeList, Time);
    end
    else
    begin
      frmGoPhast.PhastModel.FrontDataSet := nil;
      frmGoPhast.PhastModel.FrontTimeList := nil;
    end;
    if FSideItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.PhastModel.UpdateSideTimeDataSet(TimeList, Time);
    end
    else
    begin
      frmGoPhast.PhastModel.SideDataSet := nil;
      frmGoPhast.PhastModel.SideTimeList := nil;
    end;
    ADataArray := nil;
    if frmGoPhast.PhastModel.TopDataSet <> nil then
    begin
      ADataArray := frmGoPhast.PhastModel.TopDataSet;
    end
    else if frmGoPhast.PhastModel.FrontDataSet <> nil then
    begin
      ADataArray := frmGoPhast.PhastModel.FrontDataSet;
    end
    else if frmGoPhast.PhastModel.SideDataSet <> nil then
    begin
      ADataArray := frmGoPhast.PhastModel.SideDataSet;
    end;
    FLegend.ValueSource := ADataArray;
    if ADataArray <> nil then
    begin
      FLegend.ColoringLimits := ADataArray.Limits;
    end;
  end
  else if AnObject is TEdgeDisplayEdit then
  begin
    frmGoPhast.PhastModel.TopDataSet := nil;
    frmGoPhast.PhastModel.TopTimeList := nil;
    frmGoPhast.PhastModel.FrontDataSet := nil;
    frmGoPhast.PhastModel.FrontTimeList := nil;
    frmGoPhast.PhastModel.SideDataSet := nil;
    frmGoPhast.PhastModel.SideTimeList := nil;
    frmGoPhast.PhastModel.ThreeDDataSet := nil;
    frmGoPhast.PhastModel.ThreeDTimeList := nil;
    EdgeEdit := TEdgeDisplayEdit(AnObject);
    if comboTime3D.Enabled then
    begin
      EdgeEdit.DisplayTime := StrToFloat(comboTime3D.Text);
      frmGoPhast.PhastModel.ThreeDDisplayTime := EdgeEdit.DisplayTime;
    end;
    AssignLimits(rdtDouble, EdgeEdit.Edge.Limits[EdgeEdit.DataIndex]);
    if (frmGoPhast.PhastModel.EdgeDisplay <> EdgeEdit.Edge)
      or (EdgeEdit.Edge.DataToPlot <> EdgeEdit.DataIndex) then
    begin
      comboMethod.ItemIndex := 0;
    end;
    EdgeEdit.Edge.DataToPlot := EdgeEdit.DataIndex;
    frmGoPhast.PhastModel.EdgeDisplay := EdgeEdit.Edge;
    FLegend.ValueSource := EdgeEdit.Edge;
    FLegend.ColoringLimits := EdgeEdit.Edge.Limits[EdgeEdit.DataIndex];
    FLegend.EdgeDataToPlot := EdgeEdit.DataIndex;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.TopDataSet := nil;
      ChildModel.TopTimeList := nil;
      ChildModel.FrontDataSet := nil;
      ChildModel.FrontTimeList := nil;
      ChildModel.SideDataSet := nil;
      ChildModel.SideTimeList := nil;
      ChildModel.ThreeDDataSet := nil;
      ChildModel.ThreeDTimeList := nil;
      if frmGoPhast.PhastModel.EdgeDisplay = frmGoPhast.PhastModel.HfbDisplayer then
      begin
        ChildModel.HfbDisplayer.DataToPlot := EdgeEdit.DataIndex;
        ChildModel.EdgeDisplay := ChildModel.HfbDisplayer;
      end
      else
      begin
        Assert(False);
      end;
    end;

  end
  else
  begin
    Assert(False);
  end;
  frmGoPhast.acColoredGrid.Enabled := (frmGoPhast.PhastModel.ThreeDDataSet <> nil)
    or (frmGoPhast.PhastModel.EdgeDisplay <> nil);
  if not frmGoPhast.acColoredGrid.Enabled then
  begin
    frmGoPhast.acColoredGrid.Checked := False;
    frmGoPhast.tb3DColors.Down := False;
  end;
  if frmGoPhast.acColoredGrid.Enabled and not Is3DSelected then
  begin
    frmGoPhast.acColoredGrid.Checked := True;
    frmGoPhast.tb3DColors.Down := True;
  end;
  frmGoPhast.PhastModel.DiscretizationChanged;
  GridColors := frmGoPhast.PhastModel.GridColorParameters;
  GridColors.ColorScheme := comboColorScheme.ItemIndex;
  GridColors.ColorCycles := seCycles.AsInteger;
  GridColors.ColorExponent := seColorExponent.Value;
  tabLegend.TabVisible := FLegend.ValueSource <> nil;
  FLegend.ColorParameters := GridColors;
  if FStoredLegend <> nil then
  begin
    FLegend.Assign(FStoredLegend);
  end;
end;

procedure TframeColorGrid.comboTime3DChange(Sender: TObject);
begin
  inherited;
  SetTimeComboColor;
end;

destructor TframeColorGrid.Destroy;
begin
  FEdgeEdits.Free;
  FBoundaryClassifications.Free;
  inherited;
end;

procedure TframeColorGrid.GetBoundaryConditions;
begin
//  if frmGoPhast.Grid = nil then
//  begin
//
//  end
//  else
  begin
    FillVirtStrTreeWithBoundaryConditions(frmGoPhast.PhastModel.ThreeDDataSet,
      frmGoPhast.PhastModel.ThreeDTimeList, frmGoPhast.PhastModel.EdgeDisplay,
      FBoundaryClassifications, FEdgeEdits, virttreecomboDataSets.Tree);
  end;
end;

procedure TframeColorGrid.GetData;
var
  GridColors: TColorParameters;
  VirtNoneNode: PVirtualNode;
  EndTime: Double;
  TimeIndex: Integer;
  AllTimes: TRealList;
begin
  inherited;
  Handle;
  FGettingData := True;
  try
    if frmGoPhast.PhastModel.ColorSchemes.Count > 0 then
    begin
      UpdateColorSchemes;
    end;
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

    GetDataSets;
    GetBoundaryConditions;
    UpdateTopFrontAndSideItems;
    virttreecomboDataSetsChange(nil);

  finally
    FGettingData := False;
  end;

  comboTime3D.Handle;
  comboTime3D.Text := FloatToStr(frmGoPhast.PhastModel.ThreeDDisplayTime);

  GridColors := frmGoPhast.PhastModel.GridColorParameters;
  comboColorScheme.ItemIndex := GridColors.ColorScheme;
  seCycles.Value := GridColors.ColorCycles;
  jsColorExponent.Value := Round(GridColors.ColorExponent*100);
  seColorExponent.Value := GridColors.ColorExponent;

  if frmGoPhast.ModelSelection in ModflowSelection then
  begin
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillStringsWithStartTimes(comboTime3D.Items);
    EndTime := frmGoPhast.PhastModel.ModflowStressPeriods[
      frmGoPhast.PhastModel.ModflowStressPeriods.Count-1].EndTime;
    comboTime3D.Items.Add(FloatToStr(EndTime));
  end
  else if frmGoPhast.ModelSelection in SutraSelection then
  begin
    frmGoPhast.PhastModel.SutraTimeOptions.CalculateAllTimes;
    AllTimes := frmGoPhast.PhastModel.SutraTimeOptions.AllTimes;
    comboTime3D.Items.Clear;
    comboTime3D.Items.Capacity := AllTimes.Count;
    for TimeIndex := 0 to AllTimes.Count - 1 do
    begin
      comboTime3D.Items.Add(FloatToStr(AllTimes[TimeIndex]));
    end;
  end;


end;

function TframeColorGrid.GetSelectedArray: TDataArray;
begin
//  if frmGoPhast.PhastModel = nil then
//  begin
//    result :=  nil;
//  end
//  else
//  begin
    result := frmGoPhast.PhastModel.ThreeDDataSet;
    if result = nil then
    begin
      result := frmGoPhast.PhastModel.TopDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.PhastModel.FrontDataSet;
    end;
    if result = nil then
    begin
      result := frmGoPhast.PhastModel.SideDataSet;
    end;

//  end;
end;

procedure TframeColorGrid.HandleLimitChoice(DataSet: TDataArray);
var
  ColorDataSet: TDataArray;
  Mesh: TSutraMesh3D;
begin
  case rgUpdateLimitChoice.ItemIndex of
    0:
      begin
        ReadLimits(DataSet.DataType, DataSet.Limits);
      end;
    1:
      begin
        ColorDataSet := nil;
        if frmGoPhast.Grid <> nil then
        begin
          ColorDataSet := frmGoPhast.Grid.TopDataSet;
          if ColorDataSet = nil then
          begin
            ColorDataSet := frmGoPhast.Grid.FrontDataSet;
          end;
          if ColorDataSet = nil then
          begin
            ColorDataSet := frmGoPhast.Grid.SideDataSet;
          end;
        end
        else
        begin
          Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
          if Mesh <> nil then
          begin
            ColorDataSet := Mesh.TopDataSet;
            if ColorDataSet = nil then
            begin
              ColorDataSet := Mesh.ThreeDDataSet;
            end;
          end;
        end;
        if ColorDataSet = nil then
        begin
          ColorDataSet := frmGoPhast.PhastModel.ThreeDDataSet;
        end;
        if (ColorDataSet <> nil) and (DataSet.DataType = ColorDataSet.DataType) then
        begin
          ReadLimits(ColorDataSet.DataType, ColorDataSet.Limits);
        end
        else
        begin
          ReadLimits(DataSet.DataType, DataSet.Limits);
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TframeColorGrid.HandleSelectedObject(AnObject: TObject);
var
  DataSet: TDataArray;
  GridDisplay: TEdgeDisplayEdit;
  TimeList: TCustomTimeList;
begin
  if (csLoading in ComponentState) or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if (AnObject = nil) then
  begin
    frameCheck3DMin.Enabled := False;
    frameCheck3DMax.Enabled := False;
    reComment.Enabled := False;
    comboTime3D.Enabled := False;
    cbActiveOnly.Enabled := False;
    rdgValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.AsInteger := 0;
    seNumberOfValuesToIgnoreChange(nil);
    rdgValuesToIgnore.Cells[0,1] := '';
    cbLogTransform.Enabled := False;
    SetTimeComboColor;
  end
  else if (AnObject is TDataArray) then
  begin
    reComment.Enabled := True;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    cbActiveOnly.Enabled := true;
    DataSet := TDataArray(AnObject);
    reComment.Text := DataSet.Comment;
    rdgValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;
    comboTime3D.Enabled := False;
    cbLogTransform.Enabled := DataSet.DataType = rdtDouble;
    SetTimeComboColor;
    HandleLimitChoice(DataSet);
  end
  else if AnObject is TCustomTimeList then
  begin
    reComment.Enabled := False;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    cbActiveOnly.Enabled := true;
    comboTime3D.Enabled := true;
    SetTimeComboColor;
    TimeList := TCustomTimeList(AnObject);
    rdgValuesToIgnore.Enabled := TimeList.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := TimeList.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;
    cbLogTransform.Enabled := TimeList.DataType = rdtDouble;
    ReadLimits(TimeList.DataType, TimeList.Limits);
  end
  else
  begin
    reComment.Enabled := False;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    GridDisplay := AnObject as TEdgeDisplayEdit;
    cbActiveOnly.Enabled := True;
    comboTime3D.Enabled := (frmGoPhast.ModelSelection = msModflow2015)
      and (GridDisplay.Edge is THfbDisplayer);
    if comboTime3D.Enabled then
    begin
      comboTime3D.Text := GridDisplay.DisplayTime.ToString;
    end;
    rdgValuesToIgnore.Enabled := True;
    seNumberOfValuesToIgnore.Enabled := True;
    SetTimeComboColor;
    GridDisplay.Edge.DataToPlot := GridDisplay.DataIndex;
    GridDisplay.Edge.UpdateMinMax;
    cbLogTransform.Enabled := True;
    ReadLimits(rdtDouble, GridDisplay.Edge.Limits[GridDisplay.DataIndex]);
  end;
  UpdateLabelsAndLegend;
end;

procedure TframeColorGrid.Loaded;
begin
  inherited;
  FLegend := frmGoPhast.PhastModel.ColorLegend;
  FLegend.LegendType := ltColor;

  FBoundaryClassifications := TObjectList.Create;

  udTime.Max := High(SmallInt);
  udTime.Min := Low(SmallInt);
  FEdgeEdits := TObjectList.Create;

  lblTime.Parent := tabSelection;
  comboTime3D.Parent := tabSelection;
  udTime.Parent := tabSelection;
end;

procedure TframeColorGrid.SetData;
var
  AnObject: TObject;
begin
  FSettingData := True;
  try
    frmGoPhast.CanDraw := False;
    Screen.Cursor := crHourGlass;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      RetrieveSelectedObject(AnObject);
      ColorGrid(AnObject);

      if (AnObject <> nil) and frmErrorsAndWarnings.HasMessages then
      begin
        frmErrorsAndWarnings.Show;
      end;

    finally
      frmFormulaErrors.sgErrors.EndUpdate;
      Screen.Cursor := crDefault;
      frmGoPhast.CanDraw := True;
    end;

    UpdateLabelsAndLegend;
    FLegend.Contours := nil;
    frmProgressMM.Hide;
  finally
    FSettingData := False;
  end;
end;

procedure TframeColorGrid.SetMinMaxLabels;
var
  AnObject: TObject;
  DataSet: TDataArray;
  TimeList: TCustomTimeList;
  ATime: Extended;
  GridDisplay: TEdgeDisplayEdit;
begin
  RetrieveSelectedObject(AnObject);

  lblLowerLimit.Caption := StrLowerLimit;
  lblUpperLimit.Caption := StrUpperLimit;
  if (AnObject = nil) then
  begin
  end
  else if (AnObject is TDataArray) then
  begin
    DataSet := TDataArray(AnObject);
    if (frmGoPhast.ModelSelection in ModflowSelection) 
      and (not frmGoPhast.DisvUsed) 
      and (DataSet.Orientation = dso3D)
      and (not frmGoPhast.ModflowGrid.UpdatingElevations) then
    begin
      frmGoPhast.ModflowGrid.UpdateCellElevations;
    end;
    lblLowerLimit.Caption := StrLowerLimit
      + Format(StrMinValueS, [DataSet.MinValue]);
    lblUpperLimit.Caption := StrUpperLimit
      + Format(StrMaxValueS, [DataSet.MaxValue]);
    LegendDataSource := DataSet;
  end
  else if AnObject is TCustomTimeList then
  begin
    TimeList := TCustomTimeList(AnObject);
    if (frmGoPhast.ModelSelection in ModflowSelection)
      and (not frmGoPhast.DisvUsed)
      and (TimeList.Orientation = dso3D)
      and (not frmGoPhast.ModflowGrid.UpdatingElevations) then
    begin
      frmGoPhast.ModflowGrid.UpdateCellElevations;
    end;
    if TryStrToFloat(comboTime3D.Text, ATime) then
    begin
      lblLowerLimit.Caption := StrLowerLimit
        + Format(StrMinValueS, [TimeList.MinValue(ATime)]);
      lblUpperLimit.Caption := StrUpperLimit
        + Format(StrMaxValueS, [TimeList.MaxValue(ATime)]);
    end;
  end
  else
  begin
    GridDisplay := AnObject as TEdgeDisplayEdit;
    lblLowerLimit.Caption := StrLowerLimit
      + Format(StrMinValueS, [GridDisplay.Edge.MinValue]);
    lblUpperLimit.Caption := StrUpperLimit
      + Format(StrMaxValueS, [GridDisplay.Edge.MaxValue]);
  end;
end;

procedure TframeColorGrid.SetTimeComboColor;
var
  ATime: Double;
begin
  if comboTime3D.Enabled then
  begin
    if (comboTime3D.Text = '') or TryStrToFloat(comboTime3D.Text, ATime) then
    begin
      comboTime3D.Color := clWindow;
    end
    else
    begin
      comboTime3D.Color := clRed;
    end;
  end
  else
  begin
    comboTime3D.Color := clBtnFace;
  end;
end;

procedure TframeColorGrid.StoreBoundaryDataSetsInLists;
var
  Index: Integer;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.BoundaryDataSetCount - 1 do
  begin
    DataSet := DataArrayManager.BoundaryDataSets[Index];
    case DataSet.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
      dso3D:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
          FFrontItems.AddObject(DataSet.Name, DataSet);
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TframeColorGrid.StoreTimelistsInLists;
var
  TimeList: TCustomTimeList;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
  begin
    TimeList := frmGoPhast.PhastModel.TimeLists[Index];
    case TimeList.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(TimeList.Name, TimeList);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(TimeList.Name, TimeList);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(TimeList.Name, TimeList);
        end;
      dso3D:
        begin
          FTopItems.AddObject(TimeList.Name, TimeList);
          FFrontItems.AddObject(TimeList.Name, TimeList);
          FSideItems.AddObject(TimeList.Name, TimeList);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TframeColorGrid.udTimeChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
var
  ATime: double;
  NewIndex: Integer;
  RealList: TRealList;
  Index: Integer;
begin
  inherited;
  NewIndex := -1;
  if comboTime3D.ItemIndex >= 0 then
  begin
    NewIndex := comboTime3D.ItemIndex;
  end
  else if TryStrToFloat(comboTime3D.Text, ATime) then
  begin
    if comboTime3D.Items.Count = 0 then
    begin
      Exit;
    end;
    RealList := TRealList.Create;
    try
      RealList.Capacity:= comboTime3D.Items.Count;
      for Index := 0 to comboTime3D.Items.Count - 1 do
      begin
        RealList.Add(StrToFloat(comboTime3D.Items[Index]));
      end;
      NewIndex := RealList.IndexOf(ATime);
      if NewIndex < 0 then
      begin
        NewIndex := RealList.IndexOfClosest(ATime);
        if (RealList[NewIndex] > ATime) and (Direction = updUp) then
        begin
          Dec(NewIndex);
        end
        else if (RealList[NewIndex] < ATime) and (Direction = updDown) then
        begin
          Inc(NewIndex);
        end;
      end;
    finally
      RealList.Free;
    end;
  end;
  case Direction of
    updNone: ;
    updUp: Dec(NewIndex);
    updDown: Inc(NewIndex);
  end;
  if NewIndex < 0 then
  begin
    NewIndex := 0;
  end
  else if NewIndex >= comboTime3D.Items.Count then
  begin
    NewIndex := comboTime3D.Items.Count-1;
  end;
  if comboTime3D.ItemIndex <> NewIndex then
  begin
    comboTime3D.ItemIndex := NewIndex;
    SetData;
  end;
  udTime.ControlStyle := udTime.ControlStyle - [csCaptureMouse];
end;

procedure TframeColorGrid.UpdateLabelsAndLegend;
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
    UpdateLegend;
  finally
    FUpdatingLegend := False;
  end;
end;

procedure TframeColorGrid.UpdateTopFrontAndSideItems;
begin
  StoreDataSetsInLists;
  StoreBoundaryDataSetsInLists;
  StoreTimelistsInLists;

  FinalizeList(FTopItems);
  FinalizeList(FFrontItems);
  FinalizeList(FSideItems);
end;

procedure TframeColorGrid.virttreecomboDataSetsChange(Sender: TObject);
var
  AnObject: TObject;
begin
  inherited;
  ResetTreeText;
  RetrieveSelectedObject(AnObject);
  HandleSelectedObject(AnObject);
end;

procedure TframeColorGrid.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
//  inherited;
//  if FSelectingNode then
//  begin
//    Exit;
//  end;
  FSelectingNode := True;
  try
    inherited;
//    SetSelectedNode(Sender, Node);
  finally
    FSelectingNode := False;
  end;
end;

procedure TframeColorGrid.virttreecomboDataSetsTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

end.
