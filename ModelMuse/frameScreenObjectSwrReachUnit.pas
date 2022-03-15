unit frameScreenObjectSwrReachUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, StdCtrls,
  RbwEdit, frameScreenObjectNoParamUnit, ComCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, UndoItemsScreenObjects, JvPageListTreeView, RbwDataGrid4,
  frameGridUnit, Mask, JvExMask, JvSpin, ArgusDataEntry, ExtCtrls,
  JvToolEdit, GrayTabs;

type
  TSwrReachColumns = (srcStartTime, srcEndTime, srcGeomName, srcVerticalOffset,
    srcReachType, srcStage);
  TSwrConnectionColumns = (sccConnectionMethod, sccObjectName, sccReach);

  TframeScreenObjectSwrReach = class(TframeScreenObject)
    pgcSwr: TPageControl;
    tabSteady: TTabSheet;
    tabTransient: TTabSheet;
    frameSwr: TframeScreenObjectNoParam;
    lblDescription: TLabel;
    frameConnections: TframeGrid;
    pnlSteady: TPanel;
    rdeGroupNumber: TRbwDataEntry;
    lblGroupNumber: TLabel;
    cbGrouped: TCheckBox;
    cbMultilayer: TCheckBox;
    lblReachLength: TLabel;
    edReachLength: TRbwEdit;
    btnEditReachLength: TButton;
    comboRouteType: TJvImageComboBox;
    lblRouteType: TLabel;
    grpConnections: TGroupBox;
    lblObservationType: TLabel;
    cbbObservationTypes: TJvCheckedComboBox;
    procedure frameSwrdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameConnectionsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameConnectionsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameSwrdgModflowBoundarySelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure cbMultilayerClick(Sender: TObject);
    procedure cbGroupedClick(Sender: TObject);
    procedure frameConnectionsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
  private
    FSwrReachNode: TJvPageIndexNode;
    FClearedConnections: boolean;
    procedure FillListOfScreenObjects(ListOfScreenObjects: TList;
      List: TScreenObjectEditCollection);
    { Private declarations }
  public
    procedure InitializeFrame;
    procedure GetData(List: TScreenObjectEditCollection;
      ReachNode: TJvPageIndexNode);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectSwrReach: TframeScreenObjectSwrReach;

implementation

uses
  GoPhastTypes, ScreenObjectUnit, ModflowSwrReachUnit, frmGoPhastUnit,
  PhastModelUnit, ModflowPackageSelectionUnit, ModflowSwrObsUnit;

resourcestring
  StrGeometryIGEONUMR = 'Geometry (IGEONUMR)';
  StrVerticalOffsetGZS = 'Vertical Offset (GZSHIFT)';
  StrReachTypeISWRBND = 'Reach Type (ISWRBND)';
  StrStageSTAGE = 'Stage (STAGE)';
  StrObject = 'Object';
  StrConnectionMethod = 'Connection Method';
  StrReach = 'Reach';
  StrTheKinematicWaveA = 'The kinematic wave approximation is supported in M' +
  'ODFLOW-NWT but might not be supported in MODFLOW-OWHM.';

{$R *.dfm}

{ TframeScreenObjectSwrReach }

procedure TframeScreenObjectSwrReach.cbGroupedClick(Sender: TObject);
begin
  inherited;
  cbGrouped.AllowGrayed := False;
  rdeGroupNumber.Enabled := cbGrouped.Checked;
end;

procedure TframeScreenObjectSwrReach.cbMultilayerClick(Sender: TObject);
begin
  inherited;
  cbMultilayer.AllowGrayed := False;

end;

procedure TframeScreenObjectSwrReach.FillListOfScreenObjects(
  ListOfScreenObjects: TList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ModflowSwrReaches <> nil)
      and (ScreenObject.ModflowSwrReaches.Used) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TframeScreenObjectSwrReach.frameConnectionsGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if (ACol = Ord(sccObjectName))
    and (ARow >= frameConnections.Grid.FixedRows+PestRowOffset) then
  begin
    if (frameConnections.Grid.Cells[ACol, ARow] <> '')
      and (frameConnections.Grid.ItemIndex[ACol, ARow] < 0) then
    begin
      frameConnections.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TframeScreenObjectSwrReach.frameConnectionsGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  ItemIndex: integer;
begin
  inherited;
  if (ARow >= frameConnections.Grid.FixedRows+PestRowOffset)
    and (ACol in [Ord(sccReach), Ord(sccObjectName)]) then
  begin
    ItemIndex := frameConnections.Grid.ItemIndex[Ord(sccConnectionMethod), ARow];
    CanSelect := (ItemIndex >= 0);
    if CanSelect then
    begin
      case TSwrConnectionColumns(ACol) of
        sccObjectName:
          begin
            CanSelect := (TSwrConnectionMethod(ItemIndex) in [scmObject, scmSameCell])
          end;
        sccReach:
          begin
            CanSelect := (TSwrConnectionMethod(ItemIndex) = scmSpecifiedReach)
          end;
        else
          Assert(False);
      end;
    end;

  end;
end;

procedure TframeScreenObjectSwrReach.frameConnectionsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameConnections.Grid.Invalidate;
end;

procedure TframeScreenObjectSwrReach.frameSwrdgModflowBoundarySelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  frameSwr.rdgModflowBoundarySelectCell(Sender, ACol, ARow, CanSelect);
  if (ARow >= frameSwr.rdgModflowBoundary.FixedRows+PestRowOffset)
    and (ACol = Ord(srcStage)) then
  begin
    CanSelect := (ARow = 1)
      or (frameSwr.rdgModflowBoundary.ItemIndex[Ord(srcReachType), ARow] = 2)
      or (comboRouteType.ItemIndex in [0,1]);
  end;
end;

procedure TframeScreenObjectSwrReach.frameSwrdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameSwr.rdgModflowBoundarySetEditText(Sender, ACol, ARow, Value);
  if (ARow > 1) and (ACol = Ord(srcReachType)) then
  begin
    frameSwr.rdgModflowBoundary.Invalidate;
  end;
end;

procedure TframeScreenObjectSwrReach.GetData(List: TScreenObjectEditCollection;
  ReachNode: TJvPageIndexNode);
var
  ScreenObjectList: TList;
  AScreenObject: TScreenObject;
  Reaches: TSwrReachBoundary;
  FirstReaches: TSwrReachBoundary;
  TimeIndex: Integer;
  AnItem: TSwrTransientReachItem;
  Grid: TRbwDataGrid4;
  ObjectIndex: integer;
  PickList: TStringList;
  index: Integer;
  LocalModel: TCustomModel;
  ConnectionIndex: Integer;
  AConnection: TSwrConnectionItem;
  GeomIndex: Integer;
  ObsType: TSwrObsType;
begin
  FSwrReachNode := ReachNode;
  if FSwrReachNode = nil then
  begin
    Exit;
  end;

  pgcSwr.ActivePageIndex := 0;

  LocalModel := frmGoPhast.PhastModel;

  FClearedConnections := False;

  PickList := TStringList.Create;
  try
    for index := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[index];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if (AScreenObject.ModflowSwrReaches <> nil)
        and ((List.Count > 1)
        or (List.IndexOfScreenObjectName(AScreenObject.Name) < 0)) then
      begin
        PickList.Add(AScreenObject.Name);
      end;
    end;
    frameConnections.Grid.Columns[Ord(sccObjectName)].PickList := PickList;

    PickList.Clear;
    for GeomIndex := 0 to LocalModel.SwrReachGeometry.Count - 1 do
    begin
      PickList.Add(LocalModel.SwrReachGeometry[GeomIndex].Name);
    end;
    frameSwr.rdgModflowBoundary.Columns[Ord(srcGeomName)].PickList := PickList;
  finally
    PickList.Free;
  end;

//  cbbObservationTypes doesn't paint properly when it isn't enabled.

//  cbbObservationTypes.Enabled :=
//    frmGoPhast.PhastModel.ModflowPackages.SwrPackage.SaveObs
//    in [ssoSaveObs, ssoSaveObsAll];
  cbbObservationTypes.Visible :=
    frmGoPhast.PhastModel.ModflowPackages.SwrPackage.SaveObs
    in [ssoSaveObs, ssoSaveObsAll];
  comboRouteType.ItemIndex := 0;
  edReachLength.Text := TSwrReachBoundary.DefaultReachLengthFormula;
  ClearGrid(frameSwr.rdgModflowBoundary);

  frameSwr.rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
  frameSwr.rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  frameSwr.PestMethod[Ord(srcVerticalOffset)] :=
    TSwrReachBoundary.DefaultBoundaryMethod(SwrVerticalOffsetPosition);
  if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
  begin
    frameSwr.PestMethod[Ord(srcStage)] :=
      TSwrReachBoundary.DefaultBoundaryMethod(SwrStagePosition);
  end;

  ClearGrid(frameConnections.Grid);
  ScreenObjectList := TList.Create;
  try
    FillListOfScreenObjects(ScreenObjectList, List);
    if ScreenObjectList.Count = 0 then
    begin
      FSwrReachNode.StateIndex := 1;
    end
    else if ScreenObjectList.Count = List.Count then
    begin
      FSwrReachNode.StateIndex := 2;
    end
    else
    begin
      FSwrReachNode.StateIndex := 3;
    end;

    cbbObservationTypes.SetUnCheckedAll;
    cbbObservationTypes.Text := '';

    if ScreenObjectList.Count >= 1 then
    begin
      AScreenObject := ScreenObjectList[0];
      Reaches := AScreenObject.ModflowSwrReaches;
      FirstReaches := Reaches;
      comboRouteType.ItemIndex := Ord(Reaches.RouteType);
      edReachLength.Text := Reaches.ReachLengthFormula;
      cbMultilayer.Checked := Reaches.MultiLayer;
      cbGrouped.Checked := Reaches.Grouped;
      rdeGroupNumber.IntegerValue := Reaches.GroupNumber;

      frameSwr.PestModifier[Ord(srcVerticalOffset)] :=
        Reaches.PestVerticalOffsetFormula;
      frameSwr.PestMethod[Ord(srcVerticalOffset)] :=
        Reaches.PestVerticalOffsetMethod;

      if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
      begin
        frameSwr.PestModifier[Ord(srcStage)] :=
          Reaches.PestStageFormula;
        frameSwr.PestMethod[Ord(srcStage)] :=
          Reaches.PestStageMethod;
      end;

      frameSwr.seNumberOfTimes.AsInteger := Reaches.Values.Count;

      Grid := frameSwr.rdgModflowBoundary;
      for TimeIndex := 0 to Reaches.Values.Count - 1 do
      begin
        AnItem := Reaches.Values[TimeIndex] as TSwrTransientReachItem;
        Grid.RealValue[Ord(srcStartTime), TimeIndex+1+PestRowOffset] := AnItem.StartTime;
        Grid.RealValue[Ord(srcEndTime), TimeIndex+1+PestRowOffset] := AnItem.EndTime;
        Grid.Cells[Ord(srcGeomName), TimeIndex+1+PestRowOffset] := AnItem.GeometryName;
        Grid.Cells[Ord(srcVerticalOffset), TimeIndex+1+PestRowOffset] := AnItem.VerticalOffset;
        Grid.ItemIndex[Ord(srcReachType), TimeIndex+1+PestRowOffset] := Ord(AnItem.ReachType);
        if Ord(srcStage) < Grid.ColCount then
        begin
          Grid.Cells[Ord(srcStage), TimeIndex+1+PestRowOffset] := AnItem.Stage;
        end;
      end;

      frameConnections.seNumber.AsInteger := Reaches.Connections.Count;
      Grid := frameConnections.Grid;
      for ConnectionIndex := 0 to Reaches.Connections.Count - 1 do
      begin
        AConnection := Reaches.Connections[ConnectionIndex];
        Grid.Cells[Ord(sccObjectName), ConnectionIndex+1] := AConnection.ScreenObjectName;
        Grid.ItemIndex[Ord(sccConnectionMethod), ConnectionIndex+1] := Ord(AConnection.Method);
        Grid.IntegerValue[Ord(sccReach), ConnectionIndex+1] := AConnection.Reach;
      end;

      for ObsType := Low(TSwrObsType) to High(TSwrObsType) do
      begin
        if ObsType in Reaches.ObsTypes then
        begin
          cbbObservationTypes.Checked[Ord(ObsType)] := True;
        end
        else
        begin
          cbbObservationTypes.Checked[Ord(ObsType)] := False;
        end;

      end;

      for ObjectIndex := 1 to ScreenObjectList.Count - 1 do
      begin
        AScreenObject := ScreenObjectList[ObjectIndex];
        Reaches := AScreenObject.ModflowSwrReaches;

        if frameSwr.PestModifierAssigned[Ord(srcVerticalOffset)]
          and (frameSwr.PestModifier[Ord(srcVerticalOffset)] <>
          Reaches.PestVerticalOffsetFormula) then
        begin
          frameSwr.PestModifierAssigned[Ord(srcVerticalOffset)] := False
        end;
        if frameSwr.PestMethodAssigned[Ord(srcVerticalOffset)]
          and (frameSwr.PestMethod[Ord(srcVerticalOffset)] <>
          Reaches.PestVerticalOffsetMethod) then
        begin
          frameSwr.PestMethodAssigned[Ord(srcVerticalOffset)] := False
        end;

        if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
        begin
          if frameSwr.PestModifierAssigned[Ord(srcStage)]
            and (frameSwr.PestModifier[Ord(srcStage)] <>
            Reaches.PestStageFormula) then
          begin
            frameSwr.PestModifierAssigned[Ord(srcStage)] := False
          end;
          if frameSwr.PestMethodAssigned[Ord(srcStage)]
            and (frameSwr.PestMethod[Ord(srcStage)] <>
            Reaches.PestStageMethod) then
          begin
            frameSwr.PestMethodAssigned[Ord(srcStage)] := False
          end;
        end;

        if comboRouteType.ItemIndex <> Ord(Reaches.RouteType) then
        begin
          comboRouteType.ItemIndex := -1;
        end;
        if edReachLength.Text <> Reaches.ReachLengthFormula then
        begin
          edReachLength.Text := '';
        end;
        if cbMultilayer.Checked <> Reaches.MultiLayer then
        begin
          cbMultilayer.AllowGrayed := True;
          cbMultilayer.State := cbGrayed;
        end;
        if cbGrouped.Checked <> Reaches.Grouped then
        begin
          cbGrouped.AllowGrayed := True;
          cbGrouped.State := cbGrayed;
        end;
        if (rdeGroupNumber.Text <> '')
          and (rdeGroupNumber.IntegerValue <> Reaches.GroupNumber) then
        begin
          rdeGroupNumber.Text := '';
        end;

        for ObsType := Low(TSwrObsType) to High(TSwrObsType) do
        begin
          if cbbObservationTypes.Checked[Ord(ObsType)] <>
            (ObsType in Reaches.ObsTypes) then
          begin
            cbbObservationTypes.State[Ord(ObsType)] := cbGrayed;
          end;
        end;

        if not Reaches.Values.IsSame(FirstReaches.Values) then
        begin
          ClearGrid(frameSwr.rdgModflowBoundary);
        end;
        if not Reaches.Connections.IsSame(FirstReaches.Connections) then
        begin
          ClearGrid(frameConnections.Grid);
          FClearedConnections := True;
        end;
      end;
    end;

  finally
    ScreenObjectList.Free;
  end;
end;

procedure TframeScreenObjectSwrReach.InitializeFrame;
var
  LocalModel: TPhastModel;
  SwrPackage: TSwrPackage;
  Column: TRbwColumn4;
begin
  cbbObservationTypes.OrderedText := True;
  FSwrReachNode := nil;

  LocalModel := frmGoPhast.PhastModel;
  SwrPackage := LocalModel.ModflowPackages.SwrPackage;
  if SwrPackage.StageSpecification = smArray then
  begin
    frameSwr.rdgModflowBoundary.ColCount := 5;
  end
  else
  begin
    frameSwr.rdgModflowBoundary.ColCount := 6;
    Column := frameSwr.rdgModflowBoundary.Columns[
      frameSwr.rdgModflowBoundary.ColCount-1];
    Column.ButtonUsed := True;
    Column.WordWrapCaptions := True;
    Column.AutoAdjustColWidths := True;
    Column.AutoAdjustRowHeights := True;
    Column.Format := rcf4String;
    Column.ButtonCaption := StrFormulaButtonCaption;
    Column.ButtonWidth := 35;
  end;

  frameSwr.rdgModflowBoundary.Cells[Ord(srcStartTime), 0] := StrStartingTime;
  frameSwr.rdgModflowBoundary.Cells[Ord(srcEndTime), 0] := StrEndingTime;
  frameSwr.rdgModflowBoundary.Cells[Ord(srcGeomName), 0] := StrGeometryIGEONUMR;
  frameSwr.rdgModflowBoundary.Cells[Ord(srcVerticalOffset), 0] := StrVerticalOffsetGZS;
  frameSwr.rdgModflowBoundary.Cells[Ord(srcReachType), 0] := StrReachTypeISWRBND;
  if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
  begin
    frameSwr.rdgModflowBoundary.Cells[Ord(srcStage), 0] := StrStageSTAGE;
  end;

  frameConnections.Grid.Cells[Ord(sccObjectName), 0] := StrObject;
  frameConnections.Grid.Cells[Ord(sccConnectionMethod), 0] := StrConnectionMethod;
  frameConnections.Grid.Cells[Ord(sccReach), 0] := StrReach;

  if Assigned(frameSwr.seNumberOfTimes.OnChange) then
  begin
    frameSwr.seNumberOfTimes.OnChange(frameSwr.seNumberOfTimes);
  end;

  frameSwr.GetStartTimes(Ord(srcStartTime));
  frameSwr.GetEndTimes(Ord(srcEndTime));
  comboRouteType.ItemIndex := -1;
  cbMultilayer.Checked := False;
  cbGrouped.Checked := False;
  edReachLength.Text := '';
  rdeGroupNumber.Text := '0';
  ClearGrid(frameSwr.rdgModflowBoundary);
  ClearGrid(frameConnections.Grid);
  frameConnections.seNumber.AsInteger := 0;
end;

procedure TframeScreenObjectSwrReach.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  StartTime: double;
  EndTime: double;
  Formula: string;
  Values: TSwrReachCollection;
  AnItem: TSwrTransientReachItem;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Reaches: TSwrReachBoundary;
  NewConnections: TSwrConnections;
  ObjectName: string;
  ItemIndex: Integer;
  Method: TSwrConnectionMethod;
  Reach: Integer;
  ANewConnection: TSwrConnectionItem;
  CreateNewItem: Boolean;
  GeomName: string;
  StageFormula: string;
  ObsType: TSwrObsType;
  ObsTypes: TSwrObsTypes;
begin
  Values := nil;
  if ClearAll then
  begin
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      AScreenObject := List[ScreenObjectIndex].ScreenObject;
      AScreenObject.ModflowSwrReaches := nil;
    end;
  end
  else
  begin
    StageFormula := '';
    Values := TSwrReachCollection.Create(nil, nil, nil);
    NewConnections := TSwrConnections.Create(nil);
    try
      Grid := frameSwr.rdgModflowBoundary;
      for RowIndex := 1 + PestRowOffset to
        frameSwr.seNumberOfTimes.AsInteger + PestRowOffset do
      begin
        if TryStrToFloat(Grid.Cells[Ord(srcStartTime), RowIndex], StartTime)
          and TryStrToFloat(Grid.Cells[Ord(srcEndTime), RowIndex], EndTime) then
        begin
          Formula := Grid.Cells[Ord(srcVerticalOffset), RowIndex];
          GeomName := Grid.Cells[Ord(srcGeomName), RowIndex];
          ItemIndex := Grid.ItemIndex[Ord(srcReachType), RowIndex];
          if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
          begin
            StageFormula := Grid.Cells[Ord(srcStage), RowIndex];
            if StageFormula = '' then
            begin
              StageFormula := '0';
            end;
          end;
          if (Formula <> '') and (GeomName <> '') and (ItemIndex >= 0) then
          begin
            AnItem := Values.Add as TSwrTransientReachItem;
            AnItem.StartTime := StartTime;
            AnItem.EndTime := EndTime;
            AnItem.VerticalOffset := Formula;
            AnItem.GeometryName := GeomName;
            AnItem.ReachType := TSwrReachType(ItemIndex);
            if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
            begin
              AnItem.Stage := StageFormula;
            end;
          end;
        end;
      end;
      if Values.Count = 0 then
      begin
        FreeAndNil(Values);
      end;

      Grid := frameConnections.Grid;
      for RowIndex := 1 to frameConnections.seNumber.AsInteger do
      begin
        ItemIndex := Grid.ItemIndex[Ord(sccConnectionMethod), RowIndex];
        if (ItemIndex >= 0) then
        begin
          Method := TSwrConnectionMethod(ItemIndex);
          CreateNewItem := False;
          Reach := 0;
          ObjectName := '';
          case Method of
            scmObject, scmSameCell:
              begin
                ObjectName := Grid.Cells[Ord(sccObjectName), RowIndex];
                CreateNewItem := ObjectName <> '';
              end;
            scmSpecifiedReach:
              begin
                if TryStrToInt(Grid.Cells[Ord(sccReach), RowIndex], Reach) then
                begin
                  CreateNewItem := True;
                end;
              end
            else
              Assert(False);
          end;
          if CreateNewItem then
          begin
            ANewConnection := NewConnections.Add;
            ANewConnection.ScreenObjectName := ObjectName;
            ANewConnection.Method := Method;
            ANewConnection.Reach := Reach
          end;
        end;
      end;

      if (comboRouteType.ItemIndex = 3)
        and (frmGoPhast.PhastModel.ModelSelection <> msModflowNWT) then
      begin
        Beep;
        MessageDlg(StrTheKinematicWaveA, mtWarning, [mbOK], 0);
      end;

      for ScreenObjectIndex := 0 to List.Count - 1 do
      begin
        AScreenObject := List[ScreenObjectIndex].ScreenObject;
        if SetAll then
        begin
          AScreenObject.CreateSwrReachesBoundary;
        end;
        Reaches := AScreenObject.ModflowSwrReaches;
        if Reaches <> nil then
        begin
          if frameSwr.PestModifierAssigned[Ord(srcVerticalOffset)] then
          begin
            Reaches.PestVerticalOffsetFormula :=
              frameSwr.PestModifier[Ord(srcVerticalOffset)];
          end;

          if frameSwr.PestMethodAssigned[Ord(srcVerticalOffset)] then
          begin
            Reaches.PestVerticalOffsetMethod :=
              frameSwr.PestMethod[Ord(srcVerticalOffset)];
          end;

          if Ord(srcStage) < frameSwr.rdgModflowBoundary.ColCount then
          begin
            if frameSwr.PestModifierAssigned[Ord(srcStage)] then
            begin
              Reaches.PestStageFormula := frameSwr.PestModifier[Ord(srcStage)];
            end;
            if frameSwr.PestMethodAssigned[Ord(srcStage)] then
            begin
              Reaches.PestStageMethod := frameSwr.PestMethod[Ord(srcStage)];
            end;
          end;

          if comboRouteType.ItemIndex >= 0 then
          begin
            Reaches.RouteType := TSwrRouteType(comboRouteType.ItemIndex);
          end;
          if edReachLength.Text <> '' then
          begin
            Reaches.ReachLengthFormula := edReachLength.Text;
          end;
          if cbMultilayer.State <> cbGrayed then
          begin
            Reaches.MultiLayer := cbMultilayer.Checked;
          end;
          if cbGrouped.State <> cbGrayed then
          begin
            Reaches.Grouped := cbGrouped.Checked;
          end;
          if rdeGroupNumber.Text <> '' then
          begin
            Reaches.GroupNumber := rdeGroupNumber.IntegerValue;
          end;
          if Values <> nil then
          begin
            Reaches.Values := Values;
          end;
          if not FClearedConnections or (NewConnections.Count > 0) then
          begin
            Reaches.Connections := NewConnections;
          end;

          ObsTypes := Reaches.ObsTypes;
          for ObsType := Low(TSwrObsType) to High(TSwrObsType) do
          begin
            if cbbObservationTypes.State[Ord(ObsType)] = cbGrayed then
            begin
              Continue;
            end;
            if cbbObservationTypes.Checked[Ord(ObsType)] then
            begin
              Include(ObsTypes, ObsType);
            end
            else
            begin
              Exclude(ObsTypes, ObsType);
            end;
          end;
          Reaches.ObsTypes := ObsTypes;
        end;
      end;
    finally
      Values.Free;
      NewConnections.Free
    end;
  end;
end;

end.
