unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ScreenObjectUnit,
  DataSetUnit, VirtualTrees, FastGEO, GoPhastTypes, SsButtonEd,
  RbwStringTreeCombo, Grids, RbwDataGrid4, ExtCtrls,
  RbwRollupPanel, Vcl.Mask;

type
  TPathLineColumn = (plcLabel, plcFirst, plcLast, plcClosest);
  TPathlineRow = (plrLabel, plrNumber, plrX, plrY, plrZ, plrXPrime, plrYPrime, plrLocalZ,
    plrTime, plrColumn, plrRow, plrLayer, plrTimeStep, plrGroup);
  TEndPointColumn = (epcLabel, epcStart, epcEnd);
  TEndPointRow = (eprLabel, eprNumber, eprZone, eprColumn, eprRow, eprLayer, eprX, eprY,
    eprZ, eprXPrime, eprYPrime, eprLocalZ, eprTimeStep, eprParticleGroup);
  TGncColumns = (gcContaining, gcLinked, gcID, gcWeight);


  TfrmGridValue = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnClose: TBitBtn;
    pnlTabs: TPanel;
    comboModel: TComboBox;
    lblRow: TStaticText;
    lblColumn: TStaticText;
    memoExplanation: TMemo;
    lblSelectedObject: TStaticText;
    lblVertex: TStaticText;
    lblSection: TStaticText;
    cbShowThirdDValues: TCheckBox;
    lblHigher3rdDimensionCoordinate: TStaticText;
    lblLower3rdDimensionCoordinate: TStaticText;
    virttreecomboDataSets: TRbwStringTreeCombo;
    lblSelectValue: TLabel;
    edSelectValue: TEdit;
    lblSelectExplanation: TLabel;
    memoSelectExplanation: TMemo;
    btnUpdate: TButton;
    rdgPathline: TRbwDataGrid4;
    pnlEndPoints: TPanel;
    lbledtReleaseTime: TLabeledEdit;
    lbledtTerminationCode: TLabeledEdit;
    lbledtTrackingTime: TLabeledEdit;
    rdgEndPoints: TRbwDataGrid4;
    splAllDataSets: TSplitter;
    pnlPathLength: TPanel;
    lblLength: TLabel;
    edLength: TEdit;
    rdgSwrReaches: TRbwDataGrid4;
    pnlSwrReaches: TPanel;
    lblSwrReaches: TLabel;
    pnlSwrStructures: TPanel;
    lblSwrStructures: TLabel;
    rdgSwrStructures: TRbwDataGrid4;
    rrlcurrentData: TRbwRollupPanel;
    lblModel: TLabel;
    lblLayer: TStaticText;
    lblLayerHeight: TStaticText;
    lblRowWidth: TStaticText;
    lblColumnWidth: TStaticText;
    lblDataSet: TLabel;
    lblCellValue: TLabel;
    edCellValue: TEdit;
    lblExplanation: TLabel;
    rrlAllDataSets: TRbwRollupPanel;
    rrlPathline: TRbwRollupPanel;
    rrlEndPoint: TRbwRollupPanel;
    rrlSWR: TRbwRollupPanel;
    pnlSWR: TPanel;
    pnlPathline: TPanel;
    pnlEndpoint: TPanel;
    rrlGNC: TRbwRollupPanel;
    rdgGhostNode: TRbwDataGrid4;
    rrlXt3d: TRbwRollupPanel;
    lblAngle1: TLabel;
    lblAngle2: TLabel;
    lblAngle3: TLabel;
    lblKz: TLabel;
    lblKy: TLabel;
    lblKx: TLabel;
    splPathline: TSplitter;
    splEndPoint: TSplitter;
    splSWR: TSplitter;
    splGNC: TSplitter;
    splXt3d: TSplitter;
    btnUpdateXT3D: TButton;
    imgAngle1: TImage;
    imgAngle2: TImage;
    imgAngle3: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure edCellValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure memoExplanationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure virttreecomboDataSetsTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure virttreecomboDataSetsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure jvrltEndPointCollapse(Sender: TObject);
    procedure jvrltEndPointExpand(Sender: TObject);
    procedure jvrltPathlineCollapse(Sender: TObject);
    procedure jvrltPathlineExpand(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnUpdateXT3DClick(Sender: TObject);
  private
    FSelectedScreenObject: TScreenObject;
    FColumn: Integer;
    FRow: Integer;
    FLayer: Integer;
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;
    FViewDirection: TViewDirection;
    FPriorLocation: TPoint2D;
    FPriorEndPointLocation: TPoint2D;
    FModel: TBaseModel;
    procedure DisplayEndPointData(const Location: TPoint2D);
    procedure InitializeEndpointGrid;
    procedure InitializeGncGrid;
    procedure DisplayGnc;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    procedure UpdatedSelectedObject;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure UpdateScreenObjectInfo(const Column, Row, Layer: Integer;
      Location: TPoint2D; Model: TBaseModel);
    procedure UpdateSelectedData(Layer, Row, Column: integer);
    procedure GetSelectedDataArray(var OtherDataSet: TDataArray);
    procedure InitializePathlineGrid;
    procedure DisplayPathlineData(const Location: TPoint2D);
    function DiscretizationDefined: Boolean;
    procedure GetWidthForModpathPanels(var AvailableWidth: Integer);
    procedure DisplaySwrData(Layer, Row, Column: integer);
    procedure InitializeSwrGrids;
    procedure UpdateXt3d;
    procedure ArrangeSplitters;
    { Private declarations }
  public
    procedure UpdateValue(const Layer, Row, Column: integer;
      const DataSetName, CellValue: string; Explanation: string;
      const Location: TPoint2D; ViewDirection: TViewDirection;
      EvaluatedAt: TEvaluatedAt);
    procedure UpdateDataSets;
    { Public declarations }
  end;

procedure UpdateFrmGridValue;

var
  frmGridValue: TfrmGridValue;

implementation

uses AbstractGridUnit, frmGoPhastUnit,
  GIS_Functions, RbwParser, Contnrs, ClassificationUnit,
  PhastModelUnit, PathlineReader, QuadtreeClass, ZoomBox2, InteractiveTools,
  SutraMeshUnit, DisplaySettingsUnit,
  System.Generics.Collections, ModflowSwrStructureUnit,
  ModflowIrregularMeshUnit, ModflowGncUnit;

resourcestring
  StrSelectedObject = 'Selected object';
  StrHigher3rdDimension = 'Higher 3rd dimension coordinate';
  StrLower3rdDimension = 'Lower 3rd dimension coordinate';
  StrNotAssigned = ': (not assigned)';
  StrHigher = 'Higher ';
  StrLower = 'Lower ';
  StrPriorVertex = 'Prior vertex number %s';
  StrParentModel = 'Parent model';
  StrDataSetValuesNeed = 'Data set values need to be updated.';
  StrTheMouseIsNotOve = 'The mouse is not over the grid.';
  StrTheDataSetDoesNo = 'The data set does not have a value at the current l' +
  'ocation.';
  StrLayerD = 'Layer: %d';
  StrRowD = 'Row: %d';
  StrColumn = 'Column: %d';
  StrLayerHeightG = 'Layer height: %g';
  StrRowWidthG = 'Row width: %g';
  StrColumnWidthG = 'Column width: %g';
  StrLayerHeight = 'Layer height:';
  StrRowWidth = 'Row width:';
  StrColumnWidth = 'Column width:';
  StrLayer = 'Layer';
  StrRow = 'Row';
  StrColumn1 = 'Column';
  Str0s1s2gOn = '%0:s %1:s: %2:g on %3:s %4:d';
  StrZcoordinate = 'Z-coordinate';
  StrYcoordinate = 'Y-coordinate';
  StrXcoordinate = 'X-coordinate';
  StrSection = 'Section = ?';
  StrSectionD = 'Section %d';
  StrSelectedObjectMu = 'Selected object: (multiple objects)';
  StrSection1 = 'Section';
  StrSelectedObjectNo = 'Selected object: (none)';
  StrFirst = 'First';
  StrLast = 'Last';
  StrClosest = 'Closest';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrXPrime = 'X''';
  StrYPrime = 'Y''';
  StrLocalZ = 'Local Z';
  StrTime = 'Time';
  StrTimeStep = 'Time step';
  StrStart = 'Start';
  StrEnd = 'End';
  StrZone = 'Zone';
  StrGroup = 'Group';
  StrNumber = 'Number';
  StrElementD = 'Element: %d';
  StrNodeD = 'Node: %d';
  StrYouMustColorTheG = 'You must color the grid with a data set evaluated a' +
  't %s to see this data set''s values.';
  StrElements = 'elements';
  StrNodes = 'nodes';
  StrICONd = 'ICON[%d]';
  StrReach = 'Reach';
  StrObject = 'Object';
  StrStructure = 'Structure';
  StrConnectedReach = 'Connected Reach';
  StrNoModelSelectionH = 'No model selection has been made.';

{$R *.dfm}

procedure UpdateFrmGridValue;
begin
  if (frmGridValue <> nil) and frmGridValue.Visible then
  begin
    frmGridValue.UpdateDataSets;
  end;
end;

{ TfrmGridValue }

procedure TfrmGridValue.ArrangeSplitters;
var
  CurrentLeft: Integer;
  procedure ArrangeASplitter(Spliter, RollUp: TControl);
  begin
    Spliter.Visible := RollUp.Visible;
    if Spliter.Visible then
    begin
      Spliter.Left := CurrentLeft;
      CurrentLeft := CurrentLeft + Spliter.Width;
      RollUp.Left := CurrentLeft;
      CurrentLeft := CurrentLeft + RollUp.Width;
    end;
  end;
begin
  CurrentLeft := rrlAllDataSets.Left + rrlAllDataSets.Width;
  ArrangeASplitter(splPathline, rrlPathline);
  ArrangeASplitter(splEndPoint, rrlEndPoint);
  ArrangeASplitter(splSWR, rrlSWR);
  ArrangeASplitter(splGNC, rrlGNC);
  ArrangeASplitter(splXt3d, rrlXt3d);

end;

procedure TfrmGridValue.btnUpdateClick(Sender: TObject);
var
  OtherDataSet: TDataArray;
begin
  inherited;
  GetSelectedDataArray(OtherDataSet);
  if (OtherDataSet <> nil) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    OtherDataSet.Initialize;
    frmGoPhast.PhastModel.DataArrayManager.AddDataSetToCache(OtherDataSet);
  end;
  UpdateSelectedData(FLayer, FRow, FColumn);
end;

procedure TfrmGridValue.btnUpdateXT3DClick(Sender: TObject);
var
  DataArrayManager: TDataArrayManager;
  procedure UpdateDataArray(const AName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := DataArrayManager.GetDataSetByName(AName);
    if DataArray <> nil then
    begin
      DataArray.Initialize;
    end;
  end;
begin
  inherited;
  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
//  Npf := frmGoPhast.PhastModel.ModflowPackages.NpfPackage;
//  if not Npf.UseXT3D then
//  begin
//    Exit;
//  end;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  UpdateDataArray(KXT3DAngle1);
  UpdateDataArray(KXT3DAngle2);
  UpdateDataArray(KXT3DAngle3);
  UpdateDataArray(rsKx);
  UpdateDataArray(rsKy);
  UpdateDataArray(rsKz);

  UpdateXt3d;
end;

procedure TfrmGridValue.comboModelChange(Sender: TObject);
var
  LocalModel: TCustomModel;
begin
  inherited;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  if LocalModel <> nil then
  begin
    frmGoPhast.PhastModel.SelectedModel := LocalModel;
    UpdateCurrentModel(LocalModel);
  end;
end;

procedure TfrmGridValue.edCellValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    edCellValue.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmGridValue := nil;
  Action := caFree;
end;

procedure TfrmGridValue.InitializeGncGrid;
begin
  rdgGhostNode.Cells[Ord(gcContaining), 0] := 'Containing cell ID';
  rdgGhostNode.Cells[Ord(gcLinked), 0] := 'Linked cell ID';
  rdgGhostNode.Cells[Ord(gcID), 0] := 'Cell ID';
  rdgGhostNode.Cells[Ord(gcWeight), 0] := 'Weight';
end;

procedure TfrmGridValue.FormCreate(Sender: TObject);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  AdjustFormPosition(dpLeft);
  FDataSetDummyObjects := TObjectList.Create;

  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowNWT, msModflowCfp, msModflow2015, msSutra22,
       msSutra30, msSutra40, msFootPrint:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel)
      end;
    msModflowLGR, msModflowLGR2, msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
    :
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel);
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          if ChildModel <> nil then
          begin
            comboModel.Items.AddObject(ChildModel.ModelName, ChildModel);
          end;
        end;
      end;
    msUndefined:
      begin
        Beep;
        MessageDlg(StrNoModelSelectionH, mtError, [mbOK], 0);
        Close;
        Exit;
      end
    else
      Assert(False);
  end;
  comboModel.ItemIndex := comboModel.Items.IndexOfObject(
    frmGoPhast.PhastModel.SelectedModel);

  InitializePathlineGrid;
  InitializeEndpointGrid;
  InitializeSwrGrids;
  InitializeGncGrid;
end;

procedure TfrmGridValue.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataSetDummyObjects.Free;
end;

procedure TfrmGridValue.FormResize(Sender: TObject);
begin
  inherited;
    rdgPathline.Width := rrlPathline.ClientWidth;
end;

procedure TfrmGridValue.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDataSets;
  // virttreecomboDataSets thows an exception if it is not on the
  // active page when TfrmGridValue is created.
//  Assert(pcDataDisplay.ActivePageIndex = 1);
  rrlcurrentData.Collapsed := False;
  rrlAllDataSets.Collapsed := True;
  rrlPathline.Collapsed := True;
  rrlEndPoint.Collapsed := True;
  rrlGNC.Collapsed := True;
  rrlSWR.Collapsed := True;
  rrlXt3d.Collapsed := True;

//  pcDataDisplay.ActivePageIndex := 0;
end;

procedure TfrmGridValue.memoExplanationKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    memoExplanation.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.UpdateSelectedData(Layer, Row, Column: integer);
var
  OtherDataSet: TDataArray;
begin
  btnUpdate.Enabled := False;
  GetSelectedDataArray(OtherDataSet);
  if OtherDataSet = nil then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := '';
    Exit;
  end;

  if not OtherDataSet.UpToDate then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := StrDataSetValuesNeed;
    btnUpdate.Enabled := True;
    Exit;
  end;

  if (Layer < 0) or (Row < 0) or (Column < 0) then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := StrTheMouseIsNotOve;
    Exit;
  end;


  case OtherDataSet.Orientation of
    dsoTop: Layer := 0;
    dsoFront: Row := 0;
    dsoSide: Column := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;

  if GlobalEvaluatedAt <> OtherDataSet.EvaluatedAt then
  begin
    edSelectValue.Text := '';
    case GlobalEvaluatedAt of
      eaBlocks:
        begin
          memoSelectExplanation.Text := Format(StrYouMustColorTheG, [StrElements]);
        end;
      eaNodes:
        begin
          memoSelectExplanation.Text := Format(StrYouMustColorTheG, [StrNodes]);
        end;
      else Assert(False);
    end;

    Exit;
  end;

  if Layer > OtherDataSet.LayerCount then
  begin
    Layer := OtherDataSet.LayerCount -1;
  end;
  if Row > OtherDataSet.RowCount then
  begin
    Row := OtherDataSet.RowCount -1;
  end;
  if Column > OtherDataSet.ColumnCount then
  begin
    Column := OtherDataSet.ColumnCount -1;
  end;

  if not OtherDataSet.IsValue[Layer, Row, Column] then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := StrTheDataSetDoesNo;
    Exit;
  end;

  case OtherDataSet.DataType of
    rdtDouble: edSelectValue.Text := FloatToStr(
      OtherDataSet.RealData[Layer, Row, Column]);
    rdtInteger: edSelectValue.Text := IntToStr(
      OtherDataSet.IntegerData[Layer, Row, Column]);
    rdtBoolean:
      begin
        if OtherDataSet.BooleanData[Layer, Row, Column] then
        begin
          edSelectValue.Text := 'True';
        end
        else
        begin
          edSelectValue.Text := 'False';
        end;
      end;
    rdtString: edSelectValue.Text :=
      OtherDataSet.StringData[Layer, Row, Column];
  end;
  memoSelectExplanation.Text :=
    OtherDataSet.Annotation[Layer, Row, Column];
end;

procedure TfrmGridValue.DisplayGnc;
var
  Model: TCustomModel;
  RowIndex: Integer;
  ColIndex: Integer;
  ItemIndex: Integer;
  CellWeight: TWeightedCellId;
  GhostNodeArray: TGhostNodeArray;
  GhostNode: TGhostNode;
  RowCount: Integer;
  GhostNodeIndex: Integer;
begin
  Model := FModel as TCustomModel;
  if not Model.DisvUsed then
  begin
    rrlGNC.Visible := False;
    Exit;
  end;
  if not Model.ModflowPackages.GncPackage.IsSelected then
  begin
    rrlGNC.Visible := False;
    Exit;
  end;
  rrlGNC.Visible := True;

  GhostNodeArray := Model.DisvGrid.TwoDGrid.GhostNodes.GhostNodesByCell[FColumn];
  if GhostNodeArray = nil then
  begin
    rdgGhostNode.BeginUpdate;
    try
      for RowIndex := 1 to rdgGhostNode.RowCount - 1 do
      begin
        for ColIndex := 0 to rdgGhostNode.ColCount - 1 do
        begin
          rdgGhostNode.Cells[ColIndex,RowIndex] := '';
        end;
      end;
    finally
      rdgGhostNode.EndUpdate;
      rdgGhostNode.Repaint;
    end;
  end
  else
  begin

//    rdgGhostNode.RowCount := Length(GhostNodeArray) + 1;
    RowCount := 1;
    for GhostNodeIndex := 0 to Length(GhostNodeArray) - 1 do
    begin
      GhostNode := GhostNodeArray[GhostNodeIndex];
      if GhostNode <> nil then
      begin
        RowCount := RowCount + GhostNode.CellWeights.Count;
      end;
    end;
    rdgGhostNode.RowCount := RowCount;

    rdgGhostNode.BeginUpdate;
    try

      for RowIndex := 1 to rdgGhostNode.RowCount - 1 do
      begin
        for ColIndex := 0 to rdgGhostNode.ColCount - 1 do
        begin
          rdgGhostNode.Cells[ColIndex,RowIndex] := '';
        end;
      end;

      RowIndex := 1;
      for GhostNodeIndex := 0 to Length(GhostNodeArray) - 1 do
      begin
        GhostNode := GhostNodeArray[GhostNodeIndex];
        if GhostNode = nil then
        begin
          Continue;
        end;
		
        rdgGhostNode.Cells[Ord(gcContaining),RowIndex]
          := (GhostNode.ContainingCell.Cell+1).ToString;
        rdgGhostNode.Cells[Ord(gcLinked),RowIndex]
          := (GhostNode.LinkedCell.Cell+1).ToString;

        for ItemIndex := 0 to GhostNode.CellWeights.Count - 1 do
        begin
          CellWeight := GhostNode.CellWeights[ItemIndex];
          rdgGhostNode.Cells[Ord(gcID),RowIndex]
            := (CellWeight.Cell+1).ToString;
          rdgGhostNode.Cells[Ord(gcWeight),RowIndex]
            := CellWeight.Weight.ToString;
          Inc(RowIndex);
        end;
      end;
    finally
      rdgGhostNode.EndUpdate;
      rdgGhostNode.Repaint;
    end;
  end;
end;

procedure TfrmGridValue.UpdateValue(const Layer, Row, Column: integer;
  const DataSetName, CellValue: string; Explanation: string;
  const Location: TPoint2D; ViewDirection: TViewDirection;
  EvaluatedAt: TEvaluatedAt);
var
  DataArray: TDataArray;

  ColumnWidth: Double;
  RowWidth: Double;
  LayerHeight: Double;
  Mesh: TSutraMesh3D;
  AnElement3D: TSutraElement3D;
  ANode3D: TSutraNode3D;
  AnElement2D: TSutraElement2D;
  ANode2D: TSutraNode2D;
//  DiscretizationDefined: Boolean;
begin

  DisplaySwrData(Layer, Row, Column);
  FModel := frmGoPhast.PhastModel.SelectedModel;
  FViewDirection := ViewDirection;
  FColumn := Column;
  FRow := Row;
  FLayer := Layer;
  lblDataSet.Caption := DataSetName;
  edCellValue.Text := CellValue;
  if (Explanation <> StrNoValueAssigned)
    and (Pos(StrNoValueAssigned, Explanation) > 0) then
  begin
    Explanation := StringReplace(Explanation, StrNoValueAssigned, '', []);
  end;
  memoExplanation.Text := Explanation;
  if DiscretizationDefined then
  begin
    if DataSetName <> '' then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.
        GetDataSetByName(DataSetName)
    end
    else
    begin
      DataArray := nil;
    end;
    if DataArray <> nil then
//    begin
//      EvaluatedAt := eaBlocks;
//    end
//    else
    begin
      EvaluatedAt := DataArray.EvaluatedAt;
    end;

    if not (FModel.ModelSelection in SutraSelection) then
    begin
      lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
      lblRow.Caption := Format(StrRowD, [(Row+1)]);
      lblColumn.Caption := Format(StrColumn, [(Column+1)]);
      Mesh := nil;
    end
    else
    begin
      lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
      lblColumn.Caption := '';
      Mesh := (FModel as TPhastModel).SutraMesh;
      if Mesh.MeshType = mt3D then
      begin
        if Mesh.LayerCount > 0 then
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                AnElement3D := Mesh.ElementArray[Layer,Column];
                lblRow.Caption := Format(StrElementD, [AnElement3D.ElementNumber+1]);
              end;
            eaNodes:
              begin
                ANode3D := Mesh.NodeArray[Layer,Column];
                lblRow.Caption := Format(StrNodeD, [ANode3D.Number+1]);
              end;
            else Assert(False);
          end;
        end;
      end
      else
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              AnElement2D := Mesh.Mesh2D.Elements[Column];
              lblRow.Caption := Format(StrElementD, [AnElement2D.ElementNumber+1]);
            end;
          eaNodes:
            begin
              ANode2D := Mesh.Mesh2D.Nodes[Column];
              lblRow.Caption := Format(StrNodeD, [ANode2D.Number+1]);
            end;
          else Assert(False);
        end;
      end;
    end;
    GlobalEvaluatedAt := EvaluatedAt;
    ColumnWidth := GetColumnWidth(Column);
    RowWidth := GetRowWidth(Row);

    if not (FModel.ModelSelection in SutraSelection) or (Mesh.LayerCount > 0) then
    begin
      LayerHeight := GetLayerHeight(Column, Row, Layer);
      lblLayerHeight.Caption := Format(StrLayerHeightG, [LayerHeight]);
    end;

    if not (FModel.ModelSelection in SutraSelection) then
    begin
      lblRowWidth.Caption := Format(StrRowWidthG, [RowWidth]);
      lblColumnWidth.Caption := Format(StrColumnWidthG, [ColumnWidth]);
    end
    else
    begin
      lblRowWidth.Caption := '';
      lblColumnWidth.Caption := '';
    end;
  end
  else
  begin
    lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
    lblRow.Caption := Format(StrRowD, [(Row+1)]);
    lblColumn.Caption := Format(StrColumn, [(Column+1)]);
    lblLayerHeight.Caption := StrLayerHeight;
    if not (FModel.ModelSelection in SutraSelection) then
    begin
      lblRowWidth.Caption := StrRowWidth;
      lblColumnWidth.Caption := StrColumnWidth;
    end
    else
    begin
      lblRowWidth.Caption := '';
      lblColumnWidth.Caption := '';
    end;
  end;

  UpdateScreenObjectInfo(Column, Row, Layer, Location, FModel);
  UpdateSelectedData(Layer, Row, Column);
  DisplayPathlineData(Location);
  DisplayEndPointData(Location);
  DisplayGnc;
  UpdateXt3d;
  ArrangeSplitters;
end;

procedure TfrmGridValue.UpdateXt3d;
var
  LocalModel: TCustomModel;
  function NewCaption(DataArrayName: string): string;
  var
    DataArray: TDataArray;
  begin
    DataArray := LocalModel.DataArrayManager.GetDataSetbyName(DataArrayName);
    if DataArray = nil then
    begin
      result := DataArrayName;
    end
    else if DataArray.UpToDate then
    begin
      result := DataArrayName + ': '
        + DataArray.RealData[FLayer, FRow, FColumn].ToString;
    end
    else
    begin
      result := DataArrayName + ': ?';
    end;
  end;
  procedure PlotAngle(const DataArrayName: string; ImageBox: TImage);
  var
    BitMap: TBitmap;
    ARect: TRect;
    DataArray: TDataArray;
    Angle: Double;
    APoint: TPoint;
    DeltaX: Int64;
    DeltaY: Int64;

  begin
    BitMap := TBitmap.Create;
    try
      BitMap.Width := ImageBox.Width;
      BitMap.Height := ImageBox.Height;
      BitMap.Canvas.Pen.Color := clBlack;
      BitMap.Canvas.Pen.Style := psSolid;
      BitMap.Canvas.Brush.Color := clWhite;
      BitMap.Canvas.Brush.Style := bsSolid;
      ARect.Left := 0;
      ARect.Top := 0;
      ARect.Right := BitMap.Width;
      ARect.Bottom := BitMap.Height;
      BitMap.Canvas.FillRect(ARect);
      DataArray := LocalModel.DataArrayManager.GetDataSetbyName(DataArrayName);
      if (DataArray <> nil) and DataArray.UpToDate then
      begin
        BitMap.Canvas.Brush.Style := bsClear;
        BitMap.Canvas.Ellipse(ARect);

        Angle := DataArray.RealData[FLayer, FRow, FColumn];
        Angle := Angle*Pi/180;
        DeltaX := Round(Cos(Angle)*BitMap.Width/2);
        DeltaY := Round(Sin(Angle)*BitMap.Height/2);

        APoint.X := BitMap.Width div 2;
        APoint.Y := BitMap.Height div 2;

        BitMap.Canvas.MoveTo(APoint.X, APoint.Y);
        BitMap.Canvas.LineTo(APoint.X+DeltaX, APoint.Y-DeltaY);
      end;

      ImageBox.Picture.Assign(BitMap);
    finally
      BitMap.Free;
    end;
  end;
begin
  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    rrlXt3d.Visible := False;
    Exit;
  end;
  LocalModel := frmGoPhast.PhastModel;
//  if not frmGoPhast.PhastModel.ModflowPackages.NpfPackage.UseXT3D then
//  begin
//    rrlXt3d.Visible := False;
//    Exit;
//  end;

  rrlXt3d.Visible := True;
  if (FLayer >= 0) and (FRow >= 0) and (FColumn >= 0) then
  begin
    lblAngle1.Caption := NewCaption(KXT3DAngle1);
    lblAngle2.Caption := NewCaption(KXT3DAngle2);
    lblAngle3.Caption := NewCaption(KXT3DAngle3);
    lblKx.Caption := NewCaption(rsKx);
    lblKy.Caption := NewCaption(rsKy);
    lblKz.Caption := NewCaption(rsKz);

    PlotAngle(KXT3DAngle1, imgAngle1);
    PlotAngle(KXT3DAngle2, imgAngle2);
    PlotAngle(KXT3DAngle3, imgAngle3);
  end
  else
  begin
    lblAngle1.Caption := KXT3DAngle1;
    lblAngle2.Caption := KXT3DAngle2;
    lblAngle3.Caption := KXT3DAngle3;
    lblKx.Caption := rsKx;
    lblKy.Caption := rsKy;
    lblKz.Caption := rsKz;
  end;

end;

procedure TfrmGridValue.virttreecomboDataSetsChange(Sender: TObject);
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets);
  UpdateSelectedData(FLayer, FRow, FColumn);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.GetSelectedDataArray(var OtherDataSet: TDataArray);
begin
  if DiscretizationDefined then
  begin
    OtherDataSet := frmGoPhast.PhastModel.SelectedModel.DataArrayManager.
      GetDataSetByName(virttreecomboDataSets.Text);
  end
  else
  begin
    OtherDataSet := nil;
  end;
end;

procedure TfrmGridValue.UpdateScreenObjectInfo
  (const Column, Row, Layer: Integer; Location: TPoint2D;
  Model: TBaseModel);
var
  Value: Double;
  DirectionText: string;
  Segment: TCellElementSegment;
  ASegment: TCellElementSegment;
  LocalModel: TCustomModel;
  temp: TFloat;
  Segments: TCellElementSegmentList;
  SegmentIndex: Integer;
  procedure GetDirectionVariables(var VarIndex, MaxCount: Integer;
    var VarLabel: string);
  var
    Mesh: TSutraMesh3D;
  begin
    VarIndex := -1;
    MaxCount := 0;
    VarLabel := '';
    case frmGoPhast.ModelSelection of
      msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015, msFootPrint:
        begin
          case FSelectedScreenObject.ViewDirection of
            vdTop:
              begin
                VarIndex := 0;
                MaxCount := frmGoPhast.PhastModel.LayerCount;
                VarLabel := StrLayer;
              end;
            vdFront:
              begin
                VarIndex := 1;
                MaxCount := frmGoPhast.PhastModel.RowCount;
                VarLabel := StrRow;
              end;
            vdSide:
              begin
                VarIndex := 2;
                MaxCount := frmGoPhast.PhastModel.ColumnCount;
                VarLabel := StrColumn1;
              end;
            else
              Assert(False);
          end;
        end;
      msSutra22, msSutra30, msSutra40:
        begin
          Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
          case FSelectedScreenObject.ViewDirection of
            vdTop:
              begin
                VarIndex := 0;
                MaxCount := Mesh.LayerCount;
                VarLabel := StrLayer;
              end;
            vdFront:
              begin
                VarIndex := 1;
                MaxCount := 1;
                VarLabel := StrRow;
              end;
            vdSide:
              begin
                VarIndex := 2;
                MaxCount := Mesh.Mesh2D.Nodes.Count;
                VarLabel := StrColumn1;
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;
  end;
  procedure AssignHigherElevLabel(const ExtraText: string);
  var
    Indices: array[0..2] of Integer;
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsHigher3DElevationAssigned(Column, Row, Layer, LocalModel) then
      begin
        Value := FSelectedScreenObject.
          Higher3DElevations[LocalModel][Layer, Row, Column];
        lblHigher3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indices[0] := Layer;
        Indices[1] := Row;
        Indices[2] := Column;
        GetDirectionVariables(VarIndex, MaxCount, VarLabel);

        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indices[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsHigher3DElevationAssigned(Indices[2], Indices[1],
            Indices[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Higher3DElevations[LocalModel][Indices[0], Indices[1], Indices[2]];
            lblHigher3rdDimensionCoordinate.Caption := Format(Str0s1s2gOn,
              [ExtraText, DirectionText, Value, VarLabel, LayRowColIndex+1]);
            FoundValue := True;
            break;
          end
        end;
        if not FoundValue then
        begin
          lblHigher3rdDimensionCoordinate.Caption :=
            ExtraText + DirectionText + StrNotAssigned;
        end;
      end;
    end
    else
    begin
      lblHigher3rdDimensionCoordinate.Caption := ExtraText + DirectionText;
    end;
  end;
  procedure AssignLowerElevLabel(const ExtraText: string);
  var
    Indices: array[0..2] of Integer;
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsLower3DElevationAssigned(Column, Row, Layer, LocalModel) then
      begin
        Value := FSelectedScreenObject.
          Lower3DElevations[LocalModel][Layer, Row, Column];
        lblLower3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indices[0] := Layer;
        Indices[1] := Row;
        Indices[2] := Column;

        GetDirectionVariables(VarIndex, MaxCount, VarLabel);
//        Grid := frmGoPhast.Grid;
//
//        VarIndex := -1;
//        MaxCount := 0;
//        VarLabel := '';
//        case FSelectedScreenObject.ViewDirection of
//          vdTop:
//            begin
//              VarIndex := 0;
//              MaxCount := Grid.LayerCount;
//              VarLabel := StrLayer;
//            end;
//          vdFront:
//            begin
//              VarIndex := 1;
//              MaxCount := Grid.RowCount;
//              VarLabel := StrRow;
//            end;
//          vdSide:
//            begin
//              VarIndex := 2;
//              MaxCount := Grid.ColumnCount;
//              VarLabel := StrColumn1;
//            end;
//          else
//            Assert(False);
//        end;

        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indices[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsLower3DElevationAssigned(Indices[2], Indices[1],
            Indices[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Lower3DElevations[LocalModel][Indices[0], Indices[1], Indices[2]];
            lblLower3rdDimensionCoordinate.Caption := Format(Str0s1s2gOn,
              [ExtraText, DirectionText, Value, VarLabel, LayRowColIndex+1]);
            FoundValue := True;
            break;
          end
        end;
        if not FoundValue then
        begin
          lblLower3rdDimensionCoordinate.Caption :=
            ExtraText + DirectionText + StrNotAssigned;
        end;
      end;
    end
    else
    begin
      lblLower3rdDimensionCoordinate.Caption := ExtraText + DirectionText;
    end;
  end;
begin
  LocalModel := frmGoPhast.PhastModel.SelectedModel;
  if (frmGoPhast.PhastModel.SelectedScreenObjectCount = 1) then
  begin
    if (FSelectedScreenObject = nil)
      or not FSelectedScreenObject.Selected then
    begin
      UpdatedSelectedObject;
    end;
    if FSelectedScreenObject = nil then
    begin
      lblSelectedObject.Caption := StrSelectedObject;
      Exit;
    end;
    lblSelectedObject.Caption := StrSelectedObject
      + ': ' + FSelectedScreenObject.Name;

    if FViewDirection = FSelectedScreenObject.ViewDirection then
    begin
      if FViewDirection = vdSide then
      begin
        temp := Location.x;
        Location.x := Location.y;
        Location.y := temp;
      end;

      Segment := nil;
      Segments := FSelectedScreenObject.Segments[Model];
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        case FSelectedScreenObject.ViewDirection of
          vdTop:
            begin
              if (ASegment.Col = Column)
                and (ASegment.Row = Row) then
              begin
                Segment := ASegment;
                break;
              end;
            end;
          vdFront:
            begin
              if (ASegment.Col = Column)
                and (ASegment.Layer = Layer) then
              begin
                Segment := ASegment;
                break;
              end;
            end;
          vdSide:
            begin
              if (ASegment.Row = Row) and (ASegment.Layer = Layer) then
              begin
                Segment := ASegment;
                break;
              end
            end;
          else Assert(False);
        end;
      end;

//      Segment := FSelectedScreenObject.Segments[Model].ClosestSegment(Location, LocalAnisotropy);
      case FSelectedScreenObject.ViewDirection of
        vdTop:
          begin
            DirectionText := StrZcoordinate;
            if (Segment <> nil) and (Segment.Col = Column)
              and (Segment.Row = Row) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
        vdFront:
          begin
            DirectionText := StrYcoordinate;
            if (Segment <> nil) and (Segment.Col = Column)
              and (Segment.Layer = Layer) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
        vdSide:
          begin
            DirectionText := StrXcoordinate;
            if (Segment <> nil)
              and (Segment.Row = Row) and (Segment.Layer = Layer) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      lblVertex.Caption := '';
      lblSection.Caption := '';
    end;


    case FSelectedScreenObject.ElevationCount of
      ecZero:
        begin
          cbShowThirdDValues.Enabled := False;
          lblHigher3rdDimensionCoordinate.Caption := '';
          lblLower3rdDimensionCoordinate.Caption := '';
        end;
      ecOne:
        begin
          cbShowThirdDValues.Enabled := True;
          lblLower3rdDimensionCoordinate.Caption := '';
          AssignHigherElevLabel('');
        end;
      ecTwo:
        begin
          cbShowThirdDValues.Enabled := True;
          AssignHigherElevLabel(StrHigher);
          AssignLowerElevLabel(StrLower);
        end;
    else
      Assert(False);
    end;
  end
  else if frmGoPhast.PhastModel.SelectedScreenObjectCount > 1 then
  begin
    lblSelectedObject.Caption := StrSelectedObjectMu;
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := Format(StrPriorVertex, ['']);
    lblSection.Caption := StrSection1;
  end
  else
  begin
    lblSelectedObject.Caption := StrSelectedObjectNo;
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := Format(StrPriorVertex, ['']);
    lblSection.Caption := StrSection1;
  end;
end;

procedure TfrmGridValue.UpdateDataSets;
var
  VirtNoneNode: PVirtualNode;
begin
  virttreecomboDataSets.Tree.Clear;
  VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
  virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

  FillVirtualStringTreeWithDataSets(virttreecomboDataSets.Tree,
    FDataSetDummyObjects, nil, CanDisplayDataSet);
end;

procedure TfrmGridValue.GetWidthForModpathPanels(var AvailableWidth: Integer);
begin
  AvailableWidth := ClientWidth - splAllDataSets.Width;
  if not rrlcurrentData.Collapsed then
  begin
    AvailableWidth := AvailableWidth - rrlcurrentData.Width;
  end
  else
  begin
    AvailableWidth := AvailableWidth - rrlcurrentData.LabelWidth;
  end;
  if not rrlAllDataSets.Collapsed then
  begin
    AvailableWidth := AvailableWidth - rrlAllDataSets.Width;
  end
  else
  begin
    AvailableWidth := AvailableWidth - rrlAllDataSets.LabelWidth;
  end;
end;

function TfrmGridValue.DiscretizationDefined: Boolean;
var
  Grid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
  DisvGrid: TModflowDisvGrid;
begin
  result := False;
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
      :
      begin
        Grid := frmGoPhast.Grid;
        result := (Grid <> nil) and (Grid.LayerCount >= 1)
          and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1);
      end;
    msModflow2015:
      begin
        if frmGoPhast.PhastModel.DisvUsed then
        begin
          DisvGrid := frmGoPhast.PhastModel.SelectedModel.DisvGrid;
          result := (DisvGrid <> nil) and (DisvGrid.ColumnCount > 0)
            and (DisvGrid.LayerCount > 0);
        end
        else
        begin
          Grid := frmGoPhast.Grid;
          result := (Grid <> nil) and (Grid.LayerCount >= 1)
            and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1);
        end;
      end;
    msSutra22, msSutra30, msSutra40:
      begin
        Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
        result := (Mesh <> nil) and (Mesh.Mesh2D.Nodes.Count > 0);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmGridValue.DisplayEndPointData(const Location: TPoint2D);
var
  AnEndPoint: TEndPoint;
  APointer: Pointer;
  Y: TFloat;
  X: TFloat;
  EndPointQuadTree: TRbwQuadTree;
  EndPoints: TEndPointReader;
  DisplayPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
//  ColIndex: Integer;
  RowIndex: Integer;
  ALayer: Integer;
  ColIndex: Integer;
  LocalModel: TCustomModel;
  TwoDMesh: TModflowIrregularGrid2D;
  CellNumber: Integer;
  ACell: TModflowIrregularCell2D;
  Corner1: TPoint2D;
  Corner2: TPoint2D;
  TestDistance: double;
begin
  if (FPriorEndPointLocation.x = Location.x)
    and (FPriorEndPointLocation.y = Location.Y)then
  begin
    Exit;
  end;
  if frmGoPhast.ModelSelection in [msPhast, msSutra22, msSutra30, msSutra40, msFootPrint] then
  begin
    rrlEndPoint.Visible := False;
    Exit;
  end;
  FPriorEndPointLocation := Location;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  EndPoints := LocalModel.EndPoints;
  EndPointQuadTree := nil;
  if EndPoints.Visible then
  begin
    case FViewDirection of
      vdTop:
        EndPointQuadTree := EndPoints.TopQuadTree;
      vdFront:
        EndPointQuadTree := EndPoints.FrontQuadTree;
      vdSide:
        EndPointQuadTree := EndPoints.SideQuadTree;
    else
      Assert(False);
    end;
  end;
  rrlEndPoint.Visible := EndPoints.Visible and (EndPointQuadTree.Count > 0);
  if rrlEndPoint.Visible then
  begin
    X := Location.X;
    Y := Location.Y;
    EndPointQuadTree.FirstNearestPoint(X, Y, APointer);
    AnEndPoint := APointer;
    Assert(AnEndPoint <> nil);



    DisplayPoint := False;

    case EndPoints.DisplayLimits.WhereToPlot of
      wtpStart:
        begin
          case FViewDirection of
            vdTop:
              begin
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (AnEndPoint as TEndPointV7).InitialCellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  Corner1.x := ACell.MinX;
                  Corner1.y := ACell.MinY;
                  Corner2.x := ACell.MaxX;
                  Corner2.y := ACell.MaxY;
                  TestDistance := Distance(Corner1,Corner2);

                  DisplayPoint := (Distance(Corner1, Location) <= TestDistance)
                    or (Distance(Corner2, Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner1.x, Corner2.y), Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner2.x, Corner1.y), Location) <= TestDistance);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - AnEndPoint.StartColumn) <= 1)
                    and (Abs(FRow+1 - AnEndPoint.StartRow) <= 1);
                end;
              end;
            vdFront:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.StartLayer);
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (AnEndPoint as TEndPointV7).InitialCellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  DisplayPoint :=
                    (ACell.MinX <= Location.X) and (ACell.MaxX >= Location.X)
                    and (Abs(FLayer - ALayer) <= 1);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - AnEndPoint.StartColumn) <= 1)
                    and (Abs(FLayer - ALayer) <= 1);
                end;
              end;
            vdSide:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.StartLayer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.StartRow) <= 1);
              end;
            else Assert(False);
          end;
        end;
      wtpEnd:
        begin
          case FViewDirection of
            vdTop:
              begin
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (AnEndPoint as TEndPointV7).FinalCellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  Corner1.x := ACell.MinX;
                  Corner1.y := ACell.MinY;
                  Corner2.x := ACell.MaxX;
                  Corner2.y := ACell.MaxY;
                  TestDistance := Distance(Corner1,Corner2);

                  DisplayPoint := (Distance(Corner1, Location) <= TestDistance)
                    or (Distance(Corner2, Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner1.x, Corner2.y), Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner2.x, Corner1.y), Location) <= TestDistance);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - AnEndPoint.EndColumn) <= 1)
                    and (Abs(FRow+1 - AnEndPoint.EndRow) <= 1);
                end;
              end;
            vdFront:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.EndLayer);
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (AnEndPoint as TEndPointV7).FinalCellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  DisplayPoint :=
                    (ACell.MinX <= Location.X) and (ACell.MaxX >= Location.X)
                    and (Abs(FLayer - ALayer) <= 1);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - AnEndPoint.EndColumn) <= 1)
                    and (Abs(FLayer - ALayer) <= 1);
                end;
              end;
            vdSide:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.EndLayer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.endRow) <= 1);
              end;
            else Assert(False);
          end;
        end;
      else
        Assert(False);
    end;


    if not DisplayPoint then
    begin
      ZoomBox := nil;
      case FViewDirection of
        vdTop: ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        vdFront: ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        vdSide: ZoomBox := frmGoPhast.framesideView.ZoomBox;
        else Assert(False);
      end;
      DisplayPoint :=
        (Abs(ZoomBox.XCoord(X) - ZoomBox.XCoord(Location.X)) <= SelectionWidth)
        and (Abs(ZoomBox.YCoord(Y) - ZoomBox.YCoord(Location.Y)) <= SelectionWidth);
    end;

    if DisplayPoint then
    begin
      lbledtReleaseTime.Text := FloatToStr(AnEndPoint.ReleaseTime);
      lbledtTerminationCode.Text := IntToStr(AnEndPoint.TerminationCode);
      lbledtTrackingTime.Text := FloatToStr(AnEndPoint.TrackingTime);

      rdgEndPoints.BeginUpdate;
      try
        begin
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprNumber)] :=
            IntToStr(AnEndPoint.ParticleNumber);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprZone)] :=
            IntToStr(AnEndPoint.StartZoneCode);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprColumn)] :=
            IntToStr(AnEndPoint.StartColumn);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprRow)] :=
            IntToStr(AnEndPoint.StartRow);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprLayer)] :=
            IntToStr(AnEndPoint.StartLayer);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprX)] :=
            FloatToStr(AnEndPoint.StartX);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprY)] :=
            FloatToStr(AnEndPoint.StartY);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprZ)] :=
            FloatToStr(AnEndPoint.StartZ);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprXPrime)] :=
            FloatToStr(AnEndPoint.StartXPrime);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprYPrime)] :=
            FloatToStr(AnEndPoint.StartYPrime);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprLocalZ)] :=
            FloatToStr(AnEndPoint.StartLocalZ);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprTimeStep)] :=
            IntToStr(AnEndPoint.StartTimeStep);
          if AnEndPoint is TEndPointV6 then
          begin
            rdgEndPoints.Cells[Ord(epcStart), Ord(eprParticleGroup)] :=
              IntToStr(TEndPointV6(AnEndPoint).ParticleGroup);
          end
          else
          begin
            rdgEndPoints.Cells[Ord(epcStart), Ord(eprParticleGroup)] := '';
          end;

          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprNumber)] :=
            IntToStr(AnEndPoint.ParticleNumber);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprZone)] :=
            IntToStr(AnEndPoint.EndZoneCode);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprColumn)] :=
            IntToStr(AnEndPoint.EndColumn);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprRow)] :=
            IntToStr(AnEndPoint.EndRow);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprLayer)] :=
            IntToStr(AnEndPoint.EndLayer);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprX)] :=
            FloatToStr(AnEndPoint.EndX);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprY)] :=
            FloatToStr(AnEndPoint.EndY);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprZ)] :=
            FloatToStr(AnEndPoint.EndZ);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprXPrime)] :=
            FloatToStr(AnEndPoint.EndXPrime);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprYPrime)] :=
            FloatToStr(AnEndPoint.EndYPrime);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprLocalZ)] :=
            FloatToStr(AnEndPoint.EndLocalZ);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprTimeStep)] :=
            IntToStr(AnEndPoint.EndTimeStep);

          if AnEndPoint is TEndPointV6 then
          begin
            rdgEndPoints.Cells[Ord(epcEnd), Ord(eprParticleGroup)] :=
              IntToStr(TEndPointV6(AnEndPoint).ParticleGroup);
          end
          else
          begin
            rdgEndPoints.Cells[Ord(epcEnd), Ord(eprParticleGroup)] := '';
          end;
        end;
      finally
        rdgEndPoints.EndUpdate;
      end;
    end
    else
    begin
      rdgEndPoints.BeginUpdate;
      try
        for ColIndex := 1 to rdgEndPoints.ColCount - 1 do
        begin
          for RowIndex := 1 to rdgEndPoints.RowCount - 1 do
          begin
            rdgEndPoints.Cells[ColIndex, RowIndex] := '';
          end;
        end;
      finally
        rdgEndPoints.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmGridValue.DisplayPathlineData(const Location: TPoint2D);
var
  PathLinePoint: TPathLinePoint;
  APointer: Pointer;
  Y: TFloat;
  X: TFloat;
  PathQuadTree: TRbwQuadTree;
  PathLines: TPathLineReader;
  DisplayPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
  ColIndex: Integer;
  RowIndex: Integer;
  PathLine: TCustomPathLine;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  List: TList;
  APathLinePoint: TPathLinePoint;
  ALayer: Integer;
  LocalModel: TCustomModel;
  TwoDMesh: TModflowIrregularGrid2D;
  ACell: TModflowIrregularCell2D;
  CellNumber: Integer;
  Corner1: TPoint2D;
  Corner2: TPoint2D;
  TestDistance: double;
begin
  if (FPriorLocation.x = Location.x)
    and (FPriorLocation.y = Location.Y)then
  begin
    Exit;
  end;
  if frmGoPhast.ModelSelection in [msPhast, msSutra22, msSutra30, msSutra40, msFootPrint] then
  begin
    rrlPathline.Visible := False;
    Exit;
  end;
  FPriorLocation := Location;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  PathLines := LocalModel.PathLines;
  PathQuadTree := nil;
  if PathLines.Visible then
  begin
    case FViewDirection of
      vdTop:
        PathQuadTree := PathLines.TopQuadTree;
      vdFront:
        PathQuadTree := PathLines.FrontQuadTree;
      vdSide:
        PathQuadTree := PathLines.SideQuadTree;
    else
      Assert(False);
    end;
  end;
  rrlPathline.Visible := PathLines.Visible and (PathQuadTree.Count > 0);
  if rrlPathline.Visible then
  begin
    X := Location.X;
    Y := Location.Y;
    PathQuadTree.FirstNearestPoint(X, Y, APointer);
    DisplayPoint := False;
//    case PathLines.ModpathVersion of
//      pv5:
//        begin
          PathLinePoint := APointer;
          Assert(PathLinePoint <> nil);
          case FViewDirection of
            vdTop:
              begin
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (PathLinePoint as TPathLinePointV7).CellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  Corner1.x := ACell.MinX;
                  Corner1.y := ACell.MinY;
                  Corner2.x := ACell.MaxX;
                  Corner2.y := ACell.MaxY;
                  TestDistance := Distance(Corner1,Corner2);

                  DisplayPoint := (Distance(Corner1, Location) <= TestDistance)
                    or (Distance(Corner2, Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner1.x, Corner2.y), Location) <= TestDistance)
                    or (Distance(EquatePoint(Corner2.x, Corner1.y), Location) <= TestDistance);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
                    and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
                end;
              end;
            vdFront:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(PathLinePoint.Layer);
                if LocalModel.DisvUsed then
                begin
                  TwoDMesh := LocalModel.DisvGrid.TwoDGrid;
                  CellNumber := (PathLinePoint as TPathLinePointV7).CellNumber-1;
                  CellNumber := CellNumber mod TwoDMesh.ColumnCount;
                  ACell := TwoDMesh.Cells[CellNumber];
                  DisplayPoint :=
                    (ACell.MinX <= Location.X) and (ACell.MaxX >= Location.X)
                    and (Abs(FLayer - ALayer) <= 1);
                end
                else
                begin
                  DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
                    and (Abs(FLayer - ALayer) <= 1);
                end;
              end;
            vdSide:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(PathLinePoint.Layer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
              end;
            else Assert(False);
          end;
//        end;
//      pv6_0:
//        begin
//          PathLinePointV6 := APointer;
//          Assert(PathLinePointV6 <> nil);
//          case FViewDirection of
//            vdTop:
//              begin
//                DisplayPoint := (Abs(FColumn+1 - PathLinePointV6.Column) <= 1)
//                  and (Abs(FRow+1 - PathLinePointV6.Row) <= 1);
//              end;
//            vdFront:
//              begin
//                ALayer := frmGoPhast.PhastModel.
//                  ModflowLayerToDataSetLayer(PathLinePointV6.Layer);
//                DisplayPoint := (Abs(FColumn+1 - PathLinePointV6.Column) <= 1)
//                  and (Abs(FLayer - ALayer) <= 1);
//              end;
//            vdSide:
//              begin
//                ALayer := frmGoPhast.PhastModel.
//                  ModflowLayerToDataSetLayer(PathLinePointV6.Layer);
//                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
//                  and (Abs(FRow+1 - PathLinePointV6.Row) <= 1);
//              end;
//            else Assert(False);
//          end;
//        end;
//      else
//        Assert(False);
//    end;


    if not DisplayPoint then
    begin
      ZoomBox := nil;
      case FViewDirection of
        vdTop: ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        vdFront: ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        vdSide: ZoomBox := frmGoPhast.framesideView.ZoomBox;
        else Assert(False);
      end;
      DisplayPoint :=
        (Abs(ZoomBox.XCoord(X) - ZoomBox.XCoord(Location.X)) <= SelectionWidth)
        and (Abs(ZoomBox.YCoord(Y) - ZoomBox.YCoord(Location.Y)) <= SelectionWidth);
    end;

    if DisplayPoint then
    begin
      PathLine := PathLinePoint.ParentLine;
      edLength.Text := FloatToStr(PathLine.Length);
      FirstPoint :=PathLine.Points[0];
      LastPoint :=PathLine.Points[PathLine.Points.Count -1];
      List := TList.Create;
      try
        List.Add(nil);
        List.Add(FirstPoint);
        List.Add(LastPoint);
        List.Add(PathLinePoint);
        rdgPathline.BeginUpdate;
        try
          for ColIndex := Ord(plcFirst) to Ord(plcClosest) do
          begin
            APathLinePoint := List[ColIndex];
            rdgPathline.Cells[ColIndex, Ord(plrNumber)] :=
              IntToStr(PathLine.Index+1);

            rdgPathline.Cells[ColIndex, Ord(plrX)] :=
              FloatToStr(APathLinePoint.X);
            rdgPathline.Cells[ColIndex, Ord(plrY)] :=
              FloatToStr(APathLinePoint.Y);
            rdgPathline.Cells[ColIndex, Ord(plrZ)] :=
              FloatToStr(APathLinePoint.Z);
            rdgPathline.Cells[ColIndex, Ord(plrXPrime)] :=
              FloatToStr(APathLinePoint.XPrime);
            rdgPathline.Cells[ColIndex, Ord(plrYPrime)] :=
              FloatToStr(APathLinePoint.YPrime);
            rdgPathline.Cells[ColIndex, Ord(plrLocalZ)] :=
              FloatToStr(APathLinePoint.LocalZ);
            rdgPathline.Cells[ColIndex, Ord(plrTime)] :=
              FloatToStr(APathLinePoint.AbsoluteTime);
            rdgPathline.Cells[ColIndex, Ord(plrColumn)] :=
              IntToStr(APathLinePoint.Column);
            rdgPathline.Cells[ColIndex, Ord(plrRow)] :=
              IntToStr(APathLinePoint.Row);
            rdgPathline.Cells[ColIndex, Ord(plrLayer)] :=
              IntToStr(APathLinePoint.Layer);
            rdgPathline.Cells[ColIndex, Ord(plrTimeStep)] :=
              IntToStr(APathLinePoint.TimeStep);
            if APathLinePoint is TPathLinePointV6 then
            begin
              rdgPathline.Cells[ColIndex, Ord(plrGroup)] :=
                IntToStr(TPathLinePointV6(APathLinePoint).ParticleGroup);
            end
            else
            begin
              rdgPathline.Cells[ColIndex, Ord(plrGroup)] := '';
            end;
          end;
        finally
          rdgPathline.EndUpdate;
        end;
      finally
        List.Free;
      end;
    end
    else
    begin
      rdgPathline.BeginUpdate;
      try
        for ColIndex := 1 to rdgPathline.ColCount - 1 do
        begin
          for RowIndex := 1 to rdgPathline.RowCount - 1 do
          begin
            rdgPathline.Cells[ColIndex, RowIndex] := '';
          end;
        end;
      finally
        rdgPathline.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmGridValue.DisplaySwrData(Layer, Row, Column: integer);
var
  Model: TCustomModel;
  SwrConnections: TSwrReachConnectionsPlot;
  Reaches: TList<TSwrReachPlot>;
  ReachIndex: integer;
  AReach: TSwrReachPlot;
  RowIndex: Integer;
  ColIndex: integer;
  MaxNeighbors: integer;
  NeighborIndex: integer;
  AStructure: TStructure;
  StuctureList: TList<TStructure>;
  StructureIndex: integer;
begin
  // For an unknown reason, assigning values to cells in the first non-fixed
  // column of a TRbwDataGrid4 causes the dialog box to fail to update properly.
//  rdgSwrReaches.ColWidths[0] := 1;
//  rdgSwrStructures.ColWidths[0] := 1;
  Model := frmGoPhast.PhastModel.SelectedModel;
  SwrConnections := Model.SwrReachConnectionsPlot;
  rrlSWR.Visible := Model.SwrIsSelected
    and (SwrConnections.ReachesToPlot <> stpNone)
    and (SwrConnections.PlotReachConnections
    or SwrConnections.PlotStructures or SwrConnections.PlotUnconnected);
  if rrlSWR.Visible and not rrlSWR.Collapsed then
  begin
    Reaches := TList<TSwrReachPlot>.Create;
    StuctureList := TList<TStructure>.Create;
    try
      MaxNeighbors := 0;
      for ReachIndex := 0 to SwrConnections.ReachList.Count - 1 do
      begin
        AReach := SwrConnections.ReachList[ReachIndex];
        if (AReach.Layer = Layer) and (AReach.Row = Row) and (AReach.Column = Column) then
        begin
          Reaches.Add(AReach);
          if MaxNeighbors < AReach.NeighborCount then
          begin
            MaxNeighbors := AReach.NeighborCount;
          end;
          for StructureIndex := 0 to Model.SwrStructures.Count - 1 do
          begin
            AStructure := Model.SwrStructures[StructureIndex];
            if AStructure.Reach = AReach.Reach then
            begin
              StuctureList.Add(AStructure);
            end;
          end;
        end;
      end;

      rdgSwrReaches.BeginUpdate;
      try
        if Reaches.Count > 0 then
        begin
          rdgSwrReaches.RowCount := Reaches.Count+1;
          rdgSwrReaches.ColCount := MaxNeighbors+2;

          for NeighborIndex := 0 to MaxNeighbors - 1 do
          begin
            ColIndex := NeighborIndex+2;
            rdgSwrReaches.Cells[ColIndex,0] := Format(StrICONd, [NeighborIndex+1]);
          end;

          for ReachIndex := 0 to Reaches.Count - 1 do
          begin
            AReach := Reaches[ReachIndex];
            RowIndex := ReachIndex+1;
            rdgSwrReaches.Cells[0,RowIndex] := IntToStr(AReach.Reach);
            rdgSwrReaches.Cells[1,RowIndex] :=
              (AReach.ScreenObject as TScreenObject).Name;
            for NeighborIndex := 0 to MaxNeighbors - 1 do
            begin
              ColIndex := NeighborIndex+2;
              if NeighborIndex < AReach.NeighborCount then
              begin
                rdgSwrReaches.Cells[ColIndex,RowIndex] :=
                  IntToStr(AReach.Neighbors[NeighborIndex]);
              end
              else
              begin
                rdgSwrReaches.Cells[ColIndex,RowIndex] := '';
              end;
            end;
          end;
        end
        else
        begin
          rdgSwrReaches.RowCount := 2;

          for ColIndex := 0 to rdgSwrReaches.ColCount - 1 do
          begin
            rdgSwrReaches.Cells[ColIndex,1] := '';
          end;
        end;
      finally
        rdgSwrReaches.EndUpdate;
        rdgSwrReaches.Repaint;
      end;


      rdgSwrStructures.BeginUpdate;
      try
        if StuctureList.Count > 0 then
        begin
          rdgSwrStructures.RowCount := StuctureList.Count+1;
          for StructureIndex := 0 to StuctureList.Count - 1 do
          begin
            RowIndex := StructureIndex+1;
            AStructure := StuctureList[StructureIndex];
            rdgSwrStructures.Cells[0,RowIndex] := AStructure.Name;
            rdgSwrStructures.Cells[1,RowIndex] := IntToStr(AStructure.Reach);
            rdgSwrStructures.Cells[2,RowIndex] := IntToStr(AStructure.ConnectedReach);
          end;
        end
        else
        begin
          rdgSwrStructures.RowCount := 2;
          for ColIndex := 0 to rdgSwrStructures.ColCount - 1 do
          begin
            rdgSwrStructures.Cells[ColIndex,1] := '';
          end;
        end;
      finally
        rdgSwrStructures.EndUpdate;
        rdgSwrStructures.Repaint;
      end;

    finally
      Reaches.Free;
      StuctureList.Free;
    end;
  end;
end;

procedure TfrmGridValue.InitializePathlineGrid;
begin
  rdgPathline.Cells[Ord(plcFirst), 0] := StrFirst;
  rdgPathline.Cells[Ord(plcLast), 0] := StrLast;
  rdgPathline.Cells[Ord(plcClosest), 0] := StrClosest;
  rdgPathline.Cells[0, Ord(plrNumber)] := StrNumber;
  rdgPathline.Cells[0, Ord(plrX)] := StrX;
  rdgPathline.Cells[0, Ord(plrY)] := StrY;
  rdgPathline.Cells[0, Ord(plrZ)] := StrZ;
  rdgPathline.Cells[0, Ord(plrXPrime)] := StrXPrime;
  rdgPathline.Cells[0, Ord(plrYPrime)] := StrYPrime;
  rdgPathline.Cells[0, Ord(plrLocalZ)] := StrLocalZ;
  rdgPathline.Cells[0, Ord(plrTime)] := StrTime;
  rdgPathline.Cells[0, Ord(plrColumn)] := StrColumn1;
  rdgPathline.Cells[0, Ord(plrRow)] := StrRow;
  rdgPathline.Cells[0, Ord(plrLayer)] := StrLayer;
  rdgPathline.Cells[0, Ord(plrTimeStep)] := StrTimeStep;
  rdgPathline.Cells[0, Ord(plrGroup)] := StrGroup;
end;

procedure TfrmGridValue.InitializeSwrGrids;
begin
  rdgSwrReaches.BeginUpdate;
  try
    rdgSwrReaches.Cells[0,0] := StrReach;
    rdgSwrReaches.Cells[1,0] := StrObject;
  finally
    rdgSwrReaches.EndUpdate;
  end;

  rdgSwrStructures.BeginUpdate;
  try
    rdgSwrStructures.Columns[2].AutoAdjustColWidths := true;
    rdgSwrStructures.Cells[0,0] := StrStructure;
    rdgSwrStructures.Cells[1,0] := StrReach;
    rdgSwrStructures.Cells[2,0] := StrConnectedReach;
  finally
    rdgSwrStructures.EndUpdate;
    rdgSwrStructures.Columns[2].AutoAdjustColWidths := False;
  end;
end;

procedure TfrmGridValue.jvrltEndPointCollapse(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
   GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if not rrlPathline.Collapsed then
    begin
      rrlPathline.Width := AvailableWidth-rrlPathline.LabelWidth;
    end;
  end;
//  if rrlPathline.Collapsed then
//  begin
//    jvrltEndPoint.Align := alLeft;
//  end
//  else
//  begin
//    jvrltEndPoint.Align := alRight;
//    Application.ProcessMessages;
//    rrlPathline.Align := alClient;
//  end;
end;

procedure TfrmGridValue.jvrltEndPointExpand(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
//  rrlPathline.Align := alLeft;
//    Application.ProcessMessages;
  GetWidthForModpathPanels(AvailableWidth);

  if AvailableWidth > 0 then
  begin
    if rrlPathline.Collapsed then
    begin
      rrlEndPoint.Width := AvailableWidth-rrlPathline.LabelWidth
    end
    else
    begin
      rrlEndPoint.Width := AvailableWidth div 2;
      rrlPathline.Width := AvailableWidth div 2;
    end;

  end;
//  rrlEndPoint.Align := alClient;
end;

procedure TfrmGridValue.jvrltPathlineCollapse(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
  GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if not rrlEndPoint.Collapsed then
    begin
      rrlEndPoint.Width := AvailableWidth-rrlEndPoint.LabelWidth;
    end;
  end;
//  jvrltPathline.Align := alLeft;
//  if rrlEndPoint.Collapsed then
//  begin
//    rrlEndPoint.Align := alLeft;
//  end
//  else
//  begin
//    rrlEndPoint.Align := alClient;
//  end;
//  Application.ProcessMessages;
//  rrlEndPoint.Left := jvrltPathline.Left + 22;
end;

procedure TfrmGridValue.jvrltPathlineExpand(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
//  rrlPathline.Align := alClient;
//  rrlEndPoint.Align := alRight;
  GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if rrlEndPoint.Collapsed then
    begin
      rrlPathline.Width := AvailableWidth -rrlEndPoint.LabelWidth;
    end
    else
    begin
      rrlPathline.Width := AvailableWidth div 2;
      rrlEndPoint.Width := AvailableWidth div 2;
    end;
    rdgPathline.Width := rrlPathline.ClientWidth;
  end;
end;

procedure TfrmGridValue.InitializeEndpointGrid;
begin
  rdgEndPoints.Cells[Ord(epcStart), 0] := StrStart;
  rdgEndPoints.Cells[Ord(epcEnd), 0] := StrEnd;
  rdgEndPoints.Cells[0, Ord(eprNumber)] := StrNumber;
  rdgEndPoints.Cells[0, Ord(eprZone)] := StrZone;
  rdgEndPoints.Cells[0, Ord(eprX)] := StrX;
  rdgEndPoints.Cells[0, Ord(eprY)] := StrY;
  rdgEndPoints.Cells[0, Ord(eprZ)] := StrZ;
  rdgEndPoints.Cells[0, Ord(eprXPrime)] := StrXPrime;
  rdgEndPoints.Cells[0, Ord(eprYPrime)] := StrYPrime;
  rdgEndPoints.Cells[0, Ord(eprLocalZ)] := StrLocalZ;
  rdgEndPoints.Cells[0, Ord(eprColumn)] := StrColumn1;
  rdgEndPoints.Cells[0, Ord(eprRow)] := StrRow;
  rdgEndPoints.Cells[0, Ord(eprLayer)] := StrLayer;
  rdgEndPoints.Cells[0, Ord(eprTimeStep)] := StrTimeStep;
  rdgEndPoints.Cells[0, Ord(eprParticleGroup)] := StrGroup;
end;

procedure TfrmGridValue.UpdatedSelectedObject;
var
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  FSelectedScreenObject := nil;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      FSelectedScreenObject := ScreenObject;
      break;
    end;
  end;
end;

end.
