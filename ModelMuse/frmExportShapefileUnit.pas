unit frmExportShapefileUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, JvToolEdit, Mask, JvExMask,
  JvSpin, Grids, RbwDataGrid4, ComCtrls, ImgList, Contnrs, ExtCtrls,
  ShapefileUnit, AbstractGridUnit, GoPhastTypes, XBase1, SutraMeshUnit,
  SsButtonEd, RbwStringTreeCombo, DataSetUnit, VirtualTrees, System.ImageList,
  MeshRenumberingTypes;

type
  TExportShapeChoice = (escTwoDPoly, escTwoDPoint,escThreeDPoly,
    escThreeDPoint);

  TfrmExportShapefile = class(TfrmCustomGoPhast)
    tvExportItems: TTreeView;
    rdgTime: TRbwDataGrid4;
    seTimeCount: TJvSpinEdit;
    jfeElements: TJvFilenameEdit;
    jfeNodes: TJvFilenameEdit;
    jfeHorizontalFlowBarrier: TJvFilenameEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    lblExportItems: TLabel;
    lblTime: TLabel;
    lblTimeCount: TLabel;
    lblElements: TLabel;
    lblNodes: TLabel;
    lblHorizontalFlowBarrier: TLabel;
    ilCheckImages: TImageList;
    rgExportObjectType: TRadioGroup;
    rgHfbDimensions: TRadioGroup;
    comboModel: TComboBox;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    treeComboFilter: TRbwStringTreeCombo;
    lblFilter: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure tvExportItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seTimeCountChange(Sender: TObject);
    procedure rdgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
    procedure treeComboFilterTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure treeComboFilterTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FEdgeEdits: TList;
    FShapeFileName: string;
    FLocalGrid: TCustomModelGrid;
    FLayerLimit: Integer;
    FFields: TStringList;
    FNames: TStringList;
    FShapeDataBase: TXBase;
    FShapeType: Integer;
    FShapeFileWriter: TShapefileGeometryWriter;
    FGettingData: Boolean;
    FLocalMesh: IMesh3D;
    FFilterDataSets: TList;
    FFilterArray: TDataArray;
    FDisvUsed: Boolean;
    procedure GetDataSets;
    procedure GetBoundaryConditions;
    procedure UpdateParentNodeStates;
    procedure UpdateEnabledControls;
    procedure GetData;
    procedure InitializeNodeStates;
    procedure InitializeFileNames;
    procedure ReadSelectedTimes;
    procedure SetData;
    procedure ExportNodeShapes(DataSets, TimeLists: TList);
    procedure ExportElementShapes(DataSets, TimeLists: TList);
    procedure ExportHfbShapes(Edits: TList);
    procedure Assign2DShapeGeometry(Shape: TShapeObject; ColIndex,
      RowIndex: Integer; EvaluatedAt: TEvaluatedAt);
    function GetShapeType: Integer;
    // A side effect of @name is to add the data sets in
    // the @link(TCustomTimeList)s in "TimeLists" to "DataSets".
    procedure GetFieldNames(Names, Fields: TStringList;
      LayerLimit: Integer; TimeLists, DataSets: TList; EvaluatedAt: TEvaluatedAt);
    procedure InitializeControls;
    procedure Assign2DID_Fields(ID, ColIndex, RowIndex: Integer;
      ShapeDataBase: TXBase; EvaluatedAt: TEvaluatedAt);
    procedure Assign2DDataSetValuesToDataBase(DataSets: TList;
        DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
        RowIndex: integer; ShapeDataBase: TXBase; NameIndexStart: integer);
    procedure Export2DNodeShapes(DataSets: TList);
    procedure Export3DNodeShapes(DataSets: TList);
    procedure Assign3DShapeGeometry(Shape: TShapeObject; ColIndex,
      RowIndex, LayerIndex: Integer; EvaluatedAt: TEvaluatedAt);
    procedure Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex: Integer;
      ShapeDataBase: TXBase; EvaluatedAt: TEvaluatedAt);
    procedure Assign3DDataSetValuesToDataBase(DataSets: TList;
      DataSetIndex: Integer; Names: TStringList; ColIndex,
      RowIndex, LayerIndex: integer; ShapeDataBase: TXBase);
    procedure Export2DElementShapes(DataSets: TList);
    procedure Export3DElementShapes(DataSets: TList);
    procedure GetModels;
    procedure GetFilterDataSets;
    function BooleanOnly(DataArray: TDataArray): boolean;
    procedure GetFilterDataArray(var DataArray: TDataArray);
    function CanExportDataSet(DataArray: TDataArray): boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure InitializeDataBase(const ShapeFileName: string;
    ShapeDataBase: TXBase; Fields: TStringList);

var
  frmExportShapefile: TfrmExportShapefile;

implementation

uses Math, ClassificationUnit, PhastModelUnit, frmGoPhastUnit,
  PhastDataSets, RealListUnit, ModflowTimeUnit,
  TimeUnit, FastGEO, RbwParser, EdgeDisplayUnit, ModelMuseUtilities,
  frameCustomColorUnit, SutraTimeScheduleUnit, SutraBoundariesUnit,
  frmProgressUnit, SutraBoundaryUnit, ConvexHullUnit;

resourcestring
  StrSAlreadyExists = '%s already exists.  Do you want to replace it?';
  StrParentModel = 'Parent model';
  StrElementShapefileN = '&Element Shapefile name';
  StrNodeShapefileName = '&Node Shapefile name';
  StrCellShapefileName = '&Cell Shapefile name';
  StrCellCornerShapefi = '&Cell-Corner Shapefile name';
  StrMODFLOWHorizontalF = 'MODFLOW Horizontal Flow Barrier';
  StrExportGridDataTo = 'Export Grid Data to Shapefile';
  StrExportMeshDataTo = 'Export Mesh Data to Shapefile';
  StrTheFileSizeOfThe2GB = 'The file size of the dBASE file (%d) for this Sh' +
  'apefile exceeds 2 GB. It may not be useable in some software. Do you want' +
  ' to continue?';
  StrDoYouWantToSave = 'Do you want to save the a Shapefile at the %s?';

{$R *.dfm}

function ConvertPoint(Point: TPoint2D): TShapePoint; overload;
begin
  result.X := Point.x;
  result.Y := Point.y;
end;

function ConvertPoint(Point: TPoint3D): TShapePoint; overload;
begin
  result.X := Point.x;
  result.Y := Point.y;
end;

{function ConvertPoint(Point: T3DRealPoint): TShapePoint; overload;
begin
  result.X := Point.x;
  result.Y := Point.y;
end;}

function TfrmExportShapefile.BooleanOnly(DataArray: TDataArray): boolean;
begin
  result := (DataArray.DataType = rdtBoolean);
end;

procedure TfrmExportShapefile.GetFilterDataArray(var DataArray: TDataArray);
var
  LocalModel: TCustomModel;
begin
  DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(
    treeComboFilter.Text);
  if DataArray = nil then
  begin
    Exit;
  end;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  if LocalModel <> frmGoPhast.PhastModel then
  begin
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(DataArray.Name);
  end;
end;

procedure TfrmExportShapefile.GetFilterDataSets;
begin
  FillVirtualStringTreeWithDataSets(treeComboFilter.Tree, FFilterDataSets, nil,
    BooleanOnly);
end;

function TfrmExportShapefile.CanExportDataSet(DataArray: TDataArray): boolean;
begin
  result := False;
  case DataArray.EvaluatedAt of
    eaBlocks: result := True;
    eaNodes: result := frmGoPhast.PhastModel.ModelSelection
      in [msPhast, msSutra22, msSutra30, msSutra40];
    else Assert(False);
  end;
end;

procedure TfrmExportShapefile.GetDataSets;
var
  Index: Integer;
  Node: TTreeNode;
  SelectedDataArray: TDataArray;
  ClassificationList: TStringList;
  LayerGroupsDataSets: TList;
  SutraLayerGroupsDataSets: TList;
  DataSetList: TClassificationList;
  LayerGroupList: TClassificationList;
  SutraLayerGroupList: TClassificationList;
  DataSet: TDataArray;
  ClassificationObject: TDataSetClassification;
  SelectedName: string;
  Position: integer;
  HydrogeologicUnitNames: TStringList;
  HufDataArrays: TClassificationList;
  DataArrayManager: TDataArrayManager;
begin
  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }
  SelectedDataArray := frmGoPhast.PhastModel.ThreeDDataSet;
  if SelectedDataArray = nil then
  begin
    SelectedName := '';
  end
  else
  begin
    SelectedName := SelectedDataArray.Name;
  end;
  ClassificationList := TStringList.Create;
  try

    HydrogeologicUnitNames := TStringList.Create;
    HufDataArrays := TClassificationList.Create;
    LayerGroupsDataSets := TList.Create;
    SutraLayerGroupsDataSets := TList.Create;
    DataSetList:= TClassificationList.Create;
    LayerGroupList := TClassificationList.Create;
    SutraLayerGroupList := TClassificationList.Create;
    try
      frmGoPhast.PhastModel.HydrogeologicUnits.FillDataArrayNames(
        HydrogeologicUnitNames);
      HydrogeologicUnitNames.CaseSensitive := False;
      for Index := 0 to HydrogeologicUnitNames.Count - 1 do
      begin
        HufDataArrays.Add(nil);
      end;

      frmGoPhast.PhastModel.GetModflowLayerGroupDataSets(LayerGroupsDataSets);
      for Index := 0 to LayerGroupsDataSets.Count - 1 do
      begin
        LayerGroupList.Add(nil);
      end;

      frmGoPhast.PhastModel.GetSutraLayerGroupDataSets(SutraLayerGroupsDataSets);
      for Index := 0 to SutraLayerGroupsDataSets.Count - 1 do
      begin
        SutraLayerGroupList.Add(nil);
      end;

      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      for Index := 0 to DataArrayManager.DataSetCount - 1 do
      begin
        DataSet := DataArrayManager.DataSets[Index];
        if CanExportDataSet(DataSet) then
        begin
          ClassificationObject := TDataSetClassification.Create(DataSet);
          DataSetList.Add(ClassificationObject);
          Position := LayerGroupsDataSets.IndexOf(DataSet);
          if Position >= 0 then
          begin
            LayerGroupList[Position] := ClassificationObject;
          end;

          Position := SutraLayerGroupsDataSets.IndexOf(DataSet);
          if Position >= 0 then
          begin
            SutraLayerGroupList[Position] := ClassificationObject;
          end;

          Position := HydrogeologicUnitNames.IndexOf(DataSet.Name);
          if Position >= 0 then
          begin
            HufDataArrays[Position] := ClassificationObject;
          end;
        end;
      end;

      ClassifyListedObjects(ClassificationList, DataSetList,
        [LayerGroupList, SutraLayerGroupList, HufDataArrays]);

      CreateClassifiedNodes(ClassificationList, 0, tvExportItems,
        SelectedName);

      for Index := 0 to tvExportItems.Items.Count - 1 do
      begin
        Node := tvExportItems.Items[Index];
        ClassificationObject := Node.Data;
        if ClassificationObject <> nil then
        begin
          Node.Data := ClassificationObject.DataArray;
        end;
      end;
    finally
      LayerGroupList.Free;
      for Index := 0 to DataSetList.Count - 1 do
      begin
        DataSetList[Index].Free;
      end;
      SutraLayerGroupList.Free;
      SutraLayerGroupsDataSets.Free;
      DataSetList.Free;
      LayerGroupsDataSets.Free;
      HufDataArrays.Free;
      HydrogeologicUnitNames.Free;
    end;
  finally
    ClassificationList.Free;
  end;
end;

procedure TfrmExportShapefile.treeComboFilterTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmExportShapefile.treeComboFilterTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmExportShapefile.tvExportItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  procedure UpdateChildNodeStates(Node: TTreeNode);
  var
    ChildNode: TTreeNode;
  begin
    ChildNode := Node.GetFirstChild;
    while ChildNode <> nil do
    begin
      ChildNode.StateIndex := Node.StateIndex;
      UpdateChildNodeStates(ChildNode);
      ChildNode := ChildNode.getNextSibling;
    end;
  end;
begin
  inherited;
  if htOnStateIcon in tvExportItems.GetHitTestInfoAt(X, Y) then
  begin
    case tvExportItems.Selected.StateIndex of
      1:
        begin
          tvExportItems.Selected.StateIndex := 2;
        end;
      2:
        begin
          tvExportItems.Selected.StateIndex := 1;
        end;
      3:
        begin
          tvExportItems.Selected.StateIndex := 2;
        end;
      else Assert(False);
    end;
    Node := tvExportItems.Selected;
    UpdateChildNodeStates(Node);
    UpdateParentNodeStates;
    UpdateEnabledControls;
  end;
end;

procedure TfrmExportShapefile.InitializeFileNames;
var
  NewFileName: string;
begin
  NewFileName := ChangeFileExt(frmGoPhast.sdSaveDialog.FileName, '');
  try
    jfeElements.FileName := NewFileName + '_E.shp';
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  try
    jfeNodes.FileName := NewFileName + '_N.shp';
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  try
    jfeHorizontalFlowBarrier.FileName := NewFileName + '_HFB.shp';
  except on EComboEditError do
    begin
      // do nothing.
    end;

  end;
end;

procedure TfrmExportShapefile.ReadSelectedTimes;
var
  StressPeriod: TModflowStressPeriod;
  TimeItem: TTimeItem;
  Index: Integer;
  RealList: TRealList;
  SutraTimeOptions: TSutraTimeOptions;
begin
  RealList := TRealList.Create;
  try
    RealList.Sorted := True;
    case frmGoPhast.PhastModel.ModelSelection of
      msPhast:
        begin
          RealList.Add(0);
          for Index := 0 to frmGoPhast.PhastModel.Times.Count - 1 do
          begin
            TimeItem := frmGoPhast.PhastModel.Times.Items[Index] as TTimeItem;
            RealList.AddUnique(TimeItem.EndingTime);
          end;
        end;
      msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
        :
        begin
          for Index := 0 to
            frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
          begin
            StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[Index];
            RealList.AddUnique(StressPeriod.StartTime);
            RealList.AddUnique(StressPeriod.EndTime);
          end;
        end;
      msSutra22, msSutra30, msSutra40:
        begin
          SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
          SutraTimeOptions.CalculateAllTimes;
          RealList.Assign(SutraTimeOptions.AllTimes);
          RealList.Sorted := True;
        end;
      msFootPrint: ; // do nothing.
    else
      Assert(False);
    end;
    for Index := 0 to RealList.Count - 1 do
    begin
      rdgTime.Columns[0].PickList.Add(FloatToStr(RealList[Index]));
    end;
  finally
    RealList.Free;
  end;
end;

procedure TfrmExportShapefile.seTimeCountChange(Sender: TObject);
begin
  inherited;
  rdgTime.RowCount := seTimeCount.AsInteger + 1;
end;

procedure TfrmExportShapefile.GetData;
begin
  FGettingData := True;
  try
    GetModels;
    GetFilterDataSets;
    GetDataSets;
    GetBoundaryConditions;
    InitializeNodeStates;
    UpdateEnabledControls;
    InitializeFileNames;
    ReadSelectedTimes;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmExportShapefile.InitializeNodeStates;
var
  Index: Integer;
  Node: TTreeNode;
begin
  if tvExportItems.Selected <> nil then
  begin
    tvExportItems.Selected.StateIndex := 2;
  end;
  for Index := 0 to tvExportItems.Items.Count - 1 do
  begin
    Node := tvExportItems.Items[Index];
    if (not Node.HasChildren) and (Node.StateIndex <> 2) then
    begin
      Node.StateIndex := 1;
    end;
  end;
  UpdateParentNodeStates;
end;

procedure TfrmExportShapefile.rdgTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if seTimeCount.AsInteger <> rdgTime.RowCount -1 then
  begin
    seTimeCount.AsInteger := rdgTime.RowCount -1;
  end;
end;

procedure TfrmExportShapefile.ExportNodeShapes(DataSets, TimeLists: TList);
begin
  if (DataSets.Count = 0) and (TimeLists.Count = 0) then
  begin
    Exit;
  end;

  FShapeFileName := ChangeFileExt(jfeNodes.FileName, '.shp');
  if ExpandFileName(FShapeFileName) <> FShapeFileName then
  begin
    Beep;
    if (MessageDlg(Format(StrDoYouWantToSave, [ExpandFileName(FShapeFileName)]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Exit;
    end;
    FShapeFileName := ExpandFileName(FShapeFileName);
  end;

  if FileExists(FShapeFileName)
    and (MessageDlg(Format(StrSAlreadyExists, [FShapeFileName]),
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  if FLocalGrid <> nil then
  begin
    FLayerLimit := FLocalGrid.LayerCount+1;
  end
  else
  begin
    if not FLocalMesh.Is3DMesh then
    begin
      FLayerLimit := 1;
    end
    else
    begin
      FLayerLimit := FLocalMesh.LayerCount+1;
    end;
  end;

  FFields := TStringList.Create;
  FNames := TStringList.Create;
  try
    GetFieldNames(FNames, FFields, FLayerLimit, TimeLists, DataSets, eaNodes);
    // DataSets now contains TDataArrays from
    // TimeLists for all the specified times.

    FShapeDataBase := TXBase.Create(nil);
    try
      try
        InitializeDataBase(FShapeFileName, FShapeDataBase, FFields);
      except
        on E: EFOpenError do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;

      FShapeType := GetShapeType;
      if FShapeType  in [stPolygon, stPoint] then
      begin
        Export2DNodeShapes(DataSets);
      end
      else
      begin
        Export3DNodeShapes(DataSets);
      end;
    finally
      FShapeDataBase.Free;
    end;
  finally
    FNames.Free;
    FFields.Free;
  end;
end;

procedure TfrmExportShapefile.ExportElementShapes(DataSets, TimeLists: TList);
begin
  if (DataSets.Count = 0) and (TimeLists.Count = 0) then
  begin
    Exit;
  end;

  FShapeFileName := ChangeFileExt(jfeElements.FileName, '.shp');

  if ExpandFileName(FShapeFileName) <> FShapeFileName then
  begin
    Beep;
    if (MessageDlg(Format(StrDoYouWantToSave, [ExpandFileName(FShapeFileName)]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Exit;
    end;
    FShapeFileName := ExpandFileName(FShapeFileName);
  end;

  if FileExists(FShapeFileName)
    and (MessageDlg(Format(StrSAlreadyExists, [FShapeFileName]),
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  if FLocalGrid <> nil then
  begin
    FLayerLimit := FLocalGrid.LayerCount;
  end
  else
  begin
    if not FLocalMesh.Is3DMesh then
    begin
      FLayerLimit := 1;
    end
    else
    begin
      FLayerLimit := FLocalMesh.LayerCount;
    end;
  end;

  FFields := TStringList.Create;
  FNames := TStringList.Create;
  try
    GetFieldNames(FNames, FFields, FLayerLimit, TimeLists, DataSets, eaBlocks);
    // DataSets now contains TDataArrays from
    // TimeLists for all the specified times.

    FShapeDataBase := TXBase.Create(nil);
    try
      try
        InitializeDataBase(FShapeFileName, FShapeDataBase, FFields);
      except
        on E: EFOpenError do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      FShapeType := GetShapeType;
      if FShapeType  in [stPolygon, stPoint] then
      begin
        Export2DElementShapes(DataSets);
      end
      else
      begin
        Export3DElementShapes(DataSets);
      end;
    finally
      FShapeDataBase.Free;
    end;
  finally
    FFields.Free;
    FNames.Free;
  end;
end;

procedure TfrmExportShapefile.ExportHfbShapes(Edits: TList);
var
  Fields: TStringList;
  EditDisplayIndex: Integer;
  Edit: TEdgeDisplayEdit;
  Field: string;
  ShapeDataBase: TXBase;
  ShapeFileName: string;
  ShapeType: Integer;
  ShapeFileWriter: TShapefileGeometryWriter;
  EdgeDisplay: TCustomModflowGridEdgeDisplay;
  EdgeIndex: Integer;
  Edge: TCustomModflowGridEdgeFeature;
  Shape: TShapeObject;
  Names: TStringList;
  IndexFileName: string;
  ID: integer;
  Element: T3DElementCoordinates;
  StartingIndex: Integer;
  PointIndex: Integer;
  ArrayPosition: Integer;
  DBaseFileSize: Int64;
begin
  if (Edits.Count = 0) then
  begin
    Exit;
  end;

  ShapeFileName := ChangeFileExt(jfeHorizontalFlowBarrier.FileName, '.shp');;

  if ExpandFileName(ShapeFileName) <> ShapeFileName then
  begin
    Beep;
    if (MessageDlg(Format(StrDoYouWantToSave, [ExpandFileName(ShapeFileName)]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Exit;
    end;
    ShapeFileName := ExpandFileName(ShapeFileName);
  end;

  if FileExists(ShapeFileName)
    and (MessageDlg(Format(StrSAlreadyExists, [ShapeFileName]),
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  Names := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Add('COL1=N');
    Fields.Add('COL2=N');
    Fields.Add('ROW1=N');
    Fields.Add('ROW2=N');
    Fields.Add('LAYER=N');
    Fields.Add('ID=N');
    EdgeDisplay := nil;
    for EditDisplayIndex := 0 to Edits.Count - 1 do
    begin
      Edit := Edits[EditDisplayIndex];
      if EditDisplayIndex = 0 then
      begin
        EdgeDisplay := Edit.Edge;
      end
      else
      begin
        Assert(EdgeDisplay = Edit.Edge);
      end;
      Field := Edit.Edge.RealDescription[Edit.DataIndex];
      if Length(Field) > 10 then
      begin
        SetLength(Field, 10);
      end;
      Field := UpperCase(Field);
      Field := StringReplace(Field, ' ', '_', [rfReplaceAll]);
      Names.Add(Field);
      Field := Field + '=N18,10';
      Fields.Add(Field);
    end;
    EdgeDisplay.UpdateData;
    ShapeDataBase := TXBase.Create(nil);
    try
      try
        InitializeDataBase(ShapeFileName, ShapeDataBase, Fields);
      except
        on E: EFOpenError do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EXBaseException do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;

      ShapeType := stPolyLine;
      case rgHfbDimensions.ItemIndex of
        0:
          begin
            ShapeType := stPolyLine;
          end;
        1:
          begin
            ShapeType := stMultiPatch;
          end;
        else
          Assert(False);
      end;

      ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
      try
        ID := 0;
        DBaseFileSize := EdgeDisplay.Count * ShapeDataBase.RecordLength;
        if DBaseFileSize >= MaxInt then
        begin
          Beep;
          if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
          mtWarning, [mbYes, mbNo], 0) <> mrYes then
          begin
            Exit;
          end;
        end;

        for EdgeIndex := 0 to EdgeDisplay.Count - 1 do
        begin
          Inc(ID);
          Edge := EdgeDisplay[EdgeIndex];

          Shape := TShapeObject.Create;
          Shape.FShapeType := ShapeType;
          ShapeFileWriter.AddShape(Shape);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 1);
          Shape.FParts[0] := 0;

          case ShapeType of
            stPolyLine:
              begin
                SetLength(Shape.FPartTypes, 0);
                Shape.FNumPoints := 2;
                SetLength(Shape.FPoints, 2);
                Shape.FPoints[0] := ConvertPoint(Edge.StartingLocation);
                Shape.FPoints[1] := ConvertPoint(Edge.EndingLocation);
              end;
            stMultiPatch:
              begin
                SetLength(Shape.FPartTypes, 1);
                Shape.FPartTypes[0] := ptTriangleStrip;
                Shape.FNumPoints := 6;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);
                Element := FLocalGrid.ElementCoordinates[
                  Edge.Col1, Edge.Row1, Edge.Layer];
//                StartingIndex := -1;
                if Edge.Col1 = Edge.Col2 then
                begin
                  if Edge.Row1 < Edge.Row2 then
                  begin
                    StartingIndex := 4;
                  end
                  else
                  begin
                    StartingIndex := 0;
                  end;
                end
                else
                begin
                  if Edge.Col1 < Edge.Col2 then
                  begin
                    StartingIndex := 2;
                  end
                  else
                  begin
                    StartingIndex := 6;
                  end;
                  Assert(Edge.Row1 = Edge.Row2);
                end;

                for PointIndex := 0 to 2 do
                begin
                  ArrayPosition := PointIndex+StartingIndex;
                  if ArrayPosition = Length(Element.TopEdge) then
                  begin
                    ArrayPosition := 0;
                  end;
                  Shape.FPoints[PointIndex*2] :=
                    ConvertPoint(Element.TopEdge[ArrayPosition]);
                  Shape.FPoints[PointIndex*2+1] :=
                     ConvertPoint(Element.BottomEdge[ArrayPosition]);
                  Shape.FZArray[PointIndex*2] :=
                    Element.TopEdge[ArrayPosition].Z;
                  Shape.FZArray[PointIndex*2+1] :=
                     Element.BottomEdge[ArrayPosition].Z;
                  Shape.FMArray[PointIndex*2] := 0;
                  Shape.FMArray[PointIndex*2+1] := 0;
                end;
              end;
            else
              Assert(False);
          end;

          ShapeDataBase.AppendBlank;
          ShapeDataBase.UpdFieldInt('COL1', Edge.Col1+1);
          ShapeDataBase.UpdFieldInt('COL2', Edge.Col2+1);
          ShapeDataBase.UpdFieldInt('ROW1', Edge.Row1+1);
          ShapeDataBase.UpdFieldInt('ROW2', Edge.Row2+1);
          ShapeDataBase.UpdFieldInt('LAYER', Edge.Layer+1);
          ShapeDataBase.UpdFieldInt('ID', ID);
          for EditDisplayIndex := 0 to Edits.Count - 1 do
          begin
            Edit := Edits[EditDisplayIndex];
            ShapeDataBase.UpdFieldNum(AnsiString(Names[EditDisplayIndex]),
              Edge.RealValue[Edit.DataIndex]);
          end;
          ShapeDataBase.PostChanges;

        end;
        IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
        ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);

      finally
        ShapeFileWriter.Free;
      end;
    finally
      ShapeDataBase.Free;
    end;
  finally
    Fields.Free;
    Names.Free;
  end;
end;

procedure TfrmExportShapefile.Assign2DShapeGeometry(Shape: TShapeObject;
  ColIndex: Integer; RowIndex: Integer; EvaluatedAt: TEvaluatedAt);
var
  APoint: TPoint2D;
  ANode: INode2D;
  CellOutline: TVertexArray;
  PointIndex: Integer;
  AnElement: IElement2D;
  procedure EnforceClockwise(var Points: TShapePointArray);
  var
    Index: Integer;
    TempPoint: TShapePoint;
    ArrayLength: Integer;
    ShapeOrientation: Integer;
    APolygon: TPolygon2D;
    Hull: TPolygon2D;
    PointIndex: Integer;
  begin
    ArrayLength := Length(Points);
    SetLength(APolygon, ArrayLength);
    for PointIndex := 0 to ArrayLength - 1 do
    begin
      APolygon[PointIndex].x := Points[PointIndex].x;
      APolygon[PointIndex].y := Points[PointIndex].y;
    end;
    ConvexHull2(APolygon, ShapeOrientation, Hull);

    if ShapeOrientation <> Clockwise then
    begin
      for Index := 0 to (ArrayLength div 2) - 1 do
      begin
        TempPoint := Points[Index];
        Points[Index] := Points[ArrayLength-Index-1];
        Points[ArrayLength-Index-1] := TempPoint;
      end;
    end;
  end;
begin
  Shape.FMArray := nil;
  Shape.FZArray := nil;
  case Shape.FShapeType of
    stPolygon:
      begin
        Shape.FNumParts := 1;
        SetLength(Shape.FParts, 1);
        Shape.FParts[0] := 0;
        SetLength(Shape.FPartTypes, 1);
        Shape.FPartTypes[0] := ptOuterRing;

        if FLocalGrid <> nil then
        begin
          Shape.FNumPoints := 5;
          SetLength(Shape.FPoints, 5);
          case EvaluatedAt of
            eaBlocks: APoint := FLocalGrid.TwoDElementCorner(ColIndex, RowIndex);
            eaNodes: APoint := FLocalGrid.TwoDCellCorner(ColIndex, RowIndex);
            else Assert(False);
          end;

          Shape.FPoints[0] := ConvertPoint(APoint);
          Shape.FPoints[4] := Shape.FPoints[0];
          case EvaluatedAt of
            eaBlocks: APoint :=
              FLocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex + 1);
            eaNodes: APoint :=
              FLocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex + 1);
            else Assert(False);
          end;
          Shape.FPoints[2] := ConvertPoint(APoint);
          // The points must be in clockwise order.
          case FLocalGrid.RowDirection of
            rdSouthToNorth:
              begin
                case EvaluatedAt of
                  eaBlocks: APoint :=
                    FLocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex);
                  eaNodes: APoint :=
                    FLocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex);
                  else Assert(False);
                end;
                Shape.FPoints[3] := ConvertPoint(APoint);
                case EvaluatedAt of
                  eaBlocks: APoint :=
                    FLocalGrid.TwoDElementCorner(ColIndex, RowIndex + 1);
                  eaNodes: APoint :=
                    FLocalGrid.TwoDCellCorner(ColIndex, RowIndex + 1);
                  else Assert(False);
                end;
                Shape.FPoints[1] := ConvertPoint(APoint);
              end;
            rdNorthToSouth:
              begin
                case EvaluatedAt of
                  eaBlocks: APoint :=
                    FLocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex);
                  eaNodes: APoint :=
                    FLocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex);
                  else Assert(False);
                end;
                Shape.FPoints[1] := ConvertPoint(APoint);
                case EvaluatedAt of
                  eaBlocks: APoint :=
                    FLocalGrid.TwoDElementCorner(ColIndex, RowIndex + 1);
                  eaNodes: APoint :=
                    FLocalGrid.TwoDCellCorner(ColIndex, RowIndex + 1);
                  else Assert(False);
                end;
                Shape.FPoints[3] := ConvertPoint(APoint);
              end;
          else
            Assert(False);
          end;
        end
        else
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                AnElement := FLocalMesh.Mesh2DI.ElementsI2D[ColIndex];
                Shape.FNumPoints := AnElement.NodeCount + 1;
                SetLength(Shape.FPoints, Shape.FNumPoints);
//                Assert(AnElement.NodeCount = 4);
                for PointIndex := 0 to AnElement.NodeCount - 1 do
                begin
                  // The points must be in clockwise order.
                  ANode := AnElement.NodesI[
                    AnElement.NodeCount - 1 - PointIndex];
                  Shape.FPoints[PointIndex] :=
                    ConvertPoint(ANode.Location);
                end;
                Shape.FPoints[Shape.FNumPoints-1] := Shape.FPoints[0];
              end;
            eaNodes:
              begin
                // The points must be in clockwise order.
                ANode := FLocalMesh.Mesh2DI.NodesI2D[ColIndex];
                (ANode as TSutraNode2D).GetCellOutline(CellOutline);
                Shape.FNumPoints := Length(CellOutline)+1;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                for PointIndex := 0 to Length(CellOutline) - 1 do
                begin
                  Shape.FPoints[PointIndex] :=
                    ConvertPoint(CellOutline[PointIndex]);
                end;
                Shape.FPoints[Shape.FNumPoints-1] := Shape.FPoints[0];
              end;
            else Assert(False);
          end;
        end;
        EnforceClockwise(Shape.FPoints);
      end;
    stPoint:
      begin
        Shape.FNumPoints := 1;
        SetLength(Shape.FPoints, 1);
        if FLocalGrid <> nil then
        begin
          case EvaluatedAt of
            eaBlocks: APoint := FLocalGrid.TwoDElementCenter(ColIndex, RowIndex);
            eaNodes: APoint := FLocalGrid.TwoDElementCorner(ColIndex, RowIndex);
            else Assert(False);
          end;
        end
        else
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                AnElement := FLocalMesh.Mesh2DI.ElementsI2D[ColIndex];
                APoint := AnElement.Center;
              end;
            eaNodes:
              begin
                ANode := FLocalMesh.Mesh2DI.NodesI2D[ColIndex];
                APoint := ANode.Location;
              end;
            else Assert(False);
          end;
        end;
        Shape.FPoints[0] := ConvertPoint(APoint);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.Assign3DShapeGeometry(Shape: TShapeObject;
  ColIndex, RowIndex, LayerIndex: Integer; EvaluatedAt: TEvaluatedAt);
var
  APoint: T3DRealPoint;
  Cell: T3DCellCoordinates;
  Element: T3DElementCoordinates;
  MIndex: Integer;
  PointIndex: Integer;
  Node3D: INode3D;
  Element3D: IElement3D;
  Node2D: INode2D;
  CellOutline: TVertexArray;
  PIndex: integer;
  PStart: integer;
  NodeIndex: integer;
  TopNodeCount: Integer;
  NodeLocation: TPoint3D;
begin
  Shape.FMArray := nil;
  Shape.FZArray := nil;
  case Shape.FShapeType of
    stPointZ:
      begin
        Shape.FNumParts := 0;
        SetLength(Shape.FParts, 0);
        Shape.FNumPoints := 1;
        SetLength(Shape.FPoints, 1);
        SetLength(Shape.FZArray, 1);
        SetLength(Shape.FMArray, 1);
        if FLocalGrid <> nil then
        begin
          case EvaluatedAt of
            eaBlocks: APoint := FLocalGrid.RotatedThreeDElementCenter(
              ColIndex, RowIndex, LayerIndex);
            eaNodes: APoint := FLocalGrid.RotatedThreeDElementCorner(
              ColIndex, RowIndex, LayerIndex);
            else Assert(False);
          end;
        end
        else
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                Element3D := FLocalMesh.ElementArrayI[LayerIndex,ColIndex];
                APoint := Element3D.CenterLocation;
              end;
            eaNodes:
              begin
                Node3D := FLocalMesh.NodeArrayI[LayerIndex,ColIndex];
                APoint := Node3D.NodeLocation;
              end;
            else Assert(False);
          end;
        end;
        
        Shape.FPoints[0] := ConvertPoint(APoint);
        Shape.FZArray[0] := APoint.Z;
        Shape.FMArray[0] := 0;
      end;
    stMultiPatch:
      begin
        if FLocalGrid <> nil then
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                Shape.FNumParts := 3;
                SetLength(Shape.FParts, Shape.FNumParts);
                SetLength(Shape.FPartTypes, Shape.FNumParts);
                Shape.FNumPoints := 38;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);

                Shape.FParts[0] := 0;
                Shape.FPartTypes[0] := ptTriangleFan;
                Shape.FParts[1] := 10;
                Shape.FPartTypes[1] := ptTriangleFan;
                Shape.FParts[2] := 20;
                Shape.FPartTypes[2] := ptTriangleStrip;

                Element := FLocalGrid.ElementCoordinates[
                  ColIndex, RowIndex, LayerIndex];

                Shape.FPoints[0] := ConvertPoint(Element.TopCenter);
                Shape.FZArray[0] := Element.TopCenter.Z;
                for PointIndex := 0 to 7 do
                begin
                  Shape.FPoints[PointIndex+1] :=
                    ConvertPoint(Element.TopEdge[PointIndex]);
                  Shape.FZArray[PointIndex+1] := Element.TopEdge[PointIndex].Z;
                end;
                Shape.FPoints[9] := ConvertPoint(Element.TopEdge[0]);
                Shape.FZArray[9] := Element.TopEdge[0].Z;

                Shape.FPoints[10] := ConvertPoint(Element.BottomCenter);
                Shape.FZArray[10] := Element.BottomCenter.Z;
                for PointIndex := 0 to 7 do
                begin
                  Shape.FPoints[PointIndex+11] :=
                    ConvertPoint(Element.BottomEdge[PointIndex]);
                  Shape.FZArray[PointIndex+11] :=
                    Element.BottomEdge[PointIndex].Z;
                end;
                Shape.FPoints[19] := ConvertPoint(Element.BottomEdge[0]);
                Shape.FZArray[19] := Element.BottomEdge[0].Z;

                for PointIndex := 0 to 7 do
                begin
                  Shape.FPoints[PointIndex*2+20] :=
                    ConvertPoint(Element.TopEdge[PointIndex]);
                  Shape.FZArray[PointIndex*2+20] :=
                    Element.TopEdge[PointIndex].Z;
                  Shape.FPoints[PointIndex*2+21] :=
                    ConvertPoint(Element.BottomEdge[PointIndex]);
                  Shape.FZArray[PointIndex*2+21] :=
                    Element.BottomEdge[PointIndex].Z;
                end;
                Shape.FPoints[36] := ConvertPoint(Element.TopEdge[0]);
                Shape.FZArray[36] := Element.TopEdge[0].Z;
                Shape.FPoints[37] := ConvertPoint(Element.BottomEdge[0]);
                Shape.FZArray[37] := Element.BottomEdge[0].Z;
              end;
            eaNodes:
              begin
                Shape.FNumParts := 1;
                SetLength(Shape.FParts, 1);
                SetLength(Shape.FPartTypes, 1);
                Shape.FNumPoints := 14;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);

                Shape.FParts[0] := 0;
                Shape.FPartTypes[0] := ptTriangleStrip;

                Cell := FLocalGrid.CellCoordinates[ColIndex, RowIndex, LayerIndex];

                Shape.FPoints[0] := ConvertPoint(Cell.Col1_Row1_Lay1);
                Shape.FZArray[0] := Cell.Col1_Row1_Lay1.Z;

                Shape.FPoints[1] := ConvertPoint(Cell.Col2_Row1_Lay1);
                Shape.FZArray[1] := Cell.Col2_Row1_Lay1.Z;

                Shape.FPoints[2] := ConvertPoint(Cell.Col1_Row2_Lay1);
                Shape.FZArray[2] := Cell.Col1_Row2_Lay1.Z;

                Shape.FPoints[3] := ConvertPoint(Cell.Col2_Row2_Lay1);
                Shape.FZArray[3] := Cell.Col2_Row2_Lay1.Z;

                Shape.FPoints[4] := ConvertPoint(Cell.Col2_Row2_Lay2);
                Shape.FZArray[4] := Cell.Col2_Row2_Lay2.Z;

                Shape.FPoints[5] := ConvertPoint(Cell.Col2_Row1_Lay1);
                Shape.FZArray[5] := Cell.Col2_Row1_Lay1.Z;

                Shape.FPoints[6] := ConvertPoint(Cell.Col2_Row1_Lay2);
                Shape.FZArray[6] := Cell.Col2_Row1_Lay2.Z;

                Shape.FPoints[7] := ConvertPoint(Cell.Col1_Row1_Lay1);
                Shape.FZArray[7] := Cell.Col1_Row1_Lay1.Z;

                Shape.FPoints[8] := ConvertPoint(Cell.Col1_Row1_Lay2);
                Shape.FZArray[8] := Cell.Col1_Row1_Lay2.Z;

                Shape.FPoints[9] := ConvertPoint(Cell.Col1_Row2_Lay1);
                Shape.FZArray[9] := Cell.Col1_Row2_Lay1.Z;

                Shape.FPoints[10] := ConvertPoint(Cell.Col1_Row2_Lay2);
                Shape.FZArray[10] := Cell.Col1_Row2_Lay2.Z;

                Shape.FPoints[11] := ConvertPoint(Cell.Col2_Row2_Lay2);
                Shape.FZArray[11] := Cell.Col2_Row2_Lay2.Z;

                Shape.FPoints[12] := ConvertPoint(Cell.Col1_Row1_Lay2);
                Shape.FZArray[12] := Cell.Col1_Row1_Lay2.Z;

                Shape.FPoints[13] := ConvertPoint(Cell.Col2_Row1_Lay2);
                Shape.FZArray[13] := Cell.Col2_Row1_Lay2.Z;
              end;
            else
              Assert(False);
          end;
        end
        else
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                Element3D := FLocalMesh.ElementArrayI[LayerIndex, ColIndex];
                TopNodeCount := Element3D.NodeCount div 2;

                Shape.FNumParts := 3;
                SetLength(Shape.FParts, Shape.FNumParts);
                SetLength(Shape.FPartTypes, Shape.FNumParts);
                Shape.FNumPoints := TopNodeCount * 4 + 2; //18;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);

                Shape.FParts[0] := 0;
                Shape.FPartTypes[0] := ptTriangleFan;
//                Shape.FParts[1] := 5;
                Shape.FPartTypes[1] := ptTriangleFan;
//                Shape.FParts[2] := 10;
                Shape.FPartTypes[2] := ptTriangleStrip;

                PIndex := 0;
                for NodeIndex := 0 to TopNodeCount -1 do
                begin
//                  Node3D := Element3D.NodesI[NodeIndex];
                  NodeLocation := Element3D.NodeLocation[NodeIndex];
                  Shape.FPoints[PIndex] := ConvertPoint(NodeLocation);
                  Shape.FZArray[PIndex] := NodeLocation.Z;
                  Inc(PIndex);
                end;
                Shape.FParts[1] := PIndex;

                for NodeIndex := TopNodeCount to Element3D.NodeCount -1 do
                begin
//                  Node3D := Element3D.NodesI[NodeIndex];
                  NodeLocation := Element3D.NodeLocation[NodeIndex];
                  Shape.FPoints[PIndex] := ConvertPoint(NodeLocation);
                  Shape.FZArray[PIndex] := NodeLocation.Z;
                  Inc(PIndex);
                end;
                Shape.FParts[2] := PIndex;

                PStart := PIndex;
                for NodeIndex := 0 to TopNodeCount -1 do
                begin
//                  Node3D := Element3D.NodesI[NodeIndex];
                  NodeLocation := Element3D.NodeLocation[NodeIndex];
                  Shape.FPoints[PIndex] := ConvertPoint(NodeLocation);
                  Shape.FZArray[PIndex] := NodeLocation.Z;
                  Inc(PIndex);

//                  Node3D := Element3D.NodesI[NodeIndex+TopNodeCount];
                  NodeLocation := Element3D.NodeLocation[NodeIndex+TopNodeCount];
                  Shape.FPoints[PIndex] := ConvertPoint(NodeLocation);
                  Shape.FZArray[PIndex] := NodeLocation.Z;
                  Inc(PIndex);
                end;
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);
                Inc(PStart);
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);

                Assert(PIndex = Length(Shape.FPoints));
              end;
            eaNodes:
              begin
                { TODO -cSUTRA : The node may also be a point on the cell outline. If this is the case, maybe that location should be removed from the cell outline. }
                Shape.FNumParts := 3;
                SetLength(Shape.FParts, Shape.FNumParts);
                SetLength(Shape.FPartTypes, Shape.FNumParts);

                Node2D := FLocalMesh.Mesh2DI.NodesI2D[ColIndex];
                (Node2D as TSutraNode2D).GetCellOutline(CellOutline);

                Shape.FNumPoints := (Length(CellOutline)+1)*4+2;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);

                Shape.FParts[0] := 0;
                Shape.FPartTypes[0] := ptTriangleFan;
//                Shape.FParts[1] := Length(CellOutline)+2;
                Shape.FPartTypes[1] := ptTriangleFan;
//                Shape.FParts[2] := (Length(CellOutline)+1)*2+1;
                Shape.FPartTypes[2] := ptTriangleStrip;

                Node3D := FLocalMesh.NodeArrayI[LayerIndex,ColIndex];

                PIndex := 0;
                Shape.FPoints[PIndex] := ConvertPoint(Node2D.Location);
                Shape.FZArray[PIndex] := Node3D.Top;
                Inc(PIndex);
                PStart := PIndex;
                for PointIndex := 0 to Length(CellOutline)-1 do
                begin
                  Shape.FPoints[PIndex] :=
                    ConvertPoint(CellOutline[PointIndex]);
                  Shape.FZArray[PIndex] := Node3D.Top;
                  Inc(PIndex);
                end;
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);
                Shape.FParts[1] := PIndex;

                Shape.FPoints[PIndex] := ConvertPoint(Node2D.Location);
                Shape.FZArray[PIndex] := Node3D.Bottom;
                Inc(PIndex);
                PStart := PIndex;
                for PointIndex := 0 to Length(CellOutline)-1 do
                begin
                  Shape.FPoints[PIndex] :=
                    ConvertPoint(CellOutline[PointIndex]);
                  Shape.FZArray[PIndex] := Node3D.Bottom;
                  Inc(PIndex);
                end;
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);
                Shape.FParts[2] := PIndex;

                PStart := PIndex;
                for PointIndex := 0 to Length(CellOutline)-1 do
                begin
                  Shape.FPoints[PIndex] :=
                    ConvertPoint(CellOutline[PointIndex]);
                  Shape.FZArray[PIndex] := Node3D.Top;
                  Inc(PIndex);

                  Shape.FPoints[PIndex] :=
                    ConvertPoint(CellOutline[PointIndex]);
                  Shape.FZArray[PIndex] := Node3D.Bottom;
                  Inc(PIndex);
                end;
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);
                Inc(PStart);
                Shape.FPoints[PIndex] := Shape.FPoints[PStart];
                Shape.FZArray[PIndex] := Shape.FZArray[PStart];
                Inc(PIndex);

                Assert(PIndex = Length(Shape.FPoints));

              end;
            else Assert(False);
          end;
        end;
        for MIndex := 0 to Length(Shape.FMArray) - 1 do
        begin
          Shape.FMArray[MIndex] := 0;
        end;
      end;
  else
    Assert(False);
  end;
end;

function TfrmExportShapefile.GetShapeType: Integer;
begin
  result := stNull;
//   TExportShapeChoice = (escTwoDPoly, escTwoDPoint,escThreeDPoly, escThreeDPoint);

  case TExportShapeChoice(rgExportObjectType.ItemIndex) of
    escTwoDPoly:
      result := stPolygon;
    escTwoDPoint:
      result := stPoint;
    escThreeDPoly:
      result := stMultiPatch;
    escThreeDPoint:
      result := stPointZ;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.GetFieldNames(Names, Fields: TStringList;
  LayerLimit: Integer; TimeLists, DataSets: TList; EvaluatedAt: TEvaluatedAt);
var
  TimeRoot: AnsiString;
  DataSetIndex: Integer;
  TimeValue: Double;
  TimeList: TCustomTimeList;
  TimeListIndex: Integer;
  TimeCharacters: Integer;
  TimeIndex: Integer;
  RealList: TRealList;
  FieldName: AnsiString;
  LayerIndex: Integer;
  FieldFormat: AnsiString;
  RootName: AnsiString;
  DataArray: TDataArray;
  Index: Integer;
  LayerCharacters: Integer;
  SuffixInt: integer;
  SuffixStr: AnsiString;
  Value: Extended;
begin
  FShapeType := GetShapeType;

  if FLocalGrid <> nil then
  begin
    Fields.Add('COLUMN=N');
    Fields.Add('ROW=N');
  end
  else if FDisvUsed then
  begin
    Fields.Add('CELL=N');
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks: Fields.Add('ELEMENT=N');
      eaNodes: Fields.Add('NODE=N');
      else Assert(False);
    end;
  end;
  if (FShapeType  in [stPolygon, stPoint]) then
  begin
    if FLocalGrid <> nil then
    begin
      LayerCharacters := Trunc(Log10(FLocalGrid.LayerCount + 1)) + 1;
    end
    else
    begin
      if not FLocalMesh.Is3DMesh then
      begin
        LayerCharacters := 0;
      end
      else
      begin
        LayerCharacters := Trunc(Log10(FLocalMesh.LayerCount + 1)) + 1;
      end;
    end;
  end
  else
  begin
    Fields.Add('LAYER=N');
    LayerCharacters := 0;
  end;
  Fields.Add('ID=N');
//  LocalGrid := frmGoPhast.Grid;
  for Index := 0 to DataSets.Count - 1 do
  begin
    DataArray := DataSets[Index];
    DataArray.Initialize;
    DataArray.CacheData;
    frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    RootName := AnsiString(UpperCase(DataArray.Name));
    case DataArray.Orientation of
      dsoTop:
        begin
          if Length(RootName) > 10 then
          begin
            SetLength(RootName, 10);
          end;

          SuffixInt := 1;
          while Names.IndexOf(string(RootName)) >= 0 do
          begin
            SuffixStr := AnsiString(IntToStr(SuffixInt));
            Inc(SuffixInt);
            RootName := Copy(RootName, 1, 10-Length(SuffixStr))
              + SuffixStr;
          end;

          case DataArray.DataType of
            rdtDouble:
              FieldFormat := 'N18,10';
            rdtInteger:
              FieldFormat := 'N';
            rdtBoolean:
              FieldFormat := 'N';
            rdtString:
              FieldFormat := 'C18';
            else
              Assert(False);
          end;
          FieldName := RootName;
          FieldName := FixShapeFileFieldName(FieldName, Names);
          Names.AddObject(string(FieldName), DataArray);
          Fields.AddObject(string(FieldName + '=' + FieldFormat), DataArray);
        end;
      dsoFront, dsoSide, dso3D:
        begin
          if Length(RootName) > 10 then
          begin
            SetLength(RootName, 10);
          end;
          if LayerCharacters > 0 then
          begin
            if Length(RootName) > 9 - LayerCharacters then
            begin
              SetLength(RootName, 9 - LayerCharacters);
            end;
            SuffixInt := 1;
            while Names.IndexOf(string(RootName)) >= 0 do
            begin
              SuffixStr := AnsiString(IntToStr(SuffixInt));
              Inc(SuffixInt);
              RootName := AnsiString(Copy(string(RootName), 1,
                9 - LayerCharacters-Length(SuffixStr)))
                + SuffixStr;
            end;
          end;
          case DataArray.DataType of
            rdtDouble:
              FieldFormat := 'N18,10';
            rdtInteger:
              FieldFormat := 'N';
            rdtBoolean:
              FieldFormat := 'N';
            rdtString:
              FieldFormat := 'C18';
          end;
          if LayerCharacters = 0 then
          begin
            FieldName := RootName;
            FieldName := FixShapeFileFieldName(FieldName, Names);
            Names.AddObject(string(FieldName), DataArray);
            Fields.AddObject(String(FieldName + '=' + FieldFormat), DataArray);
          end
          else
          begin
            for LayerIndex := 1 to LayerLimit do
            begin
              FieldName := RootName + 'L' + AnsiString(IntToStr(LayerIndex));
              FieldName := FixShapeFileFieldName(FieldName, Names);
              Names.AddObject(string(FieldName), DataArray);
              Fields.AddObject(String(FieldName + '=' + FieldFormat), DataArray);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
  if TimeLists.Count > 0 then
  begin
    RealList := TRealList.Create;
    try
      for TimeIndex := 1 to rdgTime.RowCount - 1 do
      begin
        if TryStrToFloat(rdgTime.Cells[0, TimeIndex], Value) then
        begin
          RealList.Add(Value);
        end;
      end;
      TimeCharacters := Trunc(Log10(RealList.Count)) + 1;
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[TimeListIndex];
        frmProgressMM.ShouldContinue := True;
        TimeList.Initialize;
      end;
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[TimeListIndex];
        if TimeList.Count = 0 then
        begin
          Continue;
        end;
        for TimeIndex := 0 to RealList.Count - 1 do
        begin
          TimeValue := RealList[TimeIndex];
          DataSetIndex := TimeList.FirstTimeGreaterThan(TimeValue) - 1;
          if DataSetIndex >= 0 then
          begin
            DataArray := TimeList.Items[DataSetIndex];
            DataSets.Add(DataArray);
            RootName := AnsiString(UpperCase(TimeList.Name));
            RootName := AnsiString(StringReplace(string(RootName),
              ' ', '_', [rfReplaceAll]));
            if Length(RootName) > 8 - LayerCharacters - TimeCharacters then
            begin
              SetLength(RootName, 8 - LayerCharacters - TimeCharacters);
            end;
            TimeRoot := RootName + 'T' + AnsiString(IntToStr(TimeIndex + 1));
            case DataArray.DataType of
              rdtDouble:
                FieldFormat := 'N18,10';
              rdtInteger:
                FieldFormat := 'N';
              rdtBoolean:
                FieldFormat := 'N';
              rdtString:
                FieldFormat := 'C18';
            end;
            if LayerCharacters = 0 then
            begin
              FieldName := TimeRoot;
              FieldName := FixShapeFileFieldName(FieldName, Fields);
              Names.AddObject(string(FieldName), DataArray);
              Fields.AddObject(string(FieldName + '=' + FieldFormat), DataArray);
            end
            else
            begin
              for LayerIndex := 1 to LayerLimit do
              begin
                FieldName := TimeRoot + 'L' + AnsiString(IntToStr(LayerIndex));
                FieldName := FixShapeFileFieldName(FieldName, Fields);
                Names.AddObject(string(FieldName), DataArray);
                Fields.AddObject(string(FieldName + '='
                  + FieldFormat), DataArray);
              end;
            end;
          end;
        end;
      end;
    finally
      RealList.Free;
    end;
  end;
end;

procedure InitializeDataBase(const ShapeFileName: string;
  ShapeDataBase: TXBase; Fields: TStringList);
var
  DataBaseFileName: string;
begin
  DataBaseFileName := ChangeFileExt(ShapeFileName, '.dbf');
  if FileExists(DataBaseFileName) then
  begin
    DeleteFile(DataBaseFileName);
  end;
  ShapeDataBase.DBFCreate(DataBaseFileName, Fields);
  ShapeDataBase.FileName := DataBaseFileName;
  ShapeDataBase.Active := True;
  ShapeDataBase.GotoBOF;
end;

procedure TfrmExportShapefile.Assign3DDataSetValuesToDataBase(DataSets: TList;
  DataSetIndex: Integer; Names: TStringList; ColIndex,
  RowIndex, LayerIndex: integer; ShapeDataBase: TXBase);
var
  DataArray: TDataArray;
  RootName: AnsiString;
  Col: Integer;
  Row: Integer;
  Layer: Integer;
  FieldName: AnsiString;
begin
  DataArray := DataSets[DataSetIndex];
  RootName := AnsiString(Names[DataSetIndex]);
  Col := ColIndex;
  Row := RowIndex;
  Layer := LayerIndex;
  case DataArray.Orientation of
    dsoTop: Layer := 0;
    dsoFront: Row := 0;
    dsoSide: Col := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;
  FieldName := RootName;
  if DataArray.IsValue[Layer, Row, Col] then
  begin
    case DataArray.DataType of
      rdtDouble:
        begin
          ShapeDataBase.UpdFieldNum(FieldName,
            DataArray.RealData[Layer, Row, Col]);
        end;
      rdtInteger:
        begin
          ShapeDataBase.UpdFieldInt(FieldName,
            DataArray.IntegerData[Layer, Row, Col]);
        end;
      rdtBoolean:
        begin
          if DataArray.BooleanData[Layer, Row, Col] then
          begin
            ShapeDataBase.UpdFieldInt(FieldName, 1);
          end
          else
          begin
            ShapeDataBase.UpdFieldInt(FieldName, 0);
          end;
        end;
      rdtString:
        begin
          ShapeDataBase.UpdFieldStr(FieldName,
            AnsiString(DataArray.StringData[Layer, Row, Col]));
        end;
      else Assert(False);
    end;
  end;
end;

procedure TfrmExportShapefile.Export3DElementShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  LayerIndex: Integer;
  Element3D: IElement3D;
  LayerCount: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
  RecordCount: Int64;
  DBaseFileSize: Int64;
  function LocationOK: boolean;
  var
    LIndex: Integer;
  begin
    Result := (FFilterArray = nil) or (FFilterArray.EvaluatedAt = eaNodes);
    if not result then
    begin
      if FFilterArray.Orientation = dsoTop then
      begin
        LIndex := 0;
      end
      else
      begin
        LIndex := LayerIndex;
      end;
      result := FFilterArray.BooleanData[LIndex, RowIndex, ColIndex];
    end;
  end;
begin
  FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
  try
    ID := 0;
    if FLocalGrid <> nil then
    begin
      FShapeFileWriter.Capacity := FLocalGrid.RowCount * FLocalGrid.ColumnCount
        * FLocalGrid.LayerCount;
      Assert(FLocalGrid.ColumnDirection = cdWestToEast);
      RecordCount := 0;
      for LayerIndex := 0 to FLocalGrid.LayerCount - 1 do
      begin
        for RowIndex := 0 to FLocalGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
          begin
            if LocationOK then
            begin
              Inc(RecordCount);
            end;
          end;
        end;
      end;
      DBaseFileSize := RecordCount * FShapeDataBase.RecordLength;
      if DBaseFileSize >= MaxInt then
      begin
        Beep;
        if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
      for LayerIndex := 0 to FLocalGrid.LayerCount - 1 do
      begin
        for RowIndex := 0 to FLocalGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
          begin
            if LocationOK then
            begin
              Inc(ID);
              Shape := TShapeObject.Create;
              Shape.FShapeType := FShapeType;
              FShapeFileWriter.AddShape(Shape);
              Assign3DShapeGeometry(Shape, ColIndex, RowIndex, LayerIndex, eaBlocks);
              Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex, FShapeDataBase, eaBlocks);
              FShapeDataBase.PostChanges;
            end;
          end;
        end;
      end;
    end
    else
    begin
      RowIndex := 0;
      FShapeFileWriter.Capacity := FLocalMesh.ActiveElementCount;
      for LayerIndex := 0 to FLocalMesh.LayerCount - 1 do
      begin
        for ColIndex := 0 to FLocalMesh.Mesh2DI.ElementCount - 1 do
        begin
          Element3D := FLocalMesh.ElementArrayI[LayerIndex, ColIndex];
          if not Element3D.Active then
          begin
            Continue;
          end;
          if LocationOK then
          begin
            Inc(ID);
            Shape := TShapeObject.Create;
            Shape.FShapeType := FShapeType;
            FShapeFileWriter.AddShape(Shape);
            Assign3DShapeGeometry(Shape, ColIndex, 0, LayerIndex, eaBlocks);
            Assign3DID_Fields(ID, Element3D.ElementNumber, 0, 0, FShapeDataBase, eaBlocks);
            FShapeDataBase.PostChanges;
          end;
        end;
      end;
    end;

    if FLocalGrid <> nil then
    begin
      LayerCount := FLocalGrid.LayerCount;
      RowCount := FLocalGrid.RowCount;
      ColumnCount := FLocalGrid.ColumnCount;
    end
    else
    begin
      LayerCount := FLocalMesh.LayerCount;
      RowCount := 1;
      ColumnCount := FLocalMesh.Mesh2DI.ElementCount
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      FShapeDataBase.GotoBOF;
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        for RowIndex := 0 to RowCount - 1 do
        begin
          for ColIndex := 0 to ColumnCount - 1 do
          begin
            if FLocalMesh <> nil then
            begin
              Element3D := FLocalMesh.ElementArrayI[LayerIndex, ColIndex];
              if not Element3D.Active then
              begin
                Continue;
              end;
            end;
            if LocationOK then
            begin
              Assign3DDataSetValuesToDataBase(DataSets, DataSetIndex, FNames,
                ColIndex, RowIndex, LayerIndex, FShapeDataBase);
              FShapeDataBase.PostChanges;
              if (RowIndex < RowCount - 1)
                or (ColIndex < ColumnCount - 1)
                or (LayerIndex < LayerCount - 1) then
              begin
                FShapeDataBase.GotoNext;
              end;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(FShapeFileName, '.shx');
    FShapeFileWriter.WriteToFile(FShapeFileName, IndexFileName);
  finally
    FShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.GetModels;
var
  ChildModel: TChildModel;
  ChildIndex: Integer;
begin
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowNWT
      , msModflowCfp, msSutra22, msSutra30, msSutra40, msFootPrint, msModflow2015:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel);
        comboModel.Visible := False;
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
          comboModel.Items.AddObject(ChildModel.ModelName, ChildModel);
        end;
        comboModel.Visible := True;
      end;
  else
    Assert(False);
  end;
  comboModel.ItemIndex := comboModel.Items.IndexOfObject(
    frmGoPhast.PhastModel.SelectedModel);
end;


procedure TfrmExportShapefile.Export2DElementShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  ElementIndex: Integer;
  RowCount: integer;
  ColumnCount: integer;
  NameIndexStart: Integer;
  RecordCount: Int64;
  DBaseFileSize: Int64;
  function LocationOK: boolean;
  begin
    Result := (FFilterArray = nil) or (FFilterArray.EvaluatedAt = eaNodes);
    if not result then
    begin
      result := FFilterArray.BooleanData[0, RowIndex, ColIndex];
    end;
  end;
begin
  FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
  try
    ID := 0;
    if FLocalGrid <> nil then
    begin
      FShapeFileWriter.Capacity := FLocalGrid.RowCount * FLocalGrid.ColumnCount;
      Assert(FLocalGrid.ColumnDirection = cdWestToEast);
      RecordCount := 0;
      for RowIndex := 0 to FLocalGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
        begin
          if LocationOK then
          begin
            Inc(RecordCount);
          end;
        end;
      end;
      DBaseFileSize := RecordCount * FShapeDataBase.RecordLength;
      if DBaseFileSize >= MaxInt then
      begin
        Beep;
        if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
          mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
      for RowIndex := 0 to FLocalGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to FLocalGrid.ColumnCount - 1 do
        begin
          if LocationOK then
          begin
            Inc(ID);
            Shape := TShapeObject.Create;
            Shape.FShapeType := FShapeType;
            FShapeFileWriter.AddShape(Shape);
            Assign2DShapeGeometry(Shape, ColIndex, RowIndex, eaBlocks);
            Assign2DID_Fields(ID, ColIndex, RowIndex, FShapeDataBase, eaBlocks);
            FShapeDataBase.PostChanges;
          end;
        end;
      end;
    end
    else
    begin
      RowIndex := 0;
      FShapeFileWriter.Capacity := FLocalMesh.Mesh2DI.ElementCount;
      for ElementIndex := 0 to FLocalMesh.Mesh2DI.ElementCount - 1 do
      begin
        ColIndex := ElementIndex;
        if LocationOK then
        begin
          Inc(ID);
          Shape := TShapeObject.Create;
          Shape.FShapeType := FShapeType;
          FShapeFileWriter.AddShape(Shape);
          Assign2DShapeGeometry(Shape, ElementIndex, 0, eaBlocks);
          Assign2DID_Fields(ID, ElementIndex, 0, FShapeDataBase, eaBlocks);
          FShapeDataBase.PostChanges;
        end;
      end;
    end;
    if FLocalGrid <> nil then
    begin
      RowCount := FLocalGrid.RowCount;
      ColumnCount := FLocalGrid.ColumnCount;
    end
    else
    begin
      RowCount := 1;
      ColumnCount := FLocalMesh.Mesh2DI.ElementCount;
    end;
    NameIndexStart := 0;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      FShapeDataBase.GotoBOF;
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          if LocationOK then
          begin
            Assign2DDataSetValuesToDataBase(DataSets, DataSetIndex, FNames,
              FLayerLimit, ColIndex, RowIndex, FShapeDataBase, NameIndexStart);
            FShapeDataBase.PostChanges;
            if (RowIndex < RowCount - 1)
              or (ColIndex < ColumnCount - 1) then
            begin
              FShapeDataBase.GotoNext;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      NameIndexStart := NameIndexStart + DataArray.LayerCount;
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(FShapeFileName, '.shx');
    FShapeFileWriter.WriteToFile(FShapeFileName, IndexFileName);
  finally
    FShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.Assign2DDataSetValuesToDataBase(DataSets: TList;
  DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
  RowIndex: integer; ShapeDataBase: TXBase; NameIndexStart: integer);
var
  DataArray: TDataArray;
  RootName: AnsiString;
  LayerIndex: Integer;
  Col: Integer;
  Row: Integer;
  Layer: Integer;
  FieldName: AnsiString;
begin
  DataArray := DataSets[DataSetIndex];
  RootName := AnsiString(Names[DataSetIndex]);
  if DataArray.Orientation = dsoTop then
  begin
    LayerLimit := 1;
  end;
  for LayerIndex := 0 to LayerLimit - 1 do
  begin
    Col := ColIndex;
    Row := RowIndex;
    Layer := LayerIndex;
    case DataArray.Orientation of
      dsoTop: Layer := 0;
      dsoFront: Row := 0;
      dsoSide: Col := 0;
      dso3D: ; // do nothing
      else Assert(False);
    end;
//    if (FLocalMesh <> nil) and (FLocalMesh.MeshType in [mt2D, mtProfile]) then
//    begin
//      FieldName := RootName;
//    end
//    else if DataArray.Orientation = dsoTop then
//    begin
//      FieldName := RootName;
//    end
//    else
//    begin
//      FieldName := RootName + 'L' + AnsiString(IntToStr(Layer+1));
//    end;
//    FieldName := FixShapeFileFieldName(FieldName);
    FieldName := AnsiString(FNames[NameIndexStart+LayerIndex]);
    if DataArray.IsValue[Layer, Row, Col] then
    begin
      case DataArray.DataType of
        rdtDouble:
          begin
            ShapeDataBase.UpdFieldNum(FieldName,
              DataArray.RealData[Layer, Row, Col]);
          end;
        rdtInteger:
          begin
            ShapeDataBase.UpdFieldInt(FieldName,
              DataArray.IntegerData[Layer, Row, Col]);
          end;
        rdtBoolean:
          begin
            if DataArray.BooleanData[Layer, Row, Col] then
            begin
              ShapeDataBase.UpdFieldInt(FieldName, 1);
            end
            else
            begin
              ShapeDataBase.UpdFieldInt(FieldName, 0);
            end;
          end;
        rdtString:
          begin
            ShapeDataBase.UpdFieldStr(FieldName,
              AnsiString(DataArray.StringData[Layer, Row, Col]));
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TfrmExportShapefile.Export3DNodeShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  LayerIndex: Integer;
  ANode3D: INode3D;
  LayerCount: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
  RecordCount: Int64;
  DBaseFileSize: Int64;
  function LocationOK: boolean;
  var
    LIndex: Integer;
  begin
    Result := (FFilterArray = nil) or (FFilterArray.EvaluatedAt = eaBlocks);
    if not result then
    begin
      if FFilterArray.Orientation = dsoTop then
      begin
        LIndex := 0;
      end
      else
      begin
        LIndex := LayerIndex;
      end;
      result := FFilterArray.BooleanData[LIndex, RowIndex, ColIndex];
    end;
  end;
begin
  FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
  try
    ID := 0;
    if FLocalGrid <> nil then
    begin
      FShapeFileWriter.Capacity := (FLocalGrid.RowCount + 1)
        * (FLocalGrid.ColumnCount + 1) * (FLocalGrid.LayerCount + 1);
      Assert(FLocalGrid.ColumnDirection = cdWestToEast);
      RecordCount := 0;
      for LayerIndex := 0 to FLocalGrid.LayerCount do
      begin
        for RowIndex := 0 to FLocalGrid.RowCount do
        begin
          for ColIndex := 0 to FLocalGrid.ColumnCount do
          begin
            if LocationOK then
            begin
              Inc(RecordCount);
            end;
          end;
        end;
      end;
      DBaseFileSize := RecordCount * FShapeDataBase.RecordLength;
      if DBaseFileSize >= MaxInt then
      begin
        Beep;
        if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
      for LayerIndex := 0 to FLocalGrid.LayerCount do
      begin
        for RowIndex := 0 to FLocalGrid.RowCount do
        begin
          for ColIndex := 0 to FLocalGrid.ColumnCount do
          begin
            if LocationOK then
            begin
              Inc(ID);
              Shape := TShapeObject.Create;
              Shape.FShapeType := FShapeType;
              FShapeFileWriter.AddShape(Shape);
              Assign3DShapeGeometry(Shape, ColIndex, RowIndex, LayerIndex, eaNodes);
              Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex, FShapeDataBase, eaNodes);
              FShapeDataBase.PostChanges;
            end;
          end;
          //            ShapeDataBase.GotoNext;
        end;
      end;
    end
    else
    begin
      FShapeFileWriter.Capacity := FLocalMesh.ActiveNodeCount;
      RowIndex := 0;
      for ColIndex := 0 to FLocalMesh.Mesh2DI.NodeCount - 1 do
      begin
        for LayerIndex := 0 to FLocalMesh.LayerCount - 1 do
        begin
          ANode3D := FLocalMesh.NodeArrayI[LayerIndex, ColIndex];
          if ANode3D.Active then
          begin
            if LocationOK then
            begin
              Inc(ID);
              Shape := TShapeObject.Create;
              Shape.FShapeType := FShapeType;
              FShapeFileWriter.AddShape(Shape);
              Assign3DShapeGeometry(Shape, ColIndex, 0, LayerIndex, eaNodes);
              Assign3DID_Fields(ID, ANode3D.NodeNumber, 0, 0, FShapeDataBase, eaNodes);
              FShapeDataBase.PostChanges;
            end;
          end;
        end;
      end;
    end;
    if FLocalGrid <> nil then
    begin
      LayerCount := FLocalGrid.LayerCount;
      RowCount := FLocalGrid.RowCount;
      ColumnCount := FLocalGrid.ColumnCount;
    end
    else
    begin
      LayerCount := FLocalMesh.LayerCount;
      RowCount := 0;
      ColumnCount := FLocalMesh.Mesh2DI.NodeCount-1;
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      FShapeDataBase.GotoBOF;
      for LayerIndex := 0 to LayerCount do
      begin
        for RowIndex := 0 to RowCount do
        begin
          for ColIndex := 0 to ColumnCount do
          begin
            if FLocalMesh <> nil then
            begin
              ANode3D := FLocalMesh.NodeArrayI[LayerIndex, ColIndex];
              if not ANode3D.Active then
              begin
                Continue;
              end;
            end;
            if LocationOK then
            begin
              Assign3DDataSetValuesToDataBase(DataSets, DataSetIndex,
                FNames, ColIndex, RowIndex, LayerIndex, FShapeDataBase);
              FShapeDataBase.PostChanges;
              if (RowIndex < RowCount)
                or (ColIndex < ColumnCount)
                or (LayerIndex < LayerCount) then
              begin
                FShapeDataBase.GotoNext;
              end;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(FShapeFileName, '.shx');
    FShapeFileWriter.WriteToFile(FShapeFileName, IndexFileName);
  finally
    FShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.Export2DNodeShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  NodeIndex: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
  NameIndexStart: Integer;
  RecordCount: Int64;
  DBaseFileSize: Int64;
  function LocationOK: boolean;
  begin
    Result := (FFilterArray = nil) or (FFilterArray.EvaluatedAt = eaBlocks);
    if not result then
    begin
      result := FFilterArray.BooleanData[0, RowIndex, ColIndex];
    end;
  end;
begin
  FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
  try
    ID := 0;
    if FLocalGrid <> nil then
    begin
      FShapeFileWriter.Capacity := (FLocalGrid.RowCount + 1)
        * (FLocalGrid.ColumnCount + 1);
      Assert(FLocalGrid.ColumnDirection = cdWestToEast);

      RecordCount := 0;
      for RowIndex := 0 to FLocalGrid.RowCount do
      begin
        for ColIndex := 0 to FLocalGrid.ColumnCount do
        begin
          if LocationOK then
          begin
            Inc(RecordCount);
          end;
        end;
      end;
      DBaseFileSize := RecordCount * FShapeDataBase.RecordLength;
      if DBaseFileSize >= MaxInt then
      begin
        Beep;
        if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
      for RowIndex := 0 to FLocalGrid.RowCount do
      begin
        for ColIndex := 0 to FLocalGrid.ColumnCount do
        begin
          if LocationOK then
          begin
            Inc(ID);
            Shape := TShapeObject.Create;
            Shape.FShapeType := FShapeType;
            FShapeFileWriter.AddShape(Shape);
            Assign2DShapeGeometry(Shape, ColIndex, RowIndex, eaNodes);
            Assign2DID_Fields(ID, ColIndex, RowIndex, FShapeDataBase, eaNodes);
            FShapeDataBase.PostChanges;
          end;
        end;
      end;
    end
    else
    begin
      RowIndex := 0;
      FShapeFileWriter.Capacity := FLocalMesh.Mesh2DI.NodeCount;
      for NodeIndex := 0 to FLocalMesh.Mesh2DI.NodeCount - 1 do
      begin
        ColIndex := NodeIndex;
        if LocationOK then
        begin
          Inc(ID);
          Shape := TShapeObject.Create;
          Shape.FShapeType := FShapeType;
          FShapeFileWriter.AddShape(Shape);
          Assign2DShapeGeometry(Shape, NodeIndex, 0, eaNodes);
          Assign2DID_Fields(ID, NodeIndex, 0, FShapeDataBase, eaNodes);
          FShapeDataBase.PostChanges;
        end;
      end;
    end;
    if FLocalGrid <> nil then
    begin
      RowCount := FLocalGrid.RowCount;
      ColumnCount := FLocalGrid.ColumnCount;
    end
    else
    begin
      RowCount := 0;
      ColumnCount := FLocalMesh.Mesh2DI.NodeCount-1;
    end;
    NameIndexStart := 0;
    try
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      FShapeDataBase.GotoBOF;
      for RowIndex := 0 to RowCount do
      begin
        for ColIndex := 0 to ColumnCount do
        begin
          if LocationOK then
          begin
            Assign2DDataSetValuesToDataBase(DataSets, DataSetIndex, FNames,
              FLayerLimit, ColIndex, RowIndex, FShapeDataBase, NameIndexStart);
            FShapeDataBase.PostChanges;
            if (RowIndex < RowCount)
              or (ColIndex < ColumnCount) then
            begin
              FShapeDataBase.GotoNext;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      NameIndexStart := NameIndexStart + DataArray.LayerCount;
      DataArray.CacheData;
    end;
    except
      ShowMessage(DataSetIndex.ToString);
    end;
    IndexFileName := ChangeFileExt(FShapeFileName, '.shx');
    FShapeFileWriter.WriteToFile(FShapeFileName, IndexFileName);
  finally
    FShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.InitializeControls;
begin
  rdgTime.Cells[0, 0] := 'Times';
  rdgTime.Cells[0, 1] := '0';

  case frmGoPhast.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        lblElements.Caption := StrElementShapefileN;
        lblNodes.Caption := StrNodeShapefileName;
        Caption := StrExportGridDataTo;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
      :
      begin
        lblElements.Caption := StrCellShapefileName;
        lblNodes.Caption := StrCellCornerShapefi;
        Caption := StrExportGridDataTo;
      end;
    msSutra22, msSutra30, msSutra40:
      begin
        lblElements.Caption := StrElementShapefileN;
        lblNodes.Caption := StrNodeShapefileName;
        Caption := StrExportMeshDataTo;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.Assign2DID_Fields(ID, ColIndex, RowIndex: Integer;
  ShapeDataBase: TXBase; EvaluatedAt: TEvaluatedAt);
begin
  ShapeDataBase.AppendBlank;

  if FLocalGrid <> nil then
  begin
    ShapeDataBase.UpdFieldInt('ROW', RowIndex + 1);
    ShapeDataBase.UpdFieldInt('COLUMN', ColIndex + 1);
  end
  else if FDisvUsed then
  begin
    Assert(EvaluatedAt = eaBlocks);
    ShapeDataBase.UpdFieldInt('CELL', ColIndex + 1);
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks: ShapeDataBase.UpdFieldInt('ELEMENT', ColIndex + 1);
      eaNodes: ShapeDataBase.UpdFieldInt('NODE', ColIndex + 1);
      else Assert(False);
    end;
  end;
  ShapeDataBase.UpdFieldInt('ID', ID);
end;

procedure TfrmExportShapefile.Assign3DID_Fields(ID,
  ColIndex, RowIndex, LayerIndex: Integer; ShapeDataBase: TXBase;
  EvaluatedAt: TEvaluatedAt);
begin
  ShapeDataBase.AppendBlank;

  if FLocalGrid <> nil then
  begin
    ShapeDataBase.UpdFieldInt('ROW', RowIndex + 1);
    ShapeDataBase.UpdFieldInt('COLUMN', ColIndex + 1);
    ShapeDataBase.UpdFieldInt('LAYER', LayerIndex + 1);
  end
  else if FDisvUsed then
  begin
    ShapeDataBase.UpdFieldInt('CELL', ColIndex + 1);
    ShapeDataBase.UpdFieldInt('LAYER', LayerIndex + 1);
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          ShapeDataBase.UpdFieldInt('ELEMENT', ColIndex + 1);
        end;
      eaNodes:
        begin
          ShapeDataBase.UpdFieldInt('NODE', ColIndex + 1);
        end;
      else Assert(False);
    end;
  end;
  ShapeDataBase.UpdFieldInt('ID', ID);
end;

procedure TfrmExportShapefile.SetData;
var
  Index: Integer;
  Node: TTreeNode;
  AnObject: TObject;
  NodeDataSets: TList;
  NodeTimeLists: TList;
  ElementDataSets: TList;
  ElementTimeLists: TList;
  HfbEdits: TList;
  DataArray: TDataArray;
  TimeList: TCustomTimeList;
  LocalModel: TCustomModel;
  TimeLists: TStringList;
  TimeListIndex: Integer;
  EdgeDisplay: TEdgeDisplayEdit;
begin
  Screen.Cursor := crHourGlass;
  try
    GetFilterDataArray(FFilterArray);
    LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
    FDisvUsed := LocalModel.DisvUsed;
    FLocalGrid := LocalModel.Grid;
    FLocalMesh := LocalModel.Mesh3D;
    if (FLocalGrid = nil) then
    begin
      Assert(FLocalMesh <> nil);
    end
    else
    begin
      Assert(FLocalMesh = nil);
    end;
    NodeDataSets := TList.Create;
    NodeTimeLists := TList.Create;
    ElementDataSets := TList.Create;
    ElementTimeLists := TList.Create;
    HfbEdits:= TList.Create;
    TimeLists := TStringList.Create;
    try
      if LocalModel <> frmGoPhast.PhastModel then
      begin
        for TimeListIndex := 0 to LocalModel.TimeListCount - 1 do
        begin
          TimeList := LocalModel.TimeLists[TimeListIndex];
          Assert(TimeList.Name <> '');
          TimeLists.AddObject(TimeList.Name, TimeList);
        end;
        TimeLists.Sort;
      end;
      for Index := 0 to tvExportItems.Items.Count - 1 do
      begin
        Node := tvExportItems.Items[Index];
        if Node.StateIndex = 2 then
        begin
          AnObject := Node.Data;
          if AnObject = nil then
          begin
            Continue;
          end
          else if (AnObject is TDataArray) then
          begin
            DataArray := TDataArray(AnObject);
            if LocalModel <> frmGoPhast.PhastModel then
            begin
              DataArray := LocalModel.DataArrayManager.
                GetDataSetByName(DataArray.Name)
            end;
            case DataArray.EvaluatedAt of
              eaBlocks: ElementDataSets.Add(DataArray);
              eaNodes: NodeDataSets.Add(DataArray);
              else Assert(False);
            end;
          end
          else if (AnObject is TCustomTimeList) then
          begin
            TimeList := TCustomTimeList(AnObject);
            if LocalModel <> frmGoPhast.PhastModel then
            begin
              TimeListIndex := TimeLists.IndexOf(TimeList.Name);
              Assert(TimeListIndex >= 0);
              TimeList := TimeLists.Objects[TimeListIndex] as TCustomTimeList;
            end;
            TimeList.Initialize;
            if TimeList is TPhastTimeList then
            begin
              NodeTimeLists.Add(TimeList);
            end
            else if TimeList is TSutraMergedTimeList then
            begin
              NodeTimeLists.Add(TimeList);
            end
            else
            begin
              ElementTimeLists.Add(TimeList);
            end;
          end
          else
          begin
            EdgeDisplay := AnObject as TEdgeDisplayEdit;
            if LocalModel <> frmGoPhast.PhastModel then
            begin
              EdgeDisplay.Edge := LocalModel.HfbDisplayer;
            end;
            HfbEdits.Add(EdgeDisplay)
          end;
        end;
      end;
      ExportNodeShapes(NodeDataSets, NodeTimeLists);
      ExportElementShapes(ElementDataSets, ElementTimeLists);
      ExportHfbShapes(HfbEdits);
    finally
      TimeLists.Free;
      ElementDataSets.Free;
      ElementTimeLists.Free;
      NodeDataSets.Free;
      NodeTimeLists.Free;
      HfbEdits.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmExportShapefile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData
end;

procedure TfrmExportShapefile.FormCreate(Sender: TObject);
begin
  inherited;
  FFilterDataSets := TObjectList.Create;
  FEdgeEdits := TObjectList.Create;
  InitializeControls;
  GetData;
end;

procedure TfrmExportShapefile.FormDestroy(Sender: TObject);
begin
  inherited;
  FFilterDataSets.Free;
  FEdgeEdits.Free;
end;

procedure TfrmExportShapefile.GetBoundaryConditions;
var
  Node: TTreeNode;
  DataSetIndex: Integer;
  ClassificationNode: TTreeNode;
  TimeList: TCustomTimeList;
  List: TStringList;
  ClassificationPosition: Integer;
  DataSet: TDataArray;
  Index: Integer;
  DataSetClassifications: TStringList;
  PhastBoundaryRootNode: TTreeNode;
  SelectedTimeList: TCustomTimeList;
  SelectedDataArray: TDataArray;
  EdgeEdit: TEdgeDisplayEdit;
  DataArrayManager: TDataArrayManager;
begin
  SelectedDataArray := frmGoPhast.PhastModel.ThreeDDataSet;
  SelectedTimeList := frmGoPhast.PhastModel.ThreeDTimeList;

  PhastBoundaryRootNode := tvExportItems.Items.AddChild(nil,
    StrBoundaryConditions);
  DataSetClassifications := TStringList.Create;
  try
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.BoundaryDataSetCount - 1 do
    begin
      DataSet := DataArrayManager.BoundaryDataSets[Index];
      if CanExportDataSet(DataSet) then
      begin
        ClassificationPosition :=
          DataSetClassifications.IndexOf(DataSet.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          DataSetClassifications.AddObject(DataSet.Classification, List);
        end
        else
        begin
          List := DataSetClassifications.
            Objects[ClassificationPosition] as TStringList;
        end;
        List.AddObject(DataSet.Name, DataSet);
      end;
    end;
    FEdgeEdits.Clear;
    List := TStringList.Create;
    DataSetClassifications.AddObject(StrMODFLOWHorizontalF, List);
    for Index := 0 to frmGoPhast.PhastModel.
      HfbDisplayer.RealValueTypeCount - 1 do
    begin
      EdgeEdit := TEdgeDisplayEdit.Create;
      FEdgeEdits.Add(EdgeEdit);
      EdgeEdit.DataIndex := Index;
      EdgeEdit.Edge := frmGoPhast.PhastModel.HfbDisplayer;
      List.AddObject(EdgeEdit.Edge.RealDescription[Index], EdgeEdit);
    end;
    for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
    begin
      TimeList := frmGoPhast.PhastModel.TimeLists[Index];
      if TimeList.UsedByModel and (TimeList.Name <> StrMODFLOWHeadObservations) then
      begin
        ClassificationPosition := DataSetClassifications.IndexOf(
          TimeList.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          DataSetClassifications.AddObject(TimeList.Classification, List);
        end
        else
        begin
          List := DataSetClassifications.
            Objects[ClassificationPosition] as TStringList;
        end;
        List.AddObject(TimeList.Name, TimeList);
      end;
    end;
    DataSetClassifications.Sort;
    for Index := 0 to DataSetClassifications.Count - 1 do
    begin
      ClassificationNode := tvExportItems.Items.
        AddChild(PhastBoundaryRootNode, DataSetClassifications[Index]);
      ClassificationNode.Data := nil;
      List := DataSetClassifications.Objects[Index] as TStringList;
      List.Sort;
      for DataSetIndex := 0 to List.Count - 1 do
      begin
        Node := tvExportItems.Items.
          AddChild(ClassificationNode, List[DataSetIndex]);
        Node.Data := List.Objects[DataSetIndex];
        if (Node.Data = SelectedDataArray)
          or (Node.Data = SelectedTimeList) then
        begin
          tvExportItems.Selected := Node;
        end;
      end;
    end;
  finally
    for Index := 0 to DataSetClassifications.Count - 1 do
    begin
      DataSetClassifications.Objects[Index].Free;
    end;
    DataSetClassifications.Free;
  end;
end;

procedure TfrmExportShapefile.UpdateEnabledControls;
var
  Index: Integer;
  Node: TTreeNode;
  AnObject: TObject;
  ShouldEnableElement: Boolean;
  ShouldEnableNode: Boolean;
  ShouldEnableHfb: Boolean;
  ShouldEnableTime: Boolean;
begin
  ShouldEnableElement := False;
  ShouldEnableNode := False;
  ShouldEnableHfb := False;
  ShouldEnableTime := False;
  for Index := 0 to tvExportItems.Items.Count - 1 do
  begin
    Node := tvExportItems.Items[Index];
    if Node.StateIndex = 2 then
    begin
      AnObject := Node.Data;
      if AnObject = nil then
      begin
        Continue;
      end
      else if (AnObject is TDataArray) then
      begin
        case TDataArray(AnObject).EvaluatedAt of
          eaBlocks: ShouldEnableElement := True;
          eaNodes: ShouldEnableNode := True;
          else Assert(False);
        end;
      end
      else if (AnObject is TCustomTimeList) then
      begin
        ShouldEnableTime := True;
        if AnObject is TPhastTimeList then
        begin
          ShouldEnableNode := True;
        end
        else if AnObject is TSutraMergedTimeList then
        begin
          ShouldEnableNode := True;
        end
        else
        begin
          ShouldEnableElement := True;
        end;
      end
      else
      begin
        Assert(AnObject is TEdgeDisplayEdit);
        ShouldEnableHfb := True;
      end;
    end;
  end;
  jfeElements.Enabled := ShouldEnableElement;
  jfeNodes.Enabled := ShouldEnableNode;
  jfeHorizontalFlowBarrier.Enabled := ShouldEnableHfb;
  rgHfbDimensions.Enabled := ShouldEnableHfb;
  rdgTime.Enabled := ShouldEnableTime;
  if ShouldEnableTime then
  begin
    rdgTime.Color := clWindow;
  end
  else
  begin
    rdgTime.Color := clBtnFace;
  end;
  seTimeCount.Enabled := ShouldEnableTime;
  rgExportObjectType.Enabled := (ShouldEnableElement or ShouldEnableNode);
  btnOK.Enabled := (ShouldEnableElement or ShouldEnableNode or ShouldEnableHfb);

  if (not FGettingData)
    and (frmGoPhast.PhastModel.ModelSelection in SutraSelection) then
  begin
    if frmGoPhast.PhastModel.SutraMesh.MeshType in [mt2D, mtProfile] then
    begin
      rgExportObjectType.Buttons[Ord(escThreeDPoly)].Enabled := False;
      rgExportObjectType.Buttons[Ord(escThreeDPoint)].Enabled := False;
    end;
  end;
end;

procedure TfrmExportShapefile.UpdateParentNodeStates;
var
  ChildNode: TTreeNode;
  Stack: TStack;
  Index: Integer;
  Node: TTreeNode;
begin
  Stack := TStack.Create;
  try
    for Index := 0 to tvExportItems.Items.Count - 1 do
    begin
      Node := tvExportItems.Items[Index];
      if Node.HasChildren then
      begin
        Stack.Push(Node);
      end;
    end;
    while Stack.Count > 0 do
    begin
      Node := Stack.Pop;
      ChildNode := Node.GetFirstChild;
      Node.StateIndex := ChildNode.StateIndex;
      if Node.StateIndex = 3 then
      begin
        Continue;
      end;
      ChildNode := ChildNode.getNextSibling;
      while ChildNode <> nil do
      begin
        if Node.StateIndex <> ChildNode.StateIndex then
        begin
          Node.StateIndex := 3;
          break;
        end;
        ChildNode := ChildNode.getNextSibling;
      end;
    end;
  finally
    Stack.Free;
  end;
end;

end.
