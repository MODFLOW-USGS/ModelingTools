unit frmDataSetValuesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, VirtualTrees,
  ComCtrls, Grids, RbwDataGrid4, JvExStdCtrls, JvListBox, SsButtonEd,
  RbwStringTreeCombo, DataSetUnit, GrayTabs;

type
  TfrmDataSetValues = class(TfrmCustomGoPhast)
    Panel1: TPanel;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    pcDataSet: TPageControl;
    btnCopy: TButton;
    Panel2: TPanel;
    lbLayers: TJvListBox;
    lblLayer: TLabel;
    lblDataSet: TLabel;
    comboModel: TComboBox;
    lblModel: TLabel;
    treecomboDataSets: TRbwStringTreeCombo;
    comboOrientation: TComboBox;
    btnSave: TButton;
    dlgSave: TSaveDialog;
    cbIncludeCaptions: TCheckBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure treecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure treecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure treecomboDataSetsChange(Sender: TObject);
    procedure treecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure btnCopyClick(Sender: TObject);
    procedure lbLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboModelChange(Sender: TObject);
    procedure treecomboDataSets1TreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure comboOrientationChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FSelectedVirtNode: PVirtualNode;
    // @name is implemented as a TObjectList.
    FDataSets: TList;
    FTempControls: TList;
    procedure GetData;
    procedure SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GetDataArray(var DataArray: TDataArray);
    procedure AssignGridValues(DataArray: TDataArray);
    procedure AssignMeshValues(DataArray: TDataArray);
    { Private declarations }
  public
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Contnrs, ClassificationUnit, frmGoPhastUnit, RbwParser,
  GoPhastTypes, PhastModelUnit, SutraMeshUnit;

resourcestring
  StrParentModel = 'Parent model';
  StrLayer = 'Layer';
  StrRow = 'Row';
  StrColumn = 'Column';
  StrToSaveDataSetVal = 'To save data set values, you must first select a da' +
  'ta set.';
  StrOnlyDataSetsThat = 'Only data sets that have numeric values can be save' +
  'd.';
  StrElement = 'Element';
  StrNode = 'Node';

procedure TfrmDataSetValues.AssignMeshValues(DataArray: TDataArray);
var
  Mesh: TSutraMesh3D;
  Element3D: TSutraElement3D;
  Node3D: TSutraNode3D;
  APage: TTabSheet;
  AGrid: TRbwDataGrid4;
  ColumnFormat: TRbwColumnFormat4;
  ColIndex: Integer;
  RowIndex: Integer;
  Row: Integer;
  LayerIndex: Integer;
  LIndex: Integer;
begin
  Assert(frmGoPhast.ModelSelection in SutraSelection);
  lbLayers.Items.Add('1');
  APage := TTabSheet.Create(self);
  FTempControls.Add(APage);
  APage.PageControl := pcDataSet;
  APage.Caption := '1';
  AGrid := TRbwDataGrid4.Create(self);
  AGrid.Parent := APage;
  AGrid.Align := alClient;

  lblLayer.Caption := '';

  case DataArray.EvaluatedAt of
    eaBlocks: AGrid.Cells[0,0] := StrElement;
    eaNodes: AGrid.Cells[0,0] := StrNode;
    else Assert(False);
  end;

  Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
  case Mesh.MeshType of
    mt2D, mtProfile:
      begin
        case DataArray.EvaluatedAt of
          eaBlocks:
            begin
              AGrid.RowCount := Mesh.Mesh2D.Elements.Count + 1;
            end;
          eaNodes:
            begin
              AGrid.RowCount := Mesh.Mesh2D.Nodes.Count + 1;
            end;
          else Assert(False);
        end;
      end;
    mt3D:
      begin
        case DataArray.EvaluatedAt of
          eaBlocks:
            begin
              if DataArray.Orientation = dso3D then
              begin
                AGrid.RowCount := Mesh.ActiveElementCount + 1
              end
              else
              begin
                AGrid.RowCount := Mesh.Mesh2D.Elements.Count + 1
              end;
            end;
          eaNodes:
            begin
              if DataArray.Orientation = dso3D then
              begin
                AGrid.RowCount := Mesh.ActiveNodeCount + 1
              end
              else
              begin
                AGrid.RowCount := Mesh.Mesh2D.Nodes.Count + 1
              end;
            end;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;

  AGrid.ColCount := 2;
  AGrid.Options := AGrid.Options - [goEditing];
  AGrid.DefaultColWidth := 10;
  AGrid.ColorSelectedRow := False;
  AGrid.AutoMultiEdit := True;
  ColumnFormat := rcf4Real;
  case DataArray.DataType of
    rdtDouble:
      ColumnFormat := rcf4Real;
    rdtInteger:
      ColumnFormat := rcf4Integer;
    rdtBoolean:
      ColumnFormat := rcf4Boolean;
    rdtString:
      ColumnFormat := rcf4String;
  else
    Assert(False);
  end;
  AGrid.Columns[0].AutoAdjustColWidths := True;
  for ColIndex := 1 to AGrid.ColCount - 1 do
  begin
    AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
    AGrid.Columns[ColIndex].Format := ColumnFormat;
    AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
  end;
//  AGrid.RowCount := DataArray.RowCount + 1;
  AGrid.BeginUpdate;
  try
    for RowIndex := 1 to AGrid.RowCount - 1 do
    begin
      AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
    end;
    RowIndex := 0;
    for ColIndex := 0 to DataArray.ColumnCount - 1 do
    begin
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin

        Row := -1;
        case Mesh.MeshType of
          mt2D, mtProfile:
            begin
              Row := ColIndex + 1;
            end;
          mt3D:
            begin
              case DataArray.EvaluatedAt of
                eaBlocks:
                  begin
                    if DataArray.Orientation = dso3D then
                    begin
                      Element3D := Mesh.ElementArray[LayerIndex, ColIndex];
                      if Element3D.Active then
                      begin
                        Row := Element3D.ElementNumber + 1;
                      end
                      else
                      begin
                        Row := -1;
                      end;
                    end
                    else
                    begin
                      Row:= -1;
                      for LIndex := 0 to Mesh.LayerCount - 1 do
                      begin
                        Element3D := Mesh.ElementArray[LIndex, ColIndex];
                        if Element3D.Active then
                        begin
                          Row := ColIndex + 1;
                          AGrid.Cells[0, Row] := IntToStr(Element3D.ElementNumber + 1);
                          break;
                        end;
                      end;
                    end;
                  end;
                eaNodes:
                  begin
                    if DataArray.Orientation = dso3D then
                    begin
                      Node3D := Mesh.NodeArray[LayerIndex, ColIndex];
                      if Node3D.Active then
                      begin
                        Row := Node3D.Number + 1;
                      end
                      else
                      begin
                        Row := -1;
                      end;
                    end
                    else
                    begin
                      Row := -1;
                      for LIndex := 0 to Mesh.LayerCount - 1 do
                      begin
                        Node3D := Mesh.NodeArray[LIndex, ColIndex];
                        if Node3D.Active then
                        begin
                          Row := ColIndex + 1;
                          AGrid.Cells[0, Row] := IntToStr(Node3D.Number + 1);
                          break;
                        end;
                      end;
                    end;
                  end;
                else Assert(False);
              end;
            end;
          else
            Assert(False);
        end;


        if Row > 0 then
        begin
          case DataArray.DataType of
            rdtDouble:
              AGrid.Cells[1,Row] := FloatToStr(DataArray.RealData[
                LayerIndex, RowIndex, ColIndex]);
            rdtInteger:
              AGrid.Cells[1,Row] := IntToStr(DataArray.IntegerData[
                LayerIndex, RowIndex, ColIndex]);
            rdtBoolean:
              AGrid.Checked[1,Row] := DataArray.BooleanData[
                LayerIndex, RowIndex, ColIndex];
            rdtString:
              AGrid.Cells[1,Row] := DataArray.StringData[
                LayerIndex, RowIndex, ColIndex];
            else
              Assert(False);
          end;
        end;
      end;
    end;
  finally
    AGrid.EndUpdate;
  end;
  APage.TabVisible := False;
end;

procedure TfrmDataSetValues.btnCopyClick(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  IncludeCaptions: Boolean;
begin
  inherited;
  if pcDataSet.ActivePage <> nil then
  begin
    pcDataSet.ActivePage.Handle;
    Grid := pcDataSet.ActivePage.Controls[0] as TRbwDataGrid4;
    IncludeCaptions := cbIncludeCaptions.Checked;
    Grid.CopyAllCellsToClipboard(IncludeCaptions);
  end;
end;

procedure TfrmDataSetValues.btnSaveClick(Sender: TObject);
var
  DataArray: TDataArray;
  AFileName: string;
  Lines: TStringList;
  StringBuilder: TStringBuilder;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  GetDataArray(DataArray);
  if DataArray = nil then
  begin
    Beep;
    MessageDlg(StrToSaveDataSetVal, mtWarning, [mbOK], 0);
    Exit;
  end;
  if not (DataArray.DataType in [rdtDouble, rdtInteger]) then
  begin
    Beep;
    MessageDlg(StrOnlyDataSetsThat, mtWarning, [mbOK], 0);
    Exit;
  end;
  AFileName := ExtractFileDir(frmGoPhast.PhastModel.ModelFileName);
  AFileName := IncludeTrailingPathDelimiter(AFileName)
    + DataArray.Name + dlgSave.DefaultExt;
  dlgSave.FileName := AFileName;
  if dlgSave.Execute then
  begin
    Lines := TStringList.Create;
    StringBuilder := TStringBuilder.Create;
    try
      // Format code
      Lines.Add('1');

      // number of data sets
      Lines.Add('1');

      // data array names
      Lines.Add(DataArray.Name);

      // Number of layers, row, and columns
      Lines.Add(Format('%0:d %1:d %2:d',
        [DataArray.LayerCount, DataArray.RowCount, DataArray.ColumnCount]));

      // data set values.
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          StringBuilder.Clear;
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            case DataArray.DataType of
              rdtDouble: StringBuilder.Append(
                DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
              rdtInteger: StringBuilder.Append(
                DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
              else Assert(False);
            end;
            StringBuilder.Append(' ');
          end;
          Lines.Add(StringBuilder.ToString);
        end;
      end;
      try
        Lines.SaveToFile(dlgSave.FileName);
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
        end;
      end;
    finally
      StringBuilder.Free;
      Lines.Free;
    end;
  end;
end;

procedure TfrmDataSetValues.comboModelChange(Sender: TObject);
begin
  inherited;
  treecomboDataSetsChange(nil);
end;

procedure TfrmDataSetValues.comboOrientationChange(Sender: TObject);
begin
  inherited;
  treecomboDataSetsChange(Sender);
end;

procedure TfrmDataSetValues.FormCreate(Sender: TObject);
begin
  inherited;
  FDataSets := TObjectList.Create;
  FSelectedVirtNode := nil;
  FTempControls := TObjectList.Create;
  GetData;
end;

procedure TfrmDataSetValues.FormDestroy(Sender: TObject);
begin
  inherited;
  FTempControls.Free;
  FDataSets.Free;
end;

procedure TfrmDataSetValues.GetData;
var
  Node: PVirtualNode;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowNWT, msModflowCfp, msSutra22, msSutra30,
    msSutra40, msFootPrint, msModflow2015:
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
    else
      Assert(False);
  end;
  comboModel.ItemIndex := comboModel.Items.IndexOfObject(frmGoPhast.PhastModel.SelectedModel);

  FillVirtualStringTreeWithDataSets(treecomboDataSets.Tree,
    FDataSets, nil, CanDisplayDataSet);
  Node := treecomboDataSets.Tree.GetFirst;
  if Node <> nil then
  begin
    treecomboDataSets.Tree.Selected[Node] := True;
    treecomboDataSetsChange(nil);
  end;
end;

procedure TfrmDataSetValues.lbLayersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if lbLayers.ItemIndex >= 0 then
  begin
    pcDataSet.ActivePageIndex := lbLayers.ItemIndex;
  end;
end;

procedure TfrmDataSetValues.treecomboDataSets1TreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmDataSetValues.treecomboDataSetsChange(Sender: TObject);
var
  DataArray: TDataArray;
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, treecomboDataSets);
  GetDataArray(DataArray);

  if DataArray = nil then
  begin
    Exit;
  end;

  FTempControls.Clear;
  lbLayers.Items.Clear;
  DataArray.Initialize;

  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    AssignMeshValues(DataArray);
  end
  else
  begin
    AssignGridValues(DataArray);
  end;
  pcDataSet.ActivePageIndex := 0;
  lbLayers.ItemIndex := 0;
  btnCopy.Enabled := True;
  btnSave.Enabled := DataArray.DataType in [rdtDouble, rdtInteger];
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmDataSetValues.SetSelectedNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, treecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmDataSetValues.GetDataArray(var DataArray: TDataArray);
var
  LocalModel: TCustomModel;
begin
  DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(
    treecomboDataSets.Text);
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

procedure TfrmDataSetValues.AssignGridValues(DataArray: TDataArray);
var
  ColIndex: Integer;
  ColIndex2: Integer;
  LayerIndex: Integer;
  RowIndex2: Integer;
  APage: TTabSheet;
  ColumnFormat: TRbwColumnFormat4;
  AGrid: TRbwDataGrid4;
  RowIndex: Integer;
begin
  case DataArray.Orientation of
    dso3D:
      begin
        case comboOrientation.ItemIndex of
          0:
            begin
              // Layer
              lblLayer.Caption := StrLayer;
              for LayerIndex := 0 to DataArray.LayerCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(LayerIndex + 1));
                APage := TTabSheet.Create(self);
                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(LayerIndex + 1);
                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.ColumnCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble:
                    ColumnFormat := rcf4Real;
                  rdtInteger:
                    ColumnFormat := rcf4Integer;
                  rdtBoolean:
                    ColumnFormat := rcf4Boolean;
                  rdtString:
                    ColumnFormat := rcf4String;
                else
                  Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex].Format := ColumnFormat;
                  AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
                end;
                AGrid.RowCount := DataArray.RowCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
                  end;
                  for ColIndex := 0 to DataArray.ColumnCount - 1 do
                  begin
                    for RowIndex := 0 to DataArray.RowCount - 1 do
                    begin
					  if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
					  begin
                      case DataArray.DataType of
                        rdtDouble:
                          AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                            FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger:
                          AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                            IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean:
                          AGrid.Checked[ColIndex + 1, RowIndex + 1] :=
                            DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString:
                          AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                            DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
					  end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end;
            end;
          1:
            begin
              // Row
              lblLayer.Caption := StrRow;
              for RowIndex := 0 to DataArray.RowCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(RowIndex + 1));
                APage := TTabSheet.Create(self);
                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(RowIndex + 1);
                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.ColumnCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble:
                    ColumnFormat := rcf4Real;
                  rdtInteger:
                    ColumnFormat := rcf4Integer;
                  rdtBoolean:
                    ColumnFormat := rcf4Boolean;
                  rdtString:
                    ColumnFormat := rcf4String;
                else
                  Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex].Format := ColumnFormat;
                  AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
                end;
                AGrid.RowCount := DataArray.LayerCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex2 := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0, RowIndex2] := IntToStr(RowIndex2);
                  end;
                  for ColIndex := 0 to DataArray.ColumnCount - 1 do
                  begin
                    for LayerIndex := 0 to DataArray.LayerCount - 1 do
                    begin
                      case DataArray.DataType of
                        rdtDouble:
                          AGrid.Cells[ColIndex + 1, LayerIndex + 1] := FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger:
                          AGrid.Cells[ColIndex + 1, LayerIndex + 1] :=
                            IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean:
                          AGrid.Checked[ColIndex + 1, LayerIndex + 1] :=
                            DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString:
                          AGrid.Cells[ColIndex + 1, LayerIndex + 1] :=
                            DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end;
            end;
          2:
            begin
              // Column
              lblLayer.Caption := StrColumn;
              for ColIndex := 0 to DataArray.ColumnCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(ColIndex + 1));
                APage := TTabSheet.Create(self);
                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(ColIndex + 1);
                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.RowCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble:
                    ColumnFormat := rcf4Real;
                  rdtInteger:
                    ColumnFormat := rcf4Integer;
                  rdtBoolean:
                    ColumnFormat := rcf4Boolean;
                  rdtString:
                    ColumnFormat := rcf4String;
                else
                  Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex2 := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex2].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex2].Format := ColumnFormat;
                  AGrid.Cells[ColIndex2, 0] := IntToStr(ColIndex2);
                end;
                AGrid.RowCount := DataArray.LayerCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
                  end;
                  for RowIndex := 0 to DataArray.RowCount - 1 do
                  begin
                    for LayerIndex := 0 to DataArray.LayerCount - 1 do
                    begin
                      case DataArray.DataType of
                        rdtDouble:
                          AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                            FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger:
                          AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                            IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean:
                          AGrid.Checked[RowIndex + 1, LayerIndex + 1] :=
                            DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString:
                          AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                            DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    dsoTop:
      begin
        lblLayer.Caption := StrLayer;
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          lbLayers.Items.Add(IntToStr(LayerIndex + 1));
          APage := TTabSheet.Create(self);
          FTempControls.Add(APage);
          APage.PageControl := pcDataSet;
          APage.Caption := IntToStr(LayerIndex + 1);
          AGrid := TRbwDataGrid4.Create(self);
          AGrid.Parent := APage;
          AGrid.Align := alClient;
          AGrid.ColCount := DataArray.ColumnCount + 1;
          AGrid.Options := AGrid.Options - [goEditing];
          AGrid.DefaultColWidth := 10;
          AGrid.ColorSelectedRow := False;
          AGrid.AutoMultiEdit := True;
          ColumnFormat := rcf4Real;
          case DataArray.DataType of
            rdtDouble:
              ColumnFormat := rcf4Real;
            rdtInteger:
              ColumnFormat := rcf4Integer;
            rdtBoolean:
              ColumnFormat := rcf4Boolean;
            rdtString:
              ColumnFormat := rcf4String;
          else
            Assert(False);
          end;
          AGrid.Columns[0].AutoAdjustColWidths := True;
          for ColIndex := 1 to AGrid.ColCount - 1 do
          begin
            AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
            AGrid.Columns[ColIndex].Format := ColumnFormat;
            AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
          end;
          AGrid.RowCount := DataArray.RowCount + 1;
          AGrid.BeginUpdate;
          try
            for RowIndex := 1 to AGrid.RowCount - 1 do
            begin
              AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
            end;
            for ColIndex := 0 to DataArray.ColumnCount - 1 do
            begin
              for RowIndex := 0 to DataArray.RowCount - 1 do
              begin
                case DataArray.DataType of
                  rdtDouble:
                    AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                      FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                  rdtInteger:
                    AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                      IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                  rdtBoolean:
                    AGrid.Checked[ColIndex + 1, RowIndex + 1] :=
                      DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                  rdtString:
                    AGrid.Cells[ColIndex + 1, RowIndex + 1] :=
                      DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                end;
              end;
            end;
          finally
            AGrid.EndUpdate;
          end;
          APage.TabVisible := False;
        end;
      end;
    dsoFront:
      begin
        lblLayer.Caption := StrRow;
        lbLayers.Items.Add('1');
        APage := TTabSheet.Create(self);
        FTempControls.Add(APage);
        APage.PageControl := pcDataSet;
        APage.Caption := IntToStr(1);
        AGrid := TRbwDataGrid4.Create(self);
        AGrid.Parent := APage;
        AGrid.Align := alClient;
        AGrid.ColCount := DataArray.ColumnCount + 1;
        AGrid.Options := AGrid.Options - [goEditing];
        AGrid.DefaultColWidth := 10;
        AGrid.ColorSelectedRow := False;
        AGrid.AutoMultiEdit := True;
        ColumnFormat := rcf4Real;
        case DataArray.DataType of
          rdtDouble:
            ColumnFormat := rcf4Real;
          rdtInteger:
            ColumnFormat := rcf4Integer;
          rdtBoolean:
            ColumnFormat := rcf4Boolean;
          rdtString:
            ColumnFormat := rcf4String;
        else
          Assert(False);
        end;
        AGrid.Columns[0].AutoAdjustColWidths := True;
        for ColIndex := 1 to AGrid.ColCount - 1 do
        begin
          AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
          AGrid.Columns[ColIndex].Format := ColumnFormat;
          AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
        end;
        AGrid.RowCount := DataArray.LayerCount + 1;
        AGrid.BeginUpdate;
        try
          for RowIndex := 1 to AGrid.RowCount - 1 do
          begin
            AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
          end;
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            for LayerIndex := 0 to DataArray.LayerCount - 1 do
            begin
              case DataArray.DataType of
                rdtDouble:
                  AGrid.Cells[ColIndex + 1, LayerIndex + 1] :=
                    FloatToStr(DataArray.RealData[LayerIndex, 0, ColIndex]);
                rdtInteger:
                  AGrid.Cells[ColIndex + 1, LayerIndex + 1] :=
                    IntToStr(DataArray.IntegerData[LayerIndex, 0, ColIndex]);
                rdtBoolean:
                  AGrid.Checked[ColIndex + 1, LayerIndex + 1] :=
                    DataArray.BooleanData[LayerIndex, 0, ColIndex];
                rdtString:
                  AGrid.Cells[ColIndex + 1, LayerIndex + 1] :=
                    DataArray.StringData[LayerIndex, 0, ColIndex];
              end;
            end;
          end;
        finally
          AGrid.EndUpdate;
        end;
        APage.TabVisible := False;
      end;
    dsoSide:
      begin
        lblLayer.Caption := StrColumn;
        lbLayers.Items.Add('1');
        APage := TTabSheet.Create(self);
        FTempControls.Add(APage);
        APage.PageControl := pcDataSet;
        APage.Caption := IntToStr(1);
        AGrid := TRbwDataGrid4.Create(self);
        AGrid.Parent := APage;
        AGrid.Align := alClient;
        AGrid.ColCount := DataArray.RowCount + 1;
        AGrid.Options := AGrid.Options - [goEditing];
        AGrid.DefaultColWidth := 10;
        AGrid.ColorSelectedRow := False;
        AGrid.AutoMultiEdit := True;
        ColumnFormat := rcf4Real;
        case DataArray.DataType of
          rdtDouble:
            ColumnFormat := rcf4Real;
          rdtInteger:
            ColumnFormat := rcf4Integer;
          rdtBoolean:
            ColumnFormat := rcf4Boolean;
          rdtString:
            ColumnFormat := rcf4String;
        else
          Assert(False);
        end;
        AGrid.Columns[0].AutoAdjustColWidths := True;
        for ColIndex := 1 to AGrid.ColCount - 1 do
        begin
          AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
          AGrid.Columns[ColIndex].Format := ColumnFormat;
          AGrid.Cells[ColIndex, 0] := IntToStr(ColIndex);
        end;
        AGrid.RowCount := DataArray.LayerCount + 1;
        AGrid.BeginUpdate;
        try
          for RowIndex := 1 to AGrid.RowCount - 1 do
          begin
            AGrid.Cells[0, RowIndex] := IntToStr(RowIndex);
          end;
          for RowIndex := 0 to DataArray.RowCount - 1 do
          begin
            for LayerIndex := 0 to DataArray.LayerCount - 1 do
            begin
              case DataArray.DataType of
                rdtDouble:
                  AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                    FloatToStr(DataArray.RealData[LayerIndex, RowIndex, 0]);
                rdtInteger:
                  AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                    IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, 0]);
                rdtBoolean:
                  AGrid.Checked[RowIndex + 1, LayerIndex + 1] :=
                    DataArray.BooleanData[LayerIndex, RowIndex, 0];
                rdtString:
                  AGrid.Cells[RowIndex + 1, LayerIndex + 1] :=
                    DataArray.StringData[LayerIndex, RowIndex, 0];
              end;
            end;
          end;
        finally
          AGrid.EndUpdate;
        end;
        APage.TabVisible := False;
      end;
  else
    Assert(False);
  end;
end;

end.
