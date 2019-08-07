unit frmExportCSVUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, VirtualTrees, DataSetUnit, StdCtrls, ExtCtrls,
  Buttons, GoPhastTypes, Vcl.CheckLst, JvExCheckLst, JvCheckListBox;

type
  TDataToExport = (dteLocation, dteCell, dteDataSetValues);
  TDataToExportSet = set of TDataToExport;

  TfrmExportCSV = class(TfrmCustomGoPhast)
    sdSaveCSV: TSaveDialog;
    Panel1: TPanel;
    vstDataSets: TVirtualStringTree;
    rgOrientation: TRadioGroup;
    rgEvaluatedAt: TRadioGroup;
    btnHelp: TBitBtn;
    btnSave: TBitBtn;
    pnlModel: TPanel;
    lblModel: TLabel;
    comboModel: TComboBox;
    clIncluded: TJvCheckListBox;
    lblDataToExport: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure vstDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure rgOrientationClick(Sender: TObject);
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure vstDataSetsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure sdSaveCSVTypeChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FDataSets: TList;
    FFileStream: TFileStream;
    procedure GetData;
    procedure SetData;
    function SelectDataArrays(DataArray: TDataArray): boolean;
    procedure GetOrientationAndEvalAt(var EvaluatedAt: TEvaluatedAt;
      var Orientation: TDataSetOrientation);
    procedure WriteString(const Value: String);
    procedure NewLine;
    function ExportTitle(DataArrayList: TList): string;
    function GetExportOptions: TDataToExportSet;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExportCSV: TfrmExportCSV;

implementation

uses
  Contnrs, ClassificationUnit, frmGoPhastUnit, PhastModelUnit, AbstractGridUnit,
  CustomModflowWriterUnit, RbwParser,
  Generics.Collections, FastGEO, Generics.Defaults, MeshRenumberingTypes;

resourcestring
  StrYouMustDefineThe = 'You must define the grid before you can export data' +
  '.';
  StrYouMustDefineMesh = 'You must define the mesh before you can export data' +
  '.';
  StrNoDataSetsHaveBe = 'No data sets have been selected. Do you want to jus' +
  't export the coordinates?';
  Str2DXY = 'X, Y';
  StrZ = ', Z';
  StrPrimes = ', X_Prime, Y_Prime';
  Str2D_CellLocation = 'Column, Row';
  Str3DPrimes = 'X, Y, Z, X_Prime, Y_Prime';
  StrLayer = ', Layer';
  StrElement = 'Element';
  StrNode = 'Node';
//  StrCell = ', Cell';
  StrLayerCell = 'Layer, Cell';

{$R *.dfm}

type
  TSortItem = class(TObject)
    Location: T3DRealPoint;
    Number: Integer;
    Layer: integer;
    Column: Integer;
  end;

  TSortList = TObjectList<TSortItem>;

  TSortComparer = TComparer<TSortItem>;

{ TfrmExportCSV }

procedure TfrmExportCSV.btnSaveClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmExportCSV.FormCreate(Sender: TObject);
var
  Index: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  clIncluded.CheckAll;
  FDataSets := TObjectList.Create;
  rgEvaluatedAt.Enabled := frmGoPhast.PhastModel.ModelSelection
    in [msPhast, msSutra22, msSutra30];

  comboModel.AddItem(StrParentModel, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      comboModel.AddItem(ChildModel.ModelName, ChildModel);
    end;
  end
  else
  begin
    pnlModel.Visible := False;
  end;
  comboModel.ItemIndex := 0;

  GetData;
end;

procedure TfrmExportCSV.FormDestroy(Sender: TObject);
begin
  FDataSets.Free;
  inherited;
end;

procedure TfrmExportCSV.GetData;
var
  Node: PVirtualNode;
begin
  vstDataSets.Clear;
  FillVirtualStringTreeWithDataSets(vstDataSets,
    FDataSets, nil, SelectDataArrays);
  Node := vstDataSets.GetFirst;
  while Assigned(Node) do
  begin
    if Node.ChildCount > 0 then
    begin
      Node.CheckType := ctTriStateCheckBox;
    end
    else
    begin
      Node.CheckType := ctCheckBox;
    end;
    Node := vstDataSets.GetNext(Node);
  end;
end;

procedure TfrmExportCSV.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmExportCSV.rgOrientationClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmExportCSV.sdSaveCSVTypeChange(Sender: TObject);
begin
  inherited;
  case sdSaveCSV.FilterIndex of
    1:
      begin
        sdSaveCSV.DefaultExt := 'csv';
      end;
    2:
      begin
        sdSaveCSV.DefaultExt := '';
      end;
    else Assert(False);
  end;
end;

function TfrmExportCSV.SelectDataArrays(DataArray: TDataArray): boolean;
var
  Orientation: TDataSetOrientation;
  EvaluatedAt: TEvaluatedAt;
begin
  GetOrientationAndEvalAt(EvaluatedAt, Orientation);
  result := (DataArray.Orientation = Orientation)
    and (DataArray.EvaluatedAt = EvaluatedAt);

end;

function TfrmExportCSV.ExportTitle(DataArrayList: TList): string;
var
  EvaluatedAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
//  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  LocalModel: TCustomModel;
  ExportOptions: TDataToExportSet;
  Index: Integer;
  ADataArray: TDataArray;
  Mesh: IMesh3D;
begin
  GetOrientationAndEvalAt(EvaluatedAt, Orientation);
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
//  Grid := nil;
  Mesh := nil;
  case LocalModel.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint:
      begin
//        Grid := LocalModel.Grid;
      end;
    msModflow2015:
      begin
        if LocalModel.DisvUsed then
        begin
          Mesh := LocalModel.Mesh3D;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := LocalModel.Mesh3D;
      end;
    else Assert(False);
  end;
  ExportOptions := GetExportOptions;
  result := '';
  if dteLocation in ExportOptions then
  begin
    result := Str2DXY;
    if Orientation = dso3D then
    begin
      result := result + StrZ;
    end;
    if Mesh = nil then
    begin
      result := result + StrPrimes;
    end;
  end;
  if dteCell in ExportOptions then
  begin
    if result <> '' then
    begin
      result := result + ', ';
    end;
    if (Mesh = nil) then
    begin
      result := result + Str2D_CellLocation;
      if Orientation = dso3D then
      begin
        result := result + StrLayer;
      end;
    end
    else if LocalModel.DisvUsed then
    begin
      result := result + StrLayerCell;
    end
    else
    begin
      case EvaluatedAt of
        eaBlocks: result := result + StrElement;
        eaNodes: result := result + StrNode;
      end;
    end;
  end;
  if dteDataSetValues in ExportOptions then
  begin
    if result <> '' then
    begin
      result := result + ', ';
    end;
    for Index := 0 to DataArrayList.Count - 1 do
    begin
      ADataArray := DataArrayList[Index];
      if Index > 0 then
      begin
        result := result + ', ';
      end;
      result := result + ADataArray.Name;
      ADataArray.Initialize;
    end;
  end;
//  Str2DXY = 'X, Y';
//StrZ
//  StrPrimes = ', X_Prime, Y_Prime';
//  Str2D_CellLocation = 'Column, Row';
//  Str3DPrimes = 'X, Y, Z, X_Prime, Y_Prime';
//  StrLayer = ', Layer';
//  StrElement = 'Element';
//  StrNode = 'Node';
end;

function TfrmExportCSV.GetExportOptions: TDataToExportSet;
var
  dteIndex: TDataToExport;
begin
  result := [];
  for dteIndex := Low(TDataToExport) to High(TDataToExport) do
  begin
    if clIncluded.Checked[Ord(dteIndex)] then
    begin
      Include(result, dteIndex);
    end;
  end;
end;

procedure TfrmExportCSV.SetData;
var
  DataArrayList: TList;
  Node: PVirtualNode;
  NodeData: PClassificationNodeData;
  ADataArray: TDataArray;
  EvaluatedAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
  Index: Integer;
  Grid: TCustomModelGrid;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  Location: T3DRealPoint;
  DataArrayManager: TDataArrayManager;
  LocalModel: TCustomModel;
//  Mesh: TSutraMesh3D;
  ColIndex: Integer;
//  Element2D: TSutraElement2D;
  Location2D: TPoint2D;
  SortItem: TSortItem;
//  Element3D: TSutraElement3D;
  SortList: TSortList;
//  Node2D: TSutraNode2D;
//  Node3D: TSutraNode3D;
  SortIndex: Integer;
  NonRotatedLocation: TPoint3D;
  ExportOptions: TDataToExportSet;
  Mesh: IMesh3D;
  Element2D: IElement2D;
  Element3D: IElement3D;
  Node2D: INode2D;
  Node3D: INode3D;
  function FreeFormattedReal(
    const Value: double): string;
  begin
    result := TCustomModflowWriter.FortranDecimal(Format('%.13e', [Value]));
  end;
  procedure WriteAMeshLine;
  var
    Index: integer;
    PreviousWritten: Boolean;
  begin
    PreviousWritten := False;
    if dteLocation in ExportOptions then
    begin
      WriteString(FreeFormattedReal(SortItem.Location.X) + ', ');
      WriteString(FreeFormattedReal(SortItem.Location.Y));
      if (Orientation = dso3D) then
      begin
        WriteString(', ' + FreeFormattedReal(SortItem.Location.Z));
      end;
      PreviousWritten := True;
    end;
    if dteCell in ExportOptions then
    begin
      if PreviousWritten then
      begin
        WriteString(', ');
      end;
      if LocalModel.DisvUsed then
      begin
      WriteString(IntToStr(SortItem.Layer+1) + ', ');
      end;
      WriteString(IntToStr(SortItem.Number+1));
      PreviousWritten := True;
    end;
    if dteDataSetValues in ExportOptions then
    begin
      if PreviousWritten then
      begin
        WriteString(', ');
      end;
      for Index := 0 to DataArrayList.Count - 1 do
      begin
        if Index > 0 then
        begin
          WriteString(', ');
        end;
        ADataArray := DataArrayList[Index];
        case ADataArray.DataType of
          rdtDouble:
            begin
              WriteString(FreeFormattedReal(
                ADataArray.RealData[SortItem.Layer, 0, SortItem.Column]));
            end;
          rdtInteger:
            begin
              WriteString(IntToStr(
                ADataArray.IntegerData[SortItem.Layer, 0, SortItem.Column]));
            end;
          rdtBoolean:
            begin
              if ADataArray.BooleanData[SortItem.Layer, 0, SortItem.Column] then
              begin
                WriteString('True');
              end
              else
              begin
                WriteString('False');
              end;
            end;
          rdtString:
            begin
              WriteString('"' +
                ADataArray.StringData[SortItem.Layer, 0, SortItem.Column] + '"' );
            end;
          else Assert(False);
        end;
      end;
    end;
    NewLine;
  end;
  procedure WriteAGridLine;
  var
    Index: integer;
    PreviousWritten: boolean;
  begin
    PreviousWritten := False;
    if dteLocation in ExportOptions then
    begin
      PreviousWritten := True;
      WriteString(FreeFormattedReal(Location.X) + ', ');
      WriteString(FreeFormattedReal(Location.Y));
      if (Orientation = dso3D) then
      begin
        WriteString(', ' + FreeFormattedReal(Location.Z));
      end;
      if Mesh = nil then
      begin
        WriteString(', ' + FreeFormattedReal(NonRotatedLocation.X));
        WriteString(', ' + FreeFormattedReal(NonRotatedLocation.Y));
      end;
    end;
    if dteCell in ExportOptions then
    begin
      if PreviousWritten then
      begin
        WriteString(', ');
      end;
      WriteString(IntToStr(ColumnIndex+1));
      WriteString(', ' + IntToStr(RowIndex+1));
      if (Orientation = dso3D) then
      begin
        WriteString(', ' + IntToStr(LayerIndex+1));
      end;
      PreviousWritten := true;
    end;
    if dteDataSetValues in ExportOptions then
    begin
      for Index := 0 to DataArrayList.Count - 1 do
      begin
        if (Index > 0) or PreviousWritten then
        begin
          WriteString(', ');
        end;
        ADataArray := DataArrayList[Index];
        case ADataArray.DataType of
          rdtDouble:
            begin
              WriteString(FreeFormattedReal(
                ADataArray.RealData[LayerIndex, RowIndex, ColumnIndex]));
            end;
          rdtInteger:
            begin
              WriteString(IntToStr(
                ADataArray.IntegerData[LayerIndex, RowIndex, ColumnIndex]));
            end;
          rdtBoolean:
            begin
              if ADataArray.BooleanData[LayerIndex, RowIndex, ColumnIndex] then
              begin
                WriteString('True');
              end
              else
              begin
                WriteString('False');
              end;
            end;
          rdtString:
            begin
              WriteString('"' +
                ADataArray.StringData[LayerIndex, RowIndex, ColumnIndex] + '"' );
            end;
          else Assert(False);
        end;
      end;
    end;
    NewLine;
  end;
begin
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  Grid := nil;
  Mesh := nil;
  case LocalModel.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint:
      begin
        Grid := LocalModel.Grid;
        if (Grid.ColumnCount = 0)
          or (Grid.RowCount = 0)
          or (Grid.LayerCount = 0) then
        begin
          MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    msModflow2015:
      begin
        if LocalModel.DisvUsed then
        begin
          Mesh := LocalModel.Mesh3D;
          if Mesh.ElementCount = 0 then
          begin
            MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
            Exit;
          end;
        end
        else
        begin
          Grid := LocalModel.Grid;
          if (Grid.ColumnCount = 0)
            or (Grid.RowCount = 0)
            or (Grid.LayerCount = 0) then
          begin
            MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := LocalModel.Mesh3D;
        if not Mesh.Is3DMesh then
        begin
          if Mesh.Mesh2DI.ElementCount = 0 then
          begin
            MessageDlg(StrYouMustDefineMesh, mtError, [mbOK], 0);
            Exit;
          end;
        end
        else
        begin
          if Mesh.ElementCount = 0 then
          begin
            MessageDlg(StrYouMustDefineMesh, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end;
    else Assert(False);
  end;
  Screen.Cursor := crHourGlass;
  DataArrayList := TList.Create;
  try
    Node := vstDataSets.GetFirst;
    while Assigned(Node) do
    begin
      if (Node.ChildCount = 0)
        and (Node.CheckState in [csCheckedNormal, csCheckedPressed]) then
      begin
        NodeData := vstDataSets.GetNodeData(Node);
        if Assigned(NodeData)
          and Assigned(NodeData.ClassificationObject) then
        begin
          if NodeData.ClassificationObject is TDataSetClassification then
          begin
            ADataArray := TDataSetClassification(
              NodeData.ClassificationObject).DataArray;
            if LocalModel <> frmGoPhast.PhastModel then
            begin
              ADataArray := LocalModel.DataArrayManager.
                GetDataSetByName(ADataArray.Name);
              Assert(ADataArray <> nil);
            end;
            DataArrayList.Add(ADataArray);
          end
        end;
      end;
      Node := vstDataSets.GetNext(Node);
    end;
    if DataArrayList.Count = 0 then
    begin
      if (MessageDlg(StrNoDataSetsHaveBe,
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
      begin
        Exit;
      end;
    end;
    if sdSaveCSV.Execute then
    begin
      GetOrientationAndEvalAt(EvaluatedAt, Orientation);
      try
        FFileStream := TFileStream.Create(sdSaveCSV.FileName,
          fmCreate or fmShareDenyWrite);
        WriteString(ExportTitle(DataArrayList));
        try
          NewLine;
          ExportOptions := GetExportOptions;

          if Mesh = nil then
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                    case Orientation of
                      dsoTop:
                        begin
                          LayerIndex := 0;
                          for RowIndex := 0 to Grid.RowCount - 1 do
                          begin
                            for ColumnIndex := 0 to Grid.ColumnCount - 1 do
                            begin
                              Location := Grid.
                                RotatedThreeDElementCenter(
                                ColumnIndex, RowIndex, 0);
                              NonRotatedLocation :=
                                Grid.ThreeDElementCenter(
                                ColumnIndex, RowIndex, 0);
                              WriteAGridLine;
                            end;
                          end;
                        end;
                      dso3D:
                        begin
                          for LayerIndex := 0 to Grid.LayerCount - 1 do
                          begin
                            for RowIndex := 0 to Grid.RowCount - 1 do
                            begin
                              for ColumnIndex := 0 to Grid.ColumnCount - 1 do
                              begin
                                Location := Grid.
                                  RotatedThreeDElementCenter(
                                  ColumnIndex, RowIndex, LayerIndex);
                                NonRotatedLocation :=
                                  Grid.ThreeDElementCenter(
                                  ColumnIndex, RowIndex, LayerIndex);
                                WriteAGridLine;
                              end;
                            end;
                          end;
                        end;
                      else Assert(False);
                    end;
                end;
              eaNodes:
                begin
                  case Orientation of
                    dsoTop:
                      begin
                        LayerIndex := 0;
                        for RowIndex := 0 to Grid.RowCount do
                        begin
                          for ColumnIndex := 0 to Grid.ColumnCount do
                          begin
                            Location := Grid.
                              RotatedThreeDElementCorner(
                              ColumnIndex, RowIndex, 0);
                            WriteAGridLine;
                          end;
                        end;
                      end;
                    dso3D:
                      begin
                        for LayerIndex := 0 to Grid.LayerCount do
                        begin
                          for RowIndex := 0 to Grid.RowCount do
                          begin
                            for ColumnIndex := 0 to Grid.ColumnCount do
                            begin
                              Location := Grid.
                                RotatedThreeDElementCorner(
                                ColumnIndex, RowIndex, LayerIndex);
                              WriteAGridLine;
                            end;
                          end;
                        end;
                      end;
                    else Assert(False);
                  end;
                end;
              else Assert(False);
            end;
          end
          else
          begin
            SortList := TSortList.Create;
            try
              case EvaluatedAt of
                eaBlocks:
                  begin
                    for ColIndex := 0 to Mesh.Mesh2DI.ElementCount - 1 do
                    begin
                      Element2D := Mesh.Mesh2DI.ElementsI2D[ColIndex];
                      Location2D := Element2D.Center;
                      Location.X := Location2D.X;
                      Location.Y := Location2D.Y;
                      if Mesh.Is3DMesh and (Orientation = dso3D) then
                      begin
                        for LayerIndex := 0 to Mesh.LayerCount - 1 do
                        begin
                          Element3D := Mesh.ElementArrayI[LayerIndex, ColIndex];
                          if Element3D.Active then
                          begin
                            SortItem := TSortItem.Create;
                            SortList.Add(SortItem);
                            SortItem.Number := Element3D.ElementNumber;
                            Location.Z := Element3D.CenterElevation;
                            SortItem.Location := Location;
                            SortItem.Layer := LayerIndex;
                            SortItem.Column := ColIndex;
                          end;
                        end;
                      end
                      else
                      begin
                        SortItem := nil;
                        if Mesh.Is3DMesh then
                        begin
                          for LayerIndex := 0 to Mesh.LayerCount - 1 do
                          begin
                            Element3D := Mesh.ElementArrayI[LayerIndex, ColIndex];
                            if Element3D.Active then
                            begin
                              SortItem := TSortItem.Create;
                              SortList.Add(SortItem);
                              SortItem.Number := Element3D.ElementNumber;
                              Location.Z := Element3D.CenterElevation;
                              SortItem.Location := Location;
                              SortItem.Layer := 0;
                              SortItem.Column := ColIndex;
                              Break;
                            end;
                          end;
                        end
                        else
                        begin
                          SortItem := TSortItem.Create;
                          SortList.Add(SortItem);
                          SortItem.Number := Element2D.ElementNumber;
                          Location.Z := 0;
                          SortItem.Location := Location;
                          SortItem.Layer := 0;
                          SortItem.Column := ColIndex;
                        end;

                      end;
                    end;
                  end;
                eaNodes:
                  begin
                    for ColIndex := 0 to Mesh.Mesh2DI.NodeCount - 1 do
                    begin
                      Node2D := Mesh.Mesh2DI.NodesI2D[ColIndex];
                      Location.X := Node2D.Location.X;
                      Location.Y := Node2D.Location.Y;
                      if Mesh.Is3DMesh and (Orientation = dso3D) then
                      begin
                        for LayerIndex := 0 to Mesh.LayerCount do
                        begin
                          Node3D := Mesh.NodeArrayI[LayerIndex, ColIndex];
                          if Node3D.Active then
                          begin
                            SortItem := TSortItem.Create;
                            SortList.Add(SortItem);
                            SortItem.Number := Node3D.NodeNumber;
                            Location.Z := Node3D.Z;
                            SortItem.Location := Location;
                            SortItem.Layer := LayerIndex;
                            SortItem.Column := ColIndex;
                          end;
                        end;
                      end
                      else
                      begin
                        SortItem := nil;
                        if Mesh.Is3DMesh then
                        begin
                          for LayerIndex := 0 to Mesh.LayerCount do
                          begin
                            Node3D := Mesh.NodeArrayI[LayerIndex, ColIndex];
                            if Node3D.Active then
                            begin
                              SortItem := TSortItem.Create;
                              SortList.Add(SortItem);
                              SortItem.Number := Node3D.NodeNumber;
                              Location.Z := Node3D.Z;
                              SortItem.Location := Location;
                              SortItem.Layer := 0;
                              SortItem.Column := ColIndex;
                              Break;
                            end;
                          end;
                        end
                        else
                        begin
                          SortItem := TSortItem.Create;
                          SortList.Add(SortItem);
                          SortItem.Number := Node2D.NodeNumber;
                          Location.Z := 0;
                          SortItem.Location := Location;
                          SortItem.Layer := 0;
                          SortItem.Column := ColIndex;
                        end;

                      end;
                    end;
                  end;
                else Assert(False);
              end;
              SortList.Sort(TSortComparer.Construct(
                function(const Item1, Item2: TSortItem): Integer
                begin
                  result := Item1.Layer - Item2.Layer;
                  if result = 0 then
                  begin
                    result := Item1.Number - Item2.Number;
                  end;
                end)
                );
                for SortIndex := 0 to SortList.Count - 1 do
                begin
                  SortItem := SortList[SortIndex];
                  WriteAMeshLine;
                end;
            finally
              SortList.Free;
            end;
          end;
        finally
          FFileStream.Free;
        end;
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end;
    if DataArrayList.Count > 0 then
    begin
      DataArrayManager := LocalModel.DataArrayManager;
      for Index := 0 to DataArrayList.Count - 1 do
      begin
        ADataArray := DataArrayList[Index];
        DataArrayManager.AddDataSetToCache(ADataArray);
      end;
      DataArrayManager.CacheDataArrays;
    end;
  finally
    Screen.Cursor := crDefault;
    DataArrayList.Free;
  end;
  ModalResult := mrOK;
end;
procedure TfrmExportCSV.GetOrientationAndEvalAt(var EvaluatedAt: TEvaluatedAt;
  var Orientation: TDataSetOrientation);
begin
  Orientation := dso3D;
  case rgOrientation.ItemIndex of
    0:
      begin
        Orientation := dsoTop;
      end;
    1:
      begin
        Orientation := dso3D;
      end;
  else
    Assert(False);
  end;
  Assert(rgEvaluatedAt.ItemIndex >= 0);
  EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
end;

procedure TfrmExportCSV.vstDataSetsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  ChildNode: PVirtualNode;
begin
  inherited;
  if Node.CheckState in [csUncheckedNormal,
    csUncheckedPressed, csCheckedNormal, csCheckedPressed] then
  begin
    if Node.ChildCount > 0 then
    begin
      ChildNode := Node.FirstChild;
      while Assigned(ChildNode) do
      begin
        ChildNode.CheckState := Node.CheckState;
        ChildNode := ChildNode.NextSibling;
      end;
    end;
  end;
end;

procedure TfrmExportCSV.vstDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmExportCSV.vstDataSetsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmExportCSV.WriteString(const Value: String);
begin
  if Length(Value) > 0 then
  begin
    FFileStream.Write(Value[1], ByteLength(Value));
  end;
end;

procedure TfrmExportCSV.NewLine;
begin
  WriteString(sLineBreak);
end;


end.
