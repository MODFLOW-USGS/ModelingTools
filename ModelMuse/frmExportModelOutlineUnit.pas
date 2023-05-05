unit frmExportModelOutlineUnit;

interface

uses
  Winapi.Windows, System.UITypes, Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, XBase1, frmGoPhastUnit, FastGEO,
  System.Generics.Collections, AbstractGridUnit, GPC_Classes, ShapefileUnit,
  PhastModelUnit, Vcl.Mask, JvExMask, JvSpin;

type
  TPointList = TList<TPoint2D>;

  TExportChoice = (ecEntireGrid, edOutlinecurrentLayer, ecActiveCells,
    ecActiveAndInactive, ecGridLinesAll, ecGridLinesActive);

  TfrmExportModelOutline = class(TfrmCustomGoPhast)
    rgExportChoice: TRadioGroup;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    sdShapefile: TSaveDialog;
    xbsShapeFile: TXBase;
    comboModelSelection: TComboBox;
    lblModel: TLabel;
    seGridLines: TJvSpinEdit;
    lblGridLines: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rgExportChoiceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FGrid: TCustomModelGrid;
    FOutline: TGpcPolygonClass;
    FGeomWriter: TShapefileGeometryWriter;
    FModel: TCustomModel;
    FRowIndex: Integer;
    FColIndex: Integer;
    procedure SetData;
    procedure GetEntireGridOutline(var Polygon: TGpcPolygonClass);
    procedure GetActiveGridOutline(var ActiveCells, InactiveCells: TGpcPolygonClass;
      out InactiveCellCount: Integer);
    procedure GetActiveAndInactiveGrid;
    procedure GetGridLines;
    procedure GetSutraMeshOutline;
    procedure GetDisvOutline;
    procedure GetDisvInactiveOutline;
    procedure StoreAShape;
    procedure AppendDataBaseRecord(Active: Boolean);
    procedure InitializeDataBase;
    procedure FinalizeDataBase;
    procedure InitializeGeometryWriter;
    procedure FinalizeGeometryWriter;
    procedure UpdateChoices;
    procedure GetActiveOutlineOfCurrentLayer(var ActiveCells: TGpcPolygonClass;
      out InactiveCellCount: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExportModelOutline: TfrmExportModelOutline;

implementation

uses
  DataSetUnit, gpc, SutraMeshUnit, GoPhastTypes, ModflowIrregularMeshUnit,
  DataSetNamesUnit;

const
  StrAREA = 'AREA';
  StrACTIVE = 'ACTIVE';
  StrRow = 'ROW';
  StrColumn = 'COLUMN';

{$R *.dfm}

procedure TfrmExportModelOutline.btnOKClick(Sender: TObject);
begin
  inherited;
  if sdShapefile.Execute then
  begin
    SetData
  end
  else
  begin
    ModalResult := mrNone;
  end;
end;

procedure TfrmExportModelOutline.FormCreate(Sender: TObject);
var
  Model: TPhastModel;
  ChildIndex: Integer;
  AModel: TChildModel;
begin
  inherited;
  FOutline := TGpcPolygonClass.Create;

  Model := frmGoPhast.PhastModel;
  if Model.LgrUsed then
  begin
    comboModelSelection.Items.AddObject('Parent', Model);
    for ChildIndex := 0 to Model.ChildModels.Count - 1 do
    begin
      AModel := Model.ChildModels[ChildIndex].ChildModel;
      if AModel <> nil then
      begin
        comboModelSelection.Items.AddObject(AModel.ModelName, AModel);
      end;
    end;
    comboModelSelection.Enabled := True;
  end
  else
  begin
    comboModelSelection.Items.AddObject('Model', Model);
    comboModelSelection.Enabled := False;
  end;
  comboModelSelection.ItemIndex := 0;
end;

procedure TfrmExportModelOutline.FormDestroy(Sender: TObject);
begin
  inherited;
  FOutline.Free;
end;

procedure TfrmExportModelOutline.FormShow(Sender: TObject);
begin
  inherited;
  UpdateChoices;
end;

procedure TfrmExportModelOutline.GetActiveAndInactiveGrid;
var
  ActiveCells: TGpcPolygonClass;
  GridOutline: TGpcPolygonClass;
  InactiveCount: Integer;
  Temp: TGpcPolygonClass;
  InactiveCells: TGpcPolygonClass;
begin
  ActiveCells := TGpcPolygonClass.Create;
  GridOutline := TGpcPolygonClass.Create;
  InactiveCells := TGpcPolygonClass.Create;
  try
    InactiveCount := 0;
    GetActiveGridOutline(ActiveCells, InactiveCells, InactiveCount);
    if InactiveCount = 0 then
    begin
      GetEntireGridOutline(GridOutline);
      Temp := FOutline;
      try
        FOutline := GridOutline;
        StoreAShape;
        AppendDataBaseRecord(True);
      finally
        FOutline := Temp;
      end;
    end
    else
    begin
      Temp := FOutline;
      try
        FOutline := ActiveCells;
        StoreAShape;
        AppendDataBaseRecord(True);

        FOutline := InactiveCells;
        StoreAShape;
        AppendDataBaseRecord(False);
      finally
        FOutline := Temp;
      end;
    end;
  finally
    ActiveCells.Free;
    GridOutline.Free;
    InactiveCells.Free;
  end;
end;

procedure TfrmExportModelOutline.GetActiveGridOutline(
  var ActiveCells, InactiveCells: TGpcPolygonClass; out InactiveCellCount: Integer);
var
  ActiveDataArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  IsActive: Boolean;
  LayerIndex: Integer;
  Cell: TGpcPolygonClass;
  NewOutline: TGpcPolygonClass;
  APoint: TPoint2D;
begin
  InactiveCellCount := 0;
  ActiveCells.NumberOfContours := 0;
  Cell := TGpcPolygonClass.Create;
  try
    Cell.NumberOfContours := 1;
    Cell.VertexCount[0] := 4;
    if FModel.ModelSelection <> msModflow2015 then
    begin
      ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(rsActive);
    end
    else
    begin
      ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    end;
    for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
      begin
        IsActive := False;
        for LayerIndex := 0 to ActiveDataArray.LayerCount - 1 do
        begin
          if FModel.ModelSelection <> msModflow2015 then
          begin
            IsActive := ActiveDataArray.BooleanData[LayerIndex,RowIndex,ColIndex];
          end
          else
          begin
            IsActive := ActiveDataArray.IntegerData[LayerIndex,RowIndex,ColIndex] > 0;
          end;
          if IsActive then
          begin
            break;
          end;
        end;
        APoint := FGrid.TwoDElementCorner(ColIndex, RowIndex);
        Cell.Vertices[0,0] := APoint;
        APoint := FGrid.TwoDElementCorner(ColIndex+1, RowIndex);
        Cell.Vertices[0,1] := APoint;
        APoint := FGrid.TwoDElementCorner(ColIndex+1, RowIndex+1);
        Cell.Vertices[0,2] := APoint;
        APoint := FGrid.TwoDElementCorner(ColIndex, RowIndex+1);
        Cell.Vertices[0,3] := APoint;
        if IsActive then
        begin
          NewOutline := TGpcPolygonClass.CreateFromOperation(
            GPC_UNION, Cell, ActiveCells);
          ActiveCells.Free;
          ActiveCells := NewOutline;
        end
        else
        begin
          NewOutline := TGpcPolygonClass.CreateFromOperation(
            GPC_UNION, Cell, InactiveCells);
          InactiveCells.Free;
          InactiveCells := NewOutline;
          Inc(InactiveCellCount);
        end;
      end;
    end;
  finally
    Cell.Free;
  end;
end;

procedure TfrmExportModelOutline.GetDisvInactiveOutline;
var
  Outline: TPolygon2Darray;
  PolyIndex: Integer;
  APolygon: TPolygon2D;
  PointIndex: Integer;
begin
  FModel.DisvGrid.GetInactiveOutlineTop(Outline);
  FOutline.NumberOfContours := Length(Outline);
  for PolyIndex := 0 to FOutline.NumberOfContours  - 1 do
  begin
    APolygon := Outline[PolyIndex];
    FOutline.VertexCount[PolyIndex] := Length(APolygon);
    for PointIndex := 0 to Length(APolygon) - 1 do
    begin
      FOutline.Vertices[PolyIndex, PointIndex] := APolygon[PointIndex];
    end;
  end;
end;

procedure TfrmExportModelOutline.GetDisvOutline;
var
  Outline: TPolygon2Darray;
  PolyIndex: Integer;
  APolygon: TPolygon2D;
  PointIndex: Integer;
begin
  FModel.DisvGrid.GetModelOutlineTop(Outline);
//  case rgExportChoice.ItemIndex of
//    0:
//      begin
//        FModel.DisvGrid.GetModelOutlineTop(Outline);
//      end;
//    1:
//      begin
//        FModel.DisvGrid.GetCellsTop(-1, Outline, True);
//      end;
//    2:
//      begin
//        FModel.DisvGrid.GetCellsTop(-1, Outline, False);
//      end;
//    else
//      Assert(False);
//  end;
  FOutline.NumberOfContours := Length(Outline);
  for PolyIndex := 0 to FOutline.NumberOfContours  - 1 do
  begin
    APolygon := Outline[PolyIndex];
    FOutline.VertexCount[PolyIndex] := Length(APolygon);
    for PointIndex := 0 to Length(APolygon) - 1 do
    begin
      FOutline.Vertices[PolyIndex, PointIndex] := APolygon[PointIndex];
    end;
  end;
end;

procedure TfrmExportModelOutline.StoreAShape;
var
  PartIndex: Integer;
  AShape: TShapeObject;
  StartIndex: Integer;
  PointIndex: Integer;
  VertexIndex: Integer;
  APoint: TPoint2D;
  AShapePoint: TShapePoint;
begin
  AShape := TShapeObject.Create;
  AShape.FShapeType := stPolygon;
  AShape.FNumParts := FOutline.NumberOfContours;
  SetLength(AShape.FParts, AShape.FNumParts);
  AShape.FNumPoints := 0;
  StartIndex := 0;
  for PartIndex := 0 to FOutline.NumberOfContours - 1 do
  begin
    AShape.FParts[PartIndex] := StartIndex;
    AShape.FNumPoints := AShape.FNumPoints + FOutline.VertexCount[PartIndex]+1;
    StartIndex := AShape.FNumPoints;
  end;
  SetLength(AShape.FPoints, AShape.FNumPoints);
  PointIndex := 0;
  for PartIndex := 0 to FOutline.NumberOfContours - 1 do
  begin
    for VertexIndex := 0 to FOutline.VertexCount[PartIndex] - 1 do
    begin
      APoint := FOutline.Vertices[PartIndex,VertexIndex];
      AShapePoint.x := APoint.x;
      AShapePoint.y := APoint.y;
      AShape.FPoints[PointIndex] := AShapePoint;
      Inc(PointIndex);
    end;
    APoint := FOutline.Vertices[PartIndex,0];
    AShapePoint.x := APoint.x;
    AShapePoint.y := APoint.y;
    AShape.FPoints[PointIndex] := AShapePoint;
    Inc(PointIndex);
  end;
  FGeomWriter.AddShape(AShape);
end;

procedure TfrmExportModelOutline.AppendDataBaseRecord(Active: Boolean);
begin
  xbsShapeFile.AppendBlank;
  case TExportChoice(rgExportChoice.ItemIndex) of
    ecEntireGrid:
      begin
        xbsShapeFile.UpdFieldStr(StrAREA, 'Grid Outline');
      end;
    ecActiveCells, edOutlinecurrentLayer, ecActiveAndInactive:
      begin
        if Active then
        begin
          xbsShapeFile.UpdFieldStr(StrAREA, 'Active');
        end
        else
        begin
          xbsShapeFile.UpdFieldStr(StrAREA, 'Inactive');
        end;
      end;
    ecGridLinesAll, ecGridLinesActive:
      begin
        xbsShapeFile.UpdFieldInt(StrRow, FRowIndex);
        xbsShapeFile.UpdFieldInt(StrColumn, FColIndex);
      end;
	else
	  Assert(False);
  end;

  xbsShapeFile.PostChanges;
end;

procedure TfrmExportModelOutline.InitializeDataBase;
var
  FieldDefinitions: TStringList;
  DataBaseFileName: string;
begin
  DataBaseFileName := ChangeFileExt(sdShapefile.FileName, '.dbf');
  if FileExists(DataBaseFileName) then
  begin
    DeleteFile(DataBaseFileName);
  end;
  FieldDefinitions := TStringList.Create;
  try
    if TExportChoice(rgExportChoice.ItemIndex) in
      [ecGridLinesAll, ecGridLinesActive] then
    begin
      FieldDefinitions.Add(StrRow + '=N');
      FieldDefinitions.Add(StrColumn + '=N');
    end
    else
    begin
      FieldDefinitions.Add(StrAREA + '=C20');
    end;

    xbsShapeFile.DBFCreate(DataBaseFileName, FieldDefinitions);
  finally
    FieldDefinitions.Free;
  end;
  xbsShapeFile.FileName := DataBaseFileName;
  xbsShapeFile.Active := True;
  xbsShapeFile.GotoBOF;
end;

procedure TfrmExportModelOutline.FinalizeDataBase;
begin
  xbsShapeFile.Active := False;
end;

procedure TfrmExportModelOutline.InitializeGeometryWriter;
begin
  if TExportChoice(rgExportChoice.ItemIndex) in
    [ecGridLinesAll, ecGridLinesActive] then
  begin
    FGeomWriter := TShapefileGeometryWriter.Create(stPolyLine, True);
  end
  else
  begin
    FGeomWriter := TShapefileGeometryWriter.Create(stPolygon, True);
  end;
end;

procedure TfrmExportModelOutline.rgExportChoiceClick(Sender: TObject);
begin
  inherited;
  seGridLines.Enabled := TExportChoice(rgExportChoice.ItemIndex) in
    [ecGridLinesAll, ecGridLinesActive];
  lblGridLines.Enabled := seGridLines.Enabled;
end;

procedure TfrmExportModelOutline.FinalizeGeometryWriter;
begin
  FGeomWriter.WriteToFile(sdShapefile.FileName,
    ChangeFileExt(sdShapefile.FileName, '.shx'));
  FGeomWriter.Free;
end;

procedure TfrmExportModelOutline.UpdateChoices;
var
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  if Model.ModelSelection in SutraSelection then
  begin
    rgExportChoice.Items[0] := 'Mesh outline';
    rgExportChoice.Items[1] := 'Active elements';
    rgExportChoice.Controls[1].Enabled := False;
    rgExportChoice.Items[2] := 'Active and inactive elements';
    rgExportChoice.Controls[2].Enabled := False;
    rgExportChoice.Controls[3].Enabled := False;
    rgExportChoice.Controls[4].Enabled := False;
  end
  else if Model.DisvUsed then
  begin
    rgExportChoice.Controls[3].Enabled := False;
    rgExportChoice.Controls[4].Enabled := False;
  end;
end;

procedure TfrmExportModelOutline.GetActiveOutlineOfCurrentLayer(
  var ActiveCells: TGpcPolygonClass;
  out InactiveCellCount: Integer);
var
  ActiveDataArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  IsActive: Boolean;
  LayerIndex: Integer;
  Cell: TGpcPolygonClass;
  NewOutline: TGpcPolygonClass;
  APoint: TPoint2D;
begin
  InactiveCellCount := 0;
  ActiveCells.NumberOfContours := 0;
  Cell := TGpcPolygonClass.Create;
  try
    Cell.NumberOfContours := 1;
    Cell.VertexCount[0] := 4;
    if FModel.ModelSelection <> msModflow2015 then
    begin
      ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(rsActive);
    end
    else
    begin
      ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    end;
    for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
      begin
        IsActive := False;
        LayerIndex := FModel.SelectedLayer;
        if FModel.ModelSelection <> msModflow2015 then
        begin
          IsActive := ActiveDataArray.BooleanData[LayerIndex,RowIndex,ColIndex];
        end
        else
        begin
          IsActive := ActiveDataArray.IntegerData[LayerIndex,RowIndex,ColIndex] > 0;
        end;
        if IsActive then
        begin
          APoint := FGrid.TwoDElementCorner(ColIndex, RowIndex);
          Cell.Vertices[0,0] := APoint;
          APoint := FGrid.TwoDElementCorner(ColIndex+1, RowIndex);
          Cell.Vertices[0,1] := APoint;
          APoint := FGrid.TwoDElementCorner(ColIndex+1, RowIndex+1);
          Cell.Vertices[0,2] := APoint;
          APoint := FGrid.TwoDElementCorner(ColIndex, RowIndex+1);
          Cell.Vertices[0,3] := APoint;
          NewOutline := TGpcPolygonClass.CreateFromOperation(
            GPC_UNION, Cell, ActiveCells);
          ActiveCells.Free;
          ActiveCells := NewOutline;
        end;
      end;
    end;
  finally
    Cell.Free;
  end;
end;

procedure TfrmExportModelOutline.GetEntireGridOutline(var Polygon: TGpcPolygonClass);
var
  ColIndex: Integer;
  APoint: TPoint2D;
  RowIndex: Integer;
  PointList: TPointList;
  index: Integer;
begin
  PointList := TPointList.Create;
  try
    case FGrid.RowDirection of
      rdSouthToNorth:
        begin
          // PHAST
          for ColIndex := FGrid.ColumnCount downto 0 do
          begin
            APoint := FGrid.TwoDElementCorner(ColIndex, 0);
            PointList.Add(APoint);
          end;
          for RowIndex := FGrid.RowCount downto 1 do
          begin
            APoint := FGrid.TwoDElementCorner(FGrid.ColumnCount, RowIndex);
            PointList.Add(APoint);
          end;
          for ColIndex := 0 to FGrid.ColumnCount -1 do
          begin
            APoint := FGrid.TwoDElementCorner(ColIndex, FGrid.RowCount);
            PointList.Add(APoint);
          end;
          for RowIndex := 1 to FGrid.RowCount -1 do
          begin
            APoint := FGrid.TwoDElementCorner(0, RowIndex);
            PointList.Add(APoint);
          end;
        end;
      rdNorthToSouth:
        begin
          // MODFLOW
          for ColIndex := 0 to FGrid.ColumnCount do
          begin
            APoint := FGrid.TwoDElementCorner(ColIndex, 0);
            PointList.Add(APoint);
          end;
          for RowIndex := 1 to FGrid.RowCount do
          begin
            APoint := FGrid.TwoDElementCorner(FGrid.ColumnCount, RowIndex);
            PointList.Add(APoint);
          end;
          for ColIndex := FGrid.ColumnCount -1 downto 0 do
          begin
            APoint := FGrid.TwoDElementCorner(ColIndex, FGrid.RowCount);
            PointList.Add(APoint);
          end;
          for RowIndex := FGrid.RowCount -1 downto 1 do
          begin
            APoint := FGrid.TwoDElementCorner(0, RowIndex);
            PointList.Add(APoint);
          end;
        end;
    end;
    Polygon.NumberOfContours := 1;
    Polygon.VertexCount[0] := PointList.Count;
    for index := 0 to PointList.Count - 1 do
    begin
      Polygon.Vertices[0,index] := PointList[index];
    end;
  finally
    PointList.Free;
  end;

end;

procedure TfrmExportModelOutline.GetGridLines;
var
  RowIndex: Integer;
  ColIndex: Integer;
  Frequency: Integer;
  AShape: TShapeObject;
  APoint: TPoint2D;
  AShapePoint: TShapePoint;
  Active: TDataArray;
  PriorActive: Boolean;
  PointIndex: Integer;
  function IsActive(Col,Row: Integer): Boolean;
  var
    LayerIndex: Integer;
  begin
    Result := False;
    if (Col < FGrid.ColumnCount) and (Row < FGrid.RowCount) then
    begin
      for LayerIndex := 0 to Active.LayerCount - 1 do
      begin
        if FModel.ModelSelection <> msModflow2015 then
        begin
          Result := Active.BooleanData[LayerIndex, Row, Col];
        end
        else
        begin
          Result := Active.IntegerData[LayerIndex, Row, Col] > 0;
        end;
        if Result then
        begin
          Exit;
        end;
      end;
    end;
    if (Col > 0) and (Row < FGrid.RowCount) then
    begin
      for LayerIndex := 0 to Active.LayerCount - 1 do
      begin
        if FModel.ModelSelection <> msModflow2015 then
        begin
          Result := Active.BooleanData[LayerIndex, Row, Col-1];
        end
        else
        begin
          Result := Active.IntegerData[LayerIndex, Row, Col-1] > 0;
        end;
        if Result then
        begin
          Exit;
        end;
      end;
    end;
    if (Row > 0) and (Col < FGrid.ColumnCount) then
    begin
      for LayerIndex := 0 to Active.LayerCount - 1 do
      begin
        if FModel.ModelSelection <> msModflow2015 then
        begin
          Result := Active.BooleanData[LayerIndex, Row-1, Col];
        end
        else
        begin
          Result := Active.IntegerData[LayerIndex, Row-1, Col] > 0;
        end;
        if Result then
        begin
          Exit;
        end;
      end;
    end;
    if (Col > 0) and (Row > 0) then
    begin
      for LayerIndex := 0 to Active.LayerCount - 1 do
      begin
        if FModel.ModelSelection <> msModflow2015 then
        begin
          Result := Active.BooleanData[LayerIndex, Row-1, Col-1];
        end
        else
        begin
          Result := Active.IntegerData[LayerIndex, Row-1, Col-1] > 0;
        end;
        if Result then
        begin
          Exit;
        end;
      end;
    end;
  end;
begin
  Frequency := seGridLines.AsInteger;
  FRowIndex := -1;
  FColIndex := -1;
  if FModel.ModelSelection <> msModflow2015 then
  begin
    Active := FModel.DataArrayManager.GetDataSetByName(rsActive);
  end
  else
  begin
    Active := FModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  end;
  For RowIndex := 0 to FGrid.RowCount do
  begin
  	if (RowIndex = 0) or (RowIndex = FGrid.RowCount) or ((RowIndex mod Frequency) = 0) then
    begin
      FRowIndex := RowIndex;
      AppendDataBaseRecord(False);
      AShape := TShapeObject.Create;
      AShape.FShapeType := stPolyLine;
      AShape.FNumParts := 0;
      if rgExportChoice.ItemIndex = Ord(ecGridLinesActive) then
      begin
        AShape.FNumPoints := 0;
        SetLength(AShape.FPoints, FGrid.ColumnCount + 1);
        PriorActive := False;
        PointIndex := 0;
        for ColIndex := 0 to FGrid.ColumnCount do
        begin
          if IsActive(ColIndex, FRowIndex) then
          begin
            APoint := FGrid.TwoDElementCorner(ColIndex, FRowIndex);
            AShapePoint.x := APoint.x;
            AShapePoint.y := APoint.y;
            AShape.FPoints[AShape.FNumPoints] := AShapePoint;
            AShape.FNumPoints := AShape.FNumPoints + 1;
            if not PriorActive then
            begin
              AShape.FNumParts := AShape.FNumParts + 1;
              SetLength(AShape.FParts, AShape.FNumParts);
              AShape.FParts[AShape.FNumParts-1] := PointIndex;
            end;
            PriorActive := True;
            Inc(PointIndex);
          end
          else
          begin
            PriorActive := False;
          end;
        end;
        SetLength(AShape.FPoints, AShape.FNumPoints);
        if AShape.FNumPoints = 0 then
        begin
          AShape.FShapeType := stNull;
        end;
      end
      else
      begin
        AShape.FNumPoints := 2;
        SetLength(AShape.FPoints, AShape.FNumPoints);
        APoint := FGrid.TwoDElementCorner(0, FRowIndex);
        AShapePoint.x := APoint.x;
        AShapePoint.y := APoint.y;
        AShape.FPoints[0] := AShapePoint;
        APoint := FGrid.TwoDElementCorner(FGrid.ColumnCount, FRowIndex);
        AShapePoint.x := APoint.x;
        AShapePoint.y := APoint.y;
        AShape.FPoints[1] := AShapePoint;
      end;
      FGeomWriter.AddShape(AShape);
    end;
  end;
  FRowIndex := -1;
  FColIndex := -1;
  For ColIndex := 0 to FGrid.ColumnCount do
  begin
	  if (ColIndex = 0) or (ColIndex = FGrid.ColumnCount) or ((ColIndex mod Frequency) = 0) then
    begin
      FColIndex := ColIndex;
      AppendDataBaseRecord(False);
      AShape := TShapeObject.Create;
      AShape.FShapeType := stPolyLine;
      AShape.FNumParts := 0;
      if rgExportChoice.ItemIndex = Ord(ecGridLinesActive) then
      begin
        AShape.FNumPoints := 0;
        SetLength(AShape.FPoints, FGrid.RowCount+1);
        PriorActive := False;
        PointIndex := 0;
        for RowIndex := 0 to FGrid.RowCount do
        begin
          if IsActive(FColIndex, RowIndex) then
          begin
            APoint := FGrid.TwoDElementCorner(FColIndex, RowIndex);
            AShapePoint.x := APoint.x;
            AShapePoint.y := APoint.y;
            AShape.FPoints[AShape.FNumPoints] := AShapePoint;
            AShape.FNumPoints := AShape.FNumPoints + 1;
            if not PriorActive then
            begin
              AShape.FNumParts := AShape.FNumParts + 1;
              SetLength(AShape.FParts, AShape.FNumParts);
              AShape.FParts[AShape.FNumParts-1] := PointIndex;
            end;
            PriorActive := True;
            Inc(PointIndex);
          end
          else
          begin
            PriorActive := False;
          end;
        end;
        SetLength(AShape.FPoints, AShape.FNumPoints);
      end
      else
      begin
        AShape.FNumPoints := 2;
        SetLength(AShape.FPoints, AShape.FNumPoints);
        APoint := FGrid.TwoDElementCorner(FColIndex, 0);
        AShapePoint.x := APoint.x;
        AShapePoint.y := APoint.y;
        AShape.FPoints[0] := AShapePoint;
        APoint := FGrid.TwoDElementCorner(FColIndex, FGrid.RowCount);
        AShapePoint.x := APoint.x;
        AShapePoint.y := APoint.y;
        AShape.FPoints[1] := AShapePoint;
      end;
      FGeomWriter.AddShape(AShape);
    end;
  end;
end;

procedure TfrmExportModelOutline.GetSutraMeshOutline;
var
  Cell: TGpcPolygonClass;
  NewOutline: TGpcPolygonClass;
  APoint: TPoint2D;
  Mesh: TSutraMesh2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
begin
  Mesh := (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).Mesh2D;
  Assert(Mesh <> nil);
  FOutline.NumberOfContours := 0;
  Cell := TGpcPolygonClass.Create;
  try
    Cell.NumberOfContours := 1;
    Cell.VertexCount[0] := 4;
    for ElementIndex := 0 to Mesh.Elements.Count - 1 do
    begin
      AnElement := Mesh.Elements[ElementIndex];
      for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
      begin
        ANode := AnElement.Nodes[NodeIndex].Node;
        APoint := ANode.Location;
        Cell.Vertices[0,NodeIndex] := APoint;
      end;
      NewOutline := TGpcPolygonClass.CreateFromOperation(
        GPC_UNION, Cell, FOutline);
      FOutline.Free;
      FOutline := NewOutline;
    end;
  finally
    Cell.Free;
  end;
end;

procedure TfrmExportModelOutline.SetData;
var
  Dummy: integer;
  InactiveCells: TGpcPolygonClass;
begin
  try
    InitializeDataBase;
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
  InitializeGeometryWriter;
  try
    FModel := comboModelSelection.Items.Objects[
      comboModelSelection.ItemIndex] as TCustomModel;
    FGrid := FModel.Grid;
    if FGrid <> nil then
    begin
      case TExportChoice(rgExportChoice.ItemIndex) of
        ecEntireGrid:
          begin
            GetEntireGridOutline(FOutline);
            StoreAShape;
            AppendDataBaseRecord(False);
          end;
        edOutlinecurrentLayer:
          begin
            GetActiveOutlineOfCurrentLayer(FOutline, Dummy);
            StoreAShape;
            AppendDataBaseRecord(True);
          end;
        ecActiveCells:
          begin
            Dummy := 0;
            InactiveCells := TGpcPolygonClass.Create;
            try
              GetActiveGridOutline(FOutline, InactiveCells, Dummy);
            finally
              InactiveCells.Free;
            end;
            StoreAShape;
            AppendDataBaseRecord(True);
          end;
        ecActiveAndInactive:
          begin
            GetActiveAndInactiveGrid;
          end;
        ecGridLinesAll, ecGridLinesActive:
          begin
            GetGridLines;
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      if FModel.ModelSelection in SutraSelection then
      begin
        GetSutraMeshOutline;
      end
      else
      begin
        case TExportChoice(rgExportChoice.ItemIndex) of
          ecEntireGrid:
            begin
              FGrid := FModel.ModflowGrid;
              GetEntireGridOutline(FOutline);
            end;
          ecActiveCells:
            begin
              GetDisvOutline;
            end;
          ecActiveAndInactive:
            begin
              GetDisvInactiveOutline;
              StoreAShape;
              AppendDataBaseRecord(False);
              GetDisvOutline;
            end;
          else
            Assert(False)
        end;
      end;
      StoreAShape;
      AppendDataBaseRecord(True);
    end;
  finally
    FinalizeGeometryWriter;
    FinalizeDataBase;
  end;
end;

end.
