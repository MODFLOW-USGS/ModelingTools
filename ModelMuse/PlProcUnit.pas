{
This unit defines function that can be used to communicate with PLPROC.
http://www.pesthomepage.org/About_Us.php
}
unit PlProcUnit;

interface

uses
  ModflowIrregularMeshUnit, CustomModflowWriterUnit, System.SysUtils, FastGEO,
  AbstractGridUnit, GoPhastTypes, MeshRenumberingTypes, System.Classes,
  DataSetUnit;

type
  // @name is used to write a file that can be read by read_mf_usg_grid_specs().
  TUsgGridSpecWrite = class(TCustomFileWriter)
  private
    FDISV: TModflowDisvGrid;
    procedure WriteLine1;
    procedure WriteLine2;
    procedure WriteLine3;
    procedure WriteVertices;
    procedure WriteNodes;
  public
    procedure WriteUsgGridSpecs(DISV: TModflowDisvGrid; FileName: string);
  end;

  TInterpolationDataWriter = class(TCustomFileWriter)
  public
    class function Extension: string; override;
    // X and Y values are data point locations.
    // Z values are the data to be interpolated.
    procedure WriteFile(Points: TPoint3DArray; var FileName: string);
  end;

  TResultLocationWriter = class(TCustomFileWriter)
  public
    class function Extension: string; override;
    procedure WriteFile(Points: TPoint2DArray; var FileName: string); overload;
    procedure WriteFile(Grid: TCustomModelGrid; EvaluatedAt: TEvaluatedAt;
      Orientation: TDataSetOrientation; var FileName: string); overload;
    procedure WriteFile(Mesh: IMesh2D; EvaluatedAt: TEvaluatedAt;
      var FileName: string); overload;
  end;

  TParameterZoneWriter = class(TCustomFileWriter)
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(var AFileName: string; DataArray: TDataArray);
  end;

implementation

uses
  ModflowParameterUnit, OrderedCollectionUnit;

{ TUsgGridSpecWrite }

procedure TUsgGridSpecWrite.WriteLine1;
begin
  WriteString('UNSTRUCTURED GWF');
  NewLine;
end;

procedure TUsgGridSpecWrite.WriteLine2;
var
  nnode: Integer;
  nlay: Integer;
  iz: Integer;
  ic: Integer;
begin
  nnode := FDISV.LayerCount * FDISV.TwoDGrid.ElementCount;
  nlay := FDISV.LayerCount;
  iz := 1;
  ic := 1;
  WriteInteger(nnode);
  WriteInteger(nlay);
  WriteInteger(iz);
  WriteInteger(ic);
  NewLine;
end;

procedure TUsgGridSpecWrite.WriteLine3;
var
  nvertex: Integer;
begin
  nvertex := FDISV.TwoDGrid.CellCorners.Count * (FDISV.LayerCount + 1);
  WriteInteger(nvertex);
  NewLine;
end;

procedure TUsgGridSpecWrite.WriteNodes;
var
  Cells: TModflowIrregularCell2DCollection;
  LayerIndex: Integer;
  CellIndex: Integer;
  ACell2D: TModflowIrregularCell2D;
  inode: Integer;
  x: Double;
  y: Double;
  z: Double;
  lay: Integer;
  m: Integer;
  ivertex: Integer;
  ACell3D: TModflowDisVCell;
  VertexIndex: Integer;
  ANode2D: TModflowNode;
begin
  Cells := FDISV.TwoDGrid.Cells;
  inode := 1;
  for LayerIndex := 0 to FDISV.LayerCount - 1 do
  begin
    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell2D := Cells[CellIndex];
      ACell3D := FDISV.Cells[LayerIndex, ACell2D.ElementNumber];
      x := ACell2D.X;
      y := ACell2D.Y;
      z := ACell3D.CenterElevation;
      lay := LayerIndex + 1;
      m := ACell2D.NodeCount*2;
      WriteInteger(inode);
      WriteFloat(x);
      WriteFloat(y);
      WriteFloat(z);
      WriteInteger(lay);
      WriteInteger(m);
      for VertexIndex := 0 to ACell2D.NodeCount - 1 do
      begin
        ANode2D := ACell2D.ElementCorners[VertexIndex];
        ivertex := LayerIndex * Cells.Count + ANode2D.NodeNumber + 1;
        WriteInteger(ivertex);
      end;
      for VertexIndex := 0 to ACell2D.NodeCount - 1 do
      begin
        ANode2D := ACell2D.ElementCorners[VertexIndex];
        ivertex := (LayerIndex + 1) * Cells.Count + ANode2D.NodeNumber + 1;
        WriteInteger(ivertex);
      end;
      NewLine;

      Inc(inode);
    end;
  end;

end;

procedure TUsgGridSpecWrite.WriteUsgGridSpecs(DISV: TModflowDisvGrid; FileName: string);
begin
  FDISV := DISV;
  FileName := ChangeFileExt(FileName, '.gsf');

  OpenFile(FileName);
  try
    WriteLine1;
    WriteLine2;
    WriteLine3;
    WriteVertices;
    WriteNodes;
  finally
    CloseFile;
  end;
end;

procedure TUsgGridSpecWrite.WriteVertices;
var
  LayerIndex: Integer;
  VertexIndex: Integer;
  CellCorners: TModflowNodes;
  Node2D: TModflowNode;
  X: double;
  Y: double;
  Z: double;
  CellIndex: Integer;
  ACell2D: TModflowIrregularCell2D;
  ACell3D: TModflowDisVCell;
begin
  CellCorners := FDISV.TwoDGrid.CellCorners;
  for LayerIndex := 0 to FDISV.LayerCount - 1 do
  begin
    for VertexIndex := 0 to CellCorners.Count - 1 do
    begin
      Node2D := CellCorners[VertexIndex];
      X := Node2D.X;
      Y := Node2D.Y;
      Z := 0;
      for CellIndex := 0 to Node2D.ActiveElementCount - 1 do
      begin
        ACell2D := Node2D.Cells[CellIndex];
        ACell3D := FDISV.Cells[LayerIndex, ACell2D.ElementNumber];
        Z := Z + ACell3D.Top;
      end;
      if Node2D.ActiveElementCount > 0 then
      begin
        Z := Z / Node2D.ActiveElementCount;
      end;
      WriteFloat(X);
      WriteFloat(Y);
      WriteFloat(Z);
      NewLine;
    end;
  end;
  LayerIndex := FDISV.LayerCount - 1;
  for VertexIndex := 0 to CellCorners.Count - 1 do
  begin
    Node2D := CellCorners[VertexIndex];
    X := Node2D.X;
    Y := Node2D.Y;
    Z := 0;
    for CellIndex := 0 to Node2D.ActiveElementCount - 1 do
    begin
      ACell2D := Node2D.Cells[CellIndex];
      ACell3D := FDISV.Cells[LayerIndex, ACell2D.ElementNumber];
      Z := Z + ACell3D.Bottom;
    end;
    if Node2D.ActiveElementCount > 0 then
    begin
      Z := Z / Node2D.ActiveElementCount;
    end;
    WriteFloat(X);
    WriteFloat(Y);
    WriteFloat(Z);
    NewLine;
  end;
end;

{ TInterpolationDataWriter }

class function TInterpolationDataWriter.Extension: string;
begin
  result := '.ip_data';
end;

procedure TInterpolationDataWriter.WriteFile(Points: TPoint3DArray;
  var FileName: string);
var
  VertexIndex: Integer;
  APoint: TPoint3D;
begin
  FileName := ChangeFileExt(FileName, Extension);

//  FInputFileName := FileName;
  OpenFile(FileName);
  try
    WriteString('        ID           X               Y           Value');
    NewLine;

    for VertexIndex := 0 to Length(Points) - 1 do
    begin
      WriteInteger(VertexIndex+1);
      APoint := Points[VertexIndex];
      WriteFloat(APoint.x);
      WriteFloat(APoint.y);
      WriteFloat(APoint.z);
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

{ TResultLocationWriter }

procedure TResultLocationWriter.WriteFile(Points: TPoint2DArray;
  var FileName: string);
var
  VertexIndex: Integer;
  APoint: TPoint2D;
begin
  FileName := ChangeFileExt(FileName, Extension);
//  FInputFileName := FileName;
  OpenFile(FileName);
  try
    WriteString('        ID           X               Y');
    NewLine;

    for VertexIndex := 0 to Length(Points) - 1 do
    begin
      WriteInteger(VertexIndex+1);
      APoint := Points[VertexIndex];
      WriteFloat(APoint.x);
      WriteFloat(APoint.y);
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

class function TResultLocationWriter.Extension: string;
begin
  Result := '.result_locations';
end;

procedure TResultLocationWriter.WriteFile(Mesh: IMesh2D;
  EvaluatedAt: TEvaluatedAt; var FileName: string);
var
  ElementIndex: Integer;
  Element: IElement2D;
  Points: TPoint2DArray;
  NodeIndex: Integer;
  Node: INode2D;
begin
  if EvaluatedAt = eaBlocks then
  begin
    SetLength(Points, Mesh.ElementCount);
    for ElementIndex := 0 to Mesh.ElementCount - 1 do
    begin
      Element := Mesh.ElementsI2D[ElementIndex];
      Points[ElementIndex] := Element.Center;
    end;
  end
  else
  begin
    Assert(EvaluatedAt = eaNodes);
    SetLength(Points, Mesh.NodeCount);
    for NodeIndex := 0 to Mesh.NodeCount - 1 do
    begin
      Node := Mesh.NodesI2D[NodeIndex];
      Points[NodeIndex] := Node.Location;
    end;
  end;
  WriteFile(Points, FileName);
end;

procedure TResultLocationWriter.WriteFile(Grid: TCustomModelGrid;
  EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  var FileName: string);
var
  Points: TPoint2DArray;
  RowIndex: Integer;
  ColIndex: Integer;
  PointIndex: Integer;
  LayerIndex: Integer;
  APoint3D: TPoint3D;
begin
  Assert(Orientation <> dso3D);
  PointIndex := 0;
  case EvaluatedAt of
    eaBlocks:
      begin
        case Orientation of
          dsoTop:
            begin
              SetLength(Points, Grid.ColumnCount * Grid.RowCount);
              for RowIndex := 0 to Grid.RowCount - 1 do
              begin
                for ColIndex := 0 to Grid.ColumnCount - 1 do
                begin
                  Points[PointIndex] :=
                    Grid.TwoDElementCenter(ColIndex,RowIndex);
                  Inc(PointIndex);
                end;
              end;
            end;
          dsoFront:
            begin
              SetLength(Points, Grid.ColumnCount * Grid.LayerCount);
              for LayerIndex := 0 to Grid.LayerCount - 1 do
              begin
                for ColIndex := 0 to Grid.ColumnCount - 1 do
                begin
                  APoint3D := Grid.ThreeDElementCenter(ColIndex, 0, LayerIndex);
                  Points[PointIndex].x := APoint3D.x;
                  Points[PointIndex].y := APoint3D.z;
                  Inc(PointIndex);
                end;
              end;
            end;
          dsoSide:
            begin
              SetLength(Points, Grid.RowCount * Grid.LayerCount);
              for LayerIndex := 0 to Grid.LayerCount - 1 do
              begin
                for RowIndex := 0 to Grid.RowCount - 1 do
                begin
                  APoint3D := Grid.ThreeDElementCenter(0, RowIndex, LayerIndex);
                  Points[PointIndex].x := APoint3D.y;
                  Points[PointIndex].y := APoint3D.z;
                  Inc(PointIndex);
                end;
              end;
            end;
          else
            Assert(False);
        end;
      end;
    eaNodes:
      begin
        case Orientation of
          dsoTop:
            begin
              SetLength(Points, (Grid.ColumnCount + 1) * (Grid.RowCount + 1));
              for RowIndex := 0 to Grid.RowCount do
              begin
                for ColIndex := 0 to Grid.ColumnCount do
                begin
                  Points[PointIndex] :=
                    Grid.TwoDCellCorner(ColIndex,RowIndex);
                  Inc(PointIndex);
                end;
              end;
            end;
          dsoFront:
            begin
              SetLength(Points, (Grid.ColumnCount + 1) * (Grid.LayerCount + 1));
              for LayerIndex := 0 to Grid.LayerCount do
              begin
                for ColIndex := 0 to Grid.ColumnCount do
                begin
                  APoint3D := Grid.ThreeDElementCorner(ColIndex, 0, LayerIndex);
                  Points[PointIndex].x := APoint3D.x;
                  Points[PointIndex].y := APoint3D.z;
                  Inc(PointIndex);
                end;
              end;
            end;
          dsoSide:
            begin
              SetLength(Points, (Grid.RowCount + 1) * (Grid.LayerCount + 1));
              for LayerIndex := 0 to Grid.LayerCount do
              begin
                for RowIndex := 0 to Grid.RowCount do
                begin
                  APoint3D := Grid.ThreeDElementCorner(0, RowIndex, LayerIndex);
                  Points[PointIndex].x := APoint3D.y;
                  Points[PointIndex].y := APoint3D.z;
                  Inc(PointIndex);
                end;
              end;
            end;
          else
            Assert(False);
        end;
      end
    else
      Assert(False);
  end;
  WriteFile(Points, FileName);
end;

{ TParameterZoneWriter }

class function TParameterZoneWriter.Extension: string;
begin
  Result := '.PstZone';
end;

procedure TParameterZoneWriter.WriteFile(var AFileName: string;
  DataArray: TDataArray);
var
  PNames: TStringList;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  RowIndex: Integer;
  ColIndex: Integer;
  ID: Integer;
  LayerIndex: Integer;
  AName: string;
  ParamDataArray: TDataArray;
  ParamListFileName: string;
  ScriptFileName: string;
  GrbFileName: string;
  UsedParamList: TStringList;
  ParameterIndex: Integer;
  ModelInputFileName: string;
begin
  ParamDataArray := Model.DataArrayManager.GetDataSetByName
    (DataArray.ParamDataSetName);
  Assert(ParamDataArray <> nil);
  ParamDataArray.Initialize;
  ParamListFileName := ChangeFileExt(AFileName, '.' + DataArray.Name) + Extension;;
  PNames := TStringList.Create;
  UsedParamList := TStringList.Create;
  try
    UsedParamList.Duplicates := dupIgnore;
    UsedParamList.Sorted := True;
    for PIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[PIndex];
      if AParam.ParameterType = ptPEST then
      begin
        PNames.AddObject(LowerCase(AParam.ParameterName), AParam);
      end;
    end;
    PNames.Sorted := True;

//    AFileName := AFileName + Extension;
    OpenFile(ParamListFileName);
    try
      ID := 1;
      for RowIndex := 0 to ParamDataArray.RowCount - 1 do
      begin
        for ColIndex := 0 to ParamDataArray.ColumnCount - 1 do
        begin
          WriteInteger(ID);

          for LayerIndex := 0 to ParamDataArray.LayerCount - 1 do
          begin
            AName := LowerCase(ParamDataArray.StringData[LayerIndex,RowIndex,ColIndex]);
            if (AName = '') then
            begin
              PIndex := -1
            end
            else
            begin
              PIndex := PNames.IndexOf(AName);
            end;

            if PIndex >= 0 then
            begin
              AParam := PNames.Objects[PIndex] as TModflowSteadyParameter;
              UsedParamList.AddObject(AParam.ParameterName, AParam);
              if AParam.Value <> 0 then
              begin
                WriteFloat(DataArray.RealData[LayerIndex,RowIndex,ColIndex]
                  / AParam.Value);
              end
              else
              begin
                WriteFloat(DataArray.RealData[LayerIndex,RowIndex,ColIndex]);
              end;
            end
            else
            begin
              WriteFloat(DataArray.RealData[LayerIndex,RowIndex,ColIndex]);
            end;

            WriteInteger(PIndex+1);
          end;

          Inc(ID);
          NewLine;
        end;
      end;
    finally
      CloseFile;
    end;

    Model.DataArrayManager.AddDataSetToCache(DataArray);
    Model.DataArrayManager.AddDataSetToCache(ParamDataArray);

    ScriptFileName := ChangeFileExt(ParamListFileName, '.tpl');
    OpenFile(ScriptFileName);
    try
      WriteString('ptf ');
      WriteString(Model.PestProperties.TemplateCharacter);
      NewLine;
      WriteString('#Script for PLPROC');
      NewLine;
      NewLine;
      //
      WriteString('#Read MODFLOW 6 grid information file');
      NewLine;
      GrbFileName := ChangeFileExt(AFileName, '');
      if Model.DisvUsed then
      begin
        GrbFileName := ChangeFileExt(GrbFileName, '.disv.grb');
      end
      else
      begin
        GrbFileName := ChangeFileExt(GrbFileName, '.dis.grb');
      end;
      GrbFileName := ExtractFileName(GrbFileName);

      WriteString(Format('cl_mf6 = read_mf6_grid_specs(file=''%s'', &',
        [GrbFileName]));
      NewLine;
      WriteString('    dimensions=2, &');
      NewLine;
      for LayerIndex := 0 to Model.LayerCount - 1 do
      begin
        WriteString(Format('    slist_layer_idomain=id%0:d; layer=%0:d, &', [LayerIndex+1]));
        NewLine;
      end;
      for LayerIndex := 0 to Model.LayerCount - 1 do
      begin
        WriteString(Format('    plist_layer_bottom =bot%0:d; layer=%0:d, &', [LayerIndex+1]));
        NewLine;
      end;
      WriteString('    plist_top = top)');
      NewLine;
      NewLine;

      WriteString('#Read data to modify');
      NewLine;
      WriteString('read_list_file(reference_clist=''cl_mf6'',skiplines=0, &');
      NewLine;
      ColIndex := 2;
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        WriteString(Format('  plist=p_Value%0:d;column=%1:d, &', [LayerIndex+1, ColIndex]));
        Inc(ColIndex);
        NewLine;

        WriteString(Format('  slist=s_PIndex%0:d;column=%1:d, &', [LayerIndex+1, ColIndex]));
        Inc(ColIndex);
        NewLine;
      end;
      WriteString(Format('  file=''%s'')', [ParamListFileName]));
      NewLine;
      NewLine;

      WriteString('#Read parameter values');
      NewLine;
      for ParameterIndex := 0 to UsedParamList.Count - 1 do
      begin
        AParam := UsedParamList.Objects[ParameterIndex] as TModflowSteadyParameter;
        WriteString(Format('%0:s = %1:s                        %0:s%1:s', [AParam.ParameterName, Model.PestProperties.TemplateCharacter]));
        NewLine;
      end;
      NewLine;

      WriteString('#Modfify data values');
      NewLine;
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        for ParameterIndex := 0 to UsedParamList.Count - 1 do
        begin
          AParam := UsedParamList.Objects[ParameterIndex] as TModflowSteadyParameter;
          PIndex := PNames.IndexOf(AParam.ParameterName) +1;
          WriteString(Format('p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = p_Value%0:d * %2:s',
            [LayerIndex+1, PIndex, AParam.ParameterName]));
          NewLine
        end;
      end;
      NewLine;

      WriteString('#Write new data values');
      NewLine;
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        ModelInputFileName := AFileName + '.' + Trim(DataArray.Name)
          + '_' + IntToStr(LayerIndex+1);
        ModelInputFileName := 'arrays' + PathDelim + ExtractFileName(ModelInputFileName);

        WriteString('write_column_data_file(header=''no'', &');
        NewLine;
        WriteString(Format('  file=''%s'';delim="space", &', [ModelInputFileName]));
        NewLine;
        WriteString(Format('  plist=p_Value%0:d)', [LayerIndex+1]));
        NewLine;
      end;

    finally
      CloseFile;
    end;
  finally
    PNames.Free;
    UsedParamList.Free;
  end;
end;

end.
