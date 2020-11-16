{
This unit defines function that can be used to communicate with PLPROC.
http://www.pesthomepage.org/About_Us.php
}
unit PlProcUnit;

interface

uses
  ModflowIrregularMeshUnit, CustomModflowWriterUnit, System.SysUtils, FastGEO,
  AbstractGridUnit, GoPhastTypes, MeshRenumberingTypes, System.Classes,
  DataSetUnit, QuadTreeClass, System.Generics.Collections, SutraMeshUnit,
  PhastModelUnit, RbwParser;

type
   TQuadTreeObjectList = TObjectList<TRbwQuadTree>;
   TQuadTreeObjectList2D = TObjectList<TQuadTreeObjectList>;

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

  TScriptChoice =(scWriteScript, scWriteTemplate);

  TParameterZoneWriter = class(TCustomFileWriter)
  private
    // contains values to be modified by PEST.
    FParamValuesFileName: string;
    FUsedParamList: TStringList;
    FPNames: TStringList;
    FDataArray: TDataArray;
    FParamDataArray: TDataArray;
    FQuadTrees: TQuadTreeObjectList2D;
    // Then numbers in @name correspond to the positions of parameters in
    // @link(FUsedParamList).
    // @name is set in @link(WriteValuesFile).
    FZoneNumbers: array of array of array of Integer;
    FValues: array of array of array of double;
    FPilotPointsUsed: Boolean;
    FPilotPointFileName: string;
    FKrigingFactorsFile: string;
    procedure WriteTemplateFile(const AFileName: string;
      ScriptChoice: TScriptChoice);
    procedure WriteValuesFile(const AFileName: string);
    procedure WritePilotPointsFile(const AFileName: string);
    procedure AddParametersToPVAL;
    procedure ReadPilotPoints;
    procedure ReadDiscretization(const AFileName: string);
    procedure SaveKrigingFactors(const AFileName: string);
    procedure WriteKrigingFactorsScript(const AFileName: string);
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(var AFileName: string; DataArray: TDataArray);
  end;

  // Write a 2D CLIST file of SUTRA nodes for PEST
  TSutraNodDisWriter  = class(TCustomFileWriter)
  private
    FInputFileName: string;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

  // Write a 2D CLIST file of SUTRA elements for PEST
  TSutraEleDisWriter  = class(TCustomFileWriter)
  private
    FInputFileName: string;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

  TCustomSutraPlprocFileWriter = class(TCustomFileWriter)
  protected
    procedure GetParameterNames(ParameterNames: TStringList);
  end;

  TSutraNodeDataWriter = class(TCustomSutraPlprocFileWriter)
  private
    FFileName: string;
    FMesh: TSutraMesh3D;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(var AFileName: string; DataArray: TDataArray); overload;
    procedure WriteFile(var AFileName: string; DataArrayName: string;
      DataType: TRbwDataType); overload;
  end;

  TSutraData14BScriptWriter = class(TCustomSutraPlprocFileWriter)
  private
    FRoot: string;
    FFileName: string;
    FParameterNames: TStringList;
    procedure WriteAFile(ScriptChoice: TScriptChoice);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFiles(var AFileName: string);
  end;

implementation

uses
  ModflowParameterUnit, OrderedCollectionUnit;

const
  KDisName = 'cl_Discretization';

type
  PDouble = ^Double;

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
  Result := '.PstValues';
end;

procedure TParameterZoneWriter.WriteFile(var AFileName: string;
  DataArray: TDataArray);
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  FDataArray := DataArray;
  FParamDataArray := Model.DataArrayManager.GetDataSetByName
    (FDataArray.ParamDataSetName);
  Assert(FParamDataArray <> nil);
  FParamDataArray.Initialize;
  FPNames := TStringList.Create;
  FUsedParamList := TStringList.Create;
  FQuadTrees := TQuadTreeObjectList2D.Create;
  try
    FUsedParamList.Duplicates := dupIgnore;
    FUsedParamList.Sorted := True;
    for PIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[PIndex];
      if AParam.ParameterType = ptPEST then
      begin
        FPNames.AddObject(LowerCase(AParam.ParameterName), AParam);
      end;
    end;
    FPNames.Sorted := True;

    WriteValuesFile(AFileName);
    WritePilotPointsFile(AFileName);
    if FPilotPointsUsed then
    begin
      WriteKrigingFactorsScript(AFileName);
    end;
    WriteTemplateFile(AFileName, scWriteScript);
    WriteTemplateFile(AFileName, scWriteTemplate);
    AddParametersToPVAL;

    Model.DataArrayManager.AddDataSetToCache(FDataArray);
    Model.DataArrayManager.AddDataSetToCache(FParamDataArray);
  finally
    FQuadTrees.Free;
    FPNames.Free;
    FUsedParamList.Free;
  end;
end;

procedure TParameterZoneWriter.WriteKrigingFactorsScript(
  const AFileName: string);
var
  ScriptFileName: string;
begin
  ScriptFileName := ChangeFileExt(FParamValuesFileName, '.krig_factors_script');
  OpenFile(ScriptFileName);
  try
    WriteString('#Script for PLPROC for saving kriging factors');
    NewLine;
    NewLine;
    ReadPilotPoints;
    ReadDiscretization(AFileName);
    SaveKrigingFactors(AFileName);
  finally
    CloseFile
  end;
end;

procedure TParameterZoneWriter.SaveKrigingFactors(const AFileName: string);
begin
  WriteString('#Save Kriging factors');
  NewLine;
  WriteString('calc_kriging_factors_auto_2d( &');
  NewLine;
  WriteString(Format('  target_clist=%s, &', [KDisName]));
  NewLine;
  WriteString('  source_clist=PilotPoints, &');
  NewLine;
  FKrigingFactorsFile := ChangeFileExt(AFileName, '.' + FDataArray.Name)
     + '.Factors';
  WriteString(Format('  file=%s;format=formatted)',
    [ExtractFileName(FKrigingFactorsFile)]));
  NewLine;
  NewLine;
end;

procedure TParameterZoneWriter.ReadDiscretization(const AFileName: string);
var
  GrbFileName: string;
  LayerIndex: Integer;
begin
  case Model.ModelSelection of
    msUndefined,msPhast, msFootPrint:
      begin
        Assert(False);
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp, msModflowCfp:
      begin
        WriteString('#Read MODFLOW-2005 grid information file');
        NewLine;
        GrbFileName := ChangeFileExt(AFileName, '');
        GrbFileName := ChangeFileExt(GrbFileName, '.gsf');
        WriteString(Format('%0:s = read_mf_grid_specs(file="%1:s")',
          [KDisName, GrbFileName]));
      end;
    msSutra22, msSutra30:
      begin
        GrbFileName := ChangeFileExt(AFileName, '');
        case FDataArray.EvaluatedAt of
          eaBlocks:
            begin
              GrbFileName := ChangeFileExt(AFileName, TSutraEleDisWriter.Extension);
            end;
          eaNodes:
            begin
              GrbFileName := ChangeFileExt(AFileName, TSutraNodDisWriter.Extension);
            end;
        end;
        WriteString(Format('%s = read_list_file(skiplines=1,dimensions=2, &',
          [KDisName]));
        NewLine;
        WriteString(Format('  id_type=''indexed'',file=''%s'')', [GrbFileName]));
        NewLine;
      end;
    msModflow2015:
      begin
        WriteString('#Read MODFLOW 6 grid information file');
        NewLine;
        // Remove second extension.
        GrbFileName := ChangeFileExt(AFileName, '');
        // Change first extension
        if Model.DisvUsed then
        begin
          GrbFileName := ChangeFileExt(GrbFileName, '.disv.grb');
        end
        else
        begin
          GrbFileName := ChangeFileExt(GrbFileName, '.dis.grb');
        end;
        GrbFileName := ExtractFileName(GrbFileName);
        WriteString(Format('%0:s = read_mf6_grid_specs(file=''%1:s'', &',
          [KDisName, GrbFileName]));
        NewLine;
        WriteString('  dimensions=2, &');
        NewLine;
        for LayerIndex := 0 to Model.LayerCount - 1 do
        begin
          WriteString(Format('  slist_layer_idomain=id%0:d; layer=%0:d, &',
            [LayerIndex + 1]));
          NewLine;
        end;
        for LayerIndex := 0 to Model.LayerCount - 1 do
        begin
          WriteString(Format('  plist_layer_bottom =bot%0:d; layer=%0:d, &',
            [LayerIndex + 1]));
          NewLine;
        end;
        WriteString('  plist_top = top)');
        NewLine;
        NewLine;
      end;
    else
      Assert(False);
  end;
end;

procedure TParameterZoneWriter.ReadPilotPoints;
var
  ColIndex: Integer;
  AQuadList: TQuadTreeObjectList;
  AQuadTree: TRbwQuadTree;
  PIndex: Integer;
  LayerIndex: Integer;
  AParam: TModflowSteadyParameter;
  PListName: string;
begin
  WriteString('#Read pilot point data');
  NewLine;
  WriteString('PilotPoints = read_list_file(skiplines=1,dimensions=2, &');
  NewLine;
  ColIndex := 4;
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      AQuadList := FQuadTrees[PIndex];
      for LayerIndex := 0 to FDataArray.LayerCount - 1 do
      begin
        AQuadTree := AQuadList[LayerIndex];
        if AQuadTree.Count > 0 then
        begin
          PListName := Format('%0:s_%1:d',
            [AParam.ParameterName, LayerIndex + 1]);
          WriteString(Format('  plist=''%0:s'';column=%1:d, &',
            [PListName, ColIndex]));
          Inc(ColIndex);
          NewLine;
        end;
      end;
    end;
  end;
  WriteString(Format('  id_type=''indexed'',file=''%s'')',
    [ExtractFileName(FPilotPointFileName)]));
  NewLine;
  NewLine;
end;

procedure TParameterZoneWriter.AddParametersToPVAL;
var
  ParamIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  for ParamIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[ParamIndex] as TModflowSteadyParameter;
    Model.WritePValAndTemplate(AParam.ParameterName, AParam.Value);
  end;
end;

procedure TParameterZoneWriter.WritePilotPointsFile(const AFileName: string);
var
  DisLimits: TGridLimit;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  AQuadList: TQuadTreeObjectList;
  LayerIndex: Integer;
  AQuadTree: TRbwQuadTree;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint: TPoint2D;
  PilotPointIndex: Integer;
  DPtr: PDouble;
  PListName: string;
begin
  DisLimits := Model.DiscretizationLimits(vdTop);
  FPilotPointsUsed := False;
  FQuadTrees.Capacity := FUsedParamList.Count;
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      FPilotPointsUsed := True;
      AQuadList := TQuadTreeObjectList.Create;
      AQuadList.Capacity := FDataArray.LayerCount;
      FQuadTrees.Add(AQuadList);
      for LayerIndex := 0 to FDataArray.LayerCount - 1 do
      begin
        AQuadTree := TRbwQuadTree.Create(nil);
        AQuadList.Add(AQuadTree);
        AQuadTree.XMin := DisLimits.MinX;
        AQuadTree.XMax := DisLimits.MaxX;
        AQuadTree.YMin := DisLimits.MinY;
        AQuadTree.YMax := DisLimits.MaxY;
        for RowIndex := 0 to FDataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to FDataArray.ColumnCount - 1 do
          begin
            if FZoneNumbers[LayerIndex, RowIndex, ColIndex] = PIndex then
            begin
              APoint := Model.Discretization.ItemTopLocation[
                FDataArray.EvaluatedAt, ColIndex, RowIndex];
              AQuadTree.AddPoint(APoint.x, APoint.y,
                Addr(FValues[LayerIndex, RowIndex, ColIndex]));
            end;
          end;
        end;
      end;
    end
    else
    begin
      FQuadTrees.Add(nil);
    end;
  end;

  if FPilotPointsUsed then
  begin
    // Write pilot point data
    FPilotPointFileName := ChangeFileExt(AFileName, FDataArray.Name) + '.ppoints';
    OpenFile(FPilotPointFileName);
    try
      WriteString('Index X Y ');
      for PIndex := 0 to FUsedParamList.Count - 1 do
      begin
        AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
        if AParam.UsePilotPoints then
        begin
          AQuadList := FQuadTrees[PIndex];
          for LayerIndex := 0 to FDataArray.LayerCount - 1 do
          begin
            AQuadTree := AQuadList[LayerIndex];
            if AQuadTree.Count > 0 then
            begin
              PListName := Format('%0:s_%1:d',
                [AParam.ParameterName, LayerIndex+1]);
              WriteString(PListName + ' ');
            end;
          end;
        end;
      end;
      NewLine;

      for PilotPointIndex := 0 to Model.PestProperties.PilotPointCount - 1 do
      begin
        APoint := Model.PestProperties.PilotPoints[PilotPointIndex];
        WriteInteger(PilotPointIndex+1);
        WriteFloat(APoint.x);
        WriteFloat(APoint.y);
        for PIndex := 0 to FUsedParamList.Count - 1 do
        begin
          AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
          if AParam.UsePilotPoints then
          begin
            AQuadList := FQuadTrees[PIndex];
            for LayerIndex := 0 to FDataArray.LayerCount - 1 do
            begin
              AQuadTree := AQuadList[LayerIndex];
              if AQuadTree.Count > 0 then
              begin
                DPtr := AQuadTree.NearestPointsFirstData(APoint.x, APoint.y);
                WriteFloat(DPtr^);
              end;
            end;
          end;
        end;
        NewLine;
      end;
    finally
      CloseFile;
    end;
  end;
end;

procedure TParameterZoneWriter.WriteValuesFile(const AFileName: string);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  AName: string;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  SListName: string;
  PListName: string;
begin
  SetLength(FZoneNumbers, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);
  SetLength(FValues, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);

  // Write values to be modified by PEST.
  FParamValuesFileName := ChangeFileExt(AFileName, '.' + FDataArray.Name)
    + Extension;
  OpenFile(FParamValuesFileName);
  try
    WriteString(' Index');
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      SListName := Format('s_PIndex%0:d', [LayerIndex + 1]);
      WriteString(' ' + SListName);

      PListName := Format('p_Value%0:d', [LayerIndex + 1]);
      WriteString(' ' + PListName);
    end;
    NewLine;

    ID := 1;
    for RowIndex := 0 to FParamDataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to FParamDataArray.ColumnCount - 1 do
      begin
        WriteInteger(ID);
        for LayerIndex := 0 to FParamDataArray.LayerCount - 1 do
        begin
          AName := LowerCase(FParamDataArray.StringData[
            LayerIndex, RowIndex, ColIndex]);
          if (AName = '') then
          begin
            PIndex := -1;
          end
          else
          begin
            PIndex := FPNames.IndexOf(AName);
          end;
          if PIndex >= 0 then
          begin
            AParam := FPNames.Objects[PIndex] as TModflowSteadyParameter;
            FUsedParamList.AddObject(AParam.ParameterName, AParam);
            if AParam.Value <> 0 then
            begin
              FValues[LayerIndex, RowIndex, ColIndex] :=
                FDataArray.RealData[LayerIndex, RowIndex, ColIndex] / AParam.Value;
            end
            else
            begin
              FValues[LayerIndex, RowIndex, ColIndex] :=
                FDataArray.RealData[LayerIndex, RowIndex, ColIndex];
            end;
          end
          else
          begin
            FValues[LayerIndex, RowIndex, ColIndex] :=
              FDataArray.RealData[LayerIndex, RowIndex, ColIndex];
          end;
          WriteInteger(PIndex + 1);
          WriteFloat(FValues[LayerIndex, RowIndex, ColIndex]);
          FZoneNumbers[LayerIndex, RowIndex, ColIndex] := PIndex;
        end;
        Inc(ID);
        NewLine;
      end;
    end;
  finally
    CloseFile;
  end;
end;

procedure TParameterZoneWriter.WriteTemplateFile(const AFileName: string;
  ScriptChoice: TScriptChoice);
var
  ScriptFileName: string;
  ParameterIndex: Integer;
  ModelInputFileName: string;
  LayerIndex: Integer;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  ColIndex: Integer;
  PListName: string;
  QuadList: TQuadTreeObjectList;
  QuadTree: TRbwQuadTree;
  SListName: string;
begin
  ScriptFileName := ChangeFileExt(FParamValuesFileName, '.script');
  if ScriptChoice = scWriteTemplate then
  begin
    ScriptFileName := ScriptFileName + '.tpl';
  end;
  OpenFile(ScriptFileName);

  try
    if ScriptChoice = scWriteScript then
    begin
      WriteString('#');
    end;
    WriteString('ptf ');
    WriteString(Model.PestProperties.TemplateCharacter);
    NewLine;
    WriteString('#Script for PLPROC');
    NewLine;
    NewLine;

    if FPilotPointsUsed then
    begin
      ReadPilotPoints;
    end;

    ReadDiscretization(AFileName);

    WriteString('#Read data to modify');
    NewLine;
    {$REGION 'Data set values'}
    WriteString(Format('read_list_file(reference_clist=''%s'',skiplines=1, &',
      [KDisName]));
    NewLine;
    ColIndex := 2;
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      SListName := Format('s_PIndex%0:d', [LayerIndex + 1]);
      WriteString(Format('  slist=%0:s;column=%1:d, &',
        [SListName, ColIndex]));
      Inc(ColIndex);
      NewLine;
      PListName := Format('p_Value%0:d', [LayerIndex + 1]);
      WriteString(Format('  plist=%0:s;column=%1:d, &',
        [PListName, ColIndex]));
      Inc(ColIndex);
      NewLine;
    end;
    WriteString(Format('  file=''%s'')', [ExtractFileName(FParamValuesFileName)]));
    NewLine;
    NewLine;
    {$ENDREGION}

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FUsedParamList.Count - 1 do
    begin
      AParam := FUsedParamList.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if ScriptChoice = scWriteScript then
      begin
        WriteString('#');
      end;
      WriteString(Format('%0:s = %1:s                        %0:s%1:s',
        [AParam.ParameterName, Model.PestProperties.TemplateCharacter]));
      NewLine;
      if ScriptChoice = scWriteTemplate then
      begin
        WriteString('#');
      end;
      WriteString(Format('%0:s = %1:g',
        [AParam.ParameterName, AParam.Value]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    WriteString('# Modfify data values');
    NewLine;
    {$REGION 'Modify data values'}
    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      [KDisName]));
    NewLine;

    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      WriteString('# Setting values for layer');
      WriteInteger(LayerIndex + 1);
      NewLine;
      for ParameterIndex := 0 to FUsedParamList.Count - 1 do
      begin
        AParam := FUsedParamList.Objects[ParameterIndex]
          as TModflowSteadyParameter;
        PIndex := FPNames.IndexOf(AParam.ParameterName) + 1;
        WriteString('  # Setting values for parameter ');
        WriteString(AParam.ParameterName);
        NewLine;
        if AParam.UsePilotPoints then
        begin
          WriteString('    # Substituting interpolated values');
          NewLine;
          QuadList := FQuadTrees[ParameterIndex];
          QuadTree := QuadList[LayerIndex];
          if QuadTree.Count > 0 then
          begin
            PListName := Format('%0:s_%1:d',
              [AParam.ParameterName, LayerIndex+1]);
            WriteString('    # Get interpolated values');
            NewLine;
            WriteString(Format(
              '    temp=%0:s.krige_using_file(file=''%1:s'';form=''formatted'',transform=''none'')',
              [PListName, ExtractFileName(FKrigingFactorsFile)]));
            NewLine;
            WriteString('    # Write interpolated values in zones');
            NewLine;
            WriteString(Format(
              '    p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = temp * %2:s',
              [LayerIndex + 1, PIndex, AParam.ParameterName]));
            NewLine;
          end
          else
          begin
            WriteString('    # no interpolated values defined for this layer and parameter');
            NewLine;
          end;
        end
        else
        begin
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format(
            '    p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = p_Value%0:d * %2:s',
            [LayerIndex + 1, PIndex, AParam.ParameterName]));
          NewLine;
        end;
      end;
    end;
    NewLine;
    {$ENDREGION}

    WriteString('#Write new data values');
    NewLine;
    {$REGION 'Write new data values'}
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      ModelInputFileName := AFileName + '.' + Trim(FDataArray.Name) + '_'
        + IntToStr(LayerIndex + 1);
      ModelInputFileName := 'arrays' + PathDelim
        + ExtractFileName(ModelInputFileName);
      WriteString('write_column_data_file(header=''no'', &');
      NewLine;
      WriteString(Format('  file=''%s'';delim="space", &',
        [ModelInputFileName]));
      NewLine;
      WriteString(Format('  plist=p_Value%0:d)', [LayerIndex + 1]));
      NewLine;
      {$ENDREGION}
    end;
  finally
    CloseFile;
  end;
end;

{ TSutraNodDisWriter }

class function TSutraNodDisWriter.Extension: string;
begin
  result := '.c_nod';
end;

procedure TSutraNodDisWriter.WriteFile(const AFileName: string);
var
  NodeIndex: Integer;
  SutraMesh2D: TSutraMesh2D;
  ANode: TSutraNode2D;
  LayerIndex: Integer;
  SutraMesh3D: TSutraMesh3D;
  ANode3D: TSutraNode3D;
begin
  FInputFileName := FileName(AFileName);
  OpenFile(FInputFileName);
  try
    SutraMesh3D := Model.SutraMesh;
    SutraMesh2D := SutraMesh3D.Mesh2D;
    WriteString(' Index X Y Node_Number');
    NewLine;
    for NodeIndex := 0 to SutraMesh2D.Nodes.Count - 1 do
    begin
      ANode := SutraMesh2D.Nodes[NodeIndex];
      WriteInteger(NodeIndex + 1);
      WriteFloat(ANode.Location.x);
      WriteFloat(ANode.Location.y);
      WriteInteger(ANode.Number+1);
      if SutraMesh3D.MeshType = mt3D then
      begin
        for LayerIndex := 0 to SutraMesh3D.LayerCount do
        begin
          ANode3D := SutraMesh3D.NodeArray[LayerIndex,ANode.Number];
          WriteFloat(ANode3D.Z);
          WriteInteger(ANode3D.Number+1);
        end;
      end;
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

{ TSutraEleDisWriter }

class function TSutraEleDisWriter.Extension: string;
begin
  result := '.c_ele';
end;

procedure TSutraEleDisWriter.WriteFile(const AFileName: string);
var
  SutraMesh2D: TSutraMesh2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  Location: TPoint2D;
begin
  FInputFileName := FileName(AFileName);
  OpenFile(FInputFileName);
  try
    SutraMesh2D := Model.SutraMesh.Mesh2D;
    WriteString(' Index X Y Element_Number');
    NewLine;
    for ElementIndex := 0 to SutraMesh2D.Elements.Count - 1 do
    begin
      AnElement := SutraMesh2D.Elements[ElementIndex];
      WriteInteger(ElementIndex + 1);
      Location := AnElement.Center;
      WriteFloat(Location.x);
      WriteFloat(Location.y);
      WriteInteger(AnElement.ElementNumber+1);
      NewLine;
    end;

  finally
    CloseFile;
  end;
end;

{ TSutraNodeDataWriter }

constructor TSutraNodeDataWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FMesh := AModel.SutraMesh;

end;

class function TSutraNodeDataWriter.Extension: string;
begin
  Result := '.PstValues';
end;

procedure TSutraNodeDataWriter.WriteFile(var AFileName: string;
  DataArrayName: string; DataType: TRbwDataType);
var
  LayerIndex: Integer;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  ANode3D: TSutraNode3D;
begin
  FFileName := ChangeFileExt(AFileName, '.' + DataArrayName);

  OpenFile(FFileName);
  try
    WriteString('NN2D ');
    if FMesh.MeshType = mt3D then
    begin
      WriteString('NN3D Layer ');
    end;
    for LayerIndex := 0 to FMesh.LayerCount - 1 do
    begin
      WriteString(Format('%0:s%1:d ', [DataArrayName, LayerIndex+1]));
      if DataType = rdtDouble then
      begin
        WriteString(Format('%0:sParameter%1:d ', [DataArrayName, LayerIndex+1]))
      end;
    end;
    NewLine;

    for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
    begin
      ANode2D := FMesh.Mesh2D.Nodes[NodeIndex];
      WriteInteger(ANode2D.Number+1);
      for LayerIndex := 0 to FMesh.LayerCount - 1 do
      begin
        if FMesh.MeshType = mt3D then
        begin
          ANode3D := FMesh.NodeArray[LayerIndex,NodeIndex];
          WriteInteger(ANode3D.Number + 1);
          WriteInteger(LayerIndex + 1);
        end;
        case DataType of
          rdtDouble:
            begin
              WriteFloat(0);
            end;
          rdtInteger:
            begin
              WriteInteger(0);
            end;
          rdtBoolean:
            begin
              Assert(False);
            end;
          rdtString:
            begin
              Assert(False);
            end;
          else
            begin
              Assert(False);
            end;
        end;
        if DataType = rdtDouble then
        begin
          WriteInteger(0);
        end;
      end;
      NewLine;
    end;

  finally
    CloseFile;
  end;
end;

procedure TSutraNodeDataWriter.WriteFile(var AFileName: string;
  DataArray: TDataArray);
var
  ParameterNames: TStringList;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  ParamArray: TDataArray;
  LayerIndex: Integer;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  ANode3D: TSutraNode3D;
begin
  ParameterNames := TStringList.Create;
  try
    FFileName := ChangeFileExt(AFileName, '.' + DataArray.Name);
    GetParameterNames(ParameterNames);

    if DataArray.PestParametersUsed and (DataArray.DataType = rdtDouble) then
    begin
      ParamArray := Model.DataArrayManager.GetDataSetByName
        (DataArray.ParamDataSetName);
    end
    else
    begin
      ParamArray := nil;
    end;

    OpenFile(FFileName);
    try
      WriteString('NN2D ');
      if FMesh.MeshType = mt3D then
      begin
        WriteString('NN3D Layer ');
      end;
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        WriteString(Format('%0:s%1:d ', [DataArray.Name, LayerIndex+1]));
        if DataArray.DataType = rdtDouble then
        begin
          WriteString(Format('%0:sParameter%1:d ', [DataArray.Name, LayerIndex+1]))
        end;
      end;
      NewLine;

      for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
      begin
        ANode2D := FMesh.Mesh2D.Nodes[NodeIndex];
        WriteInteger(ANode2D.Number+1);
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          if FMesh.MeshType = mt3D then
          begin
            ANode3D := FMesh.NodeArray[LayerIndex,NodeIndex];
            WriteInteger(ANode3D.Number + 1);
            WriteInteger(LayerIndex + 1);
          end;
          if ParamArray <> nil then
          begin
            PIndex := ParameterNames.IndexOf(LowerCase(
              ParamArray.StringData[LayerIndex, 0, NodeIndex]));
          end
          else
          begin
            PIndex := -1;
          end;
          if PIndex >=0 then
          begin
            AParam := ParameterNames.Objects[PIndex] as TModflowSteadyParameter;
          end
          else
          begin
            AParam := nil;
          end;
          case DataArray.DataType of
            rdtDouble:
              begin
                if (AParam = nil) or (AParam.Value = 0) then
                begin
                  WriteFloat(DataArray.RealData[LayerIndex, 0, NodeIndex]);
                end
                else
                begin
                  WriteFloat(DataArray.RealData[LayerIndex, 0, NodeIndex]
                    /AParam.Value);
                end;
              end;
            rdtInteger:
              begin
                WriteInteger(DataArray.IntegerData[LayerIndex, 0, NodeIndex]);
              end;
            rdtBoolean:
              begin
                WriteInteger(Ord(DataArray.BooleanData[LayerIndex, 0, NodeIndex]));
              end;
            rdtString:
              begin
                Assert(False);
              end;
            else
              begin
                Assert(False);
              end;
          end;
          if DataArray.DataType = rdtDouble then
          begin
            WriteInteger(PIndex+1);
          end;
        end;
        NewLine;
      end;

    finally
      CloseFile;
    end;
  finally
    ParameterNames.Free;
  end;
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  if ParamArray <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(ParamArray);
  end;
end;

{ TSutraData14BScriptWriter }

constructor TSutraData14BScriptWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  Assert(Model.ModelSelection in SutraSelection);
  FParameterNames := TStringList.Create;
end;

destructor TSutraData14BScriptWriter.Destroy;
begin
  FParameterNames.Free;
  inherited;
end;

class function TSutraData14BScriptWriter.Extension: string;
begin
  result := '.14B.script';
end;

procedure TSutraData14BScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  Mesh: TSutraMesh3D;
  LayerCount: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  if ScriptChoice = scWriteTemplate then
  begin
    FFileName := FFileName + '.tpl';
  end;

  Mesh := Model.SutraMesh;

  OpenFile(FFileName);
  try
    if ScriptChoice = scWriteScript then
    begin
      WriteString('# ');
    end;
    WriteString('ptf ');
    WriteString(Model.PestProperties.TemplateCharacter);
    NewLine;
    WriteString('#Script for PLPROC');
    NewLine;
    NewLine;

    if Mesh.MeshType = mt3D then
    begin
      LayerCount := Mesh.LayerCount + 1;
    end
    else
    begin
      LayerCount := 1;
    end;

    {$REGION 'Node discretization'}
    WriteString('#Read SUTRA node information file');
    NewLine;
    WriteString('cl_Discretization = read_list_file(skiplines=1,dimensions=2, &');
    NewLine;
    WriteString('  plist=p_x;column=2, &');
    NewLine;
    WriteString('  plist=p_y;column=3, &');
    NewLine;
    if Mesh.MeshType = mt3D then
    begin
      ColIndex := 4;
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString(Format('  plist=p_z%0:d;column=%1:d, &',[LayerIndex, ColIndex]));
        NewLine;
        Inc(ColIndex);

        WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &',[LayerIndex, ColIndex]));
        NewLine;
        Inc(ColIndex);
      end;
    end;
    WriteString(Format('  id_type=''indexed'',file=''%s.c_nod'')', [FRoot]));
    NewLine;
    NewLine;
    {$ENDREGION}

    WriteString('#Read data to modify');
    NewLine;
    {$REGION 'Porosity'}
    WriteString('# Read porosity');
    NewLine;
    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
    NewLine;
    ColIndex := 2;
    for LayerIndex := 1 to LayerCount do
    begin
      WriteString(Format('  plist=p_Porosity%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);

      WriteString(Format('  slist=s_PorPar%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
    end;
    WriteString(Format('  file=''%s.Nodal_Porosity'')', [FRoot]));
    NewLine;
    NewLine;
    {$ENDREGION}

    {$REGION 'Thickness'}
    if Mesh.MeshType <> mt3D then
    begin
      WriteString('# Read Thickness');
      NewLine;
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      WriteString('  plist=p_Thickness;column=2, &');
      NewLine;
      WriteString('  slist=s_Thickness;column=3, &');
      NewLine;
      WriteString(Format('  file=''%s.Nodal_Thickness'')', [FRoot]));
      NewLine;
      NewLine;
    end;
    {$ENDREGION}

    {$REGION 'Unsaturated zone'}
    WriteString('# Read Unsaturated Zone');
    NewLine;
    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
    NewLine;
    ColIndex := 2;
    for LayerIndex := 1 to LayerCount do
    begin
      WriteString(Format('  slist=s_Unsat_Region%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
    end;
    WriteString(Format('  file=''%s.Unsat_Region_Nodes'')', [FRoot]));
    NewLine;
    NewLine;
    {$ENDREGION}

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if ScriptChoice = scWriteScript then
      begin
        WriteString('#');
      end;
      WriteString(Format('%0:s = %1:s                        %0:s%1:s',
        [AParam.ParameterName, Model.PestProperties.TemplateCharacter]));
      NewLine;
      if ScriptChoice = scWriteTemplate then
      begin
        WriteString('#');
      end;
      WriteString(Format('%0:s = %1:g',
        [AParam.ParameterName, AParam.Value]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    {$REGION 'Apply parameters'}
    WriteString('# applying parameter values');
    NewLine;
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      WriteString(Format('# applying parameter %s', [AParam.ParameterName]));
      NewLine;
      if Mesh.MeshType <> mt3D then
      begin
        WriteString(Format('p_Thickness(select=(s_Thickness == %1:d)) = p_Thickness * %0:s',
          [AParam.ParameterName, ParameterIndex+1]));
        NewLine;
      end;
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString(Format('p_Porosity%0:d(select=(s_PorPar%0:d == %2:d)) = p_Porosity%0:d * %1:s',
          [LayerIndex, AParam.ParameterName, ParameterIndex+1]));
        NewLine;
      end;
      NewLine;
    end;
    {$ENDREGION}

    WriteString('# Write new data values');
    NewLine;
    if Mesh.MeshType = mt3D then
    begin
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString('write_column_data_file(header = ''no'', &');
        NewLine;
        WriteString(Format('  file=''%s.14B_%1:d'';delim="space", &',
          [FRoot, LayerIndex]));
        NewLine;
//        WriteString(Format('  select=(s_Layer%0:d = %0:d), &', [LayerIndex]));
//        NewLine;
        WriteString(Format('  slist=''s_NN3D%0:d'', &', [LayerIndex]));
        NewLine;
        WriteString(Format('  slist=s_Unsat_Region%0:d, &', [LayerIndex]));
        NewLine;
        WriteString('  plist=p_x, &');
        NewLine;
        WriteString('  plist=p_y, &');
        NewLine;
        WriteString(Format('  plist=p_z%0:d, &', [LayerIndex]));
        NewLine;
        WriteString(Format('  plist=p_Porosity%0:d)', [LayerIndex]));
        NewLine;
      end;
    end
    else
    begin
      WriteString('write_column_data_file(header = ''no'', &');
      NewLine;
      WriteString(Format('  file=''%s.14B'';delim="space", &', [FRoot]));
      NewLine;
      WriteString('  clist_spec=''id'', &');
      NewLine;
      WriteString('  slist=s_Unsat_Region1, &');
      NewLine;
      WriteString('  plist=p_x, &');
      NewLine;
      WriteString('  plist=p_y, &');
      NewLine;
      WriteString('  plist=p_Thickness, &');
      NewLine;
      WriteString('  plist=p_Porosity1)');
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

procedure TSutraData14BScriptWriter.WriteFiles(var AFileName: string);
begin
  FFileName := FileName(AFileName);
  Model.PestTemplateLines.Add(Format('plproc ''%s''', [FFileName]));
  FRoot := ExtractFileName(ChangeFileExt(AFileName , ''));
  GetParameterNames(FParameterNames);

  WriteAFile(scWriteScript);
  WriteAFile(scWriteTemplate);
end;

procedure TCustomSutraPlprocFileWriter.GetParameterNames(ParameterNames: TStringList);
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  if Model.PestUsed then
  begin
    for PIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[PIndex];
      if AParam.ParameterType = ptPEST then
      begin
        ParameterNames.AddObject(LowerCase(AParam.ParameterName), AParam);
      end;
    end;
    ParameterNames.Sorted := True;
  end;
end;

end.
