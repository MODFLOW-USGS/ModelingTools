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
  PhastModelUnit, RbwParser, PestPilotPointFileWriterUnit, PilotPointDataUnit;

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
    // The numbers in @name correspond to the positions of parameters in
    // @link(FUsedParamList).
    // @name is set in @link(WriteValuesFile).
    FZoneNumbers: array of array of array of Integer;
    FValues: array of array of array of double;
    FPilotPointsUsed: Boolean;
    FKrigingFactorsFile: string;
    FDataArrayID: string;
    FPilotPointFiles: TPilotPointFiles;
    FPrefix: string;
    procedure WriteTemplateFile(const AFileName: string;
      ScriptChoice: TScriptChoice);
    procedure WriteValuesFile(const AFileName: string);
    procedure WritePilotPointsFile(const AFileName, Prefix: string);
    procedure AddParametersToPVAL;
    procedure ReadPilotPoints;
    procedure ReadDiscretization(const AFileName: string);
    procedure SaveKrigingFactors(const AFileName: string);
    procedure WriteKrigingFactorsScript(const AFileName: string);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(var AFileName: string; DataArray: TDataArray;
      const DataArrayID: string; Prefix: string = '');
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

  // Write a 3D CLIST file of SUTRA nodes for PEST
  TSutraNod3DDisWriter  = class(TCustomFileWriter)
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

  TSutraElementDataWriter = class(TCustomSutraPlprocFileWriter)
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
    FPorosityPilotPointFiles: TPilotPointFiles;
    FThicknessPilotPointFiles: TPilotPointFiles;
    FUsedParamList: TStringList;
    FMesh: TSutraMesh3D;
    FPorKrigingFactorsFileRoot: string;
    FThicknessKrigingFactorsFileRoot: string;
    procedure WriteAFile(ScriptChoice: TScriptChoice);
    procedure WritePilotPointFiles;
    procedure GetUsedParameters;
    procedure WriteKrigingFactors;
    procedure ReadPilotPoints;
    procedure ReadDiscretization;
    procedure SaveKrigingFactors;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFiles(var AFileName: string);
  end;

  TDataRecord = class(TObject)
    DataArray: TDataArray;
    PilotPointFiles: TPilotPointFiles;
    ID: string;
    Prefix: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TDataRecordList = TObjectList<TDataRecord>;

  TSutraData15BScriptWriter = class(TCustomSutraPlprocFileWriter)
  private
    FRoot: string;
    FFileName: string;
    FParameterNames: TStringList;
    FDataRecordList: TDataRecordList;
    FUsedParamList: TStringList;
    procedure WriteAFile(ScriptChoice: TScriptChoice);
    procedure WritePilotPointFiles;
    procedure GetUsedParameters;
//    procedure WriteKrigingFactors;
//    procedure ReadPilotPoints;
    procedure ReadDiscretization;
//    procedure SaveKrigingFactors;
//    function GetKrigFactorRoot(ADataRec: TDataRecord): string;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFiles(var AFileName: string);
  end;

  TSutraInitCondScriptWriter = class(TCustomSutraPlprocFileWriter)
  private
    FRoot: string;
    FFileName: string;
    FParameterNames: TStringList;
    FDataArrayName: string;
    FPilotPointFiles: TPilotPointFiles;
    FID: string;
    FDataArray: TDataArray;
    FMesh: TSutraMesh3D;
    FUsedParamList: TStringList;
    procedure WriteAFile(ScriptChoice: TScriptChoice);
    procedure WritePilotPointFiles(Prefix: string);
    procedure GetUsedParameters;
    procedure WriteKrigingFactors;
    procedure ReadPilotPoints;
    procedure SaveKrigingFactors;
    function GetKrigFactorRoot: string;
    procedure ReadDiscretization;
    procedure Read3DDiscretization;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFiles(var AFileName: string; const DataArrayName: string; ID, Prefix: string);
  end;

implementation

uses
  ModflowParameterUnit, OrderedCollectionUnit, SutraOptionsUnit;

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

constructor TParameterZoneWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FPilotPointFiles := TPilotPointFiles.Create;
end;

destructor TParameterZoneWriter.Destroy;
begin
  FPilotPointFiles.Free;
  inherited;
end;

class function TParameterZoneWriter.Extension: string;
begin
  Result := '.PstValues';
end;

procedure TParameterZoneWriter.WriteFile(var AFileName: string;
  DataArray: TDataArray; const DataArrayID: string; Prefix: string = '');
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  FPrefix := Prefix;
  FDataArray := DataArray;
  FDataArrayID := DataArrayID;
  FParamDataArray := Model.DataArrayManager.GetDataSetByName
    (FDataArray.ParamDataSetName);
  Assert(FParamDataArray <> nil);
  FParamDataArray.Initialize;
  FPNames := TStringList.Create;
  FUsedParamList := TStringList.Create;
  try
    FUsedParamList.Duplicates := dupIgnore;
    FUsedParamList.Sorted := True;
    for PIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[PIndex];
      if AParam.ParameterType = ptPEST then
      begin
        FPNames.AddObject(LowerCase(AParam.ParameterName), AParam);
        AParam.IsUsedInTemplate := True;
      end;
    end;
    FPNames.Sorted := True;

    WriteValuesFile(AFileName);
    WritePilotPointsFile(AFileName, Prefix);
    if FPilotPointsUsed then
    begin
      WriteKrigingFactorsScript(AFileName);
    end;
    WriteTemplateFile(AFileName, scWriteScript);
    WriteTemplateFile(AFileName, scWriteTemplate);
    AddParametersToPVAL;

    Model.DataArrayManager.AddDataSetToCache(FDataArray);
    Model.DataArrayManager.AddDataSetToCache(FParamDataArray);

    Model.PilotPointData.AddPilotPointFileObjects(FPilotPointFiles);

  finally
    FPNames.Free;
    FUsedParamList.Free;
  end;
end;

procedure TParameterZoneWriter.WriteKrigingFactorsScript(
  const AFileName: string);
var
  ScriptFileName: string;
  PLPROC_Location: string;
begin
  ScriptFileName := ChangeFileExt(FParamValuesFileName, StrKrigfactorsscript);
  OpenFile(ScriptFileName);
  try
    WriteString('#Script for PLPROC for saving kriging factors');
    NewLine;
    NewLine;
    ReadPilotPoints;
    ReadDiscretization(ChangeFileExt(AFileName, ''));
    SaveKrigingFactors(ChangeFileExt(AFileName, ''));
  finally
    CloseFile
  end;

  PLPROC_Location := GetPLPROC_Location(FParamValuesFileName, Model);
  Model.KrigfactorsScriptLines.Add(Format('"%0:s" ''%1:s''',
    [PLPROC_Location, ExtractFileName(ScriptFileName)]));
end;

procedure TParameterZoneWriter.SaveKrigingFactors(const AFileName: string);
var
  PIndex: Integer;
  PPIndex: Integer;
  FileProperties: TPilotPointFileObject;
  AParam: TModflowSteadyParameter;
begin
  WriteString('#Save Kriging factors');
  NewLine;

  FKrigingFactorsFile := ChangeFileExt(AFileName, '.' + FDataArray.Name)
    + '.Factors';
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      for PPIndex := 0 to FPilotPointFiles.Count - 1 do
      begin
        FileProperties := FPilotPointFiles[PPIndex];
        if FileProperties.Parameter = AParam then
        begin
          WriteString('calc_kriging_factors_auto_2d( &');
          NewLine;
          if Model.ModelSelection = msModflow2015 then
          begin
            WriteString(Format('  target_clist=%0:s%1:d, &',
              [KDisName, FileProperties.Layer+1]));
          end
          else
          begin
            WriteString(Format('  target_clist=%s, &', [KDisName]));
          end;
          NewLine;
          WriteString(Format('  source_clist=%0:sPilotPoints%1:d, &', [FPrefix, PPIndex+1]));
          NewLine;
          WriteString(Format('  file=%0:s%1:d;format=formatted)',
            [ExtractFileName(FKrigingFactorsFile), PPIndex+1]));
          NewLine;
        end;
      end;
    end;
  end;
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
        GrbFileName := ExtractFileName(GrbFileName);
        WriteString(Format('%0:s = read_mf_grid_specs(file="%1:s")',
          [KDisName, GrbFileName]));
      end;
    msSutra22, msSutra30:
      begin
        GrbFileName := ChangeFileExt(AFileName, '');
        GrbFileName := ChangeFileExt(GrbFileName, '');
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
        GrbFileName := ExtractFileName(GrbFileName);
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

        for LayerIndex := 0 to Model.LayerCount - 1 do
        begin
          WriteString(Format('%0:s%2:d = read_mf6_grid_specs(file=''%1:s'', &',
            [KDisName, GrbFileName, LayerIndex+1]));
          NewLine;
          WriteString('  dimensions=2, &');
          NewLine;
          WriteString(Format('  slist_layer_idomain=id%0:d; layer=%0:d, &',
            [LayerIndex + 1]));
          NewLine;
          WriteString(Format('  plist_layer_bottom =bot%0:d; layer=%0:d, &',
            [LayerIndex + 1]));
          NewLine;
          if LayerIndex = 0 then
          begin
            WriteString('  plist_top = top)');
          end
          else
          begin
            WriteString('  )');
          end;
          NewLine;
        end;
//        for LayerIndex := 0 to Model.LayerCount - 1 do
//        begin
//        end;
        NewLine;
        NewLine;
      end;
    else
      Assert(False);
  end;
end;

procedure TParameterZoneWriter.ReadPilotPoints;
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  PListName: string;
  PPIndex: Integer;
  FileProperties: TPilotPointFileObject;
begin
  WriteString('#Read pilot point data');
  NewLine;
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      for PPIndex := 0 to FPilotPointFiles.Count - 1 do
      begin
        FileProperties := FPilotPointFiles[PPIndex];
        if FileProperties.Parameter = AParam then
        begin
          WriteString(Format('%0:sPilotPoints%1:d = read_list_file(skiplines=0,dimensions=2, &',
            [FPrefix, PPIndex+1]));
          NewLine;
          PListName := Format('%0:s_%1:d',
            [AParam.ParameterName, FileProperties.Layer + 1]);
          WriteString(Format('  plist=''%0:s'';column=%1:d, &',
            [PListName, 5]));
//          Inc(ColIndex);
          NewLine;
          WriteString(Format('  id_type=''character'',file=''%s'')',
            [ExtractFileName(FileProperties.FileName)]));
          NewLine;
        end;
      end;
    end;
  end;
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
    Model.WritePValAndTemplate(AParam.ParameterName, AParam.Value, AParam);
  end;
end;

procedure TParameterZoneWriter.WritePilotPointsFile(const AFileName, Prefix: string);
var
  PilotPointWriter: TPilotPointWriter;
  PriorCount: Integer;
begin
  PilotPointWriter := TPilotPointWriter.Create(Model, etExport);
  try
    PriorCount := FPilotPointFiles.Count;
    PilotPointWriter.WriteFile(ChangeFileExt(AFileName, ''), FDataArray, FPilotPointFiles,
      FDataArrayID, Prefix);
    FPilotPointsUsed := FPilotPointFiles.Count > PriorCount;
    FDataArray.PilotPointsUsed := FPilotPointsUsed
  finally
    PilotPointWriter.Free;
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
  DataValue: Double;
begin
  SetLength(FZoneNumbers, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);
  SetLength(FValues, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);

  // Write values to be modified by PEST.
  FParamValuesFileName := ChangeFileExt(ChangeFileExt(AFileName, ''), '.' + FDataArray.Name)
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

          if FDataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            DataValue := FDataArray.RealData[LayerIndex, RowIndex, ColIndex]
          end
          else
          begin
            DataValue := 0
          end;
          if PIndex >= 0 then
          begin
            AParam := FPNames.Objects[PIndex] as TModflowSteadyParameter;
            FUsedParamList.AddObject(AParam.ParameterName, AParam);
            if AParam.Value <> 0 then
            begin
              FValues[LayerIndex, RowIndex, ColIndex] :=
                DataValue / AParam.Value;
            end
            else
            begin
              FValues[LayerIndex, RowIndex, ColIndex] := DataValue;
//                FDataArray.RealData[LayerIndex, RowIndex, ColIndex];
            end;
          end
          else
          begin
            FValues[LayerIndex, RowIndex, ColIndex] := DataValue
//              FDataArray.RealData[LayerIndex, RowIndex, ColIndex];
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
  SListName: string;
  FileIndex: Integer;
  FileProperties: TPilotPointFileObject;
  UsedFileProperties: TPilotPointFileObject;
  ParamIndex: Integer;
  Root: string;
begin
  ScriptFileName := ChangeFileExt(FParamValuesFileName, '.script');
  if ScriptChoice = scWriteTemplate then
  begin
    ScriptFileName := ScriptFileName + '.tpl';
  end;
  OpenFile(ScriptFileName);

  try
    if ScriptChoice = scWriteTemplate then
    begin
      WriteString('ptf ');
      WriteString(Model.PestProperties.TemplateCharacter);
      NewLine;
    end;
    WriteString('#Script for PLPROC');
    NewLine;
    NewLine;

    if FPilotPointsUsed then
    begin
      ReadPilotPoints;
    end;

    ReadDiscretization(ChangeFileExt(AFileName, ''));
    NewLine;

    WriteString('#Read data to modify');
    NewLine;
    {$REGION 'Data set values'}
    ColIndex := 2;
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      if Model.ModelSelection = msModflow2015 then
      begin
        WriteString(Format('read_list_file(reference_clist=''%0:s%1:d'',skiplines=1, &',
          [KDisName, LayerIndex+1]));
      end
      else
      begin
        WriteString(Format('read_list_file(reference_clist=''%s'',skiplines=1, &',
          [KDisName]));
      end;
      NewLine;
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
      WriteString(Format('  file=''%s'')', [ExtractFileName(FParamValuesFileName)]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FUsedParamList.Count - 1 do
    begin
      AParam := FUsedParamList.Objects[ParameterIndex]
        as TModflowSteadyParameter;

//      if ScriptChoice = scWriteScript then
//      begin
//        WriteString('#');
//      end;
      if ScriptChoice = scWriteTemplate then
      begin
        WriteString(Format('%0:s = %1:s                        %0:s%1:s',
          [AParam.ParameterName, Model.PestProperties.TemplateCharacter]));
        NewLine;
      end;
//      if ScriptChoice = scWriteTemplate then
//      begin
//        WriteString('#');
//      end;
      if ScriptChoice = scWriteScript then
      begin
        WriteString(Format('%0:s = %1:g',
          [AParam.ParameterName, AParam.Value]));
        NewLine;
      end;
      if AParam.UsePilotPoints then
      begin
        WriteString(Format('# Pilot points are used with %s.',
          [AParam.ParameterName]));
      end
      else
      begin
        WriteString(Format('# Pilot points are not used with %s.',
          [AParam.ParameterName]));
      end;
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    WriteString('# Modfify data values');
    NewLine;
    {$REGION 'Modify data values'}

    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      if Model.ModelSelection = msModflow2015 then
      begin
        WriteString(Format('temp%1:d=new_plist(reference_clist=%0:s%1:d,value=0.0)',
          [KDisName, LayerIndex+1]));
      end
      else
      begin
        WriteString(Format('temp%1:d=new_plist(reference_clist=%0:s,value=0.0)',
          [KDisName, LayerIndex+1]));
      end;
      NewLine;
      WriteString('# Setting values for layer');
      WriteInteger(LayerIndex + 1);
      NewLine;
      for ParameterIndex := 0 to FUsedParamList.Count - 1 do
      begin
        AParam := FUsedParamList.Objects[ParameterIndex]
          as TModflowSteadyParameter;
        ParamIndex := FPNames.IndexOf(AParam.ParameterName);

        WriteString('  # Setting values for parameter ');
        WriteString(AParam.ParameterName);
        NewLine;
        UsedFileProperties := nil;
        if AParam.UsePilotPoints then
        begin
          WriteString('    # Substituting interpolated values');
          NewLine;

          PIndex := 0;
          for FileIndex := 0 to FPilotPointFiles.Count - 1 do
          begin
            FileProperties := FPilotPointFiles[FileIndex];
            if (FileProperties.Parameter = AParam)
              and (FileProperties.Layer = LayerIndex) then
            begin
              UsedFileProperties := FileProperties;
              PIndex := FileIndex;
              break;
            end;
          end;

          if UsedFileProperties <> nil then
          begin
            PListName := Format('%0:s_%1:d',
              [AParam.ParameterName, LayerIndex+1]);
            WriteString('    # Get interpolated values');
            NewLine;
            WriteString(Format(
              '    temp%3:d=%0:s.krige_using_file(file=''%1:s%2:d'';form=''formatted'', &',
              [PListName, ExtractFileName(FKrigingFactorsFile), PIndex+1, LayerIndex+1]));
            NewLine;
            if AParam.Transform = ptLog then
            begin
              WriteString('      transform=''log'')');
            end
            else
            begin
              WriteString('      transform=''none'')');
            end;
            NewLine;
            WriteString('    # Write interpolated values in zones');
            NewLine;
            WriteString(Format(
              '    p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = temp%0:d',
              [LayerIndex + 1, ParamIndex+1]));
            NewLine;
          end
          else
          begin
            WriteString(Format(
              '    # no interpolated values defined for parameter %1:s in layer %0:d',
              [LayerIndex+1, AParam.ParameterName]));
            NewLine;
          end;
        end;
        if UsedFileProperties = nil then
        begin
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format(
            '    p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = p_Value%0:d * %2:s',
            [LayerIndex + 1, ParamIndex+1, AParam.ParameterName]));
          NewLine;
        end;
      end;
    end;
    NewLine;
    {$ENDREGION}

    Root := ChangeFileExt(AFileName, '');
    Root := ChangeFileExt(Root, '');
    WriteString('#Write new data values');
    NewLine;
    {$REGION 'Write new data values'}
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      ModelInputFileName := Root + '.' + Trim(FDataArray.Name) + '_'
        + IntToStr(LayerIndex + 1) + StrArraysExt;
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
    WriteString(' Index X Y Node_Number2D');
    if SutraMesh3D.MeshType = mt3D then
    begin
      for LayerIndex := 0 to SutraMesh3D.LayerCount do
      begin
        WriteString(Format('Z_%0:d NodeNumber3D_%0:d Active_%0:d', [LayerIndex+1]));
      end;
    end;
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
          if ANode3D.Active then
          begin
            WriteInteger(1);
          end
          else
          begin
            WriteInteger(0);
          end;
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
  AnElement3D: TSutraElement3D;
  LayerIndex: Integer;
  SutraMesh3D: TSutraMesh3D;
begin
  FInputFileName := FileName(AFileName);
  OpenFile(FInputFileName);
  try
    SutraMesh3D := Model.SutraMesh;
    SutraMesh2D := SutraMesh3D.Mesh2D;
    WriteString(' Index X Y Element_Number2D');
    if SutraMesh3D.MeshType = mt3D then
    begin
      for LayerIndex := 0 to SutraMesh3D.LayerCount-1 do
      begin
        WriteString(Format('Z_%0:d ElementNumber3D_%0:d Active_%0:d', [LayerIndex+1]));
      end;
    end;
    NewLine;
    for ElementIndex := 0 to SutraMesh2D.Elements.Count - 1 do
    begin
      AnElement := SutraMesh2D.Elements[ElementIndex];
      WriteInteger(ElementIndex + 1);
      Location := AnElement.Center;
      WriteFloat(Location.x);
      WriteFloat(Location.y);
      WriteInteger(AnElement.ElementNumber+1);
      if SutraMesh3D.MeshType = mt3D then
      begin
        for LayerIndex := 0 to SutraMesh3D.LayerCount-1 do
        begin
          AnElement3D := SutraMesh3D.ElementArray[LayerIndex, ElementIndex];
          WriteFloat(AnElement3D.CenterElevation);
          WriteInteger(AnElement3D.ElementNumber+1);
          if AnElement3D.Active then
          begin
            WriteInteger(1);
          end
          else
          begin
            WriteInteger(0);
          end;
        end;
      end;
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
  LayerCount: Integer;
begin
  FFileName := ChangeFileExt(AFileName, '.' + DataArrayName);

  OpenFile(FFileName);
  try
    WriteString('NN2D ');
    LayerCount := FMesh.LayerCount + 1;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      if FMesh.MeshType = mt3D then
      begin
        WriteString(Format('NN3D Layer%d ', [LayerIndex+1]));
      end;
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
      for LayerIndex := 0 to LayerCount - 1 do
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
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        if FMesh.MeshType = mt3D then
        begin
          WriteString(Format('NN3D%0:d Layer%0:d ', [LayerIndex+1]));
        end;
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
  FPorosityPilotPointFiles := TPilotPointFiles.Create;
  FThicknessPilotPointFiles := TPilotPointFiles.Create;
  FUsedParamList := TStringList.Create;
  FUsedParamList.Duplicates := dupIgnore;
  FUsedParamList.Sorted := True;
end;

destructor TSutraData14BScriptWriter.Destroy;
begin
  FUsedParamList.Free;
  FThicknessPilotPointFiles.Free;
  FPorosityPilotPointFiles.Free;
  FParameterNames.Free;
  inherited;
end;

class function TSutraData14BScriptWriter.Extension: string;
begin
  result := '.14B.script';
end;

procedure TSutraData14BScriptWriter.GetUsedParameters;
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  Porosity: TDataArray;
  ParamArray: TDataArray;
  Thickness: TDataArray;
  NodeIndex: Integer;
  LayerIndex: Integer;
begin
  FMesh := Model.SutraMesh;

  Porosity := Model.DataArrayManager.GetDataSetByName(KNodalPorosity);
  if Porosity.PestParametersUsed then
  begin
    ParamArray := Model.DataArrayManager.GetDataSetByName
      (Porosity.ParamDataSetName);

    for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
    begin
      for LayerIndex := 0 to ParamArray.LayerCount - 1 do
      begin
        PIndex := FParameterNames.IndexOf(LowerCase(
          ParamArray.StringData[LayerIndex, 0, NodeIndex]));
        if PIndex >=0 then
        begin
          AParam := FParameterNames.Objects[PIndex] as TModflowSteadyParameter;
        end
        else
        begin
          AParam := nil;
        end;

        if AParam <> nil then
        begin
          FUsedParamList.AddObject(AParam.ParameterName, AParam);
        end;
      end;
    end;
  end;

  if Model.SutraMesh.MeshType in [mt2D, mtProfile] then
  begin
    Thickness := Model.DataArrayManager.GetDataSetByName(KNodalThickness);
    if Thickness.PestParametersUsed then
    begin
      ParamArray := Model.DataArrayManager.GetDataSetByName
        (Thickness.ParamDataSetName);

      for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
      begin
        for LayerIndex := 0 to ParamArray.LayerCount - 1 do
        begin
          PIndex := FParameterNames.IndexOf(LowerCase(
            ParamArray.StringData[LayerIndex, 0, NodeIndex]));
          if PIndex >=0 then
          begin
            AParam := FParameterNames.Objects[PIndex] as TModflowSteadyParameter;
          end
          else
          begin
            AParam := nil;
          end;

          if AParam <> nil then
          begin
            FUsedParamList.AddObject(AParam.ParameterName, AParam);
          end;
        end;
      end;
    end
  end;

end;

procedure TSutraData14BScriptWriter.ReadPilotPoints;
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  procedure HandlePilotPointList(PilotPointFiles: TPilotPointFiles; const DataId: string);
  var
    PPIndex: Integer;
    FileProperties: TPilotPointFileObject;
    PListName: string;
  begin
    for PPIndex := 0 to PilotPointFiles.Count - 1 do
    begin
      FileProperties := PilotPointFiles[PPIndex];
      if FileProperties.Parameter = AParam then
      begin
        WriteString(Format('%0:s_PilotPoints%d = read_list_file(skiplines=0,dimensions=2, &',
          [DataId, PPIndex+1]));
        NewLine;
        PListName := Format('%0:s_%1:s_%2:d',
          [DataId, AParam.ParameterName, FileProperties.Layer + 1]);
        WriteString(Format('  plist=''%0:s'';column=%1:d, &',
          [PListName, 5]));
//          Inc(ColIndex);
        NewLine;
        WriteString(Format('  id_type=''character'',file=''%s'')',
          [ExtractFileName(FileProperties.FileName)]));
        NewLine;
      end;
    end;
  end;
begin
  WriteString('#Read pilot point data');
  NewLine;
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      HandlePilotPointList(FPorosityPilotPointFiles, 'POR');
      HandlePilotPointList(FThicknessPilotPointFiles, 'Z');
    end;
  end;
  NewLine;
end;

procedure TSutraData14BScriptWriter.SaveKrigingFactors;
var
  FileProperties: TPilotPointFileObject;
  AParam: TModflowSteadyParameter;
  Porosity: TDataArray;
  Thickness: TDataArray;
  procedure HandleDataArray(DataID: string; DataArray: TDataArray;
    PilotPointFiles: TPilotPointFiles; FileName: string);
  var
    PIndex: Integer;
    PPIndex: Integer;
  begin
    for PIndex := 0 to FUsedParamList.Count - 1 do
    begin
      AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
      if AParam.UsePilotPoints then
      begin
        for PPIndex := 0 to PilotPointFiles.Count - 1 do
        begin
          FileProperties := PilotPointFiles[PPIndex];
          if FileProperties.Parameter = AParam then
          begin
            WriteString('calc_kriging_factors_auto_2d( &');
            NewLine;
            WriteString(Format('  target_clist=%s, &', [KDisName]));
            NewLine;
            WriteString(Format('  source_clist=%0:s_PilotPoints%1:d, &', [DataID, PPIndex+1]));
            NewLine;
            WriteString(Format('  file=%0:s%1:d;format=formatted)',
              [ExtractFileName(FileName), PPIndex+1]));
            NewLine;
          end;
        end;
      end;
    end;
    NewLine;
  end;
begin
  WriteString('#Save Kriging factors');
  NewLine;
  if FPorosityPilotPointFiles.Count > 0 then
  begin
    Porosity := Model.DataArrayManager.GetDataSetByName(KNodalPorosity);
    FPorKrigingFactorsFileRoot := FRoot +  '.' + Porosity.Name
    + '.Factors';
    HandleDataArray('POR', Porosity, FPorosityPilotPointFiles, FPorKrigingFactorsFileRoot);
  end;

  if FThicknessPilotPointFiles.Count > 0 then
  begin
    Thickness := Model.DataArrayManager.GetDataSetByName(KNodalThickness);
    FThicknessKrigingFactorsFileRoot := FRoot +  '.' + Thickness.Name
    + '.Factors';
    HandleDataArray('Z', Thickness, FThicknessPilotPointFiles, FThicknessKrigingFactorsFileRoot);
  end;

  NewLine;
end;

procedure TSutraData14BScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  LayerCount: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
  PIndex: Integer;
  FileIndex: Integer;
  FileProperties: TPilotPointFileObject;
  UsedFileProperties: TPilotPointFileObject;
  PListName: string;
begin
  if ScriptChoice = scWriteTemplate then
  begin
    FFileName := FFileName + '.tpl';
  end;

  FMesh := Model.SutraMesh;

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

    if FMesh.MeshType = mt3D then
    begin
      LayerCount := FMesh.LayerCount + 1;
    end
    else
    begin
      LayerCount := 1;
    end;

    {$REGION 'Node discretization'}
    ReadDiscretization;
    {$ENDREGION}

    ReadPilotPoints;

    WriteString('# Read data to modify');
    NewLine;
    {$REGION 'Porosity'}
    WriteString('# Read porosity');
    NewLine;
    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
    NewLine;
    ColIndex := 1;

    Inc(ColIndex);

    WriteString(Format('  file=''%s.Nodal_Porosity'')', [FRoot]));
    NewLine;

    for LayerIndex := 1 to LayerCount do
    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      if FMesh.MeshType = mt3D then
      begin
  //      WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
  //      NewLine;
        Inc(ColIndex);

        WriteString(Format('  slist=s_Layer%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
        NewLine;
        Inc(ColIndex);
      end;

      WriteString(Format('  plist=p_Porosity%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);

      WriteString(Format('  slist=s_PorPar%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  file=''%s.Nodal_Porosity'')', [FRoot]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    {$REGION 'Thickness'}
    if FMesh.MeshType <> mt3D then
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
    ColIndex := 2;
    for LayerIndex := 1 to LayerCount do
    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;

      if FMesh.MeshType = mt3D then
      begin
        Inc(ColIndex,2);
      end;
//      Inc(ColIndex);
      WriteString(Format('  slist=s_Unsat_Region%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);

      WriteString(Format('  file=''%s.Unsat_Region_Nodes'')', [FRoot]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if AParam.UsePilotPoints then
      begin
        Continue;
      end;
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

    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      ['cl_Discretization']));
    NewLine;

    WriteString('# applying parameter values');
    NewLine;
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      WriteString(Format('# applying parameter %s', [AParam.ParameterName]));
      NewLine;
      if FMesh.MeshType <> mt3D then
      begin
        if AParam.UsePilotPoints then
        begin
          LayerIndex := 0;
          WriteString('    # Substituting interpolated values');
          NewLine;

          UsedFileProperties := nil;
          PIndex := 0;
          for FileIndex := 0 to FThicknessPilotPointFiles.Count - 1 do
          begin
            FileProperties := FThicknessPilotPointFiles[FileIndex];
            if (FileProperties.Parameter = AParam)
              and (FileProperties.Layer = LayerIndex) then
            begin
              UsedFileProperties := FileProperties;
              PIndex := FileIndex;
              break;
            end;
          end;

          if UsedFileProperties <> nil then
          begin
            PListName := Format('%0:s_%1:s_%2:d',
              ['Z',AParam.ParameterName, LayerIndex+1]);
            WriteString('    # Get interpolated values');
            NewLine;
            WriteString(Format(
              '    temp=%0:s.krige_using_file(file=''%1:s%2:d'';form=''formatted'', &',
              [PListName, ExtractFileName(FThicknessKrigingFactorsFileRoot), PIndex+1]));
            NewLine;
            if AParam.Transform = ptLog then
            begin
              WriteString('      transform=''log'')');
            end
            else
            begin
              WriteString('      transform=''none'')');
            end;
            NewLine;
            WriteString('    # Write interpolated values in zones');
            NewLine;
            WriteString(Format(
              '    p_Thickness(select=(s_Thickness == %1:d)) = temp',
              [LayerIndex + 1, ParameterIndex+1]));
            NewLine;
          end
          else
          begin
            WriteString(Format(
              '    # no interpolated values defined for parameter %1:s in layer %0:d',
              [LayerIndex+1, AParam.ParameterName]));
            NewLine;
          end;
        end
        else
        begin
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format('p_Thickness(select=(s_Thickness == %1:d)) = p_Thickness * %0:s',
            [AParam.ParameterName, ParameterIndex+1]));
          NewLine;
        end;
      end;
      for LayerIndex := 0 to LayerCount-1 do
      begin
        if AParam.UsePilotPoints then
        begin
          WriteString('    # Substituting interpolated values');
          NewLine;

          UsedFileProperties := nil;
          PIndex := 0;
          for FileIndex := 0 to FPorosityPilotPointFiles.Count - 1 do
          begin
            FileProperties := FPorosityPilotPointFiles[FileIndex];
            if (FileProperties.Parameter = AParam)
              and (FileProperties.Layer = LayerIndex) then
            begin
              UsedFileProperties := FileProperties;
              PIndex := FileIndex;
              break;
            end;
          end;

          if UsedFileProperties <> nil then
          begin
            PListName := Format('%0:s_%1:s_%2:d',
              ['POR',AParam.ParameterName, LayerIndex+1]);
            WriteString('    # Get interpolated values');
            NewLine;
            WriteString(Format(
              '    temp=%0:s.krige_using_file(file=''%1:s%2:d'';form=''formatted'', &',
              [PListName, ExtractFileName(FPorKrigingFactorsFileRoot), PIndex+1]));
            NewLine;
            if AParam.Transform = ptLog then
            begin
              WriteString('transform=''log'')');
            end
            else
            begin
              WriteString('transform=''none'')');
            end;
            NewLine;
            WriteString('    # Write interpolated values in zones');
            NewLine;
            WriteString(Format(
              '    p_Porosity%0:d(select=(s_PorPar%0:d == %1:d)) = temp',
              [LayerIndex + 1, ParameterIndex+1]));
            NewLine;
          end
          else
          begin
            WriteString(Format(
              '    # no interpolated values defined for parameter %1:s in layer %0:d',
              [LayerIndex+1, AParam.ParameterName]));
            NewLine;
          end;
        end
        else
        begin
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format('p_Porosity%0:d(select=(s_PorPar%0:d == %2:d)) = p_Porosity%0:d * %1:s',
            [LayerIndex+1, AParam.ParameterName, ParameterIndex+1]));
          NewLine;
        end;
      end;
      NewLine;
    end;
    {$ENDREGION}

    WriteString('# Write new data values');
    NewLine;
  {$REGION 'Write values'}
    if FMesh.MeshType = mt3D then
    begin
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString('write_column_data_file(header = ''no'', &');
        NewLine;
        WriteString(Format('  file=''%s.14B_%1:d'';delim="space", &',
          [FRoot, LayerIndex]));
        NewLine;
        WriteString(Format('  select=(s_Active_%d == 1), &',
          [LayerIndex]));
        NewLine;
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
  {$ENDREGION}
  finally
    CloseFile;
  end;
end;

procedure TSutraData14BScriptWriter.WriteFiles(var AFileName: string);
var
  PLPROC_Location: string;
begin
  FFileName := FileName(AFileName);
  Model.SutraPestScripts.Add(FFileName);
  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  Model.PestTemplateLines.Add(Format('"%0:s" ''%1:s''', [PLPROC_Location, ExtractFileName(FFileName)]));
  FRoot := ExtractFileName(ChangeFileExt(AFileName , ''));
  GetParameterNames(FParameterNames);
  GetUsedParameters;
  WritePilotPointFiles;
  WriteKrigingFactors;

  WriteAFile(scWriteScript);
  WriteAFile(scWriteTemplate);

  Model.PilotPointData.AddPilotPointFileObjects(FPorosityPilotPointFiles);
  Model.PilotPointData.AddPilotPointFileObjects(FThicknessPilotPointFiles);

end;

procedure TSutraData14BScriptWriter.ReadDiscretization;
var
  LayerIndex: Integer;
  ColIndex: Integer;
  LayerCount: Integer;
begin
  if FMesh.MeshType = mt3D then
  begin
    LayerCount := FMesh.LayerCount + 1;
  end
  else
  begin
    LayerCount := 1;
  end;

  WriteString('#Read SUTRA node information file');
  NewLine;
  WriteString('cl_Discretization = read_list_file(skiplines=1,dimensions=2, &');
  NewLine;
  // X coordinate
  WriteString('  plist=p_x;column=2, &');
  NewLine;
  // Y coordinate
  WriteString('  plist=p_y;column=3, &');
  NewLine;
  // 2D node number
  WriteString('  slist=s_NN2D;column=4, &');
  NewLine;
  WriteString(Format('  id_type=''indexed'',file=''%s.c_nod'')', [FRoot]));
  NewLine;
  if FMesh.MeshType = mt3D then
  begin
    ColIndex := 5;
    for LayerIndex := 1 to LayerCount do
    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      // Z coordinate
      WriteString(Format('  plist=p_z%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      // 3D node number
      WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      // Active or not
      WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  id_type=''indexed'',file=''%s.c_nod'')', [FRoot]));
      NewLine;
    end;
  end;
  NewLine;
end;

procedure TSutraData14BScriptWriter.WriteKrigingFactors;
var
  ScriptFileName: string;
  PLPROC_Location: string;
begin
  if (FPorosityPilotPointFiles.Count > 0)
    or (FThicknessPilotPointFiles.Count > 0) then
  begin
    ScriptFileName := ChangeFileExt(FFileName, StrKrigfactorsscript);
    OpenFile(ScriptFileName);
    try
      WriteString('#Script for PLPROC for saving kriging factors');
      NewLine;
      NewLine;
      ReadPilotPoints;
      ReadDiscretization;
      SaveKrigingFactors;
      PLPROC_Location := GetPLPROC_Location(FFileName, Model);
      Model.KrigfactorsScriptLines.Add(Format('"%0:s" ''%1:s''',
        [PLPROC_Location, ExtractFileName(ScriptFileName)]));

    finally
      CloseFile
    end;
  end;
end;

procedure TSutraData14BScriptWriter.WritePilotPointFiles;
var
  Porosity: TDataArray;
  PilotPointWriter: TPilotPointWriter;
  Thickness: TDataArray;
begin
  Porosity := Model.DataArrayManager.GetDataSetByName(KNodalPorosity);
  if Porosity.PestParametersUsed then
  begin
    PilotPointWriter := TPilotPointWriter.Create(Model, etExport);
    try
      PilotPointWriter.WriteFile(FFileName, Porosity, FPorosityPilotPointFiles,
        'POR', 'POR_');
    finally
      PilotPointWriter.Free;
    end;
  end;

  if Model.SutraMesh.MeshType in [mt2D, mtProfile] then
  begin
    Thickness := Model.DataArrayManager.GetDataSetByName(KNodalThickness);
    if Thickness.PestParametersUsed then
    begin
      PilotPointWriter := TPilotPointWriter.Create(Model, etExport);
      try
        PilotPointWriter.WriteFile(FFileName, Thickness, FThicknessPilotPointFiles,
          'Z', 'Z_');
      finally
        PilotPointWriter.Free;
      end;
    end;
  end;
end;

procedure TCustomSutraPlprocFileWriter.GetParameterNames(ParameterNames: TStringList);
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  if Model.PestUsed then
  begin
    ParameterNames.Clear;
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

{ TSutraElementDataWriter }

constructor TSutraElementDataWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FMesh := AModel.SutraMesh;
end;

class function TSutraElementDataWriter.Extension: string;
begin
  Result := '.PstValues';
end;

procedure TSutraElementDataWriter.WriteFile(var AFileName: string;
  DataArray: TDataArray);
var
  ParameterNames: TStringList;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  ParamArray: TDataArray;
  LayerIndex: Integer;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  AnElement3D: TSutraElement3D;
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
      WriteString('EN2D ');
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        if FMesh.MeshType = mt3D then
        begin
          WriteString(Format('EN3D%0:d Layer%0:d ', [LayerIndex+1]));
        end;
        WriteString(Format('%0:s%1:d ', [DataArray.Name, LayerIndex+1]));
        if DataArray.DataType = rdtDouble then
        begin
          WriteString(Format('%0:sParameter%1:d ', [DataArray.Name, LayerIndex+1]))
        end;
      end;
      NewLine;

      for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
      begin
        AnElement2D := FMesh.Mesh2D.Elements[ElementIndex];
        WriteInteger(AnElement2D.ElementNumber+1);
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          if FMesh.MeshType = mt3D then
          begin
            AnElement3D := FMesh.ElementArray[LayerIndex,ElementIndex];
            WriteInteger(AnElement3D.ElementNumber + 1);
            WriteInteger(LayerIndex + 1);
          end;
          if ParamArray <> nil then
          begin
            PIndex := ParameterNames.IndexOf(LowerCase(
              ParamArray.StringData[LayerIndex, 0, ElementIndex]));
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
                  WriteFloat(DataArray.RealData[LayerIndex, 0, ElementIndex]);
                end
                else
                begin
                  WriteFloat(DataArray.RealData[LayerIndex, 0, ElementIndex]
                    /AParam.Value);
                end;
              end;
            rdtInteger:
              begin
                WriteInteger(DataArray.IntegerData[LayerIndex, 0, ElementIndex]);
              end;
            rdtBoolean:
              begin
                WriteInteger(Ord(DataArray.BooleanData[LayerIndex, 0, ElementIndex]));
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

procedure TSutraElementDataWriter.WriteFile(var AFileName: string;
  DataArrayName: string; DataType: TRbwDataType);
var
  LayerIndex: Integer;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  AElement3D: TSutraElement3D;
  LayerCount: Integer;
begin
  FFileName := ChangeFileExt(AFileName, '.' + DataArrayName);

  OpenFile(FFileName);
  try
    WriteString('EN2D ');
    LayerCount := FMesh.LayerCount;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      if FMesh.MeshType = mt3D then
      begin
        WriteString(Format('EN3D Layer%d ', [LayerIndex+1]));
      end;
      WriteString(Format('%0:s%1:d ', [DataArrayName, LayerIndex+1]));
      if DataType = rdtDouble then
      begin
        WriteString(Format('%0:sParameter%1:d ', [DataArrayName, LayerIndex+1]))
      end;
    end;
    NewLine;

    for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
    begin
      AnElement2D := FMesh.Mesh2D.Elements[ElementIndex];
      WriteInteger(AnElement2D.ElementNumber+1);
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        if FMesh.MeshType = mt3D then
        begin
          AElement3D := FMesh.ElementArray[LayerIndex,ElementIndex];
          WriteInteger(AElement3D.ElementNumber + 1);
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

{ TSutraData15BScriptWriter }

constructor TSutraData15BScriptWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  Assert(Model.ModelSelection in SutraSelection);
  FParameterNames := TStringList.Create;
  FDataRecordList := TDataRecordList.Create;
  FUsedParamList := TStringList.Create;

  FUsedParamList.Sorted := True;
  FUsedParamList.Duplicates := dupIgnore;
end;

destructor TSutraData15BScriptWriter.Destroy;
begin
  FUsedParamList.Free;
  FDataRecordList.Free;
  FParameterNames.Free;
  inherited;
end;

class function TSutraData15BScriptWriter.Extension: string;
begin
  result := '.15B.script';
end;

procedure TSutraData15BScriptWriter.GetUsedParameters;
  procedure CreateDataRecord(DataArray: TDataArray; const ID, Prefix: string);
  var
    DataRecord: TDataRecord;
  begin
    if DataArray <> nil then
    begin
      DataRecord := TDataRecord.Create;
      FDataRecordList.Add(DataRecord);
      DataRecord.DataArray := DataArray;
      DataRecord.ID := ID;
      DataRecord.Prefix := Prefix;
    end;
  end;
var
  DataArray: TDataArray;
  Options: TSutraOptions;
  Mesh: TSutraMesh3D;
  Index: Integer;
  ParamArray: TDataArray;
  ElementIndex: Integer;
  LayerIndex: Integer;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
begin
  Options := Model.SutraOptions;
  Mesh := Model.SutraMesh;
  {$REGION 'PMAX'}
  DataArray := nil;
  case Options.TransportChoice of
    tcSolute, tcEnergy:
      DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumPermeability);
    tcSoluteHead:
      DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumK);
    else Assert(False);
  end;
//    DataRoot := 'PMAX';
  CreateDataRecord(DataArray,  'PMAX', 'PMX');
  {$ENDREGION}

  {$REGION 'PMID'}
  DataArray := nil;
  if Mesh.MeshType = mt3D then
  begin
    case Options.TransportChoice of
      tcSolute, tcEnergy:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMiddlePermeability);
      tcSoluteHead:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMiddleK);
      else Assert(False);
    end;
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'PMID', 'PMD');
  {$ENDREGION}

  {$REGION 'PMIN'}
  DataArray := nil;
  case Options.TransportChoice of
    tcSolute, tcEnergy:
      DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumPermeability);
    tcSoluteHead:
      DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumK);
    else Assert(False);
  end;
  CreateDataRecord(DataArray, 'PMIN', 'PMN');
  {$ENDREGION}

{$REGION 'ANGLE1'}
  DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalAngle);
  CreateDataRecord(DataArray, 'ANGLE1', 'AN1');
{$ENDREGION}

{$REGION 'ANGLE2'}
  if Mesh.MeshType = mt3D then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KVerticalAngle);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'ANGLE2', 'AN2');
{$ENDREGION}

{$REGION 'ANGLE3'}
  if Mesh.MeshType = mt3D then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KRotationalAngle);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'ANGLE3', 'AN3');
{$ENDREGION}

{$REGION 'ALMAX'}
  DataArray := Model.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
  CreateDataRecord(DataArray, 'ALMAX', 'AMX');
{$ENDREGION}

{$REGION 'ALMID'}
  if Mesh.MeshType = mt3D then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KMidLongitudinalDisp);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'ALMID', 'AMD');
{$ENDREGION}

{$REGION 'ALMIN'}
  DataArray := Model.DataArrayManager.GetDataSetByName(KMinLongitudinalDisp);
  CreateDataRecord(DataArray, 'ALMIN', 'AMN');
{$ENDREGION}

{$REGION 'ATMAX'}
  DataArray := Model.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
  CreateDataRecord(DataArray, 'ATMAX', 'TMX');
{$ENDREGION}

{$REGION 'ATMID'}
  if Mesh.MeshType = mt3D then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KMidTransverseDisp);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'ATMID', 'TMD');
{$ENDREGION}

{$REGION 'ATMIN'}
  DataArray := Model.DataArrayManager.GetDataSetByName(KMinTransverseDisp);
  CreateDataRecord(DataArray, 'ATMIN', 'TMN');
{$ENDREGION}

  for Index := 0 to FDataRecordList.Count - 1 do
  begin
    DataArray := FDataRecordList[Index].DataArray;

    if DataArray.PestParametersUsed then
    begin
      ParamArray := Model.DataArrayManager.GetDataSetByName
        (DataArray.ParamDataSetName);

      for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
      begin
        for LayerIndex := 0 to ParamArray.LayerCount - 1 do
        begin
          PIndex := FParameterNames.IndexOf(LowerCase(
            ParamArray.StringData[LayerIndex, 0, ElementIndex]));
          if PIndex >=0 then
          begin
            AParam := FParameterNames.Objects[PIndex] as TModflowSteadyParameter;
          end
          else
          begin
            AParam := nil;
          end;

          if AParam <> nil then
          begin
            FUsedParamList.AddObject(AParam.ParameterName, AParam);
          end;
        end;
      end;
    end
  end;


end;

//procedure TSutraData15BScriptWriter.ReadPilotPoints;
//var
//  PIndex: Integer;
//  AParam: TModflowSteadyParameter;
//  DataArrayIndex: Integer;
//  ADataRec: TDataRecord;
//  procedure HandlePilotPointList(PilotPointFiles: TPilotPointFiles; const DataId: string);
//  var
//    PPIndex: Integer;
//    FileProperties: TPilotPointFileObject;
//    PListName: string;
//  begin
//    for PPIndex := 0 to PilotPointFiles.Count - 1 do
//    begin
//      FileProperties := PilotPointFiles[PPIndex];
//      if FileProperties.Parameter = AParam then
//      begin
//        WriteString(Format('%0:s_PilotPoints%d = read_list_file(skiplines=0,dimensions=2, &',
//          [DataId, PPIndex+1]));
//        NewLine;
//        PListName := Format('%0:s_%1:s_%2:d',
//          [DataId, AParam.ParameterName, FileProperties.Layer + 1]);
//        WriteString(Format('  plist=''%0:s'';column=%1:d, &',
//          [PListName, 5]));
//        NewLine;
//        WriteString(Format('  id_type=''character'',file=''%s'')',
//          [ExtractFileName(FileProperties.FileName)]));
//        NewLine;
//      end;
//    end;
//  end;
//begin
//  WriteString('#Read pilot point data');
//  NewLine;
//  for PIndex := 0 to FUsedParamList.Count - 1 do
//  begin
//    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
//    if AParam.UsePilotPoints then
//    begin
//      for DataArrayIndex := 0 to FDataRecordList.Count - 1 do
//      begin
//        ADataRec := FDataRecordList[DataArrayIndex];
//        HandlePilotPointList(ADataRec.PilotPointFiles, ADataRec.ID);
//      end;
//    end;
//  end;
//  NewLine;
//end;

//procedure TSutraData15BScriptWriter.SaveKrigingFactors;
//var
//  FileProperties: TPilotPointFileObject;
//  AParam: TModflowSteadyParameter;
//  Index: Integer;
//  ADataRec: TDataRecord;
//  KrigingFactorsFileRoot: string;
//  procedure HandleDataArray(DataID: string; DataArray: TDataArray;
//    PilotPointFiles: TPilotPointFiles; FileName: string);
//  var
//    PIndex: Integer;
//    PPIndex: Integer;
//  begin
//    for PIndex := 0 to FUsedParamList.Count - 1 do
//    begin
//      AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
//      if AParam.UsePilotPoints then
//      begin
//        for PPIndex := 0 to PilotPointFiles.Count - 1 do
//        begin
//          FileProperties := PilotPointFiles[PPIndex];
//          if FileProperties.Parameter = AParam then
//          begin
//            WriteString('calc_kriging_factors_auto_2d( &');
//            NewLine;
//            WriteString(Format('  target_clist=%s, &', [KDisName]));
//            NewLine;
//            WriteString(Format('  source_clist=%0:s_PilotPoints%1:d, &',
//              [DataID, PPIndex+1]));
//            NewLine;
//            WriteString(Format('  file=%0:s%1:d;format=formatted)',
//              [ExtractFileName(FileName), PPIndex+1]));
//            NewLine;
//          end;
//        end;
//      end;
//    end;
//    NewLine;
//  end;
//begin
//  WriteString('#Save Kriging factors');
//  NewLine;
//  for Index := 0 to FDataRecordList.Count - 1 do
//  begin
//    ADataRec := FDataRecordList[Index];
//    if ADataRec.PilotPointFiles.Count > 0 then
//    begin
//      KrigingFactorsFileRoot := GetKrigFactorRoot(ADataRec);
//      HandleDataArray(ADataRec.ID, ADataRec.DataArray, ADataRec.PilotPointFiles,
//        KrigingFactorsFileRoot);
//    end;
//  end;
//
//  NewLine;
//end;

procedure TSutraData15BScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  Mesh: TSutraMesh3D;
  LayerCount: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
  Options: TSutraOptions;
  DataArray: TDataArray;
  UsedDataRoots: TStringList;
  DataRoot: string;
  RootIndex: Integer;
  procedure ReadData(DataArray: TDataArray; const DataRoot: string);
  var
    LayerIndex: Integer;
    ArrayFileName: string;
  begin
    if DataArray <> nil then
    begin
      UsedDataRoots.Add(DataRoot);
      WriteString(Format('# Read %s', [DataArray.Name]));
      NewLine;
      ColIndex := 1;

      Inc(ColIndex);

      if not DataArray.PestParametersUsed then
      begin

        for LayerIndex := 1 to LayerCount do
        begin
          // PLPROC has a limit of 5 s_lists per call of read_list_file.
          // To avoid reaching that limit, a separate call is used for each layer.
          WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
          NewLine;

          if Mesh.MeshType = mt3D then
          begin
            Inc(ColIndex);
            Inc(ColIndex);
          end;

          WriteString(Format('  plist=p_%0:s%1:d;column=%2:d, &',
            [DataRoot, LayerIndex, ColIndex]));
          NewLine;
          Inc(ColIndex);

          WriteString(Format('  slist=s_%0:sPar%1:d;column=%2:d, &',
            [DataRoot, LayerIndex, ColIndex]));
          NewLine;
          Inc(ColIndex);

          WriteString(Format('  file=''%0:s.%1:s'')',
            [FRoot, DataArray.Name]));
          NewLine;
        end;
      end
      else
      begin
        for LayerIndex := 1 to LayerCount do
        begin
          WriteString(Format('  p_%0:s%1:d=new_plist(reference_clist=''cl_Discretization'',value=1.0)',
            [DataRoot, LayerIndex]));
          NewLine;
          ArrayFileName := Format('arrays\%0:s.%1:s_%2:d.arrays',
            [FRoot, DataArray.Name, LayerIndex]);
          Model.FilesToDelete.Add(ArrayFileName);
          WriteString(Format('  p_%0:s%1:d.read_list_as_array(file=''%2:s'')',
            [DataRoot, LayerIndex, ArrayFileName]));
          NewLine;
          WriteString(Format('  s_%0:sPar%1:d=new_slist(reference_clist=''cl_Discretization'',value=%1:d)',
            [DataRoot, LayerIndex]));
          NewLine;
        end;
      end;
      NewLine;
    end;
  end;
begin
  if ScriptChoice = scWriteTemplate then
  begin
    FFileName := FFileName + '.tpl';
  end;

  Mesh := Model.SutraMesh;
  Options := Model.SutraOptions;

  UsedDataRoots := TStringList.Create;
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
      LayerCount := Mesh.LayerCount;
    end
    else
    begin
      LayerCount := 1;
    end;

    {$REGION 'Element discretization'}
    ReadDiscretization;
    {$ENDREGION}

    WriteString('#Read data to modify');
    NewLine;
    {$REGION 'Unsaturated zone'}
    WriteString('# Read Unsaturated Zone');
    NewLine;
    ColIndex := 2;
    for LayerIndex := 1 to LayerCount do
    begin
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;

      if Mesh.MeshType = mt3D then
      begin
        Inc(ColIndex,2);
      end;

      WriteString(Format('  slist=s_LREG%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);

      WriteString(Format('  file=''%s.Unsat_Region_Elements'')', [FRoot]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    {$REGION 'PMAX'}
    DataArray := nil;
    case Options.TransportChoice of
      tcSolute, tcEnergy:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumPermeability);
      tcSoluteHead:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumK);
      else Assert(False);
    end;
//    DataRoot := 'PMAX';
    ReadData(DataArray,  'PMAX');
    {$ENDREGION}

    {$REGION 'PMID'}
    DataArray := nil;
    if Mesh.MeshType = mt3D then
    begin
      case Options.TransportChoice of
        tcSolute, tcEnergy:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMiddlePermeability);
        tcSoluteHead:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMiddleK);
        else Assert(False);
      end;
    end
    else
    begin
      DataArray := nil;
    end;
    ReadData(DataArray, 'PMID');
    {$ENDREGION}

    {$REGION 'PMIN'}
    DataArray := nil;
    case Options.TransportChoice of
      tcSolute, tcEnergy:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumPermeability);
      tcSoluteHead:
        DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumK);
      else Assert(False);
    end;
    ReadData(DataArray, 'PMIN');
    {$ENDREGION}

    {$REGION 'ANGLE1'}
    DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalAngle);
    ReadData(DataArray, 'ANGLE1');
    {$ENDREGION}

    {$REGION 'ANGLE2'}
    if Mesh.MeshType = mt3D then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(KVerticalAngle);
    end
    else
    begin
      DataArray := nil;
    end;
    ReadData(DataArray, 'ANGLE2');
    {$ENDREGION}

    {$REGION 'ANGLE3'}
    if Mesh.MeshType = mt3D then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(KRotationalAngle);
    end
    else
    begin
      DataArray := nil;
    end;
    ReadData(DataArray, 'ANGLE3');
    {$ENDREGION}

    {$REGION 'ALMAX'}
    DataArray := Model.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
    ReadData(DataArray, 'ALMAX');
    {$ENDREGION}

    {$REGION 'ALMID'}
    if Mesh.MeshType = mt3D then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(KMidLongitudinalDisp);
    end
    else
    begin
      DataArray := nil;
    end;
    ReadData(DataArray, 'ALMID');
    {$ENDREGION}

    {$REGION 'ALMIN'}
    DataArray := Model.DataArrayManager.GetDataSetByName(KMinLongitudinalDisp);
    ReadData(DataArray, 'ALMIN');
    {$ENDREGION}

    {$REGION 'ATMAX'}
    DataArray := Model.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
    ReadData(DataArray, 'ATMAX');
    {$ENDREGION}

    {$REGION 'ATMID'}
    if Mesh.MeshType = mt3D then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(KMidTransverseDisp);
    end
    else
    begin
      DataArray := nil;
    end;
    ReadData(DataArray, 'ATMID');
    {$ENDREGION}

    {$REGION 'ATMIN'}
    DataArray := Model.DataArrayManager.GetDataSetByName(KMinTransverseDisp);
    ReadData(DataArray, 'ATMIN');
    {$ENDREGION}

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if AParam.UsePilotPoints then
      begin
        Continue;
      end;
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

    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      ['cl_Discretization']));
    NewLine;

    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      WriteString(Format('# applying parameter %s', [AParam.ParameterName]));
      NewLine;
      for RootIndex := 0 to UsedDataRoots.Count-1 do
      begin
        DataRoot := UsedDataRoots[RootIndex];
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          if AParam.UsePilotPoints then
          begin
            Continue;
          end
          else
          begin
            WriteString('    # Substituting parameter values in zones');
            NewLine;
            WriteString(Format('p_%3:s%0:d(select=(s_%3:sPar%0:d == %2:d)) = p_%3:s%0:d * %1:s',
              [LayerIndex+1, AParam.ParameterName, ParameterIndex+1, DataRoot]));
            NewLine;
          end;
        end;
      end;
      NewLine;
    end;
    {$ENDREGION}

    {$REGION 'Write values'}
    WriteString('# Write new data values');
    NewLine;
    if Mesh.MeshType = mt3D then
    begin
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString('write_column_data_file(header = ''no'', &');
        NewLine;
        WriteString(Format('  file=''%s.15B_%1:d'';delim="space", &',
          [FRoot, LayerIndex]));
        NewLine;
        WriteString(Format('  select=(s_Active_%d == 1), &',
          [LayerIndex]));
        NewLine;
        WriteString(Format('  slist=''s_EN3D%0:d'', &', [LayerIndex]));
        NewLine;
        WriteString(Format('  slist=s_LREG%0:d, &', [LayerIndex]));
        NewLine;
        for RootIndex := 0 to UsedDataRoots.Count-1 do
        begin
          DataRoot := UsedDataRoots[RootIndex];
          WriteString(Format('  plist=p_%1:s%0:d', [LayerIndex, DataRoot]));
          if RootIndex = UsedDataRoots.Count-1 then
          begin
            WriteString(')');
          end
          else
          begin
            WriteString(', &');
          end;
          NewLine;
        end;
      end;
    end
    else
    begin
      WriteString('write_column_data_file(header = ''no'', &');
      NewLine;
      WriteString(Format('  file=''%s.15B'';delim="space", &', [FRoot]));
      NewLine;
      WriteString('  clist_spec=''id'', &');
      NewLine;
      WriteString('  slist=s_LREG1, &');
      NewLine;
      for RootIndex := 0 to UsedDataRoots.Count-1 do
      begin
        DataRoot := UsedDataRoots[RootIndex];
        WriteString(Format('  plist=p_%s1', [DataRoot]));
        if RootIndex = UsedDataRoots.Count-1 then
        begin
          WriteString(')');
        end
        else
        begin
          WriteString(', &');
        end;
        NewLine;
      end;
    end;
    {$ENDREGION}
  finally
    CloseFile;
    UsedDataRoots.Free;
  end;
end;

procedure TSutraData15BScriptWriter.WriteFiles(var AFileName: string);
var
//  Index: Integer;
  PLPROC_Location: string;
begin
  FFileName := FileName(AFileName);
  Model.SutraPestScripts.Add(FFileName);
  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  Model.PestTemplateLines.Add(Format('"%0:s" ''%1:s''', [PLPROC_Location, ExtractFileName(FFileName)]));
  FRoot := ExtractFileName(ChangeFileExt(AFileName , ''));
  GetParameterNames(FParameterNames);
  GetUsedParameters;
  WritePilotPointFiles;

  WriteAFile(scWriteScript);
  WriteAFile(scWriteTemplate);

end;

//function TSutraData15BScriptWriter.GetKrigFactorRoot(ADataRec: TDataRecord): string;
//begin
//  result := FRoot + '.' + ADataRec.DataArray.Name + '.Factors';
//end;

procedure TSutraData15BScriptWriter.ReadDiscretization;
var
  Mesh: TSutraMesh3D;
  LayerCount: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
begin
  Mesh := Model.SutraMesh;
  if Mesh.MeshType = mt3D then
  begin
    LayerCount := Mesh.LayerCount;
  end
  else
  begin
    LayerCount := 1;
  end;

  WriteString('#Read SUTRA element information file');
  NewLine;
  WriteString('cl_Discretization = read_list_file(skiplines=1,dimensions=2, &');
  NewLine;
  WriteString('  plist=p_x;column=2, &');
  NewLine;
  WriteString('  plist=p_y;column=3, &');
  NewLine;
  WriteString('  plist=p_EN2D;column=4, &');
  NewLine;
  WriteString(Format('  id_type=''indexed'',file=''%s.c_ele'')', [FRoot]));
  NewLine;

  if Mesh.MeshType = mt3D then
  begin
    ColIndex := 5;
    for LayerIndex := 1 to LayerCount do
    begin
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      WriteString(Format('  plist=p_z%0:d;column=%1:d, &',
        [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  slist=s_EN3D%0:d;column=%1:d, &',
        [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &',
        [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  id_type=''indexed'',file=''%s.c_ele'')', [FRoot]));
      NewLine;
    end;
    NewLine;
  end;

  NewLine;
end;

//procedure TSutraData15BScriptWriter.WriteKrigingFactors;
//var
//  ScriptFileName: string;
//  index: Integer;
//  PilotPointsUsed: Boolean;
//  PLPROC_Location: string;
//begin
//  PilotPointsUsed := False;
//  for index := 0 to FDataRecordList.Count - 1 do
//  begin
//    if FDataRecordList[index].PilotPointFiles.Count > 0 then
//    begin
//      PilotPointsUsed := True;
//      break;
//    end;
//  end;
//  if PilotPointsUsed then
//  begin
//    ScriptFileName := ChangeFileExt(FFileName, StrKrigfactorsscript);
//    OpenFile(ScriptFileName);
//    try
//      WriteString('#Script for PLPROC for saving kriging factors');
//      NewLine;
//      NewLine;
//      ReadPilotPoints;
//      ReadDiscretization;
//      SaveKrigingFactors;
//      PLPROC_Location := GetPLPROC_Location(FFileName, Model);
//      Model.KrigfactorsScriptLines.Add(Format('"%0:s" ''%1:s''',
//        [PLPROC_Location, ExtractFileName(ScriptFileName)]));
//    finally
//      CloseFile
//    end;
//  end;
//end;

procedure TSutraData15BScriptWriter.WritePilotPointFiles;
var
  Index: Integer;
  ADataRec: TDataRecord;
begin
  for Index := 0 to FDataRecordList.Count - 1 do
  begin
    ADataRec := FDataRecordList[Index];
    if ADataRec.DataArray.PestParametersUsed then
    begin
      WritePestZones(ADataRec.DataArray, FFileName, ADataRec.Id, ADataRec.Prefix);
    end;
  end;
end;

{ TSutraInitCondScriptWriter }

constructor TSutraInitCondScriptWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  Assert(Model.ModelSelection in SutraSelection);
  FParameterNames := TStringList.Create;
  FPilotPointFiles := TPilotPointFiles.Create;
  FUsedParamList := TStringList.Create;

  FUsedParamList.Sorted := True;
  FUsedParamList.Duplicates := dupIgnore;
end;

destructor TSutraInitCondScriptWriter.Destroy;
begin
  FUsedParamList.Free;
  FPilotPointFiles.Free;
  FParameterNames.Free;
  inherited;
end;

class function TSutraInitCondScriptWriter.Extension: string;
begin
  result := '';
end;

function TSutraInitCondScriptWriter.GetKrigFactorRoot: string;
begin
  result := FRoot + '.' + FDataArrayName + '.Factors';
end;

procedure TSutraInitCondScriptWriter.GetUsedParameters;
var
  ParamArray: TDataArray;
  NodeIndex: Integer;
  LayerIndex: Integer;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  PName: string;
begin
  if FDataArray.PestParametersUsed then
  begin
    ParamArray := Model.DataArrayManager.GetDataSetByName
      (FDataArray.ParamDataSetName);

    for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
    begin
      for LayerIndex := 0 to ParamArray.LayerCount - 1 do
      begin
        PName := Trim(ParamArray.StringData[LayerIndex, 0, NodeIndex]);
        if PName <> '' then
        begin
          PIndex := FParameterNames.IndexOf(LowerCase(PName));
          if PIndex >=0 then
          begin
            AParam := FParameterNames.Objects[PIndex] as TModflowSteadyParameter;
          end
          else
          begin
            AParam := nil;
          end;

          if AParam <> nil then
          begin
            FUsedParamList.AddObject(AParam.ParameterName, AParam);
          end;
        end;
      end;
    end;
  end
end;

procedure TSutraInitCondScriptWriter.ReadPilotPoints;
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  procedure HandlePilotPointList(PilotPointFiles: TPilotPointFiles; const DataId: string);
  var
    PPIndex: Integer;
    FileProperties: TPilotPointFileObject;
    PListName: string;
  begin
    for PPIndex := 0 to PilotPointFiles.Count - 1 do
    begin
      FileProperties := PilotPointFiles[PPIndex];
      if FileProperties.Parameter = AParam then
      begin
        WriteString(Format('%0:s_PilotPoints%d = read_list_file(skiplines=0,dimensions=2, &',
          [DataId, PPIndex+1]));
        NewLine;
        PListName := Format('%0:s_%1:s_%2:d',
          [DataId, AParam.ParameterName, FileProperties.Layer + 1]);
        WriteString(Format('  plist=''%0:s'';column=%1:d, &',
          [PListName, 5]));
//          Inc(ColIndex);
        NewLine;
        WriteString(Format('  id_type=''character'',file=''%s'')',
          [ExtractFileName(FileProperties.FileName)]));
        NewLine;
      end;
    end;
  end;
begin
  WriteString('#Read pilot point data');
  NewLine;
  for PIndex := 0 to FUsedParamList.Count - 1 do
  begin
    AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
    if AParam.UsePilotPoints then
    begin
      HandlePilotPointList(FPilotPointFiles, FID);
    end;
  end;
  NewLine;
end;

procedure TSutraInitCondScriptWriter.SaveKrigingFactors;
var
  FileProperties: TPilotPointFileObject;
  AParam: TModflowSteadyParameter;
  KrigingFactorsFileRoot: string;
  procedure HandleDataArray(DataID: string; DataArray: TDataArray;
    PilotPointFiles: TPilotPointFiles; FileName: string);
  var
    PIndex: Integer;
    PPIndex: Integer;
  begin
    for PIndex := 0 to FUsedParamList.Count - 1 do
    begin
      AParam := FUsedParamList.Objects[PIndex] as TModflowSteadyParameter;
      if AParam.UsePilotPoints then
      begin
        for PPIndex := 0 to PilotPointFiles.Count - 1 do
        begin
          FileProperties := PilotPointFiles[PPIndex];
          if FileProperties.Parameter = AParam then
          begin
            WriteString('calc_kriging_factors_auto_2d( &');
            NewLine;
            WriteString(Format('  target_clist=%s, &', [KDisName]));
            NewLine;
            WriteString(Format('  source_clist=%0:s_PilotPoints%1:d, &',
              [DataID, PPIndex+1]));
            NewLine;
            WriteString(Format('  file=%0:s%1:d;format=formatted)',
              [ExtractFileName(FileName), PPIndex+1]));
            NewLine;
          end;
        end;
      end;
    end;
    NewLine;
  end;
begin
  WriteString('#Save Kriging factors');
  NewLine;
  if FPilotPointFiles.Count > 0 then
  begin
    KrigingFactorsFileRoot := GetKrigFactorRoot;
    HandleDataArray(FID, FDataArray, FPilotPointFiles,
      KrigingFactorsFileRoot);
  end;

  NewLine;
end;

procedure TSutraInitCondScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  Mesh: TSutraMesh3D;
  LayerCount: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
  FileProperties: TPilotPointFileObject;
  UsedFileProperties: TPilotPointFileObject;
  FileIndex: Integer;
  PListName: string;
  PIndex: Integer;
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
    ReadDiscretization;

    if FMesh.MeshType = mt3D then
    begin
      Read3DDiscretization
    end;
    {$ENDREGION}

    WriteString('#Read data to modify');
    NewLine;
    {$REGION data}
    WriteString(Format('# Read %s', [FDataArrayName]));
    NewLine;
    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
    NewLine;
    ColIndex := 1;

    WriteString(Format('  slist=s_NN2D_Data;column=%d, &', [ColIndex]));
    NewLine;
    Inc(ColIndex);

    WriteString(Format('  file=''%0:s.%1:s'')', [FRoot, FDataArrayName]));
    NewLine;

    for LayerIndex := 1 to LayerCount do
    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      if Mesh.MeshType = mt3D then
      begin
        Inc(ColIndex);

        WriteString(Format('  slist=s_Layer%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
        NewLine;
        Inc(ColIndex);
      end;

      WriteString(Format('  plist=p_%2:s%0:d;column=%1:d, &', [LayerIndex, ColIndex, 'Data']));
      NewLine;
      Inc(ColIndex);

      WriteString(Format('  slist=s_%2:sPar%0:d;column=%1:d, &', [LayerIndex, ColIndex, 'Data']));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  file=''%0:s.%1:s'')', [FRoot, FDataArrayName]));
      NewLine;
    end;
    NewLine;
    {$ENDREGION}

    ReadPilotPoints;

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if AParam.UsePilotPoints then
      begin
        Continue;
      end;
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

    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      ['cl_Discretization']));
    NewLine;

    if Mesh.MeshType = mt3D then
    begin
      WriteString(Format('temp3D=new_plist(reference_clist=%s,value=0.0)',
        ['cl_Discretization3D']));
      NewLine;
    end;

    for ParameterIndex := 0 to FParameterNames.Count - 1 do
    begin
      AParam := FParameterNames.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      WriteString(Format('# applying parameter %s', [AParam.ParameterName]));
      NewLine;
      for LayerIndex := 1 to LayerCount do
      begin
        if AParam.UsePilotPoints then
        begin
          WriteString('    # Substituting interpolated values');
          NewLine;

          UsedFileProperties := nil;
          PIndex := 0;

          for FileIndex := 0 to FPilotPointFiles.Count - 1 do
          begin
            FileProperties := FPilotPointFiles[FileIndex];
            if (FileProperties.Parameter = AParam)
              and (FileProperties.Layer = LayerIndex-1) then
            begin
              UsedFileProperties := FileProperties;
              PIndex := FileIndex;
              break;
            end;
          end;

          if UsedFileProperties <> nil then
          begin
            PListName := Format('%0:s_%1:s_%2:d',
              [FID,AParam.ParameterName, LayerIndex]);
            WriteString('    # Get interpolated values');
            NewLine;
            WriteString(Format(
              '    temp=%0:s.krige_using_file(file=''%1:s%2:d'';form=''formatted'', &',
              [PListName, ExtractFileName(GetKrigFactorRoot), PIndex+1]));
            NewLine;
            if AParam.Transform = ptLog then
            begin
              WriteString('      transform=''log'')');
            end
            else
            begin
              WriteString('      transform=''none'')');
            end;
            NewLine;
            WriteString('    # Write interpolated values in zones');
            NewLine;
            WriteString(Format(
              '    p_Data%0:d(select=(s_DataPar%0:d == %2:d)) = temp',
              [LayerIndex, AParam.ParameterName, ParameterIndex+1, FID]));
            NewLine;
          end
          else
          begin
            WriteString(Format(
              '    # no interpolated values defined for parameter %1:s in layer %0:d',
              [LayerIndex, AParam.ParameterName]));
            NewLine;
          end;

        end
        else
        begin
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format('p_%3:s%0:d(select=(s_%3:sPar%0:d == %2:d)) = p_%3:s%0:d * %1:s',
            [LayerIndex, AParam.ParameterName, ParameterIndex+1, 'Data']));
          NewLine;
        end;

      end;
      NewLine;
    end;
    {$ENDREGION}

    if Mesh.MeshType = mt3D then
    begin
      WriteString('    # Apply values to 3D mesh');
      NewLine;
      for LayerIndex := 1 to LayerCount do
      begin
        WriteString(Format('temp3D(select=(s_Layer3D.eq.%0:d)) &', [LayerIndex]));
        NewLine;
        WriteString(Format('  =  p_Data%0:d.assign_by_relation(relate=slist;source=s_NN2D;target=s_NN2D_3D)', [LayerIndex]));
        NewLine;
      end;
    end;

    WriteString('# Write new data values');
    NewLine;
    if Mesh.MeshType = mt3D then
    begin
        WriteString('write_column_data_file(header = ''no'', &');
        NewLine;
        WriteString(Format('  file=''%s.%1:s'';delim="space", &',
          [FRoot, FID]));
        NewLine;
        WriteString('  select=(s_Active3D == 1), &');
        NewLine;
        WriteString('  plist=temp3D)');
        NewLine;
    end
    else
    begin
      WriteString('write_column_data_file(header = ''no'', &');
      NewLine;
      WriteString(Format('  file=''%0:s.%1:s'';delim="space", &', [FRoot, FID]));
      NewLine;
      WriteString(Format('  plist=p_%s1)', ['Data']));
      NewLine;
    end;
  finally
    CloseFile;
  end;
end;

procedure TSutraInitCondScriptWriter.WriteFiles(var AFileName: string;
  const DataArrayName: string; ID, Prefix: string);
var
  PLPROC_Location: string;
begin
  FID := ID;
  FDataArrayName := DataArrayName;
  FDataArray := Model.DataArrayManager.GetDataSetByName(FDataArrayName);
  FMesh := Model.SutraMesh;
  FFileName := ChangeFileExt(AFileName, '.' + FDataArrayName + '.script');
  Model.SutraPestScripts.Add(FFileName);
  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  Model.PestTemplateLines.Add(Format('"%0:s" ''%1:s''',
   [PLPROC_Location, ExtractFileName(FFileName)]));
  FRoot := ExtractFileName(ChangeFileExt(AFileName , ''));
  GetParameterNames(FParameterNames);
  GetUsedParameters;
  WritePilotPointFiles(Prefix);
  WriteKrigingFactors;

  WriteAFile(scWriteScript);
  WriteAFile(scWriteTemplate);

  Model.PilotPointData.AddPilotPointFileObjects(FPilotPointFiles);
end;

procedure TSutraInitCondScriptWriter.Read3DDiscretization;
begin
  WriteString('#Read 3D SUTRA node information file');
  NewLine;
  WriteString('cl_Discretization3D = read_list_file(skiplines=1,dimensions=3, &');
  NewLine;
  WriteString('  plist=p_x3D;column=2, &');
  NewLine;
  WriteString('  plist=p_y3D;column=3, &');
  NewLine;
  WriteString('  plist=p_z3D;column=4, &');
  NewLine;
  WriteString('  slist=s_NN2D_3D;column=5, &');
  NewLine;
  WriteString('  slist=s_NN3D;column=6, &');
  NewLine;
  WriteString('  slist=s_Active3D;column=7, &');
  NewLine;
  WriteString('  slist=s_Layer3D;column=8, &');
  NewLine;
  WriteString(Format('  id_type=''indexed'',file=''%s.c_nod3D'')', [FRoot]));
  NewLine;
  NewLine;
end;

procedure TSutraInitCondScriptWriter.ReadDiscretization;
var
  LayerIndex: Integer;
  LayerCount: Integer;
  ColIndex: Integer;
begin
  if FMesh.MeshType = mt3D then
  begin
    LayerCount := FMesh.LayerCount + 1;
  end
  else
  begin
    LayerCount := 1;
  end;

  WriteString('#Read SUTRA node information file');
  NewLine;
  WriteString('cl_Discretization = read_list_file(skiplines=1,dimensions=2, &');
  NewLine;
  WriteString('  plist=p_x;column=2, &');
  NewLine;
  WriteString('  plist=p_y;column=3, &');
  NewLine;
  WriteString('  slist=s_NN2D;column=4, &');
  NewLine;
  WriteString(Format('  id_type=''indexed'',file=''%s.c_nod'')', [FRoot]));
  NewLine;
  if FMesh.MeshType = mt3D then
  begin
    ColIndex := 5;
    for LayerIndex := 1 to LayerCount do
    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      WriteString(Format('  plist=p_z%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      WriteString(Format('  id_type=''indexed'',file=''%s.c_nod'')', [FRoot]));
      NewLine;
    end;
  end;
  NewLine;
end;

procedure TSutraInitCondScriptWriter.WriteKrigingFactors;
var
  ScriptFileName: string;
  PilotPointsUsed: Boolean;
  PLPROC_Location: string;
begin
  PilotPointsUsed := FPilotPointFiles.Count > 0;
  if PilotPointsUsed then
  begin
    ScriptFileName := ChangeFileExt(FFileName, StrKrigfactorsscript);
    OpenFile(ScriptFileName);
    try
      WriteString('#Script for PLPROC for saving kriging factors');
      NewLine;
      NewLine;
      ReadPilotPoints;
      ReadDiscretization;
      SaveKrigingFactors;
      PLPROC_Location := GetPLPROC_Location(FFileName, Model);
      Model.KrigfactorsScriptLines.Add(Format('"%0:s" ''%1:s''',
        [PLPROC_Location, ExtractFileName(ScriptFileName)]));
    finally
      CloseFile
    end;
  end;
end;

procedure TSutraInitCondScriptWriter.WritePilotPointFiles(Prefix: string);
var
  PilotPointWriter: TPilotPointWriter;
begin
  if FDataArray.PestParametersUsed then
  begin
    PilotPointWriter := TPilotPointWriter.Create(Model, etExport);
    try
      PilotPointWriter.WriteFile(FFileName, FDataArray,
        FPilotPointFiles, FID, Prefix);
    finally
      PilotPointWriter.Free;
    end;
  end;
end;

{ TDataRecord }

constructor TDataRecord.Create;
begin
  PilotPointFiles := TPilotPointFiles.Create;
end;

destructor TDataRecord.Destroy;
begin
  PilotPointFiles.Free;
  inherited;
end;

{ TSutraNod3DDisWriter }

class function TSutraNod3DDisWriter.Extension: string;
begin
  result := '.c_nod3D';
end;

procedure TSutraNod3DDisWriter.WriteFile(const AFileName: string);
var
  NodeIndex: Integer;
  SutraMesh2D: TSutraMesh2D;
  ANode: TSutraNode2D;
  LayerIndex: Integer;
  SutraMesh3D: TSutraMesh3D;
  ANode3D: TSutraNode3D;
  NN: Integer;
begin
  FInputFileName := FileName(AFileName);
  OpenFile(FInputFileName);
  try
    SutraMesh3D := Model.SutraMesh;
    SutraMesh2D := SutraMesh3D.Mesh2D;
    WriteString(' Index X Y Z Node_Number2D Node_Number3D Active Layer');
    NN := 1;
    NewLine;
    for NodeIndex := 0 to SutraMesh2D.Nodes.Count - 1 do
    begin
      ANode := SutraMesh2D.Nodes[NodeIndex];
      if SutraMesh3D.MeshType = mt3D then
      begin
        for LayerIndex := 0 to SutraMesh3D.LayerCount do
        begin
          ANode3D := SutraMesh3D.NodeArray[LayerIndex,ANode.Number];
          WriteInteger(NN);
          WriteFloat(ANode.Location.x);
          WriteFloat(ANode.Location.y);
          WriteFloat(ANode3D.Z);
          WriteInteger(ANode.Number+1);
          WriteInteger(ANode3D.Number+1);
          if ANode3D.Active then
          begin
            WriteInteger(1);
          end
          else
          begin
            WriteInteger(0);
          end;
          WriteInteger(LayerIndex+1);
          NewLine;
          Inc(NN);
        end;
      end;
    end;
  finally
    CloseFile;
  end;
end;

end.
