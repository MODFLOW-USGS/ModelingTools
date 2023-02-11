{
This unit defines functions that can be used to communicate with PLPROC.
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
    FParamValuesFileNameRoot: string;
    FTemplateNeeded: Boolean;
    procedure WriteTemplateFile(const AFileName: string;
      ScriptChoice: TScriptChoice);
    procedure WriteValuesFile(const AFileName: string);
    procedure WritePilotPointsFile(const AFileName, Prefix: string);
    procedure AddParametersToPVAL;
    procedure ReadPilotPoints;
    procedure ReadDiscretization(const AFileName: string);
    procedure SaveKrigingFactors(const AFileName: string);
    procedure WriteKrigingFactorsScript(const AFileName: string);
    procedure SetParamValuesFileName(var FileIndexUnit: Integer);
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
    FFileNameRoot: string;
    procedure SetCNodValuesFileName(var FileIndexUnit: Integer);
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
    procedure GetInputFileName(var FileIndex: Integer; const AFileName: string);
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
    FSutra4NodePilotPointFiles: TObjectList<TPilotPointFiles>;
    FUsedParamList: TStringList;
    FMesh: TSutraMesh3D;
    FPorKrigingFactorsFileRoot: string;
    FThicknessKrigingFactorsFileRoot: string;
    FDataSetNames: TStringList;
    FDataSetAbbreviations: TStringList;
    FSutra4DataSetRoots: TStringList;
    SListCount: Integer;
    procedure WriteAFile(ScriptChoice: TScriptChoice);
    procedure WritePilotPointFiles;
    procedure GetUsedParameters;
    procedure WriteKrigingFactors;
    procedure ReadPilotPoints;
    procedure ReadNodeDiscretization(ReadSLists: Boolean; LayerToRead: Integer = -1);
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

  TAnisotropyWriter = class(TCustomFileWriter)
  private
    FRoot: string;
    FDataArray: TDataArray;
    FReferenceDataArray: TDataArray;
    FAnistropy: array of Double;
    procedure CalculateAnisotropy(LayerIndex: Integer);
    procedure WriteTemplate(LayerIndex: Integer);
  protected
    class function Extension: string; override;
  public
    Procedure WriteFile(const Root: string; DataArray, ReferenceDataArray: TDataArray);
  end;

  TSutraData15BScriptWriter = class(TCustomSutraPlprocFileWriter)
  private
    FRoot: string;
    FFileName: string;
    FParameterNames: TStringList;
    FDataRecordList: TDataRecordList;
    FUsedParamList: TStringList;
    // @name must be modified if new variables are added to data set 15B.
    procedure WriteAFile(ScriptChoice: TScriptChoice);
    procedure WritePilotPointFiles;
    // @name must be modified if new variables are added to data set 15B.
    procedure GetUsedParameters;
//    procedure ReadDiscretization;
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
    procedure ReadPilotPoints(Layer: integer);
    procedure SaveKrigingFactors;
    function GetKrigFactorRoot: string;
    procedure ReadDiscretization(ReadSLists: Boolean);
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
  ModflowParameterUnit, OrderedCollectionUnit, SutraOptionsUnit, System.Math;

const
  KDisName = 'cl_Discretization';
  DataToWriteCount = 20;

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
  inherited Create(AModel, EvaluationType);
//  FSubstituteParamValuesInZones := SubstituteParamValuesInZones;
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
  if FDataArray <> nil then
  begin
    FDataArray.TemplateNeeded := False;
  end;
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
    FTemplateNeeded := False;
    WriteTemplateFile(AFileName, scWriteScript);
    if FTemplateNeeded then
    begin
      WriteTemplateFile(AFileName, scWriteTemplate);
    end;
    AddParametersToPVAL;

    Model.DataArrayManager.AddDataSetToCache(FDataArray);
    Model.DataArrayManager.AddDataSetToCache(FParamDataArray);

    Model.PilotPointData.AddPilotPointFileObjects(FPilotPointFiles);

  finally
    FPNames.Free;
    FUsedParamList.Free;
  end;
end;

procedure TParameterZoneWriter.SetParamValuesFileName(var FileIndexUnit: Integer);
begin
  if FileIndexUnit = 0 then
  begin
    FParamValuesFileName := FParamValuesFileNameRoot + Extension;
  end
  else
  begin
    FParamValuesFileName := FParamValuesFileNameRoot + '_' + IntToStr(FileIndexUnit) + Extension;
  end;
  Inc(FileIndexUnit);
end;

procedure TParameterZoneWriter.WriteKrigingFactorsScript(
  const AFileName: string);
var
  ScriptFileName: string;
  PLPROC_Location: string;
  ModelDirectory: string;
begin
  ScriptFileName := FParamValuesFileNameRoot + StrKrigfactorsscript;
  ModelDirectory := ExtractFileDir(ScriptFileName);
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
  if Model.PestUsed then
  begin
    MoveAppToDirectory(PLPROC_Location, ModelDirectory);
    PLPROC_Location := ExtractFileName(PLPROC_Location);
  end;
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
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
      :
      begin
        WriteString('#Read MODFLOW-2005 grid information file');
        NewLine;
        GrbFileName := ChangeFileExt(AFileName, '');
        GrbFileName := ChangeFileExt(GrbFileName, '.gsf');
        GrbFileName := ExtractFileName(GrbFileName);
        WriteString(Format('%0:s = read_mf_grid_specs(file="%1:s")',
          [KDisName, GrbFileName]));
      end;
    msSutra22, msSutra30, msSutra40:
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
  FileIndexUnit: Integer;
//  OpenedFile: Boolean;
  MaxLayer: Integer;
  InnerLayerIndex: Integer;
begin
  SetLength(FZoneNumbers, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);
  SetLength(FValues, FDataArray.LayerCount, FDataArray.RowCount,
    FDataArray.ColumnCount);

  // Write values to be modified by PEST.
  FParamValuesFileNameRoot := ChangeFileExt(ChangeFileExt(AFileName, ''), '.'
    + FDataArray.Name);

  FileIndexUnit := 0;
  for LayerIndex := 0 to FDataArray.LayerCount - 1 do
  begin
    if (LayerIndex mod DataToWriteCount) = 0 then
    begin
      SetParamValuesFileName(FileIndexUnit);
      OpenFile(FParamValuesFileName);
      try
        MaxLayer := Min(FDataArray.LayerCount, LayerIndex+DataToWriteCount);
        for InnerLayerIndex := LayerIndex to MaxLayer - 1 do
        begin
          if LayerIndex = InnerLayerIndex then
          begin
            WriteString(' Index');
          end;

          SListName := Format('s_PIndex%0:d', [InnerLayerIndex + 1]);
          WriteString(' ' + SListName);

          PListName := Format('p_Value%0:d', [InnerLayerIndex + 1]);
          WriteString(' ' + PListName);
        end;
        NewLine;

        ID := 1;
        for RowIndex := 0 to FParamDataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to FParamDataArray.ColumnCount - 1 do
          begin
            WriteInteger(ID);
            for InnerLayerIndex := LayerIndex to MaxLayer - 1 do
            begin
              AName := LowerCase(FParamDataArray.StringData[
                InnerLayerIndex, RowIndex, ColIndex]);
              if (AName = '') then
              begin
                PIndex := -1;
              end
              else
              begin
                PIndex := FPNames.IndexOf(AName);
              end;

              if FDataArray.IsValue[InnerLayerIndex, RowIndex, ColIndex] then
              begin
                DataValue := FDataArray.RealData[InnerLayerIndex, RowIndex, ColIndex]
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
                  FValues[InnerLayerIndex, RowIndex, ColIndex] :=
                    DataValue / AParam.Value;
                end
                else
                begin
                  FValues[InnerLayerIndex, RowIndex, ColIndex] := DataValue;
                end;
              end
              else
              begin
                FValues[InnerLayerIndex, RowIndex, ColIndex] := DataValue
              end;
              WriteInteger(PIndex + 1);
              WriteFloat(FValues[InnerLayerIndex, RowIndex, ColIndex]);
              FZoneNumbers[InnerLayerIndex, RowIndex, ColIndex] := PIndex;
            end;
            Inc(ID);
            NewLine;
          end;
        end;
      finally
        CloseFile;
      end;
    end;
  end;
end;

procedure TParameterZoneWriter.WriteTemplateFile(const AFileName: string;
  ScriptChoice: TScriptChoice);
var
  ScriptFileName: string;
  ParameterIndex: Integer;
  ModelInputFileName: string;
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
  LayerIndex: Integer;
begin
  ScriptFileName := FParamValuesFileNameRoot + '.script';
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

    WriteString('#Read parameter values');
    NewLine;
    {$REGION 'Parameter values'}
    for ParameterIndex := 0 to FUsedParamList.Count - 1 do
    begin
      AParam := FUsedParamList.Objects[ParameterIndex]
        as TModflowSteadyParameter;
      if ScriptChoice = scWriteTemplate then
      begin
        WriteString(Format('%0:s = %1:s                        %0:s%1:s',
          [AParam.ParameterName, Model.PestProperties.TemplateCharacter]));
        NewLine;
      end;
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

    if FPilotPointsUsed then
    begin
      ReadPilotPoints;
    end;

    ReadDiscretization(ChangeFileExt(AFileName, ''));
    NewLine;

    ColIndex := 2;
    for LayerIndex := 0 to FDataArray.LayerCount - 1 do
    begin
      WriteString('# Layer');
      WriteInteger(LayerIndex + 1);
      NewLine;
      NewLine;
      WriteString('#Read data to modify');
      NewLine;
      {$REGION 'Data set values'}
//      ColIndex := 2;
      FileIndex := 0;
//      for LayerIndex := 0 to FDataArray.LayerCount - 1 do
      begin
        if (LayerIndex mod DataToWriteCount) = 0 then
        begin
          SetParamValuesFileName(FileIndex);
          ColIndex := 2;
        end;

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
        WriteString(Format('  file=''%0:s'')',
          [ExtractFileName(FParamValuesFileName)]));
        NewLine;
      end;
      NewLine;
      {$ENDREGION}


      WriteString('# Modfify data values');
      NewLine;
      {$REGION 'Modify data values'}

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
        if (UsedFileProperties = nil) then
        begin
          AParam.UsedDirectly := True;
          FTemplateNeeded := True;
          if FDataArray <> nil then
          begin
            FDataArray.TemplateNeeded := True;
          end;
          WriteString('    # Substituting parameter values in zones');
          NewLine;
          WriteString(Format(
            '    p_Value%0:d(select=(s_PIndex%0:d == %1:d)) = p_Value%0:d * %2:s',
            [LayerIndex + 1, ParamIndex+1, AParam.ParameterName]));
          NewLine;
        end;
      end;

      NewLine;
      {$ENDREGION}

      Root := ChangeFileExt(AFileName, '');
      Root := ChangeFileExt(Root, '');
      WriteString('#Write new data values');
      NewLine;
      {$REGION 'Write new data values'}
//      for LayerIndex := 0 to FDataArray.LayerCount - 1 do
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
      NewLine;

      WriteString('# Remove sLists and pLists');
      NewLine;
      WriteString(SListName);
      WriteString('.remove()');
      NewLine;
      WriteString(PListName);
      WriteString('.remove()');
      NewLine;
      WriteString(Format('temp%d.remove()', [LayerIndex+1]));
      NewLine;
      NewLine;
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

procedure TSutraNodDisWriter.SetCNodValuesFileName(var FileIndexUnit: Integer);
begin
  if FileIndexUnit = 0 then
  begin
    FInputFileName := FFileNameRoot + Extension;
  end
  else
  begin
    FInputFileName := FFileNameRoot + '.' + IntToStr(FileIndexUnit) + Extension;
  end;
  Inc(FileIndexUnit);
end;

procedure TSutraNodDisWriter.WriteFile(const AFileName: string);
var
  NodeIndex: Integer;
  SutraMesh2D: TSutraMesh2D;
  ANode: TSutraNode2D;
  InnerLayerIndex: Integer;
  SutraMesh3D: TSutraMesh3D;
  ANode3D: TSutraNode3D;
  FileIndexUnit: Integer;
  LayerIndex: Integer;
  MaxLayer: Integer;
begin
  FileIndexUnit := 0;
  SutraMesh3D := Model.SutraMesh;
  SutraMesh2D := SutraMesh3D.Mesh2D;

  FFileNameRoot := ChangeFileExt(AFileName, '');
  FInputFileName := FileName(AFileName);
  for LayerIndex := 0 to SutraMesh3D.LayerCount do
  begin
    if (LayerIndex mod DataToWriteCount) = 0 then
    begin
      SetCNodValuesFileName(FileIndexUnit);
      MaxLayer := Min(LayerIndex+DataToWriteCount-1, SutraMesh3D.LayerCount);
      OpenFile(FInputFileName);
      try
        WriteString(' Index X Y Node_Number2D');
        if SutraMesh3D.MeshType = mt3D then
        begin
          for InnerLayerIndex := LayerIndex to MaxLayer do
          begin
            WriteString(Format(' Z_%0:d NodeNumber3D_%0:d Active_%0:d', [InnerLayerIndex+1]));
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
            for InnerLayerIndex := LayerIndex to MaxLayer do
            begin
              ANode3D := SutraMesh3D.NodeArray[InnerLayerIndex,ANode.Number];
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
  InnerLayerIndex: Integer;
  SutraMesh3D: TSutraMesh3D;
  FileIndex: Integer;
  LayerIndex: Integer;
  MaxLayer: Integer;
begin
  FileIndex := 0;
  SutraMesh3D := Model.SutraMesh;
  for LayerIndex := 0 to SutraMesh3D.LayerCount-1 do
  begin
    if (LayerIndex mod DataToWriteCount) = 0 then
    begin
      GetInputFileName(FileIndex, AFileName);
      OpenFile(FInputFileName);
      try
        MaxLayer := Min(LayerIndex+DataToWriteCount, SutraMesh3D.LayerCount);
        SutraMesh2D := SutraMesh3D.Mesh2D;
        WriteString(' Index X Y Element_Number2D');
        if SutraMesh3D.MeshType = mt3D then
        begin
          for InnerLayerIndex := LayerIndex to MaxLayer-1 do
          begin
            WriteString(Format(' Z_%0:d ElementNumber3D_%0:d Active_%0:d', [InnerLayerIndex+1]));
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
            for InnerLayerIndex := LayerIndex to MaxLayer-1 do
            begin
              AnElement3D := SutraMesh3D.ElementArray[InnerLayerIndex, ElementIndex];
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
  end;
end;

procedure TSutraEleDisWriter.GetInputFileName(var FileIndex: Integer; const AFileName: string);
begin
  if FileIndex = 0 then
  begin
    FInputFileName := FileName(AFileName);
  end
  else
  begin
    FInputFileName := ChangeFileExt(AFileName, '') + '_' + IntToStr(FileIndex) + Extension;
  end;
  Inc(FileIndex);
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
  InnerLayerIndex: Integer;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  ANode3D: TSutraNode3D;
  DataFileIndex: Integer;
  MaxLayers: Int64;
  LayerIndex: Integer;
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

    DataFileIndex := 0;
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      if LayerIndex mod DataToWriteCount = 0 then
      begin
        if DataFileIndex > 0 then
        begin
          FFileName := ChangeFileExt(AFileName, '.' + DataArray.Name) + '_' + IntToStr(DataFileIndex);
        end;
        MaxLayers := Min(LayerIndex+DataToWriteCount, DataArray.LayerCount);
        Inc(DataFileIndex);
        OpenFile(FFileName);
        try
          WriteString('NN2D ');
          for InnerLayerIndex := LayerIndex to MaxLayers - 1 do
          begin
            if FMesh.MeshType = mt3D then
            begin
              WriteString(Format('NN3D%0:d Layer%0:d ', [InnerLayerIndex+1]));
            end;
            WriteString(Format('%0:s%1:d ', [DataArray.Name, InnerLayerIndex+1]));
            if DataArray.DataType = rdtDouble then
            begin
              WriteString(Format('%0:sParameter%1:d ', [DataArray.Name, InnerLayerIndex+1]))
            end;
          end;
          NewLine;

          for  NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
          begin
            ANode2D := FMesh.Mesh2D.Nodes[NodeIndex];
            WriteInteger(ANode2D.Number+1);
            for InnerLayerIndex := LayerIndex to MaxLayers - 1 do
            begin
              if FMesh.MeshType = mt3D then
              begin
                ANode3D := FMesh.NodeArray[InnerLayerIndex,NodeIndex];
                WriteInteger(ANode3D.Number + 1);
                WriteInteger(InnerLayerIndex + 1);
              end;
              if ParamArray <> nil then
              begin
                PIndex := ParameterNames.IndexOf(LowerCase(
                  ParamArray.StringData[InnerLayerIndex, 0, NodeIndex]));
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
                      WriteFloat(DataArray.RealData[InnerLayerIndex, 0, NodeIndex]);
                    end
                    else
                    begin
                      WriteFloat(DataArray.RealData[InnerLayerIndex, 0, NodeIndex]
                        /AParam.Value);
                    end;
                  end;
                rdtInteger:
                  begin
                    WriteInteger(DataArray.IntegerData[InnerLayerIndex, 0, NodeIndex]);
                  end;
                rdtBoolean:
                  begin
                    WriteInteger(Ord(DataArray.BooleanData[InnerLayerIndex, 0, NodeIndex]));
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
      end;
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
  FDataSetNames := TStringList.Create;
  FDataSetAbbreviations := TStringList.Create;
  FSutra4NodePilotPointFiles := TObjectList<TPilotPointFiles>.Create;
  FSutra4DataSetRoots := TStringList.Create;
  if Model.Sutra4Used(nil) then
  begin
    FDataSetNames.Add(KSolidMatrixComp);
    FDataSetAbbreviations.Add('COMPMA');
    FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);

    if Model.Sutra4EnergyUsed(nil) then
    begin
      FDataSetNames.Add(KSolidGrainSpecificHeat);
      FDataSetAbbreviations.Add('CS');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);
    end
    else
    begin
      // An empty data set name means that an arbitrary value must
      // be exported.
      FDataSetNames.Add('');
      FDataSetAbbreviations.Add('CS');
      FSutra4NodePilotPointFiles.Add(nil);
    end;

    if Model.Sutra4EnergyOrSorptionUsed(nil) then
    begin
      FDataSetNames.Add(KSolidGrainDensity);
      FDataSetAbbreviations.Add('RHOS');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);
    end
    else
    begin
      // An empty data set name means that an arbitrary value must
      // be exported.
      FDataSetNames.Add('');
      FDataSetAbbreviations.Add('RHOS');
      FSutra4NodePilotPointFiles.Add(nil);
    end;

    if Model.Sutra4ProductionUsed(nil) then
    begin
    end;
    if Model.Sutra4ProductionUsed(nil) then
    begin
      FDataSetNames.Add(KZeroOrderProductionRateInLiquid);
      FDataSetAbbreviations.Add('PRODL0');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);

      FDataSetNames.Add(KZeroOrderProductionRateInImmobile);
      FDataSetAbbreviations.Add('PRODS0');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);
    end;

    if Model.Sutra4SoluteUsed(nil) then
    begin
      FDataSetNames.Add(KFirstOrderProductionRateInLiquid);
      FDataSetAbbreviations.Add('PRODL1');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);

      FDataSetNames.Add(KFirstOrderProductionRateInImmobile);
      FDataSetAbbreviations.Add('PRODS1');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);
    end;

    if Model.Sutra4FreezingUsed(nil) then
    begin
    end;
    if Model.Sutra4FreezingUsed(nil) then
    begin
      FDataSetNames.Add(KZeroOrderProductionRateInIce);
      FDataSetAbbreviations.Add('PRODI');
      FSutra4NodePilotPointFiles.Add(TPilotPointFiles.Create);
    end;
  end;

  FUsedParamList := TStringList.Create;
  FUsedParamList.Duplicates := dupIgnore;
  FUsedParamList.Sorted := True;
end;

destructor TSutraData14BScriptWriter.Destroy;
begin
  FSutra4DataSetRoots.Free;
  FDataSetAbbreviations.Free;
  FDataSetNames.Free;
  FSutra4NodePilotPointFiles.Free;
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
  Sutra4Index: Integer;
  Sutra4DataSetName: string;
  Sutra4DataSet: TDataArray;
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

  for Sutra4Index := 0 to FDataSetNames.Count - 1 do
  begin
    Sutra4DataSetName := FDataSetNames[Sutra4Index];
    if Sutra4DataSetName = '' then
    begin
      Continue;
    end;
    Sutra4DataSet := Model.DataArrayManager.GetDataSetByName(Sutra4DataSetName);
    if Sutra4DataSet.PestParametersUsed then
    begin
      ParamArray := Model.DataArrayManager.GetDataSetByName
        (Sutra4DataSet.ParamDataSetName);

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
  Sutra4Index: Integer;
  PilotPointFiles: TPilotPointFiles;
  Abbreviation: string;
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
      for Sutra4Index := 0 to FSutra4NodePilotPointFiles.Count - 1 do
      begin
        PilotPointFiles := FSutra4NodePilotPointFiles[Sutra4Index];
        if PilotPointFiles <> nil then
        begin
          Abbreviation := FDataSetAbbreviations[Sutra4Index];
          HandlePilotPointList(PilotPointFiles, Abbreviation);
        end;
      end;
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
  Sutra4Index: Integer;
  Sutra4DataSetName: string;
  SutraDataArray: TDataArray;
  Sutra4FileRoot: string;
//  Abbreviation: string;
//  PilotPointFiles: TPilotPointFiles;
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
//            WriteString(Format('  source_clist=PilotPoints%1:d, &', [DataID, PPIndex+1]));
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
//    HandleDataArray('POR', Porosity, FPorosityPilotPointFiles, FPorKrigingFactorsFileRoot);
  end;

  if FThicknessPilotPointFiles.Count > 0 then
  begin
    Thickness := Model.DataArrayManager.GetDataSetByName(KNodalThickness);
    FThicknessKrigingFactorsFileRoot := FRoot +  '.' + Thickness.Name
    + '.Factors';
//    HandleDataArray('Z', Thickness, FThicknessPilotPointFiles, FThicknessKrigingFactorsFileRoot);
  end;

  for Sutra4Index := 0 to FDataSetNames.Count - 1 do
  begin
    Sutra4DataSetName := FDataSetNames[Sutra4Index];
    if Sutra4DataSetName = '' then
    begin
      FSutra4DataSetRoots.Add('');
      Continue;
    end;
    SutraDataArray := Model.DataArrayManager.GetDataSetByName(Sutra4DataSetName);
    Sutra4FileRoot := FRoot +  '.' + SutraDataArray.Name + '.Factors';
    FSutra4DataSetRoots.Add(Sutra4FileRoot);
//    Abbreviation := FDataSetAbbreviations[Sutra4Index];
//    PilotPointFiles :=  FSutra4PilotPointFiles[Sutra4Index];
//    HandleDataArray(Abbreviation, SutraDataArray, PilotPointFiles, Sutra4FileRoot);
  end;

  NewLine;
end;

procedure TSutraData14BScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  LayerCount: Integer;
  LayerIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
  FileIndex: Integer;
  FileProperties: TPilotPointFileObject;
  UsedFileProperties: TPilotPointFileObject;
  PListName: string;
  DataSetName: string;
  ID: string;
  Sutra4Index: Integer;
  Sutra4PilotPointFiles: TPilotPointFiles;
  Sutra4Root: string;
  PorosityColIndex: Integer;
  DataSetColIndex: Integer;
  UnsatColIndex: Integer;
  LayerIndex2D: Integer;
  FileNameAddition: string;
  FileIndexNumber: Integer;
  PriorDataSetColIndex: Integer;
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



    if FMesh.MeshType = mt3D then
    begin
      LayerCount := FMesh.LayerCount + 1;
    end
    else
    begin
      LayerCount := 1;
    end;

    {$REGION 'Node discretization'}
    ReadNodeDiscretization(True, 0);
    {$ENDREGION}

    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      ['cl_Discretization']));
    NewLine;


    ReadPilotPoints;

    WriteString('# Read data to modify');
    NewLine;
//    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
//    NewLine;
//    WriteString(Format('  file=''%s.Nodal_Porosity'')', [FRoot]));
//    NewLine;

    FileNameAddition := '';
    FileIndexNumber := 0;
    PorosityColIndex := 2;
    UnsatColIndex := 2;
    PriorDataSetColIndex := 2;
    DataSetColIndex := 2;
    for LayerIndex := 1 to LayerCount do
    begin
      if (LayerIndex > 1) and (((LayerIndex -1) mod DataToWriteCount) = 0) then
      begin
        Inc(FileIndexNumber);
        FileNameAddition := '_' + IntToStr(FileIndexNumber);
        PorosityColIndex := 2;
        UnsatColIndex := 2;
        PriorDataSetColIndex := 2;
      end;
      WriteString('# Layer');
      WriteInteger(LayerIndex);
      NewLine;
      NewLine;

      ReadNodeDiscretization(True, LayerIndex);

      NewLine;
      {$REGION 'Porosity'}
      WriteString('# Read porosity');
      NewLine;
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      if FMesh.MeshType = mt3D then
      begin
        Inc(PorosityColIndex);

        WriteString(Format('  slist=s_Layer%0:d;column=%1:d, &', [LayerIndex, PorosityColIndex]));
        NewLine;
        Inc(PorosityColIndex);
      end;

      WriteString(Format('  plist=p_Porosity%0:d;column=%1:d, &', [LayerIndex, PorosityColIndex]));
      NewLine;
      Inc(PorosityColIndex);

      WriteString(Format('  slist=s_PorPar%0:d;column=%1:d, &', [LayerIndex, PorosityColIndex]));
      NewLine;
      Inc(PorosityColIndex);
      WriteString(Format('  file=''%0:s.Nodal_Porosity%1:s'')', [FRoot, FileNameAddition]));
      NewLine;
      {$ENDREGION}
//    end;
      NewLine;

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
        WriteString(Format('  file=''%0:s.Nodal_Thickness%1:s'')', [FRoot, FileNameAddition]));
        NewLine;
        NewLine;
      end;
      {$ENDREGION}

    {$REGION 'Unsaturated zone'}
    WriteString('# Read Unsaturated Zone');
    NewLine;
//    for LayerIndex := 1 to LayerCount do
//    begin
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;

      if FMesh.MeshType = mt3D then
      begin
        Inc(UnsatColIndex,2);
      end;

      WriteString(Format('  slist=s_Unsat_Region%0:d;column=%1:d, &', [LayerIndex, UnsatColIndex]));
      NewLine;
      Inc(UnsatColIndex);

      WriteString(Format('  file=''%0:s.%1:s%2:s'')', [FRoot,KUnsatRegionNodes, FileNameAddition]));
      NewLine;
//    end;
      NewLine;
      {$ENDREGION}

      {$REGION 'SUTRA 4 data sets'}
      if Model.Sutra4Used(nil) then
      begin
        for Sutra4Index := 0 to FDataSetNames.Count - 1 do
        begin
          DataSetColIndex := PriorDataSetColIndex;
          DataSetName := FDataSetNames[Sutra4Index];
          ID := FDataSetAbbreviations[Sutra4Index];
          if DataSetName = '' then
          begin
            WriteString('# Assign arbitrary value to ');
            WriteString(ID);
            NewLine;

//            for LayerIndex := 1 to LayerCount do
            begin
              WriteString(Format('p_%0:s%1:d=new_plist(reference_clist=''cl_Discretization'',value=0.0', [ID,LayerIndex]));
              WriteString(Format('s_%0:s%1:d=new_slist(reference_clist=''cl_Discretization'',value=-1', [ID,LayerIndex]));
              NewLine;
            end;
          end
          else
          begin
            WriteString('# Read ');
            WriteString(ID);
            NewLine;

            WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
            NewLine;

            WriteString(Format('  file=''%0:s.%1:s%2:s'')', [FRoot, DataSetName, FileNameAddition]));
            NewLine;

  //          for LayerIndex := 1 to LayerCount do
            begin
              // PLPROC has a limit of 5 s_lists per call of read_list_file.
              // To avoid reaching that limit, a separate call is used for each layer.
              WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
              NewLine;
              if FMesh.MeshType = mt3D then
              begin
                Inc(DataSetColIndex);
                Inc(DataSetColIndex);
              end;

              WriteString(Format('  plist=p_%0:s%1:d;column=%2:d, &', [ID, LayerIndex, DataSetColIndex]));
              NewLine;
              Inc(DataSetColIndex);

              WriteString(Format('  slist=s_%0:sPar%1:d;column=%2:d, &', [ID, LayerIndex, DataSetColIndex]));
              NewLine;
              Inc(DataSetColIndex);
              WriteString(Format('  file=''%0:s.%1:s%2:s'')', [FRoot,DataSetName, FileNameAddition]));
              NewLine;
            end;
            NewLine;
          end;
        end;
        PriorDataSetColIndex := DataSetColIndex;
      end;
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
        if FMesh.MeshType <> mt3D then
        begin
          if AParam.UsePilotPoints then
          begin
            LayerIndex2D := 0;
            WriteString('    # Substituting interpolated values');
            NewLine;

            UsedFileProperties := nil;
  //          PIndex := 0;
            for FileIndex := 0 to FThicknessPilotPointFiles.Count - 1 do
            begin
              FileProperties := FThicknessPilotPointFiles[FileIndex];
              if (FileProperties.Parameter = AParam)
                and (FileProperties.Layer = LayerIndex2D) then
              begin
                UsedFileProperties := FileProperties;
  //              PIndex := FileIndex;
                break;
              end;
            end;

            if UsedFileProperties <> nil then
            begin
              PListName := Format('%0:s_%1:s_%2:d',
                ['Z',AParam.ParameterName, LayerIndex2D+1]);
              WriteString('    # Get interpolated values');
              NewLine;
            end
            else
            begin
              WriteString(Format(
                '    # no interpolated values defined for parameter %1:s in layer %0:d',
                [LayerIndex2D+1, AParam.ParameterName]));
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

        {$REGION 'Porosity'}
//        for LayerIndex+1 := 0 to LayerCount-1 do
        begin
          if AParam.UsePilotPoints then
          begin
            WriteString('    # Substituting interpolated values');
            NewLine;

            UsedFileProperties := nil;
            for FileIndex := 0 to FPorosityPilotPointFiles.Count - 1 do
            begin
              FileProperties := FPorosityPilotPointFiles[FileIndex];
              if (FileProperties.Parameter = AParam)
                and (FileProperties.Layer = LayerIndex) then
              begin
                UsedFileProperties := FileProperties;
                break;
              end;
            end;

            if UsedFileProperties <> nil then
            begin
              PListName := Format('%0:s_%1:s_%2:d',
                ['POR',AParam.ParameterName, LayerIndex]);
              WriteString('    # Get interpolated values');
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
            WriteString(Format('p_Porosity%0:d(select=(s_PorPar%0:d == %2:d)) = p_Porosity%0:d * %1:s',
              [LayerIndex, AParam.ParameterName, ParameterIndex+1]));
            NewLine;
          end;
        end;
        NewLine;
        {$ENDREGION}

        {$REGION 'Sutra 4 data sets'}
        for Sutra4Index := 0 to FDataSetNames.Count - 1 do
        begin
          Sutra4PilotPointFiles := FSutra4NodePilotPointFiles[Sutra4Index];
          if (Sutra4PilotPointFiles <> nil) then
          begin
            ID := FDataSetAbbreviations[Sutra4Index];
//            for LayerIndex+1 := 0 to LayerCount-1 do
            begin
              if AParam.UsePilotPoints and (Sutra4PilotPointFiles.Count > 0) then
              begin
                Sutra4Root := FSutra4DataSetRoots[Sutra4Index];

                WriteString('    # Substituting interpolated values');
                NewLine;

                UsedFileProperties := nil;
                for FileIndex := 0 to Sutra4PilotPointFiles.Count - 1 do
                begin
                  FileProperties := Sutra4PilotPointFiles[FileIndex];
                  if (FileProperties.Parameter = AParam)
                    and (FileProperties.Layer = LayerIndex) then
                  begin
                    UsedFileProperties := FileProperties;
                    break;
                  end;
                end;

                if UsedFileProperties <> nil then
                begin
                  PListName := Format('%0:s_%1:s_%2:d',
                    [ID,AParam.ParameterName, LayerIndex]);
                  WriteString('    # Get interpolated values');
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
                WriteString(Format('p_%0:s%1:d(select=(s_%0:sPar%1:d == %3:d)) = p_%0:s%1:d * %2:s',
                  [ID, LayerIndex, AParam.ParameterName, ParameterIndex+1]));
                NewLine;
              end;
            end;
            NewLine;
          end;
        end;
        {$ENDREGION}
      end;

      // remove unneeded sLists.
      WriteString('# removing unneeded slists.');
      NewLine;
      {$REGION 'Porosity'}
      WriteString(Format('s_PorPar%0:d.remove()',
        [LayerIndex]));
      NewLine;
      {$ENDREGION}

      {$REGION 'Sutra 4 data sets'}
      for Sutra4Index := 0 to FDataSetNames.Count - 1 do
      begin
        ID := FDataSetAbbreviations[Sutra4Index];
        WriteString(Format('s_%0:sPar%1:d.remove()',
          [ID, LayerIndex]));
        NewLine;
      end;
      {$ENDREGION}
      NewLine;

//    end;
      {$ENDREGION}

      WriteString('# Write new data values');
      NewLine;
      {$REGION 'Write values'}
      if FMesh.MeshType = mt3D then
      begin
  //      for LayerIndex := 1 to LayerCount do
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
          WriteString(Format('  plist=p_Porosity%0:d', [LayerIndex]));
          if FDataSetAbbreviations.Count > 0 then
          begin
            WriteString(', &');
          end
          else
          begin
            WriteString(')');
          end;
          NewLine;
          for Sutra4Index := 0 to FDataSetAbbreviations.Count - 1 do
          begin
            ID := FDataSetAbbreviations[Sutra4Index];
            WriteString(Format('  plist=p_%0:s%1:d', [ID,LayerIndex]));
            if Sutra4Index < FDataSetAbbreviations.Count - 1 then
            begin
              WriteString(', &');
            end
            else
            begin
              WriteString(')');
            end;
            NewLine;
          end;
        end;

        NewLine;
        WriteString('# remove unneeded slists and plists.');
        NewLine;
        begin
          WriteString(Format('s_Active_%d.remove()',
            [LayerIndex]));
          NewLine;
          WriteString(Format('s_NN3D%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('s_Unsat_Region%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('p_z%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('p_Porosity%0:d.remove()', [LayerIndex]));
          NewLine;
          for Sutra4Index := 0 to FDataSetAbbreviations.Count - 1 do
          begin
            ID := FDataSetAbbreviations[Sutra4Index];
            WriteString(Format('p_%0:s%1:d.remove()', [ID,LayerIndex]));
            NewLine;
          end;
        end
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
        WriteString('  plist=p_Porosity1');
        if FDataSetAbbreviations.Count > 0 then
        begin
          WriteString(', &');
        end
        else
        begin
          WriteString(')');
        end;
        NewLine;
        for Sutra4Index := 0 to FDataSetAbbreviations.Count - 1 do
        begin
          ID := FDataSetAbbreviations[Sutra4Index];
          WriteString(Format('  plist=p_%0:s1',[ID]));
          if Sutra4Index < FDataSetAbbreviations.Count - 1 then
          begin
            WriteString(', &');
          end
          else
          begin
            WriteString(')');
          end;
          NewLine;
        end;
      end;
      {$ENDREGION}
      NewLine;


    end;
  finally
    CloseFile;
  end;
end;

procedure TSutraData14BScriptWriter.WriteFiles(var AFileName: string);
var
  PLPROC_Location: string;
  ModelDirectory: string;
  Sutra4Index: Integer;
  PilotPointFiles: TPilotPointFiles;
begin
  FFileName := FileName(AFileName);
  ModelDirectory := ExtractFileDir(FFileName);
  Model.SutraPestScripts.Add(FFileName);
  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  if Model.PestUsed then
  begin
    MoveAppToDirectory(PLPROC_Location, ModelDirectory);
    PLPROC_Location := ExtractFileName(PLPROC_Location);
  end;
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
  for Sutra4Index := 0 to FSutra4NodePilotPointFiles.Count - 1 do
  begin
    PilotPointFiles := FSutra4NodePilotPointFiles[Sutra4Index];
    if PilotPointFiles <> nil then
    begin
      Model.PilotPointData.AddPilotPointFileObjects(PilotPointFiles);
    end;
  end;

end;

procedure TSutraData14BScriptWriter.ReadNodeDiscretization(ReadSLists: Boolean;
  LayerToRead: Integer = -1);
var
  LayerIndex: Integer;
  ColIndex: Integer;
  LayerCount: Integer;
  FileIndexUnit: Integer;
  NameAddition: String;
begin
  if FMesh.MeshType = mt3D then
  begin
    LayerCount := FMesh.LayerCount + 1;
  end
  else
  begin
    LayerCount := 1;
  end;

  FileIndexUnit := 0;
  NameAddition := '';

  if LayerToRead <= 0 then
  begin
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
    WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_nod'')', [FRoot, NameAddition]));
    NewLine;
  end;
  if FMesh.MeshType = mt3D then
  begin
    ColIndex := 5;
    for LayerIndex := 1 to LayerCount do
    begin
      if (LayerIndex > 1) and (((LayerIndex-1) mod DataToWriteCount) = 0) then
      begin
        Inc(FileIndexUnit);
        NameAddition := '.' + IntToStr(FileIndexUnit);
        ColIndex := 5;
      end;
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      if (LayerIndex = LayerToRead) or (LayerToRead = -1) then
      begin
        WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
        NewLine;
        // Z coordinate
        WriteString(Format('  plist=p_z%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
        NewLine;
      end;
      Inc(ColIndex);
      if (LayerIndex = LayerToRead) or (LayerToRead = -1) then
      begin
        // 3D node number
        if ReadSLists then
        begin
          WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
          NewLine;
        end;
      end;
      Inc(ColIndex);
      if (LayerIndex = LayerToRead) or (LayerToRead = -1) then
      begin
        // Active or not
        if ReadSLists then
        begin
          WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
          NewLine;
        end;
      end;
      Inc(ColIndex);
      if (LayerIndex = LayerToRead) or (LayerToRead = -1) then
      begin
        WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_nod'')', [FRoot, NameAddition]));
        NewLine;
      end;
    end;
  end;
  NewLine;
end;

procedure TSutraData14BScriptWriter.WriteKrigingFactors;
var
  ScriptFileName: string;
  PLPROC_Location: string;
  ModelDirectory: string;
  function Sutra4PilotPointsUsed: Boolean;
  var
    Sutra4Index: Integer;
    PilotPointFiles: TPilotPointFiles;
  begin
    result := False;
    for Sutra4Index := 0 to FSutra4NodePilotPointFiles.Count - 1 do
    begin
      PilotPointFiles := FSutra4NodePilotPointFiles[Sutra4Index];
      if (PilotPointFiles <> nil) and (PilotPointFiles.Count > 0) then
      begin
        result := True;
        Exit;
      end;
    end;
  end;
begin
  if (FPorosityPilotPointFiles.Count > 0)
    or (FThicknessPilotPointFiles.Count > 0) or Sutra4PilotPointsUsed then
  begin
    ScriptFileName := ChangeFileExt(FFileName, StrKrigfactorsscript);
    OpenFile(ScriptFileName);
    try
      SListCount := 0;
      WriteString('#Script for PLPROC for saving kriging factors');
      NewLine;
      NewLine;
      ReadPilotPoints;
      ReadNodeDiscretization(False);
      SaveKrigingFactors;
      PLPROC_Location := GetPLPROC_Location(FFileName, Model);
      ModelDirectory := ExtractFileDir(FFileName);
      if Model.PestUsed then
      begin
        MoveAppToDirectory(PLPROC_Location, ModelDirectory);
        PLPROC_Location := ExtractFileName(PLPROC_Location);
      end;
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
  Sutra4Index: Integer;
  Sutra4DataSetName: string;
  Sutra4DataSet: TDataArray;
  Sutra4PilotPointFiles: TPilotPointFiles;
  Abbreviation: string;
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

  for Sutra4Index := 0 to FDataSetNames.Count - 1 do
  begin
    Sutra4DataSetName := FDataSetNames[Sutra4Index];
    if Sutra4DataSetName = '' then
    begin
      Continue;
    end;
    Sutra4DataSet := Model.DataArrayManager.GetDataSetByName(Sutra4DataSetName);
    if Sutra4DataSet.PestParametersUsed then
    begin
      PilotPointWriter := TPilotPointWriter.Create(Model, etExport);
      try
        Sutra4PilotPointFiles := FSutra4NodePilotPointFiles[Sutra4Index];
        Abbreviation := FDataSetAbbreviations[Sutra4Index];
        PilotPointWriter.WriteFile(FFileName, Sutra4DataSet, Sutra4PilotPointFiles,
          Abbreviation, Abbreviation);
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
    tcSolute, tcEnergy, tcFreezing:
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
      tcSolute, tcEnergy, tcFreezing:
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
    tcSolute, tcEnergy, tcFreezing:
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

{$REGION 'SIGMAS'}
  if Model.Sutra4EnergyUsed(nil) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KScaledSolidGrainThermalConductivity);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'SIGMAS', 'SGS');
{$ENDREGION}

{$REGION 'SIGMAA'}
  if Model.Sutra4EnergyUsed(nil) then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(KScaledEffectiveAirThermalConductivity);
  end
  else
  begin
    DataArray := nil;
  end;
  CreateDataRecord(DataArray, 'SIGMAA', 'SGA');
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

procedure TSutraData15BScriptWriter.WriteAFile(ScriptChoice: TScriptChoice);
var
  Mesh: TSutraMesh3D;
  LayerCount: Integer;
  LayerIndex: Integer;
  Options: TSutraOptions;
  DataArray: TDataArray;
  UsedDataRoots: TStringList;
  DataRoot: string;
  RootIndex: Integer;
  PestAnisotropyOptions: TSutraPestAnisotropyOptions;
  ReferenceDataArray: TDataArray;
  PmaxColIndex: Integer;
  PmidColIndex: Integer;
  PMinColIndex: Integer;
  Angle1ColIndex: Integer;
  Angle2ColIndex: Integer;
  Angle3ColIndex: Integer;
  AlmaxColIndex: Integer;
  AlMidColIndex: Integer;
  AlminColIndex: Integer;
  AtmaxColIndex: Integer;
  AtmidColIndex: Integer;
  AtMinColIndex: Integer;
  SigmasColIndex: Integer;
  SigmaaColIndex: Integer;
  UnsatColIndex: Integer;
  DisFileIndex: Integer;
  DisColIndex: Integer;
  FileNameAddition: string;
  procedure ReadData(DataArray: TDataArray; const DataRoot: string;
    AnisotropyUsed: Boolean; Var ColIndex: integer);
  var
    AnisotropyWriter: TAnisotropyWriter;
    procedure ImportArrayFile;
    var
      ArrayFileName: string;
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
    end;
  begin
    if DataArray <> nil then
    begin
      UsedDataRoots.Add(DataRoot);
      WriteString(Format('# Read %s', [DataArray.Name]));
      NewLine;
      ColIndex := 1;

      Inc(ColIndex);

      if AnisotropyUsed then
      begin
        ImportArrayFile;

//        if ScriptChoice = scWriteTemplate then
        begin
          AnisotropyWriter := TAnisotropyWriter.Create(Model, etExport);
          try
            AnisotropyWriter.WriteFile(FRoot, DataArray, ReferenceDataArray);
          finally
            AnisotropyWriter.Free;
          end;
        end;
      end
      else if not DataArray.PestParametersUsed then
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

          Inc(ColIndex);

          WriteString(Format('  file=''%0:s.%1:s'')',
            [FRoot, DataArray.Name]));
          NewLine;
      end
      else
      begin
        ImportArrayFile;
      end;
    end;
  end;
begin
  if ScriptChoice = scWriteTemplate then
  begin
    FFileName := FFileName + '.tpl';
  end;

  Mesh := Model.SutraMesh;
  Options := Model.SutraOptions;
  PestAnisotropyOptions := Options.PestAnisotropyOptions;

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
//    ReadDiscretization;

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
    NewLine;

    DisFileIndex := 0;
    DisColIndex := 5;
    PmaxColIndex := 1;
    PmidColIndex := 1;
    PMinColIndex := 1;
    Angle1ColIndex := 1;
    Angle2ColIndex := 1;
    Angle3ColIndex := 1;
    AlmaxColIndex := 1;
    AlMidColIndex := 1;
    AlminColIndex := 1;
    AtmaxColIndex := 1;
    AtmidColIndex := 1;
    AtMinColIndex := 1;
    SigmasColIndex := 1;
    SigmaaColIndex := 1;
    UnsatColIndex := 2;

    FileNameAddition := '';
    for LayerIndex := 1 to LayerCount do
    begin
      if Mesh.MeshType = mt3D then
      begin
        WriteString('# Layer ');
        WriteInteger(LayerIndex);
        NewLine;
        WriteString('#Read SUTRA element information file');
        NewLine;
        if ((LayerIndex-1) mod DataToWriteCount) = 0 then
        begin
          if DisFileIndex = 0 then
          begin
            FileNameAddition := '';
          end
          else
          begin
            FileNameAddition := '_' + IntToStr(DisFileIndex);
          end;
          Inc(DisFileIndex);
          DisColIndex := 5;
          PmaxColIndex := 1;
          PmidColIndex := 1;
          PMinColIndex := 1;
          Angle1ColIndex := 1;
          Angle2ColIndex := 1;
          Angle3ColIndex := 1;
          AlmaxColIndex := 1;
          AlMidColIndex := 1;
          AlminColIndex := 1;
          AtmaxColIndex := 1;
          AtmidColIndex := 1;
          AtMinColIndex := 1;
          SigmasColIndex := 1;
          SigmaaColIndex := 1;
          UnsatColIndex := 2;
        end;
        WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
        NewLine;
        WriteString(Format('  plist=p_z%0:d;column=%1:d, &',
          [LayerIndex, DisColIndex]));
        NewLine;
        Inc(DisColIndex);
        WriteString(Format('  slist=s_EN3D%0:d;column=%1:d, &',
          [LayerIndex, DisColIndex]));
        NewLine;
        Inc(DisColIndex);
        WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &',
          [LayerIndex, DisColIndex]));
        NewLine;
        Inc(DisColIndex);
          WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_ele'')', [FRoot, FileNameAddition]));
        NewLine;
      end;

    NewLine;
    {$ENDREGION}
      UsedDataRoots.Clear;
      WriteString('#Read data to modify');
      NewLine;
      {$REGION 'Unsaturated zone'}
      WriteString('# Read Unsaturated Zone');
      NewLine;
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;

      if Mesh.MeshType = mt3D then
      begin
        Inc(UnsatColIndex,2);
      end;

      WriteString(Format('  slist=s_LREG%0:d;column=%1:d, &', [LayerIndex, UnsatColIndex]));
      NewLine;
      Inc(UnsatColIndex);

      WriteString(Format('  file=''%0:s.%1:s'')', [FRoot, KUnsatRegionElements]));
      NewLine;
      {$ENDREGION}

      {$REGION 'PMAX'}
      DataArray := nil;
      case Options.TransportChoice of
        tcSolute, tcEnergy, tcFreezing:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumPermeability);
        tcSoluteHead:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMaximumK);
        else Assert(False);
      end;
      ReadData(DataArray,  'PMAX', False, PmaxColIndex);
      ReferenceDataArray := DataArray;
      {$ENDREGION}

      {$REGION 'PMID'}
      DataArray := nil;
      if Mesh.MeshType = mt3D then
      begin
        case Options.TransportChoice of
          tcSolute, tcEnergy, tcFreezing:
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
      ReadData(DataArray, 'PMID',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UsePmaxPmidAnisotropy, PmidColIndex);
      {$ENDREGION}

      {$REGION 'PMIN'}
      DataArray := nil;
      case Options.TransportChoice of
        tcSolute, tcEnergy, tcFreezing:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumPermeability);
        tcSoluteHead:
          DataArray := Model.DataArrayManager.GetDataSetByName(KMinimumK);
        else Assert(False);
      end;
      ReadData(DataArray, 'PMIN',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UsePmaxPminAnisotropy, PMinColIndex);
      {$ENDREGION}

      {$REGION 'ANGLE1'}
      DataArray := Model.DataArrayManager.GetDataSetByName(KHorizontalAngle);
      ReadData(DataArray, 'ANGLE1', False, Angle1ColIndex);
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
      ReadData(DataArray, 'ANGLE2', False, Angle2ColIndex);
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
      ReadData(DataArray, 'ANGLE3', False, Angle3ColIndex);
      {$ENDREGION}

      {$REGION 'ALMAX'}
      DataArray := Model.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
      ReadData(DataArray, 'ALMAX', False, AlmaxColIndex);
      ReferenceDataArray := DataArray;
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
      ReadData(DataArray, 'ALMID',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UseAlmaxAlmidAnisotropy, AlMidColIndex);
      {$ENDREGION}

      {$REGION 'ALMIN'}
      DataArray := Model.DataArrayManager.GetDataSetByName(KMinLongitudinalDisp);
      ReadData(DataArray, 'ALMIN',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UseAlmaxAlminAnisotropy, AlminColIndex);
      {$ENDREGION}

      {$REGION 'ATMAX'}
      DataArray := Model.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
      ReadData(DataArray, 'ATMAX', False, AtmaxColIndex);
      ReferenceDataArray := DataArray;
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
      ReadData(DataArray, 'ATMID',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UseAtmaxAtmidAnisotropy, AtmidColIndex);
      {$ENDREGION}

      {$REGION 'ATMIN'}
      DataArray := Model.DataArrayManager.GetDataSetByName(KMinTransverseDisp);
      ReadData(DataArray, 'ATMIN',
        ReferenceDataArray.PestParametersUsed
        and PestAnisotropyOptions.UseAtmaxAtminAnisotropy, AtMinColIndex);
      {$ENDREGION}

    {$REGION 'SIGMAS'}
      if Model.Sutra4EnergyUsed(nil) then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(KScaledSolidGrainThermalConductivity);
      end
      else
      begin
        DataArray := nil;
      end;
      ReadData(DataArray, 'SIGMAS', False, SigmasColIndex);
    {$ENDREGION}

   {$REGION 'SIGMAA'}
     if Model.Sutra4EnergyUsed(nil) then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(KScaledEffectiveAirThermalConductivity);
      end
      else
      begin
        DataArray := nil;
      end;
      ReadData(DataArray, 'SIGMAA', False, SigmaaColIndex);
   {$ENDREGION}
      NewLine;


      {$REGION 'Write values'}
      WriteString('# Write new data values');
      NewLine;
      if Mesh.MeshType = mt3D then
      begin
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
          NewLine;

          WriteString('# remove unneeded slists and plists');
          NewLine;
          WriteString(Format('p_z%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('s_EN3D%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('s_LREG%0:d.remove()', [LayerIndex]));
          NewLine;
          WriteString(Format('s_Active_%0:d.remove()', [LayerIndex]));
          NewLine;

          for RootIndex := 0 to UsedDataRoots.Count-1 do
          begin
            DataRoot := UsedDataRoots[RootIndex];
            WriteString(Format('p_%1:s%0:d.remove()', [LayerIndex, DataRoot]));
            NewLine;
          end;
          NewLine;

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
    end;
  finally
    CloseFile;
    UsedDataRoots.Free;
  end;
end;

procedure TSutraData15BScriptWriter.WriteFiles(var AFileName: string);
var
  PLPROC_Location: string;
  ScriptLine: string;
  ModelDirectory: string;
begin
  FFileName := FileName(AFileName);
//  Model.SutraPestScripts.Add(FFileName);

  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  ModelDirectory := ExtractFileDir(FFileName);
  if Model.PestUsed then
  begin
    MoveAppToDirectory(PLPROC_Location, ModelDirectory);
    PLPROC_Location := ExtractFileName(PLPROC_Location);
  end;
  ScriptLine := Format('"%0:s" ''%1:s''', [PLPROC_Location, ExtractFileName(FFileName)]);

  FRoot := ExtractFileName(ChangeFileExt(AFileName , ''));
  GetParameterNames(FParameterNames);
  GetUsedParameters;
  WritePilotPointFiles;

  WriteAFile(scWriteScript);
//  WriteAFile(scWriteTemplate);

  Model.PestTemplateLines.Add(ScriptLine);
end;

//procedure TSutraData15BScriptWriter.ReadDiscretization;
//var
//  Mesh: TSutraMesh3D;
//  LayerCount: Integer;
//  LayerIndex: Integer;
//  DisColIndex: Integer;
//  DisFileIndex: Integer;
//  FileNameAddition: string;
//begin
//  Mesh := Model.SutraMesh;
//  if Mesh.MeshType = mt3D then
//  begin
//    LayerCount := Mesh.LayerCount;
//  end
//  else
//  begin
//    LayerCount := 1;
//  end;
//
//  WriteString('#Read SUTRA element information file');
//  NewLine;
//  WriteString('cl_Discretization = read_list_file(skiplines=1,dimensions=2, &');
//  NewLine;
//  WriteString('  plist=p_x;column=2, &');
//  NewLine;
//  WriteString('  plist=p_y;column=3, &');
//  NewLine;
//  WriteString('  plist=p_EN2D;column=4, &');
//  NewLine;
//  WriteString(Format('  id_type=''indexed'',file=''%s.c_ele'')', [FRoot]));
//  NewLine;
//
//  DisFileIndex := 0;
//  if Mesh.MeshType = mt3D then
//  begin
//    DisColIndex := 5;
//    for LayerIndex := 1 to LayerCount do
//    begin
//      if ((LayerIndex-1) mod DataToWriteCount) = 0 then
//      begin
//        if DisFileIndex = 0 then
//        begin
//          FileNameAddition := '';
//        end
//        else
//        begin
//          FileNameAddition := '_' + IntToStr(DisFileIndex);
//        end;
//        Inc(DisFileIndex);
//        DisColIndex := 5;
//      end;
//      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
//      NewLine;
//      WriteString(Format('  plist=p_z%0:d;column=%1:d, &',
//        [LayerIndex, DisColIndex]));
//      NewLine;
//      Inc(DisColIndex);
//      WriteString(Format('  slist=s_EN3D%0:d;column=%1:d, &',
//        [LayerIndex, DisColIndex]));
//      NewLine;
//      Inc(DisColIndex);
//      WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &',
//        [LayerIndex, DisColIndex]));
//      NewLine;
//      Inc(DisColIndex);
//        WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_ele'')', [FRoot, FileNameAddition]));
//      NewLine;
//    end;
//    NewLine;
//  end;
//
//  NewLine;
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

procedure TSutraInitCondScriptWriter.ReadPilotPoints(Layer: integer);
var
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  procedure HandlePilotPointList(PilotPointFiles: TPilotPointFiles;
    const DataId: string);
  var
    PPIndex: Integer;
    FileProperties: TPilotPointFileObject;
    PListName: string;
  begin
    for PPIndex := 0 to PilotPointFiles.Count - 1 do
    begin
      FileProperties := PilotPointFiles[PPIndex];
      if (FileProperties.Parameter = AParam)
        and (FileProperties.Layer + 1 = Layer) then
      begin
        WriteString(Format('%0:s_PilotPoints%d = read_list_file(skiplines=0,dimensions=2, &',
          [DataId, PPIndex+1]));
        NewLine;
        PListName := Format('%0:s_%1:s_%2:d',
          [DataId, AParam.ParameterName, FileProperties.Layer + 1]);
        WriteString(Format('  plist=''%0:s'';column=%1:d, &',
          [PListName, 5]));
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
//  ColIndex: Integer;
  ParameterIndex: Integer;
  AParam: TModflowSteadyParameter;
  FileProperties: TPilotPointFileObject;
  UsedFileProperties: TPilotPointFileObject;
  FileIndex: Integer;
  PListName: string;
  PIndex: Integer;
  FileNameAddition: string;
  FileToReadIndex: Integer;
//  DisColIndex: Integer;
  DataColIndex: Integer;
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


//    if Mesh.MeshType = mt3D then
//    begin
//      LayerCount := Mesh.LayerCount + 1;
//    end
//    else
//    begin
//      LayerCount := 1;
//    end;

    if FMesh.MeshType = mt3D then
    begin
      Read3DDiscretization;
      WriteString(Format('temp3D=new_plist(reference_clist=%s,value=0.0)',
        ['cl_Discretization3D']));
      NewLine;
      NewLine;
    end;

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

//    WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
//    NewLine;
//    ColIndex := 1;
//
//    WriteString(Format('  slist=s_NN2D_Data;column=%d, &', [ColIndex]));
//    NewLine;
//    Inc(ColIndex);
//
//    WriteString(Format('  file=''%0:s.%1:s'')', [FRoot, FDataArrayName]));
//    NewLine;

    WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
      ['cl_Discretization']));
    NewLine;
    NewLine;

    DataColIndex := 2;
//    DisColIndex := 5;
    for LayerIndex := 1 to LayerCount do
    begin
      WriteString('# Layer');
      WriteInteger(LayerIndex);
      NewLine;
      {$REGION 'Node discretization'}
      if FMesh.MeshType = mt3D then
      begin
        FileNameAddition := '';
        FileIndex := 0;
        if (LayerIndex > 1) and (((LayerIndex -1) mod DataToWriteCount) =  0) then
        begin
          Inc(FileIndex);
          if LayerIndex = 0 then
          begin
            FileNameAddition := '';
          end
          else
          begin
            FileNameAddition := '.' + IntToStr(FileIndex);
          end;
//          DisColIndex := 5;
        end;
        // PLPROC has a limit of 5 s_lists per call of read_list_file.
        // To avoid reaching that limit, a separate call is used for each layer.
//        WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
//        NewLine;
//        WriteString(Format('  plist=p_z%0:d;column=%1:d, &', [LayerIndex, DisColIndex]));
//        NewLine;
//        Inc(DisColIndex);
//        WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, DisColIndex]));
//        NewLine;
//        Inc(DisColIndex);
//        WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &', [LayerIndex, DisColIndex]));
//        NewLine;
//        Inc(DisColIndex);
//        WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_nod'')', [FRoot, FileNameAddition]));
//        NewLine;
      end;
     {$ENDREGION}
//    end;
    NewLine;

//    for LayerIndex := 1 to LayerCount do
//    begin
      ReadPilotPoints(LayerIndex);
//    end;


      WriteString('#Read data to modify');
      NewLine;
      {$REGION data}
      WriteString(Format('# Read %s', [FDataArrayName]));
      NewLine;


      WriteString('temp.remove()');
      NewLine;
      WriteString(Format('temp=new_plist(reference_clist=%s,value=0.0)',
        ['cl_Discretization']));
      NewLine;
      NewLine;

      FileNameAddition := '';
      FileToReadIndex := 0;
//    for LayerIndex := 1 to LayerCount do
//    begin
      if (LayerIndex > 1) and ((LayerIndex-1) mod DataToWriteCount = 0) then
      begin
        Inc(FileToReadIndex);
        if LayerIndex = 0 then
        begin
          FileNameAddition := '';
        end
        else
        begin
          FileNameAddition := '_' + IntToStr(FileToReadIndex);
        end;
        DataColIndex := 2;
//        DisColIndex := 5;
      end;

      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      if Mesh.MeshType = mt3D then
      begin
        Inc(DataColIndex);

//        WriteString(Format('  slist=s_Layer%0:d;column=%1:d, &', [LayerIndex, DataColIndex]));
//        NewLine;
        Inc(DataColIndex);
      end;

      WriteString(Format('  plist=p_%2:s%0:d;column=%1:d, &', [LayerIndex, DataColIndex, 'Data']));
      NewLine;
      Inc(DataColIndex);

      WriteString(Format('  slist=s_%2:sPar%0:d;column=%1:d, &', [LayerIndex, DataColIndex, 'Data']));
      NewLine;
      Inc(DataColIndex);
      WriteString(Format('  file=''%0:s.%1:s%2:s'')', [FRoot, FDataArrayName, FileNameAddition]));
      NewLine;
//      WriteString('temp.remove()');
//      NewLine;
//    end;
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
//      for LayerIndex := 1 to LayerCount do
//      begin
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
//          WriteString('    # Substituting parameter values in zones');
//          NewLine;
//          WriteString(Format('p_%3:s%0:d(select=(s_%3:sPar%0:d == %2:d)) = p_%3:s%0:d * %1:s',
//            [LayerIndex, AParam.ParameterName, ParameterIndex+1, 'Data']));
//          NewLine;
        end;

//      end;
        NewLine;

      end;
    {$ENDREGION}
      WriteString(Format('s_DataPar%0:d.remove()', [LayerIndex]));
      NewLine;
      NewLine;

      if Mesh.MeshType = mt3D then
      begin
        WriteString('# Apply values to 3D mesh');
        NewLine;
  //      for LayerIndex := 1 to LayerCount do
        begin
          WriteString(Format('temp3D(select=(s_Layer3D.eq.%0:d)) &', [LayerIndex]));
          NewLine;
          WriteString(Format('  =  p_Data%0:d.assign_by_relation(relate=slist;source=s_NN2D;target=s_NN2D_3D)', [LayerIndex]));
          NewLine;
        end;
      end;

      WriteString(Format('p_Data%0:d.remove()', [LayerIndex]));
      NewLine;
      NewLine;
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
  ModelDirectory: string;
begin
  FID := ID;
  FDataArrayName := DataArrayName;
  FDataArray := Model.DataArrayManager.GetDataSetByName(FDataArrayName);
  FMesh := Model.SutraMesh;
  FFileName := ChangeFileExt(AFileName, '.' + FDataArrayName + '.script');
//  Model.SutraPestScripts.Add(FFileName);
  PLPROC_Location := GetPLPROC_Location(FFileName, Model);
  ModelDirectory := ExtractFileDir(FFileName);
  if Model.PestUsed then
  begin
    MoveAppToDirectory(PLPROC_Location, ModelDirectory);
    PLPROC_Location := ExtractFileName(PLPROC_Location);
  end;
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

procedure TSutraInitCondScriptWriter.ReadDiscretization(ReadSLists: Boolean);
var
  LayerIndex: Integer;
  LayerCount: Integer;
  ColIndex: Integer;
  FileNameAddition: string;
  FileIndex: Integer;
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
    FileNameAddition := '';
    FileIndex := 0;
    for LayerIndex := 1 to LayerCount do
    begin
      if (LayerIndex > 1) and (((LayerIndex -1) mod DataToWriteCount) =  0) then
      begin
        Inc(FileIndex);
        if LayerIndex = 0 then
        begin
          FileNameAddition := '';
        end
        else
        begin
          FileNameAddition := '.' + IntToStr(FileIndex);
        end;
        ColIndex := 5;
      end;
      // PLPROC has a limit of 5 s_lists per call of read_list_file.
      // To avoid reaching that limit, a separate call is used for each layer.
      WriteString('read_list_file(reference_clist=''cl_Discretization'',skiplines=1, &');
      NewLine;
      WriteString(Format('  plist=p_z%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
      NewLine;
      Inc(ColIndex);
      if ReadSLists then
      begin
        WriteString(Format('  slist=s_NN3D%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
        NewLine;
      end;
      Inc(ColIndex);
      if ReadSLists then
      begin
        WriteString(Format('  slist=s_Active_%0:d;column=%1:d, &', [LayerIndex, ColIndex]));
        NewLine;
      end;
      Inc(ColIndex);
      WriteString(Format('  id_type=''indexed'',file=''%0:s%1:s.c_nod'')', [FRoot, FileNameAddition]));
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
  ModelDirectory: string;
  LayerCount: Integer;
  LayerIndex: Integer;
begin
  if FMesh.MeshType = mt3D then
  begin
    LayerCount := FMesh.LayerCount + 1;
  end
  else
  begin
    LayerCount := 1;
  end;
  PilotPointsUsed := FPilotPointFiles.Count > 0;
  if PilotPointsUsed then
  begin
    ScriptFileName := ChangeFileExt(FFileName, StrKrigfactorsscript);
    OpenFile(ScriptFileName);
    try
      WriteString('#Script for PLPROC for saving kriging factors');
      NewLine;
      NewLine;
      for LayerIndex := 1 to LayerCount do
      begin
        ReadPilotPoints(LayerIndex);
      end;

      ReadDiscretization(False);
      SaveKrigingFactors;
      PLPROC_Location := GetPLPROC_Location(FFileName, Model);
      ModelDirectory := ExtractFileDir(FFileName);
      if Model.PestUsed then
      begin
        MoveAppToDirectory(PLPROC_Location, ModelDirectory);
        PLPROC_Location := ExtractFileName(PLPROC_Location);
      end;
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

{ TAnisotropyWriter }

procedure TAnisotropyWriter.CalculateAnisotropy(LayerIndex: Integer);
var
  ColIndex: Integer;
  ReferenceValue: Double;
begin
  for ColIndex := 0 to FDataArray.ColumnCount-1 do
  begin
    ReferenceValue := FReferenceDataArray.RealData[LayerIndex,0,ColIndex];
    if ReferenceValue = 0 then
    begin
      FAnistropy[ColIndex] := 1;
    end
    else
    begin
      FAnistropy[ColIndex] :=
        FDataArray.RealData[LayerIndex,0,ColIndex]/ReferenceValue;
    end;
  end;
end;

class function TAnisotropyWriter.Extension: string;
begin
  result := '';
end;

procedure TAnisotropyWriter.WriteFile(const Root: string; DataArray,
  ReferenceDataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  FRoot := Root;
  FDataArray := DataArray;
  FReferenceDataArray := ReferenceDataArray;
  SetLength(FAnistropy, FDataArray.ColumnCount);
  for LayerIndex := 0 to FDataArray.LayerCount -1 do
  begin
    CalculateAnisotropy(LayerIndex);
    WriteTemplate(LayerIndex);
  end;
end;

procedure TAnisotropyWriter.WriteTemplate(LayerIndex: Integer);
var
  TemplateFileName: string;
  ReferenceFileName: string;
  ArrayFileName: string;
  ExtendedTemplateCharacter: Char;
  ArrayTemplateCharacter: Char;
  ArrayFileReference: string;
  ColIndex: Integer;
  ALine: string;
begin
  ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
  ArrayTemplateCharacter := Model.PestProperties.ArrayTemplateCharacter;

  ArrayFileName := Format('arrays\%0:s.%1:s_%2:d.arrays',
    [FRoot, FDataArray.Name, LayerIndex+1]);
  ReferenceFileName := Format('arrays\%0:s.%1:s_%2:d.arrays',
   [FRoot, FReferenceDataArray.Name, LayerIndex+1]);
  Model.FilesToDelete.Add(ArrayFileName);
  ArrayFileReference := Format('arrays\%0:s.%1:s_%2:d.ArraysFile',
    [FRoot, FDataArray.Name, LayerIndex+1]);
  OpenFile(ExpandFileName(ArrayFileReference));
  try
    WriteString(ArrayTemplateCharacter);
    NewLine;
    WriteString(FReferenceDataArray.Name);
    WriteString('[1,1,');
    WriteInteger(FReferenceDataArray.ColumnCount);
    WriteString('] ');
    WriteString(ReferenceFileName);
    NewLine;
  finally
    CloseFile;
  end;

  TemplateFileName := ArrayFileName + '.tpl';

  OpenFile(ExpandFileName(TemplateFileName));
  try
    WriteString('etf ');
    WriteString(ExtendedTemplateCharacter);
    NewLine;
    WriteString(ExtendedTemplateCharacter);
    WriteString('ReadArrays(');
    WriteString(ArrayFileReference);
    WriteString(')');
    WriteString(ExtendedTemplateCharacter);
    NewLine;
    for ColIndex := 0 to FReferenceDataArray.ColumnCount - 1 do
    begin
      WriteString(ExtendedTemplateCharacter);
      WriteString('                ');
      WriteFloat(FAnistropy[ColIndex]);
      WriteString(' * ');
      WriteString(ArrayTemplateCharacter);
      WriteString('                ');
      WriteString(FReferenceDataArray.Name);
      WriteString('[1,1,');
      WriteInteger(ColIndex+1);
      WriteString(']');
      WriteString(ArrayTemplateCharacter);
      WriteString(ExtendedTemplateCharacter);
      NewLine;
    end;
  finally
    CloseFile;
  end;
  ALine := PestUtilityProgramPath(
    StrEnhancedTemplateProc, TemplateFileName) + ' ' + TemplateFileName;
  Model.PestTemplateLines.Add(ALine);
end;

end.
